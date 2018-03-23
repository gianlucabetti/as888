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
001600100222
001700100222       //--------------------------------------------------------------
001800100222       //?Dichiarazione file.
001900100222       //--------------------------------------------------------------
002000100222
002100100222       // -?Ordini Ritiro Merce?
002200100222     fFNORM01L  if   e           k disk
002300100222
002400100222       // -?Ordini Ritiro Merce - Estensione?
002500100222     fFNORE01L  if   e           k disk
002600100324
002700100324       // -?Tabelle?
002800100324     fTNTBE01L  if   e           k disk
002900100324     fTABEL00F  if   e           k disk
003000100223
003100100223       // -?Organigramma filiale/aziendale?
003200100223     fAZORG01L  if   e           k disk
003300100222
003400100222       // -?Spool da inviare via e-mail:?
003500100224       //   ·?Corpo del messaggio di posta elettronica?
003600100222     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003700100224       //   ·?Allegato al messaggio di posta elettronica?
003800100222     fFIOR52P   o    e             printer usropn
003900100222
004000100222       //--------------------------------------------------------------
004100100222       //?Definizione costanti.                                        ?
004200100222       //--------------------------------------------------------------
004300100222
004400100324       // -?Codice utente / Lingua italiana  in tabelle su TABEL00F
004500100324     d c_Kut           c                   const(1)
004600100222
004700100222       // -?Dati di default?
004800100223     d c_Mittente      c                   const('ced')
004900100222     d c_at            c                   const('@bartolini.it')
005000100222     d c_Sede          c                   const('046')
005100100222
005200100223       // -?Comandi di override al PrtF "corpo e-mail"?
005300100223     d c_CmdOvrPrtF_1  c                   const('OVRPRTF +
005400100222     d                                           file(PRTEMAIL) +
005500100222     d                                           pagesize(66 132) +
005600100222     d                                           lpi(6) cpi(10) +
005700100222     d                                           ovrscope(*actgrpdfn) +
005800100222     d                                           ')
005900100223     d c_CmdDltOvr_1   c                   const('DLTOVR +
006000100222     d                                            file(PRTEMAIL) +
006100100222     d                                            lvl(*actgrpdfn)')
006200100223
006300100223       // -?Comandi di override ai PrtF "allegato e-mail"?
006400100223     d c_CmdOvrPrtF_2  c                   const('OVRPRTF +
006500100223     d                                           file(FIOR52P) +
006600100222     d                                           ovrscope(*actgrpdfn) +
006700100222     d                                           ')
006800100223     d c_CmdDltOvr_2   c                   const('DLTOVR +
006900100223     d                                            file(FIOR52P) +
007000100222     d                                            lvl(*actgrpdfn)')
007100100222
007200100222       //--------------------------------------------------------------
007300100222       //?Definizione schiere.                                         ?
007400100222       //--------------------------------------------------------------
007500100222
007600100222
007700100222       //--------------------------------------------------------------
007800100222       //?Definizione aree dati.                                       ?
007900100222       //--------------------------------------------------------------
008000100222
008100100222       // -?Dati utente?
008200100222     d §AzUte        e ds                  extname(AZUTE00F)
008300100222     d                                     dtaara
008400100222     d §DatiUte      e ds                  extname(dDatiUte)
008500100222     d                                     dtaara
008600100222
008700100222       //--------------------------------------------------------------
008800100222       //?Definizione strutture dati.                                  ?
008900100222       //--------------------------------------------------------------
009000100222
009100100222       // -?Parametri ricevuti?
009200100222     d KPJBA         e ds
009300100224     d FIOR52ds      e ds
009400100222
009500100222       // -?Tab. "MRA" = Bart-Mailing x FIOR52R?
009600100222     d dMRAOR52      e ds                  inz
009700100317
009800100317       // -?Tipo servizio?
009900100317     d dOREgen       e ds                  inz  qualified
010000100317     d ds5E          e ds                  inz
010100100222
010200100222       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
010300100222     d TRTCM1ds      e ds                  inz
010400100222       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010500100222     d   §CM1mitt    e                     inz('ced')
010600100222       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010700100222     d   §CM1dst     e                     inz('stefano.merighi@bartolini.it')
010800100222       //  ?·§CM1tips = Tipo lettera via e-mail:?
010900100222       //   ?"LET" = testo allegato in corpo con logo?
011000100222       //           ?(richiede righe libere iniziali per il logo)?
011100100222       //   ?"COR" = testo integrato senza logo?
011200100222       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011300100222       //   ?"SP1" = spool allegato (in formato PDF)?
011400100222     d   §CM1tips    e                     inz('OR1')
011500100222       //  ?·§CM1po   = Filiale?
011600100222     d   §CM1po      e                     inz('046')
011700100222       //  ?·§CM1var  = Oggetto e-mail?
011800100315     d*//§CM1var     e                     inz('*OBJM*+
011900100315     d*//                                  Ritiro per conto CEVA')
012000100315     d   §CM1var     e                     inz('*OBJM*')
012100100222       //  ?·§CM1sts  = Stato?
012200100222     d   §CM1sts     e                     inz('H')
012300100222       //  ?·§CM1sts  = Id processo?
012400100222     d   §CM1idp     e                     inz('3')
012500100222       //  ?·§CM1att  = Numero attach corrente?
012600100222     d   §CM1att     e                     inz('01')
012700100222       //  ?·§CM1totatt  = Numero attach totali?
012800100222     d   §CM1totatt  e                     inz('02')
012900100223
013000100223       // -?Codice a Barre dell'O.R.M.?
013100100223     d BARcodeORMds    ds            14    inz
013200100223     d  ORMpoe                             inz
013300100223     d  ORMnsr                             inz
013400100223     d  ORMnor                             inz
013500100223     d  ORMnrv                             inz
013600100222
013700100222       //--------------------------------------------------------------
013800100222       //?Definizione variabili.                                       ?
013900100222       //--------------------------------------------------------------
014000100222
014100100223       // -?Indici di schiera?
014200100223     d xx              s              3  0 inz
014300100223
014400100223       // -?Campi di stampa?
014500100223     d O_txt           s            132    inz
014600100226     d Save_IndEmail   s                   like(OREdati) inz
014700100226     d Save_Note       s                   like(OREdati) inz
014800100223
014900100223       // -?Campi di comodo?
015000100225     d w_OREtrc        s                   like(OREtrc)  inz
015100100223     d wDate_EUR       s               d   datfmt(*eur)  inz
015200100223     d wDate           s              8  0 inz
015300100222
015400100222       //--------------------------------------------------------------
015500100222       //?Definizione prototipi procedure.                             ?
015600100222       //--------------------------------------------------------------
015700100222
015800100222       // -?Reperimento dati utente?
015900130627     d TIBS34ds      e ds                  inz
016000100222      /copy gaitrasrc/srcProtoPR,TIBS34R
016100100223
016200100223       // -?Gestione numeratori in GRU?
016300100223     d TRUL33ds      e ds                  inz
016400100223     d   I33tla      e                     inz('L')
016500100223     d   I33ope      e                     inz(*zero)
016600100223     d   I33cnu      e                     inz(024)
016700100223     d   I33num      e                     inz(1)
016800100223      /copy gaitrasrc/srcProtoPR,TRUL33R
016900100223
017000100223       // -?Controllo/Decodifica cliente?
017100100223     d TIBS69ds      e ds                  qualified
017200100223     d                                     inz
017300100223     d ds_CNACO      e ds                  extname(CNACO00F) qualified
017400100223     d                                     inz
017500100223     d ds_CNIND      e ds                  extname(CNIND00F) qualified
017600100223     d                                     inz
017700100223     d ds_CNCLP      e ds                  extname(CNCLP00F) qualified
017800100223     d                                     inz
017900100223     d ds_FNCLS      e ds                  extname(FNCLS00F) qualified
018000100223     d                                     inz
018100100223      /copy gaitrasrc/srcProtoPR,TIBS69R
018200100222
018300100223       // -?Parametri API QCAPCMD (Process Commands)?
018400100223     d Qcmd            s           2048    inz  varying
018500100223      /copy qSysInc/qRpgleSrc,QCAPCMD
018600100223      /copy gaitrasrc/srcProtoPR,QCAPCMD
018700100223
018800100223       // -?Parametri gestione errori API.?
018900100223      /copy qsysinc/qrpglesrc,QUSEC
019000100222
019100100222       //--------------------------------------------------------------
019200100222       //?Definizione key-list.                                        ?
019300100222       //--------------------------------------------------------------
019400100222
019500100222       // -?File FNORM01L?
019600100222     d k04fnorm01    e ds                  extname(FNORM01L : *key)
019700100222     d                                     prefix(k_)   inz
019800100222
019900100222       // -?File FNORE01L?
020000100222     d k05fnore01    e ds                  extname(FNORE01L : *key)
020100100222     d                                     prefix(k_)   inz
020200100324
020300100324       // -?File TNTBE01L?
020400100324     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020500100324     d                                     prefix(k_)   inz
020600100324
020700100324       // -?File TABEL00F?
020800100324     d k03tabel00    e ds                  extname(TABEL00F : *key)
020900100324     d                                     prefix(k_)   inz
021000100222
021100100222       //--------------------------------------------------------------
021200100222       //?M A I N - L I N E                                            ?
021300100222       //--------------------------------------------------------------
021400100222
021500100222     c     *Entry        plist
021600100222     c                   parm                    KPJBA
021700100224     c                   parm                    FIOR52ds
021800100222
021900100222      /free
022000100222
022100100222       // -?Operazioni iniziali?
022200100222       exsr  sr_RoutInz;
022300100222
022400100223       // -?Stampa (ed invio) e-mail?
022500100222       exsr  sr_PrtEmail;
022600100222
022700100222       // -?Operazioni finali?
022800100222       exsr  sr_RoutEnd;
022900100222
023000100222       //--------------------------------------------------------------
023100100222       //?Operazioni iniziali.                                         ?
023200100222       //--------------------------------------------------------------
023300100222       BEGSR  sr_RoutInz;
023400100222
023500100222         *inLR = *on;
023600100223
023700100223         // -?Impostazione parametri ricevuti?
023800100224         if  %parms() > 1;
023900100225           O52err = *off;
024000100224           clear  O52msg;
024100100224         endif;
024200100226
024300100226         // -?Pulizia inziale campi di comodo?
024400100226         clear  Save_IndEmail;
024500100222
024600100222         // -?Reperimento dati del lavoro?
024700100222         exsr  sr_DatiJob;
024800100224
024900100226         // -?*-=»>  Solo TEST in stampa  <«=-*?
025000100224         if  %parms() = 1;
025100100224           open  PrtEMAIL;
025200100224           open  FIOR52P;
025300100224           exsr  sr_TEST;
025400100224           leavesr;
025500100224         endif;
025600100222
025700100223         // -?Reperimento tab. "MRA"?
025800100224         exsr  sr_TabMRA;
025900100223
026000100223         // -?Reperimento dati O.R.M.?
026100100224         exsr  sr_RepFNORM;
026200100223
026300100223         // -?Reperimento indirizzo email del destinatario da O.R.M.?
026400100224         exsr  sr_RepFNORE;
026500100224
026600100224         // -?Reperimento ultimi dati (decodifiche)?
026700100224         exsr  sr_AltriDati;
026800100223
026900100223         // -?Reperimento nuovo progressivo da numeratore "024"?
027000100224         exsr  sr_Num024;
027100100223
027200100223         // -?Override ai file di stampa (per impostarvi i dati per?
027300100223         //  ?l'invio via e-mail)?
027400100224         exsr sr_Override;
027500100223
027600100223         // -?Apertura file di stampa?
027700100223         open  PrtEMAIL;
027800100223         open  FIOR52P;
027900100222
028000100222       ENDSR;
028100100222
028200100222       //--------------------------------------------------------------
028300100222       //?Reperimento Dati del job (Utente/Operativi).                 ?
028400100222       //--------------------------------------------------------------
028500100222       BEGSR  sr_DatiJob;
028600100222
028700100222         in(E) §AzUte;
028800100222         if NOT %error;
028900100222           in(E) §DatiUte;
029000100222         endif;
029100100222         if %error or RSut = *blanks;
029200100222           clear TIBS34ds;
029300100222           tibs34r(tibs34ds);
029400100222           in §AzUte;
029500100222           in §DatiUte;
029600100222         endif;
029700100222
029800100222       ENDSR;
029900100224
030000100224       //--------------------------------------------------------------
030100100224       //?Reperimento dati da tab. "MRA".                              ?
030200100224       //--------------------------------------------------------------
030300100224       BEGSR  sr_TabMRA;
030400100224
030500100224         clear  dMRAor52;
030600100324
030700100324         k_TBEcod = 'MRA';
030800100324         k_TBEke1 = 'FIOR52R';
030900100324         k_TBEke2 = 'CEVA';
031000100324
031100100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
031200100324
031300100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
031400100324           dMRAor52 = TBEuni;
031500100324         else;
031600100324           O52err = *on;
031700100324           O52msg = 'Parametri "' + %trimr(k_TBEke1) +
031800100324                    '" non reperiti in tab. "MRA"';
031900100324           exsr  sr_RoutEnd;
032000100324         endif;
032100100224
032200100224       ENDSR;
032300100224
032400100224       //--------------------------------------------------------------
032500100224       //?Reperimento dati O.R.M. (file FNORM00F)                      ?
032600100224       //--------------------------------------------------------------
032700100224       BEGSR  sr_RepFNORM;
032800100224
032900100224         //clear  k04fnorm01;          (già così)
033000100224         k_ORMpoe = I52poe;
033100100224         k_ORMnsr = I52nsr;
033200100224         k_ORMnor = I52nor;
033300100224         k_ORMnrv = I52nrv;
033400100224         chain  %kds( k04fnorm01 )  FNORM000;
033500100224         if  NOT %found(FNORM01L)   or   ORMatb <> *blank;
033600100224           O52err = *on;
033700100224           O52msg = 'Non trovato O.R.M. ' + %editc( I52poe : 'X' ) +
033800100224                    '-' + %trim( %editc( I52nsr : '3' ) ) +
033900100224                    '-' + %trim( %editc( I52nor : '3' ) ) +
034000100224                    '-' + %trim( %editc( I52nrv : '3' ) );
034100100224           exsr  sr_RoutEnd;
034200100224         endif;
034300100224
034400100224       ENDSR;
034500100224
034600100224       //--------------------------------------------------------------
034700100224       //?Reperimento dati aggiuntivi O.R.M. (file FNORE00F)           ?
034800100224       //--------------------------------------------------------------
034900100224       BEGSR  sr_RepFNORE;
035000100317
035100100317         // -?Reperimento estensione ORM?
035200100317         w_OREtrc = 'G ';
035300100317         exsr sr_ChainFNORE;
035400100317         dOREgen = OREdati;
035500100317         clear  ds5E;
035600100317         if  dOREgen.§OREtsp <> *blank;
035700100324           k_TBLkut = c_Kut;
035800100324           k_TBLcod = '5E';
035900100324           k_TBLkey = dOREgen.§OREtsp;
036000100324           chain  %kds( k03tabel00 )  TABEL;
036100100324           if  %found(TABEL00F)   and   TBLflg = *blank;
036200100324             ds5E = TBLuni;
036300100324           endif;
036400100317         endif;
036500100224
036600100225         // -?Reperimento indirizzo e-mail del destinatario?
036700100225         w_OREtrc = 'M ';
036800100225         exsr sr_ChainFNORE;
036900100226         if  %found(FNORE01L);
037000100226          Save_IndEmail = OREdati;
037100100226         else;
037200100224           O52err = *on;
037300100224           O52msg = 'Non trovato indirizzo e-mail del destinatario da ORM '+
037400100224                    %editc( I52poe : 'X' ) + '-' +
037500100224                    %trim( %editc( I52nsr : '3' ) ) + '-' +
037600100224                    %trim( %editc( I52nor : '3' ) ) + '-' +
037700100224                    %trim( %editc( I52nrv : '3' ) );
037800100226           exsr  sr_RoutEnd;
037900100224         endif;
038000100225
038100100225         // -?Reperimento note ritiro?
038200100225         w_OREtrc = 'N ';
038300100225         exsr sr_ChainFNORE;
038400100226         Save_Note = OREdati;
038500100224
038600100224       ENDSR;
038700100225
038800100225       //--------------------------------------------------------------
038900100225       //?Aggancio record specifico in dati aggiuntivi O.R.M.          ?
039000100225       //--------------------------------------------------------------
039100100225       BEGSR  sr_ChainFNORE;
039200100225
039300100225         clear  OREdati;
039400100225
039500100225         clear  k05fnore01;
039600100225         k_OREpoe = I52poe;
039700100225         k_OREnsr = I52nsr;
039800100225         k_OREnor = I52nor;
039900100225         k_OREnrv = I52nrv;
040000100225         k_OREtrc = w_OREtrc;
040100100225
040200100225         chain  %kds( k05fnore01 )  FNORE000;
040300100225
040400100225       ENDSR;
040500100224
040600100224       //--------------------------------------------------------------
040700100224       //?Reperimento altri dati (decodifiche).                        ?
040800100224       //--------------------------------------------------------------
040900100224       BEGSR  sr_AltriDati;
041000100224
041100100224         // -?Decodifica cliente pagante?
041200100224         clear  TIBS69ds;
041300100224         TIBS69ds.I69sif = knsif;
041400100224         TIBS69ds.I69kcc = DUTkci;
041500100224         TIBS69ds.I69kac = ORMksc;
041600100224         tibs69r( TIBS69ds :
041700100224                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
041800100224
041900100224         // -?Dati anagrafici della filiale di ritiro?
042000100224         chain  (ORMpor)  AZORG;
042100100224         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
042200100224           clear  ORGloc;
042300100224           clear  ORGind;
042400100224           clear  ORGpro;
042500100224           clear  ORGtel;
042600100224         endif;
042700100224
042800100224       ENDSR;
042900100224
043000100224       //--------------------------------------------------------------
043100100224       //?Reperimento progressivo contatore "024"                      ?
043200100224       //--------------------------------------------------------------
043300100224       BEGSR  sr_Num024;
043400100224
043500100224         reset  TRUL33ds;
043600100224
043700100224         kpjbu  = TRUL33ds;
043800100224         trul33r ( KPJBA );
043900100224         TRUL33ds = kpjbu;
044000100224
044100100224         if  O33err <> *zero;
044200100224           O52err = *on;
044300100224           O52msg = 'Fallito reperimento numeratore "' +
044400100224                    %editc(I33cnu : 'X') + '"';
044500100224           exsr  sr_RoutEnd;
044600100224         endif;
044700100224
044800100224       ENDSR;
044900100224
045000100224       //--------------------------------------------------------------
045100100224       //?Impostazione dati per stampa di prova.                       ?
045200100224       //--------------------------------------------------------------
045300100224       BEGSR  sr_TEST;
045400100224
045500100224         wDate_EUR = %date();
045600100224         ORMdar = %dec( wDate_EUR : *iso );
045700100224
045800100224         ORMorr = 6789;
045900100224
046000100224         ds_CNACO.ACOrag = *all'W';
046100100224
046200100224         ORGloc = *all'W';
046300100224         ORGind = *all'X';
046400100224         ORGpro = *all'W';
046500100224         ORGtel = '1234/67890123';
046600100224
046700100224         ORMpoe = 321;
046800100224         ORMnsr = 98;
046900100224         ORMnor = 1234567;
047000100224         ORMnrv = 89;
047100100224
047200100224         ORMpkg = 123456,7;
047300100224         ORMncl = 54321;
047400100224         ORMvlm = 98,765;
047500100224
047600100224         ORMrer = *all'W';
047700100224         ORMrsr = *all'W';
047800100224         ORMinr = *all'X';
047900100224         ORMcar = '12345____';
048000100224         ORMlor = *all'W';
048100100224         ORMprr = *all'W';
048200100224         ORMter = '1234/67890123456';
048300100224
048400100224         ORMrsc = *all'W';
048500100224         ORMinc = *all'X';
048600100224         ORMcac = '12345____';
048700100224         ORMloc = *all'W';
048800100224         ORMprc = *all'W';
048900100224
049000100224         ORMrfa = *all'W';
049100100317
049200100317         dOREgen.§OREtsp = 'Q';
049300100317         §5Edes = *all'W';
049400100224
049500100224         OREdati = *all'W';
049600100226         Save_Note = OREdati;
049700100224
049800100224         ORMrso = *all'W';
049900100224         ORMino = *all'X';
050000100224         ORMcao = '12345____';
050100100224         ORMloo = *all'W';
050200100224         ORMpro = *all'W';
050300100224
050400100224       ENDSR;
050500100223
050600100223       //--------------------------------------------------------------
050700100223       //?Preparazione ed invio e-mail.                                ?
050800100223       //--------------------------------------------------------------
050900100223       BEGSR  sr_PrtEmail;
051000100223
051100100223         wDate_EUR = %date( ORMdar : *iso );
051200100223         wDate = %dec( wDate_EUR );
051300100223
051400100223         // -?Stampa "corpo e-mail"?
051500100223         For  xx = 1  To  20;
051600100223           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
051700100223           select;
051800100223             when  xx = 1;
051900100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
052000100223             when  xx = 2;
052100100223               O_txt = %trimr(O_txt) + %trim( ORMrso );
052200100223             when  xx = 3;
052300100223               O_txt = %trimr(O_txt) + %trim( ORMrsr );
052400100223             when  xx = 4;
052500100223               O_txt = %trimr(O_txt) + %trim( ORMinr );
052600100223             when  xx = 5;
052700100223               O_txt = %trimr(O_txt) + %trim( ORMcar );
052800100223             when  xx = 6;
052900100223               O_txt = %trimr(O_txt) + %trim( ORMlor );
053000100223             when  xx = 7;
053100100223               O_txt = %trimr(O_txt) + %trim( ORMprr );
053200100223             when  xx = 8;
053300100223               O_txt = %trimr(O_txt) + %trim( ORMrer );
053400100223             when  xx = 9;
053500100223               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
053600100223             when  xx = 10;
053700100223               O_txt = %trimr(O_txt) + %triml( %editw( ORMorr : '0 :  ' ) );
053800100223             when  xx = 11;
053900100223               O_txt = %trimr(O_txt) + %triml( ORMrso );
054000100223             when  xx = 12;
054100100223               O_txt = %trimr(O_txt) + %triml( ds_CNACO.ACOrag );
054200100223             when  xx = 13;
054300100223               O_txt = %trimr(O_txt) + %triml( ORGloc );
054400100223             when  xx = 14;
054500100223               O_txt = %trimr(O_txt) + %triml( ORGind );
054600100223             when  xx = 15;
054700100223               O_txt = %trimr(O_txt) + %triml( ORGpro );
054800100223             when  xx = 16;
054900100223               O_txt = %trimr(O_txt) + %triml( ORGtel );
055000100223             when  xx = 17;
055100100223               O_txt = %trimr(O_txt) + %editc( ORMpoe : 'X' );
055200100223             when  xx = 18;
055300100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnsr : '3' ) );
055400100223             when  xx = 19;
055500100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnor : '3' ) );
055600100223             when  xx = 20;
055700100223               O_txt = %trimr(O_txt) + %triml( %editc( ORMnrv : '3' ) );
055800100223           endsl;
055900100223           O_txt = %trimr(O_txt) + '}{';
056000100223           except  PRTparm;
056100100223         EndFor;
056200100223
056300130627         // -?Impostazione variabili "allegato e-mail"?
056400130627         // -?Dati dell'O.R.M.?
056500100223         pBCO = BarCodeORMds;
056600100223         pDAR = wDate;
056700100223         pPKG = ORMpkg;
056800100223         pNCL = ORMncl;
056900100223         pVLM = ORMvlm;
057000100223         // -?Dati del mittente?
057100100223         pRER = ORMrer;
057200100223         pRSR = ORMrsr;
057300100223         pINR = ORMinr;
057400100223         pCAR = ORMcar;
057500100223         pLOR = ORMlor;
057600100223         pPRR = '(' + ORMprr + ')';
057700100223         pTER = ORMter;
057800100223         // -?Dati del destinatario?
057900100223         pRSC = ORMrsc;
058000100223         pINC = ORMinc;
058100100223         pCAC = ORMcac;
058200100223         pLOC = ORMloc;
058300100223         pPRC = '(' + ORMprc + ')';
058400100223         // -?Riferimento mittente?
058500100223         pRFA = ORMrfa;
058600100317         // -?Tipo servizio?
058700100317         if  dOREgen.§OREtsp <> *blank;
058800100317           pkTSP = 'Tipo servizio';
058900100317           pTSP = dOREgen.§OREtsp + ' - ' + §5Edes;
059000100317         endif;
059100100223         // -?Note ritiro?
059200100226         pNOTE = Save_Note;
059300100223         // -?Dati del commissionante?
059400100223         pRSO = ORMrso;
059500100223         pINO = ORMino;
059600100223         pCAO = ORMcao;
059700100223         pLOO = ORMloo;
059800100223         pPRO = '(' + ORMpro + ')';
059900100223
060000130627         // -?Stampa "allegato e-mail"?
060100100226         write  OR52corpo;
060200100223         write  OR52box;
060300100223         write  OR52piede;
060400100223
060500100223       ENDSR;
060600100222
060700100222       //--------------------------------------------------------------
060800100222       //?Override al file di stampa per impostarvi i dati per         ?
060900100223       //?  l'invio via e-mail.                                        ?
061000100222       //--------------------------------------------------------------
061100100222       BEGSR sr_Override;
061200100222
061300100222         reset TRTCM1ds;
061400100308         §CM1mitt = §MRAmitt;
061500100226         §CM1dst  = %trim(Save_IndEmail);
061600100223         §CM1po   = %editc( DUTpou : 'X' );
061700100329         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm) + ' - ORM ' +
061800100329                    %editc( ORMpoe : 'X' ) + '-' +
061900100329                    %triml( %editc( ORMnsr : '3' ) ) + '-' +
062000100329                    %triml( %editc( ORMnor : '3' ) ) + '-' +
062100100329                    %triml( %editc( ORMnrv : '3' ) );
062200100223         §CM1sts  = §MRAsts;
062300100223         §CM1idp  = §MRAidpro;
062400100223         §CM1prog = %subst( %editc( O33nri : 'X' ) :
062500100223                            %size(O33nri) - %size(§CM1prog) + 1 );
062600100222
062700100223         // -?Override al "corpo e-mail"?
062800100223         if  §MRAregCor <> *blank;
062900100223           §CM1tips = §MRAregCor;
063000100223           §CM1att  = '02';
063100100226           Qcmd = c_CmdOvrPrtF_1
063200100223                + ' outq(' + %trim(§MRAoutqi) + ')'
063300100222                + ' usrdfndta(''' + TRTCM1ds + ''')';
063400100223         else;
063500100226           Qcmd = c_CmdOvrPrtF_1;
063600100222         endif;
063700100222
063800100223         exsr  sr_ExecCmd;
063900100223
064000100223         if  Qusei <> *blank;
064100100223           O52err = *on;
064200100223           O52msg = 'Fallita override al file di stampa per +
064300100223                     "corpo e-mail"';
064400100223           exsr  sr_RoutEnd;
064500100223         endif;
064600100223
064700100223         // -?Override all'"allegato e-mail"?
064800100223         if  §MRAregAll <> *blank;
064900100223           §CM1tips = §MRAregAll;
065000100223           §CM1att  = '01';
065100100226           Qcmd = c_CmdOvrPrtF_2
065200100223                + ' outq(' + %trim(§MRAoutqi) + ')'
065300100223                + ' usrdfndta(''' + TRTCM1ds + ''')';
065400100223         else;
065500100226           Qcmd = c_CmdOvrPrtF_2;
065600100223         endif;
065700100223
065800100223         exsr  sr_ExecCmd;
065900100223
066000100223         if  Qusei <> *blank;
066100100223           O52err = *on;
066200100223           O52msg = 'Fallita override al file di stampa per +
066300100223                     "allegato e-mail"';
066400100223           exsr  sr_RoutEnd;
066500100223         endif;
066600100222
066700100222       ENDSR;
066800100222
066900100222       //--------------------------------------------------------------
067000100223       //?Chiusura file stampa & Cancellazione override.               ?
067100100222       //--------------------------------------------------------------
067200100222       BEGSR sr_DeleteOvr;
067300100222
067400100223         // -?Cancellazine override al "corpo e-mail"?
067500100223         if %open(PrtEMAIL);
067600100223           close PRTEMAIL;
067700100223           Qcmd = c_CmdDltOvr_1;
067800100223           exsr  sr_ExecCmd;
067900100223         endif;
068000100223
068100100223         // -?Cancellazione override all'"allegato e-mail"?
068200100222         if  %open(FIOR52P);
068300100222           close FIOR52P;
068400100223           Qcmd = c_CmdDltOvr_2;
068500100223           exsr  sr_ExecCmd;
068600100222         endif;
068700100222
068800100222       ENDSR;
068900100223
069000100223       //--------------------------------------------------------------
069100100223       //?Esecuzione del comando (già impostato).                      ?
069200100223       //--------------------------------------------------------------
069300100223       BEGSR  sr_ExecCmd;
069400100223
069500100223         clear Qcap0100;
069600100223         Qcabcsdh = *off;
069700100223         Qcapa    = *off;
069800100223         Qcacmdss = *off;
069900100223         Qcaerved = *allX'00';
070000100223
070100100223         clear Qusec;
070200100223         Qusbprv  = %size(Qusec);
070300100223
070400100223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
070500100223                           %size(Qcap0100) : 'CPOP0100' : *omit :
070600100223                           0 : 0 : Qusec);
070700100223
070800100223         //if  Qusei <> *blank;
070900100223         //  ...;
071000100223         //endif;
071100100223
071200100223       ENDSR;
071300100222
071400100222       //--------------------------------------------------------------
071500100222       //?Operazioni finali.                                           ?
071600100222       //--------------------------------------------------------------
071700100222       BEGSR  sr_RoutEnd;
071800100222
071900100224         if  %parms() > 1;
072000100224
072100100224           // -?Chiusura file stampa & Cancellazione override?
072200100224           exsr sr_DeleteOvr;
072300100224
072400100224           // -?Chiusura applicazioni prima richiamate?
072500100224           clear  TIBS69ds;
072600100224           TIBS69ds.I69tla = 'C';
072700100224           tibs69r( tibs69ds :
072800100224                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
072900100224
073000100224         endif;
073100100223
073200100223         // -?Uscita?
073300100223         return;
073400100222
073500100222       ENDSR;
073600100222
073700100222      /end-free
073800100223
073900100223       //--------------------------------------------------------------
074000100223       //?Specifiche di stampa.                                        ?
074100100223       //--------------------------------------------------------------
074200100223
074300100223     oPrtEMAIL  e            PRTparm     1
074400100223     o                       O_txt
