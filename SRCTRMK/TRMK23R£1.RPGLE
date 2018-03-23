000100100310       //==============================================================
000200100909       //?E-mail di conferma appuntamento                              ?
000300100310       //==============================================================
000400100310
000500100310       //--------------------------------------------------------------
000600100310       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100310       //--------------------------------------------------------------
000800100310
000900100310     /*END
001000100310
001100100310       //--------------------------------------------------------------
001200100310       //?Specifiche di controllo.                                     ?
001300100310       //--------------------------------------------------------------
001400100310
001500100310     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600100324     h*// dftactgrp(*no)
001700100310
001800100310       //--------------------------------------------------------------
001900100310       //?Dichiarazione file.
002000100310       //--------------------------------------------------------------
002100100310
002200100310
002300100310       // -?Organigramma filiale/aziendale?
002400100310     fAZORG01L  if   e           k disk
002500100324
002600100324       // -?Tabelle?
002700100324     fTNTBE01L  if   e           k disk
002800100310
002900100310       // -?Spool da inviare via e-mail:?
003000100310       //   ·?Corpo del messaggio di posta elettronica?
003100100310     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003200100310
003300100310       //--------------------------------------------------------------
003400100310       //?Definizione costanti.                                        ?
003500100310       //--------------------------------------------------------------
003600100310
003700100324       // -?Codice Utente / Lingua italiana  in tabelle su TABEL00F?
003800100324     d c_Kut           c                   const(1)
003900100310
004000100310       // -?Dati di default?
004100100310     d c_Mittente      c                   const('ced')
004200120305     d c_at            c                   const('@brt.it')
004300100310     d c_Sede          c                   const('046')
004400100310
004500100310       // -?Comandi di override al PrtF "corpo e-mail"?
004600100310     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004700100310     d                                           file(PRTEMAIL) +
004800100310     d                                           pagesize(66 132) +
004900100310     d                                           lpi(6) cpi(10) +
005000100310     d                                           ovrscope(*actgrpdfn) +
005100100310     d                                           ')
005200100310     d c_CmdDltOvr     c                   const('DLTOVR +
005300100310     d                                            file(PRTEMAIL) +
005400100310     d                                            lvl(*actgrpdfn)')
005500100310
005600100310       //--------------------------------------------------------------
005700100310       //?Definizione schiere.                                         ?
005800100310       //--------------------------------------------------------------
005900100310
006000100310
006100100310       //--------------------------------------------------------------
006200100310       //?Definizione aree dati.                                       ?
006300100310       //--------------------------------------------------------------
006400100310
006500100310       // -?Dati utente?
006600100310     d §AzUte        e ds                  extname(AZUTE00F)
006700100310     d                                     dtaara
006800100310     d §DatiUte      e ds                  extname(dDatiUte)
006900100310     d                                     dtaara
007000100310
007100100310       //--------------------------------------------------------------
007200100310       //?Definizione strutture dati.                                  ?
007300100310       //--------------------------------------------------------------
007400100310
007500100310       // -?Parametri ricevuti?
007600100310     d KPJBA         e ds
007700100310     d TRMK23ds      e ds                  inz
007800100310
007900100310       // -?Parametri per Reperimento dati utente?
008000100310     d TIBS34ds      e ds                  inz
008100100310
008200100310       // -?Tab. "HTM" = Dati per testo html?
008300100310     d dHTM_app      e ds                  extname(dHTM)
008400100310     d                                     inz  prefix(App)
008500100310     d dHTM_att      e ds                  extname(dHTM)
008600100310     d                                     inz  prefix(Att)
008700100310
008800100310       // -?Tab. "MRA" = Bart-Mailing x TRMK23R?
008900100413     d dMRAmk23      e ds                  inz
009000100310
009100100310       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
009200100310     d TRTCM1ds      e ds                  inz
009300100310       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
009400100310     d   §CM1mitt    e                     inz('ced')
009500100310       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
009600120305     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
009700100310       //  ?·§CM1tips = Tipo lettera via e-mail:?
009800100310       //   ?"LET" = testo allegato in corpo con logo?
009900100310       //           ?(richiede righe libere iniziali per il logo)?
010000100310       //   ?"COR" = testo integrato senza logo?
010100100310       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
010200100310       //   ?"SP1" = spool allegato (in formato PDF)?
010300100413     d   §CM1tips    e                     inz('CM1')
010400100310       //  ?·§CM1po   = Filiale?
010500100310     d   §CM1po      e                     inz('046')
010600100310       //  ?·§CM1var  = Oggetto e-mail?
010700100413     d*//§CM1var     e                     inz('*OBJM*+
010800120305     d*//                                  BRT: Conferma appuntamento')
010900100413     d   §CM1var     e                     inz('*OBJM*')
011000100310       //  ?·§CM1sts  = Stato?
011100100413     d   §CM1sts     e                     inz('H')
011200100310       //  ?·§CM1idp  = Id processo?
011300100310     d   §CM1idp     e                     inz('3')
011400100310       //  ?·§CM1att  = Numero attach corrente?
011500100310     d   §CM1att     e                     inz('01')
011600100310       //  ?·§CM1totatt  = Numero attach totali?
011700100310     d   §CM1totatt  e                     inz('01')
011800100310
011900100310       // -?Status ds?
012000100310     d Status         sds
012100100310     d  SDSpgm           *proc
012200100310
012300100310       //--------------------------------------------------------------
012400100310       //?Definizione variabili.                                       ?
012500100310       //--------------------------------------------------------------
012600100310
012700100310       // -?Indici di schiera?
012800100310     d xx              s              3  0 inz
012900100310
013000100310       // -?Campi di stampa?
013100100310     d O_txt           s            132    inz
013200100310
013300100310       // -?Campi di comodo?
013400100310     d wDate_EUR       s               d   datfmt(*eur)  inz
013500100310     d wDate           s              8  0 inz
013600100310
013700100310       //--------------------------------------------------------------
013800100310       //?Definizione prototipi procedure.                             ?
013900100310       //--------------------------------------------------------------
014000100310
014100100310       // -?Reperimento dati utente?
014200100310      /copy gaitrasrc/srcProtoPR,TIBS34R
014300100413
014400100413       // -?Gestione numeratori in GRU?
014500100413     d TRUL33ds      e ds                  inz
014600100413     d   I33tla      e                     inz('L')
014700100413     d   I33ope      e                     inz(*zero)
014800100413     d   I33cnu      e                     inz(024)
014900100413     d   I33num      e                     inz(1)
015000100413      /copy gaitrasrc/srcProtoPR,TRUL33R
015100100310
015200100310       // -?Parametri API QCAPCMD (Process Commands)?
015300100310     d Qcmd            s           2048    inz  varying
015400100310      /copy qSysInc/qRpgleSrc,QCAPCMD
015500100310       // -?API QCAPCMD (Process Commands)?
015600100310      /copy gaitrasrc/srcProtoPR,QCAPCMD
015700100310
015800100310       // -?Parametri gestione errori API.?
015900100310      /copy qsysinc/qrpglesrc,QUSEC
016000100310
016100100310       //--------------------------------------------------------------
016200100310       //?Definizione key-list.                                        ?
016300100310       //--------------------------------------------------------------
016400100310
016500100324       // -?File TNTBE01L?
016600100324     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016700100324     d                                     prefix(k_)   inz
016800100310
016900100310       //--------------------------------------------------------------
017000100310       //?M A I N - L I N E                                            ?
017100100310       //--------------------------------------------------------------
017200100310
017300100310     c     *Entry        plist
017400100310     c                   parm                    KPJBA
017500100310
017600100310      /free
017700100310
017800100310       // -?Operazioni iniziali?
017900100310       exsr  sr_RoutInz;
018000100310
018100100310       // -?Stampa (ed invio) e-mail?
018200100310       exsr  sr_PrtEmail;
018300100310
018400100310       // -?Operazioni finali?
018500100310       exsr  sr_RoutEnd;
018600100310
018700100310       //--------------------------------------------------------------
018800100310       //?Operazioni iniziali.                                         ?
018900100310       //--------------------------------------------------------------
019000100310       BEGSR  sr_RoutInz;
019100100310
019200100310         *inLR = *on;
019300100310
019400100310         // -?Impostazione parametri ricevuti?
019500100310         clear  TRMK23ds;
019600100310         if  %parms > *zero   and   KPJBU <> *blank;
019700100310           TRMK23ds = KPJBU;
019800100310         endif;
019900100310         Omk23err = *off;
020000100310         clear  Omk23msg;
020100100310
020200100310         // -?Reperimento dati del lavoro?
020300100310         exsr  sr_DatiJob;
020400100310
020500100310         // -?*-=»>  Solo TEST in stampa  <«=-*?
020600100310         if  %parms = *zero   or   KPJBU = *blank;
020700100310           open  PrtEMAIL;
020800100310           exsr  sr_TEST;
020900100310           leavesr;
021000100310         endif;
021100100310
021200100310         // -?Reperimento tab. "MRA"?
021300100310         exsr  sr_TabMRA;
021400100310
021500100310         // -?Reperimento dati comm.le appuntamento?
021600100310         exsr  sr_CommApp;
021700100310
021800100310         // -?Reperimento dati comm.le attività?
021900100310         exsr  sr_CommAtt;
022000100413
022100100413         // -?Reperimento nuovo progressivo da numeratore "024"?
022200100413         exsr  sr_Num024;
022300100310
022400100310         // -?Override ai file di stampa?
022500100310         //  ?(per impostarvi i dati per l'invio via e-mail)?
022600100310         exsr sr_Override;
022700100310
022800100310         // -?Apertura file di stampa?
022900100310         open  PrtEMAIL;
023000100310
023100100310       ENDSR;
023200100310
023300100310       //--------------------------------------------------------------
023400100310       //?Reperimento Dati del job (Utente/Operativi).                 ?
023500100310       //--------------------------------------------------------------
023600100310       BEGSR  sr_DatiJob;
023700100310
023800100310         in(E) §AzUte;
023900100310         if NOT %error;
024000100310           in(E) §DatiUte;
024100100310         endif;
024200100310         if %error or RSut = *blanks;
024300100310           clear TIBS34ds;
024400100310           tibs34r(tibs34ds);
024500100310           in §AzUte;
024600100310           in §DatiUte;
024700100310         endif;
024800100310
024900100310       ENDSR;
025000100310
025100100310       //--------------------------------------------------------------
025200100310       //?Reperimento dati da tab. "MRA".                              ?
025300100310       //--------------------------------------------------------------
025400100310       BEGSR  sr_TabMRA;
025500100310
025600100413         clear  dMRAmk23;
025700100324
025800100324         k_TBEcod = 'MRA';
025900100324         k_TBEke1 = SDSpgm;
026000100324         k_TBEke2 = *blank;
026100100324
026200100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
026300100324
026400100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
026500100413           dMRAmk23 = TBEuni;
026600100310         else;
026700100310           Omk23err = *on;
026800100324           Omk23msg = 'Parametri "' + %trimr(k_TBEke1) +
026900100310                      '" non reperiti in tab. "MRA"';
027000100310           exsr  sr_RoutEnd;
027100100310         endif;
027200100310
027300100310       ENDSR;
027400100310
027500100310       //--------------------------------------------------------------
027600100310       //?Reperimento dati del Commerciale Appuntamento                ?
027700100310       //--------------------------------------------------------------
027800100310       BEGSR  sr_CommApp;
027900100310
028000100310         clear  dHTM_app;
028100100324
028200100324         k_TBEcod = 'HTM';
028300100324         k_TBEke1 = %editc( IMK23cmm : 'X' );
028400100324         k_TBEke2 = 'CC';
028500100324
028600100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
028700100324
028800100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
028900100324           dHTM_app = TBEuni;
029000100310         else;
029100100310           Omk23err = *on;
029200100324           Omk23msg = 'Dati agente "' + %trimr(k_TBEke1) +
029300100310                      '" non reperiti in tab. "HTM"';
029400100310           exsr  sr_RoutEnd;
029500100310         endif;
029600100310
029700100310       ENDSR;
029800100310
029900100310       //--------------------------------------------------------------
030000100310       //?Reperimento dati del Commerciale Attività                    ?
030100100310       //--------------------------------------------------------------
030200100310       BEGSR  sr_CommAtt;
030300100310
030400100310         clear  dHTM_att;
030500100324
030600100324         k_TBEcod = 'HTM';
030700100324         k_TBEke1 = %editc( IMK23cma : 'X' );
030800100324         k_TBEke2 = 'CC';
030900100324
031000100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
031100100324
031200100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
031300100324           dHTM_att = TBEuni;
031400100310         else;
031500100310           Omk23err = *on;
031600100324           Omk23msg = 'Dati agente "' + %trimr(k_TBEke1) +
031700100310                      '" non reperiti in tab. "HTM"';
031800100310           exsr  sr_RoutEnd;
031900100310         endif;
032000100310
032100100310         // -?Decodifica filiale del comm.le attività?
032200100310         ORGfil = Imk23cma / 10000;
032300100310         chain  (ORGfil)  AZORG;
032400100310         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
032500100310           clear  ORGdes;
032600100310           clear  ORGind;
032700100310           clear  ORGcpf;
032800100310           clear  ORGloc;
032900100310           clear  ORGpro;
033000100310           clear  ORGtel;
033100100310           clear  ORGfax;
033200100310         endif;
033300100310
033400100310       ENDSR;
033500100413
033600100413       //--------------------------------------------------------------
033700100413       //?Reperimento progressivo contatore "024"                      ?
033800100413       //--------------------------------------------------------------
033900100413       BEGSR  sr_Num024;
034000100413
034100100413         reset  TRUL33ds;
034200100413
034300100413         kpjbu  = TRUL33ds;
034400100413         trul33r ( KPJBA );
034500100413         TRUL33ds = kpjbu;
034600100413
034700100413         if  O33err <> *zero;
034800100413           Omk23err = *on;
034900100413           Omk23msg = 'Fallito reperimento numeratore "' +
035000100413                      %editc(I33cnu : 'X') + '"';
035100100413           exsr  sr_RoutEnd;
035200100413         endif;
035300100413
035400100413       ENDSR;
035500100310
035600100310       //--------------------------------------------------------------
035700100310       //?Impostazione dati per stampa di prova.                       ?
035800100310       //--------------------------------------------------------------
035900100310       BEGSR  sr_TEST;
036000100310
036100100310         wDate_EUR = %date();
036200100310
036300100310         Imk23cma = 1234567;
036400100310         Imk23cmm = 7654321;
036500100310         Imk23dta = %dec( wDate_EUR : *iso );
036600100310         Imk23ora = 6789;
036700100310         Imk23rst = *all'W';
036800100310         Imk23emr = *all'W';
036900100310         Imk23emr = %subst(imk23emr:1:%len(Imk23emr)-9) = '@pippo.it';
037000100310
037100100310         App§HTMnmc = *all'W';
037200100310         App§HTMind = *all'X';
037300100310         Att§HTMnmc = *all'M';
037400100310         Att§HTMind = *all'K';
037500100310
037600100310         ORGdes = *all'Y';
037700100310         ORGind = *all'Q';
037800100310         ORGcpf = *all'123456789';
037900100310         ORGloc = *all'W';
038000100310         ORGpro = *all'X';
038100100310         ORGtel = '1234-56789023';
038200100310         ORGfax = '9876-54321098';
038300100310
038400100310       ENDSR;
038500100310
038600100310       //--------------------------------------------------------------
038700100310       //?Preparazione ed invio e-mail.                                ?
038800100310       //--------------------------------------------------------------
038900100310       BEGSR  sr_PrtEmail;
039000100310
039100100310         wDate_EUR = %date( Imk23dta : *iso );
039200100310         wDate = %dec( wDate_EUR );
039300100310
039400100310         // -?Stampa "corpo e-mail"?
039500100317         For  xx = 1  To  13;
039600100310           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
039700100310           select;
039800100310             when  xx = 1;
039900100310               O_txt = %trimr(O_txt) + %triml( Imk23rst );
040000100310             when  xx = 2;
040100100310               O_txt = %trimr(O_txt) + %triml( App§HTMnmc );
040200100310             when  xx = 3;
040300100310               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
040400100310             when  xx = 4;
040500100310               O_txt = %trimr(O_txt) + %triml( %editw( Imk23ora : '0 :  ' ) );
040600100317             when  xx = 5;
040700100317               O_txt = %trimr(O_txt) + %triml( Att§HTMnmc );
040800100317             when  xx = 6;
040900100317               O_txt = %trimr(O_txt) + %triml( ORGdes );
041000100317             when  xx = 7;
041100100317               O_txt = %trimr(O_txt) + %triml( ORGind );
041200100317             when  xx = 8;
041300100317               O_txt = %trimr(O_txt) + %editc( ORGcpf : 'X' );
041400100317             when  xx = 9;
041500100317               O_txt = %trimr(O_txt) + %triml( ORGloc );
041600100317             when  xx = 10;
041700100317               O_txt = %trimr(O_txt) + ORGpro;
041800100317             when  xx = 11;
041900100317               O_txt = %trimr(O_txt) + %triml( ORGtel );
042000100317             when  xx = 12;
042100100317               O_txt = %trimr(O_txt) + %triml( ORGfax );
042200100317             when  xx = 13;
042300110519               O_txt = %trimr(O_txt) + %trim( Att§HTMind ) + c_at;
042400100310           endsl;
042500100310           O_txt = %trimr(O_txt) + '}{';
042600100310           except  PRTparm;
042700100310         EndFor;
042800100310
042900100310       ENDSR;
043000100310
043100100310       //--------------------------------------------------------------
043200100310       //?Override al file di stampa per impostarvi i dati per         ?
043300100310       //?  l'invio via e-mail.                                        ?
043400100310       //--------------------------------------------------------------
043500100310       BEGSR sr_Override;
043600100310
043700100310         reset TRTCM1ds;
043800100413         if  §MRAmitt <> *blank;
043900100413           §CM1mitt = %trim(§MRAmitt);
044000100413         else;
044100100413           §CM1mitt = %trim(Att§HTMind);
044200100413         endif;
044300100413         §CM1dst  = %trim(iMK23emr);
044400100413         §CM1tips = §MRAregCor;
044500100310         §CM1po   = %editc( DUTpou : 'X' );
044600100413         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm);
044700100413         §CM1sts  = §MRAsts;
044800100413         §CM1idp  = §MRAidpro;
044900100413         §CM1prog = %subst( %editc( O33nri : 'X' ) :
045000100413                            %size(O33nri) - %size(§CM1prog) + 1 );
045100100310
045200100310         // -?Override al "corpo e-mail"?
045300100413         if  §MRAregCor <> *blank;
045400100310           Qcmd = c_CmdOvrPrtF
045500100413                + ' outq(' + %trim(§MRAoutqi) + ')'
045600100310                + ' usrdfndta(''' + TRTCM1ds + ''')';
045700100310         else;
045800100310           Qcmd = c_CmdOvrPrtF;
045900100310         endif;
046000100310
046100100310         exsr  sr_ExecCmd;
046200100310
046300100310         if  Qusei <> *blank;
046400100310           Omk23err = *on;
046500100310           Omk23msg = 'Fallita override al file di stampa per +
046600100310                       "corpo e-mail"';
046700100310           exsr  sr_RoutEnd;
046800100310         endif;
046900100310
047000100310       ENDSR;
047100100310
047200100310       //--------------------------------------------------------------
047300100310       //?Chiusura file stampa & Cancellazione override.               ?
047400100310       //--------------------------------------------------------------
047500100310       BEGSR sr_DeleteOvr;
047600100310
047700100310         // -?Cancellazione override al "corpo e-mail"?
047800100310         if %open(PrtEMAIL);
047900100310           close PRTEMAIL;
048000100310           Qcmd = c_CmdDltOvr;
048100100310           exsr  sr_ExecCmd;
048200100310         endif;
048300100310
048400100310       ENDSR;
048500100310
048600100310       //--------------------------------------------------------------
048700100310       //?Esecuzione del comando (già impostato).                      ?
048800100310       //--------------------------------------------------------------
048900100310       BEGSR  sr_ExecCmd;
049000100310
049100100310         clear Qcap0100;
049200100310         Qcabcsdh = *off;
049300100310         Qcapa    = *off;
049400100310         Qcacmdss = *off;
049500100310         Qcaerved = *allX'00';
049600100310
049700100310         clear Qusec;
049800100310         Qusbprv  = %size(Qusec);
049900100310
050000100310         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050100100310                           %size(Qcap0100) : 'CPOP0100' : *omit :
050200100310                           0 : 0 : Qusec);
050300100310
050400100310         //if  Qusei <> *blank;
050500100310         //  ...;
050600100310         //endif;
050700100310
050800100310       ENDSR;
050900100310
051000100310       //--------------------------------------------------------------
051100100310       //?Operazioni finali.                                           ?
051200100310       //--------------------------------------------------------------
051300100310       BEGSR  sr_RoutEnd;
051400100310
051500100310         if  %parms > *zero   and   KPJBU <> *blank;
051600100310
051700100310           // -?Chiusura file stampa & Cancellazione override?
051800100310           exsr sr_DeleteOvr;
051900100331
052000100331           // -?Restituzione parametri?
052100100331           kpjbu = TRMK23ds;
052200100310
052300100310         endif;
052400100310
052500100310         // -?Uscita?
052600100310         return;
052700100310
052800100310       ENDSR;
052900100310
053000100310      /end-free
053100100310
053200100310       //--------------------------------------------------------------
053300100310       //?Specifiche di stampa.                                        ?
053400100310       //--------------------------------------------------------------
053500100310
053600100310     oPrtEMAIL  e            PRTparm     1
053700100310     o                       O_txt
