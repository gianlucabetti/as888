000100100310       //==============================================================
000200100909       //?E-mail di conferma appuntamento                              ?
000300100310       //==============================================================
000400100310
000500100310       //--------------------------------------------------------------
000600100310       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100310       //--------------------------------------------------------------
000800100310
000900100310       //--------------------------------------------------------------
001000100310       //?Specifiche di controllo.                                     ?
001100100310       //--------------------------------------------------------------
001200100310
001300100310     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001400100310
001500100310       //--------------------------------------------------------------
001600100310       //?Dichiarazione file.
001700100310       //--------------------------------------------------------------
001800100310
001900100310
002000100310       // -?Organigramma filiale/aziendale?
002100100310     fAZORG01L  if   e           k disk
002200100324
002300100324       // -?Tabelle?
002400100324     fTNTBE01L  if   e           k disk
002500130830
002600130830       // -?Dati dei Commerciali?
002700130830     fAZNTC01L  if   e           k disk
002800100310
002900100310       // -?Spool da inviare via e-mail:?
003000100310       //   ·?Corpo del messaggio di posta elettronica?
003100100310     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003200100310
003300100310       //--------------------------------------------------------------
003400100310       //?Definizione costanti.                                        ?
003500100310       //--------------------------------------------------------------
003600100310
003700100310       // -?Dati di default?
003800100310     d c_Mittente      c                   const('ced')
003900120305     d c_at            c                   const('@brt.it')
004000100310     d c_Sede          c                   const('046')
004100100310
004200100310       // -?Comandi di override al PrtF "corpo e-mail"?
004300100310     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004400100310     d                                           file(PRTEMAIL) +
004500100310     d                                           pagesize(66 132) +
004600100310     d                                           lpi(6) cpi(10) +
004700100310     d                                           ovrscope(*actgrpdfn) +
004800100310     d                                           ')
004900100310     d c_CmdDltOvr     c                   const('DLTOVR +
005000100310     d                                            file(PRTEMAIL) +
005100100310     d                                            lvl(*actgrpdfn)')
005200100310
005300100310       //--------------------------------------------------------------
005400100310       //?Definizione schiere.                                         ?
005500100310       //--------------------------------------------------------------
005600100310
005700100310
005800100310       //--------------------------------------------------------------
005900100310       //?Definizione aree dati.                                       ?
006000100310       //--------------------------------------------------------------
006100100310
006200100310       // -?Dati utente?
006300100310     d §AzUte        e ds                  extname(AZUTE00F)
006400100310     d                                     dtaara
006500100310     d §DatiUte      e ds                  extname(dDatiUte)
006600100310     d                                     dtaara
006700100310
006800100310       //--------------------------------------------------------------
006900100310       //?Definizione strutture dati.                                  ?
007000100310       //--------------------------------------------------------------
007100100310
007200100310       // -?Parametri ricevuti?
007300100310     d KPJBA         e ds
007400100310     d TRMK23ds      e ds                  inz
007500100310
007600100310       // -?Parametri per Reperimento dati utente?
007700100310     d TIBS34ds      e ds                  inz
007800100310
007900100310       // -?Tab. "MRA" = Bart-Mailing x TRMK23R?
008000100413     d dMRAmk23      e ds                  inz
008100100310
008200100310       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
008300100310     d TRTCM1ds      e ds                  inz
008400100310       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
008500100310     d   §CM1mitt    e                     inz('ced')
008600100310       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
008700120305     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
008800100310       //  ?·§CM1tips = Tipo lettera via e-mail:?
008900100310       //   ?"LET" = testo allegato in corpo con logo?
009000100310       //           ?(richiede righe libere iniziali per il logo)?
009100100310       //   ?"COR" = testo integrato senza logo?
009200100310       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
009300100310       //   ?"SP1" = spool allegato (in formato PDF)?
009400100413     d   §CM1tips    e                     inz('CM1')
009500100310       //  ?·§CM1po   = Filiale?
009600100310     d   §CM1po      e                     inz('046')
009700100310       //  ?·§CM1var  = Oggetto e-mail?
009800100413     d*//§CM1var     e                     inz('*OBJM*+
009900120305     d*//                                  BRT: Conferma appuntamento')
010000100413     d   §CM1var     e                     inz('*OBJM*')
010100100310       //  ?·§CM1sts  = Stato?
010200100413     d   §CM1sts     e                     inz('H')
010300100310       //  ?·§CM1idp  = Id processo?
010400100310     d   §CM1idp     e                     inz('3')
010500100310       //  ?·§CM1att  = Numero attach corrente?
010600100310     d   §CM1att     e                     inz('01')
010700100310       //  ?·§CM1totatt  = Numero attach totali?
010800100310     d   §CM1totatt  e                     inz('01')
010900100310
011000100310       // -?Status ds?
011100100310     d Status         sds
011200100310     d  SDSpgm           *proc
011300100310
011400100310       //--------------------------------------------------------------
011500100310       //?Definizione variabili.                                       ?
011600100310       //--------------------------------------------------------------
011700100310
011800100310       // -?Indici di schiera?
011900100310     d xx              s              3  0 inz
012000100310
012100100310       // -?Campi di stampa?
012200100310     d O_txt           s            132    inz
012300130830
012400130830       // -?Memorizzazione dati da AZNTC00F?
012500130830     d wAppNTCname     s                   inz  like(NTCrnt)
012600130830     d*// wAppNTCmail     s                   inz  like(NTCrnt)
012700130830     d wAttNTCname     s                   inz  like(NTCrnt)
012800130830     d wAttNTCmail     s                   inz  like(NTCrnt)
012900100310
013000100310       // -?Campi di comodo?
013100100310     d wDate_EUR       s               d   datfmt(*eur)  inz
013200100310     d wDate           s              8  0 inz
013300100310
013400100310       //--------------------------------------------------------------
013500100310       //?Definizione prototipi procedure.                             ?
013600100310       //--------------------------------------------------------------
013700100310
013800100310       // -?Reperimento dati utente?
013900100310      /copy gaitrasrc/srcProtoPR,TIBS34R
014000100413
014100100413       // -?Gestione numeratori in GRU?
014200100413     d TRUL33ds      e ds                  inz
014300100413     d   I33tla      e                     inz('L')
014400100413     d   I33ope      e                     inz(*zero)
014500100413     d   I33cnu      e                     inz(024)
014600100413     d   I33num      e                     inz(1)
014700100413      /copy gaitrasrc/srcProtoPR,TRUL33R
014800100310
014900100310       // -?Parametri API QCAPCMD (Process Commands)?
015000100310     d Qcmd            s           2048    inz  varying
015100100310      /copy qSysInc/qRpgleSrc,QCAPCMD
015200100310       // -?API QCAPCMD (Process Commands)?
015300100310      /copy gaitrasrc/srcProtoPR,QCAPCMD
015400100310
015500100310       // -?Parametri gestione errori API.?
015600100310      /copy qsysinc/qrpglesrc,QUSEC
015700100310
015800100310       //--------------------------------------------------------------
015900100310       //?Definizione key-list.                                        ?
016000100310       //--------------------------------------------------------------
016100100310
016200100324       // -?File TNTBE01L?
016300100324     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016400100324     d                                     prefix(k_)   inz
016500130830
016600130830       // -?File AZNTC01L?
016700130830     d k_azntc01     e ds                  extname(AZNTC01L : *key)
016800130830     d                                     prefix(k_)   inz
016900100310
017000100310       //--------------------------------------------------------------
017100100310       //?M A I N - L I N E                                            ?
017200100310       //--------------------------------------------------------------
017300100310
017400100310     c     *Entry        plist
017500100310     c                   parm                    KPJBA
017600100310
017700100310      /free
017800100310
017900100310       // -?Operazioni iniziali?
018000100310       exsr  sr_RoutInz;
018100100310
018200100310       // -?Stampa (ed invio) e-mail?
018300100310       exsr  sr_PrtEmail;
018400100310
018500100310       // -?Operazioni finali?
018600100310       exsr  sr_RoutEnd;
018700100310
018800100310       //--------------------------------------------------------------
018900100310       //?Operazioni iniziali.                                         ?
019000100310       //--------------------------------------------------------------
019100100310       BEGSR  sr_RoutInz;
019200100310
019300100310         *inLR = *on;
019400100310
019500100310         // -?Impostazione parametri ricevuti?
019600100310         clear  TRMK23ds;
019700100310         if  %parms > *zero   and   KPJBU <> *blank;
019800100310           TRMK23ds = KPJBU;
019900100310         endif;
020000100310         Omk23err = *off;
020100100310         clear  Omk23msg;
020200100310
020300100310         // -?Reperimento dati del lavoro?
020400100310         exsr  sr_DatiJob;
020500100310
020600100310         // -?*-=»>  Solo TEST in stampa  <«=-*?
020700100310         if  %parms = *zero   or   KPJBU = *blank;
020800100310           open  PrtEMAIL;
020900100310           exsr  sr_TEST;
021000100310           leavesr;
021100100310         endif;
021200100310
021300100310         // -?Reperimento tab. "MRA"?
021400100310         exsr  sr_TabMRA;
021500100310
021600100310         // -?Reperimento dati comm.le appuntamento?
021700100310         exsr  sr_CommApp;
021800100310
021900100310         // -?Reperimento dati comm.le attività?
022000100310         exsr  sr_CommAtt;
022100100413
022200100413         // -?Reperimento nuovo progressivo da numeratore "024"?
022300100413         exsr  sr_Num024;
022400100310
022500100310         // -?Override ai file di stampa?
022600100310         //  ?(per impostarvi i dati per l'invio via e-mail)?
022700100310         exsr sr_Override;
022800100310
022900100310         // -?Apertura file di stampa?
023000100310         open  PrtEMAIL;
023100100310
023200100310       ENDSR;
023300100310
023400100310       //--------------------------------------------------------------
023500100310       //?Reperimento Dati del job (Utente/Operativi).                 ?
023600100310       //--------------------------------------------------------------
023700100310       BEGSR  sr_DatiJob;
023800100310
023900100310         in(E) §AzUte;
024000100310         if NOT %error;
024100100310           in(E) §DatiUte;
024200100310         endif;
024300100310         if %error or RSut = *blanks;
024400100310           clear TIBS34ds;
024500100310           tibs34r(tibs34ds);
024600100310           in §AzUte;
024700100310           in §DatiUte;
024800100310         endif;
024900100310
025000100310       ENDSR;
025100100310
025200100310       //--------------------------------------------------------------
025300100310       //?Reperimento dati da tab. "MRA".                              ?
025400100310       //--------------------------------------------------------------
025500100310       BEGSR  sr_TabMRA;
025600100310
025700100413         clear  dMRAmk23;
025800100324
025900100324         k_TBEcod = 'MRA';
026000100324         k_TBEke1 = SDSpgm;
026100100324         k_TBEke2 = *blank;
026200100324
026300100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
026400100324
026500100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
026600100413           dMRAmk23 = TBEuni;
026700100310         else;
026800100310           Omk23err = *on;
026900100324           Omk23msg = 'Parametri "' + %trimr(k_TBEke1) +
027000100310                      '" non reperiti in tab. "MRA"';
027100100310           exsr  sr_RoutEnd;
027200100310         endif;
027300100310
027400100310       ENDSR;
027500100310
027600100310       //--------------------------------------------------------------
027700100310       //?Reperimento dati del Commerciale Appuntamento                ?
027800100310       //--------------------------------------------------------------
027900100310       BEGSR  sr_CommApp;
028000100310
028100130830         clear  wAppNTCname;
028200130830         //clear  wAppNTCmail;
028300130830
028400130830         k_NTCcmm = Imk23cmm;
028500100324
028600130830         // ·?Nominativo?
028700130830         k_NTCtnt = '01';
028800100324
028900130830         chain  %kds( k_azntc01 )  AZNTC000;
029000100324
029100130830         if  %found(AZNTC01L);
029200130830             wAppNTCname = NTCrnt;
029300130830         else;
029400100310           Omk23err = *on;
029500130830           Omk23msg = 'Descrizione comm.le "' + %trimr(k_TBEke1) +
029600130830                      '" non reperita nel file AZNTC00F';
029700100310           exsr  sr_RoutEnd;
029800100310         endif;
029900130830
030000130830         //// ·?e-Mail?
030100130830         //k_NTCtnt = '02';
030200130830         //
030300130830         //chain  %kds( k_azntc01 )  AZNTC000;
030400130830         //
030500130830         //if  %found(AZNTC01L);
030600130830         //    wAppNTCmail = NTCrnt;
030700130830         //else;
030800130830         //  Omk23err = *on;
030900130830         //  Omk23msg = 'E-Mail comm.le "' + %trimr(k_TBEke1) +
031000130830         //             '" non reperita nel file AZNTC00F';
031100130830         //  exsr  sr_RoutEnd;
031200130830         //endif;
031300100310
031400100310       ENDSR;
031500100310
031600100310       //--------------------------------------------------------------
031700100310       //?Reperimento dati del Commerciale Attività                    ?
031800100310       //--------------------------------------------------------------
031900100310       BEGSR  sr_CommAtt;
032000130830
032100130830         clear  wAttNTCname;
032200130830         clear  wAttNTCmail;
032300130830
032400130830         k_NTCcmm = Imk23cma;
032500130830
032600130830         // ·?Nominativo?
032700130830         k_NTCtnt = '01';
032800130830
032900130830         chain  %kds( k_azntc01 )  AZNTC000;
033000130830
033100130830         if  %found(AZNTC01L);
033200130830             wAttNTCname = NTCrnt;
033300130830         else;
033400130830           Omk23err = *on;
033500130830           Omk23msg = 'Descrizione comm.le "' + %trimr(k_TBEke1) +
033600130830                      '" non reperita nel file AZNTC00F';
033700130830           exsr  sr_RoutEnd;
033800130830         endif;
033900130830
034000130830         // ·?e-Mail?
034100130830         k_NTCtnt = '02';
034200130830
034300130830         chain  %kds( k_azntc01 )  AZNTC000;
034400130830
034500130830         if  %found(AZNTC01L);
034600130830             wAttNTCmail = NTCrnt;
034700130830         else;
034800130830           Omk23err = *on;
034900130830           Omk23msg = 'E-Mail comm.le "' + %trimr(k_TBEke1) +
035000130830                      '" non reperita nel file AZNTC00F';
035100130830           exsr  sr_RoutEnd;
035200130830         endif;
035300100310
035400100310         // -?Decodifica filiale del comm.le attività?
035500100310         ORGfil = Imk23cma / 10000;
035600100310         chain  (ORGfil)  AZORG;
035700100310         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
035800100310           clear  ORGdes;
035900100310           clear  ORGind;
036000100310           clear  ORGcpf;
036100100310           clear  ORGloc;
036200100310           clear  ORGpro;
036300100310           clear  ORGtel;
036400100310           clear  ORGfax;
036500100310         endif;
036600100310
036700100310       ENDSR;
036800100413
036900100413       //--------------------------------------------------------------
037000100413       //?Reperimento progressivo contatore "024"                      ?
037100100413       //--------------------------------------------------------------
037200100413       BEGSR  sr_Num024;
037300100413
037400100413         reset  TRUL33ds;
037500100413
037600100413         kpjbu  = TRUL33ds;
037700100413         trul33r ( KPJBA );
037800100413         TRUL33ds = kpjbu;
037900100413
038000100413         if  O33err <> *zero;
038100100413           Omk23err = *on;
038200100413           Omk23msg = 'Fallito reperimento numeratore "' +
038300100413                      %editc(I33cnu : 'X') + '"';
038400100413           exsr  sr_RoutEnd;
038500100413         endif;
038600100413
038700100413       ENDSR;
038800100310
038900100310       //--------------------------------------------------------------
039000100310       //?Impostazione dati per stampa di prova.                       ?
039100100310       //--------------------------------------------------------------
039200100310       BEGSR  sr_TEST;
039300100310
039400100310         wDate_EUR = %date();
039500100310
039600100310         Imk23cma = 1234567;
039700100310         Imk23cmm = 7654321;
039800100310         Imk23dta = %dec( wDate_EUR : *iso );
039900100310         Imk23ora = 6789;
040000100310         Imk23rst = *all'W';
040100100310         Imk23emr = *all'W';
040200100310         Imk23emr = %subst(imk23emr:1:%len(Imk23emr)-9) = '@pippo.it';
040300100310
040400130830         wAppNTCname = *all'W';
040500130830         //wAppNTCmail = *all'X';
040600130830         wAttNTCname = *all'M';
040700130830         wAttNTCmail = *all'K';
040800100310
040900100310         ORGdes = *all'Y';
041000100310         ORGind = *all'Q';
041100100310         ORGcpf = *all'123456789';
041200100310         ORGloc = *all'W';
041300100310         ORGpro = *all'X';
041400100310         ORGtel = '1234-56789023';
041500100310         ORGfax = '9876-54321098';
041600100310
041700100310       ENDSR;
041800100310
041900100310       //--------------------------------------------------------------
042000100310       //?Preparazione ed invio e-mail.                                ?
042100100310       //--------------------------------------------------------------
042200100310       BEGSR  sr_PrtEmail;
042300100310
042400100310         wDate_EUR = %date( Imk23dta : *iso );
042500100310         wDate = %dec( wDate_EUR );
042600100310
042700100310         // -?Stampa "corpo e-mail"?
042800100317         For  xx = 1  To  13;
042900100310           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
043000100310           select;
043100100310             when  xx = 1;
043200100310               O_txt = %trimr(O_txt) + %triml( Imk23rst );
043300100310             when  xx = 2;
043400130830               O_txt = %trimr(O_txt) + %triml( wAppNTCname );
043500100310             when  xx = 3;
043600100310               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
043700100310             when  xx = 4;
043800100310               O_txt = %trimr(O_txt) + %triml( %editw( Imk23ora : '0 :  ' ) );
043900100317             when  xx = 5;
044000130830               O_txt = %trimr(O_txt) + %triml( wAttNTCname );
044100100317             when  xx = 6;
044200100317               O_txt = %trimr(O_txt) + %triml( ORGdes );
044300100317             when  xx = 7;
044400100317               O_txt = %trimr(O_txt) + %triml( ORGind );
044500100317             when  xx = 8;
044600100317               O_txt = %trimr(O_txt) + %editc( ORGcpf : 'X' );
044700100317             when  xx = 9;
044800100317               O_txt = %trimr(O_txt) + %triml( ORGloc );
044900100317             when  xx = 10;
045000100317               O_txt = %trimr(O_txt) + ORGpro;
045100100317             when  xx = 11;
045200100317               O_txt = %trimr(O_txt) + %triml( ORGtel );
045300100317             when  xx = 12;
045400100317               O_txt = %trimr(O_txt) + %triml( ORGfax );
045500100317             when  xx = 13;
045600130830               O_txt = %trimr(O_txt) + %trim( wAttNTCmail ) + c_at;
045700100310           endsl;
045800100310           O_txt = %trimr(O_txt) + '}{';
045900100310           except  PRTparm;
046000100310         EndFor;
046100100310
046200100310       ENDSR;
046300100310
046400100310       //--------------------------------------------------------------
046500100310       //?Override al file di stampa per impostarvi i dati per         ?
046600100310       //?  l'invio via e-mail.                                        ?
046700100310       //--------------------------------------------------------------
046800100310       BEGSR sr_Override;
046900100310
047000100310         reset TRTCM1ds;
047100100413         if  §MRAmitt <> *blank;
047200100413           §CM1mitt = %trim(§MRAmitt);
047300100413         else;
047400130830           §CM1mitt = %trim(wAttNTCmail);
047500100413         endif;
047600100413         §CM1dst  = %trim(iMK23emr);
047700100413         §CM1tips = §MRAregCor;
047800100310         §CM1po   = %editc( DUTpou : 'X' );
047900100413         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm);
048000100413         §CM1sts  = §MRAsts;
048100100413         §CM1idp  = §MRAidpro;
048200100413         §CM1prog = %subst( %editc( O33nri : 'X' ) :
048300100413                            %size(O33nri) - %size(§CM1prog) + 1 );
048400100310
048500100310         // -?Override al "corpo e-mail"?
048600100413         if  §MRAregCor <> *blank;
048700100310           Qcmd = c_CmdOvrPrtF
048800100413                + ' outq(' + %trim(§MRAoutqi) + ')'
048900100310                + ' usrdfndta(''' + TRTCM1ds + ''')';
049000100310         else;
049100100310           Qcmd = c_CmdOvrPrtF;
049200100310         endif;
049300100310
049400100310         exsr  sr_ExecCmd;
049500100310
049600100310         if  Qusei <> *blank;
049700100310           Omk23err = *on;
049800100310           Omk23msg = 'Fallita override al file di stampa per +
049900100310                       "corpo e-mail"';
050000100310           exsr  sr_RoutEnd;
050100100310         endif;
050200100310
050300100310       ENDSR;
050400100310
050500100310       //--------------------------------------------------------------
050600100310       //?Chiusura file stampa & Cancellazione override.               ?
050700100310       //--------------------------------------------------------------
050800100310       BEGSR sr_DeleteOvr;
050900100310
051000100310         // -?Cancellazione override al "corpo e-mail"?
051100100310         if %open(PrtEMAIL);
051200100310           close PRTEMAIL;
051300100310           Qcmd = c_CmdDltOvr;
051400100310           exsr  sr_ExecCmd;
051500100310         endif;
051600100310
051700100310       ENDSR;
051800100310
051900100310       //--------------------------------------------------------------
052000100310       //?Esecuzione del comando (già impostato).                      ?
052100100310       //--------------------------------------------------------------
052200100310       BEGSR  sr_ExecCmd;
052300100310
052400100310         clear Qcap0100;
052500100310         Qcabcsdh = *off;
052600100310         Qcapa    = *off;
052700100310         Qcacmdss = *off;
052800100310         Qcaerved = *allX'00';
052900100310
053000100310         clear Qusec;
053100100310         Qusbprv  = %size(Qusec);
053200100310
053300100310         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
053400100310                           %size(Qcap0100) : 'CPOP0100' : *omit :
053500100310                           0 : 0 : Qusec);
053600100310
053700100310         //if  Qusei <> *blank;
053800100310         //  ...;
053900100310         //endif;
054000100310
054100100310       ENDSR;
054200100310
054300100310       //--------------------------------------------------------------
054400100310       //?Operazioni finali.                                           ?
054500100310       //--------------------------------------------------------------
054600100310       BEGSR  sr_RoutEnd;
054700100310
054800100310         if  %parms > *zero   and   KPJBU <> *blank;
054900100310
055000100310           // -?Chiusura file stampa & Cancellazione override?
055100100310           exsr sr_DeleteOvr;
055200100331
055300100331           // -?Restituzione parametri?
055400100331           kpjbu = TRMK23ds;
055500100310
055600100310         endif;
055700100310
055800100310         // -?Uscita?
055900100310         return;
056000100310
056100100310       ENDSR;
056200100310
056300100310      /end-free
056400100310
056500100310       //--------------------------------------------------------------
056600100310       //?Specifiche di stampa.                                        ?
056700100310       //--------------------------------------------------------------
056800100310
056900100310     oPrtEMAIL  e            PRTparm     1
057000100310     o                       O_txt
