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
002800130830
002900130830       // -?Dati dei Commerciali?
003000130830     fAZNTC01L  if   e           k disk
003100100310
003200100310       // -?Spool da inviare via e-mail:?
003300100310       //   ·?Corpo del messaggio di posta elettronica?
003400100310     fPrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
003500100310
003600100310       //--------------------------------------------------------------
003700100310       //?Definizione costanti.                                        ?
003800100310       //--------------------------------------------------------------
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
008200100310       // -?Tab. "MRA" = Bart-Mailing x TRMK23R?
008300100413     d dMRAmk23      e ds                  inz
008400100310
008500100310       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
008600100310     d TRTCM1ds      e ds                  inz
008700100310       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
008800100310     d   §CM1mitt    e                     inz('ced')
008900100310       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
009000120305     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
009100100310       //  ?·§CM1tips = Tipo lettera via e-mail:?
009200100310       //   ?"LET" = testo allegato in corpo con logo?
009300100310       //           ?(richiede righe libere iniziali per il logo)?
009400100310       //   ?"COR" = testo integrato senza logo?
009500100310       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
009600100310       //   ?"SP1" = spool allegato (in formato PDF)?
009700100413     d   §CM1tips    e                     inz('CM1')
009800100310       //  ?·§CM1po   = Filiale?
009900100310     d   §CM1po      e                     inz('046')
010000100310       //  ?·§CM1var  = Oggetto e-mail?
010100100413     d*//§CM1var     e                     inz('*OBJM*+
010200120305     d*//                                  BRT: Conferma appuntamento')
010300100413     d   §CM1var     e                     inz('*OBJM*')
010400100310       //  ?·§CM1sts  = Stato?
010500100413     d   §CM1sts     e                     inz('H')
010600100310       //  ?·§CM1idp  = Id processo?
010700100310     d   §CM1idp     e                     inz('3')
010800100310       //  ?·§CM1att  = Numero attach corrente?
010900100310     d   §CM1att     e                     inz('01')
011000100310       //  ?·§CM1totatt  = Numero attach totali?
011100100310     d   §CM1totatt  e                     inz('01')
011200100310
011300100310       // -?Status ds?
011400100310     d Status         sds
011500100310     d  SDSpgm           *proc
011600100310
011700100310       //--------------------------------------------------------------
011800100310       //?Definizione variabili.                                       ?
011900100310       //--------------------------------------------------------------
012000100310
012100100310       // -?Indici di schiera?
012200100310     d xx              s              3  0 inz
012300100310
012400100310       // -?Campi di stampa?
012500100310     d O_txt           s            132    inz
012600130830
012700130830       // -?Memorizzazione dati da AZNTC00F?
012800130830     d wAppNTCname     s                   inz  like(NTCrnt)
012900130830     d*// wAppNTCmail     s                   inz  like(NTCrnt)
013000130830     d wAttNTCname     s                   inz  like(NTCrnt)
013100130830     d wAttNTCmail     s                   inz  like(NTCrnt)
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
016800130830
016900130830       // -?File AZNTC01L?
017000130830     d k_azntc01     e ds                  extname(AZNTC01L : *key)
017100130830     d                                     prefix(k_)   inz
017200100310
017300100310       //--------------------------------------------------------------
017400100310       //?M A I N - L I N E                                            ?
017500100310       //--------------------------------------------------------------
017600100310
017700100310     c     *Entry        plist
017800100310     c                   parm                    KPJBA
017900100310
018000100310      /free
018100100310
018200100310       // -?Operazioni iniziali?
018300100310       exsr  sr_RoutInz;
018400100310
018500100310       // -?Stampa (ed invio) e-mail?
018600100310       exsr  sr_PrtEmail;
018700100310
018800100310       // -?Operazioni finali?
018900100310       exsr  sr_RoutEnd;
019000100310
019100100310       //--------------------------------------------------------------
019200100310       //?Operazioni iniziali.                                         ?
019300100310       //--------------------------------------------------------------
019400100310       BEGSR  sr_RoutInz;
019500100310
019600100310         *inLR = *on;
019700100310
019800100310         // -?Impostazione parametri ricevuti?
019900100310         clear  TRMK23ds;
020000100310         if  %parms > *zero   and   KPJBU <> *blank;
020100100310           TRMK23ds = KPJBU;
020200100310         endif;
020300100310         Omk23err = *off;
020400100310         clear  Omk23msg;
020500100310
020600100310         // -?Reperimento dati del lavoro?
020700100310         exsr  sr_DatiJob;
020800100310
020900100310         // -?*-=»>  Solo TEST in stampa  <«=-*?
021000100310         if  %parms = *zero   or   KPJBU = *blank;
021100100310           open  PrtEMAIL;
021200100310           exsr  sr_TEST;
021300100310           leavesr;
021400100310         endif;
021500100310
021600100310         // -?Reperimento tab. "MRA"?
021700100310         exsr  sr_TabMRA;
021800100310
021900100310         // -?Reperimento dati comm.le appuntamento?
022000100310         exsr  sr_CommApp;
022100100310
022200100310         // -?Reperimento dati comm.le attività?
022300100310         exsr  sr_CommAtt;
022400100413
022500100413         // -?Reperimento nuovo progressivo da numeratore "024"?
022600100413         exsr  sr_Num024;
022700100310
022800100310         // -?Override ai file di stampa?
022900100310         //  ?(per impostarvi i dati per l'invio via e-mail)?
023000100310         exsr sr_Override;
023100100310
023200100310         // -?Apertura file di stampa?
023300100310         open  PrtEMAIL;
023400100310
023500100310       ENDSR;
023600100310
023700100310       //--------------------------------------------------------------
023800100310       //?Reperimento Dati del job (Utente/Operativi).                 ?
023900100310       //--------------------------------------------------------------
024000100310       BEGSR  sr_DatiJob;
024100100310
024200100310         in(E) §AzUte;
024300100310         if NOT %error;
024400100310           in(E) §DatiUte;
024500100310         endif;
024600100310         if %error or RSut = *blanks;
024700100310           clear TIBS34ds;
024800100310           tibs34r(tibs34ds);
024900100310           in §AzUte;
025000100310           in §DatiUte;
025100100310         endif;
025200100310
025300100310       ENDSR;
025400100310
025500100310       //--------------------------------------------------------------
025600100310       //?Reperimento dati da tab. "MRA".                              ?
025700100310       //--------------------------------------------------------------
025800100310       BEGSR  sr_TabMRA;
025900100310
026000100413         clear  dMRAmk23;
026100100324
026200100324         k_TBEcod = 'MRA';
026300100324         k_TBEke1 = SDSpgm;
026400100324         k_TBEke2 = *blank;
026500100324
026600100324         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
026700100324
026800100324         if  %found(TNTBE01L)   and   TBEatb = *blank;
026900100413           dMRAmk23 = TBEuni;
027000100310         else;
027100100310           Omk23err = *on;
027200100324           Omk23msg = 'Parametri "' + %trimr(k_TBEke1) +
027300100310                      '" non reperiti in tab. "MRA"';
027400100310           exsr  sr_RoutEnd;
027500100310         endif;
027600100310
027700100310       ENDSR;
027800100310
027900100310       //--------------------------------------------------------------
028000100310       //?Reperimento dati del Commerciale Appuntamento                ?
028100100310       //--------------------------------------------------------------
028200100310       BEGSR  sr_CommApp;
028300100310
028400130830         clear  wAppNTCname;
028500130830         //clear  wAppNTCmail;
028600130830
028700130830         k_NTCcmm = Imk23cmm;
028800100324
028900130830         // ·?Nominativo?
029000130830         k_NTCtnt = '01';
029100100324
029200130830         chain  %kds( k_azntc01 )  AZNTC000;
029300100324
029400130830         if  %found(AZNTC01L);
029500130830             wAppNTCname = NTCrnt;
029600130830         else;
029700100310           Omk23err = *on;
029800130830           Omk23msg = 'Descrizione comm.le "' + %trimr(k_TBEke1) +
029900130830                      '" non reperita nel file AZNTC00F';
030000100310           exsr  sr_RoutEnd;
030100100310         endif;
030200130830
030300130830         //// ·?e-Mail?
030400130830         //k_NTCtnt = '02';
030500130830         //
030600130830         //chain  %kds( k_azntc01 )  AZNTC000;
030700130830         //
030800130830         //if  %found(AZNTC01L);
030900130830         //    wAppNTCmail = NTCrnt;
031000130830         //else;
031100130830         //  Omk23err = *on;
031200130830         //  Omk23msg = 'E-Mail comm.le "' + %trimr(k_TBEke1) +
031300130830         //             '" non reperita nel file AZNTC00F';
031400130830         //  exsr  sr_RoutEnd;
031500130830         //endif;
031600100310
031700100310       ENDSR;
031800100310
031900100310       //--------------------------------------------------------------
032000100310       //?Reperimento dati del Commerciale Attività                    ?
032100100310       //--------------------------------------------------------------
032200100310       BEGSR  sr_CommAtt;
032300130830
032400130830         clear  wAttNTCname;
032500130830         clear  wAttNTCmail;
032600130830
032700130830         k_NTCcmm = Imk23cma;
032800130830
032900130830         // ·?Nominativo?
033000130830         k_NTCtnt = '01';
033100130830
033200130830         chain  %kds( k_azntc01 )  AZNTC000;
033300130830
033400130830         if  %found(AZNTC01L);
033500130830             wAttNTCname = NTCrnt;
033600130830         else;
033700130830           Omk23err = *on;
033800130830           Omk23msg = 'Descrizione comm.le "' + %trimr(k_TBEke1) +
033900130830                      '" non reperita nel file AZNTC00F';
034000130830           exsr  sr_RoutEnd;
034100130830         endif;
034200130830
034300130830         // ·?e-Mail?
034400130830         k_NTCtnt = '02';
034500130830
034600130830         chain  %kds( k_azntc01 )  AZNTC000;
034700130830
034800130830         if  %found(AZNTC01L);
034900130830             wAttNTCmail = NTCrnt;
035000130830         else;
035100130830           Omk23err = *on;
035200130830           Omk23msg = 'E-Mail comm.le "' + %trimr(k_TBEke1) +
035300130830                      '" non reperita nel file AZNTC00F';
035400130830           exsr  sr_RoutEnd;
035500130830         endif;
035600100310
035700100310         // -?Decodifica filiale del comm.le attività?
035800100310         ORGfil = Imk23cma / 10000;
035900100310         chain  (ORGfil)  AZORG;
036000100310         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
036100100310           clear  ORGdes;
036200100310           clear  ORGind;
036300100310           clear  ORGcpf;
036400100310           clear  ORGloc;
036500100310           clear  ORGpro;
036600100310           clear  ORGtel;
036700100310           clear  ORGfax;
036800100310         endif;
036900100310
037000100310       ENDSR;
037100100413
037200100413       //--------------------------------------------------------------
037300100413       //?Reperimento progressivo contatore "024"                      ?
037400100413       //--------------------------------------------------------------
037500100413       BEGSR  sr_Num024;
037600100413
037700100413         reset  TRUL33ds;
037800100413
037900100413         kpjbu  = TRUL33ds;
038000100413         trul33r ( KPJBA );
038100100413         TRUL33ds = kpjbu;
038200100413
038300100413         if  O33err <> *zero;
038400100413           Omk23err = *on;
038500100413           Omk23msg = 'Fallito reperimento numeratore "' +
038600100413                      %editc(I33cnu : 'X') + '"';
038700100413           exsr  sr_RoutEnd;
038800100413         endif;
038900100413
039000100413       ENDSR;
039100100310
039200100310       //--------------------------------------------------------------
039300100310       //?Impostazione dati per stampa di prova.                       ?
039400100310       //--------------------------------------------------------------
039500100310       BEGSR  sr_TEST;
039600100310
039700100310         wDate_EUR = %date();
039800100310
039900100310         Imk23cma = 1234567;
040000100310         Imk23cmm = 7654321;
040100100310         Imk23dta = %dec( wDate_EUR : *iso );
040200100310         Imk23ora = 6789;
040300100310         Imk23rst = *all'W';
040400100310         Imk23emr = *all'W';
040500100310         Imk23emr = %subst(imk23emr:1:%len(Imk23emr)-9) = '@pippo.it';
040600100310
040700130830         wAppNTCname = *all'W';
040800130830         //wAppNTCmail = *all'X';
040900130830         wAttNTCname = *all'M';
041000130830         wAttNTCmail = *all'K';
041100100310
041200100310         ORGdes = *all'Y';
041300100310         ORGind = *all'Q';
041400100310         ORGcpf = *all'123456789';
041500100310         ORGloc = *all'W';
041600100310         ORGpro = *all'X';
041700100310         ORGtel = '1234-56789023';
041800100310         ORGfax = '9876-54321098';
041900100310
042000100310       ENDSR;
042100100310
042200100310       //--------------------------------------------------------------
042300100310       //?Preparazione ed invio e-mail.                                ?
042400100310       //--------------------------------------------------------------
042500100310       BEGSR  sr_PrtEmail;
042600100310
042700100310         wDate_EUR = %date( Imk23dta : *iso );
042800100310         wDate = %dec( wDate_EUR );
042900100310
043000100310         // -?Stampa "corpo e-mail"?
043100100317         For  xx = 1  To  13;
043200100310           O_txt = '§§Tag' + %editc( xx : 'X') + '§§#';
043300100310           select;
043400100310             when  xx = 1;
043500100310               O_txt = %trimr(O_txt) + %triml( Imk23rst );
043600100310             when  xx = 2;
043700130830               O_txt = %trimr(O_txt) + %triml( wAppNTCname );
043800100310             when  xx = 3;
043900100310               O_txt = %trimr(O_txt) + %triml( %editc( wDate : 'Y' ) );
044000100310             when  xx = 4;
044100100310               O_txt = %trimr(O_txt) + %triml( %editw( Imk23ora : '0 :  ' ) );
044200100317             when  xx = 5;
044300130830               O_txt = %trimr(O_txt) + %triml( wAttNTCname );
044400100317             when  xx = 6;
044500100317               O_txt = %trimr(O_txt) + %triml( ORGdes );
044600100317             when  xx = 7;
044700100317               O_txt = %trimr(O_txt) + %triml( ORGind );
044800100317             when  xx = 8;
044900100317               O_txt = %trimr(O_txt) + %editc( ORGcpf : 'X' );
045000100317             when  xx = 9;
045100100317               O_txt = %trimr(O_txt) + %triml( ORGloc );
045200100317             when  xx = 10;
045300100317               O_txt = %trimr(O_txt) + ORGpro;
045400100317             when  xx = 11;
045500100317               O_txt = %trimr(O_txt) + %triml( ORGtel );
045600100317             when  xx = 12;
045700100317               O_txt = %trimr(O_txt) + %triml( ORGfax );
045800100317             when  xx = 13;
045900130830               O_txt = %trimr(O_txt) + %trim( wAttNTCmail ) + c_at;
046000100310           endsl;
046100100310           O_txt = %trimr(O_txt) + '}{';
046200100310           except  PRTparm;
046300100310         EndFor;
046400100310
046500100310       ENDSR;
046600100310
046700100310       //--------------------------------------------------------------
046800100310       //?Override al file di stampa per impostarvi i dati per         ?
046900100310       //?  l'invio via e-mail.                                        ?
047000100310       //--------------------------------------------------------------
047100100310       BEGSR sr_Override;
047200100310
047300100310         reset TRTCM1ds;
047400100413         if  §MRAmitt <> *blank;
047500100413           §CM1mitt = %trim(§MRAmitt);
047600100413         else;
047700130830           §CM1mitt = %trim(wAttNTCmail);
047800100413         endif;
047900100413         §CM1dst  = %trim(iMK23emr);
048000100413         §CM1tips = §MRAregCor;
048100100310         §CM1po   = %editc( DUTpou : 'X' );
048200100413         §CM1var  = %trimr(§CM1var) + %trim(§MRAobjm);
048300100413         §CM1sts  = §MRAsts;
048400100413         §CM1idp  = §MRAidpro;
048500100413         §CM1prog = %subst( %editc( O33nri : 'X' ) :
048600100413                            %size(O33nri) - %size(§CM1prog) + 1 );
048700100310
048800100310         // -?Override al "corpo e-mail"?
048900100413         if  §MRAregCor <> *blank;
049000100310           Qcmd = c_CmdOvrPrtF
049100100413                + ' outq(' + %trim(§MRAoutqi) + ')'
049200100310                + ' usrdfndta(''' + TRTCM1ds + ''')';
049300100310         else;
049400100310           Qcmd = c_CmdOvrPrtF;
049500100310         endif;
049600100310
049700100310         exsr  sr_ExecCmd;
049800100310
049900100310         if  Qusei <> *blank;
050000100310           Omk23err = *on;
050100100310           Omk23msg = 'Fallita override al file di stampa per +
050200100310                       "corpo e-mail"';
050300100310           exsr  sr_RoutEnd;
050400100310         endif;
050500100310
050600100310       ENDSR;
050700100310
050800100310       //--------------------------------------------------------------
050900100310       //?Chiusura file stampa & Cancellazione override.               ?
051000100310       //--------------------------------------------------------------
051100100310       BEGSR sr_DeleteOvr;
051200100310
051300100310         // -?Cancellazione override al "corpo e-mail"?
051400100310         if %open(PrtEMAIL);
051500100310           close PRTEMAIL;
051600100310           Qcmd = c_CmdDltOvr;
051700100310           exsr  sr_ExecCmd;
051800100310         endif;
051900100310
052000100310       ENDSR;
052100100310
052200100310       //--------------------------------------------------------------
052300100310       //?Esecuzione del comando (già impostato).                      ?
052400100310       //--------------------------------------------------------------
052500100310       BEGSR  sr_ExecCmd;
052600100310
052700100310         clear Qcap0100;
052800100310         Qcabcsdh = *off;
052900100310         Qcapa    = *off;
053000100310         Qcacmdss = *off;
053100100310         Qcaerved = *allX'00';
053200100310
053300100310         clear Qusec;
053400100310         Qusbprv  = %size(Qusec);
053500100310
053600100310         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
053700100310                           %size(Qcap0100) : 'CPOP0100' : *omit :
053800100310                           0 : 0 : Qusec);
053900100310
054000100310         //if  Qusei <> *blank;
054100100310         //  ...;
054200100310         //endif;
054300100310
054400100310       ENDSR;
054500100310
054600100310       //--------------------------------------------------------------
054700100310       //?Operazioni finali.                                           ?
054800100310       //--------------------------------------------------------------
054900100310       BEGSR  sr_RoutEnd;
055000100310
055100100310         if  %parms > *zero   and   KPJBU <> *blank;
055200100310
055300100310           // -?Chiusura file stampa & Cancellazione override?
055400100310           exsr sr_DeleteOvr;
055500100331
055600100331           // -?Restituzione parametri?
055700100331           kpjbu = TRMK23ds;
055800100310
055900100310         endif;
056000100310
056100100310         // -?Uscita?
056200100310         return;
056300100310
056400100310       ENDSR;
056500100310
056600100310      /end-free
056700100310
056800100310       //--------------------------------------------------------------
056900100310       //?Specifiche di stampa.                                        ?
057000100310       //--------------------------------------------------------------
057100100310
057200100310     oPrtEMAIL  e            PRTparm     1
057300100310     o                       O_txt
