000100120330       //==============================================================
000200120330       //?E-mail a SAS con elenco spedizioni "Manca Tariffa".          ?
000300120330       //==============================================================
000400120330
000500120330       //--------------------------------------------------------------
000600120330       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120330       //--------------------------------------------------------------
000800120330
000900120330     /*END
001000120330
001100120330       //--------------------------------------------------------------
001200120330       //?Specifiche di controllo.                                     ?
001300120330       //--------------------------------------------------------------
001400120330
001500120330     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600120330     h*// dftactgrp(*no)
001700120330
001800120330       //--------------------------------------------------------------
001900120330       //?Dichiarazione file.                                          ?
002000120330       //--------------------------------------------------------------
002100120330
002200120330       // -?Tabella "MRA"?
002300120330     fTNTBE01L  if   e           k disk
002400120330
002500120330       // -?Spool da inviare via e-mail:?
002600120330       //   ·?Corpo del messaggio di posta elettronica?
002700120330     f*//PrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002800120330     fTNSB66P   o    e             printer  oflind(*in25)  usropn
002900120330
003000120330       //--------------------------------------------------------------
003100120330       //?Definizione costanti.                                        ?
003200120330       //--------------------------------------------------------------
003300120330
003400120330       // -?Dati di default?
003500120330     d c_Mittente      c                   const('ced')
003600120330     d c_at            c                   const('@brt.it')
003700120330     d c_Sede          c                   const('046')
003800120402
003900120402       // -?Comando di override al membro del *file WFMTC00F?
004000120402     d c_CmdOvrDbF     c                   const('OVRDBF +
004100120402     d                                           file(WFMTC00F) +
004200120402     d                                           mbr(M&aaammgg) +
004300120402     d                                           ovrscope(*actgrpdfn) +
004400120402     d                                           ')
004500120330
004600120402       // -?Comando di override al PrtF?
004700120330     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004800120330     d                                           file(TNSB66P) +
004900120330     d                                           pagesize(66 132) +
005000120330     d                                           lpi(6) cpi(10) +
005100120330     d                                           ovrscope(*actgrpdfn) +
005200120330     d                                           ')
005300120402
005400120402       // -?Comando di cellazione override (entrambe)?
005500120330     d c_CmdDltOvr     c                   const('DLTOVR +
005600120402     d                                            file(*all) +
005700120330     d                                            lvl(*actgrpdfn)')
005800120330
005900120330       //--------------------------------------------------------------
006000120330       //?Definizione schiere.                                         ?
006100120330       //--------------------------------------------------------------
006200120330
006300120402       // -?Descrizione mesi?
006400120402     d $Mese           s              9a   dim(12)  ctdata   perrcd( 1)
006500120330
006600120330       //--------------------------------------------------------------
006700120330       //?Definizione aree dati.                                       ?
006800120330       //--------------------------------------------------------------
006900120330
007000120330       // -?Dati utente?
007100120330     d §AzUte        e ds                  extname(AZUTE00F)
007200120330     d                                     dtaara
007300120330     d §DatiUte      e ds                  extname(dDatiUte)
007400120330     d                                     dtaara
007500120330
007600120330       //--------------------------------------------------------------
007700120330       //?Definizione strutture dati.                                  ?
007800120330       //--------------------------------------------------------------
007900120330
008000120330       // -?Parametri ricevuti?
008100120330     d KPJBA         e ds
008200120330
008300120330       // -?Parametri per Reperimento dati utente?
008400120330     d TIBS34ds      e ds                  inz
008500120330
008600120330       // -?Tab. "MRA" = Bart-Mailing?
008700120330     d dMRAdan       e ds                  inz
008800120330     d   §MRAddes    e                     inz('Sped. con Imp.Ass. +
008900120330     d                                          > Lim.')
009000120403     d   §MRAdreg    e                     inz('E05')
009100120330     d   §MRAdmitt   e                     inz('ced')
009200120330     d   §MRAddest   e                     inz('antonella.gallucci+
009300120330     d                                          @brt.it; +
009400120330     d                                          stefano.merighi+
009500120330     d                                          @brt.it')
009600120403     d   §MRAdidpro  e                     inz('1')
009700120403     d   §MRAdoutqi  e                     inz('QPRINTS')
009800120330
009900120330       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
010000120330     d TRTCM1ds      e ds                  inz
010100120330       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010200120330     d   §CM1mitt    e                     inz('ced')
010300120330       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010400120330     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
010500120330       //  ?·§CM1tips = Tipo lettera via e-mail:?
010600120330       //   ?"LET" = testo allegato in corpo con logo?
010700120330       //           ?(richiede righe libere iniziali per il logo)?
010800120330       //   ?"COR" = testo integrato senza logo?
010900120330       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
011000120330       //   ?"SP1" = spool allegato (in formato PDF)?
011100120330     d   §CM1tips    e                     inz('COR')
011200120330       //  ?·§CM1po   = Filiale?
011300120330     d   §CM1po      e                     inz(c_Sede)
011400120402       //  ?·§CM1var  = Oggetto e-mail (max 80 char)?
011500120330     d*//§CM1var     e                     inz('*OBJM*')
011600120330     d   §CM1var     e                     inz('*OBJM*+
011700120402       //                                  *...+....1....+....2....+..
011800120402     d                                     Elenco spedizioni con impor+
011900120402       //                                  ..3....+....4....+....5....
012000120402     d                                     to da assicurare maggiore d+
012100120402       //                                  +....6....+....7....+....8
012200120402     d                                     el limite imposto')
012300120330       //  ?·§CM1sts  = Stato?
012400120330     d   §CM1sts     e                     inz(*off)
012500120330       //  ?·§CM1idp  = Id processo?
012600120330     d   §CM1idp     e                     inz('2')
012700120330
012800120330       // -?Dati estratti via SQL da WFMTC00F?
012900120330     d WFMTC00F      e ds                  qualified  based(nullPtr)
013000120330     d wSQL_dati       ds                  inz
013100120330     d   MTCksc                            inz  like(WFMTC00F.MTCksc)
013200120330     d   MTCrag                            inz  like(WFMTC00F.MTCrag)
013300120330     d   MTCaas                            inz  like(WFMTC00F.MTCaas)
013400120330     d   MTClnp                            inz  like(WFMTC00F.MTClnp)
013500120330     d   MTCnrs                            inz  like(WFMTC00F.MTCnrs)
013600120330     d   MTCnsp                            inz  like(WFMTC00F.MTCnsp)
013700120330     d   MTCtbl                            inz  like(WFMTC00F.MTCtbl)
013800120330     d   MTCdsp                            inz  like(WFMTC00F.MTCdsp)
013900120330     d   MTCias                            inz  like(WFMTC00F.MTCias)
014000120330     d   MTCvas                            inz  like(WFMTC00F.MTCvas)
014100120330
014200120330       // -?Status ds?
014300120330     d Status         sds
014400120404     d   SDSpgm          *proc
014500120404     d   JobName             244    253                                         Job name
014600120404     d   JobUser             254    263                                         User name
014700120404     d   JobNumber           264    269s 0                                      Job number
014800120330
014900120330       //--------------------------------------------------------------
015000120330       //?Definizione variabili.                                       ?
015100120330       //--------------------------------------------------------------
015200120330
015300120330       // -?Flags booleani?
015400120330     d $EoF            s               n   inz
015500120330
015600120330       // -?Stringa SQL da eseguire?
015700120330     d wSQL            s           2048    Varying        inz
015800120330
015900120330       // -?Campi di comodo?
016000120330     d wDate_EUR       s               d   datfmt(*eur)  inz
016100120403     d wDate_ISO       s               d   datfmt(*iso)  inz
016200120402     d*// wDate           s              8  0 inz
016300120330
016400120330       //--------------------------------------------------------------
016500120330       //?Definizione prototipi procedure.                             ?
016600120330       //--------------------------------------------------------------
016700120330
016800120330       // -?Reperimento dati utente?
016900120330      /copy gaitrasrc/srcProtoPR,TIBS34R
017000120330
017100120330       // -?Parametri API QCAPCMD (Process Commands)?
017200120330     d Qcmd            s           2048    inz  varying
017300120330      /copy qSysInc/qRpgleSrc,QCAPCMD
017400120330       // -?API QCAPCMD (Process Commands)?
017500120330      /copy gaitrasrc/srcProtoPR,QCAPCMD
017600120330
017700120330       // -?Parametri gestione errori API.?
017800120330      /copy qsysinc/qrpglesrc,QUSEC
017900120330
018000120330       //--------------------------------------------------------------
018100120330       //?Definizione key-list.                                        ?
018200120330       //--------------------------------------------------------------
018300120330
018400120330       // -?File TNTBE01L?
018500120330     d k05tntbe01    e ds                  extname(TNTBE01L:*key) inz
018600120330     d                                     prefix(K_)
018700120330
018800120330       //--------------------------------------------------------------
018900120330       //?M A I N - L I N E                                            ?
019000120330       //--------------------------------------------------------------
019100120330
019200120330     c     *Entry        plist
019300120330     c                   parm                    KPJBA
019400120330
019500120330      /free
019600120330
019700120330       // -?Operazioni iniziali?
019800120330       exsr  sr_RoutInz;
019900120330
020000120330       // -?Ciclo di lettura  e  Stampa (ed invio) e-mail?
020100120402       DoU  $EoF;
020200120330         exsr  sr_ReadCursor;
020300120330       EndDo;
020400120330
020500120330       // -?Operazioni finali?
020600120330       exsr  sr_RoutEnd;
020700120330
020800120330       //--------------------------------------------------------------
020900120330       //?Operazioni iniziali.                                         ?
021000120330       //--------------------------------------------------------------
021100120330       BEGSR  sr_RoutInz;
021200120330
021300120330         *inLR = *on;
021400120330
021500120330         // -?Reperimento dati del lavoro?
021600120330         exsr  sr_DatiJob;
021700120330
021800120330         // -?*-=»>  Solo TEST in stampa  <«=-*?
021900120330         if  %parms = *zero;
022000120330           exsr  sr_TEST;
022100120402           exsr  sr_RoutEnd;
022200120330         endif;
022300120330
022400120330         // -?Reperimento tab. "MRA"?
022500120330         exsr  sr_TabMRA;
022600120402
022700120402         // -?Override al file di stampa?
022800120402         //  ?(per impostarvi i dati per l'invio via e-mail)?
022900120402         exsr sr_OvrPrtF;
023000120402
023100120402         // -?Apertura file di stampa?
023200120402         open  TNSB66P;
023300120330
023400120402         // -?Overrire al membro odierno del *file WFMTC00F?
023500120402         exsr  sr_OvrDbF;
023600120402
023700120330         // -?Preparazione stringa SQL?
023800120330         exsr  sr_PrepSQL;
023900120330
024000120330         // -?Apertura cursore SQL?
024100120330         exsr  sr_OpenCursor;
024200120404
024300120404         // -?Stampa testata?
024400120404         //  ?(se NON rilevati errori all'apertura del cursore)?
024500120404         if  *month > 1;
024600120404           PTmese = $Mese(*month - 1);
024700120404         else;
024800120404           PTmese = $Mese(12);
024900120404         endif;
025000120404         write  SB66tes;
025100120330
025200120330       ENDSR;
025300120330
025400120330       //--------------------------------------------------------------
025500120330       //?Reperimento Dati del job (Utente/Operativi).                 ?
025600120330       //--------------------------------------------------------------
025700120330       BEGSR  sr_DatiJob;
025800120330
025900120330         in(E) §AzUte;
026000120330         if NOT %error;
026100120330           in(E) §DatiUte;
026200120330         endif;
026300120330         if %error or RSut = *blanks;
026400120330           clear TIBS34ds;
026500120330           tibs34r(tibs34ds);
026600120330           in §AzUte;
026700120330           in §DatiUte;
026800120330         endif;
026900120330
027000120330       ENDSR;
027100120330
027200120330       //--------------------------------------------------------------
027300120330       //?Reperimento dati da tab. "MRA".                              ?
027400120330       //--------------------------------------------------------------
027500120330       BEGSR  sr_TabMRA;
027600120330
027700120330         reset  dMRAdan;
027800120330
027900120330         k_TBEcod = 'MRA';
028000120330         k_TBEke1 = SDSpgm;
028100120330         k_TBEke2 = *blank;
028200120330
028300120330         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
028400120330
028500120330         if  %found(TNTBE01L)   and   TBEatb = *blank;
028600120330           dMRAdan = TBEuni;
028700120330         endif;
028800120330
028900120330       ENDSR;
029000120330
029100120330       //--------------------------------------------------------------
029200120330       //?Impostazione dati per stampa di prova.                       ?
029300120330       //--------------------------------------------------------------
029400120330       BEGSR  sr_TEST;
029500120402
029600120402         // -?Apertura file di stampa?
029700120402         open  TNSB66P;
029800120402
029900120402         // -?Stampa testata?
030000120402         write  SB66tes;
030100120330
030200120402         // -?Stampa dettaglio?
030300120330         MTCaas = *year;
030400120330         MTClnp = 321;
030500120330         MTCnrs = 98;
030600120330         MTCnsp = 1234567;
030700120402         //MTCdsp = %dec( %date() );
030800120402         wDate_Eur = %date();
030900120402         pDSP = %dec( wDate_Eur );
031000120330         MTCtbl = 'XY';
031100120330         MTCksc = 9876543;
031200120330         MTCrag = *all'W';
031300120330         MTCias = 1234567890.123;
031400120330         MTCvas = 'ITL';
031500120402
031600120402         write  SB66det;
031700120402         write  SB66det;
031800120402         write  SB66det;
031900120330
032000120330       ENDSR;
032100120402
032200120402       //--------------------------------------------------------------
032300120402       //?Override al membro odierno del work-file WFMTC00F.           ?
032400120402       //--------------------------------------------------------------
032500120402       BEGSR  sr_OvrDbF;
032600120402
032700120403         wDate_Iso = %date();
032800120403         if  %subdt( wDate_Iso : *days ) > 1;
032900120403           wDate_Iso -= %days( %subdt( %date() : *days ) - 1 );
033000120403         endif;
033100120403
033200120403         //Qcmd = %trim( %replace( %editc( %dec( %date() ) : 'X' ) :
033300120403         Qcmd = %trim( %replace( %editc( %dec( wDate_Iso ) : 'X' ) :
033400120402                                 c_CmdOvrDbF :
033500120402                                 %scan( '&aaammgg' : c_CmdOvrDbF ) ) );
033600120402
033700120402         exsr  sr_ExecCmd;
033800120402
033900120402         if  Qusei <> *blank;
034000120402           $EoF = *on;
034100120402           exsr  sr_PrintErr;
034200120402         endif;
034300120402
034400120402       ENDSR;
034500120330
034600120330       //--------------------------------------------------------------
034700120330       //?Preparazione stringa SQL.                                    ?
034800120330       //--------------------------------------------------------------
034900120330       BEGSR  sr_PrepSQL;
035000120330
035100120330         wSQL = 'select mtcKSC, mtcRAG, mtcAAS, mtcLNP, mtcNRS, +
035200120330                        mtcNSP, mtcTBL, mtcDSP, mtcIAS, mtcVAS +
035300120330                   from WFMTC00F +
035400120330                  where mtcASS = ''ASS'' +
035500120330                    for fetch only';
035600120330
035700120330       ENDSR;
035800120330
035900120330       //--------------------------------------------------------------
036000120330       //?Apertura cursore.                                            ?
036100120330       //--------------------------------------------------------------
036200120330       BEGSR  sr_OpenCursor;
036300120330
036400120330         // -?Dichiarazione cursore?
036500120330         exec sql   prepare S1   from :wSQL;
036600120330         exec sql   declare C1   cursor for S1;
036700120402
036800120402         if  SQLcode < *zero;
036900120402           $EoF = *on;
037000120402           exsr  sr_PrintErr;
037100120402         endif;
037200120330
037300120330         // -?Apertura del cursore?
037400120330         exec sql   open C1;
037500120330
037600120330         if  SQLcode < *zero;
037700120330           $EoF = *on;
037800120330           exsr  sr_PrintErr;
037900120330         endif;
038000120330
038100120330       ENDSR;
038200120330
038300120330       //--------------------------------------------------------------
038400120330       //?Lettura cursore.                                             ?
038500120330       //--------------------------------------------------------------
038600120330       BEGSR  sr_ReadCursor;
038700120330
038800120330         clear  wSQL_dati;
038900120330
039000120330         exec sql   fetch next   from C1   into :wSQL_dati;
039100120330
039200120330         select;
039300120330           when  SQLcode = 100;
039400120330             $EoF = *on;
039500120404             write  SB66end;
039600120330           when  SQLcode < *zero;
039700120330             $EoF = *on;
039800120330             exsr  sr_PrintErr;
039900120330           other;
040000120330             exsr  sr_PrtEmail;
040100120330         endsl;
040200120330
040300120330       ENDSR;
040400120330
040500120330       //--------------------------------------------------------------
040600120330       //?Chiusura cursore.                                            ?
040700120330       //--------------------------------------------------------------
040800120330       BEGSR  sr_CloseCursor;
040900120330
041000120330         // -?Chiusura del cursore?
041100120330         exec sql   close C1;
041200120330
041300120330       ENDSR;
041400120330
041500120330       //--------------------------------------------------------------
041600120330       //?Stampa segnalazione dell'errore rilevato.                    ?
041700120330       //--------------------------------------------------------------
041800120330       BEGSR  sr_PrintErr;
041900120330
042000120330         // -?Stampa del Dump?
042100120330         Dump(A);
042200120330
042300120330         // -?Stampa del Job-Log?
042400120330         Qcmd = 'DSPJOBLOG job(*) output(*print)';
042500120330         exsr  sr_ExecCmd;
042600120404
042700120404         // -?Stampa messaggio indicativo nel file di stampa?
042800120404         //  ?(messaggio che arriverà via e-mail)?
042900120404         PTpgm  = %trimr( SDSpgm ) + '.';
043000120404         PTjobn = %trimr( JobName ) + '/' + %trimr( JobUser ) + '/' +
043100120404                  %editc( JobNumber : 'X' ) + '.';
043200120404         write SB66err;
043300120330
043400120330         // -?Chiusura del programma?
043500120330         exsr  sr_RoutEnd;
043600120330
043700120330       ENDSR;
043800120330
043900120330       //--------------------------------------------------------------
044000120330       //?Preparazione e-mail.                                         ?
044100120330       //--------------------------------------------------------------
044200120330       BEGSR  sr_PrtEmail;
044300120330
044400120330         wDate_Eur = %date( MTCdsp : *iso );
044500120330         pDSP = %dec( wDate_Eur );
044600120330
044700120330         write  SB66det;
044800120330
044900120330       ENDSR;
045000120330
045100120330       //--------------------------------------------------------------
045200120330       //?Override al file di stampa per impostarvi i dati per         ?
045300120330       //?  l'invio via e-mail.                                        ?
045400120330       //--------------------------------------------------------------
045500120402       BEGSR sr_OvrPrtF;
045600120330
045700120330         reset TRTCM1ds;
045800120330         if  §MRAdmitt <> *blank;
045900120330           §CM1mitt = %trim(§MRAdmitt);
046000120330         endif;
046100120330         §CM1dst  = %trim(§MRAddest);
046200120330         §CM1tips = §MRAdreg;
046300120330         //§CM1var  = '*OBJM*' + §MRAddes;
046400120330         §CM1idp  = §MRAdidpro;
046500120330
046600120330         // -?Override al "corpo e-mail"?
046700120330         if  §MRAdreg <> *blank;
046800120330           Qcmd = c_CmdOvrPrtF
046900120330                + ' outq(' + %trim(§MRAdoutqi) + ')'
047000120330                + ' usrdfndta(''' + TRTCM1ds + ''')';
047100120330         else;
047200120330           Qcmd = c_CmdOvrPrtF;
047300120330         endif;
047400120330
047500120330         exsr  sr_ExecCmd;
047600120330
047700120330         //if  Qusei <> *blank;
047800120330         //  ...;
047900120330         //endif;
048000120330
048100120330       ENDSR;
048200120330
048300120330       //--------------------------------------------------------------
048400120402       //?Chiusura cursore e file stampa   &                           ?
048500120402       //?Cancellazione override (entrambe).                           ?
048600120330       //--------------------------------------------------------------
048700120330       BEGSR sr_DeleteOvr;
048800120402
048900120402         // -?Chiusura cursore SQL?
049000120402         exsr  sr_CloseCursor;
049100120330
049200120402         // -?Chiusura file di stampa?
049300120402         if  %open(TNSB66P);
049400120402           close  TNSB66P;
049500120402         endif;
049600120402
049700120402         // -?Cancellazione override al file WFMTC00F ed?
049800120402         //                         ?al file di stampa?
049900120402         Qcmd = c_CmdDltOvr;
050000120402         exsr  sr_ExecCmd;
050100120330
050200120330       ENDSR;
050300120330
050400120330       //--------------------------------------------------------------
050500120330       //?Esecuzione del comando (già impostato).                      ?
050600120330       //--------------------------------------------------------------
050700120330       BEGSR  sr_ExecCmd;
050800120330
050900120330         clear Qcap0100;
051000120330         Qcabcsdh = *off;
051100120330         Qcapa    = *off;
051200120330         Qcacmdss = *off;
051300120330         Qcaerved = *allX'00';
051400120330
051500120330         clear Qusec;
051600120330         Qusbprv  = %size(Qusec);
051700120330
051800120330         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
051900120330                           %size(Qcap0100) : 'CPOP0100' : *omit :
052000120330                           0 : 0 : Qusec);
052100120330
052200120330         //if  Qusei <> *blank;
052300120330         //  ...;
052400120330         //endif;
052500120330
052600120330       ENDSR;
052700120330
052800120330       //--------------------------------------------------------------
052900120330       //?Operazioni finali.                                           ?
053000120330       //--------------------------------------------------------------
053100120330       BEGSR  sr_RoutEnd;
053200120402
053300120402         // -?Chiusura file stampa & Cancellazione override (TUTTE)?
053400120402         exsr sr_DeleteOvr;
053500120330
053600120330         // -?Uscita?
053700120330         return;
053800120330
053900120330       ENDSR;
054000120330
054100120330      /end-free
054200120404
054300120404       //--------------------------------------------------------------
054400120404       //?Schiere a tempo di compilazione.                             ?
054500120404       //--------------------------------------------------------------
054600120402
054700120404** -?$Mese?-?Descrizione dei mesi?
054800120402gennaio
054900120402febbraio
055000120402marzo
055100120402aprile
055200120402maggio
055300120402giugno
055400120402luglio
055500120402agosto
055600120402settembre
055700120402ottobre
055800120402novembre
055900120402dicembre
