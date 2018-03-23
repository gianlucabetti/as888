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
001600120330
001700120330       //--------------------------------------------------------------
001800120330       //?Dichiarazione file.                                          ?
001900120330       //--------------------------------------------------------------
002000120330
002100120330       // -?Tabella "MRA"?
002200120330     fTNTBE01L  if   e           k disk
002300120330
002400120330       // -?Spool da inviare via e-mail:?
002500120330       //   ·?Corpo del messaggio di posta elettronica?
002600120330     f*//PrtEMAIL  o    f  132        printer  oflind(*inOF)  usropn
002700120330     fTNSB66P   o    e             printer  oflind(*in25)  usropn
002800120330
002900120330       //--------------------------------------------------------------
003000120330       //?Definizione costanti.                                        ?
003100120330       //--------------------------------------------------------------
003200120330
003300120330       // -?Dati di default?
003400120330     d c_Mittente      c                   const('ced')
003500120330     d c_at            c                   const('@brt.it')
003600120330     d c_Sede          c                   const('046')
003700120402
003800120402       // -?Comando di override al membro del *file WFMTC00F?
003900120402     d c_CmdOvrDbF     c                   const('OVRDBF +
004000120402     d                                           file(WFMTC00F) +
004100120402     d                                           mbr(M&aaammgg) +
004200120402     d                                           ovrscope(*actgrpdfn) +
004300120402     d                                           ')
004400120330
004500120402       // -?Comando di override al PrtF?
004600120330     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004700120330     d                                           file(TNSB66P) +
004800120330     d                                           pagesize(66 132) +
004900120330     d                                           lpi(6) cpi(10) +
005000120330     d                                           ovrscope(*actgrpdfn) +
005100120330     d                                           ')
005200120402
005300120402       // -?Comando di cellazione override (entrambe)?
005400120330     d c_CmdDltOvr     c                   const('DLTOVR +
005500120402     d                                            file(*all) +
005600120330     d                                            lvl(*actgrpdfn)')
005700120330
005800120330       //--------------------------------------------------------------
005900120330       //?Definizione schiere.                                         ?
006000120330       //--------------------------------------------------------------
006100120330
006200120402       // -?Descrizione mesi?
006300120402     d $Mese           s              9a   dim(12)  ctdata   perrcd( 1)
006400120330
006500120330       //--------------------------------------------------------------
006600120330       //?Definizione aree dati.                                       ?
006700120330       //--------------------------------------------------------------
006800120330
006900120330       // -?Dati utente?
007000120330     d §AzUte        e ds                  extname(AZUTE00F)
007100120330     d                                     dtaara
007200120330     d §DatiUte      e ds                  extname(dDatiUte)
007300120330     d                                     dtaara
007400120330
007500120330       //--------------------------------------------------------------
007600120330       //?Definizione strutture dati.                                  ?
007700120330       //--------------------------------------------------------------
007800120330
007900120330       // -?Parametri ricevuti?
008000120330     d KPJBA         e ds
008100120330
008200120330       // -?Parametri per Reperimento dati utente?
008300120330     d TIBS34ds      e ds                  inz
008400120330
008500120330       // -?Tab. "MRA" = Bart-Mailing?
008600120330     d dMRAdan       e ds                  inz
008700120330     d   §MRAddes    e                     inz('Sped. con Imp.Ass. +
008800120330     d                                          > Lim.')
008900120403     d   §MRAdreg    e                     inz('E05')
009000120330     d   §MRAdmitt   e                     inz('ced')
009100120330     d   §MRAddest   e                     inz('antonella.gallucci+
009200120330     d                                          @brt.it; +
009300120330     d                                          stefano.merighi+
009400120330     d                                          @brt.it')
009500120403     d   §MRAdidpro  e                     inz('1')
009600120403     d   §MRAdoutqi  e                     inz('QPRINTS')
009700120330
009800120330       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
009900120330     d TRTCM1ds      e ds                  inz
010000120330       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
010100120330     d   §CM1mitt    e                     inz('ced')
010200120330       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
010300120330     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
010400120330       //  ?·§CM1tips = Tipo lettera via e-mail:?
010500120330       //   ?"LET" = testo allegato in corpo con logo?
010600120330       //           ?(richiede righe libere iniziali per il logo)?
010700120330       //   ?"COR" = testo integrato senza logo?
010800120330       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
010900120330       //   ?"SP1" = spool allegato (in formato PDF)?
011000120330     d   §CM1tips    e                     inz('COR')
011100120330       //  ?·§CM1po   = Filiale?
011200120330     d   §CM1po      e                     inz(c_Sede)
011300120402       //  ?·§CM1var  = Oggetto e-mail (max 80 char)?
011400120330     d*//§CM1var     e                     inz('*OBJM*')
011500120330     d   §CM1var     e                     inz('*OBJM*+
011600120402       //                                  *...+....1....+....2....+..
011700120402     d                                     Elenco spedizioni con impor+
011800120402       //                                  ..3....+....4....+....5....
011900120402     d                                     to da assicurare maggiore d+
012000120402       //                                  +....6....+....7....+....8
012100120402     d                                     el limite imposto')
012200120330       //  ?·§CM1sts  = Stato?
012300120330     d   §CM1sts     e                     inz(*off)
012400120330       //  ?·§CM1idp  = Id processo?
012500120330     d   §CM1idp     e                     inz('2')
012600120330
012700120330       // -?Dati estratti via SQL da WFMTC00F?
012800120330     d WFMTC00F      e ds                  qualified  based(nullPtr)
012900120330     d wSQL_dati       ds                  inz
013000120330     d   MTCksc                            inz  like(WFMTC00F.MTCksc)
013100120330     d   MTCrag                            inz  like(WFMTC00F.MTCrag)
013200120330     d   MTCaas                            inz  like(WFMTC00F.MTCaas)
013300120330     d   MTClnp                            inz  like(WFMTC00F.MTClnp)
013400120330     d   MTCnrs                            inz  like(WFMTC00F.MTCnrs)
013500120330     d   MTCnsp                            inz  like(WFMTC00F.MTCnsp)
013600120330     d   MTCtbl                            inz  like(WFMTC00F.MTCtbl)
013700120330     d   MTCdsp                            inz  like(WFMTC00F.MTCdsp)
013800120330     d   MTCias                            inz  like(WFMTC00F.MTCias)
013900120330     d   MTCvas                            inz  like(WFMTC00F.MTCvas)
014000120330
014100120330       // -?Status ds?
014200120330     d Status         sds
014300120404     d   SDSpgm          *proc
014400120404     d   JobName             244    253                                         Job name
014500120404     d   JobUser             254    263                                         User name
014600120404     d   JobNumber           264    269s 0                                      Job number
014700120330
014800120330       //--------------------------------------------------------------
014900120330       //?Definizione variabili.                                       ?
015000120330       //--------------------------------------------------------------
015100120330
015200120330       // -?Flags booleani?
015300120330     d $EoF            s               n   inz
015400120330
015500120330       // -?Stringa SQL da eseguire?
015600120330     d wSQL            s           2048    Varying        inz
015700120330
015800120330       // -?Campi di comodo?
015900120330     d wDate_EUR       s               d   datfmt(*eur)  inz
016000120403     d wDate_ISO       s               d   datfmt(*iso)  inz
016100120402     d*// wDate           s              8  0 inz
016200120330
016300120330       //--------------------------------------------------------------
016400120330       //?Definizione prototipi procedure.                             ?
016500120330       //--------------------------------------------------------------
016600120330
016700120330       // -?Reperimento dati utente?
016800120330      /copy gaitrasrc/srcProtoPR,TIBS34R
016900120330
017000120330       // -?Parametri API QCAPCMD (Process Commands)?
017100120330     d Qcmd            s           2048    inz  varying
017200120330      /copy qSysInc/qRpgleSrc,QCAPCMD
017300120330       // -?API QCAPCMD (Process Commands)?
017400120330      /copy gaitrasrc/srcProtoPR,QCAPCMD
017500120330
017600120330       // -?Parametri gestione errori API.?
017700120330      /copy qsysinc/qrpglesrc,QUSEC
017800120330
017900120330       //--------------------------------------------------------------
018000120330       //?Definizione key-list.                                        ?
018100120330       //--------------------------------------------------------------
018200120330
018300120330       // -?File TNTBE01L?
018400120330     d k05tntbe01    e ds                  extname(TNTBE01L:*key) inz
018500120330     d                                     prefix(K_)
018600120330
018700120330       //--------------------------------------------------------------
018800120330       //?M A I N - L I N E                                            ?
018900120330       //--------------------------------------------------------------
019000120330
019100120330     c     *Entry        plist
019200120330     c                   parm                    KPJBA
019300120330
019400120330      /free
019500120330
019600120330       // -?Operazioni iniziali?
019700120330       exsr  sr_RoutInz;
019800120330
019900120330       // -?Ciclo di lettura  e  Stampa (ed invio) e-mail?
020000120402       DoU  $EoF;
020100120330         exsr  sr_ReadCursor;
020200120330       EndDo;
020300120330
020400120330       // -?Operazioni finali?
020500120330       exsr  sr_RoutEnd;
020600120330
020700120330       //--------------------------------------------------------------
020800120330       //?Operazioni iniziali.                                         ?
020900120330       //--------------------------------------------------------------
021000120330       BEGSR  sr_RoutInz;
021100120330
021200120330         *inLR = *on;
021300120330
021400120330         // -?Reperimento dati del lavoro?
021500120330         exsr  sr_DatiJob;
021600120330
021700120330         // -?*-=»>  Solo TEST in stampa  <«=-*?
021800120330         if  %parms = *zero;
021900120330           exsr  sr_TEST;
022000120402           exsr  sr_RoutEnd;
022100120330         endif;
022200120330
022300120330         // -?Reperimento tab. "MRA"?
022400120330         exsr  sr_TabMRA;
022500120402
022600120402         // -?Override al file di stampa?
022700120402         //  ?(per impostarvi i dati per l'invio via e-mail)?
022800120402         exsr sr_OvrPrtF;
022900120402
023000120402         // -?Apertura file di stampa?
023100120402         open  TNSB66P;
023200120330
023300120402         // -?Overrire al membro odierno del *file WFMTC00F?
023400120402         exsr  sr_OvrDbF;
023500120402
023600120330         // -?Preparazione stringa SQL?
023700120330         exsr  sr_PrepSQL;
023800120330
023900120330         // -?Apertura cursore SQL?
024000120330         exsr  sr_OpenCursor;
024100120404
024200120404         // -?Stampa testata?
024300120404         //  ?(se NON rilevati errori all'apertura del cursore)?
024400120404         if  *month > 1;
024500120404           PTmese = $Mese(*month - 1);
024600120404         else;
024700120404           PTmese = $Mese(12);
024800120404         endif;
024900120404         write  SB66tes;
025000120330
025100120330       ENDSR;
025200120330
025300120330       //--------------------------------------------------------------
025400120330       //?Reperimento Dati del job (Utente/Operativi).                 ?
025500120330       //--------------------------------------------------------------
025600120330       BEGSR  sr_DatiJob;
025700120330
025800120330         in(E) §AzUte;
025900120330         if NOT %error;
026000120330           in(E) §DatiUte;
026100120330         endif;
026200120330         if %error or RSut = *blanks;
026300120330           clear TIBS34ds;
026400120330           tibs34r(tibs34ds);
026500120330           in §AzUte;
026600120330           in §DatiUte;
026700120330         endif;
026800120330
026900120330       ENDSR;
027000120330
027100120330       //--------------------------------------------------------------
027200120330       //?Reperimento dati da tab. "MRA".                              ?
027300120330       //--------------------------------------------------------------
027400120330       BEGSR  sr_TabMRA;
027500120330
027600120330         reset  dMRAdan;
027700120330
027800120330         k_TBEcod = 'MRA';
027900120330         k_TBEke1 = SDSpgm;
028000120330         k_TBEke2 = *blank;
028100120330
028200120330         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
028300120330
028400120330         if  %found(TNTBE01L)   and   TBEatb = *blank;
028500120330           dMRAdan = TBEuni;
028600120330         endif;
028700120330
028800120330       ENDSR;
028900120330
029000120330       //--------------------------------------------------------------
029100120330       //?Impostazione dati per stampa di prova.                       ?
029200120330       //--------------------------------------------------------------
029300120330       BEGSR  sr_TEST;
029400120402
029500120402         // -?Apertura file di stampa?
029600120402         open  TNSB66P;
029700120402
029800120402         // -?Stampa testata?
029900120402         write  SB66tes;
030000120330
030100120402         // -?Stampa dettaglio?
030200120330         MTCaas = *year;
030300120330         MTClnp = 321;
030400120330         MTCnrs = 98;
030500120330         MTCnsp = 1234567;
030600120402         //MTCdsp = %dec( %date() );
030700120402         wDate_Eur = %date();
030800120402         pDSP = %dec( wDate_Eur );
030900120330         MTCtbl = 'XY';
031000120330         MTCksc = 9876543;
031100120330         MTCrag = *all'W';
031200120330         MTCias = 1234567890.123;
031300120330         MTCvas = 'ITL';
031400120402
031500120402         write  SB66det;
031600120402         write  SB66det;
031700120402         write  SB66det;
031800120330
031900120330       ENDSR;
032000120402
032100120402       //--------------------------------------------------------------
032200120402       //?Override al membro odierno del work-file WFMTC00F.           ?
032300120402       //--------------------------------------------------------------
032400120402       BEGSR  sr_OvrDbF;
032500120402
032600120403         wDate_Iso = %date();
032700120403         if  %subdt( wDate_Iso : *days ) > 1;
032800120403           wDate_Iso -= %days( %subdt( %date() : *days ) - 1 );
032900120403         endif;
033000120403
033100120403         //Qcmd = %trim( %replace( %editc( %dec( %date() ) : 'X' ) :
033200120403         Qcmd = %trim( %replace( %editc( %dec( wDate_Iso ) : 'X' ) :
033300120402                                 c_CmdOvrDbF :
033400120402                                 %scan( '&aaammgg' : c_CmdOvrDbF ) ) );
033500120402
033600120402         exsr  sr_ExecCmd;
033700120402
033800120402         if  Qusei <> *blank;
033900120402           $EoF = *on;
034000120402           exsr  sr_PrintErr;
034100120402         endif;
034200120402
034300120402       ENDSR;
034400120330
034500120330       //--------------------------------------------------------------
034600120330       //?Preparazione stringa SQL.                                    ?
034700120330       //--------------------------------------------------------------
034800120330       BEGSR  sr_PrepSQL;
034900120330
035000120330         wSQL = 'select mtcKSC, mtcRAG, mtcAAS, mtcLNP, mtcNRS, +
035100120330                        mtcNSP, mtcTBL, mtcDSP, mtcIAS, mtcVAS +
035200120330                   from WFMTC00F +
035300120330                  where mtcASS = ''ASS'' +
035400120330                    for fetch only';
035500120330
035600120330       ENDSR;
035700120330
035800120330       //--------------------------------------------------------------
035900120330       //?Apertura cursore.                                            ?
036000120330       //--------------------------------------------------------------
036100120330       BEGSR  sr_OpenCursor;
036200120330
036300120330         // -?Dichiarazione cursore?
036400120330         exec sql   prepare S1   from :wSQL;
036500120330         exec sql   declare C1   cursor for S1;
036600120402
036700120402         if  SQLcode < *zero;
036800120402           $EoF = *on;
036900120402           exsr  sr_PrintErr;
037000120402         endif;
037100120330
037200120330         // -?Apertura del cursore?
037300120330         exec sql   open C1;
037400120330
037500120330         if  SQLcode < *zero;
037600120330           $EoF = *on;
037700120330           exsr  sr_PrintErr;
037800120330         endif;
037900120330
038000120330       ENDSR;
038100120330
038200120330       //--------------------------------------------------------------
038300120330       //?Lettura cursore.                                             ?
038400120330       //--------------------------------------------------------------
038500120330       BEGSR  sr_ReadCursor;
038600120330
038700120330         clear  wSQL_dati;
038800120330
038900120330         exec sql   fetch next   from C1   into :wSQL_dati;
039000120330
039100120330         select;
039200120330           when  SQLcode = 100;
039300120330             $EoF = *on;
039400120404             write  SB66end;
039500120330           when  SQLcode < *zero;
039600120330             $EoF = *on;
039700120330             exsr  sr_PrintErr;
039800120330           other;
039900120330             exsr  sr_PrtEmail;
040000120330         endsl;
040100120330
040200120330       ENDSR;
040300120330
040400120330       //--------------------------------------------------------------
040500120330       //?Chiusura cursore.                                            ?
040600120330       //--------------------------------------------------------------
040700120330       BEGSR  sr_CloseCursor;
040800120330
040900120330         // -?Chiusura del cursore?
041000120330         exec sql   close C1;
041100120330
041200120330       ENDSR;
041300120330
041400120330       //--------------------------------------------------------------
041500120330       //?Stampa segnalazione dell'errore rilevato.                    ?
041600120330       //--------------------------------------------------------------
041700120330       BEGSR  sr_PrintErr;
041800120330
041900120330         // -?Stampa del Dump?
042000120330         Dump(A);
042100120330
042200120330         // -?Stampa del Job-Log?
042300120330         Qcmd = 'DSPJOBLOG job(*) output(*print)';
042400120330         exsr  sr_ExecCmd;
042500120404
042600120404         // -?Stampa messaggio indicativo nel file di stampa?
042700120404         //  ?(messaggio che arriverà via e-mail)?
042800120404         PTpgm  = %trimr( SDSpgm ) + '.';
042900120404         PTjobn = %trimr( JobName ) + '/' + %trimr( JobUser ) + '/' +
043000120404                  %editc( JobNumber : 'X' ) + '.';
043100120404         write SB66err;
043200120330
043300120330         // -?Chiusura del programma?
043400120330         exsr  sr_RoutEnd;
043500120330
043600120330       ENDSR;
043700120330
043800120330       //--------------------------------------------------------------
043900120330       //?Preparazione e-mail.                                         ?
044000120330       //--------------------------------------------------------------
044100120330       BEGSR  sr_PrtEmail;
044200120330
044300120330         wDate_Eur = %date( MTCdsp : *iso );
044400120330         pDSP = %dec( wDate_Eur );
044500120330
044600120330         write  SB66det;
044700120330
044800120330       ENDSR;
044900120330
045000120330       //--------------------------------------------------------------
045100120330       //?Override al file di stampa per impostarvi i dati per         ?
045200120330       //?  l'invio via e-mail.                                        ?
045300120330       //--------------------------------------------------------------
045400120402       BEGSR sr_OvrPrtF;
045500120330
045600120330         reset TRTCM1ds;
045700120330         if  §MRAdmitt <> *blank;
045800120330           §CM1mitt = %trim(§MRAdmitt);
045900120330         endif;
046000120330         §CM1dst  = %trim(§MRAddest);
046100120330         §CM1tips = §MRAdreg;
046200120330         //§CM1var  = '*OBJM*' + §MRAddes;
046300120330         §CM1idp  = §MRAdidpro;
046400120330
046500120330         // -?Override al "corpo e-mail"?
046600120330         if  §MRAdreg <> *blank;
046700120330           Qcmd = c_CmdOvrPrtF
046800120330                + ' outq(' + %trim(§MRAdoutqi) + ')'
046900120330                + ' usrdfndta(''' + TRTCM1ds + ''')';
047000120330         else;
047100120330           Qcmd = c_CmdOvrPrtF;
047200120330         endif;
047300120330
047400120330         exsr  sr_ExecCmd;
047500120330
047600120330         //if  Qusei <> *blank;
047700120330         //  ...;
047800120330         //endif;
047900120330
048000120330       ENDSR;
048100120330
048200120330       //--------------------------------------------------------------
048300120402       //?Chiusura cursore e file stampa   &                           ?
048400120402       //?Cancellazione override (entrambe).                           ?
048500120330       //--------------------------------------------------------------
048600120330       BEGSR sr_DeleteOvr;
048700120402
048800120402         // -?Chiusura cursore SQL?
048900120402         exsr  sr_CloseCursor;
049000120330
049100120402         // -?Chiusura file di stampa?
049200120402         if  %open(TNSB66P);
049300120402           close  TNSB66P;
049400120402         endif;
049500120402
049600120402         // -?Cancellazione override al file WFMTC00F ed?
049700120402         //                         ?al file di stampa?
049800120402         Qcmd = c_CmdDltOvr;
049900120402         exsr  sr_ExecCmd;
050000120330
050100120330       ENDSR;
050200120330
050300120330       //--------------------------------------------------------------
050400120330       //?Esecuzione del comando (già impostato).                      ?
050500120330       //--------------------------------------------------------------
050600120330       BEGSR  sr_ExecCmd;
050700120330
050800120330         clear Qcap0100;
050900120330         Qcabcsdh = *off;
051000120330         Qcapa    = *off;
051100120330         Qcacmdss = *off;
051200120330         Qcaerved = *allX'00';
051300120330
051400120330         clear Qusec;
051500120330         Qusbprv  = %size(Qusec);
051600120330
051700120330         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
051800120330                           %size(Qcap0100) : 'CPOP0100' : *omit :
051900120330                           0 : 0 : Qusec);
052000120330
052100120330         //if  Qusei <> *blank;
052200120330         //  ...;
052300120330         //endif;
052400120330
052500120330       ENDSR;
052600120330
052700120330       //--------------------------------------------------------------
052800120330       //?Operazioni finali.                                           ?
052900120330       //--------------------------------------------------------------
053000120330       BEGSR  sr_RoutEnd;
053100120402
053200120402         // -?Chiusura file stampa & Cancellazione override (TUTTE)?
053300120402         exsr sr_DeleteOvr;
053400120330
053500120330         // -?Uscita?
053600120330         return;
053700120330
053800120330       ENDSR;
053900120330
054000120330      /end-free
054100120404
054200120404       //--------------------------------------------------------------
054300120404       //?Schiere a tempo di compilazione.                             ?
054400120404       //--------------------------------------------------------------
054500120402
054600120404** -?$Mese?-?Descrizione dei mesi?
054700120402gennaio
054800120402febbraio
054900120402marzo
055000120402aprile
055100120402maggio
055200120402giugno
055300120402luglio
055400120402agosto
055500120402settembre
055600120402ottobre
055700120402novembre
055800120402dicembre
