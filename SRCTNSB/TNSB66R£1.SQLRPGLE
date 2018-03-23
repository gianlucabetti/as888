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
003800120330
003900120330       // -?Comandi di override al PrtF?
004000120330     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004100120330     d                                           file(TNSB66P) +
004200120330     d                                           pagesize(66 132) +
004300120330     d                                           lpi(6) cpi(10) +
004400120330     d                                           ovrscope(*actgrpdfn) +
004500120330     d                                           ')
004600120330     d c_CmdDltOvr     c                   const('DLTOVR +
004700120330     d                                            file(PRTEMAIL) +
004800120330     d                                            lvl(*actgrpdfn)')
004900120330
005000120330       //--------------------------------------------------------------
005100120330       //?Definizione schiere.                                         ?
005200120330       //--------------------------------------------------------------
005300120330
005400120330
005500120330       //--------------------------------------------------------------
005600120330       //?Definizione aree dati.                                       ?
005700120330       //--------------------------------------------------------------
005800120330
005900120330       // -?Dati utente?
006000120330     d §AzUte        e ds                  extname(AZUTE00F)
006100120330     d                                     dtaara
006200120330     d §DatiUte      e ds                  extname(dDatiUte)
006300120330     d                                     dtaara
006400120330
006500120330       //--------------------------------------------------------------
006600120330       //?Definizione strutture dati.                                  ?
006700120330       //--------------------------------------------------------------
006800120330
006900120330       // -?Parametri ricevuti?
007000120330     d KPJBA         e ds
007100120330
007200120330       // -?Parametri per Reperimento dati utente?
007300120330     d TIBS34ds      e ds                  inz
007400120330
007500120330       // -?Tab. "MRA" = Bart-Mailing?
007600120330     d dMRAdan       e ds                  inz
007700120330     d   §MRAddes    e                     inz('Sped. con Imp.Ass. +
007800120330     d                                          > Lim.')
007900120330     d   §MRAdreg    e                     inz('COR')
008000120330     d   §MRAdmitt   e                     inz('ced')
008100120330     d   §MRAddest   e                     inz('antonella.gallucci+
008200120330     d                                          @brt.it; +
008300120330     d                                          stefano.merighi+
008400120330     d                                          @brt.it')
008500120330     d   §MRAdidpro  e                     inz('2')
008600120330     d   §MRAdoutqi  e                     inz('EMAILIN')
008700120330
008800120330       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
008900120330     d TRTCM1ds      e ds                  inz
009000120330       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
009100120330     d   §CM1mitt    e                     inz('ced')
009200120330       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
009300120330     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
009400120330       //  ?·§CM1tips = Tipo lettera via e-mail:?
009500120330       //   ?"LET" = testo allegato in corpo con logo?
009600120330       //           ?(richiede righe libere iniziali per il logo)?
009700120330       //   ?"COR" = testo integrato senza logo?
009800120330       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
009900120330       //   ?"SP1" = spool allegato (in formato PDF)?
010000120330     d   §CM1tips    e                     inz('COR')
010100120330       //  ?·§CM1po   = Filiale?
010200120330     d   §CM1po      e                     inz(c_Sede)
010300120330       //  ?·§CM1var  = Oggetto e-mail?
010400120330     d*//§CM1var     e                     inz('*OBJM*')
010500120330     d   §CM1var     e                     inz('*OBJM*+
010600120330     d                                     Elenco sped. con importo d+
010700120330     d                                     a assicurare > del limite +
010800120330     d                                     imposto')
010900120330       //  ?·§CM1sts  = Stato?
011000120330     d   §CM1sts     e                     inz(*off)
011100120330       //  ?·§CM1idp  = Id processo?
011200120330     d   §CM1idp     e                     inz('2')
011300120330
011400120330       // -?Dati estratti via SQL da WFMTC00F?
011500120330     d WFMTC00F      e ds                  qualified  based(nullPtr)
011600120330     d wSQL_dati       ds                  inz
011700120330     d   MTCksc                            inz  like(WFMTC00F.MTCksc)
011800120330     d   MTCrag                            inz  like(WFMTC00F.MTCrag)
011900120330     d   MTCaas                            inz  like(WFMTC00F.MTCaas)
012000120330     d   MTClnp                            inz  like(WFMTC00F.MTClnp)
012100120330     d   MTCnrs                            inz  like(WFMTC00F.MTCnrs)
012200120330     d   MTCnsp                            inz  like(WFMTC00F.MTCnsp)
012300120330     d   MTCtbl                            inz  like(WFMTC00F.MTCtbl)
012400120330     d   MTCdsp                            inz  like(WFMTC00F.MTCdsp)
012500120330     d   MTCias                            inz  like(WFMTC00F.MTCias)
012600120330     d   MTCvas                            inz  like(WFMTC00F.MTCvas)
012700120330
012800120330       // -?Status ds?
012900120330     d Status         sds
013000120330     d   SDSpgm          *proc
013100120330     d*//JobName             244    253                                         Job name
013200120330     d*//JobUser             254    263                                         User name
013300120330     d*//JobNumber           264    269s 0                                      Job number
013400120330
013500120330       //--------------------------------------------------------------
013600120330       //?Definizione variabili.                                       ?
013700120330       //--------------------------------------------------------------
013800120330
013900120330       // -?Flags booleani?
014000120330     d $EoF            s               n   inz
014100120330     d $Err            s               n   inz
014200120330
014300120330       // -?Stringa SQL da eseguire?
014400120330     d wSQL            s           2048    Varying        inz
014500120330
014600120330       // -?Campi di comodo?
014700120330     d wDate_EUR       s               d   datfmt(*eur)  inz
014800120330     d wDate           s              8  0 inz
014900120330
015000120330       //--------------------------------------------------------------
015100120330       //?Definizione prototipi procedure.                             ?
015200120330       //--------------------------------------------------------------
015300120330
015400120330       // -?Reperimento dati utente?
015500120330      /copy gaitrasrc/srcProtoPR,TIBS34R
015600120330
015700120330       // -?Parametri API QCAPCMD (Process Commands)?
015800120330     d Qcmd            s           2048    inz  varying
015900120330      /copy qSysInc/qRpgleSrc,QCAPCMD
016000120330       // -?API QCAPCMD (Process Commands)?
016100120330      /copy gaitrasrc/srcProtoPR,QCAPCMD
016200120330
016300120330       // -?Parametri gestione errori API.?
016400120330      /copy qsysinc/qrpglesrc,QUSEC
016500120330
016600120330       //--------------------------------------------------------------
016700120330       //?Definizione key-list.                                        ?
016800120330       //--------------------------------------------------------------
016900120330
017000120330       // -?File TNTBE01L?
017100120330     d k05tntbe01    e ds                  extname(TNTBE01L:*key) inz
017200120330     d                                     prefix(K_)
017300120330
017400120330       //--------------------------------------------------------------
017500120330       //?M A I N - L I N E                                            ?
017600120330       //--------------------------------------------------------------
017700120330
017800120330     c     *Entry        plist
017900120330     c                   parm                    KPJBA
018000120330
018100120330      /free
018200120330
018300120330       // -?Operazioni iniziali?
018400120330       exsr  sr_RoutInz;
018500120330
018600120330       // -?Ciclo di lettura  e  Stampa (ed invio) e-mail?
018700120330       exsr  sr_ReadCursor;
018800120330       DoW  not $EoF;
018900120330         exsr  sr_PrtEmail;
019000120330         exsr  sr_ReadCursor;
019100120330       EndDo;
019200120330
019300120330       // -?Operazioni finali?
019400120330       exsr  sr_RoutEnd;
019500120330
019600120330       //--------------------------------------------------------------
019700120330       //?Operazioni iniziali.                                         ?
019800120330       //--------------------------------------------------------------
019900120330       BEGSR  sr_RoutInz;
020000120330
020100120330         *inLR = *on;
020200120330
020300120330         // -?Reperimento dati del lavoro?
020400120330         exsr  sr_DatiJob;
020500120330
020600120330         // -?*-=»>  Solo TEST in stampa  <«=-*?
020700120330         if  %parms = *zero;
020800120330           open  TNSB66P;
020900120330           exsr  sr_TEST;
021000120330           leavesr;
021100120330         endif;
021200120330
021300120330         // -?Reperimento tab. "MRA"?
021400120330         exsr  sr_TabMRA;
021500120330
021600120330         // -?Preparazione stringa SQL?
021700120330         exsr  sr_PrepSQL;
021800120330
021900120330         // -?Apertura cursore SQL?
022000120330         exsr  sr_OpenCursor;
022100120330
022200120330       ENDSR;
022300120330
022400120330       //--------------------------------------------------------------
022500120330       //?Reperimento Dati del job (Utente/Operativi).                 ?
022600120330       //--------------------------------------------------------------
022700120330       BEGSR  sr_DatiJob;
022800120330
022900120330         in(E) §AzUte;
023000120330         if NOT %error;
023100120330           in(E) §DatiUte;
023200120330         endif;
023300120330         if %error or RSut = *blanks;
023400120330           clear TIBS34ds;
023500120330           tibs34r(tibs34ds);
023600120330           in §AzUte;
023700120330           in §DatiUte;
023800120330         endif;
023900120330
024000120330       ENDSR;
024100120330
024200120330       //--------------------------------------------------------------
024300120330       //?Reperimento dati da tab. "MRA".                              ?
024400120330       //--------------------------------------------------------------
024500120330       BEGSR  sr_TabMRA;
024600120330
024700120330         reset  dMRAdan;
024800120330
024900120330         k_TBEcod = 'MRA';
025000120330         k_TBEke1 = SDSpgm;
025100120330         k_TBEke2 = *blank;
025200120330
025300120330         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
025400120330
025500120330         if  %found(TNTBE01L)   and   TBEatb = *blank;
025600120330           dMRAdan = TBEuni;
025700120330         endif;
025800120330
025900120330       ENDSR;
026000120330
026100120330       //--------------------------------------------------------------
026200120330       //?Impostazione dati per stampa di prova.                       ?
026300120330       //--------------------------------------------------------------
026400120330       BEGSR  sr_TEST;
026500120330
026600120330         MTCaas = *year;
026700120330         MTClnp = 321;
026800120330         MTCnrs = 98;
026900120330         MTCnsp = 1234567;
027000120330         MTCdsp = %dec( %date() );
027100120330         MTCtbl = 'XY';
027200120330         MTCksc = 9876543;
027300120330         MTCrag = *all'W';
027400120330         MTCias = 1234567890.123;
027500120330         MTCvas = 'ITL';
027600120330
027700120330       ENDSR;
027800120330
027900120330       //--------------------------------------------------------------
028000120330       //?Preparazione stringa SQL.                                    ?
028100120330       //--------------------------------------------------------------
028200120330       BEGSR  sr_PrepSQL;
028300120330
028400120330         wSQL = 'select mtcKSC, mtcRAG, mtcAAS, mtcLNP, mtcNRS, +
028500120330                        mtcNSP, mtcTBL, mtcDSP, mtcIAS, mtcVAS +
028600120330                   from WFMTC00F +
028700120330                  where mtcASS = ''ASS'' +
028800120330                    for fetch only';
028900120330
029000120330       ENDSR;
029100120330
029200120330       //--------------------------------------------------------------
029300120330       //?Apertura cursore.                                            ?
029400120330       //--------------------------------------------------------------
029500120330       BEGSR  sr_OpenCursor;
029600120330
029700120330         // -?Dichiarazione cursore?
029800120330         exec sql   prepare S1   from :wSQL;
029900120330         exec sql   declare C1   cursor for S1;
030000120330
030100120330         // -?Apertura del cursore?
030200120330         exec sql   open C1;
030300120330
030400120330         if  SQLcode < *zero;
030500120330           $Err = *on;
030600120330           $EoF = *on;
030700120330           exsr  sr_PrintErr;
030800120330         endif;
030900120330
031000120330       ENDSR;
031100120330
031200120330       //--------------------------------------------------------------
031300120330       //?Lettura cursore.                                             ?
031400120330       //--------------------------------------------------------------
031500120330       BEGSR  sr_ReadCursor;
031600120330
031700120330         clear  wSQL_dati;
031800120330
031900120330         exec sql   fetch next   from C1   into :wSQL_dati;
032000120330
032100120330         select;
032200120330           when  SQLcode = 100;
032300120330             $EoF = *on;
032400120330           when  SQLcode < *zero;
032500120330             $Err = *on;
032600120330             $EoF = *on;
032700120330             exsr  sr_PrintErr;
032800120330           other;
032900120330             exsr  sr_PrtEmail;
033000120330         endsl;
033100120330
033200120330       ENDSR;
033300120330
033400120330       //--------------------------------------------------------------
033500120330       //?Chiusura cursore.                                            ?
033600120330       //--------------------------------------------------------------
033700120330       BEGSR  sr_CloseCursor;
033800120330
033900120330         // -?Chiusura del cursore?
034000120330         exec sql   close C1;
034100120330
034200120330       ENDSR;
034300120330
034400120330       //--------------------------------------------------------------
034500120330       //?Stampa segnalazione dell'errore rilevato.                    ?
034600120330       //--------------------------------------------------------------
034700120330       BEGSR  sr_PrintErr;
034800120330
034900120330         // -?Stampa del Dump?
035000120330         Dump(A);
035100120330
035200120330         // -?Stampa del Job-Log?
035300120330         Qcmd = 'DSPJOBLOG job(*) output(*print)';
035400120330         exsr  sr_ExecCmd;
035500120330
035600120330         // -?Chiusura del programma?
035700120330         exsr  sr_RoutEnd;
035800120330
035900120330       ENDSR;
036000120330
036100120330       //--------------------------------------------------------------
036200120330       //?Preparazione e-mail.                                         ?
036300120330       //--------------------------------------------------------------
036400120330       BEGSR  sr_PrtEmail;
036500120330
036600120330         // -?Esecuzione override al "corpo e-mail"?
036700120330         If  not  %open(TNSB66P);
036800120330
036900120330           // -?Override al file di stampa?
037000120330           //  ?(per impostarvi i dati per l'invio via e-mail)?
037100120330           exsr sr_Override;
037200120330
037300120330           // -?Apertura file di stampa?
037400120330           open  TNSB66P;
037500120330
037600120330           // -?Stampa testata?
037700120330           write  SB66tes;
037800120330
037900120330         EndIf;
038000120330
038100120330         wDate_Eur = %date( MTCdsp : *iso );
038200120330         pDSP = %dec( wDate_Eur );
038300120330
038400120330         write  SB66det;
038500120330
038600120330       ENDSR;
038700120330
038800120330       //--------------------------------------------------------------
038900120330       //?Override al file di stampa per impostarvi i dati per         ?
039000120330       //?  l'invio via e-mail.                                        ?
039100120330       //--------------------------------------------------------------
039200120330       BEGSR sr_Override;
039300120330
039400120330         reset TRTCM1ds;
039500120330         if  §MRAdmitt <> *blank;
039600120330           §CM1mitt = %trim(§MRAdmitt);
039700120330         endif;
039800120330         §CM1dst  = %trim(§MRAddest);
039900120330         §CM1tips = §MRAdreg;
040000120330         //§CM1var  = '*OBJM*' + §MRAddes;
040100120330         §CM1idp  = §MRAdidpro;
040200120330
040300120330         // -?Override al "corpo e-mail"?
040400120330         if  §MRAdreg <> *blank;
040500120330           Qcmd = c_CmdOvrPrtF
040600120330                + ' outq(' + %trim(§MRAdoutqi) + ')'
040700120330                + ' usrdfndta(''' + TRTCM1ds + ''')';
040800120330         else;
040900120330           Qcmd = c_CmdOvrPrtF;
041000120330         endif;
041100120330
041200120330         exsr  sr_ExecCmd;
041300120330
041400120330         //if  Qusei <> *blank;
041500120330         //  ...;
041600120330         //endif;
041700120330
041800120330       ENDSR;
041900120330
042000120330       //--------------------------------------------------------------
042100120330       //?Chiusura file stampa & Cancellazione override.               ?
042200120330       //--------------------------------------------------------------
042300120330       BEGSR sr_DeleteOvr;
042400120330
042500120330         // -?Cancellazione override al "corpo e-mail"?
042600120330         If  %open(TNSB66P);
042700120330
042800120330           // -?Chiusura file di stampa?
042900120330           close TNSB66P;
043000120330
043100120330           // -?Cancellazione override dal file di stampa?
043200120330           Qcmd = c_CmdDltOvr;
043300120330           exsr  sr_ExecCmd;
043400120330
043500120330         EndIf;
043600120330
043700120330       ENDSR;
043800120330
043900120330       //--------------------------------------------------------------
044000120330       //?Esecuzione del comando (già impostato).                      ?
044100120330       //--------------------------------------------------------------
044200120330       BEGSR  sr_ExecCmd;
044300120330
044400120330         clear Qcap0100;
044500120330         Qcabcsdh = *off;
044600120330         Qcapa    = *off;
044700120330         Qcacmdss = *off;
044800120330         Qcaerved = *allX'00';
044900120330
045000120330         clear Qusec;
045100120330         Qusbprv  = %size(Qusec);
045200120330
045300120330         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
045400120330                           %size(Qcap0100) : 'CPOP0100' : *omit :
045500120330                           0 : 0 : Qusec);
045600120330
045700120330         //if  Qusei <> *blank;
045800120330         //  ...;
045900120330         //endif;
046000120330
046100120330       ENDSR;
046200120330
046300120330       //--------------------------------------------------------------
046400120330       //?Operazioni finali.                                           ?
046500120330       //--------------------------------------------------------------
046600120330       BEGSR  sr_RoutEnd;
046700120330
046800120330         If  %open(TNSB66P);
046900120330
047000120330           // -?Stampa piede?
047100120330           write  SB66end;
047200120330
047300120330           // -?Chiusura file stampa & Cancellazione override?
047400120330           exsr sr_DeleteOvr;
047500120330
047600120330         EndIf;
047700120330
047800120330         // -?Uscita?
047900120330         return;
048000120330
048100120330       ENDSR;
048200120330
048300120330      /end-free
