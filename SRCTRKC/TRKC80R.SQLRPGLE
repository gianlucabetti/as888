000100150223       //==============================================================
000200150223       //?TRKC80R * Pulizia dei Clienti in Campagna.                   ?
000300150223       //==============================================================
000400150223
000500150223       //--------------------------------------------------------------
000600150223       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150223       //--------------------------------------------------------------
000800150223
000900150223     /*PRM  dbgview(*source)
001000150223     /*END
001100150223
001200150223       //--------------------------------------------------------------
001300150223       //?Specifiche di controllo.                                     ?
001400150223       //--------------------------------------------------------------
001500150223
001600150223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150223     h dftactgrp(*no)
001800150223     h bnddir('TRUL')
001900150223     h alwnull(*inputonly)
002000150223
002100150223       //--------------------------------------------------------------
002200150223       //?Dichiarazione file.                                          ?
002300150223       //--------------------------------------------------------------
002400150224
002500150224       // -?Anagrafica Clienti in Campagna?
002600150305     fTICMC01L  if   e           k disk
002700150305
002800150305       // -?Fasi Avanzamento Clienti in Campagne Commerciali?
002900150305     fTICMF01L  if   e           k disk
003000150223
003100150223       // -?File di stampa per eventuale invio e-mail?
003200150223     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003300150224
003400150224       // -?File di stampa (SE non richiesto l'aggiornamento)?
003500150224     fQSYSPRT   o    f  132        printer  usropn  oflind(*inOA)
003600150223
003700150223       //--------------------------------------------------------------
003800150223       //?Definizione costanti.                                        ?
003900150223       //--------------------------------------------------------------
004000150223
004100150223       // -?Numero Campagna per l'esclusione dei Clienti?
004200150223     d c_Campagna_ClientiEsclusi...
004300150223     d                 c                   const(5001)
004400150226      *// // -?Fase di Esclusione dalla Campagna comm.le per il Cliente?
004500150226     d*// c_FaseEsclus    c                   const(' 90')
004600150226       // -?Fase di Chiusura dalla Campagna comm.le per il Cliente?
004700150226     d c_FaseChiusura  c                   const(' 30')
004800150223
004900150223       // -?Comandi da eseguire?
005000150223     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005100150223     d                                           file(PRTEMAIL) +
005200150223     d                                           pagesize(66 132) +
005300150223     d                                           lpi(6) cpi(10) +
005400150223     d                                           ovrscope(*actgrpdfn) +
005500150223     d                                           usrdta(''ERR-TRKC80'') +
005600150223     d                                           ')
005700150223     d c_CmdDltOvr     c                   const('DLTOVR +
005800150223     d                                            file(PRTEMAIL) +
005900150223     d                                            lvl(*actgrpdfn)')
006000150223
006100150223       //--------------------------------------------------------------
006200150223       //?Definizione schiere.                                         ?
006300150223       //--------------------------------------------------------------
006400150223
006500150223
006600150223       //--------------------------------------------------------------
006700150223       //?Definizione aree dati.                                       ?
006800150223       //--------------------------------------------------------------
006900150223
007000150223       // -?Dati utente?
007100150223     d §AzUte        e ds                  extname(AZUTE00F)
007200150223     d                                     dtaara
007300150223     d §DatiUte      e ds                  extname(dDatiUte)
007400150223     d                                     dtaara
007500150223
007600150223       //--------------------------------------------------------------
007700150223       //?Definizione strutture dati.                                  ?
007800150223       //--------------------------------------------------------------
007900150223
008000150223       // -?Status?
008100150223     d Status         sds
008200150223     d   SDSpgm          *proc
008300150223     d*//SDSprm          *parms
008400150223     d*//SDSdta              191    198
008500150223     d   SDSjobName          244    253
008600150223     d   SDSjobUser          254    263
008700150223     d   SDSjobNbr           264    269s 0
008800150223
008900150223       // -?Dati estratti via SQL?
009000150223     d wSQL_ds         ds                  qualified     inz
009100150223     d   wNCM                              like(CMFncm)  inz
009200150223     d   wKSU                              like(CMFksu)  inz
009300150223     d   wKSC                              like(CMFksc)  inz
009400150223     d   wCPO                              like(CMFcpo)  inz
009500150223
009600150223       // -?Data/Ora attuali?
009700150223     d wTime_ds        ds                  inz
009800150223     d   wDate                        8s 0 inz
009900150223     d   wTime                        6s 0 inz
010000150223
010100150223       // -?Parametri ricevuti?
010200150223     d KPJBA         e ds
010300150223     d TRKC06ds      e ds
010400150223
010500150223       // -?Tab. "MRA" = Mailing - regole x invio da gestionale?
010600150223     d dMRAdan       e ds                  inz
010700150223     d   §MRADdes    e                     inz('ERR Puliz.Cli.in+
010800150223     d                                         Campagna')
010900150223     d   §MRADreg    e                     inz('COR')
011000150223     d   §MRADmitt   e                     inz('ced')
011100150223     d   §MRADdest   e                     inz('ced@brt.it; +
011200150223     d                                          stefano.merighi+
011300150223     d                                          @brt.it')
011400150223     d   §MRADidPro  e                     inz('2')
011500150223     d   §MRADoutqI  e                     inz('EMAILIN')
011600150223
011700150223       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
011800150223     d TRTCM1ds      e ds                  inz
011900150223         //?·§CM1mitt = Indirizzo e-mail del mittente?
012000150223     d   §CM1mitt    e                     inz('ced')
012100150223         //?·§CM1dst  = Indirizzo e-mail del destinatario?
012200150223     d   §CM1dst     e                     inz('ced@brt.it;+
012300150223     d                                          stefano.merighi+
012400150223     d                                          @brt.it')
012500150223         //?·§CM1tips = Tipo lettera via e-mail:?
012600150223         // ?"LET" = testo allegato in corpo con logo?
012700150223         //         ?(richiede righe libere iniziali per il logo)?
012800150223         // ?"COR" = testo integrato senza logo?
012900150223         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
013000150223     d   §CM1tips    e                     inz('COR')
013100150223         //?·§CM1po   = Filiale?
013200150223     d   §CM1po      e                     inz('046')
013300150223         //?·§CM1var  = Oggetto e-mail?
013400150223     d   §CM1var     e                     inz('*OBJM*+
013500150223     d                                     ERRORE in Pulizia dei -
013600150223     d                                     Clienti in Campagna')
013700150223         //?·§CM1sts  = Stato?
013800150223     d   §CM1sts     e                     inz(*off)
013900150223         //?·§CM1sts  = Id processo?
014000150223     d   §CM1idp     e                     inz('2')
014100150223
014200150223      //---------------------------------------------------------------
014300150223      //?Definizione variabili globali.
014400150223      //---------------------------------------------------------------
014500150223
014600150223       // -?Flags booleani?
014700150223     d $EoF            s               n   inz
014800150224     d $SoloStampa     s               n   inz
014900150223
015000150223       // -?Stringa SQL da eseguire?
015100150223     d wSQL            s           7000    inz  varying
015200150223
015300150223       // -?Codice errore rilevato?
015400150223     d wErr            s              3  0 inz
015500150223
015600150223       // -?Data *ISO?
015700150223     d wDate_Eur       s               d   inz  datfmt(*EUR)
015800150223
015900150223       //--------------------------------------------------------------
016000150223       //?Definizione prototipi/procedure usate.                       ?
016100150223       //--------------------------------------------------------------
016200150223
016300150223       // -?Reperimento dati utente?
016400150223     d TIBS34ds      e ds                  inz
016500150223      /copy gaitrasrc/srcProtoPR,TIBS34R
016600150223
016700150223       // -?Modifica campagna x codice unificante?
016800150223     d TRKC75ds      e ds                  inz
016900150223      /copy gaitrasrc/srcProtoPR,TRKC75R
017000150223
017100150223       // -?Reperimento dati tabelle?
017200150223      /copy gaitrasrc/srcProtoPI,TRULTAB
017300150223      /copy gaitrasrc/srcProtoPR,TRULTAB
017400150223
017500150223       // -?Parametri API QCAPCMD (Process Commands)?
017600150223     d Qcmd            s           2048    inz  varying
017700150223      /copy qSysInc/qRpgleSrc,QCAPCMD
017800150223       // -?API QCAPCMD (Process Commands)?
017900150223      /copy gaitrasrc/srcProtoPR,QCAPCMD
018000150223
018100150223       // -?Parametri gestione errori API.?
018200150223      /copy qsysinc/qrpglesrc,QUSEC
018300150223
018400150223       //--------------------------------------------------------------
018500150223       //?Definizione key-list.                                        ?
018600150223       //--------------------------------------------------------------
018700150223
018800150224       // -?File TICMC01L: Anagrafica Clienti in Campagna?
018900150224     d keyTICMC01    e ds                  extname( TICMC01L : *key )
019000150224     d                                     prefix( k_ )   inz
019100150305
019200150305       // -?File TICMF01L: Fasi Avanzamento Clienti in Campagne Comm.li?
019300150305     d keyTICMF01    e ds                  extname( TICMF01L : *key )
019400150305     d                                     prefix( k_ )   inz
019500150223
019600150223       //--------------------------------------------------------------
019700150223       //?Riepilogo indicatori utilizzati.                             ?
019800150223       //--------------------------------------------------------------
019900150223
020000150223
020100150223       //--------------------------------------------------------------
020200150223       //?M A I N - L I N E                                            ?
020300150223       //--------------------------------------------------------------
020400150223
020500150223     c     *Entry        plist
020600150223     c                   parm                    KPJBA
020700150223
020800150223      /free
020900150223
021000150223       // -?Operazioni iniziali?
021100150223       exsr  sr_RoutInz;
021200150223
021300150305       // -?Preparazione SQL per estrazione dati da TICMC00F e TICMI00F?
021400150223       exsr  sr_PrepSQL;
021500150223
021600150305       // -?Ciclo di elaborazione file TICMC00F e TICMI00F?
021700150223       exsr  sr_OpenCursor;
021800150223
021900150223       DoW  Not $EoF;
022000150223         exsr  sr_ReadCursor;
022100150223       EndDo;
022200150223
022300150223       exsr  sr_CloseCursor;
022400150305
022500150305       // -?Ciclo di elaborazione file TICMF00F?
022600150305       clear  keyTICMC01;
022700150305       setll  %kds( keyTICMC01 )  TICMC000;
022800150305       read   TICMC000;
022900150305
023000150305       DoW  Not %eof(TICMC01L)  and  CMCncm < c_Campagna_ClientiEsclusi;
023100150305         if  CMCufe = c_FaseChiusura;
023200150305           exsr  sr_ElabTICMF;
023300150305         endif;
023400150305         read  TICMC000;
023500150305       EndDo;
023600150223
023700150223       // -?Operazioni finali?
023800150223       exsr  sr_RoutEnd;
023900150223
024000150223       //--------------------------------------------------------------
024100150223       //?Operazioni iniziali.                                         ?
024200150223       //--------------------------------------------------------------
024300150223       BEGSR  sr_RoutInz;
024400150223
024500150223         // -?Impostazione opzioni per SQL?
024600150223         exec sql   set  option  DynUsrPrf = *Owner,
024700150223                                 CloSqlCsr = *EndMod;
024800150223
024900150223         // -?Impostazione chiusura?
025000150223         *inLR = *on;
025100150223
025200150223         // -?Reperimento dati job?
025300150223         exsr  sr_DatiJob;
025400150223
025500150223         // -?Reperimento data e ora attuali?
025600150223         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
025700150223         wDate_Eur = %date( wDate : *Iso );
025800150223         wDate     = %dec( wDate_Eur );
025900150224
026000150224         // -?Verifica SE richiesta solo la stampa?
026100150224         $SoloStampa = ( %subst( kpjbu : 1 : 10 ) = 'STAMPA    ' );
026200150224         if  $SoloStampa;
026300150224           open  QSYSPRT;
026400150224           except  STAMPAtxt;
026500150224           *inOA = *off;
026600150224         endif;
026700150223
026800150223       ENDSR;
026900150223
027000150223       //--------------------------------------------------------------
027100150223       //?Reperimento Dati del job (Utente/Operativi).                 ?
027200150223       //--------------------------------------------------------------
027300150223       BEGSR  sr_DatiJob;
027400150223
027500150223         in(e) §AzUte;
027600150223         if NOT %error;
027700150223           in(e) §DatiUte;
027800150223         endif;
027900150223         if %error or RSut = *blank;
028000150223           tibs34r ( tibs34ds );
028100150223           in §AzUte;
028200150223           in §DatiUte;
028300150223         endif;
028400150223
028500150223       ENDSR;
028600150223
028700150223       //--------------------------------------------------------------
028800150223       //?Preparazione SQL.                                            ?
028900150223       //--------------------------------------------------------------
029000150223       BEGSR  sr_PrepSQL;
029100150223
029200150223         // -?Preparazione SQL?
029300150223         clear  wSQL;
029400150223         wSQL = 'with +
029500150226                    KSU_CMC as ( select CMCncm as NCM, CMCksu as KSU, +
029600150226                                        CMCksc as KSC, CMCcpo as CPO, +
029700150224                                        CMCufe, CMCcch +
029800150223                                   from TICMC00F +
029900150223                                  where CMCncm in ( 1, 2 ) +
030000150223                                    and CMCcch <> ''  '' +
030100150226                                  order by CMCksu ), +
030200150305                    KSU_CMI as ( select CMIncm as NCM, CMIksu as KSU, +
030300150305                                        CMIksc as KSC, CMIcpo as CPO, +
030400150305                                        CMCufe, CMCcch +
030500150305                                   from TICMI00F inner join TICMC00F +
030600150305                                     on CMIncm = CMCncm +
030700150305                                    and CMIksu = CMCksu +
030800150305                                    and CMIksc = CMCksc +
030900150305                                    and CMIcpo = CMCcpo +
031000150305                                  where CMIncm in ( 1, 2 ) +
031100150305                                    and CMCcch = '' '' +
031200150305                                    and CMIpde >= -15 +
031300150305                                  order by CMIksu ) +
031400150226                 select * from KSU_CMC +
031500150226                  union +
031600150305                 select * from KSU_CMI +
031700150226                  order by NCM, KSU, KSC, CPO +
031800150223                    for fetch only';
031900150223
032000150223         exec sql   prepare S1   from :wSQL;
032100150223
032200150223         // -?Dichiarazione cursore?
032300150223         exec sql   declare C1   cursor for S1;
032400150223
032500150223         if  SQLcode < *zero;
032600150223           $EoF = *on;
032700150223           exsr  sr_PrintErrSQL;
032800150223         endif;
032900150223
033000150223       ENDSR;
033100150223
033200150223       //--------------------------------------------------------------
033300150223       //?Apertura cursore.                                            ?
033400150223       //--------------------------------------------------------------
033500150223       BEGSR  sr_OpenCursor;
033600150223
033700150223         // -?Apertura del cursore?
033800150223         exec sql   open C1;
033900150223
034000150223         if  SQLcode < *zero;
034100150223           $EoF = *on;
034200150223           exsr  sr_PrintErrSQL;
034300150223         endif;
034400150223
034500150223       ENDSR;
034600150223
034700150223       //--------------------------------------------------------------
034800150223       //?Lettura cursore.                                             ?
034900150223       //--------------------------------------------------------------
035000150223       BEGSR  sr_ReadCursor;
035100150223
035200150223         clear  wSQL_ds;
035300150223
035400150223         exec sql   fetch next   from C1   into :wSQL_ds;
035500150223
035600150223         Select;
035700150223
035800150223           // -?Fine File?
035900150223           When  SQLcode = 100;
036000150223             $EoF = *on;
036100150223
036200150223           // -?Errore SQL?
036300150223           When  SQLcode < *zero;
036400150223             $EoF = *on;
036500150223             exsr  sr_PrintErrSQL;
036600150223
036700150224           // -?Richiesta SOLO stampa?
036800150224           When  $SoloStampa;
036900150224             exsr  sr_Stampa;
037000150224
037100150223           // -?Elaborazione singolo cliente?
037200150223           Other;
037300150223             exsr  sr_ElabRec;
037400150223
037500150223         EndSl;
037600150223
037700150223       ENDSR;
037800150223
037900150223       //--------------------------------------------------------------
038000150223       //?Chiusura cursore.                                            ?
038100150223       //--------------------------------------------------------------
038200150223       BEGSR  sr_CloseCursor;
038300150223
038400150223         // -?Chiusura del cursore?
038500150223         exec sql   close C1;
038600150223
038700150223       ENDSR;
038800150305
038900150305       //--------------------------------------------------------------
039000150305       //?Elaborazione singolo Cliente dal file TICMF00F.              ?
039100150305       //--------------------------------------------------------------
039200150305       BEGSR  sr_ElabTICMF;
039300150305
039400150305         clear  keyTICMF01;
039500150305         k_CMFncm = CMCncm;
039600150305         k_CMFksu = CMCksu;
039700150305         k_CMFksc = CMCksc;
039800150305         k_CMFcpo = CMCcpo;
039900150305         k_CMFacm = CMCufe;
040000150305
040100150305         chain  %kds( keyTICMF01 : 5 )  TICMF000;
040200150305
040300150305         If  %found(TICMF01L)  and  CMFpea = *zero;
040400150305
040500150305           // -?Modifica Campagna per Cliente Unificante?
040600150305           clear  wSQL_ds;
040700150305           if  $SoloStampa;
040800150305             exsr  sr_Stampa;
040900150305           else;
041000150305             wSQL_ds.wNCM = CMFncm;
041100150305             wSQL_ds.wKSU = CMFksu;
041200150305             wSQL_ds.wKSC = CMFksc;
041300150305             wSQL_ds.wCPO = CMFcpo;
041400150305             exsr  sr_ElabRec;
041500150305           endif;
041600150305
041700150305         EndIf;
041800150305
041900150305       ENDSR;
042000150305
042100150305       //--------------------------------------------------------------
042200150305       //?Stampa del singolo Cliente eliminabile della Campagna.       ?
042300150305       //--------------------------------------------------------------
042400150305       BEGSR  sr_Stampa;
042500150305
042600150305         // -?SE si viene dalla lettura via SQL: mancano i dati di TICMC?
042700150305         //  ?SE si viene dalla lettura di TICMF: i dati di TICMC ci sono già?
042800150305         if  wSQL_ds.wNCM <> *zero;
042900150305           clear  keyTICMC01;
043000150305           k_CMCncm = wSQL_ds.wNCM;
043100150305           k_CMCksu = wSQL_ds.wKSU;
043200150305           k_CMCksc = wSQL_ds.wKSC;
043300150305           k_CMCcpo = wSQL_ds.wCPO;
043400150305           chain  %kds( keyTICMC01 )  TICMC000;
043500150305           if  Not %found(TICMC01L);
043600150305             clear  CMCufe;
043700150305             clear  CMCcch;
043800150305             clear  CMCdch;
043900150305           endif;
044000150305         endif;
044100150305
044200150305         if  *inOA;
044300150305           except  STAMPAtxt;
044400150305           *inOA = *off;
044500150305         endif;
044600150305
044700150305         except STAMPAdet;
044800150305
044900150305       ENDSR;
045000150305
045100150305       //--------------------------------------------------------------
045200150305       //?Elaborazione singolo Cliente da eliminare dalla Campagna.    ?
045300150305       //--------------------------------------------------------------
045400150305       BEGSR  sr_ElabRec;
045500150305
045600150305         // -?Modifica Campagna per Cliente Unificante?
045700150305         clear  TRKC75ds;
045800150305         iKC75ncm  = wSQL_ds.wNCM;
045900150305         iKC75ksu  = wSQL_ds.wKSU;
046000150305         iKC75ksc  = wSQL_ds.wKSC;
046100150305         iKC75cpo  = wSQL_ds.wCPO;
046200150305         iKC75nncm = c_Campagna_ClientiEsclusi;
046300150305
046400150305         TRKC75R ( kpjba : TRKC75ds );
046500150305
046600150305         if  oKC75err <> *blank;
046700150305           wErr = 2;
046800150305           exsr  sr_SendEmail;
046900150305         endif;
047000150305
047100150305       ENDSR;
047200150223
047300150223       //--------------------------------------------------------------
047400150223       //?Stampa segnalazione dell'errore rilevato via SQL             ?
047500150223       //--------------------------------------------------------------
047600150223       BEGSR  sr_PrintErrSQL;
047700150223
047800150223         // -?Stampa del Dump?
047900150223         Dump(A);
048000150223
048100150223         // -?Stampa del Job-Log?
048200150223         Qcmd = 'DSPJOBLOG job(*) output(*print)';
048300150223         exsr  sr_ExecCmd;
048400150223
048500150223         // -?Invio e-mail con errore rilevato?
048600150223         wErr = 1;
048700150223         exsr  sr_SendEmail;
048800150223
048900150223         // -?Chiusura del programma?
049000150223         exsr  sr_RoutEnd;
049100150223
049200150223       ENDSR;
049300150223
049400150223       //--------------------------------------------------------------
049500150223       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
049600150223       //--------------------------------------------------------------
049700150223       BEGSR  sr_SendEmail;
049800150223
049900150223         // -?Override al file di stampa ed apertura dello stesso?
050000150223         If  NOT  %open(PrtEMAIL);
050100150223
050200150223           // -?Reperimento regole per mailing da tab. "MRA"?
050300150223           clear  dMRAdan;
050400150223           if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
050500150223                            *omit : *omit :
050600150223                            *omit : *omit :
050700150223                            *omit : *omit : *omit : *omit :
050800150223                            *omit : *omit : *omit : *omit :
050900150223                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
051000150223                            = *zero      AND
051100150223                 ds_TNTBE.TBEatb = *blank;
051200150223             dMRAdan = ds_TNTBE.TBEuni;
051300150223           endif;
051400150223
051500150223           // -?Impostazione regole per mailing?
051600150223           §CM1mitt = %trim(§MRADmitt);
051700150223           §CM1dst  = %trim(§MRADdest);
051800150223           §CM1tips = §MRAdreg;
051900150223           §CM1var  = '*OBJM*' + §MRADdes;
052000150223           §CM1idp  = §MRADidPro;
052100150223
052200150223           // -?Esecuzione override al file di stampa (per mailing)?
052300150223           Qcmd = c_CmdOvrPrtF
052400150223                + ' outq(' + %trim(§MRADoutqI) + ')'
052500150223                + ' usrdfndta(''' + TRTCM1ds + ''')';
052600150223           exsr  sr_ExecCmd;
052700150223
052800150223           // -?Apertura file di stampa?
052900150223           open  PrtEMAIL;
053000150223           *inOF = *on;
053100150223
053200150223         EndIf;
053300150223
053400150223         // -?Preparazione e-Mail per il singolo errore rilevato?
053500150223         if  *inOF;
053600150223           except  PRTtxt;
053700150223           *inOF = *off;
053800150223         endif;
053900150223
054000150223         Select;
054100150223           When  wErr = 1;
054200150223             except  PRTerr01;
054300150223           When  wErr = 2;
054400150223             except  PRTerr02;
054500150223         EndSl;
054600150223
054700150223       ENDSR;
054800150223
054900150223       //--------------------------------------------------------------
055000150223       //?Esecuzione del comando (già impostato).                      ?
055100150223       //--------------------------------------------------------------
055200150223       BEGSR  sr_ExecCmd;
055300150223
055400150223         clear Qcap0100;
055500150223         Qcabcsdh = *off;
055600150223         Qcapa    = *off;
055700150223         Qcacmdss = *off;
055800150223         Qcaerved = *allX'00';
055900150223
056000150223         clear Qusec;
056100150223         Qusbprv  = %size(Qusec);
056200150223
056300150223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
056400150223                           %size(Qcap0100) : 'CPOP0100' : *omit :
056500150223                           0 : 0 : Qusec);
056600150223
056700150223         //if  Qusei <> *blank;
056800150223         //  ...;
056900150223         //endif;
057000150223
057100150223       ENDSR;
057200150223
057300150223       //--------------------------------------------------------------
057400150223       //?Operazioni finali.                                           ?
057500150223       //--------------------------------------------------------------
057600150223       BEGSR  sr_RoutEnd;
057700150223
057800150223         // -?Chiusura funzioni precedentemente aperte?
057900150223         cloTabella();
058000150223
058100150223         // -?SE stampato almeno un Errore:?
058200150223         //  ?stampa "fine" e chiude il file di stampa?
058300150223         if  %open(PrtEMAIL);
058400150223           except  PRTend;
058500150223           close   PrtEMAIL;
058600150223         endif;
058700150224
058800150224         // -?SE richiesta SOLO stampa:?
058900150224         //  ?stampa "fine" e chiude il file di stampa?
059000150224         if  $SoloStampa  and  %open(QSYSPRT);
059100150224           except  STAMPAend;
059200150224           close   QSYSPRT;
059300150224         endif;
059400150223
059500150223         // -?Uscita dal *pgm?
059600150223         return;
059700150223
059800150223       ENDSR;
059900150223
060000150223      /end-free
060100150223
060200150223       //--------------------------------------------------------------
060300150223       //?S P O O L - F I L E S                                        ?
060400150223       //--------------------------------------------------------------
060500150223
060600150223     oPrtEMAIL  e            PRTtxt            1
060700150223     o                       RSUT
060800150223     o                                        +   5 'PULIZIA DEI CLIENTI IN +
060900150223     o                                               CAMPAGNA'
061000150223     o                       SDSpgm           +   5
061100150223     o                       wDate         y  +   5
061200150223     o          e            PRTtxt      1
061300150223     o                       KNSIF
061400150223     o                       KNMUS            +   1
061500150223     o                                        +   4 '-----------------------+
061600150223     o                                               --------'
061700150223     o                                        +   5 'Pag.'
061800150223     o                       Page          z  +   0
061900150224     o                       wTime            +   7 '  :  :  '
062000150223      *
062100150223     o          e            PRTerr01    2
062200150223     o                                              'Rilevato ERRORE n-
062300150223     o                                              ella pulizia dei c-
062400150223     o                                              lienti in Campagna-
062500150223     o                                              .'
062600150223     o          e            PRTerr01    2
062700150223     o                                              'Consultare le sta-
062800150223     o                                              mpe del lavoro'
062900150223     o
063000150223     o                       SDSjobNbr     3  +   1
063100150223     o                                        +   0 '/'
063200150223     o                       SDSjobUser       +   0
063300150223     o                                        +   0 '/'
063400150223     o                       SDSjobName       +   0
063500150223      *
063600150223     o          e            PRTerr02    2
063700150223     o                                              'Campagna'
063800150223     o                       wSQL_ds.wNCM  z  +   1
063900150223     o                                        +   2 'Unificante'
064000150223     o                       wSQL_ds.wKSU     +   1
064100150223     o                                        +   1 '-'
064200150223     o                       oKC75msg         +   1
064300150223      *
064400150223     o          e            PRTend      2
064500150223     o                                        +  10 '***  Fine Lista  ***'
064600150224
064700150224       //--------------------------------------------------------------
064800150224
064900150224     oQSYSPRT   e            STAMPAtxt         1
065000150224     o                       RSUT
065100150224     o                                        +   5 'PULIZIA DEI CLIENTI IN +
065200150224     o                                               CAMPAGNA'
065300150224     o                       SDSpgm           +   5
065400150224     o                       wDate         y  +   5
065500150224     o          e            STAMPAtxt   1
065600150224     o                       KNSIF
065700150224     o                       KNMUS            +   1
065800150224     o                                        +   4 '-----------------------+
065900150224     o                                               --------'
066000150224     o                                        +   5 'Pag.'
066100150224     o                       Page2         z  +   0
066200150224     o                       wTime            +   7 '  :  :  '
066300150224     o          e            STAMPAtxt   2
066400150224     o                                              'Campagna'
066500150224     o                                        +   3 'Unific.'
066600150224     o                                        +   3 'Cliente'
066700150224     o                                        +   3 ' Potenziale'
066800150224     o                                        +   3 'Fase'
066900150224     o                                        +   3 'C.Ch'
067000150224     o                                        +   3 'Data Chius'
067100150224     o          e            STAMPAtxt   1  1
067200150224     o                                              '--------'
067300150224     o                                        +   3 '-------'
067400150224     o                                        +   3 '-------'
067500150224     o                                        +   3 '-----------'
067600150224     o                                        +   3 '----'
067700150224     o                                        +   3 '----'
067800150224     o                                        +   3 '----------'
067900150224      *
068000150224Campag*a   Unific.   Cliente    Potenziale   Fase   C.Ch   Data (CMC)
068100150224123456*    1234567   1234567   12345678901   123     12    1234/56/78
068200150224     o          e            STAMPAdet   1
068300150305     o                       CMCncm        z
068400150305     o                       CMCksu           +   4
068500150305     o                       CMCksc           +   3
068600150305     o                       CMCcpo           +   3
068700150224     o                       CMCufe           +   3
068800150224     o                       CMCcch           +   5
068900150224     o                       CMCdch           +   4 '    /  /  '
069000150224      *
069100150224     o          e            STAMPAend   2
069200150224     o                                        +  25 '***  Fine Lista  ***'
