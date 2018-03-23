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
002400150223
002500150223       // -?File di stampa per eventuale invio e-mail?
002600150223     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
002700150223
002800150223       //--------------------------------------------------------------
002900150223       //?Definizione costanti.                                        ?
003000150223       //--------------------------------------------------------------
003100150223
003200150223       // -?Numero Campagna per l'esclusione dei Clienti?
003300150223     d c_Campagna_ClientiEsclusi...
003400150223     d                 c                   const(5001)
003500150224       // -?Fase di Esclusione dalla Campagna comm.le per il Cliente?
003600150224     d c_FaseEsclus    c                   const(' 90')
003700150223
003800150223       // -?Comandi da eseguire?
003900150223     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004000150223     d                                           file(PRTEMAIL) +
004100150223     d                                           pagesize(66 132) +
004200150223     d                                           lpi(6) cpi(10) +
004300150223     d                                           ovrscope(*actgrpdfn) +
004400150223     d                                           usrdta(''ERR-TRKC80'') +
004500150223     d                                           ')
004600150223     d c_CmdDltOvr     c                   const('DLTOVR +
004700150223     d                                            file(PRTEMAIL) +
004800150223     d                                            lvl(*actgrpdfn)')
004900150223
005000150223       //--------------------------------------------------------------
005100150223       //?Definizione schiere.                                         ?
005200150223       //--------------------------------------------------------------
005300150223
005400150223
005500150223       //--------------------------------------------------------------
005600150223       //?Definizione aree dati.                                       ?
005700150223       //--------------------------------------------------------------
005800150223
005900150223       // -?Dati utente?
006000150223     d §AzUte        e ds                  extname(AZUTE00F)
006100150223     d                                     dtaara
006200150223     d §DatiUte      e ds                  extname(dDatiUte)
006300150223     d                                     dtaara
006400150223
006500150223       //--------------------------------------------------------------
006600150223       //?Definizione strutture dati.                                  ?
006700150223       //--------------------------------------------------------------
006800150223
006900150223       // -?Status?
007000150223     d Status         sds
007100150223     d   SDSpgm          *proc
007200150223     d*//SDSprm          *parms
007300150223     d*//SDSdta              191    198
007400150223     d   SDSjobName          244    253
007500150223     d   SDSjobUser          254    263
007600150223     d   SDSjobNbr           264    269s 0
007700150223
007800150223       // -?Dati estratti via SQL?
007900150223     d TICMF00F      e ds                  extname(TICMF00F)
008000150223     d                                     based(nullPtr)
008100150223     d wSQL_ds         ds                  qualified     inz
008200150223     d   wNCM                              like(CMFncm)  inz
008300150223     d   wKSU                              like(CMFksu)  inz
008400150223     d   wKSC                              like(CMFksc)  inz
008500150223     d   wCPO                              like(CMFcpo)  inz
008600150223
008700150223       // -?Data/Ora attuali?
008800150223     d wTime_ds        ds                  inz
008900150223     d   wDate                        8s 0 inz
009000150223     d   wTime                        6s 0 inz
009100150223
009200150223       // -?Parametri ricevuti?
009300150223     d KPJBA         e ds
009400150223     d TRKC06ds      e ds
009500150223
009600150223       // -?Tab. "MRA" = Mailing - regole x invio da gestionale?
009700150223     d dMRAdan       e ds                  inz
009800150223     d   §MRADdes    e                     inz('ERR Puliz.Cli.in+
009900150223     d                                         Campagna')
010000150223     d   §MRADreg    e                     inz('COR')
010100150223     d   §MRADmitt   e                     inz('ced')
010200150223     d   §MRADdest   e                     inz('ced@brt.it; +
010300150223     d                                          stefano.merighi+
010400150223     d                                          @brt.it')
010500150223     d   §MRADidPro  e                     inz('2')
010600150223     d   §MRADoutqI  e                     inz('EMAILIN')
010700150223
010800150223       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
010900150223     d TRTCM1ds      e ds                  inz
011000150223         //?·§CM1mitt = Indirizzo e-mail del mittente?
011100150223     d   §CM1mitt    e                     inz('ced')
011200150223         //?·§CM1dst  = Indirizzo e-mail del destinatario?
011300150223     d   §CM1dst     e                     inz('ced@brt.it;+
011400150223     d                                          stefano.merighi+
011500150223     d                                          @brt.it')
011600150223         //?·§CM1tips = Tipo lettera via e-mail:?
011700150223         // ?"LET" = testo allegato in corpo con logo?
011800150223         //         ?(richiede righe libere iniziali per il logo)?
011900150223         // ?"COR" = testo integrato senza logo?
012000150223         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
012100150223     d   §CM1tips    e                     inz('COR')
012200150223         //?·§CM1po   = Filiale?
012300150223     d   §CM1po      e                     inz('046')
012400150223         //?·§CM1var  = Oggetto e-mail?
012500150223     d   §CM1var     e                     inz('*OBJM*+
012600150223     d                                     ERRORE in Pulizia dei -
012700150223     d                                     Clienti in Campagna')
012800150223         //?·§CM1sts  = Stato?
012900150223     d   §CM1sts     e                     inz(*off)
013000150223         //?·§CM1sts  = Id processo?
013100150223     d   §CM1idp     e                     inz('2')
013200150223
013300150223      //---------------------------------------------------------------
013400150223      //?Definizione variabili globali.
013500150223      //---------------------------------------------------------------
013600150223
013700150223       // -?Flags booleani?
013800150223     d $EoF            s               n   inz
013900150223
014000150223       // -?Stringa SQL da eseguire?
014100150223     d wSQL            s           7000    inz  varying
014200150223
014300150223       // -?Codice errore rilevato?
014400150223     d wErr            s              3  0 inz
014500150223
014600150223       // -?Data *ISO?
014700150223     d wDate_Eur       s               d   inz  datfmt(*EUR)
014800150223
014900150223       //--------------------------------------------------------------
015000150223       //?Definizione prototipi/procedure usate.                       ?
015100150223       //--------------------------------------------------------------
015200150223
015300150223       // -?Reperimento dati utente?
015400150223     d TIBS34ds      e ds                  inz
015500150223      /copy gaitrasrc/srcProtoPR,TIBS34R
015600150223
015700150223       // -?Modifica campagna x codice unificante?
015800150223     d TRKC75ds      e ds                  inz
015900150223      /copy gaitrasrc/srcProtoPR,TRKC75R
016000150223
016100150223       // -?Reperimento dati tabelle?
016200150223      /copy gaitrasrc/srcProtoPI,TRULTAB
016300150223      /copy gaitrasrc/srcProtoPR,TRULTAB
016400150223
016500150223       // -?Parametri API QCAPCMD (Process Commands)?
016600150223     d Qcmd            s           2048    inz  varying
016700150223      /copy qSysInc/qRpgleSrc,QCAPCMD
016800150223       // -?API QCAPCMD (Process Commands)?
016900150223      /copy gaitrasrc/srcProtoPR,QCAPCMD
017000150223
017100150223       // -?Parametri gestione errori API.?
017200150223      /copy qsysinc/qrpglesrc,QUSEC
017300150223
017400150223       //--------------------------------------------------------------
017500150223       //?Definizione key-list.                                        ?
017600150223       //--------------------------------------------------------------
017700150223
017800150223
017900150223       //--------------------------------------------------------------
018000150223       //?Riepilogo indicatori utilizzati.                             ?
018100150223       //--------------------------------------------------------------
018200150223
018300150223
018400150223       //--------------------------------------------------------------
018500150223       //?M A I N - L I N E                                            ?
018600150223       //--------------------------------------------------------------
018700150223
018800150223     c     *Entry        plist
018900150223     c                   parm                    KPJBA
019000150223
019100150223      /free
019200150223
019300150223       // -?Operazioni iniziali?
019400150223       exsr  sr_RoutInz;
019500150223
019600150223       // -?Preparazione SQL per estrazione dati?
019700150223       exsr  sr_PrepSQL;
019800150223
019900150223       // -?Ciclo di elaborazione?
020000150223       exsr  sr_OpenCursor;
020100150223
020200150223       DoW  Not $EoF;
020300150223         exsr  sr_ReadCursor;
020400150223       EndDo;
020500150223
020600150223       exsr  sr_CloseCursor;
020700150223
020800150223       // -?Operazioni finali?
020900150223       exsr  sr_RoutEnd;
021000150223
021100150223       //--------------------------------------------------------------
021200150223       //?Operazioni iniziali.                                         ?
021300150223       //--------------------------------------------------------------
021400150223       BEGSR  sr_RoutInz;
021500150223
021600150223         // -?Impostazione opzioni per SQL?
021700150223         exec sql   set  option  DynUsrPrf = *Owner,
021800150223                                 CloSqlCsr = *EndMod;
021900150223
022000150223         // -?Impostazione chiusura?
022100150223         *inLR = *on;
022200150223
022300150223         // -?Reperimento dati job?
022400150223         exsr  sr_DatiJob;
022500150223
022600150223         // -?Reperimento data e ora attuali?
022700150223         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
022800150223         wDate_Eur = %date( wDate : *Iso );
022900150223         wDate     = %dec( wDate_Eur );
023000150223
023100150223       ENDSR;
023200150223
023300150223       //--------------------------------------------------------------
023400150223       //?Reperimento Dati del job (Utente/Operativi).                 ?
023500150223       //--------------------------------------------------------------
023600150223       BEGSR  sr_DatiJob;
023700150223
023800150223         in(e) §AzUte;
023900150223         if NOT %error;
024000150223           in(e) §DatiUte;
024100150223         endif;
024200150223         if %error or RSut = *blank;
024300150223           tibs34r ( tibs34ds );
024400150223           in §AzUte;
024500150223           in §DatiUte;
024600150223         endif;
024700150223
024800150223       ENDSR;
024900150223
025000150223       //--------------------------------------------------------------
025100150223       //?Preparazione SQL.                                            ?
025200150223       //--------------------------------------------------------------
025300150223       BEGSR  sr_PrepSQL;
025400150223
025500150223         // -?Preparazione SQL?
025600150223         clear  wSQL;
025700150223         wSQL = 'with +
025800150223                    KSU_CMC as ( select CMCncm, CMCksu, CMCksc, CMCcpo +
025900150223                                   from TICMC00F +
026000150223                                  where CMCncm in ( 1, 2 ) +
026100150223                                    and CMCcch <> ''  '' +
026200150223                               order by CMCksu ), +
026300150223                    KSU_CMF as ( select CMFncm, CMFksu, CMFksc, CMFcpo +
026400150223                                   from TICMF00F iner join KSU_CMC +
026500150223                                     on CMFncm = CMCncm +
026600150223                                    and CMFksu = CMCksu +
026700150223                                    and CMFksc = CMCksc +
026800150223                                    and CMFcpo = CMCcpo +
026900150224                                  where CMFacm = ''' + c_FaseEsclus + ''' +
027000150223                                    and CMFpea = 0 +
027100150223                               order by CMFksu ) +
027200150223                 select * +
027300150223                   from KSU_CMF +
027400150224                  order by CMFncm, CMFksu, CMFksc, CMFcpo +
027500150223                    for fetch only';
027600150223
027700150223         exec sql   prepare S1   from :wSQL;
027800150223
027900150223         // -?Dichiarazione cursore?
028000150223         exec sql   declare C1   cursor for S1;
028100150223
028200150223         if  SQLcode < *zero;
028300150223           $EoF = *on;
028400150223           exsr  sr_PrintErrSQL;
028500150223         endif;
028600150223
028700150223       ENDSR;
028800150223
028900150223       //--------------------------------------------------------------
029000150223       //?Apertura cursore.                                            ?
029100150223       //--------------------------------------------------------------
029200150223       BEGSR  sr_OpenCursor;
029300150223
029400150223         // -?Apertura del cursore?
029500150223         exec sql   open C1;
029600150223
029700150223         if  SQLcode < *zero;
029800150223           $EoF = *on;
029900150223           exsr  sr_PrintErrSQL;
030000150223         endif;
030100150223
030200150223       ENDSR;
030300150223
030400150223       //--------------------------------------------------------------
030500150223       //?Lettura cursore.                                             ?
030600150223       //--------------------------------------------------------------
030700150223       BEGSR  sr_ReadCursor;
030800150223
030900150223         clear  wSQL_ds;
031000150223
031100150223         exec sql   fetch next   from C1   into :wSQL_ds;
031200150223
031300150223         Select;
031400150223
031500150223           // -?Fine File?
031600150223           When  SQLcode = 100;
031700150223             $EoF = *on;
031800150223
031900150223           // -?Errore SQL?
032000150223           When  SQLcode < *zero;
032100150223             $EoF = *on;
032200150223             exsr  sr_PrintErrSQL;
032300150223
032400150223           // -?Elaborazione singolo cliente?
032500150223           Other;
032600150223             exsr  sr_ElabRec;
032700150223
032800150223         EndSl;
032900150223
033000150223       ENDSR;
033100150223
033200150223       //--------------------------------------------------------------
033300150223       //?Elaborazione singolo Cliente da eliminare dalla Campagna.    ?
033400150223       //--------------------------------------------------------------
033500150223       BEGSR  sr_ElabRec;
033600150223
033700150223         // -?Modifica Campagna per Cliente Unificante?
033800150223         clear  TRKC75ds;
033900150223         iKC75ncm  = wSQL_ds.wNCM;
034000150223         iKC75ksu  = wSQL_ds.wKSU;
034100150223         iKC75ksc  = wSQL_ds.wKSC;
034200150223         iKC75cpo  = wSQL_ds.wCPO;
034300150223         iKC75nncm = c_Campagna_ClientiEsclusi;
034400150223
034500150223         TRKC75R ( kpjba : TRKC75ds );
034600150223
034700150223         if  oKC75err <> *blank;
034800150223           wErr = 2;
034900150223           exsr  sr_SendEmail;
035000150223         endif;
035100150223
035200150223       ENDSR;
035300150223
035400150223       //--------------------------------------------------------------
035500150223       //?Chiusura cursore.                                            ?
035600150223       //--------------------------------------------------------------
035700150223       BEGSR  sr_CloseCursor;
035800150223
035900150223         // -?Chiusura del cursore?
036000150223         exec sql   close C1;
036100150223
036200150223       ENDSR;
036300150223
036400150223       //--------------------------------------------------------------
036500150223       //?Stampa segnalazione dell'errore rilevato via SQL             ?
036600150223       //--------------------------------------------------------------
036700150223       BEGSR  sr_PrintErrSQL;
036800150223
036900150223         // -?Stampa del Dump?
037000150223         Dump(A);
037100150223
037200150223         // -?Stampa del Job-Log?
037300150223         Qcmd = 'DSPJOBLOG job(*) output(*print)';
037400150223         exsr  sr_ExecCmd;
037500150223
037600150223         // -?Invio e-mail con errore rilevato?
037700150223         wErr = 1;
037800150223         exsr  sr_SendEmail;
037900150223
038000150223         // -?Chiusura del programma?
038100150223         exsr  sr_RoutEnd;
038200150223
038300150223       ENDSR;
038400150223
038500150223       //--------------------------------------------------------------
038600150223       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
038700150223       //--------------------------------------------------------------
038800150223       BEGSR  sr_SendEmail;
038900150223
039000150223         // -?Override al file di stampa ed apertura dello stesso?
039100150223         If  NOT  %open(PrtEMAIL);
039200150223
039300150223           // -?Reperimento regole per mailing da tab. "MRA"?
039400150223           clear  dMRAdan;
039500150223           if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
039600150223                            *omit : *omit :
039700150223                            *omit : *omit :
039800150223                            *omit : *omit : *omit : *omit :
039900150223                            *omit : *omit : *omit : *omit :
040000150223                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
040100150223                            = *zero      AND
040200150223                 ds_TNTBE.TBEatb = *blank;
040300150223             dMRAdan = ds_TNTBE.TBEuni;
040400150223           endif;
040500150223
040600150223           // -?Impostazione regole per mailing?
040700150223           §CM1mitt = %trim(§MRADmitt);
040800150223           §CM1dst  = %trim(§MRADdest);
040900150223           §CM1tips = §MRAdreg;
041000150223           §CM1var  = '*OBJM*' + §MRADdes;
041100150223           §CM1idp  = §MRADidPro;
041200150223
041300150223           // -?Esecuzione override al file di stampa (per mailing)?
041400150223           Qcmd = c_CmdOvrPrtF
041500150223                + ' outq(' + %trim(§MRADoutqI) + ')'
041600150223                + ' usrdfndta(''' + TRTCM1ds + ''')';
041700150223           exsr  sr_ExecCmd;
041800150223
041900150223           // -?Apertura file di stampa?
042000150223           open  PrtEMAIL;
042100150223           *inOF = *on;
042200150223
042300150223         EndIf;
042400150223
042500150223         // -?Preparazione e-Mail per il singolo errore rilevato?
042600150223         if  *inOF;
042700150223           except  PRTtxt;
042800150223           *inOF = *off;
042900150223         endif;
043000150223
043100150223         Select;
043200150223           When  wErr = 1;
043300150223             except  PRTerr01;
043400150223           When  wErr = 2;
043500150223             except  PRTerr02;
043600150223         EndSl;
043700150223
043800150223       ENDSR;
043900150223
044000150223       //--------------------------------------------------------------
044100150223       //?Esecuzione del comando (già impostato).                      ?
044200150223       //--------------------------------------------------------------
044300150223       BEGSR  sr_ExecCmd;
044400150223
044500150223         clear Qcap0100;
044600150223         Qcabcsdh = *off;
044700150223         Qcapa    = *off;
044800150223         Qcacmdss = *off;
044900150223         Qcaerved = *allX'00';
045000150223
045100150223         clear Qusec;
045200150223         Qusbprv  = %size(Qusec);
045300150223
045400150223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
045500150223                           %size(Qcap0100) : 'CPOP0100' : *omit :
045600150223                           0 : 0 : Qusec);
045700150223
045800150223         //if  Qusei <> *blank;
045900150223         //  ...;
046000150223         //endif;
046100150223
046200150223       ENDSR;
046300150223
046400150223       //--------------------------------------------------------------
046500150223       //?Operazioni finali.                                           ?
046600150223       //--------------------------------------------------------------
046700150223       BEGSR  sr_RoutEnd;
046800150223
046900150223         // -?Chiusura funzioni precedentemente aperte?
047000150223         cloTabella();
047100150223
047200150223         // -?SE stampato almeno un Errore:?
047300150223         //  ?stampa "fine" e chiude il file di stampa?
047400150223         if  %open(PrtEMAIL);
047500150223           except  PRTend;
047600150223           close   PrtEMAIL;
047700150223         endif;
047800150223
047900150223         // -?Uscita dal *pgm?
048000150223         return;
048100150223
048200150223       ENDSR;
048300150223
048400150223      /end-free
048500150223
048600150223       //--------------------------------------------------------------
048700150223       //?S P O O L - F I L E S                                        ?
048800150223       //--------------------------------------------------------------
048900150223
049000150223     oPrtEMAIL  e            PRTtxt            1
049100150223     o                       RSUT
049200150223     o                                        +   5 'PULIZIA DEI CLIENTI IN +
049300150223     o                                               CAMPAGNA'
049400150223     o                       SDSpgm           +   5
049500150223     o                       wDate         y  +   5
049600150223     o          e            PRTtxt      1
049700150223     o                       KNSIF
049800150223     o                       KNMUS            +   1
049900150223     o                                        +   4 '-----------------------+
050000150223     o                                               --------'
050100150223     o                                        +   5 'Pag.'
050200150223     o                       Page          z  +   0
050300150224     o                       wTime            +   7 '  :  :  '
050400150223      *
050500150223     o          e            PRTerr01    2
050600150223     o                                              'Rilevato ERRORE n-
050700150223     o                                              ella pulizia dei c-
050800150223     o                                              lienti in Campagna-
050900150223     o                                              .'
051000150223     o          e            PRTerr01    2
051100150223     o                                              'Consultare le sta-
051200150223     o                                              mpe del lavoro'
051300150223     o
051400150223     o                       SDSjobNbr     3  +   1
051500150223     o                                        +   0 '/'
051600150223     o                       SDSjobUser       +   0
051700150223     o                                        +   0 '/'
051800150223     o                       SDSjobName       +   0
051900150223      *
052000150223     o          e            PRTerr02    2
052100150223     o                                              'Campagna'
052200150223     o                       wSQL_ds.wNCM  z  +   1
052300150223     o                                        +   2 'Unificante'
052400150223     o                       wSQL_ds.wKSU     +   1
052500150223     o                                        +   1 '-'
052600150223     o                       oKC75msg         +   1
052700150223      *
052800150223     o          e            PRTend      2
052900150223     o                                        +  10 '***  Fine Lista  ***'
