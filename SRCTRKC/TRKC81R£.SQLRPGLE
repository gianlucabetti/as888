000100150223       //==============================================================
000200150303       //?TRKC80R1 * Pulizia dei Clienti in Campagna.                   ?
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
002600150224     fTICMC01L  if   e           k disk     usropn
002700150223
002800150223       // -?File di stampa per eventuale invio e-mail?
002900150223     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003000150224
003100150224       // -?File di stampa (SE non richiesto l'aggiornamento)?
003200150224     fQSYSPRT   o    f  132        printer  usropn  oflind(*inOA)
003300150223
003400150223       //--------------------------------------------------------------
003500150223       //?Definizione costanti.                                        ?
003600150223       //--------------------------------------------------------------
003700150226       // -?Fase di Chiusura dalla Campagna comm.le per il Cliente?
003800150226     d c_FaseChiusura  c                   const(' 30')
003900150223
004000150223       // -?Comandi da eseguire?
004100150223     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004200150223     d                                           file(PRTEMAIL) +
004300150223     d                                           pagesize(66 132) +
004400150223     d                                           lpi(6) cpi(10) +
004500150223     d                                           ovrscope(*actgrpdfn) +
004600150223     d                                           usrdta(''ERR-TRKC80'') +
004700150223     d                                           ')
004800150223     d c_CmdDltOvr     c                   const('DLTOVR +
004900150223     d                                            file(PRTEMAIL) +
005000150223     d                                            lvl(*actgrpdfn)')
005100150223
005200150223       //--------------------------------------------------------------
005300150223       //?Definizione schiere.                                         ?
005400150223       //--------------------------------------------------------------
005500150223
005600150223
005700150223       //--------------------------------------------------------------
005800150223       //?Definizione aree dati.                                       ?
005900150223       //--------------------------------------------------------------
006000150223
006100150223       // -?Dati utente?
006200150223     d §AzUte        e ds                  extname(AZUTE00F)
006300150223     d                                     dtaara
006400150223     d §DatiUte      e ds                  extname(dDatiUte)
006500150223     d                                     dtaara
006600150223
006700150223       //--------------------------------------------------------------
006800150223       //?Definizione strutture dati.                                  ?
006900150223       //--------------------------------------------------------------
007000150223
007100150223       // -?Status?
007200150223     d Status         sds
007300150223     d   SDSpgm          *proc
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
013900150224     d $SoloStampa     s               n   inz
014000150223
014100150223       // -?Stringa SQL da eseguire?
014200150223     d wSQL            s           7000    inz  varying
014300150223
014400150223       // -?Codice errore rilevato?
014500150223     d wErr            s              3  0 inz
014600150223
014700150223       // -?Data *ISO?
014800150223     d wDate_Eur       s               d   inz  datfmt(*EUR)
014900150223
015000150223       //--------------------------------------------------------------
015100150223       //?Definizione prototipi/procedure usate.                       ?
015200150223       //--------------------------------------------------------------
015300150223
015400150223       // -?Reperimento dati utente?
015500150223     d TIBS34ds      e ds                  inz
015600150223      /copy gaitrasrc/srcProtoPR,TIBS34R
015700150223
015800150303       // -?Deleta i clienti in campagna
015900150303     d TRKC77ds      e ds                  inz
016000150303      /copy gaitrasrc/srcProtoPR,TRKC77R
016100150223
016200150223       // -?Reperimento dati tabelle?
016300150223      /copy gaitrasrc/srcProtoPI,TRULTAB
016400150223      /copy gaitrasrc/srcProtoPR,TRULTAB
016500150223
016600150223       // -?Parametri API QCAPCMD (Process Commands)?
016700150223     d Qcmd            s           2048    inz  varying
016800150223      /copy qSysInc/qRpgleSrc,QCAPCMD
016900150223       // -?API QCAPCMD (Process Commands)?
017000150223      /copy gaitrasrc/srcProtoPR,QCAPCMD
017100150223
017200150223       // -?Parametri gestione errori API.?
017300150223      /copy qsysinc/qrpglesrc,QUSEC
017400150223
017500150223       //--------------------------------------------------------------
017600150223       //?Definizione key-list.                                        ?
017700150223       //--------------------------------------------------------------
017800150223
017900150224       // -?File TICMC01L: Anagrafica Clienti in Campagna?
018000150224     d keyTICMC01    e ds                  extname( TICMC01L : *key )
018100150224     d                                     prefix( k_ )   inz
018200150223
018300150223       //--------------------------------------------------------------
018400150223       //?Riepilogo indicatori utilizzati.                             ?
018500150223       //--------------------------------------------------------------
018600150223
018700150223
018800150223       //--------------------------------------------------------------
018900150223       //?M A I N - L I N E                                            ?
019000150223       //--------------------------------------------------------------
019100150223
019200150223     c     *Entry        plist
019300150223     c                   parm                    KPJBA
019400150223
019500150223      /free
019600150223
019700150223       // -?Operazioni iniziali?
019800150223       exsr  sr_RoutInz;
019900150223
020000150223       // -?Preparazione SQL per estrazione dati?
020100150223       exsr  sr_PrepSQL;
020200150223
020300150223       // -?Ciclo di elaborazione?
020400150223       exsr  sr_OpenCursor;
020500150223
020600150223       DoW  Not $EoF;
020700150223         exsr  sr_ReadCursor;
020800150223       EndDo;
020900150223
021000150223       exsr  sr_CloseCursor;
021100150223
021200150223       // -?Operazioni finali?
021300150223       exsr  sr_RoutEnd;
021400150223
021500150223       //--------------------------------------------------------------
021600150223       //?Operazioni iniziali.                                         ?
021700150223       //--------------------------------------------------------------
021800150223       BEGSR  sr_RoutInz;
021900150223
022000150223         // -?Impostazione opzioni per SQL?
022100150223         exec sql   set  option  DynUsrPrf = *Owner,
022200150223                                 CloSqlCsr = *EndMod;
022300150223
022400150223         // -?Impostazione chiusura?
022500150223         *inLR = *on;
022600150223
022700150223         // -?Reperimento dati job?
022800150223         exsr  sr_DatiJob;
022900150223
023000150223         // -?Reperimento data e ora attuali?
023100150223         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
023200150223         wDate_Eur = %date( wDate : *Iso );
023300150223         wDate     = %dec( wDate_Eur );
023400150224
023500150224         // -?Verifica SE richiesta solo la stampa?
023600150224         $SoloStampa = ( %subst( kpjbu : 1 : 10 ) = 'STAMPA    ' );
023700150224         if  $SoloStampa;
023800150224           open  TICMC01L;
023900150224           open  QSYSPRT;
024000150224           except  STAMPAtxt;
024100150224           *inOA = *off;
024200150224         endif;
024300150223
024400150223       ENDSR;
024500150223
024600150223       //--------------------------------------------------------------
024700150223       //?Reperimento Dati del job (Utente/Operativi).                 ?
024800150223       //--------------------------------------------------------------
024900150223       BEGSR  sr_DatiJob;
025000150223
025100150223         in(e) §AzUte;
025200150223         if NOT %error;
025300150223           in(e) §DatiUte;
025400150223         endif;
025500150223         if %error or RSut = *blank;
025600150223           tibs34r ( tibs34ds );
025700150223           in §AzUte;
025800150223           in §DatiUte;
025900150223         endif;
026000150223
026100150223       ENDSR;
026200150223
026300150223       //--------------------------------------------------------------
026400150223       //?Preparazione SQL.                                            ?
026500150223       //--------------------------------------------------------------
026600150223       BEGSR  sr_PrepSQL;
026700150223
026800150303         // -?Preparazione SQL?
026900150223         clear  wSQL;
027000150223         wSQL = 'with +
027100150226                    KSU_CMC as ( select CMCncm as NCM, CMCksu as KSU, +
027200150226                                        CMCksc as KSC, CMCcpo as CPO, +
027300150224                                        CMCufe, CMCcch +
027400150223                                   from TICMC00F +
027500150223                                  where CMCncm in ( 1, 2 ) +
027600150223                                    and CMCcch <> ''  '' +
027700150226                                  order by CMCksu ), +
027800150226                    KSU_CMF as ( select distinct +
027900150226                                        CMFncm as NCM, CMFksu as KSU, +
028000150226                                        CMFksc as KSC, CMFcpo as CPO, +
028100150226                                        CMFacm, CMFcch +
028200150226                                   from TICMF00F inner join TICMC00F +
028300150226                                     on CMFncm = CMCncm +
028400150226                                    and CMFksu = CMCksu +
028500150226                                    and CMFksc = CMCksc +
028600150226                                    and CMFcpo = CMCcpo +
028700150226                                    and CMFacm = CMCufe +
028800150226                                  where CMFncm in ( 1, 2 ) +
028900150226                                    and CMCcch = '' '' +
029000150226                                    and CMFacm = ''' + c_FaseChiusura +
029100150226                                ''' and CMFpea = 0 +
029200150226                                  order by CMFksu ) +
029300150226                 select * from KSU_CMC +
029400150226                  union +
029500150226                 select * from KSU_CMF +
029600150226                  order by NCM, KSU, KSC, CPO +
029700150223                    for fetch only';
029800150223
029900150223         exec sql   prepare S1   from :wSQL;
030000150223
030100150223         // -?Dichiarazione cursore?
030200150223         exec sql   declare C1   cursor for S1;
030300150223
030400150223         if  SQLcode < *zero;
030500150223           $EoF = *on;
030600150223           exsr  sr_PrintErrSQL;
030700150223         endif;
030800150223
030900150223       ENDSR;
031000150223
031100150223       //--------------------------------------------------------------
031200150223       //?Apertura cursore.                                            ?
031300150223       //--------------------------------------------------------------
031400150223       BEGSR  sr_OpenCursor;
031500150223
031600150223         // -?Apertura del cursore?
031700150223         exec sql   open C1;
031800150223
031900150223         if  SQLcode < *zero;
032000150223           $EoF = *on;
032100150223           exsr  sr_PrintErrSQL;
032200150223         endif;
032300150223
032400150223       ENDSR;
032500150223
032600150223       //--------------------------------------------------------------
032700150223       //?Lettura cursore.                                             ?
032800150223       //--------------------------------------------------------------
032900150223       BEGSR  sr_ReadCursor;
033000150223
033100150223         clear  wSQL_ds;
033200150223
033300150223         exec sql   fetch next   from C1   into :wSQL_ds;
033400150223
033500150223         Select;
033600150223
033700150223           // -?Fine File?
033800150223           When  SQLcode = 100;
033900150223             $EoF = *on;
034000150223
034100150223           // -?Errore SQL?
034200150223           When  SQLcode < *zero;
034300150223             $EoF = *on;
034400150223             exsr  sr_PrintErrSQL;
034500150223
034600150224           // -?Richiesta SOLO stampa?
034700150224           When  $SoloStampa;
034800150224             exsr  sr_Stampa;
034900150224
035000150223           // -?Elaborazione singolo cliente?
035100150223           Other;
035200150223             exsr  sr_ElabRec;
035300150223
035400150223         EndSl;
035500150223
035600150223       ENDSR;
035700150224
035800150224       //--------------------------------------------------------------
035900150224       //?Stampa del singolo Cliente eliminabile della Campagna.       ?
036000150224       //--------------------------------------------------------------
036100150224       BEGSR  sr_Stampa;
036200150224
036300150224         clear  keyTICMC01;
036400150224         k_CMCncm = wSQL_ds.wNCM;
036500150224         k_CMCksu = wSQL_ds.wKSU;
036600150224         k_CMCksc = wSQL_ds.wKSC;
036700150224         k_CMCcpo = wSQL_ds.wCPO;
036800150224         chain  %kds( keyTICMC01 )  TICMC000;
036900150224         if  Not %found(TICMC01L);
037000150224           clear  CMCufe;
037100150224           clear  CMCcch;
037200150224           clear  CMCdch;
037300150224         endif;
037400150223
037500150224         if  *inOA;
037600150224           except  STAMPAtxt;
037700150224           *inOA = *off;
037800150224         endif;
037900150224
038000150224         except STAMPAdet;
038100150224
038200150224       ENDSR;
038300150223       //--------------------------------------------------------------
038400150223       //?Elaborazione singolo Cliente da eliminare dalla Campagna.    ?
038500150223       //--------------------------------------------------------------
038600150223       BEGSR  sr_ElabRec;
038700150223
038800150303         // -?Cancello il Cliente dalla Campagna?
038900150303         clear  TRKC77ds;
039000150303         iKC77ncm  = wSQL_ds.wNCM;
039100150303         iKC77ksu  = wSQL_ds.wKSU;
039200150303         iKC77ksc  = wSQL_ds.wKSC;
039300150303         iKC77cpo  = wSQL_ds.wCPO;
039400150223
039500150303         TRKC77R ( kpjba : TRKC77ds );
039600150223
039700150303         if  oKC77err <> *blank;
039800150223           wErr = 2;
039900150223           exsr  sr_SendEmail;
040000150223         endif;
040100150223
040200150223       ENDSR;
040300150223
040400150223       //--------------------------------------------------------------
040500150223       //?Chiusura cursore.                                            ?
040600150223       //--------------------------------------------------------------
040700150223       BEGSR  sr_CloseCursor;
040800150223
040900150223         // -?Chiusura del cursore?
041000150223         exec sql   close C1;
041100150223
041200150223       ENDSR;
041300150223
041400150223       //--------------------------------------------------------------
041500150223       //?Stampa segnalazione dell'errore rilevato via SQL             ?
041600150223       //--------------------------------------------------------------
041700150223       BEGSR  sr_PrintErrSQL;
041800150223
041900150223         // -?Stampa del Dump?
042000150223         Dump(A);
042100150223
042200150223         // -?Stampa del Job-Log?
042300150223         Qcmd = 'DSPJOBLOG job(*) output(*print)';
042400150223         exsr  sr_ExecCmd;
042500150223
042600150223         // -?Invio e-mail con errore rilevato?
042700150223         wErr = 1;
042800150223         exsr  sr_SendEmail;
042900150223
043000150223         // -?Chiusura del programma?
043100150223         exsr  sr_RoutEnd;
043200150223
043300150223       ENDSR;
043400150223
043500150223       //--------------------------------------------------------------
043600150223       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
043700150223       //--------------------------------------------------------------
043800150223       BEGSR  sr_SendEmail;
043900150223
044000150223         // -?Override al file di stampa ed apertura dello stesso?
044100150223         If  NOT  %open(PrtEMAIL);
044200150223
044300150223           // -?Reperimento regole per mailing da tab. "MRA"?
044400150223           clear  dMRAdan;
044500150223           if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
044600150223                            *omit : *omit :
044700150223                            *omit : *omit :
044800150223                            *omit : *omit : *omit : *omit :
044900150223                            *omit : *omit : *omit : *omit :
045000150223                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
045100150223                            = *zero      AND
045200150223                 ds_TNTBE.TBEatb = *blank;
045300150223             dMRAdan = ds_TNTBE.TBEuni;
045400150223           endif;
045500150223
045600150223           // -?Impostazione regole per mailing?
045700150223           §CM1mitt = %trim(§MRADmitt);
045800150223           §CM1dst  = %trim(§MRADdest);
045900150223           §CM1tips = §MRAdreg;
046000150223           §CM1var  = '*OBJM*' + §MRADdes;
046100150223           §CM1idp  = §MRADidPro;
046200150223
046300150223           // -?Esecuzione override al file di stampa (per mailing)?
046400150223           Qcmd = c_CmdOvrPrtF
046500150223                + ' outq(' + %trim(§MRADoutqI) + ')'
046600150223                + ' usrdfndta(''' + TRTCM1ds + ''')';
046700150223           exsr  sr_ExecCmd;
046800150223
046900150223           // -?Apertura file di stampa?
047000150223           open  PrtEMAIL;
047100150223           *inOF = *on;
047200150223
047300150223         EndIf;
047400150223
047500150223         // -?Preparazione e-Mail per il singolo errore rilevato?
047600150223         if  *inOF;
047700150223           except  PRTtxt;
047800150223           *inOF = *off;
047900150223         endif;
048000150223
048100150223         Select;
048200150223           When  wErr = 1;
048300150223             except  PRTerr01;
048400150223           When  wErr = 2;
048500150223             except  PRTerr02;
048600150223         EndSl;
048700150223
048800150223       ENDSR;
048900150223
049000150223       //--------------------------------------------------------------
049100150223       //?Esecuzione del comando (già impostato).                      ?
049200150223       //--------------------------------------------------------------
049300150223       BEGSR  sr_ExecCmd;
049400150223
049500150223         clear Qcap0100;
049600150223         Qcabcsdh = *off;
049700150223         Qcapa    = *off;
049800150223         Qcacmdss = *off;
049900150223         Qcaerved = *allX'00';
050000150223
050100150223         clear Qusec;
050200150223         Qusbprv  = %size(Qusec);
050300150223
050400150223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
050500150223                           %size(Qcap0100) : 'CPOP0100' : *omit :
050600150223                           0 : 0 : Qusec);
050700150223
050800150223         //if  Qusei <> *blank;
050900150223         //  ...;
051000150223         //endif;
051100150223
051200150223       ENDSR;
051300150223
051400150223       //--------------------------------------------------------------
051500150223       //?Operazioni finali.                                           ?
051600150223       //--------------------------------------------------------------
051700150223       BEGSR  sr_RoutEnd;
051800150223
051900150223         // -?Chiusura funzioni precedentemente aperte?
052000150223         cloTabella();
052100150223
052200150223         // -?SE stampato almeno un Errore:?
052300150223         //  ?stampa "fine" e chiude il file di stampa?
052400150223         if  %open(PrtEMAIL);
052500150223           except  PRTend;
052600150223           close   PrtEMAIL;
052700150223         endif;
052800150224
052900150224         // -?SE richiesta SOLO stampa:?
053000150224         //  ?stampa "fine" e chiude il file di stampa?
053100150224         if  $SoloStampa  and  %open(QSYSPRT);
053200150224           except  STAMPAend;
053300150224           close   QSYSPRT;
053400150224         endif;
053500150223
053600150223         // -?Uscita dal *pgm?
053700150223         return;
053800150223
053900150223       ENDSR;
054000150223
054100150223      /end-free
054200150223
054300150223       //--------------------------------------------------------------
054400150223       //?S P O O L - F I L E S                                        ?
054500150223       //--------------------------------------------------------------
054600150223
054700150223     oPrtEMAIL  e            PRTtxt            1
054800150223     o                       RSUT
054900150223     o                                        +   5 'PULIZIA DEI CLIENTI IN +
055000150223     o                                               CAMPAGNA'
055100150223     o                       SDSpgm           +   5
055200150223     o                       wDate         y  +   5
055300150223     o          e            PRTtxt      1
055400150223     o                       KNSIF
055500150223     o                       KNMUS            +   1
055600150223     o                                        +   4 '-----------------------+
055700150223     o                                               --------'
055800150223     o                                        +   5 'Pag.'
055900150223     o                       Page          z  +   0
056000150224     o                       wTime            +   7 '  :  :  '
056100150223      *
056200150223     o          e            PRTerr01    2
056300150223     o                                              'Rilevato ERRORE n-
056400150223     o                                              ella pulizia dei c-
056500150223     o                                              lienti in Campagna-
056600150223     o                                              .'
056700150223     o          e            PRTerr01    2
056800150223     o                                              'Consultare le sta-
056900150223     o                                              mpe del lavoro'
057000150223     o
057100150223     o                       SDSjobNbr     3  +   1
057200150223     o                                        +   0 '/'
057300150223     o                       SDSjobUser       +   0
057400150223     o                                        +   0 '/'
057500150223     o                       SDSjobName       +   0
057600150223      *
057700150223     o          e            PRTerr02    2
057800150223     o                                              'Campagna'
057900150223     o                       wSQL_ds.wNCM  z  +   1
058000150223     o                                        +   2 'Unificante'
058100150223     o                       wSQL_ds.wKSU     +   1
058200150223     o                                        +   1 '-'
058300150303     o                       oKC77msg         +   1
058400150223      *
058500150223     o          e            PRTend      2
058600150223     o                                        +  10 '***  Fine Lista  ***'
058700150224
058800150224       //--------------------------------------------------------------
058900150224
059000150224     oQSYSPRT   e            STAMPAtxt         1
059100150224     o                       RSUT
059200150224     o                                        +   5 'PULIZIA DEI CLIENTI IN +
059300150224     o                                               CAMPAGNA'
059400150224     o                       SDSpgm           +   5
059500150224     o                       wDate         y  +   5
059600150224     o          e            STAMPAtxt   1
059700150224     o                       KNSIF
059800150224     o                       KNMUS            +   1
059900150224     o                                        +   4 '-----------------------+
060000150224     o                                               --------'
060100150224     o                                        +   5 'Pag.'
060200150224     o                       Page2         z  +   0
060300150224     o                       wTime            +   7 '  :  :  '
060400150224     o          e            STAMPAtxt   2
060500150224     o                                              'Campagna'
060600150224     o                                        +   3 'Unific.'
060700150224     o                                        +   3 'Cliente'
060800150224     o                                        +   3 ' Potenziale'
060900150224     o                                        +   3 'Fase'
061000150224     o                                        +   3 'C.Ch'
061100150224     o                                        +   3 'Data Chius'
061200150224     o          e            STAMPAtxt   1  1
061300150224     o                                              '--------'
061400150224     o                                        +   3 '-------'
061500150224     o                                        +   3 '-------'
061600150224     o                                        +   3 '-----------'
061700150224     o                                        +   3 '----'
061800150224     o                                        +   3 '----'
061900150224     o                                        +   3 '----------'
062000150224      *
062100150224Campag*a   Unific.   Cliente    Potenziale   Fase   C.Ch   Data (CMC)
062200150224123456*    1234567   1234567   12345678901   123     12    1234/56/78
062300150224     o          e            STAMPAdet   1
062400150224     o                       k_CMCncm      z
062500150224     o                       k_CMCksu         +   4
062600150224     o                       k_CMCksc         +   3
062700150224     o                       k_CMCcpo         +   3
062800150224     o                       CMCufe           +   3
062900150224     o                       CMCcch           +   5
063000150224     o                       CMCdch           +   4 '    /  /  '
063100150224      *
063200150224     o          e            STAMPAend   2
063300150224     o                                        +  25 '***  Fine Lista  ***'
