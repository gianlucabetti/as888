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
003700150223
003800150223       // -?Numero Campagna per l'esclusione dei Clienti?
003900150223     d c_Campagna_ClientiEsclusi...
004000150223     d                 c                   const(5001)
004100150226      *// // -?Fase di Esclusione dalla Campagna comm.le per il Cliente?
004200150226     d*// c_FaseEsclus    c                   const(' 90')
004300150226       // -?Fase di Chiusura dalla Campagna comm.le per il Cliente?
004400150226     d c_FaseChiusura  c                   const(' 30')
004500150223
004600150223       // -?Comandi da eseguire?
004700150223     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004800150223     d                                           file(PRTEMAIL) +
004900150223     d                                           pagesize(66 132) +
005000150223     d                                           lpi(6) cpi(10) +
005100150223     d                                           ovrscope(*actgrpdfn) +
005200150223     d                                           usrdta(''ERR-TRKC80'') +
005300150223     d                                           ')
005400150223     d c_CmdDltOvr     c                   const('DLTOVR +
005500150223     d                                            file(PRTEMAIL) +
005600150223     d                                            lvl(*actgrpdfn)')
005700150223
005800150223       //--------------------------------------------------------------
005900150223       //?Definizione schiere.                                         ?
006000150223       //--------------------------------------------------------------
006100150223
006200150223
006300150223       //--------------------------------------------------------------
006400150223       //?Definizione aree dati.                                       ?
006500150223       //--------------------------------------------------------------
006600150223
006700150223       // -?Dati utente?
006800150223     d §AzUte        e ds                  extname(AZUTE00F)
006900150223     d                                     dtaara
007000150223     d §DatiUte      e ds                  extname(dDatiUte)
007100150223     d                                     dtaara
007200150223
007300150223       //--------------------------------------------------------------
007400150223       //?Definizione strutture dati.                                  ?
007500150223       //--------------------------------------------------------------
007600150223
007700150223       // -?Status?
007800150223     d Status         sds
007900150223     d   SDSpgm          *proc
008000150223     d*//SDSprm          *parms
008100150223     d*//SDSdta              191    198
008200150223     d   SDSjobName          244    253
008300150223     d   SDSjobUser          254    263
008400150223     d   SDSjobNbr           264    269s 0
008500150223
008600150223       // -?Dati estratti via SQL?
008700150223     d TICMF00F      e ds                  extname(TICMF00F)
008800150223     d                                     based(nullPtr)
008900150223     d wSQL_ds         ds                  qualified     inz
009000150223     d   wNCM                              like(CMFncm)  inz
009100150223     d   wKSU                              like(CMFksu)  inz
009200150223     d   wKSC                              like(CMFksc)  inz
009300150223     d   wCPO                              like(CMFcpo)  inz
009400150223
009500150223       // -?Data/Ora attuali?
009600150223     d wTime_ds        ds                  inz
009700150223     d   wDate                        8s 0 inz
009800150223     d   wTime                        6s 0 inz
009900150223
010000150223       // -?Parametri ricevuti?
010100150223     d KPJBA         e ds
010200150223     d TRKC06ds      e ds
010300150223
010400150223       // -?Tab. "MRA" = Mailing - regole x invio da gestionale?
010500150223     d dMRAdan       e ds                  inz
010600150223     d   §MRADdes    e                     inz('ERR Puliz.Cli.in+
010700150223     d                                         Campagna')
010800150223     d   §MRADreg    e                     inz('COR')
010900150223     d   §MRADmitt   e                     inz('ced')
011000150223     d   §MRADdest   e                     inz('ced@brt.it; +
011100150223     d                                          stefano.merighi+
011200150223     d                                          @brt.it')
011300150223     d   §MRADidPro  e                     inz('2')
011400150223     d   §MRADoutqI  e                     inz('EMAILIN')
011500150223
011600150223       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
011700150223     d TRTCM1ds      e ds                  inz
011800150223         //?·§CM1mitt = Indirizzo e-mail del mittente?
011900150223     d   §CM1mitt    e                     inz('ced')
012000150223         //?·§CM1dst  = Indirizzo e-mail del destinatario?
012100150223     d   §CM1dst     e                     inz('ced@brt.it;+
012200150223     d                                          stefano.merighi+
012300150223     d                                          @brt.it')
012400150223         //?·§CM1tips = Tipo lettera via e-mail:?
012500150223         // ?"LET" = testo allegato in corpo con logo?
012600150223         //         ?(richiede righe libere iniziali per il logo)?
012700150223         // ?"COR" = testo integrato senza logo?
012800150223         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
012900150223     d   §CM1tips    e                     inz('COR')
013000150223         //?·§CM1po   = Filiale?
013100150223     d   §CM1po      e                     inz('046')
013200150223         //?·§CM1var  = Oggetto e-mail?
013300150223     d   §CM1var     e                     inz('*OBJM*+
013400150223     d                                     ERRORE in Pulizia dei -
013500150223     d                                     Clienti in Campagna')
013600150223         //?·§CM1sts  = Stato?
013700150223     d   §CM1sts     e                     inz(*off)
013800150223         //?·§CM1sts  = Id processo?
013900150223     d   §CM1idp     e                     inz('2')
014000150223
014100150223      //---------------------------------------------------------------
014200150223      //?Definizione variabili globali.
014300150223      //---------------------------------------------------------------
014400150223
014500150223       // -?Flags booleani?
014600150223     d $EoF            s               n   inz
014700150224     d $SoloStampa     s               n   inz
014800150223
014900150223       // -?Stringa SQL da eseguire?
015000150223     d wSQL            s           7000    inz  varying
015100150223
015200150223       // -?Codice errore rilevato?
015300150223     d wErr            s              3  0 inz
015400150223
015500150223       // -?Data *ISO?
015600150223     d wDate_Eur       s               d   inz  datfmt(*EUR)
015700150223
015800150223       //--------------------------------------------------------------
015900150223       //?Definizione prototipi/procedure usate.                       ?
016000150223       //--------------------------------------------------------------
016100150223
016200150223       // -?Reperimento dati utente?
016300150223     d TIBS34ds      e ds                  inz
016400150223      /copy gaitrasrc/srcProtoPR,TIBS34R
016500150223
016600150223       // -?Modifica campagna x codice unificante?
016700150223     d TRKC75ds      e ds                  inz
016800150223      /copy gaitrasrc/srcProtoPR,TRKC75R
016900150223
017000150223       // -?Reperimento dati tabelle?
017100150223      /copy gaitrasrc/srcProtoPI,TRULTAB
017200150223      /copy gaitrasrc/srcProtoPR,TRULTAB
017300150223
017400150223       // -?Parametri API QCAPCMD (Process Commands)?
017500150223     d Qcmd            s           2048    inz  varying
017600150223      /copy qSysInc/qRpgleSrc,QCAPCMD
017700150223       // -?API QCAPCMD (Process Commands)?
017800150223      /copy gaitrasrc/srcProtoPR,QCAPCMD
017900150223
018000150223       // -?Parametri gestione errori API.?
018100150223      /copy qsysinc/qrpglesrc,QUSEC
018200150223
018300150223       //--------------------------------------------------------------
018400150223       //?Definizione key-list.                                        ?
018500150223       //--------------------------------------------------------------
018600150223
018700150224       // -?File TICMC01L: Anagrafica Clienti in Campagna?
018800150224     d keyTICMC01    e ds                  extname( TICMC01L : *key )
018900150224     d                                     prefix( k_ )   inz
019000150223
019100150223       //--------------------------------------------------------------
019200150223       //?Riepilogo indicatori utilizzati.                             ?
019300150223       //--------------------------------------------------------------
019400150223
019500150223
019600150223       //--------------------------------------------------------------
019700150223       //?M A I N - L I N E                                            ?
019800150223       //--------------------------------------------------------------
019900150223
020000150223     c     *Entry        plist
020100150223     c                   parm                    KPJBA
020200150223
020300150223      /free
020400150223
020500150223       // -?Operazioni iniziali?
020600150223       exsr  sr_RoutInz;
020700150223
020800150223       // -?Preparazione SQL per estrazione dati?
020900150223       exsr  sr_PrepSQL;
021000150223
021100150223       // -?Ciclo di elaborazione?
021200150223       exsr  sr_OpenCursor;
021300150223
021400150223       DoW  Not $EoF;
021500150223         exsr  sr_ReadCursor;
021600150223       EndDo;
021700150223
021800150223       exsr  sr_CloseCursor;
021900150223
022000150223       // -?Operazioni finali?
022100150223       exsr  sr_RoutEnd;
022200150223
022300150223       //--------------------------------------------------------------
022400150223       //?Operazioni iniziali.                                         ?
022500150223       //--------------------------------------------------------------
022600150223       BEGSR  sr_RoutInz;
022700150223
022800150223         // -?Impostazione opzioni per SQL?
022900150223         exec sql   set  option  DynUsrPrf = *Owner,
023000150223                                 CloSqlCsr = *EndMod;
023100150223
023200150223         // -?Impostazione chiusura?
023300150223         *inLR = *on;
023400150223
023500150223         // -?Reperimento dati job?
023600150223         exsr  sr_DatiJob;
023700150223
023800150223         // -?Reperimento data e ora attuali?
023900150223         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
024000150223         wDate_Eur = %date( wDate : *Iso );
024100150223         wDate     = %dec( wDate_Eur );
024200150224
024300150224         // -?Verifica SE richiesta solo la stampa?
024400150224         $SoloStampa = ( %subst( kpjbu : 1 : 10 ) = 'STAMPA    ' );
024500150224         if  $SoloStampa;
024600150224           open  TICMC01L;
024700150224           open  QSYSPRT;
024800150224           except  STAMPAtxt;
024900150224           *inOA = *off;
025000150224         endif;
025100150223
025200150223       ENDSR;
025300150223
025400150223       //--------------------------------------------------------------
025500150223       //?Reperimento Dati del job (Utente/Operativi).                 ?
025600150223       //--------------------------------------------------------------
025700150223       BEGSR  sr_DatiJob;
025800150223
025900150223         in(e) §AzUte;
026000150223         if NOT %error;
026100150223           in(e) §DatiUte;
026200150223         endif;
026300150223         if %error or RSut = *blank;
026400150223           tibs34r ( tibs34ds );
026500150223           in §AzUte;
026600150223           in §DatiUte;
026700150223         endif;
026800150223
026900150223       ENDSR;
027000150223
027100150223       //--------------------------------------------------------------
027200150223       //?Preparazione SQL.                                            ?
027300150223       //--------------------------------------------------------------
027400150223       BEGSR  sr_PrepSQL;
027500150223
027600150223         // -?Preparazione SQL?
027700150223         clear  wSQL;
027800150223         wSQL = 'with +
027900150226                    KSU_CMC as ( select CMCncm as NCM, CMCksu as KSU, +
028000150226                                        CMCksc as KSC, CMCcpo as CPO, +
028100150224                                        CMCufe, CMCcch +
028200150223                                   from TICMC00F +
028300150223                                  where CMCncm in ( 1, 2 ) +
028400150223                                    and CMCcch <> ''  '' +
028500150226                                  order by CMCksu ), +
028600150226                    KSU_CMF as ( select distinct +
028700150226                                        CMFncm as NCM, CMFksu as KSU, +
028800150226                                        CMFksc as KSC, CMFcpo as CPO, +
028900150226                                        CMFacm, CMFcch +
029000150226                                   from TICMF00F inner join TICMC00F +
029100150226                                     on CMFncm = CMCncm +
029200150226                                    and CMFksu = CMCksu +
029300150226                                    and CMFksc = CMCksc +
029400150226                                    and CMFcpo = CMCcpo +
029500150226                                    and CMFacm = CMCufe +
029600150226                                  where CMFncm in ( 1, 2 ) +
029700150226                                    and CMCcch = '' '' +
029800150226                                    and CMFacm = ''' + c_FaseChiusura +
029900150226                                ''' and CMFpea = 0 +
030000150226                                  order by CMFksu ) +
030100150226                 select * from KSU_CMC +
030200150226                  union +
030300150226                 select * from KSU_CMF +
030400150226                  order by NCM, KSU, KSC, CPO +
030500150223                    for fetch only';
030600150223
030700150223         exec sql   prepare S1   from :wSQL;
030800150223
030900150223         // -?Dichiarazione cursore?
031000150223         exec sql   declare C1   cursor for S1;
031100150223
031200150223         if  SQLcode < *zero;
031300150223           $EoF = *on;
031400150223           exsr  sr_PrintErrSQL;
031500150223         endif;
031600150223
031700150223       ENDSR;
031800150223
031900150223       //--------------------------------------------------------------
032000150223       //?Apertura cursore.                                            ?
032100150223       //--------------------------------------------------------------
032200150223       BEGSR  sr_OpenCursor;
032300150223
032400150223         // -?Apertura del cursore?
032500150223         exec sql   open C1;
032600150223
032700150223         if  SQLcode < *zero;
032800150223           $EoF = *on;
032900150223           exsr  sr_PrintErrSQL;
033000150223         endif;
033100150223
033200150223       ENDSR;
033300150223
033400150223       //--------------------------------------------------------------
033500150223       //?Lettura cursore.                                             ?
033600150223       //--------------------------------------------------------------
033700150223       BEGSR  sr_ReadCursor;
033800150223
033900150223         clear  wSQL_ds;
034000150223
034100150223         exec sql   fetch next   from C1   into :wSQL_ds;
034200150223
034300150223         Select;
034400150223
034500150223           // -?Fine File?
034600150223           When  SQLcode = 100;
034700150223             $EoF = *on;
034800150223
034900150223           // -?Errore SQL?
035000150223           When  SQLcode < *zero;
035100150223             $EoF = *on;
035200150223             exsr  sr_PrintErrSQL;
035300150223
035400150224           // -?Richiesta SOLO stampa?
035500150224           When  $SoloStampa;
035600150224             exsr  sr_Stampa;
035700150224
035800150223           // -?Elaborazione singolo cliente?
035900150223           Other;
036000150223             exsr  sr_ElabRec;
036100150223
036200150223         EndSl;
036300150223
036400150223       ENDSR;
036500150224
036600150224       //--------------------------------------------------------------
036700150224       //?Stampa del singolo Cliente eliminabile della Campagna.       ?
036800150224       //--------------------------------------------------------------
036900150224       BEGSR  sr_Stampa;
037000150224
037100150224         clear  keyTICMC01;
037200150224         k_CMCncm = wSQL_ds.wNCM;
037300150224         k_CMCksu = wSQL_ds.wKSU;
037400150224         k_CMCksc = wSQL_ds.wKSC;
037500150224         k_CMCcpo = wSQL_ds.wCPO;
037600150224         chain  %kds( keyTICMC01 )  TICMC000;
037700150224         if  Not %found(TICMC01L);
037800150224           clear  CMCufe;
037900150224           clear  CMCcch;
038000150224           clear  CMCdch;
038100150224         endif;
038200150223
038300150224         if  *inOA;
038400150224           except  STAMPAtxt;
038500150224           *inOA = *off;
038600150224         endif;
038700150224
038800150224         except STAMPAdet;
038900150224
039000150224       ENDSR;
039100150223       //--------------------------------------------------------------
039200150223       //?Elaborazione singolo Cliente da eliminare dalla Campagna.    ?
039300150223       //--------------------------------------------------------------
039400150223       BEGSR  sr_ElabRec;
039500150223
039600150223         // -?Modifica Campagna per Cliente Unificante?
039700150223         clear  TRKC75ds;
039800150223         iKC75ncm  = wSQL_ds.wNCM;
039900150223         iKC75ksu  = wSQL_ds.wKSU;
040000150223         iKC75ksc  = wSQL_ds.wKSC;
040100150223         iKC75cpo  = wSQL_ds.wCPO;
040200150223         iKC75nncm = c_Campagna_ClientiEsclusi;
040300150223
040400150223         TRKC75R ( kpjba : TRKC75ds );
040500150223
040600150223         if  oKC75err <> *blank;
040700150223           wErr = 2;
040800150223           exsr  sr_SendEmail;
040900150223         endif;
041000150223
041100150223       ENDSR;
041200150223
041300150223       //--------------------------------------------------------------
041400150223       //?Chiusura cursore.                                            ?
041500150223       //--------------------------------------------------------------
041600150223       BEGSR  sr_CloseCursor;
041700150223
041800150223         // -?Chiusura del cursore?
041900150223         exec sql   close C1;
042000150223
042100150223       ENDSR;
042200150223
042300150223       //--------------------------------------------------------------
042400150223       //?Stampa segnalazione dell'errore rilevato via SQL             ?
042500150223       //--------------------------------------------------------------
042600150223       BEGSR  sr_PrintErrSQL;
042700150223
042800150223         // -?Stampa del Dump?
042900150223         Dump(A);
043000150223
043100150223         // -?Stampa del Job-Log?
043200150223         Qcmd = 'DSPJOBLOG job(*) output(*print)';
043300150223         exsr  sr_ExecCmd;
043400150223
043500150223         // -?Invio e-mail con errore rilevato?
043600150223         wErr = 1;
043700150223         exsr  sr_SendEmail;
043800150223
043900150223         // -?Chiusura del programma?
044000150223         exsr  sr_RoutEnd;
044100150223
044200150223       ENDSR;
044300150223
044400150223       //--------------------------------------------------------------
044500150223       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
044600150223       //--------------------------------------------------------------
044700150223       BEGSR  sr_SendEmail;
044800150223
044900150223         // -?Override al file di stampa ed apertura dello stesso?
045000150223         If  NOT  %open(PrtEMAIL);
045100150223
045200150223           // -?Reperimento regole per mailing da tab. "MRA"?
045300150223           clear  dMRAdan;
045400150223           if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
045500150223                            *omit : *omit :
045600150223                            *omit : *omit :
045700150223                            *omit : *omit : *omit : *omit :
045800150223                            *omit : *omit : *omit : *omit :
045900150223                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
046000150223                            = *zero      AND
046100150223                 ds_TNTBE.TBEatb = *blank;
046200150223             dMRAdan = ds_TNTBE.TBEuni;
046300150223           endif;
046400150223
046500150223           // -?Impostazione regole per mailing?
046600150223           §CM1mitt = %trim(§MRADmitt);
046700150223           §CM1dst  = %trim(§MRADdest);
046800150223           §CM1tips = §MRAdreg;
046900150223           §CM1var  = '*OBJM*' + §MRADdes;
047000150223           §CM1idp  = §MRADidPro;
047100150223
047200150223           // -?Esecuzione override al file di stampa (per mailing)?
047300150223           Qcmd = c_CmdOvrPrtF
047400150223                + ' outq(' + %trim(§MRADoutqI) + ')'
047500150223                + ' usrdfndta(''' + TRTCM1ds + ''')';
047600150223           exsr  sr_ExecCmd;
047700150223
047800150223           // -?Apertura file di stampa?
047900150223           open  PrtEMAIL;
048000150223           *inOF = *on;
048100150223
048200150223         EndIf;
048300150223
048400150223         // -?Preparazione e-Mail per il singolo errore rilevato?
048500150223         if  *inOF;
048600150223           except  PRTtxt;
048700150223           *inOF = *off;
048800150223         endif;
048900150223
049000150223         Select;
049100150223           When  wErr = 1;
049200150223             except  PRTerr01;
049300150223           When  wErr = 2;
049400150223             except  PRTerr02;
049500150223         EndSl;
049600150223
049700150223       ENDSR;
049800150223
049900150223       //--------------------------------------------------------------
050000150223       //?Esecuzione del comando (già impostato).                      ?
050100150223       //--------------------------------------------------------------
050200150223       BEGSR  sr_ExecCmd;
050300150223
050400150223         clear Qcap0100;
050500150223         Qcabcsdh = *off;
050600150223         Qcapa    = *off;
050700150223         Qcacmdss = *off;
050800150223         Qcaerved = *allX'00';
050900150223
051000150223         clear Qusec;
051100150223         Qusbprv  = %size(Qusec);
051200150223
051300150223         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
051400150223                           %size(Qcap0100) : 'CPOP0100' : *omit :
051500150223                           0 : 0 : Qusec);
051600150223
051700150223         //if  Qusei <> *blank;
051800150223         //  ...;
051900150223         //endif;
052000150223
052100150223       ENDSR;
052200150223
052300150223       //--------------------------------------------------------------
052400150223       //?Operazioni finali.                                           ?
052500150223       //--------------------------------------------------------------
052600150223       BEGSR  sr_RoutEnd;
052700150223
052800150223         // -?Chiusura funzioni precedentemente aperte?
052900150223         cloTabella();
053000150223
053100150223         // -?SE stampato almeno un Errore:?
053200150223         //  ?stampa "fine" e chiude il file di stampa?
053300150223         if  %open(PrtEMAIL);
053400150223           except  PRTend;
053500150223           close   PrtEMAIL;
053600150223         endif;
053700150224
053800150224         // -?SE richiesta SOLO stampa:?
053900150224         //  ?stampa "fine" e chiude il file di stampa?
054000150224         if  $SoloStampa  and  %open(QSYSPRT);
054100150224           except  STAMPAend;
054200150224           close   QSYSPRT;
054300150224         endif;
054400150223
054500150223         // -?Uscita dal *pgm?
054600150223         return;
054700150223
054800150223       ENDSR;
054900150223
055000150223      /end-free
055100150223
055200150223       //--------------------------------------------------------------
055300150223       //?S P O O L - F I L E S                                        ?
055400150223       //--------------------------------------------------------------
055500150223
055600150223     oPrtEMAIL  e            PRTtxt            1
055700150223     o                       RSUT
055800150223     o                                        +   5 'PULIZIA DEI CLIENTI IN +
055900150223     o                                               CAMPAGNA'
056000150223     o                       SDSpgm           +   5
056100150223     o                       wDate         y  +   5
056200150223     o          e            PRTtxt      1
056300150223     o                       KNSIF
056400150223     o                       KNMUS            +   1
056500150223     o                                        +   4 '-----------------------+
056600150223     o                                               --------'
056700150223     o                                        +   5 'Pag.'
056800150223     o                       Page          z  +   0
056900150224     o                       wTime            +   7 '  :  :  '
057000150223      *
057100150223     o          e            PRTerr01    2
057200150223     o                                              'Rilevato ERRORE n-
057300150223     o                                              ella pulizia dei c-
057400150223     o                                              lienti in Campagna-
057500150223     o                                              .'
057600150223     o          e            PRTerr01    2
057700150223     o                                              'Consultare le sta-
057800150223     o                                              mpe del lavoro'
057900150223     o
058000150223     o                       SDSjobNbr     3  +   1
058100150223     o                                        +   0 '/'
058200150223     o                       SDSjobUser       +   0
058300150223     o                                        +   0 '/'
058400150223     o                       SDSjobName       +   0
058500150223      *
058600150223     o          e            PRTerr02    2
058700150223     o                                              'Campagna'
058800150223     o                       wSQL_ds.wNCM  z  +   1
058900150223     o                                        +   2 'Unificante'
059000150223     o                       wSQL_ds.wKSU     +   1
059100150223     o                                        +   1 '-'
059200150223     o                       oKC75msg         +   1
059300150223      *
059400150223     o          e            PRTend      2
059500150223     o                                        +  10 '***  Fine Lista  ***'
059600150224
059700150224       //--------------------------------------------------------------
059800150224
059900150224     oQSYSPRT   e            STAMPAtxt         1
060000150224     o                       RSUT
060100150224     o                                        +   5 'PULIZIA DEI CLIENTI IN +
060200150224     o                                               CAMPAGNA'
060300150224     o                       SDSpgm           +   5
060400150224     o                       wDate         y  +   5
060500150224     o          e            STAMPAtxt   1
060600150224     o                       KNSIF
060700150224     o                       KNMUS            +   1
060800150224     o                                        +   4 '-----------------------+
060900150224     o                                               --------'
061000150224     o                                        +   5 'Pag.'
061100150224     o                       Page2         z  +   0
061200150224     o                       wTime            +   7 '  :  :  '
061300150224     o          e            STAMPAtxt   2
061400150224     o                                              'Campagna'
061500150224     o                                        +   3 'Unific.'
061600150224     o                                        +   3 'Cliente'
061700150224     o                                        +   3 ' Potenziale'
061800150224     o                                        +   3 'Fase'
061900150224     o                                        +   3 'C.Ch'
062000150224     o                                        +   3 'Data Chius'
062100150224     o          e            STAMPAtxt   1  1
062200150224     o                                              '--------'
062300150224     o                                        +   3 '-------'
062400150224     o                                        +   3 '-------'
062500150224     o                                        +   3 '-----------'
062600150224     o                                        +   3 '----'
062700150224     o                                        +   3 '----'
062800150224     o                                        +   3 '----------'
062900150224      *
063000150224Campag*a   Unific.   Cliente    Potenziale   Fase   C.Ch   Data (CMC)
063100150224123456*    1234567   1234567   12345678901   123     12    1234/56/78
063200150224     o          e            STAMPAdet   1
063300150224     o                       k_CMCncm      z
063400150224     o                       k_CMCksu         +   4
063500150224     o                       k_CMCksc         +   3
063600150224     o                       k_CMCcpo         +   3
063700150224     o                       CMCufe           +   3
063800150224     o                       CMCcch           +   5
063900150224     o                       CMCdch           +   4 '    /  /  '
064000150224      *
064100150224     o          e            STAMPAend   2
064200150224     o                                        +  25 '***  Fine Lista  ***'
