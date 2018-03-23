000100150616       //==============================================================
000200150616       //?FIDNA8R - Chiusura R.A. di SEDE aperte da più di un anno.    ?
000300150616       //==============================================================
000400150616
000500150616       //--------------------------------------------------------------
000600150616       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150616       //--------------------------------------------------------------
000800150616
000900150616     /*PRM  dbgview(*source)
001000150616     /*END
001100150616
001200150616       //--------------------------------------------------------------
001300150616       //?Specifiche di controllo.                                     ?
001400150616       //--------------------------------------------------------------
001500150616
001600150616     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150616     h dftactgrp(*no)
001800150616     h alwnull(*inputonly)
001900150616
002000150616       //--------------------------------------------------------------
002100150616       //?Dichiarazione file.                                          ?
002200150616       //--------------------------------------------------------------
002300150616
002400150616       // -?Bolle Sede?
002500150616     fTITAS30C  if   e           k disk    usropn
002600150616
002700150616       // -?O.R.M.?
002800150616     fFNORM01L  if   e           k disk    usropn
002900150618
003000150720       // -?Spool per Lista R.A. di sede da chiudere (ogg. Generico)?
003100150720     fPrtEMail  o    f  132        printer usropn  oflind(*inOF)
003200150616
003300150616       //--------------------------------------------------------------
003400150616       //?Definizione costanti.                                        ?
003500150616       //--------------------------------------------------------------
003600150616
003700150622       // -?Codice Sede?
003800150619     d c_Sede          c                   const(046)
003900150622
004000150622       // -?Motivo di Chiusura per le R.A.?
004100150622     d c_CodChiusura   c                   const('  I')
004200150720
004300150720       // -?Comandi di override ai PrtF (e-mail)?
004400150720     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004500150720     d                                           file(PrtEMail) +
004600150720     d                                           pagesize(66 120 *rowcol) +
004700150720     d                                           lpi(6) cpi(16.7) +
004800150720     d                                           ovrflw(63) +
004900150720     d                                           hold(*no) +
005000150720     d                                           ovrscope(*actgrpdfn)')
005100150720     d c_CmdDltOvr     c                   const('DLTOVR +
005200150720     d                                            file(PrtEMail) +
005300150720     d                                            lvl(*actgrpdfn)')
005400150616
005500150616       //--------------------------------------------------------------
005600150616       //?Definizione schiere.                                         ?
005700150616       //--------------------------------------------------------------
005800150616
005900150616
006000150616       //--------------------------------------------------------------
006100150616       //?Definizione aree dati.                                       ?
006200150616       //--------------------------------------------------------------
006300150616
006400150616       // -?Dati utente?
006500150616     d §AzUte        e ds                  extname(AZUTE00F)
006600150616     d                                     dtaara
006700150616     d §DatiUte      e ds                  extname(dDatiUte)
006800150616     d                                     dtaara
006900150616
007000150616       //--------------------------------------------------------------
007100150616       //?Definizione strutture dati.                                  ?
007200150616       //--------------------------------------------------------------
007300150616
007400150720       // -?Tab. MRA/DAN = Bart-Maling - Danni?
007500150720     d dMRAdan       e ds
007600150720
007700150720       // -?Parametri x Ridefinizione dati utente estesi per mailing?
007800150720     d TRTCM1ds      e ds                  inz
007900150720       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
008000150721     d   §CM1mitt    e                     inz('ced')
008100150720       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
008200150720     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
008300150720       //  ?·§CM1tips = Tipo lettera via e-mail:?
008400150720       //   ?"LET" = testo allegato in corpo con logo?
008500150720       //           ?(richiede righe libere iniziali per il logo)?
008600150720       //   ?"COR" = testo integrato senza logo?
008700150720       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
008800150720     d   §CM1tips    e                     inz('COI')
008900150720       //  ?·§CM1po   = Filiale?
009000150720     d   §CM1po      e                     inz('046')
009100150720       //  ?·§CM1var  = Oggetto e-mail?
009200150720     d   §CM1var     e                     inz('*OBJM*+
009300150720     d                                     Chiusura R.A. di Sede +
009400150720     d                                     aperte da più di un anno')
009500150720       //  ?·§CM1sts  = Stato?
009600150720     d   §CM1sts     e                     inz(*off)
009700150720       //  ?·§CM1sts  = Id processo?
009800150721     d   §CM1idp     e                     inz('*')
009900150720
010000150616       // -?Status ds?
010100150616     d Status         sds
010200150618     d   SDSpgm          *proc
010300150618     d*//SDSprm          *parms
010400150618     d*//SDSdta              191    198
010500150618     d   SDSjobName          244    253
010600150618     d   SDSjobUser          254    263
010700150618     d   SDSjobNbr           264    269s 0
010800150616
010900150616       // -?Data/Ora attuali?
011000150616     d wTime_ds        ds            14    inz
011100150616     d   wDate                        8s 0 inz
011200150616     d   wTime                        6s 0 inz
011300150618
011400150618       // -?Oggetto = Spedizione?
011500150618     d WRKspe_ds       ds            16    inz
011600150618     d   WRKlnp                       3
011700150618     d   WRKnrs                       2
011800150618     d   WRKnsp                       7
011900150619     d   WRKaas                       4
012000150618
012100150618       // -?Oggetto = O.R.M.?
012200150618     d WRKorm_ds       ds            14    inz
012300150618     d   WRKpoe                       3
012400150618     d   WRKnso                       2
012500150618     d   WRKnor                       7
012600150618     d   WRKnrv                       2
012700150616
012800150616       // -?Dati estratti via SQL?
012900150618     d FITGD_ds      e ds                  extname(FITGD00F)  inz
013000150618     d*// FITGD00F      e ds                  based(nullPtr)
013100150618     d*// wSQL_ds         ds                  qualified     inz
013200150618     d*//   wPOT                              like(TGDpot)  inz
013300150618     d*//   wANT                              like(TGDant)  inz
013400150618     d*//   wNUT                              like(TGDnut)  inz
013500150618     d*//   wPDT                              like(TGDpdt)  inz
013600150618     d*//   wDAC                              like(TGDdac)  inz
013700150618     d*//   wORC                              like(TGDorc)  inz
013800150618     d*//*//wPRU                              like(TGDpru)  inz
013900150618     d*//   wRSC                              like(TGDrsc)  inz
014000150618     d*//   wTOR                              like(TGDtor)  inz
014100150618     d*//   wOGG                              like(TGDogg)  inz
014200150618     d*//*//wPRT                              like(TGDprt)  inz
014300150618     d*//*//wPOG                              like(TGDpog)  inz
014400150618     d*//*//wFAR                              like(TGDfar)  inz
014500150618     d*//   wKSC                              like(TGDksc)  inz
014600150616
014700150616       // -?Parametri ricevuti?
014800150616     d KPJBA         e ds
014900150616
015000150616       //--------------------------------------------------------------
015100150616       //?Definizione variabili globali.                               ?
015200150616       //--------------------------------------------------------------
015300150616
015400150616       // -?Flags booleani?
015500150619     d $Test           s               n   inz
015600150618     d $EoF            s               n   inz
015700150618     d $ChiudiRA       s               n   inz
015800150617
015900150617       // -?Stringa SQL da eseguire?
016000150618     d wSQL            s           2048    inz  varying
016100150618
016200150618       // -?Campi di comodo?
016300150618     d wDate_EUR       s               d   inz  datfmt(*EUR)
016400150618     d wOggi           s              8s 0 inz
016500150622     d wDataLim        s              8s 0 inz
016600150618
016700150619       // -?Campi per controllo "rotture" di livello?
016800150619     d wSaveTGDtor     s                   inz  like(TGDtor)
016900150619
017000150618       // -?Campi di stampa?
017100150618     d wwwMSG          s            100    inz
017200150618     d wwwDAC          s              8  0 inz
017300150618     d wwwOGG          s             21    inz
017400150619     d wwwNOT1         s                   inz  like(DB0no1)
017500150619     d wwwNOT2         s                   inz  like(DB0no2)
017600150616
017700150616       //--------------------------------------------------------------
017800150616       //?Definizione prototipi procedure.                             ?
017900150616       //--------------------------------------------------------------
018000150616
018100150616       // -?Reperimento dati utente?
018200150616     d TIBS34ds      e ds
018300150616      /copy gaitrasrc/srcProtoPR,TIBS34R
018400150720
018500150720       // -?Ricerca/Controllo tabelle?
018600150720     d TIBS02ds      e ds                  inz
018700150720      /copy gaitrasrc/srcProtoPR,TIBS02R
018800150616
018900150616       // -?Chiusura singola Richiesta Assistenza?
019000150616     d FIDNA7ds      e ds                  inz
019100150616     d fidnA7r         pr                  extpgm('FIDNA7R')
019200150616     d   fidnA7ds                          likeds(FIDNA7ds)
019300150618
019400150618       // -?Gestione Note delle Richiesta Assistenza?
019500150618     d FIDNB0ds      e ds                  inz
019600150618     d fidnB0r         pr                  extpgm('FIDNB0R')
019700150618     d   kpjba                             likeds(KPJBA)
019800150618     d   fidnB0ds                          likeds(FIDNB0ds)
019900150619
020000150619       // -?Parametri API QCAPCMD (Process Commands)?
020100150619     d Qcmd            s           2048    inz  varying
020200150619      /copy qSysInc/qRpgleSrc,QCAPCMD
020300150619       // -?API QCAPCMD (Process Commands)?
020400150619      /copy gaitrasrc/srcProtoPR,QCAPCMD
020500150619
020600150619       // -?Parametri gestione errori API.?
020700150619      /copy qsysinc/qrpglesrc,QUSEC
020800150616
020900150616       //--------------------------------------------------------------
021000150616       //?Definizione key-list.                                        ?
021100150616       //--------------------------------------------------------------
021200150616
021300150618       // -?File TITAS30C?
021400150618     d keyTITAS30    e ds                  extname( TITAS30C : *key )
021500150618     d                                     prefix( k_ )  inz
021600150618
021700150618       // -?File FNORM01L?
021800150618     d keyFNORM01    e ds                  extname( FNORM01L : *key )
021900150618     d                                     prefix( k_ )  inz
022000150616
022100150616       //--------------------------------------------------------------
022200150616       //?M A I N - L I N E                                            ?
022300150616       //--------------------------------------------------------------
022400150616
022500150616     c     *Entry        plist
022600150616     c                   parm                    KPJBA
022700150616
022800150616      /free
022900150616
023000150616       // -?Operazioni iniziali?
023100150616       exsr  sr_RoutInz;
023200150616
023300150616       // -?Preparazione SQL per estrazione R.A. aperte da più di un anno?
023400150616       exsr  sr_PrepSQL;
023500150616
023600150616       // -?Apertura Cursore?
023700150616       exsr  sr_OpenCursor;
023800150616
023900150616       // -?Ciclo di lettura R.A. di Sede aperte da più di un anno?
024000150616       DoW  Not $EoF;
024100150616         exsr  sr_ReadCursor;
024200150616       EndDo;
024300150616
024400150616       // -?Chiusura Cursore?
024500150616       exsr  sr_CloseCursor;
024600150616
024700150616       // -?Operazioni finali?
024800150616       exsr  sr_RoutEnd;
024900150616
025000150616       //--------------------------------------------------------------
025100150616       //?Operazioni iniziali.                                         ?
025200150616       //--------------------------------------------------------------
025300150616       BEGSR  sr_RoutInz;
025400150617
025500150618         // -?Impostazione opzioni per SQL?
025600150618         exec sql   set  option  DynUsrPrf = *Owner,
025700150618                                 CloSqlCsr = *EndMod;
025800150616
025900150617         // -?Impostazione chiusura?
026000150616         *inLR = *on;
026100150616
026200150616         // -?Reperimento dati job?
026300150616         exsr  sr_DatiJob;
026400150616
026500150618         // -?Reperimento Data e Ora attuali?
026600150618         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
026700150618         wDate_Eur = %date( wDate : *Iso );
026800150618         wOggi     = %dec( wDate_Eur );
026900150622         wDataLim  = %dec( wDate_Eur - %years(1) );
027000150619
027100150619         $Test = ( %subst( KPJBU : 1 : 5 ) = 'TEST '  or
027200150619                   %subst( KPJBU : 1 : 5 ) = 'PROVA' );
027300150616
027400150616       ENDSR;
027500150616
027600150616       //--------------------------------------------------------------
027700150616       //?Reperimento Dati del job (Utente/Operativi).                 ?
027800150616       //--------------------------------------------------------------
027900150616       BEGSR  sr_DatiJob;
028000150616
028100150616         in(E) §AzUte;
028200150616         if NOT %error;
028300150616           in(E) §DatiUte;
028400150616         endif;
028500150616         if %error or RSut = *blanks;
028600150616           clear TIBS34ds;
028700150616           tibs34r ( tibs34ds );
028800150616           in §AzUte;
028900150616           in §DatiUte;
029000150616         endif;
029100150616
029200150616       ENDSR;
029300150617
029400150617       //--------------------------------------------------------------
029500150617       //?Preparazione stringa SQL per l'estrazione dei dati.          ?
029600150617       //--------------------------------------------------------------
029700150617       BEGSR  sr_PrepSQL;
029800150617
029900150618         clear  wSQL;
030000150618
030100150618         //wSQL = 'select * +
030200150618         //          from FITGD00F left outer join FITGN00F +
030300150618         //            on TGDant = TGNant +
030400150618         //           and TGDnut = TGNnut +
030500150618         //           and TGDpdt = TGNpdt +
030600150619         //         where TGDpot = ' + %editc( c_Sede : 'X' ) +
030700150618         //         ' and TGDcdt = '' '' +
030800150618         //         order by TGDtor, TGDdac, TGDorc, TGNtin, TGNpgn +
030900150618         //           for fetch only';
031000150618
031100150618         wSQL = 'select * +
031200150618                   from FITGD00F +
031300150619                  where TGDpot = ' + %editc( c_Sede : 'X' ) +
031400150618                  ' and TGDcdt = '' '' +
031500150622                    and TGDdac < ' + %editc( wDataLim : 'X' ) +
031600150622                ' order by TGDtor, TGDdac, TGDorc +
031700150618                    for fetch only';
031800150617
031900150617       ENDSR;
032000150617
032100150617       //--------------------------------------------------------------
032200150617       //?Apertura cursore SQL.                                        ?
032300150617       //--------------------------------------------------------------
032400150617       BEGSR  sr_OpenCursor;
032500150617
032600150617         // -?Dichiarazione cursore?
032700150617         exec sql   prepare S1   from :wSQL;
032800150617         exec sql   declare C1   cursor for S1;
032900150617
033000150617         if  SQLcode < *zero;
033100150617           $EoF = *on;
033200150618           exsr  sr_PrintErrSQL;
033300150617         endif;
033400150617
033500150617         // -?Apertura del cursore?
033600150617         exec sql   open C1;
033700150617
033800150617         if  SQLcode < *zero;
033900150617           $EoF = *on;
034000150617           exsr  sr_PrintErrSQL;
034100150617         endif;
034200150617
034300150617       ENDSR;
034400150617
034500150617       //--------------------------------------------------------------
034600150617       //?Lettura cursore.                                             ?
034700150617       //--------------------------------------------------------------
034800150617       BEGSR  sr_ReadCursor;
034900150617
035000150619         clear  FITGD_ds;
035100150617
035200150618         exec sql   fetch next   from C1   into :FITGD_ds;
035300150617
035400150618         Select;
035500150618
035600150618           // -?Fine File?
035700150618           When  SQLcode = 100;
035800150617             $EoF = *on;
035900150618
036000150618           // -?Errore SQL?
036100150618           When  SQLcode < *zero;
036200150617             $EoF = *on;
036300150617             exsr  sr_PrintErrSQL;
036400150618
036500150618           // -?Elaborazione singola R.A.?
036600150618           Other;
036700150618             exsr  sr_ElabRec;
036800150618
036900150618         EndSl;
037000150617
037100150617       ENDSR;
037200150617
037300150617       //--------------------------------------------------------------
037400150617       //?Chiusura cursore SQL.                                        ?
037500150617       //--------------------------------------------------------------
037600150617       BEGSR  sr_CloseCursor;
037700150617
037800150617         // -?Chiusura del cursore?
037900150617         exec sql   close C1;
038000150617
038100150617       ENDSR;
038200150617
038300150617       //--------------------------------------------------------------
038400150618       //?Elaborazione singola R.A.                                    ?
038500150617       //--------------------------------------------------------------
038600150618       BEGSR  sr_ElabRec;
038700150617
038800150618         clear  $ChiudiRA;
038900150618         clear  wwwMSG;
039000150619
039100150619         wwwOGG = TGDogg;
039200150618
039300150618         Select;
039400150618
039500150618           // -?Tipo Oggetto GENERICO: R.A. solo stampata?
039600150618           When  TGDtor = 'G';
039700150619             if  TGDtor <> wSaveTGDtor;
039800150619               wwwMSG = 'R.A. con tipo oggetto GENERICO:';
039900150619             endif;
040000150618             exsr  sr_StampaRA;
040100150915             wSaveTGDtor = TGDtor;
040200150618
040300150618           // -?Tipo Oggetto O.R.M.: controllo SE "chiudibile"?
040400150618           When  TGDtor = 'O';
040500150618             exsr  sr_CtrlORM;
040600150618             if  $ChiudiRA;
040700150915               if  $Test;
040800150915                 if  TGDtor <> wSaveTGDtor;
040900150915                   wwwMSG = 'R.A. chiuse per O.R.M. non più presente:';
041000150915                 endif;
041100150915                 exsr  sr_StampaRA;
041200150915               endif;
041300150618               exsr  sr_ChiusuraRA;
041400150915               wSaveTGDtor = TGDtor;
041500150618             endif;
041600150618
041700150618           // -?Tipo Oggetto SPEDIZIONE: controllo SE "chiudibile"?
041800150618           When  TGDtor = 'S';
041900150618             exsr  sr_CtrlSPED;
042000150618             if  $ChiudiRA;
042100150915               if  $Test;
042200150915                 if  TGDtor <> wSaveTGDtor;
042300150915                   wwwMSG = 'R.A. chiuse per Spedizione non più presente:';
042400150915                 endif;
042500150915                 exsr  sr_StampaRA;
042600150915               endif;
042700150618               exsr  sr_ChiusuraRA;
042800150915               wSaveTGDtor = TGDtor;
042900150618             endif;
043000150618
043100150618           // -?Tipo Oggetto ? ? ?: R.A. solo stampata?
043200150618           Other;
043300150619             if  TGDtor <> wSaveTGDtor;
043400150619               wwwMSG = 'R.A. con tipo oggetto NON previsto:';
043500150619             endif;
043600150618             exsr  sr_StampaRA;
043700150915             wSaveTGDtor = TGDtor;
043800150618
043900150618         EndSl;
044000150619
044100150617
044200150617       ENDSR;
044300150618
044400150618       //--------------------------------------------------------------
044500150618       //?Controllo O.R.M.                                             ?
044600150618       //--------------------------------------------------------------
044700150618       BEGSR  sr_CtrlORM;
044800150618
044900150618         WRKorm_ds = TGDogg;
045000150618
045100150618         // -?Impostazione O.R.M. in stampa?
045200150618         wwwOGG = WRKpoe + '/' + WRKnso + '/' + WRKnor + '/' + WRKnrv;
045300150618
045400150618         // -?Verifica esistenza O.R.M.?
045500150619         if  NOT %open(FNORM01L);
045600150619           open  FNORM01L;
045700150619         endif;
045800150618         clear  keyFNORM01;
045900150618         k_ORMpoe = %int( WRKpoe );
046000150618         k_ORMnsr = %int( WRKnso );
046100150618         k_ORMnor = %int( WRKnor );
046200150618         k_ORMnrv = %int( WRKnrv );
046300150618
046400150618         chain  %kds( keyFNORM01 )  FNORM000;
046500150618
046600150618         // -?O.R.M. ancora presente => uscita?
046700150618         if  %found( FNORM01L );
046800150618           leavesr;
046900150618         endif;
047000150618
047100150618         // -?O.R.M. non più presente (R.A. da chiudere)?
047200150618         $ChiudiRA = *on;
047300150618
047400150618       ENDSR;
047500150618
047600150618       //--------------------------------------------------------------
047700150618       //?Controllo SPEDIZIONE.                                        ?
047800150618       //--------------------------------------------------------------
047900150618       BEGSR  sr_CtrlSPED;
048000150618
048100150618         WRKspe_ds = TGDogg;
048200150618
048300150618         // -?Impostazione SPEDIZIONE in stampa?
048400150618         wwwOGG = WRKlnp + '/' + WRKnrs + '/' + WRKnsp + ' ' + WRKaas;
048500150618
048600150618         // -?Verifica esistenza SPEDIZIONE?
048700150619         if  NOT %open(TITAS30C);
048800150619           open  TITAS30C;
048900150619         endif;
049000150618         clear  keyTITAS30;
049100150618         k_TASaas = %int( WRKaas );
049200150618         k_TASlnp = %int( WRKlnp );
049300150618         k_TASnrs = %int( WRKnrs );
049400150618         k_TASnsp = %int( WRKnsp );
049500150618
049600150915         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
049700150618
049800150916         // -?Spedizione ancora presente => uscita?
049900150618         if  %found( TITAS30C );
050000150618           leavesr;
050100150618         endif;
050200150618
050300150916         // -?Spedizione non più presente (R.A. da chiudere)?
050400150618         $ChiudiRA = *on;
050500150618
050600150618       ENDSR;
050700150618
050800150618       //--------------------------------------------------------------
050900150618       //?Stampa singola R.A.                                          ?
051000150618       //--------------------------------------------------------------
051100150618       BEGSR  sr_StampaRA;
051200150619
051300150720         If  NOT %open(PrtEMail);
051400150720
051500150720           // -?Reperimento tab. MRA = Bart-Maling - Danni?
051600150720           clear  dMRAdan;
051700150720           clear  TIBS02ds;
051800150720           T02mod = 'C';
051900150720           //T02sif = knsif;
052000150720           T02cod = 'MRA';
052100150720           T02ke1 = %trimr(SDSpgm);
052200150720
052300150720           TNTBE_RicercaControllo ( kpjba : tibs02ds );
052400150720
052500150720           if T02err = *blanks;
052600150720             dMRAdan = T02uni;
052700150720           endif;
052800150720
052900150720           // -?Esecuzione override?
053000150720           reset  TRTCM1ds;
053100150720           If  §MRAdReg <> *blank;
053200150720
053300150720             §CM1mitt = %trim(§MRAdMitt);
053400150720             §CM1dst  = §MRAdDest;
053500150720             §CM1tips = §MRAdReg;
053600150720             §CM1po   = %editc( DUTpou : 'X' );
053700150720             //§CM1var  = '*OBJM*' + §MRAdDes;
053800150720             §CM1idp  = §MRAdIdPro;
053900150720
054000150720             Qcmd = c_CmdOvrPrtF
054100150720                  + ' outq(' + %trim(§MRAdOutqI) + ')'
054200150720                  + ' usrdfndta(''' + TRTCM1ds + ''')';
054300150720             exsr  sr_ExecCmd;
054400150720
054500150720           EndIf;
054600150720
054700150720           open  PrtEMail;
054800150720           *inOF = *on;
054900150720
055000150720         EndIf;
055100150619
055200150720         // -?Stampa "Testata" Messaggi?
055300150619         if  wwwMSG <> *blank;
055400150720           if  *inOF;
055500150720             except  PRTtxt;
055600150720             if  $Test;
055700150720               except  PRTtxt2;
055800150720             endif;
055900150720           endif;
056000150720           except  PRTmsg;
056100150720           except  PRTtxt3;
056200150720           *inOF = *off;
056300150619         endif;
056400150618
056500150618         // -?Data Creazione?
056600150618         wDate_Eur = %date( TGDdac : *Iso );
056700150618         wwwDAC    = %dec( wDate_Eur );
056800150618
056900150618         // -?Reperimento eventuali Note?
057000150618         clear  FIDNB0ds;
057100150618         iB0ant = TGDant;
057200150618         iB0nut = TGDnut;
057300150618         iB0pdt = TGDpdt;
057400150618         iB0tin = 'GEN';
057500150618         iB0flm = 'R';
057600150618
057700150618         fidnB0r ( kpjba : fidnB0ds );
057800150618
057900150618         // -?Stampa?
058000150720         if  *inOF;
058100150720           except  PRTtxt;
058200150720           if  $Test;
058300150720             except  PRTtxt2;
058400150720           endif;
058500150720           except  PRTtxt3;
058600150720           *inOF = *off;
058700150720         endif;
058800150720
058900150720         except  PRTdet;
059000150720
059100150720         if  DB0no1 <> *blank  or  DB0no2 <> *blank;
059200150720           wwwNOT1 = DB0no1;
059300150720           wwwNOT2 = DB0no2;
059400150720           except  PRTdet2;
059500150720         endif;
059600150618
059700150618       ENDSR;
059800150618
059900150618       //--------------------------------------------------------------
060000150618       //?Chiusura singola R.A.                                        ?
060100150618       //--------------------------------------------------------------
060200150618       BEGSR  sr_ChiusuraRA;
060300150619
060400150619         if  $Test;
060500150619           leavesr;
060600150619         endif;
060700150618
060800150618         clear  fidnA7ds;
060900150619         iDNA7ant = TGDant;
061000150619         iDNA7nut = TGDnut;
061100150619         iDNA7pdt = TGDpdt;
061200150622         iDNA7pct = c_Sede;
061300150622         iDNA7utc = 'BATCH     ';
061400150622         iDNA7cdt = c_CodChiusura;
061500150619         iDNA7not = 'Chiusura automatica';
061600150619         select;
061700150619           when  TGDtor = 'O';
061800150619             iDNA7not = %trimR( iDNA7not ) + ': O.R.M. non più presente';
061900150619           when  TGDtor = 'S';
062000150619             iDNA7not = %trimR( iDNA7not ) + ': Spedizione non più presente';
062100150619         endsl;
062200150618
062300150618         fidnA7r ( fidnA7ds );
062400150618
062500150618       ENDSR;
062600150617
062700150617       //--------------------------------------------------------------
062800150617       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
062900150617       //--------------------------------------------------------------
063000150617       BEGSR  sr_PrintErrSQL;
063100150617
063200150617         // -?Stampa del Dump?
063300150617         Dump(A);
063400150617
063500150617         // -?Stampa del Job-Log?
063600150617         Qcmd = 'DSPJOBLOG job(*) output(*print)';
063700150617         exsr  sr_ExecCmd;
063800150617
063900150617         // -?Chiusura del programma?
064000150617         exsr  sr_RoutEnd;
064100150617
064200150617       ENDSR;
064300150617
064400150617       //--------------------------------------------------------------
064500150617       //?Esecuzione del comando (già impostato).                      ?
064600150617       //--------------------------------------------------------------
064700150617       BEGSR  sr_ExecCmd;
064800150617
064900150617         clear Qcap0100;
065000150617         Qcabcsdh = *off;
065100150617         Qcapa    = *off;
065200150617         Qcacmdss = *off;
065300150617         Qcaerved = *allX'00';
065400150617
065500150617         clear Qusec;
065600150617         Qusbprv  = %size(Qusec);
065700150617
065800150617         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
065900150617                           %size(Qcap0100) : 'CPOP0100' : *omit :
066000150617                           0 : 0 : Qusec);
066100150617
066200150617         //if  Qusei <> *blank;
066300150617         //  ...;
066400150617         //endif;
066500150617
066600150617       ENDSR;
066700150616
066800150616       //--------------------------------------------------------------
066900150616       //?Operazioni finali.                                           ?
067000150616       //--------------------------------------------------------------
067100150616       BEGSR  sr_RoutEnd;
067200150618
067300150618         // -?SE chiusa/stampata almeno una R.A.:?
067400150618         //  ?stampa di "fine" e chiusura del file di stampa?
067500150720         if  %open(PrtEMail);
067600150618           except  PRTend;
067700150720           close  PrtEMail;
067800150720           Qcmd = c_CmdDltOvr;
067900150720           exsr  sr_ExecCmd;
068000150618         endif;
068100150618
068200150618         // -?Chiusura applicazioni precedentemente aperte?
068300150618         clear  FIDNB0ds;
068400150618         iB0tla = 'C';
068500150618         fidnB0r ( kpjba : fidnB0ds );
068600150720
068700150720         clear  TIBS02ds;
068800150720         T02tla = 'C';
068900150720         TNTBE_RicercaControllo ( kpjba : tibs02ds );
069000150616
069100150616         // -?Chiusura pgm?
069200150616         return;
069300150616
069400150616       ENDSR;
069500150616
069600150616      /end-free
069700150618
069800150618       //--------------------------------------------------------------
069900150618       //?S P O O L - F I L E S                                        ?
070000150618       //--------------------------------------------------------------
070100150618
070200150720     oPrtEMail  e            PRTtxt            3
070300150618     o                       RSUT
070400150720     o                                        +   5 'LISTA R.A. DI SEDE DA +
070500150720     o                                               CHIUDERE POICHÈ APERTE +
070600150618     o                                               DA PIÙ DI UN ANNO'
070700150618     o                       SDSpgm           +   5
070800150618     o                       wOggi         y  +   5
070900150618     o          e            PRTtxt      0
071000150720     o                                        +  25 'LISTA R.A. DI SEDE DA +
071100150720     o                                               CHIUDERE POICHÈ APERTE +
071200150618     o                                               DA PIÙ DI UN ANNO'
071300150619     o          e            PRTtxt      1
071400150618     o                       KNSIF
071500150618     o                       KNMUS            +   1
071600150720     o                                        +   4 '----------------------+
071700150720     o                                               -----------------------+
071800150618     o                                               -----------------'
071900150618     o                                        +   5 'Pag.'
072000150618     o                       Page          z  +   0
072100150618     o                       wTime            +   7 '  :  :  '
072200150619     o          e            PRTtxt      0  1
072300150720     o                                        +  25 '----------------------+
072400150720     o                                               -----------------------+
072500150618     o                                               -----------------'
072600150622      *
072700150622     o          e            PRTtxt2     2
072800150622     o                                        +  25 '*  SOLO STAMPA  *'
072900150720     o          e            PRTtxt2     0
073000150622     o                                        +  25 '*  SOLO STAMPA  *'
073100150720      *
073200150720     o          e            PRTmsg      2  1
073300150720     o                       wwwMSG
073400150618      *
073500150720     o          e            PRTtxt3     2
073600150618     o                                              'Richiesta     '
073700150618     o                                        +   1 '   '
073800150618     o                                        +   1 'Inserita da'
073900150618     o                                        +   1 ' il giorno'
074000150618     o                                        +   1 '  alle  '
074100150618     o                                        +   1 'Cliente'
074200150619     o                                        +   1 '                    -
074300150619     o                                                             '
074400150720     o                                        +   1 'Oggetto               '
074500150720     o*//                                     +   1 'Note                -
074600150720     o*//                                                          '
074700150622     o          e            PRTtxt3     1
074800150618     o                                              '=============='
074900150618     o                                        +   1 '==='
075000150618     o                                        +   1 '==========='
075100150618     o                                        +   1 '=========='
075200150618     o                                        +   1 '========'
075300150618     o                                        +   1 '======='
075400150619     o                                        +   1 '====================-
075500150619     o                                              ==============='
075600150720     o                                        +   1 '======================'
075700150720     o*//                                     +   1 '====================-
075800150720     o*//                                           ==============='
075900150619      *//*
076000150619     o*//       e            PRTnull     1
076100150618      *
076200150618     o          e            PRTdet      1
076300150619     o                       TGDnut        z
076400150619     o                       TGDant        z  +   1
076500150619     o                       TGDpdt        z  +   1
076600150619     o                       TGDpru           +   1
076700150618     o                       wwwDAC        y  +   2
076800150619     o                       TGDorc           +   1 '0 :  :  '
076900150619     o                       TGDksc        z  +   1
077000150619     o                       TGDrsc           +   1
077100150619     o                       TGDtor           +   1
077200150618     o                       wwwOGG           +   1
077300150720     o          e            PRTdet2     1
077400150720     o                                        +  19 'Note:'
077500150618     o                       wwwNOT1          +   1
077600150619     o                       wwwNOT2          +   1
077700150618      *
077800150618     o          e            PRTend      2
077900150619     o                                        +  25 '***  Fine Lista  ***'
078000150618     o          e            PRTend      0
078100150619     o                                        +  25 '***  Fine Lista  ***'
