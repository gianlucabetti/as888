000100150331       //==============================================================
000200150331       //?Invio e-mail a clienti in Campagna                           ?
000300150331       //==============================================================
000400150331
000500150331       //--------------------------------------------------------------
000600150331       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150331       //--------------------------------------------------------------
000800150331
000900150331     /*PRM  dbgview(*source)
001000150402     /*CMD  ovrprtf file(TRKC32P2) tofile(TRKC32P) ovrscope(*calllvl)
001100150402     /*END  dltovr file(TRKC32P2) lvl(*)
001200150331     /*END
001300150331
001400150331       //--------------------------------------------------------------
001500150331       //?Specifiche di controllo.                                     ?
001600150331       //--------------------------------------------------------------
001700150331
001800150331     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001900150331     h dftactgrp(*no) bnddir('TRUL')
002000150331     h alwnull(*inputonly)
002100150331
002200150331       //--------------------------------------------------------------
002300150331       //?Dichiarazione file.
002400150331       //--------------------------------------------------------------
002500150331
002600150402      *// // -?Anagrafica Commerciali?
002700150402     f*//AZCMM01L  if   e           k disk
002800150331       // -?Note/Rubrica Commerciali?
002900150402     fAZNTC01L  if   e           k disk    prefix(AZ)
003000150401       // -?Note Clienti?
003100150402     fTFNTC01L  if   e           k disk    prefix(TF)
003200150402
003300150402       // -?Fasi Avanzamento Clienti in Campagne commerciali?
003400150402     fTICMF01L  Uf   e           k disk    usropn
003500150331
003600150401       // -?Spool da inviare via e-mail: allegato per il cliente?
003700150402     fTRKC32P   o    e             printer usropn
003800150401       // -?Spool in stampa: allegato per il comm.le?
003900150402     fTRKC32P2  o    e             printer usropn
004000150402     f                                     rename( KC32CLI : P2_CLI )
004100150414     f                                     rename( KC32CMM : P2_CMM )
004200150402     f                                     rename( KC32OGG : P2_OGG )
004300150402     f                                     rename( KC32TXT : P2_TXT )
004400150402
004500150401       // -?Spool in stampa: Errori rilevati?
004600150402     fPrtF198   o    f  198        printer usropn  oflind(*inOE)
004700150331
004800150331       //--------------------------------------------------------------
004900150331       //?Definizione costanti.                                        ?
005000150331       //--------------------------------------------------------------
005100150331
005200150331       // -?Fase della Campagna comm.le da elaborare per i Clienti?
005300150331     d c_FaseDaElab    c                   const(' LT')
005400150401
005500150402       // -?Comandi di override al PrtF "allegato e-mail"?
005600150402     d c_CmdOvrPrtF_1  c                   const('OVRPRTF +
005700150402     d                                           file(TRKC32P) +
005800150402     d                                           ovrscope(*actgrpdfn) +
005900150402     d                                           ')
006000150402     d c_CmdDltOvr_1   c                   const('DLTOVR +
006100150402     d                                            file(TRKC32P) +
006200150402     d                                            lvl(*actgrpdfn)')
006300150402
006400150401       // -?Comandi di override al PrtF per i Commerciali?
006500150402     d c_CmdOvrPrtF_2  c                   const('OVRPRTF +
006600150402     d                                           file(TRKC32P2) +
006700150402     d                                           tofile(TRKC32P) +
006800150401     d                                           ovrscope(*actgrpdfn) +
006900150408     d                                           usrdta(''BRTACT_FCM'') +
007000150401     d                                           ')
007100150402     d c_CmdDltOvr_2   c                   const('DLTOVR +
007200150402     d                                            file(TRKC32P2) +
007300150401     d                                            lvl(*actgrpdfn)')
007400150401
007500150401       // -?Dati di default?
007600150401     d c_TipoStampa    c                   const('AUM')
007700150401     d c_OutQemail     c                   const('EMAILIN')
007800150401     d c_at            c                   const('@brt.it')
007900150403     d c_Oggetto       c                   const('Aggiornamento +
008000150403     d                                     condizioni tariffarie')
008100150403
008200150403     d c_CharMin       c                   const('abcdefghijklm+
008300150403     d                                            nopqrstuvwxyz+
008400150403     d                                            àáâãäå+
008500150403     d                                            æç+
008600150403     d                                            èéêë+
008700150403     d                                            ìíîï+
008800150403     d                                            òóôõö+
008900150403     d                                            ùúûü')
009000150403     d c_CharMai       c                   const('ABCDEFGHIJKLM+
009100150403     d                                            NOPQRSTUVWXYZ+
009200150403     d                                            ÀÁÂÃÄÅ+
009300150403     d                                            ÆÇ+
009400150403     d                                            ÈÉÊË+
009500150403     d                                            ÌÍÎÏ+
009600150403     d                                            ÒÓÔÕÖ+
009700150403     d                                            ÙÚÛÜ')
009800150331
009900150331       //--------------------------------------------------------------
010000150331       //?Definizione schiere.                                         ?
010100150331       //--------------------------------------------------------------
010200150331
010300150402       // -?Testo della lettera allegata e-mail?
010400150401     d sk_Alleg        s             95    dim(19)  ctdata  perrcd(1)
010500150331
010600150331       //--------------------------------------------------------------
010700150331       //?Definizione aree dati.                                       ?
010800150331       //--------------------------------------------------------------
010900150331
011000150331       // -?Dati utente?
011100150331     d §AzUte        e ds                  extname(AZUTE00F)
011200150331     d                                     dtaara
011300150331     d §DatiUte      e ds                  extname(dDatiUte)
011400150331     d                                     dtaara
011500150331
011600150331       //--------------------------------------------------------------
011700150331       //?Definizione strutture dati.                                  ?
011800150331       //--------------------------------------------------------------
011900150331
012000150331       // -?Status?
012100150331     d Status         sds
012200150331     d   SDSpgm          *proc
012300150331     d*//SDSprm          *parms
012400150331     d*//SDSdta              191    198
012500150331     d   SDSjobName          244    253
012600150331     d   SDSjobUser          254    263
012700150331     d   SDSjobNbr           264    269s 0
012800150331
012900150331       // -?Dati estratti via SQL?
013000150401     d TICMI00F      e ds                  based(nullPtr)
013100150331     d wSQL_ds         ds                  qualified     inz
013200150331     d   wNCM                              like(CMFncm)  inz
013300150331     d   wKSU                              like(CMFksu)  inz
013400150331     d   wKSC                              like(CMFksc)  inz
013500150331     d   wCPO                              like(CMFcpo)  inz
013600150331     d   wACM                              like(CMFacm)  inz
013700150403     d   wDFA                              like(CMFdfa)  inz
013800150401     d   wPEA                              like(CMFpea)  inz
013900150331     d   wFLO                              like(CMFflo)  inz
014000150331     d   wCMM                              like(CMIcmm)  inz
014100150331     d   wFCM                              like(CMIfcm)  inz
014200150331
014300150331       // -?Data/Ora attuali?
014400150331     d wTime_ds        ds                  inz
014500150331     d   wDate                        8s 0 inz
014600150331     d   wTime                        6s 0 inz
014700150331
014800150331       // -?Parametri ricevuti?
014900150331     d KPJBA         e ds
015000150331     d TRKC32ds      e ds
015100150402
015200150403      *// // -?Tab. "MRA" = Bart-Mailing x TRKC32R?
015300150403     d*// dMRA          e ds                  inz
015400150401
015500150401       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
015600150401     d TRTCM1ds      e ds                  inz
015700150401       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
015800150401     d   §CM1mitt    e                     inz('ced')
015900150401       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
016000150401     d   §CM1dst     e                     inz('stefano.merighi+
016100150401     d                                          @brt.it')
016200150401       //  ?·§CM1tips = Tipo lettera via e-mail:?
016300150401       //   ?"LET" = PDF lettera: testo allegato in corpo con logo?
016400150401       //           ?(richiede righe libere iniziali per il logo)?
016500150401       //   ?"COR" = Spool in corpo mail: testo integrato senza logo?
016600150401       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
016700150401       //   ?"S01" = Standard vuoto?
016800150401       //   ?"SP1" = spool allegato (in formato PDF)?
016900150401     d   §CM1tips    e                     inz('AUM')
017000150401       //  ?·§CM1po   = Filiale?
017100150401     d   §CM1po      e                     inz('046')
017200150401       //  ?·§CM1var  = Oggetto e-mail?
017300150401     d   §CM1var     e                     inz('*OBJM*+
017400150401     d                                     Aggiornamento condizioni +
017500150401     d                                     tariffarie')
017600150401       //  ?·§CM1sts  = Stato spool?
017700150401     d   §CM1sts     e                     inz('H')
017800150401       //  ?·§CM1sts  = Id processo?
017900150401     d   §CM1idp     e                     inz('*')
018000150401       //  ?·§CM1att  = Numero attach corrente?
018100150401     d   §CM1att     e                     inz('01')
018200150401       //  ?·§CM1totatt  = Numero attach totali?
018300150401     d   §CM1totatt  e                     inz('02')
018400150331
018500150331       //--------------------------------------------------------------
018600150331       //?Definizione variabili globali.                               ?
018700150331       //--------------------------------------------------------------
018800150331
018900150331       // -?Flags booleani?
019000150331     d $EoF            s               n   inz
019100150401     d $First          s               n   inz(*on)
019200150401
019300150401       // -?Indici di schiera?
019400150401     d xx              s              3  0 inz
019500150331
019600150331       // -?Stringa SQL da eseguire?
019700150331     d wSQL            s           7000    inz  varying
019800150331
019900150331       // -?Codice errore rilevato?
020000150331     d wErr            s              3  0 inz
020100150331
020200150331       // -?Campi di controllo "rotture"?
020300150331     d wSaveCMM        s                   inz  like(CMIcmm)
020400150401     d wSaveKSU        s                   inz  like(CMIksu)
020500150401
020600150401       // -?Campi di Stampa?
020700150401     d o_PEA           s              2  1 inz
020800150331
020900150401       // -?Campi Data?
021000150331     d wDate_Eur       s               d   inz  datfmt(*EUR)
021100150331     d wOggi           s              8s 0 inz
021200150331
021300150331       //--------------------------------------------------------------
021400150331       //?Definizione prototipi procedure.                             ?
021500150331       //--------------------------------------------------------------
021600150331
021700150331       // -?Reperimento dati utente?
021800150331     d TIBS34ds      e ds                  inz
021900150331      /copy gaitrasrc/srcProtoPR,TIBS34R
022000150401
022100150401       // -?Controllo/Decodifica cliente?
022200150401      /copy gaitrasrc/srcProtoPI,TIBS69R
022300150401      /copy gaitrasrc/srcProtoPR,TIBS69R
022400150331
022500150403      *// // -?Reperimento dati tabelle?
022600150403      *///copy gaitrasrc/srcProtoPI,TRULTAB
022700150403      *///copy gaitrasrc/srcProtoPR,TRULTAB
022800150401
022900150403       // -?Reperimento USRDFNDTA x comunicazioni via e-mail?
023000150403     d TRUL44ds      e ds                  inz
023100150403     d trul44R         pr                  extpgm('TRUL44R')
023200150403     d   trul44ds                          likeds(TRUL44ds)
023300150403     d   trtcM1ds                          likeds(TRTCM1ds)
023400150331
023500150331       // -?API QCAPCMD (Process Commands)?
023600150331     d Qcmd            s           2048    inz  varying
023700150331      /copy qSysInc/qRpgleSrc,QCAPCMD
023800150331      /copy gaitrasrc/srcProtoPR,QCAPCMD
023900150331
024000150331       // -?Parametri gestione errori API.?
024100150331      /copy qsysinc/qrpglesrc,QUSEC
024200150331
024300150331       //--------------------------------------------------------------
024400150331       //?Definizione key-list.                                        ?
024500150331       //--------------------------------------------------------------
024600150331
024700150402      *// // -?File AZCMM01L?
024800150402     d*// keyAZCMM01    e ds                  extname( AZCMM01L : *key )
024900150402     d*//                                     prefix(k_)  inz
025000150331
025100150331       // -?File AZNTC01L?
025200150331     d keyAZNTC01    e ds                  extname( AZNTC01L : *key )
025300150331     d                                     prefix(k_AZ)  inz
025400150331
025500150331       // -?File TFNTC01L?
025600150331     d keyTFNTC01    e ds                  extname( TFNTC01L : *key )
025700150331     d                                     prefix(k_TF)  inz
025800150402
025900150402       // -?File TICMF01L?
026000150402     d keyTICMF01    e ds                  extname( TICMF01L : *key )
026100150402     d                                     prefix(k_)    inz
026200150331
026300150331       //--------------------------------------------------------------
026400150331       //?M A I N - L I N E                                            ?
026500150331       //--------------------------------------------------------------
026600150331
026700150331     c     *Entry        plist
026800150331     c                   parm                    KPJBA
026900150331
027000150331      /free
027100150331
027200150331       // -?Operazioni iniziali?
027300150331       exsr  sr_RoutInz;
027400150331
027500150331       // -?Preparazione SQL per estrazione dati da TICMF00F e TICMI00F?
027600150331       exsr  sr_PrepSQL;
027700150331
027800150331       // -?Ciclo di elaborazione file TICMF00F e TICMI00F?
027900150331       exsr  sr_OpenCursor;
028000150331
028100150331       DoW  Not $EoF;
028200150331         exsr  sr_ReadCursor;
028300150331       EndDo;
028400150331
028500150331       exsr  sr_CloseCursor;
028600150331
028700150331       // -?Operazioni finali?
028800150331       exsr  sr_RoutEnd;
028900150331
029000150331       //--------------------------------------------------------------
029100150331       //?Operazioni iniziali.                                         ?
029200150331       //--------------------------------------------------------------
029300150331       BEGSR  sr_RoutInz;
029400150331
029500150331         // -?Impostazione opzioni per SQL?
029600150331         exec sql   set  option  DynUsrPrf = *Owner,
029700150331                                 CloSqlCsr = *EndMod;
029800150331
029900150331         // -?Impostazione chiusura?
030000150331         *inLR = *on;
030100150331
030200150331         // -?Impostazione parametri ricevuti?
030300150331         TRKC32ds = kpjbu;
030400150331
030500150331         // -?Reperimento dati del lavoro?
030600150331         exsr  sr_DatiJob;
030700150402
030800150402         // -?Reperimento tab. "MRA"?
030900150403         //clear  dMRA;
031000150403         //if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
031100150403         //                 *omit : *omit :
031200150403         //                 *omit : *omit :
031300150403         //                 *omit : *omit : *omit : *omit :
031400150403         //                 *omit : *omit : *omit : *omit :
031500150403         //                 ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
031600150403         //                 = *zero      AND
031700150403         //      ds_TNTBE.TBEatb = *blank;
031800150403         //  dMRA = ds_TNTBE.TBEuni;
031900150403         //endif;
032000150331
032100150331         // -?Reperimento data e ora attuali?
032200150331         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
032300150331         wDate_Eur = %date( wDate : *Iso );
032400150331         wOggi     = %dec( wDate_Eur );
032500150402
032600150402         // -?Apertura *file TICMF00F per aggiornamento flag?
032700150402         //  ?"E-Mail inviata"?
032800150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
032900150402           open  TICMF01L;
033000150402         endif;
033100150331
033200150331       ENDSR;
033300150331
033400150331       //--------------------------------------------------------------
033500150331       //?Reperimento Dati del job (Utente/Operativi).                 ?
033600150331       //--------------------------------------------------------------
033700150331       BEGSR  sr_DatiJob;
033800150331
033900150331         in(E) §AzUte;
034000150331         if NOT %error;
034100150331           in(E) §DatiUte;
034200150331         endif;
034300150331         if %error or RSut = *blanks;
034400150331           clear TIBS34ds;
034500150331           tibs34r(tibs34ds);
034600150331           in §AzUte;
034700150331           in §DatiUte;
034800150331         endif;
034900150331
035000150331       ENDSR;
035100150331
035200150331       //--------------------------------------------------------------
035300150331       //?Preparazione SQL.                                            ?
035400150331       //--------------------------------------------------------------
035500150331       BEGSR  sr_PrepSQL;
035600150331
035700150331         // -?Preparazione SQL?
035800150331         clear  wSQL;
035900150331         wSQL = 'with +
036000150331                      TICMF as +
036100150331                      ( select CMFncm, CMFksu, CMFksc, CMFcpo, +
036200150403                               CMFacm, CMFdfa, CMFpea, CMFflo +
036300150331                          from TICMF00F +
036400150401                         where CMFacm = ''' + c_FaseDaElab + '''';
036500150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
036600150401           wSQL +=       ' and substr( CMFflo, 1, 1 ) = '' ''';
036700150331         else;
036800150401           wSQL +=       ' and substr( CMFflo, 1, 1 ) <> '' ''';
036900150331         endif;
037000150331         if  iKC32ncm > *zero;
037100150331           wSQL +=       ' and CMFncm = ' + %trim( %editc( iKC32ncm : '3' ) );
037200150331         endif;
037300150331         wSQL +=       ' order by CMFncm, CMFksu, CMFksc, CMFcpo ) +
037400150331
037500150331                 select TICMF.*, CMIcmm, CMIfcm +
037600150331                   from TICMF inner join TICMI00F +
037700150331                     on CMFncm = CMIncm +
037800150331                    and CMFksu = CMIksu +
037900150331                    and CMFksc = CMIksc +
038000150331                    and CMFcpo = CMIcpo ';
038100150331         $First = *on;
038200150331         if  iKC32cmm > *zero;
038300150331           if  $First;
038400150331             wSQL  += 'where';
038500150331             $First = *off;
038600150331           else;
038700150331             wSQL  += 'and';
038800150331           endif;
038900150331           wSQL +=    ' CMIcmm = ' + %trim( %editc( iKC32cmm : '3' ) );
039000150331         endif;
039100150331         if  iKC32fcm > *zero;
039200150331           if  $First;
039300150331             wSQL += 'where';
039400150331             $First = *off;
039500150331           else;
039600150331             wSQL += 'and';
039700150331           endif;
039800150331           wSQL +=    ' CMIfcm = ' + %trim( %editc( iKC32fcm : '3' ) );
039900150331           $First = *off;
040000150331         endif;
040100150331         wSQL += ' +
040200150331                  order by CMIfcm, CMIcmm, +
040300150331                           CMFncm, CMFksu, CMFksc, CMFcpo +
040400150331                    for fetch only';
040500150331
040600150331         exec sql   prepare S1   from :wSQL;
040700150331
040800150331         // -?Dichiarazione cursore?
040900150331         exec sql   declare C1   cursor for S1;
041000150331
041100150331         if  SQLcode < *zero;
041200150331           $EoF = *on;
041300150331           wErr = 1;
041400150331           exsr  sr_PrintErr;
041500150401           exsr  sr_RoutEnd;
041600150331         endif;
041700150331
041800150331       ENDSR;
041900150331
042000150331       //--------------------------------------------------------------
042100150331       //?Apertura cursore.                                            ?
042200150331       //--------------------------------------------------------------
042300150331       BEGSR  sr_OpenCursor;
042400150331
042500150331         // -?Apertura del cursore?
042600150331         exec sql   open C1;
042700150331
042800150331         if  SQLcode < *zero;
042900150331           $EoF = *on;
043000150331           wErr = 1;
043100150331           exsr  sr_PrintErr;
043200150401           exsr  sr_RoutEnd;
043300150331         endif;
043400150331
043500150331       ENDSR;
043600150331
043700150331       //--------------------------------------------------------------
043800150331       //?Lettura cursore.                                             ?
043900150331       //--------------------------------------------------------------
044000150331       BEGSR  sr_ReadCursor;
044100150331
044200150331         clear  wErr;
044300150331         clear  wSQL_ds;
044400150331
044500150331         exec sql   fetch next   from C1   into :wSQL_ds;
044600150331
044700150331         Select;
044800150331
044900150331           // -?Fine File?
045000150331           When  SQLcode = 100;
045100150331             $EoF = *on;
045200150331
045300150331           // -?Errore SQL?
045400150331           When  SQLcode < *zero;
045500150331             $EoF = *on;
045600150331             wErr = 1;
045700150331             exsr  sr_PrintErr;
045800150401             exsr  sr_RoutEnd;
045900150331
046000150331           // -?Elaborazione singolo cliente?
046100150331           Other;
046200150331             exsr  sr_ElabRec;
046300150331
046400150331         EndSl;
046500150331
046600150331       ENDSR;
046700150331
046800150331       //--------------------------------------------------------------
046900150331       //?Chiusura cursore.                                            ?
047000150331       //--------------------------------------------------------------
047100150331       BEGSR  sr_CloseCursor;
047200150331
047300150331         // -?Chiusura del cursore?
047400150331         exec sql   close C1;
047500150331
047600150331       ENDSR;
047700150331
047800150331       //--------------------------------------------------------------
047900150331       //?Elaborazione singolo Cliente a cui inviare la e-Mail.        ?
048000150331       //--------------------------------------------------------------
048100150331       BEGSR  sr_ElabRec;
048200150331
048300150331         // -?Reperimento dati del nuovo commerciale?
048400150402         If  wSQL_ds.wCMM <> wSaveCMM;
048500150401
048600150414           // -?Apertura file di stampa x comm.le?
048700150414           If  iKC32fsr = *blank  or  iKC32fsr = 'R';
048800150414             if  %open(TRKC32P2);
048900150414               close  TRKC32P2;
049000150414               Qcmd = c_CmdDltOvr_2;
049100150414               exsr  sr_ExecCmd;
049200150414             endif;
049300150414             Qcmd = %replace( %subst( %editc( wSQL_ds.wCMM : 'X' ) : 4 : 4 ) :
049400150414                              c_CmdOvrPrtF_2 :
049500150414                              %scan( '_FCM' : c_CmdOvrPrtF_2 ) ) +
049600150414                   'outq(R' + %editc( wSQL_ds.wFCM : 'X' ) + 'SEDE)';
049700150414             exsr  sr_ExecCmd;
049800150414             open  TRKC32P2;
049900150414           EndIf;
050000150331
050100150402           //// -?Controllo validità in AZCMM00F?
050200150402           //clear  AZNTCrnt;
050300150402           //clear  keyAZCMM01;
050400150402           //k_CMMcod = wSQL_ds.wCMM;
050500150402           //chain  %kds( keyAZCMM01 )  AZCMM000;
050600150402           //
050700150402           //if  Not %found(AZCMM01L)  or
050800150402           //    CMMpar  <> *blank     or
050900150402           //    CMMdtIni > wDate      or
051000150402           //    CMMdtFin < wDate;
051100150402           //  wErr = 2;
051200150402           //  exsr  sr_PrintErr;
051300150402           //  leavesr;
051400150402           //endif;
051500150402
051600150402           // -?Reperimento indirizzo e-mail da AZNTC00F?
051700150331           clear  keyAZNTC01;
051800150402           k_AZNTCcmm = wSQL_ds.wCMM;
051900150331           k_AZNTCtnt = '02';
052000150331           chain  %kds( keyAZNTC01 )  AZNTC000;
052100150331
052200150331           if  Not %found(AZNTC01L);
052300150331             wErr = 2;
052400150331             exsr  sr_PrintErr;
052500150331             leavesr;
052600150331           endif;
052700150402
052800150402           wSaveCMM = wSQL_ds.wCMM;
052900150331
053000150331         EndIf;
053100150331
053200150331         // -?Reperimento dati del responsabile trasporti (cliente)?
053300150331         If  wSQL_ds.wKSU <> wSaveKSU;
053400150331
053500150331           clear  TFNTCrnt;
053600150331           clear  keyTFNTC01;
053700150331           k_TFNTCapl = 'C';
053800150401           k_TFNTCnk1 = %editc( DUTkci : 'X' ) +
053900150401                        %editc( wSQL_ds.wKSU : 'X' );
054000150331           // -?1° tentativo: responsabile trasporti?
054100150331           k_TFNTCtnt = '06';
054200150331           chain  %kds( keyTFNTC01 )  TFNTC;
054300150331
054400150331           if  Not %found(TFNTC01L);
054500150331             // -?2° tentativo: invio fattura?
054600150331             k_TFNTCtnt = '84';
054700150331             chain  %kds( keyTFNTC01 )  TFNTC;
054800150331
054900150331             if  Not %found(TFNTC01L);
055000150331               // -?Ultima soluzione: e-mail del comm.le?
055100150331               TFNTCrnt = %trim( AZNTCrnt ) + c_at;
055200150331             endif;
055300150331
055400150331           endif;
055500150401
055600150401           // -?Decodifica Cliente Unificante?
055700150401           clear  TIBS69ds;
055800150401           I69sif = knsif;
055900150401           I69kcc = DUTkci;
056000150401           I69kac = wSQL_ds.wKSU;
056100150402           I69kin = I69kac;
056200150401           tibs69r( TIBS69ds :
056300150401                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
056400150331
056500150331           wSaveKSU = wSQL_ds.wKSU;
056600150331
056700150331         EndIf;
056800150331
056900150402         // -?Stampa x comm.le  ed  Invio e-mail al cliente?
057000150331         exsr  sr_SendEmail;
057100150331
057200150331       ENDSR;
057300150331
057400150331       //--------------------------------------------------------------
057500150331       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
057600150331       //--------------------------------------------------------------
057700150331       BEGSR  sr_SendEmail;
057800150401
057900150402         // -?Override al file di stampa (per impostarvi i dati per?
058000150402         //  ?l'invio via e-mail) ed Apertura dello stesso?
058100150402         //  ?(SE Stampa)?
058200150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
058300150401           exsr sr_Override;
058400150402           open  TRKC32P;
058500150401         endif;
058600150402
058700150331
058800150414         // -?Preparazione spool per il singolo cliente (ed il comm.le)?
058900150402         pKC32rsc = ACOrag;
059000150402         pKC32ind = INDvia;
059100150402         pKC32cap = INDcae;
059200150407         pKC32loc = %trimR( INDcit ) + '  ' + INDprv;
059300150414
059400150414         pKC32cmm = wSQL_ds.wCMM;
059500150403
059600150403         wDate_Eur = %date( wSQL_ds.wDFA : *Iso );
059700150403         pKC32dat = 'BOLOGNA, li ' +
059800150403                    %trimL( %editc( %dec( wDate_Eur ) : 'Y' ) );
059900150403         pKC32ogg = 'OGGETTO: ' +
060000150403                    %xlate( c_CharMin : c_CharMai : c_Oggetto : 1 ) +
060100150403                    ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
060200150402
060300150414         if  iKC32fsr = *blank  or  iKC32fsr = 'R';
060400150414           write  P2_cli;
060500150414           write  P2_cmm;
060600150414           write  P2_ogg;
060700150414         endif;
060800150402
060900150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
061000150402           write  KC32cli;
061100150402           write  KC32ogg;
061200150402         endif;
061300150402
061400150401         For  xx = 1  To  %elem( sk_Alleg );
061500150401
061600150402           pKC32txt = sk_Alleg(xx);
061700150401
061800150401           if  xx = 9;
061900150401             o_PEA   = wSQL_ds.wPEA;
062000150402             pKC32txt = %trim( %replace( %editc( o_PEA : '3' ) :
062100150402                                         pKC32txt :
062200150402                                         %scan( '_,_' : pKC32txt ) ) );
062300150401           endif;
062400150401
062500150414           if  iKC32fsr = *blank  or  iKC32fsr = 'R';
062600150414             write  P2_txt;
062700150414           endif;
062800150414           if  iKC32fsr = *blank  or  iKC32fsr = 'M';
062900150402             write  KC32txt;
063000150402           endif;
063100150401
063200150401         EndFor;
063300150401
063400150402
063500150402         // -?Chiusura file di stampa ed invio e-mail?
063600150402         //  ?Rimozione Override al file di stampa per e-mail?
063700150402         //  ?(SE Stampa)?
063800150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
063900150402           close  TRKC32p;
064000150402           Qcmd = c_CmdDltOvr_1;
064100150402           exsr  sr_ExecCmd;
064200150402         endif;
064300150402
064400150402
064500150402         // -?Aggiornamento TICMF00F.CMFFLO?
064600150402         //  ?(SE Stampa)?
064700150414         If  iKC32fsr = *blank  or  iKC32fsr = 'M';
064800150402
064900150402           clear  keyTICMF01;
065000150402           k_CMFncm = wSQL_ds.wNCM;
065100150402           k_CMFksu = wSQL_ds.wKSU;
065200150402           k_CMFksc = wSQL_ds.wKSC;
065300150402           k_CMFcpo = wSQL_ds.wCPO;
065400150402           k_CMFacm = wSQL_ds.wACM;
065500150402           chain  %kds( keyTICMF01 : 5 )  TICMF000;
065600150402
065700150402           if  %found(TICMF01L);
065800150402             %subst( CMFflo : 1 : 1 ) = 'S';
065900150402             //_______________
066000150402             update  TICMF000;
066100150402             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
066200150402           endif;
066300150402
066400150402         EndIf;
066500150331
066600150331       ENDSR;
066700150401
066800150401       //--------------------------------------------------------------
066900150401       //?Override al file di Stampa per generarvi una e-mail.         ?
067000150401       //--------------------------------------------------------------
067100150401       BEGSR  sr_Override;
067200150401
067300150401         // -?Impostazione regole per mailing?
067400150401         clear  TRTCM1ds;
067500150401         §CM1mitt = %trim(AZNTCrnt);
067600150401         §CM1dst  = %trim(TFNTCrnt);
067700150401         §CM1tips = c_TipoStampa;
067800150401         §CM1po   = %editc( wSQL_ds.wFCM : 'X' );
067900150407         §CM1var  = '*OBJM*' + c_Oggetto +
068000150407                    ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
068100150402         §CM1sts  = *off;
068200150403         //§CM1idp  = §MRAidPro;
068300150403         §CM1idp  = '*';
068400150401
068500150403         clear  TRUL44ds;
068600150403         D44pgm = SDSpgm;
068700150403         D44po  = wSQL_ds.wFCM;
068800150403         D44des = %trim(TFNTCrnt);
068900150403
069000150403         trul44r ( TRUL44ds : TRTCM1ds );
069100150401
069200150401         // -?Override all'"allegato e-mail"?
069300150403         if  D44err = *blank;
069400150403           Qcmd = c_CmdOvrPrtF_1
069500150403                + 'outq(' + %trim( D44outQ ) + ') '
069600150403                + 'usrdfndta(''' + D44dta + ''')';
069700150403         else;
069800150403           wErr = 3;
069900150403           exsr  sr_PrintErr;
070000150403           Qcmd = c_CmdOvrPrtF_1;
070100150403         endif;
070200150402
070300150403         //if  §MRAoutQi <> *blank;
070400150403         //  Qcmd = c_CmdOvrPrtF_1 +
070500150403         //        'outq(' + %trim( §MRAoutQi ) + ') +
070600150403         //         usrdfndta(''' + TRTCM1ds + ''')';
070700150403         //else;
070800150403         //  Qcmd = c_CmdOvrPrtF_1;
070900150403         //endif;
071000150401
071100150401         exsr  sr_ExecCmd;
071200150401
071300150401         if  Qusei <> *blank;
071400150401           $EoF = *on;
071500150401           wErr = 3;
071600150401           exsr  sr_PrintErr;
071700150401           exsr  sr_RoutEnd;
071800150401         endif;
071900150401
072000150401       ENDSR;
072100150401
072200150401       //--------------------------------------------------------------
072300150401       //?Stampa segnalazione dell'errore rilevato (non solo da SQL).  ?
072400150401       //--------------------------------------------------------------
072500150401       BEGSR  sr_PrintErr;
072600150401
072700150401         // -?Stampa errore rilevato?
072800150401         if  Not %open(PrtF198);
072900150401           open  PrtF198;
073000150401           *inOE = *on;
073100150401         endif;
073200150401
073300150401         if  *inOE;
073400150401           except  PRTerrTXT;
073500150402           *inOE = *off;
073600150401         endif;
073700150401
073800150401         select;
073900150401           when  wErr = 1;
074000150401             except  PRTerr01;
074100150408             // -?Stampa del Dump?
074200150408             Dump(A);
074300150408             // -?Stampa del Job-Log?
074400150408             Qcmd = 'DSPJOBLOG job(*) output(*print)';
074500150408             exsr  sr_ExecCmd;
074600150401           when  wErr = 2;
074700150401             except  PRTerr02;
074800150401           when  wErr = 3;
074900150401             except  PRTerr03;
075000150401         endsl;
075100150401
075200150401       ENDSR;
075300150331
075400150331       //--------------------------------------------------------------
075500150331       //?Esecuzione del comando (già impostato).                      ?
075600150331       //--------------------------------------------------------------
075700150331       BEGSR  sr_ExecCmd;
075800150331
075900150331         clear Qcap0100;
076000150331         Qcabcsdh = *off;
076100150331         Qcapa    = *off;
076200150331         Qcacmdss = *off;
076300150331         Qcaerved = *allX'00';
076400150331
076500150331         clear Qusec;
076600150331         Qusbprv  = %size(Qusec);
076700150331
076800150331         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
076900150331                           %size(Qcap0100) : 'CPOP0100' : *omit :
077000150331                           0 : 0 : Qusec);
077100150331
077200150331         //if  Qusei <> *blank;
077300150331         //  ...;
077400150331         //endif;
077500150331
077600150331       ENDSR;
077700150331
077800150331       //--------------------------------------------------------------
077900150331       //?Operazioni finali.                                           ?
078000150331       //--------------------------------------------------------------
078100150331       BEGSR  sr_RoutEnd;
078200150402
078300150402         // -?Chiusura *file TICMF00F (se in aggiornamento)?
078400150402         if  %open(TICMF01L);
078500150402           close  TICMF01L;
078600150402         endif;
078700150331
078800150401         // -?Chiusura delle funzioni precedentemente aperte?
078900150403         //cloTabella();
079000150401
079100150401         // -?Chiusura dei file di stampa per le e-mail?
079200150402         if  %open(TRKC32P);
079300150402           close  TRKC32P;
079400150402           Qcmd = c_CmdDltOvr_1;
079500150401           exsr  sr_ExecCmd;
079600150401         endif;
079700150401
079800150401         // -?Chiusura dei file di stampa per i comm.li?
079900150402         if  %open(TRKC32P2);
080000150402           close  TRKC32P2;
080100150402           Qcmd = c_CmdDltOvr_2;
080200150401           exsr  sr_ExecCmd;
080300150401         endif;
080400150401
080500150401         // -?SE stampato almeno un Errore:?
080600150401         //  ?stampa di "fine" e chiusura del file di stampa?
080700150401         if  %open(PrtF198);
080800150401           except  PRTerrEND;
080900150401           close  PrtF198;
081000150401         endif;
081100150331
081200150331         // -?Uscita dal *pgm?
081300150331         return;
081400150331
081500150331       ENDSR;
081600150331
081700150331      /end-free
081800150402
081900150402       //--------------------------------------------------------------
082000150402       //?S P O O L - F I L E S                                        ?
082100150402       //--------------------------------------------------------------
082200150401
082300150401       // -?Stampa ERRORI?
082400150331
082500150401     oPrtF198   e            PRTerrTXT         3
082600150331     o                       RSUT
082700150401     o                                        +   5 'ERRORI IN INVIO +
082800150401     o                                               E-MAIL A CLIENTI +
082900150401     o                                               IN CAMPAGNA'
083000150331     o                       SDSpgm           +   5
083100150402     o                       wOggi         y  +   5
083200150401     o          e            PRTerrTXT   1  1
083300150331     o                       KNSIF
083400150331     o                       KNMUS            +   1
083500150401     o                                        +   4 '----------------+
083600150401     o                                               -----------------+
083700150401     o                                               -----------'
083800150331     o                                        +   5 'Pag.'
083900150331     o                       Page          z  +   0
084000150331     o                       wTime            +   7 '  :  :  '
084100150331      *
084200150401     o          e            PRTerr01    2
084300150401     o                                              'Rilevato ERRORE n-
084400150401     o                                              ella selezione dei-
084500150401     o                                               clienti in Campag-
084600150401     o                                              a Aumenti Tariffar-
084700150401     o                                              i.'
084800150331     o          e            PRTerr01    2
084900150331     o                                              'Consultare le sta-
085000150331     o                                              mpe del lavoro'
085100150331     o
085200150331     o                       SDSjobNbr     3  +   1
085300150331     o                                        +   0 '/'
085400150331     o                       SDSjobUser       +   0
085500150331     o                                        +   0 '/'
085600150331     o                       SDSjobName       +   0
085700150401     o                                        +   0 '.'
085800150331      *
085900150401     o          e            PRTerr02    2
086000150331     o                                              'Campagna'
086100150401     o                       wSQL_ds.wNCM  2  +   1
086200150331     o                                        +   2 'Unificante'
086300150331     o                       wSQL_ds.wKSU     +   1
086400150401     o                                        +   2 'Commerciale'
086500150401     o                       wSQL_ds.WCMM     +   1
086600150401     o                                        +   2 '* Commerciale NON-
086700150401     o                                               attivo'
086800150401      *
086900150401     o          e            PRTerr03    2
087000150401     o                                              'Campagna'
087100150401     o                       wSQL_ds.wNCM  2  +   1
087200150401     o                                        +   2 'Unificante'
087300150401     o                       wSQL_ds.wKSU     +   1
087400150401     o                                        +   2 'Commerciale'
087500150401     o                       wSQL_ds.WCMM     +   1
087600150401     o                                        +   2 '* Fallita prepara-
087700150401     o                                              zione del file di -
087800150401     o                                              stampa da inviare -
087900150401     o                                              via e-mail al clie-
088000150401     o                                              nte.'
088100150331      *
088200150401     o          e            PRTerrEND   2
088300150331     o                                        +  10 '***  Fine Lista  ***'
088400150401
088500150401       //--------------------------------------------------------------
088600150401       //?Schiere a tempo di compilazione.                             ?
088700150401       //--------------------------------------------------------------
088800150401
088900150401** --?sk_Alleg:?Testo allegato e-mail?--------------------------------------------------------*
089000150401Gentile Cliente,                                                                                 1
089100150401                                                                                                 2
089200150401il nostro impegno quotidiano  ed i nostri ingenti investimenti  in strutture  e sistemi di       3
089300150401automazione, volti all'aumento dell'efficienza e della produttività, non sono oggettivamente     4
089400150401in grado di ammortizzare il continuo e notevole aumento dei costi operativi e del costo del      5
089500150401lavoro registrati nel nostro settore negli ultimi 12 mesi.                                       6
089600150401                                                                                                 7
089700150401Siamo quindi costretti a rivedere le tariffe in essere anticipandoLe che dal 1° Maggio p.v.      8
089800150401applicheremo una maggiorazione del _,_%.                                                         9
089900150401Tale adeguamento tariffario sarà effettuato sotto forma di addizionale di gestione.             10
090000150401                                                                                                11
090100150407Siamo, come sempre, a Sua completa disposizione per fornirLe opporture spiegazioni e, nel       12
090200150401ringraziarLa per la fiducia che Lei, insieme con altri numerosi Clienti, ci rinnova giorno      13
090300150401dopo giorno, cogliamo l'occasione per porgere i nostri più cordiali saluti.                     14
090400150401                                                                                                15
090500150401                                                                                                16
090600150401                                                                                                17
090700150401                                                           BRT SpA                              18
090800150401                                                      Direzione Generale                        19
