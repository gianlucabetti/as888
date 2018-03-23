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
007600161128     d c_at            c                   const('@brt.it')
007700161128
007800161128     d c_TipoStampa...
007900161128     d                 c                   const('AUM')
008000161123     d c_TipoStampa_Estero...
008100161123     d                 c                   const('AUE')
008200161128     d c_Oggetto...
008300161128     d                 c                   const('Aggiornamento +
008400150403     d                                     condizioni tariffarie')
008500161124     d c_Oggetto_Eng...
008600171019     d                 c                   const('Tariff conditions +
008700171019     d                                     update')
008800150403
008900150403     d c_CharMin       c                   const('abcdefghijklm+
009000150403     d                                            nopqrstuvwxyz+
009100150403     d                                            àáâãäå+
009200150403     d                                            æç+
009300150403     d                                            èéêë+
009400150403     d                                            ìíîï+
009500150403     d                                            òóôõö+
009600150403     d                                            ùúûü')
009700150403     d c_CharMai       c                   const('ABCDEFGHIJKLM+
009800150403     d                                            NOPQRSTUVWXYZ+
009900150403     d                                            ÀÁÂÃÄÅ+
010000150403     d                                            ÆÇ+
010100150403     d                                            ÈÉÊË+
010200150403     d                                            ÌÍÎÏ+
010300150403     d                                            ÒÓÔÕÖ+
010400150403     d                                            ÙÚÛÜ')
010500150331
010600150331       //--------------------------------------------------------------
010700150331       //?Definizione schiere.                                         ?
010800150331       //--------------------------------------------------------------
010900150331
011000150402       // -?Testo della lettera allegata e-mail?
011100171018     d sk_Alleg        s             95    dim(25)  ctdata  perrcd(1)
011200171019     d sk_Alleg_Eng    s             95    dim(25)  ctdata  perrcd(1)
011300150331
011400150331       //--------------------------------------------------------------
011500150331       //?Definizione aree dati.                                       ?
011600150331       //--------------------------------------------------------------
011700150331
011800150331       // -?Dati utente?
011900150331     d §AzUte        e ds                  extname(AZUTE00F)
012000150331     d                                     dtaara
012100150331     d §DatiUte      e ds                  extname(dDatiUte)
012200150331     d                                     dtaara
012300150331
012400150331       //--------------------------------------------------------------
012500150331       //?Definizione strutture dati.                                  ?
012600150331       //--------------------------------------------------------------
012700150331
012800150331       // -?Status?
012900150331     d Status         sds
013000150331     d   SDSpgm          *proc
013100150331     d*//SDSprm          *parms
013200150331     d*//SDSdta              191    198
013300150331     d   SDSjobName          244    253
013400150331     d   SDSjobUser          254    263
013500150331     d   SDSjobNbr           264    269s 0
013600150331
013700150331       // -?Dati estratti via SQL?
013800150401     d TICMI00F      e ds                  based(nullPtr)
013900150331     d wSQL_ds         ds                  qualified     inz
014000150331     d   wNCM                              like(CMFncm)  inz
014100150331     d   wKSU                              like(CMFksu)  inz
014200150331     d   wKSC                              like(CMFksc)  inz
014300150331     d   wCPO                              like(CMFcpo)  inz
014400150331     d   wACM                              like(CMFacm)  inz
014500150403     d   wDFA                              like(CMFdfa)  inz
014600150401     d   wPEA                              like(CMFpea)  inz
014700150331     d   wFLO                              like(CMFflo)  inz
014800150331     d   wCMM                              like(CMIcmm)  inz
014900150331     d   wFCM                              like(CMIfcm)  inz
015000150331
015100150331       // -?Data/Ora attuali?
015200150331     d wTime_ds        ds                  inz
015300150331     d   wDate                        8s 0 inz
015400150331     d   wTime                        6s 0 inz
015500150331
015600150331       // -?Parametri ricevuti?
015700150331     d KPJBA         e ds
015800150331     d TRKC32ds      e ds
015900150402
016000161123       // -?Tab. "MRA" = Bart-Mailing x TRKC32R?
016100161123     d dMRA          e ds                  qualified     inz
016200161123     d dMRA_Eng      e ds                  qualified     inz
016300161123     d                                     extname(dMRA)
016400150331
016500150331       //--------------------------------------------------------------
016600150331       //?Definizione variabili globali.                               ?
016700150331       //--------------------------------------------------------------
016800150331
016900150331       // -?Flags booleani?
017000150331     d $EoF            s               n   inz
017100150401     d $First          s               n   inz(*on)
017200161128     d $English        s               n   inz
017300150401
017400150401       // -?Indici di schiera?
017500150401     d xx              s              3  0 inz
017600150331
017700150331       // -?Stringa SQL da eseguire?
017800150331     d wSQL            s           7000    inz  varying
017900150331
018000150331       // -?Codice errore rilevato?
018100150331     d wErr            s              3  0 inz
018200150331
018300150331       // -?Campi di controllo "rotture"?
018400150331     d wSaveCMM        s                   inz  like(CMIcmm)
018500150401     d wSaveKSU        s                   inz  like(CMIksu)
018600150401
018700150401       // -?Campi di Stampa?
018800171018     d o_PEA           s              1  0 inz
018801171018     d o_PEAA          s              2  1 inz
018900150331
019000150401       // -?Campi Data?
019100150331     d wDate_Eur       s               d   inz  datfmt(*EUR)
019200150331     d wOggi           s              8s 0 inz
019300150331
019400150331       //--------------------------------------------------------------
019500150331       //?Definizione prototipi procedure.                             ?
019600150331       //--------------------------------------------------------------
019700150331
019800150331       // -?Reperimento dati utente?
019900150331     d TIBS34ds      e ds                  inz
020000150331      /copy gaitrasrc/srcProtoPR,TIBS34R
020100150401
020200150401       // -?Controllo/Decodifica cliente?
020300150401      /copy gaitrasrc/srcProtoPI,TIBS69R
020400150401      /copy gaitrasrc/srcProtoPR,TIBS69R
020500150331
020600161123       // -?Reperimento dati tabelle?
020700161123      /copy gaitrasrc/srcProtoPI,TRULTAB
020800161123      /copy gaitrasrc/srcProtoPR,TRULTAB
020900150401
021000161123       // -?Parametri x Ridefinizione dati utente estesi per mailing PDF?
021100161123     d TRTCM1ds      e ds                  inz
021200161123       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
021300161123     d   §CM1mitt    e                     inz('ced')
021400161123       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
021500161123     d   §CM1dst     e                     inz('stefano.merighi+
021600161123     d                                          @brt.it')
021700161123       //  ?·§CM1tips = Tipo lettera via e-mail:?
021800161123       //   ?"LET" = PDF lettera: testo allegato in corpo con logo?
021900161123       //           ?(richiede righe libere iniziali per il logo)?
022000161123       //   ?"COR" = Spool in corpo mail: testo integrato senza logo?
022100161123       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
022200161123       //   ?"S01" = Standard vuoto?
022300161123       //   ?"SP1" = spool allegato (in formato PDF)?
022400161123     d   §CM1tips    e                     inz('AUM')
022500161123       //  ?·§CM1po   = Filiale?
022600161123     d   §CM1po      e                     inz('046')
022700161123       //  ?·§CM1var  = Oggetto e-mail?
022800161123     d   §CM1var     e                     inz('*OBJM*+
022900161123     d                                     Aggiornamento condizioni +
023000161123     d                                     tariffarie')
023100161123       //  ?·§CM1sts  = Stato spool?
023200161123     d   §CM1sts     e                     inz('H')
023300161130       //  ?·§CM1idp  = Id processo?
023400161123     d   §CM1idp     e                     inz('*')
023500161123       //  ?·§CM1att  = Numero attach corrente?
023600161123     d   §CM1att     e                     inz('01')
023700161123       //  ?·§CM1totatt  = Numero attach totali?
023800161123     d   §CM1totatt  e                     inz('02')
023900150331
024000150331       // -?API QCAPCMD (Process Commands)?
024100150331     d Qcmd            s           2048    inz  varying
024200150331      /copy qSysInc/qRpgleSrc,QCAPCMD
024300150331      /copy gaitrasrc/srcProtoPR,QCAPCMD
024400150331
024500150331       // -?Parametri gestione errori API.?
024600150331      /copy qsysinc/qrpglesrc,QUSEC
024700150331
024800150331       //--------------------------------------------------------------
024900150331       //?Definizione key-list.                                        ?
025000150331       //--------------------------------------------------------------
025100150331
025200150402      *// // -?File AZCMM01L?
025300150402     d*// keyAZCMM01    e ds                  extname( AZCMM01L : *key )
025400150402     d*//                                     prefix(k_)  inz
025500150331
025600150331       // -?File AZNTC01L?
025700150331     d keyAZNTC01    e ds                  extname( AZNTC01L : *key )
025800150331     d                                     prefix(k_AZ)  inz
025900150331
026000150331       // -?File TFNTC01L?
026100150331     d keyTFNTC01    e ds                  extname( TFNTC01L : *key )
026200150331     d                                     prefix(k_TF)  inz
026300150402
026400150402       // -?File TICMF01L?
026500150402     d keyTICMF01    e ds                  extname( TICMF01L : *key )
026600150402     d                                     prefix(k_)    inz
026700150331
026800150331       //--------------------------------------------------------------
026900150331       //?M A I N - L I N E                                            ?
027000150331       //--------------------------------------------------------------
027100150331
027200150331     c     *Entry        plist
027300150331     c                   parm                    KPJBA
027400150331
027500150331      /free
027600150331
027700150331       // -?Operazioni iniziali?
027800150331       exsr  sr_RoutInz;
027900150331
028000150331       // -?Preparazione SQL per estrazione dati da TICMF00F e TICMI00F?
028100150331       exsr  sr_PrepSQL;
028200150331
028300150331       // -?Ciclo di elaborazione file TICMF00F e TICMI00F?
028400150331       exsr  sr_OpenCursor;
028500150331
028600150331       DoW  Not $EoF;
028700150331         exsr  sr_ReadCursor;
028800150331       EndDo;
028900150331
029000150331       exsr  sr_CloseCursor;
029100150331
029200150331       // -?Operazioni finali?
029300150331       exsr  sr_RoutEnd;
029400150331
029500150331       //--------------------------------------------------------------
029600150331       //?Operazioni iniziali.                                         ?
029700150331       //--------------------------------------------------------------
029800150331       BEGSR  sr_RoutInz;
029900150331
030000150331         // -?Impostazione opzioni per SQL?
030100150331         exec sql   set  option  DynUsrPrf = *Owner,
030200150331                                 CloSqlCsr = *EndMod;
030300150331
030400150331         // -?Impostazione chiusura?
030500150331         *inLR = *on;
030600150331
030700150331         // -?Impostazione parametri ricevuti?
030800150331         TRKC32ds = kpjbu;
030900150331
031000150331         // -?Reperimento dati del lavoro?
031100150331         exsr  sr_DatiJob;
031200150402
031300150402         // -?Reperimento tab. "MRA"?
031400161123         clear  dMRA;
031500161123         if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
031600161123                          *omit : *omit :
031700161123                          *omit : *omit :
031800161123                          *omit : *omit : *omit : *omit :
031900161123                          *omit : *omit : *omit : *omit :
032000161123                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
032100161123                          = *zero      AND
032200161123               ds_TNTBE.TBEatb = *blank;
032300161123           dMRA = ds_TNTBE.TBEuni;
032400161123         endif;
032500161123
032600161123         clear  dMRA_Eng;
032700161123         if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : 'ENG' :
032800161123                          *omit : *omit :
032900161123                          *omit : *omit :
033000161123                          *omit : *omit : *omit : *omit :
033100161123                          *omit : *omit : *omit : *omit :
033200161123                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
033300161123                          = *zero      AND
033400161123               ds_TNTBE.TBEatb = *blank;
033500161123           dMRA_Eng = ds_TNTBE.TBEuni;
033600161123         endif;
033700150331
033800150331         // -?Reperimento data e ora attuali?
033900150331         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
034000150331         wDate_Eur = %date( wDate : *Iso );
034100150331         wOggi     = %dec( wDate_Eur );
034200150402
034300150402         // -?Apertura *file TICMF00F per aggiornamento flag?
034400150402         //  ?"E-Mail inviata"?
034500150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
034600150402           open  TICMF01L;
034700150402         endif;
034800150331
034900150331       ENDSR;
035000150331
035100150331       //--------------------------------------------------------------
035200150331       //?Reperimento Dati del job (Utente/Operativi).                 ?
035300150331       //--------------------------------------------------------------
035400150331       BEGSR  sr_DatiJob;
035500150331
035600150331         in(E) §AzUte;
035700150331         if NOT %error;
035800150331           in(E) §DatiUte;
035900150331         endif;
036000150331         if %error or RSut = *blanks;
036100150331           clear TIBS34ds;
036200150331           tibs34r(tibs34ds);
036300150331           in §AzUte;
036400150331           in §DatiUte;
036500150331         endif;
036600150331
036700150331       ENDSR;
036800150331
036900150331       //--------------------------------------------------------------
037000150331       //?Preparazione SQL.                                            ?
037100150331       //--------------------------------------------------------------
037200150331       BEGSR  sr_PrepSQL;
037300150331
037400150331         // -?Preparazione SQL?
037500150331         clear  wSQL;
037600150331         wSQL = 'with +
037700161114         // -?Prima selezione da TICMF00F:?
037800150331                      TICMF as +
037900150331                      ( select CMFncm, CMFksu, CMFksc, CMFcpo, +
038000150403                               CMFacm, CMFdfa, CMFpea, CMFflo +
038100150331                          from TICMF00F +
038200161114         // ·?Selezione dei clienti nella fase " LT"?
038300150401                         where CMFacm = ''' + c_FaseDaElab + '''';
038400161114         // ·?Selezione dei clienti per l'invio e-mail o la stampa?
038500161114         //  ?(a cui NON è stata inviata o stampata - CMFFLO  = " ")?
038600150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
038700150401           wSQL +=       ' and substr( CMFflo, 1, 1 ) = '' ''';
038800161114         // ·?Selezione dei clienti per la RI-stampa?
038900161114         //  ?(a cui è GIÀ stata inviata o stampata - CMFFLO  = "S")?
039000150331         else;
039100150401           wSQL +=       ' and substr( CMFflo, 1, 1 ) <> '' ''';
039200150331         endif;
039300161114         // -?Selezione dei clienti per n° campagna?
039400150331         if  iKC32ncm > *zero;
039500150331           wSQL +=       ' and CMFncm = ' + %trim( %editc( iKC32ncm : '3' ) );
039600150331         endif;
039700150331         wSQL +=       ' order by CMFncm, CMFksu, CMFksc, CMFcpo ) +
039800150331
039900150331                 select TICMF.*, CMIcmm, CMIfcm +
040000150331                   from TICMF inner join TICMI00F +
040100150331                     on CMFncm = CMIncm +
040200150331                    and CMFksu = CMIksu +
040300150331                    and CMFksc = CMIksc +
040400150331                    and CMFcpo = CMIcpo ';
040500150331         $First = *on;
040600161114         // -?Seconda selezione da TICMI00F:?
040700161114         // ·?Selezione dei clienti per commerciale unificante?
040800150331         if  iKC32cmm > *zero;
040900150331           if  $First;
041000150331             wSQL  += 'where';
041100150331             $First = *off;
041200150331           else;
041300150331             wSQL  += 'and';
041400150331           endif;
041500150331           wSQL +=    ' CMIcmm = ' + %trim( %editc( iKC32cmm : '3' ) );
041600150331         endif;
041700161114         // ·?Selezione dei clienti per filiale del commerciale unificante?
041800150331         if  iKC32fcm > *zero;
041900150331           if  $First;
042000150331             wSQL += 'where';
042100150331             $First = *off;
042200150331           else;
042300150331             wSQL += 'and';
042400150331           endif;
042500150331           wSQL +=    ' CMIfcm = ' + %trim( %editc( iKC32fcm : '3' ) );
042600150331           $First = *off;
042700150331         endif;
042800161114         // -?Ordinamento per Filiale commerciale unificante,?
042900161114         //  ?                Commerciale unificante,?
043000161114         //  ?                Numero campagna commerciale,?
043100161114         //  ?                Cliente unificante,?
043200161114         //  ?                Cliente,?
043300161114         //  ?                Potenziale.?
043400150331         wSQL += ' +
043500150331                  order by CMIfcm, CMIcmm, +
043600150331                           CMFncm, CMFksu, CMFksc, CMFcpo +
043700150331                    for fetch only';
043800150331
043900150331         exec sql   prepare S1   from :wSQL;
044000150331
044100150331         // -?Dichiarazione cursore?
044200150331         exec sql   declare C1   cursor for S1;
044300150331
044400150331         if  SQLcode < *zero;
044500150331           $EoF = *on;
044600150331           wErr = 1;
044700150331           exsr  sr_PrintErr;
044800150401           exsr  sr_RoutEnd;
044900150331         endif;
045000150331
045100150331       ENDSR;
045200150331
045300150331       //--------------------------------------------------------------
045400150331       //?Apertura cursore.                                            ?
045500150331       //--------------------------------------------------------------
045600150331       BEGSR  sr_OpenCursor;
045700150331
045800150331         // -?Apertura del cursore?
045900150331         exec sql   open C1;
046000150331
046100150331         if  SQLcode < *zero;
046200150331           $EoF = *on;
046300150331           wErr = 1;
046400150331           exsr  sr_PrintErr;
046500150401           exsr  sr_RoutEnd;
046600150331         endif;
046700150331
046800150331       ENDSR;
046900150331
047000150331       //--------------------------------------------------------------
047100150331       //?Lettura cursore.                                             ?
047200150331       //--------------------------------------------------------------
047300150331       BEGSR  sr_ReadCursor;
047400150331
047500150331         clear  wErr;
047600150331         clear  wSQL_ds;
047700150331
047800150331         exec sql   fetch next   from C1   into :wSQL_ds;
047900150331
048000150331         Select;
048100150331
048200150331           // -?Fine File?
048300150331           When  SQLcode = 100;
048400150331             $EoF = *on;
048500150331
048600150331           // -?Errore SQL?
048700150331           When  SQLcode < *zero;
048800150331             $EoF = *on;
048900150331             wErr = 1;
049000150331             exsr  sr_PrintErr;
049100150401             exsr  sr_RoutEnd;
049200150331
049300150331           // -?Elaborazione singolo cliente?
049400150331           Other;
049500150331             exsr  sr_ElabRec;
049600150331
049700150331         EndSl;
049800150331
049900150331       ENDSR;
050000150331
050100150331       //--------------------------------------------------------------
050200150331       //?Chiusura cursore.                                            ?
050300150331       //--------------------------------------------------------------
050400150331       BEGSR  sr_CloseCursor;
050500150331
050600150331         // -?Chiusura del cursore?
050700150331         exec sql   close C1;
050800150331
050900150331       ENDSR;
051000150331
051100150331       //--------------------------------------------------------------
051200150331       //?Elaborazione singolo Cliente a cui inviare la e-Mail.        ?
051300150331       //--------------------------------------------------------------
051400150331       BEGSR  sr_ElabRec;
051500150331
051600150331         // -?Reperimento dati del nuovo commerciale?
051700150402         If  wSQL_ds.wCMM <> wSaveCMM;
051800150401
051900150414           // -?Apertura file di stampa x comm.le?
052000150414           If  iKC32fsr = *blank  or  iKC32fsr = 'R';
052100150414             if  %open(TRKC32P2);
052200150414               close  TRKC32P2;
052300150414               Qcmd = c_CmdDltOvr_2;
052400150414               exsr  sr_ExecCmd;
052500150414             endif;
052600150414             Qcmd = %replace( %subst( %editc( wSQL_ds.wCMM : 'X' ) : 4 : 4 ) :
052700150414                              c_CmdOvrPrtF_2 :
052800150414                              %scan( '_FCM' : c_CmdOvrPrtF_2 ) ) +
052900150414                   'outq(R' + %editc( wSQL_ds.wFCM : 'X' ) + 'SEDE)';
053000150414             exsr  sr_ExecCmd;
053100150414             open  TRKC32P2;
053200150414           EndIf;
053300150331
053400150402           //// -?Controllo validità in AZCMM00F?
053500150402           //clear  AZNTCrnt;
053600150402           //clear  keyAZCMM01;
053700150402           //k_CMMcod = wSQL_ds.wCMM;
053800150402           //chain  %kds( keyAZCMM01 )  AZCMM000;
053900150402           //
054000150402           //if  Not %found(AZCMM01L)  or
054100150402           //    CMMpar  <> *blank     or
054200150402           //    CMMdtIni > wDate      or
054300150402           //    CMMdtFin < wDate;
054400150402           //  wErr = 2;
054500150402           //  exsr  sr_PrintErr;
054600150402           //  leavesr;
054700150402           //endif;
054800150402
054900150402           // -?Reperimento indirizzo e-mail da AZNTC00F?
055000150331           clear  keyAZNTC01;
055100150402           k_AZNTCcmm = wSQL_ds.wCMM;
055200150331           k_AZNTCtnt = '02';
055300150331           chain  %kds( keyAZNTC01 )  AZNTC000;
055400150331
055500150331           if  Not %found(AZNTC01L);
055600150331             wErr = 2;
055700150331             exsr  sr_PrintErr;
055800150331             leavesr;
055900150331           endif;
056000150402
056100150402           wSaveCMM = wSQL_ds.wCMM;
056200150331
056300150331         EndIf;
056400150331
056500150331         // -?Reperimento dati del responsabile trasporti (cliente)?
056600150331         If  wSQL_ds.wKSU <> wSaveKSU;
056700150331
056800150331           clear  TFNTCrnt;
056900150331           clear  keyTFNTC01;
057000150331           k_TFNTCapl = 'C';
057100150401           k_TFNTCnk1 = %editc( DUTkci : 'X' ) +
057200150401                        %editc( wSQL_ds.wKSU : 'X' );
057300150331           // -?1° tentativo: responsabile trasporti?
057400150331           k_TFNTCtnt = '06';
057500150331           chain  %kds( keyTFNTC01 )  TFNTC;
057600150331
057700150331           if  Not %found(TFNTC01L);
057800150331             // -?2° tentativo: invio fattura?
057900150331             k_TFNTCtnt = '84';
058000150331             chain  %kds( keyTFNTC01 )  TFNTC;
058100150331
058200150331             if  Not %found(TFNTC01L);
058300150331               // -?Ultima soluzione: e-mail del comm.le?
058400150331               TFNTCrnt = %trim( AZNTCrnt ) + c_at;
058500161128               wErr = 4;
058600161128               exsr  sr_PrintErr;
058700150331             endif;
058800150331
058900150331           endif;
059000150401
059100150401           // -?Decodifica Cliente Unificante?
059200150401           clear  TIBS69ds;
059300150401           I69sif = knsif;
059400150401           I69kcc = DUTkci;
059500150401           I69kac = wSQL_ds.wKSU;
059600150402           I69kin = I69kac;
059700161123           I69kcs = I69kac;
059800150401           tibs69r( TIBS69ds :
059900150401                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
060000150331
060100150331           wSaveKSU = wSQL_ds.wKSU;
060200150331
060300150331         EndIf;
060400150331
060500150402         // -?Stampa x comm.le  ed  Invio e-mail al cliente?
060600150331         exsr  sr_SendEmail;
060700150331
060800150331       ENDSR;
060900150331
061000150331       //--------------------------------------------------------------
061100150331       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
061200150331       //--------------------------------------------------------------
061300150331       BEGSR  sr_SendEmail;
061400161128
061500161128         $English = ( %subst( CLSflo : 4 : 1 ) = 'S' );
061600150401
061700150402         // -?Override al file di stampa (per impostarvi i dati per?
061800150402         //  ?l'invio via e-mail) ed Apertura dello stesso?
061900150402         //  ?(SE Stampa)?
062000150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
062100150401           exsr sr_Override;
062200150402           open  TRKC32P;
062300150401         endif;
062400150402
062500150331
062600150414         // -?Preparazione spool per il singolo cliente (ed il comm.le)?
062700161128         if  $English;
062800161201           pKC32int = 'Dear    ';
062900161124         else;
063000161124           pKC32int = 'Spett.le';
063100161124         endif;
063200150402         pKC32rsc = ACOrag;
063300150402         pKC32ind = INDvia;
063400150402         pKC32cap = INDcae;
063500161128         if  INDsta <> *blank;
063600161128           pKC32loc = %trimR( INDcit ) + '  ' + INDsta;
063700161128         else;
063800161128           pKC32loc = %trimR( INDcit ) + '  ' + INDprv;
063900161128         endif;
064000150414
064100150414         pKC32cmm = wSQL_ds.wCMM;
064200150403
064300150403         wDate_Eur = %date( wSQL_ds.wDFA : *Iso );
064400161124         // -?Testo in lingua Inglese?
064500161128         if  $English;
064600161201           pKC32dat = 'Bologna, ' +
064700161124                      %trimL( %editc( %dec( wDate_Eur ) : 'Y' ) );
064800171019           //pKC32ogg = 'OBJECT: ' +
064801171019           pKC32ogg = 'SUBJECT: ' +
064900161124                      %xlate( c_CharMin : c_CharMai : c_Oggetto_Eng : 1 ) +
065000161124                      ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
065100161124         // -?Testo in lingua Italiana?
065200161124         else;
065300161201           pKC32dat = 'Bologna, li ' +
065400161124                      %trimL( %editc( %dec( wDate_Eur ) : 'Y' ) );
065500161124           pKC32ogg = 'OGGETTO: ' +
065600161124                      %xlate( c_CharMin : c_CharMai : c_Oggetto : 1 ) +
065700161124                      ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
065800161124         endif;
065900150402
066000150414         if  iKC32fsr = *blank  or  iKC32fsr = 'R';
066100150414           write  P2_cli;
066200150414           write  P2_cmm;
066300150414           write  P2_ogg;
066400150414         endif;
066500150402
066600150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
066700150402           write  KC32cli;
066800150402           write  KC32ogg;
066900150402         endif;
067000150402
067100161128         // -?Stampa del corpo - per entrambe le lingue (Italiano ed?
067200171018         //  ?Inglese):                                              ?
067300161124         o_PEA   = wSQL_ds.wPEA;
067301171018         o_PEAA  = wSQL_ds.wPEA;
067400161124
067500171018         For  xx = 1  To  %elem( sk_Alleg );
067600171018
067700161123           // -?Testo in lingua Inglese?
067800161128           If  $English;
067900161124
068000171019             pKC32txt = sk_Alleg_Eng(xx);
068100171019             if  xx = 9;
068101171019               // -?Campagna per il 1%
068102171019               IF  o_PEAA = 1;
068103171019                 %subst(pKC32txt:66:22) = %editc(o_PEA:'3') +
068104171019                 '% with effect from';
068105171019               ENDIF;
068106171019               // -?Campagna per il 2,5%
068107171019               if  o_PEAA = 2,5;
068108171019                 %subst(pKC32txt:66:22) = %editc(o_PEAA:'3') +
068401171019                 '% with effect from';
068402171019               ENDIF;
068500161123             endif;
068600161123
068700161123           // -?Testo in lingua Italiana?
068800161123           Else;
068900161123
069000161123             pKC32txt = sk_Alleg(xx);
069100171018             if  xx = 10;
069101171018               // -?Campagna per il 1%
069200171018               IF  o_PEAA = 1;
069300171018                 pKC32txt = 'misura dell' + ''' ' + %editc(o_PEA:'3');
069400171018               ENDIF;
069500171018               // -?Campagna per il 2,5%
069600171018               if  o_PEAA = 2,5;
069700171018                 pKC32txt = 'misura del ' + %editc(o_PEAA:'3');
069800161124               endif;
069900171018               pKC32txt = %trim(pKC32txt) +
070000171018               '%, con decorrenza 1 Novembre 2017.';
070100161123             endif;
070200161123
070300161123           EndIf;
070400150401
070500150414           if  iKC32fsr = *blank  or  iKC32fsr = 'R';
070600150414             write  P2_txt;
070700150414           endif;
070800150414           if  iKC32fsr = *blank  or  iKC32fsr = 'M';
070900150402             write  KC32txt;
071000150402           endif;
071100150401
071200150401         EndFor;
071300150401
071400150402
071500161128         // -?Chiusura file di stampa ed invio e-mail  e?
071600150402         //  ?Rimozione Override al file di stampa per e-mail?
071700150402         //  ?(SE Stampa)?
071800150414         if  iKC32fsr = *blank  or  iKC32fsr = 'M';
071900150402           close  TRKC32p;
072000150402           Qcmd = c_CmdDltOvr_1;
072100150402           exsr  sr_ExecCmd;
072200150402         endif;
072300150402
072400150402
072500150402         // -?Aggiornamento TICMF00F.CMFFLO?
072600150402         //  ?(SE Stampa)?
072700150414         If  iKC32fsr = *blank  or  iKC32fsr = 'M';
072800150402
072900150402           clear  keyTICMF01;
073000150402           k_CMFncm = wSQL_ds.wNCM;
073100150402           k_CMFksu = wSQL_ds.wKSU;
073200150402           k_CMFksc = wSQL_ds.wKSC;
073300150402           k_CMFcpo = wSQL_ds.wCPO;
073400150402           k_CMFacm = wSQL_ds.wACM;
073500150402           chain  %kds( keyTICMF01 : 5 )  TICMF000;
073600150402
073700150402           if  %found(TICMF01L);
073800150402             %subst( CMFflo : 1 : 1 ) = 'S';
073900161128             %subst( CMFflo : 2 : 1 ) = *blank;
074000161128             if  $English;
074100161128               %subst( CMFflo : 2 : 1 ) = 'E';
074200161128             endif;
074300150402             //_______________
074400150402             update  TICMF000;
074500150402             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
074600150402           endif;
074700150402
074800150402         EndIf;
074900150331
075000150331       ENDSR;
075100150401
075200150401       //--------------------------------------------------------------
075300150401       //?Override al file di Stampa per generarvi una e-mail.         ?
075400150401       //--------------------------------------------------------------
075500150401       BEGSR  sr_Override;
075600150401
075700150401         // -?Impostazione regole per mailing?
075800150401         clear  TRTCM1ds;
075900150401         §CM1mitt = %trim(AZNTCrnt);
076000150401         §CM1dst  = %trim(TFNTCrnt);
076100161129         §CM1po   = %editc( wSQL_ds.wFCM : 'X' );
076200161128         if  $English;
076300161123           §CM1tips = c_TipoStampa_Estero;
076400161123           §CM1idp  = dMRA_Eng.§MRAidPro;
076500161129           §CM1var  = '*OBJM*' + c_Oggetto_Eng +
076600161129                      ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
076700161123         else;
076800161123           §CM1tips = c_TipoStampa;
076900161123           §CM1idp  = dMRA.§MRAidPro;
077000161129           §CM1var  = '*OBJM*' + c_Oggetto +
077100161129                      ' (' + %editc( wSQL_ds.wKSU : 'X' ) + ')';
077200161123         endif;
077300161130         //*·§CM1sts  = *off;
077400161130         §CM1sts  = 'H';
077500150401
077600150401         // -?Override all'"allegato e-mail"?
077700161129         if  $English;
077800161129           Qcmd = c_CmdOvrPrtF_1
077900161129                + 'outq(' + %trim( dMRA_Eng.§MRAoutQI ) + ') '
078000161129                + 'usrdfndta(''' + TRTCM1ds + ''')';
078100161129         else;
078200161129           Qcmd = c_CmdOvrPrtF_1
078300161129                + 'outq(' + %trim( dMRA.§MRAoutQI ) + ') '
078400161129                + 'usrdfndta(''' + TRTCM1ds + ''')';
078500161129         endif;
078600150401
078700150401         exsr  sr_ExecCmd;
078800150401
078900150401         if  Qusei <> *blank;
079000150401           $EoF = *on;
079100150401           wErr = 3;
079200150401           exsr  sr_PrintErr;
079300150401           exsr  sr_RoutEnd;
079400150401         endif;
079500150401
079600150401       ENDSR;
079700150401
079800150401       //--------------------------------------------------------------
079900150401       //?Stampa segnalazione dell'errore rilevato (non solo da SQL).  ?
080000150401       //--------------------------------------------------------------
080100150401       BEGSR  sr_PrintErr;
080200150401
080300150401         // -?Stampa errore rilevato?
080400150401         if  Not %open(PrtF198);
080500150401           open  PrtF198;
080600150401           *inOE = *on;
080700150401         endif;
080800150401
080900150401         if  *inOE;
081000150401           except  PRTerrTXT;
081100150402           *inOE = *off;
081200150401         endif;
081300150401
081400150401         select;
081500161128           // -?Errore SQL?
081600150401           when  wErr = 1;
081700150401             except  PRTerr01;
081800150408             // -?Stampa del Dump?
081900150408             Dump(A);
082000150408             // -?Stampa del Job-Log?
082100150408             Qcmd = 'DSPJOBLOG job(*) output(*print)';
082200150408             exsr  sr_ExecCmd;
082300161128           // -?Errore SQL?
082400150401           when  wErr = 2;
082500150401             except  PRTerr02;
082600161128           // -?Errore SQL?
082700150401           when  wErr = 3;
082800150401             except  PRTerr03;
082900161128           // -?Non reperito indirizzo e-mail del cliente?
083000161128           when  wErr = 4;
083100161128             except  PRTerr04;
083200150401         endsl;
083300150401
083400150401       ENDSR;
083500150331
083600150331       //--------------------------------------------------------------
083700150331       //?Esecuzione del comando (già impostato).                      ?
083800150331       //--------------------------------------------------------------
083900150331       BEGSR  sr_ExecCmd;
084000150331
084100150331         clear Qcap0100;
084200150331         Qcabcsdh = *off;
084300150331         Qcapa    = *off;
084400150331         Qcacmdss = *off;
084500150331         Qcaerved = *allX'00';
084600150331
084700150331         clear Qusec;
084800150331         Qusbprv  = %size(Qusec);
084900150331
085000150331         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
085100150331                           %size(Qcap0100) : 'CPOP0100' : *omit :
085200150331                           0 : 0 : Qusec);
085300150331
085400150331         //if  Qusei <> *blank;
085500150331         //  ...;
085600150331         //endif;
085700150331
085800150331       ENDSR;
085900150331
086000150331       //--------------------------------------------------------------
086100150331       //?Operazioni finali.                                           ?
086200150331       //--------------------------------------------------------------
086300150331       BEGSR  sr_RoutEnd;
086400150402
086500150402         // -?Chiusura *file TICMF00F (se in aggiornamento)?
086600150402         if  %open(TICMF01L);
086700150402           close  TICMF01L;
086800150402         endif;
086900150331
087000150401         // -?Chiusura delle funzioni precedentemente aperte?
087100161123         cloTabella();
087200150401
087300150401         // -?Chiusura dei file di stampa per le e-mail?
087400150402         if  %open(TRKC32P);
087500150402           close  TRKC32P;
087600150402           Qcmd = c_CmdDltOvr_1;
087700150401           exsr  sr_ExecCmd;
087800150401         endif;
087900150401
088000150401         // -?Chiusura dei file di stampa per i comm.li?
088100150402         if  %open(TRKC32P2);
088200150402           close  TRKC32P2;
088300150402           Qcmd = c_CmdDltOvr_2;
088400150401           exsr  sr_ExecCmd;
088500150401         endif;
088600150401
088700150401         // -?SE stampato almeno un Errore:?
088800150401         //  ?stampa di "fine" e chiusura del file di stampa?
088900150401         if  %open(PrtF198);
089000150401           except  PRTerrEND;
089100150401           close  PrtF198;
089200150401         endif;
089300150331
089400150331         // -?Uscita dal *pgm?
089500150331         return;
089600150331
089700150331       ENDSR;
089800150331
089900150331      /end-free
090000150402
090100150402       //--------------------------------------------------------------
090200150402       //?S P O O L - F I L E S                                        ?
090300150402       //--------------------------------------------------------------
090400150401
090500150401       // -?Stampa ERRORI?
090600150331
090700150401     oPrtF198   e            PRTerrTXT         3
090800150331     o                       RSUT
090900150401     o                                        +   5 'ERRORI IN INVIO +
091000150401     o                                               E-MAIL A CLIENTI +
091100150401     o                                               IN CAMPAGNA'
091200150331     o                       SDSpgm           +   5
091300150402     o                       wOggi         y  +   5
091400150401     o          e            PRTerrTXT   1  1
091500150331     o                       KNSIF
091600150331     o                       KNMUS            +   1
091700150401     o                                        +   4 '----------------+
091800150401     o                                               -----------------+
091900150401     o                                               -----------'
092000150331     o                                        +   5 'Pag.'
092100150331     o                       Page          z  +   0
092200150331     o                       wTime            +   7 '  :  :  '
092300150331      *
092400150401     o          e            PRTerr01    2
092500150401     o                                              'Rilevato ERRORE n-
092600150401     o                                              ella selezione dei-
092700150401     o                                               clienti in Campag-
092800150401     o                                              a Aumenti Tariffar-
092900150401     o                                              i.'
093000150331     o          e            PRTerr01    2
093100150331     o                                              'Consultare le sta-
093200150331     o                                              mpe del lavoro'
093300150331     o
093400150331     o                       SDSjobNbr     3  +   1
093500150331     o                                        +   0 '/'
093600150331     o                       SDSjobUser       +   0
093700150331     o                                        +   0 '/'
093800150331     o                       SDSjobName       +   0
093900150401     o                                        +   0 '.'
094000150331      *
094100150401     o          e            PRTerr02    2
094200150331     o                                              'Campagna'
094300150401     o                       wSQL_ds.wNCM  2  +   1
094400150331     o                                        +   2 'Unificante'
094500150331     o                       wSQL_ds.wKSU     +   1
094600150401     o                                        +   2 'Commerciale'
094700150401     o                       wSQL_ds.WCMM     +   1
094800150401     o                                        +   2 '* Commerciale NON-
094900150401     o                                               attivo'
095000150401      *
095100150401     o          e            PRTerr03    2
095200150401     o                                              'Campagna'
095300150401     o                       wSQL_ds.wNCM  2  +   1
095400150401     o                                        +   2 'Unificante'
095500150401     o                       wSQL_ds.wKSU     +   1
095600150401     o                                        +   2 'Commerciale'
095700150401     o                       wSQL_ds.WCMM     +   1
095800161128     o                                        +   2 '* Fallita prepara-
095900150401     o                                              zione del file di -
096000150401     o                                              stampa da inviare -
096100150401     o                                              via e-mail al clie-
096200161128     o                                              nte.'
096300161128      *
096400161128     o          e            PRTerr04    2
096500161128     o                                              'Campagna'
096600161128     o                       wSQL_ds.wNCM  2  +   1
096700161128     o                                        +   2 'Unificante'
096800161128     o                       wSQL_ds.wKSU     +   1
096900161128     o                                        +   2 'Commerciale'
097000161128     o                       wSQL_ds.WCMM     +   1
097100161128     o                                        +   2 '* Indirizzo e-mai-
097200161128     o                                              l del cliente NON -
097300161128     o                                              reperito: impostat-
097400161128     o                                              o quello del comme-
097500161128     o                                              rciale.'
097600150331      *
097700150401     o          e            PRTerrEND   2
097800161128     o                                        +  10 '***  Fine Lista  -
097900161128     o                                              ***'
098000150401
098100150401       //--------------------------------------------------------------
098200150401       //?Schiere a tempo di compilazione.                             ?
098300150401       //--------------------------------------------------------------
098400150401
098500150401** --?sk_Alleg:?Testo allegato e-mail?--------------------------------------------------------*
098600161124Gentile Cliente,                                                                                 1
098700161124                                                                                                 2
098800171020come testimoniano  le note  vicende sindacali  dell'ultimo periodo, tutto  il comparto           3
098900171018espresso sta subendo un incremento straordinario ed imprevedibile del costo del lavoro.          4
099000171018                                                                                                 5
099100171018Ciononostante, l'estesa capillarità del  nostro network e gli investimenti che abbiamo           6
099200171018già effettuato per ottimizzare i  processi operativi di movimentazione e distribuzione,          7
099300171018aumentandone efficienza e produttività, ci permettono di ridurre al minimo la richiesta          8
099400171018di un urgente  adeguamento delle  condizioni tariffarie  in essere, contenendola nella          9
099500171018misura dell'                                                                                    10
099600171018                                                                                                11
099700171018I nostri  commerciali  sono a sua disposizione  per poterle  illustrare  al  meglio le          12
099800171018motivazioni  di questa eccezionale ed improrogabile esigenza e per scusarsi di persona          13
099900171018del breve preavviso con cui siamo stati costretti ad informarla.                                14
100000171018                                                                                                15
100100171018Certi della sua comprensione, la ringraziamo per la fiducia che ci accorda ogni giorno          16
100200171018affidandoci le sue spedizioni e le assicuriamo il nostro massimo impegno per garantirle         17
100300171018la qualità di sempre.                                                                           18
100400171018                                                                                                19
100500171018Distinti saluti.                                                                                20
100600171018                                                                                                21
100700171018                                                                                                22
100800171018                                                                                                23
100900171018                                                           BRT S.p.A.                           24
101000171018                                                       Direzione Generale                       25
101100161128** --?sk_Alleg_Eng:?Testo allegato e-mail (in lingua Inglese)?--------------------------------*
101200161128Dear Client,                                                                                     1
101300161128                                                                                                 2
101400171019as evidenced by the well-known trade union events of the last period, the entire express         3
101500171019compartment is undergoing an extraordinary and unpredictable increase in labour costs.           4
101600171019                                                                                                 5
101700171019Nevertheless, the extensive density of our network and the investments we have already           6
101800171019made to optimize the handling and distribution processes, increasing their efficiency            7
101900171019and  productivity, allow us to  minimize the demand  for an urgent  adjustment of the            8
102000171019existing tariff conditions. This adjustment will consist of only                                 9
1020011710191 November 2017.                                                                                10
102100171019                                                                                                11
102200171019Our sales force is at your disposal to explain you this exceptional and undelayable             12
102300171019necessity and to apologize in person for the short notice we have to give you.                  13
102301171019                                                                                                14
102400171019Certain of your understanding, we thank you for the trust you place in us every day by          15
102401171019handing over  to us  your shipments  and  we assure you  our utmost commitment to  the          16
102500171019quality standards we have always guaranteed.                                                    17
102600171019                                                                                                18
102700171019Best Regards.                                                                                   19
102800171019                                                                                                20
102900171019                                                                                                21
103000171019                                                                                                22
103100171019                                                           BRT S.p.A.                           23
103200171019                                                        General Management                      24
103300171019                                                                                                25
