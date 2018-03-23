000100150616       //==============================================================
000200150616       //?FIDNA8R - Chiusura R.A. di SEDE aperte da più di un anno.    ?
000300161128       //?          e Pulizia RA Automatiche su bolle recupero   ?
000400150616       //==============================================================
000500150616
000600150616       //--------------------------------------------------------------
000700150616       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800150616       //--------------------------------------------------------------
000900150616
001000150616     /*PRM  dbgview(*source)
001100150616     /*END
001200150616
001300150616       //--------------------------------------------------------------
001400150616       //?Specifiche di controllo.                                     ?
001500150616       //--------------------------------------------------------------
001600150616
001700150616     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800150616     h dftactgrp(*no)
001900150616     h alwnull(*inputonly)
002000150616
002100150616       //--------------------------------------------------------------
002200150616       //?Dichiarazione file.                                          ?
002300150616       //--------------------------------------------------------------
002400150616
002500150616       // -?Bolle Sede?
002600150616     fTITAS30C  if   e           k disk    usropn
002700150616
002800150616       // -?O.R.M.?
002900150616     fFNORM01L  if   e           k disk    usropn
003000150618
003100150720       // -?Spool per Lista R.A. di sede da chiudere (ogg. Generico)?
003200150720     fPrtEMail  o    f  132        printer usropn  oflind(*inOF)
003300150616
003400150616       //--------------------------------------------------------------
003500150616       //?Definizione costanti.                                        ?
003600150616       //--------------------------------------------------------------
003700150616
003800150622       // -?Codice Sede?
003900150619     d c_Sede          c                   const(046)
004000150622
004100150622       // -?Motivo di Chiusura per le R.A.?
004200150622     d c_CodChiusura   c                   const('  I')
004300150720
004400150720       // -?Comandi di override ai PrtF (e-mail)?
004500150720     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004600150720     d                                           file(PrtEMail) +
004700150720     d                                           pagesize(66 120 *rowcol) +
004800150720     d                                           lpi(6) cpi(16.7) +
004900150720     d                                           ovrflw(63) +
005000150720     d                                           hold(*no) +
005100150720     d                                           ovrscope(*actgrpdfn)')
005200150720     d c_CmdDltOvr     c                   const('DLTOVR +
005300150720     d                                            file(PrtEMail) +
005400150720     d                                            lvl(*actgrpdfn)')
005500150616
005600150616       //--------------------------------------------------------------
005700150616       //?Definizione schiere.                                         ?
005800150616       //--------------------------------------------------------------
005900150616
006000150616
006100150616       //--------------------------------------------------------------
006200150616       //?Definizione aree dati.                                       ?
006300150616       //--------------------------------------------------------------
006400150616
006500150616       // -?Dati utente?
006600150616     d §AzUte        e ds                  extname(AZUTE00F)
006700150616     d                                     dtaara
006800150616     d §DatiUte      e ds                  extname(dDatiUte)
006900150616     d                                     dtaara
007000150616
007100150616       //--------------------------------------------------------------
007200150616       //?Definizione strutture dati.                                  ?
007300150616       //--------------------------------------------------------------
007400150616
007500150720       // -?Tab. MRA/DAN = Bart-Maling - Danni?
007600150720     d dMRAdan       e ds
007700150720
007800150720       // -?Parametri x Ridefinizione dati utente estesi per mailing?
007900150720     d TRTCM1ds      e ds                  inz
008000150720       //  ?·§CM1mitt = Indirizzo e-mail del mittente?
008100150721     d   §CM1mitt    e                     inz('ced')
008200150720       //  ?·§CM1dst  = Indirizzo e-mail del destinatario?
008300150720     d   §CM1dst     e                     inz('stefano.merighi@brt.it')
008400150720       //  ?·§CM1tips = Tipo lettera via e-mail:?
008500150720       //   ?"LET" = testo allegato in corpo con logo?
008600150720       //           ?(richiede righe libere iniziali per il logo)?
008700150720       //   ?"COR" = testo integrato senza logo?
008800150720       //           ?(non consente né UNDERLINE né HIGHLIGHT)?
008900150720     d   §CM1tips    e                     inz('COI')
009000150720       //  ?·§CM1po   = Filiale?
009100150720     d   §CM1po      e                     inz('046')
009200150720       //  ?·§CM1var  = Oggetto e-mail?
009300150720     d   §CM1var     e                     inz('*OBJM*+
009400150720     d                                     Chiusura R.A. di Sede +
009500150720     d                                     aperte da più di un anno')
009600150720       //  ?·§CM1sts  = Stato?
009700150720     d   §CM1sts     e                     inz(*off)
009800150720       //  ?·§CM1sts  = Id processo?
009900150721     d   §CM1idp     e                     inz('*')
010000150720
010100150616       // -?Status ds?
010200150616     d Status         sds
010300150618     d   SDSpgm          *proc
010400150618     d*//SDSprm          *parms
010500150618     d*//SDSdta              191    198
010600150618     d   SDSjobName          244    253
010700150618     d   SDSjobUser          254    263
010800150618     d   SDSjobNbr           264    269s 0
010900150616
011000150616       // -?Data/Ora attuali?
011100150616     d wTime_ds        ds            14    inz
011200150616     d   wDate                        8s 0 inz
011300150616     d   wTime                        6s 0 inz
011400150618
011500150618       // -?Oggetto = Spedizione?
011600150618     d WRKspe_ds       ds            16    inz
011700150618     d   WRKlnp                       3
011800150618     d   WRKnrs                       2
011900150618     d   WRKnsp                       7
012000150619     d   WRKaas                       4
012100150618
012200150618       // -?Oggetto = O.R.M.?
012300150618     d WRKorm_ds       ds            14    inz
012400150618     d   WRKpoe                       3
012500150618     d   WRKnso                       2
012600150618     d   WRKnor                       7
012700150618     d   WRKnrv                       2
012800150616
012900150616       // -?Dati estratti via SQL?
013000150618     d FITGD_ds      e ds                  extname(FITGD00F)  inz
013100150618     d*// FITGD00F      e ds                  based(nullPtr)
013200150618     d*// wSQL_ds         ds                  qualified     inz
013300150618     d*//   wPOT                              like(TGDpot)  inz
013400150618     d*//   wANT                              like(TGDant)  inz
013500150618     d*//   wNUT                              like(TGDnut)  inz
013600150618     d*//   wPDT                              like(TGDpdt)  inz
013700150618     d*//   wDAC                              like(TGDdac)  inz
013800150618     d*//   wORC                              like(TGDorc)  inz
013900150618     d*//*//wPRU                              like(TGDpru)  inz
014000150618     d*//   wRSC                              like(TGDrsc)  inz
014100150618     d*//   wTOR                              like(TGDtor)  inz
014200150618     d*//   wOGG                              like(TGDogg)  inz
014300150618     d*//*//wPRT                              like(TGDprt)  inz
014400150618     d*//*//wPOG                              like(TGDpog)  inz
014500150618     d*//*//wFAR                              like(TGDfar)  inz
014600150618     d*//   wKSC                              like(TGDksc)  inz
014700150616
014800150616       // -?Parametri ricevuti?
014900150616     d KPJBA         e ds
015000150616
015100150616       //--------------------------------------------------------------
015200150616       //?Definizione variabili globali.                               ?
015300150616       //--------------------------------------------------------------
015400150616
015500150616       // -?Flags booleani?
015600150619     d $Test           s               n   inz
015700150618     d $EoF            s               n   inz
015800161128     d $EoFRA          s               n   inz
015900150618     d $ChiudiRA       s               n   inz
016000150617
016100150617       // -?Stringa SQL da eseguire?
016200150618     d wSQL            s           2048    inz  varying
016300150618
016400150618       // -?Campi di comodo?
016500150618     d wDate_EUR       s               d   inz  datfmt(*EUR)
016600150618     d wOggi           s              8s 0 inz
016700150622     d wDataLim        s              8s 0 inz
016800150618
016900150619       // -?Campi per controllo "rotture" di livello?
017000150619     d wSaveTGDtor     s                   inz  like(TGDtor)
017100150619
017200150618       // -?Campi di stampa?
017300150618     d wwwMSG          s            100    inz
017400150618     d wwwDAC          s              8  0 inz
017500150618     d wwwOGG          s             21    inz
017600150619     d wwwNOT1         s                   inz  like(DB0no1)
017700150619     d wwwNOT2         s                   inz  like(DB0no2)
017800150616
017900150616       //--------------------------------------------------------------
018000150616       //?Definizione prototipi procedure.                             ?
018100150616       //--------------------------------------------------------------
018200150616
018300150616       // -?Reperimento dati utente?
018400150616     d TIBS34ds      e ds
018500150616      /copy gaitrasrc/srcProtoPR,TIBS34R
018600150720
018700150720       // -?Ricerca/Controllo tabelle?
018800150720     d TIBS02ds      e ds                  inz
018900150720      /copy gaitrasrc/srcProtoPR,TIBS02R
019000150616
019100150616       // -?Chiusura singola Richiesta Assistenza?
019200150616     d FIDNA7ds      e ds                  inz
019300150616     d fidnA7r         pr                  extpgm('FIDNA7R')
019400150616     d   fidnA7ds                          likeds(FIDNA7ds)
019500150618
019600150618       // -?Gestione Note delle Richiesta Assistenza?
019700150618     d FIDNB0ds      e ds                  inz
019800150618     d fidnB0r         pr                  extpgm('FIDNB0R')
019900150618     d   kpjba                             likeds(KPJBA)
020000150618     d   fidnB0ds                          likeds(FIDNB0ds)
020100161128
020200161128       // -?Pgm Pulizia RA
020300161128     d FIDNCADS      e ds                  inz
020400161128     d fidncar         pr                  extpgm('FIDNCAR')
020500161128     d  fidncads                           likeds(FIDNCADS)
020600150619
020700150619       // -?Parametri API QCAPCMD (Process Commands)?
020800150619     d Qcmd            s           2048    inz  varying
020900150619      /copy qSysInc/qRpgleSrc,QCAPCMD
021000150619       // -?API QCAPCMD (Process Commands)?
021100150619      /copy gaitrasrc/srcProtoPR,QCAPCMD
021200150619
021300150619       // -?Parametri gestione errori API.?
021400150619      /copy qsysinc/qrpglesrc,QUSEC
021500150616
021600150616       //--------------------------------------------------------------
021700150616       //?Definizione key-list.                                        ?
021800150616       //--------------------------------------------------------------
021900150616
022000150618       // -?File TITAS30C?
022100150618     d keyTITAS30    e ds                  extname( TITAS30C : *key )
022200150618     d                                     prefix( k_ )  inz
022300150618
022400150618       // -?File FNORM01L?
022500150618     d keyFNORM01    e ds                  extname( FNORM01L : *key )
022600150618     d                                     prefix( k_ )  inz
022700150616
022800150616       //--------------------------------------------------------------
022900150616       //?M A I N - L I N E                                            ?
023000150616       //--------------------------------------------------------------
023100150616
023200150616     c     *Entry        plist
023300150616     c                   parm                    KPJBA
023400150616
023500150616      /free
023600150616
023700150616       // -?Operazioni iniziali?
023800150616       exsr  sr_RoutInz;
023900150616
024000150616       // -?Preparazione SQL per estrazione R.A. aperte da più di un anno?
024100150616       exsr  sr_PrepSQL;
024200150616
024300150616       // -?Apertura Cursore?
024400150616       exsr  sr_OpenCursor;
024500150616
024600150616       // -?Ciclo di lettura R.A. di Sede aperte da più di un anno?
024700150616       DoW  Not $EoF;
024800150616         exsr  sr_ReadCursor;
024900150616       EndDo;
025000150616
025100150616       // -?Chiusura Cursore?
025200150616       exsr  sr_CloseCursor;
025300161128
025400161128       // -?Pulizia RA automatiche legate a bolle di recupero non più presenti?
025500161128       exsr PuliziaRaAuto;
025600150616
025700150616       // -?Operazioni finali?
025800150616       exsr  sr_RoutEnd;
025900150616
026000150616       //--------------------------------------------------------------
026100150616       //?Operazioni iniziali.                                         ?
026200150616       //--------------------------------------------------------------
026300150616       BEGSR  sr_RoutInz;
026400150617
026500150618         // -?Impostazione opzioni per SQL?
026600150618         exec sql   set  option  DynUsrPrf = *Owner,
026700150618                                 CloSqlCsr = *EndMod;
026800150616
026900150617         // -?Impostazione chiusura?
027000150616         *inLR = *on;
027100150616
027200150616         // -?Reperimento dati job?
027300150616         exsr  sr_DatiJob;
027400150616
027500150618         // -?Reperimento Data e Ora attuali?
027600150618         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
027700150618         wDate_Eur = %date( wDate : *Iso );
027800150618         wOggi     = %dec( wDate_Eur );
027900150622         wDataLim  = %dec( wDate_Eur - %years(1) );
028000150619
028100150619         $Test = ( %subst( KPJBU : 1 : 5 ) = 'TEST '  or
028200150619                   %subst( KPJBU : 1 : 5 ) = 'PROVA' );
028300150616
028400150616       ENDSR;
028500150616
028600150616       //--------------------------------------------------------------
028700150616       //?Reperimento Dati del job (Utente/Operativi).                 ?
028800150616       //--------------------------------------------------------------
028900150616       BEGSR  sr_DatiJob;
029000150616
029100150616         in(E) §AzUte;
029200150616         if NOT %error;
029300150616           in(E) §DatiUte;
029400150616         endif;
029500150616         if %error or RSut = *blanks;
029600150616           clear TIBS34ds;
029700150616           tibs34r ( tibs34ds );
029800150616           in §AzUte;
029900150616           in §DatiUte;
030000150616         endif;
030100150616
030200150616       ENDSR;
030300150617
030400150617       //--------------------------------------------------------------
030500150617       //?Preparazione stringa SQL per l'estrazione dei dati.          ?
030600150617       //--------------------------------------------------------------
030700150617       BEGSR  sr_PrepSQL;
030800150617
030900150618         clear  wSQL;
031000150618
031100150618         //wSQL = 'select * +
031200150618         //          from FITGD00F left outer join FITGN00F +
031300150618         //            on TGDant = TGNant +
031400150618         //           and TGDnut = TGNnut +
031500150618         //           and TGDpdt = TGNpdt +
031600150619         //         where TGDpot = ' + %editc( c_Sede : 'X' ) +
031700150618         //         ' and TGDcdt = '' '' +
031800150618         //         order by TGDtor, TGDdac, TGDorc, TGNtin, TGNpgn +
031900150618         //           for fetch only';
032000150618
032100150618         wSQL = 'select * +
032200150618                   from FITGD00F +
032300150619                  where TGDpot = ' + %editc( c_Sede : 'X' ) +
032400150618                  ' and TGDcdt = '' '' +
032500150622                    and TGDdac < ' + %editc( wDataLim : 'X' ) +
032600150622                ' order by TGDtor, TGDdac, TGDorc +
032700150618                    for fetch only';
032800150617
032900150617       ENDSR;
033000150617
033100150617       //--------------------------------------------------------------
033200150617       //?Apertura cursore SQL.                                        ?
033300150617       //--------------------------------------------------------------
033400150617       BEGSR  sr_OpenCursor;
033500150617
033600150617         // -?Dichiarazione cursore?
033700150617         exec sql   prepare S1   from :wSQL;
033800150617         exec sql   declare C1   cursor for S1;
033900150617
034000150617         if  SQLcode < *zero;
034100150617           $EoF = *on;
034200150618           exsr  sr_PrintErrSQL;
034300150617         endif;
034400150617
034500150617         // -?Apertura del cursore?
034600150617         exec sql   open C1;
034700150617
034800150617         if  SQLcode < *zero;
034900150617           $EoF = *on;
035000150617           exsr  sr_PrintErrSQL;
035100150617         endif;
035200150617
035300150617       ENDSR;
035400150617
035500150617       //--------------------------------------------------------------
035600150617       //?Lettura cursore.                                             ?
035700150617       //--------------------------------------------------------------
035800150617       BEGSR  sr_ReadCursor;
035900150617
036000150619         clear  FITGD_ds;
036100150617
036200150618         exec sql   fetch next   from C1   into :FITGD_ds;
036300150617
036400150618         Select;
036500150618
036600150618           // -?Fine File?
036700150618           When  SQLcode = 100;
036800150617             $EoF = *on;
036900150618
037000150618           // -?Errore SQL?
037100150618           When  SQLcode < *zero;
037200150617             $EoF = *on;
037300150617             exsr  sr_PrintErrSQL;
037400150618
037500150618           // -?Elaborazione singola R.A.?
037600150618           Other;
037700150618             exsr  sr_ElabRec;
037800150618
037900150618         EndSl;
038000150617
038100150617       ENDSR;
038200150617
038300150617       //--------------------------------------------------------------
038400150617       //?Chiusura cursore SQL.                                        ?
038500150617       //--------------------------------------------------------------
038600150617       BEGSR  sr_CloseCursor;
038700150617
038800150617         // -?Chiusura del cursore?
038900150617         exec sql   close C1;
039000150617
039100150617       ENDSR;
039200161128
039300161128       //--------------------------------------------------------------
039400161128       //?Elaborazione RA automatiche                                  ?
039500161128       //--------------------------------------------------------------
039600161128       BEGSR  PuliziaRaAuto;
039700161128
039800161128         $EoFRA = *off;
039900161128
040000161128         exec sql
040100161128         DECLARE RAAUTO cursor for
040200161128         SELECT * from FITGD00F
040300161128         WHERE TGDpru = 'EDPFAT    ' and TGDpot = 46 and
040400161128               TGDtor = 'S';
040500161128
040600161128         exec sql open RAAUTO;
040700161128
040800161128         IF  sqlcode < 0;
040900161128           exec sql close RAAUTO;
041000161128           leavesr;
041100161128         ENDIF;
041200161128
041300161128         DOW  not $EoFRA;
041400161128           exec sql
041500161128           FETCH NEXT from RAAUTO into :FITGD_ds;
041600161128           IF sqlcod = 100 or sqlcod < 0;
041700161128             $EoFRA = *on;
041800161128             leave;
041900161128           ENDIF;
042000161128
042100161128         //?Controllo se esiste ancora la bolla su TITAS
042200161128           $ChiudiRA = *off;
042300161128           exsr sr_CtrlSPED;
042400161128         //?Non esiste più la bolla cancello RA
042500161128           IF  $ChiudiRA;
042600161128             clear FIDNCADS;
042700161128             IC0ant = TGDant;
042800161128             IC0nut = TGDnut;
042900161128             IC0pdt = TGDpdt;
043000161128             fidncar (FIDNCADS);
043100161128           ENDIF;
043200161128
043300161128         ENDDO;
043400161128
043500161128         exec sql close RAAUTO;
043600161128
043700161128       ENDSR;
043800150617
043900150617       //--------------------------------------------------------------
044000150618       //?Elaborazione singola R.A.                                    ?
044100150617       //--------------------------------------------------------------
044200150618       BEGSR  sr_ElabRec;
044300150617
044400150618         clear  $ChiudiRA;
044500150618         clear  wwwMSG;
044600150619
044700150619         wwwOGG = TGDogg;
044800150618
044900150618         Select;
045000150618
045100150618           // -?Tipo Oggetto GENERICO: R.A. solo stampata?
045200150618           When  TGDtor = 'G';
045300150619             if  TGDtor <> wSaveTGDtor;
045400150619               wwwMSG = 'R.A. con tipo oggetto GENERICO:';
045500150619             endif;
045600150618             exsr  sr_StampaRA;
045700150915             wSaveTGDtor = TGDtor;
045800150618
045900150618           // -?Tipo Oggetto O.R.M.: controllo SE "chiudibile"?
046000150618           When  TGDtor = 'O';
046100150618             exsr  sr_CtrlORM;
046200150618             if  $ChiudiRA;
046300150915               if  $Test;
046400150915                 if  TGDtor <> wSaveTGDtor;
046500150915                   wwwMSG = 'R.A. chiuse per O.R.M. non più presente:';
046600150915                 endif;
046700150915                 exsr  sr_StampaRA;
046800150915               endif;
046900150618               exsr  sr_ChiusuraRA;
047000150915               wSaveTGDtor = TGDtor;
047100150618             endif;
047200150618
047300150618           // -?Tipo Oggetto SPEDIZIONE: controllo SE "chiudibile"?
047400150618           When  TGDtor = 'S';
047500150618             exsr  sr_CtrlSPED;
047600150618             if  $ChiudiRA;
047700150915               if  $Test;
047800150915                 if  TGDtor <> wSaveTGDtor;
047900150915                   wwwMSG = 'R.A. chiuse per Spedizione non più presente:';
048000150915                 endif;
048100150915                 exsr  sr_StampaRA;
048200150915               endif;
048300150618               exsr  sr_ChiusuraRA;
048400150915               wSaveTGDtor = TGDtor;
048500150618             endif;
048600150618
048700150618           // -?Tipo Oggetto ? ? ?: R.A. solo stampata?
048800150618           Other;
048900150619             if  TGDtor <> wSaveTGDtor;
049000150619               wwwMSG = 'R.A. con tipo oggetto NON previsto:';
049100150619             endif;
049200150618             exsr  sr_StampaRA;
049300150915             wSaveTGDtor = TGDtor;
049400150618
049500150618         EndSl;
049600150619
049700150617
049800150617       ENDSR;
049900150618
050000150618       //--------------------------------------------------------------
050100150618       //?Controllo O.R.M.                                             ?
050200150618       //--------------------------------------------------------------
050300150618       BEGSR  sr_CtrlORM;
050400150618
050500150618         WRKorm_ds = TGDogg;
050600150618
050700150618         // -?Impostazione O.R.M. in stampa?
050800150618         wwwOGG = WRKpoe + '/' + WRKnso + '/' + WRKnor + '/' + WRKnrv;
050900150618
051000150618         // -?Verifica esistenza O.R.M.?
051100150619         if  NOT %open(FNORM01L);
051200150619           open  FNORM01L;
051300150619         endif;
051400150618         clear  keyFNORM01;
051500150618         k_ORMpoe = %int( WRKpoe );
051600150618         k_ORMnsr = %int( WRKnso );
051700150618         k_ORMnor = %int( WRKnor );
051800150618         k_ORMnrv = %int( WRKnrv );
051900150618
052000150618         chain  %kds( keyFNORM01 )  FNORM000;
052100150618
052200150618         // -?O.R.M. ancora presente => uscita?
052300150618         if  %found( FNORM01L );
052400150618           leavesr;
052500150618         endif;
052600150618
052700150618         // -?O.R.M. non più presente (R.A. da chiudere)?
052800150618         $ChiudiRA = *on;
052900150618
053000150618       ENDSR;
053100150618
053200150618       //--------------------------------------------------------------
053300150618       //?Controllo SPEDIZIONE.                                        ?
053400150618       //--------------------------------------------------------------
053500150618       BEGSR  sr_CtrlSPED;
053600150618
053700150618         WRKspe_ds = TGDogg;
053800150618
053900150618         // -?Impostazione SPEDIZIONE in stampa?
054000150618         wwwOGG = WRKlnp + '/' + WRKnrs + '/' + WRKnsp + ' ' + WRKaas;
054100150618
054200150618         // -?Verifica esistenza SPEDIZIONE?
054300150619         if  NOT %open(TITAS30C);
054400150619           open  TITAS30C;
054500150619         endif;
054600150618         clear  keyTITAS30;
054700150618         k_TASaas = %int( WRKaas );
054800150618         k_TASlnp = %int( WRKlnp );
054900150618         k_TASnrs = %int( WRKnrs );
055000150618         k_TASnsp = %int( WRKnsp );
055100150618
055200150915         chain  %kds( keyTITAS30 : 4 )  TITAS30C;
055300150618
055400150916         // -?Spedizione ancora presente => uscita?
055500150618         if  %found( TITAS30C );
055600150618           leavesr;
055700150618         endif;
055800150618
055900150916         // -?Spedizione non più presente (R.A. da chiudere)?
056000150618         $ChiudiRA = *on;
056100150618
056200150618       ENDSR;
056300150618
056400150618       //--------------------------------------------------------------
056500150618       //?Stampa singola R.A.                                          ?
056600150618       //--------------------------------------------------------------
056700150618       BEGSR  sr_StampaRA;
056800150619
056900150720         If  NOT %open(PrtEMail);
057000150720
057100150720           // -?Reperimento tab. MRA = Bart-Maling - Danni?
057200150720           clear  dMRAdan;
057300150720           clear  TIBS02ds;
057400150720           T02mod = 'C';
057500150720           //T02sif = knsif;
057600150720           T02cod = 'MRA';
057700150720           T02ke1 = %trimr(SDSpgm);
057800150720
057900150720           TNTBE_RicercaControllo ( kpjba : tibs02ds );
058000150720
058100150720           if T02err = *blanks;
058200150720             dMRAdan = T02uni;
058300150720           endif;
058400150720
058500150720           // -?Esecuzione override?
058600150720           reset  TRTCM1ds;
058700150720           If  §MRAdReg <> *blank;
058800150720
058900150720             §CM1mitt = %trim(§MRAdMitt);
059000150720             §CM1dst  = §MRAdDest;
059100150720             §CM1tips = §MRAdReg;
059200150720             §CM1po   = %editc( DUTpou : 'X' );
059300150720             //§CM1var  = '*OBJM*' + §MRAdDes;
059400150720             §CM1idp  = §MRAdIdPro;
059500150720
059600150720             Qcmd = c_CmdOvrPrtF
059700150720                  + ' outq(' + %trim(§MRAdOutqI) + ')'
059800150720                  + ' usrdfndta(''' + TRTCM1ds + ''')';
059900150720             exsr  sr_ExecCmd;
060000150720
060100150720           EndIf;
060200150720
060300150720           open  PrtEMail;
060400150720           *inOF = *on;
060500150720
060600150720         EndIf;
060700150619
060800150720         // -?Stampa "Testata" Messaggi?
060900150619         if  wwwMSG <> *blank;
061000150720           if  *inOF;
061100150720             except  PRTtxt;
061200150720             if  $Test;
061300150720               except  PRTtxt2;
061400150720             endif;
061500150720           endif;
061600150720           except  PRTmsg;
061700150720           except  PRTtxt3;
061800150720           *inOF = *off;
061900150619         endif;
062000150618
062100150618         // -?Data Creazione?
062200150618         wDate_Eur = %date( TGDdac : *Iso );
062300150618         wwwDAC    = %dec( wDate_Eur );
062400150618
062500150618         // -?Reperimento eventuali Note?
062600150618         clear  FIDNB0ds;
062700150618         iB0ant = TGDant;
062800150618         iB0nut = TGDnut;
062900150618         iB0pdt = TGDpdt;
063000150618         iB0tin = 'GEN';
063100150618         iB0flm = 'R';
063200150618
063300150618         fidnB0r ( kpjba : fidnB0ds );
063400150618
063500150618         // -?Stampa?
063600150720         if  *inOF;
063700150720           except  PRTtxt;
063800150720           if  $Test;
063900150720             except  PRTtxt2;
064000150720           endif;
064100150720           except  PRTtxt3;
064200150720           *inOF = *off;
064300150720         endif;
064400150720
064500150720         except  PRTdet;
064600150720
064700150720         if  DB0no1 <> *blank  or  DB0no2 <> *blank;
064800150720           wwwNOT1 = DB0no1;
064900150720           wwwNOT2 = DB0no2;
065000150720           except  PRTdet2;
065100150720         endif;
065200150618
065300150618       ENDSR;
065400150618
065500150618       //--------------------------------------------------------------
065600150618       //?Chiusura singola R.A.                                        ?
065700150618       //--------------------------------------------------------------
065800150618       BEGSR  sr_ChiusuraRA;
065900150619
066000150619         if  $Test;
066100150619           leavesr;
066200150619         endif;
066300150618
066400150618         clear  fidnA7ds;
066500150619         iDNA7ant = TGDant;
066600150619         iDNA7nut = TGDnut;
066700150619         iDNA7pdt = TGDpdt;
066800150622         iDNA7pct = c_Sede;
066900150622         iDNA7utc = 'BATCH     ';
067000150622         iDNA7cdt = c_CodChiusura;
067100150619         iDNA7not = 'Chiusura automatica';
067200150619         select;
067300150619           when  TGDtor = 'O';
067400150619             iDNA7not = %trimR( iDNA7not ) + ': O.R.M. non più presente';
067500150619           when  TGDtor = 'S';
067600150619             iDNA7not = %trimR( iDNA7not ) + ': Spedizione non più presente';
067700150619         endsl;
067800150618
067900150618         fidnA7r ( fidnA7ds );
068000150618
068100150618       ENDSR;
068200150617
068300150617       //--------------------------------------------------------------
068400150617       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
068500150617       //--------------------------------------------------------------
068600150617       BEGSR  sr_PrintErrSQL;
068700150617
068800150617         // -?Stampa del Dump?
068900150617         Dump(A);
069000150617
069100150617         // -?Stampa del Job-Log?
069200150617         Qcmd = 'DSPJOBLOG job(*) output(*print)';
069300150617         exsr  sr_ExecCmd;
069400150617
069500150617         // -?Chiusura del programma?
069600150617         exsr  sr_RoutEnd;
069700150617
069800150617       ENDSR;
069900150617
070000150617       //--------------------------------------------------------------
070100150617       //?Esecuzione del comando (già impostato).                      ?
070200150617       //--------------------------------------------------------------
070300150617       BEGSR  sr_ExecCmd;
070400150617
070500150617         clear Qcap0100;
070600150617         Qcabcsdh = *off;
070700150617         Qcapa    = *off;
070800150617         Qcacmdss = *off;
070900150617         Qcaerved = *allX'00';
071000150617
071100150617         clear Qusec;
071200150617         Qusbprv  = %size(Qusec);
071300150617
071400150617         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
071500150617                           %size(Qcap0100) : 'CPOP0100' : *omit :
071600150617                           0 : 0 : Qusec);
071700150617
071800150617         //if  Qusei <> *blank;
071900150617         //  ...;
072000150617         //endif;
072100150617
072200150617       ENDSR;
072300150616
072400150616       //--------------------------------------------------------------
072500150616       //?Operazioni finali.                                           ?
072600150616       //--------------------------------------------------------------
072700150616       BEGSR  sr_RoutEnd;
072800150618
072900150618         // -?SE chiusa/stampata almeno una R.A.:?
073000150618         //  ?stampa di "fine" e chiusura del file di stampa?
073100150720         if  %open(PrtEMail);
073200150618           except  PRTend;
073300150720           close  PrtEMail;
073400150720           Qcmd = c_CmdDltOvr;
073500150720           exsr  sr_ExecCmd;
073600150618         endif;
073700150618
073800150618         // -?Chiusura applicazioni precedentemente aperte?
073900150618         clear  FIDNB0ds;
074000150618         iB0tla = 'C';
074100150618         fidnB0r ( kpjba : fidnB0ds );
074200161128
074300161128         clear  FIDNCADS;
074400161128         IC0tla = 'C';
074500161128         fidncar ( FIDNCADS );
074600150720
074700150720         clear  TIBS02ds;
074800150720         T02tla = 'C';
074900150720         TNTBE_RicercaControllo ( kpjba : tibs02ds );
075000150616
075100150616         // -?Chiusura pgm?
075200150616         return;
075300150616
075400150616       ENDSR;
075500150616
075600150616      /end-free
075700150618
075800150618       //--------------------------------------------------------------
075900150618       //?S P O O L - F I L E S                                        ?
076000150618       //--------------------------------------------------------------
076100150618
076200150720     oPrtEMail  e            PRTtxt            3
076300150618     o                       RSUT
076400150720     o                                        +   5 'LISTA R.A. DI SEDE DA +
076500150720     o                                               CHIUDERE POICHÈ APERTE +
076600150618     o                                               DA PIÙ DI UN ANNO'
076700150618     o                       SDSpgm           +   5
076800150618     o                       wOggi         y  +   5
076900150618     o          e            PRTtxt      0
077000150720     o                                        +  25 'LISTA R.A. DI SEDE DA +
077100150720     o                                               CHIUDERE POICHÈ APERTE +
077200150618     o                                               DA PIÙ DI UN ANNO'
077300150619     o          e            PRTtxt      1
077400150618     o                       KNSIF
077500150618     o                       KNMUS            +   1
077600150720     o                                        +   4 '----------------------+
077700150720     o                                               -----------------------+
077800150618     o                                               -----------------'
077900150618     o                                        +   5 'Pag.'
078000150618     o                       Page          z  +   0
078100150618     o                       wTime            +   7 '  :  :  '
078200150619     o          e            PRTtxt      0  1
078300150720     o                                        +  25 '----------------------+
078400150720     o                                               -----------------------+
078500150618     o                                               -----------------'
078600150622      *
078700150622     o          e            PRTtxt2     2
078800150622     o                                        +  25 '*  SOLO STAMPA  *'
078900150720     o          e            PRTtxt2     0
079000150622     o                                        +  25 '*  SOLO STAMPA  *'
079100150720      *
079200150720     o          e            PRTmsg      2  1
079300150720     o                       wwwMSG
079400150618      *
079500150720     o          e            PRTtxt3     2
079600150618     o                                              'Richiesta     '
079700150618     o                                        +   1 '   '
079800150618     o                                        +   1 'Inserita da'
079900150618     o                                        +   1 ' il giorno'
080000150618     o                                        +   1 '  alle  '
080100150618     o                                        +   1 'Cliente'
080200150619     o                                        +   1 '                    -
080300150619     o                                                             '
080400150720     o                                        +   1 'Oggetto               '
080500150720     o*//                                     +   1 'Note                -
080600150720     o*//                                                          '
080700150622     o          e            PRTtxt3     1
080800150618     o                                              '=============='
080900150618     o                                        +   1 '==='
081000150618     o                                        +   1 '==========='
081100150618     o                                        +   1 '=========='
081200150618     o                                        +   1 '========'
081300150618     o                                        +   1 '======='
081400150619     o                                        +   1 '====================-
081500150619     o                                              ==============='
081600150720     o                                        +   1 '======================'
081700150720     o*//                                     +   1 '====================-
081800150720     o*//                                           ==============='
081900150619      *//*
082000150619     o*//       e            PRTnull     1
082100150618      *
082200150618     o          e            PRTdet      1
082300150619     o                       TGDnut        z
082400150619     o                       TGDant        z  +   1
082500150619     o                       TGDpdt        z  +   1
082600150619     o                       TGDpru           +   1
082700150618     o                       wwwDAC        y  +   2
082800150619     o                       TGDorc           +   1 '0 :  :  '
082900150619     o                       TGDksc        z  +   1
083000150619     o                       TGDrsc           +   1
083100150619     o                       TGDtor           +   1
083200150618     o                       wwwOGG           +   1
083300150720     o          e            PRTdet2     1
083400150720     o                                        +  19 'Note:'
083500150618     o                       wwwNOT1          +   1
083600150619     o                       wwwNOT2          +   1
083700150618      *
083800150618     o          e            PRTend      2
083900150619     o                                        +  25 '***  Fine Lista  ***'
084000150618     o          e            PRTend      0
084100150619     o                                        +  25 '***  Fine Lista  ***'
