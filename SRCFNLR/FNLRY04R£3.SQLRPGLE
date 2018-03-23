000100150210       //==============================================================
000200150210       //?FNLRY04R * Scrittura file TIIDS00F (Stat.Disposiz.Cons. Web) ?
000300150210       //==============================================================
000400150210
000500150210       //--------------------------------------------------------------
000600150210       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150210       //--------------------------------------------------------------
000800150210
000900150210     /*PRM dbgview(*source)
001000150210     /*CMD ovrdbf file(TIIDC00F) tofile(UNITRAGRP/TIIDC00F) +
001100150210     /*CMD        ovrscope(*calllvl)
001200150210     /*CMD ovrdbf file(TIIDS00F) tofile(UNITRAGRP/TIIDS00F) +
001300150210     /*CMD        ovrscope(*calllvl)
001400150210     /*END dltovr file(TIIDC00F TIIDS00F) lvl(*)
001500150210     /*END
001600150210
001700150210       //--------------------------------------------------------------
001800150210       //?Specifiche di controllo.                                     ?
001900150210       //--------------------------------------------------------------
002000150210
002100150210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200150210     h dftactgrp(*no)
002300150210     h bnddir('TRUL')
002400150210     h alwnull(*inputonly)
002500150210
002600150210       //--------------------------------------------------------------
002700150210       //?Dichiarazione file.                                          ?
002800150210       //--------------------------------------------------------------
002900150210
003000150210       // -?Statistica Disposizioni di Consegna da Internet?
003100150210     fTIIDS00F  o    e             disk    extfile(wLibFile)
003200150210     f                                     usropn
003300150210
003400150210       // -?File di stampa per eventuale invio e-mail?
003500150210     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003600150210
003700150210       //--------------------------------------------------------------
003800150210       //?Definizione costanti.                                        ?
003900150210       //--------------------------------------------------------------
004000150210
004100150210      *// // -?Numero giorni da estrarre da TIIDC00F: 7?
004200150210     d*// c_NumeroGiorniDaEstrarre...
004300150210     d*//                 c                   const(7)
004400150210      *//
004500150210      *// // -?Primo giorno da estrarre da TIIDC00F: la Domenica Precedente?
004600150210     d*// c_PrimoGiornoDaEstrarre...
004700150210     d*//                 c                   const(7)
004800150210
004900150210       // -?Comandi da eseguire?
005000150210     d c_CmdOvrPrtF    c                   const('OVRPRTF +
005100150210     d                                           file(PRTEMAIL) +
005200150210     d                                           pagesize(66 132) +
005300150210     d                                           lpi(6) cpi(10) +
005400150210     d                                           ovrscope(*actgrpdfn) +
005500150210     d                                           usrdta(''*ERR_TIIDS'') +
005600150210     d                                           ')
005700150210     d c_CmdDltOvr     c                   const('DLTOVR +
005800150210     d                                            file(PRTEMAIL) +
005900150210     d                                            lvl(*actgrpdfn)')
006000150210
006100150210       //--------------------------------------------------------------
006200150210       //?Definizione schiere.                                         ?
006300150210       //--------------------------------------------------------------
006400150210
006500150210
006600150210       //--------------------------------------------------------------
006700150210       //?Definizione aree dati.                                       ?
006800150210       //--------------------------------------------------------------
006900150210
007000150210       // -?Dati utente?
007100150210     d §AzUte        e ds                  extname(AZUTE00F)
007200150210     d                                     dtaara
007300150210     d §DatiUte      e ds                  extname(dDatiUte)
007400150210     d                                     dtaara
007500150210
007600150210       //--------------------------------------------------------------
007700150210       //?Definizione strutture dati.                                  ?
007800150210       //--------------------------------------------------------------
007900150210
008000150210       // -?Status ds?
008100150210     d Status         sds
008200150210     d   SDSpgm          *proc
008300150210     d*//SDSprm          *parms
008400150210     d*//SDSdta              191    198
008500150210     d*//SDSjob              244    253
008600150210     d   SDSusr              254    263
008700150210
008800150210       // -?Data/Ora attuali?
008900150210     d wTime_ds        ds                  inz
009000150210     d   wDate                        8s 0 inz
009100150210     d   wTime                        6s 0 inz
009200150211
009300150211       // -?Dati estratti via SQL?
009400150211     d TIIDC00F      e ds                  extname(TIIDC00F)
009500150211     d                                     based(nullPtr)
009600150211     d TIIDC_ds        ds                  inz  qualified
009700150212     d   wIDCinsData                  8  0 inz
009800150211     d   IDCfgs                            inz  like(IDCfgs)
009900150211     d   IDCtipoDis                        inz  like(IDCtipoDis)
010000150212     d   wIDCsnDir                    1    inz
010100150212     d   wIDCsnApp                    1    inz
010200150211     d   IDCtipoCli                        inz  like(IDCtipoCli)
010300150212     d   wIDCcount                    9  0 inz
010400150210
010500150210       // -?Parametri ricevuti?
010600150210     d KPJBA         e ds
010700150210     d FNLRY04ds       ds                  inz
010800150210     d   iLRY04dti                    8  0 inz(*loval)
010900150210     d   iLRY04dtf                    8  0 inz(*hival)
011000150210
011100150210       // -?Tab. "5A"/"SEDE1" (per giorni limite cancellazione dati)?
011200150210     d ds5AS1        e ds                  inz
011300150210
011400150210       // -?Tab. "MRA" = Mailing - regole x invio da gestionale?
011500150210     d dMRAdan       e ds                  inz
011600150210     d   §MRADdes    e                     inz('ERR Stat.Dispos.+
011700150210     d                                         Cons.Web')
011800150210     d   §MRADreg    e                     inz('COR')
011900150210     d   §MRADmitt   e                     inz('ced')
012000150210     d   §MRADdest   e                     inz('ced@brt.it; +
012100150210     d                                          stefano.merighi+
012200150210     d                                          @brt.it')
012300150210     d   §MRADidPro  e                     inz('2')
012400150210     d   §MRADoutqI  e                     inz('EMAILIN')
012500150210
012600150210       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
012700150210     d TRTCM1ds      e ds                  inz
012800150210         //?·§CM1mitt = Indirizzo e-mail del mittente?
012900150210     d   §CM1mitt    e                     inz('ced')
013000150210         //?·§CM1dst  = Indirizzo e-mail del destinatario?
013100150210     d   §CM1dst     e                     inz('ced@brt.it;+
013200150210     d                                          stefano.merighi+
013300150210     d                                          @brt.it')
013400150210         //?·§CM1tips = Tipo lettera via e-mail:?
013500150210         // ?"LET" = testo allegato in corpo con logo?
013600150210         //         ?(richiede righe libere iniziali per il logo)?
013700150210         // ?"COR" = testo integrato senza logo?
013800150210         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
013900150210     d   §CM1tips    e                     inz('COR')
014000150210         //?·§CM1po   = Filiale?
014100150210     d   §CM1po      e                     inz('046')
014200150210         //?·§CM1var  = Oggetto e-mail?
014300150210     d   §CM1var     e                     inz('*OBJM*+
014400150210     d                                     ERRORE in estrazione dati -
014500150210     d                                     per Statistica Disposizioni -
014600150210     d                                     di Consegna da web')
014700150210         //?·§CM1sts  = Stato?
014800150210     d   §CM1sts     e                     inz(*off)
014900150210         //?·§CM1sts  = Id processo?
015000150210     d   §CM1idp     e                     inz('2')
015100150210
015200150210       //--------------------------------------------------------------
015300150210       //?Definizione variabili globali.                               ?
015400150210       //--------------------------------------------------------------
015500150210
015600150210       // -?Flags booleani?
015700150210     d $EoF            s               n   inz
015800150210
015900150210       // -?Nome Libreria del file di input e di quello di output?
016000150210     d wLib            s             10a   inz
016100150210       // -?Nome esteso Libreria/File del file di output?
016200150210     d wLibFile        s             21a   inz
016300150210
016400150210       // -?Stringa SQL da eseguire?
016500150210     d wSQL            s           7000    inz  varying
016600150210
016700150210       // -?Data *ISO?
016800150210     d wDate_Iso       s               d   inz  datfmt(*ISO)
016900150210
017000150210       // -?Date Iniziale e Finale per estrazione dati da TIIDC00F?
017100150210     d wDataIniz       s               d   inz  datfmt(*ISO)
017200150210
017300150210       // -?Data Inserimento Richiesta su AS/400 (da TIIDC00F)?
017400150210     d wInsData        s              8  0 inz
017500150210
017600150212       // -?Campi di controllo "rottura"?
017700150212     d SaveIDCsnDir    s                   inz  like(IDSsnDir)
017800150212     d SaveIDCsnApp    s                   inz  like(IDSsnApp)
017900150210       // -?Numero totale di Disposizioni di Consegna elaborate?
018000150210       //  ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
018100150210     d wTotDisp        s                   inz  like(IDStotIDC)
018200150210
018300150210       // -?Codice errore rilevato?
018400150223     d wErr            s              3  0 inz
018500150223
018600150223       // -?Campi di stampa?
018700150223     d s_Date          s              8  0 inz
018800150210
018900150210       //--------------------------------------------------------------
019000150210       //?Definizione prototipi procedure usate.                       ?
019100150210       //--------------------------------------------------------------
019200150210
019300150210       // -?Reperimento NETA sistema AS/400 corrente?
019400150210     d currSysNeta     s              8a   inz
019500150210      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019600150210
019700150210       // -?Reperimento dati utente?
019800150210     d TIBS34ds      e ds                  inz
019900150210      /copy gaitrasrc/srcProtoPR,TIBS34R
020000150210
020100150210       // -?Reperimento dati tabelle?
020200150210      /copy gaitrasrc/srcProtoPI,TRULTAB
020300150210      /copy gaitrasrc/srcProtoPR,TRULTAB
020400150210
020500150210       // -?Parametri API QCAPCMD (Process Commands)?
020600150210     d Qcmd            s           2048    inz  varying
020700150210      /copy qSysInc/qRpgleSrc,QCAPCMD
020800150210       // -?API QCAPCMD (Process Commands)?
020900150210      /copy gaitrasrc/srcProtoPR,QCAPCMD
021000150210
021100150210       // -?Parametri gestione errori API.?
021200150210      /copy qsysinc/qrpglesrc,QUSEC
021300150210
021400150210       //--------------------------------------------------------------
021500150210       //?Definizione key-list.                                        ?
021600150210       //--------------------------------------------------------------
021700150210
021800150210
021900150210       //--------------------------------------------------------------
022000150210       //?Riepilogo indicatori utilizzati.                             ?
022100150210       //--------------------------------------------------------------
022200150210
022300150210
022400150210       //--------------------------------------------------------------
022500150210       //?M A I N - L I N E                                            ?
022600150210       //--------------------------------------------------------------
022700150210
022800150210     c     *Entry        plist
022900150210     c                   parm                    KPJBA
023000150210
023100150210      /free
023200150210
023300150210       // -?Operazioni iniziali?
023400150210       exsr  sr_RoutInz;
023500150210
023600150210       // -?Preparazione SQL per estrazione dati?
023700150210       exsr  sr_PrepSQL;
023800150210
023900150210       // -?Ciclo di elaborazione?
024000150210       exsr  sr_OpenCursor;
024100150210
024200150210       DoW  Not $EoF;
024300150210         exsr  sr_ReadCursor;
024400150210       EndDo;
024500150210
024600150210       exsr  sr_CloseCursor;
024700150210
024800150210       // -?Operazioni finali?
024900150210       exsr  sr_RoutEnd;
025000150210
025100150210       //--------------------------------------------------------------
025200150210       //?Operazioni iniziali.                                         ?
025300150210       //--------------------------------------------------------------
025400150210       BEGSR  sr_RoutInz;
025500150210
025600150210         // -?Impostazione opzioni per SQL?
025700150210         exec sql   set  option  DynUsrPrf = *Owner,
025800150210                                 CloSqlCsr = *EndMod;
025900150210
026000150210         // -?Impostazione chiusura?
026100150210         *inLR = *on;
026200150210
026300150210         // -?Verifica del sistema AS/400 corrente?
026400150210         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026500150210           exsr  sr_RoutEnd;
026600150210         endif;
026700150210
026800150210         // -?Reperimento dati job?
026900150210         exsr  sr_DatiJob;
027000150223
027100150223         // -?Reperimento data odierna (in stampa)?
027200150223         s_Date = %dec( %date() );
027300150210
027400150210         // -?Reperimento regole per mailing da tab. "MRA"?
027500150210         clear  dMRAdan;
027600150210         if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
027700150210                          *omit : *omit :
027800150210                          *omit : *omit :
027900150210                          *omit : *omit : *omit : *omit :
028000150210                          *omit : *omit : *omit : *omit :
028100150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
028200150210                          = *zero      AND
028300150210               ds_TNTBE.TBEatb = *blank;
028400150210           dMRAdan = ds_TNTBE.TBEuni;
028500150210         endif;
028600150210
028700150210         // -?Impostazione libreria e nome esatto del file?
028800150210         if  currSysNetA = 'AS888   ';
028900150210           wLib = 'UNITRAGRP';
029000150210         else;
029100150210           wLib = 'UNITRAGRU';
029200150210         endif;
029300150210         wLibFile = %trimR( wLib ) + '/TIIDS00F';
029400150210
029500150210         // -?Cancellazione vecchi record?
029600150210         exsr  sr_DeleteTIIDS;
029700150210
029800150210         // -?Gestione parametri (forzati)?
029900150210         exsr  sr_NoParms;
030000150210
030100150210         // -?Apertura file?
030200150210         open  TIIDS00F;
030300150210
030400150210       ENDSR;
030500150210
030600150210       //--------------------------------------------------------------
030700150210       //?Reperimento Dati del job (Utente/Operativi).                 ?
030800150210       //--------------------------------------------------------------
030900150210       BEGSR  sr_DatiJob;
031000150210
031100150210         in(e) §AzUte;
031200150210         if NOT %error;
031300150210           in(e) §DatiUte;
031400150210         endif;
031500150210         if %error or RSut = *blank;
031600150210           tibs34r ( tibs34ds );
031700150210           in §AzUte;
031800150210           in §DatiUte;
031900150210         endif;
032000150210
032100150210       ENDSR;
032200150210
032300150210       //--------------------------------------------------------------
032400150210       //?Cancellazione vecchi record da TIIDS00F.                     ?
032500150210       //--------------------------------------------------------------
032600150210       BEGSR  sr_DeleteTIIDS;
032700150210
032800150210         // -?Reperimento tab. "5A/SEDE1"?
032900150210         clear  ds5AS1;
033000150210         if  getTabella ( c_Tabel : '5A'  : 'SEDE1' : *omit :
033100150210                          *omit : *omit :
033200150210                          *omit : *omit :
033300150210                          *omit : *omit : *omit : *omit :
033400150210                          *omit : *omit : *omit : *omit :
033500150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
033600150210                        = *zero      AND
033700150210             ds_TNTBE.TBEatb = *blank;
033800150210           ds5AS1 = ds_TNTBE.TBEuni;
033900150210         endif;
034000150210
034100150213         if  §5AS1ids = *zero;
034200150210           leavesr;
034300150210         endif;
034400150210
034500150210
034600150210         // -?Cancellazione vecchi record dal file?
034700150210         clear  wSQL;
034800150210         wSQL = 'DELETE from ' + %trim(wLibFile) +
034900150210                ' where IDCinsData < ' +
035000150213                %subst( %char( %dec( %timestamp() - %months( §5AS1ids ) ) )
035100150210                        : 1 : 8 );
035200150210
035300150210         exec SQL   execute immediate :wSQL;
035400150210
035500150210       ENDSR;
035600150210
035700150210       //--------------------------------------------------------------
035800150210       //?Calcolo range di date da ultima estrazione a ieri.           ?
035900150210       //--------------------------------------------------------------
036000150210       BEGSR  sr_NoParms;
036100150210
036200150210         // -?Reperimento data e ora attuali?
036300150210         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
036400150210
036500150210         // -?Preparazione SQL per reperimento ultima data di estrazione?
036600150210         clear  wSQL;
036700150210         wSQL = 'select coalesce( max( IDSinsData ), 0 ) +
036800150210                   from ' + %trim( wLibFile ) +
036900150210                  ' for fetch only';
037000150210
037100150210         // -?Dichiarazione cursore?
037200150210         exec sql   prepare S0   from :wSQL;
037300150210         exec sql   declare C0   cursor   for S0;
037400150210
037500150210         // -?Apertura del cursore?
037600150210         exec sql   open C0;
037700150210
037800150210         if  SQLcode < *zero;
037900150210           $EoF = *on;
038000150210           wErr = 1;
038100150210           exsr  sr_PrintErrSQL;
038200150210         endif;
038300150210
038400150210         // -?Lettura cursore?
038500150210         clear  IDSinsData;
038600150210
038700150210         exec sql   fetch   from C0   into :IDSinsData;
038800150210
038900150210         Select;
039000150210
039100150210           // -?Errore in elaborazione: si chiude il *pgm?
039200150210           When  SQLcode < *zero;
039300150210             $EoF = *on;
039400150210             wErr = 1;
039500150210             exsr  sr_PrintErrSQL;
039600150210
039700150210           // -?File vuoto?
039800150210           When  SQLcode = 100  or  IDSinsData = *zero;
039900150213             wDataIniz = %date( wDate : *Iso ) - %months( §5AS1ids );
040000150210
040100150210           // -?Si riparte dall'ultima data?
040200150210           Other;
040300150210             wDataIniz = %date( IDSinsData : *Iso );
040400150210
040500150210         EndSl;
040600150210
040700150210         // -?Chiusura del cursore?
040800150210         exec sql   close C0;
040900150210
041000150210         // -?Impostazione date - parametri - per l'estrazione?
041100150210         //  ?dal 1° giorno successivo all'ultimo estratto   a ieri?
041200150210         clear  FNLRY04ds;
041300150210         iLRY04dti = %dec( wDataIniz + %days(1) );
041400150210         //iLRY04dtf = %dec( %date() - %days(1) );
041500150210         wDate_Iso = %date( wDate : *Iso ) - %days(1);
041600150210         iLRY04dtf = %dec( wDate_Iso );
041700150210
041800150210         // -?Test fattibilità?
041900150210         if  iLRY04dti > iLRY04dtf;
042000150210           exsr  sr_RoutEnd;
042100150210         endif;
042200150210
042300150210       ENDSR;
042400150210
042500150210       //--------------------------------------------------------------
042600150210       //?Preparazione SQL.                                            ?
042700150210       //--------------------------------------------------------------
042800150210       BEGSR  sr_PrepSQL;
042900150210
043000150210         // -?Preparazione SQL?
043100150210         clear  wSQL;
043200150212         wSQL = 'with TIIDC as +
043300150212                      ( select int( IDCinsData / 1000000 ) as wIDCinsData, +
043400150212                               IDCfgs, IDCtipoDis, +
043500150212                               case IDCnewLna when 0 then ''N'' +
043600150212                                              else ''S'' end as wIDCsnDir, +
043700150212                               case IDCdcr    when 0 then ''N'' +
043800150212                                              else ''S'' end as wIDCsnApp, +
043900150212                               IDCtipoCli +
044000150212                          from TIIDC00F +
044100150212                         where IDCip <> '' '' +
044101170216                           and substr(idcalert, 10, 1)='' '' +
044200150212                           and IDCinsData between ' +
044300150212                                          %trim( %editc( iLRY04dti : '3' ) ) +
044400150212                                          '000000 and ' +
044500150212                                          %trim( %editc( iLRY04dtf : '3' ) ) +
044600150212                                          '999999 ) +
044700150212                 select TIIDC.*, count(*) +
044800150212                   from TIIDC +
044900150212                  group by wIDCinsData, IDCfgs, IDCtipoDis, +
045000150212                           wIDCsnDir, wIDCsnApp, IDCtipoCli +
045100150212                  order by wIDCinsData, IDCfgs, IDCtipoDis, +
045200150212                           wIDCsnDir, wIDCsnApp, IDCtipoCli +
045300150212                    for fetch only';
045400150210
045500150210         exec sql   prepare S1   from :wSQL;
045600150210
045700150210         // -?Dichiarazione cursore?
045800150210         exec sql   declare C1   cursor for S1;
045900150210
046000150210         if  SQLcode < *zero;
046100150210           $EoF = *on;
046200150210           wErr = 2;
046300150210           exsr  sr_PrintErrSQL;
046400150210         endif;
046500150210
046600150210       ENDSR;
046700150210
046800150210       //--------------------------------------------------------------
046900150210       //?Apertura cursore.                                            ?
047000150210       //--------------------------------------------------------------
047100150210       BEGSR  sr_OpenCursor;
047200150210
047300150210         // -?Apertura del cursore?
047400150210         exec sql   open C1;
047500150210
047600150210         if  SQLcode < *zero;
047700150210           $EoF = *on;
047800150210           wErr = 2;
047900150210           exsr  sr_PrintErrSQL;
048000150210         endif;
048100150210
048200150210       ENDSR;
048300150210
048400150210       //--------------------------------------------------------------
048500150210       //?Lettura cursore.                                             ?
048600150210       //--------------------------------------------------------------
048700150210       BEGSR  sr_ReadCursor;
048800150210
048900150210         clear  TIIDC_ds;
049000150210
049100150210         exec sql   fetch next   from C1   into :TIIDC_ds;
049200150210
049300150210         Select;
049400150210
049500150210           // -?Fine File?
049600150210           When  SQLcode = 100;
049700150210             $EoF = *on;
049800150210
049900150210           // -?Errore SQL?
050000150210           When  SQLcode < *zero;
050100150210             $EoF = *on;
050200150210             wErr = 2;
050300150210             exsr  sr_PrintErrSQL;
050400150210
050500150210           // -?Elaborazione singolo record?
050600150210           Other;
050700150210             exsr  sr_ElabRec;
050800150210
050900150210         EndSl;
051000150210
051100150210       ENDSR;
051200150210
051300150210       //--------------------------------------------------------------
051400150210       //?Elaborazione singolo record da TIIDC00F.                     ?
051500150210       //--------------------------------------------------------------
051600150210       BEGSR  sr_ElabRec;
051700150210
051800150210         // -?Scrittura record in TIIDS00F a Rottura...?
051900150212         If  TIIDC_ds.wIDCinsData <> IDSinsData      or
052000150212             TIIDC_ds.IDCfgs      <> IDSfgs          or
052100150212             TIIDC_ds.IDCtipoDis  <> IDStipoDis      or
052200150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
052300150212             TIIDC_ds.wIDCsnDir   <> SaveIDCsnDir )  or
052400150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
052500150212             TIIDC_ds.wIDCsnDir   =  'N'            and
052600150212             TIIDC_ds.wIDCsnApp   <> SaveIDCsnApp )  or
052700150212             TIIDC_ds.IDCtipoCli  <> IDStipoCli;
052800150210
052900150210           exsr  sr_WriteTIIDS;
053000150210
053100150210         EndIf;
053200150210
053300150211         // -?Incremento Totale Disposizioni (del tipo estratto)?
053400150212         wTotDisp += TIIDC_ds.wIDCcount;
053500150210
053600150210       ENDSR;
053700150210
053800150210       //--------------------------------------------------------------
053900150210       //?Chiusura cursore.                                            ?
054000150210       //--------------------------------------------------------------
054100150210       BEGSR  sr_CloseCursor;
054200150210
054300150210         // -?Scrittura ultimo record di Totali in TIIDS00F?
054400150210         exsr  sr_WriteTIIDS;
054500150210
054600150210         // -?Chiusura del cursore?
054700150210         exec sql   close C1;
054800150210
054900150210       ENDSR;
055000150210
055100150210       //--------------------------------------------------------------
055200150210       //?Scrittura record nel file TIIDS00F.                          ?
055300150210       //--------------------------------------------------------------
055400150210       BEGSR  sr_WriteTIIDS;
055500150212
055600150212         SaveIDCsnDir = TIIDC_ds.wIDCsnDir;
055700150212         SaveIDCsnApp = TIIDC_ds.wIDCsnApp;
055800150210
055900150210         // -?Scrittura record in TIIDS00F?
056000150210         If  wTotDisp <> *zero;
056100150210
056200150210           IDStotIDC  = wTotDisp;
056300150210
056400150210           //______________
056500150210           write  TIIDS000;
056600150210           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056700150210
056800150210         EndIf;
056900150210
057000150210         // -?Memorizzazione dei nuovi dati in TIIDS00F?
057100150210         clear  wTotDisp;
057200150210         clear  TIIDS000;
057300150210
057400150212         IDSinsData = TIIDC_ds.wIDCinsData;
057500150211         IDSfgs     = TIIDC_ds.IDCfgs;
057600150211         IDStipoDis = TIIDC_ds.IDCtipoDis;
057700150211         if  TIIDC_ds.IDCtipoDis  = '3';
057800150212           IDSsnDir  = TIIDC_ds.wIDCsnDir;
057900150212           if  TIIDC_ds.wIDCsnDir = 'N';
058000150212             IDSsnApp  = TIIDC_ds.wIDCsnApp;
058100150210           endif;
058200150210         endif;
058300150211         IDStipoCli = TIIDC_ds.IDCtipoCli;
058400150210
058500150210       ENDSR;
058600150210
058700150210       //--------------------------------------------------------------
058800150210       //?Stampa segnalazione dell'errore rilevato via SQL             ?
058900150210       //--------------------------------------------------------------
059000150210       BEGSR  sr_PrintErrSQL;
059100150210
059200150210         // -?Stampa del Dump?
059300150210         Dump(A);
059400150210
059500150210         // -?Stampa del Job-Log?
059600150210         Qcmd = 'DSPJOBLOG job(*) output(*print)';
059700150210         exsr  sr_ExecCmd;
059800150210
059900150210         // -?Invio e-mail con errore rilevato?
060000150210         exsr  sr_SendEmail;
060100150210
060200150210         // -?Chiusura del programma?
060300150210         exsr  sr_RoutEnd;
060400150210
060500150210       ENDSR;
060600150210
060700150210       //--------------------------------------------------------------
060800150210       //?Invio e-mail di avviso per l'errore rilevato via SQL.        ?
060900150210       //--------------------------------------------------------------
061000150210       BEGSR  sr_SendEmail;
061100150210
061200150210         // -?Override al file di stampa ed apertura dello stesso?
061300150210         if  NOT  %open(PrtEMAIL);
061400150210           §CM1mitt = %trim(§MRADmitt);
061500150210           §CM1dst  = %trim(§MRADdest);
061600150210           §CM1tips = §MRAdreg;
061700150210           §CM1var  = '*OBJM*' + §MRADdes;
061800150210           §CM1idp  = §MRADidPro;
061900150210           Qcmd = c_CmdOvrPrtF
062000150210                + ' outq(' + %trim(§MRADoutqI) + ')'
062100150210                + ' usrdfndta(''' + TRTCM1ds + ''')';
062200150210           exsr  sr_ExecCmd;
062300150210           open  PrtEMAIL;
062400150210           except  PRTtxt;
062500150210         endif;
062600150210
062700150210         // -?Preparazione e-Mail per il singolo errore rilevato?
062800150210         Select;
062900150210
063000150210           // -?Errore rilevato nel reperimento dell'ultima data?
063100150210           //  ?già estratta nel file TIIDS00F?
063200150210           When  wErr = 1;
063300150210             if  *inOF;
063400150210               except  PRTtxt;
063500150210               *inOF = *off;
063600150210             endif;
063700150210             except  PRTerr01;
063800150210
063900150210           // -?Errore rilevato nell'estrazione dati via SQL dal?
064000150210           //  ?file TIIDC00F?
064100150210           When  wErr = 2;
064200150210             if  *inOF;
064300150210               except  PRTtxt;
064400150210               *inOF = *off;
064500150210             endif;
064600150210             except  PRTerr02;
064700150210
064800150210         EndSl;
064900150210
065000150210         // -?Fine lista?
065100150210         except  PRTend;
065200150210
065300150210         // -?Chiusura file di stampa?
065400150210         close  PrtEMAIL;
065500150210
065600150210       ENDSR;
065700150210
065800150210       //--------------------------------------------------------------
065900150210       //?Esecuzione del comando (già impostato).                      ?
066000150210       //--------------------------------------------------------------
066100150210       BEGSR  sr_ExecCmd;
066200150210
066300150210         clear Qcap0100;
066400150210         Qcabcsdh = *off;
066500150210         Qcapa    = *off;
066600150210         Qcacmdss = *off;
066700150210         Qcaerved = *allX'00';
066800150210
066900150210         clear Qusec;
067000150210         Qusbprv  = %size(Qusec);
067100150210
067200150210         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
067300150210                           %size(Qcap0100) : 'CPOP0100' : *omit :
067400150210                           0 : 0 : Qusec);
067500150210
067600150210         //if  Qusei <> *blank;
067700150210         //  ...;
067800150210         //endif;
067900150210
068000150210       ENDSR;
068100150210
068200150210       //--------------------------------------------------------------
068300150210       //?Operazioni finali.                                           ?
068400150210       //--------------------------------------------------------------
068500150210       BEGSR  sr_RoutEnd;
068600150210
068700150210         // -?Chiusura funzioni precedentemente aperte?
068800150210         cloTabella();
068900150210
069000150210         // -?Chiusura file in output?
069100150210         close  TIIDS00F;
069200150210
069300150210         // -?SE stampato almeno un Errore:?
069400150210         //  ?stampa "fine" e chiude il file di stampa?
069500150210         if  %open(PrtEMAIL);
069600150210           except  PRTend;
069700150210           close   PrtEMAIL;
069800150210         endif;
069900150210
070000150210         // -?Uscita dal *pgm?
070100150210         return;
070200150210
070300150210       ENDSR;
070400150210
070500150210      /end-free
070600150210
070700150210       //--------------------------------------------------------------
070800150210       //?S P O O L - F I L E S                                        ?
070900150210       //--------------------------------------------------------------
071000150210
071100150210     oPrtEMAIL  e            PRTtxt            1
071200150210     o                       RSUT
071300150210     o                                        +   5 'STATISTICA DISPOSIZIONI DI-
071400150210     o                                               CONSEGNA DA WEB: ERRORI RI-
071500150210     o                                              LEVATI'
071600150210     o                       SDSpgm           +   5
071700150223     o                       s_Date        y  +   5
071800150210     o          e            PRTtxt      1
071900150210     o                       KNSIF
072000150210     o                       KNMUS            +   1
072100150210     o                                        +   4 '---------------------------
072200150210     o                                              ----------------------------
072300150210     o                                              ------'
072400150210     o                                        +   5 'Pag.'
072500150210     o                       Page          z  +   0
072600150210     o                       wTime            +   5 '  :  :  '
072700150210      *
072800150210     o          e            PRTerr01    2
072900150210     o                                              'Rilevato ERRORE n-
073000150210     o                                              el reperimento del-
073100150210     o                                              l''ultima data già-
073200150210     o                                               estratta.'
073300150210     o          e            PRTerr02    2
073400150210     o                                              'Rilevato ERRORE n-
073500150210     o                                              ella lettura delle-
073600150210     o                                               Disposizioni di C-
073700150210     o                                              onsegna via Web (f-
073800150210     o                                              ile TIIDC00F).'
073900150210      *
074000150210     o          e            PRTend      2
074100150210     o                                        +  10 '***  Fine Lista  ***'
