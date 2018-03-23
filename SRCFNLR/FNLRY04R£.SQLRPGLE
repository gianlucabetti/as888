000100150210       //==============================================================
000200150210       //?FNLRY04R * Scrittura file TIIDS00F (Stat.Disposiz.Cons. Web) ?
000300150210       //==============================================================
000400150210
000500150210       //--------------------------------------------------------------
000600150210       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150210       //--------------------------------------------------------------
000800150210
000900150210     /*PRM dbgview(*source)
001000150210     /*END
001100150210
001200150210       //--------------------------------------------------------------
001300150210       //?Specifiche di controllo.                                     ?
001400150210       //--------------------------------------------------------------
001500150210
001600150210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150210     h dftactgrp(*no)
001800150210     h bnddir('TRUL')
001900150210     h alwnull(*inputonly)
002000150210
002100150210       //--------------------------------------------------------------
002200150210       //?Dichiarazione file.                                          ?
002300150210       //--------------------------------------------------------------
002400150210
002500150210       // -?Statistica Disposizioni di Consegna da Internet?
002600150210     fTIIDS00F  o    e             disk    extfile(wLibFile)
002700150210     f                                     usropn
002800150210
002900150210       // -?File di stampa per eventuale invio e-mail?
003000150210     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003100150210
003200150210       //--------------------------------------------------------------
003300150210       //?Definizione costanti.                                        ?
003400150210       //--------------------------------------------------------------
003500150210
003600150210      *// // -?Numero giorni da estrarre da TIIDC00F: 7?
003700150210     d*// c_NumeroGiorniDaEstrarre...
003800150210     d*//                 c                   const(7)
003900150210      *//
004000150210      *// // -?Primo giorno da estrarre da TIIDC00F: la Domenica Precedente?
004100150210     d*// c_PrimoGiornoDaEstrarre...
004200150210     d*//                 c                   const(7)
004300150210
004400150210       // -?Comandi da eseguire?
004500150210     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004600150210     d                                           file(PRTEMAIL) +
004700150210     d                                           pagesize(66 132) +
004800150210     d                                           lpi(6) cpi(10) +
004900150210     d                                           ovrscope(*actgrpdfn) +
005000150210     d                                           usrdta(''*ERR_TIIDS'') +
005100150210     d                                           ')
005200150210     d c_CmdDltOvr     c                   const('DLTOVR +
005300150210     d                                            file(PRTEMAIL) +
005400150210     d                                            lvl(*actgrpdfn)')
005500150210
005600150210       //--------------------------------------------------------------
005700150210       //?Definizione schiere.                                         ?
005800150210       //--------------------------------------------------------------
005900150210
006000150210
006100150210       //--------------------------------------------------------------
006200150210       //?Definizione aree dati.                                       ?
006300150210       //--------------------------------------------------------------
006400150210
006500150210       // -?Dati utente?
006600150210     d §AzUte        e ds                  extname(AZUTE00F)
006700150210     d                                     dtaara
006800150210     d §DatiUte      e ds                  extname(dDatiUte)
006900150210     d                                     dtaara
007000150210
007100150210       //--------------------------------------------------------------
007200150210       //?Definizione strutture dati.                                  ?
007300150210       //--------------------------------------------------------------
007400150210
007500150210       // -?Status ds?
007600150210     d Status         sds
007700150210     d   SDSpgm          *proc
007800150210     d*//SDSprm          *parms
007900150210     d*//SDSdta              191    198
008000150210     d*//SDSjob              244    253
008100150210     d   SDSusr              254    263
008200150210
008300150210       // -?Data/Ora attuali?
008400150210     d wTime_ds        ds                  inz
008500150210     d   wDate                        8s 0 inz
008600150210     d   wTime                        6s 0 inz
008700150211
008800150211       // -?Dati estratti via SQL?
008900150211     d TIIDC00F      e ds                  extname(TIIDC00F)
009000150211     d                                     based(nullPtr)
009100150211     d TIIDC_ds        ds                  inz  qualified
009200150212     d   wIDCinsData                  8  0 inz
009300150211     d   IDCfgs                            inz  like(IDCfgs)
009400170412     d   wIDCstato                    1    inz
009500150211     d   IDCtipoDis                        inz  like(IDCtipoDis)
009600150212     d   wIDCsnDir                    1    inz
009700150212     d   wIDCsnApp                    1    inz
009800150211     d   IDCtipoCli                        inz  like(IDCtipoCli)
009900150212     d   wIDCcount                    9  0 inz
010000150210
010100150210       // -?Parametri ricevuti?
010200150210     d KPJBA         e ds
010300150210     d FNLRY04ds       ds                  inz
010400150210     d   iLRY04dti                    8  0 inz(*loval)
010500150210     d   iLRY04dtf                    8  0 inz(*hival)
010600150210
010700150210       // -?Tab. "5A"/"SEDE1" (per giorni limite cancellazione dati)?
010800150210     d ds5AS1        e ds                  inz
010900150210
011000150210       // -?Tab. "MRA" = Mailing - regole x invio da gestionale?
011100150210     d dMRAdan       e ds                  inz
011200150210     d   §MRADdes    e                     inz('ERR Stat.Dispos.+
011300150210     d                                         Cons.Web')
011400150210     d   §MRADreg    e                     inz('COR')
011500150210     d   §MRADmitt   e                     inz('ced')
011600150210     d   §MRADdest   e                     inz('ced@brt.it; +
011700150210     d                                          stefano.merighi+
011800150210     d                                          @brt.it')
011900150210     d   §MRADidPro  e                     inz('2')
012000150210     d   §MRADoutqI  e                     inz('EMAILIN')
012100150210
012200150210       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
012300150210     d TRTCM1ds      e ds                  inz
012400150210         //?·§CM1mitt = Indirizzo e-mail del mittente?
012500150210     d   §CM1mitt    e                     inz('ced')
012600150210         //?·§CM1dst  = Indirizzo e-mail del destinatario?
012700150210     d   §CM1dst     e                     inz('ced@brt.it;+
012800150210     d                                          stefano.merighi+
012900150210     d                                          @brt.it')
013000150210         //?·§CM1tips = Tipo lettera via e-mail:?
013100150210         // ?"LET" = testo allegato in corpo con logo?
013200150210         //         ?(richiede righe libere iniziali per il logo)?
013300150210         // ?"COR" = testo integrato senza logo?
013400150210         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
013500150210     d   §CM1tips    e                     inz('COR')
013600150210         //?·§CM1po   = Filiale?
013700150210     d   §CM1po      e                     inz('046')
013800150210         //?·§CM1var  = Oggetto e-mail?
013900150210     d   §CM1var     e                     inz('*OBJM*+
014000150210     d                                     ERRORE in estrazione dati -
014100150210     d                                     per Statistica Disposizioni -
014200150210     d                                     di Consegna da web')
014300150210         //?·§CM1sts  = Stato?
014400150210     d   §CM1sts     e                     inz(*off)
014500150210         //?·§CM1sts  = Id processo?
014600150210     d   §CM1idp     e                     inz('2')
014700150210
014800150210       //--------------------------------------------------------------
014900150210       //?Definizione variabili globali.                               ?
015000150210       //--------------------------------------------------------------
015100150210
015200150210       // -?Flags booleani?
015300150210     d $EoF            s               n   inz
015400150210
015500150210       // -?Nome Libreria del file di input e di quello di output?
015600150210     d wLib            s             10a   inz
015700150210       // -?Nome esteso Libreria/File del file di output?
015800150210     d wLibFile        s             21a   inz
015900150210
016000150210       // -?Stringa SQL da eseguire?
016100150210     d wSQL            s           7000    inz  varying
016200150210
016300150210       // -?Data *ISO?
016400150210     d wDate_Iso       s               d   inz  datfmt(*ISO)
016500150210
016600150210       // -?Date Iniziale e Finale per estrazione dati da TIIDC00F?
016700150210     d wDataIniz       s               d   inz  datfmt(*ISO)
016800150210
016900150210       // -?Data Inserimento Richiesta su AS/400 (da TIIDC00F)?
017000150210     d wInsData        s              8  0 inz
017100150210
017200150212       // -?Campi di controllo "rottura"?
017300150212     d SaveIDCsnDir    s                   inz  like(IDSsnDir)
017400150212     d SaveIDCsnApp    s                   inz  like(IDSsnApp)
017500150210       // -?Numero totale di Disposizioni di Consegna elaborate?
017600150210       //  ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
017700150210     d wTotDisp        s                   inz  like(IDStotIDC)
017800150210
017900150210       // -?Codice errore rilevato?
018000150223     d wErr            s              3  0 inz
018100150223
018200150223       // -?Campi di stampa?
018300150223     d s_Date          s              8  0 inz
018400150210
018500150210       //--------------------------------------------------------------
018600150210       //?Definizione prototipi procedure usate.                       ?
018700150210       //--------------------------------------------------------------
018800150210
018900150210       // -?Reperimento NETA sistema AS/400 corrente?
019000150210     d currSysNeta     s              8a   inz
019100150210      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019200150210
019300150210       // -?Reperimento dati utente?
019400150210     d TIBS34ds      e ds                  inz
019500150210      /copy gaitrasrc/srcProtoPR,TIBS34R
019600150210
019700150210       // -?Reperimento dati tabelle?
019800150210      /copy gaitrasrc/srcProtoPI,TRULTAB
019900150210      /copy gaitrasrc/srcProtoPR,TRULTAB
020000150210
020100150210       // -?Parametri API QCAPCMD (Process Commands)?
020200150210     d Qcmd            s           2048    inz  varying
020300150210      /copy qSysInc/qRpgleSrc,QCAPCMD
020400150210       // -?API QCAPCMD (Process Commands)?
020500150210      /copy gaitrasrc/srcProtoPR,QCAPCMD
020600150210
020700150210       // -?Parametri gestione errori API.?
020800150210      /copy qsysinc/qrpglesrc,QUSEC
020900150210
021000150210       //--------------------------------------------------------------
021100150210       //?Definizione key-list.                                        ?
021200150210       //--------------------------------------------------------------
021300150210
021400150210
021500150210       //--------------------------------------------------------------
021600150210       //?Riepilogo indicatori utilizzati.                             ?
021700150210       //--------------------------------------------------------------
021800150210
021900150210
022000150210       //--------------------------------------------------------------
022100150210       //?M A I N - L I N E                                            ?
022200150210       //--------------------------------------------------------------
022300150210
022400150210     c     *Entry        plist
022500150210     c                   parm                    KPJBA
022600150210
022700150210      /free
022800150210
022900150210       // -?Operazioni iniziali?
023000150210       exsr  sr_RoutInz;
023100150210
023200150210       // -?Preparazione SQL per estrazione dati?
023300150210       exsr  sr_PrepSQL;
023400150210
023500150210       // -?Ciclo di elaborazione?
023600150210       exsr  sr_OpenCursor;
023700150210
023800150210       DoW  Not $EoF;
023900150210         exsr  sr_ReadCursor;
024000150210       EndDo;
024100150210
024200150210       exsr  sr_CloseCursor;
024300150210
024400150210       // -?Operazioni finali?
024500150210       exsr  sr_RoutEnd;
024600150210
024700150210       //--------------------------------------------------------------
024800150210       //?Operazioni iniziali.                                         ?
024900150210       //--------------------------------------------------------------
025000150210       BEGSR  sr_RoutInz;
025100150210
025200150210         // -?Impostazione opzioni per SQL?
025300150210         exec sql   set  option  DynUsrPrf = *Owner,
025400150210                                 CloSqlCsr = *EndMod;
025500150210
025600150210         // -?Impostazione chiusura?
025700150210         *inLR = *on;
025800150210
025900150210         // -?Verifica del sistema AS/400 corrente?
026000150210         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026100150210           exsr  sr_RoutEnd;
026200150210         endif;
026300150210
026400150210         // -?Reperimento dati job?
026500150210         exsr  sr_DatiJob;
026600150223
026700150223         // -?Reperimento data odierna (in stampa)?
026800150223         s_Date = %dec( %date() );
026900150210
027000150210         // -?Reperimento regole per mailing da tab. "MRA"?
027100150210         clear  dMRAdan;
027200150210         if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
027300150210                          *omit : *omit :
027400150210                          *omit : *omit :
027500150210                          *omit : *omit : *omit : *omit :
027600150210                          *omit : *omit : *omit : *omit :
027700150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
027800150210                          = *zero      AND
027900150210               ds_TNTBE.TBEatb = *blank;
028000150210           dMRAdan = ds_TNTBE.TBEuni;
028100150210         endif;
028200150210
028300150210         // -?Impostazione libreria e nome esatto del file?
028400150210         if  currSysNetA = 'AS888   ';
028500150210           wLib = 'UNITRAGRP';
028600150210         else;
028700150210           wLib = 'UNITRAGRU';
028800150210         endif;
028900150210         wLibFile = %trimR( wLib ) + '/TIIDS00F';
029000150210
029100150210         // -?Cancellazione vecchi record?
029200150210         exsr  sr_DeleteTIIDS;
029300150210
029400150210         // -?Gestione parametri (forzati)?
029500150210         exsr  sr_NoParms;
029600150210
029700150210         // -?Apertura file?
029800150210         open  TIIDS00F;
029900150210
030000150210       ENDSR;
030100150210
030200150210       //--------------------------------------------------------------
030300150210       //?Reperimento Dati del job (Utente/Operativi).                 ?
030400150210       //--------------------------------------------------------------
030500150210       BEGSR  sr_DatiJob;
030600150210
030700150210         in(e) §AzUte;
030800150210         if NOT %error;
030900150210           in(e) §DatiUte;
031000150210         endif;
031100150210         if %error or RSut = *blank;
031200150210           tibs34r ( tibs34ds );
031300150210           in §AzUte;
031400150210           in §DatiUte;
031500150210         endif;
031600150210
031700150210       ENDSR;
031800150210
031900150210       //--------------------------------------------------------------
032000150210       //?Cancellazione vecchi record da TIIDS00F.                     ?
032100150210       //--------------------------------------------------------------
032200150210       BEGSR  sr_DeleteTIIDS;
032300150210
032400150210         // -?Reperimento tab. "5A/SEDE1"?
032500150210         clear  ds5AS1;
032600150210         if  getTabella ( c_Tabel : '5A'  : 'SEDE1' : *omit :
032700150210                          *omit : *omit :
032800150210                          *omit : *omit :
032900150210                          *omit : *omit : *omit : *omit :
033000150210                          *omit : *omit : *omit : *omit :
033100150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
033200150210                        = *zero      AND
033300150210             ds_TNTBE.TBEatb = *blank;
033400150210           ds5AS1 = ds_TNTBE.TBEuni;
033500150210         endif;
033600150210
033700150213         if  §5AS1ids = *zero;
033800150210           leavesr;
033900150210         endif;
034000150210
034100150210
034200150210         // -?Cancellazione vecchi record dal file?
034300150210         clear  wSQL;
034400150210         wSQL = 'DELETE from ' + %trim(wLibFile) +
034500150210                ' where IDCinsData < ' +
034600150213                %subst( %char( %dec( %timestamp() - %months( §5AS1ids ) ) )
034700150210                        : 1 : 8 );
034800150210
034900150210         exec SQL   execute immediate :wSQL;
035000150210
035100150210       ENDSR;
035200150210
035300150210       //--------------------------------------------------------------
035400150210       //?Calcolo range di date da ultima estrazione a ieri.           ?
035500150210       //--------------------------------------------------------------
035600150210       BEGSR  sr_NoParms;
035700150210
035800150210         // -?Reperimento data e ora attuali?
035900150210         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
036000150210
036100150210         // -?Preparazione SQL per reperimento ultima data di estrazione?
036200150210         clear  wSQL;
036300150210         wSQL = 'select coalesce( max( IDSinsData ), 0 ) +
036400150210                   from ' + %trim( wLibFile ) +
036500150210                  ' for fetch only';
036600150210
036700150210         // -?Dichiarazione cursore?
036800150210         exec sql   prepare S0   from :wSQL;
036900150210         exec sql   declare C0   cursor   for S0;
037000150210
037100150210         // -?Apertura del cursore?
037200150210         exec sql   open C0;
037300150210
037400150210         if  SQLcode < *zero;
037500150210           $EoF = *on;
037600150210           wErr = 1;
037700150210           exsr  sr_PrintErrSQL;
037800150210         endif;
037900150210
038000150210         // -?Lettura cursore?
038100150210         clear  IDSinsData;
038200150210
038300150210         exec sql   fetch   from C0   into :IDSinsData;
038400150210
038500150210         Select;
038600150210
038700150210           // -?Errore in elaborazione: si chiude il *pgm?
038800150210           When  SQLcode < *zero;
038900150210             $EoF = *on;
039000150210             wErr = 1;
039100150210             exsr  sr_PrintErrSQL;
039200150210
039300150210           // -?File vuoto?
039400150210           When  SQLcode = 100  or  IDSinsData = *zero;
039500150213             wDataIniz = %date( wDate : *Iso ) - %months( §5AS1ids );
039600150210
039700150210           // -?Si riparte dall'ultima data?
039800150210           Other;
039900150210             wDataIniz = %date( IDSinsData : *Iso );
040000150210
040100150210         EndSl;
040200150210
040300150210         // -?Chiusura del cursore?
040400150210         exec sql   close C0;
040500150210
040600150210         // -?Impostazione date - parametri - per l'estrazione?
040700150210         //  ?dal 1° giorno successivo all'ultimo estratto   a ieri?
040800150210         clear  FNLRY04ds;
040900150210         iLRY04dti = %dec( wDataIniz + %days(1) );
041000150210         //iLRY04dtf = %dec( %date() - %days(1) );
041100150210         wDate_Iso = %date( wDate : *Iso ) - %days(1);
041200150210         iLRY04dtf = %dec( wDate_Iso );
041300150210
041400150210         // -?Test fattibilità?
041500150210         if  iLRY04dti > iLRY04dtf;
041600150210           exsr  sr_RoutEnd;
041700150210         endif;
041800150210
041900150210       ENDSR;
042000150210
042100150210       //--------------------------------------------------------------
042200150210       //?Preparazione SQL.                                            ?
042300150210       //--------------------------------------------------------------
042400150210       BEGSR  sr_PrepSQL;
042500150210
042600150210         // -?Preparazione SQL?
042700150210         clear  wSQL;
042800150212         wSQL = 'with TIIDC as +
042900150212                      ( select int( IDCinsData / 1000000 ) as wIDCinsData, +
043000170412                               IDCfgs, +
043100170412                               substr( IDCalert, 10, 1 ) as wIDCstato, +
043200170412                               IDCtipoDis, +
043300150212                               case IDCnewLna when 0 then ''N'' +
043400150212                                              else ''S'' end as wIDCsnDir, +
043500150212                               case IDCdcr    when 0 then ''N'' +
043600150212                                              else ''S'' end as wIDCsnApp, +
043700150212                               IDCtipoCli +
043800150212                          from TIIDC00F +
043900150212                         where IDCip <> '' '' +
044000150212                           and IDCinsData between ' +
044100150212                                          %trim( %editc( iLRY04dti : '3' ) ) +
044200150212                                          '000000 and ' +
044300150212                                          %trim( %editc( iLRY04dtf : '3' ) ) +
044400150212                                          '999999 ) +
044500150212                 select TIIDC.*, count(*) +
044600150212                   from TIIDC +
044700170412                  group by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
044800150212                           wIDCsnDir, wIDCsnApp, IDCtipoCli +
044900170412                  order by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
045000150212                           wIDCsnDir, wIDCsnApp, IDCtipoCli +
045100150212                    for fetch only';
045200150210
045300150210         exec sql   prepare S1   from :wSQL;
045400150210
045500150210         // -?Dichiarazione cursore?
045600150210         exec sql   declare C1   cursor for S1;
045700150210
045800150210         if  SQLcode < *zero;
045900150210           $EoF = *on;
046000150210           wErr = 2;
046100150210           exsr  sr_PrintErrSQL;
046200150210         endif;
046300150210
046400150210       ENDSR;
046500150210
046600150210       //--------------------------------------------------------------
046700150210       //?Apertura cursore.                                            ?
046800150210       //--------------------------------------------------------------
046900150210       BEGSR  sr_OpenCursor;
047000150210
047100150210         // -?Apertura del cursore?
047200150210         exec sql   open C1;
047300150210
047400150210         if  SQLcode < *zero;
047500150210           $EoF = *on;
047600150210           wErr = 2;
047700150210           exsr  sr_PrintErrSQL;
047800150210         endif;
047900150210
048000150210       ENDSR;
048100150210
048200150210       //--------------------------------------------------------------
048300150210       //?Lettura cursore.                                             ?
048400150210       //--------------------------------------------------------------
048500150210       BEGSR  sr_ReadCursor;
048600150210
048700150210         clear  TIIDC_ds;
048800150210
048900150210         exec sql   fetch next   from C1   into :TIIDC_ds;
049000150210
049100150210         Select;
049200150210
049300150210           // -?Fine File?
049400150210           When  SQLcode = 100;
049500150210             $EoF = *on;
049600150210
049700150210           // -?Errore SQL?
049800150210           When  SQLcode < *zero;
049900150210             $EoF = *on;
050000150210             wErr = 2;
050100150210             exsr  sr_PrintErrSQL;
050200150210
050300150210           // -?Elaborazione singolo record?
050400150210           Other;
050500150210             exsr  sr_ElabRec;
050600150210
050700150210         EndSl;
050800150210
050900150210       ENDSR;
051000150210
051100150210       //--------------------------------------------------------------
051200150210       //?Elaborazione singolo record da TIIDC00F.                     ?
051300150210       //--------------------------------------------------------------
051400150210       BEGSR  sr_ElabRec;
051500150210
051600150210         // -?Scrittura record in TIIDS00F a Rottura...?
051700150212         If  TIIDC_ds.wIDCinsData <> IDSinsData      or
051800150212             TIIDC_ds.IDCfgs      <> IDSfgs          or
051900170412             TIIDC_ds.wIDCstato   <> IDSstato        or
052000150212             TIIDC_ds.IDCtipoDis  <> IDStipoDis      or
052100150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
052200150212             TIIDC_ds.wIDCsnDir   <> SaveIDCsnDir )  or
052300150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
052400150212             TIIDC_ds.wIDCsnDir   =  'N'            and
052500150212             TIIDC_ds.wIDCsnApp   <> SaveIDCsnApp )  or
052600150212             TIIDC_ds.IDCtipoCli  <> IDStipoCli;
052700150210
052800150210           exsr  sr_WriteTIIDS;
052900150210
053000150210         EndIf;
053100150210
053200150211         // -?Incremento Totale Disposizioni (del tipo estratto)?
053300150212         wTotDisp += TIIDC_ds.wIDCcount;
053400150210
053500150210       ENDSR;
053600150210
053700150210       //--------------------------------------------------------------
053800150210       //?Chiusura cursore.                                            ?
053900150210       //--------------------------------------------------------------
054000150210       BEGSR  sr_CloseCursor;
054100150210
054200150210         // -?Scrittura ultimo record di Totali in TIIDS00F?
054300150210         exsr  sr_WriteTIIDS;
054400150210
054500150210         // -?Chiusura del cursore?
054600150210         exec sql   close C1;
054700150210
054800150210       ENDSR;
054900150210
055000150210       //--------------------------------------------------------------
055100150210       //?Scrittura record nel file TIIDS00F.                          ?
055200150210       //--------------------------------------------------------------
055300150210       BEGSR  sr_WriteTIIDS;
055400150212
055500150212         SaveIDCsnDir = TIIDC_ds.wIDCsnDir;
055600150212         SaveIDCsnApp = TIIDC_ds.wIDCsnApp;
055700150210
055800150210         // -?Scrittura record in TIIDS00F?
055900150210         If  wTotDisp <> *zero;
056000150210
056100150210           IDStotIDC  = wTotDisp;
056200150210
056300150210           //______________
056400150210           write  TIIDS000;
056500150210           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
056600150210
056700150210         EndIf;
056800150210
056900150210         // -?Memorizzazione dei nuovi dati in TIIDS00F?
057000150210         clear  wTotDisp;
057100150210         clear  TIIDS000;
057200150210
057300150212         IDSinsData = TIIDC_ds.wIDCinsData;
057400150211         IDSfgs     = TIIDC_ds.IDCfgs;
057500170412         IDSstato   = TIIDC_ds.wIDCstato;
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
