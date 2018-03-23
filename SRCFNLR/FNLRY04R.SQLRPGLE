000100150210       //==============================================================
000200180302       //? FNLRY04R * Scrittura file TIIDS00F (Stat.Disposiz.Cons. Web) ?
000300150210       //==============================================================
000400150210
000500150210       //--------------------------------------------------------------
000600180302       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150210       //--------------------------------------------------------------
000800150210
000900150210     /*PRM dbgview(*source)
001000150210     /*END
001100150210
001200150210       //--------------------------------------------------------------
001300180302       //? Specifiche di controllo.                                     ?
001400150210       //--------------------------------------------------------------
001500150210
001600150210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150210     h dftactgrp(*no)
001800150210     h bnddir('TRUL')
001900150210     h alwnull(*inputonly)
002000150210
002100150210       //--------------------------------------------------------------
002200180302       //? Dichiarazione file.                                          ?
002300150210       //--------------------------------------------------------------
002400180227
002500180302       // - ?BOLLE FIL./SEDE: Estensione testata bolla?
002600180227     fFIAR531C  if   e           k disk
002700150210
002800180302       // - ?Statistica Disposizioni di Consegna da Internet?
002900150210     fTIIDS00F  o    e             disk    extfile(wLibFile)
003000150210     f                                     usropn
003100150210
003200180302       // - ?File di stampa per eventuale invio e-mail?
003300150210     fPrtEMAIL  o    f  132        printer  usropn  oflind(*inOF)
003400150210
003500150210       //--------------------------------------------------------------
003600180302       //? Definizione costanti.                                        ?
003700150210       //--------------------------------------------------------------
003800150210
003900180302      *// // - ?Numero giorni da estrarre da TIIDC00F: 7?
004000150210     d*// c_NumeroGiorniDaEstrarre...
004100150210     d*//                 c                   const(7)
004200150210      *//
004300180302      *// // - ?Primo giorno da estrarre da TIIDC00F: la Domenica Precedente?
004400150210     d*// c_PrimoGiornoDaEstrarre...
004500150210     d*//                 c                   const(7)
004600150210
004700180302       // - ?Comandi da eseguire?
004800150210     d c_CmdOvrPrtF    c                   const('OVRPRTF +
004900150210     d                                           file(PRTEMAIL) +
005000150210     d                                           pagesize(66 132) +
005100150210     d                                           lpi(6) cpi(10) +
005200150210     d                                           ovrscope(*actgrpdfn) +
005300150210     d                                           usrdta(''*ERR_TIIDS'') +
005400150210     d                                           ')
005500150210     d c_CmdDltOvr     c                   const('DLTOVR +
005600150210     d                                            file(PRTEMAIL) +
005700150210     d                                            lvl(*actgrpdfn)')
005800150210
005900150210       //--------------------------------------------------------------
006000180302       //? Definizione schiere.                                         ?
006100150210       //--------------------------------------------------------------
006200150210
006300150210
006400150210       //--------------------------------------------------------------
006500180302       //? Definizione aree dati.                                       ?
006600150210       //--------------------------------------------------------------
006700150210
006800180302       // - ?Dati utente?
006900150210     d §AzUte        e ds                  extname(AZUTE00F)
007000150210     d                                     dtaara
007100150210     d §DatiUte      e ds                  extname(dDatiUte)
007200150210     d                                     dtaara
007300150210
007400150210       //--------------------------------------------------------------
007500180302       //? Definizione strutture dati.                                  ?
007600150210       //--------------------------------------------------------------
007700150210
007800180302       // - ?Status ds?
007900150210     d Status         sds
008000150210     d   SDSpgm          *proc
008100150210     d*//SDSprm          *parms
008200150210     d*//SDSdta              191    198
008300150210     d*//SDSjob              244    253
008400150210     d   SDSusr              254    263
008500150210
008600180302       // - ?Data/Ora attuali?
008700150210     d wTime_ds        ds                  inz
008800150210     d   wDate                        8s 0 inz
008900150210     d   wTime                        6s 0 inz
009000150211
009100180302       // - ?Dati estratti via SQL?
009200150211     d TIIDC00F      e ds                  extname(TIIDC00F)
009300150211     d                                     based(nullPtr)
009400150211     d TIIDC_ds        ds                  inz  qualified
009500150212     d   wIDCinsData                  8  0 inz
009600150211     d   IDCfgs                            inz  like(IDCfgs)
009700170412     d   wIDCstato                    1    inz
009800150211     d   IDCtipoDis                        inz  like(IDCtipoDis)
009900150212     d   wIDCsnDir                    1    inz
010000150212     d   wIDCsnApp                    1    inz
010100150211     d   IDCtipoCli                        inz  like(IDCtipoCli)
010200180227     d   IDCaas                            inz  like(IDCaas)
010300180227     d   IDClnp                            inz  like(IDClnp)
010400180227     d   IDCnrs                            inz  like(IDCnrs)
010500180227     d   IDCnsp                            inz  like(IDCnsp)
010600150212     d   wIDCcount                    9  0 inz
010700150210
010800180302       // - ?Parametri ricevuti?
010900150210     d KPJBA         e ds
011000150210     d FNLRY04ds       ds                  inz
011100150210     d   iLRY04dti                    8  0 inz(*loval)
011200150210     d   iLRY04dtf                    8  0 inz(*hival)
011300180227
011400180302       // - ?DS Estensione FIAR5 tipo record "EMD"?
011500180227     d dAR5emd       e ds                  inz  qualified
011600150210
011700180302       // - ?Tab. "5A"/"SEDE1" (per giorni limite cancellazione dati)?
011800150210     d ds5AS1        e ds                  inz
011900150210
012000180302       // - ?Tab. "MRA" = Mailing - regole x invio da gestionale?
012100150210     d dMRAdan       e ds                  inz
012200150210     d   §MRADdes    e                     inz('ERR Stat.Dispos.+
012300150210     d                                         Cons.Web')
012400150210     d   §MRADreg    e                     inz('COR')
012500150210     d   §MRADmitt   e                     inz('ced')
012600150210     d   §MRADdest   e                     inz('ced@brt.it; +
012700150210     d                                          stefano.merighi+
012800150210     d                                          @brt.it')
012900150210     d   §MRADidPro  e                     inz('2')
013000150210     d   §MRADoutqI  e                     inz('EMAILIN')
013100150210
013200180302       // - ?Parametri x Ridefinizione dati utente estesi x mailing *msg?
013300150210     d TRTCM1ds      e ds                  inz
013400180302         // ?·§CM1mitt = Indirizzo e-mail del mittente?
013500150210     d   §CM1mitt    e                     inz('ced')
013600180302         // ?·§CM1dst  = Indirizzo e-mail del destinatario?
013700150210     d   §CM1dst     e                     inz('ced@brt.it;+
013800150210     d                                          stefano.merighi+
013900150210     d                                          @brt.it')
014000180302         // ?·§CM1tips = Tipo lettera via e-mail:?
014100180302         //  ?"LET" = testo allegato in corpo con logo?
014200180302         //          ?(richiede righe libere iniziali per il logo)?
014300180302         //  ?"COR" = testo integrato senza logo?
014400180302         //          ?(non consente né UNDERLINE né HIGHLIGHT)?
014500150210     d   §CM1tips    e                     inz('COR')
014600180302         // ?·§CM1po   = Filiale?
014700150210     d   §CM1po      e                     inz('046')
014800180302         // ?·§CM1var  = Oggetto e-mail?
014900150210     d   §CM1var     e                     inz('*OBJM*+
015000150210     d                                     ERRORE in estrazione dati -
015100150210     d                                     per Statistica Disposizioni -
015200150210     d                                     di Consegna da web')
015300180302         // ?·§CM1sts  = Stato?
015400150210     d   §CM1sts     e                     inz(*off)
015500180302         // ?·§CM1sts  = Id processo?
015600150210     d   §CM1idp     e                     inz('2')
015700150210
015800150210       //--------------------------------------------------------------
015900180302       //? Definizione variabili globali.                               ?
016000150210       //--------------------------------------------------------------
016100150210
016200180302       // - ?Flags booleani?
016300150210     d $EoF            s               n   inz
016400180302     d $Alert          s               n   inz
016500150210
016600180302       // - ?Nome Libreria del file di input e di quello di output?
016700150210     d wLib            s             10a   inz
016800180302       // - ?Nome esteso Libreria/File del file di output?
016900150210     d wLibFile        s             21a   inz
017000150210
017100180302       // - ?Stringa SQL da eseguire?
017200180227     d wSQL            s          32740    inz  varying
017300150210
017400180302       // - ?Data *ISO?
017500150210     d wDate_Iso       s               d   inz  datfmt(*ISO)
017600150210
017700180302       // - ?Date Iniziale e Finale per estrazione dati da TIIDC00F?
017800150210     d wDataIniz       s               d   inz  datfmt(*ISO)
017900150210
018000180302      *//· // - ?Data Inserimento Richiesta su AS/400 (da TIIDC00F)?
018100180227     d*//· wInsData        s              8  0 inz
018200150210
018300180302       // - ?Campi di controllo "rottura"?
018400150212     d SaveIDCsnDir    s                   inz  like(IDSsnDir)
018500150212     d SaveIDCsnApp    s                   inz  like(IDSsnApp)
018600180302
018700180302       // - ?Numero totale di Disposizioni di Consegna elaborate?
018800180302       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
018900180302       //   ?con e senza Alert?
019000180302     d wTotDispSiAlert...
019100180302     d                 s                   inz  like(IDStotIDC)
019200180302     d wTotDispNoAlert...
019300180302     d                 s                   inz  like(IDStotIDC)
019400180302       // - ?Numero totale di Spedizioni (a cui fanno riferimento?
019500180302       //   ?le disposizioni di consegna di cui sopra)?
019600180302       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
019700180302       //   ?con e senza Alert?
019800180302     d wTotSpedSiAlert...
019900180302     d                 s                   inz  like(IDStotALRT)
020000180302     d wTotSpedNoAlert...
020100180302     d                 s                   inz  like(IDStotALRT)
020200150210
020300180302       // - ?Codice errore rilevato?
020400150223     d wErr            s              3  0 inz
020500150223
020600180302       // - ?Campi di stampa?
020700150223     d s_Date          s              8  0 inz
020800150210
020900150210       //--------------------------------------------------------------
021000180302       //? Definizione prototipi procedure usate.                       ?
021100150210       //--------------------------------------------------------------
021200150210
021300180302       // - ?Reperimento NETA sistema AS/400 corrente?
021400150210     d currSysNeta     s              8a   inz
021500150210      /copy gaitrasrc/srcProtoPR,UBRTVNETA
021600150210
021700180302       // - ?Reperimento dati utente?
021800150210     d TIBS34ds      e ds                  inz
021900150210      /copy gaitrasrc/srcProtoPR,TIBS34R
022000150210
022100180302       // - ?Reperimento dati tabelle?
022200150210      /copy gaitrasrc/srcProtoPI,TRULTAB
022300150210      /copy gaitrasrc/srcProtoPR,TRULTAB
022400150210
022500180302       // - ?Parametri API QCAPCMD (Process Commands)?
022600150210     d Qcmd            s           2048    inz  varying
022700150210      /copy qSysInc/qRpgleSrc,QCAPCMD
022800180302       // - ?API QCAPCMD (Process Commands)?
022900150210      /copy gaitrasrc/srcProtoPR,QCAPCMD
023000150210
023100180302       // - ?Parametri gestione errori API.?
023200150210      /copy qsysinc/qrpglesrc,QUSEC
023300150210
023400150210       //--------------------------------------------------------------
023500180302       //? Definizione key-list.                                        ?
023600150210       //--------------------------------------------------------------
023700150210
023800180302       // - ?File FIAR531C?
023900180227     d keyFIAR531    e ds                  extname( FIAR531C : *key )
024000180227     d                                     prefix( k_ )  inz
024100150210
024200150210       //--------------------------------------------------------------
024300180302       //? Riepilogo indicatori utilizzati.                             ?
024400150210       //--------------------------------------------------------------
024500150210
024600150210
024700150210       //--------------------------------------------------------------
024800180302       //? M A I N - L I N E                                            ?
024900150210       //--------------------------------------------------------------
025000150210
025100150210     c     *Entry        plist
025200150210     c                   parm                    KPJBA
025300150210
025400150210      /free
025500150210
025600180302       // - ?Operazioni iniziali?
025700150210       exsr  sr_RoutInz;
025800150210
025900180302       // - ?Preparazione SQL per estrazione dati?
026000150210       exsr  sr_PrepSQL;
026100150210
026200180302       // - ?Ciclo di elaborazione?
026300150210       exsr  sr_OpenCursor;
026400150210
026500150210       DoW  Not $EoF;
026600150210         exsr  sr_ReadCursor;
026700150210       EndDo;
026800150210
026900150210       exsr  sr_CloseCursor;
027000150210
027100180302       // - ?Operazioni finali?
027200150210       exsr  sr_RoutEnd;
027300150210
027400150210       //--------------------------------------------------------------
027500180302       //? Operazioni iniziali.                                         ?
027600150210       //--------------------------------------------------------------
027700150210       BEGSR  sr_RoutInz;
027800150210
027900180302         // - ?Impostazione opzioni per SQL?
028000150210         exec sql   set  option  DynUsrPrf = *Owner,
028100150210                                 CloSqlCsr = *EndMod;
028200150210
028300180302         // - ?Impostazione chiusura?
028400150210         *inLR = *on;
028500150210
028600180302         // - ?Verifica del sistema AS/400 corrente?
028700150210         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
028800150210           exsr  sr_RoutEnd;
028900150210         endif;
029000150210
029100180302         // - ?Reperimento dati job?
029200150210         exsr  sr_DatiJob;
029300150223
029400180302         // - ?Reperimento data odierna (in stampa)?
029500150223         s_Date = %dec( %date() );
029600150210
029700180302         // - ?Reperimento regole per mailing da tab. "MRA"?
029800150210         clear  dMRAdan;
029900150210         if  getTabella ( c_Tntbe : 'MRA' : SDSpgm : *blank :
030000150210                          *omit : *omit :
030100150210                          *omit : *omit :
030200150210                          *omit : *omit : *omit : *omit :
030300150210                          *omit : *omit : *omit : *omit :
030400150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
030500150210                          = *zero      AND
030600150210               ds_TNTBE.TBEatb = *blank;
030700150210           dMRAdan = ds_TNTBE.TBEuni;
030800150210         endif;
030900150210
031000180302         // - ?Impostazione libreria e nome esatto del file?
031100150210         if  currSysNetA = 'AS888   ';
031200150210           wLib = 'UNITRAGRP';
031300150210         else;
031400150210           wLib = 'UNITRAGRU';
031500150210         endif;
031600150210         wLibFile = %trimR( wLib ) + '/TIIDS00F';
031700150210
031800180302         // - ?Cancellazione vecchi record?
031900150210         exsr  sr_DeleteTIIDS;
032000150210
032100180302         // - ?Gestione parametri (forzati)?
032200150210         exsr  sr_NoParms;
032300150210
032400180302         // - ?Apertura file?
032500150210         open  TIIDS00F;
032600150210
032700150210       ENDSR;
032800150210
032900150210       //--------------------------------------------------------------
033000180302       //? Reperimento Dati del job (Utente/Operativi).                 ?
033100150210       //--------------------------------------------------------------
033200150210       BEGSR  sr_DatiJob;
033300150210
033400150210         in(e) §AzUte;
033500150210         if NOT %error;
033600150210           in(e) §DatiUte;
033700150210         endif;
033800150210         if %error or RSut = *blank;
033900150210           tibs34r ( tibs34ds );
034000150210           in §AzUte;
034100150210           in §DatiUte;
034200150210         endif;
034300150210
034400150210       ENDSR;
034500150210
034600150210       //--------------------------------------------------------------
034700180302       //? Cancellazione vecchi record da TIIDS00F.                     ?
034800150210       //--------------------------------------------------------------
034900150210       BEGSR  sr_DeleteTIIDS;
035000150210
035100180302         // - ?Reperimento tab. "5A/SEDE1"?
035200150210         clear  ds5AS1;
035300150210         if  getTabella ( c_Tabel : '5A'  : 'SEDE1' : *omit :
035400150210                          *omit : *omit :
035500150210                          *omit : *omit :
035600150210                          *omit : *omit : *omit : *omit :
035700150210                          *omit : *omit : *omit : *omit :
035800150210                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
035900150210                        = *zero      AND
036000150210             ds_TNTBE.TBEatb = *blank;
036100150210           ds5AS1 = ds_TNTBE.TBEuni;
036200150210         endif;
036300150210
036400150213         if  §5AS1ids = *zero;
036500150210           leavesr;
036600150210         endif;
036700150210
036800150210
036900180302         // - ?Cancellazione vecchi record dal file?
037000150210         clear  wSQL;
037100150210         wSQL = 'DELETE from ' + %trim(wLibFile) +
037200180227                ' where IDSinsData < ' +
037300150213                %subst( %char( %dec( %timestamp() - %months( §5AS1ids ) ) )
037400150210                        : 1 : 8 );
037500150210
037600150210         exec SQL   execute immediate :wSQL;
037700150210
037800150210       ENDSR;
037900150210
038000150210       //--------------------------------------------------------------
038100180302       //? Calcolo range di date da ultima estrazione a ieri.           ?
038200150210       //--------------------------------------------------------------
038300150210       BEGSR  sr_NoParms;
038400150210
038500180302         // - ?Reperimento data e ora attuali?
038600150210         wTime_ds  = %editc( %dec( %timestamp() ) : 'X' );
038700150210
038800180302         // - ?Preparazione SQL per reperimento ultima data di estrazione?
038900150210         clear  wSQL;
039000150210         wSQL = 'select coalesce( max( IDSinsData ), 0 ) +
039100150210                   from ' + %trim( wLibFile ) +
039200150210                  ' for fetch only';
039300150210
039400180302         // - ?Dichiarazione cursore?
039500150210         exec sql   prepare S0   from :wSQL;
039600150210         exec sql   declare C0   cursor   for S0;
039700150210
039800180302         // - ?Apertura del cursore?
039900150210         exec sql   open C0;
040000150210
040100150210         if  SQLcode < *zero;
040200150210           $EoF = *on;
040300150210           wErr = 1;
040400150210           exsr  sr_PrintErrSQL;
040500150210         endif;
040600150210
040700180302         // - ?Lettura cursore?
040800150210         clear  IDSinsData;
040900150210
041000150210         exec sql   fetch   from C0   into :IDSinsData;
041100150210
041200150210         Select;
041300150210
041400180302           // - ?Errore in elaborazione: si chiude il *pgm?
041500150210           When  SQLcode < *zero;
041600150210             $EoF = *on;
041700150210             wErr = 1;
041800150210             exsr  sr_PrintErrSQL;
041900150210
042000180302           // - ?File vuoto?
042100150210           When  SQLcode = 100  or  IDSinsData = *zero;
042200150213             wDataIniz = %date( wDate : *Iso ) - %months( §5AS1ids );
042300150210
042400180302           // - ?Si riparte dall'ultima data?
042500150210           Other;
042600150210             wDataIniz = %date( IDSinsData : *Iso );
042700150210
042800150210         EndSl;
042900150210
043000180302         // - ?Chiusura del cursore?
043100150210         exec sql   close C0;
043200150210
043300180302         // - ?Impostazione date - parametri - per l'estrazione?
043400180302         //   ?dal 1° giorno successivo all'ultimo estratto   a ieri?
043500150210         clear  FNLRY04ds;
043600150210         iLRY04dti = %dec( wDataIniz + %days(1) );
043700150210         //iLRY04dtf = %dec( %date() - %days(1) );
043800150210         wDate_Iso = %date( wDate : *Iso ) - %days(1);
043900150210         iLRY04dtf = %dec( wDate_Iso );
044000150210
044100180302         // - ?Test fattibilità?
044200150210         if  iLRY04dti > iLRY04dtf;
044300150210           exsr  sr_RoutEnd;
044400150210         endif;
044500150210
044600150210       ENDSR;
044700150210
044800150210       //--------------------------------------------------------------
044900180302       //? Preparazione SQL.                                            ?
045000150210       //--------------------------------------------------------------
045100150210       BEGSR  sr_PrepSQL;
045200150210
045300180302         // - ?Preparazione SQL?
045400150210         clear  wSQL;
045500150212         wSQL = 'with TIIDC as +
045600150212                      ( select int( IDCinsData / 1000000 ) as wIDCinsData, +
045700170412                               IDCfgs, +
045800170412                               substr( IDCalert, 10, 1 ) as wIDCstato, +
045900170412                               IDCtipoDis, +
046000150212                               case IDCnewLna when 0 then ''N'' +
046100150212                                              else ''S'' end as wIDCsnDir, +
046200150212                               case IDCdcr    when 0 then ''N'' +
046300150212                                              else ''S'' end as wIDCsnApp, +
046400180227                               IDCtipoCli, +
046500180227                               IDCaas, IDClnp, IDCnrs, IDCnsp +
046600150212                          from TIIDC00F +
046700150212                         where IDCip <> '' '' +
046800150212                           and IDCinsData between ' +
046900150212                                          %trim( %editc( iLRY04dti : '3' ) ) +
047000150212                                          '000000 and ' +
047100150212                                          %trim( %editc( iLRY04dtf : '3' ) ) +
047200150212                                          '999999 ) +
047300150212                 select TIIDC.*, count(*) +
047400150212                   from TIIDC +
047500170412                  group by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
047600180227                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
047700180227                           IDCaas, IDClnp, IDCnrs, IDCnsp +
047800170412                  order by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
047900180227                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
048000180227                           IDCaas, IDClnp, IDCnrs, IDCnsp +
048100150212                    for fetch only';
048200150210
048300150210         exec sql   prepare S1   from :wSQL;
048400150210
048500180302         // - ?Dichiarazione cursore?
048600150210         exec sql   declare C1   cursor for S1;
048700150210
048800150210         if  SQLcode < *zero;
048900150210           $EoF = *on;
049000150210           wErr = 2;
049100150210           exsr  sr_PrintErrSQL;
049200150210         endif;
049300150210
049400150210       ENDSR;
049500150210
049600150210       //--------------------------------------------------------------
049700180302       //? Apertura cursore.                                            ?
049800150210       //--------------------------------------------------------------
049900150210       BEGSR  sr_OpenCursor;
050000150210
050100150210         exec sql   open C1;
050200150210
050300150210         if  SQLcode < *zero;
050400150210           $EoF = *on;
050500150210           wErr = 2;
050600150210           exsr  sr_PrintErrSQL;
050700150210         endif;
050800150210
050900150210       ENDSR;
051000150210
051100150210       //--------------------------------------------------------------
051200180302       //? Lettura cursore.                                             ?
051300150210       //--------------------------------------------------------------
051400150210       BEGSR  sr_ReadCursor;
051500150210
051600150210         clear  TIIDC_ds;
051700150210
051800150210         exec sql   fetch next   from C1   into :TIIDC_ds;
051900150210
052000150210         Select;
052100150210
052200180302           // - ?Fine File?
052300150210           When  SQLcode = 100;
052400150210             $EoF = *on;
052500180302             // - ?Scrittura ultimo record di Totali in TIIDS00F?
052600180301             exsr  sr_WriteTIIDS;
052700150210
052800180302           // - ?Errore SQL?
052900150210           When  SQLcode < *zero;
053000150210             $EoF = *on;
053100150210             wErr = 2;
053200150210             exsr  sr_PrintErrSQL;
053300150210
053400180302           // - ?Elaborazione singolo record?
053500150210           Other;
053600150210             exsr  sr_ElabRec;
053700150210
053800150210         EndSl;
053900150210
054000150210       ENDSR;
054100150210
054200150210       //--------------------------------------------------------------
054300180302       //? Elaborazione singolo record da TIIDC00F.                     ?
054400150210       //--------------------------------------------------------------
054500150210       BEGSR  sr_ElabRec;
054600150210
054700180302         // - ?Scrittura doppio record in TIIDS00F a Rottura...?
054800150212         If  TIIDC_ds.wIDCinsData <> IDSinsData      or
054900150212             TIIDC_ds.IDCfgs      <> IDSfgs          or
055000170412             TIIDC_ds.wIDCstato   <> IDSstato        or
055100150212             TIIDC_ds.IDCtipoDis  <> IDStipoDis      or
055200150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
055300150212             TIIDC_ds.wIDCsnDir   <> SaveIDCsnDir )  or
055400150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
055500150212             TIIDC_ds.wIDCsnDir   =  'N'            and
055600150212             TIIDC_ds.wIDCsnApp   <> SaveIDCsnApp )  or
055700150212             TIIDC_ds.IDCtipoCli  <> IDStipoCli;
055800150210
055900150210           exsr  sr_WriteTIIDS;
056000150210
056100150210         EndIf;
056200180227
056300180302         // - ?Verifica SE spedizione con ALERT?
056400180302         clear  $Alert;
056500180227         clear  keyFIAR531;
056600180227         k_AR5aas = TIIDC_ds.IDCaas;
056700180227         k_AR5lnp = TIIDC_ds.IDClnp;
056800180227         k_AR5nrs = TIIDC_ds.IDCnrs;
056900180227         k_AR5nsp = TIIDC_ds.IDCnsp;
057000180227         k_AR5trd = 'EMD';
057100180227         setll  %kds( keyFIAR531 )  FIAR531C;
057200180227         readE  %kds( keyFIAR531 : 5 )  FIAR531C;
057300180227         DoW  Not %eof( FIAR531C );
057400180227           dAR5emd = AR5uni;
057500180301           If  dAR5emd.§AR5smp <> *blank  or
057600180301               dAR5emd.§AR5mmp <> *blank;
057700180302             $Alert = *on;
057800180301             leave;
057900180227           EndIf;
058000180227           readE  %kds( keyFIAR531 : 5 )  FIAR531C;
058100180227         EndDo;
058200180302
058300180302         // - ?Incremento Totale Disposizioni (del tipo estratto) e?
058400180302         //   ?del relativo Totale Spedizioni - Con e Senza Alert?
058500180302         If  $Alert;
058600180302           wTotDispSiAlert += TIIDC_ds.wIDCcount;
058700180302           wTotSpedSiAlert += 1;
058800180302         Else;
058900180302           wTotDispNoAlert += TIIDC_ds.wIDCcount;
059000180302           wTotSpedNoAlert += 1;
059100180302         EndIf;
059200150210
059300150210       ENDSR;
059400150210
059500150210       //--------------------------------------------------------------
059600180302       //? Chiusura cursore.                                            ?
059700150210       //--------------------------------------------------------------
059800150210       BEGSR  sr_CloseCursor;
059900150210
060000150210         exec sql   close C1;
060100150210
060200150210       ENDSR;
060300150210
060400150210       //--------------------------------------------------------------
060500180302       //? Scrittura record nel file TIIDS00F.                          ?
060600150210       //--------------------------------------------------------------
060700150210       BEGSR  sr_WriteTIIDS;
060800150212
060900150212         SaveIDCsnDir = TIIDC_ds.wIDCsnDir;
061000150212         SaveIDCsnApp = TIIDC_ds.wIDCsnApp;
061100150210
061200180302         // - ?Scrittura record in TIIDS00F?
061300180302         //   ?riferito alle spedizioni SENZA Alert?
061400180302         If  wTotDispNoAlert <> *zero;
061500150210
061600180302           IDStotIDC  = wTotDispNoAlert;
061700180302           IDStotALRT = 0;
061800150210
061900150210           //______________
062000150210           write  TIIDS000;
062100150210           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062200150210
062300150210         EndIf;
062400180302
062500180302         // - ?Scrittura record in TIIDS00F?
062600180302         //   ?riferito alle spedizioni CON Alert?
062700180302         If  wTotDispSiAlert <> *zero;
062800180302
062900180302           IDStotIDC  = wTotDispSiAlert;
063000180302           IDStotALRT = wTotSpedSiAlert;
063100180302
063200180302           //______________
063300180302           write  TIIDS000;
063400180302           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063500180302
063600180302         EndIf;
063700150210
063800180302         // - ?Pulizia dei campi di lavoro usati per calcolare i totali?
063900180302         clear  wTotDispNoAlert;
064000180302         clear  wTotDispSiAlert;
064100180302         clear  wTotSpedNoAlert;
064200180302         clear  wTotSpedSiAlert;
064300150210         clear  TIIDS000;
064400180302
064500180302         // - ?Memorizzazione dei nuovi dati in TIIDS00F?
064600150212         IDSinsData = TIIDC_ds.wIDCinsData;
064700150211         IDSfgs     = TIIDC_ds.IDCfgs;
064800170412         IDSstato   = TIIDC_ds.wIDCstato;
064900150211         IDStipoDis = TIIDC_ds.IDCtipoDis;
065000150211         if  TIIDC_ds.IDCtipoDis  = '3';
065100150212           IDSsnDir  = TIIDC_ds.wIDCsnDir;
065200150212           if  TIIDC_ds.wIDCsnDir = 'N';
065300150212             IDSsnApp  = TIIDC_ds.wIDCsnApp;
065400150210           endif;
065500150210         endif;
065600150211         IDStipoCli = TIIDC_ds.IDCtipoCli;
065700150210
065800150210       ENDSR;
065900150210
066000150210       //--------------------------------------------------------------
066100180302       //? Stampa segnalazione dell'errore rilevato via SQL             ?
066200150210       //--------------------------------------------------------------
066300150210       BEGSR  sr_PrintErrSQL;
066400150210
066500180302         // - ?Stampa del Dump?
066600150210         Dump(A);
066700150210
066800180302         // - ?Stampa del Job-Log?
066900150210         Qcmd = 'DSPJOBLOG job(*) output(*print)';
067000150210         exsr  sr_ExecCmd;
067100150210
067200180302         // - ?Invio e-mail con errore rilevato?
067300150210         exsr  sr_SendEmail;
067400150210
067500180302         // - ?Chiusura del programma?
067600150210         exsr  sr_RoutEnd;
067700150210
067800150210       ENDSR;
067900150210
068000150210       //--------------------------------------------------------------
068100180302       //? Invio e-mail di avviso per l'errore rilevato via SQL.        ?
068200150210       //--------------------------------------------------------------
068300150210       BEGSR  sr_SendEmail;
068400150210
068500180302         // - ?Override al file di stampa ed apertura dello stesso?
068600150210         if  NOT  %open(PrtEMAIL);
068700150210           §CM1mitt = %trim(§MRADmitt);
068800150210           §CM1dst  = %trim(§MRADdest);
068900150210           §CM1tips = §MRAdreg;
069000150210           §CM1var  = '*OBJM*' + §MRADdes;
069100150210           §CM1idp  = §MRADidPro;
069200150210           Qcmd = c_CmdOvrPrtF
069300150210                + ' outq(' + %trim(§MRADoutqI) + ')'
069400150210                + ' usrdfndta(''' + TRTCM1ds + ''')';
069500150210           exsr  sr_ExecCmd;
069600150210           open  PrtEMAIL;
069700150210           except  PRTtxt;
069800150210         endif;
069900150210
070000180302         // - ?Preparazione e-Mail per il singolo errore rilevato?
070100150210         Select;
070200150210
070300180302           // - ?Errore rilevato nel reperimento dell'ultima data?
070400180302           //   ?già estratta nel file TIIDS00F?
070500150210           When  wErr = 1;
070600150210             if  *inOF;
070700150210               except  PRTtxt;
070800150210               *inOF = *off;
070900150210             endif;
071000150210             except  PRTerr01;
071100150210
071200180302           // - ?Errore rilevato nell'estrazione dati via SQL dal?
071300180302           //   ?file TIIDC00F?
071400150210           When  wErr = 2;
071500150210             if  *inOF;
071600150210               except  PRTtxt;
071700150210               *inOF = *off;
071800150210             endif;
071900150210             except  PRTerr02;
072000150210
072100150210         EndSl;
072200150210
072300180302         // - ?Fine lista?
072400150210         except  PRTend;
072500150210
072600180302         // - ?Chiusura file di stampa?
072700150210         close  PrtEMAIL;
072800180227         Qcmd = c_CmdDltOvr;
072900180227         exsr  sr_ExecCmd;
073000150210
073100150210       ENDSR;
073200150210
073300150210       //--------------------------------------------------------------
073400180302       //? Esecuzione del comando (già impostato).                      ?
073500150210       //--------------------------------------------------------------
073600150210       BEGSR  sr_ExecCmd;
073700150210
073800150210         clear Qcap0100;
073900150210         Qcabcsdh = *off;
074000150210         Qcapa    = *off;
074100150210         Qcacmdss = *off;
074200150210         Qcaerved = *allX'00';
074300150210
074400150210         clear Qusec;
074500150210         Qusbprv  = %size(Qusec);
074600150210
074700150210         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
074800150210                           %size(Qcap0100) : 'CPOP0100' : *omit :
074900150210                           0 : 0 : Qusec);
075000150210
075100150210         //if  Qusei <> *blank;
075200150210         //  ...;
075300150210         //endif;
075400150210
075500150210       ENDSR;
075600150210
075700150210       //--------------------------------------------------------------
075800180302       //? Operazioni finali.                                           ?
075900150210       //--------------------------------------------------------------
076000150210       BEGSR  sr_RoutEnd;
076100150210
076200180302         // - ?Chiusura funzioni precedentemente aperte?
076300150210         cloTabella();
076400150210
076500180302         // - ?Chiusura file in output?
076600150210         close  TIIDS00F;
076700150210
076800180302         // - ?SE stampato almeno un Errore:?
076900180302         //   ?stampa "fine" e chiude il file di stampa?
077000150210         if  %open(PrtEMAIL);
077100150210           except  PRTend;
077200150210           close   PrtEMAIL;
077300150210         endif;
077400150210
077500180302         // - ?Uscita dal *pgm?
077600150210         return;
077700150210
077800150210       ENDSR;
077900150210
078000150210      /end-free
078100150210
078200150210       //--------------------------------------------------------------
078300180302       //? S P O O L - F I L E S                                        ?
078400150210       //--------------------------------------------------------------
078500150210
078600150210     oPrtEMAIL  e            PRTtxt            1
078700150210     o                       RSUT
078800150210     o                                        +   5 'STATISTICA DISPOSIZIONI DI-
078900150210     o                                               CONSEGNA DA WEB: ERRORI RI-
079000150210     o                                              LEVATI'
079100150210     o                       SDSpgm           +   5
079200150223     o                       s_Date        y  +   5
079300150210     o          e            PRTtxt      1
079400150210     o                       KNSIF
079500150210     o                       KNMUS            +   1
079600150210     o                                        +   4 '---------------------------
079700150210     o                                              ----------------------------
079800150210     o                                              ------'
079900150210     o                                        +   5 'Pag.'
080000150210     o                       Page          z  +   0
080100150210     o                       wTime            +   5 '  :  :  '
080200150210      *
080300150210     o          e            PRTerr01    2
080400150210     o                                              'Rilevato ERRORE n-
080500150210     o                                              el reperimento del-
080600150210     o                                              l''ultima data già-
080700150210     o                                               estratta.'
080800150210     o          e            PRTerr02    2
080900150210     o                                              'Rilevato ERRORE n-
081000150210     o                                              ella lettura delle-
081100150210     o                                               Disposizioni di C-
081200150210     o                                              onsegna via Web (f-
081300150210     o                                              ile TIIDC00F).'
081400150210      *
081500150210     o          e            PRTend      2
081600150210     o                                        +  10 '***  Fine Lista  ***'
