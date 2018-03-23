000100180228       //==============================================================
000200180228       //? TNVRSM014R * Aggiornamento TIIDS00F.IDSTOTALRT               ?
000300180228       //==============================================================
000400180228
000500180228       //--------------------------------------------------------------
000600180228       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180228       //--------------------------------------------------------------
000800180228
000900180228     /*PRM  dbgview(*source)
001000180228     /*END
001100180228
001200180228       //--------------------------------------------------------------
001300180228       //? Specifiche di controllo.                                     ?
001400180228       //--------------------------------------------------------------
001500180228
001600180228     h decedit('0,') datedit(*ymd.) option(*nodebugio)
001700180228     h dftactgrp(*no)
001800180228     h alwnull(*inputonly)
001900180228
002000180228       //--------------------------------------------------------------
002100180228       //? Dichiarazione file.                                          ?
002200180228       //--------------------------------------------------------------
002300180228
002400180228       // - ?BOLLE SEDE: fatturate ("storico" + "ultimi mesi") +?
002500180228       //   ?            da fatturare?
002600180228     fTITAS30C  if   e           k disk
002700180228
002800180228       //--------------------------------------------------------------
002900180228       //? Definizione costanti.                                        ?
003000180228       //--------------------------------------------------------------
003100180228
003200180228
003300180228       //--------------------------------------------------------------
003400180228       //? Definizione schiere.                                         ?
003500180228       //--------------------------------------------------------------
003600180228
003700180228
003800180228       //--------------------------------------------------------------
003900180228       //? Definizione aree dati.                                       ?
004000180228       //--------------------------------------------------------------
004100180228
004200180228
004300180228       //--------------------------------------------------------------
004400180228       //? Definizione strutture dati.                                  ?
004500180228       //--------------------------------------------------------------
004600180228
004700180228
004800180228       // -?Statistica Disposizioni di Consegna da Internet?
004900180228     d TIIDS_ds      e ds                  extname(TIIDS00F)
005000180228     d                                     inz  qualified
005100180228
005200180228       // - ?Dati estratti via SQL?
005300180228     d TIIDC00F      e ds                  extname(TIIDC00F)
005400180228     d                                     based(nullPtr)
005500180228     d TIIDC_ds        ds                  inz  qualified
005600180228     d   wIDCinsData                  8  0 inz
005700180228     d   IDCfgs                            inz  like(IDCfgs)
005800180228     d   wIDCstato                    1    inz
005900180228     d   IDCtipoDis                        inz  like(IDCtipoDis)
006000180228     d   wIDCsnDir                    1    inz
006100180228     d   wIDCsnApp                    1    inz
006200180228     d   IDCtipoCli                        inz  like(IDCtipoCli)
006300180228     d   IDCaas                            inz  like(IDCaas)
006400180228     d   IDClnp                            inz  like(IDClnp)
006500180228     d   IDCnrs                            inz  like(IDCnrs)
006600180228     d   IDCnsp                            inz  like(IDCnsp)
006700180228     d   wIDCcount                    9  0 inz
006800180228
006900180228       // - ?DS lettura campo TASFLO di TITAS?
007000180228     d dTASflo       e ds                  inz  qualified
007100180228
007200180228       //--------------------------------------------------------------
007300180228       //? Definizione variabili globali.                               ?
007400180228       //--------------------------------------------------------------
007500180228
007600180228       // - ?Flag Booleani?
007700180228     d $EoF            s               n   inz
007800180302     d $Alert          s               n   inz
007900180228
008000180228       // - ?Stringa SQL da eseguire?
008100180228     d wSQL            s          32740    inz  varying
008200180228
008300180228       // - ?Campi di controllo "rottura"?
008400180228     d SaveIDCsnDir    s                   inz  like(TIIDS_ds.IDSsnDir)
008500180228     d SaveIDCsnApp    s                   inz  like(TIIDS_ds.IDSsnApp)
008600180301     d*//· wIDSsnDir       s                   inz  like(TIIDC_ds.wIDCsnDir)
008700180301     d*//· wIDSsnApp       s                   inz  like(TIIDC_ds.wIDCsnApp)
008800180302
008900180228       // - ?Numero totale di Disposizioni di Consegna elaborate?
009000180228       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
009100180302       //   ?con e senza Alert?
009200180302     d wTotDispSiAlert...
009300180302     d                 s                   inz  like(TIIDS_ds.IDStotIDC)
009400180302     d wTotDispNoAlert...
009500180302     d                 s                   inz  like(TIIDS_ds.IDStotIDC)
009600180302       // - ?Numero totale di Spedizioni (a cui fanno rifermento?
009700180302       //   ?le disposizioni di consegna di cui sopra)?
009800180302       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
009900180302       //   ?con e senza Alert?
010000180302     d wTotSpedSiAlert...
010100180302     d                                     inz  like(TIIDS_ds.IDStotALRT)
010200180302     d wTotSpedNoAlert...
010300180302     d                                     inz  like(TIIDS_ds.IDStotALRT)
010400180228
010500180228       //--------------------------------------------------------------
010600180228       //? Definizione prototipi procedure.                             ?
010700180228       //--------------------------------------------------------------
010800180228
010900180228
011000180228       //--------------------------------------------------------------
011100180228       //? Definizione key-list.                                        ?
011200180228       //--------------------------------------------------------------
011300180228
011400180228       // - ?File TITAS30C?
011500180228     d keyTITAS30    e ds                  extname( TITAS30C : *key )
011600180228     d                                     prefix( k_ )  inz
011700180228
011800180228       //--------------------------------------------------------------
011900180228       //? M A I N - L I N E                                            ?
012000180228       //--------------------------------------------------------------
012100180228
012200180228     c*//  *Entry        plist
012300180228     c*//                parm                    KPJBA
012400180228
012500180228      /free
012600180228
012700180228       // - ?Impostazione chiusura?
012800180228       *inLR = *on;
012900180228
013000180228       // - ?Preparazione SQL per estrazione dati?
013100180228       exsr  sr_PrepSQL;
013200180228
013300180228       // - ?Dichiarazione cursore?
013400180228       exsr  sr_DeclareCursor;
013500180228
013600180228       // - ?Apertura cursore?
013700180228       exsr  sr_OpenCursor;
013800180228
013900180228       // - ?Ciclo di elaborazione?
014000180228       DoW  Not $EoF;
014100180228         exsr  sr_ReadCursor;
014200180228       EndDo;
014300180228
014400180228       // - ?Chiusura cursore?
014500180228       exsr  sr_CloseCursor;
014600180228
014700180228       // - ?Uscita dal *pgm?
014800180228       return;
014900180228
015000180228       //--------------------------------------------------------------
015100180228       //? Preparazione SQL.                                            ?
015200180228       //--------------------------------------------------------------
015300180228       BEGSR  sr_PrepSQL;
015400180228
015500180228         // - ?Preparazione SQL?
015600180228         clear  wSQL;
015700180228         wSQL = 'with TIIDC as +
015800180228                      ( select int( IDCinsData / 1000000 ) as wIDCinsData, +
015900180228                               IDCfgs, +
016000180228                               substr( IDCalert, 10, 1 ) as wIDCstato, +
016100180228                               IDCtipoDis, +
016200180228                               case IDCnewLna when 0 then ''N'' +
016300180228                                              else ''S'' end as wIDCsnDir, +
016400180228                               case IDCdcr    when 0 then ''N'' +
016500180228                                              else ''S'' end as wIDCsnApp, +
016600180228                               IDCtipoCli, +
016700180228                               IDCaas, IDClnp, IDCnrs, IDCnsp +
016800180228                          from TIIDC00F +
016900180228                         where IDCip <> '' '' ) +
017000180228                 select TIIDC.*, count(*) +
017100180228                   from TIIDC +
017200180228                  group by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
017300180228                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
017400180228                           IDCaas, IDClnp, IDCnrs, IDCnsp +
017500180228                  order by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
017600180228                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
017700180228                           IDCaas, IDClnp, IDCnrs, IDCnsp +
017800180228                    for fetch only';
017900180228
018000180228         exec sql   prepare S1   from :wSQL;
018100180228
018200180228       ENDSR;
018300180228
018400180228       //--------------------------------------------------------------
018500180228       //? Dichiarazione cursore.                                       ?
018600180228       //--------------------------------------------------------------
018700180228       BEGSR  sr_DeclareCursor;
018800180228
018900180228         exec sql   declare C1   cursor for S1;
019000180228
019100180228         if  SQLcode < *zero;
019200180228           $EoF  = *on;
019300180228           *inH1 = *on;
019400180228         endif;
019500180228
019600180228       ENDSR;
019700180228
019800180228       //--------------------------------------------------------------
019900180228       //? Apertura cursore.                                            ?
020000180228       //--------------------------------------------------------------
020100180228       BEGSR  sr_OpenCursor;
020200180228
020300180228         exec sql   open C1;
020400180228
020500180228         if  SQLcode < *zero;
020600180228           $EoF  = *on;
020700180228           *inH1 = *on;
020800180228         endif;
020900180228
021000180228       ENDSR;
021100180228
021200180228       //--------------------------------------------------------------
021300180228       //? Lettura cursore.                                             ?
021400180228       //--------------------------------------------------------------
021500180228       BEGSR  sr_ReadCursor;
021600180228
021700180228         clear  TIIDC_ds;
021800180228
021900180228         exec sql   fetch next   from C1   into :TIIDC_ds;
022000180228
022100180228         Select;
022200180228
022300180228           // - ?Fine File?
022400180228           When  SQLcode = 100;
022500180228             $EoF = *on;
022600180228             // - ?Aggiornamento ultimo record di Totali in TIIDS00F?
022700180228             exsr  sr_UpdateTIIDS;
022800180228
022900180228           // - ?Errore SQL?
023000180228           When  SQLcode < *zero;
023100180228             $EoF  = *on;
023200180228             *inH1 = *on;
023300180228
023400180228           // - ?Elaborazione singolo record?
023500180228           Other;
023600180228             exsr  sr_ElabRec;
023700180228
023800180228         EndSl;
023900180228
024000180228       ENDSR;
024100180228
024200180228       //--------------------------------------------------------------
024300180228       //? Elaborazione singolo record da TIIDC00F.                     ?
024400180228       //--------------------------------------------------------------
024500180228       BEGSR  sr_ElabRec;
024600180228
024700180228         // - ?Aggiornamento record in TIIDS00F a Rottura...?
024800180228         If  TIIDC_ds.wIDCinsData <> TIIDS_ds.IDSinsData  or
024900180228             TIIDC_ds.IDCfgs      <> TIIDS_ds.IDSfgs      or
025000180228             TIIDC_ds.wIDCstato   <> TIIDS_ds.IDSstato    or
025100180228             TIIDC_ds.IDCtipoDis  <> TIIDS_ds.IDStipoDis  or
025200180228           ( TIIDC_ds.IDCtipoDis  =  '3'             and
025300180228             TIIDC_ds.wIDCsnDir   <> SaveIDCsnDir )       or
025400180228           ( TIIDC_ds.IDCtipoDis  =  '3'             and
025500180228             TIIDC_ds.wIDCsnDir   =  'N'             and
025600180228             TIIDC_ds.wIDCsnApp   <> SaveIDCsnApp )       or
025700180228             TIIDC_ds.IDCtipoCli  <> TIIDS_ds.IDStipoCli;
025800180228
025900180228           exsr  sr_UpdateTIIDS;
026000180228
026100180228         EndIf;
026200180228
026300180302         // - ?Verifica SE spedizione con Alert?
026400180302         clear  $Alert;
026500180228         clear  keyTITAS30;
026600180228         k_TASaas = TIIDC_ds.IDCaas;
026700180228         k_TASlnp = TIIDC_ds.IDClnp;
026800180228         k_TASnrs = TIIDC_ds.IDCnrs;
026900180228         k_TASnsp = TIIDC_ds.IDCnsp;
027000180228         setll  %kds( keyTITAS30 )  TITAS30C;
027100180228         readE  %kds( keyTITAS30 : 4 )  TITAS30C;
027200180228         DoW  Not %eof( TITAS30C );
027300180228           dTASflo = TASflo;
027400180228           If  dTASflo.§FLOemd <> *blank;
027500180302             $Alert = *on;
027600180228             leave;
027700180228           EndIf;
027800180228           readE  %kds( keyTITAS30 : 4 )  TITAS30C;
027900180228         EndDo;
028000180302
028100180302         // - ?Incremento Totale Disposizioni (del tipo estratto) e del?
028200180302         //   ?relativo Totale Spedizioni con e senza Alert?
028300180302         If  $Alert;
028400180302           wTotDispSiAlert += TIIDC_ds.wIDCcount;
028500180302           wTotSpedSiAlert += 1;
028600180302         Else;
028700180302           wTotDispNoAlert += TIIDC_ds.wIDCcount;
028800180302           wTotSpedNoAlert += 1;
028900180302         EndIf;
029000180228
029100180228       ENDSR;
029200180228
029300180228       //--------------------------------------------------------------
029400180228       //? Chiusura cursore.                                            ?
029500180228       //--------------------------------------------------------------
029600180228       BEGSR  sr_CloseCursor;
029700180228
029800180228         // - ?Chiusura del cursore?
029900180228         exec sql   close C1;
030000180228
030100180228       ENDSR;
030200180228
030300180228       //--------------------------------------------------------------
030400180228       //? Aggiornamento record nel file TIIDS00F.                      ?
030500180228       //--------------------------------------------------------------
030600180228       BEGSR  sr_UpdateTIIDS;
030700180228
030800180228         SaveIDCsnDir  = TIIDC_ds.wIDCsnDir;
030900180228         SaveIDCsnApp  = TIIDC_ds.wIDCsnApp;
031000180228
031100180302         // - ?Scrittura record in TIIDS?
031200180302         //   ?riferiti alle Spedizioni SENZA Alert?
031300180302         If  wTotDispNoAlert <> *zero;
031400180228
031500180302           IDStotIDC  = wTotDispNoAlert;
031600180302           IDStotALRT = wTotSpedNoAlert;
031700180302
031800180302           //______________
031900180302           write  TIIDS000;
032000180302           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
032100180228
032200180228         EndIf;
032300180228
032400180228         // - ?Pulizia del nuovo totale in TIIDS00F?
032500180228         clear  wTotDisp;
032600180228         clear  wTotAlert;
032700180228
032800180228         // -?Memorizzazione dei nuovi dati in TIIDS00F?
032900180228         clear  TIIDS_ds;
033000180228
033100180228         TIIDS_ds.IDSinsData = TIIDC_ds.wIDCinsData;
033200180228         TIIDS_ds.IDSfgs     = TIIDC_ds.IDCfgs;
033300180228         TIIDS_ds.IDSstato   = TIIDC_ds.wIDCstato;
033400180228         TIIDS_ds.IDStipoDis = TIIDC_ds.IDCtipoDis;
033500180228         if  TIIDC_ds.IDCtipoDis  = '3';
033600180228           TIIDS_ds.IDSsnDir = TIIDC_ds.wIDCsnDir;
033700180228           if  TIIDC_ds.wIDCsnDir = 'N';
033800180228             TIIDS_ds.IDSsnApp = TIIDC_ds.wIDCsnApp;
033900180228           endif;
034000180228         endif;
034100180228         TIIDS_ds.IDStipoCli = TIIDC_ds.IDCtipoCli;
034200180228
034300180228       ENDSR;
034400180228
034500180228      /end-free
