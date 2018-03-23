000100180302       //==============================================================
000200180302       //? TNVRSM014R * Scrittura file TIIDS00F (Stat.Disposiz.Cons. Web)?
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
002500180302       // - ?BOLLE SEDE: Fatturate ("storico" + "ultimi mesi") +?
002600180302       //   ?            da Fatturare?
002700180302     fTITAS30C  if   e           k disk
002800150210
002900180302       // - ?Statistica Disposizioni di Consegna da Internet?
003000180302     fTIIDS00F  o    e             disk
003100150210
003200150210       //--------------------------------------------------------------
003300180302       //? Definizione costanti.                                        ?
003400150210       //--------------------------------------------------------------
003500150210
003600150210
003700150210       //--------------------------------------------------------------
003800180302       //? Definizione schiere.                                         ?
003900150210       //--------------------------------------------------------------
004000150210
004100150210
004200150210       //--------------------------------------------------------------
004300180302       //? Definizione aree dati.                                       ?
004400150210       //--------------------------------------------------------------
004500150210
004600150210
004700150210       //--------------------------------------------------------------
004800180302       //? Definizione strutture dati.                                  ?
004900150210       //--------------------------------------------------------------
005000150210
005100180302       // - ?Dati estratti via SQL?
005200150211     d TIIDC00F      e ds                  extname(TIIDC00F)
005300150211     d                                     based(nullPtr)
005400150211     d TIIDC_ds        ds                  inz  qualified
005500150212     d   wIDCinsData                  8  0 inz
005600150211     d   IDCfgs                            inz  like(IDCfgs)
005700170412     d   wIDCstato                    1    inz
005800150211     d   IDCtipoDis                        inz  like(IDCtipoDis)
005900150212     d   wIDCsnDir                    1    inz
006000150212     d   wIDCsnApp                    1    inz
006100150211     d   IDCtipoCli                        inz  like(IDCtipoCli)
006200180227     d   IDCaas                            inz  like(IDCaas)
006300180227     d   IDClnp                            inz  like(IDClnp)
006400180227     d   IDCnrs                            inz  like(IDCnrs)
006500180227     d   IDCnsp                            inz  like(IDCnsp)
006600150212     d   wIDCcount                    9  0 inz
006700180227
006800180302       // - ?DS lettura campo TASFLO di TITAS?
006900180302     d dTASflo       e ds                  inz  qualified
007000150210
007100150210       //--------------------------------------------------------------
007200180302       //? Definizione variabili globali.                               ?
007300150210       //--------------------------------------------------------------
007400150210
007500180302       // - ?Flags booleani?
007600150210     d $EoF            s               n   inz
007700180302     d $Alert          s               n   inz
007800150210
007900180302       // - ?Stringa SQL da eseguire?
008000180227     d wSQL            s          32740    inz  varying
008100150210
008200180302       // - ?Campi di controllo "rottura"?
008300150212     d SaveIDCsnDir    s                   inz  like(IDSsnDir)
008400150212     d SaveIDCsnApp    s                   inz  like(IDSsnApp)
008500180302
008600180302       // - ?Numero totale di Disposizioni di Consegna elaborate?
008700180302       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
008800180302       //   ?con e senza Alert?
008900180302     d wTotDispSiAlert...
009000180302     d                 s                   inz  like(IDStotIDC)
009100180302     d wTotDispNoAlert...
009200180302     d                 s                   inz  like(IDStotIDC)
009300180302
009400180302       // - ?Numero totale di Spedizioni (a cui fanno riferimento?
009500180302       //   ?le disposizioni di consegna di cui sopra)?
009600180302       //   ?(per InsData/Fgs/TipoDis/NewLna/Dcr/TipoCli)?
009700180302       //   ?con e senza Alert?
009800180302     d wTotSpedSiAlert...
009900180302     d                 s                   inz  like(IDStotALRT)
010000180302     d wTotSpedNoAlert...
010100180302     d                 s                   inz  like(IDStotALRT)
010200150210
010300150210       //--------------------------------------------------------------
010400180302       //? Definizione prototipi procedure usate.                       ?
010500150210       //--------------------------------------------------------------
010600150210
010700150210
010800150210       //--------------------------------------------------------------
010900180302       //? Definizione key-list.                                        ?
011000150210       //--------------------------------------------------------------
011100150210
011200180302       // - ?File TITAS30C?
011300180302     d keyTITAS30    e ds                  extname( TITAS30C : *key )
011400180227     d                                     prefix( k_ )  inz
011500150210
011600150210       //--------------------------------------------------------------
011700180302       //? Riepilogo indicatori utilizzati.                             ?
011800150210       //--------------------------------------------------------------
011900150210
012000150210
012100150210       //--------------------------------------------------------------
012200180302       //? M A I N - L I N E                                            ?
012300150210       //--------------------------------------------------------------
012400150210
012500180302     c*//- *Entry        plist
012600180302     c*//-               parm                    KPJBA
012700150210
012800150210      /free
012900180302
013000180302       // - ?Impostazione opzioni per SQL?
013100180302       exec sql   set  option  DynUsrPrf = *Owner,
013200180302                               CloSqlCsr = *EndMod;
013300150210
013400180302       // - ?Impostazione chiusura *pgm?
013500180302       *inLR = *on;
013600150210
013700180302       // - ?Preparazione SQL per estrazione dati?
013800150210       exsr  sr_PrepSQL;
013900180302
014000180302       // - ?Dichiarazione cursore?
014100180302       exsr  sr_DeclareCursor;
014200180302
014300180302       // - ?Apertura cursore?
014400180302       exsr  sr_OpenCursor;
014500180302
014600180302       // - ?Ciclo di elaborazione?
014700150210       DoW  Not $EoF;
014800150210         exsr  sr_ReadCursor;
014900150210       EndDo;
015000150210
015100180302       // - ?Chiusura cursore?
015200150210       exsr  sr_CloseCursor;
015300150210
015400180302       // - ?Uscita dal *pgm?
015500180302       return;
015600150210
015700150210       //--------------------------------------------------------------
015800180302       //? Preparazione SQL.                                            ?
015900150210       //--------------------------------------------------------------
016000150210       BEGSR  sr_PrepSQL;
016100150210
016200150210         clear  wSQL;
016300150212         wSQL = 'with TIIDC as +
016400150212                      ( select int( IDCinsData / 1000000 ) as wIDCinsData, +
016500170412                               IDCfgs, +
016600170412                               substr( IDCalert, 10, 1 ) as wIDCstato, +
016700170412                               IDCtipoDis, +
016800150212                               case IDCnewLna when 0 then ''N'' +
016900150212                                              else ''S'' end as wIDCsnDir, +
017000150212                               case IDCdcr    when 0 then ''N'' +
017100150212                                              else ''S'' end as wIDCsnApp, +
017200180227                               IDCtipoCli, +
017300180227                               IDCaas, IDClnp, IDCnrs, IDCnsp +
017400150212                          from TIIDC00F +
017500180302                         where IDCip <> '' '' ) +
017600150212                 select TIIDC.*, count(*) +
017700150212                   from TIIDC +
017800170412                  group by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
017900180227                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
018000180227                           IDCaas, IDClnp, IDCnrs, IDCnsp +
018100170412                  order by wIDCinsData, IDCfgs, wIDCstato, IDCtipoDis, +
018200180227                           wIDCsnDir, wIDCsnApp, IDCtipoCli, +
018300180227                           IDCaas, IDClnp, IDCnrs, IDCnsp +
018400150212                    for fetch only';
018500150210
018600150210         exec sql   prepare S1   from :wSQL;
018700180302
018800180302       ENDSR;
018900180302
019000180302       //--------------------------------------------------------------
019100180302       //? Dichiarazione xursore.                                        ?
019200180302       //--------------------------------------------------------------
019300180302       BEGSR  sr_DeclareCursor;
019400150210
019500150210         exec sql   declare C1   cursor for S1;
019600150210
019700150210         if  SQLcode < *zero;
019800180302           $EoF  = *on;
019900180302           *inH1 = *on;
020000150210         endif;
020100150210
020200150210       ENDSR;
020300150210
020400150210       //--------------------------------------------------------------
020500180302       //? Apertura cursore.                                            ?
020600150210       //--------------------------------------------------------------
020700150210       BEGSR  sr_OpenCursor;
020800150210
020900150210         exec sql   open C1;
021000150210
021100150210         if  SQLcode < *zero;
021200180302           $EoF  = *on;
021300180302           *inH1 = *on;
021400150210         endif;
021500150210
021600150210       ENDSR;
021700150210
021800150210       //--------------------------------------------------------------
021900180302       //? Lettura cursore.                                             ?
022000150210       //--------------------------------------------------------------
022100150210       BEGSR  sr_ReadCursor;
022200150210
022300150210         clear  TIIDC_ds;
022400150210
022500150210         exec sql   fetch next   from C1   into :TIIDC_ds;
022600150210
022700150210         Select;
022800150210
022900180302           // - ?Fine File?
023000150210           When  SQLcode = 100;
023100150210             $EoF = *on;
023200180302             // - ?Scrittura ultimi record di Totali in TIIDS00F?
023300180301             exsr  sr_WriteTIIDS;
023400150210
023500180302           // - ?Errore SQL?
023600150210           When  SQLcode < *zero;
023700180302             $EoF  = *on;
023800180302             *inH1 = *on;
023900150210
024000180302           // - ?Elaborazione singolo record?
024100150210           Other;
024200150210             exsr  sr_ElabRec;
024300150210
024400150210         EndSl;
024500150210
024600150210       ENDSR;
024700150210
024800150210       //--------------------------------------------------------------
024900180302       //? Elaborazione coppia di record da TIIDC00F.                    ?
025000150210       //--------------------------------------------------------------
025100150210       BEGSR  sr_ElabRec;
025200150210
025300180302         // - ?Scrittura doppio record in TIIDS00F a Rottura...?
025400150212         If  TIIDC_ds.wIDCinsData <> IDSinsData      or
025500150212             TIIDC_ds.IDCfgs      <> IDSfgs          or
025600170412             TIIDC_ds.wIDCstato   <> IDSstato        or
025700150212             TIIDC_ds.IDCtipoDis  <> IDStipoDis      or
025800150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
025900150212             TIIDC_ds.wIDCsnDir   <> SaveIDCsnDir )  or
026000150212           ( TIIDC_ds.IDCtipoDis  =  '3'            and
026100150212             TIIDC_ds.wIDCsnDir   =  'N'            and
026200150212             TIIDC_ds.wIDCsnApp   <> SaveIDCsnApp )  or
026300150212             TIIDC_ds.IDCtipoCli  <> IDStipoCli;
026400150210
026500150210           exsr  sr_WriteTIIDS;
026600150210
026700150210         EndIf;
026800180227
026900180302         // - ?Verifica SE spedizione con ALERT?
027000180302         clear  $Alert;
027100180302         clear  keyTITAS30;
027200180302         k_TASaas = TIIDC_ds.IDCaas;
027300180302         k_TASlnp = TIIDC_ds.IDClnp;
027400180302         k_TASnrs = TIIDC_ds.IDCnrs;
027500180302         k_TASnsp = TIIDC_ds.IDCnsp;
027600180302         setll  %kds( keyTITAS30 )  TITAS30C;
027700180302         readE  %kds( keyTITAS30 : 4 )  TITAS30C;
027800180302         DoW  Not %eof( TITAS30C );
027900180302           dTASflo = TASflo;
028000180302           If  dTASflo.§FLOemd <> *blank;
028100180302             $Alert = *on;
028200180301             leave;
028300180227           EndIf;
028400180302           readE  %kds( keyTITAS30 : 4 )  TITAS30C;
028500180227         EndDo;
028600180302
028700180302         // - ?Incremento Totale Disposizioni (del tipo estratto) e?
028800180302         //   ?del relativo Totale Spedizioni - Con e Senza Alert?
028900180302         If  $Alert;
029000180302           wTotDispSiAlert += TIIDC_ds.wIDCcount;
029100180302           wTotSpedSiAlert += 1;
029200180302         Else;
029300180302           wTotDispNoAlert += TIIDC_ds.wIDCcount;
029400180302           wTotSpedNoAlert += 1;
029500180302         EndIf;
029600150210
029700150210       ENDSR;
029800150210
029900150210       //--------------------------------------------------------------
030000180302       //? Chiusura cursore.                                            ?
030100150210       //--------------------------------------------------------------
030200150210       BEGSR  sr_CloseCursor;
030300150210
030400150210         exec sql   close C1;
030500150210
030600150210       ENDSR;
030700150210
030800150210       //--------------------------------------------------------------
030900180302       //? Scrittura record nel file TIIDS00F.                          ?
031000150210       //--------------------------------------------------------------
031100150210       BEGSR  sr_WriteTIIDS;
031200150212
031300150212         SaveIDCsnDir = TIIDC_ds.wIDCsnDir;
031400150212         SaveIDCsnApp = TIIDC_ds.wIDCsnApp;
031500150210
031600180302         // - ?Scrittura record in TIIDS00F?
031700180302         //   ?riferito alle spedizioni SENZA Alert?
031800180302         If  wTotDispNoAlert <> *zero;
031900150210
032000180302           IDStotIDC  = wTotDispNoAlert;
032100180302           IDStotALRT = 0;
032200150210
032300150210           //______________
032400150210           write  TIIDS000;
032500150210           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
032600150210
032700150210         EndIf;
032800180302
032900180302         // - ?Scrittura record in TIIDS00F?
033000180302         //   ?riferito alle spedizioni CON Alert?
033100180302         If  wTotDispSiAlert <> *zero;
033200180302
033300180302           IDStotIDC  = wTotDispSiAlert;
033400180302           IDStotALRT = wTotSpedSiAlert;
033500180302
033600180302           //______________
033700180302           write  TIIDS000;
033800180302           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
033900180302
034000180302         EndIf;
034100150210
034200180302         // - ?Pulizia dei campi di lavoro usati per calcolare i totali?
034300180302         clear  wTotDispNoAlert;
034400180302         clear  wTotDispSiAlert;
034500180302         clear  wTotSpedNoAlert;
034600180302         clear  wTotSpedSiAlert;
034700150210         clear  TIIDS000;
034800180302
034900180302         // - ?Memorizzazione dei nuovi dati in TIIDS00F?
035000150212         IDSinsData = TIIDC_ds.wIDCinsData;
035100150211         IDSfgs     = TIIDC_ds.IDCfgs;
035200170412         IDSstato   = TIIDC_ds.wIDCstato;
035300150211         IDStipoDis = TIIDC_ds.IDCtipoDis;
035400150211         if  TIIDC_ds.IDCtipoDis  = '3';
035500150212           IDSsnDir  = TIIDC_ds.wIDCsnDir;
035600150212           if  TIIDC_ds.wIDCsnDir = 'N';
035700150212             IDSsnApp  = TIIDC_ds.wIDCsnApp;
035800150210           endif;
035900150210         endif;
036000150211         IDStipoCli = TIIDC_ds.IDCtipoCli;
036100150210
036200150210       ENDSR;
036300150210
036400150210      /end-free
