000100130702       //==============================================================
000200130708       //?TRMK42R * Verifica se Commerciale ANNULLABILE/DISATTIVABILE. ?
000300130702       //==============================================================
000400130702
000500130702       //--------------------------------------------------------------
000600130702       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130702       //--------------------------------------------------------------
000800130702
000900130702     /*PRM dbgview(*source)
001000130702     /*END
001100130702
001200130702       //--------------------------------------------------------------
001300130702       //?Specifiche di controllo.                                     ?
001400130702       //--------------------------------------------------------------
001500130702
001600130702     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130702     h dftactgrp(*no)
001800130702
001900130702       //--------------------------------------------------------------
002000130702       //?Dichiarazione file.                                          ?
002100130702       //--------------------------------------------------------------
002200130702
002300130708       // -?Anagrafica Commerciali?
002400130708     fAZCMM01L  if   e           k disk
002500130704
002600130704       // -?Anagrafica Particolarità clienti e PdC?
002700130704     fCNCLP01L  if   e           k disk
002800130704     fCNACO00F  if   e           k disk
002900130704
003000130704       // -?Anagrafica Clienti Potenziali?
003100130704     fTNCPO08L  if   e           k disk
003200130702
003300130702       //--------------------------------------------------------------
003400130702       //?Definizione costanti.                                        ?
003500130702       //--------------------------------------------------------------
003600130702
003700130702
003800130702       //--------------------------------------------------------------
003900130702       //?Definizione schiere.                                         ?
004000130702       //--------------------------------------------------------------
004100130702
004200130702       // -?Messaggi di errore?
004300130704     d $Msg            s             78    dim( 7) ctdata perrcd(1)
004400130702
004500130702       //--------------------------------------------------------------
004600130702       //?Definizione aree dati.                                       ?
004700130702       //--------------------------------------------------------------
004800130702
004900130704       // -?Dati utente?
005000130704     d §AzUte        e ds                  extname(AZUTE00F)
005100130704     d                                     dtaara
005200130704     d §DatiUte      e ds                  extname(dDatiUte)
005300130704     d                                     dtaara
005400130702
005500130702       //--------------------------------------------------------------
005600130702       //?Definizione strutture dati.                                  ?
005700130702       //--------------------------------------------------------------
005800130702
005900130702       // -?Parametri ricevuti?
006000130702     d KPJBA         e ds
006100130704     d TRMK42ds      e ds                  inz
006200130702
006300130702       //--------------------------------------------------------------
006400130702       //?Definizione variabili globali.                               ?
006500130702       //--------------------------------------------------------------
006600130702
006700130704       // -?Stringa SQL?
006800130704     d wSQL            s           4096    varying
006900130704
007000130704       // -?Conteggio record estratti via SQL [count(*)]?
007100130704     d wConta          s              5  0 inz
007200130704
007300130704       // -?Campi di comodo?
007400130704     d wDate           s              8  0 inz
007500130702
007600130702       //--------------------------------------------------------------
007700130702       //?Definizione prototipi procedure.                             ?
007800130702       //--------------------------------------------------------------
007900130702
008000130704       // -?Reperimento dati utente?
008100130704     d TIBS34ds      e ds                  inz
008200130704      /copy gaitrasrc/srcProtoPR,TIBS34R
008300130702
008400130702       //--------------------------------------------------------------
008500130702       //?Definizione key-list.                                        ?
008600130702       //--------------------------------------------------------------
008700130702
008800130708       // -?File AZCMM01L: Anagrafica Commerciali?
008900130708     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
009000130702     d                                     prefix(k_)   inz
009100130704
009200130704       // -?File CNCLP01L: Anagrafica Particolarità clienti e PdC?
009300130704     d k04cnclp01    e ds                  extname(CNCLP01L : *key)
009400130704     d                                     prefix(k_)   inz
009500130704
009600130704       // -?File CNACO00F: Anagrafica PdC?
009700130704     d k03cnaco00    e ds                  extname(CNACO00F : *key)
009800130704     d                                     prefix(k_)   inz
009900130704
010000130704       // -?File TNCPO08L: Anagrafica Clienti Potenziali?
010100130704     d k02tncpo08    e ds                  extname(TNCPO08L : *key)
010200130704     d                                     prefix(k_)   inz
010300130702
010400130702       //--------------------------------------------------------------
010500130702       //?M A I N - L I N E                                            ?
010600130702       //--------------------------------------------------------------
010700130702
010800130702     c     *Entry        plist
010900130702     c                   parm                    KPJBA
011000130702
011100130702      /free
011200130702
011300130702       // -?Operazioni iniziali?
011400130702       exsr sr_RoutInz;
011500130702
011600130704       // -?Verifica se il commerciale è disattivabile?
011700130704       exsr  sr_CtrlDisatt;
011800130702
011900130702       // -?Operazioni finali?
012000130702       exsr sr_RoutEnd;
012100130702
012200130702       //--------------------------------------------------------------
012300130702       //?Operazioni iniziali.                                         ?
012400130702       //--------------------------------------------------------------
012500130702       BEGSR  sr_RoutInz;
012600130702
012700130702         *inLR = *on;
012800130704
012900130704         // -?Reperimento dati job?
013000130704         exsr  sr_DatiJob;
013100130702
013200130702         // -?Reperimento data odierna?
013300130702         wDate = %int( %subst( %char( %dec( %timestamp() ) )
013400130702                               : 1 : 8 ) );
013500130702
013600130702         // -?Impostazione parametri ricevuti?
013700130704         TRMK42ds = KPJBU;
013800130702
013900130704         // -?Pulizia eventuali parametri di output?
014000130704         oMK42err = *off;
014100130704         clear  oMK42msg;
014200130702
014300130702       ENDSR;
014400130704
014500130704       //--------------------------------------------------------------
014600130704       //?Reperimento Dati del job (Utente/Operativi).                 ?
014700130704       //--------------------------------------------------------------
014800130704       BEGSR  sr_DatiJob;
014900130704
015000130704         in(e) §AzUte;
015100130704         if NOT %error;
015200130704           in(e) §DatiUte;
015300130704         endif;
015400130704         if %error or RSut = *blank;
015500130704           tibs34r ( tibs34ds );
015600130704           in §AzUte;
015700130704           in §DatiUte;
015800130704         endif;
015900130704
016000130704       ENDSR;
016100130703
016200130703       //--------------------------------------------------------------
016300130704       //?Verifica se il commerciale è disattivabile.                  ?
016400130703       //--------------------------------------------------------------
016500130703       BEGSR  sr_CtrlDisatt;
016600130704
016700130704         // ·?Commerciale inesistente o già disattivato?
016800130704         exsr  sr_CtrlComm;
016900130704
017000130704         // -?Commerciale con ancora Clienti nel PdC?
017100130704         exsr  sr_CtrlPDC;
017200130704
017300130704         // -?Commerciale con ancora Potenziali?
017400130704         exsr  sr_CtrlTNCPO;
017500130704
017600130704         // -?Commerciale con ancora Trattative Aperte?
017700130704         exsr  sr_CtrlTIVIS;
017800130704
017900130704         // -?Commerciale con ancora Attività Aperte?
018000130704         exsr  sr_CtrlTIATC;
018100130704
018200130704         // -?Commerciale Unificante con Figli?
018300130708         exsr  sr_CtrlAZCMM;
018400130704
018500130704       ENDSR;
018600130704
018700130704       //--------------------------------------------------------------
018800130704       //?Verifica se commerciale ancora attivo.                       ?
018900130704       //--------------------------------------------------------------
019000130704       BEGSR  sr_CtrlComm;
019100130704
019200130704         // -?Commerciale inesistente?
019300130708         k_CMMcod = iMK42cmm;
019400130708         chain  %kds(k01azcmm01)  AZCMM000;
019500130708         if  Not %found(AZCMM01L);
019600130704           oMK42err = 'A';
019700130704           oMK42msg = $Msg(01);
019800130704           exsr  sr_RoutEnd;
019900130704         endif;
020000130704
020100130704         // -?Commerciale già disattivato?
020200130708         if  CMMdtFin <= wDate;
020300130704           oMK42err = 'B';
020400130704           oMK42msg = $Msg(02);
020500130704           exsr  sr_RoutEnd;
020600130704         endif;
020700130704
020800130704       ENDSR;
020900130704
021000130704       //--------------------------------------------------------------
021100130704       //?Verifica se commerciale con ancora clienti nel PdC.          ?
021200130704       //--------------------------------------------------------------
021300130704       BEGSR  sr_CtrlPDC;
021400130704
021500130704         // -?Commerciale ancora presente sul PdC:?
021600130704         // -?Lettura di CNCLP per recuperare gli eventuali clienti?
021700130708         //    ?legati al commerciale?
021800130704         clear  k04cnclp01;
021900130704         k_CLPkut = 1;
022000130704         k_CLPkcc = DUTkci;
022100130708         k_CLPage = iMK42cmm;
022200130704         setll  %kds(k04cnclp01)  CNCLP000;
022300130704         reade  %kds(k04cnclp01 : 3)  CNCLP000;
022400130704         DoW  Not  %eof(CNCLP01L);
022500130704           // -?Controllo che il piano dei conti del cliente letto sia?
022600130704           //  ?annullato?
022700130704           clear  k03cnaco00;
022800130704           k_ACOkut = 1;
022900130704           k_ACOkcc = DUTkci;
023000130704           k_ACOksc = CLPksc;
023100130704           chain  %kds(k03cnaco00)  CNACO000;
023200130704           if  %found(CNACO00F)  and  ACOflg = *blank;
023300130704             oMK42err = 'C';
023400130704             oMK42msg = $Msg(03);
023500130704             exsr  sr_RoutEnd;
023600130704           endif;
023700130704           reade  %kds(k04cnclp01 : 3)  CNCLP000;
023800130704         EndDo;
023900130704
024000130704       ENDSR;
024100130704
024200130704       //--------------------------------------------------------------
024300130704       //?Verifica se commerciale con ancora clienti Potenziali.       ?
024400130704       //--------------------------------------------------------------
024500130704       BEGSR  sr_CtrlTNCPO;
024600130704
024700130704         // -?Lettura di TNCPO per recuperare gli eventuali potenziali?
024800130708         //    ?legati al commerciale?
024900130704         clear  k02tncpo08;
025000130708         k_CPOcmm = iMK42cmm;
025100130704         setll  %kds(k02tncpo08)  TNCPO000;
025200130704         reade  %kds(k02tncpo08 : 1)  TNCPO000;
025300130704         DoW  Not  %eof(TNCPO08L);
025400130704           if  CPOatb <> 'A';
025500130704             oMK42err = 'D';
025600130704             oMK42msg = $Msg(04);
025700130704             exsr  sr_RoutEnd;
025800130704           endif;
025900130704           reade  %kds(k02tncpo08 : 1)  TNCPO000;
026000130704         EndDo;
026100130704
026200130704       ENDSR;
026300130704
026400130704       //--------------------------------------------------------------
026500130704       //?Verifica se commerciale con ancora Trattative Aperte.        ?
026600130704       //--------------------------------------------------------------
026700130704       BEGSR  sr_CtrlTIVIS;
026800130704
026900130704         // -?Preparazione stringa SQL?
027000130704         wSQL = 'select count(*) from TIVIS00F +
027100130708                  where VIScmmi = ' + %editc( iMK42cmm : 'X' ) +
027200130708                   ' or VIScmm  = ' + %editc( iMK42cmm : 'X' );
027300130704
027400130704         exec sql   prepare S1  from :wSQL;
027500130704
027600130704         // -?Dichiarazione cursore?
027700130704         exec sql   declare C1  cursor for S1;
027800130704
027900130704         // -?Apertura cursore?
028000130704         exec sql   open C1;
028100130704
028200130704         // -?Lettura del file?
028300130704         clear  wConta;
028400130704         exec sql   fetch next   from C1   into :wConta;
028500130704
028600130704         select;
028700130704           // -?Non trovati dati: si pulisce il contatore di comodo?
028800130704           when  SqlCode = 100;
028900130704             clear  wConta;
029000130704           // -?Rilevato errore: uscita?
029100130704           when  SqlCode < *zero;
029200130704             oMK42err = *on;
029300130704             oMK42msg = 'Rilevato errore SQL: contattare il CED.';
029400130704             exsr  sr_RoutEnd;
029500130704         endsl;
029600130704
029700130704         // -?Chiusura del cursore?
029800130704         exec sql   close C1;
029900130704
030000130704         if  wConta > *zero;
030100130704           oMK42err = 'E';
030200130704           oMK42msg = $Msg(05);
030300130704           exsr  sr_RoutEnd;
030400130704         endif;
030500130704
030600130704       ENDSR;
030700130704
030800130704       //--------------------------------------------------------------
030900130704       //?Verifica se commerciale con ancora Attività Aperte.          ?
031000130704       //--------------------------------------------------------------
031100130704       BEGSR  sr_CtrlTIATC;
031200130704
031300130704         // -?Preparazione stringa SQL?
031400130704         wSQL = 'select count(*) from TIATC00F +
031500130708                  where (ATCcmm = ' + %editc( iMK42cmm : 'X' ) + ' or +
031600130708                         ATCco3 = ' + %editc( iMK42cmm : 'X' ) + ' or +
031700130708                         ATCcco = ' + %editc( iMK42cmm : 'X' ) + ') +
031800130704                    and ATCcco = 0';
031900130704
032000130704         exec sql   prepare S2  from :wSQL;
032100130704
032200130704         // -?Dichiarazione cursore?
032300130704         exec sql   declare C2  cursor for S2;
032400130704
032500130704         // -?Apertura cursore?
032600130704         exec sql   open C2;
032700130704
032800130704         // -?Lettura del file?
032900130704         clear  wConta;
033000130704         exec sql   fetch next   from C2   into :wConta;
033100130704
033200130704         select;
033300130704           // -?Non trovati dati: si pulisce il contatore di comodo?
033400130704           when  SqlCode = 100;
033500130704             clear  wConta;
033600130704           // -?Rilevato errore: uscita?
033700130704           when  SqlCode < *zero;
033800130704             oMK42err = *on;
033900130704             oMK42msg = 'Rilevato errore SQL: contattare il CED.';
034000130704             exsr  sr_RoutEnd;
034100130704         endsl;
034200130704
034300130704         // -?Chiusura del cursore?
034400130704         exec sql   close C2;
034500130704
034600130704         if  wConta > *zero;
034700130704           oMK42err = 'F';
034800130704           oMK42msg = $Msg(06);
034900130704           exsr  sr_RoutEnd;
035000130704         endif;
035100130704
035200130704       ENDSR;
035300130704
035400130704       //--------------------------------------------------------------
035500130704       //?Verifica se commerciale Unificante con Figli.                ?
035600130704       //--------------------------------------------------------------
035700130708       BEGSR  sr_CtrlAZCMM;
035800130704
035900130704         // -?Preparazione stringa SQL?
036000130708         wSQL = 'select count(*) from AZCMM00F +
036100130708                  where CMMcod <> ' + %editc( iMK42cmm : 'X' ) +
036200130708                  ' and CMMuni = '  + %editc( iMK42cmm : 'X' );
036300130704
036400130704         exec sql   prepare S3   from :wSQL;
036500130704
036600130704         // -?Dichiarazione cursore?
036700130704         exec sql   declare C3   cursor for S3;
036800130704
036900130704         // -?Apertura del cursore?
037000130704         exec sql   open C3;
037100130704
037200130704         // -?Lettura del cursore?
037300130704         clear  wConta;
037400130704         exec sql   fetch next   from C3   into :wConta;
037500130704
037600130704         select;
037700130704           // -?Non trovati dati: si pulisce il contatore di comodo?
037800130704           when  SqlCode = 100;
037900130704             clear  wConta;
038000130704           // -?Rilevato errore: uscita?
038100130704           when  SqlCode < *zero;
038200130704             oMK42err = *on;
038300130704             oMK42msg = 'Rilevato errore SQL: contattare il CED.';
038400130704             exsr  sr_RoutEnd;
038500130704         endsl;
038600130704
038700130704         // -?Chiusura del cursore?
038800130704         exec sql   close C3;
038900130704
039000130704         if  wConta > *zero;
039100130704           oMK42err = 'G';
039200130704           oMK42msg = $Msg(07);
039300130704           exsr  sr_RoutEnd;
039400130704         endif;
039500130704
039600130704       ENDSR;
039700130704
039800130704       //--------------------------------------------------------------
039900130704       //?Operazioni iniziali.                                         ?
040000130704       //--------------------------------------------------------------
040100130704       BEGSR  sr_RoutEnd;
040200130704
040300130704         // -?Restituzione parametri?
040400130704         KPJBU = TRMK42ds;
040500130704
040600130704         // -?Uscita?
040700130704         return;
040800130704
040900130704       ENDSR;
041000130704
041100130704      /end-free
041200130702
041300130702       //--------------------------------------------------------------
041400130702       //?Schiere a tempo di compilazione.                             ?
041500130702       //--------------------------------------------------------------
041600130702
041700130702** -?$Msg?(Messaggi di errore)?----------------------------------------------*
041800130708Codice commerciale errato                                                      1
041900130708Codice commerciale annullato, consentito solo il ripristino                    2
042000130710Commerciale ancora presente sul Piano dei Conti, impossibile cancellare        3
042100130710Commerciale ancora presente su Cod.Potenziali, impossibile cancellare          4
042200130710Commerciale con trattative aperte, impossibile cancellare                      5
042300130710Commerciale con attività aperte, impossibile cancellare                        6
042400130710Commerciale unificante con figli, impossibile cancellare                       7
