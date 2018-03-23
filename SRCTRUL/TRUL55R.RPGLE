000100110627       //==============================================================
000200090128       //?TRUL55R - Determina il nome immagine da impostare in TILAC ?
000300130410       //?                                                e in TIJDC ?
000400110627       //==============================================================
000500110627
000600110627       //--------------------------------------------------------------
000700110627       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110627       //--------------------------------------------------------------
000900110627
001000111111     /*PRM dbgview(*source)
001100110627     /*END
001200110627
001300110627       //--------------------------------------------------------------
001400110627       //?Specifiche di controllo.                                     ?
001500110627       //--------------------------------------------------------------
001600090128
001700090128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800090128     h dftactgrp(*no) actgrp(*caller)
001900090128
002000090128       //--------------------------------------------------------------
002100090128       //?Dichiarazione file.                                          ?
002200090128       //--------------------------------------------------------------
002300090128
002400090129     fTITAS30C  if   e           k disk    usropn
002500090128     fTITA430C  if   e           k disk    usropn
002600090303
002700090303     fTITAA30C  if   e           k disk    usropn
002800090303     fTABEL00F  if   e           k disk    usropn
002900090128
003000090128       //--------------------------------------------------------------
003100090128       //?Definizione costanti.                                        ?
003200090128       //--------------------------------------------------------------
003300090128
003400101102       // -?Tipo riferimento per *file TITA430C?
003500090128     d c_Ktrc          c                   const('A')
003600090212
003700101102       // -?Valori in TIBS02DS per *pgm TIBS02R?
003800090212     d TNTBE_Modo_Controllo...
003900090212     d                 c                   const('C')
004000090212     d TNTBE_Errore_NonTrovato...
004100090212     d                 c                   const('E')
004200090212     d TNTBE_Errore_Annullato...
004300090212     d                 c                   const('A')
004400090128
004500090128       //--------------------------------------------------------------
004600090128       //?Definizione schiere.                                         ?
004700090128       //--------------------------------------------------------------
004800090128
004900101102       // -?Messaggi di errore?
005000090129     d $Msg            s             78    dim( 4) ctdata  perrcd(1)
005100090128
005200090128       //--------------------------------------------------------------
005300090128       //?Definizione aree dati.                                       ?
005400090128       //--------------------------------------------------------------
005500090128
005600090128
005700090128       //--------------------------------------------------------------
005800090128       //?Definizione strutture dati.                                  ?
005900090128       //--------------------------------------------------------------
006000090128
006100101102       // -?Parametri ricevuti?
006200090128     d TRUL55ds      e ds
006300090129
006400120222       // -?Ricerca/Controllo tabelle?
006500090212     d kpjba         e ds                  inz
006600090303     d TIBS02ds      e ds                  qualified  inz
006700090212     d  T02mod       e                     inz(TNTBE_Modo_Controllo)
006800090303
006900101102       // -?Controllo/Decodifica cliente?
007000090303     d TIBS69ds      e ds                  qualified  inz
007100090303     d ds_CNACO      e ds                  extname(CNACO00F)
007200090303     d                                     qualified  inz
007300090303     d ds_CNIND      e ds                  extname(CNIND00F)
007400090303     d                                     qualified  inz
007500090303     d ds_CNCLP      e ds                  extname(CNCLP00F)
007600090303     d                                     qualified  inz
007700090303     d ds_FNCLS      e ds                  extname(FNCLS00F)
007800090303     d                                     qualified  inz
007900090212
008000101102       // -?Tab. "LAC" = Clienti per ritorno immagini?
008100090212     d dLAC          e ds                  inz
008200130410
008300130410       // -?Tab. "JDC" = Clienti per ritorno immagini?
008400130410     d dJDC          e ds                  inz
008500090303
008600101102       // -?Tab. "3A" = Codici Bolla?
008700090303     d ds3A          e ds                  inz
008800090303
008900101102       // -?DS per TITA400F trk "A" = Bolla Sede: Riferimento?
009000090303     d dTA4a         e ds                  inz
009100090128
009200090128       //--------------------------------------------------------------
009300090128       //?Definizione variabili globali.                               ?
009400090128       //--------------------------------------------------------------
009500090128
009600101102       // -?Indici?
009700090128     d xi              s              3  0 inz
009800090212
009900101102       // -?Flags booleani?
010000090212     d $Tibs02r        s               n   inz
010100090303     d $Tibs69r        s               n   inz
010200090303
010300101102       // -?Variabili di comodo?
010400090303     d wRagSoc         s                   like(TAArsc)   inz
010500130410     d wNIM            s                   like(O55nimd)  inz
010600090306
010700101102       // -?Parametri per XCHKCHAR?
010800090306      /copy gaitrasrc/srcProtoPI,XCHKCHAR
010900090128
011000090128       //--------------------------------------------------------------
011100090128       //?Definizione procedure usate.                                 ?
011200090128       //--------------------------------------------------------------
011300090128
011400101102       // -?Ricerca/Controllo tabelle?
011500090306      /copy gaitrasrc/srcProtoPR,TIBS02R
011600090303
011700101102       // -?Controllo/Decodifica cliente?
011800090306      /copy gaitrasrc/srcProtoPR,TIBS69R
011900090306
012000101102       // -?"Bonifica" stringa da caratteri indesiderati?
012100090306      /copy gaitrasrc/srcProtoPR,XCHKCHAR
012200090128
012300090128       //--------------------------------------------------------------
012400090128       //?Definizione key-list.                                        ?
012500090128       //--------------------------------------------------------------
012600090128
012700101102       // -?TITAS30C?
012800090129     d KeyTitas30c   e ds                  extname(TITAS30C : *key)
012900090129     d                                     inz
013000101102       // -?TITA430C?
013100090128     d KeyTita430c   e ds                  extname(TITA430C : *key)
013200090128     d                                     inz
013300090128
013400090128       //--------------------------------------------------------------
013500090128       //?M A I N - L I N E                                            ?
013600090128       //--------------------------------------------------------------
013700090128
013800090128     c     *Entry        plist
013900090128     c                   parm                    TRUL55ds
014000090128
014100090128      /free
014200090128
014300101102       // -?Operazioni iniziali?
014400101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
014500090129       exsr  sr_RoutInz;
014600090128
014700101102       // -?Impostazione del nome dell'immagine?
014800101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
014900090129       exsr  sr_Elab;
015000090128
015100101102       // -?Operazioni finali?
015200101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
015300090129       exsr  sr_RoutEnd;
015400090128
015500090129       //--------------------------------------------------------------
015600090129       //?Operazioni iniziali.                                         ?
015700090129       //--------------------------------------------------------------
015800090129       BEGSR  sr_RoutInz;
015900090129
016000101102         // -?Pulizia parametri di output?
016100101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
016200130410         clear  O55niml;
016300130410         clear  O55nimd;
016400090129         clear  O55msg;
016500090209         O55err = *off;
016600090209
016700101102         // -?Impostazione indicatore di ritorno?
016800101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
016900090209         select;
017000090209           when  I55tla = *blank;
017100090209             *inRT = *on;
017200090209           when  I55tla = 'C';
017300090209             *inLR = *on;
017400090209             exsr  sr_RoutEnd;
017500090209           other;
017600090209             *inLR = *on;
017700090209         endsl;
017800090129
017900101102         // -?Controllo parametri ricevuti?
018000101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
018100090129         select;
018200090129
018300130410           // -?Formato immagine mancante?
018400130410           when  I55fmi = *blank;
018500130410             O55err = *on;
018600130410             O55msg = $Msg(01);
018700130410             exsr  sr_RoutEnd;
018800130410
018900110627           // -?Formato immagine errato o mancante?
019000130410           when  I55fmi <> *blank;
019100130410             $Tibs02r = *on;
019200130410             reset  TIBS02ds;
019300130410             tibs02ds.T02cod = 'NIM';
019400130410             tibs02ds.T02ke1 = I55fmi;
019500130410             TNTBE_RicercaControllo ( kpjba : TIBS02ds );
019600130410             if  tibs02ds.T02Err = TNTBE_Errore_NonTrovato   or
019700130410                 tibs02ds.T02Err = TNTBE_Errore_Annullato;
019800090129             O55err = *on;
019900090129             O55msg = $Msg(01);
020000090129             exsr  sr_RoutEnd;
020100130410             endif;
020200090129
020300110627           // -?Numero spedizione errato o mancante?
020400090129           when  I55lnp = *zero   or
020500090129                 I55nsp = *zero   or
020600090129                 I55aas = *zero;
020700090129             O55err = *on;
020800090129             O55msg = $Msg(02);
020900090129             exsr  sr_RoutEnd;
021000090129
021100090129         endsl;
021200090129
021300101102         // -?Eventuale reperimento dati mancanti da TITAS30C?
021400101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
021500090212         clear  dLAC;
021600090129         IF       I55fmi = *blank
021700090212             OR ((I55fmi = 'DA'  or  I55fmi = 'DN'  or  I55fmi = 'NA'
021800090212                                 or  I55fmi = 'D0'  or  I55fmi = '0A')
021900090129                                and  I55mgs = *zero)
022000090129             OR ((I55fmi = 'DN'  or  I55fmi = 'N '  or  I55fmi = 'NA'
022100090209                                 or  I55fmi = 'NN'  or  I55fmi = 'NS'
022200130327                                 or  I55fmi = 'SN'
022300090212              or  I55fmi = 'D0'  or  I55fmi = '0 '  or  I55fmi = '0A'
022400090212                                 or  I55fmi = 'N0'  or  I55fmi = '0S'
022500101014                                 or  I55fmi = '10'  or  I55fmi = 'LN'
022600111111                                 or  I55fmi = 'RN'  or  I55fmi = 'AN'
022700120222                                 or  I55fmi = '3C'  or  I55fmi = 'A1')
022800090303                                and  I55rmn = *zero)
022900110627             OR   I55fmi = 'CE'
023000090303             OR   I55fmi = 'MM';
023100090303
023200090129           if  NOT %open(TITAS30C);
023300090129             open TITAS30C;
023400090129           endif;
023500090129           TASaas = I55aas;
023600090129           TASlnp = I55lnp;
023700090129           TASnrs = I55nrs;
023800090129           TASnsp = I55nsp;
023900090129           chain  %kds( KeyTitas30c : 4 )  TITAS30C;
024000090129           if  NOT  %found(TITAS30C);
024100090129             O55err = *on;
024200090129             O55msg = $Msg(03);
024300090129             exsr  sr_RoutEnd;
024400090129           endif;
024500090303
024600110627           // -?Impostazione "formato immagine" (non ricevuto)?
024700090129           if  I55fmi = *blank;
024800130410             // -?Ricerco sulla LAC x flag = LDV o per flag vuoto?
024900130410             IF  I55fld <> 'D';
025000090212             $Tibs02r = *on;
025100090212             reset  TIBS02ds;
025200090212             tibs02ds.T02cod = 'LAC';
025300090212             tibs02ds.T02ke1 = %editc( TASksc : 'X' );
025400090212             TNTBE_RicercaControllo ( kpjba : TIBS02ds );
025500090212             if  tibs02ds.T02Err = TNTBE_Errore_NonTrovato   or
025600090212                 tibs02ds.T02Err = TNTBE_Errore_Annullato;
025700090212               O55err = *on;
025800090212               O55msg = $Msg(04);
025900090212               exsr  sr_RoutEnd;
026000090212             endif;
026100090212             dLAC = tibs02ds.T02uni;
026200090212             I55fmi = §LACfmi;
026300130410             ELSE;
026400130410             // -?Ricerco sulla JDC x flag = DOC?
026500130410               $Tibs02r = *on;
026600130410               reset  TIBS02ds;
026700130410               tibs02ds.T02cod = 'JDC';
026800130410               tibs02ds.T02ke1 = %editc( TASksc : 'X' );
026900130410               TNTBE_RicercaControllo ( kpjba : TIBS02ds );
027000130410               if  tibs02ds.T02Err = TNTBE_Errore_NonTrovato   or
027100130410                   tibs02ds.T02Err = TNTBE_Errore_Annullato;
027200130410                 O55err = *on;
027300130410                 O55msg = $Msg(04);
027400130410                 exsr  sr_RoutEnd;
027500130410               endif;
027600130410               dJDC = tibs02ds.T02uni;
027700130410               I55fmi = §JDCfmi;
027800130410             ENDIF;
027900090129           endif;
028000090212
028100110627           // -?Impostazione giorno della spedizione (non ricevuto)?
028200090129           if  I55mgs = *zero;
028300090129             I55mgs = TASmgs;
028400090129           endif;
028500110627           // -?Impostazione "riferimento mittente numerico" (non ricevuto)?
028600101102           if  I55rmn = *zero;
028700090129             I55rmn = TASrmn;
028800090129           endif;
028900090303
029000110627           // -?Impostazione "ragione sociale"?
029100110627           //  ?(per clienti Mediolanum e Ceva/Enel)?
029200110627           If  I55fmi = 'MM'  or  I55fmi = 'CE';
029300090303             clear  wRagSoc;
029400101102             // -?Reperimento tab. "3A" = Codice Bolla?
029500090303             if  not %open(TABEL00F);
029600090303               open  TABEL00F;
029700090303             endif;
029800090303             clear  ds3A;
029900090303             TBLkey = TAScbo;
030000090303             chain ( 1 : '3A' : TBLkey )  TABEL;
030100090605             if  %found(TABEL00F);
030200090303               ds3A = TBLuni;
030300090605             endif;
030400101102             // -?Reperimento ragione sociale per Porto Assegnato?
030500090605             if  %subst(§3Atb1:1:1) = 'A';
030600101102               // -?Se non codificato aggancio TITAA?
030700090303               if  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero   or
030800090303                   %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
030900090303                 if  NOT %open(TITAA30C);
031000090303                   open TITAA30C;
031100090303                 endif;
031200110627                 chain  ( TASaas : TASlnp : TASnrs : TASnsp : 'M' )  TITAA30C;
031300090303                 if  %found(TITAA30C);
031400090303                   wRagSoc = TAArsc;
031500090303                 endif;
031600101102               // -?Se codificato aggancio anagrafico?
031700090303               else;
031800090303                 $Tibs69r = *on;
031900090303                 clear  TIBS69ds;
032000090303                 tibs69ds.I69kac = TASccm;
032100090303                 clear  ds_CNACO;
032200090303                 clear  ds_CNIND;
032300090303                 clear  ds_CNCLP;
032400090303                 clear  ds_FNCLS;
032500090303                 tibs69r ( TIBS69ds : ds_CNACO : ds_CNIND :
032600090303                                      ds_CNCLP : ds_FNCLS );
032700090303                 wRagSoc = ds_CNACO.ACOrag;
032800090303               endif;
032900101102             // -?Reperimento ragione sociale per Porto Franco?
033000090303             else;
033100090303               wRagSoc = TASrsd;
033200090303             endif;
033300110627
033400110627             // -?"Sistemazione" ragione sociale??(solo x cliente "Ceva" / "Enel")?
033500110627             if  I55fmi = 'CE'                                        and
033600110627                 %subst( wRagSoc :  1 : 19 ) = 'ENEL SERVIZI S.R.L.'  and
033700110627                 %subst( wRagSoc : 20 )     <> *blank;
033800110627               wRagSoc = %trim( %subst( wRagSoc : 21 ) );
033900110627             endif;
034000110627
034100110627             // -?"Pulizia" ragione sociale da caratteri non ammessi?
034200090306             TxtInOut = wRagSoc;
034300090306             ElencoChar = '\/:*?''"<>|';
034400090306             TipoElenco = '1';
034500090306             xchkchar ( TxtInOut :
034600090306                        ElencoChar :
034700090306                        TipoElenco :
034800090306                        CharSost :
034900090306                        UpperCase :
035000090306                        ChkNull :
035100090306                        CharNull :
035200090306                        Esito );
035300090306             select;
035400101102               // -?Nessuna modifica effettuata?
035500090306               when  Esito = *off;
035600101102               // -?Modifica effettuata?
035700090306               when  Esito = *on;
035800090306                 wRagSoc = TxtInOut;
035900101102               // -?Errore nei parametri di input?
036000090306               when  Esito = 'I';
036100090306                 clear  wRagSoc;
036200101102               // -?Errore di elaborazione?
036300090306               when  Esito = 'E';
036400090306                 clear  wRagSoc;
036500090306             endsl;
036600090303           EndIf;
036700090303
036800090129         ENDIF;
036900090129
037000101102         //?Eventuale reperimento dati mancanti (RMA) da TITA430C?
037100101102         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
037200090129         clear dTA4a;
037300090129         IF  (%subst( I55fmi : 1 : 1 ) = 'A'  or  I55fmi = 'DA'
037400101014                                              or  I55fmi = 'LN'
037500090212                                              or  I55fmi = 'NA'
037600101014                                              or  I55fmi = 'RN'
037700110627                                              or  I55fmi = '0A'
037800111111                                              or  I55fmi = '3C'
037900110627                                              or  I55fmi = 'CE')  and
038000090129              I55rma = *blank;
038100090129           if  NOT %open(TITA430C);
038200090129             open TITA430C;
038300090129           endif;
038400090129           TA4aas = I55aas;
038500090129           TA4lnp = I55lnp;
038600090129           TA4nrs = I55nrs;
038700090129           TA4nsp = I55nsp;
038800090129           TA4trc = c_Ktrc;
038900090129           chain  %kds( KeyTita430c )  TITA430C;
039000090129           if  %found(TITA430C);
039100090129             dTA4a = TA4not;
039200090129           endif;
039300090129           I55rma = %trim(§TA4aRMA);
039400160203           // -?"Pulizia" riferimento alfanumerico dei caratteri non ammessi?
039500160203           TxtInOut = I55rma;
039600160203           ElencoChar = '@';
039700160203           TipoElenco = '1';
039800160203           xchkchar ( TxtInOut :
039900160203                      ElencoChar :
040000160203                      TipoElenco :
040100160203                      CharSost :
040200160203                      UpperCase :
040300160203                      ChkNull :
040400160203                      CharNull :
040500160203                      Esito );
040600160203           SELECT;
040700160203           // -?Nessuna modifica effettuata?
040800160203           WHEN  Esito = *off;
040900160203           // -?Modifica effettuata?
041000160203           WHEN  Esito = *on;
041100160203             I55rma = TxtInOut;
041200160203           // -?Errore nei parametri di input?
041300160203           WHEN  Esito = 'I';
041400160203             clear I55rma;
041500160203           // -?Errore di elaborazione?
041600160203           WHEN  Esito = 'E';
041700160203             clear I55rma;
041800160203           ENDSL;
041900090129         ENDIF;
042000090129
042100090129       ENDSR;
042200090129
042300090129       //--------------------------------------------------------------
042400090129       //?Elaborazione.                                                ?
042500090129       //--------------------------------------------------------------
042600090129       BEGSR  sr_Elab;
042700130410
042800130410         clear  wNIM;
042900090129
043000090129         SELECT;
043100090129
043200111206           // -?"S1" = numero spedizione?
043300111206           WHEN  I55fmi = 'S1';
043400130410               wNIM = %subst( %editc( I55aas : 'X' ) :
043500111206                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
043600100203                    + %editc( I55lnp : 'X' )
043700100203                    + %editc( I55nrs : 'X' )
043800100203                    + %editc( I55nsp : 'X' );
043900090129
044000101102           // -?"N " = Riferimento Mittente Numerico?
044100090129           WHEN  I55fmi = 'N ';
044200130410               wNIM = %trim( %editc( I55rmn : 'X' ) );
044300090212
044400101102           // -?"0 " = Riferimento Mittente Numerico?
044500101102           //         ?(senza zeri NON significativi)?
044600090212           WHEN  I55fmi = '0 ';
044700130410               wNIM = %trim( %editc( I55rmn : '3' ) );
044800090209
044900101102           // -?"10" = Ultime 10 cifre del Riferimento Mittente Numerico?
045000090209           WHEN  I55fmi = '10';
045100130410               wNIM = %subst( %editc( I55rmn : 'X' ) :
045200090209                              %len( I55rmn ) - 9 );
045300101102
045400101102           // -?"AN" = Anno spedizione _ RMN _ Numero spedizione?
045500101102           WHEN  I55fmi = 'AN';
045600130410               wNIM = %editc( I55aas : 'X' ) + '_' +
045700101102                      %editc( I55rmn : 'X' ) + '_' +
045800101102                      %subst( %editc( I55aas : 'X' ) :
045900120222                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 ) +
046000101102                      %editc( I55lnp : 'X' ) +
046100101102                      %editc( I55nrs : 'X' ) +
046200101102                      %editc( I55nsp : 'X' );
046300090129
046400120222           // -?"Ax" = Riferimento Mittente Alfanumerico ( _ ... )?
046500101102           //  ?"DA" = Data spedizione _ RiferimentoMittenteAlfanumerico?
046600101102           //  ?"NA" = RMN _ RMA _ Data spedizione?
046700101102           //  ?"RN" = RMN _ RMA _ Num. spedizione?(solo x cliente L.N.)?
046800101102           //  ?"LN" = RMN _ RMA?
046900101102           //  ?"0A" = RMN (editato) _ RMA _ Data spedizione?
047000111111           //  ?"3C" = RMA _ RMN (editato)?(solo x cliente TRECCANI)?
047100111111           //  ?"CE" = RMA _ Ragione Sociale Destinatario?(solo x cliente CEVA/ENEL)?
047200090129           WHEN  %subst(I55fmi : 1 : 1) = 'A'  or  I55fmi = 'DA'
047300090212                                               or  I55fmi = 'NA'
047400101014                                               or  I55fmi = 'LN'
047500101014                                               or  I55fmi = 'RN'
047600110627                                               or  I55fmi = '0A'
047700111111                                               or  I55fmi = '3C'
047800110627                                               or  I55fmi = 'CE';
047900111111             // -?Verifica correttezza del riferimento alfanumerico?
048000111111             //  ?(solo x cliente TRECCANI)?
048100111111             if  I55fmi = '3C'  and  %subst( I55rma : 1 : 2 ) <> 'CL';
048200111111               clear  I55rma;
048300111111             endif;
048400111111
048500110627             // -?"Compattamento" interno del riferimento alfanumerico?
048600090129             if  I55rma <> *blank;
048700090129               for  xi = 1  to  %len( %trimr( I55rma ) );
048800090129                 if  %subst( I55rma : xi : 1 ) = *blank;
048900090129                   I55rma = %subst( I55rma : 1 : xi - 1 ) +
049000090129                            %subst( I55rma : xi + 1 );
049100090129                   xi -= 1;
049200090129                 endif;
049300090129               endfor;
049400090129             endif;
049500101102             // -?Eliminazione dei caratteri NON ammessi dal RMA?
049600090605             TxtInOut = I55rma;
049700090605             ElencoChar = '\/:*?''"<>|';
049800090605             TipoElenco = '1';
049900090605             xchkchar ( TxtInOut :
050000090605                        ElencoChar :
050100090605                        TipoElenco :
050200090605                        CharSost :
050300090605                        UpperCase :
050400090605                        ChkNull :
050500090605                        CharNull :
050600090605                        Esito );
050700090605             select;
050800101102               // -?Nessuna modifica effettuata?
050900090605               when  Esito = *off;
051000101102               // -?Modifica effettuata?
051100090605               when  Esito = *on;
051200090605                 I55rma = TxtInOut;
051300101102               // -?Errore nei parametri di input?
051400090605               when  Esito = 'I';
051500090605                 clear  I55rma;
051600101102               // -?Errore di elaborazione?
051700090605               when  Esito = 'E';
051800090605                 clear  I55rma;
051900090605             endsl;
052000090129             select;
052100090129
052200101102               // -?"AS" = Riferimento Mittente Alfanumerico _?
052300101102               //       ?+ Numero spedizione?
052400090129               when  I55fmi = 'AS';
052500130410                   wNIM = %trim( I55rma ) + '_'
052600090129                        + %subst( %editc( I55aas : 'X' ) :
052700101014                            %len( %triml( %editc(I55aas:'X') ) )-1 : 2 )
052800090129                        + %editc( I55lnp : 'X' )
052900090129                        + %editc( I55nrs : 'X' )
053000090129                        + %editc( I55nsp : 'X' );
053100090129
053200101102               // -?"A " = Riferimento Mittente Alfanumerico?
053300090129               when  I55fmi = 'A ';
053400090324                 if  I55rma <> *blank;
053500130410                     wNIM = %trim( I55rma );
053600090324                 else;
053700130410                     wNIM = 'NO_RMA_'
053800090324                          + %editc( I55aas : 'X' )
053900090324                          + %editc( I55lnp : 'X' )
054000090324                          + %editc( I55nrs : 'X' )
054100090324                          + %editc( I55nsp : 'X' );
054200090324                 endif;
054300120222
054400120222               // -?"A1" = Riferimento Mittente Alfanumerico _?
054500120222               //       ?+ Riferimento Mittente Numerico (editato)?
054600120222               when  I55fmi = 'A1';
054700130410                   wNIM = %trim( I55rma ) + '_'
054800120222                        + %trim( %editc( I55rmn : '3' ) );
054900090129
055000101102               // -?"AA" = Anno spedizione _?
055100101102               //       ?+ Riferimento Mittente Alfanumerico?
055200090129               when  I55fmi = 'AA';
055300130410                   wNIM = %trim( %editc( I55aas : 'X' ) )
055400090129                        + '_' + %trim( I55rma );
055500090129
055600101102               // -?"DA" = Data spedizione _?
055700101102               //       ?+ Riferimento Mittente Alfanumerico?
055800090129               when  I55fmi = 'DA';
055900130410                   wNIM = %char( I55aas * 10000 + I55mgs )
056000090129                        + '_' + %trim( I55rma );
056100101014
056200101102               // -?"LN" = Riferimento Mittente Numerico _?
056300101102               //       ?+ Riferimento Mittente Alfanumerico?
056400101014               when  I55fmi = 'LN';
056500130410                   wNIM = %trim( %editc( I55rmn : 'X' ) )
056600101014                        + '_' + %trim( I55rma );
056700090129
056800101102               // -?"NA" = Riferimento Mittente Numerico _?
056900101102               //       ?+ Riferimento Mittente Alfanumerico _?
057000101102               //       ?+ Data spedizione?
057100090129               when  I55fmi = 'NA';
057200130410                   wNIM = %trim( %editc( I55rmn : 'X' ) )
057300090129                        + '_' + %trim( I55rma )
057400090129                        + '_' + %char( I55aas * 10000 + I55mgs );
057500090212
057600101102               // -?"RN" = Riferimento Mittente Numerico _?
057700101102               //       ?+ Riferimento Mittente Alfanumerico _?
057800101102               //       ?+ Numero spedizione?
057900101102               //  ?(solo x cliente "L.N.")?
058000101102               //  ?La lunghezza massima è 40 bytes.?
058100101102               //  ?SE il riferimento mittente alfanumerico risultasse?
058200101102               //    ?più lungo del "normale", il numero spedizione al?
058300101102               //    ?completo NON ci starebbe più: AAS+LNP+NRS+NSP?
058400101102               //                                  ? 2 + 3 + 2 + 7 = 14?
058500101102               //  ?In questo caso viene riportato SOLO il numero NSP.?
058600101014               when  I55fmi = 'RN';
058700130410                   wNIM = %trim( %editc( I55rmn : 'X' ) );
058800101015                 if  I55rma = *blank;
058900130410                     wNIM = %trimr( wNIM ) + '_ _';
059000101015                 else;
059100130410                     wNIM = %trimr( wNIM ) + '_' +
059200101015                            %trim( I55rma ) + '_';
059300101015                 endif;
059400130410                 if  %len( wNIM ) - %len( %trimr( wNIM ) ) >= 14;
059500130410                     wNIM = %trimr( wNIM ) +
059600101015                            %subst( %editc( I55aas : 'X' ) :
059700101015                              %len( %editc( I55aas : 'X' ) )-1 : 2 ) +
059800101015                            %editc( I55lnp : 'X' ) +
059900101015                            %editc( I55nrs : 'X' ) +
060000101015                            %editc( I55nsp : 'X' );
060100101015                 else;
060200130410                     wNIM = %trimr( wNIM ) +
060300101015                            %editc( I55nsp : 'X' );
060400101015                 endif;
060500101014
060600101102               // -?"0A" = Riferimento Mittente Numerico (editato) _?
060700101102               //       ?+ Riferimento Mittente Alfanumerico _?
060800101102               //       ?+ Data spedizione?
060900090212               when  I55fmi = '0A';
061000130410                   wNIM = %trim( %editc( I55rmn : '3' ) )
061100090212                        + '_' + %trim( I55rma )
061200090212                        + '_' + %char( I55aas * 10000 + I55mgs );
061300111111
061400111111               // -?"3C" = Riferimento Mittente Alfanumerico _?
061500111111               //       ?+ Riferimento Mittente Numerico (editato)?
061600111111               when  I55fmi = '3C';
061700130410                   wNIM = %trim( I55rma ) + '_'
061800111111                        + %trim( %editc( I55rmn : '3' ) );
061900110627
062000110627               // -?"CE" = Riferim.Mitt.Alfanumerico _ Ragione Sociale?
062100110627               when  I55fmi = 'CE';
062200130410                   wNIM = %trim( I55rma ) + '_'
062300110627                        + %trim( %subst( wRagSoc : 1 ) );
062400090129
062500090129             endsl;
062600090129
062700101102           // -?"NS" = Riferimento Mittente Numerico _ Numero Spedizione?
062800090129           WHEN  I55fmi = 'NS';
062900130410               wNIM = %trim( %editc( I55rmn : 'X' ) ) + '_'
063000090129                    + %subst( %editc( I55aas : 'X' ) :
063100120222                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
063200090129                    + %editc( I55lnp : 'X' )
063300090129                    + %editc( I55nrs : 'X' )
063400090129                    + %editc( I55nsp : 'X' );
063500090212
063600101102           // -?"0S" = Riferim.Mitt.Numerico (editato) _ Numero Spedizione?
063700090212           WHEN  I55fmi = '0S';
063800130410               wNIM = %trim( %editc( I55rmn : '3' ) ) + '_'
063900090212                    + %subst( %editc( I55aas : 'X' ) :
064000090212                         %len( %editc(I55aas:'X') ) - 1 : 2 )
064100090212                    + %editc( I55lnp : 'X' )
064200090212                    + %editc( I55nrs : 'X' )
064300090212                    + %editc( I55nsp : 'X' );
064400090129
064500101102           // -?"NN" = Anno Spedizione _ Riferimento Mittente Numerico?
064600090129           WHEN  I55fmi = 'NN';
064700130410               wNIM = %trim( %editc( I55aas : 'X' ) ) + '_'
064800090212                    + %trim( %editc( I55rmn : 'X' ) );
064900090212
065000101102           // -?"N0" = Anno Spedizione _ Riferim.Mitt.Numerico (editato)?
065100090212           WHEN  I55fmi = 'N0';
065200130410               wNIM = %trim( %editc( I55aas : 'X' ) ) + '_'
065300090212                    + %trim( %editc( I55rmn : '3' ) );
065400090129
065500101102           // -?"DN" = Data Spedizione _ Riferimento Mittente Numerico?
065600090129           WHEN  I55fmi = 'DN';
065700130410               wNIM = %char( I55aas * 10000 + I55mgs ) + '_'
065800090212                    + %trim( %editc( I55rmn : 'X' ) );
065900090212
066000101102           // -?"D0" = Data Spedizione _ Riferim.Mitt.Numerico (editato)?
066100090212           WHEN  I55fmi = 'D0';
066200130410               wNIM = %char( I55aas * 10000 + I55mgs ) + '_'
066300090212                    + %trim( %editc( I55rmn : '3' ) );
066400090303
066500101102           // -?"MM" = Ragione Sociale _ Data Spedizione _ Num. Spedizione?
066600090303           WHEN  I55fmi = 'MM';
066700130410               wNIM = %trim( %subst( wRagSoc : 1 : 22 ) ) + '_'
066800090303                    + %subst( %editc( I55aas : 'X' ) : 3 : 2 )
066900090303                    + %editc( I55mgs : 'X' ) + '_'
067000090303                    + %editc( I55lnp : 'X' )
067100090303                    + %editc( I55nsp : 'X' );
067200130327
067300130327           // -?"SN" = numero spedizione_Riferimento Mittente Numerico (editato)?
067400130327           WHEN  I55fmi = 'SN';
067500130410               wNIM = %subst( %editc( I55aas : 'X' ) :
067600130327                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
067700130327                    + %editc( I55lnp : 'X' )
067800130327                    + %editc( I55nrs : 'X' )
067900130327                    + %editc( I55nsp : 'X' ) + '_'
068000130327                    + %trim( %editc( I55rmn : '3' ) );
068100090129
068200090129         ENDSL;
068300130410
068400130410       // -?Per flag = LDV o vuoto imposto il campo + corto
068500130410         IF  I55fld <> 'D';
068600130410           O55niml = wNIM;
068700130410       // -?Per flag = DOC imposto il campo + lungo
068800130410         ELSE;
068900130410           O55nimd = wNIM;
069000130410         ENDIF;
069100090129
069200090129       ENDSR;
069300090129
069400090129       //--------------------------------------------------------------
069500090129       //?Operazioni finali.                                           ?
069600090129       //--------------------------------------------------------------
069700090129       BEGSR  sr_RoutEnd;
069800090212
069900101102         // -?Chiusura applicazioni richiamate?
070000090212         if  I55tla  <> *blank   and   $Tibs02r = *on;
070100090212           clear  tibs02ds;
070200090212           tibs02ds.T02tla = 'C';
070300090212           TNTBE_RicercaControllo ( kpjba : tibs02ds );
070400090212         endif;
070500090303         if  I55tla  <> *blank   and   $Tibs69r = *on;
070600090303           clear  tibs69ds;
070700090303           tibs69ds.I69tla = 'C';
070800090303           clear  ds_CNACO;
070900090303           clear  ds_CNIND;
071000090303           clear  ds_CNCLP;
071100090303           clear  ds_FNCLS;
071200090303           tibs69r ( TIBS69ds : ds_CNACO : ds_CNIND :
071300090303                                ds_CNCLP : ds_FNCLS );
071400090303         endif;
071500090129
071600090129         return;
071700090129
071800090129       ENDSR;
071900090129
072000090128      /end-free
072100090129
072200090129       //--------------------------------------------------------------
072300090129       //?Messaggi di errore.                                          ?
072400090129       //--------------------------------------------------------------
072500110627
072600101102** -?$Msg?-------------------------------------------------------------------*
072700090129Flag "nome immagine" errato o mancante                                          1
072800090129Numero spedizione obbligatorio                                                  2
072900090129Numero spedizione errato                                                        3
073000090129Non prevista immagine per la spedizione                                         4
