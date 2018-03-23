000100110627       //==============================================================
000200090128       //?TRUL55R - Determina il nome immagine da impostare in TILAC ?
000300110627       //==============================================================
000400110627
000500110627       //--------------------------------------------------------------
000600110627       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700110627       //--------------------------------------------------------------
000800110627
000900111111     /*PRM dbgview(*source)
001000110627     /*END
001100110627
001200110627       //--------------------------------------------------------------
001300110627       //?Specifiche di controllo.                                     ?
001400110627       //--------------------------------------------------------------
001500090128
001600090128     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700090128     h dftactgrp(*no) actgrp(*caller)
001800090128
001900090128       //--------------------------------------------------------------
002000090128       //?Dichiarazione file.                                          ?
002100090128       //--------------------------------------------------------------
002200090128
002300090129     fTITAS30C  if   e           k disk    usropn
002400090128     fTITA430C  if   e           k disk    usropn
002500090303
002600090303     fTITAA30C  if   e           k disk    usropn
002700090303     fTABEL00F  if   e           k disk    usropn
002800090128
002900090128       //--------------------------------------------------------------
003000090128       //?Definizione costanti.                                        ?
003100090128       //--------------------------------------------------------------
003200090128
003300101102       // -?Tipo riferimento per *file TITA430C?
003400090128     d c_Ktrc          c                   const('A')
003500090212
003600101102       // -?Valori in TIBS02DS per *pgm TIBS02R?
003700090212     d TNTBE_Modo_Controllo...
003800090212     d                 c                   const('C')
003900090212     d TNTBE_Errore_NonTrovato...
004000090212     d                 c                   const('E')
004100090212     d TNTBE_Errore_Annullato...
004200090212     d                 c                   const('A')
004300090128
004400090128       //--------------------------------------------------------------
004500090128       //?Definizione schiere.                                         ?
004600090128       //--------------------------------------------------------------
004700090128
004800101102       // -?Messaggi di errore?
004900090129     d $Msg            s             78    dim( 4) ctdata  perrcd(1)
005000090128
005100090128       //--------------------------------------------------------------
005200090128       //?Definizione aree dati.                                       ?
005300090128       //--------------------------------------------------------------
005400090128
005500090128
005600090128       //--------------------------------------------------------------
005700090128       //?Definizione strutture dati.                                  ?
005800090128       //--------------------------------------------------------------
005900090128
006000101102       // -?Parametri ricevuti?
006100090128     d TRUL55ds      e ds
006200090129
006300120222       // -?Ricerca/Controllo tabelle?
006400090212     d kpjba         e ds                  inz
006500090303     d TIBS02ds      e ds                  qualified  inz
006600090212     d  T02mod       e                     inz(TNTBE_Modo_Controllo)
006700090303
006800101102       // -?Controllo/Decodifica cliente?
006900090303     d TIBS69ds      e ds                  qualified  inz
007000090303     d ds_CNACO      e ds                  extname(CNACO00F)
007100090303     d                                     qualified  inz
007200090303     d ds_CNIND      e ds                  extname(CNIND00F)
007300090303     d                                     qualified  inz
007400090303     d ds_CNCLP      e ds                  extname(CNCLP00F)
007500090303     d                                     qualified  inz
007600090303     d ds_FNCLS      e ds                  extname(FNCLS00F)
007700090303     d                                     qualified  inz
007800090212
007900101102       // -?Tab. "LAC" = Clienti per ritorno immagini?
008000090212     d dLAC          e ds                  inz
008100090303
008200101102       // -?Tab. "3A" = Codici Bolla?
008300090303     d ds3A          e ds                  inz
008400090303
008500101102       // -?DS per TITA400F trk "A" = Bolla Sede: Riferimento?
008600090303     d dTA4a         e ds                  inz
008700090128
008800090128       //--------------------------------------------------------------
008900090128       //?Definizione variabili globali.                               ?
009000090128       //--------------------------------------------------------------
009100090128
009200101102       // -?Indici?
009300090128     d xi              s              3  0 inz
009400090212
009500101102       // -?Flags booleani?
009600090212     d $Tibs02r        s               n   inz
009700090303     d $Tibs69r        s               n   inz
009800090303
009900101102       // -?Variabili di comodo?
010000090303     d wRagSoc         s                   like(TAArsc)   inz
010100090306
010200101102       // -?Parametri per XCHKCHAR?
010300090306      /copy gaitrasrc/srcProtoPI,XCHKCHAR
010400090128
010500090128       //--------------------------------------------------------------
010600090128       //?Definizione procedure usate.                                 ?
010700090128       //--------------------------------------------------------------
010800090128
010900101102       // -?Ricerca/Controllo tabelle?
011000090306      /copy gaitrasrc/srcProtoPR,TIBS02R
011100090303
011200101102       // -?Controllo/Decodifica cliente?
011300090306      /copy gaitrasrc/srcProtoPR,TIBS69R
011400090306
011500101102       // -?"Bonifica" stringa da caratteri indesiderati?
011600090306      /copy gaitrasrc/srcProtoPR,XCHKCHAR
011700090128
011800090128       //--------------------------------------------------------------
011900090128       //?Definizione key-list.                                        ?
012000090128       //--------------------------------------------------------------
012100090128
012200101102       // -?TITAS30C?
012300090129     d KeyTitas30c   e ds                  extname(TITAS30C : *key)
012400090129     d                                     inz
012500101102       // -?TITA430C?
012600090128     d KeyTita430c   e ds                  extname(TITA430C : *key)
012700090128     d                                     inz
012800090128
012900090128       //--------------------------------------------------------------
013000090128       //?M A I N - L I N E                                            ?
013100090128       //--------------------------------------------------------------
013200090128
013300090128     c     *Entry        plist
013400090128     c                   parm                    TRUL55ds
013500090128
013600090128      /free
013700090128
013800101102       // -?Operazioni iniziali?
013900101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
014000090129       exsr  sr_RoutInz;
014100090128
014200101102       // -?Impostazione del nome dell'immagine?
014300101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
014400090129       exsr  sr_Elab;
014500090128
014600101102       // -?Operazioni finali?
014700101102       //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
014800090129       exsr  sr_RoutEnd;
014900090128
015000090129       //--------------------------------------------------------------
015100090129       //?Operazioni iniziali.                                         ?
015200090129       //--------------------------------------------------------------
015300090129       BEGSR  sr_RoutInz;
015400090129
015500101102         // -?Pulizia parametri di output?
015600101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
015700090129         clear  O55nim;
015800090129         clear  O55msg;
015900090209         O55err = *off;
016000090209
016100101102         // -?Impostazione indicatore di ritorno?
016200101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
016300090209         select;
016400090209           when  I55tla = *blank;
016500090209             *inRT = *on;
016600090209           when  I55tla = 'C';
016700090209             *inLR = *on;
016800090209             exsr  sr_RoutEnd;
016900090209           other;
017000090209             *inLR = *on;
017100090209         endsl;
017200090129
017300101102         // -?Controllo parametri ricevuti?
017400101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
017500090129         select;
017600090129
017700110627           // -?Formato immagine errato o mancante?
017800090129           when  I55fmi <> *blank and
017900100203                 I55fmi <> 'S1'   and
018000130327                 I55fmi <> 'SN'   and
018100090129                 I55fmi <> 'N '   and
018200090212                 I55fmi <> '0 '   and
018300090129                 I55fmi <> 'A '   and
018400101102                 I55fmi <> 'AN'   and
018500090129                 I55fmi <> 'AA'   and
018600090129                 I55fmi <> 'AS'   and
018700120222                 I55fmi <> 'A1'   and
018800090129                 I55fmi <> 'DA'   and
018900090129                 I55fmi <> 'DN'   and
019000090212                 I55fmi <> 'D0'   and
019100090129                 I55fmi <> 'NA'   and
019200101014                 I55fmi <> 'LN'   and
019300101014                 I55fmi <> 'RN'   and
019400090212                 I55fmi <> '0A'   and
019500090129                 I55fmi <> 'NN'   and
019600090212                 I55fmi <> 'N0'   and
019700090209                 I55fmi <> 'NS'   and
019800090212                 I55fmi <> '0S'   and
019900090303                 I55fmi <> '10'   and
020000111111                 I55fmi <> '3C'   and
020100110627                 I55fmi <> 'CE'   and
020200090303                 I55fmi <> 'MM';
020300090129             O55err = *on;
020400090129             O55msg = $Msg(01);
020500090129             exsr  sr_RoutEnd;
020600090129
020700110627           // -?Numero spedizione errato o mancante?
020800090129           when  I55lnp = *zero   or
020900090129                 I55nsp = *zero   or
021000090129                 I55aas = *zero;
021100090129             O55err = *on;
021200090129             O55msg = $Msg(02);
021300090129             exsr  sr_RoutEnd;
021400090129
021500090129         endsl;
021600090129
021700101102         // -?Eventuale reperimento dati mancanti da TITAS30C?
021800101102         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
021900090212         clear  dLAC;
022000090129         IF       I55fmi = *blank
022100090212             OR ((I55fmi = 'DA'  or  I55fmi = 'DN'  or  I55fmi = 'NA'
022200090212                                 or  I55fmi = 'D0'  or  I55fmi = '0A')
022300090129                                and  I55mgs = *zero)
022400090129             OR ((I55fmi = 'DN'  or  I55fmi = 'N '  or  I55fmi = 'NA'
022500090209                                 or  I55fmi = 'NN'  or  I55fmi = 'NS'
022600130327                                 or  I55fmi = 'SN'
022700090212              or  I55fmi = 'D0'  or  I55fmi = '0 '  or  I55fmi = '0A'
022800090212                                 or  I55fmi = 'N0'  or  I55fmi = '0S'
022900101014                                 or  I55fmi = '10'  or  I55fmi = 'LN'
023000111111                                 or  I55fmi = 'RN'  or  I55fmi = 'AN'
023100120222                                 or  I55fmi = '3C'  or  I55fmi = 'A1')
023200090303                                and  I55rmn = *zero)
023300110627             OR   I55fmi = 'CE'
023400090303             OR   I55fmi = 'MM';
023500090303
023600090129           if  NOT %open(TITAS30C);
023700090129             open TITAS30C;
023800090129           endif;
023900090129           TASaas = I55aas;
024000090129           TASlnp = I55lnp;
024100090129           TASnrs = I55nrs;
024200090129           TASnsp = I55nsp;
024300090129           chain  %kds( KeyTitas30c : 4 )  TITAS30C;
024400090129           if  NOT  %found(TITAS30C);
024500090129             O55err = *on;
024600090129             O55msg = $Msg(03);
024700090129             exsr  sr_RoutEnd;
024800090129           endif;
024900090303
025000110627           // -?Impostazione "formato immagine" (non ricevuto)?
025100090129           if  I55fmi = *blank;
025200090212             $Tibs02r = *on;
025300090212             reset  TIBS02ds;
025400090212             tibs02ds.T02cod = 'LAC';
025500090212             tibs02ds.T02ke1 = %editc( TASksc : 'X' );
025600090212             TNTBE_RicercaControllo ( kpjba : TIBS02ds );
025700090212             if  tibs02ds.T02Err = TNTBE_Errore_NonTrovato   or
025800090212                 tibs02ds.T02Err = TNTBE_Errore_Annullato;
025900090212               O55err = *on;
026000090212               O55msg = $Msg(04);
026100090212               exsr  sr_RoutEnd;
026200090212             endif;
026300090212             dLAC = tibs02ds.T02uni;
026400090212             I55fmi = §LACfmi;
026500090129           endif;
026600090212
026700110627           // -?Impostazione giorno della spedizione (non ricevuto)?
026800090129           if  I55mgs = *zero;
026900090129             I55mgs = TASmgs;
027000090129           endif;
027100110627           // -?Impostazione "riferimento mittente numerico" (non ricevuto)?
027200101102           if  I55rmn = *zero;
027300090129             I55rmn = TASrmn;
027400090129           endif;
027500090303
027600110627           // -?Impostazione "ragione sociale"?
027700110627           //  ?(per clienti Mediolanum e Ceva/Enel)?
027800110627           If  I55fmi = 'MM'  or  I55fmi = 'CE';
027900090303             clear  wRagSoc;
028000101102             // -?Reperimento tab. "3A" = Codice Bolla?
028100090303             if  not %open(TABEL00F);
028200090303               open  TABEL00F;
028300090303             endif;
028400090303             clear  ds3A;
028500090303             TBLkey = TAScbo;
028600090303             chain ( 1 : '3A' : TBLkey )  TABEL;
028700090605             if  %found(TABEL00F);
028800090303               ds3A = TBLuni;
028900090605             endif;
029000101102             // -?Reperimento ragione sociale per Porto Assegnato?
029100090605             if  %subst(§3Atb1:1:1) = 'A';
029200101102               // -?Se non codificato aggancio TITAA?
029300090303               if  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero   or
029400090303                   %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
029500090303                 if  NOT %open(TITAA30C);
029600090303                   open TITAA30C;
029700090303                 endif;
029800110627                 chain  ( TASaas : TASlnp : TASnrs : TASnsp : 'M' )  TITAA30C;
029900090303                 if  %found(TITAA30C);
030000090303                   wRagSoc = TAArsc;
030100090303                 endif;
030200101102               // -?Se codificato aggancio anagrafico?
030300090303               else;
030400090303                 $Tibs69r = *on;
030500090303                 clear  TIBS69ds;
030600090303                 tibs69ds.I69kac = TASccm;
030700090303                 clear  ds_CNACO;
030800090303                 clear  ds_CNIND;
030900090303                 clear  ds_CNCLP;
031000090303                 clear  ds_FNCLS;
031100090303                 tibs69r ( TIBS69ds : ds_CNACO : ds_CNIND :
031200090303                                      ds_CNCLP : ds_FNCLS );
031300090303                 wRagSoc = ds_CNACO.ACOrag;
031400090303               endif;
031500101102             // -?Reperimento ragione sociale per Porto Franco?
031600090303             else;
031700090303               wRagSoc = TASrsd;
031800090303             endif;
031900110627
032000110627             // -?"Sistemazione" ragione sociale??(solo x cliente "Ceva" / "Enel")?
032100110627             if  I55fmi = 'CE'                                        and
032200110627                 %subst( wRagSoc :  1 : 19 ) = 'ENEL SERVIZI S.R.L.'  and
032300110627                 %subst( wRagSoc : 20 )     <> *blank;
032400110627               wRagSoc = %trim( %subst( wRagSoc : 21 ) );
032500110627             endif;
032600110627
032700110627             // -?"Pulizia" ragione sociale da caratteri non ammessi?
032800090306             TxtInOut = wRagSoc;
032900090306             ElencoChar = '\/:*?''"<>|';
033000090306             TipoElenco = '1';
033100090306             xchkchar ( TxtInOut :
033200090306                        ElencoChar :
033300090306                        TipoElenco :
033400090306                        CharSost :
033500090306                        UpperCase :
033600090306                        ChkNull :
033700090306                        CharNull :
033800090306                        Esito );
033900090306             select;
034000101102               // -?Nessuna modifica effettuata?
034100090306               when  Esito = *off;
034200101102               // -?Modifica effettuata?
034300090306               when  Esito = *on;
034400090306                 wRagSoc = TxtInOut;
034500101102               // -?Errore nei parametri di input?
034600090306               when  Esito = 'I';
034700090306                 clear  wRagSoc;
034800101102               // -?Errore di elaborazione?
034900090306               when  Esito = 'E';
035000090306                 clear  wRagSoc;
035100090306             endsl;
035200090303           EndIf;
035300090303
035400090129         ENDIF;
035500090129
035600101102         //?Eventuale reperimento dati mancanti (RMA) da TITA430C?
035700101102         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
035800090129         clear dTA4a;
035900090129         IF  (%subst( I55fmi : 1 : 1 ) = 'A'  or  I55fmi = 'DA'
036000101014                                              or  I55fmi = 'LN'
036100090212                                              or  I55fmi = 'NA'
036200101014                                              or  I55fmi = 'RN'
036300110627                                              or  I55fmi = '0A'
036400111111                                              or  I55fmi = '3C'
036500110627                                              or  I55fmi = 'CE')  and
036600090129              I55rma = *blank;
036700090129           if  NOT %open(TITA430C);
036800090129             open TITA430C;
036900090129           endif;
037000090129           TA4aas = I55aas;
037100090129           TA4lnp = I55lnp;
037200090129           TA4nrs = I55nrs;
037300090129           TA4nsp = I55nsp;
037400090129           TA4trc = c_Ktrc;
037500090129           chain  %kds( KeyTita430c )  TITA430C;
037600090129           if  %found(TITA430C);
037700090129             dTA4a = TA4not;
037800090129           endif;
037900090129           I55rma = %trim(§TA4aRMA);
038000090129         ENDIF;
038100090129
038200090129       ENDSR;
038300090129
038400090129       //--------------------------------------------------------------
038500090129       //?Elaborazione.                                                ?
038600090129       //--------------------------------------------------------------
038700090129       BEGSR  sr_Elab;
038800090129
038900090129         SELECT;
039000090129
039100111206           // -?"S1" = numero spedizione?
039200111206           WHEN  I55fmi = 'S1';
039300100203             O55nim = %subst( %editc( I55aas : 'X' ) :
039400111206                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
039500100203                    + %editc( I55lnp : 'X' )
039600100203                    + %editc( I55nrs : 'X' )
039700100203                    + %editc( I55nsp : 'X' );
039800090129
039900101102           // -?"N " = Riferimento Mittente Numerico?
040000090129           WHEN  I55fmi = 'N ';
040100090212             O55nim = %trim( %editc( I55rmn : 'X' ) );
040200090212
040300101102           // -?"0 " = Riferimento Mittente Numerico?
040400101102           //         ?(senza zeri NON significativi)?
040500090212           WHEN  I55fmi = '0 ';
040600090212             O55nim = %trim( %editc( I55rmn : '3' ) );
040700090209
040800101102           // -?"10" = Ultime 10 cifre del Riferimento Mittente Numerico?
040900090209           WHEN  I55fmi = '10';
041000090209             O55nim = %subst( %editc( I55rmn : 'X' ) :
041100090209                              %len( I55rmn ) - 9 );
041200101102
041300101102           // -?"AN" = Anno spedizione _ RMN _ Numero spedizione?
041400101102           WHEN  I55fmi = 'AN';
041500101102             O55nim = %editc( I55aas : 'X' ) + '_' +
041600101102                      %editc( I55rmn : 'X' ) + '_' +
041700101102                      %subst( %editc( I55aas : 'X' ) :
041800120222                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 ) +
041900101102                      %editc( I55lnp : 'X' ) +
042000101102                      %editc( I55nrs : 'X' ) +
042100101102                      %editc( I55nsp : 'X' );
042200090129
042300120222           // -?"Ax" = Riferimento Mittente Alfanumerico ( _ ... )?
042400101102           //  ?"DA" = Data spedizione _ RiferimentoMittenteAlfanumerico?
042500101102           //  ?"NA" = RMN _ RMA _ Data spedizione?
042600101102           //  ?"RN" = RMN _ RMA _ Num. spedizione?(solo x cliente L.N.)?
042700101102           //  ?"LN" = RMN _ RMA?
042800101102           //  ?"0A" = RMN (editato) _ RMA _ Data spedizione?
042900111111           //  ?"3C" = RMA _ RMN (editato)?(solo x cliente TRECCANI)?
043000111111           //  ?"CE" = RMA _ Ragione Sociale Destinatario?(solo x cliente CEVA/ENEL)?
043100090129           WHEN  %subst(I55fmi : 1 : 1) = 'A'  or  I55fmi = 'DA'
043200090212                                               or  I55fmi = 'NA'
043300101014                                               or  I55fmi = 'LN'
043400101014                                               or  I55fmi = 'RN'
043500110627                                               or  I55fmi = '0A'
043600111111                                               or  I55fmi = '3C'
043700110627                                               or  I55fmi = 'CE';
043800111111             // -?Verifica correttezza del riferimento alfanumerico?
043900111111             //  ?(solo x cliente TRECCANI)?
044000111111             if  I55fmi = '3C'  and  %subst( I55rma : 1 : 2 ) <> 'CL';
044100111111               clear  I55rma;
044200111111             endif;
044300111111
044400110627             // -?"Compattamento" interno del riferimento alfanumerico?
044500090129             if  I55rma <> *blank;
044600090129               for  xi = 1  to  %len( %trimr( I55rma ) );
044700090129                 if  %subst( I55rma : xi : 1 ) = *blank;
044800090129                   I55rma = %subst( I55rma : 1 : xi - 1 ) +
044900090129                            %subst( I55rma : xi + 1 );
045000090129                   xi -= 1;
045100090129                 endif;
045200090129               endfor;
045300090129             endif;
045400101102             // -?Eliminazione dei caratteri NON ammessi dal RMA?
045500090605             TxtInOut = I55rma;
045600090605             ElencoChar = '\/:*?''"<>|';
045700090605             TipoElenco = '1';
045800090605             xchkchar ( TxtInOut :
045900090605                        ElencoChar :
046000090605                        TipoElenco :
046100090605                        CharSost :
046200090605                        UpperCase :
046300090605                        ChkNull :
046400090605                        CharNull :
046500090605                        Esito );
046600090605             select;
046700101102               // -?Nessuna modifica effettuata?
046800090605               when  Esito = *off;
046900101102               // -?Modifica effettuata?
047000090605               when  Esito = *on;
047100090605                 I55rma = TxtInOut;
047200101102               // -?Errore nei parametri di input?
047300090605               when  Esito = 'I';
047400090605                 clear  I55rma;
047500101102               // -?Errore di elaborazione?
047600090605               when  Esito = 'E';
047700090605                 clear  I55rma;
047800090605             endsl;
047900090129             select;
048000090129
048100101102               // -?"AS" = Riferimento Mittente Alfanumerico _?
048200101102               //       ?+ Numero spedizione?
048300090129               when  I55fmi = 'AS';
048400090129                 O55nim = %trim( I55rma ) + '_'
048500090129                        + %subst( %editc( I55aas : 'X' ) :
048600101014                            %len( %triml( %editc(I55aas:'X') ) )-1 : 2 )
048700090129                        + %editc( I55lnp : 'X' )
048800090129                        + %editc( I55nrs : 'X' )
048900090129                        + %editc( I55nsp : 'X' );
049000090129
049100101102               // -?"A " = Riferimento Mittente Alfanumerico?
049200090129               when  I55fmi = 'A ';
049300090324                 if  I55rma <> *blank;
049400090324                   O55nim = %trim( I55rma );
049500090324                 else;
049600090324                   O55nim = 'NO_RMA_'
049700090324                          + %editc( I55aas : 'X' )
049800090324                          + %editc( I55lnp : 'X' )
049900090324                          + %editc( I55nrs : 'X' )
050000090324                          + %editc( I55nsp : 'X' );
050100090324                 endif;
050200120222
050300120222               // -?"A1" = Riferimento Mittente Alfanumerico _?
050400120222               //       ?+ Riferimento Mittente Numerico (editato)?
050500120222               when  I55fmi = 'A1';
050600120222                 O55nim = %trim( I55rma ) + '_'
050700120222                        + %trim( %editc( I55rmn : '3' ) );
050800090129
050900101102               // -?"AA" = Anno spedizione _?
051000101102               //       ?+ Riferimento Mittente Alfanumerico?
051100090129               when  I55fmi = 'AA';
051200090129                 O55nim = %trim( %editc( I55aas : 'X' ) )
051300090129                        + '_' + %trim( I55rma );
051400090129
051500101102               // -?"DA" = Data spedizione _?
051600101102               //       ?+ Riferimento Mittente Alfanumerico?
051700090129               when  I55fmi = 'DA';
051800090129                 O55nim = %char( I55aas * 10000 + I55mgs )
051900090129                        + '_' + %trim( I55rma );
052000101014
052100101102               // -?"LN" = Riferimento Mittente Numerico _?
052200101102               //       ?+ Riferimento Mittente Alfanumerico?
052300101014               when  I55fmi = 'LN';
052400101014                 O55nim = %trim( %editc( I55rmn : 'X' ) )
052500101014                        + '_' + %trim( I55rma );
052600090129
052700101102               // -?"NA" = Riferimento Mittente Numerico _?
052800101102               //       ?+ Riferimento Mittente Alfanumerico _?
052900101102               //       ?+ Data spedizione?
053000090129               when  I55fmi = 'NA';
053100090212                 O55nim = %trim( %editc( I55rmn : 'X' ) )
053200090129                        + '_' + %trim( I55rma )
053300090129                        + '_' + %char( I55aas * 10000 + I55mgs );
053400090212
053500101102               // -?"RN" = Riferimento Mittente Numerico _?
053600101102               //       ?+ Riferimento Mittente Alfanumerico _?
053700101102               //       ?+ Numero spedizione?
053800101102               //  ?(solo x cliente "L.N.")?
053900101102               //  ?La lunghezza massima è 40 bytes.?
054000101102               //  ?SE il riferimento mittente alfanumerico risultasse?
054100101102               //    ?più lungo del "normale", il numero spedizione al?
054200101102               //    ?completo NON ci starebbe più: AAS+LNP+NRS+NSP?
054300101102               //                                  ? 2 + 3 + 2 + 7 = 14?
054400101102               //  ?In questo caso viene riportato SOLO il numero NSP.?
054500101014               when  I55fmi = 'RN';
054600101015                 O55nim = %trim( %editc( I55rmn : 'X' ) );
054700101015                 if  I55rma = *blank;
054800101015                   O55nim = %trimr( O55nim ) + '_ _';
054900101015                 else;
055000101015                   O55nim = %trimr( O55nim ) + '_' +
055100101015                            %trim( I55rma ) + '_';
055200101015                 endif;
055300101015                 if  %len( O55nim ) - %len( %trimr( O55nim ) ) >= 14;
055400101015                   O55nim = %trimr( O55nim ) +
055500101015                            %subst( %editc( I55aas : 'X' ) :
055600101015                              %len( %editc( I55aas : 'X' ) )-1 : 2 ) +
055700101015                            %editc( I55lnp : 'X' ) +
055800101015                            %editc( I55nrs : 'X' ) +
055900101015                            %editc( I55nsp : 'X' );
056000101015                 else;
056100101015                   O55nim = %trimr( O55nim ) +
056200101015                            %editc( I55nsp : 'X' );
056300101015                 endif;
056400101014
056500101102               // -?"0A" = Riferimento Mittente Numerico (editato) _?
056600101102               //       ?+ Riferimento Mittente Alfanumerico _?
056700101102               //       ?+ Data spedizione?
056800090212               when  I55fmi = '0A';
056900090212                 O55nim = %trim( %editc( I55rmn : '3' ) )
057000090212                        + '_' + %trim( I55rma )
057100090212                        + '_' + %char( I55aas * 10000 + I55mgs );
057200111111
057300111111               // -?"3C" = Riferimento Mittente Alfanumerico _?
057400111111               //       ?+ Riferimento Mittente Numerico (editato)?
057500111111               when  I55fmi = '3C';
057600111111                 O55nim = %trim( I55rma ) + '_'
057700111111                        + %trim( %editc( I55rmn : '3' ) );
057800110627
057900110627               // -?"CE" = Riferim.Mitt.Alfanumerico _ Ragione Sociale?
058000110627               when  I55fmi = 'CE';
058100110627                 O55nim = %trim( I55rma ) + '_'
058200110627                        + %trim( %subst( wRagSoc : 1 ) );
058300090129
058400090129             endsl;
058500090129
058600101102           // -?"NS" = Riferimento Mittente Numerico _ Numero Spedizione?
058700090129           WHEN  I55fmi = 'NS';
058800090212             O55nim = %trim( %editc( I55rmn : 'X' ) ) + '_'
058900090129                    + %subst( %editc( I55aas : 'X' ) :
059000120222                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
059100090129                    + %editc( I55lnp : 'X' )
059200090129                    + %editc( I55nrs : 'X' )
059300090129                    + %editc( I55nsp : 'X' );
059400090212
059500101102           // -?"0S" = Riferim.Mitt.Numerico (editato) _ Numero Spedizione?
059600090212           WHEN  I55fmi = '0S';
059700090212             O55nim = %trim( %editc( I55rmn : '3' ) ) + '_'
059800090212                    + %subst( %editc( I55aas : 'X' ) :
059900090212                         %len( %editc(I55aas:'X') ) - 1 : 2 )
060000090212                    + %editc( I55lnp : 'X' )
060100090212                    + %editc( I55nrs : 'X' )
060200090212                    + %editc( I55nsp : 'X' );
060300090129
060400101102           // -?"NN" = Anno Spedizione _ Riferimento Mittente Numerico?
060500090129           WHEN  I55fmi = 'NN';
060600090129             O55nim = %trim( %editc( I55aas : 'X' ) ) + '_'
060700090212                    + %trim( %editc( I55rmn : 'X' ) );
060800090212
060900101102           // -?"N0" = Anno Spedizione _ Riferim.Mitt.Numerico (editato)?
061000090212           WHEN  I55fmi = 'N0';
061100090212             O55nim = %trim( %editc( I55aas : 'X' ) ) + '_'
061200090212                    + %trim( %editc( I55rmn : '3' ) );
061300090129
061400101102           // -?"DN" = Data Spedizione _ Riferimento Mittente Numerico?
061500090129           WHEN  I55fmi = 'DN';
061600090129             O55nim = %char( I55aas * 10000 + I55mgs ) + '_'
061700090212                    + %trim( %editc( I55rmn : 'X' ) );
061800090212
061900101102           // -?"D0" = Data Spedizione _ Riferim.Mitt.Numerico (editato)?
062000090212           WHEN  I55fmi = 'D0';
062100090212             O55nim = %char( I55aas * 10000 + I55mgs ) + '_'
062200090212                    + %trim( %editc( I55rmn : '3' ) );
062300090303
062400101102           // -?"MM" = Ragione Sociale _ Data Spedizione _ Num. Spedizione?
062500090303           WHEN  I55fmi = 'MM';
062600090303             O55nim = %trim( %subst( wRagSoc : 1 : 22 ) ) + '_'
062700090303                    + %subst( %editc( I55aas : 'X' ) : 3 : 2 )
062800090303                    + %editc( I55mgs : 'X' ) + '_'
062900090303                    + %editc( I55lnp : 'X' )
063000090303                    + %editc( I55nsp : 'X' );
063100130327
063200130327           // -?"SN" = numero spedizione_Riferimento Mittente Numerico (editato)?
063300130327           WHEN  I55fmi = 'SN';
063400130327             O55nim = %subst( %editc( I55aas : 'X' ) :
063500130327                         %len( %editc( I55aas : 'X' ) ) - 1 : 2 )
063600130327                    + %editc( I55lnp : 'X' )
063700130327                    + %editc( I55nrs : 'X' )
063800130327                    + %editc( I55nsp : 'X' ) + '_'
063900130327                    + %trim( %editc( I55rmn : '3' ) );
064000090129
064100090129         ENDSL;
064200090129
064300090129       ENDSR;
064400090129
064500090129       //--------------------------------------------------------------
064600090129       //?Operazioni finali.                                           ?
064700090129       //--------------------------------------------------------------
064800090129       BEGSR  sr_RoutEnd;
064900090212
065000101102         // -?Chiusura applicazioni richiamate?
065100090212         if  I55tla  <> *blank   and   $Tibs02r = *on;
065200090212           clear  tibs02ds;
065300090212           tibs02ds.T02tla = 'C';
065400090212           TNTBE_RicercaControllo ( kpjba : tibs02ds );
065500090212         endif;
065600090303         if  I55tla  <> *blank   and   $Tibs69r = *on;
065700090303           clear  tibs69ds;
065800090303           tibs69ds.I69tla = 'C';
065900090303           clear  ds_CNACO;
066000090303           clear  ds_CNIND;
066100090303           clear  ds_CNCLP;
066200090303           clear  ds_FNCLS;
066300090303           tibs69r ( TIBS69ds : ds_CNACO : ds_CNIND :
066400090303                                ds_CNCLP : ds_FNCLS );
066500090303         endif;
066600090129
066700090129         return;
066800090129
066900090129       ENDSR;
067000090129
067100090128      /end-free
067200090129
067300090129       //--------------------------------------------------------------
067400090129       //?Messaggi di errore.                                          ?
067500090129       //--------------------------------------------------------------
067600110627
067700101102** -?$Msg?-------------------------------------------------------------------*
067800090129Flag "nome immagine" errato o mancante                                          1
067900090129Numero spedizione obbligatorio                                                  2
068000090129Numero spedizione errato                                                        3
068100090129Non prevista immagine per la spedizione                                         4
