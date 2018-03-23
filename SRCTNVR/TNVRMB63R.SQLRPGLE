000100180302      //-----------------------------------------------------------------
000200180302      //?TNVRMB63R - Conteggio ORM posticipabili con Ordinante Codificato
000300180302      //-----------------------------------------------------------------
000400180302     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500180302     h dftactgrp(*no) actgrp(*caller)
000600180302     h bnddir('UBRTVNETA')
000700180302
000800180302      //---------------------------------------------------------------
000900180302      //?Dichiarazione file.
001000180302      //---------------------------------------------------------------
001100180302      // - File estensioni ORM
001200180302     fFNORE01L  if   e           k disk
001300180302     fFNORG01L  if   e           k disk
001400180302
001500180302      // - File di Stampa
001600180302     fPRTF198   o    f  198        printer oflind(*inof)
001700180302
001800180302      //---------------------------------------------------------------
001900180302      //?Definizione costanti.
002000180302      //---------------------------------------------------------------
002100180302
002200180302      //---------------------------------------------------------------
002300180302      //?Definizione schiere.
002400180302      //---------------------------------------------------------------
002500180302     d skTCO_M         s             10s 0 dim(7) inz
002600180302     d skTCO_E         s             10s 0 dim(7) inz
002700180302     d skTCO_F         s             10s 0 dim(7) inz
002800180302     d skTCO_I         s             10s 0 dim(7) inz
002900180302
003000180302      //---------------------------------------------------------------
003100180302      //?Definizione aree dati.
003200180302      //---------------------------------------------------------------
003300180302      // - Dati utente
003400180302     d §AzUte        e ds                  extname(AZUTE00F)
003500180302     d                                     dtaara
003600180302     d §DatiUte      e ds                  extname(dDatiUte)
003700180302     d                                     dtaara
003800180302
003900180302      //---------------------------------------------------------------
004000180302      //?Definizione strutture dati.
004100180302      //---------------------------------------------------------------
004200180302      // - Parametri ricevuti
004300180302     d KPJBA         e ds
004400180302
004500180302      // - Reperimento dati utente
004600180302     d TIBS34DS      e ds
004700180302
004800180302      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
004900180302     d dOREdt        e ds
005000180302
005100180302      // File ORM
005200180302     d FNORM00F      e ds                  extname(FNORM00F)
005300180302
005400180302      //---------------------------------------------------------------
005500180302      //?Definizione variabili globali.
005600180302      //---------------------------------------------------------------
005700180302      // - Flags booleani
005800180302     d EndSqlORM       s               n   inz(*off)
005900180302     d Fine            s               n   inz(*off)
006000180302
006100180302      // - Indici di schiera
006200180302     d indicex         s              4s 0 inz
006300180302
006400180302      // - Campi di comodo data
006500180302     d Data_ISO        s               d   datfmt(*iso)
006600180302
006700180302      // - Campi di comodo
006800180302     d sav_ORMdao      s                   like(ORMdao)
006900180302     d wDay_Of_Week    s              2  0 inz
007000180302     d wdatada         s              8s 0 inz(20180201)
007100180302     d wdataal         s              8s 0 inz(20180228)
007200180302     d wGiorni         s              2s 0 inz
007300180302     d Name_Of_Day     s             10    inz
007400180302     d wyoox           s                   like(ORMcor) inz(0017732000)
007500180302     d Internet        s             10s 0 inz
007600180302     d Mail            s             10s 0 inz
007700180302     d File            s             10s 0 inz
007800180302     d Telefonici      s             10s 0 inz
007900180302     d tot_E           s             10s 0 inz
008000180302     d tot_M           s             10s 0 inz
008100180302     d tot_F           s             10s 0 inz
008200180302     d tot_I           s             10s 0 inz
008300180302
008400180302      //---------------------------------------------------------------
008500180302      //?Definizione procedure esterne.
008600180302      //---------------------------------------------------------------
008700180302
008800180302      //---------------------------------------------------------------
008900180302      //?Definizione prototipi.
009000180302      //---------------------------------------------------------------
009100180302      /copy gaitrasrc/srcprotopr,TIBS34R
009200180302
009300180302      //---------------------------------------------------------------
009400180302      //?Definizione key-list.
009500180302      //---------------------------------------------------------------
009600180302
009700180302      //---------------------------------------------------------------
009800180302      //?M A I N - L I N E
009900180302      //---------------------------------------------------------------
010000180302     c     *Entry        plist
010100180302     c                   parm                    kpjba
010200180302
010300180302       //?Operazioni iniziali
010400180302       exsr RoutInz;
010500180302
010600180302       //?Elaborazione Statistica
010700180302       exsr Elabora;
010800180302
010900180302       //?Operazioni finali
011000180302       exsr RoutEnd;
011100180302
011200180302       //--------------------------------------------------------------
011300180302       //?Operazioni iniziali.
011400180302       //--------------------------------------------------------------
011500180302       BEGSR RoutInz;
011600180302
011700180302         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011800180302
011900180302
012000180302       //?Reperimento dati job
012100180302         exsr DatiJob;
012200180302
012300180302       ENDSR;
012400180302
012500180302       //--------------------------------------------------------------
012600180302       //?Reperimento Dati del job (Utente/Operativi).
012700180302       //--------------------------------------------------------------
012800180302       BEGSR DatiJob;
012900180302
013000180302         in(E) §AzUte;
013100180302         IF  NOT %error;
013200180302           in(E) §DatiUte;
013300180302         ENDIF;
013400180302         IF  %error or RSut = *blanks;
013500180302           clear TIBS34ds;
013600180302           tibs34r(tibs34ds);
013700180302           in §AzUte;
013800180302           in §DatiUte;
013900180302         ENDIF;
014000180302
014100180302       ENDSR;
014200180302
014300180302       //--------------------------------------------------------------
014400180302       //?Elabora Statistica.
014500180302       //--------------------------------------------------------------
014600180302       BEGSR Elabora;
014700180302
014800180302         EndSqlORM = *off;
014900180302
015000180302       //?Preparo la stringa SQL
015100180302         exec sql DECLARE wORM cursor for
015200180302                  SELECT * from FNORM00F
015300180302                  WHERE ORMdao between :wdatada and :wdataal and
015400180305                        ORMcra = 0 and
015500180302                        ORMcor <> 0 and ORMcor <> :wyoox and
015600180302                        ORMnar = ' ' and ORMnsr = 0 and
015700180302                        substr(ORMflo, 9, 1) = ' ' and
015800180302                        ORMtor <> 'P' and ORMtco <> 'S' and
015900180302                        ORMtco <> 'P'
016000180302                  ORDER BY ORMdao;
016100180302
016200180302       //?Apertura del cursore
016300180302         exec sql open WORM;
016400180302
016500180302         IF  sqlcode < 0;
016600180302           EndSqlORM = *on;
016700180302         ENDIF;
016800180302
016900180302         DOW  not EndSqlORM;
017000180302           exec sql
017100180302           fetch next from WORM into :FNORM00F;
017200180302           IF  sqlcod = 100 or sqlcod < 0;
017300180302             EndSqlORM = *on;
017400180302             leave;
017500180302           ENDIF;
017600180302
017700180302         //?Elaboro a dettaglio per ORM
017800180302           exsr DettaglioORM;
017900180302
018000180302         ENDDO;
018100180302
018200180302         exec sql close WORM;
018300180305
018400180305         // Totalizzo gli ultimi dati
018500180305         IF  sav_ORMdao <> *zeros;
018600180305           exsr Totali;
018700180305           clear tot_M;
018800180305           clear tot_E;
018900180305           clear tot_F;
019000180305           clear tot_I;
019100180305         ENDIF;
019200180302
019300180302         //?Stampo
019400180302         exsr Stampa;
019500180302
019600180302       ENDSR;
019700180302
019800180302       //--------------------------------------------------------------
019900180302       //?Elaboro a dettaglio ORM.
020000180302       //--------------------------------------------------------------
020100180302       BEGSR DettaglioORM;
020200180302
020300180302       //aggancio il file di estensione rcd 'DT'
020400180302         OREtrc = 'DT';
020500180302         clear dOREdt;
020600180302         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
020700180302         IF  %found(FNORE01L);
020800180302           dOREdt = OREdati;
020900180302         ENDIF;
021000180302
021100180302       //escludo dal conteggio se data pronta merce > data immissione
021200180302         IF  §OREdpm > ORMdao;
021300180302           leavesr;
021400180302         ENDIF;
021500180302
021600180302       //aggancio FNORG00F
021700180302         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
021800180305         IF  not %found(FNORG01L);
021900180302           leavesr;
022000180302         ENDIF;
022100180302
022200180302       //escludo dal conteggio se peso > 5 Kg.
022300180302         IF  ORGpkg > 5;
022400180302           leavesr;
022500180302         ENDIF;
022600180302
022700180302       //?Conto i Ritiri
022800180302         exsr Conta;
022900180302
023000180302       ENDSR;
023100180302
023200180302       //--------------------------------------------------------------
023300180302       //?Conto i ritiri.
023400180302       //--------------------------------------------------------------
023500180302       BEGSR Conta;
023600180302
023700180302       // Cambio giorno immissione
023800180305         IF  sav_ORMdao <> ORMdao;
023900180302
024000180302         // Totalizzo per giorno e tipo comunicazione
024100180302           IF  sav_ORMdao <> *zeros;
024200180302             exsr Totali;
024300180302             clear tot_M;
024400180302             clear tot_E;
024500180302             clear tot_F;
024600180302             clear tot_I;
024700180302           ENDIF;
024800180302           sav_ORMdao = ORMdao;
024900180302         ENDIF;
025000180302
025100180302       //?Conto ORM
025200180302         SELECT;
025300180302         WHEN  ORMtco = 'E';
025400180302           tot_E += 1;
025500180302         WHEN  ORMtco = 'M';
025600180302           tot_M += 1;
025700180302         WHEN  ORMtco = 'I';
025800180302           tot_I += 1;
025900180302         WHEN  ORMtco = 'F';
026000180302           tot_F += 1;
026100180302         ENDSL;
026200180302
026300180302       ENDSR;
026400180302
026500180302       //--------------------------------------------------------------
026600180302       //?Totalizzo per giorno e per tipo comunicazione.
026700180302       //--------------------------------------------------------------
026800180302       BEGSR Totali;
026900180302
027000180302       //?Cerco che giorno della settimana è il giorno di immissione
027100180302         Data_ISO = %date(sav_ORMdao:*ISO);
027200180302
027300180302         clear wDay_Of_Week;
027400180302         exec sql
027500180302         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
027600180302
027700180302         IF  tot_M > 0;
027800180302           skTCO_M(wDay_Of_Week) += tot_M;
027900180302         ENDIF;
028000180302
028100180302         IF  tot_E > 0;
028200180302           skTCO_E(wDay_Of_Week) += tot_E;
028300180302         ENDIF;
028400180302
028500180302         IF  tot_F > 0;
028600180302           skTCO_F(wDay_Of_Week) += tot_F;
028700180302         ENDIF;
028800180302
028900180302         IF  tot_I > 0;
029000180302           skTCO_I(wDay_Of_Week) += tot_I;
029100180302         ENDIF;
029200180302
029300180302       ENDSR;
029400180302
029500180302       //--------------------------------------------------------------
029600180302       //?Stampo i totali.
029700180302       //--------------------------------------------------------------
029800180302       BEGSR Stampa;
029900180302
030000180302         except Testata;
030100180302
030200180302       //Schiera di totale per giorno
030300180302         Name_Of_Day = 'Lunedì';
030400180302         Telefonici = skTCO_M(01);
030500180302         Mail = skTCO_E(01);
030600180302         File = skTCO_F(01);
030700180302         Internet = skTCO_I(01);
030800180302         except Riga;
030900180302
031000180302         Name_Of_Day = 'Martedì';
031100180302         Telefonici = skTCO_M(02);
031200180302         Mail = skTCO_E(02);
031300180302         File = skTCO_F(02);
031400180302         Internet = skTCO_I(02);
031500180302         except Riga;
031600180302
031700180302         Name_Of_Day = 'Mercoledì';
031800180302         Telefonici = skTCO_M(03);
031900180302         Mail = skTCO_E(03);
032000180302         File = skTCO_F(03);
032100180302         Internet = skTCO_I(03);
032200180302         except Riga;
032300180302
032400180302         Name_Of_Day = 'Giovedì';
032500180302         Telefonici = skTCO_M(04);
032600180302         Mail = skTCO_E(04);
032700180302         File = skTCO_F(04);
032800180302         Internet = skTCO_I(04);
032900180302         except Riga;
033000180302
033100180302         Name_Of_Day = 'Venerdì';
033200180302         Telefonici = skTCO_M(05);
033300180302         Mail = skTCO_E(05);
033400180302         File = skTCO_F(05);
033500180302         Internet = skTCO_I(05);
033600180302         except Riga;
033700180302
033800180302         Name_Of_Day = 'Sabato';
033900180302         Telefonici = skTCO_M(06);
034000180302         Mail = skTCO_E(06);
034100180302         File = skTCO_F(06);
034200180302         Internet = skTCO_I(06);
034300180302         except Riga;
034400180302
034500180302         Name_Of_Day = 'Domenica';
034600180302         Telefonici = skTCO_M(07);
034700180302         Mail = skTCO_E(07);
034800180302         File = skTCO_F(07);
034900180302         Internet = skTCO_I(07);
035000180302         except Riga;
035100180302
035200180302       ENDSR;
035300180302
035400180302       //--------------------------------------------------------------
035500180302       //?Operazioni finali.
035600180302       //--------------------------------------------------------------
035700180302       BEGSR RoutEnd;
035800180302
035900180302         *inLR = *on;
036000180302         return;
036100180302
036200180302       ENDSR;
036300180302
036400180302     oPRTF198   e            Testata        2 02
036500180302     o                                           20 'BRT S.p.A.'
036600180302     o                                           60 '**  TOTALI ORM **'
036700180302     o                       udate              110 '  /  /  '
036800180302     o                                          120 'TNVRMB63R'
036900180302     o                                          128 'Pag.'
037000180302     o                       page          Z    132
037100180302     o          e            Testata        2
037200180302     o                                           20 'ORM posticipabili:'
037300180302     o          e            Riga           1
037400180302     o                                            1 ' '
037500180302     o                       Telefonici    2   +  1
037600180302     o                       Mail          2   +  1
037700180302     o                       File          2   +  1
037800180302     o                       Internet      2   +  1
