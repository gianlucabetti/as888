000100180307      //-----------------------------------------------------------------
000200180307      //?TNVRMB65R - Conteggio ORM posticipabili suddivisi per peso
000300180307      //-----------------------------------------------------------------
000400180307     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500180307     h dftactgrp(*no) actgrp(*caller)
000600180307     h bnddir('UBRTVNETA')
000700180307
000800180307      //---------------------------------------------------------------
000900180307      //?Dichiarazione file.
001000180307      //---------------------------------------------------------------
001100180307      // - File estensioni ORM
001200180307     fFNORE01L  if   e           k disk
001300180307     fFNORG01L  if   e           k disk
001400180307
001500180307      // - File di Stampa
001600180307     fPRTF198   o    f  198        printer oflind(*inof)
001700180307
001800180307      //---------------------------------------------------------------
001900180307      //?Definizione costanti.
002000180307      //---------------------------------------------------------------
002100180307
002200180307      //---------------------------------------------------------------
002300180307      //?Definizione schiere.
002400180307      //---------------------------------------------------------------
002500180307     d skinf5          s             10s 0 dim(7) inz
002600180307     d skda5a10        s             10s 0 dim(7) inz
002700180307     d skda10a20       s             10s 0 dim(7) inz
002800180307     d skda20a50       s             10s 0 dim(7) inz
002900180307     d skda50a100      s             10s 0 dim(7) inz
003000180307
003100180307      //---------------------------------------------------------------
003200180307      //?Definizione aree dati.
003300180307      //---------------------------------------------------------------
003400180307      // - Dati utente
003500180307     d §AzUte        e ds                  extname(AZUTE00F)
003600180307     d                                     dtaara
003700180307     d §DatiUte      e ds                  extname(dDatiUte)
003800180307     d                                     dtaara
003900180307
004000180307      //---------------------------------------------------------------
004100180307      //?Definizione strutture dati.
004200180307      //---------------------------------------------------------------
004300180307      // - Parametri ricevuti
004400180307     d KPJBA         e ds
004500180307
004600180307      // - Reperimento dati utente
004700180307     d TIBS34DS      e ds
004800180307
004900180307      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
005000180307     d dOREdt        e ds
005100180307
005200180307      // File ORM
005300180307     d FNORM00F      e ds                  extname(FNORM00F)
005400180307
005500180307      //---------------------------------------------------------------
005600180307      //?Definizione variabili globali.
005700180307      //---------------------------------------------------------------
005800180307      // - Flags booleani
005900180307     d EndSqlORM       s               n   inz(*off)
006000180307     d Fine            s               n   inz(*off)
006100180307
006200180307      // - Indici di schiera
006300180307     d indicex         s              4s 0 inz
006400180307
006500180307      // - Campi di comodo data
006600180307     d Data_ISO        s               d   datfmt(*iso)
006700180307
006800180307      // - Campi di comodo
006900180307     d sav_ORMdao      s                   like(ORMdao)
007000180307     d wDay_Of_Week    s              2  0 inz
007100180307     d wdatada         s              8s 0 inz(20180201)
007200180307     d wdataal         s              8s 0 inz(20180228)
007300180307     d wGiorni         s              2s 0 inz
007400180307     d Name_Of_Day     s             10    inz
007500180307     d Inferiore5      s             10s 0 inz
007600180307     d Da5a10          s             10s 0 inz
007700180307     d da10a20         s             10s 0 inz
007800180307     d da20a50         s             10s 0 inz
007900180307     d da50a100        s             10s 0 inz
008000180307     d totinf5         s             10s 0 inz
008100180307     d totda5a10       s             10s 0 inz
008200180307     d totda10a20      s             10s 0 inz
008300180307     d totda20a50      s             10s 0 inz
008400180307     d totda50a100     s             10s 0 inz
008500180307
008600180307      //---------------------------------------------------------------
008700180307      //?Definizione procedure esterne.
008800180307      //---------------------------------------------------------------
008900180307
009000180307      //---------------------------------------------------------------
009100180307      //?Definizione prototipi.
009200180307      //---------------------------------------------------------------
009300180307      /copy gaitrasrc/srcprotopr,TIBS34R
009400180307
009500180307      //---------------------------------------------------------------
009600180307      //?Definizione key-list.
009700180307      //---------------------------------------------------------------
009800180307
009900180307      //---------------------------------------------------------------
010000180307      //?M A I N - L I N E
010100180307      //---------------------------------------------------------------
010200180307     c     *Entry        plist
010300180307     c                   parm                    kpjba
010400180307
010500180307       //?Operazioni iniziali
010600180307       exsr RoutInz;
010700180307
010800180307       //?Elaborazione Statistica
010900180307       exsr Elabora;
011000180307
011100180307       //?Operazioni finali
011200180307       exsr RoutEnd;
011300180307
011400180307       //--------------------------------------------------------------
011500180307       //?Operazioni iniziali.
011600180307       //--------------------------------------------------------------
011700180307       BEGSR RoutInz;
011800180307
011900180307         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012000180307
012100180307
012200180307       //?Reperimento dati job
012300180307         exsr DatiJob;
012400180307
012500180307       ENDSR;
012600180307
012700180307       //--------------------------------------------------------------
012800180307       //?Reperimento Dati del job (Utente/Operativi).
012900180307       //--------------------------------------------------------------
013000180307       BEGSR DatiJob;
013100180307
013200180307         in(E) §AzUte;
013300180307         IF  NOT %error;
013400180307           in(E) §DatiUte;
013500180307         ENDIF;
013600180307         IF  %error or RSut = *blanks;
013700180307           clear TIBS34ds;
013800180307           tibs34r(tibs34ds);
013900180307           in §AzUte;
014000180307           in §DatiUte;
014100180307         ENDIF;
014200180307
014300180307       ENDSR;
014400180307
014500180307       //--------------------------------------------------------------
014600180307       //?Elabora Statistica.
014700180307       //--------------------------------------------------------------
014800180307       BEGSR Elabora;
014900180307
015000180307         EndSqlORM = *off;
015100180307
015200180307       //?Preparo la stringa SQL
015300180307         exec sql DECLARE wORM cursor for
015400180307                  SELECT * from FNORM00F
015500180307                  WHERE ORMdao between :wdatada and :wdataal and
015600180307                        ORMcra = 0 and ORMcor = 0 and
015700180307                        ORMnar = ' ' and ORMnsr = 0 and
015800180307                        substr(ORMflo, 9, 1) = ' ' and
015900180307                        ORMtor <> 'P' and ORMtco <> 'P'
016000180307                  ORDER BY ORMdao;
016100180307
016200180307       //?Apertura del cursore
016300180307         exec sql open WORM;
016400180307
016500180307         IF  sqlcode < 0;
016600180307           EndSqlORM = *on;
016700180307         ENDIF;
016800180307
016900180307         DOW  not EndSqlORM;
017000180307           exec sql
017100180307           fetch next from WORM into :FNORM00F;
017200180307           IF  sqlcod = 100 or sqlcod < 0;
017300180307             EndSqlORM = *on;
017400180307             leave;
017500180307           ENDIF;
017600180307
017700180307         //?Elaboro a dettaglio per ORM
017800180307           exsr DettaglioORM;
017900180307
018000180307         ENDDO;
018100180307
018200180307         exec sql close WORM;
018300180307
018400180307         // Totalizzo gli ultimi dati
018500180307         IF  sav_ORMdao <> *zeros;
018600180307           exsr Totali;
018700180307           clear totinf5;
018800180307           clear totda5a10;
018900180307           clear totda10a20;
019000180307           clear totda20a50;
019100180307           clear totda50a100;
019200180307         ENDIF;
019300180307
019400180307         //?Stampo
019500180307         exsr Stampa;
019600180307
019700180307       ENDSR;
019800180307
019900180307       //--------------------------------------------------------------
020000180307       //?Elaboro a dettaglio ORM.
020100180307       //--------------------------------------------------------------
020200180307       BEGSR DettaglioORM;
020300180307
020400180307       //aggancio il file di estensione rcd 'DT'
020500180307         OREtrc = 'DT';
020600180307         clear dOREdt;
020700180307         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
020800180307         IF  %found(FNORE01L);
020900180307           dOREdt = OREdati;
021000180307         ENDIF;
021100180307
021200180307       //escludo dal conteggio se data pronta merce > data immissione
021300180307         IF  §OREdpm > ORMdao;
021400180307           leavesr;
021500180307         ENDIF;
021600180307
021700180307       //aggancio FNORG00F
021800180307         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
021900180307         IF  not %found(FNORG01L);
022000180307           leavesr;
022100180307         ENDIF;
022200180307
022300180307       //?Conto i Ritiri
022400180307         exsr Conta;
022500180307
022600180307       ENDSR;
022700180307
022800180307       //--------------------------------------------------------------
022900180307       //?Conto i ritiri.
023000180307       //--------------------------------------------------------------
023100180307       BEGSR Conta;
023200180307
023300180307       // Cambio giorno immissione
023400180307         IF  sav_ORMdao <> ORMdao;
023500180307
023600180307         // Totalizzo per giorno e peso
023700180307           IF  sav_ORMdao <> *zeros;
023800180307             exsr Totali;
023900180307             clear totinf5;
024000180307             clear totda5a10;
024100180307             clear totda10a20;
024200180307             clear totda20a50;
024300180307             clear totda50a100;
024400180307           ENDIF;
024500180307           sav_ORMdao = ORMdao;
024600180307         ENDIF;
024700180307
024800180307       //?Conto ORM
024900180307         SELECT;
025000180307         WHEN  ORGpkg <= 5;
025100180307           totinf5 += 1;
025200180307         WHEN  ORGpkg > 5 and ORGpkg <= 10;
025300180307           totda5a10 += 1;
025400180307         WHEN  ORGpkg > 10 and ORGpkg <= 20;
025500180307           totda10a20 += 1;
025600180307         WHEN  ORGpkg > 20 and ORGpkg <= 50;
025700180307           totda20a50 += 1;
025800180307         WHEN  ORGpkg > 50 and ORGpkg <= 100;
025900180307           totda50a100 += 1;
026000180307         ENDSL;
026100180307
026200180307       ENDSR;
026300180307
026400180307       //--------------------------------------------------------------
026500180307       //?Totalizzo per giorno e per tipo comunicazione.
026600180307       //--------------------------------------------------------------
026700180307       BEGSR Totali;
026800180307
026900180307       //?Cerco che giorno della settimana è il giorno di immissione
027000180307         Data_ISO = %date(sav_ORMdao:*ISO);
027100180307
027200180307         clear wDay_Of_Week;
027300180307         exec sql
027400180307         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
027500180307
027600180307         IF  totinf5 > 0;
027700180307           skinf5(wDay_Of_Week) += totinf5;
027800180307         ENDIF;
027900180307
028000180307         IF  totda5a10 > 0;
028100180307           skda5a10(wDay_Of_Week) += totda5a10;
028200180307         ENDIF;
028300180307
028400180307         IF  totda10a20 > 0;
028500180307           skda10a20(wDay_Of_Week) += totda10a20;
028600180307         ENDIF;
028700180307
028800180307         IF  totda20a50 > 0;
028900180307           skda20a50(wDay_Of_Week) += totda20a50;
029000180307         ENDIF;
029100180307
029200180307         IF  totda50a100 > 0;
029300180307           skda50a100(wDay_Of_Week) += totda50a100;
029400180307         ENDIF;
029500180307
029600180307       ENDSR;
029700180307
029800180307       //--------------------------------------------------------------
029900180307       //?Stampo i totali.
030000180307       //--------------------------------------------------------------
030100180307       BEGSR Stampa;
030200180307
030300180307         except Testata;
030400180307
030500180307       //Schiera di totale per giorno
030600180307         Name_Of_Day = 'Lunedì';
030700180307         inferiore5 = skinf5(01);
030800180307         da5a10 = skda5a10(01);
030900180307         da10a20 = skda10a20(01);
031000180307         da20a50 = skda20a50(01);
031100180307         da50a100 = skda50a100(01);
031200180307         except Riga;
031300180307
031400180307         Name_Of_Day = 'Martedì';
031500180307         inferiore5 = skinf5(02);
031600180307         da5a10 = skda5a10(02);
031700180307         da10a20 = skda10a20(02);
031800180307         da20a50 = skda20a50(02);
031900180307         da50a100 = skda50a100(02);
032000180307         except Riga;
032100180307
032200180307         Name_Of_Day = 'Mercoledì';
032300180307         inferiore5 = skinf5(03);
032400180307         da5a10 = skda5a10(03);
032500180307         da10a20 = skda10a20(03);
032600180307         da20a50 = skda20a50(03);
032700180307         da50a100 = skda50a100(03);
032800180307         except Riga;
032900180307
033000180307         Name_Of_Day = 'Giovedì';
033100180307         inferiore5 = skinf5(04);
033200180307         da5a10 = skda5a10(04);
033300180307         da10a20 = skda10a20(04);
033400180307         da20a50 = skda20a50(04);
033500180307         da50a100 = skda50a100(04);
033600180307         except Riga;
033700180307
033800180307         Name_Of_Day = 'Venerdì';
033900180307         inferiore5 = skinf5(05);
034000180307         da5a10 = skda5a10(05);
034100180307         da10a20 = skda10a20(05);
034200180307         da20a50 = skda20a50(05);
034300180307         da50a100 = skda50a100(05);
034400180307         except Riga;
034500180307
034600180307         Name_Of_Day = 'Sabato';
034700180307         inferiore5 = skinf5(06);
034800180307         da5a10 = skda5a10(06);
034900180307         da10a20 = skda10a20(06);
035000180307         da20a50 = skda20a50(06);
035100180307         da50a100 = skda50a100(06);
035200180307         except Riga;
035300180307
035400180307         Name_Of_Day = 'Domenica';
035500180307         inferiore5 = skinf5(07);
035600180307         da5a10 = skda5a10(07);
035700180307         da10a20 = skda10a20(07);
035800180307         da20a50 = skda20a50(07);
035900180307         da50a100 = skda50a100(07);
036000180307         except Riga;
036100180307
036200180307       ENDSR;
036300180307
036400180307       //--------------------------------------------------------------
036500180307       //?Operazioni finali.
036600180307       //--------------------------------------------------------------
036700180307       BEGSR RoutEnd;
036800180307
036900180307         *inLR = *on;
037000180307         return;
037100180307
037200180307       ENDSR;
037300180307
037400180307     oPRTF198   e            Testata        2 02
037500180307     o                                           20 'BRT S.p.A.'
037600180307     o                                           60 '**  TOTALI ORM **'
037700180307     o                       udate              110 '  /  /  '
037800180307     o                                          120 'TNVRMB63R'
037900180307     o                                          128 'Pag.'
038000180307     o                       page          Z    132
038100180307     o          e            Testata        2
038200180307     o                                           20 'ORM posticipabili:'
038300180307     o          e            Testata        2
038400180307     o                                           25 'Fino a 5 Kg.'
038500180307     o                                           45 'Da 5 a 10 Kg.'
038600180307     o                                           65 'Da 10 a 20 Kg.'
038700180307     o                                           85 'Da 20 a 50 Kg.'
038800180307     o                                          105 'Da 50 a 100 Kg.'
038900180307     o          e            Riga           1
039000180307     o                       Name_Of_Day         10
039100180307     o                       inferiore5    2     25
039200180307     o                       da5a10        2     45
039300180307     o                       da10a20       2     65
039400180307     o                       da20a50       2     85
039500180307     o                       da50a100      2    105
