000100180308      //-----------------------------------------------------------------
000200180308      //?TNVRMB65R - Conteggio ORM prepagati suddivisi per peso
000300180308      //-----------------------------------------------------------------
000400180308     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500180308     h dftactgrp(*no) actgrp(*caller)
000600180308     h bnddir('UBRTVNETA')
000700180308
000800180308      //---------------------------------------------------------------
000900180308      //?Dichiarazione file.
001000180308      //---------------------------------------------------------------
001100180308      // - File estensioni ORM
001200180308     fFNORE01L  if   e           k disk
001300180308     fFNORG01L  if   e           k disk
001400180308
001500180308      // - File di Stampa
001600180308     fPRTF198   o    f  198        printer oflind(*inof)
001700180308
001800180308      //---------------------------------------------------------------
001900180308      //?Definizione costanti.
002000180308      //---------------------------------------------------------------
002100180308
002200180308      //---------------------------------------------------------------
002300180308      //?Definizione schiere.
002400180308      //---------------------------------------------------------------
002500180308     d skinf5          s             10s 0 dim(7) inz
002600180308     d skda5a10        s             10s 0 dim(7) inz
002700180308     d skda10a20       s             10s 0 dim(7) inz
002800180308     d skda20a50       s             10s 0 dim(7) inz
002900180308     d skda50a100      s             10s 0 dim(7) inz
003000180308     d skoltre100      s             10s 0 dim(7) inz
003100180308
003200180308      //---------------------------------------------------------------
003300180308      //?Definizione aree dati.
003400180308      //---------------------------------------------------------------
003500180308      // - Dati utente
003600180308     d §AzUte        e ds                  extname(AZUTE00F)
003700180308     d                                     dtaara
003800180308     d §DatiUte      e ds                  extname(dDatiUte)
003900180308     d                                     dtaara
004000180308
004100180308      //---------------------------------------------------------------
004200180308      //?Definizione strutture dati.
004300180308      //---------------------------------------------------------------
004400180308      // - Parametri ricevuti
004500180308     d KPJBA         e ds
004600180308
004700180308      // - Reperimento dati utente
004800180308     d TIBS34DS      e ds
004900180308
005000180308      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
005100180308     d dOREdt        e ds
005200180308
005300180308      // File ORM
005400180308     d FNORM00F      e ds                  extname(FNORM00F)
005500180308
005600180308      //---------------------------------------------------------------
005700180308      //?Definizione variabili globali.
005800180308      //---------------------------------------------------------------
005900180308      // - Flags booleani
006000180308     d EndSqlORM       s               n   inz(*off)
006100180308     d Fine            s               n   inz(*off)
006200180308
006300180308      // - Indici di schiera
006400180308     d indicex         s              4s 0 inz
006500180308
006600180308      // - Campi di comodo data
006700180308     d Data_ISO        s               d   datfmt(*iso)
006800180308
006900180308      // - Campi di comodo
007000180308     d sav_ORMdao      s                   like(ORMdao)
007100180308     d wDay_Of_Week    s              2  0 inz
007200180308     d wdatada         s              8s 0 inz(20180201)
007300180308     d wdataal         s              8s 0 inz(20180228)
007400180308     d wGiorni         s              2s 0 inz
007500180308     d Name_Of_Day     s             10    inz
007600180308     d Inferiore5      s             10s 0 inz
007700180308     d Da5a10          s             10s 0 inz
007800180308     d da10a20         s             10s 0 inz
007900180308     d da20a50         s             10s 0 inz
008000180308     d da50a100        s             10s 0 inz
008100180308     d oltre100        s             10s 0 inz
008200180308     d totinf5         s             10s 0 inz
008300180308     d totda5a10       s             10s 0 inz
008400180308     d totda10a20      s             10s 0 inz
008500180308     d totda20a50      s             10s 0 inz
008600180308     d totda50a100     s             10s 0 inz
008700180308     d totoltre100     s             10s 0 inz
008800180308
008900180308      //---------------------------------------------------------------
009000180308      //?Definizione procedure esterne.
009100180308      //---------------------------------------------------------------
009200180308
009300180308      //---------------------------------------------------------------
009400180308      //?Definizione prototipi.
009500180308      //---------------------------------------------------------------
009600180308      /copy gaitrasrc/srcprotopr,TIBS34R
009700180308
009800180308      //---------------------------------------------------------------
009900180308      //?Definizione key-list.
010000180308      //---------------------------------------------------------------
010100180308
010200180308      //---------------------------------------------------------------
010300180308      //?M A I N - L I N E
010400180308      //---------------------------------------------------------------
010500180308     c     *Entry        plist
010600180308     c                   parm                    kpjba
010700180308
010800180308       //?Operazioni iniziali
010900180308       exsr RoutInz;
011000180308
011100180308       //?Elaborazione Statistica
011200180308       exsr Elabora;
011300180308
011400180308       //?Operazioni finali
011500180308       exsr RoutEnd;
011600180308
011700180308       //--------------------------------------------------------------
011800180308       //?Operazioni iniziali.
011900180308       //--------------------------------------------------------------
012000180308       BEGSR RoutInz;
012100180308
012200180308         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012300180308
012400180308       //?Reperimento dati job
012500180308         exsr DatiJob;
012600180308
012700180308       ENDSR;
012800180308
012900180308       //--------------------------------------------------------------
013000180308       //?Reperimento Dati del job (Utente/Operativi).
013100180308       //--------------------------------------------------------------
013200180308       BEGSR DatiJob;
013300180308
013400180308         in(E) §AzUte;
013500180308         IF  NOT %error;
013600180308           in(E) §DatiUte;
013700180308         ENDIF;
013800180308         IF  %error or RSut = *blanks;
013900180308           clear TIBS34ds;
014000180308           tibs34r(tibs34ds);
014100180308           in §AzUte;
014200180308           in §DatiUte;
014300180308         ENDIF;
014400180308
014500180308       ENDSR;
014600180308
014700180308       //--------------------------------------------------------------
014800180308       //?Elabora Statistica.
014900180308       //--------------------------------------------------------------
015000180308       BEGSR Elabora;
015100180308
015200180308         EndSqlORM = *off;
015300180308
015400180308       //?Preparo la stringa SQL
015500180308         exec sql DECLARE wORM cursor for
015600180308                  SELECT * from FNORM00F
015700180308                  WHERE ORMdao between :wdatada and :wdataal and
015800180308                        ORMcra = 0 and ORMcor = 0 and
015900180308                        ORMnar = ' ' and ORMnsr = 0 and
016000180308                        substr(ORMflo, 9, 1) = ' ' and
016100180308                        (ORMtor = 'P' or ORMtco = 'P')
016200180308                  ORDER BY ORMdao;
016300180308
016400180308       //?Apertura del cursore
016500180308         exec sql open WORM;
016600180308
016700180308         IF  sqlcode < 0;
016800180308           EndSqlORM = *on;
016900180308         ENDIF;
017000180308
017100180308         DOW  not EndSqlORM;
017200180308           exec sql
017300180308           fetch next from WORM into :FNORM00F;
017400180308           IF  sqlcod = 100 or sqlcod < 0;
017500180308             EndSqlORM = *on;
017600180308             leave;
017700180308           ENDIF;
017800180308
017900180308         //?Elaboro a dettaglio per ORM
018000180308           exsr DettaglioORM;
018100180308
018200180308         ENDDO;
018300180308
018400180308         exec sql close WORM;
018500180308
018600180308         // Totalizzo gli ultimi dati
018700180308         IF  sav_ORMdao <> *zeros;
018800180308           exsr Totali;
018900180308           clear totinf5;
019000180308           clear totda5a10;
019100180308           clear totda10a20;
019200180308           clear totda20a50;
019300180308           clear totda50a100;
019400180308           clear totoltre100;
019500180308         ENDIF;
019600180308
019700180308         //?Stampo
019800180308         exsr Stampa;
019900180308
020000180308       ENDSR;
020100180308
020200180308       //--------------------------------------------------------------
020300180308       //?Elaboro a dettaglio ORM.
020400180308       //--------------------------------------------------------------
020500180308       BEGSR DettaglioORM;
020600180308
020700180308       //aggancio il file di estensione rcd 'DT'
020800180308         OREtrc = 'DT';
020900180308         clear dOREdt;
021000180308         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
021100180308         IF  %found(FNORE01L);
021200180308           dOREdt = OREdati;
021300180308         ENDIF;
021400180308
021500180308       //escludo dal conteggio se data pronta merce > data immissione
021600180308         IF  §OREdpm > ORMdao;
021700180308           leavesr;
021800180308         ENDIF;
021900180308
022000180308       //aggancio FNORG00F
022100180308         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
022200180308         IF  not %found(FNORG01L);
022300180308           leavesr;
022400180308         ENDIF;
022500180308
022600180308       //?Conto i Ritiri
022700180308         exsr Conta;
022800180308
022900180308       ENDSR;
023000180308
023100180308       //--------------------------------------------------------------
023200180308       //?Conto i ritiri.
023300180308       //--------------------------------------------------------------
023400180308       BEGSR Conta;
023500180308
023600180308       // Cambio giorno immissione
023700180308         IF  sav_ORMdao <> ORMdao;
023800180308
023900180308         // Totalizzo per giorno e peso
024000180308           IF  sav_ORMdao <> *zeros;
024100180308             exsr Totali;
024200180308             clear totinf5;
024300180308             clear totda5a10;
024400180308             clear totda10a20;
024500180308             clear totda20a50;
024600180308             clear totda50a100;
024700180308             clear totoltre100;
024800180308           ENDIF;
024900180308           sav_ORMdao = ORMdao;
025000180308         ENDIF;
025100180308
025200180308       //?Conto ORM
025300180308         SELECT;
025400180308         WHEN  ORGpkg <= 5;
025500180308           totinf5 += 1;
025600180308         WHEN  ORGpkg > 5 and ORGpkg <= 10;
025700180308           totda5a10 += 1;
025800180308         WHEN  ORGpkg > 10 and ORGpkg <= 20;
025900180308           totda10a20 += 1;
026000180308         WHEN  ORGpkg > 20 and ORGpkg <= 50;
026100180308           totda20a50 += 1;
026200180308         WHEN  ORGpkg > 50 and ORGpkg <= 100;
026300180308           totda50a100 += 1;
026400180308         WHEN  ORGpkg > 100;
026500180308           totoltre100 += 1;
026600180308         ENDSL;
026700180308
026800180308       ENDSR;
026900180308
027000180308       //--------------------------------------------------------------
027100180308       //?Totalizzo per giorno e per tipo comunicazione.
027200180308       //--------------------------------------------------------------
027300180308       BEGSR Totali;
027400180308
027500180308       //?Cerco che giorno della settimana è il giorno di immissione
027600180308         Data_ISO = %date(sav_ORMdao:*ISO);
027700180308
027800180308         clear wDay_Of_Week;
027900180308         exec sql
028000180308         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
028100180308
028200180308         IF  totinf5 > 0;
028300180308           skinf5(wDay_Of_Week) += totinf5;
028400180308         ENDIF;
028500180308
028600180308         IF  totda5a10 > 0;
028700180308           skda5a10(wDay_Of_Week) += totda5a10;
028800180308         ENDIF;
028900180308
029000180308         IF  totda10a20 > 0;
029100180308           skda10a20(wDay_Of_Week) += totda10a20;
029200180308         ENDIF;
029300180308
029400180308         IF  totda20a50 > 0;
029500180308           skda20a50(wDay_Of_Week) += totda20a50;
029600180308         ENDIF;
029700180308
029800180308         IF  totda50a100 > 0;
029900180308           skda50a100(wDay_Of_Week) += totda50a100;
030000180308         ENDIF;
030100180308
030200180308         IF  totoltre100 > 0;
030300180308           skoltre100(wDay_Of_Week) += totoltre100;
030400180308         ENDIF;
030500180308
030600180308       ENDSR;
030700180308
030800180308       //--------------------------------------------------------------
030900180308       //?Stampo i totali.
031000180308       //--------------------------------------------------------------
031100180308       BEGSR Stampa;
031200180308
031300180308         except Testata;
031400180308
031500180308       //Schiera di totale per giorno
031600180308         Name_Of_Day = 'Lunedì';
031700180308         inferiore5 = skinf5(01);
031800180308         da5a10 = skda5a10(01);
031900180308         da10a20 = skda10a20(01);
032000180308         da20a50 = skda20a50(01);
032100180308         da50a100 = skda50a100(01);
032200180308         oltre100 = skoltre100(01);
032300180308         except Riga;
032400180308
032500180308         Name_Of_Day = 'Martedì';
032600180308         inferiore5 = skinf5(02);
032700180308         da5a10 = skda5a10(02);
032800180308         da10a20 = skda10a20(02);
032900180308         da20a50 = skda20a50(02);
033000180308         da50a100 = skda50a100(02);
033100180308         oltre100 = skoltre100(02);
033200180308         except Riga;
033300180308
033400180308         Name_Of_Day = 'Mercoledì';
033500180308         inferiore5 = skinf5(03);
033600180308         da5a10 = skda5a10(03);
033700180308         da10a20 = skda10a20(03);
033800180308         da20a50 = skda20a50(03);
033900180308         da50a100 = skda50a100(03);
034000180308         oltre100 = skoltre100(03);
034100180308         except Riga;
034200180308
034300180308         Name_Of_Day = 'Giovedì';
034400180308         inferiore5 = skinf5(04);
034500180308         da5a10 = skda5a10(04);
034600180308         da10a20 = skda10a20(04);
034700180308         da20a50 = skda20a50(04);
034800180308         da50a100 = skda50a100(04);
034900180308         oltre100 = skoltre100(04);
035000180308         except Riga;
035100180308
035200180308         Name_Of_Day = 'Venerdì';
035300180308         inferiore5 = skinf5(05);
035400180308         da5a10 = skda5a10(05);
035500180308         da10a20 = skda10a20(05);
035600180308         da20a50 = skda20a50(05);
035700180308         da50a100 = skda50a100(05);
035800180308         oltre100 = skoltre100(05);
035900180308         except Riga;
036000180308
036100180308         Name_Of_Day = 'Sabato';
036200180308         inferiore5 = skinf5(06);
036300180308         da5a10 = skda5a10(06);
036400180308         da10a20 = skda10a20(06);
036500180308         da20a50 = skda20a50(06);
036600180308         da50a100 = skda50a100(06);
036700180308         oltre100 = skoltre100(06);
036800180308         except Riga;
036900180308
037000180308         Name_Of_Day = 'Domenica';
037100180308         inferiore5 = skinf5(07);
037200180308         da5a10 = skda5a10(07);
037300180308         da10a20 = skda10a20(07);
037400180308         da20a50 = skda20a50(07);
037500180308         da50a100 = skda50a100(07);
037600180308         oltre100 = skoltre100(07);
037700180308         except Riga;
037800180308
037900180308       ENDSR;
038000180308
038100180308       //--------------------------------------------------------------
038200180308       //?Operazioni finali.
038300180308       //--------------------------------------------------------------
038400180308       BEGSR RoutEnd;
038500180308
038600180308         *inLR = *on;
038700180308         return;
038800180308
038900180308       ENDSR;
039000180308
039100180308     oPRTF198   e            Testata        2 02
039200180308     o                                           20 'BRT S.p.A.'
039300180308     o                                           60 '**  TOTALI ORM **'
039400180308     o                       udate              110 '  /  /  '
039500180308     o                                          120 'TNVRMB66R'
039600180308     o                                          128 'Pag.'
039700180308     o                       page          Z    132
039800180308     o          e            Testata        2
039900180308     o                                           20 'ORM prepagati:'
040000180308     o          e            Testata        2
040100180308     o                                           25 'Fino a 5 Kg.'
040200180308     o                                           45 'Da 5 a 10 Kg.'
040300180308     o                                           65 'Da 10 a 20 Kg.'
040400180308     o                                           85 'Da 20 a 50 Kg.'
040500180308     o                                          105 'Da 50 a 100 Kg.'
040600180308     o                                          125 'Oltre 100 Kg.'
040700180308     o          e            Riga           1
040800180308     o                       Name_Of_Day         10
040900180308     o                       inferiore5    2     25
041000180308     o                       da5a10        2     45
041100180308     o                       da10a20       2     65
041200180308     o                       da20a50       2     85
041300180308     o                       da50a100      2    105
041400180308     o                       oltre100      2    125
