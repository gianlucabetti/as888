000100180307      //-----------------------------------------------------------------
000200180307      //?TNVRMB63R - Conteggio ORM posticipabili suddivisi per peso
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
002600180307     d skda5a100       s             10s 0 dim(7) inz
002700180307     d skoltre100      s             10s 0 dim(7) inz
002800180307
002900180307      //---------------------------------------------------------------
003000180307      //?Definizione aree dati.
003100180307      //---------------------------------------------------------------
003200180307      // - Dati utente
003300180307     d §AzUte        e ds                  extname(AZUTE00F)
003400180307     d                                     dtaara
003500180307     d §DatiUte      e ds                  extname(dDatiUte)
003600180307     d                                     dtaara
003700180307
003800180307      //---------------------------------------------------------------
003900180307      //?Definizione strutture dati.
004000180307      //---------------------------------------------------------------
004100180307      // - Parametri ricevuti
004200180307     d KPJBA         e ds
004300180307
004400180307      // - Reperimento dati utente
004500180307     d TIBS34DS      e ds
004600180307
004700180307      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
004800180307     d dOREdt        e ds
004900180307
005000180307      // File ORM
005100180307     d FNORM00F      e ds                  extname(FNORM00F)
005200180307
005300180307      //---------------------------------------------------------------
005400180307      //?Definizione variabili globali.
005500180307      //---------------------------------------------------------------
005600180307      // - Flags booleani
005700180307     d EndSqlORM       s               n   inz(*off)
005800180307     d Fine            s               n   inz(*off)
005900180307
006000180307      // - Indici di schiera
006100180307     d indicex         s              4s 0 inz
006200180307
006300180307      // - Campi di comodo data
006400180307     d Data_ISO        s               d   datfmt(*iso)
006500180307
006600180307      // - Campi di comodo
006700180307     d sav_ORMdao      s                   like(ORMdao)
006800180307     d wDay_Of_Week    s              2  0 inz
006900180307     d wdatada         s              8s 0 inz(20180201)
007000180307     d wdataal         s              8s 0 inz(20180228)
007100180307     d wGiorni         s              2s 0 inz
007200180307     d Name_Of_Day     s             10    inz
007300180307     d Inferiore5      s             10s 0 inz
007400180307     d Da5a100         s             10s 0 inz
007500180307     d Oltre5          s             10s 0 inz
007600180307     d Oltre100        s             10s 0 inz
007700180307     d totinf5         s             10s 0 inz
007800180307     d totda5a100      s             10s 0 inz
007900180307     d totoltre100     s             10s 0 inz
008000180307
008100180307      //---------------------------------------------------------------
008200180307      //?Definizione procedure esterne.
008300180307      //---------------------------------------------------------------
008400180307
008500180307      //---------------------------------------------------------------
008600180307      //?Definizione prototipi.
008700180307      //---------------------------------------------------------------
008800180307      /copy gaitrasrc/srcprotopr,TIBS34R
008900180307
009000180307      //---------------------------------------------------------------
009100180307      //?Definizione key-list.
009200180307      //---------------------------------------------------------------
009300180307
009400180307      //---------------------------------------------------------------
009500180307      //?M A I N - L I N E
009600180307      //---------------------------------------------------------------
009700180307     c     *Entry        plist
009800180307     c                   parm                    kpjba
009900180307
010000180307       //?Operazioni iniziali
010100180307       exsr RoutInz;
010200180307
010300180307       //?Elaborazione Statistica
010400180307       exsr Elabora;
010500180307
010600180307       //?Operazioni finali
010700180307       exsr RoutEnd;
010800180307
010900180307       //--------------------------------------------------------------
011000180307       //?Operazioni iniziali.
011100180307       //--------------------------------------------------------------
011200180307       BEGSR RoutInz;
011300180307
011400180307         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011500180307
011600180307
011700180307       //?Reperimento dati job
011800180307         exsr DatiJob;
011900180307
012000180307       ENDSR;
012100180307
012200180307       //--------------------------------------------------------------
012300180307       //?Reperimento Dati del job (Utente/Operativi).
012400180307       //--------------------------------------------------------------
012500180307       BEGSR DatiJob;
012600180307
012700180307         in(E) §AzUte;
012800180307         IF  NOT %error;
012900180307           in(E) §DatiUte;
013000180307         ENDIF;
013100180307         IF  %error or RSut = *blanks;
013200180307           clear TIBS34ds;
013300180307           tibs34r(tibs34ds);
013400180307           in §AzUte;
013500180307           in §DatiUte;
013600180307         ENDIF;
013700180307
013800180307       ENDSR;
013900180307
014000180307       //--------------------------------------------------------------
014100180307       //?Elabora Statistica.
014200180307       //--------------------------------------------------------------
014300180307       BEGSR Elabora;
014400180307
014500180307         EndSqlORM = *off;
014600180307
014700180307       //?Preparo la stringa SQL
014800180307         exec sql DECLARE wORM cursor for
014900180307                  SELECT * from FNORM00F
015000180307                  WHERE ORMdao between :wdatada and :wdataal and
015100180307                        ORMcra = 0 and ORMcor = 0 and
015200180307                        ORMnar = ' ' and ORMnsr = 0 and
015300180307                        substr(ORMflo, 9, 1) = ' ' and
015400180307                        ORMtor <> 'P' and ORMtco <> 'P'
015500180307                  ORDER BY ORMdao;
015600180307
015700180307       //?Apertura del cursore
015800180307         exec sql open WORM;
015900180307
016000180307         IF  sqlcode < 0;
016100180307           EndSqlORM = *on;
016200180307         ENDIF;
016300180307
016400180307         DOW  not EndSqlORM;
016500180307           exec sql
016600180307           fetch next from WORM into :FNORM00F;
016700180307           IF  sqlcod = 100 or sqlcod < 0;
016800180307             EndSqlORM = *on;
016900180307             leave;
017000180307           ENDIF;
017100180307
017200180307         //?Elaboro a dettaglio per ORM
017300180307           exsr DettaglioORM;
017400180307
017500180307         ENDDO;
017600180307
017700180307         exec sql close WORM;
017800180307
017900180307         // Totalizzo gli ultimi dati
018000180307         IF  sav_ORMdao <> *zeros;
018100180307           exsr Totali;
018200180307           clear totinf5;
018300180307           clear totda5a100;
018400180307           clear totoltre100;
018500180307         ENDIF;
018600180307
018700180307         //?Stampo
018800180307         exsr Stampa;
018900180307
019000180307       ENDSR;
019100180307
019200180307       //--------------------------------------------------------------
019300180307       //?Elaboro a dettaglio ORM.
019400180307       //--------------------------------------------------------------
019500180307       BEGSR DettaglioORM;
019600180307
019700180307       //aggancio il file di estensione rcd 'DT'
019800180307         OREtrc = 'DT';
019900180307         clear dOREdt;
020000180307         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
020100180307         IF  %found(FNORE01L);
020200180307           dOREdt = OREdati;
020300180307         ENDIF;
020400180307
020500180307       //escludo dal conteggio se data pronta merce > data immissione
020600180307         IF  §OREdpm > ORMdao;
020700180307           leavesr;
020800180307         ENDIF;
020900180307
021000180307       //aggancio FNORG00F
021100180307         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
021200180307         IF  not %found(FNORG01L);
021300180307           leavesr;
021400180307         ENDIF;
021500180307
021600180307       //?Conto i Ritiri
021700180307         exsr Conta;
021800180307
021900180307       ENDSR;
022000180307
022100180307       //--------------------------------------------------------------
022200180307       //?Conto i ritiri.
022300180307       //--------------------------------------------------------------
022400180307       BEGSR Conta;
022500180307
022600180307       // Cambio giorno immissione
022700180307         IF  sav_ORMdao <> ORMdao;
022800180307
022900180307         // Totalizzo per giorno e peso
023000180307           IF  sav_ORMdao <> *zeros;
023100180307             exsr Totali;
023200180307             clear totinf5;
023300180307             clear totda5a100;
023400180307             clear totoltre100;
023500180307           ENDIF;
023600180307           sav_ORMdao = ORMdao;
023700180307         ENDIF;
023800180307
023900180307       //?Conto ORM
024000180307         SELECT;
024100180307         WHEN  ORGpkg <= 5;
024200180307           totinf5 += 1;
024300180307         WHEN  ORGpkg > 5 and ORGpkg <= 100;
024400180307           totda5a100 += 1;
024500180307         WHEN  ORGpkg > 100;
024600180307           totoltre100 += 1;
024700180307         ENDSL;
024800180307
024900180307       ENDSR;
025000180307
025100180307       //--------------------------------------------------------------
025200180307       //?Totalizzo per giorno e per tipo comunicazione.
025300180307       //--------------------------------------------------------------
025400180307       BEGSR Totali;
025500180307
025600180307       //?Cerco che giorno della settimana è il giorno di immissione
025700180307         Data_ISO = %date(sav_ORMdao:*ISO);
025800180307
025900180307         clear wDay_Of_Week;
026000180307         exec sql
026100180307         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
026200180307
026300180307         IF  totinf5 > 0;
026400180307           skinf5(wDay_Of_Week) += totinf5;
026500180307         ENDIF;
026600180307
026700180307         IF  totda5a100 > 0;
026800180307           skda5a100(wDay_Of_Week) += totda5a100;
026900180307         ENDIF;
027000180307
027100180307         IF  totoltre100 > 0;
027200180307           skoltre100(wDay_Of_Week) += totoltre100;
027300180307         ENDIF;
027400180307
027500180307       ENDSR;
027600180307
027700180307       //--------------------------------------------------------------
027800180307       //?Stampo i totali.
027900180307       //--------------------------------------------------------------
028000180307       BEGSR Stampa;
028100180307
028200180307         except Testata;
028300180307
028400180307       //Schiera di totale per giorno
028500180307         Name_Of_Day = 'Lunedì';
028600180307         inferiore5 = skinf5(01);
028700180307         da5a100 = skda5a100(01);
028800180307         oltre100 = skoltre100(01);
028900180307         oltre5 = da5a100 + oltre100;
029000180307         except Riga;
029100180307
029200180307         Name_Of_Day = 'Martedì';
029300180307         inferiore5 = skinf5(02);
029400180307         da5a100 = skda5a100(02);
029500180307         oltre100 = skoltre100(02);
029600180307         oltre5 = da5a100 + oltre100;
029700180307         except Riga;
029800180307
029900180307         Name_Of_Day = 'Mercoledì';
030000180307         inferiore5 = skinf5(03);
030100180307         da5a100 = skda5a100(03);
030200180307         oltre100 = skoltre100(03);
030300180307         oltre5 = da5a100 + oltre100;
030400180307         except Riga;
030500180307
030600180307         Name_Of_Day = 'Giovedì';
030700180307         inferiore5 = skinf5(04);
030800180307         da5a100 = skda5a100(04);
030900180307         oltre100 = skoltre100(04);
031000180307         oltre5 = da5a100 + oltre100;
031100180307         except Riga;
031200180307
031300180307         Name_Of_Day = 'Venerdì';
031400180307         inferiore5 = skinf5(05);
031500180307         da5a100 = skda5a100(05);
031600180307         oltre100 = skoltre100(05);
031700180307         oltre5 = da5a100 + oltre100;
031800180307         except Riga;
031900180307
032000180307         Name_Of_Day = 'Sabato';
032100180307         inferiore5 = skinf5(06);
032200180307         da5a100 = skda5a100(06);
032300180307         oltre100 = skoltre100(06);
032400180307         oltre5 = da5a100 + oltre100;
032500180307         except Riga;
032600180307
032700180307         Name_Of_Day = 'Domenica';
032800180307         inferiore5 = skinf5(07);
032900180307         da5a100 = skda5a100(07);
033000180307         oltre100 = skoltre100(07);
033100180307         oltre5 = da5a100 + oltre100;
033200180307         except Riga;
033300180307
033400180307       ENDSR;
033500180307
033600180307       //--------------------------------------------------------------
033700180307       //?Operazioni finali.
033800180307       //--------------------------------------------------------------
033900180307       BEGSR RoutEnd;
034000180307
034100180307         *inLR = *on;
034200180307         return;
034300180307
034400180307       ENDSR;
034500180307
034600180307     oPRTF198   e            Testata        2 02
034700180307     o                                           20 'BRT S.p.A.'
034800180307     o                                           60 '**  TOTALI ORM **'
034900180307     o                       udate              110 '  /  /  '
035000180307     o                                          120 'TNVRMB63R'
035100180307     o                                          128 'Pag.'
035200180307     o                       page          Z    132
035300180307     o          e            Testata        2
035400180307     o                                           20 'ORM posticipabili:'
035500180307     o          e            Testata        2
035600180307     o                                           20 'Fino a 5 Kg.'
035700180307     o                                           +1 'Da 5 a 100 Kg.'
035800180307     o                                           +1 'Oltre 100 Kg.'
035900180307     o                                           +1 'Oltre 5 Kg.'
036000180307     o          e            Riga           1
036100180307     o                                            1 ' '
036200180307     o                       inferiore5    2   +  1
036300180307     o                       da5a100       2   +  1
036400180307     o                       oltre100      2   +  1
036500180307     o                       oltre5        2   +  1
