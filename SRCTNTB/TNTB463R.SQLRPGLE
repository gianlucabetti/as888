000100120326       //==============================================================
000200120424       //?TNTB463R * Conta quanti cd schedulati nel giorno
000300120326       //==============================================================
000400120326
000500120326     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600120326     h dftactgrp(*no)
000700120326
000800120326       //--------------------------------------------------------------
000900120326       //?Dichiarazione file.                                          ?
001000120326       //--------------------------------------------------------------
001100120326
001200120326       // -?Tabelle?
001300120424     fTNTBE00F  if   e             disk
001400120326
001500120326       // -?Video?
001600120424     fTNTB463D  cf   e             workstn
001700120424     f                                     sfile(TB463S01 : S01nrr)
001800120326     f                                     indds(IndDspF)
001900120326     f                                     infds(InfDspF)
002000120326
002100120326       //--------------------------------------------------------------
002200120326       //?Definizione costanti.                                        ?
002300120326       //--------------------------------------------------------------
002400120326
002500120326       // -?Codice tabella in gestione?
002600120326     d c_Tab           c                   const('LAC')
002700120326
002800120326       // -?Numero massimo di record gestibili?
002900120326     d c_MaxRec        c                   const(9999)
003000120326
003100120326       // -?Tasti funzionali a video?
003200120326     d c_F01           c                   const(x'31')
003300120326     d c_F02           c                   const(x'32')
003400120326     d c_F03           c                   const(x'33')
003500120326     d c_F04           c                   const(x'34')
003600120326     d c_F05           c                   const(x'35')
003700120326     d c_F06           c                   const(x'36')
003800120326     d c_F07           c                   const(x'37')
003900120326     d c_F08           c                   const(x'38')
004000120326     d c_F09           c                   const(x'39')
004100120326     d c_F10           c                   const(x'3A')
004200120326     d c_F11           c                   const(x'3B')
004300120326     d c_F12           c                   const(x'3C')
004400120326     d c_F13           c                   const(x'B1')
004500120326     d c_F14           c                   const(x'B2')
004600120326     d c_F15           c                   const(x'B3')
004700120326     d c_F16           c                   const(x'B4')
004800120326     d c_F17           c                   const(x'B5')
004900120326     d c_F18           c                   const(x'B6')
005000120326     d c_F19           c                   const(x'B7')
005100120326     d c_F20           c                   const(x'B8')
005200120326     d c_F21           c                   const(x'B9')
005300120326     d c_F22           c                   const(x'BA')
005400120326     d c_F23           c                   const(x'BB')
005500120326     d c_F24           c                   const(x'BC')
005600120326     d c_Enter         c                   const(x'F1')
005700120326     d c_RollDown      c                   const(x'F4')
005800120326     d c_RollUp        c                   const(x'F5')
005900120326
006000120326       //--------------------------------------------------------------
006100120326       //?Definizione schiere.                                         ?
006200120326       //--------------------------------------------------------------
006300120326
006400120326       //--------------------------------------------------------------
006500120326       //?Definizione aree dati.                                       ?
006600120326       //--------------------------------------------------------------
006700120326
006800120326       //--------------------------------------------------------------
006900120326       //?Definizione strutture dati.                                  ?
007000120326       //--------------------------------------------------------------
007100120326
007200120326       // -?InfDS?
007300120326     d InfDspF         ds
007400120326     d   dsp_aid             369    369a
007500120326     d   sfl_rrn             376    377i 0
007600120326     d   min_nrr             378    379i 0
007700120326     d   num_rcds            380    381i 0
007800120326
007900120326       // -?Indicatori su DspF?
008000120326     d IndDspF         ds                  inz
008100120326         // -?Abilitazione tasti funzionali?
008200120326         // -?Indicatori di gestione del subfile?
008300120326     d   SflDsp_N                      n   overlay(IndDspF : 30)
008400120326     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
008500120326     d   SflEnd                        n   overlay(IndDspF : 33)
008600120426         //   -?Posizionamento cursore
008700120426     d   PosCurMese                    n   overlay(IndDspF : 50)
008800120326         //   -?Riemissione videata?
008900120326     d   ErrGenerico                   n   overlay(IndDspF : 99)
009000120326
009100120326       // -?Parametri ricevuti?
009200120326     d KPJBA         e ds
009300120326
009400120326       //--------------------------------------------------------------
009500120326       //?Definizione variabili globali.                               ?
009600120326       //--------------------------------------------------------------
009700120326
009800120326       // -?Flags booleani?
009900120426     d xMese           s               n   inz
010000120424     d wFine           s               n   inz
010100120424     d wEnd            s               n   inz
010200120424     d wInzS01         s               n   inz(*on)
010300120326
010400120326       // -?Variabili per la gestione del video?
010500120424     d wVideo          s              2    inz('S1')
010600120326     d S01nrr          s                   like(C1RcdNbr) inz
010700120426
010800120426       // -?Stringa SQL da eseguire
010900120426     d wSQL            s           2048    Varying        inz
011000120424
011100120424       // -?Campi di comodo
011200120426     d savC01mese      s                   like(C01mese)
011300120426     d savwlacgg       s              2a   inz
011400120424     d wlacgg          s              2a   inz
011500120424     d wnrcd           s              3s 0 inz
011600120326
011700120326       //--------------------------------------------------------------
011800120326       //?Definizione prototipi procedure.                             ?
011900120326       //--------------------------------------------------------------
012000120326
012100120326       //--------------------------------------------------------------
012200120326       //?Definizione key-list.                                        ?
012300120326       //--------------------------------------------------------------
012400120326
012500120326       //--------------------------------------------------------------
012600120326       //?Riepilogo indicatori utilizzati.                             ?
012700120326       //--------------------------------------------------------------
012800120326
012900120326
013000120326       //--------------------------------------------------------------
013100120326       //?M A I N - L I N E                                            ?
013200120326       //--------------------------------------------------------------
013300120326
013400120326     c     *Entry        plist
013500120326     c                   parm                    KPJBA
013600120326
013700120326      /free
013800120326
013900120326       // -?Operazioni iniziali?
014000120426       exsr RoutInz;
014100120326
014200120326       // -?Ciclo di gestione del file video?
014300120424       DoW  wFine = *off;
014400120326         select;
014500120326           // -?Gestione videata S1 (subfile)?
014600120424           when wVideo = 'S1';
014700120426             exsr GesS01;
014800120326           other;
014900120424             wFine = *on;
015000120326         endsl;
015100120326       enddo;
015200120326
015300120326       // -?Operazioni finali?
015400120426       exsr RoutEnd;
015500120326
015600120326       //--------------------------------------------------------------
015700120326       //?Operazioni iniziali.                                         ?
015800120326       //--------------------------------------------------------------
015900120426       BEGSR  RoutInz;
016000120327
016100120327         // -?Impostazione chiusura?
016200120327         *inLR = *on;
016300120424
016400120424         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016500120326
016600120326       ENDSR;
016700120326
016800120326       //--------------------------------------------------------------
016900120326       //?Gestione subfile S01.                                        ?
017000120326       //--------------------------------------------------------------
017100120426       BEGSR  GesS01;
017200120326
017300120326         // -?Inizializzazione videata?
017400120426         IF  wInzS01 = *on;
017500120426           exsr  InzS01;
017600120424           wInzS01 = *off;
017700120426         ENDIF;
017800120326
017900120424         // -?Emissione Piede?
018000120424         write  TB463P01;
018100120326
018200120326         // -?Posizionamento cursore?
018300120326         if  C1CsrRrn > *zero;
018400120326           C1RcdNbr = C1CsrRrn;
018500120326         endif;
018600120326
018700120326         // -?Emissione videata?
018800120426         PosCurMese = *on;
018900120424         exfmt  TB463C01;
019000120326
019100120326         reset  ErrGenerico;
019200120326
019300120326         select;
019400120326
019500120326           // -?F12=Ritorno?
019600120326           when  dsp_aid = c_F12;
019700120424             wFine  = *on;
019800120326
019900120326           // -?Invio?
020000120326           other;
020100120426             // -?Richiesto mese          ?
020200120426             IF  C01mese <> savC01mese;
020300120426               xMese = *on;
020400120426               IF  C01mese = 0;
020500120426                 xMese = *off;
020600120426               ENDIF;
020700120426               wInzS01 = *on;
020800120426               savC01mese = C01mese;
020900120426             ENDIF;
021000120326
021100120326         endsl;
021200120326
021300120326       ENDSR;
021400120326
021500120326       //--------------------------------------------------------------
021600120326       //?Inizializzazione subfile S01.                                ?
021700120326       //--------------------------------------------------------------
021800120426       BEGSR  InzS01;
021900120426
022000120426         wEnd = *off;
022100120426         clear savwlacgg;
022200120426
022300120426         // -?Mese calcolato?
022400120426         IF  not xMese;
022500120426           clear C01mese;
022600120426           clear savC01mese;
022700120426         ENDIF;
022800120326
022900120326         // -?Pulizia subfile?
023000120326         SflDsp_N    = *on;
023100120326         SflDspCtl_N = *on;
023200120424         write  TB463C01;
023300120326         SflDspCtl_N = *off;
023400120326         SflEnd      = *off;
023500120326
023600120326         clear  C1RcdNbr;
023700120326         clear  C1CsrRrn;
023800120326         clear  S01nrr;
023900120326         ErrGenerico = *off;
024000120326
024100120426         // -?Leggo la tabella LAC?
024200120426         exsr PrepSql;
024300120426
024400120426         exec sql
024500120426           prepare S1   from :wSQL;
024600120426         exec sql
024700120426           declare LAC  cursor for S1;
024800120424
024900120426         exec sql
025000120426          OPEN  LAC;
025100120424
025200120426         IF sqlcode < 0;
025300120426           wEnd = *on;
025400120426         ENDIF;
025500120424
025600120426         DOW not wEnd;
025700120426           exec sql
025800120426             FETCH NEXT   from LAC  into :wlacgg;
025900120426           IF sqlcod = 100 or sqlcod < 0;
026000120426             wEnd = *on;
026100120426             leave;
026200120426           ENDIF;
026300120426
026400120426           IF  wlacgg <> savwlacgg;
026500120426             IF  savwlacgg <> *blanks;
026600120426               exsr  CarS01;
026700120426             ENDIF;
026800120426             savwlacgg = wlacgg;
026900120426             clear wnrcd;
027000120426           ENDIF;
027100120424
027200120426           wnrcd += 1;
027300120424
027400120426         ENDDO;
027500120426
027600120426         // -?Carico ultimo rcd nel subfile?
027700120426         IF  savwlacgg <> *blanks;
027800120426           exsr  CarS01;
027900120426         ENDIF;
028000120424
028100120426         exec sql
028200120426           CLOSE  LAC;
028300120424
028400120424         // -?Visualizzazione del SFL (se ci sono dati)?
028500120424         SflDsp_N = (S01nrr <= *zero);
028600120424
028700120424         // -?Attivazione del SFLEND?
028800120424         SflEnd = ( wEnd );
028900120326
029000120326         // -?Impaginazione subfile?
029100120326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
029200120326         if S01nrr > *zero;
029300120326           C1RcdNbr  = 1;
029400120326           C1CsrRrn  = 1;
029500120326         endif;
029600120326
029700120326       ENDSR;
029800120426
029900120426       //--------------------------------------------------------------
030000120426       //?Preparo la stringa SQL.
030100120426       //--------------------------------------------------------------
030200120426       BEGSR  PrepSQL;
030300120426
030400120426         wSQL = 'SELECT substr(TBEuni, 69, 2), +
030500120426                        substr(TBEuni, 156, 7) +
030600120426                 FROM TNTBE00F +
030700120426                 WHERE TBEcod = ''LAC'' +
030800120426                       and substr(TBEuni, 66, 1) = ''S''';
030900120426         SELECT;
031000120426           WHEN  C01mese = 1;
031100120426             wSQL += ' and substr(TBEuni, 71, 1 ) = ''X''';
031200120426           WHEN  C01mese = 2;
031300120426             wSQL += ' and substr(TBEuni, 72, 1 ) = ''X''';
031400120426           WHEN  C01mese = 3;
031500120426             wSQL += ' and substr(TBEuni, 73, 1 ) = ''X''';
031600120426           WHEN  C01mese = 4;
031700120426             wSQL += ' and substr(TBEuni, 74, 1 ) = ''X''';
031800120426           WHEN  C01mese = 5;
031900120426             wSQL += ' and substr(TBEuni, 75, 1 ) = ''X''';
032000120426           WHEN  C01mese = 6;
032100120426             wSQL += ' and substr(TBEuni, 76, 1 ) = ''X''';
032200120426           WHEN  C01mese = 7;
032300120426             wSQL += ' and substr(TBEuni, 77, 1 ) = ''X''';
032400120426           WHEN  C01mese = 8;
032500120426             wSQL += ' and substr(TBEuni, 78, 1 ) = ''X''';
032600120426           WHEN  C01mese = 9;
032700120426             wSQL += ' and substr(TBEuni, 79, 1 ) = ''X''';
032800120426           WHEN  C01mese = 10;
032900120426             wSQL += ' and substr(TBEuni, 80, 1 ) = ''X''';
033000120426           WHEN  C01mese = 11;
033100120426             wSQL += ' and substr(TBEuni, 81, 1 ) = ''X''';
033200120426           WHEN  C01mese = 12;
033300120426             wSQL += ' and substr(TBEuni, 82, 1 ) = ''X''';
033400120426         ENDSL;
033500120426
033600120426         wSQL += ' GROUP BY substr(TBEuni, 69, 2), substr(TBEuni, 156, 7)';
033700120426         wSQL += ' ORDER BY substr(TBEuni, 69, 2), substr(TBEuni, 156, 7)';
033800120426
033900120426       ENDSR;
034000120326
034100120326       //--------------------------------------------------------------
034200120326       //?Caricamento completo del subfile S01                         ?
034300120326       //--------------------------------------------------------------
034400120426       BEGSR  CarS01;
034500120326
034600120424         S01gg = %dec(savwlacgg:2:0);
034700120424         S01cd = wnrcd;
034800120424         S01nrr += 1;
034900120424         write  TB463S01;
035000120326
035100120326       ENDSR;
035200120326
035300120326       //--------------------------------------------------------------
035400120326       //?Operazioni finali.                                           ?
035500120326       //--------------------------------------------------------------
035600120426       BEGSR  RoutEnd;
035700120326
035800120326         // -?"Chiusura" *pgm?
035900120326         return;
036000120326
036100120326       ENDSR;
036200120326
036300120326      /end-free
