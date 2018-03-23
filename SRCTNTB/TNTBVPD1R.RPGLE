000100151007       //==============================================================
000200151007       //?TNTBVPD1R * Selez. tab."VPD" = Variabili Calcolo Peso Desunto?
000300151007       //==============================================================
000400151007
000500151007       //--------------------------------------------------------------
000600151007       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700151007       //--------------------------------------------------------------
000800151007
000900151007     /*PRM dbgview(*source)
001000151007     /*END
001100151007
001200151007       //--------------------------------------------------------------
001300151007       //?Specifiche di controllo.                                     ?
001400151007       //--------------------------------------------------------------
001500151007
001600151007     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700151007     h dftactgrp(*no)
001800151007
001900151007       //--------------------------------------------------------------
002000151007       //?Dichiarazione file.                                          ?
002100151007       //--------------------------------------------------------------
002200151007
002300151007       // -?Tabelle?
002400151008     fTNTBE01L  if   e           k disk
002500151007
002600151007       // -?Video?
002700151007     fTNTBVPD1D cf   e             workstn
002800151007     f                                     sfile( TBVPDS01 : S01nrr )
002900151007     f                                     indds( IndDspF )
003000151007     f                                     infds( InfDspF )
003100151007
003200151007       //--------------------------------------------------------------
003300151007       //?Definizione costanti.                                        ?
003400151007       //--------------------------------------------------------------
003500151007
003600151007       // -?Codice tabella in gestione?
003700151007     d c_Tab           c                   const('VPD')
003800151007
003900151007       // -?Numero di record in ciascuna videata di subfile?
004000151007     d c_SflPag        c                   const(15)
004100151008
004200151008       // -?Numero massimo di record gestibili?
004300151008     d c_MaxRec        c                   const(9999)
004400151007
004500151007       // -?Costante per controllo "caratteri solo numerici"?
004600151007     d c_Digits        c                   const('0123456789')
004700151007
004800151007       // -?Tasti funzionali a video?
004900151007     d c_F01           c                   const(x'31')
005000151007     d c_F02           c                   const(x'32')
005100151007     d c_F03           c                   const(x'33')
005200151007     d c_F04           c                   const(x'34')
005300151007     d c_F05           c                   const(x'35')
005400151007     d c_F06           c                   const(x'36')
005500151007     d c_F07           c                   const(x'37')
005600151007     d c_F08           c                   const(x'38')
005700151007     d c_F09           c                   const(x'39')
005800151007     d c_F10           c                   const(x'3A')
005900151007     d c_F11           c                   const(x'3B')
006000151007     d c_F12           c                   const(x'3C')
006100151007     d c_F13           c                   const(x'B1')
006200151007     d c_F14           c                   const(x'B2')
006300151007     d c_F15           c                   const(x'B3')
006400151007     d c_F16           c                   const(x'B4')
006500151007     d c_F17           c                   const(x'B5')
006600151007     d c_F18           c                   const(x'B6')
006700151007     d c_F19           c                   const(x'B7')
006800151007     d c_F20           c                   const(x'B8')
006900151007     d c_F21           c                   const(x'B9')
007000151007     d c_F22           c                   const(x'BA')
007100151007     d c_F23           c                   const(x'BB')
007200151007     d c_F24           c                   const(x'BC')
007300151007     d c_Enter         c                   const(x'F1')
007400151007     d c_RollDown      c                   const(x'F4')
007500151007     d c_RollUp        c                   const(x'F5')
007600151007
007700151007       //--------------------------------------------------------------
007800151007       //?Definizione schiere.                                         ?
007900151007       //--------------------------------------------------------------
008000151007
008100151007       // -?Messaggi di errore?
008200151008     d sk_Msg          s             78    dim( 3)  ctdata  perrcd(1)
008300151007
008400151007       //--------------------------------------------------------------
008500151007       //?Definizione aree dati.                                       ?
008600151007       //--------------------------------------------------------------
008700151007
008800151007       // -?Dati utente?
008900151007     d §AzUte        e ds                  extname(AZUTE00F)
009000151007     d                                     dtaara
009100151007     d §DatiUte      e ds                  extname(dDatiUte)
009200151007     d                                     dtaara
009300151007
009400151007       //--------------------------------------------------------------
009500151007       //?Definizione strutture dati.                                  ?
009600151007       //--------------------------------------------------------------
009700151007
009800151007       // -?Status ds?
009900151007     d Status         sds
010000151007     d   SDSpgm          *proc
010100151007     d*//SDSprm          *parms
010200151007
010300151007       // -?InfDS?
010400151007     d InfDspF         ds
010500151007     d   dsp_aid             369    369a
010600151007     d   sfl_rrn             376    377i 0
010700151007     d   min_nrr             378    379i 0
010800151007     d   num_rcds            380    381i 0
010900151007
011000151007       // -?Indicatori su DspF?
011100151007     d IndDspF         ds                  inz
011200151007         // -?Abilitazione tasti funzionali?
011300151008     d*//$F3Attivo                     n   overlay( IndDspF : 03 )
011400151007     d   $F9Attivo                     n   overlay( IndDspF : 09 )
011500151007     d   $F12Attivo                    n   overlay( IndDspF : 12 )
011600151007         // -?Emissione messaggio di errore?
011700151007     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011800151007         // -?Indicatori di gestione del subfile?
011900151007     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
012000151007     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
012100151007     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
012200151007     d   $SflEnd                       n   overlay( IndDspF : 33 )
012300151007     d   $SflStart                     n   overlay( IndDspF : 34 )
012400151007         // -?Indicatori per Attibuti di visualizzazione?
012500151007         //  ?o   Condizionamento visualizzazione campi?
012600151007     d   $PosizKE1                     n   overlay( IndDspF : 40 )
012700151007     d   $RecAnnull                    n   overlay( IndDspF : 41 )
012800151007     d   $DataErrata                   n   overlay( IndDspF : 42 )
012900151008     d   $RecAnnullP1                  n   overlay( IndDspF : 49 )
013000151007         // -?Posizionamento cursore & segnalazione errore?
013100151007     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
013200151007         // -?Riemissione videata?
013300151007     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013400151007
013500151007       // -?Parametri ricevuti?
013600151007     d KPJBA         e ds
013700151007     d TNTB79ds      e ds                  inz
013800151007
013900151007       // -?Tab. VPD = Variabili calcolo Peso Desunto?
014000151007     d dVPD          e ds                  inz
014100151007
014200151007       //--------------------------------------------------------------
014300151007       //?Definizione variabili globali.                               ?
014400151007       //--------------------------------------------------------------
014500151007
014600151007       // -?Flags booleani?
014700151007     d $Fine           s               n   inz
014800151007     d $InzS01         s               n   inz(*on)
014900151007
015000151007       // -?Variabili per la gestione del video?
015100151007     d $Video          s              2    inz('S1')
015200151007     d S01nrr          s                   like(C1RcdNbr) inz
015300151007     d wSavS01csr      s                   like(C1RcdNbr) inz
015400151008
015500151008       // -?Campi per il posizionamento nel subfile?
015600151008     d wSaveKE1        s                   like(C1Cke1)   inz
015700151008     d wAAMGKE1        s              8    inz
015800151007
015900151007       // -?Conteggio selezioni (opz. 1) inserite?
016000151007     d wMemoSel        s                   like(S1Copz)   inz
016100151007
016200151007       // -?Variabili di comodo?
016300151007     d wData_EUR       s               d   datfmt(*eur)  inz(*loval)
016400151007
016500151007       //--------------------------------------------------------------
016600151007       //?Definizione prototipi procedure.                             ?
016700151007       //--------------------------------------------------------------
016800151007
016900151007       // -?Reperimento dati utente?
017000151007     d TIBS34ds      e ds                  inz
017100151007      /copy gaitrasrc/srcProtoPR,TIBS34R
017200151008
017300151008       // -?Controllo/Inversione data?
017400151008     d WLBdat          ds                  inz
017500151008     d  G08dat                        8  0 inz
017600151008     d  G08inv                        8  0 inz
017700151008     d  G08err                        1    inz('3')
017800151008     d  G08tgi                        5  0 inz
017900151008      /copy gaitrasrc/srcProtoPR,XSRDA8
018000151007
018100151007       //--------------------------------------------------------------
018200151007       //?Definizione key-list.                                        ?
018300151007       //--------------------------------------------------------------
018400151007
018500151007       // -?File TNTBE01L?
018600151007     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
018700151007     d                                     prefix( k_ )   inz
018800151007
018900151007       //--------------------------------------------------------------
019000151007       //?Riepilogo indicatori utilizzati.                             ?
019100151007       //--------------------------------------------------------------
019200151007
019300151007
019400151007       //--------------------------------------------------------------
019500151007       //?M A I N - L I N E                                            ?
019600151007       //--------------------------------------------------------------
019700151007
019800151007     c     *Entry        plist
019900151007     c                   parm                    KPJBA
020000151007
020100151007      /free
020200151007
020300151007       // -?Operazioni iniziali?
020400151007       exsr sr_RoutInz;
020500151007
020600151007       // -?Ciclo di gestione del file video?
020700151007       DoW  $Fine = *off;
020800151007
020900151007         select;
021000151007
021100151007           // -?Gestione videata S1 (subfile)?
021200151007           when $Video = 'S1';
021300151007             exsr  sr_GesS01;
021400151007
021500151007           // -?? ? ??
021600151007           other;
021700151007             $Fine = *on;
021800151007
021900151007         endsl;
022000151007
022100151007       EndDo;
022200151007
022300151007       // -?Operazioni finali?
022400151007       exsr sr_RoutEnd;
022500151007
022600151007       //--------------------------------------------------------------
022700151007       //?Operazioni iniziali.                                         ?
022800151007       //--------------------------------------------------------------
022900151007       BEGSR  sr_RoutInz;
023000151007
023100151007         // -?Impostazione chiusura?
023200151007         *inLR = *on;
023300151007
023400151007         // -?Reperimento dati job?
023500151007         exsr  sr_DatiJob;
023600151007
023700151007         // -?Impostazione nome programma a video?
023800151007         V1Tpgm = SDSpgm;
023900151007
024000151007         // -?Pulizia / impostazione iniziale dei campi chiave?
024100151007         clear  keyTNTBE01;
024200151007         k_TBEcod = c_Tab;
024300151007
024400151007         // -?Impostazione videata iniziale?
024500151007         $Video = 'S1';
024600151007         reset  $InzS01;
024700151007
024800151007       ENDSR;
024900151007
025000151007       //--------------------------------------------------------------
025100151007       //?Reperimento Dati del job (Utente/Operativi).                 ?
025200151007       //--------------------------------------------------------------
025300151007       BEGSR  sr_DatiJob;
025400151007
025500151007         in(e) §AzUte;
025600151007         if NOT %error;
025700151007           in(e) §DatiUte;
025800151007         endif;
025900151007         if %error or RSut = *blank;
026000151007           tibs34r ( tibs34ds );
026100151007           in §AzUte;
026200151007           in §DatiUte;
026300151007         endif;
026400151007
026500151007       ENDSR;
026600151007
026700151007       //--------------------------------------------------------------
026800151007       //?Gestione subfile S01 - Subfile con elenco tab. "3EW".        ?
026900151007       //--------------------------------------------------------------
027000151007       BEGSR  sr_GesS01;
027100151007
027200151007         // -?Inizializzazione videata?
027300151007         if  $InzS01;
027400151007           exsr  sr_InzS01;
027500151007           $InzS01 = *off;
027600151007         endif;
027700151007
027800151008         // -?(Dis)Abilitazione tasti funzionali?
027900151008         exsr  sr_AbilitFxxS01;
028000151007
028100151007         // -?Emissione Testata & Piede con tasti funzionali abilitati?
028200151007         write  tbVPDt01;
028300151007         write  tbVPDp01;
028400151007
028500151007         // -?Posizionamento cursore?
028600151007         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
028700151007         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
028800151007         //  ?si visualizza la pagina che vedeva l'utente quando ha?
028900151007         //  ?premuto l'ultimo tasto.?
029000151007         if  C1CsrRrn > *zero;
029100151007           C1RcdNbr = C1CsrRrn;
029200151007         else;
029300151007           C1RcdNbr = 1;
029400151007           write  tbVPDs00;            //?(no rec.)?
029500151007         endif;
029600151007
029700151007         // -?Emissione videata?
029800151007         exfmt  tbVPDc01;
029900151007
030000151008         reset  $ErrGenerico;
030100151008         reset  $ErrMessage;
030200151007         clear  V1Dmsg;
030300151007
030400151007         Select;
030500151007
030600151007           // -?F3=Fine?
030700151007           When  dsp_aid = c_F03;
030800151007             //oVPDfxx = '1';
030900151007             $Fine = *on;
031000151007
031100151007           // -?F5=Rivisualizza?
031200151007           When  dsp_aid = c_F05;
031300151007             $InzS01 = *on;
031400151007
031500151007           // -?F12=Ritorno?
031600151007           When  dsp_aid = c_F12;
031700151007             //oVPDfxx = '2';
031800151007             $Fine = *on;
031900151007
032000151008           //// -?SubFile vuoto?
032100151008           ////  ?(potrebbe essere stato modificato il posizionamento...)?
032200151008           //When  S01nrr = *zero;
032300151007
032400151007           // -?Invio?
032500151007           Other;
032600151008             // -?Gestione opzioni?
032700151008             if  S01nrr > *zero;
032800151008               exsr  sr_OpzS01;
032900151008               select;
033000151008                 when  $ErrGenerico;
033100151008                   leavesr;
033200151008                 when  wMemoSel > *zero;
033300151008                   $Fine = *on;
033400151008                   leavesr;
033500151008               endsl;
033600151008             endif;
033700151008             // -?Richiesto posizionamento?
033800151007             if  C1Cke1 <> wSaveKE1;
033900151007               $InzS01  =  *on;
034000151007               wSaveKe1 =  C1Cke1;
034100151007             endif;
034200151007
034300151007         EndSl;
034400151007
034500151007       ENDSR;
034600151007
034700151007       //--------------------------------------------------------------
034800151007       //?Inizializzazione subfile S01.                                ?
034900151007       //--------------------------------------------------------------
035000151007       BEGSR  sr_InzS01;
035100151007
035200151007         // -?Spegnimento degli indicatori di errore?
035300151007         %subst( IndDspF : 50 ) = *off;
035400151007
035500151007         // -?Pulizia subfile-control?
035600151007         clear  tbVPDc01;
035700151007
035800151007         C1Cke1  = wSaveKE1;
035900151008
036000151008         clear  WLBdat;
036100151008         G08dat = C1Cke1;
036200151008         XSRDA8 ( WLBdat );
036300151008         if  G08err = *off;
036400151008           C1Cke1   = G08dat;
036500151008           wSaveKE1 = G08dat;
036600151008           wAAMGke1 = %editc( G08inv : 'X' );
036700151008         else;
036800151008           wAAMGke1 = %subst( %editc( C1Cke1 : 'X' ) : 5 : 4 ) +
036900151008                      %subst( %editc( C1Cke1 : 'X' ) : 3 : 2 ) +
037000151008                      %subst( %editc( C1Cke1 : 'X' ) : 1 : 2 );
037100151008         endif;
037200151007
037300151007         // -?Pulizia subfile?
037400151007         $SflDsp_N    = *on;
037500151007         $SflDspCtl_N = *on;
037600151007         write  tbVPDc01;
037700151007         $SflDspCtl_N = *off;
037800151007         $SflEnd      = *off;
037900151007
038000151007         clear  C1RcdNbr;
038100151007         clear  C1CsrRrn;
038200151007         clear  S01nrr;
038300151007         clear  V1Dmsg;
038400151007         $SflNxtChg   = *off;
038500151007         $ErrMessage  = *off;
038600151007         $ErrGenerico = *off;
038700151008         $RecAnnullP1 = *off;
038800151008
038900151008         clear  wMemoSel;
039000151007
039100151007         // -?Caricamento completo dei dati nel subfile (per codice)?
039200151007         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
039300151008         //  ?l'eventuale ordinamento - PER ORA NON GESTITO)?
039400151007         clear  keyTNTBE01;
039500151007         k_TBEcod = c_Tab;
039600151008         if  wAAMGke1 <> *blank;
039700151008           k_TBEke1 = wAAMGke1;
039800151008         endif;
039900151007         setll  %kds( keyTNTBE01 )  TNTBE000;
040000151007         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
040100151007         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
040200151008           if  TBEke1 >= wAAMGke1;
040300151008             exsr  sr_WriteS01;
040400151008           endif;
040500151008           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
040600151007         EndDo;
040700151007
040800151007         // -?Visualizzazione del SFL (se ci sono dati)?
040900151007         $SflDsp_N  = (S01nrr <= *zero);
041000151007
041100151007         // -?Attivazione del SFLEND?
041200151007         $SflEnd    = %eof(TNTBE01L);
041300151007
041400151007         // -?Impaginazione subfile?
041500151007         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
041600151007         if  S01nrr > *zero;
041700151007           C1RcdNbr  = 1;
041800151007           C1CsrRrn  = 1;
041900151007         else;
042000151007           clear  C1RcdNbr;
042100151007         endif;
042200151007
042300151007         //// -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
042400151007         //RrnLast  = S01nrr;
042500151007         //
042600151007         //// -?Ordinamento subfile (se premuto F8)?
042700151007         //if  $Ord_x_...  and  S01nrr > 1;
042800151007         //  exsr  sr_SortSfl;
042900151007         //endif;
043000151007
043100151007
043200151007         // -?(Dis)Abilitazione tasti funzionali?
043300151008         exsr  sr_AbilitFxxS01;
043400151007
043500151007         // -?Visualizzazione richiesta di posizionamento?
043600151008         $PosizKE1 = ( S01nrr <= 1  and  C1Cke1 = *zero );
043700151007
043800151007         // -?Emissione messaggio di segnalazione raggiungimento limite?
043900151007         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
044000151008           $ErrGenerico = *on;
044100151008           $ErrMessage  = *on;
044200151008           $PosCurOPZ   = *on;
044300151007           V1Dmsg = sk_Msg(01);
044400151007           leavesr;
044500151007         endif;
044600151007
044700151007       ENDSR;
044800151007
044900151007       //--------------------------------------------------------------
045000151007       //?Registrazione del singolo rec. nel subfile S01.              ?
045100151007       //--------------------------------------------------------------
045200151007       BEGSR  sr_WriteS01;
045300151007
045400151007         dVPD = TBEuni;
045500151007
045600151007         // -?Impostazione campi nel record del subfile?
045700151007         clear  tbVPDs01;
045800151007
045900151008         S1Hke1 = TBEke1;
046000151008         S1Hatb = TBEatb;
046100151007
046200151007         //S1Cke1 = %int( %trimR( TBEke1 ) );
046300151007         reset  WLBdat;
046400151007         G08inv = %int( %trim( TBEke1 ) );
046500151007         XSRDA8 ( WLBdat );
046600151007         if  G08err = *off;
046700151007           S1Cke1 = G08dat;
046800151007         else;
046900151007           S1Cke1 = %int( %subst( TBEke1 : 5 : 2 ) +
047000151007                          %subst( TBEke1 : 3 : 2 ) +
047100151007                          %subst( TBEke1 : 1 : 2 ) );
047200151007         endif;
047300151007         S1mesi   = §VPDmesi;
047400151007         S1nSped  = §VPDnSped;
047500151007         S1colli  = §VPDcolli;
047600151008         S1pKda1  = §VPDpKda1;
047700151008         S1pKal1  = §VPDpKal1;
047800151008         S1scoDa1 = §VPDscoDa1 * (-1);
047900151008         S1scoA1  = §VPDscoA1  * (-1);
048000151008         S1pKda2  = §VPDpKda2;
048100151008         S1pKal2  = §VPDpKal2;
048200151008         S1scoDa2 = §VPDscoDa2 * (-1);
048300151008         S1scoA2  = §VPDscoA2  * (-1);
048400151007
048500151007         // -?Impostazione attributi di visualizzazione?
048600151007         //  ?per il singolo record del subfile?
048700151007         $RecAnnull  = ( TBEatb <> *blank );
048800151007         $DataErrata = ( G08err = *on );
048900151008
049000151008         if  $RecAnnull;
049100151008           $RecAnnullP1 = *on;
049200151008         endif;
049300151007
049400151007         // -?Registrazione singolo record nel subfile?
049500151007         S01nrr += 1;
049600151007         write  tbVPDs01;
049700151007
049800151007       ENDSR;
049900151008
050000151008       //--------------------------------------------------------------
050100151008       //?(Dis)Abilitazione tasti funzionali per subfile S01.          ?
050200151008       //--------------------------------------------------------------
050300151008       BEGSR  sr_AbilitFxxS01;
050400151008
050500151008         //// ·?F3=Fine?
050600151008         //$F3Attivo  = *off;
050700151008
050800151008         //// ·?F8=Ordinamento x ...?
050900151008         //$F8Attivo  = ( S01nrr > 1 );
051000151008
051100151008         // ·?F9=Altri Dati?
051200151008         $F9Attivo  = ( S01nrr > *zero );
051300151008
051400151008         // ·?F12=Ritorno?
051500151008         $F12Attivo = *on;
051600151008
051700151008       ENDSR;
051800151007
051900151007       //--------------------------------------------------------------
052000151007       //?Gestione opzioni subfile S01.                                ?
052100151007       //--------------------------------------------------------------
052200151007       BEGSR  sr_OpzS01;
052300151007
052400151007         // -?Spegnimento degli indicatori di errore?
052500151007         %subst( IndDspF : 50 ) = *off;
052600151007
052700151007         clear  wSavS01csr;
052800151008         clear  wMemoSel;
052900151007
053000151007         // -?Ciclo di lettura subfile?
053100151007         readc  tbVPDs01;
053200151007
053300151007         DoW  Not %eof(TNTBVPD1D);
053400151007
053500151007           $SflNxtChg = *off;
053600151007           %subst( IndDspF : 50 ) = *off;
053700151007           C1RcdNbr = S01nrr;
053800151007
053900151007           select;
054000151007
054100151007             // -?Opz. 1=Selezione?
054200151007             when  S1Copz = 1;
054300151007               // -?Già selezionato un cliente?
054400151007               if  wMemoSel > *zero;
054500151007                 $ErrGenerico = *on;
054600151007                 $ErrMessage  = *on;
054700151007                 $PosCurOPZ   = *on;
054800151007                 V1Dmsg = sk_Msg(02);
054900151007               else;
055000151007                 wMemoSel = S1Copz;
055100151007               endif;
055200151007               KPJBU = S1Hke1;
055300151008               k_TBEke1 = S1Hke1;
055400151008               chain  %kds( keyTNTBE01 )  TNTBE000;
055500151008               if  %found(TNTBE01L);
055600151007                 KPJBU = TBEke1 + TBEuni;
055700151007               endif;
055800151007
055900151007             // -?Nessuna opzione?
056000151007             when  S1Copz = *zero;
056100151007
056200151007             // -?Opzione errata?
056300151007             other;
056400151007               $ErrGenerico = *on;
056500151008               $ErrMessage  = *on;
056600151007               $PosCurOPZ   = *on;
056700151007               V1Dmsg = sk_Msg(03);
056800151007
056900151007           endsl;
057000151007
057100151007           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
057200151007           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
057300151008           if  S1Copz  <> *zero   and
057400151008              (wSavS01csr = *zero  or  wSavS01csr < C1RcdNbr);
057500151008             wSavS01csr = C1RcdNbr;
057600151007           endif;
057700151007
057800151007           // -?Aggiornamento sfl?
057900151007           select;
058000151007             when  $ErrMessage  or  S1Copz <> *zero;
058100151008               C1CsrRrn = C1RcdNbr;
058200151007             when  $ErrGenerico;
058300151008               C1CsrRrn = C1RcdNbr;
058400151007               if  S1Copz <> 1;
058500151007                 clear  S1Copz;
058600151007               endif;
058700151007             other;
058800151007               if  S1Copz <> 1;
058900151007                 clear  S1Copz;
059000151007               endif;
059100151007           endsl;
059200151007
059300151007           if  S1Copz <> *zero;
059400151007             $SflNxtChg = *on;
059500151007           endif;
059600151007
059700151007           // -?Impostazione attributi di visualizzazione?
059800151007           //  ?per il singolo record del subfile?
059900151007           $RecAnnull  = ( S1Hatb <> *blank );
060000151007           //$DataErrata =
060100151007
060200151007           UPDATE  tbVPDs01;
060300151007
060400151007           if  $ErrGenerico  or  $ErrMessage;
060500151007             leave;
060600151007           endif;
060700151007
060800151007           readc  tbVPDs01;
060900151007
061000151007         EndDo;
061100151007
061200151007         // -?Riposizionam. cursore sul record contenente la prima opz.?
061300151007         //  ?(se non sono stati rilevati errori)?
061400151007         if  NOT $ErrMessage  and  NOT $ErrGenerico  and
061500151007             wSavS01csr > *zero;
061600151007           C1CsrRrn = wSavS01csr;
061700151007         endif;
061800151007
061900151007       ENDSR;
062000151007
062100151007       //--------------------------------------------------------------
062200151007       //?Operazioni finali.                                           ?
062300151007       //--------------------------------------------------------------
062400151007       BEGSR  sr_RoutEnd;
062500151007
062600151007         // -?"Chiusura" *pgm?
062700151007         return;
062800151007
062900151007       ENDSR;
063000151007
063100151007      /end-free
063200151007
063300151007       //--------------------------------------------------------------
063400151007       //?Schiere a tempo di compilazione.                             ?
063500151007       //--------------------------------------------------------------
063600151007
063700151007** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
063800151007Raggiunto il limite massimo dei record caricabili a video                      1
063900151007Selezionate più di una variabile                                               2
064000151007Opzione errata                                                                 3
