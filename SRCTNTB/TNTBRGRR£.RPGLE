000100150508       //==============================================================
000200150508       //?Gestione tabella "RGR" (Reparti Gestione R.A.)               ?
000300150508       //==============================================================
000400150508
000500150508       //--------------------------------------------------------------
000600150508       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150508       //--------------------------------------------------------------
000800150508
000900150508     /*PRM  dbgview(*source)
001000150508     /*END
001100150508
001200150508       //--------------------------------------------------------------
001300150508       //?Specifiche di controllo.                                     ?
001400150508       //--------------------------------------------------------------
001500150508
001600150508     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150508     h dftactgrp(*no)
001800150508
001900150508       //--------------------------------------------------------------
002000150508       //?Dichiarazione file.                                          ?
002100150508       //--------------------------------------------------------------
002200150508
002300150508       // -?Tabelle
002400150508     fTNTBE01L  Uf A e           k disk
002500150508
002600150508       // -?Video?
002700150508     fTNTBRGRD  cf   e             workstn
002800150508     f                                     sfile(TBRGRS01 : S01nrr)
002900150508     f                                     indds(IndDspF)
003000150508     f                                     infds(InfDspF)
003100150508
003200150508       //--------------------------------------------------------------
003300150508       //?Definizione costanti.                                        ?
003400150508       //--------------------------------------------------------------
003500150508
003600150508       // -?Codice tabella in gestione?
003700150508     d c_Tab           c                   const('RGR')
003800150508
003900150508       // -?Numero di record in ciascuna videata di subfile?
004000150508     d c_SflPag        c                   const(13)
004100150508
004200150508       // -?Numero massimo di record gestibili?
004300150508     d c_MaxRec        c                   const(9999)
004400150508
004500150508       // -?Tasti funzionali a video?
004600150508     d c_F01           c                   const(x'31')
004700150508     d c_F02           c                   const(x'32')
004800150508     d c_F03           c                   const(x'33')
004900150508     d c_F04           c                   const(x'34')
005000150508     d c_F05           c                   const(x'35')
005100150508     d c_F06           c                   const(x'36')
005200150508     d c_F07           c                   const(x'37')
005300150508     d c_F08           c                   const(x'38')
005400150508     d c_F09           c                   const(x'39')
005500150508     d c_F10           c                   const(x'3A')
005600150508     d c_F11           c                   const(x'3B')
005700150508     d c_F12           c                   const(x'3C')
005800150508     d c_F13           c                   const(x'B1')
005900150508     d c_F14           c                   const(x'B2')
006000150508     d c_F15           c                   const(x'B3')
006100150508     d c_F16           c                   const(x'B4')
006200150508     d c_F17           c                   const(x'B5')
006300150508     d c_F18           c                   const(x'B6')
006400150508     d c_F19           c                   const(x'B7')
006500150508     d c_F20           c                   const(x'B8')
006600150508     d c_F21           c                   const(x'B9')
006700150508     d c_F22           c                   const(x'BA')
006800150508     d c_F23           c                   const(x'BB')
006900150508     d c_F24           c                   const(x'BC')
007000150508     d c_Enter         c                   const(x'F1')
007100150508     d c_RollDown      c                   const(x'F4')
007200150508     d c_RollUp        c                   const(x'F5')
007300150508
007400150508       //--------------------------------------------------------------
007500150508       //?Definizione schiere.                                         ?
007600150508       //--------------------------------------------------------------
007700150508
007800150508       // -?Messaggi di errore?
007900150508     d sk_Msg          s             78    dim( 6)  ctdata  perrcd( 1)
008000150508
008100150508       //--------------------------------------------------------------
008200150508       //?Definizione aree dati.                                       ?
008300150508       //--------------------------------------------------------------
008400150508
008500150508       // -?Dati utente?
008600150508     d §AzUte        e ds                  extname(AZUTE00F)
008700150508     d                                     dtaara
008800150508     d §DatiUte      e ds                  extname(dDatiUte)
008900150508     d                                     dtaara
009000150508
009100150508       //--------------------------------------------------------------
009200150508       //?Definizione strutture dati.                                  ?
009300150508       //--------------------------------------------------------------
009400150508
009500150508       // -?Status ds?
009600150508     d Status         sds
009700150508     d   SDSpgm          *proc
009800150508
009900150508       // -?InfDS?
010000150508     d InfDspF         ds
010100150508     d   dsp_aid             369    369a
010200150508     d*//sfl_rrn             376    377i 0
010300150508     d*//min_nrr             378    379i 0
010400150508     d*//num_rcds            380    381i 0
010500150508
010600150508       // -?Indicatori su DspF?
010700150508     d IndDspF         ds                  inz
010800150508         // -?Abilitazione tasti funzionali?
010900150508     d   $F3Attivo                     n   overlay( IndDspF : 03 )
011000150508     d   $F5Attivo                     n   overlay( IndDspF : 05 )
011100150508     d   $F6Attivo                     n   overlay( IndDspF : 06 )
011200150508     d   $F8Attivo                     n   overlay( IndDspF : 08 )
011300150508     d   $F10Attivo                    n   overlay( IndDspF : 10 )
011400150508     d   $F12Attivo                    n   overlay( IndDspF : 12 )
011500150508     d   $F16Attivo                    n   overlay( IndDspF : 16 )
011600150508         // -?Emissione messaggio di errore?
011700150508     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011800150508         // -?Emissione messaggio di errore in window W1?
011900150508     d   $ErrMessageW1...
012000150508     d                                 n   overlay( IndDspF : 29 )
012100150508         // -?Indicatori di gestione del subfile?
012200150508     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
012300150508     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
012400150508     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
012500150508     d   $SflEnd                       n   overlay( IndDspF : 33 )
012600150508         // -?Indicatori per Attributi di visualizzazione?
012700150508     d   $DisabilOPZ                   n   overlay( IndDspF : 40 )
012800150508     d   $Ord_x_Des                    n   overlay( IndDspF : 41 )
012900150508     d   $ProtectKEY                   n   overlay( IndDspF : 42 )
013000150508         // -?Posizionamento cursore & segnalazione errore?
013100150508     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
013200150508     d   $PosCurRGR                    n   overlay( IndDspF : 51 )
013300150508     d   $PosCurDES                    n   overlay( IndDspF : 52 )
013400150508         // -?Riemissione videata?
013500150508     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013600150508
013700150508       // -?Parametri ricevuti?
013800150508     d KPJBA         e ds
013900150508     d TNTBRGRds     e ds                  inz
014000150508
014100150508       // -?Dati record principale della tabella?
014200150508     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014300150508     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014400150508     d                                          prefix(TBX:3)
014500150508
014600150508       // -?Tabelle usate:?
014700150508       // ·?"RGR" = Reparti Gestione R.A.?
014800150508     d dRGR          e ds                  inz  qualified
014900150508
015000150508       //--------------------------------------------------------------
015100150508       //?Definizione variabili globali.                               ?
015200150508       //--------------------------------------------------------------
015300150508
015400150508       // -?Flags booleani?
015500150508     d $Called         s               n   inz
015600150508     d $Fine           s               n   inz
015700150508     d $InzS01         s               n   inz(*on)
015800150508     d $InzD02         s               n   inz(*on)
015900150508     d $InzW01         s               n   inz(*on)
016000150508     d $Annullamento   s               n   inz
016100150508     d $Immissione     s               n   inz
016200150508     d $Ripristino     s               n   inz
016300150508     d $Protect        s               n   inz
016400150508
016500150508       // -?Variabili per la gestione del video?
016600150508     d $Video          s              2    inz('S1')
016700150508     d S01nrr          s                   like(C1RcdNbr) inz
016800150508     d SavS01csr       s                   like(C1RcdNbr) inz
016900150508
017000150508       // -?Conteggio selezioni (opz. 1) inserite?
017100150508     d wMemoSel        s                   like(S1Copz)   inz
017200150508
017300150508       // -?Campi di comodo?
017400150508     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
017500150508     d wDate           s              8s 0 inz
017600150508
017700150508       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
017800150508
017900150508       //--------------------------------------------------------------
018000150508       // -?Constants?
018100150508       //--------------------------------------------------------------
018200150508       // -?MaxKey - Maximum number of key fields allowed by this program?
018300150508     d c_MaxKey        c                   const(2)
018400150508       // -?Sort order:?
018500150508       //  ?1 = Sort in an ascending sequence?
018600150508       //  ?2 = Sort in a descending sequence?
018700150508     d c_Ascendente    c                   const(1)
018800150508     d c_Discendente   c                   const(2)
018900150508       // -?Key data type:?
019000150508       //  ? 0 = Signed binary?
019100150508       //  ? 2 = Signed zoned decimal?
019200150508       //  ? 3 = Signed packed decimal?
019300150508       //  ? 6 = Character with no national language sort sequence applied,?
019400150508       //  ?     if specified?
019500150508       //  ? 7 = Unsigned packed decimal?
019600150508       //  ?     All numbers will have the sign forced positive ('F'X).?
019700150508       //  ? 8 = Unsigned zoned decimal?
019800150508       //  ?     All numbers will have the sign forced positive ('F'X).?
019900150508       //  ? 9 = Unsigned binary?
020000150508       //  ?14 = Date in form of DD/MM/YY?
020100150508       //  ?15 = Date in form of DD.MM.YYYY?
020200150508     d c_Numero        c                   const(2)
020300150508     d c_Carattere     c                   const(6)
020400150508     d c_NumIntero     c                   const(8)
020500150508       //
020600150508     d c_Put           c                   const(1)
020700150508     d c_EndPut        c                   const(2)
020800150508     d c_Get           c                   const(3)
020900150508
021000150508       //--------------------------------------------------------------
021100150508       // -?Data Structures?
021200150508       //  ?SflRcd     - The entire subfile record to pass to the sort?
021300150508       //  ?QLGSCB     - The sort request block for the QLGSORT API?
021400150508       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
021500150508       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
021600150508       //  ?QUSEC      - Error structure for the QLGSORT API?
021700150508       //--------------------------------------------------------------
021800150508     d SflRcd          ds                  inz
021900150508     d   S1Crgr                            inz
022000150508     d   S1Cdes                            inz
022100150508     d   S1Copz                            inz
022200150508     d   Selected                     1a   inz
022300150508      /copy QSYSINC/QRPGLESRC,QLGSORT
022400150508     d QLGKL                         16    dim(c_MaxKey)
022500150508     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
022600150508     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
022700150508     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
022800150508     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
022900150508      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023000150508      /copy QSYSINC/QRPGLESRC,QUSEC
023100150508
023200150508       //--------------------------------------------------------------
023300150508       // -?Standalone fields?
023400150508       //  ?Nrr        - Relative record number for subfile?
023500150508       //  ?SizeList   - Size of list?
023600150508       //  ?ReturnSize - Size of list returned by sort API?
023700150508       //--------------------------------------------------------------
023800150508     d NotUsed         s             16a   inz
023900150508     d ReturnSize      s              9b 0 inz
024000150508     d SizeList        s              9b 0 inz
024100150508     d RrnLast         s              5i 0 inz
024200150508     d Nrr             s              5i 0 inz
024300150508
024400150508       //--------------------------------------------------------------
024500150508       //?Definizione prototipi procedure.                             ?
024600150508       //--------------------------------------------------------------
024700150508
024800150508       // -?Reperimento dati utente?
024900150508     d TIBS34ds      e ds
025000150508      /copy gaitrasrc/srcProtoPR,TIBS34R
025100150508
025200150508       // -?Ricerca e controllo nuove tabelle?
025300150508     d TIBS02ds      e ds                  inz
025400150508     d   T02mod      e                     inz('C')
025500150508      /copy gaitrasrc/srcProtoPR,TIBS02R
025600150508
025700150508       // -?Ordinamento subfile?
025800150508      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025900150508
026000150508       //--------------------------------------------------------------
026100150508       //?Definizione key-list.                                        ?
026200150508       //--------------------------------------------------------------
026300150508
026400150508       // -?File TNTBE01L?
026500150508     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
026600150508     d                                     prefix(k_)   inz
026700150508
026800150508       //--------------------------------------------------------------
026900150508       //?Riepilogo indicatori utilizzati.                             ?
027000150508       //--------------------------------------------------------------
027100150508       //--------------------------------------------------------------
027200150508
027300150508       //--------------------------------------------------------------
027400150508       //?M A I N - L I N E                                            ?
027500150508       //--------------------------------------------------------------
027600150508
027700150508     c     *Entry        plist
027800150508     c                   parm                    KPJBA
027900150508
028000150508      /free
028100150508
028200150508       // -?Operazioni iniziali?
028300150508       exsr  sr_RoutInz;
028400150508
028500150508       // -?Ciclo di gestione del file video?
028600150508       DOW  $Fine = *off;
028700150508
028800150508         select;
028900150508
029000150508           // -?Gestione videata S1 (subfile)?
029100150508           when $Video = 'S1';
029200150508             exsr sr_GesS01;
029300150508
029400150508           // -?Manutenzione dati della singola tabella?
029500150508           when $Video = 'D2';
029600150508             exsr  sr_GesD02;
029700150508
029800150508           // -?Richiesta dati per trasmissione record?
029900150508           when $Video = 'W1';
030000150508             exsr  sr_GesW01;
030100150508
030200150508           // -?? ? ??
030300150508           other;
030400150508             $Fine = *on;
030500150508
030600150508         endsl;
030700150508
030800150508       ENDDO;
030900150508
031000150508       // -?Operazioni finali?
031100150508       exsr  sr_RoutEnd;
031200150508
031300150508       //--------------------------------------------------------------
031400150508       //?Operazioni iniziali.                                         ?
031500150508       //--------------------------------------------------------------
031600150508       BEGSR  sr_RoutInz;
031700150508
031800150508         // -?Impostazione chiusura?
031900150508         *inLR = *on;
032000150508
032100150508         // -?Reperimento dati job?
032200150508         exsr  sr_DatiJob;
032300150508
032400150508         // -?Reperimento data odierna?
032500150508         wDate = %dec( %date() );
032600150508
032700150508         // -?Impostazione nome programma a video?
032800150508         V1Tpgm = SDSpgm;
032900150508
033000150508         // -?Aggancio dati generali della tabella in esame?
033100150508         clear  keyTNTBE01;
033200150508         k_TBEke1 = *zero;
033300150508         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
033400150508                = c_Tab;
033500150508         k_TBEsif = KNSIF;
033600150508         chain(n)  %kds(keyTNTBE01)  TNTBE000;
033700150508         if  not  %found(TNTBE01L);
033800150508           clear  k_TBEsif;
033900150508           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
034000150508         endif;
034100150508         if  %found(TNTBE01L);
034200150508           xTNTBEds = TNTBEds;
034300150508         else;
034400150508           clear  xTNTBEds;
034500150508         endif;
034600150508
034700150508         // -?Verifica parametri ricevuti?
034800150508         $Called = ( KPJBU <> *blank );
034900150508
035000150508         // -?Pulizia parametri di output?
035100150508         clear  oRGRcod;
035200150508         clear  oRGRdes;
035300150508         clear  oRGRfxx;
035400150508         oRGRerr = *off;
035500150508         clear  oRGRmsg;
035600150508
035700150508         // -?Impostazione videata iniziale?
035800150508         $Video = 'S1';
035900150508         reset  $InzS01;
036000150508         $Ord_x_Des  = *off;
036100150508         $DisabilOpz = *off;
036200150508
036300150508         // -?Impostazione iniziale / pulizia dei campi chiave?
036400150508         clear  keyTNTBE01;
036500150508         k_TBEcod = c_Tab;
036600150508
036700150508       ENDSR;
036800150508
036900150508       //--------------------------------------------------------------
037000150508       //?Reperimento Dati del job (Utente/Operativi).                 ?
037100150508       //--------------------------------------------------------------
037200150508       BEGSR  sr_DatiJob;
037300150508
037400150508         in(E) §AzUte;
037500150508         if NOT %error;
037600150508           in(E) §DatiUte;
037700150508         endif;
037800150508         if %error or RSut = *blanks;
037900150508           clear TIBS34ds;
038000150508           tibs34r ( tibs34ds );
038100150508           in §AzUte;
038200150508           in §DatiUte;
038300150508         endif;
038400150508
038500150508       ENDSR;
038600150508
038700150508       //--------------------------------------------------------------
038800150508       //?Gestione subfile S01                                         ?
038900150508       //--------------------------------------------------------------
039000150508       BEGSR  sr_GesS01;
039100150508
039200150508         // -?Inizializzazione videata?
039300150508         if  $InzS01 = *on;
039400150508           exsr  sr_InzS01;
039500150508           $InzS01 = *off;
039600150508         endif;
039700150508
039800150508         // -?Emissione Piede con tasti funzionali abilitati (window)?
039900150508         write  TBRGRP01;
040000150508         // -?Emissione Testata?
040100150508         write  TBRGRT01;
040200150508
040300150508         // -?Posizionamento cursore?
040400150508         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
040500150508         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
040600150508         //  ?si visualizza la pagina che vedeva l'utente quando ha?
040700150508         //  ?premuto l'ultimo tasto.?
040800150508         if  C1CsrRrn > *zero;
040900150508           C1RcdNbr = C1CsrRrn;
041000150508         else;
041100150508           C1RcdNbr = 1;
041200150508           write  TBRGRS00;       //?(no rec.)?
041300150508         endif;
041400150508
041500150508         // -?Emissione videata (testata compresa) in window?
041600150508         exfmt  TBRGRC01;
041700150508
041800150508         clear  V1Dmsg;
041900150508         reset  $ErrMessage;
042000150508         reset  $ErrGenerico;
042100150508
042200150508         reset  $Immissione;
042300150508         reset  $Ripristino;
042400150508
042500150508         SELECT;
042600150508
042700150508           // -?F3=Fine?
042800150508           WHEN  dsp_aid = c_F03;
042900150508             oRGRfxx = '03';
043000150508             $Fine   = *on;
043100150508
043200150508           // -?F8=Cambio ordinamento?
043300150508           WHEN  dsp_aid = c_F08;
043400150508             exsr  sr_F08S01;
043500150508
043600150508           // -?F10=Inserimento Reparto Gestione R.A.?
043700150508           WHEN  dsp_aid = c_F10;
043800150508             $Immissione = *on;
043900150508             $Video  = 'D2';
044000150508             $InzD02 = *on;
044100150508
044200150508           // -?F12=Ritorno?
044300150508           WHEN  dsp_aid = c_F12;
044400150508             oRGRfxx = '12';
044500150508             $Fine   = *on;
044600150508
044700150508           // -?Subfile vuoto?
044800150508           WHEN  S01nrr = *zero;
044900150508
045000150508           // -?Invio?
045100150508           OTHER;
045200150508             // -?Gestione opzioni?
045300150508             exsr  sr_OpzS01;
045400150508             select;
045500150508               when  $ErrGenerico;
045600150508                 leavesr;
045700150508               when  wMemoSel > *zero;
045800150508                 $Fine = *on;
045900150508                 leavesr;
046000150508             endsl;
046100150508
046200150508         ENDSL;
046300150508
046400150508       ENDSR;
046500150508
046600150508       //--------------------------------------------------------------
046700150508       //?Inizializzazione subfile S01                                 ?
046800150508       //--------------------------------------------------------------
046900150508       BEGSR  sr_InzS01;
047000150508
047100150508         // -?Spegnimento degli indicatori di errore?
047200150508         %subst(IndDspF : 50) = *off;
047300150508
047400150508         // -?Pulizia subfile?
047500150508         $SflDsp_N    = *on;
047600150508         $SflDspCtl_N = *on;
047700150508         write  TBRGRC01;
047800150508         $SflDspCtl_N = *off;
047900150508         $SflEnd      = *off;
048000150508
048100150508         clear  C1RcdNbr;
048200150508         clear  C1CsrRrn;
048300150508         clear  S01nrr;
048400150508         clear  V1Dmsg;
048500150508         $ErrMessage  = *off;
048600150508         $ErrGenerico = *off;
048700150508
048800150508         $SflNxtChg = *off;
048900150508
049000150508         // -?Caricamento completo dei dati nel subfile (per codice)?
049100150508         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
049200150508         //  ?l'eventuale ordinamento)?
049300150508         clear  k_TBEke1;
049400150508         clear  k_TBEke2;
049500150508         clear  k_TBElin;
049600150508         clear  k_TBEsif;
049700150508         setll  %kds( keyTNTBE01 )  TNTBE000;
049800150508         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
049900150508
050000150508         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
050100150508           if  TBEatb = *blank;
050200150508             exsr  sr_WriteS01;
050300150508           endif;
050400150508           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
050500150508         EndDo;
050600150508
050700150508         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
050800150508         RrnLast   = S01nrr;
050900150508
051000150508         // -?Visualizzazione del SFL (se ci sono dati)?
051100150508         $SflDsp_N = ( S01nrr <= *zero );
051200150508
051300150508         // -?Attivazione del SFLEND?
051400150508         $SflEnd   = %eof(TNTBE01L);
051500150508
051600150508         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
051700150508         if  S01nrr > *zero;
051800150508           C1RcdNbr = 1;
051900150508         else;
052000150508           clear  C1RcdNbr;
052100150508         endif;
052200150508
052300150508         // -?Ordinamento subfile (se premuto F8)?
052400150508         if  $Ord_x_Des  and  S01nrr > 1;
052500150508           exsr  sr_SortSfl;
052600150508         endif;
052700150508
052800150508         // -?Impostazione Opzioni?
052900150508         if  $Called;
053000150508           C1Dopz = '1=Selezione';
053100150508         else;
053200150508           C1Dopz = '2=Modifica, 5=Visualizzazione';
053300150508         endif;
053400150508
053500150508         // -?(Dis)Abilitazione tasti funzionali?
053600150508         // -?F3 = Fine?
053700150508         $F3Attivo  = ( Not $Called );
053800150508         // -?F8 = Ordinamento x ...?
053900150508         $F8Attivo  = ( S01nrr > *zero );
054000150508         // -?F10 = Immissione?
054100150508         $F10Attivo = ( Not $Called );
054200150508         // -?F12 = Ritorno?
054300150508         $F12Attivo = ( $Called );
054400150508
054500150508         // -?Emissione messaggio di segnalazione raggiungimento limite?
054600150508         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
054700150508           $ErrGenerico = *on;
054800150508           $ErrMessage  = *on;
054900150508           $PosCurOPZ   = *on;
055000150508           V1Dmsg = sk_Msg(01);
055100150508           leavesr;
055200150508         endif;
055300150508
055400150508       ENDSR;
055500150508
055600150508       //--------------------------------------------------------------
055700150508       //?Registrazione del singolo rec. nel subfile S01               ?
055800150508       //--------------------------------------------------------------
055900150508       BEGSR  sr_WriteS01;
056000150508
056100150508         dRGR = TBEuni;
056200150508
056300150508         // -?Impostazione campi nel record del subfile?
056400150508         clear  TBRGRS01;
056500150508         S1Crgr  = %trimR( TBEke1 );
056600150508         S1Cdes  = dRGR.§RGRdes;
056700150508
056800150508         // -?Registrazione singolo record nel subfile?
056900150508         S01nrr += 1;
057000150508         write  TBRGRS01;
057100150508
057200150508       ENDSR;
057300150508
057400150508       //--------------------------------------------------------------
057500150508       //?Gestione opzioni del subfile S01                             ?
057600150508       //--------------------------------------------------------------
057700150508       BEGSR  sr_OpzS01;
057800150508
057900150508         // -?Spegnimento degli indicatori di errore?
058000150508         %subst(IndDspF : 50) = *off;
058100150508
058200150508         clear  SavS01csr;
058300150508         clear  wMemoSel;
058400150508
058500150508         // -?Ciclo di lettura subfile?
058600150508         readc  TBRGRS01;
058700150508
058800150508         DoW  Not %eof(TNTBRGRD);
058900150508
059000150508           $SflNxtChg = *off;
059100150508           %subst( IndDspF : 50 ) = *off;
059200150508           C1RcdNbr   = S01nrr;
059300150508
059400150508           Select;
059500150508
059600150508             // -?Nessuna opzione?
059700150508             When  S1Copz = *zero;
059800150508
059900150508             // -?Opz. 1=Selezione commerciale?
060000150508             When  $Called  and  S1Copz = 1;
060100150508               // -?Già selezionato un cliente?
060200150508               if  wMemoSel > *zero;
060300150508                 $ErrGenerico = *on;
060400150508                 $ErrMessage  = *on;
060500150508                 $PosCurOPZ   = *on;
060600150508                 V1Dmsg   = sk_Msg(03);
060700150508               else;
060800150508                 wMemoSel = S1Copz;
060900150508               endif;
061000150508
061100150508             // -?Opz. 2=Modifica?
061200150508             When  Not $Called  and  S1Copz = 2;
061300150508               $ProtectKEY = *on;
061400150508               $InzD02     = *on;
061500150508               $Video      = 'D2';
061600150508               DoW  $Video = 'D2';
061700150508                 exsr  sr_GesD02;
061800150508               EndDo;
061900150508               clear  $ProtectKEY;
062000150508
062100150508             // -?Opz. 5=Visualizzazione?
062200150508             When  Not $Called  and  S1Copz = 5;
062300150508               $Protect    = *on;
062400150508               $InzD02     = *on;
062500150508               $Video      = 'D2';
062600150508               DoW  $Video = 'D2';
062700150508                 exsr  sr_GesD02;
062800150508               EndDo;
062900150508               clear  $Protect;
063000150508
063100150508             // -?Opzione errata?
063200150508             Other;
063300150508               $ErrGenerico = *on;
063400150508               $ErrMessage  = *on;
063500150508               $PosCurOPZ   = *on;
063600150508               V1Dmsg = sk_Msg(02);
063700150508
063800150508           EndSl;
063900150508
064000150508           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
064100150508           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
064200150508           if  S1Copz   <> *zero  and
064300150508              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
064400150508             SavS01csr   = C1RcdNbr;
064500150508           endif;
064600150508
064700150508           // -?Aggiornamento sfl?
064800150508           select;
064900150508             when  $ErrMessage  or  S1Copz = 1;
065000150508               $SflNxtChg = *on;
065100150508               C1CsrRrn   = C1RcdNbr;
065200150508             when  $ErrGenerico;
065300150508               C1CsrRrn   = C1RcdNbr;
065400150508               if  S1Copz <> 1;
065500150508                 clear  S1Copz;          //?(se selezionato più di uno?)?
065600150508               endif;
065700150508             other;
065800150508               if  S1Copz <> 1;
065900150508                 clear  S1Copz;          //?(se selezionato più di uno?)?
066000150508               endif;
066100150508           endsl;
066200150508
066300150508           UPDATE  TBRGRS01;
066400150508
066500150508           if  $ErrGenerico  or  $ErrMessage;
066600150508             leave;
066700150508           endif;
066800150508
066900150508           readc  TBRGRS01;
067000150508
067100150508         ENDDO;
067200150508
067300150508         // -?Riposizionam. cursore sul record contenente la prima opz.?
067400150508         //  ?(se non sono stati rilevati errori)?
067500150508         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
067600150508             SavS01csr > *zero;
067700150508           C1CsrRrn = SavS01csr;
067800150508         endif;
067900150508
068000150508       ENDSR;
068100150508
068200150508       //--------------------------------------------------------------
068300150508       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
068400150508       //?F8 = Cambia ordinamento sfl                                  ?
068500150508       //--------------------------------------------------------------
068600150508       BEGSR  sr_F08S01;
068700150508
068800150508         if  Not $Ord_x_Des;
068900150508             $Ord_x_Des = *on;
069000150508         else;
069100150508           $Ord_x_Des = *off;
069200150508         endif;
069300150508
069400150508         // -?SubFile già completamente caricato: basta riordinarlo?
069500150508         exsr  sr_SortSfl;
069600150508
069700150508       ENDSR;
069800150508
069900150508       //--------------------------------------------------------------
070000150508       //?Ordinamento subfile                                          ?
070100150508       //?  This subroutine sorts the subfile records.                 ?
070200150508       //--------------------------------------------------------------
070300150508       BEGSR  sr_SortSfl;
070400150508
070500150508         // -?Impostazione NRR dell'ultimo record del subfile          ?
070600150508         //  ?(già fatto in sr_InzS01:                                 ?
070700150508         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
070800150508         //RrnLast  = S01nrr;
070900150508
071000150508         C1RcdNbr = 1;
071100150508         clear  C1CsrRrn;
071200150508         clear  S01nrr;
071300150508         clear  V1Dmsg;
071400150508         $SflNxtChg   = *off;
071500150508         $ErrMessage  = *off;
071600150508         $ErrGenerico = *off;
071700150508         %subst( IndDspF : 50 ) = *off;
071800150508
071900150508         //?___________________________________________________________?
072000150508         //?Initialize the key fields to sort on.?
072100150508         //?There is one extra field not in the subfile -- Selected --?
072200150508         //?that is added to the record. This field is used to place?
072300150508         //?selected records in front of nonselected records.?
072400150508         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
072500150508
072600150508         clear  QLGKL;
072700150508         clear  QLGSKL;
072800150508         clear  QLGSCB;
072900150508         clear  QLGSCB00;
073000150508
073100150508         // -?Gestione della scelta ordinamento:?
073200150508         Select;
073300150508
073400150508           // -?Ordinamento per Codice Reparto?
073500150508           when  Not $Ord_x_Des;
073600150508             // -?1 campo chiave?
073700150508             QLGNBRK  = 1;
073800150508             // -?1° campo: codice comando (S1Crgr)?
073900150508             //            ?a posizione  1, 10 byte, char, ascending.?
074000150508             QLGSP    = 1;
074100150508             QLGSS    = %size(S1Crgr);
074200150508             QLGDT    = c_Carattere;
074300150508             QLGSO    = c_Ascendente;
074400150508             QLGKL(1) = QLGSKL;
074500150508
074600150508           // -?Ordinamento per Descrizione Reparto?
074700150508           when  $Ord_x_Des;
074800150508             // -?2 campi chiave?
074900150508             QLGNBRK  = 2;
075000150508             // -?1° campo: descr. comando (S1Cdes)?
075100150508             //            ?a posizione 11, 30 byte, char, ascending.?
075200150508             QLGSP    = %size(S1Crgr) + 1;
075300150508             QLGSS    = %size(S1Cdes);
075400150508             QLGDT    = c_Carattere;
075500150508             QLGSO    = c_Ascendente;
075600150508             QLGKL(1) = QLGSKL;
075700150508             // -?2° campo: codice comando (S1Crgr)?
075800150508             //            ?a posizone  1, 10 byte, char, ascending.?
075900150508             QLGSP    = 1;
076000150508             QLGSS    = %size(S1Crgr);
076100150508             QLGDT    = c_Carattere;
076200150508             QLGSO    = c_Ascendente;
076300150508             QLGKL(2) = QLGSKL;
076400150508
076500150508         EndSl;
076600150508
076700150508         // -?Load other sort parameters.?
076800150508         QLGLB   = 80 + 16 * c_MaxKey;
076900150508         QLGRL   = %size(SflRcd) - 1;
077000150508         QLGRT   = 8;
077100150508         QLGOKL  = 80;
077200150508         QLGLKE  = 16;
077300150508         QLGLSS  = 290;
077400150508         // -?Initialize Sort I/O API fields.?
077500150508         QLGRL00 = QLGRL;
077600150508         QLGRC00 = 1;
077700150508         clear  QUSEI;
077800150508         QUSBPRV = %size(QUSEC);
077900150508
078000150508         // -?First step - Initialize the sort routine.?
078100150508         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
078200150508                      SizeList : ReturnSize : QUSEC );
078300150508
078400150508         // -?Next step - Write records to I/O routine.?
078500150508         QLGRT00  = c_Put;
078600150508         For  Nrr = 1  To  RrnLast;
078700150508           chain  Nrr  TBRGRS01;
078800150508           // -?Solo le righe con Selected = 'Y' sono riordinate,?
078900150508           //  ?quindi per fare un ordinamento di tutte le righe?
079000150508           //  ?metto 'Y' sempre.?
079100150508           Selected = 'Y';
079200150508           clear  QUSEI;
079300150508           QUSBPRV  = %size(QUSEC);
079400150508           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
079500150508                         SizeList : NotUsed : QUSEC );
079600150508         EndFor;
079700150508
079800150508         // -?Next step - Signal end of input, clear subfile for reload.?
079900150508         QLGRT00 = c_EndPut;
080000150508         clear  QUSEI;
080100150508         QUSBPRV = %size(QUSEC);
080200150508         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
080300150508                       SizeList : NotUsed : QUSEC );
080400150508
080500150508         // -?Pulizia subfile?
080600150508         $SflDsp_N    = *on;
080700150508         $SflDspCtl_N = *on;
080800150508         write  TBRGRC01;
080900150508         $SflDspCtl_N = *off;
081000150508         $SflEnd      = *off;
081100150508
081200150508         // -?Final step - Write the records back to the subfile.?
081300150508         QLGRT00  = c_Get;
081400150508         For  Nrr = 1  To RrnLast;
081500150508           clear  QUSEI;
081600150508           QUSBPRV = %size(QUSEC);
081700150508           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
081800150508                         QLGRL00  : NotUsed : QUSEC );
081900150508           // -?Ripristino indicatori utilizzati nel sfl rec?
082000150508           $SflNxtChg = ( S1Copz <> *zero );
082100150508           // -?e aggiorno?
082200150508           S01nrr = Nrr;
082300150508           write  TBRGRS01;
082400150508         EndFor;
082500150508
082600150508         C1CsrRrn = 1;
082700150508
082800150508         // -?Visualizzazione del SFL (se ci sono dati)?
082900150508         $SflDsp_N = ( S01nrr <= *zero );
083000150508
083100150508         // -?Attivazione del SFLEND?
083200150508         $SflEnd   = %eof(TNTBE01L);
083300150508
083400150508       ENDSR;
083500150508
083600150508
083700150508       //--------------------------------------------------------------
083800150508       //?Gestione videata D02.                                        ?
083900150508       //--------------------------------------------------------------
084000150508       BEGSR  sr_GesD02;
084100150508
084200150508         // -?Inizializzazione videata?
084300150508         if  $InzD02 = *on;
084400150508           exsr  sr_InzD02;
084500150508           $InzD02 = *off;
084600150508         endif;
084700150508
084800150508         // -?Emissione Testata e Piede con tasti funzionali abilitati?
084900150508         write  TBRGRP02;
085000150508         write  TBRGRT02;
085100150508
085200150508         // -?Emissione videata?
085300150508         if  Not $Protect;
085400150508           exfmt  TBRGRD02;
085500150508         else;
085600150508           write  TBRGRD02;
085700150508           exfmt  Protect;
085800150508         endif;
085900150508
086000150508         clear  V1Dmsg;
086100150508         reset  $ErrMessage;
086200150508         reset  $ErrGenerico;
086300150508
086400150508         SELECT;
086500150508
086600150508           // -?F3=Fine?
086700150508           WHEN  dsp_aid = c_F03;
086800150508             unlock TNTBE01L;
086900150508             $Video = 'S1';
087000150508             $Fine  = *on;
087100150508
087200150508           // -?F12=Ritorno?
087300150508           WHEN  dsp_aid = c_F12;
087400150508             unlock TNTBE01L;
087500150508             $Video = 'S1';
087600150508
087700150508           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
087800150508           OTHER;
087900150508             $Ripristino   = (dsp_aid = c_F05);
088000150508             $Annullamento = (dsp_aid = c_F16);
088100150508             // -?Controlli eseguiti NON se F16=Annullamento?
088200150508             if  Not $Annullamento;
088300150508               exsr  sr_CtrD02;
088400150508             // -?Controlli eseguiti SOLO se F16=Annullamento?
088500150508             //else;
088600150508             //  exsr  sr_CtrANN;
088700150508             endif;
088800150508             if  $ErrGenerico;
088900150508               leavesr;
089000150508             endif;
089100150508             // -?Aggiornamento?
089200150508             if  dsp_aid = c_F05   or
089300150508                 dsp_aid = c_F06   or
089400150508                 dsp_aid = c_F16;
089500150508               $Video = 'W1';
089600150508               reset  $InzW01;
089700150508               exsr  sr_GesW01;
089800150508             endif;
089900150508
090000150508         ENDSL;
090100150508
090200150508       ENDSR;
090300150508
090400150508       //--------------------------------------------------------------
090500150508       //?Inizializzazione videata D02.                                ?
090600150508       //--------------------------------------------------------------
090700150508       BEGSR  sr_InzD02;
090800150508
090900150508         // -?Spegnimento degli indicatori di errore?
091000150508         %subst(IndDspF : 50) = *off;
091100150508
091200150508         // -?Pulizia videata?
091300150508         clear  TBRGRD02;
091400150508
091500150508         // -?Impostazione testata?
091600150508         clear  V1Topz;
091700150508         Select;
091800150508           When  $Immissione;
091900150508             V1Topz = '  INSERIMENTO  ';
092000150508           When  TBEatb <> *blank;
092100150508             V1Topz = '  RIPRISTINO   ';
092200150508             $Ripristino = *on;
092300150508           Other;
092400150508             V1Topz = '   MODIFICA    ';
092500150508         EndSl;
092600150508
092700150508         // -?Reperimento dati del Reparto Gestione R.A.?
092800150508         //  ?(SE NON Immissione)?
092900150508         If  Not $Immissione;
093000150508
093100150508           // -?Reperimento dati?
093200150508           k_TBEke1 = S1Crgr;
093300150508           chain  %kds( KeyTNTBE01 )  TNTBE000;
093400150508
093500150508           // -?Caricamento dati del Codice?
093600150508           if  %found(TNTBE01L);
093700150508             exsr  sr_RieD02;
093800150508           endif;
093900150508
094000150508         EndiF;
094100150508
094200150508         // -?Impostazione indicatori per abilitazione tasti funzionali?
094300150508         exsr  sr_AbilitFxxD02;
094400150508
094500150508       ENDSR;
094600150508
094700150508       //--------------------------------------------------------------
094800150508       //?Caricamento dati del singolo record della tab. "CMD"         ?
094900150508       //?nella videata D02.                                           ?
095000150508       //--------------------------------------------------------------
095100150508       BEGSR  sr_RieD02;
095200150508
095300150508         dRGR = TBEuni;
095400150508
095500150508         V1Crgr  = TBEke1;
095600150508         V1Drgr  = dRGR.§RGRdes;
095700150508
095800150508       ENDSR;
095900150508
096000150508       //--------------------------------------------------------------
096100150508       //?Settaggio indicatori nella videata D02 per abilitazione      ?
096200150508       //?  tasti funzionali.                                          ?
096300150508       //--------------------------------------------------------------
096400150508       BEGSR  sr_AbilitFxxD02;
096500150508
096600150508         // -?F3 = Fine?
096700150508         $F3Attivo  = *off;
096800150508
096900150508         // -?F5 = Ripristino?
097000150508         $F5Attivo  = ( %found(TNTBE01L)  and  TBEatb <> *blank
097100150508                                          and  Not $Protect );
097200150508
097300150508         // -?F6 = Conferma?
097400150508         $F6Attivo  = ( NOT $F5Attivo  and  NOT $Protect );
097500150508
097600150508         // -?F12 = Ritorno?
097700150508         $F12Attivo = *on;
097800150508
097900150508         // -?F16 = Annullamento?
098000150508         $F16Attivo = ( %found(TNTBE01L)  and  TBEatb = *blank
098100150508                                          and  Not $Protect );
098200150508
098300150508       ENDSR;
098400150508
098500150508       //--------------------------------------------------------------
098600150508       //?Controllo dati videata D02.                                  ?
098700150508       //--------------------------------------------------------------
098800150508       BEGSR  sr_CtrD02;
098900150508
099000150508         // -?Spegnimento degli indicatori di errore?
099100150508         %subst(IndDspF : 50) = *off;
099200150508
099300150508         // -?Controllo immissione codice?
099400150508         If  V1Crgr = *blank;
099500150508           V1Dmsg = sk_Msg(04);
099600150508           $PosCurRGR   = *on;
099700150508           $ErrMessage  = *on;
099800150508           $ErrGenerico = *on;
099900150508           leavesr;
100000150508         EndIf;
100100150508
100200150508         // -?Verifica esistenza?
100300150508         If  $Immissione;
100400150508           k_TBEke1 = V1Crgr;
100500150508           setll  %kds( KeyTNTBE01 )  TNTBE000;
100600150508           if  %equal(TNTBE01L);
100700150508             V1Dmsg = sk_Msg(05);
100800150508             $PosCurRGR   = *on;
100900150508             $ErrMessage  = *on;
101000150508             $ErrGenerico = *on;
101100150508             leavesr;
101200150508           endif;
101300150508         EndIf;
101400150508
101500150508         // -?Descrizione obbligatoria?
101600150508         If  V1Drgr = *blank;
101700150508           V1Dmsg = sk_Msg(06);
101800150508           $PosCurDES   = *on;
101900150508           $ErrMessage  = *on;
102000150508           $ErrGenerico = *on;
102100150508           leavesr;
102200150508         EndIf;
102300150508
102400150508       ENDSR;
102500150508
102600150508       //--------------------------------------------------------------
102700150508       //?Gestione trasmissione aggiornamento                          ?
102800150508       //--------------------------------------------------------------
102900150508       BEGSR  sr_GesW01;
103000150508
103100150508         // -?Inizializzazione videata?
103200150508         if $InzW01 = *on;
103300150508           exsr  sr_InzW01;
103400150508           $InzW01 = *off;
103500150508         endif;
103600150508
103700150508         // -?Emissione window?
103800150508         if  TBXftt = 'S';
103900150508           exfmt  TBRGRW01;
104000150508           $ErrMessage  = *off;
104100150508           $ErrGenerico = *off;
104200150508           clear  V1Dmsg;
104300150508         else;
104400150508           dsp_aid = c_F06;
104500150508         endif;
104600150508
104700150508         select;
104800150508
104900150508           // -?F12=Ritorno (gestito solo se emesso W01)?
105000150508           when  dsp_aid = c_F12;
105100150508             $Video = 'D2';
105200150508
105300150508           // -?F6=Conferma?
105400150508           when  dsp_aid = c_F06;
105500150508             exsr  sr_UpdTNTBE;
105600150508             // -?Ritorno alla videata iniziale?
105700150508             if  NOT  $ErrGenerico;
105800150508               $Video  = 'S1';
105900150508               $InzS01 = *on;
106000150508             endif;
106100150508
106200150508         endsl;
106300150508
106400150508       ENDSR;
106500150508
106600150508       //--------------------------------------------------------------
106700150508       //?Inizializzazione videata W01                                 ?
106800150508       //--------------------------------------------------------------
106900150508       BEGSR  sr_InzW01;
107000150508
107100150508         clear TBRGRW01;
107200150508
107300150508         if  $Immissione;
107400150508
107500150508           // -?Se immissione?
107600150508           W1ftt  = TBXftt;
107700150508
107800150508         else;
107900150508
108000150508           // -?Se NON immissione:?
108100150508           //  ?visualizza i dati relativi all'ultima trasmissione?
108200150508           W1ftt  = TBEftt;
108300150508           W1flt  = TBEflt;
108400150508           W1ftr  = TBEftr;
108500150508           if TBEdtr <> *zero;
108600150508             wDate_EUR = %date(TBEdtr : *iso);
108700150508             W1dtr     = %dec(wDate_EUR);
108800150508           endif;
108900150508
109000150508         endif;
109100150508
109200150508       ENDSR;
109300150508
109400150508       //--------------------------------------------------------------
109500150508       //?Aggiornamento records TNTBE00F (tab. "RGR")                  ?
109600150508       //--------------------------------------------------------------
109700150508       BEGSR  sr_UpdTNTBE;
109800150508
109900150508         // -?Aggancio record di TNTBE00F?
110000150508         clear  keyTNTBE01;
110100150508         k_TBEcod  = c_Tab;
110200150508         k_TBEke1  = V1Crgr;
110300150508
110400150508         chain  %kds( KeyTNTBE01 )  TNTBE000;
110500150508
110600150508         clear  dRGR;
110700150508         dRGR.§RGRdes  = V1Drgr;
110800150508
110900150508         TBEuni = dRGR;
111000150508
111100150508         // -?Aggiornamento tab. "RGR"?
111200150508         SELECT;
111300150508
111400150508           // -?F5=Ripristino?
111500150508           WHEN  $Ripristino;
111600150508             clear  TBEatb;
111700150508             TBEftt = W1ftt;
111800150508             clear  TBEftr;
111900150508             //_______________
112000150508             UPDATE  TNTBE000;
112100150508             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112200150508
112300150508           // -?F16=Annullamento?
112400150508           WHEN  $Annullamento;
112500150508             TBEatb = 'A';
112600150508             TBEftt = W1ftt;
112700150508             clear  TBEftr;
112800150508             //_______________
112900150508             UPDATE  TNTBE000;
113000150508             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
113100150508
113200150508           // -?Immissione o Modifica?
113300150508           OTHER;
113400150508             if  NOT %found(TNTBE01L);
113500150508               clear TNTBE000;
113600150508               TBEcod  = k_TBEcod;
113700150508               TBEke1  = k_TBEke1;
113800150508               TBEke2  = k_TBEke2;
113900150508               TBElin  = k_TBElin;
114000150508               TBEsif  = k_TBEsif;
114100150508               TBEuni  = dRGR;
114200150508             endif;
114300150508             TBEapl = TBXapl;
114400150508             clear  TBEatb;
114500150508             TBEftt = W1ftt;
114600150508             clear  TBEflt;
114700150508             clear  TBEftr;
114800150508             //clear TBEdtr;
114900150508             // -?Aggiornamento record?
115000150508             if  %found(TNTBE01L);
115100150508               //_____________
115200150508               UPDATE TNTBE000;
115300150508               //¯¯¯¯¯¯¯¯¯¯¯¯¯
115400150508             else;
115500150508               //_____________
115600150508               WRITE  TNTBE000;
115700150508               //¯¯¯¯¯¯¯¯¯¯¯¯¯
115800150508             endif;
115900150508
116000150508         ENDSL;
116100150508
116200150508       ENDSR;
116300150508
116400150508       //--------------------------------------------------------------
116500150508       //?Operazioni finali.                                           ?
116600150508       //--------------------------------------------------------------
116700150508       BEGSR  sr_RoutEnd;
116800150508
116900150508         // -?Chiusura funzioni precedentemente aperte?
117000150508         clear  TIBS02ds;
117100150508         T02tla = 'C';
117200150508         TNTBE_RicercaControllo ( kpjba : tibs02ds );
117300150508
117400150508         // -?Restituzione parametri?
117500150508         //  ?(se selezionato un Reparto Gestione R.A.)?
117600150508         If  wMemoSel > *zero;
117700150508
117800150508           chain  SavS01csr  TBRGRS01;
117900150508
118000150508           oRGRcod  = S1Crgr;
118100150508           oRGRdes  = S1Cdes;
118200150508
118300150508         EndIf;
118400150508
118500150508         kpjbu = tntbRGRds;
118600150508
118700150508         //  ?Uscita?
118800150508         return;
118900150508
119000150508       ENDSR;
119100150508
119200150508      /end-free
119300150508
119400150508       //--------------------------------------------------------------
119500150508       //?Definizione schiere a tempo di compilazione.                 ?
119600150508       //--------------------------------------------------------------
119700150508
119800150508** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
119900150508Raggiunto il limite massimo dei record caricabili a video                      1
120000150508Opzione errata                                                                 2
120100150508Selezionati più di un Reparto                                                  3
120200150508Immettere il Reparto Gestione R.A.                                             4
120300150508Reparto già esistente                                                          5
120400150508Descrizione obbligatoria                                                       6
