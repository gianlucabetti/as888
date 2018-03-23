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
017000150508       // -?Variabili per la gestione del posizionamento nel subfile?
017100150508     d SavC1Crgr       s                   like(C1Crgr)   inz
017200150508     d SavC1Cdes       s                   like(C1Cdes)   inz
017300150508
017400150508       // -?Conteggio selezioni (opz. 1) inserite?
017500150508     d wMemoSel        s                   like(S1Copz)   inz
017600150508
017700150508       // -?Campi di comodo?
017800150508     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
017900150508     d wDate           s              8s 0 inz
018000150508
018100150508       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018200150508
018300150508       //--------------------------------------------------------------
018400150508       // -?Constants?
018500150508       //--------------------------------------------------------------
018600150508       // -?MaxKey - Maximum number of key fields allowed by this program?
018700150508     d c_MaxKey        c                   const(2)
018800150508       // -?Sort order:?
018900150508       //  ?1 = Sort in an ascending sequence?
019000150508       //  ?2 = Sort in a descending sequence?
019100150508     d c_Ascendente    c                   const(1)
019200150508     d c_Discendente   c                   const(2)
019300150508       // -?Key data type:?
019400150508       //  ? 0 = Signed binary?
019500150508       //  ? 2 = Signed zoned decimal?
019600150508       //  ? 3 = Signed packed decimal?
019700150508       //  ? 6 = Character with no national language sort sequence applied,?
019800150508       //  ?     if specified?
019900150508       //  ? 7 = Unsigned packed decimal?
020000150508       //  ?     All numbers will have the sign forced positive ('F'X).?
020100150508       //  ? 8 = Unsigned zoned decimal?
020200150508       //  ?     All numbers will have the sign forced positive ('F'X).?
020300150508       //  ? 9 = Unsigned binary?
020400150508       //  ?14 = Date in form of DD/MM/YY?
020500150508       //  ?15 = Date in form of DD.MM.YYYY?
020600150508     d c_Numero        c                   const(2)
020700150508     d c_Carattere     c                   const(6)
020800150508     d c_NumIntero     c                   const(8)
020900150508       //
021000150508     d c_Put           c                   const(1)
021100150508     d c_EndPut        c                   const(2)
021200150508     d c_Get           c                   const(3)
021300150508
021400150508       //--------------------------------------------------------------
021500150508       // -?Data Structures?
021600150508       //  ?SflRcd     - The entire subfile record to pass to the sort?
021700150508       //  ?QLGSCB     - The sort request block for the QLGSORT API?
021800150508       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
021900150508       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
022000150508       //  ?QUSEC      - Error structure for the QLGSORT API?
022100150508       //--------------------------------------------------------------
022200150508     d SflRcd          ds                  inz
022300150508     d   S1Crgr                            inz
022400150508     d   S1Cdes                            inz
022500150508     d   S1Copz                            inz
022600150508     d   Selected                     1a   inz
022700150508      /copy QSYSINC/QRPGLESRC,QLGSORT
022800150508     d QLGKL                         16    dim(c_MaxKey)
022900150508     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023000150508     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023100150508     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023200150508     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023300150508      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023400150508      /copy QSYSINC/QRPGLESRC,QUSEC
023500150508
023600150508       //--------------------------------------------------------------
023700150508       // -?Standalone fields?
023800150508       //  ?Nrr        - Relative record number for subfile?
023900150508       //  ?SizeList   - Size of list?
024000150508       //  ?ReturnSize - Size of list returned by sort API?
024100150508       //--------------------------------------------------------------
024200150508     d NotUsed         s             16a   inz
024300150508     d ReturnSize      s              9b 0 inz
024400150508     d SizeList        s              9b 0 inz
024500150508     d RrnLast         s              5i 0 inz
024600150508     d Nrr             s              5i 0 inz
024700150508
024800150508       //--------------------------------------------------------------
024900150508       //?Definizione prototipi procedure.                             ?
025000150508       //--------------------------------------------------------------
025100150508
025200150508       // -?Reperimento dati utente?
025300150508     d TIBS34ds      e ds
025400150508      /copy gaitrasrc/srcProtoPR,TIBS34R
025500150508
025600150508       // -?Ricerca e controllo nuove tabelle?
025700150508     d TIBS02ds      e ds                  inz
025800150508     d   T02mod      e                     inz('C')
025900150508      /copy gaitrasrc/srcProtoPR,TIBS02R
026000150508
026100150508       // -?Ordinamento subfile?
026200150508      /copy gaitrasrc/srcProtoPr,QLGSRTIO
026300150508
026400150508       //--------------------------------------------------------------
026500150508       //?Definizione key-list.                                        ?
026600150508       //--------------------------------------------------------------
026700150508
026800150508       // -?File TNTBE01L?
026900150508     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
027000150508     d                                     prefix(k_)   inz
027100150508
027200150508       //--------------------------------------------------------------
027300150508       //?Riepilogo indicatori utilizzati.                             ?
027400150508       //--------------------------------------------------------------
027500150508       //--------------------------------------------------------------
027600150508
027700150508       //--------------------------------------------------------------
027800150508       //?M A I N - L I N E                                            ?
027900150508       //--------------------------------------------------------------
028000150508
028100150508     c     *Entry        plist
028200150508     c                   parm                    KPJBA
028300150508
028400150508      /free
028500150508
028600150508       // -?Operazioni iniziali?
028700150508       exsr  sr_RoutInz;
028800150508
028900150508       // -?Ciclo di gestione del file video?
029000150508       DOW  $Fine = *off;
029100150508
029200150508         select;
029300150508
029400150508           // -?Gestione videata S1 (subfile)?
029500150508           when $Video = 'S1';
029600150508             exsr sr_GesS01;
029700150508
029800150508           // -?Manutenzione dati della singola tabella?
029900150508           when $Video = 'D2';
030000150508             exsr  sr_GesD02;
030100150508
030200150508           // -?Richiesta dati per trasmissione record?
030300150508           when $Video = 'W1';
030400150508             exsr  sr_GesW01;
030500150508
030600150508           // -?? ? ??
030700150508           other;
030800150508             $Fine = *on;
030900150508
031000150508         endsl;
031100150508
031200150508       ENDDO;
031300150508
031400150508       // -?Operazioni finali?
031500150508       exsr  sr_RoutEnd;
031600150508
031700150508       //--------------------------------------------------------------
031800150508       //?Operazioni iniziali.                                         ?
031900150508       //--------------------------------------------------------------
032000150508       BEGSR  sr_RoutInz;
032100150508
032200150508         // -?Impostazione chiusura?
032300150508         *inLR = *on;
032400150508
032500150508         // -?Reperimento dati job?
032600150508         exsr  sr_DatiJob;
032700150508
032800150508         // -?Reperimento data odierna?
032900150508         wDate = %dec( %date() );
033000150508
033100150508         // -?Impostazione nome programma a video?
033200150508         V1Tpgm = SDSpgm;
033300150508
033400150508         // -?Aggancio dati generali della tabella in esame?
033500150508         clear  keyTNTBE01;
033600150508         k_TBEke1 = *zero;
033700150508         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
033800150508                = c_Tab;
033900150508         k_TBEsif = KNSIF;
034000150508         chain(n)  %kds(keyTNTBE01)  TNTBE000;
034100150508         if  not  %found(TNTBE01L);
034200150508           clear  k_TBEsif;
034300150508           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
034400150508         endif;
034500150508         if  %found(TNTBE01L);
034600150508           xTNTBEds = TNTBEds;
034700150508         else;
034800150508           clear  xTNTBEds;
034900150508         endif;
035000150508
035100150508         // -?Verifica parametri ricevuti?
035200150508         $Called = ( KPJBU <> *blank );
035300150508
035400150508         // -?Pulizia parametri di output?
035500150508         clear  oRGRcod;
035600150508         clear  oRGRdes;
035700150508         clear  oRGRfxx;
035800150508         oRGRerr = *off;
035900150508         clear  oRGRmsg;
036000150508
036100150508         // -?Impostazione videata iniziale?
036200150508         $Video = 'S1';
036300150508         reset  $InzS01;
036400150508         $Ord_x_Des  = *off;
036500150508         $DisabilOpz = *off;
036600150508
036700150508         // -?Impostazione iniziale / pulizia dei campi chiave?
036800150508         clear  keyTNTBE01;
036900150508         k_TBEcod = c_Tab;
037000150508
037100150508       ENDSR;
037200150508
037300150508       //--------------------------------------------------------------
037400150508       //?Reperimento Dati del job (Utente/Operativi).                 ?
037500150508       //--------------------------------------------------------------
037600150508       BEGSR  sr_DatiJob;
037700150508
037800150508         in(E) §AzUte;
037900150508         if NOT %error;
038000150508           in(E) §DatiUte;
038100150508         endif;
038200150508         if %error or RSut = *blanks;
038300150508           clear TIBS34ds;
038400150508           tibs34r ( tibs34ds );
038500150508           in §AzUte;
038600150508           in §DatiUte;
038700150508         endif;
038800150508
038900150508       ENDSR;
039000150508
039100150508       //--------------------------------------------------------------
039200150508       //?Gestione subfile S01                                         ?
039300150508       //--------------------------------------------------------------
039400150508       BEGSR  sr_GesS01;
039500150508
039600150508         // -?Inizializzazione videata?
039700150508         if  $InzS01 = *on;
039800150508           exsr  sr_InzS01;
039900150508           $InzS01 = *off;
040000150508         endif;
040100150508
040200150508         // -?Emissione Piede con tasti funzionali abilitati (window)?
040300150508         write  TBRGRP01;
040400150508         // -?Emissione Testata?
040500150508         write  TBRGRT01;
040600150508
040700150508         // -?Posizionamento cursore?
040800150508         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
040900150508         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
041000150508         //  ?si visualizza la pagina che vedeva l'utente quando ha?
041100150508         //  ?premuto l'ultimo tasto.?
041200150508         if  C1CsrRrn > *zero;
041300150508           C1RcdNbr = C1CsrRrn;
041400150508         else;
041500150508           C1RcdNbr = 1;
041600150508           write  TBRGRS00;       //?(no rec.)?
041700150508         endif;
041800150508
041900150508         // -?Emissione videata (testata compresa) in window?
042000150508         exfmt  TBRGRC01;
042100150508
042200150508         clear  V1Dmsg;
042300150508         reset  $ErrMessage;
042400150508         reset  $ErrGenerico;
042500150508
042600150508         reset  $Immissione;
042700150508         reset  $Ripristino;
042800150508
042900150508         SELECT;
043000150508
043100150508           // -?F3=Fine?
043200150508           WHEN  dsp_aid = c_F03;
043300150508             oRGRfxx = '03';
043400150508             $Fine   = *on;
043500150508
043600150508           // -?F8=Cambio ordinamento?
043700150508           WHEN  dsp_aid = c_F08;
043800150508             exsr  sr_F08S01;
043900150508
044000150508           // -?F10=Inserimento Reparto Gestione R.A.?
044100150508           WHEN  dsp_aid = c_F10;
044200150508             $Immissione = *on;
044300150508             $Video  = 'D2';
044400150508             $InzD02 = *on;
044500150508
044600150508           // -?F12=Ritorno?
044700150508           WHEN  dsp_aid = c_F12;
044800150508             oRGRfxx = '12';
044900150508             $Fine   = *on;
045000150508
045100150508           // -?Subfile vuoto?
045200150508           WHEN  S01nrr = *zero;
045300150508             if  C1Crgr  <> SavC1Crgr   or
045400150508                 C1Cdes  <> SavC1Cdes;
045500150508               $InzS01 = *on;
045600150508             endif;
045700150508
045800150508           // -?Invio?
045900150508           OTHER;
046000150508             // -?Gestione opzioni?
046100150508             exsr  sr_OpzS01;
046200150508             select;
046300150508               when  $ErrGenerico;
046400150508                 leavesr;
046500150508               when  wMemoSel > *zero;
046600150508                 $Fine = *on;
046700150508                 leavesr;
046800150508             endsl;
046900150508             // -?Richiesto Posizionamento?
047000150508             if  ( Not $Ord_x_Des  and  C1Crgr <> SavC1Crgr )  or
047100150508                 ( $Ord_x_Des      and  C1Cdes <> SavC1Cdes );
047200150508               $InzS01 = *on;
047300150508               SavC1Crgr = C1Crgr;
047400150508               SavC1Cdes = C1Cdes;
047500150508             endif;
047600150508
047700150508         ENDSL;
047800150508
047900150508       ENDSR;
048000150508
048100150508       //--------------------------------------------------------------
048200150508       //?Inizializzazione subfile S01                                 ?
048300150508       //--------------------------------------------------------------
048400150508       BEGSR  sr_InzS01;
048500150508
048600150508         // -?Spegnimento degli indicatori di errore?
048700150508         %subst( IndDspF : 50 ) = *off;
048800150508
048900150508         // -?Pulizia subfile?
049000150508         $SflDsp_N    = *on;
049100150508         $SflDspCtl_N = *on;
049200150508         write  TBRGRC01;
049300150508         $SflDspCtl_N = *off;
049400150508         $SflEnd      = *off;
049500150508
049600150508         clear  C1RcdNbr;
049700150508         clear  C1CsrRrn;
049800150508         clear  S01nrr;
049900150508         clear  V1Dmsg;
050000150508         $ErrMessage  = *off;
050100150508         $ErrGenerico = *off;
050200150508
050300150508         $SflNxtChg = *off;
050400150508
050500150508         // -?Caricamento completo dei dati nel subfile (per codice)?
050600150508         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
050700150508         //  ?l'eventuale ordinamento)?
050800150508         clear  k_TBEke1;
050900150508         if  Not  $Ord_x_Des;
051000150508           k_TBEke1 = C1Crgr;
051100150508         endif;
051200150508         clear  k_TBEke2;
051300150508         clear  k_TBElin;
051400150508         clear  k_TBEsif;
051500150508         setll  %kds( keyTNTBE01 )  TNTBE000;
051600150508         reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
051700150508
051800150508         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
051900150508           if  TBEatb = *blank;
052000150508             exsr  sr_WriteS01;
052100150508           endif;
052200150508           reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
052300150508         EndDo;
052400150508
052500150508         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
052600150508         RrnLast   = S01nrr;
052700150508
052800150508         // -?Visualizzazione del SFL (se ci sono dati)?
052900150508         $SflDsp_N = ( S01nrr <= *zero );
053000150508
053100150508         // -?Attivazione del SFLEND?
053200150508         $SflEnd   = %eof(TNTBE01L);
053300150508
053400150508         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
053500150508         if  S01nrr > *zero;
053600150508           C1RcdNbr = 1;
053700150508         else;
053800150508           clear  C1RcdNbr;
053900150508         endif;
054000150508
054100150508         // -?Ordinamento subfile (se premuto F8)?
054200150508         if  $Ord_x_Des  and  S01nrr > 1;
054300150508           exsr  sr_SortSfl;
054400150508         endif;
054500150508
054600150508         // -?Impostazione Opzioni?
054700150508         if  $Called;
054800150508           C1Dopz = '1=Selezione';
054900150508         else;
055000150508           C1Dopz = '2=Modifica, 5=Visualizzazione';
055100150508         endif;
055200150508
055300150508         // -?(Dis)Abilitazione tasti funzionali?
055400150508         // -?F3 = Fine?
055500150508         $F3Attivo  = ( Not $Called );
055600150508         // -?F8 = Ordinamento x ...?
055700150508         $F8Attivo  = ( S01nrr > *zero );
055800150508         // -?F10 = Immissione?
055900150508         $F10Attivo = ( Not $Called );
056000150508         // -?F12 = Ritorno?
056100150508         $F12Attivo = ( $Called );
056200150508
056300150508         // -?Emissione messaggio di segnalazione raggiungimento limite?
056400150508         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
056500150508           $ErrGenerico = *on;
056600150508           $ErrMessage  = *on;
056700150508           $PosCurOPZ   = *on;
056800150508           V1Dmsg = sk_Msg(01);
056900150508           leavesr;
057000150508         endif;
057100150508
057200150508       ENDSR;
057300150508
057400150508       //--------------------------------------------------------------
057500150508       //?Registrazione del singolo rec. nel subfile S01               ?
057600150508       //--------------------------------------------------------------
057700150508       BEGSR  sr_WriteS01;
057800150508
057900150508         // -?Eventuale posizionamento richiesto?
058000150508         select;
058100150508           when  Not $Ord_x_Des  and  TBEcod < C1Crgr;
058200150508             leavesr;
058300150508           when  $Ord_x_Des      and  TBEuni < C1Cdes;
058400150508             leavesr;
058500150508         endsl;
058600150508
058700150508         dRGR = TBEuni;
058800150508
058900150508         // -?Impostazione campi nel record del subfile?
059000150508         clear  TBRGRS01;
059100150508         S1Crgr  = %trimR( TBEke1 );
059200150508         S1Cdes  = dRGR.§RGRdes;
059300150508
059400150508         // -?Registrazione singolo record nel subfile?
059500150508         S01nrr += 1;
059600150508         write  TBRGRS01;
059700150508
059800150508       ENDSR;
059900150508
060000150508       //--------------------------------------------------------------
060100150508       //?Gestione opzioni del subfile S01                             ?
060200150508       //--------------------------------------------------------------
060300150508       BEGSR  sr_OpzS01;
060400150508
060500150508         // -?Spegnimento degli indicatori di errore?
060600150508         %subst(IndDspF : 50) = *off;
060700150508
060800150508         clear  SavS01csr;
060900150508         clear  wMemoSel;
061000150508
061100150508         // -?Ciclo di lettura subfile?
061200150508         readc  TBRGRS01;
061300150508
061400150508         DoW  Not %eof(TNTBRGRD);
061500150508
061600150508           $SflNxtChg = *off;
061700150508           %subst( IndDspF : 50 ) = *off;
061800150508           C1RcdNbr   = S01nrr;
061900150508
062000150508           Select;
062100150508
062200150508             // -?Nessuna opzione?
062300150508             When  S1Copz = *zero;
062400150508
062500150508             // -?Opz. 1=Selezione commerciale?
062600150508             When  $Called  and  S1Copz = 1;
062700150508               // -?Già selezionato un cliente?
062800150508               if  wMemoSel > *zero;
062900150508                 $ErrGenerico = *on;
063000150508                 $ErrMessage  = *on;
063100150508                 $PosCurOPZ   = *on;
063200150508                 V1Dmsg   = sk_Msg(03);
063300150508               else;
063400150508                 wMemoSel = S1Copz;
063500150508               endif;
063600150508
063700150508             // -?Opz. 2=Modifica?
063800150508             When  Not $Called  and  S1Copz = 2;
063900150508               $ProtectKEY = *on;
064000150508               $InzD02     = *on;
064100150508               $Video      = 'D2';
064200150508               DoW  $Video = 'D2';
064300150508                 exsr  sr_GesD02;
064400150508               EndDo;
064500150508               clear  $ProtectKEY;
064600150508
064700150508             // -?Opz. 5=Visualizzazione?
064800150508             When  Not $Called  and  S1Copz = 5;
064900150508               $Protect    = *on;
065000150508               $InzD02     = *on;
065100150508               $Video      = 'D2';
065200150508               DoW  $Video = 'D2';
065300150508                 exsr  sr_GesD02;
065400150508               EndDo;
065500150508               clear  $Protect;
065600150508
065700150508             // -?Opzione errata?
065800150508             Other;
065900150508               $ErrGenerico = *on;
066000150508               $ErrMessage  = *on;
066100150508               $PosCurOPZ   = *on;
066200150508               V1Dmsg = sk_Msg(02);
066300150508
066400150508           EndSl;
066500150508
066600150508           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
066700150508           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
066800150508           if  S1Copz   <> *zero  and
066900150508              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
067000150508             SavS01csr   = C1RcdNbr;
067100150508           endif;
067200150508
067300150508           // -?Aggiornamento sfl?
067400150508           select;
067500150508             when  $ErrMessage  or  S1Copz = 1;
067600150508               $SflNxtChg = *on;
067700150508               C1CsrRrn   = C1RcdNbr;
067800150508             when  $ErrGenerico;
067900150508               C1CsrRrn   = C1RcdNbr;
068000150508               if  S1Copz <> 1;
068100150508                 clear  S1Copz;          //?(se selezionato più di uno?)?
068200150508               endif;
068300150508             other;
068400150508               if  S1Copz <> 1;
068500150508                 clear  S1Copz;          //?(se selezionato più di uno?)?
068600150508               endif;
068700150508           endsl;
068800150508
068900150508           UPDATE  TBRGRS01;
069000150508
069100150508           if  $ErrGenerico  or  $ErrMessage;
069200150508             leave;
069300150508           endif;
069400150508
069500150508           readc  TBRGRS01;
069600150508
069700150508         ENDDO;
069800150508
069900150508         // -?Riposizionam. cursore sul record contenente la prima opz.?
070000150508         //  ?(se non sono stati rilevati errori)?
070100150508         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
070200150508             SavS01csr > *zero;
070300150508           C1CsrRrn = SavS01csr;
070400150508         endif;
070500150508
070600150508       ENDSR;
070700150508
070800150508       //--------------------------------------------------------------
070900150508       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
071000150508       //?F8 = Cambia ordinamento sfl                                  ?
071100150508       //--------------------------------------------------------------
071200150508       BEGSR  sr_F08S01;
071300150508
071400150508         if  Not $Ord_x_Des;
071500150508             $Ord_x_Des = *on;
071600150508         else;
071700150508           $Ord_x_Des = *off;
071800150508         endif;
071900150508
072000150508         //// -?SubFile già completamente caricato: basta riordinarlo?
072100150508         //exsr  sr_SortSfl;
072200150508         // -?Potrebbero essere state richiesti posizionamenti:?
072300150508         //  ?conviene ricaricare il subfile?
072400150508         exsr  sr_InzS01;
072500150508
072600150508       ENDSR;
072700150508
072800150508       //--------------------------------------------------------------
072900150508       //?Ordinamento subfile                                          ?
073000150508       //?  This subroutine sorts the subfile records.                 ?
073100150508       //--------------------------------------------------------------
073200150508       BEGSR  sr_SortSfl;
073300150508
073400150508         // -?Impostazione NRR dell'ultimo record del subfile          ?
073500150508         //  ?(già fatto in sr_InzS01:                                 ?
073600150508         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
073700150508         //RrnLast  = S01nrr;
073800150508
073900150508         C1RcdNbr = 1;
074000150508         clear  C1CsrRrn;
074100150508         clear  S01nrr;
074200150508         clear  V1Dmsg;
074300150508         $SflNxtChg   = *off;
074400150508         $ErrMessage  = *off;
074500150508         $ErrGenerico = *off;
074600150508         %subst( IndDspF : 50 ) = *off;
074700150508
074800150508         //?___________________________________________________________?
074900150508         //?Initialize the key fields to sort on.?
075000150508         //?There is one extra field not in the subfile -- Selected --?
075100150508         //?that is added to the record. This field is used to place?
075200150508         //?selected records in front of nonselected records.?
075300150508         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
075400150508
075500150508         clear  QLGKL;
075600150508         clear  QLGSKL;
075700150508         clear  QLGSCB;
075800150508         clear  QLGSCB00;
075900150508
076000150508         // -?Gestione della scelta ordinamento:?
076100150508         Select;
076200150508
076300150508           // -?Ordinamento per Codice Reparto?
076400150508           when  Not $Ord_x_Des;
076500150508             // -?1 campo chiave?
076600150508             QLGNBRK  = 1;
076700150508             // -?1° campo: codice comando (S1Crgr)?
076800150508             //            ?a posizione  1, 10 byte, char, ascending.?
076900150508             QLGSP    = 1;
077000150508             QLGSS    = %size(S1Crgr);
077100150508             QLGDT    = c_Carattere;
077200150508             QLGSO    = c_Ascendente;
077300150508             QLGKL(1) = QLGSKL;
077400150508
077500150508           // -?Ordinamento per Descrizione Reparto?
077600150508           when  $Ord_x_Des;
077700150508             // -?2 campi chiave?
077800150508             QLGNBRK  = 2;
077900150508             // -?1° campo: descr. comando (S1Cdes)?
078000150508             //            ?a posizione 11, 30 byte, char, ascending.?
078100150508             QLGSP    = %size(S1Crgr) + 1;
078200150508             QLGSS    = %size(S1Cdes);
078300150508             QLGDT    = c_Carattere;
078400150508             QLGSO    = c_Ascendente;
078500150508             QLGKL(1) = QLGSKL;
078600150508             // -?2° campo: codice comando (S1Crgr)?
078700150508             //            ?a posizone  1, 10 byte, char, ascending.?
078800150508             QLGSP    = 1;
078900150508             QLGSS    = %size(S1Crgr);
079000150508             QLGDT    = c_Carattere;
079100150508             QLGSO    = c_Ascendente;
079200150508             QLGKL(2) = QLGSKL;
079300150508
079400150508         EndSl;
079500150508
079600150508         // -?Load other sort parameters.?
079700150508         QLGLB   = 80 + 16 * c_MaxKey;
079800150508         QLGRL   = %size(SflRcd) - 1;
079900150508         QLGRT   = 8;
080000150508         QLGOKL  = 80;
080100150508         QLGLKE  = 16;
080200150508         QLGLSS  = 290;
080300150508         // -?Initialize Sort I/O API fields.?
080400150508         QLGRL00 = QLGRL;
080500150508         QLGRC00 = 1;
080600150508         clear  QUSEI;
080700150508         QUSBPRV = %size(QUSEC);
080800150508
080900150508         // -?First step - Initialize the sort routine.?
081000150508         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
081100150508                      SizeList : ReturnSize : QUSEC );
081200150508
081300150508         // -?Next step - Write records to I/O routine.?
081400150508         QLGRT00  = c_Put;
081500150508         For  Nrr = 1  To  RrnLast;
081600150508           chain  Nrr  TBRGRS01;
081700150508           // -?Solo le righe con Selected = 'Y' sono riordinate,?
081800150508           //  ?quindi per fare un ordinamento di tutte le righe?
081900150508           //  ?metto 'Y' sempre.?
082000150508           Selected = 'Y';
082100150508           clear  QUSEI;
082200150508           QUSBPRV  = %size(QUSEC);
082300150508           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
082400150508                         SizeList : NotUsed : QUSEC );
082500150508         EndFor;
082600150508
082700150508         // -?Next step - Signal end of input, clear subfile for reload.?
082800150508         QLGRT00 = c_EndPut;
082900150508         clear  QUSEI;
083000150508         QUSBPRV = %size(QUSEC);
083100150508         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
083200150508                       SizeList : NotUsed : QUSEC );
083300150508
083400150508         // -?Pulizia subfile?
083500150508         $SflDsp_N    = *on;
083600150508         $SflDspCtl_N = *on;
083700150508         write  TBRGRC01;
083800150508         $SflDspCtl_N = *off;
083900150508         $SflEnd      = *off;
084000150508
084100150508         // -?Final step - Write the records back to the subfile.?
084200150508         QLGRT00  = c_Get;
084300150508         For  Nrr = 1  To RrnLast;
084400150508           clear  QUSEI;
084500150508           QUSBPRV = %size(QUSEC);
084600150508           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
084700150508                         QLGRL00  : NotUsed : QUSEC );
084800150508           // -?Ripristino indicatori utilizzati nel sfl rec?
084900150508           $SflNxtChg = ( S1Copz <> *zero );
085000150508           // -?e aggiorno?
085100150508           S01nrr = Nrr;
085200150508           write  TBRGRS01;
085300150508         EndFor;
085400150508
085500150508         C1CsrRrn = 1;
085600150508
085700150508         // -?Visualizzazione del SFL (se ci sono dati)?
085800150508         $SflDsp_N = ( S01nrr <= *zero );
085900150508
086000150508         // -?Attivazione del SFLEND?
086100150508         $SflEnd   = %eof(TNTBE01L);
086200150508
086300150508       ENDSR;
086400150508
086500150508
086600150508       //--------------------------------------------------------------
086700150508       //?Gestione videata D02.                                        ?
086800150508       //--------------------------------------------------------------
086900150508       BEGSR  sr_GesD02;
087000150508
087100150508         // -?Inizializzazione videata?
087200150508         if  $InzD02 = *on;
087300150508           exsr  sr_InzD02;
087400150508           $InzD02 = *off;
087500150508         endif;
087600150508
087700150508         // -?Emissione Testata e Piede con tasti funzionali abilitati?
087800150508         write  TBRGRP02;
087900150508         write  TBRGRT02;
088000150508
088100150508         // -?Emissione videata?
088200150508         if  Not $Protect;
088300150508           exfmt  TBRGRD02;
088400150508         else;
088500150508           write  TBRGRD02;
088600150508           exfmt  Protect;
088700150508         endif;
088800150508
088900150508         clear  V1Dmsg;
089000150508         reset  $ErrMessage;
089100150508         reset  $ErrGenerico;
089200150508
089300150508         SELECT;
089400150508
089500150508           // -?F3=Fine?
089600150508           WHEN  dsp_aid = c_F03;
089700150508             unlock TNTBE01L;
089800150508             $Video = 'S1';
089900150508             $Fine  = *on;
090000150508
090100150508           // -?F12=Ritorno?
090200150508           WHEN  dsp_aid = c_F12;
090300150508             unlock TNTBE01L;
090400150508             $Video = 'S1';
090500150508
090600150508           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
090700150508           OTHER;
090800150508             $Ripristino   = (dsp_aid = c_F05);
090900150508             $Annullamento = (dsp_aid = c_F16);
091000150508             // -?Controlli eseguiti NON se F16=Annullamento?
091100150508             if  Not $Annullamento;
091200150508               exsr  sr_CtrD02;
091300150508             // -?Controlli eseguiti SOLO se F16=Annullamento?
091400150508             //else;
091500150508             //  exsr  sr_CtrANN;
091600150508             endif;
091700150508             if  $ErrGenerico;
091800150508               leavesr;
091900150508             endif;
092000150508             // -?Aggiornamento?
092100150508             if  dsp_aid = c_F05   or
092200150508                 dsp_aid = c_F06   or
092300150508                 dsp_aid = c_F16;
092400150508               $Video = 'W1';
092500150508               reset  $InzW01;
092600150508               exsr  sr_GesW01;
092700150508             endif;
092800150508
092900150508         ENDSL;
093000150508
093100150508       ENDSR;
093200150508
093300150508       //--------------------------------------------------------------
093400150508       //?Inizializzazione videata D02.                                ?
093500150508       //--------------------------------------------------------------
093600150508       BEGSR  sr_InzD02;
093700150508
093800150508         // -?Spegnimento degli indicatori di errore?
093900150508         %subst(IndDspF : 50) = *off;
094000150508
094100150508         // -?Pulizia videata?
094200150508         clear  TBRGRD02;
094300150508
094400150508         // -?Impostazione testata?
094500150508         clear  V1Topz;
094600150508         Select;
094700150508           When  $Protect;
094800150508             V1Topz = 'VISUALIZZAZIONE';
094900150508           When  $Immissione;
095000150508             V1Topz = '  INSERIMENTO  ';
095100150508           When  TBEatb <> *blank;
095200150508             V1Topz = '  RIPRISTINO   ';
095300150508             $Ripristino = *on;
095400150508           Other;
095500150508             V1Topz = '   MODIFICA    ';
095600150508         EndSl;
095700150508
095800150508         // -?Reperimento dati del Reparto Gestione R.A.?
095900150508         //  ?(SE NON Immissione)?
096000150508         If  Not $Immissione;
096100150508
096200150508           // -?Reperimento dati?
096300150508           k_TBEke1 = S1Crgr;
096400150508           if  $Protect;
096500150508             chain(N)  %kds( KeyTNTBE01 )  TNTBE000;
096600150508           else;
096700150508             chain  %kds( KeyTNTBE01 )  TNTBE000;
096800150508           endif;
096900150508
097000150508           // -?Caricamento dati del Codice?
097100150508           if  %found(TNTBE01L);
097200150508             exsr  sr_RieD02;
097300150508           endif;
097400150508
097500150508         EndiF;
097600150508
097700150508         // -?Impostazione indicatori per abilitazione tasti funzionali?
097800150508         exsr  sr_AbilitFxxD02;
097900150508
098000150508       ENDSR;
098100150508
098200150508       //--------------------------------------------------------------
098300150508       //?Caricamento dati del singolo record della tab. "CMD"         ?
098400150508       //?nella videata D02.                                           ?
098500150508       //--------------------------------------------------------------
098600150508       BEGSR  sr_RieD02;
098700150508
098800150508         dRGR = TBEuni;
098900150508
099000150508         V1Crgr  = TBEke1;
099100150508         V1Drgr  = dRGR.§RGRdes;
099200150508
099300150508       ENDSR;
099400150508
099500150508       //--------------------------------------------------------------
099600150508       //?Settaggio indicatori nella videata D02 per abilitazione      ?
099700150508       //?  tasti funzionali.                                          ?
099800150508       //--------------------------------------------------------------
099900150508       BEGSR  sr_AbilitFxxD02;
100000150508
100100150508         // -?F3 = Fine?
100200150508         $F3Attivo  = *off;
100300150508
100400150508         // -?F5 = Ripristino?
100500150508         $F5Attivo  = ( %found(TNTBE01L)  and  TBEatb <> *blank
100600150508                                          and  Not $Protect );
100700150508
100800150508         // -?F6 = Conferma?
100900150508         $F6Attivo  = ( NOT $F5Attivo  and  NOT $Protect );
101000150508
101100150508         // -?F12 = Ritorno?
101200150508         $F12Attivo = *on;
101300150508
101400150508         // -?F16 = Annullamento?
101500150508         $F16Attivo = ( %found(TNTBE01L)  and  TBEatb = *blank
101600150508                                          and  Not $Protect );
101700150508
101800150508       ENDSR;
101900150508
102000150508       //--------------------------------------------------------------
102100150508       //?Controllo dati videata D02.                                  ?
102200150508       //--------------------------------------------------------------
102300150508       BEGSR  sr_CtrD02;
102400150508
102500150508         // -?Spegnimento degli indicatori di errore?
102600150508         %subst(IndDspF : 50) = *off;
102700150508
102800150508         // -?Controllo immissione codice?
102900150508         If  V1Crgr = *blank;
103000150508           V1Dmsg = sk_Msg(04);
103100150508           $PosCurRGR   = *on;
103200150508           $ErrMessage  = *on;
103300150508           $ErrGenerico = *on;
103400150508           leavesr;
103500150508         EndIf;
103600150508
103700150508         // -?Verifica esistenza?
103800150508         If  $Immissione;
103900150508           k_TBEke1 = V1Crgr;
104000150508           setll  %kds( KeyTNTBE01 )  TNTBE000;
104100150508           if  %equal(TNTBE01L);
104200150508             V1Dmsg = sk_Msg(05);
104300150508             $PosCurRGR   = *on;
104400150508             $ErrMessage  = *on;
104500150508             $ErrGenerico = *on;
104600150508             leavesr;
104700150508           endif;
104800150508         EndIf;
104900150508
105000150508         // -?Descrizione obbligatoria?
105100150508         If  V1Drgr = *blank;
105200150508           V1Dmsg = sk_Msg(06);
105300150508           $PosCurDES   = *on;
105400150508           $ErrMessage  = *on;
105500150508           $ErrGenerico = *on;
105600150508           leavesr;
105700150508         EndIf;
105800150508
105900150508       ENDSR;
106000150508
106100150508       //--------------------------------------------------------------
106200150508       //?Gestione trasmissione aggiornamento                          ?
106300150508       //--------------------------------------------------------------
106400150508       BEGSR  sr_GesW01;
106500150508
106600150508         // -?Inizializzazione videata?
106700150508         if $InzW01 = *on;
106800150508           exsr  sr_InzW01;
106900150508           $InzW01 = *off;
107000150508         endif;
107100150508
107200150508         // -?Emissione window?
107300150508         if  TBXftt = 'S';
107400150508           exfmt  TBRGRW01;
107500150508           $ErrMessage  = *off;
107600150508           $ErrGenerico = *off;
107700150508           clear  V1Dmsg;
107800150508         else;
107900150508           dsp_aid = c_F06;
108000150508         endif;
108100150508
108200150508         select;
108300150508
108400150508           // -?F12=Ritorno (gestito solo se emesso W01)?
108500150508           when  dsp_aid = c_F12;
108600150508             $Video = 'D2';
108700150508
108800150508           // -?F6=Conferma?
108900150508           when  dsp_aid = c_F06;
109000150508             exsr  sr_UpdTNTBE;
109100150508             // -?Ritorno alla videata iniziale?
109200150508             if  NOT  $ErrGenerico;
109300150508               $Video  = 'S1';
109400150508               $InzS01 = *on;
109500150508             endif;
109600150508
109700150508         endsl;
109800150508
109900150508       ENDSR;
110000150508
110100150508       //--------------------------------------------------------------
110200150508       //?Inizializzazione videata W01                                 ?
110300150508       //--------------------------------------------------------------
110400150508       BEGSR  sr_InzW01;
110500150508
110600150508         clear TBRGRW01;
110700150508
110800150508         if  $Immissione;
110900150508
111000150508           // -?Se immissione?
111100150508           W1ftt  = TBXftt;
111200150508
111300150508         else;
111400150508
111500150508           // -?Se NON immissione:?
111600150508           //  ?visualizza i dati relativi all'ultima trasmissione?
111700150508           W1ftt  = TBEftt;
111800150508           W1flt  = TBEflt;
111900150508           W1ftr  = TBEftr;
112000150508           if TBEdtr <> *zero;
112100150508             wDate_EUR = %date(TBEdtr : *iso);
112200150508             W1dtr     = %dec(wDate_EUR);
112300150508           endif;
112400150508
112500150508         endif;
112600150508
112700150508       ENDSR;
112800150508
112900150508       //--------------------------------------------------------------
113000150508       //?Aggiornamento records TNTBE00F (tab. "RGR")                  ?
113100150508       //--------------------------------------------------------------
113200150508       BEGSR  sr_UpdTNTBE;
113300150508
113400150508         // -?Aggancio record di TNTBE00F?
113500150508         clear  keyTNTBE01;
113600150508         k_TBEcod  = c_Tab;
113700150508         k_TBEke1  = V1Crgr;
113800150508
113900150508         chain  %kds( KeyTNTBE01 )  TNTBE000;
114000150508
114100150508         clear  dRGR;
114200150508         dRGR.§RGRdes  = V1Drgr;
114300150508
114400150508         TBEuni = dRGR;
114500150508
114600150508         // -?Aggiornamento tab. "RGR"?
114700150508         SELECT;
114800150508
114900150508           // -?F5=Ripristino?
115000150508           WHEN  $Ripristino;
115100150508             clear  TBEatb;
115200150508             TBEftt = W1ftt;
115300150508             clear  TBEftr;
115400150508             //_______________
115500150508             UPDATE  TNTBE000;
115600150508             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
115700150508
115800150508           // -?F16=Annullamento?
115900150508           WHEN  $Annullamento;
116000150508             TBEatb = 'A';
116100150508             TBEftt = W1ftt;
116200150508             clear  TBEftr;
116300150508             //_______________
116400150508             UPDATE  TNTBE000;
116500150508             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
116600150508
116700150508           // -?Immissione o Modifica?
116800150508           OTHER;
116900150508             if  NOT %found(TNTBE01L);
117000150508               clear TNTBE000;
117100150508               TBEcod  = k_TBEcod;
117200150508               TBEke1  = k_TBEke1;
117300150508               TBEke2  = k_TBEke2;
117400150508               TBElin  = k_TBElin;
117500150508               TBEsif  = k_TBEsif;
117600150508               TBEuni  = dRGR;
117700150508             endif;
117800150508             TBEapl = TBXapl;
117900150508             clear  TBEatb;
118000150508             TBEftt = W1ftt;
118100150508             clear  TBEflt;
118200150508             clear  TBEftr;
118300150508             //clear TBEdtr;
118400150508             // -?Aggiornamento record?
118500150508             if  %found(TNTBE01L);
118600150508               //_____________
118700150508               UPDATE TNTBE000;
118800150508               //¯¯¯¯¯¯¯¯¯¯¯¯¯
118900150508             else;
119000150508               //_____________
119100150508               WRITE  TNTBE000;
119200150508               //¯¯¯¯¯¯¯¯¯¯¯¯¯
119300150508             endif;
119400150508
119500150508         ENDSL;
119600150508
119700150508       ENDSR;
119800150508
119900150508       //--------------------------------------------------------------
120000150508       //?Operazioni finali.                                           ?
120100150508       //--------------------------------------------------------------
120200150508       BEGSR  sr_RoutEnd;
120300150508
120400150508         // -?Chiusura funzioni precedentemente aperte?
120500150508         clear  TIBS02ds;
120600150508         T02tla = 'C';
120700150508         TNTBE_RicercaControllo ( kpjba : tibs02ds );
120800150508
120900150508         // -?Restituzione parametri?
121000150508         //  ?(se selezionato un Reparto Gestione R.A.)?
121100150508         If  wMemoSel > *zero;
121200150508
121300150508           chain  SavS01csr  TBRGRS01;
121400150508
121500150508           oRGRcod  = S1Crgr;
121600150508           oRGRdes  = S1Cdes;
121700150508
121800150508         EndIf;
121900150508
122000150508         kpjbu = tntbRGRds;
122100150508
122200150508         //  ?Uscita?
122300150508         return;
122400150508
122500150508       ENDSR;
122600150508
122700150508      /end-free
122800150508
122900150508       //--------------------------------------------------------------
123000150508       //?Definizione schiere a tempo di compilazione.                 ?
123100150508       //--------------------------------------------------------------
123200150508
123300150508** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
123400150508Raggiunto il limite massimo dei record caricabili a video                      1
123500150508Opzione errata                                                                 2
123600150508Selezionati più di un Reparto                                                  3
123700150508Immettere il Reparto Gestione R.A.                                             4
123800150508Reparto già esistente                                                          5
123900150508Descrizione obbligatoria                                                       6
