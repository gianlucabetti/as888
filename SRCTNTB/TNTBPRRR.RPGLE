000100160118       //==============================================================
000200160118       //?Gestione tabella "PRR" (Stato R.A. - ex Priorità)            ?
000300160118       //==============================================================
000400160118
000500160118       //--------------------------------------------------------------
000600160118       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160118       //--------------------------------------------------------------
000800160118
000900160118     /*PRM  dbgview(*source)
001000160118     /*END
001100160118
001200160118       //--------------------------------------------------------------
001300160118       //?Specifiche di controllo.                                     ?
001400160118       //--------------------------------------------------------------
001500160118
001600160118     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160118     h dftactgrp(*no)
001800160118
001900160118       //--------------------------------------------------------------
002000160118       //?Dichiarazione file.                                          ?
002100160118       //--------------------------------------------------------------
002200160118
002300160118       // -?Tabelle
002400160118     fTNTBE01L  Uf A e           k disk
002500160118
002600160118       // -?Video?
002700160118     fTNTBPRRD  cf   e             workstn
002800160118     f                                     sfile(TBPRRS01 : S01nrr)
002900160118     f                                     indds(IndDspF)
003000160118     f                                     infds(InfDspF)
003100160118
003200160118       //--------------------------------------------------------------
003300160118       //?Definizione costanti.                                        ?
003400160118       //--------------------------------------------------------------
003500160118
003600160118       // -?Codice tabella in gestione?
003700160118     d c_Tab           c                   const('PRR')
003800160118
003900160118       // -?Numero di record in ciascuna videata di subfile?
004000160118     d c_SflPag        c                   const(13)
004100160118
004200160118       // -?Numero massimo di record gestibili?
004300160118     d c_MaxRec        c                   const(9999)
004400160118
004500160118       // -?Tasti funzionali a video?
004600160118     d c_F01           c                   const(x'31')
004700160118     d c_F02           c                   const(x'32')
004800160118     d c_F03           c                   const(x'33')
004900160118     d c_F04           c                   const(x'34')
005000160118     d c_F05           c                   const(x'35')
005100160118     d c_F06           c                   const(x'36')
005200160118     d c_F07           c                   const(x'37')
005300160118     d c_F08           c                   const(x'38')
005400160118     d c_F09           c                   const(x'39')
005500160118     d c_F10           c                   const(x'3A')
005600160118     d c_F11           c                   const(x'3B')
005700160118     d c_F12           c                   const(x'3C')
005800160118     d c_F13           c                   const(x'B1')
005900160118     d c_F14           c                   const(x'B2')
006000160118     d c_F15           c                   const(x'B3')
006100160118     d c_F16           c                   const(x'B4')
006200160118     d c_F17           c                   const(x'B5')
006300160118     d c_F18           c                   const(x'B6')
006400160118     d c_F19           c                   const(x'B7')
006500160118     d c_F20           c                   const(x'B8')
006600160118     d c_F21           c                   const(x'B9')
006700160118     d c_F22           c                   const(x'BA')
006800160118     d c_F23           c                   const(x'BB')
006900160118     d c_F24           c                   const(x'BC')
007000160118     d c_Enter         c                   const(x'F1')
007100160118     d c_RollDown      c                   const(x'F4')
007200160118     d c_RollUp        c                   const(x'F5')
007300160118
007400160118       //--------------------------------------------------------------
007500160118       //?Definizione schiere.                                         ?
007600160118       //--------------------------------------------------------------
007700160118
007800160118       // -?Messaggi di errore?
007900160118     d sk_Msg          s             78    dim( 6)  ctdata  perrcd( 1)
008000160118
008100160118       //--------------------------------------------------------------
008200160118       //?Definizione aree dati.                                       ?
008300160118       //--------------------------------------------------------------
008400160118
008500160118       // -?Dati utente?
008600160118     d §AzUte        e ds                  extname(AZUTE00F)
008700160118     d                                     dtaara
008800160118     d §DatiUte      e ds                  extname(dDatiUte)
008900160118     d                                     dtaara
009000160118
009100160118       //--------------------------------------------------------------
009200160118       //?Definizione strutture dati.                                  ?
009300160118       //--------------------------------------------------------------
009400160118
009500160118       // -?Status ds?
009600160118     d Status         sds
009700160118     d   SDSpgm          *proc
009800160118
009900160118       // -?InfDS?
010000160118     d InfDspF         ds
010100160118     d   dsp_aid             369    369a
010200160118     d*//sfl_rrn             376    377i 0
010300160118     d*//min_nrr             378    379i 0
010400160118     d*//num_rcds            380    381i 0
010500160118
010600160118       // -?Indicatori su DspF?
010700160118     d IndDspF         ds                  inz
010800160118         // -?Abilitazione tasti funzionali?
010900160118     d   $F3Attivo                     n   overlay( IndDspF : 03 )
011000160118     d   $F5Attivo                     n   overlay( IndDspF : 05 )
011100160118     d   $F6Attivo                     n   overlay( IndDspF : 06 )
011200160118     d   $F8Attivo                     n   overlay( IndDspF : 08 )
011300160118     d   $F10Attivo                    n   overlay( IndDspF : 10 )
011400160118     d   $F12Attivo                    n   overlay( IndDspF : 12 )
011500160118     d   $F16Attivo                    n   overlay( IndDspF : 16 )
011600160118         // -?Emissione messaggio di errore?
011700160118     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011800160118         // -?Emissione messaggio di errore in window W1?
011900160118     d   $ErrMessageW1...
012000160118     d                                 n   overlay( IndDspF : 29 )
012100160118         // -?Indicatori di gestione del subfile?
012200160118     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
012300160118     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
012400160118     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
012500160118     d   $SflEnd                       n   overlay( IndDspF : 33 )
012600160118         // -?Indicatori per Attributi di visualizzazione?
012700160118     d   $DisabilOPZ                   n   overlay( IndDspF : 40 )
012800160118     d   $Ord_x_Des                    n   overlay( IndDspF : 41 )
012900160118     d   $ProtectKEY                   n   overlay( IndDspF : 42 )
013000160118         // -?Posizionamento cursore & segnalazione errore?
013100160118     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
013200160118     d   $PosCurCOD                    n   overlay( IndDspF : 51 )
013300160118     d   $PosCurDES                    n   overlay( IndDspF : 52 )
013400160118         // -?Riemissione videata?
013500160118     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013600160118
013700160118       // -?Parametri ricevuti?
013800160118     d KPJBA         e ds
013900160118     d TNTBPRRds     e ds                  inz
014000160118
014100160118       // -?Dati record principale della tabella?
014200160118     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014300160118     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014400160118     d                                          prefix(TBX:3)
014500160118
014600160118       // -?Tabelle usate:?
014700160118       // ·?"PRR" = Stato R.A. (ex Priorità)?
014800160118     d dPRR          e ds                  inz  qualified
014900160118
015000160118       //--------------------------------------------------------------
015100160118       //?Definizione variabili globali.                               ?
015200160118       //--------------------------------------------------------------
015300160118
015400160118       // -?Flags booleani?
015500160118     d $Called         s               n   inz
015600160118     d $Fine           s               n   inz
015700160118     d $InzS01         s               n   inz(*on)
015800160118     d $InzD02         s               n   inz(*on)
015900160118     d $InzW01         s               n   inz(*on)
016000160118     d $Annullamento   s               n   inz
016100160118     d $Immissione     s               n   inz
016200160118     d $Ripristino     s               n   inz
016300160118     d $Protect        s               n   inz
016400160118
016500160118       // -?Variabili per la gestione del video?
016600160118     d $Video          s              2    inz('S1')
016700160118     d S01nrr          s                   like(C1RcdNbr) inz
016800160118     d SavS01csr       s                   like(C1RcdNbr) inz
016900160118
017000160118       // -?Variabili per la gestione del posizionamento nel subfile?
017100160118     d SavC1Ccod       s                   like(C1Ccod)   inz
017200160118     d SavC1Cdes       s                   like(C1Cdes)   inz
017300160118
017400160118       // -?Conteggio selezioni (opz. 1) inserite?
017500160118     d wMemoSel        s                   like(S1Copz)   inz
017600160118
017700160118       // -?Campi di comodo?
017800160118     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
017900160118     d wDate           s              8s 0 inz
018000160118
018100160118       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018200160118
018300160118       //--------------------------------------------------------------
018400160118       // -?Constants?
018500160118       //--------------------------------------------------------------
018600160118       // -?MaxKey - Maximum number of key fields allowed by this program?
018700160118     d c_MaxKey        c                   const(2)
018800160118       // -?Sort order:?
018900160118       //  ?1 = Sort in an ascending sequence?
019000160118       //  ?2 = Sort in a descending sequence?
019100160118     d c_Ascendente    c                   const(1)
019200160118     d c_Discendente   c                   const(2)
019300160118       // -?Key data type:?
019400160118       //  ? 0 = Signed binary?
019500160118       //  ? 2 = Signed zoned decimal?
019600160118       //  ? 3 = Signed packed decimal?
019700160118       //  ? 6 = Character with no national language sort sequence applied,?
019800160118       //  ?     if specified?
019900160118       //  ? 7 = Unsigned packed decimal?
020000160118       //  ?     All numbers will have the sign forced positive ('F'X).?
020100160118       //  ? 8 = Unsigned zoned decimal?
020200160118       //  ?     All numbers will have the sign forced positive ('F'X).?
020300160118       //  ? 9 = Unsigned binary?
020400160118       //  ?14 = Date in form of DD/MM/YY?
020500160118       //  ?15 = Date in form of DD.MM.YYYY?
020600160118     d c_Numero        c                   const(2)
020700160118     d c_Carattere     c                   const(6)
020800160118     d c_NumIntero     c                   const(8)
020900160118       //
021000160118     d c_Put           c                   const(1)
021100160118     d c_EndPut        c                   const(2)
021200160118     d c_Get           c                   const(3)
021300160118
021400160118       //--------------------------------------------------------------
021500160118       // -?Data Structures?
021600160118       //  ?SflRcd     - The entire subfile record to pass to the sort?
021700160118       //  ?QLGSCB     - The sort request block for the QLGSORT API?
021800160118       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
021900160118       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
022000160118       //  ?QUSEC      - Error structure for the QLGSORT API?
022100160118       //--------------------------------------------------------------
022200160118     d SflRcd          ds                  inz
022300160118     d   S1Ccod                            inz
022400160118     d   S1Cdes                            inz
022500160118     d   S1Catb                            inz
022600160118     d   S1Copz                            inz
022700160118     d   Selected                     1a   inz
022800160118      /copy QSYSINC/QRPGLESRC,QLGSORT
022900160118     d QLGKL                         16    dim(c_MaxKey)
023000160118     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023100160118     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023200160118     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023300160118     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023400160118      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023500160118      /copy QSYSINC/QRPGLESRC,QUSEC
023600160118
023700160118       //--------------------------------------------------------------
023800160118       // -?Standalone fields?
023900160118       //  ?Nrr        - Relative record number for subfile?
024000160118       //  ?SizeList   - Size of list?
024100160118       //  ?ReturnSize - Size of list returned by sort API?
024200160118       //--------------------------------------------------------------
024300160118     d NotUsed         s             16a   inz
024400160118     d ReturnSize      s              9b 0 inz
024500160118     d SizeList        s              9b 0 inz
024600160118     d RrnLast         s              5i 0 inz
024700160118     d Nrr             s              5i 0 inz
024800160118
024900160118       //--------------------------------------------------------------
025000160118       //?Definizione prototipi procedure.                             ?
025100160118       //--------------------------------------------------------------
025200160118
025300160118       // -?Reperimento dati utente?
025400160118     d TIBS34ds      e ds
025500160118      /copy gaitrasrc/srcProtoPR,TIBS34R
025600160118
025700160118       // -?Ricerca e controllo nuove tabelle?
025800160118     d TIBS02ds      e ds                  inz
025900160118     d   T02mod      e                     inz('C')
026000160118      /copy gaitrasrc/srcProtoPR,TIBS02R
026100160118
026200160118       // -?Ordinamento subfile?
026300160118      /copy gaitrasrc/srcProtoPr,QLGSRTIO
026400160118
026500160118       //--------------------------------------------------------------
026600160118       //?Definizione key-list.                                        ?
026700160118       //--------------------------------------------------------------
026800160118
026900160118       // -?File TNTBE01L?
027000160118     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
027100160118     d                                     prefix(k_)   inz
027200160118
027300160118       //--------------------------------------------------------------
027400160118       //?Riepilogo indicatori utilizzati.                             ?
027500160118       //--------------------------------------------------------------
027600160118       //--------------------------------------------------------------
027700160118
027800160118       //--------------------------------------------------------------
027900160118       //?M A I N - L I N E                                            ?
028000160118       //--------------------------------------------------------------
028100160118
028200160118     c     *Entry        plist
028300160118     c                   parm                    KPJBA
028400160118
028500160118      /free
028600160118
028700160118       // -?Operazioni iniziali?
028800160118       exsr  sr_RoutInz;
028900160118
029000160118       // -?Ciclo di gestione del file video?
029100160118       DOW  $Fine = *off;
029200160118
029300160118         select;
029400160118
029500160118           // -?Gestione videata S1 (subfile)?
029600160118           when $Video = 'S1';
029700160118             exsr sr_GesS01;
029800160118
029900160118           // -?Manutenzione dati della singola tabella?
030000160118           when $Video = 'D2';
030100160118             exsr  sr_GesD02;
030200160118
030300160118           // -?Richiesta dati per trasmissione record?
030400160118           when $Video = 'W1';
030500160118             exsr  sr_GesW01;
030600160118
030700160118           // -?? ? ??
030800160118           other;
030900160118             $Fine = *on;
031000160118
031100160118         endsl;
031200160118
031300160118       ENDDO;
031400160118
031500160118       // -?Operazioni finali?
031600160118       exsr  sr_RoutEnd;
031700160118
031800160118       //--------------------------------------------------------------
031900160118       //?Operazioni iniziali.                                         ?
032000160118       //--------------------------------------------------------------
032100160118       BEGSR  sr_RoutInz;
032200160118
032300160118         // -?Impostazione chiusura?
032400160118         *inLR = *on;
032500160118
032600160118         // -?Reperimento dati job?
032700160118         exsr  sr_DatiJob;
032800160118
032900160118         // -?Reperimento data odierna?
033000160118         wDate = %dec( %date() );
033100160118
033200160118         // -?Impostazione nome programma a video?
033300160118         V1Tpgm = SDSpgm;
033400160118
033500160118         // -?Aggancio dati generali della tabella in esame?
033600160118         clear  keyTNTBE01;
033700160118         k_TBEke1 = *zero;
033800160118         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
033900160118                = c_Tab;
034000160118         k_TBEsif = KNSIF;
034100160118         chain(n)  %kds(keyTNTBE01)  TNTBE000;
034200160118         if  not  %found(TNTBE01L);
034300160118           clear  k_TBEsif;
034400160118           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
034500160118         endif;
034600160118         if  %found(TNTBE01L);
034700160118           xTNTBEds = TNTBEds;
034800160118         else;
034900160118           clear  xTNTBEds;
035000160118         endif;
035100160118
035200160118         // -?Verifica parametri ricevuti?
035300160118         $Called = ( KPJBU <> *blank );
035400160118         if  $Called;
035500160118           tntbPRRds = kpjbu;
035600160118         else;
035700160118           clear  tntbPRRds;
035800160118         endif;
035900160118
036000160118         // -?Pulizia parametri di output?
036100160118         clear  oPRRcod;
036200160118         clear  oPRRdes;
036300160118         clear  oPRRfxx;
036400160118         oPRRerr = *off;
036500160118         clear  oPRRmsg;
036600160118
036700160118         // -?Impostazione videata iniziale?
036800160118         $Video = 'S1';
036900160118         reset  $InzS01;
037000160118         $Ord_x_Des  = *off;
037100160118         $DisabilOpz = *off;
037200160118
037300160118         // -?Impostazione iniziale / pulizia dei campi chiave?
037400160118         clear  keyTNTBE01;
037500160118         k_TBEcod = c_Tab;
037600160118
037700160118       ENDSR;
037800160118
037900160118       //--------------------------------------------------------------
038000160118       //?Reperimento Dati del job (Utente/Operativi).                 ?
038100160118       //--------------------------------------------------------------
038200160118       BEGSR  sr_DatiJob;
038300160118
038400160118         in(E) §AzUte;
038500160118         if NOT %error;
038600160118           in(E) §DatiUte;
038700160118         endif;
038800160118         if %error or RSut = *blanks;
038900160118           clear TIBS34ds;
039000160118           tibs34r ( tibs34ds );
039100160118           in §AzUte;
039200160118           in §DatiUte;
039300160118         endif;
039400160118
039500160118       ENDSR;
039600160118
039700160118       //--------------------------------------------------------------
039800160118       //?Gestione subfile S01                                         ?
039900160118       //--------------------------------------------------------------
040000160118       BEGSR  sr_GesS01;
040100160118
040200160118         // -?Inizializzazione videata?
040300160118         if  $InzS01 = *on;
040400160118           exsr  sr_InzS01;
040500160118           $InzS01 = *off;
040600160118         endif;
040700160125
040800160125         // -?(Dis)Abilitazione tasti funzionali?
040900160125         exsr  sr_AbilitFxxC01;
041000160118
041100160118         // -?Emissione Piede con tasti funzionali abilitati (window)?
041200160118         write  TBPRRP01;
041300160118         // -?Emissione Testata?
041400160118         write  TBPRRT01;
041500160118
041600160118         // -?Posizionamento cursore?
041700160118         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
041800160118         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
041900160118         //  ?si visualizza la pagina che vedeva l'utente quando ha?
042000160118         //  ?premuto l'ultimo tasto.?
042100160118         if  C1CsrRrn > *zero;
042200160118           C1RcdNbr = C1CsrRrn;
042300160118         else;
042400160118           C1RcdNbr = 1;
042500160118           write  TBPRRS00;       //?(no rec.)?
042600160118         endif;
042700160118
042800160118         // -?Emissione videata (testata compresa) in window?
042900160118         exfmt  TBPRRC01;
043000160118
043100160118         clear  V1Dmsg;
043200160118         reset  $ErrMessage;
043300160118         reset  $ErrGenerico;
043400160118
043500160118         reset  $Immissione;
043600160118         reset  $Ripristino;
043700160118
043800160118         SELECT;
043900160118
044000160118           // -?F3=Fine?
044100160118           WHEN  dsp_aid = c_F03;
044200160118             oPRRfxx = '03';
044300160118             $Fine   = *on;
044400160118
044500160118           // -?F8=Cambio ordinamento?
044600160118           WHEN  dsp_aid = c_F08;
044700160118             exsr  sr_F08S01;
044800160118
044900160118           // -?F10=Inserimento Reparto Gestione R.A.?
045000160118           WHEN  dsp_aid = c_F10;
045100160118             $Immissione = *on;
045200160118             $Video  = 'D2';
045300160118             $InzD02 = *on;
045400160118
045500160118           // -?F12=Ritorno?
045600160118           WHEN  dsp_aid = c_F12;
045700160118             oPRRfxx = '12';
045800160118             $Fine   = *on;
045900160118
046000160118           // -?Subfile vuoto?
046100160118           WHEN  S01nrr = *zero;
046200160118             if  C1Ccod  <> SavC1Ccod   or
046300160118                 C1Cdes  <> SavC1Cdes;
046400160118               $InzS01 = *on;
046500160118             endif;
046600160118
046700160118           // -?Invio?
046800160118           OTHER;
046900160118             // -?Gestione opzioni?
047000160118             exsr  sr_OpzS01;
047100160118             select;
047200160118               when  $ErrGenerico;
047300160118                 leavesr;
047400160118               when  wMemoSel > *zero;
047500160118                 $Fine = *on;
047600160118                 leavesr;
047700160118             endsl;
047800160118             // -?Richiesto Posizionamento?
047900160118             if  ( Not $Ord_x_Des  and  C1Ccod <> SavC1Ccod )  or
048000160118                 ( $Ord_x_Des      and  C1Cdes <> SavC1Cdes );
048100160118               $InzS01 = *on;
048200160118               SavC1Ccod = C1Ccod;
048300160118               SavC1Cdes = C1Cdes;
048400160118             endif;
048500160118
048600160118         ENDSL;
048700160118
048800160118       ENDSR;
048900160118
049000160118       //--------------------------------------------------------------
049100160118       //?Inizializzazione subfile S01                                 ?
049200160118       //--------------------------------------------------------------
049300160118       BEGSR  sr_InzS01;
049400160118
049500160118         // -?Spegnimento degli indicatori di errore?
049600160118         %subst( IndDspF : 50 ) = *off;
049700160118
049800160118         // -?Pulizia subfile?
049900160118         $SflDsp_N    = *on;
050000160118         $SflDspCtl_N = *on;
050100160118         write  TBPRRC01;
050200160118         $SflDspCtl_N = *off;
050300160118         $SflEnd      = *off;
050400160118
050500160118         clear  C1RcdNbr;
050600160118         clear  C1CsrRrn;
050700160118         clear  S01nrr;
050800160118         clear  V1Dmsg;
050900160118         $ErrMessage  = *off;
051000160118         $ErrGenerico = *off;
051100160118
051200160118         $SflNxtChg = *off;
051300160118
051400160118         // -?Caricamento completo dei dati nel subfile (per codice)?
051500160118         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
051600160118         //  ?l'eventuale ordinamento)?
051700160118         clear  k_TBEke1;
051800160118         if  Not  $Ord_x_Des;
051900160118           k_TBEke1 = C1Ccod;
052000160118         endif;
052100160118         clear  k_TBEke2;
052200160118         clear  k_TBElin;
052300160118         clear  k_TBEsif;
052400160118         setll  %kds( keyTNTBE01 )  TNTBE000;
052500160118         reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
052600160118
052700160118         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
052800160118           //if  TBEatb = *blank;
052900160118             exsr  sr_WriteS01;
053000160118           //endif;
053100160118           reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
053200160118         EndDo;
053300160118
053400160118         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
053500160118         RrnLast   = S01nrr;
053600160118
053700160118         // -?Visualizzazione del SFL (se ci sono dati)?
053800160118         $SflDsp_N = ( S01nrr <= *zero );
053900160118
054000160118         // -?Attivazione del SFLEND?
054100160118         $SflEnd   = %eof(TNTBE01L);
054200160118
054300160118         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
054400160118         if  S01nrr > *zero;
054500160118           C1RcdNbr = 1;
054600160118         else;
054700160118           clear  C1RcdNbr;
054800160118         endif;
054900160118
055000160118         // -?Ordinamento subfile (se premuto F8)?
055100160118         if  $Ord_x_Des  and  S01nrr > 1;
055200160118           exsr  sr_SortSfl;
055300160118         endif;
055400160118
055500160118         // -?Impostazione Opzioni?
055600160118         if  $Called;
055700160118           C1Dopz = '1=Selezione';
055800160118         else;
055900160118           C1Dopz = '2=Modifica, 5=Visualizzazione';
056000160118         endif;
056100160118
056200160125         //// -?(Dis)Abilitazione tasti funzionali?
056300160125         //exsr  sr_AbilitFxxC01;
056400160118
056500160118         // -?Emissione messaggio di segnalazione raggiungimento limite?
056600160118         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
056700160118           $ErrGenerico = *on;
056800160118           $ErrMessage  = *on;
056900160118           $PosCurOPZ   = *on;
057000160118           V1Dmsg = sk_Msg(01);
057100160118           leavesr;
057200160118         endif;
057300160118
057400160118       ENDSR;
057500160118
057600160118       //--------------------------------------------------------------
057700160118       //?Registrazione del singolo rec. nel subfile S01               ?
057800160118       //--------------------------------------------------------------
057900160118       BEGSR  sr_WriteS01;
058000160118
058100160118         // -?Eventuale posizionamento richiesto?
058200160118         select;
058300160118           when  Not $Ord_x_Des  and  TBEke1 < C1Ccod;
058400160118             leavesr;
058500160118           when  $Ord_x_Des      and  TBEuni < C1Cdes;
058600160118             leavesr;
058700160118         endsl;
058800160118
058900160118         dPRR = TBEuni;
059000160118
059100160118         // -?Impostazione campi nel record del subfile?
059200160118         clear  TBPRRS01;
059300160118         S1Ccod  = %trimR( TBEke1 );
059400160118         S1Cdes  = dPRR.§PRRdesc;
059500160118         S1Catb  = TBEatb;
059600160118
059700160118         // -?Registrazione singolo record nel subfile?
059800160118         S01nrr += 1;
059900160118         write  TBPRRS01;
060000160118
060100160118       ENDSR;
060200160125
060300160125       //--------------------------------------------------------------
060400160125       //?Settaggio indicatori nella videata C01/S01 per abilitazione  ?
060500160125       //?  tasti funzionali.                                          ?
060600160125       //--------------------------------------------------------------
060700160125       BEGSR  sr_AbilitFxxC01;
060800160125
060900160125         // ·?F3 = Fine?
061000160125         $F3Attivo  = ( Not $Called );
061100160125
061200160125         // ·?F8 = Ordinamento x ...?
061300160125         $F8Attivo  = ( S01nrr > *zero );
061400160125
061500160125         // ·?F10 = Immissione?
061600160125         $F10Attivo = ( Not $Called );
061700160125
061800160125         // ·?F12 = Ritorno?
061900160125         $F12Attivo = ( $Called );
062000160125
062100160125       ENDSR;
062200160118
062300160118       //--------------------------------------------------------------
062400160118       //?Gestione opzioni del subfile S01                             ?
062500160118       //--------------------------------------------------------------
062600160118       BEGSR  sr_OpzS01;
062700160118
062800160118         // -?Spegnimento degli indicatori di errore?
062900160118         %subst(IndDspF : 50) = *off;
063000160118
063100160118         clear  SavS01csr;
063200160118         clear  wMemoSel;
063300160118
063400160118         // -?Ciclo di lettura subfile?
063500160118         readc  TBPRRS01;
063600160118
063700160118         DoW  Not %eof(TNTBPRRD);
063800160118
063900160118           $SflNxtChg = *off;
064000160118           %subst( IndDspF : 50 ) = *off;
064100160118           C1RcdNbr   = S01nrr;
064200160118
064300160118           Select;
064400160118
064500160118             // -?Nessuna opzione?
064600160118             When  S1Copz = *zero;
064700160118
064800160118             // -?Opz. 1=Selezione commerciale?
064900160118             When  $Called  and  S1Copz = 1;
065000160118               // -?Già selezionato un cliente?
065100160118               if  wMemoSel > *zero;
065200160118                 $ErrGenerico = *on;
065300160118                 $ErrMessage  = *on;
065400160118                 $PosCurOPZ   = *on;
065500160118                 V1Dmsg   = sk_Msg(03);
065600160118               else;
065700160118                 wMemoSel = S1Copz;
065800160118               endif;
065900160118
066000160118             // -?Opz. 2=Modifica?
066100160118             When  Not $Called  and  S1Copz = 2;
066200160118               $ProtectKEY = *on;
066300160118               $InzD02     = *on;
066400160118               $Video      = 'D2';
066500160118               DoW  $Video = 'D2';
066600160118                 exsr  sr_GesD02;
066700160118               EndDo;
066800160118               clear  $ProtectKEY;
066900160118
067000160118             // -?Opz. 5=Visualizzazione?
067100160118             When  Not $Called  and  S1Copz = 5;
067200160118               $Protect    = *on;
067300160118               $InzD02     = *on;
067400160118               $Video      = 'D2';
067500160118               DoW  $Video = 'D2';
067600160118                 exsr  sr_GesD02;
067700160118               EndDo;
067800160118               clear  $Protect;
067900160118
068000160118             // -?Opzione errata?
068100160118             Other;
068200160118               $ErrGenerico = *on;
068300160118               $ErrMessage  = *on;
068400160118               $PosCurOPZ   = *on;
068500160118               V1Dmsg = sk_Msg(02);
068600160118
068700160118           EndSl;
068800160118
068900160118           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
069000160118           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
069100160118           if  S1Copz   <> *zero  and
069200160118              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
069300160118             SavS01csr   = C1RcdNbr;
069400160118           endif;
069500160118
069600160118           // -?Aggiornamento sfl?
069700160118           select;
069800160118             when  $ErrMessage  or  S1Copz = 1;
069900160118               $SflNxtChg = *on;
070000160118               C1CsrRrn   = C1RcdNbr;
070100160118             when  $ErrGenerico;
070200160118               C1CsrRrn   = C1RcdNbr;
070300160118               if  S1Copz <> 1;
070400160118                 clear  S1Copz;          //?(se selezionato più di uno?)?
070500160118               endif;
070600160118             other;
070700160118               if  S1Copz <> 1;
070800160118                 clear  S1Copz;          //?(se selezionato più di uno?)?
070900160118               endif;
071000160118           endsl;
071100160118
071200160118           UPDATE  TBPRRS01;
071300160118
071400160118           if  $ErrGenerico  or  $ErrMessage;
071500160118             leave;
071600160118           endif;
071700160118
071800160118           readc  TBPRRS01;
071900160118
072000160118         ENDDO;
072100160118
072200160118         // -?Riposizionam. cursore sul record contenente la prima opz.?
072300160118         //  ?(se non sono stati rilevati errori)?
072400160118         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
072500160118             SavS01csr > *zero;
072600160118           C1CsrRrn = SavS01csr;
072700160118         endif;
072800160118
072900160118       ENDSR;
073000160118
073100160118       //--------------------------------------------------------------
073200160118       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
073300160118       //?F8 = Cambia ordinamento sfl                                  ?
073400160118       //--------------------------------------------------------------
073500160118       BEGSR  sr_F08S01;
073600160118
073700160118         if  Not $Ord_x_Des;
073800160118             $Ord_x_Des = *on;
073900160118         else;
074000160118           $Ord_x_Des = *off;
074100160118         endif;
074200160118
074300160118         //// -?SubFile già completamente caricato: basta riordinarlo?
074400160118         //exsr  sr_SortSfl;
074500160118         // -?Potrebbero essere state richiesti posizionamenti:?
074600160118         //  ?conviene ricaricare il subfile?
074700160118         exsr  sr_InzS01;
074800160118
074900160118       ENDSR;
075000160118
075100160118       //--------------------------------------------------------------
075200160118       //?Ordinamento subfile                                          ?
075300160118       //?  This subroutine sorts the subfile records.                 ?
075400160118       //--------------------------------------------------------------
075500160118       BEGSR  sr_SortSfl;
075600160118
075700160118         // -?Impostazione NRR dell'ultimo record del subfile          ?
075800160118         //  ?(già fatto in sr_InzS01:                                 ?
075900160118         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
076000160118         //RrnLast  = S01nrr;
076100160118
076200160118         C1RcdNbr = 1;
076300160118         clear  C1CsrRrn;
076400160118         clear  S01nrr;
076500160118         clear  V1Dmsg;
076600160118         $SflNxtChg   = *off;
076700160118         $ErrMessage  = *off;
076800160118         $ErrGenerico = *off;
076900160118         %subst( IndDspF : 50 ) = *off;
077000160118
077100160118         //?___________________________________________________________?
077200160118         //?Initialize the key fields to sort on.?
077300160118         //?There is one extra field not in the subfile -- Selected --?
077400160118         //?that is added to the record. This field is used to place?
077500160118         //?selected records in front of nonselected records.?
077600160118         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
077700160118
077800160118         clear  QLGKL;
077900160118         clear  QLGSKL;
078000160118         clear  QLGSCB;
078100160118         clear  QLGSCB00;
078200160118
078300160118         // -?Gestione della scelta ordinamento:?
078400160118         Select;
078500160118
078600160118           // -?Ordinamento per Codice Reparto?
078700160118           when  Not $Ord_x_Des;
078800160118             // -?1 campo chiave?
078900160118             QLGNBRK  = 1;
079000160118             // -?1° campo: codice comando (S1Ccod)?
079100160118             //            ?a posizione  1, 1 byte, char, ascending.?
079200160118             QLGSP    = 1;
079300160118             QLGSS    = %size(S1Ccod);
079400160118             QLGDT    = c_Carattere;
079500160118             QLGSO    = c_Ascendente;
079600160118             QLGKL(1) = QLGSKL;
079700160118
079800160118           // -?Ordinamento per Descrizione Reparto?
079900160118           when  $Ord_x_Des;
080000160118             // -?2 campi chiave?
080100160118             QLGNBRK  = 2;
080200160118             // -?1° campo: descr. comando (S1Cdes)?
080300160118             //            ?a posizione 2, 30 byte, char, ascending.?
080400160118             QLGSP    = %size(S1Ccod) + 1;
080500160118             QLGSS    = %size(S1Cdes);
080600160118             QLGDT    = c_Carattere;
080700160118             QLGSO    = c_Ascendente;
080800160118             QLGKL(1) = QLGSKL;
080900160118             // -?2° campo: codice comando (S1Ccod)?
081000160118             //            ?a posizone  1, 1 byte, char, ascending.?
081100160118             QLGSP    = 1;
081200160118             QLGSS    = %size(S1Ccod);
081300160118             QLGDT    = c_Carattere;
081400160118             QLGSO    = c_Ascendente;
081500160118             QLGKL(2) = QLGSKL;
081600160118
081700160118         EndSl;
081800160118
081900160118         // -?Load other sort parameters.?
082000160118         QLGLB   = 80 + 16 * c_MaxKey;
082100160118         QLGRL   = %size(SflRcd) - 1;
082200160118         QLGRT   = 8;
082300160118         QLGOKL  = 80;
082400160118         QLGLKE  = 16;
082500160118         QLGLSS  = 290;
082600160118         // -?Initialize Sort I/O API fields.?
082700160118         QLGRL00 = QLGRL;
082800160118         QLGRC00 = 1;
082900160118         clear  QUSEI;
083000160118         QUSBPRV = %size(QUSEC);
083100160118
083200160118         // -?First step - Initialize the sort routine.?
083300160118         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
083400160118                      SizeList : ReturnSize : QUSEC );
083500160118
083600160118         // -?Next step - Write records to I/O routine.?
083700160118         QLGRT00  = c_Put;
083800160118         For  Nrr = 1  To  RrnLast;
083900160118           chain  Nrr  TBPRRS01;
084000160118           // -?Solo le righe con Selected = 'Y' sono riordinate,?
084100160118           //  ?quindi per fare un ordinamento di tutte le righe?
084200160118           //  ?metto 'Y' sempre.?
084300160118           Selected = 'Y';
084400160118           clear  QUSEI;
084500160118           QUSBPRV  = %size(QUSEC);
084600160118           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
084700160118                         SizeList : NotUsed : QUSEC );
084800160118         EndFor;
084900160118
085000160118         // -?Next step - Signal end of input, clear subfile for reload.?
085100160118         QLGRT00 = c_EndPut;
085200160118         clear  QUSEI;
085300160118         QUSBPRV = %size(QUSEC);
085400160118         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
085500160118                       SizeList : NotUsed : QUSEC );
085600160118
085700160118         // -?Pulizia subfile?
085800160118         $SflDsp_N    = *on;
085900160118         $SflDspCtl_N = *on;
086000160118         write  TBPRRC01;
086100160118         $SflDspCtl_N = *off;
086200160118         $SflEnd      = *off;
086300160118
086400160118         // -?Final step - Write the records back to the subfile.?
086500160118         QLGRT00  = c_Get;
086600160118         For  Nrr = 1  To RrnLast;
086700160118           clear  QUSEI;
086800160118           QUSBPRV = %size(QUSEC);
086900160118           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
087000160118                         QLGRL00  : NotUsed : QUSEC );
087100160118           // -?Ripristino indicatori utilizzati nel sfl rec?
087200160118           $SflNxtChg = ( S1Copz <> *zero );
087300160118           // -?e aggiorno?
087400160118           S01nrr = Nrr;
087500160118           write  TBPRRS01;
087600160118         EndFor;
087700160118
087800160118         C1CsrRrn = 1;
087900160118
088000160118         // -?Visualizzazione del SFL (se ci sono dati)?
088100160118         $SflDsp_N = ( S01nrr <= *zero );
088200160118
088300160118         // -?Attivazione del SFLEND?
088400160118         $SflEnd   = %eof(TNTBE01L);
088500160118
088600160118       ENDSR;
088700160118
088800160118
088900160118       //--------------------------------------------------------------
089000160118       //?Gestione videata D02.                                        ?
089100160118       //--------------------------------------------------------------
089200160118       BEGSR  sr_GesD02;
089300160118
089400160118         // -?Inizializzazione videata?
089500160118         if  $InzD02 = *on;
089600160118           exsr  sr_InzD02;
089700160118           $InzD02 = *off;
089800160118         endif;
089900160118
090000160118         // -?Emissione Testata e Piede con tasti funzionali abilitati?
090100160118         write  TBPRRP02;
090200160118         write  TBPRRT02;
090300160118
090400160118         // -?Emissione videata?
090500160118         if  Not $Protect;
090600160118           exfmt  TBPRRD02;
090700160118         else;
090800160118           write  TBPRRD02;
090900160118           exfmt  Protect;
091000160118         endif;
091100160118
091200160118         clear  V1Dmsg;
091300160118         reset  $ErrMessage;
091400160118         reset  $ErrGenerico;
091500160118
091600160118         SELECT;
091700160118
091800160118           // -?F3=Fine?
091900160118           WHEN  dsp_aid = c_F03;
092000160118             unlock TNTBE01L;
092100160118             $Video = 'S1';
092200160125             //$Fine  = *on;
092300160125             $ErrGenerico = *on;
092400160118
092500160118           // -?F12=Ritorno?
092600160118           WHEN  dsp_aid = c_F12;
092700160118             unlock TNTBE01L;
092800160118             $Video = 'S1';
092900160118
093000160118           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
093100160118           OTHER;
093200160118             $Ripristino   = (dsp_aid = c_F05);
093300160118             $Annullamento = (dsp_aid = c_F16);
093400160118             // -?Controlli eseguiti NON se F16=Annullamento?
093500160118             if  Not $Annullamento;
093600160118               exsr  sr_CtrD02;
093700160118             // -?Controlli eseguiti SOLO se F16=Annullamento?
093800160118             //else;
093900160118             //  exsr  sr_CtrANN;
094000160118             endif;
094100160118             if  $ErrGenerico;
094200160118               leavesr;
094300160118             endif;
094400160118             // -?Aggiornamento?
094500160118             if  dsp_aid = c_F05   or
094600160118                 dsp_aid = c_F06   or
094700160118                 dsp_aid = c_F16;
094800160118               $Video = 'W1';
094900160118               reset  $InzW01;
095000160118               exsr  sr_GesW01;
095100160118             endif;
095200160118
095300160118         ENDSL;
095400160118
095500160118       ENDSR;
095600160118
095700160118       //--------------------------------------------------------------
095800160118       //?Inizializzazione videata D02.                                ?
095900160118       //--------------------------------------------------------------
096000160118       BEGSR  sr_InzD02;
096100160118
096200160118         // -?Spegnimento degli indicatori di errore?
096300160118         %subst(IndDspF : 50) = *off;
096400160118
096500160118         // -?Pulizia videata?
096600160118         clear  TBPRRD02;
096700160118
096800160118         // -?Impostazione testata?
096900160118         clear  V1Topz;
097000160118         Select;
097100160118           When  $Protect;
097200160118             V1Topz = 'VISUALIZZAZIONE';
097300160118           When  $Immissione;
097400160118             V1Topz = '  INSERIMENTO  ';
097500160118           When  TBEatb <> *blank;
097600160118             V1Topz = '  RIPRISTINO   ';
097700160118             $Ripristino = *on;
097800160118           Other;
097900160118             V1Topz = '   MODIFICA    ';
098000160118         EndSl;
098100160118
098200160118         // -?Reperimento dati del Reparto Gestione R.A.?
098300160118         //  ?(SE NON Immissione)?
098400160118         If  Not $Immissione;
098500160118
098600160118           // -?Reperimento dati?
098700160118           k_TBEke1 = S1Ccod;
098800160118           if  $Protect;
098900160118             chain(N)  %kds( KeyTNTBE01 )  TNTBE000;
099000160118           else;
099100160118             chain  %kds( KeyTNTBE01 )  TNTBE000;
099200160118           endif;
099300160118
099400160118           // -?Caricamento dati del Codice?
099500160118           if  %found(TNTBE01L);
099600160118             exsr  sr_RieD02;
099700160118           endif;
099800160118
099900160118         EndiF;
100000160118
100100160118         // -?Impostazione indicatori per abilitazione tasti funzionali?
100200160118         exsr  sr_AbilitFxxD02;
100300160118
100400160118       ENDSR;
100500160118
100600160118       //--------------------------------------------------------------
100700160118       //?Caricamento dati del singolo record della tab. "CMD"         ?
100800160118       //?nella videata D02.                                           ?
100900160118       //--------------------------------------------------------------
101000160118       BEGSR  sr_RieD02;
101100160118
101200160118         dPRR = TBEuni;
101300160118
101400160118         V1Csra  = TBEke1;
101500160118         V1Dsra  = dPRR.§PRRdesc;
101600160118
101700160118       ENDSR;
101800160118
101900160118       //--------------------------------------------------------------
102000160118       //?Settaggio indicatori nella videata D02 per abilitazione      ?
102100160118       //?  tasti funzionali.                                          ?
102200160118       //--------------------------------------------------------------
102300160118       BEGSR  sr_AbilitFxxD02;
102400160118
102500160118         // -?F3 = Fine?
102600160125         $F3Attivo  = ( Not $Immissione );
102700160118
102800160118         // -?F5 = Ripristino?
102900160125         $F5Attivo  = ( Not $Immissione   and
103000160125                        %found(TNTBE01L)  and  TBEatb <> *blank
103100160118                                          and  Not $Protect );
103200160118
103300160118         // -?F6 = Conferma?
103400160118         $F6Attivo  = ( NOT $F5Attivo  and  NOT $Protect );
103500160118
103600160118         // -?F12 = Ritorno?
103700160118         $F12Attivo = *on;
103800160118
103900160118         // -?F16 = Annullamento?
104000160125         $F16Attivo = ( Not $Immissione   and
104100160125                        %found(TNTBE01L)  and  TBEatb = *blank
104200160118                                          and  Not $Protect );
104300160118
104400160118       ENDSR;
104500160118
104600160118       //--------------------------------------------------------------
104700160118       //?Controllo dati videata D02.                                  ?
104800160118       //--------------------------------------------------------------
104900160118       BEGSR  sr_CtrD02;
105000160118
105100160118         // -?Spegnimento degli indicatori di errore?
105200160118         %subst(IndDspF : 50) = *off;
105300160118
105400160118         // -?Controllo immissione codice?
105500160118         If  V1Csra = *blank;
105600160118           V1Dmsg = sk_Msg(04);
105700160118           $PosCurCOD   = *on;
105800160118           $ErrMessage  = *on;
105900160118           $ErrGenerico = *on;
106000160118           leavesr;
106100160118         EndIf;
106200160118
106300160118         // -?Verifica esistenza?
106400160118         If  $Immissione;
106500160118           k_TBEke1 = V1Csra;
106600160118           setll  %kds( KeyTNTBE01 )  TNTBE000;
106700160118           if  %equal(TNTBE01L);
106800160118             V1Dmsg = sk_Msg(05);
106900160118             $PosCurCOD   = *on;
107000160118             $ErrMessage  = *on;
107100160118             $ErrGenerico = *on;
107200160118             leavesr;
107300160118           endif;
107400160118         EndIf;
107500160118
107600160118         // -?Descrizione obbligatoria?
107700160118         If  V1Dsra = *blank;
107800160118           V1Dmsg = sk_Msg(06);
107900160118           $PosCurDES   = *on;
108000160118           $ErrMessage  = *on;
108100160118           $ErrGenerico = *on;
108200160118           leavesr;
108300160118         EndIf;
108400160118
108500160118       ENDSR;
108600160118
108700160118       //--------------------------------------------------------------
108800160118       //?Gestione trasmissione aggiornamento                          ?
108900160118       //--------------------------------------------------------------
109000160118       BEGSR  sr_GesW01;
109100160118
109200160118         // -?Inizializzazione videata?
109300160118         if $InzW01 = *on;
109400160118           exsr  sr_InzW01;
109500160118           $InzW01 = *off;
109600160118         endif;
109700160118
109800160118         // -?Emissione window?
109900160118         if  TBXftt = 'S';
110000160118           exfmt  TBPRRW01;
110100160118           $ErrMessage  = *off;
110200160118           $ErrGenerico = *off;
110300160118           clear  V1Dmsg;
110400160118         else;
110500160118           dsp_aid = c_F06;
110600160118         endif;
110700160118
110800160118         select;
110900160118
111000160118           // -?F12=Ritorno (gestito solo se emesso W01)?
111100160118           when  dsp_aid = c_F12;
111200160118             $Video = 'D2';
111300160118
111400160118           // -?F6=Conferma?
111500160118           when  dsp_aid = c_F06;
111600160118             exsr  sr_UpdTNTBE;
111700160118             // -?Ritorno alla videata iniziale?
111800160118             if  NOT  $ErrGenerico;
111900160118               $Video  = 'S1';
112000160118               $InzS01 = *on;
112100160118             endif;
112200160118
112300160118         endsl;
112400160118
112500160118       ENDSR;
112600160118
112700160118       //--------------------------------------------------------------
112800160118       //?Inizializzazione videata W01                                 ?
112900160118       //--------------------------------------------------------------
113000160118       BEGSR  sr_InzW01;
113100160118
113200160118         clear TBPRRW01;
113300160118
113400160118         if  $Immissione;
113500160118
113600160118           // -?Se immissione?
113700160118           W1ftt  = TBXftt;
113800160118
113900160118         else;
114000160118
114100160118           // -?Se NON immissione:?
114200160118           //  ?visualizza i dati relativi all'ultima trasmissione?
114300160118           W1ftt  = TBEftt;
114400160118           W1flt  = TBEflt;
114500160118           W1ftr  = TBEftr;
114600160118           if TBEdtr <> *zero;
114700160118             wDate_EUR = %date(TBEdtr : *iso);
114800160118             W1dtr     = %dec(wDate_EUR);
114900160118           endif;
115000160118
115100160118         endif;
115200160118
115300160118       ENDSR;
115400160118
115500160118       //--------------------------------------------------------------
115600160118       //?Aggiornamento records TNTBE00F (tab. "PRR")                  ?
115700160118       //--------------------------------------------------------------
115800160118       BEGSR  sr_UpdTNTBE;
115900160118
116000160118         // -?Aggancio record di TNTBE00F?
116100160118         clear  keyTNTBE01;
116200160118         k_TBEcod  = c_Tab;
116300160118         k_TBEke1  = V1Csra;
116400160118
116500160118         chain  %kds( KeyTNTBE01 )  TNTBE000;
116600160118
116700160118         clear  dPRR;
116800160118         dPRR.§PRRdesc = V1Dsra;
116900160118
117000160118         TBEuni = dPRR;
117100160118
117200160118         // -?Aggiornamento tab. "PRR"?
117300160118         SELECT;
117400160118
117500160118           // -?F5=Ripristino?
117600160118           WHEN  $Ripristino;
117700160118             clear  TBEatb;
117800160118             TBEftt = W1ftt;
117900160118             clear  TBEftr;
118000160118             //_______________
118100160118             UPDATE  TNTBE000;
118200160118             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118300160118
118400160118           // -?F16=Annullamento?
118500160118           WHEN  $Annullamento;
118600160118             TBEatb = 'A';
118700160118             TBEftt = W1ftt;
118800160118             clear  TBEftr;
118900160118             //_______________
119000160118             UPDATE  TNTBE000;
119100160118             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
119200160118
119300160118           // -?Immissione o Modifica?
119400160118           OTHER;
119500160118             if  NOT %found(TNTBE01L);
119600160118               clear TNTBE000;
119700160118               TBEcod  = k_TBEcod;
119800160118               TBEke1  = k_TBEke1;
119900160118               TBEke2  = k_TBEke2;
120000160118               TBElin  = k_TBElin;
120100160118               TBEsif  = k_TBEsif;
120200160118               TBEuni  = dPRR;
120300160118             endif;
120400160118             TBEapl = TBXapl;
120500160118             clear  TBEatb;
120600160118             TBEftt = W1ftt;
120700160118             clear  TBEflt;
120800160118             clear  TBEftr;
120900160118             //clear TBEdtr;
121000160118             // -?Aggiornamento record?
121100160118             if  %found(TNTBE01L);
121200160118               //_____________
121300160118               UPDATE TNTBE000;
121400160118               //¯¯¯¯¯¯¯¯¯¯¯¯¯
121500160118             else;
121600160118               //_____________
121700160118               WRITE  TNTBE000;
121800160118               //¯¯¯¯¯¯¯¯¯¯¯¯¯
121900160118             endif;
122000160118
122100160118         ENDSL;
122200160118
122300160118       ENDSR;
122400160118
122500160118       //--------------------------------------------------------------
122600160118       //?Operazioni finali.                                           ?
122700160118       //--------------------------------------------------------------
122800160118       BEGSR  sr_RoutEnd;
122900160118
123000160118         // -?Chiusura funzioni precedentemente aperte?
123100160118         clear  TIBS02ds;
123200160118         T02tla = 'C';
123300160118         TNTBE_RicercaControllo ( kpjba : tibs02ds );
123400160118
123500160118         // -?Restituzione parametri?
123600160118         //  ?(se selezionato un Reparto Gestione R.A.)?
123700160118         If  wMemoSel > *zero;
123800160118
123900160118           chain  SavS01csr  TBPRRS01;
124000160118
124100160118           oPRRcod  = S1Ccod;
124200160118           oPRRdes  = S1Cdes;
124300160118
124400160118         EndIf;
124500160118
124600160118         kpjbu = tntbPRRds;
124700160118
124800160118         //  ?Uscita?
124900160118         return;
125000160118
125100160118       ENDSR;
125200160118
125300160118      /end-free
125400160118
125500160118       //--------------------------------------------------------------
125600160118       //?Definizione schiere a tempo di compilazione.                 ?
125700160118       //--------------------------------------------------------------
125800160118
125900160118** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
126000160118Raggiunto il limite massimo dei record caricabili a video                      1
126100160118Opzione errata                                                                 2
126200160118Selezionati più di un Reparto                                                  3
126300160118Immettere il Reparto Gestione R.A.                                             4
126400160118Reparto già esistente                                                          5
126500160118Descrizione obbligatoria                                                       6
