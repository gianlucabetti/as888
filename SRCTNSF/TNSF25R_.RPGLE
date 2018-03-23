000100170704       //==============================================================
000200171212       //? Gestione P.I. di soggetti a regime Split Payment.            ?
000300170704       //==============================================================
000400170704
000500170704       //--------------------------------------------------------------
000600171212       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170704       //--------------------------------------------------------------
000800170704
000900170704     /*PRM  dbgview(*source)
001000170704     /*END
001100170704
001200170704       //--------------------------------------------------------------
001300171212       //? Specifiche di controllo.                                     ?
001400170704       //--------------------------------------------------------------
001500170704
001600170704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170704     h dftactgrp(*no)
001800170705     h bnddir('TRUL')
001900170704
002000170704       //--------------------------------------------------------------
002100171212       //? Dichiarazione file.                                          ?
002200170704       //--------------------------------------------------------------
002300170704
002400171212       // - ?Riepilogo invio fatture   DETTAGLIO?
002500170726     fTIIFT02L  if   e           k disk    rename( TIIFT000 : TIIFT002 )
002600170726     fTIIFT03L  if   e           k disk    rename( TIIFT000 : TIIFT003 )
002700170704
002800171212       // - ?Soggetti tenuti al regime dello Split Payment?
002900170705     fANSPI01L  if   e           k disk    rename( ANSPI000 : ANSPI001 )
003000170705     f                                     infds( InfANSPI )
003100170705     fANSPI00F  Uf A e             disk
003200170704
003300171212       // - ?Video?
003400170704     fTNSF25D   cf   e             workstn
003500170704     f                                     sfile( SF25S01 : S01nrr )
003600170704     f                                     indds( IndDspF )
003700170704     f                                     infds( InfDspF )
003800170704
003900170704       //--------------------------------------------------------------
004000171212       //? Definizione costanti.                                        ?
004100170704       //--------------------------------------------------------------
004200170704
004300171212       // - ?Data Scadenza massima - in formato *EUR?
004400170704     d c_MaxDat_Eur    c                   const(31122039)
004500170704
004600171212       // - ?Numero di record in ciascuna videata di subfile?
004700170704     d c_SflPag        c                   const(15)
004800170704
004900171212       // - ?Numero massimo di record gestibili?
005000170704     d c_MaxRec        c                   const(9999)
005100170704
005200171212       // - ?Attributi di Visualizzazione?
005300170704     d c_DspAtrNorm    c                   const(x'A0')
005400170704     d c_DspAtrRI      c                   const(x'A1')
005500170704     d c_DspAtrHI      c                   const(x'A2')
005600170704     d c_DspAtrND      c                   const(x'A7')
005700170704     d c_DspAtrRED     c                   const(x'A8')
005800170704     d c_DspAtrRI_RED  c                   const(x'A9')
005900170704     d c_DspAtrBL_RED  c                   const(x'AA')
006000170704
006100171212       // - ?Tasti funzionali a video?
006200170704     d c_F01           c                   const(x'31')
006300170704     d c_F02           c                   const(x'32')
006400170704     d c_F03           c                   const(x'33')
006500170704     d c_F04           c                   const(x'34')
006600170704     d c_F05           c                   const(x'35')
006700170704     d c_F06           c                   const(x'36')
006800170704     d c_F07           c                   const(x'37')
006900170704     d c_F08           c                   const(x'38')
007000170704     d c_F09           c                   const(x'39')
007100170704     d c_F10           c                   const(x'3A')
007200170704     d c_F11           c                   const(x'3B')
007300170704     d c_F12           c                   const(x'3C')
007400170704     d c_F13           c                   const(x'B1')
007500170704     d c_F14           c                   const(x'B2')
007600170704     d c_F15           c                   const(x'B3')
007700170704     d c_F16           c                   const(x'B4')
007800170704     d c_F17           c                   const(x'B5')
007900170704     d c_F18           c                   const(x'B6')
008000170704     d c_F19           c                   const(x'B7')
008100170704     d c_F20           c                   const(x'B8')
008200170704     d c_F21           c                   const(x'B9')
008300170704     d c_F22           c                   const(x'BA')
008400170704     d c_F23           c                   const(x'BB')
008500170704     d c_F24           c                   const(x'BC')
008600170704     d c_Enter         c                   const(x'F1')
008700170704     d c_RollDown      c                   const(x'F4')
008800170704     d c_RollUp        c                   const(x'F5')
008900170704
009000170704       //--------------------------------------------------------------
009100171212       //? Definizione schiere.                                         ?
009200170704       //--------------------------------------------------------------
009300170704
009400171212       // - ?Messaggi di errore?
009500171212     d sk_Msg          s             78    dim(14)  ctdata  perrcd( 1)
009600170704
009700170704       //--------------------------------------------------------------
009800171212       //? Definizione aree dati.                                       ?
009900170704       //--------------------------------------------------------------
010000170704
010100171212       // - ?Dati utente?
010200170704     d §AzUte        e ds                  extname(AZUTE00F)
010300170704     d                                     dtaara
010400170704     d §DatiUte      e ds                  extname(dDatiUte)
010500170704     d                                     dtaara
010600170704
010700170704       //--------------------------------------------------------------
010800171212       //? Definizione strutture dati.                                  ?
010900170704       //--------------------------------------------------------------
011000170704
011100171212       // - ?Status ds?
011200170704     d Status         sds
011300170704     d   SDSpgm          *proc
011400170705
011500170705     d InfANSPI        ds                  qualified
011600170705     d   db_rrn              397    400i 0
011700170704
011800171212       // - ?InfDS?
011900170704     d InfDspF         ds
012000170704     d   dsp_aid             369    369a
012100170704     d*//sfl_rrn             376    377i 0
012200170704     d*//min_nrr             378    379i 0
012300170704     d*//num_rcds            380    381i 0
012400170704
012500171212       // - ?Indicatori su DspF?
012600170704     d IndDspF         ds                  inz
012700171212         // - ?Abilitazione tasti funzionali?
012800170704     d   $F3Attivo                     n   overlay( IndDspF : 03 )
012900170704     d   $F6Attivo                     n   overlay( IndDspF : 06 )
013000170704     d   $F10Attivo                    n   overlay( IndDspF : 10 )
013100170704     d   $F12Attivo                    n   overlay( IndDspF : 12 )
013200170704     d   $F16Attivo                    n   overlay( IndDspF : 16 )
013300171212         // - ?Emissione messaggio di errore?
013400170704     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013500171212         // - ?Indicatori di gestione del subfile?
013600170704     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
013700170704     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013800170704     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
013900170704     d   $SflEnd                       n   overlay( IndDspF : 33 )
014000171212         // - ?Indicatori per Attributi di visualizzazione?
014100170704     d*//$DisabilOPZ                   n   overlay( IndDspF : 40 )
014200170704     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
014300170704     d   $ProtectDDE                   n   overlay( IndDspF : 42 )
014400170704     d   $ProtectDSC                   n   overlay( IndDspF : 43 )
014500171212     d   $ProtectACO                   n   overlay( IndDspF : 44 )
014600171212     d   $VisualINS                    n   overlay( IndDspF : 45 )
014700171212     d   $VisualMOD                    n   overlay( IndDspF : 46 )
014800171212         // - ?Posizionamento cursore & segnalazione errore?
014900170704     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
015000170704     d   $PosCurIVA                    n   overlay( IndDspF : 51 )
015100170704     d   $PosCurDDE                    n   overlay( IndDspF : 52 )
015200170704     d   $PosCurDSC                    n   overlay( IndDspF : 53 )
015300171212     d   $PosCurACO                    n   overlay( IndDspF : 54 )
015400171212         // - ?Riemissione videata?
015500170704     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
015600170704
015700171212       // - ?Parametri ricevuti?
015800170704     d KPJBA         e ds
015900170704
016000171212       // - ?Tabella "QC" / "2"?
016100170704     d dsQC2         e ds                  qualified      inz
016200170704
016300170704       //--------------------------------------------------------------
016400171212       //? Definizione variabili globali.                               ?
016500170704       //--------------------------------------------------------------
016600170704
016700171212       // - ?Flags booleani?
016800170704     d $Fine           s               n   inz
016900170704     d $InzS01         s               n   inz(*on)
017000170704     d $InzD02         s               n   inz(*on)
017100170704     d $Cancellazione  s               n   inz
017200170704     d $Immissione     s               n   inz
017300170704     d $Protect        s               n   inz
017400170726     d $TIIFT_found    s               n   inz
017500170704
017600171212       // - ?Variabili per la gestione del video?
017700170704     d $Video          s              2    inz('S1')
017800170704     d S01nrr          s                   like(C1RcdNbr) inz
017900170704     d SavS01csr       s                   like(C1RcdNbr) inz
018000170704
018100171212       // - ?Variabili per la gestione del posizionamento nel subfile?
018200170704     d SavC1Civa       s                   like(C1Civa)   inz
018300171212       // - ?Variabili per la gestione delle selezioni nel subfile?
018400171212     d SavC1Caco       s                   like(C1Caco)   inz
018500170704
018600171212       // - ?Campi di comodo?
018700170704     d wDate           s              8s 0 inz
018800170705     d wTime           s              4s 0 inz
018900170705     d W2Ddde          s                   like(SPIdde)   inz
019000170705     d W2Ddsc          s                   like(SPIdsc)   inz
019100170704
019200170704       //--------------------------------------------------------------
019300171212       //? Definizione prototipi procedure.                             ?
019400170704       //--------------------------------------------------------------
019500170704
019600171212       // - ?Reperimento dati utente?
019700170704     d TIBS34ds      e ds
019800170704      /copy gaitrasrc/srcProtoPR,TIBS34R
019900170710
020000171212       // - ?Controllo Codice Fiscale e/o Partita Iva Europea (rel.1)?
020100170710     d xCFIVA1ds     e ds                  inz
020200170710      /copy gaitrasrc/srcProtoPR,XCFIVAR1
020300170704
020400171212       // - ?Reperimento dati tabelle?
020500170704      /copy gaitrasrc/srcProtoPI,TRULTAB
020600170704      /copy gaitrasrc/srcProtoPR,TRULTAB
020700170704
020800171212       // - ?Controllo/Inversione data?
020900170704     d WLBdat          ds                  inz
021000170704     d   G08dat                       8  0 inz
021100170704     d   G08inv                       8  0 inz
021200170704     d   G08err                       1    inz('3')
021300170704     d   G08tgi                       5  0 inz
021400170704      /copy gaitrasrc/srcProtoPR,XSRDA8
021500170704
021600171212       // - ?Parametri API QCAPCMD (Process Commands)?
021700170704     d Qcmd            s          32740    varying        inz
021800170704      /copy qSysInc/qRpgleSrc,QCAPCMD
021900171212       // - ?API QCAPCMD (Process Commands)?
022000170704      /copy gaitrasrc/srcProtoPR,QCAPCMD
022100170704
022200171212       // - ?Parametri gestione errori API.?
022300170704      /copy qsysinc/qrpglesrc,QUSEC
022400170704
022500170704       //--------------------------------------------------------------
022600171212       //? Definizione key-list.                                        ?
022700170704       //--------------------------------------------------------------
022800171212
022900171212       // - ?File ANSPI01L?
023000171212     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
023100171212     d                                     prefix( k_ )   inz
023200170704
023300171212       // - ?File TIIFT02L?
023400170704     d keyTIIFT02    e ds                  extname( TIIFT02L : *key )
023500170704     d                                     prefix( k_ )   inz
023600170726
023700171212       // - ?File TIIFT03L?
023800170726     d keyTIIFT03    e ds                  extname( TIIFT03L : *key )
023900170726     d                                     prefix( k3_ )  inz
024000170704
024100170704       //--------------------------------------------------------------
024200171212       //? Riepilogo indicatori utilizzati.                             ?
024300170704       //--------------------------------------------------------------
024400170704       //--------------------------------------------------------------
024500170704
024600170704       //--------------------------------------------------------------
024700171212       //? M A I N - L I N E                                            ?
024800170704       //--------------------------------------------------------------
024900170704
025000170704     c     *Entry        plist
025100170704     c                   parm                    KPJBA
025200170704
025300170704      /free
025400170704
025500171212       // - ?Operazioni iniziali?
025600170704       exsr  sr_RoutInz;
025700170704
025800171212       // - ?Ciclo di gestione del file video?
025900170704       DOW  $Fine = *off;
026000170704
026100170704         select;
026200170704
026300171212           // - ?Gestione videata S1 (subfile)?
026400170704           when $Video = 'S1';
026500170704             exsr sr_GesS01;
026600170704
026700171212           // - ?Manutenzione dati della singola P.I.?
026800170704           when $Video = 'D2';
026900170704             exsr  sr_GesD02;
027000170704
027100171212           // - ?? ? ??
027200170704           other;
027300170704             $Fine = *on;
027400170704
027500170704         endsl;
027600170704
027700170704       ENDDO;
027800170704
027900171212       // - ?Operazioni finali?
028000170704       exsr  sr_RoutEnd;
028100170704
028200170704       //--------------------------------------------------------------
028300171212       //? Operazioni iniziali.                                         ?
028400170704       //--------------------------------------------------------------
028500170704       BEGSR  sr_RoutInz;
028600170704
028700171212         // - ?Impostazione chiusura?
028800170704         *inLR = *on;
028900170704
029000171212         // - ?Reperimento dati job?
029100170704         exsr  sr_DatiJob;
029200170704
029300171212         // - ?Reperimento Registro Iva per Split Payment (913)?
029400170704         clear  dsQC2;
029500170704         ds_TNTBE.TBEke1 = '2';
029600170704         if  getTabella ( c_Tabel : 'QC'  : ds_TNTBE.TBEke1 :
029700170704                          *omit : *omit : *omit :
029800170704                          *omit : *omit :
029900170704                          *omit : *omit : *omit : *omit :
030000170704                          *omit : *omit : *omit : *omit :
030100170704                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
030200170704                        = *zero      AND
030300170704             ds_TNTBE.TBEatb = *blank;
030400170704           dsQC2  = ds_TNTBE.TBEuni;
030500170704         endif;
030600170704
030700171212         // - ?Reperimento data odierna?
030800170704         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030900170704                               : 1 : 8 ) );
031000171212         // - ?Reperimento orario attuale?
031100170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
031200170705                               : 9 : 4 ) );
031300170704
031400171212         // - ?Impostazione nome programma a video?
031500170704         V1Tpgm = SDSpgm;
031600170704
031700171212         // - ?Impostazione videata iniziale?
031800170704         $Video = 'S1';
031900170704         reset  $InzS01;
032000170704         reset  $InzD02;
032100170704
032200170704       ENDSR;
032300170704
032400170704       //--------------------------------------------------------------
032500171212       //? Reperimento Dati del job (Utente/Operativi).                 ?
032600170704       //--------------------------------------------------------------
032700170704       BEGSR  sr_DatiJob;
032800170704
032900170704         in(E) §AzUte;
033000170704         if NOT %error;
033100170704           in(E) §DatiUte;
033200170704         endif;
033300170704         if %error or RSut = *blanks;
033400170704           clear TIBS34ds;
033500170704           tibs34r ( tibs34ds );
033600170704           in §AzUte;
033700170704           in §DatiUte;
033800170704         endif;
033900170704
034000170704       ENDSR;
034100170704
034200170704       //--------------------------------------------------------------
034300171212       //? Gestione subfile S01.                                        ?
034400170704       //--------------------------------------------------------------
034500170704       BEGSR  sr_GesS01;
034600170704
034700171212         // - ?Inizializzazione videata?
034800170704         if  $InzS01 = *on;
034900170704           exsr  sr_InzS01;
035000170704           $InzS01 = *off;
035100170704         endif;
035200170704
035300171212         // - ?(Dis)Abilitazione tasti funzionali?
035400170706         exsr  sr_AbilitFxx;
035500170704
035600171212         // - ?Emissione Testata e Piede con tasti funzionali abilitati?
035700170704         write  SF25T01;
035800170704         write  SF25P01;
035900170704
036000171212         // - ?Posizionamento cursore?
036100171212         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
036200171212         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
036300171212         //   ?si visualizza la pagina che vedeva l'utente quando ha?
036400171212         //   ?premuto l'ultimo tasto.?
036500170704         if  C1CsrRrn > *zero;
036600170704           C1RcdNbr = C1CsrRrn;
036700170704         else;
036800170704           C1RcdNbr = 1;
036900170704           write  SF25S00;     //?(no rec.)?
037000170704         endif;
037100170704
037200171212         // - ?Emissione videata con subfile?
037300170704         exfmt  SF25C01;
037400170704
037500170704         clear  VIDmsg;
037600170704         reset  $ErrMessage;
037700170704         reset  $ErrGenerico;
037800170704
037900170704         reset  $Cancellazione;
038000170704         reset  $Immissione;
038100170706         reset  $Protect;
038200170704
038300170704         SELECT;
038400170704
038500171212           // - ?F3=Fine?
038600170704           WHEN  dsp_aid = c_F03;
038700170704             $Fine   = *on;
038800170704
038900171212           // - ?F10=Inserimento Filiale?
039000170704           WHEN  dsp_aid = c_F10;
039100170704             $Immissione = *on;
039200170704             $Video  = 'D2';
039300170704             $InzD02 = *on;
039400170704
039500171212           // - ?F12=Ritorno?
039600170704           WHEN  dsp_aid = c_F12;
039700170704             $Fine   = *on;
039800170704
039900171212           // - ?Subfile vuoto?
040000170704           WHEN  S01nrr = *zero;
040100171212             // - ?Richiesto Posizionamento?
040200170705             if  C1Civa  <> SavC1Civa;
040300170704               $InzS01 = *on;
040400170706               SavC1Civa = C1Civa;
040500170704             endif;
040600171212             // - ?Richiesta Selezione?
040700171212             if  C1Caco <> SavC1Caco;
040800171212               $InzS01 = *on;
040900171212               SavC1Caco = C1Caco;
041000171212             endif;
041100170704
041200171212           // - ?Invio?
041300170704           OTHER;
041400171212             // - ?Gestione opzioni?
041500170704             exsr  sr_OpzS01;
041600170704             if  $ErrGenerico;
041700170704               leavesr;
041800170704             endif;
041900171212             // - ?Richiesto Posizionamento?
042000170705             if  C1Civa <> SavC1Civa;
042100170704               $InzS01 = *on;
042200170705               SavC1Civa = C1Civa;
042300170704             endif;
042400171212             // - ?Richiesta Selezione?
042500171212             if  C1Caco <> SavC1Caco;
042600171212               $InzS01 = *on;
042700171212               SavC1Caco = C1Caco;
042800171212             endif;
042900170704
043000170704         ENDSL;
043100170704
043200170704       ENDSR;
043300170704
043400170704       //--------------------------------------------------------------
043500171212       //? Inizializzazione subfile S01.                                ?
043600170704       //--------------------------------------------------------------
043700170704       BEGSR  sr_InzS01;
043800170704
043900171212         // - ?Spegnimento degli indicatori di errore?
044000170704         %subst( IndDspF : 50 ) = *off;
044100170704
044200171212         // - ?Pulizia subfile?
044300170704         $SflDsp_N    = *on;
044400170704         $SflDspCtl_N = *on;
044500170704         write  SF25C01;
044600170704         $SflDspCtl_N = *off;
044700170704         $SflEnd      = *off;
044800170704
044900170704         clear  C1RcdNbr;
045000170704         clear  C1CsrRrn;
045100170704         clear  S01nrr;
045200170704         clear  VIDmsg;
045300170704         $ErrMessage  = *off;
045400170704         $ErrGenerico = *off;
045500170704
045600170704         $SflNxtChg = *off;
045700170704
045800171212         // - ?Caricamento completo dei dati nel subfile (per codice)?
045900171212         //   ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
046000171212         //   ?l'eventuale ordinamento)?
046100170704         clear  keyANSPI01;
046200170706         if  C1Civa <> *blank;
046300170706           k_SPIiva = C1Civa;
046400170706         endif;
046500170704         setll  %kds( keyANSPI01 )  ANSPI001;
046600170704         read  ANSPI001;
046700170704
046800170704         DoW  Not %eof( ANSPI01L )  and  S01nrr < c_MaxRec;
046900171212           exsr  sr_WriteS01;
047000170704           read  ANSPI001;
047100170704         EndDo;
047200170704
047300171212         // - ?Visualizzazione del SFL (se ci sono dati)?
047400170704         $SflDsp_N = ( S01nrr <= *zero );
047500170704
047600171212         // - ?Attivazione del SFLEND?
047700170704         $SflEnd   = %eof( ANSPI01L );
047800170704
047900171212         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
048000170704         if  S01nrr > *zero;
048100170704           C1RcdNbr = 1;
048200170704         else;
048300170704           clear  C1RcdNbr;
048400170704         endif;
048500170704
048600171212         // - ?Impostazione Opzioni?
048700180110         if  %subst( kNmUs : 1 : 3 ) = 'EDP';
048800180110           C1Dopz = '2=Modifica, 4=Cancellazione, 5=Visualizzazione';
048900180110         else;
049000180110           C1Dopz = '2=Modifica, 5=Visualizzazione';
049100180110         endif;
049200170704
049300171212         // - ?Emissione messaggio di segnalazione raggiungimento limite?
049400170704         if  S01nrr >= c_MaxRec   and   Not %eof( ANSPI01L );
049500170704           $ErrGenerico = *on;
049600170704           $ErrMessage  = *on;
049700170704           $PosCurOPZ   = *on;
049800170704           VIDmsg = sk_Msg(01);
049900170704           leavesr;
050000170704         endif;
050100170704
050200170704       ENDSR;
050300170704
050400170704       //--------------------------------------------------------------
050500171212       //? Registrazione del singolo rec. nel subfile S01.              ?
050600170704       //--------------------------------------------------------------
050700170704       BEGSR  sr_WriteS01;
050800170704
050900171212         // - ?Eventuale posizionamento richiesto?
051000170706         if  SPIiva < C1Civa;
051100170704           leavesr;
051200170704         endif;
051300171212
051400171212         // - ?Eventuale selezione per Anno Competenza?
051500171212         if  C1Caco <> *zero  and  C1Caco <> SPIaco;
051600171212           leavesr;
051700171212         endif;
051800170704
051900171212         // - ?Impostazione campi nel record del subfile?
052000170704         clear  SF25S01;
052100170704
052200170705         S1Hnrr   = InfANSPI.db_rrn;
052300170704         S1Hdde   = SPIdde;
052400170704         S1Hdsc   = SPIdsc;
052500170704         S1Hdim   = SPIdim;
052600170704         S1Hdmo   = SPIdmo;
052700170704
052800170704         S1Civa   = SPIiva;
052900170704
053000170704         reset  WLBdat;
053100170704         G08inv = SPIdde;
053200170704         XSRDA8( WLBdat );
053300170704         if  G08err = *zero;
053400170704           S1Ddde = G08dat;
053500170704         endif;
053600170704
053700170704         reset  WLBdat;
053800170704         G08inv = SPIdsc;
053900170704         XSRDA8( WLBdat );
054000170704         if  G08err = *zero;
054100170704           S1Ddsc = G08dat;
054200170704         endif;
054300170704
054400170707         reset  WLBdat;
054500170707         G08inv = SPIdim;
054600170707         XSRDA8( WLBdat );
054700170707         if  G08err = *zero;
054800170707           S1Ddim = G08dat;
054900170707         endif;
055000170707
055100170707         if  SPIdmo <> *zero;
055200170707           reset  WLBdat;
055300170707           G08inv = SPIdmo;
055400170707           XSRDA8( WLBdat );
055500170707           if  G08err = *zero;
055600170707             S1Ddmo = G08dat;
055700170707           endif;
055800170707         endif;
055900171212
056000171212         S1Daco = SPIaco;
056100170704
056200171212         // - ?Registrazione singolo record nel subfile?
056300170704         S01nrr += 1;
056400170704         write  SF25S01;
056500170704
056600170704       ENDSR;
056700170704
056800170704       //--------------------------------------------------------------
056900171212       //? Gestione opzioni del subfile S01                             ?
057000170704       //--------------------------------------------------------------
057100170704       BEGSR  sr_OpzS01;
057200170704
057300171212         // - ?Spegnimento degli indicatori di errore?
057400170704         %subst( IndDspF : 50 ) = *off;
057500170704
057600170704         clear  SavS01csr;
057700170704
057800171212         // - ?Ciclo di lettura subfile?
057900170704         readc  SF25S01;
058000170704
058100170704         DoW  Not %eof(TNSF25D);
058200170704
058300170704           $SflNxtChg = *off;
058400170704           %subst( IndDspF : 50 ) = *off;
058500170704           C1RcdNbr   = S01nrr;
058600170704
058700170704           Select;
058800170704
058900171212             // - ?Nessuna opzione?
059000170704             When  S1Copz = *blank;
059100171212
059200171212             // - ?Opz. 2=Modifica/Cancellazione?
059300170704             When  S1Copz  = '2';
059400170704               $ProtectKEY = *on;
059500170704               $InzD02     = *on;
059600170704               $Video      = 'D2';
059700170704               DoW  $Video = 'D2';
059800170704                 exsr  sr_GesD02;
059900170704               EndDo;
060000170704               clear  $ProtectKEY;
060100170704
060200171212             // - ?Opz. 4=Cancellazione?
060300171212             //   ?Opz. 5=Visualizzazione?
060400180110             When  S1Copz  = '4'  and
060500180110                   %subst( kNmUs : 1 : 3 ) <> 'EDP';
060600180110               $ErrGenerico = *on;
060700180110               $ErrMessage  = *on;
060800180110               $PosCurOPZ   = *on;
060900180110               VIDmsg = sk_Msg(02);
061000180110
061100170707             When  S1Copz  = '4'  or
061200170707                   S1Copz  = '5';
061300170707               $Cancellazione = ( S1Copz = '4' );
061400170704               $Protect    = *on;
061500170704               $InzD02     = *on;
061600170704               $Video      = 'D2';
061700170704               DoW  $Video = 'D2';
061800170704                 exsr  sr_GesD02;
061900170704               EndDo;
062000170704               clear  $Protect;
062100170704
062200171212             // - ?Opzione errata?
062300170704             Other;
062400170704               $ErrGenerico = *on;
062500170704               $ErrMessage  = *on;
062600170704               $PosCurOPZ   = *on;
062700170704               VIDmsg = sk_Msg(02);
062800170704
062900170704           EndSl;
063000170704
063100171212           // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
063200171212           //   ?riposizionarvici il cursore dopo il tasto "Invio"?
063300170704           if  S1Copz   <> *blank  and
063400170704              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
063500170704             SavS01csr   = C1RcdNbr;
063600170704           endif;
063700170704
063800171212           // - ?Aggiornamento sfl?
063900170704           if  $ErrGenerico;
064000170704             C1CsrRrn   = C1RcdNbr;
064100170707             $SflNxtChg = *on;
064200170707             //*·clear  S1Copz;
064300170704           else;
064400170704             clear  S1Copz;
064500170704           endif;
064600170704
064700170704           UPDATE  SF25S01;
064800170704
064900170704           if  $ErrGenerico  or  $ErrMessage;
065000170704             leave;
065100170704           endif;
065200170704
065300170704           readc  SF25S01;
065400170704
065500170704         ENDDO;
065600170704
065700171212         // - ?Riposizionam. cursore sul record contenente la prima opz.?
065800171212         //   ?(se non sono stati rilevati errori)?
065900170704         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
066000170704             SavS01csr > *zero;
066100170704           C1CsrRrn = SavS01csr;
066200170704         endif;
066300170704
066400170704       ENDSR;
066500170704
066600170704       //--------------------------------------------------------------
066700171212       //? Gestione videata D02.                                        ?
066800170704       //--------------------------------------------------------------
066900170704       BEGSR  sr_GesD02;
067000170704
067100171212         // - ?Inizializzazione videata?
067200170704         if  $InzD02 = *on;
067300170704           exsr  sr_InzD02;
067400170704           $InzD02 = *off;
067500170704         endif;
067600170706
067700171212         // - ?Cancellazione (da opz. 4 => $Cancellazione) NON ammessa?
067800171212         //   ?per la presenza di fatture con Registro Iva 913?
067900171212         //   ?(quella da F16 è già stata disabilitata nella subr. sr_InzD02)?
068000180110         If  $Cancellazione  and  $TIIFT_found;
068100170706           $Video = 'S1';
068200170707           VIDmsg = %trimr( sk_Msg(12) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
068300170706           $ErrMessage  = *on;
068400170706           $ErrGenerico = *on;
068500170706           leavesr;
068600170706         EndIf;
068700170704
068800171212         // - ?Emissione Testata e Piede con tasti funzionali abilitati?
068900170704         write  SF25T01;
069000170704         write  SF25P01;
069100170704
069200171212         // - ?Emissione videata?
069300170704         if  Not $Protect;
069400170704           exfmt  SF25D02;
069500170704         else;
069600170704           write  SF25D02;
069700170704           exfmt  Protect;
069800170704         endif;
069900170704
070000170704         clear  VIDmsg;
070100170704         reset  $ErrMessage;
070200170704         reset  $ErrGenerico;
070300170704
070400170704         SELECT;
070500170704
070600171212           // - ?F3=Fine?
070700170704           WHEN  dsp_aid = c_F03;
070800170705             unlock  ANSPI00F;
070900170704             $Video = 'S1';
071000170704             $ErrGenerico = *on;
071100170704             if  $Immissione;
071200170704               $Fine = *on;
071300170704             endif;
071400170704
071500171212           // - ?F12=Ritorno?
071600170704           WHEN  dsp_aid = c_F12;
071700170705             unlock  ANSPI00F;
071800170704             $Video = 'S1';
071900170704
072000171212           // - ?Enter / F6=Conferma / F16=Cancellazione?
072100170704           OTHER;
072200171212             // - ?Controlli eseguiti NON se F16=Annullamento?
072300170707             //*·if  Not $Cancellazione;
072400170707             if  dsp_Aid <> c_F16;
072500170704               exsr  sr_CtrD02;
072600170704             endif;
072700170704             if  $ErrGenerico;
072800170704               leavesr;
072900170704             endif;
073000171212             // - ?Aggiornamento?
073100170704             If  dsp_aid = c_F06   or
073200170704                 dsp_aid = c_F16;
073300170704               exsr  sr_Upd_ANSPI;
073400171212               // - ?Rilevato errore in fase di aggiornamento?
073500170704               if  $ErrGenerico = *on;
073600170704                 leavesr;
073700170704               endif;
073800171212               // - ?SubFile iniziale altrimenti?
073900170704               $Video  = 'S1';
074000170704               $InzS01 = *on;
074100170704             EndIf;
074200170704
074300170704         ENDSL;
074400170704
074500170704       ENDSR;
074600170704
074700170704       //--------------------------------------------------------------
074800171212       //? Inizializzazione videata D02.                                ?
074900170704       //--------------------------------------------------------------
075000170704       BEGSR  sr_InzD02;
075100170704
075200171212         // - ?Spegnimento degli indicatori di errore?
075300170706         %subst( IndDspF : 41 ) = *off;
075400170704
075500171212         // - ?Pulizia videata?
075600170704         clear  SF25D02;
075700170704
075800171212         // - ?Reperimento dati in gestione?
075900171212         //   ?(SE NON Immissione)?
076000170704         If  Not $Immissione;
076100170704
076200171212           // - ?Reperimento dati?
076300170707           if  $Protect;
076400170705             chain(N)  S1Hnrr  ANSPI000;
076500170704           else;
076600170705             chain  S1Hnrr  ANSPI000;
076700170704           endif;
076800170704
076900171212           // - ?Caricamento dati a video?
077000170705           if  %found( ANSPI00F );
077100170704             exsr  sr_RieD02;
077200170704           else;
077300170704             VIDmsg = sk_Msg(03);
077400170704             //*·$PosCurIVA   = *on;
077500170704             $ErrMessage  = *on;
077600170704             $ErrGenerico = *on;
077700170704             //*·leavesr;
077800170704           endif;
077900170704
078000170704         Else;
078100170704
078200171212           // - ?Impostazione dati di default SE immissione?
078300170704           V2Ddde = *date;
078400170704           V2Ddsc = c_MaxDat_Eur;
078500170704
078600170704         EndiF;
078700170704
078800171212         // - ?Impostazione testata?
078900170704         clear  V2Topz;
079000170704         Select;
079100170706           When  $Cancellazione;
079200170706             V2Topz = ' CANCELLAZIONE ';
079300170704           When  $Immissione;
079400170704             V2Topz = '  INSERIMENTO  ';
079500170706           When  $Protect;
079600170706             V2Topz = 'VISUALIZZAZIONE';
079700170704           Other;
079800170704             V2Topz = '   MODIFICA    ';
079900170704         EndSl;
080000170704
080100171212         // - ?Impostazione attributi di visualizzazione?
080200170707         $ProtectKEY = ( Not $Immissione );
080300170707         //*·$ProtectDDE = ( Not $Immissione  and
080400170704         //*·                SPIdde <= wDate );
080500170707         //*·$ProtectDSC = ( Not $Immissione  and
080600170706         //*·                SPIdsc <= wDate );
080700170704
080800171212         // - ?(Dis)Abilitazione tasti funzionali?
080900170706         exsr  sr_AbilitFxx;
081000170704
081100170704       ENDSR;
081200170704
081300170704       //--------------------------------------------------------------
081400171212       //? Caricamento dati del singolo record del file ANSPI00F nella  ?
081500171212       //? videata D02.                                                 ?
081600170704       //--------------------------------------------------------------
081700170704       BEGSR  sr_RieD02;
081800170704
081900170707         V2Civa = SPIiva;
082000170707         V2Dima = SPIima;
082100170704
082200170707         reset  WLBdat;
082300170707         G08inv   = SPIdde;
082400170707         XSRDA8(WLBdat);
082500170707         if  G08err = *zero;
082600170707           V2Ddde = G08dat;
082700170707         endif;
082800170704
082900170707         reset  WLBdat;
083000170707         G08inv   = SPIdsc;
083100170707         XSRDA8(WLBdat);
083200170707         if  G08err = *zero;
083300170707           V2Ddsc = G08dat;
083400170707         endif;
083500171212
083600171212         V2Daco = SPIaco;
083700170705
083800170707         if  SPIdim <> *zero  or  SPIpim <> *blank;
083900170707           reset  WLBdat;
084000170707           G08inv   = SPIdim;
084100170707           XSRDA8(WLBdat);
084200170707           if  G08err = *zero;
084300170707             V2Ddim = G08dat;
084400170707           endif;
084500170707         endif;
084600170707         V2Dhim = SPIhim;
084700170707         V2Dpim = SPIpim;
084800170705
084900170707         if  SPIdmo <> *zero  or  SPIpmo <> *blank;
085000170707           reset  WLBdat;
085100170707           G08inv   = SPIdmo;
085200170707           XSRDA8(WLBdat);
085300170707           if  G08err = *zero;
085400170707             V2Ddmo = G08dat;
085500170707           endif;
085600170707         endif;
085700170707         V2Dhmo = SPIhmo;
085800170707         V2Dpmo = SPIpmo;
085900170706
086000171212         // - ?Ricerca della 1ª data fattura del cliente con Registro IVA?
086100171212         //   ?"Split Payment" (913)?
086200170706         exsr  sr_RicercaDataFattura913_Prima;
086300170706
086400171212         // - ?Ricerca dell'ultima data fattura del cliente con Registro IVA?
086500171212         //   ?"Split Payment" (913)?
086600170706         exsr  sr_RicercaDataFattura913_Ultima;
086700170705
086800171212         // - ?Impostazione Attributi di visualizzazione?
086900170707         $VisualINS = ( Not $Immissione );
087000170707         $VisualMOD = ( Not $Immissione  and  SPIdmo > *zero );
087100170704
087200170704       ENDSR;
087300170704
087400170704       //--------------------------------------------------------------
087500171212       //? Controllo dati videata D02.                                  ?
087600170704       //--------------------------------------------------------------
087700170704       BEGSR  sr_CtrD02;
087800170704
087900171212         // - ?Spegnimento degli indicatori di errore?
088000170704         %subst(IndDspF : 50) = *off;
088100170704
088200171212         // - ?Controllo inserimento Partita Iva / Codice Fiscale?
088300170705         clear  V2Diva;
088400170705         If  V2Civa = *blank;
088500170705           VIDmsg = sk_Msg(04);
088600170705           $PosCurIVA   = *on;
088700170704           $ErrMessage  = *on;
088800170704           $ErrGenerico = *on;
088900170704           leavesr;
089000170704         EndIf;
089100170705
089200171212         // - ?Controllo correttezza Partita Iva inserita?
089300171212         //   ?(solo ITALIANA può essere)?
089400170710         If  $Immissione;
089500170710           clear  xCFIVA1ds;
089600170710           xCFIVAmod = 'P';
089700170710           // -?Partita Iva con Nazione davanti?
089800170710           if  %subst( V2Civa : 1 : 2 ) < '00';
089900170710             xCFIVApar = V2Civa;
090000171212           // - ?Partita Iva senza Nazione davanti (Italiana)?
090100170710           else;
090200170710             xCFIVApar = '  ' + V2Civa;
090300170710           endif;
090400170710           xCFIVAr1 ( xCFIVA1ds );
090500170714           if  xCFIVAflg < *zero;
090600170714             clear  xCFIVA1ds;
090700170714             //*·xCFIVAmod = *blank;
090800171212             // - ?Codice Fiscale (Italiano)?
090900170714             xCFIVApar = V2Civa;
091000170714             xCFIVAr1 ( xCFIVA1ds );
091100170714           endif;
091200170710           if  xCFIVAflg < *zero;
091300170710             VIDmsg = xCFIVAmsg;
091400170710             $PosCurIVA   = *on;
091500170710             $ErrMessage  = *on;
091600170710             $ErrGenerico = *on;
091700170710             leavesr;
091800170710           endif;
091900170710         EndIf;
092000170704
092100171212         // - ?Controllo che la P.I. non risulti già inserita?
092200171212         //   ?(anche se per periodo già scaduto...)?
092300170707         If  $Immissione;
092400170706           clear  keyANSPI01;
092500170706           k_SPIiva = V2Civa;
092600170706           chain  %kds( keyANSPI01 )  ANSPI001;
092700170706           if  %found( ANSPI01L );
092800170707             VIDmsg = sk_Msg(05);
092900170706             reset  WLBdat;
093000170706             G08inv   = SPIdde;
093100170706             XSRDA8(WLBdat);
093200170706             VIDmsg = %trimr( VIDmsg ) + ' (per il periodo dal ' +
093300170706                      %trim( %editw( G08dat : '  /  /    ' ) );
093400170706             reset  WLBdat;
093500170706             G08inv   = SPIdsc;
093600170706             XSRDA8(WLBdat);
093700170706             VIDmsg = %trimr( VIDmsg ) + ' al ' +
093800170706                      %trim( %editw( G08dat : '  /  /    ' ) ) + ')';
093900170707             $PosCurIVA   = *on;
094000170706             $ErrMessage  = *on;
094100170706             $ErrGenerico = *on;
094200170706             leavesr;
094300170706           endif;
094400170706         EndIf;
094500170704
094600171212         // - ?Controllo periodo validità?
094700171212         // · ?Data Decorrenza?
094800170704         clear  WLBdat;
094900170705         G08dat = V2Ddde;
095000170705         XSRDA8( WLBdat );
095100170704         if  G08err = *zero;
095200170705           V2Ddde = G08dat;
095300170705           W2Ddde = G08inv;
095400170704         else;
095500170707           VIDmsg = sk_Msg(06);
095600170705           $PosCurDDE   = *on;
095700170704           $ErrMessage  = *on;
095800170704           $ErrGenerico = *on;
095900170704           leavesr;
096000170704         endif;
096100171212         // · ?Data Scadenza?
096200170704         clear  WLBdat;
096300170705         G08dat = V2Ddsc;
096400170705         XSRDA8( WLBdat );
096500170704         if  G08err = *zero;
096600170705           V2Ddsc = G08dat;
096700170705           W2Ddsc = G08inv;
096800170704         else;
096900170707           VIDmsg = sk_Msg(07);
097000170705           $PosCurDSC   = *on;
097100170704           $ErrMessage  = *on;
097200170704           $ErrGenerico = *on;
097300170704           leavesr;
097400170704         endif;
097500171212         // · ?Range di date?
097600170705         if  W2Ddde > W2Ddsc;
097700170707           VIDmsg = sk_Msg(08);
097800170705           $PosCurDSC   = *on;
097900170704           $ErrMessage  = *on;
098000170704           $ErrGenerico = *on;
098100170704           leavesr;
098200170704         endif;
098300170704
098400171212         // - ?La Data Decorrenza NON deve essere successiva alla 1ª data?
098500171212         //   ?fattura del cliente con registro IVA "Split Payment" (913)?
098600170706         exsr  sr_RicercaDataFattura913_Prima;
098700170831         If  $TIIFT_found  and  W2Ddde > IFTdft;
098800170707           VIDmsg = %trimr( sk_Msg(09) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
098900170705           $PosCurDDE   = *on;
099000170705           $ErrMessage  = *on;
099100170705           $ErrGenerico = *on;
099200170705           leavesr;
099300170705         EndIf;
099400170705
099500171212         // - ?La Data Scadenza NON deve essere precedente all'ultima data?
099600171212         //   ?fattura del cliente con registro IVA "Split Payment" (913)?
099700170706         exsr  sr_RicercaDataFattura913_Ultima;
099800170831         If  $TIIFT_found  and  W2Ddsc < IFTdft;
099900170707           VIDmsg = %trimr( sk_Msg(10) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
100000170705           $PosCurDSC   = *on;
100100170705           $ErrMessage  = *on;
100200170705           $ErrGenerico = *on;
100300170705           leavesr;
100400170705         EndIf;
100500170704
100600171212         // - ?Verifica esistenza in ANSPI01L?
100700171212         //   ?(con sovrapposizione di giorni nel periodo)?
100800171212         // · ?Esempi - Periodo esistente: · · 3-----6 · ·
100900171212         // · ? 1)     Periodo inseribile: 1-2?
101000171212         // · ? 2) Periodo NON inseribile:   2---4?
101100171212         // · ? 3) Periodo NON inseribile:       4-5?
101200171212         // · ? 4) Periodo NON inseribile:         5---7?
101300171212         // · ? 5) Periodo NON inseribile:   2---------7?
101400171212         // · ? 6)     Periodo inseribile:             7-8?
101500170707         IF  $Immissione;
101600170705
101700170705           clear  keyANSPI01;
101800170705           k_SPIiva = V2Civa;
101900170705           setll  %kds( keyANSPI01 )  ANSPI001;
102000170705           reade  %kds( keyANSPI01 )  ANSPI001;
102100170705
102200170705           DoW  NOT %eof( ANSPI01L );
102300170705
102400170705             select;
102500170705               // ·? 2) e 3)?
102600170705               when  ( W2Ddsc >= SPIdde  and
102700170705                       W2Ddsc <= SPIdsc );
102800170707                 VIDmsg = sk_Msg(11);
102900170705                 $PosCurDDE   = *on;
103000170705                 $ErrMessage  = *on;
103100170705                 $ErrGenerico = *on;
103200170705                 leavesr;
103300170705               // ·? 3) e 4)?
103400170705               when  ( W2Ddde >= SPIdde  and
103500170705                       W2Ddde <= SPIdsc );
103600170707                 VIDmsg = sk_Msg(11);
103700170705                 $PosCurDDE   = *on;
103800170705                 $ErrMessage  = *on;
103900170705                 $ErrGenerico = *on;
104000170705                 leavesr;
104100170705               // ·? 5)?
104200170705               when  ( W2Ddde <= SPIdde  and
104300170705                       W2Ddde >= SPIdsc );
104400170707                 VIDmsg = sk_Msg(11);
104500170705                 $PosCurDDE   = *on;
104600170705                 $ErrMessage  = *on;
104700170705                 $ErrGenerico = *on;
104800170705                 leavesr;
104900170705             endsl;
105000170705
105100170705             reade  %kds( keyANSPI01 )  ANSPI001;
105200170705
105300170704           EndDo;
105400170705
105500170705         ENDIF;
105600171212
105700171212         // - ?Controllo Anno di Competenza (obbligatorio)?
105800171212         if  V2Daco < 2016   or
105900171212             V2Daco > 2048;
106000171212           VIDmsg = sk_Msg(14);
106100171212           $PosCurDSC   = *on;
106200171212           $ErrMessage  = *on;
106300171212           $ErrGenerico = *on;
106400171212           leavesr;
106500171212         endif;
106600170704
106700170704       ENDSR;
106800170706
106900170706       //--------------------------------------------------------------
107000171212       //? Settaggio degli indicatori per la (dis)abilitazione dei tasti?
107100171212       //?   funzionali nelle diverse videate.                          ?
107200170706       //--------------------------------------------------------------
107300170706       BEGSR  sr_AbilitFxx;
107400170706
107500171212         // - ?F3 = Fine?
107600170706         $F3Attivo  = *on;
107700170706
107800171212         // - ?F6 = Conferma?
107900170706         $F6Attivo  = ( $Video = 'D2'  and  Not $Protect );
108000170706
108100171212         // - ?F10 = Inserimento?
108200170706         $F10Attivo = ( $Video = 'S1' );
108300170706
108400171212         // - ?F12 = Ritorno?
108500170706         $F12Attivo = ( $Video = 'D2' );
108600170706
108700171212         // - ?F16 = Cancellazione?
108800170706         $F16Attivo = ( $Video = 'D2'     and
108900170706                        Not $Immissione   and
109000170706                        %found(ANSPI00F)  and
109100180110                        Not $TIIFT_found  and
109200170706                      ( Not $Protect      or
109300180110                        $Cancellazione )  and
109400180110                        %subst( kNmUs : 1 : 3 ) = 'EDP' );
109500180110
109600170706       ENDSR;
109700170706
109800170706       //--------------------------------------------------------------
109900171212       //? Ricerca della data della PRIMA fattura con Registro IVA 913  ?
110000171212       //?   per il cliente.                                            ?
110100170706       //--------------------------------------------------------------
110200170706       BEGSR  sr_RicercaDataFattura913_Prima;
110300170706
110400170706         clear  V2Dtxd;
110500170726         clear  $TIIFT_found;
110600170706
110700170706         clear  keyTIIFT02;
110800170706         k_IFTpiva = V2Civa;
110900170706         k_IFTfiv  = dsQC2.§QCfi7;
111000170717         //*·k_IFTdft  = *zero;
111100170706
111200170726         setll  %kds( keyTIIFT02 )  TIIFT002;
111300170726         reade  %kds( keyTIIFT02 : 2 )  TIIFT002;
111400170706
111500170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
111600170726
111700170726         If  Not $TIIFT_found;
111800170726
111900170726           clear  keyTIIFT03;
112000170726           k3_IFTcdf = V2Civa;
112100170726           k3_IFTfiv = dsQC2.§QCfi7;
112200170726           //*·k3_IFTdft = *zero;
112300170726
112400170726           setll  %kds( keyTIIFT03 )  TIIFT003;
112500170726           reade  %kds( keyTIIFT03 : 2 )  TIIFT003;
112600170726
112700170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
112800170726
112900170726         EndIf;
113000170726
113100170726         If  $TIIFT_found;
113200170706
113300170706           reset  WLBdat;
113400170706           G08inv   = IFTdft;
113500170706           XSRDA8(WLBdat);
113600170706
113700170706           V2Dtxd = 'Fatt. con Reg. IVA ' +
113800170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' dal ' +
113900170706                    %editw( G08dat : '  /  /    ' );
114000170706
114100170706         EndIf;
114200170706
114300170706       ENDSR;
114400170706
114500170706       //--------------------------------------------------------------
114600171212       //? Ricerca della data dell'ULTIMA fattura con Registro IVA 913  ?
114700171212       //?   per il cliente.                                            ?
114800170706       //--------------------------------------------------------------
114900170706       BEGSR  sr_RicercaDataFattura913_Ultima;
115000170706
115100170706         clear  V2Dtxs;
115200170726         clear  $TIIFT_found;
115300170706
115400170706         clear  keyTIIFT02;
115500170706         k_IFTpiva = V2Civa;
115600170706         k_IFTfiv  = dsQC2.§QCfi7;
115700170706         k_IFTdft  = *hival;
115800170726
115900170726         setgt  %kds( keyTIIFT02 )  TIIFT002;
116000170726         readPe  %kds( keyTIIFT02 : 2 )  TIIFT002;
116100170726
116200170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
116300170726
116400170726         If  Not $TIIFT_found;
116500170726
116600170726           clear  keyTIIFT03;
116700170726           k3_IFTcdf = V2Civa;
116800170726           k3_IFTfiv = dsQC2.§QCfi7;
116900170726           k3_IFTdft = *hival;
117000170726
117100170726           setgt  %kds( keyTIIFT03 )  TIIFT003;
117200170726           readPe  %kds( keyTIIFT03 : 2 )  TIIFT003;
117300170726
117400170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
117500170726
117600170726         EndIf;
117700170706
117800170726         If  $TIIFT_found;
117900170706
118000170706           reset  WLBdat;
118100170706           G08inv   = IFTdft;
118200170706           XSRDA8(WLBdat);
118300170706
118400170706           V2Dtxs = 'Fatt. con Reg. IVA ' +
118500170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' al  ' +
118600170706                    %editw( G08dat : '  /  /    ' );
118700170706
118800170706         EndIf;
118900170706
119000170706       ENDSR;
119100170704
119200170704       //--------------------------------------------------------------
119300171212       //? Aggiornamento dati nel *file ANSPI00F.                       ?
119400170704       //--------------------------------------------------------------
119500170704       BEGSR  sr_Upd_ANSPI;
119600170704
119700171212         // - ?Aggancio record di ANSPI00F?
119800170707         If  Not $Immissione;
119900170705           chain  S1Hnrr  ANSPI000;
120000170705         EndIf;
120100170705
120200171212         // - ?Reperimento data odierna?
120300170705         wDate = %int( %subst( %char( %dec( %timestamp() ) )
120400170705                               : 1 : 8 ) );
120500171212         // - ?Reperimento orario attuale?
120600170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
120700170705                               : 9 : 4 ) );
120800170704
120900171212         // - ?Impostazione dati?
121000170707         If  $Immissione  or
121100170705             ( NOT %found( ANSPI00F ) and
121200170704               NOT $Cancellazione );
121300170705           clear  ANSPI000;
121400170705           SPIiva = V2Civa;
121500170705           SPIdim = wDate;
121600170705           SPIhim = wTime;
121700170705           SPIpim = kNmUs;
121800170705           SPIima = 'M';            //?(M=Inserimento Manuale)?
121900170705         EndIf;
122000170704
122100170707         //*·If  Not $Cancellazione;
122200170707         If  dsp_aid <> c_F16;
122300170707
122400170705           SPIdde = W2Ddde;
122500170705           SPIdsc = W2Ddsc;
122600170705
122700170707           If  Not $Immissione;
122800170707             SPIdmo = wDate;
122900170707             SPIhmo = wTime;
123000170707             SPIpmo = kNmUs;
123100170707           EndIf;
123200170707
123300170707         EndIf;
123400170704
123500171212         // - ?Aggiornamento record?
123600170704         SELECT;
123700170704
123800171212           // - ?F16=Cancellazione?
123900170707           //*·WHEN  $Cancellazione  and  %found( ANSPI00F );
124000170707           WHEN  dsp_aid = c_F16  and  %found( ANSPI00F );
124100170705             //__________________
124200170705             delete(E)  ANSPI000;
124300170705             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
124400170704
124500171212           // - ?Immissione o Copia o Modifica?
124600170704           OTHER;
124700170707             if  Not $Immissione  and
124800170705                 %found( ANSPI00F );
124900170704               //_________________
125000170705               update(E) ANSPI000;
125100170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
125200170704             else;
125300170704               //_________________
125400170705               write(E)  ANSPI000;
125500170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
125600170704             endif;
125700170704
125800170704         ENDSL;
125900170704
126000171212         // - ?Segnalazione errore rilevato in fase di?
126100171212         //   ?scrittura/aggiornamento/cancellazione?
126200170704         If  %error();
126300170707           VIDmsg = sk_Msg(13);
126400170704           $ErrMessage  = *on;
126500170704           $ErrGenerico = *on;
126600170704           // -?Stampa del Dump?
126700170704           Dump(A);
126800171212           // - ?Stampa del Job-Log?
126900170704           Qcmd = 'DSPJOBLOG job(*) output(*print)';
127000170704           exsr  sr_ExecCmd;
127100170704           leavesr;
127200170704         EndIf;
127300170704
127400170704       ENDSR;
127500170704
127600170704       //--------------------------------------------------------------
127700171212       //? Esecuzione del comando (già impostato).                      ?
127800170704       //--------------------------------------------------------------
127900170704       BEGSR  sr_ExecCmd;
128000170704
128100170704         clear Qcap0100;
128200170704         Qcabcsdh = *off;
128300170704         Qcapa    = *off;
128400170704         Qcacmdss = *off;
128500170704         Qcaerved = *allX'00';
128600170704
128700170704         clear Qusec;
128800170704         Qusbprv  = %size(Qusec);
128900170704
129000170704         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
129100170704                           %size(Qcap0100) : 'CPOP0100' : *omit :
129200170704                           0 : 0 : Qusec);
129300170704
129400170704         //*·if  Qusei <> *blank;
129500170704         //*·  exsr  sr_PrintErr;
129600170704         //*·endif;
129700170704
129800170704       ENDSR;
129900170704
130000170704       //--------------------------------------------------------------
130100171212       //? Operazioni finali.                                           ?
130200170704       //--------------------------------------------------------------
130300170704       BEGSR  sr_RoutEnd;
130400170707
130500171212         // - ?Chiusura funzioni precedentemente aperte?
130600170707         cloTabella ( c_Tabel );
130700170704
130800171212         // - ?Uscita?
130900170704         return;
131000170704
131100170704       ENDSR;
131200170704
131300170704      /end-free
131400170704
131500170704       //--------------------------------------------------------------
131600171212       //? Definizione schiere a tempo di compilazione.                 ?
131700170704       //--------------------------------------------------------------
131800170704
131900171212** -- ?sk_Msg:?Messaggi di Errore? --------------------------------------------*
132000170704Raggiunto il limite massimo dei record caricabili a video                       1
132100170704Opzione errata                                                                  2
132200170704Partita Iva NON più reperibile in questo archivio                               3
132300170704Immettere la Partita Iva                                                        4
132400170707Partita Iva già inserita                                                        5
132500170707Data decorrenza formalmente errata                                              6
132600170707Data scadenza formalmente errata                                                7
132700170707Data di scadenza precedente a quella di decorrenza                              8
132800170707Data decorrenza successiva quella della 1ª fattura con Registro IVA             9
132900170707Data scadenza precedente quella dell'ultima fattura con Registro IVA           10
133000170707Il periodo si sovrappone a quello di un'altro Split Payment già inserito       11
133100170707Partita Iva NON cancellabile: trovate fatture con Registro IVA                 12
133200170707Rilevato errore in fase di registrazione dati => Avvisare CED                  13
133300171212Anno di competenza errato                                                      14
