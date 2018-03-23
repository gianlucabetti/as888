000100170704       //==============================================================
000200170704       //?Gestione P.I. di soggetti a regime Split Payment.            ?
000300170704       //==============================================================
000400170704
000500170704       //--------------------------------------------------------------
000600170704       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170704       //--------------------------------------------------------------
000800170704
000900170704     /*PRM  dbgview(*source)
001000170704     /*END
001100170704
001200170704       //--------------------------------------------------------------
001300170704       //?Specifiche di controllo.                                     ?
001400170704       //--------------------------------------------------------------
001500170704
001600170704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170704     h dftactgrp(*no)
001800170705     h bnddir('TRUL')
001900170704
002000170704       //--------------------------------------------------------------
002100170704       //?Dichiarazione file.                                          ?
002200170704       //--------------------------------------------------------------
002300170704
002400170704       // -?Riepilogo invio fatture   DETTAGLIO?
002500170726     fTIIFT02L  if   e           k disk    rename( TIIFT000 : TIIFT002 )
002600170726     fTIIFT03L  if   e           k disk    rename( TIIFT000 : TIIFT003 )
002700170704
002800170704       // -?Soggetti tenuti al regime dello Split Payment?
002900170705     fANSPI01L  if   e           k disk    rename( ANSPI000 : ANSPI001 )
003000170705     f                                     infds( InfANSPI )
003100170705     fANSPI00F  Uf A e             disk
003200170704
003300170704       // -?Video?
003400170704     fTNSF25D   cf   e             workstn
003500170704     f                                     sfile( SF25S01 : S01nrr )
003600170704     f                                     indds( IndDspF )
003700170704     f                                     infds( InfDspF )
003800170704
003900170704       //--------------------------------------------------------------
004000170704       //?Definizione costanti.                                        ?
004100170704       //--------------------------------------------------------------
004200170704
004300170704       // -?Data Scadenza massima - in formato *EUR?
004400170704     d c_MaxDat_Eur    c                   const(31122039)
004500170704
004600170704       // -?Numero di record in ciascuna videata di subfile?
004700170704     d c_SflPag        c                   const(15)
004800170704
004900170704       // -?Numero massimo di record gestibili?
005000170704     d c_MaxRec        c                   const(9999)
005100170704
005200170704       // -?Attributi di Visualizzazione?
005300170704     d c_DspAtrNorm    c                   const(x'A0')
005400170704     d c_DspAtrRI      c                   const(x'A1')
005500170704     d c_DspAtrHI      c                   const(x'A2')
005600170704     d c_DspAtrND      c                   const(x'A7')
005700170704     d c_DspAtrRED     c                   const(x'A8')
005800170704     d c_DspAtrRI_RED  c                   const(x'A9')
005900170704     d c_DspAtrBL_RED  c                   const(x'AA')
006000170704
006100170704       // -?Tasti funzionali a video?
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
009100170704       //?Definizione schiere.                                         ?
009200170704       //--------------------------------------------------------------
009300170704
009400170704       // -?Messaggi di errore?
009500170707     d sk_Msg          s             78    dim(13)  ctdata  perrcd( 1)
009600170704
009700170704       //--------------------------------------------------------------
009800170704       //?Definizione aree dati.                                       ?
009900170704       //--------------------------------------------------------------
010000170704
010100170704       // -?Dati utente?
010200170704     d §AzUte        e ds                  extname(AZUTE00F)
010300170704     d                                     dtaara
010400170704     d §DatiUte      e ds                  extname(dDatiUte)
010500170704     d                                     dtaara
010600170704
010700170704       //--------------------------------------------------------------
010800170704       //?Definizione strutture dati.                                  ?
010900170704       //--------------------------------------------------------------
011000170704
011100170704       // -?Status ds?
011200170704     d Status         sds
011300170704     d   SDSpgm          *proc
011400170705
011500170705     d InfANSPI        ds                  qualified
011600170705     d   db_rrn              397    400i 0
011700170704
011800170704       // -?InfDS?
011900170704     d InfDspF         ds
012000170704     d   dsp_aid             369    369a
012100170704     d*//sfl_rrn             376    377i 0
012200170704     d*//min_nrr             378    379i 0
012300170704     d*//num_rcds            380    381i 0
012400170704
012500170704       // -?Indicatori su DspF?
012600170704     d IndDspF         ds                  inz
012700170704         // -?Abilitazione tasti funzionali?
012800170704     d   $F3Attivo                     n   overlay( IndDspF : 03 )
012900170704     d   $F6Attivo                     n   overlay( IndDspF : 06 )
013000170704     d   $F10Attivo                    n   overlay( IndDspF : 10 )
013100170704     d   $F12Attivo                    n   overlay( IndDspF : 12 )
013200170704     d   $F16Attivo                    n   overlay( IndDspF : 16 )
013300170704         // -?Emissione messaggio di errore?
013400170704     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013500170704         // -?Indicatori di gestione del subfile?
013600170704     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
013700170704     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013800170704     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
013900170704     d   $SflEnd                       n   overlay( IndDspF : 33 )
014000170704         // -?Indicatori per Attributi di visualizzazione?
014100170704     d*//$DisabilOPZ                   n   overlay( IndDspF : 40 )
014200170704     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
014300170704     d   $ProtectDDE                   n   overlay( IndDspF : 42 )
014400170704     d   $ProtectDSC                   n   overlay( IndDspF : 43 )
014500170704     d   $VisualINS                    n   overlay( IndDspF : 44 )
014600170704     d   $VisualMOD                    n   overlay( IndDspF : 45 )
014700170704         // -?Posizionamento cursore & segnalazione errore?
014800170704     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
014900170704     d   $PosCurIVA                    n   overlay( IndDspF : 51 )
015000170704     d   $PosCurDDE                    n   overlay( IndDspF : 52 )
015100170704     d   $PosCurDSC                    n   overlay( IndDspF : 53 )
015200170704         // -?Riemissione videata?
015300170704     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
015400170704
015500170704       // -?Parametri ricevuti?
015600170704     d KPJBA         e ds
015700170704
015800170704       // -?Tabella "QC" / "2"?
015900170704     d dsQC2         e ds                  qualified      inz
016000170704
016100170704       //--------------------------------------------------------------
016200170704       //?Definizione variabili globali.                               ?
016300170704       //--------------------------------------------------------------
016400170704
016500170704       // -?Flags booleani?
016600170704     d $Fine           s               n   inz
016700170704     d $InzS01         s               n   inz(*on)
016800170704     d $InzD02         s               n   inz(*on)
016900170704     d $Cancellazione  s               n   inz
017000170704     d $Immissione     s               n   inz
017100170704     d $Protect        s               n   inz
017200170726     d $TIIFT_found    s               n   inz
017300170704
017400170704       // -?Variabili per la gestione del video?
017500170704     d $Video          s              2    inz('S1')
017600170704     d S01nrr          s                   like(C1RcdNbr) inz
017700170704     d SavS01csr       s                   like(C1RcdNbr) inz
017800170704
017900170704       // -?Variabili per la gestione del posizionamento nel subfile?
018000170704     d SavC1Civa       s                   like(C1Civa)   inz
018100170704
018200170704       // -?Campi di comodo?
018300170704     d wDate           s              8s 0 inz
018400170705     d wTime           s              4s 0 inz
018500170705     d W2Ddde          s                   like(SPIdde)   inz
018600170705     d W2Ddsc          s                   like(SPIdsc)   inz
018700170704
018800170704       //--------------------------------------------------------------
018900170704       //?Definizione prototipi procedure.                             ?
019000170704       //--------------------------------------------------------------
019100170704
019200170704       // -?Reperimento dati utente?
019300170704     d TIBS34ds      e ds
019400170704      /copy gaitrasrc/srcProtoPR,TIBS34R
019500170710
019600170710       // -?Controllo Codice Fiscale e/o Partita Iva Europea (rel.1)?
019700170710     d xCFIVA1ds     e ds                  inz
019800170710      /copy gaitrasrc/srcProtoPR,XCFIVAR1
019900170704
020000170704       // -?Reperimento dati tabelle?
020100170704      /copy gaitrasrc/srcProtoPI,TRULTAB
020200170704      /copy gaitrasrc/srcProtoPR,TRULTAB
020300170704
020400170704       // -?Controllo/Inversione data?
020500170704     d WLBdat          ds                  inz
020600170704     d   G08dat                       8  0 inz
020700170704     d   G08inv                       8  0 inz
020800170704     d   G08err                       1    inz('3')
020900170704     d   G08tgi                       5  0 inz
021000170704      /copy gaitrasrc/srcProtoPR,XSRDA8
021100170704
021200170704       // -?Parametri API QCAPCMD (Process Commands)?
021300170704     d Qcmd            s          32740    varying        inz
021400170704      /copy qSysInc/qRpgleSrc,QCAPCMD
021500170704       // -?API QCAPCMD (Process Commands)?
021600170704      /copy gaitrasrc/srcProtoPR,QCAPCMD
021700170704
021800170704       // -?Parametri gestione errori API.?
021900170704      /copy qsysinc/qrpglesrc,QUSEC
022000170704
022100170704       //--------------------------------------------------------------
022200170704       //?Definizione key-list.                                        ?
022300170704       //--------------------------------------------------------------
022400170704
022500170704       // -?File TIIFT02L?
022600170704     d keyTIIFT02    e ds                  extname( TIIFT02L : *key )
022700170704     d                                     prefix( k_ )   inz
022800170726
022900170726       // -?File TIIFT03L?
023000170726     d keyTIIFT03    e ds                  extname( TIIFT03L : *key )
023100170726     d                                     prefix( k3_ )  inz
023200170704
023300170704       // -?File ANSPI01L?
023400170704     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
023500170704     d                                     prefix( k_ )   inz
023600170704
023700170704       //--------------------------------------------------------------
023800170704       //?Riepilogo indicatori utilizzati.                             ?
023900170704       //--------------------------------------------------------------
024000170704       //--------------------------------------------------------------
024100170704
024200170704       //--------------------------------------------------------------
024300170704       //?M A I N - L I N E                                            ?
024400170704       //--------------------------------------------------------------
024500170704
024600170704     c     *Entry        plist
024700170704     c                   parm                    KPJBA
024800170704
024900170704      /free
025000170704
025100170704       // -?Operazioni iniziali?
025200170704       exsr  sr_RoutInz;
025300170704
025400170704       // -?Ciclo di gestione del file video?
025500170704       DOW  $Fine = *off;
025600170704
025700170704         select;
025800170704
025900170704           // -?Gestione videata S1 (subfile)?
026000170704           when $Video = 'S1';
026100170704             exsr sr_GesS01;
026200170704
026300170704           // -?Manutenzione dati della singola P.I.?
026400170704           when $Video = 'D2';
026500170704             exsr  sr_GesD02;
026600170704
026700170704           // -?? ? ??
026800170704           other;
026900170704             $Fine = *on;
027000170704
027100170704         endsl;
027200170704
027300170704       ENDDO;
027400170704
027500170704       // -?Operazioni finali?
027600170704       exsr  sr_RoutEnd;
027700170704
027800170704       //--------------------------------------------------------------
027900170704       //?Operazioni iniziali.                                         ?
028000170704       //--------------------------------------------------------------
028100170704       BEGSR  sr_RoutInz;
028200170704
028300170704         // -?Impostazione chiusura?
028400170704         *inLR = *on;
028500170704
028600170704         // -?Reperimento dati job?
028700170704         exsr  sr_DatiJob;
028800170704
028900170705         // -?Reperimento Registro Iva per Split Payment (913)?
029000170704         clear  dsQC2;
029100170704         ds_TNTBE.TBEke1 = '2';
029200170704         if  getTabella ( c_Tabel : 'QC'  : ds_TNTBE.TBEke1 :
029300170704                          *omit : *omit : *omit :
029400170704                          *omit : *omit :
029500170704                          *omit : *omit : *omit : *omit :
029600170704                          *omit : *omit : *omit : *omit :
029700170704                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
029800170704                        = *zero      AND
029900170704             ds_TNTBE.TBEatb = *blank;
030000170704           dsQC2  = ds_TNTBE.TBEuni;
030100170704         endif;
030200170704
030300170704         // -?Reperimento data odierna?
030400170704         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030500170704                               : 1 : 8 ) );
030600170705         // -?Reperimento orario attuale?
030700170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
030800170705                               : 9 : 4 ) );
030900170704
031000170704         // -?Impostazione nome programma a video?
031100170704         V1Tpgm = SDSpgm;
031200170704
031300170704         // -?Impostazione videata iniziale?
031400170704         $Video = 'S1';
031500170704         reset  $InzS01;
031600170704         reset  $InzD02;
031700170704
031800170704       ENDSR;
031900170704
032000170704       //--------------------------------------------------------------
032100170704       //?Reperimento Dati del job (Utente/Operativi).                 ?
032200170704       //--------------------------------------------------------------
032300170704       BEGSR  sr_DatiJob;
032400170704
032500170704         in(E) §AzUte;
032600170704         if NOT %error;
032700170704           in(E) §DatiUte;
032800170704         endif;
032900170704         if %error or RSut = *blanks;
033000170704           clear TIBS34ds;
033100170704           tibs34r ( tibs34ds );
033200170704           in §AzUte;
033300170704           in §DatiUte;
033400170704         endif;
033500170704
033600170704       ENDSR;
033700170704
033800170704       //--------------------------------------------------------------
033900170704       //?Gestione subfile S01.                                        ?
034000170704       //--------------------------------------------------------------
034100170704       BEGSR  sr_GesS01;
034200170704
034300170704         // -?Inizializzazione videata?
034400170704         if  $InzS01 = *on;
034500170704           exsr  sr_InzS01;
034600170704           $InzS01 = *off;
034700170704         endif;
034800170704
034900170704         // -?(Dis)Abilitazione tasti funzionali?
035000170706         exsr  sr_AbilitFxx;
035100170704
035200170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
035300170704         write  SF25T01;
035400170704         write  SF25P01;
035500170704
035600170704         // -?Posizionamento cursore?
035700170704         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035800170704         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035900170704         //  ?si visualizza la pagina che vedeva l'utente quando ha?
036000170704         //  ?premuto l'ultimo tasto.?
036100170704         if  C1CsrRrn > *zero;
036200170704           C1RcdNbr = C1CsrRrn;
036300170704         else;
036400170704           C1RcdNbr = 1;
036500170704           write  SF25S00;     //?(no rec.)?
036600170704         endif;
036700170704
036800170706         // -?Emissione videata con subfile?
036900170704         exfmt  SF25C01;
037000170704
037100170704         clear  VIDmsg;
037200170704         reset  $ErrMessage;
037300170704         reset  $ErrGenerico;
037400170704
037500170704         reset  $Cancellazione;
037600170704         reset  $Immissione;
037700170706         reset  $Protect;
037800170704
037900170704         SELECT;
038000170704
038100170704           // -?F3=Fine?
038200170704           WHEN  dsp_aid = c_F03;
038300170704             $Fine   = *on;
038400170704
038500170704           // -?F10=Inserimento Filiale?
038600170704           WHEN  dsp_aid = c_F10;
038700170704             $Immissione = *on;
038800170704             $Video  = 'D2';
038900170704             $InzD02 = *on;
039000170704
039100170704           // -?F12=Ritorno?
039200170704           WHEN  dsp_aid = c_F12;
039300170704             $Fine   = *on;
039400170704
039500170704           // -?Subfile vuoto?
039600170704           WHEN  S01nrr = *zero;
039700170705             if  C1Civa  <> SavC1Civa;
039800170704               $InzS01 = *on;
039900170706               SavC1Civa = C1Civa;
040000170704             endif;
040100170704
040200170704           // -?Invio?
040300170704           OTHER;
040400170704             // -?Gestione opzioni?
040500170704             exsr  sr_OpzS01;
040600170704             if  $ErrGenerico;
040700170704               leavesr;
040800170704             endif;
040900170704             // -?Richiesto Posizionamento?
041000170705             if  C1Civa <> SavC1Civa;
041100170704               $InzS01 = *on;
041200170705               SavC1Civa = C1Civa;
041300170704             endif;
041400170704
041500170704         ENDSL;
041600170704
041700170704       ENDSR;
041800170704
041900170704       //--------------------------------------------------------------
042000170704       //?Inizializzazione subfile S01.                                ?
042100170704       //--------------------------------------------------------------
042200170704       BEGSR  sr_InzS01;
042300170704
042400170704         // -?Spegnimento degli indicatori di errore?
042500170704         %subst( IndDspF : 50 ) = *off;
042600170704
042700170704         // -?Pulizia subfile?
042800170704         $SflDsp_N    = *on;
042900170704         $SflDspCtl_N = *on;
043000170704         write  SF25C01;
043100170704         $SflDspCtl_N = *off;
043200170704         $SflEnd      = *off;
043300170704
043400170704         clear  C1RcdNbr;
043500170704         clear  C1CsrRrn;
043600170704         clear  S01nrr;
043700170704         clear  VIDmsg;
043800170704         $ErrMessage  = *off;
043900170704         $ErrGenerico = *off;
044000170704
044100170704         $SflNxtChg = *off;
044200170704
044300170704         // -?Caricamento completo dei dati nel subfile (per codice)?
044400170704         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
044500170704         //  ?l'eventuale ordinamento)?
044600170704         clear  keyANSPI01;
044700170706         if  C1Civa <> *blank;
044800170706           k_SPIiva = C1Civa;
044900170706         endif;
045000170704         setll  %kds( keyANSPI01 )  ANSPI001;
045100170704         read  ANSPI001;
045200170704
045300170704         DoW  Not %eof( ANSPI01L )  and  S01nrr < c_MaxRec;
045400170704           exsr  sr_WriteS01;
045500170704           read  ANSPI001;
045600170704         EndDo;
045700170704
045800170704         // -?Visualizzazione del SFL (se ci sono dati)?
045900170704         $SflDsp_N = ( S01nrr <= *zero );
046000170704
046100170704         // -?Attivazione del SFLEND?
046200170704         $SflEnd   = %eof( ANSPI01L );
046300170704
046400170704         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
046500170704         if  S01nrr > *zero;
046600170704           C1RcdNbr = 1;
046700170704         else;
046800170704           clear  C1RcdNbr;
046900170704         endif;
047000170704
047100170704         // -?Impostazione Opzioni?
047200180110         if  %subst( kNmUs : 1 : 3 ) = 'EDP';
047300180110           C1Dopz = '2=Modifica, 4=Cancellazione, 5=Visualizzazione';
047400180110         else;
047500180110           C1Dopz = '2=Modifica, 5=Visualizzazione';
047600180110         endif;
047700170704
047800170704         // -?Emissione messaggio di segnalazione raggiungimento limite?
047900170704         if  S01nrr >= c_MaxRec   and   Not %eof( ANSPI01L );
048000170704           $ErrGenerico = *on;
048100170704           $ErrMessage  = *on;
048200170704           $PosCurOPZ   = *on;
048300170704           VIDmsg = sk_Msg(01);
048400170704           leavesr;
048500170704         endif;
048600170704
048700170704       ENDSR;
048800170704
048900170704       //--------------------------------------------------------------
049000170704       //?Registrazione del singolo rec. nel subfile S01.              ?
049100170704       //--------------------------------------------------------------
049200170704       BEGSR  sr_WriteS01;
049300170704
049400170704         // -?Eventuale posizionamento richiesto?
049500170706         if  SPIiva < C1Civa;
049600170704           leavesr;
049700170704         endif;
049800170704
049900170704         // -?Impostazione campi nel record del subfile?
050000170704         clear  SF25S01;
050100170704
050200170705         S1Hnrr   = InfANSPI.db_rrn;
050300170704         S1Hdde   = SPIdde;
050400170704         S1Hdsc   = SPIdsc;
050500170704         S1Hdim   = SPIdim;
050600170704         S1Hdmo   = SPIdmo;
050700170704
050800170704         S1Civa   = SPIiva;
050900170704
051000170704         reset  WLBdat;
051100170704         G08inv = SPIdde;
051200170704         XSRDA8( WLBdat );
051300170704         if  G08err = *zero;
051400170704           S1Ddde = G08dat;
051500170704         endif;
051600170704
051700170704         reset  WLBdat;
051800170704         G08inv = SPIdsc;
051900170704         XSRDA8( WLBdat );
052000170704         if  G08err = *zero;
052100170704           S1Ddsc = G08dat;
052200170704         endif;
052300170704
052400170707         reset  WLBdat;
052500170707         G08inv = SPIdim;
052600170707         XSRDA8( WLBdat );
052700170707         if  G08err = *zero;
052800170707           S1Ddim = G08dat;
052900170707         endif;
053000170707
053100170707         if  SPIdmo <> *zero;
053200170707           reset  WLBdat;
053300170707           G08inv = SPIdmo;
053400170707           XSRDA8( WLBdat );
053500170707           if  G08err = *zero;
053600170707             S1Ddmo = G08dat;
053700170707           endif;
053800170707         endif;
053900170704
054000170704         // -?Registrazione singolo record nel subfile?
054100170704         S01nrr += 1;
054200170704         write  SF25S01;
054300170704
054400170704       ENDSR;
054500170704
054600170704       //--------------------------------------------------------------
054700170704       //?Gestione opzioni del subfile S01                             ?
054800170704       //--------------------------------------------------------------
054900170704       BEGSR  sr_OpzS01;
055000170704
055100170704         // -?Spegnimento degli indicatori di errore?
055200170704         %subst( IndDspF : 50 ) = *off;
055300170704
055400170704         clear  SavS01csr;
055500170704
055600170704         // -?Ciclo di lettura subfile?
055700170704         readc  SF25S01;
055800170704
055900170704         DoW  Not %eof(TNSF25D);
056000170704
056100170704           $SflNxtChg = *off;
056200170704           %subst( IndDspF : 50 ) = *off;
056300170704           C1RcdNbr   = S01nrr;
056400170704
056500170704           Select;
056600170704
056700170704             // -?Nessuna opzione?
056800170704             When  S1Copz = *blank;
056900170704
057000170704             // -?Opz. 2=Modifica/Cancellazione?
057100170704             When  S1Copz  = '2';
057200170704               $ProtectKEY = *on;
057300170704               $InzD02     = *on;
057400170704               $Video      = 'D2';
057500170704               DoW  $Video = 'D2';
057600170704                 exsr  sr_GesD02;
057700170704               EndDo;
057800170704               clear  $ProtectKEY;
057900170704
058000170707             // -?Opz. 4=Cancellazione?
058100170707             //  ?Opz. 5=Visualizzazione?
058200180110             When  S1Copz  = '4'  and
058300180110                   %subst( kNmUs : 1 : 3 ) <> 'EDP';
058400180110               $ErrGenerico = *on;
058500180110               $ErrMessage  = *on;
058600180110               $PosCurOPZ   = *on;
058700180110               VIDmsg = sk_Msg(02);
058800180110
058900170707             When  S1Copz  = '4'  or
059000170707                   S1Copz  = '5';
059100170707               $Cancellazione = ( S1Copz = '4' );
059200170704               $Protect    = *on;
059300170704               $InzD02     = *on;
059400170704               $Video      = 'D2';
059500170704               DoW  $Video = 'D2';
059600170704                 exsr  sr_GesD02;
059700170704               EndDo;
059800170704               clear  $Protect;
059900170704
060000170704             // -?Opzione errata?
060100170704             Other;
060200170704               $ErrGenerico = *on;
060300170704               $ErrMessage  = *on;
060400170704               $PosCurOPZ   = *on;
060500170704               VIDmsg = sk_Msg(02);
060600170704
060700170704           EndSl;
060800170704
060900170704           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
061000170704           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
061100170704           if  S1Copz   <> *blank  and
061200170704              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
061300170704             SavS01csr   = C1RcdNbr;
061400170704           endif;
061500170704
061600170704           // -?Aggiornamento sfl?
061700170704           if  $ErrGenerico;
061800170704             C1CsrRrn   = C1RcdNbr;
061900170707             $SflNxtChg = *on;
062000170707             //*·clear  S1Copz;
062100170704           else;
062200170704             clear  S1Copz;
062300170704           endif;
062400170704
062500170704           UPDATE  SF25S01;
062600170704
062700170704           if  $ErrGenerico  or  $ErrMessage;
062800170704             leave;
062900170704           endif;
063000170704
063100170704           readc  SF25S01;
063200170704
063300170704         ENDDO;
063400170704
063500170704         // -?Riposizionam. cursore sul record contenente la prima opz.?
063600170704         //  ?(se non sono stati rilevati errori)?
063700170704         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
063800170704             SavS01csr > *zero;
063900170704           C1CsrRrn = SavS01csr;
064000170704         endif;
064100170704
064200170704       ENDSR;
064300170704
064400170704       //--------------------------------------------------------------
064500170704       //?Gestione videata D02.                                        ?
064600170704       //--------------------------------------------------------------
064700170704       BEGSR  sr_GesD02;
064800170704
064900170704         // -?Inizializzazione videata?
065000170704         if  $InzD02 = *on;
065100170704           exsr  sr_InzD02;
065200170704           $InzD02 = *off;
065300170704         endif;
065400170706
065500170706         // -?Cancellazione (da opz. 4 => $Cancellazione) NON ammessa?
065600170706         //  ?per la presenza di fatture con Registro Iva 913?
065700170706         //  ?(quella da F16 è già stata disabilitata nella subr. sr_InzD02)?
065800180110         If  $Cancellazione  and  $TIIFT_found;
065900170706           $Video = 'S1';
066000170707           VIDmsg = %trimr( sk_Msg(12) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
066100170706           $ErrMessage  = *on;
066200170706           $ErrGenerico = *on;
066300170706           leavesr;
066400170706         EndIf;
066500170704
066600170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
066700170704         write  SF25T01;
066800170704         write  SF25P01;
066900170704
067000170704         // -?Emissione videata?
067100170704         if  Not $Protect;
067200170704           exfmt  SF25D02;
067300170704         else;
067400170704           write  SF25D02;
067500170704           exfmt  Protect;
067600170704         endif;
067700170704
067800170704         clear  VIDmsg;
067900170704         reset  $ErrMessage;
068000170704         reset  $ErrGenerico;
068100170704
068200170704         SELECT;
068300170704
068400170704           // -?F3=Fine?
068500170704           WHEN  dsp_aid = c_F03;
068600170705             unlock  ANSPI00F;
068700170704             $Video = 'S1';
068800170704             $ErrGenerico = *on;
068900170704             if  $Immissione;
069000170704               $Fine = *on;
069100170704             endif;
069200170704
069300170704           // -?F12=Ritorno?
069400170704           WHEN  dsp_aid = c_F12;
069500170705             unlock  ANSPI00F;
069600170704             $Video = 'S1';
069700170704
069800170704           // -?Enter / F6=Conferma / F16=Cancellazione?
069900170704           OTHER;
070000170704             // -?Controlli eseguiti NON se F16=Annullamento?
070100170707             //*·if  Not $Cancellazione;
070200170707             if  dsp_Aid <> c_F16;
070300170704               exsr  sr_CtrD02;
070400170704             endif;
070500170704             if  $ErrGenerico;
070600170704               leavesr;
070700170704             endif;
070800170706             // -?Aggiornamento?
070900170704             If  dsp_aid = c_F06   or
071000170704                 dsp_aid = c_F16;
071100170704               exsr  sr_Upd_ANSPI;
071200170704               // -?Rilevato errore in fase di aggiornamento?
071300170704               if  $ErrGenerico = *on;
071400170704                 leavesr;
071500170704               endif;
071600170704               // -?SubFile iniziale altrimenti?
071700170704               $Video  = 'S1';
071800170704               $InzS01 = *on;
071900170704             EndIf;
072000170704
072100170704         ENDSL;
072200170704
072300170704       ENDSR;
072400170704
072500170704       //--------------------------------------------------------------
072600170704       //?Inizializzazione videata D02.                                ?
072700170704       //--------------------------------------------------------------
072800170704       BEGSR  sr_InzD02;
072900170704
073000170704         // -?Spegnimento degli indicatori di errore?
073100170706         %subst( IndDspF : 41 ) = *off;
073200170704
073300170704         // -?Pulizia videata?
073400170704         clear  SF25D02;
073500170704
073600170704         // -?Reperimento dati in gestione?
073700170704         //  ?(SE NON Immissione)?
073800170704         If  Not $Immissione;
073900170704
074000170704           // -?Reperimento dati?
074100170707           if  $Protect;
074200170705             chain(N)  S1Hnrr  ANSPI000;
074300170704           else;
074400170705             chain  S1Hnrr  ANSPI000;
074500170704           endif;
074600170704
074700170704           // -?Caricamento dati a video?
074800170705           if  %found( ANSPI00F );
074900170704             exsr  sr_RieD02;
075000170704           else;
075100170704             VIDmsg = sk_Msg(03);
075200170704             //*·$PosCurIVA   = *on;
075300170704             $ErrMessage  = *on;
075400170704             $ErrGenerico = *on;
075500170704             //*·leavesr;
075600170704           endif;
075700170704
075800170704         Else;
075900170704
076000170704           // -?Impostazione dati di default SE immissione?
076100170704           V2Ddde = *date;
076200170704           V2Ddsc = c_MaxDat_Eur;
076300170704
076400170704         EndiF;
076500170704
076600170704         // -?Impostazione testata?
076700170704         clear  V2Topz;
076800170704         Select;
076900170706           When  $Cancellazione;
077000170706             V2Topz = ' CANCELLAZIONE ';
077100170704           When  $Immissione;
077200170704             V2Topz = '  INSERIMENTO  ';
077300170706           When  $Protect;
077400170706             V2Topz = 'VISUALIZZAZIONE';
077500170704           Other;
077600170704             V2Topz = '   MODIFICA    ';
077700170704         EndSl;
077800170704
077900170704         // -?Impostazione attributi di visualizzazione?
078000170707         $ProtectKEY = ( Not $Immissione );
078100170707         //*·$ProtectDDE = ( Not $Immissione  and
078200170704         //*·                SPIdde <= wDate );
078300170707         //*·$ProtectDSC = ( Not $Immissione  and
078400170706         //*·                SPIdsc <= wDate );
078500170704
078600170706         // -?(Dis)Abilitazione tasti funzionali?
078700170706         exsr  sr_AbilitFxx;
078800170704
078900170704       ENDSR;
079000170704
079100170704       //--------------------------------------------------------------
079200170705       //?Caricamento dati del singolo record del file ANSPI00F nella  ?
079300170704       //?videata D02.                                                 ?
079400170704       //--------------------------------------------------------------
079500170704       BEGSR  sr_RieD02;
079600170704
079700170707         V2Civa = SPIiva;
079800170707         V2Dima = SPIima;
079900170704
080000170707         reset  WLBdat;
080100170707         G08inv   = SPIdde;
080200170707         XSRDA8(WLBdat);
080300170707         if  G08err = *zero;
080400170707           V2Ddde = G08dat;
080500170707         endif;
080600170704
080700170707         reset  WLBdat;
080800170707         G08inv   = SPIdsc;
080900170707         XSRDA8(WLBdat);
081000170707         if  G08err = *zero;
081100170707           V2Ddsc = G08dat;
081200170707         endif;
081300170705
081400170707         if  SPIdim <> *zero  or  SPIpim <> *blank;
081500170707           reset  WLBdat;
081600170707           G08inv   = SPIdim;
081700170707           XSRDA8(WLBdat);
081800170707           if  G08err = *zero;
081900170707             V2Ddim = G08dat;
082000170707           endif;
082100170707         endif;
082200170707         V2Dhim = SPIhim;
082300170707         V2Dpim = SPIpim;
082400170705
082500170707         if  SPIdmo <> *zero  or  SPIpmo <> *blank;
082600170707           reset  WLBdat;
082700170707           G08inv   = SPIdmo;
082800170707           XSRDA8(WLBdat);
082900170707           if  G08err = *zero;
083000170707             V2Ddmo = G08dat;
083100170707           endif;
083200170707         endif;
083300170707         V2Dhmo = SPIhmo;
083400170707         V2Dpmo = SPIpmo;
083500170706
083600170706         // -?Ricerca della 1ª data fattura del cliente con Registro IVA?
083700170706         //  ?"Split Payment" (913)?
083800170706         exsr  sr_RicercaDataFattura913_Prima;
083900170706
084000170706         // -?Ricerca dell'ultima data fattura del cliente con Registro IVA?
084100170706         //  ?"Split Payment" (913)?
084200170706         exsr  sr_RicercaDataFattura913_Ultima;
084300170705
084400170705         // -?Impostazione Attributi di visualizzazione?
084500170707         $VisualINS = ( Not $Immissione );
084600170707         $VisualMOD = ( Not $Immissione  and  SPIdmo > *zero );
084700170704
084800170704       ENDSR;
084900170704
085000170704       //--------------------------------------------------------------
085100170704       //?Controllo dati videata D02.                                  ?
085200170704       //--------------------------------------------------------------
085300170704       BEGSR  sr_CtrD02;
085400170704
085500170704         // -?Spegnimento degli indicatori di errore?
085600170704         %subst(IndDspF : 50) = *off;
085700170704
085800170714         // -?Controllo inserimento Partita Iva / Codice Fiscale?
085900170705         clear  V2Diva;
086000170705         If  V2Civa = *blank;
086100170705           VIDmsg = sk_Msg(04);
086200170705           $PosCurIVA   = *on;
086300170704           $ErrMessage  = *on;
086400170704           $ErrGenerico = *on;
086500170704           leavesr;
086600170704         EndIf;
086700170705
086800170714         // -?Controllo correttezza Partita Iva inserita?
086900170710         //  ?(solo ITALIANA può essere)?
087000170710         If  $Immissione;
087100170710           clear  xCFIVA1ds;
087200170710           xCFIVAmod = 'P';
087300170710           // -?Partita Iva con Nazione davanti?
087400170710           if  %subst( V2Civa : 1 : 2 ) < '00';
087500170710             xCFIVApar = V2Civa;
087600170710           // -?Partita Iva senza Nazione davanti (Italiana)?
087700170710           else;
087800170710             xCFIVApar = '  ' + V2Civa;
087900170710           endif;
088000170710           xCFIVAr1 ( xCFIVA1ds );
088100170714           if  xCFIVAflg < *zero;
088200170714             clear  xCFIVA1ds;
088300170714             //*·xCFIVAmod = *blank;
088400170714             // -?Codice Fiscale (Italiano)?
088500170714             xCFIVApar = V2Civa;
088600170714             xCFIVAr1 ( xCFIVA1ds );
088700170714           endif;
088800170710           if  xCFIVAflg < *zero;
088900170710             VIDmsg = xCFIVAmsg;
089000170710             $PosCurIVA   = *on;
089100170710             $ErrMessage  = *on;
089200170710             $ErrGenerico = *on;
089300170710             leavesr;
089400170710           endif;
089500170710         EndIf;
089600170704
089700170706         // -?Controllo che la P.I. non risulti già inserita?
089800170706         //  ?(anche se per periodo già scaduto...)?
089900170707         If  $Immissione;
090000170706           clear  keyANSPI01;
090100170706           k_SPIiva = V2Civa;
090200170706           chain  %kds( keyANSPI01 )  ANSPI001;
090300170706           if  %found( ANSPI01L );
090400170707             VIDmsg = sk_Msg(05);
090500170706             reset  WLBdat;
090600170706             G08inv   = SPIdde;
090700170706             XSRDA8(WLBdat);
090800170706             VIDmsg = %trimr( VIDmsg ) + ' (per il periodo dal ' +
090900170706                      %trim( %editw( G08dat : '  /  /    ' ) );
091000170706             reset  WLBdat;
091100170706             G08inv   = SPIdsc;
091200170706             XSRDA8(WLBdat);
091300170706             VIDmsg = %trimr( VIDmsg ) + ' al ' +
091400170706                      %trim( %editw( G08dat : '  /  /    ' ) ) + ')';
091500170707             $PosCurIVA   = *on;
091600170706             $ErrMessage  = *on;
091700170706             $ErrGenerico = *on;
091800170706             leavesr;
091900170706           endif;
092000170706         EndIf;
092100170704
092200170704         // -?Controllo periodo validità?
092300170705         // ·?Data Decorrenza?
092400170704         clear  WLBdat;
092500170705         G08dat = V2Ddde;
092600170705         XSRDA8( WLBdat );
092700170704         if  G08err = *zero;
092800170705           V2Ddde = G08dat;
092900170705           W2Ddde = G08inv;
093000170704         else;
093100170707           VIDmsg = sk_Msg(06);
093200170705           $PosCurDDE   = *on;
093300170704           $ErrMessage  = *on;
093400170704           $ErrGenerico = *on;
093500170704           leavesr;
093600170704         endif;
093700170704         // ·?Data Scadenza?
093800170704         clear  WLBdat;
093900170705         G08dat = V2Ddsc;
094000170705         XSRDA8( WLBdat );
094100170704         if  G08err = *zero;
094200170705           V2Ddsc = G08dat;
094300170705           W2Ddsc = G08inv;
094400170704         else;
094500170707           VIDmsg = sk_Msg(07);
094600170705           $PosCurDSC   = *on;
094700170704           $ErrMessage  = *on;
094800170704           $ErrGenerico = *on;
094900170704           leavesr;
095000170704         endif;
095100170704         // ·?Range di date?
095200170705         if  W2Ddde > W2Ddsc;
095300170707           VIDmsg = sk_Msg(08);
095400170705           $PosCurDSC   = *on;
095500170704           $ErrMessage  = *on;
095600170704           $ErrGenerico = *on;
095700170704           leavesr;
095800170704         endif;
095900170704
096000170706         // -?La Data Decorrenza NON deve essere successiva alla 1ª data?
096100170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
096200170706         exsr  sr_RicercaDataFattura913_Prima;
096300170831         If  $TIIFT_found  and  W2Ddde > IFTdft;
096400170707           VIDmsg = %trimr( sk_Msg(09) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
096500170705           $PosCurDDE   = *on;
096600170705           $ErrMessage  = *on;
096700170705           $ErrGenerico = *on;
096800170705           leavesr;
096900170705         EndIf;
097000170705
097100170706         // -?La Data Scadenza NON deve essere precedente all'ultima data?
097200170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
097300170706         exsr  sr_RicercaDataFattura913_Ultima;
097400170831         If  $TIIFT_found  and  W2Ddsc < IFTdft;
097500170707           VIDmsg = %trimr( sk_Msg(10) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
097600170705           $PosCurDSC   = *on;
097700170705           $ErrMessage  = *on;
097800170705           $ErrGenerico = *on;
097900170705           leavesr;
098000170705         EndIf;
098100170704
098200170705         // -?Verifica esistenza in ANSPI01L?
098300170705         //  ?(con sovrapposizione di giorni nel periodo)?
098400170704         // ·?Esempi - Periodo esistente:?· · 3-----6 · ·?
098500170705         // ·? 1)     Periodo inseribile: 1-2?
098600170705         // ·? 2) Periodo NON inseribile:   2---4?
098700170705         // ·? 3) Periodo NON inseribile:       4-5?
098800170705         // ·? 4) Periodo NON inseribile:         5---7?
098900170705         // ·? 5) Periodo NON inseribile:   2---------7?
099000170705         // ·? 6)     Periodo inseribile:             7-8?
099100170707         IF  $Immissione;
099200170705
099300170705           clear  keyANSPI01;
099400170705           k_SPIiva = V2Civa;
099500170705           setll  %kds( keyANSPI01 )  ANSPI001;
099600170705           reade  %kds( keyANSPI01 )  ANSPI001;
099700170705
099800170705           DoW  NOT %eof( ANSPI01L );
099900170705
100000170705             select;
100100170705               // ·? 2) e 3)?
100200170705               when  ( W2Ddsc >= SPIdde  and
100300170705                       W2Ddsc <= SPIdsc );
100400170707                 VIDmsg = sk_Msg(11);
100500170705                 $PosCurDDE   = *on;
100600170705                 $ErrMessage  = *on;
100700170705                 $ErrGenerico = *on;
100800170705                 leavesr;
100900170705               // ·? 3) e 4)?
101000170705               when  ( W2Ddde >= SPIdde  and
101100170705                       W2Ddde <= SPIdsc );
101200170707                 VIDmsg = sk_Msg(11);
101300170705                 $PosCurDDE   = *on;
101400170705                 $ErrMessage  = *on;
101500170705                 $ErrGenerico = *on;
101600170705                 leavesr;
101700170705               // ·? 5)?
101800170705               when  ( W2Ddde <= SPIdde  and
101900170705                       W2Ddde >= SPIdsc );
102000170707                 VIDmsg = sk_Msg(11);
102100170705                 $PosCurDDE   = *on;
102200170705                 $ErrMessage  = *on;
102300170705                 $ErrGenerico = *on;
102400170705                 leavesr;
102500170705             endsl;
102600170705
102700170705             reade  %kds( keyANSPI01 )  ANSPI001;
102800170705
102900170704           EndDo;
103000170705
103100170705         ENDIF;
103200170704
103300170704       ENDSR;
103400170706
103500170706       //--------------------------------------------------------------
103600170706       //?Settaggio degli indicatori per la (dis)abilitazione dei tasti?
103700170706       //?  funzionali nelle diverse videate.                          ?
103800170706       //--------------------------------------------------------------
103900170706       BEGSR  sr_AbilitFxx;
104000170706
104100170706         // -?F3 = Fine?
104200170706         $F3Attivo  = *on;
104300170706
104400170706         // -?F6 = Conferma?
104500170706         $F6Attivo  = ( $Video = 'D2'  and  Not $Protect );
104600170706
104700170706         // -?F10 = Inserimento?
104800170706         $F10Attivo = ( $Video = 'S1' );
104900170706
105000170706         // -?F12 = Ritorno?
105100170706         $F12Attivo = ( $Video = 'D2' );
105200170706
105300170706         // -?F16 = Cancellazione?
105400170706         $F16Attivo = ( $Video = 'D2'     and
105500170706                        Not $Immissione   and
105600170706                        %found(ANSPI00F)  and
105700180110                        Not $TIIFT_found  and
105800170706                      ( Not $Protect      or
105900180110                        $Cancellazione )  and
106000180110                        %subst( kNmUs : 1 : 3 ) = 'EDP' );
106100180110
106200170706       ENDSR;
106300170706
106400170706       //--------------------------------------------------------------
106500170706       //?Ricerca della data della PRIMA fattura con Registro IVA 913  ?
106600170706       //?per il cliente.                                              ?
106700170706       //--------------------------------------------------------------
106800170706       BEGSR  sr_RicercaDataFattura913_Prima;
106900170706
107000170706         clear  V2Dtxd;
107100170726         clear  $TIIFT_found;
107200170706
107300170706         clear  keyTIIFT02;
107400170706         k_IFTpiva = V2Civa;
107500170706         k_IFTfiv  = dsQC2.§QCfi7;
107600170717         //*·k_IFTdft  = *zero;
107700170706
107800170726         setll  %kds( keyTIIFT02 )  TIIFT002;
107900170726         reade  %kds( keyTIIFT02 : 2 )  TIIFT002;
108000170706
108100170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
108200170726
108300170726         If  Not $TIIFT_found;
108400170726
108500170726           clear  keyTIIFT03;
108600170726           k3_IFTcdf = V2Civa;
108700170726           k3_IFTfiv = dsQC2.§QCfi7;
108800170726           //*·k3_IFTdft = *zero;
108900170726
109000170726           setll  %kds( keyTIIFT03 )  TIIFT003;
109100170726           reade  %kds( keyTIIFT03 : 2 )  TIIFT003;
109200170726
109300170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
109400170726
109500170726         EndIf;
109600170726
109700170726         If  $TIIFT_found;
109800170706
109900170706           reset  WLBdat;
110000170706           G08inv   = IFTdft;
110100170706           XSRDA8(WLBdat);
110200170706
110300170706           V2Dtxd = 'Fatt. con Reg. IVA ' +
110400170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' dal ' +
110500170706                    %editw( G08dat : '  /  /    ' );
110600170706
110700170706         EndIf;
110800170706
110900170706       ENDSR;
111000170706
111100170706       //--------------------------------------------------------------
111200170706       //?Ricerca della data dell'ULTIMA fattura con Registro IVA 913  ?
111300170706       //?per il cliente.                                              ?
111400170706       //--------------------------------------------------------------
111500170706       BEGSR  sr_RicercaDataFattura913_Ultima;
111600170706
111700170706         clear  V2Dtxs;
111800170726         clear  $TIIFT_found;
111900170706
112000170706         clear  keyTIIFT02;
112100170706         k_IFTpiva = V2Civa;
112200170706         k_IFTfiv  = dsQC2.§QCfi7;
112300170706         k_IFTdft  = *hival;
112400170726
112500170726         setgt  %kds( keyTIIFT02 )  TIIFT002;
112600170726         readPe  %kds( keyTIIFT02 : 2 )  TIIFT002;
112700170726
112800170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
112900170726
113000170726         If  Not $TIIFT_found;
113100170726
113200170726           clear  keyTIIFT03;
113300170726           k3_IFTcdf = V2Civa;
113400170726           k3_IFTfiv = dsQC2.§QCfi7;
113500170726           k3_IFTdft = *hival;
113600170726
113700170726           setgt  %kds( keyTIIFT03 )  TIIFT003;
113800170726           readPe  %kds( keyTIIFT03 : 2 )  TIIFT003;
113900170726
114000170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
114100170726
114200170726         EndIf;
114300170706
114400170726         If  $TIIFT_found;
114500170706
114600170706           reset  WLBdat;
114700170706           G08inv   = IFTdft;
114800170706           XSRDA8(WLBdat);
114900170706
115000170706           V2Dtxs = 'Fatt. con Reg. IVA ' +
115100170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' al  ' +
115200170706                    %editw( G08dat : '  /  /    ' );
115300170706
115400170706         EndIf;
115500170706
115600170706       ENDSR;
115700170704
115800170704       //--------------------------------------------------------------
115900170704       //?Aggiornamento dati nel *file ANSPI00F.                       ?
116000170704       //--------------------------------------------------------------
116100170704       BEGSR  sr_Upd_ANSPI;
116200170704
116300170705         // -?Aggancio record di ANSPI00F?
116400170707         If  Not $Immissione;
116500170705           chain  S1Hnrr  ANSPI000;
116600170705         EndIf;
116700170705
116800170705         // -?Reperimento data odierna?
116900170705         wDate = %int( %subst( %char( %dec( %timestamp() ) )
117000170705                               : 1 : 8 ) );
117100170705         // -?Reperimento orario attuale?
117200170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
117300170705                               : 9 : 4 ) );
117400170704
117500170704         // -?Impostazione dati?
117600170707         If  $Immissione  or
117700170705             ( NOT %found( ANSPI00F ) and
117800170704               NOT $Cancellazione );
117900170705           clear  ANSPI000;
118000170705           SPIiva = V2Civa;
118100170705           SPIdim = wDate;
118200170705           SPIhim = wTime;
118300170705           SPIpim = kNmUs;
118400170705           SPIima = 'M';            //?(M=Inserimento Manuale)?
118500170705         EndIf;
118600170704
118700170707         //*·If  Not $Cancellazione;
118800170707         If  dsp_aid <> c_F16;
118900170707
119000170705           SPIdde = W2Ddde;
119100170705           SPIdsc = W2Ddsc;
119200170705
119300170707           If  Not $Immissione;
119400170707             SPIdmo = wDate;
119500170707             SPIhmo = wTime;
119600170707             SPIpmo = kNmUs;
119700170707           EndIf;
119800170707
119900170707         EndIf;
120000170704
120100170704         // -?Aggiornamento record?
120200170704         SELECT;
120300170704
120400170704           // -?F16=Cancellazione?
120500170707           //*·WHEN  $Cancellazione  and  %found( ANSPI00F );
120600170707           WHEN  dsp_aid = c_F16  and  %found( ANSPI00F );
120700170705             //__________________
120800170705             delete(E)  ANSPI000;
120900170705             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121000170704
121100170704           // -?Immissione o Copia o Modifica?
121200170704           OTHER;
121300170707             if  Not $Immissione  and
121400170705                 %found( ANSPI00F );
121500170704               //_________________
121600170705               update(E) ANSPI000;
121700170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121800170704             else;
121900170704               //_________________
122000170705               write(E)  ANSPI000;
122100170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
122200170704             endif;
122300170704
122400170704         ENDSL;
122500170704
122600170704         // -?Segnalazione errore rilevato in fase di?
122700170704         //  ?scrittura/aggiornamento/cancellazione?
122800170704         If  %error();
122900170707           VIDmsg = sk_Msg(13);
123000170704           $ErrMessage  = *on;
123100170704           $ErrGenerico = *on;
123200170704           // -?Stampa del Dump?
123300170704           Dump(A);
123400170704           // -?Stampa del Job-Log?
123500170704           Qcmd = 'DSPJOBLOG job(*) output(*print)';
123600170704           exsr  sr_ExecCmd;
123700170704           leavesr;
123800170704         EndIf;
123900170704
124000170704       ENDSR;
124100170704
124200170704       //--------------------------------------------------------------
124300170704       //?Esecuzione del comando (già impostato).                      ?
124400170704       //--------------------------------------------------------------
124500170704       BEGSR  sr_ExecCmd;
124600170704
124700170704         clear Qcap0100;
124800170704         Qcabcsdh = *off;
124900170704         Qcapa    = *off;
125000170704         Qcacmdss = *off;
125100170704         Qcaerved = *allX'00';
125200170704
125300170704         clear Qusec;
125400170704         Qusbprv  = %size(Qusec);
125500170704
125600170704         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
125700170704                           %size(Qcap0100) : 'CPOP0100' : *omit :
125800170704                           0 : 0 : Qusec);
125900170704
126000170704         //*·if  Qusei <> *blank;
126100170704         //*·  exsr  sr_PrintErr;
126200170704         //*·endif;
126300170704
126400170704       ENDSR;
126500170704
126600170704       //--------------------------------------------------------------
126700170704       //?Operazioni finali.                                           ?
126800170704       //--------------------------------------------------------------
126900170704       BEGSR  sr_RoutEnd;
127000170707
127100170707         // -?Chiusura funzioni precedentemente aperte?
127200170707         cloTabella ( c_Tabel );
127300170704
127400170704         //  ?Uscita?
127500170704         return;
127600170704
127700170704       ENDSR;
127800170704
127900170704      /end-free
128000170704
128100170704       //--------------------------------------------------------------
128200170704       //?Definizione schiere a tempo di compilazione.                 ?
128300170704       //--------------------------------------------------------------
128400170704
128500170704** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
128600170704Raggiunto il limite massimo dei record caricabili a video                       1
128700170704Opzione errata                                                                  2
128800170704Partita Iva NON più reperibile in questo archivio                               3
128900170704Immettere la Partita Iva                                                        4
129000170707Partita Iva già inserita                                                        5
129100170707Data decorrenza formalmente errata                                              6
129200170707Data scadenza formalmente errata                                                7
129300170707Data di scadenza precedente a quella di decorrenza                              8
129400170707Data decorrenza successiva quella della 1ª fattura con Registro IVA             9
129500170707Data scadenza precedente quella dell'ultima fattura con Registro IVA           10
129600170707Il periodo si sovrappone a quello di un'altro Split Payment già inserito       11
129700170707Partita Iva NON cancellabile: trovate fatture con Registro IVA                 12
129800170707Rilevato errore in fase di registrazione dati => Avvisare CED                  13
