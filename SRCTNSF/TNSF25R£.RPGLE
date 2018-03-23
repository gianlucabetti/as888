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
017200170705     d $TIIFT_FI7      s               n   inz
017300170726     d $TIIFT_found    s               n   inz
017400170704
017500170704       // -?Variabili per la gestione del video?
017600170704     d $Video          s              2    inz('S1')
017700170704     d S01nrr          s                   like(C1RcdNbr) inz
017800170704     d SavS01csr       s                   like(C1RcdNbr) inz
017900170704
018000170704       // -?Variabili per la gestione del posizionamento nel subfile?
018100170704     d SavC1Civa       s                   like(C1Civa)   inz
018200170704
018300170704       // -?Campi di comodo?
018400170704     d wDate           s              8s 0 inz
018500170705     d wTime           s              4s 0 inz
018600170705     d W2Ddde          s                   like(SPIdde)   inz
018700170705     d W2Ddsc          s                   like(SPIdsc)   inz
018800170704
018900170704       //--------------------------------------------------------------
019000170704       //?Definizione prototipi procedure.                             ?
019100170704       //--------------------------------------------------------------
019200170704
019300170704       // -?Reperimento dati utente?
019400170704     d TIBS34ds      e ds
019500170704      /copy gaitrasrc/srcProtoPR,TIBS34R
019600170710
019700170710       // -?Controllo Codice Fiscale e/o Partita Iva Europea (rel.1)?
019800170710     d xCFIVA1ds     e ds                  inz
019900170710      /copy gaitrasrc/srcProtoPR,XCFIVAR1
020000170704
020100170704       // -?Reperimento dati tabelle?
020200170704      /copy gaitrasrc/srcProtoPI,TRULTAB
020300170704      /copy gaitrasrc/srcProtoPR,TRULTAB
020400170704
020500170704       // -?Controllo/Inversione data?
020600170704     d WLBdat          ds                  inz
020700170704     d   G08dat                       8  0 inz
020800170704     d   G08inv                       8  0 inz
020900170704     d   G08err                       1    inz('3')
021000170704     d   G08tgi                       5  0 inz
021100170704      /copy gaitrasrc/srcProtoPR,XSRDA8
021200170704
021300170704       // -?Parametri API QCAPCMD (Process Commands)?
021400170704     d Qcmd            s          32740    varying        inz
021500170704      /copy qSysInc/qRpgleSrc,QCAPCMD
021600170704       // -?API QCAPCMD (Process Commands)?
021700170704      /copy gaitrasrc/srcProtoPR,QCAPCMD
021800170704
021900170704       // -?Parametri gestione errori API.?
022000170704      /copy qsysinc/qrpglesrc,QUSEC
022100170704
022200170704       //--------------------------------------------------------------
022300170704       //?Definizione key-list.                                        ?
022400170704       //--------------------------------------------------------------
022500170704
022600170704       // -?File TIIFT02L?
022700170704     d keyTIIFT02    e ds                  extname( TIIFT02L : *key )
022800170704     d                                     prefix( k_ )   inz
022900170726
023000170726       // -?File TIIFT03L?
023100170726     d keyTIIFT03    e ds                  extname( TIIFT03L : *key )
023200170726     d                                     prefix( k3_ )  inz
023300170704
023400170704       // -?File ANSPI01L?
023500170704     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
023600170704     d                                     prefix( k_ )   inz
023700170704
023800170704       //--------------------------------------------------------------
023900170704       //?Riepilogo indicatori utilizzati.                             ?
024000170704       //--------------------------------------------------------------
024100170704       //--------------------------------------------------------------
024200170704
024300170704       //--------------------------------------------------------------
024400170704       //?M A I N - L I N E                                            ?
024500170704       //--------------------------------------------------------------
024600170704
024700170704     c     *Entry        plist
024800170704     c                   parm                    KPJBA
024900170704
025000170704      /free
025100170704
025200170704       // -?Operazioni iniziali?
025300170704       exsr  sr_RoutInz;
025400170704
025500170704       // -?Ciclo di gestione del file video?
025600170704       DOW  $Fine = *off;
025700170704
025800170704         select;
025900170704
026000170704           // -?Gestione videata S1 (subfile)?
026100170704           when $Video = 'S1';
026200170704             exsr sr_GesS01;
026300170704
026400170704           // -?Manutenzione dati della singola P.I.?
026500170704           when $Video = 'D2';
026600170704             exsr  sr_GesD02;
026700170704
026800170704           // -?? ? ??
026900170704           other;
027000170704             $Fine = *on;
027100170704
027200170704         endsl;
027300170704
027400170704       ENDDO;
027500170704
027600170704       // -?Operazioni finali?
027700170704       exsr  sr_RoutEnd;
027800170704
027900170704       //--------------------------------------------------------------
028000170704       //?Operazioni iniziali.                                         ?
028100170704       //--------------------------------------------------------------
028200170704       BEGSR  sr_RoutInz;
028300170704
028400170704         // -?Impostazione chiusura?
028500170704         *inLR = *on;
028600170704
028700170704         // -?Reperimento dati job?
028800170704         exsr  sr_DatiJob;
028900170704
029000170705         // -?Reperimento Registro Iva per Split Payment (913)?
029100170704         clear  dsQC2;
029200170704         ds_TNTBE.TBEke1 = '2';
029300170704         if  getTabella ( c_Tabel : 'QC'  : ds_TNTBE.TBEke1 :
029400170704                          *omit : *omit : *omit :
029500170704                          *omit : *omit :
029600170704                          *omit : *omit : *omit : *omit :
029700170704                          *omit : *omit : *omit : *omit :
029800170704                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
029900170704                        = *zero      AND
030000170704             ds_TNTBE.TBEatb = *blank;
030100170704           dsQC2  = ds_TNTBE.TBEuni;
030200170704         endif;
030300170704
030400170704         // -?Reperimento data odierna?
030500170704         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030600170704                               : 1 : 8 ) );
030700170705         // -?Reperimento orario attuale?
030800170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
030900170705                               : 9 : 4 ) );
031000170704
031100170704         // -?Impostazione nome programma a video?
031200170704         V1Tpgm = SDSpgm;
031300170704
031400170704         // -?Impostazione videata iniziale?
031500170704         $Video = 'S1';
031600170704         reset  $InzS01;
031700170704         reset  $InzD02;
031800170704
031900170704       ENDSR;
032000170704
032100170704       //--------------------------------------------------------------
032200170704       //?Reperimento Dati del job (Utente/Operativi).                 ?
032300170704       //--------------------------------------------------------------
032400170704       BEGSR  sr_DatiJob;
032500170704
032600170704         in(E) §AzUte;
032700170704         if NOT %error;
032800170704           in(E) §DatiUte;
032900170704         endif;
033000170704         if %error or RSut = *blanks;
033100170704           clear TIBS34ds;
033200170704           tibs34r ( tibs34ds );
033300170704           in §AzUte;
033400170704           in §DatiUte;
033500170704         endif;
033600170704
033700170704       ENDSR;
033800170704
033900170704       //--------------------------------------------------------------
034000170704       //?Gestione subfile S01.                                        ?
034100170704       //--------------------------------------------------------------
034200170704       BEGSR  sr_GesS01;
034300170704
034400170704         // -?Inizializzazione videata?
034500170704         if  $InzS01 = *on;
034600170704           exsr  sr_InzS01;
034700170704           $InzS01 = *off;
034800170704         endif;
034900170704
035000170704         // -?(Dis)Abilitazione tasti funzionali?
035100170706         exsr  sr_AbilitFxx;
035200170704
035300170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
035400170704         write  SF25T01;
035500170704         write  SF25P01;
035600170704
035700170704         // -?Posizionamento cursore?
035800170704         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035900170704         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
036000170704         //  ?si visualizza la pagina che vedeva l'utente quando ha?
036100170704         //  ?premuto l'ultimo tasto.?
036200170704         if  C1CsrRrn > *zero;
036300170704           C1RcdNbr = C1CsrRrn;
036400170704         else;
036500170704           C1RcdNbr = 1;
036600170704           write  SF25S00;     //?(no rec.)?
036700170704         endif;
036800170704
036900170706         // -?Emissione videata con subfile?
037000170704         exfmt  SF25C01;
037100170704
037200170704         clear  VIDmsg;
037300170704         reset  $ErrMessage;
037400170704         reset  $ErrGenerico;
037500170704
037600170704         reset  $Cancellazione;
037700170704         reset  $Immissione;
037800170706         reset  $Protect;
037900170704
038000170704         SELECT;
038100170704
038200170704           // -?F3=Fine?
038300170704           WHEN  dsp_aid = c_F03;
038400170704             $Fine   = *on;
038500170704
038600170704           // -?F10=Inserimento Filiale?
038700170704           WHEN  dsp_aid = c_F10;
038800170704             $Immissione = *on;
038900170704             $Video  = 'D2';
039000170704             $InzD02 = *on;
039100170704
039200170704           // -?F12=Ritorno?
039300170704           WHEN  dsp_aid = c_F12;
039400170704             $Fine   = *on;
039500170704
039600170704           // -?Subfile vuoto?
039700170704           WHEN  S01nrr = *zero;
039800170705             if  C1Civa  <> SavC1Civa;
039900170704               $InzS01 = *on;
040000170706               SavC1Civa = C1Civa;
040100170704             endif;
040200170704
040300170704           // -?Invio?
040400170704           OTHER;
040500170704             // -?Gestione opzioni?
040600170704             exsr  sr_OpzS01;
040700170704             if  $ErrGenerico;
040800170704               leavesr;
040900170704             endif;
041000170704             // -?Richiesto Posizionamento?
041100170705             if  C1Civa <> SavC1Civa;
041200170704               $InzS01 = *on;
041300170705               SavC1Civa = C1Civa;
041400170704             endif;
041500170704
041600170704         ENDSL;
041700170704
041800170704       ENDSR;
041900170704
042000170704       //--------------------------------------------------------------
042100170704       //?Inizializzazione subfile S01.                                ?
042200170704       //--------------------------------------------------------------
042300170704       BEGSR  sr_InzS01;
042400170704
042500170704         // -?Spegnimento degli indicatori di errore?
042600170704         %subst( IndDspF : 50 ) = *off;
042700170704
042800170704         // -?Pulizia subfile?
042900170704         $SflDsp_N    = *on;
043000170704         $SflDspCtl_N = *on;
043100170704         write  SF25C01;
043200170704         $SflDspCtl_N = *off;
043300170704         $SflEnd      = *off;
043400170704
043500170704         clear  C1RcdNbr;
043600170704         clear  C1CsrRrn;
043700170704         clear  S01nrr;
043800170704         clear  VIDmsg;
043900170704         $ErrMessage  = *off;
044000170704         $ErrGenerico = *off;
044100170704
044200170704         $SflNxtChg = *off;
044300170704
044400170704         // -?Caricamento completo dei dati nel subfile (per codice)?
044500170704         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
044600170704         //  ?l'eventuale ordinamento)?
044700170704         clear  keyANSPI01;
044800170706         if  C1Civa <> *blank;
044900170706           k_SPIiva = C1Civa;
045000170706         endif;
045100170704         setll  %kds( keyANSPI01 )  ANSPI001;
045200170704         read  ANSPI001;
045300170704
045400170704         DoW  Not %eof( ANSPI01L )  and  S01nrr < c_MaxRec;
045500170704           exsr  sr_WriteS01;
045600170704           read  ANSPI001;
045700170704         EndDo;
045800170704
045900170704         // -?Visualizzazione del SFL (se ci sono dati)?
046000170704         $SflDsp_N = ( S01nrr <= *zero );
046100170704
046200170704         // -?Attivazione del SFLEND?
046300170704         $SflEnd   = %eof( ANSPI01L );
046400170704
046500170704         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
046600170704         if  S01nrr > *zero;
046700170704           C1RcdNbr = 1;
046800170704         else;
046900170704           clear  C1RcdNbr;
047000170704         endif;
047100170704
047200170704         // -?Impostazione Opzioni?
047300170707         C1Dopz = '2=Modifica, 4=Cancellazione, 5=Visualizzazione';
047400170704
047500170704         // -?Emissione messaggio di segnalazione raggiungimento limite?
047600170704         if  S01nrr >= c_MaxRec   and   Not %eof( ANSPI01L );
047700170704           $ErrGenerico = *on;
047800170704           $ErrMessage  = *on;
047900170704           $PosCurOPZ   = *on;
048000170704           VIDmsg = sk_Msg(01);
048100170704           leavesr;
048200170704         endif;
048300170704
048400170704       ENDSR;
048500170704
048600170704       //--------------------------------------------------------------
048700170704       //?Registrazione del singolo rec. nel subfile S01.              ?
048800170704       //--------------------------------------------------------------
048900170704       BEGSR  sr_WriteS01;
049000170704
049100170704         // -?Eventuale posizionamento richiesto?
049200170706         if  SPIiva < C1Civa;
049300170704           leavesr;
049400170704         endif;
049500170704
049600170704         // -?Impostazione campi nel record del subfile?
049700170704         clear  SF25S01;
049800170704
049900170705         S1Hnrr   = InfANSPI.db_rrn;
050000170704         S1Hdde   = SPIdde;
050100170704         S1Hdsc   = SPIdsc;
050200170704         S1Hdim   = SPIdim;
050300170704         S1Hdmo   = SPIdmo;
050400170704
050500170704         S1Civa   = SPIiva;
050600170704
050700170704         reset  WLBdat;
050800170704         G08inv = SPIdde;
050900170704         XSRDA8( WLBdat );
051000170704         if  G08err = *zero;
051100170704           S1Ddde = G08dat;
051200170704         endif;
051300170704
051400170704         reset  WLBdat;
051500170704         G08inv = SPIdsc;
051600170704         XSRDA8( WLBdat );
051700170704         if  G08err = *zero;
051800170704           S1Ddsc = G08dat;
051900170704         endif;
052000170704
052100170707         reset  WLBdat;
052200170707         G08inv = SPIdim;
052300170707         XSRDA8( WLBdat );
052400170707         if  G08err = *zero;
052500170707           S1Ddim = G08dat;
052600170707         endif;
052700170707
052800170707         if  SPIdmo <> *zero;
052900170707           reset  WLBdat;
053000170707           G08inv = SPIdmo;
053100170707           XSRDA8( WLBdat );
053200170707           if  G08err = *zero;
053300170707             S1Ddmo = G08dat;
053400170707           endif;
053500170707         endif;
053600170704
053700170704         // -?Registrazione singolo record nel subfile?
053800170704         S01nrr += 1;
053900170704         write  SF25S01;
054000170704
054100170704       ENDSR;
054200170704
054300170704       //--------------------------------------------------------------
054400170704       //?Gestione opzioni del subfile S01                             ?
054500170704       //--------------------------------------------------------------
054600170704       BEGSR  sr_OpzS01;
054700170704
054800170704         // -?Spegnimento degli indicatori di errore?
054900170704         %subst( IndDspF : 50 ) = *off;
055000170704
055100170704         clear  SavS01csr;
055200170704
055300170704         // -?Ciclo di lettura subfile?
055400170704         readc  SF25S01;
055500170704
055600170704         DoW  Not %eof(TNSF25D);
055700170704
055800170704           $SflNxtChg = *off;
055900170704           %subst( IndDspF : 50 ) = *off;
056000170704           C1RcdNbr   = S01nrr;
056100170704
056200170704           Select;
056300170704
056400170704             // -?Nessuna opzione?
056500170704             When  S1Copz = *blank;
056600170704
056700170704             // -?Opz. 2=Modifica/Cancellazione?
056800170704             When  S1Copz  = '2';
056900170704               $ProtectKEY = *on;
057000170704               $InzD02     = *on;
057100170704               $Video      = 'D2';
057200170704               DoW  $Video = 'D2';
057300170704                 exsr  sr_GesD02;
057400170704               EndDo;
057500170704               clear  $ProtectKEY;
057600170704
057700170707             // -?Opz. 4=Cancellazione?
057800170707             //  ?Opz. 5=Visualizzazione?
057900170707             When  S1Copz  = '4'  or
058000170707                   S1Copz  = '5';
058100170707               $Cancellazione = ( S1Copz = '4' );
058200170704               $Protect    = *on;
058300170704               $InzD02     = *on;
058400170704               $Video      = 'D2';
058500170704               DoW  $Video = 'D2';
058600170704                 exsr  sr_GesD02;
058700170704               EndDo;
058800170704               clear  $Protect;
058900170704
059000170704             // -?Opzione errata?
059100170704             Other;
059200170704               $ErrGenerico = *on;
059300170704               $ErrMessage  = *on;
059400170704               $PosCurOPZ   = *on;
059500170704               VIDmsg = sk_Msg(02);
059600170704
059700170704           EndSl;
059800170704
059900170704           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
060000170704           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
060100170704           if  S1Copz   <> *blank  and
060200170704              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
060300170704             SavS01csr   = C1RcdNbr;
060400170704           endif;
060500170704
060600170704           // -?Aggiornamento sfl?
060700170704           if  $ErrGenerico;
060800170704             C1CsrRrn   = C1RcdNbr;
060900170707             $SflNxtChg = *on;
061000170707             //*·clear  S1Copz;
061100170704           else;
061200170704             clear  S1Copz;
061300170704           endif;
061400170704
061500170704           UPDATE  SF25S01;
061600170704
061700170704           if  $ErrGenerico  or  $ErrMessage;
061800170704             leave;
061900170704           endif;
062000170704
062100170704           readc  SF25S01;
062200170704
062300170704         ENDDO;
062400170704
062500170704         // -?Riposizionam. cursore sul record contenente la prima opz.?
062600170704         //  ?(se non sono stati rilevati errori)?
062700170704         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
062800170704             SavS01csr > *zero;
062900170704           C1CsrRrn = SavS01csr;
063000170704         endif;
063100170704
063200170704       ENDSR;
063300170704
063400170704       //--------------------------------------------------------------
063500170704       //?Gestione videata D02.                                        ?
063600170704       //--------------------------------------------------------------
063700170704       BEGSR  sr_GesD02;
063800170704
063900170704         // -?Inizializzazione videata?
064000170704         if  $InzD02 = *on;
064100170704           exsr  sr_InzD02;
064200170704           $InzD02 = *off;
064300170704         endif;
064400170706
064500170706         // -?Cancellazione (da opz. 4 => $Cancellazione) NON ammessa?
064600170706         //  ?per la presenza di fatture con Registro Iva 913?
064700170706         //  ?(quella da F16 è già stata disabilitata nella subr. sr_InzD02)?
064800170706         If  $Cancellazione  and  $TIIFT_FI7;
064900170706           $Video = 'S1';
065000170707           VIDmsg = %trimr( sk_Msg(12) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
065100170706           $ErrMessage  = *on;
065200170706           $ErrGenerico = *on;
065300170706           leavesr;
065400170706         EndIf;
065500170704
065600170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
065700170704         write  SF25T01;
065800170704         write  SF25P01;
065900170704
066000170704         // -?Emissione videata?
066100170704         if  Not $Protect;
066200170704           exfmt  SF25D02;
066300170704         else;
066400170704           write  SF25D02;
066500170704           exfmt  Protect;
066600170704         endif;
066700170704
066800170704         clear  VIDmsg;
066900170704         reset  $ErrMessage;
067000170704         reset  $ErrGenerico;
067100170704
067200170704         SELECT;
067300170704
067400170704           // -?F3=Fine?
067500170704           WHEN  dsp_aid = c_F03;
067600170705             unlock  ANSPI00F;
067700170704             $Video = 'S1';
067800170704             $ErrGenerico = *on;
067900170704             if  $Immissione;
068000170704               $Fine = *on;
068100170704             endif;
068200170704
068300170704           // -?F12=Ritorno?
068400170704           WHEN  dsp_aid = c_F12;
068500170705             unlock  ANSPI00F;
068600170704             $Video = 'S1';
068700170704
068800170704           // -?Enter / F6=Conferma / F16=Cancellazione?
068900170704           OTHER;
069000170704             // -?Controlli eseguiti NON se F16=Annullamento?
069100170707             //*·if  Not $Cancellazione;
069200170707             if  dsp_Aid <> c_F16;
069300170704               exsr  sr_CtrD02;
069400170704             endif;
069500170704             if  $ErrGenerico;
069600170704               leavesr;
069700170704             endif;
069800170706             // -?Aggiornamento?
069900170704             If  dsp_aid = c_F06   or
070000170704                 dsp_aid = c_F16;
070100170704               exsr  sr_Upd_ANSPI;
070200170704               // -?Rilevato errore in fase di aggiornamento?
070300170704               if  $ErrGenerico = *on;
070400170704                 leavesr;
070500170704               endif;
070600170704               // -?SubFile iniziale altrimenti?
070700170704               $Video  = 'S1';
070800170704               $InzS01 = *on;
070900170704             EndIf;
071000170704
071100170704         ENDSL;
071200170704
071300170704       ENDSR;
071400170704
071500170704       //--------------------------------------------------------------
071600170704       //?Inizializzazione videata D02.                                ?
071700170704       //--------------------------------------------------------------
071800170704       BEGSR  sr_InzD02;
071900170704
072000170704         // -?Spegnimento degli indicatori di errore?
072100170706         %subst( IndDspF : 41 ) = *off;
072200170704
072300170704         // -?Pulizia videata?
072400170704         clear  SF25D02;
072500170704
072600170704         // -?Reperimento dati in gestione?
072700170704         //  ?(SE NON Immissione)?
072800170704         If  Not $Immissione;
072900170704
073000170704           // -?Reperimento dati?
073100170707           if  $Protect;
073200170705             chain(N)  S1Hnrr  ANSPI000;
073300170704           else;
073400170705             chain  S1Hnrr  ANSPI000;
073500170704           endif;
073600170704
073700170704           // -?Caricamento dati a video?
073800170705           if  %found( ANSPI00F );
073900170704             exsr  sr_RieD02;
074000170704           else;
074100170704             VIDmsg = sk_Msg(03);
074200170704             //*·$PosCurIVA   = *on;
074300170704             $ErrMessage  = *on;
074400170704             $ErrGenerico = *on;
074500170704             //*·leavesr;
074600170704           endif;
074700170704
074800170704         Else;
074900170704
075000170704           // -?Impostazione dati di default SE immissione?
075100170704           V2Ddde = *date;
075200170704           V2Ddsc = c_MaxDat_Eur;
075300170704
075400170704         EndiF;
075500170704
075600170704         // -?Impostazione testata?
075700170704         clear  V2Topz;
075800170704         Select;
075900170706           When  $Cancellazione;
076000170706             V2Topz = ' CANCELLAZIONE ';
076100170704           When  $Immissione;
076200170704             V2Topz = '  INSERIMENTO  ';
076300170706           When  $Protect;
076400170706             V2Topz = 'VISUALIZZAZIONE';
076500170704           Other;
076600170704             V2Topz = '   MODIFICA    ';
076700170704         EndSl;
076800170704
076900170704         // -?Impostazione attributi di visualizzazione?
077000170707         $ProtectKEY = ( Not $Immissione );
077100170707         //*·$ProtectDDE = ( Not $Immissione  and
077200170704         //*·                SPIdde <= wDate );
077300170707         //*·$ProtectDSC = ( Not $Immissione  and
077400170706         //*·                SPIdsc <= wDate );
077500170704
077600170706         // -?(Dis)Abilitazione tasti funzionali?
077700170706         exsr  sr_AbilitFxx;
077800170704
077900170704       ENDSR;
078000170704
078100170704       //--------------------------------------------------------------
078200170705       //?Caricamento dati del singolo record del file ANSPI00F nella  ?
078300170704       //?videata D02.                                                 ?
078400170704       //--------------------------------------------------------------
078500170704       BEGSR  sr_RieD02;
078600170704
078700170707         V2Civa = SPIiva;
078800170707         V2Dima = SPIima;
078900170704
079000170707         reset  WLBdat;
079100170707         G08inv   = SPIdde;
079200170707         XSRDA8(WLBdat);
079300170707         if  G08err = *zero;
079400170707           V2Ddde = G08dat;
079500170707         endif;
079600170704
079700170707         reset  WLBdat;
079800170707         G08inv   = SPIdsc;
079900170707         XSRDA8(WLBdat);
080000170707         if  G08err = *zero;
080100170707           V2Ddsc = G08dat;
080200170707         endif;
080300170705
080400170707         if  SPIdim <> *zero  or  SPIpim <> *blank;
080500170707           reset  WLBdat;
080600170707           G08inv   = SPIdim;
080700170707           XSRDA8(WLBdat);
080800170707           if  G08err = *zero;
080900170707             V2Ddim = G08dat;
081000170707           endif;
081100170707         endif;
081200170707         V2Dhim = SPIhim;
081300170707         V2Dpim = SPIpim;
081400170705
081500170707         if  SPIdmo <> *zero  or  SPIpmo <> *blank;
081600170707           reset  WLBdat;
081700170707           G08inv   = SPIdmo;
081800170707           XSRDA8(WLBdat);
081900170707           if  G08err = *zero;
082000170707             V2Ddmo = G08dat;
082100170707           endif;
082200170707         endif;
082300170707         V2Dhmo = SPIhmo;
082400170707         V2Dpmo = SPIpmo;
082500170706
082600170706         // -?Ricerca della 1ª data fattura del cliente con Registro IVA?
082700170706         //  ?"Split Payment" (913)?
082800170706         exsr  sr_RicercaDataFattura913_Prima;
082900170706
083000170706         // -?Ricerca dell'ultima data fattura del cliente con Registro IVA?
083100170706         //  ?"Split Payment" (913)?
083200170706         exsr  sr_RicercaDataFattura913_Ultima;
083300170705
083400170705         // -?Impostazione Attributi di visualizzazione?
083500170707         $VisualINS = ( Not $Immissione );
083600170707         $VisualMOD = ( Not $Immissione  and  SPIdmo > *zero );
083700170704
083800170704       ENDSR;
083900170704
084000170704       //--------------------------------------------------------------
084100170704       //?Controllo dati videata D02.                                  ?
084200170704       //--------------------------------------------------------------
084300170704       BEGSR  sr_CtrD02;
084400170704
084500170704         // -?Spegnimento degli indicatori di errore?
084600170704         %subst(IndDspF : 50) = *off;
084700170704
084800170714         // -?Controllo inserimento Partita Iva / Codice Fiscale?
084900170705         clear  V2Diva;
085000170705         If  V2Civa = *blank;
085100170705           VIDmsg = sk_Msg(04);
085200170705           $PosCurIVA   = *on;
085300170704           $ErrMessage  = *on;
085400170704           $ErrGenerico = *on;
085500170704           leavesr;
085600170704         EndIf;
085700170705
085800170714         // -?Controllo correttezza Partita Iva inserita?
085900170710         //  ?(solo ITALIANA può essere)?
086000170710         If  $Immissione;
086100170710           clear  xCFIVA1ds;
086200170710           xCFIVAmod = 'P';
086300170710           // -?Partita Iva con Nazione davanti?
086400170710           if  %subst( V2Civa : 1 : 2 ) < '00';
086500170710             xCFIVApar = V2Civa;
086600170710           // -?Partita Iva senza Nazione davanti (Italiana)?
086700170710           else;
086800170710             xCFIVApar = '  ' + V2Civa;
086900170710           endif;
087000170710           xCFIVAr1 ( xCFIVA1ds );
087100170714           if  xCFIVAflg < *zero;
087200170714             clear  xCFIVA1ds;
087300170714             //*·xCFIVAmod = *blank;
087400170714             // -?Codice Fiscale (Italiano)?
087500170714             xCFIVApar = V2Civa;
087600170714             xCFIVAr1 ( xCFIVA1ds );
087700170714           endif;
087800170710           if  xCFIVAflg < *zero;
087900170710             VIDmsg = xCFIVAmsg;
088000170710             $PosCurIVA   = *on;
088100170710             $ErrMessage  = *on;
088200170710             $ErrGenerico = *on;
088300170710             leavesr;
088400170710           endif;
088500170710         EndIf;
088600170704
088700170706         // -?Controllo che la P.I. non risulti già inserita?
088800170706         //  ?(anche se per periodo già scaduto...)?
088900170707         If  $Immissione;
089000170706           clear  keyANSPI01;
089100170706           k_SPIiva = V2Civa;
089200170706           chain  %kds( keyANSPI01 )  ANSPI001;
089300170706           if  %found( ANSPI01L );
089400170707             VIDmsg = sk_Msg(05);
089500170706             reset  WLBdat;
089600170706             G08inv   = SPIdde;
089700170706             XSRDA8(WLBdat);
089800170706             VIDmsg = %trimr( VIDmsg ) + ' (per il periodo dal ' +
089900170706                      %trim( %editw( G08dat : '  /  /    ' ) );
090000170706             reset  WLBdat;
090100170706             G08inv   = SPIdsc;
090200170706             XSRDA8(WLBdat);
090300170706             VIDmsg = %trimr( VIDmsg ) + ' al ' +
090400170706                      %trim( %editw( G08dat : '  /  /    ' ) ) + ')';
090500170707             $PosCurIVA   = *on;
090600170706             $ErrMessage  = *on;
090700170706             $ErrGenerico = *on;
090800170706             leavesr;
090900170706           endif;
091000170706         EndIf;
091100170704
091200170704         // -?Controllo periodo validità?
091300170705         // ·?Data Decorrenza?
091400170704         clear  WLBdat;
091500170705         G08dat = V2Ddde;
091600170705         XSRDA8( WLBdat );
091700170704         if  G08err = *zero;
091800170705           V2Ddde = G08dat;
091900170705           W2Ddde = G08inv;
092000170704         else;
092100170707           VIDmsg = sk_Msg(06);
092200170705           $PosCurDDE   = *on;
092300170704           $ErrMessage  = *on;
092400170704           $ErrGenerico = *on;
092500170704           leavesr;
092600170704         endif;
092700170704         // ·?Data Scadenza?
092800170704         clear  WLBdat;
092900170705         G08dat = V2Ddsc;
093000170705         XSRDA8( WLBdat );
093100170704         if  G08err = *zero;
093200170705           V2Ddsc = G08dat;
093300170705           W2Ddsc = G08inv;
093400170704         else;
093500170707           VIDmsg = sk_Msg(07);
093600170705           $PosCurDSC   = *on;
093700170704           $ErrMessage  = *on;
093800170704           $ErrGenerico = *on;
093900170704           leavesr;
094000170704         endif;
094100170704         // ·?Range di date?
094200170705         if  W2Ddde > W2Ddsc;
094300170707           VIDmsg = sk_Msg(08);
094400170705           $PosCurDSC   = *on;
094500170704           $ErrMessage  = *on;
094600170704           $ErrGenerico = *on;
094700170704           leavesr;
094800170704         endif;
094900170704
095000170706         // -?La Data Decorrenza NON deve essere successiva alla 1ª data?
095100170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
095200170706         exsr  sr_RicercaDataFattura913_Prima;
095300170831         If  $TIIFT_found  and  W2Ddde > IFTdft;
095400170707           VIDmsg = %trimr( sk_Msg(09) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
095500170705           $PosCurDDE   = *on;
095600170705           $ErrMessage  = *on;
095700170705           $ErrGenerico = *on;
095800170705           leavesr;
095900170705         EndIf;
096000170705
096100170706         // -?La Data Scadenza NON deve essere precedente all'ultima data?
096200170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
096300170706         exsr  sr_RicercaDataFattura913_Ultima;
096400170831         If  $TIIFT_found  and  W2Ddsc < IFTdft;
096500170707           VIDmsg = %trimr( sk_Msg(10) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
096600170705           $PosCurDSC   = *on;
096700170705           $ErrMessage  = *on;
096800170705           $ErrGenerico = *on;
096900170705           leavesr;
097000170705         EndIf;
097100170704
097200170705         // -?Verifica esistenza in ANSPI01L?
097300170705         //  ?(con sovrapposizione di giorni nel periodo)?
097400170704         // ·?Esempi - Periodo esistente:?· · 3-----6 · ·?
097500170705         // ·? 1)     Periodo inseribile: 1-2?
097600170705         // ·? 2) Periodo NON inseribile:   2---4?
097700170705         // ·? 3) Periodo NON inseribile:       4-5?
097800170705         // ·? 4) Periodo NON inseribile:         5---7?
097900170705         // ·? 5) Periodo NON inseribile:   2---------7?
098000170705         // ·? 6)     Periodo inseribile:             7-8?
098100170707         IF  $Immissione;
098200170705
098300170705           clear  keyANSPI01;
098400170705           k_SPIiva = V2Civa;
098500170705           setll  %kds( keyANSPI01 )  ANSPI001;
098600170705           reade  %kds( keyANSPI01 )  ANSPI001;
098700170705
098800170705           DoW  NOT %eof( ANSPI01L );
098900170705
099000170705             select;
099100170705               // ·? 2) e 3)?
099200170705               when  ( W2Ddsc >= SPIdde  and
099300170705                       W2Ddsc <= SPIdsc );
099400170707                 VIDmsg = sk_Msg(11);
099500170705                 $PosCurDDE   = *on;
099600170705                 $ErrMessage  = *on;
099700170705                 $ErrGenerico = *on;
099800170705                 leavesr;
099900170705               // ·? 3) e 4)?
100000170705               when  ( W2Ddde >= SPIdde  and
100100170705                       W2Ddde <= SPIdsc );
100200170707                 VIDmsg = sk_Msg(11);
100300170705                 $PosCurDDE   = *on;
100400170705                 $ErrMessage  = *on;
100500170705                 $ErrGenerico = *on;
100600170705                 leavesr;
100700170705               // ·? 5)?
100800170705               when  ( W2Ddde <= SPIdde  and
100900170705                       W2Ddde >= SPIdsc );
101000170707                 VIDmsg = sk_Msg(11);
101100170705                 $PosCurDDE   = *on;
101200170705                 $ErrMessage  = *on;
101300170705                 $ErrGenerico = *on;
101400170705                 leavesr;
101500170705             endsl;
101600170705
101700170705             reade  %kds( keyANSPI01 )  ANSPI001;
101800170705
101900170704           EndDo;
102000170705
102100170705         ENDIF;
102200170704
102300170704       ENDSR;
102400170706
102500170706       //--------------------------------------------------------------
102600170706       //?Settaggio degli indicatori per la (dis)abilitazione dei tasti?
102700170706       //?  funzionali nelle diverse videate.                          ?
102800170706       //--------------------------------------------------------------
102900170706       BEGSR  sr_AbilitFxx;
103000170706
103100170706         // -?F3 = Fine?
103200170706         $F3Attivo  = *on;
103300170706
103400170706         // -?F6 = Conferma?
103500170706         $F6Attivo  = ( $Video = 'D2'  and  Not $Protect );
103600170706
103700170706         // -?F10 = Inserimento?
103800170706         $F10Attivo = ( $Video = 'S1' );
103900170706
104000170706         // -?F12 = Ritorno?
104100170706         $F12Attivo = ( $Video = 'D2' );
104200170706
104300170706         // -?F16 = Cancellazione?
104400170706         $F16Attivo = ( $Video = 'D2'     and
104500170706                        Not $Immissione   and
104600170706                        %found(ANSPI00F)  and
104700170706                        Not $TIIFT_FI7    and
104800170706                      ( Not $Protect      or
104900170706                        $Cancellazione ) );
105000170706       ENDSR;
105100170706
105200170706       //--------------------------------------------------------------
105300170706       //?Ricerca della data della PRIMA fattura con Registro IVA 913  ?
105400170706       //?per il cliente.                                              ?
105500170706       //--------------------------------------------------------------
105600170706       BEGSR  sr_RicercaDataFattura913_Prima;
105700170706
105800170706         clear  V2Dtxd;
105900170726         clear  $TIIFT_found;
106000170706
106100170706         clear  keyTIIFT02;
106200170706         k_IFTpiva = V2Civa;
106300170706         k_IFTfiv  = dsQC2.§QCfi7;
106400170717         //*·k_IFTdft  = *zero;
106500170706
106600170726         setll  %kds( keyTIIFT02 )  TIIFT002;
106700170726         reade  %kds( keyTIIFT02 : 2 )  TIIFT002;
106800170706
106900170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
107000170726
107100170726         If  Not $TIIFT_found;
107200170726
107300170726           clear  keyTIIFT03;
107400170726           k3_IFTcdf = V2Civa;
107500170726           k3_IFTfiv = dsQC2.§QCfi7;
107600170726           //*·k3_IFTdft = *zero;
107700170726
107800170726           setll  %kds( keyTIIFT03 )  TIIFT003;
107900170726           reade  %kds( keyTIIFT03 : 2 )  TIIFT003;
108000170726
108100170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
108200170726
108300170726         EndIf;
108400170726
108500170726         If  $TIIFT_found;
108600170706
108700170706           reset  WLBdat;
108800170706           G08inv   = IFTdft;
108900170706           XSRDA8(WLBdat);
109000170706
109100170706           V2Dtxd = 'Fatt. con Reg. IVA ' +
109200170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' dal ' +
109300170706                    %editw( G08dat : '  /  /    ' );
109400170706
109500170706         EndIf;
109600170706
109700170706       ENDSR;
109800170706
109900170706       //--------------------------------------------------------------
110000170706       //?Ricerca della data dell'ULTIMA fattura con Registro IVA 913  ?
110100170706       //?per il cliente.                                              ?
110200170706       //--------------------------------------------------------------
110300170706       BEGSR  sr_RicercaDataFattura913_Ultima;
110400170706
110500170706         clear  V2Dtxs;
110600170726         clear  $TIIFT_found;
110700170706
110800170706         clear  keyTIIFT02;
110900170706         k_IFTpiva = V2Civa;
111000170706         k_IFTfiv  = dsQC2.§QCfi7;
111100170706         k_IFTdft  = *hival;
111200170726
111300170726         setgt  %kds( keyTIIFT02 )  TIIFT002;
111400170726         readPe  %kds( keyTIIFT02 : 2 )  TIIFT002;
111500170726
111600170726         $TIIFT_found = ( Not %eof( TIIFT02L ) );
111700170726
111800170726         If  Not $TIIFT_found;
111900170726
112000170726           clear  keyTIIFT03;
112100170726           k3_IFTcdf = V2Civa;
112200170726           k3_IFTfiv = dsQC2.§QCfi7;
112300170726           k3_IFTdft = *hival;
112400170726
112500170726           setgt  %kds( keyTIIFT03 )  TIIFT003;
112600170726           readPe  %kds( keyTIIFT03 : 2 )  TIIFT003;
112700170726
112800170726           $TIIFT_found = ( Not %eof( TIIFT03L ) );
112900170726
113000170726         EndIf;
113100170706
113200170726         If  $TIIFT_found;
113300170706
113400170706           reset  WLBdat;
113500170706           G08inv   = IFTdft;
113600170706           XSRDA8(WLBdat);
113700170706
113800170706           V2Dtxs = 'Fatt. con Reg. IVA ' +
113900170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' al  ' +
114000170706                    %editw( G08dat : '  /  /    ' );
114100170706
114200170706         EndIf;
114300170706
114400170706       ENDSR;
114500170704
114600170704       //--------------------------------------------------------------
114700170704       //?Aggiornamento dati nel *file ANSPI00F.                       ?
114800170704       //--------------------------------------------------------------
114900170704       BEGSR  sr_Upd_ANSPI;
115000170704
115100170705         // -?Aggancio record di ANSPI00F?
115200170707         If  Not $Immissione;
115300170705           chain  S1Hnrr  ANSPI000;
115400170705         EndIf;
115500170705
115600170705         // -?Reperimento data odierna?
115700170705         wDate = %int( %subst( %char( %dec( %timestamp() ) )
115800170705                               : 1 : 8 ) );
115900170705         // -?Reperimento orario attuale?
116000170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
116100170705                               : 9 : 4 ) );
116200170704
116300170704         // -?Impostazione dati?
116400170707         If  $Immissione  or
116500170705             ( NOT %found( ANSPI00F ) and
116600170704               NOT $Cancellazione );
116700170705           clear  ANSPI000;
116800170705           SPIiva = V2Civa;
116900170705           SPIdim = wDate;
117000170705           SPIhim = wTime;
117100170705           SPIpim = kNmUs;
117200170705           SPIima = 'M';            //?(M=Inserimento Manuale)?
117300170705         EndIf;
117400170704
117500170707         //*·If  Not $Cancellazione;
117600170707         If  dsp_aid <> c_F16;
117700170707
117800170705           SPIdde = W2Ddde;
117900170705           SPIdsc = W2Ddsc;
118000170705
118100170707           If  Not $Immissione;
118200170707             SPIdmo = wDate;
118300170707             SPIhmo = wTime;
118400170707             SPIpmo = kNmUs;
118500170707           EndIf;
118600170707
118700170707         EndIf;
118800170704
118900170704         // -?Aggiornamento record?
119000170704         SELECT;
119100170704
119200170704           // -?F16=Cancellazione?
119300170707           //*·WHEN  $Cancellazione  and  %found( ANSPI00F );
119400170707           WHEN  dsp_aid = c_F16  and  %found( ANSPI00F );
119500170705             //__________________
119600170705             delete(E)  ANSPI000;
119700170705             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
119800170704
119900170704           // -?Immissione o Copia o Modifica?
120000170704           OTHER;
120100170707             if  Not $Immissione  and
120200170705                 %found( ANSPI00F );
120300170704               //_________________
120400170705               update(E) ANSPI000;
120500170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
120600170704             else;
120700170704               //_________________
120800170705               write(E)  ANSPI000;
120900170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
121000170704             endif;
121100170704
121200170704         ENDSL;
121300170704
121400170704         // -?Segnalazione errore rilevato in fase di?
121500170704         //  ?scrittura/aggiornamento/cancellazione?
121600170704         If  %error();
121700170707           VIDmsg = sk_Msg(13);
121800170704           $ErrMessage  = *on;
121900170704           $ErrGenerico = *on;
122000170704           // -?Stampa del Dump?
122100170704           Dump(A);
122200170704           // -?Stampa del Job-Log?
122300170704           Qcmd = 'DSPJOBLOG job(*) output(*print)';
122400170704           exsr  sr_ExecCmd;
122500170704           leavesr;
122600170704         EndIf;
122700170704
122800170704       ENDSR;
122900170704
123000170704       //--------------------------------------------------------------
123100170704       //?Esecuzione del comando (già impostato).                      ?
123200170704       //--------------------------------------------------------------
123300170704       BEGSR  sr_ExecCmd;
123400170704
123500170704         clear Qcap0100;
123600170704         Qcabcsdh = *off;
123700170704         Qcapa    = *off;
123800170704         Qcacmdss = *off;
123900170704         Qcaerved = *allX'00';
124000170704
124100170704         clear Qusec;
124200170704         Qusbprv  = %size(Qusec);
124300170704
124400170704         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
124500170704                           %size(Qcap0100) : 'CPOP0100' : *omit :
124600170704                           0 : 0 : Qusec);
124700170704
124800170704         //*·if  Qusei <> *blank;
124900170704         //*·  exsr  sr_PrintErr;
125000170704         //*·endif;
125100170704
125200170704       ENDSR;
125300170704
125400170704       //--------------------------------------------------------------
125500170704       //?Operazioni finali.                                           ?
125600170704       //--------------------------------------------------------------
125700170704       BEGSR  sr_RoutEnd;
125800170707
125900170707         // -?Chiusura funzioni precedentemente aperte?
126000170707         cloTabella ( c_Tabel );
126100170704
126200170704         //  ?Uscita?
126300170704         return;
126400170704
126500170704       ENDSR;
126600170704
126700170704      /end-free
126800170704
126900170704       //--------------------------------------------------------------
127000170704       //?Definizione schiere a tempo di compilazione.                 ?
127100170704       //--------------------------------------------------------------
127200170704
127300170704** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
127400170704Raggiunto il limite massimo dei record caricabili a video                       1
127500170704Opzione errata                                                                  2
127600170704Partita Iva NON più reperibile in questo archivio                               3
127700170704Immettere la Partita Iva                                                        4
127800170707Partita Iva già inserita                                                        5
127900170707Data decorrenza formalmente errata                                              6
128000170707Data scadenza formalmente errata                                                7
128100170707Data di scadenza precedente a quella di decorrenza                              8
128200170707Data decorrenza successiva quella della 1ª fattura con Registro IVA             9
128300170707Data scadenza precedente quella dell'ultima fattura con Registro IVA           10
128400170707Il periodo si sovrappone a quello di un'altro Split Payment già inserito       11
128500170707Partita Iva NON cancellabile: trovate fatture con Registro IVA                 12
128600170707Rilevato errore in fase di registrazione dati => Avvisare CED                  13
