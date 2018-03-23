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
002500170704     fTIIFT02L  if   e           k disk
002600170704
002700170704       // -?Soggetti tenuti al regime dello Split Payment?
002800170705     fANSPI01L  if   e           k disk    rename( ANSPI000 : ANSPI001 )
002900170705     f                                     infds( InfANSPI )
003000170705     fANSPI00F  Uf A e             disk
003100170704
003200170704       // -?Video?
003300170704     fTNSF25D   cf   e             workstn
003400170704     f                                     sfile( SF25S01 : S01nrr )
003500170704     f                                     indds( IndDspF )
003600170704     f                                     infds( InfDspF )
003700170704
003800170704       //--------------------------------------------------------------
003900170704       //?Definizione costanti.                                        ?
004000170704       //--------------------------------------------------------------
004100170704
004200170704       // -?Data Scadenza massima - in formato *EUR?
004300170704     d c_MaxDat_Eur    c                   const(31122039)
004400170704
004500170704       // -?Numero di record in ciascuna videata di subfile?
004600170704     d c_SflPag        c                   const(15)
004700170704
004800170704       // -?Numero massimo di record gestibili?
004900170704     d c_MaxRec        c                   const(9999)
005000170704
005100170704       // -?Attributi di Visualizzazione?
005200170704     d c_DspAtrNorm    c                   const(x'A0')
005300170704     d c_DspAtrRI      c                   const(x'A1')
005400170704     d c_DspAtrHI      c                   const(x'A2')
005500170704     d c_DspAtrND      c                   const(x'A7')
005600170704     d c_DspAtrRED     c                   const(x'A8')
005700170704     d c_DspAtrRI_RED  c                   const(x'A9')
005800170704     d c_DspAtrBL_RED  c                   const(x'AA')
005900170704
006000170704       // -?Tasti funzionali a video?
006100170704     d c_F01           c                   const(x'31')
006200170704     d c_F02           c                   const(x'32')
006300170704     d c_F03           c                   const(x'33')
006400170704     d c_F04           c                   const(x'34')
006500170704     d c_F05           c                   const(x'35')
006600170704     d c_F06           c                   const(x'36')
006700170704     d c_F07           c                   const(x'37')
006800170704     d c_F08           c                   const(x'38')
006900170704     d c_F09           c                   const(x'39')
007000170704     d c_F10           c                   const(x'3A')
007100170704     d c_F11           c                   const(x'3B')
007200170704     d c_F12           c                   const(x'3C')
007300170704     d c_F13           c                   const(x'B1')
007400170704     d c_F14           c                   const(x'B2')
007500170704     d c_F15           c                   const(x'B3')
007600170704     d c_F16           c                   const(x'B4')
007700170704     d c_F17           c                   const(x'B5')
007800170704     d c_F18           c                   const(x'B6')
007900170704     d c_F19           c                   const(x'B7')
008000170704     d c_F20           c                   const(x'B8')
008100170704     d c_F21           c                   const(x'B9')
008200170704     d c_F22           c                   const(x'BA')
008300170704     d c_F23           c                   const(x'BB')
008400170704     d c_F24           c                   const(x'BC')
008500170704     d c_Enter         c                   const(x'F1')
008600170704     d c_RollDown      c                   const(x'F4')
008700170704     d c_RollUp        c                   const(x'F5')
008800170704
008900170704       //--------------------------------------------------------------
009000170704       //?Definizione schiere.                                         ?
009100170704       //--------------------------------------------------------------
009200170704
009300170704       // -?Messaggi di errore?
009400170707     d sk_Msg          s             78    dim(13)  ctdata  perrcd( 1)
009500170704
009600170704       //--------------------------------------------------------------
009700170704       //?Definizione aree dati.                                       ?
009800170704       //--------------------------------------------------------------
009900170704
010000170704       // -?Dati utente?
010100170704     d §AzUte        e ds                  extname(AZUTE00F)
010200170704     d                                     dtaara
010300170704     d §DatiUte      e ds                  extname(dDatiUte)
010400170704     d                                     dtaara
010500170704
010600170704       //--------------------------------------------------------------
010700170704       //?Definizione strutture dati.                                  ?
010800170704       //--------------------------------------------------------------
010900170704
011000170704       // -?Status ds?
011100170704     d Status         sds
011200170704     d   SDSpgm          *proc
011300170705
011400170705     d InfANSPI        ds                  qualified
011500170705     d   db_rrn              397    400i 0
011600170704
011700170704       // -?InfDS?
011800170704     d InfDspF         ds
011900170704     d   dsp_aid             369    369a
012000170704     d*//sfl_rrn             376    377i 0
012100170704     d*//min_nrr             378    379i 0
012200170704     d*//num_rcds            380    381i 0
012300170704
012400170704       // -?Indicatori su DspF?
012500170704     d IndDspF         ds                  inz
012600170704         // -?Abilitazione tasti funzionali?
012700170704     d   $F3Attivo                     n   overlay( IndDspF : 03 )
012800170704     d   $F6Attivo                     n   overlay( IndDspF : 06 )
012900170704     d   $F10Attivo                    n   overlay( IndDspF : 10 )
013000170704     d   $F12Attivo                    n   overlay( IndDspF : 12 )
013100170704     d   $F16Attivo                    n   overlay( IndDspF : 16 )
013200170704         // -?Emissione messaggio di errore?
013300170704     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013400170704         // -?Indicatori di gestione del subfile?
013500170704     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
013600170704     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013700170704     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
013800170704     d   $SflEnd                       n   overlay( IndDspF : 33 )
013900170704         // -?Indicatori per Attributi di visualizzazione?
014000170704     d*//$DisabilOPZ                   n   overlay( IndDspF : 40 )
014100170704     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
014200170704     d   $ProtectDDE                   n   overlay( IndDspF : 42 )
014300170704     d   $ProtectDSC                   n   overlay( IndDspF : 43 )
014400170704     d   $VisualINS                    n   overlay( IndDspF : 44 )
014500170704     d   $VisualMOD                    n   overlay( IndDspF : 45 )
014600170704         // -?Posizionamento cursore & segnalazione errore?
014700170704     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
014800170704     d   $PosCurIVA                    n   overlay( IndDspF : 51 )
014900170704     d   $PosCurDDE                    n   overlay( IndDspF : 52 )
015000170704     d   $PosCurDSC                    n   overlay( IndDspF : 53 )
015100170704         // -?Riemissione videata?
015200170704     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
015300170704
015400170704       // -?Parametri ricevuti?
015500170704     d KPJBA         e ds
015600170704
015700170704       // -?Tabella "QC" / "2"?
015800170704     d dsQC2         e ds                  qualified      inz
015900170704
016000170704       //--------------------------------------------------------------
016100170704       //?Definizione variabili globali.                               ?
016200170704       //--------------------------------------------------------------
016300170704
016400170704       // -?Flags booleani?
016500170704     d $Fine           s               n   inz
016600170704     d $InzS01         s               n   inz(*on)
016700170704     d $InzD02         s               n   inz(*on)
016800170704     d $Cancellazione  s               n   inz
016900170704     d $Immissione     s               n   inz
017000170704     d $Protect        s               n   inz
017100170705     d $TIIFT_FI7      s               n   inz
017200170706     d*//· $TIIFT          s               n   inz
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
022800170704
022900170704       // -?File ANSPI01L?
023000170704     d keyANSPI01    e ds                  extname( ANSPI01L : *key )
023100170704     d                                     prefix( k_ )   inz
023200170704
023300170704       //--------------------------------------------------------------
023400170704       //?Riepilogo indicatori utilizzati.                             ?
023500170704       //--------------------------------------------------------------
023600170704       //--------------------------------------------------------------
023700170704
023800170704       //--------------------------------------------------------------
023900170704       //?M A I N - L I N E                                            ?
024000170704       //--------------------------------------------------------------
024100170704
024200170704     c     *Entry        plist
024300170704     c                   parm                    KPJBA
024400170704
024500170704      /free
024600170704
024700170704       // -?Operazioni iniziali?
024800170704       exsr  sr_RoutInz;
024900170704
025000170704       // -?Ciclo di gestione del file video?
025100170704       DOW  $Fine = *off;
025200170704
025300170704         select;
025400170704
025500170704           // -?Gestione videata S1 (subfile)?
025600170704           when $Video = 'S1';
025700170704             exsr sr_GesS01;
025800170704
025900170704           // -?Manutenzione dati della singola P.I.?
026000170704           when $Video = 'D2';
026100170704             exsr  sr_GesD02;
026200170704
026300170704           // -?? ? ??
026400170704           other;
026500170704             $Fine = *on;
026600170704
026700170704         endsl;
026800170704
026900170704       ENDDO;
027000170704
027100170704       // -?Operazioni finali?
027200170704       exsr  sr_RoutEnd;
027300170704
027400170704       //--------------------------------------------------------------
027500170704       //?Operazioni iniziali.                                         ?
027600170704       //--------------------------------------------------------------
027700170704       BEGSR  sr_RoutInz;
027800170704
027900170704         // -?Impostazione chiusura?
028000170704         *inLR = *on;
028100170704
028200170704         // -?Reperimento dati job?
028300170704         exsr  sr_DatiJob;
028400170704
028500170705         // -?Reperimento Registro Iva per Split Payment (913)?
028600170704         clear  dsQC2;
028700170704         ds_TNTBE.TBEke1 = '2';
028800170704         if  getTabella ( c_Tabel : 'QC'  : ds_TNTBE.TBEke1 :
028900170704                          *omit : *omit : *omit :
029000170704                          *omit : *omit :
029100170704                          *omit : *omit : *omit : *omit :
029200170704                          *omit : *omit : *omit : *omit :
029300170704                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
029400170704                        = *zero      AND
029500170704             ds_TNTBE.TBEatb = *blank;
029600170704           dsQC2  = ds_TNTBE.TBEuni;
029700170704         endif;
029800170704
029900170704         // -?Reperimento data odierna?
030000170704         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030100170704                               : 1 : 8 ) );
030200170705         // -?Reperimento orario attuale?
030300170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
030400170705                               : 9 : 4 ) );
030500170704
030600170704         // -?Impostazione nome programma a video?
030700170704         V1Tpgm = SDSpgm;
030800170704
030900170704         // -?Impostazione videata iniziale?
031000170704         $Video = 'S1';
031100170704         reset  $InzS01;
031200170704         reset  $InzD02;
031300170704
031400170704       ENDSR;
031500170704
031600170704       //--------------------------------------------------------------
031700170704       //?Reperimento Dati del job (Utente/Operativi).                 ?
031800170704       //--------------------------------------------------------------
031900170704       BEGSR  sr_DatiJob;
032000170704
032100170704         in(E) §AzUte;
032200170704         if NOT %error;
032300170704           in(E) §DatiUte;
032400170704         endif;
032500170704         if %error or RSut = *blanks;
032600170704           clear TIBS34ds;
032700170704           tibs34r ( tibs34ds );
032800170704           in §AzUte;
032900170704           in §DatiUte;
033000170704         endif;
033100170704
033200170704       ENDSR;
033300170704
033400170704       //--------------------------------------------------------------
033500170704       //?Gestione subfile S01.                                        ?
033600170704       //--------------------------------------------------------------
033700170704       BEGSR  sr_GesS01;
033800170704
033900170704         // -?Inizializzazione videata?
034000170704         if  $InzS01 = *on;
034100170704           exsr  sr_InzS01;
034200170704           $InzS01 = *off;
034300170704         endif;
034400170704
034500170704         // -?(Dis)Abilitazione tasti funzionali?
034600170706         exsr  sr_AbilitFxx;
034700170704
034800170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
034900170704         write  SF25T01;
035000170704         write  SF25P01;
035100170704
035200170704         // -?Posizionamento cursore?
035300170704         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035400170704         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035500170704         //  ?si visualizza la pagina che vedeva l'utente quando ha?
035600170704         //  ?premuto l'ultimo tasto.?
035700170704         if  C1CsrRrn > *zero;
035800170704           C1RcdNbr = C1CsrRrn;
035900170704         else;
036000170704           C1RcdNbr = 1;
036100170704           write  SF25S00;     //?(no rec.)?
036200170704         endif;
036300170704
036400170706         // -?Emissione videata con subfile?
036500170704         exfmt  SF25C01;
036600170704
036700170704         clear  VIDmsg;
036800170704         reset  $ErrMessage;
036900170704         reset  $ErrGenerico;
037000170704
037100170704         reset  $Cancellazione;
037200170704         reset  $Immissione;
037300170706         reset  $Protect;
037400170704
037500170704         SELECT;
037600170704
037700170704           // -?F3=Fine?
037800170704           WHEN  dsp_aid = c_F03;
037900170704             $Fine   = *on;
038000170704
038100170704           // -?F10=Inserimento Filiale?
038200170704           WHEN  dsp_aid = c_F10;
038300170704             $Immissione = *on;
038400170704             $Video  = 'D2';
038500170704             $InzD02 = *on;
038600170704
038700170704           // -?F12=Ritorno?
038800170704           WHEN  dsp_aid = c_F12;
038900170704             $Fine   = *on;
039000170704
039100170704           // -?Subfile vuoto?
039200170704           WHEN  S01nrr = *zero;
039300170705             if  C1Civa  <> SavC1Civa;
039400170704               $InzS01 = *on;
039500170706               SavC1Civa = C1Civa;
039600170704             endif;
039700170704
039800170704           // -?Invio?
039900170704           OTHER;
040000170704             // -?Gestione opzioni?
040100170704             exsr  sr_OpzS01;
040200170704             if  $ErrGenerico;
040300170704               leavesr;
040400170704             endif;
040500170704             // -?Richiesto Posizionamento?
040600170705             if  C1Civa <> SavC1Civa;
040700170704               $InzS01 = *on;
040800170705               SavC1Civa = C1Civa;
040900170704             endif;
041000170704
041100170704         ENDSL;
041200170704
041300170704       ENDSR;
041400170704
041500170704       //--------------------------------------------------------------
041600170704       //?Inizializzazione subfile S01.                                ?
041700170704       //--------------------------------------------------------------
041800170704       BEGSR  sr_InzS01;
041900170704
042000170704         // -?Spegnimento degli indicatori di errore?
042100170704         %subst( IndDspF : 50 ) = *off;
042200170704
042300170704         // -?Pulizia subfile?
042400170704         $SflDsp_N    = *on;
042500170704         $SflDspCtl_N = *on;
042600170704         write  SF25C01;
042700170704         $SflDspCtl_N = *off;
042800170704         $SflEnd      = *off;
042900170704
043000170704         clear  C1RcdNbr;
043100170704         clear  C1CsrRrn;
043200170704         clear  S01nrr;
043300170704         clear  VIDmsg;
043400170704         $ErrMessage  = *off;
043500170704         $ErrGenerico = *off;
043600170704
043700170704         $SflNxtChg = *off;
043800170704
043900170704         // -?Caricamento completo dei dati nel subfile (per codice)?
044000170704         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
044100170704         //  ?l'eventuale ordinamento)?
044200170704         clear  keyANSPI01;
044300170706         if  C1Civa <> *blank;
044400170706           k_SPIiva = C1Civa;
044500170706         endif;
044600170704         setll  %kds( keyANSPI01 )  ANSPI001;
044700170704         read  ANSPI001;
044800170704
044900170704         DoW  Not %eof( ANSPI01L )  and  S01nrr < c_MaxRec;
045000170704           exsr  sr_WriteS01;
045100170704           read  ANSPI001;
045200170704         EndDo;
045300170704
045400170704         // -?Visualizzazione del SFL (se ci sono dati)?
045500170704         $SflDsp_N = ( S01nrr <= *zero );
045600170704
045700170704         // -?Attivazione del SFLEND?
045800170704         $SflEnd   = %eof( ANSPI01L );
045900170704
046000170704         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
046100170704         if  S01nrr > *zero;
046200170704           C1RcdNbr = 1;
046300170704         else;
046400170704           clear  C1RcdNbr;
046500170704         endif;
046600170704
046700170704         // -?Impostazione Opzioni?
046800170707         C1Dopz = '2=Modifica, 4=Cancellazione, 5=Visualizzazione';
046900170704
047000170704         // -?Emissione messaggio di segnalazione raggiungimento limite?
047100170704         if  S01nrr >= c_MaxRec   and   Not %eof( ANSPI01L );
047200170704           $ErrGenerico = *on;
047300170704           $ErrMessage  = *on;
047400170704           $PosCurOPZ   = *on;
047500170704           VIDmsg = sk_Msg(01);
047600170704           leavesr;
047700170704         endif;
047800170704
047900170704       ENDSR;
048000170704
048100170704       //--------------------------------------------------------------
048200170704       //?Registrazione del singolo rec. nel subfile S01.              ?
048300170704       //--------------------------------------------------------------
048400170704       BEGSR  sr_WriteS01;
048500170704
048600170704         // -?Eventuale posizionamento richiesto?
048700170706         if  SPIiva < C1Civa;
048800170704           leavesr;
048900170704         endif;
049000170704
049100170704         // -?Impostazione campi nel record del subfile?
049200170704         clear  SF25S01;
049300170704
049400170705         S1Hnrr   = InfANSPI.db_rrn;
049500170704         S1Hdde   = SPIdde;
049600170704         S1Hdsc   = SPIdsc;
049700170704         S1Hdim   = SPIdim;
049800170704         S1Hdmo   = SPIdmo;
049900170704
050000170704         S1Civa   = SPIiva;
050100170704
050200170704         reset  WLBdat;
050300170704         G08inv = SPIdde;
050400170704         XSRDA8( WLBdat );
050500170704         if  G08err = *zero;
050600170704           S1Ddde = G08dat;
050700170704         endif;
050800170704
050900170704         reset  WLBdat;
051000170704         G08inv = SPIdsc;
051100170704         XSRDA8( WLBdat );
051200170704         if  G08err = *zero;
051300170704           S1Ddsc = G08dat;
051400170704         endif;
051500170704
051600170707         reset  WLBdat;
051700170707         G08inv = SPIdim;
051800170707         XSRDA8( WLBdat );
051900170707         if  G08err = *zero;
052000170707           S1Ddim = G08dat;
052100170707         endif;
052200170707
052300170707         if  SPIdmo <> *zero;
052400170707           reset  WLBdat;
052500170707           G08inv = SPIdmo;
052600170707           XSRDA8( WLBdat );
052700170707           if  G08err = *zero;
052800170707             S1Ddmo = G08dat;
052900170707           endif;
053000170707         endif;
053100170704
053200170704         // -?Registrazione singolo record nel subfile?
053300170704         S01nrr += 1;
053400170704         write  SF25S01;
053500170704
053600170704       ENDSR;
053700170704
053800170704       //--------------------------------------------------------------
053900170704       //?Gestione opzioni del subfile S01                             ?
054000170704       //--------------------------------------------------------------
054100170704       BEGSR  sr_OpzS01;
054200170704
054300170704         // -?Spegnimento degli indicatori di errore?
054400170704         %subst( IndDspF : 50 ) = *off;
054500170704
054600170704         clear  SavS01csr;
054700170704
054800170704         // -?Ciclo di lettura subfile?
054900170704         readc  SF25S01;
055000170704
055100170704         DoW  Not %eof(TNSF25D);
055200170704
055300170704           $SflNxtChg = *off;
055400170704           %subst( IndDspF : 50 ) = *off;
055500170704           C1RcdNbr   = S01nrr;
055600170704
055700170704           Select;
055800170704
055900170704             // -?Nessuna opzione?
056000170704             When  S1Copz = *blank;
056100170704
056200170704             // -?Opz. 2=Modifica/Cancellazione?
056300170704             When  S1Copz  = '2';
056400170704               $ProtectKEY = *on;
056500170704               $InzD02     = *on;
056600170704               $Video      = 'D2';
056700170704               DoW  $Video = 'D2';
056800170704                 exsr  sr_GesD02;
056900170704               EndDo;
057000170704               clear  $ProtectKEY;
057100170704
057200170707             // -?Opz. 4=Cancellazione?
057300170707             //  ?Opz. 5=Visualizzazione?
057400170707             When  S1Copz  = '4'  or
057500170707                   S1Copz  = '5';
057600170707               $Cancellazione = ( S1Copz = '4' );
057700170704               $Protect    = *on;
057800170704               $InzD02     = *on;
057900170704               $Video      = 'D2';
058000170704               DoW  $Video = 'D2';
058100170704                 exsr  sr_GesD02;
058200170704               EndDo;
058300170704               clear  $Protect;
058400170704
058500170704             // -?Opzione errata?
058600170704             Other;
058700170704               $ErrGenerico = *on;
058800170704               $ErrMessage  = *on;
058900170704               $PosCurOPZ   = *on;
059000170704               VIDmsg = sk_Msg(02);
059100170704
059200170704           EndSl;
059300170704
059400170704           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
059500170704           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
059600170704           if  S1Copz   <> *blank  and
059700170704              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
059800170704             SavS01csr   = C1RcdNbr;
059900170704           endif;
060000170704
060100170704           // -?Aggiornamento sfl?
060200170704           if  $ErrGenerico;
060300170704             C1CsrRrn   = C1RcdNbr;
060400170707             $SflNxtChg = *on;
060500170707             //*·clear  S1Copz;
060600170704           else;
060700170704             clear  S1Copz;
060800170704           endif;
060900170704
061000170704           UPDATE  SF25S01;
061100170704
061200170704           if  $ErrGenerico  or  $ErrMessage;
061300170704             leave;
061400170704           endif;
061500170704
061600170704           readc  SF25S01;
061700170704
061800170704         ENDDO;
061900170704
062000170704         // -?Riposizionam. cursore sul record contenente la prima opz.?
062100170704         //  ?(se non sono stati rilevati errori)?
062200170704         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
062300170704             SavS01csr > *zero;
062400170704           C1CsrRrn = SavS01csr;
062500170704         endif;
062600170704
062700170704       ENDSR;
062800170704
062900170704       //--------------------------------------------------------------
063000170704       //?Gestione videata D02.                                        ?
063100170704       //--------------------------------------------------------------
063200170704       BEGSR  sr_GesD02;
063300170704
063400170704         // -?Inizializzazione videata?
063500170704         if  $InzD02 = *on;
063600170704           exsr  sr_InzD02;
063700170704           $InzD02 = *off;
063800170704         endif;
063900170706
064000170706         // -?Cancellazione (da opz. 4 => $Cancellazione) NON ammessa?
064100170706         //  ?per la presenza di fatture con Registro Iva 913?
064200170706         //  ?(quella da F16 è già stata disabilitata nella subr. sr_InzD02)?
064300170706         If  $Cancellazione  and  $TIIFT_FI7;
064400170706           $Video = 'S1';
064500170707           VIDmsg = %trimr( sk_Msg(12) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
064600170706           $ErrMessage  = *on;
064700170706           $ErrGenerico = *on;
064800170706           leavesr;
064900170706         EndIf;
065000170704
065100170704         // -?Emissione Testata e Piede con tasti funzionali abilitati?
065200170704         write  SF25T01;
065300170704         write  SF25P01;
065400170704
065500170704         // -?Emissione videata?
065600170704         if  Not $Protect;
065700170704           exfmt  SF25D02;
065800170704         else;
065900170704           write  SF25D02;
066000170704           exfmt  Protect;
066100170704         endif;
066200170704
066300170704         clear  VIDmsg;
066400170704         reset  $ErrMessage;
066500170704         reset  $ErrGenerico;
066600170704
066700170704         SELECT;
066800170704
066900170704           // -?F3=Fine?
067000170704           WHEN  dsp_aid = c_F03;
067100170705             unlock  ANSPI00F;
067200170704             $Video = 'S1';
067300170704             $ErrGenerico = *on;
067400170704             if  $Immissione;
067500170704               $Fine = *on;
067600170704             endif;
067700170704
067800170704           // -?F12=Ritorno?
067900170704           WHEN  dsp_aid = c_F12;
068000170705             unlock  ANSPI00F;
068100170704             $Video = 'S1';
068200170704
068300170704           // -?Enter / F6=Conferma / F16=Cancellazione?
068400170704           OTHER;
068500170704             // -?Controlli eseguiti NON se F16=Annullamento?
068600170707             //*·if  Not $Cancellazione;
068700170707             if  dsp_Aid <> c_F16;
068800170704               exsr  sr_CtrD02;
068900170704             endif;
069000170704             if  $ErrGenerico;
069100170704               leavesr;
069200170704             endif;
069300170706             // -?Aggiornamento?
069400170704             If  dsp_aid = c_F06   or
069500170704                 dsp_aid = c_F16;
069600170704               exsr  sr_Upd_ANSPI;
069700170704               // -?Rilevato errore in fase di aggiornamento?
069800170704               if  $ErrGenerico = *on;
069900170704                 leavesr;
070000170704               endif;
070100170704               // -?SubFile iniziale altrimenti?
070200170704               $Video  = 'S1';
070300170704               $InzS01 = *on;
070400170704             EndIf;
070500170704
070600170704         ENDSL;
070700170704
070800170704       ENDSR;
070900170704
071000170704       //--------------------------------------------------------------
071100170704       //?Inizializzazione videata D02.                                ?
071200170704       //--------------------------------------------------------------
071300170704       BEGSR  sr_InzD02;
071400170704
071500170704         // -?Spegnimento degli indicatori di errore?
071600170706         %subst( IndDspF : 41 ) = *off;
071700170704
071800170704         // -?Pulizia videata?
071900170704         clear  SF25D02;
072000170704
072100170704         // -?Reperimento dati in gestione?
072200170704         //  ?(SE NON Immissione)?
072300170704         If  Not $Immissione;
072400170704
072500170704           // -?Reperimento dati?
072600170707           if  $Protect;
072700170705             chain(N)  S1Hnrr  ANSPI000;
072800170704           else;
072900170705             chain  S1Hnrr  ANSPI000;
073000170704           endif;
073100170704
073200170704           // -?Caricamento dati a video?
073300170705           if  %found( ANSPI00F );
073400170704             exsr  sr_RieD02;
073500170704           else;
073600170704             VIDmsg = sk_Msg(03);
073700170704             //*·$PosCurIVA   = *on;
073800170704             $ErrMessage  = *on;
073900170704             $ErrGenerico = *on;
074000170704             //*·leavesr;
074100170704           endif;
074200170704
074300170704         Else;
074400170704
074500170704           // -?Impostazione dati di default SE immissione?
074600170704           V2Ddde = *date;
074700170704           V2Ddsc = c_MaxDat_Eur;
074800170704
074900170704         EndiF;
075000170704
075100170704         // -?Impostazione testata?
075200170704         clear  V2Topz;
075300170704         Select;
075400170706           When  $Cancellazione;
075500170706             V2Topz = ' CANCELLAZIONE ';
075600170704           When  $Immissione;
075700170704             V2Topz = '  INSERIMENTO  ';
075800170706           When  $Protect;
075900170706             V2Topz = 'VISUALIZZAZIONE';
076000170704           Other;
076100170704             V2Topz = '   MODIFICA    ';
076200170704         EndSl;
076300170704
076400170704         // -?Impostazione attributi di visualizzazione?
076500170707         $ProtectKEY = ( Not $Immissione );
076600170707         //*·$ProtectDDE = ( Not $Immissione  and
076700170704         //*·                SPIdde <= wDate );
076800170707         //*·$ProtectDSC = ( Not $Immissione  and
076900170706         //*·                SPIdsc <= wDate );
077000170704
077100170706         // -?(Dis)Abilitazione tasti funzionali?
077200170706         exsr  sr_AbilitFxx;
077300170704
077400170704       ENDSR;
077500170704
077600170704       //--------------------------------------------------------------
077700170705       //?Caricamento dati del singolo record del file ANSPI00F nella  ?
077800170704       //?videata D02.                                                 ?
077900170704       //--------------------------------------------------------------
078000170704       BEGSR  sr_RieD02;
078100170704
078200170707         V2Civa = SPIiva;
078300170707         V2Dima = SPIima;
078400170704
078500170707         reset  WLBdat;
078600170707         G08inv   = SPIdde;
078700170707         XSRDA8(WLBdat);
078800170707         if  G08err = *zero;
078900170707           V2Ddde = G08dat;
079000170707         endif;
079100170704
079200170707         reset  WLBdat;
079300170707         G08inv   = SPIdsc;
079400170707         XSRDA8(WLBdat);
079500170707         if  G08err = *zero;
079600170707           V2Ddsc = G08dat;
079700170707         endif;
079800170705
079900170707         if  SPIdim <> *zero  or  SPIpim <> *blank;
080000170707           reset  WLBdat;
080100170707           G08inv   = SPIdim;
080200170707           XSRDA8(WLBdat);
080300170707           if  G08err = *zero;
080400170707             V2Ddim = G08dat;
080500170707           endif;
080600170707         endif;
080700170707         V2Dhim = SPIhim;
080800170707         V2Dpim = SPIpim;
080900170705
081000170707         if  SPIdmo <> *zero  or  SPIpmo <> *blank;
081100170707           reset  WLBdat;
081200170707           G08inv   = SPIdmo;
081300170707           XSRDA8(WLBdat);
081400170707           if  G08err = *zero;
081500170707             V2Ddmo = G08dat;
081600170707           endif;
081700170707         endif;
081800170707         V2Dhmo = SPIhmo;
081900170707         V2Dpmo = SPIpmo;
082000170706
082100170706         // -?Ricerca della 1ª data fattura del cliente con Registro IVA?
082200170706         //  ?"Split Payment" (913)?
082300170706         exsr  sr_RicercaDataFattura913_Prima;
082400170706
082500170706         // -?Ricerca dell'ultima data fattura del cliente con Registro IVA?
082600170706         //  ?"Split Payment" (913)?
082700170706         exsr  sr_RicercaDataFattura913_Ultima;
082800170705
082900170705         // -?Impostazione Attributi di visualizzazione?
083000170707         $VisualINS = ( Not $Immissione );
083100170707         $VisualMOD = ( Not $Immissione  and  SPIdmo > *zero );
083200170704
083300170704       ENDSR;
083400170704
083500170704       //--------------------------------------------------------------
083600170704       //?Controllo dati videata D02.                                  ?
083700170704       //--------------------------------------------------------------
083800170704       BEGSR  sr_CtrD02;
083900170704
084000170704         // -?Spegnimento degli indicatori di errore?
084100170704         %subst(IndDspF : 50) = *off;
084200170704
084300170705         // -?Controllo inserimento Partita Iva?
084400170705         clear  V2Diva;
084500170705         If  V2Civa = *blank;
084600170705           VIDmsg = sk_Msg(04);
084700170705           $PosCurIVA   = *on;
084800170704           $ErrMessage  = *on;
084900170704           $ErrGenerico = *on;
085000170704           leavesr;
085100170704         EndIf;
085200170705
085300170710         // -?Controllo correttezza Partita Iva inserita?
085400170710         //  ?(solo ITALIANA può essere)?
085500170710         If  $Immissione;
085600170710           clear  xCFIVA1ds;
085700170710           xCFIVAmod = 'P';
085800170710           // -?Partita Iva con Nazione davanti?
085900170710           if  %subst( V2Civa : 1 : 2 ) < '00';
086000170710             xCFIVApar = V2Civa;
086100170710           // -?Partita Iva senza Nazione davanti (Italiana)?
086200170710           else;
086300170710             xCFIVApar = '  ' + V2Civa;
086400170710           endif;
086500170710           xCFIVAr1 ( xCFIVA1ds );
086600170710           if  xCFIVAflg < *zero;
086700170710             VIDmsg = xCFIVAmsg;
086800170710             $PosCurIVA   = *on;
086900170710             $ErrMessage  = *on;
087000170710             $ErrGenerico = *on;
087100170710             leavesr;
087200170710           endif;
087300170710         EndIf;
087400170704
087500170706         // -?Controllo che la P.I. non risulti già inserita?
087600170706         //  ?(anche se per periodo già scaduto...)?
087700170707         If  $Immissione;
087800170706           clear  keyANSPI01;
087900170706           k_SPIiva = V2Civa;
088000170706           chain  %kds( keyANSPI01 )  ANSPI001;
088100170706           if  %found( ANSPI01L );
088200170707             VIDmsg = sk_Msg(05);
088300170706             reset  WLBdat;
088400170706             G08inv   = SPIdde;
088500170706             XSRDA8(WLBdat);
088600170706             VIDmsg = %trimr( VIDmsg ) + ' (per il periodo dal ' +
088700170706                      %trim( %editw( G08dat : '  /  /    ' ) );
088800170706             reset  WLBdat;
088900170706             G08inv   = SPIdsc;
089000170706             XSRDA8(WLBdat);
089100170706             VIDmsg = %trimr( VIDmsg ) + ' al ' +
089200170706                      %trim( %editw( G08dat : '  /  /    ' ) ) + ')';
089300170707             $PosCurIVA   = *on;
089400170706             $ErrMessage  = *on;
089500170706             $ErrGenerico = *on;
089600170706             leavesr;
089700170706           endif;
089800170706         EndIf;
089900170704
090000170704         // -?Controllo periodo validità?
090100170705         // ·?Data Decorrenza?
090200170704         clear  WLBdat;
090300170705         G08dat = V2Ddde;
090400170705         XSRDA8( WLBdat );
090500170704         if  G08err = *zero;
090600170705           V2Ddde = G08dat;
090700170705           W2Ddde = G08inv;
090800170704         else;
090900170707           VIDmsg = sk_Msg(06);
091000170705           $PosCurDDE   = *on;
091100170704           $ErrMessage  = *on;
091200170704           $ErrGenerico = *on;
091300170704           leavesr;
091400170704         endif;
091500170704         // ·?Data Scadenza?
091600170704         clear  WLBdat;
091700170705         G08dat = V2Ddsc;
091800170705         XSRDA8( WLBdat );
091900170704         if  G08err = *zero;
092000170705           V2Ddsc = G08dat;
092100170705           W2Ddsc = G08inv;
092200170704         else;
092300170707           VIDmsg = sk_Msg(07);
092400170705           $PosCurDSC   = *on;
092500170704           $ErrMessage  = *on;
092600170704           $ErrGenerico = *on;
092700170704           leavesr;
092800170704         endif;
092900170704         // ·?Range di date?
093000170705         if  W2Ddde > W2Ddsc;
093100170707           VIDmsg = sk_Msg(08);
093200170705           $PosCurDSC   = *on;
093300170704           $ErrMessage  = *on;
093400170704           $ErrGenerico = *on;
093500170704           leavesr;
093600170704         endif;
093700170704
093800170706         // -?La Data Decorrenza NON deve essere successiva alla 1ª data?
093900170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
094000170706         exsr  sr_RicercaDataFattura913_Prima;
094100170705         If  Not %eof( TIIFT02L )  and
094200170705             W2Ddde > IFTdft;
094300170707           VIDmsg = %trimr( sk_Msg(09) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
094400170705           $PosCurDDE   = *on;
094500170705           $ErrMessage  = *on;
094600170705           $ErrGenerico = *on;
094700170705           leavesr;
094800170705         EndIf;
094900170705
095000170706         // -?La Data Scadenza NON deve essere precedente all'ultima data?
095100170706         //  ?fattura del cliente con registro IVA "Split Payment" (913)?
095200170706         exsr  sr_RicercaDataFattura913_Ultima;
095300170705         If  Not %eof( TIIFT02L )  and
095400170705             W2Ddsc < IFTdft;
095500170707           VIDmsg = %trimr( sk_Msg(10) ) + ' ' + %editc( dsQC2.§QCfi7 : 'X' );
095600170705           $PosCurDSC   = *on;
095700170705           $ErrMessage  = *on;
095800170705           $ErrGenerico = *on;
095900170705           leavesr;
096000170705         EndIf;
096100170704
096200170705         // -?Verifica esistenza in ANSPI01L?
096300170705         //  ?(con sovrapposizione di giorni nel periodo)?
096400170704         // ·?Esempi - Periodo esistente:?· · 3-----6 · ·?
096500170705         // ·? 1)     Periodo inseribile: 1-2?
096600170705         // ·? 2) Periodo NON inseribile:   2---4?
096700170705         // ·? 3) Periodo NON inseribile:       4-5?
096800170705         // ·? 4) Periodo NON inseribile:         5---7?
096900170705         // ·? 5) Periodo NON inseribile:   2---------7?
097000170705         // ·? 6)     Periodo inseribile:             7-8?
097100170707         IF  $Immissione;
097200170705
097300170705           clear  keyANSPI01;
097400170705           k_SPIiva = V2Civa;
097500170705           setll  %kds( keyANSPI01 )  ANSPI001;
097600170705           reade  %kds( keyANSPI01 )  ANSPI001;
097700170705
097800170705           DoW  NOT %eof( ANSPI01L );
097900170705
098000170705             select;
098100170705               // ·? 2) e 3)?
098200170705               when  ( W2Ddsc >= SPIdde  and
098300170705                       W2Ddsc <= SPIdsc );
098400170707                 VIDmsg = sk_Msg(11);
098500170705                 $PosCurDDE   = *on;
098600170705                 $ErrMessage  = *on;
098700170705                 $ErrGenerico = *on;
098800170705                 leavesr;
098900170705               // ·? 3) e 4)?
099000170705               when  ( W2Ddde >= SPIdde  and
099100170705                       W2Ddde <= SPIdsc );
099200170707                 VIDmsg = sk_Msg(11);
099300170705                 $PosCurDDE   = *on;
099400170705                 $ErrMessage  = *on;
099500170705                 $ErrGenerico = *on;
099600170705                 leavesr;
099700170705               // ·? 5)?
099800170705               when  ( W2Ddde <= SPIdde  and
099900170705                       W2Ddde >= SPIdsc );
100000170707                 VIDmsg = sk_Msg(11);
100100170705                 $PosCurDDE   = *on;
100200170705                 $ErrMessage  = *on;
100300170705                 $ErrGenerico = *on;
100400170705                 leavesr;
100500170705             endsl;
100600170705
100700170705             reade  %kds( keyANSPI01 )  ANSPI001;
100800170705
100900170704           EndDo;
101000170705
101100170705         ENDIF;
101200170704
101300170704       ENDSR;
101400170706
101500170706       //--------------------------------------------------------------
101600170706       //?Settaggio degli indicatori per la (dis)abilitazione dei tasti?
101700170706       //?  funzionali nelle diverse videate.                          ?
101800170706       //--------------------------------------------------------------
101900170706       BEGSR  sr_AbilitFxx;
102000170706
102100170706         // -?F3 = Fine?
102200170706         $F3Attivo  = *on;
102300170706
102400170706         // -?F6 = Conferma?
102500170706         $F6Attivo  = ( $Video = 'D2'  and  Not $Protect );
102600170706
102700170706         // -?F10 = Inserimento?
102800170706         $F10Attivo = ( $Video = 'S1' );
102900170706
103000170706         // -?F12 = Ritorno?
103100170706         $F12Attivo = ( $Video = 'D2' );
103200170706
103300170706         // -?F16 = Cancellazione?
103400170706         $F16Attivo = ( $Video = 'D2'     and
103500170706                        Not $Immissione   and
103600170706                        %found(ANSPI00F)  and
103700170706                        Not $TIIFT_FI7    and
103800170706                      ( Not $Protect      or
103900170706                        $Cancellazione ) );
104000170706       ENDSR;
104100170706
104200170706       //--------------------------------------------------------------
104300170706       //?Ricerca della data della PRIMA fattura con Registro IVA 913  ?
104400170706       //?per il cliente.                                              ?
104500170706       //--------------------------------------------------------------
104600170706       BEGSR  sr_RicercaDataFattura913_Prima;
104700170706
104800170706         clear  V2Dtxd;
104900170706
105000170706         clear  keyTIIFT02;
105100170706         k_IFTpiva = V2Civa;
105200170706         k_IFTfiv  = dsQC2.§QCfi7;
105300170706
105400170706         //*·k_IFTdft  = W2Ddde;
105500170706         //*·setll  %kds( keyTIIFT02 )  TIIFT000;
105600170706         //*·readPe  %kds( keyTIIFT02 : 2 )  TIIFT000;
105700170706
105800170706         //*·clear  k_IFTdft;
105900170706         setll  %kds( keyTIIFT02 )  TIIFT000;
106000170706         reade  %kds( keyTIIFT02 : 2 )  TIIFT000;
106100170706
106200170706         If  Not %eof( TIIFT02L );
106300170706
106400170706           reset  WLBdat;
106500170706           G08inv   = IFTdft;
106600170706           XSRDA8(WLBdat);
106700170706
106800170706           V2Dtxd = 'Fatt. con Reg. IVA ' +
106900170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' dal ' +
107000170706                    %editw( G08dat : '  /  /    ' );
107100170706
107200170706         EndIf;
107300170706
107400170706       ENDSR;
107500170706
107600170706       //--------------------------------------------------------------
107700170706       //?Ricerca della data dell'ULTIMA fattura con Registro IVA 913  ?
107800170706       //?per il cliente.                                              ?
107900170706       //--------------------------------------------------------------
108000170706       BEGSR  sr_RicercaDataFattura913_Ultima;
108100170706
108200170706         clear  V2Dtxs;
108300170706
108400170706         clear  keyTIIFT02;
108500170706         k_IFTpiva = V2Civa;
108600170706         k_IFTfiv  = dsQC2.§QCfi7;
108700170706
108800170706         //*·k_IFTdft  = W2Ddsc;
108900170706         //*·setgt  %kds( keyTIIFT02 )  TIIFT000;
109000170706         //*·reade  %kds( keyTIIFT02 : 2 )  TIIFT000;
109100170706
109200170706         k_IFTdft  = *hival;
109300170706         setgt  %kds( keyTIIFT02 )  TIIFT000;
109400170706         readPe  %kds( keyTIIFT02 : 2 )  TIIFT000;
109500170706
109600170706         If  Not %eof( TIIFT02L );
109700170706
109800170706           reset  WLBdat;
109900170706           G08inv   = IFTdft;
110000170706           XSRDA8(WLBdat);
110100170706
110200170706           V2Dtxs = 'Fatt. con Reg. IVA ' +
110300170706                    %editc( dsQC2.§QCfi7 : 'X' ) + ' al  ' +
110400170706                    %editw( G08dat : '  /  /    ' );
110500170706
110600170706         EndIf;
110700170706
110800170706       ENDSR;
110900170704
111000170704       //--------------------------------------------------------------
111100170704       //?Aggiornamento dati nel *file ANSPI00F.                       ?
111200170704       //--------------------------------------------------------------
111300170704       BEGSR  sr_Upd_ANSPI;
111400170704
111500170705         // -?Aggancio record di ANSPI00F?
111600170707         If  Not $Immissione;
111700170705           chain  S1Hnrr  ANSPI000;
111800170705         EndIf;
111900170705
112000170705         // -?Reperimento data odierna?
112100170705         wDate = %int( %subst( %char( %dec( %timestamp() ) )
112200170705                               : 1 : 8 ) );
112300170705         // -?Reperimento orario attuale?
112400170705         wTime = %int( %subst( %char( %dec( %timestamp() ) )
112500170705                               : 9 : 4 ) );
112600170704
112700170704         // -?Impostazione dati?
112800170707         If  $Immissione  or
112900170705             ( NOT %found( ANSPI00F ) and
113000170704               NOT $Cancellazione );
113100170705           clear  ANSPI000;
113200170705           SPIiva = V2Civa;
113300170705           SPIdim = wDate;
113400170705           SPIhim = wTime;
113500170705           SPIpim = kNmUs;
113600170705           SPIima = 'M';            //?(M=Inserimento Manuale)?
113700170705         EndIf;
113800170704
113900170707         //*·If  Not $Cancellazione;
114000170707         If  dsp_aid <> c_F16;
114100170707
114200170705           SPIdde = W2Ddde;
114300170705           SPIdsc = W2Ddsc;
114400170705
114500170707           If  Not $Immissione;
114600170707             SPIdmo = wDate;
114700170707             SPIhmo = wTime;
114800170707             SPIpmo = kNmUs;
114900170707           EndIf;
115000170707
115100170707         EndIf;
115200170704
115300170704         // -?Aggiornamento record?
115400170704         SELECT;
115500170704
115600170704           // -?F16=Cancellazione?
115700170707           //*·WHEN  $Cancellazione  and  %found( ANSPI00F );
115800170707           WHEN  dsp_aid = c_F16  and  %found( ANSPI00F );
115900170705             //__________________
116000170705             delete(E)  ANSPI000;
116100170705             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
116200170704
116300170704           // -?Immissione o Copia o Modifica?
116400170704           OTHER;
116500170707             if  Not $Immissione  and
116600170705                 %found( ANSPI00F );
116700170704               //_________________
116800170705               update(E) ANSPI000;
116900170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117000170704             else;
117100170704               //_________________
117200170705               write(E)  ANSPI000;
117300170704               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117400170704             endif;
117500170704
117600170704         ENDSL;
117700170704
117800170704         // -?Segnalazione errore rilevato in fase di?
117900170704         //  ?scrittura/aggiornamento/cancellazione?
118000170704         If  %error();
118100170707           VIDmsg = sk_Msg(13);
118200170704           $ErrMessage  = *on;
118300170704           $ErrGenerico = *on;
118400170704           // -?Stampa del Dump?
118500170704           Dump(A);
118600170704           // -?Stampa del Job-Log?
118700170704           Qcmd = 'DSPJOBLOG job(*) output(*print)';
118800170704           exsr  sr_ExecCmd;
118900170704           leavesr;
119000170704         EndIf;
119100170704
119200170704       ENDSR;
119300170704
119400170704       //--------------------------------------------------------------
119500170704       //?Esecuzione del comando (già impostato).                      ?
119600170704       //--------------------------------------------------------------
119700170704       BEGSR  sr_ExecCmd;
119800170704
119900170704         clear Qcap0100;
120000170704         Qcabcsdh = *off;
120100170704         Qcapa    = *off;
120200170704         Qcacmdss = *off;
120300170704         Qcaerved = *allX'00';
120400170704
120500170704         clear Qusec;
120600170704         Qusbprv  = %size(Qusec);
120700170704
120800170704         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
120900170704                           %size(Qcap0100) : 'CPOP0100' : *omit :
121000170704                           0 : 0 : Qusec);
121100170704
121200170704         //*·if  Qusei <> *blank;
121300170704         //*·  exsr  sr_PrintErr;
121400170704         //*·endif;
121500170704
121600170704       ENDSR;
121700170704
121800170704       //--------------------------------------------------------------
121900170704       //?Operazioni finali.                                           ?
122000170704       //--------------------------------------------------------------
122100170704       BEGSR  sr_RoutEnd;
122200170707
122300170707         // -?Chiusura funzioni precedentemente aperte?
122400170707         cloTabella ( c_Tabel );
122500170704
122600170704         //  ?Uscita?
122700170704         return;
122800170704
122900170704       ENDSR;
123000170704
123100170704      /end-free
123200170704
123300170704       //--------------------------------------------------------------
123400170704       //?Definizione schiere a tempo di compilazione.                 ?
123500170704       //--------------------------------------------------------------
123600170704
123700170704** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
123800170704Raggiunto il limite massimo dei record caricabili a video                       1
123900170704Opzione errata                                                                  2
124000170704Partita Iva NON più reperibile in questo archivio                               3
124100170704Immettere la Partita Iva                                                        4
124200170707Partita Iva già inserita                                                        5
124300170707Data decorrenza formalmente errata                                              6
124400170707Data scadenza formalmente errata                                                7
124500170707Data di scadenza precedente a quella di decorrenza                              8
124600170707Data decorrenza successiva quella della 1ª fattura con Registro IVA             9
124700170707Data scadenza precedente quella dell'ultima fattura con Registro IVA           10
124800170707Il periodo si sovrappone a quello di un'altro Split Payment già inserito       11
124900170707Partita Iva NON cancellabile: trovate fatture con Registro IVA                 12
125000170707Rilevato errore in fase di registrazione dati => Avvisare CED                  13
