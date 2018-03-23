000100170403       //==============================================================
000200170403       //?Gestione ECCEZIONI per Orari di Inizio/Fine picking.         ?
000300170403       //==============================================================
000400170403
000500170403       //--------------------------------------------------------------
000600170403       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170403       //--------------------------------------------------------------
000800170403
000900170403     /*PRM  dbgview(*source)
001000170403     /*END
001100170403
001200170403       //--------------------------------------------------------------
001300170403       //?Specifiche di controllo.                                     ?
001400170403       //--------------------------------------------------------------
001500170403
001600170403     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700170403     h dftactgrp(*no)
001800170403
001900170403       //--------------------------------------------------------------
002000170403       //?Dichiarazione file.                                          ?
002100170403       //--------------------------------------------------------------
002200170403
002300170403       // -?Organigramma?
002400170403     fAZORG01L  if   e           k disk
002500170403
002600170403       // -?Letture modalità Picking - Eccezioni?
002700170404     fFIMSE01L  if   e           k disk    rename( FIMSE000 : FIMSE001 )
002800170404     f                                     infds( InfFIMSE )
002900170404     fFIMSE00F  Uf A e             disk
003000170403
003100170403       // -?Video?
003200170403     fFIMS27D   cf   e             workstn
003300170403     f                                     sfile( MS27S01 : S01nrr )
003400170403     f                                     indds( IndDspF )
003500170403     f                                     infds( InfDspF )
003600170403
003700170403       //--------------------------------------------------------------
003800170403       //?Definizione costanti.                                        ?
003900170403       //--------------------------------------------------------------
004000170403
004100170404       // -?Data Scadenza massima - in formato *EUR?
004200170404     d c_MaxDat_Eur    c                   const(31122039)
004300170404
004400170403       // -?Numero di record in ciascuna videata di subfile?
004500170403     d c_SflPag        c                   const(15)
004600170403
004700170403       // -?Numero massimo di record gestibili?
004800170403     d c_MaxRec        c                   const(9999)
004900170403
005000170403       // -?Attributi di Visualizzazione?
005100170403     d c_DspAtrNorm    c                   const(x'A0')
005200170403     d c_DspAtrRI      c                   const(x'A1')
005300170403     d c_DspAtrHI      c                   const(x'A2')
005400170403     d c_DspAtrND      c                   const(x'A7')
005500170403     d c_DspAtrRED     c                   const(x'A8')
005600170403     d c_DspAtrRI_RED  c                   const(x'A9')
005700170403     d c_DspAtrBL_RED  c                   const(x'AA')
005800170403
005900170403       // -?Tasti funzionali a video?
006000170403     d c_F01           c                   const(x'31')
006100170403     d c_F02           c                   const(x'32')
006200170403     d c_F03           c                   const(x'33')
006300170403     d c_F04           c                   const(x'34')
006400170403     d c_F05           c                   const(x'35')
006500170403     d c_F06           c                   const(x'36')
006600170403     d c_F07           c                   const(x'37')
006700170403     d c_F08           c                   const(x'38')
006800170403     d c_F09           c                   const(x'39')
006900170403     d c_F10           c                   const(x'3A')
007000170403     d c_F11           c                   const(x'3B')
007100170403     d c_F12           c                   const(x'3C')
007200170403     d c_F13           c                   const(x'B1')
007300170403     d c_F14           c                   const(x'B2')
007400170403     d c_F15           c                   const(x'B3')
007500170403     d c_F16           c                   const(x'B4')
007600170403     d c_F17           c                   const(x'B5')
007700170403     d c_F18           c                   const(x'B6')
007800170403     d c_F19           c                   const(x'B7')
007900170403     d c_F20           c                   const(x'B8')
008000170403     d c_F21           c                   const(x'B9')
008100170403     d c_F22           c                   const(x'BA')
008200170403     d c_F23           c                   const(x'BB')
008300170403     d c_F24           c                   const(x'BC')
008400170403     d c_Enter         c                   const(x'F1')
008500170403     d c_RollDown      c                   const(x'F4')
008600170403     d c_RollUp        c                   const(x'F5')
008700170403
008800170403       //--------------------------------------------------------------
008900170403       //?Definizione schiere.                                         ?
009000170403       //--------------------------------------------------------------
009100170403
009200170403       // -?Giorni della settimana?
009300170403     d sk_Gio          s              9    dim( 7)  ctdata  perrcd( 1)
009400170403
009500170403       // -?Messaggi di errore?
009600170407     d sk_Msg          s             78    dim(16)  ctdata  perrcd( 1)
009700170403
009800170403       //--------------------------------------------------------------
009900170403       //?Definizione aree dati.                                       ?
010000170403       //--------------------------------------------------------------
010100170403
010200170403       // -?Dati utente?
010300170403     d §AzUte        e ds                  extname(AZUTE00F)
010400170403     d                                     dtaara
010500170403     d §DatiUte      e ds                  extname(dDatiUte)
010600170403     d                                     dtaara
010700170403
010800170403       //--------------------------------------------------------------
010900170403       //?Definizione strutture dati.                                  ?
011000170403       //--------------------------------------------------------------
011100170403
011200170403       // -?Status ds?
011300170403     d Status         sds
011400170403     d   SDSpgm          *proc
011500170403
011600170403       // -?InfDS?
011700170403     d InfDspF         ds
011800170403     d   dsp_aid             369    369a
011900170403     d*//sfl_rrn             376    377i 0
012000170403     d*//min_nrr             378    379i 0
012100170403     d*//num_rcds            380    381i 0
012200170404
012300170404     d InfFIMSE        ds                  qualified
012400170404     d   db_rrn              397    400i 0
012500170403
012600170403       // -?Indicatori su DspF?
012700170403     d IndDspF         ds                  inz
012800170403         // -?Abilitazione tasti funzionali?
012900170403     d   $F3Attivo                     n   overlay( IndDspF : 03 )
013000170403     d   $F6Attivo                     n   overlay( IndDspF : 06 )
013100170403     d   $F10Attivo                    n   overlay( IndDspF : 10 )
013200170403     d   $F12Attivo                    n   overlay( IndDspF : 12 )
013300170403     d   $F16Attivo                    n   overlay( IndDspF : 16 )
013400170403         // -?Emissione messaggio di errore?
013500170403     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013600170403         // -?Indicatori di gestione del subfile?
013700170403     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
013800170403     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
013900170403     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
014000170403     d   $SflEnd                       n   overlay( IndDspF : 33 )
014100170403         // -?Indicatori per Attributi di visualizzazione?
014200170403     d*//$DisabilOPZ                   n   overlay( IndDspF : 40 )
014300170403     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
014400170403     d   $ProtectDTI                   n   overlay( IndDspF : 42 )
014500170403         // -?Posizionamento cursore & segnalazione errore?
014600170403     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
014700170403     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
014800170403     d   $PosCurTUR                    n   overlay( IndDspF : 52 )
014900170403     d   $PosCurHMI                    n   overlay( IndDspF : 53 )
015000170403     d   $PosCurHMF                    n   overlay( IndDspF : 54 )
015100170403     d   $PosCurVS1                    n   overlay( IndDspF : 55 )
015200170403     d   $PosCurVS2                    n   overlay( IndDspF : 56 )
015300170403     d   $PosCurVS3                    n   overlay( IndDspF : 57 )
015400170403     d   $PosCurVS4                    n   overlay( IndDspF : 58 )
015500170403     d   $PosCurVS5                    n   overlay( IndDspF : 59 )
015600170403     d   $PosCurVS6                    n   overlay( IndDspF : 60 )
015700170403     d   $PosCurVS7                    n   overlay( IndDspF : 61 )
015800170403     d   $PosCurDTI                    n   overlay( IndDspF : 62 )
015900170403     d   $PosCurDTF                    n   overlay( IndDspF : 63 )
016000170403         // -?Riemissione videata?
016100170403     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
016200170403
016300170403       // -?Struttura dati per definire un orario (hh:mm:ss)?
016400170403     d wTime_ds        ds             6    qualified      inz
016500170403     d   wHH                          2s 0 inz
016600170403     d   wMM                          2s 0 inz
016700170403     d   wSS                          2s 0 inz
016800170403     d   wTime                 1      6s 0 inz
016900170403
017000170403       // -?Struttura dati per definire un orario (hh:mm:ss)?
017100170403     d wV2Cvs_ds       ds             7    inz
017200170403     d   V2Cvs1                            inz
017300170403     d   V2Cvs2                            inz
017400170403     d   V2Cvs3                            inz
017500170403     d   V2Cvs4                            inz
017600170403     d   V2Cvs5                            inz
017700170403     d   V2Cvs6                            inz
017800170403     d   V2Cvs7                            inz
017900170403     d   sk_V2Cvs              1      7    dim(7)         inz
018000170403
018100170403       // -?Parametri ricevuti?
018200170403     d KPJBA         e ds
018300170403
018400170407      *// // -?150ª descrizione Organigramma?
018500170407     d*// Og150         e ds                  qualified      inz
018600170403
018700170403       //--------------------------------------------------------------
018800170403       //?Definizione variabili globali.                               ?
018900170403       //--------------------------------------------------------------
019000170403
019100170403       // -?Flags booleani?
019200170403     d $Fine           s               n   inz
019300170403     d $InzS01         s               n   inz(*on)
019400170403     d $InzD02         s               n   inz(*on)
019500170403     d $Cancellazione  s               n   inz
019600170403     d $Copia          s               n   inz
019700170403     d $Immissione     s               n   inz
019800170403     d $Protect        s               n   inz
019900170403
020000170403       // -?Indici di schiera?
020100170403     d xx              s              3  0 inz
020200170403
020300170403       // -?Variabili per la gestione del video?
020400170403     d $Video          s              2    inz('S1')
020500170403     d S01nrr          s                   like(C1RcdNbr) inz
020600170403     d SavS01csr       s                   like(C1RcdNbr) inz
020700170403
020800170403       // -?Variabili per la gestione del posizionamento nel subfile?
020900170403     d SavC1Cfil       s                   like(C1Cfil)   inz
021000170403
021100170403       // -?Campi di comodo?
021200170403     d wDate           s              8s 0 inz
021300170403     d W2Cdti          s                   like(MSEdti)   inz
021400170403     d W2Cdtf          s                   like(MSEdtf)   inz
021500170403
021600170403       //--------------------------------------------------------------
021700170403       //?Definizione prototipi procedure.                             ?
021800170403       //--------------------------------------------------------------
021900170403
022000170403       // -?Reperimento dati utente?
022100170403     d TIBS34ds      e ds
022200170403      /copy gaitrasrc/srcProtoPR,TIBS34R
022300170403
022400170403       // -?Controllo/Inversione data?
022500170403     d WLBdat          ds                  inz
022600170404     d   G08dat                       8  0 inz
022700170404     d   G08inv                       8  0 inz
022800170404     d   G08err                       1    inz('3')
022900170404     d   G08tgi                       5  0 inz
023000170403      /copy gaitrasrc/srcProtoPR,XSRDA8
023100170405
023200170405       // -?Parametri API QCAPCMD (Process Commands)?
023300170405     d Qcmd            s           2048    inz  varying
023400170405      /copy qSysInc/qRpgleSrc,QCAPCMD
023500170405       // -?API QCAPCMD (Process Commands)?
023600170405      /copy gaitrasrc/srcProtoPR,QCAPCMD
023700170405
023800170405       // -?Parametri gestione errori API.?
023900170405      /copy qsysinc/qrpglesrc,QUSEC
024000170403
024100170403       //--------------------------------------------------------------
024200170403       //?Definizione key-list.                                        ?
024300170403       //--------------------------------------------------------------
024400170403
024500170403       // -?File FIMSE01L?
024600170403     d keyFIMSE01    e ds                  extname( FIMSE01L : *key )
024700170403     d                                     prefix( k_ )   inz
024800170403
024900170403       //--------------------------------------------------------------
025000170403       //?Riepilogo indicatori utilizzati.                             ?
025100170403       //--------------------------------------------------------------
025200170403       //--------------------------------------------------------------
025300170403
025400170403       //--------------------------------------------------------------
025500170403       //?M A I N - L I N E                                            ?
025600170403       //--------------------------------------------------------------
025700170403
025800170403     c     *Entry        plist
025900170403     c                   parm                    KPJBA
026000170403
026100170403      /free
026200170403
026300170403       // -?Operazioni iniziali?
026400170403       exsr  sr_RoutInz;
026500170403
026600170403       // -?Ciclo di gestione del file video?
026700170403       DOW  $Fine = *off;
026800170403
026900170403         select;
027000170403
027100170403           // -?Gestione videata S1 (subfile)?
027200170403           when $Video = 'S1';
027300170403             exsr sr_GesS01;
027400170403
027500170403           // -?Manutenzione dati della singola tabella?
027600170403           when $Video = 'D2';
027700170403             exsr  sr_GesD02;
027800170403
027900170403           // -?? ? ??
028000170403           other;
028100170403             $Fine = *on;
028200170403
028300170403         endsl;
028400170403
028500170403       ENDDO;
028600170403
028700170403       // -?Operazioni finali?
028800170403       exsr  sr_RoutEnd;
028900170403
029000170403       //--------------------------------------------------------------
029100170403       //?Operazioni iniziali.                                         ?
029200170403       //--------------------------------------------------------------
029300170403       BEGSR  sr_RoutInz;
029400170403
029500170403         // -?Impostazione chiusura?
029600170403         *inLR = *on;
029700170403
029800170403         // -?Reperimento dati job?
029900170403         exsr  sr_DatiJob;
030000170403
030100170403         // -?Reperimento data odierna?
030200170403         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030300170403                               : 1 : 8 ) );
030400170403
030500170403         // -?Impostazione nome programma a video?
030600170403         V1Tpgm = SDSpgm;
030700170403
030800170403         // -?Impostazione videata iniziale?
030900170403         $Video = 'S1';
031000170403         reset  $InzS01;
031100170403         reset  $InzD02;
031200170403
031300170403       ENDSR;
031400170403
031500170403       //--------------------------------------------------------------
031600170403       //?Reperimento Dati del job (Utente/Operativi).                 ?
031700170403       //--------------------------------------------------------------
031800170403       BEGSR  sr_DatiJob;
031900170403
032000170403         in(E) §AzUte;
032100170403         if NOT %error;
032200170403           in(E) §DatiUte;
032300170403         endif;
032400170403         if %error or RSut = *blanks;
032500170403           clear TIBS34ds;
032600170403           tibs34r ( tibs34ds );
032700170403           in §AzUte;
032800170403           in §DatiUte;
032900170403         endif;
033000170403
033100170403       ENDSR;
033200170403
033300170403       //--------------------------------------------------------------
033400170403       //?Gestione subfile S01.                                        ?
033500170403       //--------------------------------------------------------------
033600170403       BEGSR  sr_GesS01;
033700170403
033800170403         // -?Inizializzazione videata?
033900170403         if  $InzS01 = *on;
034000170403           exsr  sr_InzS01;
034100170403           $InzS01 = *off;
034200170403         endif;
034300170403
034400170403         // -?(Dis)Abilitazione tasti funzionali?
034500170403         exsr  sr_AbilitFxxC01;
034600170403
034700170403         // -?Emissione Testata e Piede con tasti funzionali abilitati?
034800170403         write  MS27T01;
034900170403         write  MS27P01;
035000170403
035100170403         // -?Posizionamento cursore?
035200170403         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035300170403         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035400170403         //  ?si visualizza la pagina che vedeva l'utente quando ha?
035500170403         //  ?premuto l'ultimo tasto.?
035600170403         if  C1CsrRrn > *zero;
035700170403           C1RcdNbr = C1CsrRrn;
035800170403         else;
035900170403           C1RcdNbr = 1;
036000170403           write  MS27S00;     //?(no rec.)?
036100170403         endif;
036200170403
036300170403         // -?Emissione videata (testata compresa) in window?
036400170403         exfmt  MS27C01;
036500170403
036600170403         clear  VIDmsg;
036700170403         reset  $ErrMessage;
036800170403         reset  $ErrGenerico;
036900170403
037000170403         reset  $Cancellazione;
037100170403         reset  $Copia;
037200170403         reset  $Immissione;
037300170403
037400170403         SELECT;
037500170403
037600170403           // -?F3=Fine?
037700170403           WHEN  dsp_aid = c_F03;
037800170403             $Fine   = *on;
037900170403
038000170403           // -?F10=Inserimento Filiale?
038100170403           WHEN  dsp_aid = c_F10;
038200170403             $Immissione = *on;
038300170403             $Video  = 'D2';
038400170403             $InzD02 = *on;
038500170403
038600170403           // -?F12=Ritorno?
038700170403           WHEN  dsp_aid = c_F12;
038800170403             $Fine   = *on;
038900170403
039000170403           // -?Subfile vuoto?
039100170403           WHEN  S01nrr = *zero;
039200170403             if  C1Cfil  <> SavC1Cfil;
039300170403               $InzS01 = *on;
039400170403             endif;
039500170403
039600170403           // -?Invio?
039700170403           OTHER;
039800170403             // -?Gestione opzioni?
039900170403             exsr  sr_OpzS01;
040000170403             if  $ErrGenerico;
040100170403               leavesr;
040200170403             endif;
040300170403             // -?Richiesto Posizionamento?
040400170403             if  C1Cfil <> SavC1Cfil;
040500170403               $InzS01 = *on;
040600170403               SavC1Cfil = C1Cfil;
040700170403             endif;
040800170403
040900170403         ENDSL;
041000170403
041100170403       ENDSR;
041200170403
041300170403       //--------------------------------------------------------------
041400170403       //?Inizializzazione subfile S01.                                ?
041500170403       //--------------------------------------------------------------
041600170403       BEGSR  sr_InzS01;
041700170403
041800170403         // -?Spegnimento degli indicatori di errore?
041900170403         %subst( IndDspF : 50 ) = *off;
042000170403
042100170403         // -?Pulizia subfile?
042200170403         $SflDsp_N    = *on;
042300170403         $SflDspCtl_N = *on;
042400170403         write  MS27C01;
042500170403         $SflDspCtl_N = *off;
042600170403         $SflEnd      = *off;
042700170403
042800170403         clear  C1RcdNbr;
042900170403         clear  C1CsrRrn;
043000170403         clear  S01nrr;
043100170403         clear  VIDmsg;
043200170403         $ErrMessage  = *off;
043300170403         $ErrGenerico = *off;
043400170403
043500170403         $SflNxtChg = *off;
043600170403
043700170403         // -?Caricamento completo dei dati nel subfile (per codice)?
043800170403         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
043900170403         //  ?l'eventuale ordinamento)?
044000170403         clear  keyFIMSE01;
044100170404         setll  %kds( keyFIMSE01 )  FIMSE001;
044200170404         read  FIMSE001;
044300170403
044400170403         DoW  Not %eof( FIMSE01L )  and  S01nrr < c_MaxRec;
044500170403           exsr  sr_WriteS01;
044600170404           read  FIMSE001;
044700170403         EndDo;
044800170403
044900170403         // -?Visualizzazione del SFL (se ci sono dati)?
045000170403         $SflDsp_N = ( S01nrr <= *zero );
045100170403
045200170403         // -?Attivazione del SFLEND?
045300170403         $SflEnd   = %eof( FIMSE01L );
045400170403
045500170403         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
045600170403         if  S01nrr > *zero;
045700170403           C1RcdNbr = 1;
045800170403         else;
045900170403           clear  C1RcdNbr;
046000170403         endif;
046100170403
046200170403         // -?Impostazione Opzioni?
046300170403         C1Dopz = '2=Modifica, 3=Copia, 5=Visualizzazione';
046400170403
046500170403         // -?Emissione messaggio di segnalazione raggiungimento limite?
046600170403         if  S01nrr >= c_MaxRec   and   Not %eof( FIMSE01L );
046700170403           $ErrGenerico = *on;
046800170403           $ErrMessage  = *on;
046900170403           $PosCurOPZ   = *on;
047000170403           VIDmsg = sk_Msg(01);
047100170403           leavesr;
047200170403         endif;
047300170403
047400170403       ENDSR;
047500170403
047600170403       //--------------------------------------------------------------
047700170403       //?Registrazione del singolo rec. nel subfile S01.              ?
047800170403       //--------------------------------------------------------------
047900170403       BEGSR  sr_WriteS01;
048000170403
048100170403         // -?Eventuale posizionamento richiesto?
048200170403         if  MSEfil < C1Cfil;
048300170403           leavesr;
048400170403         endif;
048500170403
048600170403         // -?Impostazione campi nel record del subfile?
048700170403         clear  MS27S01;
048800170404
048900170404         S1Hnrr   = InfFIMSE.db_rrn;
049000170404         S1Hdti   = MSEdti;
049100170404         S1Hdtf   = MSEdtf;
049200170404
049300170403         S1Cfil   = MSEfil;
049400170403         S1Ctur   = MSEtur;
049500170403         reset  WLBdat;
049600170403         G08inv = MSEdti;
049700170403         XSRDA8(WLBdat);
049800170404         if  G08err = *zero;
049900170403           S1Cdti = G08dat;
050000170403         endif;
050100170403         reset  WLBdat;
050200170403         G08inv = MSEdtf;
050300170403         XSRDA8(WLBdat);
050400170404         if  G08err = *zero;
050500170403           S1Cdtf = G08dat;
050600170403         endif;
050700170403         S1Chmi   = MSEhmi;
050800170403         S1Chmf   = MSEhmf;
050900170403         S1Cvs1   = MSEvs1;
051000170403         S1Cvs2   = MSEvs2;
051100170403         S1Cvs3   = MSEvs3;
051200170403         S1Cvs4   = MSEvs4;
051300170403         S1Cvs5   = MSEvs5;
051400170403         S1Cvs6   = MSEvs6;
051500170403         S1Cvs7   = MSEvs7;
051600170403
051700170403         // -?Decodifiche?
051800170403         chain  ( S1Cfil )  AZORG;
051900170403         if  %found(AZORG01L);
052000170403           S1Dfil = ORGdes;
052100170403         else;
052200170403           S1Dfil = *all'? ';
052300170403         endif;
052400170403
052500170403         // -?Registrazione singolo record nel subfile?
052600170403         S01nrr += 1;
052700170403         write  MS27S01;
052800170403
052900170403       ENDSR;
053000170403
053100170403       //--------------------------------------------------------------
053200170403       //?Settaggio indicatori nella videata C01/S01 per abilitazione  ?
053300170403       //?  tasti funzionali.                                          ?
053400170403       //--------------------------------------------------------------
053500170403       BEGSR  sr_AbilitFxxC01;
053600170403
053700170403         // ·?F3 = Fine?
053800170403         $F3Attivo  = *on;
053900170403
054000170403         // ·?F10 = Immissione?
054100170403         $F10Attivo = *on;
054200170403
054300170403         // ·?F12 = Ritorno?
054400170403         $F12Attivo = *off;
054500170403
054600170403       ENDSR;
054700170403
054800170403       //--------------------------------------------------------------
054900170403       //?Gestione opzioni del subfile S01                             ?
055000170403       //--------------------------------------------------------------
055100170403       BEGSR  sr_OpzS01;
055200170403
055300170403         // -?Spegnimento degli indicatori di errore?
055400170403         %subst( IndDspF : 50 ) = *off;
055500170403
055600170403         clear  SavS01csr;
055700170403
055800170403         // -?Ciclo di lettura subfile?
055900170403         readc  MS27S01;
056000170403
056100170403         DoW  Not %eof(FIMS27D);
056200170403
056300170403           $SflNxtChg = *off;
056400170403           %subst( IndDspF : 50 ) = *off;
056500170403           C1RcdNbr   = S01nrr;
056600170403
056700170403           Select;
056800170403
056900170403             // -?Nessuna opzione?
057000170403             When  S1Copz = *blank;
057100170403
057200170403             // -?Opz. 2=Modifica/Cancellazione?
057300170403             When  S1Copz = '2';
057400170403               $ProtectKEY = *on;
057500170403               $InzD02     = *on;
057600170403               $Video      = 'D2';
057700170403               DoW  $Video = 'D2';
057800170403                 exsr  sr_GesD02;
057900170403               EndDo;
058000170403               clear  $ProtectKEY;
058100170403
058200170403             // -?Opz. 3=Copia?
058300170403             When  S1Copz = '3';
058400170403               $Copia      = *on;
058500170403               $ProtectKEY = *off;
058600170403               $InzD02     = *on;
058700170403               $Video      = 'D2';
058800170403               DoW  $Video = 'D2';
058900170403                 exsr  sr_GesD02;
059000170403               EndDo;
059100170403               clear  $ProtectKEY;
059200170403
059300170403             // -?Opz. 5=Visualizzazione?
059400170403             When  S1Copz = '5';
059500170403               $Protect    = *on;
059600170403               $InzD02     = *on;
059700170403               $Video      = 'D2';
059800170403               DoW  $Video = 'D2';
059900170403                 exsr  sr_GesD02;
060000170403               EndDo;
060100170403               clear  $Protect;
060200170403
060300170403             // -?Opzione errata?
060400170403             Other;
060500170403               $ErrGenerico = *on;
060600170403               $ErrMessage  = *on;
060700170403               $PosCurOPZ   = *on;
060800170403               VIDmsg = sk_Msg(02);
060900170403
061000170403           EndSl;
061100170403
061200170403           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
061300170403           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
061400170403           if  S1Copz   <> *blank  and
061500170403              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
061600170403             SavS01csr   = C1RcdNbr;
061700170403           endif;
061800170403
061900170403           // -?Aggiornamento sfl?
062000170403           if  $ErrGenerico;
062100170403             C1CsrRrn   = C1RcdNbr;
062200170403             clear  S1Copz;
062300170403           else;
062400170403             clear  S1Copz;
062500170403           endif;
062600170403
062700170403           UPDATE  MS27S01;
062800170403
062900170403           if  $ErrGenerico  or  $ErrMessage;
063000170403             leave;
063100170403           endif;
063200170403
063300170403           readc  MS27S01;
063400170403
063500170403         ENDDO;
063600170403
063700170403         // -?Riposizionam. cursore sul record contenente la prima opz.?
063800170403         //  ?(se non sono stati rilevati errori)?
063900170403         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
064000170403             SavS01csr > *zero;
064100170403           C1CsrRrn = SavS01csr;
064200170403         endif;
064300170403
064400170403       ENDSR;
064500170403
064600170403       //--------------------------------------------------------------
064700170403       //?Gestione videata D02.                                        ?
064800170403       //--------------------------------------------------------------
064900170403       BEGSR  sr_GesD02;
065000170403
065100170403         // -?Inizializzazione videata?
065200170403         if  $InzD02 = *on;
065300170403           exsr  sr_InzD02;
065400170403           $InzD02 = *off;
065500170403         endif;
065600170403
065700170403         // -?Emissione Testata e Piede con tasti funzionali abilitati?
065800170403         write  MS27T01;
065900170403         write  MS27P02;
066000170403
066100170403         // -?Emissione videata?
066200170403         if  Not $Protect;
066300170403           exfmt  MS27D02;
066400170403         else;
066500170403           write  MS27D02;
066600170403           exfmt  Protect;
066700170403         endif;
066800170403
066900170403         clear  VIDmsg;
067000170403         reset  $ErrMessage;
067100170403         reset  $ErrGenerico;
067200170403
067300170403         SELECT;
067400170403
067500170403           // -?F3=Fine?
067600170403           WHEN  dsp_aid = c_F03;
067700170404             unlock FIMSE00F;
067800170404             $Video = 'S1';
067900170404             $ErrGenerico = *on;
068000170404             if  $Immissione;
068100170404               $Fine = *on;
068200170404             endif;
068300170403
068400170403           // -?F12=Ritorno?
068500170403           WHEN  dsp_aid = c_F12;
068600170404             unlock FIMSE00F;
068700170403             $Video = 'S1';
068800170403
068900170403           // -?Enter / F6=Conferma / F16=Cancellazione?
069000170403           OTHER;
069100170403             $Cancellazione = (dsp_aid = c_F16);
069200170403             // -?Controlli eseguiti NON se F16=Annullamento?
069300170403             if  Not $Cancellazione;
069400170403               exsr  sr_CtrD02;
069500170403             endif;
069600170403             if  $ErrGenerico;
069700170403               leavesr;
069800170403             endif;
069900170403             // -?Aggiornamento?
070000170403             If  dsp_aid = c_F06   or
070100170403                 dsp_aid = c_F16;
070200170403               exsr  sr_updFIMSE;
070300170403               // -?Rilevato errore in fase di aggiornamento?
070400170403               if  $ErrGenerico = *on;
070500170403                 leavesr;
070600170403               endif;
070700170403               // -?SubFile iniziale altrimenti?
070800170403               $Video  = 'S1';
070900170403               $InzS01 = *on;
071000170403             EndIf;
071100170403
071200170403         ENDSL;
071300170403
071400170403       ENDSR;
071500170403
071600170403       //--------------------------------------------------------------
071700170403       //?Inizializzazione videata D02.                                ?
071800170403       //--------------------------------------------------------------
071900170403       BEGSR  sr_InzD02;
072000170403
072100170403         // -?Spegnimento degli indicatori di errore?
072200170403         %subst( IndDspF : 50 ) = *off;
072300170403
072400170403         // -?Pulizia videata?
072500170403         clear  MS27D02;
072600170403
072700170403         // -?Reperimento dati in gestione?
072800170403         //  ?(SE NON Immissione)?
072900170403         If  Not $Immissione;
073000170403
073100170403           // -?Reperimento dati?
073200170403           if  $Protect  or  $Copia;
073300170404             chain(N)  S1Hnrr  FIMSE000;
073400170403           else;
073500170404             chain  S1Hnrr  FIMSE000;
073600170403           endif;
073700170403
073800170404           // -?Caricamento dati a video?
073900170404           if  %found( FIMSE00F );
074000170403             exsr  sr_RieD02;
074100170403           endif;
074200170404
074300170404         Else;
074400170404
074500170404           // -?Impostazione dati di default SE immissione?
074600170404           V2Cdti = *date;
074700170404           V2Cdtf = c_MaxDat_Eur;
074800170403
074900170403         EndiF;
075000170403
075100170403         // -?Impostazione testata?
075200170404         clear  V2Topz;
075300170403         Select;
075400170403           When  $Protect;
075500170404             V2Topz = 'VISUALIZZAZIONE';
075600170403           When  $Copia;
075700170404             V2Topz = '     COPIA     ';
075800170403           When  $Immissione;
075900170404             V2Topz = '  INSERIMENTO  ';
076000170404           //*·When  $Cancellazione;
076100170404           //*·  V2Topz = ' CANCELLAZIONE ';
076200170403           Other;
076300170404             V2Topz = '   MODIFICA    ';
076400170403         EndSl;
076500170403
076600170403         // -?Impostazione attributi di visualizzazione?
076700170403         $ProtectKEY = ( Not $Immissione  and  Not $Copia );
076800170403         $ProtectDTI = ( Not $Immissione  and  Not $Copia  and
076900170404                         MSEdti <= wDate );
077000170403
077100170403         // -?Impostazione indicatori per abilitazione tasti funzionali?
077200170403         exsr  sr_AbilitFxxD02;
077300170403
077400170403       ENDSR;
077500170403
077600170403       //--------------------------------------------------------------
077700170403       //?Caricamento dati del singolo record del file FIMSE00F        ?
077800170403       //?nella videata D02.                                           ?
077900170403       //--------------------------------------------------------------
078000170403       BEGSR  sr_RieD02;
078100170403
078200170403         if  Not $Copia;
078300170403           V2Cfil = MSEfil;
078400170403         endif;
078500170403
078600170403         V2Ctur   = MSEtur;
078700170403         V2Chmi   = MSEhmi;
078800170403         V2Chmf   = MSEhmf;
078900170403         V2Cvs1   = MSEvs1;
079000170403         V2Cvs2   = MSEvs2;
079100170403         V2Cvs3   = MSEvs3;
079200170403         V2Cvs4   = MSEvs4;
079300170403         V2Cvs5   = MSEvs5;
079400170403         V2Cvs6   = MSEvs6;
079500170403         V2Cvs7   = MSEvs7;
079600170404
079700170407         If  Not $Copia   or
079800170407             ( $Copia    and
079900170407               MSEdti >= wDate );
080000170405           reset  WLBdat;
080100170405           G08inv   = MSEdti;
080200170405           XSRDA8(WLBdat);
080300170405           if  G08err = *zero;
080400170405             V2Cdti = G08dat;
080500170405           endif;
080600170405         Else;
080700170405           V2Cdti = *Date;
080800170405         EndIf;
080900170407         If  Not $Copia   or
081000170407             ( $Copia    and
081100170407               MSEdtf > wDate );
081200170405           reset  WLBdat;
081300170405           G08inv   = MSEdtf;
081400170405           XSRDA8(WLBdat);
081500170405           if  G08err = *zero;
081600170405             V2Cdtf = G08dat;
081700170405           endif;
081800170405         Else;
081900170405           V2Cdtf = c_MaxDat_Eur;
082000170405         EndIf;
082100170403
082200170403         // -?Decodifica Filiale?
082300170403         clear  V2Dfil;
082400170403         if  V2Cfil > *zero;
082500170403           chain  ( V2Cfil )  AZORG;
082600170403           if  %found(AZORG01L);
082700170403             V2Dfil = ORGdes;
082800170403           else;
082900170403             V2Dfil = *all'? ';
083000170403           endif;
083100170403         endif;
083200170403
083300170403       ENDSR;
083400170403
083500170403       //--------------------------------------------------------------
083600170403       //?Settaggio indicatori nella videata D02 per abilitazione      ?
083700170403       //?  tasti funzionali.                                          ?
083800170403       //--------------------------------------------------------------
083900170403       BEGSR  sr_AbilitFxxD02;
084000170403
084100170403         // -?F3 = Fine?
084200170404         //*·$F3Attivo  = ( Not $Immissione );
084300170404         $F3Attivo  = *on;
084400170403
084500170403         // -?F6 = Conferma?
084600170403         $F6Attivo  = ( Not $Protect );
084700170403
084800170403         // -?F12 = Ritorno?
084900170403         $F12Attivo = *on;
085000170403
085100170403         // -?F16 = Cancellazione?
085200170403         $F16Attivo = ( Not $Immissione   and
085300170403                        Not $Copia        and
085400170404                        %found(FIMSE00F)  and
085500170403                        Not $Protect      and
085600170403                        W2Cdti > wDate );
085700170403
085800170403       ENDSR;
085900170403
086000170403       //--------------------------------------------------------------
086100170403       //?Controllo dati videata D02.                                  ?
086200170403       //--------------------------------------------------------------
086300170403       BEGSR  sr_CtrD02;
086400170403
086500170403         // -?Spegnimento degli indicatori di errore?
086600170403         %subst(IndDspF : 50) = *off;
086700170403
086800170403         // -?Controllo immissione codice Filiale?
086900170407         //*·clear  Og150;
087000170403         clear  V2Dfil;
087100170403         If  V2Cfil = *zero;
087200170403           VIDmsg = sk_Msg(03);
087300170403           $PosCurFIL   = *on;
087400170403           $ErrMessage  = *on;
087500170403           $ErrGenerico = *on;
087600170403           leavesr;
087700170403         EndIf;
087800170403
087900170403         // -?Verifica esistenza in AZORG / Decodifica Filiale?
088000170403         chain  ( V2Cfil )  AZORG;
088100170404         if  %found( AZORG01L );
088200170404           V2Dfil   = ORGdes;
088300170407           //*·Og150    = ORGdf0;
088400170404         endif;
088500170404
088600170404         if  NOT %found( AZORG01L )  or
088700170403             ORGfva <> *blank;
088800170403           VIDmsg = sk_Msg(04);
088900170403           $PosCurFIL   = *on;
089000170403           $ErrMessage  = *on;
089100170403           $ErrGenerico = *on;
089200170403           leavesr;
089300170403         endif;
089400170404
089500170407         //*·// -?Controllo abilitazione al Picking della filiale?
089600170407         //*·if  Og150.§ogPCK <> 'S';
089700170407         //*·  VIDmsg = %trimR( sk_Msg(04) ) + ': non è abilitata +
089800170407         //*·           al picking';
089900170407         //*·  $PosCurFIL   = *on;
090000170407         //*·  $ErrMessage  = *on;
090100170407         //*·  $ErrGenerico = *on;
090200170407         //*·  leavesr;
090300170407         //*·endif;
090400170403
090500170403         // -?Controllo Turno Picking?
090600170403         select;
090700170403           when  V2Ctur <> '1'   and
090800170403                 V2Ctur <> '2'   and
090900170407                 V2Ctur <> 'U'   and
091000170403                 V2Ctur <> 'M';
091100170403             VIDmsg = sk_Msg(05);
091200170403             $PosCurTUR   = *on;
091300170403             $ErrMessage  = *on;
091400170403             $ErrGenerico = *on;
091500170403             leavesr;
091600170407           //*·when  V2Ctur         = '2'   and
091700170407           //*·      Og150.§ogTurno = *blank;
091800170407           //*·  VIDmsg = %trimR( sk_Msg(05) ) + ': per questa filiale +
091900170407           //*·           NON è previsto il Turno Picking';
092000170407           //*·  $PosCurTUR   = *on;
092100170407           //*·  $ErrMessage  = *on;
092200170407           //*·  $ErrGenerico = *on;
092300170407           //*·  leavesr;
092400170407           //*·when  V2Ctur       = 'M'   and
092500170407           //*·      Og150.§ogPkM = *blank;
092600170407           //*·  VIDmsg = %trimR( sk_Msg(05) ) + ': per questa filiale +
092700170407           //*·           NON è previsto il Picking Messaggerie';
092800170407           //*·  $PosCurTUR   = *on;
092900170407           //*·  $ErrMessage  = *on;
093000170407           //*·  $ErrGenerico = *on;
093100170407           //*·  leavesr;
093200170403         endsl;
093300170403
093400170403         // -?Controllo Orari del Turno Picking?
093500170403         // ·?Orario iniziale?
093600170403         wTime_ds.wTime = V2Chmi * 100;
093700170403         if  wTime_ds.wHH < 00  or  wTime_ds.wHH > 23  or
093800170403             wTime_ds.wMM < 00  or  wTime_ds.wMM > 59  or
093900170403             wTime_ds.wSS < 00  or  wTime_ds.wSS > 59;
094000170403           VIDmsg = sk_Msg(06);
094100170403           $PosCurHMI   = *on;
094200170403           $ErrMessage  = *on;
094300170403           $ErrGenerico = *on;
094400170403           leavesr;
094500170403         endif;
094600170403         // ·?Orario finale?
094700170403         wTime_ds.wTime = V2Chmf * 100;
094800170403         if  wTime_ds.wHH < 00  or  wTime_ds.wHH > 23  or
094900170403             wTime_ds.wMM < 00  or  wTime_ds.wMM > 59  or
095000170403             wTime_ds.wSS < 00  or  wTime_ds.wSS > 59;
095100170403           VIDmsg = sk_Msg(07);
095200170403           $PosCurHMF   = *on;
095300170403           $ErrMessage  = *on;
095400170403           $ErrGenerico = *on;
095500170403           leavesr;
095600170403         endif;
095700170403         // ·?Range di orari?
095800170403         select;
095900170403           when  V2Chmi > V2Chmf;
096000170403             VIDmsg = sk_Msg(08);
096100170403             $PosCurHMI   = *on;
096200170403             $ErrMessage  = *on;
096300170403             $ErrGenerico = *on;
096400170403             leavesr;
096500170403           when  V2Chmi = V2Chmf;
096600170403             VIDmsg = sk_Msg(08);
096700170403             VIDmsg = %replace( '  uguale  ' : VIDmsg :
096800170403                         %scan( 'successivo' : sk_Msg(08) ) );
096900170403             $PosCurHMI   = *on;
097000170403             $ErrMessage  = *on;
097100170403             $ErrGenerico = *on;
097200170403             leavesr;
097300170403         endsl;
097400170403
097500170403         // -?Controllo Validità Settimanale?
097600170404         If  wV2Cvs_ds = *blank;
097700170404           VIDmsg = sk_Msg(09);
097800170404           $PosCurVS1   = *on;
097900170404           $ErrMessage  = *on;
098000170404           $ErrGenerico = *on;
098100170404           leavesr;
098200170404         EndIf;
098300170404
098400170403         For  xx = 1  To 7;
098500170403
098600170403           if  sk_V2Cvs(xx) <> *blank  and
098700170403               sk_V2Cvs(xx) <> 'X';
098800170404             VIDmsg = %trimR( sk_Msg(10) ) + ' ' + %trimR( sk_Gio(xx) )
098900170403                                           + ': ammessi " " o "X"';
099000170403             //*·$PosCurVSx   = *on;
099100170403             %subst( IndDspF : 54 + xx ) = *on;
099200170403             $ErrMessage  = *on;
099300170403             $ErrGenerico = *on;
099400170403             leavesr;
099500170403           endif;
099600170403
099700170403         EndFor;
099800170403
099900170403         // -?Controllo periodo validità?
100000170403         // ·?Data Decorrnza?
100100170403         clear  WLBdat;
100200170403         G08dat = V2Cdti;
100300170403         XSRDA8(WLBdat);
100400170404         if  G08err = *zero;
100500170403           V2Cdti = G08dat;
100600170403           W2Cdti = G08inv;
100700170403         else;
100800170404           VIDmsg = sk_Msg(11);
100900170403           $PosCurDTI   = *on;
101000170403           $ErrMessage  = *on;
101100170403           $ErrGenerico = *on;
101200170403           leavesr;
101300170403         endif;
101400170407         // ·?Data Scadenza?
101500170403         clear  WLBdat;
101600170403         G08dat = V2Cdtf;
101700170403         XSRDA8(WLBdat);
101800170404         if  G08err = *zero;
101900170403           V2Cdtf = G08dat;
102000170403           W2Cdtf = G08inv;
102100170403         else;
102200170404           VIDmsg = sk_Msg(12);
102300170403           $PosCurDTF   = *on;
102400170403           $ErrMessage  = *on;
102500170403           $ErrGenerico = *on;
102600170403           leavesr;
102700170403         endif;
102800170407         // ·?Range di date?
102900170403         if  W2Cdti > W2Cdtf;
103000170404           VIDmsg = sk_Msg(13);
103100170404           $PosCurDTF   = *on;
103200170403           $ErrMessage  = *on;
103300170403           $ErrGenerico = *on;
103400170403           leavesr;
103500170403         endif;
103600170407
103700170407         // -?La Data Decorrnza deve essere uguale o successiva a?
103800170407         //  ?quella odierna (perchè i dati di ieri potrebbero già?
103900170407         //  ?essere stati estratti)?
104000170407         if  ( $Copia         or
104100170407               $Immissione )  and
104200170407             W2Cdti <  wDate;
104300170407           VIDmsg = sk_Msg(14);
104400170407           $PosCurDTI   = *on;
104500170407           $ErrMessage  = *on;
104600170407           $ErrGenerico = *on;
104700170407           leavesr;
104800170407         endif;
104900170403
105000170404         // -?Verifica esistenza in FIMSE00F?
105100170403         //  ?(con sovrapposizione di giorni nei periodi o?
105200170403         //   ?con periodi precedenti l'ultimo inserito)?
105300170403         // ·?Esempi - Periodo esistente:?· · 3-----6 · ·?
105400170407         // ·?        Periodo inseribile: 1-2?
105500170403         // ·?    Periodo NON inseribile:   2---4?
105600170403         // ·?    Periodo NON inseribile:       4-5?
105700170403         // ·?    Periodo NON inseribile:         5---7?
105800170403         // ·?        Periodo inseribile:             7-8?
105900170403         If  $Immissione  or  $Copia;
106000170403           k_MSEfil = V2Cfil;
106100170403           k_MSEtur = V2Ctur;
106200170403           k_MSEdti = W2Cdtf;
106300170405           //*·setgt  %kds( keyFIMSE01 )  FIMSE001;
106400170405           //*·readPe  %kds( keyFIMSE01 : 2 )  FIMSE001;
106500170405           setll  %kds( keyFIMSE01 )  FIMSE001;
106600170405           reade  %kds( keyFIMSE01 : 2 )  FIMSE001;
106700170403           DoW  NOT %eof( FIMSE01L );
106800170403             if  MSEdtf >= W2Cdti;
106900170407               VIDmsg = sk_Msg(15);
107000170403               $PosCurDTI   = *on;
107100170403               $ErrMessage  = *on;
107200170403               $ErrGenerico = *on;
107300170403               leavesr;
107400170403             endif;
107500170405             //*·readPe  %kds( keyFIMSE01 : 2 )  FIMSE001;
107600170405             reade  %kds( keyFIMSE01 : 2 )  FIMSE001;
107700170403           EndDo;
107800170403         EndIf;
107900170403
108000170403       ENDSR;
108100170403
108200170403       //--------------------------------------------------------------
108300170403       //?Aggiornamento dati nel *file FIMSE00F.                       ?
108400170403       //--------------------------------------------------------------
108500170403       BEGSR  sr_UpdFIMSE;
108600170403
108700170403         // -?Aggancio record di FIMSE00F?
108800170404         if  Not $Immissione;
108900170404           chain  S1Hnrr  FIMSE000;
109000170404         endif;
109100170403
109200170403         // -?Impostazione dati?
109300170405         if  $Copia       or
109400170405             $Immissione  or
109500170405             ( NOT %found( FIMSE00F )  and
109600170405               NOT $Cancellazione );
109700170403           clear  FIMSE000;
109800170403           MSEfil = V2Cfil;
109900170403           MSEtur = V2Ctur;
110000170407           //*·MSEdti = W2Cdti;
110100170403         endif;
110200170403
110300170403         If  NOT $Cancellazione;
110400170407           MSEdti = W2Cdti;
110500170403           MSEdtf = W2Cdtf;
110600170403           MSEvs1 = V2Cvs1;
110700170403           MSEvs2 = V2Cvs2;
110800170403           MSEvs3 = V2Cvs3;
110900170403           MSEvs4 = V2Cvs4;
111000170403           MSEvs5 = V2Cvs5;
111100170403           MSEvs6 = V2Cvs6;
111200170403           MSEvs7 = V2Cvs7;
111300170403           MSEhmi = V2Chmi;
111400170403           MSEhmf = V2Chmf;
111500170403         EndIf;
111600170403
111700170403         // -?Aggiornamento record?
111800170403         SELECT;
111900170403
112000170403           // -?F16=Cancellazione?
112100170404           WHEN  $Cancellazione  and  %found( FIMSE00F );
112200170403             //_______________
112300170405             delete(E)  FIMSE000;
112400170403             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112500170403
112600170403           // -?Immissione o Copia o Modifica?
112700170403           OTHER;
112800170405             if  Not $Copia       and
112900170405                 Not $Immissione  and
113000170405                 %found( FIMSE00F );
113100170404               //_________________
113200170404               update(E) FIMSE000;
113300170404               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
113400170403             else;
113500170404               //_________________
113600170404               write(E)  FIMSE000;
113700170404               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
113800170403             endif;
113900170403
114000170403         ENDSL;
114100170404
114200170404         // -?Segnalazione errore rilevato in fase di?
114300170404         //  ?scrittura/aggiornamento/cancellazione?
114400170404         If  %error();
114500170407           VIDmsg = sk_Msg(16);
114600170404           $ErrMessage  = *on;
114700170404           $ErrGenerico = *on;
114800170405           // -?Stampa del Dump?
114900170405           Dump(A);
115000170405           // -?Stampa del Job-Log?
115100170405           Qcmd = 'DSPJOBLOG job(*) output(*print)';
115200170405           exsr  sr_ExecCmd;
115300170404           leavesr;
115400170404         EndIf;
115500170403
115600170403       ENDSR;
115700170405
115800170405       //--------------------------------------------------------------
115900170405       //?Esecuzione del comando (già impostato).                      ?
116000170405       //--------------------------------------------------------------
116100170405       BEGSR  sr_ExecCmd;
116200170405
116300170405         clear Qcap0100;
116400170405         Qcabcsdh = *off;
116500170405         Qcapa    = *off;
116600170405         Qcacmdss = *off;
116700170405         Qcaerved = *allX'00';
116800170405
116900170405         clear Qusec;
117000170405         Qusbprv  = %size(Qusec);
117100170405
117200170405         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
117300170405                           %size(Qcap0100) : 'CPOP0100' : *omit :
117400170405                           0 : 0 : Qusec);
117500170405
117600170405         //*·if  Qusei <> *blank;
117700170405         //*·  exsr  sr_PrintErr;
117800170405         //*·endif;
117900170405
118000170405       ENDSR;
118100170403
118200170403       //--------------------------------------------------------------
118300170403       //?Operazioni finali.                                           ?
118400170403       //--------------------------------------------------------------
118500170403       BEGSR  sr_RoutEnd;
118600170403
118700170403         //  ?Uscita?
118800170403         return;
118900170403
119000170403       ENDSR;
119100170403
119200170403      /end-free
119300170403
119400170403       //--------------------------------------------------------------
119500170403       //?Definizione schiere a tempo di compilazione.                 ?
119600170403       //--------------------------------------------------------------
119700170403
119800170403** --?sk_Gio:?Giorni della settimana?
119900170403Lunedì      1
120000170403Martedì     2
120100170403Mercoledì   3
120200170403Giovedì     4
120300170403Venerdì     5
120400170403Sabato      6
120500170403Domenica    7
120600170403** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
120700170403Raggiunto il limite massimo dei record caricabili a video                      1
120800170403Opzione errata                                                                 2
120900170403Immettere la Filiale                                                           3
121000170403Filiale errata                                                                 4
121100170403Turno picking errato                                                           5
121200170403Orario iniziale del turno picking errato                                       6
121300170403Orario finale del turno picking errato                                         7
121400170403Orario iniziale del turno picking  successivo  a quello finale                 8
121500170404Selezionare almeno un giorno per la validità settimanale                       9
121600170404Validità errata per il giorno                                                  10
121700170404Data decorrenza formalmente errata                                             11
121800170404Data scadenza formalmente errata                                               12
121900170404Data di scadenza precedente a quella di decorrenza                             13
122000170407La data decorrenza NON può essere precedente a quella odierna                  14
122100170407Il periodo si sovrappone a quello di un'altra eccezione già inserita           15
122200170407Rilevato errore in fase di registrazione dati - Avvisare CED                   16
