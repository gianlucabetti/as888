000100180220       //==============================================================
000200180220       //? Gestione tabella "ETF" (Eccezioni Terminal Filiale).         ?
000300180220       //==============================================================
000400180220
000500180220       //--------------------------------------------------------------
000600180220       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180220       //--------------------------------------------------------------
000800180220
000900180220     /*PRM dbgview(*source)
001000180220     /*END
001100180220
001200180220       //--------------------------------------------------------------
001300180220       //? Specifiche di controllo.                                     ?
001400180220       //--------------------------------------------------------------
001500180220
001600180220     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700180220
001800180220       //--------------------------------------------------------------
001900180220       //? Dichiarazione file.                                          ?
002000180220       //--------------------------------------------------------------
002100180220
002200180220       // - ?Organigramma?
002300180220     fAZORG01L  if   e           k disk
002400180220
002500180220       // - ?Tabella "ETF"?
002600180220     fTNTBE01L  Uf A e           k disk
002700180220
002800180220       // - ?Video?
002900180220     fTNTB47D   cf   e             workstn
003000180222     f                                     sfile( TB47S01 : S01nrr )
003100180222     f                                     sfile( TB47S02 : S02nrr )
003200180222     f                                     indds( IndDspF )
003300180222     f                                     infds( InfDspF )
003400180220
003500180220       //--------------------------------------------------------------
003600180220       //? Definizione costanti.                                        ?
003700180220       //--------------------------------------------------------------
003800180220
003900180220       // - ?Codice tabella in gestione?
004000180220     d c_Tab           c                   const('ETF')
004100180220
004200180220       // - ?Costante per controllo "caratteri solo numerici"?
004300180220     d c_Digits        c                   const('0123456789')
004400180220
004500180220       // - ?Tasti funzionali a video?
004600180220     d c_F01           c                   const(x'31')
004700180220     d c_F02           c                   const(x'32')
004800180220     d c_F03           c                   const(x'33')
004900180220     d c_F04           c                   const(x'34')
005000180220     d c_F05           c                   const(x'35')
005100180220     d c_F06           c                   const(x'36')
005200180220     d c_F07           c                   const(x'37')
005300180220     d c_F08           c                   const(x'38')
005400180220     d c_F09           c                   const(x'39')
005500180220     d c_F10           c                   const(x'3A')
005600180220     d c_F11           c                   const(x'3B')
005700180220     d c_F12           c                   const(x'3C')
005800180220     d c_F13           c                   const(x'B1')
005900180220     d c_F14           c                   const(x'B2')
006000180220     d c_F15           c                   const(x'B3')
006100180220     d c_F16           c                   const(x'B4')
006200180220     d c_F17           c                   const(x'B5')
006300180220     d c_F18           c                   const(x'B6')
006400180220     d c_F19           c                   const(x'B7')
006500180220     d c_F20           c                   const(x'B8')
006600180220     d c_F21           c                   const(x'B9')
006700180220     d c_F22           c                   const(x'BA')
006800180220     d c_F23           c                   const(x'BB')
006900180220     d c_F24           c                   const(x'BC')
007000180220     d c_Enter         c                   const(x'F1')
007100180220     d c_RollDown      c                   const(x'F4')
007200180220     d c_RollUp        c                   const(x'F5')
007300180220
007400180220       //--------------------------------------------------------------
007500180220       //? Definizione schiere.                                         ?
007600180220       //--------------------------------------------------------------
007700180220
007800180220       // - ?Messaggi di errore?
007900180222     d sk_Msg          s             78    dim( 6)  ctdata  perrcd( 1)
008000180220
008100180220       //--------------------------------------------------------------
008200180220       //? Definizione aree dati.                                       ?
008300180220       //--------------------------------------------------------------
008400180220
008500180220       // - ?Dati utente?
008600180220     d §AzUte        e ds                  extname(AZUTE00F)
008700180220     d                                     dtaara
008800180220     d §DatiUte      e ds                  extname(dDatiUte)
008900180220     d                                     dtaara
009000180220
009100180220       //--------------------------------------------------------------
009200180220       //? Definizione strutture dati.                                  ?
009300180220       //--------------------------------------------------------------
009400180220
009500180220       // - ?Status ds?
009600180220     d Status         sds
009700180220     d   SDSpgmName      *proc
009800180220
009900180220       // - ?InfDS?
010000180220     d InfDspF         ds
010100180220     d   dsp_aid             369    369a
010200180220     d   sfl_rrn             376    377i 0
010300180220     d   min_nrr             378    379i 0
010400180220     d   num_rcds            380    381i 0
010500180220
010600180220       // - ?Indicatori su DspF?
010700180220     d IndDspF         ds                  inz
010800180220         // - ?Abilitazione tasti funzionali?
010900180222     d   $F6Attivo                     n   overlay( IndDspF : 06 )
011000180222     d   $F10Attivo                    n   overlay( IndDspF : 10 )
011100180220         // - ?Emissione messaggio di errore?
011200180222     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011300180220         // - ?Indicatori di gestione del subfile?
011400180222     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
011500180222     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
011600180222     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
011700180222     d   $SflEnd                       n   overlay( IndDspF : 33 )
011800180222     d   $Sfl2Dsp_N                    n   overlay( IndDspF : 34 )
011900180220     d   $Sfl2DspCtl_N...
012000180222     d                                 n   overlay( IndDspF : 35 )
012100180222     d   $Sfl2NxtChg                   n   overlay( IndDspF : 36 )
012200180222     d   $Sfl2End                      n   overlay( IndDspF : 37 )
012300180220         // - ?Indicatori per Attibuti di visualizzazione?
012400180222     d   $RecAnnull                    n   overlay( IndDspF : 40 )
012500180222     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
012600180222     d   $ProtectETF                   n   overlay( IndDspF : 42 )
012700180220         // - ?Posizionamento cursore & segnalazione errore?
012800180222     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
012900180222     d   $PosCurLNA                    n   overlay( IndDspF : 51 )
013000180222     d   $PosCurETF                    n   overlay( IndDspF : 52 )
013100180220         // - ?Riemissione videata?
013200180222     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013300180220
013400180220       // - ?Numero MAX di Eccezioni (come Terminal di Arrivo) gestibili?
013500180220     d c_MaxETF        c                   const(05)
013600180220       // - ?Terminal di Arrivo (eccezioni) in tab. "ETF"?
013700180220     d ds_ETF          ds            15    inz
013800180222     d   S1Cetf1                           inz
013900180222     d   S1Cetf2                           inz
014000180222     d   S1Cetf3                           inz
014100180222     d   S1Cetf4                           inz
014200180222     d   S1Cetf5                           inz
014300180220     d   sk_ETF                1     15    inz  dim(c_MaxETF)
014400180222     d Save_ETF        s                   like(ds_ETF)    inz
014500180220       // - ?Descrizioni Terminal di Arrivo (eccezioni) in tab. "ETF"?
014600180222     d ds_ETFdes       ds            50    inz
014700180222     d   S1Detf1                           inz
014800180222     d   S1Detf2                           inz
014900180222     d   S1Detf3                           inz
015000180222     d   S1Detf4                           inz
015100180222     d   S1Detf5                           inz
015200180222     d   sk_ETFd               1     50    inz  dim(c_MaxETF)
015300180222     d Save_ETFdes     s                   like(ds_ETFdes) inz
015400180220
015500180220       // - ?Parametri ricevuti?
015600180220     d KPJBA         e ds
015700180220
015800180220       // - ?Dati record principale della tabella?
015900180222     d TNTBEds       e ds                  inz  extname( TNTBE00F )
016000180222     d xTNTBEds      e ds                  inz  extname( TNTBE00F )
016100180222     d                                          prefix( TBX : 3 )
016200180220
016300180220       //--------------------------------------------------------------
016400180220       //? Definizione variabili globali.                               ?
016500180220       //--------------------------------------------------------------
016600180220
016700180220       // - ?Flags booleani?
016800180220     d $Fine           s               n   inz
016900180220     d $EoF            s               n   inz
017000180220     d $Immissione     s               n   inz
017100180220     d $InzS01         s               n   inz(*on)
017200180220     d $InzS02         s               n   inz(*on)
017300180220     d $InzW01         s               n   inz(*on)
017400180220
017500180220       // - ?Indici di schiera?
017600180220     d xx              s              3  0 inz
017700180220
017800180220       // - ?Variabili per la gestione del video?
017900180220     d $Video          s              2    inz('S1')
018000180222     d S01nrr          s                   like(C1RcdNbr)  inz
018100180220     d S02nrr          s                   like(C2RcdNbr)  inz
018200180220     d SavS01csr       s                   like(C1RcdNbr)  inz
018300180220     d SavS02csr       s                   like(C2RcdNbr)  inz
018400180220
018500180220       // - ?Variabili di comodo?
018600180220     d wData_EUR       s               d   datfmt(*eur)    inz(*loval)
018700180220
018800180220       //--------------------------------------------------------------
018900180220       //? Definizione prototipi procedure.                             ?
019000180220       //--------------------------------------------------------------
019100180220
019200180220       // - ?Reperimento dati utente?
019300180220     d TIBS34ds      e ds                  inz
019400180220      /copy gaitrasrc/srcProtoPR,TIBS34R
019500180220
019600180220       // - ?Caricamento schiera filiali?
019700180220     d TRUL06ds      e ds                  inz
019800180220     d   D06cod      e                     inz('£1')
019900180220     d   D06tla      e                     inz('L')
020000180220     d   D06lin                1     90  0 inz   dim(30)
020100180220      /copy gaitrasrc/srcProtoPr,TRUL06R
020200180220
020300180220       // - ?Ricerca codici Organigramma?
020400180220     d SD19cod         s              3    inz
020500180220     d SD19tip         s              1    inz
020600180220     d SD19des         s             25    inz
020700180220     d tnsd19r         pr                  extpgm('TNSD19R')
020800180220     d   D19cod                            like(SD19cod)
020900180220     d   D19tip                            like(SD19tip)
021000180220     d   D19des                            like(SD19des)
021100180220
021200180220       // - ?API QCAPCMD (Process Commands)?
021300180220     d Qcmd            s           2048    inz  varying
021400180220      /copy qSysInc/qRpgleSrc,QCAPCMD
021500180220      /copy gaitrasrc/srcProtoPR,QCAPCMD
021600180220
021700180220       // - ?Parametri gestione errori API.?
021800180220      /copy qsysinc/qrpglesrc,QUSEC
021900180220
022000180220       //--------------------------------------------------------------
022100180220       //? Definizione key-list.                                        ?
022200180220       //--------------------------------------------------------------
022300180220
022400180220       // - ?File TNTBE01L?
022500180220     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
022600180220     d                                     prefix( k_ )    inz
022700180220
022800180220       // - ?File AZORG01L?
022900180220     d keyAZORG01    e ds                  extname( AZORG01L : *key )
023000180220     d                                     prefix( k_ )    inz
023100180220
023200180220       //--------------------------------------------------------------
023300180220       //? Riepilogo indicatori utilizzati.                             ?
023400180220       //--------------------------------------------------------------
023500180220
023600180220
023700180220       //--------------------------------------------------------------
023800180220       //? M A I N - L I N E                                            ?
023900180220       //--------------------------------------------------------------
024000180220
024100180220     c     *Entry        plist
024200180220     c                   parm                    KPJBA
024300180220
024400180220      /free
024500180220
024600180220       // - ?Operazioni iniziali?
024700180220       exsr  sr_RoutInz;
024800180220
024900180220       // - ?Ciclo di gestione del file video?
025000180220       DoW  $Fine = *off;
025100180220
025200180220         select;
025300180220
025400180220           // - ?Gestione videata S1 (subfile)?
025500180220           when  $Video = 'S1';
025600180220             exsr  sr_GesS01;
025700180220
025800180220           // - ?Gestione videata S2 (subfile)?
025900180220           when  $Video = 'S2';
026000180220             exsr  sr_GesS02;
026100180220
026200180220           // - ?Richiesta trasmissione dati?
026300180220           when  $Video = 'W1';
026400180220             exsr  sr_GesW01;
026500180220
026600180220           // - ?? ? ??
026700180220           other;
026800180220             $Fine = *on;
026900180220
027000180220         endsl;
027100180220
027200180220       EndDo;
027300180220
027400180220       // - ?Operazioni finali?
027500180220       exsr  sr_RoutEnd;
027600180220
027700180220       //--------------------------------------------------------------
027800180220       //? Operazioni iniziali.                                         ?
027900180220       //--------------------------------------------------------------
028000180220       BEGSR  sr_RoutInz;
028100180220
028200180220         // - ?Impostazione chiusura?
028300180220         *inLR = *on;
028400180222
028500180222         // - ?Impostazione nome programma a video?
028600180222         V1Tpgm = SDSpgmName;
028700180220
028800180220         // - ?Reperimento dati job?
028900180220         exsr  sr_DatiJob;
029000180220
029100180220         // - ?Aggancio dati generali della tabella in esame?
029200180220         clear  keyTNTBE01;
029300180220         k_TBEke1 = *zero;
029400180220         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
029500180220                = c_Tab;
029600180220         clear  k_TBEke2;
029700180220         clear  k_TBElin;
029800180220         k_TBEsif = KNSIF;
029900180220         chain  %kds( keyTNTBE01 )  TNTBE000;
030000180220         if  not %found(TNTBE01L);
030100180220           clear  k_TBEsif;
030200180220           chain  %kds( keyTNTBE01 )  TNTBE000;
030300180220         endif;
030400180220         if  %found(TNTBE01L);
030500180220           xTNTBEds = TNTBEds;
030600180220         else;
030700180220           clear  xTNTBEds;
030800180220         endif;
030900180220
031000180220         // - ?Pulizia / impostazione iniziale dei campi chiave?
031100180220         clear  keyTNTBE01;
031200180220         k_TBEcod = c_Tab;
031300180220
031400180220       ENDSR;
031500180220
031600180220       //--------------------------------------------------------------
031700180220       //? Reperimento Dati del job (Utente/Operativi).                 ?
031800180220       //--------------------------------------------------------------
031900180220       BEGSR  sr_DatiJob;
032000180220
032100180220         in(e) §AzUte;
032200180220         if NOT %error;
032300180220           in(e) §DatiUte;
032400180220         endif;
032500180220         if %error or RSut = *blank;
032600180220           tibs34r ( tibs34ds );
032700180220           in §AzUte;
032800180220           in §DatiUte;
032900180220         endif;
033000180220
033100180220       ENDSR;
033200180220
033300180220       //--------------------------------------------------------------
033400180220       //? Gestione subfile S01.                                        ?
033500180220       //--------------------------------------------------------------
033600180220       BEGSR  sr_GesS01;
033700180220
033800180220         // - ?Inizializzazione videata?
033900180220         if  $InzS01 = *on;
034000180220           exsr  sr_InzS01;
034100180220           $InzS01 = *off;
034200180220         endif;
034300180220
034400180220         // - ?Emissione Testata & Piede?
034500180222         write  TB47T01;
034600180222         write  TB47P01;
034700180220
034800180220         // - ?Posizionamento cursore?
034900180220         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
035000180220         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
035100180220         //   ?si visualizza la pagina che vedeva l'utente quando ha?
035200180220         //   ?premuto l'ultimo tasto.?
035300180220         if  C1CsrRrn > *zero;
035400180220           C1RcdNbr = C1CsrRrn;
035500180220         else;
035600180220           C1RcdNbr = 1;
035700180222           write  TB47S00;          // ?(no rec.)?
035800180220         endif;
035900180220
036000180220         // - ?Emissione videata?
036100180222         exfmt  TB47C01;
036200180220
036300180220         reset  $ErrGenerico;
036400180220         reset  $ErrMessage;
036500180220         clear  VIDmsg;
036600180220
036700180220         Select;
036800180220
036900180220           // - ?F3=Fine?
037000180220           When  dsp_aid = c_F03;
037100180220             $Fine = *on;
037200180220
037300180220           // - ?F5=Rivisualizza?
037400180220           When  dsp_aid = c_F05;
037500180220             $InzS01 = *on;
037600180220
037700180220           // - ?F10=Immissione?
037800180220           When  dsp_aid = c_F10;
037900180220             $Video  = 'S2';
038000180220             $InzS02 = *on;
038100180222             //*·DoW  $Video = 'S2'  and  Not $Fine;
038200180222             //*·  exsr  sr_GesS02;
038300180222             //*·EndDo;
038400180220
038500180220           // - ?SubFile vuoto?
038600180220           When  S01nrr = *zero;
038700180220
038800180220           // - ?Invio?
038900180220           Other;
039000180220             // - ?Gestione opzioni?
039100180220             exsr  sr_OpzS01;
039200180220             if  $ErrGenerico;
039300180220               leavesr;
039400180220             endif;
039500180220
039600180222         EndSl;
039700180220
039800180220       ENDSR;
039900180220
040000180220       //--------------------------------------------------------------
040100180220       //? Inizializzazione subfile S01.                                ?
040200180220       //--------------------------------------------------------------
040300180220       BEGSR  sr_InzS01;
040400180220
040500180220         // - ?Spegnimento degli indicatori di errore?
040600180220         %subst( IndDspF : 50 ) = *off;
040700180222
040800180222         // - ?Impostazioni opzioni?
040900180222         C1Dopz = '2=Modifica, +
041000180222                   4=Annullamento/Ripristino, +
041100180222                   5=Visualizzazione.';
041200180222
041300180222         // - ?Abilitazione tasti funzionali condizionati?
041400180222         $F10Attivo = *on;
041500180220
041600180220         // - ?Pulizia subfile?
041700180220         $SflDsp_N    = *on;
041800180220         $SflDspCtl_N = *on;
041900180222         write  TB47C01;
042000180220         $SflDspCtl_N = *off;
042100180220         $SflEnd      = *off;
042200180220
042300180220         clear  C1RcdNbr;
042400180220         clear  C1CsrRrn;
042500180220         clear  S01nrr;
042600180222         clear  VIDmsg;
042700180220         $ErrMessage  = *off;
042800180220         $ErrGenerico = *off;
042900180220
043000180220         // - ?Caricamento completo dei dati nel subfile?
043100180222         //   ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
043200180222         //   ?l'eventuale ordinamento)?
043300180222         $SflNxtChg = *off;
043400180220         setll  %kds( keyTNTBE01 )  TNTBE000;
043500180220         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
043600180220         $EoF = ( %eof(TNTBE01L) );
043700180222         // - ?Ciclo di caricamento dati da tab. "ETF"?
043800180222         DoW  Not $EoF;
043900180222           if  TBEke2 = *blank;
044000180222             exsr  sr_CaricaS01;
044100180222           endif;
044200180222           // - ?Lettura record successivo?
044300180222           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
044400180222           $EoF = ( %eof(TNTBE01L) );
044500180222         EndDo;
044600180222
044700180222         // - ?Visualizzazione del SFL (se ci sono dati)?
044800180222         $SflDsp_N = ( S01nrr <= *zero );
044900180222
045000180222         // - ?Attivazione del SFLEND?
045100180222         $SflEnd   = ( $EoF );
045200180222
045300180222         // - ?Impaginazione subfile?
045400180222         //   ?(posizionamento cursore al 1° rcd della 1ª pagina)?
045500180222         if  S01nrr > *zero;
045600180222           C1RcdNbr = 1;
045700180222         else;
045800180222           clear  C1RcdNbr;
045900180222         endif;
046000180222         C1CsrRrn = C1RcdNbr;
046100180220
046200180220       ENDSR;
046300180220
046400180220       //--------------------------------------------------------------
046500180222       //? Caricamento singolo rec. nel subfile S01.                    ?
046600180220       //--------------------------------------------------------------
046700180220       BEGSR  sr_CaricaS01;
046800180220
046900180222         // · ?Impostazione campi?
047000180222         clear  TB47S01;
047100180222         S1TBEke1 = TBEke1;
047200180222         H1TBEke2 = TBEke2;
047300180222         if  TBEatb  = 'A';
047400180222            S1TBEann = 'Ann';
047500180222         endif;
047600180222
047700180222         // · ?Eccezioni Terminal Filiale?
047800180222         ds_ETF = TBEuni;
047900180222
048000180222         // · ?Decodifica Linea di Arrivo?
048100180222         If  %check( c_Digits : S1tbeKE1 ) = *zero;
048200180222           k_ORGfil = %int( S1tbeKE1 );
048300180222           chain  %kds(keyAZORG01)  AZORG;
048400180222           if  %found(AZORG01L);
048500180222             S1Dlna = ORGdes;
048600180222           endif;
048700180222         EndIf;
048800180222
048900180222         // · ?Decodifica Eccezioni Terminal Filiale?
049000180222         clear  ds_ETFdes;
049100180222         For  xx = 1  To  %elem( sk_ETF );
049200180222
049300180222           If  %check( c_Digits : sk_ETF(xx) ) = *zero  and
049400180222               sk_ETF(xx)                      > *zero;
049500180222
049600180222             k_ORGfil = %int( sk_ETF(xx) );
049700180222             chain  %kds(keyAZORG01)  AZORG;
049800180222             if  %found(AZORG01L);
049900180222               if  ORGde5 <> *blank;
050000180222                 sk_ETFd(xx) = ORGde5;
050100180222               else;
050200180222                 sk_ETFd(xx) = ORGdes;
050300180222               endif;
050400180222             endif;
050500180222
050600180222           EndIf;
050700180222
050800180222         EndFor;
050900180222
051000180222         // · ?Scrittura record nel subfile?
051100180222         S01nrr += 1;
051200180222         write  TB47S01;
051300180220
051400180220       ENDSR;
051500180220
051600180220       //--------------------------------------------------------------
051700180220       //? Caricamento completo del subfile S01.                        ?
051800180220       //--------------------------------------------------------------
051900180220       BEGSR  sr_OpzS01;
052000180220
052100180220         clear  SavS01csr;
052200180220
052300180220         // - ?Ciclo di lettura subfile?
052400180222         readc  TB47S01;
052500180220
052600180222         DoW  Not %eof( TNTB47D );
052700180220
052800180222           $SflNxtChg = *off;
052900180220           %subst(IndDspF : 50) = *off;
053000180220           C1RcdNbr = S01nrr;
053100180220
053200180220           select;
053300180220
053400180220             // - ?Opz. 2=Modifica?
053500180220             //   ?Opz. 4=Annullamento/Ripristino?
053600180220             //   ?Opz. 5=Visualizzazione?
053700180220             when  S1Copz = 2  or   S1Copz = 4  or   S1Copz = 5;
053800180220               If  S1Copz = 2  and  S1TBEann <> *blank;
053900180220                 $ErrGenerico = *on;
054000180220                 $ErrMessage  = *on;
054100180220                 $PosCurOPZ   = *on;
054200180222                 VIDmsg = sk_Msg(02);
054300180220                 $Fine  = *off;
054400180220               Else;
054500180220                 $Video  = 'S2';
054600180220                 $InzS02 = *on;
054700180220                 DoW  $Video = 'S2'  and  Not $Fine;
054800180220                   exsr  sr_GesS02;
054900180220                 EndDo;
055000180220               EndIf;
055100180220
055200180220             // -?Nessuna opzione?
055300180220             when  S1Copz = *zero;
055400180220
055500180220             // -?Opzione errata?
055600180220             other;
055700180220               $ErrGenerico = *on;
055800180220               $ErrMessage  = *on;
055900180220               $PosCurOPZ   = *on;
056000180222               VIDmsg = sk_Msg(01);
056100180220               $Fine  = *off;
056200180220
056300180220           endsl;
056400180220
056500180220           // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
056600180220           //   ?riposizionarvici il cursore dopo il tasto "Invio"?
056700180220           if  S1Copz  <> *zero    and
056800180220              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
056900180220             SavS01csr   = C1RcdNbr;
057000180220           endif;
057100180220
057200180220           // - ?Aggiornamento sfl?
057300180220           select;
057400180220             when  $ErrMessage;
057500180222               $SflNxtChg = *on;
057600180220               C1CsrRrn  = C1RcdNbr;
057700180220             when  $ErrGenerico;
057800180220               C1CsrRrn  = C1RcdNbr;
057900180220               clear  S1Copz;
058000180220             other;
058100180220               clear  S1Copz;
058200180220           endsl;
058300180220
058400180222           UPDATE  TB47S01;
058500180220
058600180220           if  $ErrGenerico  or  $ErrMessage;
058700180220             leave;
058800180220           endif;
058900180220
059000180222           readc  TB47S01;
059100180220
059200180220         ENDDO;
059300180220
059400180220         // - ?Riposizionam. cursore sul record contenente la prima opz.?
059500180220         //   ?(se non sono stati rilevati errori)?
059600180220         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
059700180220             SavS01csr > *zero;
059800180220           C1CsrRrn = SavS01csr;
059900180220         endif;
060000180220
060100180220       ENDSR;
060200180220
060300180220       //--------------------------------------------------------------
060400180220       //? Gestione subfile S02.                                        ?
060500180220       //--------------------------------------------------------------
060600180220       BEGSR  sr_GesS02;
060700180220
060800180220         // - ?Inizializzazione videata?
060900180220         if  $InzS02 = *on;
061000180220           exsr  sr_InzS02;
061100180220           $InzS02 = *off;
061200180220         endif;
061300180220
061400180220         // - ?Emissione Testata & Piede?
061500180222         write  TB47T01;
061600180222         write  TB47P02;
061700180220
061800180220         // - ?Posizionamento cursore?
061900180220         //   ?C2CsrRrn contiene il numero di riga del subfile su cui?
062000180220         //   ?era posizionato il cursore; impostandolo in C2RcdNbr?
062100180220         //   ?si visualizza la pagina che vedeva l'utente quando ha?
062200180220         //   ?premuto l'ultimo tasto.?
062300180220         if  C2CsrRrn > *zero;
062400180220           C2RcdNbr = C2CsrRrn;
062500180220         endif;
062600180220
062700180220         // - ?Emissione videata?
062800180220         if  Not $Immissione  AND
062900180220            (S1Copz = 5       or
063000180220            (S1Copz = 4  and  TBEatb = *blank));
063100180222           write  TB47C02;
063200180220           exfmt  Protect;
063300180220         else;
063400180222           exfmt  TB47C02;
063500180220         endif;
063600180220
063700180220         reset  $ErrGenerico;
063800180220         reset  $ErrMessage;
063900180220         clear  VIDmsg;
064000180220
064100180220         Select;
064200180220
064300180220           // - ?F3=Fine?
064400180220           When  dsp_aid = c_F03;
064500180220             unlock  TNTBE01L;
064600180220             $Video = 'S1';
064700180220             $ErrGenerico = *on;
064800180222             ds_ETF    = Save_ETF;
064900180222             ds_ETFdes = Save_ETFdes;
065000180220
065100180220           // - ?F12=Ritorno?
065200180220           When  dsp_aid = c_F12;
065300180220             unlock  TNTBE01L;
065400180220             $Video = 'S1';
065500180222             ds_ETF    = Save_ETF;
065600180222             ds_ETFdes = Save_ETFdes;
065700180220
065800180220           // - ?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
065900180220           Other;
066000180220             // - ?Controlli eseguiti NON se F16=Annullamento?
066100180220             if  dsp_aid <> c_F16;
066200180220               exsr  sr_CtrC02;
066300180220             endif;
066400180220             if  $ErrGenerico;
066500180220               leavesr;
066600180220             endif;
066700180220             // - ?Aggiornamento?
066800180220             if  dsp_aid = c_F06;
066900180220               $Video  = 'W1';
067000180220               $InzW01 = *on;
067100180220               DoW  $Video = 'W1';
067200180220                 exsr  sr_GesW01;
067300180220               EndDo;
067400180220             endif;
067500180220
067600180220         EndSl;
067700180220
067800180220       ENDSR;
067900180220
068000180220       //--------------------------------------------------------------
068100180220       //? Inizializzazione subfile S02.                                ?
068200180220       //--------------------------------------------------------------
068300180220       BEGSR  sr_InzS02;
068400180220
068500180220         // - ?Spegnimento degli indicatori di errore?
068600180220         %subst( IndDspF : 50 ) = *off;
068700180220
068800180220         // - ?Pulizia subfile-control?
068900180222         clear  TB47C02;
069000180220
069100180220         // - ?Pulizia subfile?
069200180220         $Sfl2Dsp_N    = *on;
069300180220         $Sfl2DspCtl_N = *on;
069400180222         write  TB47C02;
069500180220         $Sfl2DspCtl_N = *off;
069600180220         $Sfl2End      = *off;
069700180220
069800180220         clear  C2RcdNbr;
069900180220         clear  C2CsrRrn;
070000180220         clear  S02nrr;
070100180220         clear  VIDmsg;
070200180220         $ErrMessage  = *off;
070300180220         $ErrGenerico = *off;
070400180220
070500180220         // - ?Caricamento dei dati nel subfile-control?
070600180220         select;
070700180220            when  dsp_aid = c_F10;
070800180220              D2DesOpz = '  IMMISSIONE   ';
070900180220            when  S1Copz  = 2;
071000180220              D2DesOpz = '   MODIFICA    ';
071100180220            when  S1Copz  = 4  and  S1TBEann = *blank;
071200180220              D2DesOpz = ' ANNULLAMENTO  ';
071300180220            when  S1Copz  = 4  and  S1TBEann <> *blank;
071400180220              D2DesOpz = '  RIPRISTINO   ';
071500180220            when  S1Copz  = 5;
071600180220              D2DesOpz = 'VISUALIZZAZIONE';
071700180220         endsl;
071800180220
071900180220         // - ?Protezione campi chiave SE NON inserimento?
072000180220         $Immissione = (dsp_aid = c_F10);
072100180222         $ProtectKEY = (dsp_aid <> c_F10);
072200180222
072300180222         // - ?Salvataggio dei dati già inseriti?
072400180222         //   ?(da rivisualizzare nel subfile per F12)?
072500180222         if  $Immissione;
072600180222           clear  Save_ETF;
072700180222           clear  Save_ETFdes;
072800180222         else;
072900180222           Save_ETF    = ds_ETF;
073000180222           Save_ETFdes = ds_ETFdes;
073100180222         endif;
073200180220
073300180220         // - ?Reperimento dati dal file?
073400180222         clear  ds_ETF;
073500180222         If  dsp_aid <> c_F10;
073600180222
073700180222           clear  keyTNTBE01;
073800180220           k_TBEcod = c_Tab;
073900180220           k_TBEke1 = S1TBEke1;
074000180222           k_TBEke2 = H1TBEke2;
074100180222           chain  %kds( keyTNTBE01 : 3 )  TNTBE000;
074200180220           if  %found(TNTBE01L);
074300180222             ds_ETF = TBEuni;
074400180222             $RecAnnull = ( TBEatb <> *blank );
074500180220           endif;
074600180222
074700180222         EndIf;
074800180220
074900180222         // - ?Impostazione campi chiave  e?
075000180222         //   ?Caricamento delle Eccezioni Terminal Filiale (di Transito)?
075100180222         //   ?(SE NON immissione)?
075200180220         If  dsp_aid <> c_F10;
075300180220
075400180222           // · ?Linea di Arrivo?
075500180222           C02lna = %int( S1TBEke1 );
075600180222           k_ORGfil = C02lna;
075700180222           chain  %kds( keyAZORG01 )  AZORG;
075800180222           if  %found( AZORG01L );
075900180222             C02lnaD = ORGdes;
076000180220           endif;
076100180220
076200180220         EndIf;
076300180220
076400180222         // · ?Decodifica Eccezioni Terminal Filiale?
076500180222         For  xx = 1  To  %elem( sk_ETF );
076600180220
076700180222           clear  TB47S02;
076800180220
076900180222           If  %check( c_Digits : sk_ETF(xx) ) = *zero  and
077000180222               sk_ETF(xx)                      > *zero;
077100180220
077200180222             $Sfl2NxtChg = *on;
077300180222             S02etf = sk_ETF(xx);
077400180222
077500180222             k_ORGfil = %int( S02etf );
077600180222             chain  %kds( keyAZORG01 )  AZORG;
077700180220             if  %found(AZORG01L);
077800180222               //*·if  ORGde5 <> *blank;
077900180222               //*·  S02etfD = ORGde5;
078000180222               //*·else;
078100180222                 S02etfD = ORGdes;
078200180222               //*·endif;
078300180220             endif;
078400180220
078500180222           EndIf;
078600180220
078700180222           // · ?Scrittura record nel subfile?
078800180220           S02nrr += 1;
078900180222           write  TB47S02;
079000180220
079100180220         EndFor;
079200180220
079300180220         // -?Posizionamento cursore sulla Filiale di Ritiro?
079400180220         //  ?SE immissione?
079500180222         $PosCurLNA = ( dsp_aid = c_F10 );
079600180220
079700180220         // -?Visualizzazione del SFL (se ci sono dati)?
079800180222         $Sfl2Dsp_N = ( S02nrr <= *zero );
079900180220
080000180220         // -?Attivazione del SFLEND?
080100180222         $Sfl2End = *on;
080200180220
080300180220         // -?Impaginazione subfile?
080400180220         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
080500180222         if  S02nrr > *zero;
080600180220           C2RcdNbr  = 1;
080700180220           C2CsrRrn  = 1;
080800180220         else;
080900180220           clear C2RcdNbr;
081000180220         endif;
081100180220
081200180220         // -?Abilitazione tasti funzionali condizionati?
081300180222         $F6Attivo  = ( $Immissione  or  S1Copz <> 5 );
081400180220
081500180220       ENDSR;
081600180220
081700180220       //--------------------------------------------------------------
081800180222       //? Controllo dati nella videata C02/S02.                        ?
081900180220       //--------------------------------------------------------------
082000180220       BEGSR  sr_CtrC02;
082100180220
082200180222         // - ?Spegnimento degli indicatori di errore?
082300180222         %subst( IndDspF : 50 ) = *off;
082400180220
082500180222         // - ?Controllo Linea di Arrivo?
082600180222         // · ?Obbligatoria?
082700180222         if  C02lna = *zero;
082800180222           $ErrGenerico = *on;
082900180222           $ErrMessage  = *on;
083000180222           $PosCurLNA   = *on;
083100180222           VIDmsg = sk_Msg(03);
083200180220           leavesr;
083300180220         endif;
083400180222         // · ?Errata?
083500180222         k_ORGfil = C02lna;
083600180222         chain  %kds( keyAZORG01)  AZORG;
083700180222         if  Not %found( AZORG01L )  or  ( ORGfag <> 'F'  and
083800180222                                           ORGfag <> 'A' );
083900180222           $ErrGenerico = *on;
084000180222           $ErrMessage  = *on;
084100180222           $PosCurLNA   = *on;
084200180222           VIDmsg = sk_Msg(03);
084300180220           leavesr;
084400180220         endif;
084500180222
084600180222         C02lnaD = ORGdes;
084700180220
084800180222         // - ?Controllo SE chiave duplicata (SE immissione)?
084900180220         If  $Immissione;
085000180222           clear  keyTNTBE01;
085100180220           k_TBEcod = c_Tab;
085200180222           k_TBEke1 = %editc( C02lna : 'X' );
085300180222           //*·k_TBEke2 = *blank;
085400180222           setll  %kds( keyTNTBE01 )  TNTBE000;
085500180222           if  %equal( TNTBE01L );
085600180222             $ErrGenerico = *on;
085700180222             $ErrMessage  = *on;
085800180222             $PosCurLNA   = *on;
085900180222             VIDmsg = sk_Msg(04);
086000180220             leavesr;
086100180220           endif;
086200180220         EndIf;
086300180220
086400180222         // - ?Controllo Eccezioni Terminal Filiale (Transito Terminal)?
086500180222         clear  sk_ETF;
086600180222         clear  sk_ETFd;
086700180222         readc  TB47S02;
086800180220
086900180222         DoW  Not %eof( TNTB47D );
087000180220
087100180220           exsr  sr_CtrS02;
087200180220
087300180222           // - ?Aggiornamento sfl?
087400180220           select;
087500180222             when  $ErrMessage;
087600180222               $Sfl2NxtChg = *on;
087700180222               C2CsrRrn = C2RcdNbr;
087800180222             when  $ErrGenerico;
087900180222               C2CsrRrn = C2RcdNbr;
088000180220           endsl;
088100180222           $Sfl2NxtChg = *on;
088200180222           UPDATE  TB47S02;
088300180220
088400180222           if  $ErrGenerico   or  $ErrMessage;
088500180220             leave;
088600180220           endif;
088700180220
088800180222           readc  TB47S02;
088900180220
089000180220         EndDo;
089100180220
089200180220       ENDSR;
089300180220
089400180220       //--------------------------------------------------------------
089500180222       //? Controllo dati nel subfile S02.                              ?
089600180220       //--------------------------------------------------------------
089700180220       BEGSR  sr_CtrS02;
089800180220
089900180222         clear  S02etfD;
090000180220
090100180222         If  S02etf <> *blank  and  S02etf <> *zero;
090200180220
090300180222           // - ?Eccezione Terminal Filiale NON numerica?
090400180222           if  %check( c_Digits : S02etf ) > *zero;
090500180222             $ErrGenerico = *on;
090600180222             $ErrMessage  = *on;
090700180222             $PosCurETF   = *on;
090800180222             VIDmsg = sk_Msg(05);
090900180220             leavesr;
091000180220           endif;
091100180220
091200180222           k_ORGfil = %int( S02etf );
091300180222           chain  %kds( keyAZORG01 )  AZORG;
091400180222           // - ?Eccezione Terminal Filiale errata?
091500180222           if  Not %found( AZORG01L )  or  ( ORGfag <> 'F'  and
091600180222                                             ORGfag <> 'A' );
091700180222             $ErrGenerico = *on;
091800180222             $ErrMessage  = *on;
091900180222             $PosCurETF   = *on;
092000180222             VIDmsg = sk_Msg(05);
092100180220             leavesr;
092200180220           endif;
092300180220
092400180222           //*·if  ORGde5 <> *blank;
092500180222           //*·  S02etfD = ORGde5;
092600180222           //*·else;
092700180222             S02etfD = ORGdes;
092800180222           //*·endif;
092900180220
093000180220         EndIf;
093100180220
093200180222         if  S02etf = *zero;
093300180222           clear  S02etf;
093400180220         endif;
093500180220
093600180222         If  S02etf <> *blank;
093700180220
093800180222           // - ?Eccezione Terminal Filiale RIPETUTA nel subfile?
093900180222           if  %lookup( S02etf : sk_ETF ) > *zero;
094000180222             $ErrGenerico = *on;
094100180222             $ErrMessage  = *on;
094200180222             $PosCurETF   = *on;
094300180222             VIDmsg = sk_Msg(06);
094400180220             leavesr;
094500180220           endif;
094600180220
094700180222           // - ?Memorizzazione Filiale di Emissione in chiera?
094800180222           xx = %lookup( *blank : sk_ETF );
094900180220           if  xx > *zero;
095000180222             sk_ETF(xx)  = S02etf;
095100180222             if  ORGde5 <> *blank;
095200180222               sk_ETFd(xx) = ORGde5;
095300180222             else;
095400180222               sk_ETFd(xx) = ORGdes;
095500180222             endif;
095600180220           endif;
095700180220
095800180220         EndIf;
095900180220
096000180220       ENDSR;
096100180220
096200180220       //--------------------------------------------------------------
096300180222       //? Gestione videata W01 (dati relativi alla trasmissione).      ?
096400180220       //--------------------------------------------------------------
096500180220       BEGSR  sr_GesW01;
096600180220
096700180222         // - ?Inizializzazione videata?
096800180220         if  $InzW01 = *on;
096900180220           exsr  sr_InzW01;
097000180220           $InzW01 = *off;
097100180220         endif;
097200180220
097300180222         // - ?Emissione window?
097400180220         IF  TBXftt = 'S';
097500180220
097600180222           exfmt  TB47W01;
097700180220
097800180222           reset  $ErrGenerico;
097900180222           reset  $ErrMessage;
098000180222           clear  VIDmsg;
098100180220
098200180220           Select;
098300180220
098400180222             // - ?F12=Ritorno?
098500180220             When  dsp_aid = c_F12;
098600180220               $Video = 'S2';
098700180220               leavesr;
098800180220
098900180222             // - ?Invio?
099000180220             When  dsp_aid <> c_F06;
099100180220               leavesr;
099200180220
099300180220           EndSl;
099400180220
099500180220         ENDIF;
099600180220
099700180222         // - ?F6=Aggiornamento TNTBE00F?
099800180220         exsr  sr_UpdTNTBE;
099900180220
100000180222         If  $ErrGenerico = *on;
100100180222           // - ?Rilevato errore in fase di aggiornamento?
100200180220           leavesr;
100300180220         Else;
100400180222           // - ?Videata iniziale altrimenti?
100500180220           $Video  = 'S1';
100600180220           $InzS01 = $Immissione;
100700180220           if  not  $Immissione;
100800180220             if  TBEatb  = 'A';
100900180220               S1TBEann = 'Ann';
101000180220             else;
101100180220               clear  S1TBEann;
101200180220             endif;
101300180220           endif;
101400180220         EndIf;
101500180220
101600180220       ENDSR;
101700180220
101800180220       //--------------------------------------------------------------
101900180222       //? Inizializzazione videata W01.                                ?
102000180220       //--------------------------------------------------------------
102100180220       BEGSR  sr_InzW01;
102200180220
102300180222         clear  TB47W01;
102400180220
102500180220         select;
102600180222           // - ?Opz.4=Ripristino?
102700180220           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = 'A';
102800180220             W1ftt = TBEftt;
102900180222           // - ?Opz.4=Annullamento?
103000180220           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = *blank;
103100180220             W1ftt = TBEftt;
103200180222           // - ?Opz.2=Modifica?
103300180220           when  S1Copz = 2  and  %found(TNTBE01L);
103400180220             W1ftt = TBEftt;
103500180222           // - ?F10=Immissione?
103600180220           when  $Immissione  and  NOT %found(TNTBE01L);
103700180220             W1ftt = TBXftt;
103800180220         endsl;
103900180220
104000180222         // - ?Se NON immissione:?
104100180222         //   ?visualizza i dati relativi all'ultima trasmissione?
104200180220         if  %found(TNTBE01L);
104300180220           W1flt  = TBEflt;
104400180220           W1ftr  = TBEftr;
104500180220           if  TBEdtr <> *zero;
104600180220             wData_Eur = %date( TBEdtr : *iso );
104700180220             W1dtr     = %dec( wData_Eur );
104800180220           endif;
104900180220         endif;
105000180220
105100180220       ENDSR;
105200180220
105300180220       //--------------------------------------------------------------
105400180222       //? Aggiornamento record TNTBE00F (tab. "ETF").                  ?
105500180220       //--------------------------------------------------------------
105600180220       BEGSR  sr_UpdTNTBE;
105700180220
105800180222         // - ?Impostazione dei campi chiave SE immissione?
105900180220         if  $Immissione;
106000180222           clear  keyTNTBE01;
106100180220           k_TBEcod = c_Tab;
106200180222           k_TBEke1 = %editc( C02lna : 'X' );
106300180222           //*·k_TBEke2 = *blank;
106400180222           //*·k_TBElin = TBXlin;
106500180222           //*·k_TBEsif = TBXsif;
106600180222           chain  %kds( keyTNTBE01 )  TNTBE000;
106700180220         endif;
106800180220
106900180222         // - ?Impostazione dati (se NON annullamento)?
107000180220         if  S1Copz <> 4;
107100180220           TBEapl = TBXapl;
107200180222           TBEuni = ds_ETF;
107300180220         endif;
107400180220
107500180220         TBEftt = W1ftt;
107600180220         TBEflt = W1flt;
107700180220         clear TBEftr;
107800180220         //clear TBEdtr;
107900180220
108000180220         select;
108100180222           // - ?Opz.4=Ripristino?
108200180220           when  S1Copz = 4  and  TBEatb <> *blank;
108300180220             clear  TBEatb;
108400180222           // - ?Opz.4=Annullamento?
108500180220           when  S1Copz = 4  and  TBEatb = *blank;
108600180220             TBEatb = 'A';
108700180220         endsl;
108800180220
108900180220         IF  NOT  %found(TNTBE01L);
109000180220           clear  TBEsif;
109100180220           clear  TBElin;
109200180220           TBEcod = c_Tab;
109300180222           TBEke1 = %editc( C02lna : 'X' );
109400180222           clear  TBEke2;
109500180220           clear  TBEdtr;
109600180220           //_________________
109700180220           WRITE(e)  TNTBE000;
109800180220           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109900180220           if  %error;
110000180222             $ErrGenerico = *on;
110100180222             $ErrMessage  = *on;
110200180222             $PosCurLNA   = *on;
110300180222             VIDmsg = sk_Msg(06);
110400180220             leavesr;
110500180220           endif;
110600180220         ELSE;
110700180220           //_______________
110800180220           UPDATE  TNTBE000;
110900180220           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
111000180220         ENDIF;
111100180220
111200180220       ENDSR;
111300180220
111400180220       //--------------------------------------------------------------
111500180222       //? Operazioni finali.                                           ?
111600180220       //--------------------------------------------------------------
111700180220       BEGSR  sr_RoutEnd;
111800180220
111900180222         //*·kpjbu = TNTB47ds;
112000180220
112100180220         return;
112200180220
112300180220       ENDSR;
112400180220
112500180220      /end-free
112600180220
112700180222** - ?sk_Msg: ?Messaggi di Errore? ------------------------------------------*
112800180220Opzione errata                                                                  1
112900180220Record annullato: usare l'opzione 4 per ripristinarlo e modificarlo             2
113000180222Linea di Arrivo mancante o errata                                               3
113100180222Linea di Arrivo già presente in tabella                                         4
113200180222Filiale di Transito Terminal mancante o errata                                  5
113300180222Filiale di Transito Terminal già indicata                                       6
