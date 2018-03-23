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
007900180223     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
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
011100180223     d   $F12Attivo                    n   overlay( IndDspF : 12 )
011200180220         // - ?Emissione messaggio di errore?
011300180222     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011400180220         // - ?Indicatori di gestione del subfile?
011500180222     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
011600180222     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
011700180222     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
011800180222     d   $SflEnd                       n   overlay( IndDspF : 33 )
011900180222     d   $Sfl2Dsp_N                    n   overlay( IndDspF : 34 )
012000180220     d   $Sfl2DspCtl_N...
012100180222     d                                 n   overlay( IndDspF : 35 )
012200180222     d   $Sfl2NxtChg                   n   overlay( IndDspF : 36 )
012300180222     d   $Sfl2End                      n   overlay( IndDspF : 37 )
012400180220         // - ?Indicatori per Attibuti di visualizzazione?
012500180222     d   $RecAnnull                    n   overlay( IndDspF : 40 )
012600180222     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
012700180222     d   $ProtectETF                   n   overlay( IndDspF : 42 )
012800180220         // - ?Posizionamento cursore & segnalazione errore?
012900180222     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
013000180222     d   $PosCurLNA                    n   overlay( IndDspF : 51 )
013100180222     d   $PosCurETF                    n   overlay( IndDspF : 52 )
013200180220         // - ?Riemissione videata?
013300180222     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013400180220
013500180220       // - ?Numero MAX di Eccezioni (come Terminal di Arrivo) gestibili?
013600180220     d c_MaxETF        c                   const(05)
013700180220       // - ?Terminal di Arrivo (eccezioni) in tab. "ETF"?
013800180220     d ds_ETF          ds            15    inz
013900180222     d   S1Cetf1                           inz
014000180222     d   S1Cetf2                           inz
014100180222     d   S1Cetf3                           inz
014200180222     d   S1Cetf4                           inz
014300180222     d   S1Cetf5                           inz
014400180220     d   sk_ETF                1     15    inz  dim(c_MaxETF)
014500180222     d Save_ETF        s                   like(ds_ETF)    inz
014600180220       // - ?Descrizioni Terminal di Arrivo (eccezioni) in tab. "ETF"?
014700180222     d ds_ETFdes       ds            50    inz
014800180222     d   S1Detf1                           inz
014900180222     d   S1Detf2                           inz
015000180222     d   S1Detf3                           inz
015100180222     d   S1Detf4                           inz
015200180222     d   S1Detf5                           inz
015300180222     d   sk_ETFd               1     50    inz  dim(c_MaxETF)
015400180222     d Save_ETFdes     s                   like(ds_ETFdes) inz
015500180220
015600180220       // - ?Parametri ricevuti?
015700180220     d KPJBA         e ds
015800180223     d TNTB47ds      e ds                  inz  qualified
015900180220
016000180220       // - ?Dati record principale della tabella?
016100180222     d TNTBEds       e ds                  inz  extname( TNTBE00F )
016200180222     d xTNTBEds      e ds                  inz  extname( TNTBE00F )
016300180222     d                                          prefix( TBX : 3 )
016400180220
016500180220       //--------------------------------------------------------------
016600180220       //? Definizione variabili globali.                               ?
016700180220       //--------------------------------------------------------------
016800180220
016900180220       // - ?Flags booleani?
017000180223     d $Called         s               n   inz
017100180220     d $Fine           s               n   inz
017200180220     d $EoF            s               n   inz
017300180220     d $Immissione     s               n   inz
017400180220     d $InzS01         s               n   inz(*on)
017500180220     d $InzS02         s               n   inz(*on)
017600180220     d $InzW01         s               n   inz(*on)
017700180220
017800180220       // - ?Indici di schiera?
017900180220     d xx              s              3  0 inz
018000180220
018100180220       // - ?Variabili per la gestione del video?
018200180220     d $Video          s              2    inz('S1')
018300180222     d S01nrr          s                   like(C1RcdNbr)  inz
018400180220     d S02nrr          s                   like(C2RcdNbr)  inz
018500180220     d SavS01csr       s                   like(C1RcdNbr)  inz
018600180223
018700180223       // -?Conteggio selezioni (opz. 1) inserite?
018800180223     d wMemoSel        s                   like(S1Copz)   inz
018900180220
019000180220       // - ?Variabili di comodo?
019100180220     d wData_EUR       s               d   datfmt(*eur)    inz(*loval)
019200180220
019300180220       //--------------------------------------------------------------
019400180220       //? Definizione prototipi procedure.                             ?
019500180220       //--------------------------------------------------------------
019600180220
019700180220       // - ?Reperimento dati utente?
019800180220     d TIBS34ds      e ds                  inz
019900180220      /copy gaitrasrc/srcProtoPR,TIBS34R
020000180220
020100180220       // - ?Caricamento schiera filiali?
020200180220     d TRUL06ds      e ds                  inz
020300180220     d   D06cod      e                     inz('£1')
020400180220     d   D06tla      e                     inz('L')
020500180220     d   D06lin                1     90  0 inz   dim(30)
020600180220      /copy gaitrasrc/srcProtoPr,TRUL06R
020700180220
020800180220       // - ?Ricerca codici Organigramma?
020900180220     d SD19cod         s              3    inz
021000180220     d SD19tip         s              1    inz
021100180220     d SD19des         s             25    inz
021200180220     d tnsd19r         pr                  extpgm('TNSD19R')
021300180220     d   D19cod                            like(SD19cod)
021400180220     d   D19tip                            like(SD19tip)
021500180220     d   D19des                            like(SD19des)
021600180220
021700180220       // - ?API QCAPCMD (Process Commands)?
021800180220     d Qcmd            s           2048    inz  varying
021900180220      /copy qSysInc/qRpgleSrc,QCAPCMD
022000180220      /copy gaitrasrc/srcProtoPR,QCAPCMD
022100180220
022200180220       // - ?Parametri gestione errori API.?
022300180220      /copy qsysinc/qrpglesrc,QUSEC
022400180220
022500180220       //--------------------------------------------------------------
022600180220       //? Definizione key-list.                                        ?
022700180220       //--------------------------------------------------------------
022800180220
022900180220       // - ?File TNTBE01L?
023000180220     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
023100180220     d                                     prefix( k_ )    inz
023200180220
023300180220       // - ?File AZORG01L?
023400180220     d keyAZORG01    e ds                  extname( AZORG01L : *key )
023500180220     d                                     prefix( k_ )    inz
023600180220
023700180220       //--------------------------------------------------------------
023800180220       //? Riepilogo indicatori utilizzati.                             ?
023900180220       //--------------------------------------------------------------
024000180220
024100180220
024200180220       //--------------------------------------------------------------
024300180220       //? M A I N - L I N E                                            ?
024400180220       //--------------------------------------------------------------
024500180220
024600180220     c     *Entry        plist
024700180220     c                   parm                    KPJBA
024800180220
024900180220      /free
025000180220
025100180220       // - ?Operazioni iniziali?
025200180220       exsr  sr_RoutInz;
025300180220
025400180220       // - ?Ciclo di gestione del file video?
025500180220       DoW  $Fine = *off;
025600180220
025700180220         select;
025800180220
025900180220           // - ?Gestione videata S1 (subfile)?
026000180220           when  $Video = 'S1';
026100180220             exsr  sr_GesS01;
026200180220
026300180220           // - ?Gestione videata S2 (subfile)?
026400180220           when  $Video = 'S2';
026500180220             exsr  sr_GesS02;
026600180220
026700180220           // - ?Richiesta trasmissione dati?
026800180220           when  $Video = 'W1';
026900180220             exsr  sr_GesW01;
027000180220
027100180220           // - ?? ? ??
027200180220           other;
027300180220             $Fine = *on;
027400180220
027500180220         endsl;
027600180220
027700180220       EndDo;
027800180220
027900180220       // - ?Operazioni finali?
028000180220       exsr  sr_RoutEnd;
028100180220
028200180220       //--------------------------------------------------------------
028300180220       //? Operazioni iniziali.                                         ?
028400180220       //--------------------------------------------------------------
028500180220       BEGSR  sr_RoutInz;
028600180220
028700180220         // - ?Impostazione chiusura?
028800180220         *inLR = *on;
028900180222
029000180222         // - ?Impostazione nome programma a video?
029100180222         V1Tpgm = SDSpgmName;
029200180220
029300180220         // - ?Reperimento dati job?
029400180220         exsr  sr_DatiJob;
029500180223
029600180223         // -?Verifica parametri ricevuti?
029700180223         $Called  = ( KPJBU <> *blank );
029800180223         if  $Called;
029900180223           tntb47ds = kpjbu;
030000180223         else;
030100180223           clear  tntb47ds;
030200180223         endif;
030300180223
030400180223         // -?Pulizia parametri di output?
030500180223         clear  tntb47ds.o47lna;
030600180223         clear  tntb47ds.o47uni;
030700180223         clear  tntb47ds.o47fxx;
030800180223         tntb47ds.o47err = *off;
030900180223         clear  tntb47ds.o47msg;
031000180223
031100180223         // -?Impostazione videata iniziale?
031200180223         if  $Called  and  ( tntb47ds.i47opz = '2'
031300180223                       or    tntb47ds.i47opz = '5' )
031400180223                      and  tntb47ds.i47lna  <> *zero;
031500180223           $Video      = 'S2';
031600180223           S1Copz      = %int( tntb47ds.i47opz );
031700180223           S1tbeKE1    = %editc( tntb47ds.i47lna : 'X' );
031800180223           $ProtectKEY = ( tntb47ds.i47opz = '2' );
031900180223           //*·$ProtectETF = ( tntb47ds.i47opz = '5' );
032000180223           $Immissione = *off;
032100180223         else;
032200180223           $Video = 'S1';
032300180223         endif;
032400180223         reset  $InzS01;
032500180223         reset  $InzS02;
032600180220
032700180220         // - ?Aggancio dati generali della tabella in esame?
032800180220         clear  keyTNTBE01;
032900180220         k_TBEke1 = *zero;
033000180220         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
033100180223                  = c_Tab;
033200180220         clear  k_TBEke2;
033300180220         clear  k_TBElin;
033400180220         k_TBEsif = KNSIF;
033500180220         chain  %kds( keyTNTBE01 )  TNTBE000;
033600180220         if  not %found(TNTBE01L);
033700180220           clear  k_TBEsif;
033800180220           chain  %kds( keyTNTBE01 )  TNTBE000;
033900180220         endif;
034000180220         if  %found(TNTBE01L);
034100180220           xTNTBEds = TNTBEds;
034200180220         else;
034300180220           clear  xTNTBEds;
034400180220         endif;
034500180220
034600180220         // - ?Pulizia / impostazione iniziale dei campi chiave?
034700180220         clear  keyTNTBE01;
034800180220         k_TBEcod = c_Tab;
034900180220
035000180220       ENDSR;
035100180220
035200180220       //--------------------------------------------------------------
035300180220       //? Reperimento Dati del job (Utente/Operativi).                 ?
035400180220       //--------------------------------------------------------------
035500180220       BEGSR  sr_DatiJob;
035600180220
035700180220         in(e) §AzUte;
035800180220         if NOT %error;
035900180220           in(e) §DatiUte;
036000180220         endif;
036100180220         if %error or RSut = *blank;
036200180220           tibs34r ( tibs34ds );
036300180220           in §AzUte;
036400180220           in §DatiUte;
036500180220         endif;
036600180220
036700180220       ENDSR;
036800180220
036900180220       //--------------------------------------------------------------
037000180220       //? Gestione subfile S01.                                        ?
037100180220       //--------------------------------------------------------------
037200180220       BEGSR  sr_GesS01;
037300180220
037400180220         // - ?Inizializzazione videata?
037500180220         if  $InzS01 = *on;
037600180220           exsr  sr_InzS01;
037700180220           $InzS01 = *off;
037800180220         endif;
037900180220
038000180220         // - ?Emissione Testata & Piede?
038100180222         write  TB47T01;
038200180222         write  TB47P01;
038300180220
038400180220         // - ?Posizionamento cursore?
038500180220         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
038600180220         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
038700180220         //   ?si visualizza la pagina che vedeva l'utente quando ha?
038800180220         //   ?premuto l'ultimo tasto.?
038900180220         if  C1CsrRrn > *zero;
039000180220           C1RcdNbr = C1CsrRrn;
039100180220         else;
039200180220           C1RcdNbr = 1;
039300180222           write  TB47S00;          // ?(no rec.)?
039400180220         endif;
039500180220
039600180220         // - ?Emissione videata?
039700180222         exfmt  TB47C01;
039800180220
039900180220         reset  $ErrGenerico;
040000180220         reset  $ErrMessage;
040100180220         clear  VIDmsg;
040200180220
040300180220         Select;
040400180220
040500180220           // - ?F3=Fine?
040600180220           When  dsp_aid = c_F03;
040700180220             $Fine = *on;
040800180223             if  $Called;
040900180223               tntb47ds.o47fxx = '03';
041000180223             endif;
041100180220
041200180220           // - ?F5=Rivisualizza?
041300180220           When  dsp_aid = c_F05;
041400180220             $InzS01 = *on;
041500180220
041600180220           // - ?F10=Immissione?
041700180220           When  dsp_aid = c_F10;
041800180220             $Video  = 'S2';
041900180220             $InzS02 = *on;
042000180222             //*·DoW  $Video = 'S2'  and  Not $Fine;
042100180222             //*·  exsr  sr_GesS02;
042200180222             //*·EndDo;
042300180223
042400180223           // - ?F12=Ritorno?
042500180223           When  dsp_aid = c_F12;
042600180223             $Fine = *on;
042700180223             if  $Called;
042800180223               tntb47ds.o47fxx = '12';
042900180223             endif;
043000180220
043100180220           // - ?SubFile vuoto?
043200180220           When  S01nrr = *zero;
043300180220
043400180220           // - ?Invio?
043500180220           Other;
043600180220             // - ?Gestione opzioni?
043700180220             exsr  sr_OpzS01;
043800180223             select;
043900180223               when  $ErrGenerico;
044000180223                 leavesr;
044100180223               when  wMemoSel > *zero;
044200180223                 $Fine = *on;
044300180223                 leavesr;
044400180223             endsl;
044500180220
044600180222         EndSl;
044700180220
044800180220       ENDSR;
044900180220
045000180220       //--------------------------------------------------------------
045100180220       //? Inizializzazione subfile S01.                                ?
045200180220       //--------------------------------------------------------------
045300180220       BEGSR  sr_InzS01;
045400180220
045500180220         // - ?Spegnimento degli indicatori di errore?
045600180220         %subst( IndDspF : 50 ) = *off;
045700180222
045800180222         // - ?Impostazioni opzioni?
045900180223         select;
046000180223           when  $Called  and  tntb47ds.i47opz = '1';
046100180223             C1Dopz = '1=Selezione, 5=Visualizzazione';
046200180223           when  $Called  and  tntb47ds.i47opz = '5';
046300180223             C1Dopz = '5=Visualizzazione';
046400180223           other;
046500180223             C1Dopz = '2=Modifica, +
046600180223                       4=Annullamento/Ripristino, +
046700180223                       5=Visualizzazione.';
046800180223         endsl;
046900180222
047000180222         // - ?Abilitazione tasti funzionali condizionati?
047100180223         $F10Attivo = ( NOT $Called  or  ( tntb47ds.i47opz <> '1'
047200180223                                     and   tntb47ds.i47opz <> '5' ) );
047300180223         $F12Attivo = $Called;
047400180220
047500180220         // - ?Pulizia subfile?
047600180220         $SflDsp_N    = *on;
047700180220         $SflDspCtl_N = *on;
047800180222         write  TB47C01;
047900180220         $SflDspCtl_N = *off;
048000180220         $SflEnd      = *off;
048100180220
048200180220         clear  C1RcdNbr;
048300180220         clear  C1CsrRrn;
048400180220         clear  S01nrr;
048500180222         clear  VIDmsg;
048600180220         $ErrMessage  = *off;
048700180220         $ErrGenerico = *off;
048800180220
048900180220         // - ?Caricamento completo dei dati nel subfile?
049000180222         //   ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
049100180222         //   ?l'eventuale ordinamento)?
049200180222         $SflNxtChg = *off;
049300180220         setll  %kds( keyTNTBE01 )  TNTBE000;
049400180220         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
049500180220         $EoF = ( %eof(TNTBE01L) );
049600180222         // - ?Ciclo di caricamento dati da tab. "ETF"?
049700180222         DoW  Not $EoF;
049800180222           if  TBEke2 = *blank;
049900180222             exsr  sr_CaricaS01;
050000180222           endif;
050100180222           // - ?Lettura record successivo?
050200180222           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
050300180222           $EoF = ( %eof(TNTBE01L) );
050400180222         EndDo;
050500180222
050600180222         // - ?Visualizzazione del SFL (se ci sono dati)?
050700180222         $SflDsp_N = ( S01nrr <= *zero );
050800180222
050900180222         // - ?Attivazione del SFLEND?
051000180222         $SflEnd   = ( $EoF );
051100180222
051200180222         // - ?Impaginazione subfile?
051300180222         //   ?(posizionamento cursore al 1° rcd della 1ª pagina)?
051400180222         if  S01nrr > *zero;
051500180222           C1RcdNbr = 1;
051600180222         else;
051700180222           clear  C1RcdNbr;
051800180222         endif;
051900180222         C1CsrRrn = C1RcdNbr;
052000180220
052100180220       ENDSR;
052200180220
052300180220       //--------------------------------------------------------------
052400180222       //? Caricamento singolo rec. nel subfile S01.                    ?
052500180220       //--------------------------------------------------------------
052600180220       BEGSR  sr_CaricaS01;
052700180220
052800180222         // · ?Impostazione campi?
052900180222         clear  TB47S01;
053000180222         S1TBEke1 = TBEke1;
053100180222         H1TBEke2 = TBEke2;
053200180222         if  TBEatb  = 'A';
053300180222            S1TBEann = 'Ann';
053400180222         endif;
053500180222
053600180222         // · ?Eccezioni Terminal Filiale?
053700180222         ds_ETF = TBEuni;
053800180222
053900180222         // · ?Decodifica Linea di Arrivo?
054000180222         If  %check( c_Digits : S1tbeKE1 ) = *zero;
054100180222           k_ORGfil = %int( S1tbeKE1 );
054200180222           chain  %kds(keyAZORG01)  AZORG;
054300180222           if  %found(AZORG01L);
054400180222             S1Dlna = ORGdes;
054500180222           endif;
054600180222         EndIf;
054700180222
054800180222         // · ?Decodifica Eccezioni Terminal Filiale?
054900180222         clear  ds_ETFdes;
055000180222         For  xx = 1  To  %elem( sk_ETF );
055100180222
055200180222           If  %check( c_Digits : sk_ETF(xx) ) = *zero  and
055300180222               sk_ETF(xx)                      > *zero;
055400180222
055500180222             k_ORGfil = %int( sk_ETF(xx) );
055600180222             chain  %kds(keyAZORG01)  AZORG;
055700180222             if  %found(AZORG01L);
055800180222               if  ORGde5 <> *blank;
055900180222                 sk_ETFd(xx) = ORGde5;
056000180222               else;
056100180222                 sk_ETFd(xx) = ORGdes;
056200180222               endif;
056300180222             endif;
056400180222
056500180222           EndIf;
056600180222
056700180222         EndFor;
056800180222
056900180222         // · ?Scrittura record nel subfile?
057000180222         S01nrr += 1;
057100180222         write  TB47S01;
057200180220
057300180220       ENDSR;
057400180220
057500180220       //--------------------------------------------------------------
057600180220       //? Caricamento completo del subfile S01.                        ?
057700180220       //--------------------------------------------------------------
057800180220       BEGSR  sr_OpzS01;
057900180220
058000180220         clear  SavS01csr;
058100180223         clear  wMemoSel;
058200180220
058300180220         // - ?Ciclo di lettura subfile?
058400180222         readc  TB47S01;
058500180220
058600180222         DoW  Not %eof( TNTB47D );
058700180220
058800180222           $SflNxtChg = *off;
058900180223           %subst( IndDspF : 50 ) = *off;
059000180220           C1RcdNbr = S01nrr;
059100180220
059200180223           Select;
059300180223
059400180223             // - ?Nessuna opzione?
059500180223             When  S1Copz = *zero;
059600180223
059700180223             // - ?Opz. 1=Selezione filiale?
059800180223             When  $Called  and  tntb47ds.i47opz = '1'
059900180223                            and  S1Copz = 1;
060000180223               // - ?Già selezionata una filiale?
060100180223               if  wMemoSel > *zero;
060200180223                 $ErrGenerico = *on;
060300180223                 $ErrMessage  = *on;
060400180223                 $PosCurOPZ   = *on;
060500180223                 VIDmsg   = sk_Msg(07);
060600180223               else;
060700180223                 wMemoSel = S1Copz;
060800180223               endif;
060900180220
061000180220             // - ?Opz. 2=Modifica?
061100180220             //   ?Opz. 4=Annullamento/Ripristino?
061200180223             //   ?Opz. 5=Visualizzazione?
061300180312             When  ( ( $Called    and
061400180312                   ( S1Copz = 2   or  S1Copz = 4 ) )    or
061500180312                   ( Not $Called  and
061600180312                   ( S1Copz = 2   or  S1Copz = 4 ) ) )  or  S1Copz = 5;
061700180220               If  S1Copz = 2  and  S1TBEann <> *blank;
061800180220                 $ErrGenerico = *on;
061900180220                 $ErrMessage  = *on;
062000180220                 $PosCurOPZ   = *on;
062100180222                 VIDmsg = sk_Msg(02);
062200180220               Else;
062300180220                 $Video  = 'S2';
062400180220                 $InzS02 = *on;
062500180220                 DoW  $Video = 'S2'  and  Not $Fine;
062600180220                   exsr  sr_GesS02;
062700180220                 EndDo;
062800180220               EndIf;
062900180220
063000180220             // -?Opzione errata?
063100180223             Other;
063200180220               $ErrGenerico = *on;
063300180220               $ErrMessage  = *on;
063400180220               $PosCurOPZ   = *on;
063500180222               VIDmsg = sk_Msg(01);
063600180220               $Fine  = *off;
063700180220
063800180220           endsl;
063900180220
064000180220           // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
064100180220           //   ?riposizionarvici il cursore dopo il tasto "Invio"?
064200180220           if  S1Copz  <> *zero    and
064300180220              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
064400180220             SavS01csr   = C1RcdNbr;
064500180220           endif;
064600180220
064700180220           // - ?Aggiornamento sfl?
064800180220           select;
064900180220             when  $ErrMessage;
065000180222               $SflNxtChg = *on;
065100180220               C1CsrRrn  = C1RcdNbr;
065200180220             when  $ErrGenerico;
065300180220               C1CsrRrn  = C1RcdNbr;
065400180220               clear  S1Copz;
065500180220             other;
065600180220               clear  S1Copz;
065700180220           endsl;
065800180220
065900180222           UPDATE  TB47S01;
066000180220
066100180220           if  $ErrGenerico  or  $ErrMessage;
066200180220             leave;
066300180220           endif;
066400180220
066500180222           readc  TB47S01;
066600180220
066700180220         ENDDO;
066800180220
066900180220         // - ?Riposizionam. cursore sul record contenente la prima opz.?
067000180220         //   ?(se non sono stati rilevati errori)?
067100180220         if  NOT $ErrMessage   and   NOT $ErrGenerico   and
067200180220             SavS01csr > *zero;
067300180220           C1CsrRrn = SavS01csr;
067400180220         endif;
067500180220
067600180220       ENDSR;
067700180220
067800180220       //--------------------------------------------------------------
067900180220       //? Gestione subfile S02.                                        ?
068000180220       //--------------------------------------------------------------
068100180220       BEGSR  sr_GesS02;
068200180220
068300180220         // - ?Inizializzazione videata?
068400180220         if  $InzS02 = *on;
068500180220           exsr  sr_InzS02;
068600180220           $InzS02 = *off;
068700180220         endif;
068800180220
068900180220         // - ?Emissione Testata & Piede?
069000180222         write  TB47T01;
069100180223         write  TB47P01;
069200180220
069300180220         // - ?Posizionamento cursore?
069400180220         //   ?C2CsrRrn contiene il numero di riga del subfile su cui?
069500180220         //   ?era posizionato il cursore; impostandolo in C2RcdNbr?
069600180220         //   ?si visualizza la pagina che vedeva l'utente quando ha?
069700180220         //   ?premuto l'ultimo tasto.?
069800180220         if  C2CsrRrn > *zero;
069900180220           C2RcdNbr = C2CsrRrn;
070000180220         endif;
070100180220
070200180220         // - ?Emissione videata?
070300180220         if  Not $Immissione  AND
070400180220            (S1Copz = 5       or
070500180220            (S1Copz = 4  and  TBEatb = *blank));
070600180222           write  TB47C02;
070700180220           exfmt  Protect;
070800180220         else;
070900180222           exfmt  TB47C02;
071000180220         endif;
071100180220
071200180220         reset  $ErrGenerico;
071300180220         reset  $ErrMessage;
071400180220         clear  VIDmsg;
071500180220
071600180220         Select;
071700180220
071800180220           // - ?F3=Fine?
071900180220           When  dsp_aid = c_F03;
072000180220             unlock  TNTBE01L;
072100180220             $Video = 'S1';
072200180220             $ErrGenerico = *on;
072300180222             ds_ETF    = Save_ETF;
072400180222             ds_ETFdes = Save_ETFdes;
072500180220
072600180220           // - ?F12=Ritorno?
072700180220           When  dsp_aid = c_F12;
072800180220             unlock  TNTBE01L;
072900180220             $Video = 'S1';
073000180222             ds_ETF    = Save_ETF;
073100180222             ds_ETFdes = Save_ETFdes;
073200180220
073300180220           // - ?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
073400180220           Other;
073500180220             // - ?Controlli eseguiti NON se F16=Annullamento?
073600180220             if  dsp_aid <> c_F16;
073700180220               exsr  sr_CtrC02;
073800180220             endif;
073900180220             if  $ErrGenerico;
074000180220               leavesr;
074100180220             endif;
074200180220             // - ?Aggiornamento?
074300180220             if  dsp_aid = c_F06;
074400180220               $Video  = 'W1';
074500180220               $InzW01 = *on;
074600180220               DoW  $Video = 'W1';
074700180220                 exsr  sr_GesW01;
074800180220               EndDo;
074900180220             endif;
075000180220
075100180220         EndSl;
075200180220
075300180220       ENDSR;
075400180220
075500180220       //--------------------------------------------------------------
075600180220       //? Inizializzazione subfile S02.                                ?
075700180220       //--------------------------------------------------------------
075800180220       BEGSR  sr_InzS02;
075900180220
076000180220         // - ?Spegnimento degli indicatori di errore?
076100180220         %subst( IndDspF : 50 ) = *off;
076200180220
076300180220         // - ?Pulizia subfile-control?
076400180222         clear  TB47C02;
076500180220
076600180220         // - ?Pulizia subfile?
076700180220         $Sfl2Dsp_N    = *on;
076800180220         $Sfl2DspCtl_N = *on;
076900180222         write  TB47C02;
077000180220         $Sfl2DspCtl_N = *off;
077100180220         $Sfl2End      = *off;
077200180220
077300180220         clear  C2RcdNbr;
077400180220         clear  C2CsrRrn;
077500180220         clear  S02nrr;
077600180220         clear  VIDmsg;
077700180220         $ErrMessage  = *off;
077800180220         $ErrGenerico = *off;
077900180220
078000180220         // - ?Caricamento dei dati nel subfile-control?
078100180220         select;
078200180220            when  dsp_aid = c_F10;
078300180220              D2DesOpz = '  IMMISSIONE   ';
078400180220            when  S1Copz  = 2;
078500180220              D2DesOpz = '   MODIFICA    ';
078600180220            when  S1Copz  = 4  and  S1TBEann = *blank;
078700180220              D2DesOpz = ' ANNULLAMENTO  ';
078800180220            when  S1Copz  = 4  and  S1TBEann <> *blank;
078900180220              D2DesOpz = '  RIPRISTINO   ';
079000180220            when  S1Copz  = 5;
079100180220              D2DesOpz = 'VISUALIZZAZIONE';
079200180220         endsl;
079300180220
079400180220         // - ?Protezione campi chiave SE NON inserimento?
079500180220         $Immissione = (dsp_aid = c_F10);
079600180222         $ProtectKEY = (dsp_aid <> c_F10);
079700180222
079800180222         // - ?Salvataggio dei dati già inseriti?
079900180222         //   ?(da rivisualizzare nel subfile per F12)?
080000180222         if  $Immissione;
080100180222           clear  Save_ETF;
080200180222           clear  Save_ETFdes;
080300180222         else;
080400180222           Save_ETF    = ds_ETF;
080500180222           Save_ETFdes = ds_ETFdes;
080600180222         endif;
080700180220
080800180220         // - ?Reperimento dati dal file?
080900180222         clear  ds_ETF;
081000180222         If  dsp_aid <> c_F10;
081100180222
081200180222           clear  keyTNTBE01;
081300180220           k_TBEcod = c_Tab;
081400180220           k_TBEke1 = S1TBEke1;
081500180222           k_TBEke2 = H1TBEke2;
081600180222           chain  %kds( keyTNTBE01 : 3 )  TNTBE000;
081700180220           if  %found(TNTBE01L);
081800180222             ds_ETF = TBEuni;
081900180222             $RecAnnull = ( TBEatb <> *blank );
082000180220           endif;
082100180222
082200180222         EndIf;
082300180220
082400180222         // - ?Impostazione campi chiave  e?
082500180222         //   ?Caricamento delle Eccezioni Terminal Filiale (di Transito)?
082600180222         //   ?(SE NON immissione)?
082700180220         If  dsp_aid <> c_F10;
082800180220
082900180222           // · ?Linea di Arrivo?
083000180222           C02lna = %int( S1TBEke1 );
083100180222           k_ORGfil = C02lna;
083200180222           chain  %kds( keyAZORG01 )  AZORG;
083300180222           if  %found( AZORG01L );
083400180222             C02lnaD = ORGdes;
083500180220           endif;
083600180220
083700180220         EndIf;
083800180220
083900180222         // · ?Decodifica Eccezioni Terminal Filiale?
084000180222         For  xx = 1  To  %elem( sk_ETF );
084100180220
084200180222           clear  TB47S02;
084300180220
084400180222           If  %check( c_Digits : sk_ETF(xx) ) = *zero  and
084500180222               sk_ETF(xx)                      > *zero;
084600180220
084700180222             $Sfl2NxtChg = *on;
084800180222             S02etf = sk_ETF(xx);
084900180222
085000180222             k_ORGfil = %int( S02etf );
085100180222             chain  %kds( keyAZORG01 )  AZORG;
085200180220             if  %found(AZORG01L);
085300180222               //*·if  ORGde5 <> *blank;
085400180222               //*·  S02etfD = ORGde5;
085500180222               //*·else;
085600180222                 S02etfD = ORGdes;
085700180222               //*·endif;
085800180220             endif;
085900180220
086000180222           EndIf;
086100180220
086200180222           // · ?Scrittura record nel subfile?
086300180220           S02nrr += 1;
086400180222           write  TB47S02;
086500180220
086600180220         EndFor;
086700180220
086800180220         // -?Posizionamento cursore sulla Filiale di Ritiro?
086900180220         //  ?SE immissione?
087000180222         $PosCurLNA = ( dsp_aid = c_F10 );
087100180220
087200180220         // -?Visualizzazione del SFL (se ci sono dati)?
087300180222         $Sfl2Dsp_N = ( S02nrr <= *zero );
087400180220
087500180220         // -?Attivazione del SFLEND?
087600180222         $Sfl2End = *on;
087700180220
087800180220         // -?Impaginazione subfile?
087900180220         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
088000180222         if  S02nrr > *zero;
088100180220           C2RcdNbr  = 1;
088200180220           C2CsrRrn  = 1;
088300180220         else;
088400180220           clear C2RcdNbr;
088500180220         endif;
088600180220
088700180220         // -?Abilitazione tasti funzionali condizionati?
088800180222         $F6Attivo  = ( $Immissione  or  S1Copz <> 5 );
088900180223         $F12Attivo = *on;
089000180220
089100180220       ENDSR;
089200180220
089300180220       //--------------------------------------------------------------
089400180222       //? Controllo dati nella videata C02/S02.                        ?
089500180220       //--------------------------------------------------------------
089600180220       BEGSR  sr_CtrC02;
089700180220
089800180222         // - ?Spegnimento degli indicatori di errore?
089900180222         %subst( IndDspF : 50 ) = *off;
090000180220
090100180222         // - ?Controllo Linea di Arrivo?
090200180222         // · ?Obbligatoria?
090300180222         if  C02lna = *zero;
090400180222           $ErrGenerico = *on;
090500180222           $ErrMessage  = *on;
090600180222           $PosCurLNA   = *on;
090700180222           VIDmsg = sk_Msg(03);
090800180220           leavesr;
090900180220         endif;
091000180222         // · ?Errata?
091100180222         k_ORGfil = C02lna;
091200180222         chain  %kds( keyAZORG01)  AZORG;
091300180222         if  Not %found( AZORG01L )  or  ( ORGfag <> 'F'  and
091400180222                                           ORGfag <> 'A' );
091500180222           $ErrGenerico = *on;
091600180222           $ErrMessage  = *on;
091700180222           $PosCurLNA   = *on;
091800180222           VIDmsg = sk_Msg(03);
091900180220           leavesr;
092000180220         endif;
092100180222
092200180222         C02lnaD = ORGdes;
092300180220
092400180222         // - ?Controllo SE chiave duplicata (SE immissione)?
092500180220         If  $Immissione;
092600180222           clear  keyTNTBE01;
092700180220           k_TBEcod = c_Tab;
092800180222           k_TBEke1 = %editc( C02lna : 'X' );
092900180222           //*·k_TBEke2 = *blank;
093000180222           setll  %kds( keyTNTBE01 )  TNTBE000;
093100180222           if  %equal( TNTBE01L );
093200180222             $ErrGenerico = *on;
093300180222             $ErrMessage  = *on;
093400180222             $PosCurLNA   = *on;
093500180222             VIDmsg = sk_Msg(04);
093600180220             leavesr;
093700180220           endif;
093800180220         EndIf;
093900180220
094000180222         // - ?Controllo Eccezioni Terminal Filiale (Transito Terminal)?
094100180222         clear  sk_ETF;
094200180222         clear  sk_ETFd;
094300180222         readc  TB47S02;
094400180220
094500180222         DoW  Not %eof( TNTB47D );
094600180220
094700180220           exsr  sr_CtrS02;
094800180220
094900180222           // - ?Aggiornamento sfl?
095000180220           select;
095100180222             when  $ErrMessage;
095200180222               $Sfl2NxtChg = *on;
095300180222               C2CsrRrn = C2RcdNbr;
095400180222             when  $ErrGenerico;
095500180222               C2CsrRrn = C2RcdNbr;
095600180220           endsl;
095700180222           $Sfl2NxtChg = *on;
095800180222           UPDATE  TB47S02;
095900180220
096000180222           if  $ErrGenerico   or  $ErrMessage;
096100180220             leave;
096200180220           endif;
096300180220
096400180222           readc  TB47S02;
096500180220
096600180220         EndDo;
096700180220
096800180220       ENDSR;
096900180220
097000180220       //--------------------------------------------------------------
097100180222       //? Controllo dati nel subfile S02.                              ?
097200180220       //--------------------------------------------------------------
097300180220       BEGSR  sr_CtrS02;
097400180220
097500180222         clear  S02etfD;
097600180220
097700180222         If  S02etf <> *blank  and  S02etf <> *zero;
097800180220
097900180222           // - ?Eccezione Terminal Filiale NON numerica?
098000180222           if  %check( c_Digits : S02etf ) > *zero;
098100180222             $ErrGenerico = *on;
098200180222             $ErrMessage  = *on;
098300180222             $PosCurETF   = *on;
098400180222             VIDmsg = sk_Msg(05);
098500180220             leavesr;
098600180220           endif;
098700180220
098800180222           k_ORGfil = %int( S02etf );
098900180222           chain  %kds( keyAZORG01 )  AZORG;
099000180222           // - ?Eccezione Terminal Filiale errata?
099100180222           if  Not %found( AZORG01L )  or  ( ORGfag <> 'F'  and
099200180222                                             ORGfag <> 'A' );
099300180222             $ErrGenerico = *on;
099400180222             $ErrMessage  = *on;
099500180222             $PosCurETF   = *on;
099600180222             VIDmsg = sk_Msg(05);
099700180220             leavesr;
099800180220           endif;
099900180220
100000180222           //*·if  ORGde5 <> *blank;
100100180222           //*·  S02etfD = ORGde5;
100200180222           //*·else;
100300180222             S02etfD = ORGdes;
100400180222           //*·endif;
100500180220
100600180220         EndIf;
100700180220
100800180222         if  S02etf = *zero;
100900180222           clear  S02etf;
101000180220         endif;
101100180220
101200180222         If  S02etf <> *blank;
101300180220
101400180222           // - ?Eccezione Terminal Filiale RIPETUTA nel subfile?
101500180222           if  %lookup( S02etf : sk_ETF ) > *zero;
101600180222             $ErrGenerico = *on;
101700180222             $ErrMessage  = *on;
101800180222             $PosCurETF   = *on;
101900180222             VIDmsg = sk_Msg(06);
102000180220             leavesr;
102100180220           endif;
102200180220
102300180222           // - ?Memorizzazione Filiale di Emissione in chiera?
102400180222           xx = %lookup( *blank : sk_ETF );
102500180220           if  xx > *zero;
102600180222             sk_ETF(xx)  = S02etf;
102700180222             if  ORGde5 <> *blank;
102800180222               sk_ETFd(xx) = ORGde5;
102900180222             else;
103000180222               sk_ETFd(xx) = ORGdes;
103100180222             endif;
103200180220           endif;
103300180220
103400180220         EndIf;
103500180220
103600180220       ENDSR;
103700180220
103800180220       //--------------------------------------------------------------
103900180222       //? Gestione videata W01 (dati relativi alla trasmissione).      ?
104000180220       //--------------------------------------------------------------
104100180220       BEGSR  sr_GesW01;
104200180220
104300180222         // - ?Inizializzazione videata?
104400180220         if  $InzW01 = *on;
104500180220           exsr  sr_InzW01;
104600180220           $InzW01 = *off;
104700180220         endif;
104800180220
104900180222         // - ?Emissione window?
105000180220         IF  TBXftt = 'S';
105100180220
105200180222           exfmt  TB47W01;
105300180220
105400180222           reset  $ErrGenerico;
105500180222           reset  $ErrMessage;
105600180222           clear  VIDmsg;
105700180220
105800180220           Select;
105900180220
106000180222             // - ?F12=Ritorno?
106100180220             When  dsp_aid = c_F12;
106200180220               $Video = 'S2';
106300180220               leavesr;
106400180220
106500180222             // - ?Invio?
106600180220             When  dsp_aid <> c_F06;
106700180220               leavesr;
106800180220
106900180220           EndSl;
107000180220
107100180220         ENDIF;
107200180220
107300180222         // - ?F6=Aggiornamento TNTBE00F?
107400180220         exsr  sr_UpdTNTBE;
107500180220
107600180222         If  $ErrGenerico = *on;
107700180222           // - ?Rilevato errore in fase di aggiornamento?
107800180220           leavesr;
107900180220         Else;
108000180222           // - ?Videata iniziale altrimenti?
108100180220           $Video  = 'S1';
108200180220           $InzS01 = $Immissione;
108300180220           if  not  $Immissione;
108400180220             if  TBEatb  = 'A';
108500180220               S1TBEann = 'Ann';
108600180220             else;
108700180220               clear  S1TBEann;
108800180220             endif;
108900180220           endif;
109000180220         EndIf;
109100180220
109200180220       ENDSR;
109300180220
109400180220       //--------------------------------------------------------------
109500180222       //? Inizializzazione videata W01.                                ?
109600180220       //--------------------------------------------------------------
109700180220       BEGSR  sr_InzW01;
109800180220
109900180222         clear  TB47W01;
110000180220
110100180220         select;
110200180222           // - ?Opz.4=Ripristino?
110300180220           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = 'A';
110400180220             W1ftt = TBEftt;
110500180222           // - ?Opz.4=Annullamento?
110600180220           when  S1Copz = 4  and  %found(TNTBE01L)  and  TBEatb = *blank;
110700180220             W1ftt = TBEftt;
110800180222           // - ?Opz.2=Modifica?
110900180220           when  S1Copz = 2  and  %found(TNTBE01L);
111000180220             W1ftt = TBEftt;
111100180222           // - ?F10=Immissione?
111200180220           when  $Immissione  and  NOT %found(TNTBE01L);
111300180220             W1ftt = TBXftt;
111400180220         endsl;
111500180220
111600180222         // - ?Se NON immissione:?
111700180222         //   ?visualizza i dati relativi all'ultima trasmissione?
111800180220         if  %found(TNTBE01L);
111900180220           W1flt  = TBEflt;
112000180220           W1ftr  = TBEftr;
112100180220           if  TBEdtr <> *zero;
112200180220             wData_Eur = %date( TBEdtr : *iso );
112300180220             W1dtr     = %dec( wData_Eur );
112400180220           endif;
112500180220         endif;
112600180220
112700180220       ENDSR;
112800180220
112900180220       //--------------------------------------------------------------
113000180222       //? Aggiornamento record TNTBE00F (tab. "ETF").                  ?
113100180220       //--------------------------------------------------------------
113200180220       BEGSR  sr_UpdTNTBE;
113300180220
113400180222         // - ?Impostazione dei campi chiave SE immissione?
113500180220         if  $Immissione;
113600180222           clear  keyTNTBE01;
113700180220           k_TBEcod = c_Tab;
113800180222           k_TBEke1 = %editc( C02lna : 'X' );
113900180222           //*·k_TBEke2 = *blank;
114000180222           //*·k_TBElin = TBXlin;
114100180222           //*·k_TBEsif = TBXsif;
114200180222           chain  %kds( keyTNTBE01 )  TNTBE000;
114300180220         endif;
114400180220
114500180222         // - ?Impostazione dati (se NON annullamento)?
114600180220         if  S1Copz <> 4;
114700180220           TBEapl = TBXapl;
114800180222           TBEuni = ds_ETF;
114900180220         endif;
115000180220
115100180220         TBEftt = W1ftt;
115200180220         TBEflt = W1flt;
115300180220         clear TBEftr;
115400180220         //clear TBEdtr;
115500180220
115600180220         select;
115700180222           // - ?Opz.4=Ripristino?
115800180220           when  S1Copz = 4  and  TBEatb <> *blank;
115900180220             clear  TBEatb;
116000180222           // - ?Opz.4=Annullamento?
116100180220           when  S1Copz = 4  and  TBEatb = *blank;
116200180220             TBEatb = 'A';
116300180220         endsl;
116400180220
116500180220         IF  NOT  %found(TNTBE01L);
116600180220           clear  TBEsif;
116700180220           clear  TBElin;
116800180220           TBEcod = c_Tab;
116900180222           TBEke1 = %editc( C02lna : 'X' );
117000180222           clear  TBEke2;
117100180220           clear  TBEdtr;
117200180220           //_________________
117300180220           WRITE(e)  TNTBE000;
117400180220           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117500180220           if  %error;
117600180222             $ErrGenerico = *on;
117700180222             $ErrMessage  = *on;
117800180222             $PosCurLNA   = *on;
117900180222             VIDmsg = sk_Msg(06);
118000180220             leavesr;
118100180220           endif;
118200180220         ELSE;
118300180220           //_______________
118400180220           UPDATE  TNTBE000;
118500180220           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118600180220         ENDIF;
118700180220
118800180220       ENDSR;
118900180220
119000180220       //--------------------------------------------------------------
119100180222       //? Operazioni finali.                                           ?
119200180220       //--------------------------------------------------------------
119300180220       BEGSR  sr_RoutEnd;
119400180220
119500180223         // - ?Restituzione parametri?
119600180223         //   ?(se selezionata una Filiale)?
119700180223         If  wMemoSel > *zero;
119800180223           chain  SavS01csr  TB47S01;
119900180223           tntb47ds.o47lna  = %int( S1tbeKE1 );
120000180223           tntb47ds.o47uni  = ds_ETF;
120100180223         EndIf;
120200180223
120300180223         kpjbu = TNTB47ds;
120400180220
120500180223         // - ?Uscita?
120600180220         return;
120700180220
120800180220       ENDSR;
120900180220
121000180220      /end-free
121100180220
121200180222** - ?sk_Msg: ?Messaggi di Errore? ------------------------------------------*
121300180220Opzione errata                                                                  1
121400180220Record annullato: usare l'opzione 4 per ripristinarlo e modificarlo             2
121500180222Linea di Arrivo mancante o errata                                               3
121600180222Linea di Arrivo già presente in tabella                                         4
121700180222Filiale di Transito Terminal mancante o errata                                  5
121800180222Filiale di Transito Terminal già indicata                                       6
121900180223Selezionate più di una Filiale                                                  7
