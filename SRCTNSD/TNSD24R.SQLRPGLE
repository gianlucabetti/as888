000100180104       //==============================================================
000200180104       //? Ricerca Codici ORGANIGRAMMA.                                 ?
000300180104       //==============================================================
000400180104
000500180104       //--------------------------------------------------------------
000600180104       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180104       //--------------------------------------------------------------
000800180104
000900180104     /*PRM  dbgview(*source)
001000180104     /*END
001100180104
001200180104       //--------------------------------------------------------------
001300180104       //? Specifiche di controllo.                                     ?
001400180104       //--------------------------------------------------------------
001500180104
001600180104     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700180104     h alwnull(*inputonly)
001800180104     h dftactgrp(*no)
001900180104     h bnddir('TRUL')
002000180104
002100180104       //--------------------------------------------------------------
002200180104       //? Dichiarazione file.                                          ?
002300180104       //--------------------------------------------------------------
002400180104
002500180104       // - ?Video?
002600180104     fTNSD24D   cf   e             workstn
002700180104     f                                     sfile( SD24S01 : S01nrr )
002800180104     f                                     indds( IndDspF )
002900180104     f                                     infds( InfDspF )
003000180104
003100180104       //--------------------------------------------------------------
003200180104       //? Definizione costanti.                                        ?
003300180104       //--------------------------------------------------------------
003400180104
003500180104       // - ?Numero di record in ciascuna videata di subfile?
003600180104     d c_SflPag        c                   const(15)
003700180104
003800180104       // - ?Numero massimo di record gestibili?
003900180104     d c_MaxRec        c                   const(9999)
004000180104
004100180104       // - ?Tasti funzionali a video?
004200180104     d c_F01           c                   const(x'31')
004300180104     d c_F02           c                   const(x'32')
004400180104     d c_F03           c                   const(x'33')
004500180104     d c_F04           c                   const(x'34')
004600180104     d c_F05           c                   const(x'35')
004700180104     d c_F06           c                   const(x'36')
004800180104     d c_F07           c                   const(x'37')
004900180104     d c_F08           c                   const(x'38')
005000180104     d c_F09           c                   const(x'39')
005100180104     d c_F10           c                   const(x'3A')
005200180104     d c_F11           c                   const(x'3B')
005300180104     d c_F12           c                   const(x'3C')
005400180104     d c_F13           c                   const(x'B1')
005500180104     d c_F14           c                   const(x'B2')
005600180104     d c_F15           c                   const(x'B3')
005700180104     d c_F16           c                   const(x'B4')
005800180104     d c_F17           c                   const(x'B5')
005900180104     d c_F18           c                   const(x'B6')
006000180104     d c_F19           c                   const(x'B7')
006100180104     d c_F20           c                   const(x'B8')
006200180104     d c_F21           c                   const(x'B9')
006300180104     d c_F22           c                   const(x'BA')
006400180104     d c_F23           c                   const(x'BB')
006500180104     d c_F24           c                   const(x'BC')
006600180104     d c_Enter         c                   const(x'F1')
006700180104     d c_RollDown      c                   const(x'F4')
006800180104     d c_RollUp        c                   const(x'F5')
006900180104
007000180104       //--------------------------------------------------------------
007100180104       //? Definizione schiere.                                         ?
007200180104       //--------------------------------------------------------------
007300180104
007400180104       // - ?Messaggi di errore?
007500180109     d sk_Msg          s             78    dim(04)  ctdata  perrcd( 1)
007600180104
007700180104       // - ?Filiali in gestione?
007800180104     d sk_POg          s              3    dim(250)  inz
007900180104
008000180104       //--------------------------------------------------------------
008100180104       //? Definizione aree dati.                                       ?
008200180104       //--------------------------------------------------------------
008300180104
008400180104       // - ?Dati utente?
008500180104     d §AzUte        e ds                  extname(AZUTE00F)
008600180104     d                                     dtaara
008700180104     d §DatiUte      e ds                  extname(dDatiUte)
008800180104     d                                     dtaara
008900180104
009000180104       //--------------------------------------------------------------
009100180104       //? Definizione strutture dati.                                  ?
009200180104       //--------------------------------------------------------------
009300180104
009400180104       // - ?Status ds?
009500180104     d Status         sds
009600180104     d   SDSpgmName      *proc
009700180104
009800180104       // - ?InfDS?
009900180104     d InfDspF         ds
010000180104     d   dsp_aid             369    369a
010100180104     d*//sfl_rrn             376    377i 0
010200180104     d*//min_nrr             378    379i 0
010300180104     d*//num_rcds            380    381i 0
010400180104
010500180104       // - ?Indicatori su DspF?
010600180104     d IndDspF         ds                  inz
010700180104         // - ?Emissione messaggio di errore?
010800180104     d   $ErrMessage                   n   overlay( IndDspF : 28 )
010900180104         // - ?Indicatori di gestione del subfile?
011000180104     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
011100180104     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
011200180104     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
011300180104     d   $SflEnd                       n   overlay( IndDspF : 33 )
011400180104         // - ?Indicatori per Attributi di visualizzazione?
011500180104     d   $Ord_x_Cod                    n   overlay( IndDspF : 40 )
011600180104         // - ?Posizionamento cursore & segnalazione errore?
011700180104     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
011800180109     d   $PosCurNTW                    n   overlay( IndDspF : 51 )
011900180104         // - ?Riemissione videata?
012000180104     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
012100180104
012200180104       // - ?Parametri Gestione?
012300180104     d KPJBA         e ds                  inz
012400180104     d wKPJBA        e ds                  extname(KPJBA)
012500180104     d                                     qualified
012600180104
012700180104       // - ?Dati estratti via SQL?
012800180104     d AZORG_ds      e ds                  extname(AZORG00F)
012900180104     d                                     inz
013000180104
013100180104       // - ?143ª descrizione dell'Organigramma?
013200180104     d Og143         e ds                  inz
013300180104     d                                     qualified
013400180104
013500180104       //--------------------------------------------------------------
013600180104       //? Definizione variabili globali.                               ?
013700180104       //--------------------------------------------------------------
013800180104
013900180104       // - ?Flags booleani?
014000180104     d $EoF            s               n   inz
014100180104     d $Fine           s               n   inz
014200180104     d $InzS01         s               n   inz(*on)
014300180104     d $Where          s               n   inz
014400180104
014500180104       // - ?Indici di schiera?
014600180104     d xx              s              5  0 inz
014700180104
014800180104       // - ?Variabili per la gestione del video?
014900180104     d $Video          s              2    inz('S1')
015000180104     d S01nrr          s                   like(C1RcdNbr) inz
015100180104     d SavS01csr       s                   like(C1RcdNbr) inz
015200180104
015300180104       // - ?Stringa SQL da eseguire?
015400180104     d wSQL            s          32740    varying        inz
015500180104
015600180104       // - ?Altri dati legati all'SQL?
015700180104     d wFil            s          32740    varying        inz
015800180108     d*//· wAZORG_rrn      s                   like(S1Hnrr)   inz
015900180104
016000180104       // - ?Variabili per la gestione del posizionamento nel subfile?
016100180104     d SavC1Cntw       s                   like(C1Cntw)   inz
016200180104     d SavC1Cpou       s                   like(C1Cpou)   inz
016300180104     d SavC1Cdes       s                   like(C1Cdes)   inz
016400180104
016500180104       // - ?Campi di comodo?
016600180104     d wDate           s              8s 0 inz
016700180104     d wTime           s              4s 0 inz
016800180104
016900180104       //--------------------------------------------------------------
017000180104       //? Definizione prototipi procedure.                             ?
017100180104       //--------------------------------------------------------------
017200180104
017300180104       // - ?Reperimento dati utente?
017400180104     d TIBS34ds      e ds
017500180104      /copy gaitrasrc/srcProtoPR,TIBS34R
017600180104
017700180104       // - ?Caricamento filiali gestire dall'utente?
017800180104     d TRUL31ds      e ds                  qualified      inz
017900180104      /copy gaitrasrc/srcProtoPR,TRUL31R
018000180104     d wFIL_ds         ds                                 inz
018100180104     d   sk_Fil                1    750    dim(250)       inz
018200180104
018300180104       // - ?Controllo profili utente?
018400180104     d TIBS36ds      e ds                  qualified      inz
018500180104     d tibs36r         pr                  extpgm('TIBS36R')
018600180104     d   kpjba                             likeds(TIBS36ds)
018700180109
018800180109       // - ?Ricerca/Controllo Tabelle?
018900180109     d TIBS02ds      e ds                  qualified      inz
019000180109      /copy gaitrasrc/srcProtoPR,TIBS02R
019100180104
019200180104       // - ?Parametri API QCAPCMD (Process Commands)?
019300180104     d Qcmd            s          32740    varying        inz
019400180104      /copy qSysInc/qRpgleSrc,QCAPCMD
019500180104       // - ?API QCAPCMD (Process Commands)?
019600180104      /copy gaitrasrc/srcProtoPR,QCAPCMD
019700180104
019800180104       // - ?Parametri gestione errori API.?
019900180104      /copy qsysinc/qrpglesrc,QUSEC
020000180104
020100180104       //--------------------------------------------------------------
020200180104       //? Definizione key-list.                                        ?
020300180104       //--------------------------------------------------------------
020400180104
020500180104
020600180104       //--------------------------------------------------------------
020700180104       //? Riepilogo indicatori utilizzati.                             ?
020800180104       //--------------------------------------------------------------
020900180104
021000180104
021100180104       //--------------------------------------------------------------
021200180104       //? M A I N - L I N E                                            ?
021300180104       //--------------------------------------------------------------
021400180104
021500180104     c     *Entry        plist
021600180104     c                   parm                    §Cod              3
021700180104     c                   parm                    §Tip              1
021800180104     c                   parm                    §Des             25
021900180104     c                   parm                    §Pra              3
022000180104     c                   parm                    §Abi              2
022100180104     c                   parm                    wKPJBA
022200180104
022300180104      /free
022400180104
022500180104       // - ?Operazioni iniziali?
022600180104       exsr  sr_RoutInz;
022700180104
022800180104       // - ?Ciclo di gestione del file video?
022900180104       DOW  $Fine = *off;
023000180104
023100180104         select;
023200180104
023300180104           // - ?Gestione videata S1 (subfile)?
023400180104           when $Video = 'S1';
023500180104             exsr sr_GesS01;
023600180104
023700180104           // - ?? ? ??
023800180104           other;
023900180104             $Fine = *on;
024000180104
024100180104         endsl;
024200180104
024300180104       ENDDO;
024400180104
024500180104       // - ?Operazioni finali?
024600180104       exsr  sr_RoutEnd;
024700180104
024800180104       //--------------------------------------------------------------
024900180104       //? Operazioni iniziali.                                         ?
025000180104       //--------------------------------------------------------------
025100180104       BEGSR  sr_RoutInz;
025200180104
025300180104         // - ?Impostazione opzioni per SQL?
025400180104         exec sql   set  option  DynUsrPrf = *Owner,
025500180104                                 CloSqlCsr = *EndMod;
025600180104
025700180104         // - ?Impostazione chiusura?
025800180104         *inLR = *on;
025900180104
026000180104         // - ?Reperimento dati job?
026100180104         exsr  sr_DatiJob;
026200180104
026300180104         // - ?Reperimento parametri?
026400180104
026500180104         // - ?Impostazione nome programma a video?
026600180104         V1Tpgm = SDSpgmName;
026700180104
026800180104         // - ?Impostazione videata iniziale?
026900180104         $Video = 'S1';
027000180104         reset  $InzS01;
027100180108
027200180108         select;
027300180108           when  §Tip = *blank  and  §Des = *blank;
027400180108             C1Cdes = 'A ';
027500180108           when  §Tip = *blank;
027600180108             C1Cdes = §Des;
027700180108         endsl;
027800180108         SavC1Cdes  = C1Cdes;
027900180108         clear  C1Cpou;
028000180108         clear  C1Cntw;
028100180104
028200180108         // - ?Pulizia campi di output - NON qui!!!?
028300180108         //*-clear  §Cod;
028400180108         //*-clear  §Tip;
028500180108         //*-clear  §Des;
028600180104
028700180104         // - ?Verifica abilitazione?
028800180104         If  %parms() > 4;
028900180104
029000180104           KPJBA = wKPJBA;
029100180104
029200180104           // - ?Reperimento filiale dell'utente?
029300180104           clear  TIBS36ds;
029400180104           TIBS36ds.I36ute = kNmUs;
029500180104           TIBS36ds.I36tla = 'L';
029600180104           TIBS36R ( TIBS36ds );
029700180104
029800180104           // - ?Reperimento filiali gestibili dall'utente?
029900180104           clear  TRUL31ds;
030000180104           TRUL31ds.I31abi = §Abi;
030100180104           TRUL31ds.I31cpo = TIBS36ds.O36pou;
030200180104           TRUL31R ( KPJBA : TRUL31ds );
030300180104           if  TRUL31ds.O31pog > *zero;
030400180104             wFIL_ds = TRUL31ds.O31pog;
030500180104           endif;
030600180104
030700180104           clear  wFil;
030800180104           For  xx = 1  To  %elem( sk_Fil );
030900180104             if  sk_Fil(xx) <> *zero;
031000180104               if  wFil = *blank;
031100180104                 wFil += '(';
031200180104               else;
031300180104                 wFil += ', ';
031400180104               endif;
031500180104               wFil += sk_Fil(xx);
031600180104             endif;
031700180104           EndFor;
031800180104           if  wFIl <> *blank;
031900180104             wFil += ')';
032000180104           endif;
032100180104
032200180104         Else;
032300180104
032400180104           clear  KPJBA;
032500180104
032600180104         EndIf;
032700180104
032800180104       ENDSR;
032900180104
033000180104       //--------------------------------------------------------------
033100180104       //? Reperimento Dati del job (Utente/Operativi).                 ?
033200180104       //--------------------------------------------------------------
033300180104       BEGSR  sr_DatiJob;
033400180104
033500180104         in(E) §AzUte;
033600180104         if NOT %error;
033700180104           in(E) §DatiUte;
033800180104         endif;
033900180104         if %error or RSut = *blanks;
034000180104           clear TIBS34ds;
034100180104           tibs34r ( tibs34ds );
034200180104           in §AzUte;
034300180104           in §DatiUte;
034400180104         endif;
034500180104
034600180104       ENDSR;
034700180104
034800180104       //--------------------------------------------------------------
034900180104       //? Gestione subfile S01.                                        ?
035000180104       //--------------------------------------------------------------
035100180104       BEGSR  sr_GesS01;
035200180104
035300180104         // - ?Inizializzazione videata?
035400180104         if  $InzS01 = *on;
035500180104           exsr  sr_InzS01;
035600180104           $InzS01 = *off;
035700180104         endif;
035800180104
035900180104         // - ?Emissione Testata e Piede con tasti funzionali abilitati?
036000180104         write  SD24T01;
036100180104         write  SD24P01;
036200180104
036300180104         // - ?Posizionamento cursore?
036400180104         //   ?C1CsrRrn contiene il numero di riga del subfile su cui?
036500180104         //   ?era posizionato il cursore; impostandolo in C1RcdNbr?
036600180104         //   ?si visualizza la pagina che vedeva l'utente quando ha?
036700180104         //   ?premuto l'ultimo tasto.?
036800180104         if  C1CsrRrn > *zero;
036900180104           C1RcdNbr = C1CsrRrn;
037000180104         else;
037100180104           C1RcdNbr = 1;
037200180104           write  SD24S00;     //?(no rec.)?
037300180104         endif;
037400180104
037500180104         // - ?Emissione videata con subfile?
037600180104         exfmt  SD24C01;
037700180104
037800180104         clear  VIDmsg;
037900180104         reset  $ErrMessage;
038000180104         reset  $ErrGenerico;
038100180104
038200180104         SELECT;
038300180104
038400180104           // - ?F12=Ritorno?
038500180104           WHEN  dsp_aid = c_F12;
038600180108             clear  §Cod;
038700180108             clear  §Tip;
038800180108             clear  §Des;
038900180104             $Fine   = *on;
039000180104
039100180104           // - ?F11=Ordinamento per ...?
039200180104           WHEN  dsp_aid = c_F11;
039300180108             //*·clear  C1Cpou;
039400180108             //*·clear  C1Cdes;
039500180104             if  NOT $Ord_x_Cod;
039600180104               $Ord_x_Cod = *on;
039700180104             else;
039800180104               $Ord_x_Cod = *off;
039900180108               //*·select;
040000180108               //*·  when  §Tip = *blank  and  §Des = *blank;
040100180108               //*·    C1Cdes = 'A ';
040200180108               //*·  when  §Tip = *blank;
040300180108               //*·    C1Cdes = §Des;
040400180108               //*·endsl;
040500180104             endif;
040600180108             SavC1Cntw = C1Cntw;
040700180108             SavC1Cpou = C1Cpou;
040800180108             SavC1Cdes = C1Cdes;
040900180104             $InzS01 = *on;
041000180104
041100180104           // - ?Invio?
041200180104           OTHER;
041300180104             // - ?Gestione opzioni?
041400180104             if  S01nrr > *zero;
041500180104               exsr  sr_OpzS01;
041600180104             endif;
041700180104             if  $ErrGenerico;
041800180104               leavesr;
041900180104             endif;
042000180109             // - ?Ricerca/Controllo Network?
042100180109             IF  C1Cntw <> *blank;
042200180109               If  %scan( '?' : C1Cntw ) > *zero;
042300180109                 clear  C1Cntw;
042400180109                 clear  TIBS02ds;
042500180109                 TIBS02ds.T02mod = 'R';
042600180109                 TIBS02ds.T02cod = 'NTW';
042700180109                 TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
042800180109                 if  TIBS02ds.T02err = *blank;
042900180109                   C1Cntw = TIBS02ds.T02ke1;
043000180109                   //*·dNTW   = T02uni;
043100180109                 endif;
043200180109                 $PosCurNTW   = *on;
043300180109                 $ErrGenerico = *on;
043400180109               EndIf;
043500180109               If  C1Cntw <> *blank;
043600180109                 clear  TIBS02ds;
043700180109                 TIBS02ds.T02mod = 'C';
043800180109                 TIBS02ds.T02cod = 'NTW';
043900180109                 TIBS02ds.T02ke1 = C1Cntw;
044000180109                 TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
044100180109                 if  TIBS02ds.T02err <> *blank;
044200180109                   $ErrGenerico = *on;
044300180109                   $ErrMessage  = *on;
044400180109                   $PosCurNTW   = *on;
044500180109                   VIDmsg = sk_Msg(04);
044600180109                 endif;
044700180109               EndIf;
044800180109               if  $ErrGenerico = *on;
044900180109                 leavesr;
045000180109               endif;
045100180109               //*·dNTW   = T02uni;
045200180109             ENDIF;
045300180104             // - ?Richiesto Posizionamento e/o Selezione?
045400180104             if  C1Cntw <> SavC1Cntw  or
045500180104                 C1Cpou <> SavC1Cpou  or
045600180104                 C1Cdes <> SavC1Cdes;
045700180104               $InzS01 = *on;
045800180104               SavC1Cntw = C1Cntw;
045900180104               SavC1Cpou = C1Cpou;
046000180104               SavC1Cdes = C1Cdes;
046100180104             endif;
046200180104
046300180104         ENDSL;
046400180104
046500180104       ENDSR;
046600180104
046700180104       //--------------------------------------------------------------
046800180104       //? Inizializzazione subfile S01.                                ?
046900180104       //--------------------------------------------------------------
047000180104       BEGSR  sr_InzS01;
047100180104
047200180104         // - ?Spegnimento degli indicatori di errore?
047300180104         %subst( IndDspF : 50 ) = *off;
047400180104
047500180104         // - ?Pulizia subfile?
047600180104         $SflDsp_N    = *on;
047700180104         $SflDspCtl_N = *on;
047800180104         write  SD24C01;
047900180104         $SflDspCtl_N = *off;
048000180104         $SflEnd      = *off;
048100180104
048200180104         clear  C1RcdNbr;
048300180104         clear  C1CsrRrn;
048400180104         clear  S01nrr;
048500180104         clear  VIDmsg;
048600180104         $ErrMessage  = *off;
048700180104         $ErrGenerico = *off;
048800180104
048900180104         $SflNxtChg = *off;
049000180104
049100180104         // - ?Caricamento completo dei dati nel subfile?
049200180104         //   ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
049300180104         //   ?l'eventuale ordinamento)?
049400180104
049500180104         // - ?Preparazione stringa SQL per estrazione dati?
049600180104         exsr  sr_PrepSQL;
049700180104
049800180104         // - ?Apertura Cursore?
049900180104         exsr  sr_OpenCursor;
050000180104
050100180104         // - ?Ciclo di elaborazione?
050200180104         DoW  $EoF = *off;
050300180104           exsr  sr_ReadCursor;
050400180104         EndDo;
050500180104
050600180104         // - ?Chiusura Cursore?
050700180104         exsr  sr_CloseCursor;
050800180104
050900180104         // - ?Visualizzazione del SFL (se ci sono dati)?
051000180104         $SflDsp_N = ( S01nrr <= *zero );
051100180104
051200180104         // - ?Attivazione del SFLEND?
051300180104         $SflEnd   = $EoF;
051400180104
051500180104         // - ?Posizionamento cursore al 1° rcd della 1ª pagina?
051600180104         if  S01nrr > *zero;
051700180104           C1RcdNbr = 1;
051800180104         else;
051900180104           clear  C1RcdNbr;
052000180104         endif;
052100180104         C1CsrRrn = C1RcdNbr;
052200180104
052300180104         // - ?Emissione messaggio di segnalazione raggiungimento limite?
052400180104         if  S01nrr >= c_MaxRec   and   Not $EoF;
052500180104           $ErrGenerico = *on;
052600180104           $ErrMessage  = *on;
052700180104           $PosCurOPZ   = *on;
052800180104           VIDmsg = sk_Msg(02);
052900180104           leavesr;
053000180104         endif;
053100180104
053200180104       ENDSR;
053300180104
053400180104       //--------------------------------------------------------------
053500180104       //? Gestione opzioni del subfile S01                             ?
053600180104       //--------------------------------------------------------------
053700180104       BEGSR  sr_OpzS01;
053800180104
053900180104         // - ?Spegnimento degli indicatori di errore?
054000180104         %subst( IndDspF : 50 ) = *off;
054100180104
054200180104         clear  SavS01csr;
054300180104
054400180104         // - ?Ciclo di lettura subfile?
054500180104         readc  SD24S01;
054600180104
054700180104         DoW  Not %eof(TNSD24D);
054800180104
054900180104           $SflNxtChg = *off;
055000180104           %subst( IndDspF : 50 ) = *off;
055100180104           C1RcdNbr   = S01nrr;
055200180104
055300180104           Select;
055400180104
055500180104             // - ?Nessuna opzione?
055600180104             When  S01opz = *blank;
055700180104
055800180104             // - ?Opz. 1=Selezione?
055900180104             When  S01opz  = '1';
056000180104               §Cod  = %editc( S01fil : 'X' );
056100180104               §Tip  = S01fag;
056200180104               §Des  = S01des;
056300180104               $Fine = *on;
056400180104               $ErrGenerico = *on;
056500180104
056600180104             // - ?Opzione errata?
056700180104             Other;
056800180104               $ErrGenerico = *on;
056900180104               $ErrMessage  = *on;
057000180104               $PosCurOPZ   = *on;
057100180104               VIDmsg = sk_Msg(03);
057200180104
057300180104           EndSl;
057400180104
057500180104           // - ?Memorizzazione nrr del sfl con la 1ª opzione per?
057600180104           //   ?riposizionarvici il cursore dopo il tasto "Invio"?
057700180104           if  S01opz   <> *blank  and
057800180104              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
057900180104             SavS01csr   = C1RcdNbr;
058000180104           endif;
058100180104
058200180104           // - ?Aggiornamento sfl?
058300180104           if  $ErrGenerico;
058400180104             C1CsrRrn   = C1RcdNbr;
058500180104             $SflNxtChg = *on;
058600180104             //*·clear  S1Copz;
058700180104           else;
058800180104             clear  S01opz;
058900180104           endif;
059000180104
059100180104           UPDATE  SD24S01;
059200180104
059300180104           if  $ErrGenerico  or  $ErrMessage;
059400180104             leave;
059500180104           endif;
059600180104
059700180104           readc  SD24S01;
059800180104
059900180104         ENDDO;
060000180104
060100180104         // - ?Riposizionam. cursore sul record contenente la prima opz.?
060200180104         //   ?(se non sono stati rilevati errori)?
060300180104         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
060400180104             SavS01csr > *zero;
060500180104           C1CsrRrn = SavS01csr;
060600180104         endif;
060700180104
060800180104       ENDSR;
060900180104
061000180104       //--------------------------------------------------------------
061100180104       //? Preparazione stringa SQL - per estrazione dati da AZORG/AZOSE?
061200180104       //--------------------------------------------------------------
061300180104       BEGSR  sr_PrepSQL;
061400180104
061500180104         clear  $Where;
061600180104
061700180104         if  §Tip = '§';
061800180104           wSQL  =
061900180108                 'select AZOSE00F.* +
062000180104                    from AZOSE00F';
062100180104         else;
062200180104           wSQL  =
062300180108                 'select AZORG00F.* +
062400180104                    from AZORG00F';
062500180104         endif;
062600180104
062700180104         // - ?Selezione per Filiale (a video)?
062800180104         if  $Ord_x_Cod  and  C1Cpou <> *zero;
062900180104           exsr  sr_Set_Where_And;
063000180104           wSQL +=     ' ORGfil >= ' + %editc( C1Cpou : 'X' );
063100180104         endif;
063200180104
063300180104         // - ?Selezione per Descrizione (a video)?
063400180104         if  NOT $Ord_x_Cod  and  C1Cdes <> *blank;
063500180104           exsr  sr_Set_Where_And;
063600180104           wSQL +=     ' ORGdes >= ''' + C1Cdes + '''';
063700180104         endif;
063800180104
063900180104         // - ?Selezione per Network (a video)?
064000180104         if  C1Cntw <> *blank;
064100180104           exsr  sr_Set_Where_And;
064200180104           wSQL +=     ' substr( ORGde3, 11, 3 ) = ''' + C1Cntw + '''';
064300180104         endif;
064400180104
064500180109         // - ?Selezioni in base alla filiale ricevuta come parametro?
064600180104         if  §Tip <> *blank  and  %parms() > 4;
064700180104           exsr  sr_Set_Where_And;
064800180104           wSQL +=     ' ORGfil in ' + %trimR( wFIL );
064900180104         endif;
065000180104
065100180104         Select;
065200180104
065300180104           // - ?Filiale?
065400180104           When  §Tip = 'F';
065500180104             exsr  sr_Set_Where_And;
065600180104             wSQL +=   ' ORGfag = ''F''';
065700180104
065800180104           // - ?Agenzia?
065900180104           When  §Tip = 'A';
066000180104             exsr  sr_Set_Where_And;
066100180104             wSQL +=   ' ORGfag = ''A''';
066200180104
066300180104           // - ?Agenzia/Filiale?
066400180104           When  §Tip = 'E';
066500180104             exsr  sr_Set_Where_And;
066600180104             wSQL +=   ' ORGfag in (''F'', ''A'')';
066700180104
066800180104           // - ?Varie?
066900180104           When  §Tip = 'V';
067000180104             exsr  sr_Set_Where_And;
067100180104             wSQL +=   ' ORGfag = ''V'' and ORGfl2 = '' ''';
067200180104
067300180104           // - ?Linea per Tariffe?
067400180104           When  §Tip = 'T';
067500180104             exsr  sr_Set_Where_And;
067600180104             wSQL +=   ' ORGfag = ''V'' and ORGfl2 = ''S''';
067700180104
067800180104           // - ?Varie + Linea per Tariffe?
067900180104           When  §Tip = 'W';
068000180104             exsr  sr_Set_Where_And;
068100180104             wSQL +=   ' ORGfag = ''V''';
068200180104
068300180104         EndSl;
068400180104
068500180104         // - ?Ordinamento?
068600180104         wSQL += ' order by';
068700180104         if  NOT $Ord_x_Cod;
068800180104           wSQL +=     ' ORGdes';
068900180104         else;
069000180104           wSQL +=     ' ORGfil';
069100180104         endif;
069200180104
069300180104         wSQL +=   ' for fetch only';
069400180104
069500180104       ENDSR;
069600180104
069700180104       //--------------------------------------------------------------
069800180104       //? Apertura cursore.                                            ?
069900180104       //--------------------------------------------------------------
070000180104       BEGSR  sr_Set_Where_And;
070100180104
070200180104         if  NOT $Where;
070300180104           $Where  =  *on;
070400180104           wSQL +=
070500180104                 ' where';
070600180104         else;
070700180104           wSQL +=
070800180104                   ' and';
070900180104         endif;
071000180104
071100180104       ENDSR;
071200180104
071300180104       //--------------------------------------------------------------
071400180104       //? Apertura cursore.                                            ?
071500180104       //--------------------------------------------------------------
071600180104       BEGSR  sr_OpenCursor;
071700180104
071800180104         $EoF  = *off;
071900180104
072000180104         // - ?Dichiarazione cursore?
072100180104         exec sql   prepare S1   from :wSQL;
072200180104         exec sql   declare C1   cursor for S1;
072300180104
072400180104         if  SQLcode < *zero;
072500180104           $EoF  = *on;
072600180104           exsr  sr_PrintERR;
072700180104           $ErrGenerico = *on;
072800180104           $ErrMessage  = *on;
072900180104           VIDmsg = sk_Msg(01);
073000180104           leavesr;
073100180104         endif;
073200180104
073300180104         // - ?Apertura del cursore?
073400180104         exec sql   open C1;
073500180104
073600180104         if  SQLcode < *zero;
073700180104           $EoF  = *on;
073800180104           exsr  sr_PrintERR;
073900180104           $ErrGenerico = *on;
074000180104           $ErrMessage  = *on;
074100180104           VIDmsg = sk_Msg(01);
074200180104           leavesr;
074300180104         endif;
074400180104
074500180104       ENDSR;
074600180104
074700180104       //--------------------------------------------------------------
074800180104       //? Chiusura cursore.                                            ?
074900180104       //--------------------------------------------------------------
075000180104       BEGSR  sr_CloseCursor;
075100180104
075200180104         // - ?Chiusura del cursore?
075300180104         exec sql   close C1;
075400180104
075500180104       ENDSR;
075600180104
075700180104       //--------------------------------------------------------------
075800180104       //? Lettura cursore.                                             ?
075900180104       //--------------------------------------------------------------
076000180104       BEGSR  sr_ReadCursor;
076100180104
076200180104         clear  AZORG_ds;
076300180104
076400180108         exec sql   fetch next   from C1   into :AZORG_ds;
076500180104
076600180104         Select;
076700180104
076800180104           // - ?E.o.F.?
076900180104           When  SQLcode = 100;
077000180104             $EoF = *on;
077100180104
077200180104           // - ?Errore?
077300180104           When  SQLcode < *zero;
077400180104             $EoF = *on;
077500180104             exsr  sr_PrintERR;
077600180104             $ErrGenerico = *on;
077700180104             $ErrMessage  = *on;
077800180104             VIDmsg = sk_Msg(01);
077900180104
078000180104           // - ?Elaborazione?
078100180104           Other;
078200180104             exsr  sr_WriteS01;
078300180104
078400180104         EndSl;
078500180104
078600180104       ENDSR;
078700180104
078800180104       //--------------------------------------------------------------
078900180104       //? Registrazione del singolo rec. nel subfile S01.              ?
079000180104       //--------------------------------------------------------------
079100180104       BEGSR  sr_WriteS01;
079200180104
079300180104         // - ?Impostazione campi nel record del subfile?
079400180104         clear  SD24S01;
079500180104
079600180108         //*·S1Hnrr   = wAZORG_rrn;
079700180104
079800180104         Og143  = ORGde3;
079900180104
080000180104         S01fag   = ORGfag;
080100180104         if  ORGfag = 'V'  and  ORGfl2 = 'S';
080200180104           S01fag = 'T';
080300180104         endif;
080400180104
080500180104         S01fil = ORGfil;
080600180104         S01des = ORGdes;
080700180104         S01ntw = Og143.§OGntw;
080800180104         S01fva = ORGfva;
080900180104
081000180104         // - ?Registrazione singolo record nel subfile?
081100180104         S01nrr += 1;
081200180104         write  SD24S01;
081300180104
081400180104       ENDSR;
081500180104
081600180104       //--------------------------------------------------------------
081700180104       //? Stampa segnalazione dell'errore rilevato via SQL.            ?
081800180104       //--------------------------------------------------------------
081900180104       BEGSR  sr_PrintERR;
082000180104
082100180104         // - ?Stampa del Dump?
082200180104         Dump(A);
082300180104
082400180104         // - ?Stampa del Job-Log?
082500180104         Qcmd = 'DSPJOBLOG job(*) output(*print)';
082600180104         exsr  sr_ExecCmd;
082700180104
082800180104       ENDSR;
082900180104
083000180104       //--------------------------------------------------------------
083100180104       //? Esecuzione del comando (già impostato).                      ?
083200180104       //--------------------------------------------------------------
083300180104       BEGSR  sr_ExecCmd;
083400180104
083500180104         clear Qcap0100;
083600180104         Qcabcsdh = *off;
083700180104         Qcapa    = *off;
083800180104         Qcacmdss = *off;
083900180104         Qcaerved = *allX'00';
084000180104
084100180104         clear Qusec;
084200180104         Qusbprv  = %size(Qusec);
084300180104
084400180104         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084500180104                           %size(Qcap0100) : 'CPOP0100' : *omit :
084600180104                           0 : 0 : Qusec);
084700180104
084800180104         //*·if  Qusei <> *blank;
084900180104         //*·  exsr  sr_PrintErr;
085000180104         //*·endif;
085100180104
085200180104       ENDSR;
085300180104
085400180104       //--------------------------------------------------------------
085500180104       //? Operazioni finali.                                           ?
085600180104       //--------------------------------------------------------------
085700180104       BEGSR  sr_RoutEnd;
085800180104
085900180104         // - ?Uscita?
086000180104         return;
086100180104
086200180104       ENDSR;
086300180104
086400180104      /end-free
086500180104
086600180104       //--------------------------------------------------------------
086700180104       //? Definizione schiere a tempo di compilazione.                 ?
086800180104       //--------------------------------------------------------------
086900180104
087000180104** -- ?sk_Msg:? Messaggi di Errore?--------------------------------------------*
087100180104Rilevato errore nel caricamento dei dati. Controllare stampa ed avvisare CED    1
087200180104Raggiunto il limite massimo dei record caricabili a video                       2
087300180104Opzione errata                                                                  3
087400180109Network errato                                                                  4
