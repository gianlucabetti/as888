000100120604       //===============================================================
000200120604       //?TNTBJDC1R * Selezione tab. "JDC" = Clienti x Ritorno Documenti.?
000300120604       //===============================================================
000400120326
000500120326       //--------------------------------------------------------------
000600120326       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120326       //--------------------------------------------------------------
000800120326
000900120326     /*PRM dbgview(*source)
001000120326     /*END
001100120326
001200120326       //--------------------------------------------------------------
001300120326       //?Specifiche di controllo.                                     ?
001400120326       //--------------------------------------------------------------
001500120326
001600120326     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700120326     h dftactgrp(*no)
001800120326
001900120326       //--------------------------------------------------------------
002000120326       //?Dichiarazione file.                                          ?
002100120326       //--------------------------------------------------------------
002200120326
002300120326       // -?Tabelle?
002400120326     fTNTBE01L  if   e           k disk
002500120326
002600120326       // -?Video?
002700120604     fTNTBJDC1D cf   e             workstn
002800120604     f                                     sfile(TBJDC1S01 : S01nrr)
002900120326     f                                     indds(IndDspF)
003000120326     f                                     infds(InfDspF)
003100120326
003200120326       //--------------------------------------------------------------
003300120326       //?Definizione costanti.                                        ?
003400120326       //--------------------------------------------------------------
003500120326
003600120326       // -?Codice tabella in gestione?
003700120604     d c_Tab           c                   const('JDC')
003800120326
003900120326       // -?Numero massimo di record gestibili?
004000120326     d c_MaxRec        c                   const(9999)
004100120326
004200120326       // -?Tasti funzionali a video?
004300120326     d c_F01           c                   const(x'31')
004400120326     d c_F02           c                   const(x'32')
004500120326     d c_F03           c                   const(x'33')
004600120326     d c_F04           c                   const(x'34')
004700120326     d c_F05           c                   const(x'35')
004800120326     d c_F06           c                   const(x'36')
004900120326     d c_F07           c                   const(x'37')
005000120326     d c_F08           c                   const(x'38')
005100120326     d c_F09           c                   const(x'39')
005200120326     d c_F10           c                   const(x'3A')
005300120326     d c_F11           c                   const(x'3B')
005400120326     d c_F12           c                   const(x'3C')
005500120326     d c_F13           c                   const(x'B1')
005600120326     d c_F14           c                   const(x'B2')
005700120326     d c_F15           c                   const(x'B3')
005800120326     d c_F16           c                   const(x'B4')
005900120326     d c_F17           c                   const(x'B5')
006000120326     d c_F18           c                   const(x'B6')
006100120326     d c_F19           c                   const(x'B7')
006200120326     d c_F20           c                   const(x'B8')
006300120326     d c_F21           c                   const(x'B9')
006400120326     d c_F22           c                   const(x'BA')
006500120326     d c_F23           c                   const(x'BB')
006600120326     d c_F24           c                   const(x'BC')
006700120326     d c_Enter         c                   const(x'F1')
006800120326     d c_RollDown      c                   const(x'F4')
006900120326     d c_RollUp        c                   const(x'F5')
007000120326
007100120326       //--------------------------------------------------------------
007200120326       //?Definizione schiere.                                         ?
007300120326       //--------------------------------------------------------------
007400120326
007500120326       // -?Messaggi di errore?
007600120326     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
007700120326
007800120326       //--------------------------------------------------------------
007900120326       //?Definizione aree dati.                                       ?
008000120326       //--------------------------------------------------------------
008100120326
008200120326       // -?Dati utente?
008300120326     d §AzUte        e ds                  extname(AZUTE00F)
008400120326     d                                     dtaara
008500120326     d §DatiUte      e ds                  extname(dDatiUte)
008600120326     d                                     dtaara
008700120326
008800120326       //--------------------------------------------------------------
008900120326       //?Definizione strutture dati.                                  ?
009000120326       //--------------------------------------------------------------
009100120326
009200120326       // -?Status ds?
009300120326     d Status         sds
009400120326     d   SDSpgm          *proc
009500120326
009600120326       // -?InfDS?
009700120326     d InfDspF         ds
009800120326     d   dsp_aid             369    369a
009900120326     d   sfl_rrn             376    377i 0
010000120326     d   min_nrr             378    379i 0
010100120326     d   num_rcds            380    381i 0
010200120326
010300120326       // -?Indicatori su DspF?
010400120326     d IndDspF         ds                  inz
010500120326         // -?Abilitazione tasti funzionali?
010600120606     d  F11Attivo                      n   overlay(IndDspF : 11)
010700120326         // -?Emissione messaggio di errore?
010800120326     d   ErrMessage                    n   overlay(IndDspF : 28)
010900120326         // -?Indicatori di gestione del subfile?
011000120326     d   SflDsp_N                      n   overlay(IndDspF : 30)
011100120326     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011200120326     d   SflNxtChg                     n   overlay(IndDspF : 32)
011300120326     d   SflEnd                        n   overlay(IndDspF : 33)
011400120326         // -?Indicatori per Attibuti di visualizzazione?
011500120326         //  ?o   Condizionamento visualizzazione campi?
011600120326     d   ProtectOPZ                    n   overlay(IndDspF : 41)
011700120326     d   Ord_x_KSU                     n   overlay(IndDspF : 42)
011800120326         // -?Posizionamento cursore & segnalazione errore?
011900120326     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012000120326         //   -?Riemissione videata?
012100120326     d   ErrGenerico                   n   overlay(IndDspF : 99)
012200120326
012300120326       // -?Parametri ricevuti?
012400120326     d KPJBA         e ds
012500120326     d Param01         ds
012600120326       // ·?Codice          7/0 In?
012700120326     d  P01cod                        7  0 inz
012800120326       // ·?Ordinamento     1   In   (*blank = Codice;?
012900120326       //                            ?"1" = Padre/Codice;)?
013000120326     d  P01ord                        1
013100120326       // ·?Padre           7/0 In?
013200120326     d  P01ksu                        7  0 inz
013300120326       // ·?Chiave tabella  7   Out?
013400120326     d  P01ke1                        7
013500120326       // ·?KE2            15   Out?
013600120326     d  P01ke2                       15
013700120326       // ·?Codice ritorno  1   Out  (tasto funzionale di uscita)?
013800120326     d  P01rit                        1
013900120326
014000120604       // -?Tab. JDC = Clienti per ritorno documenti
014100120604     d dJDC          e ds                  inz
014200120326
014300120326       //--------------------------------------------------------------
014400120326       //?Definizione variabili globali.                               ?
014500120326       //--------------------------------------------------------------
014600120326
014700120326       // -?Parametri (parziali) in Input?
014800120326     d P01opz3         s              1
014900120326
015000120326       // -?Flags booleani?
015100120326     d $Fine           s               n   inz
015200120326     d $EoF            s               n   inz
015300120326     d $InzS01         s               n   inz(*on)
015400120326     d $RecOK          s               n   inz
015500120327
015600120327       // -?Indici di schiera?
015700120327     d x2              s              2  0 inz
015800120326
015900120326       // -?Variabili per la gestione del video?
016000120326     d $Video          s              2    inz('S1')
016100120326     d S01nrr          s                   like(C1RcdNbr) inz
016200120326     d SavS01csr       s                   like(C1RcdNbr) inz
016300120326
016400120326       // -?Variabili per la gestione del singolo campo alla variazione?
016500120326       //  ?dello stesso?
016600120326     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
016700120326     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
016800120327     d SavW1Cdes       s                   like(W1Cdes)   inz
016900120326
017000120326       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
017100120326
017200120326       //--------------------------------------------------------------
017300120326       // -?Constants?
017400120326       //--------------------------------------------------------------
017500120326       // -?MaxKey - Maximum number of key fields allowed by this program?
017600120326     d MaxKey          c                   const(2)
017700120326       // -?Sort order:?
017800120326       //  ?1 = Sort in an ascending sequence?
017900120326       //  ?2 = Sort in a descending sequence?
018000120326     d Ascendente      c                   const(1)
018100120326     d Discendente     c                   const(2)
018200120326       // -?Key data type:?
018300120326       //  ? 0 = Signed binary?
018400120326       //  ? 2 = Signed zoned decimal?
018500120326       //  ? 3 = Signed packed decimal?
018600120326       //  ? 6 = Character with no national language sort sequence applied,?
018700120326       //  ?     if specified?
018800120326       //  ? 7 = Unsigned packed decimal?
018900120326       //  ?     All numbers will have the sign forced positive ('F'X).?
019000120326       //  ? 8 = Unsigned zoned decimal?
019100120326       //  ?     All numbers will have the sign forced positive ('F'X).?
019200120326       //  ? 9 = Unsigned binary?
019300120326       //  ?14 = Date in form of DD/MM/YY?
019400120326       //  ?15 = Date in form of DD.MM.YYYY?
019500120326     d Numero          c                   const(2)
019600120326     d Carattere       c                   const(6)
019700120326       //
019800120326     d Put             c                   const(1)
019900120326     d EndPut          c                   const(2)
020000120326     d Get             c                   const(3)
020100120326
020200120326       //--------------------------------------------------------------
020300120326       // -?Data Structures?
020400120326       //  ?SflRcd     - The entire subfile record to pass to the sort?
020500120326       //  ?QLGSCB     - The sort request block for the QLGSORT API?
020600120326       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
020700120326       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
020800120326       //  ?QUSEC      - Error structure for the QLGSORT API?
020900120326       //--------------------------------------------------------------
021000120326     d SflRcd          ds                  inz
021100120327     d  S1Copz                             inz
021200120326     d  S1Ccod                             inz
021300120326     d  S1Cdes                             inz
021400120326     d  S1Cksu                             inz
021500120326     d  S1Caut                             inz
021600120604     d  S1Ctpi                             inz
021700120604     d  S1Cfmi                             inz
021800120604     d  S1Cdir                             inz
021900120326     d  Selected                      1a   inz
022000120326      /copy QSYSINC/QRPGLESRC,QLGSORT
022100120326     d QLGKL                         16    dim(MaxKey)
022200120326     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
022300120326     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
022400120326     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
022500120326     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
022600120326      /copy QSYSINC/QRPGLESRC,QLGSRTIO
022700120326      /copy QSYSINC/QRPGLESRC,QUSEC
022800120326
022900120326       //--------------------------------------------------------------
023000120326       // -?Standalone fields?
023100120326       //  ?Nrr        - Relative record number for subfile?
023200120326       //  ?SizeList   - Size of list?
023300120326       //  ?ReturnSize - Size of list returned by sort API?
023400120326       //--------------------------------------------------------------
023500120326     d NotUsed         s             16a   inz
023600120326     d ReturnSize      s              9b 0 inz
023700120326     d SizeList        s              9b 0 inz
023800120326     d RrnLast         s              5i 0 inz
023900120326     d Nrr             s              5i 0 inz
024000120326
024100120326       //--------------------------------------------------------------
024200120326       //?Definizione prototipi procedure.                             ?
024300120326       //--------------------------------------------------------------
024400120326
024500120326       // -?Reperimento dati utente?
024600120326     d TIBS34ds      e ds                  inz
024700120326      /copy gaitrasrc/srcProtoPR,TIBS34R
024800120326
024900120326       // -?Ordinamento subfile?
025000120326      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025100120326
025200120326       //--------------------------------------------------------------
025300120326       //?Definizione key-list.                                        ?
025400120326       //--------------------------------------------------------------
025500120326
025600120326       // -?File TNTBE01L?
025700120326     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025800120326     d                                     prefix(k_)   inz
025900120326
026000120326       //--------------------------------------------------------------
026100120326       //?Riepilogo indicatori utilizzati.                             ?
026200120326       //--------------------------------------------------------------
026300120326
026400120326
026500120326       //--------------------------------------------------------------
026600120326       //?M A I N - L I N E                                            ?
026700120326       //--------------------------------------------------------------
026800120326
026900120326     c     *Entry        plist
027000120326     c                   parm                    KPJBA
027100120326     c                   parm                    P01opz3
027200120326
027300120326      /free
027400120326
027500120326       // -?Operazioni iniziali?
027600120326       exsr sr_RoutInz;
027700120326
027800120326       // -?Ciclo di gestione del file video?
027900120326       DoW  $Fine = *off;
028000120326         select;
028100120326           // -?Gestione videata S1 (subfile)?
028200120326           when $Video = 'S1';
028300120326             exsr sr_GesS01;
028400120326           other;
028500120326             $Fine = *on;
028600120326         endsl;
028700120326       enddo;
028800120326
028900120326       // -?Operazioni finali?
029000120326       exsr sr_RoutEnd;
029100120326
029200120326       //--------------------------------------------------------------
029300120326       //?Operazioni iniziali.                                         ?
029400120326       //--------------------------------------------------------------
029500120326       BEGSR  sr_RoutInz;
029600120327
029700120327         // -?Impostazione chiusura?
029800120327         *inLR = *on;
029900120327
030000120327         // -?Reperimento dati job?
030100120327         exsr  sr_DatiJob;
030200120327
030300120327         // -?Impostazione nome programma a video?
030400120327         V1Tpgm = SDSpgm;
030500120326
030600120327         // -?Impostazione parametri ricevuti?
030700120326         Param01 = kpjbu;
030800120327         clear  P01ke1;
030900120327         clear  P01ke2;
031000120326         clear  P01rit;
031100120326
031200120326         // -?Verifica se ricevuti altri parametri?
031300120326         if  %parms() > 1;
031400120326           C1Dopz = '1=Scelta, 3=Copia';
031500120326         else;
031600120326           C1Dopz = '1=Scelta';
031700120326         endif;
031800120326
031900120326         // -?Impostazione iniziale / pulizia dei campi chiave?
032000120327         Ord_x_KSU = (P01ord = '1');
032100120326         clear  k05tntbe01;
032200120326         k_TBEcod = c_Tab;
032300120606
032400120606         // -?Non attivo il tasto di ordinamento F11?
032500120606         F11Attivo = *off;
032600120326
032700120326       ENDSR;
032800120327
032900120327       //--------------------------------------------------------------
033000120327       //?Reperimento Dati del job (Utente/Operativi).                 ?
033100120327       //--------------------------------------------------------------
033200120327       BEGSR  sr_DatiJob;
033300120327
033400120327         in(e) §AzUte;
033500120327         if NOT %error;
033600120327           in(e) §DatiUte;
033700120327         endif;
033800120327         if %error or RSut = *blank;
033900120327           tibs34r ( tibs34ds );
034000120327           in §AzUte;
034100120327           in §DatiUte;
034200120327         endif;
034300120327
034400120327       ENDSR;
034500120326
034600120326       //--------------------------------------------------------------
034700120326       //?Gestione subfile S01.                                        ?
034800120326       //--------------------------------------------------------------
034900120326       BEGSR  sr_GesS01;
035000120326
035100120326         // -?Inizializzazione videata?
035200120326         if  $InzS01 = *on;
035300120326           exsr  sr_InzS01;
035400120326           $InzS01 = *off;
035500120326         endif;
035600120326
035700120326         // -?Emissione Testata & Piede?
035800120604         write  TBJDC1T01;
035900120604         write  TBJDC1P01;
036000120326
036100120326         // -?Posizionamento cursore?
036200120326         if  C1CsrRrn > *zero;
036300120326           C1RcdNbr = C1CsrRrn;
036400120326         else;
036500120326           C1RcdNbr = 1;
036600120604           write  TBJDC1S00;            //?(no rec.)?
036700120326         endif;
036800120326
036900120326         // -?Emissione videata?
037000120604         exfmt  TBJDC1C01;
037100120326
037200120326         reset  ErrMessage;
037300120326         reset  ErrGenerico;
037400120326         clear  V1Dmsg;
037500120326
037600120326         select;
037700120326
037800120326           // -?F3=Fine?
037900120326           when  dsp_aid = c_F03;
038000120326             P01rit = 'C';
038100120326             $Fine  = *on;
038200120327
038300120327           // -?F4=Ricerca per descrizione?
038400120327           when  dsp_aid = c_F04;
038500120327             exsr  sr_F04S01;
038600120326
038700120326           // -?F5=Rivisualizza?
038800120424           //when  dsp_aid = c_F05;
038900120424           //  $InzS01 = *on;
039000120326
039100120326           // -?F11=Ordinamento?
039200120326           when  dsp_aid = c_F11;
039300120326             exsr  sr_F11S01;
039400120326
039500120326           // -?F12=Ritorno?
039600120326           when  dsp_aid = c_F12;
039700120326             P01rit = 'L';
039800120326             $Fine  = *on;
039900120326
040000120326           // -?Invio?
040100120326           other;
040200120326             // -?Gestione opzioni?
040300120327             if  S01nrr > *zero;
040400120327               exsr  sr_OpzS01;
040500120327               if  ErrGenerico;
040600120327                 leavesr;
040700120327               endif;
040800120327             endif;
040900120326
041000120326             // -?Richiesto posizionamento?
041100120326             if  (Not Ord_x_KSU   and   C1Cksc <> SavC1Cksc)   OR
041200120326                 (Ord_x_KSU       and   C1Cksu <> SavC1Cksu);
041300120326               $InzS01 = *on;
041400120326             endif;
041500120326
041600120326         endsl;
041700120326
041800120326       ENDSR;
041900120326
042000120326       //--------------------------------------------------------------
042100120326       //?Inizializzazione subfile S01.                                ?
042200120326       //--------------------------------------------------------------
042300120326       BEGSR  sr_InzS01;
042400120326
042500120326         // -?Spegnimento degli indicatori di errore?
042600120326         %subst(IndDspF : 50) = *off;
042700120326
042800120326         // -?Impostazione indicatore x la gestione del posizionamento?
042900120326         //  ?nel subfile?
043000120327         //No_Posiz = (P01cod <> *zero  or  P01ksu <> *zero);
043100120326
043200120326         // -?Pulizia subfile control?
043300120327         if  Not Ord_x_KSU;
043400120327           clear  C1Cksu;
043500120327           reset  SavC1Cksu;
043600120326         else;
043700120326           clear  C1Cksc;
043800120326           reset  SavC1Cksc;
043900120326         endif;
044000120326
044100120326         // -?Pulizia subfile?
044200120326         SflDsp_N    = *on;
044300120326         SflDspCtl_N = *on;
044400120604         write  TBJDC1C01;
044500120326         SflDspCtl_N = *off;
044600120326         SflEnd      = *off;
044700120326
044800120326         clear  C1RcdNbr;
044900120326         clear  C1CsrRrn;
045000120326         clear  S01nrr;
045100120326         clear  V1Dmsg;
045200120326         ErrMessage  = *off;
045300120326         ErrGenerico = *off;
045400120326
045500120326         // -?Caricamento completo dei dati nel subfile?
045600120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
045700120326         //  ?l'eventuale ordinamento)?
045800120326         setll  %kds(k05tntbe01)  TNTBE000;
045900120326         reade  %kds(k05tntbe01 : 1)  TNTBE000;
046000120326         $EoF = (%eof(TNTBE01L));
046100120326         exsr  sr_CaricaS01;
046200120326
046300120326         // -?Impaginazione subfile?
046400120326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
046500120326         if S01nrr > *zero;
046600120326           C1RcdNbr  = 1;
046700120326           C1CsrRrn  = 1;
046800120326         else;
046900120326           clear C1RcdNbr;
047000120326         endif;
047100120326
047200120326         // -?Ordinamento subfile (se premuto F11)?
047300120326         if  Ord_x_KSU   and   S01nrr >  *zero;
047400120326           exsr  sr_SortSfl;
047500120326         endif;
047600120326
047700120326       ENDSR;
047800120326
047900120326       //--------------------------------------------------------------
048000120326       //?Caricamento completo del subfile S01                         ?
048100120326       //--------------------------------------------------------------
048200120326       BEGSR  sr_CaricaS01;
048300120326
048400120326         clear  S01nrr;
048500120326         SflNxtChg = *off;
048600120326
048700120326         // -?Ciclo di caricamento dati da tab. "3EW"?
048800120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
048900120326         //  ?l'eventuale ordinamento)?
049000120326         DoW  Not $EoF;
049100120326
049200120326           // -?Selezione singolo record file TNTBE00F?
049300120326           exsr  sr_SelezRec;
049400120326           if  $EoF;
049500120326             leave;
049600120326           endif;
049700120326
049800120326           // -?Registrazione singolo record nel subfile?
049900120326           if  $RecOK;
050000120326
050100120326             // ·?Impostazione campi?
050200120604             clear  TBJDC1S01;
050300120326             S1Ccod = %int( %subst( TBEke1 : 1 : 7 ) );
050400120604             S1Cdes = §JDCrag;
050500120604             S1Cksu = §JDCksu;
050600120604             S1Caut = §JDCaut;
050700120604             S1Ctpi = §JDCtpi;
050800120604             S1Cfmi = §JDCfmi;
050900120604             S1Cdir = §JDCdir;
051000120326
051100120604             // -?Scrittura record nel subfile?
051200120326             S01nrr += 1;
051300120604             write  TBJDC1S01;
051400120326
051500120326           EndIf;
051600120326
051700120326           // -?Lettura record successivo?
051800120326           reade  %kds(k05tntbe01 : 1)  TNTBE000;
051900120326           $EoF = (%eof(TNTBE01L));
052000120326
052100120326         EndDo;
052200120326
052300120326         // -?Memorizzazione posizionamento effettuato?
052400120326         if  Not Ord_x_KSU;
052500120326           SavC1Cksc = C1Cksc;
052600120326         else;
052700120326           SavC1Cksu = C1Cksu;
052800120326         endif;
052900120326
053000120326         // -?Visualizzazione del SFL (se ci sono dati)?
053100120326         SflDsp_N = (S01nrr <= *zero);
053200120326
053300120326         // -?Attivazione del SFLEND?
053400120326         SflEnd = ( $EoF );
053500120326
053600120326         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
053700120326         if  S01nrr > *zero;
053800120326           C1RcdNbr = 1;
053900120326         else;
054000120326           clear  C1RcdNbr;
054100120326         endif;
054200120326
054300120326         C1CsrRrn = C1RcdNbr;
054400120326
054500120326       ENDSR;
054600120326
054700120326       //--------------------------------------------------------------
054800120604       //?Selezione del singolo record letto dalla tabella "JDC".      ?
054900120326       //--------------------------------------------------------------
055000120326       BEGSR  sr_SelezRec;
055100120326
055200120326         reset  $RecOK;
055300120326
055400120604         dJDC = TBEuni;
055500120326
055600120326         select;
055700120327
055800120327           // -?Selezione per cliente?
055900120327           when  P01cod <> *zero   and
056000120327                 %editc( P01cod : 'X' ) <> %trim(TBEke1);
056100120327             leavesr;
056200120326
056300120326           // -?Selezione per cliente unificante?
056400120604           when  P01ksu <> *zero   and   P01ksu <> §JDCksu;
056500120326             leavesr;
056600120326
056700120326           // -?Omissione records annullati?
056800120326           when  TBEatb <> *blank;
056900120326             leavesr;
057000120326
057100120326           // -?Richiesto posizionamento per cliente:?
057200120326           //  ?si scartano i clienti con codice inferiore?
057300120327           when  Not Ord_x_KSU   and   TBEke1 < %editc( C1Cksc : 'X' );
057400120326             leavesr;
057500120326
057600120326           // -?Richiesto posizionamento per cliente unificante:?
057700120326           //  ?si scartano i clienti con unificante inferiore?
057800120604           when  Ord_x_KSU   and   §JDCksu < C1Cksu;
057900120326             leavesr;
058000120327
058100120327           // -?Selezione per descrizione (ragione sociale cliente)?
058200120327           when  W1Cdes <> *blank   and
058300120604                 %scan( %trimr(SavW1Cdes) : §JDCrag ) = *zero;
058400120327             leavesr;
058500120326
058600120326         endsl;
058700120326
058800120326         $RecOK = *on;
058900120326
059000120326       ENDSR;
059100120327
059200120327       //--------------------------------------------------------------
059300120327       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
059400120327       //?F4-Ricerca per descrizione (ragione sociale)                 ?
059500120327       //--------------------------------------------------------------
059600120327       BEGSR  sr_F04S01;
059700120327
059800120327         // -?Emissione window W01?
059900120604         exfmt  TBJDC1W01;
060000120327
060100120327         reset  ErrMessage;
060200120327         reset  ErrGenerico;
060300120327         clear  V1Dmsg;
060400120327
060500120327         Select;
060600120327
060700120327           // -?F12=Ritorno?
060800120327           When  dsp_aid = c_F12;
060900120327             $Video = 'S1';
061000120327             leavesr;
061100120327
061200120327           // -?Invio?
061300120327           Other;
061400120327             if  W1Cdes <> SavW1Cdes;
061500120327               $InzS01   = *on;
061600120327               SavW1Cdes = W1Cdes;
061700120327             endif;
061800120327             if  W1Cdes = *blank;
061900120327               clear  C1Ddes;
062000120327             else;
062100120327               C1Ddes = 'Ricerca: ' + %trim(W1Cdes);
062200120327             endif;
062300120327             leavesr;
062400120327
062500120327         EndSl;
062600120327
062700120327       ENDSR;
062800120326
062900120326       //--------------------------------------------------------------
063000120326       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
063100120326       //?F11-Cambia ordinamento sfl                                   ?
063200120326       //--------------------------------------------------------------
063300120326       BEGSR  sr_F11S01;
063400120326
063500120326         Ord_x_KSU = (Not Ord_x_KSU);
063600120326
063700120326         select;
063800120326           // -?Subfile vuoto: nessun record da ordinare?
063900120326           when  S01nrr = *zero;
064000120326           // -?Subfile già posizionato: occorre ricaricarlo?
064100120326           when  C1Cksc <> *zero   or   C1Cksu <> *zero;
064200120326             $InzS01 = *on;
064300120326           // -?Altrimenti: basta riordinarlo?
064400120326           other;
064500120326             exsr  sr_SortSfl;
064600120326         endsl;
064700120326
064800120326         if  Not Ord_x_KSU;
064900120326           clear  C1Cksc;
065000120326           clear  SavC1Cksc;
065100120326         else;
065200120326           clear  C1Cksu;
065300120326           clear  SavC1Cksu;
065400120326         endif;
065500120326
065600120326       ENDSR;
065700120326
065800120326       //--------------------------------------------------------------
065900120326       //?Ordinamento subfile                                          ?
066000120326       //?  This subroutine sorts the subfile records.                 ?
066100120326       //--------------------------------------------------------------
066200120326       BEGSR  sr_SortSfl;
066300120326
066400120326         RrnLast  = S01nrr;
066500120326
066600120326         C1RcdNbr = 1;
066700120326         clear  C1CsrRrn;
066800120326         clear  S01nrr;
066900120326         clear  V1Dmsg;
067000120326         ErrMessage  = *off;
067100120326         SflNxtChg   = *off;
067200120326         ErrGenerico = *off;
067300120326         %subst(IndDspF : 50) = *off;
067400120326
067500120326         //?___________________________________________________________?
067600120326         //?Initialize the key fields to sort on.?
067700120326         //?There is one extra field not in the subfile -- Selected --?
067800120326         //?that is added to the record. This field is used to place?
067900120326         //?selected records in front of nonselected records.?
068000120326         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
068100120326
068200120326         clear  QLGKL;
068300120326         clear  QLGSKL;
068400120326         clear  QLGSCB;
068500120326         clear  QLGSCB00;
068600120326
068700120326         // -?Gestione della scelta ordinamento:?
068800120326         // -?Ordinamento per Padre/Codice?
068900120326         If  Ord_x_KSU = *on;
069000120326           // ·?2 campi chiave?
069100120326           QLGNBRK  = 2;
069200120326           // ·?1° campo: padre (S1Cksu)?
069300120326           QLGSP    = 1 + %size(S1Copz) + %size(S1Ccod) + %size(S1Cdes);
069400120326           QLGSS    = %size(S1Cksu);
069500120326           QLGDT    = Carattere;
069600120326           QLGSO    = Ascendente;
069700120326           QLGKL(1) = QLGSKL;
069800120326           // ·?2° campo: codice (S1Ccod)?
069900120326           QLGSP    = 1 + %size(S1Copz);
070000120326           QLGSS    = %size(S1Ccod);
070100120326           QLGDT    = Carattere;
070200120326           QLGSO    = Ascendente;
070300120326           QLGKL(2) = QLGSKL;
070400120326
070500120326         // -?Ordinamento per Cliente?
070600120326         Else;
070700120326           // ·?1 campi chiave?
070800120326           QLGNBRK  = 1;
070900120326           // ·?1° campo: cliente (S1Ccli)?
071000120326           //            ?a posizone    2,  7 byte, char, ascending.?
071100120326           QLGSP    = 1 + %size(S1Copz);
071200120326           QLGSS    = %size(S1Ccod);
071300120326           QLGDT    = Carattere;
071400120326           QLGSO    = Ascendente;
071500120326           QLGKL(1) = QLGSKL;
071600120326
071700120326         EndIf;
071800120326
071900120326         // -?Load other sort parameters.?
072000120326         QLGLB   = 80 + 16 * MaxKey;
072100120326         QLGRL   = %size(SflRcd) - 1;
072200120326         QLGRT   = 8;
072300120326         QLGOKL  = 80;
072400120326         QLGLKE  = 16;
072500120326         QLGLSS  = 290;
072600120326         // -?Initialize Sort I/O API fields.?
072700120326         QLGRL00 = QLGRL;
072800120326         QLGRC00 = 1;
072900120326         clear  QUSEI;
073000120326         QUSBPRV = %size(QUSEC);
073100120326
073200120326         // -?First step - Initialize the sort routine.?
073300120326         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
073400120326                      SizeList : ReturnSize : QUSEC );
073500120326
073600120326         // -?Next step - Write records to I/O routine.?
073700120326         QLGRT00  = Put;
073800120326         For  Nrr = 1  To  RrnLast;
073900120604           chain  Nrr  TBJDC1S01;
074000120326           // -?Solo le righe con Selected = 'Y' sono riordinate,?
074100120326           //  ?quindi per fare un ordinamento di tutte le righe?
074200120326           //  ?metto 'Y' sempre.?
074300120326           Selected = 'Y';
074400120326           clear  QUSEI;
074500120326           QUSBPRV  = %size(QUSEC);
074600120326           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
074700120326                         SizeList : NotUsed : QUSEC );
074800120326         EndFor;
074900120326
075000120326         // -?Next step - Signal end of input, clear subfile for reload.?
075100120326         QLGRT00 = EndPut;
075200120326         clear  QUSEI;
075300120326         QUSBPRV = %size(QUSEC);
075400120326         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
075500120326                       SizeList : NotUsed : QUSEC );
075600120326
075700120326         // -?Pulizia subfile?
075800120326         SflDsp_N    = *on;
075900120326         SflDspCtl_N = *on;
076000120604         write  TBJDC1C01;
076100120326         SflDspCtl_N = *off;
076200120326         SflEnd      = *off;
076300120326
076400120326         // -?Final step - Write the records back to the subfile.?
076500120326         QLGRT00  = Get;
076600120326         For  Nrr = 1  To RrnLast;
076700120326           clear  QUSEI;
076800120326           QUSBPRV = %size(QUSEC);
076900120326           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
077000120326                         QLGRL00  : NotUsed : QUSEC );
077100120326           // -?Ripristino indicatori utilizzati nel sfl rec?
077200120326           SflNxtChg = (S1Copz <> *blank);
077300120326           S01nrr = Nrr;
077400120604           write  TBJDC1S01;
077500120326         EndFor;
077600120326
077700120326         C1CsrRrn = 1;
077800120326
077900120326         // -?Visualizzazione del SFL (se ci sono dati)?
078000120326         SflDsp_N = (S01nrr <= *zero);
078100120326
078200120326         // -?Attivazione del SFLEND?
078300120326         SflEnd = $EoF;
078400120326
078500120326       ENDSR;
078600120326
078700120326       //--------------------------------------------------------------
078800120326       //?Gestione opzioni subfile S01.                                ?
078900120326       //--------------------------------------------------------------
079000120326       BEGSR  sr_OpzS01;
079100120326
079200120326         clear  SavS01csr;
079300120326
079400120326         // -?Ciclo di lettura subfile?
079500120604         readc  TBJDC1S01;
079600120326
079700120604         DoW  Not %eof(TNTBJDC1D);
079800120326
079900120326           SflNxtChg = *off;
080000120326           %subst(IndDspF : 50) = *off;
080100120326           C1RcdNbr = S01nrr;
080200120326
080300120326           select;
080400120326
080500120326             // -?Opz. 1=Selezione?
080600120326             when  S1Copz = '1'   and   Not $Fine;
080700120326               clear  P01rit;
080800120326               P01ke1 = %editc( S1Ccod : 'X' );
080900120326               clear  P01ke2;
081000120326               $Fine   = *on;
081100120327
081200120327             // -?Opz. 3=Copia?
081300120327             when  S1Copz = '3'   and   Not $Fine;
081400120327               P01rit = S1Copz;
081500120327               P01ke1 = %editc( S1Ccod : 'X' );
081600120327               clear  P01ke2;
081700120327               $Fine   = *on;
081800120326
081900120326             // -?Nessuna opzione?
082000120326             when  S1Copz = *blank;
082100120327
082200120327             // -?Ammessa una sola opzione?
082300120327             when  S1Copz <> *blank   and   $Fine;
082400120327               ErrMessage  = *on;
082500120327               ErrGenerico = *on;
082600120327               PosCurOPZ   = *on;
082700120327               V1Dmsg = $Msg(02);
082800120327               $Fine  = *off;
082900120326
083000120326             // -?Opzione errata?
083100120326             other;
083200120326               ErrMessage  = *on;
083300120326               ErrGenerico = *on;
083400120326               PosCurOPZ   = *on;
083500120326               V1Dmsg = $Msg(01);
083600120326               $Fine  = *off;
083700120326
083800120326           endsl;
083900120326
084000120326           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
084100120326           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
084200120326           if  S1Copz  <> *blank   and
084300120326              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
084400120326             SavS01csr   = C1RcdNbr;
084500120326           endif;
084600120326
084700120326           // -?Aggiornamento sfl?
084800120326           select;
084900120326             when  ErrMessage;
085000120326               SflNxtChg = *on;
085100120326               C1CsrRrn  = C1RcdNbr;
085200120326             when  ErrGenerico;
085300120326               C1CsrRrn  = C1RcdNbr;
085400120326               if  S1Copz <> '1';
085500120326                 clear  S1Copz;
085600120326               endif;
085700120326             other;
085800120326               if  S1Copz <> '1';
085900120326                 clear  S1Copz;
086000120326               endif;
086100120326           endsl;
086200120326
086300120326           if  S1Copz <> *blank;
086400120326             SflNxtChg = *on;
086500120326           endif;
086600120326
086700120604           UPDATE  TBJDC1S01;
086800120326
086900120326           if  ErrGenerico   or   ErrMessage;
087000120326             leave;
087100120326           endif;
087200120326
087300120604           readc  TBJDC1S01;
087400120326
087500120326         ENDDO;
087600120326
087700120326         // -?Riposizionam. cursore sul record contenente la prima opz.?
087800120326         //  ?(se non sono stati rilevati errori)?
087900120326         if  NOT ErrMessage   and   NOT ErrGenerico   and
088000120326             SavS01csr > *zero;
088100120326           C1CsrRrn = SavS01csr;
088200120326         endif;
088300120326
088400120326       ENDSR;
088500120326
088600120326       //--------------------------------------------------------------
088700120326       //?Operazioni finali.                                           ?
088800120326       //--------------------------------------------------------------
088900120326       BEGSR  sr_RoutEnd;
089000120327
089100120327         // -?Restituzione parametri?
089200120327         kpjbu = Param01;
089300120326
089400120326         // -?"Chiusura" *pgm?
089500120326         return;
089600120326
089700120326       ENDSR;
089800120326
089900120326      /end-free
090000120326
090100120326       //--------------------------------------------------------------
090200120326       //?Schiere a tempo di compilazione.                             ?
090300120326       //--------------------------------------------------------------
090400120326
090500120326** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
090600120326Opzione errata                                                                 1
090700120326Ammessa la selezione di un solo cliente                                        2
