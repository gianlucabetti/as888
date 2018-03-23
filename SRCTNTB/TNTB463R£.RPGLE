000100120326       //==============================================================
000200120326       //?TNTB461R * Selezione tab. "LAC" = Clienti x Ritorno Immagini.?
000300120326       //==============================================================
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
002700120326     fTNTB461D  cf   e             workstn
002800120326     f                                     sfile(TB461S01 : S01nrr)
002900120326     f                                     indds(IndDspF)
003000120326     f                                     infds(InfDspF)
003100120326
003200120326       //--------------------------------------------------------------
003300120326       //?Definizione costanti.                                        ?
003400120326       //--------------------------------------------------------------
003500120326
003600120326       // -?Codice tabella in gestione?
003700120326     d c_Tab           c                   const('LAC')
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
009500120326     d*//SDSprm          *parms
009600120326
009700120326       // -?InfDS?
009800120326     d InfDspF         ds
009900120326     d   dsp_aid             369    369a
010000120326     d   sfl_rrn             376    377i 0
010100120326     d   min_nrr             378    379i 0
010200120326     d   num_rcds            380    381i 0
010300120326
010400120326       // -?Indicatori su DspF?
010500120326     d IndDspF         ds                  inz
010600120326         // -?Abilitazione tasti funzionali?
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
014000120326       // -?Tab. LAC = Clienti per ritorno immagini?
014100120326     d dLAC          e ds                  inz
014200120327     d  sk_Sch                71     82    dim(12)
014300120326
014400120326       //--------------------------------------------------------------
014500120326       //?Definizione variabili globali.                               ?
014600120326       //--------------------------------------------------------------
014700120326
014800120326       // -?Parametri (parziali) in Input?
014900120326     d P01opz3         s              1
015000120326
015100120326       // -?Flags booleani?
015200120326     d $Fine           s               n   inz
015300120326     d $EoF            s               n   inz
015400120326     d $InzS01         s               n   inz(*on)
015500120326     d $RecOK          s               n   inz
015600120327
015700120327       // -?Indici di schiera?
015800120327     d x2              s              2  0 inz
015900120326
016000120326       // -?Variabili per la gestione del video?
016100120326     d $Video          s              2    inz('S1')
016200120326     d S01nrr          s                   like(C1RcdNbr) inz
016300120326     d SavS01csr       s                   like(C1RcdNbr) inz
016400120326
016500120326       // -?Variabili per la gestione del singolo campo alla variazione?
016600120326       //  ?dello stesso?
016700120326     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
016800120326     d SavC1Cksu       s                   like(C1Cksu)   inz(*hival)
016900120327     d SavW1Cdes       s                   like(W1Cdes)   inz
017000120326
017100120326       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
017200120326
017300120326       //--------------------------------------------------------------
017400120326       // -?Constants?
017500120326       //--------------------------------------------------------------
017600120326       // -?MaxKey - Maximum number of key fields allowed by this program?
017700120326     d MaxKey          c                   const(2)
017800120326       // -?Sort order:?
017900120326       //  ?1 = Sort in an ascending sequence?
018000120326       //  ?2 = Sort in a descending sequence?
018100120326     d Ascendente      c                   const(1)
018200120326     d Discendente     c                   const(2)
018300120326       // -?Key data type:?
018400120326       //  ? 0 = Signed binary?
018500120326       //  ? 2 = Signed zoned decimal?
018600120326       //  ? 3 = Signed packed decimal?
018700120326       //  ? 6 = Character with no national language sort sequence applied,?
018800120326       //  ?     if specified?
018900120326       //  ? 7 = Unsigned packed decimal?
019000120326       //  ?     All numbers will have the sign forced positive ('F'X).?
019100120326       //  ? 8 = Unsigned zoned decimal?
019200120326       //  ?     All numbers will have the sign forced positive ('F'X).?
019300120326       //  ? 9 = Unsigned binary?
019400120326       //  ?14 = Date in form of DD/MM/YY?
019500120326       //  ?15 = Date in form of DD.MM.YYYY?
019600120326     d Numero          c                   const(2)
019700120326     d Carattere       c                   const(6)
019800120326       //
019900120326     d Put             c                   const(1)
020000120326     d EndPut          c                   const(2)
020100120326     d Get             c                   const(3)
020200120326
020300120326       //--------------------------------------------------------------
020400120326       // -?Data Structures?
020500120326       //  ?SflRcd     - The entire subfile record to pass to the sort?
020600120326       //  ?QLGSCB     - The sort request block for the QLGSORT API?
020700120326       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
020800120326       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
020900120326       //  ?QUSEC      - Error structure for the QLGSORT API?
021000120326       //--------------------------------------------------------------
021100120326     d SflRcd          ds                  inz
021200120327     d  S1Copz                             inz
021300120326     d  S1Ccod                             inz
021400120326     d  S1Cdes                             inz
021500120326     d  S1Cksu                             inz
021600120326     d  S1Caut                             inz
021700120326     d  S1Cfmi                             inz
021800120326     d  S1Cschgg                           inz
021900120326     d  S1Cmesi                            inz
022000120326     d  S1Ctadu                            inz
022100120326     d  S1Ctad                             inz
022200120326     d  S1Cksc                             inz
022300120326     d  S1Csch                             inz
022400120326     d  S1Cctr                             inz
022500120326     d  Selected                      1a   inz
022600120326      /copy QSYSINC/QRPGLESRC,QLGSORT
022700120326     d QLGKL                         16    dim(MaxKey)
022800120326     d  QLGSP00                       9b 0 overlay(QLGKL:00001)
022900120326     d  QLGSS00                       9b 0 overlay(QLGKL:00005)
023000120326     d  QLGDT00                       9b 0 overlay(QLGKL:00009)
023100120326     d  QLGSO00                       9b 0 overlay(QLGKL:00013)
023200120326      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023300120326      /copy QSYSINC/QRPGLESRC,QUSEC
023400120326
023500120326       //--------------------------------------------------------------
023600120326       // -?Standalone fields?
023700120326       //  ?Nrr        - Relative record number for subfile?
023800120326       //  ?SizeList   - Size of list?
023900120326       //  ?ReturnSize - Size of list returned by sort API?
024000120326       //--------------------------------------------------------------
024100120326     d NotUsed         s             16a   inz
024200120326     d ReturnSize      s              9b 0 inz
024300120326     d SizeList        s              9b 0 inz
024400120326     d RrnLast         s              5i 0 inz
024500120326     d Nrr             s              5i 0 inz
024600120326
024700120326       //--------------------------------------------------------------
024800120326       //?Definizione prototipi procedure.                             ?
024900120326       //--------------------------------------------------------------
025000120326
025100120326       // -?Reperimento dati utente?
025200120326     d TIBS34ds      e ds                  inz
025300120326      /copy gaitrasrc/srcProtoPR,TIBS34R
025400120326
025500120326       // -?Ordinamento subfile?
025600120326      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025700120326
025800120326       //--------------------------------------------------------------
025900120326       //?Definizione key-list.                                        ?
026000120326       //--------------------------------------------------------------
026100120326
026200120326       // -?File TNTBE01L?
026300120326     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
026400120326     d                                     prefix(k_)   inz
026500120326
026600120326       //--------------------------------------------------------------
026700120326       //?Riepilogo indicatori utilizzati.                             ?
026800120326       //--------------------------------------------------------------
026900120326
027000120326
027100120326       //--------------------------------------------------------------
027200120326       //?M A I N - L I N E                                            ?
027300120326       //--------------------------------------------------------------
027400120326
027500120326     c     *Entry        plist
027600120326     c                   parm                    KPJBA
027700120326     c                   parm                    P01opz3
027800120326
027900120326      /free
028000120326
028100120326       // -?Operazioni iniziali?
028200120326       exsr sr_RoutInz;
028300120326
028400120326       // -?Ciclo di gestione del file video?
028500120326       DoW  $Fine = *off;
028600120326         select;
028700120326           // -?Gestione videata S1 (subfile)?
028800120326           when $Video = 'S1';
028900120326             exsr sr_GesS01;
029000120326           other;
029100120326             $Fine = *on;
029200120326         endsl;
029300120326       enddo;
029400120326
029500120326       // -?Operazioni finali?
029600120326       exsr sr_RoutEnd;
029700120326
029800120326       //--------------------------------------------------------------
029900120326       //?Operazioni iniziali.                                         ?
030000120326       //--------------------------------------------------------------
030100120326       BEGSR  sr_RoutInz;
030200120327
030300120327         // -?Impostazione chiusura?
030400120327         *inLR = *on;
030500120327
030600120327         // -?Reperimento dati job?
030700120327         exsr  sr_DatiJob;
030800120327
030900120327         // -?Impostazione nome programma a video?
031000120327         V1Tpgm = SDSpgm;
031100120326
031200120327         // -?Impostazione parametri ricevuti?
031300120326         Param01 = kpjbu;
031400120327         clear  P01ke1;
031500120327         clear  P01ke2;
031600120326         clear  P01rit;
031700120326
031800120326         // -?Verifica se ricevuti altri parametri?
031900120326         if  %parms() > 1;
032000120326           C1Dopz = '1=Scelta, 3=Copia';
032100120326         else;
032200120326           C1Dopz = '1=Scelta';
032300120326         endif;
032400120326
032500120326         // -?Impostazione iniziale / pulizia dei campi chiave?
032600120327         Ord_x_KSU = (P01ord = '1');
032700120326         clear  k05tntbe01;
032800120326         k_TBEcod = c_Tab;
032900120326
033000120326       ENDSR;
033100120327
033200120327       //--------------------------------------------------------------
033300120327       //?Reperimento Dati del job (Utente/Operativi).                 ?
033400120327       //--------------------------------------------------------------
033500120327       BEGSR  sr_DatiJob;
033600120327
033700120327         in(e) §AzUte;
033800120327         if NOT %error;
033900120327           in(e) §DatiUte;
034000120327         endif;
034100120327         if %error or RSut = *blank;
034200120327           tibs34r ( tibs34ds );
034300120327           in §AzUte;
034400120327           in §DatiUte;
034500120327         endif;
034600120327
034700120327       ENDSR;
034800120326
034900120326       //--------------------------------------------------------------
035000120326       //?Gestione subfile S01.                                        ?
035100120326       //--------------------------------------------------------------
035200120326       BEGSR  sr_GesS01;
035300120326
035400120326         // -?Inizializzazione videata?
035500120326         if  $InzS01 = *on;
035600120326           exsr  sr_InzS01;
035700120326           $InzS01 = *off;
035800120326         endif;
035900120326
036000120326         // -?Emissione Testata & Piede?
036100120326         write  TB461T01;
036200120326         write  TB461P01;
036300120326
036400120326         // -?Posizionamento cursore?
036500120326         if  C1CsrRrn > *zero;
036600120326           C1RcdNbr = C1CsrRrn;
036700120326         else;
036800120326           C1RcdNbr = 1;
036900120326           write  TB461S00;            //?(no rec.)?
037000120326         endif;
037100120326
037200120326         // -?Emissione videata?
037300120326         exfmt  TB461C01;
037400120326
037500120326         reset  ErrMessage;
037600120326         reset  ErrGenerico;
037700120326         clear  V1Dmsg;
037800120326
037900120326         select;
038000120326
038100120326           // -?F3=Fine?
038200120326           when  dsp_aid = c_F03;
038300120326             P01rit = 'C';
038400120326             $Fine  = *on;
038500120327
038600120327           // -?F4=Ricerca per descrizione?
038700120327           when  dsp_aid = c_F04;
038800120327             exsr  sr_F04S01;
038900120326
039000120326           // -?F5=Rivisualizza?
039100120326           when  dsp_aid = c_F05;
039200120326             $InzS01 = *on;
039300120326
039400120326           // -?F11=Ordinamento?
039500120326           when  dsp_aid = c_F11;
039600120326             exsr  sr_F11S01;
039700120326
039800120326           // -?F12=Ritorno?
039900120326           when  dsp_aid = c_F12;
040000120326             P01rit = 'L';
040100120326             $Fine  = *on;
040200120326
040300120326           // -?Invio?
040400120326           other;
040500120326             // -?Gestione opzioni?
040600120327             if  S01nrr > *zero;
040700120327               exsr  sr_OpzS01;
040800120327               if  ErrGenerico;
040900120327                 leavesr;
041000120327               endif;
041100120327             endif;
041200120326
041300120326             // -?Richiesto posizionamento?
041400120326             if  (Not Ord_x_KSU   and   C1Cksc <> SavC1Cksc)   OR
041500120326                 (Ord_x_KSU       and   C1Cksu <> SavC1Cksu);
041600120326               $InzS01 = *on;
041700120326             endif;
041800120326
041900120326         endsl;
042000120326
042100120326       ENDSR;
042200120326
042300120326       //--------------------------------------------------------------
042400120326       //?Inizializzazione subfile S01.                                ?
042500120326       //--------------------------------------------------------------
042600120326       BEGSR  sr_InzS01;
042700120326
042800120326         // -?Spegnimento degli indicatori di errore?
042900120326         %subst(IndDspF : 50) = *off;
043000120326
043100120326         // -?Impostazione indicatore x la gestione del posizionamento?
043200120326         //  ?nel subfile?
043300120327         //No_Posiz = (P01cod <> *zero  or  P01ksu <> *zero);
043400120326
043500120326         // -?Pulizia subfile control?
043600120327         if  Not Ord_x_KSU;
043700120327           clear  C1Cksu;
043800120327           reset  SavC1Cksu;
043900120326         else;
044000120326           clear  C1Cksc;
044100120326           reset  SavC1Cksc;
044200120326         endif;
044300120326
044400120326         // -?Pulizia subfile?
044500120326         SflDsp_N    = *on;
044600120326         SflDspCtl_N = *on;
044700120326         write  TB461C01;
044800120326         SflDspCtl_N = *off;
044900120326         SflEnd      = *off;
045000120326
045100120326         clear  C1RcdNbr;
045200120326         clear  C1CsrRrn;
045300120326         clear  S01nrr;
045400120326         clear  V1Dmsg;
045500120326         ErrMessage  = *off;
045600120326         ErrGenerico = *off;
045700120326
045800120326         // -?Caricamento completo dei dati nel subfile?
045900120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
046000120326         //  ?l'eventuale ordinamento)?
046100120326         setll  %kds(k05tntbe01)  TNTBE000;
046200120326         reade  %kds(k05tntbe01 : 1)  TNTBE000;
046300120326         $EoF = (%eof(TNTBE01L));
046400120326         exsr  sr_CaricaS01;
046500120326
046600120326         // -?Impaginazione subfile?
046700120326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
046800120326         if S01nrr > *zero;
046900120326           C1RcdNbr  = 1;
047000120326           C1CsrRrn  = 1;
047100120326         else;
047200120326           clear C1RcdNbr;
047300120326         endif;
047400120326
047500120326         // -?Ordinamento subfile (se premuto F11)?
047600120326         if  Ord_x_KSU   and   S01nrr >  *zero;
047700120326           exsr  sr_SortSfl;
047800120326         endif;
047900120326
048000120326       ENDSR;
048100120326
048200120326       //--------------------------------------------------------------
048300120326       //?Caricamento completo del subfile S01                         ?
048400120326       //--------------------------------------------------------------
048500120326       BEGSR  sr_CaricaS01;
048600120326
048700120326         clear  S01nrr;
048800120326         SflNxtChg = *off;
048900120326
049000120326         // -?Ciclo di caricamento dati da tab. "3EW"?
049100120326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
049200120326         //  ?l'eventuale ordinamento)?
049300120326         DoW  Not $EoF;
049400120326
049500120326           // -?Selezione singolo record file TNTBE00F?
049600120326           exsr  sr_SelezRec;
049700120326           if  $EoF;
049800120326             leave;
049900120326           endif;
050000120326
050100120326           // -?Registrazione singolo record nel subfile?
050200120326           if  $RecOK;
050300120326
050400120326             // ·?Impostazione campi?
050500120326             clear  TB461S01;
050600120326             S1Ccod = %int( %subst( TBEke1 : 1 : 7 ) );
050700120326             S1Cdes = §LACrag;
050800120326             S1Cksu = §LACksu;
050900120326             S1Caut = §LACaut;
051000120326             S1Cfmi = §LACfmi;
051100120326             S1Cschgg = §LACschgg;
051200120326             S1Cmesi  = §LACmesi;
051300120326             S1Ctadu  = §LACtadu;
051400120326             S1Ctad = §LACtad;
051500120326             S1Cksc = §LACksc;
051600120326             S1Cctr = §LACctr;
051700120326
051800120326             // ·?Impostazione mesi schedulati?
051900120326             clear  S1Csch;
052000120327             For  x2 = 1  To  %elem(sk_Sch);
052100120327               If  sk_Sch(x2) <> *blank;
052200120326                 if  S1Csch = *blank;
052300120327                   S1Csch = %editc( x2 : 'X' );
052400120326                 else;
052500120327                   S1Csch = %trimr(S1Csch) + ' ' + %editc( x2 : 'X' );
052600120326                 endif;
052700120326               EndIf;
052800120326             EndFor;
052900120326
053000120326             // ·?Scrittura record nel subfile?
053100120326             S01nrr += 1;
053200120326             write  TB461S01;
053300120326
053400120326           EndIf;
053500120326
053600120326           // -?Lettura record successivo?
053700120326           reade  %kds(k05tntbe01 : 1)  TNTBE000;
053800120326           $EoF = (%eof(TNTBE01L));
053900120326
054000120326         EndDo;
054100120326
054200120326         // -?Memorizzazione posizionamento effettuato?
054300120326         if  Not Ord_x_KSU;
054400120326           SavC1Cksc = C1Cksc;
054500120326         else;
054600120326           SavC1Cksu = C1Cksu;
054700120326         endif;
054800120326
054900120326         // -?Visualizzazione del SFL (se ci sono dati)?
055000120326         SflDsp_N = (S01nrr <= *zero);
055100120326
055200120326         // -?Attivazione del SFLEND?
055300120326         SflEnd = ( $EoF );
055400120326
055500120326         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
055600120326         if  S01nrr > *zero;
055700120326           C1RcdNbr = 1;
055800120326         else;
055900120326           clear  C1RcdNbr;
056000120326         endif;
056100120326
056200120326         C1CsrRrn = C1RcdNbr;
056300120326
056400120326       ENDSR;
056500120326
056600120326       //--------------------------------------------------------------
056700120326       //?Selezione del singolo record letto dalla tabella "3EW".      ?
056800120326       //--------------------------------------------------------------
056900120326       BEGSR  sr_SelezRec;
057000120326
057100120326         reset  $RecOK;
057200120326
057300120326         dLAC = TBEuni;
057400120326
057500120326         select;
057600120327
057700120327           // -?Selezione per cliente?
057800120327           when  P01cod <> *zero   and
057900120327                 %editc( P01cod : 'X' ) <> %trim(TBEke1);
058000120327             leavesr;
058100120326
058200120326           // -?Selezione per cliente unificante?
058300120326           when  P01ksu <> *zero   and   P01ksu <> §LACksu;
058400120326             leavesr;
058500120326
058600120326           // -?Omissione records annullati?
058700120326           when  TBEatb <> *blank;
058800120326             leavesr;
058900120326
059000120326           // -?Richiesto posizionamento per cliente:?
059100120326           //  ?si scartano i clienti con codice inferiore?
059200120327           when  Not Ord_x_KSU   and   TBEke1 < %editc( C1Cksc : 'X' );
059300120326             leavesr;
059400120326
059500120326           // -?Richiesto posizionamento per cliente unificante:?
059600120326           //  ?si scartano i clienti con unificante inferiore?
059700120326           when  Ord_x_KSU   and   §LACksu < C1Cksu;
059800120326             leavesr;
059900120327
060000120327           // -?Selezione per descrizione (ragione sociale cliente)?
060100120327           when  W1Cdes <> *blank   and
060200120327                 %scan( %trimr(SavW1Cdes) : §LACrag ) = *zero;
060300120327             leavesr;
060400120326
060500120326         endsl;
060600120326
060700120326         $RecOK = *on;
060800120326
060900120326       ENDSR;
061000120327
061100120327       //--------------------------------------------------------------
061200120327       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
061300120327       //?F4-Ricerca per descrizione (ragione sociale)                 ?
061400120327       //--------------------------------------------------------------
061500120327       BEGSR  sr_F04S01;
061600120327
061700120327         // -?Emissione window W01?
061800120327         exfmt  TB461W01;
061900120327
062000120327         reset  ErrMessage;
062100120327         reset  ErrGenerico;
062200120327         clear  V1Dmsg;
062300120327
062400120327         Select;
062500120327
062600120327           // -?F12=Ritorno?
062700120327           When  dsp_aid = c_F12;
062800120327             $Video = 'S1';
062900120327             leavesr;
063000120327
063100120327           // -?Invio?
063200120327           Other;
063300120327             if  W1Cdes <> SavW1Cdes;
063400120327               $InzS01   = *on;
063500120327               SavW1Cdes = W1Cdes;
063600120327             endif;
063700120327             if  W1Cdes = *blank;
063800120327               clear  C1Ddes;
063900120327             else;
064000120327               C1Ddes = 'Ricerca: ' + %trim(W1Cdes);
064100120327             endif;
064200120327             leavesr;
064300120327
064400120327         EndSl;
064500120327
064600120327       ENDSR;
064700120326
064800120326       //--------------------------------------------------------------
064900120326       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
065000120326       //?F11-Cambia ordinamento sfl                                   ?
065100120326       //--------------------------------------------------------------
065200120326       BEGSR  sr_F11S01;
065300120326
065400120326         Ord_x_KSU = (Not Ord_x_KSU);
065500120326
065600120326         select;
065700120326           // -?Subfile vuoto: nessun record da ordinare?
065800120326           when  S01nrr = *zero;
065900120326           // -?Subfile già posizionato: occorre ricaricarlo?
066000120326           when  C1Cksc <> *zero   or   C1Cksu <> *zero;
066100120326             $InzS01 = *on;
066200120326           // -?Altrimenti: basta riordinarlo?
066300120326           other;
066400120326             exsr  sr_SortSfl;
066500120326         endsl;
066600120326
066700120326         if  Not Ord_x_KSU;
066800120326           clear  C1Cksc;
066900120326           clear  SavC1Cksc;
067000120326         else;
067100120326           clear  C1Cksu;
067200120326           clear  SavC1Cksu;
067300120326         endif;
067400120326
067500120326       ENDSR;
067600120326
067700120326       //--------------------------------------------------------------
067800120326       //?Ordinamento subfile                                          ?
067900120326       //?  This subroutine sorts the subfile records.                 ?
068000120326       //--------------------------------------------------------------
068100120326       BEGSR  sr_SortSfl;
068200120326
068300120326         RrnLast  = S01nrr;
068400120326
068500120326         C1RcdNbr = 1;
068600120326         clear  C1CsrRrn;
068700120326         clear  S01nrr;
068800120326         clear  V1Dmsg;
068900120326         ErrMessage  = *off;
069000120326         SflNxtChg   = *off;
069100120326         ErrGenerico = *off;
069200120326         %subst(IndDspF : 50) = *off;
069300120326
069400120326         //?___________________________________________________________?
069500120326         //?Initialize the key fields to sort on.?
069600120326         //?There is one extra field not in the subfile -- Selected --?
069700120326         //?that is added to the record. This field is used to place?
069800120326         //?selected records in front of nonselected records.?
069900120326         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
070000120326
070100120326         clear  QLGKL;
070200120326         clear  QLGSKL;
070300120326         clear  QLGSCB;
070400120326         clear  QLGSCB00;
070500120326
070600120326         // -?Gestione della scelta ordinamento:?
070700120326         // -?Ordinamento per Padre/Codice?
070800120326         If  Ord_x_KSU = *on;
070900120326           // ·?2 campi chiave?
071000120326           QLGNBRK  = 2;
071100120326           // ·?1° campo: padre (S1Cksu)?
071200120326           QLGSP    = 1 + %size(S1Copz) + %size(S1Ccod) + %size(S1Cdes);
071300120326           QLGSS    = %size(S1Cksu);
071400120326           QLGDT    = Carattere;
071500120326           QLGSO    = Ascendente;
071600120326           QLGKL(1) = QLGSKL;
071700120326           // ·?2° campo: codice (S1Ccod)?
071800120326           QLGSP    = 1 + %size(S1Copz);
071900120326           QLGSS    = %size(S1Ccod);
072000120326           QLGDT    = Carattere;
072100120326           QLGSO    = Ascendente;
072200120326           QLGKL(2) = QLGSKL;
072300120326
072400120326         // -?Ordinamento per Cliente?
072500120326         Else;
072600120326           // ·?1 campi chiave?
072700120326           QLGNBRK  = 1;
072800120326           // ·?1° campo: cliente (S1Ccli)?
072900120326           //            ?a posizone    2,  7 byte, char, ascending.?
073000120326           QLGSP    = 1 + %size(S1Copz);
073100120326           QLGSS    = %size(S1Ccod);
073200120326           QLGDT    = Carattere;
073300120326           QLGSO    = Ascendente;
073400120326           QLGKL(1) = QLGSKL;
073500120326
073600120326         EndIf;
073700120326
073800120326         // -?Load other sort parameters.?
073900120326         QLGLB   = 80 + 16 * MaxKey;
074000120326         QLGRL   = %size(SflRcd) - 1;
074100120326         QLGRT   = 8;
074200120326         QLGOKL  = 80;
074300120326         QLGLKE  = 16;
074400120326         QLGLSS  = 290;
074500120326         // -?Initialize Sort I/O API fields.?
074600120326         QLGRL00 = QLGRL;
074700120326         QLGRC00 = 1;
074800120326         clear  QUSEI;
074900120326         QUSBPRV = %size(QUSEC);
075000120326
075100120326         // -?First step - Initialize the sort routine.?
075200120326         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
075300120326                      SizeList : ReturnSize : QUSEC );
075400120326
075500120326         // -?Next step - Write records to I/O routine.?
075600120326         QLGRT00  = Put;
075700120326         For  Nrr = 1  To  RrnLast;
075800120326           chain  Nrr  TB461S01;
075900120326           // -?Solo le righe con Selected = 'Y' sono riordinate,?
076000120326           //  ?quindi per fare un ordinamento di tutte le righe?
076100120326           //  ?metto 'Y' sempre.?
076200120326           Selected = 'Y';
076300120326           clear  QUSEI;
076400120326           QUSBPRV  = %size(QUSEC);
076500120326           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
076600120326                         SizeList : NotUsed : QUSEC );
076700120326         EndFor;
076800120326
076900120326         // -?Next step - Signal end of input, clear subfile for reload.?
077000120326         QLGRT00 = EndPut;
077100120326         clear  QUSEI;
077200120326         QUSBPRV = %size(QUSEC);
077300120326         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
077400120326                       SizeList : NotUsed : QUSEC );
077500120326
077600120326         // -?Pulizia subfile?
077700120326         SflDsp_N    = *on;
077800120326         SflDspCtl_N = *on;
077900120326         write  TB461C01;
078000120326         SflDspCtl_N = *off;
078100120326         SflEnd      = *off;
078200120326
078300120326         // -?Final step - Write the records back to the subfile.?
078400120326         QLGRT00  = Get;
078500120326         For  Nrr = 1  To RrnLast;
078600120326           clear  QUSEI;
078700120326           QUSBPRV = %size(QUSEC);
078800120326           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
078900120326                         QLGRL00  : NotUsed : QUSEC );
079000120326           // -?Ripristino indicatori utilizzati nel sfl rec?
079100120326           SflNxtChg = (S1Copz <> *blank);
079200120326           S01nrr = Nrr;
079300120326           write  TB461S01;
079400120326         EndFor;
079500120326
079600120326         C1CsrRrn = 1;
079700120326
079800120326         // -?Visualizzazione del SFL (se ci sono dati)?
079900120326         SflDsp_N = (S01nrr <= *zero);
080000120326
080100120326         // -?Attivazione del SFLEND?
080200120326         SflEnd = $EoF;
080300120326
080400120326       ENDSR;
080500120326
080600120326       //--------------------------------------------------------------
080700120326       //?Gestione opzioni subfile S01.                                ?
080800120326       //--------------------------------------------------------------
080900120326       BEGSR  sr_OpzS01;
081000120326
081100120326         clear  SavS01csr;
081200120326
081300120326         // -?Ciclo di lettura subfile?
081400120326         readc  TB461S01;
081500120326
081600120326         DoW  Not %eof(TNTB461D);
081700120326
081800120326           SflNxtChg = *off;
081900120326           %subst(IndDspF : 50) = *off;
082000120326           C1RcdNbr = S01nrr;
082100120326
082200120326           select;
082300120326
082400120326             // -?Opz. 1=Selezione?
082500120326             when  S1Copz = '1'   and   Not $Fine;
082600120326               clear  P01rit;
082700120326               P01ke1 = %editc( S1Ccod : 'X' );
082800120326               clear  P01ke2;
082900120326               $Fine   = *on;
083000120327
083100120327             // -?Opz. 3=Copia?
083200120327             when  S1Copz = '3'   and   Not $Fine;
083300120327               P01rit = S1Copz;
083400120327               P01ke1 = %editc( S1Ccod : 'X' );
083500120327               clear  P01ke2;
083600120327               $Fine   = *on;
083700120326
083800120326             // -?Nessuna opzione?
083900120326             when  S1Copz = *blank;
084000120327
084100120327             // -?Ammessa una sola opzione?
084200120327             when  S1Copz <> *blank   and   $Fine;
084300120327               ErrMessage  = *on;
084400120327               ErrGenerico = *on;
084500120327               PosCurOPZ   = *on;
084600120327               V1Dmsg = $Msg(02);
084700120327               $Fine  = *off;
084800120326
084900120326             // -?Opzione errata?
085000120326             other;
085100120326               ErrMessage  = *on;
085200120326               ErrGenerico = *on;
085300120326               PosCurOPZ   = *on;
085400120326               V1Dmsg = $Msg(01);
085500120326               $Fine  = *off;
085600120326
085700120326           endsl;
085800120326
085900120326           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
086000120326           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
086100120326           if  S1Copz  <> *blank   and
086200120326              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
086300120326             SavS01csr   = C1RcdNbr;
086400120326           endif;
086500120326
086600120326           // -?Aggiornamento sfl?
086700120326           select;
086800120326             when  ErrMessage;
086900120326               SflNxtChg = *on;
087000120326               C1CsrRrn  = C1RcdNbr;
087100120326             when  ErrGenerico;
087200120326               C1CsrRrn  = C1RcdNbr;
087300120326               if  S1Copz <> '1';
087400120326                 clear  S1Copz;
087500120326               endif;
087600120326             other;
087700120326               if  S1Copz <> '1';
087800120326                 clear  S1Copz;
087900120326               endif;
088000120326           endsl;
088100120326
088200120326           if  S1Copz <> *blank;
088300120326             SflNxtChg = *on;
088400120326           endif;
088500120326
088600120326           UPDATE  TB461S01;
088700120326
088800120326           if  ErrGenerico   or   ErrMessage;
088900120326             leave;
089000120326           endif;
089100120326
089200120326           readc  TB461S01;
089300120326
089400120326         ENDDO;
089500120326
089600120326         // -?Riposizionam. cursore sul record contenente la prima opz.?
089700120326         //  ?(se non sono stati rilevati errori)?
089800120326         if  NOT ErrMessage   and   NOT ErrGenerico   and
089900120326             SavS01csr > *zero;
090000120326           C1CsrRrn = SavS01csr;
090100120326         endif;
090200120326
090300120326       ENDSR;
090400120326
090500120326       //--------------------------------------------------------------
090600120326       //?Operazioni finali.                                           ?
090700120326       //--------------------------------------------------------------
090800120326       BEGSR  sr_RoutEnd;
090900120327
091000120327         // -?Restituzione parametri?
091100120327         kpjbu = Param01;
091200120326
091300120326         // -?"Chiusura" *pgm?
091400120326         return;
091500120326
091600120326       ENDSR;
091700120326
091800120326      /end-free
091900120326
092000120326       //--------------------------------------------------------------
092100120326       //?Schiere a tempo di compilazione.                             ?
092200120326       //--------------------------------------------------------------
092300120326
092400120326** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
092500120326Opzione errata                                                                 1
092600120326Ammessa la selezione di un solo cliente                                        2
