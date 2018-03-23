000100150326       //===============================================================
000200150326       //?TNTBVMAR1 * Selezione tab. "VMA" = V.M.A. Negato.             ?
000300150326       //===============================================================
000400150326
000500150326       //--------------------------------------------------------------
000600150326       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150326       //--------------------------------------------------------------
000800150326
000900150326     /*PRM dbgview(*source)
001000150326     /*END
001100150326
001200150326       //--------------------------------------------------------------
001300150326       //?Specifiche di controllo.                                     ?
001400150326       //--------------------------------------------------------------
001500150326
001600150326     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150326     h dftactgrp(*no)
001800150326
001900150326       //--------------------------------------------------------------
002000150326       //?Dichiarazione file.                                          ?
002100150326       //--------------------------------------------------------------
002200150326
002300150326       // -?Tabelle?
002400150326     fTNTBE01L  if   e           k disk
002500150326
002600150326       // -?Video?
002700150326     fTNTBVMAD1 cf   e             workstn
002800150327     f                                     sfile(TBVMAS01 : S01nrr)
002900150326     f                                     indds(IndDspF)
003000150326     f                                     infds(InfDspF)
003100150326
003200150326       //--------------------------------------------------------------
003300150326       //?Definizione costanti.                                        ?
003400150326       //--------------------------------------------------------------
003500150326
003600150326       // -?Codice tabella in gestione?
003700150326     d c_Tab           c                   const('VMA')
003800150326
003900150326       // -?Numero massimo di record gestibili?
004000150326     d c_MaxRec        c                   const(9999)
004100150326
004200150326       // -?Tasti funzionali a video?
004300150326     d c_F01           c                   const(x'31')
004400150326     d c_F02           c                   const(x'32')
004500150326     d c_F03           c                   const(x'33')
004600150326     d c_F04           c                   const(x'34')
004700150326     d c_F05           c                   const(x'35')
004800150326     d c_F06           c                   const(x'36')
004900150326     d c_F07           c                   const(x'37')
005000150326     d c_F08           c                   const(x'38')
005100150326     d c_F09           c                   const(x'39')
005200150326     d c_F10           c                   const(x'3A')
005300150326     d c_F11           c                   const(x'3B')
005400150326     d c_F12           c                   const(x'3C')
005500150326     d c_F13           c                   const(x'B1')
005600150326     d c_F14           c                   const(x'B2')
005700150326     d c_F15           c                   const(x'B3')
005800150326     d c_F16           c                   const(x'B4')
005900150326     d c_F17           c                   const(x'B5')
006000150326     d c_F18           c                   const(x'B6')
006100150326     d c_F19           c                   const(x'B7')
006200150326     d c_F20           c                   const(x'B8')
006300150326     d c_F21           c                   const(x'B9')
006400150326     d c_F22           c                   const(x'BA')
006500150326     d c_F23           c                   const(x'BB')
006600150326     d c_F24           c                   const(x'BC')
006700150326     d c_Enter         c                   const(x'F1')
006800150326     d c_RollDown      c                   const(x'F4')
006900150326     d c_RollUp        c                   const(x'F5')
007000150326
007100150326       //--------------------------------------------------------------
007200150326       //?Definizione schiere.                                         ?
007300150326       //--------------------------------------------------------------
007400150326
007500150326       // -?Messaggi di errore?
007600150326     d sk_Msg          s             78    dim( 2)  ctdata  perrcd(1)
007700150326
007800150326       //--------------------------------------------------------------
007900150326       //?Definizione aree dati.                                       ?
008000150326       //--------------------------------------------------------------
008100150326
008200150326       // -?Dati utente?
008300150326     d §AzUte        e ds                  extname(AZUTE00F)
008400150326     d                                     dtaara
008500150326     d §DatiUte      e ds                  extname(dDatiUte)
008600150326     d                                     dtaara
008700150326
008800150326       //--------------------------------------------------------------
008900150326       //?Definizione strutture dati.                                  ?
009000150326       //--------------------------------------------------------------
009100150326
009200150326       // -?Status ds?
009300150326     d Status         sds
009400150326     d   SDSpgm          *proc
009500150326
009600150326       // -?InfDS?
009700150326     d InfDspF         ds
009800150326     d   dsp_aid             369    369a
009900150326     d   sfl_rrn             376    377i 0
010000150326     d   min_nrr             378    379i 0
010100150326     d   num_rcds            380    381i 0
010200150326
010300150326       // -?Indicatori su DspF?
010400150326     d IndDspF         ds                  inz
010500150326         // -?Abilitazione tasti funzionali?
010600150327     d   F4Attivo                      n   overlay(IndDspF : 04)
010700150326     d   F11Attivo                     n   overlay(IndDspF : 11)
010800150326         // -?Emissione messaggio di errore?
010900150326     d   ErrMessage                    n   overlay(IndDspF : 28)
011000150326         // -?Indicatori di gestione del subfile?
011100150326     d   SflDsp_N                      n   overlay(IndDspF : 30)
011200150326     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011300150326     d   SflNxtChg                     n   overlay(IndDspF : 32)
011400150326     d   SflEnd                        n   overlay(IndDspF : 33)
011500150326         // -?Indicatori per Attibuti di visualizzazione?
011600150326         //  ?o   Condizionamento visualizzazione campi?
011700150326     d   ProtectOPZ                    n   overlay(IndDspF : 41)
011800150326     d   Ord_x_RSC                     n   overlay(IndDspF : 42)
011900150326         // -?Posizionamento cursore & segnalazione errore?
012000150326     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012100150326     d   PosCurRSCw                    n   overlay(IndDspF : 51)
012200150326         //   -?Riemissione videata?
012300150326     d   ErrGenerico                   n   overlay(IndDspF : 99)
012400150326
012500150326       // -?Parametri ricevuti?
012600150326     d KPJBA         e ds
012700150327
012800150327       // -?Parametri da restituire al chiamante?
012900150327     d tntbVMAds1    e ds                  inz
013000150326
013100150326       // -?Tab. VMA = V.M.A. Negato?
013200150326     d dVMA          e ds                  inz
013300150326
013400150326       //--------------------------------------------------------------
013500150326       //?Definizione variabili globali.                               ?
013600150326       //--------------------------------------------------------------
013700150326
013800150326       // -?Flags booleani?
013900150326     d $Fine           s               n   inz
014000150326     d $EoF            s               n   inz
014100150326     d $InzS01         s               n   inz(*on)
014200150326     d $RecOK          s               n   inz
014300150326
014400150326       // -?Variabili per la gestione del video?
014500150326     d $Video          s              2    inz('S1')
014600150326     d S01nrr          s                   like(C1RcdNbr) inz
014700150326     d SavS01csr       s                   like(C1RcdNbr) inz
014800150326
014900150326       // -?Variabili per la gestione del singolo campo alla variazione?
015000150326       //  ?dello stesso?
015100150326     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
015200150326     d SavC1Crsc       s                   like(C1Crsc)   inz(*hival)
015300150326     d SavW1Crsc       s                   like(W1Crsc)   inz
015400150326
015500150326       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
015600150326
015700150326       //--------------------------------------------------------------
015800150326       // -?Constants?
015900150326       //--------------------------------------------------------------
016000150326       // -?c_MaxKey - Maximum number of key fields allowed by this program?
016100150326     d c_MaxKey        c                   const(2)
016200150326       // -?Sort order:?
016300150326       //  ?1 = Sort in an ascending sequence?
016400150326       //  ?2 = Sort in a descending sequence?
016500150326     d c_Ascendente    c                   const(1)
016600150326     d c_Discendente   c                   const(2)
016700150326       // -?Key data type:?
016800150326       //  ? 0 = Signed binary?
016900150326       //  ? 2 = Signed zoned decimal?
017000150326       //  ? 3 = Signed packed decimal?
017100150326       //  ? 6 = Character with no national language sort sequence applied,?
017200150326       //  ?     if specified?
017300150326       //  ? 7 = Unsigned packed decimal?
017400150326       //  ?     All numbers will have the sign forced positive ('F'X).?
017500150326       //  ? 8 = Unsigned zoned decimal?
017600150326       //  ?     All numbers will have the sign forced positive ('F'X).?
017700150326       //  ? 9 = Unsigned binary?
017800150326       //  ?14 = Date in form of DD/MM/YY?
017900150326       //  ?15 = Date in form of DD.MM.YYYY?
018000150326     d c_Numero        c                   const(2)
018100150326     d c_Carattere     c                   const(6)
018200150326     d c_NumIntero     c                   const(8)
018300150326       //
018400150326     d c_Put           c                   const(1)
018500150326     d c_EndPut        c                   const(2)
018600150326     d c_Get           c                   const(3)
018700150326
018800150326       //--------------------------------------------------------------
018900150326       // -?Data Structures?
019000150326       //  ?SflRcd     - The entire subfile record to pass to the sort?
019100150326       //  ?QLGSCB     - The sort request block for the QLGSORT API?
019200150326       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
019300150326       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
019400150326       //  ?QUSEC      - Error structure for the QLGSORT API?
019500150326       //--------------------------------------------------------------
019600150326     d SflRcd          ds                  inz
019700150326     d   S1Copz                            inz
019800150326     d   S1Cksc                            inz
019900150326     d   S1Crsc                            inz
020000150326     d   S1Cddt                            inz
020100150326     d   S1Cdst                            inz
020200150326     d   Selected                     1a   inz
020300150326
020400150326      /copy QSYSINC/QRPGLESRC,QLGSORT
020500150326     d QLGKL                         16    dim(c_MaxKey)
020600150326     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
020700150326     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
020800150326     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
020900150326     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
021000150326      /copy QSYSINC/QRPGLESRC,QLGSRTIO
021100150326      /copy QSYSINC/QRPGLESRC,QUSEC
021200150326
021300150326       //--------------------------------------------------------------
021400150326       // -?Standalone fields?
021500150326       //  ?Nrr        - Relative record number for subfile?
021600150326       //  ?SizeList   - Size of list?
021700150326       //  ?ReturnSize - Size of list returned by sort API?
021800150326       //--------------------------------------------------------------
021900150326     d NotUsed         s             16a   inz
022000150326     d ReturnSize      s              9b 0 inz
022100150326     d SizeList        s              9b 0 inz
022200150326     d RrnLast         s              5i 0 inz
022300150326     d Nrr             s              5i 0 inz
022400150326
022500150326       //--------------------------------------------------------------
022600150326       //?Definizione prototipi procedure.                             ?
022700150326       //--------------------------------------------------------------
022800150326
022900150326       // -?Reperimento dati utente?
023000150326     d TIBS34ds      e ds                  inz
023100150326      /copy gaitrasrc/srcProtoPR,TIBS34R
023200150326
023300150326       // -?Controllo/Decodifica cliente?
023400150326      /copy gaitrasrc/srcProtoPI,TIBS69R
023500150326      /copy gaitrasrc/srcProtoPR,TIBS69R
023600150326
023700150326       // -?Ordinamento subfile?
023800150326      /copy gaitrasrc/srcProtoPr,QLGSRTIO
023900150326
024000150326       //--------------------------------------------------------------
024100150326       //?Definizione key-list.                                        ?
024200150326       //--------------------------------------------------------------
024300150326
024400150326       // -?File TNTBE01L?
024500150326     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024600150326     d                                     prefix(k_)   inz
024700150326
024800150326       //--------------------------------------------------------------
024900150326       //?Riepilogo indicatori utilizzati.                             ?
025000150326       //--------------------------------------------------------------
025100150326
025200150326
025300150326       //--------------------------------------------------------------
025400150326       //?M A I N - L I N E                                            ?
025500150326       //--------------------------------------------------------------
025600150326
025700150326     c     *Entry        plist
025800150326     c                   parm                    KPJBA
025900150326
026000150326      /free
026100150326
026200150326       // -?Operazioni iniziali?
026300150326       exsr sr_RoutInz;
026400150326
026500150326       // -?Ciclo di gestione del file video?
026600150326       DoW  $Fine = *off;
026700150326         select;
026800150326           // -?Gestione videata S1 (subfile)?
026900150326           when $Video = 'S1';
027000150326             exsr sr_GesS01;
027100150326           // -?? ? ??
027200150326           other;
027300150326             $Fine = *on;
027400150326         endsl;
027500150326       EndDo;
027600150326
027700150326       // -?Operazioni finali?
027800150326       exsr sr_RoutEnd;
027900150326
028000150326       //--------------------------------------------------------------
028100150326       //?Operazioni iniziali.                                         ?
028200150326       //--------------------------------------------------------------
028300150326       BEGSR  sr_RoutInz;
028400150326
028500150326         // -?Impostazione chiusura?
028600150326         *inLR = *on;
028700150326
028800150326         // -?Reperimento dati job?
028900150326         exsr  sr_DatiJob;
029000150326
029100150326         // -?Impostazione nome programma a video?
029200150326         V1Tpgm = SDSpgm;
029300150326
029400150326         // -?Impostazione iniziale / pulizia dei campi chiave?
029500150326         clear  k05tntbe01;
029600150326         k_TBEcod = c_Tab;
029700150326
029800150326         // -?Impostazione valori di default?
029900150326         Ord_x_RSC = *off;
030000150327         clear  KPJBU;
030100150327         clear  tntbVMAds1;
030200150327         oVMAerr   = *off;
030300150326
030400150326       ENDSR;
030500150326
030600150326       //--------------------------------------------------------------
030700150326       //?Reperimento Dati del job (Utente/Operativi).                 ?
030800150326       //--------------------------------------------------------------
030900150326       BEGSR  sr_DatiJob;
031000150326
031100150326         in(e) §AzUte;
031200150326         if NOT %error;
031300150326           in(e) §DatiUte;
031400150326         endif;
031500150326         if %error or RSut = *blank;
031600150326           tibs34r ( tibs34ds );
031700150326           in §AzUte;
031800150326           in §DatiUte;
031900150326         endif;
032000150326
032100150326       ENDSR;
032200150326
032300150326       //--------------------------------------------------------------
032400150326       //?Gestione subfile S01.                                        ?
032500150326       //--------------------------------------------------------------
032600150326       BEGSR  sr_GesS01;
032700150326
032800150326         // -?Inizializzazione videata?
032900150326         if  $InzS01 = *on;
033000150326           exsr  sr_InzS01;
033100150326           $InzS01 = *off;
033200150326         endif;
033300150326
033400150326         // -?Emissione Testata & Piede?
033500150327         write  TBVMAT01;
033600150327         write  TBVMAP01;
033700150326
033800150326         // -?Posizionamento cursore?
033900150326         if  C1CsrRrn > *zero;
034000150326           C1RcdNbr = C1CsrRrn;
034100150326         else;
034200150326           C1RcdNbr = 1;
034300150327           write  TBVMAS00;         //?(no rec.)?
034400150326         endif;
034500150326
034600150326         // -?Emissione videata?
034700150327         exfmt  TBVMAC01;
034800150326
034900150326         clear  ErrGenerico;
035000150326         clear  ErrMessage;
035100150326         clear  V1Dmsg;
035200150326
035300150326         select;
035400150326
035500150326           // -?F3=Fine?
035600150326           when  dsp_aid = c_F03;
035700150327             oVMAfxx = '03';
035800150327             clear  oVMAcli;
035900150326             $Fine  = *on;
036000150326
036100150326           // -?F4=Ricerca per Rag.Soc. cliente?
036200150326           when  dsp_aid = c_F04;
036300150326             exsr  sr_F04S01;
036400150326
036500150326           // -?F5=Refresh?
036600150326           //when  dsp_aid = c_F05;
036700150326           //  $InzS01 = *on;
036800150326
036900150326           // -?F11=Ordinamento?
037000150326           when  dsp_aid = c_F11;
037100150326             exsr  sr_F11S01;
037200150326
037300150326           // -?F12=Ritorno?
037400150326           when  dsp_aid = c_F12;
037500150327             oVMAfxx = '12';
037600150327             clear  oVMAcli;
037700150326             $Fine  = *on;
037800150326
037900150326           // -?Invio?
038000150326           other;
038100150326             // -?Gestione opzioni?
038200150326             if  S01nrr > *zero;
038300150326               exsr  sr_OpzS01;
038400150326               if  ErrGenerico;
038500150326                 leavesr;
038600150326               endif;
038700150326             endif;
038800150326
038900150326             // -?Richiesto posizionamento?
039000150326             if  (Not Ord_x_RSC   and   C1Cksc <> SavC1Cksc)   OR
039100150326                 (Ord_x_RSC       and   C1Crsc <> SavC1Crsc);
039200150326               $InzS01 = *on;
039300150326             endif;
039400150326
039500150326         endsl;
039600150326
039700150326       ENDSR;
039800150326
039900150326       //--------------------------------------------------------------
040000150326       //?Inizializzazione subfile S01.                                ?
040100150326       //--------------------------------------------------------------
040200150326       BEGSR  sr_InzS01;
040300150326
040400150326         // -?Spegnimento degli indicatori di errore?
040500150326         %subst(IndDspF : 50) = *off;
040600150326
040700150326         // -?Pulizia subfile control?
040800150326         if  Not Ord_x_RSC;
040900150326           clear  C1Crsc;
041000150326           reset  SavC1Crsc;
041100150326         else;
041200150326           clear  C1Cksc;
041300150326           reset  SavC1Cksc;
041400150326         endif;
041500150326
041600150326         // -?Pulizia subfile?
041700150326         SflDsp_N    = *on;
041800150326         SflDspCtl_N = *on;
041900150327         write  TBVMAC01;
042000150326         SflDspCtl_N = *off;
042100150326         SflEnd      = *off;
042200150326
042300150326         clear  C1RcdNbr;
042400150326         clear  C1CsrRrn;
042500150326         clear  S01nrr;
042600150326         clear  V1Dmsg;
042700150326         ErrMessage  = *off;
042800150326         ErrGenerico = *off;
042900150326
043000150326         // -?Caricamento completo dei dati nel subfile?
043100150326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
043200150326         //  ?l'eventuale ordinamento)?
043300150326         setll  %kds(k05tntbe01)  TNTBE000;
043400150326         reade  %kds(k05tntbe01 : 1)  TNTBE000;
043500150326         $EoF = (%eof(TNTBE01L));
043600150326         exsr  sr_CaricaS01;
043700150326
043800150326         // -?Impaginazione subfile?
043900150326         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
044000150326         if S01nrr > *zero;
044100150326           C1RcdNbr  = 1;
044200150326           C1CsrRrn  = 1;
044300150326         else;
044400150326           clear C1RcdNbr;
044500150326         endif;
044600150326
044700150326         // -?Ordinamento subfile (se premuto F11)?
044800150327         if  Ord_x_RSC   and   S01nrr > *zero;
044900150326           exsr  sr_SortSfl;
045000150326         endif;
045100150326
045200150326         // -?(Dis)Attivazione tasti funzionali?
045300150327         // ·?F4=Ricerca cliente x Rag.Soc.?
045400150327         //F4Attivo  = (S01nrr > *zero);
045500150327         F4Attivo  = *off;
045600150326         // ·?F11=Ordinamento x Rasg.Soc. cliente?
045700150327         F11Attivo = (S01nrr > *zero);
045800150326
045900150326       ENDSR;
046000150326
046100150326       //--------------------------------------------------------------
046200150326       //?Caricamento completo del subfile S01                         ?
046300150326       //--------------------------------------------------------------
046400150326       BEGSR  sr_CaricaS01;
046500150326
046600150326         clear  S01nrr;
046700150326         SflNxtChg = *off;
046800150326
046900150326         // -?Ciclo di caricamento dati da tab. "3EW"?
047000150326         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
047100150326         //  ?l'eventuale ordinamento)?
047200150326         DoW  Not $EoF;
047300150326
047400150326           // -?Selezione singolo record file TNTBE00F?
047500150326           if  Not $EoF;
047600150326             exsr  sr_SelezRec;
047700150326           endif;
047800150326           if  $EoF;
047900150326             leave;
048000150326           endif;
048100150326
048200150326           // -?Registrazione singolo record nel subfile?
048300150326           if  $RecOK;
048400150326
048500150326             // ·?Impostazione campi?
048600150327             clear  TBVMAS01;
048700150326             S1Cksc = %int( %subst( TBEke1 : 1 : 7 ) );
048800150326             S1Crsc = %subst( ACOrag : 1 : %len(S1Crsc) );
048900150326             if  §VMAddt > *zero;
049000150326               S1Cddt = %int( %subst( %editc( §VMAddt : 'X' ) : 7 : 2 )
049100150326                            + %subst( %editc( §VMAddt : 'X' ) : 5 : 2 )
049200150326                            + %subst( %editc( §VMAddt : 'X' ) : 1 : 4 ) );
049300150326             endif;
049400150326             if  §VMAdst > *zero;
049500150326               S1Cdst = %int( %subst( %editc( §VMAdst : 'X' ) : 7 : 2 )
049600150326                            + %subst( %editc( §VMAdst : 'X' ) : 5 : 2 )
049700150326                            + %subst( %editc( §VMAdst : 'X' ) : 1 : 4 ) );
049800150326             endif;
049900150326
050000150326             // -?Scrittura record nel subfile?
050100150326             S01nrr += 1;
050200150327             write  TBVMAS01;
050300150326
050400150326           EndIf;
050500150326
050600150326           // -?Lettura record successivo?
050700150326           reade  %kds(k05tntbe01 : 1)  TNTBE000;
050800150326           $EoF = (%eof(TNTBE01L));
050900150326
051000150326         EndDo;
051100150326
051200150326         // -?Memorizzazione posizionamento effettuato?
051300150326         if  Not Ord_x_RSC;
051400150326           SavC1Cksc = C1Cksc;
051500150326         else;
051600150326           SavC1Crsc = C1Crsc;
051700150326         endif;
051800150326
051900150326         // -?Visualizzazione del SFL (se ci sono dati)?
052000150326         SflDsp_N = (S01nrr <= *zero);
052100150326
052200150326         // -?Attivazione del SFLEND?
052300150326         SflEnd = ( $EoF );
052400150326
052500150326         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
052600150326         if  S01nrr > *zero;
052700150326           C1RcdNbr = 1;
052800150326         else;
052900150326           clear  C1RcdNbr;
053000150326         endif;
053100150326
053200150326         C1CsrRrn = C1RcdNbr;
053300150326
053400150326       ENDSR;
053500150326
053600150326       //--------------------------------------------------------------
053700150326       //?Selezione del singolo record letto dalla tabella "VMA".      ?
053800150326       //--------------------------------------------------------------
053900150326       BEGSR  sr_SelezRec;
054000150326
054100150326         reset  $RecOK;
054200150326
054300150326         dVMA = TBEuni;
054400150326
054500150326         select;
054600150326
054700150326           // -?Omissione records annullati?
054800150326           when  TBEatb <> *blank;
054900150326             leavesr;
055000150326
055100150326           // -?Richiesto posizionamento per codice cliente:?
055200150326           //  ?si scartano i clienti con codice inferiore?
055300150326           when  Not Ord_x_RSC  and  TBEke1 < %editc( C1Cksc : 'X' );
055400150326             leavesr;
055500150326
055600150326         endsl;
055700150326
055800150326         // -?Decodifica cliente?
055900150326         clear  TIBS69ds;
056000150326         I69sif = knsif;
056100150326         I69kcc = DUTkci;
056200150326         I69kac = %int( %subst( TBEke1 : 1 : 7 ) );
056300150326         tibs69r( tibs69ds :
056400150326                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
056500150326         if  O69err = *on;
056600150326           ACOrag = *all'? ';
056700150326         endif;
056800150326
056900150326         select;
057000150326
057100150326           // -?Richiesto posizionamento per ragione sociale cliente:?
057200150326           //  ?si scartano i clienti con ragione sociale inferiore?
057300150327           when  Ord_x_RSC  and  ACOrag < C1Crsc;
057400150326             leavesr;
057500150326
057600150326           // -?Selezione per descrizione (ragione sociale cliente)?
057700150326           when  W1Crsc <> *blank   and
057800150326                 %scan( %trimr( SavW1Crsc ) : ACOrag ) = *zero;
057900150326             leavesr;
058000150326
058100150326         endsl;
058200150326
058300150326         $RecOK = *on;
058400150326
058500150326       ENDSR;
058600150326
058700150326       //--------------------------------------------------------------
058800150326       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
058900150326       //?F4=Ricerca per ragione sociale cliente                       ?
059000150326       //--------------------------------------------------------------
059100150326       BEGSR  sr_F04S01;
059200150326
059300150326         // -?Emissione window W01?
059400150327         exfmt  TBVMAW01;
059500150326
059600150326         reset  ErrGenerico;
059700150326         reset  ErrMessage;
059800150326         clear  V1Dmsg;
059900150326
060000150326         Select;
060100150326
060200150326           // -?F12=Ritorno?
060300150326           When  dsp_aid = c_F12;
060400150326             $Video = 'S1';
060500150326             leavesr;
060600150326
060700150326           // -?Invio?
060800150326           Other;
060900150326             if  W1Crsc <> SavW1Crsc;
061000150326               $InzS01   = *on;
061100150326               SavW1Crsc = W1Crsc;
061200150326             endif;
061300150326             if  W1Crsc = *blank;
061400150326               clear  C1Ddes;
061500150326             else;
061600150326               C1Ddes = 'Ricerca: ' + %trim(W1Crsc);
061700150326             endif;
061800150326             leavesr;
061900150326
062000150326         EndSl;
062100150326
062200150326       ENDSR;
062300150326
062400150326       //--------------------------------------------------------------
062500150326       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
062600150326       //?F11=Nuovo ordinamento sfl                                    ?
062700150326       //--------------------------------------------------------------
062800150326       BEGSR  sr_F11S01;
062900150326
063000150326         Ord_x_RSC = (Not Ord_x_RSC);
063100150326
063200150326         select;
063300150326           // -?Subfile vuoto: nessun record da ordinare?
063400150326           when  S01nrr = *zero;
063500150326           // -?Subfile già posizionato: occorre ricaricarlo?
063600150326           when  C1Cksc <> *zero  or  C1Crsc <> *blank;
063700150326             $InzS01 = *on;
063800150326           // -?Altrimenti: basta riordinarlo?
063900150326           other;
064000150326             exsr  sr_SortSfl;
064100150326         endsl;
064200150326
064300150326         if  Not Ord_x_RSC;
064400150326           clear  C1Cksc;
064500150326           clear  SavC1Cksc;
064600150326         else;
064700150326           clear  C1Crsc;
064800150326           clear  SavC1Crsc;
064900150326         endif;
065000150326
065100150326       ENDSR;
065200150326
065300150326       //--------------------------------------------------------------
065400150326       //?Ordinamento subfile                                          ?
065500150326       //?  This subroutine sorts the subfile records.                 ?
065600150326       //--------------------------------------------------------------
065700150326       BEGSR  sr_SortSfl;
065800150326
065900150326         RrnLast  = S01nrr;
066000150326
066100150326         C1RcdNbr = 1;
066200150326         clear  C1CsrRrn;
066300150326         clear  S01nrr;
066400150326         clear  V1Dmsg;
066500150326         ErrMessage  = *off;
066600150326         SflNxtChg   = *off;
066700150326         ErrGenerico = *off;
066800150326         %subst(IndDspF : 50) = *off;
066900150326
067000150326         //?___________________________________________________________?
067100150326         //?Initialize the key fields to sort on.?
067200150326         //?There is one extra field not in the subfile -- Selected --?
067300150326         //?that is added to the record. This field is used to place?
067400150326         //?selected records in front of nonselected records.?
067500150326         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
067600150326
067700150326         clear  QLGKL;
067800150326         clear  QLGSKL;
067900150326         clear  QLGSCB;
068000150326         clear  QLGSCB00;
068100150326
068200150326         // -?Gestione della scelta ordinamento:?
068300150326         // -?Ordinamento per Rag.Soc./Codice cliente?
068400150326         If  Ord_x_RSC = *on;
068500150326           // ·?2 campi chiave?
068600150326           QLGNBRK  = 2;
068700150326           // ·?1° campo: padre (S1Crsc)?
068800150326           //            ?a posizone    7, 35 byte, char, ascending.?
068900150326           QLGSP    = %size(S1Copz) + %size(S1Cksc) + 1;
069000150326           QLGSS    = %size(S1Crsc);
069100150326           QLGDT    = c_Carattere;
069200150326           QLGSO    = c_Ascendente;
069300150326           QLGKL(1) = QLGSKL;
069400150326           // ·?2° campo: codice (S1Cksc)?
069500150326           //            ?a posizone    2,  7 byte,  int, ascending.?
069600150326           QLGSP    = %size(S1Copz) + 1;
069700150326           QLGSS    = %size(S1Cksc);
069800150326           QLGDT    = c_NumIntero;
069900150326           QLGSO    = c_Ascendente;
070000150326           QLGKL(2) = QLGSKL;
070100150326
070200150326         // -?Ordinamento per Codice cliente?
070300150326         Else;
070400150326           // ·?1 campi chiave?
070500150326           QLGNBRK  = 1;
070600150326           // ·?1° campo: cliente (S1Cksc)?
070700150326           //            ?a posizone    2,  7 byte,  int, ascending.?
070800150326           QLGSP    = %size(S1Copz) + 1;
070900150326           QLGSS    = %size(S1Cksc);
071000150326           QLGDT    = c_NumIntero;
071100150326           QLGSO    = c_Ascendente;
071200150326           QLGKL(1) = QLGSKL;
071300150326
071400150326         EndIf;
071500150326
071600150326         // -?Load other sort parameters.?
071700150326         QLGLB   = 80 + 16 * c_MaxKey;
071800150326         QLGRL   = %size(SflRcd) - 1;
071900150326         QLGRT   = 8;
072000150326         QLGOKL  = 80;
072100150326         QLGLKE  = 16;
072200150326         QLGLSS  = 290;
072300150326         // -?Initialize Sort I/O API fields.?
072400150326         QLGRL00 = QLGRL;
072500150326         QLGRC00 = 1;
072600150326         clear  QUSEI;
072700150326         QUSBPRV = %size(QUSEC);
072800150326
072900150326         // -?First step - Initialize the sort routine.?
073000150326         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
073100150326                      SizeList : ReturnSize : QUSEC );
073200150326
073300150326         // -?Next step - Write records to I/O routine.?
073400150326         QLGRT00  = c_Put;
073500150326         For  Nrr = 1  To  RrnLast;
073600150327           chain  Nrr  TBVMAS01;
073700150326           // -?Solo le righe con Selected = 'Y' sono riordinate,?
073800150326           //  ?quindi per fare un ordinamento di tutte le righe?
073900150326           //  ?metto 'Y' sempre.?
074000150326           Selected = 'Y';
074100150326           clear  QUSEI;
074200150326           QUSBPRV  = %size(QUSEC);
074300150326           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
074400150326                         SizeList : NotUsed : QUSEC );
074500150326         EndFor;
074600150326
074700150326         // -?Next step - Signal end of input, clear subfile for reload.?
074800150326         QLGRT00 = c_EndPut;
074900150326         clear  QUSEI;
075000150326         QUSBPRV = %size(QUSEC);
075100150326         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
075200150326                       SizeList : NotUsed : QUSEC );
075300150326
075400150326         // -?Pulizia subfile?
075500150326         SflDsp_N    = *on;
075600150326         SflDspCtl_N = *on;
075700150327         write  TBVMAC01;
075800150326         SflDspCtl_N = *off;
075900150326         SflEnd      = *off;
076000150326
076100150326         // -?Final step - Write the records back to the subfile.?
076200150326         QLGRT00  = c_Get;
076300150326         For  Nrr = 1  To RrnLast;
076400150326           clear  QUSEI;
076500150326           QUSBPRV = %size(QUSEC);
076600150326           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
076700150326                         QLGRL00  : NotUsed : QUSEC );
076800150326           // -?Ripristino indicatori utilizzati nel sfl rec?
076900150326           SflNxtChg = (S1Copz <> *blank);
077000150326           S01nrr = Nrr;
077100150327           write  TBVMAS01;
077200150326         EndFor;
077300150326
077400150326         C1CsrRrn = 1;
077500150326
077600150326         // -?Visualizzazione del SFL (se ci sono dati)?
077700150326         SflDsp_N = (S01nrr <= *zero);
077800150326
077900150326         // -?Attivazione del SFLEND?
078000150326         SflEnd = $EoF;
078100150326
078200150326       ENDSR;
078300150326
078400150326       //--------------------------------------------------------------
078500150326       //?Gestione opzioni subfile S01.                                ?
078600150326       //--------------------------------------------------------------
078700150326       BEGSR  sr_OpzS01;
078800150326
078900150326         clear  SavS01csr;
079000150326
079100150326         // -?Ciclo di lettura subfile?
079200150327         readc  TBVMAS01;
079300150326
079400150326         DoW  Not %eof(TNTBVMAD1);
079500150326
079600150326           SflNxtChg = *off;
079700150326           %subst(IndDspF : 50) = *off;
079800150326           C1RcdNbr = S01nrr;
079900150326
080000150326           Select;
080100150326
080200150326             // -?Nessuna opzione?
080300150326             When  S1Copz = *blank;
080400150326
080500150326             // -?Opz. 1=Selezione?
080600150326             When  S1Copz = '1'  and  Not $Fine;
080700150327               oVMAcli = S1Cksc;
080800150326               $Fine = *on;
080900150326
081000150326             // -?Ammessa una sola opzione?
081100150326             When  S1Copz = '1'  and  $Fine;
081200150326               ErrGenerico = *on;
081300150326               ErrMessage  = *on;
081400150326               PosCurOPZ   = *on;
081500150326               V1Dmsg = sk_Msg(02);
081600150326               $Fine  = *off;
081700150327               clear  oVMAcli;
081800150326
081900150326             // -?Opzione errata?
082000150326             Other;
082100150326               ErrGenerico = *on;
082200150326               ErrMessage  = *on;
082300150326               PosCurOPZ   = *on;
082400150326               V1Dmsg = sk_Msg(01);
082500150326               $Fine  = *off;
082600150326
082700150326           EndSl;
082800150326
082900150326           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
083000150326           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
083100150326           if  S1Copz  <> *blank   and
083200150326              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
083300150326             SavS01csr   = C1RcdNbr;
083400150326           endif;
083500150326
083600150326           // -?Aggiornamento Subfile?
083700150326           select;
083800150326             when  ErrMessage;
083900150326               SflNxtChg = *on;
084000150326               C1CsrRrn  = C1RcdNbr;
084100150326             when  ErrGenerico;
084200150326               C1CsrRrn  = C1RcdNbr;
084300150326               if  S1Copz <> '1';
084400150326                 clear  S1Copz;
084500150326               endif;
084600150326             other;
084700150326               if  S1Copz <> '1';
084800150326                 clear  S1Copz;
084900150326               endif;
085000150326           endsl;
085100150326
085200150326           if  S1Copz <> *blank;
085300150326             SflNxtChg = *on;
085400150326           endif;
085500150326
085600150327           UPDATE  TBVMAS01;
085700150326
085800150326           if  ErrGenerico   or   ErrMessage;
085900150326             leave;
086000150326           endif;
086100150326
086200150327           readc  TBVMAS01;
086300150326
086400150326         EndDo;
086500150326
086600150326         // -?Riposizionam. cursore sul record contenente la prima opz.?
086700150326         //  ?(se non sono stati rilevati errori)?
086800150326         if  NOT ErrMessage   and   NOT ErrGenerico   and
086900150326             SavS01csr > *zero;
087000150326           C1CsrRrn = SavS01csr;
087100150326         endif;
087200150326
087300150326       ENDSR;
087400150326
087500150326       //--------------------------------------------------------------
087600150326       //?Operazioni finali.                                           ?
087700150326       //--------------------------------------------------------------
087800150326       BEGSR  sr_RoutEnd;
087900150327
088000150327         // -?Chiusura applicazioni precedentemente aperte?
088100150327         reset  TIBS69ds;
088200150327         tibs69r ( tibs69ds :
088300150327                   ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
088400150326
088500150327         // -?Restituzione parametri in uscita?
088600150327         KPJBU = tntbVMAds1;
088700150327
088800150327         // -?Uscita?
088900150326         return;
089000150326
089100150326       ENDSR;
089200150326
089300150326      /end-free
089400150326
089500150326       //--------------------------------------------------------------
089600150326       //?Schiere a tempo di compilazione.                             ?
089700150326       //--------------------------------------------------------------
089800150326
089900150326** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
090000150326Opzione errata                                                                 1
090100150326Ammessa la selezione di un solo cliente                                        2
