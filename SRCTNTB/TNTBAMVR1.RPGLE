000100160701       //===============================================================
000200160701       //?TNTBAMVR1 * Selezione tab. "AMV" = Autorizzaz. MAI VanDerLande?
000300160701       //===============================================================
000400160701
000500160701       //--------------------------------------------------------------
000600160701       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160701       //--------------------------------------------------------------
000800160701
000900160701     /*PRM dbgview(*source)
001000160701     /*END
001100160701
001200160701       //--------------------------------------------------------------
001300160701       //?Specifiche di controllo.                                     ?
001400160701       //--------------------------------------------------------------
001500160701
001600160701     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160701     h dftactgrp(*no)
001800160701
001900160701       //--------------------------------------------------------------
002000160701       //?Dichiarazione file.                                          ?
002100160701       //--------------------------------------------------------------
002200160701
002300160701       // -?Tabelle?
002400160701     fTNTBE01L  if   e           k disk
002500160701
002600160701       // -?Video?
002700160701     fTNTBAMVD1 cf   e             workstn
002800160701     f                                     sfile(TBAMVS01 : S01nrr)
002900160701     f                                     indds(IndDspF)
003000160701     f                                     infds(InfDspF)
003100160701
003200160701       //--------------------------------------------------------------
003300160701       //?Definizione costanti.                                        ?
003400160701       //--------------------------------------------------------------
003500160701
003600160701       // -?Codice tabella in gestione?
003700160701     d c_Tab           c                   const('AMV')
003800160701
003900160701       // -?Numero massimo di record gestibili?
004000160701     d c_MaxRec        c                   const(9999)
004100160701
004200160701       // -?Tasti funzionali a video?
004300160701     d c_F01           c                   const(x'31')
004400160701     d c_F02           c                   const(x'32')
004500160701     d c_F03           c                   const(x'33')
004600160701     d c_F04           c                   const(x'34')
004700160701     d c_F05           c                   const(x'35')
004800160701     d c_F06           c                   const(x'36')
004900160701     d c_F07           c                   const(x'37')
005000160701     d c_F08           c                   const(x'38')
005100160701     d c_F09           c                   const(x'39')
005200160701     d c_F10           c                   const(x'3A')
005300160701     d c_F11           c                   const(x'3B')
005400160701     d c_F12           c                   const(x'3C')
005500160701     d c_F13           c                   const(x'B1')
005600160701     d c_F14           c                   const(x'B2')
005700160701     d c_F15           c                   const(x'B3')
005800160701     d c_F16           c                   const(x'B4')
005900160701     d c_F17           c                   const(x'B5')
006000160701     d c_F18           c                   const(x'B6')
006100160701     d c_F19           c                   const(x'B7')
006200160701     d c_F20           c                   const(x'B8')
006300160701     d c_F21           c                   const(x'B9')
006400160701     d c_F22           c                   const(x'BA')
006500160701     d c_F23           c                   const(x'BB')
006600160701     d c_F24           c                   const(x'BC')
006700160701     d c_Enter         c                   const(x'F1')
006800160701     d c_RollDown      c                   const(x'F4')
006900160701     d c_RollUp        c                   const(x'F5')
007000160701
007100160701       //--------------------------------------------------------------
007200160701       //?Definizione schiere.                                         ?
007300160701       //--------------------------------------------------------------
007400160701
007500160701       // -?Messaggi di errore?
007600160701     d sk_Msg          s             78    dim( 2)  ctdata  perrcd(1)
007700160701
007800160701       //--------------------------------------------------------------
007900160701       //?Definizione aree dati.                                       ?
008000160701       //--------------------------------------------------------------
008100160701
008200160701       // -?Dati utente?
008300160701     d §AzUte        e ds                  extname(AZUTE00F)
008400160701     d                                     dtaara
008500160701     d §DatiUte      e ds                  extname(dDatiUte)
008600160701     d                                     dtaara
008700160701
008800160701       //--------------------------------------------------------------
008900160701       //?Definizione strutture dati.                                  ?
009000160701       //--------------------------------------------------------------
009100160701
009200160701       // -?Status ds?
009300160701     d Status         sds
009400160701     d   SDSpgm          *proc
009500160701
009600160701       // -?InfDS?
009700160701     d InfDspF         ds
009800160701     d   dsp_aid             369    369a
009900160701     d   sfl_rrn             376    377i 0
010000160701     d   min_nrr             378    379i 0
010100160701     d   num_rcds            380    381i 0
010200160701
010300160701       // -?Indicatori su DspF?
010400160701     d IndDspF         ds                  inz
010500160701         // -?Abilitazione tasti funzionali?
010600160701     d   F4Attivo                      n   overlay(IndDspF : 04)
010700160701     d   F11Attivo                     n   overlay(IndDspF : 11)
010800160701         // -?Emissione messaggio di errore?
010900160701     d   ErrMessage                    n   overlay(IndDspF : 28)
011000160701         // -?Indicatori di gestione del subfile?
011100160701     d   SflDsp_N                      n   overlay(IndDspF : 30)
011200160701     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011300160701     d   SflNxtChg                     n   overlay(IndDspF : 32)
011400160701     d   SflEnd                        n   overlay(IndDspF : 33)
011500160701         // -?Indicatori per Attibuti di visualizzazione?
011600160701         //  ?o   Condizionamento visualizzazione campi?
011700160701     d   ProtectOPZ                    n   overlay(IndDspF : 41)
011800160701     d   Ord_x_RSC                     n   overlay(IndDspF : 42)
011900160701         // -?Posizionamento cursore & segnalazione errore?
012000160701     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012100160701     d   PosCurRSCw                    n   overlay(IndDspF : 51)
012200160701         //   -?Riemissione videata?
012300160701     d   ErrGenerico                   n   overlay(IndDspF : 99)
012400160701
012500160701       // -?Parametri ricevuti?
012600160701     d KPJBA         e ds
012700160701
012800160701       // -?Parametri da restituire al chiamante?
012900160701     d tntbAMVds1    e ds                  inz
013000160701
013100160701       // -?Tab. AMV = Autorizzazioni MAI VanDerLande?
013200160701     d dAMV          e ds                  inz
013300160701
013400160701       //--------------------------------------------------------------
013500160701       //?Definizione variabili globali.                               ?
013600160701       //--------------------------------------------------------------
013700160701
013800160701       // -?Flags booleani?
013900160701     d $Fine           s               n   inz
014000160701     d $EoF            s               n   inz
014100160701     d $InzS01         s               n   inz(*on)
014200160701     d $RecOK          s               n   inz
014300160701
014400160701       // -?Variabili per la gestione del video?
014500160701     d $Video          s              2    inz('S1')
014600160701     d S01nrr          s                   like(C1RcdNbr) inz
014700160701     d SavS01csr       s                   like(C1RcdNbr) inz
014800160701
014900160701       // -?Variabili per la gestione del singolo campo alla variazione?
015000160701       //  ?dello stesso?
015100160701     d SavC1Cksc       s                   like(C1Cksc)   inz(*hival)
015200160701     d SavC1Crsc       s                   like(C1Crsc)   inz(*hival)
015300160701     d SavW1Crsc       s                   like(W1Crsc)   inz
015400160701
015500160701       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
015600160701
015700160701       //--------------------------------------------------------------
015800160701       // -?Constants?
015900160701       //--------------------------------------------------------------
016000160701       // -?c_MaxKey - Maximum number of key fields allowed by this program?
016100160701     d c_MaxKey        c                   const(2)
016200160701       // -?Sort order:?
016300160701       //  ?1 = Sort in an ascending sequence?
016400160701       //  ?2 = Sort in a descending sequence?
016500160701     d c_Ascendente    c                   const(1)
016600160701     d c_Discendente   c                   const(2)
016700160701       // -?Key data type:?
016800160701       //  ? 0 = Signed binary?
016900160701       //  ? 2 = Signed zoned decimal?
017000160701       //  ? 3 = Signed packed decimal?
017100160701       //  ? 6 = Character with no national language sort sequence applied,?
017200160701       //  ?     if specified?
017300160701       //  ? 7 = Unsigned packed decimal?
017400160701       //  ?     All numbers will have the sign forced positive ('F'X).?
017500160701       //  ? 8 = Unsigned zoned decimal?
017600160701       //  ?     All numbers will have the sign forced positive ('F'X).?
017700160701       //  ? 9 = Unsigned binary?
017800160701       //  ?14 = Date in form of DD/MM/YY?
017900160701       //  ?15 = Date in form of DD.MM.YYYY?
018000160701     d c_Numero        c                   const(2)
018100160701     d c_Carattere     c                   const(6)
018200160701     d c_NumIntero     c                   const(8)
018300160701       //
018400160701     d c_Put           c                   const(1)
018500160701     d c_EndPut        c                   const(2)
018600160701     d c_Get           c                   const(3)
018700160701
018800160701       //--------------------------------------------------------------
018900160701       // -?Data Structures?
019000160701       //  ?SflRcd     - The entire subfile record to pass to the sort?
019100160701       //  ?QLGSCB     - The sort request block for the QLGSORT API?
019200160701       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
019300160701       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
019400160701       //  ?QUSEC      - Error structure for the QLGSORT API?
019500160701       //--------------------------------------------------------------
019600160701     d SflRcd          ds                  inz
019700160701     d   S1Copz                            inz
019800160701     d   S1Cksc                            inz
019900160701     d   S1Crsc                            inz
020000160701     d   S1Cdti                            inz
020100160701     d   S1Cdtf                            inz
020200160701     d   Selected                     1a   inz
020300160701
020400160701      /copy QSYSINC/QRPGLESRC,QLGSORT
020500160701     d QLGKL                         16    dim(c_MaxKey)
020600160701     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
020700160701     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
020800160701     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
020900160701     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
021000160701      /copy QSYSINC/QRPGLESRC,QLGSRTIO
021100160701      /copy QSYSINC/QRPGLESRC,QUSEC
021200160701
021300160701       //--------------------------------------------------------------
021400160701       // -?Standalone fields?
021500160701       //  ?Nrr        - Relative record number for subfile?
021600160701       //  ?SizeList   - Size of list?
021700160701       //  ?ReturnSize - Size of list returned by sort API?
021800160701       //--------------------------------------------------------------
021900160701     d NotUsed         s             16a   inz
022000160701     d ReturnSize      s              9b 0 inz
022100160701     d SizeList        s              9b 0 inz
022200160701     d RrnLast         s              5i 0 inz
022300160701     d Nrr             s              5i 0 inz
022400160701
022500160701       //--------------------------------------------------------------
022600160701       //?Definizione prototipi procedure.                             ?
022700160701       //--------------------------------------------------------------
022800160701
022900160701       // -?Reperimento dati utente?
023000160701     d TIBS34ds      e ds                  inz
023100160701      /copy gaitrasrc/srcProtoPR,TIBS34R
023200160701
023300160701       // -?Controllo/Decodifica cliente?
023400160701      /copy gaitrasrc/srcProtoPI,TIBS69R
023500160701      /copy gaitrasrc/srcProtoPR,TIBS69R
023600160701
023700160701       // -?Ordinamento subfile?
023800160701      /copy gaitrasrc/srcProtoPr,QLGSRTIO
023900160701
024000160701       //--------------------------------------------------------------
024100160701       //?Definizione key-list.                                        ?
024200160701       //--------------------------------------------------------------
024300160701
024400160701       // -?File TNTBE01L?
024500160701     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024600160701     d                                     prefix(k_)   inz
024700160701
024800160701       //--------------------------------------------------------------
024900160701       //?Riepilogo indicatori utilizzati.                             ?
025000160701       //--------------------------------------------------------------
025100160701
025200160701
025300160701       //--------------------------------------------------------------
025400160701       //?M A I N - L I N E                                            ?
025500160701       //--------------------------------------------------------------
025600160701
025700160701     c     *Entry        plist
025800160701     c                   parm                    KPJBA
025900160701
026000160701      /free
026100160701
026200160701       // -?Operazioni iniziali?
026300160701       exsr sr_RoutInz;
026400160701
026500160701       // -?Ciclo di gestione del file video?
026600160701       DoW  $Fine = *off;
026700160701         select;
026800160701           // -?Gestione videata S1 (subfile)?
026900160701           when $Video = 'S1';
027000160701             exsr sr_GesS01;
027100160701           // -?? ? ??
027200160701           other;
027300160701             $Fine = *on;
027400160701         endsl;
027500160701       EndDo;
027600160701
027700160701       // -?Operazioni finali?
027800160701       exsr sr_RoutEnd;
027900160701
028000160701       //--------------------------------------------------------------
028100160701       //?Operazioni iniziali.                                         ?
028200160701       //--------------------------------------------------------------
028300160701       BEGSR  sr_RoutInz;
028400160701
028500160701         // -?Impostazione chiusura?
028600160701         *inLR = *on;
028700160701
028800160701         // -?Reperimento dati job?
028900160701         exsr  sr_DatiJob;
029000160701
029100160701         // -?Impostazione nome programma a video?
029200160701         V1Tpgm = SDSpgm;
029300160701
029400160701         // -?Impostazione iniziale / pulizia dei campi chiave?
029500160701         clear  k05tntbe01;
029600160701         k_TBEcod = c_Tab;
029700160701
029800160701         // -?Impostazione valori di default?
029900160701         Ord_x_RSC = *off;
030000160701         clear  KPJBU;
030100160701         clear  tntbAMVds1;
030200160701         oAMVerr   = *off;
030300160701
030400160701       ENDSR;
030500160701
030600160701       //--------------------------------------------------------------
030700160701       //?Reperimento Dati del job (Utente/Operativi).                 ?
030800160701       //--------------------------------------------------------------
030900160701       BEGSR  sr_DatiJob;
031000160701
031100160701         in(e) §AzUte;
031200160701         if NOT %error;
031300160701           in(e) §DatiUte;
031400160701         endif;
031500160701         if %error or RSut = *blank;
031600160701           tibs34r ( tibs34ds );
031700160701           in §AzUte;
031800160701           in §DatiUte;
031900160701         endif;
032000160701
032100160701       ENDSR;
032200160701
032300160701       //--------------------------------------------------------------
032400160701       //?Gestione subfile S01.                                        ?
032500160701       //--------------------------------------------------------------
032600160701       BEGSR  sr_GesS01;
032700160701
032800160701         // -?Inizializzazione videata?
032900160701         if  $InzS01 = *on;
033000160701           exsr  sr_InzS01;
033100160701           $InzS01 = *off;
033200160701         endif;
033300160701
033400160701         // -?Emissione Testata & Piede?
033500160701         write  TBAMVT01;
033600160701         write  TBAMVP01;
033700160701
033800160701         // -?Posizionamento cursore?
033900160701         if  C1CsrRrn > *zero;
034000160701           C1RcdNbr = C1CsrRrn;
034100160701         else;
034200160701           C1RcdNbr = 1;
034300160701           write  TBAMVS00;         //?(no rec.)?
034400160701         endif;
034500160701
034600160701         // -?Emissione videata?
034700160701         exfmt  TBAMVC01;
034800160701
034900160701         clear  ErrGenerico;
035000160701         clear  ErrMessage;
035100160701         clear  V1Dmsg;
035200160701
035300160701         select;
035400160701
035500160701           // -?F3=Fine?
035600160701           when  dsp_aid = c_F03;
035700160701             oAMVfxx = '03';
035800160701             clear  oAMVcli;
035900160701             $Fine  = *on;
036000160701
036100160701           // -?F4=Ricerca per Rag.Soc. cliente?
036200160701           when  dsp_aid = c_F04;
036300160701             exsr  sr_F04S01;
036400160701
036500160701           // -?F5=Refresh?
036600160701           //when  dsp_aid = c_F05;
036700160701           //  $InzS01 = *on;
036800160701
036900160701           // -?F11=Ordinamento?
037000160701           when  dsp_aid = c_F11;
037100160701             exsr  sr_F11S01;
037200160701
037300160701           // -?F12=Ritorno?
037400160701           when  dsp_aid = c_F12;
037500160701             oAMVfxx = '12';
037600160701             clear  oAMVcli;
037700160701             $Fine  = *on;
037800160701
037900160701           // -?Invio?
038000160701           other;
038100160701             // -?Gestione opzioni?
038200160701             if  S01nrr > *zero;
038300160701               exsr  sr_OpzS01;
038400160701               if  ErrGenerico;
038500160701                 leavesr;
038600160701               endif;
038700160701             endif;
038800160701
038900160701             // -?Richiesto posizionamento?
039000160701             if  (Not Ord_x_RSC   and   C1Cksc <> SavC1Cksc)   OR
039100160701                 (Ord_x_RSC       and   C1Crsc <> SavC1Crsc);
039200160701               $InzS01 = *on;
039300160701             endif;
039400160701
039500160701         endsl;
039600160701
039700160701       ENDSR;
039800160701
039900160701       //--------------------------------------------------------------
040000160701       //?Inizializzazione subfile S01.                                ?
040100160701       //--------------------------------------------------------------
040200160701       BEGSR  sr_InzS01;
040300160701
040400160701         // -?Spegnimento degli indicatori di errore?
040500160701         %subst(IndDspF : 50) = *off;
040600160701
040700160701         // -?Pulizia subfile control?
040800160701         if  Not Ord_x_RSC;
040900160701           clear  C1Crsc;
041000160701           reset  SavC1Crsc;
041100160701         else;
041200160701           clear  C1Cksc;
041300160701           reset  SavC1Cksc;
041400160701         endif;
041500160701
041600160701         // -?Pulizia subfile?
041700160701         SflDsp_N    = *on;
041800160701         SflDspCtl_N = *on;
041900160701         write  TBAMVC01;
042000160701         SflDspCtl_N = *off;
042100160701         SflEnd      = *off;
042200160701
042300160701         clear  C1RcdNbr;
042400160701         clear  C1CsrRrn;
042500160701         clear  S01nrr;
042600160701         clear  V1Dmsg;
042700160701         ErrMessage  = *off;
042800160701         ErrGenerico = *off;
042900160701
043000160701         // -?Caricamento completo dei dati nel subfile?
043100160701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
043200160701         //  ?l'eventuale ordinamento)?
043300160701         setll  %kds(k05tntbe01)  TNTBE000;
043400160701         reade  %kds(k05tntbe01 : 1)  TNTBE000;
043500160701         $EoF = (%eof(TNTBE01L));
043600160701         exsr  sr_CaricaS01;
043700160701
043800160701         // -?Impaginazione subfile?
043900160701         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
044000160701         if S01nrr > *zero;
044100160701           C1RcdNbr  = 1;
044200160701           C1CsrRrn  = 1;
044300160701         else;
044400160701           clear C1RcdNbr;
044500160701         endif;
044600160701
044700160701         // -?Ordinamento subfile (se premuto F11)?
044800160701         if  Ord_x_RSC   and   S01nrr > *zero;
044900160701           exsr  sr_SortSfl;
045000160701         endif;
045100160701
045200160701         // -?(Dis)Attivazione tasti funzionali?
045300160701         // ·?F4=Ricerca cliente x Rag.Soc.?
045400160701         //F4Attivo  = (S01nrr > *zero);
045500160701         F4Attivo  = *off;
045600160701         // ·?F11=Ordinamento x Rasg.Soc. cliente?
045700160701         F11Attivo = (S01nrr > *zero);
045800160701
045900160701       ENDSR;
046000160701
046100160701       //--------------------------------------------------------------
046200160701       //?Caricamento completo del subfile S01                         ?
046300160701       //--------------------------------------------------------------
046400160701       BEGSR  sr_CaricaS01;
046500160701
046600160701         clear  S01nrr;
046700160701         SflNxtChg = *off;
046800160701
046900160701         // -?Ciclo di caricamento dati da tab. "3EW"?
047000160701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
047100160701         //  ?l'eventuale ordinamento)?
047200160701         DoW  Not $EoF;
047300160701
047400160701           // -?Selezione singolo record file TNTBE00F?
047500160701           if  Not $EoF;
047600160701             exsr  sr_SelezRec;
047700160701           endif;
047800160701           if  $EoF;
047900160701             leave;
048000160701           endif;
048100160701
048200160701           // -?Registrazione singolo record nel subfile?
048300160701           if  $RecOK;
048400160701
048500160701             // ·?Impostazione campi?
048600160701             clear  TBAMVS01;
048700160701             S1Cksc = %int( %subst( TBEke1 : 1 : 7 ) );
048800160701             S1Crsc = %subst( ACOrag : 1 : %len(S1Crsc) );
048900160701             if  §AMVdti > *zero;
049000160701               S1Cdti = %int( %subst( %editc( §AMVdti : 'X' ) : 7 : 2 )
049100160701                            + %subst( %editc( §AMVdti : 'X' ) : 5 : 2 )
049200160701                            + %subst( %editc( §AMVdti : 'X' ) : 1 : 4 ) );
049300160701             endif;
049400160701             if  §AMVdtf > *zero;
049500160701               S1Cdtf = %int( %subst( %editc( §AMVdtf : 'X' ) : 7 : 2 )
049600160701                            + %subst( %editc( §AMVdtf : 'X' ) : 5 : 2 )
049700160701                            + %subst( %editc( §AMVdtf : 'X' ) : 1 : 4 ) );
049800160701             endif;
049900160701
050000160701             // -?Scrittura record nel subfile?
050100160701             S01nrr += 1;
050200160701             write  TBAMVS01;
050300160701
050400160701           EndIf;
050500160701
050600160701           // -?Lettura record successivo?
050700160701           reade  %kds(k05tntbe01 : 1)  TNTBE000;
050800160701           $EoF = (%eof(TNTBE01L));
050900160701
051000160701         EndDo;
051100160701
051200160701         // -?Memorizzazione posizionamento effettuato?
051300160701         if  Not Ord_x_RSC;
051400160701           SavC1Cksc = C1Cksc;
051500160701         else;
051600160701           SavC1Crsc = C1Crsc;
051700160701         endif;
051800160701
051900160701         // -?Visualizzazione del SFL (se ci sono dati)?
052000160701         SflDsp_N = (S01nrr <= *zero);
052100160701
052200160701         // -?Attivazione del SFLEND?
052300160701         SflEnd = ( $EoF );
052400160701
052500160701         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
052600160701         if  S01nrr > *zero;
052700160701           C1RcdNbr = 1;
052800160701         else;
052900160701           clear  C1RcdNbr;
053000160701         endif;
053100160701
053200160701         C1CsrRrn = C1RcdNbr;
053300160701
053400160701       ENDSR;
053500160701
053600160701       //--------------------------------------------------------------
053700160701       //?Selezione del singolo record letto dalla tabella "AMV".      ?
053800160701       //--------------------------------------------------------------
053900160701       BEGSR  sr_SelezRec;
054000160701
054100160701         reset  $RecOK;
054200160701
054300160701         dAMV = TBEuni;
054400160701
054500160701         select;
054600160701
054700160701           // -?Omissione records annullati?
054800160701           when  TBEatb <> *blank;
054900160701             leavesr;
055000160701
055100160701           // -?Richiesto posizionamento per codice cliente:?
055200160701           //  ?si scartano i clienti con codice inferiore?
055300160701           when  Not Ord_x_RSC  and  TBEke1 < %editc( C1Cksc : 'X' );
055400160701             leavesr;
055500160701
055600160701         endsl;
055700160701
055800160701         // -?Decodifica cliente?
055900160701         clear  TIBS69ds;
056000160701         I69sif = knsif;
056100160701         I69kcc = DUTkci;
056200160701         I69kac = %int( %subst( TBEke1 : 1 : 7 ) );
056300160701         tibs69r( tibs69ds :
056400160701                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
056500160701         if  O69err = *on;
056600160701           ACOrag = *all'? ';
056700160701         endif;
056800160701
056900160701         select;
057000160701
057100160701           // -?Richiesto posizionamento per ragione sociale cliente:?
057200160701           //  ?si scartano i clienti con ragione sociale inferiore?
057300160701           when  Ord_x_RSC  and  ACOrag < C1Crsc;
057400160701             leavesr;
057500160701
057600160701           // -?Selezione per descrizione (ragione sociale cliente)?
057700160701           when  W1Crsc <> *blank   and
057800160701                 %scan( %trimr( SavW1Crsc ) : ACOrag ) = *zero;
057900160701             leavesr;
058000160701
058100160701         endsl;
058200160701
058300160701         $RecOK = *on;
058400160701
058500160701       ENDSR;
058600160701
058700160701       //--------------------------------------------------------------
058800160701       //?Gestione tasto funzionale F04 da videata C01 / S01           ?
058900160701       //?F4=Ricerca per ragione sociale cliente                       ?
059000160701       //--------------------------------------------------------------
059100160701       BEGSR  sr_F04S01;
059200160701
059300160701         // -?Emissione window W01?
059400160701         exfmt  TBAMVW01;
059500160701
059600160701         reset  ErrGenerico;
059700160701         reset  ErrMessage;
059800160701         clear  V1Dmsg;
059900160701
060000160701         Select;
060100160701
060200160701           // -?F12=Ritorno?
060300160701           When  dsp_aid = c_F12;
060400160701             $Video = 'S1';
060500160701             leavesr;
060600160701
060700160701           // -?Invio?
060800160701           Other;
060900160701             if  W1Crsc <> SavW1Crsc;
061000160701               $InzS01   = *on;
061100160701               SavW1Crsc = W1Crsc;
061200160701             endif;
061300160701             if  W1Crsc = *blank;
061400160701               clear  C1Ddes;
061500160701             else;
061600160701               C1Ddes = 'Ricerca: ' + %trim(W1Crsc);
061700160701             endif;
061800160701             leavesr;
061900160701
062000160701         EndSl;
062100160701
062200160701       ENDSR;
062300160701
062400160701       //--------------------------------------------------------------
062500160701       //?Gestione tasto funzionale F11 da videata C01 / S01           ?
062600160701       //?F11=Nuovo ordinamento sfl                                    ?
062700160701       //--------------------------------------------------------------
062800160701       BEGSR  sr_F11S01;
062900160701
063000160701         Ord_x_RSC = (Not Ord_x_RSC);
063100160701
063200160701         select;
063300160701           // -?Subfile vuoto: nessun record da ordinare?
063400160701           when  S01nrr = *zero;
063500160701           // -?Subfile già posizionato: occorre ricaricarlo?
063600160701           when  C1Cksc <> *zero  or  C1Crsc <> *blank;
063700160701             $InzS01 = *on;
063800160701           // -?Altrimenti: basta riordinarlo?
063900160701           other;
064000160701             exsr  sr_SortSfl;
064100160701         endsl;
064200160701
064300160701         if  Not Ord_x_RSC;
064400160701           clear  C1Cksc;
064500160701           clear  SavC1Cksc;
064600160701         else;
064700160701           clear  C1Crsc;
064800160701           clear  SavC1Crsc;
064900160701         endif;
065000160701
065100160701       ENDSR;
065200160701
065300160701       //--------------------------------------------------------------
065400160701       //?Ordinamento subfile                                          ?
065500160701       //?  This subroutine sorts the subfile records.                 ?
065600160701       //--------------------------------------------------------------
065700160701       BEGSR  sr_SortSfl;
065800160701
065900160701         RrnLast  = S01nrr;
066000160701
066100160701         C1RcdNbr = 1;
066200160701         clear  C1CsrRrn;
066300160701         clear  S01nrr;
066400160701         clear  V1Dmsg;
066500160701         ErrMessage  = *off;
066600160701         SflNxtChg   = *off;
066700160701         ErrGenerico = *off;
066800160701         %subst(IndDspF : 50) = *off;
066900160701
067000160701         //?___________________________________________________________?
067100160701         //?Initialize the key fields to sort on.?
067200160701         //?There is one extra field not in the subfile -- Selected --?
067300160701         //?that is added to the record. This field is used to place?
067400160701         //?selected records in front of nonselected records.?
067500160701         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
067600160701
067700160701         clear  QLGKL;
067800160701         clear  QLGSKL;
067900160701         clear  QLGSCB;
068000160701         clear  QLGSCB00;
068100160701
068200160701         // -?Gestione della scelta ordinamento:?
068300160701         // -?Ordinamento per Rag.Soc./Codice cliente?
068400160701         If  Ord_x_RSC = *on;
068500160701           // ·?2 campi chiave?
068600160701           QLGNBRK  = 2;
068700160701           // ·?1° campo: padre (S1Crsc)?
068800160701           //            ?a posizone    7, 35 byte, char, ascending.?
068900160701           QLGSP    = %size(S1Copz) + %size(S1Cksc) + 1;
069000160701           QLGSS    = %size(S1Crsc);
069100160701           QLGDT    = c_Carattere;
069200160701           QLGSO    = c_Ascendente;
069300160701           QLGKL(1) = QLGSKL;
069400160701           // ·?2° campo: codice (S1Cksc)?
069500160701           //            ?a posizone    2,  7 byte,  int, ascending.?
069600160701           QLGSP    = %size(S1Copz) + 1;
069700160701           QLGSS    = %size(S1Cksc);
069800160701           QLGDT    = c_NumIntero;
069900160701           QLGSO    = c_Ascendente;
070000160701           QLGKL(2) = QLGSKL;
070100160701
070200160701         // -?Ordinamento per Codice cliente?
070300160701         Else;
070400160701           // ·?1 campi chiave?
070500160701           QLGNBRK  = 1;
070600160701           // ·?1° campo: cliente (S1Cksc)?
070700160701           //            ?a posizone    2,  7 byte,  int, ascending.?
070800160701           QLGSP    = %size(S1Copz) + 1;
070900160701           QLGSS    = %size(S1Cksc);
071000160701           QLGDT    = c_NumIntero;
071100160701           QLGSO    = c_Ascendente;
071200160701           QLGKL(1) = QLGSKL;
071300160701
071400160701         EndIf;
071500160701
071600160701         // -?Load other sort parameters.?
071700160701         QLGLB   = 80 + 16 * c_MaxKey;
071800160701         QLGRL   = %size(SflRcd) - 1;
071900160701         QLGRT   = 8;
072000160701         QLGOKL  = 80;
072100160701         QLGLKE  = 16;
072200160701         QLGLSS  = 290;
072300160701         // -?Initialize Sort I/O API fields.?
072400160701         QLGRL00 = QLGRL;
072500160701         QLGRC00 = 1;
072600160701         clear  QUSEI;
072700160701         QUSBPRV = %size(QUSEC);
072800160701
072900160701         // -?First step - Initialize the sort routine.?
073000160701         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
073100160701                      SizeList : ReturnSize : QUSEC );
073200160701
073300160701         // -?Next step - Write records to I/O routine.?
073400160701         QLGRT00  = c_Put;
073500160701         For  Nrr = 1  To  RrnLast;
073600160701           chain  Nrr  TBAMVS01;
073700160701           // -?Solo le righe con Selected = 'Y' sono riordinate,?
073800160701           //  ?quindi per fare un ordinamento di tutte le righe?
073900160701           //  ?metto 'Y' sempre.?
074000160701           Selected = 'Y';
074100160701           clear  QUSEI;
074200160701           QUSBPRV  = %size(QUSEC);
074300160701           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
074400160701                         SizeList : NotUsed : QUSEC );
074500160701         EndFor;
074600160701
074700160701         // -?Next step - Signal end of input, clear subfile for reload.?
074800160701         QLGRT00 = c_EndPut;
074900160701         clear  QUSEI;
075000160701         QUSBPRV = %size(QUSEC);
075100160701         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
075200160701                       SizeList : NotUsed : QUSEC );
075300160701
075400160701         // -?Pulizia subfile?
075500160701         SflDsp_N    = *on;
075600160701         SflDspCtl_N = *on;
075700160701         write  TBAMVC01;
075800160701         SflDspCtl_N = *off;
075900160701         SflEnd      = *off;
076000160701
076100160701         // -?Final step - Write the records back to the subfile.?
076200160701         QLGRT00  = c_Get;
076300160701         For  Nrr = 1  To RrnLast;
076400160701           clear  QUSEI;
076500160701           QUSBPRV = %size(QUSEC);
076600160701           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
076700160701                         QLGRL00  : NotUsed : QUSEC );
076800160701           // -?Ripristino indicatori utilizzati nel sfl rec?
076900160701           SflNxtChg = (S1Copz <> *blank);
077000160701           S01nrr = Nrr;
077100160701           write  TBAMVS01;
077200160701         EndFor;
077300160701
077400160701         C1CsrRrn = 1;
077500160701
077600160701         // -?Visualizzazione del SFL (se ci sono dati)?
077700160701         SflDsp_N = (S01nrr <= *zero);
077800160701
077900160701         // -?Attivazione del SFLEND?
078000160701         SflEnd = $EoF;
078100160701
078200160701       ENDSR;
078300160701
078400160701       //--------------------------------------------------------------
078500160701       //?Gestione opzioni subfile S01.                                ?
078600160701       //--------------------------------------------------------------
078700160701       BEGSR  sr_OpzS01;
078800160701
078900160701         clear  SavS01csr;
079000160701
079100160701         // -?Ciclo di lettura subfile?
079200160701         readc  TBAMVS01;
079300160701
079400160701         DoW  Not %eof(TNTBAMVD1);
079500160701
079600160701           SflNxtChg = *off;
079700160701           %subst(IndDspF : 50) = *off;
079800160701           C1RcdNbr = S01nrr;
079900160701
080000160701           Select;
080100160701
080200160701             // -?Nessuna opzione?
080300160701             When  S1Copz = *blank;
080400160701
080500160701             // -?Opz. 1=Selezione?
080600160701             When  S1Copz = '1'  and  Not $Fine;
080700160701               oAMVcli = S1Cksc;
080800160701               $Fine = *on;
080900160701
081000160701             // -?Ammessa una sola opzione?
081100160701             When  S1Copz = '1'  and  $Fine;
081200160701               ErrGenerico = *on;
081300160701               ErrMessage  = *on;
081400160701               PosCurOPZ   = *on;
081500160701               V1Dmsg = sk_Msg(02);
081600160701               $Fine  = *off;
081700160701               clear  oAMVcli;
081800160701
081900160701             // -?Opzione errata?
082000160701             Other;
082100160701               ErrGenerico = *on;
082200160701               ErrMessage  = *on;
082300160701               PosCurOPZ   = *on;
082400160701               V1Dmsg = sk_Msg(01);
082500160701               $Fine  = *off;
082600160701
082700160701           EndSl;
082800160701
082900160701           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
083000160701           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
083100160701           if  S1Copz  <> *blank   and
083200160701              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
083300160701             SavS01csr   = C1RcdNbr;
083400160701           endif;
083500160701
083600160701           // -?Aggiornamento Subfile?
083700160701           select;
083800160701             when  ErrMessage;
083900160701               SflNxtChg = *on;
084000160701               C1CsrRrn  = C1RcdNbr;
084100160701             when  ErrGenerico;
084200160701               C1CsrRrn  = C1RcdNbr;
084300160701               if  S1Copz <> '1';
084400160701                 clear  S1Copz;
084500160701               endif;
084600160701             other;
084700160701               if  S1Copz <> '1';
084800160701                 clear  S1Copz;
084900160701               endif;
085000160701           endsl;
085100160701
085200160701           if  S1Copz <> *blank;
085300160701             SflNxtChg = *on;
085400160701           endif;
085500160701
085600160701           UPDATE  TBAMVS01;
085700160701
085800160701           if  ErrGenerico   or   ErrMessage;
085900160701             leave;
086000160701           endif;
086100160701
086200160701           readc  TBAMVS01;
086300160701
086400160701         EndDo;
086500160701
086600160701         // -?Riposizionam. cursore sul record contenente la prima opz.?
086700160701         //  ?(se non sono stati rilevati errori)?
086800160701         if  NOT ErrMessage   and   NOT ErrGenerico   and
086900160701             SavS01csr > *zero;
087000160701           C1CsrRrn = SavS01csr;
087100160701         endif;
087200160701
087300160701       ENDSR;
087400160701
087500160701       //--------------------------------------------------------------
087600160701       //?Operazioni finali.                                           ?
087700160701       //--------------------------------------------------------------
087800160701       BEGSR  sr_RoutEnd;
087900160701
088000160701         // -?Chiusura applicazioni precedentemente aperte?
088100160701         reset  TIBS69ds;
088200160701         tibs69r ( tibs69ds :
088300160701                   ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
088400160701
088500160701         // -?Restituzione parametri in uscita?
088600160701         KPJBU = tntbAMVds1;
088700160701
088800160701         // -?Uscita?
088900160701         return;
089000160701
089100160701       ENDSR;
089200160701
089300160701      /end-free
089400160701
089500160701       //--------------------------------------------------------------
089600160701       //?Schiere a tempo di compilazione.                             ?
089700160701       //--------------------------------------------------------------
089800160701
089900160701** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
090000160701Opzione errata                                                                 1
090100160701Ammessa la selezione di un solo cliente                                        2
