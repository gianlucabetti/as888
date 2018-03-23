000100130710       //==============================================================
000200130710       //?TRMK43R * Interrogazione Anagrafica Commerciali              ?
000300130710       //==============================================================
000400130710
000500130710       //--------------------------------------------------------------
000600130710       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130710       //--------------------------------------------------------------
000800130710
000900130710     /*PRM dbgview(*source)
001000130710     /*END
001100130710
001200130710       //--------------------------------------------------------------
001300130710       //?Specifiche di controllo.                                     ?
001400130710       //--------------------------------------------------------------
001500130710
001600130710     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130710     h dftactgrp(*no) actgrp('TRMK43R')
001800130710
001900130710       //--------------------------------------------------------------
002000130710       //?Dichiarazione file.                                          ?
002100130710       //--------------------------------------------------------------
002200130710
002300130710       // -?Organigramma filiale/aziendale?
002400130710     fAZORG01L  if   e           k disk
002500130710
002600130710       // -?Anagrafica Commerciali?
002700130710     fAZCMM01L  if   e           k disk
002800130710       // -?Note/Rubrica Commerciali?
002900130711     fAZNTC01L  if   e           k disk    usropn
003000130710
003100130710       // -?Video Gestione?
003200130710     fTRMK43D   cf   e             workstn
003300130710     f                                     sfile(MK43S01 : S01nrr)
003400130710     f                                     indds(IndDspF)
003500130710     f                                     infds(InfDspF)
003600130710
003700130710       //--------------------------------------------------------------
003800130710       //?Definizione costanti.                                        ?
003900130710       //--------------------------------------------------------------
004000130710
004100130710       // -?Numero di record in ciascuna videata di subfile?
004200130710     d c_SflPag        c                   const(15)
004300130710
004400130710       // -?Numero massimo di record gestibili?
004500130710     d c_MaxRec        c                   const(9999)
004600130710
004700130710       // -?Tasti funzionali a video?
004800130710     d c_F01           c                   const(x'31')
004900130710     d c_F02           c                   const(x'32')
005000130710     d c_F03           c                   const(x'33')
005100130710     d c_F04           c                   const(x'34')
005200130710     d c_F05           c                   const(x'35')
005300130710     d c_F06           c                   const(x'36')
005400130710     d c_F07           c                   const(x'37')
005500130710     d c_F08           c                   const(x'38')
005600130710     d c_F09           c                   const(x'39')
005700130710     d c_F10           c                   const(x'3A')
005800130710     d c_F11           c                   const(x'3B')
005900130710     d c_F12           c                   const(x'3C')
006000130710     d c_F13           c                   const(x'B1')
006100130710     d c_F14           c                   const(x'B2')
006200130710     d c_F15           c                   const(x'B3')
006300130710     d c_F16           c                   const(x'B4')
006400130710     d c_F17           c                   const(x'B5')
006500130710     d c_F18           c                   const(x'B6')
006600130710     d c_F19           c                   const(x'B7')
006700130710     d c_F20           c                   const(x'B8')
006800130710     d c_F21           c                   const(x'B9')
006900130710     d c_F22           c                   const(x'BA')
007000130710     d c_F23           c                   const(x'BB')
007100130710     d c_F24           c                   const(x'BC')
007200130710     d c_Enter         c                   const(x'F1')
007300130710     d c_RollDown      c                   const(x'F4')
007400130710     d c_RollUp        c                   const(x'F5')
007500130710
007600130710       //--------------------------------------------------------------
007700130710       //?Definizione schiere.                                         ?
007800130710       //--------------------------------------------------------------
007900130710
008000130710       // -?Messaggi di errore?
008100130711     d $Msg            s             78    dim( 7) ctdata perrcd(1)
008200130710
008300130710       //--------------------------------------------------------------
008400130710       //?Definizione aree dati.                                       ?
008500130710       //--------------------------------------------------------------
008600130710
008700130710       // -?Dati utente?
008800130710     d §AzUte        e ds                  extname(AZUTE00F)
008900130710     d                                     dtaara
009000130710     d §DatiUte      e ds                  extname(dDatiUte)
009100130710     d                                     dtaara
009200130710
009300130710       //--------------------------------------------------------------
009400130710       //?Definizione strutture dati.                                  ?
009500130710       //--------------------------------------------------------------
009600130710
009700130710       // -?Status ds?
009800130710     d Status         sds
009900130710     d   SDSpgm          *proc
010000130710     d*//SDSprm          *parms
010100130710     d*//SDSusr              254    263
010200130710
010300130710       // -?InfDS?
010400130710     d InfDspF         ds
010500130710     d   dsp_aid             369    369a
010600130710     d*//sfl_rrn             376    377i 0
010700130710     d*//min_nrr             378    379i 0
010800130710     d*//num_rcds            380    381i 0
010900130710
011000130710       // -?Indicatori su DspF?
011100130711     d IndDspF         ds                  inz
011200130710         // -?Abilitazione tasti funzionali?
011300130711     d*//F3Attivo                      n   overlay(IndDspF : 03)
011400130711     d   F5Attivo                      n   overlay(IndDspF : 05)
011500130711     d   F8Attivo                      n   overlay(IndDspF : 08)
011600130711     d   F12Attivo                     n   overlay(IndDspF : 12)
011700130710         // -?Emissione messaggio di errore?
011800130710     d   ErrMessage                    n   overlay(IndDspF : 28)
011900130710         // -?Indicatori di gestione del subfile?
012000130710     d   SflDsp_N                      n   overlay(IndDspF : 30)
012100130710     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012200130710     d   SflNxtChg                     n   overlay(IndDspF : 32)
012300130711     d   SflEnd                        n   overlay(IndDspF : 33)
012400130710         // -?Indicatori per Attributi di visualizzazione?
012500130711     d   Ord_1                         n   overlay(IndDspF : 41)
012600130711     d   Ord_2                         n   overlay(IndDspF : 42)
012700130711     d   AbilitOpz6                    n   overlay(IndDspF : 41)
012800130710         // -?Posizionamento cursore & segnalazione errore?
012900130710     d   PosCurOPZ                     n   overlay(IndDspF : 50)
013000130710     d   PosCurFIL                     n   overlay(IndDspF : 51)
013100130711     d   PosCurCOD                     n   overlay(IndDspF : 52)
013200130711     d   PosCurDES                     n   overlay(IndDspF : 53)
013300130711     d   PosCurUNI                     n   overlay(IndDspF : 54)
013400130710         // -?Riemissione videata?
013500130710     d   ErrGenerico                   n   overlay(IndDspF : 99)
013600130710
013700130710       // -?Parametri ricevuti?
013800130710     d KPJBA         e ds
013900130711     d TRMK43ds      e ds
014000130711     d prm_AZCMM_ds...
014100130711     d               e ds                  extname(AZCMM00F)
014200130711     d                                     qualified
014300130711     d prm_AZNTC01_ds...
014400130711     d               e ds                  extname(AZNTC00F)
014500130711     d                                     qualified
014600130711     d prm_AZNTC02_ds...
014700130711     d               e ds                  extname(AZNTC00F)
014800130711     d                                     qualified
014900130710
015000130710       //--------------------------------------------------------------
015100130710       //?Definizione variabili globali.                               ?
015200130710       //--------------------------------------------------------------
015300130710
015400130710       // -?Flags booleani?
015500130710     d $Fine           s               n   inz
015600130710     d $EoF            s               n   inz
015700130710     d $InzS01         s               n   inz(*on)
015800130711     d $Ord_x_CodCmm   s               n   inz
015900130711     d $Ord_x_DesCmm   s               n   inz
016000130711     d $Ord_x_CodUni   s               n   inz
016100130710
016200130710       // -?Variabili per la gestione del video?
016300130710     d $Video          s              2    inz('S1')
016400130710     d S01nrr          s                   like(C1RcdNbr) inz
016500130710     d SavS01csr       s                   like(C1RcdNbr) inz
016600130710
016700130711       // -?Variabili per la gestione del posizionamento nel subfile?
016800130711     d SavC1Cfil       s                   like(C1Cfil)   inz(*loval)
016900130711     d SavC1Ccod       s                   like(C1Ccod)   inz
017000130711     d SavC1Cdes       s                   like(C1Cdes)   inz
017100130711     d SavC1Cuni       s                   like(C1Cuni)   inz
017200130711
017300130711       // -?Conteggio selezioni (opz. 1) inserite?
017400130711     d wMemoSel        s                   like(S1Copz)   inz
017500130710
017600130710       // -?Campi di comodo?
017700130710     d wDate           s              8s 0 inz
017800130710
017900130710       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018000130710
018100130710       //--------------------------------------------------------------
018200130710       // -?Constants?
018300130710       //--------------------------------------------------------------
018400130710       // -?MaxKey - Maximum number of key fields allowed by this program?
018500130710     d MaxKey          c                   const(2)
018600130710       // -?Sort order:?
018700130710       //  ?1 = Sort in an ascending sequence?
018800130710       //  ?2 = Sort in a descending sequence?
018900130710     d Ascendente      c                   const(1)
019000130710     d Discendente     c                   const(2)
019100130710       // -?Key data type:?
019200130710       //  ? 0 = Signed binary?
019300130710       //  ? 2 = Signed zoned decimal?
019400130710       //  ? 3 = Signed packed decimal?
019500130710       //  ? 6 = Character with no national language sort sequence applied,?
019600130710       //  ?     if specified?
019700130710       //  ? 7 = Unsigned packed decimal?
019800130710       //  ?     All numbers will have the sign forced positive ('F'X).?
019900130710       //  ? 8 = Unsigned zoned decimal?
020000130710       //  ?     All numbers will have the sign forced positive ('F'X).?
020100130710       //  ? 9 = Unsigned binary?
020200130710       //  ?14 = Date in form of DD/MM/YY?
020300130710       //  ?15 = Date in form of DD.MM.YYYY?
020400130710     d Numero          c                   const(2)
020500130710     d Carattere       c                   const(6)
020600130711     d NumIntero       c                   const(8)
020700130710       //
020800130710     d Put             c                   const(1)
020900130710     d EndPut          c                   const(2)
021000130710     d Get             c                   const(3)
021100130710
021200130710       //--------------------------------------------------------------
021300130710       // -?Data Structures?
021400130710       //  ?SflRcd     - The entire subfile record to pass to the sort?
021500130710       //  ?QLGSCB     - The sort request block for the QLGSORT API?
021600130710       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
021700130710       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
021800130710       //  ?QUSEC      - Error structure for the QLGSORT API?
021900130710       //--------------------------------------------------------------
022000130710     d SflRcd          ds                  inz
022100130711     d   S1Copz                            inz
022200130711     d   S1Ccmm                            inz
022300130711     d   S1Dcmm                            inz
022400130711     d   S1Cuni                            inz
022500130711     d*//S1Duni                            inz
022600130711     d   Selected                     1a   inz
022700130710      /copy QSYSINC/QRPGLESRC,QLGSORT
022800130710     d QLGKL                         16    dim(MaxKey)
022900130711     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023000130711     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023100130711     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023200130711     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023300130710      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023400130710      /copy QSYSINC/QRPGLESRC,QUSEC
023500130710
023600130710       //--------------------------------------------------------------
023700130710       // -?Standalone fields?
023800130710       //  ?Nrr        - Relative record number for subfile?
023900130710       //  ?SizeList   - Size of list?
024000130710       //  ?ReturnSize - Size of list returned by sort API?
024100130710       //--------------------------------------------------------------
024200130710     d NotUsed         s             16a   inz
024300130710     d ReturnSize      s              9b 0 inz
024400130710     d SizeList        s              9b 0 inz
024500130710     d RrnLast         s              5i 0 inz
024600130710     d Nrr             s              5i 0 inz
024700130710
024800130710       //--------------------------------------------------------------
024900130710       //?Definizione prototipi procedure usate.                       ?
025000130710       //--------------------------------------------------------------
025100130710
025200130710       // -?Reperimento dati utente?
025300130710     d TIBS34ds      e ds                  inz
025400130710      /copy gaitrasrc/srcProtoPR,TIBS34R
025500130710
025600130710       // -?Ordinamento subfile?
025700130710      /copy gaitrasrc/srcProtoPr,QLGSRTIO
025800130710
025900130710       //--------------------------------------------------------------
026000130710       //?Definizione key-list.                                        ?
026100130710       //--------------------------------------------------------------
026200130710
026300130710       // -?File AZCMM01L?
026400130710     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
026500130710     d                                     prefix(k_)   inz
026600130710
026700130710       // -?File AZNTC01L?
026800130711     d k02azntc01    e ds                  extname(AZNTC01L : *key)
026900130711     d                                     prefix(k_)   inz
027000130710
027100130710       //--------------------------------------------------------------
027200130710       //?Riepilogo indicatori utilizzati.                             ?
027300130710       //--------------------------------------------------------------
027400130710       //--------------------------------------------------------------
027500130710
027600130710       //--------------------------------------------------------------
027700130710       //?M A I N - L I N E                                            ?
027800130710       //--------------------------------------------------------------
027900130710
028000130710     c     *Entry        plist
028100130710     c                   parm                    KPJBA
028200130712     c                   parm                    TRMK43ds
028300130711     c                   parm                    prm_AZCMM_ds
028400130711     c                   parm                    prm_AZNTC01_ds
028500130711     c                   parm                    prm_AZNTC02_ds
028600130710
028700130710      /free
028800130710
028900130710       // -?Operazioni iniziali?
029000130710       exsr sr_RoutInz;
029100130710
029200130710       // -?Ciclo di gestione del file video?
029300130710       DoW  $Fine = *off;
029400130710
029500130710         select;
029600130710
029700130710           // -?Gestione videata S1 (subfile)?
029800130710           when $Video = 'S1';
029900130710             exsr sr_GesS01;
030000130710
030100130710           // -?? ? ??
030200130710           other;
030300130710             $Fine = *on;
030400130710
030500130710         endsl;
030600130710
030700130710       EndDo;
030800130710
030900130710       // -?Operazioni finali?
031000130710       exsr sr_RoutEnd;
031100130710
031200130710       //--------------------------------------------------------------
031300130710       //?Operazioni iniziali.                                         ?
031400130710       //--------------------------------------------------------------
031500130710       BEGSR  sr_RoutInz;
031600130710
031700130710         // -?Impostazione chiusura?
031800130710         *inLR = *on;
031900130710
032000130710         // -?Reperimento dati job?
032100130710         exsr  sr_DatiJob;
032200130710
032300130710         // -?Reperimento data odierna?
032400130710         wDate = %dec( %date() );
032500130710
032600130710         // -?Impostazione nome programma a video?
032700130710         V1Tpgm = SDSpgm;
032800130711
032900130711         // -?Verifica parametri ricevuti?
033000130711         if  %parms() > 1;
033100130711           //if  iMK43solU = 'S';
033200130711           //  C1CsolU = iMK43solU;
033300130711           //endif;
033400130711           if  iMK43fil <> *zero;
033500130711             C1Cfil = %abs(iMK43Fil);
033600130711           endif;
033700130711         endif;
033800130711         AbilitOpz6 = (%parms() = 1  or  iMK43solU <> 'S');
033900130710
034000130710         // -?Pulizia parametri di output?
034100130711         if  %parms() >= 2;
034200130711           clear  oMK43cmm;
034300130711           clear  oMK43des;
034400130711           clear  oMK43fxx;
034500130730           oMK43err = *off;
034600130711           clear  oMK43msg;
034700130712           if  %parms() >= 3;
034800130712             clear  prm_AZCMM_ds;
034900130712             if  %parms() >= 4;
035000130712               clear  prm_AZNTC01_ds;
035100130712               if  %parms() >= 5;
035200130712                 clear  prm_AZNTC02_ds;
035300130712               endif;
035400130712             endif;
035500130712           endif;
035600130710         endif;
035700130711
035800130711         // -?Impostazione videata iniziale?
035900130711         $Video = 'S1';
036000130711         reset  $InzS01;
036100130711         $Ord_x_CodCmm = *off;
036200130711         $Ord_x_DesCmm = *on;
036300130711         $Ord_x_CodUni = *off;
036400130710
036500130710       ENDSR;
036600130710
036700130710       //--------------------------------------------------------------
036800130710       //?Reperimento Dati del job (Utente/Operativi).                 ?
036900130710       //--------------------------------------------------------------
037000130710       BEGSR  sr_DatiJob;
037100130710
037200130710         in(e) §AzUte;
037300130710         if NOT %error;
037400130710           in(e) §DatiUte;
037500130710         endif;
037600130710         if %error or RSut = *blank;
037700130710           tibs34r ( tibs34ds );
037800130710           in §AzUte;
037900130710           in §DatiUte;
038000130710         endif;
038100130710
038200130710       ENDSR;
038300130710
038400130710       //--------------------------------------------------------------
038500130710       //?Gestione subfile S01                                         ?
038600130710       //--------------------------------------------------------------
038700130710       BEGSR  sr_GesS01;
038800130710
038900130711         // -?Inizializzazione videata?
039000130710         if  $InzS01 = *on;
039100130710           exsr  sr_InzS01;
039200130710           $InzS01 = *off;
039300130710         endif;
039400130710
039500130711         // -?Emissione Piede con tasti funzionali abilitati (window)?
039600130711         write  MK43P01;
039700130710
039800130711         // -?Posizionamento cursore?
039900130711         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
040000130711         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
040100130711         //  ?si visualizza la pagina che vedeva l'utente quando ha?
040200130711         //  ?premuto l'ultimo tasto.?
040300130710         if  C1CsrRrn > *zero;
040400130710           C1RcdNbr = C1CsrRrn;
040500130710         else;
040600130710           C1RcdNbr = 1;
040700130711           write  MK43S00;          //?(no rec.)?
040800130710         endif;
040900130710
041000130711         // -?Emissione videata (testata compresa) in window?
041100130711         exfmt  MK43C01;
041200130710
041300130710         clear  V1Dmsg;
041400130711         reset  ErrMessage;
041500130711         reset  ErrGenerico;
041600130710
041700130710         SELECT;
041800130710
041900130711           // -?F3=Fine?
042000130710           WHEN  dsp_aid = c_F03;
042100130723             if  %parms() > 1;
042200130723               oMK43fxx = '03';
042300130723             endif;
042400130710             $Fine = *on;
042500130711
042600150112           // -?F5=Refresh?
042700130711           WHEN  dsp_aid = c_F05;
042800130711             $InzS01 = *on;
042900130710
043000130711           // -?F8=Cambio ordinamento?
043100130710           WHEN  dsp_aid = c_F08;
043200130710             exsr  sr_F08S01;
043300130711
043400130711           // -?F12=Ritorno?
043500130711           WHEN  dsp_aid = c_F12;
043600130723             if  %parms() > 1;
043700130723               oMK43fxx = '12';
043800130723             endif;
043900130711             $Fine = *on;
044000130711
044100130711           // -?Subfile vuoto?
044200130809           WHEN  S01nrr = *zero        and
044300130809                (C1Cfil  <> SavC1Cfil   or
044400130809                 C1Ccod  <> SavC1Ccod   or
044500130809                 C1Cdes  <> SavC1Cdes   or
044600130809                 C1Cuni  <> SavC1Cuni);
044700130809             $InzS01 = *on;
044800130809           WHEN  S01nrr = *zero;
044900130710
045000130711           // -?Invio?
045100130710           OTHER;
045200130711             // -?Gestione opzioni?
045300130711             exsr  sr_OpzS01;
045400130711             select;
045500130711               when  ErrGenerico;
045600130711                 leavesr;
045700130711               when  wMemoSel > *zero;
045800130711                 $Fine = *on;
045900130711                 leavesr;
046000130711             endsl;
046100130711             // -?Selezioni/Posizionamento?
046200130711             Select;
046300130711               // -?Selezionata una filiale diversa?
046400130711               When  C1Cfil <> SavC1Cfil;
046500130711                 exsr  sr_CtrFIL;
046600130711                 if  Not ErrMessage;
046700130711                   $InzS01 = *on;
046800130711                   SavC1Cfil = C1Cfil;
046900130711                 endif;
047000130711               // -?Richiesto posizionamento?
047100130711               When  ($Ord_x_CodCmm  and  C1Ccod <> SavC1Ccod)  or
047200130711                     ($Ord_x_DesCmm  and  C1Cdes <> SavC1Cdes)  or
047300130711                     ($Ord_x_CodUni  and  C1Cuni <> SavC1Cuni);
047400130711                 $InzS01 = *on;
047500130711                 SavC1Ccod = C1Ccod;
047600130711                 SavC1Cdes = C1Cdes;
047700130711                 SavC1Cuni = C1Cuni;
047800130711             EndSl;
047900130710
048000130710         ENDSL;
048100130710
048200130710       ENDSR;
048300130710
048400130710       //--------------------------------------------------------------
048500130710       //?Inizializzazione subfile S01                                 ?
048600130710       //--------------------------------------------------------------
048700130710       BEGSR  sr_InzS01;
048800130710
048900130711         // -?Spegnimento degli indicatori di errore?
049000130710         %subst(IndDspF : 50) = *off;
049100130710
049200130711         // -?Pulizia subfile?
049300130710         SflDsp_N    = *on;
049400130710         SflDspCtl_N = *on;
049500130711         write  MK43C01;
049600130710         SflDspCtl_N = *off;
049700130710         SflEnd      = *off;
049800130710
049900130710         clear  C1RcdNbr;
050000130710         clear  C1CsrRrn;
050100130710         clear  S01nrr;
050200130710         clear  V1Dmsg;
050300130710         ErrMessage  = *off;
050400130710         ErrGenerico = *off;
050500130711
050600130711         Select;
050700130711           When  $Ord_x_CodCmm;
050800130711             Ord_1 = *off;
050900130711             Ord_2 = *off;
051000130711           When  $Ord_x_DesCmm;
051100130711             Ord_1 = *on;
051200130711             Ord_2 = *off;
051300130711           When  $Ord_x_CodUni;
051400130711             Ord_1 = *off;
051500130711             Ord_2 = *on;
051600130711         EndSl;
051700130710
051800130711         // -?Caricamento completo dei dati nel subfile (per codice)?
051900130711         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
052000130711         //  ?l'eventuale ordinamento)?
052100130711         clear  k01azcmm01;
052200130711         if  C1Cfil <> *zero;
052300130711           k_CMMcod = C1Cfil * 10000;
052400130710         else;
052500130711           k_CMMcod = *loval;
052600130710         endif;
052700130711         setll  %kds(k01azcmm01)  AZCMM000;
052800130711         exsr  sr_ReadAZCMM;
052900130710
053000130711         DoW  not $EoF;
053100130711           exsr  sr_CaricaS01;
053200130711         EndDo;
053300130711
053400130711         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
053500130711         RrnLast  = S01nrr;
053600130710
053700130711         // -?Impaginazione subfile?
053800130711         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
053900130710         if S01nrr > *zero;
054000130710           C1RcdNbr  = 1;
054100130710           C1CsrRrn  = 1;
054200130710         else;
054300130710           clear C1RcdNbr;
054400130710         endif;
054500130710
054600130711         // -?Ordinamento subfile (se premuto F8)?
054700130711         if  Not $Ord_x_CodCmm  and  S01nrr > *zero;
054800130710           exsr  sr_SortSfl;
054900130710         endif;
055000130711
055100130711         // -?(Dis)Abilitazione tasti funzionali?
055200130711         //F3Attivo  = (%parms() = 1);   // -?F3 = Fine?
055300130711         F5Attivo  = *on;              // -?F5 = Refresh?
055400130711         F8Attivo  = (S01nrr > *zero); // -?F8 = Ordinamento x ...?
055500130711         //F12Attivo = (%parms() > 1);   // -?F12 = Ritorno?
055600130711         F12Attivo = *on;              // -?F12 = Ritorno?
055700130711
055800130711         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
055900130711         Select;
056000130711           When  Not F8Attivo;
056100130711             clear  P01f08;
056200130711           When  $Ord_x_CodCmm;
056300130711             P01f08 = 'F8=Ord. x Descrizione';
056400130711           When  $Ord_x_DesCmm;
056500130711             P01f08 = 'F8=Ord. x Unificante ';
056600130711           When  $Ord_x_CodUni;
056700130711             P01f08 = 'F8=Ord. x Cod.Comm.le';
056800130711         EndSl;
056900130710
057000130710       ENDSR;
057100130710
057200130710       //--------------------------------------------------------------
057300130711       //?Ricerca del 1° record valido successivo in AZCMM00F          ?
057400130710       //--------------------------------------------------------------
057500130711       BEGSR  sr_ReadAZCMM;
057600130711
057700130711         reset  $EoF;
057800130711
057900130711         DoW  Not $EoF;
058000130710
058100130711           read  AZCMM000;
058200130711
058300130711           Select;
058400130711
058500130711             // -?End Of File => Fine?
058600130711             When  %eof(AZCMM01L);
058700130711               $EoF = *on;
058800130711               leave;
058900130711
059000130711             // -?Richiesti commerciali di una filiale:?
059100130711             //  ?al 1° commerciale di filiale diversa => Fine?
059200130711             When  C1Cfil <> *zero     and
059300130711                   C1Cfil <> %int( CMMcod / 10000 );
059400130711               $EoF = *on;
059500130711               leave;
059600130711
059700130711             //  ?Si scartano i comm.li non più attivi e quelli?
059800130711             //  ?annullati => Read Next?
059900130711             When  CMMatb  <> *blank  or
060000130711                   CMMdtIni > wDate   or  CMMdtFin < wDate;
060100130711               iter;
060200130711
060300130711             // -?Richiesti solo i comm.li unificanti:?
060400130711             //  ?si scartano i comm.li NON unificanti => Read Next?
060500130711             When  %parms() > 1   and   iMK43solU = 'S'   and
060600130711                   CMMcod   <> CMMuni;
060700130711               iter;
060800130711
060900130711             // -?Richiesto posizionamento per codice comm.le:?
061000130711             //  ?si scartano i comm.li con codice inferiore?
061100130711             When  $Ord_x_CodCmm  and  CMMcod < C1Ccod;
061200130711               iter;
061300130711
061400130711             // -?Richiesto posizionamento per descrizione comm.le:?
061500130711             //  ?si scartano i comm.li con descrizione inferiore?
061600130711             When  $Ord_x_DesCmm  and  CMMdes < C1Cdes;
061700130711               iter;
061800130711
061900130711             // -?Richiesto posizionamento per unificante:?
062000130711             //  ?si scartano i comm.li con unificante inferiore?
062100130711             When  $Ord_x_CodUni  and  CMMuni < C1Cuni;
062200130711               iter;
062300130711
062400130711             // -?Superato il limite max di rec. caricabili nel sfl?
062500130711             //  ?=> Fine?
062600130711             When  S01nrr >= c_MaxRec;
062700130711               $EoF = *on;
062800130711               leave;
062900130711
063000130711             // -?Richiesti tutti i commerciali => Ok (uscita)?
063100130711             Other;
063200130711               leave;
063300130711
063400130711           EndSl;
063500130711
063600130711         EndDo;
063700130710
063800130710       ENDSR;
063900130710
064000130710       //--------------------------------------------------------------
064100130711       //?Caricamento di tutto il subfile S01.                         ?
064200130710       //--------------------------------------------------------------
064300130711       BEGSR  sr_CaricaS01;
064400130710
064500130711         clear  S01nrr;
064600130710         SflNxtChg = *off;
064700130710
064800130711         // -?Ciclo di caricamento dati da file AZCMM00F?
064900130711         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
065000130711         //  ?l'eventuale ordinamento)?
065100130710         DoW  Not $EoF;
065200130710
065300130711           // -?Impostazione campi nel record del subfile?
065400130711           clear  MK43S01;
065500130711           S1Ccmm = CMMcod;
065600130711           S1Dcmm = CMMdes;
065700130711           S1Cuni = CMMuni;
065800130710
065900130711           // -?Impostazione attributi di visualizzazione?
066000130711           Ord_1 = ($Ord_x_DesCmm);
066100130711           Ord_2 = ($Ord_x_CodUni);
066200130711
066300130711           // -?Registrazione singolo record nel subfile?
066400130710           S01nrr += 1;
066500130711           write  MK43S01;
066600130711
066700130711           // -?Lettura record successivo?
066800130711           exsr  sr_ReadAZCMM;
066900130710
067000130710         EndDo;
067100130710
067200130711         // -?Memorizzazione posizionamento effettuato?
067300130711         SavC1Cfil = C1Cfil;
067400130711         select;
067500130711           when  $Ord_x_CodCmm;
067600130711             SavC1Ccod = C1Ccod;
067700130711           when  $Ord_x_DesCmm;
067800130711             SavC1Cdes = C1Cdes;
067900130711           when  $Ord_x_CodUni;
068000130711             SavC1Cuni = C1Cuni;
068100130711         endsl;
068200130710
068300130711         // -?Visualizzazione del SFL (se ci sono dati)?
068400130710         SflDsp_N  = (S01nrr <= *zero);
068500130710
068600130711         // -?Attivazione del SFLEND?
068700130710         SflEnd = $EoF;
068800130710
068900130711         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
069000130711         if  S01nrr > *zero;
069100130711           C1RcdNbr = 1;
069200130711         else;
069300130711           clear  C1RcdNbr;
069400130711         endif;
069500130711
069600130711         // -?Emissione messaggio di segnalazione raggiungimento limite?
069700130711         if  S01nrr >= c_MaxRec   and   Not %eof(AZCMM01L);
069800130710           ErrGenerico = *on;
069900130711           ErrMessage  = *on;
070000130710           PosCurFIL   = *on;
070100130711           V1Dmsg = $Msg(01);
070200130710           leavesr;
070300130710         endif;
070400130710
070500130710       ENDSR;
070600130710
070700130710       //--------------------------------------------------------------
070800130710       //?Gestione opzioni del subfile S01                             ?
070900130710       //--------------------------------------------------------------
071000130710       BEGSR  sr_OpzS01;
071100130711
071200130711         // -?Spegnimento degli indicatori di errore?
071300130711         %subst(IndDspF : 50) = *off;
071400130710
071500130710         clear  SavS01csr;
071600130711         clear  wMemoSel;
071700130710
071800130711         // -?Ciclo di lettura subfile?
071900130711         readc  MK43S01;
072000130710
072100130711         DoW  Not %eof(TRMK43D);
072200130710
072300130710           SflNxtChg = *off;
072400130710           %subst(IndDspF : 50) = *off;
072500130710           C1RcdNbr = S01nrr;
072600130710
072700130710           select;
072800130711
072900130711             // -?Opzione + Posizionamento?
073000130711             when  S1Copz  <> *zero      and
073100130711                  (C1Cfil  <> SavC1Cfil   or
073200130711                   C1Ccod  <> SavC1Ccod   or
073300130711                   C1Cdes  <> SavC1Cdes   or
073400130711                   C1Cuni  <> SavC1Cuni);
073500130711               ErrGenerico = *on;
073600130711               ErrMessage  = *on;
073700130711               select;
073800130711                 when C1Cfil  <> SavC1Cfil;
073900130711                   PosCurFIL   = *on;
074000130711                 when C1Ccod  <> SavC1Ccod;
074100130711                   PosCurCOD   = *on;
074200130711                 when C1Cdes  <> SavC1Cdes;
074300130711                   PosCurDES   = *on;
074400130711                 when C1Cuni  <> SavC1Cuni;
074500130711                   PosCurUNI   = *on;
074600130711               endsl;
074700130711               V1Dmsg = $Msg(02);
074800130710
074900130711             // -?Opz. 1=Selezione commerciale?
075000130711             when  S1Copz = 1;
075100130711               // -?Già selezionato un commerciale?
075200130711               if  wMemoSel > *zero;
075300130711                 ErrGenerico = *on;
075400130711                 ErrMessage  = *on;
075500130711                 PosCurOPZ   = *on;
075600130711                 V1Dmsg = $Msg(04);
075700130711               else;
075800130711                 wMemoSel = S1Copz;
075900130711               endif;
076000130710
076100130711             // -?Opz. 6=Selezione unificante?
076200130711             when  S1Copz = 6  and  AbilitOpz6 = *on;
076300130711               // -?Già selezionato un commerciale?
076400130711               if  wMemoSel > *zero;
076500130711                 ErrGenerico = *on;
076600130711                 ErrMessage  = *on;
076700130711                 PosCurOPZ   = *on;
076800130711                 V1Dmsg = $Msg(04);
076900130711               else;
077000130711                 wMemoSel = S1Copz;
077100130711               endif;
077200130711
077300130711             // -?Nessuna opzione?
077400130711             when  S1Copz = *zero;
077500130710
077600130711             // -?Opzione errata?
077700130710             other;
077800130711               ErrGenerico = *on;
077900130710               ErrMessage  = *on;
078000130710               PosCurOPZ   = *on;
078100130711               V1Dmsg = $Msg(03);
078200130710
078300130710           endsl;
078400130710
078500130711           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
078600130711           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
078700130711           if  S1Copz   <> *zero  and
078800130711              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
078900130710             SavS01csr   = C1RcdNbr;
079000130710           endif;
079100130710
079200130711           // -?Aggiornamento sfl?
079300130710           select;
079400130711             when  ErrMessage  or  S1Copz <> *zero;
079500130710               SflNxtChg = *on;
079600130710               C1CsrRrn  = C1RcdNbr;
079700130710             when  ErrGenerico;
079800130710               C1CsrRrn  = C1RcdNbr;
079900130711               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
080000130710             other;
080100130711               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
080200130710           endsl;
080300130711
080400130711           UPDATE  MK43S01;
080500130711
080600130710           if  ErrGenerico   or   ErrMessage;
080700130710             leave;
080800130710           endif;
080900130710
081000130711           readc  MK43S01;
081100130710
081200130710         ENDDO;
081300130710
081400130711         // -?Riposizionam. cursore sul record contenente la prima opz.?
081500130711         //  ?(se non sono stati rilevati errori)?
081600130710         if  NOT ErrMessage   and   NOT ErrGenerico   and
081700130710             SavS01csr > *zero;
081800130710           C1CsrRrn = SavS01csr;
081900130710         endif;
082000130710
082100130710       ENDSR;
082200130710
082300130710       //--------------------------------------------------------------
082400130710       //?Controllo filiale inserita in C01                            ?
082500130710       //--------------------------------------------------------------
082600130710       BEGSR  sr_CtrFil;
082700130710
082800130711         If  C1Cfil <> *zero;
082900130710
083000130711           chain  (C1Cfil)  AZORG;
083100130710
083200130710           select;
083300130710             when  Not %found(AZORG01L);
083400130710               ErrMessage  = *on;
083500130710               ErrGenerico = *on;
083600130710               PosCurFIL   = *on;
083700130711               V1Dmsg = $Msg(05);
083800130710               leavesr;
083900130710             when  ORGfva <> *blank;
084000130710               ErrMessage  = *on;
084100130710               ErrGenerico = *on;
084200130710               PosCurFIL   = *on;
084300130711               V1Dmsg = $Msg(06);
084400130710               leavesr;
084500130710             when  ORGfag <> 'F'   and   ORGfva <> 'A';
084600130710               ErrMessage  = *on;
084700130710               ErrGenerico = *on;
084800130710               PosCurFIL   = *on;
084900130711               V1Dmsg = $Msg(07);
085000130710               leavesr;
085100130710           endsl;
085200130710
085300130711         EndIf;
085400130710
085500130711         SavC1Cfil = C1Cfil;
085600130710
085700130710       ENDSR;
085800130710
085900130710       //--------------------------------------------------------------
086000130710       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
086100130710       //?F08-Cambia ordinamento sfl                                   ?
086200130710       //--------------------------------------------------------------
086300130710       BEGSR  sr_F08S01;
086400130710
086500130711         Select;
086600130711           When $Ord_x_CodCmm;
086700130711             $Ord_x_CodCmm = *off;
086800130711             $Ord_x_DesCmm = *on;
086900130711             $Ord_x_CodUni = *off;
087000130711           When $Ord_x_DesCmm;
087100130711             $Ord_x_CodCmm = *off;
087200130711             $Ord_x_DesCmm = *off;
087300130711             $Ord_x_CodUni = *on;
087400130711           When $Ord_x_CodUni;
087500130711             $Ord_x_CodCmm = *on;
087600130711             $Ord_x_DesCmm = *off;
087700130711             $Ord_x_CodUni = *off;
087800130711         EndSl;
087900130711
088000130809         //clear  C1Cfil;    ? EH NNO!!! ?
088100130711         clear  C1Ccod;
088200130711         clear  C1Cdes;
088300130711         clear  C1Cuni;
088400130711
088500130711         select;
088600130711           // -?Subfile già parzializzato: occorre ricaricarlo?
088700130711           when  SavC1Cfil <> C1Cfil;
088800130711             $InzS01 = *on;
088900130711           // -?Subfile già posizionato: occorre ricaricarlo?
089000130711           when  SavC1Ccod <> *zero   or
089100130711                 SavC1Cdes <> *blank  or
089200130711                 SavC1Cuni <> *zero;
089300130711             $InzS01 = *on;
089400130711           // -?Altrimenti: basta riordinarlo?
089500130711           other;
089600130711             exsr  sr_SortSfl;
089700130711         endsl;
089800130711
089900130711         reset  SavC1Ccod;
090000130711         reset  SavC1Cdes;
090100130711         reset  SavC1Cuni;
090200130710
090300130710       ENDSR;
090400130710
090500130710       //--------------------------------------------------------------
090600130710       //?Ordinamento subfile                                          ?
090700130710       //?  This subroutine sorts the subfile records.                 ?
090800130710       //--------------------------------------------------------------
090900130710       BEGSR  sr_SortSfl;
091000130710
091100130711         // -?Impostazione NRR dell'ultimo record del subfile          ?
091200130711         //  ?(già fatto in sr_InzS01:                                 ?
091300130711         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
091400130711         //RrnLast  = S01nrr;
091500130710
091600130710         C1RcdNbr = 1;
091700130710         clear  C1CsrRrn;
091800130710         clear  S01nrr;
091900130710         clear  V1Dmsg;
092000130710         SflNxtChg   = *off;
092100130711         ErrMessage  = *off;
092200130710         ErrGenerico = *off;
092300130710         %subst(IndDspF : 50) = *off;
092400130710
092500130711         //?___________________________________________________________?
092600130711         //?Initialize the key fields to sort on.?
092700130711         //?There is one extra field not in the subfile -- Selected --?
092800130711         //?that is added to the record. This field is used to place?
092900130711         //?selected records in front of nonselected records.?
093000130711         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
093100130710
093200130710         clear  QLGKL;
093300130710         clear  QLGSKL;
093400130710         clear  QLGSCB;
093500130710         clear  QLGSCB00;
093600130710
093700130711         // -?Gestione della scelta ordinamento:?
093800130711         Select;
093900130711
094000130711           // -?Ordinamento per Codice Commerciale?
094100130711           when  $Ord_x_CodCmm;
094200130711             // -?1 campi chiave?
094300130711             QLGNBRK  = 1;
094400130711             // -?1° campo: codice comm.le (S1Ccmm)?
094500130711             //            ?a posizone    3,  7 byte, int,  ascending.?
094600130711             QLGSP    = %size(S1Copz) + 1;
094700130711             QLGSS    = %size(S1Ccmm);
094800130711             QLGDT    = NumIntero;
094900130711             QLGSO    = Ascendente;
095000130711             QLGKL(1) = QLGSKL;
095100130711
095200130711           // -?Ordinamento per Descrizione Commerciale?
095300130711           when  $Ord_x_DesCmm;
095400130711             // -?2 campi chiave?
095500130711             QLGNBRK  = 2;
095600130711             // -?1° campo: descr. comm.le (S1Dcmm)?
095700130711             //            ?a posizone   10, 35 byte, char, ascending.?
095800130711             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + 1;
095900130711             QLGSS    = %size(S1Dcmm);
096000130711             QLGDT    = Carattere;
096100130711             QLGSO    = Ascendente;
096200130711             QLGKL(1) = QLGSKL;
096300130711             // -?2° campo: descr. comm.le (S1Ccmm)?
096400130711             //            ?a posizone    3,  7 byte, int,  ascending.?
096500130711             QLGSP    = %size(S1Copz) + 1;
096600130711             QLGSS    = %size(S1Ccmm);
096700130711             QLGDT    = NumIntero;
096800130711             QLGSO    = Ascendente;
096900130711             QLGKL(2) = QLGSKL;
097000130711
097100130711           // -?Ordinamento per Codice Unificante?
097200130711           when  $Ord_x_CodUni;
097300130711             // -?2 campi chiave?
097400130711             QLGNBRK  = 2;
097500130711             // -?1° campo: codice unificante (S1Cuni)?
097600130711             //            ?a posizone   45,  7 byte, int,  ascending.?
097700130711             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + %size(S1Dcmm) + 1;
097800130711             QLGSS    = %size(S1Cuni);
097900130711             QLGDT    = NumIntero;
098000130711             QLGSO    = Ascendente;
098100130711             QLGKL(1) = QLGSKL;
098200130711             // -?2° campo: cod. comm.le (S1Ccmm)?
098300130711             //            ?a posizone    3,  7 byte, int,  ascending.?
098400130711             QLGSP    = %size(S1Copz) + 1;
098500130711             QLGSS    = %size(S1Ccmm);
098600130711             QLGDT    = NumIntero;
098700130711             QLGSO    = Ascendente;
098800130711             QLGKL(2) = QLGSKL;
098900130711
099000130711         EndSl;
099100130710
099200130711         // -?Load other sort parameters.?
099300130710         QLGLB   = 80 + 16 * MaxKey;
099400130710         QLGRL   = %size(SflRcd) - 1;
099500130710         QLGRT   = 8;
099600130710         QLGOKL  = 80;
099700130710         QLGLKE  = 16;
099800130710         QLGLSS  = 290;
099900130711         // -?Initialize Sort I/O API fields.?
100000130710         QLGRL00 = QLGRL;
100100130710         QLGRC00 = 1;
100200130710         clear  QUSEI;
100300130710         QUSBPRV = %size(QUSEC);
100400130710
100500130711         // -?First step - Initialize the sort routine.?
100600130710         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
100700130710                      SizeList : ReturnSize : QUSEC );
100800130710
100900130711         // -?Next step - Write records to I/O routine.?
101000130710         QLGRT00  = Put;
101100130710         For  Nrr = 1  To  RrnLast;
101200130711           chain  Nrr  MK43S01;
101300130711           // -?Solo le righe con Selected = 'Y' sono riordinate,?
101400130711           //  ?quindi per fare un ordinamento di tutte le righe?
101500130711           //  ?metto 'Y' sempre.?
101600130710           Selected = 'Y';
101700130710           clear  QUSEI;
101800130710           QUSBPRV  = %size(QUSEC);
101900130710           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
102000130710                         SizeList : NotUsed : QUSEC );
102100130710         EndFor;
102200130710
102300130711         // -?Next step - Signal end of input, clear subfile for reload.?
102400130710         QLGRT00 = EndPut;
102500130710         clear  QUSEI;
102600130710         QUSBPRV = %size(QUSEC);
102700130710         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
102800130710                       SizeList : NotUsed : QUSEC );
102900130710
103000130711         // -?Pulizia subfile?
103100130710         SflDsp_N    = *on;
103200130710         SflDspCtl_N = *on;
103300130711         write  MK43C01;
103400130710         SflDspCtl_N = *off;
103500130710         SflEnd      = *off;
103600130710
103700130711         // -?Final step - Write the records back to the subfile.?
103800130710         QLGRT00  = Get;
103900130710         For  Nrr = 1  To RrnLast;
104000130710           clear  QUSEI;
104100130710           QUSBPRV = %size(QUSEC);
104200130710           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
104300130710                         QLGRL00  : NotUsed : QUSEC );
104400130711           // -?Ripristino indicatori utilizzati nel sfl rec?
104500130711           SflNxtChg = (S1Copz <> *zero);
104600130711           Ord_1 = ( $Ord_x_DesCmm );
104700130711           Ord_2 = ( $Ord_x_CodUni );
104800130710           S01nrr = Nrr;
104900130711           write  MK43S01;
105000130710         EndFor;
105100130710
105200130710         C1CsrRrn = 1;
105300130710
105400130711         // -?Visualizzazione del SFL (se ci sono dati)?
105500130710         SflDsp_N = (S01nrr <= *zero);
105600130710
105700130711         // -?Attivazione del SFLEND?
105800130710         SflEnd = $EoF;
105900130711
106000130711         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
106100130711         Select;
106200130711           When  $Ord_x_CodCmm;
106300130711             P01f08 = 'F8=Ord. x Descrizione';
106400130711           When  $Ord_x_DesCmm;
106500130711             P01f08 = 'F8=Ord. x Unificante ';
106600130711           When  $Ord_x_CodUni;
106700130711             P01f08 = 'F8=Ord. x Cod.Comm.le';
106800130711         EndSl;
106900130710
107000130710       ENDSR;
107100130710
107200130710       //--------------------------------------------------------------
107300130710       //?Operazioni finali.                                         ?
107400130710       //--------------------------------------------------------------
107500130710       BEGSR  sr_RoutEnd;
107600130711
107700130711         // -?Restituzione parametri?
107800130711         //  ?(se selezionato un commerciale)?
107900130712         IF  wMemoSel > *zero;
108000130711
108100130711           chain  SavS01csr  MK43S01;
108200130711
108300130711           If  %parms() >= 2;
108400130711
108500130726             If  S1Copz = 1;
108600130711
108700130711               oMK43cmm = S1Ccmm;
108800130711               oMK43des = S1Dcmm;
108900130711
109000130711               if  %parms() >= 3;
109100130711                 clear  k01azcmm01;
109200130711                 k_CMMcod = S1Ccmm;
109300130711                 chain  %kds(k01azcmm01)  AZCMM000;
109400130711                 if  %found(AZCMM01L);
109500130711                   exsr  sr_Prm_AZCMM;
109600130711                 endif;
109700130711               endif;
109800130711
109900130711             Else;
110000130711
110100130711               oMK43cmm = S1Cuni;
110200130711               clear  k01azcmm01;
110300130711               k_CMMcod = S1Cuni;
110400130711               chain  %kds(k01azcmm01)  AZCMM000;
110500130711               if  %found(AZCMM01L);
110600130711                 oMK43des = CMMdes;
110700130711                 if  %parms() >= 3;
110800130711                   exsr  sr_Prm_AZCMM;
110900130711                 endif;
111000130711               endif;
111100130712
111200130712             EndIf;
111300130711
111400130712             If  %parms() >= 4;
111500130712
111600130712               if  not %open(AZNTC01L);
111700130712                 open  AZNTC01L;
111800130712               endif;
111900130712
112000130712               clear  k02azntc01;
112100130712               if  wMemoSel = 1;
112200130712                 k_NTCcmm = S1Ccmm;
112300130712               else;
112400130712                 k_NTCcmm = S1Cuni;
112500130712               endif;
112600130712
112700130712               k_NTCtnt = '01';
112800130712               chain  %kds(k02azntc01)  AZNTC000;
112900130712               if  %found(AZNTC01L);
113000130712                 prm_AZNTC01_ds.NTCatb = NTCatb;
113100130712                 prm_AZNTC01_ds.NTCcmm = NTCcmm;
113200130712                 prm_AZNTC01_ds.NTCtnt = NTCtnt;
113300130712                 prm_AZNTC01_ds.NTCrnt = NTCrnt;
113400130712               endif;
113500130712
113600130712               If  %parms() >= 5;
113700130712
113800130712                 clear  k02azntc01;
113900130712                 if  wMemoSel = 1;
114000130712                   k_NTCcmm = S1Ccmm;
114100130712                 else;
114200130712                   k_NTCcmm = S1Cuni;
114300130712                 endif;
114400130712
114500130712                 k_NTCtnt = '02';
114600130712                 chain  %kds(k02azntc01)  AZNTC000;
114700130712                 if  %found(AZNTC01L);
114800130712                   prm_AZNTC02_ds.NTCatb = NTCatb;
114900130712                   prm_AZNTC02_ds.NTCcmm = NTCcmm;
115000130712                   prm_AZNTC02_ds.NTCtnt = NTCtnt;
115100130712                   prm_AZNTC02_ds.NTCrnt = NTCrnt;
115200130712                 endif;
115300130712
115400130712               EndIf;
115500130712
115600130712             EndIf;
115700130712
115800130712           EndIf;
115900130712
116000130712         ENDIF;
116100130710
116200130711         // -?Chiusura *pgm?
116300130710         return;
116400130710
116500130710       ENDSR;
116600130711
116700130711       //--------------------------------------------------------------
116800130711       //?Impostazione parametri da restituire relativi a AZCMM00F.  ?
116900130711       //--------------------------------------------------------------
117000130711       BEGSR  sr_Prm_AZCMM;
117100130711
117200130711         prm_AZCMM_ds.CMMatb = CMMatb;
117300130711         prm_AZCMM_ds.CMMcod = CMMcod;
117400130711         prm_AZCMM_ds.CMMdes = CMMdes;
117500130711         prm_AZCMM_ds.CMMpar = CMMpar;
117600130711         prm_AZCMM_ds.CMMfun = CMMfun;
117700130711         prm_AZCMM_ds.CMMpli = CMMpli;
117800130711         prm_AZCMM_ds.CMMple = CMMple;
117900130711         prm_AZCMM_ds.CMMdtIni = CMMdtIni;
118000130711         prm_AZCMM_ds.CMMdtFin = CMMdtFin;
118100130711         prm_AZCMM_ds.CMMdip = CMMdip;
118200130711         prm_AZCMM_ds.CMMsoc = CMMsoc;
118300130711         prm_AZCMM_ds.CMMfil = CMMfil;
118400130711         prm_AZCMM_ds.CMMfil10 = CMMfil10;
118500130711         prm_AZCMM_ds.CMMdtIns = CMMdtIns;
118600130711         prm_AZCMM_ds.CMMdtMod = CMMdtMod;
118700130711         prm_AZCMM_ds.CMMute = CMMute;
118800130711         prm_AZCMM_ds.CMMuni = CMMuni;
118900130711
119000130711       ENDSR;
119100130710
119200130710      /end-free
119300130710
119400130711** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
119500130711Raggiunto il limite massimo dei record caricabili a video                      1
119600130711Non sono ammesse opzioni quando è richiesta una selezione o un posizionamento  2
119700130711Opzione errata                                                                 3
119800130711Selezionati più di un commerciale                                              4
119900130711Filiale inesistente                                                            5
120000130711Filiale annullata                                                              6
120100130711Filiale NON valida                                                             7
