000100130523       //==============================================================
000200130523       //?Gestione file FIIRD10F (Immagini Recupero Distinte)          ?
000300130523       //==============================================================
000400130523
000500130523       //--------------------------------------------------------------
000600130523       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130523       //--------------------------------------------------------------
000800130523
000900130523     /*PRM dbgview(*source)
001000130524     /*CMD ovrdbf file(FIDST01L) tofile(FILTRAPRD/FIDST01L) +
001100130524     /*CMD        ovrscope(*calllvl)
001200130524     /*END dltovr file(FIDST01L) lvl(*)
001300130523     /*END
001400130523
001500130523       //--------------------------------------------------------------
001600130523       //?Specifiche di controllo.                                     ?
001700130523       //--------------------------------------------------------------
001800130523
001900130523     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000130523
002100130523       //--------------------------------------------------------------
002200130523       //?Dichiarazione file.                                          ?
002300130523       //--------------------------------------------------------------
002400130523
002500130523       // -?Organigramma?
002600130523     fAZORG01L  if   e           k disk
002700130523
002800130523       // -?Distinte di Consegna?
002900130524     fFIDST01L  if   e           k disk    extfile(wLibFIDST)  usropn
003000130523
003100130523       // -?Anagrafica Autotrasportatori/Cooperative?
003200130523     fFIAPD01L  if   e           k disk
003300130523
003400130523       // -?Immagini Recupero Distinte?
003500130524     fFIIRD11L  Uf A e           k disk
003600130523
003700130523       // -?Video?
003800130523     fFNLR67D   cf   e             workstn
003900130523     f                                     sfile(LR67S01 : S01nrr)
004000130523     f                                     indds(IndDspF)
004100130523     f                                     infds(InfDspF)
004200130523
004300130523       //--------------------------------------------------------------
004400130523       //?Definizione costanti.                                        ?
004500130523       //--------------------------------------------------------------
004600130523
004700130523       // -?Costante per controllo "caratteri solo numerici"?
004800130523     d c_Digits        c                   const('0123456789')
004900130523
005000130523       // -?Tasti funzionali a video?
005100130523     d c_F01           c                   const(x'31')
005200130523     d c_F02           c                   const(x'32')
005300130523     d c_F03           c                   const(x'33')
005400130523     d c_F04           c                   const(x'34')
005500130523     d c_F05           c                   const(x'35')
005600130523     d c_F06           c                   const(x'36')
005700130523     d c_F07           c                   const(x'37')
005800130523     d c_F08           c                   const(x'38')
005900130523     d c_F09           c                   const(x'39')
006000130523     d c_F10           c                   const(x'3A')
006100130523     d c_F11           c                   const(x'3B')
006200130523     d c_F12           c                   const(x'3C')
006300130523     d c_F13           c                   const(x'B1')
006400130523     d c_F14           c                   const(x'B2')
006500130523     d c_F15           c                   const(x'B3')
006600130523     d c_F16           c                   const(x'B4')
006700130523     d c_F17           c                   const(x'B5')
006800130523     d c_F18           c                   const(x'B6')
006900130523     d c_F19           c                   const(x'B7')
007000130523     d c_F20           c                   const(x'B8')
007100130523     d c_F21           c                   const(x'B9')
007200130523     d c_F22           c                   const(x'BA')
007300130523     d c_F23           c                   const(x'BB')
007400130523     d c_F24           c                   const(x'BC')
007500130523     d c_Enter         c                   const(x'F1')
007600130523     d c_RollDown      c                   const(x'F4')
007700130523     d c_RollUp        c                   const(x'F5')
007800130523
007900130523       //--------------------------------------------------------------
008000130523       //?Definizione schiere.                                         ?
008100130523       //--------------------------------------------------------------
008200130523
008300130523       // -?Messaggi di errore?
008400130524     d $Msg            s             78    dim(12)  ctdata  perrcd( 1)
008500130523
008600130523       //--------------------------------------------------------------
008700130523       //?Definizione aree dati.                                       ?
008800130523       //--------------------------------------------------------------
008900130523
009000130523       // -?Dati utente?
009100130523     d §AzUte        e ds                  extname(AZUTE00F)
009200130523     d                                     dtaara
009300130523     d §DatiUte      e ds                  extname(dDatiUte)
009400130523     d                                     dtaara
009500130523
009600130523       //--------------------------------------------------------------
009700130523       //?Definizione strutture dati.                                  ?
009800130523       //--------------------------------------------------------------
009900130523
010000130523       // -?Status ds?
010100130523     d Status         sds
010200130523     d   SDSpgm          *proc
010300130523
010400130523       // -?InfDS?
010500130523     d InfDspF         ds
010600130523     d   dsp_aid             369    369a
010700130523     d   sfl_rrn             376    377i 0
010800130523     d   min_nrr             378    379i 0
010900130523     d   num_rcds            380    381i 0
011000130523
011100130523       // -?Indicatori su DspF?
011200130523     d IndDspF         ds                  inz
011300130523         // -?Abilitazione tasti funzionali?
011400130524     d   F6Attivo                      n   overlay(IndDspF : 06)
011500130523     d   F8Attivo                      n   overlay(IndDspF : 08)
011600130527     d   F9Attivo                      n   overlay(IndDspF : 09)
011700130523         // -?Emissione messaggio di errore?
011800130523     d   ErrMessage                    n   overlay(IndDspF : 28)
011900130523         // -?Indicatori di gestione del subfile?
012000130523     d   SflDsp_N                      n   overlay(IndDspF : 30)
012100130523     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012200130523     d   SflNxtChg                     n   overlay(IndDspF : 32)
012300130523     d   SflEnd                        n   overlay(IndDspF : 33)
012400130523         // -?Indicatori per Attibuti di visualizzazione?
012500130523     d   Ord_x_Dist                    n   overlay(IndDspF : 40)
012600130523     d   Ord_x_TpEl                    n   overlay(IndDspF : 41)
012700130523     d   Ord_x_DtIn                    n   overlay(IndDspF : 42)
012800130524         // -?Protezione campi chiave SE non immissione?
012900130524     d   Protect_Key                   n   overlay(IndDspF : 45)
013000130524         // -?Sistema Informativo di Sede?
013100130524     d   In_Sede                       n   overlay(IndDspF : 46)
013200130523         // -?Posizionamento cursore & segnalazione errore?
013300130523     d   PosCurOPZ                     n   overlay(IndDspF : 50)
013400130523     d   PosCurFIL                     n   overlay(IndDspF : 51)
013500130523     d   PosCurNDC                     n   overlay(IndDspF : 52)
013600130523     d   PosCurTPEL                    n   overlay(IndDspF : 53)
013700130523     d   PosCurDTIN                    n   overlay(IndDspF : 54)
013800130524     d   PosCurC1pos                   n   overlay(IndDspF : 60)
013900130524         // -?Riemissione videata?
014000130523     d   ErrGenerico                   n   overlay(IndDspF : 99)
014100130523
014200130523       // -?Parametri ricevuti?
014300130523     d KPJBA         e ds
014400130524
014500130524       // -?148ª descrizione dell'organigramma?
014600130524     d Og148         e ds                  qualified       inz
014700130523
014800130523       //--------------------------------------------------------------
014900130523       //?Definizione variabili globali.                               ?
015000130523       //--------------------------------------------------------------
015100130523
015200130523       // -?Flags booleani?
015300130524     d $Almeno1rec     s               n   inz
015400130523     d $Fine           s               n   inz
015500130524     d $RecOK          s               n   inz
015600130523     d $Immissione     s               n   inz
015700130523     d $InzD01         s               n   inz(*on)
015800130523     d $InzS01         s               n   inz(*on)
015900130523
016000130523       // -?Variabili per la gestione del video?
016100130523     d $Video          s              2    inz('S1')
016200130523     d S01nrr          s                   like(C1RcdNbr)  inz
016300130523
016400130523       // -?Variabili per la gestione del posizionamento nel subfile?
016500130524     d SavS01csr       s                   like(C1RcdNbr)  inz
016600130523     d SavC01fil       s                   like(C01fil)    inz(*hival)
016700130523     d SavC01ndc       s                   like(C01ndc)    inz(*hival)
016800130523     d SavC01tpEl      s                   like(C01tpElab) inz(*hival)
016900130523     d SavC01dtIn      s                   like(C01datIns) inz(*hival)
017000130524
017100130524       // -?Nome esteso Libreria/File Distinte Consegna?
017200130524     d wLibFIDST       s             21a   inz
017300130523
017400130523       // -?Variabili di comodo?
017500130524     d wDate_EUR       s               d   datfmt(*eur)    inz(*loval)
017600130524     d wDate_ISO       s               d   datfmt(*iso)    inz(*loval)
017700130524     d w01ndcA         s                   like(D01ndc)    inz
017800130524     d w01ndcN         s                   like(C01ndc)    inz
017900130524     d w01datIns       s                   like(IRDdatIns) inz
018000130523
018100130523       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018200130523
018300130523       //--------------------------------------------------------------
018400130523       // -?Constants?
018500130523       //--------------------------------------------------------------
018600130523       // -?MaxKey - Maximum number of key fields allowed by this program?
018700130523     d MaxKey          c                   const(2)
018800130523       // -?Sort order:?
018900130523       //  ?1 = Sort in an ascending sequence?
019000130523       //  ?2 = Sort in a descending sequence?
019100130523     d Ascendente      c                   const(1)
019200130523     d Discendente     c                   const(2)
019300130523       // -?Key data type:?
019400130523       //  ? 0 = Signed binary?
019500130523       //  ? 2 = Signed zoned decimal?
019600130523       //  ? 3 = Signed packed decimal?
019700130523       //  ? 6 = Character with no national language sort sequence applied,?
019800130523       //  ?     if specified?
019900130523       //  ? 7 = Unsigned packed decimal?
020000130523       //  ?     All numbers will have the sign forced positive ('F'X).?
020100130523       //  ? 8 = Unsigned zoned decimal?
020200130523       //  ?     All numbers will have the sign forced positive ('F'X).?
020300130523       //  ? 9 = Unsigned binary?
020400130523       //  ?14 = Date in form of DD/MM/YY?
020500130523       //  ?15 = Date in form of DD.MM.YYYY?
020600130523     d Numero          c                   const(2)
020700130523     d Carattere       c                   const(6)
020800130523       //
020900130523     d Put             c                   const(1)
021000130523     d EndPut          c                   const(2)
021100130523     d Get             c                   const(3)
021200130523
021300130523       //--------------------------------------------------------------
021400130523       // -?Data Structures?
021500130523       //  ?SflRcd     - The entire subfile record to pass to the sort?
021600130523       //  ?QLGSCB     - The sort request block for the QLGSORT API?
021700130523       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
021800130523       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
021900130523       //  ?QUSEC      - Error structure for the QLGSORT API?
022000130523       //--------------------------------------------------------------
022100130523     d SflRcd          ds                  inz
022200130524     d   S1HfgsNdc                         inz                                   9 bytes
022300130524     d   S1HdatIns                         inz                                   8 bytes
022400130524     d   S01fil                            inz                                   3 bytes
022500130524     d   S01ndc                            inz                                   6 bytes
022600130524     d   S01tpElab                         inz                                   1 byte
022700130524     d   S01datIns                         inz                                   8 bytes
022800130524     d   S01opz                            inz                                   2 bytes
022900130524     d   Selected                     1a   inz                                   1 byte
023000130523      /copy QSYSINC/QRPGLESRC,QLGSORT
023100130523     d QLGKL                         16    dim(MaxKey)
023200130523     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023300130523     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023400130523     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023500130523     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023600130523      /copy QSYSINC/QRPGLESRC,QLGSRTIO
023700130523      /copy QSYSINC/QRPGLESRC,QUSEC
023800130523
023900130523       //--------------------------------------------------------------
024000130523       // -?Standalone fields?
024100130523       //  ?Nrr        - Relative record number for subfile?
024200130523       //  ?SizeList   - Size of list?
024300130523       //  ?ReturnSize - Size of list returned by sort API?
024400130523       //--------------------------------------------------------------
024500130523     d NotUsed         s             16a   inz
024600130523     d ReturnSize      s              9b 0 inz
024700130523     d SizeList        s              9b 0 inz
024800130523     d RrnLast         s              5i 0 inz
024900130523     d Nrr             s              5i 0 inz
025000130523
025100130523       //--------------------------------------------------------------
025200130523       //?Definizione prototipi procedure.                             ?
025300130523       //--------------------------------------------------------------
025400130523
025500130523       // -?Reperimento dati utente?
025600130523     d TIBS34ds      e ds                  inz
025700130523      /copy gaitrasrc/srcProtoPR,TIBS34R
025800130523
025900130523       // -?Interrogazione Fogli Vari (*pgm FNLR02R)?
026000130523     d dsLR01        e ds                  inz
026100130523     d   DLRnpg      e                     inz(4)
026200130523     d   DLRric      e                     inz('S')
026300130524     d   DLRtfv      e                     inz('1')
026400130527      *
026500130523     d fnlr02r         pr                  extpgm('FNLR02R')
026600130523     d   kpjba                             likeds(KPJBA)
026700130527
026800130527       // -?Interrogazione Spedizioni in Distinta?
026900130527     d FIDG311ds     e ds                  inz
027000130527     d   F31npg      e                     inz(4)
027100130527      *
027200130527      /copy gaitrasrc/srcProtoPR,FIDG311R
027300130524
027400130524       // -?Controllo/Inversione data?
027500130524     d WLBdat          ds                  inz
027600130524     d  G08dat                        8  0 inz
027700130524     d  G08inv                        8  0 inz
027800130524     d  G08err                        1    inz('3')
027900130524     d  G08tgi                        5  0 inz
028000130524      /copy gaitrasrc/srcProtoPR,XSRDA8
028100130523
028200130523       // -?Ordinamento subfile?
028300130523      /copy gaitrasrc/srcProtoPr,QLGSRTIO
028400130523
028500130523       //--------------------------------------------------------------
028600130523       //?Definizione key-list.                                        ?
028700130523       //--------------------------------------------------------------
028800130523
028900130523       // -?File AZORG01L?
029000130523     d k01azorg01    e ds                  extname(AZORG01L : *key)
029100130523     d                                     prefix(k_)   inz
029200130523
029300130523       // -?File FIDST01L?
029400130523     d k03fidst01    e ds                  extname(FIDST01L : *key)
029500130523     d                                     prefix(k_)   inz
029600130523
029700130523       // -?File FIDST01L?
029800130523     d k02fiapd01    e ds                  extname(FIAPD01L : *key)
029900130523     d                                     prefix(k_)   inz
030000130523
030100130523       // -?File FIIRD11L?
030200130523     d k01fiird11    e ds                  extname(FIIRD11L : *key)
030300130523     d                                     prefix(k_)   inz
030400130523
030500130523       //--------------------------------------------------------------
030600130523       //?Riepilogo indicatori utilizzati.                             ?
030700130523       //--------------------------------------------------------------
030800130523
030900130523
031000130523       //--------------------------------------------------------------
031100130523       //?M A I N - L I N E                                            ?
031200130523       //--------------------------------------------------------------
031300130523
031400130523     c     *Entry        plist
031500130523     c                   parm                    KPJBA
031600130523
031700130523      /free
031800130523
031900130523       // -?Operazioni iniziali?
032000130523       exsr  sr_RoutInz;
032100130523
032200130523       // -?Ciclo di gestione del file video?
032300130523       DoW  $Fine = *off;
032400130523
032500130523         select;
032600130523
032700130523           // -?Gestione videata S1 (subfile)?
032800130523           when  $Video = 'S1';
032900130523             exsr  sr_GesS01;
033000130523
033100130523           // -?Gestione videata D1 (gestione singola distinta)?
033200130523           when  $Video = 'D1';
033300130523             exsr  sr_GesD01;
033400130523
033500130523           // -?? ? ??
033600130523           other;
033700130523             $Fine = *on;
033800130523
033900130523         endsl;
034000130523
034100130523       EndDo;
034200130523
034300130523       // -?Operazioni finali?
034400130523       exsr  sr_RoutEnd;
034500130523
034600130523       //--------------------------------------------------------------
034700130523       //?Operazioni iniziali.                                         ?
034800130523       //--------------------------------------------------------------
034900130523       BEGSR  sr_RoutInz;
035000130523
035100130523         // -?Impostazione chiusura?
035200130523         *inLR = *on;
035300130523
035400130523         // -?Reperimento dati job?
035500130523         exsr  sr_DatiJob;
035600130523
035700130523         // -?Impostazione nome programma a video?
035800130523         V1Tpgm = SDSpgm;
035900130524
036000130524         // -?Apertura file Distinte Consegna?
036100130524         if  %subst( KNSIF : 7 : 1 ) = 'P';
036200130524           wLibFIDST = 'FILTRAPRD/FIDST01L';
036300130524         else;
036400130524           wLibFIDST = 'FILTRA201/FIDST01L';
036500130524         endif;
036600130524         open  FIDST01L;
036700130523
036800130523         // -?Impostazione videata iniziale?
036900130523         $Video = 'S1';
037000130523         reset  $InzS01;
037100130523         reset  $InzD01;
037200130523         Ord_x_Dist = *on;
037300130523         exsr  sr_InzS01;
037400130523         $InzS01 = *off;
037500130524         $Almeno1rec = (S01nrr > *zero);
037600130523
037700130523       ENDSR;
037800130523
037900130523       //--------------------------------------------------------------
038000130523       //?Reperimento Dati del job (Utente/Operativi).                 ?
038100130523       //--------------------------------------------------------------
038200130523       BEGSR  sr_DatiJob;
038300130523
038400130523         in(e) §AzUte;
038500130523         if NOT %error;
038600130523           in(e) §DatiUte;
038700130523         endif;
038800130523         if %error or RSut = *blank;
038900130523           tibs34r ( tibs34ds );
039000130523           in §AzUte;
039100130523           in §DatiUte;
039200130523         endif;
039300130523
039400130523       ENDSR;
039500130523
039600130523       //--------------------------------------------------------------
039700130523       //?Gestione subfile S01.                                        ?
039800130523       //--------------------------------------------------------------
039900130523       BEGSR  sr_GesS01;
040000130523
040100130523         // -?Inizializzazione videata?
040200130523         if  $InzS01 = *on;
040300130523           exsr  sr_InzS01;
040400130523           $InzS01 = *off;
040500130523         endif;
040600130523
040700130523         // -?Emissione Testata & Piede?
040800130523         write  LR67T01;
040900130523         write  LR67P01;
041000130523
041100130523         // -?Posizionamento cursore?
041200130523         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
041300130523         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
041400130523         //  ?si visualizza la pagina che vedeva l'utente quando ha?
041500130523         //  ?premuto l'ultimo tasto.?
041600130523         if  C1CsrRrn > *zero;
041700130523           C1RcdNbr = C1CsrRrn;
041800130523         else;
041900130523           C1RcdNbr = 1;
042000130524           write  LR67S00;          //?(no rec.)?
042100130523         endif;
042200130523
042300130523         // -?Emissione videata?
042400130524         exfmt  LR67C01;
042500130523
042600130524         clear  V1Dmsg;
042700130524         reset  ErrMessage;
042800130523         reset  ErrGenerico;
042900130524
043000130524         $Immissione = (dsp_aid = c_F06);
043100130524
043200130524         // -?Inversione Data Inserimento per posizionamento?
043300130524         if  Ord_x_DtIn  and  C01datIns  > *zero
043400130524                         and  C01datIns <> SavC01dtIn;
043500130524           clear WLBdat;
043600130524           G08dat = C01datIns;
043700130524           XSRDA8(WLBdat);
043800130524           if  G08err = *on;
043900130524             V1Dmsg = $Msg(11);
044000130524             PosCurC1pos = *on;
044100130524             ErrMessage  = *on;
044200130524             ErrGenerico = *on;
044300130524             leavesr;
044400130524           else;
044500130524             C01datIns = G08dat;
044600130524             w01datIns = G08inv;
044700130524           endif;
044800130524         endif;
044900130523
045000130523         Select;
045100130523
045200130523           // -?F3=Fine?
045300130523           When  dsp_aid = c_F03;
045400130523             $Fine = *on;
045500130523
045600130523           // -?F5=Rivisualizza?
045700130523           When  dsp_aid = c_F05;
045800130523             $InzS01 = *on;
045900130523
046000130523           // -?F6=Immissione?
046100130523           When  dsp_aid = c_F06;
046200130524             $Video  = 'D1';
046300130524             $InzD01 = *on;
046400130524             //DoW  $Video = 'D1'  and  Not $Fine;
046500130524             //  exsr  sr_GesD01;
046600130523             //EndDo;
046700130523
046800130523           // -?F8=Ordinamento?
046900130523           when  dsp_aid = c_F08;
047000130523             exsr  sr_F08S01;
047100130523
047200130523           // -?Invio?
047300130523           Other;
047400130523             // -?Gestione opzioni?
047500130523             if  S01nrr > *zero;
047600130523               exsr  sr_OpzS01;
047700130523               if  ErrGenerico;
047800130523                 leavesr;
047900130523               endif;
048000130523             endif;
048100130523             // -?Richiesto posizionamento?
048200130523             if  (Ord_x_Dist  and  (C01fil <> SavC01fil
048300130523                               or   C01ndc <> SavC01ndc))     OR
048400130523                 (Ord_x_TpEl  and   C01tpElab <> SavC01tpEl)  OR
048500130523                 (Ord_x_DtIn  and   C01datIns <> SavC01dtIn);
048600130523               $InzS01 = *on;
048700130523             endif;
048800130523
048900130523         endsl;
049000130523
049100130523       ENDSR;
049200130523
049300130523       //--------------------------------------------------------------
049400130523       //?Inizializzazione subfile S01.                                ?
049500130523       //--------------------------------------------------------------
049600130523       BEGSR  sr_InzS01;
049700130523
049800130523         // -?Spegnimento degli indicatori di errore?
049900130523         %subst(IndDspF : 50) = *off;
050000130523
050100130523         // -?Pulizia subfile?
050200130523         SflDsp_N    = *on;
050300130523         SflDspCtl_N = *on;
050400130523         write  LR67C01;
050500130523         SflDspCtl_N = *off;
050600130523         SflEnd      = *off;
050700130523
050800130523         clear  C1RcdNbr;
050900130523         clear  C1CsrRrn;
051000130523         clear  S01nrr;
051100130523         clear  V1Dmsg;
051200130523         ErrMessage  = *off;
051300130523         ErrGenerico = *off;
051400130523
051500130523         // -?Caricamento completo dei dati nel subfile?
051600130523         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
051700130523         //  ?l'eventuale ordinamento)?
051800130523         clear  k01fiird11;
051900130524         //setll  %kds( k01fiird11 )  FIIRD000;
052000130524         setll  (*loval)  FIIRD000;
052100130524         read(N)  FIIRD000;
052200130523         exsr  sr_CaricaS01;
052300130527
052400130527         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
052500130527         RrnLast  = S01nrr;
052600130523
052700130523         // -?Impaginazione subfile?
052800130523         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
052900130523         if S01nrr > *zero;
053000130523           C1RcdNbr  = 1;
053100130523           C1CsrRrn  = 1;
053200130523         else;
053300130523           clear C1RcdNbr;
053400130523         endif;
053500130523
053600130523         // -?Ordinamento subfile (se premuto F8)?
053700130523         if  Not  Ord_x_Dist  and  S01nrr > *zero;
053800130523           exsr  sr_SortSfl;
053900130523         endif;
054000130524
054100130524         // -?(Dis)Abilitazione tasti funzionali?
054200130524         F8Attivo = (S01nrr > *zero  or  $Almeno1rec = *on);
054300130524
054400130524         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
054500130524         Select;
054600130524           When  Not F8Attivo;
054700130524           When  Ord_x_Dist;
054800130524             P01f08 = 'F8=Ord. x Tipo Elab.   ';
054900130524           When  Ord_x_TpEl;
055000130524             P01f08 = 'F8=Ord. x Data Inserim.';
055100130524           When  Ord_x_DtIn;
055200130524             P01f08 = 'F8=Ord. x Distinta     ';
055300130524         EndSl;
055400130523
055500130523       ENDSR;
055600130523
055700130523       //--------------------------------------------------------------
055800130523       //?Caricamento completo del subfile S01                         ?
055900130523       //--------------------------------------------------------------
056000130523       BEGSR  sr_CaricaS01;
056100130523
056200130523         clear  S01nrr;
056300130523         SflNxtChg = *off;
056400130523
056500130523         // -?Ciclo di caricamento dati da file FIIRD10F?
056600130523         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
056700130523         //  ?l'eventuale ordinamento)?
056800130523         DoW  Not  %eof(FIIRD11L);
056900130524
057000130524           // -?Selezione singolo record in base al posizionamento?
057100130524           exsr  sr_SelezRec;
057200130524
057300130524           // -?Registrazione singolo record nel subfile?
057400130524           If  $RecOK;
057500130523
057600130524             // -?Impostazione campi nel record del subfile?
057700130524             clear  LR67S01;
057800130524
057900130524             S1HfgsNdc = IRDfgsNdc;
058000130524             S1HdatIns = IRDdatIns;
058100130524
058200130524             S01fil    = %int( %subst( IRDfgsNdc : 1 : 3 ) );
058300130524             S01ndc    = %int( %subst( IRDfgsNdc : 4 : 6 ) );
058400130524             S01tpElab = IRDtpElab;
058500130524             wDate_Eur = %date( IRDdatIns : *iso );
058600130524             S01datIns = %dec( wDate_Eur );
058700130524
058800130524             // -?Scrittura record nel subfile?
058900130524             S01nrr += 1;
059000130524             write  LR67S01;
059100130524
059200130524           EndIf;
059300130523
059400130523           // -?Lettura record successivo?
059500130524           read(N)  FIIRD000;
059600130523
059700130523         EndDo;
059800130523
059900130523         // -?Memorizzazione posizionamento effettuato?
060000130523         select;
060100130523           when  Ord_x_Dist;
060200130523             SavC01fil  = C01fil;
060300130523             SavC01ndc  = C01ndc;
060400130523           when  Ord_x_TpEl;
060500130523             SavC01tpEl = C01tpElab;
060600130523           when  Ord_x_dtIn;
060700130523             SavC01dtIn = C01datIns;
060800130523         endsl;
060900130523
061000130523         // -?Visualizzazione del SFL (se ci sono dati)?
061100130523         SflDsp_N = (S01nrr <= *zero);
061200130523
061300130523         // -?Attivazione del SFLEND?
061400130523         SflEnd = ( %eof(FIIRD11L) );
061500130523
061600130523         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
061700130523         if  S01nrr > *zero;
061800130523           C1RcdNbr = 1;
061900130523         else;
062000130523           clear  C1RcdNbr;
062100130523         endif;
062200130523
062300130523         C1CsrRrn = C1RcdNbr;
062400130523
062500130523       ENDSR;
062600130524
062700130524       //--------------------------------------------------------------
062800130524       //?Selezione del singolo record in base al posizionamento.      ?
062900130524       //--------------------------------------------------------------
063000130524       BEGSR  sr_SelezRec;
063100130524
063200130524         reset  $RecOK;
063300130524
063400130524         select;
063500130524
063600130524           // -?Richiesto posizionamento per Distinta?
063700130524           //  ?si scartano i record con Numero Distinta inferiore?
063800130524           when  Ord_x_Dist  and
063900130524                 IRDfgsNdc < %editc( C01fil : 'X' ) + %editc( C01ndc : 'X' );
064000130524             leavesr;
064100130524
064200130524           // -?Richiesto posizionamento per Tipo Elaborazione?
064300130524           //  ?si scartano i record con Tipo Elaborazione inferiore?
064400130524           when  Ord_x_TpEl  and
064500130524                 IRDtpElab < C01tpElab;
064600130524             leavesr;
064700130524
064800130524           // -?Richiesto posizionamento per Data Inserimento?
064900130524           //  ?si scartano i record con Data Inserimento inferiore?
065000130524           when  Ord_x_DtIn  and
065100130524                 IRDdatIns < w01datIns;
065200130524             leavesr;
065300130524
065400130524         endsl;
065500130524
065600130524         $RecOK = *on;
065700130524
065800130524       ENDSR;
065900130523
066000130523       //--------------------------------------------------------------
066100130523       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
066200130523       //?F8-Cambia ordinamento sfl                                    ?
066300130523       //--------------------------------------------------------------
066400130523       BEGSR  sr_F08S01;
066500130523
066600130523         Select;
066700130523           when  Ord_x_Dist;
066800130523             Ord_x_Dist = *off;
066900130523             Ord_x_TpEl = *on;
067000130523             Ord_x_DtIn = *off;
067100130523           when  Ord_x_TpEl;
067200130523             Ord_x_Dist = *off;
067300130523             Ord_x_TpEl = *off;
067400130523             Ord_x_DtIn = *on;
067500130523           when  Ord_x_dtIn;
067600130523             Ord_x_Dist = *on;
067700130523             Ord_x_TpEl = *off;
067800130523             Ord_x_DtIn = *off;
067900130523         endsl;
068000130523
068100130523         select;
068200130523           // -?Subfile vuoto: nessun record da ordinare?
068300130524           when  Not $Almeno1rec;
068400130523           // -?Subfile già posizionato: occorre ricaricarlo?
068500130523           when  (C01fil   <> *zero   or  C01ndc <> *zero)  or
068600130523                 C01tpElab <> *blank  or
068700130523                 C01datIns <> *zero;
068800130524             clear  C01fil;
068900130524             clear  C01ndc;
069000130524             clear  C01tpElab;
069100130524             clear  C01datIns;
069200130524             reset  SavC01fil;
069300130524             reset  SavC01ndc;
069400130524             reset  SavC01tpEl;
069500130524             reset  SavC01dtIn;
069600130523             $InzS01 = *on;
069700130523           // -?Altrimenti: basta riordinarlo?
069800130523           other;
069900130523             exsr  sr_SortSfl;
070000130523         endsl;
070100130524
070200130524         clear  C01fil;
070300130524         clear  C01ndc;
070400130524         clear  C01tpElab;
070500130524         clear  C01datIns;
070600130524         clear  SavC01fil;
070700130524         clear  SavC01ndc;
070800130524         clear  SavC01tpEl;
070900130524         clear  SavC01dtIn;
071000130523
071100130523       ENDSR;
071200130523
071300130523       //--------------------------------------------------------------
071400130523       //?Ordinamento subfile                                          ?
071500130523       //?  This subroutine sorts the subfile records.                 ?
071600130523       //--------------------------------------------------------------
071700130523       BEGSR  sr_SortSfl;
071800130523
071900130527         // -?Impostazione NRR dell'ultimo record del subfile          ?
072000130527         //  ?(già fatto in sr_InzS01:                                 ?
072100130527         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
072200130527         //RrnLast  = S01nrr;
072300130523
072400130523         C1RcdNbr = 1;
072500130523         clear  C1CsrRrn;
072600130523         clear  S01nrr;
072700130523         clear  V1Dmsg;
072800130524         SflNxtChg   = *off;
072900130523         ErrMessage  = *off;
073000130524         ErrGenerico = *off;
073100130523         %subst(IndDspF : 50) = *off;
073200130523
073300130523         //?___________________________________________________________?
073400130523         //?Initialize the key fields to sort on.?
073500130523         //?There is one extra field not in the subfile -- Selected --?
073600130523         //?that is added to the record. This field is used to place?
073700130523         //?selected records in front of nonselected records.?
073800130523         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
073900130523
074000130523         clear  QLGKL;
074100130523         clear  QLGSKL;
074200130523         clear  QLGSCB;
074300130523         clear  QLGSCB00;
074400130523
074500130523         // -?Gestione della scelta ordinamento:?
074600130523         Select;
074700130523           // -?Ordinamento per Numero Distinta?
074800130523           When  Ord_x_Dist;
074900130523             // -?1 campi chiave?
075000130523             QLGNBRK  = 1;
075100130523             // -?1° campo: cod. filiale + num. distinta (S1HfgsNdc)?
075200130523             //            ?a posizone    1,  9 byte, char, ascending.?
075300130523             QLGSP    = 1;
075400130523             QLGSS    = %size(S1HfgsNdc);
075500130523             QLGDT    = Carattere;
075600130523             QLGSO    = Ascendente;
075700130523             QLGKL(1) = QLGSKL;
075800130523
075900130523           // -?Ordinamento per tipo elaborazione?
076000130523           When  Ord_x_TpEl;
076100130523             // -?2 campi chiave?
076200130524             QLGNBRK  = 2;
076300130523             // -?1° campo: tipo elaborazione (S01tpelab)?
076400130524             //            ?a posizone   27,  1 byte, char, ascending.?
076500130524             QLGSP    = 27;
076600130523             QLGSS    = %size(S01tpElab);
076700130523             QLGDT    = Carattere;
076800130523             QLGSO    = Ascendente;
076900130524             QLGKL(1) = QLGSKL;
077000130523             // -?2° campo: cod. filiale + num. distinta (S1HfgsNdc)?
077100130523             //            ?a posizone    1,  9 byte, char, ascending.?
077200130523             QLGSP    = 1;
077300130523             QLGSS    = %size(S1HfgsNdc);
077400130523             QLGDT    = Carattere;
077500130523             QLGSO    = Ascendente;
077600130523             QLGKL(2) = QLGSKL;
077700130523
077800130523           // -?Ordinamento per data inserimento?
077900130523           When  Ord_x_dtIn;
078000130523             // -?2 campi chiave?
078100130523             QLGNBRK  = 2;
078200130523             // -?1° campo: data inserimento (S1HdatIns)?
078300130523             //            ?a posizone   10,  8 byte, int,  ascending.?
078400130523             QLGSP    = 10;
078500130523             QLGSS    = %size(S1HdatIns);
078600130523             QLGDT    = Numero;
078700130523             QLGSO    = Ascendente;
078800130523             QLGKL(1) = QLGSKL;
078900130523             // -?2° campo: cod. filiale + num. distinta (S1HfgsNdc)?
079000130523             //            ?a posizone    1,  9 byte, char, ascending.?
079100130523             QLGSP    = 1;
079200130523             QLGSS    = %size(S1HfgsNdc);
079300130523             QLGDT    = Carattere;
079400130523             QLGSO    = Ascendente;
079500130523             QLGKL(2) = QLGSKL;
079600130523
079700130523         EndSl;
079800130523
079900130523         // -?Load other sort parameters.?
080000130523         QLGLB   = 80 + 16 * MaxKey;
080100130523         QLGRL   = %size(SflRcd) - 1;
080200130523         QLGRT   = 8;
080300130523         QLGOKL  = 80;
080400130523         QLGLKE  = 16;
080500130523         QLGLSS  = 290;
080600130523         // -?Initialize Sort I/O API fields.?
080700130523         QLGRL00 = QLGRL;
080800130523         QLGRC00 = 1;
080900130523         clear  QUSEI;
081000130523         QUSBPRV = %size(QUSEC);
081100130523
081200130523         // -?First step - Initialize the sort routine.?
081300130523         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
081400130523                      SizeList : ReturnSize : QUSEC );
081500130523
081600130523         // -?Next step - Write records to I/O routine.?
081700130523         QLGRT00  = Put;
081800130523         For  Nrr = 1  To  RrnLast;
081900130523           chain  Nrr  LR67S01;
082000130523           // -?Solo le righe con Selected = 'Y' sono riordinate,?
082100130523           //  ?quindi per fare un ordinamento di tutte le righe?
082200130523           //  ?metto 'Y' sempre.?
082300130523           Selected = 'Y';
082400130523           clear  QUSEI;
082500130523           QUSBPRV  = %size(QUSEC);
082600130523           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
082700130523                         SizeList : NotUsed : QUSEC );
082800130523         EndFor;
082900130523
083000130523         // -?Next step - Signal end of input, clear subfile for reload.?
083100130523         QLGRT00 = EndPut;
083200130523         clear  QUSEI;
083300130523         QUSBPRV = %size(QUSEC);
083400130523         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
083500130523                       SizeList : NotUsed : QUSEC );
083600130523
083700130523         // -?Pulizia subfile?
083800130523         SflDsp_N    = *on;
083900130523         SflDspCtl_N = *on;
084000130523         write  LR67C01;
084100130523         SflDspCtl_N = *off;
084200130523         SflEnd      = *off;
084300130523
084400130523         // -?Final step - Write the records back to the subfile.?
084500130523         QLGRT00  = Get;
084600130523         For  Nrr = 1  To  RrnLast;
084700130523           clear  QUSEI;
084800130523           QUSBPRV = %size(QUSEC);
084900130523           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
085000130523                         QLGRL00  : NotUsed : QUSEC );
085100130523           // -?Ripristino indicatori utilizzati nel sfl rec?
085200130524           SflNxtChg = (S01opz <> *zero);
085300130523           S01nrr = Nrr;
085400130524           write  LR67S01;
085500130523         EndFor;
085600130523
085700130523         C1CsrRrn = 1;
085800130523
085900130523         // -?Visualizzazione del SFL (se ci sono dati)?
086000130523         SflDsp_N = (S01nrr <= *zero);
086100130523
086200130523         // -?Attivazione del SFLEND?
086300130523         SflEnd = %eof(FIIRD11L);
086400130524
086500130524         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
086600130524         Select;
086700130524           When  Ord_x_Dist;
086800130524             P01f08 = 'F8=Ord. x Tipo Elab.   ';
086900130524           When  Ord_x_TpEl;
087000130524             P01f08 = 'F8=Ord. x Data Inserim.';
087100130524           When  Ord_x_DtIn;
087200130524             P01f08 = 'F8=Ord. x Distinta     ';
087300130524         EndSl;
087400130523
087500130523       ENDSR;
087600130523
087700130523       //--------------------------------------------------------------
087800130523       //?Caricamento completo del subfile S01                         ?
087900130523       //--------------------------------------------------------------
088000130523       BEGSR  sr_OpzS01;
088100130523
088200130523         // -?Spegnimento degli indicatori di errore?
088300130523         %subst(IndDspF : 50) = *off;
088400130524
088500130524         clear  SavS01csr;
088600130523
088700130523         // -?Ciclo di lettura subfile?
088800130523         readc  LR67S01;
088900130523
089000130523         DoW  Not %eof(FNLR67D);
089100130523
089200130523           SflNxtChg = *off;
089300130523           %subst(IndDspF : 50) = *off;
089400130523           C1RcdNbr = S01nrr;
089500130523
089600130523           select;
089700130523
089800130523             // -?Opz. 2=Modifica?
089900130523             //  ?Opz. 4=Cancellazione?
090000130523             //  ?Opz. 5=Visualizzazione?
090100130523             when  S01opz = 2  or  S01opz = 4  or  S01opz = 5;
090200130523               $Video  = 'D1';
090300130523               $InzD01 = *on;
090400130523               DoW  $Video = 'D1'  and  Not $Fine;
090500130523                 exsr  sr_GesD01;
090600130523               EndDo;
090700130523
090800130523             // -?Nessuna opzione?
090900130524             when  S01opz  = *zero;
091000130523
091100130523             // -?Opzione errata?
091200130523             other;
091300130524               V1Dmsg = $Msg(01);
091400130524               PosCurOPZ   = *on;
091500130524               ErrMessage  = *on;
091600130523               ErrGenerico = *on;
091700130523               $Fine  = *off;
091800130523
091900130523           endsl;
092000130523
092100130523           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
092200130523           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
092300130524           if  S01opz  <> *zero   and
092400130523              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
092500130523             SavS01csr   = C1RcdNbr;
092600130523           endif;
092700130523
092800130523           // -?Aggiornamento sfl?
092900130523           select;
093000130523             when  ErrMessage;
093100130523               SflNxtChg = *on;
093200130523               C1CsrRrn  = C1RcdNbr;
093300130523             when  ErrGenerico;
093400130523               C1CsrRrn  = C1RcdNbr;
093500130523               clear  S01opz;
093600130523             other;
093700130523               clear  S01opz;
093800130523           endsl;
093900130523
094000130523           UPDATE  LR67S01;
094100130523
094200130523           if  ErrGenerico   or   ErrMessage;
094300130523             leave;
094400130523           endif;
094500130523
094600130523           readc  LR67S01;
094700130523
094800130523         ENDDO;
094900130523
095000130523         // -?Riposizionam. cursore sul record contenente la prima opz.?
095100130523         //  ?(se non sono stati rilevati errori)?
095200130523         if  NOT ErrMessage   and   NOT ErrGenerico   and
095300130523             SavS01csr > *zero;
095400130523           C1CsrRrn = SavS01csr;
095500130523         endif;
095600130523
095700130523       ENDSR;
095800130523
095900130523       //--------------------------------------------------------------
096000130523       //?Gestione videata D01.                                        ?
096100130523       //--------------------------------------------------------------
096200130523       BEGSR  sr_GesD01;
096300130523
096400130523         // -?Inizializzazione videata?
096500130523         if  $InzD01 = *on;
096600130523           exsr  sr_InzD01;
096700130523           $InzD01 = *off;
096800130523         endif;
096900130523
097000130523         // -?Emissione Testata?
097100130523         write  LR67T01;
097200130523
097300130523         // -?Emissione videata?
097400130524         if  $Immissione  or  S01opz = 2;
097500130524           exfmt  LR67D01;
097600130524         else;
097700130524           write  LR67D01;
097800130524           exfmt  Protect;
097900130524         endif;
098000130523
098100130523         clear  V1Dmsg;
098200130524         reset  ErrMessage;
098300130524         reset  ErrGenerico;
098400130523
098500130524         Select;
098600130523
098700130523           // -?F3=Fine?
098800130524           When  dsp_aid = c_F03;
098900130523             $Fine = *on;
099000130527
099100130527           // -?F9=Dettaglio (spedizioni in distinta)?
099200130527           When  dsp_aid = c_F09;
099300130527             exsr  sr_CallFIDG311;
099400130523
099500130523           // -?F12=Ritorno?
099600130524           When  dsp_aid = c_F12;
099700130527             if  S01opz = 2;
099800130527               unlock  FIIRD11L;
099900130527             endif;
100000130523             $Video = 'S1';
100100130523
100200130524           // -?Invio o F6=Conferma?
100300130524           Other;
100400130524             if  $Immissione  or  S01opz = 2;
100500130524               exsr  sr_CtrD01;
100600130708             endif;
100700130708             if  not  ErrGenerico  and  dsp_aid = c_F06;
100800130708               exsr  sr_Upd_FIIRD;
100900130708               $Video = 'S1';
101000130708               reset  $InzS01;
101100130708             endif;
101200130523
101300130524         EndSl;
101400130523
101500130523       ENDSR;
101600130523
101700130523       //--------------------------------------------------------------
101800130523       //?Inizializzazione videata D01.                                ?
101900130523       //--------------------------------------------------------------
102000130523       BEGSR  sr_InzD01;
102100130523
102200130523         // -?Pulizia videata?
102300130523         clear  LR67D01;
102400130524
102500130524         In_Sede = (DUTlpo = 'S');
102600130524
102700130524         select;
102800130524           when  $Immissione;
102900130524             D01opz = '  IMMISSIONE   ';
103000130524           when  S01opz = 2;
103100130524             D01opz = '   MODIFICA    ';
103200130524           when  S01opz = 4;
103300130524             D01opz = ' CANCELLAZIONE ';
103400130524           when  S01opz = 5;
103500130524             D01opz = 'VISUALIZZAZIONE';
103600130524         endsl;
103700130523
103800130524         // -?Impostazione campi a video?
103900130524         If  $Immissione;
104000130523
104100130524           if  Not In_Sede;
104200130524             D01fil  = DUTpou;
104300130524             D01ndc  = '?';
104400130524           endif;
104500130523           D01datIns = *date;
104600130523
104700130523         Else;
104800130523
104900130524           clear  k01fiird11;
105000130523           k_IRDfgsNdc = S1Hfgsndc;
105100130524           if  S01opz  = 5;
105200130524             chain(N)  %kds( k01fiird11 )  FIIRD000;
105300130524           else;
105400130524             chain  %kds( k01fiird11 )  FIIRD000;
105500130524           endif;
105600130523
105700130524           If  %found(FIIRD11L);
105800130523
105900130523             D01fil    = %int( %subst( IRDfgsndc : 1 : %len(D01fil) ) );
106000130524             D01ndc    = %subst( IRDfgsndc : %len(D01fil) + 1 );
106100130523             D01tpElab = IRDtpElab;
106200130523             if  IRDdatIns > *zero;
106300130523               wDate_Eur = %date( IRDdatIns : *iso );
106400130524               D01DatIns = %dec( wDate_Eur );
106500130523             endif;
106600130523
106700130523             // ·?Decodifiche?
106800130523             exsr  sr_CtrD01;
106900130523             clear  V1Dmsg;
107000130524             reset  ErrMessage;
107100130524             reset  ErrGenerico;
107200130523
107300130523           Else;
107400130523
107500130523             D01fil = S01fil;
107600130524             D01ndc = %editc( S01ndc : 'X' );
107700130523
107800130523           EndIf;
107900130523
108000130523         EndIf;
108100130524
108200130524         // -?Impostazione indicatori per attributi di visualizzazione?
108300130524         Protect_Key = (Not  $Immissione);
108400130524
108500130524         // -?(Dis)Abilitazione tasti funzionali?
108600130524         F6Attivo = ($Immissione  or  S01opz <> 5);
108700130527         F9Attivo = (Not  In_Sede);
108800130523
108900130523       ENDSR;
109000130523
109100130523       //--------------------------------------------------------------
109200130523       //?Controllo dati immessi nella videata D01.                    ?
109300130523       //--------------------------------------------------------------
109400130523       BEGSR  sr_CtrD01;
109500130523
109600130523         // -?Spegnimento degli indicatori di errore?
109700130523         %subst(IndDspF : 50) = *off;
109800130523
109900130523         // -?Controllo Filiale?
110000130523         clear  D01filD;
110100130523
110200130523         // ·?Obbligatoria?
110300130523         if  D01fil = *zero;
110400130523           V1Dmsg = $Msg(02);
110500130523           PosCurFIL   = *on;
110600130523           ErrMessage  = *on;
110700130523           ErrGenerico = *on;
110800130523           leavesr;
110900130523         endif;
111000130523
111100130523         // ·?Errata?
111200130523         k_ORGfil = D01fil;
111300130523         chain  %kds( k01azorg01 )  AZORG;
111400130523         if  %found(AZORG01L);
111500130523           D01filD = ORGdes;
111600130523           Og148   = ORGde8;
111700130523         endif;
111800130523         if  Og148.§ogLPO = *blank;
111900130523           V1Dmsg = $Msg(03);
112000130523           PosCurFIL   = *on;
112100130523           ErrMessage  = *on;
112200130523           ErrGenerico = *on;
112300130523           leavesr;
112400130523         endif;
112500130523
112600130523         // -?Controllo Distinta?
112700130527         exsr  sr_CtrlNDC;
112800130524
112900130524         // -?Controllo se chiave duplicata in inserimento?
113000130524         if  $Immissione;
113100130524           clear  k01fiird11;
113200130524           k_IRDfgsNdc = %editc( D01fil : 'X' ) + w01ndcA;
113300130524           setll  %kds( k01fiird11 )  FIIRD000;
113400130524           if  %equal(FIIRD11L);
113500130524             V1Dmsg = $Msg(08);
113600130524             PosCurNDC   = *on;
113700130524             ErrMessage  = *on;
113800130524             ErrGenerico = *on;
113900130524             leavesr;
114000130524           endif;
114100130524         endif;
114200130523
114300130523         // -?Controllo Tipo Elaborazione?
114400130523         if  D01tpElab <> *blank  and  D01tpElab <> 'F';
114500130524           V1Dmsg = $Msg(09);
114600130523           PosCurTPEL  = *on;
114700130523           ErrMessage  = *on;
114800130523           ErrGenerico = *on;
114900130523           leavesr;
115000130523         endif;
115100130523
115200130523         // -?Data Inserimento obbligatoria?
115300130523         if  D01datIns = *zero;
115400130524           V1Dmsg = $Msg(10);
115500130523           PosCurDTIN  = *on;
115600130523           ErrMessage  = *on;
115700130523           ErrGenerico = *on;
115800130523           leavesr;
115900130523         endif;
116000130523
116100130523         // -?Data Inserimento formalmente errata?
116200130524         //clear  wDate_Iso;
116300130524         //Monitor;
116400130524         //  wDate_Iso = %date( D01datIns : *eur );
116500130524         //On-Error;
116600130524         //  V1Dmsg = $Msg(11);
116700130524         //  PosCurDTIN  = *on;
116800130524         //  ErrMessage  = *on;
116900130524         //  ErrGenerico = *on;
117000130524         //  leavesr;
117100130524         //EndMon;
117200130524         clear WLBdat;
117300130524         G08dat = D01datIns;
117400130524         XSRDA8(WLBdat);
117500130524         if  G08err = *on;
117600130524           V1Dmsg = $Msg(11);
117700130524           PosCurDTIN  = *on;
117800130524           ErrMessage  = *on;
117900130524           ErrGenerico = *on;
118000130524           leavesr;
118100130524         else;
118200130524           D01datIns = G08dat;
118300130524         endif;
118400130523
118500130523       ENDSR;
118600130527
118700130527       //--------------------------------------------------------------
118800130527       //?Controllo Numero Distinta Consegna                           ?
118900130527       //--------------------------------------------------------------
119000130527       BEGSR  sr_CtrlNDC;
119100130527
119200130527         clear  D01ndcD;
119300130527
119400130527         // -?Ricerca?
119500130527         if  Not In_Sede  and  %scan( '?' : D01ndc ) > *zero;
119600130527           reset  dsLR01;
119700130527           DLRpgm = SDSpgm;
119800130527           DLRfgs = D01fil;
119900130527           DLRgal = *date;
120000130527           KPJBU  = dsLR01;
120100130527           fnlr02r ( KPJBA );
120200130527           dsLR01 = KPJBU;
120300130527           evalr D01ndc = %editc( DLRnfv : 'Z' );
120400130527           leavesr;
120500130527         endif;
120600130527
120700130527         // -?Sostituzione *blank con *zero per ctrl numericità?
120800130527         w01ndcA = %xlate( ' ' : '0' : D01ndc : 1 );
120900130527         if  %check( c_Digits : w01ndcA ) > *zero;
121000130527           V1Dmsg = $Msg(04);
121100130527           PosCurNDC   = *on;
121200130527           ErrMessage  = *on;
121300130527           ErrGenerico = *on;
121400130527           leavesr;
121500130527         endif;
121600130527         w01ndcN = %int( w01ndcA );
121700130527         D01ndc  = %editc( w01ndcN : 'Z' );
121800130527
121900130527         // -?Non inserito?
122000130527         if  w01ndcN = *zero;
122100130527           V1Dmsg = $Msg(05);
122200130527           PosCurNDC   = *on;
122300130527           ErrMessage  = *on;
122400130527           ErrGenerico = *on;
122500130527           leavesr;
122600130527         endif;
122700130527
122800130527         // -?Ctrl Validità:?
122900130527         clear  k03fidst01;
123000130527         k_DSTnpg = 4;
123100130527         k_DSTnfv = w01ndcN;
123200130527         k_DSTfgs = D01fil;
123300130527         chain  %kds( k03fidst01 )  FIDST000;
123400130527         Select;
123500130527           // -?Distinta errata?
123600130527           When  NOT  %found(FIDST01L)  or  DSTatb <> *blank;
123700130527             V1Dmsg = $Msg(06);
123800130527             PosCurNDC   = *on;
123900130527             ErrMessage  = *on;
124000130527             ErrGenerico = *on;
124100130527             leavesr;
124200130527           // -?Distinta non chiusa?
124300130527           When  DSTfcf <> 'S';
124400130527             V1Dmsg = $Msg(07);
124500130527             PosCurNDC   = *on;
124600130527             ErrMessage  = *on;
124700130527             ErrGenerico = *on;
124800130527             leavesr;
124900130527           Other;
125000130527             clear  k02fiapd01;
125100130527             k_APDtip = DSTtipA;
125200130527             k_APDpdr = DSTpdr;
125300130527             chain  %kds( k02fiapd01 )  FIAPD000;
125400130527             if  %found(FIAPD01L);
125500130527               D01ndcD = APDrsc;
125600130527             endif;
125700130527         EndSl;
125800130527
125900130527       ENDSR;
126000130527
126100130527       //--------------------------------------------------------------
126200130527       //?Richiamo *pgm Interrogazione Distinte                        ?
126300130527       //--------------------------------------------------------------
126400130527       BEGSR  sr_CallFIDG311;
126500130527
126600130527         // -?Ctrl Numero Distinta Consegna?
126700130527         exsr  sr_CtrlNDC;
126800130527         if  ErrMessage;
126900130527           leavesr;
127000130527         endif;
127100130527
127200130527         // -?Interrogazine Distinta?
127300130527         reset  FIDG311ds;
127400130527         F31fgs = DSTfgs;
127500130527         //F31npg = 4;        ?(già così)?
127600130527         F31numDis  = DSTnfv;
127700130527         F31datDis  = DSTdfv;
127800130527         //F31priDati = 'C';  ?(default)?
127900130527         F31DatiVis = DSTpda;
128000130527         KPJBU = FIDG311ds;
128100130527         fidg311r ( kpjba );
128200130527         FIDG311ds  = KPJBU;
128300130527         if  F31tFunz = '03';
128400130527           $Fine = *on;
128500130527           leavesr;
128600130527         endif;
128700130527
128800130527       ENDSR;
128900130523
129000130523       //--------------------------------------------------------------
129100130523       //?Aggiornamento/Cancellazione singola distinta.                ?
129200130523       //--------------------------------------------------------------
129300130523       BEGSR  sr_Upd_FIIRD;
129400130523
129500130523         // -?Aggancio record da aggiornare/inserire/annullare?
129600130524         clear  k01fiird11;
129700130524         k_IRDfgsNdc = %editc( D01fil : 'X' ) + w01ndcA;
129800130524         chain  %kds( k01fiird11 )  FIIRD000;
129900130523
130000130523         // -?Impostazione dei campi chiave  SE immissione?
130100130524         if  NOT  %found(FIIRD11L);
130200130524           clear  FIIRD000;
130300130524           IRDfgsNdc = k_IRDfgsNdc;
130400130523         endif;
130500130523
130600130523         // -?Impostazione dati (se NON annullamento)?
130700130524         If  S01opz <> 4;
130800130524
130900130524           IRDtpElab = D01tpElab;
131000130524           //IRDdatIns = %dec( wDate_Iso );
131100130524           IRDdatIns = G08inv;
131200130524
131300130524         EndIf;
131400130523
131500130524         Select;
131600130524
131700130524           // -?Cancellazione (opz. 4)?
131800130524           When  Not $Immissione  and  S01opz = 4;
131900130524             //_______________
132000130524             DELETE  FIIRD000;
132100130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
132200130524
132300130524           // -?Modifica (opz. 2)?
132400130524           When  %found(FIIRD11L);
132500130524             //_______________
132600130524             UPDATE  FIIRD000;
132700130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
132800130524
132900130524           // -?Immissione (F6)?
133000130524           Other;
133100130524             //_________________
133200130524             WRITE(e)  FIIRD000;
133300130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
133400130524             if  %error;
133500130524               V1Dmsg = %trimr($Msg(12));
133600130524               PosCurFIL   = *on;
133700130524               ErrMessage  = *on;
133800130524               ErrGenerico = *on;
133900130524               leavesr;
134000130524             endif;
134100130524             $Almeno1rec = *on;
134200130524
134300130524         EndSl;
134400130523
134500130523       ENDSR;
134600130523
134700130523       //--------------------------------------------------------------
134800130523       //?Operazioni finali.                                           ?
134900130523       //--------------------------------------------------------------
135000130523       BEGSR  sr_RoutEnd;
135100130523
135200130523         // -?Chiusura *pgm?
135300130523         return;
135400130523
135500130523       ENDSR;
135600130523
135700130523      /end-free
135800130523
135900130523** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
136000130523Opzione errata                                                                  1
136100130523Filiale obbligatoria                                                            2
136200130523Filiale errata                                                                  3
136300130524Numero distinta errato: contiene caratteri NON numerici                         4
136400130524Numero distinta obbligatorio                                                    5
136500130524Numero distinta inesistente o annullato                                         6
136600130524Distinta NON ancora chiusa                                                      7
136700130524Distinta già inserita                                                           8
136800130524Tipo Elaborazione errato: " " = Controllo immagine su TITAS, "F" = Forzatura    9
136900130524Data Inserimento obbligatoria                                                  10
137000130524Data Inserimento formalmente errata                                            11
137100130524Rilevato errore in fase di registrazione distinta                              12
