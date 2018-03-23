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
015600140107     d $Forza          s               n   inz
015700130523     d $Immissione     s               n   inz
015800130523     d $InzD01         s               n   inz(*on)
015900130523     d $InzS01         s               n   inz(*on)
016000130523
016100130523       // -?Variabili per la gestione del video?
016200130523     d $Video          s              2    inz('S1')
016300130523     d S01nrr          s                   like(C1RcdNbr)  inz
016400130523
016500130523       // -?Variabili per la gestione del posizionamento nel subfile?
016600130524     d SavS01csr       s                   like(C1RcdNbr)  inz
016700130523     d SavC01fil       s                   like(C01fil)    inz(*hival)
016800130523     d SavC01ndc       s                   like(C01ndc)    inz(*hival)
016900130523     d SavC01tpEl      s                   like(C01tpElab) inz(*hival)
017000130523     d SavC01dtIn      s                   like(C01datIns) inz(*hival)
017100140107     d SaveFIL         s                   like(D01fil)    inz
017200140107     d SaveNDC         s                   like(D01ndc)    inz
017300130524
017400130524       // -?Nome esteso Libreria/File Distinte Consegna?
017500130524     d wLibFIDST       s             21a   inz
017600130523
017700130523       // -?Variabili di comodo?
017800130524     d wDate_EUR       s               d   datfmt(*eur)    inz(*loval)
017900130524     d wDate_ISO       s               d   datfmt(*iso)    inz(*loval)
018000130524     d w01ndcA         s                   like(D01ndc)    inz
018100130524     d w01ndcN         s                   like(C01ndc)    inz
018200130524     d w01datIns       s                   like(IRDdatIns) inz
018300130523
018400130523       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018500130523
018600130523       //--------------------------------------------------------------
018700130523       // -?Constants?
018800130523       //--------------------------------------------------------------
018900130523       // -?MaxKey - Maximum number of key fields allowed by this program?
019000130523     d MaxKey          c                   const(2)
019100130523       // -?Sort order:?
019200130523       //  ?1 = Sort in an ascending sequence?
019300130523       //  ?2 = Sort in a descending sequence?
019400130523     d Ascendente      c                   const(1)
019500130523     d Discendente     c                   const(2)
019600130523       // -?Key data type:?
019700130523       //  ? 0 = Signed binary?
019800130523       //  ? 2 = Signed zoned decimal?
019900130523       //  ? 3 = Signed packed decimal?
020000130523       //  ? 6 = Character with no national language sort sequence applied,?
020100130523       //  ?     if specified?
020200130523       //  ? 7 = Unsigned packed decimal?
020300130523       //  ?     All numbers will have the sign forced positive ('F'X).?
020400130523       //  ? 8 = Unsigned zoned decimal?
020500130523       //  ?     All numbers will have the sign forced positive ('F'X).?
020600130523       //  ? 9 = Unsigned binary?
020700130523       //  ?14 = Date in form of DD/MM/YY?
020800130523       //  ?15 = Date in form of DD.MM.YYYY?
020900130523     d Numero          c                   const(2)
021000130523     d Carattere       c                   const(6)
021100130523       //
021200130523     d Put             c                   const(1)
021300130523     d EndPut          c                   const(2)
021400130523     d Get             c                   const(3)
021500130523
021600130523       //--------------------------------------------------------------
021700130523       // -?Data Structures?
021800130523       //  ?SflRcd     - The entire subfile record to pass to the sort?
021900130523       //  ?QLGSCB     - The sort request block for the QLGSORT API?
022000130523       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
022100130523       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
022200130523       //  ?QUSEC      - Error structure for the QLGSORT API?
022300130523       //--------------------------------------------------------------
022400130523     d SflRcd          ds                  inz
022500130524     d   S1HfgsNdc                         inz                                   9 bytes
022600130524     d   S1HdatIns                         inz                                   8 bytes
022700130524     d   S01fil                            inz                                   3 bytes
022800130524     d   S01ndc                            inz                                   6 bytes
022900130524     d   S01tpElab                         inz                                   1 byte
023000130524     d   S01datIns                         inz                                   8 bytes
023100130524     d   S01opz                            inz                                   2 bytes
023200130524     d   Selected                     1a   inz                                   1 byte
023300130523      /copy QSYSINC/QRPGLESRC,QLGSORT
023400130523     d QLGKL                         16    dim(MaxKey)
023500130523     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023600130523     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023700130523     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023800130523     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023900130523      /copy QSYSINC/QRPGLESRC,QLGSRTIO
024000130523      /copy QSYSINC/QRPGLESRC,QUSEC
024100130523
024200130523       //--------------------------------------------------------------
024300130523       // -?Standalone fields?
024400130523       //  ?Nrr        - Relative record number for subfile?
024500130523       //  ?SizeList   - Size of list?
024600130523       //  ?ReturnSize - Size of list returned by sort API?
024700130523       //--------------------------------------------------------------
024800130523     d NotUsed         s             16a   inz
024900130523     d ReturnSize      s              9b 0 inz
025000130523     d SizeList        s              9b 0 inz
025100130523     d RrnLast         s              5i 0 inz
025200130523     d Nrr             s              5i 0 inz
025300130523
025400130523       //--------------------------------------------------------------
025500130523       //?Definizione prototipi procedure.                             ?
025600130523       //--------------------------------------------------------------
025700130523
025800130523       // -?Reperimento dati utente?
025900130523     d TIBS34ds      e ds                  inz
026000130523      /copy gaitrasrc/srcProtoPR,TIBS34R
026100130523
026200130523       // -?Interrogazione Fogli Vari (*pgm FNLR02R)?
026300130523     d dsLR01        e ds                  inz
026400130523     d   DLRnpg      e                     inz(4)
026500130523     d   DLRric      e                     inz('S')
026600130524     d   DLRtfv      e                     inz('1')
026700130527      *
026800130523     d fnlr02r         pr                  extpgm('FNLR02R')
026900130523     d   kpjba                             likeds(KPJBA)
027000130527
027100130527       // -?Interrogazione Spedizioni in Distinta?
027200130527     d FIDG311ds     e ds                  inz
027300130527     d   F31npg      e                     inz(4)
027400130527      *
027500130527      /copy gaitrasrc/srcProtoPR,FIDG311R
027600130524
027700130524       // -?Controllo/Inversione data?
027800130524     d WLBdat          ds                  inz
027900130524     d  G08dat                        8  0 inz
028000130524     d  G08inv                        8  0 inz
028100130524     d  G08err                        1    inz('3')
028200130524     d  G08tgi                        5  0 inz
028300130524      /copy gaitrasrc/srcProtoPR,XSRDA8
028400130523
028500130523       // -?Ordinamento subfile?
028600130523      /copy gaitrasrc/srcProtoPr,QLGSRTIO
028700130523
028800130523       //--------------------------------------------------------------
028900130523       //?Definizione key-list.                                        ?
029000130523       //--------------------------------------------------------------
029100130523
029200130523       // -?File AZORG01L?
029300130523     d k01azorg01    e ds                  extname(AZORG01L : *key)
029400130523     d                                     prefix(k_)   inz
029500130523
029600130523       // -?File FIDST01L?
029700130523     d k03fidst01    e ds                  extname(FIDST01L : *key)
029800130523     d                                     prefix(k_)   inz
029900130523
030000130523       // -?File FIDST01L?
030100130523     d k02fiapd01    e ds                  extname(FIAPD01L : *key)
030200130523     d                                     prefix(k_)   inz
030300130523
030400130523       // -?File FIIRD11L?
030500130523     d k01fiird11    e ds                  extname(FIIRD11L : *key)
030600130523     d                                     prefix(k_)   inz
030700130523
030800130523       //--------------------------------------------------------------
030900130523       //?Riepilogo indicatori utilizzati.                             ?
031000130523       //--------------------------------------------------------------
031100130523
031200130523
031300130523       //--------------------------------------------------------------
031400130523       //?M A I N - L I N E                                            ?
031500130523       //--------------------------------------------------------------
031600130523
031700130523     c     *Entry        plist
031800130523     c                   parm                    KPJBA
031900130523
032000130523      /free
032100130523
032200130523       // -?Operazioni iniziali?
032300130523       exsr  sr_RoutInz;
032400130523
032500130523       // -?Ciclo di gestione del file video?
032600130523       DoW  $Fine = *off;
032700130523
032800130523         select;
032900130523
033000130523           // -?Gestione videata S1 (subfile)?
033100130523           when  $Video = 'S1';
033200130523             exsr  sr_GesS01;
033300130523
033400130523           // -?Gestione videata D1 (gestione singola distinta)?
033500130523           when  $Video = 'D1';
033600130523             exsr  sr_GesD01;
033700130523
033800130523           // -?? ? ??
033900130523           other;
034000130523             $Fine = *on;
034100130523
034200130523         endsl;
034300130523
034400130523       EndDo;
034500130523
034600130523       // -?Operazioni finali?
034700130523       exsr  sr_RoutEnd;
034800130523
034900130523       //--------------------------------------------------------------
035000130523       //?Operazioni iniziali.                                         ?
035100130523       //--------------------------------------------------------------
035200130523       BEGSR  sr_RoutInz;
035300130523
035400130523         // -?Impostazione chiusura?
035500130523         *inLR = *on;
035600130523
035700130523         // -?Reperimento dati job?
035800130523         exsr  sr_DatiJob;
035900130523
036000130523         // -?Impostazione nome programma a video?
036100130523         V1Tpgm = SDSpgm;
036200130524
036300130524         // -?Apertura file Distinte Consegna?
036400130524         if  %subst( KNSIF : 7 : 1 ) = 'P';
036500130524           wLibFIDST = 'FILTRAPRD/FIDST01L';
036600130524         else;
036700130524           wLibFIDST = 'FILTRA201/FIDST01L';
036800130524         endif;
036900130524         open  FIDST01L;
037000130523
037100130523         // -?Impostazione videata iniziale?
037200130523         $Video = 'S1';
037300130523         reset  $InzS01;
037400130523         reset  $InzD01;
037500130523         Ord_x_Dist = *on;
037600130523         exsr  sr_InzS01;
037700130523         $InzS01 = *off;
037800130524         $Almeno1rec = (S01nrr > *zero);
037900130523
038000130523       ENDSR;
038100130523
038200130523       //--------------------------------------------------------------
038300130523       //?Reperimento Dati del job (Utente/Operativi).                 ?
038400130523       //--------------------------------------------------------------
038500130523       BEGSR  sr_DatiJob;
038600130523
038700130523         in(e) §AzUte;
038800130523         if NOT %error;
038900130523           in(e) §DatiUte;
039000130523         endif;
039100130523         if %error or RSut = *blank;
039200130523           tibs34r ( tibs34ds );
039300130523           in §AzUte;
039400130523           in §DatiUte;
039500130523         endif;
039600130523
039700130523       ENDSR;
039800130523
039900130523       //--------------------------------------------------------------
040000130523       //?Gestione subfile S01.                                        ?
040100130523       //--------------------------------------------------------------
040200130523       BEGSR  sr_GesS01;
040300130523
040400130523         // -?Inizializzazione videata?
040500130523         if  $InzS01 = *on;
040600130523           exsr  sr_InzS01;
040700130523           $InzS01 = *off;
040800130523         endif;
040900130523
041000130523         // -?Emissione Testata & Piede?
041100130523         write  LR67T01;
041200130523         write  LR67P01;
041300130523
041400130523         // -?Posizionamento cursore?
041500130523         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
041600130523         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
041700130523         //  ?si visualizza la pagina che vedeva l'utente quando ha?
041800130523         //  ?premuto l'ultimo tasto.?
041900130523         if  C1CsrRrn > *zero;
042000130523           C1RcdNbr = C1CsrRrn;
042100130523         else;
042200130523           C1RcdNbr = 1;
042300130524           write  LR67S00;          //?(no rec.)?
042400130523         endif;
042500130523
042600130523         // -?Emissione videata?
042700130524         exfmt  LR67C01;
042800130523
042900130524         clear  V1Dmsg;
043000130524         reset  ErrMessage;
043100130523         reset  ErrGenerico;
043200130524
043300130524         $Immissione = (dsp_aid = c_F06);
043400130524
043500130524         // -?Inversione Data Inserimento per posizionamento?
043600130524         if  Ord_x_DtIn  and  C01datIns  > *zero
043700130524                         and  C01datIns <> SavC01dtIn;
043800130524           clear WLBdat;
043900130524           G08dat = C01datIns;
044000130524           XSRDA8(WLBdat);
044100130524           if  G08err = *on;
044200130524             V1Dmsg = $Msg(11);
044300130524             PosCurC1pos = *on;
044400130524             ErrMessage  = *on;
044500130524             ErrGenerico = *on;
044600130524             leavesr;
044700130524           else;
044800130524             C01datIns = G08dat;
044900130524             w01datIns = G08inv;
045000130524           endif;
045100130524         endif;
045200130523
045300130523         Select;
045400130523
045500130523           // -?F3=Fine?
045600130523           When  dsp_aid = c_F03;
045700130523             $Fine = *on;
045800130523
045900130523           // -?F5=Rivisualizza?
046000130523           When  dsp_aid = c_F05;
046100130523             $InzS01 = *on;
046200130523
046300130523           // -?F6=Immissione?
046400130523           When  dsp_aid = c_F06;
046500130524             $Video  = 'D1';
046600130524             $InzD01 = *on;
046700130524             //DoW  $Video = 'D1'  and  Not $Fine;
046800130524             //  exsr  sr_GesD01;
046900130523             //EndDo;
047000130523
047100130523           // -?F8=Ordinamento?
047200130523           when  dsp_aid = c_F08;
047300130523             exsr  sr_F08S01;
047400130523
047500130523           // -?Invio?
047600130523           Other;
047700130523             // -?Gestione opzioni?
047800130523             if  S01nrr > *zero;
047900130523               exsr  sr_OpzS01;
048000130523               if  ErrGenerico;
048100130523                 leavesr;
048200130523               endif;
048300130523             endif;
048400130523             // -?Richiesto posizionamento?
048500130523             if  (Ord_x_Dist  and  (C01fil <> SavC01fil
048600130523                               or   C01ndc <> SavC01ndc))     OR
048700130523                 (Ord_x_TpEl  and   C01tpElab <> SavC01tpEl)  OR
048800130523                 (Ord_x_DtIn  and   C01datIns <> SavC01dtIn);
048900130523               $InzS01 = *on;
049000130523             endif;
049100130523
049200130523         endsl;
049300130523
049400130523       ENDSR;
049500130523
049600130523       //--------------------------------------------------------------
049700130523       //?Inizializzazione subfile S01.                                ?
049800130523       //--------------------------------------------------------------
049900130523       BEGSR  sr_InzS01;
050000130523
050100130523         // -?Spegnimento degli indicatori di errore?
050200130523         %subst(IndDspF : 50) = *off;
050300130523
050400130523         // -?Pulizia subfile?
050500130523         SflDsp_N    = *on;
050600130523         SflDspCtl_N = *on;
050700130523         write  LR67C01;
050800130523         SflDspCtl_N = *off;
050900130523         SflEnd      = *off;
051000130523
051100130523         clear  C1RcdNbr;
051200130523         clear  C1CsrRrn;
051300130523         clear  S01nrr;
051400130523         clear  V1Dmsg;
051500130523         ErrMessage  = *off;
051600130523         ErrGenerico = *off;
051700130523
051800130523         // -?Caricamento completo dei dati nel subfile?
051900130523         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
052000130523         //  ?l'eventuale ordinamento)?
052100130523         clear  k01fiird11;
052200130524         //setll  %kds( k01fiird11 )  FIIRD000;
052300130524         setll  (*loval)  FIIRD000;
052400130524         read(N)  FIIRD000;
052500130523         exsr  sr_CaricaS01;
052600130527
052700130527         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
052800130527         RrnLast  = S01nrr;
052900130523
053000130523         // -?Impaginazione subfile?
053100130523         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
053200130523         if S01nrr > *zero;
053300130523           C1RcdNbr  = 1;
053400130523           C1CsrRrn  = 1;
053500130523         else;
053600130523           clear C1RcdNbr;
053700130523         endif;
053800130523
053900130523         // -?Ordinamento subfile (se premuto F8)?
054000130523         if  Not  Ord_x_Dist  and  S01nrr > *zero;
054100130523           exsr  sr_SortSfl;
054200130523         endif;
054300130524
054400130524         // -?(Dis)Abilitazione tasti funzionali?
054500130524         F8Attivo = (S01nrr > *zero  or  $Almeno1rec = *on);
054600130524
054700130524         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
054800130524         Select;
054900130524           When  Not F8Attivo;
055000130524           When  Ord_x_Dist;
055100130524             P01f08 = 'F8=Ord. x Tipo Elab.   ';
055200130524           When  Ord_x_TpEl;
055300130524             P01f08 = 'F8=Ord. x Data Inserim.';
055400130524           When  Ord_x_DtIn;
055500130524             P01f08 = 'F8=Ord. x Distinta     ';
055600130524         EndSl;
055700130523
055800130523       ENDSR;
055900130523
056000130523       //--------------------------------------------------------------
056100130523       //?Caricamento completo del subfile S01                         ?
056200130523       //--------------------------------------------------------------
056300130523       BEGSR  sr_CaricaS01;
056400130523
056500130523         clear  S01nrr;
056600130523         SflNxtChg = *off;
056700130523
056800130523         // -?Ciclo di caricamento dati da file FIIRD10F?
056900130523         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
057000130523         //  ?l'eventuale ordinamento)?
057100130523         DoW  Not  %eof(FIIRD11L);
057200130524
057300130524           // -?Selezione singolo record in base al posizionamento?
057400130524           exsr  sr_SelezRec;
057500130524
057600130524           // -?Registrazione singolo record nel subfile?
057700130524           If  $RecOK;
057800130523
057900130524             // -?Impostazione campi nel record del subfile?
058000130524             clear  LR67S01;
058100130524
058200130524             S1HfgsNdc = IRDfgsNdc;
058300130524             S1HdatIns = IRDdatIns;
058400130524
058500130524             S01fil    = %int( %subst( IRDfgsNdc : 1 : 3 ) );
058600130524             S01ndc    = %int( %subst( IRDfgsNdc : 4 : 6 ) );
058700130524             S01tpElab = IRDtpElab;
058800130524             wDate_Eur = %date( IRDdatIns : *iso );
058900130524             S01datIns = %dec( wDate_Eur );
059000130524
059100130524             // -?Scrittura record nel subfile?
059200130524             S01nrr += 1;
059300130524             write  LR67S01;
059400130524
059500130524           EndIf;
059600130523
059700130523           // -?Lettura record successivo?
059800130524           read(N)  FIIRD000;
059900130523
060000130523         EndDo;
060100130523
060200130523         // -?Memorizzazione posizionamento effettuato?
060300130523         select;
060400130523           when  Ord_x_Dist;
060500130523             SavC01fil  = C01fil;
060600130523             SavC01ndc  = C01ndc;
060700130523           when  Ord_x_TpEl;
060800130523             SavC01tpEl = C01tpElab;
060900130523           when  Ord_x_dtIn;
061000130523             SavC01dtIn = C01datIns;
061100130523         endsl;
061200130523
061300130523         // -?Visualizzazione del SFL (se ci sono dati)?
061400130523         SflDsp_N = (S01nrr <= *zero);
061500130523
061600130523         // -?Attivazione del SFLEND?
061700130523         SflEnd = ( %eof(FIIRD11L) );
061800130523
061900130523         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
062000130523         if  S01nrr > *zero;
062100130523           C1RcdNbr = 1;
062200130523         else;
062300130523           clear  C1RcdNbr;
062400130523         endif;
062500130523
062600130523         C1CsrRrn = C1RcdNbr;
062700130523
062800130523       ENDSR;
062900130524
063000130524       //--------------------------------------------------------------
063100130524       //?Selezione del singolo record in base al posizionamento.      ?
063200130524       //--------------------------------------------------------------
063300130524       BEGSR  sr_SelezRec;
063400130524
063500130524         reset  $RecOK;
063600130524
063700130524         select;
063800130524
063900130524           // -?Richiesto posizionamento per Distinta?
064000130524           //  ?si scartano i record con Numero Distinta inferiore?
064100130524           when  Ord_x_Dist  and
064200130524                 IRDfgsNdc < %editc( C01fil : 'X' ) + %editc( C01ndc : 'X' );
064300130524             leavesr;
064400130524
064500130524           // -?Richiesto posizionamento per Tipo Elaborazione?
064600130524           //  ?si scartano i record con Tipo Elaborazione inferiore?
064700130524           when  Ord_x_TpEl  and
064800130524                 IRDtpElab < C01tpElab;
064900130524             leavesr;
065000130524
065100130524           // -?Richiesto posizionamento per Data Inserimento?
065200130524           //  ?si scartano i record con Data Inserimento inferiore?
065300130524           when  Ord_x_DtIn  and
065400130524                 IRDdatIns < w01datIns;
065500130524             leavesr;
065600130524
065700130524         endsl;
065800130524
065900130524         $RecOK = *on;
066000130524
066100130524       ENDSR;
066200130523
066300130523       //--------------------------------------------------------------
066400130523       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
066500130523       //?F8-Cambia ordinamento sfl                                    ?
066600130523       //--------------------------------------------------------------
066700130523       BEGSR  sr_F08S01;
066800130523
066900130523         Select;
067000130523           when  Ord_x_Dist;
067100130523             Ord_x_Dist = *off;
067200130523             Ord_x_TpEl = *on;
067300130523             Ord_x_DtIn = *off;
067400130523           when  Ord_x_TpEl;
067500130523             Ord_x_Dist = *off;
067600130523             Ord_x_TpEl = *off;
067700130523             Ord_x_DtIn = *on;
067800130523           when  Ord_x_dtIn;
067900130523             Ord_x_Dist = *on;
068000130523             Ord_x_TpEl = *off;
068100130523             Ord_x_DtIn = *off;
068200130523         endsl;
068300130523
068400130523         select;
068500130523           // -?Subfile vuoto: nessun record da ordinare?
068600130524           when  Not $Almeno1rec;
068700130523           // -?Subfile già posizionato: occorre ricaricarlo?
068800130523           when  (C01fil   <> *zero   or  C01ndc <> *zero)  or
068900130523                 C01tpElab <> *blank  or
069000130523                 C01datIns <> *zero;
069100130524             clear  C01fil;
069200130524             clear  C01ndc;
069300130524             clear  C01tpElab;
069400130524             clear  C01datIns;
069500130524             reset  SavC01fil;
069600130524             reset  SavC01ndc;
069700130524             reset  SavC01tpEl;
069800130524             reset  SavC01dtIn;
069900130523             $InzS01 = *on;
070000130523           // -?Altrimenti: basta riordinarlo?
070100130523           other;
070200130523             exsr  sr_SortSfl;
070300130523         endsl;
070400130524
070500130524         clear  C01fil;
070600130524         clear  C01ndc;
070700130524         clear  C01tpElab;
070800130524         clear  C01datIns;
070900130524         clear  SavC01fil;
071000130524         clear  SavC01ndc;
071100130524         clear  SavC01tpEl;
071200130524         clear  SavC01dtIn;
071300130523
071400130523       ENDSR;
071500130523
071600130523       //--------------------------------------------------------------
071700130523       //?Ordinamento subfile                                          ?
071800130523       //?  This subroutine sorts the subfile records.                 ?
071900130523       //--------------------------------------------------------------
072000130523       BEGSR  sr_SortSfl;
072100130523
072200130527         // -?Impostazione NRR dell'ultimo record del subfile          ?
072300130527         //  ?(già fatto in sr_InzS01:                                 ?
072400130527         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
072500130527         //RrnLast  = S01nrr;
072600130523
072700130523         C1RcdNbr = 1;
072800130523         clear  C1CsrRrn;
072900130523         clear  S01nrr;
073000130523         clear  V1Dmsg;
073100130524         SflNxtChg   = *off;
073200130523         ErrMessage  = *off;
073300130524         ErrGenerico = *off;
073400130523         %subst(IndDspF : 50) = *off;
073500130523
073600130523         //?___________________________________________________________?
073700130523         //?Initialize the key fields to sort on.?
073800130523         //?There is one extra field not in the subfile -- Selected --?
073900130523         //?that is added to the record. This field is used to place?
074000130523         //?selected records in front of nonselected records.?
074100130523         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
074200130523
074300130523         clear  QLGKL;
074400130523         clear  QLGSKL;
074500130523         clear  QLGSCB;
074600130523         clear  QLGSCB00;
074700130523
074800130523         // -?Gestione della scelta ordinamento:?
074900130523         Select;
075000130523           // -?Ordinamento per Numero Distinta?
075100130523           When  Ord_x_Dist;
075200130523             // -?1 campi chiave?
075300130523             QLGNBRK  = 1;
075400130523             // -?1° campo: cod. filiale + num. distinta (S1HfgsNdc)?
075500130523             //            ?a posizone    1,  9 byte, char, ascending.?
075600130523             QLGSP    = 1;
075700130523             QLGSS    = %size(S1HfgsNdc);
075800130523             QLGDT    = Carattere;
075900130523             QLGSO    = Ascendente;
076000130523             QLGKL(1) = QLGSKL;
076100130523
076200130523           // -?Ordinamento per tipo elaborazione?
076300130523           When  Ord_x_TpEl;
076400130523             // -?2 campi chiave?
076500130524             QLGNBRK  = 2;
076600130523             // -?1° campo: tipo elaborazione (S01tpelab)?
076700130524             //            ?a posizone   27,  1 byte, char, ascending.?
076800130524             QLGSP    = 27;
076900130523             QLGSS    = %size(S01tpElab);
077000130523             QLGDT    = Carattere;
077100130523             QLGSO    = Ascendente;
077200130524             QLGKL(1) = QLGSKL;
077300130523             // -?2° campo: cod. filiale + num. distinta (S1HfgsNdc)?
077400130523             //            ?a posizone    1,  9 byte, char, ascending.?
077500130523             QLGSP    = 1;
077600130523             QLGSS    = %size(S1HfgsNdc);
077700130523             QLGDT    = Carattere;
077800130523             QLGSO    = Ascendente;
077900130523             QLGKL(2) = QLGSKL;
078000130523
078100130523           // -?Ordinamento per data inserimento?
078200130523           When  Ord_x_dtIn;
078300130523             // -?2 campi chiave?
078400130523             QLGNBRK  = 2;
078500130523             // -?1° campo: data inserimento (S1HdatIns)?
078600130523             //            ?a posizone   10,  8 byte, int,  ascending.?
078700130523             QLGSP    = 10;
078800130523             QLGSS    = %size(S1HdatIns);
078900130523             QLGDT    = Numero;
079000130523             QLGSO    = Ascendente;
079100130523             QLGKL(1) = QLGSKL;
079200130523             // -?2° campo: cod. filiale + num. distinta (S1HfgsNdc)?
079300130523             //            ?a posizone    1,  9 byte, char, ascending.?
079400130523             QLGSP    = 1;
079500130523             QLGSS    = %size(S1HfgsNdc);
079600130523             QLGDT    = Carattere;
079700130523             QLGSO    = Ascendente;
079800130523             QLGKL(2) = QLGSKL;
079900130523
080000130523         EndSl;
080100130523
080200130523         // -?Load other sort parameters.?
080300130523         QLGLB   = 80 + 16 * MaxKey;
080400130523         QLGRL   = %size(SflRcd) - 1;
080500130523         QLGRT   = 8;
080600130523         QLGOKL  = 80;
080700130523         QLGLKE  = 16;
080800130523         QLGLSS  = 290;
080900130523         // -?Initialize Sort I/O API fields.?
081000130523         QLGRL00 = QLGRL;
081100130523         QLGRC00 = 1;
081200130523         clear  QUSEI;
081300130523         QUSBPRV = %size(QUSEC);
081400130523
081500130523         // -?First step - Initialize the sort routine.?
081600130523         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
081700130523                      SizeList : ReturnSize : QUSEC );
081800130523
081900130523         // -?Next step - Write records to I/O routine.?
082000130523         QLGRT00  = Put;
082100130523         For  Nrr = 1  To  RrnLast;
082200130523           chain  Nrr  LR67S01;
082300130523           // -?Solo le righe con Selected = 'Y' sono riordinate,?
082400130523           //  ?quindi per fare un ordinamento di tutte le righe?
082500130523           //  ?metto 'Y' sempre.?
082600130523           Selected = 'Y';
082700130523           clear  QUSEI;
082800130523           QUSBPRV  = %size(QUSEC);
082900130523           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
083000130523                         SizeList : NotUsed : QUSEC );
083100130523         EndFor;
083200130523
083300130523         // -?Next step - Signal end of input, clear subfile for reload.?
083400130523         QLGRT00 = EndPut;
083500130523         clear  QUSEI;
083600130523         QUSBPRV = %size(QUSEC);
083700130523         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
083800130523                       SizeList : NotUsed : QUSEC );
083900130523
084000130523         // -?Pulizia subfile?
084100130523         SflDsp_N    = *on;
084200130523         SflDspCtl_N = *on;
084300130523         write  LR67C01;
084400130523         SflDspCtl_N = *off;
084500130523         SflEnd      = *off;
084600130523
084700130523         // -?Final step - Write the records back to the subfile.?
084800130523         QLGRT00  = Get;
084900130523         For  Nrr = 1  To  RrnLast;
085000130523           clear  QUSEI;
085100130523           QUSBPRV = %size(QUSEC);
085200130523           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
085300130523                         QLGRL00  : NotUsed : QUSEC );
085400130523           // -?Ripristino indicatori utilizzati nel sfl rec?
085500130524           SflNxtChg = (S01opz <> *zero);
085600130523           S01nrr = Nrr;
085700130524           write  LR67S01;
085800130523         EndFor;
085900130523
086000130523         C1CsrRrn = 1;
086100130523
086200130523         // -?Visualizzazione del SFL (se ci sono dati)?
086300130523         SflDsp_N = (S01nrr <= *zero);
086400130523
086500130523         // -?Attivazione del SFLEND?
086600130523         SflEnd = %eof(FIIRD11L);
086700130524
086800130524         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
086900130524         Select;
087000130524           When  Ord_x_Dist;
087100130524             P01f08 = 'F8=Ord. x Tipo Elab.   ';
087200130524           When  Ord_x_TpEl;
087300130524             P01f08 = 'F8=Ord. x Data Inserim.';
087400130524           When  Ord_x_DtIn;
087500130524             P01f08 = 'F8=Ord. x Distinta     ';
087600130524         EndSl;
087700130523
087800130523       ENDSR;
087900130523
088000130523       //--------------------------------------------------------------
088100130523       //?Caricamento completo del subfile S01                         ?
088200130523       //--------------------------------------------------------------
088300130523       BEGSR  sr_OpzS01;
088400130523
088500130523         // -?Spegnimento degli indicatori di errore?
088600130523         %subst(IndDspF : 50) = *off;
088700130524
088800130524         clear  SavS01csr;
088900130523
089000130523         // -?Ciclo di lettura subfile?
089100130523         readc  LR67S01;
089200130523
089300130523         DoW  Not %eof(FNLR67D);
089400130523
089500130523           SflNxtChg = *off;
089600130523           %subst(IndDspF : 50) = *off;
089700130523           C1RcdNbr = S01nrr;
089800130523
089900130523           select;
090000130523
090100130523             // -?Opz. 2=Modifica?
090200130523             //  ?Opz. 4=Cancellazione?
090300130523             //  ?Opz. 5=Visualizzazione?
090400130523             when  S01opz = 2  or  S01opz = 4  or  S01opz = 5;
090500130523               $Video  = 'D1';
090600130523               $InzD01 = *on;
090700130523               DoW  $Video = 'D1'  and  Not $Fine;
090800130523                 exsr  sr_GesD01;
090900130523               EndDo;
091000130523
091100130523             // -?Nessuna opzione?
091200130524             when  S01opz  = *zero;
091300130523
091400130523             // -?Opzione errata?
091500130523             other;
091600130524               V1Dmsg = $Msg(01);
091700130524               PosCurOPZ   = *on;
091800130524               ErrMessage  = *on;
091900130523               ErrGenerico = *on;
092000130523               $Fine  = *off;
092100130523
092200130523           endsl;
092300130523
092400130523           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
092500130523           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
092600130524           if  S01opz  <> *zero   and
092700130523              (SavS01csr = *zero   or   SavS01csr < C1RcdNbr);
092800130523             SavS01csr   = C1RcdNbr;
092900130523           endif;
093000130523
093100130523           // -?Aggiornamento sfl?
093200130523           select;
093300130523             when  ErrMessage;
093400130523               SflNxtChg = *on;
093500130523               C1CsrRrn  = C1RcdNbr;
093600130523             when  ErrGenerico;
093700130523               C1CsrRrn  = C1RcdNbr;
093800130523               clear  S01opz;
093900130523             other;
094000130523               clear  S01opz;
094100130523           endsl;
094200130523
094300130523           UPDATE  LR67S01;
094400130523
094500130523           if  ErrGenerico   or   ErrMessage;
094600130523             leave;
094700130523           endif;
094800130523
094900130523           readc  LR67S01;
095000130523
095100130523         ENDDO;
095200130523
095300130523         // -?Riposizionam. cursore sul record contenente la prima opz.?
095400130523         //  ?(se non sono stati rilevati errori)?
095500130523         if  NOT ErrMessage   and   NOT ErrGenerico   and
095600130523             SavS01csr > *zero;
095700130523           C1CsrRrn = SavS01csr;
095800130523         endif;
095900130523
096000130523       ENDSR;
096100130523
096200130523       //--------------------------------------------------------------
096300130523       //?Gestione videata D01.                                        ?
096400130523       //--------------------------------------------------------------
096500130523       BEGSR  sr_GesD01;
096600130523
096700130523         // -?Inizializzazione videata?
096800130523         if  $InzD01 = *on;
096900130523           exsr  sr_InzD01;
097000130523           $InzD01 = *off;
097100130523         endif;
097200130523
097300130523         // -?Emissione Testata?
097400130523         write  LR67T01;
097500130523
097600130523         // -?Emissione videata?
097700130524         if  $Immissione  or  S01opz = 2;
097800130524           exfmt  LR67D01;
097900130524         else;
098000130524           write  LR67D01;
098100130524           exfmt  Protect;
098200130524         endif;
098300130523
098400130523         clear  V1Dmsg;
098500130524         reset  ErrMessage;
098600130524         reset  ErrGenerico;
098700130523
098800130524         Select;
098900130523
099000130523           // -?F3=Fine?
099100130524           When  dsp_aid = c_F03;
099200130523             $Fine = *on;
099300130527
099400130527           // -?F9=Dettaglio (spedizioni in distinta)?
099500130527           When  dsp_aid = c_F09;
099600140107             exsr  sr_CallFIDG311;
099700130523
099800130523           // -?F12=Ritorno?
099900130524           When  dsp_aid = c_F12;
100000130527             if  S01opz = 2;
100100130527               unlock  FIIRD11L;
100200130527             endif;
100300130523             $Video = 'S1';
100400130523
100500130524           // -?Invio o F6=Conferma?
100600130524           Other;
100700130524             if  $Immissione  or  S01opz = 2;
100800130524               exsr  sr_CtrD01;
100900130708             endif;
101000130708             if  not  ErrGenerico  and  dsp_aid = c_F06;
101100130708               exsr  sr_Upd_FIIRD;
101200130708               $Video = 'S1';
101300130708               reset  $InzS01;
101400130708             endif;
101500130523
101600130524         EndSl;
101700130523
101800130523       ENDSR;
101900130523
102000130523       //--------------------------------------------------------------
102100130523       //?Inizializzazione videata D01.                                ?
102200130523       //--------------------------------------------------------------
102300130523       BEGSR  sr_InzD01;
102400140107
102500140107         $Forza = *off;
102600130523
102700130523         // -?Pulizia videata?
102800130523         clear  LR67D01;
102900130524
103000130524         In_Sede = (DUTlpo = 'S');
103100130524
103200130524         select;
103300130524           when  $Immissione;
103400130524             D01opz = '  IMMISSIONE   ';
103500130524           when  S01opz = 2;
103600130524             D01opz = '   MODIFICA    ';
103700130524           when  S01opz = 4;
103800130524             D01opz = ' CANCELLAZIONE ';
103900130524           when  S01opz = 5;
104000130524             D01opz = 'VISUALIZZAZIONE';
104100130524         endsl;
104200130523
104300130524         // -?Impostazione campi a video?
104400130524         If  $Immissione;
104500130523
104600130524           if  Not In_Sede;
104700130524             D01fil  = DUTpou;
104800130524             D01ndc  = '?';
104900130524           endif;
105000130523           D01datIns = *date;
105100130523
105200130523         Else;
105300130523
105400130524           clear  k01fiird11;
105500130523           k_IRDfgsNdc = S1Hfgsndc;
105600130524           if  S01opz  = 5;
105700130524             chain(N)  %kds( k01fiird11 )  FIIRD000;
105800130524           else;
105900130524             chain  %kds( k01fiird11 )  FIIRD000;
106000130524           endif;
106100130523
106200130524           If  %found(FIIRD11L);
106300130523
106400130523             D01fil    = %int( %subst( IRDfgsndc : 1 : %len(D01fil) ) );
106500130524             D01ndc    = %subst( IRDfgsndc : %len(D01fil) + 1 );
106600130523             D01tpElab = IRDtpElab;
106700130523             if  IRDdatIns > *zero;
106800130523               wDate_Eur = %date( IRDdatIns : *iso );
106900130524               D01DatIns = %dec( wDate_Eur );
107000130523             endif;
107100130523
107200130523             // ·?Decodifiche?
107300130523             exsr  sr_CtrD01;
107400130523             clear  V1Dmsg;
107500130524             reset  ErrMessage;
107600130524             reset  ErrGenerico;
107700130523
107800130523           Else;
107900130523
108000130523             D01fil = S01fil;
108100130524             D01ndc = %editc( S01ndc : 'X' );
108200130523
108300130523           EndIf;
108400130523
108500130523         EndIf;
108600130524
108700130524         // -?Impostazione indicatori per attributi di visualizzazione?
108800130524         Protect_Key = (Not  $Immissione);
108900130524
109000130524         // -?(Dis)Abilitazione tasti funzionali?
109100130524         F6Attivo = ($Immissione  or  S01opz <> 5);
109200130527         F9Attivo = (Not  In_Sede);
109300130523
109400130523       ENDSR;
109500130523
109600130523       //--------------------------------------------------------------
109700130523       //?Controllo dati immessi nella videata D01.                    ?
109800130523       //--------------------------------------------------------------
109900130523       BEGSR  sr_CtrD01;
110000130523
110100130523         // -?Spegnimento degli indicatori di errore?
110200130523         %subst(IndDspF : 50) = *off;
110300130523
110400130523         // -?Controllo Filiale?
110500130523         clear  D01filD;
110600130523
110700130523         // ·?Obbligatoria?
110800130523         if  D01fil = *zero;
110900130523           V1Dmsg = $Msg(02);
111000130523           PosCurFIL   = *on;
111100130523           ErrMessage  = *on;
111200130523           ErrGenerico = *on;
111300130523           leavesr;
111400130523         endif;
111500130523
111600130523         // ·?Errata?
111700130523         k_ORGfil = D01fil;
111800130523         chain  %kds( k01azorg01 )  AZORG;
111900130523         if  %found(AZORG01L);
112000130523           D01filD = ORGdes;
112100130523           Og148   = ORGde8;
112200130523         endif;
112300130523         if  Og148.§ogLPO = *blank;
112400130523           V1Dmsg = $Msg(03);
112500130523           PosCurFIL   = *on;
112600130523           ErrMessage  = *on;
112700130523           ErrGenerico = *on;
112800130523           leavesr;
112900130523         endif;
113000140107
113100130523         // -?Controllo Distinta?
113200130527         exsr  sr_CtrlNDC;
113300140107         if  ErrMessage;
113400140107           leavesr;
113500140107         endif;
113600130524
113700130524         // -?Controllo se chiave duplicata in inserimento?
113800130524         if  $Immissione;
113900130524           clear  k01fiird11;
114000130524           k_IRDfgsNdc = %editc( D01fil : 'X' ) + w01ndcA;
114100130524           setll  %kds( k01fiird11 )  FIIRD000;
114200130524           if  %equal(FIIRD11L);
114300130524             V1Dmsg = $Msg(08);
114400130524             PosCurNDC   = *on;
114500130524             ErrMessage  = *on;
114600130524             ErrGenerico = *on;
114700130524             leavesr;
114800130524           endif;
114900130524         endif;
115000130523
115100130523         // -?Controllo Tipo Elaborazione?
115200130523         if  D01tpElab <> *blank  and  D01tpElab <> 'F';
115300130524           V1Dmsg = $Msg(09);
115400130523           PosCurTPEL  = *on;
115500130523           ErrMessage  = *on;
115600130523           ErrGenerico = *on;
115700130523           leavesr;
115800130523         endif;
115900130523
116000130523         // -?Data Inserimento obbligatoria?
116100130523         if  D01datIns = *zero;
116200130524           V1Dmsg = $Msg(10);
116300130523           PosCurDTIN  = *on;
116400130523           ErrMessage  = *on;
116500130523           ErrGenerico = *on;
116600130523           leavesr;
116700130523         endif;
116800130523
116900130523         // -?Data Inserimento formalmente errata?
117000130524         //clear  wDate_Iso;
117100130524         //Monitor;
117200130524         //  wDate_Iso = %date( D01datIns : *eur );
117300130524         //On-Error;
117400130524         //  V1Dmsg = $Msg(11);
117500130524         //  PosCurDTIN  = *on;
117600130524         //  ErrMessage  = *on;
117700130524         //  ErrGenerico = *on;
117800130524         //  leavesr;
117900130524         //EndMon;
118000130524         clear WLBdat;
118100130524         G08dat = D01datIns;
118200130524         XSRDA8(WLBdat);
118300130524         if  G08err = *on;
118400130524           V1Dmsg = $Msg(11);
118500130524           PosCurDTIN  = *on;
118600130524           ErrMessage  = *on;
118700130524           ErrGenerico = *on;
118800130524           leavesr;
118900130524         else;
119000130524           D01datIns = G08dat;
119100130524         endif;
119200130523
119300130523       ENDSR;
119400130527
119500130527       //--------------------------------------------------------------
119600130527       //?Controllo Numero Distinta Consegna                           ?
119700130527       //--------------------------------------------------------------
119800130527       BEGSR  sr_CtrlNDC;
119900130527
120000130527         clear  D01ndcD;
120100140107
120200140107         // -?Reset del flag di forzatura "Distinta inesistente"?
120300140107         //  ?SE cambiata distinta a video?
120400140107         if  D01fil <> SaveFIL  or  D01ndc <> SaveNDC;
120500140107           $Forza = *off;
120600140107           F9Attivo =  *on;
120700140107           SaveFIL  =  D01fil;
120800140107           SaveNDC  =  D01ndc;
120900140107         endif;
121000130527
121100130527         // -?Ricerca?
121200130527         if  Not In_Sede  and  %scan( '?' : D01ndc ) > *zero;
121300130527           reset  dsLR01;
121400130527           DLRpgm = SDSpgm;
121500130527           DLRfgs = D01fil;
121600130527           DLRgal = *date;
121700130527           KPJBU  = dsLR01;
121800130527           fnlr02r ( KPJBA );
121900130527           dsLR01 = KPJBU;
122000130527           evalr D01ndc = %editc( DLRnfv : 'Z' );
122100130527           leavesr;
122200130527         endif;
122300130527
122400130527         // -?Sostituzione *blank con *zero per ctrl numericità?
122500130527         w01ndcA = %xlate( ' ' : '0' : D01ndc : 1 );
122600130527         if  %check( c_Digits : w01ndcA ) > *zero;
122700130527           V1Dmsg = $Msg(04);
122800130527           PosCurNDC   = *on;
122900130527           ErrMessage  = *on;
123000130527           ErrGenerico = *on;
123100130527           leavesr;
123200130527         endif;
123300130527         w01ndcN = %int( w01ndcA );
123400130527         D01ndc  = %editc( w01ndcN : 'Z' );
123500130527
123600130527         // -?Non inserito?
123700130527         if  w01ndcN = *zero;
123800130527           V1Dmsg = $Msg(05);
123900130527           PosCurNDC   = *on;
124000130527           ErrMessage  = *on;
124100130527           ErrGenerico = *on;
124200130527           leavesr;
124300130527         endif;
124400130527
124500130527         // -?Ctrl Validità:?
124600130527         clear  k03fidst01;
124700130527         k_DSTnpg = 4;
124800130527         k_DSTnfv = w01ndcN;
124900130527         k_DSTfgs = D01fil;
125000130527         chain  %kds( k03fidst01 )  FIDST000;
125100140107         F9Attivo = (Not  In_Sede      and
125200140107                     %found(FIDST01L)  and  DSTatb = *blank);
125300130527         Select;
125400130527           // -?Distinta errata?
125500140107           When  (NOT  %found(FIDST01L)  or  DSTatb <> *blank)  and
125600140107                  NOT  $Forza;
125700140107             $Forza = *on;
125800130527             V1Dmsg = $Msg(06);
125900130527             PosCurNDC   = *on;
126000130527             ErrMessage  = *on;
126100130527             ErrGenerico = *on;
126200130527             leavesr;
126300130527           // -?Distinta non chiusa?
126400140107           When  %found(FIDST01L)  and  DSTatb = *blank  and
126500140107                 DSTfcf <> 'S';
126600130527             V1Dmsg = $Msg(07);
126700130527             PosCurNDC   = *on;
126800130527             ErrMessage  = *on;
126900130527             ErrGenerico = *on;
127000130527             leavesr;
127100140107           // -?Decodifica autotrasportatore?
127200130527           Other;
127300140107             if  NOT  $Forza;
127400140107               clear  k02fiapd01;
127500140107               k_APDtip = DSTtipA;
127600140107               k_APDpdr = DSTpdr;
127700140107               chain  %kds( k02fiapd01 )  FIAPD000;
127800140107               if  %found(FIAPD01L);
127900140107                 D01ndcD = APDrsc;
128000140107               endif;
128100140107             else;
128200140107               D01ndcD = *all'? ';
128300140107             endif;
128400130527         EndSl;
128500130527
128600130527       ENDSR;
128700130527
128800130527       //--------------------------------------------------------------
128900130527       //?Richiamo *pgm Interrogazione Distinte                        ?
129000130527       //--------------------------------------------------------------
129100130527       BEGSR  sr_CallFIDG311;
129200130527
129300130527         // -?Ctrl Numero Distinta Consegna?
129400130527         exsr  sr_CtrlNDC;
129500130527         if  ErrMessage;
129600130527           leavesr;
129700130527         endif;
129800130527
129900130527         // -?Interrogazine Distinta?
130000130527         reset  FIDG311ds;
130100130527         F31fgs = DSTfgs;
130200130527         //F31npg = 4;        ?(già così)?
130300130527         F31numDis  = DSTnfv;
130400130527         F31datDis  = DSTdfv;
130500130527         //F31priDati = 'C';  ?(default)?
130600130527         F31DatiVis = DSTpda;
130700130527         KPJBU = FIDG311ds;
130800130527         fidg311r ( kpjba );
130900130527         FIDG311ds  = KPJBU;
131000130527         if  F31tFunz = '03';
131100130527           $Fine = *on;
131200130527           leavesr;
131300130527         endif;
131400130527
131500130527       ENDSR;
131600130523
131700130523       //--------------------------------------------------------------
131800130523       //?Aggiornamento/Cancellazione singola distinta.                ?
131900130523       //--------------------------------------------------------------
132000130523       BEGSR  sr_Upd_FIIRD;
132100130523
132200130523         // -?Aggancio record da aggiornare/inserire/annullare?
132300130524         clear  k01fiird11;
132400130524         k_IRDfgsNdc = %editc( D01fil : 'X' ) + w01ndcA;
132500130524         chain  %kds( k01fiird11 )  FIIRD000;
132600130523
132700130523         // -?Impostazione dei campi chiave  SE immissione?
132800130524         if  NOT  %found(FIIRD11L);
132900130524           clear  FIIRD000;
133000130524           IRDfgsNdc = k_IRDfgsNdc;
133100130523         endif;
133200130523
133300130523         // -?Impostazione dati (se NON annullamento)?
133400130524         If  S01opz <> 4;
133500130524
133600130524           IRDtpElab = D01tpElab;
133700130524           //IRDdatIns = %dec( wDate_Iso );
133800130524           IRDdatIns = G08inv;
133900130524
134000130524         EndIf;
134100130523
134200130524         Select;
134300130524
134400130524           // -?Cancellazione (opz. 4)?
134500130524           When  Not $Immissione  and  S01opz = 4;
134600130524             //_______________
134700130524             DELETE  FIIRD000;
134800130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
134900130524
135000130524           // -?Modifica (opz. 2)?
135100130524           When  %found(FIIRD11L);
135200130524             //_______________
135300130524             UPDATE  FIIRD000;
135400130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
135500130524
135600130524           // -?Immissione (F6)?
135700130524           Other;
135800130524             //_________________
135900130524             WRITE(e)  FIIRD000;
136000130524             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
136100130524             if  %error;
136200130524               V1Dmsg = %trimr($Msg(12));
136300130524               PosCurFIL   = *on;
136400130524               ErrMessage  = *on;
136500130524               ErrGenerico = *on;
136600130524               leavesr;
136700130524             endif;
136800130524             $Almeno1rec = *on;
136900130524
137000130524         EndSl;
137100130523
137200130523       ENDSR;
137300130523
137400130523       //--------------------------------------------------------------
137500130523       //?Operazioni finali.                                           ?
137600130523       //--------------------------------------------------------------
137700130523       BEGSR  sr_RoutEnd;
137800130523
137900130523         // -?Chiusura *pgm?
138000130523         return;
138100130523
138200130523       ENDSR;
138300130523
138400130523      /end-free
138500130523
138600130523** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
138700130523Opzione errata                                                                  1
138800130523Filiale obbligatoria                                                            2
138900130523Filiale errata                                                                  3
139000130524Numero distinta errato: contiene caratteri NON numerici                         4
139100130524Numero distinta obbligatorio                                                    5
139200140107ATTENZIONE: distinta inesistente - Premere "Invio" per forzare                  6
139300130524Distinta NON ancora chiusa                                                      7
139400130524Distinta già inserita                                                           8
139500130524Tipo Elaborazione errato: " " = Controllo immagine su TITAS, "F" = Forzatura    9
139600130524Data Inserimento obbligatoria                                                  10
139700130524Data Inserimento formalmente errata                                            11
139800130524Rilevato errore in fase di registrazione distinta                              12
