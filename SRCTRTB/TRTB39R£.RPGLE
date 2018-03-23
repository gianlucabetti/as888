000100121012       //==============================================================
000200121012       //?TRTB39R - Manutenzione tab. "4W" (Stato del Credito Clienti) ?
000300121012       //==============================================================
000400121012
000500121012       //--------------------------------------------------------------
000600121012       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121012       //--------------------------------------------------------------
000800121012
000900121012     /*PRM  dbgview(*source)
001000121012     /*END
001100121012
001200121012       //--------------------------------------------------------------
001300121012       //?Specifiche di controllo.                                     ?
001400121012       //--------------------------------------------------------------
001500121012
001600121012     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700121012     h dftactgrp(*no)
001800121012     h bnddir('TRUL')
001900121012
002000121012       //--------------------------------------------------------------
002100121012       //?Dichiarazione file.                                          ?
002200121012       //--------------------------------------------------------------
002300121012
002400121012       // -?Tabelle?
002500121012     fTABEL00F  Uf A e           k disk
002600121012
002700121012       // -?Video?
002800121012     fTRTB39D   cf   e             workstn
002900121012     f                                     indds(IndDspF)
003000121012     f                                     infds(InfDspF)
003100121012     f                                     sfile(TB39S01:S01nrr)
003200121012
003300121012       //--------------------------------------------------------------
003400121012       //?Definizione costanti.                                        ?
003500121012       //--------------------------------------------------------------
003600121012
003700121012       // -?Codice tabella in gestione?
003800121012     d c_Tab           c                   const('4W')
003900121012
004000121012       // -?Codice utente?
004100121012     d c_Kut           c                   const(1)
004200121012
004300121012       // -?Opzioni a video?
004400121012     d c_Opz_G         c                   const('2=Modifica, +
004500121012     d                                            3=Copia, +
004600121012     d                                            4=Annullam./Ripristino, +
004700121012     d                                            5=Visualizzaz.')
004800121012     d c_Opz_S         c                   const('1=Scelta.')
004900121012
005000121012       // -?Numero di record in ciascuna videata di subfile?
005100121012     d c_SflPag        c                   const(14)
005200121012
005300121012       // -?Numero massimo di record gestibili?
005400121012     d c_MaxRec        c                   const(9999)
005500121012
005600121012       // -?Tasti funzionali a video?
005700121012     d c_F01           c                   const(x'31')
005800121012     d c_F02           c                   const(x'32')
005900121012     d c_F03           c                   const(x'33')
006000121012     d c_F04           c                   const(x'34')
006100121012     d c_F05           c                   const(x'35')
006200121012     d c_F06           c                   const(x'36')
006300121012     d c_F07           c                   const(x'37')
006400121012     d c_F08           c                   const(x'38')
006500121012     d c_F09           c                   const(x'39')
006600121012     d c_F10           c                   const(x'3A')
006700121012     d c_F11           c                   const(x'3B')
006800121012     d c_F12           c                   const(x'3C')
006900121012     d c_F13           c                   const(x'B1')
007000121012     d c_F14           c                   const(x'B2')
007100121012     d c_F15           c                   const(x'B3')
007200121012     d c_F16           c                   const(x'B4')
007300121012     d c_F17           c                   const(x'B5')
007400121012     d c_F18           c                   const(x'B6')
007500121012     d c_F19           c                   const(x'B7')
007600121012     d c_F20           c                   const(x'B8')
007700121012     d c_F21           c                   const(x'B9')
007800121012     d c_F22           c                   const(x'BA')
007900121012     d c_F23           c                   const(x'BB')
008000121012     d c_F24           c                   const(x'BC')
008100121012     d c_Enter         c                   const(x'F1')
008200121012     d c_RollDown      c                   const(x'F4')
008300121012     d c_RollUp        c                   const(x'F5')
008400121012
008500121012       //--------------------------------------------------------------
008600121012       //?Definizione schiere.                                         ?
008700121012       //--------------------------------------------------------------
008800121012
008900121012       // -?Messaggi di errore?
009000121109     d $Msg            s             78    dim(21)  ctdata  perrcd(1)
009100121012
009200121012       // -?Opzioni abilitate?
009300121012     d $Opz            s               n   dim( 5)  inz
009400121012
009500121012       //--------------------------------------------------------------
009600121012       //?Definizione aree dati.                                       ?
009700121012       //--------------------------------------------------------------
009800121012
009900121012       // -?Dati utente?
010000121012     d §AzUte        e ds                  extname(AZUTE00F)
010100121012     d                                     dtaara
010200121012     d §DatiUte      e ds                  extname(dDatiUte)
010300121012     d                                     dtaara
010400121012
010500121012       //--------------------------------------------------------------
010600121012       //?Definizione strutture dati.                                  ?
010700121012       //--------------------------------------------------------------
010800121012
010900121012       // -?Status ds?
011000121012     d Status         sds
011100121012     d   SDSpgm          *proc
011200121012
011300121012       // -?InfDS?
011400121012     d InfDspF         ds
011500121012     d   dsp_aid             369    369a
011600121012     d   sfl_rrn             376    377i 0
011700121012     d   min_nrr             378    379i 0
011800121012     d   num_rcds            380    381i 0
011900121012
012000121012       // -?Indicatori su DspF?
012100121012     d IndDspF         ds                  inz
012200121012         // -?Abilitazione tasti funzionali?
012300121012     d   F3Attivo                      n   overlay(IndDspF : 03)
012400121012     d   F5Attivo                      n   overlay(IndDspF : 05)
012500121012     d   F6Attivo                      n   overlay(IndDspF : 06)
012600121012     d   F8Attivo                      n   overlay(IndDspF : 08)
012700121012     d   F10Attivo                     n   overlay(IndDspF : 10)
012800121012     d   F12Attivo                     n   overlay(IndDspF : 12)
012900121012     d   F13Attivo                     n   overlay(IndDspF : 13)
013000121012         // -?Emissione messaggio di errore?
013100121012     d   ErrMessage                    n   overlay(IndDspF : 28)
013200121012         // -?Indicatori di gestione del subfile?
013300121012     d   SflDsp_N                      n   overlay(IndDspF : 30)
013400121012     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
013500121012     d   SflNxtChg                     n   overlay(IndDspF : 32)
013600121012     d   SflEnd                        n   overlay(IndDspF : 33)
013700121012         // -?Indicatori per Attibuti di visualizzazione?
013800121012         //  ?o   Condizionamento visualizzazione campi?
013900121012     d   Ord_x_CON                     n   overlay(IndDspF : 40)
014000121012     d   Ord_x_DES                     n   overlay(IndDspF : 41)
014100121012     d   Ord_x_TIP                     n   overlay(IndDspF : 42)
014200121012     d   Ord_x_MBL                     n   overlay(IndDspF : 43)
014300121012     d   Ord_x_TBL                     n   overlay(IndDspF : 44)
014400121012     d   Ord_x_BLC                     n   overlay(IndDspF : 45)
014500121012     d   Ord_x_MTB                     n   overlay(IndDspF : 46)
014600121012     d   Ord_x_PBL                     n   overlay(IndDspF : 47)
014700121012     d   Ord_x_CDP                     n   overlay(IndDspF : 48)
014800121012     d   Ord_x_MBP                     n   overlay(IndDspF : 49)
014900121012     d   $Protect_Key                  n   overlay(IndDspF : 50)
015000121012     d   $Cod_Annull                   n   overlay(IndDspF : 51)
015100121012         // -?Posizionamento cursore & Segnalazione errore?
015200121012     d   PosCurOPZ                     n   overlay(IndDspF : 60)
015300121012     d   PosCurCON                     n   overlay(IndDspF : 61)
015400121012     d   PosCurDES                     n   overlay(IndDspF : 62)
015500121012     d   PosCurTIP                     n   overlay(IndDspF : 63)
015600121012     d   PosCurMBL                     n   overlay(IndDspF : 64)
015700121012     d   PosCurTBL                     n   overlay(IndDspF : 65)
015800121012     d   PosCurBLC                     n   overlay(IndDspF : 66)
015900121012     d   PosCurMTB                     n   overlay(IndDspF : 67)
016000121012     d   PosCurPBL                     n   overlay(IndDspF : 68)
016100121012     d   PosCurCDP                     n   overlay(IndDspF : 69)
016200121012     d   PosCurMBP                     n   overlay(IndDspF : 70)
016300121012     d   PosCurCTZ                     n   overlay(IndDspF : 71)
016400121012         // -?Riemissione videata?
016500121012     d   ErrGenerico                   n   overlay(IndDspF : 99)
016600121012
016700121012       // -?Parametri ricevuti?
016800121012     d KPJBA         e ds
016900121012     d*// TRTB39ds      e ds                  inz
017000121012
017100121012       // -?Tab. "4W" = Stato del Credito Clienti?
017200121012     d ds4W          e ds                  inz
017300121012       // -?Tab. "BI" = Causali di Blocco?
017400121012     d dsBI          e ds                  inz
017500121012       // -?Tab. "FA" = Codici Pagamento?
017600121012     d dsFA          e ds                  inz
017700121012
017800121012       //--------------------------------------------------------------
017900121012       //?Definizione variabili globali.                               ?
018000121012       //--------------------------------------------------------------
018100121012
018200121012       // -?Flags booleani?
018300121012     d $Called         s               n   inz
018400121012     d $Fine           s               n   inz
018500121012     d $EoF            s               n   inz
018600121012     d $InzS01         s               n   inz(*on)
018700121012     d $InzD02         s               n   inz(*on)
018800121012     d $InzW01         s               n   inz(*on)
018900121012     d $RecOK          s               n   inz
019000121012     d $Inserim        s               n   inz
019100121012     d $ForzaOpz       s               n   inz
019200121012
019300121012       // -?Variabili per la gestione del video?
019400121012     d $Video          s              2    inz('S1')
019500121012     d S01nrr          s                   like(C1RcdNbr) inz
019600121012     d S01nrrMax       s                   like(S01nrr)   inz
019700121012     d SavS01csr       s                   like(C1RcdNbr) inz
019800121012     d SavS1Copz       s                   like(S1Copz)   inz
019900121012     d SavS01cs1       s                   like(C1RcdNbr) inz
020000121012     d SavS1Cop1       s                   like(S1Copz)   inz
020100121012     d SavS01cs2       s                   like(C1RcdNbr) inz
020200121012     d SavTBLuni       s                   like(TBLuni)   inz
020300121012     d SavC1Ccon       s                   like(C1Ccon)   inz
020400121012     d SavC1Cdes       s                   like(C1Cdes)   inz
020500121012     d SavC1Ctip       s                   like(C1Ctip)   inz
020600121012     d SavC1Cmbl       s                   like(C1Cmbl)   inz
020700121012     d SavC1Ctbl       s                   like(C1Ctbl)   inz
020800121012     d SavC1Cblc       s                   like(C1Cblc)   inz
020900121012     d SavC1Cmtb       s                   like(C1Cmtb)   inz
021000121012     d SavC1Cpbl       s                   like(C1Cpbl)   inz
021100121012     d SavC1Ccdp       s                   like(C1Ccdp)   inz
021200121012     d SavC1Cmbp       s                   like(C1Cmbp)   inz
021300121012     d wOrdSfl         s              1  0 inz
021400121012     d w_SflPag        s              3  0 inz
021500121012     d wPag            s              4  0 inz
021600121012     d wRig            s              3  0 inz
021700121012
021800121012       // -?Campi di comodo?
021900121012     d wDate_DMY       s               d   datfmt(*dmy)  inz
022000121012
022100121012       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
022200121012
022300121012       //--------------------------------------------------------------
022400121012       // -?Constants?
022500121012       //--------------------------------------------------------------
022600121012       // -?MaxKey - Maximum number of key fields allowed by this program?
022700121012     d MaxKey          c                   const(4)
022800121012       // -?Sort order:?
022900121012       //  ?1 = Sort in an ascending sequence?
023000121012       //  ?2 = Sort in a descending sequence?
023100121012     d Ascendente      c                   const(1)
023200121012     d Discendente     c                   const(2)
023300121012       // -?Key data type:?
023400121012       //  ? 0 = Signed binary?
023500121012       //  ? 2 = Signed zoned decimal?
023600121012       //  ? 3 = Signed packed decimal?
023700121012       //  ? 6 = Character with no national language sort sequence applied,?
023800121012       //  ?     if specified?
023900121012       //  ? 7 = Unsigned packed decimal?
024000121012       //  ?     All numbers will have the sign forced positive ('F'X).?
024100121012       //  ? 8 = Unsigned zoned decimal?
024200121012       //  ?     All numbers will have the sign forced positive ('F'X).?
024300121012       //  ? 9 = Unsigned binary?
024400121012       //  ?14 = Date in form of DD/MM/YY?
024500121012       //  ?15 = Date in form of DD.MM.YYYY?
024600121012     d Numero          c                   const(2)
024700121012     d Carattere       c                   const(6)
024800121012       //
024900121012     d Put             c                   const(1)
025000121012     d EndPut          c                   const(2)
025100121012     d Get             c                   const(3)
025200121012
025300121012       //--------------------------------------------------------------
025400121012       // -?Data Structures?
025500121012       //  ?SflRcd     - The entire subfile record to pass to the sort?
025600121012       //  ?QLGSCB     - The sort request block for the QLGSORT API?
025700121012       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
025800121012       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
025900121012       //  ?QUSEC      - Error structure for the QLGSORT API?
026000121012       //--------------------------------------------------------------
026100121012     d SflRcd          ds                  inz
026200121012     d   S1Ccon                            inz
026300121012     d   S1Cdes                            inz
026400121012     d   S1Ctip                            inz
026500121012     d   S1Cmbl                            inz
026600121012     d   S1Ctbl                            inz
026700121012     d   S1Cblc                            inz
026800121012     d   S1Cmtb                            inz
026900121012     d   S1Cpbl                            inz
027000121012     d   S1Ccdp                            inz
027100121012     d   S1Cmbp                            inz
027200121012     d   S1Catb                            inz
027300121012     d   S1Copz                            inz
027400121012     d   Selected                     1a   inz
027500121012      /copy QSYSINC/QRPGLESRC,QLGSORT
027600121012     d QLGKL                         16    dim(MaxKey)
027700121012     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
027800121012     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
027900121012     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
028000121012     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
028100121012      /copy QSYSINC/QRPGLESRC,QLGSRTIO
028200121012      /copy QSYSINC/QRPGLESRC,QUSEC
028300121012
028400121012       //--------------------------------------------------------------
028500121012       // -?Standalone fields?
028600121012       //  ?Nrr        - Relative record number for subfile?
028700121012       //  ?SizeList   - Size of list?
028800121012       //  ?ReturnSize - Size of list returned by sort API?
028900121012       //--------------------------------------------------------------
029000121012     d NotUsed         s             16a   inz
029100121012     d ReturnSize      s              9b 0 inz
029200121012     d SizeList        s              9b 0 inz
029300121012     d RrnLast         s              5i 0 inz
029400121012     d Nrr             s              5i 0 inz
029500121012
029600121012       //--------------------------------------------------------------
029700121012       //?Definizione prototipi procedure.                             ?
029800121012       //--------------------------------------------------------------
029900121012
030000121012       // -?Reperimento dati utente?
030100121012     d TIBS34ds      e ds                  inz
030200121012      /copy gaitrasrc/srcProtoPR,TIBS34R
030300121012
030400121012       // -?Ricerca tabella TABEL00F?
030500121012     d idElemento      s              8    inz
030600121012     d tastoFunzionaleUscita...
030700121012     d                 s              1    inz
030800121012      /copy gaitrasrc/srcProtoPR,TRUL19R
030900121012
031000121012       // -?Reperimento dati tabelle?
031100121012      /copy gaitrasrc/srcProtoPI,TRULTAB
031200121012      /copy gaitrasrc/srcProtoPR,TRULTAB
031300121015
031400121015       // -?Ricerca tabella "FA"?
031500121015     d XcdPagr         pr                  extpgm('XCDPAGR')
031600121015     d   CondizPagamCod...
031700121015     d                                3a
031800121015     d   CondizPagamDes...
031900121015     d                               30a
032000121012
032100121012       // -?Ordinamento subfile?
032200121012      /copy gaitrasrc/srcProtoPr,QLGSRTIO
032300121012
032400121012       //--------------------------------------------------------------
032500121012       //?Definizione key-list.                                        ?
032600121012       //--------------------------------------------------------------
032700121012
032800121012       // -?File TABEL00F?
032900121012     d k03tabel00    e ds                  extname(TABEL00F : *key)
033000121012     d                                     prefix(k_)   inz
033100121012
033200121012       //--------------------------------------------------------------
033300121012       //?Riepilogo indicatori utilizzati.                             ?
033400121012       //--------------------------------------------------------------
033500121012       //--------------------------------------------------------------
033600121012
033700121012       //--------------------------------------------------------------
033800121012       //?M A I N - L I N E                                            ?
033900121012       //--------------------------------------------------------------
034000121012
034100121012     c     *Entry        plist
034200121012     c                   parm                    KPJBA
034300121012
034400121012      /free
034500121012
034600121012       // -?Operazioni iniziali?
034700121012       exsr  sr_RoutInz;
034800121012
034900121012       // -?Ciclo di gestione del file video?
035000121012       DoW  Not  $Fine;
035100121012         select;
035200121012           // -?Gestione videata S1 (subfile Cod.Tassazione)?
035300121012           when  $Video = 'S1';
035400121012             exsr  sr_GesS01;
035500121012           // -?Gestione videata D2 (gestione singolo Cod.Tassazione)?
035600121012           //  ?- Inserimento -?
035700121012           when  $Video = 'D2';
035800121012             exsr  sr_GesD02;
035900121012           // -?Gestione videata W1 (trasmissione dati)?
036000121012           //  ?(videata gestita direttamente dalla videata D02)?
036100121012           //when  $Video = 'W1';
036200121012           //  exsr  sr_GesW01;
036300121012           // -?? ? ? ? ??
036400121012           other;
036500121012             $Fine = *on;
036600121012         endsl;
036700121012       enddo;
036800121012
036900121012       // -?Operazioni finali?
037000121012       exsr  sr_RoutEnd;
037100121012
037200121012       //--------------------------------------------------------------
037300121012       //?Operazioni iniziali.                                         ?
037400121012       //--------------------------------------------------------------
037500121012       BEGSR  sr_RoutInz;
037600121012
037700121012         // -?Impostazione chiusura?
037800121012         *inLR = *on;
037900121012
038000121012         // -?Impostazione parametri ricevuti?
038100121012         //TRTB39ds  = KPJBU;
038200121012         //clear  TB39des;
038300121012         //clear  TB39fxx;
038400121012         //TB39err = *off;
038500121012
038600121012         // -?Controllo parametri ricevuti?
038700121012         //select;
038800121012         //  when  TB39fun <> *blank  and  TB39fun <> '1';
038900121012         //    TB39err = *on;
039000121012         //endsl;
039100121012         //if  TB39err = *on;
039200121012         //  exsr  sr_RoutEnd;
039300121012         //endif;
039400121012         //$Called = (TB39fun = '1'  or  TB39con <> *blank);
039500121015         $Called = (%subst(kpjbu : 1 : 1) = '1');
039600121012
039700121012         // -?Reperimento dati job?
039800121012         exsr  sr_DatiJob;
039900121012
040000121012         // -?Impostazione nome programma a video?
040100121012         V1Tpgm = SDSpgm;
040200121012
040300121012         // -?Impostazione iniziale / pulizia dei campi chiave?
040400121012         clear  k03tabel00;
040500121012         k_TBLkut = c_Kut;
040600121012         k_TBLcod = c_Tab;
040700121012         wOrdSfl = *zero;
040800121012         //if  TB39ord >= '0'  and  TB39ord <= '9';
040900121012         //  wOrdSfl = %int(TB39ord);
041000121012         //endif;
041100121012
041200121012       ENDSR;
041300121012
041400121012       //--------------------------------------------------------------
041500121012       //?Reperimento Dati del job (Utente/Operativi).                 ?
041600121012       //--------------------------------------------------------------
041700121012       BEGSR  sr_DatiJob;
041800121012
041900121012         in(e) §AzUte;
042000121012         if NOT %error;
042100121012           in(e) §DatiUte;
042200121012         endif;
042300121012         if %error or RSut = *blank;
042400121012           tibs34r ( tibs34ds );
042500121012           in §AzUte;
042600121012           in §DatiUte;
042700121012         endif;
042800121012
042900121012       ENDSR;
043000121012
043100121012       //--------------------------------------------------------------
043200121012       //?Gestione subfile S01.                                        ?
043300121012       //--------------------------------------------------------------
043400121012       BEGSR  sr_GesS01;
043500121012
043600121012         // -?Inizializzazione videata?
043700121012         if  $InzS01 = *on;
043800121012           exsr  sr_InzS01;
043900121012           $InzS01 = *off;
044000121012         endif;
044100121012
044200121012         // -?(Dis)Abilitazione tasti funzionali?
044300121012         //  ?(meglio farlo comunque, perchè si potrebbe venire dal fmt D02)?
044400121012         exsr  sr_AbilFxxS01;
044500121012
044600121012         // -?Emissione Testata & Piede?
044700121012         write  TB39T01;
044800121012         write  TB39Z01;
044900121012
045000121012         // -?Posizionamento cursore?
045100121012         if  C1CsrRrn > *zero;
045200121012           C1RcdNbr = C1CsrRrn;
045300121012         else;
045400121012           C1RcdNbr = 1;
045500121012           write  TB39S00;             //?(no rec.)?
045600121012         endif;
045700121012
045800121012         // -?Emissione videata?
045900121012         exfmt  TB39C01;
046000121012
046100121012         reset  ErrGenerico;
046200121012         reset  ErrMessage;
046300121012         clear  V1Dmsg;
046400121012
046500121012         if  dsp_aid <> c_F03  and
046600121012             dsp_aid <> c_F13;
046700121012           clear  SavS01csr;
046800121012           clear  SavS1Copz;
046900121012           clear  SavS01cs1;
047000121012           clear  SavS1Cop1;
047100121012           clear  SavS01cs2;
047200121012         endif;
047300121012
047400121012         $Inserim = (dsp_aid = c_F10);
047500121012
047600121012         select;
047700121012
047800121012           // -?F3=Fine?
047900121012           when  dsp_aid = c_F03;
048000121012             $Fine = *on;
048100121012             //TB39fxx = 'F03';
048200121012
048300121012           // -?F5=Aggiornamento?
048400121012           WHEN  dsp_aid = c_F05;
048500121012             reset  $InzS01;
048600121012
048700121012           // -?F8=Ordinamento Sfl?
048800121012           WHEN  dsp_aid = c_F08;
048900121012             reset  $ForzaOpz;
049000121012             exsr  sr_OrdS01;
049100121012
049200121012           // -?F10=Inserimento?
049300121012           WHEN  dsp_aid = c_F10;
049400121012             reset  $ForzaOpz;
049500121012             $Video = 'D2';
049600121012             reset  $InzD02;
049700121012
049800121012           // -?F12=Ritorno?
049900121012           when  dsp_aid = c_F12  and  $Called;
050000121012             $Fine = *on;
050100121012             //TB39fxx = 'F12';
050200121012
050300121012           // -?F13=Ripetizione Opzione?
050400121012           WHEN  dsp_aid = c_F13;
050500121012             exsr  sr_DupOpzS01;
050600121012
050700121012           // -?Invio?
050800121012           OTHER;
050900121012             reset  $ForzaOpz;
051000121012             // -?Gestione opzioni?
051100121012             if  S01nrr > *zero;
051200121012               exsr  sr_OpzS01;
051300121012               if  ErrGenerico;
051400121012                 leavesr;
051500121012               endif;
051600121012             endif;
051700121012             // -?Posizionamento nel subfile?
051800121012             //  ?(da gestire sempre: il subfile si potrebbe esere?
051900121012             //  ?svuotato dopo l'ultimo posizionamento richiesto)?
052000121012             if  (Ord_x_CON  and  C1Ccon <> SavC1Ccon)  or
052100121012                 (Ord_x_DES  and  C1Cdes <> SavC1Cdes)  or
052200121012                 (Ord_x_TIP  and  C1Ctip <> SavC1Ctip)  or
052300121012                 (Ord_x_MBL  and  C1Cmbl <> SavC1Cmbl)  or
052400121012                 (Ord_x_TBL  and  C1Ctbl <> SavC1Ctbl)  or
052500121012                 (Ord_x_BLC  and  C1Cblc <> SavC1Cblc)  or
052600121012                 (Ord_x_MTB  and  C1Cmtb <> SavC1Cmtb)  or
052700121012                 (Ord_x_PBL  and  C1Cpbl <> SavC1Cpbl)  or
052800121012                 (Ord_x_CDP  and  C1Ccdp <> SavC1Ccdp)  or
052900121012                 (Ord_x_MBP  and  C1Cmbp <> SavC1Cmbp);
053000121012               $InzS01 = *on;
053100121012             endif;
053200121012
053300121012         ENDSL;
053400121012
053500121012       ENDSR;
053600121012
053700121012       //--------------------------------------------------------------
053800121012       //?Inizializzazione subfile S01.                                ?
053900121012       //--------------------------------------------------------------
054000121012       BEGSR  sr_InzS01;
054100121012
054200121012         // -?Spegnimento degli indicatori di errore?
054300121012         %subst(IndDspF : 60) = *off;
054400121015
054500121015         // -?Memorizzazione posizionamento effettuato?
054600121015         Select;
054700121015           //When  dsp_aid = c_F05;
054800121015           // ?(nulla: già fatto!)?
054900121015           When  wOrdSfl = 0;
055000121015             SavC1Ccon = C1Ccon;
055100121015             clear  SavC1Cdes;
055200121015             clear  SavC1Ctip;
055300121015             clear  SavC1Cmbl;
055400121015             clear  SavC1Ctbl;
055500121015             clear  SavC1Cblc;
055600121015             clear  SavC1Cmtb;
055700121015             clear  SavC1Cpbl;
055800121015             clear  SavC1Ccdp;
055900121015             clear  SavC1Cmbp;
056000121015           When  wOrdSfl = 1;
056100121015             clear  SavC1Ccon;
056200121015             SavC1Cdes = C1Cdes;
056300121015             clear  SavC1Ctip;
056400121015             clear  SavC1Cmbl;
056500121015             clear  SavC1Ctbl;
056600121015             clear  SavC1Cblc;
056700121015             clear  SavC1Cmtb;
056800121015             clear  SavC1Cpbl;
056900121015             clear  SavC1Ccdp;
057000121015             clear  SavC1Cmbp;
057100121015           When  wOrdSfl = 2;
057200121015             clear  SavC1Ccon;
057300121015             clear  SavC1Cdes;
057400121015             SavC1Ctip = C1Ctip;
057500121015             clear  SavC1Cmbl;
057600121015             clear  SavC1Ctbl;
057700121015             clear  SavC1Cblc;
057800121015             clear  SavC1Cmtb;
057900121015             clear  SavC1Cpbl;
058000121015             clear  SavC1Ccdp;
058100121015             clear  SavC1Cmbp;
058200121015           When  wOrdSfl = 3;
058300121015             clear  SavC1Ccon;
058400121015             clear  SavC1Cdes;
058500121015             clear  SavC1Ctip;
058600121015             SavC1Cmbl = C1Cmbl;
058700121015             clear  SavC1Ctbl;
058800121015             clear  SavC1Cblc;
058900121015             clear  SavC1Cmtb;
059000121015             clear  SavC1Cpbl;
059100121015             clear  SavC1Ccdp;
059200121015             clear  SavC1Cmbp;
059300121015           When  wOrdSfl = 4;
059400121015             clear  SavC1Ccon;
059500121015             clear  SavC1Cdes;
059600121015             clear  SavC1Ctip;
059700121015             clear  SavC1Cmbl;
059800121015             SavC1Ctbl = C1Ctbl;
059900121015             clear  SavC1Cblc;
060000121015             clear  SavC1Cmtb;
060100121015             clear  SavC1Cpbl;
060200121015             clear  SavC1Ccdp;
060300121015             clear  SavC1Cmbp;
060400121015           When  wOrdSfl = 5;
060500121015             clear  SavC1Ccon;
060600121015             clear  SavC1Cdes;
060700121015             clear  SavC1Ctip;
060800121015             clear  SavC1Cmbl;
060900121015             clear  SavC1Ctbl;
061000121015             SavC1Cblc = C1Cblc;
061100121015             clear  SavC1Cmtb;
061200121015             clear  SavC1Cpbl;
061300121015             clear  SavC1Ccdp;
061400121015             clear  SavC1Cmbp;
061500121015           When  wOrdSfl = 6;
061600121015             clear  SavC1Ccon;
061700121015             clear  SavC1Cdes;
061800121015             clear  SavC1Ctip;
061900121015             clear  SavC1Cmbl;
062000121015             clear  SavC1Ctbl;
062100121015             clear  SavC1Cblc;
062200121015             SavC1Cmtb = C1Cmtb;
062300121015             clear  SavC1Cpbl;
062400121015             clear  SavC1Ccdp;
062500121015             clear  SavC1Cmbp;
062600121015           When  wOrdSfl = 7;
062700121015             clear  SavC1Ccon;
062800121015             clear  SavC1Cdes;
062900121015             clear  SavC1Ctip;
063000121015             clear  SavC1Cmbl;
063100121015             clear  SavC1Ctbl;
063200121015             clear  SavC1Cblc;
063300121015             clear  SavC1Cmtb;
063400121015             SavC1Cpbl = C1Cpbl;
063500121015             clear  SavC1Ccdp;
063600121015             clear  SavC1Cmbp;
063700121015           When  wOrdSfl = 8;
063800121015             clear  SavC1Ccon;
063900121015             clear  SavC1Cdes;
064000121015             clear  SavC1Ctip;
064100121015             clear  SavC1Cmbl;
064200121015             clear  SavC1Ctbl;
064300121015             clear  SavC1Cblc;
064400121015             clear  SavC1Cmtb;
064500121015             clear  SavC1Cpbl;
064600121015             SavC1Ccdp = C1Ccdp;
064700121015             clear  SavC1Cmbp;
064800121015           When  wOrdSfl = 9;
064900121015             clear  SavC1Ccon;
065000121015             clear  SavC1Cdes;
065100121015             clear  SavC1Ctip;
065200121015             clear  SavC1Cmbl;
065300121015             clear  SavC1Ctbl;
065400121015             clear  SavC1Cblc;
065500121015             clear  SavC1Cmtb;
065600121015             clear  SavC1Cpbl;
065700121015             clear  SavC1Ccdp;
065800121015             SavC1Cmbp = C1Cmbp;
065900121015         EndSl;
066000121012
066100121012         // -?Pulizia subfile?
066200121012         SflDsp_N    = *on;
066300121012         SflDspCtl_N = *on;
066400121012         write  TB39C01;
066500121012         SflDspCtl_N = *off;
066600121012         SflEnd      = *off;
066700121012
066800121012         clear  C1RcdNbr;
066900121012         clear  C1CsrRrn;
067000121012         clear  S01nrr;
067100121012         clear  S01nrrMax;
067200121012         clear  V1Dmsg;
067300121012         ErrGenerico = *off;
067400121012         ErrMessage  = *off;
067500121012         $EoF = *off;
067600121012
067700121012         // -?Caricamento completo (NON per pagina) dei dati nel subfile?
067800121012         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
067900121012         //  ?l'eventuale ordinamento)?
068000121012         //if  TB39cod <> *blank;
068100121012         //  C1Ccon = TB39cod;
068200121012         //endif;
068300121012         k_TBLkut = c_Kut;
068400121012         if  wOrdSfl = *zero  and  C1Ccon <> *blank;
068500121012           k_TBLkey = C1Ccon;
068600121012           setll  %kds(k03tabel00)  TABEL;
068700121012         else;
068800121012           clear  k_TBLkey;
068900121012           setll  %kds(k03tabel00 : 2)  TABEL;
069000121012         endif;
069100121012         reade(N)  %kds(k03tabel00 : 2)  TABEL;
069200121012         $EoF = (%eof(TABEL00F));
069300121012         exsr  sr_CaricaS01;
069400121012
069500121012         // -?Memorizzazione Nrr ultimo record caricato?
069600121012         //  ?(servirà per ripristinarlo DOPO la gestione di?
069700121012         //  ?un'opzione, PRIMA di un nuovo ordinamento del subfile)?
069800121012         S01nrrMax = S01nrr;
069900121012
070000121012         // -?Impaginazione subfile?
070100121012         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
070200121012         if S01nrr > *zero;
070300121012           C1RcdNbr  = 1;
070400121012           C1CsrRrn  = 1;
070500121012         else;
070600121012           clear  C1RcdNbr;
070700121012         endif;
070800121012
070900121012         // -?Ordinamento subfile (se premuto F8)?
071000121012         if  S01nrr > *zero  and  wOrdSfl > *zero;
071100121012           exsr  sr_SortSfl;
071200121012         endif;
071300121012
071400121012         // -?Impostazione richiesta di posizionamento nel subfile?
071500121012         //  ?(già impostata nel caricamento del subfile)?
071600121012         //Ord_x_CON = (S01nrr > *zero  and  wOrdSfl = 0);
071700121012         //Ord_x_DES = (S01nrr > *zero  and  wOrdSfl = 1);
071800121012         //Ord_x_TIP = (S01nrr > *zero  and  wOrdSfl = 2);
071900121012         //Ord_x_MBL = (S01nrr > *zero  and  wOrdSfl = 3);
072000121012         //Ord_x_TBL = (S01nrr > *zero  and  wOrdSfl = 4);
072100121012         //Ord_x_BLC = (S01nrr > *zero  and  wOrdSfl = 5);
072200121012         //Ord_x_MTB = (S01nrr > *zero  and  wOrdSfl = 6);
072300121012         //Ord_x_PBL = (S01nrr > *zero  and  wOrdSfl = 7);
072400121012         //Ord_x_CDP = (S01nrr > *zero  and  wOrdSfl = 8);
072500121012         //Ord_x_MBP = (S01nrr > *zero  and  wOrdSfl = 9);
072600121012
072700121012         // -?Impostazione opzioni a video?
072800121012         reset  $ForzaOpz;
072900121012         clear  $Opz;
073000121015         //If  TB39fun = '1';
073100121015         If  $Called;
073200121015           // -?Opz. "1" = Ricerca e scelta?
073300121015           C1Dopz = c_Opz_S;
073400121015           $Opz(01) = *on;
073500121015         Else;
073600121012           // -?Opz. "2" = Modifica,?
073700121012           //       ?"3" = Copia,?
073800121012           //       ?"4" = Annullamento/Ripristino,?
073900121012           //       ?"5" = Visualizzazione.?
074000121012           C1Dopz = c_Opz_G;
074100121012           $Opz(02) = *on;
074200121012           $Opz(03) = *on;
074300121012           $Opz(04) = *on;
074400121012           $Opz(05) = *on;
074500121015         EndIf;
074600121012
074700121012       ENDSR;
074800121012
074900121012       //--------------------------------------------------------------
075000121012       //?Caricamento completo del subfile S01                         ?
075100121012       //--------------------------------------------------------------
075200121012       BEGSR  sr_CaricaS01;
075300121012
075400121012         clear  S01nrr;
075500121012         SflNxtChg = *off;
075600121012
075700121012         // -?Ciclo di caricamento dati da tab. "CT"?
075800121012         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
075900121012         //  ?l'eventuale ordinamento)?
076000121012         DoW  Not $EoF;
076100121012
076200121012           // -?Selezione singolo record file TABEL00F?
076300121012           exsr  sr_SelezRec;
076400121012           if  $EoF;
076500121012             leave;
076600121012           endif;
076700121012
076800121012           // -?Registrazione singolo record nel subfile?
076900121012           if  $RecOK;
077000121012
077100121012             // ·?Impostazione campi?
077200121012             //ds4W = TBLuni;      ?(già fatto in sr_SelezRec)?
077300121012             clear  TB39S01;
077400121012             S1Ccon  = TBLkey;
077500121012             S1Cdes  = §4Wdes;
077600121012             S1Ctip  = §4Wtip;
077700121012             S1Cmbl  = §4Wmbl;
077800121012             S1Ctbl  = §4Wtbl;
077900121012             S1Cblc  = §4Wblc;
078000121012             S1Cmtb  = §4Wmtb;
078100121012             S1Cpbl  = §4Wpbl;
078200121012             S1Ccdp  = §4Wcdp;
078300121012             S1Cmbp  = §4Wmbp;
078400121012
078500121012             if  TBLflg <> *blank;
078600121012               S1Catb  = 'A';
078700121012             endif;
078800121012
078900121012             // -?Impostazione attributi di visualizzazione?
079000121012             exsr  sr_DefinizOrd;
079100121012
079200121012             // ·?Scrittura record nel subfile?
079300121012             S01nrr += 1;
079400121012             write  TB39S01;
079500121012
079600121012           EndIf;
079700121012
079800121012           // -?Lettura record successivo?
079900121012           reade(N)  %kds(k03tabel00 : 2)  TABEL;
080000121012           $EoF = (%eof(TABEL00F));
080100121012
080200121012         EndDo;
080300121012
080400121012         // -?Visualizzazione del SFL (se ci sono dati)?
080500121012         SflDsp_N = (S01nrr <= *zero);
080600121012
080700121012         // -?Emissione messaggio di segnalazione raggiungimento limite?
080800121012         if  S01nrr >= c_MaxRec   and   Not %eof(TABEL00F);
080900121012           ErrGenerico = *on;
081000121012           ErrMessage  = *on;
081100121012           V1Dmsg = $Msg(01);
081200121012           leavesr;
081300121012         endif;
081400121012
081500121012         // -?Attivazione del SFLEND?
081600121012         SflEnd = ( $EoF );
081700121012
081800121012         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
081900121012         if  S01nrr > *zero;
082000121012           C1RcdNbr = 1;
082100121012         else;
082200121012           clear  C1RcdNbr;
082300121012         endif;
082400121012
082500121012         C1CsrRrn = C1RcdNbr;
082600121012
082700121012       ENDSR;
082800121012
082900121012       //--------------------------------------------------------------
083000121012       //?Selezione del singolo record letto dalla tabella "3EW".      ?
083100121012       //--------------------------------------------------------------
083200121012       BEGSR  sr_SelezRec;
083300121012
083400121012         reset  $RecOK;
083500121012
083600121012         ds4W = TBLuni;
083700121012
083800121012         select;
083900121012
084000121012           // -?Superato il limite massimo di record registrabili nel?
084100121012           //  ?subfile?
084200121012           when  S01nrr >= c_MaxRec;
084300121012             $EoF = *on;
084400121012             leavesr;
084500121012
084600121012           // -?Ricevuto singolo codice tassazione?
084700121012           //when  TB39cod <> *blank  and  TBLkey > TB39cod;
084800121012           //  $EoF = *on;
084900121012           //  leavesr;
085000121012
085100121012           // -?Richiesto posizionamento per Codice stato del credito:?
085200121012           //  ?si scartano i record con Codice inferiore?
085300121012           when  wOrdSfl = 0  and  TBLkey < SavC1Ccon;
085400121012             leavesr;
085500121012
085600121012           // -?Richiesto posizionamento per Descrizione:?
085700121012           //  ?si scartano i record con Descrizione inferiore?
085800121012           when  wOrdSfl = 1  and  §4Wdes < SavC1Cdes;
085900121012             leavesr;
086000121012
086100121012           // -?Richiesto posizionamento per Tipologia:?
086200121012           //  ?si scartano i record con Tipologia inferiore?
086300121012           when  wOrdSfl = 2  and  §4Wtip < SavC1Ctip;
086400121012             leavesr;
086500121012
086600121012           // -?Richiesto posizionamento per S/N mod. stato credito in fil.:?
086700121012           //  ?si scartano i record con Flag inferiore?
086800121012           when  wOrdSfl = 3  and  §4Wmbl < SavC1Cmbl;
086900121012             leavesr;
087000121012
087100121012           // -?Richiesto posizionamento per Tipo blocco servizi automatico:?
087200121012           //  ?si scartano i record con Tipo inferiore?
087300121012           when  wOrdSfl = 4  and  §4Wtbl < SavC1Ctbl;
087400121012             leavesr;
087500121012
087600121012           // -?Richiesto posizionamento per Codice causale blocco clienti:?
087700121012           //  ?si scartano i record con Codice inferiore?
087800121012           when  wOrdSfl = 5  and  §4Wblc < SavC1Cblc;
087900121012             leavesr;
088000121012
088100121012           // -?Richiesto posizionamento per S/N blocco servizi in fil.:?
088200121012           //  ?si scartano i record con Flag inferiore?
088300121012           when  wOrdSfl = 6  and  §4Wmtb < SavC1Cmtb;
088400121012             leavesr;
088500121012
088600121012           // -?Richiesto posizionamento per S/N blocco pagamenti autom.:?
088700121012           //  ?si scartano i record con Flag inferiore?
088800121012           when  wOrdSfl = 7  and  §4Wpbl < SavC1Cpbl;
088900121012             leavesr;
089000121012
089100121012           // -?Richiesto posizionamento per Codice condizione di pagamento:?
089200121012           //  ?si scartano i record con Codice inferiore?
089300121012           when  wOrdSfl = 8  and  §4Wcdp < SavC1Ccdp;
089400121012             leavesr;
089500121012
089600121012           // -?Richiesto posizionamento per S/N mod. blocco pagam. in fil.:?
089700121012           //  ?si scartano i record con Flag inferiore?
089800121012           when  wOrdSfl = 9  and  §4Wmbp < SavC1Cmbp;
089900121012             leavesr;
090000121012
090100121012         endsl;
090200121012
090300121012         $RecOK = *on;
090400121012
090500121012       ENDSR;
090600121012
090700121012       //--------------------------------------------------------------
090800121012       //?Abilitazione tasti funzionali in C01.                        ?
090900121012       //--------------------------------------------------------------
091000121012       BEGSR  sr_AbilFxxS01;
091100121012
091200121012         // -?F3 = Fine?
091300121012         F3Attivo  = (Not $Called);
091400121012
091500121012         // -?F5 = Refresh?
091600121012         F5Attivo  = *on;
091700121012
091800121012         // -?F8 = Ordinamento x ...?
091900121012         F8Attivo  = (S01nrr > *zero);
092000121012
092100121012         // -?Impostazione F8 nel piede del subfile?
092200121012         select;
092300121012           when  Not F8Attivo;
092400121012             clear  V1ZF08;
092500121012           when  Ord_x_CON;
092600121012             V1ZF08 = 'F8=Ord. x Descriz.  ';
092700121012           when  Ord_x_DES;
092800121012             V1ZF08 = 'F8=Ord. x Tipologia ';
092900121012           when  Ord_x_TIP;
093000121015             V1ZF08 = 'F8=Ord.x StCred.ModF';
093100121012           when  Ord_x_MBL;
093200121012             V1ZF08 = 'F8=Ord. x BloccoServ';
093300121012           when  Ord_x_TBL;
093400121012             V1ZF08 = 'F8=Ord. x Cau.Blo.Cl';
093500121012           when  Ord_x_BLC;
093600121015             V1ZF08 = 'F8=Ord.x BlSrv.Mod.F';
093700121012           when  Ord_x_MTB;
093800121012             V1ZF08 = 'F8=Ord. x BloccoPag.';
093900121012           when  Ord_x_PBL;
094000121012             V1ZF08 = 'F8=Ord. x Cond.Pag. ';
094100121012           when  Ord_x_CDP;
094200121015             V1ZF08 = 'F8=Ord.x BlPag.Mod.F';
094300121012           when  Ord_x_MBP;
094400121015           //  V1ZF08 = 'F8=Ord. x CreaAnCont';
094500121015             V1ZF08 = 'F8=Ord. x Stato Cred';
094600121012           //when  Ord_x_CTZ;
094700121015           //  V1ZF08 = 'F8=Ord. x Stato Cred';
094800121012         endsl;
094900121012
095000121012         // -?F10 = Inserimento?
095100121012         //F10Attivo = (TB39fun <> '1');
095200121012         F10Attivo  = (Not $Called);
095300121012
095400121012         // -?F12 = Ritorno?
095500121012         F12Attivo = ($Called);
095600121012
095700121012         // -?F13 = Ripetizione?
095800121012         //F13Attivo = (TB39fun <> '1'  and  S01nrrMax > 1);
095900121015         F13Attivo = (Not $Called  and  S01nrrMax > 1);
096000121012
096100121012       ENDSR;
096200121012
096300121012       //--------------------------------------------------------------
096400121012       //?Gestione tasto funzionale F8 da videata C01 / S01            ?
096500121012       //?F8=Ordinamento subfile                                       ?
096600121012       //--------------------------------------------------------------
096700121012       BEGSR  sr_OrdS01;
096800121012
096900121012         // -?Subfile vuoto: nessun record da ordinare?
097000121012         if  S01nrrMax = *zero;
097100121012           leavesr;
097200121012         endif;
097300121012
097400121012         // -?REimpostazione del totale Nrr del subfile?
097500121012         //  ?(potrebbe essere qullo del rec. con l'ultima opzione?
097600121012         //  ?elaborata)?
097700121012         S01nrr = S01nrrMax;
097800121012
097900121012         // -?Comunque: bisogna riordinarlo?
098000121012         if  wOrdSfl < 9;
098100121012           wOrdSfl += 1;
098200121012         else;
098300121012           clear  wOrdSfl;
098400121012         endif;
098500121012
098600121012         // -?Impostazione richiesta di posizionamento nel subfile?
098700121012         exsr  sr_DefinizOrd;
098800121012
098900121012         // -?Pulizia campi di posizionamento?
099000121012         select;
099100121012           when  wOrdSfl = 0;
099200121012             clear  C1Ccon;
099300121012           when  wOrdSfl = 1;
099400121012             clear  C1Cdes;
099500121012           when  wOrdSfl = 2;
099600121012             clear  C1Ctip;
099700121012           when  wOrdSfl = 3;
099800121012             clear  C1Cmbl;
099900121012           when  wOrdSfl = 4;
100000121012             clear  C1Ctbl;
100100121012           when  wOrdSfl = 5;
100200121012             clear  C1Cblc;
100300121012           when  wOrdSfl = 6;
100400121012             clear  C1Cmtb;
100500121012           when  wOrdSfl = 7;
100600121012             clear  C1Cpbl;
100700121012           when  wOrdSfl = 8;
100800121012             clear  C1Ccdp;
100900121012           when  wOrdSfl = 9;
101000121012             clear  C1Cmbp;
101100121012         endsl;
101200121012
101300121012         // -?Subfile già posizionato: occorre ricaricarlo?
101400121012         if  SavC1Ccon <> *blank  or  SavC1Cdes <> *blank  or
101500121012             SavC1Ctip <> *blank  or  SavC1Cmbl <> *blank  or
101600121012             SavC1Ctbl <> *blank  or  SavC1Cblc <> *blank  or
101700121012             SavC1Cmtb <> *blank  or  SavC1Cpbl <> *blank  or
101800121012             SavC1Ccdp <> *blank  or  SavC1Cmbp <> *blank;
101900121012             //                   or  SavC1Cctz <> *blank;
102000121012           $InzS01 = *on;
102100121012         else;
102200121012           exsr  sr_SortSfl;
102300121012         endif;
102400121012
102500121012       ENDSR;
102600121012
102700121012       //--------------------------------------------------------------
102800121012       //?Ordinamento subfile S01.                                     ?
102900121012       //?» This subroutine sorts the subfile records. «               ?
103000121012       //--------------------------------------------------------------
103100121012       BEGSR  sr_SortSfl;
103200121012
103300121012         RrnLast  = S01nrr;
103400121012
103500121012         C1RcdNbr = 1;
103600121012         clear  C1CsrRrn;
103700121012         clear  S01nrr;
103800121012         clear  V1Dmsg;
103900121012         ErrGenerico = *off;
104000121012         ErrMessage  = *off;
104100121012         SflNxtChg   = *off;
104200121012         %subst(IndDspF : 60) = *off;
104300121012
104400121012         //?___________________________________________________________?
104500121012         //?Initialize the key fields to sort on.?
104600121012         //?There is one extra field not in the subfile -- Selected --?
104700121012         //?that is added to the record. This field is used to place?
104800121012         //?selected records in front of nonselected records.?
104900121012         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
105000121012
105100121012         clear  QLGKL;
105200121012         clear  QLGSKL;
105300121012         clear  QLGSCB;
105400121012         clear  QLGSCB00;
105500121012
105600121012         // -?Gestione della scelta ordinamento:?
105700121012         Select;
105800121012
105900121012           // ·?Ordinamento per Codice Stato del Credito?
106000121012           When  Ord_x_CON;
106100121012             // ·?n° campi chiave?
106200121012             QLGNBRK  = 1;
106300121012             // ·?1° campo: codice stato del credito?
106400121012             QLGSP    = 1;
106500121012             QLGSS    = %size(S1Ccon);
106600121012             QLGDT    = Carattere;
106700121012             QLGSO    = Ascendente;
106800121012             QLGKL(1) = QLGSKL;
106900121012
107000121012           // ·?Ordinamento per Descrizione Stato del Credito?
107100121012           When  Ord_x_DES;
107200121012             // ·?n° campi chiave?
107300121012             QLGNBRK  = 2;
107400121012             // ·?1° campo: descrizione stato del credito?
107500121012             QLGSP    = %size(S1Ccon) + 1;
107600121012             QLGSS    = %size(S1Cdes);
107700121012             QLGDT    = Carattere;
107800121012             QLGSO    = Ascendente;
107900121012             QLGKL(1) = QLGSKL;
108000121012             // ·?2° campo: codice stato del credito?
108100121012             QLGSP    = 1;
108200121012             QLGSS    = %size(S1Ccon);
108300121012             QLGDT    = Carattere;
108400121012             QLGSO    = Ascendente;
108500121012             QLGKL(2) = QLGSKL;
108600121012
108700121012           // ·?Ordinamento per Tipologia?
108800121012           When  Ord_x_TIP;
108900121012             // ·?n° campi chiave?
109000121012             QLGNBRK  = 2;
109100121012             // ·?1° campo: tipologia?
109200121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + 1;
109300121012             QLGSS    = %size(S1Ctip);
109400121012             QLGDT    = Carattere;
109500121012             QLGSO    = Ascendente;
109600121012             QLGKL(1) = QLGSKL;
109700121012             // ·?2° campo: codice stato del credito?
109800121012             QLGSP    = 1;
109900121012             QLGSS    = %size(S1Ccon);
110000121012             QLGDT    = Carattere;
110100121012             QLGSO    = Ascendente;
110200121012             QLGKL(2) = QLGSKL;
110300121012
110400121012           // ·?Ordinamento per Flag stato del credito modificabile in fil.?
110500121012           When  Ord_x_MBL;
110600121012             // ·?n° campi chiave?
110700121012             QLGNBRK  = 2;
110800121012             // ·?1° campo: flag?
110900121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
111000121012                        1;
111100121012             QLGSS    = %size(S1Cmbl);
111200121012             QLGDT    = Carattere;
111300121012             QLGSO    = Ascendente;
111400121012             QLGKL(1) = QLGSKL;
111500121012             // ·?2° campo: codice stato del credito?
111600121012             QLGSP    = 1;
111700121012             QLGSS    = %size(S1Ccon);
111800121012             QLGDT    = Carattere;
111900121012             QLGSO    = Ascendente;
112000121012             QLGKL(2) = QLGSKL;
112100121012
112200121012           // ·?Ordinamento per Blocco dei servizi automatico?
112300121012           When  Ord_x_TBL;
112400121012             // ·?n° campi chiave?
112500121012             QLGNBRK  = 2;
112600121012             // ·?1° campo: blocco dei srvizi automatico?
112700121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
112800121012                        %size(S1Cmbl) + 1;
112900121012             QLGSS    = %size(S1Ctbl);
113000121012             QLGDT    = Carattere;
113100121012             QLGSO    = Ascendente;
113200121012             QLGKL(1) = QLGSKL;
113300121012             // ·?2° campo: codice stato del credito?
113400121012             QLGSP    = 1;
113500121012             QLGSS    = %size(S1Ccon);
113600121012             QLGDT    = Carattere;
113700121012             QLGSO    = Ascendente;
113800121012             QLGKL(2) = QLGSKL;
113900121012
114000121012           // ·?Ordinamento per Cod. causale blocco automatico clienti?
114100121012           When  Ord_x_BLC;
114200121012             // ·?n° campi chiave?
114300121012             QLGNBRK  = 2;
114400121012             // ·?1° campo: cod. causale blocco automatico clienti?
114500121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
114600121012                        %size(S1Cmbl) + %size(S1Ctbl) + 1;
114700121012             QLGSS    = %size(S1Cblc);
114800121012             QLGDT    = Carattere;
114900121012             QLGSO    = Ascendente;
115000121012             QLGKL(1) = QLGSKL;
115100121012             // ·?2° campo: codice stato del credito?
115200121012             QLGSP    = 1;
115300121012             QLGSS    = %size(S1Ccon);
115400121012             QLGDT    = Carattere;
115500121012             QLGSO    = Ascendente;
115600121012             QLGKL(2) = QLGSKL;
115700121012
115800121012           // ·?Ordinamento per Flag blocco servizi modificabile in fil.?
115900121012           When  Ord_x_MTB;
116000121012             // ·?n° campi chiave?
116100121012             QLGNBRK  = 2;
116200121012             // ·?1° campo: cod. causale blocco automatico clienti?
116300121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
116400121012                        %size(S1Cmbl) + %size(S1Ctbl) + %size(S1Cblc) +
116500121012                        1;
116600121012             QLGSS    = %size(S1Cmtb);
116700121012             QLGDT    = Carattere;
116800121012             QLGSO    = Ascendente;
116900121012             QLGKL(1) = QLGSKL;
117000121012             // ·?2° campo: codice stato del credito?
117100121012             QLGSP    = 1;
117200121012             QLGSS    = %size(S1Ccon);
117300121012             QLGDT    = Carattere;
117400121012             QLGSO    = Ascendente;
117500121012             QLGKL(2) = QLGSKL;
117600121012
117700121012           // ·?Ordinamento per Flag blocco pagamenti automatico?
117800121012           When  Ord_x_PBL;
117900121012             // ·?n° campi chiave?
118000121012             QLGNBRK  = 2;
118100121012             // ·?1° campo: flag blocco pagamenti automatico?
118200121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
118300121012                        %size(S1Cmbl) + %size(S1Ctbl) + %size(S1Cblc) +
118400121012                        %size(S1Cmtb) + 1;
118500121012             QLGSS    = %size(S1Cpbl);
118600121012             QLGDT    = Carattere;
118700121012             QLGSO    = Ascendente;
118800121012             QLGKL(1) = QLGSKL;
118900121012             // ·?2° campo: codice stato del credito?
119000121012             QLGSP    = 1;
119100121012             QLGSS    = %size(S1Ccon);
119200121012             QLGDT    = Carattere;
119300121012             QLGSO    = Ascendente;
119400121012             QLGKL(2) = QLGSKL;
119500121012
119600121012           // ·?Ordinamento per Condizione di pagamento automatica?
119700121012           When  Ord_x_CDP;
119800121012             // ·?n° campi chiave?
119900121012             QLGNBRK  = 2;
120000121012             // ·?1° campo: flag blocco pagamenti automatico?
120100121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
120200121012                        %size(S1Cmbl) + %size(S1Ctbl) + %size(S1Cblc) +
120300121012                        %size(S1Cmtb) + %size(S1Cpbl) + 1;
120400121012             QLGSS    = %size(S1Ccdp);
120500121012             QLGDT    = Carattere;
120600121012             QLGSO    = Ascendente;
120700121012             QLGKL(1) = QLGSKL;
120800121012             // ·?2° campo: codice stato del credito?
120900121012             QLGSP    = 1;
121000121012             QLGSS    = %size(S1Ccon);
121100121012             QLGDT    = Carattere;
121200121012             QLGSO    = Ascendente;
121300121012             QLGKL(2) = QLGSKL;
121400121012
121500121012           // ·?Ordinamento per Flag blocco pagamenti modificabile in fil.?
121600121012           When  Ord_x_MBP;
121700121012             // ·?n° campi chiave?
121800121012             QLGNBRK  = 2;
121900121012             // ·?1° campo: flag blocco pagamenti automatico?
122000121012             QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
122100121012                        %size(S1Cmbl) + %size(S1Ctbl) + %size(S1Cblc) +
122200121012                        %size(S1Cmtb) + %size(S1Cpbl) + %size(S1Ccdp) +
122300121012                        1;
122400121012             QLGSS    = %size(S1Cmbp);
122500121012             QLGDT    = Carattere;
122600121012             QLGSO    = Ascendente;
122700121012             QLGKL(1) = QLGSKL;
122800121012             // ·?2° campo: codice stato del credito?
122900121012             QLGSP    = 1;
123000121012             QLGSS    = %size(S1Ccon);
123100121012             QLGDT    = Carattere;
123200121012             QLGSO    = Ascendente;
123300121012             QLGKL(2) = QLGSKL;
123400121012
123500121012           // ·?Ordinamento per Flag creazione anagrafica contenzioso?
123600121012           //When  Ord_x_CTZ;
123700121012           //  // ·?n° campi chiave?
123800121012           //  QLGNBRK  = 2;
123900121012           //  // ·?1° campo: flag blocco pagamenti automatico?
124000121012           //  QLGSP    = %size(S1Ccon) + %size(S1Cdes) + %size(S1Ctip) +
124100121012           //             %size(S1Cmbl) + %size(S1Ctbl) + %size(S1Cblc) +
124200121012           //             %size(S1Cmtb) + %size(S1Cpbl) + %size(S1Ccdp) +
124300121012           //             %size(S1Cmbp) + 1;
124400121012           //  QLGSS    = %size(S1Cctz);
124500121012           //  QLGDT    = Carattere;
124600121012           //  QLGSO    = Ascendente;
124700121012           //  QLGKL(1) = QLGSKL;
124800121012           //  // ·?2° campo: codice stato del credito?
124900121012           //  QLGSP    = 1;
125000121012           //  QLGSS    = %size(S1Ccon);
125100121012           //  QLGDT    = Carattere;
125200121012           //  QLGSO    = Ascendente;
125300121012           //  QLGKL(2) = QLGSKL;
125400121012
125500121012         EndSl;
125600121012
125700121012         // -?Load other sort parameters.?
125800121012         QLGLB   = 80 + 16 * MaxKey;
125900121012         QLGRL   = %size(SflRcd) - 1;
126000121012         QLGRT   = 8;
126100121012         QLGOKL  = 80;
126200121012         QLGLKE  = 16;
126300121012         QLGLSS  = 290;
126400121012         // -?Initialize Sort I/O API fields.?
126500121012         QLGRL00 = QLGRL;
126600121012         QLGRC00 = 1;
126700121012         clear  QUSEI;
126800121012         QUSBPRV = %size(QUSEC);
126900121012
127000121012         // -?First step - Initialize the sort routine.?
127100121012         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
127200121012                      SizeList : ReturnSize : QUSEC );
127300121012
127400121012         // -?Next step - Write records to I/O routine.?
127500121012         QLGRT00  = Put;
127600121012         For  Nrr = 1  To  RrnLast;
127700121012           chain  Nrr  TB39S01;
127800121012           // -?Solo le righe con Selected = 'Y' sono riordinate,?
127900121012           //  ?quindi per fare un ordinamento di tutte le righe?
128000121012           //  ?metto 'Y' sempre.?
128100121012           Selected = 'Y';
128200121012           clear  QUSEI;
128300121012           QUSBPRV  = %size(QUSEC);
128400121012           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
128500121012                         SizeList : NotUsed : QUSEC );
128600121012         EndFor;
128700121012
128800121012         // -?Next step - Signal end of input, clear subfile for reload.?
128900121012         QLGRT00 = EndPut;
129000121012         clear  QUSEI;
129100121012         QUSBPRV = %size(QUSEC);
129200121012         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
129300121012                       SizeList : NotUsed : QUSEC );
129400121012
129500121012         // -?Pulizia subfile?
129600121012         SflDsp_N    = *on;
129700121012         SflDspCtl_N = *on;
129800121012         write  TB39C01;
129900121012         SflDspCtl_N = *off;
130000121012         SflEnd      = *off;
130100121012
130200121012         // -?Final step - Write the records back to the subfile.?
130300121012         QLGRT00  = Get;
130400121012         For  Nrr = 1  To RrnLast;
130500121012           clear  QUSEI;
130600121012           QUSBPRV = %size(QUSEC);
130700121012           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
130800121012                         QLGRL00  : NotUsed : QUSEC );
130900121012           // -?Ripristino indicatori utilizzati nel sfl rec?
131000121012           SflNxtChg = (S1Copz <> *zero);
131100121012           exsr  sr_DefinizOrd;
131200121012           // -?Scrivo record nel subfile?
131300121012           S01nrr = Nrr;
131400121012           write  TB39S01;
131500121012         EndFor;
131600121012
131700121012         C1CsrRrn = 1;
131800121012
131900121012         // -?Visualizzazione del SFL (se ci sono dati)?
132000121012         SflDsp_N = (S01nrr <= *zero);
132100121012
132200121012         // -?Attivazione del SFLEND?
132300121012         SflEnd = $EoF;
132400121012
132500121012       ENDSR;
132600121012
132700121012       //--------------------------------------------------------------
132800121012       //?Definizione ordinamento SubFile (videata C01 / S01)          ?
132900121012       //--------------------------------------------------------------
133000121012       BEGSR  sr_DefinizOrd;
133100121012
133200121012         Ord_x_CON = (wOrdSfl = 0);
133300121012         Ord_x_DES = (wOrdSfl = 1);
133400121012         Ord_x_TIP = (wOrdSfl = 2);
133500121012         Ord_x_MBL = (wOrdSfl = 3);
133600121012         Ord_x_TBL = (wOrdSfl = 4);
133700121012         Ord_x_BLC = (wOrdSfl = 5);
133800121012         Ord_x_MTB = (wOrdSfl = 6);
133900121012         Ord_x_PBL = (wOrdSfl = 7);
134000121012         Ord_x_CDP = (wOrdSfl = 8);
134100121012         Ord_x_MBP = (wOrdSfl = 9);
134200121012
134300121012       ENDSR;
134400121012
134500121012       //--------------------------------------------------------------
134600121012       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
134700121012       //?F13=Ripetizione opzione                                      ?
134800121012       //--------------------------------------------------------------
134900121012       BEGSR  sr_DupOpzS01;
135000121012
135100121012         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
135200121012         //  ?dalla posizione del cursore?
135300121012         wPag = C1CsrRrn / c_SflPag;
135400121012         wRig = %rem( C1CsrRrn : c_SflPag);
135500121012         if  wRig > *zero;
135600121012           wRig = (wPag * c_SflPag) + 1;
135700121012         else;                           // -?ultimo rec. della pag.?
135800121012           wRig = (wPag * c_SflPag) - c_SflPag + 1;
135900121012         endif;
136000121012         SavS01csr = wRig;
136100121012
136200121012         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
136300121012
136400121012         // -?Reperimento della 1ª opzione inserita?
136500121012         //  ?nella pagina dov'è il cursore?
136600121012         readc  TB39S01;
136700121012         DoW  Not %eof(TRTB39D)  and  S1Copz <> *zero  and
136800121012              S01nrr < SavS01csr;
136900121012           SflNxtChg = *on;
137000121012           update  TB39S01;
137100121012           readc  TB39S01;
137200121012         EndDo;
137300121012         if  Not %eof(TRTB39D)  and  S1Copz <> *zero;
137400121012           SflNxtChg = *on;
137500121012           update  TB39S01;
137600121012         endif;
137700121012
137800121012         // -?Nessuna opzione nella pagina?
137900121012         if  %eof(TRTB39D)         or
138000121012             S01nrr <  SavS01csr   or
138100121012             S01nrr > (SavS01csr + c_SflPag - 1);
138200121012           C1RcdNbr  = SavS01csr;
138300121012           C1CsrRrn  = SavS01csr;
138400121012           reset  $ForzaOpz;
138500121012           reset  SavS1Copz;
138600121012           clear  SavS01cs1;
138700121012           clear  SavS01cs2;
138800121012           clear  SavS1Cop1;
138900121012           ErrGenerico = *on;
139000121012           ErrMessage  = *on;
139100121012           PosCurOPZ   = *on;
139200121012           V1Dmsg = $Msg(02);
139300121012           leavesr;
139400121012         endif;
139500121012
139600121012         // -?Memorizzazione 1ª opzione nella pagina?
139700121012         SavS01csr = S01nrr;
139800121012         SavS1Copz = S1Copz;
139900121012         if  SavS01csr <> SavS01cs1  or
140000121012             SavS1Copz <> SavS1Cop1;
140100121012           clear  $ForzaOpz;
140200121012         endif;
140300121012         SavS01cs1 = S01nrr;
140400121012         SavS1Cop1 = S1Copz;
140500121012
140600121012         // -?Reperimento delle altre eventuali opzioni?
140700121012         readc  TB39S01;
140800121012         DOW  Not %eof(TRTB39D);
140900121012
141000121012           // ·?Aggiornamento subfile?
141100121012           if  S1Copz <> *zero;
141200121012             SflNxtChg = *on;
141300121012             update  TB39S01;
141400121012           endif;
141500121012
141600121012           // ·?Reperita una 2ª opzione?
141700121012           Select;
141800121012
141900121012             // ·?Vuota!?
142000121012             When  S1Copz = *zero;
142100121012
142200121012             // ·?1ª opzione nella pagina del cursore  e?
142300121012             //  ?2ª opzione (diversa) in una pagina successiva?
142400121012             //  ?(messaggio di avviso di "ricopertura" - se la 2ª?
142500121012             //  ?opzione è diversa)?
142600121012             When  (SavS01csr >= wRig              and
142700121012                    SavS01csr <  wRig + c_SflPag   and
142800121012                    S01nrr    >= wRig + c_SflPag)  and
142900121012                    S1Copz    <> SavS1Copz         and
143000121012                    Not $ForzaOpz;
143100121012               $ForzaOpz = *on;
143200121012               SavS01cs2 = S01nrr;
143300121012               C1RcdNbr  = SavS01csr;
143400121012               C1CsrRrn  = SavS01csr;
143500121012               ErrGenerico = *on;
143600121012               ErrMessage  = *on;
143700121012               PosCurOPZ   = *on;
143800121012               V1Dmsg = $Msg(03);
143900121012               leavesr;
144000121012
144100121012             // ·?2ª opzione sul record dove è posizionato il cursore?
144200121012             //  ?(la nuova da considerare per la duplicazione)?
144300121012             When  C1CsrRrn = S01nrr;
144400121012               SavS01csr = S01nrr;
144500121012               SavS1Copz = S1Copz;
144600121012
144700121012             // ·?Più opzioni nella stessa videata e?
144800121012             //  ?cursore su nessuna delle opzioni?
144900121012             //  ?(riposizionare il cursore)?
145000121012             //  ?Se stessa opzione in videate diverse: vince la?
145100121012             //  ?posizione della prima opzione.?
145200121012             When  C1CsrRrn <> SavS01csr  and
145300121012                   C1CsrRrn <> S01nrr     and
145400121012                   S01nrr   <  wRig + c_SflPag;
145500121012               SavS01cs2 = S01nrr;
145600121012               C1RcdNbr  = SavS01csr;
145700121012               C1CsrRrn  = SavS01csr;
145800121012               ErrGenerico = *on;
145900121012               ErrMessage  = *on;
146000121012               PosCurOPZ   = *on;
146100121012               V1Dmsg = $Msg(04);
146200121012               leavesr;
146300121012
146400121012           EndSl;
146500121012
146600121012           readc  TB39S01;
146700121012
146800121012         ENDDO;
146900121012
147000121012         // ·?Opzione NON abilitata?
147100121012         if  SavS1Copz  > %elem($Opz)  or
147200121012            (SavS1Copz <= %elem($Opz)  and  $Opz(SavS1Copz) = *off);
147300121012           C1RcdNbr  = SavS01csr;
147400121012           C1CsrRrn  = SavS01csr;
147500121012           ErrGenerico = *on;
147600121012           ErrMessage  = *on;
147700121012           PosCurOPZ   = *on;
147800121012           V1Dmsg = $Msg(05);
147900121012           leavesr;
148000121012         endif;
148100121012
148200121012         // -?Ripetizione opzione sul resto del subfile?
148300121012         S01nrr = SavS01csr + 1;
148400121012         chain  S01nrr  TB39S01;
148500121012         DoW  %found(TRTB39D);
148600121012           SflNxtChg = *on;
148700121012           S1Copz = SavS1Copz;
148800121012           update  TB39S01;
148900121012           S01nrr += 1;
149000121012           chain  S01nrr  TB39S01;
149100121012         EndDo;
149200121012
149300121012       ENDSR;
149400121012
149500121012       //--------------------------------------------------------------
149600121012       //?Gestione opzioni del subfile S01                             ?
149700121012       //--------------------------------------------------------------
149800121012       BEGSR  sr_OpzS01;
149900121012
150000121012         clear  SavS01csr;
150100121012
150200121012         // -?Ciclo di lettura subfile?
150300121012         readc  TB39S01;
150400121012
150500121012         DoW  Not %eof(TRTB39D);
150600121012
150700121012           SflNxtChg = *off;
150800121012           %subst(IndDspF : 60) = *off;
150900121012           C1RcdNbr = S01nrr;
151000121012
151100121012           Select;
151200121012
151300121012             // -?Nessuna opzione?
151400121012             When  S1Copz = *zero;
151500121012
151600121012             // -?Opzione NON abilitata?
151700121012             When  S1Copz  > %elem($Opz)  or
151800121012                  (S1Copz <= %elem($Opz)  and  $Opz(S1Copz) = *off);
151900121012               ErrGenerico = *on;
152000121012               ErrMessage  = *on;
152100121012               PosCurOPZ   = *on;
152200121012               V1Dmsg = $Msg(05);
152300121012
152400121012             // -?Opz. 1 = Selezione.?
152500121012             When  S1Copz = 1  and  Not $Fine;
152600121012               $Fine = *on;
152700121015               //TB39cod = S1Ccon;
152800121012               //TB39des = S1Cdes;
152900121015               %subst(kpjbu : 2) = S1Ccon;
153000121015               %subst(kpjbu : %len(S1Ccon)+2) = S1Cdes;
153100121012
153200121012             // -?Opz. 1 = Selezione già reperita?
153300121012             //  ?(ammessa una sola selezione)?
153400121012             when  S1Copz = 1  and  $Fine;
153500121012               ErrGenerico = *on;
153600121012               ErrMessage  = *on;
153700121012               PosCurOPZ   = *on;
153800121012               V1Dmsg = $Msg(06);
153900121012               $Fine  = *off;
154000121015
154100121015             // -?Opz. 2 = Modifica rec. Annullato?
154200121015             When  S1Copz = 2  and  S1Catb = 'A';
154300121015               ErrGenerico = *on;
154400121015               ErrMessage  = *on;
154500121015               PosCurOPZ   = *on;
154600121015               V1Dmsg = $Msg(07);
154700121012
154800121012             // -?Opz. 2 = Modifica,?
154900121012             //       ?3 = Copia,?
155000121012             //       ?4 = Annullamento / Ripristino,?
155100121012             //       ?5 = Visualizzazione.?
155200121012             When  S1Copz = 2  or
155300121012                   S1Copz = 3  or
155400121012                   S1Copz = 4  or
155500121012                   S1Copz = 5;
155600121015               $Video  = 'D2';
155700121015               $InzD02 = *on;
155800121015               DoW  $Video = 'D2';
155900121015                 exsr  sr_GesD02;
156000121015                 if  $Fine = *on;
156100121015                   ErrGenerico = *on;
156200121015                   leavesr;
156300121015                 endif;
156400121015               EndDo;
156500121015               clear  V1Topz;
156600121015               // -?Se F12=Ritorno: esce dalla lettura del subfile?
156700121015               //if  TB39fxx = 'F12';
156800121015               if  dsp_aid = c_F12;
156900121015                 ErrGenerico = *on;
157000121015               endif;
157100121012
157200121012             // -?Opzione errata?
157300121012             Other;
157400121012               ErrGenerico = *on;
157500121012               ErrMessage  = *on;
157600121012               PosCurOPZ   = *on;
157700121012               V1Dmsg = $Msg(05);
157800121012
157900121012           EndSl;
158000121012
158100121012           // -?Memorizzazione nrr del subfile con la 1ª opzione per?
158200121015           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
158300121012           if  S1Copz  <> *zero   and
158400121012               (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
158500121012             SavS01csr = C1RcdNbr;
158600121012           endif;
158700121012
158800121012           // -?Aggiornamento subfile?
158900121012           select;
159000121012             when  ErrMessage;
159100121012               SflNxtChg = *on;
159200121012               C1CsrRrn  = C1RcdNbr;
159300121012             when  ErrGenerico;
159400121012               C1CsrRrn  = C1RcdNbr;
159500121012               clear  S1Copz;
159600121012             other;
159700121012               clear  S1Copz;
159800121012           endsl;
159900121012
160000121012           UPDATE  TB39S01;
160100121012
160200121012           if  ErrGenerico   or   ErrMessage;
160300121012             leave;
160400121012           endif;
160500121012
160600121012           readc  TB39S01;
160700121012
160800121012         ENDDO;
160900121012
161000121012         // -?Riposiziono il cursore sul record contenente la prima opz.?
161100121012         //  ?(se non sono stati rilevati errori)?
161200121012         if  NOT ErrMessage   and   NOT ErrGenerico   and
161300121012             SavS01csr > *zero;
161400121012           C1CsrRrn = SavS01csr;
161500121012         endif;
161600121012
161700121012       ENDSR;
161800121012
161900121012       //--------------------------------------------------------------
162000121012       //?Gestione videata D02.                                        ?
162100121012       //--------------------------------------------------------------
162200121012       BEGSR  sr_GesD02;
162300121012
162400121012         // -?Inizializzazione videata?
162500121012         if  $InzD02 = *on;
162600121012           exsr  sr_InzD02;
162700121012           $InzD02 = *off;
162800121012         endif;
162900121012
163000121012         // -?Impostazione attributi di visualizzazione?
163100121012         exsr  sr_AttrVisD02;
163200121012
163300121012         // -?(Ri)Abilitazione tasti funzionali?
163400121012         exsr  sr_AbilFxxD02;
163500121012
163600121012         // -?Emissione Testata?
163700121012         write  TB39T01;
163800121012
163900121012         // -?Emissione videata (e piede)?
164000121012         if  (S1Copz = 4  and  TBLflg = *blank)  or
164100121012              S1Copz = 5;
164200121012           write  TB39D02;
164300121012           exfmt  Protect;
164400121012         else;
164500121012           exfmt  TB39D02;
164600121012         endif;
164700121012
164800121012         reset  ErrGenerico;
164900121012         reset  ErrMessage;
165000121012         clear  V1Dmsg;
165100121012
165200121012         //clear  TB39fxx;
165300121012
165400121012         select;
165500121012
165600121012           // -?F3 = Fine?
165700121012           when  dsp_aid = c_F03;
165800121012             unlock  TABEL00F;
165900121012             $Fine = *on;
166000121012             //TB39fxx = 'F03';
166100121012
166200121012           // -?F8 = Successivo?
166300121012           WHEN  dsp_aid = c_F08;
166400121012             $Video = 'S1';
166500121012
166600121012           // -?F12 = Ritorno?
166700121012           WHEN  dsp_aid = c_F12;
166800121012             unlock  TABEL00F;
166900121012             $Video  = 'S1';
167000121012             //TB39fxx = 'F12';
167100121012
167200121012           // -?Invio, F6 = Conferma?
167300121012           OTHER;
167400121012             // -?Controlli eseguiti SE NON richiesto annullamento?
167500121015             if  S1Copz <> 4  or
167600121015                 (S1Copz = 4  and  TBLflg <> *blank);
167700121012               exsr  sr_CtrD02;
167800121012               if  ErrGenerico;
167900121012                 leavesr;
168000121012               endif;
168100121012             endif;
168200121012
168300121012             // -?F6 = Aggiornamento?
168400121012             IF  dsp_aid = c_F06;
168500121012
168600121012               // -?Richiesta dati di trasmissione?
168700121012               $Video  = 'W1';
168800121012               $InzW01 = *on;
168900121012               DoW  $Video = 'W1';
169000121012                 exsr  sr_GesW01;
169100121012               EndDo;
169200121012
169300121012             ENDIF;
169400121012
169500121012         ENDSL;
169600121012
169700121012       ENDSR;
169800121012
169900121012       //--------------------------------------------------------------
170000121012       //?Inizializzazione videata D02.                                ?
170100121012       //--------------------------------------------------------------
170200121012       BEGSR  sr_InzD02;
170300121012
170400121012         clear  TB39D02;
170500121012
170600121012         // -?Reperimento dati tabella (SE NON inserimento)?
170700121012         clear  ds4W;
170800121012         if  Not $Inserim;
170900121012           k_TBLkut = c_Kut;
171000121012           k_TBLkey = S1Ccon;
171100121012           if  S1Copz = 3  or  S1Copz = 5;
171200121012             chain(N)  %kds(k03tabel00)  TABEL;
171300121012           else;
171400121012             chain  %kds(k03tabel00)  TABEL;
171500121012           endif;
171600121012           if  %found(TABEL00F);
171700121012             ds4W = TBLuni;
171800121012           endif;
171900121012         endif;
172000121012
172100121012         // -?Impostazione testata?
172200121012         clear  V1Topz;
172300121012         select;
172400121012           when  $Inserim;
172500121012             //V1Topz = '  IMMISSIONE   ';
172600121012             V1Topz = '  INSERIMENTO  ';
172700121012             leavesr;
172800121012           when  S1Copz = 2;
172900121012             V1Topz = '   MODIFICA    ';
173000121012           when  S1Copz = 3;
173100121012             V1Topz = '     COPIA     ';
173200121012           when  S1Copz = 4  and  TBLflg = *blank;
173300121012             V1Topz = ' ANNULLAMENTO  ';
173400121012           when  S1Copz = 4  and  TBLflg = '*';
173500121012             V1Topz = '  RIPRISTINO   ';
173600121012           when  S1Copz = 5;
173700121012             V1Topz = 'VISUALIZZAZIONE';
173800121012           other;
173900121012             V1Topz = ' ? ? ? ? ? ? ? ';
174000121012         endsl;
174100121012
174200121012         // -?Impostazione dettaglio?
174300121015         if  S1Copz <> 3;
174400121015           V1Ccon = TBLkey;
174500121015         endif;
174600121012         V1Cdes = §4Wdes;
174700121012         V1Ctip = §4Wtip;
174800121012         V1Cmbl = §4Wmbl;
174900121012         V1Ctbl = §4Wtbl;
175000121012         V1Cblc = §4Wblc;
175100121012         V1Cmtb = §4Wmtb;
175200121012         V1Cpbl = §4Wpbl;
175300121012         V1Ccdp = §4Wcdp;
175400121012         V1Cmbp = §4Wmbp;
175500121012         V1Cctz = §4Wctz;
175600130220         V1Cbkam = §4Wpjbkamm;
175700121012
175800121012         // -?Decodifica Causale blocco automatico clienti?
175900121012         clear dsBI;
176000121012         if  §4Wblc <> *blank;
176100121012           ds_TNTBE.TBEke1 = §4Wblc;
176200121012           if  getTabella ( c_Tabel : 'BI'  : ds_TNTBE.TBEke1 :
176300121012                            *omit   : *omit : *omit :
176400121012                            *omit : *omit :
176500121012                            *omit : *omit : *omit : *omit :
176600121012                            *omit : *omit : *omit : *omit :
176700121012                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
176800121012                          = *zero      AND
176900121012               ds_TNTBE.TBEatb = *blank;
177000121012             dsBI   = ds_TNTBE.TBEuni;
177100121012           else;
177200121012             §BIdes = *all'? ';
177300121012           endif;
177400121012         EndIf;
177500121012         V1Dblc = §BIdes;
177600121012
177700121012         // -?Decodifica Condizione di Pagamento automatica?
177800121012         clear dsFA;
177900121012         If  §4Wcdp <> *blank;
178000121015           ds_TNTBE.TBEke1 = §4Wcdp + '1';
178100121012           if  getTabella ( c_Tabel : 'FA'  : ds_TNTBE.TBEke1 :
178200121012                            *omit   : *omit : *omit :
178300121012                            *omit : *omit :
178400121012                            *omit : *omit : *omit : *omit :
178500121012                            *omit : *omit : *omit : *omit :
178600121012                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
178700121012                          = *zero      AND
178800121012               ds_TNTBE.TBEatb = *blank;
178900121012             dsFA   = ds_TNTBE.TBEuni;
179000121012           else;
179100121012             §FAdes = *all'? ';
179200121012           endif;
179300121012         EndIf;
179400121012         V1Dcdp = §FAdes;
179500121012
179600121012       ENDSR;
179700121012
179800121012       //--------------------------------------------------------------
179900121012       //?Impostazione attributi di visualizzazione in D02.            ?
180000121012       //--------------------------------------------------------------
180100121012       BEGSR  sr_AttrVisD02;
180200121012
180300121012         // -?Protezione campo chiave (SE NON immissione o copia)?
180400121012         $Protect_Key = (Not $Inserim  and  S1Copz <> 3);
180500121012
180600121012         // -?Codice annullato in tabella?
180700121015         $Cod_Annull  = ($Protect_Key      and
180800121015                         %found(TABEL00F)  and  TBLflg <> *blank);
180900121012
181000121012       ENDSR;
181100121012
181200121012       //--------------------------------------------------------------
181300121012       //?Abilitazione tasti funzionali in D02.                        ?
181400121012       //--------------------------------------------------------------
181500121012       BEGSR  sr_AbilFxxD02;
181600121012
181700121012         // -?F3 = Fine?
181800121012         F3Attivo  = *on;
181900121012
182000121012         // -?F6 = Conferma?
182100121012         F6Attivo  = (S1Copz <> 5);
182200121012
182300121012         // -?F8 = Successivo?
182400121012         F8Attivo  = (Not $Inserim);
182500121012
182600121012         // -?F12 = Ritorno?
182700121012         F12Attivo = *on;
182800121012
182900121012       ENDSR;
183000121012
183100121012       //--------------------------------------------------------------
183200121012       //?Controllo dati nella videata D02.                            ?
183300121012       //--------------------------------------------------------------
183400121012       BEGSR  sr_CtrD02;
183500121012
183600121012         %subst(IndDspF : 60) = *off;
183700121012         k_TBLkut = c_Kut;
183800121012         //k_TBLcod = c_Tab;       ?(già così)?
183900121012         k_TBLkey = V1Ccon;
184000121012
184100121012         // -?Codice Stato del credito:?
184200121012         Select;
184300121012           // ·?NON obbligatorio !!!?
184400121012           //When  V1Ccon = *blank;
184500121012           //  ErrGenerico = *on;
184600121012           //  ErrMessage  = *on;
184700121012           //  PosCurCON   = *on;
184800121015           //  V1Dmsg = $Msg(08);
184900121012           //  leavesr;
185000121012           // ·?SE immissione o copia: controllo inesistenza nuovo?
185100121012           When  $Inserim  or  S1Copz = 3;
185200121012             setll  %kds(k03tabel00)  TABEL;
185300121012             if  %equal(TABEL00F);
185400121012               ErrGenerico = *on;
185500121012               ErrMessage  = *on;
185600121012               PosCurCON   = *on;
185700121015               V1Dmsg = $Msg(09);
185800121012               leavesr;
185900121012             endif;
186000121012         EndSl;
186100121012
186200121012         // -?Descrizione: obbligatoria?
186300121012         if  V1Cdes = *blank;
186400121012           ErrGenerico = *on;
186500121012           ErrMessage  = *on;
186600121012           PosCurDES   = *on;
186700121015           V1Dmsg = $Msg(10);
186800121012           leavesr;
186900121012         endif;
187000121012
187100121012         // -?Tipologia:?
187200121012         select;
187300121012           // ·?NON obbligatoria?
187400121012           //when  V1Ctip = *blank;
187500121012           //  ErrGenerico = *on;
187600121012           //  ErrMessage  = *on;
187700121012           //  PosCurTIP   = *on;
187800121015           //  V1Dmsg = $Msg(11);
187900121012           //  leavesr;
188000121012           // ·?errata?
188100121012           when  V1Ctip <> *blank  and
188200121012                 V1Ctip <> 'S'     and
188300121012                 V1Ctip <> 'C'     and
188400121012                 V1Ctip <> 'F'     and
188500121012                 V1Ctip <> 'P'     and
188600121012                 V1Ctip <> 'T';
188700121012             ErrGenerico = *on;
188800121012             ErrMessage  = *on;
188900121012             PosCurTIP   = *on;
189000121015             V1Dmsg = $Msg(12);
189100121012             leavesr;
189200121012         endsl;
189300121012
189400121012         // -?Stato del credito modificabile in filiale: errato?
189500121012         if  V1Cmbl <> 'S'  and  V1Cmbl <> 'N';
189600121012           ErrGenerico = *on;
189700121012           ErrMessage  = *on;
189800121012           PosCurMBL   = *on;
189900121015           V1Dmsg = $Msg(13);
190000121012           leavesr;
190100121012         endif;
190200121012
190300121012         // -?Tipo blocco automatico dei servizi ai clienti: errato?
190400121012         if  V1Ctbl <> *blank  and  V1Ctbl <> '8'  and  V1Ctbl <> '*';
190500121012           ErrGenerico = *on;
190600121012           ErrMessage  = *on;
190700121012           PosCurTBL   = *on;
190800121015           V1Dmsg = $Msg(14);
190900121012           leavesr;
191000121012         endif;
191100121012
191200121012         // -?Causale blocco automatico dei servizi ai clienti:?
191300121015         clear  V1Dblc;
191400121012         // ·?ricerca?
191500121012         If  %scan( '?' : V1Cblc ) > *zero;
191600121015           clear  V1Cblc;
191700121015           TABEL_Ricerca( 'BI' :
191800121015                          *blank :
191900121015                          idElemento :
192000121015                          tastoFunzionaleUscita );
192100121015           if  tastoFunzionaleUscita = *blank;
192200121015             V1Cblc = idElemento;
192300121015           endif;
192400121012           PosCurBLC = *on;
192500121012         EndIf;
192600121015         // ·?richiede il Tipo blocco servizi automatico?
192700121015         Select;
192800121015           When  V1Cblc =  *blank  and  V1Ctbl <> *blank;
192900121015             ErrGenerico = *on;
193000121015             ErrMessage  = *on;
193100121015             PosCurBLC   = *on;
193200121015             V1Dmsg = $Msg(15);
193300121015             leavesr;
193400121015           When  V1Cblc <> *blank  and  V1Ctbl = *blank;
193500121015             ErrGenerico = *on;
193600121015             ErrMessage  = *on;
193700121015             PosCurTBL   = *on;
193800121015             V1Dmsg = $Msg(16);
193900121015             leavesr;
194000121015         EndSl;
194100121012         // ·?errata?
194200121012         If  V1Cblc <> *blank;
194300121012           ds_TNTBE.TBEke1 = V1Cblc;
194400121012           if  getTabella ( c_Tabel : 'BI'  : ds_TNTBE.TBEke1 :
194500121012                            *omit   : *omit : *omit :
194600121012                            *omit : *omit :
194700121012                            *omit : *omit : *omit : *omit :
194800121012                            *omit : *omit : *omit : *omit :
194900121012                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
195000121012                          = *zero      AND
195100121012               ds_TNTBE.TBEatb = *blank;
195200121012             dsBI   = ds_TNTBE.TBEuni;
195300121012             V1Dblc = §BIdes;
195400121012           else;
195500121012             V1Dblc = *all'? ';
195600121012             ErrGenerico = *on;
195700121012             ErrMessage  = *on;
195800121012             PosCurBLC   = *on;
195900121015             V1Dmsg = $Msg(17);
196000121012             leavesr;
196100121012           endif;
196200121012         EndIf;
196300121012
196400121012         // -?Blocco dei servizi modificabile in filiale errato?
196500121012         if  V1Cmtb <> 'S'  and  V1Cmtb <> 'N';
196600121012           ErrGenerico = *on;
196700121012           ErrMessage  = *on;
196800121012           PosCurMTB   = *on;
196900121015           V1Dmsg = $Msg(13);
197000121012           leavesr;
197100121012         endif;
197200121012
197300121012         // -?Blocco dei pagamenti automatico errato?
197400121012         if  V1Cpbl <> 'S'  and  V1Cpbl <> 'N';
197500121012           ErrGenerico = *on;
197600121012           ErrMessage  = *on;
197700121012           PosCurPBL   = *on;
197800121015           V1Dmsg = $Msg(13);
197900121012           leavesr;
198000121012         endif;
198100121012
198200121012         // -?Causale blocco automatico dei servizi ai clienti:?
198300121015         clear  V1Dcdp;
198400121012         // ·?ricerca?
198500121012         If  %scan( '?' : V1Ccdp ) > *zero;
198600121015           //TABEL_Ricerca( 'FA' :
198700121015           //               *blank :
198800121015           //               idElemento :
198900121015           //               tastoFunzionaleUscita );
199000121015           //if  tastoFunzionaleUscita = *blank;
199100121015           //  V1Ccdp = %subst( idElemento : 1 : %len(V1Ccdp) );
199200121015           //endif;
199300121015           clear  V1Ccdp;
199400121015           xcdpagr ( V1Ccdp : V1Dcdp );
199500121015           if  V1Ccdp = '999';
199600121015             clear  V1Ccdp;
199700121015           endif;
199800121012           PosCurCDP = *on;
199900121012         EndIf;
200000121109         // ·?richiede il Blocco dei pagamenti automatico?
200100121109         Select;
200200121109           //When  V1Ccdp =  *blank  and  V1Cpbl = 'S';
200300121109           //  ErrGenerico = *on;
200400121109           //  ErrMessage  = *on;
200500121109           //  PosCurCDP   = *on;
200600121109           //  V1Dmsg = $Msg(20);
200700121109           //  leavesr;
200800121109           When  V1Ccdp <> *blank  and  V1Cpbl = 'N';
200900121109             ErrGenerico = *on;
201000121109             ErrMessage  = *on;
201100121109             PosCurCDP   = *on;
201200121109             V1Dmsg = $Msg(21);
201300121109             leavesr;
201400121109         EndSl;
201500121012         // ·?errata?
201600121012         If  V1Ccdp <> *blank;
201700121015           ds_TNTBE.TBEke1 = V1Ccdp + '1';
201800121012           if  getTabella ( c_Tabel : 'FA'  : ds_TNTBE.TBEke1 :
201900121012                            *omit   : *omit : *omit :
202000121012                            *omit : *omit :
202100121012                            *omit : *omit : *omit : *omit :
202200121012                            *omit : *omit : *omit : *omit :
202300121012                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
202400121012                          = *zero      AND
202500121012               ds_TNTBE.TBEatb = *blank;
202600121012             dsFA   = ds_TNTBE.TBEuni;
202700121012             V1Dcdp = §FAdes;
202800121012           else;
202900121012             V1Dcdp = *all'? ';
203000121012             ErrGenerico = *on;
203100121012             ErrMessage  = *on;
203200121012             PosCurCDP   = *on;
203300121015             V1Dmsg = $Msg(19);
203400121012             leavesr;
203500121012           endif;
203600121012         EndIf;
203700121012
203800121012         // -?Blocco dei pagamenti modificabile in filiale errato?
203900121012         if  V1Cmbp <> 'S'  and  V1Cmbp <> 'N';
204000121012           ErrGenerico = *on;
204100121012           ErrMessage  = *on;
204200121012           PosCurMBP   = *on;
204300121015           V1Dmsg = $Msg(13);
204400121012           leavesr;
204500121012         endif;
204600121012
204700121012         // -?Creazione anagrafica contenzioso errato?
204800121012         if  V1Cctz <> 'S'  and  V1Cctz <> 'N';
204900121012           ErrGenerico = *on;
205000121012           ErrMessage  = *on;
205100121012           PosCurCTZ   = *on;
205200121015           V1Dmsg = $Msg(13);
205300121012           leavesr;
205400121012         endif;
205500121012
205600121012       ENDSR;
205700121012
205800121012       //--------------------------------------------------------------
205900121012       //?Gestione window W01.                                         ?
206000121012       //--------------------------------------------------------------
206100121012       BEGSR  sr_GesW01;
206200121012
206300121012         // -?Inizializzazione videata?
206400121012         if  $InzW01 = *on;
206500121012           exsr  sr_InzW01;
206600121012           $InzW01 = *off;
206700121012         endif;
206800121012
206900121012         // -?Emissione Window?
207000121012         exfmt  TB39W01;
207100121012
207200121012         reset  ErrGenerico;
207300121012         reset  ErrMessage;
207400121012         clear  V1Dmsg;
207500121012
207600121012         select;
207700121012
207800121012           // -?F12 = Ritorno?
207900121012           WHEN  dsp_aid = c_F12;
208000121012             $Video  = 'D2';
208100121012             //TB39fxx = 'F12';
208200121012
208300121012           // -?Invio, F6 = Conferma?
208400121012           OTHER;
208500121012             exsr  sr_CtrW01;
208600121012             if  ErrGenerico;
208700121012               leavesr;
208800121012             endif;
208900121012             // -?F6 = Aggiornamento?
209000121012             If  dsp_aid = c_F06;
209100121012               exsr  sr_UpdTABEL;
209200121012               if  ErrGenerico = *on;
209300121012                 leavesr;
209400121012               endif;
209500121012               // ·?Ritorno al subfile?
209600121012               $Video  = 'S1';
209700121012               $InzS01 = *on;
209800121012               C1Ccon  = SavC1Ccon;
209900121012               C1Cdes  = SavC1Cdes;
210000121012               C1Ctip  = SavC1Ctip;
210100121012               C1Cmbl  = SavC1Cmbl;
210200121012               C1Ctbl  = SavC1Ctbl;
210300121012               C1Cblc  = SavC1Cblc;
210400121012               C1Cmtb  = SavC1Cmtb;
210500121012               C1Cpbl  = SavC1Cpbl;
210600121012               C1Ccdp  = SavC1Ccdp;
210700121012               C1Cmbp  = SavC1Cmbp;
210800121012             EndIf;
210900121012
211000121012         ENDSL;
211100121012
211200121012       ENDSR;
211300121012
211400121012       //--------------------------------------------------------------
211500121012       //?Inizializzazione window W01.                                 ?
211600121012       //--------------------------------------------------------------
211700121012       BEGSR  sr_InzW01;
211800121012
211900121012         clear  TB39W01;
212000121012
212100121012         // -?Se non immissione: imposta i campi del file?
212200121012         If  Not $Inserim;
212300121012
212400121012           W01ftt = TBLftt;
212500121012           W01flt = TBLflt;
212600121012           W01ftr = TBLftr;
212700121012           // ·?Conversione data (se NON immissione o copia)?
212800121012           if  Not $Inserim  and  S1Copz <> 3
212900121012                             and  TBLdtr  > *zero;
213000121012             wDate_DMY = %date(TBLdtr : *ymd);
213100121012             W01dtr    = %dec( wDate_DMY );
213200121012           endif;
213300121012
213400121012         EndIf;
213500121012
213600121012       ENDSR;
213700121012
213800121012       //--------------------------------------------------------------
213900121012       //?Controllo dati nella window W01.                             ?
214000121012       //--------------------------------------------------------------
214100121012       BEGSR  sr_CtrW01;
214200121012
214300121012         %subst(IndDspF : 60) = *off;
214400121012
214500121012       ENDSR;
214600121012
214700121012       //--------------------------------------------------------------
214800121012       //?Aggiornamento record nel file TABEL00F.                      ?
214900121012       //--------------------------------------------------------------
215000121012       BEGSR  sr_UpdTABEL;
215100121012
215200121012         // -?RI-aggancio record da aggiornare?
215300121012         k_TBLkut = c_Kut;
215400121012         //k_TBLcod = c_Tab;       ?(già così)?
215500121012         k_TBLkey = V1Ccon;
215600121012
215700121012         chain  %kds(k03tabel00)  TABEL;
215800121012
215900121012         // -?Impostazione campi-chiave SE record NON trovato?
216000121012         if  Not %found(TABEL00F);
216100121012           clear  TABEL;
216200121012           TBLkut = k_TBLkut;
216300121012           TBLcod = k_TBLcod;
216400121012           TBLkey = k_TBLkey;
216500121012         endif;
216600121012
216700121012
216800121012         // -?Annullamento / Ripristino?
216900121012         If  S1Copz = 4;
217000121012           select;
217100121012             // ·?Annullamento?
217200121012             when  TBLflg = *blank;
217300121012               TBLflg = '*';
217400121012             // ·?Ripristino?
217500121012             when  TBLflg <> *blank;
217600121012               clear  TBLflg;
217700121012           endsl;
217800121012         EndIf;
217900121012
218000121012
218100121012         // -?Riempimento struttura dati ds4W?
218200121012         clear  ds4W;
218300121012
218400121012         §4Wdes = V1Cdes;
218500121012         §4Wtip = V1Ctip;
218600121012         §4Wmbl = V1Cmbl;
218700121012         §4Wtbl = V1Ctbl;
218800121012         §4Wblc = V1Cblc;
218900121012         §4Wmtb = V1Cmtb;
219000121012         §4Wpbl = V1Cpbl;
219100121012         §4Wcdp = V1Ccdp;
219200121012         §4Wmbp = V1Cmbp;
219300121012         §4Wctz = V1Cctz;
219400130220         §4Wpjbkamm = V1Cbkam;
219500121012
219600121012         TBLuni = ds4W;
219700121012
219800121012         TBLftt = W01ftt;
219900121012         TBLflt = W01flt;
220000121012         clear  TBLftr;
220100121012         clear  TBLdtr;
220200121012
220300121012
220400121012         // -?Aggiornamento / Inserimento?
220500121012         If  %found(TABEL00F);
220600121012           //____________
220700121012           update  TABEL;
220800121012           //¯¯¯¯¯¯¯¯¯¯¯¯
220900121012         Else;
221000121012           //____________
221100121012           write   TABEL;
221200121012           //¯¯¯¯¯¯¯¯¯¯¯¯
221300121012         EndIf;
221400121012
221500121012       ENDSR;
221600121012
221700121012       //--------------------------------------------------------------
221800121012       //?Operazioni finali.                                           ?
221900121012       //--------------------------------------------------------------
222000121012       BEGSR  sr_RoutEnd;
222100121012
222200121012         // -?Chiusura applicazioni utilizzate?
222300121012         cloTabella();
222400121012
222500121012         // -?Restituzione parametri al chiamante?
222600121012         //kpjbu = TRTB39ds;
222700121012
222800121012         // -?Uscita dal *pgm?
222900121012         return;
223000121012
223100121012       ENDSR;
223200121012
223300121012      /end-free
223400121012
223500121012       //--------------------------------------------------------------
223600121015       //?Definizione schiere a tempo di compilazione                  ?
223700121012       //--------------------------------------------------------------
223800121012
223900121012** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
224000121012SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)         1
224100121012Immettere un'opzione su questa pagina prima di premere F13                     2
224200121012Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 3
224300121012Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 4
224400121012Opzione errata                                                                 5
224500121012Ammessa la selezione di un solo stato del credito                              6
224600121015Un codice annullato NON è modificabile: occorre ripristinarlo                  7
224700121012Immettere il codice "Stato del Credito"                                       --
224800121015Stato del Credito già esistente                                                9
224900121015Descrizione obbligatoria                                                      10
225000121012Tipologia obbligatoria                                                        --
225100121015Tipologia errata                                                              12
225200121015Campo errato o mancante: S=Sì, N=No                                           13
225300121015Tipo blocco automatico errato                                                 14
225400121015Il "Blocco automatico dei servizi" richiede la "Causale blocco autom. clienti"15
225500121015La "Causale blocco autom. clienti" richiede il "Blocco automatico dei servizi"16
225600121015Causale del blocco automatico dei servizi ai clienti  errata                  17
225700121015Condizione di pagamento automatica  obbligatoria                              --
225800121015Condizione di pagamento automatica  errata                                    19
225900121109Il "Blocco dei pagam. automatico" richiede la "Condizione di pagam. automat." 20
226000121109La "Condizione di pagam. automat." richiede il "Blocco dei pagam. automatico" 21
