000100130628       //==============================================================
000200130708       //?TRMK40R * Gestione/Selezione Commerciali                     ?
000300130628       //==============================================================
000400130628
000500130628       //--------------------------------------------------------------
000600130628       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130628       //--------------------------------------------------------------
000800130628
000900130628     /*PRM  dbgview(*source)
001000130628     /*END
001100130628
001200130628       //--------------------------------------------------------------
001300130628       //?Specifiche di controllo.                                     ?
001400130628       //--------------------------------------------------------------
001500130628
001600130628     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130628     h dftactgrp(*no)
001800130628     h bnddir('TRUL')
001900130628
002000130628       //--------------------------------------------------------------
002100130628       //?Dichiarazione file.                                          ?
002200130628       //--------------------------------------------------------------
002300130628
002400130628       // -?Organigramma filiale/aziendale?
002500130628     fAZORG01L  if   e           k disk
002600130628
002700130708       // -?Anagrafica Commerciali?
002800130708     fAZCMM01L  if   e           k disk
002900130708       // -?Note/Rubrica Commerciali?
003000130709     f*//AZNTC01L  if   e           k disk
003100130628
003200130628       // -?Video Gestione?
003300130628     fTRMK40D   cf   e             workstn
003400130628     f                                     sfile(MK40S01 : S01nrr)
003500130628     f                                     indds(IndDspF)
003600130628     f                                     infds(InfDspF)
003700130628
003800130628       //--------------------------------------------------------------
003900130628       //?Definizione costanti.                                        ?
004000130628       //--------------------------------------------------------------
004100130628
004200130628       // -?Numero di record in ciascuna videata di subfile?
004300130628     d c_SflPag        c                   const(15)
004400130628
004500130628       // -?Numero massimo di record gestibili?
004600130628     d c_MaxRec        c                   const(9999)
004700130628
004800130628       // -?Tasti funzionali a video?
004900130628     d c_F01           c                   const(x'31')
005000130628     d c_F02           c                   const(x'32')
005100130628     d c_F03           c                   const(x'33')
005200130628     d c_F04           c                   const(x'34')
005300130628     d c_F05           c                   const(x'35')
005400130628     d c_F06           c                   const(x'36')
005500130628     d c_F07           c                   const(x'37')
005600130628     d c_F08           c                   const(x'38')
005700130628     d c_F09           c                   const(x'39')
005800130628     d c_F10           c                   const(x'3A')
005900130628     d c_F11           c                   const(x'3B')
006000130628     d c_F12           c                   const(x'3C')
006100130628     d c_F13           c                   const(x'B1')
006200130628     d c_F14           c                   const(x'B2')
006300130628     d c_F15           c                   const(x'B3')
006400130628     d c_F16           c                   const(x'B4')
006500130628     d c_F17           c                   const(x'B5')
006600130628     d c_F18           c                   const(x'B6')
006700130628     d c_F19           c                   const(x'B7')
006800130628     d c_F20           c                   const(x'B8')
006900130628     d c_F21           c                   const(x'B9')
007000130628     d c_F22           c                   const(x'BA')
007100130628     d c_F23           c                   const(x'BB')
007200130628     d c_F24           c                   const(x'BC')
007300130628     d c_Enter         c                   const(x'F1')
007400130628     d c_RollDown      c                   const(x'F4')
007500130628     d c_RollUp        c                   const(x'F5')
007600130628
007700130628       //--------------------------------------------------------------
007800130628       //?Definizione schiere.                                         ?
007900130628       //--------------------------------------------------------------
008000130628
008100130628       // -?Messaggi di errore?
008200130725     d $Msg            s             78    dim(10) ctdata perrcd( 1)
008300130628
008400130628       //--------------------------------------------------------------
008500130628       //?Definizione aree dati.                                       ?
008600130628       //--------------------------------------------------------------
008700130628
008800130628       // -?Dati utente?
008900130628     d §AzUte        e ds                  extname(AZUTE00F)
009000130628     d                                     dtaara
009100130628     d §DatiUte      e ds                  extname(dDatiUte)
009200130628     d                                     dtaara
009300130628
009400130628       //--------------------------------------------------------------
009500130628       //?Definizione strutture dati.                                  ?
009600130628       //--------------------------------------------------------------
009700130628
009800130628       // -?Status ds?
009900130628     d Status         sds
010000130628     d   SDSpgm          *proc
010100130628     d*//SDSprm          *parms
010200130628     d*//SDSusr              254    263
010300130628
010400130628       // -?InfDS?
010500130628     d InfDspF         ds
010600130628     d   dsp_aid             369    369a
010700130628     d*//sfl_rrn             376    377i 0
010800130628     d*//min_nrr             378    379i 0
010900130628     d*//num_rcds            380    381i 0
011000130628
011100130628       // -?Indicatori su DspF?
011200130628     d IndDspF         ds                  inz
011300130628         // -?Abilitazione tasti funzionali?
011400130628     d   F3Attivo                      n   overlay(IndDspF : 03)
011500130628     d   F5Attivo                      n   overlay(IndDspF : 05)
011600130628     d   F6Attivo                      n   overlay(IndDspF : 06)
011700130628     d   F8Attivo                      n   overlay(IndDspF : 08)
011800130628     d   F12Attivo                     n   overlay(IndDspF : 12)
011900130709     d   F13Attivo                     n   overlay(IndDspF : 13)
012000130628         // -?Emissione messaggio di errore?
012100130628     d   ErrMessage                    n   overlay(IndDspF : 28)
012200130628         // -?Indicatori di gestione del subfile?
012300130628     d   SflDsp_N                      n   overlay(IndDspF : 30)
012400130628     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
012500130628     d   SflNxtChg                     n   overlay(IndDspF : 32)
012600130628     d   SflEnd                        n   overlay(IndDspF : 33)
012700130710         // -?Indicatori per Attributi di visualizzazione?
012800130725     d   CmmScad                       n   overlay(IndDspF : 40)
012900130628     d   Ord_1                         n   overlay(IndDspF : 41)
013000130628     d   Ord_2                         n   overlay(IndDspF : 42)
013100130725     d   Opz_Selez                     n   overlay(IndDspF : 49)
013200130628         // -?Posizionamento cursore & segnalazione errore?
013300130628     d   PosCurOPZ                     n   overlay(IndDspF : 50)
013400130725     d   PosCurFIL                     n   overlay(IndDspF : 60)
013500130725     d   PosCurUNI                     n   overlay(IndDspF : 61)
013600130725     d   PosCurSOLu                    n   overlay(IndDspF : 62)
013700130725     d   PosCurDtATT                   n   overlay(IndDspF : 63)
013800130628         // -?Riemissione videata?
013900130628     d   ErrGenerico                   n   overlay(IndDspF : 99)
014000130628
014100130628       // -?Parametri ricevuti?
014200130628     d KPJBA         e ds
014300130701     d TRMK40ds      e ds                  inz
014400130708     d prm_AZCMM_ds  e ds                  extname(AZCMM00F)  prefix(p_)
014500130709     d*// prm_AZNTC01_dse ds                  extname(AZNTC00F)  prefix(p_)
014600130709     d*// prm_AZNTC02_dse ds                  extname(AZNTC00F)  prefix(p_)
014700130628
014800130628       //--------------------------------------------------------------
014900130628       //?Definizione variabili globali.                               ?
015000130628       //--------------------------------------------------------------
015100130628
015200130628       // -?Flags booleani?
015300130701     d $EoF            s               n   inz
015400130628     d $Fine           s               n   inz
015500130628     d $InzS01         s               n   inz(*on)
015600130701     d $Almeno1rec     s               n   inz
015700130709     d $Ord_x_CodCmm   s               n   inz
015800130709     d $Ord_x_DesCmm   s               n   inz
015900130709     d $Ord_x_CodUni   s               n   inz
016000130709     d $ForzaOpz       s               n   inz
016100130628
016200130628       // -?Variabili per la gestione del video?
016300130628     d $Video          s              2    inz('S1')
016400130709     d S01nrr          s                   like(C1RcdNbr)  inz
016500130725     d SavS1Copz       s                   like(S1Copz)    inz
016600130725     d SavS1Cop1       s                   like(S1Copz)    inz
016700130709     d SavS01csr       s                   like(C1RcdNbr)  inz
016800130725     d SavS01cs1       s                   like(C1RcdNbr)  inz
016900130725     d SavS01cs2       s                   like(C1RcdNbr)  inz
017000130709     d wPag            s              4  0 inz
017100130709     d wRig            s              3  0 inz
017200130628
017300130628       // -?Variabili per la gestione del posizionamento nel subfile?
017400130725     d SavC1Cfil       s                   like(C1Cfil)    inz
017500130725     d SavC1Cuni       s                   like(C1Cuni)    inz
017600130725     d SavC1CsolU      s                   like(C1CsolU)   inz
017700130725     d SavC1CdtAtt     s                   like(C1CdtAtt)  inz
017800130628
017900130628       // -?Campi di comodo?
018000130725     d wDate           s              8s 0 inz
018100130725     d W1CdtAtt        s              8s 0 inz
018200130628
018300130628       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018400130628
018500130628       //--------------------------------------------------------------
018600130628       // -?Constants?
018700130628       //--------------------------------------------------------------
018800130628       // -?MaxKey - Maximum number of key fields allowed by this program?
018900130725     d c_MaxKey        c                   const(2)
019000130628       // -?Sort order:?
019100130628       //  ?1 = Sort in an ascending sequence?
019200130628       //  ?2 = Sort in a descending sequence?
019300130725     d c_Ascendente    c                   const(1)
019400130725     d c_Discendente   c                   const(2)
019500130628       // -?Key data type:?
019600130628       //  ? 0 = Signed binary?
019700130628       //  ? 2 = Signed zoned decimal?
019800130628       //  ? 3 = Signed packed decimal?
019900130628       //  ? 6 = Character with no national language sort sequence applied,?
020000130628       //  ?     if specified?
020100130628       //  ? 7 = Unsigned packed decimal?
020200130628       //  ?     All numbers will have the sign forced positive ('F'X).?
020300130628       //  ? 8 = Unsigned zoned decimal?
020400130628       //  ?     All numbers will have the sign forced positive ('F'X).?
020500130628       //  ? 9 = Unsigned binary?
020600130628       //  ?14 = Date in form of DD/MM/YY?
020700130628       //  ?15 = Date in form of DD.MM.YYYY?
020800130725     d c_Numero        c                   const(2)
020900130725     d c_Carattere     c                   const(6)
021000130725     d c_NumIntero     c                   const(8)
021100130628       //
021200130725     d c_Put           c                   const(1)
021300130725     d c_EndPut        c                   const(2)
021400130725     d c_Get           c                   const(3)
021500130628
021600130628       //--------------------------------------------------------------
021700130628       // -?Data Structures?
021800130628       //  ?SflRcd     - The entire subfile record to pass to the sort?
021900130628       //  ?QLGSCB     - The sort request block for the QLGSORT API?
022000130628       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
022100130628       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
022200130628       //  ?QUSEC      - Error structure for the QLGSORT API?
022300130628       //--------------------------------------------------------------
022400130628     d SflRcd          ds                  inz
022500130701     d   S1Copz                            inz
022600130708     d   S1Ccmm                            inz
022700130708     d   S1Dcmm                            inz
022800130628     d   S1Cuni                            inz
022900130628     d   S1Cdip                            inz
023000130628     d   S1Cpli                            inz
023100130628     d   S1Cple                            inz
023200130628     d   Selected                     1a   inz
023300130628      /copy QSYSINC/QRPGLESRC,QLGSORT
023400130725     d QLGKL                         16    dim(c_MaxKey)
023500130628     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
023600130628     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
023700130628     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
023800130628     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
023900130628      /copy QSYSINC/QRPGLESRC,QLGSRTIO
024000130628      /copy QSYSINC/QRPGLESRC,QUSEC
024100130628
024200130628       //--------------------------------------------------------------
024300130628       // -?Standalone fields?
024400130628       //  ?Nrr        - Relative record number for subfile?
024500130628       //  ?SizeList   - Size of list?
024600130628       //  ?ReturnSize - Size of list returned by sort API?
024700130628       //--------------------------------------------------------------
024800130628     d NotUsed         s             16a   inz
024900130628     d ReturnSize      s              9b 0 inz
025000130628     d SizeList        s              9b 0 inz
025100130628     d RrnLast         s              5i 0 inz
025200130628     d Nrr             s              5i 0 inz
025300130628
025400130628       //--------------------------------------------------------------
025500130628       //?Definizione prototipi procedure usate.                       ?
025600130628       //--------------------------------------------------------------
025700130628
025800130628       // -?Reperimento dati utente?
025900130628     d TIBS34ds      e ds                  inz
026000130628      /copy gaitrasrc/srcProtoPR,TIBS34R
026100130725
026200130725       // -?Controllo formale / Inversione data?
026300130725     d WLBdat          ds                  inz
026400130725     d   G02dat                1      8  0 inz
026500130725     d   G02inv                9     16  0 inz
026600130725     d   G02err               17     17    inz('3')
026700130725     d   G02tgi               18     22  0 inz
026800130725      /copy gaitrasrc/srcProtoPR,XSRDA8
026900130701
027000130708       // -?Manutenzione singolo commerciale?
027100130701      /copy gaitrasrc/srcProtoPI,TRMK41R
027200130701      /copy gaitrasrc/srcProtoPR,TRMK41R
027300130628
027400130628       // -?Ordinamento subfile?
027500130628      /copy gaitrasrc/srcProtoPR,QLGSRTIO
027600130628
027700130628       //--------------------------------------------------------------
027800130628       //?Definizione key-list.                                        ?
027900130628       //--------------------------------------------------------------
028000130628
028100130708       // -?File AZCMM01L?
028200130708     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
028300130628     d                                     prefix(k_)   inz
028400130628
028500130709       // -?File AZNTC01L?
028600130709     d*// k01azntc01    e ds                  extname(AZNTC01L : *key)
028700130628     d*//                                     prefix(k_)   inz
028800130628
028900130628       //--------------------------------------------------------------
029000130628       //?Riepilogo indicatori utilizzati.                             ?
029100130628       //--------------------------------------------------------------
029200130628       //--------------------------------------------------------------
029300130628
029400130628       //--------------------------------------------------------------
029500130628       //?M A I N - L I N E                                            ?
029600130628       //--------------------------------------------------------------
029700130628
029800130628     c     *Entry        plist
029900130628     c                   parm                    KPJBA
030000130708     c                   parm                    prm_AZCMM_ds
030100130709     c*//                parm                    prm_AZNTC01_ds
030200130709     c*//                parm                    prm_AZNTC02_ds
030300130628
030400130628      /free
030500130628
030600130628       // -?Operazioni iniziali?
030700130628       exsr sr_RoutInz;
030800130628
030900130628       // -?Ciclo di gestione del file video?
031000130628       DoW  $Fine = *off;
031100130628
031200130628         select;
031300130628
031400130628           // -?Gestione videata S1 (subfile)?
031500130628           when $Video = 'S1';
031600130628             exsr sr_GesS01;
031700130628
031800130628           // -?? ? ??
031900130628           other;
032000130628             $Fine = *on;
032100130628
032200130628         endsl;
032300130628
032400130628       EndDo;
032500130628
032600130628       // -?Operazioni finali?
032700130628       exsr sr_RoutEnd;
032800130628
032900130628       //--------------------------------------------------------------
033000130628       //?Operazioni iniziali.                                         ?
033100130628       //--------------------------------------------------------------
033200130628       BEGSR  sr_RoutInz;
033300130628
033400130628         // -?Impostazione chiusura?
033500130628         *inLR = *on;
033600130628
033700130628         // -?Reperimento dati job?
033800130628         exsr  sr_DatiJob;
033900130628
034000130628         // -?Reperimento data odierna?
034100130725         wDate = %dec( %date() );
034200130628
034300130628         // -?Impostazione nome programma a video?
034400130628         V1Tpgm = SDSpgm;
034500130628
034600130628         // -?Impostazione videata iniziale?
034700130701         clear  MK40C01;
034800130628         $Video = 'S1';
034900130628         reset  $InzS01;
035000130708         $Ord_x_CodCmm = *on;
035100130708         $Ord_x_DesCmm = *off;
035200130701         $Ord_x_CodUni = *off;
035300130701
035400130701         // -?Verifica parametri ricevuti?
035500130701         Opz_Selez = ( %parms() > 1 );
035600130701
035700130701         // -?Impostazione parzializzazioni richieste (se ricevute)?
035800130701         clear  TRMK40ds;
035900130701         if  kpjbu <> *blank;
036000130701           TRMK40ds = KPJBU;
036100130701         endif;
036200130725         C1Cfil   = iMK40fil;
036300130725         C1cuni   = iMK40uni;
036400130725         C1CsolU  = iMK40solU;
036500130725         if  iMK40dtAtt > *zero;
036600130725           reset  WLBdat;
036700130725           G02inv  = CMMdtIni;
036800130725           xsrda8 ( WLBdat );
036900130725           C1CdtAtt = G02dat;
037000130725           W1CdtAtt = G02inv;
037100130725         endif;
037200130701         clear  oMK40fxx;
037300130701         oMK40err = *off;
037400130701
037500130701         // -?Pulizia eventuali parametri di output (se richiesta selezione)?
037600130701         if  %parms() > 1;
037700130708           clear  prm_AZCMM_ds;
037800130709           //clear  prm_AZNTC01_ds;
037900130709           //clear  prm_AZNTC02_ds;
038000130701         endif;
038100130701
038200130701         // -?Impostazione videata iniziale?
038300130701         $Video = 'S1';
038400130701         reset  $InzS01;
038500130708         $Ord_x_CodCmm = *on;
038600130701         exsr  sr_InzS01;
038700130701         $InzS01 = *off;
038800130701         $Almeno1rec = (S01nrr > *zero);
038900130628
039000130628       ENDSR;
039100130628
039200130628       //--------------------------------------------------------------
039300130628       //?Reperimento Dati del job (Utente/Operativi).                 ?
039400130628       //--------------------------------------------------------------
039500130628       BEGSR  sr_DatiJob;
039600130628
039700130628         in(e) §AzUte;
039800130628         if NOT %error;
039900130628           in(e) §DatiUte;
040000130628         endif;
040100130628         if %error or RSut = *blank;
040200130628           tibs34r ( tibs34ds );
040300130628           in §AzUte;
040400130628           in §DatiUte;
040500130628         endif;
040600130628
040700130628       ENDSR;
040800130628
040900130628       //--------------------------------------------------------------
041000130628       //?Gestione subfile S01                                         ?
041100130628       //--------------------------------------------------------------
041200130628       BEGSR  sr_GesS01;
041300130628
041400130628         // -?Inizializzazione videata?
041500130628         if  $InzS01 = *on;
041600130628           exsr  sr_InzS01;
041700130628           $InzS01 = *off;
041800130628         endif;
041900130628
042000130628         // -?Emissione Testata & Piede con tasti funzionali abilitati?
042100130628         write  MK40T01;
042200130628         write  MK40P01;
042300130628
042400130628         // -?Posizionamento cursore?
042500130628         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
042600130628         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
042700130628         //  ?si visualizza la pagina che vedeva l'utente quando ha?
042800130628         //  ?premuto l'ultimo tasto.?
042900130628         if  C1CsrRrn > *zero;
043000130628           C1RcdNbr = C1CsrRrn;
043100130628         else;
043200130628           C1RcdNbr = 1;
043300130628           write  MK40S00;          //?(no rec.)?
043400130628         endif;
043500130628
043600130628         // -?Emissione videata?
043700130628         exfmt  MK40C01;
043800130628
043900130628         clear  V1Dmsg;
044000130628         reset  ErrMessage;
044100130628         reset  ErrGenerico;
044200130709
044300130709         if  dsp_aid <> c_F03  and
044400130709             dsp_aid <> c_F13;
044500130709           clear  SavS01csr;
044600130725           clear  SavS01cs1;
044700130725           clear  SavS01cs2;
044800130709           clear  SavS1Copz;
044900130725           clear  SavS1Cop1;
045000130709         endif;
045100130628
045200130628         SELECT;
045300130628
045400130628           // -?F3=Fine?
045500130712           WHEN  dsp_aid = c_F03;
045600130701             oMK40fxx = '03';
045700130628             $Fine = *on;
045800130628
045900150112           // -?F5=Refresh?
046000130712           WHEN  dsp_aid = c_F05;
046100130628             $InzS01 = *on;
046200130628
046300130628           // -?F6=Immissione?
046400130712           WHEN  dsp_aid = c_F06;
046500130628             exsr  sr_Call_TRMK41;
046600130705             if  oMK41fxx = '03';
046700130705               $Fine = *on;
046800130705             endif;
046900130628
047000130628           // -?F8=Ordinamento?
047100130628           WHEN  dsp_aid = c_F08;
047200130628             exsr  sr_F08S01;
047300130628
047400130628           // -?F12=Ritorno?
047500130628           WHEN  dsp_aid = c_F12;
047600130701             oMK40fxx = '12';
047700130628             $Fine = *on;
047800130709
047900130709           // -?F13=Ripetizione Opzione?
048000130709           WHEN  dsp_aid = c_F13;
048100130709             exsr  sr_F13S01;
048200130628
048300130628           // -?Invio?
048400130628           OTHER;
048500130725             // -?Controllo dati nel SflCtrl?
048600130725             exsr  sr_CtrC01;
048700130725             if  ErrGenerico;
048800130725               leavesr;
048900130725             endif;
049000130701             // -?Gestione opzioni?
049100130628             if  S01nrr > *zero;
049200130628               exsr  sr_OpzS01;
049300130628               if  ErrGenerico;
049400130628                 leavesr;
049500130628               endif;
049600130628             endif;
049700130725             // -?Selezioni?
049800130725             // -?Selezionata una filiale diversa      e/o?
049900130725             //  ?Richiesti comm.li di un unificante   e/o?
050000130725             //  ?Richiesti SOLO unificanti            e/o?
050100130725             //  ?Richiesti comm.li attivi alla data?
050200130725             If  C1Cfil   <> SavC1Cfil    or
050300130725                 C1Cuni   <> SavC1Cuni    or
050400130725                 C1CsolU  <> SavC1CsolU   or
050500130725                 C1CdtAtt <> SavC1CdtAtt;
050600130712               // -?Selezionata una filiale diversa?
050700130725               if  C1Cfil <> SavC1Cfil;
050800130712                 exsr  sr_CtrFIL;
050900130712                 if  ErrMessage;
051000130712                   leavesr;
051100130712                 endif;
051200130712               endif;
051300130712               $InzS01 = *on;
051400130712             EndIf;
051500130628
051600130628         ENDSL;
051700130628
051800130628       ENDSR;
051900130628
052000130628       //--------------------------------------------------------------
052100130628       //?Inizializzazione subfile S01                                 ?
052200130628       //--------------------------------------------------------------
052300130628       BEGSR  sr_InzS01;
052400130628
052500130701         // -?Spegnimento degli indicatori di errore?
052600130628         %subst(IndDspF : 50) = *off;
052700130628
052800130701         // -?Pulizia subfile?
052900130628         SflDsp_N    = *on;
053000130628         SflDspCtl_N = *on;
053100130701         write  MK40C01;
053200130628         SflDspCtl_N = *off;
053300130628         SflEnd      = *off;
053400130628
053500150108         SavC1Cfil   = C1Cfil;
053600150108         SavC1Cuni   = C1Cuni;
053700150108         SavC1CsolU  = C1CsolU;
053800150108         SavC1CdtAtt = C1CdtAtt;
053900150108
054000130628         clear  C1RcdNbr;
054100130628         clear  C1CsrRrn;
054200130628         clear  S01nrr;
054300130628         clear  V1Dmsg;
054400130628         ErrMessage  = *off;
054500130628         ErrGenerico = *off;
054600130628
054700130701         Select;
054800130708           When  $Ord_x_CodCmm;
054900130701             Ord_1 = *off;
055000130701             Ord_2 = *off;
055100130708           When  $Ord_x_DesCmm;
055200130701             Ord_1 = *on;
055300130701             Ord_2 = *off;
055400130701           When  $Ord_x_CodUni;
055500130701             Ord_1 = *off;
055600130701             Ord_2 = *on;
055700130701         EndSl;
055800130701
055900130701         // -?Caricamento completo dei dati nel subfile (per codice)?
056000130701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
056100130701         //  ?l'eventuale ordinamento)?
056200130708         clear  k01azcmm01;
056300130725         if  C1Cfil <> *zero;
056400130725           k_CMMcod = C1Cfil * 10000;
056500130725         else;
056600130725           k_CMMcod = *loval;
056700130725         endif;
056800130708         setll  %kds(k01azcmm01)  AZCMM000;
056900130708         exsr  sr_ReadAZCMM;
057000130701         if  Not  $EoF;
057100130701           exsr  sr_CaricaS01;
057200130701         endif;
057300130628
057400130701         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
057500130701         RrnLast  = S01nrr;
057600130701
057700130701         // -?Impaginazione subfile?
057800130701         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
057900130628         if S01nrr > *zero;
058000130628           C1RcdNbr  = 1;
058100130628           C1CsrRrn  = 1;
058200130628         else;
058300130628           clear C1RcdNbr;
058400130628         endif;
058500130628
058600130701         // -?Ordinamento subfile (se premuto F8)?
058700130708         if  Not $Ord_x_CodCmm  and  S01nrr > *zero;
058800130628           exsr  sr_SortSfl;
058900130628         endif;
059000130628
059100130701         // -?(Dis)Abilitazione tasti funzionali?
059200130709         F3Attivo  = (%parms() = 1);   // -?F3 = Fine?
059300130709         F5Attivo  = *on;              // -?F5 = Refresh?
059400130709         F6Attivo  = (%parms() = 1);   // -?F6 = Immissione?
059500130709         F8Attivo  = (S01nrr > *zero  or  $Almeno1rec); // -?F8 = Ordinamento x ...?
059600130709         //F12Attivo = (%parms() > 1);   // -?F12 = Ritorno?
059700130709         F12Attivo = *off;             // -?F12 = Ritorno?
059800130709         F13Attivo = (%parms() = 1  and  S01nrr > 1);   // -?F13 = Ripetizione opz.?
059900130701
060000130701         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
060100130701         Select;
060200130701           When  Not F8Attivo;
060300130701             clear  P01f08;
060400130708           When  $Ord_x_CodCmm;
060500130708             P01f08 = 'F8=Ord. x Descrizione';
060600130708           When  $Ord_x_DesCmm;
060700130701             P01f08 = 'F8=Ord. x Unificante ';
060800130701           When  $Ord_x_CodUni;
060900130701             P01f08 = 'F8=Ord. x Cod.Comm.le';
061000130701         EndSl;
061100130701
061200130628       ENDSR;
061300130628
061400130628       //--------------------------------------------------------------
061500130708       //?Ricerca del 1° record valido successivo in AZCMM00F          ?
061600130628       //--------------------------------------------------------------
061700130708       BEGSR  sr_ReadAZCMM;
061800130628
061900130701         reset  $EoF;
062000130628
062100130628         DoW  Not $EoF;
062200130628
062300130708           read  AZCMM000;
062400130628
062500130628           select;
062600130628
062700130701             // -?End Of File => Fine?
062800130708             when  %eof(AZCMM01L);
062900130701               $EoF = *on;
063000130701               leavesr;
063100130725
063200130725             // -?Richiesti solo i comm.li di una filiale:?
063300130725             //  ?si scartano i comm.li di filiale maggiore => Fine?
063400130725             when  C1Cfil <> *zero     and
063500130725                   C1Cfil <> %int( CMMcod / 10000 );
063600130725               $EoF = *on;
063700130725               leavesr;
063800130722
063900130722             // -?Richiesti solo i comm.li di un unificante:?
064000130722             //  ?si scartano i comm.li con unificante diverso => Read Next?
064100130725             when  C1Cuni > *zero      and  C1Cuni <> CMMuni;
064200130722               iter;
064300130628
064400130701             // -?Richiesti solo i comm.li unificanti:?
064500130701             //  ?si scartano i comm.li NON unificanti => Read Next?
064600130725             when  C1CsolU = 'S'       and  CMMcod <> CMMuni;
064700130628               iter;
064800130701
064900130725             // -?Richiesti solo i comm.li attivi ad una data:?
065000130725             //  ?si scartano i comm.li non ancora/più attivi => Read Next?
065100130725             when  C1CdtAtt > *zero    and
065200130725                  (CMMdtIni > W1CdtAtt  or  CMMdtFin < W1CdtAtt);
065300130701               iter;
065400130628
065500130701             // -?Superato il limite max di rec. caricabili nel sfl?
065600130701             //  ?=> Fine?
065700130628             when  S01nrr >= c_MaxRec;
065800130701               leavesr;
065900130628
066000130708             // -?Richiesti tutti i commerciali => Ok (uscita)?
066100130628             other;
066200130628               leave;
066300130628
066400130628           endsl;
066500130628
066600130628         EndDo;
066700130628
066800130628       ENDSR;
066900130628
067000130628       //--------------------------------------------------------------
067100130702       //?Caricamento di tutto il subfile S01.                         ?
067200130628       //--------------------------------------------------------------
067300130701       BEGSR  sr_CaricaS01;
067400130628
067500130702         clear  S01nrr;
067600130628         SflNxtChg = *off;
067700130628
067800130708         // -?Ciclo di caricamento dati da file AZCMM00F?
067900130701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
068000130701         //  ?l'eventuale ordinamento)?
068100130628         DoW  Not $EoF;
068200130628
068300130701           // -?Impostazione campi nel record del subfile?
068400130701           clear  MK40S01;
068500130708           S1Ccmm = CMMcod;
068600130708           S1Dcmm = CMMdes;
068700130708           S1Cuni = CMMuni;
068800130708           if  CMMdip <> *zero  or  CMMsoc <> *blank;
068900130709             S1Cdip = CMMsoc + '/' + %editc( CMMdip : 'Z');
069000130701           endif;
069100130708           S1Cpli = CMMpli;
069200130708           S1Cple = CMMple;
069300130701
069400130701           // -?Impostazione attributi di visualizzazione?
069500130709           Ord_1 = ($Ord_x_DesCmm);
069600130709           Ord_2 = ($Ord_x_CodUni);
069700130725           CmmScad = ( CMMdtIni > wDate  or  CMMdtFin < wDate );
069800130628
069900130701           // -?Registrazione singolo record nel subfile?
070000130628           S01nrr += 1;
070100130701           write  MK40S01;
070200130628
070300130701           // -?Lettura record successivo?
070400130708           exsr  sr_ReadAZCMM;
070500130628
070600130628         EndDo;
070700130628
070800130701         // -?Visualizzazione del SFL (se ci sono dati)?
070900130628         SflDsp_N  = (S01nrr <= *zero);
071000130628
071100130701         // -?Attivazione del SFLEND?
071200130628         SflEnd = $EoF;
071300130701
071400130701         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
071500130701         if  S01nrr > *zero;
071600130701           C1RcdNbr = 1;
071700130701         else;
071800130701           clear  C1RcdNbr;
071900130701         endif;
072000130628
072100130701         // -?Emissione messaggio di segnalazione raggiungimento limite?
072200130708         if  S01nrr >= c_MaxRec   and   Not %eof(AZCMM01L);
072300130702           ErrGenerico = *on;
072400130628           ErrMessage  = *on;
072500130702           PosCurFIL   = *on;
072600130701           V1Dmsg = $Msg(01);
072700130628           leavesr;
072800130628         endif;
072900130628
073000130628       ENDSR;
073100130725
073200130725       //--------------------------------------------------------------
073300130725       //?Controllo dati inseriti nel subfile-control                  ?
073400130725       //--------------------------------------------------------------
073500130725       BEGSR  sr_CtrC01;
073600130725
073700130725         If  C1CdtAtt <> *zero  and  C1CdtAtt <> SavC1CdtAtt;
073800130725           clear  WLBdat;
073900130725           G02dat = C1CdtAtt;
074000130725           xsrda8 ( WLBdat );
074100130725           if  G02err = *on;
074200130725             V1Dmsg = $Msg(10);
074300130725             PosCurDtATT = *on;
074400130725             ErrMessage  = *on;
074500130725             ErrGenerico = *on;
074600130725             leavesr;
074700130725           endif;
074800130725           C1CdtAtt = G02dat;
074900130725           W1CdtAtt = G02inv;
075000130725         EndIf;
075100130725
075200130725       ENDSR;
075300130628
075400130628       //--------------------------------------------------------------
075500130628       //?Gestione opzioni del subfile S01                             ?
075600130628       //--------------------------------------------------------------
075700130628       BEGSR  sr_OpzS01;
075800130701
075900130701         // -?Spegnimento degli indicatori di errore?
076000130701         %subst(IndDspF : 50) = *off;
076100130628
076200130628         clear  SavS01csr;
076300130628
076400130701         // -?Ciclo di lettura subfile?
076500130701         readc  MK40S01;
076600130628
076700130701         DoW  Not %eof(TRMK40D);
076800130628
076900130628           SflNxtChg = *off;
077000130628           %subst(IndDspF : 50) = *off;
077100130628           C1RcdNbr = S01nrr;
077200130628
077300130628           select;
077400130701
077500130701             // -?Nessuna opzione?
077600130701             when  S1Copz = *zero;
077700130701
077800130725             // -?Opzione + Parzializzazioni?
077900130725             when  S1Copz   <> *zero      and
078000130725                  (C1Cfil   <> SavC1Cfil   or
078100130725                   C1Cuni   <> SavC1Cuni   or
078200130725                   C1CsolU  <> SavC1CsolU  or
078300130725                   C1CdtAtt <> SavC1CdtAtt);
078400130702               ErrGenerico = *on;
078500130701               ErrMessage  = *on;
078600130702               select;
078700130725                 when C1Cfil   <> SavC1Cfil;
078800130725                   PosCurFIL   =  *on;
078900130725                 when C1Cuni   <> SavC1Cuni;
079000130725                   PosCurUNI   =  *on;
079100130725                 when C1CsolU  <> SavC1CsolU;
079200130725                   PosCurSolU  =  *on;
079300130725                 when C1CdtAtt <> SavC1CdtAtt;
079400130725                   PosCurDtAtt =  *on;
079500130702               endsl;
079600130701               V1Dmsg = $Msg(02);
079700130628
079800130701             // -?Opz. 1=Selezione commerciale?
079900130701             when  Opz_Selez  and  S1Copz = 1;
080000130708               k_CMMcod = S1Ccmm;
080100130708               chain  %kds(k01azcmm01)  AZCMM000;
080200130708               if  %found(AZCMM01L);
080300130708                 prm_AZCMM_ds = AZCMM_ds;
080400130701               else;
080500130708                 p_CMMcod = S1Ccmm;
080600130708                 p_CMMdes = S1Dcmm;
080700130701               endif;
080800130628               $Fine = *on;
080900130628               ErrGenerico = *on;
081000130701
081100130701             // -?Opz. 2=Modifica?
081200130701             //  ?Opz. 3=Copia?
081300130710             //  ?Opz. 4=Cancellazione?
081400130701             //  ?Opz. 5=Visualizzazione?
081500130702             when  Not Opz_Selez   and
081600130701                  (S1Copz = 2  or  S1Copz = 3  or
081700130710                   S1Copz = 4  or  S1Copz = 5);
081800130701               exsr  sr_Call_TRMK41;
081900130705               select;
082000130705                 when  ErrGenerico;
082100130705                 when  oMK41fxx  = '03';
082200130705                   ErrGenerico = *on;
082300130705               endsl;
082400130710
082500130710             // -?Opz. 11=Selezione unificante?
082600130710             when  Opz_Selez  and  S1Copz = 11;
082700130710               k_CMMcod = S1Cuni;
082800130710               chain  %kds(k01azcmm01)  AZCMM000;
082900130710               if  %found(AZCMM01L);
083000130710                 prm_AZCMM_ds = AZCMM_ds;
083100130710               else;
083200130710                 p_CMMcod = S1Cuni;
083300130710               endif;
083400130710               $Fine = *on;
083500130710               ErrGenerico = *on;
083600130628
083700130701             // -?Opzione errata?
083800130628             other;
083900130628               ErrGenerico = *on;
084000130702               ErrMessage  = *on;
084100130628               PosCurOPZ   = *on;
084200130701               V1Dmsg = $Msg(03);
084300130628
084400130628           endsl;
084500130628
084600130701           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
084700130701           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
084800130701           if  S1Copz   <> *zero  and
084900130701              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
085000130628             SavS01csr   = C1RcdNbr;
085100130628           endif;
085200130628
085300130701           // -?Aggiornamento sfl?
085400130628           select;
085500130628             when  ErrMessage;
085600130628               SflNxtChg = *on;
085700130628               C1CsrRrn  = C1RcdNbr;
085800130628             when  ErrGenerico;
085900130628               C1CsrRrn  = C1RcdNbr;
086000130628               clear  S1Copz;
086100130628             other;
086200130628               clear  S1Copz;
086300130628           endsl;
086400130701
086500130701           UPDATE  MK40S01;
086600130701
086700130628           if  ErrGenerico   or   ErrMessage;
086800130628             leave;
086900130628           endif;
087000130628
087100130701           readc  MK40S01;
087200130628
087300130628         ENDDO;
087400130628
087500130701         // -?Riposizionam. cursore sul record contenente la prima opz.?
087600130701         //  ?(se non sono stati rilevati errori)?
087700130628         if  NOT ErrMessage   and   NOT ErrGenerico   and
087800130628             SavS01csr > *zero;
087900130628           C1CsrRrn = SavS01csr;
088000130628         endif;
088100130628
088200130628       ENDSR;
088300130628
088400130628       //--------------------------------------------------------------
088500130628       //?Controllo filiale inserita in C01                            ?
088600130628       //--------------------------------------------------------------
088700130628       BEGSR  sr_CtrFil;
088800130628
088900130725         If  C1Cfil <> *zero;
089000130628
089100130725           chain  (C1Cfil)  AZORG;
089200130628
089300130628           select;
089400130628             when  Not %found(AZORG01L);
089500130628               ErrGenerico = *on;
089600130702               ErrMessage  = *on;
089700130628               PosCurFIL   = *on;
089800130701               V1Dmsg = $Msg(04);
089900130628               leavesr;
090000130628             when  ORGfva <> *blank;
090100130628               ErrGenerico = *on;
090200130702               ErrMessage  = *on;
090300130628               PosCurFIL   = *on;
090400130701               V1Dmsg = $Msg(05);
090500130628               leavesr;
090600130628             when  ORGfag <> 'F'   and   ORGfva <> 'A';
090700130628               ErrGenerico = *on;
090800130702               ErrMessage  = *on;
090900130628               PosCurFIL   = *on;
091000130701               V1Dmsg = $Msg(06);
091100130628               leavesr;
091200130628           endsl;
091300130628
091400130701         EndIf;
091500130628
091600130725         SavC1Cfil = C1Cfil;
091700130628
091800130628       ENDSR;
091900130628
092000130628       //--------------------------------------------------------------
092100130628       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
092200130628       //?F08-Cambia ordinamento sfl                                   ?
092300130628       //--------------------------------------------------------------
092400130628       BEGSR  sr_F08S01;
092500130628
092600130701         Select;
092700130708           When $Ord_x_CodCmm;
092800130708             $Ord_x_CodCmm = *off;
092900130708             $Ord_x_DesCmm = *on;
093000130702             $Ord_x_CodUni = *off;
093100130708           When $Ord_x_DesCmm;
093200130708             $Ord_x_CodCmm = *off;
093300130708             $Ord_x_DesCmm = *off;
093400130702             $Ord_x_CodUni = *on;
093500130701           When $Ord_x_CodUni;
093600130709             $Ord_x_CodCmm = *on;
093700130708             $Ord_x_DesCmm = *off;
093800130702             $Ord_x_CodUni = *off;
093900130701         EndSl;
094000130628
094100130701         select;
094200130701           // -?Subfile vuoto: nessun record da ordinare?
094300130701           when  Not $Almeno1rec;
094400130705           // -?Subfile già parzializzato: occorre ricaricarlo?
094500130725           when  SavC1Cfil   <> C1Cfil   or
094600130725                 SavC1Cuni   <> C1Cuni   or
094700130725                 SavC1CsolU  <> C1CsolU  or
094800130725                 SavC1CdtAtt <> C1CdtAtt;
094900130705             $InzS01 = *on;
095000130701           // -?Altrimenti: basta riordinarlo?
095100130701           other;
095200130701             exsr  sr_SortSfl;
095300130701         endsl;
095400130628
095500130628       ENDSR;
095600130709
095700130709       //--------------------------------------------------------------
095800130709       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
095900130709       //?F13-Ripetizione opzione                                      ?
096000130709       //--------------------------------------------------------------
096100130709       BEGSR  sr_F13S01;
096200130709
096300130709         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
096400130709         //  ?dalla posizione del cursore?
096500130709         wPag = C1CsrRrn / c_SflPag;
096600130709         wRig = %rem( C1CsrRrn : c_SflPag);
096700130709         if  wRig > *zero;
096800130709           wRig = (wPag * c_SflPag) + 1;
096900130709         else;                           // -?ultimo rec. della pag.?
097000130709           wRig = (wPag * c_SflPag) - c_SflPag + 1;
097100130709         endif;
097200130709         SavS01csr = wRig;
097300130709
097400130709         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
097500130709
097600130709         // -?Reperimento della 1ª opzione inserita?
097700130709         //  ?nella pagina dov'è il cursore?
097800130709         readc  MK40S01;
097900130709         DoW  Not %eof(TRMK40D)  and  S1Copz <> *zero  and
098000130709              S01nrr < SavS01csr;
098100130709           SflNxtChg = *on;
098200130709           update  MK40S01;
098300130709           readc  MK40S01;
098400130709         EndDo;
098500130709         if  Not %eof(TRMK40D)  and  S1Copz <> *zero;
098600130709           SflNxtChg = *on;
098700130709           update  MK40S01;
098800130709         endif;
098900130709
099000130709         // -?Nessuna opzione nella pagina?
099100130709         if  %eof(TRMK40D)         or
099200130709             S01nrr <  SavS01csr   or
099300130709             S01nrr > (SavS01csr + c_SflPag - 1);
099400130709           C1RcdNbr  = SavS01csr;
099500130709           C1CsrRrn  = SavS01csr;
099600130725           clear  $ForzaOpz;
099700130725           clear  SavS1Copz;
099800130725           clear  SavS1Cop1;
099900130709           clear  SavS01cs1;
100000130709           clear  SavS01cs2;
100100130709           ErrGenerico = *on;
100200130709           ErrMessage  = *on;
100300130709           PosCurOPZ   = *on;
100400130709           V1Dmsg = $Msg(07);
100500130709           leavesr;
100600130709         endif;
100700130709
100800130709         // -?Memorizzazione 1ª opzione nella pagina?
100900130709         SavS01csr = S01nrr;
101000130709         SavS1Copz = S1Copz;
101100130709         if  S01nrr <> SavS01cs1  or
101200130709             S1Copz <> SavS1Cop1;
101300130725           clear  $ForzaOpz;
101400130709         endif;
101500130709         SavS01cs1 = S01nrr;
101600130709         SavS1Cop1 = S1Copz;
101700130709
101800130709         // -?Reperimento delle altre eventuali opzioni?
101900130709         readc  MK40S01;
102000130709         DOW  Not %eof(TRMK40D);
102100130709
102200130709           // ·?Aggiornamento subfile?
102300130709           if  S1Copz <> *zero;
102400130709             SflNxtChg = *on;
102500130709             update  MK40S01;
102600130709           endif;
102700130709
102800130709           // ·?Reperita una 2ª opzione?
102900130709           Select;
103000130709
103100130709             // ·?Vuota!?
103200130709             When  S1Copz = *zero;
103300130709
103400130709             // ·?1ª opzione nella pagina del cursore  e?
103500130710             //  ?2ª opzione successiva diversa:?
103600130710             //  ?messaggio di avviso di "ricopertura"?
103700130709             When  (SavS01csr >= wRig              and
103800130709                    S01nrr    >  SavS01csr)        and
103900130709                    S1Copz    <> SavS1Copz         and
104000130709                    Not $ForzaOpz;
104100130709               $ForzaOpz = *on;
104200130709               SavS01cs2 = S01nrr;
104300130709               C1RcdNbr  = SavS01csr;
104400130709               C1CsrRrn  = SavS01csr;
104500130709               ErrGenerico = *on;
104600130709               ErrMessage  = *on;
104700130709               PosCurOPZ   = *on;
104800130709               V1Dmsg = $Msg(08);
104900130709               leavesr;
105000130709
105100130709             // ·?2ª opzione sul record dove è posizionato il cursore?
105200130709             //  ?(la nuova da considerare per la duplicazione)?
105300130709             When  C1CsrRrn = S01nrr;
105400130709               SavS01csr = S01nrr;
105500130709               SavS1Copz = S1Copz;
105600130709
105700130709             // ·?Più opzioni nella stessa videata e?
105800130709             //  ?cursore su nessuna delle opzioni?
105900130709             //  ?(riposizionare il cursore)?
106000130709             //  ?Se stessa opzione in videate diverse: vince la?
106100130709             //  ?posizione della prima opzione.?
106200130709             When  C1CsrRrn <> SavS01csr  and
106300130709                   C1CsrRrn <> S01nrr     and
106400130709                   S01nrr   <  wRig + c_SflPag;
106500130709               SavS01cs2 = S01nrr;
106600130709               C1RcdNbr  = SavS01csr;
106700130709               C1CsrRrn  = SavS01csr;
106800130709               ErrGenerico = *on;
106900130709               ErrMessage  = *on;
107000130709               PosCurOPZ   = *on;
107100130709               V1Dmsg = $Msg(09);
107200130709               leavesr;
107300130709
107400130709           EndSl;
107500130709
107600130709           readc  MK40S01;
107700130709
107800130709         ENDDO;
107900130709
108000130709         // ·?Opzione NON abilitata?
108100130709         if  SavS1Copz <> 1  and  SavS1Copz <> 2  and
108200130709             SavS1Copz <> 3  and  SavS1Copz <> 4  and
108300130710             SavS1Copz <> 5  and  SavS1Copz <> 11;
108400130709           C1RcdNbr  = SavS01csr;
108500130709           C1CsrRrn  = SavS01csr;
108600130709           ErrGenerico = *on;
108700130709           ErrMessage  = *on;
108800130709           PosCurOPZ   = *on;
108900130709           V1Dmsg = $Msg(03);
109000130709           leavesr;
109100130709         endif;
109200130709
109300130709         // -?Ripetizione opzione sul resto del subfile?
109400130709         S01nrr = SavS01csr + 1;
109500130709         chain  S01nrr  MK40S01;
109600130709         DoW  %found(TRMK40D);
109700130709           SflNxtChg = *on;
109800130709           S1Copz = SavS1Copz;
109900130709           update  MK40S01;
110000130709           S01nrr += 1;
110100130709           chain  S01nrr  MK40S01;
110200130709         EndDo;
110300130709
110400130709       ENDSR;
110500130628
110600130628       //--------------------------------------------------------------
110700130628       //?Ordinamento subfile                                          ?
110800130628       //?  This subroutine sorts the subfile records.                 ?
110900130628       //--------------------------------------------------------------
111000130628       BEGSR  sr_SortSfl;
111100130628
111200130701         // -?Impostazione NRR dell'ultimo record del subfile          ?
111300130701         //  ?(già fatto in sr_InzS01:                                 ?
111400130701         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
111500130701         //RrnLast  = S01nrr;
111600130628
111700130628         C1RcdNbr = 1;
111800130628         clear  C1CsrRrn;
111900130628         clear  S01nrr;
112000130628         clear  V1Dmsg;
112100130628         SflNxtChg   = *off;
112200130711         ErrMessage  = *off;
112300130628         ErrGenerico = *off;
112400130628         %subst(IndDspF : 50) = *off;
112500130628
112600130701         //?___________________________________________________________?
112700130701         //?Initialize the key fields to sort on.?
112800130701         //?There is one extra field not in the subfile -- Selected --?
112900130701         //?that is added to the record. This field is used to place?
113000130701         //?selected records in front of nonselected records.?
113100130701         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
113200130628
113300130628         clear  QLGKL;
113400130628         clear  QLGSKL;
113500130628         clear  QLGSCB;
113600130628         clear  QLGSCB00;
113700130628
113800130701         // -?Gestione della scelta ordinamento:?
113900130701         Select;
114000130710
114100130701           // -?Ordinamento per Codice Commerciale?
114200130708           When  $Ord_x_CodCmm;
114300130701             // -?1 campi chiave?
114400130701             QLGNBRK  = 1;
114500130708             // -?1° campo: codice comm.le (S1Ccmm)?
114600130701             //            ?a posizone    3,  7 byte, int,  ascending.?
114700130710             QLGSP    = %size(S1Copz) + 1;
114800130708             QLGSS    = %size(S1Ccmm);
114900130725             QLGDT    = c_NumIntero;
115000130725             QLGSO    = c_Ascendente;
115100130701             QLGKL(1) = QLGSKL;
115200130701
115300130711           // -?Ordinamento per Descrizione Commerciale?
115400130708           when  $Ord_x_DesCmm;
115500130701             // -?2 campi chiave?
115600130701             QLGNBRK  = 2;
115700130711             // -?1° campo: descr. comm.le (S1Dcmm)?
115800130711             //            ?a posizone   10, 25 byte, char, ascending.?
115900130710             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + 1;
116000130708             QLGSS    = %size(S1Dcmm);
116100130725             QLGDT    = c_Carattere;
116200130725             QLGSO    = c_Ascendente;
116300130701             QLGKL(1) = QLGSKL;
116400130708             // -?2° campo: descr. comm.le (S1Ccmm)?
116500130701             //            ?a posizone    3,  7 byte, int,  ascending.?
116600130710             QLGSP    = %size(S1Copz) + 1;
116700130708             QLGSS    = %size(S1Ccmm);
116800130725             QLGDT    = c_NumIntero;
116900130725             QLGSO    = c_Ascendente;
117000130701             QLGKL(2) = QLGSKL;
117100130628
117200130701           // -?Ordinamento per Codice Unificante?
117300130701           when  $Ord_x_CodUni;
117400130701             // -?2 campi chiave?
117500130701             QLGNBRK  = 2;
117600130701             // -?1° campo: codice unificante (S1Cuni)?
117700130711             //            ?a posizone   35,  7 byte, int,  ascending.?
117800130710             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + %size(S1Dcmm) + 1;
117900130701             QLGSS    = %size(S1Cuni);
118000130725             QLGDT    = c_NumIntero;
118100130725             QLGSO    = c_Ascendente;
118200130701             QLGKL(1) = QLGSKL;
118300130708             // -?2° campo: cod. comm.le (S1Ccmm)?
118400130701             //            ?a posizone    3,  7 byte, int,  ascending.?
118500130710             QLGSP    = %size(S1Copz) + 1;
118600130708             QLGSS    = %size(S1Ccmm);
118700130725             QLGDT    = c_NumIntero;
118800130725             QLGSO    = c_Ascendente;
118900130701             QLGKL(2) = QLGSKL;
119000130628
119100130701         EndSl;
119200130628
119300130701         // -?Load other sort parameters.?
119400130725         QLGLB   = 80 + 16 * c_MaxKey;
119500130628         QLGRL   = %size(SflRcd) - 1;
119600130628         QLGRT   = 8;
119700130628         QLGOKL  = 80;
119800130628         QLGLKE  = 16;
119900130628         QLGLSS  = 290;
120000130701         // -?Initialize Sort I/O API fields.?
120100130628         QLGRL00 = QLGRL;
120200130628         QLGRC00 = 1;
120300130628         clear  QUSEI;
120400130628         QUSBPRV = %size(QUSEC);
120500130628
120600130701         // -?First step - Initialize the sort routine.?
120700130628         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
120800130628                      SizeList : ReturnSize : QUSEC );
120900130628
121000130701         // -?Next step - Write records to I/O routine.?
121100130725         QLGRT00  = c_Put;
121200130628         For  Nrr = 1  To  RrnLast;
121300130701           chain  Nrr  MK40S01;
121400130701           // -?Solo le righe con Selected = 'Y' sono riordinate,?
121500130701           //  ?quindi per fare un ordinamento di tutte le righe?
121600130701           //  ?metto 'Y' sempre.?
121700130628           Selected = 'Y';
121800130628           clear  QUSEI;
121900130628           QUSBPRV  = %size(QUSEC);
122000130628           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122100130628                         SizeList : NotUsed : QUSEC );
122200130628         EndFor;
122300130628
122400130701         // -?Next step - Signal end of input, clear subfile for reload.?
122500130725         QLGRT00 = c_EndPut;
122600130628         clear  QUSEI;
122700130628         QUSBPRV = %size(QUSEC);
122800130628         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
122900130628                       SizeList : NotUsed : QUSEC );
123000130628
123100130701         // -?Pulizia subfile?
123200130628         SflDsp_N    = *on;
123300130628         SflDspCtl_N = *on;
123400130701         write  MK40C01;
123500130628         SflDspCtl_N = *off;
123600130628         SflEnd      = *off;
123700130628
123800130701         // -?Final step - Write the records back to the subfile.?
123900130725         QLGRT00  = c_Get;
124000130628         For  Nrr = 1  To RrnLast;
124100130628           clear  QUSEI;
124200130628           QUSBPRV = %size(QUSEC);
124300130628           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
124400130628                         QLGRL00  : NotUsed : QUSEC );
124500130701           // -?Ripristino indicatori utilizzati nel sfl rec?
124600130701           SflNxtChg = (S1Copz <> *zero);
124700130709           Ord_1 = ( $Ord_x_DesCmm );
124800130709           Ord_2 = ( $Ord_x_CodUni );
124900130628           S01nrr = Nrr;
125000130701           write  MK40S01;
125100130628         EndFor;
125200130628
125300130628         C1CsrRrn = 1;
125400130628
125500130701         // -?Visualizzazione del SFL (se ci sono dati)?
125600130628         SflDsp_N = (S01nrr <= *zero);
125700130628
125800130701         // -?Attivazione del SFLEND?
125900130628         SflEnd = $EoF;
126000130701
126100130701         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
126200130701         Select;
126300130708           When  $Ord_x_CodCmm;
126400130708             P01f08 = 'F8=Ord. x Descrizione';
126500130709           When  $Ord_x_DesCmm;
126600130701             P01f08 = 'F8=Ord. x Unificante ';
126700130701           When  $Ord_x_CodUni;
126800130701             P01f08 = 'F8=Ord. x Cod.Comm.le';
126900130701         EndSl;
127000130628
127100130628       ENDSR;
127200130701
127300130701       //--------------------------------------------------------------
127400130701       //?Richiamo *pgm di manutenzione singolo commerciale.         ?
127500130701       //--------------------------------------------------------------
127600130701       BEGSR  sr_Call_TRMK41;
127700130701
127800130701         clear  TRMK41ds;
127900130708         clear  AZCMM_ds;
128000130701         if  dsp_aid <> c_F06;
128100130701           iMK41opz = S1Copz;
128200130708           iMK41cmm = S1Ccmm;
128300130701         endif;
128400130701
128500130701         kpjbu = TRMK41ds;
128600130708         trmk41r ( kpjba : AZCMM_ds );
128700130701         TRMK41ds = KPJBU;
128800130701
128900130701         Select;
129000130710
129100130710           // -?Restituito errore?
129200130705           When  oMK41err = *on;
129300130705             V1Dmsg = oMK41msg;
129400130705             ErrMessage  = *on;
129500130705             ErrGenerico = *on;
129600130710
129700130710           // -?Uscito premendo F3 o F12?
129800130705           When  oMK41fxx = '03'  or  oMK41fxx = '12';
129900130701             leavesr;
130000130710
130100130701           // -?Eseguito Inserimento o Copia?
130200130701           When  dsp_aid = c_F06  or  S1Copz = 3;
130300130701             $InzS01 = *on;
130400130701             $Almeno1rec = *on;
130500130710
130600130701           // -?Eseguita Modifica?
130700130710           When  S1Copz = 2;
130800130725             $InzS01 = (C1Cuni   > *zero  and  S1Cuni <> AZCMM_ds.CMMuni)     or
130900130725                       (C1CsolU  = 'S'    and  S1Cuni <> AZCMM_ds.CMMuni)     or
131000130725                       (C1CdtAtt > *zero  and  (AZCMM_ds.CMMdtIni > W1CdtAtt  or
131100130725                                                AZCMM_ds.CMMdtFin < W1CdtAtt));
131200130708             S1Dcmm = AZCMM_ds.CMMdes;
131300130708             S1Cuni = AZCMM_ds.CMMuni;
131400130708             if  AZCMM_ds.CMMdip <> *zero  or  AZCMM_ds.CMMsoc <> *blank;
131500130709               S1Cdip = AZCMM_ds.CMMsoc + '/' + %editc( AZCMM_ds.CMMdip : 'Z');
131600130725             else;
131700130725               clear  S1Cdip;
131800130708             endif;
131900130708             S1Cpli = AZCMM_ds.CMMpli;
132000130708             S1Cple = AZCMM_ds.CMMple;
132100130710
132200130710           // -?Eseguita Cancellazione?
132300130710           When  S1Copz = 4;
132400130710             $InzS01 = *on;
132500130710
132600130701         EndSl;
132700130701
132800130701       ENDSR;
132900130628
133000130628       //--------------------------------------------------------------
133100130628       //?Operazioni finali.                                         ?
133200130628       //--------------------------------------------------------------
133300130628       BEGSR  sr_RoutEnd;
133400130628
133500130701         // -?Chiusura *pgm?
133600130628         return;
133700130628
133800130628       ENDSR;
133900130628
134000130628      /end-free
134100130628
134200130701** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
134300130701Raggiunto il limite massimo dei record caricabili a video                      1
134400150108Non sono ammesse opzioni quando è richiesta una selezione                      2
134500130701Opzione errata                                                                 3
134600130701Filiale inesistente                                                            4
134700130701Filiale annullata                                                              5
134800130701Filiale NON valida                                                             6
134900130709Immettere un'opzione su questa pagina prima di premere F13                     7
135000130709Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 8
135100130709Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 9
135200130725Data formalmente errata                                                       10
