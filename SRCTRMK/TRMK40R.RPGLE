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
014700170330
014800170330       // -?Descrizioni dell'organigramma:?
014900170330     d Og143         e ds                  inz  qualified
015000130628
015100130628       //--------------------------------------------------------------
015200130628       //?Definizione variabili globali.                               ?
015300130628       //--------------------------------------------------------------
015400130628
015500130628       // -?Flags booleani?
015600130701     d $EoF            s               n   inz
015700130628     d $Fine           s               n   inz
015800130628     d $InzS01         s               n   inz(*on)
015900130701     d $Almeno1rec     s               n   inz
016000130709     d $Ord_x_CodCmm   s               n   inz
016100130709     d $Ord_x_DesCmm   s               n   inz
016200130709     d $Ord_x_CodUni   s               n   inz
016300130709     d $ForzaOpz       s               n   inz
016400130628
016500130628       // -?Variabili per la gestione del video?
016600130628     d $Video          s              2    inz('S1')
016700130709     d S01nrr          s                   like(C1RcdNbr)  inz
016800130725     d SavS1Copz       s                   like(S1Copz)    inz
016900130725     d SavS1Cop1       s                   like(S1Copz)    inz
017000130709     d SavS01csr       s                   like(C1RcdNbr)  inz
017100130725     d SavS01cs1       s                   like(C1RcdNbr)  inz
017200130725     d SavS01cs2       s                   like(C1RcdNbr)  inz
017300130709     d wPag            s              4  0 inz
017400130709     d wRig            s              3  0 inz
017500130628
017600130628       // -?Variabili per la gestione del posizionamento nel subfile?
017700130725     d SavC1Cfil       s                   like(C1Cfil)    inz
017800130725     d SavC1Cuni       s                   like(C1Cuni)    inz
017900130725     d SavC1CsolU      s                   like(C1CsolU)   inz
018000130725     d SavC1CdtAtt     s                   like(C1CdtAtt)  inz
018100130628
018200130628       // -?Campi di comodo?
018300130725     d wDate           s              8s 0 inz
018400130725     d W1CdtAtt        s              8s 0 inz
018500130628
018600130628       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
018700130628
018800130628       //--------------------------------------------------------------
018900130628       // -?Constants?
019000130628       //--------------------------------------------------------------
019100130628       // -?MaxKey - Maximum number of key fields allowed by this program?
019200130725     d c_MaxKey        c                   const(2)
019300130628       // -?Sort order:?
019400130628       //  ?1 = Sort in an ascending sequence?
019500130628       //  ?2 = Sort in a descending sequence?
019600130725     d c_Ascendente    c                   const(1)
019700130725     d c_Discendente   c                   const(2)
019800130628       // -?Key data type:?
019900130628       //  ? 0 = Signed binary?
020000130628       //  ? 2 = Signed zoned decimal?
020100130628       //  ? 3 = Signed packed decimal?
020200130628       //  ? 6 = Character with no national language sort sequence applied,?
020300130628       //  ?     if specified?
020400130628       //  ? 7 = Unsigned packed decimal?
020500130628       //  ?     All numbers will have the sign forced positive ('F'X).?
020600130628       //  ? 8 = Unsigned zoned decimal?
020700130628       //  ?     All numbers will have the sign forced positive ('F'X).?
020800130628       //  ? 9 = Unsigned binary?
020900130628       //  ?14 = Date in form of DD/MM/YY?
021000130628       //  ?15 = Date in form of DD.MM.YYYY?
021100130725     d c_Numero        c                   const(2)
021200130725     d c_Carattere     c                   const(6)
021300130725     d c_NumIntero     c                   const(8)
021400130628       //
021500130725     d c_Put           c                   const(1)
021600130725     d c_EndPut        c                   const(2)
021700130725     d c_Get           c                   const(3)
021800130628
021900130628       //--------------------------------------------------------------
022000130628       // -?Data Structures?
022100130628       //  ?SflRcd     - The entire subfile record to pass to the sort?
022200130628       //  ?QLGSCB     - The sort request block for the QLGSORT API?
022300130628       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
022400130628       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
022500130628       //  ?QUSEC      - Error structure for the QLGSORT API?
022600130628       //--------------------------------------------------------------
022700130628     d SflRcd          ds                  inz
022800130701     d   S1Copz                            inz
022900130708     d   S1Ccmm                            inz
023000130708     d   S1Dcmm                            inz
023100130628     d   S1Cuni                            inz
023200130628     d   S1Cdip                            inz
023300130628     d   S1Cpli                            inz
023400130628     d   S1Cple                            inz
023500130628     d   Selected                     1a   inz
023600130628      /copy QSYSINC/QRPGLESRC,QLGSORT
023700170331     d*// QLGKL                         20    dim(c_MaxKey)
023800170331     d QLGKL                         16    dim(c_MaxKey)
023900130628     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
024000130628     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
024100130628     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
024200130628     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
024300170331     d*//QLGOP00                      9b 0 overlay(QLGKL:00017)
024400130628      /copy QSYSINC/QRPGLESRC,QLGSRTIO
024500130628      /copy QSYSINC/QRPGLESRC,QUSEC
024600130628
024700130628       //--------------------------------------------------------------
024800130628       // -?Standalone fields?
024900130628       //  ?Nrr        - Relative record number for subfile?
025000130628       //  ?SizeList   - Size of list?
025100130628       //  ?ReturnSize - Size of list returned by sort API?
025200130628       //--------------------------------------------------------------
025300130628     d NotUsed         s             16a   inz
025400170330     d ReturnSize      s             10i 0 inz
025500170330     d SizeList        s             10i 0 inz
025600130628     d RrnLast         s              5i 0 inz
025700130628     d Nrr             s              5i 0 inz
025800130628
025900130628       //--------------------------------------------------------------
026000130628       //?Definizione prototipi procedure usate.                       ?
026100130628       //--------------------------------------------------------------
026200130628
026300130628       // -?Reperimento dati utente?
026400130628     d TIBS34ds      e ds                  inz
026500130628      /copy gaitrasrc/srcProtoPR,TIBS34R
026600130725
026700130725       // -?Controllo formale / Inversione data?
026800130725     d WLBdat          ds                  inz
026900130725     d   G02dat                1      8  0 inz
027000130725     d   G02inv                9     16  0 inz
027100130725     d   G02err               17     17    inz('3')
027200130725     d   G02tgi               18     22  0 inz
027300130725      /copy gaitrasrc/srcProtoPR,XSRDA8
027400130701
027500130708       // -?Manutenzione singolo commerciale?
027600130701      /copy gaitrasrc/srcProtoPI,TRMK41R
027700130701      /copy gaitrasrc/srcProtoPR,TRMK41R
027800130628
027900130628       // -?Ordinamento subfile?
028000130628      /copy gaitrasrc/srcProtoPR,QLGSRTIO
028100130628
028200130628       //--------------------------------------------------------------
028300130628       //?Definizione key-list.                                        ?
028400130628       //--------------------------------------------------------------
028500130628
028600130708       // -?File AZCMM01L?
028700130708     d k01azcmm01    e ds                  extname(AZCMM01L : *key)
028800130628     d                                     prefix(k_)   inz
028900130628
029000130709       // -?File AZNTC01L?
029100130709     d*// k01azntc01    e ds                  extname(AZNTC01L : *key)
029200130628     d*//                                     prefix(k_)   inz
029300130628
029400130628       //--------------------------------------------------------------
029500130628       //?Riepilogo indicatori utilizzati.                             ?
029600130628       //--------------------------------------------------------------
029700130628       //--------------------------------------------------------------
029800130628
029900130628       //--------------------------------------------------------------
030000130628       //?M A I N - L I N E                                            ?
030100130628       //--------------------------------------------------------------
030200130628
030300130628     c     *Entry        plist
030400130628     c                   parm                    KPJBA
030500130708     c                   parm                    prm_AZCMM_ds
030600130709     c*//                parm                    prm_AZNTC01_ds
030700130709     c*//                parm                    prm_AZNTC02_ds
030800130628
030900130628      /free
031000130628
031100130628       // -?Operazioni iniziali?
031200130628       exsr sr_RoutInz;
031300130628
031400130628       // -?Ciclo di gestione del file video?
031500130628       DoW  $Fine = *off;
031600130628
031700130628         select;
031800130628
031900130628           // -?Gestione videata S1 (subfile)?
032000130628           when $Video = 'S1';
032100130628             exsr sr_GesS01;
032200130628
032300130628           // -?? ? ??
032400130628           other;
032500130628             $Fine = *on;
032600130628
032700130628         endsl;
032800130628
032900130628       EndDo;
033000130628
033100130628       // -?Operazioni finali?
033200130628       exsr sr_RoutEnd;
033300130628
033400130628       //--------------------------------------------------------------
033500130628       //?Operazioni iniziali.                                         ?
033600130628       //--------------------------------------------------------------
033700130628       BEGSR  sr_RoutInz;
033800130628
033900130628         // -?Impostazione chiusura?
034000130628         *inLR = *on;
034100130628
034200130628         // -?Reperimento dati job?
034300130628         exsr  sr_DatiJob;
034400130628
034500130628         // -?Reperimento data odierna?
034600130725         wDate = %dec( %date() );
034700130628
034800130628         // -?Impostazione nome programma a video?
034900130628         V1Tpgm = SDSpgm;
035000130628
035100130628         // -?Impostazione videata iniziale?
035200130701         clear  MK40C01;
035300130628         $Video = 'S1';
035400130628         reset  $InzS01;
035500130708         $Ord_x_CodCmm = *on;
035600130708         $Ord_x_DesCmm = *off;
035700130701         $Ord_x_CodUni = *off;
035800130701
035900130701         // -?Verifica parametri ricevuti?
036000130701         Opz_Selez = ( %parms() > 1 );
036100130701
036200130701         // -?Impostazione parzializzazioni richieste (se ricevute)?
036300130701         clear  TRMK40ds;
036400130701         if  kpjbu <> *blank;
036500130701           TRMK40ds = KPJBU;
036600130701         endif;
036700130725         C1Cfil   = iMK40fil;
036800130725         C1cuni   = iMK40uni;
036900130725         C1CsolU  = iMK40solU;
037000130725         if  iMK40dtAtt > *zero;
037100130725           reset  WLBdat;
037200170330           G02inv  = iMK40dtAtt;
037300130725           xsrda8 ( WLBdat );
037400130725           C1CdtAtt = G02dat;
037500130725           W1CdtAtt = G02inv;
037600130725         endif;
037700130701         clear  oMK40fxx;
037800130701         oMK40err = *off;
037900130701
038000130701         // -?Pulizia eventuali parametri di output (se richiesta selezione)?
038100130701         if  %parms() > 1;
038200130708           clear  prm_AZCMM_ds;
038300130709           //clear  prm_AZNTC01_ds;
038400130709           //clear  prm_AZNTC02_ds;
038500130701         endif;
038600130701
038700130701         // -?Impostazione videata iniziale?
038800130701         $Video = 'S1';
038900130701         reset  $InzS01;
039000130708         $Ord_x_CodCmm = *on;
039100130701         exsr  sr_InzS01;
039200130701         $InzS01 = *off;
039300130701         $Almeno1rec = (S01nrr > *zero);
039400130628
039500130628       ENDSR;
039600130628
039700130628       //--------------------------------------------------------------
039800130628       //?Reperimento Dati del job (Utente/Operativi).                 ?
039900130628       //--------------------------------------------------------------
040000130628       BEGSR  sr_DatiJob;
040100130628
040200130628         in(e) §AzUte;
040300130628         if NOT %error;
040400130628           in(e) §DatiUte;
040500130628         endif;
040600130628         if %error or RSut = *blank;
040700130628           tibs34r ( tibs34ds );
040800130628           in §AzUte;
040900130628           in §DatiUte;
041000130628         endif;
041100130628
041200130628       ENDSR;
041300130628
041400130628       //--------------------------------------------------------------
041500130628       //?Gestione subfile S01                                         ?
041600130628       //--------------------------------------------------------------
041700130628       BEGSR  sr_GesS01;
041800130628
041900130628         // -?Inizializzazione videata?
042000130628         if  $InzS01 = *on;
042100130628           exsr  sr_InzS01;
042200130628           $InzS01 = *off;
042300130628         endif;
042400130628
042500130628         // -?Emissione Testata & Piede con tasti funzionali abilitati?
042600130628         write  MK40T01;
042700130628         write  MK40P01;
042800130628
042900130628         // -?Posizionamento cursore?
043000130628         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
043100130628         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
043200130628         //  ?si visualizza la pagina che vedeva l'utente quando ha?
043300130628         //  ?premuto l'ultimo tasto.?
043400130628         if  C1CsrRrn > *zero;
043500130628           C1RcdNbr = C1CsrRrn;
043600130628         else;
043700130628           C1RcdNbr = 1;
043800130628           write  MK40S00;          //?(no rec.)?
043900130628         endif;
044000130628
044100130628         // -?Emissione videata?
044200130628         exfmt  MK40C01;
044300130628
044400130628         clear  V1Dmsg;
044500130628         reset  ErrMessage;
044600130628         reset  ErrGenerico;
044700130709
044800130709         if  dsp_aid <> c_F03  and
044900130709             dsp_aid <> c_F13;
045000130709           clear  SavS01csr;
045100130725           clear  SavS01cs1;
045200130725           clear  SavS01cs2;
045300130709           clear  SavS1Copz;
045400130725           clear  SavS1Cop1;
045500130709         endif;
045600130628
045700130628         SELECT;
045800130628
045900130628           // -?F3=Fine?
046000130712           WHEN  dsp_aid = c_F03;
046100130701             oMK40fxx = '03';
046200130628             $Fine = *on;
046300130628
046400150112           // -?F5=Refresh?
046500130712           WHEN  dsp_aid = c_F05;
046600130628             $InzS01 = *on;
046700130628
046800130628           // -?F6=Immissione?
046900130712           WHEN  dsp_aid = c_F06;
047000130628             exsr  sr_Call_TRMK41;
047100130705             if  oMK41fxx = '03';
047200130705               $Fine = *on;
047300130705             endif;
047400130628
047500130628           // -?F8=Ordinamento?
047600130628           WHEN  dsp_aid = c_F08;
047700130628             exsr  sr_F08S01;
047800130628
047900130628           // -?F12=Ritorno?
048000130628           WHEN  dsp_aid = c_F12;
048100130701             oMK40fxx = '12';
048200130628             $Fine = *on;
048300130709
048400130709           // -?F13=Ripetizione Opzione?
048500130709           WHEN  dsp_aid = c_F13;
048600130709             exsr  sr_F13S01;
048700130628
048800130628           // -?Invio?
048900130628           OTHER;
049000130725             // -?Controllo dati nel SflCtrl?
049100130725             exsr  sr_CtrC01;
049200130725             if  ErrGenerico;
049300130725               leavesr;
049400130725             endif;
049500130701             // -?Gestione opzioni?
049600130628             if  S01nrr > *zero;
049700130628               exsr  sr_OpzS01;
049800130628               if  ErrGenerico;
049900130628                 leavesr;
050000130628               endif;
050100130628             endif;
050200130725             // -?Selezioni?
050300130725             // -?Selezionata una filiale diversa      e/o?
050400130725             //  ?Richiesti comm.li di un unificante   e/o?
050500130725             //  ?Richiesti SOLO unificanti            e/o?
050600130725             //  ?Richiesti comm.li attivi alla data?
050700130725             If  C1Cfil   <> SavC1Cfil    or
050800130725                 C1Cuni   <> SavC1Cuni    or
050900130725                 C1CsolU  <> SavC1CsolU   or
051000130725                 C1CdtAtt <> SavC1CdtAtt;
051100130712               // -?Selezionata una filiale diversa?
051200130725               if  C1Cfil <> SavC1Cfil;
051300170330                 exsr  sr_CtrlFIL;
051400130712                 if  ErrMessage;
051500130712                   leavesr;
051600130712                 endif;
051700130712               endif;
051800130712               $InzS01 = *on;
051900130712             EndIf;
052000130628
052100130628         ENDSL;
052200130628
052300130628       ENDSR;
052400130628
052500130628       //--------------------------------------------------------------
052600130628       //?Inizializzazione subfile S01                                 ?
052700130628       //--------------------------------------------------------------
052800130628       BEGSR  sr_InzS01;
052900130628
053000130701         // -?Spegnimento degli indicatori di errore?
053100130628         %subst(IndDspF : 50) = *off;
053200130628
053300130701         // -?Pulizia subfile?
053400130628         SflDsp_N    = *on;
053500130628         SflDspCtl_N = *on;
053600130701         write  MK40C01;
053700130628         SflDspCtl_N = *off;
053800130628         SflEnd      = *off;
053900130628
054000150108         SavC1Cfil   = C1Cfil;
054100150108         SavC1Cuni   = C1Cuni;
054200150108         SavC1CsolU  = C1CsolU;
054300150108         SavC1CdtAtt = C1CdtAtt;
054400150108
054500130628         clear  C1RcdNbr;
054600130628         clear  C1CsrRrn;
054700130628         clear  S01nrr;
054800130628         clear  V1Dmsg;
054900130628         ErrMessage  = *off;
055000130628         ErrGenerico = *off;
055100130628
055200130701         Select;
055300130708           When  $Ord_x_CodCmm;
055400130701             Ord_1 = *off;
055500130701             Ord_2 = *off;
055600130708           When  $Ord_x_DesCmm;
055700130701             Ord_1 = *on;
055800130701             Ord_2 = *off;
055900130701           When  $Ord_x_CodUni;
056000130701             Ord_1 = *off;
056100130701             Ord_2 = *on;
056200130701         EndSl;
056300130701
056400130701         // -?Caricamento completo dei dati nel subfile (per codice)?
056500130701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
056600130701         //  ?l'eventuale ordinamento)?
056700130708         clear  k01azcmm01;
056800130725         if  C1Cfil <> *zero;
056900130725           k_CMMcod = C1Cfil * 10000;
057000130725         else;
057100130725           k_CMMcod = *loval;
057200130725         endif;
057300130708         setll  %kds(k01azcmm01)  AZCMM000;
057400130708         exsr  sr_ReadAZCMM;
057500130701         if  Not  $EoF;
057600130701           exsr  sr_CaricaS01;
057700130701         endif;
057800130628
057900130701         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
058000130701         RrnLast  = S01nrr;
058100130701
058200130701         // -?Impaginazione subfile?
058300130701         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
058400130628         if S01nrr > *zero;
058500130628           C1RcdNbr  = 1;
058600130628           C1CsrRrn  = 1;
058700130628         else;
058800130628           clear C1RcdNbr;
058900130628         endif;
059000130628
059100130701         // -?Ordinamento subfile (se premuto F8)?
059200130708         if  Not $Ord_x_CodCmm  and  S01nrr > *zero;
059300130628           exsr  sr_SortSfl;
059400130628         endif;
059500130628
059600130701         // -?(Dis)Abilitazione tasti funzionali?
059700130709         F3Attivo  = (%parms() = 1);   // -?F3 = Fine?
059800130709         F5Attivo  = *on;              // -?F5 = Refresh?
059900130709         F6Attivo  = (%parms() = 1);   // -?F6 = Immissione?
060000130709         F8Attivo  = (S01nrr > *zero  or  $Almeno1rec); // -?F8 = Ordinamento x ...?
060100130709         //F12Attivo = (%parms() > 1);   // -?F12 = Ritorno?
060200130709         F12Attivo = *off;             // -?F12 = Ritorno?
060300130709         F13Attivo = (%parms() = 1  and  S01nrr > 1);   // -?F13 = Ripetizione opz.?
060400130701
060500130701         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
060600130701         Select;
060700130701           When  Not F8Attivo;
060800130701             clear  P01f08;
060900130708           When  $Ord_x_CodCmm;
061000130708             P01f08 = 'F8=Ord. x Descrizione';
061100130708           When  $Ord_x_DesCmm;
061200130701             P01f08 = 'F8=Ord. x Unificante ';
061300130701           When  $Ord_x_CodUni;
061400130701             P01f08 = 'F8=Ord. x Cod.Comm.le';
061500130701         EndSl;
061600130701
061700130628       ENDSR;
061800130628
061900130628       //--------------------------------------------------------------
062000130708       //?Ricerca del 1° record valido successivo in AZCMM00F          ?
062100130628       //--------------------------------------------------------------
062200130708       BEGSR  sr_ReadAZCMM;
062300130628
062400130701         reset  $EoF;
062500130628
062600130628         DoW  Not $EoF;
062700130628
062800130708           read  AZCMM000;
062900130628
063000130628           select;
063100130628
063200130701             // -?End Of File => Fine?
063300130708             when  %eof(AZCMM01L);
063400130701               $EoF = *on;
063500130701               leavesr;
063600130725
063700130725             // -?Richiesti solo i comm.li di una filiale:?
063800130725             //  ?si scartano i comm.li di filiale maggiore => Fine?
063900130725             when  C1Cfil <> *zero     and
064000130725                   C1Cfil <> %int( CMMcod / 10000 );
064100130725               $EoF = *on;
064200130725               leavesr;
064300130722
064400130722             // -?Richiesti solo i comm.li di un unificante:?
064500130722             //  ?si scartano i comm.li con unificante diverso => Read Next?
064600130725             when  C1Cuni > *zero      and  C1Cuni <> CMMuni;
064700130722               iter;
064800130628
064900130701             // -?Richiesti solo i comm.li unificanti:?
065000130701             //  ?si scartano i comm.li NON unificanti => Read Next?
065100130725             when  C1CsolU = 'S'       and  CMMcod <> CMMuni;
065200130628               iter;
065300130701
065400130725             // -?Richiesti solo i comm.li attivi ad una data:?
065500130725             //  ?si scartano i comm.li non ancora/più attivi => Read Next?
065600130725             when  C1CdtAtt > *zero    and
065700130725                  (CMMdtIni > W1CdtAtt  or  CMMdtFin < W1CdtAtt);
065800130701               iter;
065900130628
066000130701             // -?Superato il limite max di rec. caricabili nel sfl?
066100130701             //  ?=> Fine?
066200130628             when  S01nrr >= c_MaxRec;
066300130701               leavesr;
066400130628
066500130708             // -?Richiesti tutti i commerciali => Ok (uscita)?
066600130628             other;
066700130628               leave;
066800130628
066900130628           endsl;
067000130628
067100130628         EndDo;
067200130628
067300130628       ENDSR;
067400130628
067500130628       //--------------------------------------------------------------
067600130702       //?Caricamento di tutto il subfile S01.                         ?
067700130628       //--------------------------------------------------------------
067800130701       BEGSR  sr_CaricaS01;
067900130628
068000130702         clear  S01nrr;
068100130628         SflNxtChg = *off;
068200130628
068300130708         // -?Ciclo di caricamento dati da file AZCMM00F?
068400130701         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
068500130701         //  ?l'eventuale ordinamento)?
068600130628         DoW  Not $EoF;
068700130628
068800130701           // -?Impostazione campi nel record del subfile?
068900130701           clear  MK40S01;
069000130708           S1Ccmm = CMMcod;
069100130708           S1Dcmm = CMMdes;
069200130708           S1Cuni = CMMuni;
069300130708           if  CMMdip <> *zero  or  CMMsoc <> *blank;
069400130709             S1Cdip = CMMsoc + '/' + %editc( CMMdip : 'Z');
069500130701           endif;
069600130708           S1Cpli = CMMpli;
069700130708           S1Cple = CMMple;
069800130701
069900130701           // -?Impostazione attributi di visualizzazione?
070000130709           Ord_1 = ($Ord_x_DesCmm);
070100130709           Ord_2 = ($Ord_x_CodUni);
070200130725           CmmScad = ( CMMdtIni > wDate  or  CMMdtFin < wDate );
070300130628
070400130701           // -?Registrazione singolo record nel subfile?
070500130628           S01nrr += 1;
070600130701           write  MK40S01;
070700130628
070800130701           // -?Lettura record successivo?
070900130708           exsr  sr_ReadAZCMM;
071000130628
071100130628         EndDo;
071200130628
071300130701         // -?Visualizzazione del SFL (se ci sono dati)?
071400130628         SflDsp_N  = (S01nrr <= *zero);
071500130628
071600130701         // -?Attivazione del SFLEND?
071700130628         SflEnd = $EoF;
071800130701
071900130701         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
072000130701         if  S01nrr > *zero;
072100130701           C1RcdNbr = 1;
072200130701         else;
072300130701           clear  C1RcdNbr;
072400130701         endif;
072500130628
072600130701         // -?Emissione messaggio di segnalazione raggiungimento limite?
072700130708         if  S01nrr >= c_MaxRec   and   Not %eof(AZCMM01L);
072800130702           ErrGenerico = *on;
072900130628           ErrMessage  = *on;
073000130702           PosCurFIL   = *on;
073100130701           V1Dmsg = $Msg(01);
073200130628           leavesr;
073300130628         endif;
073400130628
073500130628       ENDSR;
073600130725
073700130725       //--------------------------------------------------------------
073800130725       //?Controllo dati inseriti nel subfile-control                  ?
073900130725       //--------------------------------------------------------------
074000130725       BEGSR  sr_CtrC01;
074100130725
074200130725         If  C1CdtAtt <> *zero  and  C1CdtAtt <> SavC1CdtAtt;
074300130725           clear  WLBdat;
074400130725           G02dat = C1CdtAtt;
074500130725           xsrda8 ( WLBdat );
074600130725           if  G02err = *on;
074700130725             V1Dmsg = $Msg(10);
074800130725             PosCurDtATT = *on;
074900130725             ErrMessage  = *on;
075000130725             ErrGenerico = *on;
075100130725             leavesr;
075200130725           endif;
075300130725           C1CdtAtt = G02dat;
075400130725           W1CdtAtt = G02inv;
075500130725         EndIf;
075600130725
075700130725       ENDSR;
075800130628
075900130628       //--------------------------------------------------------------
076000130628       //?Gestione opzioni del subfile S01                             ?
076100130628       //--------------------------------------------------------------
076200130628       BEGSR  sr_OpzS01;
076300130701
076400130701         // -?Spegnimento degli indicatori di errore?
076500130701         %subst(IndDspF : 50) = *off;
076600130628
076700130628         clear  SavS01csr;
076800130628
076900130701         // -?Ciclo di lettura subfile?
077000130701         readc  MK40S01;
077100130628
077200130701         DoW  Not %eof(TRMK40D);
077300130628
077400130628           SflNxtChg = *off;
077500130628           %subst(IndDspF : 50) = *off;
077600130628           C1RcdNbr = S01nrr;
077700130628
077800130628           select;
077900130701
078000130701             // -?Nessuna opzione?
078100130701             when  S1Copz = *zero;
078200130701
078300130725             // -?Opzione + Parzializzazioni?
078400130725             when  S1Copz   <> *zero      and
078500130725                  (C1Cfil   <> SavC1Cfil   or
078600130725                   C1Cuni   <> SavC1Cuni   or
078700130725                   C1CsolU  <> SavC1CsolU  or
078800130725                   C1CdtAtt <> SavC1CdtAtt);
078900130702               ErrGenerico = *on;
079000130701               ErrMessage  = *on;
079100130702               select;
079200130725                 when C1Cfil   <> SavC1Cfil;
079300130725                   PosCurFIL   =  *on;
079400130725                 when C1Cuni   <> SavC1Cuni;
079500130725                   PosCurUNI   =  *on;
079600130725                 when C1CsolU  <> SavC1CsolU;
079700130725                   PosCurSolU  =  *on;
079800130725                 when C1CdtAtt <> SavC1CdtAtt;
079900130725                   PosCurDtAtt =  *on;
080000130702               endsl;
080100130701               V1Dmsg = $Msg(02);
080200130628
080300130701             // -?Opz. 1=Selezione commerciale?
080400130701             when  Opz_Selez  and  S1Copz = 1;
080500130708               k_CMMcod = S1Ccmm;
080600130708               chain  %kds(k01azcmm01)  AZCMM000;
080700130708               if  %found(AZCMM01L);
080800130708                 prm_AZCMM_ds = AZCMM_ds;
080900130701               else;
081000130708                 p_CMMcod = S1Ccmm;
081100130708                 p_CMMdes = S1Dcmm;
081200130701               endif;
081300130628               $Fine = *on;
081400130628               ErrGenerico = *on;
081500130701
081600130701             // -?Opz. 2=Modifica?
081700130701             //  ?Opz. 3=Copia?
081800130710             //  ?Opz. 4=Cancellazione?
081900130701             //  ?Opz. 5=Visualizzazione?
082000130702             when  Not Opz_Selez   and
082100130701                  (S1Copz = 2  or  S1Copz = 3  or
082200130710                   S1Copz = 4  or  S1Copz = 5);
082300130701               exsr  sr_Call_TRMK41;
082400130705               select;
082500130705                 when  ErrGenerico;
082600130705                 when  oMK41fxx  = '03';
082700130705                   ErrGenerico = *on;
082800130705               endsl;
082900130710
083000130710             // -?Opz. 11=Selezione unificante?
083100130710             when  Opz_Selez  and  S1Copz = 11;
083200130710               k_CMMcod = S1Cuni;
083300130710               chain  %kds(k01azcmm01)  AZCMM000;
083400130710               if  %found(AZCMM01L);
083500130710                 prm_AZCMM_ds = AZCMM_ds;
083600130710               else;
083700130710                 p_CMMcod = S1Cuni;
083800130710               endif;
083900130710               $Fine = *on;
084000130710               ErrGenerico = *on;
084100130628
084200130701             // -?Opzione errata?
084300130628             other;
084400130628               ErrGenerico = *on;
084500130702               ErrMessage  = *on;
084600130628               PosCurOPZ   = *on;
084700130701               V1Dmsg = $Msg(03);
084800130628
084900130628           endsl;
085000130628
085100130701           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
085200130701           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
085300130701           if  S1Copz   <> *zero  and
085400130701              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
085500130628             SavS01csr   = C1RcdNbr;
085600130628           endif;
085700130628
085800130701           // -?Aggiornamento sfl?
085900130628           select;
086000130628             when  ErrMessage;
086100130628               SflNxtChg = *on;
086200130628               C1CsrRrn  = C1RcdNbr;
086300130628             when  ErrGenerico;
086400130628               C1CsrRrn  = C1RcdNbr;
086500130628               clear  S1Copz;
086600130628             other;
086700130628               clear  S1Copz;
086800130628           endsl;
086900130701
087000130701           UPDATE  MK40S01;
087100130701
087200130628           if  ErrGenerico   or   ErrMessage;
087300130628             leave;
087400130628           endif;
087500130628
087600130701           readc  MK40S01;
087700130628
087800130628         ENDDO;
087900130628
088000130701         // -?Riposizionam. cursore sul record contenente la prima opz.?
088100130701         //  ?(se non sono stati rilevati errori)?
088200130628         if  NOT ErrMessage   and   NOT ErrGenerico   and
088300130628             SavS01csr > *zero;
088400130628           C1CsrRrn = SavS01csr;
088500130628         endif;
088600130628
088700130628       ENDSR;
088800130628
088900130628       //--------------------------------------------------------------
089000130628       //?Controllo filiale inserita in C01                            ?
089100130628       //--------------------------------------------------------------
089200170330       BEGSR  sr_CtrlFil;
089300170330
089400170330         clear Og143;
089500130628
089600130725         If  C1Cfil <> *zero;
089700130628
089800130725           chain  (C1Cfil)  AZORG;
089900170330           if  %found(AZORG01L);
090000170330             Og143  = ORGde3;
090100170330           endif;
090200130628
090300130628           select;
090400130628             when  Not %found(AZORG01L);
090500130628               ErrGenerico = *on;
090600130702               ErrMessage  = *on;
090700130628               PosCurFIL   = *on;
090800130701               V1Dmsg = $Msg(04);
090900130628               leavesr;
091000130628             when  ORGfva <> *blank;
091100130628               ErrGenerico = *on;
091200130702               ErrMessage  = *on;
091300130628               PosCurFIL   = *on;
091400130701               V1Dmsg = $Msg(05);
091500130628               leavesr;
091600170330             when  ORGfag <> 'F'   and   ORGfva <> 'A'  and
091700170330                   Og143.§OGntw <> 'LOG';
091800130628               ErrGenerico = *on;
091900130702               ErrMessage  = *on;
092000130628               PosCurFIL   = *on;
092100130701               V1Dmsg = $Msg(06);
092200130628               leavesr;
092300130628           endsl;
092400130628
092500130701         EndIf;
092600130628
092700130725         SavC1Cfil = C1Cfil;
092800130628
092900130628       ENDSR;
093000130628
093100130628       //--------------------------------------------------------------
093200130628       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
093300130628       //?F08-Cambia ordinamento sfl                                   ?
093400130628       //--------------------------------------------------------------
093500130628       BEGSR  sr_F08S01;
093600130628
093700130701         Select;
093800130708           When $Ord_x_CodCmm;
093900130708             $Ord_x_CodCmm = *off;
094000130708             $Ord_x_DesCmm = *on;
094100130702             $Ord_x_CodUni = *off;
094200130708           When $Ord_x_DesCmm;
094300130708             $Ord_x_CodCmm = *off;
094400130708             $Ord_x_DesCmm = *off;
094500130702             $Ord_x_CodUni = *on;
094600130701           When $Ord_x_CodUni;
094700130709             $Ord_x_CodCmm = *on;
094800130708             $Ord_x_DesCmm = *off;
094900130702             $Ord_x_CodUni = *off;
095000130701         EndSl;
095100130628
095200130701         select;
095300130701           // -?Subfile vuoto: nessun record da ordinare?
095400130701           when  Not $Almeno1rec;
095500130705           // -?Subfile già parzializzato: occorre ricaricarlo?
095600130725           when  SavC1Cfil   <> C1Cfil   or
095700130725                 SavC1Cuni   <> C1Cuni   or
095800130725                 SavC1CsolU  <> C1CsolU  or
095900130725                 SavC1CdtAtt <> C1CdtAtt;
096000130705             $InzS01 = *on;
096100130701           // -?Altrimenti: basta riordinarlo?
096200130701           other;
096300130701             exsr  sr_SortSfl;
096400130701         endsl;
096500130628
096600130628       ENDSR;
096700130709
096800130709       //--------------------------------------------------------------
096900130709       //?Gestione tasto funzionale F13 da videata C01 / S01           ?
097000130709       //?F13-Ripetizione opzione                                      ?
097100130709       //--------------------------------------------------------------
097200130709       BEGSR  sr_F13S01;
097300130709
097400130709         // -?Reperimento pagina del sfl (e Nrr del suo primo record)?
097500130709         //  ?dalla posizione del cursore?
097600130709         wPag = C1CsrRrn / c_SflPag;
097700130709         wRig = %rem( C1CsrRrn : c_SflPag);
097800130709         if  wRig > *zero;
097900130709           wRig = (wPag * c_SflPag) + 1;
098000130709         else;                           // -?ultimo rec. della pag.?
098100130709           wRig = (wPag * c_SflPag) - c_SflPag + 1;
098200130709         endif;
098300130709         SavS01csr = wRig;
098400130709
098500130709         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
098600130709
098700130709         // -?Reperimento della 1ª opzione inserita?
098800130709         //  ?nella pagina dov'è il cursore?
098900130709         readc  MK40S01;
099000130709         DoW  Not %eof(TRMK40D)  and  S1Copz <> *zero  and
099100130709              S01nrr < SavS01csr;
099200130709           SflNxtChg = *on;
099300130709           update  MK40S01;
099400130709           readc  MK40S01;
099500130709         EndDo;
099600130709         if  Not %eof(TRMK40D)  and  S1Copz <> *zero;
099700130709           SflNxtChg = *on;
099800130709           update  MK40S01;
099900130709         endif;
100000130709
100100130709         // -?Nessuna opzione nella pagina?
100200130709         if  %eof(TRMK40D)         or
100300130709             S01nrr <  SavS01csr   or
100400130709             S01nrr > (SavS01csr + c_SflPag - 1);
100500130709           C1RcdNbr  = SavS01csr;
100600130709           C1CsrRrn  = SavS01csr;
100700130725           clear  $ForzaOpz;
100800130725           clear  SavS1Copz;
100900130725           clear  SavS1Cop1;
101000130709           clear  SavS01cs1;
101100130709           clear  SavS01cs2;
101200130709           ErrGenerico = *on;
101300130709           ErrMessage  = *on;
101400130709           PosCurOPZ   = *on;
101500130709           V1Dmsg = $Msg(07);
101600130709           leavesr;
101700130709         endif;
101800130709
101900130709         // -?Memorizzazione 1ª opzione nella pagina?
102000130709         SavS01csr = S01nrr;
102100130709         SavS1Copz = S1Copz;
102200130709         if  S01nrr <> SavS01cs1  or
102300130709             S1Copz <> SavS1Cop1;
102400130725           clear  $ForzaOpz;
102500130709         endif;
102600130709         SavS01cs1 = S01nrr;
102700130709         SavS1Cop1 = S1Copz;
102800130709
102900130709         // -?Reperimento delle altre eventuali opzioni?
103000130709         readc  MK40S01;
103100130709         DOW  Not %eof(TRMK40D);
103200130709
103300130709           // ·?Aggiornamento subfile?
103400130709           if  S1Copz <> *zero;
103500130709             SflNxtChg = *on;
103600130709             update  MK40S01;
103700130709           endif;
103800130709
103900130709           // ·?Reperita una 2ª opzione?
104000130709           Select;
104100130709
104200130709             // ·?Vuota!?
104300130709             When  S1Copz = *zero;
104400130709
104500130709             // ·?1ª opzione nella pagina del cursore  e?
104600130710             //  ?2ª opzione successiva diversa:?
104700130710             //  ?messaggio di avviso di "ricopertura"?
104800130709             When  (SavS01csr >= wRig              and
104900130709                    S01nrr    >  SavS01csr)        and
105000130709                    S1Copz    <> SavS1Copz         and
105100130709                    Not $ForzaOpz;
105200130709               $ForzaOpz = *on;
105300130709               SavS01cs2 = S01nrr;
105400130709               C1RcdNbr  = SavS01csr;
105500130709               C1CsrRrn  = SavS01csr;
105600130709               ErrGenerico = *on;
105700130709               ErrMessage  = *on;
105800130709               PosCurOPZ   = *on;
105900130709               V1Dmsg = $Msg(08);
106000130709               leavesr;
106100130709
106200130709             // ·?2ª opzione sul record dove è posizionato il cursore?
106300130709             //  ?(la nuova da considerare per la duplicazione)?
106400130709             When  C1CsrRrn = S01nrr;
106500130709               SavS01csr = S01nrr;
106600130709               SavS1Copz = S1Copz;
106700130709
106800130709             // ·?Più opzioni nella stessa videata e?
106900130709             //  ?cursore su nessuna delle opzioni?
107000130709             //  ?(riposizionare il cursore)?
107100130709             //  ?Se stessa opzione in videate diverse: vince la?
107200130709             //  ?posizione della prima opzione.?
107300130709             When  C1CsrRrn <> SavS01csr  and
107400130709                   C1CsrRrn <> S01nrr     and
107500130709                   S01nrr   <  wRig + c_SflPag;
107600130709               SavS01cs2 = S01nrr;
107700130709               C1RcdNbr  = SavS01csr;
107800130709               C1CsrRrn  = SavS01csr;
107900130709               ErrGenerico = *on;
108000130709               ErrMessage  = *on;
108100130709               PosCurOPZ   = *on;
108200130709               V1Dmsg = $Msg(09);
108300130709               leavesr;
108400130709
108500130709           EndSl;
108600130709
108700130709           readc  MK40S01;
108800130709
108900130709         ENDDO;
109000130709
109100130709         // ·?Opzione NON abilitata?
109200130709         if  SavS1Copz <> 1  and  SavS1Copz <> 2  and
109300130709             SavS1Copz <> 3  and  SavS1Copz <> 4  and
109400130710             SavS1Copz <> 5  and  SavS1Copz <> 11;
109500130709           C1RcdNbr  = SavS01csr;
109600130709           C1CsrRrn  = SavS01csr;
109700130709           ErrGenerico = *on;
109800130709           ErrMessage  = *on;
109900130709           PosCurOPZ   = *on;
110000130709           V1Dmsg = $Msg(03);
110100130709           leavesr;
110200130709         endif;
110300130709
110400130709         // -?Ripetizione opzione sul resto del subfile?
110500130709         S01nrr = SavS01csr + 1;
110600130709         chain  S01nrr  MK40S01;
110700130709         DoW  %found(TRMK40D);
110800130709           SflNxtChg = *on;
110900130709           S1Copz = SavS1Copz;
111000130709           update  MK40S01;
111100130709           S01nrr += 1;
111200130709           chain  S01nrr  MK40S01;
111300130709         EndDo;
111400130709
111500130709       ENDSR;
111600130628
111700130628       //--------------------------------------------------------------
111800130628       //?Ordinamento subfile                                          ?
111900130628       //?  This subroutine sorts the subfile records.                 ?
112000130628       //--------------------------------------------------------------
112100130628       BEGSR  sr_SortSfl;
112200130628
112300130701         // -?Impostazione NRR dell'ultimo record del subfile          ?
112400130701         //  ?(già fatto in sr_InzS01:                                 ?
112500130701         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
112600130701         //RrnLast  = S01nrr;
112700130628
112800130628         C1RcdNbr = 1;
112900130628         clear  C1CsrRrn;
113000130628         clear  S01nrr;
113100130628         clear  V1Dmsg;
113200130628         SflNxtChg   = *off;
113300130711         ErrMessage  = *off;
113400130628         ErrGenerico = *off;
113500130628         %subst(IndDspF : 50) = *off;
113600130628
113700130701         //?___________________________________________________________?
113800130701         //?Initialize the key fields to sort on.?
113900130701         //?There is one extra field not in the subfile -- Selected --?
114000130701         //?that is added to the record. This field is used to place?
114100130701         //?selected records in front of nonselected records.?
114200130701         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
114300130628
114400130628         clear  QLGKL;
114500130628         clear  QLGSKL;
114600130628         clear  QLGSCB;
114700130628         clear  QLGSCB00;
114800130628
114900130701         // -?Gestione della scelta ordinamento:?
115000130701         Select;
115100130710
115200130701           // -?Ordinamento per Codice Commerciale?
115300130708           When  $Ord_x_CodCmm;
115400130701             // -?1 campi chiave?
115500130701             QLGNBRK  = 1;
115600130708             // -?1° campo: codice comm.le (S1Ccmm)?
115700130701             //            ?a posizone    3,  7 byte, int,  ascending.?
115800130710             QLGSP    = %size(S1Copz) + 1;
115900130708             QLGSS    = %size(S1Ccmm);
116000130725             QLGDT    = c_NumIntero;
116100130725             QLGSO    = c_Ascendente;
116200130701             QLGKL(1) = QLGSKL;
116300130701
116400130711           // -?Ordinamento per Descrizione Commerciale?
116500130708           when  $Ord_x_DesCmm;
116600130701             // -?2 campi chiave?
116700130701             QLGNBRK  = 2;
116800130711             // -?1° campo: descr. comm.le (S1Dcmm)?
116900130711             //            ?a posizone   10, 25 byte, char, ascending.?
117000130710             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + 1;
117100130708             QLGSS    = %size(S1Dcmm);
117200130725             QLGDT    = c_Carattere;
117300130725             QLGSO    = c_Ascendente;
117400130701             QLGKL(1) = QLGSKL;
117500130708             // -?2° campo: descr. comm.le (S1Ccmm)?
117600130701             //            ?a posizone    3,  7 byte, int,  ascending.?
117700130710             QLGSP    = %size(S1Copz) + 1;
117800130708             QLGSS    = %size(S1Ccmm);
117900130725             QLGDT    = c_NumIntero;
118000130725             QLGSO    = c_Ascendente;
118100130701             QLGKL(2) = QLGSKL;
118200130628
118300130701           // -?Ordinamento per Codice Unificante?
118400130701           when  $Ord_x_CodUni;
118500130701             // -?2 campi chiave?
118600130701             QLGNBRK  = 2;
118700130701             // -?1° campo: codice unificante (S1Cuni)?
118800130711             //            ?a posizone   35,  7 byte, int,  ascending.?
118900130710             QLGSP    = %size(S1Copz) + %size(S1Ccmm) + %size(S1Dcmm) + 1;
119000130701             QLGSS    = %size(S1Cuni);
119100130725             QLGDT    = c_NumIntero;
119200130725             QLGSO    = c_Ascendente;
119300130701             QLGKL(1) = QLGSKL;
119400130708             // -?2° campo: cod. comm.le (S1Ccmm)?
119500130701             //            ?a posizone    3,  7 byte, int,  ascending.?
119600130710             QLGSP    = %size(S1Copz) + 1;
119700130708             QLGSS    = %size(S1Ccmm);
119800130725             QLGDT    = c_NumIntero;
119900130725             QLGSO    = c_Ascendente;
120000130701             QLGKL(2) = QLGSKL;
120100130628
120200130701         EndSl;
120300130628
120400130701         // -?Load other sort parameters.?
120500130725         QLGLB   = 80 + 16 * c_MaxKey;
120600130628         QLGRL   = %size(SflRcd) - 1;
120700130628         QLGRT   = 8;
120800130628         QLGOKL  = 80;
120900130628         QLGLKE  = 16;
121000130628         QLGLSS  = 290;
121100130701         // -?Initialize Sort I/O API fields.?
121200130628         QLGRL00 = QLGRL;
121300130628         QLGRC00 = 1;
121400130628         clear  QUSEI;
121500130628         QUSBPRV = %size(QUSEC);
121600130628
121700130701         // -?First step - Initialize the sort routine.?
121800130628         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
121900130628                      SizeList : ReturnSize : QUSEC );
122000130628
122100130701         // -?Next step - Write records to I/O routine.?
122200130725         QLGRT00  = c_Put;
122300130628         For  Nrr = 1  To  RrnLast;
122400130701           chain  Nrr  MK40S01;
122500130701           // -?Solo le righe con Selected = 'Y' sono riordinate,?
122600130701           //  ?quindi per fare un ordinamento di tutte le righe?
122700130701           //  ?metto 'Y' sempre.?
122800130628           Selected = 'Y';
122900130628           clear  QUSEI;
123000130628           QUSBPRV  = %size(QUSEC);
123100130628           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
123200130628                         SizeList : NotUsed : QUSEC );
123300130628         EndFor;
123400130628
123500130701         // -?Next step - Signal end of input, clear subfile for reload.?
123600130725         QLGRT00 = c_EndPut;
123700130628         clear  QUSEI;
123800130628         QUSBPRV = %size(QUSEC);
123900130628         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
124000130628                       SizeList : NotUsed : QUSEC );
124100130628
124200130701         // -?Pulizia subfile?
124300130628         SflDsp_N    = *on;
124400130628         SflDspCtl_N = *on;
124500130701         write  MK40C01;
124600130628         SflDspCtl_N = *off;
124700130628         SflEnd      = *off;
124800130628
124900130701         // -?Final step - Write the records back to the subfile.?
125000130725         QLGRT00  = c_Get;
125100130628         For  Nrr = 1  To RrnLast;
125200130628           clear  QUSEI;
125300130628           QUSBPRV = %size(QUSEC);
125400130628           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
125500130628                         QLGRL00  : NotUsed : QUSEC );
125600130701           // -?Ripristino indicatori utilizzati nel sfl rec?
125700130701           SflNxtChg = (S1Copz <> *zero);
125800130709           Ord_1 = ( $Ord_x_DesCmm );
125900130709           Ord_2 = ( $Ord_x_CodUni );
126000130628           S01nrr = Nrr;
126100130701           write  MK40S01;
126200130628         EndFor;
126300130628
126400130628         C1CsrRrn = 1;
126500130628
126600130701         // -?Visualizzazione del SFL (se ci sono dati)?
126700130628         SflDsp_N = (S01nrr <= *zero);
126800130628
126900130701         // -?Attivazione del SFLEND?
127000130628         SflEnd = $EoF;
127100130701
127200130701         // -?Impostazione nuova descrizione tasto F8=Ord. x ...?
127300130701         Select;
127400130708           When  $Ord_x_CodCmm;
127500130708             P01f08 = 'F8=Ord. x Descrizione';
127600130709           When  $Ord_x_DesCmm;
127700130701             P01f08 = 'F8=Ord. x Unificante ';
127800130701           When  $Ord_x_CodUni;
127900130701             P01f08 = 'F8=Ord. x Cod.Comm.le';
128000130701         EndSl;
128100130628
128200130628       ENDSR;
128300130701
128400130701       //--------------------------------------------------------------
128500130701       //?Richiamo *pgm di manutenzione singolo commerciale.         ?
128600130701       //--------------------------------------------------------------
128700130701       BEGSR  sr_Call_TRMK41;
128800130701
128900130701         clear  TRMK41ds;
129000130708         clear  AZCMM_ds;
129100130701         if  dsp_aid <> c_F06;
129200130701           iMK41opz = S1Copz;
129300130708           iMK41cmm = S1Ccmm;
129400130701         endif;
129500130701
129600130701         kpjbu = TRMK41ds;
129700130708         trmk41r ( kpjba : AZCMM_ds );
129800130701         TRMK41ds = KPJBU;
129900130701
130000130701         Select;
130100130710
130200130710           // -?Restituito errore?
130300130705           When  oMK41err = *on;
130400130705             V1Dmsg = oMK41msg;
130500130705             ErrMessage  = *on;
130600130705             ErrGenerico = *on;
130700130710
130800130710           // -?Uscito premendo F3 o F12?
130900130705           When  oMK41fxx = '03'  or  oMK41fxx = '12';
131000130701             leavesr;
131100130710
131200130701           // -?Eseguito Inserimento o Copia?
131300130701           When  dsp_aid = c_F06  or  S1Copz = 3;
131400130701             $InzS01 = *on;
131500130701             $Almeno1rec = *on;
131600130710
131700130701           // -?Eseguita Modifica?
131800130710           When  S1Copz = 2;
131900130725             $InzS01 = (C1Cuni   > *zero  and  S1Cuni <> AZCMM_ds.CMMuni)     or
132000130725                       (C1CsolU  = 'S'    and  S1Cuni <> AZCMM_ds.CMMuni)     or
132100130725                       (C1CdtAtt > *zero  and  (AZCMM_ds.CMMdtIni > W1CdtAtt  or
132200130725                                                AZCMM_ds.CMMdtFin < W1CdtAtt));
132300130708             S1Dcmm = AZCMM_ds.CMMdes;
132400130708             S1Cuni = AZCMM_ds.CMMuni;
132500130708             if  AZCMM_ds.CMMdip <> *zero  or  AZCMM_ds.CMMsoc <> *blank;
132600130709               S1Cdip = AZCMM_ds.CMMsoc + '/' + %editc( AZCMM_ds.CMMdip : 'Z');
132700130725             else;
132800130725               clear  S1Cdip;
132900130708             endif;
133000130708             S1Cpli = AZCMM_ds.CMMpli;
133100130708             S1Cple = AZCMM_ds.CMMple;
133200130710
133300130710           // -?Eseguita Cancellazione?
133400130710           When  S1Copz = 4;
133500130710             $InzS01 = *on;
133600130710
133700130701         EndSl;
133800130701
133900130701       ENDSR;
134000130628
134100130628       //--------------------------------------------------------------
134200130628       //?Operazioni finali.                                         ?
134300130628       //--------------------------------------------------------------
134400130628       BEGSR  sr_RoutEnd;
134500130628
134600130701         // -?Chiusura *pgm?
134700130628         return;
134800130628
134900130628       ENDSR;
135000130628
135100130628      /end-free
135200130628
135300130701** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
135400130701Raggiunto il limite massimo dei record caricabili a video                      1
135500150108Non sono ammesse opzioni quando è richiesta una selezione                      2
135600130701Opzione errata                                                                 3
135700130701Filiale inesistente                                                            4
135800130701Filiale annullata                                                              5
135900130701Filiale NON valida                                                             6
136000130709Immettere un'opzione su questa pagina prima di premere F13                     7
136100130709Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 8
136200130709Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 9
136300130725Data formalmente errata                                                       10
