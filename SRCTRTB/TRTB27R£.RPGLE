000100140520       //==============================================================
000200140521       //?TRTB27R * Manutenzione tab. "1P"+"SP" = Consegne Particolari ?
000300140520       //==============================================================
000400140520
000500140520       //--------------------------------------------------------------
000600140520       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140520       //--------------------------------------------------------------
000800140520
000900140520     /*END
001000140520
001100140520       //--------------------------------------------------------------
001200140520       //?Specifiche di controllo.                                     ?
001300140520       //--------------------------------------------------------------
001400140520
001500140520     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600140520     h dftactgrp(*no)
001700140521     h bnddir('TRUL')
001800140520
001900140520       //--------------------------------------------------------------
002000140520       //?Dichiarazione file.                                          ?
002100140520       //--------------------------------------------------------------
002200140520
002300140520       // -?Anagrafico Lingue?
002400140520     fAZLIN01L  if   e           k disk
002500140520
002600140526       // -?Tabelle "1P" ed "SP"?
002700140520     fTABEL00F  Uf A e           k disk
002800140520
002900140520       // -?Video Gestione?
003000140520     fTRTB27D   cf   e             workstn
003100140520     f                                     sfile(TB27S01 : S01nrr)
003200140520     f                                     sfile(TB27S02 : S02nrr)
003300140520     f                                     indds(IndDspF)
003400140520     f                                     infds(InfDspF)
003500140520
003600140520       //--------------------------------------------------------------
003700140520       //?Definizione costanti.                                        ?
003800140520       //--------------------------------------------------------------
003900140520
004000140520       // -?Codice tabella in gestione?
004100140520     d c_Tab           c                   const('1P')
004200140520     d c_Tab2          c                   const('SP')
004300140520
004400140520       // -?Codice utente?
004500140520     d c_Kut           c                   const(1)
004600140520
004700140520       // -?Numero di record in ciascuna pagina di subfile?
004800140520     d c_SflPag        c                   const(16)
004900140520
005000140520       // -?Opzioni a video?
005100140520     d c_Opz_G         c                   const('2=Modifica, +
005200140520     d                                            3=Copia, +
005300140526     d                                            4=Annullamento/+
005400140526     d                                              Ripristino, +
005500140526     d                                            5=Visualizzazione.')
005600140526     d c_Opz_S         c                   const('1=Scelta.')
005700140520
005800140520       // -?Costante per controllo "caratteri solo numerici"?
005900140520     d c_Digits        c                   const('0123456789')
006000140521
006100140521       // -?Numero massimo di record gestibili?
006200140521     d c_MaxRec        c                   const(9999)
006300140520
006400140520       // -?Tasti funzionali a video?
006500140520     d c_F01           c                   const(x'31')
006600140520     d c_F02           c                   const(x'32')
006700140520     d c_F03           c                   const(x'33')
006800140520     d c_F04           c                   const(x'34')
006900140520     d c_F05           c                   const(x'35')
007000140520     d c_F06           c                   const(x'36')
007100140520     d c_F07           c                   const(x'37')
007200140520     d c_F08           c                   const(x'38')
007300140520     d c_F09           c                   const(x'39')
007400140520     d c_F10           c                   const(x'3A')
007500140520     d c_F11           c                   const(x'3B')
007600140520     d c_F12           c                   const(x'3C')
007700140520     d c_F13           c                   const(x'B1')
007800140520     d c_F14           c                   const(x'B2')
007900140520     d c_F15           c                   const(x'B3')
008000140520     d c_F16           c                   const(x'B4')
008100140520     d c_F17           c                   const(x'B5')
008200140520     d c_F18           c                   const(x'B6')
008300140520     d c_F19           c                   const(x'B7')
008400140520     d c_F20           c                   const(x'B8')
008500140520     d c_F21           c                   const(x'B9')
008600140520     d c_F22           c                   const(x'BA')
008700140520     d c_F23           c                   const(x'BB')
008800140520     d c_F24           c                   const(x'BC')
008900140520     d c_Enter         c                   const(x'F1')
009000140520     d c_RollDown      c                   const(x'F4')
009100140520     d c_RollUp        c                   const(x'F5')
009200140520
009300140520       //--------------------------------------------------------------
009400140520       //?Definizione schiere.                                         ?
009500140520       //--------------------------------------------------------------
009600140520
009700140520       // -?Messaggi di errore?
009800140523     d sk_Msg          s             78    dim(13) ctdata perrcd(1)
009900140520
010000140520       //--------------------------------------------------------------
010100140520       //?Definizione aree dati.                                       ?
010200140520       //--------------------------------------------------------------
010300140520
010400140520       // -?Dati utente?
010500140520     d §AzUte        e ds                  extname(AZUTE00F)
010600140520     d                                     dtaara
010700140520     d §DatiUte      e ds                  extname(dDatiUte)
010800140520     d                                     dtaara
010900140520
011000140520       //--------------------------------------------------------------
011100140520       //?Definizione strutture dati.                                  ?
011200140520       //--------------------------------------------------------------
011300140520
011400140520       // -?Status ds?
011500140520     d Status         sds
011600140520     d   SDSpgm          *proc
011700140520
011800140520       // -?InfDS?
011900140520     d InfDspF         ds
012000140520     d   dsp_aid             369    369a
012100140520     d   sfl_rrn             376    377i 0
012200140520     d   min_nrr             378    379i 0
012300140520     d   num_rcds            380    381i 0
012400140520
012500140520       // -?Indicatori su DspF?
012600140520     d IndDspF         ds                  inz
012700140520         // -?Abilitazione tasti funzionali?
012800140520     d   F6Attivo                      n   overlay(IndDspF : 06)
012900140520     d   F10Attivo                     n   overlay(IndDspF : 10)
013000140520     d   F12Attivo                     n   overlay(IndDspF : 12)
013100140526     d   RUpAttivo                     n   overlay(IndDspF : 25)
013200140526     d   RDnAttivo                     n   overlay(IndDspF : 26)
013300140520         // -?Emissione messaggio di errore?
013400140520     d   ErrMessage                    n   overlay(IndDspF : 28)
013500140520         // -?Indicatori di gestione del subfile?
013600140520     d   SflDsp_N                      n   overlay(IndDspF : 30)
013700140520     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
013800140520     d   SflNxtChg                     n   overlay(IndDspF : 32)
013900140520     d   SflEnd                        n   overlay(IndDspF : 33)
014000140520     d   Sfl2Dsp_N                     n   overlay(IndDspF : 34)
014100140520     d   Sfl2DspCtl_N                  n   overlay(IndDspF : 35)
014200140520     d   Sfl2NxtChg                    n   overlay(IndDspF : 36)
014300140520     d   Sfl2End                       n   overlay(IndDspF : 37)
014400140520         // -?Indicatori per Attibuti di visualizzazione?
014500140520     d   VisAnnull                     n   overlay(IndDspF : 40)
014600140520     d   ProtectCOD                    n   overlay(IndDspF : 41)
014700140520     d*//ProtectALL                    n   overlay(IndDspF : 42)
014800140520         // -?Posizionamento cursore & Segnalazione errore?
014900140520     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015000140520     d   PosCurCOD                     n   overlay(IndDspF : 51)
015100140520     d   PosCurDES                     n   overlay(IndDspF : 52)
015200140520     d   PosCurTT0                     n   overlay(IndDspF : 53)
015300140520     d   PosCurTT1                     n   overlay(IndDspF : 54)
015400140520     d   PosCurTT2                     n   overlay(IndDspF : 55)
015500140520     d   PosCurTT3                     n   overlay(IndDspF : 56)
015600140520     d   PosCurTT4                     n   overlay(IndDspF : 57)
015700140520     d   PosCurTT5                     n   overlay(IndDspF : 58)
015800140520     d   PosCurTT6                     n   overlay(IndDspF : 59)
015900140520     d   PosCurTT7                     n   overlay(IndDspF : 60)
016000140520     d   PosCurTT8                     n   overlay(IndDspF : 61)
016100140520     d   PosCurTT9                     n   overlay(IndDspF : 62)
016200140520     d   PosCurTTV                     n   overlay(IndDspF : 63)
016300140520     d   PosCurCTS                     n   overlay(IndDspF : 64)
016400140520     d   PosCurCTE                     n   overlay(IndDspF : 65)
016500140520     d   PosCurELI                     n   overlay(IndDspF : 66)
016600140520     d   PosCurDES2                    n   overlay(IndDspF : 67)
016700140520     d   PosCurS2DES                   n   overlay(IndDspF : 71)
016800140523     d   PosCurS2DEE                   n   overlay(IndDspF : 72)
016900140523     d   PosCurS2DFE                   n   overlay(IndDspF : 73)
017000140520         // -?Riemissione videata?
017100140520     d   ErrGenerico                   n   overlay(IndDspF : 99)
017200140520
017300140520       // -?Parametri ricevuti?
017400140520     d ds_CTT          ds                  inz
017500140523     d   V1Ctt0                            inz
017600140523     d   V1Ctt1                            inz
017700140523     d   V1Ctt2                            inz
017800140523     d   V1Ctt3                            inz
017900140523     d   V1Ctt4                            inz
018000140523     d   V1Ctt5                            inz
018100140523     d   V1Ctt6                            inz
018200140523     d   V1Ctt7                            inz
018300140523     d   V1Ctt8                            inz
018400140523     d   V1Ctt9                            inz
018500140523     d   V1CttV                            inz
018600140520     d     sk_CTT              1     11    dim(11)
018700140520
018800140520       // -?Parametri ricevuti?
018900140520     d KPJBA         e ds
019000140520     d TRTB27ds      e ds                  inz
019100140520
019200140520       // -?Tabelle usate:?
019300140523     d ds            e ds                  inz   qualified
019400140520       // ·?"1P" = Consegne Particolari Clienti?
019500140523     d ds1P          e ds                  inz   qualified
019600140520       // ·?"SP" = Estensione della tab. "1P" (Consegne Particolari Clienti)?
019700140523     d dsSP          e ds                  inz   qualified
019800140520
019900140520       //--------------------------------------------------------------
020000140520       //?Definizione variabili globali.                               ?
020100140520       //--------------------------------------------------------------
020200140520
020300140520       // -?Flags booleani?
020400140520     d $Fine           s               n   inz
020500140520     d $InzS01         s               n   inz(*on)
020600140520     d $InzS02         s               n   inz(*on)
020700140521     d $InzD01         s               n   inz(*on)
020800140520     d $InzW01         s               n   inz(*on)
020900140521     d $ForzaOpz       s               n   inz
021000140526
021100140526       // -?Indici di schiera?
021200140526     d xx              s              3  0 inz
021300140520
021400140520       // -?Variabili per la gestione del video?
021500140520     d $Video          s              2    inz('S1')
021600140523     d $VideoPrec      s                   like($Video)   inz
021700140521     d S01nrr          s                   like(C1RcdNbr) inz
021800140523     d S02nrr          s                   like(C2RcdNbr) inz
021900140521     d SavS01csr       s                   like(C1RcdNbr) inz
022000140521     d SavS1Copz       s                   like(VS1opz)   inz
022100140521     d SavS01cs1       s                   like(C1RcdNbr) inz
022200140521     d SavS1Cop1       s                   like(VS1opz)   inz
022300140521     d SavS01cs2       s                   like(C1RcdNbr) inz
022400140521     d*// SavS1Cop2       s                   like(VS1opz)   inz
022500140523     d SavS02csr       s                   like(C2RcdNbr) inz
022600140526     d wPag            s              4  0 inz
022700140526     d wRig            s              3  0 inz
022800140521
022900140521       // -?Conteggio selezioni (opz. 1) inserite?
023000140523     d wMemoSel        s                   like(VS1opz)  inz
023100140523
023200140520       // -?Memo per controllo variazioni a video?
023300140523     d SaveCOD         s                   like(C1Ccod)  inz
023400140523
023500140523       // -?Campi del file TABEL00F?
023600140523     d SaveTBLftt      s                   like(TBLftr)  inz
023700140523     d SaveTBLflt      s                   like(TBLflt)  inz
023800140523     d SaveTBLftr      s                   like(TBLftr)  inz
023900140523     d SaveTBLdtr      s                   like(TBLdtr)  inz
024000140523     d wNewTBLflg      s                   like(TBLflg)  inz
024100140526     d SaveT1Puni      s                   like(TBLuni)  inz
024200140526     d SaveTSPuni      s                   like(TBLuni)  inz
024300140520
024400140520       // -?Campi di comodo?
024500140526     d*// wDate           s              8  0 inz
024600140521     d wDate_EUR       s               d   inz  datfmt(*EUR)
024700140526     d wDate_DMY       s               d   inz  datfmt(*DMY)
024800140520
024900140520       //--------------------------------------------------------------
025000140520       //?Definizione prototipi procedure.                             ?
025100140520       //--------------------------------------------------------------
025200140520
025300140520       // -?Reperimento dati utente?
025400140520     d TIBS34ds      e ds                  inz
025500140520      /copy gaitrasrc/srcProtoPR,TIBS34R
025600140521
025700140521       // -?Reperimento dati tabelle?
025800140521      /copy gaitrasrc/srcProtoPI,TRULTAB
025900140521      /copy gaitrasrc/srcProtoPR,TRULTAB
026000140520
026100140520       // -?Gestione Tab. "CT" = Codici Tassazione?
026200140520     d TRTB09ds      e ds                  inz
026300140520     d trtb09r         pr                  extpgm('TRTB09R')
026400140520     d   kpjba                             likeds(KPJBA)
026500140520
026600140526       // -?Ricerca Tabella TABEL00F - Selezione multipla?
026700140526     d Local           ds                  inz  qualified
026800140526     d   idElemento                   8a
026900140526     d   tastoFunzionaleUscita...
027000140526     d                                1a
027100140520      /copy gaitrasrc/srcProtoPR,TRUL19R
027200140523
027300140523       // -?Controllo ed inversione date?
027400140523     d WLBdat          ds                  inz
027500140523     d   G08dat                       8  0 inz
027600140523     d   G08inv                       8  0 inz
027700140523     d   G08err                       1    inz('3')
027800140523     d   G08tgi                       5  0 inz
027900140523      /copy gaitrasrc/srcProtoPR,XSRDA8
028000140520
028100140520       //--------------------------------------------------------------
028200140520       //?Definizione key-list.                                        ?
028300140520       //--------------------------------------------------------------
028400140520
028500140520       // -?File TABEL00F?
028600140521     d keyTABEL00    e ds                  extname(TABEL00F : *key)
028700140520     d                                     prefix(k_)   inz
028800140523
028900140523       // -?File AZLIN01L?
029000140523     d keyAZLIN01    e ds                  extname(AZLIN01L : *key)
029100140523     d                                     prefix(k_)   inz
029200140520
029300140520       //--------------------------------------------------------------
029400140520       //?M A I N - L I N E                                            ?
029500140520       //--------------------------------------------------------------
029600140520
029700140520     c     *Entry        plist
029800140520     c                   parm                    KPJBA
029900140520
030000140520      /free
030100140520
030200140520       // -?Operazioni iniziali?
030300140520       exsr sr_RoutInz;
030400140520
030500140520       // -?Gestione videate?
030600140520       DoW  $Fine = *off;
030700140520
030800140520         select;
030900140520
031000140520           // -?Gestione videata S1 (subfile)?
031100140520           when $Video = 'S1';
031200140520             exsr  sr_GesS01;
031300140520
031400140520           // -?Gestione videata D1 (manutenzione 1ª videata)?
031500140523           //when $Video = 'D1';
031600140523           //  exsr  sr_GesD0102;
031700140520
031800140520           // -?Gestione videata D2 (manutenzione 2ª videata)?
031900140523           //when $Video = 'D2';
032000140523           //  exsr  sr_GesD0102;
032100140523
032200140523           // -?Gestione videata S2 (descrizioni in lingua)?
032300140523           //when $Video = 'S2';
032400140523           //  exsr sr_GesS02;
032500140520
032600140520           // -?Gestione videata W1 (conferma trasmissione dati)?
032700140521           //when $Video = 'W1';
032800140521           //  exsr  sr_GesW01;
032900140520
033000140520           // -?? ? ? ? ??
033100140520           other;
033200140520             $Fine = *on;
033300140520
033400140520         endsl;
033500140520
033600140520       EndDo;
033700140520
033800140520       // -?Operazioni finali?
033900140520       exsr sr_RoutEnd;
034000140520
034100140520       //--------------------------------------------------------------
034200140520       //?Operazioni iniziali.                                         ?
034300140520       //--------------------------------------------------------------
034400140520       BEGSR  sr_RoutInz;
034500140520
034600140520         *inLR = *on;
034700140520
034800140520         // -?Recezione parametri?
034900140520         TRTB27ds = KPJBU;
035000140520
035100140520         // -?Reperimento dati job?
035200140520         exsr  sr_DatiJob;
035300140520
035400140520         // -?Impostazione nome programma a video?
035500140520         VT1pgm = SDSpgm;
035600140521
035700140521         // -?Impostazione videata iniziale?
035800140521         $Video = 'S1';
035900140521         reset  $InzS01;
036000140521         reset  $InzS02;
036100140521         reset  $InzD01;
036200140523
036300140523         // -?Reperimento dati indice Tabelle?
036400140523         clear  ds;
036500140523         clear  keyTABEL00;
036600140523         k_TBLkut = c_Kut;
036700140523         k_TBLkey = *zero;
036800140523         %subst( TBLkey : %len(TBLkey) - 1 ) = c_Tab;
036900140526         chain(N)  %kds( keyTABEL00 )  TABEL;
037000140523         if  %found(TABEL00F)  and  TBLflg = *blank;
037100140523           ds = TBLuni;
037200140523         endif;
037300140520
037400140521         // -?Impostazione iniziale / pulizia dei campi chiave?
037500140521         clear  keyTABEL00;
037600140520         k_TBLkut = c_Kut;
037700140520         k_TBLcod = c_Tab;
037800140520
037900140520         // -?Reperimento data odierna?
038000140526         //wDate = %int( %subst( %char( %dec( %timestamp() ) )
038100140526         //                      : 1 : 8 ) );
038200140520
038300140520       ENDSR;
038400140520
038500140520       //--------------------------------------------------------------
038600140520       //?Reperimento Dati del job (Utente/Operativi).                 ?
038700140520       //--------------------------------------------------------------
038800140520       BEGSR  sr_DatiJob;
038900140520
039000140520         in(e) §AzUte;
039100140520         if NOT %error;
039200140520           in(e) §DatiUte;
039300140520         endif;
039400140520         if %error or RSut = *blank;
039500140520           tibs34r ( tibs34ds );
039600140520           in §AzUte;
039700140520           in §DatiUte;
039800140520         endif;
039900140520
040000140520       ENDSR;
040100140520
040200140520       //--------------------------------------------------------------
040300140521       //?Gestione subfile S01 - Subfile con elenco tab. "1P"/"SP".    ?
040400140520       //--------------------------------------------------------------
040500140520       BEGSR  sr_GesS01;
040600140520
040700140520         // -?Inizializzazione videata?
040800140520         if  $InzS01;
040900140520           exsr  sr_InzS01;
041000140520           $InzS01 = *off;
041100140520         endif;
041200140520
041300140520         // -?Emissione Testata & Piede con tasti funzionali abilitati?
041400140520         write  TB27T01;
041500140520         write  TB27P01;
041600140520
041700140520         // -?Posizionamento cursore?
041800140521         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
041900140521         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
042000140521         //  ?si visualizza la pagina che vedeva l'utente quando ha?
042100140521         //  ?premuto l'ultimo tasto.?
042200140520         if  C1CsrRrn > *zero;
042300140520           C1RcdNbr = C1CsrRrn;
042400140520         else;
042500140520           C1RcdNbr = 1;
042600140520           write  TB27S00;             //?(no rec.)?
042700140520         endif;
042800140520
042900140520         // -?Emissione videata?
043000140520         exfmt  TB27C01;
043100140520
043200140523         clear  VIDmsg;
043300140520         reset  ErrMessage;
043400140520         reset  ErrGenerico;
043500140521
043600140521         clear  TB27fxx;
043700140521
043800140521         if  dsp_aid <> c_F03  and
043900140521             dsp_aid <> c_F13;
044000140521           clear  SavS01csr;
044100140521           clear  SavS1Copz;
044200140521           clear  SavS01cs1;
044300140521           clear  SavS1Cop1;
044400140521           clear  SavS01cs2;
044500140521           //clear  SavS1Cop2;
044600140521         endif;
044700140520
044800140520         Select;
044900140520
045000140520           // -?Ricevuta Opzione Errata?
045100140521           When  TB27esito = *on;
045200140520             $Fine = *on;
045300140520
045400140520           // -?F3=Fine?
045500140520           When  dsp_aid = c_F03;
045600140521             TB27fxx = 'C';
045700140520             $Fine = *on;
045800140520
045900140520           // -?F5=Refresh?
046000140520           When  dsp_aid = c_F05;
046100140520             $InzS01 = *on;
046200140520
046300140520           // -?F10=Immissione?
046400140520           When  dsp_aid = c_F10;
046500140521             reset  $ForzaOpz;
046600140521             $Video  = 'D1';
046700140521             $InzD01 = *on;
046800140520             clear  TRTB27ds;
046900140521             TB27fxx = 'J';
047000140520
047100140520           // -?F13=Ripetizione opzione?
047200140520           When  dsp_aid = c_F13;
047300140521             exsr  sr_DupOpzS01;
047400140520
047500140520           // -?Invio?
047600140520           Other;
047700140521             reset  $ForzaOpz;
047800140521             // -?Gestione opzioni?
047900140521             if  S01nrr > *zero;
048000140523               exsr  sr_OpzS01;
048100140523               select;
048200140523                 when  ErrGenerico;
048300140523                   leavesr;
048400140523                 when  wMemoSel > *zero;
048500140523                   $Fine = *on;
048600140523                   leavesr;
048700140523               endsl;
048800140523             endif;
048900140523             // -?Posizionamento nel subfile?
049000140523             //  ?(da gestire sempre: il subfile si potrebbe esere?
049100140523             //  ?svuotato dopo l'ultimo posizionamento richiesto)?
049200140523             if  C1Ccod <> SaveCOD;
049300140523               $InzS01 = *on;
049400140523               SaveCOD = C1Ccod;
049500140523             endif;
049600140520
049700140520         EndSl;
049800140520
049900140520       ENDSR;
050000140520
050100140520       //--------------------------------------------------------------
050200140520       //?Inizializzazione subfile S01.                                ?
050300140520       //--------------------------------------------------------------
050400140520       BEGSR  sr_InzS01;
050500140521
050600140521         // -?Spegnimento degli indicatori di errore?
050700140521         %subst(IndDspF : 50) = *off;
050800140521
050900140521         // -?Pulizia subfile-control?
051000140521         clear  TB27C01;
051100140521
051200140521         C1Ccod  = SaveCOD;
051300140520
051400140520         // -?Impostazione Opzioni & Abilitazione tasti funzionali?
051500140520         //  ?a seconda dell'opzione richiesta?
051600140521         reset  $ForzaOpz;
051700140520         select;
051800140521           when  TB27fun = '1';
051900140520             F10Attivo = *off;
052000140521             C1Dopz = c_Opz_S;
052100140521           when  TB27fun = *blank;
052200140520             F10Attivo = *on;
052300140521             C1Dopz = c_Opz_G;
052400140520           other;
052500140521             TB27esito = *on;
052600140520             exsr  sr_RoutEnd;
052700140520         endsl;
052800140521
052900140521         // -?Pulizia subfile?
053000140521         SflDsp_N    = *on;
053100140521         SflDspCtl_N = *on;
053200140521         write  TB27C01;
053300140521         SflDspCtl_N = *off;
053400140521         SflEnd      = *off;
053500140521
053600140521         clear  C1RcdNbr;
053700140521         clear  C1CsrRrn;
053800140521         clear  S01nrr;
053900140521         clear  VIDmsg;
054000140521         ErrMessage  = *off;
054100140521         ErrGenerico = *off;
054200140521
054300140521         SflNxtChg = *off;
054400140521
054500140521         // -?Caricamento completo dei dati nel subfile (per codice)?
054600140521         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
054700140521         //  ?l'eventuale ordinamento)?
054800140526         k_TBLkut = c_Kut;
054900140526         k_TBLcod = c_Tab;
055000140521         k_TBLkey = C1Ccod;
055100140521         setll  %kds( keyTABEL00 )  TABEL;
055200140521         reade(N)  %kds( keyTABEL00 : 2 )  TABEL;
055300140521
055400140521         DoW  Not %eof(TABEL00F)  and  S01nrr < c_MaxRec;
055500140521           if  ( (TB27fun  = '1'  and  TBLflg = *blank)  or
055600140521                  TB27fun  = *blank )  and
055700140521               ( (TB27ass  = 'S'  and  ds1P.§1Pasc <> 'S'
055800140521                                  and  ds1P.§1Prcv <> 'S')  or
055900140521                  TB27ass <> 'S' );
056000140521             exsr  sr_WriteS01;
056100140521           endif;
056200140526           reade(N)  %kds(keyTABEL00 : 2)  TABEL;
056300140521         EndDo;
056400140521
056500140521         // -?Impaginazione subfile?
056600140521         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
056700140521         if S01nrr > *zero;
056800140521           C1RcdNbr = 1;
056900140521           C1CsrRrn = 1;
057000140521         else;
057100140521           clear C1RcdNbr;
057200140521         endif;
057300140521
057400140521         // -?Visualizzazione del SFL (se ci sono dati)?
057500140521         SflDsp_N = ( S01nrr <= *zero );
057600140521
057700140521         // -?Attivazione del SFLEND?
057800140521         SflEnd = %eof( TABEL00F );
057900140521
058000140521         // -?(Dis)Abilitazione tasti funzionali?
058100140521         F10Attivo = ( TB27fun <> '1' );
058200140521
058300140521         // -?Emissione messaggio di segnalazione raggiungimento limite?
058400140521         if  S01nrr >= c_MaxRec   and   Not %eof(TABEL00F);
058500140521           ErrGenerico = *on;
058600140521           ErrMessage  = *on;
058700140521           PosCurOPZ   = *on;
058800140523           VIDmsg = sk_Msg(01);
058900140521           leavesr;
059000140521         endif;
059100140521
059200140521       ENDSR;
059300140521
059400140521       //--------------------------------------------------------------
059500140523       //?Registrazione del singolo rec. nel subfile S01.              ?
059600140521       //--------------------------------------------------------------
059700140521       BEGSR  sr_WriteS01;
059800140521
059900140521         ds1P = TBLuni;
060000140521
060100140521         // -?Reperimento tabella "SP" = Estensione della "1P"?
060200140521         clear  dsSP;
060300140521         if  getTabella ( c_Tabel : c_Tab2 : TBLkey :
060400140521                          *omit : *omit : *omit :
060500140521                          *omit : *omit :
060600140521                          *omit : *omit : *omit : *omit :
060700140521                          *omit : *omit : *omit : *omit :
060800140521                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
060900140521                        = *zero       AND
061000140521             ( ds_TNTBE.TBEatb = *blank  or
061100140521               ds_TNTBE.TBEatb = TBLflg );
061200140521           dsSP = ds_TNTBE.TBEuni;
061300140521         endif;
061400140521
061500140521         // -?Impostazione campi nel record del subfile?
061600140521         clear  TB27S01;
061700140521
061800140521         VS1cod = %trimr( TBLkey );
061900140521         VS1des = ds1P.§1Pdes;
062000140521         VS1var = ds1P.§1Pvar;
062100140521         VS1uta = ds1P.§1Puta;
062200140521         VS1upr = ds1P.§1Pupr;
062300140521         VS1uar = ds1P.§1Puar;
062400140521         VS1tco = ds1P.§1Ptco;
062500140521         VS1fie = ds1P.§1Pfie;
062600140521         VS1fdp = ds1P.§1Pfdp;
062700140521         VS1ffe = ds1P.§1Pffe;
062800140521         VS1tst = ds1P.§1Ptst;
062900140521         VS1ost = ds1P.§1Post;
063000140521
063100140521         if TBLflg <> *blank;
063200140521           VS1atb = 'A';
063300140521         endif;
063400140521
063500140521         VS1des2 = dsSP.§SPdes;
063600140521         VS1dfe2 = dsSP.§SPdfe;
063700140521
063800140521         // -?Registrazione singolo record nel subfile?
063900140521         S01nrr += 1;
064000140521         write  TB27S01;
064100140521
064200140521       ENDSR;
064300140521
064400140521       //--------------------------------------------------------------
064500140521       //?Gestione tasto funzionale F13 da videata C01 / S01.          ?
064600140521       //?F13=Ripetizione opzione.                                     ?
064700140521       //--------------------------------------------------------------
064800140521       BEGSR  sr_DupOpzS01;
064900140521
065000140526         // -?Reperimento pagina del sfl e Nrr del suo primo record?
065100140521         //  ?dalla posizione del cursore?
065200140521         wPag = C1CsrRrn / c_SflPag;
065300140521         wRig = %rem( C1CsrRrn : c_SflPag);
065400140521         if  wRig > *zero;
065500140521           wRig = (wPag * c_SflPag) + 1;
065600140521         else;                           // -?ultimo rec. della pag.?
065700140521           wRig = (wPag * c_SflPag) - c_SflPag + 1;
065800140521         endif;
065900140521         SavS01csr = wRig;
066000140521
066100140521         // -?CONTROLLO OPZIONI INSERITE NEL SUBFILE:?
066200140521
066300140521         // -?Reperimento della 1ª opzione inserita?
066400140521         //  ?nella pagina dov'è il cursore?
066500140521         readc  TB27S01;
066600140526         DoW  Not %eof(TRTB27D)  and  VS1opz = *zero  and
066700140521              S01nrr < SavS01csr;
066800140521           SflNxtChg = *on;
066900140521           update  TB27S01;
067000140521           readc  TB27S01;
067100140521         EndDo;
067200140521         if  Not %eof(TRTB27D)  and  VS1opz <> *zero;
067300140521           SflNxtChg = *on;
067400140521           update  TB27S01;
067500140521         endif;
067600140521
067700140521         // -?Nessuna opzione nella pagina?
067800140521         if  %eof(TRTB27D)         or
067900140521             S01nrr <  SavS01csr   or
068000140521             S01nrr > (SavS01csr + c_SflPag - 1);
068100140521           C1RcdNbr  = SavS01csr;
068200140521           C1CsrRrn  = SavS01csr;
068300140521           reset  $ForzaOpz;
068400140521           reset  SavS1Copz;
068500140521           clear  SavS01cs1;
068600140521           clear  SavS01cs2;
068700140521           clear  SavS1Cop1;
068800140521           //clear  SavS1Cop2;
068900140521           ErrGenerico = *on;
069000140521           ErrMessage  = *on;
069100140521           PosCurOPZ   = *on;
069200140523           VIDmsg = sk_Msg(02);
069300140521           leavesr;
069400140521         endif;
069500140521
069600140521         // -?Memorizzazione 1ª opzione nella pagina?
069700140521         SavS01csr = S01nrr;
069800140521         SavS1Copz = VS1opz;
069900140521         if  SavS01csr <> SavS01cs1  or
070000140521             SavS1Copz <> SavS1Cop1;
070100140521           clear  $ForzaOpz;
070200140521         endif;
070300140521         SavS01cs1 = S01nrr;
070400140521         SavS1Cop1 = VS1opz;
070500140521
070600140521         // -?Reperimento delle altre eventuali opzioni?
070700140521         readc  TB27S01;
070800140521         DOW  Not %eof(TRTB27D);
070900140521
071000140521           // ·?Aggiornamento subfile?
071100140521           if  VS1opz <> *zero;
071200140521             SflNxtChg = *on;
071300140521             update  TB27S01;
071400140521           endif;
071500140521
071600140521           // ·?Reperita una 2ª opzione?
071700140521           Select;
071800140521
071900140521             // ·?Vuota!?
072000140521             When  VS1opz = *zero;
072100140521
072200140521             // ·?1ª opzione nella pagina del cursore  e?
072300140521             //  ?2ª opzione (diversa) in una pagina successiva?
072400140521             //  ?(messaggio di avviso di "ricopertura" - se la 2ª?
072500140521             //  ?opzione è diversa)?
072600140521             When  (SavS01csr >= wRig              and
072700140521                    SavS01csr <  wRig + c_SflPag   and
072800140521                    S01nrr    >= wRig + c_SflPag)  and
072900140521                    VS1opz    <> SavS1Copz         and
073000140521                    Not $ForzaOpz;
073100140521               $ForzaOpz = *on;
073200140521               SavS01cs2 = S01nrr;
073300140521               //SavS1Cop2 = VS1opz;
073400140521               C1RcdNbr  = SavS01csr;
073500140521               C1CsrRrn  = SavS01csr;
073600140521               ErrGenerico = *on;
073700140521               ErrMessage  = *on;
073800140521               PosCurOPZ   = *on;
073900140523               VIDmsg = sk_Msg(03);
074000140521               leavesr;
074100140521
074200140521             // ·?2ª opzione sul record dove è posizionato il cursore?
074300140521             //  ?(la nuova da considerare per la duplicazione)?
074400140521             When  C1CsrRrn = S01nrr;
074500140521               SavS01csr = S01nrr;
074600140521               SavS1Copz = VS1opz;
074700140521
074800140521             // ·?Più opzioni nella stessa videata e?
074900140521             //  ?cursore su nessuna delle opzioni?
075000140521             //  ?(riposizionare il cursore)?
075100140521             //  ?Se stessa opzione in videate diverse: vince la?
075200140521             //  ?posizione della prima opzione.?
075300140521             When  C1CsrRrn <> SavS01csr  and
075400140521                   C1CsrRrn <> S01nrr     and
075500140521                   S01nrr   <  wRig + c_SflPag;
075600140521               SavS01cs2 = S01nrr;
075700140521               //SavS1Cop2 = VS1opz;
075800140521               C1RcdNbr  = SavS01csr;
075900140521               C1CsrRrn  = SavS01csr;
076000140521               ErrGenerico = *on;
076100140521               ErrMessage  = *on;
076200140521               PosCurOPZ   = *on;
076300140523               VIDmsg = sk_Msg(04);
076400140521               leavesr;
076500140521
076600140521           EndSl;
076700140521
076800140521           readc  TB27S01;
076900140521
077000140521         ENDDO;
077100140521
077200140521         // ·?Opzione NON abilitata?
077300140521         if  ( TB27fun = '1'     and  SavS1Copz <> 1 )  or
077400140521             ( TB27fun = *blank  and  SavS1Copz <> 2
077500140521                                 and  SavS1Copz <> 3
077600140521                                 and  SavS1Copz <> 4
077700140521                                 and  SavS1Copz <> 5 );
077800140521           C1RcdNbr  = SavS01csr;
077900140521           C1CsrRrn  = SavS01csr;
078000140521           ErrGenerico = *on;
078100140521           ErrMessage  = *on;
078200140521           PosCurOPZ   = *on;
078300140523           VIDmsg = sk_Msg(05);
078400140521           leavesr;
078500140521         endif;
078600140521
078700140521         // -?Ripetizione opzione sul resto del subfile?
078800140521         S01nrr = SavS01csr + 1;
078900140521         chain  S01nrr  TB27S01;
079000140521         DoW  %found(TRTB27D);
079100140521           SflNxtChg = *on;
079200140521           VS1opz = SavS1Copz;
079300140521           update  TB27S01;
079400140521           S01nrr += 1;
079500140521           chain  S01nrr  TB27S01;
079600140521         EndDo;
079700140521
079800140521       ENDSR;
079900140521
080000140521       //--------------------------------------------------------------
080100140521       //?Gestione opzioni nel subfile S01.                            ?
080200140521       //--------------------------------------------------------------
080300140521       BEGSR  sr_OpzS01;
080400140521
080500140521         clear  SavS01csr;
080600140521
080700140521         // -?Ciclo di lettura subfile?
080800140521         readc  TB27S01;
080900140521
081000140521         DoW  Not %eof(TRTB27D);
081100140521
081200140521           SflNxtChg = *off;
081300140521           %subst(IndDspF : 50) = *off;
081400140521           C1RcdNbr = S01nrr;
081500140521
081600140521           Select;
081700140521
081800140521             // -?Nessuna opzione?
081900140521             When  VS1opz = *zero;
082000140521
082100140521             // -?Opzione NON abilitata?
082200140526             When  ( TB27fun = '1'     and  VS1opz <> 1 )  or
082300140526                   ( TB27fun = *blank  and  VS1opz <> 2
082400140526                                       and  VS1opz <> 3
082500140526                                       and  VS1opz <> 4
082600140526                                       and  VS1opz <> 5 );
082700140521               ErrGenerico = *on;
082800140521               ErrMessage  = *on;
082900140521               PosCurOPZ   = *on;
083000140523               VIDmsg = sk_Msg(05);
083100140521
083200140521             // -?Opz. 1 = Selezione.?
083300140521             When  VS1opz = 1  and  Not $Fine;
083400140521               $Fine = *on;
083500140521               TB27cod  = VS1cod;
083600140521               TB27des  = VS1des;
083700140521               TB27des2 = VS1des2;
083800140521
083900140521             // -?Opz. 1 = Selezione già reperita?
084000140521             //  ?(ammessa una sola selezione)?
084100140521             When  VS1opz = 1  and  $Fine;
084200140521               ErrGenerico = *on;
084300140521               ErrMessage  = *on;
084400140521               PosCurOPZ   = *on;
084500140523               VIDmsg = sk_Msg(06);
084600140521               $Fine  = *off;
084700140521
084800140521             // -?Opz. 2 = Modifica rec. Annullato?
084900140526             When  VS1opz = 2  and  VS1atb <> *blank;
085000140521               ErrGenerico = *on;
085100140521               ErrMessage  = *on;
085200140521               PosCurOPZ   = *on;
085300140523               VIDmsg = sk_Msg(07);
085400140521
085500140521             // -?Opz. 2 = Modifica,?
085600140521             //       ?3 = Copia,?
085700140521             //       ?4 = Annullamento / Ripristino,?
085800140521             //       ?5 = Visualizzazione.?
085900140521             When  VS1opz = 2  or
086000140521                   VS1opz = 3  or
086100140521                   VS1opz = 4  or
086200140521                   VS1opz = 5;
086300140521               $Video  = 'D1';
086400140521               $InzD01 = *on;
086500140526               TB27opz = VS1opz;
086600140521               DoW  $Video = 'D1'  or  $Video = 'D2';
086700140521                 exsr  sr_GesD0102;
086800140521                 if  $Fine = *on;
086900140521                   ErrGenerico = *on;
087000140521                   leavesr;
087100140521                 endif;
087200140521               EndDo;
087300140526               clear  VT1opzD;
087400140521               // -?Se F12=Ritorno: esce dalla lettura del subfile?
087500140521               if  TB27fxx = 'L';
087600140521                 ErrGenerico = *on;
087700140521               endif;
087800140521
087900140521             // -?Opzione errata?
088000140521             Other;
088100140521               ErrGenerico = *on;
088200140521               ErrMessage  = *on;
088300140521               PosCurOPZ   = *on;
088400140523               VIDmsg = sk_Msg(05);
088500140521
088600140521           EndSl;
088700140521
088800140521           // -?Memorizzazione nrr del subfile con la 1ª opzione per?
088900140521           //  ?riposizionarvicisi il cursore dopo il tasto "Invio"?
089000140521           if  VS1opz  <> *zero   and
089100140521               (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
089200140521             SavS01csr = C1RcdNbr;
089300140521           endif;
089400140521
089500140521           // -?Aggiornamento subfile?
089600140521           select;
089700140521             when  ErrMessage;
089800140521               SflNxtChg = *on;
089900140521               C1CsrRrn  = C1RcdNbr;
090000140521             when  ErrGenerico;
090100140521               C1CsrRrn  = C1RcdNbr;
090200140521               clear  VS1opz;
090300140521             other;
090400140521               clear  VS1opz;
090500140521           endsl;
090600140521
090700140521           UPDATE  TB27S01;
090800140521
090900140521           if  ErrGenerico   or   ErrMessage;
091000140521             leave;
091100140521           endif;
091200140521
091300140521           readc  TB27S01;
091400140521
091500140521         ENDDO;
091600140521
091700140521         // -?Riposiziono il cursore sul record contenente la prima opz.?
091800140521         //  ?(se non sono stati rilevati errori)?
091900140521         if  NOT ErrMessage   and   NOT ErrGenerico   and
092000140521             SavS01csr > *zero;
092100140521           C1CsrRrn = SavS01csr;
092200140521         endif;
092300140521
092400140521       ENDSR;
092500140520
092600140520       //--------------------------------------------------------------
092700140521       //?Gestione videate D01/D02 - Dati Tariffa in tab. "1P"/"SP".   ?
092800140520       //--------------------------------------------------------------
092900140521       BEGSR  sr_GesD0102;
093000140520
093100140521         // -?Inizializzazione videate?
093200140520         if  $InzD01 = *on;
093300140521           exsr  sr_InzD01;
093400140521           exsr  sr_InzD02;
093500140520           $InzD01 = *off;
093600140520         endif;
093700140521
093800140521         // -?Impostazione attributi di visualizzazione?
093900140521         VisAnnull  = ( TBLflg <> *blank );
094000140526         ProtectCOD = ( TB27fxx <> 'J'  and  TB27opz <> 3 );
094100140526         //ProtectALL = ( TB27opz = 4  or  TB27opz = 5 );
094200140521
094300140521         // -?(Ri)Abilitazione tasti funzionali?
094400140526         F6Attivo  = ( TB27opz <> 5 );
094500140526         F12Attivo = *off;  //?F12 sempre attivo. Il flag serve solo x Protect.?
094600140526         RUpAttivo = ( $Video = 'D1'  and
094700140526                     ( (VS1opz = 4  and  TBLflg = *blank)  or
094800140526                        VS1opz = 5 ) );
094900140526         RDnAttivo = ( $Video = 'D2'  and
095000140526                     ( (VS1opz = 4  and  TBLflg = *blank)  or
095100140526                        VS1opz = 5 ) );
095200140520
095300140521         // -?Emissione Testata e Piede?
095400140521         write  TB27T01;
095500140521         write  TB27P02;
095600140520
095700140520         // -?Emissione videata?
095800140521         if  (VS1opz = 4  and  TBLflg = *blank)  or
095900140521              VS1opz = 5;
096000140521           if  $Video = 'D1';
096100140521             write  TB27D01;
096200140521           else;
096300140521             write  TB27D02;
096400140521           endif;
096500140521           exfmt  Protect;
096600140521         else;
096700140521           if  $Video = 'D1';
096800140521             exfmt  TB27D01;
096900140521           else;
097000140521             exfmt  TB27D02;
097100140521           endif;
097200140521         endif;
097300140520
097400140520         clear  VIDmsg;
097500140520         reset  ErrMessage;
097600140520         reset  ErrGenerico;
097700140520
097800140520         SELECT;
097900140520
098000140520           // -?F3=Fine?
098100140520           WHEN  dsp_aid = c_F03;
098200140526             unlock  TABEL00F;
098300140526             $Fine   = *on;
098400140521             TB27fxx = 'C';
098500140521
098600140521           // -?F8=Successivo?
098700140521           WHEN  dsp_aid = c_F08;
098800140526             unlock  TABEL00F;
098900140523             $Video  = 'S1';
099000140521             TB27fxx = 'H';
099100140521
099200140521           // -?F9=Traduzione?
099300140523           //  ?(sola visualizzazione)?
099400140521           WHEN  dsp_aid = c_F09;
099500140523             $VideoPrec  = $Video;
099600140521             $Video  = 'S2';
099700140521             $InzS02 = *on;
099800140521             DoW  $Video = 'S2';
099900140521               exsr  sr_GesS02;
100000140521               if  $Fine = *on;
100100140521                 ErrGenerico = *on;
100200140521                 leavesr;
100300140521               endif;
100400140521             EndDo;
100500140521
100600140521           // -?F12=Ritorno?
100700140521           WHEN  dsp_aid = c_F12;
100800140521             unlock  TABEL00F;
100900140526             if  $Video = 'D1';
101000140526               $Video  = 'S1';
101100140526             else;
101200140526               $Video  = 'D1';
101300140526             endif;
101400140526             TB27fxx = 'L';
101500140521
101600140521           // -?RollUp=Videata successiva?
101700140521           WHEN  dsp_aid = c_RollUp  and  $Video = 'D1';
101800140526             $Video  = 'D2';
101900140521
102000140521           // -?RollDown=Videata precedente?
102100140521           WHEN  dsp_aid = c_RollDown  and  $Video = 'D2';
102200140526             $Video  = 'D1';
102300140520
102400140521           // -?Invio, F6=Conferma?
102500140520           OTHER;
102600140521             // -?Controlli eseguiti SE NON richiesto annullamento?
102700140526             If  VS1opz <> 4  or
102800140526                 (VS1opz = 4  and  TBLflg <> *blank);
102900140526               $Video  = 'D1';
103000140526               exsr  sr_CtrD01;
103100140526               if  ErrGenerico;
103200140526                 leavesr;
103300140526               else;
103400140526                 $Video  = 'D2';
103500140526                 exsr  sr_CtrD02;
103600140526                 if  ErrGenerico;
103700140526                   leavesr;
103800140526                 endif;
103900140521               endif;
104000140521             EndIf;
104100140521
104200140521             // -?F6=Aggiornamento?
104300140521             IF  dsp_aid = c_F06;
104400140523
104500140521               // -?Richiesta dati di trasmissione?
104600140521               $Video  = 'W1';
104700140521               $InzW01 = *on;
104800140521               DoW  $Video = 'W1';
104900140521                 exsr  sr_GesW01;
105000140521               EndDo;
105100140523
105200140523               IF  dsp_aid = c_F06;
105300140523
105400140523                 // -?Aggiornamento tabelle "1P" e "SP"?
105500140523                 //  ?(lingua "Italiano")?
105600140523                 exsr  sr_UpdTABEL;
105700140523                 if  ErrGenerico;
105800140523                   leavesr;
105900140523                 endif;
106000140526                 TB27fxx = 'F';
106100140523
106200140523                 // -?Gestione Traduzioni?
106300140523                 //  ?(SE NON richiesto Annullamento/Ripristino)?
106400140526                 If  ( TB27fxx = 'J'  or  TB27opz <> 4 )  and
106500140523                     ds.§fgLin = 'S';
106600140523                   $Video  = 'S2';
106700140523                   $InzS02 = *on;
106800140523                   DoW  $Video = 'S2';
106900140523                     exsr  sr_GesS02;
107000140523                     if  ErrGenerico;
107100140523                       leavesr;
107200140523                     endif;
107300140523                   EndDo;
107400140523                 EndIf;
107500140523
107600140523               ENDIF;
107700140523
107800140521             ENDIF;
107900140520
108000140520         ENDSL;
108100140520
108200140520       ENDSR;
108300140520
108400140520       //--------------------------------------------------------------
108500140521       //?Inizializzazione videata D01 - Dati della Tariffa / Tab."1P".?
108600140520       //--------------------------------------------------------------
108700140521       BEGSR  sr_InzD01;
108800140520
108900140521         clear  TB27D01;
109000140521
109100140521         // -?Reperimento dati tabella (SE NON inserimento)?
109200140521         clear  ds1P;
109300140523         clear  wNewTBLflg;
109400140523         clear  SaveTBLftt;
109500140523         clear  SaveTBLflt;
109600140523         clear  SaveTBLftr;
109700140523         clear  SaveTBLdtr;
109800140521
109900140521         IF  TB27fxx <> 'J';
110000140521
110100140521           k_TBLkut = c_Kut;
110200140521           k_TBLcod = c_Tab;
110300140521           k_TBLkey = VS1cod;
110400140521           if  VS1opz = 3  or  VS1opz = 5;
110500140526             chain(N)  %kds(keyTABEL00)  TABEL;
110600140521           else;
110700140526             chain  %kds(keyTABEL00)  TABEL;
110800140521           endif;
110900140521           if  %found(TABEL00F);
111000140521             ds1P = TBLuni;
111100140523             wNewTBLflg = TBLflg;
111200140523             SaveTBLftt = TBLftt;
111300140523             SaveTBLflt = TBLflt;
111400140523             SaveTBLftr = TBLftr;
111500140523             SaveTBLdtr = TBLdtr;
111600140521           endif;
111700140523
111800140523           // -?Annullamento / Ripristino?
111900140526           If  VS1opz = 4;
112000140523             select;
112100140523               // ·?Annullamento?
112200140523               when  TBLflg = *blank;
112300140523                 wNewTBLflg = '*';
112400140523               // ·?Ripristino?
112500140523               when  TBLflg <> *blank;
112600140523                 clear  wNewTBLflg;
112700140523             endsl;
112800140523           EndIf;
112900140521
113000140521         ENDIF;
113100140521
113200140521         // -?Impostazione testata?
113300140526         clear  VT1opzD;
113400140521         select;
113500140521           when  TB27fxx = 'J';
113600140526             //VT1opzD = '  IMMISSIONE   ';
113700140526             VT1opzD = '  INSERIMENTO  ';
113800140521             leavesr;
113900140521           when  VS1opz = 2;
114000140526             VT1opzD = '   MODIFICA    ';
114100140521           when  VS1opz = 3;
114200140526             VT1opzD = '     COPIA     ';
114300140521           when  VS1opz = 4  and  TBLflg = *blank;
114400140526             VT1opzD = ' ANNULLAMENTO  ';
114500140521           when  VS1opz = 4  and  TBLflg = '*';
114600140526             VT1opzD = '  RIPRISTINO   ';
114700140521           when  VS1opz = 5;
114800140526             VT1opzD = 'VISUALIZZAZIONE';
114900140521           other;
115000140526             VT1opzD = ' ? ? ? ? ? ? ? ';
115100140521         endsl;
115200140521
115300140521         // -?Impostazione chiave?
115400140521         if  VS1opz <> 3;
115500140521           V1Ccod = TBLkey;
115600140521         endif;
115700140521
115800140521         // -?Impostazione dettaglio?
115900140521         V1Cdes   = ds1P.§1Pdes;
116000140521         V1Cfcp   = ds1P.§1Pfcp;
116100140521         V1Cuta   = ds1P.§1Puta;
116200140521         V1Cupr   = ds1P.§1Pupr;
116300140521         V1Cuar   = ds1P.§1Puar;
116400140521         V1Cvar   = ds1P.§1Pvar;
116500140521         V1Ctco   = ds1P.§1Ptco;
116600140521         V1Cfie   = ds1P.§1Pfie;
116700140521         V1Cfpt   = ds1P.§1Pfpt;
116800140521         V1Cfdp   = ds1P.§1Pfdp;
116900140521         V1Cffe   = ds1P.§1Pffe;
117000140521         V1Cmodtp = ds1P.§1Pmodtp;
117100140521         V1Ctt0   = ds1P.§1Ptt0;
117200140521         V1Ctt1   = ds1P.§1Ptt1;
117300140521         V1Ctt2   = ds1P.§1Ptt2;
117400140521         V1Ctt3   = ds1P.§1Ptt3;
117500140521         V1Ctt4   = ds1P.§1Ptt4;
117600140521         V1Ctt5   = ds1P.§1Ptt5;
117700140521         V1Ctt6   = ds1P.§1Ptt6;
117800140521         V1Ctt7   = ds1P.§1Ptt7;
117900140521         V1Ctt8   = ds1P.§1Ptt8;
118000140521         V1Ctt9   = ds1P.§1Ptt9;
118100140521         V1Cttv   = ds1P.§1Pttv;
118200140521         V1Cdrf   = ds1P.§1Pdrf;
118300140521         V1Casc   = ds1P.§1Pasc;
118400140521         V1Crcv   = ds1P.§1Prcv;
118500140521         V1Cvas   = ds1P.§1Pvas;
118600140521         V1Cvvm   = ds1P.§1Pvvm;
118700140521         V1Cvca   = ds1P.§1Pvca;
118800140521         V1Cvmn   = ds1P.§1Pvmn;
118900140521         V1Cvmx   = ds1P.§1Pvmx;
119000140521         V1Ccts   = ds1P.§1Pcts;
119100140521         V1Ccte   = ds1P.§1Pcte;
119200140521         V1Cfqt   = ds1P.§1Pfqt;
119300140521         V1Cdfe   = ds1P.§1Pdfe;
119400140521         V1Ctps   = ds1P.§1Ptps;
119500140521         V1Cfpc   = ds1P.§1Pfpc;
119600140521         V1Ctst   = ds1P.§1Ptst;
119700140521         V1Cost   = ds1P.§1Post;
119800140521         V1Cain   = ds1P.§1Pain;
119900140521         V1Cptv   = ds1P.§1Pptv;
120000140521         V1Cpda   = ds1P.§1Ppda;
120100140526         wDate_EUR = *loval;
120200140521         if  ds1P.§1Peli > *zero;
120300140521           Monitor;
120400140526             wDate_EUR = %date( %int( ds1P.§1Peli ) : *iso );
120500140526           On-Error;
120600140521           EndMon;
120700140521         endif;
120800140521         if  wDate_EUR > *loval;
120900140521           V1Celi   = %dec( wDate_Eur );
121000140521         endif;
121100140521
121200140521       ENDSR;
121300140521
121400140521       //--------------------------------------------------------------
121500140521       //?Inizializzazione videata D02 - Dati della Tariffa / Tab."SP".?
121600140521       //--------------------------------------------------------------
121700140521       BEGSR  sr_InzD02;
121800140521
121900140521         clear  TB27D02;
122000140521
122100140521         // -?Reperimento dati tabella (SE NON inserimento)?
122200140521         clear  dsSP;
122300140521
122400140521         IF  TB27fxx <> 'J';
122500140521
122600140521           k_TBLkut = c_Kut;
122700140521           k_TBLcod = c_Tab2;
122800140521           k_TBLkey = VS1cod;
122900140521           if  VS1opz = 3  or  VS1opz = 5;
123000140526             chain(N)  %kds(keyTABEL00)  TABEL;
123100140521           else;
123200140526             chain  %kds(keyTABEL00)  TABEL;
123300140521           endif;
123400140521           if  %found(TABEL00F);
123500140521             dsSP = TBLuni;
123600140521           endif;
123700140521
123800140521         ENDIF;
123900140521
124000140521         // -?Impostazione dettaglio?
124100140523         V2Ccod   = V1Ccod;
124200140523         V2Cdes1  = V1Cdes;
124300140523         V2Cdes   = dsSP.§SPdes;
124400140521         V2Cdfe   = dsSP.§SPdfe;
124500140521         V2Cdac   = dsSP.§SPdac;
124600140523         V2Csud   = dsSP.§SPsud;
124700140523         V2Caba   = dsSP.§SPaba;
124800140521
124900140521       ENDSR;
125000140521
125100140521       //--------------------------------------------------------------
125200140521       //?Controllo dati nella videata D01.                            ?
125300140521       //--------------------------------------------------------------
125400140521       BEGSR  sr_CtrD01;
125500140523
125600140523         %subst(IndDspF : 50) = *off;
125700140521
125800140523         // -?Codice Tariffa Particolare obbligatorio?
125900140523         if  V1Ccod = *blank;
126000140523           VIDmsg = sk_Msg(08);
126100140523           PosCurCOD   = *on;
126200140523           ErrMessage  = *on;
126300140523           ErrGenerico = *on;
126400140523           leavesr;
126500140523         endif;
126600140523
126700140523         // -?Immissione: Codice Tariffa Particolare esistente?
126800140526         If  TB27fxx = 'J'  or  TB27opz = 3;
126900140523
127000140523           k_TBLkut = c_Kut;
127100140523           k_TBLcod = c_Tab2;
127200140526           k_TBLkey = V1Ccod;
127300140523
127400140526           chain(N)  %kds(keyTABEL00)  TABEL;
127500140523
127600140523           if  %found(TABEL00F);
127700140523             VIDmsg = sk_Msg(09);
127800140523             PosCurCOD   = *on;
127900140523             ErrMessage  = *on;
128000140523             ErrGenerico = *on;
128100140523             leavesr;
128200140523           endif;
128300140523
128400140523         EndIf;
128500140523
128600140523         // -?Descrizione Tariffa Particolare obbligatoria?
128700140523         if V1Cdes = *blank;
128800140523           VIDmsg = sk_Msg(10);
128900140523           PosCurDES   = *on;
129000140523           ErrMessage  = *on;
129100140523           ErrGenerico = *on;
129200140523           leavesr;
129300140523         endif;
129400140523
129500140523         // -?Tipo Tariffa?
129600140523         For xx = 1  by 1  to %elem(sk_CTT);
129700140523
129800140523           Select;
129900140523             When sk_CTT(xx) = *blanks;
130000140523             When sk_CTT(xx) = '?';
130100140526               TABEL_Ricerca ( 'TR'
130200140526                             : *blank
130300140526                             : Local.idElemento
130400140526                             : Local.tastoFunzionaleUscita
130500140526                             );
130600140526               if Local.tastoFunzionaleUscita = *blank;
130700140526                 sk_CTT(xx) = Local.idElemento;
130800140526               else;
130900140526                 clear  sk_CTT(xx);
131000140526               endif;
131100140523               %subst( IndDspF : 52 + xx ) = *on;
131200140523             Other;
131300140523               if  getTabella ( c_Tabel : 'TR' : sk_CTT(xx) :
131400140523                                *omit : *omit : *omit :
131500140523                                *omit : *omit :
131600140523                                *omit : *omit : *omit : *omit :
131700140523                                *omit : *omit : *omit : *omit :
131800140523                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
131900140523                             <> *zero      OR
132000140523                   ds_TNTBE.TBEatb <> *blank;
132100140523                 VIDmsg = sk_Msg(11);
132200140523                 %subst( IndDspF : 52 + xx ) = *on;
132300140523                 ErrMessage  = *on;
132400140523                 ErrGenerico = *on;
132500140523                 leavesr;
132600140523               endif;
132700140523           EndSl;
132800140523         EndFor;
132900140523
133000140523         // -?Codice Tassazione Italia?
133100140523         Select;
133200140523           When  V1Ccts = '?';
133300140523             clear  TRTB09ds;
133400140523             TB09fun = '1';
133500140523             TB09ord = '1';
133600140523             kpjbu = TRTB09ds;
133700140523             TRTB09R ( kpjba );
133800140523             TRTB09ds = kpjbu;
133900140523             V1Ccts = TB09cod;
134000140523             PosCurCTS = *on;
134100140523           When  V1Ccts <> *blank;
134200140523             if  getTabella ( c_Tabel : 'CT' : V1Ccts :
134300140523                              *omit : *omit : *omit :
134400140523                              *omit : *omit :
134500140523                              *omit : *omit : *omit : *omit :
134600140523                              *omit : *omit : *omit : *omit :
134700140523                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
134800140523                           <> *zero      OR
134900140523                 ds_TNTBE.TBEatb <> *blank;
135000140523               VIDmsg = sk_Msg(12);
135100140523               PosCurCTS   = *on;
135200140523               ErrMessage  = *on;
135300140523               ErrGenerico = *on;
135400140523               leavesr;
135500140523             endif;
135600140523         EndSl;
135700140523
135800140523         // -?Codice Tassazione Estero?
135900140523         Select;
136000140523           When  V1Ccte = '?';
136100140523             clear  TRTB09ds;
136200140523             TB09fun = '1';
136300140523             TB09ord = '1';
136400140523             kpjbu = TRTB09ds;
136500140523             TRTB09R ( kpjba );
136600140523             TRTB09ds = kpjbu;
136700140523             V1Ccte = TB09cod;
136800140523             PosCurCTE = *on;
136900140523           When  V1Ccte <> *blank;
137000140523             if  getTabella ( c_Tabel : 'CT' : V1Ccte :
137100140523                              *omit : *omit : *omit :
137200140523                              *omit : *omit :
137300140523                              *omit : *omit : *omit : *omit :
137400140523                              *omit : *omit : *omit : *omit :
137500140523                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
137600140523                           <> *zero      OR
137700140523                 ds_TNTBE.TBEatb <> *blank;
137800140523               VIDmsg = sk_Msg(12);
137900140523               PosCurCTE   = *on;
138000140523               ErrMessage  = *on;
138100140523               ErrGenerico = *on;
138200140523               leavesr;
138300140523             endif;
138400140523         EndSl;
138500140523
138600140523         // -?Data Eliminazione?
138700140526        If  V1Celi <> *zero;
138800140523          clear  WLBdat;
138900140526          G08dat = V1Celi;
139000140523          xsrda8 ( WLBdat );
139100140523          if  G08err =  *on;
139200140523            VIDmsg = sk_Msg(13);
139300140523            PosCurELI   = *on;
139400140523            ErrMessage  = *on;
139500140523            ErrGenerico = *on;
139600140523            leavesr;
139700140523          endif;
139800140523        EndIf;
139900140521
140000140523       ENDSR;
140100140521
140200140521       //--------------------------------------------------------------
140300140521       //?Controllo dati nella videata D02.                            ?
140400140521       //--------------------------------------------------------------
140500140521       BEGSR  sr_CtrD02;
140600140523
140700140523         %subst(IndDspF : 50) = *off;
140800140521
140900140523         // -?Descrizione Tariffa Particolare obbligatoria?
141000140523         if V2Cdes = *blank;
141100140523           VIDmsg = sk_Msg(10);
141200140523           PosCurDES2  = *on;
141300140523           ErrMessage  = *on;
141400140523           ErrGenerico = *on;
141500140523           leavesr;
141600140523         endif;
141700140521
141800140521       ENDSR;
141900140523
142000140523       //--------------------------------------------------------------
142100140523       //?Gestione window W01.                                         ?
142200140523       //--------------------------------------------------------------
142300140523       BEGSR  sr_GesW01;
142400140523
142500140523         // -?Inizializzazione videata?
142600140523         if  $InzW01 = *on;
142700140523           exsr  sr_InzW01;
142800140523           $InzW01 = *off;
142900140523         endif;
143000140523
143100140523         // -?Emissione Window?
143200140526         exfmt  TB27W01;
143300140523
143400140523         reset  ErrGenerico;
143500140523         reset  ErrMessage;
143600140523         clear  VIDmsg;
143700140523
143800140523         Select;
143900140523
144000140523           // -?F12 = Ritorno?
144100140523           When  dsp_aid = c_F12;
144200140523             $Video  = 'D2';
144300140523             //TB39fxx = 'F12';
144400140523
144500140523           // -?Sola visualizzazione?
144600140523           When  F12Attivo;
144700140523             leavesr;
144800140523
144900140523         EndSl;
145000140523
145100140523       ENDSR;
145200140523
145300140523       //--------------------------------------------------------------
145400140523       //?Inizializzazione window W01.                                 ?
145500140523       //--------------------------------------------------------------
145600140523       BEGSR  sr_InzW01;
145700140523
145800140523         clear  TB27W01;
145900140523
146000140523         // -?Se non immissione: imposta i campi del file?
146100140526         If  TB27fxx <> 'J';
146200140523
146300140523           W01ftt = SaveTBLftt;
146400140523           W01flt = SaveTBLflt;
146500140523           W01ftr = SaveTBLftr;
146600140523           // ·?Conversione data (se NON immissione o copia)?
146700140526           if  VS1opz <> 3  and  SaveTBLdtr  > *zero;
146800140523             wDate_DMY = %date(SaveTBLdtr : *ymd);
146900140523             W01dtr    = %dec( wDate_DMY );
147000140523           endif;
147100140523
147200140523         EndIf;
147300140523
147400140523       ENDSR;
147500140523
147600140523       //--------------------------------------------------------------
147700140523       //?Gestione subfile S02 - Subfile in lingua.                    ?
147800140523       //--------------------------------------------------------------
147900140523       BEGSR  sr_GesS02;
148000140523
148100140523         // -?Inizializzazione videata?
148200140523         if  $InzS02;
148300140523           exsr  sr_InzS02;
148400140523           $InzS02 = *off;
148500140523         endif;
148600140523
148700140523         // -?Emissione Testata & Piede con tasti funzionali abilitati?
148800140523         write  TB27T01;
148900140523         write  TB27P03;
149000140523
149100140523         // -?Posizionamento cursore?
149200140523         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
149300140523         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
149400140523         //  ?si visualizza la pagina che vedeva l'utente quando ha?
149500140523         //  ?premuto l'ultimo tasto.?
149600140523         if  C2CsrRrn > *zero;
149700140523           C2RcdNbr = C2CsrRrn;
149800140523         else;
149900140523           C2RcdNbr = 1;
150000140523           write  TB27S00;             //?(no rec.)?
150100140523         endif;
150200140523
150300140523         // -?Emissione videata?
150400140526         if  Not  F12Attivo;
150500140526           exfmt  TB27C02;
150600140526         else;
150700140526           write  TB27C02;
150800140526           exfmt  Protect;
150900140526         endif;
151000140523
151100140523         clear  VIDmsg;
151200140523         reset  ErrMessage;
151300140523         reset  ErrGenerico;
151400140523
151500140523         Select;
151600140523
151700140523           // -?F3=Fine?
151800140523           //When  dsp_aid = c_F03;
151900140523           //  TB27fxx = 'C';
152000140523           //  $Fine = *on;
152100140523
152200140523           // -?F12=Ritorno?
152300140523           When  dsp_aid = c_F12;
152400140523             $Video = $VideoPrec;
152500140523
152600140523           // -?Invio, F6=Conferma?
152700140523           //Other;
152800140523           When  S02nrr > *zero;
152900140523             // -?Controllo subfile Lingue?
153000140523             exsr  sr_CtrS02;
153100140523             select;
153200140523               when  ErrGenerico;
153300140523                 leavesr;
153400140523               when  dsp_aid = c_F06;
153500140523                 exsr  sr_F06S02;
153600140523             endsl;
153700140523
153800140523         EndSl;
153900140523
154000140523       ENDSR;
154100140523
154200140523       //--------------------------------------------------------------
154300140523       //?Inizializzazione subfile S02 - Descrizioni in lingua.        ?
154400140523       //--------------------------------------------------------------
154500140523       BEGSR  sr_InzS02;
154600140523
154700140523         // -?Spegnimento degli indicatori di errore?
154800140523         %subst(IndDspF : 50) = *off;
154900140523
155000140523         // -?Pulizia subfile-control?
155100140523         clear  TB27C02;
155200140523
155300140523         C2Ccod  = V1Ccod;
155400140523         C2Cdes  = V1Cdes;
155500140523         C2Cdes2 = V2Cdes;
155600140523         C2Cdfe2 = V2Cdfe;
155700140523
155800140523         // -?Pulizia subfile?
155900140523         Sfl2Dsp_N    = *on;
156000140523         Sfl2DspCtl_N = *on;
156100140523         write  TB27C02;
156200140523         Sfl2DspCtl_N = *off;
156300140523         Sfl2End      = *off;
156400140523
156500140523         clear  C2RcdNbr;
156600140523         clear  C2CsrRrn;
156700140523         clear  S02nrr;
156800140523         clear  VIDmsg;
156900140523         ErrMessage  = *off;
157000140523         ErrGenerico = *off;
157100140523
157200140523         Sfl2NxtChg  = *off;
157300140523
157400140523         // -?Caricamento completo dei dati nel subfile (per codice)?
157500140523         setgt  ( c_Kut )  AZLIN000;
157600140523         read  AZLIN000;
157700140523
157800140523         DoW  Not %eof(AZLIN01L)  and  S02nrr < c_MaxRec;
157900140523           exsr  sr_WriteS02;
158000140523           read  AZLIN000;
158100140523         EndDo;
158200140523
158300140523         // -?Memorizzazione Nrr ultimo record caricato?
158400140523         //  ?(servirà per ripristinarlo DOPO la gestione di?
158500140523         //  ?un'opzione, PRIMA di un nuovo ordinamento del subfile)?
158600140523         // -?Impaginazione subfile?
158700140523         //  ?-> forza l'impaginazione sul 1° rec. del subfile?
158800140523         if  S02nrr > *zero;
158900140523           C2RcdNbr = 1;
159000140523           C2CsrRrn = 1;
159100140523         else;
159200140523           clear C2RcdNbr;
159300140523         endif;
159400140523
159500140523         // -?Visualizzazione del SFL (se ci sono dati)?
159600140523         Sfl2Dsp_N = ( S02nrr <= *zero );
159700140523
159800140523         // -?Attivazione del SFLEND?
159900140526         Sfl2End = %eof( AZLIN01L );
160000140523
160100140523         // -?(Dis)Abilitazione tasti funzionali?
160200140523         F12Attivo = ( dsp_aid = c_F09 );
160300140523
160400140523       ENDSR;
160500140523
160600140523       //--------------------------------------------------------------
160700140523       //?Registrazione del singolo rec. nel subfile S02.              ?
160800140523       //--------------------------------------------------------------
160900140523       BEGSR  sr_WriteS02;
161000140523
161100140523         clear  ds1P;
161200140523         clear  dsSP;
161300140523
161400140523         k_TBLkut = LINtabel;
161500140523         k_TBLcod = c_Tab;
161600140523         k_TBLkey = C2Ccod;
161700140523         chain(N)  %kds( keyTABEL00 )  TABEL;
161800140523         if  %found( TABEL00F );
161900140523           ds1P = TBLuni;
162000140523         endif;
162100140523
162200140523         k_TBLcod = c_Tab2;
162300140523         chain(N)  %kds( keytabel00 )  TABEL;
162400140523         if  %found(TABEL00F);
162500140523           dsSP = TBLuni;
162600140523         endif;
162700140523
162800140523         // -?Impostazione campi nel record del subfile?
162900140523         clear  TB27S02;
163000140523
163100140526         S2HcdLin = %editc( TBLkut : 'X' );
163200140523
163300140523         S2Dlin   = LINdesIta;
163400140523
163500140526         S2Dtip   = 'Normale';
163600140523         S2Cdes   = ds1P.§1Pdes;
163700140526         S2Dtip2  = 'Estesa ';
163800140523         S2Cdes2  = dsSP.§SPdes;
163900140526         S2DtipF  = 'FedEx  ';
164000140523         S2Cdfe2  = dsSP.§SPdfe;
164100140526
164200140526         // -?Impostazione attributi di Visualizzazione?
164300140526         //  ?- & (Dis)Abilitazione tasti funzionali -?
164400140526         F12Attivo = ( dsp_aid = c_F09 );
164500140523
164600140523         // -?Registrazione singolo record nel subfile?
164700140523         S02nrr += 1;
164800140523         write  TB27S02;
164900140523
165000140523       ENDSR;
165100140523
165200140523       //--------------------------------------------------------------
165300140523       //?Controllo dati nel subfile S02.                              ?
165400140523       //--------------------------------------------------------------
165500140523       BEGSR  sr_CtrS02;
165600140523
165700140523         clear  SavS02csr;
165800140523
165900140523         // -?Ciclo di lettura subfile?
166000140523         readc  TB27S02;
166100140523
166200140523         DoW  Not %eof(TRTB27D);
166300140523
166400140523           Sfl2NxtChg = *off;
166500140523           %subst(IndDspF : 50) = *off;
166600140523           C2RcdNbr = S02nrr;
166700140523
166800140523           Select;
166900140523
167000140523             // -?Controllo descrizione in lingua per la tab. "1P"?
167100140523             When  S2Cdes  = *blank;
167200140523               ErrGenerico = *on;
167300140523               ErrMessage  = *on;
167400140523               PosCurS2des = *on;
167500140523               VIDmsg = sk_Msg(10);
167600140523
167700140523             // -?Controllo Descr.Estesa in lingua per la tab. "SP"?
167800140523             When  S2Cdes2 = *blank;
167900140523               ErrGenerico = *on;
168000140523               ErrMessage  = *on;
168100140523               PosCurS2dee = *on;
168200140523               VIDmsg = sk_Msg(10);
168300140523
168400140523             // -?Controllo Descr.FedEx in lingua per la tab. "SP"?
168500140523             When  S2Cdfe2 = *blank;
168600140523
168700140523           EndSl;
168800140523
168900140523           // -?Memorizzazione nrr del subfile con la 1ª descriz. per?
169000140523           //  ?riposizionarvicisi il cursore dopo il tasto "Invio"?
169100140523           if  (S2Cdes  <> *blank  or  S2Cdes2 <> *blank)  and
169200140523               (SavS02csr = *zero  or  SavS02csr < C2RcdNbr);
169300140523             SavS02csr = C2RcdNbr;
169400140523           endif;
169500140523
169600140523           // -?Aggiornamento subfile?
169700140523           select;
169800140523             when  ErrMessage;
169900140523               Sfl2NxtChg = *on;
170000140523               C2CsrRrn  = C2RcdNbr;
170100140523             when  ErrGenerico;
170200140523               Sfl2NxtChg = *on;
170300140523               C2CsrRrn  = C2RcdNbr;
170400140523             when  S2Cdes <> *blank;
170500140523               Sfl2NxtChg = *on;
170600140523               //C2CsrRrn  = C2RcdNbr;
170700140523           endsl;
170800140523
170900140523           UPDATE  TB27S02;
171000140523
171100140523           if  ErrGenerico   or   ErrMessage;
171200140523             leave;
171300140523           endif;
171400140523
171500140523           readc  TB27S02;
171600140523
171700140523         ENDDO;
171800140523
171900140523         // -?Riposiziono il cursore sul record contenente la prima opz.?
172000140523         //  ?(se non sono stati rilevati errori)?
172100140523         if  NOT ErrMessage   and   NOT ErrGenerico   and
172200140523             SavS02csr > *zero;
172300140523           C2CsrRrn = SavS02csr;
172400140523         endif;
172500140523
172600140523       ENDSR;
172700140521
172800140521       //--------------------------------------------------------------
172900140523       //?Aggiornamento records nel file TABEL00F e AZLIN01L.          ?
173000140521       //--------------------------------------------------------------
173100140521       BEGSR  sr_UpdTABEL;
173200140523
173300140523         // -?Aggiornamento tabella "SP"?
173400140523         exsr  sr_UpdTabSP;
173500140521
173600140523
173700140523         // -?RI-aggancio record da aggiornare su TABEL00F?
173800140523         //  ?(tab. "1P")?
173900140523         k_TBLkut = c_Kut;
174000140523         k_TBLcod = c_Tab;
174100140523         k_TBLkey = V1Ccod;
174200140523
174300140526         chain  %kds(keyTABEL00)  TABEL;
174400140523
174500140523         // -?Impostazione campi-chiave SE record NON trovato?
174600140523         if  Not %found(TABEL00F);
174700140523           clear  TABEL;
174800140523           TBLkut = k_TBLkut;
174900140523           TBLcod = k_TBLcod;
175000140523           TBLkey = k_TBLkey;
175100140523         endif;
175200140523
175300140523         // -?Impostazione Flag di Annullamento?
175400140523         TBLflg = wNewTBLflg;
175500140521
175600140523         // -?Riempimento struttura dati ds1P?
175700140523         clear  ds1P;
175800140526         ds1P.§1Pdes = V1Cdes;
175900140526         ds1P.§1Pfcp = V1Cfcp;
176000140526         ds1P.§1Puta = V1Cuta;
176100140526         ds1P.§1Puar = V1Cuar;
176200140526         ds1P.§1Pupr = V1Cupr;
176300140526         ds1P.§1Ptt0 = V1Ctt0;
176400140526         ds1P.§1Ptt1 = V1Ctt1;
176500140526         ds1P.§1Ptt2 = V1Ctt2;
176600140526         ds1P.§1Ptt3 = V1Ctt3;
176700140526         ds1P.§1Ptt4 = V1Ctt4;
176800140526         ds1P.§1Ptt5 = V1Ctt5;
176900140526         ds1P.§1Ptt6 = V1Ctt6;
177000140526         ds1P.§1Ptt7 = V1Ctt7;
177100140526         ds1P.§1Ptt8 = V1Ctt8;
177200140526         ds1P.§1Ptt9 = V1Ctt9;
177300140526         ds1P.§1Pcts = V1Ccts;
177400140526         ds1P.§1Pcte = V1Ccte;
177500140526         ds1P.§1Pptv = V1Cptv;
177600140526         ds1P.§1Pvas = V1Cvas;
177700140526         ds1P.§1Pvvm = V1Cvvm;
177800140526         ds1P.§1Pvca = V1Cvca;
177900140526         ds1P.§1Pvar = V1Cvar;
178000140526         ds1P.§1Pvmn = V1Cvmn;
178100140526         ds1P.§1Pvmx = V1Cvmx;
178200140526         ds1P.§1Pfie = V1Cfie;
178300140526         ds1P.§1Pain = V1Cain;
178400140526         ds1P.§1Pttv = V1Cttv;
178500140526         ds1P.§1Pasc = V1Casc;
178600140526         ds1P.§1Prcv = V1Crcv;
178700140526         ds1P.§1Pfpt = V1Cfpt;
178800140526         ds1P.§1Pfpc = V1Cfpc;
178900140526         ds1P.§1Ptco = V1Ctco;
179000140526         ds1P.§1Ptps = V1Ctps;
179100140526         ds1P.§1Post = V1Cost;
179200140526         ds1P.§1Ptst = V1Ctst;
179300140526         ds1P.§1Pffe = V1Cffe;
179400140526         ds1P.§1Pfdp = V1Cfdp;
179500140526         ds1P.§1Pdfe = V1Cdfe;
179600140526         ds1P.§1Pfqt = V1Cfqt;
179700140526         ds1P.§1Peli = %editc( G08inv : 'X' );
179800140526         ds1P.§1Pdrf = V1Cdrf;
179900140526         ds1P.§1Ppda = V1Cpda;
180000140526         ds1P.§1PmodTp = V1CmodTp;
180100140521
180200140523         TBLuni = ds1P;
180300140521
180400140521         TBLftt = W01ftt;
180500140521         TBLflt = W01flt;
180600140521         clear  TBLftr;
180700140521         clear  TBLdtr;
180800140521
180900140521
181000140521         // -?Aggiornamento / Inserimento?
181100140521         If  %found(TABEL00F);
181200140521           //____________
181300140521           update  TABEL;
181400140521           //¯¯¯¯¯¯¯¯¯¯¯¯
181500140521         Else;
181600140521           //____________
181700140521           write   TABEL;
181800140521           //¯¯¯¯¯¯¯¯¯¯¯¯
181900140521         EndIf;
182000140523
182100140523
182200140523         // -?Annullamento/Ripristino traduzioni in AZLIN00F?
182300140526         If  VS1opz = 4;
182400140523
182500140523           setGT  ( c_Kut )  AZLIN000;
182600140523           read  AZLIN000;
182700140523
182800140523           DoW  NOT %eof(AZLIN01L);
182900140523
183000140523             k_TBLkut = LINtabel;
183100140523             chain  %kds( keyTABEL00 )  TABEL;
183200140523             if  %found(TABEL00F);
183300140523               TBLflg = wNewTBLflg;
183400140523               update  TABEL;
183500140523             endif;
183600140523
183700140523             read  AZLIN000;
183800140523
183900140523           EndDo;
184000140523
184100140523         EndIf;
184200140521
184300140521       ENDSR;
184400140523
184500140523       //--------------------------------------------------------------
184600140523       //?Aggiornamento tabelle "1P" ed "SP" in Lingua.                ?
184700140523       //--------------------------------------------------------------
184800140523       BEGSR  sr_F06S02;
184900140523
185000140526         // -?Recupero dei dati delle tabelle ("1P"+"SP") in italiano?
185100140526         k_TBLkut = c_Kut;
185200140526         k_TBLkey = V2Ccod;
185300140526         k_TBLcod = c_Tab;
185400140526         clear SaveT1Puni;
185500140526         chain(N)  %kds(keyTABEL00)  TABEL;
185600140526         if  %found(TABEL00F);
185700140526           SaveT1Puni = TBLuni;
185800140526         endif;
185900140526         k_TBLcod = c_Tab2;
186000140526         clear SaveTSPuni;
186100140526         chain(N)  %kds(keyTABEL00)  TABEL;
186200140526         if  %found(TABEL00F);
186300140526           SaveTSPuni = TBLuni;
186400140526         endif;
186500140526
186600140526         // -?Lettura subfile?
186700140526         S02nrr = 1;
186800140526         chain  S02nrr  TB27S02;
186900140526
187000140526         DoW  %found(TRTB27D);
187100140526
187200140526           // -?Aggiornamento/scrittura dei dati in tab. "1P"?
187300140526           k_TBLkut = %int( S2HcdLin );
187400140526           k_TBLcod = c_Tab;
187500140526           chain  %kds(keyTABEL00)  TABEL;
187600140526           if  NOT %found(TABEL00F);
187700140526             clear  TABEL;
187800140526           endif;
187900140526           ds1P = SaveT1Puni;
188000140526           ds1P.§1Pdes = S2Cdes;
188100140526           TBLuni = ds1P;
188200140526           TBLftt = W01ftt;
188300140526           TBLflt = W01flt;
188400140526           clear  TBLftr;
188500140526           clear  TBLdtr;
188600140526           if  NOT %found(TABEL00F);
188700140526             // -?Scrittura?
188800140526             TBLkut = k_TBLkut;
188900140526             TBLcod = k_TBLcod;
189000140526             TBLkey = V2Ccod;
189100140526             write  TABEL;
189200140526           else;
189300140526             // -?Aggiornamento?
189400140526             update  TABEL;
189500140526           endif;
189600140526
189700140526           // -?Aggiornamento/scrittura dei dati in tab. "SP"?
189800140526           k_TBLkut = %int( S2HcdLin );
189900140526           k_TBLcod = c_Tab2;
190000140526           chain  %kds(keyTABEL00)  TABEL;
190100140526           if  NOT %found(TABEL00F);
190200140526             clear  TABEL;
190300140526           endif;
190400140526           dsSP = SaveTSPuni;
190500140526           dsSP.§SPdes = S2Cdes2;
190600140526           dsSP.§SPdfe = S2Cdfe2;
190700140526           TBLuni = dsSP;
190800140526           TBLftt = W01ftt;
190900140526           TBLflt = W01flt;
191000140526           clear  TBLftr;
191100140526           clear  TBLdtr;
191200140526           if  NOT %found(TABEL00F);
191300140526             // -?Scrittura?
191400140526             TBLkut = k_TBLkut;
191500140526             TBLcod = k_TBLcod;
191600140526             TBLkey = V2Ccod;
191700140526             write  TABEL;
191800140526           else;
191900140526             // -?Aggiornamento?
192000140526             update  TABEL;
192100140526           endif;
192200140526
192300140526           S02nrr += 1;
192400140526           chain  S02nrr  TB27S02;
192500140526
192600140526         EndDo;
192700140526
192800140526         // -?Ritorno al subfile S01?
192900140526         $InzS01 = *on;
193000140526         $Video  = 'S1';
193100140523
193200140523       ENDSR;
193300140523
193400140523       //--------------------------------------------------------------
193500140523       //?Aggiornamento tab. "SP" nei file TABEL00F e AZLIN01L.        ?
193600140523       //--------------------------------------------------------------
193700140523       BEGSR  sr_UpdTabSP;
193800140523
193900140523         // -?RI-aggancio record da aggiornare su TABEL00F?
194000140523         //  ?(tab. "SP")?
194100140523         k_TBLkut = c_Kut;
194200140523         k_TBLcod = c_Tab2;
194300140523         k_TBLkey = V2Ccod;
194400140523
194500140526         chain  %kds(keyTABEL00)  TABEL;
194600140523
194700140523         // -?Impostazione campi-chiave SE record NON trovato?
194800140523         if  Not %found(TABEL00F);
194900140523           clear  TABEL;
195000140523           TBLkut = k_TBLkut;
195100140523           TBLcod = k_TBLcod;
195200140523           TBLkey = k_TBLkey;
195300140523         endif;
195400140523
195500140523         // -?Impostazione Flag di Annullamento?
195600140526         TBLflg = wNewTBLflg;
195700140523
195800140523         // -?Riempimento struttura dati ds1P?
195900140523         clear  dsSP;
196000140523
196100140526         dsSP.§SPdes = V2Cdes;
196200140526         dsSP.§SPdac = V2Cdac;
196300140526         dsSP.§SPsud = V2Csud;
196400140526         dsSP.§SPdfe = V2Cdfe;
196500140526         dsSP.§SPaba = V2Caba;
196600140523
196700140523         TBLuni = dsSP;
196800140523
196900140523         TBLftt = W01ftt;
197000140523         TBLflt = W01flt;
197100140523         clear  TBLftr;
197200140523         clear  TBLdtr;
197300140523
197400140523
197500140523         // -?Aggiornamento / Inserimento?
197600140523         If  %found(TABEL00F);
197700140523           //____________
197800140523           update  TABEL;
197900140523           //¯¯¯¯¯¯¯¯¯¯¯¯
198000140523         Else;
198100140523           //____________
198200140523           write   TABEL;
198300140523           //¯¯¯¯¯¯¯¯¯¯¯¯
198400140523         EndIf;
198500140523
198600140523
198700140523         // -?Annullamento/Ripristino traduzioni in AZLIN00F?
198800140526         If  VS1opz = 4;
198900140523
199000140523           setGT  ( c_Kut )  AZLIN000;
199100140523           read  AZLIN000;
199200140523
199300140523           DoW  NOT %eof(AZLIN01L);
199400140523
199500140523             k_TBLkut = LINtabel;
199600140523             chain  %kds( keyTABEL00 )  TABEL;
199700140523             if  %found(TABEL00F);
199800140523               TBLflg = wNewTBLflg;
199900140523               update  TABEL;
200000140523             endif;
200100140523
200200140523             read  AZLIN000;
200300140523
200400140523           EndDo;
200500140523
200600140523         EndIf;
200700140526
200800140526       ENDSR;
200900140521
201000140521       //--------------------------------------------------------------
201100140521       //?Operazioni finali.                                           ?
201200140521       //--------------------------------------------------------------
201300140521       BEGSR  sr_RoutEnd;
201400140521
201500140521         // -?Chiusura applicazioni utilizzate?
201600140521         cloTabella();
201700140521
201800140521         // -?Restituzione parametri al chiamante?
201900140521         KPJBU = TRTB27ds;
202000140521
202100140521         // -?Uscita dal *pgm?
202200140521         return;
202300140521
202400140521       ENDSR;
202500140521
202600140521      /end-free
202700140521
202800140521       //--------------------------------------------------------------
202900140521       //?Definizione schiere a tempo di compilazione.                 ?
203000140521       //--------------------------------------------------------------
203100140521
203200140523** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
203300140521Superato il limite massimo dei record caricabili a video (9999)                1
203400140521Immettere un'opzione su questa pagina prima di premere F13                     2
203500140521Sono state immesse altre opzioni sotto questa: premere F13 per ripetere questa 3
203600140521Ripetizione NON eseguita: posizionare il cursore su un'opzione e ripremere F13 4
203700140521Opzione errata                                                                 5
203800140521Ammessa la selezione di una sola Tariffa Particolare                           6
203900140521Un codice annullato NON è modificabile: occorre ripristinarlo                  7
204000140523Immettere il codice                                                            8
204100140523Codice errato                                                                  9
204200140523Immettere la descrizione                                                      10
204300140523Tipo tariffa errato                                                           11
204400140523Codice tassazione errato                                                      12
204500140523Data errata                                                                   13
