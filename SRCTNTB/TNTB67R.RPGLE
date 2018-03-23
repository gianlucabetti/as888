000100111124       //==============================================================
000200111124       //?Gestione tabella "VPO-TELLUS" (variabili *pgm TELLUS)        ?
000300111124       //==============================================================
000400111124
000500111124       //--------------------------------------------------------------
000600111124       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700111124       //--------------------------------------------------------------
000800111124
000900111124     /*PRM dbgview(*source)
001000111124     /*END
001100111124
001200111124       //--------------------------------------------------------------
001300111124       //?Specifiche di controllo.                                     ?
001400111124       //--------------------------------------------------------------
001500111124
001600111124     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700111124     h dftactgrp(*no)
001800111124
001900111124       //--------------------------------------------------------------
002000111124       //?Dichiarazione file.                                          ?
002100111124       //--------------------------------------------------------------
002200111124
002300111124       // -?Tabella "VPO"?
002400111124     fTNTBE01L  Uf A e           k disk
002500111124
002600111124       // -?Video Gestione?
002700111124     fTNTB67D   cf   e             workstn
002800111124     f                                     indds(IndDspF)
002900111124     f                                     infds(InfDspF)
003000111124
003100111124       //--------------------------------------------------------------
003200111124       //?Definizione costanti.                                        ?
003300111124       //--------------------------------------------------------------
003400111124
003500111124       // -?Codice tabella in gestione?
003600111124     d c_Tab           c                   const('VPO')
003700111124
003800111124       // -?Costante per controllo "caratteri solo numerici"?
003900111124     d c_Digits        c                   const('0123456789')
004000111124
004100111124       // -?Tasti funzionali a video?
004200111124     d c_F01           c                   const(x'31')
004300111124     d c_F02           c                   const(x'32')
004400111124     d c_F03           c                   const(x'33')
004500111124     d c_F04           c                   const(x'34')
004600111124     d c_F05           c                   const(x'35')
004700111124     d c_F06           c                   const(x'36')
004800111124     d c_F07           c                   const(x'37')
004900111124     d c_F08           c                   const(x'38')
005000111124     d c_F09           c                   const(x'39')
005100111124     d c_F10           c                   const(x'3A')
005200111124     d c_F11           c                   const(x'3B')
005300111124     d c_F12           c                   const(x'3C')
005400111124     d c_F13           c                   const(x'B1')
005500111124     d c_F14           c                   const(x'B2')
005600111124     d c_F15           c                   const(x'B3')
005700111124     d c_F16           c                   const(x'B4')
005800111124     d c_F17           c                   const(x'B5')
005900111124     d c_F18           c                   const(x'B6')
006000111124     d c_F19           c                   const(x'B7')
006100111124     d c_F20           c                   const(x'B8')
006200111124     d c_F21           c                   const(x'B9')
006300111124     d c_F22           c                   const(x'BA')
006400111124     d c_F23           c                   const(x'BB')
006500111124     d c_F24           c                   const(x'BC')
006600111124     d c_Enter         c                   const(x'F1')
006700111124     d c_RollDown      c                   const(x'F4')
006800111124     d c_RollUp        c                   const(x'F5')
006900111124
007000111124       // -?Attributi di Visualizzazione?
007100111124     d c_DspAtrNorm    c                   const(x'A0')
007200111124     d c_DspAtrRI      c                   const(x'A1')
007300111124     d c_DspAtrRED     c                   const(x'A8')
007400111124     d c_DspAtrRI_RED  c                   const(x'A9')
007500111124
007600111124       //--------------------------------------------------------------
007700111124       //?Definizione schiere.                                         ?
007800111124       //--------------------------------------------------------------
007900111124
008000111124       // -?Messaggi di errore?
008100111124     d sk_Msg          s             78    dim( 4)  ctdata  perrcd(01)
008200111124
008300111124       //--------------------------------------------------------------
008400111124       //?Definizione aree dati.                                       ?
008500111124       //--------------------------------------------------------------
008600111124
008700111124       // -?Dati utente?
008800111124     d §AzUte        e ds                  extname(AZUTE00F)
008900111124     d                                     dtaara
009000111124     d §DatiUte      e ds                  extname(dDatiUte)
009100111124     d                                     dtaara
009200111124
009300111124       //--------------------------------------------------------------
009400111124       //?Definizione strutture dati.                                  ?
009500111124       //--------------------------------------------------------------
009600111124
009700111124       // -?Status ds?
009800111124     d Status         sds
009900111124     d  SDSpgm           *proc
010000111124
010100111124       // -?InfDS?
010200111124     d InfDspF         ds
010300111124     d   dsp_aid             369    369a
010400111124
010500111124       // -?Indicatori su DspF?
010600111124     d IndDspF         ds                  inz
010700111124         // -?Abilitazione tasti funzionali?
010800111124     d   F3Attivo                      n   overlay(IndDspF : 03)
010900111124     d   F5Attivo                      n   overlay(IndDspF : 05)
011000111124     d   F6Attivo                      n   overlay(IndDspF : 06)
011100111124     d   F16Attivo                     n   overlay(IndDspF : 16)
011200111124         // -?Emissione messaggio di errore?
011300111124     d   ErrMessage                    n   overlay(IndDspF : 28)
011400111124         // -?Indicatori per Attibuti di visualizzazione?
011500111124         // -?Posizionamento cursore & Segnalazione errore?
011600111124     d   PosCurCod                     n   overlay(IndDspF : 50)
011700111124     d   PosCurBOTLLS                  n   overlay(IndDspF : 51)
011800111124     d   PosCurBOMAX                   n   overlay(IndDspF : 52)
011900111124     d   PosCurATJOB                   n   overlay(IndDspF : 53)
012000111124     d   PosCurATBE1                   n   overlay(IndDspF : 54)
012100111124     d   PosCurATBE2                   n   overlay(IndDspF : 55)
012200111124     d   PosCurATAF1                   n   overlay(IndDspF : 56)
012300111124     d   PosCurATAF2                   n   overlay(IndDspF : 57)
012400111124         // -?Riemissione videata?
012500111124     d   ErrGenerico                   n   overlay(IndDspF : 99)
012600111124
012700111124       // -?Parametri ricevuti?
012800111124     d KPJBA         e ds
012900111124
013000111124       // -?Dati record principale della tabella?
013100111124     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013200111124     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013300111124     d                                          prefix(TBX:3)
013400111124
013500111124       // -?Tabella in manutenzione?
013600111124     d dVPOtellus    e ds                  inz
013700111124
013800111124       //--------------------------------------------------------------
013900111124       //?Definizione variabili globali.                               ?
014000111124       //--------------------------------------------------------------
014100111124
014200111124       // -?Flags booleani?
014300111124     d $Fine           s               n   inz
014400111124     d $InzD01         s               n   inz(*on)
014500111124     d $InzD02         s               n   inz(*on)
014600111124     d $InzW01         s               n   inz(*on)
014700111124     d $Annullamento   s               n   inz
014800111124     d $Immissione     s               n   inz
014900111124     d $Ripristino     s               n   inz
015000111124     d $ErrBoMax       s               n   inz
015100111124     d $ErrAtJob       s               n   inz
015200111124
015300111124       // -?Variabili per la gestione del video?
015400111124     d $Video          s              2    inz('D2')
015500111124     d $VideoPrec      s                   like($Video)  inz
015600111124
015700111124       // -?Campi di comodo?
015800111124     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015900111124
016000111124       //--------------------------------------------------------------
016100111124       //?Definizione prototipi procedure.                             ?
016200111124       //--------------------------------------------------------------
016300111124
016400111124       // -?Reperimento dati utente?
016500111124     d TIBS34ds      e ds                  inz
016600111124      /copy gaitrasrc/srcProtoPR,TIBS34R
016700111124
016800111124       //--------------------------------------------------------------
016900111124       //?Definizione key-list.                                        ?
017000111124       //--------------------------------------------------------------
017100111124
017200111124       // -?File TNTBE01L?
017300111124     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017400111124     d                                     prefix(k_)   inz
017500111124
017600111124       //--------------------------------------------------------------
017700111124       //?Riepilogo indicatori utilizzati.                             ?
017800111124       //--------------------------------------------------------------
017900111124       //--------------------------------------------------------------
018000111124
018100111124       //--------------------------------------------------------------
018200111124       //?M A I N - L I N E                                            ?
018300111124       //--------------------------------------------------------------
018400111124
018500111124     c     *Entry        plist
018600111124     c                   parm                    KPJBA
018700111124
018800111124      /free
018900111124
019000111124       // -?Operazioni iniziali?
019100111124       exsr sr_RoutInz;
019200111124
019300111124       // -?Gestione videate?
019400111124       DOW  $Fine = *off;
019500111124
019600111124         select;
019700111124
019800111124           // -?Gestione videata D1?
019900111124           //  ?(richiesta chiave tabella - fissa "TELLUS")?
020000111124           //when $Video = 'D1';
020100111124           //  exsr sr_GesD01;
020200111124
020300111124           // -?Gestione videata D2?
020400111124           //  ?(manutenzione dati tabella "VPO/TELLUS")?
020500111124           when $Video = 'D2';
020600111124             exsr sr_GesD02;
020700111124
020800111124           // -?Gestione videata W1?
020900111124           //  ?(richiesta dati per trasmissione record)?
021000111124           when $Video = 'W1';
021100111124             exsr sr_GesW01;
021200111124
021300111124           // -?Gestione videata ????
021400111124           other;
021500111124             $Fine = *on;
021600111124
021700111124         endsl;
021800111124
021900111124       ENDDO;
022000111124
022100111124       // -?Operazioni finali?
022200111124       exsr sr_RoutEnd;
022300111124
022400111124       //--------------------------------------------------------------
022500111124       //?Operazioni iniziali.                                         ?
022600111124       //--------------------------------------------------------------
022700111124       BEGSR  sr_RoutInz;
022800111124
022900111124         *inLR = *on;
023000111124
023100111124         // -?Reperimento dati job?
023200111124         exsr  sr_DatiJob;
023300111124
023400111124         // -?Impostazione nome programma a video?
023500111124         V1Tpgm = SDSpgm;
023600111124
023700111124         // -?Aggancio dati generali della tabella in esame?
023800111124         clear  k_TBEcod;
023900111124         k_TBEke1 = *zero;
024000111124         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
024100111124                = c_Tab;
024200111124         clear  k_TBEke2;
024300111124         clear  k_TBElin;
024400111124         k_TBEsif = KNSIF;
024500111124         chain(n)  %kds(k05tntbe01)  TNTBE000;
024600111124         if  not  %found(TNTBE01L);
024700111124           clear  k_TBEsif;
024800111124           chain(n)  %kds(k05tntbe01)  TNTBE000;
024900111124         endif;
025000111124         if  %found(TNTBE01L);
025100111124           xTNTBEds = TNTBEds;
025200111124         else;
025300111124           clear  xTNTBEds;
025400111124         endif;
025500111124
025600111124         // -?Impostazione iniziale / pulizia dei campi chiave?
025700111124         k_TBEcod = c_Tab;
025800111124         k_TBEke1 = 'TELLUS';
025900111124         clear  k_TBEke2;
026000111124         clear  k_TBElin;
026100111124         clear  k_TBEsif;
026200111124
026300111124         // -?Inizializzazione 1ª videata - NON emessa!!!?
026400111124         exsr  sr_InzD01;
026500111124
026600111124       ENDSR;
026700111124
026800111124       //--------------------------------------------------------------
026900111124       //?Reperimento Dati del job (Utente/Operativi).                 ?
027000111124       //--------------------------------------------------------------
027100111124       BEGSR  sr_DatiJob;
027200111124
027300111124         in(E) §AzUte;
027400111124         if NOT %error;
027500111124           in(E) §DatiUte;
027600111124         endif;
027700111124         if %error or RSut = *blanks;
027800111124           clear TIBS34ds;
027900111124           tibs34r ( tibs34ds );
028000111124           in §AzUte;
028100111124           in §DatiUte;
028200111124         endif;
028300111124
028400111124       ENDSR;
028500111124
028600111124       //--------------------------------------------------------------
028700111124       //?Gestione videata D01                                         ?
028800111124       //--------------------------------------------------------------
028900111124       BEGSR  sr_GesD01;
029000111124
029100111124         // -?Inizializzazione videata?
029200111124         if  $InzD01 = *on;
029300111124           exsr  sr_InzD01;
029400111124           $InzD01 = *off;
029500111124         endif;
029600111124
029700111124         // -?Emissione Testata & Piede?
029800111124         write  TB67T01;
029900111124         write  TB67P01;
030000111124
030100111124         // -?Emissione videata?
030200111124         exfmt  TB67D01;
030300111124
030400111124         reset  ErrMessage;
030500111124         reset  ErrGenerico;
030600111124         clear  V1Dmsg;
030700111124
030800111124         SELECT;
030900111124
031000111124           // -?F3=Fine?
031100111124           WHEN  dsp_aid = c_F03;
031200111124             $Fine = *on;
031300111124
031400111124           // -?Invio?
031500111124           OTHER;
031600111124             exsr sr_CtrD01;
031700111124             if  ErrGenerico = *on;
031800111124               leavesr;
031900111124             endif;
032000111124             $Video    = 'D2';
032100111124             $InzD02   = *on;
032200111124
032300111124         ENDSL;
032400111124
032500111124       ENDSR;
032600111124
032700111124       //--------------------------------------------------------------
032800111124       //?Inizializzazione videata D01                                 ?
032900111124       //--------------------------------------------------------------
033000111124       BEGSR  sr_InzD01;
033100111124
033200111124         clear  TB67D01;
033300111124
033400111124         V1Ccod = 'TELLUS';
033500111124
033600111124       ENDSR;
033700111124
033800111124       //--------------------------------------------------------------
033900111124       //?Controllo dati immessi nella videata D01                     ?
034000111124       //--------------------------------------------------------------
034100111124       BEGSR  sr_CtrD01;
034200111124
034300111124         %subst(IndDspF : 28) = *off;
034400111124
034500111124       ENDSR;
034600111124
034700111124       //--------------------------------------------------------------
034800111124       //?Gestione videata D02                                         ?
034900111124       //--------------------------------------------------------------
035000111124       BEGSR  sr_GesD02;
035100111124
035200111124         // -?Inizializzazione videata?
035300111124         if  $InzD02 = *on;
035400111124           exsr sr_InzD02;
035500111124           $InzD02 = *off;
035600111124         endif;
035700111124
035800111124         // -?Emissione Testata e Piede con tasti funzionali abilitati?
035900111124         if  errGenerico = *off;
036000111124           write  TB67T01;
036100111124           write  TB67D01;
036200111124           write  Protect;
036300111124           write  TB67P02;
036400111124         endif;
036500111124
036600111124         // -?Emissione videata?
036700111124         exfmt  TB67D02;
036800111124         clear  V1Dmsg;
036900111124         reset  ErrMessage;
037000111124         reset  ErrGenerico;
037100111124         reset  $Annullamento;
037200111124
037300111124         SELECT;
037400111124
037500111124           // -?F3=Fine?
037600111124           WHEN  dsp_aid = c_F03;
037700111124             unlock TNTBE01L;
037800111124             $Fine = *on;
037900111124
038000111124           // -?F12=Ritorno?
038100111124           WHEN  dsp_aid = c_F12;
038200111124             unlock TNTBE01L;
038300111124             $Video = 'D1';
038400111124             clear  V1Topz;
038500111124
038600111124           // -?Invio;?
038700111124           //  ?F5=Ripristino; F6=Conferma; F16=Annullamento?
038800111124           OTHER;
038900111124             If  dsp_aid <> c_F16;
039000111124               exsr  sr_CtrD02;
039100111124             EndIf;
039200111124             Select;
039300111124               // -?Errore?
039400111124               When  ErrGenerico;
039500111124                 leavesr;
039600111124               // -?F5=Ripristino; F6=Conferma; F16=Annullamento?
039700111124               When  dsp_aid = c_F05  or
039800111124                     dsp_aid = c_F06  or
039900111124                     dsp_aid = c_F16;
040000111124                 $Annullamento = (dsp_aid = c_F16);
040100111124                 $VideoPrec = $Video;
040200111124                 $Video = 'W1';
040300111124                 reset  $InzW01;
040400111124             EndSl;
040500111124
040600111124         ENDSL;
040700111124
040800111124       ENDSR;
040900111124
041000111124       //--------------------------------------------------------------
041100111124       //?Inizializzazione videata D02                                 ?
041200111124       //--------------------------------------------------------------
041300111124       BEGSR  sr_InzD02;
041400111124
041500111124         // -?Spegnimento di tutti gli indicatori?
041600111124         IndDspF = *off;
041700111124
041800111124         // -?Pulizia videata?
041900111124         clear  TB67D02;
042000111124
042100111124         // -?Reperimento dati da caricare a video?
042200111124         chain  %kds(k05tntbe01)  TNTBE000;
042300111124
042400111124         // -?Impostazione tipo manutenzione?
042500111124         $Immissione = (Not %found(TNTBE01L));
042600111124         $Ripristino = (%found(TNTBE01L)  and  TBEatb <> *blank);
042700111124         reset  $Annullamento;
042800111124
042900111124         select;
043000111124           when  $Immissione;
043100111124             V1Topz = 'IMMISSIONE';
043200111124           when  $Ripristino;
043300111124             V1Topz = 'RIPRISTINO';
043400111124           other;
043500111124             V1Topz = ' MODIFICA ';
043600111124         endsl;
043700111124
043800111124         // -?Caricamento dati a video?
043900111124         If  $Immissione;
044000111124           clear  dVPOtellus;
044100111124         Else;
044200111124           dVPOtellus = TBEuni;
044300111124           V2CboTlls = §VPOboTlls;
044400111124           V2CboMax  = §VPOboMax;
044500111124           V2CatJob  = §VPOatJob;
044600111124           V2CatBe1  = §VPOatBe1;
044700111124           V2CatBe2  = §VPOatBe2;
044800111124           V2CatAf1  = §VPOatAf1;
044900111124           V2CatAf2  = §VPOatAf2;
044901150304           V2CUtTlls = §VPOUtTlls;
045000111124         EndIf;
045100111124
045200111124         // -?Visualizzazione *msg di avvertimento della trasmissione?
045300111124         //  ?della tabella?
045400111124         if  TBXftt = 'S';
045500111124           V2Dnota = ' Ricordarsi di trasmettere la tabella. ';
045600111124           §da01  = c_DspAtrRI_RED;
045700111124         else;
045800111124           §da01  = c_DspAtrNorm;
045900111124         endif;
046000111124
046100111124         // -?Abilitazione tasti funzionali?
046200111124         exsr  sr_AbilFxxD02;
046300111124
046400111124       ENDSR;
046500111124
046600111124       //--------------------------------------------------------------
046700111124       //?Abilitazione tasti funzionali in P02 (per D02).              ?
046800111124       //--------------------------------------------------------------
046900111124       BEGSR  sr_AbilFxxD02;
047000111124
047100111124         // -?F5=Ripristino?
047200111124         F5Attivo  = (NOT $Immissione  and  $Ripristino);
047300111124
047400111124         // -?F6=Conferma?
047500111124         F6Attivo  = (NOT F5Attivo);
047600111124
047700111124         // -?F16=Annullamento?
047800111124         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
047900111124
048000111124       ENDSR;
048100111124
048200111124       //--------------------------------------------------------------
048300111124       //?Controllo dati immessi nella videata D02                     ?
048400111124       //--------------------------------------------------------------
048500111124       BEGSR  sr_CtrD02;
048600111124
048700111124         %subst(IndDspF : 50) = *off;
048800111124
048900111124         // -?Numero minimo  di bolle da passare a Tellus?
049000111124         if  V2CboTlls < *zero;
049100111124           ErrGenerico  = *on;
049200111124           ErrMessage   = *on;
049300111124           PosCurBOTLLS = *on;
049400111124           V1Dmsg = sk_Msg(01);
049500111124           leavesr;
049600111124         endif;
049700111124
049800111124         // -?Numero massimo di bolle per invio a Tellus?
049900111124         select;
050000111124           when  V2CboMax < *zero;
050100111124             ErrGenerico  = *on;
050200111124             ErrMessage   = *on;
050300111124             PosCurBOMAX  = *on;
050400111124             V1Dmsg = sk_Msg(01);
050500111124             leavesr;
050600111124           when  V2CboMax < 500  and  Not $ErrBoMax;
050700111124             $ErrBoMax = *on;
050800111124             ErrGenerico  = *on;
050900111124             ErrMessage   = *on;
051000111124             PosCurBOMAX  = *on;
051100111124             V1Dmsg = sk_Msg(02);
051200111124             leavesr;
051300111124           when  V2CboMax >= 500  and  $ErrBoMax;
051400111124             $ErrBoMax = *off;
051500111124         endsl;
051600111124
051700111124         // -?Numero massimo di lavori in attesa di elaborazione?
051800111124         select;
051900111124           when  V2CatJob < *zero;
052000111124             ErrGenerico  = *on;
052100111124             ErrMessage   = *on;
052200111124             PosCurATJOB  = *on;
052300111124             V1Dmsg = sk_Msg(01);
052400111124             leavesr;
052500111124           when  (V2CatJob < 1  or  V2CatJob > 30)  and  Not $ErrAtJob;
052600111124             $ErrAtJob = *on;
052700111124             ErrGenerico  = *on;
052800111124             ErrMessage   = *on;
052900111124             PosCurBOMAX  = *on;
053000111124             V1Dmsg = sk_Msg(03);
053100111124             leavesr;
053200111124           when  V2CatJob >= 1  and  V2CatJob <= 30  and  $ErrAtJob;
053300111124             $ErrAtJob = *off;
053400111124         endsl;
053500111124
053600111124         // -?Secondi di attesa PRIMA di elaborare 1° liv.?
053700111124         select;
053800111124           when  V2CatBe1 < *zero;
053900111124             ErrGenerico  = *on;
054000111124             ErrMessage   = *on;
054100111124             PosCurATBE1  = *on;
054200111124             V1Dmsg = sk_Msg(01);
054300111124             leavesr;
054400111124           when  V2CatBe1 = *zero;
054500111124             ErrGenerico  = *on;
054600111124             ErrMessage   = *on;
054700111124             PosCurATBE1  = *on;
054800111124             V1Dmsg = sk_Msg(04);
054900111124             leavesr;
055000111124         endsl;
055100111124
055200111124         // -?Secondi di attesa PRIMA di elaborare 2° liv.?
055300111124         select;
055400111124           when  V2CatBe2 < *zero;
055500111124             ErrGenerico  = *on;
055600111124             ErrMessage   = *on;
055700111124             PosCurATBE2  = *on;
055800111124             V1Dmsg = sk_Msg(01);
055900111124             leavesr;
056000111124           when  V2CatBe2 = *zero;
056100111124             ErrGenerico  = *on;
056200111124             ErrMessage   = *on;
056300111124             PosCurATBE1  = *on;
056400111124             V1Dmsg = sk_Msg(04);
056500111124             leavesr;
056600111124         endsl;
056700111124
056800111124         // -?Secondi di attesa SE troppi lavori in coda 1° liv.?
056900111124         select;
057000111124           when  V2CatAf1 < *zero;
057100111124             ErrGenerico  = *on;
057200111124             ErrMessage   = *on;
057300111124             PosCurATAF1  = *on;
057400111124             V1Dmsg = sk_Msg(01);
057500111124             leavesr;
057600111124           when  V2CatAf1 = *zero;
057700111124             ErrGenerico  = *on;
057800111124             ErrMessage   = *on;
057900111124             PosCurATAF1  = *on;
058000111124             V1Dmsg = sk_Msg(04);
058100111124             leavesr;
058200111124         endsl;
058300111124
058400111124       ENDSR;
058500111124
058600111124       //--------------------------------------------------------------
058700111124       //?Gestione trasmissione aggiornamento                          ?
058800111124       //--------------------------------------------------------------
058900111124       BEGSR  sr_GesW01;
059000111124
059100111124         // -?Inizializzazione videata?
059200111124         if $InzW01 = *on;
059300111124           exsr  sr_InzW01;
059400111124           $InzW01 = *off;
059500111124         endif;
059600111124
059700111124         // -?Emissione window?
059800111124         if  TBXftt = 'S';
059900111124           exfmt  TB67W01;
060000111124           ErrGenerico = *off;
060100111124           ErrMessage  = *off;
060200111124           clear  V1Dmsg;
060300111124           Select;
060400111124             // -?F12=Ritorno (gestito solo se emesso W01)?
060500111124             When  dsp_aid = c_F12;
060600111124               $Video = $VideoPrec;
060700111124               leavesr;
060800111124             // -?Invio (gestito solo se emesso W01)?
060900111124             When  dsp_aid <> c_F06;
061000111124               leavesr;
061100111124           EndSl;
061200111124         endif;
061300111124
061400111124         // -?Aggiornamento TNTBE00F?
061500111124         exsr  sr_Upd_TNTBE;
061600111124         select;
061700111124           // ·?Rilevato errore in fase di aggiornamento?
061800111124           when  ErrGenerico;
061900111124             leavesr;
062000111124           // ·?Chiusura *pgm altrimenti?
062100111124           other;
062200111124             //clear  V1Topz;
062300111124             //reset  $Video;
062400111124             //$InzD01 = *on;
062500111124             $Fine = *on;
062600111124         endsl;
062700111124
062800111124       ENDSR;
062900111124
063000111124       //--------------------------------------------------------------
063100111124       //?Inizializzazione videata W01                                 ?
063200111124       //--------------------------------------------------------------
063300111124       BEGSR  sr_InzW01;
063400111124
063500111124         clear TB67W01;
063600111124
063700111124         if  $Immissione;
063800111124
063900111124           // -?Se immissione?
064000111124           W1ftt  = TBXftt;
064100111124
064200111124         else;
064300111124
064400111124           // -?Se NON immissione:?
064500111124           //  ?visualizza i dati relativi all'ultima trasmissione?
064600111124           W1ftt  = TBEftt;
064700111124           W1flt  = TBEflt;
064800111124           W1ftr  = TBEftr;
064900111124           if TBEdtr <> *zero;
065000111124             wDate_EUR = %date(TBEdtr : *iso);
065100111124             W1dtr     = %dec(wDate_EUR);
065200111124           endif;
065300111124
065400111124         endif;
065500111124
065600111124       ENDSR;
065700111124
065800111124       //--------------------------------------------------------------
065900111124       //?Aggiornamento record TNTBE00F (tab. "KB4")                   ?
066000111124       //--------------------------------------------------------------
066100111124       BEGSR  sr_Upd_TNTBE;
066200111124
066300111124         // -?Impostazione campi della struttura dati?
066400111124         if  Not  $Annullamento;
066500111124           clear  dVPOtellus;
066600111124           §VPOboTlls = V2CboTlls;
066700111124           §VPOboMax  = V2CboMax;
066800111124           §VPOatJob  = V2CatJob;
066900111124           §VPOatBe1  = V2CatBe1;
067000111124           §VPOatBe2  = V2CatBe2;
067100111124           §VPOatAf1  = V2CatAf1;
067200111124           §VPOatAf2  = V2CatAf2;
067201150304           §VPOUtTlls = V2CUtTlls;
067300111124         endif;
067400111124
067500111124         // -?Impostazione campi del record di TNTBE00F?
067600111124         If  $Immissione;
067700111124           clear TNTBE000;
067800111124           TBEcod  = k_TBEcod;
067900111124           TBEke1  = k_TBEke1;
068000111124           TBEke2  = k_TBEke2;
068100111124           TBElin  = k_TBElin;
068200111124           TBEsif  = k_TBEsif;
068300111124         EndIf;
068400111124
068500111124         TBEapl = TBXapl;
068600111124         if  $Annullamento;
068700111124           TBEatb = 'A';
068800111124         else;
068900111124           clear  TBEatb;
069000111124         endif;
069100111124         TBEftt = W1ftt;
069200111124         TBEFlt = W1flt;
069300111124         clear  TBEftr;
069400111124         //clear TBEdtr;
069500111124
069600111124         TBEuni = dVPOtellus;
069700111124
069800111124         // ·?Aggiornamento record?
069900111124         if  $Immissione;
070000111124           //_____________
070100111124           WRITE  TNTBE000;
070200111124           //¯¯¯¯¯¯¯¯¯¯¯¯¯
070300111124         else;
070400111124           //_____________
070500111124           UPDATE TNTBE000;
070600111124           //¯¯¯¯¯¯¯¯¯¯¯¯¯
070700111124         endif;
070800111124
070900111124       ENDSR;
071000111124
071100111124       //--------------------------------------------------------------
071200111124       //?Operazioni finali.                                           ?
071300111124       //--------------------------------------------------------------
071400111124       BEGSR  sr_RoutEnd;
071500111124
071600111124         // -?Uscita dal *pgm?
071700111124         return;
071800111124
071900111124       ENDSR;
072000111124
072100111124      /end-free
072200111124
072300111124       //--------------------------------------------------------------
072400111124       //?Definizione schiere a tempo di compilazione                  ?
072500111124       //--------------------------------------------------------------
072600111124
072700111124** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
072800111124Non ammessi numeri negativi                                                    01
072900111124Numero inferiore al limite previsto (500) - premere "Invio" per continuare     02
073000111124Numero esterno al range previsto (1-30) - premere "Invio" per continuare       03
073100111124Inserire il numero di secondi di attesa                                        04
