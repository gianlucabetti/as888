000100140109       //==============================================================
000200140109       //?TNTBCMDR1 * Selezione Comando Telefonata da tab. "CMD"       ?
000300140109       //==============================================================
000400140109
000500140109       //--------------------------------------------------------------
000600140109       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140109       //--------------------------------------------------------------
000800140109
000900140109     /*PRM dbgview(*source)
001000140109     /*END
001100140109
001200140109       //--------------------------------------------------------------
001300140109       //?Specifiche di controllo.                                     ?
001400140109       //--------------------------------------------------------------
001500140109
001600140109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140109
001800140109       //--------------------------------------------------------------
001900140109       //?Dichiarazione file.                                          ?
002000140109       //--------------------------------------------------------------
002100140109
002200140109       // -?Tabella "CMD"?
002300140109     fTNTBE01L  if   e           k disk
002400140109
002500140109       // -?Video Gestione?
002600140109     fTNTBCMDD1 cf   e             workstn
002700140109     f                                     sfile(TBCMDS01 : S01nrr)
002800140109     f                                     indds(IndDspF)
002900140109     f                                     infds(InfDspF)
003000140109
003100140109       //--------------------------------------------------------------
003200140109       //?Definizione costanti.                                        ?
003300140109       //--------------------------------------------------------------
003400140109
003500140109       // -?Codice tabella in gestione?
003600140109     d c_Tab           c                   const('CMD')
003700140109
003800140109       // -?Numero di record in ciascuna videata di subfile?
003900140109     d c_SflPag        c                   const(13)
004000140109
004100140109       // -?Numero massimo di record gestibili?
004200140109     d c_MaxRec        c                   const(9999)
004300140109
004400140109       // -?Tasti funzionali a video?
004500140109     d c_F01           c                   const(x'31')
004600140109     d c_F02           c                   const(x'32')
004700140109     d c_F03           c                   const(x'33')
004800140109     d c_F04           c                   const(x'34')
004900140109     d c_F05           c                   const(x'35')
005000140109     d c_F06           c                   const(x'36')
005100140109     d c_F07           c                   const(x'37')
005200140109     d c_F08           c                   const(x'38')
005300140109     d c_F09           c                   const(x'39')
005400140109     d c_F10           c                   const(x'3A')
005500140109     d c_F11           c                   const(x'3B')
005600140109     d c_F12           c                   const(x'3C')
005700140109     d c_F13           c                   const(x'B1')
005800140109     d c_F14           c                   const(x'B2')
005900140109     d c_F15           c                   const(x'B3')
006000140109     d c_F16           c                   const(x'B4')
006100140109     d c_F17           c                   const(x'B5')
006200140109     d c_F18           c                   const(x'B6')
006300140109     d c_F19           c                   const(x'B7')
006400140109     d c_F20           c                   const(x'B8')
006500140109     d c_F21           c                   const(x'B9')
006600140109     d c_F22           c                   const(x'BA')
006700140109     d c_F23           c                   const(x'BB')
006800140109     d c_F24           c                   const(x'BC')
006900140109     d c_Enter         c                   const(x'F1')
007000140109     d c_RollDown      c                   const(x'F4')
007100140109     d c_RollUp        c                   const(x'F5')
007200140109
007300140109       //--------------------------------------------------------------
007400140109       //?Definizione schiere.                                         ?
007500140109       //--------------------------------------------------------------
007600140109
007700140109       // -?Messaggi di errore?
007800140109     d sk_Msg          s             78    dim( 3) ctdata perrcd(1)
007900140109
008000140109       //--------------------------------------------------------------
008100140109       //?Definizione aree dati.                                       ?
008200140109       //--------------------------------------------------------------
008300140109
008400140109       // -?Dati utente?
008500140109     d §AzUte        e ds                  extname(AZUTE00F)
008600140109     d                                     dtaara
008700140109     d §DatiUte      e ds                  extname(dDatiUte)
008800140109     d                                     dtaara
008900140109
009000140109       //--------------------------------------------------------------
009100140109       //?Definizione strutture dati.                                  ?
009200140109       //--------------------------------------------------------------
009300140109
009400140109       // -?Status ds?
009500140109     d Status         sds
009600140109     d   SDSpgm          *proc
009700140109     d*//SDSprm          *parms
009800140109     d*//SDSusr              254    263
009900140109
010000140109       // -?InfDS?
010100140109     d InfDspF         ds
010200140109     d   dsp_aid             369    369a
010300140109     d*//sfl_rrn             376    377i 0
010400140109     d*//min_nrr             378    379i 0
010500140109     d*//num_rcds            380    381i 0
010600140109
010700140109       // -?Indicatori su DspF?
010800140109     d IndDspF         ds                  inz
010900140109         // -?Abilitazione tasti funzionali?
011000140109     d   F8Attivo                      n   overlay(IndDspF : 08)
011100140109         // -?Emissione messaggio di errore?
011200140109     d   ErrMessage                    n   overlay(IndDspF : 28)
011300140109         // -?Indicatori di gestione del subfile?
011400140109     d   SflDsp_N                      n   overlay(IndDspF : 30)
011500140109     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011600140109     d   SflNxtChg                     n   overlay(IndDspF : 32)
011700140109     d   SflEnd                        n   overlay(IndDspF : 33)
011800140109         // -?Indicatori per Attributi di visualizzazione?
011900140109     d   DisabilitOpz                  n   overlay(IndDspF : 40)
012000140109     d   Ord_x_Des                     n   overlay(IndDspF : 41)
012100140109         // -?Posizionamento cursore & segnalazione errore?
012200140109     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012300140109         // -?Riemissione videata?
012400140109     d   ErrGenerico                   n   overlay(IndDspF : 99)
012500140109
012600140109       // -?Parametri ricevuti?
012700140109     d KPJBA         e ds
012800140109     d TNTBCMDds1    e ds                  inz
012900140109
013000140109       // -?Tabella "CMD" = Comandi Telefonata?
013100140109     d dCMD          e ds                  qualified      inz
013200140109
013300140109       //--------------------------------------------------------------
013400140109       //?Definizione variabili globali.                               ?
013500140109       //--------------------------------------------------------------
013600140109
013700140109       // -?Flags booleani?
013800140109     d $Fine           s               n   inz
013900140109     d $InzS01         s               n   inz(*on)
014000140109
014100140109       // -?Variabili per la gestione del video?
014200140109     d $Video          s              2    inz('S1')
014300140109     d S01nrr          s                   like(C1RcdNbr) inz
014400140109     d SavS01csr       s                   like(C1RcdNbr) inz
014500140109
014600140109       // -?Conteggio selezioni (opz. 1) inserite?
014700140109     d wMemoSel        s                   like(S1Copz)   inz
014800140109
014900140109       // -?Campi di comodo?
015000140109     d wDate           s              8s 0 inz
015100140109
015200140109       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
015300140109
015400140109       //--------------------------------------------------------------
015500140109       // -?Constants?
015600140109       //--------------------------------------------------------------
015700140109       // -?MaxKey - Maximum number of key fields allowed by this program?
015800140109     d c_MaxKey        c                   const(2)
015900140109       // -?Sort order:?
016000140109       //  ?1 = Sort in an ascending sequence?
016100140109       //  ?2 = Sort in a descending sequence?
016200140109     d c_Ascendente    c                   const(1)
016300140109     d c_Discendente   c                   const(2)
016400140109       // -?Key data type:?
016500140109       //  ? 0 = Signed binary?
016600140109       //  ? 2 = Signed zoned decimal?
016700140109       //  ? 3 = Signed packed decimal?
016800140109       //  ? 6 = Character with no national language sort sequence applied,?
016900140109       //  ?     if specified?
017000140109       //  ? 7 = Unsigned packed decimal?
017100140109       //  ?     All numbers will have the sign forced positive ('F'X).?
017200140109       //  ? 8 = Unsigned zoned decimal?
017300140109       //  ?     All numbers will have the sign forced positive ('F'X).?
017400140109       //  ? 9 = Unsigned binary?
017500140109       //  ?14 = Date in form of DD/MM/YY?
017600140109       //  ?15 = Date in form of DD.MM.YYYY?
017700140109     d c_Numero        c                   const(2)
017800140109     d c_Carattere     c                   const(6)
017900140109     d c_NumIntero     c                   const(8)
018000140109       //
018100140109     d c_Put           c                   const(1)
018200140109     d c_EndPut        c                   const(2)
018300140109     d c_Get           c                   const(3)
018400140109
018500140109       //--------------------------------------------------------------
018600140109       // -?Data Structures?
018700140109       //  ?SflRcd     - The entire subfile record to pass to the sort?
018800140109       //  ?QLGSCB     - The sort request block for the QLGSORT API?
018900140109       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
019000140109       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
019100140109       //  ?QUSEC      - Error structure for the QLGSORT API?
019200140109       //--------------------------------------------------------------
019300140109     d SflRcd          ds                  inz
019400140109     d   S1Ccmd                            inz
019500140109     d   S1Dcmd                            inz
019600140109     d   S1Ctip                            inz
019700140109     d   S1CutUt                           inz
019800140109     d   S1Copz                            inz
019900140109     d   Selected                     1a   inz
020000140109      /copy QSYSINC/QRPGLESRC,QLGSORT
020100140109     d QLGKL                         16    dim(c_MaxKey)
020200140109     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
020300140109     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
020400140109     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
020500140109     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
020600140109      /copy QSYSINC/QRPGLESRC,QLGSRTIO
020700140109      /copy QSYSINC/QRPGLESRC,QUSEC
020800140109
020900140109       //--------------------------------------------------------------
021000140109       // -?Standalone fields?
021100140109       //  ?Nrr        - Relative record number for subfile?
021200140109       //  ?SizeList   - Size of list?
021300140109       //  ?ReturnSize - Size of list returned by sort API?
021400140109       //--------------------------------------------------------------
021500140109     d NotUsed         s             16a   inz
021600140109     d ReturnSize      s              9b 0 inz
021700140109     d SizeList        s              9b 0 inz
021800140109     d RrnLast         s              5i 0 inz
021900140109     d Nrr             s              5i 0 inz
022000140109
022100140109       //--------------------------------------------------------------
022200140109       //?Definizione prototipi procedure usate.                       ?
022300140109       //--------------------------------------------------------------
022400140109
022500140109       // -?Reperimento dati utente?
022600140109     d TIBS34ds      e ds                  inz
022700140109      /copy gaitrasrc/srcProtoPR,TIBS34R
022800140109
022900140109       // -?Ordinamento subfile?
023000140109      /copy gaitrasrc/srcProtoPr,QLGSRTIO
023100140109
023200140109       //--------------------------------------------------------------
023300140109       //?Definizione key-list.                                        ?
023400140109       //--------------------------------------------------------------
023500140109
023600140109       // -?File TNTBE01L?
023700140109     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
023800140109     d                                     prefix(k_)   inz
023900140109
024000140109       //--------------------------------------------------------------
024100140109       //?Riepilogo indicatori utilizzati.                             ?
024200140109       //--------------------------------------------------------------
024300140109       //--------------------------------------------------------------
024400140109
024500140109       //--------------------------------------------------------------
024600140109       //?M A I N - L I N E                                            ?
024700140109       //--------------------------------------------------------------
024800140109
024900140109     c     *Entry        plist
025000140109     c                   parm                    KPJBA
025100140109
025200140109      /free
025300140109
025400140109       // -?Operazioni iniziali?
025500140109       exsr sr_RoutInz;
025600140109
025700140109       // -?Ciclo di gestione del file video?
025800140109       DoW  $Fine = *off;
025900140109
026000140109         select;
026100140109
026200140109           // -?Gestione videata S1 (subfile)?
026300140109           when $Video = 'S1';
026400140109             exsr sr_GesS01;
026500140109
026600140109           // -?? ? ??
026700140109           other;
026800140109             $Fine = *on;
026900140109
027000140109         endsl;
027100140109
027200140109       EndDo;
027300140109
027400140109       // -?Operazioni finali?
027500140109       exsr sr_RoutEnd;
027600140109
027700140109       //--------------------------------------------------------------
027800140109       //?Operazioni iniziali.                                         ?
027900140109       //--------------------------------------------------------------
028000140109       BEGSR  sr_RoutInz;
028100140109
028200140109         // -?Impostazione chiusura?
028300140109         *inLR = *on;
028400140109
028500140109         // -?Reperimento dati job?
028600140109         exsr  sr_DatiJob;
028700140109
028800140109         // -?Reperimento data odierna?
028900140109         wDate = %dec( %date() );
029000140109
029100140109         // -?Impostazione nome programma a video?
029200140109         //V1Tpgm = SDSpgm;
029300140109
029400140109         // -?Verifica parametri ricevuti?
029500140109         if  kpjbu <> *blank;
029600140109           tntbCMDds1 = kpjbu;
029700140109         else;
029800140109           clear  tntbCMDds1;
029900140109         endif;
030000140109
030100140109         // -?Pulizia parametri di output?
030200140109         clear  oCMDcmd;
030300140109         clear  oCMDdes;
030400140109         clear  oCMDfxx;
030500140109         clear  oCMDmsg;
030600140109         oCMDerr = *off;
030700140109
030800140109         // -?Impostazione videata iniziale?
030900140109         $Video = 'S1';
031000140109         reset  $InzS01;
031100140109         Ord_x_Des = *off;
031200140109         DisabilitOpz = *off;
031300140109
031400140109         // -?Impostazione iniziale / pulizia dei campi chiave?
031500140109         clear  keyTNTBE01;
031600140109         k_TBEcod = c_Tab;
031700140109
031800140109       ENDSR;
031900140109
032000140109       //--------------------------------------------------------------
032100140109       //?Reperimento Dati del job (Utente/Operativi).                 ?
032200140109       //--------------------------------------------------------------
032300140109       BEGSR  sr_DatiJob;
032400140109
032500140109         in(e) §AzUte;
032600140109         if NOT %error;
032700140109           in(e) §DatiUte;
032800140109         endif;
032900140109         if %error or RSut = *blank;
033000140109           tibs34r ( tibs34ds );
033100140109           in §AzUte;
033200140109           in §DatiUte;
033300140109         endif;
033400140109
033500140109       ENDSR;
033600140109
033700140109       //--------------------------------------------------------------
033800140109       //?Gestione subfile S01                                         ?
033900140109       //--------------------------------------------------------------
034000140109       BEGSR  sr_GesS01;
034100140109
034200140109         // -?Inizializzazione videata?
034300140109         if  $InzS01 = *on;
034400140109           exsr  sr_InzS01;
034500140109           $InzS01 = *off;
034600140109         endif;
034700140109
034800140109         // -?Emissione Piede con tasti funzionali abilitati (window)?
034900140109         write  TBCMDP01;
035000140109
035100140109         // -?Posizionamento cursore?
035200140109         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035300140109         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035400140109         //  ?si visualizza la pagina che vedeva l'utente quando ha?
035500140109         //  ?premuto l'ultimo tasto.?
035600140109         if  C1CsrRrn > *zero;
035700140109           C1RcdNbr = C1CsrRrn;
035800140109         else;
035900140109           C1RcdNbr = 1;
036000140109           write  TBCMDS00;       //?(no rec.)?
036100140109         endif;
036200140109
036300140109         // -?Emissione videata (testata compresa) in window?
036400140109         exfmt  TBCMDC01;
036500140109
036600140109         clear  V1Dmsg;
036700140109         reset  ErrMessage;
036800140109         reset  ErrGenerico;
036900140109
037000140109         SELECT;
037100140109
037200140109           // -?F3=Fine?
037300140109           WHEN  dsp_aid = c_F03;
037400140109             oCMDfxx = '03';
037500140109             $Fine   = *on;
037600140109
037700140109           // -?F8=Cambio ordinamento?
037800140109           WHEN  dsp_aid = c_F08;
037900140109             exsr  sr_F08S01;
038000140109
038100140109           // -?F12=Ritorno?
038200140109           WHEN  dsp_aid = c_F12;
038300140109             oCMDfxx = '12';
038400140109             $Fine   = *on;
038500140109
038600140109           // -?Subfile vuoto?
038700140109           WHEN  S01nrr = *zero;
038800140109
038900140109           // -?Invio?
039000140109           OTHER;
039100140109             // -?Gestione opzioni?
039200140109             exsr  sr_OpzS01;
039300140109             select;
039400140109               when  ErrGenerico;
039500140109                 leavesr;
039600140109               when  wMemoSel > *zero;
039700140109                 $Fine = *on;
039800140109                 leavesr;
039900140109             endsl;
040000140109
040100140109         ENDSL;
040200140109
040300140109       ENDSR;
040400140109
040500140109       //--------------------------------------------------------------
040600140109       //?Inizializzazione subfile S01                                 ?
040700140109       //--------------------------------------------------------------
040800140109       BEGSR  sr_InzS01;
040900140109
041000140109         // -?Spegnimento degli indicatori di errore?
041100140109         %subst(IndDspF : 50) = *off;
041200140109
041300140109         // -?Pulizia subfile?
041400140109         SflDsp_N    = *on;
041500140109         SflDspCtl_N = *on;
041600140109         write  TBCMDC01;
041700140109         SflDspCtl_N = *off;
041800140109         SflEnd      = *off;
041900140109
042000140109         clear  C1RcdNbr;
042100140109         clear  C1CsrRrn;
042200140109         clear  S01nrr;
042300140109         clear  V1Dmsg;
042400140109         ErrMessage  = *off;
042500140109         ErrGenerico = *off;
042600140109
042700140109         // -?Caricamento completo dei dati nel subfile (per codice)?
042800140109         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
042900140109         //  ?l'eventuale ordinamento)?
043000140109         clear  S01nrr;
043100140109         SflNxtChg = *off;
043200140109         clear  k_TBEke1;
043300140109         clear  k_TBEke2;
043400140109         clear  k_TBElin;
043500140109         clear  k_TBEsif;
043600140109         setll  %kds(keyTNTBE01)  TNTBE000;
043700140109         reade  %kds(keyTNTBE01 : 1)  TNTBE000;
043800140109
043900140109         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
044000140109           if  TBEatb = *blank;
044100140109             exsr  sr_WriteS01;
044200140109           endif;
044300140109           reade  %kds(keyTNTBE01 : 1)  TNTBE000;
044400140109         EndDo;
044500140109
044600140109         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
044700140109         RrnLast  = S01nrr;
044800140109
044900140109         // -?Visualizzazione del SFL (se ci sono dati)?
045000140109         SflDsp_N  = (S01nrr <= *zero);
045100140109
045200140109         // -?Attivazione del SFLEND?
045300140109         SflEnd = %eof(TNTBE01L);
045400140109
045500140109         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
045600140109         if  S01nrr > *zero;
045700140109           C1RcdNbr = 1;
045800140109         else;
045900140109           clear  C1RcdNbr;
046000140109         endif;
046100140109
046200140109         // -?Ordinamento subfile (se premuto F8)?
046300140109         if  Ord_x_Des  and  S01nrr > 1;
046400140109           exsr  sr_SortSfl;
046500140109         endif;
046600140109
046700140109         // -?(Dis)Abilitazione tasti funzionali?
046800140109         F8Attivo  = (S01nrr > *zero); // -?F8 = Ordinamento x ...?
046900140109
047000140109         // -?Emissione messaggio di segnalazione raggiungimento limite?
047100140109         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
047200140109           ErrGenerico = *on;
047300140109           ErrMessage  = *on;
047400140109           PosCurOPZ   = *on;
047500140109           V1Dmsg = sk_Msg(01);
047600140109           leavesr;
047700140109         endif;
047800140109
047900140109       ENDSR;
048000140109
048100140109       //--------------------------------------------------------------
048200140109       //?Registrazione del singolo rec. nel subfile S01               ?
048300140109       //--------------------------------------------------------------
048400140109       BEGSR  sr_WriteS01;
048500140109
048600140109         dCMD = TBEuni;
048700140109
048800140109         select;
048900140109           // -?Scarta i comandi in base alla tipologia?
049000140110           when  iCMDtip <> *blank  and  dCMD.§CMDtip <> iCMDtip;
049100140109             leavesr;
049200140109           // -?Scarta i comandi NON consentiti agli utenti (SE richiesto)?
049300140109           when  iCMDutUt = 'N'  and  dCMD.§CMDutUt = *blank;
049400140109             leavesr;
049500140109           // -?Scarta i comandi consentiti agli utenti (SE richiesto)?
049600140109           when  iCMDutUt = *blank  and  dCMD.§CMDutUt = 'N';
049700140109             leavesr;
049800140109         endsl;
049900140109
050000140109         // -?Impostazione campi nel record del subfile?
050100140109         clear  TBCMDS01;
050200140109         S1Ccmd  = %trimr( TBEke1 );
050300140109         S1Dcmd  = dCMD.§CMDdes;
050400140109         S1Ctip  = dCMD.§CMDtip;
050500140109         S1Cutut = dCMD.§CMDutUt;
050600140109
050700140109         // -?Registrazione singolo record nel subfile?
050800140109         S01nrr += 1;
050900140109         write  TBCMDS01;
051000140109
051100140109       ENDSR;
051200140109
051300140109       //--------------------------------------------------------------
051400140109       //?Gestione opzioni del subfile S01                             ?
051500140109       //--------------------------------------------------------------
051600140109       BEGSR  sr_OpzS01;
051700140109
051800140109         // -?Spegnimento degli indicatori di errore?
051900140109         %subst(IndDspF : 50) = *off;
052000140109
052100140109         clear  SavS01csr;
052200140109         clear  wMemoSel;
052300140109
052400140109         // -?Ciclo di lettura subfile?
052500140109         readc  TBCMDS01;
052600140109
052700140109         DoW  Not %eof(TNTBCMDD1);
052800140109
052900140109           SflNxtChg = *off;
053000140109           %subst(IndDspF : 50) = *off;
053100140109           C1RcdNbr = S01nrr;
053200140109
053300140109           select;
053400140109
053500140109             // -?Opz. 1=Selezione commerciale?
053600140109             when  S1Copz = 1;
053700140109               // -?Già selezionato un cliente?
053800140109               if  wMemoSel > *zero;
053900140109                 ErrGenerico = *on;
054000140109                 ErrMessage  = *on;
054100140109                 PosCurOPZ   = *on;
054200140109                 V1Dmsg = sk_Msg(03);
054300140109               else;
054400140109                 wMemoSel = S1Copz;
054500140109               endif;
054600140109
054700140109             // -?Nessuna opzione?
054800140109             when  S1Copz = *zero;
054900140109
055000140109             // -?Opzione errata?
055100140109             other;
055200140109               ErrGenerico = *on;
055300140109               ErrMessage  = *on;
055400140109               PosCurOPZ   = *on;
055500140109               V1Dmsg = sk_Msg(02);
055600140109
055700140109           endsl;
055800140109
055900140109           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
056000140109           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
056100140109           if  S1Copz   <> *zero  and
056200140109              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
056300140109             SavS01csr   = C1RcdNbr;
056400140109           endif;
056500140109
056600140109           // -?Aggiornamento sfl?
056700140109           select;
056800140109             when  ErrMessage  or  S1Copz <> *zero;
056900140109               SflNxtChg = *on;
057000140109               C1CsrRrn  = C1RcdNbr;
057100140109             when  ErrGenerico;
057200140109               C1CsrRrn  = C1RcdNbr;
057300140109               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
057400140109             other;
057500140109               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
057600140109           endsl;
057700140109
057800140109           UPDATE  TBCMDS01;
057900140109
058000140109           if  ErrGenerico   or   ErrMessage;
058100140109             leave;
058200140109           endif;
058300140109
058400140109           readc  TBCMDS01;
058500140109
058600140109         ENDDO;
058700140109
058800140109         // -?Riposizionam. cursore sul record contenente la prima opz.?
058900140109         //  ?(se non sono stati rilevati errori)?
059000140109         if  NOT ErrMessage   and   NOT ErrGenerico   and
059100140109             SavS01csr > *zero;
059200140109           C1CsrRrn = SavS01csr;
059300140109         endif;
059400140109
059500140109       ENDSR;
059600140109
059700140109       //--------------------------------------------------------------
059800140109       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
059900140109       //?F08-Cambia ordinamento sfl                                   ?
060000140109       //--------------------------------------------------------------
060100140109       BEGSR  sr_F08S01;
060200140109
060300140109         if  Not Ord_x_Des;
060400140109             Ord_x_Des = *on;
060500140109         else;
060600140109           Ord_x_Des = *off;
060700140109         endif;
060800140109
060900140109         // -?SubFile già completamente caricato: basta riordinarlo?
061000140109         exsr  sr_SortSfl;
061100140109
061200140109       ENDSR;
061300140109
061400140109       //--------------------------------------------------------------
061500140109       //?Ordinamento subfile                                          ?
061600140109       //?  This subroutine sorts the subfile records.                 ?
061700140109       //--------------------------------------------------------------
061800140109       BEGSR  sr_SortSfl;
061900140109
062000140109         // -?Impostazione NRR dell'ultimo record del subfile          ?
062100140109         //  ?(già fatto in sr_InzS01:                                 ?
062200140109         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
062300140109         //RrnLast  = S01nrr;
062400140109
062500140109         C1RcdNbr = 1;
062600140109         clear  C1CsrRrn;
062700140109         clear  S01nrr;
062800140109         clear  V1Dmsg;
062900140109         SflNxtChg   = *off;
063000140109         ErrMessage  = *off;
063100140109         ErrGenerico = *off;
063200140109         %subst(IndDspF : 50) = *off;
063300140109
063400140109         //?___________________________________________________________?
063500140109         //?Initialize the key fields to sort on.?
063600140109         //?There is one extra field not in the subfile -- Selected --?
063700140109         //?that is added to the record. This field is used to place?
063800140109         //?selected records in front of nonselected records.?
063900140109         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
064000140109
064100140109         clear  QLGKL;
064200140109         clear  QLGSKL;
064300140109         clear  QLGSCB;
064400140109         clear  QLGSCB00;
064500140109
064600140109         // -?Gestione della scelta ordinamento:?
064700140109         Select;
064800140109
064900140109           // -?Ordinamento per Codice Comando?
065000140109           when  Not Ord_x_Des;
065100140109             // -?1 campi chiave?
065200140109             QLGNBRK  = 1;
065300140109             // -?1° campo: codice comando (S1Ccmd)?
065400140109             //            ?a posizione  1,  1 byte, char, ascending.?
065500140109             QLGSP    = 1;
065600140109             QLGSS    = %size(S1Ccmd);
065700140109             QLGDT    = c_Carattere;
065800140109             QLGSO    = c_Ascendente;
065900140109             QLGKL(1) = QLGSKL;
066000140109
066100140109           // -?Ordinamento per Descrizione Comando?
066200140109           when  Ord_x_Des;
066300140109             // -?2 campi chiave?
066400140109             QLGNBRK  = 2;
066500140109             // -?1° campo: descr. comando (S1Dcmd)?
066600140109             //            ?a posizione 10, 20 byte, char, ascending.?
066700140109             QLGSP    = %size(S1Ccmd) + 1;
066800140109             QLGSS    = %size(S1Dcmd);
066900140109             QLGDT    = c_Carattere;
067000140109             QLGSO    = c_Ascendente;
067100140109             QLGKL(1) = QLGSKL;
067200140109             // -?2° campo: codice comando (S1Ccmd)?
067300140109             //            ?a posizone  1,  1 byte, char, ascending.?
067400140109             QLGSP    = 1;
067500140109             QLGSS    = %size(S1Ccmd);
067600140109             QLGDT    = c_Carattere;
067700140109             QLGSO    = c_Ascendente;
067800140109             QLGKL(2) = QLGSKL;
067900140109
068000140109         EndSl;
068100140109
068200140109         // -?Load other sort parameters.?
068300140109         QLGLB   = 80 + 16 * c_MaxKey;
068400140109         QLGRL   = %size(SflRcd) - 1;
068500140109         QLGRT   = 8;
068600140109         QLGOKL  = 80;
068700140109         QLGLKE  = 16;
068800140109         QLGLSS  = 290;
068900140109         // -?Initialize Sort I/O API fields.?
069000140109         QLGRL00 = QLGRL;
069100140109         QLGRC00 = 1;
069200140109         clear  QUSEI;
069300140109         QUSBPRV = %size(QUSEC);
069400140109
069500140109         // -?First step - Initialize the sort routine.?
069600140109         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
069700140109                      SizeList : ReturnSize : QUSEC );
069800140109
069900140109         // -?Next step - Write records to I/O routine.?
070000140109         QLGRT00  = c_Put;
070100140109         For  Nrr = 1  To  RrnLast;
070200140109           chain  Nrr  TBCMDS01;
070300140109           // -?Solo le righe con Selected = 'Y' sono riordinate,?
070400140109           //  ?quindi per fare un ordinamento di tutte le righe?
070500140109           //  ?metto 'Y' sempre.?
070600140109           Selected = 'Y';
070700140109           clear  QUSEI;
070800140109           QUSBPRV  = %size(QUSEC);
070900140109           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
071000140109                         SizeList : NotUsed : QUSEC );
071100140109         EndFor;
071200140109
071300140109         // -?Next step - Signal end of input, clear subfile for reload.?
071400140109         QLGRT00 = c_EndPut;
071500140109         clear  QUSEI;
071600140109         QUSBPRV = %size(QUSEC);
071700140109         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
071800140109                       SizeList : NotUsed : QUSEC );
071900140109
072000140109         // -?Pulizia subfile?
072100140109         SflDsp_N    = *on;
072200140109         SflDspCtl_N = *on;
072300140109         write  TBCMDC01;
072400140109         SflDspCtl_N = *off;
072500140109         SflEnd      = *off;
072600140109
072700140109         // -?Final step - Write the records back to the subfile.?
072800140109         QLGRT00  = c_Get;
072900140109         For  Nrr = 1  To RrnLast;
073000140109           clear  QUSEI;
073100140109           QUSBPRV = %size(QUSEC);
073200140109           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
073300140109                         QLGRL00  : NotUsed : QUSEC );
073400140109           // -?Ripristino indicatori utilizzati nel sfl rec?
073500140109           SflNxtChg = (S1Copz <> *zero);
073600140109           // -?e aggiorno?
073700140109           S01nrr = Nrr;
073800140109           write  TBCMDS01;
073900140109         EndFor;
074000140109
074100140109         C1CsrRrn = 1;
074200140109
074300140109         // -?Visualizzazione del SFL (se ci sono dati)?
074400140109         SflDsp_N = (S01nrr <= *zero);
074500140109
074600140109         // -?Attivazione del SFLEND?
074700140109         SflEnd = %eof(TNTBE01L);
074800140109
074900140109       ENDSR;
075000140109
075100140109       //--------------------------------------------------------------
075200140109       //?Operazioni finali.                                         ?
075300140109       //--------------------------------------------------------------
075400140109       BEGSR  sr_RoutEnd;
075500140109
075600140109         // -?Restituzione parametri?
075700140109         //  ?(se selezionato un motivo apertura R.A.)?
075800140109         If  wMemoSel > *zero;
075900140109
076000140109           chain  SavS01csr  TBCMDS01;
076100140109
076200140109           oCMDcmd  = S1Ccmd;
076300140109           oCMDdes  = S1Dcmd;
076400140109           oCMDtip  = S1Ctip;
076500140109           oCMDutut = S1CutUt;
076600140109
076700140109         EndIf;
076800140109
076900140109         kpjbu = tntbCMDds1;
077000140109
077100140109         // -?Chiusura *pgm?
077200140109         return;
077300140109
077400140109       ENDSR;
077500140109
077600140109      /end-free
077700140109
077800140109** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
077900140109Raggiunto il limite massimo dei record caricabili a video                      1
078000140109Opzione errata                                                                 2
078100140109Selezionati più di un Comando                                                  3
