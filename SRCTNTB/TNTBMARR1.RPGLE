000100131017       //==============================================================
000200131017       //?TNTBMARR1 * Selezione Motivo Apertura Richiesta Assistenza   ?
000300131017       //?            da tab. "MAR"                                    ?
000400131017       //==============================================================
000500131017
000600131017       //--------------------------------------------------------------
000700131017       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800131017       //--------------------------------------------------------------
000900131017
001000131017     /*PRM dbgview(*source)
001100131017     /*END
001200131017
001300131017       //--------------------------------------------------------------
001400131017       //?Specifiche di controllo.                                     ?
001500131017       //--------------------------------------------------------------
001600131017
001700131017     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800131017
001900131017       //--------------------------------------------------------------
002000131017       //?Dichiarazione file.                                          ?
002100131017       //--------------------------------------------------------------
002200131017
002300131017       // -?Tabella "MAR"?
002400131017     fTNTBE01L  if   e           k disk
002500131017
002600131017       // -?Video Gestione?
002700131017     fTNTBMARD1 cf   e             workstn
002800131017     f                                     sfile(TBMARS01 : S01nrr)
002900131017     f                                     indds(IndDspF)
003000131017     f                                     infds(InfDspF)
003100131017
003200131017       //--------------------------------------------------------------
003300131017       //?Definizione costanti.                                        ?
003400131017       //--------------------------------------------------------------
003500131017
003600131017       // -?Codice tabella in gestione?
003700131017     d c_Tab           c                   const('MAR')
003800131017
003900131017       // -?Numero di record in ciascuna videata di subfile?
004000131119     d c_SflPag        c                   const(13)
004100131017
004200131017       // -?Numero massimo di record gestibili?
004300131017     d c_MaxRec        c                   const(9999)
004400131017
004500131017       // -?Tasti funzionali a video?
004600131017     d c_F01           c                   const(x'31')
004700131017     d c_F02           c                   const(x'32')
004800131017     d c_F03           c                   const(x'33')
004900131017     d c_F04           c                   const(x'34')
005000131017     d c_F05           c                   const(x'35')
005100131017     d c_F06           c                   const(x'36')
005200131017     d c_F07           c                   const(x'37')
005300131017     d c_F08           c                   const(x'38')
005400131017     d c_F09           c                   const(x'39')
005500131017     d c_F10           c                   const(x'3A')
005600131017     d c_F11           c                   const(x'3B')
005700131017     d c_F12           c                   const(x'3C')
005800131017     d c_F13           c                   const(x'B1')
005900131017     d c_F14           c                   const(x'B2')
006000131017     d c_F15           c                   const(x'B3')
006100131017     d c_F16           c                   const(x'B4')
006200131017     d c_F17           c                   const(x'B5')
006300131017     d c_F18           c                   const(x'B6')
006400131017     d c_F19           c                   const(x'B7')
006500131017     d c_F20           c                   const(x'B8')
006600131017     d c_F21           c                   const(x'B9')
006700131017     d c_F22           c                   const(x'BA')
006800131017     d c_F23           c                   const(x'BB')
006900131017     d c_F24           c                   const(x'BC')
007000131017     d c_Enter         c                   const(x'F1')
007100131017     d c_RollDown      c                   const(x'F4')
007200131017     d c_RollUp        c                   const(x'F5')
007300131017
007400131017       //--------------------------------------------------------------
007500131017       //?Definizione schiere.                                         ?
007600131017       //--------------------------------------------------------------
007700131017
007800131017       // -?Messaggi di errore?
007900131017     d $Msg            s             78    dim( 3) ctdata perrcd(1)
008000131017
008100131017       //--------------------------------------------------------------
008200131017       //?Definizione aree dati.                                       ?
008300131017       //--------------------------------------------------------------
008400131017
008500131017       // -?Dati utente?
008600131017     d §AzUte        e ds                  extname(AZUTE00F)
008700131017     d                                     dtaara
008800131017     d §DatiUte      e ds                  extname(dDatiUte)
008900131017     d                                     dtaara
009000131017
009100131017       //--------------------------------------------------------------
009200131017       //?Definizione strutture dati.                                  ?
009300131017       //--------------------------------------------------------------
009400131017
009500131017       // -?Status ds?
009600131017     d Status         sds
009700131017     d   SDSpgm          *proc
009800131017     d*//SDSprm          *parms
009900131017     d*//SDSusr              254    263
010000131017
010100131017       // -?InfDS?
010200131017     d InfDspF         ds
010300131017     d   dsp_aid             369    369a
010400131017     d*//sfl_rrn             376    377i 0
010500131017     d*//min_nrr             378    379i 0
010600131017     d*//num_rcds            380    381i 0
010700131017
010800131017       // -?Indicatori su DspF?
010900131017     d IndDspF         ds                  inz
011000131017         // -?Abilitazione tasti funzionali?
011100131017     d   F8Attivo                      n   overlay(IndDspF : 08)
011200131017         // -?Emissione messaggio di errore?
011300131017     d   ErrMessage                    n   overlay(IndDspF : 28)
011400131017         // -?Indicatori di gestione del subfile?
011500131017     d   SflDsp_N                      n   overlay(IndDspF : 30)
011600131017     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011700131017     d   SflNxtChg                     n   overlay(IndDspF : 32)
011800131017     d   SflEnd                        n   overlay(IndDspF : 33)
011900131017         // -?Indicatori per Attributi di visualizzazione?
012000131017     d   DisabilitOpz                  n   overlay(IndDspF : 40)
012100131017     d   Ord_x_Des                     n   overlay(IndDspF : 41)
012200131017         // -?Posizionamento cursore & segnalazione errore?
012300131017     d   PosCurOPZ                     n   overlay(IndDspF : 50)
012400131017         // -?Riemissione videata?
012500131017     d   ErrGenerico                   n   overlay(IndDspF : 99)
012600131017
012700131017       // -?Parametri ricevuti?
012800131017     d KPJBA         e ds
012900131017     d TNTBMARds1    e ds                  inz
013000131017
013100131017       // -?Tabella "MAR" = Motivi Apertura Richiesta Assistenza?
013200131017     d dMAR          e ds                  qualified      inz
013300131017
013400131017       //--------------------------------------------------------------
013500131017       //?Definizione variabili globali.                               ?
013600131017       //--------------------------------------------------------------
013700131017
013800131017       // -?Flags booleani?
013900131017     d $Fine           s               n   inz
014000131017     d $InzS01         s               n   inz(*on)
014100131017
014200131017       // -?Variabili per la gestione del video?
014300131017     d $Video          s              2    inz('S1')
014400131017     d S01nrr          s                   like(C1RcdNbr) inz
014500131017     d SavS01csr       s                   like(C1RcdNbr) inz
014600131017
014700131017       // -?Conteggio selezioni (opz. 1) inserite?
014800131017     d wMemoSel        s                   like(S1Copz)   inz
014900131017
015000131017       // -?Campi di comodo?
015100131017     d wDate           s              8s 0 inz
015200131017
015300131017       // -?PARAMETRI PER ORDINAMENTO SUBFILE?
015400131017
015500131017       //--------------------------------------------------------------
015600131017       // -?Constants?
015700131017       //--------------------------------------------------------------
015800131017       // -?MaxKey - Maximum number of key fields allowed by this program?
015900131017     d c_MaxKey        c                   const(2)
016000131017       // -?Sort order:?
016100131017       //  ?1 = Sort in an ascending sequence?
016200131017       //  ?2 = Sort in a descending sequence?
016300131017     d c_Ascendente    c                   const(1)
016400131017     d c_Discendente   c                   const(2)
016500131017       // -?Key data type:?
016600131017       //  ? 0 = Signed binary?
016700131017       //  ? 2 = Signed zoned decimal?
016800131017       //  ? 3 = Signed packed decimal?
016900131017       //  ? 6 = Character with no national language sort sequence applied,?
017000131017       //  ?     if specified?
017100131017       //  ? 7 = Unsigned packed decimal?
017200131017       //  ?     All numbers will have the sign forced positive ('F'X).?
017300131017       //  ? 8 = Unsigned zoned decimal?
017400131017       //  ?     All numbers will have the sign forced positive ('F'X).?
017500131017       //  ? 9 = Unsigned binary?
017600131017       //  ?14 = Date in form of DD/MM/YY?
017700131017       //  ?15 = Date in form of DD.MM.YYYY?
017800131017     d c_Numero        c                   const(2)
017900131017     d c_Carattere     c                   const(6)
018000131017     d c_NumIntero     c                   const(8)
018100131017       //
018200131017     d c_Put           c                   const(1)
018300131017     d c_EndPut        c                   const(2)
018400131017     d c_Get           c                   const(3)
018500131017
018600131017       //--------------------------------------------------------------
018700131017       // -?Data Structures?
018800131017       //  ?SflRcd     - The entire subfile record to pass to the sort?
018900131017       //  ?QLGSCB     - The sort request block for the QLGSORT API?
019000131017       //  ?QLGSCB00   - The sort request block for the QLGSRTIO API?
019100131017       //  ?QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB?
019200131017       //  ?QUSEC      - Error structure for the QLGSORT API?
019300131017       //--------------------------------------------------------------
019400131017     d SflRcd          ds                  inz
019500131017     d   S1Copz                            inz
019600131017     d   S1Ccod                            inz
019700131017     d   S1Ddes                            inz
019800131017     d   Selected                     1a   inz
019900131017      /copy QSYSINC/QRPGLESRC,QLGSORT
020000131017     d QLGKL                         16    dim(c_MaxKey)
020100131017     d   QLGSP00                      9b 0 overlay(QLGKL:00001)
020200131017     d   QLGSS00                      9b 0 overlay(QLGKL:00005)
020300131017     d   QLGDT00                      9b 0 overlay(QLGKL:00009)
020400131017     d   QLGSO00                      9b 0 overlay(QLGKL:00013)
020500131017      /copy QSYSINC/QRPGLESRC,QLGSRTIO
020600131017      /copy QSYSINC/QRPGLESRC,QUSEC
020700131017
020800131017       //--------------------------------------------------------------
020900131017       // -?Standalone fields?
021000131017       //  ?Nrr        - Relative record number for subfile?
021100131017       //  ?SizeList   - Size of list?
021200131017       //  ?ReturnSize - Size of list returned by sort API?
021300131017       //--------------------------------------------------------------
021400131017     d NotUsed         s             16a   inz
021500131017     d ReturnSize      s              9b 0 inz
021600131017     d SizeList        s              9b 0 inz
021700131017     d RrnLast         s              5i 0 inz
021800131017     d Nrr             s              5i 0 inz
021900131017
022000131017       //--------------------------------------------------------------
022100131017       //?Definizione prototipi procedure usate.                       ?
022200131017       //--------------------------------------------------------------
022300131017
022400131017       // -?Reperimento dati utente?
022500131017     d TIBS34ds      e ds                  inz
022600131017      /copy gaitrasrc/srcProtoPR,TIBS34R
022700131017
022800131017       // -?Ordinamento subfile?
022900131017      /copy gaitrasrc/srcProtoPr,QLGSRTIO
023000131017
023100131017       //--------------------------------------------------------------
023200131017       //?Definizione key-list.                                        ?
023300131017       //--------------------------------------------------------------
023400131017
023500131017       // -?File TNTBE01L?
023600131017     d k_Tntbe01     e ds                  extname(TNTBE01L : *key)
023700131017     d                                     prefix(k_)   inz
023800131017
023900131017       //--------------------------------------------------------------
024000131017       //?Riepilogo indicatori utilizzati.                             ?
024100131017       //--------------------------------------------------------------
024200131017       //--------------------------------------------------------------
024300131017
024400131017       //--------------------------------------------------------------
024500131017       //?M A I N - L I N E                                            ?
024600131017       //--------------------------------------------------------------
024700131017
024800131017     c     *Entry        plist
024900131017     c                   parm                    KPJBA
025000131017
025100131017      /free
025200131017
025300131017       // -?Operazioni iniziali?
025400131017       exsr sr_RoutInz;
025500131017
025600131017       // -?Ciclo di gestione del file video?
025700131017       DoW  $Fine = *off;
025800131017
025900131017         select;
026000131017
026100131017           // -?Gestione videata S1 (subfile)?
026200131017           when $Video = 'S1';
026300131017             exsr sr_GesS01;
026400131017
026500131017           // -?? ? ??
026600131017           other;
026700131017             $Fine = *on;
026800131017
026900131017         endsl;
027000131017
027100131017       EndDo;
027200131017
027300131017       // -?Operazioni finali?
027400131017       exsr sr_RoutEnd;
027500131017
027600131017       //--------------------------------------------------------------
027700131017       //?Operazioni iniziali.                                         ?
027800131017       //--------------------------------------------------------------
027900131017       BEGSR  sr_RoutInz;
028000131017
028100131017         // -?Impostazione chiusura?
028200131017         *inLR = *on;
028300131017
028400131017         // -?Reperimento dati job?
028500131017         exsr  sr_DatiJob;
028600131017
028700131017         // -?Reperimento data odierna?
028800131017         wDate = %dec( %date() );
028900131017
029000131017         // -?Impostazione nome programma a video?
029100131017         //V1Tpgm = SDSpgm;
029200131017
029300131017         // -?Verifica parametri ricevuti?
029400131017         if  kpjbu <> *blank;
029500131017           tntbMARds1 = kpjbu;
029600131017         else;
029700131017           clear  tntbMARds1;
029800131017         endif;
029900131017
030000131017         // -?Pulizia parametri di output?
030100131017         clear  oMARcod;
030200131017         clear  oMARdes;
030300131017         clear  oMARfxx;
030400131017         clear  oMARmsg;
030500131017         oMARerr = *off;
030600131017
030700131017         // -?Impostazione videata iniziale?
030800131017         $Video = 'S1';
030900131017         reset  $InzS01;
031000131017         Ord_x_Des = *off;
031100131017         DisabilitOpz = *off;
031200131017
031300131017         // -?Impostazione iniziale / pulizia dei campi chiave?
031400131017         clear  k_tntbe01;
031500131017         k_TBEcod = c_Tab;
031600131017
031700131017       ENDSR;
031800131017
031900131017       //--------------------------------------------------------------
032000131017       //?Reperimento Dati del job (Utente/Operativi).                 ?
032100131017       //--------------------------------------------------------------
032200131017       BEGSR  sr_DatiJob;
032300131017
032400131017         in(e) §AzUte;
032500131017         if NOT %error;
032600131017           in(e) §DatiUte;
032700131017         endif;
032800131017         if %error or RSut = *blank;
032900131017           tibs34r ( tibs34ds );
033000131017           in §AzUte;
033100131017           in §DatiUte;
033200131017         endif;
033300131017
033400131017       ENDSR;
033500131017
033600131017       //--------------------------------------------------------------
033700131017       //?Gestione subfile S01                                         ?
033800131017       //--------------------------------------------------------------
033900131017       BEGSR  sr_GesS01;
034000131017
034100131017         // -?Inizializzazione videata?
034200131017         if  $InzS01 = *on;
034300131017           exsr  sr_InzS01;
034400131017           $InzS01 = *off;
034500131017         endif;
034600131017
034700131017         // -?Emissione Piede con tasti funzionali abilitati (window)?
034800131017         write  TBMARP01;
034900131017
035000131017         // -?Posizionamento cursore?
035100131017         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
035200131017         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
035300131017         //  ?si visualizza la pagina che vedeva l'utente quando ha?
035400131017         //  ?premuto l'ultimo tasto.?
035500131017         if  C1CsrRrn > *zero;
035600131017           C1RcdNbr = C1CsrRrn;
035700131017         else;
035800131017           C1RcdNbr = 1;
035900131017           write  TBMARS00;       //?(no rec.)?
036000131017         endif;
036100131017
036200131017         // -?Emissione videata (testata compresa) in window?
036300131017         exfmt  TBMARC01;
036400131017
036500131017         clear  V1Dmsg;
036600131017         reset  ErrMessage;
036700131017         reset  ErrGenerico;
036800131017
036900131017         SELECT;
037000131017
037100131017           // -?F3=Fine?
037200131017           WHEN  dsp_aid = c_F03;
037300131017             oMARfxx = '03';
037400131017             $Fine   = *on;
037500131017
037600131017           // -?F8=Cambio ordinamento?
037700131017           WHEN  dsp_aid = c_F08;
037800131017             exsr  sr_F08S01;
037900131017
038000131017           // -?F12=Ritorno?
038100131017           WHEN  dsp_aid = c_F12;
038200131017             oMARfxx = '12';
038300131017             $Fine   = *on;
038400131017
038500131017           // -?Subfile vuoto?
038600131017           WHEN  S01nrr = *zero;
038700131017
038800131017           // -?Invio?
038900131017           OTHER;
039000131017             // -?Gestione opzioni?
039100131017             exsr  sr_OpzS01;
039200131017             select;
039300131017               when  ErrGenerico;
039400131017                 leavesr;
039500131017               when  wMemoSel > *zero;
039600131017                 $Fine = *on;
039700131017                 leavesr;
039800131017             endsl;
039900131017
040000131017         ENDSL;
040100131017
040200131017       ENDSR;
040300131017
040400131017       //--------------------------------------------------------------
040500131017       //?Inizializzazione subfile S01                                 ?
040600131017       //--------------------------------------------------------------
040700131017       BEGSR  sr_InzS01;
040800131017
040900131017         // -?Spegnimento degli indicatori di errore?
041000131017         %subst(IndDspF : 50) = *off;
041100131017
041200131017         // -?Pulizia subfile?
041300131017         SflDsp_N    = *on;
041400131017         SflDspCtl_N = *on;
041500131017         write  TBMARC01;
041600131017         SflDspCtl_N = *off;
041700131017         SflEnd      = *off;
041800131017
041900131017         clear  C1RcdNbr;
042000131017         clear  C1CsrRrn;
042100131017         clear  S01nrr;
042200131017         clear  V1Dmsg;
042300131017         ErrMessage  = *off;
042400131017         ErrGenerico = *off;
042500131017
042600131017         // -?Caricamento completo dei dati nel subfile (per codice)?
042700131017         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
042800131017         //  ?l'eventuale ordinamento)?
042900131017         clear  S01nrr;
043000131017         SflNxtChg = *off;
043100131017         clear  k_TBEke1;
043200131017         clear  k_TBEke2;
043300131017         clear  k_TBElin;
043400131017         clear  k_TBEsif;
043500131017         setll  %kds(k_Tntbe01)  TNTBE000;
043600131017         reade  %kds(k_Tntbe01 : 1)  TNTBE000;
043700131017
043800131017         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
043900131017           if  TBEatb = *blank;
044000131017             exsr  sr_WriteS01;
044100131017           endif;
044200131017           reade  %kds(k_Tntbe01 : 1)  TNTBE000;
044300131017         EndDo;
044400131017
044500131017         // -?Memorizzazione Ultimo NRR del SubFile (per ordinamento)?
044600131017         RrnLast  = S01nrr;
044700131017
044800131017         // -?Visualizzazione del SFL (se ci sono dati)?
044900131017         SflDsp_N  = (S01nrr <= *zero);
045000131017
045100131017         // -?Attivazione del SFLEND?
045200131017         SflEnd = %eof(TNTBE01L);
045300131017
045400131017         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
045500131017         if  S01nrr > *zero;
045600131017           C1RcdNbr = 1;
045700131017         else;
045800131017           clear  C1RcdNbr;
045900131017         endif;
046000131017
046100131017         // -?Ordinamento subfile (se premuto F8)?
046200131017         if  Ord_x_Des  and  S01nrr > 1;
046300131017           exsr  sr_SortSfl;
046400131017         endif;
046500131017
046600131017         // -?(Dis)Abilitazione tasti funzionali?
046700131017         F8Attivo  = (S01nrr > *zero); // -?F8 = Ordinamento x ...?
046800131017
046900131017         // -?Emissione messaggio di segnalazione raggiungimento limite?
047000131017         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
047100131017           ErrGenerico = *on;
047200131017           ErrMessage  = *on;
047300131017           PosCurOPZ   = *on;
047400131017           V1Dmsg = $Msg(01);
047500131017           leavesr;
047600131017         endif;
047700131017
047800131017       ENDSR;
047900131017
048000131017       //--------------------------------------------------------------
048100131017       //?Registrazione del singolo rec. nel subfile S01               ?
048200131017       //--------------------------------------------------------------
048300131017       BEGSR  sr_WriteS01;
048400131017
048500131017         dMAR = TBEuni;
048600131017
048700140210         Select;
048800140210
048900140416           // -?Scarta i motivi NON consentiti IN GESTIONE agli utenti (SE richiesto)?
049000140210           When  iMARutUt = 'I'   and
049100140210                 dMAR.§MARutUt = 'N';
049200140210             leavesr;
049300140416
049400140416           // -?Scarta i motivi NON consentiti IN APERTURA agli utenti (SE richiesto)?
049500140416           When  iMARutAp = 'I'   and
049600140416                 dMAR.§MARutAp = 'N';
049700140416             leavesr;
049800140210
049900140210           // -?Scarta i motivi Esclusi dalle Statistiche (SE richiesto)?
050000140210           When  iMARest  = 'I'   and
050100140210                 dMAR.§MARest  = 'S';
050200140210             leavesr;
050300140210
050400140210           // -?Scarta i motivi Esclusi dal Calcolo Tempi Uffucio (SE richiesto)?
050500140210           When  iMARect  = 'I'   and
050600140210                 dMAR.§MARect  = 'S';
050700140210             leavesr;
050800140210
050900140210         EndSl;
051000131017
051100131017         // -?Impostazione campi nel record del subfile?
051200131017         clear  TBMARS01;
051300131017         S1Ccod = %trimr( TBEke1 );
051400131017         S1Ddes = dMAR.§MARdesc;
051500131017
051600131017         // -?Registrazione singolo record nel subfile?
051700131017         S01nrr += 1;
051800131017         write  TBMARS01;
051900131017
052000131017       ENDSR;
052100131017
052200131017       //--------------------------------------------------------------
052300131017       //?Gestione opzioni del subfile S01                             ?
052400131017       //--------------------------------------------------------------
052500131017       BEGSR  sr_OpzS01;
052600131017
052700131017         // -?Spegnimento degli indicatori di errore?
052800131017         %subst(IndDspF : 50) = *off;
052900131017
053000131017         clear  SavS01csr;
053100131017         clear  wMemoSel;
053200131017
053300131017         // -?Ciclo di lettura subfile?
053400131017         readc  TBMARS01;
053500131017
053600131017         DoW  Not %eof(TNTBMARD1);
053700131017
053800131017           SflNxtChg = *off;
053900131017           %subst(IndDspF : 50) = *off;
054000131017           C1RcdNbr = S01nrr;
054100131017
054200131017           select;
054300131017
054400131017             // -?Opz. 1=Selezione commerciale?
054500131017             when  S1Copz = 1;
054600131017               // -?Già selezionato un cliente?
054700131017               if  wMemoSel > *zero;
054800131017                 ErrGenerico = *on;
054900131017                 ErrMessage  = *on;
055000131017                 PosCurOPZ   = *on;
055100131017                 V1Dmsg = $Msg(03);
055200131017               else;
055300131017                 wMemoSel = S1Copz;
055400131017               endif;
055500131017
055600131017             // -?Nessuna opzione?
055700131017             when  S1Copz = *zero;
055800131017
055900131017             // -?Opzione errata?
056000131017             other;
056100131017               ErrGenerico = *on;
056200131017               ErrMessage  = *on;
056300131017               PosCurOPZ   = *on;
056400131017               V1Dmsg = $Msg(02);
056500131017
056600131017           endsl;
056700131017
056800131017           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
056900131017           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
057000131017           if  S1Copz   <> *zero  and
057100131017              (SavS01csr = *zero  or  SavS01csr < C1RcdNbr);
057200131017             SavS01csr   = C1RcdNbr;
057300131017           endif;
057400131017
057500131017           // -?Aggiornamento sfl?
057600131017           select;
057700131017             when  ErrMessage  or  S1Copz <> *zero;
057800131017               SflNxtChg = *on;
057900131017               C1CsrRrn  = C1RcdNbr;
058000131017             when  ErrGenerico;
058100131017               C1CsrRrn  = C1RcdNbr;
058200131017               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
058300131017             other;
058400131017               //clear  S1Copz;       ?(NO: selezione - se più di una?)?
058500131017           endsl;
058600131017
058700131017           UPDATE  TBMARS01;
058800131017
058900131017           if  ErrGenerico   or   ErrMessage;
059000131017             leave;
059100131017           endif;
059200131017
059300131017           readc  TBMARS01;
059400131017
059500131017         ENDDO;
059600131017
059700131017         // -?Riposizionam. cursore sul record contenente la prima opz.?
059800131017         //  ?(se non sono stati rilevati errori)?
059900131017         if  NOT ErrMessage   and   NOT ErrGenerico   and
060000131017             SavS01csr > *zero;
060100131017           C1CsrRrn = SavS01csr;
060200131017         endif;
060300131017
060400131017       ENDSR;
060500131017
060600131017       //--------------------------------------------------------------
060700131017       //?Gestione tasto funzionale F08 da videata C01 / S01           ?
060800131017       //?F08-Cambia ordinamento sfl                                   ?
060900131017       //--------------------------------------------------------------
061000131017       BEGSR  sr_F08S01;
061100131017
061200131017         if  Not Ord_x_Des;
061300131017             Ord_x_Des = *on;
061400131017         else;
061500131017           Ord_x_Des = *off;
061600131017         endif;
061700131017
061800131017         // -?SubFile già completamente caricato: basta riordinarlo?
061900131017         exsr  sr_SortSfl;
062000131017
062100131017       ENDSR;
062200131017
062300131017       //--------------------------------------------------------------
062400131017       //?Ordinamento subfile                                          ?
062500131017       //?  This subroutine sorts the subfile records.                 ?
062600131017       //--------------------------------------------------------------
062700131017       BEGSR  sr_SortSfl;
062800131017
062900131017         // -?Impostazione NRR dell'ultimo record del subfile          ?
063000131017         //  ?(già fatto in sr_InzS01:                                 ?
063100131017         //  ? S01NRR può essere variato, se già inserita un'opzione)  ?
063200131017         //RrnLast  = S01nrr;
063300131017
063400131017         C1RcdNbr = 1;
063500131017         clear  C1CsrRrn;
063600131017         clear  S01nrr;
063700131017         clear  V1Dmsg;
063800131017         SflNxtChg   = *off;
063900131017         ErrMessage  = *off;
064000131017         ErrGenerico = *off;
064100131017         %subst(IndDspF : 50) = *off;
064200131017
064300131017         //?___________________________________________________________?
064400131017         //?Initialize the key fields to sort on.?
064500131017         //?There is one extra field not in the subfile -- Selected --?
064600131017         //?that is added to the record. This field is used to place?
064700131017         //?selected records in front of nonselected records.?
064800131017         //?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
064900131017
065000131017         clear  QLGKL;
065100131017         clear  QLGSKL;
065200131017         clear  QLGSCB;
065300131017         clear  QLGSCB00;
065400131017
065500131017         // -?Gestione della scelta ordinamento:?
065600131017         Select;
065700131017
065800131017           // -?Ordinamento per Codice Cliente?
065900131017           when  Not Ord_x_Des;
066000131017             // -?1 campi chiave?
066100131017             QLGNBRK  = 1;
066200131017             // -?1° campo: codice cliente (S1Ccli)?
066300131017             //            ?a posizone    3,  7 byte, int,  ascending.?
066400131017             QLGSP    = %size(S1Copz) + 1;
066500131017             QLGSS    = %size(S1Ccod);
066600131017             QLGDT    = c_Carattere;
066700131017             QLGSO    = c_Ascendente;
066800131017             QLGKL(1) = QLGSKL;
066900131017
067000131017           // -?Ordinamento per Descrizione Cliente?
067100131017           when  Ord_x_Des;
067200131017             // -?2 campi chiave?
067300131017             QLGNBRK  = 2;
067400131017             // -?1° campo: descr. cliente (S1Dcli)?
067500131017             //            ?a posizone   10, 35 byte, char, ascending.?
067600131017             QLGSP    = %size(S1Copz) + %size(S1Ccod) + 1;
067700131017             QLGSS    = %size(S1Ddes);
067800131017             QLGDT    = c_Carattere;
067900131017             QLGSO    = c_Ascendente;
068000131017             QLGKL(1) = QLGSKL;
068100131017             // -?2° campo: codice cliente (S1Ccli)?
068200131017             //            ?a posizone    3,  7 byte, int,  ascending.?
068300131017             QLGSP    = %size(S1Copz) + 1;
068400131017             QLGSS    = %size(S1Ccod);
068500131017             QLGDT    = c_Carattere;
068600131017             QLGSO    = c_Ascendente;
068700131017             QLGKL(2) = QLGSKL;
068800131017
068900131017         EndSl;
069000131017
069100131017         // -?Load other sort parameters.?
069200131017         QLGLB   = 80 + 16 * c_MaxKey;
069300131017         QLGRL   = %size(SflRcd) - 1;
069400131017         QLGRT   = 8;
069500131017         QLGOKL  = 80;
069600131017         QLGLKE  = 16;
069700131017         QLGLSS  = 290;
069800131017         // -?Initialize Sort I/O API fields.?
069900131017         QLGRL00 = QLGRL;
070000131017         QLGRC00 = 1;
070100131017         clear  QUSEI;
070200131017         QUSBPRV = %size(QUSEC);
070300131017
070400131017         // -?First step - Initialize the sort routine.?
070500131017         pr_QLGSORT ( QLGSCB   : NotUsed    : NotUsed :
070600131017                      SizeList : ReturnSize : QUSEC );
070700131017
070800131017         // -?Next step - Write records to I/O routine.?
070900131017         QLGRT00  = c_Put;
071000131017         For  Nrr = 1  To  RrnLast;
071100131017           chain  Nrr  TBMARS01;
071200131017           // -?Solo le righe con Selected = 'Y' sono riordinate,?
071300131017           //  ?quindi per fare un ordinamento di tutte le righe?
071400131017           //  ?metto 'Y' sempre.?
071500131017           Selected = 'Y';
071600131017           clear  QUSEI;
071700131017           QUSBPRV  = %size(QUSEC);
071800131017           pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
071900131017                         SizeList : NotUsed : QUSEC );
072000131017         EndFor;
072100131017
072200131017         // -?Next step - Signal end of input, clear subfile for reload.?
072300131017         QLGRT00 = c_EndPut;
072400131017         clear  QUSEI;
072500131017         QUSBPRV = %size(QUSEC);
072600131017         pr_QLGSRTIO ( QLGSCB00 : SflRcd  : NotUsed :
072700131017                       SizeList : NotUsed : QUSEC );
072800131017
072900131017         // -?Pulizia subfile?
073000131017         SflDsp_N    = *on;
073100131017         SflDspCtl_N = *on;
073200131017         write  TBMARC01;
073300131017         SflDspCtl_N = *off;
073400131017         SflEnd      = *off;
073500131017
073600131017         // -?Final step - Write the records back to the subfile.?
073700131017         QLGRT00  = c_Get;
073800131017         For  Nrr = 1  To RrnLast;
073900131017           clear  QUSEI;
074000131017           QUSBPRV = %size(QUSEC);
074100131017           pr_QLGSRTIO ( QLGSCB00 : NotUsed : SflRcd :
074200131017                         QLGRL00  : NotUsed : QUSEC );
074300131017           // -?Ripristino indicatori utilizzati nel sfl rec?
074400131017           SflNxtChg = (S1Copz <> *zero);
074500131017           // -?e aggiorno?
074600131017           S01nrr = Nrr;
074700131017           write  TBMARS01;
074800131017         EndFor;
074900131017
075000131017         C1CsrRrn = 1;
075100131017
075200131017         // -?Visualizzazione del SFL (se ci sono dati)?
075300131017         SflDsp_N = (S01nrr <= *zero);
075400131017
075500131017         // -?Attivazione del SFLEND?
075600131017         SflEnd = %eof(TNTBE01L);
075700131017
075800131017       ENDSR;
075900131017
076000131017       //--------------------------------------------------------------
076100131017       //?Operazioni finali.                                         ?
076200131017       //--------------------------------------------------------------
076300131017       BEGSR  sr_RoutEnd;
076400131017
076500131017         // -?Restituzione parametri?
076600131017         //  ?(se selezionato un motivo apertura R.A.)?
076700131017         If  wMemoSel > *zero;
076800131017
076900131017           chain  SavS01csr  TBMARS01;
077000131017
077100131017           oMARcod = S1Ccod;
077200131017           oMARdes = S1Ddes;
077300131017
077400131017         EndIf;
077500131017
077600131017         kpjbu = tntbMARds1;
077700131017
077800131017         // -?Chiusura *pgm?
077900131017         return;
078000131017
078100131017       ENDSR;
078200131017
078300131017      /end-free
078400131017
078500131017** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
078600131017Raggiunto il limite massimo dei record caricabili a video                      1
078700131017Opzione errata                                                                 2
078800131017Selezionati più di un Motivo Apertura Richiesta Assistenza                     3
