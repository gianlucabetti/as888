000100100111     /*END
000200090427       //--------------------------------------------------------------
000300090427       //?TRTB30R * Manutenzione associazione stampanti - Tab. "5P"    ?
000400090427       //--------------------------------------------------------------
000500090427
000600090427     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090427
000800090427       //--------------------------------------------------------------
000900090427       //?Dichiarazione file.                                          ?
001000090427       //--------------------------------------------------------------
001100090427
001200090515       // - Organigramma filiale/aziendale
001300090427     fAZORG01L  if   e           k disk
001400090427
001500090427       // - Tabelle
001600090427     fTABEL00F  Uf A e           k disk
001700090427
001800090427       // - Video Gestione
001900090427     fTRTB30D   cf   e             workstn
002000090427     f                                     sfile(TB30S1 : S01nrr)
002100090504     f                                     sfile(TB30S2 : S02nrr)
002200090427     f                                     indds(IndDspF)
002300090427     f                                     infds(InfDspF)
002400090427
002500090427       //--------------------------------------------------------------
002600090427       //?Definizione costanti.                                        ?
002700090427       //--------------------------------------------------------------
002800090427
002900090427       // - Codice tabella in gestione
003000090428     d c_Tab           c                   const('5P')
003100090427
003200090427       // - Codice utente
003300090427     d c_Kut           c                   const(1)
003400090427
003500090427       // - Costante utente "EDP"
003600090427     d c_Edp           c                   const('EDP')
003700090505
003800090505       // - Costante "INSERIMENTO"
003900090505     d c_Insert        c                   const('INSERIMENTO')
004000090427
004100090427      // - Numero di record in ciascuna videata di subfile
004200090504     d c_SflPag        c                   const(14)
004300090504     d c_SflPag2       c                   const(14)
004400090427
004500090427      // - Numero massimo di record gestibili
004600090504     d c_MaxRec        c                   const(999)
004700090427
004800090427       // - Costante per controllo "caratteri solo numerici"
004900090428     d c_Digit         c                   const('0123456789')
005000090427
005100090427       // - Tasti funzionali a video
005200090427     d c_F01           c                   const(x'31')
005300090427     d c_F02           c                   const(x'32')
005400090427     d c_F03           c                   const(x'33')
005500090427     d c_F05           c                   const(x'35')
005600090427     d c_F06           c                   const(x'36')
005700090427     d c_F07           c                   const(x'37')
005800090427     d c_F08           c                   const(x'38')
005900090427     d c_F09           c                   const(x'39')
006000090427     d c_F10           c                   const(x'3A')
006100090427     d c_F12           c                   const(x'3C')
006200090427     d c_F13           c                   const(x'B1')
006300090427     d c_F14           c                   const(x'B2')
006400090427     d c_F15           c                   const(x'B3')
006500090427     d c_F16           c                   const(x'B4')
006600090427     d c_F17           c                   const(x'B5')
006700090427     d c_F18           c                   const(x'B6')
006800090427     d c_F19           c                   const(x'B7')
006900090427     d c_F20           c                   const(x'B8')
007000090427     d c_F21           c                   const(x'B9')
007100090427     d c_F22           c                   const(x'BA')
007200090427     d c_F23           c                   const(x'BB')
007300090427     d c_F24           c                   const(x'BC')
007400090427     d c_Enter         c                   const(x'F1')
007500090427     d c_RollDown      c                   const(x'F4')
007600090427     d c_RollUp        c                   const(x'F5')
007700090504
007800090504       // - Attributi di Visualizzazione
007900090504     d c_DspAtrNorm    c                   const(x'A0')
008000090504     d c_DspAtrRI      c                   const(x'A1')
008100090504     d c_DspAtrRED     c                   const(x'A8')
008200090427
008300090427       //--------------------------------------------------------------
008400090427       //?Definizione schiere.                                         ?
008500090427       //--------------------------------------------------------------
008600090427
008700090427       // - Messaggi di errore
008800090508     d $Msg            s             78    dim(20) ctdata perrcd(1)
008900090504
009000090504       // - Dati intabellati all'inizio del pgm
009100090504     d $Fil            s              3    dim(c_MaxRec)  inz(*hival)
009200090508     d $wFil           s              3    dim(c_MaxRec)  inz  ascend
009300090504     d $5PB            s        +     1    like(ds5PB)
009400090504     d                                     dim(c_MaxRec)  inz
009500090504     d $5PO            s        +     1    like(ds5P)
009600090504     d                                     dim(c_MaxRec)  inz
009700090504     d $5PS            s        +     1    like(ds5P)
009800090504     d                                     dim(c_MaxRec)  inz
009900090504     d $5PT            s        +     1    like(ds5P)
010000090504     d                                     dim(c_MaxRec)  inz
010100090427
010200090427       //--------------------------------------------------------------
010300090427       //?Definizione aree dati.                                       ?
010400090427       //--------------------------------------------------------------
010500090427
010600090427       // - Dati utente
010700090427     d §AzUte        e ds                  extname(AZUTE00F)
010800090427     d                                     dtaara
010900090427     d §DatiUte      e ds                  extname(dDatiUte)
011000090427     d                                     dtaara
011100090427
011200090427       //--------------------------------------------------------------
011300090427       //?Definizione strutture dati.                                  ?
011400090427       //--------------------------------------------------------------
011500090427
011600090427       // - Status ds
011700090427     d Status         sds
011800090427     d  SDSpgm           *proc
011900090505     d  SDSusr               254    263
012000090427
012100090427       // - InfDS
012200090427     d InfDspF         ds
012300090427     d   dsp_aid             369    369a
012400090427     d   sfl_rrn             376    377i 0
012500090427     d   min_nrr             378    379i 0
012600090427     d   num_rcds            380    381i 0
012700090427
012800090427       // - Indicatori su DspF
012900090427     d IndDspF         ds                  inz
013000090427         // - Abilitazione tasti funzionali
013100090427     d   F5Attivo                      n   overlay(IndDspF : 05)
013200090427     d   F6Attivo                      n   overlay(IndDspF : 06)
013300090427     d   F16Attivo                     n   overlay(IndDspF : 16)
013400090505     d   F23Attivo                     n   overlay(IndDspF : 23)
013500090427         // - Emissione messaggio di errore
013600090427     d   ErrMessage                    n   overlay(IndDspF : 28)
013700090427         // - Indicatori di gestione del subfile
013800090427     d   SflDsp_N                      n   overlay(IndDspF : 30)
013900090427     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
014000090427     d   SflNxtChg                     n   overlay(IndDspF : 32)
014100090427     d   SflEnd                        n   overlay(IndDspF : 33)
014200090504     d   SflDsp2_N                     n   overlay(IndDspF : 34)
014300090504     d   SflDspCtl2_N                  n   overlay(IndDspF : 35)
014400090504     d   SflNxtChg2                    n   overlay(IndDspF : 36)
014500090504     d   SflEnd2                       n   overlay(IndDspF : 37)
014600090504         // - Indicatori per Attibuti di visualizzazione
014700090504     d   ProtectFIL                    n   overlay(IndDspF : 40)
014800090505     d   ProtectPGM                    n   overlay(IndDspF : 41)
014900090427         // - Posizionamento cursore & segnalazione errore
015000090504     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015100090504       //PosCurBOL  (ora hidden)       n   overlay(IndDspF : 51)
015200090427     d   PosCurBA4                     n   overlay(IndDspF : 52)
015300090427     d   PosCurBA5                     n   overlay(IndDspF : 53)
015400090504       //PosCurBTS  (ora hidden)       n   overlay(IndDspF : 54)
015500090504       //PosCurBPG  (ora hidden)       n   overlay(IndDspF : 55)
015600090427     d   PosCurBPL                     n   overlay(IndDspF : 56)
015700090427     d   PosCurBUS                     n   overlay(IndDspF : 57)
015800090427     d   PosCurOTQO                    n   overlay(IndDspF : 58)
015900090427     d   PosCurPGMO                    n   overlay(IndDspF : 59)
016000090427     d   PosCurOTQS                    n   overlay(IndDspF : 60)
016100090427     d   PosCurMODS                    n   overlay(IndDspF : 61)
016200090427     d   PosCurOTQT                    n   overlay(IndDspF : 62)
016300090427         //   - Riemissione videata
016400090427     d   ErrGenerico                   n   overlay(IndDspF : 99)
016500090427
016600090427       // - Parametri ricevuti
016700090427     d KPJBA         e ds
016800090427
016900090427       // - Parametri per Reperimento dati utente
017000090428     d TIBS34ds      e ds                  inz
017100090427
017200090427       // - Tabelle usate
017300090427     d ds5P          e ds                  inz
017400090427     d ds5PB         e ds                  inz
017500090427     d ds5PM         e ds                  inz
017600090427
017700090427       //--------------------------------------------------------------
017800090427       //?Definizione variabili globali.                               ?
017900090427       //--------------------------------------------------------------
018000090427
018100090427       // - Flags booleani
018200090427     d $Fine           s               n   inz
018300090504     d $InzS01         s               n   inz(*on)
018400090504     d $InzS02         s               n   inz(*on)
018500090427     d $InzD02         s               n   inz(*on)
018600090504     d $RecAnn         s               n   inz
018700090505     d $F23            s               n   inz
018800090504
018900090504       // - Indici di schiera
019000090508     d wx              s              3  0 inz
019100090504     d xx              s              3  0 inz
019200090427
019300090427       // - Variabili per la gestione del video
019400090504     d $Video          s              2    inz('S1')
019500090427     d W_SflPag        s              3  0 inz
019600090504     d W_SflPag2       s              3  0 inz
019700090427     d wPag            s              4  0 inz
019800090427     d wRig            s              3  0 inz
019900090508     d S01nrr          s                   like(C1RcdNbr) inz
020000090508     d S02nrr          s                   like(C1RcdNbr) inz
020100090504     d SavS01csr       s                   like(C1RcdNbr) inz
020200090508     d SavC1Pfil       s                   like(C1Pfil)   inz
020300090428
020400090428       // - Variabili per i controlli del singolo campo alla variazione
020500090428       //   dello stesso
020600090428     d SaveBA4         s                   like(V2Cba4)   inz
020700090428     d SaveBA5         s                   like(V2Cba5)   inz
020800090428     d SaveBPL         s                   like(V2Cbpl)   inz
020900090428     d SaveOTQo        s                   like(V2CotqO)  inz
021000090428     d SavePGMo        s                   like(V2CpgmO)  inz
021100090428     d SaveOTQs        s                   like(V2CotqS)  inz
021200090428     d SaveOTQt        s                   like(V2CotqT)  inz
021300090427
021400090427       // - Parametri per pgm interrogazione tabelle
021500090427      /copy gaitrasrc/srcProtoPI,X§TABER
021600090428
021700090428       // - Flag errore per pgm TRUL08C
021800090428     d wErr            s              1    inz
021900090428
022000090428       // - Campi di comodo
022100090504     d WWCfil          s              3    inz
022200090504     d WWDfil          s             20    inz
022300090427
022400090427       //--------------------------------------------------------------
022500090427       //?Definizione prototipi procedure.                             ?
022600090427       //--------------------------------------------------------------
022700090427
022800090427       // - Reperimento dati utente
022900090427      /copy gaitrasrc/srcProtoPR,TIBS34R
023000090427
023100090427       // - Interrogazione tabelle
023200090427      /copy gaitrasrc/srcProtoPR,X§TABER
023300090428
023400090428       // - Verifica esistenza oggetto
023500090428     d trul08c         pr                  extpgm('TRUL08C')
023600090428     d   ul08obj                     10    const
023700090428     d   ul08lib                     10    const
023800090428     d   ul08obytyp                  10    const
023900090428     d   ul08mbr                     10    const
024000090428     d   ul08err                      1
024100090427
024200090427       //--------------------------------------------------------------
024300090427       //?Definizione key-list.                                        ?
024400090427       //--------------------------------------------------------------
024500090427
024600090427       // - File TABEL00F
024700090427     d k03tabel00    e ds                  extname(TABEL00F : *key)
024800090427     d                                     prefix(k_)   inz
024900090427
025000090427       //--------------------------------------------------------------
025100090427       //?Riepilogo indicatori utilizzati.                             ?
025200090427       //--------------------------------------------------------------
025300090427       //--------------------------------------------------------------
025400090427
025500090427       //--------------------------------------------------------------
025600090427       //?M A I N - L I N E                                            ?
025700090427       //--------------------------------------------------------------
025800090427
025900090427     c     *Entry        plist
026000090427     c                   parm                    KPJBA
026100090427
026200090427      /free
026300090427
026400090427       //?Operazioni iniziali?
026500090427       exsr sr_RoutInz;
026600090427
026700090427       DoW  $Fine = *off;
026800090427         select;
026900090504           // - Gestione subfile S1 (selez. filiale)
027000090504           when $Video = 'S1';
027100090504             exsr sr_GesS01;
027200090504           // - Gestione videata D2 (modifica)
027300090430           when $Video = 'D2';
027400090430             exsr sr_GesD02;
027500090504           // - Gestione subfile S2 (vis. tab. 5P/M)
027600090504           when $Video = 'S2';
027700090504             exsr sr_GesS02;
027800090427           other;
027900090427             $Fine = *on;
028000090427         endsl;
028100090427       enddo;
028200090427
028300090427       //?Operazioni finali?
028400090427       exsr sr_RoutEnd;
028500090427
028600090427       //--------------------------------------------------------------
028700090427       //?Operazioni iniziali.                                         ?
028800090427       //--------------------------------------------------------------
028900090427       BEGSR  sr_RoutInz;
029000090427
029100090427         *inLR = *on;
029200090427
029300090427         //?Reperimento dati job?
029400090427         exsr  sr_DatiJob;
029500090427
029600090427         //?Impostazione nome programma a video?
029700090427         V1Tpgm = SDSpgm;
029800090427         clear  V1Topz;
029900090427
030000090427         //?Impostazione campi "fissi"?
030100090427         k_TBLkut = c_Kut;
030200090427         k_TBLcod = c_Tab;
030300090427
030400090427       ENDSR;
030500090427
030600090427       //--------------------------------------------------------------
030700090427       //?Reperimento Dati del job (Utente/Operativi).                 ?
030800090427       //--------------------------------------------------------------
030900090427       BEGSR  sr_DatiJob;
031000090427
031100090427         in(e) §AzUte;
031200090427         if NOT %error;
031300090427           in(e) §DatiUte;
031400090427         endif;
031500090427         if %error or RSut = *blank;
031600090427           tibs34r ( tibs34ds );
031700090427           in §AzUte;
031800090427           in §DatiUte;
031900090427         endif;
032000090427
032100090427       ENDSR;
032200090427
032300090427       //--------------------------------------------------------------
032400090504       //?Gestione subfile S01                                         ?
032500090427       //--------------------------------------------------------------
032600090504       BEGSR  sr_GesS01;
032700090427
032800090504         //?Inizializzazione subfile?
032900090504         if  $InzS01 = *on;
033000090504           exsr  sr_InzS01;
033100090504           $InzS01 = *off;
033200090427         endif;
033300090427
033400090427         //?Emissione Testata e Piede con tasti funzionali abilitati?
033500090427         write  TB30T1;
033600090427         write  TB30P1;
033700090504
033800090504         //?Posizionamento cursore?
033900090504         if  C1CsrRrn > *zero;
034000090504           C1RcdNbr = C1CsrRrn;
034100090504         else;
034200090504           C1RcdNbr = 1;
034300090504           //write TB30S0;               // (no rec.)
034400090504         endif;
034500090427
034600090427         //?Emissione videata?
034700090504         exfmt  TB30C1;
034800090427         reset  ErrMessage;
034900090427         reset  ErrGenerico;
035000090504         clear  V1Dmsg;
035100090504
035200090427         SELECT;
035300090427
035400090504
035500090504           //?F1=Visualizzazione associazione stampanti-etichette?
035600090504           WHEN  dsp_aid = c_F01;
035700090504             $Video  = 'S2';
035800090504             $InzS02 = *on;
035900090504
036000090427           //?F3=Fine?
036100090427           WHEN  dsp_aid = c_F03;
036200090427             $Fine = *on;
036300090505
036400090505           //?F5=Refresh?
036500090505           WHEN  dsp_aid = c_F05;
036600090508             clear  $wFil;
036700090508             clear  SavC1Pfil;
036800090505             $InzS01 = *on;
036900090504
037000090505           //?F6=Inserimento?
037100090508           //WHEN  dsp_aid = c_F06;
037200090508           //  V1Topz = '  INSERIMENTO  ';
037300090508           //  $Video  = 'D2';
037400090508           //  $InzD02 = *on;
037500090505
037600090505           //?F12=Ritorno?
037700090505           WHEN  dsp_aid = c_F12;
037800090505             $Fine = *on;
037900090504
038000090504           //?Roll-Up?
038100090504           WHEN  dsp_aid = c_RollUp;
038200090504             exsr sr_RollUpS01;
038300090504
038400090504           //?SubFile vuoto?
038500090504           WHEN  S01nrr = *zero;
038600090508
038700090508           //?Richiesto posizionamento?
038800090508           //WHEN  C1Pfil <> SavC1Pfil;
038900090508           //  exsr  sr_CtrlPosizS01;
039000090508           //  if  Not ErrMessage;
039100090508           //    $InzS01 = *on;
039200090508           //  endif;
039300090504
039400090508           //?Invio?
039500090427           OTHER;
039600090508             // -?Gestione opzioni?
039700090504             exsr  sr_OpzS01;
039800090508             if  ErrGenerico;
039900090504               leavesr;
040000090504             endif;
040100090508             // -?Richiesto posizionamento?
040200090508             if  C1Pfil <> SavC1Pfil;
040300090508               $InzS01 = *on;
040400090508             endif;
040500090427
040600090427         ENDSL;
040700090427
040800090427       ENDSR;
040900090427
041000090427       //--------------------------------------------------------------
041100090504       //?Inizializzazione subfile S01                                 ?
041200090427       //--------------------------------------------------------------
041300090504       BEGSR  sr_InzS01;
041400090427
041500090504         //?Spegnimento degli indicatori di errore?
041600090505         %subst(IndDspF : 50) = *off;
041700090504
041800090504         //?Pulizia subfile?
041900090504         SflDsp_N    = *on;
042000090504         SflDspCtl_N = *on;
042100090504         write  TB30C1;
042200090504         SflDspCtl_N = *off;
042300090504         SflEnd      = *off;
042400090504
042500090504         clear  W_SflPag;
042600090504         clear  C1RcdNbr;
042700090504         clear  C1CsrRrn;
042800090504         clear  S01nrr;
042900090504         clear  V1Dmsg;
043000090504         ErrMessage  = *off;
043100090504         ErrGenerico = *off;
043200090504
043300090508         //?Caricamento di tutte le tab. 5P/x (di tutte le filiali)?
043400090508         if  $wFil(1) = *blank;
043500090508           exsr  sr_InTab5P;
043600090508         endif;
043700090504         exsr  sr_RollUpS01;
043800090504
043900090504         //?Impaginazione subfile?
044000090504         // -> forza l'impaginazione sul 1° rec. del subfile
044100090504         if S01nrr > *zero;
044200090504           C1RcdNbr  = 1;
044300090504           C1CsrRrn  = 1;
044400090504         else;
044500090504           clear C1RcdNbr;
044600090504         endif;
044700090427
044800090427       ENDSR;
044900090505
045000090505       //--------------------------------------------------------------
045100090505       //?Intabellamento dati già esistenti per filiale                ?
045200090505       //--------------------------------------------------------------
045300090508       BEGSR  sr_InTab5P;
045400090505
045500090505         reset  $Fil;
045600090505         clear  $wFil;
045700090505         clear  $5PB;
045800090505         clear  $5PO;
045900090505         clear  $5PS;
046000090505         clear  $5PT;
046100090508         clear  xx;
046200090505
046300090505         //?Ciclo di lettura:?
046400090505         //   la prima sarà la "Bxxx" (Bolle/LdV)
046500090505         k_TBLkey = 'B';
046600090505         setll  %kds(k03tabel00)  TABEL;
046700090505         reade(n)  %kds(k03tabel00 : 2)  TABEL;
046800090505
046900090505         DOW  Not %eof(TABEL00F);
047000090505
047100090505           // -?Elaborazione delle sole tabelle "B", "O", "S" e "T"?
047200090505           IF (%subst(TBLkey : 1 : 1) = 'B'     or
047300090505               %subst(TBLkey : 1 : 1) = 'O'     or
047400090505               %subst(TBLkey : 1 : 1) = 'S'     or
047500090505               %subst(TBLkey : 1 : 1) = 'T');
047600090505
047700090505             if  %subst(TBLkey : 1 : 1) = 'B';
047800090505               xx      += 1;
047900090505               $Fil(xx) = %subst(TBLkey : 2 : 3);
048000090505               $5PB(xx) = TBLflg + TBLuni;
048100090505             else;
048200090505               xx = %lookup( %subst(TBLkey : 2 : 3) : $Fil );
048300090505               if  xx = *zero;
048400090505                 xx = %lookup( *hival : $Fil );
048500090505                 $Fil(xx) = %subst(TBLkey : 2 : 3);
048600090505               endif;
048700090505               select;
048800090505                 when  %subst(TBLkey : 1 : 1) = 'O';
048900090505                   $5PO(xx) = TBLflg + TBLuni;
049000090505                 when  %subst(TBLkey : 1 : 1) = 'S';
049100090505                   $5PS(xx) = TBLflg + TBLuni;
049200090505                 when  %subst(TBLkey : 1 : 1) = 'T';
049300090505                   $5PT(xx) = TBLflg + TBLuni;
049400090505               endsl;
049500090505             endif;
049600090505
049700090505           // -?Si scarta la tabella "5P"/"Mxxx" (elaborata in S02)?
049800090505           ELSE;
049900090505
050000090505             k_TBLkey = %subst(TBLkey : 1 : 1) + '99999';
050100090505             setgt  %kds(k03tabel00)  TABEL;
050200090505
050300090505           ENDIF;
050400090505
050500090505           reade(n)  %kds(k03tabel00 : 2)  TABEL;
050600090505
050700090505         ENDDO;
050800090508
050900090508         //?Ordinamento per filiale?
051000090508         $wFil = $Fil;
051100090508         sorta $wFil;
051200090505
051300090505       ENDSR;
051400090504
051500090504       //--------------------------------------------------------------
051600090504       //?Caricamento singola pagina nel subfile S01                   ?
051700090504       //--------------------------------------------------------------
051800090504       BEGSR  sr_RollUpS01;
051900090504
052000090504         S01nrr    = (W_SflPag * c_SflPag);
052100090504         W_SflPag += 1;
052200090504         SflNxtChg = *off;
052300090508
052400090508         //?Impostaz. posizione di partenza nella schiera delle fil.?
052500090508         select;
052600090508           // -?Inzializzazione con tutte le filiali?
052700090508           when  $InzS01 = *on   and   C1Pfil <= *zero;
052800090508             wx  = 1;
052900090508           // -?Inizializzazione con solo alcune filiali?
053000090508           when  $InzS01 = *on   and   C1Pfil <> SavC1Pfil;
053100090508             wx  = %lookupGE( %editc(C1Pfil:'X') : $wFil );
053200090508           // -?Roll-Up (si prosegue con il caricamento)?
053300090508           other;
053400090511             //wx += 1;          (NO: già incrementato)
053500090508         endsl;
053600090504
053700090505         //?Ciclo di caricamento dati da schiere $5Px?
053800090508         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
053900090508              S01nrr    < c_MaxRec                and
054000090508              S01nrr    < %elem($wFil)            and
054100090508              wx        > *zero                   and
054200090508              $wFil(wx) < *hival;
054300090504
054400090508           // -?Registrazione singolo record nel subfile?
054500090508           xx = %lookup( $wFil(wx) : $Fil );
054600090504           clear  ds5PB;
054700090504           clear  ds5P;
054800090504           clear  TB30S1;
054900090504           if  xx > *zero;
055000090508             S1Cfil = $Fil(xx);
055100090508             exsr sr_FldS01;
055200090504           endif;
055300090504
055400090508           // -?Decodifica filiale?
055500090504           WWCfil = S1Cfil;
055600090504           exsr  sr_CtrFil;
055700090504           S1Dfil = WWDfil;
055800090505           ErrMessage = *off;
055900090505           %subst(IndDspF : 50) = *off;
056000090505           clear  V1Dmsg;
056100090504
056200090508           // -?Scrittura record nerl subfile?
056300090504           S01nrr += 1;
056400090504           write  TB30S1;
056500090508
056600090508           wx     += 1;
056700090504
056800090504         EndDo;
056900090508
057000090508         //?Memorizzazione posizionamento effettuato?
057100090508         SavC1Pfil = C1Pfil;
057200090504
057300090504         //?Visualizzazione del SFL (se ci sono dati)?
057400090504         SflDsp_N  = (S01nrr <= *zero);
057500090504
057600090504         //?Attivazione del SFLEND?
057700090504         SflEnd = (S01nrr >= c_MaxRec   or
057800090508                   $wFil(wx) = *hival);
057900090504
058000090504         //?Posizionamento cursore al 1° rcd della pagina?
058100090504         if  S01nrr > *zero;
058200090504           wPag     = S01nrr / c_SflPag;
058300090504           wRig     = S01nrr - (c_SflPag * wPag);
058400090504           C1RcdNbr = wPag * c_SflPag;
058500090504           if  wRig > *zero;
058600090504             C1RcdNbr += 1;
058700090504           else;
058800090504             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
058900090504           endif;
059000090504         else;
059100090504           clear  C1RcdNbr;
059200090504         endif;
059300090504
059400090504         C1CsrRrn = C1RcdNbr;
059500090504
059600090504       ENDSR;
059700090505
059800090505       //--------------------------------------------------------------
059900090505       //?Aggiornamento campi nel singolo record del subfile S01       ?
060000090505       //--------------------------------------------------------------
060100090508       BEGSR  sr_FldS01;
060200090505
060300090505         //?Tab. 5P/B = LdV?
060400090505         if  %subst( $5PB(xx) : 1 : 1 ) = *blank;
060500090505           §daM   = c_DspAtrNorm;
060600090505         else;
060700090505           S1Dann = 'Annullato';
060800090505           §daM   = c_DspAtrRI;
060900090505         endif;
061000090505         ds5PB  = %subst( $5PB(xx) : 2 );
061100090505         S1Cba4 = §5PBa4;
061200090505         S1Cba5 = §5PBa5;
061300090505         S1Cbpl = §5PBpl;
061400090505
061500090505         // -?Tab. 5P/O = O.R.M.?
061600090505         if  %subst( $5PO(xx) : 1 : 1 ) = *blank;
061700090505           §daO   = c_DspAtrNorm;
061800090505         else;
061900090505           S1Dann = 'Annullato';
062000090505           §daO   = c_DspAtrRI;
062100090505         endif;
062200090505         ds5P   = %subst( $5PO(xx) : 2 );
062300090505         S1Cooq = §5Potq;
062400090505         S1Copg = §5Ppgm;
062500090505
062600090505         // -?Tab. 5P/S = Etichette?
062700090505         if  %subst( $5PS(xx) : 1 : 1 ) = *blank;
062800090505           §daS   = c_DspAtrNorm;
062900090505         else;
063000090505           S1Dann = 'Annullato';
063100090505           §daS   = c_DspAtrRI;
063200090505         endif;
063300090505         ds5P   = %subst( $5PS(xx) : 2 );
063400090505         S1Csoq = §5Potq;
063500090505
063600090505         // -?Tab. 5P/T = Tabulati?
063700090505         if  %subst( $5PT(xx) : 1 : 1 ) = *blank;
063800090505           §daT   = c_DspAtrNorm;
063900090505         else;
064000090505           S1Dann = 'Annullato';
064100090505           §daT   = c_DspAtrRI;
064200090505         endif;
064300090505         ds5P   = %subst( $5PT(xx) : 2 );
064400090505         S1Ctoq = §5Potq;
064500090505
064600090505       ENDSR;
064700090508
064800090508       //--------------------------------------------------------------
064900090508       //?Controllo fattibilità del posizionamento nel subfile S01     ?
065000090508       //--------------------------------------------------------------
065100090508       //BEGSR  sr_CtrlPosizS01;
065200090508       //
065300090508       //  //?Ciclo di lettura subfile?
065400090508       //  readc  TB30S1;
065500090508       //
065600090508       //  DoW  Not %eof(TRTB30D);
065700090508       //
065800090508       //    // -?Trovata opzione nel subfile?
065900090508       //    if  S1Copz  <> *blank;
066000090508       //
066100090508       //      // -?Memorizzazione NRR Sfl-Rec con la 1ª opzione?
066200090508       //      ErrMessage  = *on;
066300090508       //      ErrGenerico = *on;
066400090508       //      PosCurOPZ   = *on;
066500090508       //      V1Dmsg = $Msg(20);
066600090508       //
066700090508       //      // -?Aggiornamento sfl?
066800090508       //      C1RcdNbr = S01nrr;
066900090508       //      SflNxtChg = *on;
067000090508       //      C1CsrRrn  = C1RcdNbr;
067100090508       //      UPDATE  TB30S1;
067200090508       //
067300090508       //      // -?Uscita dal ciclo?
067400090508       //      leavesr;
067500090508       //
067600090508       //    endif;
067700090508       //
067800090508       //    readc  TB30S1;
067900090508       //
068000090508       //  ENDDO;
068100090508       //
068200090508       //ENDSR;
068300090504
068400090504       //--------------------------------------------------------------
068500090504       //?Gestione opzioni del subfile S01                             ?
068600090504       //--------------------------------------------------------------
068700090504       BEGSR  sr_OpzS01;
068800090504
068900090504         clear  SavS01csr;
069000090504
069100090505         //?Ciclo di lettura subfile?
069200090504         readc  TB30S1;
069300090504
069400090504         DoW  Not %eof(TRTB30D);
069500090504
069600090505           SflNxtChg = *off;
069700090505           %subst(IndDspF : 50) = *off;
069800090505           C1RcdNbr = S01nrr;
069900090504
070000090504           select;
070100090505
070200090505             // -?2=Modifica?
070300090504             when  S1Copz = '2';
070400090504               V1Topz = '   MODIFICA    ';
070500090504               $Video  = 'D2';
070600090504               $InzD02 = *on;
070700090505
070800090505             // -?3=Copia?
070900090504             when  S1Copz = '3';
071000090504               V1Topz = '     COPIA     ';
071100090504               $Video  = 'D2';
071200090504               $InzD02 = *on;
071300090505
071400090505             // -?4=Annullamento/Ripristino?
071500090504             when  S1Copz = '4';
071600090504               V1Topz = ' ANNULLAMENTO  ';
071700090504               $Video  = 'D2';
071800090504               $InzD02 = *on;
071900090505
072000090505             // -?5=Visualizzazione?
072100090504             when  S1Copz = '5';
072200090504               V1Topz = 'VISUALIZZAZIONE';
072300090504               $Video  = 'D2';
072400090504               $InzD02 = *on;
072500090505
072600090505             // -?Nessuna opzione?
072700090504             when  S1Copz = *blank;
072800090505
072900090505             // -?Opzione errata?
073000090504             other;
073100090504               ErrMessage  = *on;
073200090504               ErrGenerico = *on;
073300090504               PosCurOPZ   = *on;
073400090504               V1Dmsg = $Msg(01);
073500090505
073600090504           endsl;
073700090505
073800090505           // -?Gestione opzione richiesta (se valida)?
073900090505           dow  $Video = 'D2';
074000090505             exsr  sr_GesD02;
074100090505           enddo;
074200090504
074300090505           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
074400090505           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
074500090504           if  S1Copz  <> *blank   and
074600090504              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
074700090504             SavS01csr   = C1RcdNbr;
074800090504           endif;
074900090504
075000090505           // -?Aggiornamento sfl?
075100090504           select;
075200090504             when  ErrMessage;
075300090504               SflNxtChg = *on;
075400090504               C1CsrRrn  = C1RcdNbr;
075500090504             when  ErrGenerico;
075600090504               C1CsrRrn  = C1RcdNbr;
075700090504               clear  S1Copz;
075800090504             other;
075900090504               clear  S1Copz;
076000090504           endsl;
076100090504           UPDATE  TB30S1;
076200090504           if  ErrGenerico   or   ErrMessage;
076300090504             leave;
076400090504           endif;
076500090504
076600090504           readc  TB30S1;
076700090504
076800090504         ENDDO;
076900090504
077000090505         // -?Riposiziono il cursore sul record contenente la prima opz.?
077100090505         //  ?(se non sono stati rilevati errori)?
077200090504         if  NOT ErrMessage   and   NOT ErrGenerico   and
077300090504             SavS01csr > *zero;
077400090504           C1CsrRrn = SavS01csr;
077500090504         endif;
077600090504
077700090504       ENDSR;
077800090427
077900090427       //--------------------------------------------------------------
078000090504       //?Controllo Filiale                                            ?
078100090427       //--------------------------------------------------------------
078200090504       BEGSR  sr_CtrFil;
078300090427
078400090504         clear  WWDfil;
078500090504         SELECT;
078600090504
078700090504           //?Ricerca codice filiale?
078800090504           WHEN  %scan('?' : WWCfil) > *zero;
078900090504             X§Tkut = c_Kut;
079000090504             X§Tcod = c_Tab;
079100090504             clear  X§Tkey;
079200090504             clear  X§Tdes;
079300090504             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
079400090504             WWCfil = %subst(X§Tkey : 2 : %len(WWCfil));
079500090504             ErrGenerico = *on;
079600090427
079700090427           //?Controllo immissione codice filiale?
079800090504           WHEN  WWCfil = *blank  or  WWCfil  = *zero;
079900090504             ErrMessage  = *on;
080000090504             ErrGenerico = *on;
080100090504             PosCurOPZ   = *on;
080200090504             V1Dmsg = $Msg(02);
080300090504             leavesr;
080400090504
080500090504         ENDSL;
080600090427
080700090427         //?Controllo/decodifica filiale?
080800090504         IF  WWCfil <> c_Edp;
080900090428
081000090428           // - Controllo immissione solo caratteri numerici
081100090504           if  %check(c_Digit : WWCfil) > *zero;
081200090428             ErrMessage  = *on;
081300090428             ErrGenerico = *on;
081400090504             PosCurOPZ   = *on;
081500090504             V1Dmsg = $Msg(03);
081600090428             leavesr;
081700090428           endif;
081800090428
081900090428           // - Reperimento dati anagrafici
082000090504           ORGfil = %int(WWCfil);
082100090427           chain  (ORGfil)  AZORG;
082200090427           if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
082300090504             V1Dmsg = $Msg(04);
082400090427             ErrMessage  = *on;
082500090427             ErrGenerico = *on;
082600090504             PosCurOPZ   = *on;
082700090427             leavesr;
082800090427           endif;
082900090504           WWDfil = ORGdes;
083000090428
083100090427         ENDIF;
083200090427
083300090427       ENDSR;
083400090504
083500090504       //--------------------------------------------------------------
083600090504       //?Gestione subfile S02 - Visualizzazione pgm x stampa etichette?
083700090504       //--------------------------------------------------------------
083800090504       BEGSR  sr_GesS02;
083900090504
084000090504         //?Inizializzazione subfile?
084100090504         if  $InzS02 = *on;
084200090504           exsr sr_InzS02;
084300090504           $InzS02 = *off;
084400090504         endif;
084500090504
084600090504         //?Emissione Testata e Piede con tasti funzionali abilitati?
084700090504         if  errGenerico = *off;
084800090504           write  TB30T1;
084900090504           write  TB30P2;
085000090504         endif;
085100090504
085200090504         //?Posizionamento cursore?
085300090504         if  C1CsrRrn > *zero;
085400090504           C1RcdNbr = C1CsrRrn;
085500090504         else;
085600090504           C1RcdNbr = 1;
085700090504         endif;
085800090504
085900090504         //?Emissione videata?
086000090504         exfmt  TB30C2;
086100090504         reset  ErrMessage;
086200090504         reset  ErrGenerico;
086300090504         clear  V1Dmsg;
086400090504
086500090504         SELECT;
086600090504
086700090504           //?F3=Fine?
086800090504           WHEN  dsp_aid = c_F03;
086900090504             $Fine = *on;
087000090504
087100090504           //?F12=Ritorno?
087200090504           WHEN  dsp_aid = c_F12;
087300090504             $Video = 'S1';
087400090504
087500090504           //?Roll-Up?
087600090504           WHEN  dsp_aid = c_RollUp;
087700090504             exsr sr_RollUpS02;
087800090504
087900090504           //?Invio (non fa nulla)?
088000090504           OTHER;
088100090504
088200090504         ENDSL;
088300090504
088400090504       ENDSR;
088500090504
088600090504       //--------------------------------------------------------------
088700090504       //?Inizializzazione subfile S02                                 ?
088800090504       //--------------------------------------------------------------
088900090504       BEGSR  sr_InzS02;
089000090504
089100090504         //?Spegnimento degli indicatori di errore?
089200090506         %subst(IndDspF : 50) = *off;
089300090504
089400090504         //?Pulizia subfile?
089500090504         SflDsp2_N    = *on;
089600090504         SflDspCtl2_N = *on;
089700090504         write  TB30C2;
089800090504         SflDspCtl2_N = *off;
089900090504         SflEnd2      = *off;
090000090504
090100111006         clear  W_SflPag2;
090200090504         clear  C2RcdNbr;
090300090504         clear  C2CsrRrn;
090400090504         clear  S02nrr;
090500090504         clear  V1Dmsg;
090600090504         ErrMessage  = *off;
090700090504         ErrGenerico = *off;
090800090504
090900090504         //?Caricamento tab. 5P/M per la filiale?
091000090504         k_TBLkey = 'M';
091100090504         setll  %kds(k03tabel00)  TABEL;
091200090504         reade(n)  %kds(k03tabel00 : 2)  TABEL;
091300090504         exsr  sr_RollUpS02;
091400090504
091500090504         //?Impaginazione subfile?
091600090504         // -> forza l'impaginazione sul 1° rec. del subfile
091700090504         if S02nrr > *zero;
091800090504           C2RcdNbr  = 1;
091900090504           C2CsrRrn  = 1;
092000090504         else;
092100090504           clear C2RcdNbr;
092200090504         endif;
092300090504
092400090504       ENDSR;
092500090504
092600090504       //--------------------------------------------------------------
092700090504       //?Caricamento singola pagina nel subfile S02                   ?
092800090504       //--------------------------------------------------------------
092900090504       BEGSR  sr_RollUpS02;
093000090504
093100090504         S02nrr     = (W_SflPag2 * c_SflPag2);
093200090504         W_SflPag2 += 1;
093300090504         SflNxtChg2 = *off;
093400090504
093500090504         clear  ds5PM;
093600090504
093700090505         //?Ciclo di caricamento dati per tab. 5P/M?
093800090504         DoW  S02nrr < (W_SflPag2 * c_SflPag2)   and
093900090504              S02nrr < c_MaxRec                  and
094000090504              Not %eof(TABEL00F)                 and
094100090504              %subst(TBLkey : 1 : 1) = 'M';
094200090504
094300090504           ds5PM = TBLuni;
094400090504           clear  TB30S2;
094500090504           S2Ckey  = %subst(TBLkey : 2);
094600090504           S2Cdes  = %subst(TBLuni : 52 : %len(S2Cdes));
094700090504           S2Cpss  = §5PPss;
094800090504           S2Cpsb  = §5PPsb;
094900090504           S2Cpsd  = §5PPsd;
095000090504           S2Cpsf  = §5PPsf;
095100090504
095200090504           S02nrr += 1;
095300090504           write  TB30S2;
095400090504
095500090504           reade(n)  %kds(k03tabel00 : 2)  TABEL;
095600090504
095700090504         EndDo;
095800090504
095900090504         //?Visualizzazione del SFL (se ci sono dati)?
096000090504         SflDsp2_N  = (S02nrr <= *zero);
096100090504
096200090504         //?Attivazione del SFLEND?
096300090504         SflEnd2 = (%eof(TABEL00F)  or  S02nrr >= c_MaxRec  or
096400090504                    %subst(TBLkey : 1 : 1) <> 'M');
096500090504
096600090504         //?Posizionamento cursore al 1° rcd della pagina?
096700090504         if  S02nrr > *zero;
096800090504           wPag     = S02nrr / c_SflPag2;
096900090504           wRig     = S02nrr - (c_SflPag2 * wPag);
097000090504           C2RcdNbr = wPag * c_SflPag2;
097100090504           if  wRig > *zero;
097200090504             C2RcdNbr += 1;
097300090504           else;
097400090504             C2RcdNbr = C2RcdNbr - c_SflPag2 + 1;
097500090504           endif;
097600090504         else;
097700090504           clear  C2RcdNbr;
097800090504         endif;
097900090504
098000090504         C2CsrRrn = C2RcdNbr;
098100090504
098200090504       ENDSR;
098300090427
098400090427       //--------------------------------------------------------------
098500090504       //?Gestione videata D02 - Dati tab. "5P" / "B", "O", "S" e "T"  ?
098600090427       //--------------------------------------------------------------
098700090427       BEGSR  sr_GesD02;
098800090427
098900090427         //?Inizializzazione videata?
099000090427         if  $InzD02 = *on;
099100090427           exsr sr_InzD02;
099200090427           $InzD02 = *off;
099300090427         endif;
099400090427
099500090427         //?Emissione Testata e Piede con tasti funzionali abilitati?
099600090427         if  ErrGenerico = *off;
099700090427           write  TB30T1;
099800090504           if  S1Copz = '5';
099900090504             write  TB30P2;
100000090504           else;
100100090504             write  TB30P3;
100200090504           endif;
100300090427         endif;
100400090427
100500090427         //?Emissione videata?
100600090504         if  S1Copz = '5';
100700090504           write  TB30D2;
100800090504           exfmt  Protect;
100900090504         else;
101000090504           exfmt  TB30D2;
101100090504         endif;
101200090504
101300090427         reset  ErrMessage;
101400090427         reset  ErrGenerico;
101500090427         clear  V1Dmsg;
101600090427         SELECT;
101700090427
101800090427           //?F3=Fine?
101900090427           WHEN  dsp_aid = c_F03;
102000090427             unlock TABEL00F;
102100090504             reset  $Video;
102200090504             ErrGenerico = *on;
102300090427
102400090427           //?F12=Ritorno?
102500090427           WHEN  dsp_aid = c_F12;
102600090427             unlock TABEL00F;
102700090427             reset  $Video;
102800090505
102900090505           //?F23=Sblocco dei campi con i pgm di stampa?
103000090505           WHEN  dsp_aid = c_F23;
103100090505             $F23 = *on;
103200090505             exsr  sr_SetIndD02;
103300090427
103400090428           //?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
103500090428           OTHER;
103600090505             //?Nessun controllo dati per:?
103700090505             //  ?· F16=Annullamento?
103800090505             //  ?· Opz. 4=Annullamento (sì controlli al Ripristino)?
103900090505             //  ?· Opz. 5=Visualizzazione?
104000090504             // - Controllo chiave di accesso solo per copia/inserim.
104100090505             if  S1Copz = '3'   or
104200090505                 %trim(V1Topz) = c_Insert;
104300090505               exsr  sr_CtrCpyIns;
104400090504               if  ErrGenerico = *on;
104500090504                 leavesr;
104600090504               endif;
104700090504             endif;
104800090504             // - Controllo altri dati se non richiesto annullamento
104900090504             if  dsp_aid <> c_F16   and
105000090505                 S1Copz  <> '5'     and
105100090505                (S1Copz  <> '4'     or    S1Dann <> *blank);
105200090428               exsr  sr_CtrD02;
105300090428               if  ErrGenerico = *on;
105400090428                 leavesr;
105500090428               endif;
105600090428             endif;
105700090427
105800090427             //?F5=Ripristino; F6=Conferma; F16=Annullamento?
105900090427             if  dsp_aid = c_F05  or
106000090427                 dsp_aid = c_F06  or
106100090427                 dsp_aid = c_F16;
106200090427               exsr  sr_UpdTABEL;
106300090505               reset $Video;
106400090427             endif;
106500090427
106600090427         ENDSL;
106700090505
106800090505         //?Pulizia del sotto-titolo in testata al ritorno nel sfl S01?
106900090505         if  $Video <> 'D2';
107000090505           clear  V1Topz;
107100090505         endif;
107200090427
107300090427       ENDSR;
107400090427
107500090427       //--------------------------------------------------------------
107600090504       //?Inizializzazione videata D02                                 ?
107700090427       //--------------------------------------------------------------
107800090427       BEGSR  sr_InzD02;
107900090427
108000090427         //?Spegnimento degli indicatori di errore?
108100090506         %subst(IndDspF : 50) = *off;
108200090505
108300090505         //?Spegnimento indicatore per la protezione dei campi con i?
108400090505         //  ?pgm di stampa?
108500090505         //reset  $F23;
108600090505         //?Protezione dei campi con i pgm di stampa SE NON inserimento?
108700090505         $F23 = (dsp_aid = c_F06);
108800090428
108900090504         //?Pulizia campi di salvataggio per schivare controlli?
109000090428         clear  SaveBA4;
109100090428         clear  SaveBA5;
109200090428         clear  SaveBPL;
109300090428         clear  SaveOTQo;
109400090428         clear  SavePGMo;
109500090428         clear  SaveOTQs;
109600090428         clear  SaveOTQt;
109700090428
109800090427         //?Pulizia videata?
109900090427         clear  TB30D2;
110000090427
110100090427         //?Caricamento dati per la filiale?
110200090504         if  dsp_aid <> c_F06;
110300090504           exsr  sr_RieD02;
110400090504         endif;
110500090427
110600090505         //?Impostazione tipo manutenzione per opzione 4=Ripristino?
110700090505         if  S1Copz = '4'   and   $RecAnn = *on;
110800090505           V1Topz = '  RIPRISTINO   ';
110900090505         endif;
111000090427
111100090427         //?Impostazione indicatori per abilitazione tasti funzionali?
111200090505         exsr  sr_SetIndD02;
111300090427
111400090427       ENDSR;
111500090504
111600090504       //--------------------------------------------------------------
111700090504       //?Caricamento dati nella videata D02                           ?
111800090504       //--------------------------------------------------------------
111900090504       BEGSR  sr_RieD02;
112000090505
112100090505         //?Resettaggio del flag che indica se TUTTI i rec. risultano?
112200090505         //  ?annullati?
112300090505         $RecAnn = *on;
112400090504
112500090505         //?Impostazione filiale SE NON "copia"?
112600090504         if  S1Copz <> '3';
112700090504           V2Cfil = S1Cfil;
112800090504           WWCfil = V2Cfil;
112900090504           exsr  sr_CtrFil;
113000090504           V2Dfil = WWDfil;
113100090505           ErrMessage = *off;
113200090505           %subst(IndDspF : 50) = *off;
113300090505           clear  V1Dmsg;
113400090504         endif;
113500090504
113600090505         //?Tab. 5P/B = LdV?
113700090504         clear  ds5PB;
113800090504         k_TBLkey = 'B' + S1Cfil;
113900090504         if  S1Copz = '3'   or
114000090504             S1Copz = '5';
114100090504           chain(n)  %kds(k03tabel00)  TABEL;
114200090504         else;
114300090504           chain  %kds(k03tabel00)  TABEL;
114400090504         endif;
114500090504         if  %found(TABEL00F);
114600090504           ds5PB = TBLuni;
114700090505           if  TBLflg = *blank   or   S1Copz = '3';
114800090504             reset  $RecAnn;
114900090504           else;
115000090504             V2Canb = 'ANNULLATA ';
115100090504           endif;
115200090504         endif;
115300090504
115400090504         V2Hbol  = §5PBol;
115500090504         V2Hbts  = §5PBts;
115600090504         V2Hbpg  = §5PBpg;
115700090504         V2Cba4  = §5PBa4;
115800090504         V2Cba5  = §5PBa5;
115900090504         V2Cbpl  = §5PBpl;
116000090504         V2Cbus  = §5PBus;
116100090504
116200090505         //?Tab. 5P/O = O.R.M.?
116300090504         clear  ds5P;
116400090504         k_TBLkey = 'O' + S1Cfil;
116500090504         if  S1Copz = '3'   or
116600090504             S1Copz = '5';
116700090504           chain(n)  %kds(k03tabel00)  TABEL;
116800090504         else;
116900090504           chain  %kds(k03tabel00)  TABEL;
117000090504         endif;
117100090504         if  %found(TABEL00F);
117200090504           ds5P = TBLuni;
117300090505           if  TBLflg = *blank   or   S1Copz = '3';
117400090504             reset  $RecAnn;
117500090504           else;
117600090504             V2Cano = 'ANNULLATA ';
117700090504           endif;
117800090504         endif;
117900090504
118000090504         V2CotqO = §5Potq;
118100090504         V2CpgmO = §5Ppgm;
118200090504
118300090505         //?Tab. 5P/S = Etichette?
118400090504         clear  ds5P;
118500090504         k_TBLkey = 'S' + S1Cfil;
118600090504         if  S1Copz = '3'   or
118700090504             S1Copz = '5';
118800090504           chain(n)  %kds(k03tabel00)  TABEL;
118900090504         else;
119000090504           chain  %kds(k03tabel00)  TABEL;
119100090504         endif;
119200090504         if  %found(TABEL00F);
119300090504           ds5P = TBLuni;
119400090505           if  TBLflg = *blank   or   S1Copz = '3';
119500090504             reset  $RecAnn;
119600090504           else;
119700090504             V2Cans = 'ANNULLATA ';
119800090504           endif;
119900090504         endif;
120000090504
120100090504         V2CotqS = §5Potq;
120200090504         V2CmodS = §5Pmod;
120300090504
120400090505         //?Tab. 5P/T = Tabulati?
120500090504         clear  ds5P;
120600090504         k_TBLkey = 'T' + S1Cfil;
120700090504         if  S1Copz = '3'   or
120800090504             S1Copz = '5';
120900090504           chain(n)  %kds(k03tabel00)  TABEL;
121000090504         else;
121100090504           chain  %kds(k03tabel00)  TABEL;
121200090504         endif;
121300090504         if  %found(TABEL00F);
121400090504           ds5P = TBLuni;
121500090505           if  TBLflg = *blank   or   S1Copz = '3';
121600090504             reset  $RecAnn;
121700090504           else;
121800090504             V2Cant = 'ANNULLATA ';
121900090504           endif;
122000090504         endif;
122100090504
122200090504         V2CotqT = §5Potq;
122300090504
122400090505         //?Decodifica modello stampante etichette?
122500090504         exsr  sr_Decod5PM;
122600090504
122700090504       ENDSR;
122800090427
122900090427       //--------------------------------------------------------------
123000090427       //?Settaggio indicatori per abilitazione tasti funzionali nella ?
123100090504       //?  videata D02                                                ?
123200090427       //--------------------------------------------------------------
123300090505       BEGSR  sr_SetIndD02;
123400090427
123500090504         // Protezione campi chiave
123600090505         ProtectFIL = ( S1Copz  <> '3'   and
123700090505                  %trim(V1Topz) <> c_Insert );
123800090505
123900090505         // Protezione campi con programma di stampa
124000090505         ProtectPGM = ( Not $F23 );
124100090504
124200090427         // F5=Ripristino
124300090505         F5Attivo   =  ( S1Copz = '2'   and   $RecAnn = *on );
124400090427
124500090427         // F6=Conferma
124600090505         F6Attivo   = (NOT F5Attivo);
124700090427
124800090427         // F16=Annullamento
124900090505         F16Attivo  = ( S1Copz = '2'   and   $RecAnn = *off );
125000090505
125100090505         // F23=Sblocco campi con pgm di stampa
125200090505         F23Attivo  = ( Not $F23   and   SDSusr = 'EDPSM     ' );
125300090427
125400090427       ENDSR;
125500090504
125600090504       //--------------------------------------------------------------
125700090505       //?Controllo dati immessi nella videata D02 x 3=Copia/F6=Inser. ?
125800090504       //--------------------------------------------------------------
125900090505       BEGSR  sr_CtrCpyIns;
126000090504
126100090504         //?Verifica esistenza records per tale filiale in tab. "5P"?
126200090504         // - 1ª verifica in tab. 5P/B
126300090504         k_TBLkey = 'B' + V2Cfil;
126400090504         setll  %kds(k03tabel00)  TABEL;
126500090504         if  Not %equal(TABEL00F);
126600090504           // - 2ª verifica in tab. 5P/O
126700090504           k_TBLkey = 'O' + V2Cfil;
126800090504           setll  %kds(k03tabel00)  TABEL;
126900090504           if  Not %equal(TABEL00F);
127000090504             // - 3ª verifica in tab. 5P/S
127100090504             k_TBLkey = 'S' + V2Cfil;
127200090504             setll  %kds(k03tabel00)  TABEL;
127300090504             if  Not %equal(TABEL00F);
127400090504               // - 4ª verifica in tab. 5P/S
127500090504               k_TBLkey = 'T' + V2Cfil;
127600090504               setll  %kds(k03tabel00)  TABEL;
127700090504             endif;
127800090504           endif;
127900090504         endif;
128000090504         if  %equal(TABEL00F);
128100090504           ErrMessage  = *on;
128200090504           ErrGenerico = *on;
128300090504           PosCurOPZ   = *on;
128400090504           V1Dmsg = %trimr($Msg(05)) + %subst(k_TBLkey : 1 : 1);
128500090504           leavesr;
128600090504         endif;
128700090504
128800090504       ENDSR;
128900090427
129000090427       //--------------------------------------------------------------
129100090504       //?Controllo dati immessi nella videata D02                     ?
129200090427       //--------------------------------------------------------------
129300090427       BEGSR  sr_CtrD02;
129400090427
129500090427         %subst(IndDspF : 50) = *off;
129600090505
129700090505         //?Controllo codice filiale?
129800090505         clear  V2Dfil;
129900090505         WWCfil = V2Cfil;
130000090505         exsr  sr_CtrFil;
130100090505         V2Dfil = WWDfil;
130200090505         if  ErrGenerico = *on;
130300090505           leavesr;
130400090505         endif;
130500090505
130600090505         //?Non consentita la cancellazione di tutte le outq nelle?
130700090505         //  ?varie tabelle 5P/x - SE NON con l'opz.4?
130800090505         if  S1Copz <> '4'      and
130900090505             V2Cba4  = *blank   and   V2Cba5 = *blank   and
131000090505             V2CotqO = *blank   and
131100090505             V2CotqS = *blank   and
131200090505             V2CotqT = *blank;
131300090505           ErrMessage  = *on;
131400090505           ErrGenerico = *on;
131500090505           PosCurBA4   = *on;
131600090505           if  %trim(V1Topz) = c_Insert;
131700090505             V1Dmsg = %subst( $Msg(17) : 1 : 37 );
131800090505           else;
131900090505             V1Dmsg = $Msg(17);
132000090505           endif;
132100090505           leavesr;
132200090505         endif;
132300090428
132400090428         //?Controlli per stampa LdV (tab. 5P/B)?
132500090505
132600090505         // -?Programma di stampa (campo protetto) da eliminare?
132700090505         if  V2Cbpl <> *blank   and
132800090505            (V2Cba4 =  *blank   and  V2Cba5 = *blank)   and
132900090505             ProtectPGM;
133000090505           clear  V2Cbpl;
133100090505           clear  V2Cbus;
133200090505         endif;
133300090428
133400090505         // -?Controllo code di output per stampanti laser?
133500090505         select;
133600090505           when  V2Cbpl <> *blank   and
133700090505                 V2Cbus =  *blank   and
133800090505                (V2Cba4 =  *blank   or   V2Cba5 = *blank);
133900090505             ErrMessage  = *on;
134000090505             ErrGenerico = *on;
134100090505             if  V2Cba4 = *blank;
134200090505               PosCurBA4 = *on;
134300090505             else;
134400090505               PosCurBA5 = *on;
134500090505             endif;
134600090505             V1Dmsg = $Msg(07);
134700090505             leavesr;
134800090505           when  V2Cbpl <> *blank   and
134900090505                 V2Cbus =  '4'      and
135000090505                 V2Cba4 =  *blank;
135100090505             ErrMessage  = *on;
135200090505             ErrGenerico = *on;
135300090505             PosCurBA4   = *on;
135400090505             V1Dmsg = %trimr( $Msg(18) )  + ' A4';
135500090505             leavesr;
135600090505           when  V2Cbpl <> *blank   and
135700090505                 V2Cbus =  '5'      and
135800090505                 V2Cba5 =  *blank;
135900090505             ErrMessage  = *on;
136000090505             ErrGenerico = *on;
136100090505             PosCurBA5   = *on;
136200090505             V1Dmsg = %trimr( $Msg(18) )  + ' A5';
136300090505             leavesr;
136400090505         endsl;
136500090430
136600090428         if  V2Cba4 <> *blank   and   V2Cba4 <> SaveBA4;
136700090428           SaveBA4 = V2Cba4;
136800090428           clear  wErr;
136900090428           trul08c (V2Cba4 : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
137000090428           if  wErr = 'P';
137100090428             ErrMessage  = *on;
137200090428             ErrGenerico = *on;
137300090428             PosCurBA4   = *on;
137400090504             V1Dmsg = $Msg(06);
137500090428             leavesr;
137600090428           endif;
137700090428         endif;
137800090428
137900090428         if  V2Cba5 <> *blank   and   V2Cba5 <> SaveBA5;
138000090428           SaveBA5 = V2Cba5;
138100090428           clear  wErr;
138200090428           trul08c (V2Cba5 : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
138300090428           if  wErr = 'P';
138400090428             ErrMessage  = *on;
138500090428             ErrGenerico = *on;
138600090428             PosCurBA5   = *on;
138700090504             V1Dmsg = $Msg(06);
138800090428             leavesr;
138900090428           endif;
139000090428         endif;
139100090428
139200090505         // -?Controllo programma di stampa su stampante laser?
139300090428         select;
139400090505           when (V2Cba4 <> *blank         or
139500090505                 V2Cba5 <> *blank)        and
139600090505                 V2Cbpl =  *blank;
139700090505             ErrMessage  = *on;
139800090505             ErrGenerico = *on;
139900090505             PosCurBPL   = *on;
140000090505             V1Dmsg = $Msg(08);
140100090505             leavesr;
140200090505           when  V2Cbpl <> *blank         and
140300090505                 V2Cbpl <> 'FNLSB5R   '   and
140400090505                 V2Cbpl <> 'FNLSB6R   ';
140500090428             ErrMessage  = *on;
140600090428             ErrGenerico = *on;
140700090428             PosCurBPL   = *on;
140800090504             V1Dmsg = %trimr($Msg(09)) + ' "FNLSB5R" o "FNLSB6R"';
140900090428             leavesr;
141000090428         endsl;
141100090428
141200090505         if  V2Cbpl <> *blank   and   V2Cbpl <> SaveBPL;
141300090428           SaveBPL = V2Cbpl;
141400090428           clear  wErr;
141500090428           trul08c (V2Cbpl : '*LIBL' : '*PGM' : '*NONE' : wErr);
141600090428           if  wErr = 'P';
141700090428             ErrMessage  = *on;
141800090428             ErrGenerico = *on;
141900090428             PosCurBPL   = *on;
142000090504             V1Dmsg = $Msg(10);
142100090428             leavesr;
142200090428           endif;
142300090428         endif;
142400090428
142500090428         // -?Controllo richiesta stampante unica?
142600090428         select;
142700090428           when  V2Cbus <> *blank   and
142800090428                 V2Cbus <> '4'      and
142900090428                 V2Cbus <> '5';
143000090428             ErrMessage  = *on;
143100090428             ErrGenerico = *on;
143200090428             PosCurBUS   = *on;
143300090504             V1Dmsg = $Msg(11);
143400090428             leavesr;
143500090505           when  V2Cbpl =  'FNLSB5R   '   and
143600090505                 V2Cbus <> *blank;
143700090505             ErrMessage  = *on;
143800090505             ErrGenerico = *on;
143900090505             PosCurBUS   = *on;
144000090505             V1Dmsg = $Msg(19);
144100090505             leavesr;
144200090505           when  V2Cbpl =  'FNLSB6R   '   and
144300090505                 V2Cbus =  *blank;
144400090505             ErrMessage  = *on;
144500090505             ErrGenerico = *on;
144600090505             PosCurBUS   = *on;
144700090505             V1Dmsg = $Msg(19);
144800090505             leavesr;
144900090428         endsl;
145000090428
145100090428         //?Controlli per stampa O.R.M. (tab. 5P/O)?
145200090505
145300090505         // -?Programma di stampa (campo protetto) da eliminare?
145400090505         if  V2CpgmO <> *blank   and
145500090505             V2CotqO =  *blank   and
145600090505             ProtectPGM;
145700090505           clear  V2CpgmO;
145800090505         endif;
145900090428
146000090428         // -?Controllo code di output?
146100090428         if  V2CotqO <> *blank   and   V2CotqO <> SaveOTQo;
146200090428           SaveOTQo = V2CotqO;
146300090428           clear  wErr;
146400090428           trul08c (V2CotqO : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
146500090428           if  wErr = 'P';
146600090428             ErrMessage  = *on;
146700090428             ErrGenerico = *on;
146800090428             PosCurOTQO  = *on;
146900090504             V1Dmsg = $Msg(06);
147000090428             leavesr;
147100090428           endif;
147200090428         endif;
147300090428
147400090428         // -?Controllo programma di stampa?
147500090505         select;
147600090505           when  V2CotqO =  *blank   and
147700090505                 V2CpgmO <> *blank;
147800090505             ErrMessage  = *on;
147900090505             ErrGenerico = *on;
148000090505             PosCurBPL   = *on;
148100090505             V1Dmsg = $Msg(12);
148200090505             leavesr;
148300090505           when  V2CotqO <> *blank   and
148400090505                 V2CpgmO =  *blank;
148500090505             ErrMessage  = *on;
148600090505             ErrGenerico = *on;
148700090505             PosCurBPL   = *on;
148800090505             V1Dmsg = $Msg(13);
148900090505             leavesr;
149000090505           when  V2CpgmO <> *blank         and
149100090505                 V2CpgmO <> 'FIOR16R   '   and
149200090505                 V2CpgmO <> 'FIOR18R   ';
149300090505             ErrMessage  = *on;
149400090505             ErrGenerico = *on;
149500090505             PosCurBPL   = *on;
149600090505             V1Dmsg = %trimr($Msg(09)) + ' "FIOR16R" o "FIOR18R"';
149700090505             leavesr;
149800090505         endsl;
149900090505
150000090505         if  V2CpgmO <> *blank   and   V2CpgmO <> SavePGMo;
150100090505           SavePGMo = V2CpgmO;
150200090505           clear  wErr;
150300090505           trul08c (V2CpgmO : '*LIBL' : '*PGM' : '*NONE' : wErr);
150400090505           if  wErr = 'P';
150500090505             ErrMessage  = *on;
150600090505             ErrGenerico = *on;
150700090505             PosCurPGMO  = *on;
150800090505             V1Dmsg = $Msg(10);
150900090505             leavesr;
151000090505           endif;
151100090505         endif;
151200090428
151300090428         // -?Verifica compatibilità dati inseriti?
151400090428         if  (V2CotqO <> *blank   and   V2CpgmO =  *blank)   or
151500090428             (V2CotqO =  *blank   and   V2CpgmO <> *blank);
151600090428           ErrMessage  = *on;
151700090428           ErrGenerico = *on;
151800090428           if  V2CotqO = *blank;
151900090428             PosCurOTQO  = *on;
152000090504             V1Dmsg = $Msg(12);
152100090428           else;
152200090428             PosCurPGMO  = *on;
152300090504             V1Dmsg = $Msg(13);
152400090428           endif;
152500090428           leavesr;
152600090428         endif;
152700090428
152800090428         //?Controlli per stampa Etichette (tab. 5P/S)?
152900090428
153000090428         // -?Controllo code di output?
153100090428         if  V2CotqS <> *blank   and   V2CotqS <> SaveOTQs;
153200090428           SaveOTQs = V2CotqS;
153300090428           clear  wErr;
153400090428           trul08c (V2CotqS : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
153500090428           if  wErr = 'P';
153600090428             ErrMessage  = *on;
153700090428             ErrGenerico = *on;
153800090428             PosCurOTQS  = *on;
153900090504             V1Dmsg = $Msg(06);
154000090428             leavesr;
154100090428           endif;
154200090428         endif;
154300090428
154400090428         // -?Verifica correttezza modello stampante?
154500090505         if  V2CotqS <> *blank   or   V2CmodS <> *blank;
154600090505           clear  V2DmodS;
154700090505           exsr  sr_Decod5PM;
154800090505           if  Not %found(TABEL00F);
154900090505             ErrMessage  = *on;
155000090505             ErrGenerico = *on;
155100090505             PosCurMODS  = *on;
155200090505             V1Dmsg = $Msg(14);
155300090505             leavesr;
155400090505           endif;
155500090428         endif;
155600090428
155700090428         // -?Verifica compatibilità dati inseriti?
155800090428         if  (V2CotqS <> *blank   and   V2CmodS =  *blank)   or
155900090428             (V2CotqS =  *blank   and   V2CmodS <> *blank);
156000090428           ErrMessage  = *on;
156100090428           ErrGenerico = *on;
156200090428           if  V2CotqS = *blank;
156300090428             PosCurOTQS  = *on;
156400090504             V1Dmsg = $Msg(15);
156500090428           else;
156600090428             PosCurMODS  = *on;
156700090504             V1Dmsg = $Msg(16);
156800090428           endif;
156900090428           leavesr;
157000090428         endif;
157100090428
157200090428         //?Controlli per stampa Tabulati (tab. 5P/T)?
157300090428
157400090428         // -?Controllo code di output?
157500090428         if  V2CotqT <> *blank   and   V2CotqT <> SaveOTQt;
157600090428           SaveOTQt = V2CotqT;
157700090428           clear  wErr;
157800090428           trul08c (V2CotqT : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
157900090428           if  wErr = 'P';
158000090428             ErrMessage  = *on;
158100090428             ErrGenerico = *on;
158200090428             PosCurOTQT  = *on;
158300090504             V1Dmsg = $Msg(06);
158400090428             leavesr;
158500090428           endif;
158600090428         endif;
158700090427
158800090427       ENDSR;
158900090427
159000090504       //--------------------------------------------------------------
159100090504       //?Aggiornamento dati in tab. "5P" / "B", "O", "S", "T"         ?
159200090504       //--------------------------------------------------------------
159300090504       BEGSR  sr_Decod5PM;
159400090504
159500090504         k_TBLkey = 'M' + V2CmodS;
159600090504         chain(n)  %kds(k03tabel00)  TABEL;
159700090504         if  %found(TABEL00F);
159800090504           V2DmodS = %subst(TBLuni : 52 : %len(V2DmodS));
159900090504         endif;
160000090504
160100090504       ENDSR;
160200090504
160300090427       //--------------------------------------------------------------
160400090427       //?Aggiornamento dati in tab. "5P" / "B", "O", "S", "T"         ?
160500090427       //--------------------------------------------------------------
160600090427       BEGSR  sr_UpdTABEL;
160700090427
160800090505         //?Reperimento elemento di schiera da aggiornare?
160900090505         xx = %lookup( V2Cfil : $Fil );
161000090505
161100090505
161200090505         //?Aggiornamento tabelle "5P" (file TABEL00F e schiere $5Px)?
161300090505
161400090427         // -?Tab. 5P/B = LdV?
161500090427         clear  ds5PB;
161600090430         §5PBol  = V2Hbol;
161700090427         §5PBa4  = V2Cba4;
161800090427         §5PBa5  = V2Cba5;
161900090508         if  V2Cbpl <> *blank;
162000090505           §5PBts  = 'S';
162100090505         endif;
162200090430         §5PBpg  = V2Hbpg;
162300090427         §5PBpl  = V2Cbpl;
162400090427         §5PBus  = V2Cbus;
162500090427
162600090504         k_TBLkey = 'B' + V2Cfil;
162700090427         chain  %kds(k03tabel00)  TABEL;
162800090427         TBLuni = ds5PB;
162900090427         exsr  sr_UpdTABEL_b;
163000090505
163100090505         if  xx > *zero;
163200090505           $5PB(xx) = TBLflg + TBLuni;
163300090505         endif;
163400090427
163500090427         // -?Tab. 5P/O = O.R.M.?
163600090427         clear  ds5P;
163700090427         §5Potq = V2CotqO;
163800090427         §5Ppgm = V2CpgmO;
163900090427
164000090504         k_TBLkey = 'O' + V2Cfil;
164100090427         chain  %kds(k03tabel00)  TABEL;
164200090427         TBLuni = ds5P;
164300090427         exsr  sr_UpdTABEL_b;
164400090505
164500090505         if  xx > *zero;
164600090505           $5PO(xx) = TBLflg + TBLuni;
164700090505         endif;
164800090427
164900090427         // -?Tab. 5P/S = Etichette?
165000090427         clear  ds5P;
165100090427         §5Potq = V2CotqS;
165200090427         §5Pmod = V2CmodS;
165300090427
165400090504         k_TBLkey = 'S' + V2Cfil;
165500090427         chain  %kds(k03tabel00)  TABEL;
165600090427         TBLuni = ds5P;
165700090427         exsr  sr_UpdTABEL_b;
165800090505
165900090505         if  xx > *zero;
166000090505           $5PS(xx) = TBLflg + TBLuni;
166100090505         endif;
166200090427
166300090427         // -?Tab. 5P/T = Tabulati?
166400090427         clear  ds5P;
166500090428         §5Potq = V2CotqT;
166600090427
166700090504         k_TBLkey = 'T' + V2Cfil;
166800090427         chain  %kds(k03tabel00)  TABEL;
166900090427         TBLuni = ds5P;
167000090427         exsr  sr_UpdTABEL_b;
167100090505
167200090505         if  xx > *zero;
167300090505           $5PT(xx) = TBLflg + TBLuni;
167400090505         endif;
167500090505
167600090505
167700090505         //?Aggiornamento campi nel subfile S01?
167800090505         if  Not $InzS01;
167900090508           exsr  sr_FldS01;
168000090505         endif;
168100090427
168200090427       ENDSR;
168300090427
168400090427       //--------------------------------------------------------------
168500090427       //?Inserimento/Aggiornamento singolo record nel file TABEL00F   ?
168600090427       //--------------------------------------------------------------
168700090427       BEGSR  sr_UpdTABEL_b;
168800090427
168900090427         select;
169000090505           // -?Annullamento (opzione 4)?
169100090505           when (S1Copz  = '4'  or  dsp_aid = c_F16)  and
169200090505                 %found(TABEL00F)                     and
169300090505                 $RecAnn = *off;
169400090504             TBLflg = '*';
169500090505           // -?Ripristino (opzione 4)?
169600090505           when (S1Copz  = '4'  or  dsp_aid = c_F16)  and
169700090505                 %found(TABEL00F)                     and
169800090505                 $RecAnn = *on;
169900090504             clear  TBLflg;
170000090505           // -?Annullamento (F16)?
170100090505           when  dsp_aid = c_F16  and  %found(TABEL00F);
170200090505             TBLflg = '*';
170300090505           // -?Ripristino (F5)?
170400090505           when  dsp_aid = c_F05  and  %found(TABEL00F);
170500090505             clear  TBLflg;
170600090505           // -?Precauzionale... se tab. non trovata e da non inserire?
170700090505           other;
170800090505             clear  TBLflg;
170900090427         endsl;
171000090427
171100090505         // -?Impostazione campi per trasmissione?
171200100111         //if  Not %found(TABEL00F);
171300100111           TBLftt = '1';
171400090427           clear  TBLflt;
171500100111         //endif;
171600090427         clear  TBLftr;
171700090427         clear  TBLdtr;
171800090427
171900090505
172000090505         select;
172100090505
172200090505           // -?Aggiornamento singolo record?
172300090505           when  %found(TABEL00F)       and
172400090505                 TBLuni <> *blank;
172500090505             //____________
172600090505             update  TABEL;
172700090505             //¯¯¯¯¯¯¯¯¯¯¯¯
172800090505
172900090505           // -?Cancellazione singolo record?
173000090505           when  %found(TABEL00F)       and
173100090505                 TBLuni = *blank;
173200090505             //____________
173300090505             delete  TABEL;
173400090505             //¯¯¯¯¯¯¯¯¯¯¯¯
173500090505
173600090505           // -?Aggiunta singolo record?
173700090505           when  Not %found(TABEL00F)   and
173800090505                 TBLuni <> *blank;
173900090505             TBLkut = k_TBLkut;
174000090505             TBLcod = k_TBLcod;
174100090505             TBLkey = k_TBLkey;
174200090505             //___________
174300090505             write  TABEL;
174400090505             //¯¯¯¯¯¯¯¯¯¯¯
174500090508             clear  $wFil;
174600090508             clear  SavC1Pfil;
174700090505             $InzS01 = *on;
174800090505
174900090505           // -?Nulla?
175000090505           when  Not %found(TABEL00F)   and
175100090505                 TBLuni = *blank;
175200090505
175300090505         endsl;
175400090427
175500090427       ENDSR;
175600090427
175700090427       //--------------------------------------------------------------
175800090427       //?Operazioni finali.                                           ?
175900090427       //--------------------------------------------------------------
176000090427       BEGSR  sr_RoutEnd;
176100090427
176200090427         return;
176300090427
176400090427       ENDSR;
176500090427
176600090427      /end-free
176700090427
176800090427** - $Msg -------------------------------------------------------------------*
176900090504Opzione errata                                                                 1
177000090504Immettere il codice filiale                                                    2
177100090504Immettere SOLO caratteri numerici (se non "EDP")                               3
177200090504Filiale inesistente o annullata                                                4
177300090504Esistono già records per questa filiale - in tab. 5P/                          5
177400090504AVVISO: coda di output inesistente.   Premere Invio per proseguire.            6
177500090504Richieste entrambe le code di output A4 ed A5                                  7
177600090504Richiesto il "Pgm per stampa laser"                                            8
177700090504Programma di stampa errato: può essere                                         9
177800090504Programma di stampa errato: non trovato oggetto                               10
177900090504Flag "Richiesta unica stampante" errato                                       11
178000090504Richiesta la "Coda di output" se specificato un "Programma di stampa"         12
178100090504Richiesto il "Programma di stampa" se specificata una "Coda di output"        13
178200090504Modello stampante errato                                                      14
178300090504Richiesta la "Coda di output" se specificato il "Modello stampante"           15
178400090504Richiesto il "Modello stampante" se specificata una "Coda di output"          16
178500090508Specificare ALMENO una coda di output  oppure  procedere con l'ANNULLAMENTO   17
178600090505Richiesta la coda di output                                                   18
178700090505"Richiesta unica stampante" incompatibile con "Programma di stampa" indicato  19
178800090508Non sono ammesse opzioni quando è richiesto un posizionamento                 20
