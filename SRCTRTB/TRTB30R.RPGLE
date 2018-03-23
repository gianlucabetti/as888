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
022300131023     d tipokey         s              1    inz
022400090427
022500090427       //--------------------------------------------------------------
022600090427       //?Definizione prototipi procedure.                             ?
022700090427       //--------------------------------------------------------------
022800090427
022900090427       // - Reperimento dati utente
023000090427      /copy gaitrasrc/srcProtoPR,TIBS34R
023100090427
023200090427       // - Interrogazione tabelle
023300090427      /copy gaitrasrc/srcProtoPR,X§TABER
023400090428
023500090428       // - Verifica esistenza oggetto
023600090428     d trul08c         pr                  extpgm('TRUL08C')
023700090428     d   ul08obj                     10    const
023800090428     d   ul08lib                     10    const
023900090428     d   ul08obytyp                  10    const
024000090428     d   ul08mbr                     10    const
024100090428     d   ul08err                      1
024200090427
024300090427       //--------------------------------------------------------------
024400090427       //?Definizione key-list.                                        ?
024500090427       //--------------------------------------------------------------
024600090427
024700090427       // - File TABEL00F
024800090427     d k03tabel00    e ds                  extname(TABEL00F : *key)
024900090427     d                                     prefix(k_)   inz
025000090427
025100090427       //--------------------------------------------------------------
025200090427       //?Riepilogo indicatori utilizzati.                             ?
025300090427       //--------------------------------------------------------------
025400090427       //--------------------------------------------------------------
025500090427
025600090427       //--------------------------------------------------------------
025700090427       //?M A I N - L I N E                                            ?
025800090427       //--------------------------------------------------------------
025900090427
026000090427     c     *Entry        plist
026100090427     c                   parm                    KPJBA
026200090427
026300090427      /free
026400090427
026500090427       //?Operazioni iniziali?
026600090427       exsr sr_RoutInz;
026700090427
026800090427       DoW  $Fine = *off;
026900090427         select;
027000090504           // - Gestione subfile S1 (selez. filiale)
027100090504           when $Video = 'S1';
027200090504             exsr sr_GesS01;
027300090504           // - Gestione videata D2 (modifica)
027400090430           when $Video = 'D2';
027500090430             exsr sr_GesD02;
027600090504           // - Gestione subfile S2 (vis. tab. 5P/M)
027700090504           when $Video = 'S2';
027800090504             exsr sr_GesS02;
027900090427           other;
028000090427             $Fine = *on;
028100090427         endsl;
028200090427       enddo;
028300090427
028400090427       //?Operazioni finali?
028500090427       exsr sr_RoutEnd;
028600090427
028700090427       //--------------------------------------------------------------
028800090427       //?Operazioni iniziali.                                         ?
028900090427       //--------------------------------------------------------------
029000090427       BEGSR  sr_RoutInz;
029100090427
029200090427         *inLR = *on;
029300090427
029400090427         //?Reperimento dati job?
029500090427         exsr  sr_DatiJob;
029600090427
029700090427         //?Impostazione nome programma a video?
029800090427         V1Tpgm = SDSpgm;
029900090427         clear  V1Topz;
030000090427
030100090427         //?Impostazione campi "fissi"?
030200090427         k_TBLkut = c_Kut;
030300090427         k_TBLcod = c_Tab;
030400090427
030500090427       ENDSR;
030600090427
030700090427       //--------------------------------------------------------------
030800090427       //?Reperimento Dati del job (Utente/Operativi).                 ?
030900090427       //--------------------------------------------------------------
031000090427       BEGSR  sr_DatiJob;
031100090427
031200090427         in(e) §AzUte;
031300090427         if NOT %error;
031400090427           in(e) §DatiUte;
031500090427         endif;
031600090427         if %error or RSut = *blank;
031700090427           tibs34r ( tibs34ds );
031800090427           in §AzUte;
031900090427           in §DatiUte;
032000090427         endif;
032100090427
032200090427       ENDSR;
032300090427
032400090427       //--------------------------------------------------------------
032500090504       //?Gestione subfile S01                                         ?
032600090427       //--------------------------------------------------------------
032700090504       BEGSR  sr_GesS01;
032800090427
032900090504         //?Inizializzazione subfile?
033000090504         if  $InzS01 = *on;
033100090504           exsr  sr_InzS01;
033200090504           $InzS01 = *off;
033300090427         endif;
033400090427
033500090427         //?Emissione Testata e Piede con tasti funzionali abilitati?
033600090427         write  TB30T1;
033700090427         write  TB30P1;
033800090504
033900090504         //?Posizionamento cursore?
034000090504         if  C1CsrRrn > *zero;
034100090504           C1RcdNbr = C1CsrRrn;
034200090504         else;
034300090504           C1RcdNbr = 1;
034400090504           //write TB30S0;               // (no rec.)
034500090504         endif;
034600090427
034700090427         //?Emissione videata?
034800090504         exfmt  TB30C1;
034900090427         reset  ErrMessage;
035000090427         reset  ErrGenerico;
035100090504         clear  V1Dmsg;
035200090504
035300090427         SELECT;
035400090427
035500090504
035600090504           //?F1=Visualizzazione associazione stampanti-etichette?
035700090504           WHEN  dsp_aid = c_F01;
035800090504             $Video  = 'S2';
035900090504             $InzS02 = *on;
036000090504
036100090427           //?F3=Fine?
036200090427           WHEN  dsp_aid = c_F03;
036300090427             $Fine = *on;
036400090505
036500090505           //?F5=Refresh?
036600090505           WHEN  dsp_aid = c_F05;
036700090508             clear  $wFil;
036800090508             clear  SavC1Pfil;
036900090505             $InzS01 = *on;
037000090504
037100090505           //?F6=Inserimento?
037200090508           //WHEN  dsp_aid = c_F06;
037300090508           //  V1Topz = '  INSERIMENTO  ';
037400090508           //  $Video  = 'D2';
037500090508           //  $InzD02 = *on;
037600090505
037700090505           //?F12=Ritorno?
037800090505           WHEN  dsp_aid = c_F12;
037900090505             $Fine = *on;
038000090504
038100090504           //?Roll-Up?
038200090504           WHEN  dsp_aid = c_RollUp;
038300090504             exsr sr_RollUpS01;
038400090504
038500090504           //?SubFile vuoto?
038600090504           WHEN  S01nrr = *zero;
038700090508
038800090508           //?Richiesto posizionamento?
038900090508           //WHEN  C1Pfil <> SavC1Pfil;
039000090508           //  exsr  sr_CtrlPosizS01;
039100090508           //  if  Not ErrMessage;
039200090508           //    $InzS01 = *on;
039300090508           //  endif;
039400090504
039500090508           //?Invio?
039600090427           OTHER;
039700090508             // -?Gestione opzioni?
039800090504             exsr  sr_OpzS01;
039900090508             if  ErrGenerico;
040000090504               leavesr;
040100090504             endif;
040200090508             // -?Richiesto posizionamento?
040300090508             if  C1Pfil <> SavC1Pfil;
040400090508               $InzS01 = *on;
040500090508             endif;
040600090427
040700090427         ENDSL;
040800090427
040900090427       ENDSR;
041000090427
041100090427       //--------------------------------------------------------------
041200090504       //?Inizializzazione subfile S01                                 ?
041300090427       //--------------------------------------------------------------
041400090504       BEGSR  sr_InzS01;
041500090427
041600090504         //?Spegnimento degli indicatori di errore?
041700090505         %subst(IndDspF : 50) = *off;
041800090504
041900090504         //?Pulizia subfile?
042000090504         SflDsp_N    = *on;
042100090504         SflDspCtl_N = *on;
042200090504         write  TB30C1;
042300090504         SflDspCtl_N = *off;
042400090504         SflEnd      = *off;
042500090504
042600090504         clear  W_SflPag;
042700090504         clear  C1RcdNbr;
042800090504         clear  C1CsrRrn;
042900090504         clear  S01nrr;
043000090504         clear  V1Dmsg;
043100090504         ErrMessage  = *off;
043200090504         ErrGenerico = *off;
043300090504
043400090508         //?Caricamento di tutte le tab. 5P/x (di tutte le filiali)?
043500090508         if  $wFil(1) = *blank;
043600090508           exsr  sr_InTab5P;
043700090508         endif;
043800090504         exsr  sr_RollUpS01;
043900090504
044000090504         //?Impaginazione subfile?
044100090504         // -> forza l'impaginazione sul 1° rec. del subfile
044200090504         if S01nrr > *zero;
044300090504           C1RcdNbr  = 1;
044400090504           C1CsrRrn  = 1;
044500090504         else;
044600090504           clear C1RcdNbr;
044700090504         endif;
044800090427
044900090427       ENDSR;
045000090505
045100090505       //--------------------------------------------------------------
045200090505       //?Intabellamento dati già esistenti per filiale                ?
045300090505       //--------------------------------------------------------------
045400090508       BEGSR  sr_InTab5P;
045500090505
045600090505         reset  $Fil;
045700090505         clear  $wFil;
045800090505         clear  $5PB;
045900090505         clear  $5PO;
046000090505         clear  $5PS;
046100090505         clear  $5PT;
046200090508         clear  xx;
046300090505
046400090505         //?Ciclo di lettura:?
046500090505         //   la prima sarà la "Bxxx" (Bolle/LdV)
046600090505         k_TBLkey = 'B';
046700090505         setll  %kds(k03tabel00)  TABEL;
046800090505         reade(n)  %kds(k03tabel00 : 2)  TABEL;
046900090505
047000090505         DOW  Not %eof(TABEL00F);
047100090505
047200090505           // -?Elaborazione delle sole tabelle "B", "O", "S" e "T"?
047300090505           IF (%subst(TBLkey : 1 : 1) = 'B'     or
047400090505               %subst(TBLkey : 1 : 1) = 'O'     or
047500090505               %subst(TBLkey : 1 : 1) = 'S'     or
047600090505               %subst(TBLkey : 1 : 1) = 'T');
047700090505
047800090505             if  %subst(TBLkey : 1 : 1) = 'B';
047900090505               xx      += 1;
048000090505               $Fil(xx) = %subst(TBLkey : 2 : 3);
048100090505               $5PB(xx) = TBLflg + TBLuni;
048200090505             else;
048300090505               xx = %lookup( %subst(TBLkey : 2 : 3) : $Fil );
048400090505               if  xx = *zero;
048500090505                 xx = %lookup( *hival : $Fil );
048600090505                 $Fil(xx) = %subst(TBLkey : 2 : 3);
048700090505               endif;
048800090505               select;
048900090505                 when  %subst(TBLkey : 1 : 1) = 'O';
049000090505                   $5PO(xx) = TBLflg + TBLuni;
049100090505                 when  %subst(TBLkey : 1 : 1) = 'S';
049200090505                   $5PS(xx) = TBLflg + TBLuni;
049300090505                 when  %subst(TBLkey : 1 : 1) = 'T';
049400090505                   $5PT(xx) = TBLflg + TBLuni;
049500090505               endsl;
049600090505             endif;
049700090505
049800090505           // -?Si scarta la tabella "5P"/"Mxxx" (elaborata in S02)?
049900090505           ELSE;
050000090505
050100090505             k_TBLkey = %subst(TBLkey : 1 : 1) + '99999';
050200090505             setgt  %kds(k03tabel00)  TABEL;
050300090505
050400090505           ENDIF;
050500090505
050600090505           reade(n)  %kds(k03tabel00 : 2)  TABEL;
050700090505
050800090505         ENDDO;
050900090508
051000090508         //?Ordinamento per filiale?
051100090508         $wFil = $Fil;
051200090508         sorta $wFil;
051300090505
051400090505       ENDSR;
051500090504
051600090504       //--------------------------------------------------------------
051700090504       //?Caricamento singola pagina nel subfile S01                   ?
051800090504       //--------------------------------------------------------------
051900090504       BEGSR  sr_RollUpS01;
052000090504
052100090504         S01nrr    = (W_SflPag * c_SflPag);
052200090504         W_SflPag += 1;
052300090504         SflNxtChg = *off;
052400090508
052500090508         //?Impostaz. posizione di partenza nella schiera delle fil.?
052600090508         select;
052700090508           // -?Inzializzazione con tutte le filiali?
052800090508           when  $InzS01 = *on   and   C1Pfil <= *zero;
052900090508             wx  = 1;
053000090508           // -?Inizializzazione con solo alcune filiali?
053100090508           when  $InzS01 = *on   and   C1Pfil <> SavC1Pfil;
053200090508             wx  = %lookupGE( %editc(C1Pfil:'X') : $wFil );
053300090508           // -?Roll-Up (si prosegue con il caricamento)?
053400090508           other;
053500090511             //wx += 1;          (NO: già incrementato)
053600090508         endsl;
053700090504
053800090505         //?Ciclo di caricamento dati da schiere $5Px?
053900090508         DoW  S01nrr    < (W_SflPag * c_SflPag)   and
054000090508              S01nrr    < c_MaxRec                and
054100090508              S01nrr    < %elem($wFil)            and
054200090508              wx        > *zero                   and
054300090508              $wFil(wx) < *hival;
054400090504
054500090508           // -?Registrazione singolo record nel subfile?
054600090508           xx = %lookup( $wFil(wx) : $Fil );
054700090504           clear  ds5PB;
054800090504           clear  ds5P;
054900090504           clear  TB30S1;
055000090504           if  xx > *zero;
055100090508             S1Cfil = $Fil(xx);
055200090508             exsr sr_FldS01;
055300090504           endif;
055400090504
055500090508           // -?Decodifica filiale?
055600090504           WWCfil = S1Cfil;
055700090504           exsr  sr_CtrFil;
055800090504           S1Dfil = WWDfil;
055900090505           ErrMessage = *off;
056000090505           %subst(IndDspF : 50) = *off;
056100090505           clear  V1Dmsg;
056200090504
056300090508           // -?Scrittura record nerl subfile?
056400090504           S01nrr += 1;
056500090504           write  TB30S1;
056600090508
056700090508           wx     += 1;
056800090504
056900090504         EndDo;
057000090508
057100090508         //?Memorizzazione posizionamento effettuato?
057200090508         SavC1Pfil = C1Pfil;
057300090504
057400090504         //?Visualizzazione del SFL (se ci sono dati)?
057500090504         SflDsp_N  = (S01nrr <= *zero);
057600090504
057700090504         //?Attivazione del SFLEND?
057800090504         SflEnd = (S01nrr >= c_MaxRec   or
057900090508                   $wFil(wx) = *hival);
058000090504
058100090504         //?Posizionamento cursore al 1° rcd della pagina?
058200090504         if  S01nrr > *zero;
058300090504           wPag     = S01nrr / c_SflPag;
058400090504           wRig     = S01nrr - (c_SflPag * wPag);
058500090504           C1RcdNbr = wPag * c_SflPag;
058600090504           if  wRig > *zero;
058700090504             C1RcdNbr += 1;
058800090504           else;
058900090504             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
059000090504           endif;
059100090504         else;
059200090504           clear  C1RcdNbr;
059300090504         endif;
059400090504
059500090504         C1CsrRrn = C1RcdNbr;
059600090504
059700090504       ENDSR;
059800090505
059900090505       //--------------------------------------------------------------
060000090505       //?Aggiornamento campi nel singolo record del subfile S01       ?
060100090505       //--------------------------------------------------------------
060200090508       BEGSR  sr_FldS01;
060300090505
060400090505         //?Tab. 5P/B = LdV?
060500090505         if  %subst( $5PB(xx) : 1 : 1 ) = *blank;
060600090505           §daM   = c_DspAtrNorm;
060700090505         else;
060800090505           S1Dann = 'Annullato';
060900090505           §daM   = c_DspAtrRI;
061000090505         endif;
061100090505         ds5PB  = %subst( $5PB(xx) : 2 );
061200090505         S1Cba4 = §5PBa4;
061300090505         S1Cba5 = §5PBa5;
061400090505         S1Cbpl = §5PBpl;
061500090505
061600090505         // -?Tab. 5P/O = O.R.M.?
061700090505         if  %subst( $5PO(xx) : 1 : 1 ) = *blank;
061800090505           §daO   = c_DspAtrNorm;
061900090505         else;
062000090505           S1Dann = 'Annullato';
062100090505           §daO   = c_DspAtrRI;
062200090505         endif;
062300090505         ds5P   = %subst( $5PO(xx) : 2 );
062400090505         S1Cooq = §5Potq;
062500090505         S1Copg = §5Ppgm;
062600090505
062700090505         // -?Tab. 5P/S = Etichette?
062800090505         if  %subst( $5PS(xx) : 1 : 1 ) = *blank;
062900090505           §daS   = c_DspAtrNorm;
063000090505         else;
063100090505           S1Dann = 'Annullato';
063200090505           §daS   = c_DspAtrRI;
063300090505         endif;
063400090505         ds5P   = %subst( $5PS(xx) : 2 );
063500090505         S1Csoq = §5Potq;
063600090505
063700090505         // -?Tab. 5P/T = Tabulati?
063800090505         if  %subst( $5PT(xx) : 1 : 1 ) = *blank;
063900090505           §daT   = c_DspAtrNorm;
064000090505         else;
064100090505           S1Dann = 'Annullato';
064200090505           §daT   = c_DspAtrRI;
064300090505         endif;
064400090505         ds5P   = %subst( $5PT(xx) : 2 );
064500090505         S1Ctoq = §5Potq;
064600090505
064700090505       ENDSR;
064800090508
064900090508       //--------------------------------------------------------------
065000090508       //?Controllo fattibilità del posizionamento nel subfile S01     ?
065100090508       //--------------------------------------------------------------
065200090508       //BEGSR  sr_CtrlPosizS01;
065300090508       //
065400090508       //  //?Ciclo di lettura subfile?
065500090508       //  readc  TB30S1;
065600090508       //
065700090508       //  DoW  Not %eof(TRTB30D);
065800090508       //
065900090508       //    // -?Trovata opzione nel subfile?
066000090508       //    if  S1Copz  <> *blank;
066100090508       //
066200090508       //      // -?Memorizzazione NRR Sfl-Rec con la 1ª opzione?
066300090508       //      ErrMessage  = *on;
066400090508       //      ErrGenerico = *on;
066500090508       //      PosCurOPZ   = *on;
066600090508       //      V1Dmsg = $Msg(20);
066700090508       //
066800090508       //      // -?Aggiornamento sfl?
066900090508       //      C1RcdNbr = S01nrr;
067000090508       //      SflNxtChg = *on;
067100090508       //      C1CsrRrn  = C1RcdNbr;
067200090508       //      UPDATE  TB30S1;
067300090508       //
067400090508       //      // -?Uscita dal ciclo?
067500090508       //      leavesr;
067600090508       //
067700090508       //    endif;
067800090508       //
067900090508       //    readc  TB30S1;
068000090508       //
068100090508       //  ENDDO;
068200090508       //
068300090508       //ENDSR;
068400090504
068500090504       //--------------------------------------------------------------
068600090504       //?Gestione opzioni del subfile S01                             ?
068700090504       //--------------------------------------------------------------
068800090504       BEGSR  sr_OpzS01;
068900090504
069000090504         clear  SavS01csr;
069100090504
069200090505         //?Ciclo di lettura subfile?
069300090504         readc  TB30S1;
069400090504
069500090504         DoW  Not %eof(TRTB30D);
069600090504
069700090505           SflNxtChg = *off;
069800090505           %subst(IndDspF : 50) = *off;
069900090505           C1RcdNbr = S01nrr;
070000090504
070100090504           select;
070200090505
070300090505             // -?2=Modifica?
070400090504             when  S1Copz = '2';
070500090504               V1Topz = '   MODIFICA    ';
070600090504               $Video  = 'D2';
070700090504               $InzD02 = *on;
070800090505
070900090505             // -?3=Copia?
071000090504             when  S1Copz = '3';
071100090504               V1Topz = '     COPIA     ';
071200090504               $Video  = 'D2';
071300090504               $InzD02 = *on;
071400090505
071500090505             // -?4=Annullamento/Ripristino?
071600090504             when  S1Copz = '4';
071700090504               V1Topz = ' ANNULLAMENTO  ';
071800090504               $Video  = 'D2';
071900090504               $InzD02 = *on;
072000090505
072100090505             // -?5=Visualizzazione?
072200090504             when  S1Copz = '5';
072300090504               V1Topz = 'VISUALIZZAZIONE';
072400090504               $Video  = 'D2';
072500090504               $InzD02 = *on;
072600090505
072700090505             // -?Nessuna opzione?
072800090504             when  S1Copz = *blank;
072900090505
073000090505             // -?Opzione errata?
073100090504             other;
073200090504               ErrMessage  = *on;
073300090504               ErrGenerico = *on;
073400090504               PosCurOPZ   = *on;
073500090504               V1Dmsg = $Msg(01);
073600090505
073700090504           endsl;
073800090505
073900090505           // -?Gestione opzione richiesta (se valida)?
074000090505           dow  $Video = 'D2';
074100090505             exsr  sr_GesD02;
074200090505           enddo;
074300090504
074400090505           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
074500090505           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
074600090504           if  S1Copz  <> *blank   and
074700090504              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
074800090504             SavS01csr   = C1RcdNbr;
074900090504           endif;
075000090504
075100090505           // -?Aggiornamento sfl?
075200090504           select;
075300090504             when  ErrMessage;
075400090504               SflNxtChg = *on;
075500090504               C1CsrRrn  = C1RcdNbr;
075600090504             when  ErrGenerico;
075700090504               C1CsrRrn  = C1RcdNbr;
075800090504               clear  S1Copz;
075900090504             other;
076000090504               clear  S1Copz;
076100090504           endsl;
076200090504           UPDATE  TB30S1;
076300090504           if  ErrGenerico   or   ErrMessage;
076400090504             leave;
076500090504           endif;
076600090504
076700090504           readc  TB30S1;
076800090504
076900090504         ENDDO;
077000090504
077100090505         // -?Riposiziono il cursore sul record contenente la prima opz.?
077200090505         //  ?(se non sono stati rilevati errori)?
077300090504         if  NOT ErrMessage   and   NOT ErrGenerico   and
077400090504             SavS01csr > *zero;
077500090504           C1CsrRrn = SavS01csr;
077600090504         endif;
077700090504
077800090504       ENDSR;
077900090427
078000090427       //--------------------------------------------------------------
078100090504       //?Controllo Filiale                                            ?
078200090427       //--------------------------------------------------------------
078300090504       BEGSR  sr_CtrFil;
078400090427
078500090504         clear  WWDfil;
078600131023         clear tipokey     ;
078700090504         SELECT;
078800090504
078900090504           //?Ricerca codice filiale?
079000090504           WHEN  %scan('?' : WWCfil) > *zero;
079100090504             X§Tkut = c_Kut;
079200090504             X§Tcod = c_Tab;
079300090504             clear  X§Tkey;
079400090504             clear  X§Tdes;
079500090504             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
079600090504             WWCfil = %subst(X§Tkey : 2 : %len(WWCfil));
079700090504             ErrGenerico = *on;
079800090427
079900090427           //?Controllo immissione codice filiale?
080000090504           WHEN  WWCfil = *blank  or  WWCfil  = *zero;
080100090504             ErrMessage  = *on;
080200090504             ErrGenerico = *on;
080300090504             PosCurOPZ   = *on;
080400090504             V1Dmsg = $Msg(02);
080500090504             leavesr;
080600090504
080700090504         ENDSL;
080800090427
080900090427         //?Controllo/decodifica filiale?
081000090504         IF  WWCfil <> c_Edp;
081100090428
081200090428           // - Controllo immissione solo caratteri numerici
081300090504           if  %check(c_Digit : WWCfil) > *zero;
081400131023
081500131023           // - il primo carattere deve essere alfabetico
081600131023           if %subst(wwcfil:1:1)>=*zeros    ;
081700090428             ErrMessage  = *on;
081800090428             ErrGenerico = *on;
081900090504             PosCurOPZ   = *on;
082000090504             V1Dmsg = $Msg(03);
082100090428             leavesr;
082200131023           else  ;
082300131023           tipokey= 'A'      ;
082400090428           endif;
082500131023           endif;
082600090428
082700090428           // - Reperimento dati anagrafici
082800131023           if tipokey<>'A'   ;
082900090504           ORGfil = %int(WWCfil);
083000090427           chain  (ORGfil)  AZORG;
083100090427           if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
083200090504             V1Dmsg = $Msg(04);
083300090427             ErrMessage  = *on;
083400090427             ErrGenerico = *on;
083500090504             PosCurOPZ   = *on;
083600090427             leavesr;
083700090427           endif;
083800131023
083900090504           WWDfil = ORGdes;
084000131023           endif;
084100090428
084200090427         ENDIF;
084300090427
084400090427       ENDSR;
084500090504
084600090504       //--------------------------------------------------------------
084700090504       //?Gestione subfile S02 - Visualizzazione pgm x stampa etichette?
084800090504       //--------------------------------------------------------------
084900090504       BEGSR  sr_GesS02;
085000090504
085100090504         //?Inizializzazione subfile?
085200090504         if  $InzS02 = *on;
085300090504           exsr sr_InzS02;
085400090504           $InzS02 = *off;
085500090504         endif;
085600090504
085700090504         //?Emissione Testata e Piede con tasti funzionali abilitati?
085800090504         if  errGenerico = *off;
085900090504           write  TB30T1;
086000090504           write  TB30P2;
086100090504         endif;
086200090504
086300090504         //?Posizionamento cursore?
086400090504         if  C1CsrRrn > *zero;
086500090504           C1RcdNbr = C1CsrRrn;
086600090504         else;
086700090504           C1RcdNbr = 1;
086800090504         endif;
086900090504
087000090504         //?Emissione videata?
087100090504         exfmt  TB30C2;
087200090504         reset  ErrMessage;
087300090504         reset  ErrGenerico;
087400090504         clear  V1Dmsg;
087500090504
087600090504         SELECT;
087700090504
087800090504           //?F3=Fine?
087900090504           WHEN  dsp_aid = c_F03;
088000090504             $Fine = *on;
088100090504
088200090504           //?F12=Ritorno?
088300090504           WHEN  dsp_aid = c_F12;
088400090504             $Video = 'S1';
088500090504
088600090504           //?Roll-Up?
088700090504           WHEN  dsp_aid = c_RollUp;
088800090504             exsr sr_RollUpS02;
088900090504
089000090504           //?Invio (non fa nulla)?
089100090504           OTHER;
089200090504
089300090504         ENDSL;
089400090504
089500090504       ENDSR;
089600090504
089700090504       //--------------------------------------------------------------
089800090504       //?Inizializzazione subfile S02                                 ?
089900090504       //--------------------------------------------------------------
090000090504       BEGSR  sr_InzS02;
090100090504
090200090504         //?Spegnimento degli indicatori di errore?
090300090506         %subst(IndDspF : 50) = *off;
090400090504
090500090504         //?Pulizia subfile?
090600090504         SflDsp2_N    = *on;
090700090504         SflDspCtl2_N = *on;
090800090504         write  TB30C2;
090900090504         SflDspCtl2_N = *off;
091000090504         SflEnd2      = *off;
091100090504
091200111006         clear  W_SflPag2;
091300090504         clear  C2RcdNbr;
091400090504         clear  C2CsrRrn;
091500090504         clear  S02nrr;
091600090504         clear  V1Dmsg;
091700090504         ErrMessage  = *off;
091800090504         ErrGenerico = *off;
091900090504
092000090504         //?Caricamento tab. 5P/M per la filiale?
092100090504         k_TBLkey = 'M';
092200090504         setll  %kds(k03tabel00)  TABEL;
092300090504         reade(n)  %kds(k03tabel00 : 2)  TABEL;
092400090504         exsr  sr_RollUpS02;
092500090504
092600090504         //?Impaginazione subfile?
092700090504         // -> forza l'impaginazione sul 1° rec. del subfile
092800090504         if S02nrr > *zero;
092900090504           C2RcdNbr  = 1;
093000090504           C2CsrRrn  = 1;
093100090504         else;
093200090504           clear C2RcdNbr;
093300090504         endif;
093400090504
093500090504       ENDSR;
093600090504
093700090504       //--------------------------------------------------------------
093800090504       //?Caricamento singola pagina nel subfile S02                   ?
093900090504       //--------------------------------------------------------------
094000090504       BEGSR  sr_RollUpS02;
094100090504
094200090504         S02nrr     = (W_SflPag2 * c_SflPag2);
094300090504         W_SflPag2 += 1;
094400090504         SflNxtChg2 = *off;
094500090504
094600090504         clear  ds5PM;
094700090504
094800090505         //?Ciclo di caricamento dati per tab. 5P/M?
094900090504         DoW  S02nrr < (W_SflPag2 * c_SflPag2)   and
095000090504              S02nrr < c_MaxRec                  and
095100090504              Not %eof(TABEL00F)                 and
095200090504              %subst(TBLkey : 1 : 1) = 'M';
095300090504
095400090504           ds5PM = TBLuni;
095500090504           clear  TB30S2;
095600090504           S2Ckey  = %subst(TBLkey : 2);
095700090504           S2Cdes  = %subst(TBLuni : 52 : %len(S2Cdes));
095800090504           S2Cpss  = §5PPss;
095900090504           S2Cpsb  = §5PPsb;
096000090504           S2Cpsd  = §5PPsd;
096100090504           S2Cpsf  = §5PPsf;
096200090504
096300090504           S02nrr += 1;
096400090504           write  TB30S2;
096500090504
096600090504           reade(n)  %kds(k03tabel00 : 2)  TABEL;
096700090504
096800090504         EndDo;
096900090504
097000090504         //?Visualizzazione del SFL (se ci sono dati)?
097100090504         SflDsp2_N  = (S02nrr <= *zero);
097200090504
097300090504         //?Attivazione del SFLEND?
097400090504         SflEnd2 = (%eof(TABEL00F)  or  S02nrr >= c_MaxRec  or
097500090504                    %subst(TBLkey : 1 : 1) <> 'M');
097600090504
097700090504         //?Posizionamento cursore al 1° rcd della pagina?
097800090504         if  S02nrr > *zero;
097900090504           wPag     = S02nrr / c_SflPag2;
098000090504           wRig     = S02nrr - (c_SflPag2 * wPag);
098100090504           C2RcdNbr = wPag * c_SflPag2;
098200090504           if  wRig > *zero;
098300090504             C2RcdNbr += 1;
098400090504           else;
098500090504             C2RcdNbr = C2RcdNbr - c_SflPag2 + 1;
098600090504           endif;
098700090504         else;
098800090504           clear  C2RcdNbr;
098900090504         endif;
099000090504
099100090504         C2CsrRrn = C2RcdNbr;
099200090504
099300090504       ENDSR;
099400090427
099500090427       //--------------------------------------------------------------
099600090504       //?Gestione videata D02 - Dati tab. "5P" / "B", "O", "S" e "T"  ?
099700090427       //--------------------------------------------------------------
099800090427       BEGSR  sr_GesD02;
099900090427
100000090427         //?Inizializzazione videata?
100100090427         if  $InzD02 = *on;
100200090427           exsr sr_InzD02;
100300090427           $InzD02 = *off;
100400090427         endif;
100500090427
100600090427         //?Emissione Testata e Piede con tasti funzionali abilitati?
100700090427         if  ErrGenerico = *off;
100800090427           write  TB30T1;
100900090504           if  S1Copz = '5';
101000090504             write  TB30P2;
101100090504           else;
101200090504             write  TB30P3;
101300090504           endif;
101400090427         endif;
101500090427
101600090427         //?Emissione videata?
101700090504         if  S1Copz = '5';
101800090504           write  TB30D2;
101900090504           exfmt  Protect;
102000090504         else;
102100090504           exfmt  TB30D2;
102200090504         endif;
102300090504
102400090427         reset  ErrMessage;
102500090427         reset  ErrGenerico;
102600090427         clear  V1Dmsg;
102700090427         SELECT;
102800090427
102900090427           //?F3=Fine?
103000090427           WHEN  dsp_aid = c_F03;
103100090427             unlock TABEL00F;
103200090504             reset  $Video;
103300090504             ErrGenerico = *on;
103400090427
103500090427           //?F12=Ritorno?
103600090427           WHEN  dsp_aid = c_F12;
103700090427             unlock TABEL00F;
103800090427             reset  $Video;
103900090505
104000090505           //?F23=Sblocco dei campi con i pgm di stampa?
104100090505           WHEN  dsp_aid = c_F23;
104200090505             $F23 = *on;
104300090505             exsr  sr_SetIndD02;
104400090427
104500090428           //?Invio; F5=Ripristino; F6=Conferma; F16=Annullamento?
104600090428           OTHER;
104700090505             //?Nessun controllo dati per:?
104800090505             //  ?· F16=Annullamento?
104900090505             //  ?· Opz. 4=Annullamento (sì controlli al Ripristino)?
105000090505             //  ?· Opz. 5=Visualizzazione?
105100090504             // - Controllo chiave di accesso solo per copia/inserim.
105200090505             if  S1Copz = '3'   or
105300090505                 %trim(V1Topz) = c_Insert;
105400090505               exsr  sr_CtrCpyIns;
105500090504               if  ErrGenerico = *on;
105600090504                 leavesr;
105700090504               endif;
105800090504             endif;
105900090504             // - Controllo altri dati se non richiesto annullamento
106000090504             if  dsp_aid <> c_F16   and
106100090505                 S1Copz  <> '5'     and
106200090505                (S1Copz  <> '4'     or    S1Dann <> *blank);
106300090428               exsr  sr_CtrD02;
106400090428               if  ErrGenerico = *on;
106500090428                 leavesr;
106600090428               endif;
106700090428             endif;
106800090427
106900090427             //?F5=Ripristino; F6=Conferma; F16=Annullamento?
107000090427             if  dsp_aid = c_F05  or
107100090427                 dsp_aid = c_F06  or
107200090427                 dsp_aid = c_F16;
107300090427               exsr  sr_UpdTABEL;
107400090505               reset $Video;
107500090427             endif;
107600090427
107700090427         ENDSL;
107800090505
107900090505         //?Pulizia del sotto-titolo in testata al ritorno nel sfl S01?
108000090505         if  $Video <> 'D2';
108100090505           clear  V1Topz;
108200090505         endif;
108300090427
108400090427       ENDSR;
108500090427
108600090427       //--------------------------------------------------------------
108700090504       //?Inizializzazione videata D02                                 ?
108800090427       //--------------------------------------------------------------
108900090427       BEGSR  sr_InzD02;
109000090427
109100090427         //?Spegnimento degli indicatori di errore?
109200090506         %subst(IndDspF : 50) = *off;
109300090505
109400090505         //?Spegnimento indicatore per la protezione dei campi con i?
109500090505         //  ?pgm di stampa?
109600090505         //reset  $F23;
109700090505         //?Protezione dei campi con i pgm di stampa SE NON inserimento?
109800090505         $F23 = (dsp_aid = c_F06);
109900090428
110000090504         //?Pulizia campi di salvataggio per schivare controlli?
110100090428         clear  SaveBA4;
110200090428         clear  SaveBA5;
110300090428         clear  SaveBPL;
110400090428         clear  SaveOTQo;
110500090428         clear  SavePGMo;
110600090428         clear  SaveOTQs;
110700090428         clear  SaveOTQt;
110800090428
110900090427         //?Pulizia videata?
111000090427         clear  TB30D2;
111100090427
111200090427         //?Caricamento dati per la filiale?
111300090504         if  dsp_aid <> c_F06;
111400090504           exsr  sr_RieD02;
111500090504         endif;
111600090427
111700090505         //?Impostazione tipo manutenzione per opzione 4=Ripristino?
111800090505         if  S1Copz = '4'   and   $RecAnn = *on;
111900090505           V1Topz = '  RIPRISTINO   ';
112000090505         endif;
112100090427
112200090427         //?Impostazione indicatori per abilitazione tasti funzionali?
112300090505         exsr  sr_SetIndD02;
112400090427
112500090427       ENDSR;
112600090504
112700090504       //--------------------------------------------------------------
112800090504       //?Caricamento dati nella videata D02                           ?
112900090504       //--------------------------------------------------------------
113000090504       BEGSR  sr_RieD02;
113100090505
113200090505         //?Resettaggio del flag che indica se TUTTI i rec. risultano?
113300090505         //  ?annullati?
113400090505         $RecAnn = *on;
113500090504
113600090505         //?Impostazione filiale SE NON "copia"?
113700090504         if  S1Copz <> '3';
113800090504           V2Cfil = S1Cfil;
113900090504           WWCfil = V2Cfil;
114000090504           exsr  sr_CtrFil;
114100090504           V2Dfil = WWDfil;
114200090505           ErrMessage = *off;
114300090505           %subst(IndDspF : 50) = *off;
114400090505           clear  V1Dmsg;
114500090504         endif;
114600090504
114700090505         //?Tab. 5P/B = LdV?
114800090504         clear  ds5PB;
114900090504         k_TBLkey = 'B' + S1Cfil;
115000090504         if  S1Copz = '3'   or
115100090504             S1Copz = '5';
115200090504           chain(n)  %kds(k03tabel00)  TABEL;
115300090504         else;
115400090504           chain  %kds(k03tabel00)  TABEL;
115500090504         endif;
115600090504         if  %found(TABEL00F);
115700090504           ds5PB = TBLuni;
115800090505           if  TBLflg = *blank   or   S1Copz = '3';
115900090504             reset  $RecAnn;
116000090504           else;
116100090504             V2Canb = 'ANNULLATA ';
116200090504           endif;
116300090504         endif;
116400090504
116500090504         V2Hbol  = §5PBol;
116600090504         V2Hbts  = §5PBts;
116700090504         V2Hbpg  = §5PBpg;
116800090504         V2Cba4  = §5PBa4;
116900090504         V2Cba5  = §5PBa5;
117000090504         V2Cbpl  = §5PBpl;
117100090504         V2Cbus  = §5PBus;
117200090504
117300090505         //?Tab. 5P/O = O.R.M.?
117400090504         clear  ds5P;
117500090504         k_TBLkey = 'O' + S1Cfil;
117600090504         if  S1Copz = '3'   or
117700090504             S1Copz = '5';
117800090504           chain(n)  %kds(k03tabel00)  TABEL;
117900090504         else;
118000090504           chain  %kds(k03tabel00)  TABEL;
118100090504         endif;
118200090504         if  %found(TABEL00F);
118300090504           ds5P = TBLuni;
118400090505           if  TBLflg = *blank   or   S1Copz = '3';
118500090504             reset  $RecAnn;
118600090504           else;
118700090504             V2Cano = 'ANNULLATA ';
118800090504           endif;
118900090504         endif;
119000090504
119100090504         V2CotqO = §5Potq;
119200090504         V2CpgmO = §5Ppgm;
119300090504
119400090505         //?Tab. 5P/S = Etichette?
119500090504         clear  ds5P;
119600090504         k_TBLkey = 'S' + S1Cfil;
119700090504         if  S1Copz = '3'   or
119800090504             S1Copz = '5';
119900090504           chain(n)  %kds(k03tabel00)  TABEL;
120000090504         else;
120100090504           chain  %kds(k03tabel00)  TABEL;
120200090504         endif;
120300090504         if  %found(TABEL00F);
120400090504           ds5P = TBLuni;
120500090505           if  TBLflg = *blank   or   S1Copz = '3';
120600090504             reset  $RecAnn;
120700090504           else;
120800090504             V2Cans = 'ANNULLATA ';
120900090504           endif;
121000090504         endif;
121100090504
121200090504         V2CotqS = §5Potq;
121300090504         V2CmodS = §5Pmod;
121400090504
121500090505         //?Tab. 5P/T = Tabulati?
121600090504         clear  ds5P;
121700090504         k_TBLkey = 'T' + S1Cfil;
121800090504         if  S1Copz = '3'   or
121900090504             S1Copz = '5';
122000090504           chain(n)  %kds(k03tabel00)  TABEL;
122100090504         else;
122200090504           chain  %kds(k03tabel00)  TABEL;
122300090504         endif;
122400090504         if  %found(TABEL00F);
122500090504           ds5P = TBLuni;
122600090505           if  TBLflg = *blank   or   S1Copz = '3';
122700090504             reset  $RecAnn;
122800090504           else;
122900090504             V2Cant = 'ANNULLATA ';
123000090504           endif;
123100090504         endif;
123200090504
123300090504         V2CotqT = §5Potq;
123400090504
123500090505         //?Decodifica modello stampante etichette?
123600090504         exsr  sr_Decod5PM;
123700090504
123800090504       ENDSR;
123900090427
124000090427       //--------------------------------------------------------------
124100090427       //?Settaggio indicatori per abilitazione tasti funzionali nella ?
124200090504       //?  videata D02                                                ?
124300090427       //--------------------------------------------------------------
124400090505       BEGSR  sr_SetIndD02;
124500090427
124600090504         // Protezione campi chiave
124700090505         ProtectFIL = ( S1Copz  <> '3'   and
124800090505                  %trim(V1Topz) <> c_Insert );
124900090505
125000090505         // Protezione campi con programma di stampa
125100090505         ProtectPGM = ( Not $F23 );
125200090504
125300090427         // F5=Ripristino
125400090505         F5Attivo   =  ( S1Copz = '2'   and   $RecAnn = *on );
125500090427
125600090427         // F6=Conferma
125700090505         F6Attivo   = (NOT F5Attivo);
125800090427
125900090427         // F16=Annullamento
126000090505         F16Attivo  = ( S1Copz = '2'   and   $RecAnn = *off );
126100090505
126200090505         // F23=Sblocco campi con pgm di stampa
126300090505         F23Attivo  = ( Not $F23   and   SDSusr = 'EDPSM     ' );
126400090427
126500090427       ENDSR;
126600090504
126700090504       //--------------------------------------------------------------
126800090505       //?Controllo dati immessi nella videata D02 x 3=Copia/F6=Inser. ?
126900090504       //--------------------------------------------------------------
127000090505       BEGSR  sr_CtrCpyIns;
127100090504
127200090504         //?Verifica esistenza records per tale filiale in tab. "5P"?
127300090504         // - 1ª verifica in tab. 5P/B
127400090504         k_TBLkey = 'B' + V2Cfil;
127500090504         setll  %kds(k03tabel00)  TABEL;
127600090504         if  Not %equal(TABEL00F);
127700090504           // - 2ª verifica in tab. 5P/O
127800090504           k_TBLkey = 'O' + V2Cfil;
127900090504           setll  %kds(k03tabel00)  TABEL;
128000090504           if  Not %equal(TABEL00F);
128100090504             // - 3ª verifica in tab. 5P/S
128200090504             k_TBLkey = 'S' + V2Cfil;
128300090504             setll  %kds(k03tabel00)  TABEL;
128400090504             if  Not %equal(TABEL00F);
128500090504               // - 4ª verifica in tab. 5P/S
128600090504               k_TBLkey = 'T' + V2Cfil;
128700090504               setll  %kds(k03tabel00)  TABEL;
128800090504             endif;
128900090504           endif;
129000090504         endif;
129100090504         if  %equal(TABEL00F);
129200090504           ErrMessage  = *on;
129300090504           ErrGenerico = *on;
129400090504           PosCurOPZ   = *on;
129500090504           V1Dmsg = %trimr($Msg(05)) + %subst(k_TBLkey : 1 : 1);
129600090504           leavesr;
129700090504         endif;
129800090504
129900090504       ENDSR;
130000090427
130100090427       //--------------------------------------------------------------
130200090504       //?Controllo dati immessi nella videata D02                     ?
130300090427       //--------------------------------------------------------------
130400090427       BEGSR  sr_CtrD02;
130500090427
130600090427         %subst(IndDspF : 50) = *off;
130700090505
130800090505         //?Controllo codice filiale?
130900090505         clear  V2Dfil;
131000090505         WWCfil = V2Cfil;
131100090505         exsr  sr_CtrFil;
131200090505         V2Dfil = WWDfil;
131300090505         if  ErrGenerico = *on;
131400090505           leavesr;
131500090505         endif;
131600090505
131700090505         //?Non consentita la cancellazione di tutte le outq nelle?
131800090505         //  ?varie tabelle 5P/x - SE NON con l'opz.4?
131900090505         if  S1Copz <> '4'      and
132000090505             V2Cba4  = *blank   and   V2Cba5 = *blank   and
132100090505             V2CotqO = *blank   and
132200090505             V2CotqS = *blank   and
132300090505             V2CotqT = *blank;
132400090505           ErrMessage  = *on;
132500090505           ErrGenerico = *on;
132600090505           PosCurBA4   = *on;
132700090505           if  %trim(V1Topz) = c_Insert;
132800090505             V1Dmsg = %subst( $Msg(17) : 1 : 37 );
132900090505           else;
133000090505             V1Dmsg = $Msg(17);
133100090505           endif;
133200090505           leavesr;
133300090505         endif;
133400090428
133500090428         //?Controlli per stampa LdV (tab. 5P/B)?
133600090505
133700090505         // -?Programma di stampa (campo protetto) da eliminare?
133800090505         if  V2Cbpl <> *blank   and
133900090505            (V2Cba4 =  *blank   and  V2Cba5 = *blank)   and
134000090505             ProtectPGM;
134100090505           clear  V2Cbpl;
134200090505           clear  V2Cbus;
134300090505         endif;
134400090428
134500090505         // -?Controllo code di output per stampanti laser?
134600090505         select;
134700090505           when  V2Cbpl <> *blank   and
134800090505                 V2Cbus =  *blank   and
134900090505                (V2Cba4 =  *blank   or   V2Cba5 = *blank);
135000090505             ErrMessage  = *on;
135100090505             ErrGenerico = *on;
135200090505             if  V2Cba4 = *blank;
135300090505               PosCurBA4 = *on;
135400090505             else;
135500090505               PosCurBA5 = *on;
135600090505             endif;
135700090505             V1Dmsg = $Msg(07);
135800090505             leavesr;
135900090505           when  V2Cbpl <> *blank   and
136000090505                 V2Cbus =  '4'      and
136100090505                 V2Cba4 =  *blank;
136200090505             ErrMessage  = *on;
136300090505             ErrGenerico = *on;
136400090505             PosCurBA4   = *on;
136500090505             V1Dmsg = %trimr( $Msg(18) )  + ' A4';
136600090505             leavesr;
136700090505           when  V2Cbpl <> *blank   and
136800090505                 V2Cbus =  '5'      and
136900090505                 V2Cba5 =  *blank;
137000090505             ErrMessage  = *on;
137100090505             ErrGenerico = *on;
137200090505             PosCurBA5   = *on;
137300090505             V1Dmsg = %trimr( $Msg(18) )  + ' A5';
137400090505             leavesr;
137500090505         endsl;
137600090430
137700090428         if  V2Cba4 <> *blank   and   V2Cba4 <> SaveBA4;
137800090428           SaveBA4 = V2Cba4;
137900090428           clear  wErr;
138000090428           trul08c (V2Cba4 : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
138100090428           if  wErr = 'P';
138200090428             ErrMessage  = *on;
138300090428             ErrGenerico = *on;
138400090428             PosCurBA4   = *on;
138500090504             V1Dmsg = $Msg(06);
138600090428             leavesr;
138700090428           endif;
138800090428         endif;
138900090428
139000090428         if  V2Cba5 <> *blank   and   V2Cba5 <> SaveBA5;
139100090428           SaveBA5 = V2Cba5;
139200090428           clear  wErr;
139300090428           trul08c (V2Cba5 : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
139400090428           if  wErr = 'P';
139500090428             ErrMessage  = *on;
139600090428             ErrGenerico = *on;
139700090428             PosCurBA5   = *on;
139800090504             V1Dmsg = $Msg(06);
139900090428             leavesr;
140000090428           endif;
140100090428         endif;
140200090428
140300090505         // -?Controllo programma di stampa su stampante laser?
140400090428         select;
140500090505           when (V2Cba4 <> *blank         or
140600090505                 V2Cba5 <> *blank)        and
140700090505                 V2Cbpl =  *blank;
140800090505             ErrMessage  = *on;
140900090505             ErrGenerico = *on;
141000090505             PosCurBPL   = *on;
141100090505             V1Dmsg = $Msg(08);
141200090505             leavesr;
141300090505           when  V2Cbpl <> *blank         and
141400090505                 V2Cbpl <> 'FNLSB5R   '   and
141500090505                 V2Cbpl <> 'FNLSB6R   ';
141600090428             ErrMessage  = *on;
141700090428             ErrGenerico = *on;
141800090428             PosCurBPL   = *on;
141900090504             V1Dmsg = %trimr($Msg(09)) + ' "FNLSB5R" o "FNLSB6R"';
142000090428             leavesr;
142100090428         endsl;
142200090428
142300090505         if  V2Cbpl <> *blank   and   V2Cbpl <> SaveBPL;
142400090428           SaveBPL = V2Cbpl;
142500090428           clear  wErr;
142600090428           trul08c (V2Cbpl : '*LIBL' : '*PGM' : '*NONE' : wErr);
142700090428           if  wErr = 'P';
142800090428             ErrMessage  = *on;
142900090428             ErrGenerico = *on;
143000090428             PosCurBPL   = *on;
143100090504             V1Dmsg = $Msg(10);
143200090428             leavesr;
143300090428           endif;
143400090428         endif;
143500090428
143600090428         // -?Controllo richiesta stampante unica?
143700090428         select;
143800090428           when  V2Cbus <> *blank   and
143900090428                 V2Cbus <> '4'      and
144000090428                 V2Cbus <> '5';
144100090428             ErrMessage  = *on;
144200090428             ErrGenerico = *on;
144300090428             PosCurBUS   = *on;
144400090504             V1Dmsg = $Msg(11);
144500090428             leavesr;
144600090505           when  V2Cbpl =  'FNLSB5R   '   and
144700090505                 V2Cbus <> *blank;
144800090505             ErrMessage  = *on;
144900090505             ErrGenerico = *on;
145000090505             PosCurBUS   = *on;
145100090505             V1Dmsg = $Msg(19);
145200090505             leavesr;
145300090505           when  V2Cbpl =  'FNLSB6R   '   and
145400090505                 V2Cbus =  *blank;
145500090505             ErrMessage  = *on;
145600090505             ErrGenerico = *on;
145700090505             PosCurBUS   = *on;
145800090505             V1Dmsg = $Msg(19);
145900090505             leavesr;
146000090428         endsl;
146100090428
146200090428         //?Controlli per stampa O.R.M. (tab. 5P/O)?
146300090505
146400090505         // -?Programma di stampa (campo protetto) da eliminare?
146500090505         if  V2CpgmO <> *blank   and
146600090505             V2CotqO =  *blank   and
146700090505             ProtectPGM;
146800090505           clear  V2CpgmO;
146900090505         endif;
147000090428
147100090428         // -?Controllo code di output?
147200090428         if  V2CotqO <> *blank   and   V2CotqO <> SaveOTQo;
147300090428           SaveOTQo = V2CotqO;
147400090428           clear  wErr;
147500090428           trul08c (V2CotqO : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
147600090428           if  wErr = 'P';
147700090428             ErrMessage  = *on;
147800090428             ErrGenerico = *on;
147900090428             PosCurOTQO  = *on;
148000090504             V1Dmsg = $Msg(06);
148100090428             leavesr;
148200090428           endif;
148300090428         endif;
148400090428
148500090428         // -?Controllo programma di stampa?
148600090505         select;
148700090505           when  V2CotqO =  *blank   and
148800090505                 V2CpgmO <> *blank;
148900090505             ErrMessage  = *on;
149000090505             ErrGenerico = *on;
149100090505             PosCurBPL   = *on;
149200090505             V1Dmsg = $Msg(12);
149300090505             leavesr;
149400090505           when  V2CotqO <> *blank   and
149500090505                 V2CpgmO =  *blank;
149600090505             ErrMessage  = *on;
149700090505             ErrGenerico = *on;
149800090505             PosCurBPL   = *on;
149900090505             V1Dmsg = $Msg(13);
150000090505             leavesr;
150100090505           when  V2CpgmO <> *blank         and
150200090505                 V2CpgmO <> 'FIOR16R   '   and
150300090505                 V2CpgmO <> 'FIOR18R   ';
150400090505             ErrMessage  = *on;
150500090505             ErrGenerico = *on;
150600090505             PosCurBPL   = *on;
150700090505             V1Dmsg = %trimr($Msg(09)) + ' "FIOR16R" o "FIOR18R"';
150800090505             leavesr;
150900090505         endsl;
151000090505
151100090505         if  V2CpgmO <> *blank   and   V2CpgmO <> SavePGMo;
151200090505           SavePGMo = V2CpgmO;
151300090505           clear  wErr;
151400090505           trul08c (V2CpgmO : '*LIBL' : '*PGM' : '*NONE' : wErr);
151500090505           if  wErr = 'P';
151600090505             ErrMessage  = *on;
151700090505             ErrGenerico = *on;
151800090505             PosCurPGMO  = *on;
151900090505             V1Dmsg = $Msg(10);
152000090505             leavesr;
152100090505           endif;
152200090505         endif;
152300090428
152400090428         // -?Verifica compatibilità dati inseriti?
152500090428         if  (V2CotqO <> *blank   and   V2CpgmO =  *blank)   or
152600090428             (V2CotqO =  *blank   and   V2CpgmO <> *blank);
152700090428           ErrMessage  = *on;
152800090428           ErrGenerico = *on;
152900090428           if  V2CotqO = *blank;
153000090428             PosCurOTQO  = *on;
153100090504             V1Dmsg = $Msg(12);
153200090428           else;
153300090428             PosCurPGMO  = *on;
153400090504             V1Dmsg = $Msg(13);
153500090428           endif;
153600090428           leavesr;
153700090428         endif;
153800090428
153900090428         //?Controlli per stampa Etichette (tab. 5P/S)?
154000090428
154100090428         // -?Controllo code di output?
154200090428         if  V2CotqS <> *blank   and   V2CotqS <> SaveOTQs;
154300090428           SaveOTQs = V2CotqS;
154400090428           clear  wErr;
154500090428           trul08c (V2CotqS : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
154600090428           if  wErr = 'P';
154700090428             ErrMessage  = *on;
154800090428             ErrGenerico = *on;
154900090428             PosCurOTQS  = *on;
155000090504             V1Dmsg = $Msg(06);
155100090428             leavesr;
155200090428           endif;
155300090428         endif;
155400090428
155500090428         // -?Verifica correttezza modello stampante?
155600090505         if  V2CotqS <> *blank   or   V2CmodS <> *blank;
155700090505           clear  V2DmodS;
155800090505           exsr  sr_Decod5PM;
155900090505           if  Not %found(TABEL00F);
156000090505             ErrMessage  = *on;
156100090505             ErrGenerico = *on;
156200090505             PosCurMODS  = *on;
156300090505             V1Dmsg = $Msg(14);
156400090505             leavesr;
156500090505           endif;
156600090428         endif;
156700090428
156800090428         // -?Verifica compatibilità dati inseriti?
156900090428         if  (V2CotqS <> *blank   and   V2CmodS =  *blank)   or
157000090428             (V2CotqS =  *blank   and   V2CmodS <> *blank);
157100090428           ErrMessage  = *on;
157200090428           ErrGenerico = *on;
157300090428           if  V2CotqS = *blank;
157400090428             PosCurOTQS  = *on;
157500090504             V1Dmsg = $Msg(15);
157600090428           else;
157700090428             PosCurMODS  = *on;
157800090504             V1Dmsg = $Msg(16);
157900090428           endif;
158000090428           leavesr;
158100090428         endif;
158200090428
158300090428         //?Controlli per stampa Tabulati (tab. 5P/T)?
158400090428
158500090428         // -?Controllo code di output?
158600090428         if  V2CotqT <> *blank   and   V2CotqT <> SaveOTQt;
158700090428           SaveOTQt = V2CotqT;
158800090428           clear  wErr;
158900090428           trul08c (V2CotqT : '*LIBL' : '*OUTQ' : '*NONE' : wErr);
159000090428           if  wErr = 'P';
159100090428             ErrMessage  = *on;
159200090428             ErrGenerico = *on;
159300090428             PosCurOTQT  = *on;
159400090504             V1Dmsg = $Msg(06);
159500090428             leavesr;
159600090428           endif;
159700090428         endif;
159800090427
159900090427       ENDSR;
160000090427
160100090504       //--------------------------------------------------------------
160200090504       //?Aggiornamento dati in tab. "5P" / "B", "O", "S", "T"         ?
160300090504       //--------------------------------------------------------------
160400090504       BEGSR  sr_Decod5PM;
160500090504
160600090504         k_TBLkey = 'M' + V2CmodS;
160700090504         chain(n)  %kds(k03tabel00)  TABEL;
160800090504         if  %found(TABEL00F);
160900090504           V2DmodS = %subst(TBLuni : 52 : %len(V2DmodS));
161000090504         endif;
161100090504
161200090504       ENDSR;
161300090504
161400090427       //--------------------------------------------------------------
161500090427       //?Aggiornamento dati in tab. "5P" / "B", "O", "S", "T"         ?
161600090427       //--------------------------------------------------------------
161700090427       BEGSR  sr_UpdTABEL;
161800090427
161900090505         //?Reperimento elemento di schiera da aggiornare?
162000090505         xx = %lookup( V2Cfil : $Fil );
162100090505
162200090505
162300090505         //?Aggiornamento tabelle "5P" (file TABEL00F e schiere $5Px)?
162400090505
162500090427         // -?Tab. 5P/B = LdV?
162600090427         clear  ds5PB;
162700090430         §5PBol  = V2Hbol;
162800090427         §5PBa4  = V2Cba4;
162900090427         §5PBa5  = V2Cba5;
163000090508         if  V2Cbpl <> *blank;
163100090505           §5PBts  = 'S';
163200090505         endif;
163300090430         §5PBpg  = V2Hbpg;
163400090427         §5PBpl  = V2Cbpl;
163500090427         §5PBus  = V2Cbus;
163600090427
163700090504         k_TBLkey = 'B' + V2Cfil;
163800090427         chain  %kds(k03tabel00)  TABEL;
163900090427         TBLuni = ds5PB;
164000090427         exsr  sr_UpdTABEL_b;
164100090505
164200090505         if  xx > *zero;
164300090505           $5PB(xx) = TBLflg + TBLuni;
164400090505         endif;
164500090427
164600090427         // -?Tab. 5P/O = O.R.M.?
164700090427         clear  ds5P;
164800090427         §5Potq = V2CotqO;
164900090427         §5Ppgm = V2CpgmO;
165000090427
165100090504         k_TBLkey = 'O' + V2Cfil;
165200090427         chain  %kds(k03tabel00)  TABEL;
165300090427         TBLuni = ds5P;
165400090427         exsr  sr_UpdTABEL_b;
165500090505
165600090505         if  xx > *zero;
165700090505           $5PO(xx) = TBLflg + TBLuni;
165800090505         endif;
165900090427
166000090427         // -?Tab. 5P/S = Etichette?
166100090427         clear  ds5P;
166200090427         §5Potq = V2CotqS;
166300090427         §5Pmod = V2CmodS;
166400090427
166500090504         k_TBLkey = 'S' + V2Cfil;
166600090427         chain  %kds(k03tabel00)  TABEL;
166700090427         TBLuni = ds5P;
166800090427         exsr  sr_UpdTABEL_b;
166900090505
167000090505         if  xx > *zero;
167100090505           $5PS(xx) = TBLflg + TBLuni;
167200090505         endif;
167300090427
167400090427         // -?Tab. 5P/T = Tabulati?
167500090427         clear  ds5P;
167600090428         §5Potq = V2CotqT;
167700090427
167800090504         k_TBLkey = 'T' + V2Cfil;
167900090427         chain  %kds(k03tabel00)  TABEL;
168000090427         TBLuni = ds5P;
168100090427         exsr  sr_UpdTABEL_b;
168200090505
168300090505         if  xx > *zero;
168400090505           $5PT(xx) = TBLflg + TBLuni;
168500090505         endif;
168600090505
168700090505
168800090505         //?Aggiornamento campi nel subfile S01?
168900090505         if  Not $InzS01;
169000090508           exsr  sr_FldS01;
169100090505         endif;
169200090427
169300090427       ENDSR;
169400090427
169500090427       //--------------------------------------------------------------
169600090427       //?Inserimento/Aggiornamento singolo record nel file TABEL00F   ?
169700090427       //--------------------------------------------------------------
169800090427       BEGSR  sr_UpdTABEL_b;
169900090427
170000090427         select;
170100090505           // -?Annullamento (opzione 4)?
170200090505           when (S1Copz  = '4'  or  dsp_aid = c_F16)  and
170300090505                 %found(TABEL00F)                     and
170400090505                 $RecAnn = *off;
170500090504             TBLflg = '*';
170600090505           // -?Ripristino (opzione 4)?
170700090505           when (S1Copz  = '4'  or  dsp_aid = c_F16)  and
170800090505                 %found(TABEL00F)                     and
170900090505                 $RecAnn = *on;
171000090504             clear  TBLflg;
171100090505           // -?Annullamento (F16)?
171200090505           when  dsp_aid = c_F16  and  %found(TABEL00F);
171300090505             TBLflg = '*';
171400090505           // -?Ripristino (F5)?
171500090505           when  dsp_aid = c_F05  and  %found(TABEL00F);
171600090505             clear  TBLflg;
171700090505           // -?Precauzionale... se tab. non trovata e da non inserire?
171800090505           other;
171900090505             clear  TBLflg;
172000090427         endsl;
172100090427
172200090505         // -?Impostazione campi per trasmissione?
172300100111         //if  Not %found(TABEL00F);
172400100111           TBLftt = '1';
172500090427           clear  TBLflt;
172600100111         //endif;
172700090427         clear  TBLftr;
172800090427         clear  TBLdtr;
172900090427
173000090505
173100090505         select;
173200090505
173300090505           // -?Aggiornamento singolo record?
173400090505           when  %found(TABEL00F)       and
173500090505                 TBLuni <> *blank;
173600090505             //____________
173700090505             update  TABEL;
173800090505             //¯¯¯¯¯¯¯¯¯¯¯¯
173900090505
174000090505           // -?Cancellazione singolo record?
174100090505           when  %found(TABEL00F)       and
174200090505                 TBLuni = *blank;
174300090505             //____________
174400090505             delete  TABEL;
174500090505             //¯¯¯¯¯¯¯¯¯¯¯¯
174600090505
174700090505           // -?Aggiunta singolo record?
174800090505           when  Not %found(TABEL00F)   and
174900090505                 TBLuni <> *blank;
175000090505             TBLkut = k_TBLkut;
175100090505             TBLcod = k_TBLcod;
175200090505             TBLkey = k_TBLkey;
175300090505             //___________
175400090505             write  TABEL;
175500090505             //¯¯¯¯¯¯¯¯¯¯¯
175600090508             clear  $wFil;
175700090508             clear  SavC1Pfil;
175800090505             $InzS01 = *on;
175900090505
176000090505           // -?Nulla?
176100090505           when  Not %found(TABEL00F)   and
176200090505                 TBLuni = *blank;
176300090505
176400090505         endsl;
176500090427
176600090427       ENDSR;
176700090427
176800090427       //--------------------------------------------------------------
176900090427       //?Operazioni finali.                                           ?
177000090427       //--------------------------------------------------------------
177100090427       BEGSR  sr_RoutEnd;
177200090427
177300090427         return;
177400090427
177500090427       ENDSR;
177600090427
177700090427      /end-free
177800090427
177900090427** - $Msg -------------------------------------------------------------------*
178000090504Opzione errata                                                                 1
178100090504Immettere il codice filiale                                                    2
178200131023Immettere caratteri numerici PER filiale o una chiave alfanumerica x SEDE      3
178300090504Filiale inesistente o annullata                                                4
178400090504Esistono già records per questa filiale - in tab. 5P/                          5
178500090504AVVISO: coda di output inesistente.   Premere Invio per proseguire.            6
178600090504Richieste entrambe le code di output A4 ed A5                                  7
178700090504Richiesto il "Pgm per stampa laser"                                            8
178800090504Programma di stampa errato: può essere                                         9
178900090504Programma di stampa errato: non trovato oggetto                               10
179000090504Flag "Richiesta unica stampante" errato                                       11
179100090504Richiesta la "Coda di output" se specificato un "Programma di stampa"         12
179200090504Richiesto il "Programma di stampa" se specificata una "Coda di output"        13
179300090504Modello stampante errato                                                      14
179400090504Richiesta la "Coda di output" se specificato il "Modello stampante"           15
179500090504Richiesto il "Modello stampante" se specificata una "Coda di output"          16
179600090508Specificare ALMENO una coda di output  oppure  procedere con l'ANNULLAMENTO   17
179700090505Richiesta la coda di output                                                   18
179800090505"Richiesta unica stampante" incompatibile con "Programma di stampa" indicato  19
179900090508Non sono ammesse opzioni quando è richiesto un posizionamento                 20
