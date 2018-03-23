000100091001       //--------------------------------------------------------------
000200091001       //?STATISTICA NUMERO PAGINE DI DDT IN LDV PER CLIENTE           ?
000300091001       //--------------------------------------------------------------
000400091001       //? N.B.: ?Il conteggio dei record in FIAR8 avviene in maniera
000500091001       // piuttosto "chiodata":
000600091001       // ogni "Tipo Spampa DDT" prevede un suo limite di OverFlow,
000700091001       // pertanto certi conteggi non possono che essere "chiodati" di
000800091001       // conseguenza.
000900091001       //?Pertanto:?se/quando si dovesse prevedere un nuovo
001000091001       // "Tipo Stampa DDT" in tab. "TSD" occorrerà prevederlo anche
001100091001       // nei conteggi operati da questo programma!!!
001200091001       //?Stessa cosa?(ma questo è più difficle) se venisse modificato
001300091001       // (nel programma di stampa LdV) un limite di OverFlow, o se
001400091001       // venisse gestito un nuovo tipo record per un cliente...
001500091001       //--------------------------------------------------------------
001600091001
001700091001     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800091002     h dftactgrp(*no) actgrp('QILE')
001900091002     h BndDir('UBRTVNETA')
002000091001     h alwnull(*inputonly)
002100091001
002200091001       //--------------------------------------------------------------
002300091001       //?Dichiarazione file.                                          ?
002400091001       //--------------------------------------------------------------
002500091001
002600091001       // - Tabelle
002700091002     fTABEL00F  if   e           k disk
002800091001     fTNTBE01L  if   e           k disk
002900091002
003000091002       // - Packing-List in LdV
003100091002     fFIAR801L  if   e           k disk    extfile(wLibFile)  usropn
003200091002
003300091002       // - Video
003400091002     fTNVRSM005Dcf   e             workstn
003500091002     f                                     indds(IndDspF)
003600091002     f                                     infds(InfDspF)
003700091001
003800091001       // - Stampa Statistica n° Pagine di DDT in LdV
003900091001     fQSYSPRT   o    f  132        printer usropn
004000091001     f                                     oflind(*inOF)
004100091001
004200091001       //--------------------------------------------------------------
004300091001       //?Definizione costanti.                                        ?
004400091001       //--------------------------------------------------------------
004500091002
004600091002       // - Tasti funzionali a video
004700091002     d c_F01           c                   const(x'31')
004800091002     d c_F02           c                   const(x'32')
004900091002     d c_F03           c                   const(x'33')
005000091002     d c_F05           c                   const(x'35')
005100091002     d c_F06           c                   const(x'36')
005200091002     d c_F07           c                   const(x'37')
005300091002     d c_F08           c                   const(x'38')
005400091002     d c_F09           c                   const(x'39')
005500091002     d c_F10           c                   const(x'3A')
005600091002     d c_F12           c                   const(x'3C')
005700091002     d c_F13           c                   const(x'B1')
005800091002     d c_F14           c                   const(x'B2')
005900091002     d c_F15           c                   const(x'B3')
006000091002     d c_F16           c                   const(x'B4')
006100091002     d c_F17           c                   const(x'B5')
006200091002     d c_F18           c                   const(x'B6')
006300091002     d c_F19           c                   const(x'B7')
006400091002     d c_F20           c                   const(x'B8')
006500091002     d c_F21           c                   const(x'B9')
006600091002     d c_F22           c                   const(x'BA')
006700091002     d c_F23           c                   const(x'BB')
006800091002     d c_F24           c                   const(x'BC')
006900091002     d c_Enter         c                   const(x'F1')
007000091002     d c_RollDown      c                   const(x'F4')
007100091002     d c_RollUp        c                   const(x'F5')
007200091001
007300091001       // - Costanti per stringa SQL
007400091002     d c_Union         c                   const(' UNION ')
007500091002     d c_OrderBy       c                   const(' ORDER BY +
007600091002     d                                             AR8tmd, +
007700091002     d                                             AR8aas, +
007800091002     d                                             AR8lnp, +
007900091002     d                                             AR8nrs, +
008000091002     d                                             AR8nsp ')
008100091002     d c_ForFetch      c                   const('FOR FETCH ONLY')
008200091001      *
008300091001     d c_OvrPrtF       c                   const('OVRPRTF +
008400091001     d                                            FILE(QSYSPRT) +
008500091002     d                                            USRDTA(''Stat.LdV'')')
008600091001     d c_DltOvr        c                   const('DLTOVR +
008700091001     d                                            FILE(QSYSPRT)')
008800091001
008900091001       //--------------------------------------------------------------
009000091001       //?Definizione schiere.                                         ?
009100091001       //--------------------------------------------------------------
009200091001
009300091001       // - Stringhe SQL da eseguire
009400091001     d $Sql_1          s             70    dim( 6)  ctdata  perrcd(1)
009500091001     d $Sql_2          s             70    dim(21)  ctdata  perrcd(1)
009600091001
009700091001       // - Schiera tab. "TSD" = Tipo Stampa DDT
009800091001     d $TSD            s                   like(§AR8tsd) dim(015) inz
009900091001
010000091001       // - Filiali e relativi pgm di stampa da tab. "5P"/"B"
010100091002     d $5PBfil         s              3    dim(999)  inz
010200091001     d $5PBpgm         s             10    dim(999)  inz
010300091001
010400091001       // - Schiere tab. "AR8" = Tipo Modulo DDT x file FIAR8:
010500091001       // - - con "Tipo stampa DDT" = "S1" (Std senza logo)
010600091001     d $AR8_S1         s                   like(AR8tmd)  dim(010) inz
010700091001       // - - con "Tipo stampa DDT" = "S2" (Std con logo)
010800091001     d*// $AR8_S2         s                   like(AR8tmd)  dim(010) inz
010900091001       // - - con "Tipo stampa DDT" = "E1" (Esprinet)
011000091001     d $AR8_E1         s                   like(AR8tmd)  dim(010) inz
011100091001       // - - con "Tipo stampa DDT" = "K1" (Karnak)
011200091001     d $AR8_K1         s                   like(AR8tmd)  dim(010) inz
011300091001       // - - con "Tipo stampa DDT" = "00" (personalizzato - v. Arcte)
011400091001     d $AR8_00         s                   like(AR8tmd)  dim(010) inz
011500091001       // - - con "Tipo stampa DDT" = "L*" (Logistica, Sait o Brumar)
011600091001     d $AR8_Lx         s                   like(AR8tmd)  dim(050) inz
011700091001
011800091001       //--------------------------------------------------------------
011900091001       //?Definizione aree dati.                                       ?
012000091001       //--------------------------------------------------------------
012100091001
012200091001       // - Dati utente
012300091001     d §AzUte        e ds                  extname(AZUTE00F)
012400091001     d                                     dtaara
012500091001     d §DatiUte      e ds                  extname(dDatiUte)
012600091001     d                                     dtaara
012700091001
012800091001       //--------------------------------------------------------------
012900091001       //?Definizione strutture dati.                                  ?
013000091001       //--------------------------------------------------------------
013100091001
013200091001       // - Status ds
013300091001     d Status         sds
013400091001     d  SDSpgm           *proc
013500091001     d  SDSprm           *parms
013600091002
013700091002       // - InfDS
013800091002     d InfDspF         ds
013900091002     d   dsp_aid             369    369a
014000091002
014100091002       // - Indicatori su DspF
014200091002     d IndDspF         ds                  inz
014300091002       //   · Emissione messaggio di errore
014400091002     d   ErrMessage                    n   overlay(IndDspF : 28)
014500091002       //   · Data errata
014600091002     d   PosCurDTDC                    n   overlay(IndDspF : 50)
014700091002       //   · Filiali errate
014800091002     d   PosCurFil1                    n   overlay(IndDspF : 51)
014900091002     d   PosCurFil2                    n   overlay(IndDspF : 52)
015000091002     d   PosCurFil3                    n   overlay(IndDspF : 53)
015100091002     d   PosCurFil4                    n   overlay(IndDspF : 54)
015200091002     d   PosCurFil5                    n   overlay(IndDspF : 55)
015300091002       //   · Riemissione videata
015400091002     d   ErrGenerico                   n   overlay(IndDspF : 99)
015500091001
015600091001       // - Parametri ricevuti
015700091001     d KPJBA         e ds
015800091002     d TNVRSM005ds     ds                  inz
015900091002     d  SMdtdc                        8  0 inz(*loval)
016000091001       // - Filiali da gestire
016100091002     d  SMfil1                        3s 0 inz(*hival)
016200091002     d  SMfil2                        3s 0 inz
016300091002     d  SMfil3                        3s 0 inz
016400091002     d  SMfil4                        3s 0 inz
016500091002     d  SMfil5                        3s 0 inz
016600091001     d    s_fil                9     23s 0 inz         dim(5)
016700091001       // - Codici bolla per selezione spedizioni
016800091002     d  SMcbo1                        2    inz
016900091002     d  SMcbo2                        2    inz
017000091002     d  SMcbo3                        2    inz
017100091002     d  SMcbo4                        2    inz
017200091002     d  SMcbo5                        2    inz
017300091002     d  SMcbo6                        2    inz
017400091002     d  SMcbo7                        2    inz
017500091002     d  SMcbo8                        2    inz
017600091002     d  SMcbo9                        2    inz
017700091002     d  SMcboA                        2    inz
017800091001     d    $CBO                24     43    inz         dim(10)
017900091002     d**SMfclr                        1    inz('S')
018000091001
018100091001       // - Parametri per Reperimento dati utente
018200091001     d TIBS34ds      e ds                  inz
018300091001
018400091001       // - Reperimento nome pgm di stampa LdV
018500091001     d ds5PB         e ds                  inz
018600091001
018700091001       // - Tab. AR8 = Tipi Modulo DDT x file FIAR8
018800091001     d dAR8          e ds                  inz
018900091001
019000091001       // - Parametri x Stampa LdV
019100091001     d FNLSB5ds      e ds                  inz
019200091001     d  DB0tbo       e                     inz('A')
019300091001     d  DB0ris       e                     inz('R')
019400091001     d  DB0fl9       e                     inz(*on)
019500130312     d FNLSB6ds1     e ds
019600130312     d                                     BASED(nullPtr)
019700091001
019800091001       // - Ds di comodo per la definizione dei campi estratti via SQL
019900091001     d FNARBds       e ds                  inz  extname(FNARB00F)
020000091001
020100091001     d wSQLds          ds                  inz
020200091001     d  SQLtmd                             inz  like(AR8tmd)
020300091001     d  SQLaas                             inz  like(AR8aas)
020400091001     d  SQLlnp                             inz  like(AR8lnp)
020500091001     d  SQLnrs                             inz  like(AR8nrs)
020600091001     d  SQLnsp                             inz  like(AR8nsp)
020700091001     d  SQLtrk                        2    inz
020800091001     d  SQLrec                        9  0 inz
020900091002
021000091002       // - Schiera per "scomposizione" stringa SQL in stampa
021100091002     d ds_Temp         ds          5000    inz
021200091002     d   $Temp                 1   5000    inz  dim(50)
021300091002
021400091002       // - Parametri per pgm. controllo/inversione data
021500091002     d WLBdat          ds                  inz
021600091002     d  G08dat                        8  0 inz
021700091002     d  G08inv                        8  0 inz
021800091002     d  G08err                        1    inz('3')
021900091002     d  G08tgi                        5  0 inz
022000091001
022100091001       //--------------------------------------------------------------
022200091001       //?Definizione variabili globali.                               ?
022300091001       //--------------------------------------------------------------
022400091001
022500091001       // - Flags booleani
022600091002     d $Fine           s               n   inz
022700091001     d $EoF            s               n   inz
022800091001     d $Err            s               n   inz
022900091002     d $InzD01         s               n   inz(*on)
023000091001
023100091001       // - Indici di schiera
023200091002     d ww              s              3  0 inz
023300091001     d xx              s              3  0 inz
023400091001     d yy              s              3  0 inz
023500091001     d xS1             s              3  0 inz
023600091001     d xE1             s              3  0 inz
023700091001     d xK1             s              3  0 inz
023800091001     d x00             s              3  0 inz
023900091001     d xLx             s              3  0 inz
024000091005     d yS1             s              3  0 inz
024100091005     d yE1             s              3  0 inz
024200091005     d yK1             s              3  0 inz
024300091005     d y00             s              3  0 inz
024400091005     d yLx             s              3  0 inz
024500091001
024600091001       // - Contatori
024700091002     d prt_Data        s             10    inz
024800091001     d prt_Fil         s              3  0 inz
024900091001     d prt_NrA4        s              9  0 inz
025000091001     d prt_NrA5        s              9  0 inz
025100091002     d prt_NrDDT       s              9  0 inz
025200091002     d prt_PagDDT      s              9  0 inz
025300091005     d prt_NrColli     s              9  0 inz
025400091001
025500091002       // - Stringhe SQL da eseguire
025600091002     d wSQL_1          s           5000    inz  varying
025700091002     d wSQL_2          s           5000    inz  varying
025800091001
025900091001       // - Campi per QCMDEXC
026000091001     d Qcmd            s            128    inz  varying
026100091002
026200091002       // - Nome del sistema
026300091002     d currSysNeta     s              8a   inz
026400091001
026500091001       // - Campi di comodo
026600091002     d SAVddc          s                   inz  like(ARBddc)
026700091001     d wErr            s              3  0 inz
026800091001     d wTime           s              6  0 inz
026900091001     d wDate           s              8  0 inz
027000091002     d wNrPag          s              3  0 inz
027100091002     d wResto          s              3  0 inz
027200091001     d wData_Iso       s               d   inz  datfmt(*iso)
027300091001     d w_Fil           s             25    inz  varying
027400091002
027500091002       // - Nome della libreria in cui è il file
027600091002     d wLibFile        s             21a   inz
027700091001
027800091001       //--------------------------------------------------------------
027900091001       //?Definizione prototipi procedure.                             ?
028000091001       //--------------------------------------------------------------
028100091002
028200091002       // - Execuzione comando di sistema
028300091002      /copy gaitrasrc/srcProtoPR,QCMDEXC
028400091001
028500091001       // - Reperimento dati utente
028600091001      /copy gaitrasrc/srcProtoPR,TIBS34R
028700091002
028800091002       // - Reperimento NETA sistema AS/400 corrente
028900091002      /copy gaitrasrc/srcProtoPr,UBRTVNETA
029000091002
029100091002       // - Controllo/Inversione data
029200091002      /copy gaitrasrc/srcProtoPr,XSRDA8
029300091001
029400091001       // - Stampa LdV
029500091001     d fnlsB5r         pr                  extpgm('FNLSB5R')
029600091001     d   fnlsB5ds                          likeds(FNLSB5ds)
029700091001      /copy gaitrasrc/srcProtoPR,FNLSB6R
029800091001
029900091001       //--------------------------------------------------------------
030000091001       //?Definizione key-list.                                        ?
030100091001       //--------------------------------------------------------------
030200091001
030300091001       // - File TNTBE01L
030400091001     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
030500091001     d                                     prefix(k_)   inz
030600091001
030700091001       //--------------------------------------------------------------
030800091001       //?Riepilogo indicatori utilizzati.                             ?
030900091001       //--------------------------------------------------------------
031000091001       //--------------------------------------------------------------
031100091001
031200091001       //--------------------------------------------------------------
031300091001       //?M A I N - L I N E                                            ?
031400091001       //--------------------------------------------------------------
031500091001
031600091001     c     *Entry        plist
031700091001     c                   parm                    KPJBA
031800091001
031900091001      /free
032000091001
032100091001       // -?Operazioni iniziali?
032200091001       exsr  sr_RoutInz;
032300091002
032400091002       // -?Gestione video?
032500091002       DoW  Not  $Fine;
032600091002         exsr  sr_GesD01;
032700091002       EndDo;
032800091001
032900091001       // -?Operazioni finali?
033000091001       exsr sr_RoutEnd;
033100091001
033200091001       //--------------------------------------------------------------
033300091001       //?Operazioni iniziali.                                         ?
033400091001       //--------------------------------------------------------------
033500091001       BEGSR  sr_RoutInz;
033600091001
033700091001         *inLR = *on;
033800091001
033900091001         // -?Impostazione opzioni per SQL?
034000091001         exec sql   set  option  DynUsrPrf = *Owner,
034100091001                                 CloSqlCsr = *EndMod;
034200091001
034300091001         // -?Reperimento dati job?
034400091001         exsr  sr_DatiJob;
034500091001
034600091001       ENDSR;
034700091001
034800091001       //--------------------------------------------------------------
034900091001       //?Reperimento Dati del job (Utente/Operativi).                 ?
035000091001       //--------------------------------------------------------------
035100091001       BEGSR  sr_DatiJob;
035200091001
035300091001         in(E) §AzUte;
035400091001         if NOT %error;
035500091001           in(E) §DatiUte;
035600091001         endif;
035700091001         if %error or RSut = *blanks;
035800091001           clear TIBS34ds;
035900091001           tibs34r ( tibs34ds );
036000091001           in §AzUte;
036100091001           in §DatiUte;
036200091001         endif;
036300091001
036400091001       ENDSR;
036500091002
036600091002       //--------------------------------------------------------------
036700091002       //?Gestione filtro di lancio.                                   ?
036800091002       //--------------------------------------------------------------
036900091002       BEGSR  sr_GesD01;
037000091002
037100091002         // - Inizializzazione videata
037200091002         if  $InzD01 = *on;
037300091002           V1Tpgm = SDSpgm;
037400091002           V1dtdc = *date;
037500091002           V1fil1 = 063;
037600091002           V1fil2 = 005;
037700091002           V1fil3 = 050;
037800091002           V1fil4 = 032;
037900091002           V1cbo1 = '1 ';
038000091002           V1cbo2 = '4 ';
038100091002           V1cbo3 = '5 ';
038200091002           $InzD01 = *off;
038300091002         endif;
038400091002
038500091005         select;
038600091005           when  %subst(KNMUS : 1 : 3) <> 'EDP';
038700091005             ErrMessage  = *on;
038800091005             ErrGenerico = *on;
038900091005             V1Dmsg = 'Funzione eseguibile solo da utenti "EDP..."';
039000091005           when  %subst(KNSIF : 1 : 3) <> 'FIL';
039100091005             ErrMessage  = *on;
039200091005             ErrGenerico = *on;
039300091005             V1Dmsg = 'Funzione eseguibile solo dal sistema +
039400091005                       informativo di filiale (NON di sede)';
039500091005         endsl;
039600091002
039700091002         // - Emissione videata per richiedere i parametri di lancio
039800091002         write  TNVRSM5T;
039900091002         write  TNVRSM5P;
040000091002         exfmt  TNVRSM5D;
040100091002
040200091002         clear  ErrMessage;
040300091002         clear  ErrGenerico;
040400091002         clear  V1Dmsg;
040500091002
040600091002         select;
040700091002
040800091002           when  %subst(KNMUS : 1 : 3) <> 'EDP';
040900091002             $Fine = *on;
041000091005
041100091005           when  %subst(KNSIF : 1 : 3) <> 'FIL';
041200091005             $Fine = *on;
041300091002
041400091002           // - F3 = Fine
041500091002           when  dsp_aid = c_F03;
041600091002             $Fine = *on;
041700091002
041800091002           // - F9 = Personalizzazione Batch
041900091002           //when  dsp_aid = c_F09;
042000091002           //  Kcoaz = 'VRSM';
042100091002           //  BCH09 ( KPJBA );
042200091002
042300091002           other;
042400091002             exsr  sr_CtrD01;
042500091002             select;
042600091002               // - Errore
042700091002               when  ErrGenerico;
042800091002                 leavesr;
042900091002               // - F6 = Conferma
043000091002               when  dsp_aid = c_F06;
043100091005                 write  Protect;
043200091002                 V1text = 'Elaborazione in corso...';
043300091002                 write  TNVRSM5W;
043400091002                 exsr  sr_F06D01;
043500091002                 $Fine  = *on;
043600091002             endsl;
043700091002
043800091002         endsl;
043900091002
044000091002       ENDSR;
044100091002
044200091002       //--------------------------------------------------------------
044300091002       //?Controllo parametri di lancio                                ?
044400091002       //--------------------------------------------------------------
044500091002       BEGSR  sr_CtrD01;
044600091002
044700091002         %subst(IndDspF : 50) = *off;
044800091002
044900091002         // - Controllo Data Distinta Consegna
045000091002         clear  WLBdat;
045100091002         G08dat = V1dtdc;
045200091002         xsrDA8 ( WLBdat );
045300091002         if  G08err = *on;
045400091002           PosCurDTDC  = *on;
045500091002           ErrMessage  = *on;
045600091002           ErrGenerico = *on;
045700091002           V1Dmsg = 'Data errata';
045800091002           leavesr;
045900091002         endif;
046000091002         V1dtdc = G08dat;
046100091002         SMdtdc = G08inv;
046200091002
046300091002         // - Controllo immissione almeno una filiale
046400091002         if  (V1fil1 + V1fil2 + V1fil3 + V1fil4 + V1fil5) = *zero;
046500091002           PosCurFIL1  = *on;
046600091002           ErrMessage  = *on;
046700091002           ErrGenerico = *on;
046800091002           V1Dmsg = 'Inserire almeno una filiale';
046900091002           leavesr;
047000091002         endif;
047100091002         select;
047200091002           when  V1fil1  < *zero;
047300091002             PosCurFIL1  = *on;
047400091002             ErrMessage  = *on;
047500091002             ErrGenerico = *on;
047600091002             V1Dmsg = 'Filiale negativa ?!?!?!';
047700091002             leavesr;
047800091002           when  V1fil2  < *zero;
047900091002             PosCurFIL2  = *on;
048000091002             ErrMessage  = *on;
048100091002             ErrGenerico = *on;
048200091002             V1Dmsg = 'Filiale negativa ?!?!?!';
048300091002             leavesr;
048400091002           when  V1fil3  < *zero;
048500091002             PosCurFIL3  = *on;
048600091002             ErrMessage  = *on;
048700091002             ErrGenerico = *on;
048800091002             V1Dmsg = 'Filiale negativa ?!?!?!';
048900091002             leavesr;
049000091002           when  V1fil4  < *zero;
049100091002             PosCurFIL4  = *on;
049200091002             ErrMessage  = *on;
049300091002             ErrGenerico = *on;
049400091002             V1Dmsg = 'Filiale negativa ?!?!?!';
049500091002             leavesr;
049600091002           when  V1fil5  < *zero;
049700091002             PosCurFIL5  = *on;
049800091002             ErrMessage  = *on;
049900091002             ErrGenerico = *on;
050000091002             V1Dmsg = 'Filiale negativa ?!?!?!';
050100091002             leavesr;
050200091002         endsl;
050300091002
050400091002       ENDSR;
050500091002
050600091002       //--------------------------------------------------------------
050700091002       //?Gestione tasto funzionale F6=Conferma                        ?
050800091002       //--------------------------------------------------------------
050900091002       BEGSR  sr_F06D01;
051000091002
051100091002         // -?Apertura archivio "packing-list"?
051200091002         if  not %open(FIAR801L);
051300091002
051400091002           // -?Verifica del sistema AS/400 corrente?
051500091002           if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
051600091002             // ...???
051700091002           endif;
051800091002
051900091002           if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
052000091002             wLibFile = 'FILTRA201/FIAR801L';
052100091002           else;
052200091002             wLibFile = 'FILTRAPRD/FIAR801L';
052300091002           endif;
052400091002           open  FIAR801L;
052500091002
052600091002         endif;
052700091002
052800091002         // -?Reperimento data odierna in formato aaaammgg?
052900091002         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
053000091002         wData_Iso = %date(wDate : *iso);
053100091002
053200091002         // -?Reperimento orario?
053300091002         wTime = %dec( %time() );
053400091002
053500091002         // -?Gestione parametri (impostati a video)?
053600091002         //SMdtdc = V1dtdc;            (già girata)
053700091002         SMfil1 = V1fil1;
053800091002         SMfil2 = V1fil2;
053900091002         SMfil3 = V1fil3;
054000091002         SMfil4 = V1fil4;
054100091002         SMfil5 = V1fil5;
054200091002         SMcbo1 = V1cbo1;
054300091002         SMcbo2 = V1cbo2;
054400091002         SMcbo3 = V1cbo3;
054500091002         SMcbo4 = V1cbo4;
054600091002         SMcbo5 = V1cbo5;
054700091002         SMcbo6 = V1cbo6;
054800091002         SMcbo7 = V1cbo7;
054900091002         SMcbo8 = V1cbo8;
055000091002         SMcbo9 = V1cbo9;
055100091002         SMcboA = V1cboA;
055200091002         // - Costruzione elenco filiali da elaborare via SQL
055300091002         clear  xx;
055400091002         //clear w_Fil;                (già/ancora così)
055500091002         For  xx = 1  To %elem(s_Fil);
055600091002           select;
055700091002             when  s_Fil(xx) = *zero   or   s_Fil(xx) = *hival;
055800091002             when  w_Fil = *blank;
055900091002               w_Fil = %char(s_Fil(xx));
056000091002             other;
056100091002               w_Fil += ', ' + %char(s_Fil(xx));
056200091002           endsl;
056300091002         EndFor;
056400091002
056500091002         // -?Intabellamento dati da tab. "TSD" & "AR8"?
056600091002         exsr  sr_IntabAR8;
056700091002
056800091002         // -?Intabellamento tab. 5P/B di tutte le filiali?
056900091002         exsr  sr_InTab5Pb;
057000091002
057100091002         clear  ww;
057200091002         xx = 1;
057300091002
057400091002         // -?Preparazione stringa SQL?
057500091002         exsr  sr_PrepSQL_1;
057600091002
057700091002         // -?Ciclo di elaborazione SQL?
057800091002         exsr  sr_OpenCursor_1;
057900091002
058000091002         dou  $EoF = *on;
058100091002           exsr  sr_ReadCursor_1;
058200091002         enddo;
058300091002
058400091002         exsr  sr_CloseCursor_1;
058500091002
058600091002       ENDSR;
058700091001
058800091001       //--------------------------------------------------------------
058900091001       //?Intabellamento dati da tab. "TSD" e "AR8"                    ?
059000091001       //--------------------------------------------------------------
059100091001       BEGSR  sr_IntabAR8;
059200091001
059300091001         // -?Intabellamento Tipi Stampa da tab. "TSD"?
059400091001         clear  xx;
059500091001         //clear  $TSD;                (già/ancora così)
059600091001         k_TBEcod = 'TSD';
059700091001         clear  k_TBEke1;
059800091001         clear  k_TBEke2;
059900091001         clear  k_TBElin;
060000091001         clear  k_TBEsif;
060100091001         setll  %kds(k05tntbe01)  TNTBE000;
060200091001         reade  %kds(k05tntbe01 : 1)  TNTBE000;
060300091001         DoW  Not %eof(TNTBE01L);
060400091001           if  TBEatb = *blank;
060500091001             xx += 1;
060600091001             $TSD(xx) = TBEke1;
060700091001             if  TBEke1 <> 'E1'   and
060800091001                 TBEke1 <> 'K1'   and
060900091001                 TBEke1 <> 'S1'   and
061000091001                 TBEke1 <> 'S2'   and
061100091001                 TBEke1 <> '00'   and
061200130328                 TBEke1 <> '§§'   and            // PDF, no Pckg-Lst
061300091001                 %subst(TBEke1 : 1 : 1) <> 'L';
061400091002               wErr = -1;
061500091002               exsr  sr_Print;
061600091001             endif;
061700091001           endif;
061800091001           reade  %kds(k05tntbe01 : 1)  TNTBE000;
061900091001         EndDo;
062000091001
062100091001         // -?Intabellamento Tipi Modulo DDT da tab. "AR8"?
062200091001         k_TBEcod = 'AR8';
062300091001         clear  k_TBEke1;
062400091001         clear  k_TBEke2;
062500091001         clear  k_TBElin;
062600091001         clear  k_TBEsif;
062700091001         setll  %kds(k05tntbe01)  TNTBE000;
062800091001         reade  %kds(k05tntbe01 : 1)  TNTBE000;
062900091001
063000091001         DoW  Not %eof(TNTBE01L);
063100091001
063200091001           dAR8 = TBEuni;
063300091001           select;
063400091001
063500091001             // -?Rec. annullato?
063600091001             when  TBEatb <> *blank;
063700130328
063800130328             // -?PDF allegati: da non elaborare?
063900130328             when  §AR8tsd = '§§';
064000091001
064100091001             // -?Esprinet?
064200091001             when  §AR8tsd = 'E1';
064300091001               xE1 += 1;
064400091001               $AR8_E1(xE1) = TBEke1;
064500091001
064600091001             // -?Karnak?
064700091001             when  §AR8tsd = 'K1';
064800091001               xK1 += 1;
064900091001               $AR8_K1(xK1) = TBEke1;
065000091001
065100091001             // -?Logistica, Sait e Brumar?
065200091001             when  %subst(§AR8tsd : 1 : 1) = 'L';
065300091001               xLx += 1;
065400091001               $AR8_Lx(xLx) = TBEke1;
065500091001
065600091001             // -?Standard senza logo?
065700091001             when  §AR8tsd = 'S1';
065800091001               xS1 += 1;
065900091001               $AR8_S1(xS1) = TBEke1;
066000091001
066100091001             // -?Standard con logo?
066200091001             //   (NON memorizzati in schiera)
066300091001             when  §AR8tsd = 'S2';
066400091001
066500091001             // -?Modulo personalizzato (vedi Arcte)?
066600091001             when  §AR8tsd = '00';
066700091001               x00 += 1;
066800091001               $AR8_00(x00) = TBEke1;
066900091001
067000091001             // -?Tipo Modulo NON PREVISTO!!!?
067100091001             other;
067200091002               wErr = -2;
067300091002               exsr  sr_Print;
067400091001
067500091001           endsl;
067600091001
067700091001           reade  %kds(k05tntbe01 : 1)  TNTBE000;
067800091001
067900091001         EndDo;
068000091001
068100091001       ENDSR;
068200091001
068300091001       //--------------------------------------------------------------
068400091001       //?Intabellamento pgm di stampa LdV per filiale (da tab. "5P/B")?
068500091001       //--------------------------------------------------------------
068600091001       BEGSR  sr_InTab5Pb;
068700091001
068800091001         // -?Intabellam. filiali & relativi pgm di stampa da tab. "5P"?
068900091001         clear  xx;
069000091001         //clear  $5PBfil;             (già/ancora così)
069100091001         //clear  $5PBpgm;             (già/ancora così)
069200091002         setll  (1 : '5P' : 'B   ')  TABEL;
069300091002         reade  (1 : '5P')  TABEL;
069400091001         DoW  Not %eof(TABEL00F);
069500091001           select;
069600091001             when  %subst(TBLkey : 1 : 1) <> 'B';
069700091001               leave;
069800091001             when  TBLflg = *blank;
069900091001               xx   += 1;
070000091001               ds5PB = TBLuni;
070100091002               $5PBfil(xx) = %subst( TBLkey : 2 : 3 );
070200091001               $5PBpgm(xx) = §5Pbpl;
070300091001           endsl;
070400091002         reade  (1 : '5P')  TABEL;
070500091001         EndDo;
070600091001
070700091001         yy = xx + 1;
070800091001         $5PBfil(yy) = *loval;
070900091001         $5PBpgm(yy) = 'FNLSB6R';
071000091001
071100091001       ENDSR;
071200091001
071300091001       //--------------------------------------------------------------
071400091001       //?Preparazione stringa SQL                                    ?
071500091001       //--------------------------------------------------------------
071600091002       BEGSR  sr_PrepSQL_1;
071700091001
071800091001         // - Impostazione parametri ricevuti come parzializzazioni
071900091001         //   nella stringa SQL
072000091001         //
072100091001         //  ? E S E M P I O : ?
072200091001         //
072300091002         //   select  *
072400091001         //   from    FNARB00F
072500091001         //   where  (ARBncd > 0   or   ARBfns = 'S')
072600091002         //      and  ARBddc = :VRdtdc
072700091002         //      and  ARBifp in (063, 005, 050, 032)
072800091001         //   order   by  ARBddc,ARBifp, ARBaas,ARBlnp,ARBnrs,ARBnsp
072900091001         //   for     FETCH  only
073000091001         //
073100091001         // -? N.B. ?
073200091001         //   · Seleziona le LdV in distinta del giorno ricevuto
073300091001
073400091002         clear  wSql_1;
073500091002         wSql_1 = %trimr($Sql_1(01)) + ' '
073600091002                +  %trim($Sql_1(02)) + ' '
073700091002                +  %trim($Sql_1(03)) + ' '
073800091002                +  %trim($Sql_1(04)) + ' ' + %char(SMdtdc) + ' '
073900091002                                           // Data Distinta Consegna
074000091002                +  %trim($Sql_1(05)) + w_Fil + ') '
074100091002                                     // - Filiali da elaborare
074200091002         // -?ordinamento?
074300091002                +  %trim($Sql_1(06)) + ' '
074400091001         // -?for fetch only?
074500091002                +  c_ForFetch;
074600091001
074700091001       ENDSR;
074800091001
074900091001       //--------------------------------------------------------------
075000091001       //?Apertura cursore.                                            ?
075100091001       //--------------------------------------------------------------
075200091001       BEGSR  sr_OpenCursor_1;
075300091001
075400091001         // -?Dichiarazione cursore?
075500091002         exec sql   prepare S1   from :wSQL_1;
075600091001         exec sql   declare C1   cursor for S1;
075700091001
075800091001         // -?Apertura del cursore?
075900091001         exec sql   open C1;
076000091001
076100091001         if  SQLcode < *zero;
076200091001           $Err = *on;
076300091001           $EoF = *on;
076400091001           exsr  sr_PrintErrSQL;
076500091001         endif;
076600091001
076700091001       ENDSR;
076800091001
076900091001       //--------------------------------------------------------------
077000091001       //?Chiusura cursore.                                            ?
077100091001       //--------------------------------------------------------------
077200091001       BEGSR  sr_CloseCursor_1;
077300091001
077400091001         // -?Stampa totale ultima filiale?
077500091005         if  prt_NrA4 <> *zero  or  prt_NrA5 <> *zero   or
077600091005             prt_NrColli <> *zero;
077700091002           exsr  sr_Print;
077800091001         endif;
077900091005         if  %open(QSYSPRT);
078000091005           except  PRTend;
078100091005         endif;
078200091001
078300091001         // -?Richiamo dei pgm. di stampa LdV per chiusura?
078400091001         clear  FNLSB5ds;
078500091001         DB0tla = 'C';
078600091001         fnlsB5r ( FNLSB5ds );
078700091001         fnlsB6r ( FNLSB5ds );
078800091001
078900091001         //?Chiusura del cursore?
079000091001         exec sql   close C1;
079100091001
079200091001       ENDSR;
079300091001
079400091001       //--------------------------------------------------------------
079500091001       //?Lettura cursore.                                             ?
079600091001       //--------------------------------------------------------------
079700091001       BEGSR  sr_ReadCursor_1;
079800091001
079900091005         clear  FNARBds;
080000091001
080100091001         exec sql   fetch NEXT from C1 into :FNARBds;
080200091001
080300091001         select;
080400091001           when  SQLcode = 100;
080500091001             $EoF = *on;
080600091001           when  SQLcode < *zero;
080700091001             $Err = *on;
080800091001             $Eof = *on;
080900091001           other;
081000091001             exsr  sr_Elab_1;
081100091001         endsl;
081200091001
081300091001       ENDSR;
081400091001
081500091001       //--------------------------------------------------------------
081600091001       //?Elaborazione singolo record da FNARB00F                      ?
081700091001       //--------------------------------------------------------------
081800091001       BEGSR  sr_Elab_1;
081900091001
082000091002         // -?Test rottura di livello (filiale)?
082100091002         IF  ARBddc <> SAVddc   or   ARBifp <> prt_Fil;
082200091002           // - Stampa totali della filiale precedente
082300091002           if  prt_Data <> *blank   and   prt_Fil <> *zero;
082400091002             exsr  sr_Print;
082500091002           endif;
082600091002           // - Memorizzazione nuovi dati e ripulizia nuovi totali
082700091002           SAVddc = ARBddc;
082800091002           reset  WLBdat;
082900091002           G08inv = ARBddc;
083000091002           xsrDA8 ( WLBdat );
083100091002           if  G08err = *off;
083200091002             prt_Data = %editc( G08dat : 'Y');
083300091002           else;
083400091002             prt_Data = %editw( ARBddc : '    /  /0 ');
083500091002           endif;
083600091002           prt_Fil  = ARBifp;
083700091002           clear  prt_NrA4;
083800091002           clear  prt_NrA5;
083900091002           clear  prt_NrDDT;
084000091002           clear  prt_PagDDT;
084100091005           clear  prt_NrColli;
084200091002         ENDIF;
084300091002
084400091001         // -?Reperimento pgm di stampa LdV per la LNA della spedizione?
084500091001         exsr  sr_Rep5PxLNA;
084600091005
084700091005         // -?Conteggio Nr. Colli?
084800091005         prt_NrColli += ARBncl;
084900091001
085000091002         // -?Richiamo del pgm. di stampa LdVSOLO?per ricavarne il?
085100091001         //  ?tipo modulo (A4/A5) richiesto dalla LdV in esame?
085200091001         reset  FNLSB5ds;
085300091001         DB0aas = ARBaas;
085400091001         DB0lnp = ARBlnp;
085500091001         DB0nrs = ARBnrs;
085600091001         DB0nsp = ARBnsp;
085700091001         //DB0tbo = 'A';               - (già così)
085800091001         //DB0ris = 'R';               - (già così)
085900091001         //DB0fl9 = *on;               - (già così)
086000091001         select;
086100091001           when  $5PBpgm(xx) = 'FNLSB5R ';
086200091001             fnlsB5r ( FNLSB5ds );
086300091001           when  $5PBpgm(xx) = 'FNLSB6R ';
086400091001             fnlsB6r ( FNLSB5ds );
086500091001         endsl;
086600091001
086700091001         // -?Conteggio LdV per tipo modulo?
086800091001         if  DB0tms = '4';
086900091001           prt_NrA4 += 1;
087000091001         else;
087100091001           prt_NrA5 += 1;
087200091001         endif;
087300091001
087400091002         // -?Conteggio numero e pagine DDT?
087500091002         //  ? N.B. ?scartate le LdV con:?
087600091002         //   · senza segnacolli
087700091002         //   · ARBKSC = xxx9999
087800091002         //   · codice bolla <> "1 ", "4 "e "5 " (immesse a video)
087900091002         if  DB0tms <> '4'                                      or
088000091002             %subst( %editc( ARBksc : 'X' ) : 4 : 4 ) = '9999'  or
088100091002            ($CBO(1)+$CBO(2)+$CBO(3)+$CBO(4)+$CBO(5)+
088200091002             $CBO(6)+$CBO(7)+$CBO(8)+$CBO(9)+$CBO(10) <> *blank and
088300091002             %lookup( ARBcbo : $CBO ) = *zero);
088400091002           leavesr;
088500091002         endif;
088600091002         //   · dati in FIAR801L
088700091002         setll  (ARBaas : ARBlnp :  ARBnrs : ARBnsp)  FIAR8000;
088800091002         if  Not  %equal(FIAR801L);
088900091002           leavesr;
089000091002         endif;
089100130318         setgt  (ARBaas : ARBlnp :  ARBnrs : ARBnsp : '§999')  FIAR8000;
089200130318         reade  (ARBaas : ARBlnp :  ARBnrs : ARBnsp)  FIAR8000;
089300130318         if  %eof(FIAR801L);
089400130318           leavesr;
089500130318         endif;
089600091002
089700091002         // -?Conteggio LdV con Packing-List?
089800091002         prt_NrDDT += 1;
089900091001
090000091002         // -?Preparazione stringa SQL?
090100091002         exsr  sr_PrepSQL_2;
090200091002
090300091002         // -?Ciclo di elaborazione?
090400091002         exsr  sr_OpenCursor_2;
090500091002
090600091002         dou  $EoF = *on;
090700091002           exsr  sr_ReadCursor_2;
090800091002         enddo;
090900091002
091000091002         exsr  sr_CloseCursor_2;
091100091001
091200091001       ENDSR;
091300091002
091400091002       //--------------------------------------------------------------
091500091002       //?Reperimento pgm di stampa LdV da tab. "5P"/"B" per la LNA    ?
091600091002       //--------------------------------------------------------------
091700091002       BEGSR  sr_Rep5PxLNA;
091800091002
091900091002         if  %editc(ARBlna : 'X') <> $5PBfil(xx);
092000091002
092100091002           xx = %lookup( %editc(ARBlna : 'X') : $5PBfil );
092200091002
092300091002           // - Se filiale non reperita, forzo "FNLSB6R"
092400091002           //   [memorizzato in $5PBpgm(yy)]
092500091002           if  xx = *zero;
092600091002             xx = yy;
092700091002           endif;
092800091002
092900091002         endif;
093000091002
093100091002       ENDSR;
093200091001
093300091001       //--------------------------------------------------------------
093400091001       //?Preparazione stringa SQL                                     ?
093500091001       //--------------------------------------------------------------
093600091001       BEGSR  sr_PrepSQL_2;
093700091001
093800091001         // -?Impostazione parametri ricevuti come parzializzazioni?
093900091001         //  ?nella stringa SQL?
094000091001         //
094100091001         //  ? E S E M P I O : ?
094200091001         //
094300091001         //   select AR8tmd,
094400091001         //          AR8aas, AR8lnp, AR8nrs, AR8nsp,
094500091001         //          '* ', count(*)
094600091001         //   from   FIAR800F
094700091001         //   where  AR8aas = :BLPaas
094800091001         //     and  AR8lnp = :BLPlnp
094900091001         //     and  AR8nrs = :BLPnrs
095000091001         //     and  AR8nsp = :BLPnsp
095100091001         //     and  AR8atb = ' '
095200091001         //     and  AR8tmd = 'STD1'
095300091001         //   group  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
095400091001         //   UNION
095500091001         //   select 'STD2' as AR8tmd,
095600091001         //          AR8aas, AR8lnp, AR8nrs, AR8nsp,
095700091001         //          '* ', count(*)
095800091001         //   from   FIAR800F
095900091001         //   where  AR8aas = :BLPaas
096000091001         //     and  AR8lnp = :BLPlnp
096100091001         //     and  AR8nrs = :BLPnrs
096200091001         //     and  AR8nsp = :BLPnsp
096300091001         //     and  AR8atb = ' '
096400091001         //     and  AR8tmd NOT in  ('STD1', 'KARN', 'EDK ', 'TITA',
096500091001         //                          'SAIT', 'ARCT', 'ESP1')
096600091001         //     and  AR8tmd NOT like 'LOG%'
096700091001         //   group  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
096800091001         //   UNION
096900091001         //   select AR8tmd,
097000091001         //          AR8aas, AR8lnp, AR8nrs, AR8nsp,
097100091001         //          'C*', count(*)
097200091001         //   from   FIAR800F
097300091001         //   where  AR8aas = :BLPaas
097400091001         //     and  AR8lnp = :BLPlnp
097500091001         //     and  AR8nrs = :BLPnrs
097600091001         //     and  AR8nsp = :BLPnsp
097700091001         //     and  AR8atb = ' '
097800091001         //     and (AR8tmd in  ('KARN', 'EDK ', 'TITA', 'SAIT', 'ARCT', 'ESP1')
097900091001         //      or  AR8tmd like 'LOG%')
098000091001         //     and  AR8trk in  ('C1', 'C2', 'C3', 'O1', 'O2')
098100091001         //   group  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
098200091001         //   UNION
098300091001         //   select AR8tmd,
098400091001         //          AR8aas, AR8lnp, AR8nrs, AR8nsp,
098500091001         //          'Z1', count(*)
098600091001         //   from   FIAR800F
098700091001         //   where  AR8aas = :BLPaas
098800091001         //     and  AR8lnp = :BLPlnp
098900091001         //     and  AR8nrs = :BLPnrs
099000091001         //     and  AR8nsp = :BLPnsp
099100091001         //     and  AR8atb = ' '
099200091001         //     and (AR8tmd in  ('SAIT')
099300091001         //      or  AR8tmd like 'LOG%')
099400091001         //     and  AR8trk in  ('Z1')
099500091001         //   group  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
099600091001         //   UNION
099700091001         //   select AR8tmd,
099800091001         //          AR8aas, AR8lnp, AR8nrs, AR8nsp,
099900091001         //          'S*', count(*)
100000091001         //   from   FIAR800F
100100091001         //   where  AR8aas = :BLPaas
100200091001         //     and  AR8lnp = :BLPlnp
100300091001         //     and  AR8nrs = :BLPnrs
100400091001         //     and  AR8nsp = :BLPnsp
100500091001         //     and  AR8atb = ' '
100600091001         //     and (AR8tmd in  ('SAIT')
100700091001         //      or  AR8tmd like 'LOG%')
100800091001         //     and  AR8trk in  ('S2', 'S3')
100900091001         //   group  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
101000091001         //
101100091001         //   order  by  AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp
101200091001         //   for    fetch  only
101300091001
101400091002         clear  wSql_2;
101500091001
101600091001         // -?DDT standard senza logo ("STD1"): trk T1, T2, C1?
101700091002         wSql_2 = %trimr($Sql_2( 1)) + ' '
101800091002                +  %trim($Sql_2( 2)) + ' '
101900091002                +  %trim($Sql_2( 4)) + ' '
102000091002                +  %trim($Sql_2( 5)) + ' '
102100091002                +  %trim($Sql_2( 9)) + ' '
102200091002                +  %trim($Sql_2(10)) + ' ' + %editc(arbAAS : 'X') + ' '
102300091002                +  %trim($Sql_2(11)) + ' ' + %editc(arbLNP : 'X') + ' '
102400091002                +  %trim($Sql_2(12)) + ' ' + %editc(arbNRS : 'X') + ' '
102500091002                +  %trim($Sql_2(13)) + ' ' + %char(arbNSP) + ' '
102600091002                +  %trim($Sql_2(14)) + ' '
102700091002                +  %trim($Sql_2(15)) + ' '
102800091002                +  %trim($Sql_2(21)) + ' '
102900091001
103000091002                + c_Union
103100091001
103200091001         // -?DDT standard  con  logo ("STD2"): trk T1, T2, C1?
103300091002                + %trimr($Sql_2( 1)) + ' '
103400091002                +  %trim($Sql_2( 3)) + ' '
103500091002                +  %trim($Sql_2( 4)) + ' '
103600091002                +  %trim($Sql_2( 5)) + ' '
103700091002                +  %trim($Sql_2( 9)) + ' '
103800091002                +  %trim($Sql_2(10)) + ' ' + %editc(arbAAS : 'X') + ' '
103900091002                +  %trim($Sql_2(11)) + ' ' + %editc(arbLNP : 'X') + ' '
104000091002                +  %trim($Sql_2(12)) + ' ' + %editc(arbNRS : 'X') + ' '
104100091002                +  %trim($Sql_2(13)) + ' ' + %char(arbNSP) + ' '
104200091002                +  %trim($Sql_2(14)) + ' '
104300091002                +  %trim($Sql_2(16));
104400091001         // -?Intabellamento loghi da scartare (NON di tipo "S2"):?
104500091001         // - -?di tipo "E1" (Esprinet)?
104600091002         For  ww = 1  To xE1;
104700091002           if  $AR8_E1(ww) <> *blank;
104800091002             if  ww = 1;
104900091002               wSql_2 += '''' + $AR8_E1(ww) + '''';
105000091001             else;
105100091002               wSql_2 += ', ''' + $AR8_E1(ww) + '''';
105200091001             endif;
105300091001           else;
105400091001             leave;
105500091001           endif;
105600091001         EndFor;
105700091001         // - -?di tipo "K1" (Karnak)?
105800091002         For  ww = 1  To xK1;
105900091002           if  $AR8_K1(ww) <> *blank;
106000091002             if  ww = 1   and   xE1 = *zero;
106100091002               wSql_2 += '''' + $AR8_K1(ww) + '''';
106200091001             else;
106300091002               wSql_2 += ', ''' + $AR8_K1(ww) + '''';
106400091001             endif;
106500091001           else;
106600091001             leave;
106700091001           endif;
106800091001         EndFor;
106900091001         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
107000091002         For  ww = 1  To xLx;
107100091002           if  $AR8_Lx(ww) <> *blank;
107200091002             if  ww = 1   and   xE1 = *zero
107300091001                          and   xK1 = *zero;
107400091002               wSql_2 += '''' + $AR8_Lx(ww) + '''';
107500091001             else;
107600091002               wSql_2 += ', ''' + $AR8_Lx(ww) + '''';
107700091001             endif;
107800091001           else;
107900091001             leave;
108000091001           endif;
108100091001         EndFor;
108200091001         // - -?di tipo "S1" (Std senza logo)?
108300091002         For  ww = 1  To xS1;
108400091002           if  $AR8_S1(ww) <> *blank;
108500091002             if  ww = 1   and   xE1 = *zero
108600091001                          and   xK1 = *zero
108700091001                          and   xLx = *zero;
108800091002               wSql_2 += '''' + $AR8_S1(ww) + '''';
108900091001             else;
109000091002               wSql_2 += ', ''' + $AR8_S1(ww) + '''';
109100091001             endif;
109200091001           else;
109300091001             leave;
109400091001           endif;
109500091001         EndFor;
109600091001         // - -?di tipo "00" (personalizzato - v. Arcte)?
109700091002         For  ww = 1  To x00;
109800091002           if  $AR8_00(ww) <> *blank;
109900091002             if  ww = 1   and   xE1 = *zero
110000091001                          and   xK1 = *zero
110100091001                          and   xLx = *zero
110200091001                          and   xS1 = *zero;
110300091002               wSql_2 += '''' + $AR8_00(ww) + '''';
110400091001             else;
110500091002               wSql_2 += ', ''' + $AR8_00(ww) + '''';
110600091001             endif;
110700091001           else;
110800091001             leave;
110900091001           endif;
111000091001         EndFor;
111100091001         // - - "chiusura" stringa sql
111200091005         wSql_2 += ') '
111300091005                +  %trim($Sql_2(21))
111400091001
111500091002                +  c_Union
111600091001
111700091001         // -?Dettaglio in DDT:?
111800091001         //  ?· Logistica o Sait          : trk C1, C2, C3, C4, C5?
111900091001         //  ?· DDT Karnak, EDk o Titanedi: trk C1, C2, C3, O1, O2?
112000091001         //  ?· DDT Arcte                 : trk C1, C2?
112100091001         //  ?· DDT Esprinet              : trk C1, C2?
112200091002                + %trimr($Sql_2( 1)) + ' '
112300091002                +  %trim($Sql_2( 2)) + ' '
112400091002                +  %trim($Sql_2( 4)) + ' '
112500091002                +  %trim($Sql_2( 6)) + ' '
112600091002                +  %trim($Sql_2( 9)) + ' '
112700091002                +  %trim($Sql_2(10)) + ' ' + %editc(arbAAS : 'X') + ' '
112800091002                +  %trim($Sql_2(11)) + ' ' + %editc(arbLNP : 'X') + ' '
112900091002                +  %trim($Sql_2(12)) + ' ' + %editc(arbNRS : 'X') + ' '
113000091002                +  %trim($Sql_2(13)) + ' ' + %char(arbNSP) + ' '
113100091002                +  %trim($Sql_2(14)) + ' '
113200091002                +  %trim($Sql_2(17));
113300091001         // -?Intabellamento loghi da eleborare:?
113400091001         // - -?di tipo "E1" (Esprinet)?
113500091002         For  ww = 1  To xE1;
113600091002           if  $AR8_E1(ww) <> *blank;
113700091002             if  ww = 1;
113800091002               wSql_2 += '''' + $AR8_E1(ww) + '''';
113900091001             else;
114000091002               wSql_2 += ', ''' + $AR8_E1(ww) + '''';
114100091001             endif;
114200091001           else;
114300091001             leave;
114400091001           endif;
114500091001         EndFor;
114600091001         // - -?di tipo "K1" (Karnak)?
114700091002         For  ww = 1  To xK1;
114800091002           if  $AR8_K1(ww) <> *blank;
114900091002             if  ww = 1   and   xE1 = *zero;
115000091002               wSql_2 += '''' + $AR8_K1(ww) + '''';
115100091001             else;
115200091002               wSql_2 += ', ''' + $AR8_K1(ww) + '''';
115300091001             endif;
115400091001           else;
115500091001             leave;
115600091001           endif;
115700091001         EndFor;
115800091001         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
115900091002         For  ww = 1  To xLx;
116000091002           if  $AR8_Lx(ww) <> *blank;
116100091002             if  ww = 1   and   xE1 = *zero
116200091001                          and   xK1 = *zero;
116300091002               wSql_2 += '''' + $AR8_Lx(ww) + '''';
116400091001             else;
116500091002               wSql_2 += ', ''' + $AR8_Lx(ww) + '''';
116600091001             endif;
116700091001           else;
116800091001             leave;
116900091001           endif;
117000091001         EndFor;
117100091001         // - -?di tipo "00" (personalizzato - v. Arcte)?
117200091002         For  ww = 1  To x00;
117300091002           if  $AR8_00(ww) <> *blank;
117400091002             if  ww = 1   and   xE1 = *zero
117500091001                          and   xK1 = *zero
117600091001                          and   xLx = *zero
117700091001                          and   xS1 = *zero;
117800091002               wSql_2 += '''' + $AR8_00(ww) + '''';
117900091001             else;
118000091002               wSql_2 += ', ''' + $AR8_00(ww) + '''';
118100091001             endif;
118200091001           else;
118300091001             leave;
118400091001           endif;
118500091001         EndFor;
118600091001         // - -?"chiusura" stringa sql?
118700091002         wSql_2 += ') '
118800091002                +  %trim($Sql_2(18)) + ' '
118900091002                +  %trim($Sql_2(21))
119000091001
119100091002              + c_Union
119200091001
119300091001         // -?Packing-List in DDT Logistica, Sait o Brumar: trk Z1?
119400091002              + %trimr($Sql_2( 1)) + ' '
119500091002              +  %trim($Sql_2( 2)) + ' '
119600091002              +  %trim($Sql_2( 4)) + ' '
119700091002              +  %trim($Sql_2( 7)) + ' '
119800091002              +  %trim($Sql_2( 9)) + ' '
119900091002              +  %trim($Sql_2(10)) + ' ' + %editc(arbAAS : 'X') + ' '
120000091002              +  %trim($Sql_2(11)) + ' ' + %editc(arbLNP : 'X') + ' '
120100091002              +  %trim($Sql_2(12)) + ' ' + %editc(arbNRS : 'X') + ' '
120200091002              +  %trim($Sql_2(13)) + ' ' + %char(arbNSP) + ' '
120300091002              +  %trim($Sql_2(14)) + ' '
120400091002              +  %trim($Sql_2(17));
120500091001         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
120600091002         For  ww = 1  To xLx;
120700091002           if  $AR8_Lx(ww) <> *blank;
120800091002             if  ww = 1;
120900091002               wSql_2 += '''' + $AR8_Lx(ww) + '''';
121000091001             else;
121100091002               wSql_2 += ', ''' + $AR8_Lx(ww) + '''';
121200091001             endif;
121300091001           else;
121400091001             leave;
121500091001           endif;
121600091001         EndFor;
121700091001         // - -?"chiusura" stringa sql?
121800091002         wSql_2 += ') '
121900091005                +  %trim($Sql_2(19)) + ' '
122000091002                +  %trim($Sql_2(21))
122100091001
122200091002              + c_Union
122300091001
122400091001         // -?Fattura in DDT Logistica, Sait o Brumar: trk S2, S3?
122500091002              + %trimr($Sql_2( 1)) + ' '
122600091002              +  %trim($Sql_2( 2)) + ' '
122700091002              +  %trim($Sql_2( 4)) + ' '
122800091002              +  %trim($Sql_2( 8)) + ' '
122900091002              +  %trim($Sql_2( 9)) + ' '
123000091002              +  %trim($Sql_2(10)) + ' ' + %editc(arbAAS : 'X') + ' '
123100091002              +  %trim($Sql_2(11)) + ' ' + %editc(arbLNP : 'X') + ' '
123200091002              +  %trim($Sql_2(12)) + ' ' + %editc(arbNRS : 'X') + ' '
123300091002              +  %trim($Sql_2(13)) + ' ' + %char(arbNSP) + ' '
123400091002              +  %trim($Sql_2(14)) + ' '
123500091002              +  %trim($Sql_2(17));
123600091001         // - -?di tipo "L*" (Logistica, Sait e Brumar)?
123700091002         For  ww = 1  To xLx;
123800091002           if  $AR8_Lx(ww) <> *blank;
123900091002             if  ww = 1;
124000091002               wSql_2 += '''' + $AR8_Lx(ww) + '''';
124100091001             else;
124200091002               wSql_2 += ', ''' + $AR8_Lx(ww) + '''';
124300091001             endif;
124400091001           else;
124500091001             leave;
124600091001           endif;
124700091001         EndFor;
124800091001         // - -?"chiusura" stringa sql?
124900091002         wSql_2 += ') '
125000091002                +  %trim($Sql_2(20)) + ' '
125100091002                +  %trim($Sql_2(21))
125200091001
125300091001         // -?Order by?
125400091002              + c_OrderBy
125500091001
125600091001         // -?For fetch only?
125700091002              + c_ForFetch;
125800091001
125900091001       ENDSR;
126000091001
126100091001       //--------------------------------------------------------------
126200091001       //?Apertura cursore.                                            ?
126300091001       //--------------------------------------------------------------
126400091001       BEGSR  sr_OpenCursor_2;
126500091001
126600091001         // -?Dichiarazione cursore?
126700091002         exec sql   prepare S2   from :wSQL_2;
126800091002         exec sql   declare C2   cursor for S2;
126900091001
127000091001         // -?Apertura del cursore?
127100091002         exec sql   open C2;
127200091001
127300091001         if  SQLcode < *zero;
127400091001           $Err = *on;
127500091001           $EoF = *on;
127600091001           exsr  sr_PrintErrSQL;
127700091001         endif;
127800091001
127900091001       ENDSR;
128000091001
128100091001       //--------------------------------------------------------------
128200091001       //?Chiusura cursore.                                            ?
128300091001       //--------------------------------------------------------------
128400091001       BEGSR  sr_CloseCursor_2;
128500091001
128600091001         // -?Chiusura del cursore?
128700091002         exec sql   close C2;
128800091002
128900091002         $EoF = *off;
129000091001
129100091001       ENDSR;
129200091001
129300091001       //--------------------------------------------------------------
129400091001       //?Lettura cursore.                                             ?
129500091001       //--------------------------------------------------------------
129600091001       BEGSR  sr_ReadCursor_2;
129700091001
129800091001         clear  wSQLds;
129900091001
130000091002         exec sql   fetch next   from C2   into :wSQLds;
130100091001
130200091001         select;
130300091001           when  SQLcode = 100;
130400091001             $EoF = *on;
130500091001           when  SQLcode < *zero;
130600091001             $Err = *on;
130700091001             $Eof = *on;
130800091001             exsr  sr_PrintErrSQL;
130900091001           other;
131000091002             exsr  sr_Elab_2;
131100091001         endsl;
131200091001
131300091001       ENDSR;
131400091001
131500091001       //--------------------------------------------------------------
131600091001       //?Elaborazione singolo record da di totale                     ?
131700091001       //--------------------------------------------------------------
131800091001       BEGSR  sr_Elab_2;
131900091001
132000091001         // -?Reperimento tipologia di DDT?
132100091005         clear  yS1;
132200091005         clear  yE1;
132300091005         clear  yK1;
132400091005         clear  y00;
132500091005         clear  yLx;
132600091005         yS1 = %lookup( SQLtmd : $AR8_S1 );
132700091005         yE1 = %lookup( SQLtmd : $AR8_E1 );
132800091005         yK1 = %lookup( SQLtmd : $AR8_K1 );
132900091005         y00 = %lookup( SQLtmd : $AR8_00 );
133000091005         yLx = %lookup( SQLtmd : $AR8_Lx );
133100091001
133200091001         //?Calcolo numero pagine per DDT in esame?
133300091001
133400091001         clear  wNrPag;
133500091001         clear  wResto;
133600091001
133700091001         SELECT;
133800091001
133900091001           // -?DDT standard con o senza logo (trk T1, T2, C1)?
134000091001           // · 42 righe x pagina
134100091005           WHEN  yS1              > *zero   or
134200091005                (yE1+yK1+y00+yLx) = *zero;
134300091001             wNrPag = %div( SQlrec : 42 );
134400091001             wResto = %rem( SQLrec : 42 );
134500091001
134600091001           // -?DDT Logistica, Sait o Brumar?
134700091005           WHEN  yLx > *zero;
134800091001             select;
134900091001               // - rec. di "corpo" (trk C1, C2, C3):
135000091001               //   24 righe x pagina con   testata e con   piede
135100091001               //   28 righe x pagina con   testata e senza piede
135200091001               //   39 righe x pagina senza testata e senza piede
135300091001               //   35 righe x pagina senza testata e con   piede
135400091001               when  SQLtrk = 'C*'   and   SQLrec <= 24;
135500091001                 wNrPag  = 1;
135600091001               when  SQLtrk = 'C*'   and   SQLrec <= (28 + 35);
135700091001                 wNrPag  = 2;
135800091001               when  SQLtrk = 'C*'   and   SQLrec >  (28 + 35);
135900091001                 wResto  = SQLrec - (28 + 35);
136000091001                 wNrPag  = %div( wResto : 39 );
136100091001                 wResto  = %rem( wResto : 39 );
136200091001                 wNrPag += 2;
136300091001               // - rec. di "packing-list" (trk Z1):
136400091001               //   29 righe x pagina con   testata (NO piede)
136500091001               //   40 righe x pagina senza testata (NO piede)
136600091001               when  SQLtrk = 'Z1'   and   SQLrec <= 29;
136700091001                 wNrPag  = 1;
136800091001               when  SQLtrk = 'Z1'   and   SQLrec >  29;
136900091001                 wResto  = SQLrec - 29;
137000091001                 wNrPag  = %div( wResto : 40 );
137100091001                 wResto  = %rem( wResto : 40 );
137200091001                 wNrPag += 1;
137300091001               // - rec. di "fattura" (trk S2, S3):
137400091001               //   26 righe x pagina con   testata e con   piede
137500091001               //   29 righe x pagina con   testata e senza piede
137600091001               //   42 righe x pagina senza testata e senza piede
137700091001               //   39 righe x pagina senza testata e con   piede
137800091001               when  SQLtrk = 'S*'   and   SQLrec <= 26;
137900091001                 wNrPag  = 1;
138000091001               when  SQLtrk = 'S*'   and   SQLrec <= (29 + 39);
138100091001                 wNrPag  = 2;
138200091001               when  SQLtrk = 'S*'   and   SQLrec >  (29 + 39);
138300091001                 wResto  = SQLrec - (29 + 39);
138400091001                 wNrPag  = %div( wResto : 42 );
138500091001                 wResto  = %rem( wResto : 42 );
138600091001                 wNrPag += 2;
138700091001             endsl;
138800091001
138900091001           // .?DDT Karnak, EDk o Titanedi?
139000091001           // · 19 righe x pagina (omaggi ESCLUSI)
139100091005           WHEN  yK1 > *zero;
139200091001             if  SQLtrk = 'C*';
139300091001                 wNrPag  = %div( SQLrec : 19 );
139400091001                 wResto  = %rem( SQLrec : 19 );
139500091001             endif;
139600091001
139700091001           // -?DDT Arcte?
139800091005           WHEN  y00 > *zero;
139900091001             select;
140000091001               // - rec. di "corpo" (trk C1, C2):
140100091001               //   14 righe x pagina con   testata e con   piede
140200091001               //   29 righe x pagina con   testata e senza piede
140300091001               //   40 righe x pagina senza testata e senza piede
140400091001               //   36 righe x pagina senza testata e con   piede
140500091001               when  SQLtrk = 'C*'   and   SQLrec <= 25;
140600091001                 wNrPag  = 1;
140700091001               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 36);
140800091001                 wNrPag  = 2;
140900091001               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 36);
141000091001                 wResto  = SQLrec - (29 + 36);
141100091001                 wNrPag  = %div( wResto : 40 );
141200091001                 wResto  = %rem( wResto : 40 );
141300091001                 wNrPag += 2;
141400091001             endsl;
141500091001
141600091001           // -?DDT Esprinet?
141700091005           WHEN  yE1 > *zero;
141800091001             select;
141900091001               // - rec. di "corpo" (trk C1, C2)
142000091001               //   18 righe x pagina con   testata e con   piede
142100091001               //   29 righe x pagina con   testata e senza piede
142200091001               //   39 righe x pagina senza testata e senza piede
142300091001               //   28 righe x pagina senza testata e con   piede
142400091001               when  SQLtrk = 'C*'   and   SQLrec <= 18;
142500091001                 wNrPag  = 1;
142600091001               when  SQLtrk = 'C*'   and   SQLrec <= (29 + 28);
142700091001                 wNrPag  = 2;
142800091001               when  SQLtrk = 'C*'   and   SQLrec >  (29 + 28);
142900091001                 wResto  = SQLrec - (29 + 28);
143000091001                 wNrPag  = %div( wResto : 39 );
143100091001                 wResto  = %rem( wResto : 39 );
143200091001                 wNrPag += 2;
143300091001             endsl;
143400091001
143500091001         ENDSL;
143600091001
143700091001         if  wResto > *zero;
143800091001           wNrPag += 1;
143900091001         endif;
144000091002
144100091002         // -?Aggiornamento totale pag. A4 x packink-list?
144200091002         prt_PagDDT += wNrPag;
144300091001
144400091001       ENDSR;
144500091002
144600091002       //--------------------------------------------------------------
144700091002       //?Stampa                                                      ?
144800091002       //--------------------------------------------------------------
144900091002       BEGSR  sr_Print;
145000091002
145100091002         // -?Apertura file di stampa?
145200091002         if  Not %open(QSYSPRT);
145300091002           ExecuteCommand ( c_OvrPrtF : %len(c_OvrPrtF) );
145400091002           open   QSYSPRT;
145500091002           except PRTtxt;
145600091002         endif;
145700091002
145800091002         // -?Stampa testata?
145900091002         if  *inOF;
146000091002           except PRTtxt;
146100091002           *inOF = *off;
146200091002         endif;
146300091002
146400091002         // -?Stampa dettaglio o messaggio di errore?
146500091002         select;
146600091002
146700091002           // - Totale filiale
146800091002           when  wErr = *zero;
146900091002             except  PRTtotFIL;
147000091002
147100091002           // - Errore: tab. "TSD" non prevista
147200091002           when  wErr = -1;
147300091002             except  PRTerrTSD;
147400091002
147500091002           // - Errore: tab. "AR8" non prevista
147600091002           when  wErr = -2;
147700091002             except  PRTerrAR8;
147800091002
147900091002           // - Errore: SQL failed
148000091002           when  wErr = -3;
148100091002             except  PRTerrSQL1;
148200091002             *inOF = *off;
148300091002             For  ww = 1  To  %elem($Temp);
148400091002               if  $Temp(ww) = *blank;
148500091002                 leave;
148600091002               endif;
148700091002               if  *inOF;
148800091002                 except  PRTtxt;
148900091002                 *inOF = *off;
149000091002               endif;
149100091002               except  PRTerrSQL2;
149200091002             EndFor;
149300091002
149400091002         endsl;
149500091002
149600091002         clear  wErr;
149700091002
149800091002       ENDSR;
149900091002
150000091002       //--------------------------------------------------------------
150100091002       //?Stampa segnalazione dell'errore rilevato via SQL             ?
150200091002       //--------------------------------------------------------------
150300091002       BEGSR  sr_PrintErrSQL;
150400091002
150500091002         //?Stampa del Dump?
150600091002         Dump(A);
150700091002
150800091002         //?Stampa del Job-Log?
150900091002         Qcmd = 'DSPJOBLOG job(*) output(*print)';
151000091002         ExecuteCommand ( Qcmd : %len(Qcmd) );
151100091002
151200091002         //?Stampa dell'errore in QSYSPRT?
151300091002         clear  ds_Temp;
151400091002         ds_Temp = wSQL_2;
151500091002         wErr = -3;
151600091002         exsr  sr_Print;
151700091002
151800091002         //?Chiusura del programma?
151900091002         exsr  sr_RoutEnd;
152000091002
152100091002       ENDSR;
152200091001
152300091001       //--------------------------------------------------------------
152400091001       //?Operazioni finali.                                           ?
152500091001       //--------------------------------------------------------------
152600091001       BEGSR  sr_RoutEnd;
152700091001
152800091001         //?Chiusura del printer-file?
152900091001         if  %open(QSYSPRT);
153000091001           close  QSYSPRT;
153100091001           ExecuteCommand ( c_DltOvr : %len(c_DltOvr) );
153200091001         endif;
153300091001
153400091001         //?"Chiusura" pgm?
153500091001         return;
153600091001
153700091001       ENDSR;
153800091001
153900091001      /end-free
154000091001
154100091001       //--------------------------------------------------------------
154200091001       //?S P O O L - F I L E S                                        ?
154300091001       //--------------------------------------------------------------
154400091001
154500091001     oQSYSPRT   e            PRTtxt            1
154600091001     o                       RSUT
154700091002     o                                        +  10 'STATISTICA CONSUM-
154800091002     o                                              I LdV e SEGNACOLLI'
154900091002     o                       SDSpgm           +  10
155000091005     o                       *Date         y  +   3
155100091001     o          e            PRTtxt      1
155200091001     o                       KNSIF
155300091001     o                       KNMUS            +   1
155400091002     o                                        +   9 '------------------
155500091002     o                                              ------------------'
155600091002     o                                        +  10 'Pag.'
155700091001     o                       Page          z  +   0
155800091001     o                       wTime            +   5 '  :  :  '
155900091002     o          e            PRTtxt      2
156000091002     o                                              'Data Dist.'
156100091002     o                                        +   3 '   '
156200091002     o                                        +   3 '            '
156300091002     o                                        +   3 '            '
156400091002     o                                        +   3 ' Nr. LdV con'
156500091002     o                                        +   3 ' Nr. pagine '
156600091002     o          e            PRTtxt      1
156700091002     o                                              'Consegna  '
156800091002     o                                        +   3 'Fil'
156900091002     o                                        +   3 ' Nr. LdV A5 '
157000091002     o                                        +   3 ' Nr. LdV A4 '
157100091002     o                                        +   3 'Packing-List'
157200091002     o                                        +   3 'Packing-List'
157300091005     o                                        +   3 ' Segnacolli '
157400091002     o          e            PRTtxt      1
157500091002     o                                              '=========='
157600091002     o                                        +   3 '==='
157700091002     o                                        +   3 '============'
157800091002     o                                        +   3 '============'
157900091002     o                                        +   3 '============'
158000091002     o                                        +   3 '============'
158100091005     o                                        +   3 '============'
158200091002       //
158300091001     o          e            PRTerrTSD   2
158400091001     o                                              'Reperito nuovo -
158500091001     o                                              "Tipo Stampa DDT" -
158600091001     o                                              in tab. "TSD":'
158700091001     o                       TBEke1           +   1
158800091001     o          e            PRTerrTSD   1
158900091002     o                                              'AVVISO: le righe -
159000091001     o                                              di tale tipo nelle-
159100091001     o                                               packing-list verr-
159200091001     o                                              anno contate come -
159300091002     o                                              "STANDARD CON LOGO-
159400091002     o                                              ".'
159500091002       //
159600091001     o          e            PRTerrAR8   2
159700091001     o                                              'Reperito nuovo -
159800091001     o                                              "Tipo Stampa DDT" -
159900091001     o                                              in tab. "AR8":'
160000091001     o                       §AR8tsd          +   1
160100091001     o                                        +   1 'per il logo'
160200091001     o                       TBEke1           +   1
160300091001     o          e            PRTerrAR8   1
160400091001     o                                              'AVVISO: le righe -
160500091001     o                                              della packing-list-
160600091001     o                                               relativa alla spe-
160700091001     o                                              dizione'
160800091001     o                       SQLlnp           +   1
160900091001     o                                        +   0 '/'
161000091001     o                       SQLnrs        z  +   0
161100091001     o                                        +   0 '/'
161200091001     o                       SQLnsp        z  +   0
161300091001     o                                        +   1 'anno'
161400091001     o                       SQLaas        z  +   1
161500091001     o          e            PRTerrAR8   1
161600091001     o                                        +   3 'sono contate come-
161700091001     o                                               "STANDARD CON LOG-
161800091001     o                                              O".'
161900091002       //
162000091002     o          e            PRTerrSQL1  2
162100091001     o                                              'RILEVATO ERRORE: +
162200091001     o                                               SQLCODE'
162300091001     o                       SQLcode       k  +   1
162400091001     o                                        +   1 'SQLSTATE'
162500091001     o                       SQLstate         +   1
162600091001     o                                        +   1 'DURANTE L''ESECUZ-
162700091001     o                                              IONE DEL SEGUENTE -
162800091001     o                                              COMANDO SQL:'
162900091002     o          e            PRTerrSQL1  0  1
163000091001     o                                              'RILEVATO ERRORE: +
163100091001     o                                               SQLCODE'
163200091001     o                       SQLcode       k  +   1
163300091001     o                                        +   1 'SQLSTATE'
163400091001     o                       SQLstate         +   1
163500091002     o          e            PRTerrSQL2  1
163600091002     o                       $Temp(ww)
163700091002       //
163800091002     o          e            PRTtotFIL   2
163900091002     o                       prt_Data
164000091002     o                       prt_Fil          +   3
164100091002     o                       prt_NrA5      j  +   3
164200091002     o                       prt_NrA4      j  +   3
164300091002     o                       prt_NrDDT     j  +   3
164400091002     o                       prt_PagDDT    j  +   3
164500091005     o                       prt_NrColli   j  +   3
164600091002       //
164700091002     o          e            PRTend      2
164800091005     o                                        +  40 '***  Fine  Lista  ***'
164900091001
165000091001       //--------------------------------------------------------------
165100091001       //?S C H I E R E   A   T E M P O   D I   C O M P I L A Z I O N E?
165200091001       //--------------------------------------------------------------
165300091001
165400091001** - $Sql_1 ---------------------------------------------------------*
165500091002select *                                                               1
165600091002  from FNARB00F                                                        2
165700091002 where (ARBncd > 0  or  ARBfns = 'S')                                  3
165800091002   and ARBddc =                                                        4
165900091002   and ARBifp in (                                                     5
166000091002 order by ARBddc, ARBifp, ARBaas, ARBlnp, ARBnrs, ARBnsp               6
166100091001** - $Sql_2 ---------------------------------------------------------*
166200091001select                                                                  1
166300091001       AR8tmd,                                                          2 a1
166400091001       'STD2' as AR8tmd,                                                3 a2
166500091001       AR8aas, AR8lnp, AR8nrs, AR8nsp,                                  4
166600091001       '* ', count(*)                                                   5 b1
166700091001       'C*', count(*)                                                   6 b2
166800091001       'Z1', count(*)                                                   7 b3
166900091001       'S*', count(*)                                                   8 b4
167000091002  from FIAR800F                                                         9
167100091002 where AR8aas =                                                        10
167200091002   and AR8lnp =                                                        11
167300091002   and AR8nrs =                                                        12
167400091002   and AR8nsp =                                                        13
167500130318   and AR8atb = ' ' and AR8tmd not like '§%'                           14
167600091002   and AR8tmd = 'STD1'                                                 15 c1
167700091002   and AR8tmd NOT in (                                                 16 c2
167800091002   and AR8tmd in (                                                     17 c3
167900091002   and (AR8trk like 'C%' or AR8trk like 'O%')                          18 d1
168000091002   and AR8trk in ('Z1')                                                19 d2
168100091002   and AR8trk in ('S2', 'S3')                                          20 d3
168200091002 group by AR8tmd, AR8aas, AR8lnp, AR8nrs, AR8nsp                       21
