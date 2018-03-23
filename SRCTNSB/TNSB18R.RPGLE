000100180306       //==============================================================
000200180306       //? Ricerca nominativi in bolle sede per G.d.F.                  ?
000300180306       //==============================================================
000400180306
000500180306       //--------------------------------------------------------------
000600180306       //? Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700180306       //--------------------------------------------------------------
000800180306
000900180306     /*PRM dbgview(*source)
001000180306     /*END
001100180306
001200180306       //--------------------------------------------------------------
001300180306       //? Specifiche di controllo.                                     ?
001400180306       //--------------------------------------------------------------
001500180306
001600050809     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700180306     h dftactgrp(*no)
001800180306     h bnddir('TRUL')
001900180306
002000180306       //--------------------------------------------------------------
002100180306       //? Dichiarazione file.                                          ?
002200180306       //--------------------------------------------------------------
002300180306
002400180308       // - ?BOLLE SEDE: fatturate "ultimi mesi"  e  da fatturare?
002500180309     fTITAS30C  if   e           k disk    usropn
002600180308     f                                     ignore( TITASP00 )
002700180306       // - ?Anagrafica Autotrasp./Cooperative?
002800180309     fFIAPD01L  if   e           k disk    usropn
002900180306
003000180306       // - ?Video?
003100180306     fTNSB18D   cf   e             workstn
003200180306     f                                     indds( IndDspF )
003300180306     f                                     infds( InfDspF )
003400180306
003500180306       //--------------------------------------------------------------
003600180306       //? Definizione costanti.                                        ?
003700180306       //--------------------------------------------------------------
003800180306
003900180306       // - ?Tasti funzionali a video?
004000180306     d c_F01           c                   const(x'31')
004100180306     d c_F02           c                   const(x'32')
004200180306     d c_F03           c                   const(x'33')
004300180306     d c_F04           c                   const(x'34')
004400180306     d c_F05           c                   const(x'35')
004500180306     d c_F06           c                   const(x'36')
004600180306     d c_F07           c                   const(x'37')
004700180306     d c_F08           c                   const(x'38')
004800180306     d c_F09           c                   const(x'39')
004900180306     d c_F10           c                   const(x'3A')
005000180306     d c_F11           c                   const(x'3B')
005100180306     d c_F12           c                   const(x'3C')
005200180306     d c_F13           c                   const(x'B1')
005300180306     d c_F14           c                   const(x'B2')
005400180306     d c_F15           c                   const(x'B3')
005500180306     d c_F16           c                   const(x'B4')
005600180306     d c_F17           c                   const(x'B5')
005700180306     d c_F18           c                   const(x'B6')
005800180306     d c_F19           c                   const(x'B7')
005900180306     d c_F20           c                   const(x'B8')
006000180306     d c_F21           c                   const(x'B9')
006100180306     d c_F22           c                   const(x'BA')
006200180306     d c_F23           c                   const(x'BB')
006300180306     d c_F24           c                   const(x'BC')
006400180305     d c_Enter         c                   const(x'F1')
006500180306     d c_RollDown      c                   const(x'F4')
006600180305     d c_RollUp        c                   const(x'F5')
006700180306
006800180306       //--------------------------------------------------------------
006900180306       //? Definizione schiere.                                         ?
007000180306       //--------------------------------------------------------------
007100180306
007200180306       // - ?Messaggi di errore?
007300180308     d sk_Msg          s             78    dim(18)  ctdata  perrcd( 1)
007400180306
007500180306
007600180306       //--------------------------------------------------------------
007700180306       //? Definizione aree dati.                                       ?
007800180306       //--------------------------------------------------------------
007900180306
008000180306       // - ?Dati utente?
008100180306     d §AzUte        e ds                  extname(AZUTE00F)
008200180306     d                                     dtaara
008300180306     d §DatiUte      e ds                  extname(dDatiUte)
008400180306     d                                     dtaara
008500180306
008600180306       //--------------------------------------------------------------
008700180306       //? Definizione strutture dati.                                  ?
008800180306       //--------------------------------------------------------------
008900180306
009000180306       // - ?Parametri ricevuti?
009100050809     d KPJBA         e ds
009200180306
009300180306       // - ?Tabella "$3" = Date avanzamento trasferimenti?
009400180306     d ds$3          e ds                  inz   qualified
009500180306
009600180306       // - ?Status ds?
009700180306     d Status         sds
009800180306     d   SDSpgmName      *proc
009900180306
010000180306       // - ?InfDS?
010100180305     d InfDspF         ds
010200180306     d   $dsp_aid            369    369a
010300180306
010400180306       // - ?Indicatori su DspF?
010500180306     d IndDspF         ds                  inz
010600180306         // - ?Emissione messaggio di errore?
010700180306     d   $ErrMessage                   n   overlay( IndDspF : 28 )
010800180306         // - ?Posizionamento cursore & segnalazione errore?
010900180306     d   $PosCurDAL                    n   overlay( IndDspF : 47 )
011000180306     d   $PosCurAL                     n   overlay( IndDspF : 48 )
011100180306     d   $PosCurCWF                    n   overlay( IndDspF : 49 )
011200180306     d   $PosCurRIC                    n   overlay( IndDspF : 50 )
011300180306     d   $PosCurRS1                    n   overlay( IndDspF : 51 )
011400180306     d   $PosCurRS2                    n   overlay( IndDspF : 52 )
011500180306     d   $PosCurRS3                    n   overlay( IndDspF : 53 )
011600180306     d   $PosCurRS4                    n   overlay( IndDspF : 54 )
011700180306     d   $PosCurRS5                    n   overlay( IndDspF : 55 )
011800180306     d   $PosCurIN1                    n   overlay( IndDspF : 56 )
011900180306     d   $PosCurIN2                    n   overlay( IndDspF : 57 )
012000180306     d   $PosCurIN3                    n   overlay( IndDspF : 58 )
012100180306     d   $PosCurIN4                    n   overlay( IndDspF : 59 )
012200180306     d   $PosCurIN5                    n   overlay( IndDspF : 60 )
012300180306     d   $PosCurLO1                    n   overlay( IndDspF : 61 )
012400180306     d   $PosCurLO2                    n   overlay( IndDspF : 61 )
012500180306     d   $PosCurLO3                    n   overlay( IndDspF : 63 )
012600180306     d   $PosCurLO4                    n   overlay( IndDspF : 64 )
012700180306     d   $PosCurLO5                    n   overlay( IndDspF : 65 )
012800180306     d   $PosCurPR1                    n   overlay( IndDspF : 66 )
012900180306     d   $PosCurPR2                    n   overlay( IndDspF : 67 )
013000180306     d   $PosCurPR3                    n   overlay( IndDspF : 68 )
013100180306     d   $PosCurPR4                    n   overlay( IndDspF : 69 )
013200180306     d   $PosCurPR5                    n   overlay( IndDspF : 70 )
013300180306     d   $PosCurCC1                    n   overlay( IndDspF : 71 )
013400180306     d   $PosCurCC2                    n   overlay( IndDspF : 72 )
013500180306     d   $PosCurCC3                    n   overlay( IndDspF : 73 )
013600180306     d   $PosCurCC4                    n   overlay( IndDspF : 74 )
013700180306     d   $PosCurCC5                    n   overlay( IndDspF : 75 )
013800180306     d   $PosCurLP1                    n   overlay( IndDspF : 76 )
013900180306     d   $PosCurLP2                    n   overlay( IndDspF : 77 )
014000180306     d   $PosCurLP3                    n   overlay( IndDspF : 78 )
014100180306     d   $PosCurLP4                    n   overlay( IndDspF : 79 )
014200180306     d   $PosCurLP5                    n   overlay( IndDspF : 80 )
014300180306     d   $PosCurAS1                    n   overlay( IndDspF : 81 )
014400180306     d   $PosCurAS2                    n   overlay( IndDspF : 82 )
014500180306     d   $PosCurAS3                    n   overlay( IndDspF : 83 )
014600180306     d   $PosCurAS4                    n   overlay( IndDspF : 84 )
014700180306     d   $PosCurAS5                    n   overlay( IndDspF : 85 )
014800180306     d   $PosCurPD1                    n   overlay( IndDspF : 86 )
014900180306     d   $PosCurPD2                    n   overlay( IndDspF : 87 )
015000180306     d   $PosCurPD3                    n   overlay( IndDspF : 88 )
015100180306     d   $PosCurPD4                    n   overlay( IndDspF : 89 )
015200180306     d   $PosCurPD5                    n   overlay( IndDspF : 90 )
015300180306         // - ?Riemissione videata?
015400180306     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
015500180306
015600180306       // - ?"Intabellamento" dati a video?
015700180306     d c_MaxElem       c                   const(5)
015800180306
015900070629     d ds_RS           ds                  inz
016000070629     d   V1Crs1                            inz
016100070629     d   V1Crs2                            inz
016200070629     d   V1Crs3                            inz
016300070629     d   V1Crs4                            inz
016400070629     d   V1Crs5                            inz
016500180306     d   sk_V1Crs              1    150    inz     dim(c_MaxElem)
016600100623     d ds_IND          ds                  inz
016700100623     d   V1Cin1                            inz
016800100623     d   V1Cin2                            inz
016900100623     d   V1Cin3                            inz
017000100623     d   V1Cin4                            inz
017100100623     d   V1Cin5                            inz
017200180306     d   sk_V1Cin              1    150    inz     dim(c_MaxElem)
017300180306     d ds_LOC          ds                  inz
017400100623     d   V1Clo1                            inz
017500100623     d   V1Clo2                            inz
017600100623     d   V1Clo3                            inz
017700100623     d   V1Clo4                            inz
017800100623     d   V1Clo5                            inz
017900180306     d   sk_V1Clo              1    150    inz     dim(c_MaxElem)
018000180305     d ds_PRV          ds                  inz
018100180305     d   V1Cpr1                            inz
018200180305     d   V1Cpr2                            inz
018300180305     d   V1Cpr3                            inz
018400180305     d   V1Cpr4                            inz
018500180305     d   V1Cpr5                            inz
018600180306     d   sk_V1Cpr              1     10    inz     dim(c_MaxElem)
018700180306
018800051024     d ds_KSC          ds                  inz
018900051024     d   V1Ccc1                            inz
019000051024     d   V1Ccc2                            inz
019100051024     d   V1Ccc3                            inz
019200051024     d   V1Ccc4                            inz
019300070629     d   V1Ccc5                            inz
019400180306     d   sk_CC                 1     35  0 inz     dim(c_MaxElem)
019500051024     d ds_KSCD         ds                  inz
019600051024     d   V1Dcc1                            inz
019700051024     d   V1Dcc2                            inz
019800051024     d   V1Dcc3                            inz
019900051024     d   V1Dcc4                            inz
020000070629     d   V1Dcc5                            inz
020100180306     d   sk_CCD                1    115    inz     dim(c_MaxElem)
020200180306
020300180305     d ds_SPE          ds                  inz
020400180308     d   V1aas1                            inz
020500180305     d   V1lnp1                            inz
020600180305     d   V1nrs1                            inz
020700180305     d   V1nsp1                            inz
020800180308     d   V1aas2                            inz
020900180305     d   V1lnp2                            inz
021000180305     d   V1nrs2                            inz
021100180305     d   V1nsp2                            inz
021200180308     d   V1aas3                            inz
021300180305     d   V1lnp3                            inz
021400180305     d   V1nrs3                            inz
021500180305     d   V1nsp3                            inz
021600180308     d   V1aas4                            inz
021700180305     d   V1lnp4                            inz
021800180305     d   V1nrs4                            inz
021900180305     d   V1nsp4                            inz
022000180308     d   V1aas5                            inz
022100180305     d   V1lnp5                            inz
022200180305     d   V1nrs5                            inz
022300180305     d   V1nsp5                            inz
022400180306     d   sk_SPE                1     80  0 inz     dim(c_MaxElem)
022500180306
022600180305     d ds_PDR          ds                  inz
022700180305     d   V1Cpd1                            inz
022800180305     d   V1Cpd2                            inz
022900180305     d   V1Cpd3                            inz
023000180305     d   V1Cpd4                            inz
023100180305     d   V1Cpd5                            inz
023200180306     d   sk_PDR                1     35  0 inz     dim(c_MaxElem)
023300180305     d ds_PDRD         ds                  inz
023400180305     d   V1Dpd1                            inz
023500180305     d   V1Dpd2                            inz
023600180305     d   V1Dpd3                            inz
023700180305     d   V1Dpd4                            inz
023800180305     d   V1Dpd5                            inz
023900180306     d   sk_PDD                1    115    inz     dim(c_MaxElem)
024000180306
024100180306       // - ?Comodo per identificare 1 (UNA) spedizione?
024200180306     d ds_SPED         ds                  inz     qualified
024300180308     d   AaS                               inz     like( V1aas1 )
024400180306     d   LnP                               inz     like( V1lnp1 )
024500180306     d   NrS                               inz     like( V1nrs1 )
024600180306     d   NSp                               inz     like( V1nsp1 )
024700180306
024800180306       //--------------------------------------------------------------
024900180306       //? Definizione variabili globali.                               ?
025000180306       //--------------------------------------------------------------
025100180306
025200180306       // - ?Flags booleani?
025300180306     d $Fine           s               n   inz
025400100128     d $ErrSisInf      s               n   inz
025500180306     d $InzD01         s               n   inz(*on)
025600180306
025700180306       // - ?Indici di schiera?
025800180305     d XX              s              3  0 inz
025900180306
026000180306       // - ?Variabili riferite al data base o al display file?
026100180306     d W1Cdal          s                   inz     like( TNSB19ds.SB19dal )
026200180306     d W1Cal           s                   inz     like( TNSB19ds.SB19al  )
026300180306
026400180306       // - ?Variabili per la gestione del video?
026500180305     d $Video          s              2    inz('D1')
026600180306
026700180306       // - ?Variabili di comodo?
026800180307     d wDate_Iso       s               d   inz   datfmt(*ISO)
026900180307     d wDate_Eur       s               d   inz   datfmt(*EUR)
027000180306     d W§$3d10         s              8  0 inz
027100180306
027200180306       //--------------------------------------------------------------
027300180306       //? Definizione prototipi procedure.                             ?
027400180306       //--------------------------------------------------------------
027500180306
027600180306       // - ?Reperimento dati utente?
027700180306     d TIBS34ds      e ds                  inz
027800180306      /copy gaitrasrc/srcProtoPR,TIBS34R
027900180306
028000180306       // - ?Controllo/Decodifica cliente?
028100180306      /copy gaitrasrc/srcProtoPI,TIBS69R
028200180306      /copy gaitrasrc/srcProtoPR,TIBS69R
028300180306
028400180306       // - ?Reperimento dati tabelle?
028500180306      /copy gaitrasrc/srcProtoPI,TRULTAB
028600180306      /copy gaitrasrc/srcProtoPR,TRULTAB
028700180306
028800180306       // - ?Gestione dati del WrkF WFPRM00F?
028900180306       //   ?(Parametri per applicazioni batch)?
029000180306     d TRUL01ds      e ds                  inz   qualified
029100180306     d   UL01opz     e                     inz(*on)
029200180306     d   UL01err     e                     inz(*off)
029300180306     d trul01r         pr                  extpgm('TRUL01R')
029400180306     d   trul01ds                          likeds(TRUL01ds)
029500180306
029600180306       // - ?Controllo ed inversione date?
029700180306     d WLBdat          ds                  inz   qualified
029800180306     d   G08dat                       8  0 inz
029900180306     d   G08inv                       8  0 inz
030000180306     d   G08err                       1    inz('3')
030100180306     d   G08tgi                       5  0 inz
030200180306      /copy gaitrasrc/srcProtoPr,XSRDA8
030300180306
030400180306       // - ?Richiamo diretto lavoro batch?
030500180306     d TNSB19ds      e ds                  inz   qualified
030600180306     d tnsb19r         pr                  extpgm('TNSB19R')
030700180306     d   kpjba                             likeds(KPJBA)
030800180306
030900180306       // - ?Personalizzazione lavoro batch?
031000180306     d*// Bch09           pr                  extpgm('BCH09')
031100180306     d*//   kpjba                             likeds(KPJBA)
031200180306
031300180306       // - ?Sottomissione lavoro batch?
031400180306      /copy gaitrasrc/srcProtoPR,BCH10
031500180306
031600180306       // - ?Parametri API QCAPCMD (Process Commands)?
031700180306     d Qcmd            s           2048    inz  varying
031800180306      /copy qSysInc/qRpgleSrc,QCAPCMD
031900180306       // - ?API QCAPCMD (Process Commands)?
032000180306      /copy gaitrasrc/srcProtoPR,QCAPCMD
032100180306
032200180306       // - ?Parametri gestione errori API.?
032300180306      /copy qsysinc/qrpglesrc,QUSEC
032400180306
032500180306       //--------------------------------------------------------------
032600180306       //? Definizione key-list.                                        ?
032700180306       //--------------------------------------------------------------
032800180308
032900180308       // -?File TITAS30C?
033000180308     d keyTITAS30    e ds                  extname( TITAS30C : *key )
033100180308     d                                     prefix( k_ )   inz
033200180306
033300180306       // -?File FIAPD01L?
033400180306     d keyFIAPD01    e ds                  extname( FIAPD01L : *key )
033500180306     d                                     prefix( k_ )   inz
033600180306
033700180306       //--------------------------------------------------------------
033800180306       //? Indicatori utilizzati.                                       ?
033900180306       //--------------------------------------------------------------
034000180306
034100180306       //  28 - Emissione Massaggio di Errore a video
034200180306       //  47 - Data Spedizioni Iniziale Errata
034300180306       //  48 - Data Spedizioni Finale   Errata
034400180306       //  49 - Richiesta di Pulizia Anticipata Archivio Errata
034500180306       //  50 - Cliente di Ricerca per dati anagrafici del cliente Errato
034600180306       //  51 - 1ª Ragione Sociale errata
034700180306       //  52 - 2ª Ragione Sociale errata
034800180306       //  53 - 3ª Ragione Sociale errata
034900180306       //  54 - 4ª Ragione Sociale errata
035000180306       //  55 - 5ª Ragione Sociale errata
035100180306       //  56 - 1° Indirizzo errato
035200180306       //  57 - 2° Indirizzo errato
035300180306       //  58 - 3° Indirizzo errato
035400180306       //  59 - 4° Indirizzo errato
035500180306       //  60 - 5° Indirizzo errato
035600180306       //  61 - 1ª Località errata
035700180306       //  62 - 2ª Località errata
035800180306       //  63 - 3ª Località errata
035900180306       //  64 - 4ª Località errata
036000180306       //  65 - 5ª Località errata
036100180306       //  66 - 1ª Provincia errata
036200180306       //  67 - 2ª Provincia errata
036300180306       //  68 - 3ª Provincia errata
036400180306       //  69 - 4ª Provincia errata
036500180306       //  70 - 5ª Provincia errata
036600180306       //  71 - 1° Codice Cliente errato
036700180306       //  72 - 2° Codice Cliente errato
036800180306       //  73 - 3° Codice Cliente errato
036900180306       //  74 - 4° Codice Cliente errato
037000180306       //  75 - 5° Codice Cliente errato
037100180306       //  76 - 1ª Spedizione errata
037200180306       //  77 - 2ª Spedizione errata
037300180306       //  78 - 3ª Spedizione errata
037400180306       //  79 - 4ª Spedizione errata
037500180306       //  80 - 5ª Spedizione errata
037600180306       //  81 - Anno 1ª spedizione errato
037700180306       //  82 - Anno 2ª spedizione errato
037800180306       //  83 - Anno 3ª spedizione errato
037900180306       //  84 - Anno 4ª spedizione errato
038000180306       //  85 - Anno 5ª spedizione errato
038100180306       //  86 - 1° Codice Padroncino errato
038200180306       //  87 - 2° Codice Padroncino errato
038300180306       //  88 - 3° Codice Padroncino errato
038400180306       //  89 - 4° Codice Padroncino errato
038500180306       //  90 - 5° Codice Padroncino errato
038600180306       //  99 - Errore Generico
038700180306
038800180306       //--------------------------------------------------------------
038900180306       //? M A I N - L I N E                                            ?
039000180306       //--------------------------------------------------------------
039100180306
039200050809     c     *Entry        plist
039300050809     c                   parm                    KPJBA
039400180306
039500180306      /free
039600180306
039700180306       // - ?Operazioni iniziali?
039800180306       exsr  sr_RoutInz;
039900180306
040000180306
040100180306       // - ?Ciclo di gestione del file video?
040200180306       DoW  $Fine = *off;
040300180306
040400180306         select;
040500180306
040600180306           // - ?Richiesta parametri iniziali?
040700180306           when $Video = 'D1';
040800180306             exsr  sr_GesD01;
040900180306
041000180306           // - ?? ? ??
041100180306           other;
041200180306             $Fine = *on;
041300180306
041400180306         endsl;
041500180306
041600180306       EndDo;
041700180306
041800180306       // - ?Operazioni finali?
041900180306       exsr sr_RoutEnd;
042000180306
042100180306       //--------------------------------------------------------------
042200180306       //? Operazioni iniziali.                                         ?
042300180306       //--------------------------------------------------------------
042400180306       BEGSR  sr_RoutInz;
042500180306
042600180306         // -?Impostazione chiusura?
042700180306         *inLR = *on;
042800180306
042900180306         // -?Reperimento dati job?
043000180306         exsr  sr_DatiJob;
043100180306
043200180306         // -?Impostazione nome programma a video?
043300180306         V1Tpgm = SDSpgmName;
043400180306
043500180306         // - ?Impostazione messaggio di avvertimento?
043600180306         //   ?con la data dell'ultima pulizia del *file TITAS10F?
043700180306         clear  ds$3;
043800180306         ds_TNTBE.TBEke1 = '1';
043900180306         if  getTabella ( c_Tabel : '$3'  : ds_TNTBE.TBEke1 :
044000180306                          *omit : *omit : *omit :
044100180306                          *omit : *omit :
044200180306                          *omit : *omit : *omit : *omit :
044300180306                          *omit : *omit : *omit : *omit :
044400180306                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
044500180306                        = *zero      AND
044600180306             ds_TNTBE.TBEatb = *blank;
044700180306           ds$3   = ds_TNTBE.TBEuni;
044800180306         endif;
044900180306
045000180306         if  ds$3.§$3d10 > *zero;
045100180306           reset  WLBdat;
045200180306           WLBdat.G08inv = ds$3.§$3d10;
045300180306           XSRDA8 ( WLBdat );
045400180306           if  WLBdat.G08err  = *off;
045500180306             wDate_Iso = %date( WLBdat.G08dat : *eur ) + %days(1);
045600180307             wDate_Eur = wDate_Iso;
045700180306             W§$3d10   = %dec( wDate_Iso );
045800180306           endif;
045900180306         endif;
046000180306
046100180306       ENDSR;
046200180306
046300180306       //--------------------------------------------------------------
046400180306       //?Reperimento Dati del job (Utente/Operativi).                 ?
046500180306       //--------------------------------------------------------------
046600180306       BEGSR  sr_DatiJob;
046700180306
046800180306         in(e) §AzUte;
046900180306         if NOT %error;
047000180306           in(e) §DatiUte;
047100180306         endif;
047200180306         if %error or RSut = *blank;
047300180306           tibs34r ( tibs34ds );
047400180306           in §AzUte;
047500180306           in §DatiUte;
047600180306         endif;
047700180306
047800180306       ENDSR;
047900180306
048000180306       //--------------------------------------------------------------
048100180306       //? Gestione videata D01.                                        ?
048200180306       //--------------------------------------------------------------
048300180306       BEGSR  sr_GesD01;
048400180306
048500180306         // - ?Inizializzazione videata?
048600180306         if  $InzD01 = *on;
048700180306           exsr  sr_InzD01;
048800180306           $InzD01 = *off;
048900180306         endif;
049000180306
049100180306         // - ?Emissione videata?
049200180306         write  SB18T01;
049300180306         write  SB18Z01;
049400180306
049500180306         exfmt  SB18D01;
049600180306
049700180306         clear  VIDmsg;
049800180306         reset  $ErrMessage;
049900180306         reset  $ErrGenerico;
050000180306
050100180306         Select;
050200180306
050300180306           // - ?Errore: sistema informativo errato?
050400180306           When  $ErrSisInf;
050500180306             $Fine  = *on;
050600180306
050700180306           // - ?F3=Fine?
050800180306           When  $dsp_aid = c_F03;
050900180306             $Fine  = *on;
051000180306
051100180306           // - ?F9=Visualizzazione WrkF WFNIB00F?
051200180306           WHEN  $dsp_aid = c_F09;
051300180306             Qcmd = 'DSPDBF file(*libl/WFNIB00F) mbr(*first) output(*)';
051400180306             exsr  sr_ExecCmd;
051500180306
051600180306           // -?Invio / F6=Conferma?
051700180306           Other;
051800180306             exsr  sr_CtrD01;
051900180306             if  $ErrGenerico = *on;
052000180306               leavesr;
052100180306             endif;
052200180306             if  $dsp_aid = c_F06;
052300180306               exsr  sr_SbmJob;
052400180306               $Fine = *on;
052500180306             endif;
052600180306
052700180306         ENDSL;
052800180306
052900180306       ENDSR;
053000180306
053100180306       //--------------------------------------------------------------
053200180306       //? Inizializzazione videata D01.                                ?
053300180306       //--------------------------------------------------------------
053400180306       BEGSR  sr_InzD01;
053500180306
053600180306         clear  SB18D01;
053700180306
053800180306         V1Cric  =  'E';
053900180306
054000180306         // - ?Controllo del sistema informativo?
054100180306         if  %subst( KNSIF : 1 : 3 ) <> 'GAI';
054200180306           $ErrSisInf   = *on;
054300180306           $ErrMessage  = *on;
054400180306           $ErrGenerico = *on;
054500180306           VIDmsg = sk_Msg(12);
054600180306           leavesr;
054700180306         endif;
054800180307
054900180307         // - ?Testo con la data limite per la selezione delle spedizioni?
055000180307         if  W§$3d10 > *zero;
055100180307           V1Dtxt = '(Spedizioni disponibili dal ' +
055200180307                    %editc( %dec( wDate_Eur ) : 'Y' ) + ')';
055300180307         endif;
055400180309
055500180309         // - ?Apertura archivi di sola Sede?
055600180309         if  NOT %open( TITAS30C );
055700180309           open  TITAS30C;
055800180309         endif;
055900180309         if  NOT %open( FIAPD01L );
056000180309           open  FIAPD01L;
056100180309         endif;
056200180306
056300180306       ENDSR;
056400180306
056500180306       //--------------------------------------------------------------
056600180306       //? Controllo dati nella videata D01.                            ?
056700180306       //--------------------------------------------------------------
056800180306       BEGSR  sr_CtrD01;
056900180306
057000180306         // - ?Spegnimento indicatori di posizionamento cursore?
057100180306         %subst( IndDspF : 47 ) = *off;
057200180306
057300180306         // - ?RAGIONE SOCIALE?
057400180306         //    ·?Obbligatoria almeno una selezione?
057500180306         //     ?SE non inserite altre parzializzazioni?
057600180306         If  ds_RS  =  *blank  and
057700180306             ds_IND =  *blank  and
057800180306             ds_LOC =  *blank  and
057900180306             ds_PRV =  *blank  and
058000180306             ds_KSC =  *zero   and
058100180306             ds_SPE =  *zero   and
058200180306             ds_PDR =  *zero;
058300180306           $PosCurRS1   = *on;
058400180306           $ErrMessage  = *on;
058500180306           $ErrGenerico = *on;
058600180306           VIDmsg = sk_Msg(01);
058700180306           leavesr;
058800180306         EndIf;
058900180306
059000180306         //    ·?Controllo dei caratteri inseriti?
059100180306         clear  xx;
059200180306         For  xx = 1  To  %elem( sk_V1Crs );
059300180306
059400180306           //    ·?"apostrofi" non ammessi né in ragione sociale?
059500180306           if  sk_V1Crs(xx) <> *blank  and
059600180306               %scan( '''' : %trimr( sk_V1Crs(xx) ) ) > *zero;
059700180306             %subst( IndDspF : 50+xx : 1 ) = *on;
059800180306             $ErrMessage  = *on;
059900180306             $ErrGenerico = *on;
060000180306             VIDmsg = sk_Msg(09);
060100180306             leavesr;
060200180306           endif;
060300180306           //     ?                        né in indirizzo?
060400180306           if  sk_V1Cin(xx) <> *blank  and
060500180306               %scan( '''' : %trimr( sk_V1Cin(xx) ) ) > *zero;
060600180306             %subst( IndDspF : 55+xx : 1 ) = *on;
060700180306             $ErrMessage  = *on;
060800180306             $ErrGenerico = *on;
060900180306             VIDmsg = sk_Msg(09);
061000180306             leavesr;
061100180306           endif;
061200180306           //     ?                        né in località?
061300180306           if  sk_V1Clo(xx) <> *blank  and
061400180306               %scan( '''' : %trimr( sk_V1Clo(xx) ) ) > *zero;
061500180306             %subst( IndDspF : 60+xx : 1 ) = *on;
061600180306             $ErrMessage  = *on;
061700180306             $ErrGenerico = *on;
061800180306             VIDmsg = sk_Msg(09);
061900180306             leavesr;
062000180306           endif;
062100180306           //    ·?"percentuali" non ammesse né in ragione sociale?
062200180306           if  sk_V1Crs(xx) <> *blank  and
062300180306               %scan( '%' : %trimr( sk_V1Crs(xx) ) ) > *zero;
062400180306             %subst( IndDspF : 50+xx : 1 ) = *on;
062500180306             $ErrMessage  = *on;
062600180306             $ErrGenerico = *on;
062700180308             VIDmsg = sk_Msg(10);
062800180306             leavesr;
062900180306           endif;
063000180306           //     ?                          né in indirizzo?
063100180306           if  sk_V1Cin(xx) <> *blank  and
063200180306               %scan( '%' : %trimr( sk_V1Cin(xx) ) ) > *zero;
063300180306             %subst( IndDspF : 55+xx : 1 ) = *on;
063400180306             $ErrMessage  = *on;
063500180306             $ErrGenerico = *on;
063600180308             VIDmsg = sk_Msg(10);
063700180306             leavesr;
063800180306           endif;
063900180306           //     ?                          né in località?
064000180306           if  sk_V1Clo(xx) <> *blank  and
064100180306               %scan( '%' : %trimr( sk_V1Clo(xx) ) ) > *zero;
064200180306             %subst( IndDspF : 60+xx : 1 ) = *on;
064300180306             $ErrMessage  = *on;
064400180306             $ErrGenerico = *on;
064500180308             VIDmsg = sk_Msg(10);
064600180306             leavesr;
064700180306           endif;
064800180306
064900180306         EndFor;
065000180306
065100180306         // - ?PROVINCIA?
065200180306         //    ·?Obbligatoria almeno una selezione?
065300180306         //     ?SE inserito almeno un indirizzo?
065400180306         if  ds_RS  =  *blank  and
065500180306             ds_IND <> *blank  and
065600180306             ds_LOC =  *blank  and
065700180306             ds_PRV =  *blank  and
065800180306             ds_KSC =  *zero   and
065900180306             ds_SPE =  *zero   and
066000180306             ds_PDR =  *zero;
066100180306           $PosCurPR1   = *on;
066200180306           $ErrMessage  = *on;
066300180306           $ErrGenerico = *on;
066400180306           VIDmsg = sk_Msg(08);
066500180306           leavesr;
066600180306         endif;
066700180306
066800180306         // - ?CODICE CLIENTE?
066900180306         //    ·?inibisce l'immissione di altri dati?
067000180306         IF  ds_KSC   <> *zero  AND
067100180306             ( ds_RS  <> *blank  or
067200180306               ds_IND <> *blank  or
067300180306               ds_LOC <> *blank  or
067400180308               ds_PRV <> *blank);
067500180306
067600180308           For  xx = 1  To  %elem( sk_CC );
067700180308             if  sk_CC(xx) <> *zero;
067800180308               leave;
067900180308             endif;
068000180308           EndFor;
068100180308
068200180308           %subst( IndDspF : 70+xx : 1 ) = *on;
068300180306           $ErrMessage  = *on;
068400180306           $ErrGenerico = *on;
068500180306           VIDmsg = %trimR( sk_Msg(06) ) +
068600180306                    ' RAGIONE SOCIALE/INDIRIZZO/...';
068700180306           leavesr;
068800180306
068900180306         ENDIF;
069000180306
069100180306         // - ?TIPO RICERCA?
069200180306         //    ·?deve essere M=Mittente, D=Destinatario, E=Entrambi?
069300180306         //     ?SE inserite ragioni sociali?
069400180306         If  V1Cric <> 'M'  and
069500180306             V1Cric <> 'D'  and
069600180306             V1Cric <> 'E'  and
069700180306             ds_RS  <> *blank;
069800180306           $PosCurRIC   = *on;
069900180306           $ErrMessage  = *on;
070000180306           $ErrGenerico = *on;
070100180306           VIDmsg = sk_Msg(02);
070200180306           leavesr;
070300180306         EndIf;
070400180306
070500180306         // - ?CODICE CLIENTE?
070600180306         //    ·?controllo e decodifica?
070700180306         clear  sk_CCD;
070800180306         For  xx = 1  To  %elem( sk_CC );
070900180306
071000180306           if  sk_CC(xx) = *zero;
071100180306             iter;
071200180306           endif;
071300180306
071400180306           clear  TIBS69ds;
071500180306           I69sif = knsif;
071600180306           I69kcc = DUTkci;
071700180306           I69kac = sk_CC(xx);
071800180306           I69kin = I69kac;
071900180306           I69kcs = I69kac;
072000180306           tibs69r( TIBS69ds :
072100180306                    ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
072200180306
072300180306           if  O69err = *blank;
072400180306             sk_CCd(xx) = ACOrag;
072500180306           else;
072600180306             sk_CCd(xx) = *all'? ';
072700180308             %subst( IndDspF : 70+xx : 1 ) = *on;
072800180306             $ErrMessage  = *on;
072900180306             $ErrGenerico = *on;
073000180306             VIDmsg = sk_Msg(07);
073100180306             leavesr;
073200180306           endif;
073300180306
073400180306         EndFor;
073500180306
073600180306         // - ?SPEDIZIONE?
073700180306         //    ·?inibisce l'immissione di altri dati?
073800180306         IF  ds_SPE <> *zero;
073900180306
074000180308           For  xx = 1  To  %elem( sk_SPE );
074100180308             if  sk_SPE(xx) <> *zero;
074200180308               leave;
074300180308             endif;
074400180308           EndFor;
074500180306
074600180308           Select;
074700180308             When  ds_RS  <> *blank  or
074800180308                   ds_IND <> *blank  or
074900180308                   ds_LOC <> *blank  or
075000180308                   ds_PRV <> *blank;
075100180308               %subst( IndDspF : 75+xx : 1 ) = *on;
075200180308               $ErrMessage  = *on;
075300180308               $ErrGenerico = *on;
075400180306               VIDmsg = %trimR( sk_Msg(06) ) +
075500180306                        ' RAGIONE SOCIALE/INDIRIZZO/...';
075600180306               leavesr;
075700180306             When  ds_KSC <> *zero;
075800180308               %subst( IndDspF : 75+xx : 1 ) = *on;
075900180308               $ErrMessage  = *on;
076000180308               $ErrGenerico = *on;
076100180306               VIDmsg = %trimR( sk_Msg(06) ) +
076200180306                        ' CODICE CLIENTE';
076300180306               leavesr;
076400180306           EndSl;
076500180306
076600180306         ENDIF;
076700180306
076800180306         //    ·?obbligatoria almeno una spedizione?
076900180306         //     ?SE inserita almeno una LNP o un NSP o un AAS?
077000180306         IF  ds_SPE <> *zero;
077100180306
077200180306           For  xx = 1  To  %elem( sk_SPE );
077300180306
077400180306             ds_Sped = %editc( sk_SPE(xx) : 'X' );
077500180306             if  ds_Sped <> *zero;
077600180306
077700180306               select;
077800180306                 when  ds_Sped.LNP <= *zero;
077900180306                   %subst( IndDspF : 75+xx : 1 ) = *on;
078000180306                   $ErrMessage  = *on;
078100180306                   $ErrGenerico = *on;
078200180306                   VIDmsg = sk_Msg(13);
078300180306                   leavesr;
078400180306                 when  ds_Sped.NSP <= *zero;
078500180306                   %subst( IndDspF : 75+xx : 1 ) = *on;
078600180306                   $ErrMessage  = *on;
078700180306                   $ErrGenerico = *on;
078800180306                   VIDmsg = sk_Msg(14);
078900180306                   leavesr;
079000180306                 when  ds_Sped.AAS <= *zero;
079100180306                   %subst( IndDspF : 80+xx : 1 ) = *on;
079200180306                   $ErrMessage  = *on;
079300180306                   $ErrGenerico = *on;
079400180306                   VIDmsg = sk_Msg(15);
079500180306                   leavesr;
079600180308                 other;
079700180308                   select;
079800180308                     when  ds_Sped.AAS < 100  and  ds_Sped.AAS >  60;
079900180308                       ds_Sped.AAS += 1900;
080000180308                       sk_SPE(xx) = %int( ds_Sped );
080100180308                     when  ds_Sped.AAS < 100  and  ds_Sped.AAS <= 60;
080200180308                       ds_Sped.AAS += 2000;
080300180308                       sk_SPE(xx) = %int( ds_Sped );
080400180308                   endsl;
080500180308                   clear  keyTITAS30;
080600180308                   k_TASaas = ds_Sped.AAS;
080700180308                   k_TASlnp = ds_Sped.LNP;
080800180308                   k_TASnrs = ds_Sped.NRS;
080900180308                   k_TASnsp = ds_Sped.NSP;
081000180308                   chain  %kds( keyTITAS30 : 4 )  TITAS30C;
081100180308                   if  NOT %found( TITAS30C );
081200180308                     %subst( IndDspF : 75+xx : 1 ) = *on;
081300180308                     $ErrMessage  = *on;
081400180308                     $ErrGenerico = *on;
081500180308                     VIDmsg = sk_Msg(16);
081600180308                     leavesr;
081700180308                   endif;
081800180306               endsl;
081900180306
082000180306             endif;
082100180306
082200180306           EndFor;
082300180306
082400180306         ENDIF;
082500180306
082600180306         // - ?PADRONCINO?
082700180306         //    ·?inibisce l'immissione di altri dati?
082800180306         IF  ds_PDR <> *zero;
082900180306
083000180308           For  xx = 1  To  %elem( sk_PDR );
083100180308             if  sk_PDR(xx) <> *zero;
083200180308               leave;
083300180308             endif;
083400180308           EndFor;
083500180308
083600180308           Select;
083700180308             When  ds_RS  <> *blank  or
083800180308                   ds_IND <> *blank  or
083900180308                   ds_LOC <> *blank  or
084000180308                   ds_PRV <> *blank;
084100180308               %subst( IndDspF : 85+xx : 1 ) = *on;
084200180306               $ErrMessage  = *on;
084300180306               $ErrGenerico = *on;
084400180306               VIDmsg = %trimR( sk_Msg(06) ) +
084500180306                        ' RAGIONE SOCIALE/INDIRIZZO/...';
084600180306               leavesr;
084700180306             When  ds_KSC <> *zero;
084800180308               %subst( IndDspF : 85+xx : 1 ) = *on;
084900180306               $ErrMessage  = *on;
085000180306               $ErrGenerico = *on;
085100180306               VIDmsg = %trimR( sk_Msg(06) ) +
085200180306                        ' CODICE CLIENTE';
085300180306               leavesr;
085400180306             When  ds_SPE <> *zero;
085500180308               %subst( IndDspF : 85+xx : 1 ) = *on;
085600180306               $ErrMessage  = *on;
085700180306               $ErrGenerico = *on;
085800180306               VIDmsg = %trimR( sk_Msg(06) ) +
085900180306                        ' SPEDIZIONE';
086000180306               leavesr;
086100180306           EndSl;
086200180306
086300180306         ENDIF;
086400180306
086500180306         //    ·?controllo e decodifica?
086600180306         clear  sk_PDD;
086700180306         If  ds_PDR <> *zero;
086800180306           For  xx = 1  To  %elem( sk_PDR );
086900180306             if  sk_PDR(xx) = *zero;
087000180306               iter;
087100180306             endif;
087200180306             clear  keyFIAPD01;
087300180306             k_APDtip = 'A';
087400180306             k_APDpdr = sk_PDR(xx);
087500180306             chain  %kds( keyFIAPD01 )  FIAPD000;
087600180306             if  %found( FIAPD01L )  and
087700180306                 APDatb = *blank;
087800180306               sk_PDD(xx) = APDrsc;
087900180306             else;
088000180306               sk_PDD(xx) = *all'? ';
088100180306               %subst( IndDspF : 85+xx : 1 ) = *on;
088200180306               $ErrMessage  = *on;
088300180306               $ErrGenerico = *on;
088400180308               VIDmsg = sk_Msg(17);
088500180306               leavesr;
088600180306             endif;
088700180306           EndFor;
088800180306         EndIf;
088900180306
089000180306         // - ?DATA LIMITE INIZIALE ("DAL") PER RICERCA?
089100180306         reset  W1Cdal;
089200180306         clear  WLBdat;
089300180306         if  V1Cdal <> *zero;
089400180306           WLBdat.G08dat  =  V1Cdal;
089500180306           xsrda8 ( WLBdat );
089600180306         endif;
089700180309         if  WLBdat.G08err = *on    or
089800180309             ( V1Cdal      = *zero  and
089900180309               ds_SPE      = *zero );
090000180306           $PosCurDAL   = *on;
090100180306           $ErrMessage  = *on;
090200180306           $ErrGenerico = *on;
090300180306           VIDmsg = sk_Msg(03);
090400180306           leavesr;
090500180306         endif;
090600180306
090700180306         V1Cdal = WLBdat.G08dat;
090800180306         W1Cdal = WLBdat.G08inv;
090900180306
091000180309         If  W1Cdal <> *zero  and
091100180309             W1Cdal <= W§$3d10;
091200180306           $PosCurDAL   = *on;
091300180306           $ErrMessage  = *on;
091400180306           $ErrGenerico = *on;
091500180308           VIDmsg = %trimR( sk_Msg(18) ) +
091600180307                    %editc( %dec( wDate_Eur ) : 'Y' );
091700180306           leavesr;
091800180306         endif;
091900180306
092000180306         // - ?DATA LIMITE INIZIALE ("AL") PER RICERCA?
092100180306         reset  W1Cal;
092200180306         clear  WLBdat;
092300180306         if  V1Cal <> *zero;
092400180306           WLBdat.G08dat  =  V1Cal;
092500180306           xsrda8 ( WLBdat );
092600180306         endif;
092700180309         if  WLBdat.G08err = *on    or
092800180309             ( V1Cal       = *zero  and
092900180309               ds_SPE      = *zero );
093000180306           $PosCurAL    = *on;
093100180306           $ErrMessage  = *on;
093200180306           $ErrGenerico = *on;
093300180306           VIDmsg = sk_Msg(03);
093400180306           leavesr;
093500180306         endif;
093600180306
093700180306         V1Cal  = WLBdat.G08dat;
093800180306         W1Cal  = WLBdat.G08inv;
093900180306
094000180306         // - ?RANGE DI DATE?
094100180306         if  W1Cdal > W1Cal;
094200180306           $PosCurDAL   = *on;
094300180306           $ErrMessage  = *on;
094400180306           $ErrGenerico = *on;
094500180306           VIDmsg = sk_Msg(04);
094600180306           leavesr;
094700180306         endif;
094800180306
094900180306         // - ?PULIZIA ANTICIPATA DEL WORKFILE: "S" o "N"?
095000180306         if  V1Cclrwf <> 'S'  and
095100180306             V1Cclrwf <> 'N';
095200180306           $PosCurCWF   = *on;
095300180306           $ErrMessage  = *on;
095400180306           $ErrGenerico = *on;
095500180306           VIDmsg = sk_Msg(05);
095600180306           leavesr;
095700180306         endif;
095800180306
095900180306       ENDSR;
096000180306
096100180306       //--------------------------------------------------------------
096200180306       //?Sottomissione lavoro batch                                   ?
096300180306       //--------------------------------------------------------------
096400180306       BEGSR  sr_SbmJob;
096500180306
096600180306         // - ?Preperazione DS parametri?
096700180306         clear  TNSB19ds;
096800180306         TNSB19ds.SB19cwf = V1Cclrwf;
096900180306         TNSB19ds.SB19dal = W1Cdal;
097000180306         TNSB19ds.SB19al  = W1Cal;
097100180306
097200180306         Select;
097300180306
097400180306           // - ?Parzializzazione per Ragione Sociale / Località?
097500180306           When  ds_RS  <> *blank  or
097600180306                 ds_IND <> *blank  or
097700180306                 ds_LOC <> *blank  or
097800180306                 ds_PRV <> *blank;
097900180306             TNSB19ds.SB19ric = V1Cric;
098000180306             TNSB19ds.SB19rs1 = V1Crs1;
098100180306             TNSB19ds.SB19rs2 = V1Crs2;
098200180306             TNSB19ds.SB19rs3 = V1Crs3;
098300180306             TNSB19ds.SB19rs4 = V1Crs4;
098400180306             TNSB19ds.SB19rs5 = V1Crs5;
098500180306             TNSB19ds.SB19in1 = V1Cin1;
098600180306             TNSB19ds.SB19in2 = V1Cin2;
098700180306             TNSB19ds.SB19in3 = V1Cin3;
098800180306             TNSB19ds.SB19in4 = V1Cin4;
098900180306             TNSB19ds.SB19in5 = V1Cin5;
099000180306             TNSB19ds.SB19lo1 = V1Clo1;
099100180306             TNSB19ds.SB19lo2 = V1Clo2;
099200180306             TNSB19ds.SB19lo3 = V1Clo3;
099300180306             TNSB19ds.SB19lo4 = V1Clo4;
099400180306             TNSB19ds.SB19lo5 = V1Clo5;
099500180306             TNSB19ds.SB19pr1 = V1Cpr1;
099600180306             TNSB19ds.SB19pr2 = V1Cpr2;
099700180306             TNSB19ds.SB19pr3 = V1Cpr3;
099800180306             TNSB19ds.SB19pr4 = V1Cpr4;
099900180306             TNSB19ds.SB19pr5 = V1Cpr5;
100000180306
100100180306           // - ?Parzializzazione per Codice Cliente?
100200180306           When  ds_KSC <> *zero;
100300180306             TNSB19ds.SB19ric = 'K';
100400180306             if  V1Ccc1 > *zero;
100500180306               TNSB19ds.SB19rs1 = %editc( V1Ccc1 : 'X' );
100600180306             endif;
100700180306             if  V1Ccc2 > *zero;
100800180306               TNSB19ds.SB19rs2 = %editc( V1Ccc2 : 'X' );
100900180306             endif;
101000180306             if  V1Ccc3 > *zero;
101100180306               TNSB19ds.SB19rs3 = %editc( V1Ccc3 : 'X' );
101200180306             endif;
101300180306             if  V1Ccc4 > *zero;
101400180306               TNSB19ds.SB19rs4 = %editc( V1Ccc4 : 'X' );
101500180306             endif;
101600180306             if  V1Ccc5 > *zero;
101700180306               TNSB19ds.SB19rs5 = %editc( V1Ccc5 : 'X' );
101800180306             endif;
101900180306
102000180306           // - ?Parzializzazione per Spedizione?
102100180306           When  ds_SPE <> *zero;
102200180306             TNSB19ds.SB19ric = 'S';
102300180306             if  V1nsp1 > *zero;
102400180306               clear  ds_Sped;
102500180306               ds_Sped.AAS = V1aas1;
102600180306               ds_Sped.LNP = V1lnp1;
102700180306               ds_Sped.NRS = V1nrs1;
102800180306               ds_Sped.NSP = V1nsp1;
102900180306               TNSB19ds.SB19rs1 = ds_Sped;
103000180306             endif;
103100180306             if  V1nsp2 > *zero;
103200180306               clear  ds_Sped;
103300180306               ds_Sped.AAS = V1aas2;
103400180306               ds_Sped.LNP = V1lnp2;
103500180306               ds_Sped.NRS = V1nrs2;
103600180306               ds_Sped.NSP = V1nsp2;
103700180306               TNSB19ds.SB19rs2 = ds_Sped;
103800180306             endif;
103900180306             if  V1nsp3 > *zero;
104000180306               clear  ds_Sped;
104100180306               ds_Sped.AAS = V1aas3;
104200180306               ds_Sped.LNP = V1lnp3;
104300180306               ds_Sped.NRS = V1nrs3;
104400180306               ds_Sped.NSP = V1nsp3;
104500180306               TNSB19ds.SB19rs3 = ds_Sped;
104600180306             endif;
104700180306             if  V1nsp4 > *zero;
104800180306               clear  ds_Sped;
104900180306               ds_Sped.AAS = V1aas4;
105000180306               ds_Sped.LNP = V1lnp4;
105100180306               ds_Sped.NRS = V1nrs4;
105200180306               ds_Sped.NSP = V1nsp4;
105300180306               TNSB19ds.SB19rs4 = ds_Sped;
105400180306             endif;
105500180306             if  V1nsp5 > *zero;
105600180306               clear  ds_Sped;
105700180306               ds_Sped.AAS = V1aas5;
105800180306               ds_Sped.LNP = V1lnp5;
105900180306               ds_Sped.NRS = V1nrs5;
106000180306               ds_Sped.NSP = V1nsp5;
106100180306               TNSB19ds.SB19rs5 = ds_Sped;
106200180306             endif;
106300180306
106400180306           // - ?Parzializzazione per Padroncino?
106500180306           When  ds_PDR <> *zero;
106600180306             TNSB19ds.SB19ric = 'P';
106700180306             if  V1Cpd1 > *zero;
106800180306               TNSB19ds.SB19rs1 = %editc( V1Cpd1 : 'X' );
106900180306             endif;
107000180306             if  V1Cpd2 > *zero;
107100180306               TNSB19ds.SB19rs2 = %editc( V1Cpd2 : 'X' );
107200180306             endif;
107300180306             if  V1Cpd3 > *zero;
107400180306               TNSB19ds.SB19rs3 = %editc( V1Cpd3 : 'X' );
107500180306             endif;
107600180306             if  V1Cpd4 > *zero;
107700180306               TNSB19ds.SB19rs4 = %editc( V1Cpd4 : 'X' );
107800180306             endif;
107900180306             if  V1Cpd5 > *zero;
108000180306               TNSB19ds.SB19rs5 = %editc( V1Cpd5 : 'X' );
108100180306             endif;
108200180306
108300180306         EndSl;
108400180306
108500180306         KCOAZ = 'SB19';
108600180306         //*·KPJBU = TNSB19ds;
108700180306
108800180306         reset  TRUL01ds;
108900180306         //*·TRUL01ds.UL01opz = *on;    ?(già così)?
109000180306         TRUL01ds.UL01azn = KCOAZ;
109100180306         TRUL01ds.UL01prm = TNSB19ds;
109200180306         //*·TRUL01ds.UL01err = *off;   ?(già così)?
109300180306
109400180306         TRUL01R ( TRUL01ds );
109500180306
109600180306         if  TRUL01ds.UL01err = *on;
109700180306           $ErrMessage  = *on;
109800180306           $ErrGenerico = *on;
109900180306           VIDmsg = sk_Msg(11);
110000180306           leavesr;
110100180306         endif;
110200180306         KPJBU = %editc( TRUL01ds.UL01prg : 'X' ) + '-' +
110300180306                 %subst( TRUL01ds.UL01prm : 1 : %len(KPJBU) - 1 -
110400180306                                                %len(%trimr(KPJBU)) );
110500180306
110600180306         // - ?Sottomiossione del lavoro Batch?
110700180306         //*·BCH09 ( KPJBA );
110800180306         if  KNMUS = *all'1';
110900180306           tnsb19r ( kpjba );
111000180306         else;
111100180306           bch10 ( kpjba );
111200180306         endif;
111300180306
111400180306       ENDSR;
111500180306
111600180306       //--------------------------------------------------------------
111700180306       //?Esecuzione del comando (già impostato).                      ?
111800180306       //--------------------------------------------------------------
111900180306       BEGSR  sr_ExecCmd;
112000180306
112100180306         clear Qcap0100;
112200180306         Qcabcsdh = *off;
112300180306         Qcapa    = *off;
112400180306         Qcacmdss = *off;
112500180306         Qcaerved = *allX'00';
112600180306
112700180306         clear Qusec;
112800180306         Qusbprv  = %size(Qusec);
112900180306
113000180306         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
113100180306                           %size(Qcap0100) : 'CPOP0100' : *omit :
113200180306                           0 : 0 : Qusec);
113300180306
113400180306         //if  Qusei <> *blank;
113500180306         //  ...;
113600180306         //endif;
113700180306
113800180306       ENDSR;
113900180306
114000180306       //--------------------------------------------------------------
114100180306       //? Operazioni finali.                                           ?
114200180306       //--------------------------------------------------------------
114300180306       BEGSR  sr_RoutEnd;
114400180306
114500180306         // - ?Chiusura funzioni precedentemente aperte?
114600180306         reset  TIBS69ds;
114700180306         //*·I69tla = 'C';         ?(già così)?
114800180306         tibs69r( tibs69ds :
114900180306                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
115000180306
115100180306         // - ?Chiusura pgm?
115200180306         return;
115300180306
115400180306       ENDSR;
115500180306
115600180306      /end-free
115700180306
115800180306       //--------------------------------------------------------------
115900180306       //? Definizione schiere a tempo di compilazione                  ?
116000180306       //--------------------------------------------------------------
116100180306
116200180306** - ?sk_Msg: ?Messaggi di Errore?-------------------------------------------*
116300180306Immettere una selezione: Ragione Sociale/Codice Cliente/Spedizione/Padroncino  1
116400050902Tipo ricerca immesso errato: ammessi i valori "M", "D" e "E"                   2
116500050809Data formalmente errata                                                        3
116600050831Data iniziale > Data finale                                                    4
116700050902Immettere "S" (sì) o "N" (no)                                                  5
116800180305Già inserite selezioni per                                                     6
116900051024Codice cliente errato                                                          7
117000061205Se inserito il solo indirizzo inserire anche la provincia                      8
117100061228Carattere "'" non gestibile in selezione                                       9
117200061228Carattere "%" non gestibile in selezione                                       10
117300070629Rilevati problemi nell'impostazione parametri per l'azione batch               11
117400100128Lavoro NON eseguibile in questo Sistema Informativo. Premere Invio per uscire. 12
117500180305Immettere la linea di partenza                                                 13
117600180305Immettere il numero spedizione                                                 14
117700180305Immettere l'anno spedizione                                                    15
117800180308Numero spedizione inesistente                                                  16
117900180308Codice Padroncino errato                                                       17
118000180308Data precedente quella della prima bolla disponibile:                          18
