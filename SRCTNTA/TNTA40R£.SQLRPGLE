000100100126      //--------------------------------------------------------------
000200100126      //?TNTA40R - RICERCA CLIENTI X P.IVA RECUPERA SOTTOCONTO FATT.
000300100126      //--------------------------------------------------------------
000400100126
000500100126     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100126     h dftactgrp(*no) actgrp(*caller)
000700100126
000800100126      //---------------------------------------------------------------
000900100126      //?Dichiarazione file.
001000100126      //---------------------------------------------------------------
001100100126
001200100126      // - Video
001300100126     fTNTA40D   cf   e             workstn
001400100126     f                                     sfile(TA40S01 : S01nrr)
001500100126     f                                     indds(IndDspF)
001600100126     f                                     infds(InfDspF)
001700100126
001800100126      //---------------------------------------------------------------
001900100126      //?Definizione costanti.
002000100126      //---------------------------------------------------------------
002100100126
002200100126      // - Tasti funzionali a video
002300100126     d c_F01           c                   const(x'31')
002400100126     d c_F02           c                   const(x'32')
002500100126     d c_F03           c                   const(x'33')
002600100126     d c_F04           c                   const(x'34')
002700100126     d c_F05           c                   const(x'35')
002800100126     d c_F06           c                   const(x'36')
002900100126     d c_F07           c                   const(x'37')
003000100126     d c_F08           c                   const(x'38')
003100100126     d c_F09           c                   const(x'39')
003200100126     d c_F10           c                   const(x'3A')
003300100126     d c_F11           c                   const(x'3B')
003400100126     d c_F12           c                   const(x'3C')
003500100126     d c_F13           c                   const(x'B1')
003600100126     d c_F14           c                   const(x'B2')
003700100126     d c_F15           c                   const(x'B3')
003800100126     d c_F16           c                   const(x'B4')
003900100126     d c_F17           c                   const(x'B5')
004000100126     d c_F18           c                   const(x'B6')
004100100126     d c_F19           c                   const(x'B7')
004200100126     d c_F20           c                   const(x'B8')
004300100126     d c_F21           c                   const(x'B9')
004400100126     d c_F22           c                   const(x'BA')
004500100126     d c_F23           c                   const(x'BB')
004600100126     d c_F24           c                   const(x'BC')
004700100126     d c_Enter         c                   const(x'F1')
004800100126     d c_RollDown      c                   const(x'F4')
004900100126     d c_RollUp        c                   const(x'F5')
005000100126
005100100126      //---------------------------------------------------------------
005200100126      //?Definizione schiere.
005300100126      //---------------------------------------------------------------
005400100127
005500100127      // - Sk comodo codici int.fattura
005600100127     d $CLI            s              7  0 dim(99)
005700100126
005800100126      // - Messaggi di errore
005900100126     d $Msg            s             78    dim(20) ctdata perrcd(1)
006000100126
006100100126      //---------------------------------------------------------------
006200100126      //?Definizione aree dati.
006300100126      //---------------------------------------------------------------
006400100126
006500100126      // - Dati utente
006600100126     d §AzUte        e ds                  extname(AZUTE00F)
006700100126     d                                     dtaara
006800100126     d §DatiUte      e ds                  extname(dDatiUte)
006900100126     d                                     dtaara
007000100126
007100100126      //---------------------------------------------------------------
007200100126      //?Definizione strutture dati.
007300100126      //---------------------------------------------------------------
007400100126
007500100126      // - Status
007600100126     d Psds           sds
007700100126     d   SDSpgm          *proc
007800100126
007900100126      // - InfDS
008000100126     d InfDspF         ds
008100100126     d  dsp_aid              369    369a
008200100126     d  dsp_rig              370    370
008300100126     d  dsp_col              371    371
008400100126
008500100126      // - Indicatori su DspF
008600100126     d IndDspF         ds
008700100126        // - Indicatori di gestione del subfile
008800100126     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008900100126     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009000100126     d  SflEnd                        1n   overlay(IndDspF : 32)
009100100126        // - Indicatori di errore
009200100126     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300100126        // - Indicatori di errore generico
009400100126     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009500100126
009600100126      // Campi di comodo per gestione indicatori a video
009700100126     d WindDspF        ds                  inz
009800100126     d   WindDspFa             1     49    inz(*zero)
009900100126     d   WindDspFb            50     99    inz(*zero)
010000100126
010100100126      // - Parametri ricevuti
010200100126     d KPJBA         e ds
010300100126     d TNTA40DS      e ds
010400100126
010500100126       // - Reperimento dati anagrafici
010600100126     d TIBS69ds      e ds
010700100126     d DS_cnaco      e ds                  inz extname(CNACO00F)
010800100126     d DS_cnind      e ds                  inz extname(CNIND00F)
010900100126     d DS_cnclp      e ds                  inz extname(CNCLP00F)
011000100126     d DS_fncls      e ds                  inz extname(FNCLS00F)
011100100126
011200100126      // - Reperimento dati utente
011300100126     d TIBS34ds      e ds
011400100126     d dUte01        e ds
011500100126
011600100126      // - Tabella LAT = Autorizzazione
011700100126     d dlat          e ds                  inz
011800100126
011900100126      //---------------------------------------------------------------
012000100126      //?Definizione variabili globali.
012100100126      //---------------------------------------------------------------
012200100126
012300100126      // - Flags booleani
012400100126     d $EoF            s               n   inz(*off)
012500100126     d $Fine           s               n   inz(*off)
012600100126     d $InzW01         s               n   inz(*on)
012700100126
012800100126      // - Indici di schiera
012900100126     d xx              s              3  0 inz
013000100126
013100100126      // - Campi associati al video
013200100126     d $Video          s              2    inz('W1')
013300100126     d S01Nrr          s              4  0 inz
013400100126
013500100126     d dsp_mod         s             10I 0
013600100126
013700100126      // - Campi di comodo data
013800100126     d  data_eur       s               d   Datfmt(*eur)
013900100126     d  data_iso       s               d   Datfmt(*iso)
014000100126
014100100127      // - Campi di comodo per procedura data ultima fatturazione
014200100127     d  esito          s             10i 0
014300100128     d  csc            s              7p 0
014400100127     d  drufc          s               d
014500100126
014600100127      // - Campi di comodo
014700100128
014800100128      //---------------------------------------------------------------
014900100128      //?Ordinamento SBFL
015000100128      //---------------------------------------------------------------
015100100128      // ----------------------------------------------------------------------
015200100128      //?Costanti per ordinamento subfile
015300100128      // ----------------------------------------------------------------------
015400100128     d MaxKey          c                   4
015500100128     d Ascendente      c                   1
015600100128     d Discendente     c                   2
015700100128     d Carattere       c                   6
015800100128     d Numerico        c                   8
015900100128     d Put             c                   1
016000100128     d EndPut          c                   2
016100100128     d Get             c                   3
016200100128      **********************************************************************
016300100128      * Campi utili:
016400100128      *     nrr        - Numero relativo di record del Subfile
016500100128      *     SizeList   - Dimensione della lista
016600100128      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
016700100128      **********************************************************************
016800100128     d NotUsed         s             16A
016900100128     d ReturnSize      s              9B 0
017000100128     d SizeList        s              9B 0
017100100128     d RrnLast         s              5I 0
017200100128     d WrkSort         s              1  0 inz(0)
017300100128      **********************************************************************
017400100128      * Data Structures
017500100128      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
017600100128      *     QLGSCB     - The sort request block for the QLGSORT API
017700100128      *     QLGSCB00   - The sort request block for the QLGSRTIO API
017800100128      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
017900100128      *     QUSEC      - Error structure for the QLGSORT API
018000100128      **********************************************************************
018100100128     d sflrcd          ds
018200100128     d  wh1data
018300100128     d  ws1opz
018400100128     d  ws1ksc
018500100128     d  ws1rag
018600100128     d  ws1data
018700100128     d  selected                      1A
018800100128
018900100128      /COPY QSYSINC/QRPGLESRC,QLGSORT
019000100128     d QLGKL                         16    DIM(MaxKey)
019100100128     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
019200100128     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
019300100128     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
019400100128     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
019500100128
019600100128      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
019700100128      /COPY QSYSINC/QRPGLESRC,QUSEC
019800100128
019900100127
020000100126      //---------------------------------------------------------------
020100100126      //?Definizione procedure esterne.
020200100126      //---------------------------------------------------------------
020300100128
020400100128     d Qlgsort_pr      pr                  extpgm('QLGSORT')
020500100128     d  pr_QLGSCB                          like(Qlgscb)
020600100128     d  pr_NotUsed1                        like(NotUsed)
020700100128     d  pr_NotUsed2                        like(NotUsed)
020800100128     d  pr_SizeList                        like(SizeList)
020900100128     d  pr_ReturnSize                      like(ReturnSize)
021000100128     d  pr_QUSEC                           like(Qusec)
021100100128     d                                     options(*varsize)
021200100128
021300100128     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
021400100128     d  pr_QLGSCB00                        like(QLGSCB00)
021500100128     d  pr_SflRcd                          like(SflRcd)
021600100128     d  pr_NotUsed1                        like(NotUsed)
021700100128     d  pr_SizeList                        like(SizeList)
021800100128     d  pr_NotUsed2                        like(NotUsed)
021900100128     d  pr_QUSEC                           like(Qusec)
022000100128     d                                     options(*varsize)
022100100128
022200100128     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
022300100128     d  pr_QLGSCB00                        like(QLGSCB00)
022400100128     d  pr_NotUsed1                        like(NotUsed)
022500100128     d  pr_SflRcd                          like(SflRcd)
022600100128     d  pr_SizeList                        like(SizeList)
022700100128     d  pr_NotUsed2                        like(NotUsed)
022800100128     d  pr_QUSEC                           like(Qusec)
022900100128     d                                     options(*varsize)
023000100128
023100100128      //---------------------------------------------------------------
023200100128      //?Definizione prototipi.
023300100128      //---------------------------------------------------------------
023400100126
023500100126      /copy gaitrasrc/srcprotopr,tibs69r
023600100126      /copy gaitrasrc/srcprotopr,tibs34r
023700100127      /copy gaitrasrc/srcprotopr,ycodrufc
023800100126
023900100126      //---------------------------------------------------------------
024000100126      //?Definizione key-list.
024100100126      //---------------------------------------------------------------
024200100126
024300100126
024400100126      //---------------------------------------------------------------
024500100126      //?Riepilogo indicatori.
024600100126      //---------------------------------------------------------------
024700100126      // 28    : Emissione messaggio di errore a video
024800100126      // 30    : Condiziona SFLDSP
024900100126      // 31    : Condiziona SFLDSPCTL
025000100126      // 32    : Condiziona SFLEND
025100100126      // 99    : Generico di Errore
025200100126      //---------------------------------------------------------------
025300100126
025400100126      //---------------------------------------------------------------
025500100126      //?M A I N - L I N E
025600100126      //---------------------------------------------------------------
025700100126
025800100126     c     *Entry        plist
025900100126     c                   parm                    KPJBA
026000100126     c                   parm                    TNTA40ds
026100100126
026200100126      /free
026300100126
026400100126       //?Operazioni iniziali
026500100126       exsr sr_RoutInz;
026600100126
026700100126       //?Gestione video
026800100126       DOW Not $Fine;
026900100126         select;
027000100126           when $Video = 'W1';
027100100126             exsr GesW01;
027200100126           other;
027300100126             $Fine = *on;
027400100126         endsl;
027500100126       ENDDO;
027600100126
027700100126       //?Operazioni finali
027800100126       exsr sr_RoutEnd;
027900100126
028000100126       //--------------------------------------------------------------
028100100126       //?Operazioni iniziali.
028200100126       //--------------------------------------------------------------
028300100126       BEGSR sr_RoutInz;
028400100126
028500100126         //?Reperimento dati job
028600100126         exsr DatiJob;
028700100127
028800100127         //?Se tipo lancio sbagliato imposto fisso blank
028900100127         IF  ITA40tla <> 'C' and ITA40tla <> 'L' and
029000100127             ITA40tla <> *blanks;
029100100127           clear ITA40tla;
029200100127         ENDIF;
029300100127
029400100127         //?Se tipo lancio 'C' vado subito a fine e chiudo
029500100127         //?solo i file
029600100127         IF  ITA40tla = 'C';
029700100127           $Fine = *on;
029800100127         ENDIF;
029900100127
030000100127         //?Se tipo elaborazione sbagliato imposto blank
030100100128         IF  ITA40fle <> 'A' and ITA40fle <> *blanks;
030200100127           clear ITA40fle;
030300100127         ENDIF;
030400100127
030500100126         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
030600100126
030700100126       ENDSR;
030800100126
030900100126       //--------------------------------------------------------------
031000100126       //?Reperimento Dati del job (Utente/Operativi).
031100100126       //--------------------------------------------------------------
031200100126       BEGSR DatiJob;
031300100126
031400100126         in(E) §AzUte;
031500100126         if NOT %error;
031600100126           in(E) §DatiUte;
031700100126         endif;
031800100126         if %error or RSut = *blanks;
031900100126           clear TIBS34ds;
032000100126           tibs34r(tibs34ds);
032100100126           in §AzUte;
032200100126           in §DatiUte;
032300100126         endif;
032400100126
032500100126       ENDSR;
032600100126
032700100126       //--------------------------------------------------------------
032800100127       //?Gestione videata W01.
032900100127       //--------------------------------------------------------------
033000100127       BEGSR GesW01;
033100100127
033200100127       //?Inizializzazione videata
033300100127         if $InzW01;
033400100127           exsr InzW01;
033500100127           $InzW01 = *off;
033600100127         endif;
033700100127
033800100127         $Eof   = *off;
033900100127
034000100127       //?Emissione videata
034100100127         SELECT;
034200100128         WHEN  ITA40fle = 'A' and s01nrr <= 1;
034300100127           $Fine = *on;
034400100127           leavesr ;
034500100127
034600100127         WHEN  s01nrr = 0;
034700100127           exfmt TA40V01;
034800100127
034900100127         WHEN  s01nrr > 0;
035000100128         SflDsp_n = *off;
035100100128         SflEnd = *on;
035200100128         WC1rcd = 1;
035300100127         write TA40Z01;
035400100127         exfmt TA40C01;
035500100127         ErrMessage   = *off;
035600100127         ErrGenerico  = *off;
035700100127         clear WC1msg;
035800100127
035900100127         ENDSL;
036000100127
036100100127         SELECT;
036200100127
036300100127       //?F12 = Ritorno
036400100127           WHEN dsp_aid = c_F12;
036500100127             exsr sr_F12W01;
036600100127             leavesr ;
036700100127
036800100127       //?Enter
036900100127           OTHER;
037000100127       //?Controllo i dati immessi nel subfile
037100110427             IF  s01nrr > 0;
037200100127             exsr CtrW01;
037300100127             IF  ErrGenerico;
037400100127               leavesr;
037500100127             ENDIF;
037600110427             ENDIF;
037700100127
037800100127         ENDSL;
037900100127
038000100127       ENDSR;
038100100127
038200100127       //--------------------------------------------------------------
038300100127       //?Caricamento videata W01.
038400100127       //--------------------------------------------------------------
038500100127       BEGSR Inzw01;
038600100127
038700100127         //?Pulizia subfile
038800100127         exsr Sr_PulSfl;
038900100127
039000100127         //?Carico subfile
039100100127         exsr sr_CarW01;
039200100127
039300100127       ENDSR;
039400100127
039500100127       //--------------------------------------------------------------
039600100127       //?Pulisco subfile.
039700100127       //--------------------------------------------------------------
039800100127       BEGSR sr_PulSfl;
039900100127
040000100127         SflDsp_N    = *on;
040100100127         SflDspCtl_N = *on;
040200100127         write  TA40C01;
040300100127         SflDspCtl_N = *off;
040400100127         SflEnd      = *off;
040500100127
040600100127         clear WC1rcd;
040700100127         clear WC1msg;
040800100127         ErrMessage  = *off;
040900100127         ErrGenerico = *off;
041000100127         clear S01Nrr;
041100100127
041200100127       ENDSR;
041300100127
041400100127       //--------------------------------------------------------------
041500100127       //?Carico il subfile.
041600100127       //--------------------------------------------------------------
041700100127       BEGSR sr_CarW01;
041800100127
041900100127         //?Pulisco sk di comodo
042000100127         clear $CLI;
042100100127
042200100127         //?Recupero i codici fatturazione con p.Iva = a quella passata
042300100127         exec sql
042400100127          declare CLI cursor for
042500100127          select clpscf
042600100127          from CNIND02L join CNCLP00F on
042700100127           indkut = clpkut and indkcc = clpkcc and
042800100127           indksc = clpksc
042900100128          where indiva = :ITA40iva and indiva <> '' and
043000100128           clpscf > 0
043100100127          order by indkut, indkcc, indiva;
043200100127
043300100127         exec sql
043400100127          open CLI;
043500100127         IF  sqlcode < 0;
043600100127           $EoF = *on;
043700100127         ENDIF;
043800100127
043900100127         DOW  not $EoF;
044000100127           exec sql
044100100127            fetch next from CLI into :clpscf;
044200100127           IF  sqlcod = 100 or sqlcod < 0;
044300100127             $EoF = *on;
044400100127             leave;
044500100127           ENDIF;
044600100127
044700100127         //?Incremento la schiera di comodo
044800100127           xx = %lookup(clpscf : $CLI);
044900100127           IF  xx = 0;
045000100127             xx = %lookup(*zeros : $CLI);
045100100127             IF  xx > 0;
045200100127               $CLI(xx) = clpscf;
045300100127             ENDIF;
045400100127           ENDIF;
045500100127
045600100127         ENDDO;
045700100127
045800100127         exec sql close CLI;
045900100127
046000100128         //?Se non ho caricato la schiera di comodo
046100100128         //?vuol dire che non ho trovato altri clienti codificati
046200100128         IF  $CLI(1) = 0;
046300100128         //?Se è stata richiesta la chiamata al buio ritorno
046400100128         //?il ksc passato al programma chiamante
046500100128           IF  ITA40fle = '1';
046600100128             OTA40scf = ITA40ksc;
046700100128             leavesr;
046800100128           ENDIF;
046900100128         //?Se è richiesta la visualizzazione dei codici
047000100128         //?carico il subfile con solo il ksc passato
047100100128         //?se > 0
047200100128           IF  ITA40ksc > 0;
047300100128             clear TA40S01;
047400100129             WS1opz = '1';
047500100128             WS1ksc = ITA40ksc;
047600100128             WS1rag = ITA40rag;
047700100128             S01Nrr += 1;
047800100128             write TA40S01;
047900100128           ENDIF;
048000100128           leavesr;
048100100128         ENDIF;
048200100127
048300100127         //?Se ho caricato solo un codice nella schiera di comodo vuol dire che
048400100128         //?vuol dire che c'è solo questo cliente con questa p.IVA
048500100128         IF  $CLI(2) = 0;
048600100128         //?Se è stata richiesta la chiamata al buio ritorno
048700100128         //?il codice fatturazione trovato al programma chiamante
048800100128           IF  ITA40fle = '1';
048900100128             OTA40scf = $CLI(1);
049000100128             leavesr;
049100100128           ENDIF;
049200100128         //?Se è richiesta la visualizzazione dei codici
049300100128         //?carico il subfile con solo il ksc passato
049400100128           clear TA40S01;
049500100129           WS1opz = '1';
049600100128           WS1ksc = $CLI(1);
049700100128           //?Decodifico cliente
049800100128           exsr dec_CLI;
049900100128           WS1rag   = acorag;
050000100128           //?Data ultima registrazione fattura
050100100128           exsr data_REG;
050200100128           data_eur = drufc;
050300100128             IF  data_eur <> *loval;
050400100128               WH1data = %dec(drufc);
050500100128               WS1data = %dec(data_eur);
050600100128             ENDIF;
050700100128           S01Nrr += 1;
050800100128           write TA40S01;
050900100128           leavesr;
051000100128         ENDIF;
051100100127
051200100128         //?Se arrivo qua vuol dire che ho caricato più di un codice
051300100127         //?Ordino la schiera di comodo
051400100127         sorta $CLI;
051500100127
051600100127         //?Carico il subfile con i dati della sk di comodo
051700100127         //?ma solo se richiesto dal pgm chiamante
051800100127         IF  ITA40fle = *blanks;
051900100127           xx = 1;
052000100127           FOR  xx by 1 to %elem($CLI);
052100100127             IF  $CLI(xx) = 0;
052200100127               iter;
052300100127             ENDIF;
052400100127
052500100127             clear TA40S01;
052600100127
052700100127             clear WS1opz;
052800100127             WS1ksc   = $cli(xx);
052900100127
053000100127           //?Decodifico cliente
053100100127             exsr dec_CLI;
053200100127             WS1rag   = acorag;
053300100127
053400100127           //?Data ultima registrazione fattura
053500100128             exsr data_REG;
053600100128             data_eur = drufc;
053700100128           //?se data = al valore minimo imposto 0
053800100128             IF  data_eur <> *loval;
053900100128               WH1data = %dec(drufc);
054000100128               WS1data = %dec(data_eur);
054100100128             ENDIF;
054200100127
054300100127             S01Nrr += 1;
054400100127             write TA40S01;
054500100127           ENDFOR;
054600100128
054700100128         //?Ordino il subfile per data registrazione fattura
054800100128           rrnlast = s01nrr;
054900100128           exsr sr_ordina;
055000100127
055100100127         ENDIF;
055200100127
055300100127       ENDSR;
055400100127
055500100127       //--------------------------------------------------------------
055600100127       //?Controllo videata W01.
055700100127       //--------------------------------------------------------------
055800100127       BEGSR CtrW01;
055900100127
056000100127         WindDspF = IndDspF;
056100100127         reset WindDspFb;
056200100127         IndDspF  = WindDspF;
056300100127
056400100127         clear S01Nrr;
056500100127
056600100127         DOW  not $EoF;
056700100127           S01Nrr += 1;
056800100127           chain S01Nrr TA40S01;
056900100127           IF  not %found;
057000100127             $EoF = *on;
057100100127             leave;
057200100127           ENDIF;
057300100127
057400100127         //?Opzione '1' ritorno il codice scelto
057500100127           IF  WS1opz = '1';
057600100128             OTA40scf = WS1ksc;
057700100127             $Fine = *on;
057800100127             $EoF = *on;
057900100127             leave;
058000100127           ENDIF;
058100100127
058200100127           WC1Rcd = S01Nrr;
058300100127           update TA40S01;
058400100127         ENDDO;
058500100127
058600100127       ENDSR;
058700100127
058800100127       //--------------------------------------------------------------
058900100127       //?Gestione tasto funzionale F12 da videata W01.
059000100127       //?F12=Ritorno
059100100127       //--------------------------------------------------------------
059200100127       BEGSR sr_F12W01;
059300100127
059400100127         // Ritorno al pgm richiamante
059500100128         OTA40scf = ITA40scf;
059600100127         OTA40f12 = 'S';
059700100127         $Fine    = *on;
059800100127
059900100127       ENDSR;
060000100127
060100100127       //--------------------------------------------------------------
060200100127       //?Decodifico il cliente.
060300100127       //--------------------------------------------------------------
060400100127       BEGSR dec_CLI;
060500100127
060600100127         clear tibs69ds;
060700100127         I69kac = WS1KSC;
060800100127         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
060900100127
061000100127       ENDSR;
061100100127
061200100127       //--------------------------------------------------------------
061300100127       //?Recupero data ultima registrazione fattura.
061400100127       //--------------------------------------------------------------
061500100127       BEGSR data_REG;
061600100128
061700100128         csc = WS1ksc;
061800100127
061900100127         DataRegistrazioneUltimaFattura ('GETDRUFC' : esito
062000100128                                                    : csc
062100100127                                                    : drufc);
062200100127
062300100127       ENDSR;
062400100128
062500100128       //--------------------------------------------------------------
062600100128       //?Ordinamento subfile.
062700100128       //--------------------------------------------------------------
062800100128       BEGSR sr_ordina;
062900100128
063000100128       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
063100100128       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
063200100128       // il campo è usato per selezionare i records dando un ordine a quelli
063300100128       // selezionati davanti ai non selezionati.
063400100128         clear QLGSCB;
063500100128         clear QLGSCB00;
063600100128
063700100128       // 2 campi chiave x Data + cliente
063800100128         QLGNBRK = 2;
063900100128
064000100128       // imposto la posizione della Data     sul subfile e la lunghezza
064100100128       // l'ordinamento è su un campo numerico e deve essere
064200100128       // ascending
064300100128         QLGSP = 1;
064400100128         QLGSS = %SIZE(wh1data);
064500100128         QLGDT = Numerico;
064600100128         QLGSO = Discendente;
064700100128         QLGKL(1) = QLGSKL;
064800100128
064900100128       // imposto la posizione del Cliente    sul subfile e la lunghezza
065000100128       // l'ordinamento è su un campo numerico e deve essere
065100100128       // ascending
065200100128         QLGSP = 1 + %size(ws1opz);
065300100128         QLGSS = %SIZE(ws1ksc);
065400100128         QLGDT = Numerico;
065500100128         QLGSO = Ascendente;
065600100128         QLGKL(2) = QLGSKL;
065700100128
065800100128       // Load other sort parameters.
065900100128         QLGLB = 80 + 16 * MaxKey;
066000100128         QLGRL = %SIZE(sflrcd) - 1;
066100100128         QLGRT = 8;
066200100128         QLGOKL = 80;
066300100128         QLGLKE = 16;
066400100128         QLGLSS = 290;
066500100128
066600100128       // Initialize Sort I/O API fields.
066700100128         QLGRL00 = QLGRL;
066800100128         QLGRC00 = 1;
066900100128         clear QUSEI;
067000100128         QUSBPRV = %SIZE(QUSEC);
067100100128
067200100128       // First step - Initialize the sort routine.
067300100128         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
067400100128
067500100128       // Next step - Write records to I/O routine.
067600100128         QLGRT00 = Put;
067700100128         for xx = 1 to rrnlast;
067800100128          chain xx TA40S01;
067900100128
068000100128       // solo le righe con Selected = 'Y' sono riordinate,
068100100128       // quindi per fare un ordinamento di tutte le righe
068200100128       // metto 'Y' sempre.
068300100128          selected  = 'Y';
068400100128          clear QUSEI;
068500100128          QUSBPRV = %SIZE(QUSEC);
068600100128          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
068700100128         endfor;
068800100128
068900100128       // Next step - Signal end of input, clear subfile for reload.
069000100128         QLGRT00 = EndPut;
069100100128         clear QUSEI;
069200100128         QUSBPRV = %SIZE(QUSEC);
069300100128         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
069400100128       // pulizia SFL
069500100128         exsr Sr_PulSfl;
069600100128
069700100128       // Final step - Write the records back to the subfile.
069800100128         QLGRT00 = Get;
069900100128         for xx = 1 to rrnlast;
070000100128          clear QUSEI;
070100100128          QUSBPRV = %SIZE(QUSEC);
070200100128          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
070300100128          s01nrr = xx;
070400100128          write TA40S01;
070500100128         endfor;
070600100128
070700100128       ENDSR;
070800090610
070900100127       //--------------------------------------------------------------
071000100127       //?Operazioni finali.
071100100127       //--------------------------------------------------------------
071200100127       BEGSR sr_RoutEnd;
071300100127
071400100127         //?Ritorno errore
071500100127         IF  ErrGenerico;
071600100127           OTA40err = 'E';
071700100127           OTA40msg = WC1msg;
071800100127         ENDIF;
071900100127
072000100127         //?Chiudo file quando richiamato per sola chiusura file
072100100127         IF  ITA40tla = 'C';
072200100127           DataRegistrazioneUltimaFattura ('SETONLRC' : esito);
072300100127         ENDIF;
072400100127
072500100128         *inLR = *on;
072600100127         return;
072700100127
072800100127       ENDSR;
072900100127
073000100127      /end-free
073100100127
073200100127       //--------------------------------------------------------------
073300100127       //?Schiere a tempo di compilazione.
073400100127       //--------------------------------------------------------------
073500100127
073600090610** - $MSG -------------------------------------------------------------------*
073700100127Partiva IVA non passata                                                       01
073800100127Cliente non passato                                                           02
