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
001100161130      // - File Tabelle
001200161130     fTABEL00F  if   e           k disk
001300100126
001400100126      // - Video
001500100126     fTNTA40D   cf   e             workstn
001600100126     f                                     sfile(TA40S01 : S01nrr)
001700100126     f                                     indds(IndDspF)
001800100126     f                                     infds(InfDspF)
001900100126
002000100126      //---------------------------------------------------------------
002100100126      //?Definizione costanti.
002200100126      //---------------------------------------------------------------
002300100126
002400100126      // - Tasti funzionali a video
002500100126     d c_F01           c                   const(x'31')
002600100126     d c_F02           c                   const(x'32')
002700100126     d c_F03           c                   const(x'33')
002800100126     d c_F04           c                   const(x'34')
002900100126     d c_F05           c                   const(x'35')
003000100126     d c_F06           c                   const(x'36')
003100100126     d c_F07           c                   const(x'37')
003200100126     d c_F08           c                   const(x'38')
003300100126     d c_F09           c                   const(x'39')
003400100126     d c_F10           c                   const(x'3A')
003500100126     d c_F11           c                   const(x'3B')
003600100126     d c_F12           c                   const(x'3C')
003700100126     d c_F13           c                   const(x'B1')
003800100126     d c_F14           c                   const(x'B2')
003900100126     d c_F15           c                   const(x'B3')
004000100126     d c_F16           c                   const(x'B4')
004100100126     d c_F17           c                   const(x'B5')
004200100126     d c_F18           c                   const(x'B6')
004300100126     d c_F19           c                   const(x'B7')
004400100126     d c_F20           c                   const(x'B8')
004500100126     d c_F21           c                   const(x'B9')
004600100126     d c_F22           c                   const(x'BA')
004700100126     d c_F23           c                   const(x'BB')
004800100126     d c_F24           c                   const(x'BC')
004900100126     d c_Enter         c                   const(x'F1')
005000100126     d c_RollDown      c                   const(x'F4')
005100100126     d c_RollUp        c                   const(x'F5')
005200100126
005300100126      //---------------------------------------------------------------
005400100126      //?Definizione schiere.
005500100126      //---------------------------------------------------------------
005600100127
005700100127      // - Sk comodo codici int.fattura
005800100127     d $CLI            s              7  0 dim(99)
005900100126
006000100126      // - Messaggi di errore
006100100126     d $Msg            s             78    dim(20) ctdata perrcd(1)
006200100126
006300100126      //---------------------------------------------------------------
006400100126      //?Definizione aree dati.
006500100126      //---------------------------------------------------------------
006600100126
006700100126      // - Dati utente
006800100126     d §AzUte        e ds                  extname(AZUTE00F)
006900100126     d                                     dtaara
007000100126     d §DatiUte      e ds                  extname(dDatiUte)
007100100126     d                                     dtaara
007200100126
007300100126      //---------------------------------------------------------------
007400100126      //?Definizione strutture dati.
007500100126      //---------------------------------------------------------------
007600100126
007700100126      // - Status
007800100126     d Psds           sds
007900100126     d   SDSpgm          *proc
008000100126
008100100126      // - InfDS
008200100126     d InfDspF         ds
008300100126     d  dsp_aid              369    369a
008400100126     d  dsp_rig              370    370
008500100126     d  dsp_col              371    371
008600100126
008700100126      // - Indicatori su DspF
008800100126     d IndDspF         ds
008900100126        // - Indicatori di gestione del subfile
009000100126     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009100100126     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009200100126     d  SflEnd                        1n   overlay(IndDspF : 32)
009300100126        // - Indicatori di errore
009400100126     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500100126        // - Indicatori di errore generico
009600100126     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009700100126
009800100126      // Campi di comodo per gestione indicatori a video
009900100126     d WindDspF        ds                  inz
010000100126     d   WindDspFa             1     49    inz(*zero)
010100100126     d   WindDspFb            50     99    inz(*zero)
010200100126
010300100126      // - Parametri ricevuti
010400100126     d KPJBA         e ds
010500100126     d TNTA40DS      e ds
010600100126
010700100126       // - Reperimento dati anagrafici
010800100126     d TIBS69ds      e ds
010900100126     d DS_cnaco      e ds                  inz extname(CNACO00F)
011000100126     d DS_cnind      e ds                  inz extname(CNIND00F)
011100100126     d DS_cnclp      e ds                  inz extname(CNCLP00F)
011200100126     d DS_fncls      e ds                  inz extname(FNCLS00F)
011300100126
011400100126      // - Reperimento dati utente
011500100126     d TIBS34ds      e ds
011600100126     d dUte01        e ds
011700161130
011800161130      // - Tabella 4W = Stato del Credito
011900161130     d ds4W          e ds
012000100126
012100100126      // - Tabella LAT = Autorizzazione
012200100126     d dlat          e ds                  inz
012300100126
012400100126      //---------------------------------------------------------------
012500100126      //?Definizione variabili globali.
012600100126      //---------------------------------------------------------------
012700100126
012800100126      // - Flags booleani
012900100126     d $EoF            s               n   inz(*off)
013000100126     d $Fine           s               n   inz(*off)
013100100126     d $InzW01         s               n   inz(*on)
013200100126
013300100126      // - Indici di schiera
013400100126     d xx              s              3  0 inz
013500100126
013600100126      // - Campi associati al video
013700100126     d $Video          s              2    inz('W1')
013800100126     d S01Nrr          s              4  0 inz
013900100126
014000100126     d dsp_mod         s             10I 0
014100100126
014200100126      // - Campi di comodo data
014300100126     d  data_eur       s               d   Datfmt(*eur)
014400100126     d  data_iso       s               d   Datfmt(*iso)
014500100126
014600100127      // - Campi di comodo per procedura data ultima fatturazione
014700100127     d  esito          s             10i 0
014800100128     d  csc            s              7p 0
014900100127     d  drufc          s               d
015000100126
015100100127      // - Campi di comodo
015200100128
015300100128      //---------------------------------------------------------------
015400100128      //?Ordinamento SBFL
015500100128      //---------------------------------------------------------------
015600100128      // ----------------------------------------------------------------------
015700100128      //?Costanti per ordinamento subfile
015800100128      // ----------------------------------------------------------------------
015900100128     d MaxKey          c                   4
016000100128     d Ascendente      c                   1
016100100128     d Discendente     c                   2
016200100128     d Carattere       c                   6
016300100128     d Numerico        c                   8
016400100128     d Put             c                   1
016500100128     d EndPut          c                   2
016600100128     d Get             c                   3
016700100128      **********************************************************************
016800100128      * Campi utili:
016900100128      *     nrr        - Numero relativo di record del Subfile
017000100128      *     SizeList   - Dimensione della lista
017100100128      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
017200100128      **********************************************************************
017300100128     d NotUsed         s             16A
017400100128     d ReturnSize      s              9B 0
017500100128     d SizeList        s              9B 0
017600100128     d RrnLast         s              5I 0
017700100128     d WrkSort         s              1  0 inz(0)
017800100128      **********************************************************************
017900100128      * Data Structures
018000100128      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
018100100128      *     QLGSCB     - The sort request block for the QLGSORT API
018200100128      *     QLGSCB00   - The sort request block for the QLGSRTIO API
018300100128      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
018400100128      *     QUSEC      - Error structure for the QLGSORT API
018500100128      **********************************************************************
018600100128     d sflrcd          ds
018700100128     d  wh1data
018800100128     d  ws1opz
018900100128     d  ws1ksc
019000100128     d  ws1rag
019100100128     d  ws1data
019200100128     d  selected                      1A
019300100128
019400100128      /COPY QSYSINC/QRPGLESRC,QLGSORT
019500100128     d QLGKL                         16    DIM(MaxKey)
019600100128     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
019700100128     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
019800100128     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
019900100128     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
020000100128
020100100128      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
020200100128      /COPY QSYSINC/QRPGLESRC,QUSEC
020300100128
020400100127
020500100126      //---------------------------------------------------------------
020600100126      //?Definizione procedure esterne.
020700100126      //---------------------------------------------------------------
020800100128
020900100128     d Qlgsort_pr      pr                  extpgm('QLGSORT')
021000100128     d  pr_QLGSCB                          like(Qlgscb)
021100100128     d  pr_NotUsed1                        like(NotUsed)
021200100128     d  pr_NotUsed2                        like(NotUsed)
021300100128     d  pr_SizeList                        like(SizeList)
021400100128     d  pr_ReturnSize                      like(ReturnSize)
021500100128     d  pr_QUSEC                           like(Qusec)
021600100128     d                                     options(*varsize)
021700100128
021800100128     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
021900100128     d  pr_QLGSCB00                        like(QLGSCB00)
022000100128     d  pr_SflRcd                          like(SflRcd)
022100100128     d  pr_NotUsed1                        like(NotUsed)
022200100128     d  pr_SizeList                        like(SizeList)
022300100128     d  pr_NotUsed2                        like(NotUsed)
022400100128     d  pr_QUSEC                           like(Qusec)
022500100128     d                                     options(*varsize)
022600100128
022700100128     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
022800100128     d  pr_QLGSCB00                        like(QLGSCB00)
022900100128     d  pr_NotUsed1                        like(NotUsed)
023000100128     d  pr_SflRcd                          like(SflRcd)
023100100128     d  pr_SizeList                        like(SizeList)
023200100128     d  pr_NotUsed2                        like(NotUsed)
023300100128     d  pr_QUSEC                           like(Qusec)
023400100128     d                                     options(*varsize)
023500100128
023600100128      //---------------------------------------------------------------
023700100128      //?Definizione prototipi.
023800100128      //---------------------------------------------------------------
023900100126
024000100126      /copy gaitrasrc/srcprotopr,tibs69r
024100100126      /copy gaitrasrc/srcprotopr,tibs34r
024200100127      /copy gaitrasrc/srcprotopr,ycodrufc
024300100126
024400100126      //---------------------------------------------------------------
024500100126      //?Definizione key-list.
024600100126      //---------------------------------------------------------------
024700100126
024800100126
024900100126      //---------------------------------------------------------------
025000100126      //?Riepilogo indicatori.
025100100126      //---------------------------------------------------------------
025200100126      // 28    : Emissione messaggio di errore a video
025300100126      // 30    : Condiziona SFLDSP
025400100126      // 31    : Condiziona SFLDSPCTL
025500100126      // 32    : Condiziona SFLEND
025600100126      // 99    : Generico di Errore
025700100126      //---------------------------------------------------------------
025800100126
025900100126      //---------------------------------------------------------------
026000100126      //?M A I N - L I N E
026100100126      //---------------------------------------------------------------
026200100126
026300100126     c     *Entry        plist
026400100126     c                   parm                    KPJBA
026500100126     c                   parm                    TNTA40ds
026600100126
026700100126      /free
026800100126
026900100126       //?Operazioni iniziali
027000100126       exsr sr_RoutInz;
027100100126
027200100126       //?Gestione video
027300100126       DOW Not $Fine;
027400100126         select;
027500100126           when $Video = 'W1';
027600100126             exsr GesW01;
027700100126           other;
027800100126             $Fine = *on;
027900100126         endsl;
028000100126       ENDDO;
028100100126
028200100126       //?Operazioni finali
028300100126       exsr sr_RoutEnd;
028400100126
028500100126       //--------------------------------------------------------------
028600100126       //?Operazioni iniziali.
028700100126       //--------------------------------------------------------------
028800100126       BEGSR sr_RoutInz;
028900100126
029000100126         //?Reperimento dati job
029100100126         exsr DatiJob;
029200100127
029300100127         //?Se tipo lancio sbagliato imposto fisso blank
029400100127         IF  ITA40tla <> 'C' and ITA40tla <> 'L' and
029500100127             ITA40tla <> *blanks;
029600100127           clear ITA40tla;
029700100127         ENDIF;
029800100127
029900100127         //?Se tipo lancio 'C' vado subito a fine e chiudo
030000100127         //?solo i file
030100100127         IF  ITA40tla = 'C';
030200100127           $Fine = *on;
030300100127         ENDIF;
030400100127
030500100127         //?Se tipo elaborazione sbagliato imposto blank
030600100128         IF  ITA40fle <> 'A' and ITA40fle <> *blanks;
030700100127           clear ITA40fle;
030800100127         ENDIF;
030900100127
031000100126         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
031100100126
031200100126       ENDSR;
031300100126
031400100126       //--------------------------------------------------------------
031500100126       //?Reperimento Dati del job (Utente/Operativi).
031600100126       //--------------------------------------------------------------
031700100126       BEGSR DatiJob;
031800100126
031900100126         in(E) §AzUte;
032000100126         if NOT %error;
032100100126           in(E) §DatiUte;
032200100126         endif;
032300100126         if %error or RSut = *blanks;
032400100126           clear TIBS34ds;
032500100126           tibs34r(tibs34ds);
032600100126           in §AzUte;
032700100126           in §DatiUte;
032800100126         endif;
032900100126
033000100126       ENDSR;
033100100126
033200100126       //--------------------------------------------------------------
033300100127       //?Gestione videata W01.
033400100127       //--------------------------------------------------------------
033500100127       BEGSR GesW01;
033600100127
033700100127       //?Inizializzazione videata
033800100127         if $InzW01;
033900100127           exsr InzW01;
034000100127           $InzW01 = *off;
034100100127         endif;
034200100127
034300100127         $Eof   = *off;
034400100127
034500100127       //?Emissione videata
034600100127         SELECT;
034700100128         WHEN  ITA40fle = 'A' and s01nrr <= 1;
034800100127           $Fine = *on;
034900100127           leavesr ;
035000100127
035100100127         WHEN  s01nrr = 0;
035200100127           exfmt TA40V01;
035300100127
035400100127         WHEN  s01nrr > 0;
035500100128         SflDsp_n = *off;
035600100128         SflEnd = *on;
035700100128         WC1rcd = 1;
035800100127         write TA40Z01;
035900100127         exfmt TA40C01;
036000100127         ErrMessage   = *off;
036100100127         ErrGenerico  = *off;
036200100127         clear WC1msg;
036300100127
036400100127         ENDSL;
036500100127
036600100127         SELECT;
036700100127
036800100127       //?F12 = Ritorno
036900100127           WHEN dsp_aid = c_F12;
037000100127             exsr sr_F12W01;
037100100127             leavesr ;
037200100127
037300100127       //?Enter
037400100127           OTHER;
037500100127       //?Controllo i dati immessi nel subfile
037600110427             IF  s01nrr > 0;
037700100127             exsr CtrW01;
037800100127             IF  ErrGenerico;
037900100127               leavesr;
038000100127             ENDIF;
038100110427             ENDIF;
038200100127
038300100127         ENDSL;
038400100127
038500100127       ENDSR;
038600100127
038700100127       //--------------------------------------------------------------
038800100127       //?Caricamento videata W01.
038900100127       //--------------------------------------------------------------
039000100127       BEGSR Inzw01;
039100100127
039200100127         //?Pulizia subfile
039300100127         exsr Sr_PulSfl;
039400100127
039500100127         //?Carico subfile
039600100127         exsr sr_CarW01;
039700100127
039800100127       ENDSR;
039900100127
040000100127       //--------------------------------------------------------------
040100100127       //?Pulisco subfile.
040200100127       //--------------------------------------------------------------
040300100127       BEGSR sr_PulSfl;
040400100127
040500100127         SflDsp_N    = *on;
040600100127         SflDspCtl_N = *on;
040700100127         write  TA40C01;
040800100127         SflDspCtl_N = *off;
040900100127         SflEnd      = *off;
041000100127
041100100127         clear WC1rcd;
041200100127         clear WC1msg;
041300100127         ErrMessage  = *off;
041400100127         ErrGenerico = *off;
041500100127         clear S01Nrr;
041600100127
041700100127       ENDSR;
041800100127
041900100127       //--------------------------------------------------------------
042000100127       //?Carico il subfile.
042100100127       //--------------------------------------------------------------
042200100127       BEGSR sr_CarW01;
042300100127
042400100127         //?Pulisco sk di comodo
042500100127         clear $CLI;
042600100127
042700100127         //?Recupero i codici fatturazione con p.Iva = a quella passata
042800100127         exec sql
042900100127          declare CLI cursor for
043000170118          select clpscf
043100100127          from CNIND02L join CNCLP00F on
043200100127           indkut = clpkut and indkcc = clpkcc and
043300100127           indksc = clpksc
043400100128          where indiva = :ITA40iva and indiva <> '' and
043500100128           clpscf > 0
043600100127          order by indkut, indkcc, indiva;
043700100127
043800100127         exec sql
043900100127          open CLI;
044000100127         IF  sqlcode < 0;
044100100127           $EoF = *on;
044200100127         ENDIF;
044300100127
044400100127         DOW  not $EoF;
044500100127           exec sql
044600170118            fetch next from CLI into :clpscf;
044700100127           IF  sqlcod = 100 or sqlcod < 0;
044800100127             $EoF = *on;
044900100127             leave;
045000100127           ENDIF;
045100100127
045200100127         //?Incremento la schiera di comodo
045300100127           xx = %lookup(clpscf : $CLI);
045400100127           IF  xx = 0;
045500100127             xx = %lookup(*zeros : $CLI);
045600100127             IF  xx > 0;
045700100127               $CLI(xx) = clpscf;
045800100127             ENDIF;
045900100127           ENDIF;
046000100127
046100100127         ENDDO;
046200100127
046300100127         exec sql close CLI;
046400170118
046500170118       //?Dalla schiera dei clienti caricati devo eliminare
046600170118       //?quelli che sono in sofferenza
046700170118         xx = 1;
046800170118         FOR  xx by 1 to %elem($CLI);
046900170118           IF  $CLI(xx) = 0;
047000170118             iter;
047100170118           ENDIF;
047200170118           clear tibs69ds;
047300170118           I69kcp = $CLI(xx);
047400170118           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
047500170118           IF  CLPcon <> *blanks;
047600170118             clear ds4W;
047700170118             TBLkut = 1;
047800170118             TBLcod = '4W';
047900170118             TBLkey = CLPcon;
048000170118             chain (TBLkut:TBLcod:TBLkey) TABEL00F;
048100170118             IF  %found(TABEL00F) and TBLflg = *blanks;
048200170118               ds4W = TBLuni;
048300170118             ENDIF;
048400170118           //?Cliente in sofferenza elimino dalla schiera
048500170118             IF  §4Wtip <> *blanks;
048600170118               clear $CLI(xx);
048700170118             ENDIF;
048800170118           ENDIF;
048900170118         ENDFOR;
049000100127
049100100128         //?Se non ho caricato la schiera di comodo
049200100128         //?vuol dire che non ho trovato altri clienti codificati
049300100128         IF  $CLI(1) = 0;
049400100128         //?Se è stata richiesta la chiamata al buio ritorno
049500100128         //?il ksc passato al programma chiamante
049600100128           IF  ITA40fle = '1';
049700100128             OTA40scf = ITA40ksc;
049800100128             leavesr;
049900100128           ENDIF;
050000100128         //?Se è richiesta la visualizzazione dei codici
050100100128         //?carico il subfile con solo il ksc passato
050200100128         //?se > 0
050300100128           IF  ITA40ksc > 0;
050400100128             clear TA40S01;
050500100129             WS1opz = '1';
050600100128             WS1ksc = ITA40ksc;
050700100128             WS1rag = ITA40rag;
050800100128             S01Nrr += 1;
050900100128             write TA40S01;
051000100128           ENDIF;
051100100128           leavesr;
051200100128         ENDIF;
051300100127
051400100127         //?Se ho caricato solo un codice nella schiera di comodo vuol dire che
051500100128         //?vuol dire che c'è solo questo cliente con questa p.IVA
051600100128         IF  $CLI(2) = 0;
051700100128         //?Se è stata richiesta la chiamata al buio ritorno
051800100128         //?il codice fatturazione trovato al programma chiamante
051900100128           IF  ITA40fle = '1';
052000100128             OTA40scf = $CLI(1);
052100100128             leavesr;
052200100128           ENDIF;
052300100128         //?Se è richiesta la visualizzazione dei codici
052400100128         //?carico il subfile con solo il ksc passato
052500100128           clear TA40S01;
052600100129           WS1opz = '1';
052700100128           WS1ksc = $CLI(1);
052800100128           //?Decodifico cliente
052900100128           exsr dec_CLI;
053000100128           WS1rag   = acorag;
053100100128           //?Data ultima registrazione fattura
053200100128           exsr data_REG;
053300100128           data_eur = drufc;
053400100128             IF  data_eur <> *loval;
053500100128               WH1data = %dec(drufc);
053600100128               WS1data = %dec(data_eur);
053700100128             ENDIF;
053800100128           S01Nrr += 1;
053900100128           write TA40S01;
054000100128           leavesr;
054100100128         ENDIF;
054200100127
054300100128         //?Se arrivo qua vuol dire che ho caricato più di un codice
054400100127         //?Ordino la schiera di comodo
054500100127         sorta $CLI;
054600100127
054700100127         //?Carico il subfile con i dati della sk di comodo
054800100127         //?ma solo se richiesto dal pgm chiamante
054900100127         IF  ITA40fle = *blanks;
055000100127           xx = 1;
055100100127           FOR  xx by 1 to %elem($CLI);
055200100127             IF  $CLI(xx) = 0;
055300100127               iter;
055400100127             ENDIF;
055500100127
055600100127             clear TA40S01;
055700100127
055800100127             clear WS1opz;
055900100127             WS1ksc   = $cli(xx);
056000100127
056100100127           //?Decodifico cliente
056200100127             exsr dec_CLI;
056300100127             WS1rag   = acorag;
056400100127
056500100127           //?Data ultima registrazione fattura
056600100128             exsr data_REG;
056700100128             data_eur = drufc;
056800100128           //?se data = al valore minimo imposto 0
056900100128             IF  data_eur <> *loval;
057000100128               WH1data = %dec(drufc);
057100100128               WS1data = %dec(data_eur);
057200100128             ENDIF;
057300100127
057400100127             S01Nrr += 1;
057500100127             write TA40S01;
057600100127           ENDFOR;
057700100128
057800100128         //?Ordino il subfile per data registrazione fattura
057900100128           rrnlast = s01nrr;
058000100128           exsr sr_ordina;
058100100127
058200100127         ENDIF;
058300100127
058400100127       ENDSR;
058500100127
058600100127       //--------------------------------------------------------------
058700100127       //?Controllo videata W01.
058800100127       //--------------------------------------------------------------
058900100127       BEGSR CtrW01;
059000100127
059100100127         WindDspF = IndDspF;
059200100127         reset WindDspFb;
059300100127         IndDspF  = WindDspF;
059400100127
059500100127         clear S01Nrr;
059600100127
059700100127         DOW  not $EoF;
059800100127           S01Nrr += 1;
059900100127           chain S01Nrr TA40S01;
060000100127           IF  not %found;
060100100127             $EoF = *on;
060200100127             leave;
060300100127           ENDIF;
060400100127
060500100127         //?Opzione '1' ritorno il codice scelto
060600100127           IF  WS1opz = '1';
060700100128             OTA40scf = WS1ksc;
060800100127             $Fine = *on;
060900100127             $EoF = *on;
061000100127             leave;
061100100127           ENDIF;
061200100127
061300100127           WC1Rcd = S01Nrr;
061400100127           update TA40S01;
061500100127         ENDDO;
061600100127
061700100127       ENDSR;
061800100127
061900100127       //--------------------------------------------------------------
062000100127       //?Gestione tasto funzionale F12 da videata W01.
062100100127       //?F12=Ritorno
062200100127       //--------------------------------------------------------------
062300100127       BEGSR sr_F12W01;
062400100127
062500100127         // Ritorno al pgm richiamante
062600100128         OTA40scf = ITA40scf;
062700100127         OTA40f12 = 'S';
062800100127         $Fine    = *on;
062900100127
063000100127       ENDSR;
063100100127
063200100127       //--------------------------------------------------------------
063300100127       //?Decodifico il cliente.
063400100127       //--------------------------------------------------------------
063500100127       BEGSR dec_CLI;
063600100127
063700100127         clear tibs69ds;
063800100127         I69kac = WS1KSC;
063900100127         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
064000100127
064100100127       ENDSR;
064200100127
064300100127       //--------------------------------------------------------------
064400100127       //?Recupero data ultima registrazione fattura.
064500100127       //--------------------------------------------------------------
064600100127       BEGSR data_REG;
064700100128
064800100128         csc = WS1ksc;
064900100127
065000100127         DataRegistrazioneUltimaFattura ('GETDRUFC' : esito
065100100128                                                    : csc
065200100127                                                    : drufc);
065300100127
065400100127       ENDSR;
065500100128
065600100128       //--------------------------------------------------------------
065700100128       //?Ordinamento subfile.
065800100128       //--------------------------------------------------------------
065900100128       BEGSR sr_ordina;
066000100128
066100100128       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
066200100128       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
066300100128       // il campo è usato per selezionare i records dando un ordine a quelli
066400100128       // selezionati davanti ai non selezionati.
066500100128         clear QLGSCB;
066600100128         clear QLGSCB00;
066700100128
066800100128       // 2 campi chiave x Data + cliente
066900100128         QLGNBRK = 2;
067000100128
067100100128       // imposto la posizione della Data     sul subfile e la lunghezza
067200100128       // l'ordinamento è su un campo numerico e deve essere
067300100128       // ascending
067400100128         QLGSP = 1;
067500100128         QLGSS = %SIZE(wh1data);
067600100128         QLGDT = Numerico;
067700100128         QLGSO = Discendente;
067800100128         QLGKL(1) = QLGSKL;
067900100128
068000100128       // imposto la posizione del Cliente    sul subfile e la lunghezza
068100100128       // l'ordinamento è su un campo numerico e deve essere
068200100128       // ascending
068300100128         QLGSP = 1 + %size(ws1opz);
068400100128         QLGSS = %SIZE(ws1ksc);
068500100128         QLGDT = Numerico;
068600100128         QLGSO = Ascendente;
068700100128         QLGKL(2) = QLGSKL;
068800100128
068900100128       // Load other sort parameters.
069000100128         QLGLB = 80 + 16 * MaxKey;
069100100128         QLGRL = %SIZE(sflrcd) - 1;
069200100128         QLGRT = 8;
069300100128         QLGOKL = 80;
069400100128         QLGLKE = 16;
069500100128         QLGLSS = 290;
069600100128
069700100128       // Initialize Sort I/O API fields.
069800100128         QLGRL00 = QLGRL;
069900100128         QLGRC00 = 1;
070000100128         clear QUSEI;
070100100128         QUSBPRV = %SIZE(QUSEC);
070200100128
070300100128       // First step - Initialize the sort routine.
070400100128         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
070500100128
070600100128       // Next step - Write records to I/O routine.
070700100128         QLGRT00 = Put;
070800100128         for xx = 1 to rrnlast;
070900100128          chain xx TA40S01;
071000100128
071100100128       // solo le righe con Selected = 'Y' sono riordinate,
071200100128       // quindi per fare un ordinamento di tutte le righe
071300100128       // metto 'Y' sempre.
071400100128          selected  = 'Y';
071500100128          clear QUSEI;
071600100128          QUSBPRV = %SIZE(QUSEC);
071700100128          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
071800100128         endfor;
071900100128
072000100128       // Next step - Signal end of input, clear subfile for reload.
072100100128         QLGRT00 = EndPut;
072200100128         clear QUSEI;
072300100128         QUSBPRV = %SIZE(QUSEC);
072400100128         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
072500100128       // pulizia SFL
072600100128         exsr Sr_PulSfl;
072700100128
072800100128       // Final step - Write the records back to the subfile.
072900100128         QLGRT00 = Get;
073000100128         for xx = 1 to rrnlast;
073100100128          clear QUSEI;
073200100128          QUSBPRV = %SIZE(QUSEC);
073300100128          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
073400100128          s01nrr = xx;
073500100128          write TA40S01;
073600100128         endfor;
073700100128
073800100128       ENDSR;
073900090610
074000100127       //--------------------------------------------------------------
074100100127       //?Operazioni finali.
074200100127       //--------------------------------------------------------------
074300100127       BEGSR sr_RoutEnd;
074400100127
074500100127         //?Ritorno errore
074600100127         IF  ErrGenerico;
074700100127           OTA40err = 'E';
074800100127           OTA40msg = WC1msg;
074900100127         ENDIF;
075000100127
075100100127         //?Chiudo file quando richiamato per sola chiusura file
075200100127         IF  ITA40tla = 'C';
075300100127           DataRegistrazioneUltimaFattura ('SETONLRC' : esito);
075400100127         ENDIF;
075500100127
075600100128         *inLR = *on;
075700100127         return;
075800100127
075900100127       ENDSR;
076000100127
076100100127      /end-free
076200100127
076300100127       //--------------------------------------------------------------
076400100127       //?Schiere a tempo di compilazione.
076500100127       //--------------------------------------------------------------
076600100127
076700090610** - $MSG -------------------------------------------------------------------*
076800100127Partiva IVA non passata                                                       01
076900100127Cliente non passato                                                           02
