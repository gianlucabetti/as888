000100080206      //--------------------------------------------------------------
000200110411      //?FIOR58R - INTERROGAZIONE ORM x AUTOTRASPORTATORE
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090601     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100100624
001200110411      // - File anagrafiche Autotrasportatori
001300110411     fFIAPD01L  if   e           k disk
001400110412
001500110412      // - File bolle
001600110412     fFIAR404L  if   e           k disk
001700110511
001800110511      // - File ORM
001900110511     fFNORG50J  if   e           k disk
002000110411
002100110412      // - Video Interrogazione ORM
002200110411     fFIOR58D   cf   e             workstn
002300110411     f                                     sfile(OR58S01 : S01nrr)
002400080208     f                                     indds(IndDspF)
002500080206     f                                     infds(InfDspF)
002600080206
002700080206      //---------------------------------------------------------------
002800090406      //?Definizione costanti.
002900080206      //---------------------------------------------------------------
003000080207
003100080207      // - Tasti funzionali a video
003200080207     d c_F01           c                   const(x'31')
003300080207     d c_F02           c                   const(x'32')
003400080207     d c_F03           c                   const(x'33')
003500090406     d c_F04           c                   const(x'34')
003600080207     d c_F05           c                   const(x'35')
003700080207     d c_F06           c                   const(x'36')
003800080207     d c_F07           c                   const(x'37')
003900080207     d c_F08           c                   const(x'38')
004000080207     d c_F09           c                   const(x'39')
004100080207     d c_F10           c                   const(x'3A')
004200090303     d c_F11           c                   const(x'3B')
004300080207     d c_F12           c                   const(x'3C')
004400080207     d c_F13           c                   const(x'B1')
004500080207     d c_F14           c                   const(x'B2')
004600080207     d c_F15           c                   const(x'B3')
004700080207     d c_F16           c                   const(x'B4')
004800080207     d c_F17           c                   const(x'B5')
004900080207     d c_F18           c                   const(x'B6')
005000080207     d c_F19           c                   const(x'B7')
005100080207     d c_F20           c                   const(x'B8')
005200080207     d c_F21           c                   const(x'B9')
005300080207     d c_F22           c                   const(x'BA')
005400080207     d c_F23           c                   const(x'BB')
005500080207     d c_F24           c                   const(x'BC')
005600080207     d c_Enter         c                   const(x'F1')
005700080207     d c_RollDown      c                   const(x'F4')
005800080207     d c_RollUp        c                   const(x'F5')
005900100211
006000100211     d Digits          c                   const('0123456789')
006100080206
006200080206      //---------------------------------------------------------------
006300080206      //?Definizione schiere.
006400080206      //---------------------------------------------------------------
006500080206
006600080206      // - Messaggi di errore
006700100408     d $Msg            s             78    dim(20) ctdata perrcd(1)
006800080206
006900080206      //---------------------------------------------------------------
007000080206      //?Definizione aree dati.
007100080206      //---------------------------------------------------------------
007200080206
007300080206      // - Dati utente
007400080206     d §AzUte        e ds                  extname(AZUTE00F)
007500080206     d                                     dtaara
007600080206     d §DatiUte      e ds                  extname(dDatiUte)
007700080206     d                                     dtaara
007800080206
007900080206      //---------------------------------------------------------------
008000080206      //?Definizione strutture dati.
008100080206      //---------------------------------------------------------------
008200080206
008300080206      // - Status
008400080206     d Psds           sds
008500080206     d   SDSpgm          *proc
008600080206
008700080206      // - InfDS
008800080206     d InfDspF         ds
008900080207     d  dsp_aid              369    369a
009000080207     d  sfl_rrn              376    377i 0
009100080207     d  min_nrr              378    379i 0
009200080207     d  num_rcds             380    381i 0
009300080206
009400080206      // - Indicatori su DspF
009500080208     d IndDspF         ds
009600100720        // - Indicatori di errore in videata
009700100720     d  ErrMessage                    1n   overlay(IndDspF : 28)
009800080206        // - Indicatori di gestione del subfile
009900080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010000080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010100080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010200080206     d  SflEnd                        1n   overlay(IndDspF : 33)
010300100624        // - Indicatori di errore
010400100624     d  PosCurOpz                     1n   overlay(IndDspF : 50)
010500090422        // - Indicatori di errore generico
010600100719     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010700090407
010800090407     d WindDspF        ds                  inz
010900090407     d   WindDspFa             1     49    inz(*zero)
011000090407     d   WindDspFb            50     99    inz(*zero)
011100110411
011200110411      // - Parametri per FIOR07R
011300110411     d FIOR07          ds                  inz
011400110411     d  OR07poe                       3  0
011500110411     d  OR07nor                       7  0
011600110412     d  OR07nsr                       2  0
011700110411     d  OR07nrv                       2  0
011800110411     d  OR07sce                       1
011900110411     d  OR07fgs                       3  0
012000080206
012100080206      // - Parametri ricevuti
012200080206     d KPJBA         e ds
012300110411     d FIOR58DS      e ds
012400080206
012500080206      // - Reperimento dati utente
012600080206     d TIBS34ds      e ds
012700100312     d TRUL31DS      e ds
012800100720     d POG                    10    759    DIM(250)
012900110412
013000110412      // - Aggiunge/Toglie gg/mm dalla data
013100110412     d XGIOLAVds     e ds                  inz
013200100428
013300080206      //---------------------------------------------------------------
013400080206      //?Definizione variabili globali.
013500080206      //---------------------------------------------------------------
013600080206
013700080206      // - Flags booleani
013800100720     d $EndS01         s               n   inz(*off)
013900100720     d $Fine           s               n   inz(*off)
014000100720     d $InzS01         s               n   inz(*on)
014100080208     d $RecOK          s               n   inz(*off)
014200080206
014300080206      // - Indici di schiera
014400100312     d xx              s              4  0 inz
014500080206
014600080207      // - Campi associati al video
014700100719     d $Video          s              2    inz('S1')
014800080208     d S01nrr          s              4  0 inz
014900110411
015000110411      // - Campi di comodo data
015100110411     d wDate_EUR       s               d   datfmt(*eur)
015200110411     d wDate_ISO       s               d   datfmt(*iso)
015300090401
015400080206      // - Campi di comodo
015500110412     d wKeyAr4         s             14    inz
015600110412     d wgg             s              3  0 inz(4)
015700111108     d wOggi           s              8  0 inz
015800110411     d wOggi_4         s              8  0 inz
015900100720     d wPag            s              4  0 inz
016000100720     d wRig            s              3  0 inz
016100100720     d w_SflPag        s              3  0 inz
016200110511
016300110511      //---------------------------------------------------------------
016400110511      //?Ordinamento SBFL
016500110511      //---------------------------------------------------------------
016600110511      // ----------------------------------------------------------------------
016700110511      //?Costanti per ordinamento subfile
016800110511      // ----------------------------------------------------------------------
016900110511     d MaxKey          c                   5
017000110511     d Ascendente      c                   1
017100110511     d Discendente     c                   2
017200110511     d Carattere       c                   6
017300110511     d Numerico        c                   8
017400110511     d Put             c                   1
017500110511     d EndPut          c                   2
017600110511     d Get             c                   3
017700110511      **********************************************************************
017800110511      * Campi utili:
017900110511      *     nrr        - Numero relativo di record del Subfile
018000110511      *     SizeList   - Dimensione della lista
018100110511      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
018200110511      **********************************************************************
018300110511     d NotUsed         s             16A
018400110511     d ReturnSize      s              9B 0
018500110511     d SizeList        s              9B 0
018600110511     d RrnLast         s              5I 0
018700110511     d WrkSort         s              1  0 inz(0)
018800110511      **********************************************************************
018900110511      * Data Structures
019000110511      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
019100110511      *     QLGSCB     - The sort request block for the QLGSORT API
019200110511      *     QLGSCB00   - The sort request block for the QLGSRTIO API
019300110511      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
019400110511      *     QUSEC      - Error structure for the QLGSORT API
019500110511      **********************************************************************
019600110511     d sflrcd          ds
019700110511     d  VS1rsr
019800110511     d  VS1poe
019900110511     d  VS1nsr
020000110511     d  VS1nor
020100110511     d  VS1nrv
020200110511     d  VS1fgs
020300110511     d  VS1opz
020400110511     d  VS1rsc
020500110511     d  VS1bol
020600110511     d  VS1tip
020700110511     d  VS1dar
020800110511     d  VS1rso
020900110511     d  selected                      1A
021000110511
021100110511      /COPY QSYSINC/QRPGLESRC,QLGSORT
021200110511     d QLGKL                         16    DIM(MaxKey)
021300110511     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021400110511     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021500110511     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021600110511     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021700110511
021800110511      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021900110511      /COPY QSYSINC/QRPGLESRC,QUSEC
022000110511
022100080208
022200090508      //---------------------------------------------------------------
022300090508      //?Definizione procedure usate.
022400090508      //---------------------------------------------------------------
022500100721
022600110411      // - Interrogazione ORM
022700110411     d fior07r         pr                  extpgm('FIOR07R')
022800100721     d  kpjba                              likeds(KPJBA)
022900110412
023000110412      // - Aggiunge/Toglie gg/mm dalla data
023100110412     d Xgiolav         pr                  extpgm('XGIOLAV')
023200110412     d  xgiolavds                          likeds(xgiolavds)
023300110511
023400110511     d Qlgsort_pr      pr                  extpgm('QLGSORT')
023500110511     d  pr_QLGSCB                          like(Qlgscb)
023600110511     d  pr_NotUsed1                        like(NotUsed)
023700110511     d  pr_NotUsed2                        like(NotUsed)
023800110511     d  pr_SizeList                        like(SizeList)
023900110511     d  pr_ReturnSize                      like(ReturnSize)
024000110511     d  pr_QUSEC                           like(Qusec)
024100110511     d                                     options(*varsize)
024200110511
024300110511     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
024400110511     d  pr_QLGSCB00                        like(QLGSCB00)
024500110511     d  pr_SflRcd                          like(SflRcd)
024600110511     d  pr_NotUsed1                        like(NotUsed)
024700110511     d  pr_SizeList                        like(SizeList)
024800110511     d  pr_NotUsed2                        like(NotUsed)
024900110511     d  pr_QUSEC                           like(Qusec)
025000110511     d                                     options(*varsize)
025100110511
025200110511     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
025300110511     d  pr_QLGSCB00                        like(QLGSCB00)
025400110511     d  pr_NotUsed1                        like(NotUsed)
025500110511     d  pr_SflRcd                          like(SflRcd)
025600110511     d  pr_SizeList                        like(SizeList)
025700110511     d  pr_NotUsed2                        like(NotUsed)
025800110511     d  pr_QUSEC                           like(Qusec)
025900110511     d                                     options(*varsize)
026000080206
026100080626      //---------------------------------------------------------------
026200080626      //?prototipi
026300080626      //---------------------------------------------------------------
026400080626      /copy gaitrasrc/srcprotopr,tibs34r
026500100624      /copy gaitrasrc/srcprotopr,trul31r
026600110511
026700110511      /copy gaitrasrc/srcprotopr,QsnQryModS
026800100624
026900080206      //---------------------------------------------------------------
027000080206      //?Definizione key-list.
027100080206      //---------------------------------------------------------------
027200100720
027300100720       // - File TABEL00F
027400100720     d k03tabel      e ds                  extname(TABEL00F:*key)
027500100720     d                                     prefix(k_)
027600080206
027700080206      //---------------------------------------------------------------
027800080206      //?Riepilogo indicatori.
027900080206      //---------------------------------------------------------------
028000080207      // 28    : Emissione messaggio di errore a video
028100080207      // 30    : Condiziona SFLDSP    (*not)
028200080207      // 31    : Condiziona SFLDSPCTL (*not)
028300080207      // 30+31 : Condiziona SFLCLR
028400100719      // 32    : Condiziona SFLNXTCHG
028500080207      // 33    : Condiziona SFLEND
028600100624      // 50    : Errore: Opzione
028700080207      // 99    : Generico di Errore
028800080206      //---------------------------------------------------------------
028900080206
029000080206      //---------------------------------------------------------------
029100080206      //?M A I N - L I N E
029200080206      //---------------------------------------------------------------
029300080206
029400080206     c     *Entry        plist
029500100720     c                   parm                    kpjba
029600110411     c                   parm                    fior58ds
029700080206
029800080206      /free
029900080206
030000100622       //?Operazioni iniziali
030100080206       exsr RoutInz;
030200080206
030300100622       //?Gestione video
030400080206       DOW $Fine = *off;
030500100624         SELECT;
030600100719           WHEN $Video = 'S1';
030700100719             exsr GesS01;
030800100624           OTHER;
030900080206             $Fine = *on;
031000100624         ENDSL;
031100080206       ENDDO;
031200080206
031300100622       //?Operazioni finali
031400080206       exsr RoutEnd;
031500080206
031600080206       //--------------------------------------------------------------
031700080206       //?Operazioni iniziali.
031800080206       //--------------------------------------------------------------
031900080206       BEGSR RoutInz;
032000100927
032100100927       //?Pulisco campi output
032200110411         clear OOR58poe;
032300110411         clear OOR58nsr;
032400110411         clear OOR58nor;
032500110411         clear OOR58nrv;
032600110411         clear OOR58f03;
032700110411         clear OOR58err;
032800110411         clear OOR58msg;
032900080206
033000100622       //?Impostazione campi "fissi"
033100110411         VT1pgm = SDSpgm;
033200080206
033300100622       //?Reperimento dati job
033400080206         exsr DatiJob;
033500110411
033600110412       //?Se non ho il codice AUT esco dal pgm con errore al chiamante
033700110412         IF  IOR58pdr = 0;
033800110411           OOR58err = '1';
033900110411           OOR58msg = $Msg(01);
034000110411           $Fine = *on;
034100110411         ENDIF;
034200110412
034300110412       //?Se non ho la filiale gestione la recupero dal codice AUT
034400110412         IF  IOR58fgs = 0;
034500110412           IOR58fgs = %dec(%subst(%editc(IOR58pdr:'X'):1:3):3:0);
034600110412         ENDIF;
034700111108
034800111108         //?Imposto oggi
034900111108         wOggi = %dec(%date());
035000110411
035100110412         //?Imposto oggi - 4 gg. lavorativi
035200110412         clear XGIOLAVDS;
035300110412         IXGLdata = %dec(%date());
035400110412         IXGLsub  = 'S';
035500110412         IXGLgg   = wgg;
035600110412         IXGLlav  = 'S';
035700110412         IXGLpa   = 'P';
035800110412         IXGLfil  = DUTpou;
035900110412         XGIOLAV (xgiolavds);
036000110412         IF  OXGLerr = *blanks;
036100110412           wOggi_4  = OXGLdata;
036200110412         ELSE;
036300110412           wOggi_4  = %dec(%date());
036400110412         ENDIF;
036500090714
036600080206       ENDSR;
036700080206
036800080206       //--------------------------------------------------------------
036900080206       //?Reperimento Dati del job (Utente/Operativi).
037000080206       //--------------------------------------------------------------
037100080206       BEGSR DatiJob;
037200080206
037300080206         in(E) §AzUte;
037400080206         if NOT %error;
037500080206           in(E) §DatiUte;
037600080206         endif;
037700080206         if %error or RSut = *blanks;
037800080206           clear TIBS34ds;
037900080206           tibs34r(tibs34ds);
038000080206           in §AzUte;
038100080206           in §DatiUte;
038200080206         endif;
038300080206
038400080206       ENDSR;
038500100622
038600100622       //--------------------------------------------------------------
038700100622       //?Gestione videata S01.
038800100622       //--------------------------------------------------------------
038900100622       BEGSR GesS01;
039000100622
039100100622       //?Inizializzazione videata
039200100622         IF  $InzS01;
039300100622           exsr InzS01;
039400100622           $InzS01  = *off;
039500100622         ENDIF;
039600110511
039700110511       //?Visualizzazione del SFL (se ci sono dati)
039800110511         SflDsp_N = (S01nrr <= *zeros);
039900110511
040000110511       //?Posizionamento cursore al 1° rcd della pagina
040100110511         IF  S01nrr > *zero;
040200110511           VC1rcd = 1;
040300110511         ELSE;
040400110511           clear VC1rcd;
040500110511         ENDIF;
040600110511
040700110511         VC1csr = VC1rcd;
040800110411
040900110411       //?Imposto AUT in testata  subfile
041000110411         VC1pdr = IOR58pdr;
041100110411         clear VC1dpdr;
041200110411         chain ('A' : VC1pdr) FIAPD01L;
041300110411         IF  %found(FIAPD01L);
041400110411           VC1dpdr = APDrsc;
041500110411         ENDIF;
041600100622
041700100622       //?Emissione Testata e Piede con tasti funzionali abilitati
041800110411         write  OR58T01;
041900110411         write  OR58P01;
042000100622
042100100622       //?Emissione videata
042200110411         exfmt  OR58C01;
042300100622         reset ErrMessage;
042400100622         reset ErrGenerico;
042500110411         clear VC1msg;
042600100622
042700100622         SELECT;
042800100719
042900100719       //?- F03=Fine
043000100719           WHEN  dsp_aid = c_F03;
043100100719             exsr F03S01;
043200100622
043300100622       //?Invio
043400100622           OTHER;
043500110412             IF  VC1csr > *zero;
043600110412               exsr OpzS01;
043700110412               IF  ErrGenerico;
043800110412                 leavesr;
043900110412               ENDIF;
044000110412             ENDIF;
044100100622
044200100622         ENDSL;
044300100622
044400100622       ENDSR;
044500100622
044600100622       //--------------------------------------------------------------
044700100622       //?Inizializzazione videata S01.
044800100622       //--------------------------------------------------------------
044900100622       BEGSR InzS01;
045000100622
045100100720         $EndS01 = *off;
045200100622
045300100622       //?Pulizia subfile
045400100622         exsr PulS01;
045500110411
045600110511       //?Caricamento subfile
045700110511         exsr Ries01;
045800110511
045900110511       //?Se ho caricato qualcosa ordino il subfile
046000110511         IF  S01nrr > 0;
046100110511           exsr Ordina_x_Mitt;
046200110511         ENDIF;
046300110505
046400110511       //?Alla fine carico una riga con l'ORM fittizio
046500110511       //?e attivo il SFLEND
046600110511         exsr Car_Orm_9;
046700110511         SflEnd = *on;
046800100622
046900100622       ENDSR;
047000100624
047100100624       //--------------------------------------------------------------
047200100624       //?Pulizia SFL01.
047300100624       //--------------------------------------------------------------
047400100624       BEGSR PulS01;
047500100624
047600100624       //?Pulizia subfile
047700100624         SflDsp_N    = *on;
047800100624         SflDspCtl_N = *on;
047900110411         write  OR58C01;
048000100624         SflDspCtl_N = *off;
048100100624         SflEnd      = *off;
048200100624
048300100624         clear W_SflPag;
048400100720         clear VC1rcd;
048500100720         clear VC1csr;
048600100624         clear S01nrr;
048700110411         clear VC1msg;
048800100720
048900100624         ErrMessage  = *off;
049000100624         ErrGenerico = *off;
049100100720
049200100624         WindDspF    = IndDspF;
049300100624         reset WindDspFb;
049400100624         IndDspF     = WindDspF;
049500100624
049600100624       ENDSR ;
049700100622
049800100622       //--------------------------------------------------------------
049900110511       //?Caricamento S01.
050000100622       //--------------------------------------------------------------
050100100622       BEGSR RieS01;
050200110511
050300110511       //?Mi posiziono sul file
050400110511         setll (Ior58fgs:IOR58pdr) FNORG50J;
050500110511         IF  not %equal(FNORG50J);
050600110511             $EndS01 = *on;
050700110511         ENDIF;
050800110511         DOW  not $EndS01;
050900110511           reade (IOR58fgs:IOR58pdr) FNORG50J;
051000110511           IF  %eof(FNORG50J);
051100110511             $EndS01 = *on;
051200110511             leavesr;
051300110511           ENDIF;
051400110511
051500110511         //?Controllo se OK il rcd
051600110511           exsr CTRrec;
051700110511           IF  not $RecOK;
051800110511             iter;
051900110511           ENDIF;
052000110511
052100110511           clear  OR58S01;
052200110511           PosCurOpz = *off;
052300110511
052400110511         //?Imposto i campi del subfile
052500110511           VS1fgs = ORGfgs;
052600110511           VS1rsr = ORMrsr;
052700110511           VS1rsc = ORMrsc;
052800110511           VS1poe = ORGpoe;
052900110511           VS1nsr = ORGnsr;
053000110511           VS1nor = ORGnor;
053100110511           VS1nrv = ORGnrv;
053200110511           wDate_ISO = %date(ORMdar:*iso);
053300110511           wDate_EUR = wDate_ISO;
053400110511           VS1dar    = %dec(wDate_EUR);
053500110511           VS1rso = ORMrso;
053600110511
053700110511           clear VS1bol;
053800110511           wKeyAr4 = %editc(ORGpoe:'X') + %editc(ORGnsr:'X') +
053900110511                     %editc(ORGnor:'X') + %editc(ORGnrv:'X');
054000110511           chain ('M' : wKeyAr4) FIAR404L;
054100110511           IF  %found(FIAR404L);
054200110511             VS1bol = 'S';
054300110511           ENDIF;
054400110511
054500110511           clear VS1tip;
054600110511           IF  ORGnsr > 0;
054700110511             VS1tip = 'RC';
054800110511           ENDIF;
054900110511
055000110511           S01nrr += 1;
055100110511           write  OR58S01;
055200110511
055300110511         ENDDO;
055400100622
055500100622       ENDSR;
055600110511
055700110511       //--------------------------------------------------------------
055800110511       //?Caricamento di ORM fittizio nel SBF.
055900110511       //--------------------------------------------------------------
056000110511       BEGSR Car_Orm_9;
056100110511
056200110511         clear  OR58S01;
056300110511         PosCurOpz = *off;
056400110511
056500110511       //?Imposto i campi del subfile
056600110511         VS1fgs = IOR58fgs;
056700110511         VS1rsr = '  ORM FITTIZIO';
056800110511         VS1poe = 999;
056900110511         VS1nsr = 99;
057000110511         VS1nor = 9999999;
057100110511         VS1nrv = 99;
057200110511         wDate_ISO = %date(IXGLdata:*iso);
057300110511         wDate_EUR = wDate_ISO;
057400110511         VS1dar    = %dec(wDate_EUR);
057500110511
057600110511         S01nrr += 1;
057700110511         write  OR58S01;
057800110511
057900110511       ENDSR;
058000110511
058100110511       //--------------------------------------------------------------
058200110511       //?Ordinamento S01.
058300110511       //--------------------------------------------------------------
058400110511       BEGSR Ordina_x_Mitt;
058500110511
058600110511       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
058700110511       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
058800110511       // il campo è usato per selezionare i records dando un ordine a quelli
058900110511       // selezionati davanti ai non selezionati.
059000110511         clear QLGSCB;
059100110511         clear QLGSCB00;
059200110511         rrnlast = S01nrr;
059300110511
059400110511       // 1 campi chiave x Mittente
059500110511         QLGNBRK = 1;
059600110511
059700110511       // imposto la posizione della Mittente sul subfile e la lunghezza
059800110511       // l'ordinamento è su un campo alfanumerico e deve essere ascending
059900110511         QLGSP = 1 ;
060000110511         QLGSS = %SIZE(VS1rsr);
060100110511         QLGDT = Carattere;
060200110511         QLGSO = Ascendente;
060300110511         QLGKL(1) = QLGSKL;
060400110511
060500110511       // Load other sort parameters.
060600110511         QLGLB = 80 + 16 * MaxKey;
060700110511         QLGRL = %SIZE(sflrcd) - 1;
060800110511         QLGRT = 8;
060900110511         QLGOKL = 80;
061000110511         QLGLKE = 16;
061100110511         QLGLSS = 290;
061200110511
061300110511       // Initialize Sort I/O API fields.
061400110511         QLGRL00 = QLGRL;
061500110511         QLGRC00 = 1;
061600110511         clear QUSEI;
061700110511         QUSBPRV = %SIZE(QUSEC);
061800110511
061900110511       // First step - Initialize the sort routine.
062000110511         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
062100110511
062200110511       // Next step - Write records to I/O routine.
062300110511         QLGRT00 = Put;
062400110511         for xx = 1 to rrnlast;
062500110511          chain xx OR58S01;
062600110511
062700110511       // solo le righe con Selected = 'Y' sono riordinate,
062800110511       // quindi per fare un ordinamento di tutte le righe
062900110511       // metto 'Y' sempre.
063000110511          selected  = 'Y';
063100110511          clear QUSEI;
063200110511          QUSBPRV = %SIZE(QUSEC);
063300110511          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
063400110511         endfor;
063500110511
063600110511       // Next step - Signal end of input, clear subfile for reload.
063700110511         QLGRT00 = EndPut;
063800110511         clear QUSEI;
063900110511         QUSBPRV = %SIZE(QUSEC);
064000110511         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
064100110511       // pulizia SFL
064200110511         exsr PulS01;
064300110511
064400110511       // Final step - Write the records back to the subfile.
064500110511         QLGRT00 = Get;
064600110511         for xx = 1 to rrnlast;
064700110511          clear QUSEI;
064800110511          QUSBPRV = %SIZE(QUSEC);
064900110511          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
065000110511          s01nrr = xx;
065100110511          write OR58S01;
065200110511         endfor;
065300110511
065400110511       ENDSR;
065500110511
065600110511       //--------------------------------------------------------------
065700110511       //?Controllo se RCD OK.
065800110511       //--------------------------------------------------------------
065900110511       BEGSR CTRrec;
066000110511
066100110511         $RecOK = *on;
066200111108
066300111108       //?Data Distinta
066400111108         IF  ORGddc > wOggi;
066500111108           $RecOK = *off;
066600111108           leavesr;
066700111108         ENDIF;
066800110511
066900110511       //?Data Ritiro
067000110511         IF  ORMdar < wOggi_4;
067100110511           $RecOK = *off;
067200110511           leavesr;
067300110511         ENDIF;
067400110511
067500110511       //?Fase
067600110511         IF  ORMfao = 410;
067700110511           $RecOK = *off;
067800110511           leavesr;
067900110511         ENDIF;
068000110511
068100110511       //?Tipo ORM
068200110511         IF  ORMtor = 'P';
068300110511           $RecOK = *off;
068400110511           leavesr;
068500110511         ENDIF;
068600110511
068700110511       ENDSR;
068800100720
068900100720       //--------------------------------------------------------------
069000100720       //?Gestione tasto funzionale F03 da videata S01
069100100720       //?F03=Fine
069200100720       //--------------------------------------------------------------
069300100720       BEGSR F03S01;
069400100720
069500100720         // Chiusura del programma
069600110411         OOR58F03 = '1';
069700100720         $Fine = *on;
069800100720
069900100720       ENDSR;
070000100622
070100100622       //--------------------------------------------------------------
070200100622       //?Gestione opzioni subfile
070300100622       //--------------------------------------------------------------
070400100622       BEGSR OpzS01;
070500100720
070600110411         readc OR58S01;
070700100622
070800110411         DOW  NOT  %eof(FIOR58D);
070900100622
071000100722           SflNxtChg = *off;
071100100622           WindDspF  = IndDspF;
071200100622           reset WindDspFb;
071300100622           IndDspF   = WindDspF;
071400100720           VC1rcd    = S01nrr;
071500110307
071600110307           PosCurOpz = *off;
071700100622
071800100622           SELECT;
071900100622
072000100624             //?- Nessuna opzione
072100100720             WHEN  VS1opz  = *blank;
072200100622
072300100624             //?- 1 = Scelta
072400100720             WHEN  VS1opz  = '1';
072500110411               //?passo i dati ORM al chiamante ed esco dal pgm
072600110411               OOR58poe = VS1poe;
072700110411               OOR58nsr = VS1nsr;
072800110411               OOR58nor = VS1nor;
072900110411               OOR58nrv = VS1nrv;
073000110411               $Fine    = *on;
073100110411               leavesr;
073200110411
073300110411             //?- 5 = Visualizza
073400110510             WHEN  VS1opz  = '5' and VS1poe <> 999;
073500110411               //?richiamo pgm dettaglio ORM fior07r
073600110411               exsr Call_FIOR07;
073700100622
073800100624             //?- ? = Opzione NON valida
073900100622             OTHER;
074000100624               ErrMessage  = *on;
074100100624               ErrGenerico = *on;
074200100622               PosCurOpz   = *on;
074300110411               VC1msg = $Msg(02);
074400110510               IF  VS1poe = 999;
074500110510                 VC1msg = %trim(VC1msg) + '. Non possibile per ORM FITTIZIO';
074600110510               ENDIF;
074700100622
074800100622           ENDSL;
074900100622
075000100624           //?Aggiornamento sfl
075100100624           SELECT;
075200100624             WHEN  ErrMessage;
075300100622               SflNxtChg = *on;
075400110510               VC1csr    = VC1rcd;
075500100624             WHEN  ErrGenerico;
075600100720               VC1csr    = VC1rcd;
075700100720               clear  VS1opz;
075800100624             OTHER;
075900100720               VC1csr    = VC1rcd;
076000100720               clear  VS1opz;
076100100624           ENDSL;
076200100622
076300110411           update OR58S01;
076400100622
076500100624           IF  ErrMessage or ErrGenerico;
076600100622             leave;
076700100624           ENDIF;
076800100622
076900110411           readc OR58S01;
077000100622
077100100622         ENDDO;
077200100622
077300100622       ENDSR;
077400100720
077500100720       //--------------------------------------------------------------
077600110411       //?Richiamo FIOR07R visualizza ORM.
077700100720       //--------------------------------------------------------------
077800110411       BEGSR Call_FIOR07;
077900100720
078000110411         clear FIOR07;
078100110412         OR07poe = VS1poe;
078200110412         OR07nsr = VS1nsr;
078300110412         OR07nor = VS1nor;
078400110412         OR07nrv = VS1nrv;
078500110412         OR07fgs = VS1fgs;
078600110412         kpjbu = fior07;
078700100721
078800110411         fior07r (kpjba);
078900100720
079000100720       ENDSR;
079100080206
079200080206       //--------------------------------------------------------------
079300080206       //?Operazioni finali.
079400080206       //--------------------------------------------------------------
079500080206       BEGSR RoutEnd;
079600080206
079700080206         *inLR = *on;
079800080206         return;
079900080206
080000080206       ENDSR;
080100080206
080200080206      /end-free
080300100624
080400080206       //--------------------------------------------------------------
080500080206       //?Schiere a tempo di compilazione.
080600080206       //--------------------------------------------------------------
080700080206
080800080206** - $MSG -------------------------------------------------------------------*
080900110411I dati necessari alla ricerca non sono stati passati                           1
081000110411Opzione errata                                                                 2
