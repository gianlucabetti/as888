000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300080627      //?TRMK50R - Gestione/interrogazione informazioni commerciali
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200080627     FTICPI01L  UF A E           K DISK
001300080702     fTNCPO01L  if   e           k disk    infds(cpo01)
001400081003     f                                     rename(tncpo000:tncpo001)
001500081003     fTNCPO00f  uf   e             disk
001600080206     fAZORG01L  if   e           k disk
001700080604     fTNTBE01l  if   e           k disk
001800081215     fTRMK50D   cf   e             workstn indds(IndDspF)
001900080206     f                                     infds(InfDspF)
002000080702     f                                     sfile(MK50S01 : S01nrr)
002100080206      //---------------------------------------------------------------
002200080207      // - Tasti funzionali a video
002300080207     d c_F01           c                   const(x'31')
002400080207     d c_F02           c                   const(x'32')
002500080207     d c_F03           c                   const(x'33')
002600080207     d c_F05           c                   const(x'35')
002700080207     d c_F06           c                   const(x'36')
002800080207     d c_F07           c                   const(x'37')
002900080207     d c_F08           c                   const(x'38')
003000080207     d c_F09           c                   const(x'39')
003100080207     d c_F10           c                   const(x'3A')
003200080207     d c_F12           c                   const(x'3C')
003300080207     d c_F13           c                   const(x'B1')
003400080207     d c_F14           c                   const(x'B2')
003500080207     d c_F15           c                   const(x'B3')
003600080207     d c_F16           c                   const(x'B4')
003700080207     d c_F17           c                   const(x'B5')
003800080207     d c_F18           c                   const(x'B6')
003900080207     d c_F19           c                   const(x'B7')
004000080207     d c_F20           c                   const(x'B8')
004100080207     d c_F21           c                   const(x'B9')
004200080207     d c_F22           c                   const(x'BA')
004300080207     d c_F23           c                   const(x'BB')
004400080207     d c_F24           c                   const(x'BC')
004500080207     d c_Enter         c                   const(x'F1')
004600080207     d c_RollDown      c                   const(x'F4')
004700080207     d c_RollUp        c                   const(x'F5')
004800080214
004900080214      // - Attributi di visualizzazione
005000080215      //  -  DspAtr() - Normale
005100080214     d C_dspatr_Norm   c                   const(x'20')
005200080215      //  -  DspAtr(RI)
005300080214     d C_dspatr_RI     c                   const(x'21')
005400080215      //  -  DspAtr(ND)
005500080214     d C_dspatr_ND     c                   const(x'27')
005600080215      //  -  DspAtr(BL) / Color(Red)
005700080214     d C_dspatr_BL     c                   const(x'28')
005800080206
005900080206      //---------------------------------------------------------------
006000080206      //?Definizione schiere.
006100080206      //---------------------------------------------------------------
006200080206
006300080206      // - Messaggi di errore
006400120105     d MSG             s             78    dim(35) ctdata perrcd(1)
006500080702     d ifo             s              3    dim(50)
006600081015     d ifods           s             70    dim(50)
006700080627     d ifocg           s              2    dim(50)
006800080702     d ifoor           s              8    dim(50)
006900081126
007000081126     d IFOval          s              8    dim(100)
007100080206      //---------------------------------------------------------------
007200080206      //?Definizione aree dati.
007300080206      //---------------------------------------------------------------
007400100226
007500080206      // - Dati utente
007600080206     d §AzUte        e ds                  extname(AZUTE00F)
007700080206     d                                     dtaara
007800080206     d §DatiUte      e ds                  extname(dDatiUte)
007900080206     d                                     dtaara
008000080206
008100080206      //---------------------------------------------------------------
008200080206      //?Definizione strutture dati.
008300080206      //---------------------------------------------------------------
008400080619     D CPO01           DS
008500080619     D  cp1NRR               397    400B 0
008600080206
008700080206      // - Status
008800080206     d Psds           sds
008900080206     d   SDSpgm          *proc
009000080206
009100080206      // - InfDS
009200080206     d InfDspF         ds
009300080207     d  dsp_aid              369    369a                                        AID byte
009400080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
009500080207     d  min_nrr              378    379i 0                                      Subfile min rrn
009600080207     d  num_rcds             380    381i 0                                      Subfile num rcds
009700080206
009800080206      // - Indicatori su DspF
009900080208     d IndDspF         ds
010000080206        // - Indicatori di gestione del subfile
010100080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
010200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
010300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
010500080206        // - Indicatori di errore
010600080206     d  errMessage                    1n   overlay(IndDspF : 28)
010700080606     d  errGenerico                   1n   overlay(IndDspF : 99)
010800080627        // - Indicatori vari
010900080627     d  InfoInt                       1n   overlay(IndDspF : 02)
011000080704     d  Infoyell                      1n   overlay(IndDspF : 03)
011100081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
011200120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
011300120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
011400080627     d  InfoImmiss                    1n   overlay(IndDspF : 10)
011500080703     d  ProtAnn                       1n   overlay(IndDspF : 13)
011600080703     d  Ifosott                       1n   overlay(IndDspF : 15)
011700080627     d  protriga                      1n   overlay(IndDspF : 16)
011800080627     d  protFat                       1n   overlay(IndDspF : 17)
011900081003     d  protVal                       1n   overlay(IndDspF : 18)
012000080627     d  protSped                      1n   overlay(IndDspF : 19)
012100080702     d  protfsn                       1n   overlay(IndDspF : 20)
012200080925     d  PFTBLU                        1n   overlay(IndDspF : 21)
012300080703     d  ErrSNA                        1n   overlay(IndDspF : 40)
012400080703     d  ErrFsn                        1n   overlay(IndDspF : 41)
012500081003     d  ErrVAL                        1n   overlay(IndDspF : 42)
012600080703     d  ErrPFT                        1n   overlay(IndDspF : 44)
012700080704     d  ErrANN                        1n   overlay(IndDspF : 45)
012800080703     d  PotAnnul                      1n   overlay(IndDspF : 70)
012900080206
013000080206      // - Parametri ricevuti
013100080206     d KPJBA         e ds
013200080702
013300080702     d TRMK50DS      e ds
013400100302     d TRMK52DS      e ds
013500080206
013600080206      // - Reperimento dati utente
013700080206     d TIBS34ds      e ds
013800080206     d dUte01        e ds
013900080702     d DLAT          e ds
014000080702
014100080702     d Dged          e ds
014200081006     d Difs          e ds
014300080206
014400080206      // - Ricerca/Controllo tabelle
014500080206     d TIBS02ds      e ds                  inz
014600081008      // - Ricerca sottipo info comm
014700081008     d TNTB70ds      e ds                  inz
014800080627
014900080206
015000080627      // - Tabella  Info commerciali e Categorie info
015100080627     d dIFO          e ds                  inz
015200080627     d dIFG          e ds                  inz
015300081003     d
015400081003     d dcpo01        e ds                  inz
015500080627     d
015600080206      //---------------------------------------------------------------
015700080206      //?Definizione variabili globali.
015800080206      //---------------------------------------------------------------
015900080206
016000080206      // - Flags booleani
016100080208     d $Fine           s               n   inz(*off)
016200080208     d $InzS01         s               n   inz(*on)
016300120105     d wAumenta10      s               n   inz(*off)
016400120510     d wAumenta10f     s               n   inz(*off)
016500120110     d wAzzera10       s               n   inz(*off)
016600120105     d wDiminuisci10   s               n   inz(*off)
016700120109     d wForzaAgg       s               n   inz(*off)
016800080206
016900080627     d $Video          s              2    inz('S1')
017000080208     d S01nrr          s              4  0 inz
017100080627     d savcat          s              2    inz
017200080627     d Wcat            s              2    inz
017300081006     d WIFS            s              4    inz
017400080702     d Wifo            s              3    inz
017500081006     d w0040           s              4  0 inz
017600081007     d w005a           s              5    inz
017700081006     d XX              s              3  0 inz
017800080627     d Indx            s              3  0 inz
017900080703     d Primavolta      s              1    inz
018000080925     d Totprc          s              4  0 inz
018100081003     d Wctrprc         s              1    inz
018200081003     d Wdescat         s             21    inz
018300081215     d SPTPFT          s                   like(cpipft) inz
018400100302     d savspt          s                   like(cpipft) inz
018500081006     d LOG40           s              1    inz
018600081006     d OUTFSN          s              1    inz
018700081006     d OUTnrr          s              4  0 inz
018800081023     d LOGFSN          s              1    inz
018900081023     d LOGnrr          s              4  0 inz
019000081126     d MOInrr          s              4  0 inz
019100081015     d Waggiocpo       s              1    inz
019200081007     d BART_10         s              4  0 inz
019300081015     d InfoITA         s              1    inz
019400081015     d InfoEST         s              1    inz
019500100226     d InfoATR         s                   like(cpival)
019600100302     d SavATR          s                   like(cpival)
019700081126     d w008a           s              8    inz
019800081126     d MOIconta        s              3  0 inz
019900081126     d yy              s              3  0 inz
020000081126     d trovacpi        s              1    inz
020100081215     d savmod          s                   like(i50mod) inz
020200090520     d ultdata         s              8  0 inz
020300120110     d sav_bar_10      s                   like(bart_10)
020400120109     d wOggi           s              8  0 inz
020500080414
020600080605     d Dataiso         s               d   datfmt(*iso)
020700080605     d Dataymd         s               d   datfmt(*ymd)
020800080605     d Datadmy         s               d   datfmt(*dmy)
020900120109     d wIFOdul         s              8  0
021000120109     d wIFOduliso      s               d   datfmt(*ISO)
021100080604     d
021200080605     d                 DS
021300080605     d  AA                     1      4  0
021400080605     d  MM                     5      6  0
021500080605     d  GG                     7      8  0
021600080605     d DATA                    1      8  0
021700081009     d
021800081126     d CoRicercaMOI    c                   const('Ricerca motivo ITA/EST''?''')
021900081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
022000080206
022100080206      //---------------------------------------------------------------
022200080206      //?Definizione procedure usate.
022300080206      //---------------------------------------------------------------
022400080414      /copy gaitrasrc/srcprotopr,tibs02r
022500080414      /copy gaitrasrc/srcprotopr,tibs34r
022600081008      /copy gaitrasrc/srcprotopr,TNTB70r
022700081023      /copy gaitrasrc/srcprotopr,TRMK51r
022800100302      /copy gaitrasrc/srcprotopr,TRMK52r
022900080414      /copy gaitrasrc/srcprotopr,trmk_opm
023000080206
023100080206      //---------------------------------------------------------------
023200080206      //?Riepilogo indicatori.
023300080206      //---------------------------------------------------------------
023400080207      // - Comuni
023500080207      // 28    : Emissione messaggio di errore a video
023600080207      // - C01/S01
023700080207      // 30    : Condiziona SFLDSP    (*not)
023800080207      // 31    : Condiziona SFLDSPCTL (*not)
023900080207      // 30+31 : Condiziona SFLCLR
024000080207      // 32    : Condiziona SFLNXTCHG in S01
024100080207      // 50    : Errore: Opzione errata
024200080207      // - D01
024300080207      // - Comuni
024400080207      // 99    : Generico di Errore
024500080206      //---------------------------------------------------------------
024600080206
024700080206      //---------------------------------------------------------------
024800080206      //?M A I N - L I N E
024900080206      //---------------------------------------------------------------
025000080206
025100080627     c     *Entry        plist
025200080206     c                   parm                    KPJBA
025300080627     C                   PARM                    trMK50ds
025400080627
025500080627     C                   CLEAR                   O50F12
025600080627     C                   CLEAR                   O50F03
025700080627     C                   CLEAR                   O50F06
025800080702
025900080206      /free
026000080206
026100081215 1      if  i50tla<>'C'   ;
026200080925
026300080206       // Operazioni iniziali
026400080206       exsr RoutInz;
026500080206
026600081215 2      if  i50mod<>'C'   ;
026700081215
026800080206       // Gestione video
026900081215 3     DOW $Fine = *off;
027000081215 4       select;
027100080530
027200080530         // Videata di selezioni
027300080627           when $Video = 'S1';
027400080627             exsr GesS01;
027500120109           when $Video = 'W1';
027600120109             exsr GesW01;
027700080530
027800080702           other   ;
027900080206             $Fine = *on;
028000081215 4       endsl;
028100081215 3     ENDDO;
028200081215
028300081215 x2    else   ;
028400081215
028500081215       // modalità CONTROLLO : carico il sfl
028600081215       //                    solo ai fini del controllo
028700081215       exsr InzS01;
028800081215       exsr Contrs01   ;
028900081215
029000081215  3    if ErrGenerico=*off;
029100081215          o50ifotot='S';
029200081215  3    endif  ;
029300080925
029400081215 2     endif    ;
029500081215 1     endif    ;
029600080206
029700080206       // Operazioni finali
029800080206       exsr RoutEnd;
029900080206
030000080206       //--------------------------------------------------------------
030100080206       //?Operazioni iniziali.
030200080206       //--------------------------------------------------------------
030300080206       BEGSR RoutInz;
030400080627       $inzs01=*on;
030500080929       clear vsctes2         ;
030600080206
030700080703       // Solo la prima volta
030800080703 1     if primavolta=' '   ;
030900080703
031000080206         // Reperimento dati job
031100080206         exsr DatiJob;
031200081003
031300081003         // Se sono EDP accendo indicatore
031400081003         if %subst(knmus:1:3)<>'EDP'    ;
031500081003         UTENonEDP=*on   ;
031600081003         endif      ;
031700080627
031800080627         // Carico tabella per divisa corrente
031900080627         reset TIBS02ds;
032000080627         T02sif = knsif;
032100080627         T02cod = 'GED';
032200080627         T02ke1 = '1'  ;
032300080627         T02mod = 'C'  ;
032400080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
032500080627
032600080703 2       if t02err=' '  ;
032700080627         dGED=t02uni    ;
032800080627         else   ;
032900080627         clear DGED    ;
033000080703 2       endif   ;
033100080627
033200080627         // Carico tabella info commerciali
033300080627         xx=0                     ;
033400080627         setll ('IFO') tntbe01l   ;
033500080627         READE ('IFO') tntbe01l   ;
033600080627
033700080703 2       dow not %eof(tntbe01l)   ;
033800080703 3       if tbeatb=' '    ;
033900080627           difo=tbeuni    ;
034000080627           xx=xx+1        ;
034100080703           ifo(xx)=tbeke1 ;
034200080627           ifods(xx)=difo ;
034300080627           ifocg(xx)=§ifoifg ;
034400080627           // ordino in base alla categoria e  num.ordinamento
034500080703           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
034600080703 3       endif               ;
034700080627
034800080627         READE ('IFO') tntbe01l   ;
034900080703 2       enddo    ;
035000080627
035100080627         sorta  IFOOR  ;
035200080703
035300080703         Primavolta='N'   ;
035400080703 1       endif      ;
035500080702
035600080703         Infoint=*off   ;
035700080704         Infoyell=*off   ;
035800080703         InfoImmiss=*off   ;
035900080703         Ifosott=*off   ;
036000080703         Protriga=*off   ;
036100080703         ProtFat =*off   ;
036200081003         ProtVal  =*off   ;
036300080703         Protsped =*off   ;
036400080703         Protfsn  =*off   ;
036500080703         PotAnnul =*off   ;
036600080703         clear wcat  ;
036700080703         clear wifo  ;
036800080704         $Fine=*off   ;
036900080703
037000080702         // Vedo se modo gestione  o interrogazione
037100080703 1       if i50mod='I' ;
037200080702         InfoInt =*on  ;
037300080703         vsctes2='INTERROGAZIONE'    ;
037400080703 1       endif;
037500080703
037600080703         if i50pgm=*blanks   ;
037700080703         vscpgm='TRMK50R'   ;
037800080703         else     ;
037900080703         vscpgm=i50pgm     ;
038000080703         endif   ;
038100080703
038200080703
038300080703         // IMPOSTO IL potenziale in visualizzazione
038400080703         vsccpo=%editc(i50cpo:'X')   ;
038500080703         vscrag=i50rag   ;
038600120111
038700120111         //?Attivo i tasti funzione F03 e F12
038800120111           F03Attivo = *on;
038900120111           F12Attivo = *on;
039000080703
039100120105         //?05/12/2012
039200120105         //?Visto che il campo per potenziale annullato non è più usato
039300120105         //?lo recuperiamo per utilizzarlo come flag per controlli da
039400120105         //?fare sulle info.
039500120110         //?Nessun controllo
039600120110           IF  I50atb = ' ';
039700120110             wAumenta10    = *off;
039800120510             wAumenta10f   = *off;
039900120110             wAzzera10     = *off;
040000120110             wDiminuisci10 = *off;
040100120110           ENDIF;
040200120110         //?Controllo per azzeramento
040300120110           IF  I50atb = '0';
040400120110             wAzzera10     = *on;
040500120110             wAumenta10    = *off;
040600120510             wAumenta10f   = *off;
040700120110             wDiminuisci10 = *off;
040800120111             F03Attivo = *off;
040900120111             F12Attivo = *off;
041000120110           ENDIF;
041100120110         //?Controllo per aumento
041200120105           IF  I50atb = '+';
041300120110             wAumenta10    = *on;
041400120510             wAumenta10f   = *off;
041500120110             wAzzera10     = *off;
041600120110             wDiminuisci10 = *off;
041700120111             F03Attivo = *off;
041800120111             F12Attivo = *off;
041900120105           ENDIF;
042000120510         //?Controllo per aumento semi obbligatorio
042100120510           IF  I50atb = '%';
042200120510             wAumenta10f   = *on;
042300120510             wAumenta10    = *off;
042400120510             wAzzera10     = *off;
042500120510             wDiminuisci10 = *off;
042600120510             F03Attivo = *off;
042700120510             F12Attivo = *off;
042800120510           ENDIF;
042900120110         //?Controllo per diminuzione
043000120109           IF  I50atb = '-';
043100120110             wDiminuisci10 = *on;
043200120110             wAumenta10    = *off;
043300120510             wAumenta10f   = *off;
043400120110             wAzzera10     = *off;
043500120111             F03Attivo = *off;
043600120111             F12Attivo = *off;
043700120105           ENDIF;
043800120105
043900081003         // Potenziale annullato ma non dovrebbero più esistere
044000080703         //  per ora è stato tolto l'annullamento
044100120105 1       // if i50atb<>' '  ;
044200120105         // PotAnnul =*oN    ;
044300120105         // InfoInt =*on  ;
044400120105 1       // endif           ;
044500081003
044600081003         // Se già  inserite tutte le info, non si possono più annulla
044700081003         //  ma solo sistemare
044800081003         if i50obl=' '  and i50ifotot='S'    ;
044900081003         i50obl='S'   ;
045000081003         endif        ;
045100080627
045200080703         // VERIFICO SE ci sono già dei dati per il potenziale
045300080703         setll   i50cpo  ticpi01l    ;
045400080703         reade   i50cpo  ticpi01l    ;
045500080703 1       dow not %eof(ticpi01l) and cpiatb<>' ' ;
045600080703         reade   i50cpo  ticpi01l    ;
045700080703 1       enddo   ;
045800080703
045900080703 1       if %eof(ticpi01l) ;
046000080703         Infoimmiss=*on     ;
046100080929 1       if vsctes2= *blanks    ;
046200080703         vsctes2='  IMMISSIONE  '    ;
046300080929         endif   ;
046400080703         else    ;
046500080929 1       if vsctes2= *blanks    ;
046600080703         vsctes2='  VARIAZIONE  '    ;
046700080703 1       endif    ;
046800080929 1       endif    ;
046900080703
047000080627         ENDSR;
047100080206
047200080206       //--------------------------------------------------------------
047300080206       //?Reperimento Dati del job (Utente/Operativi).
047400080206       //--------------------------------------------------------------
047500080206       BEGSR DatiJob;
047600080206
047700080206         in(E) §AzUte;
047800080206         if NOT %error;
047900080206           in(E) §DatiUte;
048000080206         endif;
048100080206         if %error or RSut = *blanks;
048200080206           clear TIBS34ds;
048300080206           tibs34r(tibs34ds);
048400080206           in §AzUte;
048500080206           in §DatiUte;
048600080206         endif;
048700080206
048800080206       ENDSR;
048900080206
049000080206       //--------------------------------------------------------------
049100080627       //?Gestione SFL 01
049200080206       //--------------------------------------------------------------
049300080409       BEGSR GesS01;
049400080207
049500080207         // Inizializzazione videata
049600080409         if  $InzS01 = *on;
049700080409            exsr InzS01;
049800080703         // Posizionamento cursore
049900100507         // C01csr = 1;
050000080703           s01nrr = 1;
050100080703           sfldsp_n=*off;
050200080409            $InzS01  = *off;
050300080207         endif;
050400080207
050500080207
050600080207         // Emissione Testata e Piede con tasti funzionali abilitati
050700080207         if  errGenerico = *off;
050800080702           write  MK50Z01;
050900080207         endif;
051000080703
051100080703         // Posizionamento cursore
051200080703         if  C01csr  > *zero;
051300080703           C01rcd = C01csr;
051400080703         endif  ;
051500080207
051600080207         // Emissione videata
051700080702         exfmt  MK50C01;
051800080410
051900080207         reset errMessage;
052000080207         reset errGenerico;
052100080702         clear Vscmsg;
052200081003         errVAL=*off     ;
052300080703         errPFT=*off     ;
052400080703         errSNA=*off     ;
052500080703         errFSN=*off     ;
052600080704         errANN=*off     ;
052700120109
052800120109         //?Cerco l'ultima data variazione info commerciali
052900120124         //?Se NON è interrogazione e non è immissione
053000120109           clear dCPO01;
053100120124           IF  not InfoInt and not Infoimmiss;
053200120109             chain(n) I50cpo TNCPO01L;
053300120109             IF  %found(TNCPO01L);
053400120109               dCPO01 = CPOrst;
053500120109             ENDIF;
053600120109             IF  §CPOifodul > *zeros;
053700120109               wIFOdul    = %dec(§CPOifodul:8:0);
053800120109               wIFOdulISO = %date(wIFOdul : *ISO);
053900120109               wOggi = %dec(%date());
054000120109               dataISO = %date(wOggi : *ISO);
054100120109         //?Se la data ultima variazione è più vecchia di 1 anno da oggi windows
054200120109         //?di avviso all'utente
054300120110               IF  %diff(dataISO : wIFOdulISO : *months) > 12 and not wForzaAgg;
054400120109                 $Video = 'W1';
054500120109                 ErrGenerico = *on;
054600120109                 ErrMessage  = *on;
054700120109                 VSCmsg      = msg(32);
054800120109                 wForzaAgg   = *on;
054900120109                 leavesr;
055000120109               ENDIF;
055100120109             ENDIF;
055200120109           ENDIF;
055300080207
055400080523 1       SELECT;
055500080207
055600080207         // - F3=Fine
055700080523 1         WHEN  dsp_aid = c_F03;
055800080409            $Fine = *on;
055900081006            o50f03='S'   ;
056000080207
056100080207         // - F12=Ritorno
056200080523 1         WHEN  dsp_aid = c_F12;
056300080702            $Fine = *on;
056400081006            o50f12='S'   ;
056500080414
056600080530 x1      // Invio / F6
056700080207           OTHER;
056800081006
056900081006        // Non effettuo controlli in interrogazione
057000081006 1           if i50mod<>'I' ;
057100081006               exsr ContrS01 ;
057200081006 2             if  errGenerico = *on;
057300081006                 leavesr;
057400081006 2             endif;
057500081006 2           endif;
057600080530
057700081023         // F6=conferma Aggiornamento  F10=anche stampa
057800081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
057900080702           exsr   ConfAggio ;
058000081006           o50f06='S'   ;
058100081023
058200081023              if dsp_aid=c_F10;
058300081023               kpjbu=trmk50ds  ;
058400081023               callp TRMK51R (kpjba)   ;
058500081023              endif    ;
058600081023
058700080704           $Fine = *on;
058800080530           endif  ;
058900080207
059000080523 1       ENDSL;
059100080207
059200080207       ENDSR;
059300120109
059400120109       //--------------------------------------------------------------
059500120109       //?Gestione Window errori
059600120109       //--------------------------------------------------------------
059700120109       BEGSR GesW01;
059800120109
059900120109         reset ErrMessage;
060000120109         reset ErrGenerico;
060100120109
060200120109         w1riga = vscmsg;
060300120109         exfmt  MK50W01;
060400120109
060500120109         clear Vscmsg;
060600120109         errVAL  = *off;
060700120109         errPFT  = *off;
060800120109         errSNA  = *off;
060900120109         errFSN  = *off;
061000120109         errANN  = *off;
061100120109
061200120109         $Video = 'S1';
061300120109
061400120109       ENDSR;
061500080207
061600080526       //--------------------------------------------------------------
061700080702       //?controlli SFL01
061800080409       //--------------------------------------------------------------
061900080409       BEGSR ContrS01;
062000081215       clear o50ifomin;
062100081215       clear wctrprc  ;
062200081006       clear wdescat   ;
062300081006       clear totprc   ;
062400081006       clear sptpft   ;
062500081006       clear OUTFSN   ;
062600081006       clear OUTnrr   ;
062700081023       clear LOGFSN   ;
062800081023       clear LOGnrr   ;
062900081006       clear LOG40    ;
063000081007       clear bart_10  ;
063100081015       clear infoITA  ;
063200081015       clear infoEST  ;
063300081126       clear IFOval   ;
063400081126       clear MOIconta ;
063500081126       clear MOInrr   ;
063600100226       clear InfoATR  ;
063700081126       yy=1           ;
063800080703       s01nrr=1  ;
063900080703       chain s01nrr    mk50s01    ;
064000080703
064100081006 0     dow %found    ;
064200080925       // Escludo i record di descrizione categoria
064300081006 1     if vshimm= 'C'    ;
064400081003
064500081003       // A cambio di categoria, se c'era controllo obbligatorio del
064600081003       //  100%   controllo ed eventualmente segnalo errore
064700081006       EXSR CambioCAT    ;
064800081006       if errGenerico=*on  ;
064900081006       leavesr;
065000081006       endif              ;
065100081006
065200081006x1     else    ;
065300081006
065400080925       pftblu=*off       ;
065500080925
065600081003       // se valori calcolati da pgm li pulisco prima dei controlli
065700081007 2     if vshsnf=' ' and vshprpft=' '   ;
065800080925       clear vscpft   ;
065900080925       clear vscvft   ;
066000081007 2     endif          ;
066100080703
066200080703       // Imposto la divisa, se impostato il fatturato
066300081007 2     if vscpft>0 and vscvft=*blanks   ;
066400080703       vscvft=§gedcr   ;
066500080703       errGenerico = *on;
066600081007 2     endif    ;
066700080703
066800081007 2     if vscpft=0 and vscvft<>*blanks   ;
066900080703       clear vscvft   ;
067000080703       errGenerico = *on;
067100081007 2     endif   ;
067200080703
067300080704       // se info da annullare e non c'e' la "A", msg forzabile
067400081215       //  non faccio se modalità controlli
067500081215 2     if i50mod<>'C' and vshann='S'  and vscatb=' '  ;
067600080704           errANN=*on;
067700080704           // se  contiene dati, msg forzabile
067800081007 3         if vscfsn='S'or vscval>0 or vscpft>0 or vscsna>0   ;
067900080704           vscmsg = Msg(07);
068000080704           vshann='X'      ;
068100080704           // Se non contiene dati abbligatorio l'annullamento
068200081007 x3        else            ;
068300080704           vscmsg = Msg(08);
068400081007 3         endif           ;
068500080704           EXSR AggioSFL   ;
068600080704           leavesr;
068700081007 2     endif    ;
068800110707
068900110707       // se c'e' "O"  obbligatorio l'annullamento
069000110707 2     if i50mod<>'C' and vshann='O'  and vscatb=' '  ;
069100110707           errANN=*on;
069200110707           vscmsg = Msg(08);
069300110707           EXSR AggioSFL   ;
069400110707           leavesr;
069500110707 2     endif    ;
069600080704
069700080703       // Se richiesto inserimento obbligatorio, per i dati
069800080703       //  obbligatori da tabella segnalo errore
069900081007 2     if i50obl='S' and vshobl='S'                 ;
070000081007 3     if vscatb='A'    or
070100081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
070200081003         if vshsne<>' ';
070300080703           errFSN=*on;
070400081003         endif   ;
070500081003         if vshsnv<>' ';
070600081003           errVAL=*on;
070700081003         endif   ;
070800081003         if vshsnf<>' ';
070900081003           errPFT=*on;
071000081003         endif   ;
071100080703           vscmsg = Msg(01);
071200080703           EXSR AggioSFL   ;
071300080703           leavesr;
071400081007 3     endif   ;
071500081007 2     endif   ;
071600081007
071700081007       // Non accetto se non previsto il valore '?'
071800081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
071900081007           errFSN=*on;
072000081007           vscmsg = Msg(20);
072100081007           EXSR AggioSFL   ;
072200081007           leavesr;
072300081007 3     endif    ;
072400081007       if vshifgv<>'?' and vscfsn='?'  ;
072500081007           errFSN=*on;
072600081007           vscmsg = Msg(21);
072700081007           EXSR AggioSFL   ;
072800081007           leavesr;
072900081007 3     endif    ;
073000080703
073100080703       // Se immesso "N" non indicare gli altri campi
073200081007 2     if vscfsn='N' and (vscval>0 or vscpft>0 or vscsna>0)   ;
073300080703       if vshsnf<>' '    ;
073400080703           errPFT=*on;
073500080703       else    ;
073600081003          errVAL=*on ;
073700080703       endif  ;
073800080703           vscmsg = Msg(02);
073900080703           EXSR AggioSFL   ;
074000080703           leavesr;
074100081007 2     endif   ;
074200080703
074300081003       // Se immesso almeno un dato, controllo gli obbligatori ed
074400081215       //   i facoltativi. I  facoltativi non li guardo in modalità
074500081215       //   solo controllo
074600081031       if vscatb =' ' and
074700081031 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval>0)            ;
074800081003
074900081007 3     if vscpft=0    and vshsnf='O'   ;
075000081003           errPFT=*on;
075100081003           vscmsg = Msg(03);
075200081003           EXSR AggioSFL   ;
075300081003           leavesr;
075400081007 3      endif    ;
075500100503
075600100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
075700100503 3     if i50obl='S' and
075800100503          I50mod<>'C' and  vscpft=0    and vshsnf='S'   ;
075900081003           errPFT=*on;
076000081003           vscmsg = Msg(05);
076100081003           vshsnf='X'      ;
076200081003           EXSR AggioSFL   ;
076300081003           leavesr;
076400081007 3     endif    ;
076500081007 3     if vscsna=0    and vshsns='O'   ;
076600081003           errSNA=*on;
076700081003           vscmsg = Msg(10);
076800081003           EXSR AggioSFL   ;
076900081003           leavesr;
077000081007 3     endif    ;
077100100503
077200100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
077300100503 3     if i50obl='S' and
077400100503          i50mod<>'C' and vscsna=0    and vshsns='S'   ;
077500081003           errSNA=*on;
077600081003           vscmsg = Msg(06);
077700081003           vshsns='X'      ;
077800081003           EXSR AggioSFL   ;
077900081003           leavesr;
078000081007 3     endif    ;
078100081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
078200081007           errFSN=*on;
078300081007           vscmsg = Msg(19);
078400081007           EXSR AggioSFL   ;
078500081007           leavesr;
078600081007 3     endif    ;
078700100503 3     if i50obl='S' and
078800100503 3        i50mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
078900081007           errFSN=*on;
079000081007           vscmsg = Msg(22);
079100081007           vshsne='X'      ;
079200081007           EXSR AggioSFL   ;
079300081007           leavesr;
079400081007 3     endif    ;
079500081007 3     if (vscval=0    and vshsnv='O')  or
079600100503          (vscval=0    and vshsnv='S' and i50mod<>'C'AND I50OBL='S')   ;
079700081007 4        if vshsnv='O'    ;
079800081003           vscmsg = Msg(11);
079900081003          else   ;
080000081215             vscmsg = Msg(04);
080100081215             vshsnv='X'      ;
080200081215 4        endif  ;
080300081007 4        if vsctva='%  '   ;
080400081003          %subst(vscmsg:37:18)='della % fatturato '     ;
080500081007 4        endif     ;
080600081007 4        if vsctva='KG '   ;
080700081003          %subst(vscmsg:37:18)='del peso in Kg.   '     ;
080800081007 4        endif     ;
080900081215
081000081003           errVAL=*on;
081100081003           EXSR AggioSFL   ;
081200081003           leavesr;
081300081215 3     endif    ;
081400081003
081500081003       // se Valore in % --> non deve superare 100%
081600081007 3     if  vscval>0 and vsctva='%  '    ;
081700081007 4       if vscval>100    ;
081800081003          errVAL=*on ;
081900081003          vscmsg = Msg(12);
082000081003          EXSR AggioSFL   ;
082100081003          leavesr;
082200081007 4       endif      ;
082300081003
082400080925       // Sommo la percentuale se presente per ogni categoria
082500080925       //  se supera 100% segnalo errore
082600081006       //  solo se previsto controllo sulla %
082700081007 4     if wctrprc='P'     ;
082800081006
082900110707       // Non sommo le info annullate perchè non le devo considerare valide
083000110707       if vshann=' '   ;
083100110707         totprc=totprc+vscval    ;
083200110707       endif  ;
083300110707
083400081007 5       if totprc>100    ;
083500081003         errVAL=*on ;
083600080925         vscmsg = Msg(09);
083700080925         EXSR AggioSFL   ;
083800080925         leavesr;
083900081007 5       endif      ;
084000081007 4     endif      ;
084100080925
084200081003       // Se immessa la %, visualizzo in base a SPT
084300080925       exsr   VISUALimp     ;
084400081007 3     endif      ;
084500081007 2     endif      ;
084600081006
084700081006       // Concorrenti logistica ammessi solo se outsourcing=S
084800110707 2     if vscifo='40 '  and vscatb=' ' and (vscfsn<>' ' or vscpft>0 or vscsna>0
084900110707                            or vscval>0) ;
085000081006       if outfsn<>'S'   ;
085100081006         errFSN=*on ;
085200081006         vscmsg = Msg(14);
085300081006         EXSR AggioSFL   ;
085400081006         leavesr;
085500081006       endif             ;
085600081007
085700081006       // Memorizzo presenza info 40  se immesso Si o presente altro
085800081006       //  importo
085900110707       //  if vscfsn=' ' or vscfsn='S'  ;
086000110707         if vshann=' ' and vscfsn<>'N';
086100081007         LOG40='S'         ;
086200081007         endif             ;
086300081007 2     endif             ;
086400080925
086500081008
086600081126       // Sottotipo col '?' --> Se '?' richiamo pgm di ricerca
086700081126 2     if vscfsn='?' ;
086800081008       clear tntb70ds ;
086900081008       itb70cpo=i50cpo;
087000081023       itb70rag=i50rag ;
087100081126       itb70tpf=vscifo  ;
087200081008       itb70pgm=i50pgm  ;
087300081008       callp TNTB70R (kpjba:tntb70ds)  ;
087400081008       clear vscfsn       ;
087500081126 3     if otb70spf>*zeros    ;
087600081008         vscspp=otb70spf  ;
087700081008         vsdifo=otb70dspf ;
087800081008         vscval=%int(otb70spf) ;
087900081126 3     endif         ;
088000081008       c01csr=s01nrr    ;
088100081008       exsr AggioSFL    ;
088200081008       leavesr       ;
088300081126 2     endif         ;
088400081007
088500081126 2     if vscval=0    and vshifgv='?' ;
088600081007       clear vscspp      ;
088700081009       clear vsdifs      ;
088800081009       vsdifo=CoRicerca  ;
088900081126 2     endif     ;
089000081007
089100081126       // Motivo non affidamento a noi: da richidere solo se corriere BART
089200081126       //  non è al 100
089300081126 2     if  vscifo='MOI'   ;
089400081126
089500081126         if vscval=0   ;
089600081126          vsdifo=CoRicercaMOI  ;
089700081126         endif         ;
089800081126
089900081126 3       if bart_10=100 and vscval>0        ;
090000081126           errVAL=*on ;
090100081126           vscmsg = Msg(16);
090200081126           EXSR AggioSFL   ;
090300081126           leavesr;
090400081126 3       endif             ;
090500081126
090600081126         if vscval>0      ;
090700081126           MOIconta=moiconta+1   ;
090800081126         endif     ;
090900081126
091000081126         moinrr=s01nrr    ;
091100081126
091200081126 2     endif            ;
091300081008
091400081007       // Controllo
091500081126 2     if vscval>0    and vshifgv='?' ;
091600081007       w005a=%editc(vscval:'X')    ;
091700081126       chain ('IFS':vscifo:%subst(w005a:2:4))  tntbe01l    ;
091800081126 3       if not %found(tntbe01l) or tbeatb<>' ' or %subst(w005a:1:1)<>'0';
091900081007          errVAL=*on ;
092000081007          vscmsg = Msg(18);
092100081007          EXSR AggioSFL   ;
092200081007          leavesr;
092300081126 x3       else                        ;
092400081007          vscspp=%subst(w005a:2:4)    ;
092500081007          difs=tbeuni ;
092600081007          vsdifo=§ifsdes              ;
092700081007          vsdifs=§ifsdel              ;
092800081126  3      endif             ;
092900081126
093000081126        //  Memorizzo i codici per vedere se sono doppi
093100081126        w008a=vscifo+%editc(vscval:'X')   ;
093200081126        Indx=%lookup(w008a:IFOval)   ;
093300081126 3      if Indx>0    ;
093400081126         errVAL=*on ;
093500081126 4       if vscifo='MOI'   ;
093600081126           vscmsg = Msg(28);
093700081126         else      ;
093800081126           vscmsg = Msg(30);
093900081126 4       endif     ;
094000081126         EXSR AggioSFL   ;
094100081126         leavesr;
094200081126
094300081126 x3     else   ;
094400081126          ifoval(yy)=w008a  ;
094500081126          yy=yy+1   ;
094600081126          // Se c'e' un MOI=9999 e uno no 9999, errore
094700081126          w008a='MOI09999'  ;
094800081126          Indx=%lookup(w008a:IFOval) ;
094900081126  4       if Indx>0   and moiconta>1 ;
095000081126            errVAL=*on ;
095100081126            vscmsg = Msg(29);
095200081126            EXSR AggioSFL   ;
095300081126            leavesr;
095400081126  4       endif    ;
095500081126  3     endif             ;
095600081126  2    endif             ;
095700081007
095800081126  2    if vscatb=' '  ;
095900081031
096000080925       // Se info SPT - totale spesa trasporti,  la memorizzo
096100080925       //  per il calcolo delle %
096200080925       if vscifo='SPT' ;
096300080925       SPTPFT=vscpft   ;
096400080925       endif           ;
096500081023       // Se info LOG - logistica  memorizzo per dare obbligatorio outsourci
096600081023       if vscifo='LOG' ;
096700081023       LOGfsn=vscfsn   ;
096800081023       LOGnrr=s01nrr   ;
096900081006       endif           ;
097000081023       // Se info OUT - outsourcing memorizzo per dare obbligatori
097100081023       //  i concorrenti della logistica
097200081023       if vscifo='OUT' ;
097300081023       OUTfsn=vscfsn   ;
097400081023       OUTnrr=s01nrr   ;
097500081023       endif           ;
097600081007       // Se info 10 _BAR - concorrenti utilizzati bartolini
097700081007       //  memorizzo la % che ha
097800081007       if vscifo='10' and vscspp='_BAR' ;
097900081007       bart_10=vscval   ;
098000120105
098100120110         //?Controllo % affidamento a BRT
098200120110         SELECT;
098300120110
098400120110         //?Controllo se devo azzerare la % di affidato a BRT
098500120110         WHEN  wAzzera10 and bart_10 > 0;
098600120110           errVAL = *on;
098700120125           VSCmsg = Msg(33);
098800120110           EXSR AggioSFL;
098900120110           leavesr;
099000120110
099100120110         //?Controllo se devo aumentare la % di affidato a BRT
099200120111         WHEN  wAumenta10 and bart_10 < sav_bar_10 and bart_10 < 100;
099300120110           errVAL = *on;
099400120125           VSCmsg = Msg(31);
099500120110           EXSR AggioSFL;
099600120110           leavesr;
099700120510
099800120510         //?Controllo se devo aumentare la % di affidato a BRT solo se = 0
099900120510         WHEN  wAumenta10f and bart_10 < sav_bar_10 and bart_10 = 0;
100000120510           errVAL = *on;
100100120510           VSCmsg = Msg(31);
100200120510           EXSR AggioSFL;
100300120510           leavesr;
100400120110
100500120110         //?Controllo se devo diminuire la % di affidato a BRT
100600120110         WHEN  bart_10 > 0 and
100700120110               wDiminuisci10 and bart_10 > sav_bar_10;
100800120110           errVAL = *on;
100900120125           VSCmsg = Msg(34);
101000120110           EXSR AggioSFL;
101100120110           leavesr;
101200120110         ENDSL;
101300120105
101400081007       endif           ;
101500100302       // Se info ATR - altro di ripartizione memorizzo la %
101600100226       if vscifo='ATR'                  ;
101700100226       infoATR=vscval   ;
101800100226       endif           ;
101900100226
102000081015       // Memorizzo se info Italia o Estero per il motivo non affidamento
102100081125       //if vshie='I' and (vscfsn<>' ' or vscval>0 or vscpft>0 or vscsna>0);
102200081125       //infoITA='S'  ;
102300081125       //endif        ;
102400081125       //if vshie='E' and (vscfsn<>' ' or vscval>0 or vscpft>0 or vscsna>0);
102500081125       //infoEST='S'  ;
102600081125       //endif        ;
102700081126 2     endif        ;
102800080925
102900080703       EXSR AggioSFL   ;
103000081006 1     endif   ;
103100081125
103200080703
103300080703       s01nrr=1+s01nrr  ;
103400080703       chain s01nrr    mk50s01    ;
103500081006 0     enddo         ;
103600081006
103700081006       // Controllo ultima categoria
103800081006       EXSR CambioCAT     ;
103900081006
104000081215       // A questo punto le info minime sono presenti
104100081215       // solo per controlli obbligatori
104200081215       if i50obl='S' ;
104300081215         o50ifomin='S' ;
104400081215       endif ;
104500081215
104600081126       // Un motivo non affidamento obbligatorio se Bart<100%
104700081126 3     if bart_10<100 and MOIconta =0   and i50obl='S' ;
104800081126         chain moinrr    mk50s01    ;
104900081126         s01nrr=moinrr              ;
105000081126         errVAL=*on ;
105100081126         vscmsg = Msg(27);
105200081126         EXSR AggioSFL   ;
105300081126         leavesr;
105400081126 3     endif             ;
105500081126
105600081023       // Se logistica interessante per noi, obbligatorio outsourcing
105700081023       if LOGfsn='S' and outfsn=' '  and i50obl='S'  ;
105800081023         chain lognrr    mk50s01    ;
105900081023         s01nrr=lognrr              ;
106000081023         errFSN=*on ;
106100081023         vscmsg = Msg(26);
106200081023         EXSR AggioSFL   ;
106300081023         leavesr;
106400081023       endif             ;
106500081006       // Se presente outsourcing, obbligatori concorrenti logistica
106600081006       if outfsn='S' and log40=' '  and i50obl='S'  ;
106700081006         chain outnrr    mk50s01    ;
106800081006         s01nrr=outnrr              ;
106900081006         errFSN=*on ;
107000081006         vscmsg = Msg(15);
107100081006         EXSR AggioSFL   ;
107200081006         leavesr;
107300081006       endif             ;
107400081006
107500080409       ENDSR;
107600080409
107700081006       //--------------------------------------------------------------
107800081006       //?cambiata categoria delle info conn- controllo e salvataggi
107900081006       //--------------------------------------------------------------
108000081006       BEGSR  CambioCAT    ;
108100081006       if wctrprc='P'  and  i50obl='S' and totprc<100  ;
108200081006       s01nrr=s01nrr-1 ;
108300081006       chain s01nrr    mk50s01    ;
108400081006           errVal=*on;
108500081006           vscmsg = Msg(13);
108600081006           %subst(vscmsg:40:21)=wdescat   ;
108700081006           EXSR AggioSFL   ;
108800081006       endif              ;
108900081006
109000081006       // Pulisco e memorizzo nuovi dati per nuova categoria
109100081006        clear  totprc     ;
109200081215        wctrprc=vshobl  ;
109300081215        wdescat=vsdifo   ;
109400081006        ENDSR   ;
109500080925       //--------------------------------------------------------------
109600080925       //?Visualizzo importi in base alla spesa trasporti
109700080925       //--------------------------------------------------------------
109800080925       BEGSR    VISUALimp  ;
109900081003       if vshsnf=' ' and vscpft=0 and vscval>0 and vsctva='%  '  ;
110000081215       PFTblu=*on     ;
110100081003       vscpft=(SPTpft*vscval)/100   ;
110200080925       if vscpft>0    ;
110300080925       vscvft='EUR'   ;
110400080925       endif ;
110500080925       endif ;
110600080925
110700080925       ENDSR      ;
110800080703       //--------------------------------------------------------------
110900080703       //?Aggiornamento sfl
111000080703       //--------------------------------------------------------------
111100080703       BEGSR AGGIOSFL  ;
111200080703       if vscmsg<>*blanks   ;
111300080703       errMessage  = *on;
111400080703       errGenerico = *on;
111500080704       c01csr=s01nrr    ;
111600080703       endif    ;
111700080703
111800080703       exsr ProtCampi   ;
111900080703
112000080703       //  Ripristino dei flag di forzatura
112100081003       if vshsnf='X' and vscpft>0   ;
112200081003       vshsnf='S'   ;
112300080703       endif    ;
112400081003       if vshsns='X' and vscsna>0   ;
112500080703       vshsns='S'   ;
112600080703       endif    ;
112700081003       if vshsnv='X' and vscval>0  ;
112800081003       vshsnv='S'   ;
112900080703       endif    ;
113000081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
113100081007       vshsne='S'   ;
113200081007       endif    ;
113300080703
113400110707       if vscatb='A' and vshann= 'X' ;
113500080704       vshann='S'  ;
113600080704       endif   ;
113700080703
113800080703       update  mk50s01  ;
113900080703       ENDSR   ;
114000080207       //--------------------------------------------------------------
114100080627       //?Inizializzazione SFL01
114200080207       //--------------------------------------------------------------
114300080409       BEGSR InzS01;
114400080207
114500080207       // Pulizia subfile
114600080207         SflDsp_N    = *on;
114700080207         SflDspCtl_N = *on;
114800080627         write  MK50C01;
114900080207         SflDspCtl_N = *off;
115000080207         SflEnd      = *off;
115100080530
115200080530         clear C01rcd;
115300100507         C01csr=1;
115400080627         S01nrr=0 ;
115500080702         clear Vscmsg;
115600080207         errMessage  = *off;
115700080207         errGenerico = *off;
115800080627         clear savcat  ;
115900080627         clear wcat    ;
116000080627         clear wifo    ;
116100081006         clear wIFS    ;
116200081006         clear sptpft  ;
116300100302         clear savspt  ;
116400100302         clear savatr  ;
116500120109         wForzaAgg = *off;
116600120111         clear sav_bar_10;
116700080409
116800080702       // Carico le info
116900080627       xx=1    ;
117000080627       Indx=1    ;
117100080702       dow   xx<=50    ;
117200080702       if    ifoor(xx)<>*blanks  ;
117300080627       clear  MK50S01;
117400080627
117500080627       Wcat= %subst(ifoor(xx):1:2)   ;
117600080702       Wifo= %subst(ifoor(xx):6:3)   ;
117700080627
117800080627       // cambio di categoria
117900080627        if savcat=*blanks or savcat<>wcat               ;
118000080627        savcat=wcat   ;
118100080627        Ifosott=*on   ;
118200080627
118300080627       // Verifico se devo visualizzare la titolo della categoria
118400080627         clear  DIFG  ;
118500080627         reset TIBS02ds;
118600080627         T02sif = knsif;
118700080627         T02cod = 'IFG';
118800080627         T02mod = 'C'  ;
118900080627         T02ke1 = wcat;
119000080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
119100080627         if t02err=*blanks ;
119200080627            difg=t02uni    ;
119300080627         endif            ;
119400080627
119500080627       // Se prevista la visualizzazione della categoria
119600080627       //  emetto la riga in videata,
119700080627       if §ifgvis='S'    ;
119800080703         vsdifo=§ifgdes         ;
119900080703         vsdifo=%trim(vsdifo)+':'    ;
120000080703         // In VSHIMM   metto una "C" per indicare che si tratta
120100080703         //    di riga di categoria
120200080703         vshimm='C'        ;
120300081007
120400081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
120500081003         vshobl=§ifgctpr   ;
120600081007
120700081007         EXSr ProtCampi    ;
120800081003
120900080627         s01nrr=s01nrr+1   ;
121000080627         write mk50s01     ;
121100080627         protriga=*off     ;
121200080627         Ifosott=*off      ;
121300081007         clear vscfsn      ;
121400100507         // Se la prima riga è un commento, posizionamento cursore sulla seconda
121500100507         if s01nrr=1   ;
121600100507          c01csr= 2 ;
121700100507         endif   ;
121800100507
121900081007       endif               ;
122000081007       endif               ;
122100080627
122200080627       Indx=%lookup(wifo:ifo)   ;
122300080627       if Indx>0                ;
122400080627         difo=ifods(Indx)         ;
122500080627         vscifo=ifo(Indx)       ;
122600080702
122700080702         // Dati da tabella
122800080702          vshobl=§ifoobl  ;
122900081015          vshie =§ifotip  ;
123000080704
123100080702          vshann=§ifoann  ;
123200080704
123300081003         // VALORE
123400081003          clear vshsnv  ;
123500081003          clear vsctva  ;
123600081003          if §ifosnv<>' '  ;
123700081003          vshsnv=§ifosnv  ;
123800081003          // tipo valore
123900081003          vsctva=§ifotva ;
124000080703          endif   ;
124100080704
124200081003         // SPEDIZIONI
124300080702          vshsns=§ifosns  ;
124400080704
124500081003         // FATTURATO
124600080702          vshsnf=§ifosnf  ;
124700081003
124800081003         // SI/NO
124900081003          vshsnE=§ifosnE  ;
125000081003
125100080702          vshifgv=§ifgvis  ;
125200081007          clear vscint    ;
125300080627
125400080627         // Record senza sottotipo
125500081006         select    ;
125600081006         when §ifoifs=' '    ;
125700080702            vsdifo=§ifodes         ;
125800080703            if Ifosott=*on    ;
125900080703               vsdifo=%trim(vsdifo)+':'    ;
126000080703            endif    ;
126100080702            chain    (i50cpo:wifo) ticpi01l  ;
126200081126            if %found(ticpi01l)   ;
126300081126             trovacpi='S'    ;
126400081126             else ;
126500081126             trovacpi='N'    ;
126600081126             endif  ;
126700080702            EXSR CaricaInfo        ;
126800081006
126900081006         // Record con   sottotipo da caricare tutti
127000081006         when   §ifoifs='S' and §ifoifv='S';
127100080627            EXSR CARCon    ;
127200081006
127300081006         // Record con   sottotipo da caricare col '?'
127400081006         //   la categoria visualizzata in questo caso è obbligatoria
127500081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
127600081006         //  il '?'
127700081006         when   §ifoifs='S' and §ifoifv='?';
127800081006          vshifgv=§ifoifv  ;
127900081215          yy=1        ;
128000081126
128100081126          if §ifonrec=0 ;
128200081126             §ifonrec=1  ;
128300081126          endif     ;
128400081126
128500081126          setll (i50cpo:wifo) ticpi01l  ;
128600081126          dow yy<=§ifonrec  ;
128700081006            EXSR CarconUNA            ;
128800081126            yy=yy+1  ;
128900081126          enddo    ;
129000081126
129100081006         endsl ;
129200080703
129300080703         endif ;
129400080702         endif ;
129500080702
129600080702         xx=xx+1   ;
129700080702         enddo     ;
129800080702         ENDSR    ;
129900080627
130000080702       //--------------------------------------------------------------
130100081006       //?Carico con   sottotipo  carica riga per riga
130200080702       //--------------------------------------------------------------
130300080702       BEGSR  CARcon            ;
130400080702
130500081126         tbeke1=wifo   ;
130600080925
130700081006         setll ('IFS':tbeke1) tntbe01l   ;
130800081006         READE ('IFS':tbeke1) tntbe01l   ;
130900080702
131000080702         dow not %eof(tntbe01l)   ;
131100080702         if tbeatb=' '    ;
131200081006         difs=tbeuni      ;
131300081006         wifs=tbeke2      ;
131400080702         vscspp=tbeke2      ;
131500081127         vshspp=tbeke2      ;
131600081007         vsdifo=§ifsdes  ;
131700110707         vshann=§ifsann  ;
131800080702
131900081006         chain    (i50cpo:wifo:wifs) ticpi01l  ;
132000081126            if %found(ticpi01l)   ;
132100081126             trovacpi='S'    ;
132200081126             else ;
132300081126             trovacpi='N'    ;
132400081126             endif  ;
132500080702         EXSR CaricaInfo    ;
132600080925         endif   ;
132700080702
132800081006         READE ('IFS':tbeke1) tntbe01l   ;
132900080702         enddo   ;
133000080702
133100080702       ENDSR;
133200081006       //--------------------------------------------------------------
133300081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
133400081006       //--------------------------------------------------------------
133500081006       BEGSR  CARconUNA         ;
133600081007
133700081007       vscint='?'    ;
133800081007
133900081126       reade    (i50cpo:wifo) ticpi01l  ;
134000081006
134100081126 1     if not %eof(ticpi01l)   ;
134200081126         trovacpi='S'    ;
134300081126
134400081006         vscspp=cpispf   ;
134500081127         vshspp=cpispf   ;
134600081006
134700081126         tbeke1=wifo    ;
134800081126
134900081006         tbeke2=cpispf  ;
135000081006         chain ('IFS':tbeke1:tbeke2) tntbe01l   ;
135100081006 2       if not %found(tntbe01l)    ;
135200081006         clear tbeuni       ;
135300081006 2       endif    ;
135400081007         difs=tbeuni     ;
135500081006
135600081007         vsdifo=§ifsdes   ;
135700081007         vsdifs=§ifsdel   ;
135800081007
135900081006         EXSR CaricaINFO  ;
136000081006
136100081006 1x      else    ;
136200081126         trovacpi='N'    ;
136300081006
136400081126         // se non trovato dicitura generica da tabella IFO
136500081126         if wifo='MOI'  ;
136600081126           vsdifo=coRicercaMOI    ;
136700081126           else  ;
136800081126           vsdifo=coRicerca       ;
136900081126         endif  ;
137000081007         clear vsdifs      ;
137100081126         clear vscspp      ;
137200081127         clear vshspp      ;
137300081006
137400081006         EXSR CaricaINFO   ;
137500081006         endif             ;
137600081006
137700081006       ENDSR;
137800080702       //--------------------------------------------------------------
137900081006       //?Carica ogni riga di info commerciale
138000080702       //--------------------------------------------------------------
138100080702         BEGSR CaricaINFO             ;
138200080925         clear pftblu   ;
138300080925
138400081126         if trovacpi='S'                             ;
138500080703          vshimm=' '        ;
138600080702          vscfsn=cpifsn   ;
138700081003          vscval=cpival   ;
138800081007          if vshifgv='?'   and vscspp>*zeros    ;
138900081007            w0040=%int(vscspp)  ;
139000081007            vscval=w0040     ;
139100081007            clear vscfsn     ;
139200081007          endif            ;
139300080702          vscsna=cpisna   ;
139400080702          vscpft=cpipft   ;
139500080925          if vscpft>0     ;
139600080925          vshprpft='S'    ;
139700080925          endif           ;
139800080702          vscvft=cpivft   ;
139900080702          Dataiso=%date(cpidim:*iso)  ;
140000080702          datadmy=Dataiso ;
140100080702          vscdim=%dec(datadmy)  ;
140200081003          clear vscfil  ;
140300081003          if cpifil>0   ;
140400081003          vscfil=%editc(cpifil:'X')   ;
140500081003          endif         ;
140600081003
140700081003          if cpipru<>*blanks ;
140800080702          vscpru=cpipru   ;
140900081003          vscdpru='Utente' ;
141000081003          else  ;
141100081003          clear vscpru   ;
141200081003          clear vscdpru   ;
141300081003          endif     ;
141400080925
141500080925          exsr ViSUALimp   ;
141600080925
141700080702         else             ;
141800080703          vshimm='S'      ;
141900080703          Protann=*on     ;
142000080702          clear vscfsn    ;
142100081003          clear vscval    ;
142200080702          clear vscsna    ;
142300080702          clear vscpft    ;
142400080702          clear vscvft    ;
142500080702          clear vscdim    ;
142600080702          clear vscfil    ;
142700080702          clear vscpru    ;
142800081003          clear vscdpru    ;
142900080925          clear vshprpft  ;
143000080702         endif            ;
143100081006
143200080925         exsr ProtCAMPI    ;
143300080703
143400080703         // I record da annullare li visualizzo solo se ci sono
143500110707           if vshann=' ' or (vshann<>' '  and vscfsn<>' ') or
143600110707                             (vshann<>' '  and vscpft>0)   or
143700110707                             (vshann<>' '  and vscval>0   )  ;
143800081003
143900081003         // In interrogazione visulizzo solo i record pieni col sottotipo
144000081003           if i50mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
144100080703            s01nrr=s01nrr+1   ;
144200080703            write mk50s01     ;
144300080925
144400080925         // Se info SPT - totale spesa trasporti,  la memorizzo
144500100302         //  per il calcolo delle %  e per vedere se modificata
144600080925         if vscifo='SPT' ;
144700080925          SPTPFT=vscpft   ;
144800100302          savSPT=vscpft   ;
144900080925         endif           ;
145000100302         //  per vedere se modificata
145100100302         if vscifo='ATR' ;
145200100302          savATR=vscval   ;
145300100302         endif           ;
145400120105
145500120105         //?Memorizzo la % di affidamento a BRT
145600120110           IF  VSCifo = '10' and VSCspp = '_BAR';
145700120105           //?Se devo aumentarla calcolo almeno 1% in +
145800120110             IF  wAumenta10 and VSCval > 0;
145900120111               sav_bar_10 = VSCval + 1;
146000120105             ENDIF;
146100120510             IF  (wAumenta10 or wAumenta10f) and VSCval = 0;
146200120110               sav_bar_10 = 1;
146300120110             ENDIF;
146400120105           //?Se devo dominuirla calcolo almeno 1% in -
146500120110             IF  wDiminuisci10 and VSCval > 0;
146600120111               sav_bar_10 = VSCval - 1;
146700120105             ENDIF;
146800120105           ENDIF;
146900080925
147000080703           endif ;
147100081003           endif ;
147200081003
147300080702       ENDSR;
147400080702       //--------------------------------------------------------------
147500080702       //?Gestione protezione campi del sfl
147600080702       //--------------------------------------------------------------
147700080702          BEGSR   ProtCampi   ;
147800080703           protfat=*off    ;
147900081003           protVal =*off   ;
148000080703           protsped=*off   ;
148100080703           protfsn =*off   ;
148200080703           Ifosott=*off    ;
148300080703           Protann=*off ;
148400080704           Infoyell=*off ;
148500080703
148600081007           // Se riga di categoria accendo solo protezione e sottolineatura
148700081007          if vshimm='C'    ;
148800081007           Ifosott=*on     ;
148900081007           Protriga=*on    ;
149000081007          else             ;
149100081007
149200080702          // in base a cosa prevede la tabella, proteggo i campi
149300080702          if vshifgv='N' ;
149400080702            Ifosott=*on       ;
149500080702          endif    ;
149600080702
149700080704          if vshann<>' '  ;
149800080702           protfat=*on     ;
149900081003           protVal =*on    ;
150000080702           protsped=*on    ;
150100080702           protfsn =*on    ;
150200080704           infoyell=*on    ;
150300080702          else            ;
150400081006          protann=*on     ;
150500081031
150600081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
150700081103          if vscpft>0 and vshsnf=' ' and vscval=0;
150800081031          protann=*off    ;
150900081031          endif    ;
151000081031          if vscsna>0 and vshsns=' ' ;
151100081031          protann=*off    ;
151200081031          endif    ;
151300081127          if vscval>0 and vshsnv=' ' and vshifgv<>'?'       ;
151400081031          protann=*off    ;
151500081031          endif    ;
151600081031          if vscfsn<>' ' and vshsne=' ' ;
151700081031          protann=*off    ;
151800081031          endif    ;
151900081006
152000080703            if vshsnf=' ' ;
152100080702             protfat=*on     ;
152200080702            endif           ;
152300081003            if vshsnv=' ' ;
152400081003             protVal =*on     ;
152500080702            endif           ;
152600080703            if vshsns=' ' ;
152700080702             protsped=*on     ;
152800080702            endif           ;
152900081003            if vshsne=' ' ;
153000081003             protfsn =*on     ;
153100081003            endif           ;
153200080702          endif           ;
153300080703          // Se record di nuova immissione
153400081003          //  proteggo annullamento
153500080703            if vshimm='S'  ;
153600080703             Protann=*on  ;
153700080703           endif   ;
153800081006
153900081006          // Se prevista scelta sottotipo col'?' visualizzo valore
154000081006          //   col codice e vicino il '?'
154100081006          if vshifgv='?'    ;
154200081006             protVal =*off    ;
154300081007             if vscspp>*zeros    ;
154400081007             else          ;
154500081007             infoyell=*on    ;
154600081007             endif            ;
154700081006          endif               ;
154800081007          endif               ;
154900080703
155000080702          ENDSR    ;
155100080702       //--------------------------------------------------------------
155200080702       //?conferma aggiornamento
155300080702       //--------------------------------------------------------------
155400080702        BEGSR ConfAGGIO              ;
155500090520        clear ultdata   ;
155600081003
155700080704       s01nrr=1  ;
155800080704       chain s01nrr    mk50s01    ;
155900080704
156000080704 1     dow %found    ;
156100080704       // Escludo i record di descrizione categoria
156200080704 2     if vshimm<>'C'    ;
156300080925
156400080925       // se valori calcolati da pgm li pulisco prima dei controlli
156500080925       if vshsnf=' ' and vshprpft=' '   ;
156600080925       clear vscpft   ;
156700080925       clear vscvft   ;
156800080925       endif          ;
156900080925
157000081127       IF VShifgv<>'?'    ;
157100081127         chain    (i50cpo:vscifo:vscspp) ticpi01l  ;
157200081127       else   ;
157300081127         chain    (i50cpo:vscifo:vshspp) ticpi01l  ;
157400081127       endif   ;
157500081127             if %found(ticpi01l)   ;
157600081127             trovacpi='S'    ;
157700081127             else ;
157800081127             trovacpi='N'    ;
157900081127             endif  ;
158000081127
158100081127       // A n n u l l a m e n t o oppure per qualche strano motivo
158200081127       //   la info c'e'  ma a video non è stata inserita
158300080925
158400081127 3     if trovacpi='S'      and (vscatb='A'  or (vscfsn=' ' AND
158500081003                                 vscpft=0 and vscsna=0 and vscval=0));
158600081003         delete ticpi000  ;
158700081003 x3    else    ;
158800080704
158900081127       // V a r i a z i o n e :  verifico se modificati dei dati
159000081127 31    if trovacpi='S'     and vscatb=' '   ;
159100080704 4     if (cpifsn<>vscfsn) or
159200081003          (cpival<>vscval) or
159300080704          (cpisna<>vscsna) or
159400081007          (cpispf<>vscspp) or
159500080704          (cpipft<>vscpft) or
159600080704          (cpivft<>vscvft) ;
159700080704          // reimposto data ora profilo immissione
159800080704          cpidim=*date    ;
159900090520          ultdata=*date    ;
160000080704          cpifil=utefil   ;
160100080704          cpipru=knmus    ;
160200080704
160300081007          cpifsn= vscfsn  ;
160400081007          IF VShifgv<>'?';
160500081003          cpival= vscval  ;
160600081003          cpitva= vsctva  ;
160700081007          else            ;
160800081007          cpispf=vscspp   ;
160900081007          if cpispf<>*blanks ;
161000081007          cpifsn='S'      ;
161100081007          ENDIF           ;
161200081007          ENDIF           ;
161300080704          cpisna= vscsna  ;
161400080704          cpipft= vscpft  ;
161500080704          cpivft= vscvft  ;
161600081006          // verifico se dei dati non ci voglio più: una volta aggiunti
161700080704          //  quelli previsti, lo tolgo
161800080704
161900081003 5        if (cpival>0 and vshsnv=' ')    ;
162000081006          if (cpisna>0 and vshsns<>' ')  or (cpipft>0  and vshsnf<>' ') or
162100081006             (cpifsn<>' ' and vshsne<>' ')   ;
162200081003          clear cpival   ;
162300080704          endif          ;
162400080704 5        endif          ;
162500080704 5        if (cpisna>0 and vshsns=' ')    ;
162600081006          if (cpival>0 and vshsnv<>' ')  or (cpipft>0  and vshsnf<>' ') or
162700081006             (cpifsn<>' ' and vshsne<>' ')   ;
162800080704          clear cpisna   ;
162900080704          endif          ;
163000080704 5        endif          ;
163100080704 5        if (cpipft>0 and vshsnf=' ')    ;
163200081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
163300081006             (cpifsn<>' ' and vshsne<>' ')   ;
163400080704          clear cpipft   ;
163500080704          clear cpivft   ;
163600080704          endif          ;
163700080704 5        endif          ;
163800081006 5        if (cpifsn<>' ' and vshsne=' ')    ;
163900081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
164000081006             (cpipft>0  and vshsnf<>' ')     ;
164100081006          clear cpifsn   ;
164200081006          endif          ;
164300081006 5        endif          ;
164400080704
164500080704        update ticpi000  ;
164600080704 4      endif  ;
164700081003 3a     endif  ;
164800080704
164900081127       // I m m i s s i o n e
165000081127 3     if trovacpi='N'     and vscatb=' ' and (vscfsn<>' ' or
165100081003       vscpft>0 or vscsna>0 or vscval>0) ;
165200080704       clear ticpi000;
165300080704        cpicpo=i50cpo   ;
165400080704        cpitpf=vscifo  ;
165500080704        cpispf=vscspp  ;
165600080704        cpifsn=vscfsn ;
165700081007        if vshifgv<>'?';
165800081003        cpival= vscval  ;
165900081003        cpitva= vsctva  ;
166000081007        else            ;
166100081007          if cpispf<>*blanks ;
166200081007          cpifsn='S'      ;
166300081007          endif           ;
166400081007        endif           ;
166500081007
166600080704        cpisna= vscsna  ;
166700080704        cpipft= vscpft  ;
166800080704        cpivft= vscvft  ;
166900080704        cpidim=*date    ;
167000090520        ultdata=*date    ;
167100080704        cpifil=utefil   ;
167200080704        cpipru=knmus    ;
167300080704
167400080704        write ticpi000 ;
167500081003  3a    endif           ;
167600081003  3     endif           ;
167700080704  2     endif           ;
167800080704
167900080704        s01nrr=s01nrr+1 ;
168000080704        chain s01nrr    mk50s01    ;
168100080704  1     enddo           ;
168200080704
168300081015        clear waggiocpo    ;
168400081015
168500081003       // Aggancio tncpo per aggiornare se ho tutte le info
168600081003       chain   i50cpo   tncpo01l   ;
168700081015  1      if %found(tncpo01l)    ;
168800081015         chain  cp1nrr    tncpo00f   ;
168900081015  1      endif                  ;
169000081003  1      if %found(tncpo00f)   ;
169100081015
169200081015         // Se inserimento obbligatorio--> posso impostare la "S"
169300081015         //  altrimenti controllo da CPI
169400081003
169500081015  2       if i50obl=' '   ;
169600081015            // Rifaccio il controllo del sfl come se fosse
169700081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
169800081215            savmod=i50mod   ;
169900081215            i50mod='C'      ;
170000081215            i50obl='S' ;
170100081215
170200081015            exsr contrs01   ;
170300081215
170400081215            i50obl=' ' ;
170500081215            i50mod=savmod   ;
170600081015
170700081015  3         if ErrGenerico=*off;
170800081215              Waggiocpo='S';
170900081015  3         endif  ;
171000081015
171100081015  x2      else;
171200081015          Waggiocpo='S'   ;
171300081015  2       endif           ;
171400081015
171500081015              dcpo01=cporst    ;
171600081015              §cpoifotot=Waggiocpo   ;
171700081015              o50ifotot=Waggiocpo   ;
171800090520              if ultdata > 0   ;
171900090520                §cpoifodul=%editc(ultdata:'X')  ;
172000090520              endif   ;
172100090520              // Se inserita memorizzo la spesa trasporti reale
172200090520              if sptpft>0   ;
172300090520              §cposptp=sptpft   ;
172400090520              endif   ;
172500100302
172600100302              //  Se variata spesa trasporti o % ATR, ricalcolo cod importanza potenziale
172700100302             if savSPT<> SPTPFT or savatr<>infoATR   ;
172800100302             clear trmk52ds  ;
172900100302             i52cpo=cpocpo  ;
173000100302             i52spt= SPTpfT ;
173100100302             i52ATR=infoATR  ;
173200100302
173300100302             callp TRMK52R  (trmk52ds)  ;
173400100302             cpoftr=o52cimp  ;
173500100302             endif   ;
173600100302
173700081015              cporst=dcpo01    ;
173800081015              update tncpo000  ;
173900081003  1    endif   ;
174000080702        ENDSR;
174100080702
174200080206       //--------------------------------------------------------------
174300080206       //?Operazioni finali.
174400080206       //--------------------------------------------------------------
174500080206       BEGSR RoutEnd;
174600080627
174700080704         if i50tla<>' '    ;
174800080704         // Chiusura pgm   ;
174900080704         clear tibs02ds  ;
175000080704         t02tla='C'      ;
175100080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
175200080704
175300080206         *inLR = *on;
175400080704         endif             ;
175500080704
175600080206         return;
175700080206
175800080206       ENDSR;
175900080206
176000080206      /end-free
176100080206
176200080206       //--------------------------------------------------------------
176300080206       //?Schiere a tempo di compilazione.
176400080206       //--------------------------------------------------------------
176500080206
176600080410** - MSG ------------------------------------------------------------------ -*
176700081003Info Commerciale obbligatoria                                                   01
176800080703Per Info Comm. non presente (N), non indicare gli altri dati                   02
176900081003La Info Comm. prevede l'inserimento del Fatturato presunto OBBLIGATORIO        03
177000081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx: enter per forzare     04
177100081003La  Info Comm. prevede l'inserimento del Fatturato presunto: enter per forzare    05
177200080704La  Info Comm. prevede l'inserimento del numero Spedizioni: enter per forzare  06
177300080704Info Comm. non più in essere:verificarla e se non serve annullarla.ENTER forza 07
177400080704Info Comm. non più in essere: ANNULLARLA !!                                    08
177500081003La somma delle percentuali per questa categoria di info commerc. supera 100%   09
177600081003La Info Comm. prevede l'inserimento del numero Spedizioni OBBLIGATORIO         10
177700081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx OBBLIGATORIO            11
177800081003La  percentuale di questa info commerciale supera 100%                         12
177900081003La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx  deve essere 100% 13
178000081007Inserire concorreti Logistica SOLO SE il cliente prevede Logist.in outsourcing 14
178100081007Se Logistica in outsourcing obbligatorio inserire i concorrenti logistica      15
178200110511"Motivo non affidamento a noi" da immettere solo se "Affidato a BRT" < 100%    16
178300110511Se non tutto "Affidato BRT" obbligatorio Motivo NON AFFIDAMENTO noi ITALIA     17
178400081007"MOTIVO NON AFFIDAMENTO A NOI" errato                                          18
178500081007Indicare OBBLIGATORIAMENTE la presenza (S) o assenza(N) della Info commerciale 19
178600081007Immettere solo  '?'  se si vuole attivare la ricerca                           20
178700081125Immettere "S o N" per indicare la presenza o l'assenza della Info Comm.        21
178800081007Immettere"S o N"per indicare la presenza o l'assenza della Info Comm:ENTR forza22
178900110511Se non tutto "Affidato BRT" obbligatorio Motivo NON AFFIDAMENTO noi ESTERO     23
179000081015Il cliente non prevede spesa trasporti Italia:non inserire il relativo MOTIVO  24
179100081015Il cliente non prevede spesa trasporti Estera:non inserire il relativo MOTIVO  25
179200081125Se "Logistica interessante per noi", indicare se in Outsourcing                26
179300110511Se non tutto "Affidato BRT" obbligatorio un Motivo NON AFFIDAMENTO a noi       27
179400081126I"Motivi non Affidamento a noi" devono essere diversi: uno per ITA uno per EST 28
179500081126Se immesso"9999-NON Rilevabile", l'altro motivo deve essere vuoto              29
179600081126Codice già presente !!                                                         30
179700120125Obbligatorio aumentare la % di affidamento a BRT!!                             31
179800120109Alcune Info sono più vecchie di 1 anno. Verificare se ancora corrette.         32
179900120125Obbligatorio azzerare la % di affidamento a BRT!!                              33
180000120125Obbligatorio diminiuire la % di affidamento a BRT!!                            34
