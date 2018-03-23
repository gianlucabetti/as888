000100080206      //--------------------------------------------------------------
000200080627      //?TRMK50R - Gestione/interrogazione informazioni commerciali
000300080206      //--------------------------------------------------------------
000400141001      //?  ATTENZIONE!!!!                                                      ?
000500141001      //?  Nella tabella IFO è previsto l'inserimento di un valore decimale    ?
000600141001      //?  sulle INFO, per ora questo flag viene usato SOLO per il ricavo      ?
000700141001      //?  medio spedizione calcolato in AUTOMATICO ed è una INFO non          ?
000800141001      //?  memorizzata sul file.                                               ?
000900141001      //?  In caso di necessità di INFO a valore con i decimali si dovrà       ?
001000141001      //?  variare il file TICPI00F per aggiungere un campo con i decimali.    ?
001100080206
001200080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001300080206
001400080206      //---------------------------------------------------------------
001500080206      //?Dichiarazione file.
001600080206      //---------------------------------------------------------------
001700050704
001800080627     FTICPI01L  UF A E           K DISK
001900080702     fTNCPO01L  if   e           k disk    infds(cpo01)
002000081003     f                                     rename(tncpo000:tncpo001)
002100081003     fTNCPO00f  uf   e             disk
002200140909     fTNCPO11L  uf a e           k disk
002300080206     fAZORG01L  if   e           k disk
002400080604     fTNTBE01l  if   e           k disk
002500081215     fTRMK50D   cf   e             workstn indds(IndDspF)
002600080206     f                                     infds(InfDspF)
002700080702     f                                     sfile(MK50S01 : S01nrr)
002800080206      //---------------------------------------------------------------
002900080207      // - Tasti funzionali a video
003000080207     d c_F01           c                   const(x'31')
003100080207     d c_F02           c                   const(x'32')
003200080207     d c_F03           c                   const(x'33')
003300080207     d c_F05           c                   const(x'35')
003400080207     d c_F06           c                   const(x'36')
003500080207     d c_F07           c                   const(x'37')
003600080207     d c_F08           c                   const(x'38')
003700080207     d c_F09           c                   const(x'39')
003800080207     d c_F10           c                   const(x'3A')
003900080207     d c_F12           c                   const(x'3C')
004000080207     d c_F13           c                   const(x'B1')
004100080207     d c_F14           c                   const(x'B2')
004200080207     d c_F15           c                   const(x'B3')
004300080207     d c_F16           c                   const(x'B4')
004400080207     d c_F17           c                   const(x'B5')
004500080207     d c_F18           c                   const(x'B6')
004600080207     d c_F19           c                   const(x'B7')
004700080207     d c_F20           c                   const(x'B8')
004800080207     d c_F21           c                   const(x'B9')
004900080207     d c_F22           c                   const(x'BA')
005000080207     d c_F23           c                   const(x'BB')
005100080207     d c_F24           c                   const(x'BC')
005200080207     d c_Enter         c                   const(x'F1')
005300080207     d c_RollDown      c                   const(x'F4')
005400080207     d c_RollUp        c                   const(x'F5')
005500080214
005600080214      // - Attributi di visualizzazione
005700080215      //  -  DspAtr() - Normale
005800080214     d C_dspatr_Norm   c                   const(x'20')
005900080215      //  -  DspAtr(RI)
006000080214     d C_dspatr_RI     c                   const(x'21')
006100080215      //  -  DspAtr(ND)
006200080214     d C_dspatr_ND     c                   const(x'27')
006300080215      //  -  DspAtr(BL) / Color(Red)
006400080214     d C_dspatr_BL     c                   const(x'28')
006500080206
006600080206      //---------------------------------------------------------------
006700080206      //?Definizione schiere.
006800080206      //---------------------------------------------------------------
006900080206
007000080206      // - Messaggi di errore
007100140718     d MSG             s             78    dim(50) ctdata perrcd(1)
007200140716
007300080702     d ifo             s              3    dim(50)
007400081015     d ifods           s             70    dim(50)
007500080627     d ifocg           s              2    dim(50)
007600080702     d ifoor           s              8    dim(50)
007700081126
007800081126     d IFOval          s              8    dim(100)
007900140716
008000080206      //---------------------------------------------------------------
008100080206      //?Definizione aree dati.
008200080206      //---------------------------------------------------------------
008300100226
008400080206      // - Dati utente
008500080206     d §AzUte        e ds                  extname(AZUTE00F)
008600080206     d                                     dtaara
008700080206     d §DatiUte      e ds                  extname(dDatiUte)
008800080206     d                                     dtaara
008900080206
009000080206      //---------------------------------------------------------------
009100080206      //?Definizione strutture dati.
009200080206      //---------------------------------------------------------------
009300080619     D CPO01           DS
009400080619     D  cp1NRR               397    400B 0
009500080206
009600080206      // - Status
009700080206     d Psds           sds
009800080206     d   SDSpgm          *proc
009900080206
010000080206      // - InfDS
010100080206     d InfDspF         ds
010200080207     d  dsp_aid              369    369a                                        AID byte
010300080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
010400080207     d  min_nrr              378    379i 0                                      Subfile min rrn
010500080207     d  num_rcds             380    381i 0                                      Subfile num rcds
010600080206
010700080206      // - Indicatori su DspF
010800080208     d IndDspF         ds
010900080206        // - Indicatori di gestione del subfile
011000080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011100080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011200080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011300080206     d  SflEnd                        1n   overlay(IndDspF : 33)
011400080206        // - Indicatori di errore
011500080206     d  errMessage                    1n   overlay(IndDspF : 28)
011600080606     d  errGenerico                   1n   overlay(IndDspF : 99)
011700080627        // - Indicatori vari
011800080627     d  InfoInt                       1n   overlay(IndDspF : 02)
011900080704     d  Infoyell                      1n   overlay(IndDspF : 03)
012000081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
012100120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
012200120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
012300080627     d  InfoImmiss                    1n   overlay(IndDspF : 10)
012400080703     d  ProtAnn                       1n   overlay(IndDspF : 13)
012500080703     d  Ifosott                       1n   overlay(IndDspF : 15)
012600080627     d  protriga                      1n   overlay(IndDspF : 16)
012700080627     d  protFat                       1n   overlay(IndDspF : 17)
012800081003     d  protVal                       1n   overlay(IndDspF : 18)
012900080627     d  protSped                      1n   overlay(IndDspF : 19)
013000080702     d  protfsn                       1n   overlay(IndDspF : 20)
013100080925     d  PFTBLU                        1n   overlay(IndDspF : 21)
013200140715     d  protVald                      1n   overlay(IndDspF : 24)
013300140715     d  protvala                      1n   overlay(IndDspF : 25)
013400080703     d  ErrSNA                        1n   overlay(IndDspF : 40)
013500080703     d  ErrFsn                        1n   overlay(IndDspF : 41)
013600081003     d  ErrVAL                        1n   overlay(IndDspF : 42)
013700140715     d  ErrVALD                       1n   overlay(IndDspF : 43)
013800080703     d  ErrPFT                        1n   overlay(IndDspF : 44)
013900080704     d  ErrANN                        1n   overlay(IndDspF : 45)
014000080206
014100080206      // - Parametri ricevuti
014200080206     d KPJBA         e ds
014300080702
014400080702     d TRMK50DS      e ds
014500100302     d TRMK52DS      e ds
014600080206
014700080206      // - Reperimento dati utente
014800080206     d TIBS34ds      e ds
014900080206     d dUte01        e ds
015000080702     d DLAT          e ds
015100080702
015200080702     d Dged          e ds
015300081006     d Difs          e ds
015400080206
015500080206      // - Ricerca/Controllo tabelle
015600080206     d TIBS02ds      e ds                  inz
015700081008      // - Ricerca sottipo info comm
015800081008     d TNTB70ds      e ds                  inz
015900160708
016000160708     d TRMK30DS      e ds
016100160708     d TNCPO_30      e ds                  extname(TNCPO00F) inz
016200160708     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
016300160708     d TICPI_30      e ds                  extname(TICPI00F) inz
016400080206
016500080627      // - Tabella  Info commerciali e Categorie info
016600080627     d dIFO          e ds                  inz
016700080627     d dIFG          e ds                  inz
016800081003     d
016900081003     d dcpo01        e ds                  inz
017000140725
017100080206      //---------------------------------------------------------------
017200080206      //?Definizione variabili globali.
017300080206      //---------------------------------------------------------------
017400080206
017500080206      // - Flags booleani
017600080208     d $Fine           s               n   inz(*off)
017700080208     d $InzS01         s               n   inz(*on)
017800120105     d wAumenta10      s               n   inz(*off)
017900120510     d wAumenta10f     s               n   inz(*off)
018000120110     d wAzzera10       s               n   inz(*off)
018100120105     d wDiminuisci10   s               n   inz(*off)
018200120109     d wForzaAgg       s               n   inz(*off)
018300140716     d wForzaRms       s               n   inz(*off)
018400140716     d wForzaPft       s               n   inz(*off)
018500140725     d wEstero         s               n   inz(*off)
018600140721     d wNews           s               n   inz(*off)
018700080206
018800080627     d $Video          s              2    inz('S1')
018900080208     d S01nrr          s              4  0 inz
019000080627     d savcat          s              2    inz
019100080627     d Wcat            s              2    inz
019200081006     d WIFS            s              4    inz
019300080702     d Wifo            s              3    inz
019400081006     d w0040           s              4  0 inz
019500081007     d w005a           s              5    inz
019600081006     d XX              s              3  0 inz
019700080627     d Indx            s              3  0 inz
019800080703     d Primavolta      s              1    inz
019900080925     d Totprc          s              4  0 inz
020000081003     d Wctrprc         s              1    inz
020100081003     d Wdescat         s             21    inz
020200081215     d SPTPFT          s                   like(cpipft) inz
020300100302     d savspt          s                   like(cpipft) inz
020400081006     d LOG40           s              1    inz
020500081006     d OUTFSN          s              1    inz
020600081006     d OUTnrr          s              4  0 inz
020700081023     d LOGFSN          s              1    inz
020800081023     d LOGnrr          s              4  0 inz
020900081126     d MOInrr          s              4  0 inz
021000081015     d Waggiocpo       s              1    inz
021100081007     d BART_10         s              4  0 inz
021200081015     d InfoITA         s              1    inz
021300081015     d InfoEST         s              1    inz
021400100226     d InfoATR         s                   like(cpival)
021500100302     d SavATR          s                   like(cpival)
021600081126     d w008a           s              8    inz
021700081126     d MOIconta        s              3  0 inz
021800081126     d yy              s              3  0 inz
021900081126     d trovacpi        s              1    inz
022000081215     d savmod          s                   like(i50mod) inz
022100090520     d ultdata         s              8  0 inz
022200120110     d sav_bar_10      s                   like(bart_10)
022300120109     d wOggi           s              8  0 inz
022400140721
022500140715     d sav_vscpft      s                   like(vscpft) inz
022600140715     d sav_vscsna      s                   like(vscsna) inz
022700140716     d sav_valrms      s             12  3 inz
022800140716     d wVALrms         s             12  3 inz
022900140718     d noBRTpft        s                   like(cpipft) inz
023000140721     d siBRTpft        s                   like(cpipft) inz
023100140718     d ifoNRFnrr       s                   like(S01nrr) inz
023200140721     d ifoNROnrr       s                   like(S01nrr) inz
023300140721     d ifoNERnrr       s                   like(S01nrr) inz
023400140721     d ifoNAEnrr       s                   like(S01nrr) inz
023500140725     d ifoNTRnrr       s                   like(S01nrr) inz
023600140721     d savPRFval       s                   like(cpival) inz
023700140721     d savPROval       s                   like(cpival) inz
023800140721     d savAERval       s                   like(cpival) inz
023900140721     d savCAEval       s                   like(cpival) inz
024000140725     d savATRval       s                   like(cpival) inz
024100140721     d savNRFpft       s                   like(cpipft) inz
024200140721     d savNROpft       s                   like(cpipft) inz
024300140721     d savNERpft       s                   like(cpipft) inz
024400140721     d savNAEpft       s                   like(cpipft) inz
024500140725     d savNTRpft       s                   like(cpipft) inz
024600080414
024700080605     d Dataiso         s               d   datfmt(*iso)
024800080605     d Dataymd         s               d   datfmt(*ymd)
024900080605     d Datadmy         s               d   datfmt(*dmy)
025000120109     d wIFOdul         s              8  0
025100120109     d wIFOduliso      s               d   datfmt(*ISO)
025200140725     d wData           s              8  0
025300140725     d Dataeur         s               d   datfmt(*eur)
025400080604     d
025500080605     d                 DS
025600080605     d  AA                     1      4  0
025700080605     d  MM                     5      6  0
025800080605     d  GG                     7      8  0
025900080605     d DATA                    1      8  0
026000081009     d
026100081126     d CoRicercaMOI    c                   const('Ricerca motivo ITA/EST''?''')
026200081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
026300080206
026400080206      //---------------------------------------------------------------
026500080206      //?Definizione procedure usate.
026600080206      //---------------------------------------------------------------
026700080414      /copy gaitrasrc/srcprotopr,tibs02r
026800080414      /copy gaitrasrc/srcprotopr,tibs34r
026900081008      /copy gaitrasrc/srcprotopr,TNTB70r
027000081023      /copy gaitrasrc/srcprotopr,TRMK51r
027100100302      /copy gaitrasrc/srcprotopr,TRMK52r
027200080414      /copy gaitrasrc/srcprotopr,trmk_opm
027300160708
027400160708     d TRMK30R         pr                  extpgm('TRMK30R')
027500160708     d  trmk30ds                           likeds(trmk30ds)
027600160708     d  tncpo_30                           likeds(tncpo_30)
027700160708     d  tncpo1_30                          likeds(tncpo1_30)
027800160708     d  ticpi_30                           likeds(ticpi_30)
027900080206
028000080206      //---------------------------------------------------------------
028100080206      //?Riepilogo indicatori.
028200080206      //---------------------------------------------------------------
028300080207      // - Comuni
028400080207      // 28    : Emissione messaggio di errore a video
028500080207      // - C01/S01
028600080207      // 30    : Condiziona SFLDSP    (*not)
028700080207      // 31    : Condiziona SFLDSPCTL (*not)
028800080207      // 30+31 : Condiziona SFLCLR
028900080207      // 32    : Condiziona SFLNXTCHG in S01
029000080207      // 50    : Errore: Opzione errata
029100080207      // - D01
029200080207      // - Comuni
029300080207      // 99    : Generico di Errore
029400080206      //---------------------------------------------------------------
029500080206
029600080206      //---------------------------------------------------------------
029700080206      //?M A I N - L I N E
029800080206      //---------------------------------------------------------------
029900080206
030000080627     c     *Entry        plist
030100080206     c                   parm                    KPJBA
030200080627     C                   PARM                    trMK50ds
030300080627
030400080627     C                   CLEAR                   O50F12
030500080627     C                   CLEAR                   O50F03
030600080627     C                   CLEAR                   O50F06
030700080702
030800080206      /free
030900080206
031000081215 1      if  i50tla<>'C'   ;
031100080925
031200080206       // Operazioni iniziali
031300080206       exsr RoutInz;
031400080206
031500081215 2      if  i50mod<>'C'   ;
031600081215
031700080206       // Gestione video
031800081215 3     DOW $Fine = *off;
031900081215 4       select;
032000080530
032100080530         // Videata di selezioni
032200080627           when $Video = 'S1';
032300080627             exsr GesS01;
032400120109           when $Video = 'W1';
032500120109             exsr GesW01;
032600080530
032700080702           other   ;
032800080206             $Fine = *on;
032900081215 4       endsl;
033000081215 3     ENDDO;
033100081215
033200081215 x2    else   ;
033300081215
033400081215       // modalità CONTROLLO : carico il sfl
033500081215       //                    solo ai fini del controllo
033600081215       exsr InzS01;
033700081215       exsr Contrs01   ;
033800081215
033900081215  3    if ErrGenerico=*off;
034000081215          o50ifotot='S';
034100081215  3    endif  ;
034200080925
034300081215 2     endif    ;
034400081215 1     endif    ;
034500080206
034600080206       // Operazioni finali
034700080206       exsr RoutEnd;
034800080206
034900080206       //--------------------------------------------------------------
035000080206       //?Operazioni iniziali.
035100080206       //--------------------------------------------------------------
035200080206       BEGSR RoutInz;
035300080627       $inzs01=*on;
035400080929       clear vsctes2         ;
035500080206
035600080703       // Solo la prima volta
035700080703 1     if primavolta=' '   ;
035800080703
035900080206         // Reperimento dati job
036000080206         exsr DatiJob;
036100081003
036200081003         // Se sono EDP accendo indicatore
036300081003         if %subst(knmus:1:3)<>'EDP'    ;
036400081003         UTENonEDP=*on   ;
036500081003         endif      ;
036600080627
036700080627         // Carico tabella per divisa corrente
036800080627         reset TIBS02ds;
036900080627         T02sif = knsif;
037000080627         T02cod = 'GED';
037100080627         T02ke1 = '1'  ;
037200080627         T02mod = 'C'  ;
037300080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037400080627
037500080703 2       if t02err=' '  ;
037600080627         dGED=t02uni    ;
037700080627         else   ;
037800080627         clear DGED    ;
037900080703 2       endif   ;
038000080627
038100080627         // Carico tabella info commerciali
038200080627         xx=0                     ;
038300080627         setll ('IFO') tntbe01l   ;
038400080627         READE ('IFO') tntbe01l   ;
038500080627
038600080703 2       dow not %eof(tntbe01l)   ;
038700080703 3       if tbeatb=' '    ;
038800080627           difo=tbeuni    ;
038900080627           xx=xx+1        ;
039000080703           ifo(xx)=tbeke1 ;
039100080627           ifods(xx)=difo ;
039200080627           ifocg(xx)=§ifoifg ;
039300080627           // ordino in base alla categoria e  num.ordinamento
039400080703           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
039500080703 3       endif               ;
039600080627
039700080627         READE ('IFO') tntbe01l   ;
039800080703 2       enddo    ;
039900080627
040000080627         sorta  IFOOR  ;
040100080703
040200080703         Primavolta='N'   ;
040300080703 1       endif      ;
040400080702
040500080703         Infoint=*off   ;
040600080704         Infoyell=*off   ;
040700080703         InfoImmiss=*off   ;
040800080703         Ifosott=*off   ;
040900080703         Protriga=*off   ;
041000080703         ProtFat =*off   ;
041100081003         ProtVal  =*off   ;
041200140715         ProtVald = *off;
041300140715         ProtVala = *off;
041400080703         Protsped =*off   ;
041500080703         Protfsn  =*off   ;
041600080703         clear wcat  ;
041700080703         clear wifo  ;
041800080704         $Fine=*off   ;
041900140929         clear wVALrms;
042000080703
042100080702         // Vedo se modo gestione  o interrogazione
042200080703 1       if i50mod='I' ;
042300080702         InfoInt =*on  ;
042400080703         vsctes2='INTERROGAZIONE'    ;
042500080703 1       endif;
042600080703
042700080703         if i50pgm=*blanks   ;
042800080703         vscpgm='TRMK50R'   ;
042900080703         else     ;
043000080703         vscpgm=i50pgm     ;
043100080703         endif   ;
043200080703
043300080703
043400080703         // IMPOSTO IL potenziale in visualizzazione
043500080703         vsccpo=%editc(i50cpo:'X')   ;
043600080703         vscrag=i50rag   ;
043700120111
043800120111         //?Attivo i tasti funzione F03 e F12
043900120111           F03Attivo = *on;
044000120111           F12Attivo = *on;
044100080703
044200120105         //?05/12/2012
044300120105         //?Visto che il campo per potenziale annullato non è più usato
044400120105         //?lo recuperiamo per utilizzarlo come flag per controlli da
044500120105         //?fare sulle info.
044600120110         //?Nessun controllo
044700120110           IF  I50atb = ' ';
044800120110             wAumenta10    = *off;
044900120510             wAumenta10f   = *off;
045000120110             wAzzera10     = *off;
045100120110             wDiminuisci10 = *off;
045200120110           ENDIF;
045300120110         //?Controllo per azzeramento
045400120110           IF  I50atb = '0';
045500120110             wAzzera10     = *on;
045600120110             wAumenta10    = *off;
045700120510             wAumenta10f   = *off;
045800120110             wDiminuisci10 = *off;
045900120111             F03Attivo = *off;
046000120111             F12Attivo = *off;
046100120110           ENDIF;
046200120110         //?Controllo per aumento
046300120105           IF  I50atb = '+';
046400120110             wAumenta10    = *on;
046500120510             wAumenta10f   = *off;
046600120110             wAzzera10     = *off;
046700120110             wDiminuisci10 = *off;
046800120111             F03Attivo = *off;
046900120111             F12Attivo = *off;
047000120105           ENDIF;
047100120510         //?Controllo per aumento semi obbligatorio
047200120510           IF  I50atb = '%';
047300120510             wAumenta10f   = *on;
047400120510             wAumenta10    = *off;
047500120510             wAzzera10     = *off;
047600120510             wDiminuisci10 = *off;
047700120510             F03Attivo = *off;
047800120510             F12Attivo = *off;
047900120510           ENDIF;
048000120110         //?Controllo per diminuzione
048100120109           IF  I50atb = '-';
048200120110             wDiminuisci10 = *on;
048300120110             wAumenta10    = *off;
048400120510             wAumenta10f   = *off;
048500120110             wAzzera10     = *off;
048600120111             F03Attivo = *off;
048700120111             F12Attivo = *off;
048800120105           ENDIF;
048900081003
049000081003         // Se già  inserite tutte le info, non si possono più annulla
049100081003         //  ma solo sistemare
049200081003         if i50obl=' '  and i50ifotot='S'    ;
049300081003         i50obl='S'   ;
049400081003         endif        ;
049500080627
049600080703         // VERIFICO SE ci sono già dei dati per il potenziale
049700080703         setll   i50cpo  ticpi01l    ;
049800080703         reade   i50cpo  ticpi01l    ;
049900080703 1       dow not %eof(ticpi01l) and cpiatb<>' ' ;
050000080703         reade   i50cpo  ticpi01l    ;
050100080703 1       enddo   ;
050200080703
050300080703 1       if %eof(ticpi01l) ;
050400080703         Infoimmiss=*on     ;
050500080929 1       if vsctes2= *blanks    ;
050600080703         vsctes2='  IMMISSIONE  '    ;
050700080929         endif   ;
050800080703         else    ;
050900080929 1       if vsctes2= *blanks    ;
051000080703         vsctes2='  VARIAZIONE  '    ;
051100080703 1       endif    ;
051200080929 1       endif    ;
051300080703
051400080627         ENDSR;
051500080206
051600080206       //--------------------------------------------------------------
051700080206       //?Reperimento Dati del job (Utente/Operativi).
051800080206       //--------------------------------------------------------------
051900080206       BEGSR DatiJob;
052000080206
052100080206         in(E) §AzUte;
052200080206         if NOT %error;
052300080206           in(E) §DatiUte;
052400080206         endif;
052500080206         if %error or RSut = *blanks;
052600080206           clear TIBS34ds;
052700080206           tibs34r(tibs34ds);
052800080206           in §AzUte;
052900080206           in §DatiUte;
053000080206         endif;
053100080206
053200080206       ENDSR;
053300080206
053400080206       //--------------------------------------------------------------
053500080627       //?Gestione SFL 01
053600080206       //--------------------------------------------------------------
053700080409       BEGSR GesS01;
053800080207
053900080207         // Inizializzazione videata
054000080409         if  $InzS01 = *on;
054100080409            exsr InzS01;
054200080703         // Posizionamento cursore
054300080703           s01nrr = 1;
054400080703           sfldsp_n=*off;
054500080409            $InzS01  = *off;
054600080207         endif;
054700080207
054800140725         //?Cerco l'ultima data di conferma info commerciali
054900140909           chain(n) I50cpo TNCPO11L;
055000140909           IF  %found(TNCPO11L);
055100140909             vdulconf = CPO1ducifo;
055200140909           ENDIF;
055300140909           IF  vdulconf > *zeros;
055400140909             dataISO = %date(vdulconf:*iso);
055500140909             dataEUR = dataISO;
055600140909             vdulconf = %dec(dataEUR);
055700140909           ENDIF;
055800160708         //?Se richiamto pgm per variazione/immissione salvo l'immagine prima
055900160708           IF  I50mod <> 'I';
056000160708             exsr ImmaginePrima;
056100160708           ENDIF;
056200080207
056300080207         // Emissione Testata e Piede con tasti funzionali abilitati
056400080207         if  errGenerico = *off;
056500080702           write  MK50Z01;
056600080207         endif;
056700080703
056800080703         // Posizionamento cursore
056900080703         if  C01csr  > *zero;
057000080703           C01rcd = C01csr;
057100080703         endif  ;
057200080207
057300080207         // Emissione videata
057400080702         exfmt  MK50C01;
057500080410
057600080207         reset errMessage;
057700080207         reset errGenerico;
057800080702         clear Vscmsg;
057900081003         errVAL=*off     ;
058000140715         errVALD = *off;
058100080703         errPFT=*off     ;
058200080703         errSNA=*off     ;
058300080703         errFSN=*off     ;
058400080704         errANN=*off     ;
058500120109
058600120109         //?Cerco l'ultima data variazione info commerciali
058700120124         //?Se NON è interrogazione e non è immissione
058800120109           clear dCPO01;
058900120124           IF  not InfoInt and not Infoimmiss;
059000120109             chain(n) I50cpo TNCPO01L;
059100120109             IF  %found(TNCPO01L);
059200120109               dCPO01 = CPOrst;
059300120109             ENDIF;
059400120109             IF  §CPOifodul > *zeros;
059500120109               wIFOdul    = %dec(§CPOifodul:8:0);
059600120109               wIFOdulISO = %date(wIFOdul : *ISO);
059700120109               wOggi = %dec(%date());
059800120109               dataISO = %date(wOggi : *ISO);
059900120109         //?Se la data ultima variazione è più vecchia di 1 anno da oggi windows
060000120109         //?di avviso all'utente
060100120110               IF  %diff(dataISO : wIFOdulISO : *months) > 12 and not wForzaAgg;
060200120109                 $Video = 'W1';
060300120109                 ErrGenerico = *on;
060400120109                 ErrMessage  = *on;
060500120109                 VSCmsg      = msg(32);
060600120109                 wForzaAgg   = *on;
060700120109                 leavesr;
060800120109               ENDIF;
060900120109             ENDIF;
061000120109           ENDIF;
061100080207
061200080523 1       SELECT;
061300080207
061400080207         // - F3=Fine
061500080523 1         WHEN  dsp_aid = c_F03;
061600080409            $Fine = *on;
061700081006            o50f03='S'   ;
061800080207
061900080207         // - F12=Ritorno
062000080523 1         WHEN  dsp_aid = c_F12;
062100080702            $Fine = *on;
062200081006            o50f12='S'   ;
062300080414
062400080530 x1      // Invio / F6
062500080207           OTHER;
062600081006
062700081006        // Non effettuo controlli in interrogazione
062800081006 1           if i50mod<>'I' ;
062900081006               exsr ContrS01 ;
063000081006 2             if  errGenerico = *on;
063100081006                 leavesr;
063200081006 2             endif;
063300081006 2           endif;
063400080530
063500081023         // F6=conferma Aggiornamento  F10=anche stampa
063600081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
063700080702           exsr   ConfAggio ;
063800081006           o50f06='S'   ;
063900081023
064000081023              if dsp_aid=c_F10;
064100081023               kpjbu=trmk50ds  ;
064200081023               callp TRMK51R (kpjba)   ;
064300081023              endif    ;
064400081023
064500080704           $Fine = *on;
064600080530           endif  ;
064700080207
064800080523 1       ENDSL;
064900080207
065000080207       ENDSR;
065100120109
065200120109       //--------------------------------------------------------------
065300120109       //?Gestione Window errori
065400120109       //--------------------------------------------------------------
065500120109       BEGSR GesW01;
065600120109
065700120109         reset ErrMessage;
065800120109         reset ErrGenerico;
065900120109
066000120109         w1riga = vscmsg;
066100120109         exfmt  MK50W01;
066200120109
066300120109         clear Vscmsg;
066400120109         errVAL  = *off;
066500140715         errVALD = *off;
066600120109         errPFT  = *off;
066700120109         errSNA  = *off;
066800120109         errFSN  = *off;
066900120109         errANN  = *off;
067000120109
067100120109         $Video = 'S1';
067200120109
067300120109       ENDSR;
067400080207
067500080526       //--------------------------------------------------------------
067600080702       //?controlli SFL01
067700080409       //--------------------------------------------------------------
067800080409       BEGSR ContrS01;
067900140716
068000081215       clear o50ifomin;
068100081215       clear wctrprc  ;
068200081006       clear wdescat   ;
068300081006       clear totprc   ;
068400081006       clear sptpft   ;
068500081006       clear OUTFSN   ;
068600081006       clear OUTnrr   ;
068700081023       clear LOGFSN   ;
068800081023       clear LOGnrr   ;
068900081006       clear LOG40    ;
069000081007       clear bart_10  ;
069100081015       clear infoITA  ;
069200081015       clear infoEST  ;
069300081126       clear IFOval   ;
069400081126       clear MOIconta ;
069500081126       clear MOInrr   ;
069600100226       clear InfoATR  ;
069700140718       clear noBRTpft;
069800140721       clear siBRTpft;
069900140721       clear savPRFval;
070000140721       clear savPROval;
070100140721       clear savAERval;
070200140721       clear savCAEval;
070300140725       clear savATRval;
070400140721       clear savNRFpft;
070500140721       clear savNROpft;
070600140721       clear savNERpft;
070700140721       clear savNAEpft;
070800140725       clear savNTRpft;
070900140725       wEstero = *off;
071000140716
071100081126       yy=1           ;
071200080703       s01nrr=1  ;
071300080703       chain s01nrr    mk50s01    ;
071400080703
071500081006 0     dow %found    ;
071600080925       // Escludo i record di descrizione categoria
071700081006 1     if vshimm= 'C'    ;
071800081003
071900081003       // A cambio di categoria, se c'era controllo obbligatorio del
072000081003       //  100%   controllo ed eventualmente segnalo errore
072100081006       EXSR CambioCAT    ;
072200081006       if errGenerico=*on  ;
072300081006       leavesr;
072400081006       endif              ;
072500081006
072600081006x1     else    ;
072700081006
072800080925       pftblu=*off       ;
072900080925
073000081003       // se valori calcolati da pgm li pulisco prima dei controlli
073100081007 2     if vshsnf=' ' and vshprpft=' '   ;
073200080925       clear vscpft   ;
073300140717       // clear vscvft   ;
073400081007 2     endif          ;
073500080703
073600080703       // Imposto la divisa, se impostato il fatturato
073700140717 2     //if vscpft>0 and vscvft=*blanks   ;
073800140717       //vscvft=§gedcr   ;
073900140717       //errGenerico = *on;
074000140717 2     //endif    ;
074100080703
074200140717 2     //if vscpft=0 and vscvft<>*blanks   ;
074300140717       //clear vscvft   ;
074400140717       //errGenerico = *on;
074500140717 2     //endif   ;
074600140716
074700140716       //?Se valore fatturato < 0 errore
074800140716       IF  VSCpft < 0;
074900140716         VSCmsg = msg(25);
075000140716         errPFT=*on;
075100140716         EXSR AggioSFL   ;
075200140716         leavesr;
075300140716       ENDIF;
075400140716
075500140716       //?Se valore fatturato impostato e > di 999000000 errore
075600140716       IF  VSCpft > 999000000;
075700140716         VSCmsg = msg(17);
075800140716         errPFT=*on;
075900140716         EXSR AggioSFL   ;
076000140716         leavesr;
076100140716       ENDIF;
076200140716
076300140716       //?Se valore fatturato impostato e > di 999999 warning
076400140716       IF  VSCpft > 999999 and not wForzaPft;
076500140716         VSCmsg = msg(23);
076600140716         errPFT=*on;
076700140716         wForzaPft = *on;
076800140716         EXSR AggioSFL   ;
076900140716         leavesr;
077000140716       ENDIF;
077100140716
077200140716       //?Le spedizioni non possono essere superiori del valore fatturato
077300140722       IF  VSCsna > VSCpft and VSCifo = 'SPT';
077400140716         VSCmsg = msg(24);
077500140716         errPFT=*on;
077600140716         EXSR AggioSFL   ;
077700140716         leavesr;
077800140716       ENDIF;
077900140716
078000140716       //?Se spedizioni < 0 errore
078100140716       IF  VSCsna < 0;
078200140716         VSCmsg = msg(25);
078300140716         errSNA=*on;
078400140716         EXSR AggioSFL   ;
078500140716         leavesr;
078600140716       ENDIF;
078700140723
078800140723       //?Se VAL < 0 errore
078900140723       IF  vscval < 0;
079000140723         VSCmsg = msg(25);
079100140723         errVAL=*on;
079200140723         EXSR AggioSFL   ;
079300140723         leavesr;
079400140723       ENDIF;
079500140723
079600140723       //?Se VALD < 0 errore
079700140723       IF  vscvald < 0;
079800140723         VSCmsg = msg(25);
079900140723         errVALD=*on;
080000140723         EXSR AggioSFL   ;
080100140723         leavesr;
080200140723       ENDIF;
080300080703
080400080704       // se info da annullare e non c'e' la "A", msg forzabile
080500081215       //  non faccio se modalità controlli
080600081215 2     if i50mod<>'C' and vshann='S'  and vscatb=' '  ;
080700080704           errANN=*on;
080800080704           // se  contiene dati, msg forzabile
080900081007 3         if vscfsn='S'or vscval>0 or vscpft>0 or vscsna>0   ;
081000080704           vscmsg = Msg(07);
081100080704           vshann='X'      ;
081200080704           // Se non contiene dati abbligatorio l'annullamento
081300081007 x3        else            ;
081400080704           vscmsg = Msg(08);
081500081007 3         endif           ;
081600080704           EXSR AggioSFL   ;
081700080704           leavesr;
081800081007 2     endif    ;
081900110707
082000110707       // se c'e' "O"  obbligatorio l'annullamento
082100110707 2     if i50mod<>'C' and vshann='O'  and vscatb=' '  ;
082200110707           errANN=*on;
082300110707           vscmsg = Msg(08);
082400110707           EXSR AggioSFL   ;
082500110707           leavesr;
082600110707 2     endif    ;
082700080704
082800080703       // Se richiesto inserimento obbligatorio, per i dati
082900080703       //  obbligatori da tabella segnalo errore
083000081007 2     if i50obl='S' and vshobl='S'                 ;
083100081007 3     if vscatb='A'    or
083200081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
083300081003         if vshsne<>' ';
083400080703           errFSN=*on;
083500081003         endif   ;
083600081003         if vshsnv<>' ';
083700081003           errVAL=*on;
083800081003         endif   ;
083900140715         IF  vshsnvd <> *blanks;
084000140715           errVALD = *on;
084100140715         ENDIF;
084200081003         if vshsnf<>' ';
084300081003           errPFT=*on;
084400081003         endif   ;
084500080703           vscmsg = Msg(01);
084600080703           EXSR AggioSFL   ;
084700080703           leavesr;
084800081007 3     endif   ;
084900081007 2     endif   ;
085000081007
085100081007       // Non accetto se non previsto il valore '?'
085200081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
085300081007           errFSN=*on;
085400081007           vscmsg = Msg(20);
085500081007           EXSR AggioSFL   ;
085600081007           leavesr;
085700081007 3     endif    ;
085800081007       if vshifgv<>'?' and vscfsn='?'  ;
085900081007           errFSN=*on;
086000081007           vscmsg = Msg(21);
086100081007           EXSR AggioSFL   ;
086200081007           leavesr;
086300081007 3     endif    ;
086400080703
086500080703       // Se immesso "N" non indicare gli altri campi
086600081007 2     if vscfsn='N' and (vscval>0 or vscpft>0 or vscsna>0)   ;
086700080703       if vshsnf<>' '    ;
086800080703           errPFT=*on;
086900080703       else    ;
087000081003          errVAL=*on ;
087100080703       endif  ;
087200080703           vscmsg = Msg(02);
087300080703           EXSR AggioSFL   ;
087400080703           leavesr;
087500081007 2     endif   ;
087600080703
087700081003       // Se immesso almeno un dato, controllo gli obbligatori ed
087800081215       //   i facoltativi. I  facoltativi non li guardo in modalità
087900081215       //   solo controllo
088000081031       if vscatb =' ' and
088100081031 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval>0)            ;
088200081003
088300081007 3     if vscpft=0    and vshsnf='O'   ;
088400081003           errPFT=*on;
088500081003           vscmsg = Msg(03);
088600081003           EXSR AggioSFL   ;
088700081003           leavesr;
088800081007 3      endif    ;
088900100503
089000100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
089100100503 3     if i50obl='S' and
089200100503          I50mod<>'C' and  vscpft=0    and vshsnf='S'   ;
089300081003           errPFT=*on;
089400081003           vscmsg = Msg(05);
089500081003           vshsnf='X'      ;
089600081003           EXSR AggioSFL   ;
089700081003           leavesr;
089800081007 3     endif    ;
089900081007 3     if vscsna=0    and vshsns='O'   ;
090000081003           errSNA=*on;
090100081003           vscmsg = Msg(10);
090200081003           EXSR AggioSFL   ;
090300081003           leavesr;
090400081007 3     endif    ;
090500100503
090600100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
090700100503 3     if i50obl='S' and
090800100503          i50mod<>'C' and vscsna=0    and vshsns='S'   ;
090900081003           errSNA=*on;
091000081003           vscmsg = Msg(06);
091100081003           vshsns='X'      ;
091200081003           EXSR AggioSFL   ;
091300081003           leavesr;
091400081007 3     endif    ;
091500081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
091600081007           errFSN=*on;
091700081007           vscmsg = Msg(19);
091800081007           EXSR AggioSFL   ;
091900081007           leavesr;
092000081007 3     endif    ;
092100100503 3     if i50obl='S' and
092200100503 3        i50mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
092300081007           errFSN=*on;
092400081007           vscmsg = Msg(22);
092500081007           vshsne='X'      ;
092600081007           EXSR AggioSFL   ;
092700081007           leavesr;
092800081007 3     endif    ;
092900081007 3     if (vscval=0    and vshsnv='O')  or
093000100503          (vscval=0    and vshsnv='S' and i50mod<>'C'AND I50OBL='S')   ;
093100081007 4        if vshsnv='O'    ;
093200081003           vscmsg = Msg(11);
093300081003          else   ;
093400081215             vscmsg = Msg(04);
093500081215             vshsnv='X'      ;
093600081215 4        endif  ;
093700081007 4        if vsctva='%  '   ;
093800081003          %subst(vscmsg:37:18)='della % fatturato '     ;
093900081007 4        endif     ;
094000081007 4        if vsctva='KG '   ;
094100081003          %subst(vscmsg:37:18)='del peso in Kg.   '     ;
094200081007 4        endif     ;
094300081215
094400081003           errVAL=*on;
094500081003           EXSR AggioSFL   ;
094600081003           leavesr;
094700081215 3     endif    ;
094800140721
094900140715 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
095000140715           (vscvald = 0 and vshsnvd = 'S'   and
095100140715            I50mod <> 'C' and I50obl = 'S');
095200140715 4        IF  vshsnvd = 'O';
095300140715            vscmsg = Msg(01);
095400140715          ELSE;
095500140715            vscmsg = Msg(01);
095600140715            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
095700140715            vshsnvd = 'X';
095800140715 4        ENDIF;
095900140715          errVALD = *on;
096000140715          EXSR AggioSFL;
096100140715          leavesr;
096200140715 3     ENDIF;
096300140715
096400140715       //?Mi salvo il valore del traffico totale
096500140715 3     IF  vscifo = 'SPT';
096600140716         sav_vscpft = vscpft;
096700140716         sav_vscsna = vscsna;
096800140715       ENDIF;
096900081003
097000081003       // se Valore in % --> non deve superare 100%
097100081007 3     if  vscval>0 and vsctva='%  '    ;
097200081007 4       if vscval>100    ;
097300081003          errVAL=*on ;
097400081003          vscmsg = Msg(12);
097500081003          EXSR AggioSFL   ;
097600081003          leavesr;
097700081007 4       endif      ;
097800081003
097900080925       // Sommo la percentuale se presente per ogni categoria
098000080925       //  se supera 100% segnalo errore
098100081006       //  solo se previsto controllo sulla %
098200081007 4     if wctrprc='P'     ;
098300081006
098400110707       // Non sommo le info annullate perchè non le devo considerare valide
098500110707       if vshann=' '   ;
098600110707         totprc=totprc+vscval    ;
098700110707       endif  ;
098800110707
098900081007 5       if totprc>100    ;
099000081003         errVAL=*on ;
099100080925         vscmsg = Msg(09);
099200080925         EXSR AggioSFL   ;
099300080925         leavesr;
099400081007 5       endif      ;
099500081007 4     endif      ;
099600080925
099700081003       // Se immessa la %, visualizzo in base a SPT
099800080925       exsr   VISUALimp     ;
099900081007 3     endif      ;
100000081007 2     endif      ;
100100140716
100200140716       //?Se Valore con decimali in % --> non deve superare 100%
100300140716 3     IF  vscvald <> 0 and vsctva = '%  ';
100400140716 4       IF  vscvald > 100;
100500140716           errVALD = *on;
100600140716           vscmsg = Msg(12);
100700140716           EXSR AggioSFL   ;
100800140716           leavesr;
100900140716 4       ENDIF;
101000140716 4     ENDIF;
101100140715
101200140715       //?Per Ricavo medio spedizione
101300140715 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
101400140715       //?calcolo il valore
101500140715 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
101600140715           wVALrms = sav_vscpft / sav_vscsna;
101700140715         ELSE;
101800140715           wVALrms = 0;
101900140715         ENDIF;
102000140715       //?reimposto il flag di forzatura
102100140715         IF  wVALrms <> sav_valrms;
102200140716           wForzaRms = *off;
102300140715         ENDIF;
102400140715       //?imposto il campo del video
102500140919         IF  wVALrms > 999;
102600140919           vscvald = 999;
102700140715         ELSE;
102800140715           vscvald = wVALrms;
102900140715         ENDIF;
103000140715       //?msg info se inferiore a 4
103100140716         IF  VSCvald < 4 and not wForzaRms and
103200140715             wVALrms <> sav_valrms;
103300140715           errVALD = *on;
103400140715           vscmsg = msg(35);
103500140715           exsr AggioSFL;
103600140716           wForzaRms = *on;
103700140715           sav_valrms = wVALrms;
103800140715           leavesr;
103900140715         ENDIF;
104000140715       //?msg info se superiore a 50
104100140716         IF  VSCvald > 50 and not wForzaRms and
104200140715             wVALrms <> sav_valrms;
104300140715           errVALD = *on;
104400140715           vscmsg = msg(36);
104500140715           exsr AggioSFL;
104600140716           wForzaRms = *on;
104700140715           sav_valrms = wVALrms;
104800140715           leavesr;
104900140715         ENDIF;
105000140715       ENDIF;
105100140716
105200140716       // se INFO obbligatoria
105300140716       // errore se info non inserita
105400140721       // flag di comodo per non richiedere obbligatoria la INFO ora in fase di test
105500140721       // non so ancora se è obbligatoria sempre o solo se I50obl = 'S'
105600140721       IF  VSHobl = 'S' and wnews = *on;
105700140716         SELECT;
105800140716           WHEN  vshsnv <> *blanks and vscval = 0;
105900140716             errVAL = *on;
106000140716             vscmsg = Msg(01);
106100140716             EXSR AggioSFL;
106200140716             leavesr;
106300140716           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
106400140716             errVALD = *on;
106500140716             vscmsg = Msg(01);
106600140716             EXSR AggioSFL;
106700140716             leavesr;
106800140716           WHEN  vshsnf <> *blanks and vscpft = 0;
106900140716             errPFT = *on;
107000140716             vscmsg = Msg(01);
107100140716             EXSR AggioSFL;
107200140716             leavesr;
107300140716           WHEN  vshsns <> *blanks and vscsna = 0;
107400140716             errSNA = *on;
107500140716             vscmsg = Msg(01);
107600140716             EXSR AggioSFL;
107700140716             leavesr;
107800140716           WHEN  vshsne <> *blanks and vscfsn = *blanks;
107900140716             errFSN = *on;
108000140716             vscmsg = Msg(01);
108100140716             EXSR AggioSFL;
108200140716             leavesr;
108300140716         ENDSL;
108400140716       ENDIF;
108500140725
108600140725       // Paesei esteri ammessi solo se inserita almeno 1 % di ripartizione estera
108700140725       IF  VSCifo = '50 ' and VSCatb = *blanks and
108800140725           VSCfsn <> *blanks and not wEstero;
108900140725         errFSN = *on;
109000140725         VSCmsg = Msg(37);
109100140725         EXSR AggioSFL;
109200140725         leavesr;
109300140725       ENDIF;
109400081006
109500081006       // Concorrenti logistica ammessi solo se outsourcing=S
109600110707 2     if vscifo='40 '  and vscatb=' ' and (vscfsn<>' ' or vscpft>0 or vscsna>0
109700110707                            or vscval>0) ;
109800081006       if outfsn<>'S'   ;
109900081006         errFSN=*on ;
110000081006         vscmsg = Msg(14);
110100081006         EXSR AggioSFL   ;
110200081006         leavesr;
110300081006       endif             ;
110400081007
110500081006       // Memorizzo presenza info 40  se immesso Si o presente altro
110600081006       //  importo
110700110707         if vshann=' ' and vscfsn<>'N';
110800081007         LOG40='S'         ;
110900081007         endif             ;
111000081007 2     endif             ;
111100081008
111200081126       // Sottotipo col '?' --> Se '?' richiamo pgm di ricerca
111300081126 2     if vscfsn='?' ;
111400081008       clear tntb70ds ;
111500081008       itb70cpo=i50cpo;
111600081023       itb70rag=i50rag ;
111700081126       itb70tpf=vscifo  ;
111800081008       itb70pgm=i50pgm  ;
111900081008       callp TNTB70R (kpjba:tntb70ds)  ;
112000081008       clear vscfsn       ;
112100081126 3     if otb70spf>*zeros    ;
112200081008         vscspp=otb70spf  ;
112300081008         vsdifo=otb70dspf ;
112400081008         vscval=%int(otb70spf) ;
112500081126 3     endif         ;
112600081008       c01csr=s01nrr    ;
112700081008       exsr AggioSFL    ;
112800081008       leavesr       ;
112900081126 2     endif         ;
113000081007
113100081126 2     if vscval=0    and vshifgv='?' ;
113200081007       clear vscspp      ;
113300081009       clear vsdifs      ;
113400081009       vsdifo=CoRicerca  ;
113500081126 2     endif     ;
113600081007
113700081126       // Motivo non affidamento a noi: da richidere solo se corriere BART
113800081126       //  non è al 100
113900081126 2     if  vscifo='MOI'   ;
114000081126
114100081126         if vscval=0   ;
114200081126          vsdifo=CoRicercaMOI  ;
114300081126         endif         ;
114400081126
114500081126 3       if bart_10=100 and vscval>0        ;
114600081126           errVAL=*on ;
114700081126           vscmsg = Msg(16);
114800081126           EXSR AggioSFL   ;
114900081126           leavesr;
115000081126 3       endif             ;
115100081126
115200081126         if vscval>0      ;
115300081126           MOIconta=moiconta+1   ;
115400081126         endif     ;
115500081126
115600081126         moinrr=s01nrr    ;
115700081126
115800081126 2     endif            ;
115900081008
116000081007       // Controllo
116100081126 2     if vscval>0    and vshifgv='?' ;
116200081007       w005a=%editc(vscval:'X')    ;
116300081126       chain ('IFS':vscifo:%subst(w005a:2:4))  tntbe01l    ;
116400081126 3       if not %found(tntbe01l) or tbeatb<>' ' or %subst(w005a:1:1)<>'0';
116500081007          errVAL=*on ;
116600081007          vscmsg = Msg(18);
116700081007          EXSR AggioSFL   ;
116800081007          leavesr;
116900081126 x3       else                        ;
117000081007          vscspp=%subst(w005a:2:4)    ;
117100081007          difs=tbeuni ;
117200081007          vsdifo=§ifsdes              ;
117300081007          vsdifs=§ifsdel              ;
117400081126  3      endif             ;
117500081126
117600081126        //  Memorizzo i codici per vedere se sono doppi
117700081126        w008a=vscifo+%editc(vscval:'X')   ;
117800081126        Indx=%lookup(w008a:IFOval)   ;
117900081126 3      if Indx>0    ;
118000081126         errVAL=*on ;
118100081126 4       if vscifo='MOI'   ;
118200081126           vscmsg = Msg(28);
118300081126         else      ;
118400081126           vscmsg = Msg(30);
118500081126 4       endif     ;
118600081126         EXSR AggioSFL   ;
118700081126         leavesr;
118800081126
118900081126 x3     else   ;
119000081126          ifoval(yy)=w008a  ;
119100081126          yy=yy+1   ;
119200081126          // Se c'e' un MOI=9999 e uno no 9999, errore
119300081126          w008a='MOI09999'  ;
119400081126          Indx=%lookup(w008a:IFOval) ;
119500081126  4       if Indx>0   and moiconta>1 ;
119600081126            errVAL=*on ;
119700081126            vscmsg = Msg(29);
119800081126            EXSR AggioSFL   ;
119900081126            leavesr;
120000081126  4       endif    ;
120100081126  3     endif             ;
120200081126  2    endif             ;
120300081007
120400081126  2    if vscatb=' '  ;
120500081031
120600080925       // Se info SPT - totale spesa trasporti,  la memorizzo
120700080925       //  per il calcolo delle %
120800080925       if vscifo='SPT' ;
120900080925       SPTPFT=vscpft   ;
121000080925       endif           ;
121100140725
121200140725       // Se ripartizione spesa ESTERA lo memorizzo
121300140725       IF  VSHie = 'E' and VSCval > 0;
121400140725         wEstero = *on;
121500140725       ENDIF;
121600140725
121700081023       // Se info LOG - logistica  memorizzo per dare obbligatorio outsourci
121800081023       if vscifo='LOG' ;
121900081023       LOGfsn=vscfsn   ;
122000081023       LOGnrr=s01nrr   ;
122100081006       endif           ;
122200081023       // Se info OUT - outsourcing memorizzo per dare obbligatori
122300081023       //  i concorrenti della logistica
122400081023       if vscifo='OUT' ;
122500081023       OUTfsn=vscfsn   ;
122600081023       OUTnrr=s01nrr   ;
122700081023       endif           ;
122800140718
122900081007       // Se info 10 _BAR - concorrenti utilizzati bartolini
123000081007       //  memorizzo la % che ha
123100081007       if vscifo='10' and vscspp='_BAR' ;
123200081007       bart_10=vscval   ;
123300120105
123400120110         //?Controllo % affidamento a BRT
123500120110         SELECT;
123600120110         //?Controllo se devo azzerare la % di affidato a BRT
123700120110         WHEN  wAzzera10 and bart_10 > 0;
123800120110           errVAL = *on;
123900120125           VSCmsg = Msg(33);
124000120110           EXSR AggioSFL;
124100120110           leavesr;
124200120110         //?Controllo se devo aumentare la % di affidato a BRT
124300120111         WHEN  wAumenta10 and bart_10 < sav_bar_10 and bart_10 < 100;
124400120110           errVAL = *on;
124500120125           VSCmsg = Msg(31);
124600120110           EXSR AggioSFL;
124700120110           leavesr;
124800120510         //?Controllo se devo aumentare la % di affidato a BRT solo se = 0
124900120510         WHEN  wAumenta10f and bart_10 < sav_bar_10 and bart_10 = 0;
125000120510           errVAL = *on;
125100120510           VSCmsg = Msg(31);
125200120510           EXSR AggioSFL;
125300120510           leavesr;
125400120110         //?Controllo se devo diminuire la % di affidato a BRT
125500120110         WHEN  bart_10 > 0 and
125600120110               wDiminuisci10 and bart_10 > sav_bar_10;
125700120110           errVAL = *on;
125800120125           VSCmsg = Msg(34);
125900120110           EXSR AggioSFL;
126000120110           leavesr;
126100120110         ENDSL;
126200120105
126300081007       endif           ;
126400140718
126500100302       // Se info ATR - altro di ripartizione memorizzo la %
126600100226       if vscifo='ATR'                  ;
126700100226       infoATR=vscval   ;
126800100226       endif           ;
126900140721
127000140721       //?Salvo le % di ripartizione spesa totale
127100140721       SELECT;
127200140721         WHEN  VSCifo = 'PRF';
127300140721           savPRFval = VSCval;
127400140721         WHEN  VSCifo = 'PRO';
127500140721           savPROval = VSCval;
127600140721         WHEN  VSCifo = 'AER';
127700140721           savAERval = VSCval;
127800140721         WHEN  VSCifo = 'CAE';
127900140721           savCAEval = VSCval;
128000140725         WHEN  VSCifo = 'ATR';
128100140725           savATRval = VSCval;
128200140721       ENDSL;
128300140718
128400140919       //?Salvo e sommo il NON affidato a BRT
128500140721       SELECT;
128600140721         WHEN  VSCifo = 'NRF';
128700140721           savNRFpft = VSCpft;
128800140721           noBRTpft += VSCpft;
128900140721         WHEN  VSCifo = 'NRO';
129000140721           savNROpft = VSCpft;
129100140721           noBRTpft += VSCpft;
129200140721         WHEN  VSCifo = 'NER';
129300140721           savNERpft = VSCpft;
129400140721           noBRTpft += VSCpft;
129500140721         WHEN  VSCifo = 'NAE';
129600140721           savNAEpft = VSCpft;
129700140721           noBRTpft += VSCpft;
129800140725         WHEN  VSCifo = 'NTR';
129900140725           savNTRpft = VSCpft;
130000140725           noBRTpft += VSCpft;
130100140721       ENDSL;
130200100226
130300081126 2     endif        ;
130400080925
130500080703       EXSR AggioSFL   ;
130600081006 1     endif   ;
130700081125
130800080703
130900080703       s01nrr=1+s01nrr  ;
131000080703       chain s01nrr    mk50s01    ;
131100081006 0     enddo         ;
131200081006
131300081006       // Controllo ultima categoria
131400081006       EXSR CambioCAT     ;
131500081006
131600081215       // A questo punto le info minime sono presenti
131700081215       // solo per controlli obbligatori
131800081215       if i50obl='S' ;
131900081215         o50ifomin='S' ;
132000081215       endif ;
132100081215
132200081126       // Un motivo non affidamento obbligatorio se Bart<100%
132300081126 3     if bart_10<100 and MOIconta =0   and i50obl='S' ;
132400081126         chain moinrr    mk50s01    ;
132500081126         s01nrr=moinrr              ;
132600081126         errVAL=*on ;
132700081126         vscmsg = Msg(27);
132800081126         EXSR AggioSFL   ;
132900081126         leavesr;
133000081126 3     endif             ;
133100081126
133200081023       // Se logistica interessante per noi, obbligatorio outsourcing
133300081023       if LOGfsn='S' and outfsn=' '  and i50obl='S'  ;
133400081023         chain lognrr    mk50s01    ;
133500081023         s01nrr=lognrr              ;
133600081023         errFSN=*on ;
133700081023         vscmsg = Msg(26);
133800081023         EXSR AggioSFL   ;
133900081023         leavesr;
134000081023       endif             ;
134100081006       // Se presente outsourcing, obbligatori concorrenti logistica
134200081006       if outfsn='S' and log40=' '  and i50obl='S'  ;
134300081006         chain outnrr    mk50s01    ;
134400081006         s01nrr=outnrr              ;
134500081006         errFSN=*on ;
134600081006         vscmsg = Msg(15);
134700081006         EXSR AggioSFL   ;
134800081006         leavesr;
134900081006       endif             ;
135000140929
135100140929       //?Non affidato a BRT se affidato a BRT < 100% devo richiederlo obbligatorio
135200140929       //?quando il pgm è richiamo per info obbligatorie
135300140929        IF  I50obl = 'S' and BART_10 < 100 and
135400140929            noBRTpft = 0 and
135500140929            (savPRFval > 0 or savPROval > 0 or savAERval > 0 or
135600140929             savCAEval > 0 or savATRval > 0);
135700140929          chain ifoNRFnrr mk50S01;
135800140929          s01nrr = ifoNRFnrr;
135900140929          errPFT = *on;
136000140929          VSCmsg = Msg(38);
136100140929          EXSR AggioSFL;
136200140929          leavesr;
136300140929        ENDIF;
136400140721
136500140919       //?Se Non affidato a BRT impostato deve esserci la relativa % di ripartizione
136600140721       //?spesa traffico totale
136700140722        IF  savNRFpft > 0 and savPRFval = 0;
136800140721          chain ifoNRFnrr mk50S01;
136900140721          s01nrr = ifoNRFnrr;
137000140721          errPFT = *on;
137100140722          vscmsg = Msg(42);
137200140721          EXSR AggioSFL;
137300140721          leavesr;
137400140722        ENDIF;
137500140722        IF  savNRFpft > (SPTpft * savPRFval / 100);
137600140722          chain ifoNRFnrr mk50S01;
137700140722          s01nrr = ifoNRFnrr;
137800140722          errPFT = *on;
137900140722          vscmsg = Msg(41);
138000140722          EXSR AggioSFL;
138100140722          leavesr;
138200140722        ENDIF;
138300140722        IF  savNROpft > 0 and savPROval = 0;
138400140721          chain ifoNROnrr mk50S01;
138500140721          s01nrr = ifoNROnrr;
138600140721          errPFT = *on;
138700140722          vscmsg = Msg(42);
138800140721          EXSR AggioSFL;
138900140721          leavesr;
139000140722        ENDIF;
139100140722        IF  savNROpft > (SPTpft * savPROval / 100);
139200140722          chain ifoNROnrr mk50S01;
139300140722          s01nrr = ifoNROnrr;
139400140722          errPFT = *on;
139500140722          vscmsg = Msg(41);
139600140722          EXSR AggioSFL;
139700140722          leavesr;
139800140722        ENDIF;
139900140721         IF  savNERpft > 0 and savAERval = 0;
140000140721          chain ifoNERnrr mk50S01;
140100140721          s01nrr = ifoNERnrr;
140200140721          errPFT = *on;
140300140722          vscmsg = Msg(42);
140400140721          EXSR AggioSFL;
140500140721          leavesr;
140600140721         ENDIF;
140700140722         IF  savNERpft > (SPTpft * savAERval / 100);
140800140722          chain ifoNERnrr mk50S01;
140900140722          s01nrr = ifoNERnrr;
141000140722          errPFT = *on;
141100140722          vscmsg = Msg(41);
141200140722          EXSR AggioSFL;
141300140722          leavesr;
141400140722         ENDIF;
141500140721         IF  savNAEpft > 0 and savCAEval = 0;
141600140721          chain ifoNAEnrr mk50S01;
141700140721          s01nrr = ifoNAEnrr;
141800140721          errPFT = *on;
141900140722          vscmsg = Msg(42);
142000140721          EXSR AggioSFL;
142100140721          leavesr;
142200140721         ENDIF;
142300140722         IF  savNAEpft > (SPTpft * savCAEval / 100);
142400140722          chain ifoNAEnrr mk50S01;
142500140722          s01nrr = ifoNAEnrr;
142600140722          errPFT = *on;
142700140722          vscmsg = Msg(41);
142800140722          EXSR AggioSFL;
142900140722          leavesr;
143000140722         ENDIF;
143100140725         IF  savNTRpft > 0 and savATRval = 0;
143200140725          chain ifoNTRnrr mk50S01;
143300140725          s01nrr = ifoNAEnrr;
143400140725          errPFT = *on;
143500140725          vscmsg = Msg(42);
143600140725          EXSR AggioSFL;
143700140725          leavesr;
143800140725         ENDIF;
143900140725         IF  savNTRpft > (SPTpft * savATRval / 100);
144000140725          chain ifoNTRnrr mk50S01;
144100140725          s01nrr = ifoNTRnrr;
144200140725          errPFT = *on;
144300140725          vscmsg = Msg(41);
144400140725          EXSR AggioSFL;
144500140725          leavesr;
144600140725         ENDIF;
144700140718
144800140718       //?Se affidato a BRT è 100%  non devo indicare nessuna spesa per il mancato
144900140718       //?affidamento a BRT
145000140718        IF  bart_10 = 100 and noBRTpft > 0;
145100140718          chain ifoNRFnrr mk50S01;
145200140718          s01nrr = ifoNRFnrr;
145300140718          errPFT = *on;
145400140721          vscmsg = Msg(40);
145500140718          EXSR AggioSFL;
145600140718          leavesr;
145700140718        ENDIF;
145800140721
145900140721       //?Se affidato a BRT non è 100%
146000140919       //?Il non affidato a BRT deve essere congruente
146100140804        IF  bart_10 <> 100 and noBRTpft > 0 and
146200140725            noBRTpft <> (sptpft - siBRTpft);
146300140722          SELECT;
146400140722            WHEN  savPRFval > 0;
146500140722              chain ifoNRFnrr mk50S01;
146600140722              s01nrr = ifoNRFnrr;
146700140722            WHEN  savPROval > 0;
146800140722              chain ifoNROnrr mk50S01;
146900140722              s01nrr = ifoNROnrr;
147000140722            WHEN  savAERval > 0;
147100140722              chain ifoNERnrr mk50S01;
147200140722              s01nrr = ifoNERnrr;
147300140722            WHEN  savCAEval > 0;
147400140722              chain ifoNAEnrr mk50S01;
147500140722              s01nrr = ifoNAEnrr;
147600140725            WHEN  savATRval > 0;
147700140725              chain ifoNTRnrr mk50S01;
147800140725              s01nrr = ifoNTRnrr;
147900140722          ENDSL;
148000140721          errPFT = *on;
148100140721          vscmsg = Msg(40);
148200140721          EXSR AggioSFL;
148300140721          leavesr;
148400140721        ENDIF;
148500081006
148600080409       ENDSR;
148700080409
148800081006       //--------------------------------------------------------------
148900081006       //?cambiata categoria delle info conn- controllo e salvataggi
149000081006       //--------------------------------------------------------------
149100081006       BEGSR  CambioCAT    ;
149200140716
149300081006       if wctrprc='P'  and  i50obl='S' and totprc<100  ;
149400081006       s01nrr=s01nrr-1 ;
149500081006       chain s01nrr    mk50s01    ;
149600081006           errVal=*on;
149700081006           vscmsg = Msg(13);
149800081006           %subst(vscmsg:40:21)=wdescat   ;
149900081006           EXSR AggioSFL   ;
150000081006       endif              ;
150100081006
150200081006       // Pulisco e memorizzo nuovi dati per nuova categoria
150300081006        clear  totprc     ;
150400081215        wctrprc=vshobl  ;
150500081215        wdescat=vsdifo   ;
150600140716
150700081006        ENDSR   ;
150800080925       //--------------------------------------------------------------
150900080925       //?Visualizzo importi in base alla spesa trasporti
151000080925       //--------------------------------------------------------------
151100080925       BEGSR    VISUALimp  ;
151200081003       if vshsnf=' ' and vscpft=0 and vscval>0 and vsctva='%  '  ;
151300081215       PFTblu=*on     ;
151400081003       vscpft=(SPTpft*vscval)/100   ;
151500140717       //if vscpft>0    ;
151600140717       //vscvft='EUR'   ;
151700140717       //endif ;
151800140721       //?Se sto calcolando l'importo della % di affido a BRT me la salvo
151900140721         IF  vscifo = '10' and vscspp = '_BAR';
152000140721           siBRTpft = vscpft;
152100140721         ENDIF;
152200140721
152300080925       endif ;
152400080925
152500080925       ENDSR      ;
152600080703       //--------------------------------------------------------------
152700080703       //?Aggiornamento sfl
152800080703       //--------------------------------------------------------------
152900080703       BEGSR AGGIOSFL  ;
153000080703       if vscmsg<>*blanks   ;
153100080703       errMessage  = *on;
153200080703       errGenerico = *on;
153300080704       c01csr=s01nrr    ;
153400080703       endif    ;
153500080703
153600080703       exsr ProtCampi   ;
153700080703
153800080703       //  Ripristino dei flag di forzatura
153900081003       if vshsnf='X' and vscpft>0   ;
154000081003       vshsnf='S'   ;
154100080703       endif    ;
154200081003       if vshsns='X' and vscsna>0   ;
154300080703       vshsns='S'   ;
154400080703       endif    ;
154500081003       if vshsnv='X' and vscval>0  ;
154600081003       vshsnv='S'   ;
154700080703       endif    ;
154800140715       IF  vshsnvd = 'X' and vscvald <> 0;
154900140715         vshsnvd = 'S';
155000140715       ENDIF;
155100081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
155200081007       vshsne='S'   ;
155300081007       endif    ;
155400080703
155500110707       if vscatb='A' and vshann= 'X' ;
155600080704       vshann='S'  ;
155700080704       endif   ;
155800080703
155900080703       update  mk50s01  ;
156000080703       ENDSR   ;
156100080207       //--------------------------------------------------------------
156200080627       //?Inizializzazione SFL01
156300080207       //--------------------------------------------------------------
156400080409       BEGSR InzS01;
156500080207
156600080207       // Pulizia subfile
156700080207         SflDsp_N    = *on;
156800080207         SflDspCtl_N = *on;
156900080627         write  MK50C01;
157000080207         SflDspCtl_N = *off;
157100080207         SflEnd      = *off;
157200080530
157300080530         clear C01rcd;
157400100507         C01csr=1;
157500080627         S01nrr=0 ;
157600080702         clear Vscmsg;
157700080207         errMessage  = *off;
157800080207         errGenerico = *off;
157900080627         clear savcat  ;
158000080627         clear wcat    ;
158100080627         clear wifo    ;
158200081006         clear wIFS    ;
158300081006         clear sptpft  ;
158400100302         clear savspt  ;
158500100302         clear savatr  ;
158600140716
158700120109         wForzaAgg = *off;
158800120111         clear sav_bar_10;
158900140716         wForzaPft = *off;
159000140715
159100140715         errVAL=*off     ;
159200140715         errVALD = *off;
159300080409
159400080702       // Carico le info
159500080627       xx=1    ;
159600080627       Indx=1    ;
159700080702       dow   xx<=50    ;
159800080702       if    ifoor(xx)<>*blanks  ;
159900080627       clear  MK50S01;
160000080627
160100080627       Wcat= %subst(ifoor(xx):1:2)   ;
160200080702       Wifo= %subst(ifoor(xx):6:3)   ;
160300080627
160400080627       // cambio di categoria
160500080627        if savcat=*blanks or savcat<>wcat               ;
160600080627        savcat=wcat   ;
160700080627        Ifosott=*on   ;
160800080627
160900080627       // Verifico se devo visualizzare la titolo della categoria
161000080627         clear  DIFG  ;
161100080627         reset TIBS02ds;
161200080627         T02sif = knsif;
161300080627         T02cod = 'IFG';
161400080627         T02mod = 'C'  ;
161500080627         T02ke1 = wcat;
161600080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
161700080627         if t02err=*blanks ;
161800080627            difg=t02uni    ;
161900080627         endif            ;
162000080627
162100080627       // Se prevista la visualizzazione della categoria
162200080627       //  emetto la riga in videata,
162300080627       if §ifgvis='S'    ;
162400080703         vsdifo=§ifgdes         ;
162500080703         vsdifo=%trim(vsdifo)+':'    ;
162600080703         // In VSHIMM   metto una "C" per indicare che si tratta
162700080703         //    di riga di categoria
162800080703         vshimm='C'        ;
162900081007
163000081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
163100081003         vshobl=§ifgctpr   ;
163200081007
163300081007         EXSr ProtCampi    ;
163400081003
163500080627         s01nrr=s01nrr+1   ;
163600080627         write mk50s01     ;
163700080627         protriga=*off     ;
163800080627         Ifosott=*off      ;
163900081007         clear vscfsn      ;
164000100507         // Se la prima riga è un commento, posizionamento cursore sulla seconda
164100100507         if s01nrr=1   ;
164200100507          c01csr= 2 ;
164300100507         endif   ;
164400100507
164500081007       endif               ;
164600081007       endif               ;
164700080627
164800080627       Indx=%lookup(wifo:ifo)   ;
164900080627       if Indx>0                ;
165000080627         difo=ifods(Indx)         ;
165100080627         vscifo=ifo(Indx)       ;
165200080702
165300080702         // Dati da tabella
165400080702          vshobl=§ifoobl  ;
165500081015          vshie =§ifotip  ;
165600080704
165700080702          vshann=§ifoann  ;
165800080704
165900081003         // VALORE
166000081003          clear vshsnv  ;
166100081003          clear vsctva  ;
166200081003          if §ifosnv<>' '  ;
166300081003          vshsnv=§ifosnv  ;
166400081003          // tipo valore
166500081003          vsctva=§ifotva ;
166600080703          endif   ;
166700140715
166800140715         //?VALORE con DECIMALE
166900140715          IF  §ifosnvd <> *blanks;
167000140715            vshsnvd = §ifosnvd;
167100140715          // tipo valore
167200140715            vsctva = §ifotva;
167300140715          ENDIF;
167400080704
167500081003         // SPEDIZIONI
167600080702          vshsns=§ifosns  ;
167700080704
167800081003         // FATTURATO
167900080702          vshsnf=§ifosnf  ;
168000081003
168100081003         // SI/NO
168200081003          vshsnE=§ifosnE  ;
168300081003
168400080702          vshifgv=§ifgvis  ;
168500081007          clear vscint    ;
168600080627
168700080627         // Record senza sottotipo
168800081006         select    ;
168900081006         when §ifoifs=' '    ;
169000080702            vsdifo=§ifodes         ;
169100080703            if Ifosott=*on    ;
169200080703               vsdifo=%trim(vsdifo)+':'    ;
169300080703            endif    ;
169400080702            chain    (i50cpo:wifo) ticpi01l  ;
169500081126            if %found(ticpi01l)   ;
169600081126             trovacpi='S'    ;
169700081126             else ;
169800081126             trovacpi='N'    ;
169900081126             endif  ;
170000080702            EXSR CaricaInfo        ;
170100081006
170200081006         // Record con   sottotipo da caricare tutti
170300081006         when   §ifoifs='S' and §ifoifv='S';
170400080627            EXSR CARCon    ;
170500081006
170600081006         // Record con   sottotipo da caricare col '?'
170700081006         //   la categoria visualizzata in questo caso è obbligatoria
170800081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
170900081006         //  il '?'
171000081006         when   §ifoifs='S' and §ifoifv='?';
171100081006          vshifgv=§ifoifv  ;
171200081215          yy=1        ;
171300081126
171400081126          if §ifonrec=0 ;
171500081126             §ifonrec=1  ;
171600081126          endif     ;
171700081126
171800081126          setll (i50cpo:wifo) ticpi01l  ;
171900081126          dow yy<=§ifonrec  ;
172000081006            EXSR CarconUNA            ;
172100081126            yy=yy+1  ;
172200081126          enddo    ;
172300081126
172400081006         endsl ;
172500080703
172600080703         endif ;
172700080702         endif ;
172800080702
172900080702         xx=xx+1   ;
173000080702         enddo     ;
173100080702         ENDSR    ;
173200080627
173300080702       //--------------------------------------------------------------
173400081006       //?Carico con   sottotipo  carica riga per riga
173500080702       //--------------------------------------------------------------
173600080702       BEGSR  CARcon            ;
173700080702
173800081126         tbeke1=wifo   ;
173900080925
174000081006         setll ('IFS':tbeke1) tntbe01l   ;
174100081006         READE ('IFS':tbeke1) tntbe01l   ;
174200080702
174300080702         dow not %eof(tntbe01l)   ;
174400080702         if tbeatb=' '    ;
174500081006         difs=tbeuni      ;
174600081006         wifs=tbeke2      ;
174700080702         vscspp=tbeke2      ;
174800081127         vshspp=tbeke2      ;
174900081007         vsdifo=§ifsdes  ;
175000110707         vshann=§ifsann  ;
175100080702
175200081006         chain    (i50cpo:wifo:wifs) ticpi01l  ;
175300081126            if %found(ticpi01l)   ;
175400081126             trovacpi='S'    ;
175500081126             else ;
175600081126             trovacpi='N'    ;
175700081126             endif  ;
175800080702         EXSR CaricaInfo    ;
175900080925         endif   ;
176000080702
176100081006         READE ('IFS':tbeke1) tntbe01l   ;
176200080702         enddo   ;
176300080702
176400080702       ENDSR;
176500081006       //--------------------------------------------------------------
176600081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
176700081006       //--------------------------------------------------------------
176800081006       BEGSR  CARconUNA         ;
176900081007
177000081007       vscint='?'    ;
177100081007
177200081126       reade    (i50cpo:wifo) ticpi01l  ;
177300081006
177400081126 1     if not %eof(ticpi01l)   ;
177500081126         trovacpi='S'    ;
177600081126
177700081006         vscspp=cpispf   ;
177800081127         vshspp=cpispf   ;
177900081006
178000081126         tbeke1=wifo    ;
178100081126
178200081006         tbeke2=cpispf  ;
178300081006         chain ('IFS':tbeke1:tbeke2) tntbe01l   ;
178400081006 2       if not %found(tntbe01l)    ;
178500081006         clear tbeuni       ;
178600081006 2       endif    ;
178700081007         difs=tbeuni     ;
178800081006
178900081007         vsdifo=§ifsdes   ;
179000081007         vsdifs=§ifsdel   ;
179100081007
179200081006         EXSR CaricaINFO  ;
179300081006
179400081006 1x      else    ;
179500081126         trovacpi='N'    ;
179600081006
179700081126         // se non trovato dicitura generica da tabella IFO
179800081126         if wifo='MOI'  ;
179900081126           vsdifo=coRicercaMOI    ;
180000081126           else  ;
180100081126           vsdifo=coRicerca       ;
180200081126         endif  ;
180300081007         clear vsdifs      ;
180400081126         clear vscspp      ;
180500081127         clear vshspp      ;
180600081006
180700081006         EXSR CaricaINFO   ;
180800081006         endif             ;
180900081006
181000081006       ENDSR;
181100080702       //--------------------------------------------------------------
181200081006       //?Carica ogni riga di info commerciale
181300080702       //--------------------------------------------------------------
181400080702         BEGSR CaricaINFO             ;
181500080925         clear pftblu   ;
181600080925
181700081126         if trovacpi='S'                             ;
181800080703          vshimm=' '        ;
181900080702          vscfsn=cpifsn   ;
182000081003          vscval=cpival   ;
182100081007          if vshifgv='?'   and vscspp>*zeros    ;
182200081007            w0040=%int(vscspp)  ;
182300081007            vscval=w0040     ;
182400081007            clear vscfsn     ;
182500081007          endif            ;
182600080702          vscsna=cpisna   ;
182700080702          vscpft=cpipft   ;
182800080925          if vscpft>0     ;
182900080925          vshprpft='S'    ;
183000080925          endif           ;
183100140717          //vscvft=cpivft   ;
183200080702          Dataiso=%date(cpidim:*iso)  ;
183300080702          datadmy=Dataiso ;
183400080702          vscdim=%dec(datadmy)  ;
183500081003          clear vscfil  ;
183600081003          if cpifil>0   ;
183700081003          vscfil=%editc(cpifil:'X')   ;
183800081003          endif         ;
183900081003
184000081003          if cpipru<>*blanks ;
184100080702          vscpru=cpipru   ;
184200081003          vscdpru='Utente' ;
184300081003          else  ;
184400081003          clear vscpru   ;
184500081003          clear vscdpru   ;
184600081003          endif     ;
184700080925
184800080925          exsr ViSUALimp   ;
184900140715
185000140715         //?Calcolo il ricavo medio spedizione 'RMS'
185100140715           IF  VSCifo = 'SPT' and VSCpft <> 0 and VSCsna <> 0;
185200140715             wVALrms = VSCpft / VSCsna;
185300140929             IF  wVALrms > 999;
185400140929               wVALrms = 999;
185500140715             ENDIF;
185600140715           ENDIF;
185700080925
185800080702         else             ;
185900080703          vshimm='S'      ;
186000080703          Protann=*on     ;
186100080702          clear vscfsn    ;
186200081003          clear vscval    ;
186300080702          clear vscsna    ;
186400080702          clear vscpft    ;
186500140717          //clear vscvft    ;
186600080702          clear vscdim    ;
186700080702          clear vscfil    ;
186800080702          clear vscpru    ;
186900081003          clear vscdpru    ;
187000080925          clear vshprpft  ;
187100140715
187200140715         //?Se info 'RMS' imposto il dato
187300140715           IF  VSCifo = 'RMS';
187400140715             VSCvald = wVALrms;
187500140715           ENDIF;
187600140715
187700080702         endif            ;
187800081006
187900080925         exsr ProtCAMPI    ;
188000080703
188100080703         // I record da annullare li visualizzo solo se ci sono
188200110707           if vshann=' ' or (vshann<>' '  and vscfsn<>' ') or
188300110707                             (vshann<>' '  and vscpft>0)   or
188400110707                             (vshann<>' '  and vscval>0   )  ;
188500081003
188600081003         // In interrogazione visulizzo solo i record pieni col sottotipo
188700081003           if i50mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
188800080703            s01nrr=s01nrr+1   ;
188900080703            write mk50s01     ;
189000140718
189100140919         // Mi salvo i nr di rcd del non affidato a BRT
189200140721            IF  VSCifo = 'NRF' and ifoNRFnrr = 0;
189300140718              ifoNRFnrr = S01nrr;
189400140721            ENDIF;
189500140721            IF  VSCifo = 'NRO' and ifoNROnrr = 0;
189600140721              ifoNROnrr = S01nrr;
189700140721            ENDIF;
189800140721            IF  VSCifo = 'NER' and ifoNERnrr = 0;
189900140721              ifoNERnrr = S01nrr;
190000140721            ENDIF;
190100140721            IF  VSCifo = 'NAE' and ifoNAEnrr = 0;
190200140721              ifoNAEnrr = S01nrr;
190300140721            ENDIF;
190400140725            IF  VSCifo = 'NTR' and ifoNTRnrr = 0;
190500140725              ifoNTRnrr = S01nrr;
190600140725            ENDIF;
190700080925
190800080925         // Se info SPT - totale spesa trasporti,  la memorizzo
190900100302         //  per il calcolo delle %  e per vedere se modificata
191000080925         if vscifo='SPT' ;
191100080925          SPTPFT=vscpft   ;
191200100302          savSPT=vscpft   ;
191300080925         endif           ;
191400100302         //  per vedere se modificata
191500100302         if vscifo='ATR' ;
191600100302          savATR=vscval   ;
191700100302         endif           ;
191800120105
191900120105         //?Memorizzo la % di affidamento a BRT
192000120110           IF  VSCifo = '10' and VSCspp = '_BAR';
192100120105           //?Se devo aumentarla calcolo almeno 1% in +
192200120110             IF  wAumenta10 and VSCval > 0;
192300120111               sav_bar_10 = VSCval + 1;
192400120105             ENDIF;
192500120510             IF  (wAumenta10 or wAumenta10f) and VSCval = 0;
192600120110               sav_bar_10 = 1;
192700120110             ENDIF;
192800120105           //?Se devo dominuirla calcolo almeno 1% in -
192900120110             IF  wDiminuisci10 and VSCval > 0;
193000120111               sav_bar_10 = VSCval - 1;
193100120105             ENDIF;
193200120105           ENDIF;
193300080925
193400080703           endif ;
193500081003           endif ;
193600081003
193700080702       ENDSR;
193800080702       //--------------------------------------------------------------
193900080702       //?Gestione protezione campi del sfl
194000080702       //--------------------------------------------------------------
194100080702          BEGSR   ProtCampi   ;
194200080703           protfat=*off    ;
194300081003           protVal =*off   ;
194400140715           protVald = *off;
194500140715           protVala = *off;
194600080703           protsped=*off   ;
194700080703           protfsn =*off   ;
194800080703           Ifosott=*off    ;
194900080703           Protann=*off ;
195000080704           Infoyell=*off ;
195100080703
195200081007           // Se riga di categoria accendo solo protezione e sottolineatura
195300081007          if vshimm='C'    ;
195400081007           Ifosott=*on     ;
195500081007           Protriga=*on    ;
195600081007          else             ;
195700081007
195800080702          // in base a cosa prevede la tabella, proteggo i campi
195900080702          if vshifgv='N' ;
196000080702            Ifosott=*on       ;
196100080702          endif    ;
196200080702
196300080704          if vshann<>' '  ;
196400080702           protfat=*on     ;
196500081003           protVal =*on    ;
196600140715           protVald = *on;
196700140715           protVala = *on;
196800080702           protsped=*on    ;
196900080702           protfsn =*on    ;
197000080704           infoyell=*on    ;
197100080702          else            ;
197200081006          protann=*on     ;
197300081031
197400081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
197500081103          if vscpft>0 and vshsnf=' ' and vscval=0;
197600081031          protann=*off    ;
197700081031          endif    ;
197800081031          if vscsna>0 and vshsns=' ' ;
197900081031          protann=*off    ;
198000081031          endif    ;
198100081127          if vscval>0 and vshsnv=' ' and vshifgv<>'?'       ;
198200081031          protann=*off    ;
198300081031          endif    ;
198400140715          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
198500140715            protann = *off;
198600140715          ENDIF;
198700081031          if vscfsn<>' ' and vshsne=' ' ;
198800081031          protann=*off    ;
198900081031          endif    ;
199000081006
199100080703            if vshsnf=' ' ;
199200080702             protfat=*on     ;
199300080702            endif           ;
199400081003            if vshsnv=' ' ;
199500081003             protVal =*on     ;
199600080702            endif           ;
199700140715            IF  vshsnvd = *blanks;
199800140715              protVald = *on;
199900140715            ENDIF;
200000140715            if vshsnvd='A';
200100140715             protVala=*on     ;
200200140715            endif           ;
200300080703            if vshsns=' ' ;
200400080702             protsped=*on     ;
200500080702            endif           ;
200600081003            if vshsne=' ' ;
200700081003             protfsn =*on     ;
200800081003            endif           ;
200900080702          endif           ;
201000080703          // Se record di nuova immissione
201100081003          //  proteggo annullamento
201200080703            if vshimm='S'  ;
201300080703             Protann=*on  ;
201400080703           endif   ;
201500081006
201600081006          // Se prevista scelta sottotipo col'?' visualizzo valore
201700081006          //   col codice e vicino il '?'
201800081006          if vshifgv='?'    ;
201900081006             protVal =*off    ;
202000081007             if vscspp>*zeros    ;
202100081007             else          ;
202200081007             infoyell=*on    ;
202300081007             endif            ;
202400081006          endif               ;
202500081007          endif               ;
202600080703
202700080702          ENDSR    ;
202800080702       //--------------------------------------------------------------
202900080702       //?conferma aggiornamento
203000080702       //--------------------------------------------------------------
203100080702        BEGSR ConfAGGIO              ;
203200090520        clear ultdata   ;
203300081003
203400080704       s01nrr=1  ;
203500080704       chain s01nrr    mk50s01    ;
203600080704
203700080704 1     dow %found    ;
203800080704       // Escludo i record di descrizione categoria
203900140715 2     //if vshimm<>'C'    ;
204000140715 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
204100080925
204200080925       // se valori calcolati da pgm li pulisco prima dei controlli
204300080925       if vshsnf=' ' and vshprpft=' '   ;
204400080925       clear vscpft   ;
204500140717       //clear vscvft   ;
204600080925       endif          ;
204700080925
204800081127       IF VShifgv<>'?'    ;
204900081127         chain    (i50cpo:vscifo:vscspp) ticpi01l  ;
205000081127       else   ;
205100081127         chain    (i50cpo:vscifo:vshspp) ticpi01l  ;
205200081127       endif   ;
205300081127             if %found(ticpi01l)   ;
205400081127             trovacpi='S'    ;
205500081127             else ;
205600081127             trovacpi='N'    ;
205700081127             endif  ;
205800081127
205900081127       // A n n u l l a m e n t o oppure per qualche strano motivo
206000081127       //   la info c'e'  ma a video non è stata inserita
206100080925
206200081127 3     if trovacpi='S'      and (vscatb='A'  or (vscfsn=' ' AND
206300081003                                 vscpft=0 and vscsna=0 and vscval=0));
206400081003         delete ticpi000  ;
206500081003 x3    else    ;
206600080704
206700081127       // V a r i a z i o n e :  verifico se modificati dei dati
206800081127 31    if trovacpi='S'     and vscatb=' '   ;
206900080704 4     if (cpifsn<>vscfsn) or
207000081003          (cpival<>vscval) or
207100080704          (cpisna<>vscsna) or
207200081007          (cpispf<>vscspp) or
207300140717          (cpipft<>vscpft) ;
207400140717          //(cpivft<>vscvft) ;
207500080704          // reimposto data ora profilo immissione
207600080704          cpidim=*date    ;
207700090520          ultdata=*date    ;
207800080704          cpifil=utefil   ;
207900080704          cpipru=knmus    ;
208000080704
208100081007          cpifsn= vscfsn  ;
208200081007          IF VShifgv<>'?';
208300081003          cpival= vscval  ;
208400081003          cpitva= vsctva  ;
208500081007          else            ;
208600081007          cpispf=vscspp   ;
208700081007          if cpispf<>*blanks ;
208800081007          cpifsn='S'      ;
208900081007          ENDIF           ;
209000081007          ENDIF           ;
209100080704          cpisna= vscsna  ;
209200080704          cpipft= vscpft  ;
209300140717          cpivft= 'EUR'   ;
209400081006          // verifico se dei dati non ci voglio più: una volta aggiunti
209500080704          //  quelli previsti, lo tolgo
209600080704
209700081003 5        if (cpival>0 and vshsnv=' ')    ;
209800081006          if (cpisna>0 and vshsns<>' ')  or (cpipft>0  and vshsnf<>' ') or
209900081006             (cpifsn<>' ' and vshsne<>' ')   ;
210000081003          clear cpival   ;
210100080704          endif          ;
210200080704 5        endif          ;
210300080704 5        if (cpisna>0 and vshsns=' ')    ;
210400081006          if (cpival>0 and vshsnv<>' ')  or (cpipft>0  and vshsnf<>' ') or
210500081006             (cpifsn<>' ' and vshsne<>' ')   ;
210600080704          clear cpisna   ;
210700080704          endif          ;
210800080704 5        endif          ;
210900080704 5        if (cpipft>0 and vshsnf=' ')    ;
211000081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
211100081006             (cpifsn<>' ' and vshsne<>' ')   ;
211200080704          clear cpipft   ;
211300080704          clear cpivft   ;
211400080704          endif          ;
211500080704 5        endif          ;
211600081006 5        if (cpifsn<>' ' and vshsne=' ')    ;
211700081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
211800081006             (cpipft>0  and vshsnf<>' ')     ;
211900081006          clear cpifsn   ;
212000081006          endif          ;
212100081006 5        endif          ;
212200080704
212300080704        update ticpi000  ;
212400080704 4      endif  ;
212500081003 3a     endif  ;
212600080704
212700081127       // I m m i s s i o n e
212800081127 3     if trovacpi='N'     and vscatb=' ' and (vscfsn<>' ' or
212900081003       vscpft>0 or vscsna>0 or vscval>0) ;
213000080704       clear ticpi000;
213100080704        cpicpo=i50cpo   ;
213200080704        cpitpf=vscifo  ;
213300080704        cpispf=vscspp  ;
213400080704        cpifsn=vscfsn ;
213500081007        if vshifgv<>'?';
213600081003        cpival= vscval  ;
213700081003        cpitva= vsctva  ;
213800081007        else            ;
213900081007          if cpispf<>*blanks ;
214000081007          cpifsn='S'      ;
214100081007          endif           ;
214200081007        endif           ;
214300081007
214400080704        cpisna= vscsna  ;
214500080704        cpipft= vscpft  ;
214600140717        cpivft= 'EUR'   ;
214700080704        cpidim=*date    ;
214800090520        ultdata=*date    ;
214900080704        cpifil=utefil   ;
215000080704        cpipru=knmus    ;
215100080704
215200080704        write ticpi000 ;
215300081003  3a    endif           ;
215400081003  3     endif           ;
215500080704  2     endif           ;
215600080704
215700080704        s01nrr=s01nrr+1 ;
215800080704        chain s01nrr    mk50s01    ;
215900080704  1     enddo           ;
216000140929
216100140929       //?Se affidato a BRT è 0
216200140929       //?Se ripartizione della spesa è 100%
216300140929       //?Se non affidato a BRT non è presente
216400140929       //?creo in automatico il NON affidato a BRT
216500140929         IF  BART_10 = 0 and
216600140929             (savPRFval + savPROval + savAERval +
216700140929              savCAEval + savATRval) = 100 and
216800140929             noBRTpft = 0;
216900140929           exsr NoAffidatoBRT;
217000140929         ENDIF;
217100080704
217200081015        clear waggiocpo    ;
217300081015
217400081003       // Aggancio tncpo per aggiornare se ho tutte le info
217500081003       chain   i50cpo   tncpo01l   ;
217600081015  1      if %found(tncpo01l)    ;
217700081015         chain  cp1nrr    tncpo00f   ;
217800081015  1      endif                  ;
217900081003  1      if %found(tncpo00f)   ;
218000081015
218100081015         // Se inserimento obbligatorio--> posso impostare la "S"
218200081015         //  altrimenti controllo da CPI
218300081003
218400081015  2       if i50obl=' '   ;
218500081015            // Rifaccio il controllo del sfl come se fosse
218600081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
218700081215            savmod=i50mod   ;
218800081215            i50mod='C'      ;
218900081215            i50obl='S' ;
219000081215
219100081015            exsr contrs01   ;
219200081215
219300081215            i50obl=' ' ;
219400081215            i50mod=savmod   ;
219500081015
219600081015  3         if ErrGenerico=*off;
219700081215              Waggiocpo='S';
219800081015  3         endif  ;
219900081015
220000081015  x2      else;
220100081015          Waggiocpo='S'   ;
220200081015  2       endif           ;
220300081015
220400081015              dcpo01=cporst    ;
220500081015              §cpoifotot=Waggiocpo   ;
220600081015              o50ifotot=Waggiocpo   ;
220700090520              if ultdata > 0   ;
220800090520                §cpoifodul=%editc(ultdata:'X')  ;
220900090520              endif   ;
221000090520              // Se inserita memorizzo la spesa trasporti reale
221100090520              if sptpft>0   ;
221200090520              §cposptp=sptpft   ;
221300090520              endif   ;
221400100302
221500100302              //  Se variata spesa trasporti o % ATR, ricalcolo cod importanza potenziale
221600100302             if savSPT<> SPTPFT or savatr<>infoATR   ;
221700100302             clear trmk52ds  ;
221800100302             i52cpo=cpocpo  ;
221900100302             i52spt= SPTpfT ;
222000100302             i52ATR=infoATR  ;
222100160802           //?Se la spesa trasporti è a 0 devo passare il trasporto presunto
222200160802           //?in questo modo viene calcolata la categoria del cliente sulla spesa
222300160802           //?presunta e non viene impostata a blank come quando la spesa è a 0
222400160802             IF  SPTpft = 0;
222500160802               I52spt = §CPOsptp;
222600160802               I52tspt = 'P';
222700160802             ENDIF;
222800100302
222900100302             callp TRMK52R  (trmk52ds)  ;
223000100302             cpoftr=o52cimp  ;
223100100302             endif   ;
223200100302
223300081015              cporst=dcpo01    ;
223400081015              update tncpo000  ;
223500081003  1    endif   ;
223600160708
223700160708       //?Scrivo la variazione
223800160708       exsr ScriviVar;
223900140919
224000140725       // devo aggiornare la data ultima conferma INFO COMM.
224100140909         IF  not InfoInt;
224200140725           chain i50cpo TNCPO11L;
224300140725  1        IF  %found(TNCPO11L);
224400140909             IF  ultdata > 0;
224500140909               CPO1ducifo = ultdata;
224600140909             ELSE;
224700140909               CPO1ducifo = *date;
224800140909             ENDIF;
224900140725             update TNCPO100;
225000140725           ENDIF;
225100140909           IF  not %found(TNCPO11L);
225200140909             clear TNCPO100;
225300140909             CPO1cpo = CPOcpo;
225400140909             IF  ultdata > 0;
225500140909               CPO1ducifo = ultdata;
225600140909             ELSE;
225700140909               CPO1ducifo = *date;
225800140909             ENDIF;
225900140909             write TNCPO100;
226000140909           ENDIF;
226100140725         ENDIF;
226200140725
226300080702        ENDSR;
226400140919
226500140919       //--------------------------------------------------------------
226600140919       //?Creo le info di NON Affidato a BRT.
226700140919       //--------------------------------------------------------------
226800140919        BEGSR NoAffidatoBRT;
226900140919
227000140919          clear TICPI000;
227100140919          CPIcpo = I50CPO;
227200140919          CPIvft = 'EUR';
227300140919          CPIdim = *date;
227400140919          CPIfil = utefil;
227500140919          CPIpru = knmus;
227600140919
227700140919          IF  savPRFval > 0 and savNRFpft = 0;
227800140919            CPItpf = 'NRF';
227900140919            CPIpft = (SPTpft * savPRFval / 100);
228000140919            write TICPI000;
228100140919          ENDIF;
228200140919
228300140919          IF  savPROval > 0 and savNROpft = 0;
228400140919            CPItpf = 'NRO';
228500140919            CPIpft = (SPTpft * savPROval / 100);
228600140919            write TICPI000;
228700140919          ENDIF;
228800140919
228900140919          IF  savAERval > 0 and savNERpft = 0;
229000140919            CPItpf = 'NER';
229100140919            CPIpft = (SPTpft * savAERval / 100);
229200140919            write TICPI000;
229300140919          ENDIF;
229400140919
229500140919          IF  savCAEval > 0 and savNAEpft = 0;
229600140919            CPItpf = 'NAE';
229700140919            CPIpft = (SPTpft * savCAEval / 100);
229800140919            write TICPI000;
229900140919          ENDIF;
230000140919
230100140919          IF  savATRval > 0 and savNTRpft = 0;
230200140919            CPItpf = 'NTR';
230300140919            CPIpft = (SPTpft * savATRval / 100);
230400140919            write TICPI000;
230500140919          ENDIF;
230600140919
230700140919        ENDSR;
230800160708
230900160708       //--------------------------------------------------------------
231000160708       //?Scrivo l'immagine Precedente.
231100160708       //--------------------------------------------------------------
231200160708       BEGSR ImmaginePrima;
231300160711
231400160711       //?Aggancio il TNCPO per memorizzare immagine precedente
231500160711         chain(n) (I50cpo) TNCPO01L;
231600160831         dCPO01 = CPOrst;
231700160708       //?Aggancio il rcd SPT per memorizzare immagine precedente
231800160708         chain(n) (I50cpo:'SPT') TICPI01L;
231900160711
232000160708         clear TRMK30DS;
232100160708         IMK30immag = 'P';
232200160711         IMK30tvp = 'D';
232300160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
232400160708
232500160708       ENDSR;
232600160708
232700160708       //--------------------------------------------------------------
232800160708       //?Scrivo Storico Variazioni.
232900160708       //--------------------------------------------------------------
233000160708       BEGSR ScriviVar;
233100160708
233200160831       //?Aggancio il TNCPO per memorizzare immagine variata
233300160831         chain(n) (I50cpo) TNCPO01L;
233400160831         dCPO01 = CPOrst;
233500160708       //?Aggancio il rcd SPT per memorizzare immagine variata
233600160708         chain(n) (I50cpo:'SPT') TICPI01L;
233700160831         IF  not %found(TICPI01L);
233800160831           clear TICPI_30;
233900160831         ENDIF;
234000160708         clear TRMK30DS;
234100160708         IMK30pru = knmus;
234200160708         IMK30noj = knmeb;
234300160901         IF  I50pgm = *blanks;
234400160901           IMK30pgm = 'TRMK50R';
234500160901         ELSE;
234600160901           IMK30pgm = I50pgm;
234700160901         ENDIF;
234800160708         IMK30immag = 'D';
234900160708         IMK30cta = 'M';
235000160711         IMK30tvp = 'D';
235100160708
235200160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
235300160708
235400160708       ENDSR;
235500080702
235600080206       //--------------------------------------------------------------
235700080206       //?Operazioni finali.
235800080206       //--------------------------------------------------------------
235900080206       BEGSR RoutEnd;
236000080627
236100080704         if i50tla<>' '    ;
236200080704         // Chiusura pgm   ;
236300080704         clear tibs02ds  ;
236400080704         t02tla='C'      ;
236500080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
236600080704
236700080206         *inLR = *on;
236800080704         endif             ;
236900080704
237000080206         return;
237100080206
237200080206       ENDSR;
237300080206
237400080206      /end-free
237500080206
237600080206       //--------------------------------------------------------------
237700080206       //?Schiere a tempo di compilazione.
237800080206       //--------------------------------------------------------------
237900080206
238000080410** - MSG ------------------------------------------------------------------ -*
238100081003Info Commerciale obbligatoria                                                   01
238200080703Per Info Comm. non presente (N), non indicare gli altri dati                   02
238300081003La Info Comm. prevede l'inserimento del Fatturato presunto OBBLIGATORIO        03
238400081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx: enter per forzare     04
238500081003La  Info Comm. prevede l'inserimento del Fatturato presunto: enter per forzare    05
238600080704La  Info Comm. prevede l'inserimento del numero Spedizioni: enter per forzare  06
238700080704Info Comm. non più in essere:verificarla e se non serve annullarla.ENTER forza 07
238800080704Info Comm. non più in essere: ANNULLARLA !!                                    08
238900081003La somma delle percentuali per questa categoria di info commerc. supera 100%   09
239000081003La Info Comm. prevede l'inserimento del numero Spedizioni OBBLIGATORIO         10
239100081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx OBBLIGATORIO            11
239200081003La  percentuale di questa info commerciale supera 100%                         12
239300081003La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx  deve essere 100% 13
239400081007Inserire concorreti Logistica SOLO SE il cliente prevede Logist.in outsourcing 14
239500081007Se Logistica in outsourcing obbligatorio inserire i concorrenti logistica      15
239600110511"Motivo non affidamento a noi" da immettere solo se "Affidato a BRT" < 100%    16
239700140716ATTENZIONE!! Il valore massimo consentito è di 999 milioni                     17
239800081007"MOTIVO NON AFFIDAMENTO A NOI" errato                                          18
239900081007Indicare OBBLIGATORIAMENTE la presenza (S) o assenza(N) della Info commerciale 19
240000081007Immettere solo  '?'  se si vuole attivare la ricerca                           20
240100081125Immettere "S o N" per indicare la presenza o l'assenza della Info Comm.        21
240200081007Immettere"S o N"per indicare la presenza o l'assenza della Info Comm:ENTR forza22
240300140717ATTENZIONE!! Il valore inserito e > di 1 milione - Enter per forzare.          23
240400140716Il numero delle spedizioni non può essere superiore alla spesa trasporti.      24
240500140716Non ammesso un valore negativo                                                 25
240600081125Se "Logistica interessante per noi", indicare se in Outsourcing                26
240700110511Se non tutto "Affidato BRT" obbligatorio un Motivo NON AFFIDAMENTO a noi       27
240800081126I"Motivi non Affidamento a noi" devono essere diversi: uno per ITA uno per EST 28
240900081126Se immesso"9999-NON Rilevabile", l'altro motivo deve essere vuoto              29
241000081126Codice già presente !!                                                         30
241100120125Obbligatorio aumentare la % di affidamento a BRT!!                             31
241200120109Alcune Info sono più vecchie di 1 anno. Verificare se ancora corrette.         32
241300120125Obbligatorio azzerare la % di affidamento a BRT!!                              33
241400120125Obbligatorio diminiuire la % di affidamento a BRT!!                            34
241500140715Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                 35
241600140715Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!                36
241700140725Inserire Paesi Esteri SOLO SE il cliente prevede trasporto Estero.             37
241800140922Se non tutto "Affidato BRT" obbligatorio ripartizione spesa NON affidato a BRT 38
241900140717ATTENZIONE!! Il valore inserito e > di 10.000.                                 39
242000140919Non affidato a BRT non congruente con % utilizzo concorrenti                   40
242100140919Non affidato a BRT non congruente con % ripartizione spesa trasporti           41
242200140919% Ripartizione Spesa Trasp. a 0. Non impostare il relativo non affidato a BRT  42
