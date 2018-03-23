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
010400180216     d  min_nrr              378    379i 0                                      Subfile min rrn
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
018700180216     d savForzaAgg     s               n   inz(*off)
018800080206
018900080627     d $Video          s              2    inz('S1')
019000080208     d S01nrr          s              4  0 inz
019100080627     d savcat          s              2    inz
019200080627     d Wcat            s              2    inz
019300081006     d WIFS            s              4    inz
019400080702     d Wifo            s              3    inz
019500081006     d w0040           s              4  0 inz
019600081007     d w005a           s              5    inz
019700081006     d XX              s              3  0 inz
019800080627     d Indx            s              3  0 inz
019900080703     d Primavolta      s              1    inz
020000080925     d Totprc          s              4  0 inz
020100081003     d Wctrprc         s              1    inz
020200081003     d Wdescat         s             21    inz
020300081215     d SPTPFT          s                   like(cpipft) inz
020400100302     d savspt          s                   like(cpipft) inz
020500081006     d LOG40           s              1    inz
020600081006     d OUTFSN          s              1    inz
020700081006     d OUTnrr          s              4  0 inz
020800081023     d LOGFSN          s              1    inz
020900081023     d LOGnrr          s              4  0 inz
021000081126     d MOInrr          s              4  0 inz
021100081015     d Waggiocpo       s              1    inz
021200081007     d BART_10         s              4  0 inz
021300081015     d InfoITA         s              1    inz
021400081015     d InfoEST         s              1    inz
021500100226     d InfoATR         s                   like(cpival)
021600100302     d SavATR          s                   like(cpival)
021700081126     d w008a           s              8    inz
021800081126     d MOIconta        s              3  0 inz
021900081126     d yy              s              3  0 inz
022000081126     d trovacpi        s              1    inz
022100081215     d savmod          s                   like(i50mod) inz
022200090520     d ultdata         s              8  0 inz
022300120110     d sav_bar_10      s                   like(bart_10)
022400120109     d wOggi           s              8  0 inz
022500140721
022600140715     d sav_vscpft      s                   like(vscpft) inz
022700140715     d sav_vscsna      s                   like(vscsna) inz
022800140716     d sav_valrms      s             12  3 inz
022900140716     d wVALrms         s             12  3 inz
023000140718     d noBRTpft        s                   like(cpipft) inz
023100140721     d siBRTpft        s                   like(cpipft) inz
023200140718     d ifoNRFnrr       s                   like(S01nrr) inz
023300140721     d ifoNROnrr       s                   like(S01nrr) inz
023400140721     d ifoNERnrr       s                   like(S01nrr) inz
023500140721     d ifoNAEnrr       s                   like(S01nrr) inz
023600140725     d ifoNTRnrr       s                   like(S01nrr) inz
023700140721     d savPRFval       s                   like(cpival) inz
023800140721     d savPROval       s                   like(cpival) inz
023900140721     d savAERval       s                   like(cpival) inz
024000140721     d savCAEval       s                   like(cpival) inz
024100140725     d savATRval       s                   like(cpival) inz
024200140721     d savNRFpft       s                   like(cpipft) inz
024300140721     d savNROpft       s                   like(cpipft) inz
024400140721     d savNERpft       s                   like(cpipft) inz
024500140721     d savNAEpft       s                   like(cpipft) inz
024600140725     d savNTRpft       s                   like(cpipft) inz
024700180216     d wnrr            s                   like(C01csr) inz
024800080414
024900080605     d Dataiso         s               d   datfmt(*iso)
025000080605     d Dataymd         s               d   datfmt(*ymd)
025100080605     d Datadmy         s               d   datfmt(*dmy)
025200120109     d wIFOdul         s              8  0
025300120109     d wIFOduliso      s               d   datfmt(*ISO)
025400140725     d wData           s              8  0
025500140725     d Dataeur         s               d   datfmt(*eur)
025600080604     d
025700080605     d                 DS
025800080605     d  AA                     1      4  0
025900080605     d  MM                     5      6  0
026000080605     d  GG                     7      8  0
026100080605     d DATA                    1      8  0
026200081009     d
026300081126     d CoRicercaMOI    c                   const('Ricerca motivo ITA/EST''?''')
026400081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
026500080206
026600080206      //---------------------------------------------------------------
026700080206      //?Definizione procedure usate.
026800080206      //---------------------------------------------------------------
026900080414      /copy gaitrasrc/srcprotopr,tibs02r
027000080414      /copy gaitrasrc/srcprotopr,tibs34r
027100081008      /copy gaitrasrc/srcprotopr,TNTB70r
027200081023      /copy gaitrasrc/srcprotopr,TRMK51r
027300100302      /copy gaitrasrc/srcprotopr,TRMK52r
027400080414      /copy gaitrasrc/srcprotopr,trmk_opm
027500160708
027600160708     d TRMK30R         pr                  extpgm('TRMK30R')
027700160708     d  trmk30ds                           likeds(trmk30ds)
027800160708     d  tncpo_30                           likeds(tncpo_30)
027900160708     d  tncpo1_30                          likeds(tncpo1_30)
028000160708     d  ticpi_30                           likeds(ticpi_30)
028100080206
028200080206      //---------------------------------------------------------------
028300080206      //?Riepilogo indicatori.
028400080206      //---------------------------------------------------------------
028500080207      // - Comuni
028600080207      // 28    : Emissione messaggio di errore a video
028700080207      // - C01/S01
028800080207      // 30    : Condiziona SFLDSP    (*not)
028900080207      // 31    : Condiziona SFLDSPCTL (*not)
029000080207      // 30+31 : Condiziona SFLCLR
029100080207      // 32    : Condiziona SFLNXTCHG in S01
029200080207      // 50    : Errore: Opzione errata
029300080207      // - D01
029400080207      // - Comuni
029500080207      // 99    : Generico di Errore
029600080206      //---------------------------------------------------------------
029700080206
029800080206      //---------------------------------------------------------------
029900080206      //?M A I N - L I N E
030000080206      //---------------------------------------------------------------
030100080206
030200080627     c     *Entry        plist
030300080206     c                   parm                    KPJBA
030400080627     C                   PARM                    trMK50ds
030500080627
030600080627     C                   CLEAR                   O50F12
030700080627     C                   CLEAR                   O50F03
030800080627     C                   CLEAR                   O50F06
030900080702
031000080206      /free
031100080206
031200081215 1      if  i50tla<>'C'   ;
031300080925
031400080206       // Operazioni iniziali
031500080206       exsr RoutInz;
031600080206
031700081215 2      if  i50mod<>'C'   ;
031800081215
031900080206       // Gestione video
032000081215 3     DOW $Fine = *off;
032100081215 4       select;
032200080530
032300080530         // Videata di selezioni
032400080627           when $Video = 'S1';
032500080627             exsr GesS01;
032600120109           when $Video = 'W1';
032700120109             exsr GesW01;
032800080530
032900080702           other   ;
033000080206             $Fine = *on;
033100081215 4       endsl;
033200081215 3     ENDDO;
033300081215
033400081215 x2    else   ;
033500081215
033600081215       // modalità CONTROLLO : carico il sfl
033700081215       //                    solo ai fini del controllo
033800081215       exsr InzS01;
033900081215       exsr Contrs01   ;
034000081215
034100081215  3    if ErrGenerico=*off;
034200081215          o50ifotot='S';
034300081215  3    endif  ;
034400080925
034500081215 2     endif    ;
034600081215 1     endif    ;
034700080206
034800080206       // Operazioni finali
034900080206       exsr RoutEnd;
035000080206
035100080206       //--------------------------------------------------------------
035200080206       //?Operazioni iniziali.
035300080206       //--------------------------------------------------------------
035400080206       BEGSR RoutInz;
035500080627       $inzs01=*on;
035600080929       clear vsctes2         ;
035700180216       savForzaAgg = *off;
035800080206
035900080703       // Solo la prima volta
036000080703 1     if primavolta=' '   ;
036100080703
036200080206         // Reperimento dati job
036300080206         exsr DatiJob;
036400081003
036500081003         // Se sono EDP accendo indicatore
036600081003         if %subst(knmus:1:3)<>'EDP'    ;
036700081003         UTENonEDP=*on   ;
036800081003         endif      ;
036900080627
037000080627         // Carico tabella per divisa corrente
037100080627         reset TIBS02ds;
037200080627         T02sif = knsif;
037300080627         T02cod = 'GED';
037400080627         T02ke1 = '1'  ;
037500080627         T02mod = 'C'  ;
037600080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037700080627
037800080703 2       if t02err=' '  ;
037900080627         dGED=t02uni    ;
038000080627         else   ;
038100080627         clear DGED    ;
038200080703 2       endif   ;
038300080627
038400080627         // Carico tabella info commerciali
038500080627         xx=0                     ;
038600080627         setll ('IFO') tntbe01l   ;
038700080627         READE ('IFO') tntbe01l   ;
038800080627
038900080703 2       dow not %eof(tntbe01l)   ;
039000080703 3       if tbeatb=' '    ;
039100080627           difo=tbeuni    ;
039200080627           xx=xx+1        ;
039300080703           ifo(xx)=tbeke1 ;
039400080627           ifods(xx)=difo ;
039500080627           ifocg(xx)=§ifoifg ;
039600080627           // ordino in base alla categoria e  num.ordinamento
039700080703           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
039800080703 3       endif               ;
039900080627
040000080627         READE ('IFO') tntbe01l   ;
040100080703 2       enddo    ;
040200080627
040300080627         sorta  IFOOR  ;
040400080703
040500080703         Primavolta='N'   ;
040600080703 1       endif      ;
040700080702
040800080703         Infoint=*off   ;
040900080704         Infoyell=*off   ;
041000080703         InfoImmiss=*off   ;
041100080703         Ifosott=*off   ;
041200080703         Protriga=*off   ;
041300080703         ProtFat =*off   ;
041400081003         ProtVal  =*off   ;
041500140715         ProtVald = *off;
041600140715         ProtVala = *off;
041700080703         Protsped =*off   ;
041800080703         Protfsn  =*off   ;
041900080703         clear wcat  ;
042000080703         clear wifo  ;
042100080704         $Fine=*off   ;
042200140929         clear wVALrms;
042300080703
042400080702         // Vedo se modo gestione  o interrogazione
042500080703 1       if i50mod='I' ;
042600080702         InfoInt =*on  ;
042700080703         vsctes2='INTERROGAZIONE'    ;
042800080703 1       endif;
042900080703
043000080703         if i50pgm=*blanks   ;
043100080703         vscpgm='TRMK50R'   ;
043200080703         else     ;
043300080703         vscpgm=i50pgm     ;
043400080703         endif   ;
043500080703
043600080703
043700080703         // IMPOSTO IL potenziale in visualizzazione
043800080703         vsccpo=%editc(i50cpo:'X')   ;
043900080703         vscrag=i50rag   ;
044000120111
044100120111         //?Attivo i tasti funzione F03 e F12
044200120111           F03Attivo = *on;
044300120111           F12Attivo = *on;
044400080703
044500120105         //?05/12/2012
044600120105         //?Visto che il campo per potenziale annullato non è più usato
044700120105         //?lo recuperiamo per utilizzarlo come flag per controlli da
044800120105         //?fare sulle info.
044900120110         //?Nessun controllo
045000120110           IF  I50atb = ' ';
045100120110             wAumenta10    = *off;
045200120510             wAumenta10f   = *off;
045300120110             wAzzera10     = *off;
045400120110             wDiminuisci10 = *off;
045500120110           ENDIF;
045600120110         //?Controllo per azzeramento
045700120110           IF  I50atb = '0';
045800120110             wAzzera10     = *on;
045900120110             wAumenta10    = *off;
046000120510             wAumenta10f   = *off;
046100120110             wDiminuisci10 = *off;
046200120111             F03Attivo = *off;
046300120111             F12Attivo = *off;
046400120110           ENDIF;
046500120110         //?Controllo per aumento
046600120105           IF  I50atb = '+';
046700120110             wAumenta10    = *on;
046800120510             wAumenta10f   = *off;
046900120110             wAzzera10     = *off;
047000120110             wDiminuisci10 = *off;
047100120111             F03Attivo = *off;
047200120111             F12Attivo = *off;
047300120105           ENDIF;
047400120510         //?Controllo per aumento semi obbligatorio
047500120510           IF  I50atb = '%';
047600120510             wAumenta10f   = *on;
047700120510             wAumenta10    = *off;
047800120510             wAzzera10     = *off;
047900120510             wDiminuisci10 = *off;
048000120510             F03Attivo = *off;
048100120510             F12Attivo = *off;
048200120510           ENDIF;
048300120110         //?Controllo per diminuzione
048400120109           IF  I50atb = '-';
048500120110             wDiminuisci10 = *on;
048600120110             wAumenta10    = *off;
048700120510             wAumenta10f   = *off;
048800120110             wAzzera10     = *off;
048900120111             F03Attivo = *off;
049000120111             F12Attivo = *off;
049100120105           ENDIF;
049200081003
049300081003         // Se già  inserite tutte le info, non si possono più annulla
049400081003         //  ma solo sistemare
049500081003         if i50obl=' '  and i50ifotot='S'    ;
049600081003         i50obl='S'   ;
049700081003         endif        ;
049800080627
049900080703         // VERIFICO SE ci sono già dei dati per il potenziale
050000080703         setll   i50cpo  ticpi01l    ;
050100080703         reade   i50cpo  ticpi01l    ;
050200080703 1       dow not %eof(ticpi01l) and cpiatb<>' ' ;
050300080703         reade   i50cpo  ticpi01l    ;
050400080703 1       enddo   ;
050500080703
050600080703 1       if %eof(ticpi01l) ;
050700080703         Infoimmiss=*on     ;
050800080929 1       if vsctes2= *blanks    ;
050900080703         vsctes2='  IMMISSIONE  '    ;
051000080929         endif   ;
051100080703         else    ;
051200080929 1       if vsctes2= *blanks    ;
051300080703         vsctes2='  VARIAZIONE  '    ;
051400080703 1       endif    ;
051500080929 1       endif    ;
051600080703
051700080627         ENDSR;
051800080206
051900080206       //--------------------------------------------------------------
052000080206       //?Reperimento Dati del job (Utente/Operativi).
052100080206       //--------------------------------------------------------------
052200080206       BEGSR DatiJob;
052300080206
052400080206         in(E) §AzUte;
052500080206         if NOT %error;
052600080206           in(E) §DatiUte;
052700080206         endif;
052800080206         if %error or RSut = *blanks;
052900080206           clear TIBS34ds;
053000080206           tibs34r(tibs34ds);
053100080206           in §AzUte;
053200080206           in §DatiUte;
053300080206         endif;
053400080206
053500080206       ENDSR;
053600080206
053700080206       //--------------------------------------------------------------
053800080627       //?Gestione SFL 01
053900080206       //--------------------------------------------------------------
054000080409       BEGSR GesS01;
054100080207
054200080207         // Inizializzazione videata
054300080409         if  $InzS01 = *on;
054400080409            exsr InzS01;
054500080703         // Posizionamento cursore
054600080703           s01nrr = 1;
054700080703           sfldsp_n=*off;
054800080409            $InzS01  = *off;
054900080207         endif;
055000080207
055100140725         //?Cerco l'ultima data di conferma info commerciali
055200140909           chain(n) I50cpo TNCPO11L;
055300140909           IF  %found(TNCPO11L);
055400140909             vdulconf = CPO1ducifo;
055500140909           ENDIF;
055600140909           IF  vdulconf > *zeros;
055700140909             dataISO = %date(vdulconf:*iso);
055800140909             dataEUR = dataISO;
055900140909             vdulconf = %dec(dataEUR);
056000140909           ENDIF;
056100160708         //?Se richiamto pgm per variazione/immissione salvo l'immagine prima
056200160708           IF  I50mod <> 'I';
056300160708             exsr ImmaginePrima;
056400160708           ENDIF;
056500080207
056600080207         // Emissione Testata e Piede con tasti funzionali abilitati
056700080207         if  errGenerico = *off;
056800080702           write  MK50Z01;
056900080207         endif;
057000080703
057100080703         // Posizionamento cursore
057200080703         if  C01csr  > *zero;
057300080703           C01rcd = C01csr;
057400080703         endif  ;
057500080207
057600080207         // Emissione videata
057700080702         exfmt  MK50C01;
057800080410
057900080207         reset errMessage;
058000080207         reset errGenerico;
058100080702         clear Vscmsg;
058200081003         errVAL=*off     ;
058300140715         errVALD = *off;
058400080703         errPFT=*off     ;
058500080703         errSNA=*off     ;
058600080703         errFSN=*off     ;
058700080704         errANN=*off     ;
058800120109
058900120109         //?Cerco l'ultima data variazione info commerciali
059000120124         //?Se NON è interrogazione e non è immissione
059100120109           clear dCPO01;
059200120124           IF  not InfoInt and not Infoimmiss;
059300120109             chain(n) I50cpo TNCPO01L;
059400120109             IF  %found(TNCPO01L);
059500120109               dCPO01 = CPOrst;
059600120109             ENDIF;
059700120109             IF  §CPOifodul > *zeros;
059800120109               wIFOdul    = %dec(§CPOifodul:8:0);
059900120109               wIFOdulISO = %date(wIFOdul : *ISO);
060000120109               wOggi = %dec(%date());
060100120109               dataISO = %date(wOggi : *ISO);
060200120109         //?Se la data ultima variazione è più vecchia di 1 anno da oggi windows
060300120109         //?di avviso all'utente
060400120110               IF  %diff(dataISO : wIFOdulISO : *months) > 12 and not wForzaAgg;
060500120109                 $Video = 'W1';
060600120109                 ErrGenerico = *on;
060700120109                 ErrMessage  = *on;
060800120109                 VSCmsg      = msg(32);
060900120109                 wForzaAgg   = *on;
061000120109                 leavesr;
061100120109               ENDIF;
061200120109             ENDIF;
061300120109           ENDIF;
061400080207
061500080523 1       SELECT;
061600080207
061700080207         // - F3=Fine
061800080523 1         WHEN  dsp_aid = c_F03;
061900080409            $Fine = *on;
062000081006            o50f03='S'   ;
062100080207
062200080207         // - F12=Ritorno
062300080523 1         WHEN  dsp_aid = c_F12;
062400080702            $Fine = *on;
062500081006            o50f12='S'   ;
062600180216
062700180216        // - F16=Annullamento
062800180216          WHEN  dsp_aid = c_F16;
062900180216            exsr F16S01;
063000080414
063100080530 x1      // Invio / F6
063200080207           OTHER;
063300081006
063400081006        // Non effettuo controlli in interrogazione
063500081006 1           if i50mod<>'I' ;
063600081006               exsr ContrS01 ;
063700081006 2             if  errGenerico = *on;
063800081006                 leavesr;
063900081006 2             endif;
064000081006 2           endif;
064100080530
064200081023         // F6=conferma Aggiornamento  F10=anche stampa
064300081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
064400080702           exsr   ConfAggio ;
064500081006           o50f06='S'   ;
064600081023
064700081023              if dsp_aid=c_F10;
064800081023               kpjbu=trmk50ds  ;
064900081023               callp TRMK51R (kpjba)   ;
065000081023              endif    ;
065100081023
065200080704           $Fine = *on;
065300080530           endif  ;
065400080207
065500080523 1       ENDSL;
065600080207
065700080207       ENDSR;
065800120109
065900120109       //--------------------------------------------------------------
066000120109       //?Gestione Window errori
066100120109       //--------------------------------------------------------------
066200120109       BEGSR GesW01;
066300120109
066400120109         reset ErrMessage;
066500120109         reset ErrGenerico;
066600120109
066700120109         w1riga = vscmsg;
066800120109         exfmt  MK50W01;
066900120109
067000120109         clear Vscmsg;
067100120109         errVAL  = *off;
067200140715         errVALD = *off;
067300120109         errPFT  = *off;
067400120109         errSNA  = *off;
067500120109         errFSN  = *off;
067600120109         errANN  = *off;
067700120109
067800120109         $Video = 'S1';
067900120109
068000120109       ENDSR;
068100080207
068200080526       //--------------------------------------------------------------
068300080702       //?controlli SFL01
068400080409       //--------------------------------------------------------------
068500080409       BEGSR ContrS01;
068600140716
068700081215       clear o50ifomin;
068800081215       clear wctrprc  ;
068900081006       clear wdescat   ;
069000081006       clear totprc   ;
069100081006       clear sptpft   ;
069200081006       clear OUTFSN   ;
069300081006       clear OUTnrr   ;
069400081023       clear LOGFSN   ;
069500081023       clear LOGnrr   ;
069600081006       clear LOG40    ;
069700081007       clear bart_10  ;
069800081015       clear infoITA  ;
069900081015       clear infoEST  ;
070000081126       clear IFOval   ;
070100081126       clear MOIconta ;
070200081126       clear MOInrr   ;
070300100226       clear InfoATR  ;
070400140718       clear noBRTpft;
070500140721       clear siBRTpft;
070600140721       clear savPRFval;
070700140721       clear savPROval;
070800140721       clear savAERval;
070900140721       clear savCAEval;
071000140725       clear savATRval;
071100140721       clear savNRFpft;
071200140721       clear savNROpft;
071300140721       clear savNERpft;
071400140721       clear savNAEpft;
071500140725       clear savNTRpft;
071600140725       wEstero = *off;
071700140716
071800081126       yy=1           ;
071900080703       s01nrr=1  ;
072000080703       chain s01nrr    mk50s01    ;
072100080703
072200081006 0     dow %found    ;
072300080925       // Escludo i record di descrizione categoria
072400081006 1     if vshimm= 'C'    ;
072500081003
072600081003       // A cambio di categoria, se c'era controllo obbligatorio del
072700081003       //  100%   controllo ed eventualmente segnalo errore
072800081006       EXSR CambioCAT    ;
072900081006       if errGenerico=*on  ;
073000081006       leavesr;
073100081006       endif              ;
073200081006
073300081006x1     else    ;
073400081006
073500080925       pftblu=*off       ;
073600080925
073700081003       // se valori calcolati da pgm li pulisco prima dei controlli
073800081007 2     if vshsnf=' ' and vshprpft=' '   ;
073900080925       clear vscpft   ;
074000140717       // clear vscvft   ;
074100081007 2     endif          ;
074200080703
074300080703       // Imposto la divisa, se impostato il fatturato
074400140717 2     //if vscpft>0 and vscvft=*blanks   ;
074500140717       //vscvft=§gedcr   ;
074600140717       //errGenerico = *on;
074700140717 2     //endif    ;
074800080703
074900140717 2     //if vscpft=0 and vscvft<>*blanks   ;
075000140717       //clear vscvft   ;
075100140717       //errGenerico = *on;
075200140717 2     //endif   ;
075300140716
075400140716       //?Se valore fatturato < 0 errore
075500140716       IF  VSCpft < 0;
075600140716         VSCmsg = msg(25);
075700140716         errPFT=*on;
075800140716         EXSR AggioSFL   ;
075900140716         leavesr;
076000140716       ENDIF;
076100140716
076200140716       //?Se valore fatturato impostato e > di 999000000 errore
076300140716       IF  VSCpft > 999000000;
076400140716         VSCmsg = msg(17);
076500140716         errPFT=*on;
076600140716         EXSR AggioSFL   ;
076700140716         leavesr;
076800140716       ENDIF;
076900140716
077000140716       //?Se valore fatturato impostato e > di 999999 warning
077100140716       IF  VSCpft > 999999 and not wForzaPft;
077200140716         VSCmsg = msg(23);
077300140716         errPFT=*on;
077400140716         wForzaPft = *on;
077500140716         EXSR AggioSFL   ;
077600140716         leavesr;
077700140716       ENDIF;
077800140716
077900140716       //?Le spedizioni non possono essere superiori del valore fatturato
078000140722       IF  VSCsna > VSCpft and VSCifo = 'SPT';
078100140716         VSCmsg = msg(24);
078200140716         errPFT=*on;
078300140716         EXSR AggioSFL   ;
078400140716         leavesr;
078500140716       ENDIF;
078600140716
078700140716       //?Se spedizioni < 0 errore
078800140716       IF  VSCsna < 0;
078900140716         VSCmsg = msg(25);
079000140716         errSNA=*on;
079100140716         EXSR AggioSFL   ;
079200140716         leavesr;
079300140716       ENDIF;
079400140723
079500140723       //?Se VAL < 0 errore
079600140723       IF  vscval < 0;
079700140723         VSCmsg = msg(25);
079800140723         errVAL=*on;
079900140723         EXSR AggioSFL   ;
080000140723         leavesr;
080100140723       ENDIF;
080200140723
080300140723       //?Se VALD < 0 errore
080400140723       IF  vscvald < 0;
080500140723         VSCmsg = msg(25);
080600140723         errVALD=*on;
080700140723         EXSR AggioSFL   ;
080800140723         leavesr;
080900140723       ENDIF;
081000080703
081100080704       // se info da annullare e non c'e' la "A", msg forzabile
081200081215       //  non faccio se modalità controlli
081300081215 2     if i50mod<>'C' and vshann='S'  and vscatb=' '  ;
081400080704           errANN=*on;
081500080704           // se  contiene dati, msg forzabile
081600081007 3         if vscfsn='S'or vscval>0 or vscpft>0 or vscsna>0   ;
081700080704           vscmsg = Msg(07);
081800080704           vshann='X'      ;
081900080704           // Se non contiene dati abbligatorio l'annullamento
082000081007 x3        else            ;
082100080704           vscmsg = Msg(08);
082200081007 3         endif           ;
082300080704           EXSR AggioSFL   ;
082400080704           leavesr;
082500081007 2     endif    ;
082600110707
082700110707       // se c'e' "O"  obbligatorio l'annullamento
082800110707 2     if i50mod<>'C' and vshann='O'  and vscatb=' '  ;
082900110707           errANN=*on;
083000110707           vscmsg = Msg(08);
083100110707           EXSR AggioSFL   ;
083200110707           leavesr;
083300110707 2     endif    ;
083400080704
083500080703       // Se richiesto inserimento obbligatorio, per i dati
083600080703       //  obbligatori da tabella segnalo errore
083700081007 2     if i50obl='S' and vshobl='S'                 ;
083800081007 3     if vscatb='A'    or
083900081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
084000081003         if vshsne<>' ';
084100080703           errFSN=*on;
084200081003         endif   ;
084300081003         if vshsnv<>' ';
084400081003           errVAL=*on;
084500081003         endif   ;
084600140715         IF  vshsnvd <> *blanks;
084700140715           errVALD = *on;
084800140715         ENDIF;
084900081003         if vshsnf<>' ';
085000081003           errPFT=*on;
085100081003         endif   ;
085200080703           vscmsg = Msg(01);
085300080703           EXSR AggioSFL   ;
085400080703           leavesr;
085500081007 3     endif   ;
085600081007 2     endif   ;
085700081007
085800081007       // Non accetto se non previsto il valore '?'
085900081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
086000081007           errFSN=*on;
086100081007           vscmsg = Msg(20);
086200081007           EXSR AggioSFL   ;
086300081007           leavesr;
086400081007 3     endif    ;
086500081007       if vshifgv<>'?' and vscfsn='?'  ;
086600081007           errFSN=*on;
086700081007           vscmsg = Msg(21);
086800081007           EXSR AggioSFL   ;
086900081007           leavesr;
087000081007 3     endif    ;
087100080703
087200080703       // Se immesso "N" non indicare gli altri campi
087300081007 2     if vscfsn='N' and (vscval>0 or vscpft>0 or vscsna>0)   ;
087400080703       if vshsnf<>' '    ;
087500080703           errPFT=*on;
087600080703       else    ;
087700081003          errVAL=*on ;
087800080703       endif  ;
087900080703           vscmsg = Msg(02);
088000080703           EXSR AggioSFL   ;
088100080703           leavesr;
088200081007 2     endif   ;
088300080703
088400081003       // Se immesso almeno un dato, controllo gli obbligatori ed
088500081215       //   i facoltativi. I  facoltativi non li guardo in modalità
088600081215       //   solo controllo
088700081031       if vscatb =' ' and
088800081031 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval>0)            ;
088900081003
089000081007 3     if vscpft=0    and vshsnf='O'   ;
089100081003           errPFT=*on;
089200081003           vscmsg = Msg(03);
089300081003           EXSR AggioSFL   ;
089400081003           leavesr;
089500081007 3      endif    ;
089600100503
089700100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
089800100503 3     if i50obl='S' and
089900100503          I50mod<>'C' and  vscpft=0    and vshsnf='S'   ;
090000081003           errPFT=*on;
090100081003           vscmsg = Msg(05);
090200081003           vshsnf='X'      ;
090300081003           EXSR AggioSFL   ;
090400081003           leavesr;
090500081007 3     endif    ;
090600081007 3     if vscsna=0    and vshsns='O'   ;
090700081003           errSNA=*on;
090800081003           vscmsg = Msg(10);
090900081003           EXSR AggioSFL   ;
091000081003           leavesr;
091100081007 3     endif    ;
091200100503
091300100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
091400100503 3     if i50obl='S' and
091500100503          i50mod<>'C' and vscsna=0    and vshsns='S'   ;
091600081003           errSNA=*on;
091700081003           vscmsg = Msg(06);
091800081003           vshsns='X'      ;
091900081003           EXSR AggioSFL   ;
092000081003           leavesr;
092100081007 3     endif    ;
092200081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
092300081007           errFSN=*on;
092400081007           vscmsg = Msg(19);
092500081007           EXSR AggioSFL   ;
092600081007           leavesr;
092700081007 3     endif    ;
092800100503 3     if i50obl='S' and
092900100503 3        i50mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
093000081007           errFSN=*on;
093100081007           vscmsg = Msg(22);
093200081007           vshsne='X'      ;
093300081007           EXSR AggioSFL   ;
093400081007           leavesr;
093500081007 3     endif    ;
093600081007 3     if (vscval=0    and vshsnv='O')  or
093700100503          (vscval=0    and vshsnv='S' and i50mod<>'C'AND I50OBL='S')   ;
093800081007 4        if vshsnv='O'    ;
093900081003           vscmsg = Msg(11);
094000081003          else   ;
094100081215             vscmsg = Msg(04);
094200081215             vshsnv='X'      ;
094300081215 4        endif  ;
094400081007 4        if vsctva='%  '   ;
094500081003          %subst(vscmsg:37:18)='della % fatturato '     ;
094600081007 4        endif     ;
094700081007 4        if vsctva='KG '   ;
094800081003          %subst(vscmsg:37:18)='del peso in Kg.   '     ;
094900081007 4        endif     ;
095000081215
095100081003           errVAL=*on;
095200081003           EXSR AggioSFL   ;
095300081003           leavesr;
095400081215 3     endif    ;
095500140721
095600140715 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
095700140715           (vscvald = 0 and vshsnvd = 'S'   and
095800140715            I50mod <> 'C' and I50obl = 'S');
095900140715 4        IF  vshsnvd = 'O';
096000140715            vscmsg = Msg(01);
096100140715          ELSE;
096200140715            vscmsg = Msg(01);
096300140715            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
096400140715            vshsnvd = 'X';
096500140715 4        ENDIF;
096600140715          errVALD = *on;
096700140715          EXSR AggioSFL;
096800140715          leavesr;
096900140715 3     ENDIF;
097000140715
097100140715       //?Mi salvo il valore del traffico totale
097200140715 3     IF  vscifo = 'SPT';
097300140716         sav_vscpft = vscpft;
097400140716         sav_vscsna = vscsna;
097500140715       ENDIF;
097600081003
097700081003       // se Valore in % --> non deve superare 100%
097800081007 3     if  vscval>0 and vsctva='%  '    ;
097900081007 4       if vscval>100    ;
098000081003          errVAL=*on ;
098100081003          vscmsg = Msg(12);
098200081003          EXSR AggioSFL   ;
098300081003          leavesr;
098400081007 4       endif      ;
098500081003
098600080925       // Sommo la percentuale se presente per ogni categoria
098700080925       //  se supera 100% segnalo errore
098800081006       //  solo se previsto controllo sulla %
098900081007 4     if wctrprc='P'     ;
099000081006
099100110707       // Non sommo le info annullate perchè non le devo considerare valide
099200110707       if vshann=' '   ;
099300110707         totprc=totprc+vscval    ;
099400110707       endif  ;
099500110707
099600081007 5       if totprc>100    ;
099700081003         errVAL=*on ;
099800080925         vscmsg = Msg(09);
099900080925         EXSR AggioSFL   ;
100000080925         leavesr;
100100081007 5       endif      ;
100200081007 4     endif      ;
100300080925
100400081003       // Se immessa la %, visualizzo in base a SPT
100500080925       exsr   VISUALimp     ;
100600081007 3     endif      ;
100700081007 2     endif      ;
100800140716
100900140716       //?Se Valore con decimali in % --> non deve superare 100%
101000140716 3     IF  vscvald <> 0 and vsctva = '%  ';
101100140716 4       IF  vscvald > 100;
101200140716           errVALD = *on;
101300140716           vscmsg = Msg(12);
101400140716           EXSR AggioSFL   ;
101500140716           leavesr;
101600140716 4       ENDIF;
101700140716 4     ENDIF;
101800140715
101900140715       //?Per Ricavo medio spedizione
102000140715 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
102100140715       //?calcolo il valore
102200140715 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
102300140715           wVALrms = sav_vscpft / sav_vscsna;
102400140715         ELSE;
102500140715           wVALrms = 0;
102600140715         ENDIF;
102700140715       //?reimposto il flag di forzatura
102800140715         IF  wVALrms <> sav_valrms;
102900140716           wForzaRms = *off;
103000140715         ENDIF;
103100140715       //?imposto il campo del video
103200140919         IF  wVALrms > 999;
103300140919           vscvald = 999;
103400140715         ELSE;
103500140715           vscvald = wVALrms;
103600140715         ENDIF;
103700140715       //?msg info se inferiore a 4
103800140716         IF  VSCvald < 4 and not wForzaRms and
103900140715             wVALrms <> sav_valrms;
104000140715           errVALD = *on;
104100140715           vscmsg = msg(35);
104200140715           exsr AggioSFL;
104300140716           wForzaRms = *on;
104400140715           sav_valrms = wVALrms;
104500140715           leavesr;
104600140715         ENDIF;
104700140715       //?msg info se superiore a 50
104800140716         IF  VSCvald > 50 and not wForzaRms and
104900140715             wVALrms <> sav_valrms;
105000140715           errVALD = *on;
105100140715           vscmsg = msg(36);
105200140715           exsr AggioSFL;
105300140716           wForzaRms = *on;
105400140715           sav_valrms = wVALrms;
105500140715           leavesr;
105600140715         ENDIF;
105700140715       ENDIF;
105800140716
105900140716       // se INFO obbligatoria
106000140716       // errore se info non inserita
106100140721       // flag di comodo per non richiedere obbligatoria la INFO ora in fase di test
106200140721       // non so ancora se è obbligatoria sempre o solo se I50obl = 'S'
106300140721       IF  VSHobl = 'S' and wnews = *on;
106400140716         SELECT;
106500140716           WHEN  vshsnv <> *blanks and vscval = 0;
106600140716             errVAL = *on;
106700140716             vscmsg = Msg(01);
106800140716             EXSR AggioSFL;
106900140716             leavesr;
107000140716           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
107100140716             errVALD = *on;
107200140716             vscmsg = Msg(01);
107300140716             EXSR AggioSFL;
107400140716             leavesr;
107500140716           WHEN  vshsnf <> *blanks and vscpft = 0;
107600140716             errPFT = *on;
107700140716             vscmsg = Msg(01);
107800140716             EXSR AggioSFL;
107900140716             leavesr;
108000140716           WHEN  vshsns <> *blanks and vscsna = 0;
108100140716             errSNA = *on;
108200140716             vscmsg = Msg(01);
108300140716             EXSR AggioSFL;
108400140716             leavesr;
108500140716           WHEN  vshsne <> *blanks and vscfsn = *blanks;
108600140716             errFSN = *on;
108700140716             vscmsg = Msg(01);
108800140716             EXSR AggioSFL;
108900140716             leavesr;
109000140716         ENDSL;
109100140716       ENDIF;
109200140725
109300140725       // Paesei esteri ammessi solo se inserita almeno 1 % di ripartizione estera
109400140725       IF  VSCifo = '50 ' and VSCatb = *blanks and
109500140725           VSCfsn <> *blanks and not wEstero;
109600140725         errFSN = *on;
109700140725         VSCmsg = Msg(37);
109800140725         EXSR AggioSFL;
109900140725         leavesr;
110000140725       ENDIF;
110100081006
110200081006       // Concorrenti logistica ammessi solo se outsourcing=S
110300110707 2     if vscifo='40 '  and vscatb=' ' and (vscfsn<>' ' or vscpft>0 or vscsna>0
110400110707                            or vscval>0) ;
110500081006       if outfsn<>'S'   ;
110600081006         errFSN=*on ;
110700081006         vscmsg = Msg(14);
110800081006         EXSR AggioSFL   ;
110900081006         leavesr;
111000081006       endif             ;
111100081007
111200081006       // Memorizzo presenza info 40  se immesso Si o presente altro
111300081006       //  importo
111400110707         if vshann=' ' and vscfsn<>'N';
111500081007         LOG40='S'         ;
111600081007         endif             ;
111700081007 2     endif             ;
111800081008
111900081126       // Sottotipo col '?' --> Se '?' richiamo pgm di ricerca
112000081126 2     if vscfsn='?' ;
112100081008       clear tntb70ds ;
112200081008       itb70cpo=i50cpo;
112300081023       itb70rag=i50rag ;
112400081126       itb70tpf=vscifo  ;
112500081008       itb70pgm=i50pgm  ;
112600081008       callp TNTB70R (kpjba:tntb70ds)  ;
112700081008       clear vscfsn       ;
112800081126 3     if otb70spf>*zeros    ;
112900081008         vscspp=otb70spf  ;
113000081008         vsdifo=otb70dspf ;
113100081008         vscval=%int(otb70spf) ;
113200081126 3     endif         ;
113300081008       c01csr=s01nrr    ;
113400081008       exsr AggioSFL    ;
113500081008       leavesr       ;
113600081126 2     endif         ;
113700081007
113800081126 2     if vscval=0    and vshifgv='?' ;
113900081007       clear vscspp      ;
114000081009       clear vsdifs      ;
114100081009       vsdifo=CoRicerca  ;
114200081126 2     endif     ;
114300081007
114400081126       // Motivo non affidamento a noi: da richidere solo se corriere BART
114500081126       //  non è al 100
114600081126 2     if  vscifo='MOI'   ;
114700081126
114800081126         if vscval=0   ;
114900081126          vsdifo=CoRicercaMOI  ;
115000081126         endif         ;
115100081126
115200081126 3       if bart_10=100 and vscval>0        ;
115300081126           errVAL=*on ;
115400081126           vscmsg = Msg(16);
115500081126           EXSR AggioSFL   ;
115600081126           leavesr;
115700081126 3       endif             ;
115800081126
115900081126         if vscval>0      ;
116000081126           MOIconta=moiconta+1   ;
116100081126         endif     ;
116200081126
116300081126         moinrr=s01nrr    ;
116400081126
116500081126 2     endif            ;
116600081008
116700081007       // Controllo
116800081126 2     if vscval>0    and vshifgv='?' ;
116900081007       w005a=%editc(vscval:'X')    ;
117000081126       chain ('IFS':vscifo:%subst(w005a:2:4))  tntbe01l    ;
117100081126 3       if not %found(tntbe01l) or tbeatb<>' ' or %subst(w005a:1:1)<>'0';
117200081007          errVAL=*on ;
117300081007          vscmsg = Msg(18);
117400081007          EXSR AggioSFL   ;
117500081007          leavesr;
117600081126 x3       else                        ;
117700081007          vscspp=%subst(w005a:2:4)    ;
117800081007          difs=tbeuni ;
117900081007          vsdifo=§ifsdes              ;
118000081007          vsdifs=§ifsdel              ;
118100081126  3      endif             ;
118200081126
118300081126        //  Memorizzo i codici per vedere se sono doppi
118400081126        w008a=vscifo+%editc(vscval:'X')   ;
118500081126        Indx=%lookup(w008a:IFOval)   ;
118600081126 3      if Indx>0    ;
118700081126         errVAL=*on ;
118800081126 4       if vscifo='MOI'   ;
118900081126           vscmsg = Msg(28);
119000081126         else      ;
119100081126           vscmsg = Msg(30);
119200081126 4       endif     ;
119300081126         EXSR AggioSFL   ;
119400081126         leavesr;
119500081126
119600081126 x3     else   ;
119700081126          ifoval(yy)=w008a  ;
119800081126          yy=yy+1   ;
119900081126          // Se c'e' un MOI=9999 e uno no 9999, errore
120000081126          w008a='MOI09999'  ;
120100081126          Indx=%lookup(w008a:IFOval) ;
120200081126  4       if Indx>0   and moiconta>1 ;
120300081126            errVAL=*on ;
120400081126            vscmsg = Msg(29);
120500081126            EXSR AggioSFL   ;
120600081126            leavesr;
120700081126  4       endif    ;
120800081126  3     endif             ;
120900081126  2    endif             ;
121000081007
121100081126  2    if vscatb=' '  ;
121200081031
121300080925       // Se info SPT - totale spesa trasporti,  la memorizzo
121400080925       //  per il calcolo delle %
121500080925       if vscifo='SPT' ;
121600080925       SPTPFT=vscpft   ;
121700080925       endif           ;
121800140725
121900140725       // Se ripartizione spesa ESTERA lo memorizzo
122000140725       IF  VSHie = 'E' and VSCval > 0;
122100140725         wEstero = *on;
122200140725       ENDIF;
122300140725
122400081023       // Se info LOG - logistica  memorizzo per dare obbligatorio outsourci
122500081023       if vscifo='LOG' ;
122600081023       LOGfsn=vscfsn   ;
122700081023       LOGnrr=s01nrr   ;
122800081006       endif           ;
122900081023       // Se info OUT - outsourcing memorizzo per dare obbligatori
123000081023       //  i concorrenti della logistica
123100081023       if vscifo='OUT' ;
123200081023       OUTfsn=vscfsn   ;
123300081023       OUTnrr=s01nrr   ;
123400081023       endif           ;
123500140718
123600081007       // Se info 10 _BAR - concorrenti utilizzati bartolini
123700081007       //  memorizzo la % che ha
123800081007       if vscifo='10' and vscspp='_BAR' ;
123900081007       bart_10=vscval   ;
124000120105
124100120110         //?Controllo % affidamento a BRT
124200120110         SELECT;
124300120110         //?Controllo se devo azzerare la % di affidato a BRT
124400120110         WHEN  wAzzera10 and bart_10 > 0;
124500120110           errVAL = *on;
124600120125           VSCmsg = Msg(33);
124700120110           EXSR AggioSFL;
124800120110           leavesr;
124900120110         //?Controllo se devo aumentare la % di affidato a BRT
125000120111         WHEN  wAumenta10 and bart_10 < sav_bar_10 and bart_10 < 100;
125100120110           errVAL = *on;
125200120125           VSCmsg = Msg(31);
125300120110           EXSR AggioSFL;
125400120110           leavesr;
125500120510         //?Controllo se devo aumentare la % di affidato a BRT solo se = 0
125600120510         WHEN  wAumenta10f and bart_10 < sav_bar_10 and bart_10 = 0;
125700120510           errVAL = *on;
125800120510           VSCmsg = Msg(31);
125900120510           EXSR AggioSFL;
126000120510           leavesr;
126100120110         //?Controllo se devo diminuire la % di affidato a BRT
126200120110         WHEN  bart_10 > 0 and
126300120110               wDiminuisci10 and bart_10 > sav_bar_10;
126400120110           errVAL = *on;
126500120125           VSCmsg = Msg(34);
126600120110           EXSR AggioSFL;
126700120110           leavesr;
126800120110         ENDSL;
126900120105
127000081007       endif           ;
127100140718
127200100302       // Se info ATR - altro di ripartizione memorizzo la %
127300100226       if vscifo='ATR'                  ;
127400100226       infoATR=vscval   ;
127500100226       endif           ;
127600140721
127700140721       //?Salvo le % di ripartizione spesa totale
127800140721       SELECT;
127900140721         WHEN  VSCifo = 'PRF';
128000140721           savPRFval = VSCval;
128100140721         WHEN  VSCifo = 'PRO';
128200140721           savPROval = VSCval;
128300140721         WHEN  VSCifo = 'AER';
128400140721           savAERval = VSCval;
128500140721         WHEN  VSCifo = 'CAE';
128600140721           savCAEval = VSCval;
128700140725         WHEN  VSCifo = 'ATR';
128800140725           savATRval = VSCval;
128900140721       ENDSL;
129000140718
129100140919       //?Salvo e sommo il NON affidato a BRT
129200140721       SELECT;
129300140721         WHEN  VSCifo = 'NRF';
129400140721           savNRFpft = VSCpft;
129500140721           noBRTpft += VSCpft;
129600140721         WHEN  VSCifo = 'NRO';
129700140721           savNROpft = VSCpft;
129800140721           noBRTpft += VSCpft;
129900140721         WHEN  VSCifo = 'NER';
130000140721           savNERpft = VSCpft;
130100140721           noBRTpft += VSCpft;
130200140721         WHEN  VSCifo = 'NAE';
130300140721           savNAEpft = VSCpft;
130400140721           noBRTpft += VSCpft;
130500140725         WHEN  VSCifo = 'NTR';
130600140725           savNTRpft = VSCpft;
130700140725           noBRTpft += VSCpft;
130800140721       ENDSL;
130900100226
131000081126 2     endif        ;
131100080925
131200080703       EXSR AggioSFL   ;
131300081006 1     endif   ;
131400081125
131500080703
131600080703       s01nrr=1+s01nrr  ;
131700080703       chain s01nrr    mk50s01    ;
131800081006 0     enddo         ;
131900081006
132000081006       // Controllo ultima categoria
132100081006       EXSR CambioCAT     ;
132200081006
132300081215       // A questo punto le info minime sono presenti
132400081215       // solo per controlli obbligatori
132500081215       if i50obl='S' ;
132600081215         o50ifomin='S' ;
132700081215       endif ;
132800081215
132900081126       // Un motivo non affidamento obbligatorio se Bart<100%
133000081126 3     if bart_10<100 and MOIconta =0   and i50obl='S' ;
133100081126         chain moinrr    mk50s01    ;
133200081126         s01nrr=moinrr              ;
133300081126         errVAL=*on ;
133400081126         vscmsg = Msg(27);
133500081126         EXSR AggioSFL   ;
133600081126         leavesr;
133700081126 3     endif             ;
133800081126
133900081023       // Se logistica interessante per noi, obbligatorio outsourcing
134000081023       if LOGfsn='S' and outfsn=' '  and i50obl='S'  ;
134100081023         chain lognrr    mk50s01    ;
134200081023         s01nrr=lognrr              ;
134300081023         errFSN=*on ;
134400081023         vscmsg = Msg(26);
134500081023         EXSR AggioSFL   ;
134600081023         leavesr;
134700081023       endif             ;
134800081006       // Se presente outsourcing, obbligatori concorrenti logistica
134900081006       if outfsn='S' and log40=' '  and i50obl='S'  ;
135000081006         chain outnrr    mk50s01    ;
135100081006         s01nrr=outnrr              ;
135200081006         errFSN=*on ;
135300081006         vscmsg = Msg(15);
135400081006         EXSR AggioSFL   ;
135500081006         leavesr;
135600081006       endif             ;
135700140929
135800140929       //?Non affidato a BRT se affidato a BRT < 100% devo richiederlo obbligatorio
135900140929       //?quando il pgm è richiamo per info obbligatorie
136000140929        IF  I50obl = 'S' and BART_10 < 100 and
136100140929            noBRTpft = 0 and
136200140929            (savPRFval > 0 or savPROval > 0 or savAERval > 0 or
136300140929             savCAEval > 0 or savATRval > 0);
136400140929          chain ifoNRFnrr mk50S01;
136500140929          s01nrr = ifoNRFnrr;
136600140929          errPFT = *on;
136700140929          VSCmsg = Msg(38);
136800140929          EXSR AggioSFL;
136900140929          leavesr;
137000140929        ENDIF;
137100140721
137200140919       //?Se Non affidato a BRT impostato deve esserci la relativa % di ripartizione
137300140721       //?spesa traffico totale
137400140722        IF  savNRFpft > 0 and savPRFval = 0;
137500140721          chain ifoNRFnrr mk50S01;
137600140721          s01nrr = ifoNRFnrr;
137700140721          errPFT = *on;
137800140722          vscmsg = Msg(42);
137900140721          EXSR AggioSFL;
138000140721          leavesr;
138100140722        ENDIF;
138200140722        IF  savNRFpft > (SPTpft * savPRFval / 100);
138300140722          chain ifoNRFnrr mk50S01;
138400140722          s01nrr = ifoNRFnrr;
138500140722          errPFT = *on;
138600140722          vscmsg = Msg(41);
138700140722          EXSR AggioSFL;
138800140722          leavesr;
138900140722        ENDIF;
139000140722        IF  savNROpft > 0 and savPROval = 0;
139100140721          chain ifoNROnrr mk50S01;
139200140721          s01nrr = ifoNROnrr;
139300140721          errPFT = *on;
139400140722          vscmsg = Msg(42);
139500140721          EXSR AggioSFL;
139600140721          leavesr;
139700140722        ENDIF;
139800140722        IF  savNROpft > (SPTpft * savPROval / 100);
139900140722          chain ifoNROnrr mk50S01;
140000140722          s01nrr = ifoNROnrr;
140100140722          errPFT = *on;
140200140722          vscmsg = Msg(41);
140300140722          EXSR AggioSFL;
140400140722          leavesr;
140500140722        ENDIF;
140600140721         IF  savNERpft > 0 and savAERval = 0;
140700140721          chain ifoNERnrr mk50S01;
140800140721          s01nrr = ifoNERnrr;
140900140721          errPFT = *on;
141000140722          vscmsg = Msg(42);
141100140721          EXSR AggioSFL;
141200140721          leavesr;
141300140721         ENDIF;
141400140722         IF  savNERpft > (SPTpft * savAERval / 100);
141500140722          chain ifoNERnrr mk50S01;
141600140722          s01nrr = ifoNERnrr;
141700140722          errPFT = *on;
141800140722          vscmsg = Msg(41);
141900140722          EXSR AggioSFL;
142000140722          leavesr;
142100140722         ENDIF;
142200140721         IF  savNAEpft > 0 and savCAEval = 0;
142300140721          chain ifoNAEnrr mk50S01;
142400140721          s01nrr = ifoNAEnrr;
142500140721          errPFT = *on;
142600140722          vscmsg = Msg(42);
142700140721          EXSR AggioSFL;
142800140721          leavesr;
142900140721         ENDIF;
143000140722         IF  savNAEpft > (SPTpft * savCAEval / 100);
143100140722          chain ifoNAEnrr mk50S01;
143200140722          s01nrr = ifoNAEnrr;
143300140722          errPFT = *on;
143400140722          vscmsg = Msg(41);
143500140722          EXSR AggioSFL;
143600140722          leavesr;
143700140722         ENDIF;
143800140725         IF  savNTRpft > 0 and savATRval = 0;
143900140725          chain ifoNTRnrr mk50S01;
144000140725          s01nrr = ifoNAEnrr;
144100140725          errPFT = *on;
144200140725          vscmsg = Msg(42);
144300140725          EXSR AggioSFL;
144400140725          leavesr;
144500140725         ENDIF;
144600140725         IF  savNTRpft > (SPTpft * savATRval / 100);
144700140725          chain ifoNTRnrr mk50S01;
144800140725          s01nrr = ifoNTRnrr;
144900140725          errPFT = *on;
145000140725          vscmsg = Msg(41);
145100140725          EXSR AggioSFL;
145200140725          leavesr;
145300140725         ENDIF;
145400140718
145500140718       //?Se affidato a BRT è 100%  non devo indicare nessuna spesa per il mancato
145600140718       //?affidamento a BRT
145700140718        IF  bart_10 = 100 and noBRTpft > 0;
145800140718          chain ifoNRFnrr mk50S01;
145900140718          s01nrr = ifoNRFnrr;
146000140718          errPFT = *on;
146100140721          vscmsg = Msg(40);
146200140718          EXSR AggioSFL;
146300140718          leavesr;
146400140718        ENDIF;
146500140721
146600140721       //?Se affidato a BRT non è 100%
146700140919       //?Il non affidato a BRT deve essere congruente
146800140804        IF  bart_10 <> 100 and noBRTpft > 0 and
146900140725            noBRTpft <> (sptpft - siBRTpft);
147000140722          SELECT;
147100140722            WHEN  savPRFval > 0;
147200140722              chain ifoNRFnrr mk50S01;
147300140722              s01nrr = ifoNRFnrr;
147400140722            WHEN  savPROval > 0;
147500140722              chain ifoNROnrr mk50S01;
147600140722              s01nrr = ifoNROnrr;
147700140722            WHEN  savAERval > 0;
147800140722              chain ifoNERnrr mk50S01;
147900140722              s01nrr = ifoNERnrr;
148000140722            WHEN  savCAEval > 0;
148100140722              chain ifoNAEnrr mk50S01;
148200140722              s01nrr = ifoNAEnrr;
148300140725            WHEN  savATRval > 0;
148400140725              chain ifoNTRnrr mk50S01;
148500140725              s01nrr = ifoNTRnrr;
148600140722          ENDSL;
148700140721          errPFT = *on;
148800140721          vscmsg = Msg(40);
148900140721          EXSR AggioSFL;
149000140721          leavesr;
149100140721        ENDIF;
149200081006
149300080409       ENDSR;
149400080409
149500081006       //--------------------------------------------------------------
149600081006       //?cambiata categoria delle info conn- controllo e salvataggi
149700081006       //--------------------------------------------------------------
149800081006       BEGSR  CambioCAT    ;
149900140716
150000081006       if wctrprc='P'  and  i50obl='S' and totprc<100  ;
150100081006       s01nrr=s01nrr-1 ;
150200081006       chain s01nrr    mk50s01    ;
150300081006           errVal=*on;
150400081006           vscmsg = Msg(13);
150500081006           %subst(vscmsg:40:21)=wdescat   ;
150600081006           EXSR AggioSFL   ;
150700081006       endif              ;
150800081006
150900081006       // Pulisco e memorizzo nuovi dati per nuova categoria
151000081006        clear  totprc     ;
151100081215        wctrprc=vshobl  ;
151200081215        wdescat=vsdifo   ;
151300140716
151400081006        ENDSR   ;
151500080925       //--------------------------------------------------------------
151600080925       //?Visualizzo importi in base alla spesa trasporti
151700080925       //--------------------------------------------------------------
151800080925       BEGSR    VISUALimp  ;
151900081003       if vshsnf=' ' and vscpft=0 and vscval>0 and vsctva='%  '  ;
152000081215       PFTblu=*on     ;
152100081003       vscpft=(SPTpft*vscval)/100   ;
152200140717       //if vscpft>0    ;
152300140717       //vscvft='EUR'   ;
152400140717       //endif ;
152500140721       //?Se sto calcolando l'importo della % di affido a BRT me la salvo
152600140721         IF  vscifo = '10' and vscspp = '_BAR';
152700140721           siBRTpft = vscpft;
152800140721         ENDIF;
152900140721
153000080925       endif ;
153100080925
153200080925       ENDSR      ;
153300080703       //--------------------------------------------------------------
153400080703       //?Aggiornamento sfl
153500080703       //--------------------------------------------------------------
153600080703       BEGSR AGGIOSFL  ;
153700080703       if vscmsg<>*blanks   ;
153800080703       errMessage  = *on;
153900080703       errGenerico = *on;
154000080704       c01csr=s01nrr    ;
154100080703       endif    ;
154200080703
154300080703       exsr ProtCampi   ;
154400080703
154500080703       //  Ripristino dei flag di forzatura
154600081003       if vshsnf='X' and vscpft>0   ;
154700081003       vshsnf='S'   ;
154800080703       endif    ;
154900081003       if vshsns='X' and vscsna>0   ;
155000080703       vshsns='S'   ;
155100080703       endif    ;
155200081003       if vshsnv='X' and vscval>0  ;
155300081003       vshsnv='S'   ;
155400080703       endif    ;
155500140715       IF  vshsnvd = 'X' and vscvald <> 0;
155600140715         vshsnvd = 'S';
155700140715       ENDIF;
155800081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
155900081007       vshsne='S'   ;
156000081007       endif    ;
156100080703
156200110707       if vscatb='A' and vshann= 'X' ;
156300080704       vshann='S'  ;
156400080704       endif   ;
156500080703
156600080703       update  mk50s01  ;
156700080703       ENDSR   ;
156800080207       //--------------------------------------------------------------
156900080627       //?Inizializzazione SFL01
157000080207       //--------------------------------------------------------------
157100080409       BEGSR InzS01;
157200080207
157300080207       // Pulizia subfile
157400080207         SflDsp_N    = *on;
157500080207         SflDspCtl_N = *on;
157600080627         write  MK50C01;
157700080207         SflDspCtl_N = *off;
157800080207         SflEnd      = *off;
157900080530
158000080530         clear C01rcd;
158100100507         C01csr=1;
158200080627         S01nrr=0 ;
158300080702         clear Vscmsg;
158400080207         errMessage  = *off;
158500080207         errGenerico = *off;
158600080627         clear savcat  ;
158700080627         clear wcat    ;
158800080627         clear wifo    ;
158900081006         clear wIFS    ;
159000081006         clear sptpft  ;
159100100302         clear savspt  ;
159200100302         clear savatr  ;
159300140716
159400120109         wForzaAgg = *off;
159500180216         wForzaAgg = savForzaAgg;
159600120111         clear sav_bar_10;
159700140716         wForzaPft = *off;
159800140715
159900140715         errVAL=*off     ;
160000140715         errVALD = *off;
160100080409
160200080702       // Carico le info
160300080627       xx=1    ;
160400080627       Indx=1    ;
160500080702       dow   xx<=50    ;
160600080702       if    ifoor(xx)<>*blanks  ;
160700080627       clear  MK50S01;
160800080627
160900080627       Wcat= %subst(ifoor(xx):1:2)   ;
161000080702       Wifo= %subst(ifoor(xx):6:3)   ;
161100080627
161200080627       // cambio di categoria
161300080627        if savcat=*blanks or savcat<>wcat               ;
161400080627        savcat=wcat   ;
161500080627        Ifosott=*on   ;
161600080627
161700080627       // Verifico se devo visualizzare la titolo della categoria
161800080627         clear  DIFG  ;
161900080627         reset TIBS02ds;
162000080627         T02sif = knsif;
162100080627         T02cod = 'IFG';
162200080627         T02mod = 'C'  ;
162300080627         T02ke1 = wcat;
162400080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
162500080627         if t02err=*blanks ;
162600080627            difg=t02uni    ;
162700080627         endif            ;
162800080627
162900080627       // Se prevista la visualizzazione della categoria
163000080627       //  emetto la riga in videata,
163100080627       if §ifgvis='S'    ;
163200080703         vsdifo=§ifgdes         ;
163300080703         vsdifo=%trim(vsdifo)+':'    ;
163400080703         // In VSHIMM   metto una "C" per indicare che si tratta
163500080703         //    di riga di categoria
163600080703         vshimm='C'        ;
163700081007
163800081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
163900081003         vshobl=§ifgctpr   ;
164000081007
164100081007         EXSr ProtCampi    ;
164200081003
164300080627         s01nrr=s01nrr+1   ;
164400080627         write mk50s01     ;
164500080627         protriga=*off     ;
164600080627         Ifosott=*off      ;
164700081007         clear vscfsn      ;
164800100507         // Se la prima riga è un commento, posizionamento cursore sulla seconda
164900100507         if s01nrr=1   ;
165000100507          c01csr= 2 ;
165100100507         endif   ;
165200100507
165300081007       endif               ;
165400081007       endif               ;
165500080627
165600080627       Indx=%lookup(wifo:ifo)   ;
165700080627       if Indx>0                ;
165800080627         difo=ifods(Indx)         ;
165900080627         vscifo=ifo(Indx)       ;
166000080702
166100080702         // Dati da tabella
166200080702          vshobl=§ifoobl  ;
166300081015          vshie =§ifotip  ;
166400080704
166500080702          vshann=§ifoann  ;
166600080704
166700081003         // VALORE
166800081003          clear vshsnv  ;
166900081003          clear vsctva  ;
167000081003          if §ifosnv<>' '  ;
167100081003          vshsnv=§ifosnv  ;
167200081003          // tipo valore
167300081003          vsctva=§ifotva ;
167400080703          endif   ;
167500140715
167600140715         //?VALORE con DECIMALE
167700140715          IF  §ifosnvd <> *blanks;
167800140715            vshsnvd = §ifosnvd;
167900140715          // tipo valore
168000140715            vsctva = §ifotva;
168100140715          ENDIF;
168200080704
168300081003         // SPEDIZIONI
168400080702          vshsns=§ifosns  ;
168500080704
168600081003         // FATTURATO
168700080702          vshsnf=§ifosnf  ;
168800081003
168900081003         // SI/NO
169000081003          vshsnE=§ifosnE  ;
169100081003
169200080702          vshifgv=§ifgvis  ;
169300081007          clear vscint    ;
169400080627
169500080627         // Record senza sottotipo
169600081006         select    ;
169700081006         when §ifoifs=' '    ;
169800080702            vsdifo=§ifodes         ;
169900080703            if Ifosott=*on    ;
170000080703               vsdifo=%trim(vsdifo)+':'    ;
170100080703            endif    ;
170200080702            chain    (i50cpo:wifo) ticpi01l  ;
170300081126            if %found(ticpi01l)   ;
170400081126             trovacpi='S'    ;
170500081126             else ;
170600081126             trovacpi='N'    ;
170700081126             endif  ;
170800080702            EXSR CaricaInfo        ;
170900081006
171000081006         // Record con   sottotipo da caricare tutti
171100081006         when   §ifoifs='S' and §ifoifv='S';
171200080627            EXSR CARCon    ;
171300081006
171400081006         // Record con   sottotipo da caricare col '?'
171500081006         //   la categoria visualizzata in questo caso è obbligatoria
171600081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
171700081006         //  il '?'
171800081006         when   §ifoifs='S' and §ifoifv='?';
171900081006          vshifgv=§ifoifv  ;
172000081215          yy=1        ;
172100081126
172200081126          if §ifonrec=0 ;
172300081126             §ifonrec=1  ;
172400081126          endif     ;
172500081126
172600081126          setll (i50cpo:wifo) ticpi01l  ;
172700081126          dow yy<=§ifonrec  ;
172800081006            EXSR CarconUNA            ;
172900081126            yy=yy+1  ;
173000081126          enddo    ;
173100081126
173200081006         endsl ;
173300080703
173400080703         endif ;
173500080702         endif ;
173600080702
173700080702         xx=xx+1   ;
173800080702         enddo     ;
173900080702         ENDSR    ;
174000080627
174100080702       //--------------------------------------------------------------
174200081006       //?Carico con   sottotipo  carica riga per riga
174300080702       //--------------------------------------------------------------
174400080702       BEGSR  CARcon            ;
174500080702
174600081126         tbeke1=wifo   ;
174700080925
174800081006         setll ('IFS':tbeke1) tntbe01l   ;
174900081006         READE ('IFS':tbeke1) tntbe01l   ;
175000080702
175100080702         dow not %eof(tntbe01l)   ;
175200080702         if tbeatb=' '    ;
175300081006         difs=tbeuni      ;
175400081006         wifs=tbeke2      ;
175500080702         vscspp=tbeke2      ;
175600081127         vshspp=tbeke2      ;
175700081007         vsdifo=§ifsdes  ;
175800110707         vshann=§ifsann  ;
175900080702
176000081006         chain    (i50cpo:wifo:wifs) ticpi01l  ;
176100081126            if %found(ticpi01l)   ;
176200081126             trovacpi='S'    ;
176300081126             else ;
176400081126             trovacpi='N'    ;
176500081126             endif  ;
176600080702         EXSR CaricaInfo    ;
176700080925         endif   ;
176800080702
176900081006         READE ('IFS':tbeke1) tntbe01l   ;
177000080702         enddo   ;
177100080702
177200080702       ENDSR;
177300081006       //--------------------------------------------------------------
177400081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
177500081006       //--------------------------------------------------------------
177600081006       BEGSR  CARconUNA         ;
177700081007
177800081007       vscint='?'    ;
177900081007
178000081126       reade    (i50cpo:wifo) ticpi01l  ;
178100081006
178200081126 1     if not %eof(ticpi01l)   ;
178300081126         trovacpi='S'    ;
178400081126
178500081006         vscspp=cpispf   ;
178600081127         vshspp=cpispf   ;
178700081006
178800081126         tbeke1=wifo    ;
178900081126
179000081006         tbeke2=cpispf  ;
179100081006         chain ('IFS':tbeke1:tbeke2) tntbe01l   ;
179200081006 2       if not %found(tntbe01l)    ;
179300081006         clear tbeuni       ;
179400081006 2       endif    ;
179500081007         difs=tbeuni     ;
179600081006
179700081007         vsdifo=§ifsdes   ;
179800081007         vsdifs=§ifsdel   ;
179900081007
180000081006         EXSR CaricaINFO  ;
180100081006
180200081006 1x      else    ;
180300081126         trovacpi='N'    ;
180400081006
180500081126         // se non trovato dicitura generica da tabella IFO
180600081126         if wifo='MOI'  ;
180700081126           vsdifo=coRicercaMOI    ;
180800081126           else  ;
180900081126           vsdifo=coRicerca       ;
181000081126         endif  ;
181100081007         clear vsdifs      ;
181200081126         clear vscspp      ;
181300081127         clear vshspp      ;
181400081006
181500081006         EXSR CaricaINFO   ;
181600081006         endif             ;
181700081006
181800081006       ENDSR;
181900080702       //--------------------------------------------------------------
182000081006       //?Carica ogni riga di info commerciale
182100080702       //--------------------------------------------------------------
182200080702         BEGSR CaricaINFO             ;
182300080925         clear pftblu   ;
182400080925
182500081126         if trovacpi='S'                             ;
182600080703          vshimm=' '        ;
182700080702          vscfsn=cpifsn   ;
182800081003          vscval=cpival   ;
182900081007          if vshifgv='?'   and vscspp>*zeros    ;
183000081007            w0040=%int(vscspp)  ;
183100081007            vscval=w0040     ;
183200081007            clear vscfsn     ;
183300081007          endif            ;
183400080702          vscsna=cpisna   ;
183500080702          vscpft=cpipft   ;
183600080925          if vscpft>0     ;
183700080925          vshprpft='S'    ;
183800080925          endif           ;
183900140717          //vscvft=cpivft   ;
184000080702          Dataiso=%date(cpidim:*iso)  ;
184100080702          datadmy=Dataiso ;
184200080702          vscdim=%dec(datadmy)  ;
184300081003          clear vscfil  ;
184400081003          if cpifil>0   ;
184500081003          vscfil=%editc(cpifil:'X')   ;
184600081003          endif         ;
184700081003
184800081003          if cpipru<>*blanks ;
184900080702          vscpru=cpipru   ;
185000081003          vscdpru='Utente' ;
185100081003          else  ;
185200081003          clear vscpru   ;
185300081003          clear vscdpru   ;
185400081003          endif     ;
185500080925
185600080925          exsr ViSUALimp   ;
185700140715
185800140715         //?Calcolo il ricavo medio spedizione 'RMS'
185900140715           IF  VSCifo = 'SPT' and VSCpft <> 0 and VSCsna <> 0;
186000140715             wVALrms = VSCpft / VSCsna;
186100140929             IF  wVALrms > 999;
186200140929               wVALrms = 999;
186300140715             ENDIF;
186400140715           ENDIF;
186500080925
186600080702         else             ;
186700080703          vshimm='S'      ;
186800080703          Protann=*on     ;
186900080702          clear vscfsn    ;
187000081003          clear vscval    ;
187100080702          clear vscsna    ;
187200080702          clear vscpft    ;
187300140717          //clear vscvft    ;
187400080702          clear vscdim    ;
187500080702          clear vscfil    ;
187600080702          clear vscpru    ;
187700081003          clear vscdpru    ;
187800080925          clear vshprpft  ;
187900140715
188000140715         //?Se info 'RMS' imposto il dato
188100140715           IF  VSCifo = 'RMS';
188200140715             VSCvald = wVALrms;
188300140715           ENDIF;
188400140715
188500080702         endif            ;
188600081006
188700080925         exsr ProtCAMPI    ;
188800080703
188900080703         // I record da annullare li visualizzo solo se ci sono
189000110707           if vshann=' ' or (vshann<>' '  and vscfsn<>' ') or
189100110707                             (vshann<>' '  and vscpft>0)   or
189200110707                             (vshann<>' '  and vscval>0   )  ;
189300081003
189400081003         // In interrogazione visulizzo solo i record pieni col sottotipo
189500081003           if i50mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
189600080703            s01nrr=s01nrr+1   ;
189700080703            write mk50s01     ;
189800140718
189900140919         // Mi salvo i nr di rcd del non affidato a BRT
190000140721            IF  VSCifo = 'NRF' and ifoNRFnrr = 0;
190100140718              ifoNRFnrr = S01nrr;
190200140721            ENDIF;
190300140721            IF  VSCifo = 'NRO' and ifoNROnrr = 0;
190400140721              ifoNROnrr = S01nrr;
190500140721            ENDIF;
190600140721            IF  VSCifo = 'NER' and ifoNERnrr = 0;
190700140721              ifoNERnrr = S01nrr;
190800140721            ENDIF;
190900140721            IF  VSCifo = 'NAE' and ifoNAEnrr = 0;
191000140721              ifoNAEnrr = S01nrr;
191100140721            ENDIF;
191200140725            IF  VSCifo = 'NTR' and ifoNTRnrr = 0;
191300140725              ifoNTRnrr = S01nrr;
191400140725            ENDIF;
191500080925
191600080925         // Se info SPT - totale spesa trasporti,  la memorizzo
191700100302         //  per il calcolo delle %  e per vedere se modificata
191800080925         if vscifo='SPT' ;
191900080925          SPTPFT=vscpft   ;
192000100302          savSPT=vscpft   ;
192100080925         endif           ;
192200100302         //  per vedere se modificata
192300100302         if vscifo='ATR' ;
192400100302          savATR=vscval   ;
192500100302         endif           ;
192600120105
192700120105         //?Memorizzo la % di affidamento a BRT
192800120110           IF  VSCifo = '10' and VSCspp = '_BAR';
192900120105           //?Se devo aumentarla calcolo almeno 1% in +
193000120110             IF  wAumenta10 and VSCval > 0;
193100120111               sav_bar_10 = VSCval + 1;
193200120105             ENDIF;
193300120510             IF  (wAumenta10 or wAumenta10f) and VSCval = 0;
193400120110               sav_bar_10 = 1;
193500120110             ENDIF;
193600120105           //?Se devo dominuirla calcolo almeno 1% in -
193700120110             IF  wDiminuisci10 and VSCval > 0;
193800120111               sav_bar_10 = VSCval - 1;
193900120105             ENDIF;
194000120105           ENDIF;
194100080925
194200080703           endif ;
194300081003           endif ;
194400081003
194500080702       ENDSR;
194600080702       //--------------------------------------------------------------
194700080702       //?Gestione protezione campi del sfl
194800080702       //--------------------------------------------------------------
194900080702          BEGSR   ProtCampi   ;
195000080703           protfat=*off    ;
195100081003           protVal =*off   ;
195200140715           protVald = *off;
195300140715           protVala = *off;
195400080703           protsped=*off   ;
195500080703           protfsn =*off   ;
195600080703           Ifosott=*off    ;
195700080703           Protann=*off ;
195800080704           Infoyell=*off ;
195900080703
196000081007           // Se riga di categoria accendo solo protezione e sottolineatura
196100081007          if vshimm='C'    ;
196200081007           Ifosott=*on     ;
196300081007           Protriga=*on    ;
196400081007          else             ;
196500081007
196600080702          // in base a cosa prevede la tabella, proteggo i campi
196700080702          if vshifgv='N' ;
196800080702            Ifosott=*on       ;
196900080702          endif    ;
197000080702
197100080704          if vshann<>' '  ;
197200080702           protfat=*on     ;
197300081003           protVal =*on    ;
197400140715           protVald = *on;
197500140715           protVala = *on;
197600080702           protsped=*on    ;
197700080702           protfsn =*on    ;
197800080704           infoyell=*on    ;
197900080702          else            ;
198000081006          protann=*on     ;
198100081031
198200081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
198300081103          if vscpft>0 and vshsnf=' ' and vscval=0;
198400081031          protann=*off    ;
198500081031          endif    ;
198600081031          if vscsna>0 and vshsns=' ' ;
198700081031          protann=*off    ;
198800081031          endif    ;
198900081127          if vscval>0 and vshsnv=' ' and vshifgv<>'?'       ;
199000081031          protann=*off    ;
199100081031          endif    ;
199200140715          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
199300140715            protann = *off;
199400140715          ENDIF;
199500081031          if vscfsn<>' ' and vshsne=' ' ;
199600081031          protann=*off    ;
199700081031          endif    ;
199800081006
199900080703            if vshsnf=' ' ;
200000080702             protfat=*on     ;
200100080702            endif           ;
200200081003            if vshsnv=' ' ;
200300081003             protVal =*on     ;
200400080702            endif           ;
200500140715            IF  vshsnvd = *blanks;
200600140715              protVald = *on;
200700140715            ENDIF;
200800140715            if vshsnvd='A';
200900140715             protVala=*on     ;
201000140715            endif           ;
201100080703            if vshsns=' ' ;
201200080702             protsped=*on     ;
201300080702            endif           ;
201400081003            if vshsne=' ' ;
201500081003             protfsn =*on     ;
201600081003            endif           ;
201700080702          endif           ;
201800080703          // Se record di nuova immissione
201900081003          //  proteggo annullamento
202000080703            if vshimm='S'  ;
202100080703             Protann=*on  ;
202200080703           endif   ;
202300081006
202400081006          // Se prevista scelta sottotipo col'?' visualizzo valore
202500081006          //   col codice e vicino il '?'
202600081006          if vshifgv='?'    ;
202700081006             protVal =*off    ;
202800081007             if vscspp>*zeros    ;
202900081007             else          ;
203000081007             infoyell=*on    ;
203100081007             endif            ;
203200081006          endif               ;
203300081007          endif               ;
203400080703
203500080702          ENDSR    ;
203600180215
203700180215       //--------------------------------------------------------------
203800180215       //F16 Annullamento riga info.
203900180215       //--------------------------------------------------------------
204000180215        BEGSR F16S01;
204100180215
204200180216        // devo avere un numero relativo di record del subfile
204300180216        // se a zero vuol dire che F16 è stato dato fuori dal subfile
204400180216          IF  C01csr = *zeros;
204500180216            ErrGenerico = *on;
204600180216            ErrMessage  = *on;
204700180216            VSCmsg      = 'Posizionarsi con il cursore sulla riga da annullare';
204800180216            leavesr;
204900180216          ENDIF;
205000180216
205100180216        // se ho un numero relativo di record del subfile
205200180216        // deve essere un record annullabile
205300180216          IF  C01csr > *zeros;
205400180216            S01nrr = C01csr;
205500180216            chain s01nrr MK50S01;
205600180216            IF  %found and VSHann = *blanks;
205700180216              ErrGenerico = *on;
205800180216              ErrMessage  = *on;
205900180216              VSCmsg = 'Info commerciale non annullabile';
206000180216              leavesr;
206100180216            ENDIF;
206200180216          ENDIF;
206300180216
206400180216        // se arrivo qua posso annullare il rcd
206500180216         chain (I50cpo:VSCifo:VSCspp) TICPI01L;
206600180216         IF  %found(TICPI01L);
206700180216           delete TICPI000;
206800180216         ENDIF;
206900180216
207000180216       // devo aggiornare anche la data ultima variazione INFO
207100180216         chain I50cpo TNCPO11L;
207200180216         IF  %found(TNCPO11L);
207300180216           CPO1ducifo = *date;
207400180216           update TNCPO100;
207500180216         ENDIF;
207600180216
207700180216
207800180216       // dopo aver cancellato il rcd ricarico il subfile
207900180216         $inzs01 = *on;
208000180216         savForzaAgg = wForzaAgg;
208100180216
208200180215        ENDSR;
208300080702       //--------------------------------------------------------------
208400080702       //?conferma aggiornamento
208500080702       //--------------------------------------------------------------
208600080702        BEGSR ConfAGGIO              ;
208700090520        clear ultdata   ;
208800081003
208900080704       s01nrr=1  ;
209000080704       chain s01nrr    mk50s01    ;
209100080704
209200080704 1     dow %found    ;
209300080704       // Escludo i record di descrizione categoria
209400140715 2     //if vshimm<>'C'    ;
209500140715 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
209600080925
209700080925       // se valori calcolati da pgm li pulisco prima dei controlli
209800080925       if vshsnf=' ' and vshprpft=' '   ;
209900080925       clear vscpft   ;
210000140717       //clear vscvft   ;
210100080925       endif          ;
210200080925
210300081127       IF VShifgv<>'?'    ;
210400081127         chain    (i50cpo:vscifo:vscspp) ticpi01l  ;
210500081127       else   ;
210600081127         chain    (i50cpo:vscifo:vshspp) ticpi01l  ;
210700081127       endif   ;
210800081127             if %found(ticpi01l)   ;
210900081127             trovacpi='S'    ;
211000081127             else ;
211100081127             trovacpi='N'    ;
211200081127             endif  ;
211300081127
211400081127       // A n n u l l a m e n t o oppure per qualche strano motivo
211500081127       //   la info c'e'  ma a video non è stata inserita
211600080925
211700081127 3     if trovacpi='S'      and (vscatb='A'  or (vscfsn=' ' AND
211800081003                                 vscpft=0 and vscsna=0 and vscval=0));
211900081003         delete ticpi000  ;
212000081003 x3    else    ;
212100080704
212200081127       // V a r i a z i o n e :  verifico se modificati dei dati
212300081127 31    if trovacpi='S'     and vscatb=' '   ;
212400080704 4     if (cpifsn<>vscfsn) or
212500081003          (cpival<>vscval) or
212600080704          (cpisna<>vscsna) or
212700081007          (cpispf<>vscspp) or
212800140717          (cpipft<>vscpft) ;
212900140717          //(cpivft<>vscvft) ;
213000080704          // reimposto data ora profilo immissione
213100080704          cpidim=*date    ;
213200090520          ultdata=*date    ;
213300080704          cpifil=utefil   ;
213400080704          cpipru=knmus    ;
213500080704
213600081007          cpifsn= vscfsn  ;
213700081007          IF VShifgv<>'?';
213800081003          cpival= vscval  ;
213900081003          cpitva= vsctva  ;
214000081007          else            ;
214100081007          cpispf=vscspp   ;
214200081007          if cpispf<>*blanks ;
214300081007          cpifsn='S'      ;
214400081007          ENDIF           ;
214500081007          ENDIF           ;
214600080704          cpisna= vscsna  ;
214700080704          cpipft= vscpft  ;
214800140717          cpivft= 'EUR'   ;
214900081006          // verifico se dei dati non ci voglio più: una volta aggiunti
215000080704          //  quelli previsti, lo tolgo
215100080704
215200081003 5        if (cpival>0 and vshsnv=' ')    ;
215300081006          if (cpisna>0 and vshsns<>' ')  or (cpipft>0  and vshsnf<>' ') or
215400081006             (cpifsn<>' ' and vshsne<>' ')   ;
215500081003          clear cpival   ;
215600080704          endif          ;
215700080704 5        endif          ;
215800080704 5        if (cpisna>0 and vshsns=' ')    ;
215900081006          if (cpival>0 and vshsnv<>' ')  or (cpipft>0  and vshsnf<>' ') or
216000081006             (cpifsn<>' ' and vshsne<>' ')   ;
216100080704          clear cpisna   ;
216200080704          endif          ;
216300080704 5        endif          ;
216400080704 5        if (cpipft>0 and vshsnf=' ')    ;
216500081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
216600081006             (cpifsn<>' ' and vshsne<>' ')   ;
216700080704          clear cpipft   ;
216800080704          clear cpivft   ;
216900080704          endif          ;
217000080704 5        endif          ;
217100081006 5        if (cpifsn<>' ' and vshsne=' ')    ;
217200081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
217300081006             (cpipft>0  and vshsnf<>' ')     ;
217400081006          clear cpifsn   ;
217500081006          endif          ;
217600081006 5        endif          ;
217700080704
217800080704        update ticpi000  ;
217900080704 4      endif  ;
218000081003 3a     endif  ;
218100080704
218200081127       // I m m i s s i o n e
218300081127 3     if trovacpi='N'     and vscatb=' ' and (vscfsn<>' ' or
218400081003       vscpft>0 or vscsna>0 or vscval>0) ;
218500080704       clear ticpi000;
218600080704        cpicpo=i50cpo   ;
218700080704        cpitpf=vscifo  ;
218800080704        cpispf=vscspp  ;
218900080704        cpifsn=vscfsn ;
219000081007        if vshifgv<>'?';
219100081003        cpival= vscval  ;
219200081003        cpitva= vsctva  ;
219300081007        else            ;
219400081007          if cpispf<>*blanks ;
219500081007          cpifsn='S'      ;
219600081007          endif           ;
219700081007        endif           ;
219800081007
219900080704        cpisna= vscsna  ;
220000080704        cpipft= vscpft  ;
220100140717        cpivft= 'EUR'   ;
220200080704        cpidim=*date    ;
220300090520        ultdata=*date    ;
220400080704        cpifil=utefil   ;
220500080704        cpipru=knmus    ;
220600080704
220700080704        write ticpi000 ;
220800081003  3a    endif           ;
220900081003  3     endif           ;
221000080704  2     endif           ;
221100080704
221200080704        s01nrr=s01nrr+1 ;
221300080704        chain s01nrr    mk50s01    ;
221400080704  1     enddo           ;
221500140929
221600140929       //?Se affidato a BRT è 0
221700140929       //?Se ripartizione della spesa è 100%
221800140929       //?Se non affidato a BRT non è presente
221900140929       //?creo in automatico il NON affidato a BRT
222000140929         IF  BART_10 = 0 and
222100140929             (savPRFval + savPROval + savAERval +
222200140929              savCAEval + savATRval) = 100 and
222300140929             noBRTpft = 0;
222400140929           exsr NoAffidatoBRT;
222500140929         ENDIF;
222600080704
222700081015        clear waggiocpo    ;
222800081015
222900081003       // Aggancio tncpo per aggiornare se ho tutte le info
223000081003       chain   i50cpo   tncpo01l   ;
223100081015  1      if %found(tncpo01l)    ;
223200081015         chain  cp1nrr    tncpo00f   ;
223300081015  1      endif                  ;
223400081003  1      if %found(tncpo00f)   ;
223500081015
223600081015         // Se inserimento obbligatorio--> posso impostare la "S"
223700081015         //  altrimenti controllo da CPI
223800081003
223900081015  2       if i50obl=' '   ;
224000081015            // Rifaccio il controllo del sfl come se fosse
224100081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
224200081215            savmod=i50mod   ;
224300081215            i50mod='C'      ;
224400081215            i50obl='S' ;
224500081215
224600081015            exsr contrs01   ;
224700081215
224800081215            i50obl=' ' ;
224900081215            i50mod=savmod   ;
225000081015
225100081015  3         if ErrGenerico=*off;
225200081215              Waggiocpo='S';
225300081015  3         endif  ;
225400081015
225500081015  x2      else;
225600081015          Waggiocpo='S'   ;
225700081015  2       endif           ;
225800081015
225900081015              dcpo01=cporst    ;
226000081015              §cpoifotot=Waggiocpo   ;
226100081015              o50ifotot=Waggiocpo   ;
226200090520              if ultdata > 0   ;
226300090520                §cpoifodul=%editc(ultdata:'X')  ;
226400090520              endif   ;
226500090520              // Se inserita memorizzo la spesa trasporti reale
226600090520              if sptpft>0   ;
226700090520              §cposptp=sptpft   ;
226800090520              endif   ;
226900100302
227000100302              //  Se variata spesa trasporti o % ATR, ricalcolo cod importanza potenziale
227100100302             if savSPT<> SPTPFT or savatr<>infoATR   ;
227200100302             clear trmk52ds  ;
227300100302             i52cpo=cpocpo  ;
227400100302             i52spt= SPTpfT ;
227500100302             i52ATR=infoATR  ;
227600160802           //?Se la spesa trasporti è a 0 devo passare il trasporto presunto
227700160802           //?in questo modo viene calcolata la categoria del cliente sulla spesa
227800160802           //?presunta e non viene impostata a blank come quando la spesa è a 0
227900160802             IF  SPTpft = 0;
228000160802               I52spt = §CPOsptp;
228100160802               I52tspt = 'P';
228200160802             ENDIF;
228300100302
228400100302             callp TRMK52R  (trmk52ds)  ;
228500100302             cpoftr=o52cimp  ;
228600100302             endif   ;
228700100302
228800081015              cporst=dcpo01    ;
228900081015              update tncpo000  ;
229000081003  1    endif   ;
229100160708
229200160708       //?Scrivo la variazione
229300160708       exsr ScriviVar;
229400140919
229500140725       // devo aggiornare la data ultima conferma INFO COMM.
229600140909         IF  not InfoInt;
229700140725           chain i50cpo TNCPO11L;
229800140725  1        IF  %found(TNCPO11L);
229900140909             IF  ultdata > 0;
230000140909               CPO1ducifo = ultdata;
230100140909             ELSE;
230200140909               CPO1ducifo = *date;
230300140909             ENDIF;
230400140725             update TNCPO100;
230500140725           ENDIF;
230600140909           IF  not %found(TNCPO11L);
230700140909             clear TNCPO100;
230800140909             CPO1cpo = CPOcpo;
230900140909             IF  ultdata > 0;
231000140909               CPO1ducifo = ultdata;
231100140909             ELSE;
231200140909               CPO1ducifo = *date;
231300140909             ENDIF;
231400140909             write TNCPO100;
231500140909           ENDIF;
231600140725         ENDIF;
231700140725
231800080702        ENDSR;
231900140919
232000140919       //--------------------------------------------------------------
232100140919       //?Creo le info di NON Affidato a BRT.
232200140919       //--------------------------------------------------------------
232300140919        BEGSR NoAffidatoBRT;
232400140919
232500140919          clear TICPI000;
232600140919          CPIcpo = I50CPO;
232700140919          CPIvft = 'EUR';
232800140919          CPIdim = *date;
232900140919          CPIfil = utefil;
233000140919          CPIpru = knmus;
233100140919
233200140919          IF  savPRFval > 0 and savNRFpft = 0;
233300140919            CPItpf = 'NRF';
233400140919            CPIpft = (SPTpft * savPRFval / 100);
233500140919            write TICPI000;
233600140919          ENDIF;
233700140919
233800140919          IF  savPROval > 0 and savNROpft = 0;
233900140919            CPItpf = 'NRO';
234000140919            CPIpft = (SPTpft * savPROval / 100);
234100140919            write TICPI000;
234200140919          ENDIF;
234300140919
234400140919          IF  savAERval > 0 and savNERpft = 0;
234500140919            CPItpf = 'NER';
234600140919            CPIpft = (SPTpft * savAERval / 100);
234700140919            write TICPI000;
234800140919          ENDIF;
234900140919
235000140919          IF  savCAEval > 0 and savNAEpft = 0;
235100140919            CPItpf = 'NAE';
235200140919            CPIpft = (SPTpft * savCAEval / 100);
235300140919            write TICPI000;
235400140919          ENDIF;
235500140919
235600140919          IF  savATRval > 0 and savNTRpft = 0;
235700140919            CPItpf = 'NTR';
235800140919            CPIpft = (SPTpft * savATRval / 100);
235900140919            write TICPI000;
236000140919          ENDIF;
236100140919
236200140919        ENDSR;
236300160708
236400160708       //--------------------------------------------------------------
236500160708       //?Scrivo l'immagine Precedente.
236600160708       //--------------------------------------------------------------
236700160708       BEGSR ImmaginePrima;
236800160711
236900160711       //?Aggancio il TNCPO per memorizzare immagine precedente
237000160711         chain(n) (I50cpo) TNCPO01L;
237100160831         dCPO01 = CPOrst;
237200160708       //?Aggancio il rcd SPT per memorizzare immagine precedente
237300160708         chain(n) (I50cpo:'SPT') TICPI01L;
237400160711
237500160708         clear TRMK30DS;
237600160708         IMK30immag = 'P';
237700160711         IMK30tvp = 'D';
237800160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
237900160708
238000160708       ENDSR;
238100160708
238200160708       //--------------------------------------------------------------
238300160708       //?Scrivo Storico Variazioni.
238400160708       //--------------------------------------------------------------
238500160708       BEGSR ScriviVar;
238600160708
238700160831       //?Aggancio il TNCPO per memorizzare immagine variata
238800160831         chain(n) (I50cpo) TNCPO01L;
238900160831         dCPO01 = CPOrst;
239000160708       //?Aggancio il rcd SPT per memorizzare immagine variata
239100160708         chain(n) (I50cpo:'SPT') TICPI01L;
239200160831         IF  not %found(TICPI01L);
239300160831           clear TICPI_30;
239400160831         ENDIF;
239500160708         clear TRMK30DS;
239600160708         IMK30pru = knmus;
239700160708         IMK30noj = knmeb;
239800160901         IF  I50pgm = *blanks;
239900160901           IMK30pgm = 'TRMK50R';
240000160901         ELSE;
240100160901           IMK30pgm = I50pgm;
240200160901         ENDIF;
240300160708         IMK30immag = 'D';
240400160708         IMK30cta = 'M';
240500160711         IMK30tvp = 'D';
240600160708
240700160831         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
240800160708
240900160708       ENDSR;
241000080702
241100080206       //--------------------------------------------------------------
241200080206       //?Operazioni finali.
241300080206       //--------------------------------------------------------------
241400080206       BEGSR RoutEnd;
241500080627
241600080704         if i50tla<>' '    ;
241700080704         // Chiusura pgm   ;
241800080704         clear tibs02ds  ;
241900080704         t02tla='C'      ;
242000080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
242100080704
242200080206         *inLR = *on;
242300080704         endif             ;
242400080704
242500080206         return;
242600080206
242700080206       ENDSR;
242800080206
242900080206      /end-free
243000080206
243100080206       //--------------------------------------------------------------
243200080206       //?Schiere a tempo di compilazione.
243300080206       //--------------------------------------------------------------
243400080206
243500080410** - MSG ------------------------------------------------------------------ -*
243600081003Info Commerciale obbligatoria                                                   01
243700080703Per Info Comm. non presente (N), non indicare gli altri dati                   02
243800081003La Info Comm. prevede l'inserimento del Fatturato presunto OBBLIGATORIO        03
243900081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx: enter per forzare     04
244000081003La  Info Comm. prevede l'inserimento del Fatturato presunto: enter per forzare    05
244100080704La  Info Comm. prevede l'inserimento del numero Spedizioni: enter per forzare  06
244200080704Info Comm. non più in essere:verificarla e se non serve annullarla.ENTER forza 07
244300080704Info Comm. non più in essere: ANNULLARLA !!                                    08
244400081003La somma delle percentuali per questa categoria di info commerc. supera 100%   09
244500081003La Info Comm. prevede l'inserimento del numero Spedizioni OBBLIGATORIO         10
244600081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx OBBLIGATORIO            11
244700081003La  percentuale di questa info commerciale supera 100%                         12
244800081003La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx  deve essere 100% 13
244900081007Inserire concorreti Logistica SOLO SE il cliente prevede Logist.in outsourcing 14
245000081007Se Logistica in outsourcing obbligatorio inserire i concorrenti logistica      15
245100110511"Motivo non affidamento a noi" da immettere solo se "Affidato a BRT" < 100%    16
245200140716ATTENZIONE!! Il valore massimo consentito è di 999 milioni                     17
245300081007"MOTIVO NON AFFIDAMENTO A NOI" errato                                          18
245400081007Indicare OBBLIGATORIAMENTE la presenza (S) o assenza(N) della Info commerciale 19
245500081007Immettere solo  '?'  se si vuole attivare la ricerca                           20
245600081125Immettere "S o N" per indicare la presenza o l'assenza della Info Comm.        21
245700081007Immettere"S o N"per indicare la presenza o l'assenza della Info Comm:ENTR forza22
245800140717ATTENZIONE!! Il valore inserito e > di 1 milione - Enter per forzare.          23
245900140716Il numero delle spedizioni non può essere superiore alla spesa trasporti.      24
246000140716Non ammesso un valore negativo                                                 25
246100081125Se "Logistica interessante per noi", indicare se in Outsourcing                26
246200110511Se non tutto "Affidato BRT" obbligatorio un Motivo NON AFFIDAMENTO a noi       27
246300081126I"Motivi non Affidamento a noi" devono essere diversi: uno per ITA uno per EST 28
246400081126Se immesso"9999-NON Rilevabile", l'altro motivo deve essere vuoto              29
246500081126Codice già presente !!                                                         30
246600120125Obbligatorio aumentare la % di affidamento a BRT!!                             31
246700120109Alcune Info sono più vecchie di 1 anno. Verificare se ancora corrette.         32
246800120125Obbligatorio azzerare la % di affidamento a BRT!!                              33
246900120125Obbligatorio diminiuire la % di affidamento a BRT!!                            34
247000140715Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                 35
247100140715Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!                36
247200140725Inserire Paesi Esteri SOLO SE il cliente prevede trasporto Estero.             37
247300140922Se non tutto "Affidato BRT" obbligatorio ripartizione spesa NON affidato a BRT 38
247400140717ATTENZIONE!! Il valore inserito e > di 10.000.                                 39
247500140919Non affidato a BRT non congruente con % utilizzo concorrenti                   40
247600140919Non affidato a BRT non congruente con % ripartizione spesa trasporti           41
247700140919% Ripartizione Spesa Trasp. a 0. Non impostare il relativo non affidato a BRT  42
