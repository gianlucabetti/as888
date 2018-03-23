000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300080627      //?TRMK50R - Gestione/interrogazione informazioni commerciali
000400080206      //--------------------------------------------------------------
000500141001      //?  ATTENZIONE!!!!                                                      ?
000600141001      //?  Nella tabella IFO è previsto l'inserimento di un valore decimale    ?
000700141001      //?  sulle INFO, per ora questo flag viene usato SOLO per il ricavo      ?
000800141001      //?  medio spedizione calcolato in AUTOMATICO ed è una INFO non          ?
000900141001      //?  memorizzata sul file.                                               ?
001000141001      //?  In caso di necessità di INFO a valore con i decimali si dovrà       ?
001100141001      //?  variare il file TICPI00F per aggiungere un campo con i decimali.    ?
001200080206
001300080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001400080206
001500080206      //---------------------------------------------------------------
001600080206      //?Dichiarazione file.
001700080206      //---------------------------------------------------------------
001800050704
001900080627     FTICPI01L  UF A E           K DISK
002000080702     fTNCPO01L  if   e           k disk    infds(cpo01)
002100081003     f                                     rename(tncpo000:tncpo001)
002200081003     fTNCPO00f  uf   e             disk
002300140909     fTNCPO11L  uf a e           k disk
002400080206     fAZORG01L  if   e           k disk
002500080604     fTNTBE01l  if   e           k disk
002600081215     fTRMK50D   cf   e             workstn indds(IndDspF)
002700080206     f                                     infds(InfDspF)
002800080702     f                                     sfile(MK50S01 : S01nrr)
002900080206      //---------------------------------------------------------------
003000080207      // - Tasti funzionali a video
003100080207     d c_F01           c                   const(x'31')
003200080207     d c_F02           c                   const(x'32')
003300080207     d c_F03           c                   const(x'33')
003400080207     d c_F05           c                   const(x'35')
003500080207     d c_F06           c                   const(x'36')
003600080207     d c_F07           c                   const(x'37')
003700080207     d c_F08           c                   const(x'38')
003800080207     d c_F09           c                   const(x'39')
003900080207     d c_F10           c                   const(x'3A')
004000080207     d c_F12           c                   const(x'3C')
004100080207     d c_F13           c                   const(x'B1')
004200080207     d c_F14           c                   const(x'B2')
004300080207     d c_F15           c                   const(x'B3')
004400080207     d c_F16           c                   const(x'B4')
004500080207     d c_F17           c                   const(x'B5')
004600080207     d c_F18           c                   const(x'B6')
004700080207     d c_F19           c                   const(x'B7')
004800080207     d c_F20           c                   const(x'B8')
004900080207     d c_F21           c                   const(x'B9')
005000080207     d c_F22           c                   const(x'BA')
005100080207     d c_F23           c                   const(x'BB')
005200080207     d c_F24           c                   const(x'BC')
005300080207     d c_Enter         c                   const(x'F1')
005400080207     d c_RollDown      c                   const(x'F4')
005500080207     d c_RollUp        c                   const(x'F5')
005600080214
005700080214      // - Attributi di visualizzazione
005800080215      //  -  DspAtr() - Normale
005900080214     d C_dspatr_Norm   c                   const(x'20')
006000080215      //  -  DspAtr(RI)
006100080214     d C_dspatr_RI     c                   const(x'21')
006200080215      //  -  DspAtr(ND)
006300080214     d C_dspatr_ND     c                   const(x'27')
006400080215      //  -  DspAtr(BL) / Color(Red)
006500080214     d C_dspatr_BL     c                   const(x'28')
006600080206
006700080206      //---------------------------------------------------------------
006800080206      //?Definizione schiere.
006900080206      //---------------------------------------------------------------
007000080206
007100080206      // - Messaggi di errore
007200140718     d MSG             s             78    dim(50) ctdata perrcd(1)
007300140716
007400080702     d ifo             s              3    dim(50)
007500081015     d ifods           s             70    dim(50)
007600080627     d ifocg           s              2    dim(50)
007700080702     d ifoor           s              8    dim(50)
007800081126
007900081126     d IFOval          s              8    dim(100)
008000140716
008100080206      //---------------------------------------------------------------
008200080206      //?Definizione aree dati.
008300080206      //---------------------------------------------------------------
008400100226
008500080206      // - Dati utente
008600080206     d §AzUte        e ds                  extname(AZUTE00F)
008700080206     d                                     dtaara
008800080206     d §DatiUte      e ds                  extname(dDatiUte)
008900080206     d                                     dtaara
009000080206
009100080206      //---------------------------------------------------------------
009200080206      //?Definizione strutture dati.
009300080206      //---------------------------------------------------------------
009400080619     D CPO01           DS
009500080619     D  cp1NRR               397    400B 0
009600080206
009700080206      // - Status
009800080206     d Psds           sds
009900080206     d   SDSpgm          *proc
010000080206
010100080206      // - InfDS
010200080206     d InfDspF         ds
010300080207     d  dsp_aid              369    369a                                        AID byte
010400080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
010500080207     d  min_nrr              378    379i 0                                      Subfile min rrn
010600080207     d  num_rcds             380    381i 0                                      Subfile num rcds
010700080206
010800080206      // - Indicatori su DspF
010900080208     d IndDspF         ds
011000080206        // - Indicatori di gestione del subfile
011100080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
011500080206        // - Indicatori di errore
011600080206     d  errMessage                    1n   overlay(IndDspF : 28)
011700080606     d  errGenerico                   1n   overlay(IndDspF : 99)
011800080627        // - Indicatori vari
011900080627     d  InfoInt                       1n   overlay(IndDspF : 02)
012000080704     d  Infoyell                      1n   overlay(IndDspF : 03)
012100081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
012200120111     d  F03Attivo                     1n   overlay(IndDspF : 05)
012300120111     d  F12Attivo                     1n   overlay(IndDspF : 06)
012400080627     d  InfoImmiss                    1n   overlay(IndDspF : 10)
012500080703     d  ProtAnn                       1n   overlay(IndDspF : 13)
012600080703     d  Ifosott                       1n   overlay(IndDspF : 15)
012700080627     d  protriga                      1n   overlay(IndDspF : 16)
012800080627     d  protFat                       1n   overlay(IndDspF : 17)
012900081003     d  protVal                       1n   overlay(IndDspF : 18)
013000080627     d  protSped                      1n   overlay(IndDspF : 19)
013100080702     d  protfsn                       1n   overlay(IndDspF : 20)
013200080925     d  PFTBLU                        1n   overlay(IndDspF : 21)
013300140715     d  protVald                      1n   overlay(IndDspF : 24)
013400140715     d  protvala                      1n   overlay(IndDspF : 25)
013500080703     d  ErrSNA                        1n   overlay(IndDspF : 40)
013600080703     d  ErrFsn                        1n   overlay(IndDspF : 41)
013700081003     d  ErrVAL                        1n   overlay(IndDspF : 42)
013800140715     d  ErrVALD                       1n   overlay(IndDspF : 43)
013900080703     d  ErrPFT                        1n   overlay(IndDspF : 44)
014000080704     d  ErrANN                        1n   overlay(IndDspF : 45)
014100080206
014200080206      // - Parametri ricevuti
014300080206     d KPJBA         e ds
014400080702
014500080702     d TRMK50DS      e ds
014600100302     d TRMK52DS      e ds
014700080206
014800080206      // - Reperimento dati utente
014900080206     d TIBS34ds      e ds
015000080206     d dUte01        e ds
015100080702     d DLAT          e ds
015200080702
015300080702     d Dged          e ds
015400081006     d Difs          e ds
015500080206
015600080206      // - Ricerca/Controllo tabelle
015700080206     d TIBS02ds      e ds                  inz
015800081008      // - Ricerca sottipo info comm
015900081008     d TNTB70ds      e ds                  inz
016000080627
016100080206
016200080627      // - Tabella  Info commerciali e Categorie info
016300080627     d dIFO          e ds                  inz
016400080627     d dIFG          e ds                  inz
016500081003     d
016600081003     d dcpo01        e ds                  inz
016700140725
016800080206      //---------------------------------------------------------------
016900080206      //?Definizione variabili globali.
017000080206      //---------------------------------------------------------------
017100080206
017200080206      // - Flags booleani
017300080208     d $Fine           s               n   inz(*off)
017400080208     d $InzS01         s               n   inz(*on)
017500120105     d wAumenta10      s               n   inz(*off)
017600120510     d wAumenta10f     s               n   inz(*off)
017700120110     d wAzzera10       s               n   inz(*off)
017800120105     d wDiminuisci10   s               n   inz(*off)
017900120109     d wForzaAgg       s               n   inz(*off)
018000140716     d wForzaRms       s               n   inz(*off)
018100140716     d wForzaPft       s               n   inz(*off)
018200140725     d wEstero         s               n   inz(*off)
018300140721     d wNews           s               n   inz(*off)
018400080206
018500080627     d $Video          s              2    inz('S1')
018600080208     d S01nrr          s              4  0 inz
018700080627     d savcat          s              2    inz
018800080627     d Wcat            s              2    inz
018900081006     d WIFS            s              4    inz
019000080702     d Wifo            s              3    inz
019100081006     d w0040           s              4  0 inz
019200081007     d w005a           s              5    inz
019300081006     d XX              s              3  0 inz
019400080627     d Indx            s              3  0 inz
019500080703     d Primavolta      s              1    inz
019600080925     d Totprc          s              4  0 inz
019700081003     d Wctrprc         s              1    inz
019800081003     d Wdescat         s             21    inz
019900081215     d SPTPFT          s                   like(cpipft) inz
020000100302     d savspt          s                   like(cpipft) inz
020100081006     d LOG40           s              1    inz
020200081006     d OUTFSN          s              1    inz
020300081006     d OUTnrr          s              4  0 inz
020400081023     d LOGFSN          s              1    inz
020500081023     d LOGnrr          s              4  0 inz
020600081126     d MOInrr          s              4  0 inz
020700081015     d Waggiocpo       s              1    inz
020800081007     d BART_10         s              4  0 inz
020900081015     d InfoITA         s              1    inz
021000081015     d InfoEST         s              1    inz
021100100226     d InfoATR         s                   like(cpival)
021200100302     d SavATR          s                   like(cpival)
021300081126     d w008a           s              8    inz
021400081126     d MOIconta        s              3  0 inz
021500081126     d yy              s              3  0 inz
021600081126     d trovacpi        s              1    inz
021700081215     d savmod          s                   like(i50mod) inz
021800090520     d ultdata         s              8  0 inz
021900120110     d sav_bar_10      s                   like(bart_10)
022000120109     d wOggi           s              8  0 inz
022100140721
022200140715     d sav_vscpft      s                   like(vscpft) inz
022300140715     d sav_vscsna      s                   like(vscsna) inz
022400140716     d sav_valrms      s             12  3 inz
022500140716     d wVALrms         s             12  3 inz
022600140718     d noBRTpft        s                   like(cpipft) inz
022700140721     d siBRTpft        s                   like(cpipft) inz
022800140718     d ifoNRFnrr       s                   like(S01nrr) inz
022900140721     d ifoNROnrr       s                   like(S01nrr) inz
023000140721     d ifoNERnrr       s                   like(S01nrr) inz
023100140721     d ifoNAEnrr       s                   like(S01nrr) inz
023200140725     d ifoNTRnrr       s                   like(S01nrr) inz
023300140721     d savPRFval       s                   like(cpival) inz
023400140721     d savPROval       s                   like(cpival) inz
023500140721     d savAERval       s                   like(cpival) inz
023600140721     d savCAEval       s                   like(cpival) inz
023700140725     d savATRval       s                   like(cpival) inz
023800140721     d savNRFpft       s                   like(cpipft) inz
023900140721     d savNROpft       s                   like(cpipft) inz
024000140721     d savNERpft       s                   like(cpipft) inz
024100140721     d savNAEpft       s                   like(cpipft) inz
024200140725     d savNTRpft       s                   like(cpipft) inz
024300080414
024400080605     d Dataiso         s               d   datfmt(*iso)
024500080605     d Dataymd         s               d   datfmt(*ymd)
024600080605     d Datadmy         s               d   datfmt(*dmy)
024700120109     d wIFOdul         s              8  0
024800120109     d wIFOduliso      s               d   datfmt(*ISO)
024900140725     d wData           s              8  0
025000140725     d Dataeur         s               d   datfmt(*eur)
025100080604     d
025200080605     d                 DS
025300080605     d  AA                     1      4  0
025400080605     d  MM                     5      6  0
025500080605     d  GG                     7      8  0
025600080605     d DATA                    1      8  0
025700081009     d
025800081126     d CoRicercaMOI    c                   const('Ricerca motivo ITA/EST''?''')
025900081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
026000080206
026100080206      //---------------------------------------------------------------
026200080206      //?Definizione procedure usate.
026300080206      //---------------------------------------------------------------
026400080414      /copy gaitrasrc/srcprotopr,tibs02r
026500080414      /copy gaitrasrc/srcprotopr,tibs34r
026600081008      /copy gaitrasrc/srcprotopr,TNTB70r
026700081023      /copy gaitrasrc/srcprotopr,TRMK51r
026800100302      /copy gaitrasrc/srcprotopr,TRMK52r
026900080414      /copy gaitrasrc/srcprotopr,trmk_opm
027000080206
027100080206      //---------------------------------------------------------------
027200080206      //?Riepilogo indicatori.
027300080206      //---------------------------------------------------------------
027400080207      // - Comuni
027500080207      // 28    : Emissione messaggio di errore a video
027600080207      // - C01/S01
027700080207      // 30    : Condiziona SFLDSP    (*not)
027800080207      // 31    : Condiziona SFLDSPCTL (*not)
027900080207      // 30+31 : Condiziona SFLCLR
028000080207      // 32    : Condiziona SFLNXTCHG in S01
028100080207      // 50    : Errore: Opzione errata
028200080207      // - D01
028300080207      // - Comuni
028400080207      // 99    : Generico di Errore
028500080206      //---------------------------------------------------------------
028600080206
028700080206      //---------------------------------------------------------------
028800080206      //?M A I N - L I N E
028900080206      //---------------------------------------------------------------
029000080206
029100080627     c     *Entry        plist
029200080206     c                   parm                    KPJBA
029300080627     C                   PARM                    trMK50ds
029400080627
029500080627     C                   CLEAR                   O50F12
029600080627     C                   CLEAR                   O50F03
029700080627     C                   CLEAR                   O50F06
029800080702
029900080206      /free
030000080206
030100081215 1      if  i50tla<>'C'   ;
030200080925
030300080206       // Operazioni iniziali
030400080206       exsr RoutInz;
030500080206
030600081215 2      if  i50mod<>'C'   ;
030700081215
030800080206       // Gestione video
030900081215 3     DOW $Fine = *off;
031000081215 4       select;
031100080530
031200080530         // Videata di selezioni
031300080627           when $Video = 'S1';
031400080627             exsr GesS01;
031500120109           when $Video = 'W1';
031600120109             exsr GesW01;
031700080530
031800080702           other   ;
031900080206             $Fine = *on;
032000081215 4       endsl;
032100081215 3     ENDDO;
032200081215
032300081215 x2    else   ;
032400081215
032500081215       // modalità CONTROLLO : carico il sfl
032600081215       //                    solo ai fini del controllo
032700081215       exsr InzS01;
032800081215       exsr Contrs01   ;
032900081215
033000081215  3    if ErrGenerico=*off;
033100081215          o50ifotot='S';
033200081215  3    endif  ;
033300080925
033400081215 2     endif    ;
033500081215 1     endif    ;
033600080206
033700080206       // Operazioni finali
033800080206       exsr RoutEnd;
033900080206
034000080206       //--------------------------------------------------------------
034100080206       //?Operazioni iniziali.
034200080206       //--------------------------------------------------------------
034300080206       BEGSR RoutInz;
034400080627       $inzs01=*on;
034500080929       clear vsctes2         ;
034600080206
034700080703       // Solo la prima volta
034800080703 1     if primavolta=' '   ;
034900080703
035000080206         // Reperimento dati job
035100080206         exsr DatiJob;
035200081003
035300081003         // Se sono EDP accendo indicatore
035400081003         if %subst(knmus:1:3)<>'EDP'    ;
035500081003         UTENonEDP=*on   ;
035600081003         endif      ;
035700080627
035800080627         // Carico tabella per divisa corrente
035900080627         reset TIBS02ds;
036000080627         T02sif = knsif;
036100080627         T02cod = 'GED';
036200080627         T02ke1 = '1'  ;
036300080627         T02mod = 'C'  ;
036400080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
036500080627
036600080703 2       if t02err=' '  ;
036700080627         dGED=t02uni    ;
036800080627         else   ;
036900080627         clear DGED    ;
037000080703 2       endif   ;
037100080627
037200080627         // Carico tabella info commerciali
037300080627         xx=0                     ;
037400080627         setll ('IFO') tntbe01l   ;
037500080627         READE ('IFO') tntbe01l   ;
037600080627
037700080703 2       dow not %eof(tntbe01l)   ;
037800080703 3       if tbeatb=' '    ;
037900080627           difo=tbeuni    ;
038000080627           xx=xx+1        ;
038100080703           ifo(xx)=tbeke1 ;
038200080627           ifods(xx)=difo ;
038300080627           ifocg(xx)=§ifoifg ;
038400080627           // ordino in base alla categoria e  num.ordinamento
038500080703           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
038600080703 3       endif               ;
038700080627
038800080627         READE ('IFO') tntbe01l   ;
038900080703 2       enddo    ;
039000080627
039100080627         sorta  IFOOR  ;
039200080703
039300080703         Primavolta='N'   ;
039400080703 1       endif      ;
039500080702
039600080703         Infoint=*off   ;
039700080704         Infoyell=*off   ;
039800080703         InfoImmiss=*off   ;
039900080703         Ifosott=*off   ;
040000080703         Protriga=*off   ;
040100080703         ProtFat =*off   ;
040200081003         ProtVal  =*off   ;
040300140715         ProtVald = *off;
040400140715         ProtVala = *off;
040500080703         Protsped =*off   ;
040600080703         Protfsn  =*off   ;
040700080703         clear wcat  ;
040800080703         clear wifo  ;
040900080704         $Fine=*off   ;
041000140929         clear wVALrms;
041100080703
041200080702         // Vedo se modo gestione  o interrogazione
041300080703 1       if i50mod='I' ;
041400080702         InfoInt =*on  ;
041500080703         vsctes2='INTERROGAZIONE'    ;
041600080703 1       endif;
041700080703
041800080703         if i50pgm=*blanks   ;
041900080703         vscpgm='TRMK50R'   ;
042000080703         else     ;
042100080703         vscpgm=i50pgm     ;
042200080703         endif   ;
042300080703
042400080703
042500080703         // IMPOSTO IL potenziale in visualizzazione
042600080703         vsccpo=%editc(i50cpo:'X')   ;
042700080703         vscrag=i50rag   ;
042800120111
042900120111         //?Attivo i tasti funzione F03 e F12
043000120111           F03Attivo = *on;
043100120111           F12Attivo = *on;
043200080703
043300120105         //?05/12/2012
043400120105         //?Visto che il campo per potenziale annullato non è più usato
043500120105         //?lo recuperiamo per utilizzarlo come flag per controlli da
043600120105         //?fare sulle info.
043700120110         //?Nessun controllo
043800120110           IF  I50atb = ' ';
043900120110             wAumenta10    = *off;
044000120510             wAumenta10f   = *off;
044100120110             wAzzera10     = *off;
044200120110             wDiminuisci10 = *off;
044300120110           ENDIF;
044400120110         //?Controllo per azzeramento
044500120110           IF  I50atb = '0';
044600120110             wAzzera10     = *on;
044700120110             wAumenta10    = *off;
044800120510             wAumenta10f   = *off;
044900120110             wDiminuisci10 = *off;
045000120111             F03Attivo = *off;
045100120111             F12Attivo = *off;
045200120110           ENDIF;
045300120110         //?Controllo per aumento
045400120105           IF  I50atb = '+';
045500120110             wAumenta10    = *on;
045600120510             wAumenta10f   = *off;
045700120110             wAzzera10     = *off;
045800120110             wDiminuisci10 = *off;
045900120111             F03Attivo = *off;
046000120111             F12Attivo = *off;
046100120105           ENDIF;
046200120510         //?Controllo per aumento semi obbligatorio
046300120510           IF  I50atb = '%';
046400120510             wAumenta10f   = *on;
046500120510             wAumenta10    = *off;
046600120510             wAzzera10     = *off;
046700120510             wDiminuisci10 = *off;
046800120510             F03Attivo = *off;
046900120510             F12Attivo = *off;
047000120510           ENDIF;
047100120110         //?Controllo per diminuzione
047200120109           IF  I50atb = '-';
047300120110             wDiminuisci10 = *on;
047400120110             wAumenta10    = *off;
047500120510             wAumenta10f   = *off;
047600120110             wAzzera10     = *off;
047700120111             F03Attivo = *off;
047800120111             F12Attivo = *off;
047900120105           ENDIF;
048000081003
048100081003         // Se già  inserite tutte le info, non si possono più annulla
048200081003         //  ma solo sistemare
048300081003         if i50obl=' '  and i50ifotot='S'    ;
048400081003         i50obl='S'   ;
048500081003         endif        ;
048600080627
048700080703         // VERIFICO SE ci sono già dei dati per il potenziale
048800080703         setll   i50cpo  ticpi01l    ;
048900080703         reade   i50cpo  ticpi01l    ;
049000080703 1       dow not %eof(ticpi01l) and cpiatb<>' ' ;
049100080703         reade   i50cpo  ticpi01l    ;
049200080703 1       enddo   ;
049300080703
049400080703 1       if %eof(ticpi01l) ;
049500080703         Infoimmiss=*on     ;
049600080929 1       if vsctes2= *blanks    ;
049700080703         vsctes2='  IMMISSIONE  '    ;
049800080929         endif   ;
049900080703         else    ;
050000080929 1       if vsctes2= *blanks    ;
050100080703         vsctes2='  VARIAZIONE  '    ;
050200080703 1       endif    ;
050300080929 1       endif    ;
050400080703
050500080627         ENDSR;
050600080206
050700080206       //--------------------------------------------------------------
050800080206       //?Reperimento Dati del job (Utente/Operativi).
050900080206       //--------------------------------------------------------------
051000080206       BEGSR DatiJob;
051100080206
051200080206         in(E) §AzUte;
051300080206         if NOT %error;
051400080206           in(E) §DatiUte;
051500080206         endif;
051600080206         if %error or RSut = *blanks;
051700080206           clear TIBS34ds;
051800080206           tibs34r(tibs34ds);
051900080206           in §AzUte;
052000080206           in §DatiUte;
052100080206         endif;
052200080206
052300080206       ENDSR;
052400080206
052500080206       //--------------------------------------------------------------
052600080627       //?Gestione SFL 01
052700080206       //--------------------------------------------------------------
052800080409       BEGSR GesS01;
052900080207
053000080207         // Inizializzazione videata
053100080409         if  $InzS01 = *on;
053200080409            exsr InzS01;
053300080703         // Posizionamento cursore
053400080703           s01nrr = 1;
053500080703           sfldsp_n=*off;
053600080409            $InzS01  = *off;
053700080207         endif;
053800080207
053900140725         //?Cerco l'ultima data di conferma info commerciali
054000140909           chain(n) I50cpo TNCPO11L;
054100140909           IF  %found(TNCPO11L);
054200140909             vdulconf = CPO1ducifo;
054300140909           ENDIF;
054400140909           IF  vdulconf > *zeros;
054500140909             dataISO = %date(vdulconf:*iso);
054600140909             dataEUR = dataISO;
054700140909             vdulconf = %dec(dataEUR);
054800140909           ENDIF;
054900080207
055000080207         // Emissione Testata e Piede con tasti funzionali abilitati
055100080207         if  errGenerico = *off;
055200080702           write  MK50Z01;
055300080207         endif;
055400080703
055500080703         // Posizionamento cursore
055600080703         if  C01csr  > *zero;
055700080703           C01rcd = C01csr;
055800080703         endif  ;
055900080207
056000080207         // Emissione videata
056100080702         exfmt  MK50C01;
056200080410
056300080207         reset errMessage;
056400080207         reset errGenerico;
056500080702         clear Vscmsg;
056600081003         errVAL=*off     ;
056700140715         errVALD = *off;
056800080703         errPFT=*off     ;
056900080703         errSNA=*off     ;
057000080703         errFSN=*off     ;
057100080704         errANN=*off     ;
057200120109
057300120109         //?Cerco l'ultima data variazione info commerciali
057400120124         //?Se NON è interrogazione e non è immissione
057500120109           clear dCPO01;
057600120124           IF  not InfoInt and not Infoimmiss;
057700120109             chain(n) I50cpo TNCPO01L;
057800120109             IF  %found(TNCPO01L);
057900120109               dCPO01 = CPOrst;
058000120109             ENDIF;
058100120109             IF  §CPOifodul > *zeros;
058200120109               wIFOdul    = %dec(§CPOifodul:8:0);
058300120109               wIFOdulISO = %date(wIFOdul : *ISO);
058400120109               wOggi = %dec(%date());
058500120109               dataISO = %date(wOggi : *ISO);
058600120109         //?Se la data ultima variazione è più vecchia di 1 anno da oggi windows
058700120109         //?di avviso all'utente
058800120110               IF  %diff(dataISO : wIFOdulISO : *months) > 12 and not wForzaAgg;
058900120109                 $Video = 'W1';
059000120109                 ErrGenerico = *on;
059100120109                 ErrMessage  = *on;
059200120109                 VSCmsg      = msg(32);
059300120109                 wForzaAgg   = *on;
059400120109                 leavesr;
059500120109               ENDIF;
059600120109             ENDIF;
059700120109           ENDIF;
059800080207
059900080523 1       SELECT;
060000080207
060100080207         // - F3=Fine
060200080523 1         WHEN  dsp_aid = c_F03;
060300080409            $Fine = *on;
060400081006            o50f03='S'   ;
060500080207
060600080207         // - F12=Ritorno
060700080523 1         WHEN  dsp_aid = c_F12;
060800080702            $Fine = *on;
060900081006            o50f12='S'   ;
061000080414
061100080530 x1      // Invio / F6
061200080207           OTHER;
061300081006
061400081006        // Non effettuo controlli in interrogazione
061500081006 1           if i50mod<>'I' ;
061600081006               exsr ContrS01 ;
061700081006 2             if  errGenerico = *on;
061800081006                 leavesr;
061900081006 2             endif;
062000081006 2           endif;
062100080530
062200081023         // F6=conferma Aggiornamento  F10=anche stampa
062300081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
062400080702           exsr   ConfAggio ;
062500081006           o50f06='S'   ;
062600081023
062700081023              if dsp_aid=c_F10;
062800081023               kpjbu=trmk50ds  ;
062900081023               callp TRMK51R (kpjba)   ;
063000081023              endif    ;
063100081023
063200080704           $Fine = *on;
063300080530           endif  ;
063400080207
063500080523 1       ENDSL;
063600080207
063700080207       ENDSR;
063800120109
063900120109       //--------------------------------------------------------------
064000120109       //?Gestione Window errori
064100120109       //--------------------------------------------------------------
064200120109       BEGSR GesW01;
064300120109
064400120109         reset ErrMessage;
064500120109         reset ErrGenerico;
064600120109
064700120109         w1riga = vscmsg;
064800120109         exfmt  MK50W01;
064900120109
065000120109         clear Vscmsg;
065100120109         errVAL  = *off;
065200140715         errVALD = *off;
065300120109         errPFT  = *off;
065400120109         errSNA  = *off;
065500120109         errFSN  = *off;
065600120109         errANN  = *off;
065700120109
065800120109         $Video = 'S1';
065900120109
066000120109       ENDSR;
066100080207
066200080526       //--------------------------------------------------------------
066300080702       //?controlli SFL01
066400080409       //--------------------------------------------------------------
066500080409       BEGSR ContrS01;
066600140716
066700081215       clear o50ifomin;
066800081215       clear wctrprc  ;
066900081006       clear wdescat   ;
067000081006       clear totprc   ;
067100081006       clear sptpft   ;
067200081006       clear OUTFSN   ;
067300081006       clear OUTnrr   ;
067400081023       clear LOGFSN   ;
067500081023       clear LOGnrr   ;
067600081006       clear LOG40    ;
067700081007       clear bart_10  ;
067800081015       clear infoITA  ;
067900081015       clear infoEST  ;
068000081126       clear IFOval   ;
068100081126       clear MOIconta ;
068200081126       clear MOInrr   ;
068300100226       clear InfoATR  ;
068400140718       clear noBRTpft;
068500140721       clear siBRTpft;
068600140721       clear savPRFval;
068700140721       clear savPROval;
068800140721       clear savAERval;
068900140721       clear savCAEval;
069000140725       clear savATRval;
069100140721       clear savNRFpft;
069200140721       clear savNROpft;
069300140721       clear savNERpft;
069400140721       clear savNAEpft;
069500140725       clear savNTRpft;
069600140725       wEstero = *off;
069700140716
069800081126       yy=1           ;
069900080703       s01nrr=1  ;
070000080703       chain s01nrr    mk50s01    ;
070100080703
070200081006 0     dow %found    ;
070300080925       // Escludo i record di descrizione categoria
070400081006 1     if vshimm= 'C'    ;
070500081003
070600081003       // A cambio di categoria, se c'era controllo obbligatorio del
070700081003       //  100%   controllo ed eventualmente segnalo errore
070800081006       EXSR CambioCAT    ;
070900081006       if errGenerico=*on  ;
071000081006       leavesr;
071100081006       endif              ;
071200081006
071300081006x1     else    ;
071400081006
071500080925       pftblu=*off       ;
071600080925
071700081003       // se valori calcolati da pgm li pulisco prima dei controlli
071800081007 2     if vshsnf=' ' and vshprpft=' '   ;
071900080925       clear vscpft   ;
072000140717       // clear vscvft   ;
072100081007 2     endif          ;
072200080703
072300080703       // Imposto la divisa, se impostato il fatturato
072400140717 2     //if vscpft>0 and vscvft=*blanks   ;
072500140717       //vscvft=§gedcr   ;
072600140717       //errGenerico = *on;
072700140717 2     //endif    ;
072800080703
072900140717 2     //if vscpft=0 and vscvft<>*blanks   ;
073000140717       //clear vscvft   ;
073100140717       //errGenerico = *on;
073200140717 2     //endif   ;
073300140716
073400140716       //?Se valore fatturato < 0 errore
073500140716       IF  VSCpft < 0;
073600140716         VSCmsg = msg(25);
073700140716         errPFT=*on;
073800140716         EXSR AggioSFL   ;
073900140716         leavesr;
074000140716       ENDIF;
074100140716
074200140716       //?Se valore fatturato impostato e > di 999000000 errore
074300140716       IF  VSCpft > 999000000;
074400140716         VSCmsg = msg(17);
074500140716         errPFT=*on;
074600140716         EXSR AggioSFL   ;
074700140716         leavesr;
074800140716       ENDIF;
074900140716
075000140716       //?Se valore fatturato impostato e > di 999999 warning
075100140716       IF  VSCpft > 999999 and not wForzaPft;
075200140716         VSCmsg = msg(23);
075300140716         errPFT=*on;
075400140716         wForzaPft = *on;
075500140716         EXSR AggioSFL   ;
075600140716         leavesr;
075700140716       ENDIF;
075800140716
075900140716       //?Le spedizioni non possono essere superiori del valore fatturato
076000140722       IF  VSCsna > VSCpft and VSCifo = 'SPT';
076100140716         VSCmsg = msg(24);
076200140716         errPFT=*on;
076300140716         EXSR AggioSFL   ;
076400140716         leavesr;
076500140716       ENDIF;
076600140716
076700140716       //?Se spedizioni < 0 errore
076800140716       IF  VSCsna < 0;
076900140716         VSCmsg = msg(25);
077000140716         errSNA=*on;
077100140716         EXSR AggioSFL   ;
077200140716         leavesr;
077300140716       ENDIF;
077400140723
077500140723       //?Se VAL < 0 errore
077600140723       IF  vscval < 0;
077700140723         VSCmsg = msg(25);
077800140723         errVAL=*on;
077900140723         EXSR AggioSFL   ;
078000140723         leavesr;
078100140723       ENDIF;
078200140723
078300140723       //?Se VALD < 0 errore
078400140723       IF  vscvald < 0;
078500140723         VSCmsg = msg(25);
078600140723         errVALD=*on;
078700140723         EXSR AggioSFL   ;
078800140723         leavesr;
078900140723       ENDIF;
079000080703
079100080704       // se info da annullare e non c'e' la "A", msg forzabile
079200081215       //  non faccio se modalità controlli
079300081215 2     if i50mod<>'C' and vshann='S'  and vscatb=' '  ;
079400080704           errANN=*on;
079500080704           // se  contiene dati, msg forzabile
079600081007 3         if vscfsn='S'or vscval>0 or vscpft>0 or vscsna>0   ;
079700080704           vscmsg = Msg(07);
079800080704           vshann='X'      ;
079900080704           // Se non contiene dati abbligatorio l'annullamento
080000081007 x3        else            ;
080100080704           vscmsg = Msg(08);
080200081007 3         endif           ;
080300080704           EXSR AggioSFL   ;
080400080704           leavesr;
080500081007 2     endif    ;
080600110707
080700110707       // se c'e' "O"  obbligatorio l'annullamento
080800110707 2     if i50mod<>'C' and vshann='O'  and vscatb=' '  ;
080900110707           errANN=*on;
081000110707           vscmsg = Msg(08);
081100110707           EXSR AggioSFL   ;
081200110707           leavesr;
081300110707 2     endif    ;
081400080704
081500080703       // Se richiesto inserimento obbligatorio, per i dati
081600080703       //  obbligatori da tabella segnalo errore
081700081007 2     if i50obl='S' and vshobl='S'                 ;
081800081007 3     if vscatb='A'    or
081900081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
082000081003         if vshsne<>' ';
082100080703           errFSN=*on;
082200081003         endif   ;
082300081003         if vshsnv<>' ';
082400081003           errVAL=*on;
082500081003         endif   ;
082600140715         IF  vshsnvd <> *blanks;
082700140715           errVALD = *on;
082800140715         ENDIF;
082900081003         if vshsnf<>' ';
083000081003           errPFT=*on;
083100081003         endif   ;
083200080703           vscmsg = Msg(01);
083300080703           EXSR AggioSFL   ;
083400080703           leavesr;
083500081007 3     endif   ;
083600081007 2     endif   ;
083700081007
083800081007       // Non accetto se non previsto il valore '?'
083900081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
084000081007           errFSN=*on;
084100081007           vscmsg = Msg(20);
084200081007           EXSR AggioSFL   ;
084300081007           leavesr;
084400081007 3     endif    ;
084500081007       if vshifgv<>'?' and vscfsn='?'  ;
084600081007           errFSN=*on;
084700081007           vscmsg = Msg(21);
084800081007           EXSR AggioSFL   ;
084900081007           leavesr;
085000081007 3     endif    ;
085100080703
085200080703       // Se immesso "N" non indicare gli altri campi
085300081007 2     if vscfsn='N' and (vscval>0 or vscpft>0 or vscsna>0)   ;
085400080703       if vshsnf<>' '    ;
085500080703           errPFT=*on;
085600080703       else    ;
085700081003          errVAL=*on ;
085800080703       endif  ;
085900080703           vscmsg = Msg(02);
086000080703           EXSR AggioSFL   ;
086100080703           leavesr;
086200081007 2     endif   ;
086300080703
086400081003       // Se immesso almeno un dato, controllo gli obbligatori ed
086500081215       //   i facoltativi. I  facoltativi non li guardo in modalità
086600081215       //   solo controllo
086700081031       if vscatb =' ' and
086800081031 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval>0)            ;
086900081003
087000081007 3     if vscpft=0    and vshsnf='O'   ;
087100081003           errPFT=*on;
087200081003           vscmsg = Msg(03);
087300081003           EXSR AggioSFL   ;
087400081003           leavesr;
087500081007 3      endif    ;
087600100503
087700100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
087800100503 3     if i50obl='S' and
087900100503          I50mod<>'C' and  vscpft=0    and vshsnf='S'   ;
088000081003           errPFT=*on;
088100081003           vscmsg = Msg(05);
088200081003           vshsnf='X'      ;
088300081003           EXSR AggioSFL   ;
088400081003           leavesr;
088500081007 3     endif    ;
088600081007 3     if vscsna=0    and vshsns='O'   ;
088700081003           errSNA=*on;
088800081003           vscmsg = Msg(10);
088900081003           EXSR AggioSFL   ;
089000081003           leavesr;
089100081007 3     endif    ;
089200100503
089300100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
089400100503 3     if i50obl='S' and
089500100503          i50mod<>'C' and vscsna=0    and vshsns='S'   ;
089600081003           errSNA=*on;
089700081003           vscmsg = Msg(06);
089800081003           vshsns='X'      ;
089900081003           EXSR AggioSFL   ;
090000081003           leavesr;
090100081007 3     endif    ;
090200081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
090300081007           errFSN=*on;
090400081007           vscmsg = Msg(19);
090500081007           EXSR AggioSFL   ;
090600081007           leavesr;
090700081007 3     endif    ;
090800100503 3     if i50obl='S' and
090900100503 3        i50mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
091000081007           errFSN=*on;
091100081007           vscmsg = Msg(22);
091200081007           vshsne='X'      ;
091300081007           EXSR AggioSFL   ;
091400081007           leavesr;
091500081007 3     endif    ;
091600081007 3     if (vscval=0    and vshsnv='O')  or
091700100503          (vscval=0    and vshsnv='S' and i50mod<>'C'AND I50OBL='S')   ;
091800081007 4        if vshsnv='O'    ;
091900081003           vscmsg = Msg(11);
092000081003          else   ;
092100081215             vscmsg = Msg(04);
092200081215             vshsnv='X'      ;
092300081215 4        endif  ;
092400081007 4        if vsctva='%  '   ;
092500081003          %subst(vscmsg:37:18)='della % fatturato '     ;
092600081007 4        endif     ;
092700081007 4        if vsctva='KG '   ;
092800081003          %subst(vscmsg:37:18)='del peso in Kg.   '     ;
092900081007 4        endif     ;
093000081215
093100081003           errVAL=*on;
093200081003           EXSR AggioSFL   ;
093300081003           leavesr;
093400081215 3     endif    ;
093500140721
093600140715 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
093700140715           (vscvald = 0 and vshsnvd = 'S'   and
093800140715            I50mod <> 'C' and I50obl = 'S');
093900140715 4        IF  vshsnvd = 'O';
094000140715            vscmsg = Msg(01);
094100140715          ELSE;
094200140715            vscmsg = Msg(01);
094300140715            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
094400140715            vshsnvd = 'X';
094500140715 4        ENDIF;
094600140715          errVALD = *on;
094700140715          EXSR AggioSFL;
094800140715          leavesr;
094900140715 3     ENDIF;
095000140715
095100140715       //?Mi salvo il valore del traffico totale
095200140715 3     IF  vscifo = 'SPT';
095300140716         sav_vscpft = vscpft;
095400140716         sav_vscsna = vscsna;
095500140715       ENDIF;
095600081003
095700081003       // se Valore in % --> non deve superare 100%
095800081007 3     if  vscval>0 and vsctva='%  '    ;
095900081007 4       if vscval>100    ;
096000081003          errVAL=*on ;
096100081003          vscmsg = Msg(12);
096200081003          EXSR AggioSFL   ;
096300081003          leavesr;
096400081007 4       endif      ;
096500081003
096600080925       // Sommo la percentuale se presente per ogni categoria
096700080925       //  se supera 100% segnalo errore
096800081006       //  solo se previsto controllo sulla %
096900081007 4     if wctrprc='P'     ;
097000081006
097100110707       // Non sommo le info annullate perchè non le devo considerare valide
097200110707       if vshann=' '   ;
097300110707         totprc=totprc+vscval    ;
097400110707       endif  ;
097500110707
097600081007 5       if totprc>100    ;
097700081003         errVAL=*on ;
097800080925         vscmsg = Msg(09);
097900080925         EXSR AggioSFL   ;
098000080925         leavesr;
098100081007 5       endif      ;
098200081007 4     endif      ;
098300080925
098400081003       // Se immessa la %, visualizzo in base a SPT
098500080925       exsr   VISUALimp     ;
098600081007 3     endif      ;
098700081007 2     endif      ;
098800140716
098900140716       //?Se Valore con decimali in % --> non deve superare 100%
099000140716 3     IF  vscvald <> 0 and vsctva = '%  ';
099100140716 4       IF  vscvald > 100;
099200140716           errVALD = *on;
099300140716           vscmsg = Msg(12);
099400140716           EXSR AggioSFL   ;
099500140716           leavesr;
099600140716 4       ENDIF;
099700140716 4     ENDIF;
099800140715
099900140715       //?Per Ricavo medio spedizione
100000140715 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
100100140715       //?calcolo il valore
100200140715 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
100300140715           wVALrms = sav_vscpft / sav_vscsna;
100400140715         ELSE;
100500140715           wVALrms = 0;
100600140715         ENDIF;
100700140715       //?reimposto il flag di forzatura
100800140715         IF  wVALrms <> sav_valrms;
100900140716           wForzaRms = *off;
101000140715         ENDIF;
101100140715       //?imposto il campo del video
101200140919         IF  wVALrms > 999;
101300140919           vscvald = 999;
101400140715         ELSE;
101500140715           vscvald = wVALrms;
101600140715         ENDIF;
101700140715       //?msg info se inferiore a 4
101800140716         IF  VSCvald < 4 and not wForzaRms and
101900140715             wVALrms <> sav_valrms;
102000140715           errVALD = *on;
102100140715           vscmsg = msg(35);
102200140715           exsr AggioSFL;
102300140716           wForzaRms = *on;
102400140715           sav_valrms = wVALrms;
102500140715           leavesr;
102600140715         ENDIF;
102700140715       //?msg info se superiore a 50
102800140716         IF  VSCvald > 50 and not wForzaRms and
102900140715             wVALrms <> sav_valrms;
103000140715           errVALD = *on;
103100140715           vscmsg = msg(36);
103200140715           exsr AggioSFL;
103300140716           wForzaRms = *on;
103400140715           sav_valrms = wVALrms;
103500140715           leavesr;
103600140715         ENDIF;
103700140715       ENDIF;
103800140716
103900140716       // se INFO obbligatoria
104000140716       // errore se info non inserita
104100140721       // flag di comodo per non richiedere obbligatoria la INFO ora in fase di test
104200140721       // non so ancora se è obbligatoria sempre o solo se I50obl = 'S'
104300140721       IF  VSHobl = 'S' and wnews = *on;
104400140716         SELECT;
104500140716           WHEN  vshsnv <> *blanks and vscval = 0;
104600140716             errVAL = *on;
104700140716             vscmsg = Msg(01);
104800140716             EXSR AggioSFL;
104900140716             leavesr;
105000140716           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
105100140716             errVALD = *on;
105200140716             vscmsg = Msg(01);
105300140716             EXSR AggioSFL;
105400140716             leavesr;
105500140716           WHEN  vshsnf <> *blanks and vscpft = 0;
105600140716             errPFT = *on;
105700140716             vscmsg = Msg(01);
105800140716             EXSR AggioSFL;
105900140716             leavesr;
106000140716           WHEN  vshsns <> *blanks and vscsna = 0;
106100140716             errSNA = *on;
106200140716             vscmsg = Msg(01);
106300140716             EXSR AggioSFL;
106400140716             leavesr;
106500140716           WHEN  vshsne <> *blanks and vscfsn = *blanks;
106600140716             errFSN = *on;
106700140716             vscmsg = Msg(01);
106800140716             EXSR AggioSFL;
106900140716             leavesr;
107000140716         ENDSL;
107100140716       ENDIF;
107200140725
107300140725       // Paesei esteri ammessi solo se inserita almeno 1 % di ripartizione estera
107400140725       IF  VSCifo = '50 ' and VSCatb = *blanks and
107500140725           VSCfsn <> *blanks and not wEstero;
107600140725         errFSN = *on;
107700140725         VSCmsg = Msg(37);
107800140725         EXSR AggioSFL;
107900140725         leavesr;
108000140725       ENDIF;
108100081006
108200081006       // Concorrenti logistica ammessi solo se outsourcing=S
108300110707 2     if vscifo='40 '  and vscatb=' ' and (vscfsn<>' ' or vscpft>0 or vscsna>0
108400110707                            or vscval>0) ;
108500081006       if outfsn<>'S'   ;
108600081006         errFSN=*on ;
108700081006         vscmsg = Msg(14);
108800081006         EXSR AggioSFL   ;
108900081006         leavesr;
109000081006       endif             ;
109100081007
109200081006       // Memorizzo presenza info 40  se immesso Si o presente altro
109300081006       //  importo
109400110707         if vshann=' ' and vscfsn<>'N';
109500081007         LOG40='S'         ;
109600081007         endif             ;
109700081007 2     endif             ;
109800081008
109900081126       // Sottotipo col '?' --> Se '?' richiamo pgm di ricerca
110000081126 2     if vscfsn='?' ;
110100081008       clear tntb70ds ;
110200081008       itb70cpo=i50cpo;
110300081023       itb70rag=i50rag ;
110400081126       itb70tpf=vscifo  ;
110500081008       itb70pgm=i50pgm  ;
110600081008       callp TNTB70R (kpjba:tntb70ds)  ;
110700081008       clear vscfsn       ;
110800081126 3     if otb70spf>*zeros    ;
110900081008         vscspp=otb70spf  ;
111000081008         vsdifo=otb70dspf ;
111100081008         vscval=%int(otb70spf) ;
111200081126 3     endif         ;
111300081008       c01csr=s01nrr    ;
111400081008       exsr AggioSFL    ;
111500081008       leavesr       ;
111600081126 2     endif         ;
111700081007
111800081126 2     if vscval=0    and vshifgv='?' ;
111900081007       clear vscspp      ;
112000081009       clear vsdifs      ;
112100081009       vsdifo=CoRicerca  ;
112200081126 2     endif     ;
112300081007
112400081126       // Motivo non affidamento a noi: da richidere solo se corriere BART
112500081126       //  non è al 100
112600081126 2     if  vscifo='MOI'   ;
112700081126
112800081126         if vscval=0   ;
112900081126          vsdifo=CoRicercaMOI  ;
113000081126         endif         ;
113100081126
113200081126 3       if bart_10=100 and vscval>0        ;
113300081126           errVAL=*on ;
113400081126           vscmsg = Msg(16);
113500081126           EXSR AggioSFL   ;
113600081126           leavesr;
113700081126 3       endif             ;
113800081126
113900081126         if vscval>0      ;
114000081126           MOIconta=moiconta+1   ;
114100081126         endif     ;
114200081126
114300081126         moinrr=s01nrr    ;
114400081126
114500081126 2     endif            ;
114600081008
114700081007       // Controllo
114800081126 2     if vscval>0    and vshifgv='?' ;
114900081007       w005a=%editc(vscval:'X')    ;
115000081126       chain ('IFS':vscifo:%subst(w005a:2:4))  tntbe01l    ;
115100081126 3       if not %found(tntbe01l) or tbeatb<>' ' or %subst(w005a:1:1)<>'0';
115200081007          errVAL=*on ;
115300081007          vscmsg = Msg(18);
115400081007          EXSR AggioSFL   ;
115500081007          leavesr;
115600081126 x3       else                        ;
115700081007          vscspp=%subst(w005a:2:4)    ;
115800081007          difs=tbeuni ;
115900081007          vsdifo=§ifsdes              ;
116000081007          vsdifs=§ifsdel              ;
116100081126  3      endif             ;
116200081126
116300081126        //  Memorizzo i codici per vedere se sono doppi
116400081126        w008a=vscifo+%editc(vscval:'X')   ;
116500081126        Indx=%lookup(w008a:IFOval)   ;
116600081126 3      if Indx>0    ;
116700081126         errVAL=*on ;
116800081126 4       if vscifo='MOI'   ;
116900081126           vscmsg = Msg(28);
117000081126         else      ;
117100081126           vscmsg = Msg(30);
117200081126 4       endif     ;
117300081126         EXSR AggioSFL   ;
117400081126         leavesr;
117500081126
117600081126 x3     else   ;
117700081126          ifoval(yy)=w008a  ;
117800081126          yy=yy+1   ;
117900081126          // Se c'e' un MOI=9999 e uno no 9999, errore
118000081126          w008a='MOI09999'  ;
118100081126          Indx=%lookup(w008a:IFOval) ;
118200081126  4       if Indx>0   and moiconta>1 ;
118300081126            errVAL=*on ;
118400081126            vscmsg = Msg(29);
118500081126            EXSR AggioSFL   ;
118600081126            leavesr;
118700081126  4       endif    ;
118800081126  3     endif             ;
118900081126  2    endif             ;
119000081007
119100081126  2    if vscatb=' '  ;
119200081031
119300080925       // Se info SPT - totale spesa trasporti,  la memorizzo
119400080925       //  per il calcolo delle %
119500080925       if vscifo='SPT' ;
119600080925       SPTPFT=vscpft   ;
119700080925       endif           ;
119800140725
119900140725       // Se ripartizione spesa ESTERA lo memorizzo
120000140725       IF  VSHie = 'E' and VSCval > 0;
120100140725         wEstero = *on;
120200140725       ENDIF;
120300140725
120400081023       // Se info LOG - logistica  memorizzo per dare obbligatorio outsourci
120500081023       if vscifo='LOG' ;
120600081023       LOGfsn=vscfsn   ;
120700081023       LOGnrr=s01nrr   ;
120800081006       endif           ;
120900081023       // Se info OUT - outsourcing memorizzo per dare obbligatori
121000081023       //  i concorrenti della logistica
121100081023       if vscifo='OUT' ;
121200081023       OUTfsn=vscfsn   ;
121300081023       OUTnrr=s01nrr   ;
121400081023       endif           ;
121500140718
121600081007       // Se info 10 _BAR - concorrenti utilizzati bartolini
121700081007       //  memorizzo la % che ha
121800081007       if vscifo='10' and vscspp='_BAR' ;
121900081007       bart_10=vscval   ;
122000120105
122100120110         //?Controllo % affidamento a BRT
122200120110         SELECT;
122300120110         //?Controllo se devo azzerare la % di affidato a BRT
122400120110         WHEN  wAzzera10 and bart_10 > 0;
122500120110           errVAL = *on;
122600120125           VSCmsg = Msg(33);
122700120110           EXSR AggioSFL;
122800120110           leavesr;
122900120110         //?Controllo se devo aumentare la % di affidato a BRT
123000120111         WHEN  wAumenta10 and bart_10 < sav_bar_10 and bart_10 < 100;
123100120110           errVAL = *on;
123200120125           VSCmsg = Msg(31);
123300120110           EXSR AggioSFL;
123400120110           leavesr;
123500120510         //?Controllo se devo aumentare la % di affidato a BRT solo se = 0
123600120510         WHEN  wAumenta10f and bart_10 < sav_bar_10 and bart_10 = 0;
123700120510           errVAL = *on;
123800120510           VSCmsg = Msg(31);
123900120510           EXSR AggioSFL;
124000120510           leavesr;
124100120110         //?Controllo se devo diminuire la % di affidato a BRT
124200120110         WHEN  bart_10 > 0 and
124300120110               wDiminuisci10 and bart_10 > sav_bar_10;
124400120110           errVAL = *on;
124500120125           VSCmsg = Msg(34);
124600120110           EXSR AggioSFL;
124700120110           leavesr;
124800120110         ENDSL;
124900120105
125000081007       endif           ;
125100140718
125200100302       // Se info ATR - altro di ripartizione memorizzo la %
125300100226       if vscifo='ATR'                  ;
125400100226       infoATR=vscval   ;
125500100226       endif           ;
125600140721
125700140721       //?Salvo le % di ripartizione spesa totale
125800140721       SELECT;
125900140721         WHEN  VSCifo = 'PRF';
126000140721           savPRFval = VSCval;
126100140721         WHEN  VSCifo = 'PRO';
126200140721           savPROval = VSCval;
126300140721         WHEN  VSCifo = 'AER';
126400140721           savAERval = VSCval;
126500140721         WHEN  VSCifo = 'CAE';
126600140721           savCAEval = VSCval;
126700140725         WHEN  VSCifo = 'ATR';
126800140725           savATRval = VSCval;
126900140721       ENDSL;
127000140718
127100140919       //?Salvo e sommo il NON affidato a BRT
127200140721       SELECT;
127300140721         WHEN  VSCifo = 'NRF';
127400140721           savNRFpft = VSCpft;
127500140721           noBRTpft += VSCpft;
127600140721         WHEN  VSCifo = 'NRO';
127700140721           savNROpft = VSCpft;
127800140721           noBRTpft += VSCpft;
127900140721         WHEN  VSCifo = 'NER';
128000140721           savNERpft = VSCpft;
128100140721           noBRTpft += VSCpft;
128200140721         WHEN  VSCifo = 'NAE';
128300140721           savNAEpft = VSCpft;
128400140721           noBRTpft += VSCpft;
128500140725         WHEN  VSCifo = 'NTR';
128600140725           savNTRpft = VSCpft;
128700140725           noBRTpft += VSCpft;
128800140721       ENDSL;
128900100226
129000081126 2     endif        ;
129100080925
129200080703       EXSR AggioSFL   ;
129300081006 1     endif   ;
129400081125
129500080703
129600080703       s01nrr=1+s01nrr  ;
129700080703       chain s01nrr    mk50s01    ;
129800081006 0     enddo         ;
129900081006
130000081006       // Controllo ultima categoria
130100081006       EXSR CambioCAT     ;
130200081006
130300081215       // A questo punto le info minime sono presenti
130400081215       // solo per controlli obbligatori
130500081215       if i50obl='S' ;
130600081215         o50ifomin='S' ;
130700081215       endif ;
130800081215
130900081126       // Un motivo non affidamento obbligatorio se Bart<100%
131000081126 3     if bart_10<100 and MOIconta =0   and i50obl='S' ;
131100081126         chain moinrr    mk50s01    ;
131200081126         s01nrr=moinrr              ;
131300081126         errVAL=*on ;
131400081126         vscmsg = Msg(27);
131500081126         EXSR AggioSFL   ;
131600081126         leavesr;
131700081126 3     endif             ;
131800081126
131900081023       // Se logistica interessante per noi, obbligatorio outsourcing
132000081023       if LOGfsn='S' and outfsn=' '  and i50obl='S'  ;
132100081023         chain lognrr    mk50s01    ;
132200081023         s01nrr=lognrr              ;
132300081023         errFSN=*on ;
132400081023         vscmsg = Msg(26);
132500081023         EXSR AggioSFL   ;
132600081023         leavesr;
132700081023       endif             ;
132800081006       // Se presente outsourcing, obbligatori concorrenti logistica
132900081006       if outfsn='S' and log40=' '  and i50obl='S'  ;
133000081006         chain outnrr    mk50s01    ;
133100081006         s01nrr=outnrr              ;
133200081006         errFSN=*on ;
133300081006         vscmsg = Msg(15);
133400081006         EXSR AggioSFL   ;
133500081006         leavesr;
133600081006       endif             ;
133700140929
133800140929       //?Non affidato a BRT se affidato a BRT < 100% devo richiederlo obbligatorio
133900140929       //?quando il pgm è richiamo per info obbligatorie
134000140929        IF  I50obl = 'S' and BART_10 < 100 and
134100140929            noBRTpft = 0 and
134200140929            (savPRFval > 0 or savPROval > 0 or savAERval > 0 or
134300140929             savCAEval > 0 or savATRval > 0);
134400140929          chain ifoNRFnrr mk50S01;
134500140929          s01nrr = ifoNRFnrr;
134600140929          errPFT = *on;
134700140929          VSCmsg = Msg(38);
134800140929          EXSR AggioSFL;
134900140929          leavesr;
135000140929        ENDIF;
135100140721
135200140919       //?Se Non affidato a BRT impostato deve esserci la relativa % di ripartizione
135300140721       //?spesa traffico totale
135400140722        IF  savNRFpft > 0 and savPRFval = 0;
135500140721          chain ifoNRFnrr mk50S01;
135600140721          s01nrr = ifoNRFnrr;
135700140721          errPFT = *on;
135800140722          vscmsg = Msg(42);
135900140721          EXSR AggioSFL;
136000140721          leavesr;
136100140722        ENDIF;
136200140722        IF  savNRFpft > (SPTpft * savPRFval / 100);
136300140722          chain ifoNRFnrr mk50S01;
136400140722          s01nrr = ifoNRFnrr;
136500140722          errPFT = *on;
136600140722          vscmsg = Msg(41);
136700140722          EXSR AggioSFL;
136800140722          leavesr;
136900140722        ENDIF;
137000140722        IF  savNROpft > 0 and savPROval = 0;
137100140721          chain ifoNROnrr mk50S01;
137200140721          s01nrr = ifoNROnrr;
137300140721          errPFT = *on;
137400140722          vscmsg = Msg(42);
137500140721          EXSR AggioSFL;
137600140721          leavesr;
137700140722        ENDIF;
137800140722        IF  savNROpft > (SPTpft * savPROval / 100);
137900140722          chain ifoNROnrr mk50S01;
138000140722          s01nrr = ifoNROnrr;
138100140722          errPFT = *on;
138200140722          vscmsg = Msg(41);
138300140722          EXSR AggioSFL;
138400140722          leavesr;
138500140722        ENDIF;
138600140721         IF  savNERpft > 0 and savAERval = 0;
138700140721          chain ifoNERnrr mk50S01;
138800140721          s01nrr = ifoNERnrr;
138900140721          errPFT = *on;
139000140722          vscmsg = Msg(42);
139100140721          EXSR AggioSFL;
139200140721          leavesr;
139300140721         ENDIF;
139400140722         IF  savNERpft > (SPTpft * savAERval / 100);
139500140722          chain ifoNERnrr mk50S01;
139600140722          s01nrr = ifoNERnrr;
139700140722          errPFT = *on;
139800140722          vscmsg = Msg(41);
139900140722          EXSR AggioSFL;
140000140722          leavesr;
140100140722         ENDIF;
140200140721         IF  savNAEpft > 0 and savCAEval = 0;
140300140721          chain ifoNAEnrr mk50S01;
140400140721          s01nrr = ifoNAEnrr;
140500140721          errPFT = *on;
140600140722          vscmsg = Msg(42);
140700140721          EXSR AggioSFL;
140800140721          leavesr;
140900140721         ENDIF;
141000140722         IF  savNAEpft > (SPTpft * savCAEval / 100);
141100140722          chain ifoNAEnrr mk50S01;
141200140722          s01nrr = ifoNAEnrr;
141300140722          errPFT = *on;
141400140722          vscmsg = Msg(41);
141500140722          EXSR AggioSFL;
141600140722          leavesr;
141700140722         ENDIF;
141800140725         IF  savNTRpft > 0 and savATRval = 0;
141900140725          chain ifoNTRnrr mk50S01;
142000140725          s01nrr = ifoNAEnrr;
142100140725          errPFT = *on;
142200140725          vscmsg = Msg(42);
142300140725          EXSR AggioSFL;
142400140725          leavesr;
142500140725         ENDIF;
142600140725         IF  savNTRpft > (SPTpft * savATRval / 100);
142700140725          chain ifoNTRnrr mk50S01;
142800140725          s01nrr = ifoNTRnrr;
142900140725          errPFT = *on;
143000140725          vscmsg = Msg(41);
143100140725          EXSR AggioSFL;
143200140725          leavesr;
143300140725         ENDIF;
143400140718
143500140718       //?Se affidato a BRT è 100%  non devo indicare nessuna spesa per il mancato
143600140718       //?affidamento a BRT
143700140718        IF  bart_10 = 100 and noBRTpft > 0;
143800140718          chain ifoNRFnrr mk50S01;
143900140718          s01nrr = ifoNRFnrr;
144000140718          errPFT = *on;
144100140721          vscmsg = Msg(40);
144200140718          EXSR AggioSFL;
144300140718          leavesr;
144400140718        ENDIF;
144500140721
144600140721       //?Se affidato a BRT non è 100%
144700140919       //?Il non affidato a BRT deve essere congruente
144800140804        IF  bart_10 <> 100 and noBRTpft > 0 and
144900140725            noBRTpft <> (sptpft - siBRTpft);
145000140722          SELECT;
145100140722            WHEN  savPRFval > 0;
145200140722              chain ifoNRFnrr mk50S01;
145300140722              s01nrr = ifoNRFnrr;
145400140722            WHEN  savPROval > 0;
145500140722              chain ifoNROnrr mk50S01;
145600140722              s01nrr = ifoNROnrr;
145700140722            WHEN  savAERval > 0;
145800140722              chain ifoNERnrr mk50S01;
145900140722              s01nrr = ifoNERnrr;
146000140722            WHEN  savCAEval > 0;
146100140722              chain ifoNAEnrr mk50S01;
146200140722              s01nrr = ifoNAEnrr;
146300140725            WHEN  savATRval > 0;
146400140725              chain ifoNTRnrr mk50S01;
146500140725              s01nrr = ifoNTRnrr;
146600140722          ENDSL;
146700140721          errPFT = *on;
146800140721          vscmsg = Msg(40);
146900140721          EXSR AggioSFL;
147000140721          leavesr;
147100140721        ENDIF;
147200081006
147300080409       ENDSR;
147400080409
147500081006       //--------------------------------------------------------------
147600081006       //?cambiata categoria delle info conn- controllo e salvataggi
147700081006       //--------------------------------------------------------------
147800081006       BEGSR  CambioCAT    ;
147900140716
148000081006       if wctrprc='P'  and  i50obl='S' and totprc<100  ;
148100081006       s01nrr=s01nrr-1 ;
148200081006       chain s01nrr    mk50s01    ;
148300081006           errVal=*on;
148400081006           vscmsg = Msg(13);
148500081006           %subst(vscmsg:40:21)=wdescat   ;
148600081006           EXSR AggioSFL   ;
148700081006       endif              ;
148800081006
148900081006       // Pulisco e memorizzo nuovi dati per nuova categoria
149000081006        clear  totprc     ;
149100081215        wctrprc=vshobl  ;
149200081215        wdescat=vsdifo   ;
149300140716
149400081006        ENDSR   ;
149500080925       //--------------------------------------------------------------
149600080925       //?Visualizzo importi in base alla spesa trasporti
149700080925       //--------------------------------------------------------------
149800080925       BEGSR    VISUALimp  ;
149900081003       if vshsnf=' ' and vscpft=0 and vscval>0 and vsctva='%  '  ;
150000081215       PFTblu=*on     ;
150100081003       vscpft=(SPTpft*vscval)/100   ;
150200140717       //if vscpft>0    ;
150300140717       //vscvft='EUR'   ;
150400140717       //endif ;
150500140721       //?Se sto calcolando l'importo della % di affido a BRT me la salvo
150600140721         IF  vscifo = '10' and vscspp = '_BAR';
150700140721           siBRTpft = vscpft;
150800140721         ENDIF;
150900140721
151000080925       endif ;
151100080925
151200080925       ENDSR      ;
151300080703       //--------------------------------------------------------------
151400080703       //?Aggiornamento sfl
151500080703       //--------------------------------------------------------------
151600080703       BEGSR AGGIOSFL  ;
151700080703       if vscmsg<>*blanks   ;
151800080703       errMessage  = *on;
151900080703       errGenerico = *on;
152000080704       c01csr=s01nrr    ;
152100080703       endif    ;
152200080703
152300080703       exsr ProtCampi   ;
152400080703
152500080703       //  Ripristino dei flag di forzatura
152600081003       if vshsnf='X' and vscpft>0   ;
152700081003       vshsnf='S'   ;
152800080703       endif    ;
152900081003       if vshsns='X' and vscsna>0   ;
153000080703       vshsns='S'   ;
153100080703       endif    ;
153200081003       if vshsnv='X' and vscval>0  ;
153300081003       vshsnv='S'   ;
153400080703       endif    ;
153500140715       IF  vshsnvd = 'X' and vscvald <> 0;
153600140715         vshsnvd = 'S';
153700140715       ENDIF;
153800081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
153900081007       vshsne='S'   ;
154000081007       endif    ;
154100080703
154200110707       if vscatb='A' and vshann= 'X' ;
154300080704       vshann='S'  ;
154400080704       endif   ;
154500080703
154600080703       update  mk50s01  ;
154700080703       ENDSR   ;
154800080207       //--------------------------------------------------------------
154900080627       //?Inizializzazione SFL01
155000080207       //--------------------------------------------------------------
155100080409       BEGSR InzS01;
155200080207
155300080207       // Pulizia subfile
155400080207         SflDsp_N    = *on;
155500080207         SflDspCtl_N = *on;
155600080627         write  MK50C01;
155700080207         SflDspCtl_N = *off;
155800080207         SflEnd      = *off;
155900080530
156000080530         clear C01rcd;
156100100507         C01csr=1;
156200080627         S01nrr=0 ;
156300080702         clear Vscmsg;
156400080207         errMessage  = *off;
156500080207         errGenerico = *off;
156600080627         clear savcat  ;
156700080627         clear wcat    ;
156800080627         clear wifo    ;
156900081006         clear wIFS    ;
157000081006         clear sptpft  ;
157100100302         clear savspt  ;
157200100302         clear savatr  ;
157300140716
157400120109         wForzaAgg = *off;
157500120111         clear sav_bar_10;
157600140716         wForzaPft = *off;
157700140715
157800140715         errVAL=*off     ;
157900140715         errVALD = *off;
158000080409
158100080702       // Carico le info
158200080627       xx=1    ;
158300080627       Indx=1    ;
158400080702       dow   xx<=50    ;
158500080702       if    ifoor(xx)<>*blanks  ;
158600080627       clear  MK50S01;
158700080627
158800080627       Wcat= %subst(ifoor(xx):1:2)   ;
158900080702       Wifo= %subst(ifoor(xx):6:3)   ;
159000080627
159100080627       // cambio di categoria
159200080627        if savcat=*blanks or savcat<>wcat               ;
159300080627        savcat=wcat   ;
159400080627        Ifosott=*on   ;
159500080627
159600080627       // Verifico se devo visualizzare la titolo della categoria
159700080627         clear  DIFG  ;
159800080627         reset TIBS02ds;
159900080627         T02sif = knsif;
160000080627         T02cod = 'IFG';
160100080627         T02mod = 'C'  ;
160200080627         T02ke1 = wcat;
160300080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
160400080627         if t02err=*blanks ;
160500080627            difg=t02uni    ;
160600080627         endif            ;
160700080627
160800080627       // Se prevista la visualizzazione della categoria
160900080627       //  emetto la riga in videata,
161000080627       if §ifgvis='S'    ;
161100080703         vsdifo=§ifgdes         ;
161200080703         vsdifo=%trim(vsdifo)+':'    ;
161300080703         // In VSHIMM   metto una "C" per indicare che si tratta
161400080703         //    di riga di categoria
161500080703         vshimm='C'        ;
161600081007
161700081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
161800081003         vshobl=§ifgctpr   ;
161900081007
162000081007         EXSr ProtCampi    ;
162100081003
162200080627         s01nrr=s01nrr+1   ;
162300080627         write mk50s01     ;
162400080627         protriga=*off     ;
162500080627         Ifosott=*off      ;
162600081007         clear vscfsn      ;
162700100507         // Se la prima riga è un commento, posizionamento cursore sulla seconda
162800100507         if s01nrr=1   ;
162900100507          c01csr= 2 ;
163000100507         endif   ;
163100100507
163200081007       endif               ;
163300081007       endif               ;
163400080627
163500080627       Indx=%lookup(wifo:ifo)   ;
163600080627       if Indx>0                ;
163700080627         difo=ifods(Indx)         ;
163800080627         vscifo=ifo(Indx)       ;
163900080702
164000080702         // Dati da tabella
164100080702          vshobl=§ifoobl  ;
164200081015          vshie =§ifotip  ;
164300080704
164400080702          vshann=§ifoann  ;
164500080704
164600081003         // VALORE
164700081003          clear vshsnv  ;
164800081003          clear vsctva  ;
164900081003          if §ifosnv<>' '  ;
165000081003          vshsnv=§ifosnv  ;
165100081003          // tipo valore
165200081003          vsctva=§ifotva ;
165300080703          endif   ;
165400140715
165500140715         //?VALORE con DECIMALE
165600140715          IF  §ifosnvd <> *blanks;
165700140715            vshsnvd = §ifosnvd;
165800140715          // tipo valore
165900140715            vsctva = §ifotva;
166000140715          ENDIF;
166100080704
166200081003         // SPEDIZIONI
166300080702          vshsns=§ifosns  ;
166400080704
166500081003         // FATTURATO
166600080702          vshsnf=§ifosnf  ;
166700081003
166800081003         // SI/NO
166900081003          vshsnE=§ifosnE  ;
167000081003
167100080702          vshifgv=§ifgvis  ;
167200081007          clear vscint    ;
167300080627
167400080627         // Record senza sottotipo
167500081006         select    ;
167600081006         when §ifoifs=' '    ;
167700080702            vsdifo=§ifodes         ;
167800080703            if Ifosott=*on    ;
167900080703               vsdifo=%trim(vsdifo)+':'    ;
168000080703            endif    ;
168100080702            chain    (i50cpo:wifo) ticpi01l  ;
168200081126            if %found(ticpi01l)   ;
168300081126             trovacpi='S'    ;
168400081126             else ;
168500081126             trovacpi='N'    ;
168600081126             endif  ;
168700080702            EXSR CaricaInfo        ;
168800081006
168900081006         // Record con   sottotipo da caricare tutti
169000081006         when   §ifoifs='S' and §ifoifv='S';
169100080627            EXSR CARCon    ;
169200081006
169300081006         // Record con   sottotipo da caricare col '?'
169400081006         //   la categoria visualizzata in questo caso è obbligatoria
169500081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
169600081006         //  il '?'
169700081006         when   §ifoifs='S' and §ifoifv='?';
169800081006          vshifgv=§ifoifv  ;
169900081215          yy=1        ;
170000081126
170100081126          if §ifonrec=0 ;
170200081126             §ifonrec=1  ;
170300081126          endif     ;
170400081126
170500081126          setll (i50cpo:wifo) ticpi01l  ;
170600081126          dow yy<=§ifonrec  ;
170700081006            EXSR CarconUNA            ;
170800081126            yy=yy+1  ;
170900081126          enddo    ;
171000081126
171100081006         endsl ;
171200080703
171300080703         endif ;
171400080702         endif ;
171500080702
171600080702         xx=xx+1   ;
171700080702         enddo     ;
171800080702         ENDSR    ;
171900080627
172000080702       //--------------------------------------------------------------
172100081006       //?Carico con   sottotipo  carica riga per riga
172200080702       //--------------------------------------------------------------
172300080702       BEGSR  CARcon            ;
172400080702
172500081126         tbeke1=wifo   ;
172600080925
172700081006         setll ('IFS':tbeke1) tntbe01l   ;
172800081006         READE ('IFS':tbeke1) tntbe01l   ;
172900080702
173000080702         dow not %eof(tntbe01l)   ;
173100080702         if tbeatb=' '    ;
173200081006         difs=tbeuni      ;
173300081006         wifs=tbeke2      ;
173400080702         vscspp=tbeke2      ;
173500081127         vshspp=tbeke2      ;
173600081007         vsdifo=§ifsdes  ;
173700110707         vshann=§ifsann  ;
173800080702
173900081006         chain    (i50cpo:wifo:wifs) ticpi01l  ;
174000081126            if %found(ticpi01l)   ;
174100081126             trovacpi='S'    ;
174200081126             else ;
174300081126             trovacpi='N'    ;
174400081126             endif  ;
174500080702         EXSR CaricaInfo    ;
174600080925         endif   ;
174700080702
174800081006         READE ('IFS':tbeke1) tntbe01l   ;
174900080702         enddo   ;
175000080702
175100080702       ENDSR;
175200081006       //--------------------------------------------------------------
175300081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
175400081006       //--------------------------------------------------------------
175500081006       BEGSR  CARconUNA         ;
175600081007
175700081007       vscint='?'    ;
175800081007
175900081126       reade    (i50cpo:wifo) ticpi01l  ;
176000081006
176100081126 1     if not %eof(ticpi01l)   ;
176200081126         trovacpi='S'    ;
176300081126
176400081006         vscspp=cpispf   ;
176500081127         vshspp=cpispf   ;
176600081006
176700081126         tbeke1=wifo    ;
176800081126
176900081006         tbeke2=cpispf  ;
177000081006         chain ('IFS':tbeke1:tbeke2) tntbe01l   ;
177100081006 2       if not %found(tntbe01l)    ;
177200081006         clear tbeuni       ;
177300081006 2       endif    ;
177400081007         difs=tbeuni     ;
177500081006
177600081007         vsdifo=§ifsdes   ;
177700081007         vsdifs=§ifsdel   ;
177800081007
177900081006         EXSR CaricaINFO  ;
178000081006
178100081006 1x      else    ;
178200081126         trovacpi='N'    ;
178300081006
178400081126         // se non trovato dicitura generica da tabella IFO
178500081126         if wifo='MOI'  ;
178600081126           vsdifo=coRicercaMOI    ;
178700081126           else  ;
178800081126           vsdifo=coRicerca       ;
178900081126         endif  ;
179000081007         clear vsdifs      ;
179100081126         clear vscspp      ;
179200081127         clear vshspp      ;
179300081006
179400081006         EXSR CaricaINFO   ;
179500081006         endif             ;
179600081006
179700081006       ENDSR;
179800080702       //--------------------------------------------------------------
179900081006       //?Carica ogni riga di info commerciale
180000080702       //--------------------------------------------------------------
180100080702         BEGSR CaricaINFO             ;
180200080925         clear pftblu   ;
180300080925
180400081126         if trovacpi='S'                             ;
180500080703          vshimm=' '        ;
180600080702          vscfsn=cpifsn   ;
180700081003          vscval=cpival   ;
180800081007          if vshifgv='?'   and vscspp>*zeros    ;
180900081007            w0040=%int(vscspp)  ;
181000081007            vscval=w0040     ;
181100081007            clear vscfsn     ;
181200081007          endif            ;
181300080702          vscsna=cpisna   ;
181400080702          vscpft=cpipft   ;
181500080925          if vscpft>0     ;
181600080925          vshprpft='S'    ;
181700080925          endif           ;
181800140717          //vscvft=cpivft   ;
181900080702          Dataiso=%date(cpidim:*iso)  ;
182000080702          datadmy=Dataiso ;
182100080702          vscdim=%dec(datadmy)  ;
182200081003          clear vscfil  ;
182300081003          if cpifil>0   ;
182400081003          vscfil=%editc(cpifil:'X')   ;
182500081003          endif         ;
182600081003
182700081003          if cpipru<>*blanks ;
182800080702          vscpru=cpipru   ;
182900081003          vscdpru='Utente' ;
183000081003          else  ;
183100081003          clear vscpru   ;
183200081003          clear vscdpru   ;
183300081003          endif     ;
183400080925
183500080925          exsr ViSUALimp   ;
183600140715
183700140715         //?Calcolo il ricavo medio spedizione 'RMS'
183800140715           IF  VSCifo = 'SPT' and VSCpft <> 0 and VSCsna <> 0;
183900140715             wVALrms = VSCpft / VSCsna;
184000140929             IF  wVALrms > 999;
184100140929               wVALrms = 999;
184200140715             ENDIF;
184300140715           ENDIF;
184400080925
184500080702         else             ;
184600080703          vshimm='S'      ;
184700080703          Protann=*on     ;
184800080702          clear vscfsn    ;
184900081003          clear vscval    ;
185000080702          clear vscsna    ;
185100080702          clear vscpft    ;
185200140717          //clear vscvft    ;
185300080702          clear vscdim    ;
185400080702          clear vscfil    ;
185500080702          clear vscpru    ;
185600081003          clear vscdpru    ;
185700080925          clear vshprpft  ;
185800140715
185900140715         //?Se info 'RMS' imposto il dato
186000140715           IF  VSCifo = 'RMS';
186100140715             VSCvald = wVALrms;
186200140715           ENDIF;
186300140715
186400080702         endif            ;
186500081006
186600080925         exsr ProtCAMPI    ;
186700080703
186800080703         // I record da annullare li visualizzo solo se ci sono
186900110707           if vshann=' ' or (vshann<>' '  and vscfsn<>' ') or
187000110707                             (vshann<>' '  and vscpft>0)   or
187100110707                             (vshann<>' '  and vscval>0   )  ;
187200081003
187300081003         // In interrogazione visulizzo solo i record pieni col sottotipo
187400081003           if i50mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
187500080703            s01nrr=s01nrr+1   ;
187600080703            write mk50s01     ;
187700140718
187800140919         // Mi salvo i nr di rcd del non affidato a BRT
187900140721            IF  VSCifo = 'NRF' and ifoNRFnrr = 0;
188000140718              ifoNRFnrr = S01nrr;
188100140721            ENDIF;
188200140721            IF  VSCifo = 'NRO' and ifoNROnrr = 0;
188300140721              ifoNROnrr = S01nrr;
188400140721            ENDIF;
188500140721            IF  VSCifo = 'NER' and ifoNERnrr = 0;
188600140721              ifoNERnrr = S01nrr;
188700140721            ENDIF;
188800140721            IF  VSCifo = 'NAE' and ifoNAEnrr = 0;
188900140721              ifoNAEnrr = S01nrr;
189000140721            ENDIF;
189100140725            IF  VSCifo = 'NTR' and ifoNTRnrr = 0;
189200140725              ifoNTRnrr = S01nrr;
189300140725            ENDIF;
189400080925
189500080925         // Se info SPT - totale spesa trasporti,  la memorizzo
189600100302         //  per il calcolo delle %  e per vedere se modificata
189700080925         if vscifo='SPT' ;
189800080925          SPTPFT=vscpft   ;
189900100302          savSPT=vscpft   ;
190000080925         endif           ;
190100100302         //  per vedere se modificata
190200100302         if vscifo='ATR' ;
190300100302          savATR=vscval   ;
190400100302         endif           ;
190500120105
190600120105         //?Memorizzo la % di affidamento a BRT
190700120110           IF  VSCifo = '10' and VSCspp = '_BAR';
190800120105           //?Se devo aumentarla calcolo almeno 1% in +
190900120110             IF  wAumenta10 and VSCval > 0;
191000120111               sav_bar_10 = VSCval + 1;
191100120105             ENDIF;
191200120510             IF  (wAumenta10 or wAumenta10f) and VSCval = 0;
191300120110               sav_bar_10 = 1;
191400120110             ENDIF;
191500120105           //?Se devo dominuirla calcolo almeno 1% in -
191600120110             IF  wDiminuisci10 and VSCval > 0;
191700120111               sav_bar_10 = VSCval - 1;
191800120105             ENDIF;
191900120105           ENDIF;
192000080925
192100080703           endif ;
192200081003           endif ;
192300081003
192400080702       ENDSR;
192500080702       //--------------------------------------------------------------
192600080702       //?Gestione protezione campi del sfl
192700080702       //--------------------------------------------------------------
192800080702          BEGSR   ProtCampi   ;
192900080703           protfat=*off    ;
193000081003           protVal =*off   ;
193100140715           protVald = *off;
193200140715           protVala = *off;
193300080703           protsped=*off   ;
193400080703           protfsn =*off   ;
193500080703           Ifosott=*off    ;
193600080703           Protann=*off ;
193700080704           Infoyell=*off ;
193800080703
193900081007           // Se riga di categoria accendo solo protezione e sottolineatura
194000081007          if vshimm='C'    ;
194100081007           Ifosott=*on     ;
194200081007           Protriga=*on    ;
194300081007          else             ;
194400081007
194500080702          // in base a cosa prevede la tabella, proteggo i campi
194600080702          if vshifgv='N' ;
194700080702            Ifosott=*on       ;
194800080702          endif    ;
194900080702
195000080704          if vshann<>' '  ;
195100080702           protfat=*on     ;
195200081003           protVal =*on    ;
195300140715           protVald = *on;
195400140715           protVala = *on;
195500080702           protsped=*on    ;
195600080702           protfsn =*on    ;
195700080704           infoyell=*on    ;
195800080702          else            ;
195900081006          protann=*on     ;
196000081031
196100081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
196200081103          if vscpft>0 and vshsnf=' ' and vscval=0;
196300081031          protann=*off    ;
196400081031          endif    ;
196500081031          if vscsna>0 and vshsns=' ' ;
196600081031          protann=*off    ;
196700081031          endif    ;
196800081127          if vscval>0 and vshsnv=' ' and vshifgv<>'?'       ;
196900081031          protann=*off    ;
197000081031          endif    ;
197100140715          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
197200140715            protann = *off;
197300140715          ENDIF;
197400081031          if vscfsn<>' ' and vshsne=' ' ;
197500081031          protann=*off    ;
197600081031          endif    ;
197700081006
197800080703            if vshsnf=' ' ;
197900080702             protfat=*on     ;
198000080702            endif           ;
198100081003            if vshsnv=' ' ;
198200081003             protVal =*on     ;
198300080702            endif           ;
198400140715            IF  vshsnvd = *blanks;
198500140715              protVald = *on;
198600140715            ENDIF;
198700140715            if vshsnvd='A';
198800140715             protVala=*on     ;
198900140715            endif           ;
199000080703            if vshsns=' ' ;
199100080702             protsped=*on     ;
199200080702            endif           ;
199300081003            if vshsne=' ' ;
199400081003             protfsn =*on     ;
199500081003            endif           ;
199600080702          endif           ;
199700080703          // Se record di nuova immissione
199800081003          //  proteggo annullamento
199900080703            if vshimm='S'  ;
200000080703             Protann=*on  ;
200100080703           endif   ;
200200081006
200300081006          // Se prevista scelta sottotipo col'?' visualizzo valore
200400081006          //   col codice e vicino il '?'
200500081006          if vshifgv='?'    ;
200600081006             protVal =*off    ;
200700081007             if vscspp>*zeros    ;
200800081007             else          ;
200900081007             infoyell=*on    ;
201000081007             endif            ;
201100081006          endif               ;
201200081007          endif               ;
201300080703
201400080702          ENDSR    ;
201500080702       //--------------------------------------------------------------
201600080702       //?conferma aggiornamento
201700080702       //--------------------------------------------------------------
201800080702        BEGSR ConfAGGIO              ;
201900090520        clear ultdata   ;
202000081003
202100080704       s01nrr=1  ;
202200080704       chain s01nrr    mk50s01    ;
202300080704
202400080704 1     dow %found    ;
202500080704       // Escludo i record di descrizione categoria
202600140715 2     //if vshimm<>'C'    ;
202700140715 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
202800080925
202900080925       // se valori calcolati da pgm li pulisco prima dei controlli
203000080925       if vshsnf=' ' and vshprpft=' '   ;
203100080925       clear vscpft   ;
203200140717       //clear vscvft   ;
203300080925       endif          ;
203400080925
203500081127       IF VShifgv<>'?'    ;
203600081127         chain    (i50cpo:vscifo:vscspp) ticpi01l  ;
203700081127       else   ;
203800081127         chain    (i50cpo:vscifo:vshspp) ticpi01l  ;
203900081127       endif   ;
204000081127             if %found(ticpi01l)   ;
204100081127             trovacpi='S'    ;
204200081127             else ;
204300081127             trovacpi='N'    ;
204400081127             endif  ;
204500081127
204600081127       // A n n u l l a m e n t o oppure per qualche strano motivo
204700081127       //   la info c'e'  ma a video non è stata inserita
204800080925
204900081127 3     if trovacpi='S'      and (vscatb='A'  or (vscfsn=' ' AND
205000081003                                 vscpft=0 and vscsna=0 and vscval=0));
205100081003         delete ticpi000  ;
205200081003 x3    else    ;
205300080704
205400081127       // V a r i a z i o n e :  verifico se modificati dei dati
205500081127 31    if trovacpi='S'     and vscatb=' '   ;
205600080704 4     if (cpifsn<>vscfsn) or
205700081003          (cpival<>vscval) or
205800080704          (cpisna<>vscsna) or
205900081007          (cpispf<>vscspp) or
206000140717          (cpipft<>vscpft) ;
206100140717          //(cpivft<>vscvft) ;
206200080704          // reimposto data ora profilo immissione
206300080704          cpidim=*date    ;
206400090520          ultdata=*date    ;
206500080704          cpifil=utefil   ;
206600080704          cpipru=knmus    ;
206700080704
206800081007          cpifsn= vscfsn  ;
206900081007          IF VShifgv<>'?';
207000081003          cpival= vscval  ;
207100081003          cpitva= vsctva  ;
207200081007          else            ;
207300081007          cpispf=vscspp   ;
207400081007          if cpispf<>*blanks ;
207500081007          cpifsn='S'      ;
207600081007          ENDIF           ;
207700081007          ENDIF           ;
207800080704          cpisna= vscsna  ;
207900080704          cpipft= vscpft  ;
208000140717          cpivft= 'EUR'   ;
208100081006          // verifico se dei dati non ci voglio più: una volta aggiunti
208200080704          //  quelli previsti, lo tolgo
208300080704
208400081003 5        if (cpival>0 and vshsnv=' ')    ;
208500081006          if (cpisna>0 and vshsns<>' ')  or (cpipft>0  and vshsnf<>' ') or
208600081006             (cpifsn<>' ' and vshsne<>' ')   ;
208700081003          clear cpival   ;
208800080704          endif          ;
208900080704 5        endif          ;
209000080704 5        if (cpisna>0 and vshsns=' ')    ;
209100081006          if (cpival>0 and vshsnv<>' ')  or (cpipft>0  and vshsnf<>' ') or
209200081006             (cpifsn<>' ' and vshsne<>' ')   ;
209300080704          clear cpisna   ;
209400080704          endif          ;
209500080704 5        endif          ;
209600080704 5        if (cpipft>0 and vshsnf=' ')    ;
209700081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
209800081006             (cpifsn<>' ' and vshsne<>' ')   ;
209900080704          clear cpipft   ;
210000080704          clear cpivft   ;
210100080704          endif          ;
210200080704 5        endif          ;
210300081006 5        if (cpifsn<>' ' and vshsne=' ')    ;
210400081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
210500081006             (cpipft>0  and vshsnf<>' ')     ;
210600081006          clear cpifsn   ;
210700081006          endif          ;
210800081006 5        endif          ;
210900080704
211000080704        update ticpi000  ;
211100080704 4      endif  ;
211200081003 3a     endif  ;
211300080704
211400081127       // I m m i s s i o n e
211500081127 3     if trovacpi='N'     and vscatb=' ' and (vscfsn<>' ' or
211600081003       vscpft>0 or vscsna>0 or vscval>0) ;
211700080704       clear ticpi000;
211800080704        cpicpo=i50cpo   ;
211900080704        cpitpf=vscifo  ;
212000080704        cpispf=vscspp  ;
212100080704        cpifsn=vscfsn ;
212200081007        if vshifgv<>'?';
212300081003        cpival= vscval  ;
212400081003        cpitva= vsctva  ;
212500081007        else            ;
212600081007          if cpispf<>*blanks ;
212700081007          cpifsn='S'      ;
212800081007          endif           ;
212900081007        endif           ;
213000081007
213100080704        cpisna= vscsna  ;
213200080704        cpipft= vscpft  ;
213300140717        cpivft= 'EUR'   ;
213400080704        cpidim=*date    ;
213500090520        ultdata=*date    ;
213600080704        cpifil=utefil   ;
213700080704        cpipru=knmus    ;
213800080704
213900080704        write ticpi000 ;
214000081003  3a    endif           ;
214100081003  3     endif           ;
214200080704  2     endif           ;
214300080704
214400080704        s01nrr=s01nrr+1 ;
214500080704        chain s01nrr    mk50s01    ;
214600080704  1     enddo           ;
214700140929
214800140929       //?Se affidato a BRT è 0
214900140929       //?Se ripartizione della spesa è 100%
215000140929       //?Se non affidato a BRT non è presente
215100140929       //?creo in automatico il NON affidato a BRT
215200140929         IF  BART_10 = 0 and
215300140929             (savPRFval + savPROval + savAERval +
215400140929              savCAEval + savATRval) = 100 and
215500140929             noBRTpft = 0;
215600140929           exsr NoAffidatoBRT;
215700140929         ENDIF;
215800080704
215900081015        clear waggiocpo    ;
216000081015
216100081003       // Aggancio tncpo per aggiornare se ho tutte le info
216200081003       chain   i50cpo   tncpo01l   ;
216300081015  1      if %found(tncpo01l)    ;
216400081015         chain  cp1nrr    tncpo00f   ;
216500081015  1      endif                  ;
216600081003  1      if %found(tncpo00f)   ;
216700081015
216800081015         // Se inserimento obbligatorio--> posso impostare la "S"
216900081015         //  altrimenti controllo da CPI
217000081003
217100081015  2       if i50obl=' '   ;
217200081015            // Rifaccio il controllo del sfl come se fosse
217300081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
217400081215            savmod=i50mod   ;
217500081215            i50mod='C'      ;
217600081215            i50obl='S' ;
217700081215
217800081015            exsr contrs01   ;
217900081215
218000081215            i50obl=' ' ;
218100081215            i50mod=savmod   ;
218200081015
218300081015  3         if ErrGenerico=*off;
218400081215              Waggiocpo='S';
218500081015  3         endif  ;
218600081015
218700081015  x2      else;
218800081015          Waggiocpo='S'   ;
218900081015  2       endif           ;
219000081015
219100081015              dcpo01=cporst    ;
219200081015              §cpoifotot=Waggiocpo   ;
219300081015              o50ifotot=Waggiocpo   ;
219400090520              if ultdata > 0   ;
219500090520                §cpoifodul=%editc(ultdata:'X')  ;
219600090520              endif   ;
219700090520              // Se inserita memorizzo la spesa trasporti reale
219800090520              if sptpft>0   ;
219900090520              §cposptp=sptpft   ;
220000090520              endif   ;
220100100302
220200100302              //  Se variata spesa trasporti o % ATR, ricalcolo cod importanza potenziale
220300100302             if savSPT<> SPTPFT or savatr<>infoATR   ;
220400100302             clear trmk52ds  ;
220500100302             i52cpo=cpocpo  ;
220600100302             i52spt= SPTpfT ;
220700100302             i52ATR=infoATR  ;
220800100302
220900100302             callp TRMK52R  (trmk52ds)  ;
221000100302             cpoftr=o52cimp  ;
221100100302             endif   ;
221200100302
221300081015              cporst=dcpo01    ;
221400081015              update tncpo000  ;
221500081003  1    endif   ;
221600140919
221700140725       // devo aggiornare la data ultima conferma INFO COMM.
221800140909         IF  not InfoInt;
221900140725           chain i50cpo TNCPO11L;
222000140725  1        IF  %found(TNCPO11L);
222100140909             IF  ultdata > 0;
222200140909               CPO1ducifo = ultdata;
222300140909             ELSE;
222400140909               CPO1ducifo = *date;
222500140909             ENDIF;
222600140725             update TNCPO100;
222700140725           ENDIF;
222800140909           IF  not %found(TNCPO11L);
222900140909             clear TNCPO100;
223000140909             CPO1cpo = CPOcpo;
223100140909             IF  ultdata > 0;
223200140909               CPO1ducifo = ultdata;
223300140909             ELSE;
223400140909               CPO1ducifo = *date;
223500140909             ENDIF;
223600140909             write TNCPO100;
223700140909           ENDIF;
223800140725         ENDIF;
223900140725
224000080702        ENDSR;
224100140919
224200140919       //--------------------------------------------------------------
224300140919       //?Creo le info di NON Affidato a BRT.
224400140919       //--------------------------------------------------------------
224500140919        BEGSR NoAffidatoBRT;
224600140919
224700140919          clear TICPI000;
224800140919          CPIcpo = I50CPO;
224900140919          CPIvft = 'EUR';
225000140919          CPIdim = *date;
225100140919          CPIfil = utefil;
225200140919          CPIpru = knmus;
225300140919
225400140919          IF  savPRFval > 0 and savNRFpft = 0;
225500140919            CPItpf = 'NRF';
225600140919            CPIpft = (SPTpft * savPRFval / 100);
225700140919            write TICPI000;
225800140919          ENDIF;
225900140919
226000140919          IF  savPROval > 0 and savNROpft = 0;
226100140919            CPItpf = 'NRO';
226200140919            CPIpft = (SPTpft * savPROval / 100);
226300140919            write TICPI000;
226400140919          ENDIF;
226500140919
226600140919          IF  savAERval > 0 and savNERpft = 0;
226700140919            CPItpf = 'NER';
226800140919            CPIpft = (SPTpft * savAERval / 100);
226900140919            write TICPI000;
227000140919          ENDIF;
227100140919
227200140919          IF  savCAEval > 0 and savNAEpft = 0;
227300140919            CPItpf = 'NAE';
227400140919            CPIpft = (SPTpft * savCAEval / 100);
227500140919            write TICPI000;
227600140919          ENDIF;
227700140919
227800140919          IF  savATRval > 0 and savNTRpft = 0;
227900140919            CPItpf = 'NTR';
228000140919            CPIpft = (SPTpft * savATRval / 100);
228100140919            write TICPI000;
228200140919          ENDIF;
228300140919
228400140919        ENDSR;
228500080702
228600080206       //--------------------------------------------------------------
228700080206       //?Operazioni finali.
228800080206       //--------------------------------------------------------------
228900080206       BEGSR RoutEnd;
229000080627
229100080704         if i50tla<>' '    ;
229200080704         // Chiusura pgm   ;
229300080704         clear tibs02ds  ;
229400080704         t02tla='C'      ;
229500080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
229600080704
229700080206         *inLR = *on;
229800080704         endif             ;
229900080704
230000080206         return;
230100080206
230200080206       ENDSR;
230300080206
230400080206      /end-free
230500080206
230600080206       //--------------------------------------------------------------
230700080206       //?Schiere a tempo di compilazione.
230800080206       //--------------------------------------------------------------
230900080206
231000080410** - MSG ------------------------------------------------------------------ -*
231100081003Info Commerciale obbligatoria                                                   01
231200080703Per Info Comm. non presente (N), non indicare gli altri dati                   02
231300081003La Info Comm. prevede l'inserimento del Fatturato presunto OBBLIGATORIO        03
231400081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx: enter per forzare     04
231500081003La  Info Comm. prevede l'inserimento del Fatturato presunto: enter per forzare    05
231600080704La  Info Comm. prevede l'inserimento del numero Spedizioni: enter per forzare  06
231700080704Info Comm. non più in essere:verificarla e se non serve annullarla.ENTER forza 07
231800080704Info Comm. non più in essere: ANNULLARLA !!                                    08
231900081003La somma delle percentuali per questa categoria di info commerc. supera 100%   09
232000081003La Info Comm. prevede l'inserimento del numero Spedizioni OBBLIGATORIO         10
232100081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx OBBLIGATORIO            11
232200081003La  percentuale di questa info commerciale supera 100%                         12
232300081003La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx  deve essere 100% 13
232400081007Inserire concorreti Logistica SOLO SE il cliente prevede Logist.in outsourcing 14
232500081007Se Logistica in outsourcing obbligatorio inserire i concorrenti logistica      15
232600110511"Motivo non affidamento a noi" da immettere solo se "Affidato a BRT" < 100%    16
232700140716ATTENZIONE!! Il valore massimo consentito è di 999 milioni                     17
232800081007"MOTIVO NON AFFIDAMENTO A NOI" errato                                          18
232900081007Indicare OBBLIGATORIAMENTE la presenza (S) o assenza(N) della Info commerciale 19
233000081007Immettere solo  '?'  se si vuole attivare la ricerca                           20
233100081125Immettere "S o N" per indicare la presenza o l'assenza della Info Comm.        21
233200081007Immettere"S o N"per indicare la presenza o l'assenza della Info Comm:ENTR forza22
233300140717ATTENZIONE!! Il valore inserito e > di 1 milione - Enter per forzare.          23
233400140716Il numero delle spedizioni non può essere superiore alla spesa trasporti.      24
233500140716Non ammesso un valore negativo                                                 25
233600081125Se "Logistica interessante per noi", indicare se in Outsourcing                26
233700110511Se non tutto "Affidato BRT" obbligatorio un Motivo NON AFFIDAMENTO a noi       27
233800081126I"Motivi non Affidamento a noi" devono essere diversi: uno per ITA uno per EST 28
233900081126Se immesso"9999-NON Rilevabile", l'altro motivo deve essere vuoto              29
234000081126Codice già presente !!                                                         30
234100120125Obbligatorio aumentare la % di affidamento a BRT!!                             31
234200120109Alcune Info sono più vecchie di 1 anno. Verificare se ancora corrette.         32
234300120125Obbligatorio azzerare la % di affidamento a BRT!!                              33
234400120125Obbligatorio diminiuire la % di affidamento a BRT!!                            34
234500140715Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                 35
234600140715Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!                36
234700140725Inserire Paesi Esteri SOLO SE il cliente prevede trasporto Estero.             37
234800140922Se non tutto "Affidato BRT" obbligatorio ripartizione spesa NON affidato a BRT 38
234900140717ATTENZIONE!! Il valore inserito e > di 10.000.                                 39
235000140919Non affidato a BRT non congruente con % utilizzo concorrenti                   40
235100140919Non affidato a BRT non congruente con % ripartizione spesa trasporti           41
235200140919% Ripartizione Spesa Trasp. a 0. Non impostare il relativo non affidato a BRT  42
