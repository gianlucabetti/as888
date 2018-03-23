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
015900080627
016000080206
016100080627      // - Tabella  Info commerciali e Categorie info
016200080627     d dIFO          e ds                  inz
016300080627     d dIFG          e ds                  inz
016400081003     d
016500081003     d dcpo01        e ds                  inz
016600140725
016700080206      //---------------------------------------------------------------
016800080206      //?Definizione variabili globali.
016900080206      //---------------------------------------------------------------
017000080206
017100080206      // - Flags booleani
017200080208     d $Fine           s               n   inz(*off)
017300080208     d $InzS01         s               n   inz(*on)
017400120105     d wAumenta10      s               n   inz(*off)
017500120510     d wAumenta10f     s               n   inz(*off)
017600120110     d wAzzera10       s               n   inz(*off)
017700120105     d wDiminuisci10   s               n   inz(*off)
017800120109     d wForzaAgg       s               n   inz(*off)
017900140716     d wForzaRms       s               n   inz(*off)
018000140716     d wForzaPft       s               n   inz(*off)
018100140725     d wEstero         s               n   inz(*off)
018200140721     d wNews           s               n   inz(*off)
018300080206
018400080627     d $Video          s              2    inz('S1')
018500080208     d S01nrr          s              4  0 inz
018600080627     d savcat          s              2    inz
018700080627     d Wcat            s              2    inz
018800081006     d WIFS            s              4    inz
018900080702     d Wifo            s              3    inz
019000081006     d w0040           s              4  0 inz
019100081007     d w005a           s              5    inz
019200081006     d XX              s              3  0 inz
019300080627     d Indx            s              3  0 inz
019400080703     d Primavolta      s              1    inz
019500080925     d Totprc          s              4  0 inz
019600081003     d Wctrprc         s              1    inz
019700081003     d Wdescat         s             21    inz
019800081215     d SPTPFT          s                   like(cpipft) inz
019900100302     d savspt          s                   like(cpipft) inz
020000081006     d LOG40           s              1    inz
020100081006     d OUTFSN          s              1    inz
020200081006     d OUTnrr          s              4  0 inz
020300081023     d LOGFSN          s              1    inz
020400081023     d LOGnrr          s              4  0 inz
020500081126     d MOInrr          s              4  0 inz
020600081015     d Waggiocpo       s              1    inz
020700081007     d BART_10         s              4  0 inz
020800081015     d InfoITA         s              1    inz
020900081015     d InfoEST         s              1    inz
021000100226     d InfoATR         s                   like(cpival)
021100100302     d SavATR          s                   like(cpival)
021200081126     d w008a           s              8    inz
021300081126     d MOIconta        s              3  0 inz
021400081126     d yy              s              3  0 inz
021500081126     d trovacpi        s              1    inz
021600081215     d savmod          s                   like(i50mod) inz
021700090520     d ultdata         s              8  0 inz
021800120110     d sav_bar_10      s                   like(bart_10)
021900120109     d wOggi           s              8  0 inz
022000140721
022100140715     d sav_vscpft      s                   like(vscpft) inz
022200140715     d sav_vscsna      s                   like(vscsna) inz
022300140716     d sav_valrms      s             12  3 inz
022400140716     d wVALrms         s             12  3 inz
022500140718     d noBRTpft        s                   like(cpipft) inz
022600140721     d siBRTpft        s                   like(cpipft) inz
022700140718     d ifoNRFnrr       s                   like(S01nrr) inz
022800140721     d ifoNROnrr       s                   like(S01nrr) inz
022900140721     d ifoNERnrr       s                   like(S01nrr) inz
023000140721     d ifoNAEnrr       s                   like(S01nrr) inz
023100140725     d ifoNTRnrr       s                   like(S01nrr) inz
023200140721     d savPRFval       s                   like(cpival) inz
023300140721     d savPROval       s                   like(cpival) inz
023400140721     d savAERval       s                   like(cpival) inz
023500140721     d savCAEval       s                   like(cpival) inz
023600140725     d savATRval       s                   like(cpival) inz
023700140721     d savNRFpft       s                   like(cpipft) inz
023800140721     d savNROpft       s                   like(cpipft) inz
023900140721     d savNERpft       s                   like(cpipft) inz
024000140721     d savNAEpft       s                   like(cpipft) inz
024100140725     d savNTRpft       s                   like(cpipft) inz
024200080414
024300080605     d Dataiso         s               d   datfmt(*iso)
024400080605     d Dataymd         s               d   datfmt(*ymd)
024500080605     d Datadmy         s               d   datfmt(*dmy)
024600120109     d wIFOdul         s              8  0
024700120109     d wIFOduliso      s               d   datfmt(*ISO)
024800140725     d wData           s              8  0
024900140725     d Dataeur         s               d   datfmt(*eur)
025000080604     d
025100080605     d                 DS
025200080605     d  AA                     1      4  0
025300080605     d  MM                     5      6  0
025400080605     d  GG                     7      8  0
025500080605     d DATA                    1      8  0
025600081009     d
025700081126     d CoRicercaMOI    c                   const('Ricerca motivo ITA/EST''?''')
025800081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
025900080206
026000080206      //---------------------------------------------------------------
026100080206      //?Definizione procedure usate.
026200080206      //---------------------------------------------------------------
026300080414      /copy gaitrasrc/srcprotopr,tibs02r
026400080414      /copy gaitrasrc/srcprotopr,tibs34r
026500081008      /copy gaitrasrc/srcprotopr,TNTB70r
026600081023      /copy gaitrasrc/srcprotopr,TRMK51r
026700100302      /copy gaitrasrc/srcprotopr,TRMK52r
026800080414      /copy gaitrasrc/srcprotopr,trmk_opm
026900080206
027000080206      //---------------------------------------------------------------
027100080206      //?Riepilogo indicatori.
027200080206      //---------------------------------------------------------------
027300080207      // - Comuni
027400080207      // 28    : Emissione messaggio di errore a video
027500080207      // - C01/S01
027600080207      // 30    : Condiziona SFLDSP    (*not)
027700080207      // 31    : Condiziona SFLDSPCTL (*not)
027800080207      // 30+31 : Condiziona SFLCLR
027900080207      // 32    : Condiziona SFLNXTCHG in S01
028000080207      // 50    : Errore: Opzione errata
028100080207      // - D01
028200080207      // - Comuni
028300080207      // 99    : Generico di Errore
028400080206      //---------------------------------------------------------------
028500080206
028600080206      //---------------------------------------------------------------
028700080206      //?M A I N - L I N E
028800080206      //---------------------------------------------------------------
028900080206
029000080627     c     *Entry        plist
029100080206     c                   parm                    KPJBA
029200080627     C                   PARM                    trMK50ds
029300080627
029400080627     C                   CLEAR                   O50F12
029500080627     C                   CLEAR                   O50F03
029600080627     C                   CLEAR                   O50F06
029700080702
029800080206      /free
029900080206
030000081215 1      if  i50tla<>'C'   ;
030100080925
030200080206       // Operazioni iniziali
030300080206       exsr RoutInz;
030400080206
030500081215 2      if  i50mod<>'C'   ;
030600081215
030700080206       // Gestione video
030800081215 3     DOW $Fine = *off;
030900081215 4       select;
031000080530
031100080530         // Videata di selezioni
031200080627           when $Video = 'S1';
031300080627             exsr GesS01;
031400120109           when $Video = 'W1';
031500120109             exsr GesW01;
031600080530
031700080702           other   ;
031800080206             $Fine = *on;
031900081215 4       endsl;
032000081215 3     ENDDO;
032100081215
032200081215 x2    else   ;
032300081215
032400081215       // modalità CONTROLLO : carico il sfl
032500081215       //                    solo ai fini del controllo
032600081215       exsr InzS01;
032700081215       exsr Contrs01   ;
032800081215
032900081215  3    if ErrGenerico=*off;
033000081215          o50ifotot='S';
033100081215  3    endif  ;
033200080925
033300081215 2     endif    ;
033400081215 1     endif    ;
033500080206
033600080206       // Operazioni finali
033700080206       exsr RoutEnd;
033800080206
033900080206       //--------------------------------------------------------------
034000080206       //?Operazioni iniziali.
034100080206       //--------------------------------------------------------------
034200080206       BEGSR RoutInz;
034300080627       $inzs01=*on;
034400080929       clear vsctes2         ;
034500080206
034600080703       // Solo la prima volta
034700080703 1     if primavolta=' '   ;
034800080703
034900080206         // Reperimento dati job
035000080206         exsr DatiJob;
035100081003
035200081003         // Se sono EDP accendo indicatore
035300081003         if %subst(knmus:1:3)<>'EDP'    ;
035400081003         UTENonEDP=*on   ;
035500081003         endif      ;
035600080627
035700080627         // Carico tabella per divisa corrente
035800080627         reset TIBS02ds;
035900080627         T02sif = knsif;
036000080627         T02cod = 'GED';
036100080627         T02ke1 = '1'  ;
036200080627         T02mod = 'C'  ;
036300080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
036400080627
036500080703 2       if t02err=' '  ;
036600080627         dGED=t02uni    ;
036700080627         else   ;
036800080627         clear DGED    ;
036900080703 2       endif   ;
037000080627
037100080627         // Carico tabella info commerciali
037200080627         xx=0                     ;
037300080627         setll ('IFO') tntbe01l   ;
037400080627         READE ('IFO') tntbe01l   ;
037500080627
037600080703 2       dow not %eof(tntbe01l)   ;
037700080703 3       if tbeatb=' '    ;
037800080627           difo=tbeuni    ;
037900080627           xx=xx+1        ;
038000080703           ifo(xx)=tbeke1 ;
038100080627           ifods(xx)=difo ;
038200080627           ifocg(xx)=§ifoifg ;
038300080627           // ordino in base alla categoria e  num.ordinamento
038400080703           ifoor(xx)=§ifoifg+%editc(§ifoogi:'X')+tbeke1 ;
038500080703 3       endif               ;
038600080627
038700080627         READE ('IFO') tntbe01l   ;
038800080703 2       enddo    ;
038900080627
039000080627         sorta  IFOOR  ;
039100080703
039200080703         Primavolta='N'   ;
039300080703 1       endif      ;
039400080702
039500080703         Infoint=*off   ;
039600080704         Infoyell=*off   ;
039700080703         InfoImmiss=*off   ;
039800080703         Ifosott=*off   ;
039900080703         Protriga=*off   ;
040000080703         ProtFat =*off   ;
040100081003         ProtVal  =*off   ;
040200140715         ProtVald = *off;
040300140715         ProtVala = *off;
040400080703         Protsped =*off   ;
040500080703         Protfsn  =*off   ;
040600080703         clear wcat  ;
040700080703         clear wifo  ;
040800080704         $Fine=*off   ;
040900140929         clear wVALrms;
041000080703
041100080702         // Vedo se modo gestione  o interrogazione
041200080703 1       if i50mod='I' ;
041300080702         InfoInt =*on  ;
041400080703         vsctes2='INTERROGAZIONE'    ;
041500080703 1       endif;
041600080703
041700080703         if i50pgm=*blanks   ;
041800080703         vscpgm='TRMK50R'   ;
041900080703         else     ;
042000080703         vscpgm=i50pgm     ;
042100080703         endif   ;
042200080703
042300080703
042400080703         // IMPOSTO IL potenziale in visualizzazione
042500080703         vsccpo=%editc(i50cpo:'X')   ;
042600080703         vscrag=i50rag   ;
042700120111
042800120111         //?Attivo i tasti funzione F03 e F12
042900120111           F03Attivo = *on;
043000120111           F12Attivo = *on;
043100080703
043200120105         //?05/12/2012
043300120105         //?Visto che il campo per potenziale annullato non è più usato
043400120105         //?lo recuperiamo per utilizzarlo come flag per controlli da
043500120105         //?fare sulle info.
043600120110         //?Nessun controllo
043700120110           IF  I50atb = ' ';
043800120110             wAumenta10    = *off;
043900120510             wAumenta10f   = *off;
044000120110             wAzzera10     = *off;
044100120110             wDiminuisci10 = *off;
044200120110           ENDIF;
044300120110         //?Controllo per azzeramento
044400120110           IF  I50atb = '0';
044500120110             wAzzera10     = *on;
044600120110             wAumenta10    = *off;
044700120510             wAumenta10f   = *off;
044800120110             wDiminuisci10 = *off;
044900120111             F03Attivo = *off;
045000120111             F12Attivo = *off;
045100120110           ENDIF;
045200120110         //?Controllo per aumento
045300120105           IF  I50atb = '+';
045400120110             wAumenta10    = *on;
045500120510             wAumenta10f   = *off;
045600120110             wAzzera10     = *off;
045700120110             wDiminuisci10 = *off;
045800120111             F03Attivo = *off;
045900120111             F12Attivo = *off;
046000120105           ENDIF;
046100120510         //?Controllo per aumento semi obbligatorio
046200120510           IF  I50atb = '%';
046300120510             wAumenta10f   = *on;
046400120510             wAumenta10    = *off;
046500120510             wAzzera10     = *off;
046600120510             wDiminuisci10 = *off;
046700120510             F03Attivo = *off;
046800120510             F12Attivo = *off;
046900120510           ENDIF;
047000120110         //?Controllo per diminuzione
047100120109           IF  I50atb = '-';
047200120110             wDiminuisci10 = *on;
047300120110             wAumenta10    = *off;
047400120510             wAumenta10f   = *off;
047500120110             wAzzera10     = *off;
047600120111             F03Attivo = *off;
047700120111             F12Attivo = *off;
047800120105           ENDIF;
047900081003
048000081003         // Se già  inserite tutte le info, non si possono più annulla
048100081003         //  ma solo sistemare
048200081003         if i50obl=' '  and i50ifotot='S'    ;
048300081003         i50obl='S'   ;
048400081003         endif        ;
048500080627
048600080703         // VERIFICO SE ci sono già dei dati per il potenziale
048700080703         setll   i50cpo  ticpi01l    ;
048800080703         reade   i50cpo  ticpi01l    ;
048900080703 1       dow not %eof(ticpi01l) and cpiatb<>' ' ;
049000080703         reade   i50cpo  ticpi01l    ;
049100080703 1       enddo   ;
049200080703
049300080703 1       if %eof(ticpi01l) ;
049400080703         Infoimmiss=*on     ;
049500080929 1       if vsctes2= *blanks    ;
049600080703         vsctes2='  IMMISSIONE  '    ;
049700080929         endif   ;
049800080703         else    ;
049900080929 1       if vsctes2= *blanks    ;
050000080703         vsctes2='  VARIAZIONE  '    ;
050100080703 1       endif    ;
050200080929 1       endif    ;
050300080703
050400080627         ENDSR;
050500080206
050600080206       //--------------------------------------------------------------
050700080206       //?Reperimento Dati del job (Utente/Operativi).
050800080206       //--------------------------------------------------------------
050900080206       BEGSR DatiJob;
051000080206
051100080206         in(E) §AzUte;
051200080206         if NOT %error;
051300080206           in(E) §DatiUte;
051400080206         endif;
051500080206         if %error or RSut = *blanks;
051600080206           clear TIBS34ds;
051700080206           tibs34r(tibs34ds);
051800080206           in §AzUte;
051900080206           in §DatiUte;
052000080206         endif;
052100080206
052200080206       ENDSR;
052300080206
052400080206       //--------------------------------------------------------------
052500080627       //?Gestione SFL 01
052600080206       //--------------------------------------------------------------
052700080409       BEGSR GesS01;
052800080207
052900080207         // Inizializzazione videata
053000080409         if  $InzS01 = *on;
053100080409            exsr InzS01;
053200080703         // Posizionamento cursore
053300080703           s01nrr = 1;
053400080703           sfldsp_n=*off;
053500080409            $InzS01  = *off;
053600080207         endif;
053700080207
053800140725         //?Cerco l'ultima data di conferma info commerciali
053900140909           chain(n) I50cpo TNCPO11L;
054000140909           IF  %found(TNCPO11L);
054100140909             vdulconf = CPO1ducifo;
054200140909           ENDIF;
054300140909           IF  vdulconf > *zeros;
054400140909             dataISO = %date(vdulconf:*iso);
054500140909             dataEUR = dataISO;
054600140909             vdulconf = %dec(dataEUR);
054700140909           ENDIF;
054800080207
054900080207         // Emissione Testata e Piede con tasti funzionali abilitati
055000080207         if  errGenerico = *off;
055100080702           write  MK50Z01;
055200080207         endif;
055300080703
055400080703         // Posizionamento cursore
055500080703         if  C01csr  > *zero;
055600080703           C01rcd = C01csr;
055700080703         endif  ;
055800080207
055900080207         // Emissione videata
056000080702         exfmt  MK50C01;
056100080410
056200080207         reset errMessage;
056300080207         reset errGenerico;
056400080702         clear Vscmsg;
056500081003         errVAL=*off     ;
056600140715         errVALD = *off;
056700080703         errPFT=*off     ;
056800080703         errSNA=*off     ;
056900080703         errFSN=*off     ;
057000080704         errANN=*off     ;
057100120109
057200120109         //?Cerco l'ultima data variazione info commerciali
057300120124         //?Se NON è interrogazione e non è immissione
057400120109           clear dCPO01;
057500120124           IF  not InfoInt and not Infoimmiss;
057600120109             chain(n) I50cpo TNCPO01L;
057700120109             IF  %found(TNCPO01L);
057800120109               dCPO01 = CPOrst;
057900120109             ENDIF;
058000120109             IF  §CPOifodul > *zeros;
058100120109               wIFOdul    = %dec(§CPOifodul:8:0);
058200120109               wIFOdulISO = %date(wIFOdul : *ISO);
058300120109               wOggi = %dec(%date());
058400120109               dataISO = %date(wOggi : *ISO);
058500120109         //?Se la data ultima variazione è più vecchia di 1 anno da oggi windows
058600120109         //?di avviso all'utente
058700120110               IF  %diff(dataISO : wIFOdulISO : *months) > 12 and not wForzaAgg;
058800120109                 $Video = 'W1';
058900120109                 ErrGenerico = *on;
059000120109                 ErrMessage  = *on;
059100120109                 VSCmsg      = msg(32);
059200120109                 wForzaAgg   = *on;
059300120109                 leavesr;
059400120109               ENDIF;
059500120109             ENDIF;
059600120109           ENDIF;
059700080207
059800080523 1       SELECT;
059900080207
060000080207         // - F3=Fine
060100080523 1         WHEN  dsp_aid = c_F03;
060200080409            $Fine = *on;
060300081006            o50f03='S'   ;
060400080207
060500080207         // - F12=Ritorno
060600080523 1         WHEN  dsp_aid = c_F12;
060700080702            $Fine = *on;
060800081006            o50f12='S'   ;
060900080414
061000080530 x1      // Invio / F6
061100080207           OTHER;
061200081006
061300081006        // Non effettuo controlli in interrogazione
061400081006 1           if i50mod<>'I' ;
061500081006               exsr ContrS01 ;
061600081006 2             if  errGenerico = *on;
061700081006                 leavesr;
061800081006 2             endif;
061900081006 2           endif;
062000080530
062100081023         // F6=conferma Aggiornamento  F10=anche stampa
062200081023 1         if   dsp_aid = c_F06 or  dsp_aid=c_F10;
062300080702           exsr   ConfAggio ;
062400081006           o50f06='S'   ;
062500081023
062600081023              if dsp_aid=c_F10;
062700081023               kpjbu=trmk50ds  ;
062800081023               callp TRMK51R (kpjba)   ;
062900081023              endif    ;
063000081023
063100080704           $Fine = *on;
063200080530           endif  ;
063300080207
063400080523 1       ENDSL;
063500080207
063600080207       ENDSR;
063700120109
063800120109       //--------------------------------------------------------------
063900120109       //?Gestione Window errori
064000120109       //--------------------------------------------------------------
064100120109       BEGSR GesW01;
064200120109
064300120109         reset ErrMessage;
064400120109         reset ErrGenerico;
064500120109
064600120109         w1riga = vscmsg;
064700120109         exfmt  MK50W01;
064800120109
064900120109         clear Vscmsg;
065000120109         errVAL  = *off;
065100140715         errVALD = *off;
065200120109         errPFT  = *off;
065300120109         errSNA  = *off;
065400120109         errFSN  = *off;
065500120109         errANN  = *off;
065600120109
065700120109         $Video = 'S1';
065800120109
065900120109       ENDSR;
066000080207
066100080526       //--------------------------------------------------------------
066200080702       //?controlli SFL01
066300080409       //--------------------------------------------------------------
066400080409       BEGSR ContrS01;
066500140716
066600081215       clear o50ifomin;
066700081215       clear wctrprc  ;
066800081006       clear wdescat   ;
066900081006       clear totprc   ;
067000081006       clear sptpft   ;
067100081006       clear OUTFSN   ;
067200081006       clear OUTnrr   ;
067300081023       clear LOGFSN   ;
067400081023       clear LOGnrr   ;
067500081006       clear LOG40    ;
067600081007       clear bart_10  ;
067700081015       clear infoITA  ;
067800081015       clear infoEST  ;
067900081126       clear IFOval   ;
068000081126       clear MOIconta ;
068100081126       clear MOInrr   ;
068200100226       clear InfoATR  ;
068300140718       clear noBRTpft;
068400140721       clear siBRTpft;
068500140721       clear savPRFval;
068600140721       clear savPROval;
068700140721       clear savAERval;
068800140721       clear savCAEval;
068900140725       clear savATRval;
069000140721       clear savNRFpft;
069100140721       clear savNROpft;
069200140721       clear savNERpft;
069300140721       clear savNAEpft;
069400140725       clear savNTRpft;
069500140725       wEstero = *off;
069600140716
069700081126       yy=1           ;
069800080703       s01nrr=1  ;
069900080703       chain s01nrr    mk50s01    ;
070000080703
070100081006 0     dow %found    ;
070200080925       // Escludo i record di descrizione categoria
070300081006 1     if vshimm= 'C'    ;
070400081003
070500081003       // A cambio di categoria, se c'era controllo obbligatorio del
070600081003       //  100%   controllo ed eventualmente segnalo errore
070700081006       EXSR CambioCAT    ;
070800081006       if errGenerico=*on  ;
070900081006       leavesr;
071000081006       endif              ;
071100081006
071200081006x1     else    ;
071300081006
071400080925       pftblu=*off       ;
071500080925
071600081003       // se valori calcolati da pgm li pulisco prima dei controlli
071700081007 2     if vshsnf=' ' and vshprpft=' '   ;
071800080925       clear vscpft   ;
071900140717       // clear vscvft   ;
072000081007 2     endif          ;
072100080703
072200080703       // Imposto la divisa, se impostato il fatturato
072300140717 2     //if vscpft>0 and vscvft=*blanks   ;
072400140717       //vscvft=§gedcr   ;
072500140717       //errGenerico = *on;
072600140717 2     //endif    ;
072700080703
072800140717 2     //if vscpft=0 and vscvft<>*blanks   ;
072900140717       //clear vscvft   ;
073000140717       //errGenerico = *on;
073100140717 2     //endif   ;
073200140716
073300140716       //?Se valore fatturato < 0 errore
073400140716       IF  VSCpft < 0;
073500140716         VSCmsg = msg(25);
073600140716         errPFT=*on;
073700140716         EXSR AggioSFL   ;
073800140716         leavesr;
073900140716       ENDIF;
074000140716
074100140716       //?Se valore fatturato impostato e > di 999000000 errore
074200140716       IF  VSCpft > 999000000;
074300140716         VSCmsg = msg(17);
074400140716         errPFT=*on;
074500140716         EXSR AggioSFL   ;
074600140716         leavesr;
074700140716       ENDIF;
074800140716
074900140716       //?Se valore fatturato impostato e > di 999999 warning
075000140716       IF  VSCpft > 999999 and not wForzaPft;
075100140716         VSCmsg = msg(23);
075200140716         errPFT=*on;
075300140716         wForzaPft = *on;
075400140716         EXSR AggioSFL   ;
075500140716         leavesr;
075600140716       ENDIF;
075700140716
075800140716       //?Le spedizioni non possono essere superiori del valore fatturato
075900140722       IF  VSCsna > VSCpft and VSCifo = 'SPT';
076000140716         VSCmsg = msg(24);
076100140716         errPFT=*on;
076200140716         EXSR AggioSFL   ;
076300140716         leavesr;
076400140716       ENDIF;
076500140716
076600140716       //?Se spedizioni < 0 errore
076700140716       IF  VSCsna < 0;
076800140716         VSCmsg = msg(25);
076900140716         errSNA=*on;
077000140716         EXSR AggioSFL   ;
077100140716         leavesr;
077200140716       ENDIF;
077300140723
077400140723       //?Se VAL < 0 errore
077500140723       IF  vscval < 0;
077600140723         VSCmsg = msg(25);
077700140723         errVAL=*on;
077800140723         EXSR AggioSFL   ;
077900140723         leavesr;
078000140723       ENDIF;
078100140723
078200140723       //?Se VALD < 0 errore
078300140723       IF  vscvald < 0;
078400140723         VSCmsg = msg(25);
078500140723         errVALD=*on;
078600140723         EXSR AggioSFL   ;
078700140723         leavesr;
078800140723       ENDIF;
078900080703
079000080704       // se info da annullare e non c'e' la "A", msg forzabile
079100081215       //  non faccio se modalità controlli
079200081215 2     if i50mod<>'C' and vshann='S'  and vscatb=' '  ;
079300080704           errANN=*on;
079400080704           // se  contiene dati, msg forzabile
079500081007 3         if vscfsn='S'or vscval>0 or vscpft>0 or vscsna>0   ;
079600080704           vscmsg = Msg(07);
079700080704           vshann='X'      ;
079800080704           // Se non contiene dati abbligatorio l'annullamento
079900081007 x3        else            ;
080000080704           vscmsg = Msg(08);
080100081007 3         endif           ;
080200080704           EXSR AggioSFL   ;
080300080704           leavesr;
080400081007 2     endif    ;
080500110707
080600110707       // se c'e' "O"  obbligatorio l'annullamento
080700110707 2     if i50mod<>'C' and vshann='O'  and vscatb=' '  ;
080800110707           errANN=*on;
080900110707           vscmsg = Msg(08);
081000110707           EXSR AggioSFL   ;
081100110707           leavesr;
081200110707 2     endif    ;
081300080704
081400080703       // Se richiesto inserimento obbligatorio, per i dati
081500080703       //  obbligatori da tabella segnalo errore
081600081007 2     if i50obl='S' and vshobl='S'                 ;
081700081007 3     if vscatb='A'    or
081800081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
081900081003         if vshsne<>' ';
082000080703           errFSN=*on;
082100081003         endif   ;
082200081003         if vshsnv<>' ';
082300081003           errVAL=*on;
082400081003         endif   ;
082500140715         IF  vshsnvd <> *blanks;
082600140715           errVALD = *on;
082700140715         ENDIF;
082800081003         if vshsnf<>' ';
082900081003           errPFT=*on;
083000081003         endif   ;
083100080703           vscmsg = Msg(01);
083200080703           EXSR AggioSFL   ;
083300080703           leavesr;
083400081007 3     endif   ;
083500081007 2     endif   ;
083600081007
083700081007       // Non accetto se non previsto il valore '?'
083800081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
083900081007           errFSN=*on;
084000081007           vscmsg = Msg(20);
084100081007           EXSR AggioSFL   ;
084200081007           leavesr;
084300081007 3     endif    ;
084400081007       if vshifgv<>'?' and vscfsn='?'  ;
084500081007           errFSN=*on;
084600081007           vscmsg = Msg(21);
084700081007           EXSR AggioSFL   ;
084800081007           leavesr;
084900081007 3     endif    ;
085000080703
085100080703       // Se immesso "N" non indicare gli altri campi
085200081007 2     if vscfsn='N' and (vscval>0 or vscpft>0 or vscsna>0)   ;
085300080703       if vshsnf<>' '    ;
085400080703           errPFT=*on;
085500080703       else    ;
085600081003          errVAL=*on ;
085700080703       endif  ;
085800080703           vscmsg = Msg(02);
085900080703           EXSR AggioSFL   ;
086000080703           leavesr;
086100081007 2     endif   ;
086200080703
086300081003       // Se immesso almeno un dato, controllo gli obbligatori ed
086400081215       //   i facoltativi. I  facoltativi non li guardo in modalità
086500081215       //   solo controllo
086600081031       if vscatb =' ' and
086700081031 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval>0)            ;
086800081003
086900081007 3     if vscpft=0    and vshsnf='O'   ;
087000081003           errPFT=*on;
087100081003           vscmsg = Msg(03);
087200081003           EXSR AggioSFL   ;
087300081003           leavesr;
087400081007 3      endif    ;
087500100503
087600100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
087700100503 3     if i50obl='S' and
087800100503          I50mod<>'C' and  vscpft=0    and vshsnf='S'   ;
087900081003           errPFT=*on;
088000081003           vscmsg = Msg(05);
088100081003           vshsnf='X'      ;
088200081003           EXSR AggioSFL   ;
088300081003           leavesr;
088400081007 3     endif    ;
088500081007 3     if vscsna=0    and vshsns='O'   ;
088600081003           errSNA=*on;
088700081003           vscmsg = Msg(10);
088800081003           EXSR AggioSFL   ;
088900081003           leavesr;
089000081007 3     endif    ;
089100100503
089200100503       // IL DATO OPZIONALE lo chiedo solo qudno le info sono obbligatorie
089300100503 3     if i50obl='S' and
089400100503          i50mod<>'C' and vscsna=0    and vshsns='S'   ;
089500081003           errSNA=*on;
089600081003           vscmsg = Msg(06);
089700081003           vshsns='X'      ;
089800081003           EXSR AggioSFL   ;
089900081003           leavesr;
090000081007 3     endif    ;
090100081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
090200081007           errFSN=*on;
090300081007           vscmsg = Msg(19);
090400081007           EXSR AggioSFL   ;
090500081007           leavesr;
090600081007 3     endif    ;
090700100503 3     if i50obl='S' and
090800100503 3        i50mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
090900081007           errFSN=*on;
091000081007           vscmsg = Msg(22);
091100081007           vshsne='X'      ;
091200081007           EXSR AggioSFL   ;
091300081007           leavesr;
091400081007 3     endif    ;
091500081007 3     if (vscval=0    and vshsnv='O')  or
091600100503          (vscval=0    and vshsnv='S' and i50mod<>'C'AND I50OBL='S')   ;
091700081007 4        if vshsnv='O'    ;
091800081003           vscmsg = Msg(11);
091900081003          else   ;
092000081215             vscmsg = Msg(04);
092100081215             vshsnv='X'      ;
092200081215 4        endif  ;
092300081007 4        if vsctva='%  '   ;
092400081003          %subst(vscmsg:37:18)='della % fatturato '     ;
092500081007 4        endif     ;
092600081007 4        if vsctva='KG '   ;
092700081003          %subst(vscmsg:37:18)='del peso in Kg.   '     ;
092800081007 4        endif     ;
092900081215
093000081003           errVAL=*on;
093100081003           EXSR AggioSFL   ;
093200081003           leavesr;
093300081215 3     endif    ;
093400140721
093500140715 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
093600140715           (vscvald = 0 and vshsnvd = 'S'   and
093700140715            I50mod <> 'C' and I50obl = 'S');
093800140715 4        IF  vshsnvd = 'O';
093900140715            vscmsg = Msg(01);
094000140715          ELSE;
094100140715            vscmsg = Msg(01);
094200140715            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
094300140715            vshsnvd = 'X';
094400140715 4        ENDIF;
094500140715          errVALD = *on;
094600140715          EXSR AggioSFL;
094700140715          leavesr;
094800140715 3     ENDIF;
094900140715
095000140715       //?Mi salvo il valore del traffico totale
095100140715 3     IF  vscifo = 'SPT';
095200140716         sav_vscpft = vscpft;
095300140716         sav_vscsna = vscsna;
095400140715       ENDIF;
095500081003
095600081003       // se Valore in % --> non deve superare 100%
095700081007 3     if  vscval>0 and vsctva='%  '    ;
095800081007 4       if vscval>100    ;
095900081003          errVAL=*on ;
096000081003          vscmsg = Msg(12);
096100081003          EXSR AggioSFL   ;
096200081003          leavesr;
096300081007 4       endif      ;
096400081003
096500080925       // Sommo la percentuale se presente per ogni categoria
096600080925       //  se supera 100% segnalo errore
096700081006       //  solo se previsto controllo sulla %
096800081007 4     if wctrprc='P'     ;
096900081006
097000110707       // Non sommo le info annullate perchè non le devo considerare valide
097100110707       if vshann=' '   ;
097200110707         totprc=totprc+vscval    ;
097300110707       endif  ;
097400110707
097500081007 5       if totprc>100    ;
097600081003         errVAL=*on ;
097700080925         vscmsg = Msg(09);
097800080925         EXSR AggioSFL   ;
097900080925         leavesr;
098000081007 5       endif      ;
098100081007 4     endif      ;
098200080925
098300081003       // Se immessa la %, visualizzo in base a SPT
098400080925       exsr   VISUALimp     ;
098500081007 3     endif      ;
098600081007 2     endif      ;
098700140716
098800140716       //?Se Valore con decimali in % --> non deve superare 100%
098900140716 3     IF  vscvald <> 0 and vsctva = '%  ';
099000140716 4       IF  vscvald > 100;
099100140716           errVALD = *on;
099200140716           vscmsg = Msg(12);
099300140716           EXSR AggioSFL   ;
099400140716           leavesr;
099500140716 4       ENDIF;
099600140716 4     ENDIF;
099700140715
099800140715       //?Per Ricavo medio spedizione
099900140715 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
100000140715       //?calcolo il valore
100100140715 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
100200140715           wVALrms = sav_vscpft / sav_vscsna;
100300140715         ELSE;
100400140715           wVALrms = 0;
100500140715         ENDIF;
100600140715       //?reimposto il flag di forzatura
100700140715         IF  wVALrms <> sav_valrms;
100800140716           wForzaRms = *off;
100900140715         ENDIF;
101000140715       //?imposto il campo del video
101100140919         IF  wVALrms > 999;
101200140919           vscvald = 999;
101300140715         ELSE;
101400140715           vscvald = wVALrms;
101500140715         ENDIF;
101600140715       //?msg info se inferiore a 4
101700140716         IF  VSCvald < 4 and not wForzaRms and
101800140715             wVALrms <> sav_valrms;
101900140715           errVALD = *on;
102000140715           vscmsg = msg(35);
102100140715           exsr AggioSFL;
102200140716           wForzaRms = *on;
102300140715           sav_valrms = wVALrms;
102400140715           leavesr;
102500140715         ENDIF;
102600140715       //?msg info se superiore a 50
102700140716         IF  VSCvald > 50 and not wForzaRms and
102800140715             wVALrms <> sav_valrms;
102900140715           errVALD = *on;
103000140715           vscmsg = msg(36);
103100140715           exsr AggioSFL;
103200140716           wForzaRms = *on;
103300140715           sav_valrms = wVALrms;
103400140715           leavesr;
103500140715         ENDIF;
103600140715       ENDIF;
103700140716
103800140716       // se INFO obbligatoria
103900140716       // errore se info non inserita
104000140721       // flag di comodo per non richiedere obbligatoria la INFO ora in fase di test
104100140721       // non so ancora se è obbligatoria sempre o solo se I50obl = 'S'
104200140721       IF  VSHobl = 'S' and wnews = *on;
104300140716         SELECT;
104400140716           WHEN  vshsnv <> *blanks and vscval = 0;
104500140716             errVAL = *on;
104600140716             vscmsg = Msg(01);
104700140716             EXSR AggioSFL;
104800140716             leavesr;
104900140716           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
105000140716             errVALD = *on;
105100140716             vscmsg = Msg(01);
105200140716             EXSR AggioSFL;
105300140716             leavesr;
105400140716           WHEN  vshsnf <> *blanks and vscpft = 0;
105500140716             errPFT = *on;
105600140716             vscmsg = Msg(01);
105700140716             EXSR AggioSFL;
105800140716             leavesr;
105900140716           WHEN  vshsns <> *blanks and vscsna = 0;
106000140716             errSNA = *on;
106100140716             vscmsg = Msg(01);
106200140716             EXSR AggioSFL;
106300140716             leavesr;
106400140716           WHEN  vshsne <> *blanks and vscfsn = *blanks;
106500140716             errFSN = *on;
106600140716             vscmsg = Msg(01);
106700140716             EXSR AggioSFL;
106800140716             leavesr;
106900140716         ENDSL;
107000140716       ENDIF;
107100140725
107200140725       // Paesei esteri ammessi solo se inserita almeno 1 % di ripartizione estera
107300140725       IF  VSCifo = '50 ' and VSCatb = *blanks and
107400140725           VSCfsn <> *blanks and not wEstero;
107500140725         errFSN = *on;
107600140725         VSCmsg = Msg(37);
107700140725         EXSR AggioSFL;
107800140725         leavesr;
107900140725       ENDIF;
108000081006
108100081006       // Concorrenti logistica ammessi solo se outsourcing=S
108200110707 2     if vscifo='40 '  and vscatb=' ' and (vscfsn<>' ' or vscpft>0 or vscsna>0
108300110707                            or vscval>0) ;
108400081006       if outfsn<>'S'   ;
108500081006         errFSN=*on ;
108600081006         vscmsg = Msg(14);
108700081006         EXSR AggioSFL   ;
108800081006         leavesr;
108900081006       endif             ;
109000081007
109100081006       // Memorizzo presenza info 40  se immesso Si o presente altro
109200081006       //  importo
109300110707         if vshann=' ' and vscfsn<>'N';
109400081007         LOG40='S'         ;
109500081007         endif             ;
109600081007 2     endif             ;
109700081008
109800081126       // Sottotipo col '?' --> Se '?' richiamo pgm di ricerca
109900081126 2     if vscfsn='?' ;
110000081008       clear tntb70ds ;
110100081008       itb70cpo=i50cpo;
110200081023       itb70rag=i50rag ;
110300081126       itb70tpf=vscifo  ;
110400081008       itb70pgm=i50pgm  ;
110500081008       callp TNTB70R (kpjba:tntb70ds)  ;
110600081008       clear vscfsn       ;
110700081126 3     if otb70spf>*zeros    ;
110800081008         vscspp=otb70spf  ;
110900081008         vsdifo=otb70dspf ;
111000081008         vscval=%int(otb70spf) ;
111100081126 3     endif         ;
111200081008       c01csr=s01nrr    ;
111300081008       exsr AggioSFL    ;
111400081008       leavesr       ;
111500081126 2     endif         ;
111600081007
111700081126 2     if vscval=0    and vshifgv='?' ;
111800081007       clear vscspp      ;
111900081009       clear vsdifs      ;
112000081009       vsdifo=CoRicerca  ;
112100081126 2     endif     ;
112200081007
112300081126       // Motivo non affidamento a noi: da richidere solo se corriere BART
112400081126       //  non è al 100
112500081126 2     if  vscifo='MOI'   ;
112600081126
112700081126         if vscval=0   ;
112800081126          vsdifo=CoRicercaMOI  ;
112900081126         endif         ;
113000081126
113100081126 3       if bart_10=100 and vscval>0        ;
113200081126           errVAL=*on ;
113300081126           vscmsg = Msg(16);
113400081126           EXSR AggioSFL   ;
113500081126           leavesr;
113600081126 3       endif             ;
113700081126
113800081126         if vscval>0      ;
113900081126           MOIconta=moiconta+1   ;
114000081126         endif     ;
114100081126
114200081126         moinrr=s01nrr    ;
114300081126
114400081126 2     endif            ;
114500081008
114600081007       // Controllo
114700081126 2     if vscval>0    and vshifgv='?' ;
114800081007       w005a=%editc(vscval:'X')    ;
114900081126       chain ('IFS':vscifo:%subst(w005a:2:4))  tntbe01l    ;
115000081126 3       if not %found(tntbe01l) or tbeatb<>' ' or %subst(w005a:1:1)<>'0';
115100081007          errVAL=*on ;
115200081007          vscmsg = Msg(18);
115300081007          EXSR AggioSFL   ;
115400081007          leavesr;
115500081126 x3       else                        ;
115600081007          vscspp=%subst(w005a:2:4)    ;
115700081007          difs=tbeuni ;
115800081007          vsdifo=§ifsdes              ;
115900081007          vsdifs=§ifsdel              ;
116000081126  3      endif             ;
116100081126
116200081126        //  Memorizzo i codici per vedere se sono doppi
116300081126        w008a=vscifo+%editc(vscval:'X')   ;
116400081126        Indx=%lookup(w008a:IFOval)   ;
116500081126 3      if Indx>0    ;
116600081126         errVAL=*on ;
116700081126 4       if vscifo='MOI'   ;
116800081126           vscmsg = Msg(28);
116900081126         else      ;
117000081126           vscmsg = Msg(30);
117100081126 4       endif     ;
117200081126         EXSR AggioSFL   ;
117300081126         leavesr;
117400081126
117500081126 x3     else   ;
117600081126          ifoval(yy)=w008a  ;
117700081126          yy=yy+1   ;
117800081126          // Se c'e' un MOI=9999 e uno no 9999, errore
117900081126          w008a='MOI09999'  ;
118000081126          Indx=%lookup(w008a:IFOval) ;
118100081126  4       if Indx>0   and moiconta>1 ;
118200081126            errVAL=*on ;
118300081126            vscmsg = Msg(29);
118400081126            EXSR AggioSFL   ;
118500081126            leavesr;
118600081126  4       endif    ;
118700081126  3     endif             ;
118800081126  2    endif             ;
118900081007
119000081126  2    if vscatb=' '  ;
119100081031
119200080925       // Se info SPT - totale spesa trasporti,  la memorizzo
119300080925       //  per il calcolo delle %
119400080925       if vscifo='SPT' ;
119500080925       SPTPFT=vscpft   ;
119600080925       endif           ;
119700140725
119800140725       // Se ripartizione spesa ESTERA lo memorizzo
119900140725       IF  VSHie = 'E' and VSCval > 0;
120000140725         wEstero = *on;
120100140725       ENDIF;
120200140725
120300081023       // Se info LOG - logistica  memorizzo per dare obbligatorio outsourci
120400081023       if vscifo='LOG' ;
120500081023       LOGfsn=vscfsn   ;
120600081023       LOGnrr=s01nrr   ;
120700081006       endif           ;
120800081023       // Se info OUT - outsourcing memorizzo per dare obbligatori
120900081023       //  i concorrenti della logistica
121000081023       if vscifo='OUT' ;
121100081023       OUTfsn=vscfsn   ;
121200081023       OUTnrr=s01nrr   ;
121300081023       endif           ;
121400140718
121500081007       // Se info 10 _BAR - concorrenti utilizzati bartolini
121600081007       //  memorizzo la % che ha
121700081007       if vscifo='10' and vscspp='_BAR' ;
121800081007       bart_10=vscval   ;
121900120105
122000120110         //?Controllo % affidamento a BRT
122100120110         SELECT;
122200120110         //?Controllo se devo azzerare la % di affidato a BRT
122300120110         WHEN  wAzzera10 and bart_10 > 0;
122400120110           errVAL = *on;
122500120125           VSCmsg = Msg(33);
122600120110           EXSR AggioSFL;
122700120110           leavesr;
122800120110         //?Controllo se devo aumentare la % di affidato a BRT
122900120111         WHEN  wAumenta10 and bart_10 < sav_bar_10 and bart_10 < 100;
123000120110           errVAL = *on;
123100120125           VSCmsg = Msg(31);
123200120110           EXSR AggioSFL;
123300120110           leavesr;
123400120510         //?Controllo se devo aumentare la % di affidato a BRT solo se = 0
123500120510         WHEN  wAumenta10f and bart_10 < sav_bar_10 and bart_10 = 0;
123600120510           errVAL = *on;
123700120510           VSCmsg = Msg(31);
123800120510           EXSR AggioSFL;
123900120510           leavesr;
124000120110         //?Controllo se devo diminuire la % di affidato a BRT
124100120110         WHEN  bart_10 > 0 and
124200120110               wDiminuisci10 and bart_10 > sav_bar_10;
124300120110           errVAL = *on;
124400120125           VSCmsg = Msg(34);
124500120110           EXSR AggioSFL;
124600120110           leavesr;
124700120110         ENDSL;
124800120105
124900081007       endif           ;
125000140718
125100100302       // Se info ATR - altro di ripartizione memorizzo la %
125200100226       if vscifo='ATR'                  ;
125300100226       infoATR=vscval   ;
125400100226       endif           ;
125500140721
125600140721       //?Salvo le % di ripartizione spesa totale
125700140721       SELECT;
125800140721         WHEN  VSCifo = 'PRF';
125900140721           savPRFval = VSCval;
126000140721         WHEN  VSCifo = 'PRO';
126100140721           savPROval = VSCval;
126200140721         WHEN  VSCifo = 'AER';
126300140721           savAERval = VSCval;
126400140721         WHEN  VSCifo = 'CAE';
126500140721           savCAEval = VSCval;
126600140725         WHEN  VSCifo = 'ATR';
126700140725           savATRval = VSCval;
126800140721       ENDSL;
126900140718
127000140919       //?Salvo e sommo il NON affidato a BRT
127100140721       SELECT;
127200140721         WHEN  VSCifo = 'NRF';
127300140721           savNRFpft = VSCpft;
127400140721           noBRTpft += VSCpft;
127500140721         WHEN  VSCifo = 'NRO';
127600140721           savNROpft = VSCpft;
127700140721           noBRTpft += VSCpft;
127800140721         WHEN  VSCifo = 'NER';
127900140721           savNERpft = VSCpft;
128000140721           noBRTpft += VSCpft;
128100140721         WHEN  VSCifo = 'NAE';
128200140721           savNAEpft = VSCpft;
128300140721           noBRTpft += VSCpft;
128400140725         WHEN  VSCifo = 'NTR';
128500140725           savNTRpft = VSCpft;
128600140725           noBRTpft += VSCpft;
128700140721       ENDSL;
128800100226
128900081126 2     endif        ;
129000080925
129100080703       EXSR AggioSFL   ;
129200081006 1     endif   ;
129300081125
129400080703
129500080703       s01nrr=1+s01nrr  ;
129600080703       chain s01nrr    mk50s01    ;
129700081006 0     enddo         ;
129800081006
129900081006       // Controllo ultima categoria
130000081006       EXSR CambioCAT     ;
130100081006
130200081215       // A questo punto le info minime sono presenti
130300081215       // solo per controlli obbligatori
130400081215       if i50obl='S' ;
130500081215         o50ifomin='S' ;
130600081215       endif ;
130700081215
130800081126       // Un motivo non affidamento obbligatorio se Bart<100%
130900081126 3     if bart_10<100 and MOIconta =0   and i50obl='S' ;
131000081126         chain moinrr    mk50s01    ;
131100081126         s01nrr=moinrr              ;
131200081126         errVAL=*on ;
131300081126         vscmsg = Msg(27);
131400081126         EXSR AggioSFL   ;
131500081126         leavesr;
131600081126 3     endif             ;
131700081126
131800081023       // Se logistica interessante per noi, obbligatorio outsourcing
131900081023       if LOGfsn='S' and outfsn=' '  and i50obl='S'  ;
132000081023         chain lognrr    mk50s01    ;
132100081023         s01nrr=lognrr              ;
132200081023         errFSN=*on ;
132300081023         vscmsg = Msg(26);
132400081023         EXSR AggioSFL   ;
132500081023         leavesr;
132600081023       endif             ;
132700081006       // Se presente outsourcing, obbligatori concorrenti logistica
132800081006       if outfsn='S' and log40=' '  and i50obl='S'  ;
132900081006         chain outnrr    mk50s01    ;
133000081006         s01nrr=outnrr              ;
133100081006         errFSN=*on ;
133200081006         vscmsg = Msg(15);
133300081006         EXSR AggioSFL   ;
133400081006         leavesr;
133500081006       endif             ;
133600140929
133700140929       //?Non affidato a BRT se affidato a BRT < 100% devo richiederlo obbligatorio
133800140929       //?quando il pgm è richiamo per info obbligatorie
133900140929        IF  I50obl = 'S' and BART_10 < 100 and
134000140929            noBRTpft = 0 and
134100140929            (savPRFval > 0 or savPROval > 0 or savAERval > 0 or
134200140929             savCAEval > 0 or savATRval > 0);
134300140929          chain ifoNRFnrr mk50S01;
134400140929          s01nrr = ifoNRFnrr;
134500140929          errPFT = *on;
134600140929          VSCmsg = Msg(38);
134700140929          EXSR AggioSFL;
134800140929          leavesr;
134900140929        ENDIF;
135000140721
135100140919       //?Se Non affidato a BRT impostato deve esserci la relativa % di ripartizione
135200140721       //?spesa traffico totale
135300140722        IF  savNRFpft > 0 and savPRFval = 0;
135400140721          chain ifoNRFnrr mk50S01;
135500140721          s01nrr = ifoNRFnrr;
135600140721          errPFT = *on;
135700140722          vscmsg = Msg(42);
135800140721          EXSR AggioSFL;
135900140721          leavesr;
136000140722        ENDIF;
136100140722        IF  savNRFpft > (SPTpft * savPRFval / 100);
136200140722          chain ifoNRFnrr mk50S01;
136300140722          s01nrr = ifoNRFnrr;
136400140722          errPFT = *on;
136500140722          vscmsg = Msg(41);
136600140722          EXSR AggioSFL;
136700140722          leavesr;
136800140722        ENDIF;
136900140722        IF  savNROpft > 0 and savPROval = 0;
137000140721          chain ifoNROnrr mk50S01;
137100140721          s01nrr = ifoNROnrr;
137200140721          errPFT = *on;
137300140722          vscmsg = Msg(42);
137400140721          EXSR AggioSFL;
137500140721          leavesr;
137600140722        ENDIF;
137700140722        IF  savNROpft > (SPTpft * savPROval / 100);
137800140722          chain ifoNROnrr mk50S01;
137900140722          s01nrr = ifoNROnrr;
138000140722          errPFT = *on;
138100140722          vscmsg = Msg(41);
138200140722          EXSR AggioSFL;
138300140722          leavesr;
138400140722        ENDIF;
138500140721         IF  savNERpft > 0 and savAERval = 0;
138600140721          chain ifoNERnrr mk50S01;
138700140721          s01nrr = ifoNERnrr;
138800140721          errPFT = *on;
138900140722          vscmsg = Msg(42);
139000140721          EXSR AggioSFL;
139100140721          leavesr;
139200140721         ENDIF;
139300140722         IF  savNERpft > (SPTpft * savAERval / 100);
139400140722          chain ifoNERnrr mk50S01;
139500140722          s01nrr = ifoNERnrr;
139600140722          errPFT = *on;
139700140722          vscmsg = Msg(41);
139800140722          EXSR AggioSFL;
139900140722          leavesr;
140000140722         ENDIF;
140100140721         IF  savNAEpft > 0 and savCAEval = 0;
140200140721          chain ifoNAEnrr mk50S01;
140300140721          s01nrr = ifoNAEnrr;
140400140721          errPFT = *on;
140500140722          vscmsg = Msg(42);
140600140721          EXSR AggioSFL;
140700140721          leavesr;
140800140721         ENDIF;
140900140722         IF  savNAEpft > (SPTpft * savCAEval / 100);
141000140722          chain ifoNAEnrr mk50S01;
141100140722          s01nrr = ifoNAEnrr;
141200140722          errPFT = *on;
141300140722          vscmsg = Msg(41);
141400140722          EXSR AggioSFL;
141500140722          leavesr;
141600140722         ENDIF;
141700140725         IF  savNTRpft > 0 and savATRval = 0;
141800140725          chain ifoNTRnrr mk50S01;
141900140725          s01nrr = ifoNAEnrr;
142000140725          errPFT = *on;
142100140725          vscmsg = Msg(42);
142200140725          EXSR AggioSFL;
142300140725          leavesr;
142400140725         ENDIF;
142500140725         IF  savNTRpft > (SPTpft * savATRval / 100);
142600140725          chain ifoNTRnrr mk50S01;
142700140725          s01nrr = ifoNTRnrr;
142800140725          errPFT = *on;
142900140725          vscmsg = Msg(41);
143000140725          EXSR AggioSFL;
143100140725          leavesr;
143200140725         ENDIF;
143300140718
143400140718       //?Se affidato a BRT è 100%  non devo indicare nessuna spesa per il mancato
143500140718       //?affidamento a BRT
143600140718        IF  bart_10 = 100 and noBRTpft > 0;
143700140718          chain ifoNRFnrr mk50S01;
143800140718          s01nrr = ifoNRFnrr;
143900140718          errPFT = *on;
144000140721          vscmsg = Msg(40);
144100140718          EXSR AggioSFL;
144200140718          leavesr;
144300140718        ENDIF;
144400140721
144500140721       //?Se affidato a BRT non è 100%
144600140919       //?Il non affidato a BRT deve essere congruente
144700140804        IF  bart_10 <> 100 and noBRTpft > 0 and
144800140725            noBRTpft <> (sptpft - siBRTpft);
144900140722          SELECT;
145000140722            WHEN  savPRFval > 0;
145100140722              chain ifoNRFnrr mk50S01;
145200140722              s01nrr = ifoNRFnrr;
145300140722            WHEN  savPROval > 0;
145400140722              chain ifoNROnrr mk50S01;
145500140722              s01nrr = ifoNROnrr;
145600140722            WHEN  savAERval > 0;
145700140722              chain ifoNERnrr mk50S01;
145800140722              s01nrr = ifoNERnrr;
145900140722            WHEN  savCAEval > 0;
146000140722              chain ifoNAEnrr mk50S01;
146100140722              s01nrr = ifoNAEnrr;
146200140725            WHEN  savATRval > 0;
146300140725              chain ifoNTRnrr mk50S01;
146400140725              s01nrr = ifoNTRnrr;
146500140722          ENDSL;
146600140721          errPFT = *on;
146700140721          vscmsg = Msg(40);
146800140721          EXSR AggioSFL;
146900140721          leavesr;
147000140721        ENDIF;
147100081006
147200080409       ENDSR;
147300080409
147400081006       //--------------------------------------------------------------
147500081006       //?cambiata categoria delle info conn- controllo e salvataggi
147600081006       //--------------------------------------------------------------
147700081006       BEGSR  CambioCAT    ;
147800140716
147900081006       if wctrprc='P'  and  i50obl='S' and totprc<100  ;
148000081006       s01nrr=s01nrr-1 ;
148100081006       chain s01nrr    mk50s01    ;
148200081006           errVal=*on;
148300081006           vscmsg = Msg(13);
148400081006           %subst(vscmsg:40:21)=wdescat   ;
148500081006           EXSR AggioSFL   ;
148600081006       endif              ;
148700081006
148800081006       // Pulisco e memorizzo nuovi dati per nuova categoria
148900081006        clear  totprc     ;
149000081215        wctrprc=vshobl  ;
149100081215        wdescat=vsdifo   ;
149200140716
149300081006        ENDSR   ;
149400080925       //--------------------------------------------------------------
149500080925       //?Visualizzo importi in base alla spesa trasporti
149600080925       //--------------------------------------------------------------
149700080925       BEGSR    VISUALimp  ;
149800081003       if vshsnf=' ' and vscpft=0 and vscval>0 and vsctva='%  '  ;
149900081215       PFTblu=*on     ;
150000081003       vscpft=(SPTpft*vscval)/100   ;
150100140717       //if vscpft>0    ;
150200140717       //vscvft='EUR'   ;
150300140717       //endif ;
150400140721       //?Se sto calcolando l'importo della % di affido a BRT me la salvo
150500140721         IF  vscifo = '10' and vscspp = '_BAR';
150600140721           siBRTpft = vscpft;
150700140721         ENDIF;
150800140721
150900080925       endif ;
151000080925
151100080925       ENDSR      ;
151200080703       //--------------------------------------------------------------
151300080703       //?Aggiornamento sfl
151400080703       //--------------------------------------------------------------
151500080703       BEGSR AGGIOSFL  ;
151600080703       if vscmsg<>*blanks   ;
151700080703       errMessage  = *on;
151800080703       errGenerico = *on;
151900080704       c01csr=s01nrr    ;
152000080703       endif    ;
152100080703
152200080703       exsr ProtCampi   ;
152300080703
152400080703       //  Ripristino dei flag di forzatura
152500081003       if vshsnf='X' and vscpft>0   ;
152600081003       vshsnf='S'   ;
152700080703       endif    ;
152800081003       if vshsns='X' and vscsna>0   ;
152900080703       vshsns='S'   ;
153000080703       endif    ;
153100081003       if vshsnv='X' and vscval>0  ;
153200081003       vshsnv='S'   ;
153300080703       endif    ;
153400140715       IF  vshsnvd = 'X' and vscvald <> 0;
153500140715         vshsnvd = 'S';
153600140715       ENDIF;
153700081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
153800081007       vshsne='S'   ;
153900081007       endif    ;
154000080703
154100110707       if vscatb='A' and vshann= 'X' ;
154200080704       vshann='S'  ;
154300080704       endif   ;
154400080703
154500080703       update  mk50s01  ;
154600080703       ENDSR   ;
154700080207       //--------------------------------------------------------------
154800080627       //?Inizializzazione SFL01
154900080207       //--------------------------------------------------------------
155000080409       BEGSR InzS01;
155100080207
155200080207       // Pulizia subfile
155300080207         SflDsp_N    = *on;
155400080207         SflDspCtl_N = *on;
155500080627         write  MK50C01;
155600080207         SflDspCtl_N = *off;
155700080207         SflEnd      = *off;
155800080530
155900080530         clear C01rcd;
156000100507         C01csr=1;
156100080627         S01nrr=0 ;
156200080702         clear Vscmsg;
156300080207         errMessage  = *off;
156400080207         errGenerico = *off;
156500080627         clear savcat  ;
156600080627         clear wcat    ;
156700080627         clear wifo    ;
156800081006         clear wIFS    ;
156900081006         clear sptpft  ;
157000100302         clear savspt  ;
157100100302         clear savatr  ;
157200140716
157300120109         wForzaAgg = *off;
157400120111         clear sav_bar_10;
157500140716         wForzaPft = *off;
157600140715
157700140715         errVAL=*off     ;
157800140715         errVALD = *off;
157900080409
158000080702       // Carico le info
158100080627       xx=1    ;
158200080627       Indx=1    ;
158300080702       dow   xx<=50    ;
158400080702       if    ifoor(xx)<>*blanks  ;
158500080627       clear  MK50S01;
158600080627
158700080627       Wcat= %subst(ifoor(xx):1:2)   ;
158800080702       Wifo= %subst(ifoor(xx):6:3)   ;
158900080627
159000080627       // cambio di categoria
159100080627        if savcat=*blanks or savcat<>wcat               ;
159200080627        savcat=wcat   ;
159300080627        Ifosott=*on   ;
159400080627
159500080627       // Verifico se devo visualizzare la titolo della categoria
159600080627         clear  DIFG  ;
159700080627         reset TIBS02ds;
159800080627         T02sif = knsif;
159900080627         T02cod = 'IFG';
160000080627         T02mod = 'C'  ;
160100080627         T02ke1 = wcat;
160200080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
160300080627         if t02err=*blanks ;
160400080627            difg=t02uni    ;
160500080627         endif            ;
160600080627
160700080627       // Se prevista la visualizzazione della categoria
160800080627       //  emetto la riga in videata,
160900080627       if §ifgvis='S'    ;
161000080703         vsdifo=§ifgdes         ;
161100080703         vsdifo=%trim(vsdifo)+':'    ;
161200080703         // In VSHIMM   metto una "C" per indicare che si tratta
161300080703         //    di riga di categoria
161400080703         vshimm='C'        ;
161500081007
161600081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
161700081003         vshobl=§ifgctpr   ;
161800081007
161900081007         EXSr ProtCampi    ;
162000081003
162100080627         s01nrr=s01nrr+1   ;
162200080627         write mk50s01     ;
162300080627         protriga=*off     ;
162400080627         Ifosott=*off      ;
162500081007         clear vscfsn      ;
162600100507         // Se la prima riga è un commento, posizionamento cursore sulla seconda
162700100507         if s01nrr=1   ;
162800100507          c01csr= 2 ;
162900100507         endif   ;
163000100507
163100081007       endif               ;
163200081007       endif               ;
163300080627
163400080627       Indx=%lookup(wifo:ifo)   ;
163500080627       if Indx>0                ;
163600080627         difo=ifods(Indx)         ;
163700080627         vscifo=ifo(Indx)       ;
163800080702
163900080702         // Dati da tabella
164000080702          vshobl=§ifoobl  ;
164100081015          vshie =§ifotip  ;
164200080704
164300080702          vshann=§ifoann  ;
164400080704
164500081003         // VALORE
164600081003          clear vshsnv  ;
164700081003          clear vsctva  ;
164800081003          if §ifosnv<>' '  ;
164900081003          vshsnv=§ifosnv  ;
165000081003          // tipo valore
165100081003          vsctva=§ifotva ;
165200080703          endif   ;
165300140715
165400140715         //?VALORE con DECIMALE
165500140715          IF  §ifosnvd <> *blanks;
165600140715            vshsnvd = §ifosnvd;
165700140715          // tipo valore
165800140715            vsctva = §ifotva;
165900140715          ENDIF;
166000080704
166100081003         // SPEDIZIONI
166200080702          vshsns=§ifosns  ;
166300080704
166400081003         // FATTURATO
166500080702          vshsnf=§ifosnf  ;
166600081003
166700081003         // SI/NO
166800081003          vshsnE=§ifosnE  ;
166900081003
167000080702          vshifgv=§ifgvis  ;
167100081007          clear vscint    ;
167200080627
167300080627         // Record senza sottotipo
167400081006         select    ;
167500081006         when §ifoifs=' '    ;
167600080702            vsdifo=§ifodes         ;
167700080703            if Ifosott=*on    ;
167800080703               vsdifo=%trim(vsdifo)+':'    ;
167900080703            endif    ;
168000080702            chain    (i50cpo:wifo) ticpi01l  ;
168100081126            if %found(ticpi01l)   ;
168200081126             trovacpi='S'    ;
168300081126             else ;
168400081126             trovacpi='N'    ;
168500081126             endif  ;
168600080702            EXSR CaricaInfo        ;
168700081006
168800081006         // Record con   sottotipo da caricare tutti
168900081006         when   §ifoifs='S' and §ifoifv='S';
169000080627            EXSR CARCon    ;
169100081006
169200081006         // Record con   sottotipo da caricare col '?'
169300081006         //   la categoria visualizzata in questo caso è obbligatoria
169400081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
169500081006         //  il '?'
169600081006         when   §ifoifs='S' and §ifoifv='?';
169700081006          vshifgv=§ifoifv  ;
169800081215          yy=1        ;
169900081126
170000081126          if §ifonrec=0 ;
170100081126             §ifonrec=1  ;
170200081126          endif     ;
170300081126
170400081126          setll (i50cpo:wifo) ticpi01l  ;
170500081126          dow yy<=§ifonrec  ;
170600081006            EXSR CarconUNA            ;
170700081126            yy=yy+1  ;
170800081126          enddo    ;
170900081126
171000081006         endsl ;
171100080703
171200080703         endif ;
171300080702         endif ;
171400080702
171500080702         xx=xx+1   ;
171600080702         enddo     ;
171700080702         ENDSR    ;
171800080627
171900080702       //--------------------------------------------------------------
172000081006       //?Carico con   sottotipo  carica riga per riga
172100080702       //--------------------------------------------------------------
172200080702       BEGSR  CARcon            ;
172300080702
172400081126         tbeke1=wifo   ;
172500080925
172600081006         setll ('IFS':tbeke1) tntbe01l   ;
172700081006         READE ('IFS':tbeke1) tntbe01l   ;
172800080702
172900080702         dow not %eof(tntbe01l)   ;
173000080702         if tbeatb=' '    ;
173100081006         difs=tbeuni      ;
173200081006         wifs=tbeke2      ;
173300080702         vscspp=tbeke2      ;
173400081127         vshspp=tbeke2      ;
173500081007         vsdifo=§ifsdes  ;
173600110707         vshann=§ifsann  ;
173700080702
173800081006         chain    (i50cpo:wifo:wifs) ticpi01l  ;
173900081126            if %found(ticpi01l)   ;
174000081126             trovacpi='S'    ;
174100081126             else ;
174200081126             trovacpi='N'    ;
174300081126             endif  ;
174400080702         EXSR CaricaInfo    ;
174500080925         endif   ;
174600080702
174700081006         READE ('IFS':tbeke1) tntbe01l   ;
174800080702         enddo   ;
174900080702
175000080702       ENDSR;
175100081006       //--------------------------------------------------------------
175200081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
175300081006       //--------------------------------------------------------------
175400081006       BEGSR  CARconUNA         ;
175500081007
175600081007       vscint='?'    ;
175700081007
175800081126       reade    (i50cpo:wifo) ticpi01l  ;
175900081006
176000081126 1     if not %eof(ticpi01l)   ;
176100081126         trovacpi='S'    ;
176200081126
176300081006         vscspp=cpispf   ;
176400081127         vshspp=cpispf   ;
176500081006
176600081126         tbeke1=wifo    ;
176700081126
176800081006         tbeke2=cpispf  ;
176900081006         chain ('IFS':tbeke1:tbeke2) tntbe01l   ;
177000081006 2       if not %found(tntbe01l)    ;
177100081006         clear tbeuni       ;
177200081006 2       endif    ;
177300081007         difs=tbeuni     ;
177400081006
177500081007         vsdifo=§ifsdes   ;
177600081007         vsdifs=§ifsdel   ;
177700081007
177800081006         EXSR CaricaINFO  ;
177900081006
178000081006 1x      else    ;
178100081126         trovacpi='N'    ;
178200081006
178300081126         // se non trovato dicitura generica da tabella IFO
178400081126         if wifo='MOI'  ;
178500081126           vsdifo=coRicercaMOI    ;
178600081126           else  ;
178700081126           vsdifo=coRicerca       ;
178800081126         endif  ;
178900081007         clear vsdifs      ;
179000081126         clear vscspp      ;
179100081127         clear vshspp      ;
179200081006
179300081006         EXSR CaricaINFO   ;
179400081006         endif             ;
179500081006
179600081006       ENDSR;
179700080702       //--------------------------------------------------------------
179800081006       //?Carica ogni riga di info commerciale
179900080702       //--------------------------------------------------------------
180000080702         BEGSR CaricaINFO             ;
180100080925         clear pftblu   ;
180200080925
180300081126         if trovacpi='S'                             ;
180400080703          vshimm=' '        ;
180500080702          vscfsn=cpifsn   ;
180600081003          vscval=cpival   ;
180700081007          if vshifgv='?'   and vscspp>*zeros    ;
180800081007            w0040=%int(vscspp)  ;
180900081007            vscval=w0040     ;
181000081007            clear vscfsn     ;
181100081007          endif            ;
181200080702          vscsna=cpisna   ;
181300080702          vscpft=cpipft   ;
181400080925          if vscpft>0     ;
181500080925          vshprpft='S'    ;
181600080925          endif           ;
181700140717          //vscvft=cpivft   ;
181800080702          Dataiso=%date(cpidim:*iso)  ;
181900080702          datadmy=Dataiso ;
182000080702          vscdim=%dec(datadmy)  ;
182100081003          clear vscfil  ;
182200081003          if cpifil>0   ;
182300081003          vscfil=%editc(cpifil:'X')   ;
182400081003          endif         ;
182500081003
182600081003          if cpipru<>*blanks ;
182700080702          vscpru=cpipru   ;
182800081003          vscdpru='Utente' ;
182900081003          else  ;
183000081003          clear vscpru   ;
183100081003          clear vscdpru   ;
183200081003          endif     ;
183300080925
183400080925          exsr ViSUALimp   ;
183500140715
183600140715         //?Calcolo il ricavo medio spedizione 'RMS'
183700140715           IF  VSCifo = 'SPT' and VSCpft <> 0 and VSCsna <> 0;
183800140715             wVALrms = VSCpft / VSCsna;
183900140929             IF  wVALrms > 999;
184000140929               wVALrms = 999;
184100140715             ENDIF;
184200140715           ENDIF;
184300080925
184400080702         else             ;
184500080703          vshimm='S'      ;
184600080703          Protann=*on     ;
184700080702          clear vscfsn    ;
184800081003          clear vscval    ;
184900080702          clear vscsna    ;
185000080702          clear vscpft    ;
185100140717          //clear vscvft    ;
185200080702          clear vscdim    ;
185300080702          clear vscfil    ;
185400080702          clear vscpru    ;
185500081003          clear vscdpru    ;
185600080925          clear vshprpft  ;
185700140715
185800140715         //?Se info 'RMS' imposto il dato
185900140715           IF  VSCifo = 'RMS';
186000140715             VSCvald = wVALrms;
186100140715           ENDIF;
186200140715
186300080702         endif            ;
186400081006
186500080925         exsr ProtCAMPI    ;
186600080703
186700080703         // I record da annullare li visualizzo solo se ci sono
186800110707           if vshann=' ' or (vshann<>' '  and vscfsn<>' ') or
186900110707                             (vshann<>' '  and vscpft>0)   or
187000110707                             (vshann<>' '  and vscval>0   )  ;
187100081003
187200081003         // In interrogazione visulizzo solo i record pieni col sottotipo
187300081003           if i50mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
187400080703            s01nrr=s01nrr+1   ;
187500080703            write mk50s01     ;
187600140718
187700140919         // Mi salvo i nr di rcd del non affidato a BRT
187800140721            IF  VSCifo = 'NRF' and ifoNRFnrr = 0;
187900140718              ifoNRFnrr = S01nrr;
188000140721            ENDIF;
188100140721            IF  VSCifo = 'NRO' and ifoNROnrr = 0;
188200140721              ifoNROnrr = S01nrr;
188300140721            ENDIF;
188400140721            IF  VSCifo = 'NER' and ifoNERnrr = 0;
188500140721              ifoNERnrr = S01nrr;
188600140721            ENDIF;
188700140721            IF  VSCifo = 'NAE' and ifoNAEnrr = 0;
188800140721              ifoNAEnrr = S01nrr;
188900140721            ENDIF;
189000140725            IF  VSCifo = 'NTR' and ifoNTRnrr = 0;
189100140725              ifoNTRnrr = S01nrr;
189200140725            ENDIF;
189300080925
189400080925         // Se info SPT - totale spesa trasporti,  la memorizzo
189500100302         //  per il calcolo delle %  e per vedere se modificata
189600080925         if vscifo='SPT' ;
189700080925          SPTPFT=vscpft   ;
189800100302          savSPT=vscpft   ;
189900080925         endif           ;
190000100302         //  per vedere se modificata
190100100302         if vscifo='ATR' ;
190200100302          savATR=vscval   ;
190300100302         endif           ;
190400120105
190500120105         //?Memorizzo la % di affidamento a BRT
190600120110           IF  VSCifo = '10' and VSCspp = '_BAR';
190700120105           //?Se devo aumentarla calcolo almeno 1% in +
190800120110             IF  wAumenta10 and VSCval > 0;
190900120111               sav_bar_10 = VSCval + 1;
191000120105             ENDIF;
191100120510             IF  (wAumenta10 or wAumenta10f) and VSCval = 0;
191200120110               sav_bar_10 = 1;
191300120110             ENDIF;
191400120105           //?Se devo dominuirla calcolo almeno 1% in -
191500120110             IF  wDiminuisci10 and VSCval > 0;
191600120111               sav_bar_10 = VSCval - 1;
191700120105             ENDIF;
191800120105           ENDIF;
191900080925
192000080703           endif ;
192100081003           endif ;
192200081003
192300080702       ENDSR;
192400080702       //--------------------------------------------------------------
192500080702       //?Gestione protezione campi del sfl
192600080702       //--------------------------------------------------------------
192700080702          BEGSR   ProtCampi   ;
192800080703           protfat=*off    ;
192900081003           protVal =*off   ;
193000140715           protVald = *off;
193100140715           protVala = *off;
193200080703           protsped=*off   ;
193300080703           protfsn =*off   ;
193400080703           Ifosott=*off    ;
193500080703           Protann=*off ;
193600080704           Infoyell=*off ;
193700080703
193800081007           // Se riga di categoria accendo solo protezione e sottolineatura
193900081007          if vshimm='C'    ;
194000081007           Ifosott=*on     ;
194100081007           Protriga=*on    ;
194200081007          else             ;
194300081007
194400080702          // in base a cosa prevede la tabella, proteggo i campi
194500080702          if vshifgv='N' ;
194600080702            Ifosott=*on       ;
194700080702          endif    ;
194800080702
194900080704          if vshann<>' '  ;
195000080702           protfat=*on     ;
195100081003           protVal =*on    ;
195200140715           protVald = *on;
195300140715           protVala = *on;
195400080702           protsped=*on    ;
195500080702           protfsn =*on    ;
195600080704           infoyell=*on    ;
195700080702          else            ;
195800081006          protann=*on     ;
195900081031
196000081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
196100081103          if vscpft>0 and vshsnf=' ' and vscval=0;
196200081031          protann=*off    ;
196300081031          endif    ;
196400081031          if vscsna>0 and vshsns=' ' ;
196500081031          protann=*off    ;
196600081031          endif    ;
196700081127          if vscval>0 and vshsnv=' ' and vshifgv<>'?'       ;
196800081031          protann=*off    ;
196900081031          endif    ;
197000140715          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
197100140715            protann = *off;
197200140715          ENDIF;
197300081031          if vscfsn<>' ' and vshsne=' ' ;
197400081031          protann=*off    ;
197500081031          endif    ;
197600081006
197700080703            if vshsnf=' ' ;
197800080702             protfat=*on     ;
197900080702            endif           ;
198000081003            if vshsnv=' ' ;
198100081003             protVal =*on     ;
198200080702            endif           ;
198300140715            IF  vshsnvd = *blanks;
198400140715              protVald = *on;
198500140715            ENDIF;
198600140715            if vshsnvd='A';
198700140715             protVala=*on     ;
198800140715            endif           ;
198900080703            if vshsns=' ' ;
199000080702             protsped=*on     ;
199100080702            endif           ;
199200081003            if vshsne=' ' ;
199300081003             protfsn =*on     ;
199400081003            endif           ;
199500080702          endif           ;
199600080703          // Se record di nuova immissione
199700081003          //  proteggo annullamento
199800080703            if vshimm='S'  ;
199900080703             Protann=*on  ;
200000080703           endif   ;
200100081006
200200081006          // Se prevista scelta sottotipo col'?' visualizzo valore
200300081006          //   col codice e vicino il '?'
200400081006          if vshifgv='?'    ;
200500081006             protVal =*off    ;
200600081007             if vscspp>*zeros    ;
200700081007             else          ;
200800081007             infoyell=*on    ;
200900081007             endif            ;
201000081006          endif               ;
201100081007          endif               ;
201200080703
201300080702          ENDSR    ;
201400080702       //--------------------------------------------------------------
201500080702       //?conferma aggiornamento
201600080702       //--------------------------------------------------------------
201700080702        BEGSR ConfAGGIO              ;
201800090520        clear ultdata   ;
201900081003
202000080704       s01nrr=1  ;
202100080704       chain s01nrr    mk50s01    ;
202200080704
202300080704 1     dow %found    ;
202400080704       // Escludo i record di descrizione categoria
202500140715 2     //if vshimm<>'C'    ;
202600140715 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
202700080925
202800080925       // se valori calcolati da pgm li pulisco prima dei controlli
202900080925       if vshsnf=' ' and vshprpft=' '   ;
203000080925       clear vscpft   ;
203100140717       //clear vscvft   ;
203200080925       endif          ;
203300080925
203400081127       IF VShifgv<>'?'    ;
203500081127         chain    (i50cpo:vscifo:vscspp) ticpi01l  ;
203600081127       else   ;
203700081127         chain    (i50cpo:vscifo:vshspp) ticpi01l  ;
203800081127       endif   ;
203900081127             if %found(ticpi01l)   ;
204000081127             trovacpi='S'    ;
204100081127             else ;
204200081127             trovacpi='N'    ;
204300081127             endif  ;
204400081127
204500081127       // A n n u l l a m e n t o oppure per qualche strano motivo
204600081127       //   la info c'e'  ma a video non è stata inserita
204700080925
204800081127 3     if trovacpi='S'      and (vscatb='A'  or (vscfsn=' ' AND
204900081003                                 vscpft=0 and vscsna=0 and vscval=0));
205000081003         delete ticpi000  ;
205100081003 x3    else    ;
205200080704
205300081127       // V a r i a z i o n e :  verifico se modificati dei dati
205400081127 31    if trovacpi='S'     and vscatb=' '   ;
205500080704 4     if (cpifsn<>vscfsn) or
205600081003          (cpival<>vscval) or
205700080704          (cpisna<>vscsna) or
205800081007          (cpispf<>vscspp) or
205900140717          (cpipft<>vscpft) ;
206000140717          //(cpivft<>vscvft) ;
206100080704          // reimposto data ora profilo immissione
206200080704          cpidim=*date    ;
206300090520          ultdata=*date    ;
206400080704          cpifil=utefil   ;
206500080704          cpipru=knmus    ;
206600080704
206700081007          cpifsn= vscfsn  ;
206800081007          IF VShifgv<>'?';
206900081003          cpival= vscval  ;
207000081003          cpitva= vsctva  ;
207100081007          else            ;
207200081007          cpispf=vscspp   ;
207300081007          if cpispf<>*blanks ;
207400081007          cpifsn='S'      ;
207500081007          ENDIF           ;
207600081007          ENDIF           ;
207700080704          cpisna= vscsna  ;
207800080704          cpipft= vscpft  ;
207900140717          cpivft= 'EUR'   ;
208000081006          // verifico se dei dati non ci voglio più: una volta aggiunti
208100080704          //  quelli previsti, lo tolgo
208200080704
208300081003 5        if (cpival>0 and vshsnv=' ')    ;
208400081006          if (cpisna>0 and vshsns<>' ')  or (cpipft>0  and vshsnf<>' ') or
208500081006             (cpifsn<>' ' and vshsne<>' ')   ;
208600081003          clear cpival   ;
208700080704          endif          ;
208800080704 5        endif          ;
208900080704 5        if (cpisna>0 and vshsns=' ')    ;
209000081006          if (cpival>0 and vshsnv<>' ')  or (cpipft>0  and vshsnf<>' ') or
209100081006             (cpifsn<>' ' and vshsne<>' ')   ;
209200080704          clear cpisna   ;
209300080704          endif          ;
209400080704 5        endif          ;
209500080704 5        if (cpipft>0 and vshsnf=' ')    ;
209600081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
209700081006             (cpifsn<>' ' and vshsne<>' ')   ;
209800080704          clear cpipft   ;
209900080704          clear cpivft   ;
210000080704          endif          ;
210100080704 5        endif          ;
210200081006 5        if (cpifsn<>' ' and vshsne=' ')    ;
210300081006          if (cpival>0 and vshsnv<>' ')  or (cpisna>0  and vshsns<>' ') or
210400081006             (cpipft>0  and vshsnf<>' ')     ;
210500081006          clear cpifsn   ;
210600081006          endif          ;
210700081006 5        endif          ;
210800080704
210900080704        update ticpi000  ;
211000080704 4      endif  ;
211100081003 3a     endif  ;
211200080704
211300081127       // I m m i s s i o n e
211400081127 3     if trovacpi='N'     and vscatb=' ' and (vscfsn<>' ' or
211500081003       vscpft>0 or vscsna>0 or vscval>0) ;
211600080704       clear ticpi000;
211700080704        cpicpo=i50cpo   ;
211800080704        cpitpf=vscifo  ;
211900080704        cpispf=vscspp  ;
212000080704        cpifsn=vscfsn ;
212100081007        if vshifgv<>'?';
212200081003        cpival= vscval  ;
212300081003        cpitva= vsctva  ;
212400081007        else            ;
212500081007          if cpispf<>*blanks ;
212600081007          cpifsn='S'      ;
212700081007          endif           ;
212800081007        endif           ;
212900081007
213000080704        cpisna= vscsna  ;
213100080704        cpipft= vscpft  ;
213200140717        cpivft= 'EUR'   ;
213300080704        cpidim=*date    ;
213400090520        ultdata=*date    ;
213500080704        cpifil=utefil   ;
213600080704        cpipru=knmus    ;
213700080704
213800080704        write ticpi000 ;
213900081003  3a    endif           ;
214000081003  3     endif           ;
214100080704  2     endif           ;
214200080704
214300080704        s01nrr=s01nrr+1 ;
214400080704        chain s01nrr    mk50s01    ;
214500080704  1     enddo           ;
214600140929
214700140929       //?Se affidato a BRT è 0
214800140929       //?Se ripartizione della spesa è 100%
214900140929       //?Se non affidato a BRT non è presente
215000140929       //?creo in automatico il NON affidato a BRT
215100140929         IF  BART_10 = 0 and
215200140929             (savPRFval + savPROval + savAERval +
215300140929              savCAEval + savATRval) = 100 and
215400140929             noBRTpft = 0;
215500140929           exsr NoAffidatoBRT;
215600140929         ENDIF;
215700080704
215800081015        clear waggiocpo    ;
215900081015
216000081003       // Aggancio tncpo per aggiornare se ho tutte le info
216100081003       chain   i50cpo   tncpo01l   ;
216200081015  1      if %found(tncpo01l)    ;
216300081015         chain  cp1nrr    tncpo00f   ;
216400081015  1      endif                  ;
216500081003  1      if %found(tncpo00f)   ;
216600081015
216700081015         // Se inserimento obbligatorio--> posso impostare la "S"
216800081015         //  altrimenti controllo da CPI
216900081003
217000081015  2       if i50obl=' '   ;
217100081015            // Rifaccio il controllo del sfl come se fosse
217200081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
217300081215            savmod=i50mod   ;
217400081215            i50mod='C'      ;
217500081215            i50obl='S' ;
217600081215
217700081015            exsr contrs01   ;
217800081215
217900081215            i50obl=' ' ;
218000081215            i50mod=savmod   ;
218100081015
218200081015  3         if ErrGenerico=*off;
218300081215              Waggiocpo='S';
218400081015  3         endif  ;
218500081015
218600081015  x2      else;
218700081015          Waggiocpo='S'   ;
218800081015  2       endif           ;
218900081015
219000081015              dcpo01=cporst    ;
219100081015              §cpoifotot=Waggiocpo   ;
219200081015              o50ifotot=Waggiocpo   ;
219300090520              if ultdata > 0   ;
219400090520                §cpoifodul=%editc(ultdata:'X')  ;
219500090520              endif   ;
219600090520              // Se inserita memorizzo la spesa trasporti reale
219700090520              if sptpft>0   ;
219800090520              §cposptp=sptpft   ;
219900090520              endif   ;
220000100302
220100100302              //  Se variata spesa trasporti o % ATR, ricalcolo cod importanza potenziale
220200100302             if savSPT<> SPTPFT or savatr<>infoATR   ;
220300100302             clear trmk52ds  ;
220400100302             i52cpo=cpocpo  ;
220500100302             i52spt= SPTpfT ;
220600100302             i52ATR=infoATR  ;
220700100302
220800100302             callp TRMK52R  (trmk52ds)  ;
220900100302             cpoftr=o52cimp  ;
221000100302             endif   ;
221100100302
221200081015              cporst=dcpo01    ;
221300081015              update tncpo000  ;
221400081003  1    endif   ;
221500140919
221600140725       // devo aggiornare la data ultima conferma INFO COMM.
221700140909         IF  not InfoInt;
221800140725           chain i50cpo TNCPO11L;
221900140725  1        IF  %found(TNCPO11L);
222000140909             IF  ultdata > 0;
222100140909               CPO1ducifo = ultdata;
222200140909             ELSE;
222300140909               CPO1ducifo = *date;
222400140909             ENDIF;
222500140725             update TNCPO100;
222600140725           ENDIF;
222700140909           IF  not %found(TNCPO11L);
222800140909             clear TNCPO100;
222900140909             CPO1cpo = CPOcpo;
223000140909             IF  ultdata > 0;
223100140909               CPO1ducifo = ultdata;
223200140909             ELSE;
223300140909               CPO1ducifo = *date;
223400140909             ENDIF;
223500140909             write TNCPO100;
223600140909           ENDIF;
223700140725         ENDIF;
223800140725
223900080702        ENDSR;
224000140919
224100140919       //--------------------------------------------------------------
224200140919       //?Creo le info di NON Affidato a BRT.
224300140919       //--------------------------------------------------------------
224400140919        BEGSR NoAffidatoBRT;
224500140919
224600140919          clear TICPI000;
224700140919          CPIcpo = I50CPO;
224800140919          CPIvft = 'EUR';
224900140919          CPIdim = *date;
225000140919          CPIfil = utefil;
225100140919          CPIpru = knmus;
225200140919
225300140919          IF  savPRFval > 0 and savNRFpft = 0;
225400140919            CPItpf = 'NRF';
225500140919            CPIpft = (SPTpft * savPRFval / 100);
225600140919            write TICPI000;
225700140919          ENDIF;
225800140919
225900140919          IF  savPROval > 0 and savNROpft = 0;
226000140919            CPItpf = 'NRO';
226100140919            CPIpft = (SPTpft * savPROval / 100);
226200140919            write TICPI000;
226300140919          ENDIF;
226400140919
226500140919          IF  savAERval > 0 and savNERpft = 0;
226600140919            CPItpf = 'NER';
226700140919            CPIpft = (SPTpft * savAERval / 100);
226800140919            write TICPI000;
226900140919          ENDIF;
227000140919
227100140919          IF  savCAEval > 0 and savNAEpft = 0;
227200140919            CPItpf = 'NAE';
227300140919            CPIpft = (SPTpft * savCAEval / 100);
227400140919            write TICPI000;
227500140919          ENDIF;
227600140919
227700140919          IF  savATRval > 0 and savNTRpft = 0;
227800140919            CPItpf = 'NTR';
227900140919            CPIpft = (SPTpft * savATRval / 100);
228000140919            write TICPI000;
228100140919          ENDIF;
228200140919
228300140919        ENDSR;
228400080702
228500080206       //--------------------------------------------------------------
228600080206       //?Operazioni finali.
228700080206       //--------------------------------------------------------------
228800080206       BEGSR RoutEnd;
228900080627
229000080704         if i50tla<>' '    ;
229100080704         // Chiusura pgm   ;
229200080704         clear tibs02ds  ;
229300080704         t02tla='C'      ;
229400080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
229500080704
229600080206         *inLR = *on;
229700080704         endif             ;
229800080704
229900080206         return;
230000080206
230100080206       ENDSR;
230200080206
230300080206      /end-free
230400080206
230500080206       //--------------------------------------------------------------
230600080206       //?Schiere a tempo di compilazione.
230700080206       //--------------------------------------------------------------
230800080206
230900080410** - MSG ------------------------------------------------------------------ -*
231000081003Info Commerciale obbligatoria                                                   01
231100080703Per Info Comm. non presente (N), non indicare gli altri dati                   02
231200081003La Info Comm. prevede l'inserimento del Fatturato presunto OBBLIGATORIO        03
231300081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx: enter per forzare     04
231400081003La  Info Comm. prevede l'inserimento del Fatturato presunto: enter per forzare    05
231500080704La  Info Comm. prevede l'inserimento del numero Spedizioni: enter per forzare  06
231600080704Info Comm. non più in essere:verificarla e se non serve annullarla.ENTER forza 07
231700080704Info Comm. non più in essere: ANNULLARLA !!                                    08
231800081003La somma delle percentuali per questa categoria di info commerc. supera 100%   09
231900081003La Info Comm. prevede l'inserimento del numero Spedizioni OBBLIGATORIO         10
232000081003La Info Comm. prevede l'inserimento xxxxxxxxxxxxxxxxxx OBBLIGATORIO            11
232100081003La  percentuale di questa info commerciale supera 100%                         12
232200081003La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx  deve essere 100% 13
232300081007Inserire concorreti Logistica SOLO SE il cliente prevede Logist.in outsourcing 14
232400081007Se Logistica in outsourcing obbligatorio inserire i concorrenti logistica      15
232500110511"Motivo non affidamento a noi" da immettere solo se "Affidato a BRT" < 100%    16
232600140716ATTENZIONE!! Il valore massimo consentito è di 999 milioni                     17
232700081007"MOTIVO NON AFFIDAMENTO A NOI" errato                                          18
232800081007Indicare OBBLIGATORIAMENTE la presenza (S) o assenza(N) della Info commerciale 19
232900081007Immettere solo  '?'  se si vuole attivare la ricerca                           20
233000081125Immettere "S o N" per indicare la presenza o l'assenza della Info Comm.        21
233100081007Immettere"S o N"per indicare la presenza o l'assenza della Info Comm:ENTR forza22
233200140717ATTENZIONE!! Il valore inserito e > di 1 milione - Enter per forzare.          23
233300140716Il numero delle spedizioni non può essere superiore alla spesa trasporti.      24
233400140716Non ammesso un valore negativo                                                 25
233500081125Se "Logistica interessante per noi", indicare se in Outsourcing                26
233600110511Se non tutto "Affidato BRT" obbligatorio un Motivo NON AFFIDAMENTO a noi       27
233700081126I"Motivi non Affidamento a noi" devono essere diversi: uno per ITA uno per EST 28
233800081126Se immesso"9999-NON Rilevabile", l'altro motivo deve essere vuoto              29
233900081126Codice già presente !!                                                         30
234000120125Obbligatorio aumentare la % di affidamento a BRT!!                             31
234100120109Alcune Info sono più vecchie di 1 anno. Verificare se ancora corrette.         32
234200120125Obbligatorio azzerare la % di affidamento a BRT!!                              33
234300120125Obbligatorio diminiuire la % di affidamento a BRT!!                            34
234400140715Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                 35
234500140715Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!                36
234600140725Inserire Paesi Esteri SOLO SE il cliente prevede trasporto Estero.             37
234700140922Se non tutto "Affidato BRT" obbligatorio ripartizione spesa NON affidato a BRT 38
234800140717ATTENZIONE!! Il valore inserito e > di 10.000.                                 39
234900140919Non affidato a BRT non congruente con % utilizzo concorrenti                   40
235000140919Non affidato a BRT non congruente con % ripartizione spesa trasporti           41
235100140919% Ripartizione Spesa Trasp. a 0. Non impostare il relativo non affidato a BRT  42
