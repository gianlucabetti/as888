000100080206      //--------------------------------------------------------------
000200100302      //?TNTA41R - Gestione/interrogazione informazioni commerciali
000300100303      //
000400100303      // ? ATTENZIONE!!!!! ?
000500100303      // ?  CHIODI fissi per:
000600100303      // ?    info '10'  concorrenti
000700100303      // ?    info 'DEL' delta
000800101215      // ?    info 'TRT' Traffico totale
000900110110      // ?    info 'A/S' Aumento/Sconto
001000110117      // ?    info 'KMS' Peso medio
001100110117      // ?    info 'TRE' Traffico estero
001200080206      //--------------------------------------------------------------
001300080206
001400080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001500080206
001600080206      //---------------------------------------------------------------
001700080206      //?Dichiarazione file.
001800080206      //---------------------------------------------------------------
001900050704
002000100304     fTIVII01L  uf a e           k disk
002100141106     fTIVIS05L  if   e           k disk
002200100304     fTNTBE01L  if   e           k disk
002300100302     fTNTA41D   cf   e             workstn indds(IndDspF)
002400080206     f                                     infds(InfDspF)
002500100302     f                                     sfile(TA41S01 : S01nrr)
002600080206      //---------------------------------------------------------------
002700080207      // - Tasti funzionali a video
002800080207     d c_F01           c                   const(x'31')
002900080207     d c_F02           c                   const(x'32')
003000080207     d c_F03           c                   const(x'33')
003100100303     d c_F04           c                   const(x'34')
003200080207     d c_F05           c                   const(x'35')
003300080207     d c_F06           c                   const(x'36')
003400080207     d c_F07           c                   const(x'37')
003500080207     d c_F08           c                   const(x'38')
003600080207     d c_F09           c                   const(x'39')
003700080207     d c_F10           c                   const(x'3A')
003800100303     d c_F11           c                   const(x'3B')
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
005500141106
005600141118     d FaseObjTtr      c                   const(' TR')
005700080206
005800080206      //---------------------------------------------------------------
005900080206      //?Definizione schiere.
006000080206      //---------------------------------------------------------------
006100080206
006200080206      // - Messaggi di errore
006300100304     d msg             s             78    dim(30) ctdata perrcd(1)
006400100302     d ito             s              3    dim(50)
006500100302     d itods           s             70    dim(50)
006600100302     d itoor           s              8    dim(50)
006700100303     d itoval          s              8    dim(100)
006800100303     d its             s              4    dim(99)
006900081126
007000080206      //---------------------------------------------------------------
007100080206      //?Definizione aree dati.
007200080206      //---------------------------------------------------------------
007300100226
007400080206      // - Dati utente
007500080206     d §AzUte        e ds                  extname(AZUTE00F)
007600080206     d                                     dtaara
007700080206     d §DatiUte      e ds                  extname(dDatiUte)
007800080206     d                                     dtaara
007900080206
008000080206      //---------------------------------------------------------------
008100080206      //?Definizione strutture dati.
008200080206      //---------------------------------------------------------------
008300080206
008400080206      // - InfDS
008500080206     d InfDspF         ds
008600080207     d  dsp_aid              369    369a
008700080206
008800080206      // - Indicatori su DspF
008900080208     d IndDspF         ds
009000080206        // - Indicatori di gestione del subfile
009100080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009200080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009300080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009400080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009500080206        // - Indicatori di errore
009600100304     d  ErrMessage                    1n   overlay(IndDspF : 28)
009700100304     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800080627        // - Indicatori vari
009900080627     d  InfoInt                       1n   overlay(IndDspF : 02)
010000080704     d  Infoyell                      1n   overlay(IndDspF : 03)
010100081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
010200080627     d  InfoImmiss                    1n   overlay(IndDspF : 10)
010300080703     d  ProtAnn                       1n   overlay(IndDspF : 13)
010400100302     d  ifosott                       1n   overlay(IndDspF : 15)
010500080627     d  protriga                      1n   overlay(IndDspF : 16)
010600080627     d  protFat                       1n   overlay(IndDspF : 17)
010700081003     d  protVal                       1n   overlay(IndDspF : 18)
010800080627     d  protSped                      1n   overlay(IndDspF : 19)
010900080702     d  protfsn                       1n   overlay(IndDspF : 20)
011000080925     d  PFTBLU                        1n   overlay(IndDspF : 21)
011100100423     d  protSGN                       1n   overlay(IndDspF : 22)
011200101214     d  protSGNa                      1n   overlay(IndDspF : 23)
011300110406     d  protVald                      1n   overlay(IndDspF : 24)
011400111012     d  protvala                      1n   overlay(IndDspF : 25)
011500080703     d  ErrSNA                        1n   overlay(IndDspF : 40)
011600080703     d  ErrFsn                        1n   overlay(IndDspF : 41)
011700081003     d  ErrVAL                        1n   overlay(IndDspF : 42)
011800110406     d  ErrVALD                       1n   overlay(IndDspF : 43)
011900080703     d  ErrPFT                        1n   overlay(IndDspF : 44)
012000080704     d  ErrANN                        1n   overlay(IndDspF : 45)
012100080703     d  PotAnnul                      1n   overlay(IndDspF : 70)
012200080206
012300080206      // - Parametri ricevuti
012400080206     d KPJBA         e ds
012500100302     d TNTA41DS      e ds
012600141106
012700141106      // - Pgm Cliente in Campagna
012800150114     d TRKC73DS      e ds                  inz
012900150206     d  skOKC73ncm            24     93  0 inz  dim(10)
013000141106
013100141106      // - Pgm Scrive fase se cliente in Campagna
013200150114     d TRKC71DS      e ds                  inz
013300080206
013400080206      // - Ricerca/Controllo tabelle
013500080206     d TIBS02ds      e ds                  inz
013600100304
013700100304      // - Reperimento dati utente
013800100304     d TIBS34DS      e ds
013900080206
014000100304      // - Tabella  Generica azienda
014100100304     d Dged          e ds
014200100304
014300080627      // - Tabella  Info commerciali e Categorie info
014400100304     d dITG          e ds                  inz
014500100304     d dITO          e ds                  inz
014600100304     d dITS          e ds                  inz
014700100302
014800080206      //---------------------------------------------------------------
014900080206      //?Definizione variabili globali.
015000080206      //---------------------------------------------------------------
015100080206
015200080206      // - Flags booleani
015300141106     d wcampagna       s               n   inz(*off)
015400080208     d $Fine           s               n   inz(*off)
015500080208     d $InzS01         s               n   inz(*on)
015600111012     d $ForzaKg        s               n   inz(*off)
015700111012     d $ForzaDel       s               n   inz(*off)
015800111012     d $ForzaRms       s               n   inz(*off)
015900080206
016000080627     d $Video          s              2    inz('S1')
016100080208     d S01nrr          s              4  0 inz
016200080627     d savcat          s              2    inz
016300080627     d Wcat            s              2    inz
016400100302     d Wits            s              4    inz
016500100302     d Wito            s              3    inz
016600081006     d w0040           s              4  0 inz
016700081007     d w005a           s              5    inz
016800081006     d XX              s              3  0 inz
016900080627     d Indx            s              3  0 inz
017000080703     d Primavolta      s              1    inz
017100080925     d Totprc          s              4  0 inz
017200081003     d Wctrprc         s              1    inz
017300081003     d Wdescat         s             21    inz
017400100302     d SPTPFT          s                   like(viipft) inz
017500100302     d Waggiovis       s              1    inz
017600081126     d w008a           s              8    inz
017700081126     d yy              s              3  0 inz
017800100302     d trovavii        s              1    inz
017900100302     d savmod          s                   like(I41mod) inz
018000090520     d ultdata         s              8  0 inz
018100100302     d $okUNA          s              1n   inz(*off)
018200100305     d $okqualcosa     s              1n   inz(*off)
018300100423     d WiiSGN          s              1    inz
018400101230     d wsgntrt         s                   like(vscsgn) inz
018500101230     d wsgnausc        s                   like(vscsgn) inz
018600110207     d sav_vscpft      s                   like(vscpft) inz
018700110324     d sav_vscsna      s                   like(vscsna) inz
018800111012     d sav_valkg       s                   like(vscval) inz
018900111012     d sav_valdel      s                   like(vscval) inz
019000111012     d sav_sgndel      s                   like(vscsgn) inz
019100111019     d sav_valrms      s             11  3 inz
019200111019     d wVALrms         s             11  3 inz
019300141106     d wVIIvald        s                   like(VIIvald) inz
019400101230
019500080605     d Dataiso         s               d   datfmt(*iso)
019600080605     d Datadmy         s               d   datfmt(*dmy)
019700100304
019800081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
019900080206
020000080206      //---------------------------------------------------------------
020100080206      //?Definizione procedure usate.
020200080206      //---------------------------------------------------------------
020300080414      /copy gaitrasrc/srcprotopr,tibs02r
020400080414      /copy gaitrasrc/srcprotopr,tibs34r
020500141106
020600141106      // - Pgm Cliente in Campagna
020700150114     d TRKC73R         pr                  extpgm('TRKC73R')
020800141106     d  kpjba                              likeds(kpjba)
020900150114     d  trkc73ds                           likeds(TRKC73DS)
021000141106
021100141106      // - Pgm Scrive fase se cliente in Campagna
021200150114     d TRKC71R         pr                  extpgm('TRKC71R')
021300141106     d  kpjba                              likeds(kpjba)
021400150114     d  trkc71ds                           likeds(TRKC71DS)
021500080206
021600080206      //---------------------------------------------------------------
021700080206      //?Riepilogo indicatori.
021800080206      //---------------------------------------------------------------
021900080207      // 28    : Emissione messaggio di errore a video
022000100304      // 30    : Condiziona SFLDSP
022100100304      // 31    : Condiziona SFLDSPCTL
022200100304      // 32    : Condiziona SFLNXTCHG
022300080207      // 50    : Errore: Opzione errata
022400080207      // 99    : Generico di Errore
022500080206      //---------------------------------------------------------------
022600080206
022700080206      //---------------------------------------------------------------
022800080206      //?M A I N - L I N E
022900080206      //---------------------------------------------------------------
023000080206
023100080627     c     *Entry        plist
023200080206     c                   parm                    KPJBA
023300100304     c                   parm                    TNTA41DS
023400080627
023500080206      /free
023600100303
023700100303        clear o41f12;
023800100303        clear o41f03;
023900100303        clear o41f06;
024000100303        clear o41ifotot;
024100080206
024200100302 1      if  I41tla<>'C'   ;
024300080925
024400080206       // Operazioni iniziali
024500080206       exsr RoutInz;
024600080206
024700100302 2      if  I41mod<>'C'   ;
024800081215
024900100304         // Gestione video
025000100304 3       DOW $Fine = *off;
025100100304
025200100304 4         select;
025300100304           // Videata di selezioni
025400100304             when $Video = 'S1';
025500100304               exsr GesS01;
025600111012             when $Video = 'W1';
025700111012               exsr GesW01;
025800100304             other   ;
025900100304               $Fine = *on;
026000100304 4         endsl;
026100100304
026200100304 3       ENDDO;
026300081215
026400081215 x2    else   ;
026500081215
026600100304         // modalità CONTROLLO : carico il sfl
026700100304         //                    solo ai fini del controllo
026800100304         exsr InzS01;
026900100304         exsr Contrs01   ;
027000081215
027100100304  3      if ErrGenerico=*off;
027200100304            O41ifotot='S';
027300100304  3      endif  ;
027400080925
027500081215 2     endif    ;
027600081215 1     endif    ;
027700080206
027800080206       // Operazioni finali
027900080206       exsr RoutEnd;
028000080206
028100080206       //--------------------------------------------------------------
028200080206       //?Operazioni iniziali.
028300080206       //--------------------------------------------------------------
028400080206       BEGSR RoutInz;
028500100303
028600080627       $inzs01=*on;
028700080929       clear vsctes2         ;
028800080206
028900080703       // Solo la prima volta
029000080703 1     if primavolta=' '   ;
029100080703
029200080206         // Reperimento dati job
029300080206         exsr DatiJob;
029400081003
029500081003         // Se sono EDP accendo indicatore
029600081003         if %subst(knmus:1:3)<>'EDP'    ;
029700081003         UTENonEDP=*on   ;
029800081003         endif      ;
029900080627
030000080627         // Carico tabella per divisa corrente
030100080627         reset TIBS02ds;
030200080627         T02sif = knsif;
030300080627         T02cod = 'GED';
030400080627         T02ke1 = '1'  ;
030500080627         T02mod = 'C'  ;
030600080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
030700080627
030800080703 2       if t02err=' '  ;
030900080627         dGED=t02uni    ;
031000080627         else   ;
031100080627         clear DGED    ;
031200080703 2       endif   ;
031300101215
031400101215         Primavolta='N'   ;
031500101215 1       endif      ;
031600080627
031700080627         // Carico tabella info commerciali
031800080627         xx=0                     ;
031900101215         clear ito;
032000101215         clear itods;
032100101215         clear itoor;
032200100302         setll ('ITO') tntbe01l   ;
032300100302         READE ('ITO') tntbe01l   ;
032400080627
032500101215         dow not %eof(tntbe01l)   ;
032600101215         if tbeatb=' '    ;
032700101214         //?Se tipo trattativa <> 'A' NON carico la info 'A/S'
032800101214           IF  I41tpv <> 'A' and  TBEke1 = 'A/S';
032900101214             reade ('ITO') TNTBE01L;
033000101214             iter;
033100101214           ENDIF;
033200110117         //?Se tipo trattativa = 'A' NON carico la info 'TRE' e 'KMS'
033300111012         //?e la 'RMS'
033400111012           IF  I41tpv = 'A' and  (TBEke1 = 'TRE' or TBEke1 = 'KMS' or
033500111012                                  TBEke1 = 'RMS');
033600110117             reade ('ITO') TNTBE01L;
033700110117             iter;
033800110117           ENDIF;
033900100302           dito=tbeuni    ;
034000080627           xx=xx+1        ;
034100100302           ito(xx)=tbeke1 ;
034200111024         //?Se tipo trattativa = 'A' devo modificare la descrizione x 'TRT'
034300111024           IF  I41tpv = 'A' and  TBEke1 = 'TRT';
034400111024             §ITOdes = 'Valore Aumento/Sconto';
034500111024           ENDIF;
034600100302           itods(xx)=dito ;
034700080627           // ordino in base alla categoria e  num.ordinamento
034800100302           itoor(xx)=§itoitg+%editc(§itoogi:'X')+tbeke1 ;
034900101215         endif               ;
035000080627
035100100302         READE ('ITO') tntbe01l   ;
035200101215         enddo    ;
035300080627
035400100302         sorta  itoOR  ;
035500080702
035600080703         Infoint=*off   ;
035700080704         Infoyell=*off   ;
035800080703         InfoImmiss=*off   ;
035900100302         ifosott=*off   ;
036000080703         Protriga=*off   ;
036100080703         ProtFat =*off   ;
036200081003         ProtVal  =*off   ;
036300110406         ProtVald = *off;
036400111012         ProtVala = *off;
036500100423         ProtSGN  =*off   ;
036600101214         ProtSGNa =*off   ;
036700080703         Protsped =*off   ;
036800080703         Protfsn  =*off   ;
036900080703         PotAnnul =*off   ;
037000080703         clear wcat  ;
037100100302         clear wito  ;
037200080704         $Fine=*off   ;
037300080703
037400080702         // Vedo se modo gestione  o interrogazione
037500100302 1       if I41mod='I' ;
037600080702         InfoInt =*on  ;
037700080703         vsctes2='INTERROGAZIONE'    ;
037800080703 1       endif;
037900080703
038000100302         vscpgm='TNTA41R'   ;
038100080703
038200100302         // IMPOSTO la trattativa in visualizzazione
038300100302         vscnrv=%editc(I41nrv:'Z')   ;
038400100302         vscrag=I41rag   ;
038500081003
038600100302         // Se già  inserite tutte le info, non si possono più annullare
038700081003         //  ma solo sistemare
038800100302         if I41obl=' '  and I41ifotot='S'    ;
038900100302         I41obl='S'   ;
039000081003         endif        ;
039100080627
039200100302         // VERIFICO SE ci sono già dei dati per la trattativa
039300100302         setll   I41nrv  TIVII01l    ;
039400100302         reade   I41nrv  TIVII01l    ;
039500100302 1       dow not %eof(TIVII01l) and viiatb<>' ' ;
039600100302         reade   I41nrv  TIVII01l    ;
039700080703 1       enddo   ;
039800080703
039900100302 1       if %eof(TIVII01l) ;
040000080703         Infoimmiss=*on     ;
040100080929 1       if vsctes2= *blanks    ;
040200080703         vsctes2='  IMMISSIONE  '    ;
040300080929         endif   ;
040400080703         else    ;
040500080929 1       if vsctes2= *blanks    ;
040600080703         vsctes2='  VARIAZIONE  '    ;
040700080703 1       endif    ;
040800080929 1       endif    ;
040900101214
041000101214       //?Se non è immissione e tipo trattativa <> 'A' devo deletare
041100101215       //?la info 'A/S' se sono in gestione
041200101215         IF  not InfoImmiss and I41tpv <> 'A' and I41mod = 'G';
041300101214           wito = 'A/S';
041400101214           chain (I41nrv:wito) TIVII01L;
041500101214           IF  %found(TIVII01L);
041600101214             delete tivii000;
041700101214           ENDIF;
041800101214         ENDIF;
041900080703
042000080627         ENDSR;
042100080206
042200080206       //--------------------------------------------------------------
042300080206       //?Reperimento Dati del job (Utente/Operativi).
042400080206       //--------------------------------------------------------------
042500080206       BEGSR DatiJob;
042600080206
042700080206         in(E) §AzUte;
042800080206         if NOT %error;
042900080206           in(E) §DatiUte;
043000080206         endif;
043100080206         if %error or RSut = *blanks;
043200080206           clear TIBS34ds;
043300080206           tibs34r(tibs34ds);
043400080206           in §AzUte;
043500080206           in §DatiUte;
043600080206         endif;
043700080206
043800080206       ENDSR;
043900080206
044000080206       //--------------------------------------------------------------
044100080627       //?Gestione SFL 01
044200080206       //--------------------------------------------------------------
044300080409       BEGSR GesS01;
044400080207
044500080207         // Inizializzazione videata
044600080409         if  $InzS01 = *on;
044700080409            exsr InzS01;
044800080703         // Posizionamento cursore
044900080703           s01nrr = 1;
045000080703           sfldsp_n=*off;
045100110406           $InzS01  = *off;
045200080207         endif;
045300080207
045400080207
045500080207         // Emissione Testata e Piede con tasti funzionali abilitati
045600080207         if  errGenerico = *off;
045700100302           write  TA41Z01;
045800080207         endif;
045900080703
046000080703         // Posizionamento cursore
046100080703         if  C01csr  > *zero;
046200080703           C01rcd = C01csr;
046300080703         endif  ;
046400080207
046500080207         // Emissione videata
046600100302         exfmt  TA41C01;
046700080410
046800080207         reset errMessage;
046900080207         reset errGenerico;
047000080702         clear Vscmsg;
047100081003         errVAL=*off     ;
047200110406         errVALD = *off;
047300080703         errPFT=*off     ;
047400080703         errSNA=*off     ;
047500080703         errFSN=*off     ;
047600080704         errANN=*off     ;
047700080207
047800080523 1       SELECT;
047900080207
048000080207         // - F3=Fine
048100080523 1         WHEN  dsp_aid = c_F03;
048200080409            $Fine = *on;
048300100302            O41f03='S'   ;
048400080207
048500080207         // - F12=Ritorno
048600080523 1         WHEN  dsp_aid = c_F12;
048700080702            $Fine = *on;
048800100302            O41f12='S'   ;
048900080414
049000080530 x1      // Invio / F6
049100080207           OTHER;
049200081006
049300081006        // Non effettuo controlli in interrogazione
049400100302 1           if I41mod<>'I' ;
049500081006               exsr ContrS01 ;
049600081006 2             if  errGenerico = *on;
049700111012                 $Video = 'W1';
049800081006                 leavesr;
049900081006 2             endif;
050000081006 2           endif;
050100080530
050200100302         // F6=conferma Aggiornamento
050300100302 1         if   dsp_aid = c_F06;
050400080702           exsr   ConfAggio ;
050500100305           // ritorno il flag di F6 se hanno inserito qualcosa
050600100305           // può capitare che in immissione diano F6 senza aver
050700100305           // inserito niente a video
050800100305           IF  $okqualcosa;
050900100302           O41f06='S'   ;
051000100305           ENDIF;
051100081023
051200080704           $Fine = *on;
051300080530           endif  ;
051400080207
051500080523 1       ENDSL;
051600080207
051700080207       ENDSR;
051800111012
051900111012       //--------------------------------------------------------------
052000111012       //?Gestione Window errori
052100111012       //--------------------------------------------------------------
052200111012       BEGSR GesW01;
052300111012
052400111012         reset errMessage;
052500111012         reset errGenerico;
052600111012
052700111012         w1riga = vscmsg;
052800111012         exfmt  TA41W01;
052900111012
053000111012         clear Vscmsg;
053100111012         errVAL  = *off;
053200111012         errVALD = *off;
053300111012         errPFT  = *off;
053400111012         errSNA  = *off;
053500111012         errFSN  = *off;
053600111012         errANN  = *off;
053700111012
053800111012         $Video = 'S1';
053900111012
054000111012       ENDSR;
054100080207
054200080526       //--------------------------------------------------------------
054300080702       //?controlli SFL01
054400080409       //--------------------------------------------------------------
054500080409       BEGSR ContrS01;
054600100303
054700100303       clear o41ifomin;
054800081215       clear wctrprc  ;
054900081006       clear wdescat   ;
055000081006       clear totprc   ;
055100081006       clear sptpft   ;
055200100302       clear itoval   ;
055300100303       clear its;
055400100302
055500100302       $okUNA = *off;
055600100302
055700081126       yy=1           ;
055800080703       s01nrr=1  ;
055900100302       chain s01nrr    TA41s01    ;
056000080703
056100081006 0     dow %found    ;
056200080925       // Escludo i record di descrizione categoria
056300081006 1     if vshimm= 'C'    ;
056400081003
056500100302       // A cambio di categoria
056600100302       // se c'era controllo obbligatorio del 100%
056700100302       // controllo ed eventualmente segnalo errore
056800100302       // in alternativa al controllo 100% c'è se almeno una INFO
056900100302       // deve essere inserita
057000081006       EXSR CambioCAT    ;
057100081006       if errGenerico=*on  ;
057200081006       leavesr;
057300081006       endif              ;
057400081006
057500081006x1     else    ;
057600081006
057700080925       pftblu=*off       ;
057800080925
057900081003       // se valori calcolati da pgm li pulisco prima dei controlli
058000081007 2     if vshsnf=' ' and vshprpft=' '   ;
058100080925       clear vscpft   ;
058200080925       clear vscvft   ;
058300081007 2     endif          ;
058400080703
058500080703       // Imposto la divisa, se impostato il fatturato
058600081007 2     if vscpft>0 and vscvft=*blanks   ;
058700080703       vscvft=§gedcr   ;
058800100423       //errGenerico = *on;
058900081007 2     endif    ;
059000080703
059100081007 2     if vscpft=0 and vscvft<>*blanks   ;
059200080703       clear vscvft   ;
059300080703       errGenerico = *on;
059400081007 2     endif   ;
059500100917
059600110407       //?Se valore fatturato impostato e > di 99000000 errore
059700110407       IF  VSCpft > 99000000;
059800100917         VSCmsg = msg(13);
059900100917         errPFT=*on;
060000100917         EXSR AggioSFL   ;
060100100917         leavesr;
060200100917       ENDIF;
060300111012
060400111012       //?Le spedizioni non possono essere superiori del valore fatturato
060500111012       IF  VSCsna > VSCpft;
060600111012         VSCmsg = msg(18);
060700111012         errPFT=*on;
060800111012         EXSR AggioSFL   ;
060900111012         leavesr;
061000111012       ENDIF;
061100080703
061200080704       // se info da annullare e non c'e' la "A", msg forzabile
061300081215       //  non faccio se modalità controlli
061400100302 2     if I41mod<>'C' and vshann='S'  and vscatb=' '  ;
061500080704           errANN=*on;
061600080704           // se  contiene dati, msg forzabile
061700100302 3         if vscfsn='S'or vscval<>0 or vscpft>0 or vscsna>0   ;
061800120608           vscmsg = Msg(24);
061900080704           vshann='X'      ;
062000080704           // Se non contiene dati abbligatorio l'annullamento
062100081007 x3        else            ;
062200100302           vscmsg = Msg(03);
062300081007 3         endif           ;
062400080704           EXSR AggioSFL   ;
062500080704           leavesr;
062600081007 2     endif    ;
062700120608
062800120608       // se c'e' "O"  obbligatorio l'annullamento
062900120608       if I41mod<>'C' and vshann='O'  and vscatb=' '  ;
063000120608           errANN=*on;
063100120608           vscmsg = Msg(03);
063200120608           EXSR AggioSFL   ;
063300120608           leavesr;
063400120608       endif    ;
063500080704
063600080703       // Se richiesto inserimento obbligatorio, per i dati
063700080703       //  obbligatori da tabella segnalo errore
063800100305 2     if  I41obl='S' and vshobl='S';
063900081007 3     if vscatb='A'    or
064000081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
064100081003         if vshsne<>' ';
064200080703           errFSN=*on;
064300081003         endif   ;
064400081003         if vshsnv<>' ';
064500081003           errVAL=*on;
064600081003         endif   ;
064700110406         IF  vshsnvd <> *blanks;
064800110406           errVALD = *on;
064900110406         ENDIF;
065000081003         if vshsnf<>' ';
065100081003           errPFT=*on;
065200081003         endif   ;
065300080703           vscmsg = Msg(01);
065400080703           EXSR AggioSFL   ;
065500080703           leavesr;
065600081007 3     endif   ;
065700081007 2     endif   ;
065800081007
065900081007       // Non accetto se non previsto il valore '?'
066000100805       //?Dal subfile ho tolto la possibilità di inserire '?'
066100081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
066200081007           errFSN=*on;
066300100302           vscmsg = Msg(08);
066400081007           EXSR AggioSFL   ;
066500081007           leavesr;
066600081007 3     endif    ;
066700081007       if vshifgv<>'?' and vscfsn='?'  ;
066800081007           errFSN=*on;
066900100302           vscmsg = Msg(09);
067000081007           EXSR AggioSFL   ;
067100081007           leavesr;
067200081007 3     endif    ;
067300080703
067400080703       // Se immesso "N" non indicare gli altri campi
067500100805       //?Dal subfile ho tolto la possibilità di inserire 'N'
067600100302 2     if vscfsn='N' and (vscval<>0 or vscpft>0 or vscsna>0)   ;
067700080703       if vshsnf<>' '    ;
067800080703           errPFT=*on;
067900080703       else    ;
068000081003          errVAL=*on ;
068100080703       endif  ;
068200080703           vscmsg = Msg(02);
068300080703           EXSR AggioSFL   ;
068400080703           leavesr;
068500081007 2     endif   ;
068600080703
068700081003       // Se immesso almeno un dato, controllo gli obbligatori ed
068800081215       //   i facoltativi. I  facoltativi non li guardo in modalità
068900081215       //   solo controllo
069000081031       if vscatb =' ' and
069100100302 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval<>0)     ;
069200081003
069300081007 3     if vscpft=0    and vshsnf='O'   ;
069400081003           errPFT=*on;
069500100302           vscmsg = Msg(01);
069600081003           EXSR AggioSFL   ;
069700081003           leavesr;
069800081007 3      endif    ;
069900100503
070000100503        // Lo opzionali le chiedo solo se info obbligatorie
070100100503       if  I41obl='S' and
070200100503 3        I41mod<>'C' and  vscpft=0    and vshsnf='S'   ;
070300081003           errPFT=*on;
070400100302           vscmsg = Msg(01);
070500100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
070600081003           vshsnf='X'      ;
070700081003           EXSR AggioSFL   ;
070800081003           leavesr;
070900081007 3     endif    ;
071000110118       //?OBBLIGO delle spedizioni se tipo trattativa <> 'A'
071100110118 3     if vscsna=0    and vshsns='O'   and  I41tpv <> 'A';
071200081003           errSNA=*on;
071300100302           vscmsg = Msg(01);
071400081003           EXSR AggioSFL   ;
071500081003           leavesr;
071600081007 3     endif    ;
071700100503       if  I41obl='S' and
071800100503 3        I41mod<>'C' and vscsna=0    and vshsns='S'   ;
071900081003           errSNA=*on;
072000100302           vscmsg = Msg(01);
072100100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
072200081003           vshsns='X'      ;
072300081003           EXSR AggioSFL   ;
072400081003           leavesr;
072500081007 3     endif    ;
072600081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
072700081007           errFSN=*on;
072800100302           vscmsg = Msg(09);
072900081007           EXSR AggioSFL   ;
073000081007           leavesr;
073100081007 3     endif    ;
073200100503       if  I41obl='S' and
073300100503 3        I41mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
073400081007           errFSN=*on;
073500100302           vscmsg = Msg(09);
073600100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
073700081007           vshsne='X'      ;
073800081007           EXSR AggioSFL   ;
073900081007           leavesr;
074000081007 3     endif    ;
074100081007 3     if (vscval=0    and vshsnv='O')  or
074200100503          (vscval=0    and vshsnv='S' and I41mod<>'C' AND I41OBL='S') ;
074300081007 4        if vshsnv='O'    ;
074400100302            vscmsg = Msg(01);
074500081003          else   ;
074600100302            vscmsg = Msg(01);
074700100302            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
074800100302            vshsnv='X'      ;
074900081215 4        endif  ;
075000081003           errVAL=*on;
075100081003           EXSR AggioSFL   ;
075200081003           leavesr;
075300081215 3     endif    ;
075400110406 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
075500110406           (vscvald = 0 and vshsnvd = 'S'   and
075600110406            I41mod <> 'C' and I41obl = 'S');
075700110406 4        IF  vshsnvd = 'O';
075800110406            vscmsg = Msg(01);
075900110406          ELSE;
076000110406            vscmsg = Msg(01);
076100110406            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
076200110406            vshsnvd = 'X';
076300110406 4        ENDIF;
076400110406          errVALD = *on;
076500110406          EXSR AggioSFL;
076600110406          leavesr;
076700110406 3     ENDIF;
076800110207
076900110207       //?Mi salvo il valore del traffico totale
077000110207 3     IF  vscifo = 'TRT';
077100110207       sav_vscpft = vscpft;
077200110324       sav_vscsna = vscsna;
077300110207       ENDIF;
077400110207
077500110207       //?Il traffico estero è un 'di cui' quindi non può superare
077600110207       //?il traffico totale
077700110324       //?Stessa cosa per le spedizioni
077800110324       IF  vscifo = 'TRE' and vscpft > sav_vscpft;
077900110207         errPFT = *on;
078000110408         vscmsg = msg(16);
078100110207         EXSR AggioSFL;
078200110207         leavesr;
078300110324       ENDIF;
078400110324       IF  vscifo = 'TRE' and vscsna > sav_vscsna;
078500110324         errSNA = *on;
078600110408         vscmsg = msg(17);
078700110324         EXSR AggioSFL;
078800110324         leavesr;
078900110324       ENDIF;
079000110112
079100111012       //?Solo per info "KMS" peso medio
079200111012       IF  VSCval <> 0 and  VSCtva = 'KG ' and VSCifo = 'KMS';
079300111012       //?reimposto il flag di forzatura
079400111012         IF  VSCval <> sav_valkg;
079500111012           $ForzaKg = *off;
079600111012         ENDIF;
079700111012       //?se valore in kg non deve superare i 5000 kg.
079800111012         IF  VSCval > 5000;
079900111012           errVAL = *on;
080000111012           VSCmsg = msg(15);
080100111012           exsr AggioSFL;
080200111012           sav_valkg = VSCval;
080300111012           leavesr;
080400111012         ENDIF;
080500111028       //?msg info se è ugale o supera i 500 kg.
080600111028         IF  VSCval >= 500 and VSCval <> sav_valkg and not $ForzaKg;
080700111012           errVAL = *on;
080800111012           VSCmsg = msg(19);
080900111012           exsr AggioSFL;
081000111012           $ForzaKg = *on;
081100111012           sav_valkg = VSCval;
081200111012           leavesr;
081300111012         ENDIF;
081400111012       ENDIF;
081500111012
081600111012       //?Solo per info "DEL" delta
081700111012       IF  VSCval <> 0 and VSCtva ='%  ' and VSCifo = 'DEL';
081800111012       //?reimposto il flag di forzatura
081900111012         IF  VSCval <> sav_valdel or VSCsgn <> sav_sgndel;
082000111012           $ForzaDel = *off;
082100111012         ENDIF;
082200111012       //?non posso inserire più di 2 cifre quindi al massimo 99
082300111012         IF  VSCval > 99;
082400111012           errVAL = *on;
082500111012           VSCmsg = msg(20);
082600111012           exsr AggioSFL;
082700111012           sav_valdel = VSCval;
082800111012           sav_sgndel = VSCsgn;
082900111012           leavesr;
083000111012         ENDIF;
083100111012       //?msg info se supera 30 +/- indifferente
083200111028         IF  VSCval > 30 and not $ForzaDel and
083300111012            (VSCval <> sav_valdel or VSCsgn <> sav_sgndel);
083400111012           errVAL = *on;
083500111012           VSCmsg = msg(21);
083600111012           %subst(VSCmsg:36:1) = vscsgn;
083700111012           exsr AggioSFL;
083800111012           $ForzaDel = *on;
083900111012           sav_valdel = VSCval;
084000111012           sav_sgndel = VSCsgn;
084100111012           leavesr;
084200111012         ENDIF;
084300111012       ENDIF;
084400081003
084500081003       // se Valore in % --> non deve superare 100%
084600100302 3     if  vscval<>0 and vsctva='%  '    ;
084700081007 4       if vscval>100    ;
084800081003          errVAL=*on ;
084900100302          vscmsg = Msg(05);
085000081003          EXSR AggioSFL   ;
085100081003          leavesr;
085200081007 4       endif      ;
085300081003
085400080925       // Sommo la percentuale se presente per ogni categoria
085500080925       //  se supera 100% segnalo errore
085600081006       //  solo se previsto controllo sulla %
085700081007 4     if wctrprc='P'     ;
085800081006
085900081003       totprc=totprc+vscval    ;
086000081007 5       if totprc>100    ;
086100081003         errVAL=*on ;
086200100302         vscmsg = Msg(04);
086300080925         EXSR AggioSFL   ;
086400080925         leavesr;
086500081007 5       endif      ;
086600081007 4     endif      ;
086700080925
086800100302       // Se immessa la %, visualizzo in base a INFO "SPT"
086900080925       exsr   VISUALimp     ;
087000081007 3     endif      ;
087100100303
087200081007 2     endif      ;
087300110406
087400110406       //?Se Valore con decimali in % --> non deve superare 100%
087500110406 3     IF  vscvald <> 0 and vsctva = '%  ';
087600110406 4       IF  vscvald > 100;
087700110406           errVALD = *on;
087800110406           vscmsg = Msg(05);
087900110406           EXSR AggioSFL   ;
088000110406           leavesr;
088100110406 4       ENDIF;
088200110406 4     ENDIF;
088300111012
088400111012       //?Per Ricavo medio spedizione
088500111012 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
088600111012       //?calcolo il valore
088700111012 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
088800111012           wVALrms = sav_vscpft / sav_vscsna;
088900111012         ELSE;
089000111012           wVALrms = 0;
089100111012         ENDIF;
089200111012       //?reimposto il flag di forzatura
089300111012         IF  wVALrms <> sav_valrms;
089400111012           $ForzaRms = *off;
089500111012         ENDIF;
089600111012       //?imposto il campo del video
089700140929         IF  wVALrms > 999;
089800140929           vscvald = 999;
089900111012         ELSE;
090000111012           vscvald = wVALrms;
090100111012         ENDIF;
090200111012       //?msg info se inferiore a 4
090300111012         IF  VSCvald < 4 and not $ForzaRms and
090400111012             wVALrms <> sav_valrms;
090500111012           errVALD = *on;
090600111012           vscmsg = msg(22);
090700111012           exsr AggioSFL;
090800111012           $ForzaRms = *on;
090900111012           sav_valrms = wVALrms;
091000111012           leavesr;
091100111012         ENDIF;
091200111012       //?msg info se superiore a 50
091300111012         IF  VSCvald > 50 and not $ForzaRms and
091400111012             wVALrms <> sav_valrms;
091500111012           errVALD = *on;
091600111012           vscmsg = msg(23);
091700111012           exsr AggioSFL;
091800111012           $ForzaRms = *on;
091900111012           sav_valrms = wVALrms;
092000111012           leavesr;
092100111012         ENDIF;
092200111012       ENDIF;
092300081006
092400100303
092500100305       //?------------
092600100305       //?CHIODI FISSI
092700100305       //?------------
092800100305
092900100305       // se tutte le info della categoria sono obbligatorie ed ho inserito
093000100305       // anche solo 1 info nella videata
093100100305       // errore se info non inserita
093200100305       // oppure la categoria non è tutta obbligatoria ma solo alcune sue info
093300100305       // stessa cosa, se inserito anche solo 1 info nella videta
093400100305       // errore se info non inserita
093500100305       IF  $okqualcosa and (wctrprc = 'S' or vshobl = 'S');
093600100305         SELECT;
093700100422           WHEN  vshsnv <> *blanks and vscval = 0 and vscifo <> 'DEL';
093800100305             errVAL = *on;
093900100305             vscmsg = Msg(01);
094000100305             EXSR AggioSFL;
094100100305             leavesr;
094200111012           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
094300110406             errVALD = *on;
094400110406             vscmsg = Msg(01);
094500110406             EXSR AggioSFL;
094600110406             leavesr;
094700100305           WHEN  vshsnf <> *blanks and vscpft = 0;
094800100305             errPFT = *on;
094900100305             vscmsg = Msg(01);
095000100305             EXSR AggioSFL;
095100100305             leavesr;
095200110118           WHEN  vshsns <> *blanks and vscsna = 0 and I41tpv <> 'A';
095300100305             errSNA = *on;
095400100305             vscmsg = Msg(01);
095500100305             EXSR AggioSFL;
095600100305             leavesr;
095700100305           WHEN  vshsne <> *blanks and vscfsn = *blanks;
095800100305             errFSN = *on;
095900100305             vscmsg = Msg(01);
096000100305             EXSR AggioSFL;
096100100305             leavesr;
096200100305         ENDSL;
096300100305       ENDIF;
096400100303
096500100303       // controllo se almeno una info inserita
096600120607       IF  wctrprc = 'U' and vscatb = *blanks;
096700100303         SELECT;
096800100303           WHEN  vshsnv <> *blanks and vscval <> 0;
096900100303             $okUNA = *on;
097000110406           WHEN  vshsnvd <> *blanks and vscvald <> 0;
097100110406             $okUNA = *on;
097200100303           WHEN  vshsns <> *blanks and vscsna > 0;
097300100303             $okUNA = *on;
097400100303           WHEN  vshsnf <> *blanks and vscpft > 0;
097500100303             $okUNA = *on;
097600100303           WHEN  vshsne <> *blanks and vscfsn <> *blanks;
097700100303           $okUNA = *on;
097800100303         ENDSL;
097900100303       ENDIF;
098000100303
098100100303       // carico in sk di comodo i concorrenti inseriti a video
098200100303       IF  vscifo = '10';
098300100303         // codice concorrente inserito carico in sk di appoggio
098400120607         IF  vscfsn = 'S' and %lookup(vscspp:its) = 0 and
098500120607             vscatb = *blanks;
098600100303           yy = %lookup(*blanks:its);
098700100303           its(yy) = vscspp;
098800100303         ENDIF;
098900100303         // codice concorrente non inserito ed era in sk lo tolgo
099000120607         IF  (vscfsn = 'N'or vscfsn = *blanks or vscatb = 'A');
099100100303           yy = %lookup(vscspp:its);
099200100303           IF yy > 0;
099300100303             its(yy) = *blanks;
099400100303           ENDIF;
099500100303         ENDIF;
099600100303       ENDIF;
099700100423
099800100423 2     if  vshifgv<>'?' and vshsgn='S' ;
099900110406         SELECT;
100000110406           WHEN VSHsnv <> *blanks;
100100110406             IF  not $okQualcosa and vscval = 0;
100200110406               clear vscsgn;
100300110406             ELSE;
100400110406               IF  vscsgn = *blanks;
100500110406                 vscsgn = '+';
100600110406               ENDIF;
100700110406             ENDIF;
100800110406           WHEN VSHsnvd <> *blanks;
100900110406             IF  not $okQualcosa and vscvald = 0;
101000110406               clear vscsgn;
101100110406             ELSE;
101200110406               IF  vscsgn = *blanks;
101300110406                 vscsgn = '+';
101400110406               ENDIF;
101500110406             ENDIF;
101600110406           WHEN  VSHsnf <> *blanks;
101700110406             IF  not $OkQualcosa and VSCpft = 0;
101800110406               clear VSCsgn;
101900110406             ELSE;
102000110406               IF  VSCsgn = *blanks;
102100110406                 VSCsgn = '+';
102200110406               ENDIF;
102300110406             ENDIF;
102400110406           OTHER;
102500110406             clear VSCsgn;
102600110406         ENDSL;
102700100423       endif   ;
102800081007
102900080925       // Se info SPT - totale spesa trasporti,  la memorizzo
103000080925       //  per il calcolo delle %
103100101215       if vscifo='TRT' ;
103200080925       SPTPFT=vscpft   ;
103300101215       //?non devo ripartire in %
103400101215       clear sptpft;
103500080925       endif           ;
103600101230
103700101230       //?------------
103800101230       //?CHIODI FISSI
103900101230       //?------------
104000101230       //?Se Trattativa di tipo 'A'
104100110112       //?Memorizzo il segno in campi di work
104200101230       IF  I41tpv = 'A';
104300110112         IF  VSCIFO = 'TRT';
104400110111           wsgntrt = VSCsgn;
104500101230         ENDIF;
104600110112         IF  VSCIFO = 'A/S';
104700110111           wsgnausc = VSCsgn;
104800101230         ENDIF;
104900101230       ENDIF;
105000100305
105100100305       //?-----------------------------------------------------
105200100305       //?CHIODO FISSO a mio utilizzo per controllo concorrenti
105300100305       //?-----------------------------------------------------
105400100305
105500100305       // mi memorizzo se ho inserito almeno una riga di info
105600100305       // per poi controllare meglio i concorrenti, se anche solo
105700100305       // una riga di info inserita i concorrenti sono obbligatori
105800100305         IF  Not $okqualcosa and
105900100305            (vscpft > 0 or vscfsn <> *blanks or vscval <> 0 or
106000110406             vscsna > 0 or vscvald <> 0);
106100100305           $okqualcosa = *on;
106200100305         ENDIF;
106300080925
106400080703       EXSR AggioSFL   ;
106500081006 1     endif   ;
106600081125
106700080703
106800080703       s01nrr=1+s01nrr  ;
106900100302       chain s01nrr    TA41s01    ;
107000081006 0     enddo         ;
107100110111
107200110111       //?Se Trattativa di tipo 'A'
107300110111       IF  I41tpv = 'A';
107400110112         IF  wsgntrt <> wsgnausc;
107500110112           VSCmsg = msg(14);
107600110112           c01csr = 2;
107700110112           errMessage  = *on;
107800110112           errGenerico = *on;
107900110112           leavesr;
108000110112         ENDIF;
108100110111       ENDIF;
108200081006
108300081006       // Controllo ultima categoria
108400081006       EXSR CambioCAT     ;
108500081006
108600081215       // A questo punto le info minime sono presenti
108700081215       // solo per controlli obbligatori
108800100302       if I41obl='S' ;
108900100302         O41ifomin='S' ;
109000081215       endif ;
109100081215
109200081006
109300080409       ENDSR;
109400080409
109500081006       //--------------------------------------------------------------
109600081006       //?cambiata categoria delle info conn- controllo e salvataggi
109700081006       //--------------------------------------------------------------
109800081006       BEGSR  CambioCAT    ;
109900100302
110000100302       if wctrprc='P'  and  I41obl='S' and totprc<100  ;
110100081006       s01nrr=s01nrr-1 ;
110200100302       chain s01nrr    TA41s01    ;
110300081006           errVal=*on;
110400100302           vscmsg = Msg(06);
110500081006           %subst(vscmsg:40:21)=wdescat   ;
110600081006           EXSR AggioSFL   ;
110700081006       endif              ;
110800100303
110900100303       //?-------------------------------------
111000100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
111100100303       //?-------------------------------------
111200100302
111300100303       // controllo che almeno una INFO della categoria sia inserita
111400100303       // se richiesto da tabella
111500100305       IF  wctrprc = 'U' and not $okUNA and
111600100305           (I41obl = 'S' or $okqualcosa);
111700100302         s01nrr -= 1;
111800100302         chain s01nrr TA41S01;
111900100303         SELECT;
112000100303           WHEN vshsns<>' ';
112100100303             errSNA=*on;
112200100303           WHEN vshsne<>' ';
112300100303             errFSN=*on;
112400100303           WHEN  vshsnv<>' ';
112500100303             errVAL=*on;
112600110406           WHEN  vshsnvd <> ' ';
112700110406             errVALD = *on;
112800100303           WHEN  vshsnf<>' ';
112900100303             errPFT=*on;
113000100303         ENDSL;
113100100302         vscmsg = Msg(07);
113200100302         %subst(vscmsg:43:21)=wdescat;
113300100302         EXSR AggioSFL;
113400100302       ENDIF;
113500100303
113600100303       // controllo se inserito 'NESSUN CONCORRENTE' + altri codici
113700100303       // concorrenti
113800100303       IF  %lookup('8810':its) > 0;
113900100303         yy = 1;
114000100303         FOR yy by 1 to %elem(its);
114100100303           IF its(yy) <> *blanks and its(yy) <> '8810';
114200100303             s01nrr -= 1;
114300100303             chain s01nrr TA41S01;
114400100303             errFSN = *on;
114500100303             vscmsg = Msg(12);
114600100303             EXSR AggioSFL;
114700100303             leave;
114800100303           ENDIF;
114900100303         ENDFOR;
115000100303       ENDIF;
115100081006
115200081006       // Pulisco e memorizzo nuovi dati per nuova categoria
115300081006        clear  totprc     ;
115400081215        wctrprc=vshobl  ;
115500081215        wdescat=vsdifo   ;
115600100302        $okUNA = *off;
115700100302
115800081006        ENDSR   ;
115900080925       //--------------------------------------------------------------
116000080925       //?Visualizzo importi in base alla spesa trasporti
116100080925       //--------------------------------------------------------------
116200080925       BEGSR    VISUALimp  ;
116300100302       if vshsnf=' ' and vscpft=0 and vscval<>0 and vsctva='%  '  ;
116400081215       PFTblu=*on     ;
116500081003       vscpft=(SPTpft*vscval)/100   ;
116600080925       if vscpft>0    ;
116700080925       vscvft='EUR'   ;
116800080925       endif ;
116900080925       endif ;
117000080925
117100080925       ENDSR      ;
117200080703       //--------------------------------------------------------------
117300080703       //?Aggiornamento sfl
117400080703       //--------------------------------------------------------------
117500080703       BEGSR AGGIOSFL  ;
117600100304
117700080703       if vscmsg<>*blanks   ;
117800080703       errMessage  = *on;
117900080703       errGenerico = *on;
118000080704       c01csr=s01nrr    ;
118100080703       endif    ;
118200080703
118300080703       exsr ProtCampi   ;
118400080703
118500080703       //  Ripristino dei flag di forzatura
118600081003       if vshsnf='X' and vscpft>0   ;
118700081003       vshsnf='S'   ;
118800080703       endif    ;
118900081003       if vshsns='X' and vscsna>0   ;
119000080703       vshsns='S'   ;
119100080703       endif    ;
119200100302       if vshsnv='X' and vscval<>0  ;
119300081003       vshsnv='S'   ;
119400080703       endif    ;
119500110406       IF  vshsnvd = 'X' and vscvald <> 0;
119600110406         vshsnvd = 'S';
119700110406       ENDIF;
119800081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
119900081007       vshsne='S'   ;
120000081007       endif    ;
120100080703
120200120607       if vscatb='A' and vshann = 'X';
120300080704       vshann='S'  ;
120400080704       endif   ;
120500080703
120600100302       update  TA41s01  ;
120700100304
120800080703       ENDSR   ;
120900080207       //--------------------------------------------------------------
121000080627       //?Inizializzazione SFL01
121100080207       //--------------------------------------------------------------
121200080409       BEGSR InzS01;
121300080207
121400080207       // Pulizia subfile
121500080207         SflDsp_N    = *on;
121600080207         SflDspCtl_N = *on;
121700100302         write  TA41C01;
121800080207         SflDspCtl_N = *off;
121900080207         SflEnd      = *off;
122000080530
122100080530         clear C01rcd;
122200100507         C01csr=1;
122300080627         S01nrr=0 ;
122400080702         clear Vscmsg;
122500080207         errMessage  = *off;
122600080207         errGenerico = *off;
122700080627         clear savcat  ;
122800080627         clear wcat    ;
122900100302         clear wito    ;
123000100302         clear wits    ;
123100081006         clear sptpft  ;
123200100303
123300100303         clear its;
123400100305         errVAL=*off     ;
123500110406         errVALD = *off;
123600100305         errPFT=*off     ;
123700100305         errSNA=*off     ;
123800100305         errFSN=*off     ;
123900100305         errANN=*off     ;
124000100305
124100100305       $okqualcosa = *off;
124200080409
124300080702       // Carico le info
124400080627       xx=1    ;
124500080627       Indx=1    ;
124600080702       dow   xx<=50    ;
124700100302       if    itoor(xx)<>*blanks  ;
124800100302       clear  TA41S01;
124900080627
125000100302       Wcat= %subst(itoor(xx):1:2)   ;
125100100302       Wito= %subst(itoor(xx):6:3)   ;
125200080627
125300080627       // cambio di categoria
125400080627        if savcat=*blanks or savcat<>wcat               ;
125500080627        savcat=wcat   ;
125600100302        ifosott=*on   ;
125700080627
125800100507       // Verifico se devo visualizzare il titolo della categoria
125900100302         clear  DITG  ;
126000080627         reset TIBS02ds;
126100080627         T02sif = knsif;
126200100302         T02cod = 'ITG';
126300080627         T02mod = 'C'  ;
126400080627         T02ke1 = wcat;
126500080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
126600080627         if t02err=*blanks ;
126700100302            ditg=t02uni    ;
126800080627         endif            ;
126900080627
127000080627       // Se prevista la visualizzazione della categoria
127100080627       //  emetto la riga in videata,
127200100302       if §itgvis='S'    ;
127300100302         vsdifo=§itgdes         ;
127400080703         vsdifo=%trim(vsdifo)+':'    ;
127500080703         // In VSHIMM   metto una "C" per indicare che si tratta
127600080703         //    di riga di categoria
127700080703         vshimm='C'        ;
127800081007
127900081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
128000100302         // in alternativa imposto il flag se almeno una info deve
128100100302         // essere inserita
128200100302         SELECT;
128300100302           WHEN  §itgctpr <> *blanks;
128400100302             vshobl=§itgctpr   ;
128500100305           WHEN  §itgobl <> *blanks;
128600100305             vshobl = §itgobl;
128700100302         ENDSL;
128800081007
128900081007         EXSr ProtCampi    ;
129000081003
129100080627         s01nrr=s01nrr+1   ;
129200100302         write TA41s01     ;
129300080627         protriga=*off     ;
129400100302         ifosott=*off      ;
129500081007         clear vscfsn      ;
129600100507
129700100507         // Se si tratta della prima riga, imposto posizionamento cursore
129800100507         //  sulla seconda
129900100507         if s01nrr=1  ;
130000100507            C01csr = 2;
130100100507         endif   ;
130200100507
130300081007       endif               ;
130400081007       endif               ;
130500080627
130600100302       Indx=%lookup(wito:ito)   ;
130700080627       if Indx>0                ;
130800100302         dito=itods(Indx)         ;
130900100302         vscifo=ito(Indx)       ;
131000080702
131100080702         // Dati da tabella
131200100302          vshobl=§itoobl  ;
131300100302          vshie =§itotip  ;
131400080704
131500080704          clear vshann  ;
131600100302          if §itoann='S'  ;
131700100302          vshann=§itoann  ;
131800080704          endif   ;
131900111012
132000111012         //?pulisco campi comuni per vscval
132100111012          IF  §ITOitg <> '2A' and §ITOitg <> '5A';
132200111012            clear VSHsnv;
132300111012            clear VSHsgn;
132400111012            clear VSCtva;
132500111012          ENDIF;
132600080704
132700081003         // VALORE
132800111012          if §itosnv<>' '  ;
132900100302          vshsnv=§itosnv  ;
133000100423          vshSGN=§itosgn  ;
133100081003          // tipo valore
133200100302          vsctva=§itotva ;
133300080703          endif   ;
133400110406
133500110406         //?VALORE con DECIMALE
133600110406          IF  §itosnvd <> *blanks;
133700110406            vshsnvd = §itosnvd;
133800110406            vshSGN  = §itosgn;
133900110406          // tipo valore
134000110406            vsctva = §itotva;
134100110406          ENDIF;
134200080704
134300081003         // SPEDIZIONI
134400100302          vshsns=§itosns  ;
134500080704
134600081003         // FATTURATO
134700100302          vshsnf=§itosnf  ;
134800101214          vshSGN=§itosgn  ;
134900081003
135000081003         // SI/NO
135100100302          vshsnE=§itosnE  ;
135200081003
135300100302          vshifgv=§itgvis  ;
135400081007          clear vscint    ;
135500080627
135600080627         // Record senza sottotipo
135700081006         select    ;
135800100302         when §itoits=' '    ;
135900100302            vsdifo=§itodes         ;
136000100302            if ifosott=*on    ;
136100080703               vsdifo=%trim(vsdifo)+':'    ;
136200080703            endif    ;
136300100302            chain    (I41nrv:wito) TIVII01l  ;
136400100302            if %found(TIVII01l)   ;
136500100302             trovavii='S'    ;
136600081126             else ;
136700100302             trovavii='N'    ;
136800081126             endif  ;
136900080702            EXSR CaricaInfo        ;
137000081006
137100081006         // Record con   sottotipo da caricare tutti
137200100302         when   §itoits='S' and §itoifv='S';
137300080627            EXSR CARCon    ;
137400081006
137500081006         // Record con   sottotipo da caricare col '?'
137600081006         //   la categoria visualizzata in questo caso è obbligatoria
137700081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
137800081006         //  il '?'
137900100302         when   §itoits='S' and §itoifv='?';
138000100302          vshifgv=§itoifv  ;
138100081215          yy=1        ;
138200081126
138300100302          if §itonrec=0 ;
138400100302             §itonrec=1  ;
138500081126          endif     ;
138600081126
138700100302          setll (I41nrv:wito) TIVII01l  ;
138800100302          dow yy<=§itonrec  ;
138900081006            EXSR CarconUNA            ;
139000081126            yy=yy+1  ;
139100081126          enddo    ;
139200081126
139300081006         endsl ;
139400080703
139500080703         endif ;
139600080702         endif ;
139700080702
139800080702         xx=xx+1   ;
139900080702         enddo     ;
140000100304
140100080702         ENDSR    ;
140200080627
140300080702       //--------------------------------------------------------------
140400081006       //?Carico con   sottotipo  carica riga per riga
140500080702       //--------------------------------------------------------------
140600080702       BEGSR  CARcon            ;
140700080702
140800100302         tbeke1=wito   ;
140900080925
141000100302         setll ('ITS':tbeke1) tntbe01l   ;
141100100302         READE ('ITS':tbeke1) tntbe01l   ;
141200080702
141300080702         dow not %eof(tntbe01l)   ;
141400080702         if tbeatb=' '    ;
141500100302         dits=tbeuni      ;
141600100302         wits=tbeke2      ;
141700080702         vscspp=tbeke2      ;
141800081127         vshspp=tbeke2      ;
141900100302         vsdifo=§itsdes  ;
142000120607         vshann = §itsann;
142100080702
142200100302         chain    (I41nrv:wito:wits) TIVII01l  ;
142300100302            if %found(TIVII01l)   ;
142400100302             trovavii='S'    ;
142500081126             else ;
142600100302             trovavii='N'    ;
142700081126             endif  ;
142800080702         EXSR CaricaInfo    ;
142900100303
143000100303       //?-------------------------------------
143100100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
143200100303       //?-------------------------------------
143300100303
143400100303       // carico i concorrenti in una sk di comodo
143500100303           IF  trovavii = 'S' and wito = '10';
143600100303             IF  %lookup(wits:its) = 0;
143700100303               yy = %lookup(*blanks:its);
143800100303               its(yy) = wits;
143900100303             ENDIF;
144000100303           ENDIF;
144100100303
144200080925         endif   ;
144300080702
144400100302         READE ('ITS':tbeke1) tntbe01l   ;
144500080702         enddo   ;
144600080702
144700080702       ENDSR;
144800081006       //--------------------------------------------------------------
144900081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
145000081006       //--------------------------------------------------------------
145100081006       BEGSR  CARconUNA         ;
145200081007
145300081007       vscint='?'    ;
145400081007
145500100302       reade    (I41nrv:wito) TIVII01l  ;
145600081006
145700100302 1     if not %eof(TIVII01l)   ;
145800100302         trovavii='S'    ;
145900081126
146000100302         vscspp=viispf   ;
146100100302         vshspp=viispf   ;
146200081006
146300100302         tbeke1=wito    ;
146400081126
146500100302         tbeke2=viispf  ;
146600100302         chain ('ITS':tbeke1:tbeke2) tntbe01l   ;
146700081006 2       if not %found(tntbe01l)    ;
146800081006         clear tbeuni       ;
146900081006 2       endif    ;
147000100302         dits=tbeuni     ;
147100081006
147200100302         vsdifo=§itsdes   ;
147300100302         vsdifs=§itsdel   ;
147400081007
147500081006         EXSR CaricaINFO  ;
147600081006
147700081006 1x      else    ;
147800100302         trovavii='N'    ;
147900081006
148000100302         // se non trovato dicitura generica da tabella ITO
148100081126           vsdifo=coRicerca       ;
148200081007         clear vsdifs      ;
148300081126         clear vscspp      ;
148400081127         clear vshspp      ;
148500081006
148600081006         EXSR CaricaINFO   ;
148700081006         endif             ;
148800081006
148900081006       ENDSR;
149000080702       //--------------------------------------------------------------
149100081006       //?Carica ogni riga di info commerciale
149200080702       //--------------------------------------------------------------
149300080702         BEGSR CaricaINFO             ;
149400100302
149500080925         clear pftblu   ;
149600080925
149700100302         if trovavii='S'                             ;
149800080703          vshimm=' '        ;
149900100302          vscfsn=viifsn   ;
150000100423          vscval=%abs(viival)   ;
150100110406          vscvald = %abs(viivald);
150200100423
150300100423          // Se previsto segno visualizzo + o -
150400101215          if vshsgn='S'   and vshsnv <> ' ' ;
150500100423            if  viival<0   ;
150600100423             vscsgn='-'  ;
150700100423            else    ;
150800100423             vscsgn='+'  ;
150900100423            endif   ;
151000100423          endif   ;
151100110406          IF  vshsgn = 'S' and vshsnvd <> ' ';
151200110406            IF  viivald < 0;
151300110406              vscsgn = '-';
151400110406            ELSE;
151500110406              vscsgn = '+';
151600110406            ENDIF;
151700110406          ENDIF;
151800101215
151900101215          vscpft=%abs(viipft);
152000101215          if vshsgn='S'   and vshsnf <> ' ' ;
152100101215            if  viipft<0   ;
152200101215             vscsgn='-'  ;
152300101215            else    ;
152400101215             vscsgn='+'  ;
152500101215            endif   ;
152600101215            IF  I41tpv = 'A' and viipft = 0;
152700101215             vscsgn=' '  ;
152800101215            ENDIF;
152900101215          endif   ;
153000100423
153100081007          if vshifgv='?'   and vscspp>*zeros    ;
153200081007            w0040=%int(vscspp)  ;
153300081007            vscval=w0040     ;
153400081007            clear vscfsn     ;
153500081007          endif            ;
153600100302          vscsna=viisna   ;
153700080925          if vscpft>0     ;
153800080925          vshprpft='S'    ;
153900080925          endif           ;
154000100302          vscvft=viivft   ;
154100100302          Dataiso=%date(viidim:*iso)  ;
154200080702          datadmy=Dataiso ;
154300080702          vscdim=%dec(datadmy)  ;
154400081003          clear vscfil  ;
154500100302          if viifil>0   ;
154600100302          vscfil=%editc(viifil:'X')   ;
154700081003          endif         ;
154800081003
154900100302          if viipru<>*blanks ;
155000100302          vscpru=viipru   ;
155100081003          vscdpru='Utente' ;
155200081003          else  ;
155300081003          clear vscpru   ;
155400081003          clear vscdpru   ;
155500081003          endif     ;
155600080925
155700080925          exsr ViSUALimp   ;
155800101230
155900101230         //?------------
156000101230         //?CHIODI FISSI
156100101230         //?------------
156200101230         // se trattativa di tipo 'A' memorizzo il segno del traffico
156300101230         // totale e dell'aumento/sconto
156400101230           IF  I41tpv = 'A';
156500101230             IF  VSCifo = 'TRT';
156600101230               VSHstrt = VSCsgn;
156700101230             ENDIF;
156800101230             IF  VSCifo = 'A/S';
156900101230               VSHsausc = VSCsgn;
157000101230             ENDIF;
157100101230           ENDIF;
157200111012
157300111012         //?Calcolo il ricavo medio spedizione 'RMS'
157400111012           IF  VSCifo = 'TRT' and VSCpft <> 0 and VSCsna <> 0;
157500111012             wVALrms = VSCpft / VSCsna;
157600140929             IF  wVALrms > 999;
157700140929               wVALrms = 999;
157800111012             ENDIF;
157900111012           ENDIF;
158000080925
158100080702         else             ;
158200080703          vshimm='S'      ;
158300080703          Protann=*on     ;
158400080702          clear vscfsn    ;
158500081003          clear vscval    ;
158600110406          clear vscvald   ;
158700080702          clear vscsna    ;
158800080702          clear vscpft    ;
158900080702          clear vscvft    ;
159000080702          clear vscdim    ;
159100080702          clear vscfil    ;
159200080702          clear vscpru    ;
159300081003          clear vscdpru    ;
159400080925          clear vshprpft  ;
159500101230          clear VSHstrt;
159600101230          clear VSHsausc;
159700111012
159800111012         //?Se info 'RMS' imposto il dato
159900111012           IF  VSCifo = 'RMS';
160000111012             VSCvald = wVALrms;
160100111012           ENDIF;
160200111012
160300080702         endif            ;
160400081006
160500080925         exsr ProtCAMPI    ;
160600080703
160700080703         // I record da annullare li visualizzo solo se ci sono
160800100302           IF vshann = *blanks
160900120607                       or (vshann <> ' ' and vscfsn <> *blanks)
161000120607                       or (vshann <> ' ' and vscpft > 0)
161100120607                       or (vshann <> ' ' and vscval <> 0)
161200120607                       or (vshann <> ' ' and vscvald <> 0);
161300100302
161400081003
161500081003         // In interrogazione visulizzo solo i record pieni col sottotipo
161600100302           if I41mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
161700080703            s01nrr=s01nrr+1   ;
161800100302            write TA41s01     ;
161900101215
162000101215         // Se info TRT - totale spesa trasporti,  la memorizzo
162100101215         //  per il calcolo delle %
162200101215         if vscifo='TRT' ;
162300101215          SPTPFT=vscpft   ;
162400101215         //?non devo ripartire in %
162500101215          clear sptpft;
162600101215         endif           ;
162700080925
162800080703           endif ;
162900081003           endif ;
163000081003
163100080702       ENDSR;
163200080702       //--------------------------------------------------------------
163300080702       //?Gestione protezione campi del sfl
163400080702       //--------------------------------------------------------------
163500080702          BEGSR   ProtCampi   ;
163600100304
163700080703           protfat=*off    ;
163800081003           protVal =*off   ;
163900110406           protVald = *off;
164000111012           protVala = *off;
164100100423           protSGN =*off   ;
164200101214           protSGNa=*off   ;
164300080703           protsped=*off   ;
164400080703           protfsn =*off   ;
164500100302           ifosott=*off    ;
164600080703           Protann=*off ;
164700080704           Infoyell=*off ;
164800080703
164900081007           // Se riga di categoria accendo solo protezione e sottolineatura
165000081007          if vshimm='C'    ;
165100100302           ifosott=*on     ;
165200081007           Protriga=*on    ;
165300081007          else             ;
165400081007
165500080702          // in base a cosa prevede la tabella, proteggo i campi
165600080702          if vshifgv='N' ;
165700100302            ifosott=*on       ;
165800080702          endif    ;
165900080702
166000080704          if vshann<>' '  ;
166100080702           protfat=*on     ;
166200081003           protVal =*on    ;
166300110406           protVald = *on;
166400111012           protVala = *on;
166500100423           protSGN =*on    ;
166600101214           protSGNa=*on    ;
166700080702           protsped=*on    ;
166800080702           protfsn =*on    ;
166900080704           infoyell=*on    ;
167000080702          else            ;
167100081006          protann=*on     ;
167200081031
167300081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
167400110406          if vscpft>0 and vshsnf=' ' and vscval=0 and vscvald = 0;
167500081031          protann=*off    ;
167600081031          endif    ;
167700081031          if vscsna>0 and vshsns=' ' ;
167800081031          protann=*off    ;
167900081031          endif    ;
168000110406          if vscval<>0  and vshsnv=' ' and  vshifgv<>'?'       ;
168100081031          protann=*off    ;
168200081031          endif    ;
168300110406          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
168400110406            protann = *off;
168500110406          ENDIF;
168600081031          if vscfsn<>' ' and vshsne=' ' ;
168700081031          protann=*off    ;
168800081031          endif    ;
168900081006
169000080703            if vshsnf=' ' ;
169100080702             protfat=*on     ;
169200080702            endif           ;
169300111012            if vshsnv=' ';
169400081003             protVal =*on     ;
169500101215            endif           ;
169600110406            IF  vshsnvd = *blanks;
169700110406              protVald = *on;
169800110406            ENDIF;
169900111012            if vshsnvd='A';
170000111012             protVala=*on     ;
170100111012            endif           ;
170200101215             //protSGN =*on     ;
170300101215             //else  ;
170400100423             // Senon prevede seegno, proteggo solo questo
170500100423             if vshsgn=' '   ;
170600100423             protSGN =*on     ;
170700100423             endif   ;
170800101215            //endif           ;
170900080703            if vshsns=' ' ;
171000080702             protsped=*on     ;
171100080702            endif           ;
171200081003            if vshsne=' ' ;
171300081003             protfsn =*on     ;
171400081003            endif           ;
171500080702          endif           ;
171600080703          // Se record di nuova immissione
171700081003          //  proteggo annullamento
171800080703            if vshimm='S'  ;
171900080703             Protann=*on  ;
172000080703           endif   ;
172100081006
172200081006          // Se prevista scelta sottotipo col'?' visualizzo valore
172300081006          //   col codice e vicino il '?'
172400081006          if vshifgv='?'    ;
172500081006             protVal =*off    ;
172600110406             protVald = *off;
172700111012             protVala = *off;
172800100423             protSGN =*on     ;
172900081007             if vscspp>*zeros    ;
173000081007             else          ;
173100081007             infoyell=*on    ;
173200081007             endif            ;
173300081006          endif               ;
173400081007          endif               ;
173500101214
173600101214       //?Se INFO prevede segno, INFO traffico totale e tipo trattativa
173700101214       //?NON è 'A' devo bloccare il segno, è sempre un valore positivo
173800101214       //?altrimenti lascio libera scelta all'utente.
173900101214          IF  VSHsgn = 'S' and VSCifo = 'TRT' and I41tpv <> 'A';
174000101215            protsgna = *on;
174100101214          ENDIF;
174200110207
174300110207       //?Se INFO "TRT" e trattativa di tipo 'A' non devo visualizzare
174400110207       //?il campo delle spedizioni
174500110207          IF  VSCifo = 'TRT' and I41tpv = 'A';
174600110207            protsped = *on;
174700110207          ENDIF;
174800080703
174900080702          ENDSR    ;
175000080702       //--------------------------------------------------------------
175100080702       //?conferma aggiornamento
175200080702       //--------------------------------------------------------------
175300080702        BEGSR ConfAGGIO              ;
175400100304
175500090520        clear ultdata   ;
175600141106        wcampagna = *off;
175700081003
175800080704       s01nrr=1  ;
175900100302       chain s01nrr    TA41s01    ;
176000080704
176100080704 1     dow %found    ;
176200080704       // Escludo i record di descrizione categoria
176300111012       // e i rcd con calcolo automatico del dato
176400111012 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
176500080925
176600080925       // se valori calcolati da pgm li pulisco prima dei controlli
176700080925       if vshsnf=' ' and vshprpft=' '   ;
176800080925       clear vscpft   ;
176900080925       clear vscvft   ;
177000080925       endif          ;
177100080925
177200081127       IF VShifgv<>'?'    ;
177300100302         chain    (I41nrv:vscifo:vscspp) TIVII01l  ;
177400081127       else   ;
177500100302         chain    (I41nrv:vscifo:vshspp) TIVII01l  ;
177600081127       endif   ;
177700100302             if %found(TIVII01l)   ;
177800100302             trovavii='S'    ;
177900081127             else ;
178000100302             trovavii='N'    ;
178100081127             endif  ;
178200081127
178300081127       // A n n u l l a m e n t o oppure per qualche strano motivo
178400081127       //   la info c'e'  ma a video non è stata inserita
178500080925
178600100302 3     if trovavii='S'      and (vscatb='A'  or (vscfsn=' ' AND
178700100610                                 vscpft=0 and vscsna=0 and vscval=0
178800110406                                 and vscvald = 0 and vscsgn = ' '));
178900100302         delete TIVII000  ;
179000081003 x3    else    ;
179100080704
179200081127       // V a r i a z i o n e :  verifico se modificati dei dati
179300100302 31    if trovavii='S'     and vscatb=' '   ;
179400141106
179500141106       //?Trattativa di tipo 'A' mi memorizzo se variato Aumento/Sconto
179600141106         IF  I41tpv = 'A' and VSCifo = 'A/S';
179700141106           IF  wsgnausc <> VSHsausc or
179800141106               %abs(VIIvald) <> VSCvald;
179900141106             wcampagna = *on;
180000141106             wVIIvald = VSCvald;
180100141106             wsgnausc = VSHsausc;
180200141106           ENDIF;
180300141106         ENDIF;
180400100423
180500100423       // imposto il segno del campo vlaore per il confronto (sul fil non c'è)
180600100423        clear wiisgn   ;
180700100423
180800101215         SELECT;
180900101215           WHEN  VSHsnv <> *blanks;
181000100423         if viival<0   ;
181100100423           Wiisgn='-'  ;
181200100423          else   ;
181300100423           Wiisgn=' '  ;
181400100423            if   vscsgn='+'   ;
181500100423             Wiisgn='+'  ;
181600100423            endif   ;
181700100423          endif   ;
181800110406
181900110406         WHEN  VSHsnvd <> *blanks;
182000110406           IF  viivald < 0;
182100110406             Wiisgn = '-';
182200110406           ELSE;
182300110406             Wiisgn = ' ';
182400110406             IF  vscsgn = '+';
182500110406               Wiisgn = '+';
182600110406             ENDIF;
182700110406           ENDIF;
182800101215
182900101215       // imposto il segno del campo fatturato per il confronto sul file non c'è
183000101215           WHEN  VSHsnf <> *blanks;
183100101215             IF  VIIpft < 0;
183200101215               wiisgn = '-';
183300101215             ELSE;
183400101215               clear wiisgn;
183500101215               IF  VSCsgn = '+';
183600101215                 wiisgn = '+';
183700101215               ENDIF;
183800101215             ENDIF;
183900101215           ENDSL;
184000100423
184100100302 4     if (viifsn<>vscfsn) or
184200100423          (%abs(viival)<>vscval) or
184300110406          (%abs(viivald) <> vscvald) or
184400100423          (Wiisgn<>vscsgn) or
184500100302          (viisna<>vscsna) or
184600100302          (viispf<>vscspp) or
184700101215          (%abs(viipft)<>vscpft) or
184800100302          (viivft<>vscvft) ;
184900080704          // reimposto data ora profilo immissione
185000100302          viidim=*date    ;
185100090520          ultdata=*date    ;
185200100302          viifil=utefil   ;
185300100302          viipru=knmus    ;
185400080704
185500100302          viifsn= vscfsn  ;
185600081007          IF VShifgv<>'?';
185700101215            IF  VSHsnv <> *blanks ;
185800101215            if vscsgn<>'-'  ;
185900100423             viival= %abs(vscval)  ;
186000100423            else  ;
186100100423             viival=0-%abs(vscval)   ;
186200100423            endif  ;
186300101215            ENDIF;
186400110406            IF  VSHsnvd <> *blanks;
186500110406              IF  vscsgn <> '-';
186600110406               viivald = %abs(vscvald);
186700110406              ELSE;
186800110406               viivald = 0 - %abs(vscvald);
186900110406              ENDIF;
187000110406            ENDIF;
187100101215            IF  VSHsnf <> *blanks ;
187200101215              IF  VSCsgn <> '-';
187300101215                VIIpft = %abs(VSCpft);
187400101215              ELSE;
187500101215                VIIpft = 0 - %abs(VSCpft);
187600101215              ENDIF;
187700101215            ENDIF;
187800101215
187900100302          viitva= vsctva  ;
188000081007          else            ;
188100100302          viispf=vscspp   ;
188200100302          if viispf<>*blanks ;
188300100302          viifsn='S'      ;
188400081007          ENDIF           ;
188500081007          ENDIF           ;
188600100302          viisna= vscsna  ;
188700101215          //viipft= vscpft  ;
188800100302          viivft= vscvft  ;
188900081006          // verifico se dei dati non ci voglio più: una volta aggiunti
189000080704          //  quelli previsti, lo tolgo
189100080704
189200101215 5        if (viival<>0 and vshsnv=' ')    ;
189300101215          if (viisna>0 and vshsns<>' ')  or (viipft<>0  and vshsnf<>' ') or
189400110406             (viifsn<>' ' and vshsne<>' ') or
189500110406             (viivald <> 0 and vshsnvd = ' ') ;
189600100302          clear viival   ;
189700080704          endif          ;
189800080704 5        endif          ;
189900110406 5        IF  (viivald <> 0 and vshsnvd = ' ');
190000110406            IF (viisna > 0 and vshsns <> ' ') or
190100110406               (viipft <> 0  and vshsnf <> ' ') or
190200110406               (viifsn <> ' ' and vshsne <> ' ') or
190300110406               (viival <> 0 and vshsnv = ' ');
190400110406              clear viivald;
190500110406            ENDIF;
190600110406 5        ENDIF;
190700100302 5        if (viisna>0 and vshsns=' ')    ;
190800101215          if (viival<>0 and vshsnv<>' ')  or (viipft<>0  and vshsnf<>' ') or
190900110406             (viifsn<>' ' and vshsne<>' ') or
191000110406             (viivald <> 0 and vshsnvd = ' ') ;
191100100302          clear viisna   ;
191200080704          endif          ;
191300080704 5        endif          ;
191400101215 5        if (viipft<>0 and vshsnf=' ')    ;
191500101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
191600110406             (viifsn<>' ' and vshsne<>' ') or
191700110406             (viivald <> 0 and vshsnvd = ' ') ;
191800100302          clear viipft   ;
191900100302          clear viivft   ;
192000080704          endif          ;
192100080704 5        endif          ;
192200100302 5        if (viifsn<>' ' and vshsne=' ')    ;
192300101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
192400110406             (viipft<>0  and vshsnf<>' ') or
192500110406             (viivald <> 0 and vshsnvd = ' ') ;
192600100302          clear viifsn   ;
192700081006          endif          ;
192800081006 5        endif          ;
192900080704
193000100302        update TIVII000  ;
193100080704 4      endif  ;
193200100423 x3a     else  ;
193300100423         unlock tivii01l   ;
193400081003 3a     endif  ;
193500080704
193600081127       // I m m i s s i o n e
193700100302 3     if trovavii='N'     and vscatb=' ' and (vscfsn<>' ' or
193800110406       vscpft>0 or vscsna>0 or vscval<>0 or vscvald <> 0 or
193900110406       vscsgn <> ' ') ;
194000100302       clear TIVII000;
194100100302        viinrv=I41nrv   ;
194200100302        viitpf=vscifo  ;
194300100302        viispf=vscspp  ;
194400100302        viifsn=vscfsn ;
194500081007        if vshifgv<>'?';
194600101215            IF  VSHsnv <> *blanks;
194700100423            if vscsgn<>'-'   ;
194800100423             viival= %abs(vscval)  ;
194900100423            else  ;
195000100423             viival=0-%abs(vscval)   ;
195100100423            endif  ;
195200101215            ENDIF;
195300110406          IF  VSHsnvd <> *blanks;
195400110406            IF  vscsgn <> '-';
195500110406              viivald = %abs(vscvald);
195600110406            ELSE;
195700110406              viivald = 0 - %abs(vscvald);
195800110406            ENDIF;
195900110406          ENDIF;
196000100302        viitva= vsctva  ;
196100081007        else            ;
196200100302          if viispf<>*blanks ;
196300100302          viifsn='S'      ;
196400081007          endif           ;
196500081007        endif           ;
196600081007
196700100302        viisna= vscsna  ;
196800101215        IF  VSHsnf <> *blanks;
196900101215          IF  VSCsgn <>'-';
197000101215            VIIpft = %abs(VSCpft);
197100101215          ELSE;
197200101215            VIIpft = 0 -%abs(VSCpft);
197300101215          ENDIF;
197400101215        ENDIF;
197500101215        //viipft= vscpft  ;
197600100302        viivft= vscvft  ;
197700100302        viidim=*date    ;
197800090520        ultdata=*date    ;
197900100302        viifil=utefil   ;
198000100302        viipru=knmus    ;
198100141106
198200141106       //?Trattativa di tipo 'A' mi memorizzo che ho scrutto Aumento/Sconto
198300141106         IF  I41tpv = 'A' and VSCifo = 'A/S' and VSCvald <> *zeros;
198400141106           wcampagna = *on;
198500141106           wVIIvald = VSCvald;
198600141106           wsgnausc = VSHsausc;
198700141106         ENDIF;
198800080704
198900100302        write TIVII000 ;
199000110110        feod  TIVII01L;
199100081003  3a    endif           ;
199200081003  3     endif           ;
199300080704  2     endif           ;
199400080704
199500080704        s01nrr=s01nrr+1 ;
199600100302        chain s01nrr    TA41s01    ;
199700080704  1     enddo           ;
199800080704
199900100304         // Info totali inserite
200000100304         // Se inserimento obbligatorio--> posso impostare la "S"
200100100304         //  altrimenti controllo da VII
200200100302        clear waggiovis    ;
200300081003
200400100302  2       if I41obl=' '   ;
200500081015            // Rifaccio il controllo del sfl come se fosse
200600081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
200700100302            savmod=I41mod   ;
200800100302            I41mod='C'      ;
200900100302            I41obl='S' ;
201000081215
201100081015            exsr contrs01   ;
201200081215
201300100302            I41obl=' ' ;
201400100302            I41mod=savmod   ;
201500081015
201600081015  3         if ErrGenerico=*off;
201700100302              Waggiovis='S';
201800081015  3         endif  ;
201900081015
202000081015  x2      else;
202100100302          Waggiovis='S'   ;
202200081015  2       endif           ;
202300081015
202400100302              O41ifotot=Waggiovis   ;
202500141106
202600141106       //?Se il cliente è in campagna devo scrivere la fase per OBJ da TTR
202700150114       //?per ogni campagna aperta
202800141106         IF  wcampagna;
202900150114           clear TRKC73DS;
203000141106           chain (I41nrv) TIVIS05L;
203100141106           IF  not %found(TIVIS05L) or VISksc = 0;
203200141106             leavesr;
203300141106           ENDIF;
203400150114           IKC73ksu = VISksc;
203500150114           IKC73dat = VISdat;
203600150206           IKC73ftr = 'S';
203700150114           TRKC73r (kpjba:TRKC73DS);
203800150114           IF  OKC73err <> *blanks;
203900141106             leavesr;
204000141106           ENDIF;
204100150114       //?Se arrivo qua ok cliente sulla trattativa
204200150114       //?Per ogni campagna ancora aperta sul cliente devo scrivere la fase TR
204300150114           xx = 1;
204400150114           FOR xx by 1 to %elem(skOKC73ncm);
204500150114             IF  skOKC73ncm(xx) > *zeros;
204600150114           //?quindi richiamo pgm scrittura fase
204700150114               clear TRKC71DS;
204800150114               IKC71ncm = skOKC73ncm(xx);
204900150114               IKC71ksu = OKC73ksu;
205000150114               IKC71acm = FaseObjTtr;
205100150114               IKC71pea = wVIIvald;
205200150114               IF  wsgnausc = '-';
205300150114                 IKC71pea = IKC71pea * -1;
205400150114               ENDIF;
205500150114               IKC71nrv = I41nrv;
205600150114               TRKC71R (kpjba:TRKC71DS);
205700150114               IF  OKC71err = 'E';
205800150114                 leavesr;
205900150114               ENDIF;
206000150114             ENDIF;
206100150114           ENDFOR;
206200141106         ENDIF;
206300100304
206400080702        ENDSR;
206500080702
206600080206       //--------------------------------------------------------------
206700080206       //?Operazioni finali.
206800080206       //--------------------------------------------------------------
206900080206       BEGSR RoutEnd;
207000080627
207100110218         //if I41tla<>' '    ;
207200080704         // Chiusura pgm   ;
207300080704         clear tibs02ds  ;
207400080704         t02tla='C'      ;
207500080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
207600080704
207700080206         *inLR = *on;
207800110218         //endif             ;
207900080704
208000080206         return;
208100080206
208200080206       ENDSR;
208300080206
208400080206      /end-free
208500080206
208600080206       //--------------------------------------------------------------
208700080206       //?Schiere a tempo di compilazione.
208800080206       //--------------------------------------------------------------
208900080206
209000080410** - MSG ------------------------------------------------------------------ -*
209100100302Immettere INFO.                                                                1
209200100302Per Info non presente (N), non indicare gli altri dati                         2
209300100302Info non più in essere: ANNULLARLA!!                                           3
209400100302La somma delle percentuali per questa categoria di info supera il 100%         4
209500100302La percentuale di questa info supera 100%                                      5
209600100302La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx deve essere 100%  6
209700100302Inserire almeno una info per la categoria xxxxxxxxxxxxxxxxxxxxx                7
209800100302Immettere solo  '?'  se si vuole attivare la ricerca                           8
209900100302Indicare presenza o assenza della INFO con "S o N".                            9
210000100302Codice già presente !!                                                        10
210100100423Non ammesso un valore negativo                                                11
210200100303Se immesso "NESSUN CONCORRENTE" non inserire altri concorrenti                12
210300100917ATTENZIONE!! Il valore massimo consentito è di 99 milioni                     13
210400101230Segno TRAFFICO TOTALE non congruente con segno AUMENTO/SCONTO                 14
210500111021Valore Peso Medio consentito: max 5.000 Kg.                                   15
210600110408Il Valore del Traffico Estero non può superare quello del Traffico Totale     16
210700110408Il n. di spedizioni Estere non può superare il n. di spedizioni Totale        17
210800111012Il Valore del Traffico non può essere inferiore alle Spedizioni Annue.        18
210900111028Attenzione!! Peso medio maggiore/uguale a 500 Kg. Enter per forzare.          19
211000111021Valori del Delta consentiti: max +99%; min -99%                               20
211100111012Attenzione inserito un delta oltre x30%. Enter per forzare!                   21
211200111012Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                22
211300111012Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!               23
211400120608Info non più in essere: verificarla e se non serve annullarla. ENTER forza!   24
