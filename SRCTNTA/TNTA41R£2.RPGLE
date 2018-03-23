000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300100302      //?TNTA41R - Gestione/interrogazione informazioni commerciali
000400100303      //
000500100303      // ? ATTENZIONE!!!!! ?
000600100303      // ?  CHIODI fissi per:
000700100303      // ?    info '10'  concorrenti
000800100303      // ?    info 'DEL' delta
000900101215      // ?    info 'TRT' Traffico totale
001000110110      // ?    info 'A/S' Aumento/Sconto
001100110117      // ?    info 'KMS' Peso medio
001200110117      // ?    info 'TRE' Traffico estero
001300080206      //--------------------------------------------------------------
001400080206
001500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001600080206
001700080206      //---------------------------------------------------------------
001800080206      //?Dichiarazione file.
001900080206      //---------------------------------------------------------------
002000050704
002100100304     fTIVII01L  uf a e           k disk
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
005500080206
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione schiere.
005800080206      //---------------------------------------------------------------
005900080206
006000080206      // - Messaggi di errore
006100100304     d msg             s             78    dim(30) ctdata perrcd(1)
006200100302     d ito             s              3    dim(50)
006300100302     d itods           s             70    dim(50)
006400100302     d itoor           s              8    dim(50)
006500100303     d itoval          s              8    dim(100)
006600100303     d its             s              4    dim(99)
006700081126
006800080206      //---------------------------------------------------------------
006900080206      //?Definizione aree dati.
007000080206      //---------------------------------------------------------------
007100100226
007200080206      // - Dati utente
007300080206     d §AzUte        e ds                  extname(AZUTE00F)
007400080206     d                                     dtaara
007500080206     d §DatiUte      e ds                  extname(dDatiUte)
007600080206     d                                     dtaara
007700080206
007800080206      //---------------------------------------------------------------
007900080206      //?Definizione strutture dati.
008000080206      //---------------------------------------------------------------
008100080206
008200080206      // - InfDS
008300080206     d InfDspF         ds
008400080207     d  dsp_aid              369    369a
008500080206
008600080206      // - Indicatori su DspF
008700080208     d IndDspF         ds
008800080206        // - Indicatori di gestione del subfile
008900080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009000080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009100080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009200080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009300080206        // - Indicatori di errore
009400100304     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500100304     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009600080627        // - Indicatori vari
009700080627     d  InfoInt                       1n   overlay(IndDspF : 02)
009800080704     d  Infoyell                      1n   overlay(IndDspF : 03)
009900081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
010000080627     d  InfoImmiss                    1n   overlay(IndDspF : 10)
010100080703     d  ProtAnn                       1n   overlay(IndDspF : 13)
010200100302     d  ifosott                       1n   overlay(IndDspF : 15)
010300080627     d  protriga                      1n   overlay(IndDspF : 16)
010400080627     d  protFat                       1n   overlay(IndDspF : 17)
010500081003     d  protVal                       1n   overlay(IndDspF : 18)
010600080627     d  protSped                      1n   overlay(IndDspF : 19)
010700080702     d  protfsn                       1n   overlay(IndDspF : 20)
010800080925     d  PFTBLU                        1n   overlay(IndDspF : 21)
010900100423     d  protSGN                       1n   overlay(IndDspF : 22)
011000101214     d  protSGNa                      1n   overlay(IndDspF : 23)
011100110406     d  protVald                      1n   overlay(IndDspF : 24)
011200111012     d  protvala                      1n   overlay(IndDspF : 25)
011300080703     d  ErrSNA                        1n   overlay(IndDspF : 40)
011400080703     d  ErrFsn                        1n   overlay(IndDspF : 41)
011500081003     d  ErrVAL                        1n   overlay(IndDspF : 42)
011600110406     d  ErrVALD                       1n   overlay(IndDspF : 43)
011700080703     d  ErrPFT                        1n   overlay(IndDspF : 44)
011800080704     d  ErrANN                        1n   overlay(IndDspF : 45)
011900080703     d  PotAnnul                      1n   overlay(IndDspF : 70)
012000080206
012100080206      // - Parametri ricevuti
012200080206     d KPJBA         e ds
012300100302     d TNTA41DS      e ds
012400080206
012500080206      // - Ricerca/Controllo tabelle
012600080206     d TIBS02ds      e ds                  inz
012700100304
012800100304      // - Reperimento dati utente
012900100304     d TIBS34DS      e ds
013000080206
013100100304      // - Tabella  Generica azienda
013200100304     d Dged          e ds
013300100304
013400080627      // - Tabella  Info commerciali e Categorie info
013500100304     d dITG          e ds                  inz
013600100304     d dITO          e ds                  inz
013700100304     d dITS          e ds                  inz
013800100302
013900080206      //---------------------------------------------------------------
014000080206      //?Definizione variabili globali.
014100080206      //---------------------------------------------------------------
014200080206
014300080206      // - Flags booleani
014400080208     d $Fine           s               n   inz(*off)
014500080208     d $InzS01         s               n   inz(*on)
014600111012     d $ForzaKg        s               n   inz(*off)
014700111012     d $ForzaDel       s               n   inz(*off)
014800111012     d $ForzaRms       s               n   inz(*off)
014900080206
015000080627     d $Video          s              2    inz('S1')
015100080208     d S01nrr          s              4  0 inz
015200080627     d savcat          s              2    inz
015300080627     d Wcat            s              2    inz
015400100302     d Wits            s              4    inz
015500100302     d Wito            s              3    inz
015600081006     d w0040           s              4  0 inz
015700081007     d w005a           s              5    inz
015800081006     d XX              s              3  0 inz
015900080627     d Indx            s              3  0 inz
016000080703     d Primavolta      s              1    inz
016100080925     d Totprc          s              4  0 inz
016200081003     d Wctrprc         s              1    inz
016300081003     d Wdescat         s             21    inz
016400100302     d SPTPFT          s                   like(viipft) inz
016500100302     d Waggiovis       s              1    inz
016600081126     d w008a           s              8    inz
016700081126     d yy              s              3  0 inz
016800100302     d trovavii        s              1    inz
016900100302     d savmod          s                   like(I41mod) inz
017000090520     d ultdata         s              8  0 inz
017100100302     d $okUNA          s              1n   inz(*off)
017200100305     d $okqualcosa     s              1n   inz(*off)
017300100423     d WiiSGN          s              1    inz
017400101230     d wsgntrt         s                   like(vscsgn) inz
017500101230     d wsgnausc        s                   like(vscsgn) inz
017600110207     d sav_vscpft      s                   like(vscpft) inz
017700110324     d sav_vscsna      s                   like(vscsna) inz
017800111012     d sav_valkg       s                   like(vscval) inz
017900111012     d sav_valdel      s                   like(vscval) inz
018000111012     d sav_sgndel      s                   like(vscsgn) inz
018100111019     d sav_valrms      s             11  3 inz
018200111019     d wVALrms         s             11  3 inz
018300101230
018400080605     d Dataiso         s               d   datfmt(*iso)
018500080605     d Datadmy         s               d   datfmt(*dmy)
018600100304
018700081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
018800080206
018900080206      //---------------------------------------------------------------
019000080206      //?Definizione procedure usate.
019100080206      //---------------------------------------------------------------
019200080414      /copy gaitrasrc/srcprotopr,tibs02r
019300080414      /copy gaitrasrc/srcprotopr,tibs34r
019400080206
019500080206      //---------------------------------------------------------------
019600080206      //?Riepilogo indicatori.
019700080206      //---------------------------------------------------------------
019800080207      // 28    : Emissione messaggio di errore a video
019900100304      // 30    : Condiziona SFLDSP
020000100304      // 31    : Condiziona SFLDSPCTL
020100100304      // 32    : Condiziona SFLNXTCHG
020200080207      // 50    : Errore: Opzione errata
020300080207      // 99    : Generico di Errore
020400080206      //---------------------------------------------------------------
020500080206
020600080206      //---------------------------------------------------------------
020700080206      //?M A I N - L I N E
020800080206      //---------------------------------------------------------------
020900080206
021000080627     c     *Entry        plist
021100080206     c                   parm                    KPJBA
021200100304     c                   parm                    TNTA41DS
021300080627
021400080206      /free
021500100303
021600100303        clear o41f12;
021700100303        clear o41f03;
021800100303        clear o41f06;
021900100303        clear o41ifotot;
022000080206
022100100302 1      if  I41tla<>'C'   ;
022200080925
022300080206       // Operazioni iniziali
022400080206       exsr RoutInz;
022500080206
022600100302 2      if  I41mod<>'C'   ;
022700081215
022800100304         // Gestione video
022900100304 3       DOW $Fine = *off;
023000100304
023100100304 4         select;
023200100304           // Videata di selezioni
023300100304             when $Video = 'S1';
023400100304               exsr GesS01;
023500111012             when $Video = 'W1';
023600111012               exsr GesW01;
023700100304             other   ;
023800100304               $Fine = *on;
023900100304 4         endsl;
024000100304
024100100304 3       ENDDO;
024200081215
024300081215 x2    else   ;
024400081215
024500100304         // modalità CONTROLLO : carico il sfl
024600100304         //                    solo ai fini del controllo
024700100304         exsr InzS01;
024800100304         exsr Contrs01   ;
024900081215
025000100304  3      if ErrGenerico=*off;
025100100304            O41ifotot='S';
025200100304  3      endif  ;
025300080925
025400081215 2     endif    ;
025500081215 1     endif    ;
025600080206
025700080206       // Operazioni finali
025800080206       exsr RoutEnd;
025900080206
026000080206       //--------------------------------------------------------------
026100080206       //?Operazioni iniziali.
026200080206       //--------------------------------------------------------------
026300080206       BEGSR RoutInz;
026400100303
026500080627       $inzs01=*on;
026600080929       clear vsctes2         ;
026700080206
026800080703       // Solo la prima volta
026900080703 1     if primavolta=' '   ;
027000080703
027100080206         // Reperimento dati job
027200080206         exsr DatiJob;
027300081003
027400081003         // Se sono EDP accendo indicatore
027500081003         if %subst(knmus:1:3)<>'EDP'    ;
027600081003         UTENonEDP=*on   ;
027700081003         endif      ;
027800080627
027900080627         // Carico tabella per divisa corrente
028000080627         reset TIBS02ds;
028100080627         T02sif = knsif;
028200080627         T02cod = 'GED';
028300080627         T02ke1 = '1'  ;
028400080627         T02mod = 'C'  ;
028500080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
028600080627
028700080703 2       if t02err=' '  ;
028800080627         dGED=t02uni    ;
028900080627         else   ;
029000080627         clear DGED    ;
029100080703 2       endif   ;
029200101215
029300101215         Primavolta='N'   ;
029400101215 1       endif      ;
029500080627
029600080627         // Carico tabella info commerciali
029700080627         xx=0                     ;
029800101215         clear ito;
029900101215         clear itods;
030000101215         clear itoor;
030100100302         setll ('ITO') tntbe01l   ;
030200100302         READE ('ITO') tntbe01l   ;
030300080627
030400101215         dow not %eof(tntbe01l)   ;
030500101215         if tbeatb=' '    ;
030600101214         //?Se tipo trattativa <> 'A' NON carico la info 'A/S'
030700101214           IF  I41tpv <> 'A' and  TBEke1 = 'A/S';
030800101214             reade ('ITO') TNTBE01L;
030900101214             iter;
031000101214           ENDIF;
031100110117         //?Se tipo trattativa = 'A' NON carico la info 'TRE' e 'KMS'
031200111012         //?e la 'RMS'
031300111012           IF  I41tpv = 'A' and  (TBEke1 = 'TRE' or TBEke1 = 'KMS' or
031400111012                                  TBEke1 = 'RMS');
031500110117             reade ('ITO') TNTBE01L;
031600110117             iter;
031700110117           ENDIF;
031800100302           dito=tbeuni    ;
031900080627           xx=xx+1        ;
032000100302           ito(xx)=tbeke1 ;
032100111024         //?Se tipo trattativa = 'A' devo modificare la descrizione x 'TRT'
032200111024           IF  I41tpv = 'A' and  TBEke1 = 'TRT';
032300111024             §ITOdes = 'Valore Aumento/Sconto';
032400111024           ENDIF;
032500100302           itods(xx)=dito ;
032600080627           // ordino in base alla categoria e  num.ordinamento
032700100302           itoor(xx)=§itoitg+%editc(§itoogi:'X')+tbeke1 ;
032800101215         endif               ;
032900080627
033000100302         READE ('ITO') tntbe01l   ;
033100101215         enddo    ;
033200080627
033300100302         sorta  itoOR  ;
033400080702
033500080703         Infoint=*off   ;
033600080704         Infoyell=*off   ;
033700080703         InfoImmiss=*off   ;
033800100302         ifosott=*off   ;
033900080703         Protriga=*off   ;
034000080703         ProtFat =*off   ;
034100081003         ProtVal  =*off   ;
034200110406         ProtVald = *off;
034300111012         ProtVala = *off;
034400100423         ProtSGN  =*off   ;
034500101214         ProtSGNa =*off   ;
034600080703         Protsped =*off   ;
034700080703         Protfsn  =*off   ;
034800080703         PotAnnul =*off   ;
034900080703         clear wcat  ;
035000100302         clear wito  ;
035100080704         $Fine=*off   ;
035200080703
035300080702         // Vedo se modo gestione  o interrogazione
035400100302 1       if I41mod='I' ;
035500080702         InfoInt =*on  ;
035600080703         vsctes2='INTERROGAZIONE'    ;
035700080703 1       endif;
035800080703
035900100302         vscpgm='TNTA41R'   ;
036000080703
036100100302         // IMPOSTO la trattativa in visualizzazione
036200100302         vscnrv=%editc(I41nrv:'Z')   ;
036300100302         vscrag=I41rag   ;
036400081003
036500100302         // Se già  inserite tutte le info, non si possono più annullare
036600081003         //  ma solo sistemare
036700100302         if I41obl=' '  and I41ifotot='S'    ;
036800100302         I41obl='S'   ;
036900081003         endif        ;
037000080627
037100100302         // VERIFICO SE ci sono già dei dati per la trattativa
037200100302         setll   I41nrv  TIVII01l    ;
037300100302         reade   I41nrv  TIVII01l    ;
037400100302 1       dow not %eof(TIVII01l) and viiatb<>' ' ;
037500100302         reade   I41nrv  TIVII01l    ;
037600080703 1       enddo   ;
037700080703
037800100302 1       if %eof(TIVII01l) ;
037900080703         Infoimmiss=*on     ;
038000080929 1       if vsctes2= *blanks    ;
038100080703         vsctes2='  IMMISSIONE  '    ;
038200080929         endif   ;
038300080703         else    ;
038400080929 1       if vsctes2= *blanks    ;
038500080703         vsctes2='  VARIAZIONE  '    ;
038600080703 1       endif    ;
038700080929 1       endif    ;
038800101214
038900101214       //?Se non è immissione e tipo trattativa <> 'A' devo deletare
039000101215       //?la info 'A/S' se sono in gestione
039100101215         IF  not InfoImmiss and I41tpv <> 'A' and I41mod = 'G';
039200101214           wito = 'A/S';
039300101214           chain (I41nrv:wito) TIVII01L;
039400101214           IF  %found(TIVII01L);
039500101214             delete tivii000;
039600101214           ENDIF;
039700101214         ENDIF;
039800080703
039900080627         ENDSR;
040000080206
040100080206       //--------------------------------------------------------------
040200080206       //?Reperimento Dati del job (Utente/Operativi).
040300080206       //--------------------------------------------------------------
040400080206       BEGSR DatiJob;
040500080206
040600080206         in(E) §AzUte;
040700080206         if NOT %error;
040800080206           in(E) §DatiUte;
040900080206         endif;
041000080206         if %error or RSut = *blanks;
041100080206           clear TIBS34ds;
041200080206           tibs34r(tibs34ds);
041300080206           in §AzUte;
041400080206           in §DatiUte;
041500080206         endif;
041600080206
041700080206       ENDSR;
041800080206
041900080206       //--------------------------------------------------------------
042000080627       //?Gestione SFL 01
042100080206       //--------------------------------------------------------------
042200080409       BEGSR GesS01;
042300080207
042400080207         // Inizializzazione videata
042500080409         if  $InzS01 = *on;
042600080409            exsr InzS01;
042700080703         // Posizionamento cursore
042800080703           s01nrr = 1;
042900080703           sfldsp_n=*off;
043000110406           $InzS01  = *off;
043100080207         endif;
043200080207
043300080207
043400080207         // Emissione Testata e Piede con tasti funzionali abilitati
043500080207         if  errGenerico = *off;
043600100302           write  TA41Z01;
043700080207         endif;
043800080703
043900080703         // Posizionamento cursore
044000080703         if  C01csr  > *zero;
044100080703           C01rcd = C01csr;
044200080703         endif  ;
044300080207
044400080207         // Emissione videata
044500100302         exfmt  TA41C01;
044600080410
044700080207         reset errMessage;
044800080207         reset errGenerico;
044900080702         clear Vscmsg;
045000081003         errVAL=*off     ;
045100110406         errVALD = *off;
045200080703         errPFT=*off     ;
045300080703         errSNA=*off     ;
045400080703         errFSN=*off     ;
045500080704         errANN=*off     ;
045600080207
045700080523 1       SELECT;
045800080207
045900080207         // - F3=Fine
046000080523 1         WHEN  dsp_aid = c_F03;
046100080409            $Fine = *on;
046200100302            O41f03='S'   ;
046300080207
046400080207         // - F12=Ritorno
046500080523 1         WHEN  dsp_aid = c_F12;
046600080702            $Fine = *on;
046700100302            O41f12='S'   ;
046800080414
046900080530 x1      // Invio / F6
047000080207           OTHER;
047100081006
047200081006        // Non effettuo controlli in interrogazione
047300100302 1           if I41mod<>'I' ;
047400081006               exsr ContrS01 ;
047500081006 2             if  errGenerico = *on;
047600111012                 $Video = 'W1';
047700081006                 leavesr;
047800081006 2             endif;
047900081006 2           endif;
048000080530
048100100302         // F6=conferma Aggiornamento
048200100302 1         if   dsp_aid = c_F06;
048300080702           exsr   ConfAggio ;
048400100305           // ritorno il flag di F6 se hanno inserito qualcosa
048500100305           // può capitare che in immissione diano F6 senza aver
048600100305           // inserito niente a video
048700100305           IF  $okqualcosa;
048800100302           O41f06='S'   ;
048900100305           ENDIF;
049000081023
049100080704           $Fine = *on;
049200080530           endif  ;
049300080207
049400080523 1       ENDSL;
049500080207
049600080207       ENDSR;
049700111012
049800111012       //--------------------------------------------------------------
049900111012       //?Gestione Window errori
050000111012       //--------------------------------------------------------------
050100111012       BEGSR GesW01;
050200111012
050300111012         reset errMessage;
050400111012         reset errGenerico;
050500111012
050600111012         w1riga = vscmsg;
050700111012         exfmt  TA41W01;
050800111012
050900111012         clear Vscmsg;
051000111012         errVAL  = *off;
051100111012         errVALD = *off;
051200111012         errPFT  = *off;
051300111012         errSNA  = *off;
051400111012         errFSN  = *off;
051500111012         errANN  = *off;
051600111012
051700111012         $Video = 'S1';
051800111012
051900111012       ENDSR;
052000080207
052100080526       //--------------------------------------------------------------
052200080702       //?controlli SFL01
052300080409       //--------------------------------------------------------------
052400080409       BEGSR ContrS01;
052500100303
052600100303       clear o41ifomin;
052700081215       clear wctrprc  ;
052800081006       clear wdescat   ;
052900081006       clear totprc   ;
053000081006       clear sptpft   ;
053100100302       clear itoval   ;
053200100303       clear its;
053300100302
053400100302       $okUNA = *off;
053500100302
053600081126       yy=1           ;
053700080703       s01nrr=1  ;
053800100302       chain s01nrr    TA41s01    ;
053900080703
054000081006 0     dow %found    ;
054100080925       // Escludo i record di descrizione categoria
054200081006 1     if vshimm= 'C'    ;
054300081003
054400100302       // A cambio di categoria
054500100302       // se c'era controllo obbligatorio del 100%
054600100302       // controllo ed eventualmente segnalo errore
054700100302       // in alternativa al controllo 100% c'è se almeno una INFO
054800100302       // deve essere inserita
054900081006       EXSR CambioCAT    ;
055000081006       if errGenerico=*on  ;
055100081006       leavesr;
055200081006       endif              ;
055300081006
055400081006x1     else    ;
055500081006
055600080925       pftblu=*off       ;
055700080925
055800081003       // se valori calcolati da pgm li pulisco prima dei controlli
055900081007 2     if vshsnf=' ' and vshprpft=' '   ;
056000080925       clear vscpft   ;
056100080925       clear vscvft   ;
056200081007 2     endif          ;
056300080703
056400080703       // Imposto la divisa, se impostato il fatturato
056500081007 2     if vscpft>0 and vscvft=*blanks   ;
056600080703       vscvft=§gedcr   ;
056700100423       //errGenerico = *on;
056800081007 2     endif    ;
056900080703
057000081007 2     if vscpft=0 and vscvft<>*blanks   ;
057100080703       clear vscvft   ;
057200080703       errGenerico = *on;
057300081007 2     endif   ;
057400100917
057500110407       //?Se valore fatturato impostato e > di 99000000 errore
057600110407       IF  VSCpft > 99000000;
057700100917         VSCmsg = msg(13);
057800100917         errPFT=*on;
057900100917         EXSR AggioSFL   ;
058000100917         leavesr;
058100100917       ENDIF;
058200111012
058300111012       //?Le spedizioni non possono essere superiori del valore fatturato
058400111012       IF  VSCsna > VSCpft;
058500111012         VSCmsg = msg(18);
058600111012         errPFT=*on;
058700111012         EXSR AggioSFL   ;
058800111012         leavesr;
058900111012       ENDIF;
059000080703
059100080704       // se info da annullare e non c'e' la "A", msg forzabile
059200081215       //  non faccio se modalità controlli
059300100302 2     if I41mod<>'C' and vshann='S'  and vscatb=' '  ;
059400080704           errANN=*on;
059500080704           // se  contiene dati, msg forzabile
059600100302 3         if vscfsn='S'or vscval<>0 or vscpft>0 or vscsna>0   ;
059700120608           vscmsg = Msg(24);
059800080704           vshann='X'      ;
059900080704           // Se non contiene dati abbligatorio l'annullamento
060000081007 x3        else            ;
060100100302           vscmsg = Msg(03);
060200081007 3         endif           ;
060300080704           EXSR AggioSFL   ;
060400080704           leavesr;
060500081007 2     endif    ;
060600120608
060700120608       // se c'e' "O"  obbligatorio l'annullamento
060800120608       if I41mod<>'C' and vshann='O'  and vscatb=' '  ;
060900120608           errANN=*on;
061000120608           vscmsg = Msg(03);
061100120608           EXSR AggioSFL   ;
061200120608           leavesr;
061300120608       endif    ;
061400080704
061500080703       // Se richiesto inserimento obbligatorio, per i dati
061600080703       //  obbligatori da tabella segnalo errore
061700100305 2     if  I41obl='S' and vshobl='S';
061800081007 3     if vscatb='A'    or
061900081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
062000081003         if vshsne<>' ';
062100080703           errFSN=*on;
062200081003         endif   ;
062300081003         if vshsnv<>' ';
062400081003           errVAL=*on;
062500081003         endif   ;
062600110406         IF  vshsnvd <> *blanks;
062700110406           errVALD = *on;
062800110406         ENDIF;
062900081003         if vshsnf<>' ';
063000081003           errPFT=*on;
063100081003         endif   ;
063200080703           vscmsg = Msg(01);
063300080703           EXSR AggioSFL   ;
063400080703           leavesr;
063500081007 3     endif   ;
063600081007 2     endif   ;
063700081007
063800081007       // Non accetto se non previsto il valore '?'
063900100805       //?Dal subfile ho tolto la possibilità di inserire '?'
064000081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
064100081007           errFSN=*on;
064200100302           vscmsg = Msg(08);
064300081007           EXSR AggioSFL   ;
064400081007           leavesr;
064500081007 3     endif    ;
064600081007       if vshifgv<>'?' and vscfsn='?'  ;
064700081007           errFSN=*on;
064800100302           vscmsg = Msg(09);
064900081007           EXSR AggioSFL   ;
065000081007           leavesr;
065100081007 3     endif    ;
065200080703
065300080703       // Se immesso "N" non indicare gli altri campi
065400100805       //?Dal subfile ho tolto la possibilità di inserire 'N'
065500100302 2     if vscfsn='N' and (vscval<>0 or vscpft>0 or vscsna>0)   ;
065600080703       if vshsnf<>' '    ;
065700080703           errPFT=*on;
065800080703       else    ;
065900081003          errVAL=*on ;
066000080703       endif  ;
066100080703           vscmsg = Msg(02);
066200080703           EXSR AggioSFL   ;
066300080703           leavesr;
066400081007 2     endif   ;
066500080703
066600081003       // Se immesso almeno un dato, controllo gli obbligatori ed
066700081215       //   i facoltativi. I  facoltativi non li guardo in modalità
066800081215       //   solo controllo
066900081031       if vscatb =' ' and
067000100302 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval<>0)     ;
067100081003
067200081007 3     if vscpft=0    and vshsnf='O'   ;
067300081003           errPFT=*on;
067400100302           vscmsg = Msg(01);
067500081003           EXSR AggioSFL   ;
067600081003           leavesr;
067700081007 3      endif    ;
067800100503
067900100503        // Lo opzionali le chiedo solo se info obbligatorie
068000100503       if  I41obl='S' and
068100100503 3        I41mod<>'C' and  vscpft=0    and vshsnf='S'   ;
068200081003           errPFT=*on;
068300100302           vscmsg = Msg(01);
068400100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
068500081003           vshsnf='X'      ;
068600081003           EXSR AggioSFL   ;
068700081003           leavesr;
068800081007 3     endif    ;
068900110118       //?OBBLIGO delle spedizioni se tipo trattativa <> 'A'
069000110118 3     if vscsna=0    and vshsns='O'   and  I41tpv <> 'A';
069100081003           errSNA=*on;
069200100302           vscmsg = Msg(01);
069300081003           EXSR AggioSFL   ;
069400081003           leavesr;
069500081007 3     endif    ;
069600100503       if  I41obl='S' and
069700100503 3        I41mod<>'C' and vscsna=0    and vshsns='S'   ;
069800081003           errSNA=*on;
069900100302           vscmsg = Msg(01);
070000100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
070100081003           vshsns='X'      ;
070200081003           EXSR AggioSFL   ;
070300081003           leavesr;
070400081007 3     endif    ;
070500081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
070600081007           errFSN=*on;
070700100302           vscmsg = Msg(09);
070800081007           EXSR AggioSFL   ;
070900081007           leavesr;
071000081007 3     endif    ;
071100100503       if  I41obl='S' and
071200100503 3        I41mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
071300081007           errFSN=*on;
071400100302           vscmsg = Msg(09);
071500100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
071600081007           vshsne='X'      ;
071700081007           EXSR AggioSFL   ;
071800081007           leavesr;
071900081007 3     endif    ;
072000081007 3     if (vscval=0    and vshsnv='O')  or
072100100503          (vscval=0    and vshsnv='S' and I41mod<>'C' AND I41OBL='S') ;
072200081007 4        if vshsnv='O'    ;
072300100302            vscmsg = Msg(01);
072400081003          else   ;
072500100302            vscmsg = Msg(01);
072600100302            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
072700100302            vshsnv='X'      ;
072800081215 4        endif  ;
072900081003           errVAL=*on;
073000081003           EXSR AggioSFL   ;
073100081003           leavesr;
073200081215 3     endif    ;
073300110406 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
073400110406           (vscvald = 0 and vshsnvd = 'S'   and
073500110406            I41mod <> 'C' and I41obl = 'S');
073600110406 4        IF  vshsnvd = 'O';
073700110406            vscmsg = Msg(01);
073800110406          ELSE;
073900110406            vscmsg = Msg(01);
074000110406            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
074100110406            vshsnvd = 'X';
074200110406 4        ENDIF;
074300110406          errVALD = *on;
074400110406          EXSR AggioSFL;
074500110406          leavesr;
074600110406 3     ENDIF;
074700110207
074800110207       //?Mi salvo il valore del traffico totale
074900110207 3     IF  vscifo = 'TRT';
075000110207       sav_vscpft = vscpft;
075100110324       sav_vscsna = vscsna;
075200110207       ENDIF;
075300110207
075400110207       //?Il traffico estero è un 'di cui' quindi non può superare
075500110207       //?il traffico totale
075600110324       //?Stessa cosa per le spedizioni
075700110324       IF  vscifo = 'TRE' and vscpft > sav_vscpft;
075800110207         errPFT = *on;
075900110408         vscmsg = msg(16);
076000110207         EXSR AggioSFL;
076100110207         leavesr;
076200110324       ENDIF;
076300110324       IF  vscifo = 'TRE' and vscsna > sav_vscsna;
076400110324         errSNA = *on;
076500110408         vscmsg = msg(17);
076600110324         EXSR AggioSFL;
076700110324         leavesr;
076800110324       ENDIF;
076900110112
077000111012       //?Solo per info "KMS" peso medio
077100111012       IF  VSCval <> 0 and  VSCtva = 'KG ' and VSCifo = 'KMS';
077200111012       //?reimposto il flag di forzatura
077300111012         IF  VSCval <> sav_valkg;
077400111012           $ForzaKg = *off;
077500111012         ENDIF;
077600111012       //?se valore in kg non deve superare i 5000 kg.
077700111012         IF  VSCval > 5000;
077800111012           errVAL = *on;
077900111012           VSCmsg = msg(15);
078000111012           exsr AggioSFL;
078100111012           sav_valkg = VSCval;
078200111012           leavesr;
078300111012         ENDIF;
078400111028       //?msg info se è ugale o supera i 500 kg.
078500111028         IF  VSCval >= 500 and VSCval <> sav_valkg and not $ForzaKg;
078600111012           errVAL = *on;
078700111012           VSCmsg = msg(19);
078800111012           exsr AggioSFL;
078900111012           $ForzaKg = *on;
079000111012           sav_valkg = VSCval;
079100111012           leavesr;
079200111012         ENDIF;
079300111012       ENDIF;
079400111012
079500111012       //?Solo per info "DEL" delta
079600111012       IF  VSCval <> 0 and VSCtva ='%  ' and VSCifo = 'DEL';
079700111012       //?reimposto il flag di forzatura
079800111012         IF  VSCval <> sav_valdel or VSCsgn <> sav_sgndel;
079900111012           $ForzaDel = *off;
080000111012         ENDIF;
080100111012       //?non posso inserire più di 2 cifre quindi al massimo 99
080200111012         IF  VSCval > 99;
080300111012           errVAL = *on;
080400111012           VSCmsg = msg(20);
080500111012           exsr AggioSFL;
080600111012           sav_valdel = VSCval;
080700111012           sav_sgndel = VSCsgn;
080800111012           leavesr;
080900111012         ENDIF;
081000111012       //?msg info se supera 30 +/- indifferente
081100111028         IF  VSCval > 30 and not $ForzaDel and
081200111012            (VSCval <> sav_valdel or VSCsgn <> sav_sgndel);
081300111012           errVAL = *on;
081400111012           VSCmsg = msg(21);
081500111012           %subst(VSCmsg:36:1) = vscsgn;
081600111012           exsr AggioSFL;
081700111012           $ForzaDel = *on;
081800111012           sav_valdel = VSCval;
081900111012           sav_sgndel = VSCsgn;
082000111012           leavesr;
082100111012         ENDIF;
082200111012       ENDIF;
082300081003
082400081003       // se Valore in % --> non deve superare 100%
082500100302 3     if  vscval<>0 and vsctva='%  '    ;
082600081007 4       if vscval>100    ;
082700081003          errVAL=*on ;
082800100302          vscmsg = Msg(05);
082900081003          EXSR AggioSFL   ;
083000081003          leavesr;
083100081007 4       endif      ;
083200081003
083300080925       // Sommo la percentuale se presente per ogni categoria
083400080925       //  se supera 100% segnalo errore
083500081006       //  solo se previsto controllo sulla %
083600081007 4     if wctrprc='P'     ;
083700081006
083800081003       totprc=totprc+vscval    ;
083900081007 5       if totprc>100    ;
084000081003         errVAL=*on ;
084100100302         vscmsg = Msg(04);
084200080925         EXSR AggioSFL   ;
084300080925         leavesr;
084400081007 5       endif      ;
084500081007 4     endif      ;
084600080925
084700100302       // Se immessa la %, visualizzo in base a INFO "SPT"
084800080925       exsr   VISUALimp     ;
084900081007 3     endif      ;
085000100303
085100081007 2     endif      ;
085200110406
085300110406       //?Se Valore con decimali in % --> non deve superare 100%
085400110406 3     IF  vscvald <> 0 and vsctva = '%  ';
085500110406 4       IF  vscvald > 100;
085600110406           errVALD = *on;
085700110406           vscmsg = Msg(05);
085800110406           EXSR AggioSFL   ;
085900110406           leavesr;
086000110406 4       ENDIF;
086100110406 4     ENDIF;
086200111012
086300111012       //?Per Ricavo medio spedizione
086400111012 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
086500111012       //?calcolo il valore
086600111012 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
086700111012           wVALrms = sav_vscpft / sav_vscsna;
086800111012         ELSE;
086900111012           wVALrms = 0;
087000111012         ENDIF;
087100111012       //?reimposto il flag di forzatura
087200111012         IF  wVALrms <> sav_valrms;
087300111012           $ForzaRms = *off;
087400111012         ENDIF;
087500111012       //?imposto il campo del video
087600140929         IF  wVALrms > 999;
087700140929           vscvald = 999;
087800111012         ELSE;
087900111012           vscvald = wVALrms;
088000111012         ENDIF;
088100111012       //?msg info se inferiore a 4
088200111012         IF  VSCvald < 4 and not $ForzaRms and
088300111012             wVALrms <> sav_valrms;
088400111012           errVALD = *on;
088500111012           vscmsg = msg(22);
088600111012           exsr AggioSFL;
088700111012           $ForzaRms = *on;
088800111012           sav_valrms = wVALrms;
088900111012           leavesr;
089000111012         ENDIF;
089100111012       //?msg info se superiore a 50
089200111012         IF  VSCvald > 50 and not $ForzaRms and
089300111012             wVALrms <> sav_valrms;
089400111012           errVALD = *on;
089500111012           vscmsg = msg(23);
089600111012           exsr AggioSFL;
089700111012           $ForzaRms = *on;
089800111012           sav_valrms = wVALrms;
089900111012           leavesr;
090000111012         ENDIF;
090100111012       ENDIF;
090200081006
090300100303
090400100305       //?------------
090500100305       //?CHIODI FISSI
090600100305       //?------------
090700100305
090800100305       // se tutte le info della categoria sono obbligatorie ed ho inserito
090900100305       // anche solo 1 info nella videata
091000100305       // errore se info non inserita
091100100305       // oppure la categoria non è tutta obbligatoria ma solo alcune sue info
091200100305       // stessa cosa, se inserito anche solo 1 info nella videta
091300100305       // errore se info non inserita
091400100305       IF  $okqualcosa and (wctrprc = 'S' or vshobl = 'S');
091500100305         SELECT;
091600100422           WHEN  vshsnv <> *blanks and vscval = 0 and vscifo <> 'DEL';
091700100305             errVAL = *on;
091800100305             vscmsg = Msg(01);
091900100305             EXSR AggioSFL;
092000100305             leavesr;
092100111012           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
092200110406             errVALD = *on;
092300110406             vscmsg = Msg(01);
092400110406             EXSR AggioSFL;
092500110406             leavesr;
092600100305           WHEN  vshsnf <> *blanks and vscpft = 0;
092700100305             errPFT = *on;
092800100305             vscmsg = Msg(01);
092900100305             EXSR AggioSFL;
093000100305             leavesr;
093100110118           WHEN  vshsns <> *blanks and vscsna = 0 and I41tpv <> 'A';
093200100305             errSNA = *on;
093300100305             vscmsg = Msg(01);
093400100305             EXSR AggioSFL;
093500100305             leavesr;
093600100305           WHEN  vshsne <> *blanks and vscfsn = *blanks;
093700100305             errFSN = *on;
093800100305             vscmsg = Msg(01);
093900100305             EXSR AggioSFL;
094000100305             leavesr;
094100100305         ENDSL;
094200100305       ENDIF;
094300100303
094400100303       // controllo se almeno una info inserita
094500120607       IF  wctrprc = 'U' and vscatb = *blanks;
094600100303         SELECT;
094700100303           WHEN  vshsnv <> *blanks and vscval <> 0;
094800100303             $okUNA = *on;
094900110406           WHEN  vshsnvd <> *blanks and vscvald <> 0;
095000110406             $okUNA = *on;
095100100303           WHEN  vshsns <> *blanks and vscsna > 0;
095200100303             $okUNA = *on;
095300100303           WHEN  vshsnf <> *blanks and vscpft > 0;
095400100303             $okUNA = *on;
095500100303           WHEN  vshsne <> *blanks and vscfsn <> *blanks;
095600100303           $okUNA = *on;
095700100303         ENDSL;
095800100303       ENDIF;
095900100303
096000100303       // carico in sk di comodo i concorrenti inseriti a video
096100100303       IF  vscifo = '10';
096200100303         // codice concorrente inserito carico in sk di appoggio
096300120607         IF  vscfsn = 'S' and %lookup(vscspp:its) = 0 and
096400120607             vscatb = *blanks;
096500100303           yy = %lookup(*blanks:its);
096600100303           its(yy) = vscspp;
096700100303         ENDIF;
096800100303         // codice concorrente non inserito ed era in sk lo tolgo
096900120607         IF  (vscfsn = 'N'or vscfsn = *blanks or vscatb = 'A');
097000100303           yy = %lookup(vscspp:its);
097100100303           IF yy > 0;
097200100303             its(yy) = *blanks;
097300100303           ENDIF;
097400100303         ENDIF;
097500100303       ENDIF;
097600100423
097700100423 2     if  vshifgv<>'?' and vshsgn='S' ;
097800110406         SELECT;
097900110406           WHEN VSHsnv <> *blanks;
098000110406             IF  not $okQualcosa and vscval = 0;
098100110406               clear vscsgn;
098200110406             ELSE;
098300110406               IF  vscsgn = *blanks;
098400110406                 vscsgn = '+';
098500110406               ENDIF;
098600110406             ENDIF;
098700110406           WHEN VSHsnvd <> *blanks;
098800110406             IF  not $okQualcosa and vscvald = 0;
098900110406               clear vscsgn;
099000110406             ELSE;
099100110406               IF  vscsgn = *blanks;
099200110406                 vscsgn = '+';
099300110406               ENDIF;
099400110406             ENDIF;
099500110406           WHEN  VSHsnf <> *blanks;
099600110406             IF  not $OkQualcosa and VSCpft = 0;
099700110406               clear VSCsgn;
099800110406             ELSE;
099900110406               IF  VSCsgn = *blanks;
100000110406                 VSCsgn = '+';
100100110406               ENDIF;
100200110406             ENDIF;
100300110406           OTHER;
100400110406             clear VSCsgn;
100500110406         ENDSL;
100600100423       endif   ;
100700081007
100800080925       // Se info SPT - totale spesa trasporti,  la memorizzo
100900080925       //  per il calcolo delle %
101000101215       if vscifo='TRT' ;
101100080925       SPTPFT=vscpft   ;
101200101215       //?non devo ripartire in %
101300101215       clear sptpft;
101400080925       endif           ;
101500101230
101600101230       //?------------
101700101230       //?CHIODI FISSI
101800101230       //?------------
101900101230       //?Se Trattativa di tipo 'A'
102000110112       //?Memorizzo il segno in campi di work
102100101230       IF  I41tpv = 'A';
102200110112         IF  VSCIFO = 'TRT';
102300110111           wsgntrt = VSCsgn;
102400101230         ENDIF;
102500110112         IF  VSCIFO = 'A/S';
102600110111           wsgnausc = VSCsgn;
102700101230         ENDIF;
102800101230       ENDIF;
102900100305
103000100305       //?-----------------------------------------------------
103100100305       //?CHIODO FISSO a mio utilizzo per controllo concorrenti
103200100305       //?-----------------------------------------------------
103300100305
103400100305       // mi memorizzo se ho inserito almeno una riga di info
103500100305       // per poi controllare meglio i concorrenti, se anche solo
103600100305       // una riga di info inserita i concorrenti sono obbligatori
103700100305         IF  Not $okqualcosa and
103800100305            (vscpft > 0 or vscfsn <> *blanks or vscval <> 0 or
103900110406             vscsna > 0 or vscvald <> 0);
104000100305           $okqualcosa = *on;
104100100305         ENDIF;
104200080925
104300080703       EXSR AggioSFL   ;
104400081006 1     endif   ;
104500081125
104600080703
104700080703       s01nrr=1+s01nrr  ;
104800100302       chain s01nrr    TA41s01    ;
104900081006 0     enddo         ;
105000110111
105100110111       //?Se Trattativa di tipo 'A'
105200110111       IF  I41tpv = 'A';
105300110112         IF  wsgntrt <> wsgnausc;
105400110112           VSCmsg = msg(14);
105500110112           c01csr = 2;
105600110112           errMessage  = *on;
105700110112           errGenerico = *on;
105800110112           leavesr;
105900110112         ENDIF;
106000110111       ENDIF;
106100081006
106200081006       // Controllo ultima categoria
106300081006       EXSR CambioCAT     ;
106400081006
106500081215       // A questo punto le info minime sono presenti
106600081215       // solo per controlli obbligatori
106700100302       if I41obl='S' ;
106800100302         O41ifomin='S' ;
106900081215       endif ;
107000081215
107100081006
107200080409       ENDSR;
107300080409
107400081006       //--------------------------------------------------------------
107500081006       //?cambiata categoria delle info conn- controllo e salvataggi
107600081006       //--------------------------------------------------------------
107700081006       BEGSR  CambioCAT    ;
107800100302
107900100302       if wctrprc='P'  and  I41obl='S' and totprc<100  ;
108000081006       s01nrr=s01nrr-1 ;
108100100302       chain s01nrr    TA41s01    ;
108200081006           errVal=*on;
108300100302           vscmsg = Msg(06);
108400081006           %subst(vscmsg:40:21)=wdescat   ;
108500081006           EXSR AggioSFL   ;
108600081006       endif              ;
108700100303
108800100303       //?-------------------------------------
108900100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
109000100303       //?-------------------------------------
109100100302
109200100303       // controllo che almeno una INFO della categoria sia inserita
109300100303       // se richiesto da tabella
109400100305       IF  wctrprc = 'U' and not $okUNA and
109500100305           (I41obl = 'S' or $okqualcosa);
109600100302         s01nrr -= 1;
109700100302         chain s01nrr TA41S01;
109800100303         SELECT;
109900100303           WHEN vshsns<>' ';
110000100303             errSNA=*on;
110100100303           WHEN vshsne<>' ';
110200100303             errFSN=*on;
110300100303           WHEN  vshsnv<>' ';
110400100303             errVAL=*on;
110500110406           WHEN  vshsnvd <> ' ';
110600110406             errVALD = *on;
110700100303           WHEN  vshsnf<>' ';
110800100303             errPFT=*on;
110900100303         ENDSL;
111000100302         vscmsg = Msg(07);
111100100302         %subst(vscmsg:43:21)=wdescat;
111200100302         EXSR AggioSFL;
111300100302       ENDIF;
111400100303
111500100303       // controllo se inserito 'NESSUN CONCORRENTE' + altri codici
111600100303       // concorrenti
111700100303       IF  %lookup('8810':its) > 0;
111800100303         yy = 1;
111900100303         FOR yy by 1 to %elem(its);
112000100303           IF its(yy) <> *blanks and its(yy) <> '8810';
112100100303             s01nrr -= 1;
112200100303             chain s01nrr TA41S01;
112300100303             errFSN = *on;
112400100303             vscmsg = Msg(12);
112500100303             EXSR AggioSFL;
112600100303             leave;
112700100303           ENDIF;
112800100303         ENDFOR;
112900100303       ENDIF;
113000081006
113100081006       // Pulisco e memorizzo nuovi dati per nuova categoria
113200081006        clear  totprc     ;
113300081215        wctrprc=vshobl  ;
113400081215        wdescat=vsdifo   ;
113500100302        $okUNA = *off;
113600100302
113700081006        ENDSR   ;
113800080925       //--------------------------------------------------------------
113900080925       //?Visualizzo importi in base alla spesa trasporti
114000080925       //--------------------------------------------------------------
114100080925       BEGSR    VISUALimp  ;
114200100302       if vshsnf=' ' and vscpft=0 and vscval<>0 and vsctva='%  '  ;
114300081215       PFTblu=*on     ;
114400081003       vscpft=(SPTpft*vscval)/100   ;
114500080925       if vscpft>0    ;
114600080925       vscvft='EUR'   ;
114700080925       endif ;
114800080925       endif ;
114900080925
115000080925       ENDSR      ;
115100080703       //--------------------------------------------------------------
115200080703       //?Aggiornamento sfl
115300080703       //--------------------------------------------------------------
115400080703       BEGSR AGGIOSFL  ;
115500100304
115600080703       if vscmsg<>*blanks   ;
115700080703       errMessage  = *on;
115800080703       errGenerico = *on;
115900080704       c01csr=s01nrr    ;
116000080703       endif    ;
116100080703
116200080703       exsr ProtCampi   ;
116300080703
116400080703       //  Ripristino dei flag di forzatura
116500081003       if vshsnf='X' and vscpft>0   ;
116600081003       vshsnf='S'   ;
116700080703       endif    ;
116800081003       if vshsns='X' and vscsna>0   ;
116900080703       vshsns='S'   ;
117000080703       endif    ;
117100100302       if vshsnv='X' and vscval<>0  ;
117200081003       vshsnv='S'   ;
117300080703       endif    ;
117400110406       IF  vshsnvd = 'X' and vscvald <> 0;
117500110406         vshsnvd = 'S';
117600110406       ENDIF;
117700081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
117800081007       vshsne='S'   ;
117900081007       endif    ;
118000080703
118100120607       if vscatb='A' and vshann = 'X';
118200080704       vshann='S'  ;
118300080704       endif   ;
118400080703
118500100302       update  TA41s01  ;
118600100304
118700080703       ENDSR   ;
118800080207       //--------------------------------------------------------------
118900080627       //?Inizializzazione SFL01
119000080207       //--------------------------------------------------------------
119100080409       BEGSR InzS01;
119200080207
119300080207       // Pulizia subfile
119400080207         SflDsp_N    = *on;
119500080207         SflDspCtl_N = *on;
119600100302         write  TA41C01;
119700080207         SflDspCtl_N = *off;
119800080207         SflEnd      = *off;
119900080530
120000080530         clear C01rcd;
120100100507         C01csr=1;
120200080627         S01nrr=0 ;
120300080702         clear Vscmsg;
120400080207         errMessage  = *off;
120500080207         errGenerico = *off;
120600080627         clear savcat  ;
120700080627         clear wcat    ;
120800100302         clear wito    ;
120900100302         clear wits    ;
121000081006         clear sptpft  ;
121100100303
121200100303         clear its;
121300100305         errVAL=*off     ;
121400110406         errVALD = *off;
121500100305         errPFT=*off     ;
121600100305         errSNA=*off     ;
121700100305         errFSN=*off     ;
121800100305         errANN=*off     ;
121900100305
122000100305       $okqualcosa = *off;
122100080409
122200080702       // Carico le info
122300080627       xx=1    ;
122400080627       Indx=1    ;
122500080702       dow   xx<=50    ;
122600100302       if    itoor(xx)<>*blanks  ;
122700100302       clear  TA41S01;
122800080627
122900100302       Wcat= %subst(itoor(xx):1:2)   ;
123000100302       Wito= %subst(itoor(xx):6:3)   ;
123100080627
123200080627       // cambio di categoria
123300080627        if savcat=*blanks or savcat<>wcat               ;
123400080627        savcat=wcat   ;
123500100302        ifosott=*on   ;
123600080627
123700100507       // Verifico se devo visualizzare il titolo della categoria
123800100302         clear  DITG  ;
123900080627         reset TIBS02ds;
124000080627         T02sif = knsif;
124100100302         T02cod = 'ITG';
124200080627         T02mod = 'C'  ;
124300080627         T02ke1 = wcat;
124400080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
124500080627         if t02err=*blanks ;
124600100302            ditg=t02uni    ;
124700080627         endif            ;
124800080627
124900080627       // Se prevista la visualizzazione della categoria
125000080627       //  emetto la riga in videata,
125100100302       if §itgvis='S'    ;
125200100302         vsdifo=§itgdes         ;
125300080703         vsdifo=%trim(vsdifo)+':'    ;
125400080703         // In VSHIMM   metto una "C" per indicare che si tratta
125500080703         //    di riga di categoria
125600080703         vshimm='C'        ;
125700081007
125800081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
125900100302         // in alternativa imposto il flag se almeno una info deve
126000100302         // essere inserita
126100100302         SELECT;
126200100302           WHEN  §itgctpr <> *blanks;
126300100302             vshobl=§itgctpr   ;
126400100305           WHEN  §itgobl <> *blanks;
126500100305             vshobl = §itgobl;
126600100302         ENDSL;
126700081007
126800081007         EXSr ProtCampi    ;
126900081003
127000080627         s01nrr=s01nrr+1   ;
127100100302         write TA41s01     ;
127200080627         protriga=*off     ;
127300100302         ifosott=*off      ;
127400081007         clear vscfsn      ;
127500100507
127600100507         // Se si tratta della prima riga, imposto posizionamento cursore
127700100507         //  sulla seconda
127800100507         if s01nrr=1  ;
127900100507            C01csr = 2;
128000100507         endif   ;
128100100507
128200081007       endif               ;
128300081007       endif               ;
128400080627
128500100302       Indx=%lookup(wito:ito)   ;
128600080627       if Indx>0                ;
128700100302         dito=itods(Indx)         ;
128800100302         vscifo=ito(Indx)       ;
128900080702
129000080702         // Dati da tabella
129100100302          vshobl=§itoobl  ;
129200100302          vshie =§itotip  ;
129300080704
129400080704          clear vshann  ;
129500100302          if §itoann='S'  ;
129600100302          vshann=§itoann  ;
129700080704          endif   ;
129800111012
129900111012         //?pulisco campi comuni per vscval
130000111012          IF  §ITOitg <> '2A' and §ITOitg <> '5A';
130100111012            clear VSHsnv;
130200111012            clear VSHsgn;
130300111012            clear VSCtva;
130400111012          ENDIF;
130500080704
130600081003         // VALORE
130700111012          if §itosnv<>' '  ;
130800100302          vshsnv=§itosnv  ;
130900100423          vshSGN=§itosgn  ;
131000081003          // tipo valore
131100100302          vsctva=§itotva ;
131200080703          endif   ;
131300110406
131400110406         //?VALORE con DECIMALE
131500110406          IF  §itosnvd <> *blanks;
131600110406            vshsnvd = §itosnvd;
131700110406            vshSGN  = §itosgn;
131800110406          // tipo valore
131900110406            vsctva = §itotva;
132000110406          ENDIF;
132100080704
132200081003         // SPEDIZIONI
132300100302          vshsns=§itosns  ;
132400080704
132500081003         // FATTURATO
132600100302          vshsnf=§itosnf  ;
132700101214          vshSGN=§itosgn  ;
132800081003
132900081003         // SI/NO
133000100302          vshsnE=§itosnE  ;
133100081003
133200100302          vshifgv=§itgvis  ;
133300081007          clear vscint    ;
133400080627
133500080627         // Record senza sottotipo
133600081006         select    ;
133700100302         when §itoits=' '    ;
133800100302            vsdifo=§itodes         ;
133900100302            if ifosott=*on    ;
134000080703               vsdifo=%trim(vsdifo)+':'    ;
134100080703            endif    ;
134200100302            chain    (I41nrv:wito) TIVII01l  ;
134300100302            if %found(TIVII01l)   ;
134400100302             trovavii='S'    ;
134500081126             else ;
134600100302             trovavii='N'    ;
134700081126             endif  ;
134800080702            EXSR CaricaInfo        ;
134900081006
135000081006         // Record con   sottotipo da caricare tutti
135100100302         when   §itoits='S' and §itoifv='S';
135200080627            EXSR CARCon    ;
135300081006
135400081006         // Record con   sottotipo da caricare col '?'
135500081006         //   la categoria visualizzata in questo caso è obbligatoria
135600081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
135700081006         //  il '?'
135800100302         when   §itoits='S' and §itoifv='?';
135900100302          vshifgv=§itoifv  ;
136000081215          yy=1        ;
136100081126
136200100302          if §itonrec=0 ;
136300100302             §itonrec=1  ;
136400081126          endif     ;
136500081126
136600100302          setll (I41nrv:wito) TIVII01l  ;
136700100302          dow yy<=§itonrec  ;
136800081006            EXSR CarconUNA            ;
136900081126            yy=yy+1  ;
137000081126          enddo    ;
137100081126
137200081006         endsl ;
137300080703
137400080703         endif ;
137500080702         endif ;
137600080702
137700080702         xx=xx+1   ;
137800080702         enddo     ;
137900100304
138000080702         ENDSR    ;
138100080627
138200080702       //--------------------------------------------------------------
138300081006       //?Carico con   sottotipo  carica riga per riga
138400080702       //--------------------------------------------------------------
138500080702       BEGSR  CARcon            ;
138600080702
138700100302         tbeke1=wito   ;
138800080925
138900100302         setll ('ITS':tbeke1) tntbe01l   ;
139000100302         READE ('ITS':tbeke1) tntbe01l   ;
139100080702
139200080702         dow not %eof(tntbe01l)   ;
139300080702         if tbeatb=' '    ;
139400100302         dits=tbeuni      ;
139500100302         wits=tbeke2      ;
139600080702         vscspp=tbeke2      ;
139700081127         vshspp=tbeke2      ;
139800100302         vsdifo=§itsdes  ;
139900120607         vshann = §itsann;
140000080702
140100100302         chain    (I41nrv:wito:wits) TIVII01l  ;
140200100302            if %found(TIVII01l)   ;
140300100302             trovavii='S'    ;
140400081126             else ;
140500100302             trovavii='N'    ;
140600081126             endif  ;
140700080702         EXSR CaricaInfo    ;
140800100303
140900100303       //?-------------------------------------
141000100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
141100100303       //?-------------------------------------
141200100303
141300100303       // carico i concorrenti in una sk di comodo
141400100303           IF  trovavii = 'S' and wito = '10';
141500100303             IF  %lookup(wits:its) = 0;
141600100303               yy = %lookup(*blanks:its);
141700100303               its(yy) = wits;
141800100303             ENDIF;
141900100303           ENDIF;
142000100303
142100080925         endif   ;
142200080702
142300100302         READE ('ITS':tbeke1) tntbe01l   ;
142400080702         enddo   ;
142500080702
142600080702       ENDSR;
142700081006       //--------------------------------------------------------------
142800081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
142900081006       //--------------------------------------------------------------
143000081006       BEGSR  CARconUNA         ;
143100081007
143200081007       vscint='?'    ;
143300081007
143400100302       reade    (I41nrv:wito) TIVII01l  ;
143500081006
143600100302 1     if not %eof(TIVII01l)   ;
143700100302         trovavii='S'    ;
143800081126
143900100302         vscspp=viispf   ;
144000100302         vshspp=viispf   ;
144100081006
144200100302         tbeke1=wito    ;
144300081126
144400100302         tbeke2=viispf  ;
144500100302         chain ('ITS':tbeke1:tbeke2) tntbe01l   ;
144600081006 2       if not %found(tntbe01l)    ;
144700081006         clear tbeuni       ;
144800081006 2       endif    ;
144900100302         dits=tbeuni     ;
145000081006
145100100302         vsdifo=§itsdes   ;
145200100302         vsdifs=§itsdel   ;
145300081007
145400081006         EXSR CaricaINFO  ;
145500081006
145600081006 1x      else    ;
145700100302         trovavii='N'    ;
145800081006
145900100302         // se non trovato dicitura generica da tabella ITO
146000081126           vsdifo=coRicerca       ;
146100081007         clear vsdifs      ;
146200081126         clear vscspp      ;
146300081127         clear vshspp      ;
146400081006
146500081006         EXSR CaricaINFO   ;
146600081006         endif             ;
146700081006
146800081006       ENDSR;
146900080702       //--------------------------------------------------------------
147000081006       //?Carica ogni riga di info commerciale
147100080702       //--------------------------------------------------------------
147200080702         BEGSR CaricaINFO             ;
147300100302
147400080925         clear pftblu   ;
147500080925
147600100302         if trovavii='S'                             ;
147700080703          vshimm=' '        ;
147800100302          vscfsn=viifsn   ;
147900100423          vscval=%abs(viival)   ;
148000110406          vscvald = %abs(viivald);
148100100423
148200100423          // Se previsto segno visualizzo + o -
148300101215          if vshsgn='S'   and vshsnv <> ' ' ;
148400100423            if  viival<0   ;
148500100423             vscsgn='-'  ;
148600100423            else    ;
148700100423             vscsgn='+'  ;
148800100423            endif   ;
148900100423          endif   ;
149000110406          IF  vshsgn = 'S' and vshsnvd <> ' ';
149100110406            IF  viivald < 0;
149200110406              vscsgn = '-';
149300110406            ELSE;
149400110406              vscsgn = '+';
149500110406            ENDIF;
149600110406          ENDIF;
149700101215
149800101215          vscpft=%abs(viipft);
149900101215          if vshsgn='S'   and vshsnf <> ' ' ;
150000101215            if  viipft<0   ;
150100101215             vscsgn='-'  ;
150200101215            else    ;
150300101215             vscsgn='+'  ;
150400101215            endif   ;
150500101215            IF  I41tpv = 'A' and viipft = 0;
150600101215             vscsgn=' '  ;
150700101215            ENDIF;
150800101215          endif   ;
150900100423
151000081007          if vshifgv='?'   and vscspp>*zeros    ;
151100081007            w0040=%int(vscspp)  ;
151200081007            vscval=w0040     ;
151300081007            clear vscfsn     ;
151400081007          endif            ;
151500100302          vscsna=viisna   ;
151600080925          if vscpft>0     ;
151700080925          vshprpft='S'    ;
151800080925          endif           ;
151900100302          vscvft=viivft   ;
152000100302          Dataiso=%date(viidim:*iso)  ;
152100080702          datadmy=Dataiso ;
152200080702          vscdim=%dec(datadmy)  ;
152300081003          clear vscfil  ;
152400100302          if viifil>0   ;
152500100302          vscfil=%editc(viifil:'X')   ;
152600081003          endif         ;
152700081003
152800100302          if viipru<>*blanks ;
152900100302          vscpru=viipru   ;
153000081003          vscdpru='Utente' ;
153100081003          else  ;
153200081003          clear vscpru   ;
153300081003          clear vscdpru   ;
153400081003          endif     ;
153500080925
153600080925          exsr ViSUALimp   ;
153700101230
153800101230         //?------------
153900101230         //?CHIODI FISSI
154000101230         //?------------
154100101230         // se trattativa di tipo 'A' memorizzo il segno del traffico
154200101230         // totale e dell'aumento/sconto
154300101230           IF  I41tpv = 'A';
154400101230             IF  VSCifo = 'TRT';
154500101230               VSHstrt = VSCsgn;
154600101230             ENDIF;
154700101230             IF  VSCifo = 'A/S';
154800101230               VSHsausc = VSCsgn;
154900101230             ENDIF;
155000101230           ENDIF;
155100111012
155200111012         //?Calcolo il ricavo medio spedizione 'RMS'
155300111012           IF  VSCifo = 'TRT' and VSCpft <> 0 and VSCsna <> 0;
155400111012             wVALrms = VSCpft / VSCsna;
155500140929             IF  wVALrms > 999;
155600140929               wVALrms = 999;
155700111012             ENDIF;
155800111012           ENDIF;
155900080925
156000080702         else             ;
156100080703          vshimm='S'      ;
156200080703          Protann=*on     ;
156300080702          clear vscfsn    ;
156400081003          clear vscval    ;
156500110406          clear vscvald   ;
156600080702          clear vscsna    ;
156700080702          clear vscpft    ;
156800080702          clear vscvft    ;
156900080702          clear vscdim    ;
157000080702          clear vscfil    ;
157100080702          clear vscpru    ;
157200081003          clear vscdpru    ;
157300080925          clear vshprpft  ;
157400101230          clear VSHstrt;
157500101230          clear VSHsausc;
157600111012
157700111012         //?Se info 'RMS' imposto il dato
157800111012           IF  VSCifo = 'RMS';
157900111012             VSCvald = wVALrms;
158000111012           ENDIF;
158100111012
158200080702         endif            ;
158300081006
158400080925         exsr ProtCAMPI    ;
158500080703
158600080703         // I record da annullare li visualizzo solo se ci sono
158700100302           IF vshann = *blanks
158800120607                       or (vshann <> ' ' and vscfsn <> *blanks)
158900120607                       or (vshann <> ' ' and vscpft > 0)
159000120607                       or (vshann <> ' ' and vscval <> 0)
159100120607                       or (vshann <> ' ' and vscvald <> 0);
159200100302
159300081003
159400081003         // In interrogazione visulizzo solo i record pieni col sottotipo
159500100302           if I41mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
159600080703            s01nrr=s01nrr+1   ;
159700100302            write TA41s01     ;
159800101215
159900101215         // Se info TRT - totale spesa trasporti,  la memorizzo
160000101215         //  per il calcolo delle %
160100101215         if vscifo='TRT' ;
160200101215          SPTPFT=vscpft   ;
160300101215         //?non devo ripartire in %
160400101215          clear sptpft;
160500101215         endif           ;
160600080925
160700080703           endif ;
160800081003           endif ;
160900081003
161000080702       ENDSR;
161100080702       //--------------------------------------------------------------
161200080702       //?Gestione protezione campi del sfl
161300080702       //--------------------------------------------------------------
161400080702          BEGSR   ProtCampi   ;
161500100304
161600080703           protfat=*off    ;
161700081003           protVal =*off   ;
161800110406           protVald = *off;
161900111012           protVala = *off;
162000100423           protSGN =*off   ;
162100101214           protSGNa=*off   ;
162200080703           protsped=*off   ;
162300080703           protfsn =*off   ;
162400100302           ifosott=*off    ;
162500080703           Protann=*off ;
162600080704           Infoyell=*off ;
162700080703
162800081007           // Se riga di categoria accendo solo protezione e sottolineatura
162900081007          if vshimm='C'    ;
163000100302           ifosott=*on     ;
163100081007           Protriga=*on    ;
163200081007          else             ;
163300081007
163400080702          // in base a cosa prevede la tabella, proteggo i campi
163500080702          if vshifgv='N' ;
163600100302            ifosott=*on       ;
163700080702          endif    ;
163800080702
163900080704          if vshann<>' '  ;
164000080702           protfat=*on     ;
164100081003           protVal =*on    ;
164200110406           protVald = *on;
164300111012           protVala = *on;
164400100423           protSGN =*on    ;
164500101214           protSGNa=*on    ;
164600080702           protsped=*on    ;
164700080702           protfsn =*on    ;
164800080704           infoyell=*on    ;
164900080702          else            ;
165000081006          protann=*on     ;
165100081031
165200081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
165300110406          if vscpft>0 and vshsnf=' ' and vscval=0 and vscvald = 0;
165400081031          protann=*off    ;
165500081031          endif    ;
165600081031          if vscsna>0 and vshsns=' ' ;
165700081031          protann=*off    ;
165800081031          endif    ;
165900110406          if vscval<>0  and vshsnv=' ' and  vshifgv<>'?'       ;
166000081031          protann=*off    ;
166100081031          endif    ;
166200110406          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
166300110406            protann = *off;
166400110406          ENDIF;
166500081031          if vscfsn<>' ' and vshsne=' ' ;
166600081031          protann=*off    ;
166700081031          endif    ;
166800081006
166900080703            if vshsnf=' ' ;
167000080702             protfat=*on     ;
167100080702            endif           ;
167200111012            if vshsnv=' ';
167300081003             protVal =*on     ;
167400101215            endif           ;
167500110406            IF  vshsnvd = *blanks;
167600110406              protVald = *on;
167700110406            ENDIF;
167800111012            if vshsnvd='A';
167900111012             protVala=*on     ;
168000111012            endif           ;
168100101215             //protSGN =*on     ;
168200101215             //else  ;
168300100423             // Senon prevede seegno, proteggo solo questo
168400100423             if vshsgn=' '   ;
168500100423             protSGN =*on     ;
168600100423             endif   ;
168700101215            //endif           ;
168800080703            if vshsns=' ' ;
168900080702             protsped=*on     ;
169000080702            endif           ;
169100081003            if vshsne=' ' ;
169200081003             protfsn =*on     ;
169300081003            endif           ;
169400080702          endif           ;
169500080703          // Se record di nuova immissione
169600081003          //  proteggo annullamento
169700080703            if vshimm='S'  ;
169800080703             Protann=*on  ;
169900080703           endif   ;
170000081006
170100081006          // Se prevista scelta sottotipo col'?' visualizzo valore
170200081006          //   col codice e vicino il '?'
170300081006          if vshifgv='?'    ;
170400081006             protVal =*off    ;
170500110406             protVald = *off;
170600111012             protVala = *off;
170700100423             protSGN =*on     ;
170800081007             if vscspp>*zeros    ;
170900081007             else          ;
171000081007             infoyell=*on    ;
171100081007             endif            ;
171200081006          endif               ;
171300081007          endif               ;
171400101214
171500101214       //?Se INFO prevede segno, INFO traffico totale e tipo trattativa
171600101214       //?NON è 'A' devo bloccare il segno, è sempre un valore positivo
171700101214       //?altrimenti lascio libera scelta all'utente.
171800101214          IF  VSHsgn = 'S' and VSCifo = 'TRT' and I41tpv <> 'A';
171900101215            protsgna = *on;
172000101214          ENDIF;
172100110207
172200110207       //?Se INFO "TRT" e trattativa di tipo 'A' non devo visualizzare
172300110207       //?il campo delle spedizioni
172400110207          IF  VSCifo = 'TRT' and I41tpv = 'A';
172500110207            protsped = *on;
172600110207          ENDIF;
172700080703
172800080702          ENDSR    ;
172900080702       //--------------------------------------------------------------
173000080702       //?conferma aggiornamento
173100080702       //--------------------------------------------------------------
173200080702        BEGSR ConfAGGIO              ;
173300100304
173400090520        clear ultdata   ;
173500081003
173600080704       s01nrr=1  ;
173700100302       chain s01nrr    TA41s01    ;
173800080704
173900080704 1     dow %found    ;
174000080704       // Escludo i record di descrizione categoria
174100111012       // e i rcd con calcolo automatico del dato
174200111012 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
174300080925
174400080925       // se valori calcolati da pgm li pulisco prima dei controlli
174500080925       if vshsnf=' ' and vshprpft=' '   ;
174600080925       clear vscpft   ;
174700080925       clear vscvft   ;
174800080925       endif          ;
174900080925
175000081127       IF VShifgv<>'?'    ;
175100100302         chain    (I41nrv:vscifo:vscspp) TIVII01l  ;
175200081127       else   ;
175300100302         chain    (I41nrv:vscifo:vshspp) TIVII01l  ;
175400081127       endif   ;
175500100302             if %found(TIVII01l)   ;
175600100302             trovavii='S'    ;
175700081127             else ;
175800100302             trovavii='N'    ;
175900081127             endif  ;
176000081127
176100081127       // A n n u l l a m e n t o oppure per qualche strano motivo
176200081127       //   la info c'e'  ma a video non è stata inserita
176300080925
176400100302 3     if trovavii='S'      and (vscatb='A'  or (vscfsn=' ' AND
176500100610                                 vscpft=0 and vscsna=0 and vscval=0
176600110406                                 and vscvald = 0 and vscsgn = ' '));
176700100302         delete TIVII000  ;
176800081003 x3    else    ;
176900080704
177000081127       // V a r i a z i o n e :  verifico se modificati dei dati
177100100302 31    if trovavii='S'     and vscatb=' '   ;
177200100423
177300100423       // imposto il segno del campo vlaore per il confronto (sul fil non c'è)
177400100423        clear wiisgn   ;
177500100423
177600101215         SELECT;
177700101215           WHEN  VSHsnv <> *blanks;
177800100423         if viival<0   ;
177900100423           Wiisgn='-'  ;
178000100423          else   ;
178100100423           Wiisgn=' '  ;
178200100423            if   vscsgn='+'   ;
178300100423             Wiisgn='+'  ;
178400100423            endif   ;
178500100423          endif   ;
178600110406
178700110406         WHEN  VSHsnvd <> *blanks;
178800110406           IF  viivald < 0;
178900110406             Wiisgn = '-';
179000110406           ELSE;
179100110406             Wiisgn = ' ';
179200110406             IF  vscsgn = '+';
179300110406               Wiisgn = '+';
179400110406             ENDIF;
179500110406           ENDIF;
179600101215
179700101215       // imposto il segno del campo fatturato per il confronto sul file non c'è
179800101215           WHEN  VSHsnf <> *blanks;
179900101215             IF  VIIpft < 0;
180000101215               wiisgn = '-';
180100101215             ELSE;
180200101215               clear wiisgn;
180300101215               IF  VSCsgn = '+';
180400101215                 wiisgn = '+';
180500101215               ENDIF;
180600101215             ENDIF;
180700101215           ENDSL;
180800100423
180900100302 4     if (viifsn<>vscfsn) or
181000100423          (%abs(viival)<>vscval) or
181100110406          (%abs(viivald) <> vscvald) or
181200100423          (Wiisgn<>vscsgn) or
181300100302          (viisna<>vscsna) or
181400100302          (viispf<>vscspp) or
181500101215          (%abs(viipft)<>vscpft) or
181600100302          (viivft<>vscvft) ;
181700080704          // reimposto data ora profilo immissione
181800100302          viidim=*date    ;
181900090520          ultdata=*date    ;
182000100302          viifil=utefil   ;
182100100302          viipru=knmus    ;
182200080704
182300100302          viifsn= vscfsn  ;
182400081007          IF VShifgv<>'?';
182500101215            IF  VSHsnv <> *blanks ;
182600101215            if vscsgn<>'-'  ;
182700100423             viival= %abs(vscval)  ;
182800100423            else  ;
182900100423             viival=0-%abs(vscval)   ;
183000100423            endif  ;
183100101215            ENDIF;
183200110406            IF  VSHsnvd <> *blanks;
183300110406              IF  vscsgn <> '-';
183400110406               viivald = %abs(vscvald);
183500110406              ELSE;
183600110406               viivald = 0 - %abs(vscvald);
183700110406              ENDIF;
183800110406            ENDIF;
183900101215            IF  VSHsnf <> *blanks ;
184000101215              IF  VSCsgn <> '-';
184100101215                VIIpft = %abs(VSCpft);
184200101215              ELSE;
184300101215                VIIpft = 0 - %abs(VSCpft);
184400101215              ENDIF;
184500101215            ENDIF;
184600101215
184700100302          viitva= vsctva  ;
184800081007          else            ;
184900100302          viispf=vscspp   ;
185000100302          if viispf<>*blanks ;
185100100302          viifsn='S'      ;
185200081007          ENDIF           ;
185300081007          ENDIF           ;
185400100302          viisna= vscsna  ;
185500101215          //viipft= vscpft  ;
185600100302          viivft= vscvft  ;
185700081006          // verifico se dei dati non ci voglio più: una volta aggiunti
185800080704          //  quelli previsti, lo tolgo
185900080704
186000101215 5        if (viival<>0 and vshsnv=' ')    ;
186100101215          if (viisna>0 and vshsns<>' ')  or (viipft<>0  and vshsnf<>' ') or
186200110406             (viifsn<>' ' and vshsne<>' ') or
186300110406             (viivald <> 0 and vshsnvd = ' ') ;
186400100302          clear viival   ;
186500080704          endif          ;
186600080704 5        endif          ;
186700110406 5        IF  (viivald <> 0 and vshsnvd = ' ');
186800110406            IF (viisna > 0 and vshsns <> ' ') or
186900110406               (viipft <> 0  and vshsnf <> ' ') or
187000110406               (viifsn <> ' ' and vshsne <> ' ') or
187100110406               (viival <> 0 and vshsnv = ' ');
187200110406              clear viivald;
187300110406            ENDIF;
187400110406 5        ENDIF;
187500100302 5        if (viisna>0 and vshsns=' ')    ;
187600101215          if (viival<>0 and vshsnv<>' ')  or (viipft<>0  and vshsnf<>' ') or
187700110406             (viifsn<>' ' and vshsne<>' ') or
187800110406             (viivald <> 0 and vshsnvd = ' ') ;
187900100302          clear viisna   ;
188000080704          endif          ;
188100080704 5        endif          ;
188200101215 5        if (viipft<>0 and vshsnf=' ')    ;
188300101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
188400110406             (viifsn<>' ' and vshsne<>' ') or
188500110406             (viivald <> 0 and vshsnvd = ' ') ;
188600100302          clear viipft   ;
188700100302          clear viivft   ;
188800080704          endif          ;
188900080704 5        endif          ;
189000100302 5        if (viifsn<>' ' and vshsne=' ')    ;
189100101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
189200110406             (viipft<>0  and vshsnf<>' ') or
189300110406             (viivald <> 0 and vshsnvd = ' ') ;
189400100302          clear viifsn   ;
189500081006          endif          ;
189600081006 5        endif          ;
189700080704
189800100302        update TIVII000  ;
189900080704 4      endif  ;
190000100423 x3a     else  ;
190100100423         unlock tivii01l   ;
190200081003 3a     endif  ;
190300080704
190400081127       // I m m i s s i o n e
190500100302 3     if trovavii='N'     and vscatb=' ' and (vscfsn<>' ' or
190600110406       vscpft>0 or vscsna>0 or vscval<>0 or vscvald <> 0 or
190700110406       vscsgn <> ' ') ;
190800100302       clear TIVII000;
190900100302        viinrv=I41nrv   ;
191000100302        viitpf=vscifo  ;
191100100302        viispf=vscspp  ;
191200100302        viifsn=vscfsn ;
191300081007        if vshifgv<>'?';
191400101215            IF  VSHsnv <> *blanks;
191500100423            if vscsgn<>'-'   ;
191600100423             viival= %abs(vscval)  ;
191700100423            else  ;
191800100423             viival=0-%abs(vscval)   ;
191900100423            endif  ;
192000101215            ENDIF;
192100110406          IF  VSHsnvd <> *blanks;
192200110406            IF  vscsgn <> '-';
192300110406              viivald = %abs(vscvald);
192400110406            ELSE;
192500110406              viivald = 0 - %abs(vscvald);
192600110406            ENDIF;
192700110406          ENDIF;
192800100302        viitva= vsctva  ;
192900081007        else            ;
193000100302          if viispf<>*blanks ;
193100100302          viifsn='S'      ;
193200081007          endif           ;
193300081007        endif           ;
193400081007
193500100302        viisna= vscsna  ;
193600101215        IF  VSHsnf <> *blanks;
193700101215          IF  VSCsgn <>'-';
193800101215            VIIpft = %abs(VSCpft);
193900101215          ELSE;
194000101215            VIIpft = 0 -%abs(VSCpft);
194100101215          ENDIF;
194200101215        ENDIF;
194300101215        //viipft= vscpft  ;
194400100302        viivft= vscvft  ;
194500100302        viidim=*date    ;
194600090520        ultdata=*date    ;
194700100302        viifil=utefil   ;
194800100302        viipru=knmus    ;
194900080704
195000100302        write TIVII000 ;
195100110110        feod  TIVII01L;
195200081003  3a    endif           ;
195300081003  3     endif           ;
195400080704  2     endif           ;
195500080704
195600080704        s01nrr=s01nrr+1 ;
195700100302        chain s01nrr    TA41s01    ;
195800080704  1     enddo           ;
195900080704
196000100304         // Info totali inserite
196100100304         // Se inserimento obbligatorio--> posso impostare la "S"
196200100304         //  altrimenti controllo da VII
196300100302        clear waggiovis    ;
196400081003
196500100302  2       if I41obl=' '   ;
196600081015            // Rifaccio il controllo del sfl come se fosse
196700081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
196800100302            savmod=I41mod   ;
196900100302            I41mod='C'      ;
197000100302            I41obl='S' ;
197100081215
197200081015            exsr contrs01   ;
197300081215
197400100302            I41obl=' ' ;
197500100302            I41mod=savmod   ;
197600081015
197700081015  3         if ErrGenerico=*off;
197800100302              Waggiovis='S';
197900081015  3         endif  ;
198000081015
198100081015  x2      else;
198200100302          Waggiovis='S'   ;
198300081015  2       endif           ;
198400081015
198500100302              O41ifotot=Waggiovis   ;
198600100304
198700080702        ENDSR;
198800080702
198900080206       //--------------------------------------------------------------
199000080206       //?Operazioni finali.
199100080206       //--------------------------------------------------------------
199200080206       BEGSR RoutEnd;
199300080627
199400110218         //if I41tla<>' '    ;
199500080704         // Chiusura pgm   ;
199600080704         clear tibs02ds  ;
199700080704         t02tla='C'      ;
199800080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
199900080704
200000080206         *inLR = *on;
200100110218         //endif             ;
200200080704
200300080206         return;
200400080206
200500080206       ENDSR;
200600080206
200700080206      /end-free
200800080206
200900080206       //--------------------------------------------------------------
201000080206       //?Schiere a tempo di compilazione.
201100080206       //--------------------------------------------------------------
201200080206
201300080410** - MSG ------------------------------------------------------------------ -*
201400100302Immettere INFO.                                                                1
201500100302Per Info non presente (N), non indicare gli altri dati                         2
201600100302Info non più in essere: ANNULLARLA!!                                           3
201700100302La somma delle percentuali per questa categoria di info supera il 100%         4
201800100302La percentuale di questa info supera 100%                                      5
201900100302La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx deve essere 100%  6
202000100302Inserire almeno una info per la categoria xxxxxxxxxxxxxxxxxxxxx                7
202100100302Immettere solo  '?'  se si vuole attivare la ricerca                           8
202200100302Indicare presenza o assenza della INFO con "S o N".                            9
202300100302Codice già presente !!                                                        10
202400100423Non ammesso un valore negativo                                                11
202500100303Se immesso "NESSUN CONCORRENTE" non inserire altri concorrenti                12
202600100917ATTENZIONE!! Il valore massimo consentito è di 99 milioni                     13
202700101230Segno TRAFFICO TOTALE non congruente con segno AUMENTO/SCONTO                 14
202800111021Valore Peso Medio consentito: max 5.000 Kg.                                   15
202900110408Il Valore del Traffico Estero non può superare quello del Traffico Totale     16
203000110408Il n. di spedizioni Estere non può superare il n. di spedizioni Totale        17
203100111012Il Valore del Traffico non può essere inferiore alle Spedizioni Annue.        18
203200111028Attenzione!! Peso medio maggiore/uguale a 500 Kg. Enter per forzare.          19
203300111021Valori del Delta consentiti: max +99%; min -99%                               20
203400111012Attenzione inserito un delta oltre x30%. Enter per forzare!                   21
203500111012Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                22
203600111012Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!               23
203700120608Info non più in essere: verificarla e se non serve annullarla. ENTER forza!   24
