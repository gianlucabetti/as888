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
012100080206
012200080206      // - Parametri ricevuti
012300080206     d KPJBA         e ds
012400100302     d TNTA41DS      e ds
012500141106
012600141106      // - Pgm Cliente in Campagna
012700150114     d TRKC73DS      e ds                  inz
012800150206     d  skOKC73ncm            24     93  0 inz  dim(10)
012900141106
013000141106      // - Pgm Scrive fase se cliente in Campagna
013100150114     d TRKC71DS      e ds                  inz
013200080206
013300080206      // - Ricerca/Controllo tabelle
013400080206     d TIBS02ds      e ds                  inz
013500150216
013600150216      // - Ricerca Unificante Padre?
013700150216     d TIBS10DS      e ds                  inz
013800100304
013900100304      // - Reperimento dati utente
014000100304     d TIBS34DS      e ds
014100080206
014200100304      // - Tabella  Generica azienda
014300100304     d Dged          e ds
014400100304
014500080627      // - Tabella  Info commerciali e Categorie info
014600100304     d dITG          e ds                  inz
014700100304     d dITO          e ds                  inz
014800100304     d dITS          e ds                  inz
014900100302
015000080206      //---------------------------------------------------------------
015100080206      //?Definizione variabili globali.
015200080206      //---------------------------------------------------------------
015300080206
015400080206      // - Flags booleani
015500141106     d wcampagna       s               n   inz(*off)
015600080208     d $Fine           s               n   inz(*off)
015700080208     d $InzS01         s               n   inz(*on)
015800111012     d $ForzaKg        s               n   inz(*off)
015900111012     d $ForzaDel       s               n   inz(*off)
016000111012     d $ForzaRms       s               n   inz(*off)
016100080206
016200080627     d $Video          s              2    inz('S1')
016300080208     d S01nrr          s              4  0 inz
016400080627     d savcat          s              2    inz
016500080627     d Wcat            s              2    inz
016600100302     d Wits            s              4    inz
016700100302     d Wito            s              3    inz
016800081006     d w0040           s              4  0 inz
016900081007     d w005a           s              5    inz
017000081006     d XX              s              3  0 inz
017100080627     d Indx            s              3  0 inz
017200080703     d Primavolta      s              1    inz
017300080925     d Totprc          s              4  0 inz
017400081003     d Wctrprc         s              1    inz
017500081003     d Wdescat         s             21    inz
017600100302     d SPTPFT          s                   like(viipft) inz
017700100302     d Waggiovis       s              1    inz
017800081126     d w008a           s              8    inz
017900081126     d yy              s              3  0 inz
018000100302     d trovavii        s              1    inz
018100100302     d savmod          s                   like(I41mod) inz
018200090520     d ultdata         s              8  0 inz
018300100302     d $okUNA          s              1n   inz(*off)
018400100305     d $okqualcosa     s              1n   inz(*off)
018500100423     d WiiSGN          s              1    inz
018600101230     d wsgntrt         s                   like(vscsgn) inz
018700101230     d wsgnausc        s                   like(vscsgn) inz
018800110207     d sav_vscpft      s                   like(vscpft) inz
018900110324     d sav_vscsna      s                   like(vscsna) inz
019000111012     d sav_valkg       s                   like(vscval) inz
019100111012     d sav_valdel      s                   like(vscval) inz
019200111012     d sav_sgndel      s                   like(vscsgn) inz
019300111019     d sav_valrms      s             11  3 inz
019400111019     d wVALrms         s             11  3 inz
019500141106     d wVIIvald        s                   like(VIIvald) inz
019600180220     d wnrr            s                   like(C01csr) inz
019700101230
019800080605     d Dataiso         s               d   datfmt(*iso)
019900080605     d Datadmy         s               d   datfmt(*dmy)
020000100304
020100081126     d CoRicerca       c                   const('Ricerca  col  ''?''        ')
020200080206
020300080206      //---------------------------------------------------------------
020400080206      //?Definizione procedure usate.
020500080206      //---------------------------------------------------------------
020600080414      /copy gaitrasrc/srcprotopr,tibs02r
020700150216      /copy gaitrasrc/srcprotopr,TIBS10R
020800080414      /copy gaitrasrc/srcprotopr,tibs34r
020900141106
021000141106      // - Pgm Cliente in Campagna
021100150114     d TRKC73R         pr                  extpgm('TRKC73R')
021200141106     d  kpjba                              likeds(kpjba)
021300150114     d  trkc73ds                           likeds(TRKC73DS)
021400141106
021500141106      // - Pgm Scrive fase se cliente in Campagna
021600150114     d TRKC71R         pr                  extpgm('TRKC71R')
021700141106     d  kpjba                              likeds(kpjba)
021800150114     d  trkc71ds                           likeds(TRKC71DS)
021900080206
022000080206      //---------------------------------------------------------------
022100080206      //?Riepilogo indicatori.
022200080206      //---------------------------------------------------------------
022300080207      // 28    : Emissione messaggio di errore a video
022400100304      // 30    : Condiziona SFLDSP
022500100304      // 31    : Condiziona SFLDSPCTL
022600100304      // 32    : Condiziona SFLNXTCHG
022700080207      // 50    : Errore: Opzione errata
022800080207      // 99    : Generico di Errore
022900080206      //---------------------------------------------------------------
023000080206
023100080206      //---------------------------------------------------------------
023200080206      //?M A I N - L I N E
023300080206      //---------------------------------------------------------------
023400080206
023500080627     c     *Entry        plist
023600080206     c                   parm                    KPJBA
023700100304     c                   parm                    TNTA41DS
023800080627
023900080206      /free
024000100303
024100100303        clear o41f12;
024200100303        clear o41f03;
024300100303        clear o41f06;
024400100303        clear o41ifotot;
024500080206
024600100302 1      if  I41tla<>'C'   ;
024700080925
024800080206       // Operazioni iniziali
024900080206       exsr RoutInz;
025000080206
025100100302 2      if  I41mod<>'C'   ;
025200081215
025300100304         // Gestione video
025400100304 3       DOW $Fine = *off;
025500100304
025600100304 4         select;
025700100304           // Videata di selezioni
025800100304             when $Video = 'S1';
025900100304               exsr GesS01;
026000111012             when $Video = 'W1';
026100111012               exsr GesW01;
026200100304             other   ;
026300100304               $Fine = *on;
026400100304 4         endsl;
026500100304
026600100304 3       ENDDO;
026700081215
026800081215 x2    else   ;
026900081215
027000100304         // modalità CONTROLLO : carico il sfl
027100100304         //                    solo ai fini del controllo
027200100304         exsr InzS01;
027300100304         exsr Contrs01   ;
027400081215
027500100304  3      if ErrGenerico=*off;
027600100304            O41ifotot='S';
027700100304  3      endif  ;
027800080925
027900081215 2     endif    ;
028000081215 1     endif    ;
028100080206
028200080206       // Operazioni finali
028300080206       exsr RoutEnd;
028400080206
028500080206       //--------------------------------------------------------------
028600080206       //?Operazioni iniziali.
028700080206       //--------------------------------------------------------------
028800080206       BEGSR RoutInz;
028900100303
029000080627       $inzs01=*on;
029100080929       clear vsctes2         ;
029200080206
029300080703       // Solo la prima volta
029400080703 1     if primavolta=' '   ;
029500080703
029600080206         // Reperimento dati job
029700080206         exsr DatiJob;
029800081003
029900081003         // Se sono EDP accendo indicatore
030000081003         if %subst(knmus:1:3)<>'EDP'    ;
030100081003         UTENonEDP=*on   ;
030200081003         endif      ;
030300080627
030400080627         // Carico tabella per divisa corrente
030500080627         reset TIBS02ds;
030600080627         T02sif = knsif;
030700080627         T02cod = 'GED';
030800080627         T02ke1 = '1'  ;
030900080627         T02mod = 'C'  ;
031000080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
031100080627
031200080703 2       if t02err=' '  ;
031300080627         dGED=t02uni    ;
031400080627         else   ;
031500080627         clear DGED    ;
031600080703 2       endif   ;
031700101215
031800101215         Primavolta='N'   ;
031900101215 1       endif      ;
032000080627
032100080627         // Carico tabella info commerciali
032200080627         xx=0                     ;
032300101215         clear ito;
032400101215         clear itods;
032500101215         clear itoor;
032600100302         setll ('ITO') tntbe01l   ;
032700100302         READE ('ITO') tntbe01l   ;
032800080627
032900101215         dow not %eof(tntbe01l)   ;
033000101215         if tbeatb=' '    ;
033100101214         //?Se tipo trattativa <> 'A' NON carico la info 'A/S'
033200101214           IF  I41tpv <> 'A' and  TBEke1 = 'A/S';
033300101214             reade ('ITO') TNTBE01L;
033400101214             iter;
033500101214           ENDIF;
033600110117         //?Se tipo trattativa = 'A' NON carico la info 'TRE' e 'KMS'
033700111012         //?e la 'RMS'
033800111012           IF  I41tpv = 'A' and  (TBEke1 = 'TRE' or TBEke1 = 'KMS' or
033900111012                                  TBEke1 = 'RMS');
034000110117             reade ('ITO') TNTBE01L;
034100110117             iter;
034200110117           ENDIF;
034300100302           dito=tbeuni    ;
034400080627           xx=xx+1        ;
034500100302           ito(xx)=tbeke1 ;
034600111024         //?Se tipo trattativa = 'A' devo modificare la descrizione x 'TRT'
034700111024           IF  I41tpv = 'A' and  TBEke1 = 'TRT';
034800111024             §ITOdes = 'Valore Aumento/Sconto';
034900111024           ENDIF;
035000100302           itods(xx)=dito ;
035100080627           // ordino in base alla categoria e  num.ordinamento
035200100302           itoor(xx)=§itoitg+%editc(§itoogi:'X')+tbeke1 ;
035300101215         endif               ;
035400080627
035500100302         READE ('ITO') tntbe01l   ;
035600101215         enddo    ;
035700080627
035800100302         sorta  itoOR  ;
035900080702
036000080703         Infoint=*off   ;
036100080704         Infoyell=*off   ;
036200080703         InfoImmiss=*off   ;
036300100302         ifosott=*off   ;
036400080703         Protriga=*off   ;
036500080703         ProtFat =*off   ;
036600081003         ProtVal  =*off   ;
036700110406         ProtVald = *off;
036800111012         ProtVala = *off;
036900100423         ProtSGN  =*off   ;
037000101214         ProtSGNa =*off   ;
037100080703         Protsped =*off   ;
037200080703         Protfsn  =*off   ;
037300080703         clear wcat  ;
037400100302         clear wito  ;
037500080704         $Fine=*off   ;
037600080703
037700080702         // Vedo se modo gestione  o interrogazione
037800100302 1       if I41mod='I' ;
037900080702         InfoInt =*on  ;
038000080703         vsctes2='INTERROGAZIONE'    ;
038100080703 1       endif;
038200080703
038300100302         vscpgm='TNTA41R'   ;
038400080703
038500100302         // IMPOSTO la trattativa in visualizzazione
038600100302         vscnrv=%editc(I41nrv:'Z')   ;
038700100302         vscrag=I41rag   ;
038800081003
038900100302         // Se già  inserite tutte le info, non si possono più annullare
039000081003         //  ma solo sistemare
039100100302         if I41obl=' '  and I41ifotot='S'    ;
039200100302         I41obl='S'   ;
039300081003         endif        ;
039400080627
039500100302         // VERIFICO SE ci sono già dei dati per la trattativa
039600100302         setll   I41nrv  TIVII01l    ;
039700100302         reade   I41nrv  TIVII01l    ;
039800100302 1       dow not %eof(TIVII01l) and viiatb<>' ' ;
039900100302         reade   I41nrv  TIVII01l    ;
040000080703 1       enddo   ;
040100080703
040200100302 1       if %eof(TIVII01l) ;
040300080703         Infoimmiss=*on     ;
040400080929 1       if vsctes2= *blanks    ;
040500080703         vsctes2='  IMMISSIONE  '    ;
040600080929         endif   ;
040700080703         else    ;
040800080929 1       if vsctes2= *blanks    ;
040900080703         vsctes2='  VARIAZIONE  '    ;
041000080703 1       endif    ;
041100080929 1       endif    ;
041200101214
041300101214       //?Se non è immissione e tipo trattativa <> 'A' devo deletare
041400101215       //?la info 'A/S' se sono in gestione
041500101215         IF  not InfoImmiss and I41tpv <> 'A' and I41mod = 'G';
041600101214           wito = 'A/S';
041700101214           chain (I41nrv:wito) TIVII01L;
041800101214           IF  %found(TIVII01L);
041900101214             delete tivii000;
042000101214           ENDIF;
042100101214         ENDIF;
042200080703
042300080627         ENDSR;
042400080206
042500080206       //--------------------------------------------------------------
042600080206       //?Reperimento Dati del job (Utente/Operativi).
042700080206       //--------------------------------------------------------------
042800080206       BEGSR DatiJob;
042900080206
043000080206         in(E) §AzUte;
043100080206         if NOT %error;
043200080206           in(E) §DatiUte;
043300080206         endif;
043400080206         if %error or RSut = *blanks;
043500080206           clear TIBS34ds;
043600080206           tibs34r(tibs34ds);
043700080206           in §AzUte;
043800080206           in §DatiUte;
043900080206         endif;
044000080206
044100080206       ENDSR;
044200080206
044300080206       //--------------------------------------------------------------
044400080627       //?Gestione SFL 01
044500080206       //--------------------------------------------------------------
044600080409       BEGSR GesS01;
044700080207
044800080207         // Inizializzazione videata
044900080409         if  $InzS01 = *on;
045000080409            exsr InzS01;
045100080703         // Posizionamento cursore
045200080703           s01nrr = 1;
045300080703           sfldsp_n=*off;
045400110406           $InzS01  = *off;
045500080207         endif;
045600080207
045700080207
045800080207         // Emissione Testata e Piede con tasti funzionali abilitati
045900080207         if  errGenerico = *off;
046000100302           write  TA41Z01;
046100080207         endif;
046200080703
046300080703         // Posizionamento cursore
046400080703         if  C01csr  > *zero;
046500080703           C01rcd = C01csr;
046600080703         endif  ;
046700080207
046800080207         // Emissione videata
046900100302         exfmt  TA41C01;
047000080410
047100080207         reset errMessage;
047200080207         reset errGenerico;
047300080702         clear Vscmsg;
047400081003         errVAL=*off     ;
047500110406         errVALD = *off;
047600080703         errPFT=*off     ;
047700080703         errSNA=*off     ;
047800080703         errFSN=*off     ;
047900080704         errANN=*off     ;
048000080207
048100080523 1       SELECT;
048200080207
048300080207         // - F3=Fine
048400080523 1         WHEN  dsp_aid = c_F03;
048500080409            $Fine = *on;
048600100302            O41f03='S'   ;
048700080207
048800080207         // - F12=Ritorno
048900080523 1         WHEN  dsp_aid = c_F12;
049000080702            $Fine = *on;
049100100302            O41f12='S'   ;
049200180220
049300180220         // - F16=Annulla
049400180220 1         WHEN  dsp_aid = c_F16;
049500180220            exsr F16s01;
049600080414
049700080530 x1      // Invio / F6
049800080207           OTHER;
049900081006
050000081006        // Non effettuo controlli in interrogazione
050100100302 1           if I41mod<>'I' ;
050200081006               exsr ContrS01 ;
050300081006 2             if  errGenerico = *on;
050400111012                 $Video = 'W1';
050500081006                 leavesr;
050600081006 2             endif;
050700081006 2           endif;
050800080530
050900100302         // F6=conferma Aggiornamento
051000100302 1         if   dsp_aid = c_F06;
051100080702           exsr   ConfAggio ;
051200100305           // ritorno il flag di F6 se hanno inserito qualcosa
051300100305           // può capitare che in immissione diano F6 senza aver
051400100305           // inserito niente a video
051500100305           IF  $okqualcosa;
051600100302           O41f06='S'   ;
051700100305           ENDIF;
051800081023
051900080704           $Fine = *on;
052000080530           endif  ;
052100080207
052200080523 1       ENDSL;
052300080207
052400080207       ENDSR;
052500111012
052600111012       //--------------------------------------------------------------
052700111012       //?Gestione Window errori
052800111012       //--------------------------------------------------------------
052900111012       BEGSR GesW01;
053000111012
053100111012         reset errMessage;
053200111012         reset errGenerico;
053300111012
053400111012         w1riga = vscmsg;
053500111012         exfmt  TA41W01;
053600111012
053700111012         clear Vscmsg;
053800111012         errVAL  = *off;
053900111012         errVALD = *off;
054000111012         errPFT  = *off;
054100111012         errSNA  = *off;
054200111012         errFSN  = *off;
054300111012         errANN  = *off;
054400111012
054500111012         $Video = 'S1';
054600111012
054700111012       ENDSR;
054800080207
054900080526       //--------------------------------------------------------------
055000080702       //?controlli SFL01
055100080409       //--------------------------------------------------------------
055200080409       BEGSR ContrS01;
055300100303
055400100303       clear o41ifomin;
055500081215       clear wctrprc  ;
055600081006       clear wdescat   ;
055700081006       clear totprc   ;
055800081006       clear sptpft   ;
055900100302       clear itoval   ;
056000100303       clear its;
056100100302
056200100302       $okUNA = *off;
056300100302
056400081126       yy=1           ;
056500080703       s01nrr=1  ;
056600100302       chain s01nrr    TA41s01    ;
056700080703
056800081006 0     dow %found    ;
056900080925       // Escludo i record di descrizione categoria
057000081006 1     if vshimm= 'C'    ;
057100081003
057200100302       // A cambio di categoria
057300100302       // se c'era controllo obbligatorio del 100%
057400100302       // controllo ed eventualmente segnalo errore
057500100302       // in alternativa al controllo 100% c'è se almeno una INFO
057600100302       // deve essere inserita
057700081006       EXSR CambioCAT    ;
057800081006       if errGenerico=*on  ;
057900081006       leavesr;
058000081006       endif              ;
058100081006
058200081006x1     else    ;
058300081006
058400080925       pftblu=*off       ;
058500080925
058600081003       // se valori calcolati da pgm li pulisco prima dei controlli
058700081007 2     if vshsnf=' ' and vshprpft=' '   ;
058800080925       clear vscpft   ;
058900080925       clear vscvft   ;
059000081007 2     endif          ;
059100080703
059200080703       // Imposto la divisa, se impostato il fatturato
059300081007 2     if vscpft>0 and vscvft=*blanks   ;
059400080703       vscvft=§gedcr   ;
059500100423       //errGenerico = *on;
059600081007 2     endif    ;
059700080703
059800081007 2     if vscpft=0 and vscvft<>*blanks   ;
059900080703       clear vscvft   ;
060000080703       errGenerico = *on;
060100081007 2     endif   ;
060200100917
060300110407       //?Se valore fatturato impostato e > di 99000000 errore
060400110407       IF  VSCpft > 99000000;
060500100917         VSCmsg = msg(13);
060600100917         errPFT=*on;
060700100917         EXSR AggioSFL   ;
060800100917         leavesr;
060900100917       ENDIF;
061000111012
061100111012       //?Le spedizioni non possono essere superiori del valore fatturato
061200111012       IF  VSCsna > VSCpft;
061300111012         VSCmsg = msg(18);
061400111012         errPFT=*on;
061500111012         EXSR AggioSFL   ;
061600111012         leavesr;
061700111012       ENDIF;
061800080703
061900080704       // se info da annullare e non c'e' la "A", msg forzabile
062000081215       //  non faccio se modalità controlli
062100100302 2     if I41mod<>'C' and vshann='S'  and vscatb=' '  ;
062200080704           errANN=*on;
062300080704           // se  contiene dati, msg forzabile
062400100302 3         if vscfsn='S'or vscval<>0 or vscpft>0 or vscsna>0   ;
062500120608           vscmsg = Msg(24);
062600080704           vshann='X'      ;
062700080704           // Se non contiene dati abbligatorio l'annullamento
062800081007 x3        else            ;
062900100302           vscmsg = Msg(03);
063000081007 3         endif           ;
063100080704           EXSR AggioSFL   ;
063200080704           leavesr;
063300081007 2     endif    ;
063400120608
063500120608       // se c'e' "O"  obbligatorio l'annullamento
063600120608       if I41mod<>'C' and vshann='O'  and vscatb=' '  ;
063700120608           errANN=*on;
063800120608           vscmsg = Msg(03);
063900120608           EXSR AggioSFL   ;
064000120608           leavesr;
064100120608       endif    ;
064200080704
064300080703       // Se richiesto inserimento obbligatorio, per i dati
064400080703       //  obbligatori da tabella segnalo errore
064500100305 2     if  I41obl='S' and vshobl='S';
064600081007 3     if vscatb='A'    or
064700081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
064800081003         if vshsne<>' ';
064900080703           errFSN=*on;
065000081003         endif   ;
065100081003         if vshsnv<>' ';
065200081003           errVAL=*on;
065300081003         endif   ;
065400110406         IF  vshsnvd <> *blanks;
065500110406           errVALD = *on;
065600110406         ENDIF;
065700081003         if vshsnf<>' ';
065800081003           errPFT=*on;
065900081003         endif   ;
066000080703           vscmsg = Msg(01);
066100080703           EXSR AggioSFL   ;
066200080703           leavesr;
066300081007 3     endif   ;
066400081007 2     endif   ;
066500081007
066600081007       // Non accetto se non previsto il valore '?'
066700100805       //?Dal subfile ho tolto la possibilità di inserire '?'
066800081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
066900081007           errFSN=*on;
067000100302           vscmsg = Msg(08);
067100081007           EXSR AggioSFL   ;
067200081007           leavesr;
067300081007 3     endif    ;
067400081007       if vshifgv<>'?' and vscfsn='?'  ;
067500081007           errFSN=*on;
067600100302           vscmsg = Msg(09);
067700081007           EXSR AggioSFL   ;
067800081007           leavesr;
067900081007 3     endif    ;
068000080703
068100080703       // Se immesso "N" non indicare gli altri campi
068200100805       //?Dal subfile ho tolto la possibilità di inserire 'N'
068300100302 2     if vscfsn='N' and (vscval<>0 or vscpft>0 or vscsna>0)   ;
068400080703       if vshsnf<>' '    ;
068500080703           errPFT=*on;
068600080703       else    ;
068700081003          errVAL=*on ;
068800080703       endif  ;
068900080703           vscmsg = Msg(02);
069000080703           EXSR AggioSFL   ;
069100080703           leavesr;
069200081007 2     endif   ;
069300080703
069400081003       // Se immesso almeno un dato, controllo gli obbligatori ed
069500081215       //   i facoltativi. I  facoltativi non li guardo in modalità
069600081215       //   solo controllo
069700081031       if vscatb =' ' and
069800100302 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval<>0)     ;
069900081003
070000081007 3     if vscpft=0    and vshsnf='O'   ;
070100081003           errPFT=*on;
070200100302           vscmsg = Msg(01);
070300081003           EXSR AggioSFL   ;
070400081003           leavesr;
070500081007 3      endif    ;
070600100503
070700100503        // Lo opzionali le chiedo solo se info obbligatorie
070800100503       if  I41obl='S' and
070900100503 3        I41mod<>'C' and  vscpft=0    and vshsnf='S'   ;
071000081003           errPFT=*on;
071100100302           vscmsg = Msg(01);
071200100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
071300081003           vshsnf='X'      ;
071400081003           EXSR AggioSFL   ;
071500081003           leavesr;
071600081007 3     endif    ;
071700110118       //?OBBLIGO delle spedizioni se tipo trattativa <> 'A'
071800110118 3     if vscsna=0    and vshsns='O'   and  I41tpv <> 'A';
071900081003           errSNA=*on;
072000100302           vscmsg = Msg(01);
072100081003           EXSR AggioSFL   ;
072200081003           leavesr;
072300081007 3     endif    ;
072400100503       if  I41obl='S' and
072500100503 3        I41mod<>'C' and vscsna=0    and vshsns='S'   ;
072600081003           errSNA=*on;
072700100302           vscmsg = Msg(01);
072800100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
072900081003           vshsns='X'      ;
073000081003           EXSR AggioSFL   ;
073100081003           leavesr;
073200081007 3     endif    ;
073300081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
073400081007           errFSN=*on;
073500100302           vscmsg = Msg(09);
073600081007           EXSR AggioSFL   ;
073700081007           leavesr;
073800081007 3     endif    ;
073900100503       if  I41obl='S' and
074000100503 3        I41mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
074100081007           errFSN=*on;
074200100302           vscmsg = Msg(09);
074300100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
074400081007           vshsne='X'      ;
074500081007           EXSR AggioSFL   ;
074600081007           leavesr;
074700081007 3     endif    ;
074800081007 3     if (vscval=0    and vshsnv='O')  or
074900100503          (vscval=0    and vshsnv='S' and I41mod<>'C' AND I41OBL='S') ;
075000081007 4        if vshsnv='O'    ;
075100100302            vscmsg = Msg(01);
075200081003          else   ;
075300100302            vscmsg = Msg(01);
075400100302            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
075500100302            vshsnv='X'      ;
075600081215 4        endif  ;
075700081003           errVAL=*on;
075800081003           EXSR AggioSFL   ;
075900081003           leavesr;
076000081215 3     endif    ;
076100110406 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
076200110406           (vscvald = 0 and vshsnvd = 'S'   and
076300110406            I41mod <> 'C' and I41obl = 'S');
076400110406 4        IF  vshsnvd = 'O';
076500110406            vscmsg = Msg(01);
076600110406          ELSE;
076700110406            vscmsg = Msg(01);
076800110406            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
076900110406            vshsnvd = 'X';
077000110406 4        ENDIF;
077100110406          errVALD = *on;
077200110406          EXSR AggioSFL;
077300110406          leavesr;
077400110406 3     ENDIF;
077500110207
077600110207       //?Mi salvo il valore del traffico totale
077700110207 3     IF  vscifo = 'TRT';
077800110207       sav_vscpft = vscpft;
077900110324       sav_vscsna = vscsna;
078000110207       ENDIF;
078100110207
078200110207       //?Il traffico estero è un 'di cui' quindi non può superare
078300110207       //?il traffico totale
078400110324       //?Stessa cosa per le spedizioni
078500110324       IF  vscifo = 'TRE' and vscpft > sav_vscpft;
078600110207         errPFT = *on;
078700110408         vscmsg = msg(16);
078800110207         EXSR AggioSFL;
078900110207         leavesr;
079000110324       ENDIF;
079100110324       IF  vscifo = 'TRE' and vscsna > sav_vscsna;
079200110324         errSNA = *on;
079300110408         vscmsg = msg(17);
079400110324         EXSR AggioSFL;
079500110324         leavesr;
079600110324       ENDIF;
079700110112
079800111012       //?Solo per info "KMS" peso medio
079900111012       IF  VSCval <> 0 and  VSCtva = 'KG ' and VSCifo = 'KMS';
080000111012       //?reimposto il flag di forzatura
080100111012         IF  VSCval <> sav_valkg;
080200111012           $ForzaKg = *off;
080300111012         ENDIF;
080400111012       //?se valore in kg non deve superare i 5000 kg.
080500111012         IF  VSCval > 5000;
080600111012           errVAL = *on;
080700111012           VSCmsg = msg(15);
080800111012           exsr AggioSFL;
080900111012           sav_valkg = VSCval;
081000111012           leavesr;
081100111012         ENDIF;
081200111028       //?msg info se è ugale o supera i 500 kg.
081300111028         IF  VSCval >= 500 and VSCval <> sav_valkg and not $ForzaKg;
081400111012           errVAL = *on;
081500111012           VSCmsg = msg(19);
081600111012           exsr AggioSFL;
081700111012           $ForzaKg = *on;
081800111012           sav_valkg = VSCval;
081900111012           leavesr;
082000111012         ENDIF;
082100111012       ENDIF;
082200111012
082300111012       //?Solo per info "DEL" delta
082400111012       IF  VSCval <> 0 and VSCtva ='%  ' and VSCifo = 'DEL';
082500111012       //?reimposto il flag di forzatura
082600111012         IF  VSCval <> sav_valdel or VSCsgn <> sav_sgndel;
082700111012           $ForzaDel = *off;
082800111012         ENDIF;
082900111012       //?non posso inserire più di 2 cifre quindi al massimo 99
083000111012         IF  VSCval > 99;
083100111012           errVAL = *on;
083200111012           VSCmsg = msg(20);
083300111012           exsr AggioSFL;
083400111012           sav_valdel = VSCval;
083500111012           sav_sgndel = VSCsgn;
083600111012           leavesr;
083700111012         ENDIF;
083800111012       //?msg info se supera 30 +/- indifferente
083900111028         IF  VSCval > 30 and not $ForzaDel and
084000111012            (VSCval <> sav_valdel or VSCsgn <> sav_sgndel);
084100111012           errVAL = *on;
084200111012           VSCmsg = msg(21);
084300111012           %subst(VSCmsg:36:1) = vscsgn;
084400111012           exsr AggioSFL;
084500111012           $ForzaDel = *on;
084600111012           sav_valdel = VSCval;
084700111012           sav_sgndel = VSCsgn;
084800111012           leavesr;
084900111012         ENDIF;
085000111012       ENDIF;
085100081003
085200081003       // se Valore in % --> non deve superare 100%
085300100302 3     if  vscval<>0 and vsctva='%  '    ;
085400081007 4       if vscval>100    ;
085500081003          errVAL=*on ;
085600100302          vscmsg = Msg(05);
085700081003          EXSR AggioSFL   ;
085800081003          leavesr;
085900081007 4       endif      ;
086000081003
086100080925       // Sommo la percentuale se presente per ogni categoria
086200080925       //  se supera 100% segnalo errore
086300081006       //  solo se previsto controllo sulla %
086400081007 4     if wctrprc='P'     ;
086500081006
086600081003       totprc=totprc+vscval    ;
086700081007 5       if totprc>100    ;
086800081003         errVAL=*on ;
086900100302         vscmsg = Msg(04);
087000080925         EXSR AggioSFL   ;
087100080925         leavesr;
087200081007 5       endif      ;
087300081007 4     endif      ;
087400080925
087500100302       // Se immessa la %, visualizzo in base a INFO "SPT"
087600080925       exsr   VISUALimp     ;
087700081007 3     endif      ;
087800100303
087900081007 2     endif      ;
088000110406
088100110406       //?Se Valore con decimali in % --> non deve superare 100%
088200110406 3     IF  vscvald <> 0 and vsctva = '%  ';
088300110406 4       IF  vscvald > 100;
088400110406           errVALD = *on;
088500110406           vscmsg = Msg(05);
088600110406           EXSR AggioSFL   ;
088700110406           leavesr;
088800110406 4       ENDIF;
088900110406 4     ENDIF;
089000111012
089100111012       //?Per Ricavo medio spedizione
089200111012 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
089300111012       //?calcolo il valore
089400111012 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
089500111012           wVALrms = sav_vscpft / sav_vscsna;
089600111012         ELSE;
089700111012           wVALrms = 0;
089800111012         ENDIF;
089900111012       //?reimposto il flag di forzatura
090000111012         IF  wVALrms <> sav_valrms;
090100111012           $ForzaRms = *off;
090200111012         ENDIF;
090300111012       //?imposto il campo del video
090400140929         IF  wVALrms > 999;
090500140929           vscvald = 999;
090600111012         ELSE;
090700111012           vscvald = wVALrms;
090800111012         ENDIF;
090900111012       //?msg info se inferiore a 4
091000111012         IF  VSCvald < 4 and not $ForzaRms and
091100111012             wVALrms <> sav_valrms;
091200111012           errVALD = *on;
091300111012           vscmsg = msg(22);
091400111012           exsr AggioSFL;
091500111012           $ForzaRms = *on;
091600111012           sav_valrms = wVALrms;
091700111012           leavesr;
091800111012         ENDIF;
091900111012       //?msg info se superiore a 50
092000111012         IF  VSCvald > 50 and not $ForzaRms and
092100111012             wVALrms <> sav_valrms;
092200111012           errVALD = *on;
092300111012           vscmsg = msg(23);
092400111012           exsr AggioSFL;
092500111012           $ForzaRms = *on;
092600111012           sav_valrms = wVALrms;
092700111012           leavesr;
092800111012         ENDIF;
092900111012       ENDIF;
093000081006
093100100303
093200100305       //?------------
093300100305       //?CHIODI FISSI
093400100305       //?------------
093500100305
093600100305       // se tutte le info della categoria sono obbligatorie ed ho inserito
093700100305       // anche solo 1 info nella videata
093800100305       // errore se info non inserita
093900100305       // oppure la categoria non è tutta obbligatoria ma solo alcune sue info
094000100305       // stessa cosa, se inserito anche solo 1 info nella videta
094100100305       // errore se info non inserita
094200100305       IF  $okqualcosa and (wctrprc = 'S' or vshobl = 'S');
094300100305         SELECT;
094400100422           WHEN  vshsnv <> *blanks and vscval = 0 and vscifo <> 'DEL';
094500100305             errVAL = *on;
094600100305             vscmsg = Msg(01);
094700100305             EXSR AggioSFL;
094800100305             leavesr;
094900111012           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
095000110406             errVALD = *on;
095100110406             vscmsg = Msg(01);
095200110406             EXSR AggioSFL;
095300110406             leavesr;
095400100305           WHEN  vshsnf <> *blanks and vscpft = 0;
095500100305             errPFT = *on;
095600100305             vscmsg = Msg(01);
095700100305             EXSR AggioSFL;
095800100305             leavesr;
095900110118           WHEN  vshsns <> *blanks and vscsna = 0 and I41tpv <> 'A';
096000100305             errSNA = *on;
096100100305             vscmsg = Msg(01);
096200100305             EXSR AggioSFL;
096300100305             leavesr;
096400100305           WHEN  vshsne <> *blanks and vscfsn = *blanks;
096500100305             errFSN = *on;
096600100305             vscmsg = Msg(01);
096700100305             EXSR AggioSFL;
096800100305             leavesr;
096900100305         ENDSL;
097000100305       ENDIF;
097100100303
097200100303       // controllo se almeno una info inserita
097300120607       IF  wctrprc = 'U' and vscatb = *blanks;
097400100303         SELECT;
097500100303           WHEN  vshsnv <> *blanks and vscval <> 0;
097600100303             $okUNA = *on;
097700110406           WHEN  vshsnvd <> *blanks and vscvald <> 0;
097800110406             $okUNA = *on;
097900100303           WHEN  vshsns <> *blanks and vscsna > 0;
098000100303             $okUNA = *on;
098100100303           WHEN  vshsnf <> *blanks and vscpft > 0;
098200100303             $okUNA = *on;
098300100303           WHEN  vshsne <> *blanks and vscfsn <> *blanks;
098400100303           $okUNA = *on;
098500100303         ENDSL;
098600100303       ENDIF;
098700100303
098800100303       // carico in sk di comodo i concorrenti inseriti a video
098900100303       IF  vscifo = '10';
099000100303         // codice concorrente inserito carico in sk di appoggio
099100120607         IF  vscfsn = 'S' and %lookup(vscspp:its) = 0 and
099200120607             vscatb = *blanks;
099300100303           yy = %lookup(*blanks:its);
099400100303           its(yy) = vscspp;
099500100303         ENDIF;
099600100303         // codice concorrente non inserito ed era in sk lo tolgo
099700120607         IF  (vscfsn = 'N'or vscfsn = *blanks or vscatb = 'A');
099800100303           yy = %lookup(vscspp:its);
099900100303           IF yy > 0;
100000100303             its(yy) = *blanks;
100100100303           ENDIF;
100200100303         ENDIF;
100300100303       ENDIF;
100400100423
100500100423 2     if  vshifgv<>'?' and vshsgn='S' ;
100600110406         SELECT;
100700110406           WHEN VSHsnv <> *blanks;
100800110406             IF  not $okQualcosa and vscval = 0;
100900110406               clear vscsgn;
101000110406             ELSE;
101100110406               IF  vscsgn = *blanks;
101200110406                 vscsgn = '+';
101300110406               ENDIF;
101400110406             ENDIF;
101500110406           WHEN VSHsnvd <> *blanks;
101600110406             IF  not $okQualcosa and vscvald = 0;
101700110406               clear vscsgn;
101800110406             ELSE;
101900110406               IF  vscsgn = *blanks;
102000110406                 vscsgn = '+';
102100110406               ENDIF;
102200110406             ENDIF;
102300110406           WHEN  VSHsnf <> *blanks;
102400110406             IF  not $OkQualcosa and VSCpft = 0;
102500110406               clear VSCsgn;
102600110406             ELSE;
102700110406               IF  VSCsgn = *blanks;
102800110406                 VSCsgn = '+';
102900110406               ENDIF;
103000110406             ENDIF;
103100110406           OTHER;
103200110406             clear VSCsgn;
103300110406         ENDSL;
103400100423       endif   ;
103500081007
103600080925       // Se info SPT - totale spesa trasporti,  la memorizzo
103700080925       //  per il calcolo delle %
103800101215       if vscifo='TRT' ;
103900080925       SPTPFT=vscpft   ;
104000101215       //?non devo ripartire in %
104100101215       clear sptpft;
104200080925       endif           ;
104300101230
104400101230       //?------------
104500101230       //?CHIODI FISSI
104600101230       //?------------
104700101230       //?Se Trattativa di tipo 'A'
104800110112       //?Memorizzo il segno in campi di work
104900101230       IF  I41tpv = 'A';
105000110112         IF  VSCIFO = 'TRT';
105100110111           wsgntrt = VSCsgn;
105200101230         ENDIF;
105300110112         IF  VSCIFO = 'A/S';
105400110111           wsgnausc = VSCsgn;
105500101230         ENDIF;
105600101230       ENDIF;
105700100305
105800100305       //?-----------------------------------------------------
105900100305       //?CHIODO FISSO a mio utilizzo per controllo concorrenti
106000100305       //?-----------------------------------------------------
106100100305
106200100305       // mi memorizzo se ho inserito almeno una riga di info
106300100305       // per poi controllare meglio i concorrenti, se anche solo
106400100305       // una riga di info inserita i concorrenti sono obbligatori
106500100305         IF  Not $okqualcosa and
106600100305            (vscpft > 0 or vscfsn <> *blanks or vscval <> 0 or
106700110406             vscsna > 0 or vscvald <> 0);
106800100305           $okqualcosa = *on;
106900100305         ENDIF;
107000080925
107100080703       EXSR AggioSFL   ;
107200081006 1     endif   ;
107300081125
107400080703
107500080703       s01nrr=1+s01nrr  ;
107600100302       chain s01nrr    TA41s01    ;
107700081006 0     enddo         ;
107800110111
107900110111       //?Se Trattativa di tipo 'A'
108000110111       IF  I41tpv = 'A';
108100110112         IF  wsgntrt <> wsgnausc;
108200110112           VSCmsg = msg(14);
108300110112           c01csr = 2;
108400110112           errMessage  = *on;
108500110112           errGenerico = *on;
108600110112           leavesr;
108700110112         ENDIF;
108800110111       ENDIF;
108900081006
109000081006       // Controllo ultima categoria
109100081006       EXSR CambioCAT     ;
109200081006
109300081215       // A questo punto le info minime sono presenti
109400081215       // solo per controlli obbligatori
109500100302       if I41obl='S' ;
109600100302         O41ifomin='S' ;
109700081215       endif ;
109800081215
109900081006
110000080409       ENDSR;
110100080409
110200081006       //--------------------------------------------------------------
110300081006       //?cambiata categoria delle info conn- controllo e salvataggi
110400081006       //--------------------------------------------------------------
110500081006       BEGSR  CambioCAT    ;
110600100302
110700100302       if wctrprc='P'  and  I41obl='S' and totprc<100  ;
110800081006       s01nrr=s01nrr-1 ;
110900100302       chain s01nrr    TA41s01    ;
111000081006           errVal=*on;
111100100302           vscmsg = Msg(06);
111200081006           %subst(vscmsg:40:21)=wdescat   ;
111300081006           EXSR AggioSFL   ;
111400081006       endif              ;
111500100303
111600100303       //?-------------------------------------
111700100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
111800100303       //?-------------------------------------
111900100302
112000100303       // controllo che almeno una INFO della categoria sia inserita
112100100303       // se richiesto da tabella
112200100305       IF  wctrprc = 'U' and not $okUNA and
112300100305           (I41obl = 'S' or $okqualcosa);
112400100302         s01nrr -= 1;
112500100302         chain s01nrr TA41S01;
112600100303         SELECT;
112700100303           WHEN vshsns<>' ';
112800100303             errSNA=*on;
112900100303           WHEN vshsne<>' ';
113000100303             errFSN=*on;
113100100303           WHEN  vshsnv<>' ';
113200100303             errVAL=*on;
113300110406           WHEN  vshsnvd <> ' ';
113400110406             errVALD = *on;
113500100303           WHEN  vshsnf<>' ';
113600100303             errPFT=*on;
113700100303         ENDSL;
113800100302         vscmsg = Msg(07);
113900100302         %subst(vscmsg:43:21)=wdescat;
114000100302         EXSR AggioSFL;
114100100302       ENDIF;
114200100303
114300100303       // controllo se inserito 'NESSUN CONCORRENTE' + altri codici
114400100303       // concorrenti
114500100303       IF  %lookup('8810':its) > 0;
114600100303         yy = 1;
114700100303         FOR yy by 1 to %elem(its);
114800100303           IF its(yy) <> *blanks and its(yy) <> '8810';
114900100303             s01nrr -= 1;
115000100303             chain s01nrr TA41S01;
115100100303             errFSN = *on;
115200100303             vscmsg = Msg(12);
115300100303             EXSR AggioSFL;
115400100303             leave;
115500100303           ENDIF;
115600100303         ENDFOR;
115700100303       ENDIF;
115800081006
115900081006       // Pulisco e memorizzo nuovi dati per nuova categoria
116000081006        clear  totprc     ;
116100081215        wctrprc=vshobl  ;
116200081215        wdescat=vsdifo   ;
116300100302        $okUNA = *off;
116400100302
116500081006        ENDSR   ;
116600080925       //--------------------------------------------------------------
116700080925       //?Visualizzo importi in base alla spesa trasporti
116800080925       //--------------------------------------------------------------
116900080925       BEGSR    VISUALimp  ;
117000100302       if vshsnf=' ' and vscpft=0 and vscval<>0 and vsctva='%  '  ;
117100081215       PFTblu=*on     ;
117200081003       vscpft=(SPTpft*vscval)/100   ;
117300080925       if vscpft>0    ;
117400080925       vscvft='EUR'   ;
117500080925       endif ;
117600080925       endif ;
117700080925
117800080925       ENDSR      ;
117900080703       //--------------------------------------------------------------
118000080703       //?Aggiornamento sfl
118100080703       //--------------------------------------------------------------
118200080703       BEGSR AGGIOSFL  ;
118300100304
118400080703       if vscmsg<>*blanks   ;
118500080703       errMessage  = *on;
118600080703       errGenerico = *on;
118700080704       c01csr=s01nrr    ;
118800080703       endif    ;
118900080703
119000080703       exsr ProtCampi   ;
119100080703
119200080703       //  Ripristino dei flag di forzatura
119300081003       if vshsnf='X' and vscpft>0   ;
119400081003       vshsnf='S'   ;
119500080703       endif    ;
119600081003       if vshsns='X' and vscsna>0   ;
119700080703       vshsns='S'   ;
119800080703       endif    ;
119900100302       if vshsnv='X' and vscval<>0  ;
120000081003       vshsnv='S'   ;
120100080703       endif    ;
120200110406       IF  vshsnvd = 'X' and vscvald <> 0;
120300110406         vshsnvd = 'S';
120400110406       ENDIF;
120500081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
120600081007       vshsne='S'   ;
120700081007       endif    ;
120800080703
120900120607       if vscatb='A' and vshann = 'X';
121000080704       vshann='S'  ;
121100080704       endif   ;
121200080703
121300100302       update  TA41s01  ;
121400100304
121500080703       ENDSR   ;
121600080207       //--------------------------------------------------------------
121700080627       //?Inizializzazione SFL01
121800080207       //--------------------------------------------------------------
121900080409       BEGSR InzS01;
122000080207
122100080207       // Pulizia subfile
122200080207         SflDsp_N    = *on;
122300080207         SflDspCtl_N = *on;
122400100302         write  TA41C01;
122500080207         SflDspCtl_N = *off;
122600080207         SflEnd      = *off;
122700080530
122800080530         clear C01rcd;
122900100507         C01csr=1;
123000080627         S01nrr=0 ;
123100080702         clear Vscmsg;
123200080207         errMessage  = *off;
123300080207         errGenerico = *off;
123400080627         clear savcat  ;
123500080627         clear wcat    ;
123600100302         clear wito    ;
123700100302         clear wits    ;
123800081006         clear sptpft  ;
123900100303
124000100303         clear its;
124100100305         errVAL=*off     ;
124200110406         errVALD = *off;
124300100305         errPFT=*off     ;
124400100305         errSNA=*off     ;
124500100305         errFSN=*off     ;
124600100305         errANN=*off     ;
124700100305
124800100305       $okqualcosa = *off;
124900080409
125000080702       // Carico le info
125100080627       xx=1    ;
125200080627       Indx=1    ;
125300080702       dow   xx<=50    ;
125400100302       if    itoor(xx)<>*blanks  ;
125500100302       clear  TA41S01;
125600080627
125700100302       Wcat= %subst(itoor(xx):1:2)   ;
125800100302       Wito= %subst(itoor(xx):6:3)   ;
125900080627
126000080627       // cambio di categoria
126100080627        if savcat=*blanks or savcat<>wcat               ;
126200080627        savcat=wcat   ;
126300100302        ifosott=*on   ;
126400080627
126500100507       // Verifico se devo visualizzare il titolo della categoria
126600100302         clear  DITG  ;
126700080627         reset TIBS02ds;
126800080627         T02sif = knsif;
126900100302         T02cod = 'ITG';
127000080627         T02mod = 'C'  ;
127100080627         T02ke1 = wcat;
127200080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
127300080627         if t02err=*blanks ;
127400100302            ditg=t02uni    ;
127500080627         endif            ;
127600080627
127700080627       // Se prevista la visualizzazione della categoria
127800080627       //  emetto la riga in videata,
127900100302       if §itgvis='S'    ;
128000100302         vsdifo=§itgdes         ;
128100080703         vsdifo=%trim(vsdifo)+':'    ;
128200080703         // In VSHIMM   metto una "C" per indicare che si tratta
128300080703         //    di riga di categoria
128400080703         vshimm='C'        ;
128500081007
128600081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
128700100302         // in alternativa imposto il flag se almeno una info deve
128800100302         // essere inserita
128900100302         SELECT;
129000100302           WHEN  §itgctpr <> *blanks;
129100100302             vshobl=§itgctpr   ;
129200100305           WHEN  §itgobl <> *blanks;
129300100305             vshobl = §itgobl;
129400100302         ENDSL;
129500081007
129600081007         EXSr ProtCampi    ;
129700081003
129800080627         s01nrr=s01nrr+1   ;
129900100302         write TA41s01     ;
130000080627         protriga=*off     ;
130100100302         ifosott=*off      ;
130200081007         clear vscfsn      ;
130300100507
130400100507         // Se si tratta della prima riga, imposto posizionamento cursore
130500100507         //  sulla seconda
130600100507         if s01nrr=1  ;
130700100507            C01csr = 2;
130800100507         endif   ;
130900100507
131000081007       endif               ;
131100081007       endif               ;
131200080627
131300100302       Indx=%lookup(wito:ito)   ;
131400080627       if Indx>0                ;
131500100302         dito=itods(Indx)         ;
131600100302         vscifo=ito(Indx)       ;
131700080702
131800080702         // Dati da tabella
131900100302          vshobl=§itoobl  ;
132000100302          vshie =§itotip  ;
132100080704
132200080704          clear vshann  ;
132300100302          if §itoann='S'  ;
132400100302          vshann=§itoann  ;
132500080704          endif   ;
132600111012
132700111012         //?pulisco campi comuni per vscval
132800111012          IF  §ITOitg <> '2A' and §ITOitg <> '5A';
132900111012            clear VSHsnv;
133000111012            clear VSHsgn;
133100111012            clear VSCtva;
133200111012          ENDIF;
133300080704
133400081003         // VALORE
133500111012          if §itosnv<>' '  ;
133600100302          vshsnv=§itosnv  ;
133700100423          vshSGN=§itosgn  ;
133800081003          // tipo valore
133900100302          vsctva=§itotva ;
134000080703          endif   ;
134100110406
134200110406         //?VALORE con DECIMALE
134300110406          IF  §itosnvd <> *blanks;
134400110406            vshsnvd = §itosnvd;
134500110406            vshSGN  = §itosgn;
134600110406          // tipo valore
134700110406            vsctva = §itotva;
134800110406          ENDIF;
134900080704
135000081003         // SPEDIZIONI
135100100302          vshsns=§itosns  ;
135200080704
135300081003         // FATTURATO
135400100302          vshsnf=§itosnf  ;
135500101214          vshSGN=§itosgn  ;
135600081003
135700081003         // SI/NO
135800100302          vshsnE=§itosnE  ;
135900081003
136000100302          vshifgv=§itgvis  ;
136100081007          clear vscint    ;
136200080627
136300080627         // Record senza sottotipo
136400081006         select    ;
136500100302         when §itoits=' '    ;
136600100302            vsdifo=§itodes         ;
136700100302            if ifosott=*on    ;
136800080703               vsdifo=%trim(vsdifo)+':'    ;
136900080703            endif    ;
137000100302            chain    (I41nrv:wito) TIVII01l  ;
137100100302            if %found(TIVII01l)   ;
137200100302             trovavii='S'    ;
137300081126             else ;
137400100302             trovavii='N'    ;
137500081126             endif  ;
137600080702            EXSR CaricaInfo        ;
137700081006
137800081006         // Record con   sottotipo da caricare tutti
137900100302         when   §itoits='S' and §itoifv='S';
138000080627            EXSR CARCon    ;
138100081006
138200081006         // Record con   sottotipo da caricare col '?'
138300081006         //   la categoria visualizzata in questo caso è obbligatoria
138400081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
138500081006         //  il '?'
138600100302         when   §itoits='S' and §itoifv='?';
138700100302          vshifgv=§itoifv  ;
138800081215          yy=1        ;
138900081126
139000100302          if §itonrec=0 ;
139100100302             §itonrec=1  ;
139200081126          endif     ;
139300081126
139400100302          setll (I41nrv:wito) TIVII01l  ;
139500100302          dow yy<=§itonrec  ;
139600081006            EXSR CarconUNA            ;
139700081126            yy=yy+1  ;
139800081126          enddo    ;
139900081126
140000081006         endsl ;
140100080703
140200080703         endif ;
140300080702         endif ;
140400080702
140500080702         xx=xx+1   ;
140600080702         enddo     ;
140700100304
140800080702         ENDSR    ;
140900180220
141000180220       //--------------------------------------------------------------
141100180220       //F16 Annullamento riga info.
141200180220       //--------------------------------------------------------------
141300180220        BEGSR F16S01;
141400180220
141500180220        // devo avere un numero relativo di record del subfile
141600180220        // se a zero vuol dire che F16 è stato dato fuori dal subfile
141700180220          IF  C01csr = *zeros;
141800180220            ErrGenerico = *on;
141900180220            ErrMessage  = *on;
142000180220            VSCmsg      = 'Posizionarsi con il cursore sulla riga da annullare';
142100180220            leavesr;
142200180220          ENDIF;
142300180220
142400180220        // se ho un numero relativo di record del subfile
142500180220        // deve essere un record annullabile
142600180220          IF  C01csr > *zeros;
142700180220            S01nrr = C01csr;
142800180220            chain s01nrr TA41S01;
142900180220            IF  %found and VSHann = *blanks;
143000180220              ErrGenerico = *on;
143100180220              ErrMessage  = *on;
143200180220              VSCmsg = 'Info commerciale non annullabile';
143300180220              leavesr;
143400180220            ENDIF;
143500180220          ENDIF;
143600180220
143700180220        // se arrivo qua posso annullare il rcd
143800180220         chain (I41nrv:VSCifo:VSCspp) TIVII01L;
143900180220         IF  %found(TIVII01L);
144000180220           delete TIVII000;
144100180220         ENDIF;
144200180220
144300180220       // dopo aver cancellato il rcd ricarico il subfile
144400180220         $inzs01 = *on;
144500180220
144600180220       ENDSR;
144700180220
144800080702       //--------------------------------------------------------------
144900081006       //?Carico con   sottotipo  carica riga per riga
145000080702       //--------------------------------------------------------------
145100080702       BEGSR  CARcon            ;
145200080702
145300100302         tbeke1=wito   ;
145400080925
145500100302         setll ('ITS':tbeke1) tntbe01l   ;
145600100302         READE ('ITS':tbeke1) tntbe01l   ;
145700080702
145800080702         dow not %eof(tntbe01l)   ;
145900080702         if tbeatb=' '    ;
146000100302         dits=tbeuni      ;
146100100302         wits=tbeke2      ;
146200080702         vscspp=tbeke2      ;
146300081127         vshspp=tbeke2      ;
146400100302         vsdifo=§itsdes  ;
146500120607         vshann = §itsann;
146600080702
146700100302         chain    (I41nrv:wito:wits) TIVII01l  ;
146800100302            if %found(TIVII01l)   ;
146900100302             trovavii='S'    ;
147000081126             else ;
147100100302             trovavii='N'    ;
147200081126             endif  ;
147300080702         EXSR CaricaInfo    ;
147400100303
147500100303       //?-------------------------------------
147600100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
147700100303       //?-------------------------------------
147800100303
147900100303       // carico i concorrenti in una sk di comodo
148000100303           IF  trovavii = 'S' and wito = '10';
148100100303             IF  %lookup(wits:its) = 0;
148200100303               yy = %lookup(*blanks:its);
148300100303               its(yy) = wits;
148400100303             ENDIF;
148500100303           ENDIF;
148600100303
148700080925         endif   ;
148800080702
148900100302         READE ('ITS':tbeke1) tntbe01l   ;
149000080702         enddo   ;
149100080702
149200080702       ENDSR;
149300081006       //--------------------------------------------------------------
149400081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
149500081006       //--------------------------------------------------------------
149600081006       BEGSR  CARconUNA         ;
149700081007
149800081007       vscint='?'    ;
149900081007
150000100302       reade    (I41nrv:wito) TIVII01l  ;
150100081006
150200100302 1     if not %eof(TIVII01l)   ;
150300100302         trovavii='S'    ;
150400081126
150500100302         vscspp=viispf   ;
150600100302         vshspp=viispf   ;
150700081006
150800100302         tbeke1=wito    ;
150900081126
151000100302         tbeke2=viispf  ;
151100100302         chain ('ITS':tbeke1:tbeke2) tntbe01l   ;
151200081006 2       if not %found(tntbe01l)    ;
151300081006         clear tbeuni       ;
151400081006 2       endif    ;
151500100302         dits=tbeuni     ;
151600081006
151700100302         vsdifo=§itsdes   ;
151800100302         vsdifs=§itsdel   ;
151900081007
152000081006         EXSR CaricaINFO  ;
152100081006
152200081006 1x      else    ;
152300100302         trovavii='N'    ;
152400081006
152500100302         // se non trovato dicitura generica da tabella ITO
152600081126           vsdifo=coRicerca       ;
152700081007         clear vsdifs      ;
152800081126         clear vscspp      ;
152900081127         clear vshspp      ;
153000081006
153100081006         EXSR CaricaINFO   ;
153200081006         endif             ;
153300081006
153400081006       ENDSR;
153500080702       //--------------------------------------------------------------
153600081006       //?Carica ogni riga di info commerciale
153700080702       //--------------------------------------------------------------
153800080702         BEGSR CaricaINFO             ;
153900100302
154000080925         clear pftblu   ;
154100080925
154200100302         if trovavii='S'                             ;
154300080703          vshimm=' '        ;
154400100302          vscfsn=viifsn   ;
154500100423          vscval=%abs(viival)   ;
154600110406          vscvald = %abs(viivald);
154700100423
154800100423          // Se previsto segno visualizzo + o -
154900101215          if vshsgn='S'   and vshsnv <> ' ' ;
155000100423            if  viival<0   ;
155100100423             vscsgn='-'  ;
155200100423            else    ;
155300100423             vscsgn='+'  ;
155400100423            endif   ;
155500100423          endif   ;
155600110406          IF  vshsgn = 'S' and vshsnvd <> ' ';
155700110406            IF  viivald < 0;
155800110406              vscsgn = '-';
155900110406            ELSE;
156000110406              vscsgn = '+';
156100110406            ENDIF;
156200110406          ENDIF;
156300101215
156400101215          vscpft=%abs(viipft);
156500101215          if vshsgn='S'   and vshsnf <> ' ' ;
156600101215            if  viipft<0   ;
156700101215             vscsgn='-'  ;
156800101215            else    ;
156900101215             vscsgn='+'  ;
157000101215            endif   ;
157100101215            IF  I41tpv = 'A' and viipft = 0;
157200101215             vscsgn=' '  ;
157300101215            ENDIF;
157400101215          endif   ;
157500100423
157600081007          if vshifgv='?'   and vscspp>*zeros    ;
157700081007            w0040=%int(vscspp)  ;
157800081007            vscval=w0040     ;
157900081007            clear vscfsn     ;
158000081007          endif            ;
158100100302          vscsna=viisna   ;
158200080925          if vscpft>0     ;
158300080925          vshprpft='S'    ;
158400080925          endif           ;
158500100302          vscvft=viivft   ;
158600100302          Dataiso=%date(viidim:*iso)  ;
158700080702          datadmy=Dataiso ;
158800080702          vscdim=%dec(datadmy)  ;
158900081003          clear vscfil  ;
159000100302          if viifil>0   ;
159100100302          vscfil=%editc(viifil:'X')   ;
159200081003          endif         ;
159300081003
159400100302          if viipru<>*blanks ;
159500100302          vscpru=viipru   ;
159600081003          vscdpru='Utente' ;
159700081003          else  ;
159800081003          clear vscpru   ;
159900081003          clear vscdpru   ;
160000081003          endif     ;
160100080925
160200080925          exsr ViSUALimp   ;
160300101230
160400101230         //?------------
160500101230         //?CHIODI FISSI
160600101230         //?------------
160700101230         // se trattativa di tipo 'A' memorizzo il segno del traffico
160800101230         // totale e dell'aumento/sconto
160900101230           IF  I41tpv = 'A';
161000101230             IF  VSCifo = 'TRT';
161100101230               VSHstrt = VSCsgn;
161200101230             ENDIF;
161300101230             IF  VSCifo = 'A/S';
161400101230               VSHsausc = VSCsgn;
161500101230             ENDIF;
161600101230           ENDIF;
161700111012
161800111012         //?Calcolo il ricavo medio spedizione 'RMS'
161900111012           IF  VSCifo = 'TRT' and VSCpft <> 0 and VSCsna <> 0;
162000111012             wVALrms = VSCpft / VSCsna;
162100140929             IF  wVALrms > 999;
162200140929               wVALrms = 999;
162300111012             ENDIF;
162400111012           ENDIF;
162500080925
162600080702         else             ;
162700080703          vshimm='S'      ;
162800080703          Protann=*on     ;
162900080702          clear vscfsn    ;
163000081003          clear vscval    ;
163100110406          clear vscvald   ;
163200080702          clear vscsna    ;
163300080702          clear vscpft    ;
163400080702          clear vscvft    ;
163500080702          clear vscdim    ;
163600080702          clear vscfil    ;
163700080702          clear vscpru    ;
163800081003          clear vscdpru    ;
163900080925          clear vshprpft  ;
164000101230          clear VSHstrt;
164100101230          clear VSHsausc;
164200111012
164300111012         //?Se info 'RMS' imposto il dato
164400111012           IF  VSCifo = 'RMS';
164500111012             VSCvald = wVALrms;
164600111012           ENDIF;
164700111012
164800080702         endif            ;
164900081006
165000080925         exsr ProtCAMPI    ;
165100080703
165200080703         // I record da annullare li visualizzo solo se ci sono
165300100302           IF vshann = *blanks
165400120607                       or (vshann <> ' ' and vscfsn <> *blanks)
165500120607                       or (vshann <> ' ' and vscpft > 0)
165600120607                       or (vshann <> ' ' and vscval <> 0)
165700120607                       or (vshann <> ' ' and vscvald <> 0);
165800100302
165900081003
166000081003         // In interrogazione visulizzo solo i record pieni col sottotipo
166100100302           if I41mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
166200080703            s01nrr=s01nrr+1   ;
166300100302            write TA41s01     ;
166400101215
166500101215         // Se info TRT - totale spesa trasporti,  la memorizzo
166600101215         //  per il calcolo delle %
166700101215         if vscifo='TRT' ;
166800101215          SPTPFT=vscpft   ;
166900101215         //?non devo ripartire in %
167000101215          clear sptpft;
167100101215         endif           ;
167200080925
167300080703           endif ;
167400081003           endif ;
167500081003
167600080702       ENDSR;
167700080702       //--------------------------------------------------------------
167800080702       //?Gestione protezione campi del sfl
167900080702       //--------------------------------------------------------------
168000080702          BEGSR   ProtCampi   ;
168100100304
168200080703           protfat=*off    ;
168300081003           protVal =*off   ;
168400110406           protVald = *off;
168500111012           protVala = *off;
168600100423           protSGN =*off   ;
168700101214           protSGNa=*off   ;
168800080703           protsped=*off   ;
168900080703           protfsn =*off   ;
169000100302           ifosott=*off    ;
169100080703           Protann=*off ;
169200080704           Infoyell=*off ;
169300080703
169400081007           // Se riga di categoria accendo solo protezione e sottolineatura
169500081007          if vshimm='C'    ;
169600100302           ifosott=*on     ;
169700081007           Protriga=*on    ;
169800081007          else             ;
169900081007
170000080702          // in base a cosa prevede la tabella, proteggo i campi
170100080702          if vshifgv='N' ;
170200100302            ifosott=*on       ;
170300080702          endif    ;
170400080702
170500080704          if vshann<>' '  ;
170600080702           protfat=*on     ;
170700081003           protVal =*on    ;
170800110406           protVald = *on;
170900111012           protVala = *on;
171000100423           protSGN =*on    ;
171100101214           protSGNa=*on    ;
171200080702           protsped=*on    ;
171300080702           protfsn =*on    ;
171400080704           infoyell=*on    ;
171500080702          else            ;
171600081006          protann=*on     ;
171700081031
171800081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
171900110406          if vscpft>0 and vshsnf=' ' and vscval=0 and vscvald = 0;
172000081031          protann=*off    ;
172100081031          endif    ;
172200081031          if vscsna>0 and vshsns=' ' ;
172300081031          protann=*off    ;
172400081031          endif    ;
172500110406          if vscval<>0  and vshsnv=' ' and  vshifgv<>'?'       ;
172600081031          protann=*off    ;
172700081031          endif    ;
172800110406          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
172900110406            protann = *off;
173000110406          ENDIF;
173100081031          if vscfsn<>' ' and vshsne=' ' ;
173200081031          protann=*off    ;
173300081031          endif    ;
173400081006
173500080703            if vshsnf=' ' ;
173600080702             protfat=*on     ;
173700080702            endif           ;
173800111012            if vshsnv=' ';
173900081003             protVal =*on     ;
174000101215            endif           ;
174100110406            IF  vshsnvd = *blanks;
174200110406              protVald = *on;
174300110406            ENDIF;
174400111012            if vshsnvd='A';
174500111012             protVala=*on     ;
174600111012            endif           ;
174700101215             //protSGN =*on     ;
174800101215             //else  ;
174900100423             // Senon prevede seegno, proteggo solo questo
175000100423             if vshsgn=' '   ;
175100100423             protSGN =*on     ;
175200100423             endif   ;
175300101215            //endif           ;
175400080703            if vshsns=' ' ;
175500080702             protsped=*on     ;
175600080702            endif           ;
175700081003            if vshsne=' ' ;
175800081003             protfsn =*on     ;
175900081003            endif           ;
176000080702          endif           ;
176100080703          // Se record di nuova immissione
176200081003          //  proteggo annullamento
176300080703            if vshimm='S'  ;
176400080703             Protann=*on  ;
176500080703           endif   ;
176600081006
176700081006          // Se prevista scelta sottotipo col'?' visualizzo valore
176800081006          //   col codice e vicino il '?'
176900081006          if vshifgv='?'    ;
177000081006             protVal =*off    ;
177100110406             protVald = *off;
177200111012             protVala = *off;
177300100423             protSGN =*on     ;
177400081007             if vscspp>*zeros    ;
177500081007             else          ;
177600081007             infoyell=*on    ;
177700081007             endif            ;
177800081006          endif               ;
177900081007          endif               ;
178000101214
178100101214       //?Se INFO prevede segno, INFO traffico totale e tipo trattativa
178200101214       //?NON è 'A' devo bloccare il segno, è sempre un valore positivo
178300101214       //?altrimenti lascio libera scelta all'utente.
178400101214          IF  VSHsgn = 'S' and VSCifo = 'TRT' and I41tpv <> 'A';
178500101215            protsgna = *on;
178600101214          ENDIF;
178700110207
178800110207       //?Se INFO "TRT" e trattativa di tipo 'A' non devo visualizzare
178900110207       //?il campo delle spedizioni
179000110207          IF  VSCifo = 'TRT' and I41tpv = 'A';
179100110207            protsped = *on;
179200110207          ENDIF;
179300080703
179400080702          ENDSR    ;
179500080702       //--------------------------------------------------------------
179600080702       //?conferma aggiornamento
179700080702       //--------------------------------------------------------------
179800080702        BEGSR ConfAGGIO              ;
179900100304
180000090520        clear ultdata   ;
180100141106        wcampagna = *off;
180200081003
180300080704       s01nrr=1  ;
180400100302       chain s01nrr    TA41s01    ;
180500080704
180600080704 1     dow %found    ;
180700080704       // Escludo i record di descrizione categoria
180800111012       // e i rcd con calcolo automatico del dato
180900111012 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
181000080925
181100080925       // se valori calcolati da pgm li pulisco prima dei controlli
181200080925       if vshsnf=' ' and vshprpft=' '   ;
181300080925       clear vscpft   ;
181400080925       clear vscvft   ;
181500080925       endif          ;
181600080925
181700081127       IF VShifgv<>'?'    ;
181800100302         chain    (I41nrv:vscifo:vscspp) TIVII01l  ;
181900081127       else   ;
182000100302         chain    (I41nrv:vscifo:vshspp) TIVII01l  ;
182100081127       endif   ;
182200100302             if %found(TIVII01l)   ;
182300100302             trovavii='S'    ;
182400081127             else ;
182500100302             trovavii='N'    ;
182600081127             endif  ;
182700081127
182800081127       // A n n u l l a m e n t o oppure per qualche strano motivo
182900081127       //   la info c'e'  ma a video non è stata inserita
183000080925
183100100302 3     if trovavii='S'      and (vscatb='A'  or (vscfsn=' ' AND
183200100610                                 vscpft=0 and vscsna=0 and vscval=0
183300110406                                 and vscvald = 0 and vscsgn = ' '));
183400100302         delete TIVII000  ;
183500081003 x3    else    ;
183600080704
183700081127       // V a r i a z i o n e :  verifico se modificati dei dati
183800100302 31    if trovavii='S'     and vscatb=' '   ;
183900141106
184000141106       //?Trattativa di tipo 'A' mi memorizzo se variato Aumento/Sconto
184100141106         IF  I41tpv = 'A' and VSCifo = 'A/S';
184200141106           IF  wsgnausc <> VSHsausc or
184300141106               %abs(VIIvald) <> VSCvald;
184400141106             wcampagna = *on;
184500141106             wVIIvald = VSCvald;
184600141106             wsgnausc = VSHsausc;
184700141106           ENDIF;
184800141106         ENDIF;
184900100423
185000100423       // imposto il segno del campo vlaore per il confronto (sul fil non c'è)
185100100423        clear wiisgn   ;
185200100423
185300101215         SELECT;
185400101215           WHEN  VSHsnv <> *blanks;
185500100423         if viival<0   ;
185600100423           Wiisgn='-'  ;
185700100423          else   ;
185800100423           Wiisgn=' '  ;
185900100423            if   vscsgn='+'   ;
186000100423             Wiisgn='+'  ;
186100100423            endif   ;
186200100423          endif   ;
186300110406
186400110406         WHEN  VSHsnvd <> *blanks;
186500110406           IF  viivald < 0;
186600110406             Wiisgn = '-';
186700110406           ELSE;
186800110406             Wiisgn = ' ';
186900110406             IF  vscsgn = '+';
187000110406               Wiisgn = '+';
187100110406             ENDIF;
187200110406           ENDIF;
187300101215
187400101215       // imposto il segno del campo fatturato per il confronto sul file non c'è
187500101215           WHEN  VSHsnf <> *blanks;
187600101215             IF  VIIpft < 0;
187700101215               wiisgn = '-';
187800101215             ELSE;
187900101215               clear wiisgn;
188000101215               IF  VSCsgn = '+';
188100101215                 wiisgn = '+';
188200101215               ENDIF;
188300101215             ENDIF;
188400101215           ENDSL;
188500100423
188600100302 4     if (viifsn<>vscfsn) or
188700100423          (%abs(viival)<>vscval) or
188800110406          (%abs(viivald) <> vscvald) or
188900100423          (Wiisgn<>vscsgn) or
189000100302          (viisna<>vscsna) or
189100100302          (viispf<>vscspp) or
189200101215          (%abs(viipft)<>vscpft) or
189300100302          (viivft<>vscvft) ;
189400080704          // reimposto data ora profilo immissione
189500100302          viidim=*date    ;
189600090520          ultdata=*date    ;
189700100302          viifil=utefil   ;
189800100302          viipru=knmus    ;
189900080704
190000100302          viifsn= vscfsn  ;
190100081007          IF VShifgv<>'?';
190200101215            IF  VSHsnv <> *blanks ;
190300101215            if vscsgn<>'-'  ;
190400100423             viival= %abs(vscval)  ;
190500100423            else  ;
190600100423             viival=0-%abs(vscval)   ;
190700100423            endif  ;
190800101215            ENDIF;
190900110406            IF  VSHsnvd <> *blanks;
191000110406              IF  vscsgn <> '-';
191100110406               viivald = %abs(vscvald);
191200110406              ELSE;
191300110406               viivald = 0 - %abs(vscvald);
191400110406              ENDIF;
191500110406            ENDIF;
191600101215            IF  VSHsnf <> *blanks ;
191700101215              IF  VSCsgn <> '-';
191800101215                VIIpft = %abs(VSCpft);
191900101215              ELSE;
192000101215                VIIpft = 0 - %abs(VSCpft);
192100101215              ENDIF;
192200101215            ENDIF;
192300101215
192400100302          viitva= vsctva  ;
192500081007          else            ;
192600100302          viispf=vscspp   ;
192700100302          if viispf<>*blanks ;
192800100302          viifsn='S'      ;
192900081007          ENDIF           ;
193000081007          ENDIF           ;
193100100302          viisna= vscsna  ;
193200101215          //viipft= vscpft  ;
193300100302          viivft= vscvft  ;
193400081006          // verifico se dei dati non ci voglio più: una volta aggiunti
193500080704          //  quelli previsti, lo tolgo
193600080704
193700101215 5        if (viival<>0 and vshsnv=' ')    ;
193800101215          if (viisna>0 and vshsns<>' ')  or (viipft<>0  and vshsnf<>' ') or
193900110406             (viifsn<>' ' and vshsne<>' ') or
194000110406             (viivald <> 0 and vshsnvd = ' ') ;
194100100302          clear viival   ;
194200080704          endif          ;
194300080704 5        endif          ;
194400110406 5        IF  (viivald <> 0 and vshsnvd = ' ');
194500110406            IF (viisna > 0 and vshsns <> ' ') or
194600110406               (viipft <> 0  and vshsnf <> ' ') or
194700110406               (viifsn <> ' ' and vshsne <> ' ') or
194800110406               (viival <> 0 and vshsnv = ' ');
194900110406              clear viivald;
195000110406            ENDIF;
195100110406 5        ENDIF;
195200100302 5        if (viisna>0 and vshsns=' ')    ;
195300101215          if (viival<>0 and vshsnv<>' ')  or (viipft<>0  and vshsnf<>' ') or
195400110406             (viifsn<>' ' and vshsne<>' ') or
195500110406             (viivald <> 0 and vshsnvd = ' ') ;
195600100302          clear viisna   ;
195700080704          endif          ;
195800080704 5        endif          ;
195900101215 5        if (viipft<>0 and vshsnf=' ')    ;
196000101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
196100110406             (viifsn<>' ' and vshsne<>' ') or
196200110406             (viivald <> 0 and vshsnvd = ' ') ;
196300100302          clear viipft   ;
196400100302          clear viivft   ;
196500080704          endif          ;
196600080704 5        endif          ;
196700100302 5        if (viifsn<>' ' and vshsne=' ')    ;
196800101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
196900110406             (viipft<>0  and vshsnf<>' ') or
197000110406             (viivald <> 0 and vshsnvd = ' ') ;
197100100302          clear viifsn   ;
197200081006          endif          ;
197300081006 5        endif          ;
197400080704
197500100302        update TIVII000  ;
197600080704 4      endif  ;
197700100423 x3a     else  ;
197800100423         unlock tivii01l   ;
197900081003 3a     endif  ;
198000080704
198100081127       // I m m i s s i o n e
198200100302 3     if trovavii='N'     and vscatb=' ' and (vscfsn<>' ' or
198300110406       vscpft>0 or vscsna>0 or vscval<>0 or vscvald <> 0 or
198400110406       vscsgn <> ' ') ;
198500100302       clear TIVII000;
198600100302        viinrv=I41nrv   ;
198700100302        viitpf=vscifo  ;
198800100302        viispf=vscspp  ;
198900100302        viifsn=vscfsn ;
199000081007        if vshifgv<>'?';
199100101215            IF  VSHsnv <> *blanks;
199200100423            if vscsgn<>'-'   ;
199300100423             viival= %abs(vscval)  ;
199400100423            else  ;
199500100423             viival=0-%abs(vscval)   ;
199600100423            endif  ;
199700101215            ENDIF;
199800110406          IF  VSHsnvd <> *blanks;
199900110406            IF  vscsgn <> '-';
200000110406              viivald = %abs(vscvald);
200100110406            ELSE;
200200110406              viivald = 0 - %abs(vscvald);
200300110406            ENDIF;
200400110406          ENDIF;
200500100302        viitva= vsctva  ;
200600081007        else            ;
200700100302          if viispf<>*blanks ;
200800100302          viifsn='S'      ;
200900081007          endif           ;
201000081007        endif           ;
201100081007
201200100302        viisna= vscsna  ;
201300101215        IF  VSHsnf <> *blanks;
201400101215          IF  VSCsgn <>'-';
201500101215            VIIpft = %abs(VSCpft);
201600101215          ELSE;
201700101215            VIIpft = 0 -%abs(VSCpft);
201800101215          ENDIF;
201900101215        ENDIF;
202000101215        //viipft= vscpft  ;
202100100302        viivft= vscvft  ;
202200100302        viidim=*date    ;
202300090520        ultdata=*date    ;
202400100302        viifil=utefil   ;
202500100302        viipru=knmus    ;
202600141106
202700141106       //?Trattativa di tipo 'A' mi memorizzo che ho scrutto Aumento/Sconto
202800141106         IF  I41tpv = 'A' and VSCifo = 'A/S' and VSCvald <> *zeros;
202900141106           wcampagna = *on;
203000141106           wVIIvald = VSCvald;
203100141106           wsgnausc = VSHsausc;
203200141106         ENDIF;
203300080704
203400100302        write TIVII000 ;
203500110110        feod  TIVII01L;
203600081003  3a    endif           ;
203700081003  3     endif           ;
203800080704  2     endif           ;
203900080704
204000080704        s01nrr=s01nrr+1 ;
204100100302        chain s01nrr    TA41s01    ;
204200080704  1     enddo           ;
204300080704
204400100304         // Info totali inserite
204500100304         // Se inserimento obbligatorio--> posso impostare la "S"
204600100304         //  altrimenti controllo da VII
204700100302        clear waggiovis    ;
204800081003
204900100302  2       if I41obl=' '   ;
205000081015            // Rifaccio il controllo del sfl come se fosse
205100081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
205200100302            savmod=I41mod   ;
205300100302            I41mod='C'      ;
205400100302            I41obl='S' ;
205500081215
205600081015            exsr contrs01   ;
205700081215
205800100302            I41obl=' ' ;
205900100302            I41mod=savmod   ;
206000081015
206100081015  3         if ErrGenerico=*off;
206200100302              Waggiovis='S';
206300081015  3         endif  ;
206400081015
206500081015  x2      else;
206600100302          Waggiovis='S'   ;
206700081015  2       endif           ;
206800081015
206900100302              O41ifotot=Waggiovis   ;
207000141106
207100141106       //?Se il cliente è in campagna devo scrivere la fase per OBJ da TTR
207200150114       //?per ogni campagna aperta
207300141106         IF  wcampagna;
207400150114           clear TRKC73DS;
207500141106           chain (I41nrv) TIVIS05L;
207600141106           IF  not %found(TIVIS05L) or VISksc = 0;
207700141106             leavesr;
207800141106           ENDIF;
207900150114           IKC73ksu = VISksc;
208000150114           IKC73dat = VISdat;
208100150206           IKC73ftr = 'S';
208200150114           TRKC73r (kpjba:TRKC73DS);
208300150114           IF  OKC73err <> *blanks;
208400141106             leavesr;
208500141106           ENDIF;
208600150114       //?Se arrivo qua ok cliente sulla trattativa
208700150114       //?Per ogni campagna ancora aperta sul cliente devo scrivere la fase TR
208800150114           xx = 1;
208900150114           FOR xx by 1 to %elem(skOKC73ncm);
209000150114             IF  skOKC73ncm(xx) > *zeros;
209100150114           //?quindi richiamo pgm scrittura fase
209200150114               clear TRKC71DS;
209300150114               IKC71ncm = skOKC73ncm(xx);
209400150114               IKC71ksu = OKC73ksu;
209500150114               IKC71acm = FaseObjTtr;
209600150114               IKC71pea = wVIIvald;
209700150114               IF  wsgnausc = '-';
209800150114                 IKC71pea = IKC71pea * -1;
209900150114               ENDIF;
210000150114               IKC71nrv = I41nrv;
210100150114               TRKC71R (kpjba:TRKC71DS);
210200150114               IF  OKC71err = 'E';
210300150114                 leavesr;
210400150114               ENDIF;
210500150114             ENDIF;
210600150114           ENDFOR;
210700150216       //?Dopo aver scritto la fase TR sul codice cliente della trattativa
210800150216       //?Verifico se il cliente è ancora Papà o ha cambiato Papà
210900150216           clear TIBS10DS;
211000150216           D10tle = 'ST';
211100150216           D10paf = 'P';
211200150216           D10cod = VISksc;
211300150216           TIBS10R (TIBS10DS);
211400150216         //?Se ritorna errore vuol dire che è ancora Papà
211500150216           IF  D10err <> *blanks;
211600150216             leavesr;
211700150216           ENDIF;
211800150216         //?Se non c'è il codice padre vado via
211900150216           IF  D10cop = *zeros;
212000150216             leavesr;
212100150216           ENDIF;
212200150216         //?Se stesso codice padre vado via
212300150216           IF  D10cop = OKC73ksu;
212400150216             leavesr;
212500150216           ENDIF;
212600150216         //?Se arrivo qua vuol dire che è diventato figlio
212700150216         //?verifico se il nuovo papà è in campagna per poi
212800150216         //?scrivere la fase TR anche sul nuovo papà
212900150216           clear TRKC73DS;
213000150216           IKC73ksu = D10cop;
213100150216           IKC73dat = VISdat;
213200150216           IKC73ftr = 'S';
213300150216           TRKC73r (kpjba:TRKC73DS);
213400150216           IF  OKC73err <> *blanks;
213500150216             leavesr;
213600150216           ENDIF;
213700150216       //?Se arrivo qua vuol dire che il nuovo papà è in campagna
213800150216       //?Per ogni campagna ancora aperta sul cliente devo scrivere la fase TR
213900150216           xx = 1;
214000150216           FOR xx by 1 to %elem(skOKC73ncm);
214100150216             IF  skOKC73ncm(xx) > *zeros;
214200150216           //?quindi richiamo pgm scrittura fase
214300150216               clear TRKC71DS;
214400150216               IKC71ncm = skOKC73ncm(xx);
214500150216               IKC71ksu = OKC73ksu;
214600150216               IKC71acm = FaseObjTtr;
214700150216               IKC71pea = wVIIvald;
214800150216               IF  wsgnausc = '-';
214900150216                 IKC71pea = IKC71pea * -1;
215000150216               ENDIF;
215100150216               IKC71nrv = I41nrv;
215200150216               TRKC71R (kpjba:TRKC71DS);
215300150216               IF  OKC71err = 'E';
215400150216                 leavesr;
215500150216               ENDIF;
215600150216             ENDIF;
215700150216           ENDFOR;
215800141106         ENDIF;
215900100304
216000080702        ENDSR;
216100080702
216200080206       //--------------------------------------------------------------
216300080206       //?Operazioni finali.
216400080206       //--------------------------------------------------------------
216500080206       BEGSR RoutEnd;
216600080627
216700110218         //if I41tla<>' '    ;
216800080704         // Chiusura pgm   ;
216900080704         clear tibs02ds  ;
217000080704         t02tla='C'      ;
217100080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
217200080704
217300080206         *inLR = *on;
217400110218         //endif             ;
217500080704
217600080206         return;
217700080206
217800080206       ENDSR;
217900080206
218000080206      /end-free
218100080206
218200080206       //--------------------------------------------------------------
218300080206       //?Schiere a tempo di compilazione.
218400080206       //--------------------------------------------------------------
218500080206
218600080410** - MSG ------------------------------------------------------------------ -*
218700100302Immettere INFO.                                                                1
218800100302Per Info non presente (N), non indicare gli altri dati                         2
218900100302Info non più in essere: ANNULLARLA!!                                           3
219000100302La somma delle percentuali per questa categoria di info supera il 100%         4
219100100302La percentuale di questa info supera 100%                                      5
219200100302La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx deve essere 100%  6
219300100302Inserire almeno una info per la categoria xxxxxxxxxxxxxxxxxxxxx                7
219400100302Immettere solo  '?'  se si vuole attivare la ricerca                           8
219500100302Indicare presenza o assenza della INFO con "S o N".                            9
219600100302Codice già presente !!                                                        10
219700100423Non ammesso un valore negativo                                                11
219800100303Se immesso "NESSUN CONCORRENTE" non inserire altri concorrenti                12
219900100917ATTENZIONE!! Il valore massimo consentito è di 99 milioni                     13
220000101230Segno TRAFFICO TOTALE non congruente con segno AUMENTO/SCONTO                 14
220100111021Valore Peso Medio consentito: max 5.000 Kg.                                   15
220200110408Il Valore del Traffico Estero non può superare quello del Traffico Totale     16
220300110408Il n. di spedizioni Estere non può superare il n. di spedizioni Totale        17
220400111012Il Valore del Traffico non può essere inferiore alle Spedizioni Annue.        18
220500111028Attenzione!! Peso medio maggiore/uguale a 500 Kg. Enter per forzare.          19
220600111021Valori del Delta consentiti: max +99%; min -99%                               20
220700111012Attenzione inserito un delta oltre x30%. Enter per forzare!                   21
220800111012Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                22
220900111012Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!               23
221000120608Info non più in essere: verificarla e se non serve annullarla. ENTER forza!   24
