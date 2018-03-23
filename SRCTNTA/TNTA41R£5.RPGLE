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
013600150216
013700150216      // - Ricerca Unificante Padre?
013800150216     d TIBS10DS      e ds                  inz
013900100304
014000100304      // - Reperimento dati utente
014100100304     d TIBS34DS      e ds
014200080206
014300100304      // - Tabella  Generica azienda
014400100304     d Dged          e ds
014500100304
014600080627      // - Tabella  Info commerciali e Categorie info
014700100304     d dITG          e ds                  inz
014800100304     d dITO          e ds                  inz
014900100304     d dITS          e ds                  inz
015000100302
015100080206      //---------------------------------------------------------------
015200080206      //?Definizione variabili globali.
015300080206      //---------------------------------------------------------------
015400080206
015500080206      // - Flags booleani
015600141106     d wcampagna       s               n   inz(*off)
015700080208     d $Fine           s               n   inz(*off)
015800080208     d $InzS01         s               n   inz(*on)
015900111012     d $ForzaKg        s               n   inz(*off)
016000111012     d $ForzaDel       s               n   inz(*off)
016100111012     d $ForzaRms       s               n   inz(*off)
016200080206
016300080627     d $Video          s              2    inz('S1')
016400080208     d S01nrr          s              4  0 inz
016500080627     d savcat          s              2    inz
016600080627     d Wcat            s              2    inz
016700100302     d Wits            s              4    inz
016800100302     d Wito            s              3    inz
016900081006     d w0040           s              4  0 inz
017000081007     d w005a           s              5    inz
017100081006     d XX              s              3  0 inz
017200080627     d Indx            s              3  0 inz
017300080703     d Primavolta      s              1    inz
017400080925     d Totprc          s              4  0 inz
017500081003     d Wctrprc         s              1    inz
017600081003     d Wdescat         s             21    inz
017700100302     d SPTPFT          s                   like(viipft) inz
017800100302     d Waggiovis       s              1    inz
017900081126     d w008a           s              8    inz
018000081126     d yy              s              3  0 inz
018100100302     d trovavii        s              1    inz
018200100302     d savmod          s                   like(I41mod) inz
018300090520     d ultdata         s              8  0 inz
018400100302     d $okUNA          s              1n   inz(*off)
018500100305     d $okqualcosa     s              1n   inz(*off)
018600100423     d WiiSGN          s              1    inz
018700101230     d wsgntrt         s                   like(vscsgn) inz
018800101230     d wsgnausc        s                   like(vscsgn) inz
018900110207     d sav_vscpft      s                   like(vscpft) inz
019000110324     d sav_vscsna      s                   like(vscsna) inz
019100111012     d sav_valkg       s                   like(vscval) inz
019200111012     d sav_valdel      s                   like(vscval) inz
019300111012     d sav_sgndel      s                   like(vscsgn) inz
019400111019     d sav_valrms      s             11  3 inz
019500111019     d wVALrms         s             11  3 inz
019600141106     d wVIIvald        s                   like(VIIvald) inz
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
037300080703         PotAnnul =*off   ;
037400080703         clear wcat  ;
037500100302         clear wito  ;
037600080704         $Fine=*off   ;
037700080703
037800080702         // Vedo se modo gestione  o interrogazione
037900100302 1       if I41mod='I' ;
038000080702         InfoInt =*on  ;
038100080703         vsctes2='INTERROGAZIONE'    ;
038200080703 1       endif;
038300080703
038400100302         vscpgm='TNTA41R'   ;
038500080703
038600100302         // IMPOSTO la trattativa in visualizzazione
038700100302         vscnrv=%editc(I41nrv:'Z')   ;
038800100302         vscrag=I41rag   ;
038900081003
039000100302         // Se già  inserite tutte le info, non si possono più annullare
039100081003         //  ma solo sistemare
039200100302         if I41obl=' '  and I41ifotot='S'    ;
039300100302         I41obl='S'   ;
039400081003         endif        ;
039500080627
039600100302         // VERIFICO SE ci sono già dei dati per la trattativa
039700100302         setll   I41nrv  TIVII01l    ;
039800100302         reade   I41nrv  TIVII01l    ;
039900100302 1       dow not %eof(TIVII01l) and viiatb<>' ' ;
040000100302         reade   I41nrv  TIVII01l    ;
040100080703 1       enddo   ;
040200080703
040300100302 1       if %eof(TIVII01l) ;
040400080703         Infoimmiss=*on     ;
040500080929 1       if vsctes2= *blanks    ;
040600080703         vsctes2='  IMMISSIONE  '    ;
040700080929         endif   ;
040800080703         else    ;
040900080929 1       if vsctes2= *blanks    ;
041000080703         vsctes2='  VARIAZIONE  '    ;
041100080703 1       endif    ;
041200080929 1       endif    ;
041300101214
041400101214       //?Se non è immissione e tipo trattativa <> 'A' devo deletare
041500101215       //?la info 'A/S' se sono in gestione
041600101215         IF  not InfoImmiss and I41tpv <> 'A' and I41mod = 'G';
041700101214           wito = 'A/S';
041800101214           chain (I41nrv:wito) TIVII01L;
041900101214           IF  %found(TIVII01L);
042000101214             delete tivii000;
042100101214           ENDIF;
042200101214         ENDIF;
042300080703
042400080627         ENDSR;
042500080206
042600080206       //--------------------------------------------------------------
042700080206       //?Reperimento Dati del job (Utente/Operativi).
042800080206       //--------------------------------------------------------------
042900080206       BEGSR DatiJob;
043000080206
043100080206         in(E) §AzUte;
043200080206         if NOT %error;
043300080206           in(E) §DatiUte;
043400080206         endif;
043500080206         if %error or RSut = *blanks;
043600080206           clear TIBS34ds;
043700080206           tibs34r(tibs34ds);
043800080206           in §AzUte;
043900080206           in §DatiUte;
044000080206         endif;
044100080206
044200080206       ENDSR;
044300080206
044400080206       //--------------------------------------------------------------
044500080627       //?Gestione SFL 01
044600080206       //--------------------------------------------------------------
044700080409       BEGSR GesS01;
044800080207
044900080207         // Inizializzazione videata
045000080409         if  $InzS01 = *on;
045100080409            exsr InzS01;
045200080703         // Posizionamento cursore
045300080703           s01nrr = 1;
045400080703           sfldsp_n=*off;
045500110406           $InzS01  = *off;
045600080207         endif;
045700080207
045800080207
045900080207         // Emissione Testata e Piede con tasti funzionali abilitati
046000080207         if  errGenerico = *off;
046100100302           write  TA41Z01;
046200080207         endif;
046300080703
046400080703         // Posizionamento cursore
046500080703         if  C01csr  > *zero;
046600080703           C01rcd = C01csr;
046700080703         endif  ;
046800080207
046900080207         // Emissione videata
047000100302         exfmt  TA41C01;
047100080410
047200080207         reset errMessage;
047300080207         reset errGenerico;
047400080702         clear Vscmsg;
047500081003         errVAL=*off     ;
047600110406         errVALD = *off;
047700080703         errPFT=*off     ;
047800080703         errSNA=*off     ;
047900080703         errFSN=*off     ;
048000080704         errANN=*off     ;
048100080207
048200080523 1       SELECT;
048300080207
048400080207         // - F3=Fine
048500080523 1         WHEN  dsp_aid = c_F03;
048600080409            $Fine = *on;
048700100302            O41f03='S'   ;
048800080207
048900080207         // - F12=Ritorno
049000080523 1         WHEN  dsp_aid = c_F12;
049100080702            $Fine = *on;
049200100302            O41f12='S'   ;
049300080414
049400080530 x1      // Invio / F6
049500080207           OTHER;
049600081006
049700081006        // Non effettuo controlli in interrogazione
049800100302 1           if I41mod<>'I' ;
049900081006               exsr ContrS01 ;
050000081006 2             if  errGenerico = *on;
050100111012                 $Video = 'W1';
050200081006                 leavesr;
050300081006 2             endif;
050400081006 2           endif;
050500080530
050600100302         // F6=conferma Aggiornamento
050700100302 1         if   dsp_aid = c_F06;
050800080702           exsr   ConfAggio ;
050900100305           // ritorno il flag di F6 se hanno inserito qualcosa
051000100305           // può capitare che in immissione diano F6 senza aver
051100100305           // inserito niente a video
051200100305           IF  $okqualcosa;
051300100302           O41f06='S'   ;
051400100305           ENDIF;
051500081023
051600080704           $Fine = *on;
051700080530           endif  ;
051800080207
051900080523 1       ENDSL;
052000080207
052100080207       ENDSR;
052200111012
052300111012       //--------------------------------------------------------------
052400111012       //?Gestione Window errori
052500111012       //--------------------------------------------------------------
052600111012       BEGSR GesW01;
052700111012
052800111012         reset errMessage;
052900111012         reset errGenerico;
053000111012
053100111012         w1riga = vscmsg;
053200111012         exfmt  TA41W01;
053300111012
053400111012         clear Vscmsg;
053500111012         errVAL  = *off;
053600111012         errVALD = *off;
053700111012         errPFT  = *off;
053800111012         errSNA  = *off;
053900111012         errFSN  = *off;
054000111012         errANN  = *off;
054100111012
054200111012         $Video = 'S1';
054300111012
054400111012       ENDSR;
054500080207
054600080526       //--------------------------------------------------------------
054700080702       //?controlli SFL01
054800080409       //--------------------------------------------------------------
054900080409       BEGSR ContrS01;
055000100303
055100100303       clear o41ifomin;
055200081215       clear wctrprc  ;
055300081006       clear wdescat   ;
055400081006       clear totprc   ;
055500081006       clear sptpft   ;
055600100302       clear itoval   ;
055700100303       clear its;
055800100302
055900100302       $okUNA = *off;
056000100302
056100081126       yy=1           ;
056200080703       s01nrr=1  ;
056300100302       chain s01nrr    TA41s01    ;
056400080703
056500081006 0     dow %found    ;
056600080925       // Escludo i record di descrizione categoria
056700081006 1     if vshimm= 'C'    ;
056800081003
056900100302       // A cambio di categoria
057000100302       // se c'era controllo obbligatorio del 100%
057100100302       // controllo ed eventualmente segnalo errore
057200100302       // in alternativa al controllo 100% c'è se almeno una INFO
057300100302       // deve essere inserita
057400081006       EXSR CambioCAT    ;
057500081006       if errGenerico=*on  ;
057600081006       leavesr;
057700081006       endif              ;
057800081006
057900081006x1     else    ;
058000081006
058100080925       pftblu=*off       ;
058200080925
058300081003       // se valori calcolati da pgm li pulisco prima dei controlli
058400081007 2     if vshsnf=' ' and vshprpft=' '   ;
058500080925       clear vscpft   ;
058600080925       clear vscvft   ;
058700081007 2     endif          ;
058800080703
058900080703       // Imposto la divisa, se impostato il fatturato
059000081007 2     if vscpft>0 and vscvft=*blanks   ;
059100080703       vscvft=§gedcr   ;
059200100423       //errGenerico = *on;
059300081007 2     endif    ;
059400080703
059500081007 2     if vscpft=0 and vscvft<>*blanks   ;
059600080703       clear vscvft   ;
059700080703       errGenerico = *on;
059800081007 2     endif   ;
059900100917
060000110407       //?Se valore fatturato impostato e > di 99000000 errore
060100110407       IF  VSCpft > 99000000;
060200100917         VSCmsg = msg(13);
060300100917         errPFT=*on;
060400100917         EXSR AggioSFL   ;
060500100917         leavesr;
060600100917       ENDIF;
060700111012
060800111012       //?Le spedizioni non possono essere superiori del valore fatturato
060900111012       IF  VSCsna > VSCpft;
061000111012         VSCmsg = msg(18);
061100111012         errPFT=*on;
061200111012         EXSR AggioSFL   ;
061300111012         leavesr;
061400111012       ENDIF;
061500080703
061600080704       // se info da annullare e non c'e' la "A", msg forzabile
061700081215       //  non faccio se modalità controlli
061800100302 2     if I41mod<>'C' and vshann='S'  and vscatb=' '  ;
061900080704           errANN=*on;
062000080704           // se  contiene dati, msg forzabile
062100100302 3         if vscfsn='S'or vscval<>0 or vscpft>0 or vscsna>0   ;
062200120608           vscmsg = Msg(24);
062300080704           vshann='X'      ;
062400080704           // Se non contiene dati abbligatorio l'annullamento
062500081007 x3        else            ;
062600100302           vscmsg = Msg(03);
062700081007 3         endif           ;
062800080704           EXSR AggioSFL   ;
062900080704           leavesr;
063000081007 2     endif    ;
063100120608
063200120608       // se c'e' "O"  obbligatorio l'annullamento
063300120608       if I41mod<>'C' and vshann='O'  and vscatb=' '  ;
063400120608           errANN=*on;
063500120608           vscmsg = Msg(03);
063600120608           EXSR AggioSFL   ;
063700120608           leavesr;
063800120608       endif    ;
063900080704
064000080703       // Se richiesto inserimento obbligatorio, per i dati
064100080703       //  obbligatori da tabella segnalo errore
064200100305 2     if  I41obl='S' and vshobl='S';
064300081007 3     if vscatb='A'    or
064400081003         (vscval=0 and vscpft=0 and vscfsn=' ' and vscsna=0)  ;
064500081003         if vshsne<>' ';
064600080703           errFSN=*on;
064700081003         endif   ;
064800081003         if vshsnv<>' ';
064900081003           errVAL=*on;
065000081003         endif   ;
065100110406         IF  vshsnvd <> *blanks;
065200110406           errVALD = *on;
065300110406         ENDIF;
065400081003         if vshsnf<>' ';
065500081003           errPFT=*on;
065600081003         endif   ;
065700080703           vscmsg = Msg(01);
065800080703           EXSR AggioSFL   ;
065900080703           leavesr;
066000081007 3     endif   ;
066100081007 2     endif   ;
066200081007
066300081007       // Non accetto se non previsto il valore '?'
066400100805       //?Dal subfile ho tolto la possibilità di inserire '?'
066500081007       if vshifgv='?' and (vscfsn<>' ' and vscfsn<>'?')  ;
066600081007           errFSN=*on;
066700100302           vscmsg = Msg(08);
066800081007           EXSR AggioSFL   ;
066900081007           leavesr;
067000081007 3     endif    ;
067100081007       if vshifgv<>'?' and vscfsn='?'  ;
067200081007           errFSN=*on;
067300100302           vscmsg = Msg(09);
067400081007           EXSR AggioSFL   ;
067500081007           leavesr;
067600081007 3     endif    ;
067700080703
067800080703       // Se immesso "N" non indicare gli altri campi
067900100805       //?Dal subfile ho tolto la possibilità di inserire 'N'
068000100302 2     if vscfsn='N' and (vscval<>0 or vscpft>0 or vscsna>0)   ;
068100080703       if vshsnf<>' '    ;
068200080703           errPFT=*on;
068300080703       else    ;
068400081003          errVAL=*on ;
068500080703       endif  ;
068600080703           vscmsg = Msg(02);
068700080703           EXSR AggioSFL   ;
068800080703           leavesr;
068900081007 2     endif   ;
069000080703
069100081003       // Se immesso almeno un dato, controllo gli obbligatori ed
069200081215       //   i facoltativi. I  facoltativi non li guardo in modalità
069300081215       //   solo controllo
069400081031       if vscatb =' ' and
069500100302 2       (vscfsn<>' ' or vscpft>0 or vscsna>0 or vscval<>0)     ;
069600081003
069700081007 3     if vscpft=0    and vshsnf='O'   ;
069800081003           errPFT=*on;
069900100302           vscmsg = Msg(01);
070000081003           EXSR AggioSFL   ;
070100081003           leavesr;
070200081007 3      endif    ;
070300100503
070400100503        // Lo opzionali le chiedo solo se info obbligatorie
070500100503       if  I41obl='S' and
070600100503 3        I41mod<>'C' and  vscpft=0    and vshsnf='S'   ;
070700081003           errPFT=*on;
070800100302           vscmsg = Msg(01);
070900100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
071000081003           vshsnf='X'      ;
071100081003           EXSR AggioSFL   ;
071200081003           leavesr;
071300081007 3     endif    ;
071400110118       //?OBBLIGO delle spedizioni se tipo trattativa <> 'A'
071500110118 3     if vscsna=0    and vshsns='O'   and  I41tpv <> 'A';
071600081003           errSNA=*on;
071700100302           vscmsg = Msg(01);
071800081003           EXSR AggioSFL   ;
071900081003           leavesr;
072000081007 3     endif    ;
072100100503       if  I41obl='S' and
072200100503 3        I41mod<>'C' and vscsna=0    and vshsns='S'   ;
072300081003           errSNA=*on;
072400100302           vscmsg = Msg(01);
072500100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
072600081003           vshsns='X'      ;
072700081003           EXSR AggioSFL   ;
072800081003           leavesr;
072900081007 3     endif    ;
073000081007 3     if vscfsn=' '  and vshsne='O' and vshifgv<>'?'  ;
073100081007           errFSN=*on;
073200100302           vscmsg = Msg(09);
073300081007           EXSR AggioSFL   ;
073400081007           leavesr;
073500081007 3     endif    ;
073600100503       if  I41obl='S' and
073700100503 3        I41mod<>'C' and vscFSN=' ' and vshsne='S' and vshifgv<>'?'  ;
073800081007           errFSN=*on;
073900100302           vscmsg = Msg(09);
074000100302           vscmsg = %trim(vscmsg) + ' Enter per forzare.';
074100081007           vshsne='X'      ;
074200081007           EXSR AggioSFL   ;
074300081007           leavesr;
074400081007 3     endif    ;
074500081007 3     if (vscval=0    and vshsnv='O')  or
074600100503          (vscval=0    and vshsnv='S' and I41mod<>'C' AND I41OBL='S') ;
074700081007 4        if vshsnv='O'    ;
074800100302            vscmsg = Msg(01);
074900081003          else   ;
075000100302            vscmsg = Msg(01);
075100100302            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
075200100302            vshsnv='X'      ;
075300081215 4        endif  ;
075400081003           errVAL=*on;
075500081003           EXSR AggioSFL   ;
075600081003           leavesr;
075700081215 3     endif    ;
075800110406 3     IF  (vscvald = 0 and vshsnvd = 'O')  or
075900110406           (vscvald = 0 and vshsnvd = 'S'   and
076000110406            I41mod <> 'C' and I41obl = 'S');
076100110406 4        IF  vshsnvd = 'O';
076200110406            vscmsg = Msg(01);
076300110406          ELSE;
076400110406            vscmsg = Msg(01);
076500110406            vscmsg = %trim(vscmsg) + ' Enter per forzare.';
076600110406            vshsnvd = 'X';
076700110406 4        ENDIF;
076800110406          errVALD = *on;
076900110406          EXSR AggioSFL;
077000110406          leavesr;
077100110406 3     ENDIF;
077200110207
077300110207       //?Mi salvo il valore del traffico totale
077400110207 3     IF  vscifo = 'TRT';
077500110207       sav_vscpft = vscpft;
077600110324       sav_vscsna = vscsna;
077700110207       ENDIF;
077800110207
077900110207       //?Il traffico estero è un 'di cui' quindi non può superare
078000110207       //?il traffico totale
078100110324       //?Stessa cosa per le spedizioni
078200110324       IF  vscifo = 'TRE' and vscpft > sav_vscpft;
078300110207         errPFT = *on;
078400110408         vscmsg = msg(16);
078500110207         EXSR AggioSFL;
078600110207         leavesr;
078700110324       ENDIF;
078800110324       IF  vscifo = 'TRE' and vscsna > sav_vscsna;
078900110324         errSNA = *on;
079000110408         vscmsg = msg(17);
079100110324         EXSR AggioSFL;
079200110324         leavesr;
079300110324       ENDIF;
079400110112
079500111012       //?Solo per info "KMS" peso medio
079600111012       IF  VSCval <> 0 and  VSCtva = 'KG ' and VSCifo = 'KMS';
079700111012       //?reimposto il flag di forzatura
079800111012         IF  VSCval <> sav_valkg;
079900111012           $ForzaKg = *off;
080000111012         ENDIF;
080100111012       //?se valore in kg non deve superare i 5000 kg.
080200111012         IF  VSCval > 5000;
080300111012           errVAL = *on;
080400111012           VSCmsg = msg(15);
080500111012           exsr AggioSFL;
080600111012           sav_valkg = VSCval;
080700111012           leavesr;
080800111012         ENDIF;
080900111028       //?msg info se è ugale o supera i 500 kg.
081000111028         IF  VSCval >= 500 and VSCval <> sav_valkg and not $ForzaKg;
081100111012           errVAL = *on;
081200111012           VSCmsg = msg(19);
081300111012           exsr AggioSFL;
081400111012           $ForzaKg = *on;
081500111012           sav_valkg = VSCval;
081600111012           leavesr;
081700111012         ENDIF;
081800111012       ENDIF;
081900111012
082000111012       //?Solo per info "DEL" delta
082100111012       IF  VSCval <> 0 and VSCtva ='%  ' and VSCifo = 'DEL';
082200111012       //?reimposto il flag di forzatura
082300111012         IF  VSCval <> sav_valdel or VSCsgn <> sav_sgndel;
082400111012           $ForzaDel = *off;
082500111012         ENDIF;
082600111012       //?non posso inserire più di 2 cifre quindi al massimo 99
082700111012         IF  VSCval > 99;
082800111012           errVAL = *on;
082900111012           VSCmsg = msg(20);
083000111012           exsr AggioSFL;
083100111012           sav_valdel = VSCval;
083200111012           sav_sgndel = VSCsgn;
083300111012           leavesr;
083400111012         ENDIF;
083500111012       //?msg info se supera 30 +/- indifferente
083600111028         IF  VSCval > 30 and not $ForzaDel and
083700111012            (VSCval <> sav_valdel or VSCsgn <> sav_sgndel);
083800111012           errVAL = *on;
083900111012           VSCmsg = msg(21);
084000111012           %subst(VSCmsg:36:1) = vscsgn;
084100111012           exsr AggioSFL;
084200111012           $ForzaDel = *on;
084300111012           sav_valdel = VSCval;
084400111012           sav_sgndel = VSCsgn;
084500111012           leavesr;
084600111012         ENDIF;
084700111012       ENDIF;
084800081003
084900081003       // se Valore in % --> non deve superare 100%
085000100302 3     if  vscval<>0 and vsctva='%  '    ;
085100081007 4       if vscval>100    ;
085200081003          errVAL=*on ;
085300100302          vscmsg = Msg(05);
085400081003          EXSR AggioSFL   ;
085500081003          leavesr;
085600081007 4       endif      ;
085700081003
085800080925       // Sommo la percentuale se presente per ogni categoria
085900080925       //  se supera 100% segnalo errore
086000081006       //  solo se previsto controllo sulla %
086100081007 4     if wctrprc='P'     ;
086200081006
086300081003       totprc=totprc+vscval    ;
086400081007 5       if totprc>100    ;
086500081003         errVAL=*on ;
086600100302         vscmsg = Msg(04);
086700080925         EXSR AggioSFL   ;
086800080925         leavesr;
086900081007 5       endif      ;
087000081007 4     endif      ;
087100080925
087200100302       // Se immessa la %, visualizzo in base a INFO "SPT"
087300080925       exsr   VISUALimp     ;
087400081007 3     endif      ;
087500100303
087600081007 2     endif      ;
087700110406
087800110406       //?Se Valore con decimali in % --> non deve superare 100%
087900110406 3     IF  vscvald <> 0 and vsctva = '%  ';
088000110406 4       IF  vscvald > 100;
088100110406           errVALD = *on;
088200110406           vscmsg = Msg(05);
088300110406           EXSR AggioSFL   ;
088400110406           leavesr;
088500110406 4       ENDIF;
088600110406 4     ENDIF;
088700111012
088800111012       //?Per Ricavo medio spedizione
088900111012 3     IF  vscifo = 'RMS' and vshsnvd = 'A';
089000111012       //?calcolo il valore
089100111012 3       IF  sav_vscpft <> 0 and sav_vscsna <> 0;
089200111012           wVALrms = sav_vscpft / sav_vscsna;
089300111012         ELSE;
089400111012           wVALrms = 0;
089500111012         ENDIF;
089600111012       //?reimposto il flag di forzatura
089700111012         IF  wVALrms <> sav_valrms;
089800111012           $ForzaRms = *off;
089900111012         ENDIF;
090000111012       //?imposto il campo del video
090100140929         IF  wVALrms > 999;
090200140929           vscvald = 999;
090300111012         ELSE;
090400111012           vscvald = wVALrms;
090500111012         ENDIF;
090600111012       //?msg info se inferiore a 4
090700111012         IF  VSCvald < 4 and not $ForzaRms and
090800111012             wVALrms <> sav_valrms;
090900111012           errVALD = *on;
091000111012           vscmsg = msg(22);
091100111012           exsr AggioSFL;
091200111012           $ForzaRms = *on;
091300111012           sav_valrms = wVALrms;
091400111012           leavesr;
091500111012         ENDIF;
091600111012       //?msg info se superiore a 50
091700111012         IF  VSCvald > 50 and not $ForzaRms and
091800111012             wVALrms <> sav_valrms;
091900111012           errVALD = *on;
092000111012           vscmsg = msg(23);
092100111012           exsr AggioSFL;
092200111012           $ForzaRms = *on;
092300111012           sav_valrms = wVALrms;
092400111012           leavesr;
092500111012         ENDIF;
092600111012       ENDIF;
092700081006
092800100303
092900100305       //?------------
093000100305       //?CHIODI FISSI
093100100305       //?------------
093200100305
093300100305       // se tutte le info della categoria sono obbligatorie ed ho inserito
093400100305       // anche solo 1 info nella videata
093500100305       // errore se info non inserita
093600100305       // oppure la categoria non è tutta obbligatoria ma solo alcune sue info
093700100305       // stessa cosa, se inserito anche solo 1 info nella videta
093800100305       // errore se info non inserita
093900100305       IF  $okqualcosa and (wctrprc = 'S' or vshobl = 'S');
094000100305         SELECT;
094100100422           WHEN  vshsnv <> *blanks and vscval = 0 and vscifo <> 'DEL';
094200100305             errVAL = *on;
094300100305             vscmsg = Msg(01);
094400100305             EXSR AggioSFL;
094500100305             leavesr;
094600111012           WHEN  vshsnvd <> *blanks and vscvald = 0 and vshsnvd <> 'A';
094700110406             errVALD = *on;
094800110406             vscmsg = Msg(01);
094900110406             EXSR AggioSFL;
095000110406             leavesr;
095100100305           WHEN  vshsnf <> *blanks and vscpft = 0;
095200100305             errPFT = *on;
095300100305             vscmsg = Msg(01);
095400100305             EXSR AggioSFL;
095500100305             leavesr;
095600110118           WHEN  vshsns <> *blanks and vscsna = 0 and I41tpv <> 'A';
095700100305             errSNA = *on;
095800100305             vscmsg = Msg(01);
095900100305             EXSR AggioSFL;
096000100305             leavesr;
096100100305           WHEN  vshsne <> *blanks and vscfsn = *blanks;
096200100305             errFSN = *on;
096300100305             vscmsg = Msg(01);
096400100305             EXSR AggioSFL;
096500100305             leavesr;
096600100305         ENDSL;
096700100305       ENDIF;
096800100303
096900100303       // controllo se almeno una info inserita
097000120607       IF  wctrprc = 'U' and vscatb = *blanks;
097100100303         SELECT;
097200100303           WHEN  vshsnv <> *blanks and vscval <> 0;
097300100303             $okUNA = *on;
097400110406           WHEN  vshsnvd <> *blanks and vscvald <> 0;
097500110406             $okUNA = *on;
097600100303           WHEN  vshsns <> *blanks and vscsna > 0;
097700100303             $okUNA = *on;
097800100303           WHEN  vshsnf <> *blanks and vscpft > 0;
097900100303             $okUNA = *on;
098000100303           WHEN  vshsne <> *blanks and vscfsn <> *blanks;
098100100303           $okUNA = *on;
098200100303         ENDSL;
098300100303       ENDIF;
098400100303
098500100303       // carico in sk di comodo i concorrenti inseriti a video
098600100303       IF  vscifo = '10';
098700100303         // codice concorrente inserito carico in sk di appoggio
098800120607         IF  vscfsn = 'S' and %lookup(vscspp:its) = 0 and
098900120607             vscatb = *blanks;
099000100303           yy = %lookup(*blanks:its);
099100100303           its(yy) = vscspp;
099200100303         ENDIF;
099300100303         // codice concorrente non inserito ed era in sk lo tolgo
099400120607         IF  (vscfsn = 'N'or vscfsn = *blanks or vscatb = 'A');
099500100303           yy = %lookup(vscspp:its);
099600100303           IF yy > 0;
099700100303             its(yy) = *blanks;
099800100303           ENDIF;
099900100303         ENDIF;
100000100303       ENDIF;
100100100423
100200100423 2     if  vshifgv<>'?' and vshsgn='S' ;
100300110406         SELECT;
100400110406           WHEN VSHsnv <> *blanks;
100500110406             IF  not $okQualcosa and vscval = 0;
100600110406               clear vscsgn;
100700110406             ELSE;
100800110406               IF  vscsgn = *blanks;
100900110406                 vscsgn = '+';
101000110406               ENDIF;
101100110406             ENDIF;
101200110406           WHEN VSHsnvd <> *blanks;
101300110406             IF  not $okQualcosa and vscvald = 0;
101400110406               clear vscsgn;
101500110406             ELSE;
101600110406               IF  vscsgn = *blanks;
101700110406                 vscsgn = '+';
101800110406               ENDIF;
101900110406             ENDIF;
102000110406           WHEN  VSHsnf <> *blanks;
102100110406             IF  not $OkQualcosa and VSCpft = 0;
102200110406               clear VSCsgn;
102300110406             ELSE;
102400110406               IF  VSCsgn = *blanks;
102500110406                 VSCsgn = '+';
102600110406               ENDIF;
102700110406             ENDIF;
102800110406           OTHER;
102900110406             clear VSCsgn;
103000110406         ENDSL;
103100100423       endif   ;
103200081007
103300080925       // Se info SPT - totale spesa trasporti,  la memorizzo
103400080925       //  per il calcolo delle %
103500101215       if vscifo='TRT' ;
103600080925       SPTPFT=vscpft   ;
103700101215       //?non devo ripartire in %
103800101215       clear sptpft;
103900080925       endif           ;
104000101230
104100101230       //?------------
104200101230       //?CHIODI FISSI
104300101230       //?------------
104400101230       //?Se Trattativa di tipo 'A'
104500110112       //?Memorizzo il segno in campi di work
104600101230       IF  I41tpv = 'A';
104700110112         IF  VSCIFO = 'TRT';
104800110111           wsgntrt = VSCsgn;
104900101230         ENDIF;
105000110112         IF  VSCIFO = 'A/S';
105100110111           wsgnausc = VSCsgn;
105200101230         ENDIF;
105300101230       ENDIF;
105400100305
105500100305       //?-----------------------------------------------------
105600100305       //?CHIODO FISSO a mio utilizzo per controllo concorrenti
105700100305       //?-----------------------------------------------------
105800100305
105900100305       // mi memorizzo se ho inserito almeno una riga di info
106000100305       // per poi controllare meglio i concorrenti, se anche solo
106100100305       // una riga di info inserita i concorrenti sono obbligatori
106200100305         IF  Not $okqualcosa and
106300100305            (vscpft > 0 or vscfsn <> *blanks or vscval <> 0 or
106400110406             vscsna > 0 or vscvald <> 0);
106500100305           $okqualcosa = *on;
106600100305         ENDIF;
106700080925
106800080703       EXSR AggioSFL   ;
106900081006 1     endif   ;
107000081125
107100080703
107200080703       s01nrr=1+s01nrr  ;
107300100302       chain s01nrr    TA41s01    ;
107400081006 0     enddo         ;
107500110111
107600110111       //?Se Trattativa di tipo 'A'
107700110111       IF  I41tpv = 'A';
107800110112         IF  wsgntrt <> wsgnausc;
107900110112           VSCmsg = msg(14);
108000110112           c01csr = 2;
108100110112           errMessage  = *on;
108200110112           errGenerico = *on;
108300110112           leavesr;
108400110112         ENDIF;
108500110111       ENDIF;
108600081006
108700081006       // Controllo ultima categoria
108800081006       EXSR CambioCAT     ;
108900081006
109000081215       // A questo punto le info minime sono presenti
109100081215       // solo per controlli obbligatori
109200100302       if I41obl='S' ;
109300100302         O41ifomin='S' ;
109400081215       endif ;
109500081215
109600081006
109700080409       ENDSR;
109800080409
109900081006       //--------------------------------------------------------------
110000081006       //?cambiata categoria delle info conn- controllo e salvataggi
110100081006       //--------------------------------------------------------------
110200081006       BEGSR  CambioCAT    ;
110300100302
110400100302       if wctrprc='P'  and  I41obl='S' and totprc<100  ;
110500081006       s01nrr=s01nrr-1 ;
110600100302       chain s01nrr    TA41s01    ;
110700081006           errVal=*on;
110800100302           vscmsg = Msg(06);
110900081006           %subst(vscmsg:40:21)=wdescat   ;
111000081006           EXSR AggioSFL   ;
111100081006       endif              ;
111200100303
111300100303       //?-------------------------------------
111400100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
111500100303       //?-------------------------------------
111600100302
111700100303       // controllo che almeno una INFO della categoria sia inserita
111800100303       // se richiesto da tabella
111900100305       IF  wctrprc = 'U' and not $okUNA and
112000100305           (I41obl = 'S' or $okqualcosa);
112100100302         s01nrr -= 1;
112200100302         chain s01nrr TA41S01;
112300100303         SELECT;
112400100303           WHEN vshsns<>' ';
112500100303             errSNA=*on;
112600100303           WHEN vshsne<>' ';
112700100303             errFSN=*on;
112800100303           WHEN  vshsnv<>' ';
112900100303             errVAL=*on;
113000110406           WHEN  vshsnvd <> ' ';
113100110406             errVALD = *on;
113200100303           WHEN  vshsnf<>' ';
113300100303             errPFT=*on;
113400100303         ENDSL;
113500100302         vscmsg = Msg(07);
113600100302         %subst(vscmsg:43:21)=wdescat;
113700100302         EXSR AggioSFL;
113800100302       ENDIF;
113900100303
114000100303       // controllo se inserito 'NESSUN CONCORRENTE' + altri codici
114100100303       // concorrenti
114200100303       IF  %lookup('8810':its) > 0;
114300100303         yy = 1;
114400100303         FOR yy by 1 to %elem(its);
114500100303           IF its(yy) <> *blanks and its(yy) <> '8810';
114600100303             s01nrr -= 1;
114700100303             chain s01nrr TA41S01;
114800100303             errFSN = *on;
114900100303             vscmsg = Msg(12);
115000100303             EXSR AggioSFL;
115100100303             leave;
115200100303           ENDIF;
115300100303         ENDFOR;
115400100303       ENDIF;
115500081006
115600081006       // Pulisco e memorizzo nuovi dati per nuova categoria
115700081006        clear  totprc     ;
115800081215        wctrprc=vshobl  ;
115900081215        wdescat=vsdifo   ;
116000100302        $okUNA = *off;
116100100302
116200081006        ENDSR   ;
116300080925       //--------------------------------------------------------------
116400080925       //?Visualizzo importi in base alla spesa trasporti
116500080925       //--------------------------------------------------------------
116600080925       BEGSR    VISUALimp  ;
116700100302       if vshsnf=' ' and vscpft=0 and vscval<>0 and vsctva='%  '  ;
116800081215       PFTblu=*on     ;
116900081003       vscpft=(SPTpft*vscval)/100   ;
117000080925       if vscpft>0    ;
117100080925       vscvft='EUR'   ;
117200080925       endif ;
117300080925       endif ;
117400080925
117500080925       ENDSR      ;
117600080703       //--------------------------------------------------------------
117700080703       //?Aggiornamento sfl
117800080703       //--------------------------------------------------------------
117900080703       BEGSR AGGIOSFL  ;
118000100304
118100080703       if vscmsg<>*blanks   ;
118200080703       errMessage  = *on;
118300080703       errGenerico = *on;
118400080704       c01csr=s01nrr    ;
118500080703       endif    ;
118600080703
118700080703       exsr ProtCampi   ;
118800080703
118900080703       //  Ripristino dei flag di forzatura
119000081003       if vshsnf='X' and vscpft>0   ;
119100081003       vshsnf='S'   ;
119200080703       endif    ;
119300081003       if vshsns='X' and vscsna>0   ;
119400080703       vshsns='S'   ;
119500080703       endif    ;
119600100302       if vshsnv='X' and vscval<>0  ;
119700081003       vshsnv='S'   ;
119800080703       endif    ;
119900110406       IF  vshsnvd = 'X' and vscvald <> 0;
120000110406         vshsnvd = 'S';
120100110406       ENDIF;
120200081007       if vshsne='X' and (vscfsn='S' or vscfsn='N');
120300081007       vshsne='S'   ;
120400081007       endif    ;
120500080703
120600120607       if vscatb='A' and vshann = 'X';
120700080704       vshann='S'  ;
120800080704       endif   ;
120900080703
121000100302       update  TA41s01  ;
121100100304
121200080703       ENDSR   ;
121300080207       //--------------------------------------------------------------
121400080627       //?Inizializzazione SFL01
121500080207       //--------------------------------------------------------------
121600080409       BEGSR InzS01;
121700080207
121800080207       // Pulizia subfile
121900080207         SflDsp_N    = *on;
122000080207         SflDspCtl_N = *on;
122100100302         write  TA41C01;
122200080207         SflDspCtl_N = *off;
122300080207         SflEnd      = *off;
122400080530
122500080530         clear C01rcd;
122600100507         C01csr=1;
122700080627         S01nrr=0 ;
122800080702         clear Vscmsg;
122900080207         errMessage  = *off;
123000080207         errGenerico = *off;
123100080627         clear savcat  ;
123200080627         clear wcat    ;
123300100302         clear wito    ;
123400100302         clear wits    ;
123500081006         clear sptpft  ;
123600100303
123700100303         clear its;
123800100305         errVAL=*off     ;
123900110406         errVALD = *off;
124000100305         errPFT=*off     ;
124100100305         errSNA=*off     ;
124200100305         errFSN=*off     ;
124300100305         errANN=*off     ;
124400100305
124500100305       $okqualcosa = *off;
124600080409
124700080702       // Carico le info
124800080627       xx=1    ;
124900080627       Indx=1    ;
125000080702       dow   xx<=50    ;
125100100302       if    itoor(xx)<>*blanks  ;
125200100302       clear  TA41S01;
125300080627
125400100302       Wcat= %subst(itoor(xx):1:2)   ;
125500100302       Wito= %subst(itoor(xx):6:3)   ;
125600080627
125700080627       // cambio di categoria
125800080627        if savcat=*blanks or savcat<>wcat               ;
125900080627        savcat=wcat   ;
126000100302        ifosott=*on   ;
126100080627
126200100507       // Verifico se devo visualizzare il titolo della categoria
126300100302         clear  DITG  ;
126400080627         reset TIBS02ds;
126500080627         T02sif = knsif;
126600100302         T02cod = 'ITG';
126700080627         T02mod = 'C'  ;
126800080627         T02ke1 = wcat;
126900080627         TNTBE_RicercaControllo  (kpjba : tibs02ds);
127000080627         if t02err=*blanks ;
127100100302            ditg=t02uni    ;
127200080627         endif            ;
127300080627
127400080627       // Se prevista la visualizzazione della categoria
127500080627       //  emetto la riga in videata,
127600100302       if §itgvis='S'    ;
127700100302         vsdifo=§itgdes         ;
127800080703         vsdifo=%trim(vsdifo)+':'    ;
127900080703         // In VSHIMM   metto una "C" per indicare che si tratta
128000080703         //    di riga di categoria
128100080703         vshimm='C'        ;
128200081007
128300081003         // In VSHOBL metto se devo calcolare il 100% nella categoria
128400100302         // in alternativa imposto il flag se almeno una info deve
128500100302         // essere inserita
128600100302         SELECT;
128700100302           WHEN  §itgctpr <> *blanks;
128800100302             vshobl=§itgctpr   ;
128900100305           WHEN  §itgobl <> *blanks;
129000100305             vshobl = §itgobl;
129100100302         ENDSL;
129200081007
129300081007         EXSr ProtCampi    ;
129400081003
129500080627         s01nrr=s01nrr+1   ;
129600100302         write TA41s01     ;
129700080627         protriga=*off     ;
129800100302         ifosott=*off      ;
129900081007         clear vscfsn      ;
130000100507
130100100507         // Se si tratta della prima riga, imposto posizionamento cursore
130200100507         //  sulla seconda
130300100507         if s01nrr=1  ;
130400100507            C01csr = 2;
130500100507         endif   ;
130600100507
130700081007       endif               ;
130800081007       endif               ;
130900080627
131000100302       Indx=%lookup(wito:ito)   ;
131100080627       if Indx>0                ;
131200100302         dito=itods(Indx)         ;
131300100302         vscifo=ito(Indx)       ;
131400080702
131500080702         // Dati da tabella
131600100302          vshobl=§itoobl  ;
131700100302          vshie =§itotip  ;
131800080704
131900080704          clear vshann  ;
132000100302          if §itoann='S'  ;
132100100302          vshann=§itoann  ;
132200080704          endif   ;
132300111012
132400111012         //?pulisco campi comuni per vscval
132500111012          IF  §ITOitg <> '2A' and §ITOitg <> '5A';
132600111012            clear VSHsnv;
132700111012            clear VSHsgn;
132800111012            clear VSCtva;
132900111012          ENDIF;
133000080704
133100081003         // VALORE
133200111012          if §itosnv<>' '  ;
133300100302          vshsnv=§itosnv  ;
133400100423          vshSGN=§itosgn  ;
133500081003          // tipo valore
133600100302          vsctva=§itotva ;
133700080703          endif   ;
133800110406
133900110406         //?VALORE con DECIMALE
134000110406          IF  §itosnvd <> *blanks;
134100110406            vshsnvd = §itosnvd;
134200110406            vshSGN  = §itosgn;
134300110406          // tipo valore
134400110406            vsctva = §itotva;
134500110406          ENDIF;
134600080704
134700081003         // SPEDIZIONI
134800100302          vshsns=§itosns  ;
134900080704
135000081003         // FATTURATO
135100100302          vshsnf=§itosnf  ;
135200101214          vshSGN=§itosgn  ;
135300081003
135400081003         // SI/NO
135500100302          vshsnE=§itosnE  ;
135600081003
135700100302          vshifgv=§itgvis  ;
135800081007          clear vscint    ;
135900080627
136000080627         // Record senza sottotipo
136100081006         select    ;
136200100302         when §itoits=' '    ;
136300100302            vsdifo=§itodes         ;
136400100302            if ifosott=*on    ;
136500080703               vsdifo=%trim(vsdifo)+':'    ;
136600080703            endif    ;
136700100302            chain    (I41nrv:wito) TIVII01l  ;
136800100302            if %found(TIVII01l)   ;
136900100302             trovavii='S'    ;
137000081126             else ;
137100100302             trovavii='N'    ;
137200081126             endif  ;
137300080702            EXSR CaricaInfo        ;
137400081006
137500081006         // Record con   sottotipo da caricare tutti
137600100302         when   §itoits='S' and §itoifv='S';
137700080627            EXSR CARCon    ;
137800081006
137900081006         // Record con   sottotipo da caricare col '?'
138000081006         //   la categoria visualizzata in questo caso è obbligatoria
138100081126         //  quindi in campo vhifgv è vuoto e posso memorizzare
138200081006         //  il '?'
138300100302         when   §itoits='S' and §itoifv='?';
138400100302          vshifgv=§itoifv  ;
138500081215          yy=1        ;
138600081126
138700100302          if §itonrec=0 ;
138800100302             §itonrec=1  ;
138900081126          endif     ;
139000081126
139100100302          setll (I41nrv:wito) TIVII01l  ;
139200100302          dow yy<=§itonrec  ;
139300081006            EXSR CarconUNA            ;
139400081126            yy=yy+1  ;
139500081126          enddo    ;
139600081126
139700081006         endsl ;
139800080703
139900080703         endif ;
140000080702         endif ;
140100080702
140200080702         xx=xx+1   ;
140300080702         enddo     ;
140400100304
140500080702         ENDSR    ;
140600080627
140700080702       //--------------------------------------------------------------
140800081006       //?Carico con   sottotipo  carica riga per riga
140900080702       //--------------------------------------------------------------
141000080702       BEGSR  CARcon            ;
141100080702
141200100302         tbeke1=wito   ;
141300080925
141400100302         setll ('ITS':tbeke1) tntbe01l   ;
141500100302         READE ('ITS':tbeke1) tntbe01l   ;
141600080702
141700080702         dow not %eof(tntbe01l)   ;
141800080702         if tbeatb=' '    ;
141900100302         dits=tbeuni      ;
142000100302         wits=tbeke2      ;
142100080702         vscspp=tbeke2      ;
142200081127         vshspp=tbeke2      ;
142300100302         vsdifo=§itsdes  ;
142400120607         vshann = §itsann;
142500080702
142600100302         chain    (I41nrv:wito:wits) TIVII01l  ;
142700100302            if %found(TIVII01l)   ;
142800100302             trovavii='S'    ;
142900081126             else ;
143000100302             trovavii='N'    ;
143100081126             endif  ;
143200080702         EXSR CaricaInfo    ;
143300100303
143400100303       //?-------------------------------------
143500100303       //?CHIODI FISSI SOLO PER ALCUNE INFO
143600100303       //?-------------------------------------
143700100303
143800100303       // carico i concorrenti in una sk di comodo
143900100303           IF  trovavii = 'S' and wito = '10';
144000100303             IF  %lookup(wits:its) = 0;
144100100303               yy = %lookup(*blanks:its);
144200100303               its(yy) = wits;
144300100303             ENDIF;
144400100303           ENDIF;
144500100303
144600080925         endif   ;
144700080702
144800100302         READE ('ITS':tbeke1) tntbe01l   ;
144900080702         enddo   ;
145000080702
145100080702       ENDSR;
145200081006       //--------------------------------------------------------------
145300081126       //?Carico con   sottotipo  una sola riga e ricerca col '?'
145400081006       //--------------------------------------------------------------
145500081006       BEGSR  CARconUNA         ;
145600081007
145700081007       vscint='?'    ;
145800081007
145900100302       reade    (I41nrv:wito) TIVII01l  ;
146000081006
146100100302 1     if not %eof(TIVII01l)   ;
146200100302         trovavii='S'    ;
146300081126
146400100302         vscspp=viispf   ;
146500100302         vshspp=viispf   ;
146600081006
146700100302         tbeke1=wito    ;
146800081126
146900100302         tbeke2=viispf  ;
147000100302         chain ('ITS':tbeke1:tbeke2) tntbe01l   ;
147100081006 2       if not %found(tntbe01l)    ;
147200081006         clear tbeuni       ;
147300081006 2       endif    ;
147400100302         dits=tbeuni     ;
147500081006
147600100302         vsdifo=§itsdes   ;
147700100302         vsdifs=§itsdel   ;
147800081007
147900081006         EXSR CaricaINFO  ;
148000081006
148100081006 1x      else    ;
148200100302         trovavii='N'    ;
148300081006
148400100302         // se non trovato dicitura generica da tabella ITO
148500081126           vsdifo=coRicerca       ;
148600081007         clear vsdifs      ;
148700081126         clear vscspp      ;
148800081127         clear vshspp      ;
148900081006
149000081006         EXSR CaricaINFO   ;
149100081006         endif             ;
149200081006
149300081006       ENDSR;
149400080702       //--------------------------------------------------------------
149500081006       //?Carica ogni riga di info commerciale
149600080702       //--------------------------------------------------------------
149700080702         BEGSR CaricaINFO             ;
149800100302
149900080925         clear pftblu   ;
150000080925
150100100302         if trovavii='S'                             ;
150200080703          vshimm=' '        ;
150300100302          vscfsn=viifsn   ;
150400100423          vscval=%abs(viival)   ;
150500110406          vscvald = %abs(viivald);
150600100423
150700100423          // Se previsto segno visualizzo + o -
150800101215          if vshsgn='S'   and vshsnv <> ' ' ;
150900100423            if  viival<0   ;
151000100423             vscsgn='-'  ;
151100100423            else    ;
151200100423             vscsgn='+'  ;
151300100423            endif   ;
151400100423          endif   ;
151500110406          IF  vshsgn = 'S' and vshsnvd <> ' ';
151600110406            IF  viivald < 0;
151700110406              vscsgn = '-';
151800110406            ELSE;
151900110406              vscsgn = '+';
152000110406            ENDIF;
152100110406          ENDIF;
152200101215
152300101215          vscpft=%abs(viipft);
152400101215          if vshsgn='S'   and vshsnf <> ' ' ;
152500101215            if  viipft<0   ;
152600101215             vscsgn='-'  ;
152700101215            else    ;
152800101215             vscsgn='+'  ;
152900101215            endif   ;
153000101215            IF  I41tpv = 'A' and viipft = 0;
153100101215             vscsgn=' '  ;
153200101215            ENDIF;
153300101215          endif   ;
153400100423
153500081007          if vshifgv='?'   and vscspp>*zeros    ;
153600081007            w0040=%int(vscspp)  ;
153700081007            vscval=w0040     ;
153800081007            clear vscfsn     ;
153900081007          endif            ;
154000100302          vscsna=viisna   ;
154100080925          if vscpft>0     ;
154200080925          vshprpft='S'    ;
154300080925          endif           ;
154400100302          vscvft=viivft   ;
154500100302          Dataiso=%date(viidim:*iso)  ;
154600080702          datadmy=Dataiso ;
154700080702          vscdim=%dec(datadmy)  ;
154800081003          clear vscfil  ;
154900100302          if viifil>0   ;
155000100302          vscfil=%editc(viifil:'X')   ;
155100081003          endif         ;
155200081003
155300100302          if viipru<>*blanks ;
155400100302          vscpru=viipru   ;
155500081003          vscdpru='Utente' ;
155600081003          else  ;
155700081003          clear vscpru   ;
155800081003          clear vscdpru   ;
155900081003          endif     ;
156000080925
156100080925          exsr ViSUALimp   ;
156200101230
156300101230         //?------------
156400101230         //?CHIODI FISSI
156500101230         //?------------
156600101230         // se trattativa di tipo 'A' memorizzo il segno del traffico
156700101230         // totale e dell'aumento/sconto
156800101230           IF  I41tpv = 'A';
156900101230             IF  VSCifo = 'TRT';
157000101230               VSHstrt = VSCsgn;
157100101230             ENDIF;
157200101230             IF  VSCifo = 'A/S';
157300101230               VSHsausc = VSCsgn;
157400101230             ENDIF;
157500101230           ENDIF;
157600111012
157700111012         //?Calcolo il ricavo medio spedizione 'RMS'
157800111012           IF  VSCifo = 'TRT' and VSCpft <> 0 and VSCsna <> 0;
157900111012             wVALrms = VSCpft / VSCsna;
158000140929             IF  wVALrms > 999;
158100140929               wVALrms = 999;
158200111012             ENDIF;
158300111012           ENDIF;
158400080925
158500080702         else             ;
158600080703          vshimm='S'      ;
158700080703          Protann=*on     ;
158800080702          clear vscfsn    ;
158900081003          clear vscval    ;
159000110406          clear vscvald   ;
159100080702          clear vscsna    ;
159200080702          clear vscpft    ;
159300080702          clear vscvft    ;
159400080702          clear vscdim    ;
159500080702          clear vscfil    ;
159600080702          clear vscpru    ;
159700081003          clear vscdpru    ;
159800080925          clear vshprpft  ;
159900101230          clear VSHstrt;
160000101230          clear VSHsausc;
160100111012
160200111012         //?Se info 'RMS' imposto il dato
160300111012           IF  VSCifo = 'RMS';
160400111012             VSCvald = wVALrms;
160500111012           ENDIF;
160600111012
160700080702         endif            ;
160800081006
160900080925         exsr ProtCAMPI    ;
161000080703
161100080703         // I record da annullare li visualizzo solo se ci sono
161200100302           IF vshann = *blanks
161300120607                       or (vshann <> ' ' and vscfsn <> *blanks)
161400120607                       or (vshann <> ' ' and vscpft > 0)
161500120607                       or (vshann <> ' ' and vscval <> 0)
161600120607                       or (vshann <> ' ' and vscvald <> 0);
161700100302
161800081003
161900081003         // In interrogazione visulizzo solo i record pieni col sottotipo
162000100302           if I41mod<>'I'  or (vscspp=*blanks) or (vshimm=' ')    ;
162100080703            s01nrr=s01nrr+1   ;
162200100302            write TA41s01     ;
162300101215
162400101215         // Se info TRT - totale spesa trasporti,  la memorizzo
162500101215         //  per il calcolo delle %
162600101215         if vscifo='TRT' ;
162700101215          SPTPFT=vscpft   ;
162800101215         //?non devo ripartire in %
162900101215          clear sptpft;
163000101215         endif           ;
163100080925
163200080703           endif ;
163300081003           endif ;
163400081003
163500080702       ENDSR;
163600080702       //--------------------------------------------------------------
163700080702       //?Gestione protezione campi del sfl
163800080702       //--------------------------------------------------------------
163900080702          BEGSR   ProtCampi   ;
164000100304
164100080703           protfat=*off    ;
164200081003           protVal =*off   ;
164300110406           protVald = *off;
164400111012           protVala = *off;
164500100423           protSGN =*off   ;
164600101214           protSGNa=*off   ;
164700080703           protsped=*off   ;
164800080703           protfsn =*off   ;
164900100302           ifosott=*off    ;
165000080703           Protann=*off ;
165100080704           Infoyell=*off ;
165200080703
165300081007           // Se riga di categoria accendo solo protezione e sottolineatura
165400081007          if vshimm='C'    ;
165500100302           ifosott=*on     ;
165600081007           Protriga=*on    ;
165700081007          else             ;
165800081007
165900080702          // in base a cosa prevede la tabella, proteggo i campi
166000080702          if vshifgv='N' ;
166100100302            ifosott=*on       ;
166200080702          endif    ;
166300080702
166400080704          if vshann<>' '  ;
166500080702           protfat=*on     ;
166600081003           protVal =*on    ;
166700110406           protVald = *on;
166800111012           protVala = *on;
166900100423           protSGN =*on    ;
167000101214           protSGNa=*on    ;
167100080702           protsped=*on    ;
167200080702           protfsn =*on    ;
167300080704           infoyell=*on    ;
167400080702          else            ;
167500081006          protann=*on     ;
167600081031
167700081031          // Se c'è un campo pieno non previsto, abilito l'annullamento
167800110406          if vscpft>0 and vshsnf=' ' and vscval=0 and vscvald = 0;
167900081031          protann=*off    ;
168000081031          endif    ;
168100081031          if vscsna>0 and vshsns=' ' ;
168200081031          protann=*off    ;
168300081031          endif    ;
168400110406          if vscval<>0  and vshsnv=' ' and  vshifgv<>'?'       ;
168500081031          protann=*off    ;
168600081031          endif    ;
168700110406          IF  vscvald <> 0 and vshsnvd = ' ' and vshifgv <> '?';
168800110406            protann = *off;
168900110406          ENDIF;
169000081031          if vscfsn<>' ' and vshsne=' ' ;
169100081031          protann=*off    ;
169200081031          endif    ;
169300081006
169400080703            if vshsnf=' ' ;
169500080702             protfat=*on     ;
169600080702            endif           ;
169700111012            if vshsnv=' ';
169800081003             protVal =*on     ;
169900101215            endif           ;
170000110406            IF  vshsnvd = *blanks;
170100110406              protVald = *on;
170200110406            ENDIF;
170300111012            if vshsnvd='A';
170400111012             protVala=*on     ;
170500111012            endif           ;
170600101215             //protSGN =*on     ;
170700101215             //else  ;
170800100423             // Senon prevede seegno, proteggo solo questo
170900100423             if vshsgn=' '   ;
171000100423             protSGN =*on     ;
171100100423             endif   ;
171200101215            //endif           ;
171300080703            if vshsns=' ' ;
171400080702             protsped=*on     ;
171500080702            endif           ;
171600081003            if vshsne=' ' ;
171700081003             protfsn =*on     ;
171800081003            endif           ;
171900080702          endif           ;
172000080703          // Se record di nuova immissione
172100081003          //  proteggo annullamento
172200080703            if vshimm='S'  ;
172300080703             Protann=*on  ;
172400080703           endif   ;
172500081006
172600081006          // Se prevista scelta sottotipo col'?' visualizzo valore
172700081006          //   col codice e vicino il '?'
172800081006          if vshifgv='?'    ;
172900081006             protVal =*off    ;
173000110406             protVald = *off;
173100111012             protVala = *off;
173200100423             protSGN =*on     ;
173300081007             if vscspp>*zeros    ;
173400081007             else          ;
173500081007             infoyell=*on    ;
173600081007             endif            ;
173700081006          endif               ;
173800081007          endif               ;
173900101214
174000101214       //?Se INFO prevede segno, INFO traffico totale e tipo trattativa
174100101214       //?NON è 'A' devo bloccare il segno, è sempre un valore positivo
174200101214       //?altrimenti lascio libera scelta all'utente.
174300101214          IF  VSHsgn = 'S' and VSCifo = 'TRT' and I41tpv <> 'A';
174400101215            protsgna = *on;
174500101214          ENDIF;
174600110207
174700110207       //?Se INFO "TRT" e trattativa di tipo 'A' non devo visualizzare
174800110207       //?il campo delle spedizioni
174900110207          IF  VSCifo = 'TRT' and I41tpv = 'A';
175000110207            protsped = *on;
175100110207          ENDIF;
175200080703
175300080702          ENDSR    ;
175400080702       //--------------------------------------------------------------
175500080702       //?conferma aggiornamento
175600080702       //--------------------------------------------------------------
175700080702        BEGSR ConfAGGIO              ;
175800100304
175900090520        clear ultdata   ;
176000141106        wcampagna = *off;
176100081003
176200080704       s01nrr=1  ;
176300100302       chain s01nrr    TA41s01    ;
176400080704
176500080704 1     dow %found    ;
176600080704       // Escludo i record di descrizione categoria
176700111012       // e i rcd con calcolo automatico del dato
176800111012 2     if vshimm<>'C' and vshsnvd <> 'A'   ;
176900080925
177000080925       // se valori calcolati da pgm li pulisco prima dei controlli
177100080925       if vshsnf=' ' and vshprpft=' '   ;
177200080925       clear vscpft   ;
177300080925       clear vscvft   ;
177400080925       endif          ;
177500080925
177600081127       IF VShifgv<>'?'    ;
177700100302         chain    (I41nrv:vscifo:vscspp) TIVII01l  ;
177800081127       else   ;
177900100302         chain    (I41nrv:vscifo:vshspp) TIVII01l  ;
178000081127       endif   ;
178100100302             if %found(TIVII01l)   ;
178200100302             trovavii='S'    ;
178300081127             else ;
178400100302             trovavii='N'    ;
178500081127             endif  ;
178600081127
178700081127       // A n n u l l a m e n t o oppure per qualche strano motivo
178800081127       //   la info c'e'  ma a video non è stata inserita
178900080925
179000100302 3     if trovavii='S'      and (vscatb='A'  or (vscfsn=' ' AND
179100100610                                 vscpft=0 and vscsna=0 and vscval=0
179200110406                                 and vscvald = 0 and vscsgn = ' '));
179300100302         delete TIVII000  ;
179400081003 x3    else    ;
179500080704
179600081127       // V a r i a z i o n e :  verifico se modificati dei dati
179700100302 31    if trovavii='S'     and vscatb=' '   ;
179800141106
179900141106       //?Trattativa di tipo 'A' mi memorizzo se variato Aumento/Sconto
180000141106         IF  I41tpv = 'A' and VSCifo = 'A/S';
180100141106           IF  wsgnausc <> VSHsausc or
180200141106               %abs(VIIvald) <> VSCvald;
180300141106             wcampagna = *on;
180400141106             wVIIvald = VSCvald;
180500141106             wsgnausc = VSHsausc;
180600141106           ENDIF;
180700141106         ENDIF;
180800100423
180900100423       // imposto il segno del campo vlaore per il confronto (sul fil non c'è)
181000100423        clear wiisgn   ;
181100100423
181200101215         SELECT;
181300101215           WHEN  VSHsnv <> *blanks;
181400100423         if viival<0   ;
181500100423           Wiisgn='-'  ;
181600100423          else   ;
181700100423           Wiisgn=' '  ;
181800100423            if   vscsgn='+'   ;
181900100423             Wiisgn='+'  ;
182000100423            endif   ;
182100100423          endif   ;
182200110406
182300110406         WHEN  VSHsnvd <> *blanks;
182400110406           IF  viivald < 0;
182500110406             Wiisgn = '-';
182600110406           ELSE;
182700110406             Wiisgn = ' ';
182800110406             IF  vscsgn = '+';
182900110406               Wiisgn = '+';
183000110406             ENDIF;
183100110406           ENDIF;
183200101215
183300101215       // imposto il segno del campo fatturato per il confronto sul file non c'è
183400101215           WHEN  VSHsnf <> *blanks;
183500101215             IF  VIIpft < 0;
183600101215               wiisgn = '-';
183700101215             ELSE;
183800101215               clear wiisgn;
183900101215               IF  VSCsgn = '+';
184000101215                 wiisgn = '+';
184100101215               ENDIF;
184200101215             ENDIF;
184300101215           ENDSL;
184400100423
184500100302 4     if (viifsn<>vscfsn) or
184600100423          (%abs(viival)<>vscval) or
184700110406          (%abs(viivald) <> vscvald) or
184800100423          (Wiisgn<>vscsgn) or
184900100302          (viisna<>vscsna) or
185000100302          (viispf<>vscspp) or
185100101215          (%abs(viipft)<>vscpft) or
185200100302          (viivft<>vscvft) ;
185300080704          // reimposto data ora profilo immissione
185400100302          viidim=*date    ;
185500090520          ultdata=*date    ;
185600100302          viifil=utefil   ;
185700100302          viipru=knmus    ;
185800080704
185900100302          viifsn= vscfsn  ;
186000081007          IF VShifgv<>'?';
186100101215            IF  VSHsnv <> *blanks ;
186200101215            if vscsgn<>'-'  ;
186300100423             viival= %abs(vscval)  ;
186400100423            else  ;
186500100423             viival=0-%abs(vscval)   ;
186600100423            endif  ;
186700101215            ENDIF;
186800110406            IF  VSHsnvd <> *blanks;
186900110406              IF  vscsgn <> '-';
187000110406               viivald = %abs(vscvald);
187100110406              ELSE;
187200110406               viivald = 0 - %abs(vscvald);
187300110406              ENDIF;
187400110406            ENDIF;
187500101215            IF  VSHsnf <> *blanks ;
187600101215              IF  VSCsgn <> '-';
187700101215                VIIpft = %abs(VSCpft);
187800101215              ELSE;
187900101215                VIIpft = 0 - %abs(VSCpft);
188000101215              ENDIF;
188100101215            ENDIF;
188200101215
188300100302          viitva= vsctva  ;
188400081007          else            ;
188500100302          viispf=vscspp   ;
188600100302          if viispf<>*blanks ;
188700100302          viifsn='S'      ;
188800081007          ENDIF           ;
188900081007          ENDIF           ;
189000100302          viisna= vscsna  ;
189100101215          //viipft= vscpft  ;
189200100302          viivft= vscvft  ;
189300081006          // verifico se dei dati non ci voglio più: una volta aggiunti
189400080704          //  quelli previsti, lo tolgo
189500080704
189600101215 5        if (viival<>0 and vshsnv=' ')    ;
189700101215          if (viisna>0 and vshsns<>' ')  or (viipft<>0  and vshsnf<>' ') or
189800110406             (viifsn<>' ' and vshsne<>' ') or
189900110406             (viivald <> 0 and vshsnvd = ' ') ;
190000100302          clear viival   ;
190100080704          endif          ;
190200080704 5        endif          ;
190300110406 5        IF  (viivald <> 0 and vshsnvd = ' ');
190400110406            IF (viisna > 0 and vshsns <> ' ') or
190500110406               (viipft <> 0  and vshsnf <> ' ') or
190600110406               (viifsn <> ' ' and vshsne <> ' ') or
190700110406               (viival <> 0 and vshsnv = ' ');
190800110406              clear viivald;
190900110406            ENDIF;
191000110406 5        ENDIF;
191100100302 5        if (viisna>0 and vshsns=' ')    ;
191200101215          if (viival<>0 and vshsnv<>' ')  or (viipft<>0  and vshsnf<>' ') or
191300110406             (viifsn<>' ' and vshsne<>' ') or
191400110406             (viivald <> 0 and vshsnvd = ' ') ;
191500100302          clear viisna   ;
191600080704          endif          ;
191700080704 5        endif          ;
191800101215 5        if (viipft<>0 and vshsnf=' ')    ;
191900101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
192000110406             (viifsn<>' ' and vshsne<>' ') or
192100110406             (viivald <> 0 and vshsnvd = ' ') ;
192200100302          clear viipft   ;
192300100302          clear viivft   ;
192400080704          endif          ;
192500080704 5        endif          ;
192600100302 5        if (viifsn<>' ' and vshsne=' ')    ;
192700101215          if (viival<>0 and vshsnv<>' ')  or (viisna>0  and vshsns<>' ') or
192800110406             (viipft<>0  and vshsnf<>' ') or
192900110406             (viivald <> 0 and vshsnvd = ' ') ;
193000100302          clear viifsn   ;
193100081006          endif          ;
193200081006 5        endif          ;
193300080704
193400100302        update TIVII000  ;
193500080704 4      endif  ;
193600100423 x3a     else  ;
193700100423         unlock tivii01l   ;
193800081003 3a     endif  ;
193900080704
194000081127       // I m m i s s i o n e
194100100302 3     if trovavii='N'     and vscatb=' ' and (vscfsn<>' ' or
194200110406       vscpft>0 or vscsna>0 or vscval<>0 or vscvald <> 0 or
194300110406       vscsgn <> ' ') ;
194400100302       clear TIVII000;
194500100302        viinrv=I41nrv   ;
194600100302        viitpf=vscifo  ;
194700100302        viispf=vscspp  ;
194800100302        viifsn=vscfsn ;
194900081007        if vshifgv<>'?';
195000101215            IF  VSHsnv <> *blanks;
195100100423            if vscsgn<>'-'   ;
195200100423             viival= %abs(vscval)  ;
195300100423            else  ;
195400100423             viival=0-%abs(vscval)   ;
195500100423            endif  ;
195600101215            ENDIF;
195700110406          IF  VSHsnvd <> *blanks;
195800110406            IF  vscsgn <> '-';
195900110406              viivald = %abs(vscvald);
196000110406            ELSE;
196100110406              viivald = 0 - %abs(vscvald);
196200110406            ENDIF;
196300110406          ENDIF;
196400100302        viitva= vsctva  ;
196500081007        else            ;
196600100302          if viispf<>*blanks ;
196700100302          viifsn='S'      ;
196800081007          endif           ;
196900081007        endif           ;
197000081007
197100100302        viisna= vscsna  ;
197200101215        IF  VSHsnf <> *blanks;
197300101215          IF  VSCsgn <>'-';
197400101215            VIIpft = %abs(VSCpft);
197500101215          ELSE;
197600101215            VIIpft = 0 -%abs(VSCpft);
197700101215          ENDIF;
197800101215        ENDIF;
197900101215        //viipft= vscpft  ;
198000100302        viivft= vscvft  ;
198100100302        viidim=*date    ;
198200090520        ultdata=*date    ;
198300100302        viifil=utefil   ;
198400100302        viipru=knmus    ;
198500141106
198600141106       //?Trattativa di tipo 'A' mi memorizzo che ho scrutto Aumento/Sconto
198700141106         IF  I41tpv = 'A' and VSCifo = 'A/S' and VSCvald <> *zeros;
198800141106           wcampagna = *on;
198900141106           wVIIvald = VSCvald;
199000141106           wsgnausc = VSHsausc;
199100141106         ENDIF;
199200080704
199300100302        write TIVII000 ;
199400110110        feod  TIVII01L;
199500081003  3a    endif           ;
199600081003  3     endif           ;
199700080704  2     endif           ;
199800080704
199900080704        s01nrr=s01nrr+1 ;
200000100302        chain s01nrr    TA41s01    ;
200100080704  1     enddo           ;
200200080704
200300100304         // Info totali inserite
200400100304         // Se inserimento obbligatorio--> posso impostare la "S"
200500100304         //  altrimenti controllo da VII
200600100302        clear waggiovis    ;
200700081003
200800100302  2       if I41obl=' '   ;
200900081015            // Rifaccio il controllo del sfl come se fosse
201000081015            //   obbligatorio --> se no errori aggiorno "S" tutto ok
201100100302            savmod=I41mod   ;
201200100302            I41mod='C'      ;
201300100302            I41obl='S' ;
201400081215
201500081015            exsr contrs01   ;
201600081215
201700100302            I41obl=' ' ;
201800100302            I41mod=savmod   ;
201900081015
202000081015  3         if ErrGenerico=*off;
202100100302              Waggiovis='S';
202200081015  3         endif  ;
202300081015
202400081015  x2      else;
202500100302          Waggiovis='S'   ;
202600081015  2       endif           ;
202700081015
202800100302              O41ifotot=Waggiovis   ;
202900141106
203000141106       //?Se il cliente è in campagna devo scrivere la fase per OBJ da TTR
203100150114       //?per ogni campagna aperta
203200141106         IF  wcampagna;
203300150114           clear TRKC73DS;
203400141106           chain (I41nrv) TIVIS05L;
203500141106           IF  not %found(TIVIS05L) or VISksc = 0;
203600141106             leavesr;
203700141106           ENDIF;
203800150114           IKC73ksu = VISksc;
203900150114           IKC73dat = VISdat;
204000150206           IKC73ftr = 'S';
204100150114           TRKC73r (kpjba:TRKC73DS);
204200150114           IF  OKC73err <> *blanks;
204300141106             leavesr;
204400141106           ENDIF;
204500150114       //?Se arrivo qua ok cliente sulla trattativa
204600150114       //?Per ogni campagna ancora aperta sul cliente devo scrivere la fase TR
204700150114           xx = 1;
204800150114           FOR xx by 1 to %elem(skOKC73ncm);
204900150114             IF  skOKC73ncm(xx) > *zeros;
205000150114           //?quindi richiamo pgm scrittura fase
205100150114               clear TRKC71DS;
205200150114               IKC71ncm = skOKC73ncm(xx);
205300150114               IKC71ksu = OKC73ksu;
205400150114               IKC71acm = FaseObjTtr;
205500150114               IKC71pea = wVIIvald;
205600150114               IF  wsgnausc = '-';
205700150114                 IKC71pea = IKC71pea * -1;
205800150114               ENDIF;
205900150114               IKC71nrv = I41nrv;
206000150114               TRKC71R (kpjba:TRKC71DS);
206100150114               IF  OKC71err = 'E';
206200150114                 leavesr;
206300150114               ENDIF;
206400150114             ENDIF;
206500150114           ENDFOR;
206600150216       //?Dopo aver scritto la fase TR sul codice cliente della trattativa
206700150216       //?Verifico se il cliente è ancora Papà o ha cambiato Papà
206800150216           clear TIBS10DS;
206900150216           D10tle = 'ST';
207000150216           D10paf = 'P';
207100150216           D10cod = VISksc;
207200150216           TIBS10R (TIBS10DS);
207300150216         //?Se ritorna errore vuol dire che è ancora Papà
207400150216           IF  D10err <> *blanks;
207500150216             leavesr;
207600150216           ENDIF;
207700150216         //?Se non c'è il codice padre vado via
207800150216           IF  D10cop = *zeros;
207900150216             leavesr;
208000150216           ENDIF;
208100150216         //?Se stesso codice padre vado via
208200150216           IF  D10cop = OKC73ksu;
208300150216             leavesr;
208400150216           ENDIF;
208500150216         //?Se arrivo qua vuol dire che è diventato figlio
208600150216         //?verifico se il nuovo papà è in campagna per poi
208700150216         //?scrivere la fase TR anche sul nuovo papà
208800150216           clear TRKC73DS;
208900150216           IKC73ksu = D10cop;
209000150216           IKC73dat = VISdat;
209100150216           IKC73ftr = 'S';
209200150216           TRKC73r (kpjba:TRKC73DS);
209300150216           IF  OKC73err <> *blanks;
209400150216             leavesr;
209500150216           ENDIF;
209600150216       //?Se arrivo qua vuol dire che il nuovo papà è in campagna
209700150216       //?Per ogni campagna ancora aperta sul cliente devo scrivere la fase TR
209800150216           xx = 1;
209900150216           FOR xx by 1 to %elem(skOKC73ncm);
210000150216             IF  skOKC73ncm(xx) > *zeros;
210100150216           //?quindi richiamo pgm scrittura fase
210200150216               clear TRKC71DS;
210300150216               IKC71ncm = skOKC73ncm(xx);
210400150216               IKC71ksu = OKC73ksu;
210500150216               IKC71acm = FaseObjTtr;
210600150216               IKC71pea = wVIIvald;
210700150216               IF  wsgnausc = '-';
210800150216                 IKC71pea = IKC71pea * -1;
210900150216               ENDIF;
211000150216               IKC71nrv = I41nrv;
211100150216               TRKC71R (kpjba:TRKC71DS);
211200150216               IF  OKC71err = 'E';
211300150216                 leavesr;
211400150216               ENDIF;
211500150216             ENDIF;
211600150216           ENDFOR;
211700141106         ENDIF;
211800100304
211900080702        ENDSR;
212000080702
212100080206       //--------------------------------------------------------------
212200080206       //?Operazioni finali.
212300080206       //--------------------------------------------------------------
212400080206       BEGSR RoutEnd;
212500080627
212600110218         //if I41tla<>' '    ;
212700080704         // Chiusura pgm   ;
212800080704         clear tibs02ds  ;
212900080704         t02tla='C'      ;
213000080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
213100080704
213200080206         *inLR = *on;
213300110218         //endif             ;
213400080704
213500080206         return;
213600080206
213700080206       ENDSR;
213800080206
213900080206      /end-free
214000080206
214100080206       //--------------------------------------------------------------
214200080206       //?Schiere a tempo di compilazione.
214300080206       //--------------------------------------------------------------
214400080206
214500080410** - MSG ------------------------------------------------------------------ -*
214600100302Immettere INFO.                                                                1
214700100302Per Info non presente (N), non indicare gli altri dati                         2
214800100302Info non più in essere: ANNULLARLA!!                                           3
214900100302La somma delle percentuali per questa categoria di info supera il 100%         4
215000100302La percentuale di questa info supera 100%                                      5
215100100302La somma delle % per la categoria info xxxxxxxxxxxxxxxxxxxxx deve essere 100%  6
215200100302Inserire almeno una info per la categoria xxxxxxxxxxxxxxxxxxxxx                7
215300100302Immettere solo  '?'  se si vuole attivare la ricerca                           8
215400100302Indicare presenza o assenza della INFO con "S o N".                            9
215500100302Codice già presente !!                                                        10
215600100423Non ammesso un valore negativo                                                11
215700100303Se immesso "NESSUN CONCORRENTE" non inserire altri concorrenti                12
215800100917ATTENZIONE!! Il valore massimo consentito è di 99 milioni                     13
215900101230Segno TRAFFICO TOTALE non congruente con segno AUMENTO/SCONTO                 14
216000111021Valore Peso Medio consentito: max 5.000 Kg.                                   15
216100110408Il Valore del Traffico Estero non può superare quello del Traffico Totale     16
216200110408Il n. di spedizioni Estere non può superare il n. di spedizioni Totale        17
216300111012Il Valore del Traffico non può essere inferiore alle Spedizioni Annue.        18
216400111028Attenzione!! Peso medio maggiore/uguale a 500 Kg. Enter per forzare.          19
216500111021Valori del Delta consentiti: max +99%; min -99%                               20
216600111012Attenzione inserito un delta oltre x30%. Enter per forzare!                   21
216700111012Attenzione ricavo medio inferiore a 4 EURO. Enter per forzare!                22
216800111012Attenzione ricavo medio superiore a 50 EURO. Enter per forzare!               23
216900120608Info non più in essere: verificarla e se non serve annullarla. ENTER forza!   24
