000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300101014      //?TNTB82R - Liste distribuzione creazione automatica bolle
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200101014     fTNtb82D   cf   e             workstn indds(IndDspF)
001300080206     f                                     infds(InfDspF)
001400101014     f                                     sfile(TB82S02 : S02nrr)
001500100312     FAZORG21l  IF   E           K DISK
001600100312     fazorg01l  if   e           k disk
001700101014     ftntbe01l  uf a e           k disk
001800080206      //---------------------------------------------------------------
001900100824
002000080207      // - Tasti funzionali a video
002100080207     d c_F01           c                   const(x'31')
002200080207     d c_F02           c                   const(x'32')
002300080207     d c_F03           c                   const(x'33')
002400080207     d c_F05           c                   const(x'35')
002500080207     d c_F06           c                   const(x'36')
002600080207     d c_F07           c                   const(x'37')
002700080207     d c_F08           c                   const(x'38')
002800080207     d c_F09           c                   const(x'39')
002900080207     d c_F10           c                   const(x'3A')
003000080207     d c_F12           c                   const(x'3C')
003100080207     d c_F13           c                   const(x'B1')
003200080207     d c_F14           c                   const(x'B2')
003300080207     d c_F15           c                   const(x'B3')
003400080207     d c_F16           c                   const(x'B4')
003500080207     d c_F17           c                   const(x'B5')
003600080207     d c_F18           c                   const(x'B6')
003700080207     d c_F19           c                   const(x'B7')
003800080207     d c_F20           c                   const(x'B8')
003900080207     d c_F21           c                   const(x'B9')
004000080207     d c_F22           c                   const(x'BA')
004100080207     d c_F23           c                   const(x'BB')
004200080207     d c_F24           c                   const(x'BC')
004300080207     d c_Enter         c                   const(x'F1')
004400080207     d c_RollDown      c                   const(x'F4')
004500080207     d c_RollUp        c                   const(x'F5')
004600080214
004700080214      // - Attributi di visualizzazione
004800080215      //  -  DspAtr() - Normale
004900080214     d C_dspatr_Norm   c                   const(x'20')
005000080215      //  -  DspAtr(RI)
005100080214     d C_dspatr_RI     c                   const(x'21')
005200080215      //  -  DspAtr(ND)
005300080214     d C_dspatr_ND     c                   const(x'27')
005400080215      //  -  DspAtr(BL) / Color(Red)
005500080214     d C_dspatr_BL     c                   const(x'28')
005600080206
005700080206      //---------------------------------------------------------------
005800080206      //?Definizione schiere.
005900080206      //---------------------------------------------------------------
006000080206      // - Messaggi di errore
006100100910     d MSG             s             78    dim(10) ctdata perrcd(1)
006200131029     d filiali         s              3  0 dim(154)
006300080206      //?Definizione aree dati.
006400080206      //---------------------------------------------------------------
006500080206      // - Dati utente
006600080206     d §AzUte        e ds                  extname(AZUTE00F)
006700080206     d                                     dtaara
006800080206     d §DatiUte      e ds                  extname(dDatiUte)
006900080206     d                                     dtaara
007000080206      //---------------------------------------------------------------
007100080206      //?Definizione strutture dati.
007200080206      //---------------------------------------------------------------
007300080206      // - Status
007400080206     d Psds           sds
007500080206     d   SDSpgm          *proc
007600080206
007700080206      // - InfDS
007800080206     d InfDspF         ds
007900080207     d  dsp_aid              369    369a                                        AID byte
008000080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
008100080207     d  min_nrr              378    379i 0                                      Subfile min rrn
008200080207     d  num_rcds             380    381i 0                                      Subfile num rcds
008300080206
008400080206      // - Indicatori su DspF
008500080208     d IndDspF         ds
008600080206        // - Indicatori di gestione del subfile
008700080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008800080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
008900080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009000080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009100080206        // - Indicatori di errore
009200080206     d  errMessage                    1n   overlay(IndDspF : 28)
009300080606     d  errGenerico                   1n   overlay(IndDspF : 99)
009400080627        // - Indicatori vari
009500101014     d  ProtLis                       1n   overlay(IndDspF : 10)
009600101018     d  RevFil                        1n   overlay(IndDspF : 11)
009700101018     d  RevDes                        1n   overlay(IndDspF : 12)
009800101014     d  Errfil                        1n   overlay(IndDspF : 40)
009900101014     d  ErrLIS                        1n   overlay(IndDspF : 41)
010000101014     d  Errclis                       1n   overlay(IndDspF : 43)
010100080206
010200080206      // - Parametri ricevuti
010300080206     d KPJBA         e ds
010400080206
010500080206      // - Reperimento dati utente
010600080206     d TIBS34ds      e ds
010700080206     d dUte01        e ds
010800080702     d DLAT          e ds
010900101014      // - Ricerca/Controllo tabelle
011000101014     d TIBS02ds      e ds                  inz
011100101018     d
011200101018     d Tntb82ds      e ds
011300101018     d fnlv55ds      e ds
011400101014     d
011500101014     d DLDS          e ds
011600101018     d og147         e ds
011700101018     d og148         e ds
011800101018     d og150         e ds
011900080206
012000100316     d wlbdat          ds                  inz
012100100316     d  g02dat                 1      8  0
012200100316     d  g02inv                 9     16  0
012300100316     d  g02err                17     17
012400100316     d  g02tgi                18     22  0
012500101018
012600101018     d Datasys         s               d   inz(*sys) datfmt(*iso)
012700101018     d  WOK            s              1
012800100316
012900080206      //---------------------------------------------------------------
013000080206      //?Definizione variabili globali.
013100080206      //---------------------------------------------------------------
013200080206
013300080206      // - Flags booleani
013400080208     d $Fine           s               n   inz(*off)
013500100312     d $InzS02         s               n   inz(*on)
013600100312     d $Inzd01         s               n   inz(*on)
013700080206
013800100312     d $Video          s              2    inz('D1')
013900100315     d S02nrr          s              4  0 inz
014000081006     d XX              s              3  0 inz
014100101014     d YY              s              3  0 inz
014200080627     d Indx            s              3  0 inz
014300101014     d Prima           s              1    inz
014400101014     d kke1            s                   like(tbeke1)
014500101014     d kke2            s                   like(tbeke2)
014600101014     d kfil            s                   like(orgfil)
014700080414
014800100910     D*----------------
014900101014     D* PARAMETRI TNSD24R
015000100910     D*----------------
015100101014     d   pInFIL        s              3
015200101014     d   pInFAG        s              1
015300101014     d   pInDES        s             25
015400101014     d   pInDIT        s              3
015500101014
015600101014     d Digits          c                   const('0123456789')
015700080206      //---------------------------------------------------------------
015800080206      //?Definizione procedure usate.
015900080206      //---------------------------------------------------------------
016000080414      /copy gaitrasrc/srcprotopr,tibs34r
016100100316      /copy gaitrasrc/srcprotopr,xsrda8
016200100910      /copy gaitrasrc/srcprotopr,bch10
016300101014      /copy gaitrasrc/srcprotopr,tibs02r
016400101014      /copy gaitrasrc/srcprotopr,tnsd24r
016500101018      /copy gaitrasrc/srcprotopr,fnlv55r
016600080206
016700080206      //---------------------------------------------------------------
016800080206      //?Riepilogo indicatori.
016900080206      //---------------------------------------------------------------
017000080207      // - Comuni
017100080207      // 28    : Emissione messaggio di errore a video
017200080207      // - C01/S01
017300080207      // 30    : Condiziona SFLDSP    (*not)
017400080207      // 31    : Condiziona SFLDSPCTL (*not)
017500080207      // 30+31 : Condiziona SFLCLR
017600100824      // 32    : Condiziona SFLNXTCHG
017700080207      // 50    : Errore: Opzione errata
017800080207      // - D01
017900080207      // - Comuni
018000080207      // 99    : Generico di Errore
018100080206      //---------------------------------------------------------------
018200080206
018300080206      //---------------------------------------------------------------
018400080206      //?M A I N - L I N E
018500080206      //---------------------------------------------------------------
018600080627     c     *Entry        plist
018700080206     c                   parm                    KPJBA
018800101018     c                   movel     kpjbu         tntb82ds
018900101018     c                   clear                   §tb82crea
019000080702
019100080206      /free
019200080206
019300080206       // Operazioni iniziali
019400080206       exsr RoutInz;
019500081215
019600080206       // Gestione video
019700081215 3     DOW $Fine = *off;
019800081215 4       select;
019900080530
020000080530         // Videata di selezioni
020100100312           when $Video = 'D1';
020200100312             exsr GesD01;
020300080530
020400100830         // Videata del sfl per inserimento/visualizzazione
020500100312           when $Video = 'S2';
020600100312             exsr GesS02;
020700100830
020800080702           other   ;
020900080206             $Fine = *on;
021000081215 4       endsl;
021100081215 3     ENDDO;
021200080206
021300080206       // Operazioni finali
021400080206       exsr RoutEnd;
021500080206
021600080206       //--------------------------------------------------------------
021700080206       //?Operazioni iniziali.
021800080206       //--------------------------------------------------------------
021900080206       BEGSR RoutInz;
022000100312       $inzs02=*on;
022100100312       $inzd01=*on;
022200080703
022300080206         // Reperimento dati job
022400080206         exsr DatiJob;
022500100827
022600080627         ENDSR;
022700080206
022800080206       //--------------------------------------------------------------
022900080206       //?Reperimento Dati del job (Utente/Operativi).
023000080206       //--------------------------------------------------------------
023100080206       BEGSR DatiJob;
023200080206
023300080206         in(E) §AzUte;
023400080206         if NOT %error;
023500080206           in(E) §DatiUte;
023600080206         endif;
023700080206         if %error or RSut = *blanks;
023800080206           clear TIBS34ds;
023900080206           tibs34r(tibs34ds);
024000080206           in §AzUte;
024100080206           in §DatiUte;
024200080206         endif;
024300080206
024400080206       ENDSR;
024500100312       //--------------------------------------------------------------
024600100312       //?Gestione viodeata 01
024700100312       //--------------------------------------------------------------
024800100312       BEGSR Gesd01;
024900100312
025000100312         // Inizializzazione videata
025100100312         if  $Inzd01 = *on;
025200100312            exsr Inzd01;
025300100312            $Inzd01  = *off;
025400100312         endif;
025500101018
025600101018         // Se pgm richiamato passo direttamente al  SFL
025700101018         if §tb82lis<>*blanks  ;
025800101018         leavesr   ;
025900101018         endif  ;
026000100312
026100100312         // Emissione Testata e Piede con tasti funzionali abilitati
026200100824         if  errMessage  = *off;
026300101014           write  TB82T01;
026400101014           write  TB82Z01;
026500100312         endif;
026600100312
026700100312         // Emissione videata
026800101014         exfmt  TB82D01;
026900100312
027000100312         reset errMessage;
027100100312         reset errGenerico;
027200100824         clear V1cmsg    ;
027300101014         clear Errlis    ;
027400101014         clear ErrClis    ;
027500100312
027600100312 1       SELECT;
027700100312
027800100312         // - F3=Fine
027900100312 1         WHEN  dsp_aid = c_F03;
028000100312            $Fine = *on;
028100100312
028200100312
028300100824 x1      // Invio
028400100312           OTHER;
028500100316
028600100316           exsr CTRD01  ;
028700100316
028800100316           if ErrGenerico=*ON    ;
028900100316                 leavesr;
029000100316 2             endif;
029100100316
029200100315           $video='S2' ;
029300100825           $inzs02=*on;
029400100824
029500100312 1       ENDSL;
029600100312
029700100312       ENDSR;
029800100312
029900100316       //--------------------------------------------------------------
030000100316       //?controlli video 1
030100100316       //--------------------------------------------------------------
030200100316       BEGSR CTRD01  ;
030300100316
030400101014       //  se messo '?' visualizzo tabella
030500101014       Indx=%scan('?':v1clis)   ;
030600101014 1     if Indx>0   ;
030700101014             clear  TIBS02ds;
030800101014             T02mod = 'R';
030900101014             T02cod = 'LDS';
031000101014             T02sif = KNSIF;
031100101014             TNTBE_RicercaControllo  (kpjba : tibs02ds);
031200101014 2           if  T02err = *blanks and t02ke1=*blanks ;
031300101014               V1clis = T02ke2;
031400101014               V1dlis = T02uni;
031500101018               v1cprop=%subst(t02uni:41:1) ;
031600131029               v1cldvh=%subst(t02uni:42:1) ;
031700131029               v1cpost=%subst(t02uni:43:1) ;
031800101014               else ;
031900101014               clear v1clis   ;
032000101014 2             endif   ;
032100101014               errGenerico = *on;
032200101014               leavesr;
032300101014 1     endif       ;
032400101014
032500131104        // Controllo: se trovata, prendo descrizione altrimenti la chiedo
032600101014             clear  TIBS02ds;
032700101014             T02mod = 'C';
032800101014             T02sif = knsif;
032900101014             T02cod = 'LDS';
033000101014             T02ke2 = V1clis;
033100101014             TNTBE_RicercaControllo  (kpjba : tibs02ds);
033200101018 1           if T02err = *blanks;
033300101014               v1dlis  = T02uni;
033400101018               if v1cprop=' '  ;
033500101018                v1cprop=%subst(t02uni:41:1) ;
033600101018               endif  ;
033700131029               if v1cldvh=' '  ;
033800131029                v1cldvh=%subst(t02uni:42:1) ;
033900131029               endif  ;
034000131029               if v1cpost=' '  ;
034100131029                v1cpost=%subst(t02uni:43:1) ;
034200131029               endif  ;
034300101018 x1          else;
034400101014
034500101014             // non trovato: immettere descrizione
034600101018 2           if v1clis=*blanks   ;
034700101014               errMessage  = *on;
034800101014               errGenerico = *on;
034900101014               ErrClis      = *on;
035000101014               V1cmsg = Msg(04);
035100101014               leavesr;
035200101018 2           endif;
035300101018 2           if v1dlis=*blanks   ;
035400101018 3           if §tb82lis<>*blanks and §tb82dlis<>*blanks  ;
035500101018             v1dlis=§tb82dlis  ;
035600101018 x3          else ;
035700101014               errMessage  = *on;
035800101014               errGenerico = *on;
035900101014               Errlis      = *on;
036000101014               V1cmsg = Msg(01);
036100101014               leavesr;
036200101018 3           endif;
036300101018
036400101018 2           endif;
036500101018 1           endif;
036600100708
036700100316       ENDSR  ;
036800080206       //--------------------------------------------------------------
036900100312       //?Gestione SFL 02
037000080206       //--------------------------------------------------------------
037100100312       BEGSR GesS02;
037200080207
037300080207         // Inizializzazione videata
037400100312         if  $InzS02 = *on;
037500100312            exsr InzS02;
037600100824            $InzS02  = *off;
037700100824         endif;
037800100831
037900100831         // Posizionamento cursore
038000100910         if  C02csr  >0     ;
038100100827           C02rcd = C02csr;
038200100827         endif  ;
038300080207
038400080207         // Emissione Testata e Piede con tasti funzionali abilitati
038500100824         if  errMessage  = *off;
038600101014           write  TB82T01;
038700101014           write  TB82Z02;
038800101014           write  TB82d01;
038900080207         endif;
039000080703
039100080207         // Emissione videata
039200101014         exfmt  TB82C02;
039300080410
039400080207         reset errMessage;
039500080207         reset errGenerico;
039600100824         clear V1cmsg;
039700101014         errfil=*off ;
039800080207
039900080523 1       SELECT;
040000080207
040100080207         // - F3=Fine
040200080523 1         WHEN  dsp_aid = c_F03;
040300080409            $Fine = *on;
040400080207
040500080207         // - F12=Ritorno
040600080523 1         WHEN  dsp_aid = c_F12;
040700101018           if §tb82lis<>*blanks    ;
040800101018            $Fine = *on;
040900101018            else  ;
041000100315           $video='D1'   ;
041100101014           Protlis =*off   ;
041200101014           $inzd01=*on;
041300101018           endif  ;
041400101014
041500101014         // F16= Annullamento
041600101014 1         when dsp_aid = c_F16  ;
041700101014           clear kke1   ;
041800101014           clear kke2   ;
041900101014           kke1  = v1clis   ;
042000101014           // Cancello tutti i record esistenti
042100101014           setll    ('LDS':kke1)          tntbe01l    ;
042200101014           reade    ('LDS':kke1)          tntbe01l    ;
042300101014 2         dow     not %eof(tntbe01l)   ;
042400101014
042500101014           delete tntbe000   ;
042600101014           reade    ('LDS':kke1)          tntbe01l    ;
042700101014 2         enddo   ;
042800101014
042900101014           // testata
043000101014           clear kke2   ;
043100101014           delete   ('LDS':kke2: kke1)          tntbe01l    ;
043200101014
043300101014           $video='D1' ;
043400101014           $inzd01=*on;
043500101014           Protlis =*off   ;
043600100901
043700080530 x1      // Invio / F6
043800080207           OTHER;
043900081006
044000100315               exsr ContrS02 ;
044100100910 2             if  errGenerico = *on ;
044200081006                 leavesr;
044300100824 2             endif;
044400080530
044500100907         // F6=conferma Aggiornamento
044600101014 2         if   dsp_aid = c_F06  ;
044700101014           Exsr Aggiotabel   ;
044800101018           if §tb82lis<>*blanks    ;
044900101018           §tb82crea='S'  ;
045000101018           kpjbu=tntb82ds   ;
045100101018            $Fine = *on;
045200101018           else  ;
045300101018           $video='D1'   ;
045400101018           Protlis =*off   ;
045500101018           $inzd01=*on;
045600101018           endif   ;
045700100915
045800101014 2         endif   ;
045900080207
046000100910 1       ENDSL;
046100080207
046200080207       ENDSR;
046300080526       //--------------------------------------------------------------
046400100312       //?controlli SFL02
046500080409       //--------------------------------------------------------------
046600100312       BEGSR ContrS02;
046700100315
046800101014       s02nrr=1  ;
046900101014       chain    s02nrr   TB82s02   ;
047000101014
0471001010140      dow %found     ;
047200101014
047300101014       clear   vsdfil   ;
047400101014       // '?' sulla filiale
047500101014       Indx= %scan('?':vscfil)  ;
0476001010141      if Indx>0   ;
047700101014       clear pinfil   ;
047800101014       clear pinfag   ;
047900101014       clear pindes  ;
048000101014       clear pindit   ;
048100101014        TNSD24R (pinfil:pinfag:pindes:pindit)  ;
0482001010142       if pinfil > *zeros  ;
048300101014        vscfil=pinfil  ;
048400101014        vsdfil=pindes  ;
048500101014        else ;
048600101014        clear vscfil   ;
048700101014        clear vsdfil   ;
0488001010142       endif   ;
048900101014
049000101014        Errgenerico=*on  ;
049100101014        Exsr Aggiosfl   ;
049200101014        leavesr  ;
0493001010141       endif   ;
049400101014
049500101014       // controllo la filiale  ;
0496001010141      if  vscfil<=*zeros and  (vscca<>*blanks or vscncl>0 or vscpkg>0)  ;
049700101014         ErrGenerico=*on  ;
049800101014         ErrMessage =*on  ;
049900101014         ErrFIL= *on   ;
050000101014         v1cmsg=msg(02)  ;
050100101014         EXSR AggioSFL   ;
050200101014         leavesr  ;
0503001010141      endif   ;
050400101014
0505001010141        if vscfil<>*blanks and vscfil<>*zeros  ;
050600101014
0507001010142        IF  %check(digits:vscfil) > 0;
050800101014           ErrMessage  = *on;
050900101014           ErrGenerico = *on;
051000101014           ErrFIL      = *on;
051100101014           V1cmsg      = Msg(03);
051200101014           EXSR AggioSFL   ;
051300101014           leavesr;
0514001010142        ENDIF;
051500101014
051600101014       kfil=%int(vscfil)  ;
051700101014
051800101014       clear og148          ;
051900101014       clear orgfag         ;
052000101014       chain   kfil    azorg01l   ;
0521001010142      if %found(azorg01l)  ;
052200101014       og148= orgde8   ;
0523001010142      endif   ;
052400101014
0525001010142      if  orgfag<>'A' and orgfag<>'F'  and   §oglue <>'S'  ;
052600101014           ErrMessage  = *on;
052700101014           ErrGenerico = *on;
052800101014           ErrFIL      = *on;
052900101014           V1cmsg      = Msg(03);
053000101014           EXSR AggioSFL   ;
053100101014           leavesr;
0532001010142      endif   ;
053300101014
053400101014       vsdfil=orgdes   ;
0535001010141      endif   ;
053600101014
053700101014       exsr  Aggiosfl   ;
053800101014
053900101014       s02nrr= s02nrr + 1 ;
054000101014       chain    s02nrr   TB82s02   ;
054100101014       enddo  ;
054200100830
054300100830       ENDSR  ;
054400081126
054500080703       //--------------------------------------------------------------
054600080703       //?Aggiornamento sfl
054700080703       //--------------------------------------------------------------
054800080703       BEGSR AGGIOSFL  ;
054900100830
055000100824       if v1cmsg<>*blanks   ;
055100080703       errMessage  = *on;
055200080703       errGenerico = *on;
055300100315       c02csr=s02nrr    ;
055400080703       endif    ;
055500100824
055600101014       update  TB82s02  ;
055700100830
055800080703       ENDSR   ;
055900100312       //--------------------------------------------------------------
056000100312       //?Inizializzazione videata 01
056100100312       //--------------------------------------------------------------
056200100312       BEGSR Inzd01  ;
056300100312
056400100315         clear V1cmsg;
056500100826         errMessage  = *off;
056600100312         errGenerico = *off;
056700101014         Errlis=*off       ;
056800101014         ErrClis=*off       ;
056900101014         Errfil=*off       ;
057000101014         v1clis= '?       '   ;
057100101018         clear v1cprop   ;
057200131029         clear v1cldvh   ;
057300131029         clear v1cpost   ;
057400101014         clear v1dlis    ;
057500101018
057600101018         // se passati i dati li imposto
057700101018         if §tb82lis<>*blanks   ;
057800101018         v1clis=§tb82lis   ;
057900101018         v1cprop=§tb82tipol   ;
058000101018
058100101018         exsr ctrd01   ;
058200101018
058300101018           $video='S2' ;
058400101018           $inzs02=*on;
058500101018         endif    ;
058600100831
058700100312         ENDSR    ;
058800080207       //--------------------------------------------------------------
058900100312       //?Inizializzazione SFL02
059000080207       //--------------------------------------------------------------
059100101014       BEGSR InzS02;
059200080207
059300080207       // Pulizia subfile
059400080207         SflDsp_N    = *on;
059500080207         SflDspCtl_N = *on;
059600101014         write  TB82C02;
059700080207         SflDspCtl_N = *off;
059800080207         SflEnd      = *off;
059900100824         SflDsp_N    = *off;
060000100824         Sflnxtchg   = *off;
060100080530
060200100315         S02nrr=0 ;
060300100824         clear V1cmsg;
060400080207         errMessage  = *off;
060500080207         errGenerico = *off;
060600101014         Errfil=*off       ;
060700101018         RevFil=*off       ;
060800101018         RevDes=*off       ;
060900101014         ProtLis=*on       ;
061000100708
061100101018         clear filiali ;
061200101018         clear yy   ;
061300100826
061400100823         // Predispongo le letture e carico SFL
061500101014         clear kke1   ;
061600101014         kke1=v1clis   ;
061700101014           setll    ('LDS':kke1)          tntbe01l    ;
061800101014           reade(n) ('LDS':kke1)          tntbe01l    ;
061900100823
062000101014         dow not %eof(tntbe01l)  ;
062100101014
062200100823         exsr CarSF02  ;
062300100823
062400101014           reade(n) ('LDS':kke1)          tntbe01l    ;
062500100823         enddo   ;
062600100910
062700131029         // Carico altre 154 righe vuote
062800101018         RevFil=*off   ;
062900101018         RevDes=*off   ;
063000101014         clear  vscfil  ;
063100101014         clear  vsdfil  ;
063200101014         clear  vscca   ;
063300101014         clear  vscncl  ;
063400101014         clear  vscpkg  ;
063500101018         xx=  1  ;
063600101018
063700101018         select   ;
063800101018         when v1cprop=' '   ;
063900131029         dow xx<=154   ;
064000101014           s02nrr=s02nrr+1   ;
064100101014           write  TB82S02;
064200101014         xx = xx + 1   ;
064300101014         enddo   ;
064400101018
064500101018         C02csr=1  ;
064600101018
064700101018         other              ;
064800101018         // Leggo da AZORG eventuali filiali mancanti e lì mi posiziono
064900101018         exsr LeggiAZORG  ;
065000101018         endsl   ;
065100101008
065200100823
065300100824         ENDSR  ;
065400100823       //--------------------------------------------------------------
065500100823       //?eseguo caricamento SFL
065600100823       //--------------------------------------------------------------
065700100823          BEGSR   carSF02    ;
065800100830
065900101014         dlds=tbeuni    ;
066000101018           RevFil=*off ;
066100101018           Revdes=*off ;
066200101014
066300101018 1        if tbeke2 <>*blanks   ;
066400101014           vscfil=%editc(§ldsfil:'X')   ;
066500101014
066600101014           kfil=%int(vscfil)   ;
066700101014           chain   kfil    azorg01l    ;
066800101018 2         if %found(azorg01l)   ;
066900101014            vsdfil=orgdes  ;
067000101014           else ;
067100101014            vsdfil=*all'?'  ;
067200101018            clear  og150   ;
067300101018 2         endif   ;
067400101014
067500101018           Revfil=*off  ;
067600101018
067700101018           exsr CtrFil    ;
067800101018 3         if WOK='S'                 ;
067900101018           yy=yy+1  ;
068000101018           filiali(yy)=orgfil  ;
068100101018           else  ;
068200101018           RevFil=*on  ;
068300101018           RevDES=*on  ;
068400101018            vsdfil='NON Selezionab '+orgdes  ;
068500101018 3         endif     ;
068600101018
068700101014           vscca =§ldsca  ;
068800101014           vscncl =§ldsncl ;
068900101014           vscpkg =§ldspkg ;
069000100910
069100101014           s02nrr=s02nrr+1   ;
069200100831
069300101014           write  TB82S02;
069400101014          endif  ;
069500100831
069600100830          ENDSR         ;
069700101018       //--------------------------------------------------------------
069800101018       //?Lettura azorg per filiali con picking  e rulliera
069900101018       //--------------------------------------------------------------
070000101018         BEGSR    LeggiAZORG   ;
070100101018         setll  *loval  azorg01l   ;
070200101018         read   azorg01l    ;
070300101018
070400101018 1       dow  not %eof(AZorg01l)  ;
070500101018
070600101018         Indx=%lookup(orgfil:filiali)  ;
070700101018
070800101018 2       if IndX=0    ;
070900101018         exsr Ctrfil   ;
071000101018 3       if wok='S'   ;
071100101104         clear   vscfil  ;
071200101104            VSDFIL=%editc(ORGFIL:'X' ) + '--> DA INSERIRE '  ;
071300101018            RevDes=*on  ;
071400101018             s02nrr=s02nrr+1   ;
071500101018             write  TB82S02;
071600101018             xx = xx + 1   ;
071700101018 3         endif   ;
071800101018 2       endif   ;
071900101018
072000101018         read   azorg01l    ;
072100101018 1       enddo   ;
072200101018
072300101018         C02csr=s02nrr   ;
072400101018
072500101018         RevDes=*off ;
072600101018         clear vscfil   ;
072700101018         clear vsdfil   ;
072800101018         dow xx<=50   ;
072900101018           s02nrr=s02nrr+1   ;
073000101018           write  TB82S02;
073100101018         xx = xx + 1   ;
073200101018         enddo   ;
073300101018
073400101018          ENDSR         ;
073500101018       //--------------------------------------------------------------
073600101018       //?controllo la filiale in base al tipo selezione scelto
073700101018       //--------------------------------------------------------------
073800101018         BEGSR CtrFil   ;
073900101018         clear wok   ;
074000101018
074100101018         // Escludo le annullate sempre
074200101018         if  orgfva<>' '  ;
074300101018         leavesr  ;
074400101018         endif    ;
074500101018         og147=orgde7   ;
074600101018         // Escludo quelle che non hanno la creazione automatica bolle
074700101018         if  §ognoc='N'   ;
074800101018         leavesr  ;
074900101018         endif    ;
075000101018
075100101018         // Escludo se tipi non "F" e "A" tranne la 212 per 1 e T
075200101018         if orgfag<>'F' and orgfag<>'A' ;
075300101018         if (v1cprop='1' or v1cprop='T') and orgfil=212    ;
075400101018         wok='S'   ;
075500101018         endif    ;
075600101018         leavesr  ;
075700101018         endif   ;
075800101018
075900101018         // Nessuna richiesta di filiale --> sempre OK
076000101018         if v1cprop=' ' ;
076100101018         wok='S'   ;
076200101018         else  ;
076300101018
076400101018         og150=orgdf0   ;
076500101018
076600101018         clear fnlv55ds   ;
076700101018         d55tpt='P'    ;
076800101018         d55lin=orgfil   ;
076900101018         d55drf=%dec(datasys)   ;
077000101018         callp FNLV55R  (fnlv55ds)   ;
077100101018
077200101018         select    ;
077300101018         when v1cprop='P'   ;
077400101018 5         if §ogpck='S' and §ogtis='R'  ;
077500101018             wOK='S'   ;
077600101018           endif   ;
077700101018
077800101018         when v1cprop='1'    ;
077900101018         if d55tfp=orgfil   ;
078000101018             wOK='S'   ;
078100101018           endif   ;
078200101018
078300101018         when v1cprop='2'    ;
078400101018         if d55tfp<>orgfil   ;
078500101018             wOK='S'   ;
078600101018           endif   ;
078700101018
078800101018         when v1cprop='T'    ;
078900101018             wOK='S'   ;
079000101018
079100101018         endsl   ;
079200101018         endif  ;
079300101018
079400101018         ENDSR   ;
079500101018       //--------------------------------------------------------------
079600101014       //?Aggiorno tabella con lista
079700101014       //--------------------------------------------------------------
079800101014           BEGSR   Aggiotabel  ;
079900101014
080000101014           clear  Prima  ;
080100101014           xx=1           ;
080200101014           yy=1           ;
080300101014           s02nrr=1  ;
080400101014           chain s02nrr  TB82s02    ;
080500101014
080600101014 1         dow %found   ;
080700101014           // Scrivo la lista se immesso un record di filiale
080800101014 2         if vscfil>*zeros   ;
080900101014
081000101014 3         if Prima =' '   ;
081100101014           clear kke1   ;
081200101014           clear kke2   ;
081300101014           kke1  = v1clis   ;
081400101014           // Cancello tutti i record esistenti
081500101014           setll    ('LDS':kke1)          tntbe01l    ;
081600101014           reade    ('LDS':kke1)          tntbe01l    ;
081700101014 4         dow     not %eof(tntbe01l)   ;
081800101014
081900101014           delete tntbe000   ;
082000101014           reade    ('LDS':kke1)          tntbe01l    ;
082100101014 4         enddo   ;
082200101014
082300101014           // testata
082400101014           clear kke2   ;
082500101014           chain    ('LDS':kke2: kke1)    tntbe01l    ;
082600101014           clear tntbe000   ;
082700101014           tbecod='LDS'   ;
082800101014           tbeke2=kke1    ;
082900101014           tbeuni=v1dlis   ;
083000101018           %subst(tbeuni:41:1)=v1cprop ;
083100131029           %subst(tbeuni:42:1)=v1cldvh ;
083200131029           %subst(tbeuni:43:1)=v1cpost ;
083300101014           if %found(tntbe01l)  ;
083400101014           update tntbe000   ;
083500101014           else   ;
083600101014           Write tntbe000   ;
083700101014           endif  ;
083800101014
083900101014           Prima = 'E'   ;
084000101014 3         endif  ;
084100101014
084200101014
084300101014           clear tntbe000   ;
084400101014           clear dlds   ;
084500101014           tbecod='LDS'   ;
084600101014           tbeke1=kke1    ;
084700101014           tbeke2=%editc(xx:'X')       ;
084800101014           xx=xx+1  ;
084900101014           clear tbeuni ;
085000101014           §ldsfil=%int(vscfil) ;
085100101014           §ldsca=vscca  ;
085200101014           §ldsncl=vscncl ;
085300101014           §ldspkg = vscpkg  ;
085400101014           tbeuni=dlds   ;
085500101014           write tntbe000  ;
085600101014
085700101014           endif   ;
085800101014
085900101014           s02nrr=s02nrr + 1 ;
086000101014           chain s02nrr  TB82s02    ;
086100101014 3         enddo  ;
086200101014
086300101014       ENDSR   ;
086400101014
086500080206       //--------------------------------------------------------------
086600080206       //?Operazioni finali.
086700080206       //--------------------------------------------------------------
086800080206       BEGSR RoutEnd;
086900080627
087000080704         // Chiusura pgm   ;
087100080206         *inLR = *on;
087200080704
087300080206         return;
087400080206
087500080206       ENDSR;
087600080206
087700080206      /end-free
087800080206
087900080206       //--------------------------------------------------------------
088000080206       //?Schiere a tempo di compilazione.
088100080206       //--------------------------------------------------------------
088200080206
088300080410** - MSG ------------------------------------------------------------------ -*
088400101014Nuova lista distribuzione: immettere la descrizione                             01
088500101014Immettere una filiale                                                           02
088600101014Filiale Errata                                                                  03
088700101014scegliere una lista distribuzione                                               04
