000100080206      //--------------------------------------------------------------
000200101014      //?TNTB82R - Liste distribuzione creazione automatica bolle
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100101014     fTNtb82D   cf   e             workstn indds(IndDspF)
001200080206     f                                     infds(InfDspF)
001300101014     f                                     sfile(TB82S02 : S02nrr)
001400100312     FAZORG21l  IF   E           K DISK
001500100312     fazorg01l  if   e           k disk
001600101014     ftntbe01l  uf a e           k disk
001700080206      //---------------------------------------------------------------
001800100824
001900080207      // - Tasti funzionali a video
002000080207     d c_F01           c                   const(x'31')
002100080207     d c_F02           c                   const(x'32')
002200080207     d c_F03           c                   const(x'33')
002300080207     d c_F05           c                   const(x'35')
002400080207     d c_F06           c                   const(x'36')
002500080207     d c_F07           c                   const(x'37')
002600080207     d c_F08           c                   const(x'38')
002700080207     d c_F09           c                   const(x'39')
002800080207     d c_F10           c                   const(x'3A')
002900080207     d c_F12           c                   const(x'3C')
003000080207     d c_F13           c                   const(x'B1')
003100080207     d c_F14           c                   const(x'B2')
003200080207     d c_F15           c                   const(x'B3')
003300080207     d c_F16           c                   const(x'B4')
003400080207     d c_F17           c                   const(x'B5')
003500080207     d c_F18           c                   const(x'B6')
003600080207     d c_F19           c                   const(x'B7')
003700080207     d c_F20           c                   const(x'B8')
003800080207     d c_F21           c                   const(x'B9')
003900080207     d c_F22           c                   const(x'BA')
004000080207     d c_F23           c                   const(x'BB')
004100080207     d c_F24           c                   const(x'BC')
004200080207     d c_Enter         c                   const(x'F1')
004300080207     d c_RollDown      c                   const(x'F4')
004400080207     d c_RollUp        c                   const(x'F5')
004500080214
004600080214      // - Attributi di visualizzazione
004700080215      //  -  DspAtr() - Normale
004800080214     d C_dspatr_Norm   c                   const(x'20')
004900080215      //  -  DspAtr(RI)
005000080214     d C_dspatr_RI     c                   const(x'21')
005100080215      //  -  DspAtr(ND)
005200080214     d C_dspatr_ND     c                   const(x'27')
005300080215      //  -  DspAtr(BL) / Color(Red)
005400080214     d C_dspatr_BL     c                   const(x'28')
005500080206
005600080206      //---------------------------------------------------------------
005700080206      //?Definizione schiere.
005800080206      //---------------------------------------------------------------
005900080206      // - Messaggi di errore
006000100910     d MSG             s             78    dim(10) ctdata perrcd(1)
006100101018     d filiali         s              3  0 dim(150)
006200080206      //?Definizione aree dati.
006300080206      //---------------------------------------------------------------
006400080206      // - Dati utente
006500080206     d §AzUte        e ds                  extname(AZUTE00F)
006600080206     d                                     dtaara
006700080206     d §DatiUte      e ds                  extname(dDatiUte)
006800080206     d                                     dtaara
006900080206      //---------------------------------------------------------------
007000080206      //?Definizione strutture dati.
007100080206      //---------------------------------------------------------------
007200080206      // - Status
007300080206     d Psds           sds
007400080206     d   SDSpgm          *proc
007500080206
007600080206      // - InfDS
007700080206     d InfDspF         ds
007800080207     d  dsp_aid              369    369a                                        AID byte
007900080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
008000080207     d  min_nrr              378    379i 0                                      Subfile min rrn
008100080207     d  num_rcds             380    381i 0                                      Subfile num rcds
008200080206
008300080206      // - Indicatori su DspF
008400080208     d IndDspF         ds
008500080206        // - Indicatori di gestione del subfile
008600080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008700080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
008800080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
008900080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009000080206        // - Indicatori di errore
009100080206     d  errMessage                    1n   overlay(IndDspF : 28)
009200080606     d  errGenerico                   1n   overlay(IndDspF : 99)
009300080627        // - Indicatori vari
009400101014     d  ProtLis                       1n   overlay(IndDspF : 10)
009500101018     d  RevFil                        1n   overlay(IndDspF : 11)
009600101018     d  RevDes                        1n   overlay(IndDspF : 12)
009700101014     d  Errfil                        1n   overlay(IndDspF : 40)
009800101014     d  ErrLIS                        1n   overlay(IndDspF : 41)
009900101014     d  Errclis                       1n   overlay(IndDspF : 43)
010000080206
010100080206      // - Parametri ricevuti
010200080206     d KPJBA         e ds
010300080206
010400080206      // - Reperimento dati utente
010500080206     d TIBS34ds      e ds
010600080206     d dUte01        e ds
010700080702     d DLAT          e ds
010800101014      // - Ricerca/Controllo tabelle
010900101014     d TIBS02ds      e ds                  inz
011000101018     d
011100101018     d Tntb82ds      e ds
011200101018     d fnlv55ds      e ds
011300101014     d
011400101014     d DLDS          e ds
011500101018     d og147         e ds
011600101018     d og148         e ds
011700101018     d og150         e ds
011800080206
011900100316     d wlbdat          ds                  inz
012000100316     d  g02dat                 1      8  0
012100100316     d  g02inv                 9     16  0
012200100316     d  g02err                17     17
012300100316     d  g02tgi                18     22  0
012400101018
012500101018     d Datasys         s               d   inz(*sys) datfmt(*iso)
012600101018     d  WOK            s              1
012700100316
012800080206      //---------------------------------------------------------------
012900080206      //?Definizione variabili globali.
013000080206      //---------------------------------------------------------------
013100080206
013200080206      // - Flags booleani
013300080208     d $Fine           s               n   inz(*off)
013400100312     d $InzS02         s               n   inz(*on)
013500100312     d $Inzd01         s               n   inz(*on)
013600080206
013700100312     d $Video          s              2    inz('D1')
013800100315     d S02nrr          s              4  0 inz
013900081006     d XX              s              3  0 inz
014000101014     d YY              s              3  0 inz
014100080627     d Indx            s              3  0 inz
014200101014     d Prima           s              1    inz
014300101014     d kke1            s                   like(tbeke1)
014400101014     d kke2            s                   like(tbeke2)
014500101014     d kfil            s                   like(orgfil)
014600080414
014700100910     D*----------------
014800101014     D* PARAMETRI TNSD24R
014900100910     D*----------------
015000101014     d   pInFIL        s              3
015100101014     d   pInFAG        s              1
015200101014     d   pInDES        s             25
015300101014     d   pInDIT        s              3
015400101014
015500101014     d Digits          c                   const('0123456789')
015600080206      //---------------------------------------------------------------
015700080206      //?Definizione procedure usate.
015800080206      //---------------------------------------------------------------
015900080414      /copy gaitrasrc/srcprotopr,tibs34r
016000100316      /copy gaitrasrc/srcprotopr,xsrda8
016100100910      /copy gaitrasrc/srcprotopr,bch10
016200101014      /copy gaitrasrc/srcprotopr,tibs02r
016300101014      /copy gaitrasrc/srcprotopr,tnsd24r
016400101018      /copy gaitrasrc/srcprotopr,fnlv55r
016500080206
016600080206      //---------------------------------------------------------------
016700080206      //?Riepilogo indicatori.
016800080206      //---------------------------------------------------------------
016900080207      // - Comuni
017000080207      // 28    : Emissione messaggio di errore a video
017100080207      // - C01/S01
017200080207      // 30    : Condiziona SFLDSP    (*not)
017300080207      // 31    : Condiziona SFLDSPCTL (*not)
017400080207      // 30+31 : Condiziona SFLCLR
017500100824      // 32    : Condiziona SFLNXTCHG
017600080207      // 50    : Errore: Opzione errata
017700080207      // - D01
017800080207      // - Comuni
017900080207      // 99    : Generico di Errore
018000080206      //---------------------------------------------------------------
018100080206
018200080206      //---------------------------------------------------------------
018300080206      //?M A I N - L I N E
018400080206      //---------------------------------------------------------------
018500080627     c     *Entry        plist
018600080206     c                   parm                    KPJBA
018700101018     c                   movel     kpjbu         tntb82ds
018800101018     c                   clear                   §tb82crea
018900080702
019000080206      /free
019100080206
019200080206       // Operazioni iniziali
019300080206       exsr RoutInz;
019400081215
019500080206       // Gestione video
019600081215 3     DOW $Fine = *off;
019700081215 4       select;
019800080530
019900080530         // Videata di selezioni
020000100312           when $Video = 'D1';
020100100312             exsr GesD01;
020200080530
020300100830         // Videata del sfl per inserimento/visualizzazione
020400100312           when $Video = 'S2';
020500100312             exsr GesS02;
020600100830
020700080702           other   ;
020800080206             $Fine = *on;
020900081215 4       endsl;
021000081215 3     ENDDO;
021100080206
021200080206       // Operazioni finali
021300080206       exsr RoutEnd;
021400080206
021500080206       //--------------------------------------------------------------
021600080206       //?Operazioni iniziali.
021700080206       //--------------------------------------------------------------
021800080206       BEGSR RoutInz;
021900100312       $inzs02=*on;
022000100312       $inzd01=*on;
022100080703
022200080206         // Reperimento dati job
022300080206         exsr DatiJob;
022400100827
022500080627         ENDSR;
022600080206
022700080206       //--------------------------------------------------------------
022800080206       //?Reperimento Dati del job (Utente/Operativi).
022900080206       //--------------------------------------------------------------
023000080206       BEGSR DatiJob;
023100080206
023200080206         in(E) §AzUte;
023300080206         if NOT %error;
023400080206           in(E) §DatiUte;
023500080206         endif;
023600080206         if %error or RSut = *blanks;
023700080206           clear TIBS34ds;
023800080206           tibs34r(tibs34ds);
023900080206           in §AzUte;
024000080206           in §DatiUte;
024100080206         endif;
024200080206
024300080206       ENDSR;
024400100312       //--------------------------------------------------------------
024500100312       //?Gestione viodeata 01
024600100312       //--------------------------------------------------------------
024700100312       BEGSR Gesd01;
024800100312
024900100312         // Inizializzazione videata
025000100312         if  $Inzd01 = *on;
025100100312            exsr Inzd01;
025200100312            $Inzd01  = *off;
025300100312         endif;
025400101018
025500101018         // Se pgm richiamato passo direttamente al  SFL
025600101018         if §tb82lis<>*blanks  ;
025700101018         leavesr   ;
025800101018         endif  ;
025900100312
026000100312         // Emissione Testata e Piede con tasti funzionali abilitati
026100100824         if  errMessage  = *off;
026200101014           write  TB82T01;
026300101014           write  TB82Z01;
026400100312         endif;
026500100312
026600100312         // Emissione videata
026700101014         exfmt  TB82D01;
026800100312
026900100312         reset errMessage;
027000100312         reset errGenerico;
027100100824         clear V1cmsg    ;
027200101014         clear Errlis    ;
027300101014         clear ErrClis    ;
027400100312
027500100312 1       SELECT;
027600100312
027700100312         // - F3=Fine
027800100312 1         WHEN  dsp_aid = c_F03;
027900100312            $Fine = *on;
028000100312
028100100312
028200100824 x1      // Invio
028300100312           OTHER;
028400100316
028500100316           exsr CTRD01  ;
028600100316
028700100316           if ErrGenerico=*ON    ;
028800100316                 leavesr;
028900100316 2             endif;
029000100316
029100100315           $video='S2' ;
029200100825           $inzs02=*on;
029300100824
029400100312 1       ENDSL;
029500100312
029600100312       ENDSR;
029700100312
029800100316       //--------------------------------------------------------------
029900100316       //?controlli video 1
030000100316       //--------------------------------------------------------------
030100100316       BEGSR CTRD01  ;
030200100316
030300101014       //  se messo '?' visualizzo tabella
030400101014       Indx=%scan('?':v1clis)   ;
030500101014 1     if Indx>0   ;
030600101014             clear  TIBS02ds;
030700101014             T02mod = 'R';
030800101014             T02cod = 'LDS';
030900101014             T02sif = KNSIF;
031000101014             TNTBE_RicercaControllo  (kpjba : tibs02ds);
031100101014 2           if  T02err = *blanks and t02ke1=*blanks ;
031200101014               V1clis = T02ke2;
031300101014               V1dlis = T02uni;
031400101018               v1cprop=%subst(t02uni:41:1) ;
031500101014               else ;
031600101014               clear v1clis   ;
031700101014 2             endif   ;
031800101014               errGenerico = *on;
031900101014               leavesr;
032000101014 1     endif       ;
032100101014
032200101014        // Controllo: se trovata, prrendo desrizione altrimenti la chiedo
032300101014             clear  TIBS02ds;
032400101014             T02mod = 'C';
032500101014             T02sif = knsif;
032600101014             T02cod = 'LDS';
032700101014             T02ke2 = V1clis;
032800101014             TNTBE_RicercaControllo  (kpjba : tibs02ds);
032900101018 1           if T02err = *blanks;
033000101014               v1dlis  = T02uni;
033100101018               if v1cprop=' '  ;
033200101018                v1cprop=%subst(t02uni:41:1) ;
033300101018               endif  ;
033400101018 x1          else;
033500101014
033600101014             // non trovato: immettere descrizione
033700101018 2           if v1clis=*blanks   ;
033800101014               errMessage  = *on;
033900101014               errGenerico = *on;
034000101014               ErrClis      = *on;
034100101014               V1cmsg = Msg(04);
034200101014               leavesr;
034300101018 2           endif;
034400101018 2           if v1dlis=*blanks   ;
034500101018 3           if §tb82lis<>*blanks and §tb82dlis<>*blanks  ;
034600101018             v1dlis=§tb82dlis  ;
034700101018 x3          else ;
034800101014               errMessage  = *on;
034900101014               errGenerico = *on;
035000101014               Errlis      = *on;
035100101014               V1cmsg = Msg(01);
035200101014               leavesr;
035300101018 3           endif;
035400101018
035500101018 2           endif;
035600101018 1           endif;
035700100708
035800100316       ENDSR  ;
035900080206       //--------------------------------------------------------------
036000100312       //?Gestione SFL 02
036100080206       //--------------------------------------------------------------
036200100312       BEGSR GesS02;
036300080207
036400080207         // Inizializzazione videata
036500100312         if  $InzS02 = *on;
036600100312            exsr InzS02;
036700100824            $InzS02  = *off;
036800100824         endif;
036900100831
037000100831         // Posizionamento cursore
037100100910         if  C02csr  >0     ;
037200100827           C02rcd = C02csr;
037300100827         endif  ;
037400080207
037500080207         // Emissione Testata e Piede con tasti funzionali abilitati
037600100824         if  errMessage  = *off;
037700101014           write  TB82T01;
037800101014           write  TB82Z02;
037900101014           write  TB82d01;
038000080207         endif;
038100080703
038200080207         // Emissione videata
038300101014         exfmt  TB82C02;
038400080410
038500080207         reset errMessage;
038600080207         reset errGenerico;
038700100824         clear V1cmsg;
038800101014         errfil=*off ;
038900080207
039000080523 1       SELECT;
039100080207
039200080207         // - F3=Fine
039300080523 1         WHEN  dsp_aid = c_F03;
039400080409            $Fine = *on;
039500080207
039600080207         // - F12=Ritorno
039700080523 1         WHEN  dsp_aid = c_F12;
039800101018           if §tb82lis<>*blanks    ;
039900101018            $Fine = *on;
040000101018            else  ;
040100100315           $video='D1'   ;
040200101014           Protlis =*off   ;
040300101014           $inzd01=*on;
040400101018           endif  ;
040500101014
040600101014         // F16= Annullamento
040700101014 1         when dsp_aid = c_F16  ;
040800101014           clear kke1   ;
040900101014           clear kke2   ;
041000101014           kke1  = v1clis   ;
041100101014           // Cancello tutti i record esistenti
041200101014           setll    ('LDS':kke1)          tntbe01l    ;
041300101014           reade    ('LDS':kke1)          tntbe01l    ;
041400101014 2         dow     not %eof(tntbe01l)   ;
041500101014
041600101014           delete tntbe000   ;
041700101014           reade    ('LDS':kke1)          tntbe01l    ;
041800101014 2         enddo   ;
041900101014
042000101014           // testata
042100101014           clear kke2   ;
042200101014           delete   ('LDS':kke2: kke1)          tntbe01l    ;
042300101014
042400101014           $video='D1' ;
042500101014           $inzd01=*on;
042600101014           Protlis =*off   ;
042700100901
042800080530 x1      // Invio / F6
042900080207           OTHER;
043000081006
043100100315               exsr ContrS02 ;
043200100910 2             if  errGenerico = *on ;
043300081006                 leavesr;
043400100824 2             endif;
043500080530
043600100907         // F6=conferma Aggiornamento
043700101014 2         if   dsp_aid = c_F06  ;
043800101014           Exsr Aggiotabel   ;
043900101018           if §tb82lis<>*blanks    ;
044000101018           §tb82crea='S'  ;
044100101018           kpjbu=tntb82ds   ;
044200101018            $Fine = *on;
044300101018           else  ;
044400101018           $video='D1'   ;
044500101018           Protlis =*off   ;
044600101018           $inzd01=*on;
044700101018           endif   ;
044800100915
044900101014 2         endif   ;
045000080207
045100100910 1       ENDSL;
045200080207
045300080207       ENDSR;
045400080526       //--------------------------------------------------------------
045500100312       //?controlli SFL02
045600080409       //--------------------------------------------------------------
045700100312       BEGSR ContrS02;
045800100315
045900101014       s02nrr=1  ;
046000101014       chain    s02nrr   TB82s02   ;
046100101014
0462001010140      dow %found     ;
046300101014
046400101014       clear   vsdfil   ;
046500101014       // '?' sulla filiale
046600101014       Indx= %scan('?':vscfil)  ;
0467001010141      if Indx>0   ;
046800101014       clear pinfil   ;
046900101014       clear pinfag   ;
047000101014       clear pindes  ;
047100101014       clear pindit   ;
047200101014        TNSD24R (pinfil:pinfag:pindes:pindit)  ;
0473001010142       if pinfil > *zeros  ;
047400101014        vscfil=pinfil  ;
047500101014        vsdfil=pindes  ;
047600101014        else ;
047700101014        clear vscfil   ;
047800101014        clear vsdfil   ;
0479001010142       endif   ;
048000101014
048100101014        Errgenerico=*on  ;
048200101014        Exsr Aggiosfl   ;
048300101014        leavesr  ;
0484001010141       endif   ;
048500101014
048600101014       // controllo la filiale  ;
0487001010141      if  vscfil<=*zeros and  (vscca<>*blanks or vscncl>0 or vscpkg>0)  ;
048800101014         ErrGenerico=*on  ;
048900101014         ErrMessage =*on  ;
049000101014         ErrFIL= *on   ;
049100101014         v1cmsg=msg(02)  ;
049200101014         EXSR AggioSFL   ;
049300101014         leavesr  ;
0494001010141      endif   ;
049500101014
0496001010141        if vscfil<>*blanks and vscfil<>*zeros  ;
049700101014
0498001010142        IF  %check(digits:vscfil) > 0;
049900101014           ErrMessage  = *on;
050000101014           ErrGenerico = *on;
050100101014           ErrFIL      = *on;
050200101014           V1cmsg      = Msg(03);
050300101014           EXSR AggioSFL   ;
050400101014           leavesr;
0505001010142        ENDIF;
050600101014
050700101014       kfil=%int(vscfil)  ;
050800101014
050900101014       clear og148          ;
051000101014       clear orgfag         ;
051100101014       chain   kfil    azorg01l   ;
0512001010142      if %found(azorg01l)  ;
051300101014       og148= orgde8   ;
0514001010142      endif   ;
051500101014
0516001010142      if  orgfag<>'A' and orgfag<>'F'  and   §oglue <>'S'  ;
051700101014           ErrMessage  = *on;
051800101014           ErrGenerico = *on;
051900101014           ErrFIL      = *on;
052000101014           V1cmsg      = Msg(03);
052100101014           EXSR AggioSFL   ;
052200101014           leavesr;
0523001010142      endif   ;
052400101014
052500101014       vsdfil=orgdes   ;
0526001010141      endif   ;
052700101014
052800101014       exsr  Aggiosfl   ;
052900101014
053000101014       s02nrr= s02nrr + 1 ;
053100101014       chain    s02nrr   TB82s02   ;
053200101014       enddo  ;
053300100830
053400100830       ENDSR  ;
053500081126
053600080703       //--------------------------------------------------------------
053700080703       //?Aggiornamento sfl
053800080703       //--------------------------------------------------------------
053900080703       BEGSR AGGIOSFL  ;
054000100830
054100100824       if v1cmsg<>*blanks   ;
054200080703       errMessage  = *on;
054300080703       errGenerico = *on;
054400100315       c02csr=s02nrr    ;
054500080703       endif    ;
054600100824
054700101014       update  TB82s02  ;
054800100830
054900080703       ENDSR   ;
055000100312       //--------------------------------------------------------------
055100100312       //?Inizializzazione videata 01
055200100312       //--------------------------------------------------------------
055300100312       BEGSR Inzd01  ;
055400100312
055500100315         clear V1cmsg;
055600100826         errMessage  = *off;
055700100312         errGenerico = *off;
055800101014         Errlis=*off       ;
055900101014         ErrClis=*off       ;
056000101014         Errfil=*off       ;
056100101014         v1clis= '?       '   ;
056200101018         clear v1cprop   ;
056300101014         clear v1dlis    ;
056400101018
056500101018         // se passati i dati li imposto
056600101018         if §tb82lis<>*blanks   ;
056700101018         v1clis=§tb82lis   ;
056800101018         v1cprop=§tb82tipol   ;
056900101018
057000101018         exsr ctrd01   ;
057100101018
057200101018           $video='S2' ;
057300101018           $inzs02=*on;
057400101018         endif    ;
057500100831
057600100312         ENDSR    ;
057700080207       //--------------------------------------------------------------
057800100312       //?Inizializzazione SFL02
057900080207       //--------------------------------------------------------------
058000101014       BEGSR InzS02;
058100080207
058200080207       // Pulizia subfile
058300080207         SflDsp_N    = *on;
058400080207         SflDspCtl_N = *on;
058500101014         write  TB82C02;
058600080207         SflDspCtl_N = *off;
058700080207         SflEnd      = *off;
058800100824         SflDsp_N    = *off;
058900100824         Sflnxtchg   = *off;
059000080530
059100100315         S02nrr=0 ;
059200100824         clear V1cmsg;
059300080207         errMessage  = *off;
059400080207         errGenerico = *off;
059500101014         Errfil=*off       ;
059600101018         RevFil=*off       ;
059700101018         RevDes=*off       ;
059800101014         ProtLis=*on       ;
059900100708
060000101018         clear filiali ;
060100101018         clear yy   ;
060200100826
060300100823         // Predispongo le letture e carico SFL
060400101014         clear kke1   ;
060500101014         kke1=v1clis   ;
060600101014           setll    ('LDS':kke1)          tntbe01l    ;
060700101014           reade(n) ('LDS':kke1)          tntbe01l    ;
060800100823
060900101014         dow not %eof(tntbe01l)  ;
061000101014
061100100823         exsr CarSF02  ;
061200100823
061300101014           reade(n) ('LDS':kke1)          tntbe01l    ;
061400100823         enddo   ;
061500100910
061600101014         // Carico altre 150 righe vuote
061700101018         RevFil=*off   ;
061800101018         RevDes=*off   ;
061900101014         clear  vscfil  ;
062000101014         clear  vsdfil  ;
062100101014         clear  vscca   ;
062200101014         clear  vscncl  ;
062300101014         clear  vscpkg  ;
062400101018         xx=  1  ;
062500101018
062600101018         select   ;
062700101018         when v1cprop=' '   ;
062800101018         dow xx<=150   ;
062900101014           s02nrr=s02nrr+1   ;
063000101014           write  TB82S02;
063100101014         xx = xx + 1   ;
063200101014         enddo   ;
063300101018
063400101018         C02csr=1  ;
063500101018
063600101018         other              ;
063700101018         // Leggo da AZORG eventuali filiali mancanti e lì mi posiziono
063800101018         exsr LeggiAZORG  ;
063900101018         endsl   ;
064000101008
064100100823
064200100824         ENDSR  ;
064300100823       //--------------------------------------------------------------
064400100823       //?eseguo caricamento SFL
064500100823       //--------------------------------------------------------------
064600100823          BEGSR   carSF02    ;
064700100830
064800101014         dlds=tbeuni    ;
064900101018           RevFil=*off ;
065000101018           Revdes=*off ;
065100101014
065200101018 1        if tbeke2 <>*blanks   ;
065300101014           vscfil=%editc(§ldsfil:'X')   ;
065400101014
065500101014           kfil=%int(vscfil)   ;
065600101014           chain   kfil    azorg01l    ;
065700101018 2         if %found(azorg01l)   ;
065800101014            vsdfil=orgdes  ;
065900101014           else ;
066000101014            vsdfil=*all'?'  ;
066100101018            clear  og150   ;
066200101018 2         endif   ;
066300101014
066400101018           Revfil=*off  ;
066500101018
066600101018           exsr CtrFil    ;
066700101018 3         if WOK='S'                 ;
066800101018           yy=yy+1  ;
066900101018           filiali(yy)=orgfil  ;
067000101018           else  ;
067100101018           RevFil=*on  ;
067200101018           RevDES=*on  ;
067300101018            vsdfil='NON Selezionab '+orgdes  ;
067400101018 3         endif     ;
067500101018
067600101014           vscca =§ldsca  ;
067700101014           vscncl =§ldsncl ;
067800101014           vscpkg =§ldspkg ;
067900100910
068000101014           s02nrr=s02nrr+1   ;
068100100831
068200101014           write  TB82S02;
068300101014          endif  ;
068400100831
068500100830          ENDSR         ;
068600101018       //--------------------------------------------------------------
068700101018       //?Lettura azorg per filiali con picking  e rulliera
068800101018       //--------------------------------------------------------------
068900101018         BEGSR    LeggiAZORG   ;
069000101018         setll  *loval  azorg01l   ;
069100101018         read   azorg01l    ;
069200101018
069300101018 1       dow  not %eof(AZorg01l)  ;
069400101018
069500101018         Indx=%lookup(orgfil:filiali)  ;
069600101018
069700101018 2       if IndX=0    ;
069800101018         exsr Ctrfil   ;
069900101018 3       if wok='S'   ;
070000101104         clear   vscfil  ;
070100101104            VSDFIL=%editc(ORGFIL:'X' ) + '--> DA INSERIRE '  ;
070200101018            RevDes=*on  ;
070300101018             s02nrr=s02nrr+1   ;
070400101018             write  TB82S02;
070500101018             xx = xx + 1   ;
070600101018 3         endif   ;
070700101018 2       endif   ;
070800101018
070900101018         read   azorg01l    ;
071000101018 1       enddo   ;
071100101018
071200101018         C02csr=s02nrr   ;
071300101018
071400101018         RevDes=*off ;
071500101018         clear vscfil   ;
071600101018         clear vsdfil   ;
071700101018         dow xx<=50   ;
071800101018           s02nrr=s02nrr+1   ;
071900101018           write  TB82S02;
072000101018         xx = xx + 1   ;
072100101018         enddo   ;
072200101018
072300101018          ENDSR         ;
072400101018       //--------------------------------------------------------------
072500101018       //?controllo la filiale in base al tipo selezione scelto
072600101018       //--------------------------------------------------------------
072700101018         BEGSR CtrFil   ;
072800101018         clear wok   ;
072900101018
073000101018         // Escludo le annullate sempre
073100101018         if  orgfva<>' '  ;
073200101018         leavesr  ;
073300101018         endif    ;
073400101018         og147=orgde7   ;
073500101018         // Escludo quelle che non hanno la creazione automatica bolle
073600101018         if  §ognoc='N'   ;
073700101018         leavesr  ;
073800101018         endif    ;
073900101018
074000101018         // Escludo se tipi non "F" e "A" tranne la 212 per 1 e T
074100101018         if orgfag<>'F' and orgfag<>'A' ;
074200101018         if (v1cprop='1' or v1cprop='T') and orgfil=212    ;
074300101018         wok='S'   ;
074400101018         endif    ;
074500101018         leavesr  ;
074600101018         endif   ;
074700101018
074800101018         // Nessuna richiesta di filiale --> sempre OK
074900101018         if v1cprop=' ' ;
075000101018         wok='S'   ;
075100101018         else  ;
075200101018
075300101018         og150=orgdf0   ;
075400101018
075500101018         clear fnlv55ds   ;
075600101018         d55tpt='P'    ;
075700101018         d55lin=orgfil   ;
075800101018         d55drf=%dec(datasys)   ;
075900101018         callp FNLV55R  (fnlv55ds)   ;
076000101018
076100101018         select    ;
076200101018         when v1cprop='P'   ;
076300101018 5         if §ogpck='S' and §ogtis='R'  ;
076400101018             wOK='S'   ;
076500101018           endif   ;
076600101018
076700101018         when v1cprop='1'    ;
076800101018         if d55tfp=orgfil   ;
076900101018             wOK='S'   ;
077000101018           endif   ;
077100101018
077200101018         when v1cprop='2'    ;
077300101018         if d55tfp<>orgfil   ;
077400101018             wOK='S'   ;
077500101018           endif   ;
077600101018
077700101018         when v1cprop='T'    ;
077800101018             wOK='S'   ;
077900101018
078000101018         endsl   ;
078100101018         endif  ;
078200101018
078300101018         ENDSR   ;
078400101018       //--------------------------------------------------------------
078500101014       //?Aggiorno tabella con lista
078600101014       //--------------------------------------------------------------
078700101014           BEGSR   Aggiotabel  ;
078800101014
078900101014           clear  Prima  ;
079000101014           xx=1           ;
079100101014           yy=1           ;
079200101014           s02nrr=1  ;
079300101014           chain s02nrr  TB82s02    ;
079400101014
079500101014 1         dow %found   ;
079600101014           // Scrivo la lista se immesso un record di filiale
079700101014 2         if vscfil>*zeros   ;
079800101014
079900101014 3         if Prima =' '   ;
080000101014           clear kke1   ;
080100101014           clear kke2   ;
080200101014           kke1  = v1clis   ;
080300101014           // Cancello tutti i record esistenti
080400101014           setll    ('LDS':kke1)          tntbe01l    ;
080500101014           reade    ('LDS':kke1)          tntbe01l    ;
080600101014 4         dow     not %eof(tntbe01l)   ;
080700101014
080800101014           delete tntbe000   ;
080900101014           reade    ('LDS':kke1)          tntbe01l    ;
081000101014 4         enddo   ;
081100101014
081200101014           // testata
081300101014           clear kke2   ;
081400101014           chain    ('LDS':kke2: kke1)    tntbe01l    ;
081500101014           clear tntbe000   ;
081600101014           tbecod='LDS'   ;
081700101014           tbeke2=kke1    ;
081800101014           tbeuni=v1dlis   ;
081900101018           %subst(tbeuni:41:1)=v1cprop ;
082000101014           if %found(tntbe01l)  ;
082100101014           update tntbe000   ;
082200101014           else   ;
082300101014           Write tntbe000   ;
082400101014           endif  ;
082500101014
082600101014           Prima = 'E'   ;
082700101014 3         endif  ;
082800101014
082900101014
083000101014           clear tntbe000   ;
083100101014           clear dlds   ;
083200101014           tbecod='LDS'   ;
083300101014           tbeke1=kke1    ;
083400101014           tbeke2=%editc(xx:'X')       ;
083500101014           xx=xx+1  ;
083600101014           clear tbeuni ;
083700101014           §ldsfil=%int(vscfil) ;
083800101014           §ldsca=vscca  ;
083900101014           §ldsncl=vscncl ;
084000101014           §ldspkg = vscpkg  ;
084100101014           tbeuni=dlds   ;
084200101014           write tntbe000  ;
084300101014
084400101014           endif   ;
084500101014
084600101014           s02nrr=s02nrr + 1 ;
084700101014           chain s02nrr  TB82s02    ;
084800101014 3         enddo  ;
084900101014
085000101014       ENDSR   ;
085100101014
085200080206       //--------------------------------------------------------------
085300080206       //?Operazioni finali.
085400080206       //--------------------------------------------------------------
085500080206       BEGSR RoutEnd;
085600080627
085700080704         // Chiusura pgm   ;
085800080206         *inLR = *on;
085900080704
086000080206         return;
086100080206
086200080206       ENDSR;
086300080206
086400080206      /end-free
086500080206
086600080206       //--------------------------------------------------------------
086700080206       //?Schiere a tempo di compilazione.
086800080206       //--------------------------------------------------------------
086900080206
087000080410** - MSG ------------------------------------------------------------------ -*
087100101014Nuova lista distribuzione: immettere la descrizione                             01
087200101014Immettere una filiale                                                           02
087300101014Filiale Errata                                                                  03
087400101014scegliere una lista distribuzione                                               04
