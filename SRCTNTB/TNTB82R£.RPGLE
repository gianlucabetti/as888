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
006200101018     d filiali         s              3  0 dim(150)
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
031600101014               else ;
031700101014               clear v1clis   ;
031800101014 2             endif   ;
031900101014               errGenerico = *on;
032000101014               leavesr;
032100101014 1     endif       ;
032200101014
032300101014        // Controllo: se trovata, prrendo desrizione altrimenti la chiedo
032400101014             clear  TIBS02ds;
032500101014             T02mod = 'C';
032600101014             T02sif = knsif;
032700101014             T02cod = 'LDS';
032800101014             T02ke2 = V1clis;
032900101014             TNTBE_RicercaControllo  (kpjba : tibs02ds);
033000101018 1           if T02err = *blanks;
033100101014               v1dlis  = T02uni;
033200101018               if v1cprop=' '  ;
033300101018                v1cprop=%subst(t02uni:41:1) ;
033400101018               endif  ;
033500101018 x1          else;
033600101014
033700101014             // non trovato: immettere descrizione
033800101018 2           if v1clis=*blanks   ;
033900101014               errMessage  = *on;
034000101014               errGenerico = *on;
034100101014               ErrClis      = *on;
034200101014               V1cmsg = Msg(04);
034300101014               leavesr;
034400101018 2           endif;
034500101018 2           if v1dlis=*blanks   ;
034600101018 3           if §tb82lis<>*blanks and §tb82dlis<>*blanks  ;
034700101018             v1dlis=§tb82dlis  ;
034800101018 x3          else ;
034900101014               errMessage  = *on;
035000101014               errGenerico = *on;
035100101014               Errlis      = *on;
035200101014               V1cmsg = Msg(01);
035300101014               leavesr;
035400101018 3           endif;
035500101018
035600101018 2           endif;
035700101018 1           endif;
035800100708
035900100316       ENDSR  ;
036000080206       //--------------------------------------------------------------
036100100312       //?Gestione SFL 02
036200080206       //--------------------------------------------------------------
036300100312       BEGSR GesS02;
036400080207
036500080207         // Inizializzazione videata
036600100312         if  $InzS02 = *on;
036700100312            exsr InzS02;
036800100824            $InzS02  = *off;
036900100824         endif;
037000100831
037100100831         // Posizionamento cursore
037200100910         if  C02csr  >0     ;
037300100827           C02rcd = C02csr;
037400100827         endif  ;
037500080207
037600080207         // Emissione Testata e Piede con tasti funzionali abilitati
037700100824         if  errMessage  = *off;
037800101014           write  TB82T01;
037900101014           write  TB82Z02;
038000101014           write  TB82d01;
038100080207         endif;
038200080703
038300080207         // Emissione videata
038400101014         exfmt  TB82C02;
038500080410
038600080207         reset errMessage;
038700080207         reset errGenerico;
038800100824         clear V1cmsg;
038900101014         errfil=*off ;
039000080207
039100080523 1       SELECT;
039200080207
039300080207         // - F3=Fine
039400080523 1         WHEN  dsp_aid = c_F03;
039500080409            $Fine = *on;
039600080207
039700080207         // - F12=Ritorno
039800080523 1         WHEN  dsp_aid = c_F12;
039900101018           if §tb82lis<>*blanks    ;
040000101018            $Fine = *on;
040100101018            else  ;
040200100315           $video='D1'   ;
040300101014           Protlis =*off   ;
040400101014           $inzd01=*on;
040500101018           endif  ;
040600101014
040700101014         // F16= Annullamento
040800101014 1         when dsp_aid = c_F16  ;
040900101014           clear kke1   ;
041000101014           clear kke2   ;
041100101014           kke1  = v1clis   ;
041200101014           // Cancello tutti i record esistenti
041300101014           setll    ('LDS':kke1)          tntbe01l    ;
041400101014           reade    ('LDS':kke1)          tntbe01l    ;
041500101014 2         dow     not %eof(tntbe01l)   ;
041600101014
041700101014           delete tntbe000   ;
041800101014           reade    ('LDS':kke1)          tntbe01l    ;
041900101014 2         enddo   ;
042000101014
042100101014           // testata
042200101014           clear kke2   ;
042300101014           delete   ('LDS':kke2: kke1)          tntbe01l    ;
042400101014
042500101014           $video='D1' ;
042600101014           $inzd01=*on;
042700101014           Protlis =*off   ;
042800100901
042900080530 x1      // Invio / F6
043000080207           OTHER;
043100081006
043200100315               exsr ContrS02 ;
043300100910 2             if  errGenerico = *on ;
043400081006                 leavesr;
043500100824 2             endif;
043600080530
043700100907         // F6=conferma Aggiornamento
043800101014 2         if   dsp_aid = c_F06  ;
043900101014           Exsr Aggiotabel   ;
044000101018           if §tb82lis<>*blanks    ;
044100101018           §tb82crea='S'  ;
044200101018           kpjbu=tntb82ds   ;
044300101018            $Fine = *on;
044400101018           else  ;
044500101018           $video='D1'   ;
044600101018           Protlis =*off   ;
044700101018           $inzd01=*on;
044800101018           endif   ;
044900100915
045000101014 2         endif   ;
045100080207
045200100910 1       ENDSL;
045300080207
045400080207       ENDSR;
045500080526       //--------------------------------------------------------------
045600100312       //?controlli SFL02
045700080409       //--------------------------------------------------------------
045800100312       BEGSR ContrS02;
045900100315
046000101014       s02nrr=1  ;
046100101014       chain    s02nrr   TB82s02   ;
046200101014
0463001010140      dow %found     ;
046400101014
046500101014       clear   vsdfil   ;
046600101014       // '?' sulla filiale
046700101014       Indx= %scan('?':vscfil)  ;
0468001010141      if Indx>0   ;
046900101014       clear pinfil   ;
047000101014       clear pinfag   ;
047100101014       clear pindes  ;
047200101014       clear pindit   ;
047300101014        TNSD24R (pinfil:pinfag:pindes:pindit)  ;
0474001010142       if pinfil > *zeros  ;
047500101014        vscfil=pinfil  ;
047600101014        vsdfil=pindes  ;
047700101014        else ;
047800101014        clear vscfil   ;
047900101014        clear vsdfil   ;
0480001010142       endif   ;
048100101014
048200101014        Errgenerico=*on  ;
048300101014        Exsr Aggiosfl   ;
048400101014        leavesr  ;
0485001010141       endif   ;
048600101014
048700101014       // controllo la filiale  ;
0488001010141      if  vscfil<=*zeros and  (vscca<>*blanks or vscncl>0 or vscpkg>0)  ;
048900101014         ErrGenerico=*on  ;
049000101014         ErrMessage =*on  ;
049100101014         ErrFIL= *on   ;
049200101014         v1cmsg=msg(02)  ;
049300101014         EXSR AggioSFL   ;
049400101014         leavesr  ;
0495001010141      endif   ;
049600101014
0497001010141        if vscfil<>*blanks and vscfil<>*zeros  ;
049800101014
0499001010142        IF  %check(digits:vscfil) > 0;
050000101014           ErrMessage  = *on;
050100101014           ErrGenerico = *on;
050200101014           ErrFIL      = *on;
050300101014           V1cmsg      = Msg(03);
050400101014           EXSR AggioSFL   ;
050500101014           leavesr;
0506001010142        ENDIF;
050700101014
050800101014       kfil=%int(vscfil)  ;
050900101014
051000101014       clear og148          ;
051100101014       clear orgfag         ;
051200101014       chain   kfil    azorg01l   ;
0513001010142      if %found(azorg01l)  ;
051400101014       og148= orgde8   ;
0515001010142      endif   ;
051600101014
0517001010142      if  orgfag<>'A' and orgfag<>'F'  and   §oglue <>'S'  ;
051800101014           ErrMessage  = *on;
051900101014           ErrGenerico = *on;
052000101014           ErrFIL      = *on;
052100101014           V1cmsg      = Msg(03);
052200101014           EXSR AggioSFL   ;
052300101014           leavesr;
0524001010142      endif   ;
052500101014
052600101014       vsdfil=orgdes   ;
0527001010141      endif   ;
052800101014
052900101014       exsr  Aggiosfl   ;
053000101014
053100101014       s02nrr= s02nrr + 1 ;
053200101014       chain    s02nrr   TB82s02   ;
053300101014       enddo  ;
053400100830
053500100830       ENDSR  ;
053600081126
053700080703       //--------------------------------------------------------------
053800080703       //?Aggiornamento sfl
053900080703       //--------------------------------------------------------------
054000080703       BEGSR AGGIOSFL  ;
054100100830
054200100824       if v1cmsg<>*blanks   ;
054300080703       errMessage  = *on;
054400080703       errGenerico = *on;
054500100315       c02csr=s02nrr    ;
054600080703       endif    ;
054700100824
054800101014       update  TB82s02  ;
054900100830
055000080703       ENDSR   ;
055100100312       //--------------------------------------------------------------
055200100312       //?Inizializzazione videata 01
055300100312       //--------------------------------------------------------------
055400100312       BEGSR Inzd01  ;
055500100312
055600100315         clear V1cmsg;
055700100826         errMessage  = *off;
055800100312         errGenerico = *off;
055900101014         Errlis=*off       ;
056000101014         ErrClis=*off       ;
056100101014         Errfil=*off       ;
056200101014         v1clis= '?       '   ;
056300101018         clear v1cprop   ;
056400101014         clear v1dlis    ;
056500101018
056600101018         // se passati i dati li imposto
056700101018         if §tb82lis<>*blanks   ;
056800101018         v1clis=§tb82lis   ;
056900101018         v1cprop=§tb82tipol   ;
057000101018
057100101018         exsr ctrd01   ;
057200101018
057300101018           $video='S2' ;
057400101018           $inzs02=*on;
057500101018         endif    ;
057600100831
057700100312         ENDSR    ;
057800080207       //--------------------------------------------------------------
057900100312       //?Inizializzazione SFL02
058000080207       //--------------------------------------------------------------
058100101014       BEGSR InzS02;
058200080207
058300080207       // Pulizia subfile
058400080207         SflDsp_N    = *on;
058500080207         SflDspCtl_N = *on;
058600101014         write  TB82C02;
058700080207         SflDspCtl_N = *off;
058800080207         SflEnd      = *off;
058900100824         SflDsp_N    = *off;
059000100824         Sflnxtchg   = *off;
059100080530
059200100315         S02nrr=0 ;
059300100824         clear V1cmsg;
059400080207         errMessage  = *off;
059500080207         errGenerico = *off;
059600101014         Errfil=*off       ;
059700101018         RevFil=*off       ;
059800101018         RevDes=*off       ;
059900101014         ProtLis=*on       ;
060000100708
060100101018         clear filiali ;
060200101018         clear yy   ;
060300100826
060400100823         // Predispongo le letture e carico SFL
060500101014         clear kke1   ;
060600101014         kke1=v1clis   ;
060700101014           setll    ('LDS':kke1)          tntbe01l    ;
060800101014           reade(n) ('LDS':kke1)          tntbe01l    ;
060900100823
061000101014         dow not %eof(tntbe01l)  ;
061100101014
061200100823         exsr CarSF02  ;
061300100823
061400101014           reade(n) ('LDS':kke1)          tntbe01l    ;
061500100823         enddo   ;
061600100910
061700101014         // Carico altre 150 righe vuote
061800101018         RevFil=*off   ;
061900101018         RevDes=*off   ;
062000101014         clear  vscfil  ;
062100101014         clear  vsdfil  ;
062200101014         clear  vscca   ;
062300101014         clear  vscncl  ;
062400101014         clear  vscpkg  ;
062500101018         xx=  1  ;
062600101018
062700101018         select   ;
062800101018         when v1cprop=' '   ;
062900101018         dow xx<=150   ;
063000101014           s02nrr=s02nrr+1   ;
063100101014           write  TB82S02;
063200101014         xx = xx + 1   ;
063300101014         enddo   ;
063400101018
063500101018         C02csr=1  ;
063600101018
063700101018         other              ;
063800101018         // Leggo da AZORG eventuali filiali mancanti e lì mi posiziono
063900101018         exsr LeggiAZORG  ;
064000101018         endsl   ;
064100101008
064200100823
064300100824         ENDSR  ;
064400100823       //--------------------------------------------------------------
064500100823       //?eseguo caricamento SFL
064600100823       //--------------------------------------------------------------
064700100823          BEGSR   carSF02    ;
064800100830
064900101014         dlds=tbeuni    ;
065000101018           RevFil=*off ;
065100101018           Revdes=*off ;
065200101014
065300101018 1        if tbeke2 <>*blanks   ;
065400101014           vscfil=%editc(§ldsfil:'X')   ;
065500101014
065600101014           kfil=%int(vscfil)   ;
065700101014           chain   kfil    azorg01l    ;
065800101018 2         if %found(azorg01l)   ;
065900101014            vsdfil=orgdes  ;
066000101014           else ;
066100101014            vsdfil=*all'?'  ;
066200101018            clear  og150   ;
066300101018 2         endif   ;
066400101014
066500101018           Revfil=*off  ;
066600101018
066700101018           exsr CtrFil    ;
066800101018 3         if WOK='S'                 ;
066900101018           yy=yy+1  ;
067000101018           filiali(yy)=orgfil  ;
067100101018           else  ;
067200101018           RevFil=*on  ;
067300101018           RevDES=*on  ;
067400101018            vsdfil='NON Selezionab '+orgdes  ;
067500101018 3         endif     ;
067600101018
067700101014           vscca =§ldsca  ;
067800101014           vscncl =§ldsncl ;
067900101014           vscpkg =§ldspkg ;
068000100910
068100101014           s02nrr=s02nrr+1   ;
068200100831
068300101014           write  TB82S02;
068400101014          endif  ;
068500100831
068600100830          ENDSR         ;
068700101018       //--------------------------------------------------------------
068800101018       //?Lettura azorg per filiali con picking  e rulliera
068900101018       //--------------------------------------------------------------
069000101018         BEGSR    LeggiAZORG   ;
069100101018         setll  *loval  azorg01l   ;
069200101018         read   azorg01l    ;
069300101018
069400101018 1       dow  not %eof(AZorg01l)  ;
069500101018
069600101018         Indx=%lookup(orgfil:filiali)  ;
069700101018
069800101018 2       if IndX=0    ;
069900101018         exsr Ctrfil   ;
070000101018 3       if wok='S'   ;
070100101104         clear   vscfil  ;
070200101104            VSDFIL=%editc(ORGFIL:'X' ) + '--> DA INSERIRE '  ;
070300101018            RevDes=*on  ;
070400101018             s02nrr=s02nrr+1   ;
070500101018             write  TB82S02;
070600101018             xx = xx + 1   ;
070700101018 3         endif   ;
070800101018 2       endif   ;
070900101018
071000101018         read   azorg01l    ;
071100101018 1       enddo   ;
071200101018
071300101018         C02csr=s02nrr   ;
071400101018
071500101018         RevDes=*off ;
071600101018         clear vscfil   ;
071700101018         clear vsdfil   ;
071800101018         dow xx<=50   ;
071900101018           s02nrr=s02nrr+1   ;
072000101018           write  TB82S02;
072100101018         xx = xx + 1   ;
072200101018         enddo   ;
072300101018
072400101018          ENDSR         ;
072500101018       //--------------------------------------------------------------
072600101018       //?controllo la filiale in base al tipo selezione scelto
072700101018       //--------------------------------------------------------------
072800101018         BEGSR CtrFil   ;
072900101018         clear wok   ;
073000101018
073100101018         // Escludo le annullate sempre
073200101018         if  orgfva<>' '  ;
073300101018         leavesr  ;
073400101018         endif    ;
073500101018         og147=orgde7   ;
073600101018         // Escludo quelle che non hanno la creazione automatica bolle
073700101018         if  §ognoc='N'   ;
073800101018         leavesr  ;
073900101018         endif    ;
074000101018
074100101018         // Escludo se tipi non "F" e "A" tranne la 212 per 1 e T
074200101018         if orgfag<>'F' and orgfag<>'A' ;
074300101018         if (v1cprop='1' or v1cprop='T') and orgfil=212    ;
074400101018         wok='S'   ;
074500101018         endif    ;
074600101018         leavesr  ;
074700101018         endif   ;
074800101018
074900101018         // Nessuna richiesta di filiale --> sempre OK
075000101018         if v1cprop=' ' ;
075100101018         wok='S'   ;
075200101018         else  ;
075300101018
075400101018         og150=orgdf0   ;
075500101018
075600101018         clear fnlv55ds   ;
075700101018         d55tpt='P'    ;
075800101018         d55lin=orgfil   ;
075900101018         d55drf=%dec(datasys)   ;
076000101018         callp FNLV55R  (fnlv55ds)   ;
076100101018
076200101018         select    ;
076300101018         when v1cprop='P'   ;
076400101018 5         if §ogpck='S' and §ogtis='R'  ;
076500101018             wOK='S'   ;
076600101018           endif   ;
076700101018
076800101018         when v1cprop='1'    ;
076900101018         if d55tfp=orgfil   ;
077000101018             wOK='S'   ;
077100101018           endif   ;
077200101018
077300101018         when v1cprop='2'    ;
077400101018         if d55tfp<>orgfil   ;
077500101018             wOK='S'   ;
077600101018           endif   ;
077700101018
077800101018         when v1cprop='T'    ;
077900101018             wOK='S'   ;
078000101018
078100101018         endsl   ;
078200101018         endif  ;
078300101018
078400101018         ENDSR   ;
078500101018       //--------------------------------------------------------------
078600101014       //?Aggiorno tabella con lista
078700101014       //--------------------------------------------------------------
078800101014           BEGSR   Aggiotabel  ;
078900101014
079000101014           clear  Prima  ;
079100101014           xx=1           ;
079200101014           yy=1           ;
079300101014           s02nrr=1  ;
079400101014           chain s02nrr  TB82s02    ;
079500101014
079600101014 1         dow %found   ;
079700101014           // Scrivo la lista se immesso un record di filiale
079800101014 2         if vscfil>*zeros   ;
079900101014
080000101014 3         if Prima =' '   ;
080100101014           clear kke1   ;
080200101014           clear kke2   ;
080300101014           kke1  = v1clis   ;
080400101014           // Cancello tutti i record esistenti
080500101014           setll    ('LDS':kke1)          tntbe01l    ;
080600101014           reade    ('LDS':kke1)          tntbe01l    ;
080700101014 4         dow     not %eof(tntbe01l)   ;
080800101014
080900101014           delete tntbe000   ;
081000101014           reade    ('LDS':kke1)          tntbe01l    ;
081100101014 4         enddo   ;
081200101014
081300101014           // testata
081400101014           clear kke2   ;
081500101014           chain    ('LDS':kke2: kke1)    tntbe01l    ;
081600101014           clear tntbe000   ;
081700101014           tbecod='LDS'   ;
081800101014           tbeke2=kke1    ;
081900101014           tbeuni=v1dlis   ;
082000101018           %subst(tbeuni:41:1)=v1cprop ;
082100101014           if %found(tntbe01l)  ;
082200101014           update tntbe000   ;
082300101014           else   ;
082400101014           Write tntbe000   ;
082500101014           endif  ;
082600101014
082700101014           Prima = 'E'   ;
082800101014 3         endif  ;
082900101014
083000101014
083100101014           clear tntbe000   ;
083200101014           clear dlds   ;
083300101014           tbecod='LDS'   ;
083400101014           tbeke1=kke1    ;
083500101014           tbeke2=%editc(xx:'X')       ;
083600101014           xx=xx+1  ;
083700101014           clear tbeuni ;
083800101014           §ldsfil=%int(vscfil) ;
083900101014           §ldsca=vscca  ;
084000101014           §ldsncl=vscncl ;
084100101014           §ldspkg = vscpkg  ;
084200101014           tbeuni=dlds   ;
084300101014           write tntbe000  ;
084400101014
084500101014           endif   ;
084600101014
084700101014           s02nrr=s02nrr + 1 ;
084800101014           chain s02nrr  TB82s02    ;
084900101014 3         enddo  ;
085000101014
085100101014       ENDSR   ;
085200101014
085300080206       //--------------------------------------------------------------
085400080206       //?Operazioni finali.
085500080206       //--------------------------------------------------------------
085600080206       BEGSR RoutEnd;
085700080627
085800080704         // Chiusura pgm   ;
085900080206         *inLR = *on;
086000080704
086100080206         return;
086200080206
086300080206       ENDSR;
086400080206
086500080206      /end-free
086600080206
086700080206       //--------------------------------------------------------------
086800080206       //?Schiere a tempo di compilazione.
086900080206       //--------------------------------------------------------------
087000080206
087100080410** - MSG ------------------------------------------------------------------ -*
087200101014Nuova lista distribuzione: immettere la descrizione                             01
087300101014Immettere una filiale                                                           02
087400101014Filiale Errata                                                                  03
087500101014scegliere una lista distribuzione                                               04
