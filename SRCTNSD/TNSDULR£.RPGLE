000100080206      //--------------------------------------------------------------
000200100901      //?TNSDULR - Stampa inVII/rientri  GIORNALIERI UNITÀ EDP
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100100901     fTNSDULD   cf   e             workstn indds(IndDspF)
001200080206     f                                     infds(InfDspF)
001300100312     FAZORG21l  IF   E           K DISK
001400100312     fazorg01l  if   e           k disk
001500100312     fkpprf01l  if   e           k disk
001600100826     fcnaco00f  if   e           k disk
001700080206      //---------------------------------------------------------------
001800080207      // - Tasti funzionali a video
001900080207     d c_F01           c                   const(x'31')
002000080207     d c_F02           c                   const(x'32')
002100080207     d c_F03           c                   const(x'33')
002200080207     d c_F05           c                   const(x'35')
002300080207     d c_F06           c                   const(x'36')
002400080207     d c_F07           c                   const(x'37')
002500080207     d c_F08           c                   const(x'38')
002600080207     d c_F09           c                   const(x'39')
002700080207     d c_F10           c                   const(x'3A')
002800080207     d c_F12           c                   const(x'3C')
002900080207     d c_F13           c                   const(x'B1')
003000080207     d c_F14           c                   const(x'B2')
003100080207     d c_F15           c                   const(x'B3')
003200080207     d c_F16           c                   const(x'B4')
003300080207     d c_F17           c                   const(x'B5')
003400080207     d c_F18           c                   const(x'B6')
003500080207     d c_F19           c                   const(x'B7')
003600080207     d c_F20           c                   const(x'B8')
003700080207     d c_F21           c                   const(x'B9')
003800080207     d c_F22           c                   const(x'BA')
003900080207     d c_F23           c                   const(x'BB')
004000080207     d c_F24           c                   const(x'BC')
004100080207     d c_Enter         c                   const(x'F1')
004200080207     d c_RollDown      c                   const(x'F4')
004300080207     d c_RollUp        c                   const(x'F5')
004400080214
004500080214      // - Attributi di visualizzazione
004600080215      //  -  DspAtr() - Normale
004700080214     d C_dspatr_Norm   c                   const(x'20')
004800080215      //  -  DspAtr(RI)
004900080214     d C_dspatr_RI     c                   const(x'21')
005000080215      //  -  DspAtr(ND)
005100080214     d C_dspatr_ND     c                   const(x'27')
005200080215      //  -  DspAtr(BL) / Color(Red)
005300080214     d C_dspatr_BL     c                   const(x'28')
005400080206
005500080206      //---------------------------------------------------------------
005600080206      //?Definizione schiere.
005700080206      //---------------------------------------------------------------
005800080206      // - Messaggi di errore
005900100917     d MSG             s             78    dim(13) ctdata perrcd(1)
006000080206      //---------------------------------------------------------------
006100080206      //?Definizione aree dati.
006200080206      //---------------------------------------------------------------
006300080206      // - Dati utente
006400080206     d §AzUte        e ds                  extname(AZUTE00F)
006500080206     d                                     dtaara
006600080206     d §DatiUte      e ds                  extname(dDatiUte)
006700080206     d                                     dtaara
006800080206
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
008500080206        // - Indicatori di errore
008600080206     d  errMessage                    1n   overlay(IndDspF : 28)
008700080606     d  errGenerico                   1n   overlay(IndDspF : 99)
008800080627        // - Indicatori vari
008900081003     d  uteNONEDP                     1n   overlay(IndDspF : 04)
009000100315     d
009100100315     d  ErrMAG                        1n   overlay(IndDspF : 40)
009200100315     d  ErrMGC                        1n   overlay(IndDspF : 41)
009300100315     d  ErrTIP                        1n   overlay(IndDspF : 42)
009400100901     d  ErrDTCONF                     1n   overlay(IndDspF : 43)
009500100315     d  ErrUTE                        1n   overlay(IndDspF : 44)
009600100826     d  ErrCUF                        1n   overlay(IndDspF : 46)
009700100917     d  ErrDIM                        1n   overlay(IndDspF : 47)
009800100917     d  ErrhIM                        1n   overlay(IndDspF : 48)
009900100917     d  ErrDTM                        1n   overlay(IndDspF : 52)
010000080206
010100080206
010200080206      // - Reperimento dati utente
010300080206     d TIBS34ds      e ds
010400080206     d dUte01        e ds
010500080702     d DLAT          e ds
010600080702
010700100901     d tnsdulds      e ds
010800100917     d trul90ds      e ds
010900100902     d KPJBA         e ds
011000080206
011100080206      // - Ricerca/Controllo tabelle
011200080206     d TIBS02ds      e ds                  inz
011300100316
011400100316     d wlbdat          ds                  inz
011500100316     d  g02dat                 1      8  0
011600100316     d  g02inv                 9     16  0
011700100316     d  g02err                17     17
011800100316     d  g02tgi                18     22  0
011900100826
012000100827      // ricerca filiale su organigramma
012100100827     d   pInFIL        S              3
012200100827     d   pInFAG        S              1
012300100827     d   pInDES        S             25
012400100827     d   pInDIT        S              3
012500100316
012600080206      //---------------------------------------------------------------
012700080206      //?Definizione variabili globali.
012800080206      //---------------------------------------------------------------
012900080206
013000080206      // - Flags booleani
013100080208     d $Fine           s               n   inz(*off)
013200080206
013300100901     d $Inzd01         s               n   inz(*on)
013400100312     d $Video          s              2    inz('D1')
013500080703     d Primavolta      s              1    inz
013600100901     d Indx            s              3  0 inz
013700100901     d w1cdtconf       s              8  0 inz
013800100831     d w1cdtm          s              8  0 inz
013900100917     d w1cdim          s              8  0 inz
014000100316     d wazorg          s              1    Inz(' ')
014100100316     d wmag            s              3
014200100316     d wdmag           s             30
014300100708     d wtip            s              2
014400100824     d wtMAG           s              1
014500140203     d k_key           s              3
014600100708     d wdtip           s             30
014700100917     d Datamen10       s              8  0
014800100901     d Dataoggi        s              8  0
014900080414
015000100315     d Dataeur         s               d   datfmt(*eur)
015100100315     d Dataiso         s               d   datfmt(*iso)
015200080605     d Dataymd         s               d   datfmt(*ymd)
015300080605     d Datadmy         s               d   datfmt(*dmy)
015400100901     d Datasys         s               d   inz(*sys)
015500080604     d
015600080605     d                 DS
015700100917     d  hh                     1      2  0
015800100917     d  MM                     3      4  0
015900100917     d v1chim                  1      4  0
016000081009     d
016100080206      //---------------------------------------------------------------
016200080206      //?Definizione procedure usate.
016300080206      //---------------------------------------------------------------
016400100901      /copy gaitrasrc/srcprotopr,tibs02r
016500080414      /copy gaitrasrc/srcprotopr,tibs34r
016600100316      /copy gaitrasrc/srcprotopr,xsrda8
016700100909      /copy gaitrasrc/srcprotopr,tnsd20r
016800100917      /copy gaitrasrc/srcprotopr,trul90r
016900080206
017000080206      //---------------------------------------------------------------
017100080206      //?Riepilogo indicatori.
017200080206      //---------------------------------------------------------------
017300080207      // - Comuni
017400080207      // 28    : Emissione messaggio di errore a video
017500080207      // - C01/S01
017600080207      // 30    : Condiziona SFLDSP    (*not)
017700080207      // 31    : Condiziona SFLDSPCTL (*not)
017800080207      // 30+31 : Condiziona SFLCLR
017900100824      // 32    : Condiziona SFLNXTCHG
018000080207      // 50    : Errore: Opzione errata
018100080207      // - D01
018200080207      // - Comuni
018300080207      // 99    : Generico di Errore
018400080206      //---------------------------------------------------------------
018500080206
018600080206      //---------------------------------------------------------------
018700080206      //?M A I N - L I N E
018800080206      //---------------------------------------------------------------
018900080206
019000080627     c     *Entry        plist
019100080206     c                   parm                    KPJBA
019200080702
019300080206      /free
019400080206
019500080206       // Operazioni iniziali
019600080206       exsr RoutInz;
019700100917
019800100917       // Scelta stampanti
019900100917         d90rse='S'  ;
020000140203         k_key='SED'   ;
020100140203         callp TRUL90R (kpjba:trul90ds:k_key)         ;
020200100917         if d90f3='1'  ;
020300100917          $Fine=*on  ;
020400100917         endif  ;
020500081215
020600080206       // Gestione video
020700081215 3     DOW $Fine = *off;
020800080530
020900100917 4       select;
021000080530         // Videata di selezioni
021100100312           when $Video = 'D1';
021200100312             exsr GesD01;
021300080530
021400080702           other   ;
021500080206             $Fine = *on;
021600081215 4       endsl;
021700081215 3     ENDDO;
021800080206
021900080206       // Operazioni finali
022000080206       exsr RoutEnd;
022100080206
022200080206       //--------------------------------------------------------------
022300080206       //?Operazioni iniziali.
022400080206       //--------------------------------------------------------------
022500080206       BEGSR RoutInz;
022600100312       $inzd01=*on;
022700080206
022800080703       // Solo la prima volta
022900080703 1     if primavolta=' '   ;
023000080703
023100080206         // Reperimento dati job
023200080206         exsr DatiJob;
023300100901         vscpgm='TNSDULR'         ;
023400081003
023500081003         // Se sono EDP accendo indicatore
023600081003         if %subst(knmus:1:3)<>'EDP'    ;
023700081003         UTENonEDP=*on   ;
023800100831         endif      ;
023900100826
024000100917       // Udate - 10
024100100826       dataiso=datasys ;
024200100831       dataeur=dataiso ;
024300100831       Dataoggi=%dec(dataeur) ;
024400100917       dataiso=dataiso- %days(10) ;
024500100917       datamen10=%dec(dataiso);
024600100831 1     endif      ;
024700080703
024800080627         ENDSR;
024900080206
025000080206       //--------------------------------------------------------------
025100080206       //?Reperimento Dati del job (Utente/Operativi).
025200080206       //--------------------------------------------------------------
025300080206       BEGSR DatiJob;
025400080206
025500080206         in(E) §AzUte;
025600080206         if NOT %error;
025700080206           in(E) §DatiUte;
025800080206         endif;
025900080206         if %error or RSut = *blanks;
026000080206           clear TIBS34ds;
026100080206           tibs34r(tibs34ds);
026200080206           in §AzUte;
026300080206           in §DatiUte;
026400080206         endif;
026500080206
026600080206       ENDSR;
026700100901       //--------------------------------------------------------------
026800100901       //?Inizializzazione videata 01
026900100901       //--------------------------------------------------------------
027000100901       BEGSR Inzd01  ;
027100100901
027200100901         clear V1cmsg;
027300100901         errMessage  = *off;
027400100901         errGenerico = *off;
027500100901         v1ccuf ='MAG'   ;
027600100901         v1cconf='S'  ;
027700100901         clear v1cinvr ;
027800100901         clear v1cute  ;
027900100901         clear v1cmgc  ;
028000100901         clear v1cmag  ;
028100100902         v1cdtm=Dataoggi ;
028200100901         clear v1cdtconf  ;
028300100916         clear v1cdim  ;
028400100916         clear v1chim  ;
028500101005         v1cord='D'  ;
028600100901
028700100901         ENDSR    ;
028800100312       //--------------------------------------------------------------
028900100312       //?Gestione viodeata 01
029000100312       //--------------------------------------------------------------
029100100312       BEGSR Gesd01;
029200100312
029300100312         // Inizializzazione videata
029400100312         if  $Inzd01 = *on;
029500100312            exsr Inzd01;
029600100312            $Inzd01  = *off;
029700100312         endif;
029800100312
029900100312
030000100312         // Emissione Testata e Piede con tasti funzionali abilitati
030100100824         if  errMessage  = *off;
030200100901           write  SDULT01;
030300100312         endif;
030400100312
030500100312         // Emissione videata
030600100901         exfmt  SDULD01;
030700100312
030800100312         reset errMessage;
030900100312         reset errGenerico;
031000100824         clear V1cmsg    ;
031100100315         errMAG=*off     ;
031200100315         errMGC=*off     ;
031300100315         errTIP=*off     ;
031400100315         errUTE=*off     ;
031500100826         errCUF=*off     ;
031600100901         errDTConf=*off     ;
031700100312
031800100312 1       SELECT;
031900100312
032000100312         // - F3=Fine
032100100312 1         WHEN  dsp_aid = c_F03;
032200100312            $Fine = *on;
032300100312
032400100312
032500100824 x1      // Invio
032600100312           OTHER;
032700100316
032800100316           exsr CTRD01  ;
032900100901
033000100316
033100100901 2         if ErrGenerico=*ON    ;
033200100316                 leavesr;
033300100316 2             endif;
033400100901
033500100917 2         if    dsp_aid = c_F06 or  dsp_aid = c_F01 ;
033600100901           clear tnsdulds  ;
033700100901           §sdulcuf  =v1ccuf  ;
033800101005           §sdulord  =v1cord         ;
033900100901           §sdulcconf=v1cconf ;
034000100901           §sduldtm  =w1cdtm  ;
034100100901           §sduldtcon=w1cdtconf     ;
034200100901           §sdulinvr =v1cinvr        ;
034300100901 3         if v1cmag >*zeros   ;
034400100901            §sdulmag  =%int(v1cmag)        ;
034500100901 3         endif  ;
034600100901 3         if v1cmgc >*zeros   ;
034700100901            §sdulmgc  =%int(v1cmgc)        ;
034800100901 3         endif  ;
034900100901           §sdulute   =v1cute  ;
035000100917           §sduldim  =w1cdim  ;
035100100917           if v1chim>0 ;
035200100917           §sdulhim  =v1chim+100 ;
035300100917           endif  ;
035400100917           §sdulpre  =d90pre     ;
035500100901
035600100917           // Stampa tabulato
035700100917 2         if    dsp_aid = c_F06 ;
035800100917           §sdultips ='T'     ;
035900100917           endif  ;
036000100917           // Stampa etichette
036100100917 2         if    dsp_aid = c_F01 ;
036200100917           §sdultips ='E'     ;
036300100917           endif  ;
036400100917
036500100917           kcoaz='SDUM'    ;
036600100917           kpjbu= TNSDulDS  ;
036700100917
036800100902      /end-free
036900100902     c                   call      'BCH10'
037000100902     c                   parm                    kpjba
037100100902      /free
037200100917
037300100901         $inzd01 =*on  ;
037400100901         endif ;
037500100312 1       ENDSL;
037600100312
037700100312       ENDSR;
037800100312
037900100316       //--------------------------------------------------------------
038000100316       //?controlli video 1
038100100316       //--------------------------------------------------------------
038200100316       BEGSR CTRD01  ;
038300100316
038400100826       // Ufficio obbligatorio
038500100826       if v1ccuf=*blanks   ;
038600100826               errMessage  = *on;
038700100826               errGenerico = *on;
038800100826               ErrCUF    = *on;
038900100826               V1cmsg = Msg(10);
039000100826               leavesr;
039100100826       endif  ;
039200100826
039300100826       // Per MAG possibile solo I=invio
039400140124       if v1ccuf<>'FOR' and v1cinvr<>'I' and v1cinvr<>' ' ;
039500100826               errMessage  = *on;
039600100826               errGenerico = *on;
039700100826               ErrCUF    = *on;
039800100826               V1cmsg = Msg(11);
039900100826               leavesr;
040000100827       endif  ;
040100100826
040200100901       // Immettere o i DA confermare o la data conferma già fatta
040300100901       if v1cdtconf=0 and v1cconf=' ' ;
040400100826               errMessage  = *on;
040500100826               errGenerico = *on;
040600100901               ErrDTconf = *on;
040700100826               V1cmsg = Msg(12);
040800100826               leavesr;
040900100827       endif  ;
041000100901
041100100901       //   Profilo utente
041200100901 1     if v1cute<>*blanks   ;
041300100901       chain   v1cute  kpprf01l   ;
041400100901
041500100901 2     if not %found(kpprf01l)  ;
041600100901           errUTE=*on ;
041700100901           v1cmsg = Msg(1);
041800100901           errGenerico=*on  ;
041900100901           errMessage=*on  ;
042000100901           leavesr  ;
042100100901 2       endif   ;
042200100901 1     endif   ;
042300100831
042400100917       // data immissione
042500100917       clear w1cdim  ;
042600100917 1     if v1cdim>0  ;
042700100917             clear wlbdat;
042800100917             g02dat = v1cdim ;
042900100917             xsrda8(wlbdat);
043000100917 2           if g02err = '1';
043100100917               errMessage  = *on;
043200100917               errGenerico = *on;
043300100917               ErrDIM    = *on;
043400100917               V1cmsg = Msg(02);
043500100917               leavesr;
043600100917 2           endif  ;
043700100917
043800100917       // non inferiore a 10 giorni
043900100920 2     if g02inv<datamen10  and knmus<>'EDPES'    ;
044000100917               errMessage  = *on;
044100100917               errGenerico = *on;
044200100917               ErrDIM    = *on;
044300100917               V1cmsg = Msg(08);
044400100917               leavesr;
044500100917 2     endif   ;
044600100917             v1cdim = g02dat;
044700100917             w1cdim = g02inv;
044800100917 1     endif;
044900100917
045000100917       // Se immessa ora immettere anche data
045100100917       if v1chim>0 and v1cdim=0 ;
045200100917               errMessage  = *on;
045300100917               errGenerico = *on;
045400100917               ErrDim    = *on;
045500100917               V1cmsg = Msg(09);
045600100917               leavesr;
045700100917       endif  ;
045800100917
045900100917       // Ora immissione
046000100917       if v1chim>2400 or mm>59 ;
046100100917               errMessage  = *on;
046200100917               errGenerico = *on;
046300100917               Errhim    = *on;
046400100917               V1cmsg = Msg(13);
046500100917               leavesr;
046600100917       endif  ;
046700100917
046800100917       // Data conferma movimento
046900100831       clear w1cdtm  ;
047000100902 1     if (v1cdtm>0  and v1cconf  =' ' )
047100100902         or (v1cdtm=0 and v1cconf='S') ;
047200100831               errMessage  = *on;
047300100831               errGenerico = *on;
047400100831               ErrDTM    = *on;
047500100901               V1cmsg = Msg(06);
047600100831               leavesr;
047700100901 1     endif;
047800100901
047900100901       // Emmettere o i dati da confermare o i già conmfermati
048000100901       if v1cconf='S' and v1cdtconf>0 ;
048100100901               errMessage  = *on;
048200100901               errGenerico = *on;
048300100901               ErrDTM    = *on;
048400100901               V1cmsg = Msg(07);
048500100901               leavesr;
048600100917 1     endif;
048700100901
048800100831       if v1cdtm>0  ;
048900100831             clear wlbdat;
049000100831             g02dat = v1cdtm ;
049100100831             xsrda8(wlbdat);
049200100831 2           if g02err = '1';
049300100831               errMessage  = *on;
049400100831               errGenerico = *on;
049500100831               ErrDTM    = *on;
049600100831               V1cmsg = Msg(02);
049700100831               leavesr;
049800100831 2           endif;
049900100831
050000100917       // non inferiore a 10 giorni
050100100917       if g02inv<datamen10      ;
050200100831               errMessage  = *on;
050300100831               errGenerico = *on;
050400100831               ErrDTM    = *on;
050500100901               V1cmsg = Msg(08);
050600100831               leavesr;
050700100831       endif   ;
050800100831             v1cdtm = g02dat;
050900100831             w1cdtm = g02inv;
051000100831 1     endif   ;
051100100901
051200100901       // data già confermati i dati
051300100901       clear w1cdtconf  ;
051400100901       if v1cdtconf>0  ;
051500100901             clear wlbdat;
051600100901             g02dat = v1cdtconf ;
051700100901             xsrda8(wlbdat);
051800100901 2           if g02err = '1';
051900100901               errMessage  = *on;
052000100901               errGenerico = *on;
052100100901               ErrDTCONF = *on;
052200100901               V1cmsg = Msg(02);
052300100901               leavesr;
052400100901 2           endif;
052500100901             v1cdtconf = g02dat;
052600100901             w1cdtconf = g02inv;
052700100901 1     endif   ;
052800100831
052900100316       // Magazzino destinazione
053000100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
053100100316
053200100316       wazorg='E  '   ;
053300100708       wmag=v1cmag    ;
053400100824       exsr  chkmag   ;
053500100824       v1cmag=wmag    ;
053600100901       v1dmag=wdmag    ;
053700100316
053800100708       if  ErrGenerico=*ON  ;
053900100708         ErrMag=*on  ;
054000100316          if ErrMessage=*on  ;
054100100316           v1cmsg=msg(03)  ;
054200100316          endif    ;
054300100316         leavesr  ;
054400100708       endif    ;
054500100708       else  ;
054600100708       clear v1cmag  ;
054700100901       clear v1dmag  ;
054800100708 1     endif    ;
054900100708
055000100708       // Magazzino correlato
055100100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
055200100708
055300100708       wazorg='E  '   ;
055400100708       wmag=v1cmgc    ;
055500100708       exsr  Chkmag   ;
055600100824       v1cmgc=wmag  ;
055700100901       v1dmgc=wdmag  ;
055800100708
055900100826       if  ErrGenerico=*ON  ;
056000100708         ErrMgc=*on  ;
056100100708          if ErrMessage=*on  ;
056200100708           v1cmsg=msg(04)  ;
056300100708          endif    ;
056400100708         leavesr  ;
056500100708       endif    ;
056600100708       else  ;
056700100708       clear v1cmgc  ;
056800100901       clear v1dmgc  ;
056900100708       endif    ;
057000100708
057100100316       ENDSR  ;
057200100316       //--------------------------------------------------------------
057300100708       //?controllo Magazzino
057400100316       //--------------------------------------------------------------
057500100316        BEGSR ChkMag                 ;
057600100316        clear WDMAG    ;
057700100824        Indx=%scan('?':wmag )   ;
057800100316
057900100316       // Mag.Destinazione
058000100826       // per MAG --> Filiale  - solo controllo azorg00f
058100100826       // per FOR --> sia controllo azorg20f  tipi "W" "Y" che azorg00F
058200100316       // Mag.correlato
058300100316       // per MAG --> non controllo, fisso 999
058400100316       // per FOR --> solo controllo azorg00f
058500100316
058600100316       if   wazorg=' '  ;
058700100316       select   ;
058800140124                when      wtmag='D' and v1ccuf <>'FOR'   ;
058900100316                eval      wazorg='0  '                  ;
059000100825                when      wtmag='D' and v1ccuf ='FOR'   ;
059100100826                eval      wazorg='E  '                  ;
059200140124                when      wtmag='C' and v1ccuf <>'FOR'   ;
059300100316                eval      wazorg='999'                  ;
059400100825                when      wtmag='C' and v1ccuf ='FOR'   ;
059500100316                eval      wazorg='0  '                  ;
059600100316       ENDSL  ;
059700100316       endif   ;
059800100826
059900100909       // Ricerca allfabetica
060000100909       if Indx>0      ;
060100100826       clear pinfil   ;
060200100826       clear pinfag   ;
060300100826       clear pindes   ;
060400100909       callp TNSD20R (pinfil:pinfag:pindes)   ;
060500100826       if pinfil>*zeros  ;
060600100826       wmag=pinfil   ;
060700100826       wdmag=pindes   ;
060800100826       else    ;
060900100826       clear wmag  ;
061000100826       endif  ;
061100100826
061200100826        ErrGenerico=*on   ;
061300100826        leavesr  ;
061400100826       endif  ;
061500100827
061600100827       eval   orgfil=%int(wmag)  ;
061700100316
061800100316       if   wazorg='2 '  or wazorg='E ';
061900100316       chain   orgfil   azorg21l   ;
062000100316       if %found(azorg21l) and (orgfag='W' or orgfag='Y') ;
062100100316       wdmag=orgdes  ;
062200100316       else   ;
062300100316       ErrGenerico=*on   ;
062400100316       endif    ;
062500100316       endif    ;
062600100316
062700100827       if wazorg='0 '  or (wazorg='E ' and ErrGenerico=*on )  ;
062800100316
062900100316         chain   orgfil  azorg01l   ;
063000100316
063100100316        if %found(azorg01l) and (orgfag='A' or orgfag='F')  ;
063200100316         wdmag=orgdes  ;
063300100827         ErrGenerico=*off  ;
063400100316         else     ;
063500100316         ErrGenerico=*on   ;
063600100316        endif    ;
063700100316       endif   ;
063800100316
063900100316         if        wazorg<>'0  ' and wazorg<>'2  ' and  wazorg<>'E  '
064000100316                   and wazorg<>*blanks and wmag<>wazorg       ;
064100100316         ErrGenerico=*on   ;
064200100316         endif    ;
064300100316
064400100316         if ErrGenerico =*on ;
064500100316         ErrMessage=*on  ;
064600100316         wdmag=*all'?'   ;
064700100316         endif  ;
064800100316
064900100316          ENDSR    ;
065000100708       //--------------------------------------------------------------
065100100708       //?controllo Tipo Unità
065200100708       //--------------------------------------------------------------
065300100708        BEGSR ChkTip                 ;
065400100708        clear WDTIP    ;
065500100824        indx=%scan('?':wtip )   ;
065600100827
065700100824        if Indx>0             ;
065800100708         clear tibs02ds  ;
065900100708         t02tla=' '      ;
066000100708         t02mod='R'      ;
066100100708         t02sif=knsif    ;
066200100708         t02cod='UNT'    ;
066300100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
066400100708         if t02ke1<>*blanks;
066500100824         wtip =t02ke1    ;
066600100708         wdtip=t02uni    ;
066700100708         else   ;
066800100708         clear  wtip ;
066900100708         endif  ;
067000100708
067100100708        ErrGenerico=*on   ;
067200100708        leavesr  ;
067300100708        endif   ;
067400100708
067500100708         clear tibs02ds  ;
067600100708         t02tla=' '      ;
067700100708         t02mod='C'      ;
067800100708         t02sif=knsif    ;
067900100708         t02cod='UNT'    ;
068000100708         t02ke1=wtip     ;
068100100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
068200100708
068300100708         if  t02err<>*blanks ;
068400100824         ErrGenerico=*on  ;
068500100824         ErrMessage =*on  ;
068600100824         wdtip=*all'?'   ;
068700100708         else  ;
068800100824         wdtip=t02uni    ;
068900100708         endif    ;
069000100708
069100100708          ENDSR    ;
069200080206       //--------------------------------------------------------------
069300080206       //?Operazioni finali.
069400080206       //--------------------------------------------------------------
069500080206       BEGSR RoutEnd;
069600080627
069700080704         // Chiusura pgm   ;
069800080704         clear tibs02ds  ;
069900080704         t02tla='C'      ;
070000080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
070100080704
070200080206         *inLR = *on;
070300080704
070400080206         return;
070500080206
070600080206       ENDSR;
070700080206
070800080206      /end-free
070900080206
071000080206       //--------------------------------------------------------------
071100080206       //?Schiere a tempo di compilazione.
071200080206       //--------------------------------------------------------------
071300080206
071400080410** - MSG ------------------------------------------------------------------ -*
071500100316Utente errato                                                                   01
071600100316Data Errata                                                                     02
071700100316Magazzino Destinazione Errato                                                   03
071800100316Magazzino Correlato Errato                                                      04
071900100708Tipo unità errato                                                               05
072000100902Immettere "S" per dati da confermare, se immessa relativa data e viceversa      06
072100100901Impossibile richiedere sia i dati DA Confermare che i dati GIA' Confermati      07
072200100917La data non può essere inferiore a 10 giorni                                    08
072300100917Se immessa ora immissione immettere anche data                                  09
072400100826L'indicazione dell'ufficio è obbligatoria                                       10
072500140124Per magazzino CED  o TLC possibile solo l'opzione "INVII"                       11
072600100917Indicare o dati Da Confermare o la data per invii/rientri già confermati        12
072700100917Ora immissione errata                                                           13
