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
023600161011         if %subst(knmus:1:3)<>'EDP'
023700161011            and %subst(knmus:1:3)<>'EST'    ;
023800081003         UTENonEDP=*on   ;
023900100831         endif      ;
024000100826
024100100917       // Udate - 10
024200100826       dataiso=datasys ;
024300100831       dataeur=dataiso ;
024400100831       Dataoggi=%dec(dataeur) ;
024500100917       dataiso=dataiso- %days(10) ;
024600100917       datamen10=%dec(dataiso);
024700100831 1     endif      ;
024800080703
024900080627         ENDSR;
025000080206
025100080206       //--------------------------------------------------------------
025200080206       //?Reperimento Dati del job (Utente/Operativi).
025300080206       //--------------------------------------------------------------
025400080206       BEGSR DatiJob;
025500080206
025600080206         in(E) §AzUte;
025700080206         if NOT %error;
025800080206           in(E) §DatiUte;
025900080206         endif;
026000080206         if %error or RSut = *blanks;
026100080206           clear TIBS34ds;
026200080206           tibs34r(tibs34ds);
026300080206           in §AzUte;
026400080206           in §DatiUte;
026500080206         endif;
026600080206
026700080206       ENDSR;
026800100901       //--------------------------------------------------------------
026900100901       //?Inizializzazione videata 01
027000100901       //--------------------------------------------------------------
027100100901       BEGSR Inzd01  ;
027200100901
027300100901         clear V1cmsg;
027400100901         errMessage  = *off;
027500100901         errGenerico = *off;
027600100901         v1ccuf ='MAG'   ;
027700100901         v1cconf='S'  ;
027800100901         clear v1cinvr ;
027900100901         clear v1cute  ;
028000100901         clear v1cmgc  ;
028100100901         clear v1cmag  ;
028200100902         v1cdtm=Dataoggi ;
028300100901         clear v1cdtconf  ;
028400100916         clear v1cdim  ;
028500100916         clear v1chim  ;
028600101005         v1cord='D'  ;
028700100901
028800100901         ENDSR    ;
028900100312       //--------------------------------------------------------------
029000100312       //?Gestione viodeata 01
029100100312       //--------------------------------------------------------------
029200100312       BEGSR Gesd01;
029300100312
029400100312         // Inizializzazione videata
029500100312         if  $Inzd01 = *on;
029600100312            exsr Inzd01;
029700100312            $Inzd01  = *off;
029800100312         endif;
029900100312
030000100312
030100100312         // Emissione Testata e Piede con tasti funzionali abilitati
030200100824         if  errMessage  = *off;
030300100901           write  SDULT01;
030400100312         endif;
030500100312
030600100312         // Emissione videata
030700100901         exfmt  SDULD01;
030800100312
030900100312         reset errMessage;
031000100312         reset errGenerico;
031100100824         clear V1cmsg    ;
031200100315         errMAG=*off     ;
031300100315         errMGC=*off     ;
031400100315         errTIP=*off     ;
031500100315         errUTE=*off     ;
031600100826         errCUF=*off     ;
031700100901         errDTConf=*off     ;
031800100312
031900100312 1       SELECT;
032000100312
032100100312         // - F3=Fine
032200100312 1         WHEN  dsp_aid = c_F03;
032300100312            $Fine = *on;
032400100312
032500100312
032600100824 x1      // Invio
032700100312           OTHER;
032800100316
032900100316           exsr CTRD01  ;
033000100901
033100100316
033200100901 2         if ErrGenerico=*ON    ;
033300100316                 leavesr;
033400100316 2             endif;
033500100901
033600100917 2         if    dsp_aid = c_F06 or  dsp_aid = c_F01 ;
033700100901           clear tnsdulds  ;
033800100901           §sdulcuf  =v1ccuf  ;
033900101005           §sdulord  =v1cord         ;
034000100901           §sdulcconf=v1cconf ;
034100100901           §sduldtm  =w1cdtm  ;
034200100901           §sduldtcon=w1cdtconf     ;
034300100901           §sdulinvr =v1cinvr        ;
034400100901 3         if v1cmag >*zeros   ;
034500100901            §sdulmag  =%int(v1cmag)        ;
034600100901 3         endif  ;
034700100901 3         if v1cmgc >*zeros   ;
034800100901            §sdulmgc  =%int(v1cmgc)        ;
034900100901 3         endif  ;
035000100901           §sdulute   =v1cute  ;
035100100917           §sduldim  =w1cdim  ;
035200100917           if v1chim>0 ;
035300100917           §sdulhim  =v1chim+100 ;
035400100917           endif  ;
035500100917           §sdulpre  =d90pre     ;
035600100901
035700100917           // Stampa tabulato
035800100917 2         if    dsp_aid = c_F06 ;
035900100917           §sdultips ='T'     ;
036000100917           endif  ;
036100100917           // Stampa etichette
036200100917 2         if    dsp_aid = c_F01 ;
036300100917           §sdultips ='E'     ;
036400100917           endif  ;
036500100917
036600100917           kcoaz='SDUM'    ;
036700100917           kpjbu= TNSDulDS  ;
036800100917
036900100902      /end-free
037000100902     c                   call      'BCH10'
037100100902     c                   parm                    kpjba
037200100902      /free
037300100917
037400100901         $inzd01 =*on  ;
037500100901         endif ;
037600100312 1       ENDSL;
037700100312
037800100312       ENDSR;
037900100312
038000100316       //--------------------------------------------------------------
038100100316       //?controlli video 1
038200100316       //--------------------------------------------------------------
038300100316       BEGSR CTRD01  ;
038400100316
038500100826       // Ufficio obbligatorio
038600100826       if v1ccuf=*blanks   ;
038700100826               errMessage  = *on;
038800100826               errGenerico = *on;
038900100826               ErrCUF    = *on;
039000100826               V1cmsg = Msg(10);
039100100826               leavesr;
039200100826       endif  ;
039300100826
039400100826       // Per MAG possibile solo I=invio
039500140124       if v1ccuf<>'FOR' and v1cinvr<>'I' and v1cinvr<>' ' ;
039600100826               errMessage  = *on;
039700100826               errGenerico = *on;
039800100826               ErrCUF    = *on;
039900100826               V1cmsg = Msg(11);
040000100826               leavesr;
040100100827       endif  ;
040200100826
040300100901       // Immettere o i DA confermare o la data conferma già fatta
040400100901       if v1cdtconf=0 and v1cconf=' ' ;
040500100826               errMessage  = *on;
040600100826               errGenerico = *on;
040700100901               ErrDTconf = *on;
040800100826               V1cmsg = Msg(12);
040900100826               leavesr;
041000100827       endif  ;
041100100901
041200100901       //   Profilo utente
041300100901 1     if v1cute<>*blanks   ;
041400100901       chain   v1cute  kpprf01l   ;
041500100901
041600100901 2     if not %found(kpprf01l)  ;
041700100901           errUTE=*on ;
041800100901           v1cmsg = Msg(1);
041900100901           errGenerico=*on  ;
042000100901           errMessage=*on  ;
042100100901           leavesr  ;
042200100901 2       endif   ;
042300100901 1     endif   ;
042400100831
042500100917       // data immissione
042600100917       clear w1cdim  ;
042700100917 1     if v1cdim>0  ;
042800100917             clear wlbdat;
042900100917             g02dat = v1cdim ;
043000100917             xsrda8(wlbdat);
043100100917 2           if g02err = '1';
043200100917               errMessage  = *on;
043300100917               errGenerico = *on;
043400100917               ErrDIM    = *on;
043500100917               V1cmsg = Msg(02);
043600100917               leavesr;
043700100917 2           endif  ;
043800100917
043900100917       // non inferiore a 10 giorni
044000100920 2     if g02inv<datamen10  and knmus<>'EDPES'    ;
044100100917               errMessage  = *on;
044200100917               errGenerico = *on;
044300100917               ErrDIM    = *on;
044400100917               V1cmsg = Msg(08);
044500100917               leavesr;
044600100917 2     endif   ;
044700100917             v1cdim = g02dat;
044800100917             w1cdim = g02inv;
044900100917 1     endif;
045000100917
045100100917       // Se immessa ora immettere anche data
045200100917       if v1chim>0 and v1cdim=0 ;
045300100917               errMessage  = *on;
045400100917               errGenerico = *on;
045500100917               ErrDim    = *on;
045600100917               V1cmsg = Msg(09);
045700100917               leavesr;
045800100917       endif  ;
045900100917
046000100917       // Ora immissione
046100100917       if v1chim>2400 or mm>59 ;
046200100917               errMessage  = *on;
046300100917               errGenerico = *on;
046400100917               Errhim    = *on;
046500100917               V1cmsg = Msg(13);
046600100917               leavesr;
046700100917       endif  ;
046800100917
046900100917       // Data conferma movimento
047000100831       clear w1cdtm  ;
047100100902 1     if (v1cdtm>0  and v1cconf  =' ' )
047200100902         or (v1cdtm=0 and v1cconf='S') ;
047300100831               errMessage  = *on;
047400100831               errGenerico = *on;
047500100831               ErrDTM    = *on;
047600100901               V1cmsg = Msg(06);
047700100831               leavesr;
047800100901 1     endif;
047900100901
048000100901       // Emmettere o i dati da confermare o i già conmfermati
048100100901       if v1cconf='S' and v1cdtconf>0 ;
048200100901               errMessage  = *on;
048300100901               errGenerico = *on;
048400100901               ErrDTM    = *on;
048500100901               V1cmsg = Msg(07);
048600100901               leavesr;
048700100917 1     endif;
048800100901
048900100831       if v1cdtm>0  ;
049000100831             clear wlbdat;
049100100831             g02dat = v1cdtm ;
049200100831             xsrda8(wlbdat);
049300100831 2           if g02err = '1';
049400100831               errMessage  = *on;
049500100831               errGenerico = *on;
049600100831               ErrDTM    = *on;
049700100831               V1cmsg = Msg(02);
049800100831               leavesr;
049900100831 2           endif;
050000100831
050100100917       // non inferiore a 10 giorni
050200100917       if g02inv<datamen10      ;
050300100831               errMessage  = *on;
050400100831               errGenerico = *on;
050500100831               ErrDTM    = *on;
050600100901               V1cmsg = Msg(08);
050700100831               leavesr;
050800100831       endif   ;
050900100831             v1cdtm = g02dat;
051000100831             w1cdtm = g02inv;
051100100831 1     endif   ;
051200100901
051300100901       // data già confermati i dati
051400100901       clear w1cdtconf  ;
051500100901       if v1cdtconf>0  ;
051600100901             clear wlbdat;
051700100901             g02dat = v1cdtconf ;
051800100901             xsrda8(wlbdat);
051900100901 2           if g02err = '1';
052000100901               errMessage  = *on;
052100100901               errGenerico = *on;
052200100901               ErrDTCONF = *on;
052300100901               V1cmsg = Msg(02);
052400100901               leavesr;
052500100901 2           endif;
052600100901             v1cdtconf = g02dat;
052700100901             w1cdtconf = g02inv;
052800100901 1     endif   ;
052900100831
053000100316       // Magazzino destinazione
053100100708 1     if v1cmag<>*blanks and v1cmag<>'000' ;
053200100316
053300100316       wazorg='E  '   ;
053400100708       wmag=v1cmag    ;
053500100824       exsr  chkmag   ;
053600100824       v1cmag=wmag    ;
053700100901       v1dmag=wdmag    ;
053800100316
053900100708       if  ErrGenerico=*ON  ;
054000100708         ErrMag=*on  ;
054100100316          if ErrMessage=*on  ;
054200100316           v1cmsg=msg(03)  ;
054300100316          endif    ;
054400100316         leavesr  ;
054500100708       endif    ;
054600100708       else  ;
054700100708       clear v1cmag  ;
054800100901       clear v1dmag  ;
054900100708 1     endif    ;
055000100708
055100100708       // Magazzino correlato
055200100708 1     if v1cmgc<>*blanks  and v1cmgc<>'000'   ;
055300100708
055400100708       wazorg='E  '   ;
055500100708       wmag=v1cmgc    ;
055600100708       exsr  Chkmag   ;
055700100824       v1cmgc=wmag  ;
055800100901       v1dmgc=wdmag  ;
055900100708
056000100826       if  ErrGenerico=*ON  ;
056100100708         ErrMgc=*on  ;
056200100708          if ErrMessage=*on  ;
056300100708           v1cmsg=msg(04)  ;
056400100708          endif    ;
056500100708         leavesr  ;
056600100708       endif    ;
056700100708       else  ;
056800100708       clear v1cmgc  ;
056900100901       clear v1dmgc  ;
057000100708       endif    ;
057100100708
057200100316       ENDSR  ;
057300100316       //--------------------------------------------------------------
057400100708       //?controllo Magazzino
057500100316       //--------------------------------------------------------------
057600100316        BEGSR ChkMag                 ;
057700100316        clear WDMAG    ;
057800100824        Indx=%scan('?':wmag )   ;
057900100316
058000100316       // Mag.Destinazione
058100100826       // per MAG --> Filiale  - solo controllo azorg00f
058200100826       // per FOR --> sia controllo azorg20f  tipi "W" "Y" che azorg00F
058300100316       // Mag.correlato
058400100316       // per MAG --> non controllo, fisso 999
058500100316       // per FOR --> solo controllo azorg00f
058600100316
058700100316       if   wazorg=' '  ;
058800100316       select   ;
058900140124                when      wtmag='D' and v1ccuf <>'FOR'   ;
059000100316                eval      wazorg='0  '                  ;
059100100825                when      wtmag='D' and v1ccuf ='FOR'   ;
059200100826                eval      wazorg='E  '                  ;
059300140124                when      wtmag='C' and v1ccuf <>'FOR'   ;
059400100316                eval      wazorg='999'                  ;
059500100825                when      wtmag='C' and v1ccuf ='FOR'   ;
059600100316                eval      wazorg='0  '                  ;
059700100316       ENDSL  ;
059800100316       endif   ;
059900100826
060000100909       // Ricerca allfabetica
060100100909       if Indx>0      ;
060200100826       clear pinfil   ;
060300100826       clear pinfag   ;
060400100826       clear pindes   ;
060500100909       callp TNSD20R (pinfil:pinfag:pindes)   ;
060600100826       if pinfil>*zeros  ;
060700100826       wmag=pinfil   ;
060800100826       wdmag=pindes   ;
060900100826       else    ;
061000100826       clear wmag  ;
061100100826       endif  ;
061200100826
061300100826        ErrGenerico=*on   ;
061400100826        leavesr  ;
061500100826       endif  ;
061600100827
061700100827       eval   orgfil=%int(wmag)  ;
061800100316
061900100316       if   wazorg='2 '  or wazorg='E ';
062000100316       chain   orgfil   azorg21l   ;
062100100316       if %found(azorg21l) and (orgfag='W' or orgfag='Y') ;
062200100316       wdmag=orgdes  ;
062300100316       else   ;
062400100316       ErrGenerico=*on   ;
062500100316       endif    ;
062600100316       endif    ;
062700100316
062800100827       if wazorg='0 '  or (wazorg='E ' and ErrGenerico=*on )  ;
062900100316
063000100316         chain   orgfil  azorg01l   ;
063100100316
063200100316        if %found(azorg01l) and (orgfag='A' or orgfag='F')  ;
063300100316         wdmag=orgdes  ;
063400100827         ErrGenerico=*off  ;
063500100316         else     ;
063600100316         ErrGenerico=*on   ;
063700100316        endif    ;
063800100316       endif   ;
063900100316
064000100316         if        wazorg<>'0  ' and wazorg<>'2  ' and  wazorg<>'E  '
064100100316                   and wazorg<>*blanks and wmag<>wazorg       ;
064200100316         ErrGenerico=*on   ;
064300100316         endif    ;
064400100316
064500100316         if ErrGenerico =*on ;
064600100316         ErrMessage=*on  ;
064700100316         wdmag=*all'?'   ;
064800100316         endif  ;
064900100316
065000100316          ENDSR    ;
065100100708       //--------------------------------------------------------------
065200100708       //?controllo Tipo Unità
065300100708       //--------------------------------------------------------------
065400100708        BEGSR ChkTip                 ;
065500100708        clear WDTIP    ;
065600100824        indx=%scan('?':wtip )   ;
065700100827
065800100824        if Indx>0             ;
065900100708         clear tibs02ds  ;
066000100708         t02tla=' '      ;
066100100708         t02mod='R'      ;
066200100708         t02sif=knsif    ;
066300100708         t02cod='UNT'    ;
066400100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
066500100708         if t02ke1<>*blanks;
066600100824         wtip =t02ke1    ;
066700100708         wdtip=t02uni    ;
066800100708         else   ;
066900100708         clear  wtip ;
067000100708         endif  ;
067100100708
067200100708        ErrGenerico=*on   ;
067300100708        leavesr  ;
067400100708        endif   ;
067500100708
067600100708         clear tibs02ds  ;
067700100708         t02tla=' '      ;
067800100708         t02mod='C'      ;
067900100708         t02sif=knsif    ;
068000100708         t02cod='UNT'    ;
068100100708         t02ke1=wtip     ;
068200100708         TNTBE_RicercaControllo  (kpjba : tibs02ds);
068300100708
068400100708         if  t02err<>*blanks ;
068500100824         ErrGenerico=*on  ;
068600100824         ErrMessage =*on  ;
068700100824         wdtip=*all'?'   ;
068800100708         else  ;
068900100824         wdtip=t02uni    ;
069000100708         endif    ;
069100100708
069200100708          ENDSR    ;
069300080206       //--------------------------------------------------------------
069400080206       //?Operazioni finali.
069500080206       //--------------------------------------------------------------
069600080206       BEGSR RoutEnd;
069700080627
069800080704         // Chiusura pgm   ;
069900080704         clear tibs02ds  ;
070000080704         t02tla='C'      ;
070100080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
070200080704
070300080206         *inLR = *on;
070400080704
070500080206         return;
070600080206
070700080206       ENDSR;
070800080206
070900080206      /end-free
071000080206
071100080206       //--------------------------------------------------------------
071200080206       //?Schiere a tempo di compilazione.
071300080206       //--------------------------------------------------------------
071400080206
071500080410** - MSG ------------------------------------------------------------------ -*
071600100316Utente errato                                                                   01
071700100316Data Errata                                                                     02
071800100316Magazzino Destinazione Errato                                                   03
071900100316Magazzino Correlato Errato                                                      04
072000100708Tipo unità errato                                                               05
072100100902Immettere "S" per dati da confermare, se immessa relativa data e viceversa      06
072200100901Impossibile richiedere sia i dati DA Confermare che i dati GIA' Confermati      07
072300100917La data non può essere inferiore a 10 giorni                                    08
072400100917Se immessa ora immissione immettere anche data                                  09
072500100826L'indicazione dell'ufficio è obbligatoria                                       10
072600140124Per magazzino CED  o TLC possibile solo l'opzione "INVII"                       11
072700100917Indicare o dati Da Confermare o la data per invii/rientri già confermati        12
072800100917Ora immissione errata                                                           13
