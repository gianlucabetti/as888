000200080206      //--------------------------------------------------------------
000300140318      //?TNSDUNR - Gestione Comodati
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001101140326     FUNcmd00F  uf a E             DISK    rename(uncmd000:uncmdfis)
001102140328     FUNcmd01l  iF   E           k DISK    infds(cmd01)
001103140404     f                                     prefix(c1_)
001104140324     FUNcmd02l  iF   E           k DISK    rename(uncmd000:uncmd02)
001105140331     f                                     infds(cmd02)
001106140410     FUNana10l  iF   E           k DISK
001200140318     fTNSDUND   cf   e             workstn indds(IndDspF)
001300080206     f                                     infds(InfDspF)
001400140319     f                                     sfile(SDUNS02 : S02nrr)
002700100312
002800080206      //---------------------------------------------------------------
002900100824      // - Numero di record in ciascuna videata di subfile
003000100827     d c_SflPag        c                   const(16)
003100100827     d W_SflPag        s              3  0 inz
003200100824     d PAgric          s              3  0 inz
003300100824     d totrig          s              5  0 inz
003400100824
003500080207      // - Tasti funzionali a video
003600080207     d c_F01           c                   const(x'31')
003700080207     d c_F02           c                   const(x'32')
003800080207     d c_F03           c                   const(x'33')
003900080207     d c_F05           c                   const(x'35')
004000080207     d c_F06           c                   const(x'36')
004100080207     d c_F07           c                   const(x'37')
004200080207     d c_F08           c                   const(x'38')
004300080207     d c_F09           c                   const(x'39')
004400080207     d c_F10           c                   const(x'3A')
004500101018     d c_F11           c                   const(x'3B')
004600080207     d c_F12           c                   const(x'3C')
004700080207     d c_F13           c                   const(x'B1')
004800080207     d c_F14           c                   const(x'B2')
004900080207     d c_F15           c                   const(x'B3')
005000080207     d c_F16           c                   const(x'B4')
005100080207     d c_F17           c                   const(x'B5')
005200080207     d c_F18           c                   const(x'B6')
005300080207     d c_F19           c                   const(x'B7')
005400080207     d c_F20           c                   const(x'B8')
005500080207     d c_F21           c                   const(x'B9')
005600080207     d c_F22           c                   const(x'BA')
005700080207     d c_F23           c                   const(x'BB')
005800080207     d c_F24           c                   const(x'BC')
005900080207     d c_Enter         c                   const(x'F1')
006000080207     d c_RollDown      c                   const(x'F4')
006100080207     d c_RollUp        c                   const(x'F5')
006200080214
006300080214      // - Attributi di visualizzazione
006400080215      //  -  DspAtr() - Normale
006500080214     d C_dspatr_Norm   c                   const(x'20')
006600080215      //  -  DspAtr(RI)
006700080214     d C_dspatr_RI     c                   const(x'21')
006800080215      //  -  DspAtr(ND)
006900080214     d C_dspatr_ND     c                   const(x'27')
007000080215      //  -  DspAtr(BL) / Color(Red)
007100080214     d C_dspatr_BL     c                   const(x'28')
007200080206
007300080206      //---------------------------------------------------------------
007400080206      //?Definizione schiere.
007500080206      //---------------------------------------------------------------
007600080206
007700080206      // - Messaggi di errore
007800140207     d MSG             s             78    dim(69) ctdata perrcd(1)
008000100920     d
008400080206      //---------------------------------------------------------------
008500080206      //?Definizione aree dati.
008600080206      //---------------------------------------------------------------
008601140328     D cmd01           DS
008602140328     D  cmd1NRR              397    400B 0
008603140331     D cmd02           DS
008604140331     D  cmd2NRR              397    400B 0
008700080206      // - Dati utente
008800080206     d §AzUte        e ds                  extname(AZUTE00F)
008900080206     d                                     dtaara
009000080206     d §DatiUte      e ds                  extname(dDatiUte)
009100080206     d                                     dtaara
009101140319     d TIBS69DS      E DS                  INZ
009102140319     d DS_cnaco      E DS                  extname(CNACO00F)
009103140319     d DS_cnind      E DS                  extname(CNIND00F)
009104140319     d DS_cnclp      E DS                  extname(CNCLP00F)
009105140319     d DS_fncls      E DS                  extname(FNCLS00F)
009106140324
009107140324     d DS_uncmd      E DS                  extname(UNCMD00F)
009200080206
009500080206      //---------------------------------------------------------------
009600080206      //?Definizione strutture dati.
009700080206      //---------------------------------------------------------------
010200100826
010400080206      // - Status
010500080206     d Psds           sds
010600080206     d   SDSpgm          *proc
010700080206
010800080206      // - InfDS
010900080206     d InfDspF         ds
011000080207     d  dsp_aid              369    369a                                        AID byte
011100100923     d  cursor               370    371a                                        AID byte
011200080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
011300080207     d  min_nrr              378    379i 0                                      Subfile min rrn
011400080207     d  num_rcds             380    381i 0                                      Subfile num rcds
011500080206
011600080206      // - Indicatori su DspF
011700080208     d IndDspF         ds
011800080206        // - Indicatori di gestione del subfile
011900080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
012000080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
012100080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
012200080206     d  SflEnd                        1n   overlay(IndDspF : 33)
012300080206        // - Indicatori di errore
012400080206     d  errMessage                    1n   overlay(IndDspF : 28)
012500080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012600080627        // - Indicatori vari
012800100830     d  EmisMod1                      1n   overlay(IndDspF : 02)
013100140402     d  AbilF6                        1n   overlay(IndDspF : 04)
013101140428     d  AbilF11                       1n   overlay(IndDspF : 89)
013102140402     d  IntPrNd                       1n   overlay(IndDspF : 05)
013200140319     d  IntManPr                      1n   overlay(IndDspF : 06)
013300140319     d  MANPrNd                       1n   overlay(IndDspF : 07)
013301140319     d  NewNd                         1n   overlay(IndDspF : 08)
013302140320     d  AbilAdd                       1n   overlay(IndDspF : 09)
013303140402     d  NdCnn                         1n   overlay(IndDspF : 18)
013304140403     d  ProtSfl                       1n   overlay(IndDspF : 19)
013305140319
014500100315     d
014600140321     d  Errksc                        1n   overlay(IndDspF : 40)
014601140321     d  Errtip                        1n   overlay(IndDspF : 41)
014602140321     d  Errqta                        1n   overlay(IndDspF : 42)
014603140321     d  Errcds                        1n   overlay(IndDspF : 43)
014604140324     d  Errcfg                        1n   overlay(IndDspF : 44)
014605140324     d  Errdsd                        1n   overlay(IndDspF : 45)
014606140324     d  Errdsa                        1n   overlay(IndDspF : 46)
014607140324     d  Errimd                        1n   overlay(IndDspF : 47)
014608140324     d  Errima                        1n   overlay(IndDspF : 48)
014609140324     d  Errcnn                        1n   overlay(IndDspF : 49)
014610140410     d  Erropz                        1n   overlay(IndDspF : 50)
014611140508     d  Errcdss                       1n   overlay(IndDspF : 51)
016700080206
016800080206      // - Parametri ricevuti
016900080206     d KPJBA         e ds
017000100923     d og143         e ds
017100080206
017200080206      // - Reperimento dati utente
017300080206     d TIBS34ds      e ds
017400080206     d dUte01        e ds
017500080702     d DLAT          e ds
017600080702
017700140318     d DUNR          e ds
017701140319     d DSUNIT        e ds
018900080206      // - Ricerca/Controllo tabelle
019000080206     d TIBS02ds      e ds                  inz
019001140321     D TRUL33DS      E DS
019100100907
019400100316
019500100316     d wlbdat          ds                  inz
019600100316     d  g02dat                 1      8  0
019700100316     d  g02inv                 9     16  0
019800100316     d  g02err                17     17
019900100316     d  g02tgi                18     22  0
020000121130     d
020001140403     Dtrul33R          pr                  extpgm('TRUL33R')
020002140321     D kpjba                               likeds(kpjba)
020003140428     Dtnsduir          pr                  extpgm('TNSDUIR')
020004140328     D kpjba                               likeds(kpjba)
020005140328     D parksc                         7  0
020800121130
020900100826
021000100827      //  ricerca alfabetica
021100100827     d  xpardut        s             30
021200100827     d  xparkut        s              1  0
021300100827     d  xparrag        s             48
021400100827     d  xparkcc        s              4  0
021500100827     d  xparsta        s              1  0
021600100827     d  xparflr        s             90
021700100827     d  xpardit        s              3
021800100827     d  xparnum        s              2  0
021900100827     d  xparkcm        s             80
022000100827     d  xparksm        s            140
022100100827     d  xparkdm        s             60
022200100827     d  xparesci       s              1
022300100827     d  xparerr        s              2
022400100827     d  xpariva        s             16
022500100827     d  xparcdf        s             16
022600100826
022700100827      // ricerca filiale su organigramma
022800100827     d   pInFIL        S              3
022900100827     d   pInFAG        S              1
023000100827     d   pInDES        S             25
023100100827     d   pInDIT        S              3
023200100316
023300080206      //---------------------------------------------------------------
023400080206      //?Definizione variabili globali.
023500080206      //---------------------------------------------------------------
023600080206
023700080206      // - Flags booleani
023800080208     d $Fine           s               n   inz(*off)
023900100312     d $InzS02         s               n   inz(*on)
024000100312     d $Inzd01         s               n   inz(*on)
024001140325     d $Finerec        s               n   inz(*off)
024002140328     d werrsfl         s               n   inz(*off)
024003140331     d $Finenum        s               n   inz(*off)
024004140402     d $Finew          s               n   inz(*off)
024100080206
024200100312     d $Video          s              2    inz('D1')
024300100315     d S02nrr          s              4  0 inz
024400081006     d XX              s              3  0 inz
024500080627     d Indx            s              3  0 inz
024700081126     d yy              s              3  0 inz
024800121203     d wdiffe          s              8  0 inz
024801140321     d w3dim           s                   like(v3dim)
024802140321     d w3cds           s                   like(v3cds)
024803140324     d w1dsd           s                   like(v1dsd)
024804140324     d w1dsa           s                   like(v1dsa)
024805140325     d w1imd           s                   like(v1dimd)
024806140325     d w1ima           s                   like(v1dima)
024807140331     d w0070           s              7  0
024808140324     d wand            s              1
024809140321     D Esito           s              1a
024810140327     d waddrcd         s              1a
024811140328     d wtip            s                   like(vsctip)
024812140328     d wtipd           s                   like(vsctipd)
024813140328     d wopz            s              1a
024814140328     d s02rcd          s                   like(c02rcd)
024815140321     dMessaggio        s             50
024900121203     d w1cdim          s              8  0 inz
025000100831     d w1cdtm          s              8  0 inz
025100100908     d w1cdtmd         s              8  0 inz
025200100908     d w1cdtma         s              8  0 inz
025300121203     d w1ctgid         s                   like(g02tgi)
025400121203     d w1ctgia         s                   like(g02tgi)
025500100831     d Dataoggi        s              8  0 inz
025501140404     d wtrovcnn        s              1a
025502140410     d w1cfg           s                   like(v1cfg)
025503140410     d preksc          s                   like(v3ksc)
025504140410     d wtrov_un        s              1
029800101012     d wrkgetlista     s           4096    varying
031100080414
031300100315     d Dataiso         s               d   datfmt(*iso)
031301140321     d Dataisocor      s               d   datfmt(*iso)
031302140402     d Datadmy         s               d   datfmt(*dmy)
031701140324
031702140324     D WrkStringaSql   S           4500
031704140324     D                                     VARYING
031705140325     D rrncmd          s              9s 0
031800080604     d
031900080605     d                 DS
032000080605     d  AA                     1      4  0
032100080605     d  MM                     5      6  0
032200080605     d  GG                     7      8  0
032300080605     d DATA                    1      8  0
032301140428
032302140428      * - DS Parametri per ricerca Programmi chiamanti
032303140428     D DSStack         ds
032304140428     D  £Stack                       10    Dim(100)
032400081009     d
032900080206      //---------------------------------------------------------------
033000080206      //?Definizione procedure usate.
033100080206      //---------------------------------------------------------------
033200080414      /copy gaitrasrc/srcprotopr,tibs02r
033300080414      /copy gaitrasrc/srcprotopr,tibs34r
033400100316      /copy gaitrasrc/srcprotopr,xsrda8
033500100826      /copy gaitrasrc/srcprotopr,xalfa3br
034200100920      /copy gaitrasrc/srcprotopr,qcmdexc
034201140319      /copy gaitrasrc/srcprotopr,tibs69r
034600080206
034700080206      //---------------------------------------------------------------
034800080206      //?Riepilogo indicatori.
034900080206      //---------------------------------------------------------------
035000080207      // - Comuni
035100080207      // 28    : Emissione messaggio di errore a video
035200080207      // - C01/S01
035300080207      // 30    : Condiziona SFLDSP    (*not)
035400080207      // 31    : Condiziona SFLDSPCTL (*not)
035500080207      // 30+31 : Condiziona SFLCLR
035600100824      // 32    : Condiziona SFLNXTCHG
035700080207      // 50    : Errore: Opzione errata
035800080207      // - D01
035900080207      // - Comuni
036000080207      // 99    : Generico di Errore
036100080206      //---------------------------------------------------------------
036200080206
036300080206      //---------------------------------------------------------------
036400080206      //?M A I N - L I N E
036500080206      //---------------------------------------------------------------
036600080206
036700080627     c     *Entry        plist
036800080206     c                   parm                    KPJBA
037000080702
037100080206      /free
037101140324         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
037200080206
037300080206       // Operazioni iniziali
037400080206       exsr RoutInz;
037500081215
037600080206       // Gestione video
037700081215 3     DOW $Fine = *off;
037800081215 4       select;
037900080530
038000080530         // Videata di selezioni
038100100312           when $Video = 'D1';
038200100312             exsr GesD01;
038400100830         // Videata del sfl per inserimento/visualizzazione
038500140318           when $Video = 'S2';
038600100312             exsr GesS02;
038601140325         // Videata singolo record
038602140325           when $Video = 'D3';
038603140325             exsr Gesd03;
038604140326             if dsp_aid = c_F12 or dsp_aid = c_F06;
038605140325                $video='D1';
038606140325             endif;
038700100830
038800080702           other   ;
038900080206             $Fine = *on;
039000081215 4       endsl;
039100081215 3     ENDDO;
039200080206
039300080206       // Operazioni finali
039400080206       exsr RoutEnd;
039500080206
039600080206       //--------------------------------------------------------------
039700080206       //?Operazioni iniziali.
039800080206       //--------------------------------------------------------------
039900080206       BEGSR RoutInz;
040000100312       $inzs02=*on;
040100100312       $inzd01=*on;
040600080206
041000080206         // Reperimento dati job
041100080206         exsr DatiJob;
041200140321         vscpgm='TNSDUNR'         ;
041201140321         // Data corrente
041202140321         dataisocor=%date();
041800100826
042600100827
043000100830       emisMod1=*on   ;
043001140318       v1dmod=*blanks;
043100100831
043101140428      /end-free
043102140428      * reperisco i programmi che hanno richiamato questo pgm
043103140428     c                   Call      'XRTVSTKR'
043104140428     C                   Parm                    PStack         1000
043105140428     C                   Parm                    PStartPoint       3 0
043106140428     C                   Parm                    PEntries          3 0
043107140428     c                   Movel     Pstack        DSstack
043110140428      /free
043111140428       //distattivo f11= Gestione comodati unità edp per evitare chiamata ricorsiva
043112140428         if %lookup ('TNSDUIR   ':£Stack)>0;
043113140428            abilf11=*on;
043114140428         endif;
050900080703
051000080627         ENDSR;
051100080206
051200080206       //--------------------------------------------------------------
051300080206       //?Reperimento Dati del job (Utente/Operativi).
051400080206       //--------------------------------------------------------------
051500080206       BEGSR DatiJob;
051600080206
051700080206         in(E) §AzUte;
051800080206         if NOT %error;
051900080206           in(E) §DatiUte;
052000080206         endif;
052100080206         if %error or RSut = *blanks;
052200080206           clear TIBS34ds;
052300080206           tibs34r(tibs34ds);
052400080206           in §AzUte;
052500080206           in §DatiUte;
052600080206         endif;
052700080206
052800080206       ENDSR;
052900100312       //--------------------------------------------------------------
053000100312       //?Gestione viodeata 01
053100100312       //--------------------------------------------------------------
053200100312       BEGSR Gesd01;
053300100312
053400100312         // Inizializzazione videata
053500100312         if  $Inzd01 = *on;
053600100312            exsr Inzd01;
053700100312            $Inzd01  = *off;
053800100312         endif;
053900100312
054000100312
054100100312         // Emissione Testata e Piede con tasti funzionali abilitati
054200100824         if  errMessage  = *off;
054300140318           write  SDUNT01;
054400100312         endif;
054500100312
054600100312         // Emissione videata
054700140318         exfmt  SDUND01;
054800100312
054900100312         reset errMessage;
055000100312         reset errGenerico;
055100100824         clear V1cmsg    ;
056300100312
056400100312 1       SELECT;
056500100312
056600100312         // - F3=Fine
056700100312 1         WHEN  dsp_aid = c_F03;
056800100312            $Fine = *on;
056801140318         // - F10=Nuovo Comodato
056802140319 1         WHEN  dsp_aid = c_F10;
056803140318            $video='D3' ;
056900100312
057100100824 x1      // Invio
057200100312           OTHER;
057300100316
057400100316           exsr CTRD01  ;
057500100316
057600100316           if ErrGenerico=*ON    ;
057700100316                 leavesr;
057800100316 2             endif;
057803140324       // Se immesso numero contratto emetto singola videata
057804140404       //  if v1cnn>0;
057805140404       //   $video='D3' ;
057806140404       //   vscopz='2';
057807140404       //   vshnrr=cmd2NRR;
057808140404       //  else ;
057809140324       // Altrimenti carico il sfl
058000100315           $video='S2' ;
058100140318           $inzs02=*on;
058101140404       //  endif;
058200100824
058300100312 1       ENDSL;
058400100312
058500100312       ENDSR;
058600100312
058700100316       //--------------------------------------------------------------
058800100316       //?controlli video 1
058900100316       //--------------------------------------------------------------
059000100316       BEGSR CTRD01  ;
059001140324
059002140324       errksc=*off;
059003140324       errdsd=*off;
059004140324       errdsa=*off;
059005140324       errimd=*off;
059006140324       errima=*off;
059007140324       errcnn=*off;
059100100316
059101140324       // CLIENTE
059102140324       // Ricerca alfabetica
059103140324       if v1ksc=*zeros and v1kscd<>*blanks;
059104140324          exsr sr_ricalfa;
059105140324          ErrGenerico=*on;
059106140324          leavesr;
059107140324       endif;
059108140324       // Controllo codice immesso
059109140324       if v1ksc>*zeros;
059110140324           clear DS_cnaco;
059111140324           clear DS_cnind;
059112140324           clear DS_cnclp;
059113140324           clear DS_fncls;
059114140324           clear v1kscd;
059115140324           I69kac=v1ksc  ;
059116140324           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
059117140324           if o69err=*blanks;
059118140324              v1kscd=acorag;
059119140324           else;
059120140324              Errksc=*on;
059121140324              ErrGenerico=*on;
059122140324              ErrMessage =*on  ;
059123140324              v1cmsg=msg(2);
059124140324              leavesr;
059125140324           endif;
059126140410           chain v1ksc uncmd01l;
059127140410           if not %found(uncmd01l);
059128140410              Errksc=*on;
059129140410              ErrGenerico=*on;
059130140410              ErrMessage =*on  ;
059131140410              v1cmsg=msg(8);
059132140410              leavesr;
059134140410           endif;
059135140324       endif;
059136140324       // NUMERO CONTRATTO
059137140324       if v1cnn>0;
059138140324          chain (v1cnn) uncmd02l;
059139140324          if not %found(uncmd02l);
059140140324              Errcnn=*on;
059141140324              ErrGenerico=*on;
059142140324              ErrMessage =*on  ;
059143140324              v1cmsg=msg(7);
059144140324              leavesr;
059145140324          endif;
059146140324       endif;
059147140324       // DATA STAMPA CONTRATTO DAL - AL
059148140328       clear w1dsd;
059149140328       clear w1dsa;
059150140324       // Data Dal
059151140324       if v1dsd>*zeros;
059152140324             clear wlbdat;
059153140324             g02dat = v1dsd  ;
059154140324             xsrda8(wlbdat);
059155140324             if g02err = '1';
059156140324               errMessage  = *on;
059157140324               errGenerico = *on;
059158140324               Errdsd    = *on;
059159140324               V1cmsg = Msg(04);
059160140324               leavesr;
059161140324             endif;
059162140324             w1dsd=g02inv;
059163140324       endif          ;
059164140324       // Data Al
059165140324       if v1dsa>*zeros;
059166140324             clear wlbdat;
059167140324             g02dat = v1dsa  ;
059168140324             xsrda8(wlbdat);
059169140324             if g02err = '1';
059170140324               errMessage  = *on;
059171140324               errGenerico = *on;
059172140324               Errdsd    = *on;
059173140324               V1cmsg = Msg(04);
059174140324               leavesr;
059175140324             endif;
059176140324             w1dsd=g02inv;
059177140324       endif          ;
059178140324       // Data dal no maggiore di data al
059179140324       if w1dsd>0 and w1dsa>0 and w1dsd>w1dsa;
059180140324               errMessage  = *on;
059181140324               errGenerico = *on;
059182140324               Errdsd    = *on;
059183140324               V1cmsg = Msg(06);
059184140324               leavesr;
059185140324       endif;
059186140324       // DATA IMMISSIONE       DAL - AL
059187140328       clear w1imd;
059188140328       clear w1ima;
059189140324       // Data Dal
059190140325       if v1dimd>*zeros;
059191140324             clear wlbdat;
059192140325             g02dat = v1dimd  ;
059193140324             xsrda8(wlbdat);
059194140324             if g02err = '1';
059195140324               errMessage  = *on;
059196140324               errGenerico = *on;
059197140324               Errdsd    = *on;
059198140324               V1cmsg = Msg(04);
059199140324               leavesr;
059200140324             endif;
059201140324             w1imd=g02inv;
059202140324       endif          ;
059203140324       // Data Al
059204140325       if v1dima>*zeros;
059205140324             clear wlbdat;
059206140325             g02dat = v1dima  ;
059207140324             xsrda8(wlbdat);
059208140324             if g02err = '1';
059209140324               errMessage  = *on;
059210140324               errGenerico = *on;
059211140324               Errdsd    = *on;
059212140324               V1cmsg = Msg(04);
059213140324               leavesr;
059214140324             endif;
059215140324             w1ima=g02inv;
059216140324       endif          ;
059217140324       // Data dal no maggiore di data al
059218140324       if w1imd>0 and w1ima>0 and w1imd>w1ima;
059219140324               errMessage  = *on;
059220140324               errGenerico = *on;
059221140324               Errdsd    = *on;
059222140324               V1cmsg = Msg(06);
059223140324               leavesr;
059224140324       endif;
090600100708
090700100316       ENDSR  ;
090800080206       //--------------------------------------------------------------
090900100312       //?Gestione SFL 02
091000080206       //--------------------------------------------------------------
091100100312       BEGSR GesS02;
091200080207
091300080207         // Inizializzazione videata
091400100312         if  $InzS02 = *on;
091500100312            exsr InzS02;
091501140328         // werrsfl = *off;
091502140325            c02csr=1     ;
091503140325            c02rcd=1     ;
091600100824            $InzS02  = *off;
091800100827
091900140324         // Se non trovati record errore in prima videata
092000140325            if s02nrr=0   ;
092100140325               v1cmsg=msg(08)  ;
092200140325               errGenerico=*on  ;
092300140325               ErrMessage=*on  ;
092400140325               $Video='D1'  ;
092500140325               leavesr   ;
092600140325            endif  ;
092601140324         // Se trovato un solo record emetto direttamente la videata singola
092602140325            if s02nrr=1;
092603140325              $Video='D3'  ;
092604140326              vscopz='2';
092605140325              leavesr;
092606140325            endif;
092607140325         endif;
092700100831
092701140328       // c02csr   contiene il numero di riga del subfile su cui era posizionato il cursore
092702140328       // impostando c02rcd   visualizzo la pagina che vedeva l'utente quando ha premuto
092703140328       // l'ultimo tasto
0927041403282         if c02csr   > 0;
092705140328           c02rcd   = c02csr  ;
092706140328          else;
092707140328           c02rcd   = s02rcd   ;
0927081403282         endif;
092709140328       // se non so quale pagina visualizzare forzo pagina 1
0927101403282         if c02rcd   < 1;
092711140328           eval c02rcd   = 1;
0927121403282         endif;
092713140328
092714140328       // salvo il record number del subfile
092715140508       // s02rcd    = c02rcd  ;
098000080207
098100080207         // Emissione Testata e Piede con tasti funzionali abilitati
098200140328       //if  errMessage  = *off;
098300140319           write  SDUNT01;
098500140328       //endif;
098600080703
098700080207         // Emissione videata
098701140325         write  sdunz02;
098800140319         exfmt  SDUNC02;
098900080410
099000080207         reset errMessage;
099100080207         reset errGenerico;
099200100824         clear V1cmsg;
100800100927         s02nrr=1  ;
100900080207
101000080523 1       SELECT;
101100080207
101200080207         // - F3=Fine
101300080523 1         WHEN  dsp_aid = c_F03;
101301140328         // if werrsfl = *on ;
101302140328         //    errmessage  = *on ;
101303140328         //    v1cmsg = msg(12);
101305140328         //    werrsfl = *off;
101306140328         // else;
101400140328               $Fine = *on;
101401140328         // endif;
101500080207
101600080207         // - F12=Ritorno
101700080523 1         WHEN  dsp_aid = c_F12;
101701140328         // if werrsfl = *on ;
101702140328         //    errmessage  = *on ;
101703140328         //    v1cmsg = msg(12);
101705140328         //    werrsfl = *off;
101706140328         // else;
101800140328               $video='D1'   ;
101801140328         // endif;
101802140328
101803140328         // - F5=Refresh
101804140328 1         WHEN  dsp_aid = c_F05;
101805140328         // if werrsfl = *on ;
101806140328         //    errmessage  = *on ;
101807140328         //    v1cmsg = msg(12);
101809140328          //   werrsfl = *off;
101810140328          //else;
101811140328               $inzs02 = *on ;
101812140328          //endif;
101813140327
101814140327         // - F10=Immissione
101815140327 1         WHEN  dsp_aid = c_F10;
101816140327           $video='D3'   ;
101817140327           exsr gesd03;
101818140327           $video = 'S2';
101819140327 1            if dsp_aid = c_F06;
101820140328         //       $inzs02='1';
101821140327              endif;
101900100824
102000100824         // - Roll-Up
102100100824           WHEN  dsp_aid = c_RollUp;
103300100901
105000100824
107600100901
107700080530 x1      // Invio / F6
107800080207           OTHER;
107900081006
107901140327               waddrcd='0';
107902140328               clear wopz;
108000100315               exsr ContrS02 ;
108001140328               // Se gestite opzioni riemetto il sfl
108002140328         //    if waddrcd='1';
108003140328         //       $inzs02=*on ;
108004140328               if wopz = '1';
108005140328                  errGenerico = *on;
108006140328                  leavesr;
108007140328               endif;
108400100930
108500140325 2             if  errGenerico = *on;
108900081006                 leavesr;
109000100824 2             endif;
109100080530
109500121130
109600100907         // F6=conferma Aggiornamento
109700140328           if   dsp_aid = c_F06 ;
109800140328              exsr ConfermaS02;
109801140328               if  errGenerico = *on;
109802140328                 leavesr;
109803140328               endif;
111001140327              $video='D1'   ;
111100080530           endif  ;
111200080207
111300080523 1       ENDSL;
111400080207
111500080207       ENDSR;
111600100901       //--------------------------------------------------------------
111700100901       //?chiamata pgm gestione unità EDP
111800100901       //--------------------------------------------------------------
111900140319       BEGSR CallIntCom  ;
112000100901
112100100901       clear dsunit  ;
112400140410       dsuop0=' 2'  ;
112800100901
112900100901       clear kpjbu  ;
113000100901       kpjbu=dsunit  ;
113100140428       TNSDUIR  (kpjba:v3ksc)          ;
113200100901
113300100901       ENDSR;
113400080207
122900100901
123700100901
125300100824       //--------------------------------------------------------------
125400100824       //?Caricamento pagina vuota
125500100824       //--------------------------------------------------------------
125600100824       BEGSR RollUpS02;
125700100824         S02nrr   = totrig ;
125800100824
126100100824
126200100824       ENDSR;
126300080526       //--------------------------------------------------------------
126400100312       //?controlli SFL02
126500080409       //--------------------------------------------------------------
126600100312       BEGSR ContrS02;
127200100920
127500100315       s02nrr=1  ;
127600140319       readc       sduns02    ;
127700080703
127800140328 0     dow not %eof                      ;
127900100830
128000140325       // Se immessa, controlla l'opzione, altrimenti controlla i dati variati
128001140410        if vscopz<>'A';
128002140410         erropz=*off;
128300140410          if vscopz<>*blanks                ;
128301140328            wopz='1';
128302140410       // Non è possibile annullare se presente contratto
128303140410           if vscopz='4' and vsccnn>0;
128304140410              Erropz=*on;
128307140410              v1cmsg=msg(13);
128308140410              exsr aggiosfl;
128309140410              leavesr;
128310140410           endif;
128311140325            $video='D3' ;
128312140325            exsr gesd03;
128313140327            $video='S2';
128314140325                clear vscopz;
128315140325                chain(n) vshnrr uncmd00f ;
128316140325                if %found(uncmd00f);
128317140327                   exsr carrecord;
128318140328                   exsr aggiosfl ;
128319140403                else;
128320140403                   vscopz='A';
128321140403                   protsfl=*on;
128322140403                   update  sduns02  ;
128323140325                endif ;
128324140325          else ;
128325140325                exsr ctrrecsfl;
128326140328                if errgenerico=*on;
128327140328                   leavesr;
128328140328                endif;
128329140325          endif;
128330140410        endif;
129300100920
156200140319       readc    sduNs02    ;
156300100830 0     enddo         ;
156400100830
156500100830       ENDSR  ;
213500080409
238700100312       //--------------------------------------------------------------
238800100312       //?Inizializzazione videata 01
238900100312       //--------------------------------------------------------------
239000100312       BEGSR Inzd01  ;
239100100312
239200100315         clear V1cmsg;
239400100830         EmisMOD1    = *on ;
239500100826         errMessage  = *off;
239600100312         errGenerico = *off;
239602140318       // codice cliente
239603140318         clear v1ksc ;
239604140318         clear v1kscd;
239605140318         clear v1not;
239606140318         clear v1cnn;
239607140410         v1cfg = 'D' ;
239608140318         clear v1dsd;
239609140318         clear v1dsa;
239610140318         clear v1dimd;
239611140318         clear v1dima;
244600100831
244700100312         ENDSR    ;
244701140319       //--------------------------------------------------------------
244702140319       //?Inizializzazione videata 03 per Immissione
244703140319       //--------------------------------------------------------------
244704140319       BEGSR Inzd03  ;
244705140319
244706140326         exsr inzd03uni;
244717140319         clear v3ksc ;
244718140319         clear v3kscd;
244719140508         if v1ksc>0;
244720140508            v3ksc=v1ksc;
244721140508            exsr sr_decoksc;
244722140508         endif;
244728140319
244729140319         ENDSR    ;
244730140319       //--------------------------------------------------------------
244731140319       //?Riempimento campi videata per gestione/interrogazione comodato
244732140319       //--------------------------------------------------------------
244733140319       BEGSR Ried03  ;
244734140319
244736140319         clear V1cmsg;
244737140319         EmisMOD1    = *off ;
244738140319         errMessage  = *off;
244739140319         errGenerico = *off;
244746140319       // Data immissione
244747140319         dataiso =  %date(cmddim);
244748140319         v3dim = %dec(dataiso:*eur) ;
244749140319       // Numero contratto
244750140319         v3cnn = cmdcnn;
244751140402       // Indicatore di ND su numero contratto e ragione sociale
244752140402         if cmdcnn>0;
244753140402            NdCnn = *off;
244754140402         else;
244755140402            NdCnn = *on ;
244756140402         endif;
244757140319       // Cliente e relativa decodifica
244758140319         v3ksc = cmdksc;
244759140508         exsr sr_decoksc;
244767140402       // Ragione sociale contratto
244768140402         v3rsc=cmdrsc;
244769140319
244770140319       // Tipo/modello e relativa decodifica
244771140319         v3tip =cmdtip;
244772140319         clear dunr;
244773140321         clear tibs02ds  ;
244774140321         t02tla=' '      ;
244775140321         t02mod='C'      ;
244776140321         t02sif=knsif    ;
244777140321         t02cod='UNR'    ;
244778140321         t02ke1=V3TIP    ;
244779140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
244780140321
244781140321         if  t02err=*blanks ;
244782140321            dunr=t02uni;
244783140319            v3tipd = D§UNRDES;
244784140321         ELSE;
244785140321            v3tipd = *ALL'?' ;
244786140319         endif;
244787140319         v3qta = cmdqta;
244788140319         clear v3cfs;
244789140319         if cmdcfs= 'S';
244790140319            v3cfs = cmdcfs;
244791140319         endif;
244792140325         clear v3cds;
244793140325         if cmdcds>0;
244794140325            dataiso =  %date(cmdcds);
244795140325            v3cds = %dec(dataiso:*eur) ;
244796140325         endif;
244797140319         clear v3cfi;
244798140319         if cmdcfi= 'S';
244799140325            v3cfi=cmdcfi;
244800140319         endif;
244801140319         v3cfg = cmdcfg;
244802140331         clear v3cdg;
244803140325         if cmdcdg>0;
244804140325            dataiso =  %date(cmdcdg);
244805140325            v3cdg = %dec(dataiso:*eur) ;
244806140325         endif;
244807140319         v3not = cmdnot;
244808140319
244809140319         ENDSR    ;
244810140319       //--------------------------------------------------------------
244811140326       //?Inizializzazione videata 03 unica per immissione e aggiunta comodato x cli
244812140319       //--------------------------------------------------------------
244813140326       BEGSR inzd03uni;
244814140319
244815140319         clear V1cmsg;
244816140319         EmisMOD1    = *off ;
244817140319         errMessage  = *off;
244818140319         errGenerico = *off;
244819140319       // Data immissione = data del giorno
244820140326         v3dim = %dec(dataisocor:*eur) ;
244821140326         w3dim = %dec(dataisocor) ;
244822140319         clear v3tip ;
244823140319         clear v3tipd;
244824140319         clear v3qta;
244825140319         clear v3cfs;
244826140319         clear v3cds;
244827140319         clear v3cfi;
244828140319         clear v3cfg;
244829140319         clear v3cdg ;
244830140319         clear v3not ;
244831140402         clear v3cnn;
244832140402         clear v3rsc;
244833140331         errksc=*off;
244834140331         errtip=*off;
244835140331         errqta=*off;
244836140331         errcds=*off;
244837140319
244838140319         ENDSR    ;
244839080207       //--------------------------------------------------------------
244900100312       //?Inizializzazione SFL02
245000080207       //--------------------------------------------------------------
245100100312       BEGSR InzS02;
245200080207
245300080207       // Pulizia subfile
245400080207         SflDsp_N    = *on;
245500080207         SflDspCtl_N = *on;
245600140319         write  SDUNC02;
245700080207         SflDspCtl_N = *off;
245800080207         SflEnd      = *off;
245900100824         SflDsp_N    = *off;
246000100824         Sflnxtchg   = *off;
246001140324
246002140326         exec sql close CMDcsr;
246003140411       // Se non richiesto numero contratto
246004140411         if v1cnn=0 ;
246005140324       // Richiamo routine di preparazione stringa SQL
246006140404             exsr sr_prepsql;
246007140404         endif;
246008140324
246009140324         S02nrr=0 ;
246010140324         clear V1cmsg;
246011140324         errMessage  = *off;
246012140324         errGenerico = *off;
246100080530
246101140411        if v1cnn=0            ;
246102140324       // Elaboro i records estratti
246103140404           $finerec=*off;
246104140404           exec sql prepare STRINGASQL from :WrkStringaSql;
246105140404           exec sql declare CMDCSR cursor for StringaSql;
246106140404           exec sql open CMDcsr;
246107140404           dow $finerec=*off;
246108140404              exec sql Fetch CMDcsr into :rrncmd, :ds_uncmd;
246109140404              if sqlcod=100 or sqlcod<0;
246110140404                 $finerec = *on;
246111140404                 leave;
246112140404              endif;
246113140324           // Carico record nel sfl
246114140404              exsr carsf02;
246115140404           enddo;
246116140404        else;
246119140410       // Caricamento sfl per numero contratto o per codice cliente
246121140404           setll v1cnn uncmd02l;
246122140404           reade v1cnn uncmd02l;
246127140404           dow not %eof;
246128140404              exsr carsf02;
246130140410                 reade v1cnn uncmd02l;
246134140404           enddo;
246136140404        endif;
246137140508          s02rcd    = s02nrr  ;
258600100823
258700100824         ENDSR  ;
258701140324       //--------------------------------------------------------------
258702140324       //?eseguo caricamento SFL
258703140324       //--------------------------------------------------------------
258704140324          BEGSR   carSF02    ;
258705140324          clear vscopz  ;
258709140324
258710140324          exsr CarRECORD  ;
258711140328       // Numero relativo di record  sul file comodati
258712140410       select;
258715140410       when v1cnn>0;
258716140410          vshnrr=cmd2NRR;
258717140410       other;
258718140410          vshnrr=rrncmd;
258719140410       endsl;
258725140324          errtip=*off;
258726140324          errcfg=*off;
258727140508          errcdss=*off;
258728140403          protsfl=*off;
258729140324          s02nrr=s02nrr+1   ;
258730140325          write  SDUnS02;
258731140324
258732140324          ENDSR         ;
258733140324       //--------------------------------------------------------------
258734140324       //?Caricamento record SFL
258735140324       //--------------------------------------------------------------
258736140324       BEGSR CarRecord;
258737140324       vscksc=cmdksc;
258738140324       // Decodifico il cliente
258739140324           clear DS_cnaco;
258740140324           clear DS_cnind;
258741140324           clear DS_cnclp;
258742140324           clear DS_fncls;
258743140325           clear vsckscd;
258744140324           I69kac=vscksc  ;
258745140324           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
258746140324           if o69err=*blanks;
258747140324              vsckscd=acorag;
258748140324           endif;
258749140324       // Tipo/modello + decodifica
258750140324         vsctip=cmdtip;
258751140325         exsr decotip;
258760140324       //Quantità
258761140324         vscqta=cmdqta;
258762140324       //Gestione contratto
258763140324         vsccfg=cmdcfg;
258764140324         clear vsccfgd;
258765140324         exsr decocfg;
258766140324       //Data gestione contratto
258767140402       //    clear wlbdat;
258768140402       //    g02inv = cmdcdg ;
258769140402       //    g02err ='3';
258770140402       //    xsrda8(wlbdat);
258771140402       //    vsccdg=g02dat;
258772140402             clear vsccdg;
258773140402             if cmdcdg>0;
258774140402                datadmy=%date(cmdcdg:*iso);
258775140402                vsccdg=%dec(datadmy);
258776140402             endif;
258777140324       //Flag stampa contratto
267300140324       if cmdcfs='S';
267301140324       vsccfs='Si';
267302140324       else;
267303140324       vsccfs='No';
267304140324       endif;
267305140324       //Data stampa contratto
267306140402       //    clear wlbdat;
267307140402       //    g02inv = cmdcds ;
267308140402       //    g02err ='3';
267309140402       //    xsrda8(wlbdat);
267310140402       //    vsccds=g02dat;
267311140402             clear vsccds;
267312140508             clear vshcds;
267313140402             if cmdcds>0;
267314140402                datadmy=%date(cmdcds:*iso);
267315140402                vsccds=%dec(datadmy);
267316140402             endif;
267317140324       //Flag invio a POC
267318140324       if cmdcfi='S';
267319140505         vsccfi='Si';
267320140324       else;
267321140402         vsccfi='  ';
267322140324       endif;
267323140324       //Data immissione
267324140402             vshdim=cmddim;
267325140402       //    clear wlbdat;
267326140402       //    g02inv = cmddim ;
267327140402       //    g02err ='3';
267328140402       //    xsrda8(wlbdat);
267329140402       //    vscdim=g02dat;
267330140402             clear vscdim;
267331140402             if cmddim>0;
267332140402                datadmy=%date(cmddim:*iso);
267333140402                vscdim=%dec(datadmy);
267334140402             endif;
267335140324       //numero contratto
267336140324       vsccnn=cmdcnn;
267337140324       //Note
267338140324       vscnot=cmdnot;
267339140324       ENDSR         ;
339300100830       //--------------------------------------------------------------
339400101001       //?Gestione videata 03
339500100830       //--------------------------------------------------------------
339600100830       BEGSR Gesd03;
339700100830
340400100831
340500100830
340600140319         // Controllo opzione - tasto funzionale F10=Inserimento
340700100830         Emismod1=*off ;
341800120524
341900100830 1       select   ;
341901140319         when dsp_aid = c_f10;
341903140402            v3dmod='  Immissione  '  ;
341904140319            Intprnd   =*off;
341905140402            AbilF6    =*on;
341906140319            IntManPr  =*off;
341907140319            ManPrNd   =*off;
341908140319            NewNd     =*on ;
341909140320            AbilAdd   = *off;
341910140402       // Attivo ND su numero contratto e ragione sociale contratto
341911140402            NdCnn     = *on;
341912140319            exsr inzd03;
342000140319         when vscopz='2'   ;
342200140319            v3dmod=' Manutenzione '  ;
342201140319            IntPrNd   =*off;
342202140402            AbilF6    =*on ;
342203140319            IntManPr  =*on;
342208140319            NewNd     =*off;
342209140320            AbilAdd   = *on;
342210140325            chain(e) vshnrr uncmd00f;
342211140325            if  %error or not %found;
342212140325               v1cmsg=msg(09)  ;
342213140325               ErrGenerico=*on  ;
342214140325               ErrMessage =*on  ;
342215140328               exsr aggiosfl;
342218140325               leavesr  ;
342219140325            else;
342220140331               if cmdcnn>0;
342221140331                 MANPrNd   =*on ;
342222140331               else;
342223140331                 MANPrNd   =*off;
342224140331               endif;
342225140325               exsr ried03;
342226140325            endif;
342400140319 1       when vscopz='5'   ;
342700140319            v3dmod='Interrogazione'  ;
342701140319            IntPrNd   =*on ;
342702140325            IntManPr  =*on;
342703140402            AbilF6    =*off;
342706140319            MANPrNd   =*off;
342708140319            NewNd     =*off;
342709140320            AbilAdd   = *off;
342710140325            chain(n) vshnrr uncmd00f;
342711140319            exsr ried03;
342712140402 1       when vscopz='4'   ;
342713140402            v3dmod=' Annullamento '  ;
342714140402            AbilF6=*on;
342715140402            IntPrNd   =*on ;
342716140402            IntManPr  =*on;
342717140402            MANPrNd   =*off;
342718140402            NewNd     =*off;
342719140402            AbilAdd   = *off;
342720140402            chain(e) vshnrr uncmd00f;
342721140402            if  %error or not %found;
342722140402               v1cmsg=msg(09)  ;
342723140402               ErrGenerico=*on  ;
342724140402               ErrMessage =*on  ;
342725140402               exsr aggiosfl;
342726140402               leavesr  ;
342727140402            endif;
342729140402            exsr ried03;
343500100830         endsl   ;
343600100830
343601140410         clear  preksc;
343900100830
344000100830 0       dow    $video='D3'   ;
344100100830
344200100830         // Emissione Testata
344400140319           write  SDUNT01;
344700100830         // Emissione videata
344800140319         exfmt  SDUND03;
344900100830
345000100830         reset errMessage;
345100100830         reset errGenerico;
345200100830         clear V1cmsg    ;
346900100830
347000100830 1       SELECT;
347100100830
347200100830         // - F12=Ritorno
347300100830 1         WHEN  dsp_aid = c_F12;
347600100924            ErrGenerico=*off ;
347700100924            ErrMessage =*off ;
347800100924            clear v1cmsg  ;
348200100831            Emismod1=*on  ;
348300100830            leavesr  ;
348400100901
348500140410         // - F11=Gestione Comodati Unità EDP
348600140319 1         WHEN  dsp_aid = c_F11;
349600140328           exsr CallIntCom      ;
349700100901
349800140319         // - F10=Aggiunta Comodato
349900140319 1         WHEN  dsp_aid = c_F10;
349901140319            v3dmod=' Aggiunta     '  ;
349902140402            AbilF6    =*on;
349903140319            Intprnd   =*off;
349904140319            IntManPr  =*on ;
349905140319            ManPrNd   =*off;
349906140319            NewNd     =*on ;
349907140402       // Attivo ND su numero contratto e ragione sociale contratto
349908140402            NdCnn     = *on;
349909140326            exsr inzd03Uni;
351600120524
357400100909
357500100830 x1      // Invio
357600100830           OTHER;
357700100830
357800140319           // non controllo record in interrogazione
357900140319 3         if vscopz<>'5'   ;
358000140320             exsr ctrd03    ;
358100100924           endif   ;
358200100830
358300100830 2         if ErrGenerico=*Off   ;
358400100830
358500140319           //  Aggiornamento record di UNCMD
358600140402 2         if dsp_aid = c_F06;
358601140402           // Se richiesto annullamento emetto videata per richiedere la conferma
358602140402            if IntPrNd=*on;
358603140402              exsr sr_Wann;
358604140402              if wvann='S';
358605140402                 delete uncmdfis;
358606140402              endif;
358607140402            else;
358700140321              exsr   confermaCMD ;
358701140402            endif;
358900140321              Emismod1=*on  ;
358901140327              if newnd=*on ;
358902140327                 waddrcd='1';
358903140327              endif;
358904140326              leavesr;
360000100830
360200140321 2         endif   ;
360300100830 2       endif   ;
360400100830
360500100830 1       ENDSL;
360600100830
360700100830 0     enddo   ;
360800100830
360900100830       ENDSR;
360901140320       //--------------------------------------------------------------
360902140320       //?
360903140320       //--------------------------------------------------------------
360904140320       BEGSR Ctrd03  ;
361000100830
361001140321       errksc=*off;
361002140321       errtip=*off;
361003140321       errqta=*off;
361004140321       errcds=*off;
361005140331       errcfg=*off;
361006140321
361007140321       // CLIENTE - RICERCA ALFABETICA
361008140320       if v3ksc=*zeros and v3kscd<>*blanks;
361009140320          exsr sr_ricalfa;
361010140321          ErrGenerico=*on;
361011140321          leavesr;
361012140320       endif;
361013140321       // CLIENTE - CONTROLLO
361014140321       // Controllo il codice in caso di nuovo inserimento
361015140321       if v3ksc=*zeros and IntManPr=*off;
361016140321          Errksc=*on;
361017140321          ErrGenerico=*on;
361018140321          ErrMessage =*on  ;
361019140321          v1cmsg=msg(1);
361020140321          leavesr;
361021140321       endif;
361022140321       if IntManPr =*off;
361027140321           clear v3kscd;
361028140508           exsr sr_decoksc;
361030140508           if o69err<>*blanks;
361033140321              Errksc=*on;
361034140321              ErrGenerico=*on;
361035140321              ErrMessage =*on  ;
361036140321              v1cmsg=msg(2);
361037140321              leavesr;
361038140321           endif;
361039140410       // messaggio di avviso se cliente non presente nel file UNITà nel
361040140410       // magazzino 951
361041140410          if v3ksc<>preksc;
361042140410           preksc=v3ksc;
361043140410           clear wtrov_un;
361044140410           setll (v3ksc) unana10l;
361045140410           reade v3ksc unana10l;
361046140410           dow not %eof and wtrov_un=*blanks;
361047140410              if unamag=951 ;
361048140410                 wtrov_un='S' ;
361049140410              endif;
361050140410              reade v3ksc unana10l;
361051140410           enddo;
361052140410           if wtrov_un=*blanks;
361053140410              Errksc=*on;
361054140410              ErrGenerico=*on;
361055140410              ErrMessage =*on  ;
361056140410              v1cmsg=msg(14);
361057140410              leavesr;
361058140410           endif;
361059140410          endif;
361060140321       endif;
361061140321       // TIPO/MODELLO
361062140321       // Interrogazione tabella  UNR
361063140321       if %scan('?':v3tip)>0;
361064140321          clear v3tip;
361065140321          clear v3tipd;
361066140321          exsr sr_rictip;
361067140328          v3tip=wtip;
361068140328          v3tipd=wtipd;
361069140321          ErrGenerico=*on;
361070140321          leavesr;
361071140321       endif;
361072140321       // Controllo validità del codice inserito
361073140321       if IntPrNd=*off;
361074140321         if v3tip = *blanks;
361075140321       //    Obbligatorio
361076140321              Errtip=*on;
361077140321              ErrGenerico=*on;
361078140321              ErrMessage =*on  ;
361079140321              v1cmsg=msg(1);
361080140321              leavesr;
361081140321         endif;
361082140321         clear tibs02ds  ;
361083140321         t02tla=' '      ;
361084140321         t02mod='C'      ;
361085140321         t02sif=knsif    ;
361086140321         t02cod='UNR'    ;
361087140321         t02ke1=v3tip    ;
361088140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361089140321
361090140321       // Errato
361091140321         if  t02err<>*blanks ;
361092140321            ErrGenerico=*on  ;
361093140321            ErrMessage =*on  ;
361094140321            ErrTIP     =*on  ;
361095140321            V3tipD=*all'?'   ;
361096140321            v1cmsg=msg(3);
361097140321            leavesr;
361098140321         else;
361099140321            V3TIPD=T02UNI;
361100140321         endif;
361101140321       endif;
361102140321       // QUANTITA'
361103140402       // Obbligatoria sempre
361104140402       if IntPrNd=*off ;
361105140321          if v3qta=0;
361106140321            ErrGenerico=*on  ;
361107140321            ErrMessage =*on  ;
361108140321            Errqta     =*on  ;
361109140321            v1cmsg=msg(1);
361110140321            leavesr;
361111140321          endif ;
361112140321       endif;
361113140321       // DATA STAMPA CONTRATTO
361114140321       clear w3cds;
361115140321       if v3cds>0;
361116140321             clear wlbdat;
361117140321             g02dat = v3cds  ;
361118140321             xsrda8(wlbdat);
361119140321             if g02err = '1';
361120140321               errMessage  = *on;
361121140321               errGenerico = *on;
361122140321               Errcds    = *on;
361123140321               V1cmsg = Msg(04);
361124140321               leavesr;
361125140321             endif;
361126140321
361127140321             v3cds  = g02dat;
361128140321             W3cds  = g02inv;
361129140321       endif;
361130140331       // Flag gestione contratto
361131140331         if v3cfg <> ' ' and
361132140331            v3cfg <> 'F' and
361133140331            v3cfg <> 'A' and
361134140331            v3cfg <> 'C'     ;
361135140331            ErrGenerico=*on  ;
361136140331            ErrMessage =*on  ;
361137140331            Errcfg     =*on  ;
361138140331            v1cmsg=msg(10);
361139140331            leavesr;
361140140331         endif;
361141140331       // Non ammesso flag <> blanks se manca il numero contratto
361142140402         if v3cfg <>' ' and v3cfg<>'A' and v3cfg<>'C' and v3cnn=0 ;
361143140331            ErrGenerico=*on  ;
361144140331            ErrMessage =*on  ;
361145140331            Errcfg     =*on  ;
361146140331            v1cmsg=msg(11);
361147140331            leavesr;
361148140331         endif;
361149140328       // Errore se già presente altro record comodato
361150140321       // con il cliente/data immissione/tipo/mod dati
361151140328       chain (v3ksc:w3dim:v3tip) uncmd01l;
361152140328       if %found(uncmd01l) and
361153140328            (newNd=*on or vshnrr<> cmd1nrr);
361154140321               errMessage  = *on;
361155140321               errGenerico = *on;
361156140321               Errksc    = *on;
361157140321               V1cmsg = Msg(05);
361158140321               leavesr;
361159140321       endif;
361160140320       endsr ;
361161140320       //--------------------------------------------------------------
361162140320       //? Ricerca alfabetica cliente
361163140320       //--------------------------------------------------------------
361164140320       BEGSR sr_ricalfa;
361165140320           clear xparsta  ;
361166140320           xparkcc=dutkci ;
361167140320           xparkut= 1     ;
361168140320           xpardut=rsut   ;
361169140320           if $video='D3' ;
361170140320           xparrag=v3kscd ;
361171140320           else  ;
361172140320           xparrag=v1kscd ;
361173140320           endif  ;
361174140320           clear xparflr ;
361175140320           clear xparkcm  ;
361176140320           clear xparksm  ;
361177140320           clear xparkdm  ;
361178140320           xparnum=1      ;
361179140320           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
361180140320                           :xparflr:xpardit:xparnum:xparkcm:xparksm
361181140320                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
361182140320 2          if xparsta<>-1  ;
361183140320               if $video='D3' ;
361184140320                  v3ksc =%int(%subst(xparksm:1:7))   ;
361185140320                  v3kscd=xparrag   ;
361186140320               else;
361187140320                  v1ksc =%int(%subst(xparksm:1:7))   ;
361188140320                  v1kscd=xparrag   ;
361189140320               endif;
361190140320 2          endif            ;
361191140320       endsr ;
361192140321       //--------------------------------------------------------------
361193140321       //? Ricerca Tipo/Modello su tabella UNR
361194140321       //--------------------------------------------------------------
361195140321       BEGSR sr_rictip ;
361196140328         clear wtip;
361197140328         clear wtipd;
361198140321         clear tibs02ds  ;
361199140321         t02tla=' '      ;
361200140321         t02mod='R'      ;
361201140321         t02sif=knsif    ;
361202140321         t02cod='UNR'    ;
361203140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361204140321         if t02ke1<>*blanks;
361205140328            wtip =t02ke1    ;
361206140328            wtipd=t02uni    ;
361207140321         endif;
361208140321       endsr ;
361209140321       //--------------------------------------------------------------
361210140321       //?Conferma Comodato su Data Base
361211140321       //--------------------------------------------------------------
361212140321       BEGSR ConfermaCMD;
361213140404       clear wtrovcnn;
361214140326       if NewNd=*on;
361215140326          clear uncmdfis;
361216140326       CMDDIM=w3dim;
361217140326       endif;
361218140326       CMDKSC=v3ksc;
361219140404       // FLAG Stampa Contratto = 'S':
361220140404       // Se già presente contratto per lo stesso cliente/data immissione recupero
361221140404       // quel numero, data stampa e ragione sociale contratto
361222140404       // altrimenti
361223140404       // Prelevo dal numeratore il numero contratto
361224140402       // e memorizzo la ragione sociale contratto
361225140325       if v3cfs='S' and v3cnn=0;
361226140404         exsr sr_riccnn;
361227140404         if wtrovcnn='S';
361228140404            cmdcnn=c1_cmdcnn;
361229140404            cmdrsc=c1_cmdrsc;
361230140404            cmdcds=c1_cmdcds;
361231140404         else;
361232140404            exsr RepNum    ;
361233140404            if esito = *blanks;
361234140404               cmdcnn=o33nri;
361235140404            else;
361236140404                 errMessage  = *on;
361237140404                 errGenerico = *on;
361238140404                 V1cmsg = Messaggio;
361239140404                 leavesr;
361240140404            endif;
361241140404            cmdrsc=v3kscd;
361242140404         endif;
361243140321       endif;
361244140404       // Data stampa Contratto
361245140404       if wtrovcnn=' ';
361246140404          CMDCDS=W3cds;
361247140404       endif;
361248140321       CMDTIP=v3tip;
361249140321       CMDQTA=v3qta;
361250140321       // flag stampa contratto S/N
361251140321       if v3cfs=*blanks;
361252140321          CMDCFS='N'  ;
361253140321       else;
361254140321          CMDCFS=v3cfs;
361255140321       endif;
361256140321       CMDCFG=v3cfg;
361257140321       if cmdcfg<>*blanks;
361258140321          CMDCdg= %dec(dataisocor);
361259140321       else;
361260140321          clear CMDCdg;
361261140321       endif;
361262140321       // flag invio mail a poc S/N
361263140321       if v3cfs=*blanks;
361264140321          cmdcfi='N';
361265140321       else;
361266140321          CMDCFI=v3cfi;
361267140321       endif;
361268140321       CMDNOT=v3not;
361269140325       if newnd=*on;
361270140325          write uncmdfis;
361271140325       else;
361272140325          update uncmdfis;
361273140325       endif;
361274140321       endsr;
361275140321        //-----------------------------------------------------------------------*
361276140321        // Reperimento numero contratto da numeartore
361277140321        //-----------------------------------------------------------------------*
361278140321        begsr RepNum;
361279140321
361280140321          clear esito;
361281140331          $finenum=*off;
361282140321          clear trul33ds;
361283140321
361284140321          I33TLa = 'L';
361285140321          I33Ope = *ZEROS;
361286140331          I33CNu = 020;
361287140321          I33Num = 1;
361288140321
361289140321          KPJBU = TRUL33DS;
361290140321
361291140331          dow $finenum=*off   ;
361292140403             Trul33R(kpjba);
361293140321
361294140331             TRUL33DS = KPJBU;
361295140321
361296140331             if O33Err <> *zeros;
361297140331               Esito = '2';
361298140331               $finenum=*on;
361299140331               Messaggio = 'Errore in ' +
361300140403                           'reperimento contatore (TRUL33R)';
361301140331             else;
361302140331               w0070=o33nri;
361303140331               setll w0070 uncmd02l;
361304140331        // Se numero già presente nel file comodati ne cerco un altro
361305140331               if not %equal;
361306140331                  $finenum=*on;
361307140331               endif;
361308140331             endif;
361309140331          enddo;
361310140321
361311140321          endsr;
361312140324
361313140321
361314140324       //--------------------------------------------------------------
361315140324       //?Preparazione stringa SQL
361316140324       //--------------------------------------------------------------
361317140324       BEGSR sr_prepsql;
361318140324       clear wand;
361319140328       WrkStringaSql='select rrn(uncmd00f), uncmd00f.* from uncmd00f';
361320140411       // Se richiesto cliente ignoro eventuali altre parzializz.
361321140411       if v1ksc>0;
361322140411          wand='1';
361323140411          WrkStringaSql=WrkStringaSql+ ' where ' +
361324140411          ' CMdksc=' + %editc(v1ksc:'X');
361325140411          leavesr;
361326140411       endif;
361327140324       // Richieste note
361328140324       if v1not<>*blanks;
361329140324          if wand = ' ';
361330140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361331140327             ' CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
361332140324             wand='1';
361333140324          else;
361334140324             WrkStringaSql=WrkStringaSql+
361335140327             ' and CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
361336140324          endif;
361337140324       endif;
361338140324       // Flag gestione Contratto
361339140410       if v1cfg<>*blanks;
361340140410        if v1cfg <> 'T';
361341140410          w1cfg=v1cfg;
361342140410          if w1cfg='D';
361343140410             w1cfg=' ';
361344140410          endif;
361345140324          if wand = ' ';
361346140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361347140410             ' cmdcfg =' + '''' + w1cfg + '''';
361348140324             wand='1';
361349140324          else;
361350140324             WrkStringaSql=WrkStringaSql+
361351140410             ' and cmdcfg =' + '''' + w1cfg + '''';
361352140324          endif;
361353140410        else;
361354140402          if wand = ' ';
361355140402             WrkStringaSql=WrkStringaSql+ ' where ' +
361356140402             ' cmdcfg not in(' + '''' + 'A' + '''' + ','+  '''' +
361357140402             'C' + '''' + ')';
361358140402             wand='1';
361359140402          else;
361360140402             WrkStringaSql=WrkStringaSql+
361361140402             ' and cmdcfg not in(' + '''' + 'A' + '''' + ','+  '''' +
361362140402             'C' + '''' + ')';
361363140402          endif;
361364140410        endif;
361365140410       endif;
361366140402       // Flag da stampare
361367140402       if v1cds =  'S';
361368140402          if wand = ' ';
361369140402             WrkStringaSql=WrkStringaSql+ ' where ' +
361370140402             ' cmdcnn > 0 and cmdcds= 0';
361371140402             wand='1';
361372140402          else;
361373140402             WrkStringaSql=WrkStringaSql+
361374140402             ' and cmdcnn > 0 and cmdcds= 0';
361375140402          endif;
361376140402       endif;
361377140324       // Data stampa  Contratto dal/al
361378140327       if w1dsd> 0 or w1dsa>0;
361379140327        select;
361380140327       // immesse entrambe
361381140327        when w1dsd>0 and w1dsa>0;
361382140327          if wand = ' ' ;
361383140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361384140327             ' cmdcds between';
361385140327             wand='1';
361386140327          else;
361387140327             WrkStringaSql=WrkStringaSql+
361388140327             ' and cmdcds between';
361389140327          endif;
361390140327          WrkStringaSql=WrkStringaSql+
361391140327          ' ' + %editc(w1dsd:'X') + ' and ' + %editc(w1dsa:'X');
361392140327       // immessa solo la data dal
361393140327        when w1dsd>0;
361394140327          if wand = ' ' ;
361395140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361396140327             ' cmdcds >=';
361397140327             wand='1';
361398140327          else;
361399140327             WrkStringaSql=WrkStringaSql+
361400140327             ' and cmdcds >=';
361401140327          endif;
361402140327          WrkStringaSql=WrkStringaSql+
361403140327          ' ' + %editc(w1dsd:'X') ;
361404140327       // immessa solo la data al
361405140327        when w1dsa>0;
361406140327          if wand = ' ' ;
361407140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361408140327             ' cmdcds <=';
361409140327             wand='1';
361410140327          else;
361411140327             WrkStringaSql=WrkStringaSql+
361412140327             ' and cmdcds <=';
361413140327          endif;
361414140327          WrkStringaSql=WrkStringaSql+
361415140327          ' ' + %editc(w1dsa:'X') ;
361416140327        endsl;
361417140324       endif;
361418140324       // Data Immissione dal/al
361419140324       if w1imd> 0 or w1ima>0;
361420140327        select;
361421140327        when w1imd>0 and w1ima>0;
361422140327       // immesse entrambe
361423140324          if wand = ' ' ;
361424140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361425140324             ' cmddim between';
361426140324             wand='1';
361427140324          else;
361428140324             WrkStringaSql=WrkStringaSql+
361429140324             ' and cmddim between';
361430140324          endif;
361431140324          WrkStringaSql=WrkStringaSql+
361432140327          ' ' + %editc(w1imd:'X') + ' and ' + %editc(w1ima:'X');
361433140327       // immessa solo datA dal
361434140327        when w1imd>0            ;
361435140327          if wand = ' ' ;
361436140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361437140327             ' cmddim >=';
361438140327             wand='1';
361439140327          else;
361440140327             WrkStringaSql=WrkStringaSql+
361441140327             ' and cmddim >=';
361442140327          endif;
361443140327          WrkStringaSql=WrkStringaSql+
361444140327          ' ' + %editc(w1imd:'X') ;
361445140327       // immessa solo datA al
361446140327        when w1ima>0            ;
361447140327          if wand = ' ' ;
361448140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361449140327             ' cmddim <=';
361450140327             wand='1';
361451140327          else;
361452140327             WrkStringaSql=WrkStringaSql+
361453140327             ' and cmddim <=';
361454140327          endif;
361455140327          WrkStringaSql=WrkStringaSql+
361456140327          ' ' + %editc(w1ima:'X') ;
361457140327        endsl;
361458140324       endif;
361459140508       // ordino il sfl per ragione sociale contratto/codice cliente
361460140508          WrkStringaSql=WrkStringaSql+ ' order by cmdrsc, cmdksc';
361461140324       endsr;
361462140324       //--------------------------------------------------------------
361463140324       //?Decodifica flag gestione contratto
361464140324       //--------------------------------------------------------------
361465140324       BEGSR Decocfg;
361466140324         select;
361467140324         when vsccfg=' ';
361468140324            vsccfgd='Da Ricevere';
361469140324         when vsccfg='F';
361470140324            vsccfgd='Firmato    ';
361471140324         when vsccfg='A';
361472140324            vsccfgd='Annullato  ';
361473140324         when vsccfg='C';
361474140324            vsccfgd='Chiuso     ';
361475140324         endsl;
361476140325       endsr;
361477140325       //--------------------------------------------------------------
361478140325       //?controllo riga di sfl
361479140325       //--------------------------------------------------------------
361480140325       BEGSR ctrrecsfl;
361481140328
361482140328          errtip=*off;
361483140328          errcfg=*off;
361484140508          errcdss=*off;
361485140328
361486140328       // TIPO/MODELLO
361487140410       // Disattivo i controlli del Tipo/Modello in quanto il campo è ora protetto
361488140410       if errtip=*off and errtip=*on;
361489140328       // Interrogazione tabella  UNR
361490140328       if %scan('?':vsctip)>0;
361491140328          clear vsctip;
361492140328          clear vsctipd;
361493140328          exsr sr_rictip;
361494140328          vsctip=wtip;
361495140328          vsctipd=wtipd;
361496140328          ErrGenerico=*on;
361497140328          exsr aggiosfl;
361498140328          leavesr;
361499140328       endif;
361500140328       // Controllo validità del codice inserito
361501140328         if vsctip = *blanks;
361502140328       //    Obbligatorio
361503140328              Errtip=*on;
361504140328              ErrGenerico=*on;
361505140328              ErrMessage =*on  ;
361506140328              v1cmsg=msg(1);
361507140328              exsr aggiosfl;
361508140328              leavesr;
361509140328         endif;
361510140328         clear tibs02ds  ;
361511140328         t02tla=' '      ;
361512140328         t02mod='C'      ;
361513140328         t02sif=knsif    ;
361514140328         t02cod='UNR'    ;
361515140328         t02ke1=vsctip    ;
361516140328         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361517140328
361518140328       // Errato
361519140328         if  t02err<>*blanks ;
361520140328            ErrGenerico=*on  ;
361521140328            ErrMessage =*on  ;
361522140328            ErrTIP     =*on  ;
361523140328            VsctipD=*all'?'   ;
361524140328            v1cmsg=msg(3);
361525140328            exsr aggiosfl;
361526140328            leavesr;
361527140328         else;
361528140328            VscTIPD=T02UNI;
361529140328         endif;
361530140328       // Errore se già presente altro record comodato
361531140328       // con il cliente/data immissione/tipo/mod dati
361532140328         exsr sr_altrocomod;
361533140328         if errGenerico = *on;
361534140328               leavesr;
361535140328         endif;
361536140410         endif;
361537140328       // Flag gestione contratto + Decodifica
361538140328         clear vsccfgd;
361539140328         if vsccfg <> ' ' and
361540140328            vsccfg <> 'F' and
361541140328            vsccfg <> 'A' and
361542140328            vsccfg <> 'C'     ;
361543140328            ErrGenerico=*on  ;
361544140328            ErrMessage =*on  ;
361545140328            Errcfg     =*on  ;
361546140328            v1cmsg=msg(10);
361547140328            exsr aggiosfl;
361548140328            leavesr;
361549140328         endif;
361550140328         exsr decocfg;
361551140331       // Non ammesso flag <> blanks se manca il numero contratto
361552140331         if vsccfg <>' ' and vsccnn=0;
361553140331            ErrGenerico=*on  ;
361554140331            ErrMessage =*on  ;
361555140331            Errcfg     =*on  ;
361556140331            v1cmsg=msg(11);
361557140331            exsr aggiosfl;
361558140331            leavesr;
361559140331         endif;
361560140508       // DATA STAMPA CONTRATTO
361561140508       clear vshcds;
361562140508       if vsccds>0;
361563140508             clear wlbdat;
361564140508             g02dat = vsccds ;
361565140508             xsrda8(wlbdat);
361566140508             if g02err = '1';
361567140508               errMessage  = *on;
361568140508               errGenerico = *on;
361569140508               Errcdss   = *on;
361570140508               V1cmsg = Msg(04);
361571141027               exsr aggiosfl;
361572140508               leavesr;
361573140508             endif;
361574140508           vshcds = g02inv;
361575140508       endif;
361576140328
361577140328         exsr aggiosfl;
361578140325       endsr;
376215140325       //--------------------------------------------------------------
376216140325       //?Decodifia tipo/modello
376217140325       //--------------------------------------------------------------
376218140325       BEGSR decotip;
376219140325         clear dunr;
376220140325         clear tibs02ds  ;
376221140325         t02tla=' '      ;
376222140325         t02mod='C'      ;
376223140325         t02sif=knsif    ;
376224140325         t02cod='UNR'    ;
376225140325         t02ke1=VSCTIP   ;
376226140325         TNTBE_RicercaControllo  (kpjba : tibs02ds);
376227140325
376228140325         if  t02err=*blanks ;
376229140325            dunr=t02uni;
376230140325            vsctipd = D§UNRDES;
376231140325         ELSE;
376232140325            vsctipd = *ALL'?' ;
376233140325         endif;
376234140325       endsr;
376235140328       //--------------------------------------------------------------
376236140328       //?Aggiornamento sfl
376237140328       //--------------------------------------------------------------
376238140328       BEGSR AGGIOSFL  ;
376241140328
376244140328       if v1cmsg<>*blanks   ;
376245140328       errMessage  = *on;
376246140328       errGenerico = *on;
376247140328       c02csr=s02nrr    ;
376248140328       endif    ;
376249140328
376251140328        Sflnxtchg=*on  ;
376252140403        protsfl=*off;
376254140328
376255140328       update  sduns02  ;
376257140328
376258140328       ENDSR   ;
376259140328       //--------------------------------------------------------------
376260140328       //?Conferma dati sfl2
376261140328       //--------------------------------------------------------------
376262140328       BEGSR Confermas02;
376263140328
376264140328       s02nrr=1  ;
376265140328       readc       sduns02    ;
376266140328
376267140328 0     dow not %eof ;
376268140328       // Ricontrollo che non esista già altro comodato con stesso
376269140328       // cliente/data immmissione/modello
376270140328         exsr sr_altrocomod;
376271140328         if errgenerico=*on;
376272140328       //   werrsfl = *on;
376273140328            leavesr;
376275140328         else;
376276140328            chain(e) vshnrr uncmd00f;
376277140328            if  %error or not %found;
376278140328               v1cmsg=msg(09)  ;
376279140328               ErrGenerico=*on  ;
376280140328               ErrMessage =*on  ;
376281140328               exsr aggiosfl;
376282140328       //      werrsfl = *on;
376283140328               leavesr  ;
376284140328            else;
376285140328               cmdtip=vsctip;
376286140328       //      Se flag gestione contratto diverso aggiorno anche la data
376287140328               if cmdcfg<>vsccfg;
376288140328                  CMDCdg= %dec(dataisocor);
376289140328               endif;
376290140328               cmdcfg=vsccfg;
376291140508               cmdcds=vshcds;
376292140328               update uncmdfis;
376293140328            endif;
376294140328         endif;
376295140328
376296140328       readc    sduNs02    ;
376297140328 0     enddo         ;
376298140328
376299140328       ENDSR  ;
376300140328       //--------------------------------------------------------------
376301140328       //?Controllo presenza record duplicato
376302140328       //--------------------------------------------------------------
376303140328       BEGSR sr_altrocomod;
376304140328         chain (vscksc:vshdim:vsctip) uncmd01l;
376305140328         if %found(uncmd01l) and vshnrr<>cmd1nrr;
376306140328               errMessage  = *on;
376307140328               errGenerico = *on;
376308140328               Errtip    = *on;
376309140328               V1cmsg = Msg(05);
376310140328               exsr aggiosfl;
376311140328         endif;
376312140328       endsr;
376313140402       //--------------------------------------------------------------
376314140402       //?Gestione window richiesta conferma annullamento
376315140402       //--------------------------------------------------------------
376316140402       BEGSR sr_Wann;
376317140402       clear wvann;
376318140402       eval $finew=*off;
376319140402       dow $finew =*off;
376320140402          exfmt sdunwann;
376321140402          if dsp_aid=C_F06;
376322140402             $finew=*on;
376323140402          endif;
376324140402       enddo;
376325140402       endsr;
376326140404       //--------------------------------------------------------------
376327140404       //?Ricerca esistenza contratto per stesso cliente/data immissione
376328140404       //--------------------------------------------------------------
376329140404       BEGSR sr_riccnn;
376331140404        setll (cmdksc:cmddim) uncmd01l;
376332140404        reade (cmdksc:cmddim) uncmd01l;
376333140404        dow  not %eof(uncmd01l);
376334140404           if newnd=*on or vshnrr<>cmd1NRR;
376336140404              wtrovcnn='S';
376337140404              leavesr;
376338140404           endif;
376339140404        reade (cmdksc:cmddim) uncmd01l;
376340140404        enddo;
376341140404       endsr;
376342140508       //--------------------------------------------------------------
376343140508       //?Decodifica ragione sociale cliente D03
376344140508       //--------------------------------------------------------------
376345140508       BEGSR sr_decoksc;
376346140508         clear DS_cnaco;
376347140508         clear DS_cnind;
376348140508         clear DS_cnclp;
376349140508         clear DS_fncls;
376350140508         I69kac=v3ksc  ;
376351140508         tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
376352140508         v3kscd=acorag;
376353140508       endsr;
376354140325       //--------------------------------------------------------------
376355080206       //?Operazioni finali.
376400080206       //--------------------------------------------------------------
376500080206       BEGSR RoutEnd;
376600080627
376700080704         // Chiusura pgm   ;
376800080704         clear tibs02ds  ;
376900080704         t02tla='C'      ;
377000080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
378300100920
378400080206         *inLR = *on;
378500080704
378600080206         return;
378700080206
378800080206       ENDSR;
378900080206
379000080206      /end-free
382100080206
382200080206       //--------------------------------------------------------------
382300080206       //?Schiere a tempo di compilazione.
382400080206       //--------------------------------------------------------------
382500080206
382600080410** - MSG ------------------------------------------------------------------ -*
382700140321Campo obbligatorio                                                              01
382800140321Codice inesistente                                                              02
383000140321Tipo/Modello errato                                                             03
383100140321Data errata                                                                     04
383200140321Già presente comodato per il cliente/data immissione/Modello                    05
383300140324Data dal > di Data Al                                                           06
383400140324N. Contratto inesistente                                                        07
383402140410Nessun dato da visualizzare per la selezione richiesta                          08
383403140328Modifica al momento non eseguibile: record vincolato da altro lavoro            09
383404140328Valori validi: " "=Da ricevere, "F"=Firmato, "C"=Chiuso, "A"=Annullato          10
383500140331Gestione Contratto errato: contratto non ancora creato                          11
383600140328Premere F6=Conferma per non perdere le variazioni eseguite                      12
383700140410Impossibile annullare: presente Bolla/Contratto                                 13
383800140410Attenzione:il Cliente non è presente nelle Unità con Magazzino 951.Enter FORZ.  14
