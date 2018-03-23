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
001103140324     FUNcmd02l  iF   E           k DISK    rename(uncmd000:uncmd02)
001104140331     f                                     infds(cmd02)
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
013100140319     d  IntPrNd                       1n   overlay(IndDspF : 05)
013200140319     d  IntManPr                      1n   overlay(IndDspF : 06)
013300140319     d  MANPrNd                       1n   overlay(IndDspF : 07)
013301140319     d  NewNd                         1n   overlay(IndDspF : 08)
013302140320     d  AbilAdd                       1n   overlay(IndDspF : 09)
013303140319
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
020001140321     Dtrul33c          pr                  extpgm('TRUL33C')
020002140321     D kpjba                               likeds(kpjba)
020003140328     Dtnsduir          pr                  extpgm('TNSDUIR')
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
029800101012     d wrkgetlista     s           4096    varying
031100080414
031300100315     d Dataiso         s               d   datfmt(*iso)
031301140321     d Dataisocor      s               d   datfmt(*iso)
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
032400081009     d
032900080206      //---------------------------------------------------------------
033000080206      //?Definizione procedure usate.
033100080206      //---------------------------------------------------------------
033200080414      /copy gaitrasrc/srcprotopr,tibs02r
033300080414      /copy gaitrasrc/srcprotopr,tibs34r
033400100316      /copy gaitrasrc/srcprotopr,xsrda8
033500100826      /copy gaitrasrc/srcprotopr,xalfa3br
034100100901      /copy gaitrasrc/srcprotopr,trtb47r
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
057804140324           if v1cnn>0;
057805140324            $video='D3' ;
057806140326            vscopz='2';
057807140331            vshnrr=cmd2NRR;
057808140324           else ;
057809140324       // Altrimenti carico il sfl
058000100315           $video='S2' ;
058100140318           $inzs02=*on;
058101140324           endif;
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
059126140324       endif;
059127140324       // NUMERO CONTRATTO
059128140324       if v1cnn>0;
059129140324          chain (v1cnn) uncmd02l;
059130140324          if not %found(uncmd02l);
059131140324              Errcnn=*on;
059132140324              ErrGenerico=*on;
059133140324              ErrMessage =*on  ;
059134140324              v1cmsg=msg(7);
059135140324              leavesr;
059136140324          endif;
059137140324       endif;
059138140324       // DATA STAMPA CONTRATTO DAL - AL
059139140328       clear w1dsd;
059140140328       clear w1dsa;
059141140324       // Data Dal
059142140324       if v1dsd>*zeros;
059143140324             clear wlbdat;
059144140324             g02dat = v1dsd  ;
059145140324             xsrda8(wlbdat);
059146140324             if g02err = '1';
059147140324               errMessage  = *on;
059148140324               errGenerico = *on;
059149140324               Errdsd    = *on;
059150140324               V1cmsg = Msg(04);
059151140324               leavesr;
059152140324             endif;
059153140324             w1dsd=g02inv;
059154140324       endif          ;
059155140324       // Data Al
059156140324       if v1dsa>*zeros;
059157140324             clear wlbdat;
059158140324             g02dat = v1dsa  ;
059159140324             xsrda8(wlbdat);
059160140324             if g02err = '1';
059161140324               errMessage  = *on;
059162140324               errGenerico = *on;
059163140324               Errdsd    = *on;
059164140324               V1cmsg = Msg(04);
059165140324               leavesr;
059166140324             endif;
059167140324             w1dsd=g02inv;
059168140324       endif          ;
059169140324       // Data dal no maggiore di data al
059170140324       if w1dsd>0 and w1dsa>0 and w1dsd>w1dsa;
059171140324               errMessage  = *on;
059172140324               errGenerico = *on;
059173140324               Errdsd    = *on;
059174140324               V1cmsg = Msg(06);
059175140324               leavesr;
059176140324       endif;
059177140324       // DATA IMMISSIONE       DAL - AL
059178140328       clear w1imd;
059179140328       clear w1ima;
059180140324       // Data Dal
059181140325       if v1dimd>*zeros;
059182140324             clear wlbdat;
059183140325             g02dat = v1dimd  ;
059184140324             xsrda8(wlbdat);
059185140324             if g02err = '1';
059186140324               errMessage  = *on;
059187140324               errGenerico = *on;
059188140324               Errdsd    = *on;
059189140324               V1cmsg = Msg(04);
059190140324               leavesr;
059191140324             endif;
059192140324             w1imd=g02inv;
059193140324       endif          ;
059194140324       // Data Al
059195140325       if v1dima>*zeros;
059196140324             clear wlbdat;
059197140325             g02dat = v1dima  ;
059198140324             xsrda8(wlbdat);
059199140324             if g02err = '1';
059200140324               errMessage  = *on;
059201140324               errGenerico = *on;
059202140324               Errdsd    = *on;
059203140324               V1cmsg = Msg(04);
059204140324               leavesr;
059205140324             endif;
059206140324             w1ima=g02inv;
059207140324       endif          ;
059208140324       // Data dal no maggiore di data al
059209140324       if w1imd>0 and w1ima>0 and w1imd>w1ima;
059210140324               errMessage  = *on;
059211140324               errGenerico = *on;
059212140324               Errdsd    = *on;
059213140324               V1cmsg = Msg(06);
059214140324               leavesr;
059215140324       endif;
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
092715140328          s02rcd    = c02rcd  ;
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
112400100901       dsuop0=' 5'  ;
112800100901
112900100901       clear kpjbu  ;
113000100901       kpjbu=dsunit  ;
113100140328       TNSDUIR  (kpjba:v3ksc)          ;
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
128300140325          if vscopz<>*blanks;
128301140328            wopz='1';
128302140325            $video='D3' ;
128303140325            exsr gesd03;
128304140327            $video='S2';
128305140325                clear vscopz;
128306140325                chain(n) vshnrr uncmd00f ;
128307140325                if %found(uncmd00f);
128308140327                   exsr carrecord;
128309140328                   exsr aggiosfl ;
128310140325                endif ;
128312140325          else ;
128313140325                exsr ctrrecsfl;
128314140328                if errgenerico=*on;
128315140328                   leavesr;
128316140328                endif;
128317140325          endif;
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
239607140328         v1cfg = 'T' ;
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
244740140319       // Data immissione
244742140319         dataiso =  %date(cmddim);
244743140319         v3dim = %dec(dataiso:*eur) ;
244744140319       // Numero contratto
244745140319         v3cnn = cmdcnn;
244746140319       // Cliente e relativa decodifica
244747140319         v3ksc = cmdksc;
244748140319         clear DS_cnaco;
244749140319         clear DS_cnind;
244750140319         clear DS_cnclp;
244751140319         clear DS_fncls;
244752140319         I69kac=cmdksc ;
244753140319         tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
244754140319         v3kscd=acorag;
244755140319
244756140319       // Tipo/modello e relativa decodifica
244757140319         v3tip =cmdtip;
244759140319         clear dunr;
244760140321         clear tibs02ds  ;
244761140321         t02tla=' '      ;
244762140321         t02mod='C'      ;
244763140321         t02sif=knsif    ;
244764140321         t02cod='UNR'    ;
244765140321         t02ke1=V3TIP    ;
244766140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
244767140321
244768140321         if  t02err=*blanks ;
244771140321            dunr=t02uni;
244772140319            v3tipd = D§UNRDES;
244773140321         ELSE;
244774140321            v3tipd = *ALL'?' ;
244775140319         endif;
244776140319         v3qta = cmdqta;
244777140319         clear v3cfs;
244778140319         if cmdcfs= 'S';
244779140319            v3cfs = cmdcfs;
244780140319         endif;
244781140325         clear v3cds;
244782140325         if cmdcds>0;
244783140325            dataiso =  %date(cmdcds);
244784140325            v3cds = %dec(dataiso:*eur) ;
244785140325         endif;
244786140319         clear v3cfi;
244787140319         if cmdcfi= 'S';
244788140325            v3cfi=cmdcfi;
244789140319         endif;
244790140319         v3cfg = cmdcfg;
244791140331         clear v3cdg;
244792140325         if cmdcdg>0;
244793140325            dataiso =  %date(cmdcdg);
244794140325            v3cdg = %dec(dataiso:*eur) ;
244795140325         endif;
244796140319         v3not = cmdnot;
244797140319
244798140319         ENDSR    ;
244799140319       //--------------------------------------------------------------
244800140326       //?Inizializzazione videata 03 unica per immissione e aggiunta comodato x cli
244801140319       //--------------------------------------------------------------
244802140326       BEGSR inzd03uni;
244803140319
244804140319         clear V1cmsg;
244805140319         EmisMOD1    = *off ;
244806140319         errMessage  = *off;
244807140319         errGenerico = *off;
244808140319       // Data immissione = data del giorno
244810140326         v3dim = %dec(dataisocor:*eur) ;
244811140326         w3dim = %dec(dataisocor) ;
244812140319         clear v3tip ;
244813140319         clear v3tipd;
244814140319         clear v3qta;
244815140319         clear v3cfs;
244816140319         clear v3cds;
244817140319         clear v3cfi;
244818140319         clear v3cfg;
244819140319         clear v3cdg ;
244820140319         clear v3not ;
244821140331         errksc=*off;
244822140331         errtip=*off;
244823140331         errqta=*off;
244824140331         errcds=*off;
244825140319
244826140319         ENDSR    ;
244827080207       //--------------------------------------------------------------
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
246003140324       // Richiamo routine di preparazione stringa SQL
246004140324         exsr sr_prepsql;
246005140324
246006140324         S02nrr=0 ;
246007140324         clear V1cmsg;
246008140324         errMessage  = *off;
246009140324         errGenerico = *off;
246100080530
246101140324       // Elaboro i records estratti
246102140324        $finerec=*off;
246103140324        exec sql prepare STRINGASQL from :WrkStringaSql;
246104140324        exec sql declare CMDCSR cursor for StringaSql;
246105140324        exec sql open CMDcsr;
246106140324        dow $finerec=*off;
246107140325           exec sql Fetch CMDcsr into :rrncmd, :ds_uncmd;
246108140324           if sqlcod=100 or sqlcod<0;
246109140324              $finerec = *on;
246110140324              leave;
246111140324           endif;
246112140324           // Carico record nel sfl
246113140324           exsr carsf02;
246114140324        enddo;
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
258712140328       vshnrr=rrncmd;
258713140324          errtip=*off;
258714140324          errcfg=*off;
258715140324          s02nrr=s02nrr+1   ;
258721140325          write  SDUnS02;
258724140324
258725140324          ENDSR         ;
258726140324       //--------------------------------------------------------------
258727140324       //?Caricamento record SFL
258728140324       //--------------------------------------------------------------
258729140324       BEGSR CarRecord;
258730140324       vscksc=cmdksc;
258731140324       // Decodifico il cliente
258732140324           clear DS_cnaco;
258733140324           clear DS_cnind;
258734140324           clear DS_cnclp;
258735140324           clear DS_fncls;
258736140325           clear vsckscd;
258737140324           I69kac=vscksc  ;
258738140324           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
258739140324           if o69err=*blanks;
258740140324              vsckscd=acorag;
258741140324           endif;
258742140324       // Tipo/modello + decodifica
258743140324         vsctip=cmdtip;
258744140325         exsr decotip;
258760140324       //Quantità
258761140324         vscqta=cmdqta;
258762140324       //Gestione contratto
258763140324         vsccfg=cmdcfg;
258764140324         clear vsccfgd;
258765140324         exsr decocfg;
258766140324       //Data gestione contratto
258767140324             clear wlbdat;
258768140325             g02inv = cmdcdg ;
258769140325             g02err ='3';
258770140324             xsrda8(wlbdat);
258771140324             vsccdg=g02dat;
258772140324       //Flag stampa contratto
267300140324       if cmdcfs='S';
267301140324       vsccfs='Si';
267302140324       else;
267303140324       vsccfs='No';
267304140324       endif;
267305140324       //Data stampa contratto
267306140324             clear wlbdat;
267307140325             g02inv = cmdcds ;
267308140325             g02err ='3';
267309140324             xsrda8(wlbdat);
267310140324             vsccds=g02dat;
267311140324       //Flag invio a POC
267312140324       if cmdcfi='S';
267313140324       vsccfs='Si';
267314140324       else;
267315140324       vsccfi='No';
267316140324       endif;
267317140324       //Data immissione
267318140328             vshdim=cmddim;
267319140324             clear wlbdat;
267320140325             g02inv = cmddim ;
267321140325             g02err ='3';
267322140324             xsrda8(wlbdat);
267323140324             vscdim=g02dat;
267324140324       //numero contratto
267325140324       vsccnn=cmdcnn;
267326140324       //Note
267327140324       vscnot=cmdnot;
267330140324       ENDSR         ;
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
341903140320            v3dmod='    Immissione'  ;
341904140319            Intprnd   =*off;
341905140319            IntManPr  =*off;
341906140319            ManPrNd   =*off;
341907140319            NewNd     =*on ;
341908140320            AbilAdd   = *off;
341909140319            exsr inzd03;
342000140319         when vscopz='2'   ;
342200140319            v3dmod=' Manutenzione '  ;
342201140319            IntPrNd   =*off;
342202140319            IntManPr  =*on;
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
342706140319            MANPrNd   =*off;
342708140319            NewNd     =*off;
342709140320            AbilAdd   = *off;
342710140325            chain(n) vshnrr uncmd00f;
342711140319            exsr ried03;
343500100830         endsl   ;
343600100830
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
348500140319         // - F11=Interrogazione Comodati Unità EDP
348600140319 1         WHEN  dsp_aid = c_F11;
349600140328           exsr CallIntCom      ;
349700100901
349800140319         // - F10=Aggiunta Comodato
349900140319 1         WHEN  dsp_aid = c_F10;
349901140319            v3dmod=' Aggiunta     '  ;
349902140319            Intprnd   =*off;
349903140319            IntManPr  =*on ;
349904140319            ManPrNd   =*off;
349905140319            NewNd     =*on ;
349906140326            exsr inzd03Uni;
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
358600140321 2         if dsp_aid = c_F06  and IntPrNd=*off;
358700140321              exsr   confermaCMD ;
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
361005140321
361006140321       // CLIENTE - RICERCA ALFABETICA
361007140320       if v3ksc=*zeros and v3kscd<>*blanks;
361008140320          exsr sr_ricalfa;
361009140321          ErrGenerico=*on;
361010140321          leavesr;
361011140320       endif;
361012140321       // CLIENTE - CONTROLLO
361013140321       // Controllo il codice in caso di nuovo inserimento
361014140321       if v3ksc=*zeros and IntManPr=*off;
361015140321          Errksc=*on;
361016140321          ErrGenerico=*on;
361017140321          ErrMessage =*on  ;
361018140321          v1cmsg=msg(1);
361019140321          leavesr;
361020140321       endif;
361021140321       if IntManPr =*off;
361022140321           clear DS_cnaco;
361023140321           clear DS_cnind;
361024140321           clear DS_cnclp;
361025140321           clear DS_fncls;
361026140321           clear v3kscd;
361027140321           I69kac=v3ksc  ;
361028140321           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
361029140321           if o69err=*blanks;
361030140321              v3kscd=acorag;
361031140321           else;
361032140321              Errksc=*on;
361033140321              ErrGenerico=*on;
361034140321              ErrMessage =*on  ;
361035140321              v1cmsg=msg(2);
361036140321              leavesr;
361037140321           endif;
361038140321       endif;
361039140321       // TIPO/MODELLO
361040140321       // Interrogazione tabella  UNR
361041140321       if %scan('?':v3tip)>0;
361042140321          clear v3tip;
361043140321          clear v3tipd;
361044140321          exsr sr_rictip;
361045140328          v3tip=wtip;
361046140328          v3tipd=wtipd;
361047140321          ErrGenerico=*on;
361048140321          leavesr;
361049140321       endif;
361050140321       // Controllo validità del codice inserito
361051140321       if IntPrNd=*off;
361052140321         if v3tip = *blanks;
361053140321       //    Obbligatorio
361054140321              Errtip=*on;
361055140321              ErrGenerico=*on;
361056140321              ErrMessage =*on  ;
361057140321              v1cmsg=msg(1);
361058140321              leavesr;
361059140321         endif;
361060140321         clear tibs02ds  ;
361061140321         t02tla=' '      ;
361062140321         t02mod='C'      ;
361063140321         t02sif=knsif    ;
361064140321         t02cod='UNR'    ;
361065140321         t02ke1=v3tip    ;
361066140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361067140321
361068140321       // Errato
361069140321         if  t02err<>*blanks ;
361070140321            ErrGenerico=*on  ;
361071140321            ErrMessage =*on  ;
361072140321            ErrTIP     =*on  ;
361073140321            V3tipD=*all'?'   ;
361074140321            v1cmsg=msg(3);
361075140321            leavesr;
361076140321         else;
361077140321            V3TIPD=T02UNI;
361078140321         endif;
361079140321       endif;
361080140321       // QUANTITA'
361081140321       // Obbligatoria se presente il contratto o flag stampa contratto='S'
361082140321       if IntPrNd=*off and (v3cnn>0 or v3cfs='S');
361083140321          if v3qta=0;
361084140321            ErrGenerico=*on  ;
361085140321            ErrMessage =*on  ;
361086140321            Errqta     =*on  ;
361087140321            v1cmsg=msg(1);
361088140321            leavesr;
361089140321          endif ;
361090140321       endif;
361091140321       // DATA STAMPA CONTRATTO
361092140321       clear w3cds;
361093140321       if v3cds>0;
361094140321             clear wlbdat;
361095140321             g02dat = v3cds  ;
361096140321             xsrda8(wlbdat);
361097140321             if g02err = '1';
361098140321               errMessage  = *on;
361099140321               errGenerico = *on;
361100140321               Errcds    = *on;
361101140321               V1cmsg = Msg(04);
361102140321               leavesr;
361103140321             endif;
361104140321
361105140321             v3cds  = g02dat;
361106140321             W3cds  = g02inv;
361107140321       endif;
361108140328       // Errore se già presente altro record comodato
361109140321       // con il cliente/data immissione/tipo/mod dati
361111140328       chain (v3ksc:w3dim:v3tip) uncmd01l;
361112140328       if %found(uncmd01l) and
361113140328            (newNd=*on or vshnrr<> cmd1nrr);
361114140321               errMessage  = *on;
361115140321               errGenerico = *on;
361116140321               Errksc    = *on;
361117140321               V1cmsg = Msg(05);
361118140321               leavesr;
361119140321       endif;
361120140320       endsr ;
361121140320       //--------------------------------------------------------------
361122140320       //? Ricerca alfabetica cliente
361123140320       //--------------------------------------------------------------
361124140320       BEGSR sr_ricalfa;
361125140320           clear xparsta  ;
361126140320           xparkcc=dutkci ;
361127140320           xparkut= 1     ;
361128140320           xpardut=rsut   ;
361129140320           if $video='D3' ;
361130140320           xparrag=v3kscd ;
361131140320           else  ;
361132140320           xparrag=v1kscd ;
361133140320           endif  ;
361134140320           clear xparflr ;
361135140320           clear xparkcm  ;
361136140320           clear xparksm  ;
361137140320           clear xparkdm  ;
361138140320           xparnum=1      ;
361139140320           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
361140140320                           :xparflr:xpardit:xparnum:xparkcm:xparksm
361141140320                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
361142140320 2          if xparsta<>-1  ;
361143140320               if $video='D3' ;
361144140320                  v3ksc =%int(%subst(xparksm:1:7))   ;
361145140320                  v3kscd=xparrag   ;
361146140320               else;
361147140320                  v1ksc =%int(%subst(xparksm:1:7))   ;
361148140320                  v1kscd=xparrag   ;
361149140320               endif;
361150140320 2          endif            ;
361151140320       endsr ;
361152140321       //--------------------------------------------------------------
361153140321       //? Ricerca Tipo/Modello su tabella UNR
361154140321       //--------------------------------------------------------------
361155140321       BEGSR sr_rictip ;
361156140328         clear wtip;
361157140328         clear wtipd;
361158140321         clear tibs02ds  ;
361159140321         t02tla=' '      ;
361160140321         t02mod='R'      ;
361161140321         t02sif=knsif    ;
361162140321         t02cod='UNR'    ;
361163140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361164140321         if t02ke1<>*blanks;
361165140328            wtip =t02ke1    ;
361166140328            wtipd=t02uni    ;
361167140321         endif;
361168140321       endsr ;
361169140321       //--------------------------------------------------------------
361170140321       //?Conferma Comodato su Data Base
361171140321       //--------------------------------------------------------------
361172140321       BEGSR ConfermaCMD;
361173140326       if NewNd=*on;
361174140326          clear uncmdfis;
361175140326       CMDDIM=w3dim;
361176140326       endif;
361177140326       CMDKSC=v3ksc;
361178140321       // Prelevo dal numeratore il numero contratto se flag stampa contratto=S
361179140325       if v3cfs='S' and v3cnn=0;
361180140321          exsr RepNum    ;
361181140321          if esito = *blanks;
361182140331             cmdcnn=o33nri;
361183140321          else;
361184140321               errMessage  = *on;
361185140321               errGenerico = *on;
361186140321               V1cmsg = Messaggio;
361187140321             leavesr;
361188140321          endif;
361189140321       endif;
361190140321       CMDTIP=v3tip;
361191140321       CMDQTA=v3qta;
361192140321       // flag stampa contratto S/N
361193140321       if v3cfs=*blanks;
361194140321          CMDCFS='N'  ;
361195140321       else;
361196140321          CMDCFS=v3cfs;
361197140321       endif;
361198140321       CMDCDS=W3cds;
361199140321       CMDCFG=v3cfg;
361200140321       if cmdcfg<>*blanks;
361201140321          CMDCdg= %dec(dataisocor);
361202140321       else;
361203140321          clear CMDCdg;
361204140321       endif;
361205140321       // flag invio mail a poc S/N
361206140321       if v3cfs=*blanks;
361207140321          cmdcfi='N';
361208140321       else;
361209140321          CMDCFI=v3cfi;
361210140321       endif;
361211140321       CMDNOT=v3not;
361212140325       if newnd=*on;
361213140325          write uncmdfis;
361214140325       else;
361215140325          update uncmdfis;
361216140325       endif;
361217140321       endsr;
361218140321        //-----------------------------------------------------------------------*
361219140321        // Reperimento numero contratto da numeartore
361220140321        //-----------------------------------------------------------------------*
361221140321        begsr RepNum;
361222140321
361223140321          clear esito;
361224140331          $finenum=*off;
361225140321          clear trul33ds;
361226140321          clear kpjba;
361227140321
361228140321          I33TLa = 'L';
361229140321          I33Ope = *ZEROS;
361230140331          I33CNu = 020;
361231140321          I33Num = 1;
361232140321
361233140321          KPJBU = TRUL33DS;
361234140321          KNSIF = 'GAITRA201';
361235140321
361236140331          dow $finenum=*off   ;
361237140331             Trul33c(kpjba);
361238140321
361239140331             TRUL33DS = KPJBU;
361240140321
361241140331             if O33Err <> *zeros;
361242140331               Esito = '2';
361243140331               $finenum=*on;
361244140331               Messaggio = 'Errore in ' +
361245140331                           'reperimento contatore (TRUL33C)';
361246140331             else;
361247140331               w0070=o33nri;
361248140331               setll w0070 uncmd02l;
361249140331        // Se numero già presente nel file comodati ne cerco un altro
361250140331               if not %equal;
361251140331                  $finenum=*on;
361252140331               endif;
361253140331             endif;
361254140331          enddo;
361255140321
361256140321          endsr;
361257140324
361300140321
361302140324       //--------------------------------------------------------------
361303140324       //?Preparazione stringa SQL
361304140324       //--------------------------------------------------------------
361305140324       BEGSR sr_prepsql;
361306140324       clear wand;
361307140328       WrkStringaSql='select rrn(uncmd00f), uncmd00f.* from uncmd00f';
361308140324       // Richiesto cliente
361309140324       if v1ksc>0;
361310140324       wand='1';
361311140328          WrkStringaSql=WrkStringaSql+ ' where ' +
361312140326          ' CMdksc=' + %editc(v1ksc:'X');
361313140324       endif;
361314140324       // Richieste note
361315140324       if v1not<>*blanks;
361316140324          if wand = ' ';
361317140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361318140327             ' CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
361319140324             wand='1';
361320140324          else;
361321140324             WrkStringaSql=WrkStringaSql+
361322140327             ' and CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
361323140324          endif;
361324140324       endif;
361325140324       // Flag gestione Contratto
361326140324       if v1cfg <> 'T';
361327140324          if wand = ' ';
361328140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361329140324             ' cmdcfg =' + '''' + v1cfg + '''';
361330140324             wand='1';
361331140324          else;
361332140324             WrkStringaSql=WrkStringaSql+
361333140324             ' and cmdcfg =' + '''' + v1cfg + '''';
361334140324          endif;
361335140324       endif;
361336140324       // Data stampa  Contratto dal/al
361337140327       if w1dsd> 0 or w1dsa>0;
361338140327        select;
361339140327       // immesse entrambe
361340140327        when w1dsd>0 and w1dsa>0;
361341140327          if wand = ' ' ;
361342140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361343140327             ' cmdcds between';
361344140327             wand='1';
361345140327          else;
361346140327             WrkStringaSql=WrkStringaSql+
361347140327             ' and cmdcds between';
361348140327          endif;
361349140327          WrkStringaSql=WrkStringaSql+
361350140327          ' ' + %editc(w1dsd:'X') + ' and ' + %editc(w1dsa:'X');
361351140327       // immessa solo la data dal
361352140327        when w1dsd>0;
361353140327          if wand = ' ' ;
361354140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361355140327             ' cmdcds >=';
361356140327             wand='1';
361357140327          else;
361358140327             WrkStringaSql=WrkStringaSql+
361359140327             ' and cmdcds >=';
361360140327          endif;
361361140327          WrkStringaSql=WrkStringaSql+
361362140327          ' ' + %editc(w1dsd:'X') ;
361363140327       // immessa solo la data al
361364140327        when w1dsa>0;
361365140327          if wand = ' ' ;
361366140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361367140327             ' cmdcds <=';
361368140327             wand='1';
361369140327          else;
361370140327             WrkStringaSql=WrkStringaSql+
361371140327             ' and cmdcds <=';
361372140327          endif;
361373140327          WrkStringaSql=WrkStringaSql+
361374140327          ' ' + %editc(w1dsa:'X') ;
361375140327        endsl;
361376140324       endif;
361377140324       // Data Immissione dal/al
361378140324       if w1imd> 0 or w1ima>0;
361379140327        select;
361380140327        when w1imd>0 and w1ima>0;
361381140327       // immesse entrambe
361382140324          if wand = ' ' ;
361383140328             WrkStringaSql=WrkStringaSql+  ' where ' +
361384140324             ' cmddim between';
361385140324             wand='1';
361386140324          else;
361387140324             WrkStringaSql=WrkStringaSql+
361388140324             ' and cmddim between';
361389140324          endif;
361390140324          WrkStringaSql=WrkStringaSql+
361391140327          ' ' + %editc(w1imd:'X') + ' and ' + %editc(w1ima:'X');
361392140327       // immessa solo datA dal
361393140327        when w1imd>0            ;
361394140327          if wand = ' ' ;
361395140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361396140327             ' cmddim >=';
361397140327             wand='1';
361398140327          else;
361399140327             WrkStringaSql=WrkStringaSql+
361400140327             ' and cmddim >=';
361401140327          endif;
361402140327          WrkStringaSql=WrkStringaSql+
361403140327          ' ' + %editc(w1imd:'X') ;
361404140327       // immessa solo datA al
361405140327        when w1ima>0            ;
361406140327          if wand = ' ' ;
361407140328             WrkStringaSql=WrkStringaSql+ ' where ' +
361408140327             ' cmddim <=';
361409140327             wand='1';
361410140327          else;
361411140327             WrkStringaSql=WrkStringaSql+
361412140327             ' and cmddim <=';
361413140327          endif;
361414140327          WrkStringaSql=WrkStringaSql+
361415140327          ' ' + %editc(w1ima:'X') ;
361416140327        endsl;
361417140324       endif;
361418140324       endsr;
361419140324       //--------------------------------------------------------------
361420140324       //?Decodifica flag gestione contratto
361421140324       //--------------------------------------------------------------
361422140324       BEGSR Decocfg;
361423140324         select;
361424140324         when vsccfg=' ';
361425140324            vsccfgd='Da Ricevere';
361426140324         when vsccfg='F';
361427140324            vsccfgd='Firmato    ';
361428140324         when vsccfg='A';
361429140324            vsccfgd='Annullato  ';
361430140324         when vsccfg='C';
361431140324            vsccfgd='Chiuso     ';
361432140324         endsl;
361433140325       endsr;
361434140325       //--------------------------------------------------------------
361435140325       //?controllo riga di sfl
361436140325       //--------------------------------------------------------------
361437140325       BEGSR ctrrecsfl;
361438140328
361439140328          errtip=*off;
361440140328          errcfg=*off;
361441140328
361442140328       // TIPO/MODELLO
361443140328       // Interrogazione tabella  UNR
361444140328       if %scan('?':vsctip)>0;
361445140328          clear vsctip;
361446140328          clear vsctipd;
361447140328          exsr sr_rictip;
361448140328          vsctip=wtip;
361449140328          vsctipd=wtipd;
361450140328          ErrGenerico=*on;
361451140328          exsr aggiosfl;
361452140328          leavesr;
361453140328       endif;
361454140328       // Controllo validità del codice inserito
361455140328         if vsctip = *blanks;
361456140328       //    Obbligatorio
361457140328              Errtip=*on;
361458140328              ErrGenerico=*on;
361459140328              ErrMessage =*on  ;
361460140328              v1cmsg=msg(1);
361461140328              exsr aggiosfl;
361462140328              leavesr;
361463140328         endif;
361464140328         clear tibs02ds  ;
361465140328         t02tla=' '      ;
361466140328         t02mod='C'      ;
361467140328         t02sif=knsif    ;
361468140328         t02cod='UNR'    ;
361469140328         t02ke1=vsctip    ;
361470140328         TNTBE_RicercaControllo  (kpjba : tibs02ds);
361471140328
361472140328       // Errato
361473140328         if  t02err<>*blanks ;
361474140328            ErrGenerico=*on  ;
361475140328            ErrMessage =*on  ;
361476140328            ErrTIP     =*on  ;
361477140328            VsctipD=*all'?'   ;
361478140328            v1cmsg=msg(3);
361479140328            exsr aggiosfl;
361480140328            leavesr;
361481140328         else;
361482140328            VscTIPD=T02UNI;
361483140328         endif;
361484140328       // Errore se già presente altro record comodato
361485140328       // con il cliente/data immissione/tipo/mod dati
361486140328         exsr sr_altrocomod;
361487140328         if errGenerico = *on;
361488140328               leavesr;
361489140328         endif;
361499140328       // Flag gestione contratto + Decodifica
361500140328         clear vsccfgd;
361501140328         if vsccfg <> ' ' and
361502140328            vsccfg <> 'F' and
361503140328            vsccfg <> 'A' and
361504140328            vsccfg <> 'C'     ;
361505140328            ErrGenerico=*on  ;
361506140328            ErrMessage =*on  ;
361507140328            Errcfg     =*on  ;
361508140328            v1cmsg=msg(10);
361509140328            exsr aggiosfl;
361510140328            leavesr;
361511140328         endif;
361512140328         exsr decocfg;
361513140328
361514140328         exsr aggiosfl;
361515140325       endsr;
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
376291140328               update uncmdfis;
376292140328            endif;
376293140328         endif;
376294140328
376295140328       readc    sduNs02    ;
376296140328 0     enddo         ;
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
376313140325       //--------------------------------------------------------------
376314080206       //?Operazioni finali.
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
383402140327Nessun dato da visualizzare per le selezioni richieste                          08
383403140328Modifica al momento non eseguibile: record vincolato da altro lavoro            09
383404140328Valori validi: " "=Da ricevere, "F"=Firmato, "C"=Chiuso, "A"=Annullato          10
383500100824Scorrimento non più consentito                                                  09
383600140328Premere F6=Conferma per non perdere le variazioni eseguite                      12
383800100908Indicare anche dtaImmissione o dtaConf. o dati Da Confermare o unità da Rendere 12
384000140207Tipo unità "99" non accettato per "FOR" rientri fornitori o sede                14
384100101112La quantità maggiore di 1 è da immettere solo per tipo unità 99                 15
384200100826Aumento/Sostituzione unità indicabile non per tipo "99"                         16
384300100826L'unità di RESO è indicabile solo se prevista la sostituizione dell'unità       17
384400100826Tipo unità di reso errata                                                       18
384500100826Descrizione obbligatoria                                                        19
384600100930Per tipi"48 e 49"indicare Cliente destinatario unità o digita"SCORTA" nelleNOTE 20
384700100826Codice cliente errato                                                           21
384800100924La data conferma movimento non può essere maggiore di 3 mesi circa...           22
384900130321Esiste già un invio/rientro giornaliero da confermare x questo tipo/matricola   23
385000100826Tipo/matricola già inseriti in questa pagina di lavoro!!                        24
385200100830Opzione non ammessa                                                             26
385300100831Nessun dato da visualizzare                                                     27
385400100831Indicare se Includere o Escludere il tipo unità indicato                        28
385500100831Impossibile annullare: record già confermato                                    29
385600121130Impostare data conferma movimenti                                               30
385700121130Data conferma non inferiore a oggi                                              31
385800100831In fase di conferma movimenti obbligatorio inserire la matricola                32
385900100909Numero e data documento obbligatori se RIENTRO da fornitore                     33
386000100909Per interrogare i movimenti è necessaria la matricola                           34
386100100923Matricola errata perchè di unità diversa da quella immessa e NON è modificabile 35
386200100901Tasto funzionale non ammesso in Annullamento                                    36
386300100907Opz non utilizzabile:pgm "                    "  in uso da xxxxxxxxxx il xx/xx  37
386400100903Magazzino di Destinazione e Correlato non possone essere uguali                 38
386500100908Indicare se invio o rientro da fornitore o filiale I/R/F                        39
386600100908Data AL non può essere minore data DAL                                          40
386700100908Immettere o i dati da confermare o la data conferma dal/al                      41
386800100920Il magazzino DESTINAZIONE deve essere XXX                                       42
386900101006Per opzione"9"-->Stampa Etichetta,premere tasto F10 e selezionare la stampante  43
387000100923Se immessa la matricola di reso immettere anche il TIPO unità di reso           44
387100101005Mag.Ubicazione matricola di RESO  non  uguale al mag.di INVIO. Enter forza      45
387200100923Tipo invio / rientro obbligatorio per "FOR"                                     46
387300100927Se Esclusione di tipo unità, non indicare la matricola                          47
387400100930Tasto funz. F07 abilitato solo per la ricerca matricole di RESO: posizionarsi!! 48
387500100930Matricola con tipo unità diverso da quello indicato                             49
387600101001Immettere in numero degli inserimenti da effettuare                             50
387700101001Per inserimento multiplo, immettere almeno un dato                              51
387800101012F8-Materiale preparato ammesso solo per INVII                                   52
387900120525Per opzioni: P=Materiale preparato o B=blocca stampa bolla, premere F8          53
388000101012Mancano colli e peso per il magazzino destinazione indicato                     54
388100111024Se unità in proprietà alla 923 magazzino destinazione non può essere "990 980"  55
388200110404Immettere "S" = Si per indicare il guasto fisico o lasciare vuoto               56
388300110412Nuova matricola errata perchè già esistente !!                                  57
388400110412Immettere una matricola di massino 15 caratteri                                 58
388500110610Campo note obbligatorio se guasto fisico                                        59
388600120524La stampa bolla non viene eseguita per rientro fornitore diretto in filiale     60
388700120524F15 non ammesso: il magazzino non prevede la stampa                             61
388800120525Impossibile bloccare la stampa bolla: già stampata!!                            62
388900121130Data conferma movimento no > di 3 mesi                                          63
389000121203Immettere anche data DAL per richiesta movimenti CONFERMATI                     64
389100121203Massimo un mese e possibile richiedere per i movimenti CONFERMATI               65
389200121203Impossibile modificare la data a movimenti già confermati                       66
389300130116Il magazzino di destinazione non corrisponde alla ditta manutenzione nell'anagr.67
389400130626Se unità NON in proprietà alla 923 magazzino destinazione non può essere "923"  68
389500140207La conferma dei movimenti può essere fatta solo da un utente EDP                69
