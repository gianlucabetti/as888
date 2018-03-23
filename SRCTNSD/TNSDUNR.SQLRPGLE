000100080206      //--------------------------------------------------------------
000200140318      //?TNSDUNR - Gestione Comodati
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100140326     FUNcmd00F  uf a E             DISK    rename(uncmd000:uncmdfis)
001200140328     FUNcmd01l  iF   E           k DISK    infds(cmd01)
001300140404     f                                     prefix(c1_)
001400140324     FUNcmd02l  iF   E           k DISK    rename(uncmd000:uncmd02)
001500140331     f                                     infds(cmd02)
001600140410     FUNana10l  iF   E           k DISK
001700140318     fTNSDUND   cf   e             workstn indds(IndDspF)
001800080206     f                                     infds(InfDspF)
001900140319     f                                     sfile(SDUNS02 : S02nrr)
002000100312
002100080206      //---------------------------------------------------------------
002200100824      // - Numero di record in ciascuna videata di subfile
002300100827     d c_SflPag        c                   const(16)
002400100827     d W_SflPag        s              3  0 inz
002500100824     d PAgric          s              3  0 inz
002600100824     d totrig          s              5  0 inz
002700100824
002800080207      // - Tasti funzionali a video
002900080207     d c_F01           c                   const(x'31')
003000080207     d c_F02           c                   const(x'32')
003100080207     d c_F03           c                   const(x'33')
003200080207     d c_F05           c                   const(x'35')
003300080207     d c_F06           c                   const(x'36')
003400080207     d c_F07           c                   const(x'37')
003500080207     d c_F08           c                   const(x'38')
003600080207     d c_F09           c                   const(x'39')
003700080207     d c_F10           c                   const(x'3A')
003800101018     d c_F11           c                   const(x'3B')
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
005500080214
005600080214      // - Attributi di visualizzazione
005700080215      //  -  DspAtr() - Normale
005800080214     d C_dspatr_Norm   c                   const(x'20')
005900080215      //  -  DspAtr(RI)
006000080214     d C_dspatr_RI     c                   const(x'21')
006100080215      //  -  DspAtr(ND)
006200080214     d C_dspatr_ND     c                   const(x'27')
006300080215      //  -  DspAtr(BL) / Color(Red)
006400080214     d C_dspatr_BL     c                   const(x'28')
006500080206
006600150922     D digits          c                   '0123456789'
006700080206      //---------------------------------------------------------------
006800080206      //?Definizione schiere.
006900080206      //---------------------------------------------------------------
007000080206
007100080206      // - Messaggi di errore
007200140207     d MSG             s             78    dim(69) ctdata perrcd(1)
007300100920     d
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione aree dati.
007600080206      //---------------------------------------------------------------
007700140328     D cmd01           DS
007800140328     D  cmd1NRR              397    400B 0
007900140331     D cmd02           DS
008000140331     D  cmd2NRR              397    400B 0
008100080206      // - Dati utente
008200080206     d §AzUte        e ds                  extname(AZUTE00F)
008300080206     d                                     dtaara
008400080206     d §DatiUte      e ds                  extname(dDatiUte)
008500080206     d                                     dtaara
008600140319     d TIBS69DS      E DS                  INZ
008700140319     d DS_cnaco      E DS                  extname(CNACO00F)
008800140319     d DS_cnind      E DS                  extname(CNIND00F)
008900140319     d DS_cnclp      E DS                  extname(CNCLP00F)
009000140319     d DS_fncls      E DS                  extname(FNCLS00F)
009100140324
009200140324     d DS_uncmd      E DS                  extname(UNCMD00F)
009300080206
009400080206      //---------------------------------------------------------------
009500080206      //?Definizione strutture dati.
009600080206      //---------------------------------------------------------------
009700100826
009800080206      // - Status
009900080206     d Psds           sds
010000080206     d   SDSpgm          *proc
010100080206
010200080206      // - InfDS
010300080206     d InfDspF         ds
010400080207     d  dsp_aid              369    369a                                        AID byte
010500100923     d  cursor               370    371a                                        AID byte
010600080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
010700080207     d  min_nrr              378    379i 0                                      Subfile min rrn
010800080207     d  num_rcds             380    381i 0                                      Subfile num rcds
010900080206
011000080206      // - Indicatori su DspF
011100080208     d IndDspF         ds
011200080206        // - Indicatori di gestione del subfile
011300080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
011400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
011500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
011700080206        // - Indicatori di errore
011800080206     d  errMessage                    1n   overlay(IndDspF : 28)
011900080606     d  errGenerico                   1n   overlay(IndDspF : 99)
012000080627        // - Indicatori vari
012100100830     d  EmisMod1                      1n   overlay(IndDspF : 02)
012200140402     d  AbilF6                        1n   overlay(IndDspF : 04)
012300140428     d  AbilF11                       1n   overlay(IndDspF : 89)
012400140402     d  IntPrNd                       1n   overlay(IndDspF : 05)
012500140319     d  IntManPr                      1n   overlay(IndDspF : 06)
012600140319     d  MANPrNd                       1n   overlay(IndDspF : 07)
012700140319     d  NewNd                         1n   overlay(IndDspF : 08)
012800140320     d  AbilAdd                       1n   overlay(IndDspF : 09)
012900140402     d  NdCnn                         1n   overlay(IndDspF : 18)
013000140403     d  ProtSfl                       1n   overlay(IndDspF : 19)
013100140319
013200100315     d
013300140321     d  Errksc                        1n   overlay(IndDspF : 40)
013400140321     d  Errtip                        1n   overlay(IndDspF : 41)
013500140321     d  Errqta                        1n   overlay(IndDspF : 42)
013600140321     d  Errcds                        1n   overlay(IndDspF : 43)
013700140324     d  Errcfg                        1n   overlay(IndDspF : 44)
013800140324     d  Errdsd                        1n   overlay(IndDspF : 45)
013900140324     d  Errdsa                        1n   overlay(IndDspF : 46)
014000140324     d  Errimd                        1n   overlay(IndDspF : 47)
014100140324     d  Errima                        1n   overlay(IndDspF : 48)
014200140324     d  Errcnn                        1n   overlay(IndDspF : 49)
014300140410     d  Erropz                        1n   overlay(IndDspF : 50)
014400140508     d  Errcdss                       1n   overlay(IndDspF : 51)
014500080206
014600080206      // - Parametri ricevuti
014700080206     d KPJBA         e ds
014800100923     d og143         e ds
014900080206
015000080206      // - Reperimento dati utente
015100080206     d TIBS34ds      e ds
015200080206     d dUte01        e ds
015300080702     d DLAT          e ds
015400080702
015500140318     d DUNR          e ds
015600140319     d DSUNIT        e ds
015700080206      // - Ricerca/Controllo tabelle
015800080206     d TIBS02ds      e ds                  inz
015900140321     D TRUL33DS      E DS
016000150918
016100150918     D ycostp2ds     E DS
016200150918     D ycostp0i      E DS
016300100907
016400100316
016500100316     d wlbdat          ds                  inz
016600100316     d  g02dat                 1      8  0
016700100316     d  g02inv                 9     16  0
016800100316     d  g02err                17     17
016900100316     d  g02tgi                18     22  0
017000121130     d
017100140403     Dtrul33R          pr                  extpgm('TRUL33R')
017200140321     D kpjba                               likeds(kpjba)
017300140428     Dtnsduir          pr                  extpgm('TNSDUIR')
017400140328     D kpjba                               likeds(kpjba)
017500140328     D parksc                         7  0
017600150918     Dycostp2r         pr                  extpgm('YCOSTP2R')
017700150918     D kpjba                               likeds(kpjba)
017800150918     D ycostp0i                            likeds(ycostp0i)
017900121130
018000100826
018100100827      //  ricerca alfabetica
018200100827     d  xpardut        s             30
018300100827     d  xparkut        s              1  0
018400100827     d  xparrag        s             48
018500100827     d  xparkcc        s              4  0
018600100827     d  xparsta        s              1  0
018700100827     d  xparflr        s             90
018800100827     d  xpardit        s              3
018900100827     d  xparnum        s              2  0
019000100827     d  xparkcm        s             80
019100100827     d  xparksm        s            140
019200100827     d  xparkdm        s             60
019300100827     d  xparesci       s              1
019400100827     d  xparerr        s              2
019500100827     d  xpariva        s             16
019600100827     d  xparcdf        s             16
019700100826
019800100827      // ricerca filiale su organigramma
019900100827     d   pInFIL        S              3
020000100827     d   pInFAG        S              1
020100100827     d   pInDES        S             25
020200100827     d   pInDIT        S              3
020300100316
020400080206      //---------------------------------------------------------------
020500080206      //?Definizione variabili globali.
020600080206      //---------------------------------------------------------------
020700080206
020800080206      // - Flags booleani
020900080208     d $Fine           s               n   inz(*off)
021000100312     d $InzS02         s               n   inz(*on)
021100100312     d $Inzd01         s               n   inz(*on)
021200140325     d $Finerec        s               n   inz(*off)
021300140328     d werrsfl         s               n   inz(*off)
021400140331     d $Finenum        s               n   inz(*off)
021500140402     d $Finew          s               n   inz(*off)
021600080206
021700100312     d $Video          s              2    inz('D1')
021800100315     d S02nrr          s              4  0 inz
021900081006     d XX              s              3  0 inz
022000080627     d Indx            s              3  0 inz
022100081126     d yy              s              3  0 inz
022200121203     d wdiffe          s              8  0 inz
022300140321     d w3dim           s                   like(v3dim)
022400140321     d w3cds           s                   like(v3cds)
022500140324     d w1dsd           s                   like(v1dsd)
022600140324     d w1dsa           s                   like(v1dsa)
022700140325     d w1imd           s                   like(v1dimd)
022800140325     d w1ima           s                   like(v1dima)
022900140331     d w0070           s              7  0
023000140324     d wand            s              1
023100140321     D Esito           s              1a
023200140327     d waddrcd         s              1a
023300150924     d wkcc            s              6
023400140328     d wtip            s                   like(vsctip)
023500140328     d wtipd           s                   like(vsctipd)
023600140328     d wopz            s              1a
023700140328     d s02rcd          s                   like(c02rcd)
023800140321     dMessaggio        s             50
023900121203     d w1cdim          s              8  0 inz
024000100831     d w1cdtm          s              8  0 inz
024100100908     d w1cdtmd         s              8  0 inz
024200100908     d w1cdtma         s              8  0 inz
024300121203     d w1ctgid         s                   like(g02tgi)
024400121203     d w1ctgia         s                   like(g02tgi)
024500100831     d Dataoggi        s              8  0 inz
024600140404     d wtrovcnn        s              1a
024700140410     d w1cfg           s                   like(v1cfg)
024800140410     d preksc          s                   like(v3ksc)
024900140410     d wtrov_un        s              1
025000101012     d wrkgetlista     s           4096    varying
025100080414
025200100315     d Dataiso         s               d   datfmt(*iso)
025300140321     d Dataisocor      s               d   datfmt(*iso)
025400140402     d Datadmy         s               d   datfmt(*dmy)
025500140324
025600140324     D WrkStringaSql   S           4500
025700140324     D                                     VARYING
025800140325     D rrncmd          s              9s 0
025900150922     D wunrimp         s                   like(d§unrimp)
026000150923     D wksc            s              8  0
026100080604     d
026200080605     d                 DS
026300080605     d  AA                     1      4  0
026400080605     d  MM                     5      6  0
026500080605     d  GG                     7      8  0
026600080605     d DATA                    1      8  0
026700140428
026800140428      * - DS Parametri per ricerca Programmi chiamanti
026900140428     D DSStack         ds
027000140428     D  £Stack                       10    Dim(100)
027100081009     d
027200080206      //---------------------------------------------------------------
027300080206      //?Definizione procedure usate.
027400080206      //---------------------------------------------------------------
027500080414      /copy gaitrasrc/srcprotopr,tibs02r
027600080414      /copy gaitrasrc/srcprotopr,tibs34r
027700100316      /copy gaitrasrc/srcprotopr,xsrda8
027800100826      /copy gaitrasrc/srcprotopr,xalfa3br
027900100920      /copy gaitrasrc/srcprotopr,qcmdexc
028000140319      /copy gaitrasrc/srcprotopr,tibs69r
028100080206
028200080206      //---------------------------------------------------------------
028300080206      //?Riepilogo indicatori.
028400080206      //---------------------------------------------------------------
028500080207      // - Comuni
028600080207      // 28    : Emissione messaggio di errore a video
028700080207      // - C01/S01
028800080207      // 30    : Condiziona SFLDSP    (*not)
028900080207      // 31    : Condiziona SFLDSPCTL (*not)
029000080207      // 30+31 : Condiziona SFLCLR
029100100824      // 32    : Condiziona SFLNXTCHG
029200080207      // 50    : Errore: Opzione errata
029300080207      // - D01
029400080207      // - Comuni
029500080207      // 99    : Generico di Errore
029600080206      //---------------------------------------------------------------
029700080206
029800080206      //---------------------------------------------------------------
029900080206      //?M A I N - L I N E
030000080206      //---------------------------------------------------------------
030100080206
030200080627     c     *Entry        plist
030300080206     c                   parm                    KPJBA
030400080702
030500080206      /free
030600140324         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
030700080206
030800080206       // Operazioni iniziali
030900080206       exsr RoutInz;
031000081215
031100080206       // Gestione video
031200081215 3     DOW $Fine = *off;
031300081215 4       select;
031400080530
031500080530         // Videata di selezioni
031600100312           when $Video = 'D1';
031700100312             exsr GesD01;
031800100830         // Videata del sfl per inserimento/visualizzazione
031900140318           when $Video = 'S2';
032000100312             exsr GesS02;
032100140325         // Videata singolo record
032200140325           when $Video = 'D3';
032300140325             exsr Gesd03;
032400150921             if dsp_aid = c_F12 or dsp_aid = c_F06 ;
032500140325                $video='D1';
032600140325             endif;
032700100830
032800080702           other   ;
032900080206             $Fine = *on;
033000081215 4       endsl;
033100081215 3     ENDDO;
033200080206
033300080206       // Operazioni finali
033400080206       exsr RoutEnd;
033500080206
033600080206       //--------------------------------------------------------------
033700080206       //?Operazioni iniziali.
033800080206       //--------------------------------------------------------------
033900080206       BEGSR RoutInz;
034000100312       $inzs02=*on;
034100100312       $inzd01=*on;
034200080206
034300080206         // Reperimento dati job
034400080206         exsr DatiJob;
034500140321         vscpgm='TNSDUNR'         ;
034600140321         // Data corrente
034700140321         dataisocor=%date();
034800100826
034900100827
035000100830       emisMod1=*on   ;
035100140318       v1dmod=*blanks;
035200100831
035300140428      /end-free
035400140428      * reperisco i programmi che hanno richiamato questo pgm
035500140428     c                   Call      'XRTVSTKR'
035600140428     C                   Parm                    PStack         1000
035700140428     C                   Parm                    PStartPoint       3 0
035800140428     C                   Parm                    PEntries          3 0
035900140428     c                   Movel     Pstack        DSstack
036000140428      /free
036100140428       //distattivo f11= Gestione comodati unità edp per evitare chiamata ricorsiva
036200140428         if %lookup ('TNSDUIR   ':£Stack)>0;
036300140428            abilf11=*on;
036400140428         endif;
036500080703
036600150924       // cerco capoconto contabile
036700150924         t02tla=' '      ;
036800150924         t02mod='C'      ;
036900150924         t02sif=knsif    ;
037000150924         t02cod='RCE'    ;
037100150924         t02ke1='STP'    ;
037200150924         t02ke2='CAPODARE' ;
037300150924         TNTBE_RicercaControllo  (kpjba : tibs02ds);
037400150924
037500150924         if  t02err=*blanks ;
037600150924         eval  wkcc=t02uni  ;
037700150924         else   ;
037800150924         eval wkcc='008888' ;
037900150924         endif ;
038000080627         ENDSR;
038100080206
038200080206       //--------------------------------------------------------------
038300080206       //?Reperimento Dati del job (Utente/Operativi).
038400080206       //--------------------------------------------------------------
038500080206       BEGSR DatiJob;
038600080206
038700080206         in(E) §AzUte;
038800080206         if NOT %error;
038900080206           in(E) §DatiUte;
039000080206         endif;
039100080206         if %error or RSut = *blanks;
039200080206           clear TIBS34ds;
039300080206           tibs34r(tibs34ds);
039400080206           in §AzUte;
039500080206           in §DatiUte;
039600080206         endif;
039700080206
039800080206       ENDSR;
039900100312       //--------------------------------------------------------------
040000100312       //?Gestione viodeata 01
040100100312       //--------------------------------------------------------------
040200100312       BEGSR Gesd01;
040300100312
040400100312         // Inizializzazione videata
040500100312         if  $Inzd01 = *on;
040600100312            exsr Inzd01;
040700100312            $Inzd01  = *off;
040800100312         endif;
040900100312
041000100312
041100100312         // Emissione Testata e Piede con tasti funzionali abilitati
041200150921         //if  errMessage  = *off;
041300140318           write  SDUNT01;
041400150921         //endif;
041500100312
041600100312         // Emissione videata
041700140318         exfmt  SDUND01;
041800100312
041900100312         reset errMessage;
042000100312         reset errGenerico;
042100100824         clear V1cmsg    ;
042200100312
042300100312 1       SELECT;
042400100312
042500100312         // - F3=Fine
042600100312 1         WHEN  dsp_aid = c_F03;
042700100312            $Fine = *on;
042800140318         // - F10=Nuovo Comodato
042900140319 1         WHEN  dsp_aid = c_F10;
043000140318            $video='D3' ;
043100100312
043200100824 x1      // Invio
043300100312           OTHER;
043400100316
043500100316           exsr CTRD01  ;
043600100316
043700100316           if ErrGenerico=*ON    ;
043800100316                 leavesr;
043900100316 2             endif;
044000140324       // Se immesso numero contratto emetto singola videata
044100140404       //  if v1cnn>0;
044200140404       //   $video='D3' ;
044300140404       //   vscopz='2';
044400140404       //   vshnrr=cmd2NRR;
044500140404       //  else ;
044600140324       // Altrimenti carico il sfl
044700100315           $video='S2' ;
044800140318           $inzs02=*on;
044900140404       //  endif;
045000100824
045100100312 1       ENDSL;
045200100312
045300100312       ENDSR;
045400100312
045500100316       //--------------------------------------------------------------
045600100316       //?controlli video 1
045700100316       //--------------------------------------------------------------
045800100316       BEGSR CTRD01  ;
045900140324
046000140324       errksc=*off;
046100140324       errdsd=*off;
046200140324       errdsa=*off;
046300140324       errimd=*off;
046400140324       errima=*off;
046500140324       errcnn=*off;
046600100316
046700140324       // CLIENTE
046800140324       // Ricerca alfabetica
046900140324       if v1ksc=*zeros and v1kscd<>*blanks;
047000140324          exsr sr_ricalfa;
047100140324          ErrGenerico=*on;
047200140324          leavesr;
047300140324       endif;
047400140324       // Controllo codice immesso
047500140324       if v1ksc>*zeros;
047600140324           clear DS_cnaco;
047700140324           clear DS_cnind;
047800140324           clear DS_cnclp;
047900140324           clear DS_fncls;
048000140324           clear v1kscd;
048100140324           I69kac=v1ksc  ;
048200140324           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
048300140324           if o69err=*blanks;
048400140324              v1kscd=acorag;
048500140324           else;
048600140324              Errksc=*on;
048700140324              ErrGenerico=*on;
048800140324              ErrMessage =*on  ;
048900140324              v1cmsg=msg(2);
049000140324              leavesr;
049100140324           endif;
049200140410           chain v1ksc uncmd01l;
049300140410           if not %found(uncmd01l);
049400140410              Errksc=*on;
049500140410              ErrGenerico=*on;
049600140410              ErrMessage =*on  ;
049700140410              v1cmsg=msg(8);
049800140410              leavesr;
049900140410           endif;
050000140324       endif;
050100140324       // NUMERO CONTRATTO
050200140324       if v1cnn>0;
050300140324          chain (v1cnn) uncmd02l;
050400140324          if not %found(uncmd02l);
050500140324              Errcnn=*on;
050600140324              ErrGenerico=*on;
050700140324              ErrMessage =*on  ;
050800140324              v1cmsg=msg(7);
050900140324              leavesr;
051000140324          endif;
051100140324       endif;
051200140324       // DATA STAMPA CONTRATTO DAL - AL
051300140328       clear w1dsd;
051400140328       clear w1dsa;
051500140324       // Data Dal
051600140324       if v1dsd>*zeros;
051700140324             clear wlbdat;
051800140324             g02dat = v1dsd  ;
051900140324             xsrda8(wlbdat);
052000140324             if g02err = '1';
052100140324               errMessage  = *on;
052200140324               errGenerico = *on;
052300140324               Errdsd    = *on;
052400140324               V1cmsg = Msg(04);
052500140324               leavesr;
052600140324             endif;
052700140324             w1dsd=g02inv;
052800140324       endif          ;
052900140324       // Data Al
053000140324       if v1dsa>*zeros;
053100140324             clear wlbdat;
053200140324             g02dat = v1dsa  ;
053300140324             xsrda8(wlbdat);
053400140324             if g02err = '1';
053500140324               errMessage  = *on;
053600140324               errGenerico = *on;
053700140324               Errdsd    = *on;
053800140324               V1cmsg = Msg(04);
053900140324               leavesr;
054000140324             endif;
054100140324             w1dsd=g02inv;
054200140324       endif          ;
054300140324       // Data dal no maggiore di data al
054400140324       if w1dsd>0 and w1dsa>0 and w1dsd>w1dsa;
054500140324               errMessage  = *on;
054600140324               errGenerico = *on;
054700140324               Errdsd    = *on;
054800140324               V1cmsg = Msg(06);
054900140324               leavesr;
055000140324       endif;
055100140324       // DATA IMMISSIONE       DAL - AL
055200140328       clear w1imd;
055300140328       clear w1ima;
055400140324       // Data Dal
055500140325       if v1dimd>*zeros;
055600140324             clear wlbdat;
055700140325             g02dat = v1dimd  ;
055800140324             xsrda8(wlbdat);
055900140324             if g02err = '1';
056000140324               errMessage  = *on;
056100140324               errGenerico = *on;
056200140324               Errdsd    = *on;
056300140324               V1cmsg = Msg(04);
056400140324               leavesr;
056500140324             endif;
056600140324             w1imd=g02inv;
056700140324       endif          ;
056800140324       // Data Al
056900140325       if v1dima>*zeros;
057000140324             clear wlbdat;
057100140325             g02dat = v1dima  ;
057200140324             xsrda8(wlbdat);
057300140324             if g02err = '1';
057400140324               errMessage  = *on;
057500140324               errGenerico = *on;
057600140324               Errdsd    = *on;
057700140324               V1cmsg = Msg(04);
057800140324               leavesr;
057900140324             endif;
058000140324             w1ima=g02inv;
058100140324       endif          ;
058200140324       // Data dal no maggiore di data al
058300140324       if w1imd>0 and w1ima>0 and w1imd>w1ima;
058400140324               errMessage  = *on;
058500140324               errGenerico = *on;
058600140324               Errdsd    = *on;
058700140324               V1cmsg = Msg(06);
058800140324               leavesr;
058900140324       endif;
059000100708
059100100316       ENDSR  ;
059200080206       //--------------------------------------------------------------
059300100312       //?Gestione SFL 02
059400080206       //--------------------------------------------------------------
059500100312       BEGSR GesS02;
059600080207
059700080207         // Inizializzazione videata
059800100312         if  $InzS02 = *on;
059900100312            exsr InzS02;
060000140328         // werrsfl = *off;
060100140325            c02csr=1     ;
060200140325            c02rcd=1     ;
060300100824            $InzS02  = *off;
060400100827
060500140324         // Se non trovati record errore in prima videata
060600140325            if s02nrr=0   ;
060700140325               v1cmsg=msg(08)  ;
060800140325               errGenerico=*on  ;
060900140325               ErrMessage=*on  ;
061000140325               $Video='D1'  ;
061100140325               leavesr   ;
061200140325            endif  ;
061300140324         // Se trovato un solo record emetto direttamente la videata singola
061400140325            if s02nrr=1;
061500140325              $Video='D3'  ;
061600140326              vscopz='2';
061700140325              leavesr;
061800140325            endif;
061900140325         endif;
062000100831
062100140328       // c02csr   contiene il numero di riga del subfile su cui era posizionato il cursore
062200140328       // impostando c02rcd   visualizzo la pagina che vedeva l'utente quando ha premuto
062300140328       // l'ultimo tasto
0624001403282         if c02csr   > 0;
062500140328           c02rcd   = c02csr  ;
062600140328          else;
062700140328           c02rcd   = s02rcd   ;
0628001403282         endif;
062900140328       // se non so quale pagina visualizzare forzo pagina 1
0630001403282         if c02rcd   < 1;
063100140328           eval c02rcd   = 1;
0632001403282         endif;
063300140328
063400140328       // salvo il record number del subfile
063500140508       // s02rcd    = c02rcd  ;
063600080207
063700080207         // Emissione Testata e Piede con tasti funzionali abilitati
063800140328       //if  errMessage  = *off;
063900140319           write  SDUNT01;
064000140328       //endif;
064100080703
064200080207         // Emissione videata
064300140325         write  sdunz02;
064400140319         exfmt  SDUNC02;
064500080410
064600080207         reset errMessage;
064700080207         reset errGenerico;
064800100824         clear V1cmsg;
064900100927         s02nrr=1  ;
065000080207
065100080523 1       SELECT;
065200080207
065300080207         // - F3=Fine
065400080523 1         WHEN  dsp_aid = c_F03;
065500140328         // if werrsfl = *on ;
065600140328         //    errmessage  = *on ;
065700140328         //    v1cmsg = msg(12);
065800140328         //    werrsfl = *off;
065900140328         // else;
066000140328               $Fine = *on;
066100140328         // endif;
066200080207
066300080207         // - F12=Ritorno
066400080523 1         WHEN  dsp_aid = c_F12;
066500140328         // if werrsfl = *on ;
066600140328         //    errmessage  = *on ;
066700140328         //    v1cmsg = msg(12);
066800140328         //    werrsfl = *off;
066900140328         // else;
067000140328               $video='D1'   ;
067100140328         // endif;
067200140328
067300140328         // - F5=Refresh
067400140328 1         WHEN  dsp_aid = c_F05;
067500140328         // if werrsfl = *on ;
067600140328         //    errmessage  = *on ;
067700140328         //    v1cmsg = msg(12);
067800140328          //   werrsfl = *off;
067900140328          //else;
068000140328               $inzs02 = *on ;
068100140328          //endif;
068200140327
068300140327         // - F10=Immissione
068400140327 1         WHEN  dsp_aid = c_F10;
068500140327           $video='D3'   ;
068600140327           exsr gesd03;
068700140327           $video = 'S2';
068800140327 1            if dsp_aid = c_F06;
068900140328         //       $inzs02='1';
069000140327              endif;
069100100824
069200100824         // - Roll-Up
069300100824           WHEN  dsp_aid = c_RollUp;
069400100901
069500100824
069600100901
069700080530 x1      // Invio / F6
069800080207           OTHER;
069900081006
070000140327               waddrcd='0';
070100140328               clear wopz;
070200100315               exsr ContrS02 ;
070300140328               // Se gestite opzioni riemetto il sfl
070400140328         //    if waddrcd='1';
070500140328         //       $inzs02=*on ;
070600140328               if wopz = '1';
070700140328                  errGenerico = *on;
070800140328                  leavesr;
070900140328               endif;
071000100930
071100140325 2             if  errGenerico = *on;
071200081006                 leavesr;
071300100824 2             endif;
071400080530
071500121130
071600100907         // F6=conferma Aggiornamento
071700140328           if   dsp_aid = c_F06 ;
071800140328              exsr ConfermaS02;
071900140328               if  errGenerico = *on;
072000140328                 leavesr;
072100140328               endif;
072200140327              $video='D1'   ;
072300080530           endif  ;
072400080207
072500080523 1       ENDSL;
072600080207
072700080207       ENDSR;
072800100901       //--------------------------------------------------------------
072900100901       //?chiamata pgm gestione unità EDP
073000100901       //--------------------------------------------------------------
073100140319       BEGSR CallIntCom  ;
073200100901
073300100901       clear dsunit  ;
073400140410       dsuop0=' 2'  ;
073500100901
073600100901       clear kpjbu  ;
073700100901       kpjbu=dsunit  ;
073800140428       TNSDUIR  (kpjba:v3ksc)          ;
073900100901
074000100901       ENDSR;
074100080207
074200100901
074300100901
074400100824       //--------------------------------------------------------------
074500100824       //?Caricamento pagina vuota
074600100824       //--------------------------------------------------------------
074700100824       BEGSR RollUpS02;
074800100824         S02nrr   = totrig ;
074900100824
075000100824
075100100824       ENDSR;
075200080526       //--------------------------------------------------------------
075300100312       //?controlli SFL02
075400080409       //--------------------------------------------------------------
075500100312       BEGSR ContrS02;
075600100920
075700100315       s02nrr=1  ;
075800140319       readc       sduns02    ;
075900080703
076000140328 0     dow not %eof                      ;
076100100830
076200140325       // Se immessa, controlla l'opzione, altrimenti controlla i dati variati
076300140410        if vscopz<>'A';
076400140410         erropz=*off;
076500140410          if vscopz<>*blanks                ;
076600140328            wopz='1';
076700140410       // Non è possibile annullare se presente contratto
076800140410           if vscopz='4' and vsccnn>0;
076900140410              Erropz=*on;
077000140410              v1cmsg=msg(13);
077100140410              exsr aggiosfl;
077200140410              leavesr;
077300140410           endif;
077400140325            $video='D3' ;
077500140325            exsr gesd03;
077600140327            $video='S2';
077700140325                clear vscopz;
077800140325                chain(n) vshnrr uncmd00f ;
077900140325                if %found(uncmd00f);
078000140327                   exsr carrecord;
078100140328                   exsr aggiosfl ;
078200140403                else;
078300140403                   vscopz='A';
078400140403                   protsfl=*on;
078500140403                   update  sduns02  ;
078600140325                endif ;
078700140325          else ;
078800140325                exsr ctrrecsfl;
078900140328                if errgenerico=*on;
079000140328                   leavesr;
079100140328                endif;
079200140325          endif;
079300140410        endif;
079400100920
079500140319       readc    sduNs02    ;
079600100830 0     enddo         ;
079700100830
079800100830       ENDSR  ;
079900080409
080000100312       //--------------------------------------------------------------
080100100312       //?Inizializzazione videata 01
080200100312       //--------------------------------------------------------------
080300100312       BEGSR Inzd01  ;
080400100312
080500100315         clear V1cmsg;
080600100830         EmisMOD1    = *on ;
080700100826         errMessage  = *off;
080800100312         errGenerico = *off;
080900140318       // codice cliente
081000140318         clear v1ksc ;
081100140318         clear v1kscd;
081200140318         clear v1not;
081300140318         clear v1cnn;
081400140410         v1cfg = 'D' ;
081500140318         clear v1dsd;
081600140318         clear v1dsa;
081700140318         clear v1dimd;
081800140318         clear v1dima;
081900100831
082000100312         ENDSR    ;
082100140319       //--------------------------------------------------------------
082200140319       //?Inizializzazione videata 03 per Immissione
082300140319       //--------------------------------------------------------------
082400140319       BEGSR Inzd03  ;
082500140319
082600140326         exsr inzd03uni;
082700140319         clear v3ksc ;
082800140319         clear v3kscd;
082900140508         if v1ksc>0;
083000140508            v3ksc=v1ksc;
083100140508            exsr sr_decoksc;
083200140508         endif;
083300140319
083400140319         ENDSR    ;
083500140319       //--------------------------------------------------------------
083600140319       //?Riempimento campi videata per gestione/interrogazione comodato
083700140319       //--------------------------------------------------------------
083800140319       BEGSR Ried03  ;
083900140319
084000140319         clear V1cmsg;
084100140319         EmisMOD1    = *off ;
084200140319         errMessage  = *off;
084300140319         errGenerico = *off;
084400140319       // Data immissione
084500140319         dataiso =  %date(cmddim);
084600140319         v3dim = %dec(dataiso:*eur) ;
084700140319       // Numero contratto
084800140319         v3cnn = cmdcnn;
084900140402       // Indicatore di ND su numero contratto e ragione sociale
085000140402         if cmdcnn>0;
085100140402            NdCnn = *off;
085200140402         else;
085300140402            NdCnn = *on ;
085400140402         endif;
085500140319       // Cliente e relativa decodifica
085600140319         v3ksc = cmdksc;
085700140508         exsr sr_decoksc;
085800140402       // Ragione sociale contratto
085900140402         v3rsc=cmdrsc;
086000140319
086100140319       // Tipo/modello e relativa decodifica
086200140319         v3tip =cmdtip;
086300140319         clear dunr;
086400140321         clear tibs02ds  ;
086500140321         t02tla=' '      ;
086600140321         t02mod='C'      ;
086700140321         t02sif=knsif    ;
086800140321         t02cod='UNR'    ;
086900140321         t02ke1=V3TIP    ;
087000140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
087100140321
087200140321         if  t02err=*blanks ;
087300140321            dunr=t02uni;
087400140319            v3tipd = D§UNRDES;
087500140321         ELSE;
087600140321            v3tipd = *ALL'?' ;
087700140319         endif;
087800140319         v3qta = cmdqta;
087900140319         clear v3cfs;
088000140319         if cmdcfs= 'S';
088100140319            v3cfs = cmdcfs;
088200140319         endif;
088300140325         clear v3cds;
088400140325         if cmdcds>0;
088500140325            dataiso =  %date(cmdcds);
088600140325            v3cds = %dec(dataiso:*eur) ;
088700140325         endif;
088800140319         clear v3cfi;
088900140319         if cmdcfi= 'S';
089000140325            v3cfi=cmdcfi;
089100140319         endif;
089200140319         v3cfg = cmdcfg;
089300140331         clear v3cdg;
089400140325         if cmdcdg>0;
089500140325            dataiso =  %date(cmdcdg);
089600140325            v3cdg = %dec(dataiso:*eur) ;
089700140325         endif;
089800140319         v3not = cmdnot;
089900150918         clear v3cace;
090000140319
090100140319         ENDSR    ;
090200140319       //--------------------------------------------------------------
090300140326       //?Inizializzazione videata 03 unica per immissione e aggiunta comodato x cli
090400140319       //--------------------------------------------------------------
090500140326       BEGSR inzd03uni;
090600140319
090700140319         clear V1cmsg;
090800140319         EmisMOD1    = *off ;
090900140319         errMessage  = *off;
091000140319         errGenerico = *off;
091100140319       // Data immissione = data del giorno
091200140326         v3dim = %dec(dataisocor:*eur) ;
091300140326         w3dim = %dec(dataisocor) ;
091400140319         clear v3tip ;
091500140319         clear v3tipd;
091600140319         clear v3qta;
091700140319         clear v3cfs;
091800140319         clear v3cds;
091900140319         clear v3cfi;
092000140319         clear v3cfg;
092100140319         clear v3cdg ;
092200140319         clear v3not ;
092300140402         clear v3cnn;
092400140402         clear v3rsc;
092500150918         v3cace='S' ;
092600140331         errksc=*off;
092700140331         errtip=*off;
092800140331         errqta=*off;
092900140331         errcds=*off;
093000140319
093100140319         ENDSR    ;
093200080207       //--------------------------------------------------------------
093300100312       //?Inizializzazione SFL02
093400080207       //--------------------------------------------------------------
093500100312       BEGSR InzS02;
093600080207
093700080207       // Pulizia subfile
093800080207         SflDsp_N    = *on;
093900080207         SflDspCtl_N = *on;
094000140319         write  SDUNC02;
094100080207         SflDspCtl_N = *off;
094200080207         SflEnd      = *off;
094300100824         SflDsp_N    = *off;
094400100824         Sflnxtchg   = *off;
094500140324
094600140326         exec sql close CMDcsr;
094700140411       // Se non richiesto numero contratto
094800140411         if v1cnn=0 ;
094900140324       // Richiamo routine di preparazione stringa SQL
095000140404             exsr sr_prepsql;
095100140404         endif;
095200140324
095300140324         S02nrr=0 ;
095400140324         clear V1cmsg;
095500140324         errMessage  = *off;
095600140324         errGenerico = *off;
095700080530
095800140411        if v1cnn=0            ;
095900140324       // Elaboro i records estratti
096000140404           $finerec=*off;
096100140404           exec sql prepare STRINGASQL from :WrkStringaSql;
096200140404           exec sql declare CMDCSR cursor for StringaSql;
096300140404           exec sql open CMDcsr;
096400140404           dow $finerec=*off;
096500140404              exec sql Fetch CMDcsr into :rrncmd, :ds_uncmd;
096600140404              if sqlcod=100 or sqlcod<0;
096700140404                 $finerec = *on;
096800140404                 leave;
096900140404              endif;
097000140324           // Carico record nel sfl
097100140404              exsr carsf02;
097200140404           enddo;
097300140404        else;
097400140410       // Caricamento sfl per numero contratto o per codice cliente
097500140404           setll v1cnn uncmd02l;
097600140404           reade v1cnn uncmd02l;
097700140404           dow not %eof;
097800140404              exsr carsf02;
097900140410                 reade v1cnn uncmd02l;
098000140404           enddo;
098100140404        endif;
098200140508          s02rcd    = s02nrr  ;
098300100823
098400100824         ENDSR  ;
098500140324       //--------------------------------------------------------------
098600140324       //?eseguo caricamento SFL
098700140324       //--------------------------------------------------------------
098800140324          BEGSR   carSF02    ;
098900140324          clear vscopz  ;
099000140324
099100140324          exsr CarRECORD  ;
099200140328       // Numero relativo di record  sul file comodati
099300140410       select;
099400140410       when v1cnn>0;
099500140410          vshnrr=cmd2NRR;
099600140410       other;
099700140410          vshnrr=rrncmd;
099800140410       endsl;
099900140324          errtip=*off;
100000140324          errcfg=*off;
100100140508          errcdss=*off;
100200140403          protsfl=*off;
100300140324          s02nrr=s02nrr+1   ;
100400140325          write  SDUnS02;
100500140324
100600140324          ENDSR         ;
100700140324       //--------------------------------------------------------------
100800140324       //?Caricamento record SFL
100900140324       //--------------------------------------------------------------
101000140324       BEGSR CarRecord;
101100140324       vscksc=cmdksc;
101200140324       // Decodifico il cliente
101300140324           clear DS_cnaco;
101400140324           clear DS_cnind;
101500140324           clear DS_cnclp;
101600140324           clear DS_fncls;
101700140325           clear vsckscd;
101800140324           I69kac=vscksc  ;
101900140324           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
102000140324           if o69err=*blanks;
102100140324              vsckscd=acorag;
102200140324           endif;
102300140324       // Tipo/modello + decodifica
102400140324         vsctip=cmdtip;
102500140325         exsr decotip;
102600140324       //Quantità
102700140324         vscqta=cmdqta;
102800140324       //Gestione contratto
102900140324         vsccfg=cmdcfg;
103000140324         clear vsccfgd;
103100140324         exsr decocfg;
103200140324       //Data gestione contratto
103300140402       //    clear wlbdat;
103400140402       //    g02inv = cmdcdg ;
103500140402       //    g02err ='3';
103600140402       //    xsrda8(wlbdat);
103700140402       //    vsccdg=g02dat;
103800140402             clear vsccdg;
103900140402             if cmdcdg>0;
104000140402                datadmy=%date(cmdcdg:*iso);
104100140402                vsccdg=%dec(datadmy);
104200140402             endif;
104300140324       //Flag stampa contratto
104400140324       if cmdcfs='S';
104500140324       vsccfs='Si';
104600140324       else;
104700140324       vsccfs='No';
104800140324       endif;
104900140324       //Data stampa contratto
105000140402       //    clear wlbdat;
105100140402       //    g02inv = cmdcds ;
105200140402       //    g02err ='3';
105300140402       //    xsrda8(wlbdat);
105400140402       //    vsccds=g02dat;
105500140402             clear vsccds;
105600140508             clear vshcds;
105700140402             if cmdcds>0;
105800140402                datadmy=%date(cmdcds:*iso);
105900140402                vsccds=%dec(datadmy);
106000140402             endif;
106100140324       //Flag invio a POC
106200140324       if cmdcfi='S';
106300140505         vsccfi='Si';
106400140324       else;
106500140402         vsccfi='  ';
106600140324       endif;
106700140324       //Data immissione
106800140402             vshdim=cmddim;
106900140402       //    clear wlbdat;
107000140402       //    g02inv = cmddim ;
107100140402       //    g02err ='3';
107200140402       //    xsrda8(wlbdat);
107300140402       //    vscdim=g02dat;
107400140402             clear vscdim;
107500140402             if cmddim>0;
107600140402                datadmy=%date(cmddim:*iso);
107700140402                vscdim=%dec(datadmy);
107800150921             endif;
107900140324       //numero contratto
108000140324       vsccnn=cmdcnn;
108100140324       //Note
108200140324       vscnot=cmdnot;
108300140324       ENDSR         ;
108400100830       //--------------------------------------------------------------
108500101001       //?Gestione videata 03
108600100830       //--------------------------------------------------------------
108700150921       BEGSR Gesd03;
108800100830
108900100831
109000100830
109100140319         // Controllo opzione - tasto funzionale F10=Inserimento
109200100830         Emismod1=*off ;
109300120524
109400100830 1       select   ;
109500140319         when dsp_aid = c_f10;
109600140402            v3dmod='  Immissione  '  ;
109700140319            Intprnd   =*off;
109800140402            AbilF6    =*on;
109900140319            IntManPr  =*off;
110000140319            ManPrNd   =*off;
110100140319            NewNd     =*on ;
110200140320            AbilAdd   = *off;
110300140402       // Attivo ND su numero contratto e ragione sociale contratto
110400140402            NdCnn     = *on;
110500140319            exsr inzd03;
110600140319         when vscopz='2'   ;
110700140319            v3dmod=' Manutenzione '  ;
110800140319            IntPrNd   =*off;
110900140402            AbilF6    =*on ;
111000140319            IntManPr  =*on;
111100140319            NewNd     =*off;
111200140320            AbilAdd   = *on;
111300140325            chain(e) vshnrr uncmd00f;
111400140325            if  %error or not %found;
111500140325               v1cmsg=msg(09)  ;
111600140325               ErrGenerico=*on  ;
111700140325               ErrMessage =*on  ;
111800140328               exsr aggiosfl;
111900140325               leavesr  ;
112000140325            else;
112100140331               if cmdcnn>0;
112200140331                 MANPrNd   =*on ;
112300140331               else;
112400140331                 MANPrNd   =*off;
112500140331               endif;
112600140325               exsr ried03;
112700140325            endif;
112800140319 1       when vscopz='5'   ;
112900140319            v3dmod='Interrogazione'  ;
113000140319            IntPrNd   =*on ;
113100140325            IntManPr  =*on;
113200140402            AbilF6    =*off;
113300140319            MANPrNd   =*off;
113400140319            NewNd     =*off;
113500140320            AbilAdd   = *off;
113600140325            chain(n) vshnrr uncmd00f;
113700140319            exsr ried03;
113800140402 1       when vscopz='4'   ;
113900140402            v3dmod=' Annullamento '  ;
114000140402            AbilF6=*on;
114100140402            IntPrNd   =*on ;
114200140402            IntManPr  =*on;
114300140402            MANPrNd   =*off;
114400140402            NewNd     =*off;
114500140402            AbilAdd   = *off;
114600140402            chain(e) vshnrr uncmd00f;
114700140402            if  %error or not %found;
114800140402               v1cmsg=msg(09)  ;
114900140402               ErrGenerico=*on  ;
115000140402               ErrMessage =*on  ;
115100140402               exsr aggiosfl;
115200140402               leavesr  ;
115300140402            endif;
115400140402            exsr ried03;
115500100830         endsl   ;
115600100830
115700140410         clear  preksc;
115800100830
115900100830 0       dow    $video='D3'   ;
116000100830
116100100830         // Emissione Testata
116200150921           write  SDUNT01;
116300100830         // Emissione videata
116400140319         exfmt  SDUND03;
116500100830
116600100830         reset errMessage;
116700100830         reset errGenerico;
116800100830         clear V1cmsg    ;
116900100830
117000100830 1       SELECT;
117100100830
117200100830         // - F12=Ritorno
117300100830 1         WHEN  dsp_aid = c_F12;
117400100924            ErrGenerico=*off ;
117500100924            ErrMessage =*off ;
117600100924            clear v1cmsg  ;
117700100831            Emismod1=*on  ;
117800100830            leavesr  ;
117900100901
118000140410         // - F11=Gestione Comodati Unità EDP
118100140319 1         WHEN  dsp_aid = c_F11;
118200140328           exsr CallIntCom      ;
118300100901
118400140319         // - F10=Aggiunta Comodato
118500140319 1         WHEN  dsp_aid = c_F10;
118600150921            unlock uncmd00f ;
118700140319            v3dmod=' Aggiunta     '  ;
118800140402            AbilF6    =*on;
118900140319            Intprnd   =*off;
119000140319            IntManPr  =*on ;
119100140319            ManPrNd   =*off;
119200140319            NewNd     =*on ;
119300140402       // Attivo ND su numero contratto e ragione sociale contratto
119400140402            NdCnn     = *on;
119500140326            exsr inzd03Uni;
119600120524
119700100909
119800100830 x1      // Invio
119900100830           OTHER;
120000100830
120100140319           // non controllo record in interrogazione
120200140319 3         if vscopz<>'5'   ;
120300140320             exsr ctrd03    ;
120400100924           endif   ;
120500100830
120600100830 2         if ErrGenerico=*Off   ;
120700100830
120800140319           //  Aggiornamento record di UNCMD
120900140402 2         if dsp_aid = c_F06;
121000140402           // Se richiesto annullamento emetto videata per richiedere la conferma
121100140402            if IntPrNd=*on;
121200140402              exsr sr_Wann;
121300140402              if wvann='S';
121400140402                 delete uncmdfis;
121500140402              endif;
121600140402            else;
121700150918           // Confermo i dati inseriti/modificati
121800140321              exsr   confermaCMD ;
121900150922           // Addebito in C.E.
122000150922              if ErrGenerico=*off and v3cace='S' and wunrimp>0;
122100150922                    clear YCOSTP0I ;
122200150922                    STPTIPOI='STP';
122300150924                    STPkcc  =wkcc          ;
122400150922                    STPELAB ='S';
122500150922                    kpjbu=ycostp0i;
122600150922                    clear ycostp2ds;
122700150923                    wksc=v3ksc  ;
122800150923                    STP2KSC=%editC(wksc:'X') ;
122900150922                    STP2IMP=wunrimp*v3qta   ;
123000150922                    STP2DATE=dataisocor      ;
123100150922                    ycostp2r(kpjba:ycostp2ds);
123200150922              endif;
123300140402            endif;
123400150921             if ErrGenerico=*off;
123500140321              Emismod1=*on  ;
123600140327              if newnd=*on ;
123700140327                 waddrcd='1';
123800140327              endif;
123900140326              leavesr;
124000150921             endif;
124100100830
124200140321 2         endif   ;
124300100830 2       endif   ;
124400100830
124500100830 1       ENDSL;
124600100830
124700100830 0     enddo   ;
124800100830
124900100830       ENDSR;
125000140320       //--------------------------------------------------------------
125100140320       //?
125200140320       //--------------------------------------------------------------
125300140320       BEGSR Ctrd03  ;
125400100830
125500140321       errksc=*off;
125600140321       errtip=*off;
125700140321       errqta=*off;
125800140321       errcds=*off;
125900140331       errcfg=*off;
126000140321
126100140321       // CLIENTE - RICERCA ALFABETICA
126200140320       if v3ksc=*zeros and v3kscd<>*blanks;
126300140320          exsr sr_ricalfa;
126400140321          ErrGenerico=*on;
126500140321          leavesr;
126600140320       endif;
126700140321       // CLIENTE - CONTROLLO
126800140321       // Controllo il codice in caso di nuovo inserimento
126900140321       if v3ksc=*zeros and IntManPr=*off;
127000140321          Errksc=*on;
127100140321          ErrGenerico=*on;
127200140321          ErrMessage =*on  ;
127300140321          v1cmsg=msg(1);
127400140321          leavesr;
127500140321       endif;
127600140321       if IntManPr =*off;
127700140321           clear v3kscd;
127800140508           exsr sr_decoksc;
127900140508           if o69err<>*blanks;
128000140321              Errksc=*on;
128100140321              ErrGenerico=*on;
128200140321              ErrMessage =*on  ;
128300140321              v1cmsg=msg(2);
128400140321              leavesr;
128500140321           endif;
128600140410       // messaggio di avviso se cliente non presente nel file UNITà nel
128700140410       // magazzino 951
128800140410          if v3ksc<>preksc;
128900140410           preksc=v3ksc;
129000140410           clear wtrov_un;
129100140410           setll (v3ksc) unana10l;
129200140410           reade v3ksc unana10l;
129300140410           dow not %eof and wtrov_un=*blanks;
129400140410              if unamag=951 ;
129500140410                 wtrov_un='S' ;
129600140410              endif;
129700140410              reade v3ksc unana10l;
129800140410           enddo;
129900140410           if wtrov_un=*blanks;
130000140410              Errksc=*on;
130100140410              ErrGenerico=*on;
130200140410              ErrMessage =*on  ;
130300140410              v1cmsg=msg(14);
130400140410              leavesr;
130500140410           endif;
130600140410          endif;
130700140321       endif;
130800140321       // TIPO/MODELLO
130900140321       // Interrogazione tabella  UNR
131000140321       if %scan('?':v3tip)>0;
131100140321          clear v3tip;
131200140321          clear v3tipd;
131300140321          exsr sr_rictip;
131400140328          v3tip=wtip;
131500140328          v3tipd=wtipd;
131600140321          ErrGenerico=*on;
131700140321          leavesr;
131800140321       endif;
131900140321       // Controllo validità del codice inserito
132000140321       if IntPrNd=*off;
132100140321         if v3tip = *blanks;
132200140321       //    Obbligatorio
132300140321              Errtip=*on;
132400140321              ErrGenerico=*on;
132500140321              ErrMessage =*on  ;
132600140321              v1cmsg=msg(1);
132700140321              leavesr;
132800140321         endif;
132900140321         clear tibs02ds  ;
133000150922         clear dunr;
133100150922         clear wunrimp;
133200140321         t02tla=' '      ;
133300140321         t02mod='C'      ;
133400140321         t02sif=knsif    ;
133500140321         t02cod='UNR'    ;
133600140321         t02ke1=v3tip    ;
133700140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
133800140321
133900140321       // Errato
134000140321         if  t02err<>*blanks ;
134100140321            ErrGenerico=*on  ;
134200140321            ErrMessage =*on  ;
134300140321            ErrTIP     =*on  ;
134400140321            V3tipD=*all'?'   ;
134500140321            v1cmsg=msg(3);
134600140321            leavesr;
134700140321         else;
134800140321            V3TIPD=T02UNI;
134900150922            dunr=t02uni;
135000150922            monitor;
135100150922            if d§unrimp>0;
135200150922               wunrimp=d§unrimp;
135300150922            endif;
135400150922            on-error  ;
135500150922            endmon  ;
135600140321         endif;
135700140321       endif;
135800140321       // QUANTITA'
135900140402       // Obbligatoria sempre
136000140402       if IntPrNd=*off ;
136100140321          if v3qta=0;
136200140321            ErrGenerico=*on  ;
136300140321            ErrMessage =*on  ;
136400140321            Errqta     =*on  ;
136500140321            v1cmsg=msg(1);
136600140321            leavesr;
136700140321          endif ;
136800140321       endif;
136900140321       // DATA STAMPA CONTRATTO
137000140321       clear w3cds;
137100140321       if v3cds>0;
137200140321             clear wlbdat;
137300140321             g02dat = v3cds  ;
137400140321             xsrda8(wlbdat);
137500140321             if g02err = '1';
137600140321               errMessage  = *on;
137700140321               errGenerico = *on;
137800140321               Errcds    = *on;
137900140321               V1cmsg = Msg(04);
138000140321               leavesr;
138100140321             endif;
138200140321
138300140321             v3cds  = g02dat;
138400140321             W3cds  = g02inv;
138500140321       endif;
138600140331       // Flag gestione contratto
138700140331         if v3cfg <> ' ' and
138800140331            v3cfg <> 'F' and
138900140331            v3cfg <> 'A' and
139000140331            v3cfg <> 'C'     ;
139100140331            ErrGenerico=*on  ;
139200140331            ErrMessage =*on  ;
139300140331            Errcfg     =*on  ;
139400140331            v1cmsg=msg(10);
139500140331            leavesr;
139600140331         endif;
139700140331       // Non ammesso flag <> blanks se manca il numero contratto
139800140402         if v3cfg <>' ' and v3cfg<>'A' and v3cfg<>'C' and v3cnn=0 ;
139900140331            ErrGenerico=*on  ;
140000140331            ErrMessage =*on  ;
140100140331            Errcfg     =*on  ;
140200140331            v1cmsg=msg(11);
140300140331            leavesr;
140400140331         endif;
140500140328       // Errore se già presente altro record comodato
140600140321       // con il cliente/data immissione/tipo/mod dati
140700140328       chain (v3ksc:w3dim:v3tip) uncmd01l;
140800140328       if %found(uncmd01l) and
140900140328            (newNd=*on or vshnrr<> cmd1nrr);
141000140321               errMessage  = *on;
141100140321               errGenerico = *on;
141200140321               Errksc    = *on;
141300140321               V1cmsg = Msg(05);
141400140321               leavesr;
141500140321       endif;
141600150922       // Se F6=Conferma e richiesto addebito in C.E.
141700150922       // --> errore se manca l'importo unitario su tabella UNR per il Tipo/Modello immesso
141800150922 2         if dsp_aid = c_F06
141900150922            and IntPrNd=*off
142000150922              and v3cace='S'
142100150922                 and wunrimp=0;
142200150922                    ErrGenerico=*on;
142300150922                    ErrMessage =*on  ;
142400150922                    v1cmsg=msg(15);
142500150922                    leavesr;
142600150922           endif;
142700140320       endsr ;
142800140320       //--------------------------------------------------------------
142900140320       //? Ricerca alfabetica cliente
143000140320       //--------------------------------------------------------------
143100140320       BEGSR sr_ricalfa;
143200140320           clear xparsta  ;
143300140320           xparkcc=dutkci ;
143400140320           xparkut= 1     ;
143500140320           xpardut=rsut   ;
143600140320           if $video='D3' ;
143700140320           xparrag=v3kscd ;
143800140320           else  ;
143900140320           xparrag=v1kscd ;
144000140320           endif  ;
144100140320           clear xparflr ;
144200140320           clear xparkcm  ;
144300140320           clear xparksm  ;
144400140320           clear xparkdm  ;
144500140320           xparnum=1      ;
144600140320           callp XALFA3BR ( xpardut:xparkut:xparrag:xparkcc:xparsta
144700140320                           :xparflr:xpardit:xparnum:xparkcm:xparksm
144800140320                           :xparkdm:xparesci:xparerr:xpariva:xparcdf);
144900140320 2          if xparsta<>-1  ;
145000140320               if $video='D3' ;
145100140320                  v3ksc =%int(%subst(xparksm:1:7))   ;
145200140320                  v3kscd=xparrag   ;
145300140320               else;
145400140320                  v1ksc =%int(%subst(xparksm:1:7))   ;
145500140320                  v1kscd=xparrag   ;
145600140320               endif;
145700140320 2          endif            ;
145800140320       endsr ;
145900140321       //--------------------------------------------------------------
146000140321       //? Ricerca Tipo/Modello su tabella UNR
146100140321       //--------------------------------------------------------------
146200140321       BEGSR sr_rictip ;
146300140328         clear wtip;
146400140328         clear wtipd;
146500140321         clear tibs02ds  ;
146600140321         t02tla=' '      ;
146700140321         t02mod='R'      ;
146800140321         t02sif=knsif    ;
146900140321         t02cod='UNR'    ;
147000140321         TNTBE_RicercaControllo  (kpjba : tibs02ds);
147100140321         if t02ke1<>*blanks;
147200140328            wtip =t02ke1    ;
147300140328            wtipd=t02uni    ;
147400140321         endif;
147500140321       endsr ;
147600140321       //--------------------------------------------------------------
147700140321       //?Conferma Comodato su Data Base
147800140321       //--------------------------------------------------------------
147900140321       BEGSR ConfermaCMD;
148000140404       clear wtrovcnn;
148100140326       if NewNd=*on;
148200140326          clear uncmdfis;
148300140326       CMDDIM=w3dim;
148400140326       endif;
148500140326       CMDKSC=v3ksc;
148600140404       // FLAG Stampa Contratto = 'S':
148700140404       // Se già presente contratto per lo stesso cliente/data immissione recupero
148800140404       // quel numero, data stampa e ragione sociale contratto
148900140404       // altrimenti
149000140404       // Prelevo dal numeratore il numero contratto
149100140402       // e memorizzo la ragione sociale contratto
149200140325       if v3cfs='S' and v3cnn=0;
149300140404         exsr sr_riccnn;
149400140404         if wtrovcnn='S';
149500140404            cmdcnn=c1_cmdcnn;
149600140404            cmdrsc=c1_cmdrsc;
149700140404            cmdcds=c1_cmdcds;
149800140404         else;
149900140404            exsr RepNum    ;
150000140404            if esito = *blanks;
150100140404               cmdcnn=o33nri;
150200140404            else;
150300140404                 errMessage  = *on;
150400140404                 errGenerico = *on;
150500140404                 V1cmsg = Messaggio;
150600140404                 leavesr;
150700140404            endif;
150800140404            cmdrsc=v3kscd;
150900140404         endif;
151000140321       endif;
151100140404       // Data stampa Contratto
151200140404       if wtrovcnn=' ';
151300140404          CMDCDS=W3cds;
151400140404       endif;
151500140321       CMDTIP=v3tip;
151600140321       CMDQTA=v3qta;
151700140321       // flag stampa contratto S/N
151800140321       if v3cfs=*blanks;
151900140321          CMDCFS='N'  ;
152000140321       else;
152100140321          CMDCFS=v3cfs;
152200140321       endif;
152300140321       CMDCFG=v3cfg;
152400140321       if cmdcfg<>*blanks;
152500140321          CMDCdg= %dec(dataisocor);
152600140321       else;
152700140321          clear CMDCdg;
152800140321       endif;
152900140321       // flag invio mail a poc S/N
153000140321       if v3cfs=*blanks;
153100140321          cmdcfi='N';
153200140321       else;
153300140321          CMDCFI=v3cfi;
153400140321       endif;
153500140321       CMDNOT=v3not;
153600140325       if newnd=*on;
153700140325          write uncmdfis;
153800140325       else;
153900140325          update uncmdfis;
154000140325       endif;
154100140321       endsr;
154200140321        //-----------------------------------------------------------------------*
154300140321        // Reperimento numero contratto da numeartore
154400140321        //-----------------------------------------------------------------------*
154500140321        begsr RepNum;
154600140321
154700140321          clear esito;
154800140331          $finenum=*off;
154900140321          clear trul33ds;
155000140321
155100140321          I33TLa = 'L';
155200140321          I33Ope = *ZEROS;
155300140331          I33CNu = 020;
155400140321          I33Num = 1;
155500140321
155600140321          KPJBU = TRUL33DS;
155700140321
155800140331          dow $finenum=*off   ;
155900140403             Trul33R(kpjba);
156000140321
156100140331             TRUL33DS = KPJBU;
156200140321
156300140331             if O33Err <> *zeros;
156400140331               Esito = '2';
156500140331               $finenum=*on;
156600140331               Messaggio = 'Errore in ' +
156700140403                           'reperimento contatore (TRUL33R)';
156800140331             else;
156900140331               w0070=o33nri;
157000140331               setll w0070 uncmd02l;
157100140331        // Se numero già presente nel file comodati ne cerco un altro
157200140331               if not %equal;
157300140331                  $finenum=*on;
157400140331               endif;
157500140331             endif;
157600140331          enddo;
157700140321
157800140321          endsr;
157900140324
158000140321
158100140324       //--------------------------------------------------------------
158200140324       //?Preparazione stringa SQL
158300140324       //--------------------------------------------------------------
158400140324       BEGSR sr_prepsql;
158500140324       clear wand;
158600140328       WrkStringaSql='select rrn(uncmd00f), uncmd00f.* from uncmd00f';
158700140411       // Se richiesto cliente ignoro eventuali altre parzializz.
158800140411       if v1ksc>0;
158900140411          wand='1';
159000140411          WrkStringaSql=WrkStringaSql+ ' where ' +
159100140411          ' CMdksc=' + %editc(v1ksc:'X');
159200140411          leavesr;
159300140411       endif;
159400140324       // Richieste note
159500140324       if v1not<>*blanks;
159600140324          if wand = ' ';
159700140328             WrkStringaSql=WrkStringaSql+  ' where ' +
159800140327             ' CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
159900140324             wand='1';
160000140324          else;
160100140324             WrkStringaSql=WrkStringaSql+
160200140327             ' and CMdnot like' + '''' + '%' + %trim(v1not) + '%' + '''' ;
160300140324          endif;
160400140324       endif;
160500140324       // Flag gestione Contratto
160600140410       if v1cfg<>*blanks;
160700140410        if v1cfg <> 'T';
160800140410          w1cfg=v1cfg;
160900140410          if w1cfg='D';
161000140410             w1cfg=' ';
161100140410          endif;
161200140324          if wand = ' ';
161300140328             WrkStringaSql=WrkStringaSql+ ' where ' +
161400140410             ' cmdcfg =' + '''' + w1cfg + '''';
161500140324             wand='1';
161600140324          else;
161700140324             WrkStringaSql=WrkStringaSql+
161800140410             ' and cmdcfg =' + '''' + w1cfg + '''';
161900140324          endif;
162000140410        else;
162100140402          if wand = ' ';
162200140402             WrkStringaSql=WrkStringaSql+ ' where ' +
162300140402             ' cmdcfg not in(' + '''' + 'A' + '''' + ','+  '''' +
162400140402             'C' + '''' + ')';
162500140402             wand='1';
162600140402          else;
162700140402             WrkStringaSql=WrkStringaSql+
162800140402             ' and cmdcfg not in(' + '''' + 'A' + '''' + ','+  '''' +
162900140402             'C' + '''' + ')';
163000140402          endif;
163100140410        endif;
163200140410       endif;
163300140402       // Flag da stampare
163400140402       if v1cds =  'S';
163500140402          if wand = ' ';
163600140402             WrkStringaSql=WrkStringaSql+ ' where ' +
163700140402             ' cmdcnn > 0 and cmdcds= 0';
163800140402             wand='1';
163900140402          else;
164000140402             WrkStringaSql=WrkStringaSql+
164100140402             ' and cmdcnn > 0 and cmdcds= 0';
164200140402          endif;
164300140402       endif;
164400140324       // Data stampa  Contratto dal/al
164500140327       if w1dsd> 0 or w1dsa>0;
164600140327        select;
164700140327       // immesse entrambe
164800140327        when w1dsd>0 and w1dsa>0;
164900140327          if wand = ' ' ;
165000140328             WrkStringaSql=WrkStringaSql+  ' where ' +
165100140327             ' cmdcds between';
165200140327             wand='1';
165300140327          else;
165400140327             WrkStringaSql=WrkStringaSql+
165500140327             ' and cmdcds between';
165600140327          endif;
165700140327          WrkStringaSql=WrkStringaSql+
165800140327          ' ' + %editc(w1dsd:'X') + ' and ' + %editc(w1dsa:'X');
165900140327       // immessa solo la data dal
166000140327        when w1dsd>0;
166100140327          if wand = ' ' ;
166200140328             WrkStringaSql=WrkStringaSql+  ' where ' +
166300140327             ' cmdcds >=';
166400140327             wand='1';
166500140327          else;
166600140327             WrkStringaSql=WrkStringaSql+
166700140327             ' and cmdcds >=';
166800140327          endif;
166900140327          WrkStringaSql=WrkStringaSql+
167000140327          ' ' + %editc(w1dsd:'X') ;
167100140327       // immessa solo la data al
167200140327        when w1dsa>0;
167300140327          if wand = ' ' ;
167400140328             WrkStringaSql=WrkStringaSql+ ' where ' +
167500140327             ' cmdcds <=';
167600140327             wand='1';
167700140327          else;
167800140327             WrkStringaSql=WrkStringaSql+
167900140327             ' and cmdcds <=';
168000140327          endif;
168100140327          WrkStringaSql=WrkStringaSql+
168200140327          ' ' + %editc(w1dsa:'X') ;
168300140327        endsl;
168400140324       endif;
168500140324       // Data Immissione dal/al
168600140324       if w1imd> 0 or w1ima>0;
168700140327        select;
168800140327        when w1imd>0 and w1ima>0;
168900140327       // immesse entrambe
169000140324          if wand = ' ' ;
169100140328             WrkStringaSql=WrkStringaSql+  ' where ' +
169200140324             ' cmddim between';
169300140324             wand='1';
169400140324          else;
169500140324             WrkStringaSql=WrkStringaSql+
169600140324             ' and cmddim between';
169700140324          endif;
169800140324          WrkStringaSql=WrkStringaSql+
169900140327          ' ' + %editc(w1imd:'X') + ' and ' + %editc(w1ima:'X');
170000140327       // immessa solo datA dal
170100140327        when w1imd>0            ;
170200140327          if wand = ' ' ;
170300140328             WrkStringaSql=WrkStringaSql+ ' where ' +
170400140327             ' cmddim >=';
170500140327             wand='1';
170600140327          else;
170700140327             WrkStringaSql=WrkStringaSql+
170800140327             ' and cmddim >=';
170900140327          endif;
171000140327          WrkStringaSql=WrkStringaSql+
171100140327          ' ' + %editc(w1imd:'X') ;
171200140327       // immessa solo datA al
171300140327        when w1ima>0            ;
171400140327          if wand = ' ' ;
171500140328             WrkStringaSql=WrkStringaSql+ ' where ' +
171600140327             ' cmddim <=';
171700140327             wand='1';
171800140327          else;
171900140327             WrkStringaSql=WrkStringaSql+
172000140327             ' and cmddim <=';
172100140327          endif;
172200140327          WrkStringaSql=WrkStringaSql+
172300140327          ' ' + %editc(w1ima:'X') ;
172400140327        endsl;
172500140324       endif;
172600140508       // ordino il sfl per ragione sociale contratto/codice cliente
172700140508          WrkStringaSql=WrkStringaSql+ ' order by cmdrsc, cmdksc';
172800140324       endsr;
172900140324       //--------------------------------------------------------------
173000140324       //?Decodifica flag gestione contratto
173100140324       //--------------------------------------------------------------
173200140324       BEGSR Decocfg;
173300140324         select;
173400140324         when vsccfg=' ';
173500140324            vsccfgd='Da Ricevere';
173600140324         when vsccfg='F';
173700140324            vsccfgd='Firmato    ';
173800140324         when vsccfg='A';
173900140324            vsccfgd='Annullato  ';
174000140324         when vsccfg='C';
174100140324            vsccfgd='Chiuso     ';
174200140324         endsl;
174300140325       endsr;
174400140325       //--------------------------------------------------------------
174500140325       //?controllo riga di sfl
174600140325       //--------------------------------------------------------------
174700140325       BEGSR ctrrecsfl;
174800140328
174900140328          errtip=*off;
175000140328          errcfg=*off;
175100140508          errcdss=*off;
175200140328
175300140328       // TIPO/MODELLO
175400140410       // Disattivo i controlli del Tipo/Modello in quanto il campo è ora protetto
175500140410       if errtip=*off and errtip=*on;
175600140328       // Interrogazione tabella  UNR
175700140328       if %scan('?':vsctip)>0;
175800140328          clear vsctip;
175900140328          clear vsctipd;
176000140328          exsr sr_rictip;
176100140328          vsctip=wtip;
176200140328          vsctipd=wtipd;
176300140328          ErrGenerico=*on;
176400140328          exsr aggiosfl;
176500140328          leavesr;
176600140328       endif;
176700140328       // Controllo validità del codice inserito
176800140328         if vsctip = *blanks;
176900140328       //    Obbligatorio
177000140328              Errtip=*on;
177100140328              ErrGenerico=*on;
177200140328              ErrMessage =*on  ;
177300140328              v1cmsg=msg(1);
177400140328              exsr aggiosfl;
177500140328              leavesr;
177600140328         endif;
177700140328         clear tibs02ds  ;
177800140328         t02tla=' '      ;
177900140328         t02mod='C'      ;
178000140328         t02sif=knsif    ;
178100140328         t02cod='UNR'    ;
178200140328         t02ke1=vsctip    ;
178300140328         TNTBE_RicercaControllo  (kpjba : tibs02ds);
178400140328
178500140328       // Errato
178600140328         if  t02err<>*blanks ;
178700140328            ErrGenerico=*on  ;
178800140328            ErrMessage =*on  ;
178900140328            ErrTIP     =*on  ;
179000140328            VsctipD=*all'?'   ;
179100140328            v1cmsg=msg(3);
179200140328            exsr aggiosfl;
179300140328            leavesr;
179400140328         else;
179500140328            VscTIPD=T02UNI;
179600140328         endif;
179700140328       // Errore se già presente altro record comodato
179800140328       // con il cliente/data immissione/tipo/mod dati
179900140328         exsr sr_altrocomod;
180000140328         if errGenerico = *on;
180100140328               leavesr;
180200140328         endif;
180300140410         endif;
180400140328       // Flag gestione contratto + Decodifica
180500140328         clear vsccfgd;
180600140328         if vsccfg <> ' ' and
180700140328            vsccfg <> 'F' and
180800140328            vsccfg <> 'A' and
180900140328            vsccfg <> 'C'     ;
181000140328            ErrGenerico=*on  ;
181100140328            ErrMessage =*on  ;
181200140328            Errcfg     =*on  ;
181300140328            v1cmsg=msg(10);
181400140328            exsr aggiosfl;
181500140328            leavesr;
181600140328         endif;
181700140328         exsr decocfg;
181800140331       // Non ammesso flag <> blanks se manca il numero contratto
181900140331         if vsccfg <>' ' and vsccnn=0;
182000140331            ErrGenerico=*on  ;
182100140331            ErrMessage =*on  ;
182200140331            Errcfg     =*on  ;
182300140331            v1cmsg=msg(11);
182400140331            exsr aggiosfl;
182500140331            leavesr;
182600140331         endif;
182700140508       // DATA STAMPA CONTRATTO
182800140508       clear vshcds;
182900140508       if vsccds>0;
183000140508             clear wlbdat;
183100140508             g02dat = vsccds ;
183200140508             xsrda8(wlbdat);
183300140508             if g02err = '1';
183400140508               errMessage  = *on;
183500140508               errGenerico = *on;
183600140508               Errcdss   = *on;
183700140508               V1cmsg = Msg(04);
183800141027               exsr aggiosfl;
183900140508               leavesr;
184000140508             endif;
184100140508           vshcds = g02inv;
184200140508       endif;
184300140328
184400140328         exsr aggiosfl;
184500140325       endsr;
184600140325       //--------------------------------------------------------------
184700140325       //?Decodifia tipo/modello
184800140325       //--------------------------------------------------------------
184900140325       BEGSR decotip;
185000140325         clear dunr;
185100140325         clear tibs02ds  ;
185200140325         t02tla=' '      ;
185300140325         t02mod='C'      ;
185400140325         t02sif=knsif    ;
185500140325         t02cod='UNR'    ;
185600140325         t02ke1=VSCTIP   ;
185700140325         TNTBE_RicercaControllo  (kpjba : tibs02ds);
185800140325
185900140325         if  t02err=*blanks ;
186000140325            dunr=t02uni;
186100140325            vsctipd = D§UNRDES;
186200140325         ELSE;
186300140325            vsctipd = *ALL'?' ;
186400140325         endif;
186500140325       endsr;
186600140328       //--------------------------------------------------------------
186700140328       //?Aggiornamento sfl
186800140328       //--------------------------------------------------------------
186900140328       BEGSR AGGIOSFL  ;
187000140328
187100140328       if v1cmsg<>*blanks   ;
187200140328       errMessage  = *on;
187300140328       errGenerico = *on;
187400140328       c02csr=s02nrr    ;
187500140328       endif    ;
187600140328
187700140328        Sflnxtchg=*on  ;
187800140403        protsfl=*off;
187900140328
188000140328       update  sduns02  ;
188100140328
188200140328       ENDSR   ;
188300140328       //--------------------------------------------------------------
188400140328       //?Conferma dati sfl2
188500140328       //--------------------------------------------------------------
188600140328       BEGSR Confermas02;
188700140328
188800140328       s02nrr=1  ;
188900140328       readc       sduns02    ;
189000140328
189100140328 0     dow not %eof ;
189200140328       // Ricontrollo che non esista già altro comodato con stesso
189300140328       // cliente/data immmissione/modello
189400140328         exsr sr_altrocomod;
189500140328         if errgenerico=*on;
189600140328       //   werrsfl = *on;
189700140328            leavesr;
189800140328         else;
189900140328            chain(e) vshnrr uncmd00f;
190000140328            if  %error or not %found;
190100140328               v1cmsg=msg(09)  ;
190200140328               ErrGenerico=*on  ;
190300140328               ErrMessage =*on  ;
190400140328               exsr aggiosfl;
190500140328       //      werrsfl = *on;
190600140328               leavesr  ;
190700140328            else;
190800140328               cmdtip=vsctip;
190900140328       //      Se flag gestione contratto diverso aggiorno anche la data
191000140328               if cmdcfg<>vsccfg;
191100140328                  CMDCdg= %dec(dataisocor);
191200140328               endif;
191300140328               cmdcfg=vsccfg;
191400140508               cmdcds=vshcds;
191500140328               update uncmdfis;
191600140328            endif;
191700140328         endif;
191800140328
191900140328       readc    sduNs02    ;
192000140328 0     enddo         ;
192100140328
192200140328       ENDSR  ;
192300140328       //--------------------------------------------------------------
192400140328       //?Controllo presenza record duplicato
192500140328       //--------------------------------------------------------------
192600140328       BEGSR sr_altrocomod;
192700140328         chain (vscksc:vshdim:vsctip) uncmd01l;
192800140328         if %found(uncmd01l) and vshnrr<>cmd1nrr;
192900140328               errMessage  = *on;
193000140328               errGenerico = *on;
193100140328               Errtip    = *on;
193200140328               V1cmsg = Msg(05);
193300140328               exsr aggiosfl;
193400140328         endif;
193500140328       endsr;
193600140402       //--------------------------------------------------------------
193700140402       //?Gestione window richiesta conferma annullamento
193800140402       //--------------------------------------------------------------
193900140402       BEGSR sr_Wann;
194000140402       clear wvann;
194100140402       eval $finew=*off;
194200140402       dow $finew =*off;
194300140402          exfmt sdunwann;
194400140402          if dsp_aid=C_F06;
194500140402             $finew=*on;
194600140402          endif;
194700140402       enddo;
194800140402       endsr;
194900140404       //--------------------------------------------------------------
195000140404       //?Ricerca esistenza contratto per stesso cliente/data immissione
195100140404       //--------------------------------------------------------------
195200140404       BEGSR sr_riccnn;
195300140404        setll (cmdksc:cmddim) uncmd01l;
195400140404        reade (cmdksc:cmddim) uncmd01l;
195500140404        dow  not %eof(uncmd01l);
195600140404           if newnd=*on or vshnrr<>cmd1NRR;
195700140404              wtrovcnn='S';
195800140404              leavesr;
195900140404           endif;
196000140404        reade (cmdksc:cmddim) uncmd01l;
196100140404        enddo;
196200140404       endsr;
196300140508       //--------------------------------------------------------------
196400140508       //?Decodifica ragione sociale cliente D03
196500140508       //--------------------------------------------------------------
196600140508       BEGSR sr_decoksc;
196700140508         clear DS_cnaco;
196800140508         clear DS_cnind;
196900140508         clear DS_cnclp;
197000140508         clear DS_fncls;
197100140508         I69kac=v3ksc  ;
197200140508         tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
197300140508         v3kscd=acorag;
197400140508       endsr;
197500140325       //--------------------------------------------------------------
197600080206       //?Operazioni finali.
197700080206       //--------------------------------------------------------------
197800080206       BEGSR RoutEnd;
197900080627
198000080704         // Chiusura pgm   ;
198100080704         clear tibs02ds  ;
198200080704         t02tla='C'      ;
198300080704         TNTBE_RicercaControllo  (kpjba : tibs02ds);
198400100920
198500080206         *inLR = *on;
198600080704
198700080206         return;
198800080206
198900080206       ENDSR;
199000080206
199100080206      /end-free
199200080206
199300080206       //--------------------------------------------------------------
199400080206       //?Schiere a tempo di compilazione.
199500080206       //--------------------------------------------------------------
199600080206
199700080410** - MSG ------------------------------------------------------------------ -*
199800140321Campo obbligatorio                                                              01
199900140321Codice inesistente                                                              02
200000140321Tipo/Modello errato                                                             03
200100140321Data errata                                                                     04
200200140321Già presente comodato per il cliente/data immissione/Modello                    05
200300140324Data dal > di Data Al                                                           06
200400140324N. Contratto inesistente                                                        07
200500140410Nessun dato da visualizzare per la selezione richiesta                          08
200600140328Modifica al momento non eseguibile: record vincolato da altro lavoro            09
200700140328Valori validi: " "=Da ricevere, "F"=Firmato, "C"=Chiuso, "A"=Annullato          10
200800140331Gestione Contratto errato: contratto non ancora creato                          11
200900140328Premere F6=Conferma per non perdere le variazioni eseguite                      12
201000140410Impossibile annullare: presente Bolla/Contratto                                 13
201100140410Attenzione:il Cliente non è presente nelle Unità con Magazzino 951.Enter FORZ.  14
201200150922Non è possibile eseguire l'addebito in C.E.:inserire Importo in tabella UNR     15
