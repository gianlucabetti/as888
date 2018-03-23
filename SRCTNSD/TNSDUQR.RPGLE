000200080206      //--------------------------------------------------------------
000300100910      //?TNSDUQR - Stampa bolle perInvii/rientri giornalieri unità EDP
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200100910     fTNSDUQD   cf   e             workstn indds(IndDspF)
001300080206     f                                     infds(InfDspF)
001400100910     f                                     sfile(SDUQS02 : S02nrr)
001500100910     FUNgio03L  iF   E           K DISK
001600100312     FAZORG21l  IF   E           K DISK
001700100312     fazorg01l  if   e           k disk
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
006100140117     d MSG             s             78    dim(12) ctdata perrcd(1)
006200100830     d WOPZ            s              1    dim(15)
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione aree dati.
006500080206      //---------------------------------------------------------------
006600080206      // - Dati utente
006700080206     d §AzUte        e ds                  extname(AZUTE00F)
006800080206     d                                     dtaara
006900080206     d §DatiUte      e ds                  extname(dDatiUte)
007000080206     d                                     dtaara
007100080206      //---------------------------------------------------------------
007200080206      //?Definizione strutture dati.
007300080206      //---------------------------------------------------------------
007400080206      // - Status
007500080206     d Psds           sds
007600080206     d   SDSpgm          *proc
007700080206
007800080206      // - InfDS
007900080206     d InfDspF         ds
008000080207     d  dsp_aid              369    369a                                        AID byte
008100080207     d  sfl_rrn              376    377i 0                                      Subfile rrn
008200080207     d  min_nrr              378    379i 0                                      Subfile min rrn
008300080207     d  num_rcds             380    381i 0                                      Subfile num rcds
008400080206
008500080206      // - Indicatori su DspF
008600080208     d IndDspF         ds
008700080206        // - Indicatori di gestione del subfile
008800080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009000080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009100080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009200080206        // - Indicatori di errore
009300080206     d  errMessage                    1n   overlay(IndDspF : 28)
009400080606     d  errGenerico                   1n   overlay(IndDspF : 99)
009500080627        // - Indicatori vari
009600100910     d  EmmMAG                        1n   overlay(IndDspF : 01)
009700100915     d  EmmFOR                        1n   overlay(IndDspF : 02)
009800140219     d  EmmTLC                        1n   overlay(IndDspF : 03)
009900131118     d  in_rista                      1n   overlay(IndDspF : 05)
010000100910     d  ErrDTM                        1n   overlay(IndDspF : 40)
010100100910     d  ErrOPZ                        1n   overlay(IndDspF : 41)
010200100910     d  ErrNCL                        1n   overlay(IndDspF : 42)
010300100910     d  ErrPkg                        1n   overlay(IndDspF : 43)
010400101028     d  ErrAUM                        1n   overlay(IndDspF : 44)
010500140117     d  ErrCAP                        1n   overlay(IndDspF : 45)
010600080206
010700080206      // - Parametri ricevuti
010800080206     d KPJBA         e ds
010900080206
011000080206      // - Reperimento dati utente
011100080206     d TIBS34ds      e ds
011200080206     d dUte01        e ds
011300080702     d DLAT          e ds
011400080702
011500100927     d og143         e ds
011600140117     d og148         e ds
011700100927     d DUNT          e ds
011800100901     d DSUNIT        e ds
011900100827     d TNSDV0DS      e ds
012000101005       // Ds controllo magazzino
012100101005     d tnsdmagds     e ds
012200131118     D* DS PER TRUL90R - RICHIESTA STAMPANTI
012300131118     D Trul90ds      e ds                  inz
012400140117
012500140117     d fnlv55ds      e ds
012600140117     d fnlv13ds      e ds
012700140117     d tisi95ds      e ds
012800080206
012900100316
013000100316     d wlbdat          ds                  inz
013100100316     d  g02dat                 1      8  0
013200100316     d  g02inv                 9     16  0
013300100316     d  g02err                17     17
013400100316     d  g02tgi                18     22  0
013500100316
013600080206      //---------------------------------------------------------------
013700080206      //?Definizione variabili globali.
013800080206      //---------------------------------------------------------------
013900080206
014000080206      // - Flags booleani
014100080208     d $Fine           s               n   inz(*off)
014200100312     d $InzS02         s               n   inz(*on)
014300100312     d $Inzd01         s               n   inz(*on)
014400080206
014500100312     d $Video          s              2    inz('D1')
014600100315     d S02nrr          s              4  0 inz
014700081006     d XX              s              3  0 inz
014800080627     d Indx            s              3  0 inz
014900080703     d Primavolta      s              1    inz
015000081126     d yy              s              3  0 inz
015100100831     d Dataoggi        s              8  0 inz
015200140117     d DataoggiI       s              8  0 inz
015300110221     d Datameno28      s              8  0
015400100910     d W1cdtm          s              8  0
015500100908     d wazorg          s              3    Inz('   ')
015600100316     d wmag            s              3
015700100910     d savmag          s                   like(ungmag)
015800101028     d savaum          s                   like(ungtru)
015900100824     d wOK             s              1
016000100910     d unMAG           s              1
016100101008     d totncl          s                   like(ungncl)
016200101008     d totpkg          s                   like(ungpkg)
016300140117     d w0030           s              3  0
016400080414
016500100315     d Dataeur         s               d   datfmt(*eur)
016600100315     d Dataiso         s               d   datfmt(*iso)
016700080605     d Dataymd         s               d   datfmt(*ymd)
016800080605     d Datadmy         s               d   datfmt(*dmy)
016900100901     d Datasys         s               d   inz(*sys)
017000100901     d timeiso         s               t   timfmt(*iso)
017100100910     D*----------------
017200100910     D* PARAMETRI LANCIO
017300100910     D*----------------
017400100910     D PARAM           DS
017500100910     D  PARMAG                 1      3  0
017600100910     D  PARDAT                 4     11  0
017700100910     D  PARRIS                12     12
017800100910     D  PARncl                13     17  0
017900100910     D  PARpkg                18     24  1
018000100910     D  PARMAUT               25     25
018100100915     D  PARcuf                26     28
018200101028     D  PARaum                29     29
018300131118     D  PARbol                30     30
018400131118     d* - parametri da TRUL90R:
018500131118     d* · pgm.   per stampa LDV
018600131118     d  wD90psl              157    166
018700131118     d* · stampante per LDV in formato "A4" (laser)
018800131118     d  wD90prb4             167    176
018900131118     d* · stampante per LDV in formato "A5" (laser)
019000131118     d  wD90prb5             177    186
019100131118     d* · modulo per stampa LDV su "A4"     (laser)
019200131118     d  wD90mdb4             187    196
019300131118     d* · modulo per stampa LDV su "A5"     (laser)
019400131118     d  wD90mdb5             197    206
019500131118     d* · pgm di stampa etichette segnacolli
019600131118     d  wD90pss              207    216
019700131121     d* · modulo per stampa segnacolli
019800131118     d  wD90mde              227    236
019900131121     d* · stampante per segnacolli
020000131118     d  wD90pre              247    256
020100081009     d
020200131203     D tnsdubc         pr                  extpgm('TNSDUBC')
020300131203     D  kpjba                              likeds(kpjba)
020400140117       //
020500140117     d fnlv13r         pr                  extpgm('FNLV13R')
020600140117     d  kpjba                              likeds(kpjba)
020700140117     d  fnlv13ds                           likeds(FNLV13ds)
020800140117     d  tisi95ds                           likeds(TISI95ds)
020900140117
021000080206      //---------------------------------------------------------------
021100080206      //?Definizione procedure usate.
021200080206      //---------------------------------------------------------------
021300080414      /copy gaitrasrc/srcprotopr,tibs34r
021400100316      /copy gaitrasrc/srcprotopr,xsrda8
021500100910      /copy gaitrasrc/srcprotopr,bch10
021600101005      /copy gaitrasrc/srcprotopr,tnsdMAGr
021700140117      /copy gaitrasrc/srcprotopr,fnlv55r
021800080206
021900080206      //---------------------------------------------------------------
022000080206      //?Riepilogo indicatori.
022100080206      //---------------------------------------------------------------
022200080207      // - Comuni
022300080207      // 28    : Emissione messaggio di errore a video
022400080207      // - C01/S01
022500080207      // 30    : Condiziona SFLDSP    (*not)
022600080207      // 31    : Condiziona SFLDSPCTL (*not)
022700080207      // 30+31 : Condiziona SFLCLR
022800100824      // 32    : Condiziona SFLNXTCHG
022900080207      // 50    : Errore: Opzione errata
023000080207      // - D01
023100080207      // - Comuni
023200080207      // 99    : Generico di Errore
023300080206      //---------------------------------------------------------------
023400080206
023500080206      //---------------------------------------------------------------
023600080206      //?M A I N - L I N E
023700080206      //---------------------------------------------------------------
023800080627     c     *Entry        plist
023900080206     c                   parm                    KPJBA
024000131118     C* RICHIESTA STAMPANTI
024100131118     C                   movel     'S'           D90rse
024200140207     C***                movel     'S'           D90rsb
024300131118     C                   call      'TRUL90R'
024400131118     C                   PARM                    KPJBA
024500131118     C                   parm                    Trul90ds
024600140203     C                   parm      'SED'         k_key             3
024700131118     C**
024800131118     C* F3 - FINE
024900131118     C                   IF        D90f3 = *on
025000131118     C                   eval      $Fine = *on
025100131118     C                   ENDIF
025200131118     C*
025300080702
025400080206      /free
025500080206
025600080206       // Operazioni iniziali
025700080206       exsr RoutInz;
025800081215
025900080206       // Gestione video
026000081215 3     DOW $Fine = *off;
026100081215 4       select;
026200080530
026300080530         // Videata di selezioni
026400100312           when $Video = 'D1';
026500100312             exsr GesD01;
026600080530
026700100830         // Videata del sfl per inserimento/visualizzazione
026800100312           when $Video = 'S2';
026900100312             exsr GesS02;
027000100830
027100080702           other   ;
027200080206             $Fine = *on;
027300081215 4       endsl;
027400081215 3     ENDDO;
027500080206
027600080206       // Operazioni finali
027700080206       exsr RoutEnd;
027800080206
027900080206       //--------------------------------------------------------------
028000080206       //?Operazioni iniziali.
028100080206       //--------------------------------------------------------------
028200080206       BEGSR RoutInz;
028300100312       $inzs02=*on;
028400100312       $inzd01=*on;
028500080206
028600080703       // Solo la prima volta
028700080703 1     if primavolta=' '   ;
028800080703
028900080206         // Reperimento dati job
029000080206         exsr DatiJob;
029100100826
029200100910       // Udate - 10
029300100826       dataiso=datasys ;
029400100831       dataeur=dataiso ;
029500100903       Dataoggi=%dec(dataeur) ;
029600140117       DataoggiI=%dec(dataeur:*iso) ;
029700110221       dataiso=dataiso- %days(28) ;
029800110221       datameno28=%dec(dataiso);
029900100831 1     endif      ;
030000100827
030100080627         ENDSR;
030200080206
030300080206       //--------------------------------------------------------------
030400080206       //?Reperimento Dati del job (Utente/Operativi).
030500080206       //--------------------------------------------------------------
030600080206       BEGSR DatiJob;
030700080206
030800080206         in(E) §AzUte;
030900080206         if NOT %error;
031000080206           in(E) §DatiUte;
031100080206         endif;
031200080206         if %error or RSut = *blanks;
031300080206           clear TIBS34ds;
031400080206           tibs34r(tibs34ds);
031500080206           in §AzUte;
031600080206           in §DatiUte;
031700080206         endif;
031800080206
031900080206       ENDSR;
032000100312       //--------------------------------------------------------------
032100100312       //?Gestione viodeata 01
032200100312       //--------------------------------------------------------------
032300100312       BEGSR Gesd01;
032400100312
032500100312         // Inizializzazione videata
032600100312         if  $Inzd01 = *on;
032700100312            exsr Inzd01;
032800100312            $Inzd01  = *off;
032900100312         endif;
033000100312
033100100312         // Emissione Testata e Piede con tasti funzionali abilitati
033200100824         if  errMessage  = *off;
033300100910           write  SDUQT01;
033400100910           write  SDUQZ01;
033500100312         endif;
033600100312
033700100312         // Emissione videata
033800100910         exfmt  SDUQD01;
033900100312
034000100312         reset errMessage;
034100100312         reset errGenerico;
034200100824         clear V1cmsg    ;
034300100910         errDTM=*off     ;
034400101028         errAUM=*off     ;
034500100312
034600100312 1       SELECT;
034700100312
034800100312         // - F3=Fine
034900100312 1         WHEN  dsp_aid = c_F03;
035000100312            $Fine = *on;
035100100312
035200100312
035300100824 x1      // Invio
035400100312           OTHER;
035500100316
035600100316           exsr CTRD01  ;
035700100316
035800100316           if ErrGenerico=*ON    ;
035900100316                 leavesr;
036000100316 2             endif;
036100100316
036200100315           $video='S2' ;
036300100825           $inzs02=*on;
036400100824
036500100312 1       ENDSL;
036600100312
036700100312       ENDSR;
036800100312
036900100316       //--------------------------------------------------------------
037000100316       //?controlli video 1
037100100316       //--------------------------------------------------------------
037200100316       BEGSR CTRD01  ;
037300100316
037400100826       // Ufficio obbligatorio
037500100826       if v1ccuf=*blanks   ;
037600100826               errMessage  = *on;
037700100826               errGenerico = *on;
037800100910               V1cmsg = Msg(1);
037900100826               leavesr;
038000100826       endif  ;
038100100826
038200100910       // Data conferma movimento obbligatoria
038300100831       clear w1cdtm  ;
038400100910 1     if v1cdtm=0  ;
038500100831               errMessage  = *on;
038600100831               errGenerico = *on;
038700100831               ErrDTM    = *on;
038800100910               V1cmsg = Msg(2);
038900100831               leavesr;
039000100910 1     endif;
039100100908
039200100910 1     if v1cdtm>0  ;
039300100831             clear wlbdat;
039400100831             g02dat = v1cdtm ;
039500100831             xsrda8(wlbdat);
039600100831 2           if g02err = '1';
039700100831               errMessage  = *on;
039800100831               errGenerico = *on;
039900100831               ErrDTM    = *on;
040000100910               V1cmsg = Msg(03);
040100100831               leavesr;
040200100831 2           endif;
040300100910
040400110221             // non inferiore a 28 giorni
040500110221             if g02inv<datameno28   ;
040600100910               errMessage  = *on;
040700100910               errGenerico = *on;
040800100910               ErrDTM    = *on;
040900100910               V1cmsg = Msg(04);
041000100910               leavesr;
041100100910 2           endif;
041200100831
041300100831             v1cdtm = g02dat;
041400100831             w1cdtm = g02inv;
041500100831 1     endif   ;
041600100708
041700101028             // ristampa solo unità in aumento valido solo per RISTAMPA
041800101028             if v1caum='S' and v1cris='N'  ;
041900101028               errMessage  = *on;
042000101028               errGenerico = *on;
042100101028               ErrAUM    = *on;
042200101028               V1cmsg = Msg(10);
042300101028               leavesr;
042400101028 2           endif;
042500100316       ENDSR  ;
042600080206       //--------------------------------------------------------------
042700100312       //?Gestione SFL 02
042800080206       //--------------------------------------------------------------
042900100312       BEGSR GesS02;
043000080207
043100080207         // Inizializzazione videata
043200100312         if  $InzS02 = *on;
043300100312            exsr InzS02;
043400100824            $InzS02  = *off;
043500100824         endif;
043600100827
043700100910         // Se non trovati record errore in prima videata
043800100831         if s02nrr=0   ;
043900100910         v1cmsg=msg(08)  ;
044000100831         errGenerico=*on  ;
044100100831         ErrMessage=*on  ;
044200100831         $Video='D1'  ;
044300100831         leavesr   ;
044400100831         endif  ;
044500100910
044600100910         // Decodifico ufficio scelto
044700140219         select ;
044800140219         when v1ccuf='MAG'           ;
044900100910         EmmMAG=*on  ;
045000140219         when v1ccuf='FOR'           ;
045100100910         EmmFOR=*on  ;
045200140219         other ;
045300140219         EmmTLC=*on  ;
045400140219         ENDSL  ;
045500100831
045600100831         // Posizionamento cursore
045700100910         if  C02csr  >0     ;
045800100827           C02rcd = C02csr;
045900100827         endif  ;
046000080207
046100080207         // Emissione Testata e Piede con tasti funzionali abilitati
046200100824         if  errMessage  = *off;
046300100910           write  SDUqT01;
046400100910           write  SDUqZ02;
046500100910           write  SDUqd01;
046600080207         endif;
046700080703
046800080207         // Emissione videata
046900100910         exfmt  SDUqC02;
047000080410
047100080207         reset errMessage;
047200080207         reset errGenerico;
047300100824         clear V1cmsg;
047400100910         errNCL=*off ;
047500100910         errOPZ=*off ;
047600100910         errpkg=*off ;
047700080207
047800080523 1       SELECT;
047900080207
048000080207         // - F3=Fine
048100080523 1         WHEN  dsp_aid = c_F03;
048200080409            $Fine = *on;
048300080207
048400080207         // - F12=Ritorno
048500080523 1         WHEN  dsp_aid = c_F12;
048600100315           $video='D1'   ;
048700100910           EmmMAG=*off  ;
048800100910           EmmFOR=*off  ;
048900140219           EmmTLC=*off  ;
049000100901
049100080530 x1      // Invio / F6
049200080207           OTHER;
049300081006
049400100315               exsr ContrS02 ;
049500100910 2             if  errGenerico = *on ;
049600081006                 leavesr;
049700100824 2             endif;
049800080530
049900100907         // F6=conferma Aggiornamento
050000100910 2         if   dsp_aid = c_F06  or dsp_aid = c_F01     ;
050100100910           yy=1           ;
050200100910           s02nrr=1  ;
050300100910           chain s02nrr  sduqs02    ;
050400100910
050500100910 3         dow %found   ;
050600120525 4         if vscopz<>' '   or  dsp_aid = c_F06  ;
050700100907
050800100910           clear parAM  ;
050900100910           parmag=vscmag  ;
051000120525           // Se richiesto annullamento passo una "A" nel flag ristampa
051100120525           if vscopz='4' ;
051200120525            parris='A'  ;
051300120525           else  ;
051400120525            parris=v1cris  ;
051500120525           endif  ;
051600120525
051700131118           paraum=v1caum  ;
051800131118           parbol=vscaut  ;
051900100910           Pardat= w1cdtm  ;
052000100910           parncl=vscncl  ;
052100100910           parpkg=vscpkg  ;
052200100910           parmaut='S'  ;
052300100915           parcuf =v1ccuf   ;
052400131203
052500131203      /end-free
052600131203
052700131203      * Parametri x ovrprtf
052800131203      * · stampante per etichette
052900131203     c                   movel     D90pre        wD90pre
053000131203      * · modulo per stampa etichette
053100131203     c                   movel     D90mde        wD90mde
053200131203      * · pgm. di stampa etichette segnacolli
053300131203     c                   movel     D90pss        wD90pss
053400131203      * · stampante per LDV in formato "A4" - laser
053500131203     c                   movel     D90prb4       wD90prb4
053600131203      * · stampante per LDV in formato "A5" - laser
053700131203     c                   movel     D90prb5       wD90prb5
053800131203      * · modulo per stampa LDV su "A4" ... - laser
053900131203     c                   movel     D90mdb4       wD90mdb4
054000131203      * · modulo per stampa LDV su "A5" ... - laser
054100131203     c                   movel     D90mdb5       wD90mdb5
054200131203      * · pgm.   per stampa LDV ........... - ad aghi o laser
054300131203     c                   movel     D90psl        wD90psl
054400100910
054500131203      /free
054600100910           clear kpjbu   ;
054700100910           kpjbu=param   ;
054800100910
054900100910           kcoaz='SDUB'  ;
055000131203           //callp BCH10 (kpjba)  ;
055100131203           callp tnsdubc (kpjba)  ;
055200081023
055300100315           $video='D1'   ;
055400100910 4         endif  ;
055500100915           s02nrr=s02nrr + 1 ;
055600100910           chain s02nrr  sduqs02    ;
055700100910 3         enddo  ;
055800100915
055900100915            $Fine = *on;
056000100910 2         endif  ;
056100080207
056200100910 1       ENDSL;
056300080207
056400080207       ENDSR;
056500080526       //--------------------------------------------------------------
056600100312       //?controlli SFL02
056700080409       //--------------------------------------------------------------
056800100312       BEGSR ContrS02;
056900100315
057000100910       clear unMAG  ;
057100081126       yy=1           ;
057200100315       s02nrr=1  ;
057300100910       chain s02nrr  sduqs02    ;
057400080703
057500100910 0     dow %found   ;
057600100910
057700120525       // controllo per opzione  o per F6 tutto
057800120525 1     if vscopz<>' '   or  dsp_aid = c_F06  ;
057900100910       unmag='S'   ;
058000120525
058100120525       // OPZIONE "4" per ristampa non ammmessa
058200120525 2     if  vscopz='4' and v1cris='S'   ;
058300120525         v1cmsg=msg(11)  ;
058400120525         ErrGenerico=*on  ;
058500120525         ErrMessage =*on  ;
058600120525         ErrOPZ= *on   ;
058700120525         EXSR AggioSFL   ;
058800120525         leavesr  ;
058900120525 2     endif   ;
059000100910
059100120525       // Se F6  non ammessa opzione
059200120525 2     if dsp_aid = c_F06  and vscopz<>' '    ;
059300120525         v1cmsg=msg(05)  ;
059400100910         ErrGenerico=*on  ;
059500100910         ErrMessage =*on  ;
059600100910         ErrOPZ= *on   ;
059700100910         EXSR AggioSFL   ;
059800100910         leavesr  ;
059900100910 2     endif   ;
060000140117
060100140117       // Se richiesta bolla automatica eseguo controllo CAP
060200140117       if vscaut=' ' and v1cris='N' and vscopz<>'4';
060300140117         exsr chkcap;
060400140117         if o13err<>' ';
060500140117            v1cmsg=msg(12)   ;
060600140117            ErrGenerico=*on  ;
060700140117            ErrMessage =*on  ;
060800140117            ErrCAP= *on   ;
060900140117            EXSR AggioSFL   ;
061000140117            leavesr  ;
061100140117         endif;
061200140117       endif;
061300120525
061400100910
061500120525       // Controllo se immessi colli e peso  solo in stampa non annullato
061600120525 2     if vscncl=0  and  v1cris='N' and vscopz<>'4' ;
061700100910         v1cmsg=msg(06)  ;
061800100910         ErrGenerico=*on  ;
061900100910         ErrMessage =*on  ;
062000100910         ErrNCL= *on   ;
062100100910         EXSR AggioSFL   ;
062200100910         leavesr  ;
062300100910 2     endif   ;
062400120525 2     if vscpkg=0  and v1cris='N' and vscopz<>'4' ;
062500100910         v1cmsg=msg(07)  ;
062600100910         ErrGenerico=*on  ;
062700100910         ErrMessage =*on  ;
062800100910         ErrPKG= *on   ;
062900100910         EXSR AggioSFL   ;
063000100910         leavesr  ;
063100100910 2     endif   ;
063200100910 1     endif   ;
063300100830
063400100910         EXSR AggioSFL   ;
063500100910
063600100910       s02nrr=s02nrr +1  ;
063700100910       chain s02nrr  sduqs02    ;
063800100830 0     enddo         ;
063900100910
064000120525       // Per F1 se nessuna opzione    --> errore
064100100910       if dsp_aid = c_F01   and unMAG=' '  ;
064200100910       s02nrr=1  ;
064300100910       chain s02nrr  sduqs02    ;
064400100910         v1cmsg=msg(09)  ;
064500100910         ErrGenerico=*on  ;
064600100910         ErrMessage =*on  ;
064700100910         ErrOPZ= *on   ;
064800100910         EXSR AggioSFL   ;
064900100910         leavesr  ;
065000100910 2     endif   ;
065100100830
065200100830       ENDSR  ;
065300081126
065400080703       //--------------------------------------------------------------
065500080703       //?Aggiornamento sfl
065600080703       //--------------------------------------------------------------
065700080703       BEGSR AGGIOSFL  ;
065800100830
065900100824       if v1cmsg<>*blanks   ;
066000080703       errMessage  = *on;
066100080703       errGenerico = *on;
066200100315       c02csr=s02nrr    ;
066300080703       endif    ;
066400100824
066500100910       update  sduqs02  ;
066600100830
066700080703       ENDSR   ;
066800100312       //--------------------------------------------------------------
066900100312       //?Inizializzazione videata 01
067000100312       //--------------------------------------------------------------
067100100312       BEGSR Inzd01  ;
067200100312
067300100315         clear V1cmsg;
067400100826         errMessage  = *off;
067500100312         errGenerico = *off;
067600100910         EmmMAG=*off       ;
067700100910         EmmFOR=*off       ;
067800140219         EmmTLC=*off       ;
067900100825         v1ccuf ='MAG'   ;
068000100903         v1cdtm=Dataoggi      ;
068100100910         v1cris='N'           ;
068200101028         clear v1caum         ;
068300100831
068400100312         ENDSR    ;
068500080207       //--------------------------------------------------------------
068600100312       //?Inizializzazione SFL02
068700080207       //--------------------------------------------------------------
068800100312       BEGSR InzS02;
068900080207
069000080207       // Pulizia subfile
069100080207         SflDsp_N    = *on;
069200080207         SflDspCtl_N = *on;
069300100910         write  SDUQC02;
069400080207         SflDspCtl_N = *off;
069500080207         SflEnd      = *off;
069600100824         SflDsp_N    = *off;
069700100824         Sflnxtchg   = *off;
069800080530
069900100315         S02nrr=0 ;
070000100824         clear V1cmsg;
070100101011         clear savmag ;
070200101028         clear savaum ;
070300140220         clear totncl ;
070400140220         clear totpkg ;
070500080207         errMessage  = *off;
070600080207         errGenerico = *off;
070700100910         EmmMAG=*off       ;
070800100910         EmmFOR=*off       ;
070900140219         EmmTLC=*off       ;
071000131118         In_rista=*off       ;
071100131118
071200131118         // Se si tratta di ristampa accedno indicatore
071300131118         if v1cris='S'   ;
071400131118          IN_rista = *on  ;
071500131118         endif  ;
071600100708
071700100826
071800100823         // Predispongo le letture e carico SFL
071900100910           setll (v1ccuf:w1cdtm)             ungio03l    ;
072000100910           reade (v1ccuf:w1cdtm)             ungio03l    ;
072100100823
072200100910         dow not %eof(ungio03l)  ;
072300100823         // controllo parzializzazioni
072400100823         exsr  CtrPARZ   ;
072500101008         if Wok='S'  ;
072600101008
072700101011         // se cambiato magazzino emetto sfl  ;
072800101008         if ungmag<>savmag  and savmag>0  ;
072900101028
073000101028         // Se richiesti i magazzino solo con unità in aumento,
073100101028         //  emetto solo se ne ho trovata almeno  UNA
073200101028         if v1caum=' ' or (v1caum='S' and savaum='S')   ;
073300100823         exsr CarSF02  ;
073400100823         endif   ;
073500101028          clear totncl ;
073600101028          clear totpkg ;
073700101028          clear savaum  ;
073800101028         endif   ;
073900101008
074000101008         savmag=ungmag  ;
074100101008         totncl=ungncl + totncl   ;
074200101008         totpkg = ungpkg + totpkg  ;
074300101028
074400101028         if ungtru='A'  ;
074500101028         savaum='S'  ;
074600101028         endif   ;
074700101028
074800101008         endif  ;
074900100823
075000100910           reade (v1ccuf:w1cdtm)             ungio03l    ;
075100100823         enddo   ;
075200100910
075300101008         if  savmag>0  ;
075400101028         if v1caum=' ' or (v1caum='S' and savaum='S')   ;
075500101008         exsr CarSF02  ;
075600101008         endif   ;
075700101028         endif   ;
075800101008
075900100910         C02csr=1  ;
076000100823
076100100824         ENDSR  ;
076200100823       //--------------------------------------------------------------
076300100823       //?Controllo selezioni impostate in prima videata
076400100823       //--------------------------------------------------------------
076500100823          BEGSR   CtrPARZ    ;
076600100823          Wok='S'    ;
076700100823          // Tipo movimentazione
076800100825          if ungcuf <>v1ccuf ;
076900100823            clear wok  ;
077000100823            leavesr    ;
077100100823          endif      ;
077200100908          // solo i record confermati
077300100910          if ungfmv=' ' ;
077400100908            clear wok  ;
077500100908            leavesr    ;
077600100908          endif      ;
077700100910          // data conferma movimenti
077800100910          if v1cdtm>0   and ungdtm<>w1cdtm  ;
077900100831            clear wok  ;
078000100831            leavesr    ;
078100100831          endif      ;
078200100910          // STAMPA
078300100910          if ungfst<>' '  and v1cris='N'  ;
078400100910            clear wok     ;
078500100910            leavesr    ;
078600100910          endif      ;
078700100910
078800100910          // RISTAMPA
078900120524          if ungfst<> 'S'  and v1cris='S'  ;
079000100910            clear wok  ;
079100100910            leavesr    ;
079200100910          endif      ;
079300100916
079400100916          // Elaboro solo i magazzini destinazione che prevedono una stampa:
079500100927          //  filiali, logistica fornitori
079600100916          wmag=%editc(ungmag:'X')    ;
079700100916          wazorg='E  '   ;
079800100916          exsr  chkmag   ;
079900101005          if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
080000101005             §omaglue='S'  ;
080100101005             else  ;
080200100916            clear wok  ;
080300100916            leavesr    ;
080400100916          endif  ;
080500100916
080600100823          ENDSR  ;
080700100823       //--------------------------------------------------------------
080800100823       //?eseguo caricamento SFL
080900100823       //--------------------------------------------------------------
081000100823          BEGSR   carSF02    ;
081100100830          clear vscopz  ;
081200150109          clear vscaut  ;
081300100830
081400101008          vscmag=savmag  ;
081500100910          wazorg='E  '   ;
081600101011          wmag=%editc(vscmag:'X')    ;
081700100910          exsr  chkmag   ;
081800101005          vsdmag=§omagdes ;
081900150109          if §omagfag='Y' ;
082000150109          chain (savmag) azorg21l ;
082100150109          if %found(azorg21l) and orgftr='W' ;
082200150109          vscaut='N'  ;
082300150109          endif       ;
082400150109          endif       ;
082500100910
082600101008          vscncl = totncl  ;
082700101008          vscpkg = totpkg  ;
082800100910
082900100910          s02nrr=s02nrr+1   ;
083000100831
083100100910          write  SDUQS02;
083200101008
083300100831
083400100830          ENDSR         ;
083500100316       //--------------------------------------------------------------
083600100708       //?controllo Magazzino
083700100316       //--------------------------------------------------------------
083800100316        BEGSR ChkMag                 ;
083900101005
084000101005        clear tnsdmagds  ;
084100101005        §magmag=wmag                            ;
084200101008        §imagtorg=wazorg                        ;
084300101005        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
084400101005
084500101005        // Errore
084600101005        if §omagerr='E'   ;
084700101005         ErrGenerico=*on   ;
084800101005         §omagdes=*all'?'  ;
084900101005         clear §omagfag    ;
085000101005         clear §omaglue    ;
085100101005 2      endif    ;
085200100316
085300100316          ENDSR    ;
085400140117       //--------------------------------------------------------------
085500140117       //?controllo CAP
085600140117       //--------------------------------------------------------------
085700140117        BEGSR ChkCap                 ;
085800140117
085900140117        clear tisi95ds   ;
086000140117        clear fnlv13ds                          ;
086100140117        clear og143;
086200140117        clear og148;
086300140117        // reperisco cap e provincia del magazzino di destinazione
086400140117        // Prima lo cerco su archivio di appoggio poi su organigramma
086500140117        chain (vscmag) azorg21l ;
086600140117        if not %found(azorg21l) or orgfag<>' ';
086700140117           chain (vscmag) azorg01l ;
086800140117           og143=orgde3;
086900140117           og148=orgde8;
087000140117        endif;
087100140117        // Se logistica prendo i dati dalla filiale Bartolini correlata
087200140117        if §ogfiue>*zeros;
087300140117           w0030=%dec(§ogfiue:3:0);
087400140117           chain (w0030) azorg01l;
087500140117        endif;
087600140117        i95tcn='3'                              ;
087700140117        i13af0='S'        ;
087800140117        i13cng='S'        ;
087900140117        i95tpo='F'    ;
088000140117        i95tsp='C'    ;
088100140117        I95CAP=%editc(orgcpf:'X')  ;
088200140117        I95PRV=orgpro     ;
088300140117        I95LOC=orgloc     ;
088400140117        I95DAT=dataoggiI  ;
088500140117
088600140117        // Terminal di partenza della linea di partenza
088700140117        clear fnlv55ds  ;
088800140117        d55tpt='P';
088900140117        d55lin=102   ;
089000140117        d55drf=dataoggiI ;
089100140117        fnlv55r(fnlv55ds);
089200140117
089300140117        i95tfp=d55tfp;
089400140117        fnlv13r(kpjba:fnlv13ds:tisi95ds);
089500140117
089600140117          ENDSR    ;
089700080206       //--------------------------------------------------------------
089800080206       //?Operazioni finali.
089900080206       //--------------------------------------------------------------
090000080206       BEGSR RoutEnd;
090100080627
090200080704         // Chiusura pgm   ;
090300080206         *inLR = *on;
090400080704
090500080206         return;
090600080206
090700080206       ENDSR;
090800080206
090900080206      /end-free
091000080206
091100080206       //--------------------------------------------------------------
091200080206       //?Schiere a tempo di compilazione.
091300080206       //--------------------------------------------------------------
091400080206
091500080410** - MSG ------------------------------------------------------------------ -*
091600100910L'indicazione dell'ufficio è obbligatoria                                       01
091700100910Obbligatorio impostare data conferma movimenti                                  02
091800100910Data Errata                                                                     03
091900110221La data conferma non può essere inferiore di 28 giorni                          04
092000120525Non immettere opzioni  se premuto  F6=Conferma TOTALE
092100101008Immettere il numero dei colli   per la STAMPA                                   06
092200101008Immettere il peso il Kg (un decimale) per la STAMPA                             07
092300100910Non trovato nessun movimento per la stampa bolle nella data/ufficio indicati    08
092400100910Includere almeno un magazzino ai fini della stampa se premuto F1                09
092500101028La stampa bolle delle sole unità in AUMENTO accettata solo per RISTAMPA         10
092600120525opzione di annullamento non ammmessa per ristampa                               11
092700140117Presenti errori in indirizzo destinatario:elaborazione automatica non ammessa   12
