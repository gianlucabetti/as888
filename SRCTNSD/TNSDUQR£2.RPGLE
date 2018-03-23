000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
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
009800131118     d  in_rista                      1n   overlay(IndDspF : 05)
009900100910     d  ErrDTM                        1n   overlay(IndDspF : 40)
010000100910     d  ErrOPZ                        1n   overlay(IndDspF : 41)
010100100910     d  ErrNCL                        1n   overlay(IndDspF : 42)
010200100910     d  ErrPkg                        1n   overlay(IndDspF : 43)
010300101028     d  ErrAUM                        1n   overlay(IndDspF : 44)
010301140117     d  ErrCAP                        1n   overlay(IndDspF : 45)
010400080206
010500080206      // - Parametri ricevuti
010600080206     d KPJBA         e ds
010700080206
010800080206      // - Reperimento dati utente
010900080206     d TIBS34ds      e ds
011000080206     d dUte01        e ds
011100080702     d DLAT          e ds
011200080702
011300100927     d og143         e ds
011301140117     d og148         e ds
011400100927     d DUNT          e ds
011500100901     d DSUNIT        e ds
011600100827     d TNSDV0DS      e ds
011700101005       // Ds controllo magazzino
011800101005     d tnsdmagds     e ds
011900131118     D* DS PER TRUL90R - RICHIESTA STAMPANTI
012000131118     D Trul90ds      e ds                  inz
012001140117
012002140117     d fnlv55ds      e ds
012003140117     d fnlv13ds      e ds
012004140117     d tisi95ds      e ds
012100080206
012200100316
012300100316     d wlbdat          ds                  inz
012400100316     d  g02dat                 1      8  0
012500100316     d  g02inv                 9     16  0
012600100316     d  g02err                17     17
012700100316     d  g02tgi                18     22  0
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
014100080627     d Indx            s              3  0 inz
014200080703     d Primavolta      s              1    inz
014300081126     d yy              s              3  0 inz
014400100831     d Dataoggi        s              8  0 inz
014401140117     d DataoggiI       s              8  0 inz
014500110221     d Datameno28      s              8  0
014600100910     d W1cdtm          s              8  0
014700100908     d wazorg          s              3    Inz('   ')
014800100316     d wmag            s              3
014900100910     d savmag          s                   like(ungmag)
015000101028     d savaum          s                   like(ungtru)
015100100824     d wOK             s              1
015200100910     d unMAG           s              1
015300101008     d totncl          s                   like(ungncl)
015400101008     d totpkg          s                   like(ungpkg)
015401140117     d w0030           s              3  0
015500080414
015600100315     d Dataeur         s               d   datfmt(*eur)
015700100315     d Dataiso         s               d   datfmt(*iso)
015800080605     d Dataymd         s               d   datfmt(*ymd)
015900080605     d Datadmy         s               d   datfmt(*dmy)
016000100901     d Datasys         s               d   inz(*sys)
016100100901     d timeiso         s               t   timfmt(*iso)
016200100910     D*----------------
016300100910     D* PARAMETRI LANCIO
016400100910     D*----------------
016500100910     D PARAM           DS
016600100910     D  PARMAG                 1      3  0
016700100910     D  PARDAT                 4     11  0
016800100910     D  PARRIS                12     12
016900100910     D  PARncl                13     17  0
017000100910     D  PARpkg                18     24  1
017100100910     D  PARMAUT               25     25
017200100915     D  PARcuf                26     28
017300101028     D  PARaum                29     29
017400131118     D  PARbol                30     30
017500131118     d* - parametri da TRUL90R:
017600131118     d* · pgm.   per stampa LDV
017700131118     d  wD90psl              157    166
017800131118     d* · stampante per LDV in formato "A4" (laser)
017900131118     d  wD90prb4             167    176
018000131118     d* · stampante per LDV in formato "A5" (laser)
018100131118     d  wD90prb5             177    186
018200131118     d* · modulo per stampa LDV su "A4"     (laser)
018300131118     d  wD90mdb4             187    196
018400131118     d* · modulo per stampa LDV su "A5"     (laser)
018500131118     d  wD90mdb5             197    206
018600131118     d* · pgm di stampa etichette segnacolli
018700131118     d  wD90pss              207    216
018800131121     d* · modulo per stampa segnacolli
018900131118     d  wD90mde              227    236
019000131121     d* · stampante per segnacolli
019100131118     d  wD90pre              247    256
019200081009     d
019201131203     D tnsdubc         pr                  extpgm('TNSDUBC')
019202131203     D  kpjba                              likeds(kpjba)
019203140117       //
019204140117     d fnlv13r         pr                  extpgm('FNLV13R')
019205140117     d  kpjba                              likeds(kpjba)
019206140117     d  fnlv13ds                           likeds(FNLV13ds)
019207140117     d  tisi95ds                           likeds(TISI95ds)
019210140117
019300080206      //---------------------------------------------------------------
019400080206      //?Definizione procedure usate.
019500080206      //---------------------------------------------------------------
019600080414      /copy gaitrasrc/srcprotopr,tibs34r
019700100316      /copy gaitrasrc/srcprotopr,xsrda8
019800100910      /copy gaitrasrc/srcprotopr,bch10
019900101005      /copy gaitrasrc/srcprotopr,tnsdMAGr
019901140117      /copy gaitrasrc/srcprotopr,fnlv55r
020000080206
020100080206      //---------------------------------------------------------------
020200080206      //?Riepilogo indicatori.
020300080206      //---------------------------------------------------------------
020400080207      // - Comuni
020500080207      // 28    : Emissione messaggio di errore a video
020600080207      // - C01/S01
020700080207      // 30    : Condiziona SFLDSP    (*not)
020800080207      // 31    : Condiziona SFLDSPCTL (*not)
020900080207      // 30+31 : Condiziona SFLCLR
021000100824      // 32    : Condiziona SFLNXTCHG
021100080207      // 50    : Errore: Opzione errata
021200080207      // - D01
021300080207      // - Comuni
021400080207      // 99    : Generico di Errore
021500080206      //---------------------------------------------------------------
021600080206
021700080206      //---------------------------------------------------------------
021800080206      //?M A I N - L I N E
021900080206      //---------------------------------------------------------------
022000080627     c     *Entry        plist
022100080206     c                   parm                    KPJBA
022200131118     C* RICHIESTA STAMPANTI
022300131118     C                   movel     'S'           D90rse
022400140207     C***                movel     'S'           D90rsb
022500131118     C                   call      'TRUL90R'
022600131118     C                   PARM                    KPJBA
022700131118     C                   parm                    Trul90ds
022701140203     C                   parm      'SED'         k_key             3
022800131118     C**
022900131118     C* F3 - FINE
023000131118     C                   IF        D90f3 = *on
023100131118     C                   eval      $Fine = *on
023200131118     C                   ENDIF
023300131118     C*
025100080702
025200080206      /free
025300080206
025400080206       // Operazioni iniziali
025500080206       exsr RoutInz;
025600081215
025700080206       // Gestione video
025800081215 3     DOW $Fine = *off;
025900081215 4       select;
026000080530
026100080530         // Videata di selezioni
026200100312           when $Video = 'D1';
026300100312             exsr GesD01;
026400080530
026500100830         // Videata del sfl per inserimento/visualizzazione
026600100312           when $Video = 'S2';
026700100312             exsr GesS02;
026800100830
026900080702           other   ;
027000080206             $Fine = *on;
027100081215 4       endsl;
027200081215 3     ENDDO;
027300080206
027400080206       // Operazioni finali
027500080206       exsr RoutEnd;
027600080206
027700080206       //--------------------------------------------------------------
027800080206       //?Operazioni iniziali.
027900080206       //--------------------------------------------------------------
028000080206       BEGSR RoutInz;
028100100312       $inzs02=*on;
028200100312       $inzd01=*on;
028300080206
028400080703       // Solo la prima volta
028500080703 1     if primavolta=' '   ;
028600080703
028700080206         // Reperimento dati job
028800080206         exsr DatiJob;
028900100826
029000100910       // Udate - 10
029100100826       dataiso=datasys ;
029200100831       dataeur=dataiso ;
029300100903       Dataoggi=%dec(dataeur) ;
029301140117       DataoggiI=%dec(dataeur:*iso) ;
029400110221       dataiso=dataiso- %days(28) ;
029500110221       datameno28=%dec(dataiso);
029600100831 1     endif      ;
029700100827
029800080627         ENDSR;
029900080206
030000080206       //--------------------------------------------------------------
030100080206       //?Reperimento Dati del job (Utente/Operativi).
030200080206       //--------------------------------------------------------------
030300080206       BEGSR DatiJob;
030400080206
030500080206         in(E) §AzUte;
030600080206         if NOT %error;
030700080206           in(E) §DatiUte;
030800080206         endif;
030900080206         if %error or RSut = *blanks;
031000080206           clear TIBS34ds;
031100080206           tibs34r(tibs34ds);
031200080206           in §AzUte;
031300080206           in §DatiUte;
031400080206         endif;
031500080206
031600080206       ENDSR;
031700100312       //--------------------------------------------------------------
031800100312       //?Gestione viodeata 01
031900100312       //--------------------------------------------------------------
032000100312       BEGSR Gesd01;
032100100312
032200100312         // Inizializzazione videata
032300100312         if  $Inzd01 = *on;
032400100312            exsr Inzd01;
032500100312            $Inzd01  = *off;
032600100312         endif;
032700100312
032800100312         // Emissione Testata e Piede con tasti funzionali abilitati
032900100824         if  errMessage  = *off;
033000100910           write  SDUQT01;
033100100910           write  SDUQZ01;
033200100312         endif;
033300100312
033400100312         // Emissione videata
033500100910         exfmt  SDUQD01;
033600100312
033700100312         reset errMessage;
033800100312         reset errGenerico;
033900100824         clear V1cmsg    ;
034000100910         errDTM=*off     ;
034100101028         errAUM=*off     ;
034200100312
034300100312 1       SELECT;
034400100312
034500100312         // - F3=Fine
034600100312 1         WHEN  dsp_aid = c_F03;
034700100312            $Fine = *on;
034800100312
034900100312
035000100824 x1      // Invio
035100100312           OTHER;
035200100316
035300100316           exsr CTRD01  ;
035400100316
035500100316           if ErrGenerico=*ON    ;
035600100316                 leavesr;
035700100316 2             endif;
035800100316
035900100315           $video='S2' ;
036000100825           $inzs02=*on;
036100100824
036200100312 1       ENDSL;
036300100312
036400100312       ENDSR;
036500100312
036600100316       //--------------------------------------------------------------
036700100316       //?controlli video 1
036800100316       //--------------------------------------------------------------
036900100316       BEGSR CTRD01  ;
037000100316
037100100826       // Ufficio obbligatorio
037200100826       if v1ccuf=*blanks   ;
037300100826               errMessage  = *on;
037400100826               errGenerico = *on;
037500100910               V1cmsg = Msg(1);
037600100826               leavesr;
037700100826       endif  ;
037800100826
037900100910       // Data conferma movimento obbligatoria
038000100831       clear w1cdtm  ;
038100100910 1     if v1cdtm=0  ;
038200100831               errMessage  = *on;
038300100831               errGenerico = *on;
038400100831               ErrDTM    = *on;
038500100910               V1cmsg = Msg(2);
038600100831               leavesr;
038700100910 1     endif;
038800100908
038900100910 1     if v1cdtm>0  ;
039000100831             clear wlbdat;
039100100831             g02dat = v1cdtm ;
039200100831             xsrda8(wlbdat);
039300100831 2           if g02err = '1';
039400100831               errMessage  = *on;
039500100831               errGenerico = *on;
039600100831               ErrDTM    = *on;
039700100910               V1cmsg = Msg(03);
039800100831               leavesr;
039900100831 2           endif;
040000100910
040100110221             // non inferiore a 28 giorni
040200110221             if g02inv<datameno28   ;
040300100910               errMessage  = *on;
040400100910               errGenerico = *on;
040500100910               ErrDTM    = *on;
040600100910               V1cmsg = Msg(04);
040700100910               leavesr;
040800100910 2           endif;
040900100831
041000100831             v1cdtm = g02dat;
041100100831             w1cdtm = g02inv;
041200100831 1     endif   ;
041300100708
041400101028             // ristampa solo unità in aumento valido solo per RISTAMPA
041500101028             if v1caum='S' and v1cris='N'  ;
041600101028               errMessage  = *on;
041700101028               errGenerico = *on;
041800101028               ErrAUM    = *on;
041900101028               V1cmsg = Msg(10);
042000101028               leavesr;
042100101028 2           endif;
042200100316       ENDSR  ;
042300080206       //--------------------------------------------------------------
042400100312       //?Gestione SFL 02
042500080206       //--------------------------------------------------------------
042600100312       BEGSR GesS02;
042700080207
042800080207         // Inizializzazione videata
042900100312         if  $InzS02 = *on;
043000100312            exsr InzS02;
043100100824            $InzS02  = *off;
043200100824         endif;
043300100827
043400100910         // Se non trovati record errore in prima videata
043500100831         if s02nrr=0   ;
043600100910         v1cmsg=msg(08)  ;
043700100831         errGenerico=*on  ;
043800100831         ErrMessage=*on  ;
043900100831         $Video='D1'  ;
044000100831         leavesr   ;
044100100831         endif  ;
044200100910
044300100910         // Decodifico ufficio scelto
044400100910         if v1ccuf='MAG'           ;
044500100910         EmmMAG=*on  ;
044600100910         ELSE  ;
044700100910         EmmFOR=*on  ;
044800100910         ENDIF  ;
044900100831
045000100831         // Posizionamento cursore
045100100910         if  C02csr  >0     ;
045200100827           C02rcd = C02csr;
045300100827         endif  ;
045400080207
045500080207         // Emissione Testata e Piede con tasti funzionali abilitati
045600100824         if  errMessage  = *off;
045700100910           write  SDUqT01;
045800100910           write  SDUqZ02;
045900100910           write  SDUqd01;
046000080207         endif;
046100080703
046200080207         // Emissione videata
046300100910         exfmt  SDUqC02;
046400080410
046500080207         reset errMessage;
046600080207         reset errGenerico;
046700100824         clear V1cmsg;
046800100910         errNCL=*off ;
046900100910         errOPZ=*off ;
047000100910         errpkg=*off ;
047100080207
047200080523 1       SELECT;
047300080207
047400080207         // - F3=Fine
047500080523 1         WHEN  dsp_aid = c_F03;
047600080409            $Fine = *on;
047700080207
047800080207         // - F12=Ritorno
047900080523 1         WHEN  dsp_aid = c_F12;
048000100315           $video='D1'   ;
048100100910           EmmMAG=*off  ;
048200100910           EmmFOR=*off  ;
048300100901
048400080530 x1      // Invio / F6
048500080207           OTHER;
048600081006
048700100315               exsr ContrS02 ;
048800100910 2             if  errGenerico = *on ;
048900081006                 leavesr;
049000100824 2             endif;
049100080530
049200100907         // F6=conferma Aggiornamento
049300100910 2         if   dsp_aid = c_F06  or dsp_aid = c_F01     ;
049400100910           yy=1           ;
049500100910           s02nrr=1  ;
049600100910           chain s02nrr  sduqs02    ;
049700100910
049800100910 3         dow %found   ;
049900120525 4         if vscopz<>' '   or  dsp_aid = c_F06  ;
050000100907
050100100910           clear parAM  ;
050200100910           parmag=vscmag  ;
050300120525           // Se richiesto annullamento passo una "A" nel flag ristampa
050400120525           if vscopz='4' ;
050500120525            parris='A'  ;
050600120525           else  ;
050700120525            parris=v1cris  ;
050800120525           endif  ;
050900120525
051000131118           paraum=v1caum  ;
051100131118           parbol=vscaut  ;
051200100910           Pardat= w1cdtm  ;
051300100910           parncl=vscncl  ;
051400100910           parpkg=vscpkg  ;
051500100910           parmaut='S'  ;
051600100915           parcuf =v1ccuf   ;
051601131203
051602131203      /end-free
051603131203
051604131203      * Parametri x ovrprtf
051605131203      * · stampante per etichette
051606131203     c                   movel     D90pre        wD90pre
051607131203      * · modulo per stampa etichette
051608131203     c                   movel     D90mde        wD90mde
051609131203      * · pgm. di stampa etichette segnacolli
051610131203     c                   movel     D90pss        wD90pss
051611131203      * · stampante per LDV in formato "A4" - laser
051612131203     c                   movel     D90prb4       wD90prb4
051613131203      * · stampante per LDV in formato "A5" - laser
051614131203     c                   movel     D90prb5       wD90prb5
051615131203      * · modulo per stampa LDV su "A4" ... - laser
051616131203     c                   movel     D90mdb4       wD90mdb4
051617131203      * · modulo per stampa LDV su "A5" ... - laser
051618131203     c                   movel     D90mdb5       wD90mdb5
051619131203      * · pgm.   per stampa LDV ........... - ad aghi o laser
051620131203     c                   movel     D90psl        wD90psl
051700100910
051701131203      /free
051800100910           clear kpjbu   ;
051900100910           kpjbu=param   ;
052000100910
052100100910           kcoaz='SDUB'  ;
052200131203           //callp BCH10 (kpjba)  ;
052201131203           callp tnsdubc (kpjba)  ;
052300081023
052400100315           $video='D1'   ;
052500100910 4         endif  ;
052600100915           s02nrr=s02nrr + 1 ;
052700100910           chain s02nrr  sduqs02    ;
052800100910 3         enddo  ;
052900100915
053000100915            $Fine = *on;
053100100910 2         endif  ;
053200080207
053300100910 1       ENDSL;
053400080207
053500080207       ENDSR;
053600080526       //--------------------------------------------------------------
053700100312       //?controlli SFL02
053800080409       //--------------------------------------------------------------
053900100312       BEGSR ContrS02;
054000100315
054100100910       clear unMAG  ;
054200081126       yy=1           ;
054300100315       s02nrr=1  ;
054400100910       chain s02nrr  sduqs02    ;
054500080703
054600100910 0     dow %found   ;
054700100910
054800120525       // controllo per opzione  o per F6 tutto
054900120525 1     if vscopz<>' '   or  dsp_aid = c_F06  ;
055000100910       unmag='S'   ;
055100120525
055200120525       // OPZIONE "4" per ristampa non ammmessa
055300120525 2     if  vscopz='4' and v1cris='S'   ;
055400120525         v1cmsg=msg(11)  ;
055500120525         ErrGenerico=*on  ;
055600120525         ErrMessage =*on  ;
055700120525         ErrOPZ= *on   ;
055800120525         EXSR AggioSFL   ;
055900120525         leavesr  ;
056000120525 2     endif   ;
056100100910
056200120525       // Se F6  non ammessa opzione
056300120525 2     if dsp_aid = c_F06  and vscopz<>' '    ;
056400120525         v1cmsg=msg(05)  ;
056500100910         ErrGenerico=*on  ;
056600100910         ErrMessage =*on  ;
056700100910         ErrOPZ= *on   ;
056800100910         EXSR AggioSFL   ;
056900100910         leavesr  ;
057000100910 2     endif   ;
057001140117
057002140117       // Se richiesta bolla automatica eseguo controllo CAP
057003140117       if vscaut=' ' and v1cris='N' and vscopz<>'4';
057004140117         exsr chkcap;
057005140117         if o13err<>' ';
057006140117            v1cmsg=msg(12)   ;
057007140117            ErrGenerico=*on  ;
057008140117            ErrMessage =*on  ;
057009140117            ErrCAP= *on   ;
057010140117            EXSR AggioSFL   ;
057011140117            leavesr  ;
057012140117         endif;
057013140117       endif;
057100120525
057200100910
057300120525       // Controllo se immessi colli e peso  solo in stampa non annullato
057400120525 2     if vscncl=0  and  v1cris='N' and vscopz<>'4' ;
057500100910         v1cmsg=msg(06)  ;
057600100910         ErrGenerico=*on  ;
057700100910         ErrMessage =*on  ;
057800100910         ErrNCL= *on   ;
057900100910         EXSR AggioSFL   ;
058000100910         leavesr  ;
058100100910 2     endif   ;
058200120525 2     if vscpkg=0  and v1cris='N' and vscopz<>'4' ;
058300100910         v1cmsg=msg(07)  ;
058400100910         ErrGenerico=*on  ;
058500100910         ErrMessage =*on  ;
058600100910         ErrPKG= *on   ;
058700100910         EXSR AggioSFL   ;
058800100910         leavesr  ;
058900100910 2     endif   ;
059000100910 1     endif   ;
059100100830
059200100910         EXSR AggioSFL   ;
059300100910
059400100910       s02nrr=s02nrr +1  ;
059500100910       chain s02nrr  sduqs02    ;
059600100830 0     enddo         ;
059700100910
059800120525       // Per F1 se nessuna opzione    --> errore
059900100910       if dsp_aid = c_F01   and unMAG=' '  ;
060000100910       s02nrr=1  ;
060100100910       chain s02nrr  sduqs02    ;
060200100910         v1cmsg=msg(09)  ;
060300100910         ErrGenerico=*on  ;
060400100910         ErrMessage =*on  ;
060500100910         ErrOPZ= *on   ;
060600100910         EXSR AggioSFL   ;
060700100910         leavesr  ;
060800100910 2     endif   ;
060900100830
061000100830       ENDSR  ;
061100081126
061200080703       //--------------------------------------------------------------
061300080703       //?Aggiornamento sfl
061400080703       //--------------------------------------------------------------
061500080703       BEGSR AGGIOSFL  ;
061600100830
061700100824       if v1cmsg<>*blanks   ;
061800080703       errMessage  = *on;
061900080703       errGenerico = *on;
062000100315       c02csr=s02nrr    ;
062100080703       endif    ;
062200100824
062300100910       update  sduqs02  ;
062400100830
062500080703       ENDSR   ;
062600100312       //--------------------------------------------------------------
062700100312       //?Inizializzazione videata 01
062800100312       //--------------------------------------------------------------
062900100312       BEGSR Inzd01  ;
063000100312
063100100315         clear V1cmsg;
063200100826         errMessage  = *off;
063300100312         errGenerico = *off;
063400100910         EmmMAG=*off       ;
063500100910         EmmFOR=*off       ;
063600100825         v1ccuf ='MAG'   ;
063700100903         v1cdtm=Dataoggi      ;
063800100910         v1cris='N'           ;
063900101028         clear v1caum         ;
064000100831
064100100312         ENDSR    ;
064200080207       //--------------------------------------------------------------
064300100312       //?Inizializzazione SFL02
064400080207       //--------------------------------------------------------------
064500100312       BEGSR InzS02;
064600080207
064700080207       // Pulizia subfile
064800080207         SflDsp_N    = *on;
064900080207         SflDspCtl_N = *on;
065000100910         write  SDUQC02;
065100080207         SflDspCtl_N = *off;
065200080207         SflEnd      = *off;
065300100824         SflDsp_N    = *off;
065400100824         Sflnxtchg   = *off;
065500080530
065600100315         S02nrr=0 ;
065700100824         clear V1cmsg;
065800101011         clear savmag ;
065900101028         clear savaum ;
066000080207         errMessage  = *off;
066100080207         errGenerico = *off;
066200100910         EmmMAG=*off       ;
066300100910         EmmFOR=*off       ;
066400131118         In_rista=*off       ;
066500131118
066600131118         // Se si tratta di ristampa accedno indicatore
066700131118         if v1cris='S'   ;
066800131118          IN_rista = *on  ;
066900131118         endif  ;
067000100708
067100100826
067200100823         // Predispongo le letture e carico SFL
067300100910           setll (v1ccuf:w1cdtm)             ungio03l    ;
067400100910           reade (v1ccuf:w1cdtm)             ungio03l    ;
067500100823
067600100910         dow not %eof(ungio03l)  ;
067700100823         // controllo parzializzazioni
067800100823         exsr  CtrPARZ   ;
067900101008         if Wok='S'  ;
068000101008
068100101011         // se cambiato magazzino emetto sfl  ;
068200101008         if ungmag<>savmag  and savmag>0  ;
068300101028
068400101028         // Se richiesti i magazzino solo con unità in aumento,
068500101028         //  emetto solo se ne ho trovata almeno  UNA
068600101028         if v1caum=' ' or (v1caum='S' and savaum='S')   ;
068700100823         exsr CarSF02  ;
068800100823         endif   ;
068900101028          clear totncl ;
069000101028          clear totpkg ;
069100101028          clear savaum  ;
069200101028         endif   ;
069300101008
069400101008         savmag=ungmag  ;
069500101008         totncl=ungncl + totncl   ;
069600101008         totpkg = ungpkg + totpkg  ;
069700101028
069800101028         if ungtru='A'  ;
069900101028         savaum='S'  ;
070000101028         endif   ;
070100101028
070200101008         endif  ;
070300100823
070400100910           reade (v1ccuf:w1cdtm)             ungio03l    ;
070500100823         enddo   ;
070600100910
070700101008         if  savmag>0  ;
070800101028         if v1caum=' ' or (v1caum='S' and savaum='S')   ;
070900101008         exsr CarSF02  ;
071000101008         endif   ;
071100101028         endif   ;
071200101008
071300100910         C02csr=1  ;
071400100823
071500100824         ENDSR  ;
071600100823       //--------------------------------------------------------------
071700100823       //?Controllo selezioni impostate in prima videata
071800100823       //--------------------------------------------------------------
071900100823          BEGSR   CtrPARZ    ;
072000100823          Wok='S'    ;
072100100823          // Tipo movimentazione
072200100825          if ungcuf <>v1ccuf ;
072300100823            clear wok  ;
072400100823            leavesr    ;
072500100823          endif      ;
072600100908          // solo i record confermati
072700100910          if ungfmv=' ' ;
072800100908            clear wok  ;
072900100908            leavesr    ;
073000100908          endif      ;
073100100910          // data conferma movimenti
073200100910          if v1cdtm>0   and ungdtm<>w1cdtm  ;
073300100831            clear wok  ;
073400100831            leavesr    ;
073500100831          endif      ;
073600100910          // STAMPA
073700100910          if ungfst<>' '  and v1cris='N'  ;
073800100910            clear wok     ;
073900100910            leavesr    ;
074000100910          endif      ;
074100100910
074200100910          // RISTAMPA
074300120524          if ungfst<> 'S'  and v1cris='S'  ;
074400100910            clear wok  ;
074500100910            leavesr    ;
074600100910          endif      ;
074700100916
074800100916          // Elaboro solo i magazzini destinazione che prevedono una stampa:
074900100927          //  filiali, logistica fornitori
075000100916          wmag=%editc(ungmag:'X')    ;
075100100916          wazorg='E  '   ;
075200100916          exsr  chkmag   ;
075300101005          if §omagfag='Y' or §omagfag='F' or §omagfag='A'  or
075400101005             §omaglue='S'  ;
075500101005             else  ;
075600100916            clear wok  ;
075700100916            leavesr    ;
075800100916          endif  ;
075900100916
076000100823          ENDSR  ;
076100100823       //--------------------------------------------------------------
076200100823       //?eseguo caricamento SFL
076300100823       //--------------------------------------------------------------
076400100823          BEGSR   carSF02    ;
076500100830          clear vscopz  ;
076600100830
076700101008          vscmag=savmag  ;
076800100910          wazorg='E  '   ;
076900101011          wmag=%editc(vscmag:'X')    ;
077000100910          exsr  chkmag   ;
077100101005          vsdmag=§omagdes ;
077200100910
077300101008          vscncl = totncl  ;
077400101008          vscpkg = totpkg  ;
077500100910
077600100910          s02nrr=s02nrr+1   ;
077700100831
077800100910          write  SDUQS02;
077900101008
078000100831
078100100830          ENDSR         ;
078200100316       //--------------------------------------------------------------
078300100708       //?controllo Magazzino
078400100316       //--------------------------------------------------------------
078500100316        BEGSR ChkMag                 ;
078600101005
078700101005        clear tnsdmagds  ;
078800101005        §magmag=wmag                            ;
078900101008        §imagtorg=wazorg                        ;
079000101005        callp     TNSDMAGR  (kpjba:tnsdmagds)   ;
079100101005
079200101005        // Errore
079300101005        if §omagerr='E'   ;
079400101005         ErrGenerico=*on   ;
079500101005         §omagdes=*all'?'  ;
079600101005         clear §omagfag    ;
079700101005         clear §omaglue    ;
079800101005 2      endif    ;
079900100316
080000100316          ENDSR    ;
080001140117       //--------------------------------------------------------------
080002140117       //?controllo CAP
080003140117       //--------------------------------------------------------------
080004140117        BEGSR ChkCap                 ;
080005140117
080006140117        clear tisi95ds   ;
080007140117        clear fnlv13ds                          ;
080008140117        clear og143;
080009140117        clear og148;
080010140117        // reperisco cap e provincia del magazzino di destinazione
080011140117        // Prima lo cerco su archivio di appoggio poi su organigramma
080012140117        chain (vscmag) azorg21l ;
080013140117        if not %found(azorg21l) or orgfag<>' ';
080014140117           chain (vscmag) azorg01l ;
080015140117           og143=orgde3;
080016140117           og148=orgde8;
080017140117        endif;
080018140117        // Se logistica prendo i dati dalla filiale Bartolini correlata
080019140117        if §ogfiue>*zeros;
080020140117           w0030=%dec(§ogfiue:3:0);
080021140117           chain (w0030) azorg01l;
080023140117        endif;
080024140117        i95tcn='3'                              ;
080025140117        i13af0='S'        ;
080026140117        i13cng='S'        ;
080027140117        i95tpo='F'    ;
080028140117        i95tsp='C'    ;
080029140117        I95CAP=%editc(orgcpf:'X')  ;
080030140117        I95PRV=orgpro     ;
080031140117        I95LOC=orgloc     ;
080032140117        I95DAT=dataoggiI  ;
080033140117
080034140117        // Terminal di partenza della linea di partenza
080035140117        clear fnlv55ds  ;
080036140117        d55tpt='P';
080037140117        d55lin=102   ;
080038140117        d55drf=dataoggiI ;
080039140117        fnlv55r(fnlv55ds);
080040140117
080041140117        i95tfp=d55tfp;
080042140117        fnlv13r(kpjba:fnlv13ds:tisi95ds);
080045140117
080046140117          ENDSR    ;
080100080206       //--------------------------------------------------------------
080200080206       //?Operazioni finali.
080300080206       //--------------------------------------------------------------
080400080206       BEGSR RoutEnd;
080500080627
080600080704         // Chiusura pgm   ;
080700080206         *inLR = *on;
080800080704
080900080206         return;
081000080206
081100080206       ENDSR;
081200080206
081300080206      /end-free
081400080206
081500080206       //--------------------------------------------------------------
081600080206       //?Schiere a tempo di compilazione.
081700080206       //--------------------------------------------------------------
081800080206
081900080410** - MSG ------------------------------------------------------------------ -*
082000100910L'indicazione dell'ufficio è obbligatoria                                       01
082100100910Obbligatorio impostare data conferma movimenti                                  02
082200100910Data Errata                                                                     03
082300110221La data conferma non può essere inferiore di 28 giorni                          04
082400120525Non immettere opzioni  se premuto  F6=Conferma TOTALE
082500101008Immettere il numero dei colli   per la STAMPA                                   06
082600101008Immettere il peso il Kg (un decimale) per la STAMPA                             07
082700100910Non trovato nessun movimento per la stampa bolle nella data/ufficio indicati    08
082800100910Includere almeno un magazzino ai fini della stampa se premuto F1                09
082900101028La stampa bolle delle sole unità in AUMENTO accettata solo per RISTAMPA         10
083000120525opzione di annullamento non ammmessa per ristampa                               11
083100140117Presenti errori in indirizzo destinatario:elaborazione automatica non ammessa   12
