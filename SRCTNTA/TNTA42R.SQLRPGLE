000100140218      //--------------------------------------------------------------
000200141114      //?TNTA42R - Calcolo Fuel
000300140218      //--------------------------------------------------------------
000400140218
000500140218     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140218     h dftactgrp(*no) actgrp(*caller)
000700140218
000800140218      //---------------------------------------------------------------
000900140218      //?Dichiarazione file.
001000140218      //---------------------------------------------------------------
001100140218
001200140218      // - Video
001300141114     fTNTA42D   cf   e             workstn
001400141118     f                                     indds(IndDspF)
001500141118
001600141118      // - File VMA
001700141118     FTidpb01l  IF   E           K DISK
001800140218
001900141118      // - File Tabelle
002000141118     fTNTBE01L  if   e           k disk
002100140218      //---------------------------------------------------------------
002200140218      //?Definizione costanti.
002300140218      //---------------------------------------------------------------
002400140218
002500140218      // - Tasti funzionali a video
002600140218     d c_F01           c                   const(x'31')
002700140218     d c_F02           c                   const(x'32')
002800140218     d c_F03           c                   const(x'33')
002900140218     d c_F04           c                   const(x'34')
003000140218     d c_F05           c                   const(x'35')
003100140218     d c_F06           c                   const(x'36')
003200140218     d c_F07           c                   const(x'37')
003300140218     d c_F08           c                   const(x'38')
003400140218     d c_F09           c                   const(x'39')
003500140218     d c_F10           c                   const(x'3A')
003600140218     d c_F11           c                   const(x'3B')
003700140218     d c_F12           c                   const(x'3C')
003800140218     d c_F13           c                   const(x'B1')
003900140218     d c_F14           c                   const(x'B2')
004000140218     d c_F15           c                   const(x'B3')
004100140218     d c_F16           c                   const(x'B4')
004200140218     d c_F17           c                   const(x'B5')
004300140218     d c_F18           c                   const(x'B6')
004400140218     d c_F19           c                   const(x'B7')
004500140218     d c_F20           c                   const(x'B8')
004600140218     d c_F21           c                   const(x'B9')
004700140218     d c_F22           c                   const(x'BA')
004800140218     d c_F23           c                   const(x'BB')
004900140218     d c_F24           c                   const(x'BC')
005000140218     d c_Enter         c                   const(x'F1')
005100140218     d c_RollDown      c                   const(x'F4')
005200140218     d c_RollUp        c                   const(x'F5')
005300140218
005400140218     d Digits          c                   const('0123456789')
005500140218
005600140218      //---------------------------------------------------------------
005700140218      //?Definizione schiere.
005800140218      //---------------------------------------------------------------
005900140218
006000140218      // - Messaggi di errore
006100140218     d wMsg            s             78    dim(20) ctdata perrcd(1)
006200140218
006300141118     d $IPG            s              3  0 dim(999) inz
006400141118     d                                              ascend
006500141118     d $IPGmin         s              4  3 dim(999) inz
006600141118     d                                              ascend
006700141118     d $IPGmax         s              4  3 dim(999) inz
006800141118     d                                              ascend
006900141118      *
007000140218      //---------------------------------------------------------------
007100140218      //?Definizione aree dati.
007200140218      //---------------------------------------------------------------
007300140218
007400140218      // - Dati utente
007500140218     d §AzUte        e ds                  extname(AZUTE00F)
007600140218     d                                     dtaara
007700140218     d §DatiUte      e ds                  extname(dDatiUte)
007800140218     d                                     dtaara
007900140218
008000140218      //---------------------------------------------------------------
008100140218      //?Definizione strutture dati.
008200140218      //---------------------------------------------------------------
008300140218
008400140218      // - Status
008500140218     d Psds           sds
008600140218     d   SDSpgm          *proc
008700140218
008800140218
008900140218      // - Indicatori su DspF
009000140218     d IndDspF         ds
009100140218        // - Indicatori di errore in videata
009200140218     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300140218        // - Indicatori di errore
009400141114     d  PosCurVma                     1n   overlay(IndDspF : 31)
009500141114     d  PosCurPpo                     1n   overlay(IndDspF : 31)
009600140218        // - Indicatori di errore generico
009700140218     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800140218
009900140218     d WindDspF        ds                  inz
010000140218     d   WindDspFa             1     49    inz(*zero)
010100140218     d   WindDspFb            50     99    inz(*zero)
010200140218
010300141118      // - File Date
010400141118     d DATECNV0F     e ds                  extname(DATECNV0F)
010500141118
010600140218      // - Parametri ricevuti
010700140218     d KPJBA         e ds
010800141114     d TNTA42DS      e ds
010900141114     d sgl                     1    234  3 DIM(18)
011000141114     d itr                   235    432  3 DIM(18)
011100140218
011200140218      //---------------------------------------------------------------
011300140218      //?Definizione variabili globali.
011400140218      //---------------------------------------------------------------
011500140218
011600140218      // - Flags booleani
011700140218     d wFine           s               n   inz(*off)
011800140218     d wRecOK          s               n   inz(*off)
011900140218
012000140218      // - Indici di schiera
012100140218     d xx              s              4  0 inz
012200140218
012300140218      // - Campi associati al video
012400141118     d wVideo          s              2    inz('W1')
012500140218
012600140218      // - Campi di comodo
012700141118     d Oggi            s              8s 0 inz
012800140218
012900140218      //---------------------------------------------------------------
013000140218      //?Definizione procedure usate.
013100140218      //---------------------------------------------------------------
013200140218
013300140218      //---------------------------------------------------------------
013400140218      //?Prototipi.
013500140218      //---------------------------------------------------------------
013600140218
013700140218      //---------------------------------------------------------------
013800140218      //?Definizione key-list.
013900140218      //---------------------------------------------------------------
014000140218
014100140218      //---------------------------------------------------------------
014200140218      //?Riepilogo indicatori.
014300140218      //---------------------------------------------------------------
014400140218      // 28    : Emissione messaggio di errore a video
014500141114      // 30    : Errore VMA
014600141114      // 31    : Errore Prezzo alla pompa
014700140218      // 99    : Generico di Errore
014800140218      //---------------------------------------------------------------
014900140218
015000140218      //---------------------------------------------------------------
015100140218      //?M A I N - L I N E
015200140218      //---------------------------------------------------------------
015300140218
015400140218     c     *Entry        plist
015500140218     c                   parm                    kpjba
015600141114     c                   parm                    tnta42ds
015700140218
015800140218      /free
015900140218
016000140218       //?Operazioni iniziali
016100140218       exsr RoutInz;
016200140218
016300140218       //?Gestione video
016400140218       DOW wFine = *off;
016500140218         SELECT;
016600141114             exsr GesW01;
016700140218         ENDSL;
016800140218       ENDDO;
016900140218
017000140218       //?Operazioni finali
017100140218       exsr RoutEnd;
017200140218
017300140218       //--------------------------------------------------------------
017400140218       //?Operazioni iniziali.
017500140218       //--------------------------------------------------------------
017600140218       BEGSR RoutInz;
017700140218
017800141118       //?Imposto oggi
017900141118         Oggi = %dec(%date());
018000141118
018100141118       //?Recupero il VMA e il suo scaglione
018200141118         chain (Oggi) TIdpb01l ;
018300141118
018400141118      /end-free
018500141118      * -?Intabellamento tab. "IPG"?
018600141118     c                   clear                   xx
018700141118     c                   clear                   $IPG
018800141118     c                   clear                   $IPGmin
018900141118     c                   clear                   $IPGmax
019000141118     c                   eval      TBEcod  = 'IPG'
019100141118     c     TBEcod        setll     TNTBE000
019200141118     c     TBEcod        reade     TNTBE000
019300141118      *
019400141118    1c                   DOW       NOT  %eof(TNTBE01L)
019500141118      *
019600141118    2c                   if        TBEatb  = *blanks
019700141118     c                   movel     TBEuni        dIPG
019800141118     c                   add       1             xx
019900141118     c                   movel     TBEke1        $IPG(xx)
020000141118     c                   z-add     §IPGmin       $IPGmin(xx)
020100141118     c                   z-add     §IPGmax       $IPGmax(xx)
020200141118e   2c                   endif
020300141118      *
020400141118     c     TBEcod        reade     TNTBE000
020500141118      *
020600141118e   1c                   ENDDO
020700141118      *
020800140218
020900141118      /free
021000140218       ENDSR;
021100140218
021200140218       //--------------------------------------------------------------
021300140218       //?Gestione videata S01.
021400140218       //--------------------------------------------------------------
021500141118       BEGSR GesW01;
021600140218
021700140218       //?Inizializzazione videata
021800141118           exsr InzW01;
021900140218
022000141118       //?Emissione video con tasti funzionali abilitati
022100140218
022200140218       //?Emissione videata
022300141118         exfmt  TA42T01;
022400140218         reset ErrMessage;
022500140218         reset ErrGenerico;
022600140218         clear VC1msg;
022700140218
022800140218         SELECT;
022900140218
023000140218       //?- F03=Fine
023100140218           WHEN  dsp_aid = c_F03;
023200141118             exsr F03W01;
023300140218
023400141118       //?- F06=Calcola
023500141118           WHEN  dsp_aid = c_F06;
023600141118             exsr CtrW01;
023700141118             IF  ErrGenerico;
023800141118               leavesr;
023900141118             ENDIF;
024000141118             exsr F06W01;
024100140218       //?Invio
024200140218           OTHER;
024300141118             exsr CtrW01;
024400140224             IF  ErrGenerico;
024500140224               leavesr;
024600140224             ENDIF;
024700140218
024800140218         ENDSL;
024900140218
025000140218       ENDSR;
025100140218
025200140218       //--------------------------------------------------------------
025300141118       //?Inizializzazione videata W01.
025400140218       //--------------------------------------------------------------
025500141118       BEGSR InzW01;
025600140218
025700141118         wEndW01 = *off;
025800141118
025900141118       //?Caricamento dati da DS
026000141118         exsr RieW01;
026100140224
026200140218       ENDSR;
026300141118
026400140218       //--------------------------------------------------------------
026500141118       //?Gestione tasto funzionale F03 da videata W01
026600140218       //?F03=Fine
026700140218       //--------------------------------------------------------------
026800141118       BEGSR F03W01;
026900140218
027000140218         // Chiusura del programma
027100140218         wFine = *on;
027200140218
027300140218       ENDSR;
027400140218
027500140218       //--------------------------------------------------------------
027600141118       //?Gestione Controllo W01
027700140218       //--------------------------------------------------------------
027800141118       BEGSR CtrW01;
027900140218
028000141118           PosCurVma = *off;
028100141118           PosCurPpo = *off;
028200140218
028300140218           SELECT;
028400140218
028500141118             //?- Nessuna Vma errore
028600141118             WHEN  V1CVma  = *zeros ;
028700141118               ErrMessage  = *on;
028800141118               ErrGenerico = *on;
028900141118               PosCurVma   = *on;
029000141118               VC1msg = wMsg(01);
029100141118
029200141118             //?- VMA valorizzato controllo il suo scaglione
029300141118             WHEN  V1CVma  > *zeros ;
029400141118                   xx = 1 ;
029500141118     ///////////////////////////////////////////////////////////////////
029600141118     c     V1Cpmg        lookup    $IPGmax(xx)                        25  25
029700141118sel 1c                   SELECT
029800141118      * - Scaglione non reperito (x importo né maggiore né uguale)
029900141118w   1c                   WHEN      NOT *in25
030000141118     c                   seton                                        285190
030100141118     c                   eval      V1Dmsg  = $Msg(8)
030200141118     c                   leavesr
030300141118      * - Scaglione reperito
030400141118      *   (importo V1CPMG compreso nei range in tab. "IPG")
030500141118w   1c                   WHEN      V1Cpmg >= $IPGmin(xx)
030600141118     c                   eval      V1Csca  = $IPG(xx)
030700141118     c                   eval      V1Dsca  = 'Valido da '
030800141118     c                                     + %trim(
030900141118     c                                         %editc( $IPGmin(xx) :
031000141118     c                                         '1') )
031100141118     c                                     + ' a '
031200141118     c                                     + %trim(
031300141118     c                                         %editc( $IPGmax(xx) :
031400141118     c                                         '1') )
031500141118     c                                     + ' Euro/Litro'
031600141118     *** * - - Aumento max consentito di 2 (due) scaglioni
031700141118if  2***c                   if             V1Csca > (SAVsca + 2)
031800141118     ***c                             or   V1Csca < (SAVsca - 2)
031900141118      * - - Consentita una differenza max del ±2% rispetto all'ultimo
032000141118      *     prezzo medio inserito
032100141118      *     => Portato delta massimo al ±4%.
032200141118if  2c                   if             SAVpmg > *zeros
032300141118     c                             and (V1Cpmg > ( SAVpmg +
032400141118     c                                           ((SAVpmg * 4) / 100))
032500141118     c                              or  V1Cpmg < ( SAVpmg -
032600141118     c                                           ((SAVpmg * 4) / 100)))
032700141118if  3c                   if        %subst(knmus : 1 : 3) <> 'EDP'
032800141118     c                   seton                                        285590
032900141118     c                   eval      V1Dmsg  = $Msg(9)
033000141118     c                   leavesr
033100141118x   3c                   else
033200141118     c                   eval      V1Dtxt  = 'N.B.: '
033300141118     c                                     + %subst($Msg(9) : 1 : 58)
033400141118     c                   if        *inKF   and   $First = *on
033500141118     c                   eval      *in90   = *on
033600141118     c                   eval      $First  = *off
033700141118     c                   endif
033800141118e   3c                   endif
033900141118e   2c                   endif
034000141118      * - Scaglione reperito, ma con limite massimo inferiore
034100141118      *   all'importo in V1CPMG -DOVREBBE?essere impossibile!!!
034200141118x   1c                   OTHER
034300141118     c                   seton                                        285190
034400141118     c                   eval      V1Dmsg  = $Msg(8)
034500141118     c                   leavesr
034600141118e   1c                   ENDSL
034700141118               ErrMessage  = *on;
034800141118               ErrGenerico = *on;
034900141118               PosCurVma   = *on;
035000141118               VC1msg = wMsg(01);
035100141118
035200141118             //?- Nessun Prezzo alla Pompa errore
035300141118             WHEN  V1CPpo  = *zeros ;
035400141118               ErrMessage  = *on;
035500141118               ErrGenerico = *on;
035600141118               PosCurPpo   = *on;
035700141118               VC1msg = wMsg(02);
035800141118
035900140218
036000140218             //?- 1 = Scelta
036100140218             WHEN  VS1opz  = '1';
036200140218               //?passo i dati al chiamante ed esco dal pgm
036300140218               OTA39sca = VS1sca;
036400140218               OTA39pmg = VS1pmg;
036500140218               OTA39dtd = VS1dtd;
036600140218               wFine    = *on;
036700140218               leavesr;
036800140218
036900140218             //?- ? = Opzione NON valida
037000140218             OTHER;
037100140218               ErrMessage  = *on;
037200140218               ErrGenerico = *on;
037300140218               PosCurOpz   = *on;
037400140218               VC1msg = wMsg(01);
037500140218
037600140218           ENDSL;
037700140218
037800140218           //?Aggiornamento sfl
037900140218           SELECT;
038000140218             WHEN  ErrMessage;
038100140218               SflNxtChg = *on;
038200140218               VC1csr    = VC1rcd;
038300140218             WHEN  ErrGenerico;
038400140218               VC1csr    = VC1rcd;
038500140218               clear  VS1opz;
038600140218             OTHER;
038700140218               VC1csr    = VC1rcd;
038800140218               clear  VS1opz;
038900140218           ENDSL;
039000140218
039100140218           update TA39S01;
039200140218
039300140218           IF  ErrMessage or ErrGenerico;
039400140218             leave;
039500140218           ENDIF;
039600140218
039700140218           readc TA39S01;
039800140218
039900140218         ENDDO;
040000140218
040100140218       ENDSR;
040200140218
040300140218       //--------------------------------------------------------------
040400140218       //?Operazioni finali.
040500140218       //--------------------------------------------------------------
040600140218       BEGSR RoutEnd;
040700140218
040800140218         *inLR = *on;
040900140218         return;
041000140218
041100140218       ENDSR;
041200140218
041300140218      /end-free
041400140218
041500140218       //--------------------------------------------------------------
041600140218       //?Schiere a tempo di compilazione.
041700140218       //--------------------------------------------------------------
041800140218
041900140218** - $MSG -------------------------------------------------------------------*
042000141118VMA errato , inserire un valore corretto                                       1
042100141118Il Prezzo alla Pompa non è corretto , inserire un valore corretto              2
