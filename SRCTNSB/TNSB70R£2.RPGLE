000100040119      *===============================================================*
000200120605      * TNSB70R    * Scrive TIJDC e FIAR5 per Documenti al Mittente   *
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000040331     fAZORG01L  if   e           k disk
001100061227     fTNTBE01L  if   e           k disk
001200061227     fTABEL00F  if   e           k disk
001300040331      *
001400120604     fTIJDG00R  Uf   e           k disk
001500040119      *
001600120604     fTITAS30C  if   e           k disk
001700120604     fFIAR531C  uf a e           k disk
001800040331      *
001900120604     fTIJDC01L  Uf a e           k disk
002000120604     fTIJDD00F  O    e             disk
002100121212     fTILAC04L  uf a e           k disk    prefix('L')
002200040119
002300040119      *---------------------------------------------------------------*
002400061227
002500040331      *
002600040331      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002700040331      *
002800120604     d $clijdc         s              7  0 dim(999) inz
002900120604     d $dirjdc         s                   like(§jdcdir) dim(999) inz
003000120604     d $tpijdc         s                   like(§jdctpi) dim(999) inz
003100120604     d $fmijdc         s                   like(§jdcfmi) dim(999) inz
003200120604     d $ksujdc         s                   like(§jdcksu) dim(999) inz
003300120605     d $tadujdc        s                   like(§jdctadu) dim(999) inz
003400120607     d $pagjdc         s                   like(§jdcpag) dim(999) inz
003500120605      * per ora questi campi non li utilizziamo
003600120605     d $tadjdc         s                   like(§jdctad) dim(999) inz
003700120605     d $tptjdc         s                   like(§jdctpt) dim(999) inz
003800120605     d $resjdc         s                   like(§jdcres) dim(999) inz
003900120605     d $recjdc         s                   like(§jdcrec) dim(999) inz
004000120605     d $csrjdc         s                   like(§jdccsr) dim(999) inz
004100120605     d $ssrjdc         s                   like(§jdcssr) dim(999) inz
004200120605     d $fimpjdc        s                   like(§jdcfimp) dim(999) inz
004300120605     d $impjdc         s                   like(§jdcimp) dim(999) inz
004400120605     d $kscjdc         s                   like(§jdcksc) dim(999) inz
004500120605     d $ctrjdc         s                   like(§jdcctr) dim(999) inz
004600120605     d $lnpjdc         s                   like(§jdclnp) dim(999) inz
004700120608     d $dtijdc         s                   like(§jdcdti) dim(999) inz
004800120608     d $dtfjdc         s                   like(§jdcdtf) dim(999) inz
004900121019     d $merge          s                   like(§JDCmerge) dim(999) inz
005000050314      *
005100040119      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
005200050330
005300120604     d djdc          e ds                  inz
005400120604     d dAR5doc       e ds                  inz
005500121022     d dLAC          e ds                  inz
005600121023     d dTASflo       e ds                  inz
005700090212
005800130411       // - Parametri x TRUL55R
005900130411     d TRUL55ds      e ds                  inz
006000130411     d   I55tla      e                     inz('C')
006100090212
006200040331     d DS_Time1        ds            14    inz
006300040331     d   dsT1_hms                     6  0 inz
006400040331     d   dsT1_dmy                     8  0 inz
006500040331     d DS_Time2        ds            14    inz
006600040331     d   dsT2_ymd                     8  0 inz
006700040331     d   dsT2_hms                     6  0 inz
006800120605
006900040331     d Status         sds           333
007000040331     d  SDSjob               244    253                                         Job name
007100040331      *
007200040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007300040331      *
007400040331      * - Flag booleani
007500120604     d $CLI            s              1n   inz(*off)
007600040331      * - Indici di schiera
007700110228     d xx              s              4  0 inz
007800040331      * - Campi di comodo
007900040331     d WtasDCM         s              8    inz(*zeros)
008000050330     d WtasRMN         s             15    inz(*zeros)
008100040520     d WtasPOm         s              3  0 inz
008200040331     d Wtime           s             14  0 inz
008300061227     d Wdata_00        s              8  0 inz
008400061228     d Wdata_15        s              8  0 inz
008500040331     d WdateISO        s               d   datfmt(*ISO) inz
008600120607     d klaccop         s                   like(laccop)
008700120604     d tijdc_u_w       s              1
008800121022     d tilac_u_w       s              1
008900120604     d kAR5trd         s                   like(AR5trd) inz('DOC')
009000120605     d nrweek          s              2  0 inz
009100120605     d wyear           s              4  0 inz
009200120605     d waddams         s              6    inz
009300090807     d chiudi          s              1
009400120608     d databolla       s              8  0 inz
009500090212
009600090212       //--------------------------------------------------------------
009700090212       //?Definizione procedure usate.                                 ?
009800090212       //--------------------------------------------------------------
009900090212
010000120604       // - Determinazione nome dell'immagine per TIJDC
010100130411     d trul55r         pr                  extpgm('TRUL55R')
010200130411     d  trul55ds                           likeds(trul55ds)
010300090218
010400090218       //--------------------------------------------------------------
010500090218       //?Definizione key-list.                                        ?
010600090218       //--------------------------------------------------------------
010700040119
010800040119      *---------------------------------------------------------------*
010900040119
011000040119      * Predisposizione indicatori per riconoscere i tipi record
011100040119      *   reperiti dal file TITAS30C
011200040119     iTITAS000      31
011300040119     iTITAS010      32
011400040119     iTITASP00      33
011500120604
011600120604      * Predisposizione indicatori per riconoscere i tipi record
011700120604      *   reperiti dal file FIAR531C
011800120604     iFIAR5000      41
011900120604     iFIAR5P00      43
012000040119
012100040119      *---------------------------------------------------------------*
012200040119
012300040119      *
012400040119      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
012500040119      *
012600040119     c     K04TAS30      klist
012700040119     c                   kfld                    TASaas
012800040119     c                   kfld                    TASlnp
012900040119     c                   kfld                    TASnrs
013000040119     c                   kfld                    TASnsp
013100120604
013200120604     c     K05JDC        klist
013300050314     c                   kfld                    LAGaas
013400050314     c                   kfld                    LAGlnp
013500050314     c                   kfld                    LAGnrs
013600050314     c                   kfld                    LAGnsp
013700050314     c                   kfld                    klaccop
013800120604
013900120604     c     kAR5          klist
014000120604     c                   kfld                    TASaas
014100120604     c                   kfld                    TASlnp
014200120604     c                   kfld                    TASnrs
014300120604     c                   kfld                    TASnsp
014400120604     c                   kfld                    kAR5trd
014500040331      *
014600040331      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
014700040331      *
014800040331      * 31-33 - Tracciato reperito dal file TITAS30C (000/010/P00)
014900120605      * 41/43 - Tracciato reperito dal file FIAR531C (000/P00)
015000120604      * 99    - Errore in scrittura record nel file TIJDD00F
015100040119
015200040119      *---------------------------------------------------------------*
015300090807
015400121023     c     *entry        plist
015500121023     c                   parm                    chiudi
015600040119
015700040331      * Operazioni Iniziali
015800040331     c                   exsr      OperazIniz
015900040331      *
016000120604      * Ciclo di lettura del file TIJDG00R
016100120604     c                   read      TIJDG000
016200040119      *
016300120604do  1c                   dow       NOT %eof(TIJDG00R)
016400040119      *
016500120702      * - Elaborazione dei soli rec. "IM"
016600120702if  2c                   if        LAGapp = 'IM'
016700040119      *
016800040119      * - Aggancio record corrispondente nel file TITAS30C
016900040119     c                   movel     '2000'        TASaas
017000040119     c                   move      LAGaas        TASaas
017100040119     c                   move      LAGlnp        TASlnp
017200040119     c                   move      LAGnrs        TASnrs
017300040119     c                   move      LAGnsp        TASnsp
017400040128     c                   setoff                                       313233
017500040119     c     K04TAS30      chain     TITAS30C
017600040119      *
017700040119      * - Aggiornamento dati
017800040119if  3c                   if        %found(TITAS30C)
017900121023
018000121023     c                   movel     TASflo        dTASflo
018100120605
018200120604     c* verifico se cliente presente in tabella "JDC"
018300120702      * Uso sempre TASCCM
018400050314     c                   eval      *in21   = *off
018500050314     c                   eval      $cli    = *off
018600050316     c                   z-add     1             xx
018700120702     c     TASccm        lookup    $clijdc(xx)                            21
018800050314if  4c                   if        *in21 = *on
018900120607     c                   eval      kLACcop = $pagjdc(xx)
019000120608      * controllo se la data spedizione è nel range delle date di scan.DOC
019100120608     c                   eval      databolla = (tasaas * 10000) + tasmgs
019200120608     c                   IF        databolla >= $dtijdc(xx) and
019300120608     c                             databolla <= $dtfjdc(xx)
019400120604     c                   exsr      Wrt_TIJDC
019500120608e   4c                   ENDIF
019600050314e   4c                   endif
019700120604     c                   exsr      Wrt_FIAR5
019800061227      *
019900061227x   3c                   else
020000061227      *
020100120604      * Sposto da TIJDG00R in TIJDD00F se sped. non trovata su TITAS
020200061228      *   e se record ("time") di almeno 15 gg. fa!
020300061227     c                   movel     LAGtim        wData_00
020400061228if  4c                   if        wData_00 <= wData_15
020500120604     c                   exsr      Wrt_TIJDD
020600061227e   4c                   endif
020700061227      *
020800040119e   3c                   endif
020900040119      *
021000040119e   2c                   endif
021100040119      *
021200120604      * - Lettura del record successivo nel file TIJDG00R
021300120604     c                   read      TIJDG000
021400040119      *
021500040119e   1c                   enddo
021600090807
021700090807      * testa la chiusura del sottosistema
021800121023     c                   shtdn                                        98
021900121023     c   98              eval      chiudi = 'S'
022000040119      *
022100040119      * Fine
022200040119     c                   eval      *inLR = *on
022300090212      /free
022400130411           reset  TRUL55ds;
022500130411           trul55r ( TRUL55ds );
022600090212      /end-free
022700040331      *
022800040331      **-------------------------------------------------------------**
022900040331      ** OperazIniz ** Operazioni Iniziali                           **
023000040331      **-------------------------------------------------------------**
023100040331     c     OperazIniz    BEGSR
023200120605
023300040331      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
023400040331     c                   time                    Wtime
023500040331     c                   movel     Wtime         DS_Time1
023600040331     c                   eval      dsT2_hms   = dsT1_hms
023700040331     c     *eur          movel     dsT1_dmy      WdateISO
023800040331     c                   move      WdateISO      dsT2_ymd
023900061228     c                   subdur    15 : *days    WdateISO
024000061228     c                   move      WdateISO      wData_15
024100040331      *
024200120604      * Intabellamento tabella "JDC" dei clienti che ricevono i documenti
024300050321     c* in automatico
024400050314     c                   clear                   xx
024500120604     c                   clear                   $clijdc
024600120604     c                   clear                   $dirjdc
024700120604     c                   clear                   $tpijdc
024800120604     c                   clear                   $fmijdc
024900120604     c                   clear                   $ksujdc
025000120605     c                   clear                   $tadujdc
025100120607     c                   clear                   $pagjdc
025200120605
025300120605     c                   clear                   $tadjdc
025400120605     c                   clear                   $tptjdc
025500120605     c                   clear                   $resjdc
025600120605     c                   clear                   $recjdc
025700120605     c                   clear                   $csrjdc
025800120605     c                   clear                   $ssrjdc
025900120605     c                   clear                   $fimpjdc
026000120605     c                   clear                   $impjdc
026100120605     c                   clear                   $kscjdc
026200120605     c                   clear                   $ctrjdc
026300120605     c                   clear                   $lnpjdc
026400120608     c                   clear                   $dtijdc
026500120608     c                   clear                   $dtfjdc
026600121019     c                   clear                   $merge
026700120605
026800120604     c     'JDC'         setll     tntbe01l
026900050314     c                   do        *hival
027000120604     c     'JDC'         reade     tntbe01l
027100050314     c                   if        %eof(tntbe01l)
027200050314     c                   leave
027300050314     c                   endif
027400050316     c                   if        tbeatb <> *blanks
027500050316     c                   iter
027600050316     c                   endif
027700120604     c                   movel     tbeuni        djdc
027800120604     c                   if        §jdcaut = 'A'
027900050314     c                   add       1             xx
028000120604     c                   movel     tbeke1        $clijdc(xx)
028100120604     c                   movel     §jdcdir       $dirjdc(xx)
028200120604     c                   movel     §jdctpi       $tpijdc(xx)
028300120604     c                   movel     §jdcfmi       $fmijdc(xx)
028400120604     c                   eval      $ksujdc(xx)  = §jdcksu
028500120605     c                   eval      $tadujdc(xx) = §jdctadu
028600120607     c                   eval      $pagjdc(xx)  = §jdcpag
028700120608     c                   eval      $dtijdc(xx)  = §jdcdti
028800120608     c                   eval      $dtfjdc(xx)  = §jdcdtf
028900121019     c                   eval      $merge(xx)   = §JDCmerge
029000120605
029100120605     c                   eval      $fimpjdc(xx) = §jdcfimp
029200120605     c                   eval      $impjdc(xx)  = §jdcimp
029300120605     c                   eval      $kscjdc(xx)  = §jdcksc
029400120605     c                   eval      $ctrjdc(xx)  = §jdcctr
029500120605     c                   eval      $lnpjdc(xx)  = §jdclnp
029600120605     c                   movel     §jdctad       $tadjdc(xx)
029700120605     c                   movel     §jdctpt       $tptjdc(xx)
029800120605     c                   movel     §jdcres       $resjdc(xx)
029900120605     c                   movel     §jdcrec       $recjdc(xx)
030000120605     c                   movel     §jdccsr       $csrjdc(xx)
030100120605     c                   movel     §jdcssr       $ssrjdc(xx)
030200050321     c                   endif
030300050314     c                   enddo
030400040331      *
030500040331     c                   ENDSR
030600040119      *
030700050314      **-------------------------------------------------------------**
030800120604      ** Wrt_TIJDC  ** Registrazione dati nel file TIJDC00F x cliente**
030900050314      **-------------------------------------------------------------**
031000120604     c     Wrt_TIJDC     BEGSR
031100121023
031200121023       //?Se cliente con immagine Mergiata (LDV + DOC nello stesso PDF)
031300121023       //?controllo se presente il flag LDV
031400121023     c                   IF        $merge(xx) <> *blanks and
031500121023     c                             §FLOiml = *blanks
031600121023     c                   leavesr
031700121023     c                   ENDIF
031800090310
031900050314     c                   eval      $cli = *on
032000120605
032100120604      * Se su TIJDC esiste già un record non elaborato aggiorno else scrivo
032200120604     C                   eval      Tijdc_U_W = 'W'
032300050314      *
032400120604     C     k05jdc        setll     Tijdc01L
032500120604     C     k05jdc        reade     Tijdc01L
032600120604     C                   DOW       NOT %EOF(TIJDC01L)
032700050314     C                   IF        LACela = '00'
032800120604     C                   eval      Tijdc_U_W = 'U'
032900050314     c                   leave
033000050314     C                   ELSE
033100120604     C     k05jdc        reade     Tijdc01L
033200050314     c                   ENDIF
033300050314     c                   ENDDO
033400120605
033500050314      * Preparazione del record
033600120604     c                   clear                   TIJDC000
033700050314     c                   move      DS_Time2      LACtim
033800050314     c                   move      TASaas        LACaas
033900050314     c                   move      TASlnp        LAClnp
034000050314     c                   move      TASnrs        LACnrs
034100050314     c                   move      TASnsp        LACnsp
034200120607     c                   movel     $pagjdc(xx)   LACcop
034300120604     c                   movel     $dirjdc(xx)   LACdir
034400120604     c                   movel     $tpijdc(xx)   LACtpi
034500050330     c* flag nome immagine:
034600050330     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
034700050330     c*           sa Laguna) --> quindi lascio = blanks
034800120604if  1c                   if        $fmijdc(xx)  <> 'S '
034900090212      /free
035000130411           clear  TRUL55ds;
035100130411           I55fmi = $FMIjdc(xx);
035200130411           I55lnp = TASlnp;
035300130411           I55nrs = TASnrs;
035400130411           I55nsp = TASnsp;
035500130411           I55aas = TASaas;
035600130411           I55mgs = TASmgs;
035700130411           I55rmn = TASrmn;
035800130411           I55fld = 'D';
035900130411           trul55r ( TRUL55ds );
036000130411           if  O55err = *off;
036100130411             LACnim = O55nimd;
036200090212           endif;
036300090212      /end-free
036400121023e   1c                   endif
036500121023
036600120605     c                   eval      lacela = '00'
036700120605     c                   eval      lactela = 'A'
036800120605     c                   eval      lacksc = $clijdc(xx)
036900120605     c                   eval      lacksu = $ksujdc(xx)
037000120605     c                   eval      lactadu = $tadujdc(xx)
037100120605
037200120605      * calcolo ID job in base alla frequenza addebito
037300120605     c                   select
037400120605      * - se frequenza addebito per Immagine
037500120605     c                   when      lactadu = 'I'
037600120605      *   id = n.spedizione
037700120605     c                   eval      lacidjob = lacaas + laclnp +
037800120605     c                                        lacnrs + lacnsp
037900120605      * - se frequenza addebito per Job
038000120605     c                   when      lactadu = 'J'
038100120605      *   id = ksu + oggi
038200120605     c                   eval      lacidjob = %editc(lacksu:'X') +
038300120605     c                                        %editc(dst2_ymd:'X')
038400120605      * - se frequenza addebito per Settimana
038500120605     c                   when      lactadu = 'S'
038600120605      *   ricerco il n. della settimana
038700120605     c     *iso          movel     dsT2_ymd      WdateISO
038800120605     c                   clear                   nrweek
038900120605     c                   call      'XSRWEEK'
039000120605     c                   parm                    wdateiso
039100120605     c                   parm                    nrweek
039200120605      *   imposto il campo LACADDMS
039300120605     c                   eval      wyear = %subdt(wdateiso:*years)
039400120605     c                   eval      waddams = %editc(wyear:'X') +
039500120605     c                                       %editc(nrweek:'X')
039600120605     c                   eval      lacaddms = %dec(waddams:6:0)
039700120605      *   id = ksu + S + AAAA + n.settimana
039800120605     c                   eval      lacidjob = %editc(lacksu:'X') +
039900120605     c                                        'S' + waddams
040000120605      * - se frequenza addebito per Mese
040100120605     c                   when      lactadu = 'M'
040200120605      *   imposto il campo LACADDMS
040300120605     c                   eval      lacaddms =
040400120605     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
040500120605      *   id = ksu + M + AAAA + mese
040600120605     c                   eval      lacidjob = %editc(lacksu:'X') +
040700120605     c                                        'M' + %editc(lacaddms:'X')
040800120605     c                   endsl
040900120605
041000120605      * memorizzo anche i campi che per ora non uso
041100120605     c                   eval      lacfimp = $fimpjdc(xx)
041200120605     c                   eval      lacimp = $impjdc(xx)
041300120605      * flag per creare titas, non creare o con varia negata
041400120605     c                   eval      lactad = $tadjdc(xx)
041500120605
041600120605      * cliente e tariffa tassazione
041700120605      * --> se forzati prendo da tabella
041800120605     c                   if        $kscjdc(xx) <> *zeros
041900120605     c                   eval      lacidl = %editc($kscjdc(xx):'X') +
042000120605     c                                      $ctrjdc(xx)
042100120605     c                   else
042200120605      * --> altrimenti dalla bolla
042300120702     c                   eval      lacidl = %editc(TASccm:'X') +
042400120605     c                                      %editc(tasctr:'X')
042500120605     c                   endif
042600120605
042700050314      *
042800050314      * Scrittura /aggiornamento
042900120604     C                   IF        Tijdc_U_W = 'U'
043000120604     c                   update    TIJDC000
043100050314     c                   ELSE
043200120604     c                   write     TIJDC000
043300050314     c                   ENDIF
043400121023
043500121023       //?Se cliente con immagine Mergiata ed ho scritto il TIJDC
043600121023       //?devo scrivere anche TILAC
043700121023     c                   IF        $merge(xx) <> *blanks
043800121023     c                   exsr      wrt_TILAC
043900121023     c                   ENDIF
044000050314      *
044100120604     c                   ENDSR
044200061227      *
044300061227      **-------------------------------------------------------------**
044400120604      ** Wrt_TIJDD  ** Registrazione dati nel file TIJDD00F          **
044500061227      **-------------------------------------------------------------**
044600120604     c     Wrt_TIJDD     BEGSR
044700061227      *
044800120604     c                   clear                   TIJDD000
044900061227     c                   eval      LADtim = LAGtim
045000061227     c                   eval      LADaas = LAGaas
045100061227     c                   eval      LADlnp = LAGlnp
045200061227     c                   eval      LADnrs = LAGnrs
045300061227     c                   eval      LADnsp = LAGnsp
045400061227     c                   eval      LADxx1 = LAGxx1
045500061227     c                   eval      LADcop = 'MOV'
045600120921     c                   eval      LADdir = 'ERRATE_C'
045700061227     c                   eval      LADela = *zeros
045800061227      *
045900120604     c                   WRITE     TIJDD000                             99
046000061227      *
046100120604      * Cancellazione record dal file TIJDG00R
046200120604     c                   DELETE    TIJDG000
046300061227      *
046400061227     c                   ENDSR
046500050314      *
046600120604      /free
046700120604       //-------------------------------------------------------------**
046800120604       // Wrt_FIAR5  ** Scrivo dati nel file FIAR500F rcd DOC  **
046900120604       //-------------------------------------------------------------**
047000120604        BEGSR Wrt_FIAR5;
047100120604
047200120604         chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
047300120604       //?Se esiste FIAR5 aggiorno
047400120604         IF  %found(FIAR531C);
047500120604           dAR5doc = AR5uni;
047600120604           IF  not $cli;
047700120606             §AR5jfid = 'I';
047800120604           ELSE;
047900120606             §AR5jfid = 'S';
048000120604           ENDIF;
048100120611           §AR5jdrid = %editc(%dec(%date()):'X');
048200120606           IF  $cli;
048300120606             §AR5jdiid = §AR5jdrid;
048400120606           ENDIF;
048500120604           AR5uni = dAR5doc;
048600120604           IF  *in41;
048700120604             update FIAR5000;
048800120604           ENDIF;
048900120604           IF  *in43;
049000120604             update FIAR5P00;
049100120604           ENDIF;
049200120604         ENDIF;
049300120604
049400120604       //?Se NON esiste FIAR5 lo scrivo
049500120604         IF  not %found(FIAR531C);
049600120604           IF  *in31 or *in32;
049700120604             clear FIAR5000;
049800120604           ENDIF;
049900120604           IF  *in33;
050000120604             clear FIAR5P00;
050100120604           ENDIF;
050200120604           AR5aas = TASaas;
050300120604           AR5lnp = TASlnp;
050400120604           AR5nrs = TASnrs;
050500120604           AR5nsp = TASnsp;
050600120604           AR5trd = kAR5trd;
050700120604           AR5dac = %dec(%date());
050800120604           AR5orc = %dec(%time());
050900120604           IF  not $cli;
051000120606             §AR5jfid = 'I';
051100120604           ELSE;
051200120606             §AR5jfid = 'S';
051300120604           ENDIF;
051400120604           §AR5jdrid = %editc(AR5dac:'X');
051500120606           IF  $cli;
051600120606             §AR5jdiid = §AR5jdrid;
051700120606           ENDIF;
051800120604           AR5uni = dAR5doc;
051900120604           IF  *in31 or *in32;
052000120604             write FIAR5000;
052100120604           ENDIF;
052200120604           IF  *in33;
052300120604             write FIAR5P00;
052400120604           ENDIF;
052500120604         ENDIF;
052600120604
052700120604       //?Cancello rcd su TIJDG00R
052800120604         delete  TIJDG000;
052900120604
053000120604         ENDSR;
053100121019       //-------------------------------------------------------------**
053200121019       // wrt_TILAC  ** Scrivo riga su TILAC  **
053300121019       //-------------------------------------------------------------**
053400121022         BEGSR wrt_TILAC;
053500121022
053600121022           Tilac_U_W = 'W';
053700121022
053800121022         //?Aggancio tabella LAC
053900121022           clear dLAC;
054000121022           TBEcod = 'LAC';
054100121022           TBEke1 = %editc($clijdc(xx):'X');
054200121022           chain (TBEcod:TBEke1) TNTBE01L;
054300121022           IF  %found(TNTBE01L) and TBEatb = *blanks;
054400121022             dLAC = TBEuni;
054500121022           ELSE;
054600121022             leavesr;
054700121022           ENDIF;
054800121022
054900121212           chain (LACaas:LAClnp:LACnrs:LACnsp:'CLI':§LACdir) TILAC04L;
055000121212           IF  %Found(TILAC04L);
055100121212             Tilac_U_W = 'U';
055200121212           ENDIF;
055300121022
055400121022           clear TILAC000;
055500121022           LLACtim  = LACtim;
055600121022           LLACaas  = LACaas;
055700121022           LLAClnp  = LAClnp;
055800121022           LLACnrs  = LACnrs;
055900121022           LLACnsp  = LACnsp;
056000121022           LLACcop  = 'CLI';
056100121022           LLACdir  = §LACdir;
056200121022           LLACtpi  = §LACtpi;
056300121022           LLACnim  = LACnim;
056400121022           IF  §LACksc <> *zeros;
056500121022             LLACidl  = %editc(§LACksc:'X') + §LACctr;
056600121022           ELSE;
056700121022             LLACidl  = %editc(TASksc:'X') + %editc(TASctr:'X');
056800121022           ENDIF;
056900121022           LLACela  = '00';
057000121022           LLACtela = 'A';
057100121023           LLACksc  = LACksc;
057200121022           LLACksu  = §LACksu;
057300121022           LLACtad  = §LACtad;
057400121022           LLACtadu = §LACtadu;
057500121022           LLACfimp = §LACfimp;
057600121022           LLACimp  = §LACimp;
057700121022
057800121022         //?AA/MM addebito + IDjob in base alla frequenza addebito
057900121022           SELECT;
058000121022             WHEN  LLACtadu = 'I';
058100121022               LLACidjob = LLACaas + LLAClnp + LLACnrs + LLACnsp;
058200121022             WHEN  LLACtadu = 'J';
058300121022               LLACidjob = %editc(LLACksu:'X') + %editc(dst2_ymd:'X');
058400121022             WHEN  LLACtadu = 'S';
058500121022      /end-free
058600121022     c     *iso          movel     dsT2_ymd      WdateISO
058700121022     c                   clear                   nrweek
058800121022     c                   call      'XSRWEEK'
058900121022     c                   parm                    wdateiso
059000121022     c                   parm                    nrweek
059100121022      /free
059200121022               wyear = %subdt(wdateiso:*years);
059300121022               waddams = %editc(wyear:'X') + %editc(nrweek:'X');
059400121022               LLACaddms = %dec(waddams:6:0);
059500121023               LLACidjob = %editc(LLACksu:'X') + 'S' + waddams;
059600121022             WHEN  LLACtadu = 'M';
059700121022               LLACaddms = %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0);
059800121023               LLACidjob = %editc(LLACksu:'X') + 'M' + %editc(LLACaddms:'X');
059900121022           ENDSL;
060000121022
060100121022           IF  Tilac_U_W = 'U';
060200121022             UPDATE TILAC000;
060300121022           ELSE;
060400121022             WRITE  TILAC000;
060500121022           ENDIF;
060600121019
060700121019         ENDSR;
060800121019
060900120604      /end-free
