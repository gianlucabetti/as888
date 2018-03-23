000100180313      *===============================================================*
000101180313      * TNSB10R    * Aggiornamento TITAS per immagine LdV             *
000102180313      *===============================================================*
000103180313
000104180313     h decedit('0,') datedit(*dmy/)
000105180313     h option(*nodebugio)
000106180313
000107180313      *---------------------------------------------------------------*
000108180313
000109180313
000110180313
000111180313
000112180313     //definiire tutte le tabelle/file che saranno utilizzate nel programma
000113180313       // i  inputi
000114180313       // o output
000115180313       // U upadate
000116180313
000117180313     fAZORG01L  if   e           k disk
000118180313     fTNTBE01L  if   e           k disk
000119180313     fTABEL00F  if   e           k disk
000120180313      *
000121180313     fFNLBL01L  if   e           k disk
000122180313     fTITA430C  if   e           k disk
000123180313      *
000124180313     fFIILV01L  Uf   e           k disk
000125180313      *
000126180313     fTILAG00R  Uf   e           k disk
000127180313      *
000128180313     fTITAS30C  Uf   e           k disk
000129180313     fFIAR531C  if   e           k disk
000130180313
000131180313     fFIDPO01L  if   e           k disk
000132180313      *
000133180313     fTILAO01L  Uf a e           k disk
000134180313     fTILAC01L  Uf a e           k disk    rename(tilac000:tilac01)
000135180313     fTILAC04L  uf a e           k disk
000136180313     fTIJDC01L  uf a e           k disk    prefix(JDC:3)
000137180313     fFIPND01L  if   e           k disk
000138180313     fTILAD00F  O    e             disk
000139180313
000140180313      *---------------------------------------------------------------*
000141180313
000142180313      *
000143180313      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
000144180313      *
000145180313       // variabili
000146180313     d $poDPD          s                   like(ORGfil) dim(999) inz
000147180313     d $clilac         s              7  0 dim(999) inz
000148180313     d $tptlac         s                   like(§lactpt) dim(999) inz
000149180313     d $reslac         s                   like(§lacres) dim(999) inz
000150180313     d $reclac         s                   like(§lacrec) dim(999) inz
000151180313     d $csrlac         s                   like(§laccsr) dim(999) inz
000152180313     d $ssrlac         s                   like(§lacssr) dim(999) inz
000153180313     d $dirlac         s                   like(§lacdir) dim(999) inz
000154180313     d $tpilac         s                   like(§lactpi) dim(999) inz
000155180313     d $fmilac         s                   like(§lacfmi) dim(999) inz
000156180313     d $TADlac         s                   like(§LACtad) dim(999) inz
000157180313     d $tadulac        s                   like(§lactadu) dim(999) inz
000158180313     d $fimplac        s                   like(§lacfimp) dim(999) inz
000159180313     d $implac         s 00                  like(§lacimp) dim(999) inz
000160180313     d $ksclac         s                   like(§lacksc) dim(999) inz
000161180313     d $ctrlac         s                   like(§lacctr) dim(999) inz
000162180313     d $ksulac         s                   like(§lacksu) dim(999) inz
000163180313     d $lnplac         s                   like(§laclnp) dim(999) inz
000164180313     d $merge          s                   like(§LACmerge) dim(999) inz
000165180313     d $flgLA2         s                   like(§LACla2) dim(999) inz
000166180313      *
000167180313      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
000168180313      *
000169180313      * - Ds per controlli bolla
000170180313     d titasds       e ds                  extname(TITAS00F)
000171180313     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
000172180313
000173180313     d OG143         e ds                  inz
000174180313     d dTASflo       e ds                  inz
000175180313     d dlac          e ds                  inz
000176180313     d dLA2          e ds                  inz
000177180313     d dstb          e ds                  inz
000178180313     d dsTBdpd       e ds                  extname(dstb)
000179180313     d                                     inz prefix(dpd)
000180180313     d dsbl4m        e ds
000181180313     d dJDC          e ds                  inz
000182180313     d dAR5doc       e ds                  inz
000183180313
000184180313       // - Parametri x TRUL55R
000185180313     d TRUL55ds      e ds                  inz
000186180313     d   I55tla      e                     inz('C')
000187180313
000188180313      *
000189180313     d DS_Time1        ds            14    inz
000190180313     d   dsT1_hms                     6  0 inz
000191180313     d   dsT1_dmy                     8  0 inz
000192180313     d DS_Time2        ds            14    inz
000193180313     d   dsT2_ymd                     8  0 inz
000194180313     d   dsT2_hms                     6  0 inz
000195180313      *
000196180313     d Status         sds           333
000197180313     d  SDSjob               244    253                                         Job name
000198180313      *
000199180313      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
000200180313      *
000201180313      * - Flag booleani
000202180313     d $DPD            s              1    inz(*off)
000203180313     d $CLI            s              1    inz(*off)
000204180313     d $cliLA2         s               n   inz(*off)
000205180313     d $cliJDC         s              1n   inz(*off)
000206180313      * - Indici di schiera
000207180313     d xx              s              4  0 inz
000208180313      * - Campi di comodo
000209180313     d WtasDCM         s              8    inz(*zeros)
000210180313     d WtasRMN         s             15    inz(*zeros)
000211180313     d WtasPOm         s              3  0 inz
000212180313     d Wtime           s             14  0 inz
000213180313     d Wdata_00        s              8  0 inz
000214180313     d Wdata_15        s              8  0 inz
000215180313     d WdateISO        s               d   datfmt(*ISO) inz
000216180313     d kAR5trd         s                   like(AR5trd) inz('DOC')
000217180313     d klaocop         s                   like(laocop) inz('DPD')
000218180313     d klaccop         s                   like(laccop) inz('CLI')
000219180313     d kLACdir         s                   like(LACdir)
000220180313     d kJDCcop         s                   like(JDCcop)
000221180313     d tilao_u_w       s              1
000222180313     d tilac_u_w       s              1
000223180313     d tijdc_u_w       s              1
000224180313     d kcod            s                   like(tblcod) inz('TB')
000225180313     d kkey            s                   like(tblkey)
000226180313     d savtbl          s                   like(tastbl) inz('  ')
000227180313     d WTAS_orig       s              1
000228180313     d ktrc            s                   like(ta4trc)
000229180313     d nrweek          s              2  0 inz
000230180313     d wyear           s              4  0 inz
000231180313     d waddams         s              6    inz
000232180313     d chiudi          s              1
000233180313     d noscrivi        s              1n   inz(*off)
000234180313
000235180313       //--------------------------------------------------------------
000236180313       //?Definizione procedure usate.                                 ?
000237180313       //--------------------------------------------------------------
000238180313
000239180313       // - Determinazione nome dell'immagine per TILAC
000240180313     d trul55r         pr                  extpgm('TRUL55R')
000241180313     d   trul55ds...
000242180313     d                                     likeds(trul55ds)
000243180313
000244180313       //--------------------------------------------------------------
000245180313       //?Definizione key-list.                                        ?
000246180313       //--------------------------------------------------------------
000247180313
000248180313       // - File FIILV01L
000249180313     d k06fiilv01    e ds                  extname( FIILV01L : *key )
000250180313     d                                     prefix(k_)   inz
000251180313
000252180313      *---------------------------------------------------------------*
000253180313
000254180313      * Predisposizione indicatori per riconoscere i tipi record
000255180313      *   reperiti dal file TITAS30C
000256180313     iTITAS000      31
000257180313     iTITAS010      32
000258180313     iTITASP00      33
000259180313
000260180313      *---------------------------------------------------------------*
000261180313
000262180313      *
000263180313      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
000264180313      *
000265180313     c     K04TAS30      klist
000266180313     c                   kfld                    TASaas
000267180313     c                   kfld                    TASlnp
000268180313     c                   kfld                    TASnrs
000269180313     c                   kfld                    TASnsp
000270180313     c     Ktas30_f      klist
000271180313     c                   kfld                    lblaan
000272180313     c                   kfld                    lbllpn
000273180313     c                   kfld                    lblnrn
000274180313     c                   kfld                    lblnsn
000275180313
000276180313     c     kAR5          klist
000277180313     c                   kfld                    TASaas
000278180313     c                   kfld                    TASlnp
000279180313     c                   kfld                    TASnrs
000280180313     c                   kfld                    TASnsp
000281180313     c                   kfld                    kAR5trd
000282180313      *
000283180313     c     K05LAO        klist
000284180313     c                   kfld                    LAGaas
000285180313     c                   kfld                    LAGlnp
000286180313     c                   kfld                    LAGnrs
000287180313     c                   kfld                    LAGnsp
000288180313     c                   kfld                    klaocop
000289180313     c*
000290180313     c     K05LAC        klist
000291180313     c                   kfld                    LAGaas
000292180313     c                   kfld                    LAGlnp
000293180313     c                   kfld                    LAGnrs
000294180313     c                   kfld                    LAGnsp
000295180313     c                   kfld                    klaccop
000296180313     c     K05JDC        klist
000297180313     c                   kfld                    LAGaas
000298180313     c                   kfld                    LAGlnp
000299180313     c                   kfld                    LAGnrs
000300180313     c                   kfld                    LAGnsp
000301180313     c                   kfld                    kJDCcop
000302180313
000303180313     c     Ktas          klist
000304180313     c                   kfld                    LBLaao
000305180313     c                   kfld                    LBLlpo
000306180313     c                   kfld                    LBLnro
000307180313     c                   kfld                    LBLnso
000308180313      *
000309180313     c     Kta4          klist
000310180313     c                   kfld                    TASaas
000311180313     c                   kfld                    TASlnp
000312180313     c                   kfld                    TASnrs
000313180313     c                   kfld                    TASnsp
000314180313     c                   kfld                    ktrc
000315180313     c*
000316180313     c     Ktab          klist
000317180313     C                   KFLD                    CODUT
000318180313     C                   KFLD                    kcod
000319180313     c                   kfld                    kkey
000320180313     c                   z-add     1             codut             1 0
000321180313      *
000322180313      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
000323180313      *
000324180313      * 20    - No cliente in tabella con LAC FTP
000325180313      * 21    - P.O.partenza del network DPD
000326180313      * 31-33 - Tracciato reperito dal file TITAS30C (000/010/P00)
000327180313      * 99    - Errore in scrittura record nel file TILAD00F
000328180313
000329180313      *---------------------------------------------------------------*
000330180313
000331180313     c     *entry        plist
000332180313     c                   parm                    chiudi
000333180313
000334180313      * Operazioni Iniziali
000335180313     c                   exsr      OperazIniz
000336180313      *
000337180313      * Ciclo di lettura del file TILAG00R
000338180313      *
000339180313// legge tutte le scansioni
000340180313     c                   read      TILAG000
000341180313      *
000342180313do  1c                   dow       NOT %eof(TILAG00R)
000343180313      *
000344180313      * - Elaborazione dei soli rec. "IM"
000345180313if  2c                   if        LAGapp = 'IM'
000346180313      *
000347180313      * - Aggancio record corrispondente nel file TITAS30C
000348180313     c                   movel     '2000'        TASaas
000349180313     c                   move      LAGaas        TASaas
000350180313     c                   move      LAGlnp        TASlnp
000351180313     c                   move      LAGnrs        TASnrs
000352180313     c                   move      LAGnsp        TASnsp
000353180313     c                   setoff                                       313233
000354180313     c     K04TAS30      chain     TITAS30C
000355180313      *
000356180313      * - Aggiornamento dati
000357180313if  3c                   if        %found(TITAS30C)
000358180313     c                   eval      noscrivi = *off
000359180313      *
000360180313     c* verifico se cliente presente in tabella "LAC"
000361180313     c                   eval      *in20   = *off
000362180313     c                   eval      *in21   = *off
000363180313     c                   eval      $cli    = *off
000364180313     c                   eval      $dpd    = *off
000365180313     c                   z-add     1             xx
000366180313     c     tasksc        lookup    $clilac(xx)                            20
000367180313if  4c                   if        *in20 = *on
000368180313       // cliente con richiesta di immagine con merge
000369180313       // se presente l'immagine del DOC scrivo tutti e 2 gli archivi
000370180313     c                   IF        $merge(xx) <> *blanks
000371180313     c                   exsr      ctr_AR5
000372180313     c                   ENDIF
000373180313     c                   IF        not noscrivi
000374180313     c                   exsr      Wrt_TILAC
000375180313     c                   IF        $merge(xx) <> *blanks
000376180313     c                   exsr      wrt_TIJDC
000377180313     c                   ENDIF
000378180313     c                   ENDIF
000379180313       // controllo se devo estrarre due volte la stessa immagine
000380180313     c                   eval      $clila2 = *off
000381180313     c                   IF        $flgLA2(xx) <> *blanks
000382180313     c                   exsr      ctr_LA2
000383180313     c                   IF        $cliLA2
000384180313     c                   exsr      Wrt_TILAC2
000385180313     c                   ENDIF
000386180313     c                   ENDIF
000387180313x   4c                   else
000388180313     c                   exsr      Wrt_TILAO
000389180313e   4c                   endif
000390180313      /free
000391180313       // -?Cancellazione rec. da FIILV00F (non errori)?
000392180313       k_ILVaas = 2000 + %int(LAGaas);
000393180313       k_ILVlnp = %int(LAGlnp);
000394180313       k_ILVnrs = %int(LAGnrs);
000395180313       k_ILVnsp = %int(LAGnsp);
000396180313       clear  k_ILVfgs;
000397180313       clear  k_ILVndc;
000398180313       setll  %kds(k06fiilv01 : 4)  FIILV000;
000399180313       if  %equal(FIILV01L);
000400180313         reade  %kds(k06fiilv01 : 4)  FIILV000;
000401180313         dow  NOT %eof(FIILV01L);
000402180313           if  (ILVcol > '00'  and  ILVcol < '10')  OR
000403180313               (ILVcol = '00'  and (ILVnocdst = 'S'
000404180313                                or  ILVfirpda = 'S'
000405180313                                or  ILVscan   = ' '));
000406180313             DELETE  FIILV000;
000407180313           endif;
000408180313           reade  %kds(k06fiilv01 : 4)  FIILV000;
000409180313         enddo;
000410180313       endif;
000411180313      /end-free
000412180313     c                   exsr      Upd_TITAS
000413180313      *
000414180313x   3c                   else
000415180313      *
000416180313      * Sposto da TILAG00R in TILAD00F se sped. non trovata su TITAS
000417180313      *   e se record ("time") di almeno 15 gg. fa!
000418180313     c                   movel     LAGtim        wData_00
000419180313if  4c                   if        wData_00 <= wData_15
000420180313     c                   exsr      Wrt_TILAD
000421180313e   4c                   endif
000422180313      *
000423180313e   3c                   endif
000424180313      *
000425180313e   2c                   endif
000426180313      *
000427180313      * - Lettura del record successivo nel file TILAG00R
000428180313     c                   read      TILAG000
000429180313      *
000430180313e   1c                   enddo
000431180313
000432180313      * testa la chiusura del sottosistema
000433180313     c                   shtdn                                        98
000434180313     c   98              eval      chiudi = 'S'
000435180313      *
000436180313      * Fine
000437180313     c                   eval      *inLR = *on
000438180313      /free
000439180313       reset  TRUL55ds;
000440180313       trul55r ( TRUL55ds );
000441180313      /end-free
000442180313      *
000443180313      **-------------------------------------------------------------**
000444180313      ** OperazIniz ** Operazioni Iniziali                           **
000445180313      **-------------------------------------------------------------**
000446180313     c     OperazIniz    BEGSR
000447180313      *
000448180313      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
000449180313     c                   time                    Wtime
000450180313     c                   movel     Wtime         DS_Time1
000451180313     c                   eval      dsT2_hms   = dsT1_hms
000452180313     c     *eur          movel     dsT1_dmy      WdateISO
000453180313     c                   move      WdateISO      dsT2_ymd
000454180313     c                   subdur    15 : *days    WdateISO
000455180313     c                   move      WdateISO      wData_15
000456180313      *
000457180313      * Intabellamento dei p.o. appartenenti al network "DPD"
000458180313     c                   clear                   xx
000459180313     c                   clear                   $poDPD
000460180313     c     *loval        setll     AZORG
000461180313     c                   read      AZORG
000462180313do  1c                   dow       NOT %eof(AZORG01L)
000463180313     c                   movel     ORGde3        OG143
000464180313if  2c                   if        §OGntw     = 'DPD'
000465180313     c                   add       1             xx
000466180313     c                   eval      $poDPD(xx) = ORGfil
000467180313e   2c                   endif
000468180313     c                   read      AZORG
000469180313e   1c                   enddo
000470180313      * Intabellamento tabella "LAC" dei clienti che ricevono le immagini
000471180313     c* in automatico
000472180313     c                   clear                   xx
000473180313     c                   clear                   $clilac
000474180313     c                   clear                   $tptlac
000475180313     c                   clear                   $reslac
000476180313     c                   clear                   $reclac
000477180313     c                   clear                   $csrlac
000478180313     c                   clear                   $ssrlac
000479180313     c                   clear                   $dirlac
000480180313     c                   clear                   $tpilac
000481180313     c                   clear                   $fmilac
000482180313     c                   clear                   $TADlac
000483180313     c                   clear                   $tadulac
000484180313     c                   clear                   $fimplac
000485180313     c                   clear                   $implac
000486180313     c                   clear                   $ksclac
000487180313     c                   clear                   $ctrlac
000488180313     c                   clear                   $ksulac
000489180313     c                   clear                   $lnplac
000490180313     c                   clear                   $merge
000491180313     c     'LAC'         setll     tntbe01l
000492180313     c                   do        *hival
000493180313     c     'LAC'         reade     tntbe01l
000494180313     c                   if        %eof(tntbe01l)
000495180313     c                   leave
000496180313     c                   endif
000497180313     c                   if        tbeatb <> *blanks
000498180313     c                   iter
000499180313     c                   endif
000500180313     c                   movel     tbeuni        dlac
000501180313     c                   if        §lacaut = 'A'
000502180313     c                   add       1             xx
000503180313     c                   movel     tbeke1        $clilac(xx)
000504180313     c                   movel     §lactpt       $tptlac(xx)
000505180313     c                   movel     §lacres       $reslac(xx)
000506180313     c                   movel     §lacrec       $reclac(xx)
000507180313     c                   movel     §laccsr       $csrlac(xx)
000508180313     c                   movel     §lacssr       $ssrlac(xx)
000509180313     c                   movel     §lacdir       $dirlac(xx)
000510180313     c                   movel     §lactpi       $tpilac(xx)
000511180313     c                   movel     §lacfmi       $fmilac(xx)
000512180313     c                   movel     §LACtad       $TADlac(xx)
000513180313     c                   eval      $tadulac(xx) = §lactadu
000514180313     c                   eval      $fimplac(xx) = §lacfimp
000515180313     c                   eval      $implac(xx)  = §lacimp
000516180313     c                   eval      $ksclac(xx)  = §lacksc
000517180313     c                   eval      $ctrlac(xx)  = §lacctr
000518180313     c                   eval      $ksulac(xx)  = §lacksu
000519180313     c                   eval      $lnplac(xx)  = §laclnp
000520180313     c                   eval      $merge(xx)   = §lacmerge
000521180313     c                   eval      $flgla2(xx)  = §lacla2
000522180313     c                   endif
000523180313     c                   enddo
000524180313      *
000525180313     c                   ENDSR
000526180313      /free
000527180313       //-------------------------------------------------------------**
000528180313       // crt_AR5  -- Controllo se arrivato il DOC.
000529180313       //-------------------------------------------------------------**
000530180313       BEGSR ctr_AR5;
000531180313
000532180313         noscrivi = *off;
000533180313
000534180313         //?Se su FIAR5 trovo che il DOC è già presente scrivo LAC e JDC
000535180313         chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
000536180313         IF  not %found(FIAR531C);
000537180313           noscrivi = *on;
000538180313           leavesr;
000539180313         ENDIF;
000540180313         dAR5doc = AR5uni;
000541180313         IF  §AR5jfid = *blanks;
000542180313           noscrivi = *on;
000543180313           leavesr;
000544180313         ENDIF;
000545180313
000546180313         //?Cerco i dati del cliente sulla tabella JDC
000547180313         clear dJDC;
000548180313         $cliJDc = *off;
000549180313         TBEcod = 'JDC';
000550180313         TBEke1 = %editc($clilac(xx):'X');
000551180313         chain (TBEcod:TBEke1) TNTBE01L;
000552180313         IF  %found(TNTBE01L) and TBEatb = *blanks;
000553180313           $cliJDc = *on;
000554180313           dJDC = TBEuni;
000555180313           kJDCcop = §JDCpag;
000556180313         ENDIF;
000557180313
000558180313       ENDSR;
000559180313
000560180313       //-------------------------------------------------------------**
000561180313       // crt_LA2  -- Controllo se estrazione dell'immagine da sola
000562180313       //-------------------------------------------------------------**
000563180313       BEGSR ctr_LA2;
000564180313
000565180313         //?Controllo se esiste il cliente sulla tabella LA2
000566180313         clear dLA2;
000567180313         $cliLA2 = *off;
000568180313         TBEcod = 'LA2';
000569180313         TBEke1 = %editc($clilac(xx):'X');
000570180313         chain (TBEcod:TBEke1) TNTBE01L;
000571180313         IF  %found(TNTBE01L) and TBEatb = *blanks;
000572180313           $cliLA2 = *on;
000573180313           dLA2 = TBEuni;
000574180313         ENDIF;
000575180313
000576180313       ENDSR;
000577180313      /end-free
000578180313      *
000579180313      **-------------------------------------------------------------**
000580180313      ** Wrt_TILAO  ** Registrazione dati nel file TILAO00F x DPD    **
000581180313      **-------------------------------------------------------------**
000582180313     c     Wrt_TILAO     BEGSR
000583180313      *
000584180313      * Verifica se p.o. del mittente DPD
000585180313     c                   eval      *in21   = *off
000586180313     c                   movel     TASccm        WtasPOm
000587180313     c     WtasPOm       lookup    $poDPD                                 21
000588180313      * se filiale mittente no DPD provo con la filiale del KSC
000589180313     c                   IF        not *in21
000590180313     c                   movel     TASksc        WtasPOm
000591180313     c     WtasPOm       lookup    $poDPD                                 21
000592180313     c                   ENDIF
000593180313     c                   eval      $DPD    = *in21
000594180313      *
000595180313     c     $DPD          cabeq     *off          NOwrtTILAO
000596180313      *
000597180313      * Impostazione dei campi numerici in campi alfanumerici di comodo
000598180313     c                   movel     TASdcm        WtasDCM
000599180313     c                   move      TASrmn        Wtasrmn
000600180313      *
000601180313      * Se su TILAO esiste già un record non elaborato aggiorno else scrivo
000602180313     C                   eval      Tilao_U_W = 'W'
000603180313      *
000604180313     C     k05lao        setll     Tilao01L
000605180313     C     k05lao        reade     Tilao01L
000606180313     C                   DOW       NOT %EOF(TILAO01L)
000607180313     C                   IF        LAGela = '00'
000608180313     C                   eval      Tilao_U_W = 'U'
000609180313     c                   leave
000610180313     C                   ELSE
000611180313     C     k05lao        reade     Tilao01L
000612180313     c                   ENDIF
000613180313     c                   ENDDO
000614180313      *
000615180313      * Preparazione del record
000616180313     c                   clear                   TILAO000
000617180313     c                   move      DS_Time2      LAOtim
000618180313     c                   move      TASaas        LAOaas
000619180313     c                   move      TASlnp        LAOlnp
000620180313     c                   move      TASnrs        LAOnrs
000621180313     c                   move      TASnsp        LAOnsp
000622180313     c                   movel     'DPD'         LAOcop
000623180313     c                   clear                   w014a            14
000624180313       chain (TASaas:TASlnp:TASnrs:TASnsp) FIPND01L;
000625180313       IF  not %found(FIPND01L);
000626180313         clear PNDipn;
000627180313       ENDIF;
000628180313      /free
000629180313       //?se non ho trovato il numero parcel DPD ?
000630180313       //?provo a cercarlo con il n. ORM su FIDPO?
000631180313       IF  PNDipn = *blanks;
000632180313         exsr CercaParcel;
000633180313         //?se anche così non trovo il numero parcel
000634180313         //?provo a verificare se la bolla è legata
000635180313         //?quindi cerco il n. parcel con la bolla mamma
000636180313         IF  PNDipn = *blanks;
000637180313           exsr CercaParcelMamma;
000638180313         ENDIF;
000639180313       ENDIF;
000640180313      /end-free
000641180313     c* se non trovato numero parcel lo prendo da rmn
000642180313     c                   if        PNDipn = *blanks
000643180313     c* numero parcel lungo 11 lo trucco con 0 davanti al depot e 99 davanti
000644180313     c* al progressivo
000645180313     c                   if        %subst(wtasrmn:1:3) = '000'
000646180313     c                   eval      w014a = '0'
000647180313     c                                   + %subst(wtasrmn:4:3)
000648180313     c                                   + '99'
000649180313     c                                   + %subst(wtasrmn:7:8)
000650180313     c                   else
000651180313     c                   move      wtasrmn       w014a
000652180313     c                   endif
000653180313     c                   else
000654180313     c                   if        %subst(PNDipn:12:1) = ' '
000655180313     c                   eval      w014a = '0'
000656180313     c                                   + %subst(PNDipn:1:3)
000657180313     c                                   + '99'
000658180313     c                                   + %subst(PNDipn:4:8)
000659180313     c                   else
000660180313     c                   eval      w014a = PNDipn
000661180313     c                   endif
000662180313     c                   endif
000663180313     c                   eval      laoope  = w014a
000664180313     c                                     + '_'
000665180313     c                                     + wtasdcm
000666180313     c                   movel     SDSjob        LAGidl
000667180313     c                   move      *zeros        LAGela
000668180313      *
000669180313      * Scrittura /aggiornamento
000670180313     C                   IF        Tilao_U_W = 'U'
000671180313     c                   update    TILAO000
000672180313     c                   ELSE
000673180313     c                   write     TILAO000
000674180313     c                   ENDIF
000675180313      *
000676180313     c     NOwrtTILAO    ENDSR
000677180313
000678180313      /free
000679180313       //-------------------------------------------------------------**
000680180313       // Cerco il n. parcel DPD dall'ORM.
000681180313       //-------------------------------------------------------------**
000682180313       BEGSR CercaParcel;
000683180313
000684180313         //?lo faccio solo se bolla in assegnato
000685180313         //?NO da giugno 2017 ND ha detto di farlo anche per le bolle Franco
000686180313         clear dsTBdpd;
000687180313         kkey = TAStbl;
000688180313         chain (codut:kcod:kkey) TABEL00F;
000689180313         IF  %found(TABEL00F);
000690180313           dsTBdpd = TBLuni;
000691180313         ENDIF;
000692180313         IF  dpd§TBtpo <> 'A';
000693180313         //  leavesr;
000694180313         ENDIF;
000695180313
000696180313         //?per prima cosa cerco il n. ORM sul rcd 'M' del TITA4
000697180313         clear dsBL4M;
000698180313         ktrc = 'M';
000699180313         chain (TASaas:TASlnp:TASnrs:TASnsp:ktrc) TITA430C;
000700180313         IF  not %found(TITA430C);
000701180313           leavesr;
000702180313         ENDIF;
000703180313         dsBL4M = TA4not;
000704180313
000705180313         //?con il n. ORM leggo FIDPO per numero ORM e
000706180313         //?recupero il n. parcel dal primo record che trovo
000707180313         clear PNDipn;
000708180313         setll (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000709180313         reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000710180313         DOW  not %eof(FIDPO01L);
000711180313           IF  DPOnrp <> *blanks;
000712180313             PNDipn = DPOnrp;
000713180313             leave;
000714180313           ENDIF;
000715180313           reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000716180313         ENDDO;
000717180313
000718180313       ENDSR;
000719180313
000720180313       //-------------------------------------------------------------**
000721180313       // Cerco il n. parcel DPD della bolla mamma.
000722180313       //-------------------------------------------------------------**
000723180313       BEGSR CercaParcelMamma;
000724180313
000725180313         //?come prima cosa cerco se c'è una bolla mamma
000726180313         chain (TASaas:TASlnp:TASnrs:TASnsp) FNLBL01L;
000727180313         IF  not %found(FNLBL01L);
000728180313           leavesr;
000729180313         ENDIF;
000730180313         //?trovo la bolla mamma cerco il n. parcel su FIPND
000731180313         chain (LBLaao:LBLlpo:LBLnro:LBLnsp) FIPND01L;
000732180313         IF  not %found(FIPND01L);
000733180313           clear PNDipn;
000734180313         ENDIF;
000735180313         //?se l'ho trovato ok
000736180313         IF  PNDipn <> *blanks;
000737180313           leavesr;
000738180313         ENDIF;
000739180313         //?se non l'ho trovato
000740180313         //?cerco l'ORM legato alla bolla mamma
000741180313         clear dsBL4M;
000742180313         ktrc = 'M';
000743180313         chain (LBLaao:LBLlpo:LBLnro:LBLnso:ktrc) TITA430C;
000744180313         IF  not %found(TITA430C);
000745180313           leavesr;
000746180313         ENDIF;
000747180313         dsBL4M = TA4not;
000748180313
000749180313         //?con il n. ORM leggo FIDPO per numero ORM
000750180313         //?e recupero il n. parcel dal primo record che trovo
000751180313         clear PNDipn;
000752180313         setll (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000753180313         reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000754180313         DOW  not %eof(FIDPO01L);
000755180313           IF  DPOnrp <> *blanks;
000756180313             PNDipn = DPOnrp;
000757180313             leave;
000758180313           ENDIF;
000759180313           reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000760180313         ENDDO;
000761180313
000762180313       ENDSR;
000763180313
000764180313      /end-free
000765180313      *
000766180313      **-------------------------------------------------------------**
000767180313      ** Wrt_TILAC  ** Registrazione dati nel file TILAC00F x cliente**
000768180313      **-------------------------------------------------------------**
000769180313     c     Wrt_TILAC     BEGSR
000770180313
000771180313     c                   eval      titasdsChk = titasds
000772180313     c*
000773180313     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
000774180313     c     K04tas30      chain     fnlbl01l
000775180313     c                   if        %found(fnlbl01l)
000776180313     c                   setoff                                       313233
000777180313     c     Ktas          chain(N)  titas30c
000778180313     c                   if        %found(titas30c)
000779180313     c                   eval      titasdschk = titasds
000780180313     c                   endif
000781180313     c* richaino la bolla figlia
000782180313     c                   setoff                                       313233
000783180313     c     ktas30_f      chain     titas30c
000784180313     c                   endif
000785180313     c
000786180313     c                   if        Chk_tastbl <> savtbl
000787180313     c                   movel(p)  Chk_tastbl    kkey
000788180313     c                   clear                   dstb
000789180313     c     ktab          chain     tabel00f
000790180313     c                   if        %found(tabel00f)
000791180313     c                   movel     tbluni        dstb
000792180313     c                   endif
000793180313     c                   eval      savtbl = Chk_tastbl
000794180313     c                   endif
000795180313
000796180313     c* controllo porto (rispetto alla bolla originale)
000797180313     c                   if        $tptlac(XX) <> 'E' and §tbtpo <> $tptlac(xx)
000798180313     c                   goto      NOwrtTILAC
000799180313     c                   endif
000800180313     c* reso (rispetto alla bolla corrente (figlia))
000801180313     c                   if        $reslac(xx) <> 'S' and tasfbr = 'S'
000802180313     c                   goto      NOwrtTILAC
000803180313     c                   endif
000804180313     c* recupero (rispetto alla bolla originale)
000805180313     c                   if        $reclac(xx) <> 'S' and §tbrbl = 'R'
000806180313     c                   goto      NOwrtTILAC
000807180313     c                   endif
000808180313     c* c/servizio (rispetto alla bolla originale)
000809180313     c                   if        $csrlac(xx) <> 'S' and §tbrbl = 'C'
000810180313     c                   goto      NOwrtTILAC
000811180313     c                   endif
000812180313     c* serie (rispetto alla bolla originale)
000813180313     c                   if        $ssrlac(xx) = 'S' and chk_tasnrs = *zeros
000814180313     c                   goto      NOwrtTILAC
000815180313     c                   endif
000816180313     c* linea di partenza (rispetto alla bolla originale)
000817180313     c                   if        $lnplac(xx) <> *zeros and
000818180313     c                             chk_taslnp <> $lnplac(xx)
000819180313     c                   goto      NOwrtTILAC
000820180313     c                   endif
000821180313
000822180313     c                   eval      $cli = *on
000823180313      *
000824180313      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
000825180313      * nuova vista logica per Directory e solo '00'
000826180313     C                   eval      Tilac_U_W = 'W'
000827180313
000828180313      /free
000829180313       // -?Cerco con NSP + DIR solo ELA 00?
000830180313       kLACdir = $dirLAC(xx);
000831180313       chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
000832180313       IF  %found(TILAC04L);
000833180313         Tilac_U_W = 'U';
000834180313       ENDIF;
000835180313      /end-free
000836180313
000837180313      *
000838180313      * Preparazione del record
000839180313     c                   clear                   TILAC000
000840180313     c                   move      DS_Time2      LACtim
000841180313     c                   move      TASaas        LACaas
000842180313     c                   move      TASlnp        LAClnp
000843180313     c                   move      TASnrs        LACnrs
000844180313     c                   move      TASnsp        LACnsp
000845180313     c                   movel     'CLI'         LACcop
000846180313     c                   movel     $dirlac(xx)   LACdir
000847180313     c                   movel     $tpilac(xx)   LACtpi
000848180313     c* flag nome immagine:
000849180313     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
000850180313     c*           sa Laguna) --> quindi lascio = blanks
000851180313if  1c                   if        $fmilac(xx)  <> 'S '
000852180313      /free
000853180313       clear  TRUL55ds;
000854180313       I55fmi = $FMIlac(xx);
000855180313       I55lnp = TASlnp;
000856180313       I55nrs = TASnrs;
000857180313       I55nsp = TASnsp;
000858180313       I55aas = TASaas;
000859180313       I55mgs = TASmgs;
000860180313       I55rmn = TASrmn;
000861180313       I55fld = 'L';
000862180313       trul55r ( TRUL55ds );
000863180313       if  O55err = *off;
000864180313         LACnim = O55niml;
000865180313       endif;
000866180313      /end-free
000867180313e   1c                   endif
000868180313
000869180313      * flag per creare titas, non creare o con varia negata
000870180313     c                   eval      lactad = $tadlac(xx)
000871180313
000872180313      * cliente e tariffa tassazione
000873180313      * --> se forzati prendo da tabella
000874180313     c                   if        $ksclac(xx) <> *zeros
000875180313     c                   eval      lacidl = %editc($ksclac(xx):'X') +
000876180313     c                                      $ctrlac(xx)
000877180313     c                   else
000878180313      * --> altrimenti dalla bolla
000879180313     c                   eval      lacidl = %editc(tasksc:'X') +
000880180313     c                                      %editc(tasctr:'X')
000881180313     c                   endif
000882180313     c                   move      *zeros        LACela
000883180313     c                   eval      lactela = 'A'
000884180313     c                   eval      lacksc = $clilac(xx)
000885180313     c                   eval      lacksu = $ksulac(xx)
000886180313     c                   eval      lactadu = $tadulac(xx)
000887180313     c                   eval      lacfimp = $fimplac(xx)
000888180313     c                   eval      lacimp = $implac(xx)
000889180313      * calcolo ID job in base alla frequenza addebito
000890180313     c                   select
000891180313      * - se frequenza addebito per Immagine
000892180313     c                   when      lactadu = 'I'
000893180313      *   id = n.spedizione
000894180313     c                   eval      lacidjob = lacaas + laclnp +
000895180313     c                                        lacnrs + lacnsp
000896180313      * - se frequenza addebito per Job
000897180313     c                   when      lactadu = 'J'
000898180313      *   id = ksu + oggi
000899180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000900180313     c                                        %editc(dst2_ymd:'X')
000901180313      * - se frequenza addebito per Settimana
000902180313     c                   when      lactadu = 'S'
000903180313      *   ricerco il n. della settimana
000904180313     c     *iso          movel     dsT2_ymd      WdateISO
000905180313     c                   clear                   nrweek
000906180313     c                   call      'XSRWEEK'
000907180313     c                   parm                    wdateiso
000908180313     c                   parm                    nrweek
000909180313      *   imposto il campo LACADDMS
000910180313     c                   eval      wyear = %subdt(wdateiso:*years)
000911180313     c                   eval      waddams = %editc(wyear:'X') +
000912180313     c                                       %editc(nrweek:'X')
000913180313     c                   eval      lacaddms = %dec(waddams:6:0)
000914180313      *   id = ksu + S + AAAA + n.settimana
000915180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000916180313     c                                        'S' + waddams
000917180313      * - se frequenza addebito per Mese
000918180313     c                   when      lactadu = 'M'
000919180313      *   imposto il campo LACADDMS
000920180313     c                   eval      lacaddms =
000921180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
000922180313      *   id = ksu + M + AAAA + mese
000923180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000924180313     c                                        'M' + %editc(lacaddms:'X')
000925180313     c                   endsl
000926180313      *
000927180313      * Scrittura /aggiornamento
000928180313     C                   IF        Tilac_U_W = 'U'
000929180313     c                   update    TILAC000
000930180313     c                   ELSE
000931180313     c                   write     TILAC000
000932180313     c                   ENDIF
000933180313      *
000934180313     c     NOwrtTILAC    ENDSR
000935180313
000936180313      **-------------------------------------------------------------**
000937180313      ** Wrt_TIJDC  ** Registrazione dati nel file TIJDC00F x cliente**
000938180313      **-------------------------------------------------------------**
000939180313     c     Wrt_TIJDC     BEGSR
000940180313
000941180313      * se non c'è la tabella del cliente imposto fisso 'CL1' in COP
000942180313     c                   IF        not $cliJDC
000943180313     c                   eval      kJDCcop = 'CL1'
000944180313     c                   ENDIF
000945180313
000946180313      * Se su TIJDC esiste già un record non elaborato aggiorno else scrivo
000947180313     C                   eval      Tijdc_U_W = 'W'
000948180313      *
000949180313     C     k05jdc        setll     Tijdc01L
000950180313     C     k05jdc        reade     Tijdc01L
000951180313     C                   DOW       NOT %EOF(TIJDC01L)
000952180313     C                   IF        JDCela = '00'
000953180313     C                   eval      Tijdc_U_W = 'U'
000954180313     c                   leave
000955180313     C                   ELSE
000956180313     C     k05jdc        reade     Tijdc01L
000957180313     c                   ENDIF
000958180313     c                   ENDDO
000959180313
000960180313      * Preparazione del record
000961180313     c                   clear                   TIJDC000
000962180313     c                   eval      JDCtim = LACtim
000963180313     c                   eval      JDCaas = LACaas
000964180313     c                   eval      JDClnp = LAClnp
000965180313     c                   eval      JDCnrs = LACnrs
000966180313     c                   eval      JDCnsp = LACnsp
000967180313     c                   eval      JDCcop = kJDCcop
000968180313     c                   eval      JDCdir = §JDCdir
000969180313     c                   eval      JDCtpi = §JDCtpi
000970180313if  1c                   eval      JDCnim = LACnim
000971180313     c                   eval      JDCela = '00'
000972180313     c                   eval      JDCtela = 'A'
000973180313     c                   eval      JDCksc = LACksc
000974180313     c                   eval      JDCksu = §JDCksu
000975180313     c                   eval      JDCtadu = §JDCtadu
000976180313
000977180313      * calcolo ID job in base alla frequenza addebito
000978180313     c                   select
000979180313      * - se frequenza addebito per Immagine
000980180313     c                   when      JDCtadu = 'I'
000981180313      *   id = n.spedizione
000982180313     c                   eval      JDCidjob = JDCaas + JDClnp +
000983180313     c                                        JDCnrs + JDCnsp
000984180313      * - se frequenza addebito per Job
000985180313     c                   when      JDCtadu = 'J'
000986180313      *   id = ksu + oggi
000987180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
000988180313     c                                        %editc(dst2_ymd:'X')
000989180313      * - se frequenza addebito per Settimana
000990180313     c                   when      JDCtadu = 'S'
000991180313      *   ricerco il n. della settimana
000992180313     c     *iso          movel     dsT2_ymd      WdateISO
000993180313     c                   clear                   nrweek
000994180313     c                   call      'XSRWEEK'
000995180313     c                   parm                    wdateiso
000996180313     c                   parm                    nrweek
000997180313      *   imposto il campo LACADDMS
000998180313     c                   eval      wyear = %subdt(wdateiso:*years)
000999180313     c                   eval      waddams = %editc(wyear:'X') +
001000180313     c                                       %editc(nrweek:'X')
001001180313     c                   eval      JDCaddms = %dec(waddams:6:0)
001002180313      *   id = ksu + S + AAAA + n.settimana
001003180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
001004180313     c                                        'S' + waddams
001005180313      * - se frequenza addebito per Mese
001006180313     c                   when      JDCtadu = 'M'
001007180313      *   imposto il campo LACADDMS
001008180313     c                   eval      JDCaddms =
001009180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
001010180313      *   id = ksu + M + AAAA + mese
001011180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
001012180313     c                                        'M' + %editc(JDCaddms:'X')
001013180313     c                   endsl
001014180313
001015180313      * memorizzo anche i campi che per ora non uso
001016180313     c                   eval      JDCfimp = §JDCfimp
001017180313     c                   eval      JDCimp = §JDCimp
001018180313      * flag per creare titas, non creare o con varia negata
001019180313     c                   eval      JDCtad = §JDCtad
001020180313
001021180313      * cliente e tariffa tassazione
001022180313      * --> se forzati prendo da tabella
001023180313     c                   if        §JDCksc <> *zeros
001024180313     c                   eval      JDCidl = %editc(§JDCksc:'X') + §JDCctr
001025180313     c                   else
001026180313      * --> altrimenti dalla bolla
001027180313     c                   eval      JDCidl = %editc(TASccm:'X') +
001028180313     c                                      %editc(TASctr:'X')
001029180313     c                   endif
001030180313      *
001031180313      * Scrittura /aggiornamento
001032180313     C                   IF        Tijdc_U_W = 'U'
001033180313     c                   update    TIJDC000
001034180313     c                   ELSE
001035180313     c                   write     TIJDC000
001036180313     c                   ENDIF
001037180313
001038180313     c                   ENDSR
001039180313      *
001040180313      **-------------------------------------------------------------**
001041180313      ** Wrt_TILAC2 ** Scrivo TILAC per immagine da tabella LA2      **
001042180313      **-------------------------------------------------------------**
001043180313     c     Wrt_TILAC2    BEGSR
001044180313
001045180313     c                   eval      titasdsChk = titasds
001046180313     c*
001047180313     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
001048180313     c     K04tas30      chain     fnlbl01l
001049180313     c                   if        %found(fnlbl01l)
001050180313     c                   setoff                                       313233
001051180313     c     Ktas          chain(N)  titas30c
001052180313     c                   if        %found(titas30c)
001053180313     c                   eval      titasdschk = titasds
001054180313     c                   endif
001055180313     c* richaino la bolla figlia
001056180313     c                   setoff                                       313233
001057180313     c     ktas30_f      chain     titas30c
001058180313     c                   endif
001059180313     c
001060180313     c                   if        Chk_tastbl <> savtbl
001061180313     c                   movel(p)  Chk_tastbl    kkey
001062180313     c                   clear                   dstb
001063180313     c     ktab          chain     tabel00f
001064180313     c                   if        %found(tabel00f)
001065180313     c                   movel     tbluni        dstb
001066180313     c                   endif
001067180313     c                   eval      savtbl = Chk_tastbl
001068180313     c                   endif
001069180313
001070180313     c* controllo porto (rispetto alla bolla originale)
001071180313     c                   if        §LA2tpt <> 'E' and §tbtpo <> §LA2tpt
001072180313     c                   leavesr
001073180313     c                   endif
001074180313     c* reso (rispetto alla bolla corrente (figlia))
001075180313     c                   if        §LA2res <> 'S' and tasfbr = 'S'
001076180313     c                   leavesr
001077180313     c                   endif
001078180313     c* recupero (rispetto alla bolla originale)
001079180313     c                   if        §LA2rec <> 'S' and §tbrbl = 'R'
001080180313     c                   leavesr
001081180313     c                   endif
001082180313     c* c/servizio (rispetto alla bolla originale)
001083180313     c                   if        §LA2csr <> 'S' and §tbrbl = 'C'
001084180313     c                   leavesr
001085180313     c                   endif
001086180313     c* serie (rispetto alla bolla originale)
001087180313     c                   if        §LA2ssr = 'S' and chk_tasnrs = *zeros
001088180313     c                   leavesr
001089180313     c                   endif
001090180313     c* linea di partenza (rispetto alla bolla originale)
001091180313     c                   if        §LA2lnp <> *zeros and
001092180313     c                             chk_taslnp <> §LA2lnp
001093180313     c                   leavesr
001094180313     c                   endif
001095180313
001096180313     c                   eval      $cli = *on
001097180313      *
001098180313      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
001099180313      * nuova vista logica per Directory e solo '00'
001100180313     C                   eval      Tilac_U_W = 'W'
001101180313
001102180313      /free
001103180313       // -?Cerco con NSP + DIR solo ELA 00?
001104180313       kLACdir = §LA2dir;
001105180313       chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
001106180313       IF  %found(TILAC04L);
001107180313         Tilac_U_W = 'U';
001108180313       ENDIF;
001109180313      /end-free
001110180313
001111180313      *
001112180313      * Preparazione del record
001113180313     c                   clear                   TILAC000
001114180313     c                   move      DS_Time2      LACtim
001115180313     c                   move      TASaas        LACaas
001116180313     c                   move      TASlnp        LAClnp
001117180313     c                   move      TASnrs        LACnrs
001118180313     c                   move      TASnsp        LACnsp
001119180313     c                   movel     'CLI'         LACcop
001120180313     c                   movel     §LA2dir       LACdir
001121180313     c                   movel     §LA2tpi       LACtpi
001122180313     c* flag nome immagine:
001123180313     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
001124180313     c*           sa Laguna) --> quindi lascio = blanks
001125180313if  1c                   if        §LA2fmi <> 'S '
001126180313      /free
001127180313       clear  TRUL55ds;
001128180313       I55fmi = §LA2fmi;
001129180313       I55lnp = TASlnp;
001130180313       I55nrs = TASnrs;
001131180313       I55nsp = TASnsp;
001132180313       I55aas = TASaas;
001133180313       I55mgs = TASmgs;
001134180313       I55rmn = TASrmn;
001135180313       I55fld = 'L';
001136180313       trul55r ( TRUL55ds );
001137180313       if  O55err = *off;
001138180313         LACnim = O55niml;
001139180313       endif;
001140180313      /end-free
001141180313e   1c                   endif
001142180313
001143180313      * flag per creare titas, non creare o con varia negata
001144180313     c                   eval      lactad = §LA2tad
001145180313
001146180313      * cliente e tariffa tassazione
001147180313      * --> se forzati prendo da tabella
001148180313     c                   if        §LA2ksc <> *zeros
001149180313     c                   eval      lacidl = %editc(§LA2ksc:'X') +
001150180313     c                                      §LA2ctr
001151180313     c                   else
001152180313      * --> altrimenti dalla bolla
001153180313     c                   eval      lacidl = %editc(tasksc:'X') +
001154180313     c                                      %editc(tasctr:'X')
001155180313     c                   endif
001156180313     c                   move      *zeros        LACela
001157180313     c                   eval      lactela = 'A'
001158180313     c                   eval      lacksc = $clilac(xx)
001159180313     c                   eval      lacksu = §LA2ksu
001160180313     c                   eval      lactadu = §LA2tadu
001161180313     c                   eval      lacfimp = §LA2fimp
001162180313     c                   eval      lacimp = §LA2imp
001163180313      * calcolo ID job in base alla frequenza addebito
001164180313     c                   select
001165180313      * - se frequenza addebito per Immagine
001166180313     c                   when      lactadu = 'I'
001167180313      *   id = n.spedizione
001168180313     c                   eval      lacidjob = lacaas + laclnp +
001169180313     c                                        lacnrs + lacnsp
001170180313      * - se frequenza addebito per Job
001171180313     c                   when      lactadu = 'J'
001172180313      *   id = ksu + oggi
001173180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001174180313     c                                        %editc(dst2_ymd:'X')
001175180313      * - se frequenza addebito per Settimana
001176180313     c                   when      lactadu = 'S'
001177180313      *   ricerco il n. della settimana
001178180313     c     *iso          movel     dsT2_ymd      WdateISO
001179180313     c                   clear                   nrweek
001180180313     c                   call      'XSRWEEK'
001181180313     c                   parm                    wdateiso
001182180313     c                   parm                    nrweek
001183180313      *   imposto il campo LACADDMS
001184180313     c                   eval      wyear = %subdt(wdateiso:*years)
001185180313     c                   eval      waddams = %editc(wyear:'X') +
001186180313     c                                       %editc(nrweek:'X')
001187180313     c                   eval      lacaddms = %dec(waddams:6:0)
001188180313      *   id = ksu + S + AAAA + n.settimana
001189180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001190180313     c                                        'S' + waddams
001191180313      * - se frequenza addebito per Mese
001192180313     c                   when      lactadu = 'M'
001193180313      *   imposto il campo LACADDMS
001194180313     c                   eval      lacaddms =
001195180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
001196180313      *   id = ksu + M + AAAA + mese
001197180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001198180313     c                                        'M' + %editc(lacaddms:'X')
001199180313     c                   endsl
001200180313      *
001201180313      * Scrittura /aggiornamento
001202180313     C                   IF        Tilac_U_W = 'U'
001203180313     c                   update    TILAC000
001204180313     c                   ELSE
001205180313     c                   write     TILAC000
001206180313     c                   ENDIF
001207180313      *
001208180313     c                   ENDSR
001209180313      *
001210180313      **-------------------------------------------------------------**
001211180313      ** Wrt_TILAD  ** Registrazione dati nel file TILAD00F          **
001212180313      **-------------------------------------------------------------**
001213180313     c     Wrt_TILAD     BEGSR
001214180313      *
001215180313     c                   clear                   TILAD000
001216180313     c                   eval      LADtim = LAGtim
001217180313     c                   eval      LADaas = LAGaas
001218180313     c                   eval      LADlnp = LAGlnp
001219180313     c                   eval      LADnrs = LAGnrs
001220180313     c                   eval      LADnsp = LAGnsp
001221180313     c                   eval      LADxx1 = LAGxx1
001222180313     c                   eval      LADcop = 'MOV'
001223180313     c                   eval      LADdir = 'ERRATE'
001224180313     c                   eval      LADela = *zeros
001225180313      *
001226180313     c                   WRITE     TILAD000                             99
001227180313      *
001228180313      * Cancellazione record dal file TILAG00R
001229180313     c                   DELETE    TILAG000
001230180313      *
001231180313     c                   ENDSR
001232180313      *
001233180313      **-------------------------------------------------------------**
001234180313      ** Upd_TITAS  ** Aggiornamento dati nel file TITAS30C          **
001235180313      **-------------------------------------------------------------**
001236180313     c     Upd_TITAS     BEGSR
001237180313      *
001238180313      * Possono esserci 2 records su TITAS30C
001239180313      *   (con "tipo record" diverso)
001240180313do  1c                   dou       %eof(TITAS30C)
001241180313      *
001242180313     c                   movel     TASflo        dTASflo
001243180313if  2c                   if        $DPD    = *off  and $CLI = *off
001244180313     c                   eval      §FLOiml = 'S'
001245180313x   2c                   else
001246180313     c                   eval      §FLOiml = 'I'
001247180313e   2c                   endif
001248180313     c                   movel     dTASflo       TASflo
001249180313      *
001250180313     c   31              UPDATE    TITAS000
001251180313     c   32              UPDATE    TITAS010
001252180313     c   33              UPDATE    TITASP00
001253180313      *
001254180313     c                   setoff                                       313233
001255180313     c     K04TAS30      reade     TITAS30C
001256180313      *
001257180313e   1c                   enddo
001258180313      *
001259180313      * Cancellazione record dal file TILAG00R
001260180313     c                   DELETE    TILAG000
001261180313      *
001262180313     c                   ENDSR
