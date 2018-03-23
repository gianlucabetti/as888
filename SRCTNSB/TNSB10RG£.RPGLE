000100180313      *===============================================================*
000101180313      * TNSB10R    * Aggiornamento TITAS per immagine LdV             *
000102180313      *===============================================================*
000103180313
000104180313     h decedit('0,') datedit(*dmy/)
000105180313     h option(*nodebugio)
000106180313
000107180313      *---------------------------------------------------------------*
000108180313
000109180313     fAZORG01L  if   e           k disk
000110180313     fTNTBE01L  if   e           k disk
000111180313     fTABEL00F  if   e           k disk
000112180313      *
000113180313     fFNLBL01L  if   e           k disk
000114180313     fTITA430C  if   e           k disk
000115180313      *
000116180313     fFIILV01L  Uf   e           k disk
000117180313      *
000118180313     fTILAG00R  Uf   e           k disk
000119180313      *
000120180313     fTITAS30C  Uf   e           k disk
000121180313     fFIAR531C  if   e           k disk
000122180313
000123180313     fFIDPO01L  if   e           k disk
000124180313      *
000125180313     fTILAO01L  Uf a e           k disk
000126180313     fTILAC01L  Uf a e           k disk    rename(tilac000:tilac01)
000127180313     fTILAC04L  uf a e           k disk
000128180313     fTIJDC01L  uf a e           k disk    prefix(JDC:3)
000129180313     fFIPND01L  if   e           k disk
000130180313     fTILAD00F  O    e             disk
000131180313
000132180313      *---------------------------------------------------------------*
000133180313
000134180313      *
000135180313      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
000136180313      *
000137180313     d $poDPD          s                   like(ORGfil) dim(999) inz
000138180313     d $clilac         s              7  0 dim(999) inz
000139180313     d $tptlac         s                   like(§lactpt) dim(999) inz
000140180313     d $reslac         s                   like(§lacres) dim(999) inz
000141180313     d $reclac         s                   like(§lacrec) dim(999) inz
000142180313     d $csrlac         s                   like(§laccsr) dim(999) inz
000143180313     d $ssrlac         s                   like(§lacssr) dim(999) inz
000144180313     d $dirlac         s                   like(§lacdir) dim(999) inz
000145180313     d $tpilac         s                   like(§lactpi) dim(999) inz
000146180313     d $fmilac         s                   like(§lacfmi) dim(999) inz
000147180313     d $TADlac         s                   like(§LACtad) dim(999) inz
000148180313     d $tadulac        s                   like(§lactadu) dim(999) inz
000149180313     d $fimplac        s                   like(§lacfimp) dim(999) inz
000150180313     d $implac         s                   like(§lacimp) dim(999) inz
000151180313     d $ksclac         s                   like(§lacksc) dim(999) inz
000152180313     d $ctrlac         s                   like(§lacctr) dim(999) inz
000153180313     d $ksulac         s                   like(§lacksu) dim(999) inz
000154180313     d $lnplac         s                   like(§laclnp) dim(999) inz
000155180313     d $merge          s                   like(§LACmerge) dim(999) inz
000156180313     d $flgLA2         s                   like(§LACla2) dim(999) inz
000157180313      *
000158180313      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
000159180313      *
000160180313      * - Ds per controlli bolla
000161180313     d titasds       e ds                  extname(TITAS00F)
000162180313     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
000163180313
000164180313     d OG143         e ds                  inz
000165180313     d dTASflo       e ds                  inz
000166180313     d dlac          e ds                  inz
000167180313     d dLA2          e ds                  inz
000168180313     d dstb          e ds                  inz
000169180313     d dsTBdpd       e ds                  extname(dstb)
000170180313     d                                     inz prefix(dpd)
000171180313     d dsbl4m        e ds
000172180313     d dJDC          e ds                  inz
000173180313     d dAR5doc       e ds                  inz
000174180313
000175180313       // - Parametri x TRUL55R
000176180313     d TRUL55ds      e ds                  inz
000177180313     d   I55tla      e                     inz('C')
000178180313
000179180313      *
000180180313     d DS_Time1        ds            14    inz
000181180313     d   dsT1_hms                     6  0 inz
000182180313     d   dsT1_dmy                     8  0 inz
000183180313     d DS_Time2        ds            14    inz
000184180313     d   dsT2_ymd                     8  0 inz
000185180313     d   dsT2_hms                     6  0 inz
000186180313      *
000187180313     d Status         sds           333
000188180313     d  SDSjob               244    253                                         Job name
000189180313      *
000190180313      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
000191180313      *
000192180313      * - Flag booleani
000193180313     d $DPD            s              1    inz(*off)
000194180313     d $CLI            s              1    inz(*off)
000195180313     d $cliLA2         s               n   inz(*off)
000196180313     d $cliJDC         s              1n   inz(*off)
000197180313      * - Indici di schiera
000198180313     d xx              s              4  0 inz
000199180313      * - Campi di comodo
000200180313     d WtasDCM         s              8    inz(*zeros)
000201180313     d WtasRMN         s             15    inz(*zeros)
000202180313     d WtasPOm         s              3  0 inz
000203180313     d Wtime           s             14  0 inz
000204180313     d Wdata_00        s              8  0 inz
000205180313     d Wdata_15        s              8  0 inz
000206180313     d WdateISO        s               d   datfmt(*ISO) inz
000207180313     d kAR5trd         s                   like(AR5trd) inz('DOC')
000208180313     d klaocop         s                   like(laocop) inz('DPD')
000209180313     d klaccop         s                   like(laccop) inz('CLI')
000210180313     d kLACdir         s                   like(LACdir)
000211180313     d kJDCcop         s                   like(JDCcop)
000212180313     d tilao_u_w       s              1
000213180313     d tilac_u_w       s              1
000214180313     d tijdc_u_w       s              1
000215180313     d kcod            s                   like(tblcod) inz('TB')
000216180313     d kkey            s                   like(tblkey)
000217180313     d savtbl          s                   like(tastbl) inz('  ')
000218180313     d WTAS_orig       s              1
000219180313     d ktrc            s                   like(ta4trc)
000220180313     d nrweek          s              2  0 inz
000221180313     d wyear           s              4  0 inz
000222180313     d waddams         s              6    inz
000223180313     d chiudi          s              1
000224180313     d noscrivi        s              1n   inz(*off)
000225180313
000226180313       //--------------------------------------------------------------
000227180313       //?Definizione procedure usate.                                 ?
000228180313       //--------------------------------------------------------------
000229180313
000230180313       // - Determinazione nome dell'immagine per TILAC
000231180313     d trul55r         pr                  extpgm('TRUL55R')
000232180313     d   trul55ds...
000233180313     d                                     likeds(trul55ds)
000234180313
000235180313       //--------------------------------------------------------------
000236180313       //?Definizione key-list.                                        ?
000237180313       //--------------------------------------------------------------
000238180313
000239180313       // - File FIILV01L
000240180313     d k06fiilv01    e ds                  extname( FIILV01L : *key )
000241180313     d                                     prefix(k_)   inz
000242180313
000243180313      *---------------------------------------------------------------*
000244180313
000245180313      * Predisposizione indicatori per riconoscere i tipi record
000246180313      *   reperiti dal file TITAS30C
000247180313     iTITAS000      31
000248180313     iTITAS010      32
000249180313     iTITASP00      33
000250180313
000251180313      *---------------------------------------------------------------*
000252180313
000253180313      *
000254180313      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
000255180313      *
000256180313     c     K04TAS30      klist
000257180313     c                   kfld                    TASaas
000258180313     c                   kfld                    TASlnp
000259180313     c                   kfld                    TASnrs
000260180313     c                   kfld                    TASnsp
000261180313     c     Ktas30_f      klist
000262180313     c                   kfld                    lblaan
000263180313     c                   kfld                    lbllpn
000264180313     c                   kfld                    lblnrn
000265180313     c                   kfld                    lblnsn
000266180313
000267180313     c     kAR5          klist
000268180313     c                   kfld                    TASaas
000269180313     c                   kfld                    TASlnp
000270180313     c                   kfld                    TASnrs
000271180313     c                   kfld                    TASnsp
000272180313     c                   kfld                    kAR5trd
000273180313      *
000274180313     c     K05LAO        klist
000275180313     c                   kfld                    LAGaas
000276180313     c                   kfld                    LAGlnp
000277180313     c                   kfld                    LAGnrs
000278180313     c                   kfld                    LAGnsp
000279180313     c                   kfld                    klaocop
000280180313     c*
000281180313     c     K05LAC        klist
000282180313     c                   kfld                    LAGaas
000283180313     c                   kfld                    LAGlnp
000284180313     c                   kfld                    LAGnrs
000285180313     c                   kfld                    LAGnsp
000286180313     c                   kfld                    klaccop
000287180313     c     K05JDC        klist
000288180313     c                   kfld                    LAGaas
000289180313     c                   kfld                    LAGlnp
000290180313     c                   kfld                    LAGnrs
000291180313     c                   kfld                    LAGnsp
000292180313     c                   kfld                    kJDCcop
000293180313
000294180313     c     Ktas          klist
000295180313     c                   kfld                    LBLaao
000296180313     c                   kfld                    LBLlpo
000297180313     c                   kfld                    LBLnro
000298180313     c                   kfld                    LBLnso
000299180313      *
000300180313     c     Kta4          klist
000301180313     c                   kfld                    TASaas
000302180313     c                   kfld                    TASlnp
000303180313     c                   kfld                    TASnrs
000304180313     c                   kfld                    TASnsp
000305180313     c                   kfld                    ktrc
000306180313     c*
000307180313     c     Ktab          klist
000308180313     C                   KFLD                    CODUT
000309180313     C                   KFLD                    kcod
000310180313     c                   kfld                    kkey
000311180313     c                   z-add     1             codut             1 0
000312180313      *
000313180313      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
000314180313      *
000315180313      * 20    - No cliente in tabella con LAC FTP
000316180313      * 21    - P.O.partenza del network DPD
000317180313      * 31-33 - Tracciato reperito dal file TITAS30C (000/010/P00)
000318180313      * 99    - Errore in scrittura record nel file TILAD00F
000319180313
000320180313      *---------------------------------------------------------------*
000321180313
000322180313     c     *entry        plist
000323180313     c                   parm                    chiudi
000324180313
000325180313      * Operazioni Iniziali
000326180313     c                   exsr      OperazIniz
000327180313      *
000328180313      * Ciclo di lettura del file TILAG00R
000329180313     c                   read      TILAG000
000330180313      *
000331180313do  1c                   dow       NOT %eof(TILAG00R)
000332180313      *
000333180313      * - Elaborazione dei soli rec. "IM"
000334180313if  2c                   if        LAGapp = 'IM'
000335180313      *
000336180313      * - Aggancio record corrispondente nel file TITAS30C
000337180313     c                   movel     '2000'        TASaas
000338180313     c                   move      LAGaas        TASaas
000339180313     c                   move      LAGlnp        TASlnp
000340180313     c                   move      LAGnrs        TASnrs
000341180313     c                   move      LAGnsp        TASnsp
000342180313     c                   setoff                                       313233
000343180313     c     K04TAS30      chain     TITAS30C
000344180313      *
000345180313      * - Aggiornamento dati
000346180313if  3c                   if        %found(TITAS30C)
000347180313     c                   eval      noscrivi = *off
000348180313      *
000349180313     c* verifico se cliente presente in tabella "LAC"
000350180313     c                   eval      *in20   = *off
000351180313     c                   eval      *in21   = *off
000352180313     c                   eval      $cli    = *off
000353180313     c                   eval      $dpd    = *off
000354180313     c                   z-add     1             xx
000355180313     c     tasksc        lookup    $clilac(xx)                            20
000356180313if  4c                   if        *in20 = *on
000357180313       // cliente con richiesta di immagine con merge
000358180313       // se presente l'immagine del DOC scrivo tutti e 2 gli archivi
000359180313     c                   IF        $merge(xx) <> *blanks
000360180313     c                   exsr      ctr_AR5
000361180313     c                   ENDIF
000362180313     c                   IF        not noscrivi
000363180313     c                   exsr      Wrt_TILAC
000364180313     c                   IF        $merge(xx) <> *blanks
000365180313     c                   exsr      wrt_TIJDC
000366180313     c                   ENDIF
000367180313     c                   ENDIF
000368180313       // controllo se devo estrarre due volte la stessa immagine
000369180313     c                   eval      $clila2 = *off
000370180313     c                   IF        $flgLA2(xx) <> *blanks
000371180313     c                   exsr      ctr_LA2
000372180313     c                   IF        $cliLA2
000373180313     c                   exsr      Wrt_TILAC2
000374180313     c                   ENDIF
000375180313     c                   ENDIF
000376180313x   4c                   else
000377180313     c                   exsr      Wrt_TILAO
000378180313e   4c                   endif
000379180313      /free
000380180313             // -?Cancellazione rec. da FIILV00F (non errori)?
000381180313             k_ILVaas = 2000 + %int(LAGaas);
000382180313             k_ILVlnp = %int(LAGlnp);
000383180313             k_ILVnrs = %int(LAGnrs);
000384180313             k_ILVnsp = %int(LAGnsp);
000385180313             clear  k_ILVfgs;
000386180313             clear  k_ILVndc;
000387180313             setll  %kds(k06fiilv01 : 4)  FIILV000;
000388180313             if  %equal(FIILV01L);
000389180313               reade  %kds(k06fiilv01 : 4)  FIILV000;
000390180313               dow  NOT %eof(FIILV01L);
000391180313                 if  (ILVcol > '00'  and  ILVcol < '10')  OR
000392180313                     (ILVcol = '00'  and (ILVnocdst = 'S'
000393180313                                      or  ILVfirpda = 'S'
000394180313                                      or  ILVscan   = ' '));
000395180313                   DELETE  FIILV000;
000396180313                 endif;
000397180313                 reade  %kds(k06fiilv01 : 4)  FIILV000;
000398180313               enddo;
000399180313             endif;
000400180313      /end-free
000401180313     c                   exsr      Upd_TITAS
000402180313      *
000403180313x   3c                   else
000404180313      *
000405180313      * Sposto da TILAG00R in TILAD00F se sped. non trovata su TITAS
000406180313      *   e se record ("time") di almeno 15 gg. fa!
000407180313     c                   movel     LAGtim        wData_00
000408180313if  4c                   if        wData_00 <= wData_15
000409180313     c                   exsr      Wrt_TILAD
000410180313e   4c                   endif
000411180313      *
000412180313e   3c                   endif
000413180313      *
000414180313e   2c                   endif
000415180313      *
000416180313      * - Lettura del record successivo nel file TILAG00R
000417180313     c                   read      TILAG000
000418180313      *
000419180313e   1c                   enddo
000420180313
000421180313      * testa la chiusura del sottosistema
000422180313     c                   shtdn                                        98
000423180313     c   98              eval      chiudi = 'S'
000424180313      *
000425180313      * Fine
000426180313     c                   eval      *inLR = *on
000427180313      /free
000428180313           reset  TRUL55ds;
000429180313           trul55r ( TRUL55ds );
000430180313      /end-free
000431180313      *
000432180313      **-------------------------------------------------------------**
000433180313      ** OperazIniz ** Operazioni Iniziali                           **
000434180313      **-------------------------------------------------------------**
000435180313     c     OperazIniz    BEGSR
000436180313      *
000437180313      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
000438180313     c                   time                    Wtime
000439180313     c                   movel     Wtime         DS_Time1
000440180313     c                   eval      dsT2_hms   = dsT1_hms
000441180313     c     *eur          movel     dsT1_dmy      WdateISO
000442180313     c                   move      WdateISO      dsT2_ymd
000443180313     c                   subdur    15 : *days    WdateISO
000444180313     c                   move      WdateISO      wData_15
000445180313      *
000446180313      * Intabellamento dei p.o. appartenenti al network "DPD"
000447180313     c                   clear                   xx
000448180313     c                   clear                   $poDPD
000449180313     c     *loval        setll     AZORG
000450180313     c                   read      AZORG
000451180313do  1c                   dow       NOT %eof(AZORG01L)
000452180313     c                   movel     ORGde3        OG143
000453180313if  2c                   if        §OGntw     = 'DPD'
000454180313     c                   add       1             xx
000455180313     c                   eval      $poDPD(xx) = ORGfil
000456180313e   2c                   endif
000457180313     c                   read      AZORG
000458180313e   1c                   enddo
000459180313      * Intabellamento tabella "LAC" dei clienti che ricevono le immagini
000460180313     c* in automatico
000461180313     c                   clear                   xx
000462180313     c                   clear                   $clilac
000463180313     c                   clear                   $tptlac
000464180313     c                   clear                   $reslac
000465180313     c                   clear                   $reclac
000466180313     c                   clear                   $csrlac
000467180313     c                   clear                   $ssrlac
000468180313     c                   clear                   $dirlac
000469180313     c                   clear                   $tpilac
000470180313     c                   clear                   $fmilac
000471180313     c                   clear                   $TADlac
000472180313     c                   clear                   $tadulac
000473180313     c                   clear                   $fimplac
000474180313     c                   clear                   $implac
000475180313     c                   clear                   $ksclac
000476180313     c                   clear                   $ctrlac
000477180313     c                   clear                   $ksulac
000478180313     c                   clear                   $lnplac
000479180313     c                   clear                   $merge
000480180313     c     'LAC'         setll     tntbe01l
000481180313     c                   do        *hival
000482180313     c     'LAC'         reade     tntbe01l
000483180313     c                   if        %eof(tntbe01l)
000484180313     c                   leave
000485180313     c                   endif
000486180313     c                   if        tbeatb <> *blanks
000487180313     c                   iter
000488180313     c                   endif
000489180313     c                   movel     tbeuni        dlac
000490180313     c                   if        §lacaut = 'A'
000491180313     c                   add       1             xx
000492180313     c                   movel     tbeke1        $clilac(xx)
000493180313     c                   movel     §lactpt       $tptlac(xx)
000494180313     c                   movel     §lacres       $reslac(xx)
000495180313     c                   movel     §lacrec       $reclac(xx)
000496180313     c                   movel     §laccsr       $csrlac(xx)
000497180313     c                   movel     §lacssr       $ssrlac(xx)
000498180313     c                   movel     §lacdir       $dirlac(xx)
000499180313     c                   movel     §lactpi       $tpilac(xx)
000500180313     c                   movel     §lacfmi       $fmilac(xx)
000501180313     c                   movel     §LACtad       $TADlac(xx)
000502180313     c                   eval      $tadulac(xx) = §lactadu
000503180313     c                   eval      $fimplac(xx) = §lacfimp
000504180313     c                   eval      $implac(xx)  = §lacimp
000505180313     c                   eval      $ksclac(xx)  = §lacksc
000506180313     c                   eval      $ctrlac(xx)  = §lacctr
000507180313     c                   eval      $ksulac(xx)  = §lacksu
000508180313     c                   eval      $lnplac(xx)  = §laclnp
000509180313     c                   eval      $merge(xx)   = §lacmerge
000510180313     c                   eval      $flgla2(xx)  = §lacla2
000511180313     c                   endif
000512180313     c                   enddo
000513180313      *
000514180313     c                   ENDSR
000515180313      /free
000516180313       //-------------------------------------------------------------**
000517180313       // crt_AR5  -- Controllo se arrivato il DOC.
000518180313       //-------------------------------------------------------------**
000519180313        BEGSR ctr_AR5;
000520180313
000521180313          noscrivi = *off;
000522180313
000523180313       //?Se su FIAR5 trovo che il DOC è già presente scrivo LAC e JDC
000524180313          chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
000525180313          IF  not %found(FIAR531C);
000526180313            noscrivi = *on;
000527180313            leavesr;
000528180313          ENDIF;
000529180313          dAR5doc = AR5uni;
000530180313          IF  §AR5jfid = *blanks;
000531180313            noscrivi = *on;
000532180313            leavesr;
000533180313          ENDIF;
000534180313
000535180313       //?Cerco i dati del cliente sulla tabella JDC
000536180313          clear dJDC;
000537180313          $cliJDc = *off;
000538180313          TBEcod = 'JDC';
000539180313          TBEke1 = %editc($clilac(xx):'X');
000540180313          chain (TBEcod:TBEke1) TNTBE01L;
000541180313          IF  %found(TNTBE01L) and TBEatb = *blanks;
000542180313            $cliJDc = *on;
000543180313            dJDC = TBEuni;
000544180313            kJDCcop = §JDCpag;
000545180313          ENDIF;
000546180313
000547180313        ENDSR;
000548180313
000549180313       //-------------------------------------------------------------**
000550180313       // crt_LA2  -- Controllo se estrazione dell'immagine da sola
000551180313       //-------------------------------------------------------------**
000552180313        BEGSR ctr_LA2;
000553180313
000554180313       //?Controllo se esiste il cliente sulla tabella LA2
000555180313          clear dLA2;
000556180313          $cliLA2 = *off;
000557180313          TBEcod = 'LA2';
000558180313          TBEke1 = %editc($clilac(xx):'X');
000559180313          chain (TBEcod:TBEke1) TNTBE01L;
000560180313          IF  %found(TNTBE01L) and TBEatb = *blanks;
000561180313            $cliLA2 = *on;
000562180313            dLA2 = TBEuni;
000563180313          ENDIF;
000564180313
000565180313        ENDSR;
000566180313      /end-free
000567180313      *
000568180313      **-------------------------------------------------------------**
000569180313      ** Wrt_TILAO  ** Registrazione dati nel file TILAO00F x DPD    **
000570180313      **-------------------------------------------------------------**
000571180313     c     Wrt_TILAO     BEGSR
000572180313      *
000573180313      * Verifica se p.o. del mittente DPD
000574180313     c                   eval      *in21   = *off
000575180313     c                   movel     TASccm        WtasPOm
000576180313     c     WtasPOm       lookup    $poDPD                                 21
000577180313      * se filiale mittente no DPD provo con la filiale del KSC
000578180313     c                   IF        not *in21
000579180313     c                   movel     TASksc        WtasPOm
000580180313     c     WtasPOm       lookup    $poDPD                                 21
000581180313     c                   ENDIF
000582180313     c                   eval      $DPD    = *in21
000583180313      *
000584180313     c     $DPD          cabeq     *off          NOwrtTILAO
000585180313      *
000586180313      * Impostazione dei campi numerici in campi alfanumerici di comodo
000587180313     c                   movel     TASdcm        WtasDCM
000588180313     c                   move      TASrmn        Wtasrmn
000589180313      *
000590180313      * Se su TILAO esiste già un record non elaborato aggiorno else scrivo
000591180313     C                   eval      Tilao_U_W = 'W'
000592180313      *
000593180313     C     k05lao        setll     Tilao01L
000594180313     C     k05lao        reade     Tilao01L
000595180313     C                   DOW       NOT %EOF(TILAO01L)
000596180313     C                   IF        LAGela = '00'
000597180313     C                   eval      Tilao_U_W = 'U'
000598180313     c                   leave
000599180313     C                   ELSE
000600180313     C     k05lao        reade     Tilao01L
000601180313     c                   ENDIF
000602180313     c                   ENDDO
000603180313      *
000604180313      * Preparazione del record
000605180313     c                   clear                   TILAO000
000606180313     c                   move      DS_Time2      LAOtim
000607180313     c                   move      TASaas        LAOaas
000608180313     c                   move      TASlnp        LAOlnp
000609180313     c                   move      TASnrs        LAOnrs
000610180313     c                   move      TASnsp        LAOnsp
000611180313     c                   movel     'DPD'         LAOcop
000612180313     c                   clear                   w014a            14
000613180313       chain (TASaas:TASlnp:TASnrs:TASnsp) FIPND01L;
000614180313       IF  not %found(FIPND01L);
000615180313         clear PNDipn;
000616180313       ENDIF;
000617180313      /free
000618180313       //?se non ho trovato il numero parcel DPD ?
000619180313       //?provo a cercarlo con il n. ORM su FIDPO?
000620180313         IF  PNDipn = *blanks;
000621180313           exsr CercaParcel;
000622180313         //?se anche così non trovo il numero parcel
000623180313         //?provo a verificare se la bolla è legata
000624180313         //?quindi cerco il n. parcel con la bolla mamma
000625180313           IF  PNDipn = *blanks;
000626180313             exsr CercaParcelMamma;
000627180313           ENDIF;
000628180313         ENDIF;
000629180313      /end-free
000630180313     c* se non trovato numero parcel lo prendo da rmn
000631180313     c                   if        PNDipn = *blanks
000632180313     c* numero parcel lungo 11 lo trucco con 0 davanti al depot e 99 davanti
000633180313     c* al progressivo
000634180313     c                   if        %subst(wtasrmn:1:3) = '000'
000635180313     c                   eval      w014a = '0'
000636180313     c                                   + %subst(wtasrmn:4:3)
000637180313     c                                   + '99'
000638180313     c                                   + %subst(wtasrmn:7:8)
000639180313     c                   else
000640180313     c                   move      wtasrmn       w014a
000641180313     c                   endif
000642180313     c                   else
000643180313     c                   if        %subst(PNDipn:12:1) = ' '
000644180313     c                   eval      w014a = '0'
000645180313     c                                   + %subst(PNDipn:1:3)
000646180313     c                                   + '99'
000647180313     c                                   + %subst(PNDipn:4:8)
000648180313     c                   else
000649180313     c                   eval      w014a = PNDipn
000650180313     c                   endif
000651180313     c                   endif
000652180313     c                   eval      laoope  = w014a
000653180313     c                                     + '_'
000654180313     c                                     + wtasdcm
000655180313     c                   movel     SDSjob        LAGidl
000656180313     c                   move      *zeros        LAGela
000657180313      *
000658180313      * Scrittura /aggiornamento
000659180313     C                   IF        Tilao_U_W = 'U'
000660180313     c                   update    TILAO000
000661180313     c                   ELSE
000662180313     c                   write     TILAO000
000663180313     c                   ENDIF
000664180313      *
000665180313     c     NOwrtTILAO    ENDSR
000666180313
000667180313      /free
000668180313       //-------------------------------------------------------------**
000669180313       // Cerco il n. parcel DPD dall'ORM.
000670180313       //-------------------------------------------------------------**
000671180313        BEGSR CercaParcel;
000672180313
000673180313       //?lo faccio solo se bolla in assegnato
000674180313       //?NO da giugno 2017 ND ha detto di farlo anche per le bolle Franco
000675180313         clear dsTBdpd;
000676180313         kkey = TAStbl;
000677180313         chain (codut:kcod:kkey) TABEL00F;
000678180313         IF  %found(TABEL00F);
000679180313           dsTBdpd = TBLuni;
000680180313         ENDIF;
000681180313         IF  dpd§TBtpo <> 'A';
000682180313         //  leavesr;
000683180313         ENDIF;
000684180313
000685180313       //?per prima cosa cerco il n. ORM sul rcd 'M' del TITA4
000686180313          clear dsBL4M;
000687180313          ktrc = 'M';
000688180313          chain (TASaas:TASlnp:TASnrs:TASnsp:ktrc) TITA430C;
000689180313          IF  not %found(TITA430C);
000690180313            leavesr;
000691180313          ENDIF;
000692180313          dsBL4M = TA4not;
000693180313
000694180313       //?con il n. ORM leggo FIDPO per numero ORM e
000695180313       //?recupero il n. parcel dal primo record che trovo
000696180313          clear PNDipn;
000697180313          setll (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000698180313          reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000699180313          DOW  not %eof(FIDPO01L);
000700180313            IF  DPOnrp <> *blanks;
000701180313              PNDipn = DPOnrp;
000702180313              leave;
000703180313            ENDIF;
000704180313            reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000705180313          ENDDO;
000706180313
000707180313        ENDSR;
000708180313
000709180313       //-------------------------------------------------------------**
000710180313       // Cerco il n. parcel DPD della bolla mamma.
000711180313       //-------------------------------------------------------------**
000712180313        BEGSR CercaParcelMamma;
000713180313
000714180313       //?come prima cosa cerco se c'è una bolla mamma
000715180313          chain (TASaas:TASlnp:TASnrs:TASnsp) FNLBL01L;
000716180313          IF  not %found(FNLBL01L);
000717180313            leavesr;
000718180313          ENDIF;
000719180313       //?trovo la bolla mamma cerco il n. parcel su FIPND
000720180313          chain (LBLaao:LBLlpo:LBLnro:LBLnsp) FIPND01L;
000721180313          IF  not %found(FIPND01L);
000722180313            clear PNDipn;
000723180313          ENDIF;
000724180313       //?se l'ho trovato ok
000725180313          IF  PNDipn <> *blanks;
000726180313            leavesr;
000727180313          ENDIF;
000728180313       //?se non l'ho trovato
000729180313       //?cerco l'ORM legato alla bolla mamma
000730180313          clear dsBL4M;
000731180313          ktrc = 'M';
000732180313          chain (LBLaao:LBLlpo:LBLnro:LBLnso:ktrc) TITA430C;
000733180313          IF  not %found(TITA430C);
000734180313            leavesr;
000735180313          ENDIF;
000736180313          dsBL4M = TA4not;
000737180313
000738180313       //?con il n. ORM leggo FIDPO per numero ORM
000739180313       //?e recupero il n. parcel dal primo record che trovo
000740180313          clear PNDipn;
000741180313          setll (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000742180313          reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000743180313          DOW  not %eof(FIDPO01L);
000744180313            IF  DPOnrp <> *blanks;
000745180313              PNDipn = DPOnrp;
000746180313              leave;
000747180313            ENDIF;
000748180313            reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
000749180313          ENDDO;
000750180313
000751180313        ENDSR;
000752180313
000753180313      /end-free
000754180313      *
000755180313      **-------------------------------------------------------------**
000756180313      ** Wrt_TILAC  ** Registrazione dati nel file TILAC00F x cliente**
000757180313      **-------------------------------------------------------------**
000758180313     c     Wrt_TILAC     BEGSR
000759180313
000760180313     c                   eval      titasdsChk = titasds
000761180313     c*
000762180313     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
000763180313     c     K04tas30      chain     fnlbl01l
000764180313     c                   if        %found(fnlbl01l)
000765180313     c                   setoff                                       313233
000766180313     c     Ktas          chain(N)  titas30c
000767180313     c                   if        %found(titas30c)
000768180313     c                   eval      titasdschk = titasds
000769180313     c                   endif
000770180313     c* richaino la bolla figlia
000771180313     c                   setoff                                       313233
000772180313     c     ktas30_f      chain     titas30c
000773180313     c                   endif
000774180313     c
000775180313     c                   if        Chk_tastbl <> savtbl
000776180313     c                   movel(p)  Chk_tastbl    kkey
000777180313     c                   clear                   dstb
000778180313     c     ktab          chain     tabel00f
000779180313     c                   if        %found(tabel00f)
000780180313     c                   movel     tbluni        dstb
000781180313     c                   endif
000782180313     c                   eval      savtbl = Chk_tastbl
000783180313     c                   endif
000784180313
000785180313     c* controllo porto (rispetto alla bolla originale)
000786180313     c                   if        $tptlac(XX) <> 'E' and §tbtpo <> $tptlac(xx)
000787180313     c                   goto      NOwrtTILAC
000788180313     c                   endif
000789180313     c* reso (rispetto alla bolla corrente (figlia))
000790180313     c                   if        $reslac(xx) <> 'S' and tasfbr = 'S'
000791180313     c                   goto      NOwrtTILAC
000792180313     c                   endif
000793180313     c* recupero (rispetto alla bolla originale)
000794180313     c                   if        $reclac(xx) <> 'S' and §tbrbl = 'R'
000795180313     c                   goto      NOwrtTILAC
000796180313     c                   endif
000797180313     c* c/servizio (rispetto alla bolla originale)
000798180313     c                   if        $csrlac(xx) <> 'S' and §tbrbl = 'C'
000799180313     c                   goto      NOwrtTILAC
000800180313     c                   endif
000801180313     c* serie (rispetto alla bolla originale)
000802180313     c                   if        $ssrlac(xx) = 'S' and chk_tasnrs = *zeros
000803180313     c                   goto      NOwrtTILAC
000804180313     c                   endif
000805180313     c* linea di partenza (rispetto alla bolla originale)
000806180313     c                   if        $lnplac(xx) <> *zeros and
000807180313     c                             chk_taslnp <> $lnplac(xx)
000808180313     c                   goto      NOwrtTILAC
000809180313     c                   endif
000810180313
000811180313     c                   eval      $cli = *on
000812180313      *
000813180313      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
000814180313      * nuova vista logica per Directory e solo '00'
000815180313     C                   eval      Tilac_U_W = 'W'
000816180313
000817180313      /free
000818180313       // -?Cerco con NSP + DIR solo ELA 00?
000819180313         kLACdir = $dirLAC(xx);
000820180313         chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
000821180313         IF  %found(TILAC04L);
000822180313           Tilac_U_W = 'U';
000823180313         ENDIF;
000824180313      /end-free
000825180313
000826180313      *
000827180313      * Preparazione del record
000828180313     c                   clear                   TILAC000
000829180313     c                   move      DS_Time2      LACtim
000830180313     c                   move      TASaas        LACaas
000831180313     c                   move      TASlnp        LAClnp
000832180313     c                   move      TASnrs        LACnrs
000833180313     c                   move      TASnsp        LACnsp
000834180313     c                   movel     'CLI'         LACcop
000835180313     c                   movel     $dirlac(xx)   LACdir
000836180313     c                   movel     $tpilac(xx)   LACtpi
000837180313     c* flag nome immagine:
000838180313     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
000839180313     c*           sa Laguna) --> quindi lascio = blanks
000840180313if  1c                   if        $fmilac(xx)  <> 'S '
000841180313      /free
000842180313           clear  TRUL55ds;
000843180313           I55fmi = $FMIlac(xx);
000844180313           I55lnp = TASlnp;
000845180313           I55nrs = TASnrs;
000846180313           I55nsp = TASnsp;
000847180313           I55aas = TASaas;
000848180313           I55mgs = TASmgs;
000849180313           I55rmn = TASrmn;
000850180313           I55fld = 'L';
000851180313           trul55r ( TRUL55ds );
000852180313           if  O55err = *off;
000853180313             LACnim = O55niml;
000854180313           endif;
000855180313      /end-free
000856180313e   1c                   endif
000857180313
000858180313      * flag per creare titas, non creare o con varia negata
000859180313     c                   eval      lactad = $tadlac(xx)
000860180313
000861180313      * cliente e tariffa tassazione
000862180313      * --> se forzati prendo da tabella
000863180313     c                   if        $ksclac(xx) <> *zeros
000864180313     c                   eval      lacidl = %editc($ksclac(xx):'X') +
000865180313     c                                      $ctrlac(xx)
000866180313     c                   else
000867180313      * --> altrimenti dalla bolla
000868180313     c                   eval      lacidl = %editc(tasksc:'X') +
000869180313     c                                      %editc(tasctr:'X')
000870180313     c                   endif
000871180313     c                   move      *zeros        LACela
000872180313     c                   eval      lactela = 'A'
000873180313     c                   eval      lacksc = $clilac(xx)
000874180313     c                   eval      lacksu = $ksulac(xx)
000875180313     c                   eval      lactadu = $tadulac(xx)
000876180313     c                   eval      lacfimp = $fimplac(xx)
000877180313     c                   eval      lacimp = $implac(xx)
000878180313      * calcolo ID job in base alla frequenza addebito
000879180313     c                   select
000880180313      * - se frequenza addebito per Immagine
000881180313     c                   when      lactadu = 'I'
000882180313      *   id = n.spedizione
000883180313     c                   eval      lacidjob = lacaas + laclnp +
000884180313     c                                        lacnrs + lacnsp
000885180313      * - se frequenza addebito per Job
000886180313     c                   when      lactadu = 'J'
000887180313      *   id = ksu + oggi
000888180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000889180313     c                                        %editc(dst2_ymd:'X')
000890180313      * - se frequenza addebito per Settimana
000891180313     c                   when      lactadu = 'S'
000892180313      *   ricerco il n. della settimana
000893180313     c     *iso          movel     dsT2_ymd      WdateISO
000894180313     c                   clear                   nrweek
000895180313     c                   call      'XSRWEEK'
000896180313     c                   parm                    wdateiso
000897180313     c                   parm                    nrweek
000898180313      *   imposto il campo LACADDMS
000899180313     c                   eval      wyear = %subdt(wdateiso:*years)
000900180313     c                   eval      waddams = %editc(wyear:'X') +
000901180313     c                                       %editc(nrweek:'X')
000902180313     c                   eval      lacaddms = %dec(waddams:6:0)
000903180313      *   id = ksu + S + AAAA + n.settimana
000904180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000905180313     c                                        'S' + waddams
000906180313      * - se frequenza addebito per Mese
000907180313     c                   when      lactadu = 'M'
000908180313      *   imposto il campo LACADDMS
000909180313     c                   eval      lacaddms =
000910180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
000911180313      *   id = ksu + M + AAAA + mese
000912180313     c                   eval      lacidjob = %editc(lacksu:'X') +
000913180313     c                                        'M' + %editc(lacaddms:'X')
000914180313     c                   endsl
000915180313      *
000916180313      * Scrittura /aggiornamento
000917180313     C                   IF        Tilac_U_W = 'U'
000918180313     c                   update    TILAC000
000919180313     c                   ELSE
000920180313     c                   write     TILAC000
000921180313     c                   ENDIF
000922180313      *
000923180313     c     NOwrtTILAC    ENDSR
000924180313
000925180313      **-------------------------------------------------------------**
000926180313      ** Wrt_TIJDC  ** Registrazione dati nel file TIJDC00F x cliente**
000927180313      **-------------------------------------------------------------**
000928180313     c     Wrt_TIJDC     BEGSR
000929180313
000930180313      * se non c'è la tabella del cliente imposto fisso 'CL1' in COP
000931180313     c                   IF        not $cliJDC
000932180313     c                   eval      kJDCcop = 'CL1'
000933180313     c                   ENDIF
000934180313
000935180313      * Se su TIJDC esiste già un record non elaborato aggiorno else scrivo
000936180313     C                   eval      Tijdc_U_W = 'W'
000937180313      *
000938180313     C     k05jdc        setll     Tijdc01L
000939180313     C     k05jdc        reade     Tijdc01L
000940180313     C                   DOW       NOT %EOF(TIJDC01L)
000941180313     C                   IF        JDCela = '00'
000942180313     C                   eval      Tijdc_U_W = 'U'
000943180313     c                   leave
000944180313     C                   ELSE
000945180313     C     k05jdc        reade     Tijdc01L
000946180313     c                   ENDIF
000947180313     c                   ENDDO
000948180313
000949180313      * Preparazione del record
000950180313     c                   clear                   TIJDC000
000951180313     c                   eval      JDCtim = LACtim
000952180313     c                   eval      JDCaas = LACaas
000953180313     c                   eval      JDClnp = LAClnp
000954180313     c                   eval      JDCnrs = LACnrs
000955180313     c                   eval      JDCnsp = LACnsp
000956180313     c                   eval      JDCcop = kJDCcop
000957180313     c                   eval      JDCdir = §JDCdir
000958180313     c                   eval      JDCtpi = §JDCtpi
000959180313if  1c                   eval      JDCnim = LACnim
000960180313     c                   eval      JDCela = '00'
000961180313     c                   eval      JDCtela = 'A'
000962180313     c                   eval      JDCksc = LACksc
000963180313     c                   eval      JDCksu = §JDCksu
000964180313     c                   eval      JDCtadu = §JDCtadu
000965180313
000966180313      * calcolo ID job in base alla frequenza addebito
000967180313     c                   select
000968180313      * - se frequenza addebito per Immagine
000969180313     c                   when      JDCtadu = 'I'
000970180313      *   id = n.spedizione
000971180313     c                   eval      JDCidjob = JDCaas + JDClnp +
000972180313     c                                        JDCnrs + JDCnsp
000973180313      * - se frequenza addebito per Job
000974180313     c                   when      JDCtadu = 'J'
000975180313      *   id = ksu + oggi
000976180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
000977180313     c                                        %editc(dst2_ymd:'X')
000978180313      * - se frequenza addebito per Settimana
000979180313     c                   when      JDCtadu = 'S'
000980180313      *   ricerco il n. della settimana
000981180313     c     *iso          movel     dsT2_ymd      WdateISO
000982180313     c                   clear                   nrweek
000983180313     c                   call      'XSRWEEK'
000984180313     c                   parm                    wdateiso
000985180313     c                   parm                    nrweek
000986180313      *   imposto il campo LACADDMS
000987180313     c                   eval      wyear = %subdt(wdateiso:*years)
000988180313     c                   eval      waddams = %editc(wyear:'X') +
000989180313     c                                       %editc(nrweek:'X')
000990180313     c                   eval      JDCaddms = %dec(waddams:6:0)
000991180313      *   id = ksu + S + AAAA + n.settimana
000992180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
000993180313     c                                        'S' + waddams
000994180313      * - se frequenza addebito per Mese
000995180313     c                   when      JDCtadu = 'M'
000996180313      *   imposto il campo LACADDMS
000997180313     c                   eval      JDCaddms =
000998180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
000999180313      *   id = ksu + M + AAAA + mese
001000180313     c                   eval      JDCidjob = %editc(JDCksu:'X') +
001001180313     c                                        'M' + %editc(JDCaddms:'X')
001002180313     c                   endsl
001003180313
001004180313      * memorizzo anche i campi che per ora non uso
001005180313     c                   eval      JDCfimp = §JDCfimp
001006180313     c                   eval      JDCimp = §JDCimp
001007180313      * flag per creare titas, non creare o con varia negata
001008180313     c                   eval      JDCtad = §JDCtad
001009180313
001010180313      * cliente e tariffa tassazione
001011180313      * --> se forzati prendo da tabella
001012180313     c                   if        §JDCksc <> *zeros
001013180313     c                   eval      JDCidl = %editc(§JDCksc:'X') + §JDCctr
001014180313     c                   else
001015180313      * --> altrimenti dalla bolla
001016180313     c                   eval      JDCidl = %editc(TASccm:'X') +
001017180313     c                                      %editc(TASctr:'X')
001018180313     c                   endif
001019180313      *
001020180313      * Scrittura /aggiornamento
001021180313     C                   IF        Tijdc_U_W = 'U'
001022180313     c                   update    TIJDC000
001023180313     c                   ELSE
001024180313     c                   write     TIJDC000
001025180313     c                   ENDIF
001026180313
001027180313     c                   ENDSR
001028180313      *
001029180313      **-------------------------------------------------------------**
001030180313      ** Wrt_TILAC2 ** Scrivo TILAC per immagine da tabella LA2      **
001031180313      **-------------------------------------------------------------**
001032180313     c     Wrt_TILAC2    BEGSR
001033180313
001034180313     c                   eval      titasdsChk = titasds
001035180313     c*
001036180313     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
001037180313     c     K04tas30      chain     fnlbl01l
001038180313     c                   if        %found(fnlbl01l)
001039180313     c                   setoff                                       313233
001040180313     c     Ktas          chain(N)  titas30c
001041180313     c                   if        %found(titas30c)
001042180313     c                   eval      titasdschk = titasds
001043180313     c                   endif
001044180313     c* richaino la bolla figlia
001045180313     c                   setoff                                       313233
001046180313     c     ktas30_f      chain     titas30c
001047180313     c                   endif
001048180313     c
001049180313     c                   if        Chk_tastbl <> savtbl
001050180313     c                   movel(p)  Chk_tastbl    kkey
001051180313     c                   clear                   dstb
001052180313     c     ktab          chain     tabel00f
001053180313     c                   if        %found(tabel00f)
001054180313     c                   movel     tbluni        dstb
001055180313     c                   endif
001056180313     c                   eval      savtbl = Chk_tastbl
001057180313     c                   endif
001058180313
001059180313     c* controllo porto (rispetto alla bolla originale)
001060180313     c                   if        §LA2tpt <> 'E' and §tbtpo <> §LA2tpt
001061180313     c                   leavesr
001062180313     c                   endif
001063180313     c* reso (rispetto alla bolla corrente (figlia))
001064180313     c                   if        §LA2res <> 'S' and tasfbr = 'S'
001065180313     c                   leavesr
001066180313     c                   endif
001067180313     c* recupero (rispetto alla bolla originale)
001068180313     c                   if        §LA2rec <> 'S' and §tbrbl = 'R'
001069180313     c                   leavesr
001070180313     c                   endif
001071180313     c* c/servizio (rispetto alla bolla originale)
001072180313     c                   if        §LA2csr <> 'S' and §tbrbl = 'C'
001073180313     c                   leavesr
001074180313     c                   endif
001075180313     c* serie (rispetto alla bolla originale)
001076180313     c                   if        §LA2ssr = 'S' and chk_tasnrs = *zeros
001077180313     c                   leavesr
001078180313     c                   endif
001079180313     c* linea di partenza (rispetto alla bolla originale)
001080180313     c                   if        §LA2lnp <> *zeros and
001081180313     c                             chk_taslnp <> §LA2lnp
001082180313     c                   leavesr
001083180313     c                   endif
001084180313
001085180313     c                   eval      $cli = *on
001086180313      *
001087180313      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
001088180313      * nuova vista logica per Directory e solo '00'
001089180313     C                   eval      Tilac_U_W = 'W'
001090180313
001091180313      /free
001092180313       // -?Cerco con NSP + DIR solo ELA 00?
001093180313         kLACdir = §LA2dir;
001094180313         chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
001095180313         IF  %found(TILAC04L);
001096180313           Tilac_U_W = 'U';
001097180313         ENDIF;
001098180313      /end-free
001099180313
001100180313      *
001101180313      * Preparazione del record
001102180313     c                   clear                   TILAC000
001103180313     c                   move      DS_Time2      LACtim
001104180313     c                   move      TASaas        LACaas
001105180313     c                   move      TASlnp        LAClnp
001106180313     c                   move      TASnrs        LACnrs
001107180313     c                   move      TASnsp        LACnsp
001108180313     c                   movel     'CLI'         LACcop
001109180313     c                   movel     §LA2dir       LACdir
001110180313     c                   movel     §LA2tpi       LACtpi
001111180313     c* flag nome immagine:
001112180313     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
001113180313     c*           sa Laguna) --> quindi lascio = blanks
001114180313if  1c                   if        §LA2fmi <> 'S '
001115180313      /free
001116180313           clear  TRUL55ds;
001117180313           I55fmi = §LA2fmi;
001118180313           I55lnp = TASlnp;
001119180313           I55nrs = TASnrs;
001120180313           I55nsp = TASnsp;
001121180313           I55aas = TASaas;
001122180313           I55mgs = TASmgs;
001123180313           I55rmn = TASrmn;
001124180313           I55fld = 'L';
001125180313           trul55r ( TRUL55ds );
001126180313           if  O55err = *off;
001127180313             LACnim = O55niml;
001128180313           endif;
001129180313      /end-free
001130180313e   1c                   endif
001131180313
001132180313      * flag per creare titas, non creare o con varia negata
001133180313     c                   eval      lactad = §LA2tad
001134180313
001135180313      * cliente e tariffa tassazione
001136180313      * --> se forzati prendo da tabella
001137180313     c                   if        §LA2ksc <> *zeros
001138180313     c                   eval      lacidl = %editc(§LA2ksc:'X') +
001139180313     c                                      §LA2ctr
001140180313     c                   else
001141180313      * --> altrimenti dalla bolla
001142180313     c                   eval      lacidl = %editc(tasksc:'X') +
001143180313     c                                      %editc(tasctr:'X')
001144180313     c                   endif
001145180313     c                   move      *zeros        LACela
001146180313     c                   eval      lactela = 'A'
001147180313     c                   eval      lacksc = $clilac(xx)
001148180313     c                   eval      lacksu = §LA2ksu
001149180313     c                   eval      lactadu = §LA2tadu
001150180313     c                   eval      lacfimp = §LA2fimp
001151180313     c                   eval      lacimp = §LA2imp
001152180313      * calcolo ID job in base alla frequenza addebito
001153180313     c                   select
001154180313      * - se frequenza addebito per Immagine
001155180313     c                   when      lactadu = 'I'
001156180313      *   id = n.spedizione
001157180313     c                   eval      lacidjob = lacaas + laclnp +
001158180313     c                                        lacnrs + lacnsp
001159180313      * - se frequenza addebito per Job
001160180313     c                   when      lactadu = 'J'
001161180313      *   id = ksu + oggi
001162180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001163180313     c                                        %editc(dst2_ymd:'X')
001164180313      * - se frequenza addebito per Settimana
001165180313     c                   when      lactadu = 'S'
001166180313      *   ricerco il n. della settimana
001167180313     c     *iso          movel     dsT2_ymd      WdateISO
001168180313     c                   clear                   nrweek
001169180313     c                   call      'XSRWEEK'
001170180313     c                   parm                    wdateiso
001171180313     c                   parm                    nrweek
001172180313      *   imposto il campo LACADDMS
001173180313     c                   eval      wyear = %subdt(wdateiso:*years)
001174180313     c                   eval      waddams = %editc(wyear:'X') +
001175180313     c                                       %editc(nrweek:'X')
001176180313     c                   eval      lacaddms = %dec(waddams:6:0)
001177180313      *   id = ksu + S + AAAA + n.settimana
001178180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001179180313     c                                        'S' + waddams
001180180313      * - se frequenza addebito per Mese
001181180313     c                   when      lactadu = 'M'
001182180313      *   imposto il campo LACADDMS
001183180313     c                   eval      lacaddms =
001184180313     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
001185180313      *   id = ksu + M + AAAA + mese
001186180313     c                   eval      lacidjob = %editc(lacksu:'X') +
001187180313     c                                        'M' + %editc(lacaddms:'X')
001188180313     c                   endsl
001189180313      *
001190180313      * Scrittura /aggiornamento
001191180313     C                   IF        Tilac_U_W = 'U'
001192180313     c                   update    TILAC000
001193180313     c                   ELSE
001194180313     c                   write     TILAC000
001195180313     c                   ENDIF
001196180313      *
001197180313     c                   ENDSR
001198180313      *
001199180313      **-------------------------------------------------------------**
001200180313      ** Wrt_TILAD  ** Registrazione dati nel file TILAD00F          **
001201180313      **-------------------------------------------------------------**
001202180313     c     Wrt_TILAD     BEGSR
001203180313      *
001204180313     c                   clear                   TILAD000
001205180313     c                   eval      LADtim = LAGtim
001206180313     c                   eval      LADaas = LAGaas
001207180313     c                   eval      LADlnp = LAGlnp
001208180313     c                   eval      LADnrs = LAGnrs
001209180313     c                   eval      LADnsp = LAGnsp
001210180313     c                   eval      LADxx1 = LAGxx1
001211180313     c                   eval      LADcop = 'MOV'
001212180313     c                   eval      LADdir = 'ERRATE'
001213180313     c                   eval      LADela = *zeros
001214180313      *
001215180313     c                   WRITE     TILAD000                             99
001216180313      *
001217180313      * Cancellazione record dal file TILAG00R
001218180313     c                   DELETE    TILAG000
001219180313      *
001220180313     c                   ENDSR
001221180313      *
001222180313      **-------------------------------------------------------------**
001223180313      ** Upd_TITAS  ** Aggiornamento dati nel file TITAS30C          **
001224180313      **-------------------------------------------------------------**
001225180313     c     Upd_TITAS     BEGSR
001226180313      *
001227180313      * Possono esserci 2 records su TITAS30C
001228180313      *   (con "tipo record" diverso)
001229180313do  1c                   dou       %eof(TITAS30C)
001230180313      *
001231180313     c                   movel     TASflo        dTASflo
001232180313if  2c                   if        $DPD    = *off  and $CLI = *off
001233180313     c                   eval      §FLOiml = 'S'
001234180313x   2c                   else
001235180313     c                   eval      §FLOiml = 'I'
001236180313e   2c                   endif
001237180313     c                   movel     dTASflo       TASflo
001238180313      *
001239180313     c   31              UPDATE    TITAS000
001240180313     c   32              UPDATE    TITAS010
001241180313     c   33              UPDATE    TITASP00
001242180313      *
001243180313     c                   setoff                                       313233
001244180313     c     K04TAS30      reade     TITAS30C
001245180313      *
001246180313e   1c                   enddo
001247180313      *
001248180313      * Cancellazione record dal file TILAG00R
001249180313     c                   DELETE    TILAG000
001250180313      *
001251180313     c                   ENDSR
