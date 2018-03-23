000100040119      *===============================================================*
000200040119      * TNSB10R    * Aggiornamento TITAS per immagine LdV             *
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
001300061227      *
001400061227     fFNLBL01L  if   e           k disk
001500061227     fTITA430C  if   e           k disk
001600090213      *
001700110225     fFIILV01L  Uf   e           k disk
001800040331      *
001900040119     fTILAG00R  Uf   e           k disk
002000040119      *
002100040119     fTITAS30C  Uf   e           k disk
002200121023     fFIAR531C  if   e           k disk
002300160322
002400160322     fFIDPO01L  if   e           k disk
002500040331      *
002600041021     fTILAO01L  Uf a e           k disk
002700121212     fTILAC01L  Uf a e           k disk    rename(tilac000:tilac01)
002800121212     fTILAC04L  uf a e           k disk
002900121023     fTIJDC01L  uf a e           k disk    prefix(JDC:3)
003000061227     fTILAD00F  O    e             disk
003100040119
003200040119      *---------------------------------------------------------------*
003300061227
003400040331      *
003500040331      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040331      *
003700110228     d $poDPD          s                   like(ORGfil) dim(999) inz
003800110228     d $clilac         s              7  0 dim(999) inz
003900110228     d $tptlac         s                   like(§lactpt) dim(999) inz
004000110228     d $reslac         s                   like(§lacres) dim(999) inz
004100110228     d $reclac         s                   like(§lacrec) dim(999) inz
004200110228     d $csrlac         s                   like(§laccsr) dim(999) inz
004300110228     d $ssrlac         s                   like(§lacssr) dim(999) inz
004400110228     d $dirlac         s                   like(§lacdir) dim(999) inz
004500110228     d $tpilac         s                   like(§lactpi) dim(999) inz
004600110228     d $fmilac         s                   like(§lacfmi) dim(999) inz
004700110228     d $TADlac         s                   like(§LACtad) dim(999) inz
004800110228     d $tadulac        s                   like(§lactadu) dim(999) inz
004900110228     d $fimplac        s                   like(§lacfimp) dim(999) inz
005000110228     d $implac         s                   like(§lacimp) dim(999) inz
005100110228     d $ksclac         s                   like(§lacksc) dim(999) inz
005200110228     d $ctrlac         s                   like(§lacctr) dim(999) inz
005300110228     d $ksulac         s                   like(§lacksu) dim(999) inz
005400110228     d $lnplac         s                   like(§laclnp) dim(999) inz
005500121019     d $merge          s                   like(§LACmerge) dim(999) inz
005600121217     d $flgLA2         s                   like(§LACla2) dim(999) inz
005700050314      *
005800040119      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
005900040119      *
006000050330      * - Ds per controlli bolla
006100050330     d titasds       e ds                  extname(TITAS00F)
006200050330     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
006300050330
006400040331     d OG143         e ds                  inz
006500040119     d dTASflo       e ds                  inz
006600050314     d dlac          e ds                  inz
006700121212     d dLA2          e ds                  inz
006800050314     d dstb          e ds                  inz
006900160420     d dsTBdpd       e ds                  extname(dstb)
007000160420     d                                     inz prefix(dpd)
007100060606     d dsbl4i        e ds
007200160322     d dsbl4m        e ds
007300121023     d dJDC          e ds                  inz
007400121023     d dAR5doc       e ds                  inz
007500090212
007600090212       // - Parametri x TRUL55R
007700090212     d TRUL55ds      e ds                  inz
007800090212     d   I55tla      e                     inz('C')
007900090212
008000040331      *
008100040331     d DS_Time1        ds            14    inz
008200040331     d   dsT1_hms                     6  0 inz
008300040331     d   dsT1_dmy                     8  0 inz
008400040331     d DS_Time2        ds            14    inz
008500040331     d   dsT2_ymd                     8  0 inz
008600040331     d   dsT2_hms                     6  0 inz
008700040331      *
008800040331     d Status         sds           333
008900040331     d  SDSjob               244    253                                         Job name
009000040331      *
009100040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009200040331      *
009300040331      * - Flag booleani
009400040331     d $DPD            s              1    inz(*off)
009500050314     d $CLI            s              1    inz(*off)
009600121212     d $cliLA2         s               n   inz(*off)
009700121023     d $cliJDC         s              1n   inz(*off)
009800040331      * - Indici di schiera
009900110228     d xx              s              4  0 inz
010000040331      * - Campi di comodo
010100040331     d WtasDCM         s              8    inz(*zeros)
010200050330     d WtasRMN         s             15    inz(*zeros)
010300040520     d WtasPOm         s              3  0 inz
010400040331     d Wtime           s             14  0 inz
010500061227     d Wdata_00        s              8  0 inz
010600061228     d Wdata_15        s              8  0 inz
010700040331     d WdateISO        s               d   datfmt(*ISO) inz
010800121023     d kAR5trd         s                   like(AR5trd) inz('DOC')
010900041021     d klaocop         s                   like(laocop) inz('DPD')
011000050314     d klaccop         s                   like(laccop) inz('CLI')
011100121212     d kLACdir         s                   like(LACdir)
011200121023     d kJDCcop         s                   like(JDCcop)
011300041021     d tilao_u_w       s              1
011400050314     d tilac_u_w       s              1
011500121023     d tijdc_u_w       s              1
011600050314     d kcod            s                   like(tblcod) inz('TB')
011700050314     d kkey            s                   like(tblkey)
011800050314     d savtbl          s                   like(tastbl) inz('  ')
011900050330     d WTAS_orig       s              1
012000060606     d ktrc            s                   like(ta4trc)
012100090310     d nrweek          s              2  0 inz
012200090310     d wyear           s              4  0 inz
012300090310     d waddams         s              6    inz
012400090807     d chiudi          s              1
012500121023     d noscrivi        s              1n   inz(*off)
012600090212
012700090212       //--------------------------------------------------------------
012800090212       //?Definizione procedure usate.                                 ?
012900090212       //--------------------------------------------------------------
013000090212
013100090212       // - Determinazione nome dell'immagine per TILAC
013200090212     d trul55r         pr                  extpgm('TRUL55R')
013300090212     d   trul55ds...
013400090212     d                                     likeds(trul55ds)
013500090218
013600090218       //--------------------------------------------------------------
013700090218       //?Definizione key-list.                                        ?
013800090218       //--------------------------------------------------------------
013900090218
014000110225       // - File FIILV01L
014100110225     d k06fiilv01    e ds                  extname( FIILV01L : *key )
014200110208     d                                     prefix(k_)   inz
014300040119
014400040119      *---------------------------------------------------------------*
014500040119
014600040119      * Predisposizione indicatori per riconoscere i tipi record
014700040119      *   reperiti dal file TITAS30C
014800040119     iTITAS000      31
014900040119     iTITAS010      32
015000040119     iTITASP00      33
015100040119
015200040119      *---------------------------------------------------------------*
015300040119
015400040119      *
015500040119      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
015600040119      *
015700040119     c     K04TAS30      klist
015800040119     c                   kfld                    TASaas
015900040119     c                   kfld                    TASlnp
016000040119     c                   kfld                    TASnrs
016100040119     c                   kfld                    TASnsp
016200050330     c     Ktas30_f      klist
016300050330     c                   kfld                    lblaan
016400050330     c                   kfld                    lbllpn
016500050330     c                   kfld                    lblnrn
016600050330     c                   kfld                    lblnsn
016700121023
016800121023     c     kAR5          klist
016900121023     c                   kfld                    TASaas
017000121023     c                   kfld                    TASlnp
017100121023     c                   kfld                    TASnrs
017200121023     c                   kfld                    TASnsp
017300121023     c                   kfld                    kAR5trd
017400041021      *
017500041021     c     K05LAO        klist
017600041021     c                   kfld                    LAGaas
017700041021     c                   kfld                    LAGlnp
017800041021     c                   kfld                    LAGnrs
017900041021     c                   kfld                    LAGnsp
018000041021     c                   kfld                    klaocop
018100050314     c*
018200050314     c     K05LAC        klist
018300050314     c                   kfld                    LAGaas
018400050314     c                   kfld                    LAGlnp
018500050314     c                   kfld                    LAGnrs
018600050314     c                   kfld                    LAGnsp
018700050314     c                   kfld                    klaccop
018800121023     c     K05JDC        klist
018900121023     c                   kfld                    LAGaas
019000121023     c                   kfld                    LAGlnp
019100121023     c                   kfld                    LAGnrs
019200121023     c                   kfld                    LAGnsp
019300121023     c                   kfld                    kJDCcop
019400050330
019500050330     c     Ktas          klist
019600050330     c                   kfld                    LBLaao
019700050330     c                   kfld                    LBLlpo
019800050330     c                   kfld                    LBLnro
019900050330     c                   kfld                    LBLnso
020000050330      *
020100050330     c     Kta4          klist
020200050330     c                   kfld                    TASaas
020300050330     c                   kfld                    TASlnp
020400050330     c                   kfld                    TASnrs
020500050330     c                   kfld                    TASnsp
020600050330     c                   kfld                    ktrc
020700050314     c*
020800050314     c     Ktab          klist
020900050314     C                   KFLD                    CODUT
021000050314     C                   KFLD                    kcod
021100050314     c                   kfld                    kkey
021200050314     c                   z-add     1             codut             1 0
021300040331      *
021400040331      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
021500040331      *
021600160415      * 20    - No cliente in tabella con LAC FTP
021700040331      * 21    - P.O.partenza del network DPD
021800040331      * 31-33 - Tracciato reperito dal file TITAS30C (000/010/P00)
021900061227      * 99    - Errore in scrittura record nel file TILAD00F
022000040119
022100040119      *---------------------------------------------------------------*
022200090807
022300090807     c     *entry        plist
022400090807     c                   parm                    chiudi
022500040119
022600040331      * Operazioni Iniziali
022700040331     c                   exsr      OperazIniz
022800040331      *
022900040119      * Ciclo di lettura del file TILAG00R
023000040119     c                   read      TILAG000
023100040119      *
023200040119do  1c                   dow       NOT %eof(TILAG00R)
023300040119      *
023400040119      * - Elaborazione dei soli rec. "IM"
023500040119if  2c                   if        LAGapp = 'IM'
023600040119      *
023700040119      * - Aggancio record corrispondente nel file TITAS30C
023800040119     c                   movel     '2000'        TASaas
023900040119     c                   move      LAGaas        TASaas
024000040119     c                   move      LAGlnp        TASlnp
024100040119     c                   move      LAGnrs        TASnrs
024200040119     c                   move      LAGnsp        TASnsp
024300040128     c                   setoff                                       313233
024400040119     c     K04TAS30      chain     TITAS30C
024500040119      *
024600040119      * - Aggiornamento dati
024700040119if  3c                   if        %found(TITAS30C)
024800121218     c                   eval      noscrivi = *off
024900061227      *
025000050314     c* verifico se cliente presente in tabella "LAC"
025100160415     c                   eval      *in20   = *off
025200050314     c                   eval      *in21   = *off
025300050314     c                   eval      $cli    = *off
025400050314     c                   eval      $dpd    = *off
025500160322     c                   z-add     1             xx
025600160415     c     tasksc        lookup    $clilac(xx)                            20
025700160415if  4c                   if        *in20 = *on
025800121023       // cliente con richiesta di immagine con merge
025900121023       // se presente l'immagine del DOC scrivo tutti e 2 gli archivi
026000121023     c                   IF        $merge(xx) <> *blanks
026100121023     c                   exsr      ctr_AR5
026200121023     c                   ENDIF
026300121023     c                   IF        not noscrivi
026400050314     c                   exsr      Wrt_TILAC
026500121023     c                   IF        $merge(xx) <> *blanks
026600121023     c                   exsr      wrt_TIJDC
026700121023     c                   ENDIF
026800121023     c                   ENDIF
026900121217       // controllo se devo estrarre due volte la stessa immagine
027000121217     c                   eval      $clila2 = *off
027100121217     c                   IF        $flgLA2(xx) <> *blanks
027200121217     c                   exsr      ctr_LA2
027300121217     c                   IF        $cliLA2
027400121212     c                   exsr      Wrt_TILAC2
027500121212     c                   ENDIF
027600121217     c                   ENDIF
027700050314x   4c                   else
027800040331     c                   exsr      Wrt_TILAO
027900050314e   4c                   endif
028000090218      /free
028100110208             // -?Cancellazione rec. da FIILV00F (non errori)?
028200110208             k_ILVaas = 2000 + %int(LAGaas);
028300110208             k_ILVlnp = %int(LAGlnp);
028400110208             k_ILVnrs = %int(LAGnrs);
028500110208             k_ILVnsp = %int(LAGnsp);
028600110225             clear  k_ILVfgs;
028700110225             clear  k_ILVndc;
028800110225             setll  %kds(k06fiilv01 : 4)  FIILV000;
028900110225             if  %equal(FIILV01L);
029000110225               reade  %kds(k06fiilv01 : 4)  FIILV000;
029100110225               dow  NOT %eof(FIILV01L);
029200110613                 if  (ILVcol > '00'  and  ILVcol < '10')  OR
029300110613                     (ILVcol = '00'  and (ILVnocdst = 'S'
029400110622                                      or  ILVfirpda = 'S'
029500110622                                      or  ILVscan   = ' '));
029600110208                   DELETE  FIILV000;
029700090218                 endif;
029800110225                 reade  %kds(k06fiilv01 : 4)  FIILV000;
029900090218               enddo;
030000090218             endif;
030100090218      /end-free
030200040119     c                   exsr      Upd_TITAS
030300061227      *
030400061227x   3c                   else
030500061227      *
030600061227      * Sposto da TILAG00R in TILAD00F se sped. non trovata su TITAS
030700061228      *   e se record ("time") di almeno 15 gg. fa!
030800061227     c                   movel     LAGtim        wData_00
030900061228if  4c                   if        wData_00 <= wData_15
031000061227     c                   exsr      Wrt_TILAD
031100061227e   4c                   endif
031200061227      *
031300040119e   3c                   endif
031400040119      *
031500040119e   2c                   endif
031600040119      *
031700040119      * - Lettura del record successivo nel file TILAG00R
031800040119     c                   read      TILAG000
031900040119      *
032000040119e   1c                   enddo
032100090807
032200090807      * testa la chiusura del sottosistema
032300090807     c                   shtdn                                        98
032400090807     c   98              eval      chiudi = 'S'
032500040119      *
032600040119      * Fine
032700040119     c                   eval      *inLR = *on
032800090212      /free
032900090212           reset  TRUL55ds;
033000090212           trul55r ( TRUL55ds );
033100090212      /end-free
033200040331      *
033300040331      **-------------------------------------------------------------**
033400040331      ** OperazIniz ** Operazioni Iniziali                           **
033500040331      **-------------------------------------------------------------**
033600040331     c     OperazIniz    BEGSR
033700040331      *
033800040331      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
033900040331     c                   time                    Wtime
034000040331     c                   movel     Wtime         DS_Time1
034100040331     c                   eval      dsT2_hms   = dsT1_hms
034200040331     c     *eur          movel     dsT1_dmy      WdateISO
034300040331     c                   move      WdateISO      dsT2_ymd
034400061228     c                   subdur    15 : *days    WdateISO
034500061228     c                   move      WdateISO      wData_15
034600040331      *
034700040331      * Intabellamento dei p.o. appartenenti al network "DPD"
034800040331     c                   clear                   xx
034900040331     c                   clear                   $poDPD
035000040331     c     *loval        setll     AZORG
035100040331     c                   read      AZORG
035200040331do  1c                   dow       NOT %eof(AZORG01L)
035300040331     c                   movel     ORGde3        OG143
035400040331if  2c                   if        §OGntw     = 'DPD'
035500040331     c                   add       1             xx
035600040331     c                   eval      $poDPD(xx) = ORGfil
035700040331e   2c                   endif
035800040331     c                   read      AZORG
035900040331e   1c                   enddo
036000050314      * Intabellamento tabella "LAC" dei clienti che ricevono le immagini
036100050321     c* in automatico
036200050314     c                   clear                   xx
036300050316     c                   clear                   $clilac
036400050316     c                   clear                   $tptlac
036500050316     c                   clear                   $reslac
036600050316     c                   clear                   $reclac
036700050316     c                   clear                   $csrlac
036800050316     c                   clear                   $ssrlac
036900050316     c                   clear                   $dirlac
037000050316     c                   clear                   $tpilac
037100050316     c                   clear                   $fmilac
037200071210     c                   clear                   $TADlac
037300090309     c                   clear                   $tadulac
037400090309     c                   clear                   $fimplac
037500090309     c                   clear                   $implac
037600090309     c                   clear                   $ksclac
037700090309     c                   clear                   $ctrlac
037800090309     c                   clear                   $ksulac
037900090309     c                   clear                   $lnplac
038000121019     c                   clear                   $merge
038100050316     c     'LAC'         setll     tntbe01l
038200050314     c                   do        *hival
038300050316     c     'LAC'         reade     tntbe01l
038400050314     c                   if        %eof(tntbe01l)
038500050314     c                   leave
038600050314     c                   endif
038700050316     c                   if        tbeatb <> *blanks
038800050316     c                   iter
038900050316     c                   endif
039000050321     c                   movel     tbeuni        dlac
039100090309     c                   if        §lacaut = 'A'
039200050314     c                   add       1             xx
039300050314     c                   movel     tbeke1        $clilac(xx)
039400050314     c                   movel     §lactpt       $tptlac(xx)
039500050314     c                   movel     §lacres       $reslac(xx)
039600050314     c                   movel     §lacrec       $reclac(xx)
039700050314     c                   movel     §laccsr       $csrlac(xx)
039800050314     c                   movel     §lacssr       $ssrlac(xx)
039900050314     c                   movel     §lacdir       $dirlac(xx)
040000050315     c                   movel     §lactpi       $tpilac(xx)
040100050315     c                   movel     §lacfmi       $fmilac(xx)
040200071210     c                   movel     §LACtad       $TADlac(xx)
040300090309     c                   eval      $tadulac(xx) = §lactadu
040400090309     c                   eval      $fimplac(xx) = §lacfimp
040500090309     c                   eval      $implac(xx)  = §lacimp
040600090309     c                   eval      $ksclac(xx)  = §lacksc
040700090309     c                   eval      $ctrlac(xx)  = §lacctr
040800090309     c                   eval      $ksulac(xx)  = §lacksu
040900090309     c                   eval      $lnplac(xx)  = §laclnp
041000121019     c                   eval      $merge(xx)   = §lacmerge
041100121217     c                   eval      $flgla2(xx)  = §lacla2
041200050321     c                   endif
041300050314     c                   enddo
041400040331      *
041500040331     c                   ENDSR
041600121023      /free
041700121023       //-------------------------------------------------------------**
041800121023       // crt_AR5  -- Controllo se arrivato il DOC.
041900121023       //-------------------------------------------------------------**
042000121023        BEGSR ctr_AR5;
042100121023
042200121023          noscrivi = *off;
042300121023
042400121023       //?Se su FIAR5 trovo che il DOC è già presente scrivo LAC e JDC
042500121023          chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
042600121023          IF  not %found(FIAR531C);
042700121023            noscrivi = *on;
042800121023            leavesr;
042900121023          ENDIF;
043000121023          dAR5doc = AR5uni;
043100121023          IF  §AR5jfid = *blanks;
043200121023            noscrivi = *on;
043300121023            leavesr;
043400121023          ENDIF;
043500121023
043600121023       //?Cerco i dati del cliente sulla tabella JDC
043700121023          clear dJDC;
043800121023          $cliJDc = *off;
043900121023          TBEcod = 'JDC';
044000121023          TBEke1 = %editc($clilac(xx):'X');
044100121023          chain (TBEcod:TBEke1) TNTBE01L;
044200121023          IF  %found(TNTBE01L) and TBEatb = *blanks;
044300121023            $cliJDc = *on;
044400121023            dJDC = TBEuni;
044500121023            kJDCcop = §JDCpag;
044600121023          ENDIF;
044700121023
044800121023        ENDSR;
044900121212
045000121212       //-------------------------------------------------------------**
045100121212       // crt_LA2  -- Controllo se estrazione dell'immagine da sola
045200121212       //-------------------------------------------------------------**
045300121212        BEGSR ctr_LA2;
045400121212
045500121212       //?Controllo se esiste il cliente sulla tabella LA2
045600121212          clear dLA2;
045700121212          $cliLA2 = *off;
045800121212          TBEcod = 'LA2';
045900121212          TBEke1 = %editc($clilac(xx):'X');
046000121212          chain (TBEcod:TBEke1) TNTBE01L;
046100121212          IF  %found(TNTBE01L) and TBEatb = *blanks;
046200121212            $cliLA2 = *on;
046300121212            dLA2 = TBEuni;
046400121212          ENDIF;
046500121212
046600121212        ENDSR;
046700121023      /end-free
046800040331      *
046900040331      **-------------------------------------------------------------**
047000040331      ** Wrt_TILAO  ** Registrazione dati nel file TILAO00F x DPD    **
047100040331      **-------------------------------------------------------------**
047200040331     c     Wrt_TILAO     BEGSR
047300040331      *
047400040520      * Verifica se p.o. del mittente DPD
047500040331     c                   eval      *in21   = *off
047600040520     c                   movel     TASccm        WtasPOm
047700040520     c     WtasPOm       lookup    $poDPD                                 21
047800160322      * se filiale mittente no DPD provo con la filiale del KSC
047900160322     c                   IF        not *in21
048000160322     c                   movel     TASksc        WtasPOm
048100160322     c     WtasPOm       lookup    $poDPD                                 21
048200160322     c                   ENDIF
048300040331     c                   eval      $DPD    = *in21
048400040331      *
048500040331     c     $DPD          cabeq     *off          NOwrtTILAO
048600040331      *
048700040331      * Impostazione dei campi numerici in campi alfanumerici di comodo
048800040331     c                   movel     TASdcm        WtasDCM
048900040405     c                   move      TASrmn        Wtasrmn
049000040331      *
049100041021      * Se su TILAO esiste già un record non elaborato aggiorno else scrivo
049200041021     C                   eval      Tilao_U_W = 'W'
049300041021      *
049400041021     C     k05lao        setll     Tilao01L
049500041021     C     k05lao        reade     Tilao01L
049600041021     C                   DOW       NOT %EOF(TILAO01L)
049700041021     C                   IF        LAGela = '00'
049800041021     C                   eval      Tilao_U_W = 'U'
049900041021     c                   leave
050000041021     C                   ELSE
050100041021     C     k05lao        reade     Tilao01L
050200041021     c                   ENDIF
050300041021     c                   ENDDO
050400041021      *
050500040331      * Preparazione del record
050600040331     c                   clear                   TILAO000
050700040331     c                   move      DS_Time2      LAOtim
050800040331     c                   move      TASaas        LAOaas
050900040331     c                   move      TASlnp        LAOlnp
051000040331     c                   move      TASnrs        LAOnrs
051100040331     c                   move      TASnsp        LAOnsp
051200040331     c                   movel     'DPD'         LAOcop
051300060606     c                   clear                   dsbl4i
051400060606     c                   clear                   w014a            14
051500060606     c                   movel     'I'           ktrc
051600060606     c     kta4          chain     tita430c
051700060606     c                   if        %found(tita430c)
051800060606     c                   movel     ta4not        dsbl4i
051900060606     c                   endif
052000160322      /free
052100160322       //?se non ho trovato il numero parcel DPD ?
052200160322       //?provo a cercarlo con il n. ORM su FIDPO?
052300160322         IF  §B4ipn = *blanks;
052400160322           exsr CercaParcel;
052500160322         ENDIF;
052600160322      /end-free
052700060606     c* se non trovato numero parcel su tita4 t.rk "I" lo prendo da rmn
052800060606     c                   if        §b4ipn = *blanks
052900060606     c* numero parcel lungo 11 lo trucco con 0 davanti al depot e 99 davanti
053000060606     c* al progressivo
053100100129     c                   if        %subst(wtasrmn:1:3) = '000'
053200060606     c                   eval      w014a = '0'
053300060606     c                                   + %subst(wtasrmn:4:3)
053400060606     c                                   + '99'
053500060606     c                                   + %subst(wtasrmn:7:8)
053600060606     c                   else
053700100129     c                   move      wtasrmn       w014a
053800060606     c                   endif
053900060606     c                   else
054000060606     c                   if        %subst(§b4ipn:12:1) = ' '
054100060606     c                   eval      w014a = '0'
054200060606     c                                   + %subst(§b4ipn:1:3)
054300060606     c                                   + '99'
054400060606     c                                   + %subst(§b4ipn:4:8)
054500060606     c                   else
054600060606     c                   eval      w014a = §b4ipn
054700060606     c                   endif
054800060606     c                   endif
054900060606     c                   eval      laoope  = w014a
055000060606     c                                     + '_'
055100060606     c                                     + wtasdcm
055200040331     c                   movel     SDSjob        LAGidl
055300040331     c                   move      *zeros        LAGela
055400040331      *
055500041021      * Scrittura /aggiornamento
055600041021     C                   IF        Tilao_U_W = 'U'
055700041021     c                   update    TILAO000
055800041021     c                   ELSE
055900041021     c                   write     TILAO000
056000041021     c                   ENDIF
056100040331      *
056200040331     c     NOwrtTILAO    ENDSR
056300160322
056400160322      /free
056500160322       //-------------------------------------------------------------**
056600160322       // Cerco il n. parcel DPD dall'ORM.
056700160322       //-------------------------------------------------------------**
056800160322        BEGSR CercaParcel;
056900160322
057000160322       //?lo faccio solo se bolla in assegnato
057100170606       //?NO da giugno 2017 ND ha detto di farlo anche per le bolle Franco
057200160420         clear dsTBdpd;
057300160322         kkey = TAStbl;
057400160322         chain (codut:kcod:kkey) TABEL00F;
057500160322         IF  %found(TABEL00F);
057600160420           dsTBdpd = TBLuni;
057700160322         ENDIF;
057800160420         IF  dpd§TBtpo <> 'A';
057900170606         //  leavesr;
058000160322         ENDIF;
058100160322
058200160322       //?per prima cosa cerco il n. ORM sul rcd 'M' del TITA4
058300160322          clear dsBL4M;
058400160322          ktrc = 'M';
058500160322          chain (TASaas:TASlnp:TASnrs:TASnsp:ktrc) TITA430C;
058600160322          IF  not %found(TITA430C);
058700160322            leavesr;
058800160322          ENDIF;
058900160322          dsBL4M = TA4not;
059000160322
059100160322       //?con il n. ORM leggo FIDPO per numero ORM e
059200160322       //?recupero il n. parcel dal primo record che trovo
059300160322          clear §B4ipn;
059400160322          setll (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
059500160322          reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
059600160322          DOW  not %eof(FIDPO01L);
059700160322            IF  DPOnrp <> *blanks;
059800160322              §B4ipn = DPOnrp;
059900160322              leave;
060000160322            ENDIF;
060100160322            reade (§B4poe:§B4nsr:§B4nor:§B4nrv) FIDPO01L;
060200160322          ENDDO;
060300160322
060400160322        ENDSR;
060500160322      /end-free
060600040119      *
060700050314      **-------------------------------------------------------------**
060800050314      ** Wrt_TILAC  ** Registrazione dati nel file TILAC00F x cliente**
060900050314      **-------------------------------------------------------------**
061000050314     c     Wrt_TILAC     BEGSR
061100050330
061200050330     c                   eval      titasdsChk = titasds
061300050330     c*
061400050330     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
061500050330     c     K04tas30      chain     fnlbl01l
061600050330     c                   if        %found(fnlbl01l)
061700050421     c                   setoff                                       313233
061800050330     c     Ktas          chain(N)  titas30c
061900050330     c                   if        %found(titas30c)
062000050330     c                   eval      titasdschk = titasds
062100050330     c                   endif
062200050330     c* richaino la bolla figlia
062300050421     c                   setoff                                       313233
062400050330     c     ktas30_f      chain     titas30c
062500050330     c                   endif
062600050314     c
062700050330     c                   if        Chk_tastbl <> savtbl
062800050330     c                   movel(p)  Chk_tastbl    kkey
062900050314     c                   clear                   dstb
063000050314     c     ktab          chain     tabel00f
063100050314     c                   if        %found(tabel00f)
063200050314     c                   movel     tbluni        dstb
063300050314     c                   endif
063400050330     c                   eval      savtbl = Chk_tastbl
063500050314     c                   endif
063600050330
063700050330     c* controllo porto (rispetto alla bolla originale)
063800050314     c                   if        $tptlac(XX) <> 'E' and §tbtpo <> $tptlac(xx)
063900050314     c                   goto      NOwrtTILAC
064000050314     c                   endif
064100050330     c* reso (rispetto alla bolla corrente (figlia))
064200050314     c                   if        $reslac(xx) <> 'S' and tasfbr = 'S'
064300050314     c                   goto      NOwrtTILAC
064400050314     c                   endif
064500050330     c* recupero (rispetto alla bolla originale)
064600050314     c                   if        $reclac(xx) <> 'S' and §tbrbl = 'R'
064700050314     c                   goto      NOwrtTILAC
064800050314     c                   endif
064900050330     c* c/servizio (rispetto alla bolla originale)
065000090302     c                   if        $csrlac(xx) <> 'S' and §tbrbl = 'C'
065100050314     c                   goto      NOwrtTILAC
065200050314     c                   endif
065300050330     c* serie (rispetto alla bolla originale)
065400050330     c                   if        $ssrlac(xx) = 'S' and chk_tasnrs = *zeros
065500050317     c                   goto      NOwrtTILAC
065600050314     c                   endif
065700090309     c* linea di partenza (rispetto alla bolla originale)
065800090309     c                   if        $lnplac(xx) <> *zeros and
065900090309     c                             chk_taslnp <> $lnplac(xx)
066000090309     c                   goto      NOwrtTILAC
066100090309     c                   endif
066200090310
066300050314     c                   eval      $cli = *on
066400050314      *
066500050314      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
066600121212      * nuova vista logica per Directory e solo '00'
066700050314     C                   eval      Tilac_U_W = 'W'
066800121212
066900121212      /free
067000121212       // -?Cerco con NSP + DIR solo ELA 00?
067100121212         kLACdir = $dirLAC(xx);
067200121212         chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
067300121212         IF  %found(TILAC04L);
067400121212           Tilac_U_W = 'U';
067500121212         ENDIF;
067600121212      /end-free
067700121212
067800050314      *
067900050314      * Preparazione del record
068000050314     c                   clear                   TILAC000
068100050314     c                   move      DS_Time2      LACtim
068200050314     c                   move      TASaas        LACaas
068300050314     c                   move      TASlnp        LAClnp
068400050314     c                   move      TASnrs        LACnrs
068500050314     c                   move      TASnsp        LACnsp
068600050314     c                   movel     'CLI'         LACcop
068700050314     c                   movel     $dirlac(xx)   LACdir
068800050315     c                   movel     $tpilac(xx)   LACtpi
068900050330     c* flag nome immagine:
069000050330     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
069100050330     c*           sa Laguna) --> quindi lascio = blanks
069200080314if  1c                   if        $fmilac(xx)  <> 'S '
069300090212      /free
069400090212           clear  TRUL55ds;
069500090212           I55fmi = $FMIlac(xx);
069600090212           I55lnp = TASlnp;
069700090212           I55nrs = TASnrs;
069800090212           I55nsp = TASnsp;
069900090212           I55aas = TASaas;
070000090212           I55mgs = TASmgs;
070100090212           I55rmn = TASrmn;
070200130411           I55fld = 'L';
070300090212           trul55r ( TRUL55ds );
070400090212           if  O55err = *off;
070500130411             LACnim = O55niml;
070600090212           endif;
070700090212      /end-free
070800080314e   1c                   endif
070900060419
071000090309      * flag per creare titas, non creare o con varia negata
071100090309     c                   eval      lactad = $tadlac(xx)
071200050330
071300090313      * cliente e tariffa tassazione
071400090313      * --> se forzati prendo da tabella
071500090309     c                   if        $ksclac(xx) <> *zeros
071600090310     c                   eval      lacidl = %editc($ksclac(xx):'X') +
071700090310     c                                      $ctrlac(xx)
071800090313     c                   else
071900090313      * --> altrimenti dalla bolla
072000090313     c                   eval      lacidl = %editc(tasksc:'X') +
072100090313     c                                      %editc(tasctr:'X')
072200090309     c                   endif
072300050314     c                   move      *zeros        LACela
072400090309     c                   eval      lactela = 'A'
072500090323     c                   eval      lacksc = $clilac(xx)
072600090309     c                   eval      lacksu = $ksulac(xx)
072700090309     c                   eval      lactadu = $tadulac(xx)
072800090309     c                   eval      lacfimp = $fimplac(xx)
072900090309     c                   eval      lacimp = $implac(xx)
073000090310      * calcolo ID job in base alla frequenza addebito
073100090310     c                   select
073200090310      * - se frequenza addebito per Immagine
073300090310     c                   when      lactadu = 'I'
073400090310      *   id = n.spedizione
073500090310     c                   eval      lacidjob = lacaas + laclnp +
073600090310     c                                        lacnrs + lacnsp
073700090310      * - se frequenza addebito per Job
073800090310     c                   when      lactadu = 'J'
073900090310      *   id = ksu + oggi
074000090310     c                   eval      lacidjob = %editc(lacksu:'X') +
074100090310     c                                        %editc(dst2_ymd:'X')
074200090310      * - se frequenza addebito per Settimana
074300090310     c                   when      lactadu = 'S'
074400090310      *   ricerco il n. della settimana
074500090310     c     *iso          movel     dsT2_ymd      WdateISO
074600090310     c                   clear                   nrweek
074700090310     c                   call      'XSRWEEK'
074800090310     c                   parm                    wdateiso
074900090310     c                   parm                    nrweek
075000090310      *   imposto il campo LACADDMS
075100090310     c                   eval      wyear = %subdt(wdateiso:*years)
075200090310     c                   eval      waddams = %editc(wyear:'X') +
075300090310     c                                       %editc(nrweek:'X')
075400090310     c                   eval      lacaddms = %dec(waddams:6:0)
075500090310      *   id = ksu + S + AAAA + n.settimana
075600090310     c                   eval      lacidjob = %editc(lacksu:'X') +
075700090310     c                                        'S' + waddams
075800090310      * - se frequenza addebito per Mese
075900090310     c                   when      lactadu = 'M'
076000090310      *   imposto il campo LACADDMS
076100090310     c                   eval      lacaddms =
076200090310     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
076300090310      *   id = ksu + M + AAAA + mese
076400090310     c                   eval      lacidjob = %editc(lacksu:'X') +
076500090310     c                                        'M' + %editc(lacaddms:'X')
076600090310     c                   endsl
076700050314      *
076800050314      * Scrittura /aggiornamento
076900050314     C                   IF        Tilac_U_W = 'U'
077000050314     c                   update    TILAC000
077100050314     c                   ELSE
077200050314     c                   write     TILAC000
077300050314     c                   ENDIF
077400050314      *
077500050330     c     NOwrtTILAC    ENDSR
077600121023
077700121023      **-------------------------------------------------------------**
077800121023      ** Wrt_TIJDC  ** Registrazione dati nel file TIJDC00F x cliente**
077900121023      **-------------------------------------------------------------**
078000121023     c     Wrt_TIJDC     BEGSR
078100121023
078200121023      * se non c'è la tabella del cliente imposto fisso 'CL1' in COP
078300121023     c                   IF        not $cliJDC
078400121023     c                   eval      kJDCcop = 'CL1'
078500121023     c                   ENDIF
078600121023
078700121023      * Se su TIJDC esiste già un record non elaborato aggiorno else scrivo
078800121023     C                   eval      Tijdc_U_W = 'W'
078900121023      *
079000121023     C     k05jdc        setll     Tijdc01L
079100121023     C     k05jdc        reade     Tijdc01L
079200121023     C                   DOW       NOT %EOF(TIJDC01L)
079300121023     C                   IF        JDCela = '00'
079400121023     C                   eval      Tijdc_U_W = 'U'
079500121023     c                   leave
079600121023     C                   ELSE
079700121023     C     k05jdc        reade     Tijdc01L
079800121023     c                   ENDIF
079900121023     c                   ENDDO
080000121023
080100121023      * Preparazione del record
080200121023     c                   clear                   TIJDC000
080300121023     c                   eval      JDCtim = LACtim
080400121023     c                   eval      JDCaas = LACaas
080500121023     c                   eval      JDClnp = LAClnp
080600121023     c                   eval      JDCnrs = LACnrs
080700121023     c                   eval      JDCnsp = LACnsp
080800121023     c                   eval      JDCcop = kJDCcop
080900121023     c                   eval      JDCdir = §JDCdir
081000121023     c                   eval      JDCtpi = §JDCtpi
081100121023if  1c                   eval      JDCnim = LACnim
081200121023     c                   eval      JDCela = '00'
081300121023     c                   eval      JDCtela = 'A'
081400121023     c                   eval      JDCksc = LACksc
081500121023     c                   eval      JDCksu = §JDCksu
081600121023     c                   eval      JDCtadu = §JDCtadu
081700121023
081800121023      * calcolo ID job in base alla frequenza addebito
081900121023     c                   select
082000121023      * - se frequenza addebito per Immagine
082100121023     c                   when      JDCtadu = 'I'
082200121023      *   id = n.spedizione
082300121023     c                   eval      JDCidjob = JDCaas + JDClnp +
082400121023     c                                        JDCnrs + JDCnsp
082500121023      * - se frequenza addebito per Job
082600121023     c                   when      JDCtadu = 'J'
082700121023      *   id = ksu + oggi
082800121023     c                   eval      JDCidjob = %editc(JDCksu:'X') +
082900121023     c                                        %editc(dst2_ymd:'X')
083000121023      * - se frequenza addebito per Settimana
083100121023     c                   when      JDCtadu = 'S'
083200121023      *   ricerco il n. della settimana
083300121023     c     *iso          movel     dsT2_ymd      WdateISO
083400121023     c                   clear                   nrweek
083500121023     c                   call      'XSRWEEK'
083600121023     c                   parm                    wdateiso
083700121023     c                   parm                    nrweek
083800121023      *   imposto il campo LACADDMS
083900121023     c                   eval      wyear = %subdt(wdateiso:*years)
084000121023     c                   eval      waddams = %editc(wyear:'X') +
084100121023     c                                       %editc(nrweek:'X')
084200121023     c                   eval      JDCaddms = %dec(waddams:6:0)
084300121023      *   id = ksu + S + AAAA + n.settimana
084400121023     c                   eval      JDCidjob = %editc(JDCksu:'X') +
084500121023     c                                        'S' + waddams
084600121023      * - se frequenza addebito per Mese
084700121023     c                   when      JDCtadu = 'M'
084800121023      *   imposto il campo LACADDMS
084900121023     c                   eval      JDCaddms =
085000121023     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
085100121023      *   id = ksu + M + AAAA + mese
085200121023     c                   eval      JDCidjob = %editc(JDCksu:'X') +
085300121023     c                                        'M' + %editc(JDCaddms:'X')
085400121023     c                   endsl
085500121023
085600121023      * memorizzo anche i campi che per ora non uso
085700121023     c                   eval      JDCfimp = §JDCfimp
085800121023     c                   eval      JDCimp = §JDCimp
085900121023      * flag per creare titas, non creare o con varia negata
086000121023     c                   eval      JDCtad = §JDCtad
086100121023
086200121023      * cliente e tariffa tassazione
086300121023      * --> se forzati prendo da tabella
086400121023     c                   if        §JDCksc <> *zeros
086500121023     c                   eval      JDCidl = %editc(§JDCksc:'X') + §JDCctr
086600121023     c                   else
086700121023      * --> altrimenti dalla bolla
086800121023     c                   eval      JDCidl = %editc(TASccm:'X') +
086900121023     c                                      %editc(TASctr:'X')
087000121023     c                   endif
087100121023      *
087200121023      * Scrittura /aggiornamento
087300121023     C                   IF        Tijdc_U_W = 'U'
087400121023     c                   update    TIJDC000
087500121023     c                   ELSE
087600121023     c                   write     TIJDC000
087700121023     c                   ENDIF
087800121023
087900121023     c                   ENDSR
088000121212      *
088100121212      **-------------------------------------------------------------**
088200121212      ** Wrt_TILAC2 ** Scrivo TILAC per immagine da tabella LA2      **
088300121212      **-------------------------------------------------------------**
088400121212     c     Wrt_TILAC2    BEGSR
088500121212
088600121212     c                   eval      titasdsChk = titasds
088700121212     c*
088800121212     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
088900121212     c     K04tas30      chain     fnlbl01l
089000121212     c                   if        %found(fnlbl01l)
089100121212     c                   setoff                                       313233
089200121212     c     Ktas          chain(N)  titas30c
089300121212     c                   if        %found(titas30c)
089400121212     c                   eval      titasdschk = titasds
089500121212     c                   endif
089600121212     c* richaino la bolla figlia
089700121212     c                   setoff                                       313233
089800121212     c     ktas30_f      chain     titas30c
089900121212     c                   endif
090000121212     c
090100121212     c                   if        Chk_tastbl <> savtbl
090200121212     c                   movel(p)  Chk_tastbl    kkey
090300121212     c                   clear                   dstb
090400121212     c     ktab          chain     tabel00f
090500121212     c                   if        %found(tabel00f)
090600121212     c                   movel     tbluni        dstb
090700121212     c                   endif
090800121212     c                   eval      savtbl = Chk_tastbl
090900121212     c                   endif
091000121212
091100121212     c* controllo porto (rispetto alla bolla originale)
091200121212     c                   if        §LA2tpt <> 'E' and §tbtpo <> §LA2tpt
091300121212     c                   leavesr
091400121212     c                   endif
091500121212     c* reso (rispetto alla bolla corrente (figlia))
091600121212     c                   if        §LA2res <> 'S' and tasfbr = 'S'
091700121212     c                   leavesr
091800121212     c                   endif
091900121212     c* recupero (rispetto alla bolla originale)
092000121212     c                   if        §LA2rec <> 'S' and §tbrbl = 'R'
092100121212     c                   leavesr
092200121212     c                   endif
092300121212     c* c/servizio (rispetto alla bolla originale)
092400121212     c                   if        §LA2csr <> 'S' and §tbrbl = 'C'
092500121212     c                   leavesr
092600121212     c                   endif
092700121212     c* serie (rispetto alla bolla originale)
092800121212     c                   if        §LA2ssr = 'S' and chk_tasnrs = *zeros
092900121212     c                   leavesr
093000121212     c                   endif
093100121212     c* linea di partenza (rispetto alla bolla originale)
093200121212     c                   if        §LA2lnp <> *zeros and
093300121212     c                             chk_taslnp <> §LA2lnp
093400121212     c                   leavesr
093500121212     c                   endif
093600121220
093700121220     c                   eval      $cli = *on
093800121212      *
093900121212      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
094000121212      * nuova vista logica per Directory e solo '00'
094100121212     C                   eval      Tilac_U_W = 'W'
094200121212
094300121212      /free
094400121212       // -?Cerco con NSP + DIR solo ELA 00?
094500121212         kLACdir = §LA2dir;
094600121212         chain (LAGaas:LAGlnp:LAGnrs:LAGnsp:kLACcop:kLACdir) TILAC04L;
094700121212         IF  %found(TILAC04L);
094800121212           Tilac_U_W = 'U';
094900121212         ENDIF;
095000121212      /end-free
095100121212
095200121212      *
095300121212      * Preparazione del record
095400121212     c                   clear                   TILAC000
095500121212     c                   move      DS_Time2      LACtim
095600121212     c                   move      TASaas        LACaas
095700121212     c                   move      TASlnp        LAClnp
095800121212     c                   move      TASnrs        LACnrs
095900121212     c                   move      TASnsp        LACnsp
096000121212     c                   movel     'CLI'         LACcop
096100121212     c                   movel     §LA2dir       LACdir
096200121212     c                   movel     §LA2tpi       LACtpi
096300121212     c* flag nome immagine:
096400121212     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
096500121212     c*           sa Laguna) --> quindi lascio = blanks
096600121212if  1c                   if        §LA2fmi <> 'S '
096700121212      /free
096800121212           clear  TRUL55ds;
096900121212           I55fmi = §LA2fmi;
097000121212           I55lnp = TASlnp;
097100121212           I55nrs = TASnrs;
097200121212           I55nsp = TASnsp;
097300121212           I55aas = TASaas;
097400121212           I55mgs = TASmgs;
097500121212           I55rmn = TASrmn;
097600130411           I55fld = 'L';
097700121212           trul55r ( TRUL55ds );
097800121212           if  O55err = *off;
097900130411             LACnim = O55niml;
098000121212           endif;
098100121212      /end-free
098200121212e   1c                   endif
098300121212
098400121212      * flag per creare titas, non creare o con varia negata
098500121212     c                   eval      lactad = §LA2tad
098600121212
098700121212      * cliente e tariffa tassazione
098800121212      * --> se forzati prendo da tabella
098900121212     c                   if        §LA2ksc <> *zeros
099000121212     c                   eval      lacidl = %editc(§LA2ksc:'X') +
099100121212     c                                      §LA2ctr
099200121212     c                   else
099300121212      * --> altrimenti dalla bolla
099400121212     c                   eval      lacidl = %editc(tasksc:'X') +
099500121212     c                                      %editc(tasctr:'X')
099600121212     c                   endif
099700121212     c                   move      *zeros        LACela
099800121212     c                   eval      lactela = 'A'
099900121212     c                   eval      lacksc = $clilac(xx)
100000121212     c                   eval      lacksu = §LA2ksu
100100121212     c                   eval      lactadu = §LA2tadu
100200121212     c                   eval      lacfimp = §LA2fimp
100300121212     c                   eval      lacimp = §LA2imp
100400121212      * calcolo ID job in base alla frequenza addebito
100500121212     c                   select
100600121212      * - se frequenza addebito per Immagine
100700121212     c                   when      lactadu = 'I'
100800121212      *   id = n.spedizione
100900121212     c                   eval      lacidjob = lacaas + laclnp +
101000121212     c                                        lacnrs + lacnsp
101100121212      * - se frequenza addebito per Job
101200121212     c                   when      lactadu = 'J'
101300121212      *   id = ksu + oggi
101400121212     c                   eval      lacidjob = %editc(lacksu:'X') +
101500121212     c                                        %editc(dst2_ymd:'X')
101600121212      * - se frequenza addebito per Settimana
101700121212     c                   when      lactadu = 'S'
101800121212      *   ricerco il n. della settimana
101900121212     c     *iso          movel     dsT2_ymd      WdateISO
102000121212     c                   clear                   nrweek
102100121212     c                   call      'XSRWEEK'
102200121212     c                   parm                    wdateiso
102300121212     c                   parm                    nrweek
102400121212      *   imposto il campo LACADDMS
102500121212     c                   eval      wyear = %subdt(wdateiso:*years)
102600121212     c                   eval      waddams = %editc(wyear:'X') +
102700121212     c                                       %editc(nrweek:'X')
102800121212     c                   eval      lacaddms = %dec(waddams:6:0)
102900121212      *   id = ksu + S + AAAA + n.settimana
103000121212     c                   eval      lacidjob = %editc(lacksu:'X') +
103100121212     c                                        'S' + waddams
103200121212      * - se frequenza addebito per Mese
103300121212     c                   when      lactadu = 'M'
103400121212      *   imposto il campo LACADDMS
103500121212     c                   eval      lacaddms =
103600121212     c                             %dec(%subst(%editc(dsT2_ymd:'X'):1:6):6:0)
103700121212      *   id = ksu + M + AAAA + mese
103800121212     c                   eval      lacidjob = %editc(lacksu:'X') +
103900121212     c                                        'M' + %editc(lacaddms:'X')
104000121212     c                   endsl
104100121212      *
104200121212      * Scrittura /aggiornamento
104300121212     C                   IF        Tilac_U_W = 'U'
104400121212     c                   update    TILAC000
104500121212     c                   ELSE
104600121212     c                   write     TILAC000
104700121212     c                   ENDIF
104800121212      *
104900121212     c                   ENDSR
105000061227      *
105100061227      **-------------------------------------------------------------**
105200061227      ** Wrt_TILAD  ** Registrazione dati nel file TILAD00F          **
105300061227      **-------------------------------------------------------------**
105400061227     c     Wrt_TILAD     BEGSR
105500061227      *
105600061227     c                   clear                   TILAD000
105700061227     c                   eval      LADtim = LAGtim
105800061227     c                   eval      LADaas = LAGaas
105900061227     c                   eval      LADlnp = LAGlnp
106000061227     c                   eval      LADnrs = LAGnrs
106100061227     c                   eval      LADnsp = LAGnsp
106200061227     c                   eval      LADxx1 = LAGxx1
106300061227     c                   eval      LADcop = 'MOV'
106400061227     c                   eval      LADdir = 'ERRATE'
106500061227     c                   eval      LADela = *zeros
106600061227      *
106700061227     c                   WRITE     TILAD000                             99
106800061227      *
106900061227      * Cancellazione record dal file TILAG00R
107000061227     c                   DELETE    TILAG000
107100061227      *
107200061227     c                   ENDSR
107300050314      *
107400040119      **-------------------------------------------------------------**
107500040119      ** Upd_TITAS  ** Aggiornamento dati nel file TITAS30C          **
107600040119      **-------------------------------------------------------------**
107700040119     c     Upd_TITAS     BEGSR
107800040119      *
107900040119      * Possono esserci 2 records su TITAS30C
108000040119      *   (con "tipo record" diverso)
108100040119do  1c                   dou       %eof(TITAS30C)
108200040119      *
108300040119     c                   movel     TASflo        dTASflo
108400050314if  2c                   if        $DPD    = *off  and $CLI = *off
108500040119     c                   eval      §FLOiml = 'S'
108600040331x   2c                   else
108700040331     c                   eval      §FLOiml = 'I'
108800040331e   2c                   endif
108900040119     c                   movel     dTASflo       TASflo
109000040119      *
109100040119     c   31              UPDATE    TITAS000
109200040119     c   32              UPDATE    TITAS010
109300040119     c   33              UPDATE    TITASP00
109400040119      *
109500040128     c                   setoff                                       313233
109600040119     c     K04TAS30      reade     TITAS30C
109700040119      *
109800040119e   1c                   enddo
109900040119      *
110000040119      * Cancellazione record dal file TILAG00R
110100040119     c                   DELETE    TILAG000
110200040119      *
110300040119     c                   ENDSR
