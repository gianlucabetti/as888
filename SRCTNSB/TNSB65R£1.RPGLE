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
001100040331      *
001200040119     fTILAG00R  Uf   e           k disk
001300040119      *
001400040119     fTITAS30C  Uf   e           k disk
001500040331      *
001600041021     fTILAO01L  Uf a e           k disk
001700050314      *
001800050314     fTILAC01L  Uf a e           k disk
001900050314      *
002000050314     fTNTBE01l  if   e           k disk
002100050317      *
002200050314     fTABEL00F  if   e           k disk
002300050317      *
002400050317     fFNLBL01L  if   e           k disk
002500050330      *
002600050330     fTITA430C  If   e           k disk
002700040119
002800040119      *---------------------------------------------------------------*
002900040331      *
003000040331      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003100040331      *
003200040331     d $poDPD          s                   like(ORGfil) dim(99) inz
003300050314     d $clilac         s              7  0 dim(99) inz
003400050314     d $tptlac         s                   like(§lactpt) dim(99) inz
003500050314     d $reslac         s                   like(§lacres) dim(99) inz
003600050314     d $reclac         s                   like(§lacrec) dim(99) inz
003700050314     d $csrlac         s                   like(§laccsr) dim(99) inz
003800050314     d $ssrlac         s                   like(§lacssr) dim(99) inz
003900050314     d $dirlac         s                   like(§lacdir) dim(99) inz
004000050315     d $tpilac         s                   like(§lactpi) dim(99) inz
004100050315     d $fmilac         s                   like(§lacfmi) dim(99) inz
004200050314      *
004300040119      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
004400040119      *
004500050330      * - Ds per controlli bolla
004600050330     d titasds       e ds                  extname(TITAS00F)
004700050330     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
004800050330
004900040331     d OG143         e ds                  inz
005000040119     d dTASflo       e ds                  inz
005100050314     d dlac          e ds                  inz
005200050314     d dstb          e ds                  inz
005300050330     d dta4a         e ds                  inz
005400040331      *
005500040331     d DS_Time1        ds            14    inz
005600040331     d   dsT1_hms                     6  0 inz
005700040331     d   dsT1_dmy                     8  0 inz
005800040331     d DS_Time2        ds            14    inz
005900040331     d   dsT2_ymd                     8  0 inz
006000040331     d   dsT2_hms                     6  0 inz
006100040331      *
006200040331     d Status         sds           333
006300040331     d  SDSjob               244    253                                         Job name
006400040331     d**SDSusr               254    263                                         User name
006500040331     d**SDSjnr               264    269S 0                                      Job number
006600040331      *
006700040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
006800040331      *
006900040331      * - Flag booleani
007000040331     d $DPD            s              1    inz(*off)
007100050314     d $CLI            s              1    inz(*off)
007200040331      * - Indici di schiera
007300040331     d xx              s              3  0 inz
007400040331      * - Campi di comodo
007500040331     d WtasDCM         s              8    inz(*zeros)
007600050330     d WtasRMN         s             15    inz(*zeros)
007700050330     d WtasAAS         s              4    inz(*zeros)
007800040520     d WtasPOm         s              3  0 inz
007900040331     d Wtime           s             14  0 inz
008000040331     d WdateISO        s               d   datfmt(*ISO) inz
008100041021     d klaocop         s                   like(laocop) inz('DPD')
008200050314     d klaccop         s                   like(laccop) inz('CLI')
008300041021     d tilao_u_w       s              1
008400050314     d tilac_u_w       s              1
008500050314     d kcod            s                   like(tblcod) inz('TB')
008600050314     d kkey            s                   like(tblkey)
008700050314     d savtbl          s                   like(tastbl) inz('  ')
008800050330     d WTAS_orig       s              1
008900050330     d ktrc            s                   like(ta4trc) inz('A')
009000050330     d Wrma            s                   like(§ta4arma)
009100050330     d Wnim            s                   like(lacnim)
009200040119
009300040119      *---------------------------------------------------------------*
009400040119
009500040119      * Predisposizione indicatori per riconoscere i tipi record
009600040119      *   reperiti dal file TITAS30C
009700040119     iTITAS000      31
009800040119     iTITAS010      32
009900040119     iTITASP00      33
010000040119
010100040119      *---------------------------------------------------------------*
010200040119
010300040119      *
010400040119      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
010500040119      *
010600040119     c     K04TAS30      klist
010700040119     c                   kfld                    TASaas
010800040119     c                   kfld                    TASlnp
010900040119     c                   kfld                    TASnrs
011000040119     c                   kfld                    TASnsp
011100050330     c     Ktas30_f      klist
011200050330     c                   kfld                    lblaan
011300050330     c                   kfld                    lbllpn
011400050330     c                   kfld                    lblnrn
011500050330     c                   kfld                    lblnsn
011600041021      *
011700041021     c     K05LAO        klist
011800041021     c                   kfld                    LAGaas
011900041021     c                   kfld                    LAGlnp
012000041021     c                   kfld                    LAGnrs
012100041021     c                   kfld                    LAGnsp
012200041021     c                   kfld                    klaocop
012300050314     c*
012400050314     c     K05LAC        klist
012500050314     c                   kfld                    LAGaas
012600050314     c                   kfld                    LAGlnp
012700050314     c                   kfld                    LAGnrs
012800050314     c                   kfld                    LAGnsp
012900050314     c                   kfld                    klaccop
013000050330
013100050330     c     Ktas          klist
013200050330     c                   kfld                    LBLaao
013300050330     c                   kfld                    LBLlpo
013400050330     c                   kfld                    LBLnro
013500050330     c                   kfld                    LBLnso
013600050330      *
013700050330     c     Kta4          klist
013800050330     c                   kfld                    TASaas
013900050330     c                   kfld                    TASlnp
014000050330     c                   kfld                    TASnrs
014100050330     c                   kfld                    TASnsp
014200050330     c                   kfld                    ktrc
014300050314     c*
014400050314     c     Ktab          klist
014500050314     C                   KFLD                    CODUT
014600050314     C                   KFLD                    kcod
014700050314     c                   kfld                    kkey
014800050314     c                   z-add     1             codut             1 0
014900040331      *
015000040331      *   I N D I C A T O R I   U T I L I Z Z A T I   - - - - - - - - *
015100040331      *
015200040331      * 21    - P.O.partenza del network DPD
015300040331      * 31-33 - Tracciato reperito dal file TITAS30C (000/010/P00)
015400040119
015500040119      *---------------------------------------------------------------*
015600040119
015700040331      * Operazioni Iniziali
015800040331     c                   exsr      OperazIniz
015900040331      *
016000040119      * Ciclo di lettura del file TILAG00R
016100040119     c                   read      TILAG000
016200040119      *
016300040119do  1c                   dow       NOT %eof(TILAG00R)
016400040119      *
016500040119      * - Elaborazione dei soli rec. "IM"
016600040119if  2c                   if        LAGapp = 'IM'
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
017900050314     c* verifico se cliente presente in tabella "LAC"
018000050314     c                   eval      *in21   = *off
018100050314     c                   eval      $cli    = *off
018200050314     c                   eval      $dpd    = *off
018300050316     c                   z-add     1             xx
018400050318     c     tasksc        lookup    $clilac(xx)                            21
018500050314if  4c                   if        *in21 = *on
018600050314     c                   exsr      Wrt_TILAC
018700050314x   4c                   else
018800040331     c                   exsr      Wrt_TILAO
018900050314e   4c                   endif
019000040119     c                   exsr      Upd_TITAS
019100040119e   3c                   endif
019200040119      *
019300040119e   2c                   endif
019400040119      *
019500040119      * - Lettura del record successivo nel file TILAG00R
019600040119     c                   read      TILAG000
019700040119      *
019800040119e   1c                   enddo
019900040119      *
020000040119      * Fine
020100040119     c     Fine          tag
020200040119     c                   eval      *inLR = *on
020300040331      *
020400040331      **-------------------------------------------------------------**
020500040331      ** OperazIniz ** Operazioni Iniziali                           **
020600040331      **-------------------------------------------------------------**
020700040331     c     OperazIniz    BEGSR
020800040331      *
020900040331      * Impostazione del campo LAOTIM = aaaa/mm/gg+hh:mm:ss
021000040331     c                   time                    Wtime
021100040331     c                   movel     Wtime         DS_Time1
021200040331     c                   eval      dsT2_hms   = dsT1_hms
021300040331     c     *eur          movel     dsT1_dmy      WdateISO
021400040331     c                   move      WdateISO      dsT2_ymd
021500040331      *
021600040331      * Intabellamento dei p.o. appartenenti al network "DPD"
021700040331     c                   clear                   xx
021800040331     c                   clear                   $poDPD
021900040331     c     *loval        setll     AZORG
022000040331     c                   read      AZORG
022100040331do  1c                   dow       NOT %eof(AZORG01L)
022200040331     c                   movel     ORGde3        OG143
022300040331if  2c                   if        §OGntw     = 'DPD'
022400040331     c                   add       1             xx
022500040331     c                   eval      $poDPD(xx) = ORGfil
022600040331e   2c                   endif
022700040331     c                   read      AZORG
022800040331e   1c                   enddo
022900050314      * Intabellamento tabella "LAC" dei clienti che ricevono le immagini
023000050321     c* in automatico
023100050314     c                   clear                   xx
023200050316     c                   clear                   $clilac
023300050316     c                   clear                   $tptlac
023400050316     c                   clear                   $reslac
023500050316     c                   clear                   $reclac
023600050316     c                   clear                   $csrlac
023700050316     c                   clear                   $ssrlac
023800050316     c                   clear                   $dirlac
023900050316     c                   clear                   $tpilac
024000050316     c                   clear                   $fmilac
024100050316     c     'LAC'         setll     tntbe01l
024200050314     c                   do        *hival
024300050316     c     'LAC'         reade     tntbe01l
024400050314     c                   if        %eof(tntbe01l)
024500050314     c                   leave
024600050314     c                   endif
024700050316     c                   if        tbeatb <> *blanks
024800050316     c                   iter
024900050316     c                   endif
025000050321     c                   movel     tbeuni        dlac
025100050321     c     §lacaut       ifeq      'S'
025200050314     c                   add       1             xx
025300050314     c                   movel     tbeke1        $clilac(xx)
025400050314     c                   movel     §lactpt       $tptlac(xx)
025500050314     c                   movel     §lacres       $reslac(xx)
025600050314     c                   movel     §lacrec       $reclac(xx)
025700050314     c                   movel     §laccsr       $csrlac(xx)
025800050314     c                   movel     §lacssr       $ssrlac(xx)
025900050314     c                   movel     §lacdir       $dirlac(xx)
026000050315     c                   movel     §lactpi       $tpilac(xx)
026100050315     c                   movel     §lacfmi       $fmilac(xx)
026200050321     c                   endif
026300050314     c                   enddo
026400040331      *
026500040331     c                   ENDSR
026600040331      *
026700040331      **-------------------------------------------------------------**
026800040331      ** Wrt_TILAO  ** Registrazione dati nel file TILAO00F x DPD    **
026900040331      **-------------------------------------------------------------**
027000040331     c     Wrt_TILAO     BEGSR
027100040331      *
027200040520      * Verifica se p.o. del mittente DPD
027300040331     c                   eval      *in21   = *off
027400040520     c                   movel     TASccm        WtasPOm
027500040520     c     WtasPOm       lookup    $poDPD                                 21
027600040331     c                   eval      $DPD    = *in21
027700040331      *
027800040331     c     $DPD          cabeq     *off          NOwrtTILAO
027900040331      *
028000040331      * Impostazione dei campi numerici in campi alfanumerici di comodo
028100040331     c                   movel     TASdcm        WtasDCM
028200040405     c                   move      TASrmn        Wtasrmn
028300040331      *
028400041021      * Se su TILAO esiste già un record non elaborato aggiorno else scrivo
028500041021     C                   eval      Tilao_U_W = 'W'
028600041021      *
028700041021     C     k05lao        setll     Tilao01L
028800041021     C     k05lao        reade     Tilao01L
028900041021     C                   DOW       NOT %EOF(TILAO01L)
029000041021     C                   IF        LAGela = '00'
029100041021     C                   eval      Tilao_U_W = 'U'
029200041021     c                   leave
029300041021     C                   ELSE
029400041021     C     k05lao        reade     Tilao01L
029500041021     c                   ENDIF
029600041021     c                   ENDDO
029700041021      *
029800040331      * Preparazione del record
029900040331     c                   clear                   TILAO000
030000040331     c                   move      DS_Time2      LAOtim
030100040331     c                   move      TASaas        LAOaas
030200040331     c                   move      TASlnp        LAOlnp
030300040331     c                   move      TASnrs        LAOnrs
030400040331     c                   move      TASnsp        LAOnsp
030500040331     c                   movel     'DPD'         LAOcop
030600040405     c                   eval      LAOope  = %subst(wTASrmn:4:11)
030700040331     c                                     + '_'
030800040331     c                                     + wTASdcm
030900040331     c                   movel     SDSjob        LAGidl
031000040331     c                   move      *zeros        LAGela
031100040331      *
031200041021      * Scrittura /aggiornamento
031300041021     C                   IF        Tilao_U_W = 'U'
031400041021     c                   update    TILAO000
031500041021     c                   ELSE
031600041021     c                   write     TILAO000
031700041021     c                   ENDIF
031800040331      *
031900040331     c     NOwrtTILAO    ENDSR
032000040119      *
032100050314      **-------------------------------------------------------------**
032200050314      ** Wrt_TILAC  ** Registrazione dati nel file TILAC00F x cliente**
032300050314      **-------------------------------------------------------------**
032400050314     c     Wrt_TILAC     BEGSR
032500050330
032600050330     c                   eval      titasdsChk = titasds
032700050330     c*
032800050330     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
032900050330     c     K04tas30      chain     fnlbl01l
033000050330     c                   if        %found(fnlbl01l)
033100050421     c                   setoff                                       313233
033200050330     c     Ktas          chain(N)  titas30c
033300050330     c                   if        %found(titas30c)
033400050330     c                   eval      titasdschk = titasds
033500050330     c                   endif
033600050330     c* richaino la bolla figlia
033700050421     c                   setoff                                       313233
033800050330     c     ktas30_f      chain     titas30c
033900050330     c                   endif
034000050314     c
034100050330     c                   if        Chk_tastbl <> savtbl
034200050330     c                   movel(p)  Chk_tastbl    kkey
034300050314     c                   clear                   dstb
034400050314     c     ktab          chain     tabel00f
034500050314     c                   if        %found(tabel00f)
034600050314     c                   movel     tbluni        dstb
034700050314     c                   endif
034800050330     c                   eval      savtbl = Chk_tastbl
034900050314     c                   endif
035000050330
035100050330     c* controllo porto (rispetto alla bolla originale)
035200050314     c                   if        $tptlac(XX) <> 'E' and §tbtpo <> $tptlac(xx)
035300050314     c                   goto      NOwrtTILAC
035400050314     c                   endif
035500050330     c* reso (rispetto alla bolla corrente (figlia))
035600050314     c                   if        $reslac(xx) <> 'S' and tasfbr = 'S'
035700050314     c                   goto      NOwrtTILAC
035800050314     c                   endif
035900050330     c* recupero (rispetto alla bolla originale)
036000050314     c                   if        $reclac(xx) <> 'S' and §tbrbl = 'R'
036100050314     c                   goto      NOwrtTILAC
036200050314     c                   endif
036300050330     c* c/servizio (rispetto alla bolla originale)
036400050314     c                   if        $csrlac(xx) <> 'S' and §tbrbl = 'S'
036500050314     c                   goto      NOwrtTILAC
036600050314     c                   endif
036700050330     c* serie (rispetto alla bolla originale)
036800050330     c                   if        $ssrlac(xx) = 'S' and chk_tasnrs = *zeros
036900050317     c                   goto      NOwrtTILAC
037000050314     c                   endif
037100050314     c
037200050314     c                   eval      $cli = *on
037300050314      *
037400050314      * Se su TILAC esiste già un record non elaborato aggiorno else scrivo
037500050314     C                   eval      Tilac_U_W = 'W'
037600050314      *
037700050314     C     k05lac        setll     Tilac01L
037800050314     C     k05lac        reade     Tilac01L
037900050314     C                   DOW       NOT %EOF(TILAC01L)
038000050314     C                   IF        LACela = '00'
038100050314     C                   eval      Tilac_U_W = 'U'
038200050314     c                   leave
038300050314     C                   ELSE
038400050314     C     k05lac        reade     Tilac01L
038500050314     c                   ENDIF
038600050314     c                   ENDDO
038700050330      *
038800050330      * Impostazione dei campi numerici in campi alfanumerici di comodo
038900050330     c                   move      TASrmn        WtasRMN
039000050330     c                   move      TASaas        WtasAAS
039100050314      *
039200050314      * Preparazione del record
039300050314     c                   clear                   TILAC000
039400050314     c                   move      DS_Time2      LACtim
039500050314     c                   move      TASaas        LACaas
039600050314     c                   move      TASlnp        LAClnp
039700050314     c                   move      TASnrs        LACnrs
039800050314     c                   move      TASnsp        LACnsp
039900050314     c                   movel     'CLI'         LACcop
040000050314     c                   movel     $dirlac(xx)   LACdir
040100050315     c                   movel     $tpilac(xx)   LACtpi
040200050330     c* flag nome immagine:
040300050330     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
040400050330     c*           sa Laguna) --> quindi lascio = blanks
040500050330     c                   if        $fmilac(xx)  <> 'S '
040600050330     c                   select
040700050330     c* RMN
040800050330     c                   when      $fmilac(XX) = 'N '
040900050330     c                   eval      LACNIM  = wtasrmn
041000050330     c* RMA
041100050330     c                   when      %subst($fmilac(XX):1:1 ) = 'A'
041200050330     c                   clear                   dta4a
041300050330     c                   clear                   wnim
041400050330     c     kta4          chain     tita430c
041500050330     c                   if        %found(tita430c)
041600050330     c                   movel     ta4not        dta4a
041700050330     c                   endif
041800050330     c                   eval      wrma = %trim(§TA4aRMA)
041900050330     c                   if        wrma <> *blanks
042000050330     c                   z-add     1             i                 2 0
042100050330     c                   dow       i <= %len(%trim(wrma))
042200050330     c                   if        %subst(wrma:i:1) = *blanks
042300050330     c                   eval      wrma=%subst(wrma:1:i-1)+
042400050330     c                                  %subst(wrma:i+1)
042500050330     c                   eval      i = i - 1
042600050330     c                   endif
042700050330     c                   eval      i = i + 1
042800050330     c                   enddo
042900050330     c                   endif
043000050330     c                   select
043100050330     c* rma +'_' + spedizione
043200050330     c                   when      $fmilac(xx) ='AS'
043300050330     c                   eval      wnim = %trim(wrma) + '_' + lacaas
043400050330     c                             + laclnp + lacnrs + lacnsp
043500050330     c* rma
043600050330     c                   when      $fmilac(xx) = 'A '
043700050330     c                   eval      wnim = wrma
043800050330     c* anno + '_' + rma
043900050330     c                   when      $fmilac(xx) = 'AA'
044000050330     c                   eval      wnim = wtasaas + '_' + wrma
044100050330     c                   endsl
044200050330     c                   eval      lacnim=wnim
044300050330     c
044400050330     c
044500050330     c* RMN+"_"+NSP
044600050330     c                   when      $fmilac(xx) = 'NS'
044700050330     c                   eval      lacnim  = wtasrmn + '_' + lacaas
044800050330     c                             + laclnp + lacnrs + lacnsp
044900050330     c* anno + "_"+RMN
045000050330     c                   when      $fmilac(xx) = 'NN'
045100050330     c                   eval      lacnim  = wtasaas + '_' + wtasrmn
045200050330     c                   endsl
045300050330     c                   endif
045400050330
045500050314     c                   movel     SDSjob        LACidl
045600050314     c                   move      *zeros        LACela
045700050314      *
045800050314      * Scrittura /aggiornamento
045900050314     C                   IF        Tilac_U_W = 'U'
046000050314     c                   update    TILAC000
046100050314     c                   ELSE
046200050314     c                   write     TILAC000
046300050314     c                   ENDIF
046400050314      *
046500050330     c     NOwrtTILAC    ENDSR
046600050314      *
046700040119      **-------------------------------------------------------------**
046800040119      ** Upd_TITAS  ** Aggiornamento dati nel file TITAS30C          **
046900040119      **-------------------------------------------------------------**
047000040119     c     Upd_TITAS     BEGSR
047100040119      *
047200040119      * Possono esserci 2 records su TITAS30C
047300040119      *   (con "tipo record" diverso)
047400040119do  1c                   dou       %eof(TITAS30C)
047500040119      *
047600040119     c                   movel     TASflo        dTASflo
047700050314if  2c                   if        $DPD    = *off  and $CLI = *off
047800040119     c                   eval      §FLOiml = 'S'
047900040331x   2c                   else
048000040331     c                   eval      §FLOiml = 'I'
048100040331e   2c                   endif
048200040119     c                   movel     dTASflo       TASflo
048300040119      *
048400040119     c   31              UPDATE    TITAS000
048500040119     c   32              UPDATE    TITAS010
048600040119     c   33              UPDATE    TITASP00
048700040119      *
048800040128     c                   setoff                                       313233
048900040119     c     K04TAS30      reade     TITAS30C
049000040119      *
049100040119e   1c                   enddo
049200040119      *
049300040119      * Cancellazione record dal file TILAG00R
049400040119     c                   DELETE    TILAG000
049500040119      *
049600040119     c                   ENDSR
