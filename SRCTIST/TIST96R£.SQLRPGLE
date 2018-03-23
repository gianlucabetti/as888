000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400081014      *  Statistica spedizioni per cliente unificante
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800081014     ftitas38c  if   e           k disk
000900060320     fazorg01l  if   e           k disk
001000060320     ftabel00f  if   e           k disk
001100081014     fwfspe00f  uf a e           k disk    usropn
001200081014     fwfspe10f  uf a e           k disk    usropn
001201081015     ftist96p   o    e             printer oflind(*in90)
001300060323      *------------------------------------------------------------------------*
001400060323      *  RIEPILOGO INDICATORI
001500060323      *------------------------------------------------------------------------*
001600060323      * 30 - comodo
001700060215
001800060215      *------------------------------------------------------------------------*
001900060215      *   V A R I A B I L I
002000060215      *------------------------------------------------------------------------*
002100080710     d comman          s            110
002200060320     d codut           s              1  0 inz(1)
002300081014     d annoin          s                   like(tasaas)
002400081014     d mesgioin        s                   like(tasmgs)
002500081014     d annofi          s                   like(tasaas)
002600081014     d mesgiofi        s                   like(tasmgs)
002700081014     d save_ksc        s                   like(tasksc)
002800081014     d w0030           s              3  0
002900081014     d wdata           s              8  0
003000081014     d wora            s              6  0
003001081015     d z               s              3  0
003100081014
003200060320     d kcod            s                   like(tblcod)
003300060320     d kkey            s                   like(tblkey)
003400080710     d lenght          s             15  5 inz(110)
003500060320     d wlib            s              9
003600060320     d w0140           s             14  0
003700060320
003800060320      *------------------------------------------------------------------------*
003900060320      *   S C H I E R E
004000060320      *------------------------------------------------------------------------*
004100060215
004101081015      * Tipi bolla da considerare in questa statistica
004102081015     d TBO_OK          S              2    DIM(50)                              Tipi bolla
004103081015
004200060215      *------------------------------------------------------------------------*
004300060215      *   D S   I N T E R N E / E S T E R N E
004400060215      *------------------------------------------------------------------------*
004500060320
004600060320     d parm01          ds
004700081014     d  pardin_1               1      8  0
004800081014     d  pardfi_1               9     16  0
004900081014     d  pardin_2              17     24  0
005000081014     d  pardfi_2              25     32  0
005100060321
005200060321     d wlbdat          ds                  inz
005300060321     d  g02dat                 1      8  0
005400060321     d  g02inv                 9     16  0
005500060321     d  g02err                17     17
005600060321     d  g02tgi                18     22  0
005700081014
005800081014     d DS_SPE1_TOTU    DS
005900081014     d  tot1_kun                      7  0
006000081014     d  tot1_kund                    48
006100081014     d  tot1_clv                      1
006200081014     d  tot1_car                      3  0
006300081014     d  tot1_card                    25
006400081014     d  tot1_fil                      3  0
006500081014     d  tot1_fild                    20
006600081014     d  tot1_cmu                      7  0
006700081014     d  tot1_cmud                    25
006800081014     d  tot1_spe                     13  0
006900081014     d  tot1_col                     13  0
007000081014     d  tot1_pes                     13  1
007100081014
007200060320
007300060320     d azuteds       e ds                  extname(azute00f)
007400060320     d ddatiute      e ds
007500060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
007600060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
007700060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
007800060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
007900060215     d kpjba         e ds
008000060320     d og143         e ds
008100060726     d tibs02ds      e ds
008200081014     d tibs10ds      e ds
008300060320     d tibs34ds      e ds
008400060320     d tibs69ds      e ds
008500060320     d trul31ds      e ds                  inz
008600061002     d ds01          e ds
008601081015      ** Tipi bolla
008602081015     D DSTB          E DS
008700060215
008800060215      *------------------------------------------------------------------------*
008900080707     C*
009000080707     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni
009100080707     C
009200080707     C/EXEC SQL
009300080707     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009400080707     C/END-EXEC
009500080707     C*
009600060215
009601081015      * carico i tipi bolla da prendere in considerazione
009602081015     c                   exsr      Carcbo
009700081014      * ciclo per data periodo numero 1
009800060215
009900081014     c                   movel     pardin_1      annoin
010000081014     c                   move      pardin_1      mesgioin
010100081014     c                   movel     pardfi_1      annofi
010200081014     c                   move      pardfi_1      mesgiofi
010300081014
010400081014     c                   exsr      ela_tas
010500081014
010600081014      * ciclo per data periodo numero 2
010700081014
010800081014     c                   movel     pardin_2      annoin
010900081014     c                   move      pardin_2      mesgioin
011000081014     c                   movel     pardfi_2      annofi
011100081014     c                   move      pardfi_2      mesgiofi
011200081014
011300081014     c                   exsr      ela_tas
011400081014
012600081014      * Elaboro i totali
012800081014     c                   exsr      ela_tot
012801081015
012802081015      * stampo  la fine elaborazione
012803081015     c                   write     st96s1
012900081014
013700060215     c                   eval      *inlr = *on
013800060215
013900081014      *------------------------------------------------------------------------*
014000081014      * ELABORO DETTAGLIO DELLE BOLLE DI SEDE
014100081014      *------------------------------------------------------------------------*
014200081014     c     ela_tas       begsr
014300081014
014400081014     c     Ktas02        setll     titas38c
014500081014
014600081014     c                   do        *hival
014700081014     c                   read      titas38c
014800081014
014900081014     c                   if        %eof(titas38c)
015000081014     c                   leave
015100081014     c                   endif
015200081014      * verifico data se fine periodo
015300081014      * se anno diverso vado a fine
015400081014     c                   if        tasaas <> annofi
015500081014     c                   leave
015600081014     c                   endif
015700081014
015800081014      * se mese giorno del file maggiore del finale vado a fine
015900081014     c                   if        tasmgs > mesgiofi
016000081014     c                   leave
016100081014     c                   endif
016200081014
016300081015      * verifico il tipo bolla se non è ne F1 nè A2 nè AS nè FS leggo il successivo
016301081015     c     tastbl        lookup    tbo_ok                                 30
016302081015     c                   if        not %found
016400081015     c***                if        tastbl <> 'F1' and tastbl <> 'A2' and
016401081015     c***                          tastbl <> 'AS' and 'FS'
016500081014     c                   iter
016600081014     c                   endif
016700081014
016800081014      * recupero per ogni cliente l'unificante se diverso dal precedente
016900081014     c                   if        tasksc <> Save_ksc
017000081014     c                   eval      save_ksc = tasksc
017100081014      * ricerco unificante / categoria / commerciale ecc ecc
017200081014     c                   exsr      Ric_unificanti
017300081014     c                   endif
017400081014
017500081014      * scrivo il record
017600081014     c                   exsr      Write_dett
017700081014
017800081014     c                   enddo
017900081014
018000081014     c                   endsr
018100060215      *------------------------------------------------------------------------*
018200081014      * SCRIVO O AGGIORNO IL RECORD DI DETTAGLIO
018300060215      *------------------------------------------------------------------------*
018400081014     c     Write_dett    begsr
018500060215
018600081014      * aggancio il record
018700081014     c                   eval      spedsp = (tasaas * 10000) + tasmgs
018800081014     c     Kspe02        chain     Wfspe00f
018900081014
019000081014     c                   add       1             spenspe
019100081014     c                   add       tasncl        spencol
019101081015      * peso prendo il + grande
019102081015     c                   if        taspkc > 0 and tasncl = tasncp
019103081015     c                   add       taspkc        spepeso
019104081015     c                   else
019105081015     c                   if        taspkf > taspkc
019200081014     c                   add       taspkf        spepeso
019201081015     c                   else
019202081015     c                   add       taspkc        spepeso
019203081015     c                   endif
019204081015     c                   endif
019205081015
019300081014     c                   if        %found(wfspe00f)
019400081014     c                   update    wfspe000
019500081014     c                   else
019600081014     c                   write     wfspe000
019700081014     c                   endif
019800081014
019900081014     c                   feod      wfspe00f
020000060215
020100060215     c                   endsr
020200081014      *------------------------------------------------------------------------*
020300081014      * CERCO CLIENTE E COMMERCIALE UNIFICANTE
020400081014      *------------------------------------------------------------------------*
020500081014     c     Ric_unificantibegsr
020600081014
020700081014     c                   clear                   wfspe000
020800081014      * cerco il codice cliente unificante
020900081014     c                   clear                   tibs10ds
021000081014     c                   eval      d10tle = 'ST'
021100081014     c                   eval      d10paf = 'P'
021200081014     c                   z-add     tasksc        d10cod
021300081014     c                   z-add     *date         d10drf
021400081014     c                   CALL      'TIBS10R'
021500081014     c                   parm                    tibs10ds
021600081014     c                   parm      'A'           partpexe          1
021700081014      * se ci sono errori valorizzo come unificante il cliente
021800081014     c                   if        d10err <> ' '
021900081014     c                   eval      spekun = tasksc
022000081014      *
022100081014     c                   else
022200081014      *
022300081014     c                   eval      spekun = d10cop
022400081014     c                   endif
022500081014      * cerco i dati anagrafici
022600081014     c                   clear                   tibs69ds
022700081014     c                   move      spekun        i69kac
022800081014     c                   move      spekun        i69kcp
022900081014     c                   call      'TIBS69R'
023000081014     c                   parm                    tibs69ds
023100081014     c                   parm                    ds_cnaco
023200081014     c                   parm                    ds_cnind
023300081014     c                   parm                    ds_cnclp
023400081014     c                   parm                    ds_fncls
023500081014      *
023600081014     c                   eval      spekund = acorag
023700081014     c                   eval      speclv  = clpclv
023800081014
023900081014      * verifico il codice commerciale unificante
024000081014     c                   eval      kcod = '01'
024100081014     c                   movel(p)  clpage        kkey
024200081014     c     ktabel        chain     tabel00f
024300081014     c                   if        not %found(tabel00f) or tblflg <> *blanks
024400081014     c                   eval      specmu  = clpage
024500081014     c                   eval      specmud = *all'*'
024600081014     c                   else
024700081014     c                   eval      ds01    = tbluni
024800081014     c                   eval      specmu  = §01rgf
024900081014     c                   movel(p)  specmu        kkey
025000081014     c     ktabel        chain     tabel00f
025100081014     c                   if        not %found(tabel00f) or tblflg <> *blanks
025200081014     c                   eval      specmud = *all'*'
025300081014     c                   else
025400081014     c                   eval      ds01    = tbluni
025500081014     c                   eval      specmud = §01age
025600081014     c                   endif
025700081014     c                   endif
025800081014
025900081014      * albero commerciale unificante
026000081014     c                   movel     specmu        w0030
026100081014     c     w0030         chain     azorg01l
026200081014
026300081014      * area
026400081014     c                   eval      specar = orgcar
026500081014     c                   eval      kcod = '05'
026600081014     c                   movel(p)  specar        kkey
026700081014     c     ktabel        chain     tabel00f
026800081014     c                   if        not %found(tabel00f) or tblflg <> *blanks
026900081014     c                   eval      specard = *all'*'
027000081014     c                   else
027100081014     c                   eval      specard = tbluni
027200081014     c                   endif
027300081014      * p.o.
027400081014     c                   eval      spefil = orgfil
027500081014     c                   if        not %found(azorg01l) or orgfva <> *blanks
027600081014     c                   eval      spefild = *all'*'
027700081014     c                   else
027800081014     c                   eval      spefild = orgdes
027900081014     c                   endif
028000081014
028100081014     c                   endsr
028200060320
028300081014      *------------------------------------------------------------------------*
028400081014      * ELABORO I TOTALI
028500081014      *------------------------------------------------------------------------*
028600081014     c     ela_tot       begsr
028700081014
028800081014      * primo giro con il periodo precedente
028900081014
029000081014     c/EXEC SQL
029100081014     C+ declare C1 cursor for
029200081014     C+ select spekun, spekund, speclv, specar, specard, spefil,
029300081014     C+ spefild, specmu, specmud,
029400081014     C+ decimal(sum(spenspe), 13, 0),
029500081014     C+ decimal(sum(spencol), 13, 0), decimal(sum(spepeso), 13, 1)
029600081014     C+ from wfspe00f
029700081014     C+ where spedsp between :pardin_1 and :pardfi_1
029800081014     C+ group by
029900081014     C+ spekun, spekund, speclv, specar, specard, spefil,
030000081014     C+ spefild, specmu, specmud
030100081014     C+ for read only
030200081014
030300081014     C/END-EXEC
030400081014     C
030500081014     C/EXEC SQL
030600081014     C+ open C1
030700081014     C/END-EXEC
030800081014     C
030900081014     C/EXEC SQL
031000081014     C+ Fetch next from C1 into :DS_SPE1_TOTU
031100081014     C/END-EXEC
031200081014     C
031300081014
031400081014     c                   dow       sqlcod = *zeros
031500081014      *
031600081014     c                   clear                   wfspe100
031700081014     c                   eval      sp1kun = tot1_kun
031800081014     c                   eval      sp1kund = tot1_kund
031900081014     c                   eval      sp1clv = tot1_clv
032000081014     c                   eval      sp1car = tot1_car
032100081014     c                   eval      sp1card = tot1_card
032200081014     c                   eval      sp1fil = tot1_fil
032300081014     c                   eval      sp1fild = tot1_fild
032400081014     c                   eval      sp1cmu = tot1_cmu
032500081014     c                   eval      sp1cmud = tot1_cmud
032600081014     c                   eval      sp1nspe1 = tot1_spe
032700081014     c                   eval      sp1ncol1 = tot1_col
032800081014     c                   eval      sp1peso1 = tot1_pes
032900081014     c                   write     wfspe100
033000081014     C/EXEC SQL
033100081014     C+ Fetch next from C1 into :DS_SPE1_TOTU
033200081014     C/END-EXEC
033300081014     C
033400081014     c                   enddo
033500081014     C/EXEC SQL
033600081014     C+ close C1
033700081014     C/END-EXEC
033800081014
033900081014     C                   FEOD      WFSPE10F
034000081014      * secondo giro con il secondo periodo
034100081014
034200081014     c/EXEC SQL
034300081014     C+ declare C2 cursor for
034400081014     C+ select spekun, spekund, speclv, specar, specard, spefil,
034500081014     C+ spefild, specmu, specmud,
034600081014     C+ decimal(sum(spenspe), 13, 0),
034700081014     C+ decimal(sum(spencol), 13, 0), decimal(sum(spepeso), 13, 1)
034800081014     C+ from wfspe00f
034900081014     C+ where spedsp between :pardin_2 and :pardfi_2
035000081014     C+ group by
035100081014     C+ spekun, spekund, speclv, specar, specard, spefil,
035200081014     C+ spefild, specmu, specmud
035300081014     C+ for read only
035400081014
035500081014     C/END-EXEC
035600081014     C
035700081014     C/EXEC SQL
035800081014     C+ open C2
035900081014     C/END-EXEC
036000081014     C
036100081014     C/EXEC SQL
036200081014     C+ Fetch next from C2 into :DS_SPE1_TOTU
036300081014     C/END-EXEC
036400081014     C
036500081014
036600081014     c                   dow       sqlcod = *zeros
036700081014      *
036800081014     c     tot1_kun      chain     wfspe10f
036900081014     c
037000081014     c                   If        not %found(wfspe10f)
037100081014     c                   clear                   wfspe100
037200081014     c                   eval      sp1kun = tot1_kun
037300081014     c                   eval      sp1kund = tot1_kund
037400081014     c                   eval      sp1clv = tot1_clv
037500081014     c                   eval      sp1car = tot1_car
037600081014     c                   eval      sp1card = tot1_card
037700081014     c                   eval      sp1fil = tot1_fil
037800081014     c                   eval      sp1fild = tot1_fild
037900081014     c                   eval      sp1cmu = tot1_cmu
038000081014     c                   eval      sp1cmud = tot1_cmud
038100081014     c                   endif
038200081014
038300081014     c                   eval      sp1nspe2 = tot1_spe
038400081014     c                   eval      sp1ncol2 = tot1_col
038500081014     c                   eval      sp1peso2 = tot1_pes
038600081014     c                   If        not %found(wfspe10f)
038700081014     c                   write     wfspe100
038800081014     c                   else
038900081014     c                   update    wfspe100
039000081014     c                   endif
039100081014     C/EXEC SQL
039200081014     C+ Fetch next from C2 into :DS_SPE1_TOTU
039300081014     C/END-EXEC
039400081014     C
039500081014     c                   enddo
039600081014     C/EXEC SQL
039700081014     C+ close C2
039800081014     C/END-EXEC
039900081014     c                   endsr
039901081015      *---------------------------------------------------------------*
039902081015      ** CARICAMENTO codici bolla da cosniderare in questa statistica
039906081015      *---------------------------------------------------------------*
039907081015     c     Carcbo        Begsr
039908081015      **
039909081015     c                   Movel     'TB'          KCod
039910081015     c                   z-add     0             z
039911081015     c     Ktabel02      setll     tabel00f
039912081015     c                   do        *hival
039913081015     c     Ktabel02      reade     tabel00f
039914081015      * fine file
039915081015     c                   If        %eof(tabel00f)
039916081015     c                   leave
039917081015     c                   endif
039918081015    1
039919081015    2c                   If        tblkey <> *blanks
039920081015     c                   Movel     TBLuni        DStb
039921081015      * normali e reso
039922081015    3c                   If        §tbrbl  = 'S' or §tbrbl = 'N'
039923081015     c                   add       1             z
039924081015     c                   movel     TBLkey        tbo_oK(z)
039925081015    3c                   endif
039926081015      **
039927081015    2c                   endif
039928081015    1c                   enddo
039929081015
039930081015      **
039964081015
039965081015     c                   Endsr
040000060320      *--------------------------------------------------------------*
040100060320      * ROUTINE INIZIALE
040200060320      *--------------------------------------------------------------*
040300060320     c     *inzsr        begsr
040400060320
040500060320     c     *entry        plist
040600060320     c                   parm                    kpjba
040700060320     c                   movel     kpjbu         parm01
040800060320
040900060320     c     *dtaara       define    §azute        azuteds
041000060320     c     *dtaara       define    §datiute      ddatiute
041100060320
041200060320     c                   in(e)     *dtaara
041300060320     c                   if        %error or rsut = *blanks
041400060320     c                   clear                   tibs34ds
041500060320     c                   call      'TIBS34R'
041600060320     c                   parm                    tibs34ds
041700060320     c                   in        *dtaara
041800060320     c                   endif
041900060320
042000060320
042100060320      * se s.i. di prova imposto la libreria di prova
042200060320     c                   if        %subst(knsif:7:1) = 'P'
042300060320     c                   eval      wlib = 'GAITRAAZP '
042400060320     c                   else
042500060320     c                   eval      wlib = 'GAITRAAZM '
042600060320     c                   endif
043700081015      * pulisco il file in gaitraazm o gaitraazp
043800080710     c                   clear                   comman
043901081015     c                   EVAL      comman = 'CLRPFM FILE('
043902081015     c                             + %TRIMR(WLIB)
043903081015     c                             + '/WFSPE00F)'
044000080710
044100080710     c                   call      'QCMDEXC'
044200080710     c                   parm                    comman
044300080710     c                   parm                    lenght
045401081015      * pulisco il file in gaitraazm o gaitraazp
045402081015     c                   clear                   comman
045403081015     c                   EVAL      comman = 'CLRPFM FILE('
045404081015     c                             + %TRIMR(WLIB)
045405081015     c                             + '/WFSPE10F)'
045406081015
045407081015     c                   call      'QCMDEXC'
045408081015     c                   parm                    comman
045409081015     c                   parm                    lenght
046100080710
046200081014     c                   open      wfspe00f
046300081014     c                   open      wfspe10f
046400060320
046500060320      * reperisco data e ora del lancio
046600060320     c                   time                    w0140
046700060320     c                   move      w0140         wdata
046800061002     c                   movel     w0140         wora
046801081015
046802081015      * giro le date per la stampa finale
046803081015      *
046804081015     c                   clear                   wlbdat
046805081015     c                   z-add     pardin_1      g02inv
046806081015     c                   eval      g02err = '3'
046807081015     c                   call      'XSRDA8'
046808081015     c                   parm                    wlbdat
046809081015     c                   eval      stpdin1 = g02dat
046810081015      *
046811081015     c                   clear                   wlbdat
046812081015     c                   z-add     pardin_2      g02inv
046813081015     c                   eval      g02err = '3'
046814081015     c                   call      'XSRDA8'
046815081015     c                   parm                    wlbdat
046816081015     c                   eval      stpdin2 = g02dat
046817081015      *
046818081015     c                   clear                   wlbdat
046819081015     c                   z-add     pardfi_1      g02inv
046820081015     c                   eval      g02err = '3'
046821081015     c                   call      'XSRDA8'
046822081015     c                   parm                    wlbdat
046823081015     c                   eval      stpdfi1 = g02dat
046824081015      *
046825081015     c                   clear                   wlbdat
046826081015     c                   z-add     pardfi_2      g02inv
046827081015     c                   eval      g02err = '3'
046828081015     c                   call      'XSRDA8'
046829081015     c                   parm                    wlbdat
046830081015     c                   eval      stpdfi2 = g02dat
046900060321
047000081014
047100081014     c     ktas02        klist
047200081014     c                   kfld                    Annoin
047300081014     c                   kfld                    mesgioin
047400060320
047500081014     c     kspe02        klist
047600081014     c                   kfld                    Spedsp
047700081014     c                   kfld                    Spekun
047800081014
047900081014     c     ktabel        klist
048000081014     c                   kfld                    codut
048100081014     c                   kfld                    kcod
048200081014     c                   kfld                    kkey
048201081015
048202081015     c     ktabel02      klist
048203081015     c                   kfld                    codut
048204081015     c                   kfld                    kcod
048300060320
048400060320     c                   endsr
048500060320      *--------------------------------------------------------------*
