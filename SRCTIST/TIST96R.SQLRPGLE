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
001100130902     fAZCMM01L  if   e           k disk
001200081014     fwfspe00f  uf a e           k disk    usropn
001300081014     fwfspe10f  uf a e           k disk    usropn
001400081015     ftist96p   o    e             printer oflind(*in90)
001500060323      *------------------------------------------------------------------------*
001600060323      *  RIEPILOGO INDICATORI
001700060323      *------------------------------------------------------------------------*
001800060323      * 30 - comodo
001900060215
002000060215      *------------------------------------------------------------------------*
002100060215      *   V A R I A B I L I
002200060215      *------------------------------------------------------------------------*
002300080710     d comman          s            110
002400060320     d codut           s              1  0 inz(1)
002500081014     d annoin          s                   like(tasaas)
002600081014     d mesgioin        s                   like(tasmgs)
002700081014     d annofi          s                   like(tasaas)
002800081014     d mesgiofi        s                   like(tasmgs)
002900081014     d save_ksc        s                   like(tasksc)
003000081014     d w0030           s              3  0
003100081014     d wdata           s              8  0
003200081014     d wora            s              6  0
003300081015     d z               s              3  0
003400081014
003500060320     d kcod            s                   like(tblcod)
003600060320     d kkey            s                   like(tblkey)
003700080710     d lenght          s             15  5 inz(110)
003800060320     d wlib            s              9
003900060320     d w0140           s             14  0
004000060320
004100060320      *------------------------------------------------------------------------*
004200060320      *   S C H I E R E
004300060320      *------------------------------------------------------------------------*
004400060215
004500081015      * Tipi bolla da considerare in questa statistica
004600081015     d TBO_OK          S              2    DIM(50)                              Tipi bolla
004700081015
004800060215      *------------------------------------------------------------------------*
004900060215      *   D S   I N T E R N E / E S T E R N E
005000060215      *------------------------------------------------------------------------*
005100060320
005200060320     d parm01          ds
005300081014     d  pardin_1               1      8  0
005400081014     d  pardfi_1               9     16  0
005500081014     d  pardin_2              17     24  0
005600081014     d  pardfi_2              25     32  0
005700060321
005800060321     d wlbdat          ds                  inz
005900060321     d  g02dat                 1      8  0
006000060321     d  g02inv                 9     16  0
006100060321     d  g02err                17     17
006200060321     d  g02tgi                18     22  0
006300081014
006400081014     d DS_SPE1_TOTU    DS
006500081014     d  tot1_kun                      7  0
006600081014     d  tot1_kund                    48
006700081014     d  tot1_clv                      1
006800081014     d  tot1_car                      3  0
006900081014     d  tot1_card                    25
007000081014     d  tot1_fil                      3  0
007100081014     d  tot1_fild                    20
007200081014     d  tot1_cmu                      7  0
007300081014     d  tot1_cmud                    25
007400081014     d  tot1_spe                     13  0
007500081014     d  tot1_col                     13  0
007600081014     d  tot1_pes                     13  1
007700081014
007800060320
007900060320     d azuteds       e ds                  extname(azute00f)
008000060320     d ddatiute      e ds
008100060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
008200060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
008300060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
008400060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
008500060215     d kpjba         e ds
008600081014     d tibs10ds      e ds
008700060320     d tibs34ds      e ds
008800060320     d tibs69ds      e ds
008900060320     d trul31ds      e ds                  inz
009000081015      ** Tipi bolla
009100081015     D DSTB          E DS
009200060215
009300060215      *------------------------------------------------------------------------*
009400080707     C*
009500080707     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni
009600080707     C
009700080707     C/EXEC SQL
009800080707     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009900080707     C/END-EXEC
010000080707     C*
010100060215
010200081015      * carico i tipi bolla da prendere in considerazione
010300081015     c                   exsr      Carcbo
010400081014      * ciclo per data periodo numero 1
010500060215
010600081014     c                   movel     pardin_1      annoin
010700081014     c                   move      pardin_1      mesgioin
010800081014     c                   movel     pardfi_1      annofi
010900081014     c                   move      pardfi_1      mesgiofi
011000081014
011100081014     c                   exsr      ela_tas
011200081014
011300081014      * ciclo per data periodo numero 2
011400081014
011500081014     c                   movel     pardin_2      annoin
011600081014     c                   move      pardin_2      mesgioin
011700081014     c                   movel     pardfi_2      annofi
011800081014     c                   move      pardfi_2      mesgiofi
011900081014
012000081014     c                   exsr      ela_tas
012100081014
012200081014      * Elaboro i totali
012300081014     c                   exsr      ela_tot
012400081015
012500081015      * stampo  la fine elaborazione
012600081015     c                   write     st96s1
012700081014
012800060215     c                   eval      *inlr = *on
012900060215
013000081014      *------------------------------------------------------------------------*
013100081014      * ELABORO DETTAGLIO DELLE BOLLE DI SEDE
013200081014      *------------------------------------------------------------------------*
013300081014     c     ela_tas       begsr
013400081014
013500081014     c     Ktas02        setll     titas38c
013600081014
013700081014     c                   do        *hival
013800081014     c                   read      titas38c
013900081014
014000081014     c                   if        %eof(titas38c)
014100081014     c                   leave
014200081014     c                   endif
014300081014      * verifico data se fine periodo
014400081014      * se anno diverso vado a fine
014500081014     c                   if        tasaas <> annofi
014600081014     c                   leave
014700081014     c                   endif
014800081014
014900081014      * se mese giorno del file maggiore del finale vado a fine
015000081014     c                   if        tasmgs > mesgiofi
015100081014     c                   leave
015200081014     c                   endif
015300081014
015400081015      * verifico il tipo bolla se non è ne F1 nè A2 nè AS nè FS leggo il successivo
015500081015     c     tastbl        lookup    tbo_ok                                 30
015600081015     c                   if        not %found
015700081015     c***                if        tastbl <> 'F1' and tastbl <> 'A2' and
015800081015     c***                          tastbl <> 'AS' and 'FS'
015900081014     c                   iter
016000081014     c                   endif
016100081014
016200081014      * recupero per ogni cliente l'unificante se diverso dal precedente
016300081014     c                   if        tasksc <> Save_ksc
016400081014     c                   eval      save_ksc = tasksc
016500081014      * ricerco unificante / categoria / commerciale ecc ecc
016600081014     c                   exsr      Ric_unificanti
016700081014     c                   endif
016800081014
016900081014      * scrivo il record
017000081014     c                   exsr      Write_dett
017100081014
017200081014     c                   enddo
017300081014
017400081014     c                   endsr
017500060215      *------------------------------------------------------------------------*
017600081014      * SCRIVO O AGGIORNO IL RECORD DI DETTAGLIO
017700060215      *------------------------------------------------------------------------*
017800081014     c     Write_dett    begsr
017900060215
018000081014      * aggancio il record
018100081014     c                   eval      spedsp = (tasaas * 10000) + tasmgs
018200081014     c     Kspe02        chain     Wfspe00f
018300081014
018400081014     c                   add       1             spenspe
018500081014     c                   add       tasncl        spencol
018600081015      * peso prendo il + grande
018700081015     c                   if        taspkc > 0 and tasncl = tasncp
018800081015     c                   add       taspkc        spepeso
018900081015     c                   else
019000081015     c                   if        taspkf > taspkc
019100081014     c                   add       taspkf        spepeso
019200081015     c                   else
019300081015     c                   add       taspkc        spepeso
019400081015     c                   endif
019500081015     c                   endif
019600081015
019700081014     c                   if        %found(wfspe00f)
019800081014     c                   update    wfspe000
019900081014     c                   else
020000081014     c                   write     wfspe000
020100081014     c                   endif
020200081014
020300081014     c                   feod      wfspe00f
020400060215
020500060215     c                   endsr
020600081014      *------------------------------------------------------------------------*
020700081014      * CERCO CLIENTE E COMMERCIALE UNIFICANTE
020800081014      *------------------------------------------------------------------------*
020900081014     c     Ric_unificantibegsr
021000081014
021100081014     c                   clear                   wfspe000
021200081014      * cerco il codice cliente unificante
021300081014     c                   clear                   tibs10ds
021400081014     c                   eval      d10tle = 'ST'
021500081014     c                   eval      d10paf = 'P'
021600081014     c                   z-add     tasksc        d10cod
021700081014     c                   z-add     *date         d10drf
021800081014     c                   CALL      'TIBS10R'
021900081014     c                   parm                    tibs10ds
022000081014     c                   parm      'A'           partpexe          1
022100081014      * se ci sono errori valorizzo come unificante il cliente
022200081014     c                   if        d10err <> ' '
022300081014     c                   eval      spekun = tasksc
022400081014      *
022500081014     c                   else
022600081014      *
022700081014     c                   eval      spekun = d10cop
022800081014     c                   endif
022900081014      * cerco i dati anagrafici
023000081014     c                   clear                   tibs69ds
023100081014     c                   move      spekun        i69kac
023200081014     c                   move      spekun        i69kcp
023300081014     c                   call      'TIBS69R'
023400081014     c                   parm                    tibs69ds
023500081014     c                   parm                    ds_cnaco
023600081014     c                   parm                    ds_cnind
023700081014     c                   parm                    ds_cnclp
023800081014     c                   parm                    ds_fncls
023900081014      *
024000081014     c                   eval      spekund = acorag
024100081014     c                   eval      speclv  = clpclv
024200130902      *
024300081014      * verifico il codice commerciale unificante
024400130902     c     CLPage        chain     AZCMM000
024500130902     c                   if        not %found(AZCMM01L)
024600081014     c                   eval      specmu  = clpage
024700081014     c                   eval      specmud = *all'*'
024800081014     c                   else
024900130902     c                   eval      specmu  = CMMuni
025000130902     c     SPEcmu        chain     AZCMM000
025100130902     c                   if        not %found(AZCMM01L)
025200081014     c                   eval      specmud = *all'*'
025300081014     c                   else
025400130902     c                   eval      specmud = CMMdes
025500081014     c                   endif
025600081014     c                   endif
025700081014
025800081014      * albero commerciale unificante
025900081014     c                   movel     specmu        w0030
026000081014     c     w0030         chain     azorg01l
026100081014
026200081014      * area
026300081014     c                   eval      specar = orgcar
026400081014     c                   eval      kcod = '05'
026500081014     c                   movel(p)  specar        kkey
026600081014     c     ktabel        chain     tabel00f
026700081014     c                   if        not %found(tabel00f) or tblflg <> *blanks
026800081014     c                   eval      specard = *all'*'
026900081014     c                   else
027000081014     c                   eval      specard = tbluni
027100081014     c                   endif
027200081014      * p.o.
027300081014     c                   eval      spefil = orgfil
027400081014     c                   if        not %found(azorg01l) or orgfva <> *blanks
027500081014     c                   eval      spefild = *all'*'
027600081014     c                   else
027700081014     c                   eval      spefild = orgdes
027800081014     c                   endif
027900081014
028000081014     c                   endsr
028100060320
028200081014      *------------------------------------------------------------------------*
028300081014      * ELABORO I TOTALI
028400081014      *------------------------------------------------------------------------*
028500081014     c     ela_tot       begsr
028600081014
028700081014      * primo giro con il periodo precedente
028800081014
028900081014     c/EXEC SQL
029000081014     C+ declare C1 cursor for
029100081014     C+ select spekun, spekund, speclv, specar, specard, spefil,
029200081014     C+ spefild, specmu, specmud,
029300081014     C+ decimal(sum(spenspe), 13, 0),
029400081014     C+ decimal(sum(spencol), 13, 0), decimal(sum(spepeso), 13, 1)
029500081014     C+ from wfspe00f
029600081014     C+ where spedsp between :pardin_1 and :pardfi_1
029700081014     C+ group by
029800081014     C+ spekun, spekund, speclv, specar, specard, spefil,
029900081014     C+ spefild, specmu, specmud
030000081014     C+ for read only
030100081014
030200081014     C/END-EXEC
030300081014     C
030400081014     C/EXEC SQL
030500081014     C+ open C1
030600081014     C/END-EXEC
030700081014     C
030800081014     C/EXEC SQL
030900081014     C+ Fetch next from C1 into :DS_SPE1_TOTU
031000081014     C/END-EXEC
031100081014     C
031200081014
031300081014     c                   dow       sqlcod = *zeros
031400081014      *
031500081014     c                   clear                   wfspe100
031600081014     c                   eval      sp1kun = tot1_kun
031700081014     c                   eval      sp1kund = tot1_kund
031800081014     c                   eval      sp1clv = tot1_clv
031900081014     c                   eval      sp1car = tot1_car
032000081014     c                   eval      sp1card = tot1_card
032100081014     c                   eval      sp1fil = tot1_fil
032200081014     c                   eval      sp1fild = tot1_fild
032300081014     c                   eval      sp1cmu = tot1_cmu
032400081014     c                   eval      sp1cmud = tot1_cmud
032500081014     c                   eval      sp1nspe1 = tot1_spe
032600081014     c                   eval      sp1ncol1 = tot1_col
032700081014     c                   eval      sp1peso1 = tot1_pes
032800081014     c                   write     wfspe100
032900081014     C/EXEC SQL
033000081014     C+ Fetch next from C1 into :DS_SPE1_TOTU
033100081014     C/END-EXEC
033200081014     C
033300081014     c                   enddo
033400081014     C/EXEC SQL
033500081014     C+ close C1
033600081014     C/END-EXEC
033700081014
033800081014     C                   FEOD      WFSPE10F
033900081014      * secondo giro con il secondo periodo
034000081014
034100081014     c/EXEC SQL
034200081014     C+ declare C2 cursor for
034300081014     C+ select spekun, spekund, speclv, specar, specard, spefil,
034400081014     C+ spefild, specmu, specmud,
034500081014     C+ decimal(sum(spenspe), 13, 0),
034600081014     C+ decimal(sum(spencol), 13, 0), decimal(sum(spepeso), 13, 1)
034700081014     C+ from wfspe00f
034800081014     C+ where spedsp between :pardin_2 and :pardfi_2
034900081014     C+ group by
035000081014     C+ spekun, spekund, speclv, specar, specard, spefil,
035100081014     C+ spefild, specmu, specmud
035200081014     C+ for read only
035300081014
035400081014     C/END-EXEC
035500081014     C
035600081014     C/EXEC SQL
035700081014     C+ open C2
035800081014     C/END-EXEC
035900081014     C
036000081014     C/EXEC SQL
036100081014     C+ Fetch next from C2 into :DS_SPE1_TOTU
036200081014     C/END-EXEC
036300081014     C
036400081014
036500081014     c                   dow       sqlcod = *zeros
036600081014      *
036700081014     c     tot1_kun      chain     wfspe10f
036800081014     c
036900081014     c                   If        not %found(wfspe10f)
037000081014     c                   clear                   wfspe100
037100081014     c                   eval      sp1kun = tot1_kun
037200081014     c                   eval      sp1kund = tot1_kund
037300081014     c                   eval      sp1clv = tot1_clv
037400081014     c                   eval      sp1car = tot1_car
037500081014     c                   eval      sp1card = tot1_card
037600081014     c                   eval      sp1fil = tot1_fil
037700081014     c                   eval      sp1fild = tot1_fild
037800081014     c                   eval      sp1cmu = tot1_cmu
037900081014     c                   eval      sp1cmud = tot1_cmud
038000081014     c                   endif
038100081014
038200081014     c                   eval      sp1nspe2 = tot1_spe
038300081014     c                   eval      sp1ncol2 = tot1_col
038400081014     c                   eval      sp1peso2 = tot1_pes
038500081014     c                   If        not %found(wfspe10f)
038600081014     c                   write     wfspe100
038700081014     c                   else
038800081014     c                   update    wfspe100
038900081014     c                   endif
039000081014     C/EXEC SQL
039100081014     C+ Fetch next from C2 into :DS_SPE1_TOTU
039200081014     C/END-EXEC
039300081014     C
039400081014     c                   enddo
039500081014     C/EXEC SQL
039600081014     C+ close C2
039700081014     C/END-EXEC
039800081014     c                   endsr
039900081015      *---------------------------------------------------------------*
040000081015      ** CARICAMENTO codici bolla da cosniderare in questa statistica
040100081015      *---------------------------------------------------------------*
040200081015     c     Carcbo        Begsr
040300081015      **
040400081015     c                   Movel     'TB'          KCod
040500081015     c                   z-add     0             z
040600081015     c     Ktabel02      setll     tabel00f
040700081015     c                   do        *hival
040800081015     c     Ktabel02      reade     tabel00f
040900081015      * fine file
041000081015     c                   If        %eof(tabel00f)
041100081015     c                   leave
041200081015     c                   endif
041300081015    1
041400081015    2c                   If        tblkey <> *blanks
041500081015     c                   Movel     TBLuni        DStb
041600081015      * normali e reso
041700081015    3c                   If        §tbrbl  = 'S' or §tbrbl = 'N'
041800081015     c                   add       1             z
041900081015     c                   movel     TBLkey        tbo_oK(z)
042000081015    3c                   endif
042100081015      **
042200081015    2c                   endif
042300081015    1c                   enddo
042400081015
042500081015      **
042600081015
042700081015     c                   Endsr
042800060320      *--------------------------------------------------------------*
042900060320      * ROUTINE INIZIALE
043000060320      *--------------------------------------------------------------*
043100060320     c     *inzsr        begsr
043200060320
043300060320     c     *entry        plist
043400060320     c                   parm                    kpjba
043500060320     c                   movel     kpjbu         parm01
043600060320
043700060320     c     *dtaara       define    §azute        azuteds
043800060320     c     *dtaara       define    §datiute      ddatiute
043900060320
044000060320     c                   in(e)     *dtaara
044100060320     c                   if        %error or rsut = *blanks
044200060320     c                   clear                   tibs34ds
044300060320     c                   call      'TIBS34R'
044400060320     c                   parm                    tibs34ds
044500060320     c                   in        *dtaara
044600060320     c                   endif
044700060320
044800060320
044900060320      * se s.i. di prova imposto la libreria di prova
045000060320     c                   if        %subst(knsif:7:1) = 'P'
045100060320     c                   eval      wlib = 'GAITRAAZP '
045200060320     c                   else
045300060320     c                   eval      wlib = 'GAITRAAZM '
045400060320     c                   endif
045500081015      * pulisco il file in gaitraazm o gaitraazp
045600080710     c                   clear                   comman
045700081015     c                   EVAL      comman = 'CLRPFM FILE('
045800081015     c                             + %TRIMR(WLIB)
045900081015     c                             + '/WFSPE00F)'
046000080710
046100080710     c                   call      'QCMDEXC'
046200080710     c                   parm                    comman
046300080710     c                   parm                    lenght
046400081015      * pulisco il file in gaitraazm o gaitraazp
046500081015     c                   clear                   comman
046600081015     c                   EVAL      comman = 'CLRPFM FILE('
046700081015     c                             + %TRIMR(WLIB)
046800081015     c                             + '/WFSPE10F)'
046900081015
047000081015     c                   call      'QCMDEXC'
047100081015     c                   parm                    comman
047200081015     c                   parm                    lenght
047300080710
047400081014     c                   open      wfspe00f
047500081014     c                   open      wfspe10f
047600060320
047700060320      * reperisco data e ora del lancio
047800060320     c                   time                    w0140
047900060320     c                   move      w0140         wdata
048000061002     c                   movel     w0140         wora
048100081015
048200081015      * giro le date per la stampa finale
048300081015      *
048400081015     c                   clear                   wlbdat
048500081015     c                   z-add     pardin_1      g02inv
048600081015     c                   eval      g02err = '3'
048700081015     c                   call      'XSRDA8'
048800081015     c                   parm                    wlbdat
048900081015     c                   eval      stpdin1 = g02dat
049000081015      *
049100081015     c                   clear                   wlbdat
049200081015     c                   z-add     pardin_2      g02inv
049300081015     c                   eval      g02err = '3'
049400081015     c                   call      'XSRDA8'
049500081015     c                   parm                    wlbdat
049600081015     c                   eval      stpdin2 = g02dat
049700081015      *
049800081015     c                   clear                   wlbdat
049900081015     c                   z-add     pardfi_1      g02inv
050000081015     c                   eval      g02err = '3'
050100081015     c                   call      'XSRDA8'
050200081015     c                   parm                    wlbdat
050300081015     c                   eval      stpdfi1 = g02dat
050400081015      *
050500081015     c                   clear                   wlbdat
050600081015     c                   z-add     pardfi_2      g02inv
050700081015     c                   eval      g02err = '3'
050800081015     c                   call      'XSRDA8'
050900081015     c                   parm                    wlbdat
051000081015     c                   eval      stpdfi2 = g02dat
051100060321
051200081014
051300081014     c     ktas02        klist
051400081014     c                   kfld                    Annoin
051500081014     c                   kfld                    mesgioin
051600060320
051700081014     c     kspe02        klist
051800081014     c                   kfld                    Spedsp
051900081014     c                   kfld                    Spekun
052000081014
052100081014     c     ktabel        klist
052200081014     c                   kfld                    codut
052300081014     c                   kfld                    kcod
052400081014     c                   kfld                    kkey
052500081015
052600081015     c     ktabel02      klist
052700081015     c                   kfld                    codut
052800081015     c                   kfld                    kcod
052900060320
053000060320     c                   endsr
053100060320      *--------------------------------------------------------------*
