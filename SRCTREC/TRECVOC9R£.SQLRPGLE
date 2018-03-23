000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000101160823     ftabel00f  if   e           k disk
000102160823     fwffss01l  uf a e           k disk    usropn
000103160823     d titasds       e ds                  inz extname(titas10f)
000104160823     d kpjba         e ds
000105160823     d tibs10ds      e ds
000107160823     d ds$3          e ds
000108160823      //-----------------------------------------------------------------------
000109160823      //? VARIABILI
000110160823      //-----------------------------------------------------------------------
000111160823     d kcod            s                   like(tblcod)
000112160823     d kkey            s                   like(tblkey)
000113160823     d codut           s              1  0 inz(1)
000114160823     d lengh           s             15  5 inz(80)
000115160823     d pdataal         s             10
000116160823     d pdatada         s             10
000117160823     d wdataspe        s                   like(VOCPdal)
000118160823     d wdate           s                   like(VOCPal)
000119160823     d wtime           s             14  0
000120160823      //-----------------------------------------------------------------------
000121160823      //? STRUTTURE DATI
000122160823      //-----------------------------------------------------------------------
000123160823     d param           ds
000124160823     d  waada                  1      4  0
000125160823     d  wmgda                  5      8  0
000126160823     d  wmmda                  5      6  0
000127160823     d  wggda                  7      8  0
000128160823     d  wdatada                1      8  0
000129160823     d  da                     1      8
000130160823     d  waaal                  9     12  0
000131160823     d  wmmal                 13     14  0
000132160823     d  wggal                 15     16  0
000133160823     d  wdataal                9     16  0
000134160823     d  a                      9     16
000135160823     d  anno                  17     20  0
000136160823     d  datarif               21     28  0
000137160823      //-----------------------------------------------------------------------
000138160823      //? SCHIERE
000139160823      //-----------------------------------------------------------------------
000140160823     d cmd             s             80    dim(1) ctdata perrcd(1)
000141160823      //-----------------------------------------------------------------------
023400030414     c                   seton                                        lr
023401160823      * clrpfm del file di work
023402160823     c                   call(e)   'QCMDEXC'
023403160823     c                   parm                    cmd(1)
023404160823     c                   parm                    lengh
023405160823      * se riuscito comando vado avanti con l'elaborazione
023406160823     c                   if        %error
023407160823     c                   return
023408160823     c                   end
023409160823     c                   open      wffss01l
023410160823     C/EXEC SQL
023411160823     C+ with
023412160823     C+ eccet (pdr, ddc, b001, b005) AS (
023413160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
023414160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 then CETPAR else 0 end
023415160823     C+ as a , case when ftdtsr = 'R' then cetard else 0 end as p FROM
023416160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023417160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023418160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023419160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
023420160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
023421160823     C+ tabella GROUP BY ftdpdr, ftdddc),
023422160823
023423160823     C+ ecced (pdr, ddc, b016, b019, b034, b035, b024, b043062) AS (
023424160823     C+ SELECT FTDPDR, FTDDDC, sum(x016) , sum(x019),
023425160823     C+ sum(x034), sum(x035),
023426160823     C+ sum(x024), sum(x043+x062)
023428160823     C+ from ( SELECT FTDPDR, ftdddc,
023429160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 16
023430160823     C+ then CETimp else 0 end as x016,
023431160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 19
023432160823     C+ then CETimp else 0 end as x019,
023433160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 34
023434160823     C+ then CETimp else 0 end as x034,
023435160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 43
023436160823     C+ then CETimp else 0 end as x043,
023437160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 62
023438160823     C+ then CETimp else 0 end as x062,
023439160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 24
023440160823     C+ then CETimp else 0 end as x024,
023441160823     C+ case when ftdtsr = 'R' and cedcmp = 35
023442160823     C+ then CETimp else 0 end as x035
023443160823     C+ from
023444160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023445160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023446160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023447160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
023448160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
023449160823     C+ tabella GROUP BY ftdpdr, ftdddc),
023450160823
023451160823     C+ fifss (fifss00f.*) as (
023453160823     C+ select fifss00f.* from ( SELECT ftdpdr, ftdddc FROM titas10f join
023455160823     C+ fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and tasnrs = ftdnrs
023456160823     C+ and tasnsp = ftdnsp join eccet00f on ftdaas = cetaas and ftdlnp =
023457160823     C+ cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp WHERE FTDCMC = ' '
023458160823     C+ and FTDSIC = ' ' and tasaas = 2016 and tasmgs between 0201 and 0229
023459160823     C+ and taspdc = ftdpdr and tasddc = ftdddc GROUP BY ftdpdr, ftdddc )
023460160823     C+ as tabella join fifss00f on fsspdr = ftdpdr and fssddc = ftdddc)
023461160823
023462160823     C+ SELECT * FROM eccet
023463160823     C+ UNION
023464160823     C+ SELECT * FROM ecced
023465160823     C+ UNION
023466160823     C+ SELECT * FROM fifss
023468160823     C/END-EXEC
023469160823
023470160823
023471160823
023472160823
023473160823
023474160823
023475160823
023476160823
023477160823
023478160823
023479160823
023480160823     c* tasp o tas10 COMPETENZE PARTENZA o ARRIVO
023481160823     c                   if        §$3D10>=wdataal
023482160823     C/EXEC SQL
023483160823     C+ insert INTO WFFSS00F (fsspdc, fssddc, fss001b, fss005b)
023484160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
023485160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 then CETPAR else 0 end
023486160823     C+ as a , case when ftdtsr = 'R' then cetard else 0 end as p FROM
023487160823     C+ titasp0f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023488160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023489160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023490160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
023491160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
023492160823     C+ tabella GROUP BY ftdpdr, ftdddc
023493160823     C/END-EXEC
023494160823     c                   else
023495160823     C+ insert INTO WFFSS00F (fsspdc, fssddc, fss001b, fss005b)
023496160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
023497160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 then CETPAR else 0 end
023498160823     C+ as a , case when ftdtsr = 'R' then cetard else 0 end as p FROM
023499160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023500160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023501160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023502160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
023503160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
023504160823     C+ tabella GROUP BY ftdpdr, ftdddc
023505160823     c                   end
023506160823     c* tasp o tas10 COMPETENZE DETTAGLIO
023507160823     c                   if        §$3D10>=wdataal
023508160823     C/EXEC SQL
023509160823     C+ DECLARE B1 CURSOR FOR
023510160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
023511160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 then CETPAR else 0 end
023512160823     C+ as a , case when ftdtsr = 'R' then cetard else 0 end as p FROM
023513160823     C+ titasp0f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023514160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023515160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023516160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
023517160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
023518160823     C+ tabella GROUP BY ftdpdr, ftdddc
023519160823     C/END-EXEC
023520160823
023521160823     C/EXEC SQL
023522160823     C+ OPEN b1
023523160823     C/END-EXEC
023524160823     c                   else
023525160823     C/EXEC SQL
023526160823     C+ DECLARE B2 CURSOR FOR
023527160823     C+ SELECT FIFTD00F.FTDPDR, FIFTD00F.FTDDDC, sum(ECCET00F.CETPAR) FROM
023528160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
023529160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
023530160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
023531160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and FTDDCM<>0 and tasaas = :aas
023532160823     C+ and tasmgs between :da  and :a   and ftdtsr = 'C' and taspdc =
023533160823     C+ ftdpdr and tasddc = ftdddc GROUP BY ftdpdr, ftdddc
023534160823     C/END-EXEC
023535160823
023536160823     C/EXEC SQL
023537160823     C+ OPEN b2
023538160823     C/END-EXEC
023539160823     c                   end
023540160823
023541160823     C                   DO        *HIVAL
023542160823
023543160823     c* tasp o tas0
023544160823     c                   if        §$3D10>=wdataal
023545160823     C/EXEC SQL
023546160823     C+ FETCH NEXT FROM b1 INTO :titasds
023547160823     C/END-EXEC
023548160823     c                   else
023549160823     C/EXEC SQL
023550160823     C+ FETCH NEXT FROM b2 INTO :titasds
023551160823     C/END-EXEC
023552160823     c                   end
023553160823
023554160823     C                   IF        SQLCod = 100 or sqlcod < 0
023555160823     c                   clear                   sqlcod
023556160823     C                   LEAVE
023557160823     C                   ENDIF
023558160823     c*
023559160823     c     kcev          chain     eccev30c
023560160823     c                   if        not %found(eccev30c)
023561160823     c                   iter
023562160823     c                   end
023563160823     c     kcev          chain     ecc2v30c
023564160823     C                   CLEAR                   IMP
023565160823
023566160823     c                   EXSR      conta
023567160823     c                   if        %found(ecc2v30c)
023568160823     c                   EXSR      conta2
023569160823     C                   END
023570160823      *       scrivo nuovo record
023571160823     c                   exsr      sr_scrivi
023572160823     C                   ENDDO
023573160823
023574160823     c* tasp o tas0
023575160823     c                   if        §$3D10>=wdataal
023576160823     C/EXEC SQL
023577160823     C+ CLOSE b1
023578160823     C/END-EXEC
023579160823     c                   else
023580160823     C/EXEC SQL
023581160823     C+ CLOSE b2
023582160823     C/END-EXEC
023583160823     c                   end
023584160823     C*--------------------------------------------------------------- ---------
023585160823     C     conta         BEGSR
023586160823     C*--------------------------------------------------------------- ---------
031900031107      *  se è un solo incasso o una parziale
032000031107      *   non deve avere le competenze
032200031107     c                   if        ftdSIC = 'S' or ftdCMC = 'P  '
032300031107     c                   move      'S'           Sinc_Parz
032400031107     c                   end
037500030415      * solo per le prestazioni di consegna, se data consegna a zero,
037600030414      *  non sommo le competenze. Per i Ritiri invece sempre.
037700030414     c                   move      'S'           Calc_Cmp
037800030414      *
037900030414     c                   if        Data_Cons = 0 and fcetsr='C'
038400030414     c                   move      'N'           Calc_Cmp
038500030414     c                   end
038600030414      *
038700031107      * Attenzione: se è una bolla di solo incasso o di consegna parziale
038800031107      *  non deve prendere le competenze:
038900031107     c                   if        Sinc_parz ='S'
039000031107     c                   move      'N'           Calc_Cmp
039100031107     c                   end
039200031107      *
039300030414      * ------------------ * * ------------------ * * ------------------ *
039400030415      *  Quindi solo se deve calcolare le competenze
039500030415     c                   if        Calc_Cmp = 'S'
039600030415      *
039700030414      *  Cerca record su CE
039800030414     c     kcet          chain     eccet30c
0451000304153    c                   when      fcetsr = 'R'
045200030414     c                   add       cetpar        savfss001b
045300060220     c                   add       cetpar        tot_CHK001B
045400060220     c                   add       cetpar        tot_CHKTOTB
0455000304153    c                   when      fcetsr = 'C'
045600030414     c                   add       cetard        savfss005b
045700060220     c                   add       cetard        tot_CHK005B
045800060220     c                   add       cetard        tot_CHKTOTB
0459000304153-   c                   endsl
046000030414      *
046100030414      *  quindi cerca le competenze di dettaglio se ci sono e le somma
0462000304143    c                   if        cetdet = 'S'
046300030414     c                   exsr      somma_Compe
0464000304143-   c                   endif
046500030414      *
046600060220      *  Controllo su filiali in esame: carica un work file x bolla/data
048400020521     c     kced          setll     ecced30c
048500030414     c     kced          reade     ecced30c
048600030414      *
048700030414     c                   dow       not %Eof(ecced30c)
048800030414      *
048900020521     c                   select
049000030415     c                   when      cedcmp = 16 and fcetsr = 'C'
049100030414     c                   add       cedimp        savfss016b
049200060220     c                   add       cedimp        tot_CHK016B
049300060220     c                   add       cedimp        tot_CHKTOTB
049400030414      *
049500030415     c                   when      cedcmp = 19 and fcetsr = 'C'
049600030414     c                   add       cedimp        savfss019b
049700060220     c                   add       cedimp        tot_CHK019B
049800060220     c                   add       cedimp        tot_CHKTOTB
049900030414      *
050000030415     c                   when      cedcmp = 34 and fcetsr = 'C'
050100030414     c                   add       cedimp        savfss034b
050200060220     c                   add       cedimp        tot_CHK034B
050300060220     c                   add       cedimp        tot_CHKTOTB
050400030414      *
050500030415     c                   when      cedcmp = 35 and fcetsr = 'R'
050600030414     c                   add       cedimp        savfss035b
050700060220     c                   add       cedimp        tot_CHK035B
050800060220     c                   add       cedimp        tot_CHKTOTB
050900030430      *
051000030430     c                   when      cedcmp = 24 and fcetsr = 'C'
051100030430     c                   add       cedimp        savfssxx1b
051200060220     c                   add       cedimp        tot_CHKXX1B
051300060220     c                   add       cedimp        tot_CHKTOTB
051400030430      *
051500030430     c                   when      cedcmp = 43 and fcetsr = 'C'
051600030430     c                   add       cedimp        savfssxx2b
051700060220     c                   add       cedimp        tot_CHKXX2B
051800060220     c                   add       cedimp        tot_CHKTOTB
051900030430      *
052000030430     c                   when      cedcmp = 62 and fcetsr = 'C'
052100030430     c                   add       cedimp        savfssxx2b
052200060220     c                   add       cedimp        tot_CHKXX2B
052300060220     c                   add       cedimp        tot_CHKTOTB
052400030430      *
052500020521     c                   endsl
052600030414      *
052700030414     c     kced          reade     ecced30c
052800020521     c                   enddo
053500030414      *
054700090925     c                   z-add(h)  tot_CHK001B   CHK001B
054800090925     c                   z-add(h)  tot_CHK005B   CHK005B
054900090925     c                   z-add(h)  tot_CHK016B   CHK016B
055000090925     c                   z-add(h)  tot_CHK019B   CHK019B
055100090925     c                   z-add(h)  tot_CHK034B   CHK034B
055200090925     c                   z-add(h)  tot_CHK035B   CHK035B
055300090925     c                   z-add(h)  tot_CHK997B   CHK997B
055400090925     c                   z-add(h)  tot_CHK998B   CHK998B
055500090925     c                   z-add(h)  tot_CHKXX1B   CHKXX1B
055600090925     c                   z-add(h)  tot_CHKXX2B   CHKXX2B
055700090925     c                   z-add(h)  tot_CHKTOTB   CHKTOTB
062200030414      *sommo comunque i costi da FCE e le Competenze
062400030414     c                   exsr      Compe_in_Fss
062500030414      *
062600030414     c                   update    fifss000
062700030414     c                   endif
062800030414      *
062900030414     c                   endsr
065000030414     c*==================================================================*
065100030414      *  Imposta i valori Competenze
065200030414     c*==================================================================*
065300030414     c     Compe_in_Fss  begsr
065400090925     c                   z-add(h)  savfss001b    savfss001£
065500090925     c                   add       savfss001£    fss001b
065600090925     c                   z-add(h)  savfss005b    savfss005£
065700090925     c                   add       savfss005£    fss005b
065800090925     c                   z-add(h)  savfss016b    savfss016£
065900090925     c                   add       savfss016£    fss016b
066000090925     c                   z-add(h)  savfss019b    savfss019£
066100090925     c                   add       savfss019£    fss019b
066200090925     c                   z-add(h)  savfss034b    savfss034£
066300090925     c                   add       savfss034£    fss034b
066400090925     c                   z-add(h)  savfss035b    savfss035£
066500090925     c                   add       savfss035£    fss035b
066600090925     c                   z-add(h)  savfssXX1b    savfssXX1£
066700090925     c                   add       savfssXX1£    fssXX1b
066800090925     c                   z-add(h)  savfssXX2b    savfssXX2£
066900090925     c                   add       savfssXX2£    fssXX2b
067000030414     c                   endsr
067100160823      //-----------------------------------------------------------------------
067200160823      //? Routine iniziale
067300160823      //-----------------------------------------------------------------------
067400160823     c     *inzsr        begsr
067500160823
067600160823     c     *entry        plist
067700160823     c                   parm                    kpjba
067800160823     c                   eval      param = kpjbu
067900160823
068000160823      * data del giorno
068100160823     c                   time                    wtime
068200160823     c                   move      wtime         wdate
068300160823
068400160823     c     kcev          klist
068500160823     c                   kfld                    anno
068600160823     c                   kfld                    taslnp
068700160823     c                   kfld                    tasnrs
068800160823     c                   kfld                    tasnsp
068900160823     c                   kfld                    tastbl
069000160823** cmd
069100160823CLRPFM FILE(WFfss00F)
