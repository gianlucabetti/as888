000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
001200050408     Feccev30c  IF   E           K DISK
001201050408     Fsysprt    o    f  132        printer
001202050408     d aas             s                   like(cevaas)
001203050408     d lnp             s                   like(cevlnp)
001204050408     d nrs             s                   like(cevnrs)
001205050408     d nsp             s                   like(cevnsp)
001206050408     d tbl             s                   like(cevtbl)
001207050729     d ddc             s              8  0
001208050729     d ddca            s              6
001209050408     d totale          s             13  3
001210050729     d comddc          s              6
001211050729     d mese            s              8
001212050408     c*
001213050408     C/EXEC SQL
001214050408     C+ DECLARE C1 CURSOR FOR SELECT TASAAS, TASLNP, TASNRS, TASNSP,
009100050729     C+ TASTBL, TASDDC FROM titas10f WHERE tasddc between 20050100 and
009101050729     C+ 20050630 and substr(tasgva, 1, 1) = 'P' ORDER BY tasddc
009301050408     C/END-EXEC
009302050408     c*
009303050408     C/EXEC SQL
009304050408     C+ OPEN C1
009305050408     C/END-EXEC
009306050408
009307050408     C                   DO        *HIVAL
009308050408
009309050408     C/EXEC SQL
009310050729     C+ FETCH NEXT FROM C1 INTO :aas, :lnp, :nrs, :nsp, :tbl, :ddc
009311050408     C/END-EXEC
009312050408
009313050408     C                   IF        SQLCod = 100
009314050408     C                   LEAVE
009315050729     c                   ENDIF
009316050729     c* rottura di mese distinta
009317050729     c                   movel     ddc           ddca
009318050729     c                   if        ddca <> comddc
009320050729     c                   if        totale <> 0
009321050729     c                   exsr      stampa
009323050729     c                   end
009324050729     c                   clear                   totale
009325050729     c                   eval      comddc = ddca
009326050729     c                   end
009327050729     c*
009328050408     c                   EXSR      conta
009329050408
009330050408     C                   ENDDO
009331050408
009332050408     C/EXEC SQL
009333050408     C+ CLOSE C1
009334050408     C/END-EXEC
009335050729     c                   if        totale <> 0
009336050729     c                   exsr      stampa
009337050729     c                   end
009338050408     c                   seton                                        lr
039100050408     C*--------------------------------------------------------------- ---------*
039201050729     C     stampa        BEGSR
039202050729     c                   select
039203050729     c                   when      comddc = '200501'
039204050729     c                   eval      mese = 'GENNAIO'
039205050729     c                   when      comddc = '200502'
039206050729     c                   eval      mese = 'FEBBRAIO'
039207050729     c                   when      comddc = '200503'
039208050729     c                   eval      mese = 'MARZO'
039209050729     c                   when      comddc = '200504'
039210050729     c                   eval      mese = 'APRILE'
039211050729     c                   when      comddc = '200505'
039212050729     c                   eval      mese = 'MAGGIO'
039213050729     c                   when      comddc = '200506'
039214050729     c                   eval      mese = 'GIUGNO'
039215050729     c                   endsl
039216050729     c                   except    riga
039217050729     c                   ENDSR
039218050729     C*--------------------------------------------------------------- ---------*
039219050729     C     conta         BEGSR
039220050408     c     kcev          chain     eccev30c
039221050408     c                   if        %found(eccev30c)
039222050729     c                   if        cevc01 = 243
039223050408     c                   add       cevi01        totale
039224050408     c                   end
039225050729     c                   if        cevc02 = 243
039226050408     c                   add       cevi02        totale
039227050408     c                   end
039228050729     c                   if        cevc03 = 243
039229050408     c                   add       cevi03        totale
039230050408     c                   end
039231050729     c                   if        cevc04 = 243
039232050408     c                   add       cevi04        totale
039233050408     c                   end
039234050729     c                   if        cevc05 = 243
039235050408     c                   add       cevi05        totale
039236050408     c                   end
039237050729     c                   if        cevc06 = 243
039238050408     c                   add       cevi06        totale
039239050408     c                   end
039240050729     c                   if        cevc07 = 243
039241050408     c                   add       cevi07        totale
039242050408     c                   end
039243050729     c                   if        cevc08 = 243
039244050408     c                   add       cevi08        totale
039245050408     c                   end
039246050729     c                   if        cevc09 = 243
039247050408     c                   add       cevi09        totale
039248050408     c                   end
039249050729     c                   if        cevc10 = 243
039250050408     c                   add       cevi10        totale
039251050408     c                   end
039252050729     c                   if        cevc11 = 243
039253050408     c                   add       cevi11        totale
039254050408     c                   end
039255050729     c                   if        cevc12 = 243
039256050408     c                   add       cevi12        totale
039257050408     c                   end
039258050729     c                   if        cevc13 = 243
039259050408     c                   add       cevi13        totale
039260050408     c                   end
039261050729     c                   if        cevc14 = 243 or cevc14= 247
039262050408     c                   add       cevi14        totale
039263050408     c                   end
039264050729     c                   if        cevc15 = 243
039265050408     c                   add       cevi15        totale
039266050408     c                   end
039267050729     c                   if        cevc16 = 243
039268050408     c                   add       cevi16        totale
039269050408     c                   end
039270050729     c                   if        cevc17 = 243
039271050408     c                   add       cevi17        totale
039272050408     c                   end
039273050729     c                   if        cevc18 = 243
039274050408     c                   add       cevi18        totale
039275050408     c                   end
039276050729     c                   if        cevc19 = 243
039277050408     c                   add       cevi19        totale
039278050408     c                   end
039279050729     c                   if        cevc20 = 243
039280050408     c                   add       cevi20        totale
039281050408     c                   end
039282050729     c                   if        cevc21 = 243
039283050408     c                   add       cevi21        totale
039284050408     c                   end
039285050729     c                   if        cevc22 = 243
039286050408     c                   add       cevi22        totale
039287050408     c                   end
039288050729     c                   if        cevc23 = 243
039289050408     c                   add       cevi23        totale
039290050408     c                   end
039291050729     c                   if        cevc24 = 243
039292050408     c                   add       cevi24        totale
039293050408     c                   end
039294050729     c                   if        cevc25 = 243
039295050408     c                   add       cevi25        totale
039296050408     c                   end
039297050729     c                   if        cevc26 = 243
039298050408     c                   add       cevi26        totale
039299050408     c                   end
039300050729     c                   if        cevc27 = 243
039301050408     c                   add       cevi27        totale
039302050408     c                   end
039303050729     c                   if        cevc28 = 243
039304050408     c                   add       cevi28        totale
039305050408     c                   end
039306050729     c                   if        cevc29 = 243
039307050408     c                   add       cevi29        totale
039308050408     c                   end
039309050729     c                   if        cevc30 = 243
039310050408     c                   add       cevi30        totale
039311050408     c                   end
039312050729     c                   if        cevc31 = 243
039313050408     c                   add       cevi31        totale
039314050408     c                   end
039315050729     c                   if        cevc32 = 243
039316050408     c                   add       cevi32        totale
039317050408     c                   end
039318050729     c                   if        cevc33 = 243
039319050408     c                   add       cevi33        totale
039320050408     c                   end
039321050729     c                   if        cevc34 = 243
039322050408     c                   add       cevi34        totale
039323050408     c                   end
039324050729     c                   if        cevc35 = 243
039325050408     c                   add       cevi35        totale
039326050408     c                   end
039327050729     c                   if        cevc36 = 243
039328050408     c                   add       cevi36        totale
039329050408     c                   end
039330050729     c                   if        cevc37 = 243
039331050408     c                   add       cevi37        totale
039332050408     c                   end
039333050729     c                   if        cevc38 = 243
039334050408     c                   add       cevi38        totale
039335050408     c                   end
039336050729     c                   if        cevc39 = 243
039337050408     c                   add       cevi39        totale
039338050408     c                   end
039339050729     c                   if        cevc40 = 243
039340050408     c                   add       cevi40        totale
039341050408     c                   end
039342050729     c                   if        cevc41 = 243
039343050408     c                   add       cevi41        totale
039344050408     c                   end
039345050729     c                   if        cevc42 = 243
039346050408     c                   add       cevi42        totale
039347050408     c                   end
039348050729     c                   if        cevc43 = 243
039349050408     c                   add       cevi43        totale
039350050408     c                   end
039351050729     c                   if        cevc44 = 243
039352050408     c                   add       cevi44        totale
039353050408     c                   end
039354050729     c                   if        cevc45 = 243
039355050408     c                   add       cevi45        totale
039356050408     c                   end
039357050729     c                   if        cevc46 = 243
039358050408     c                   add       cevi46        totale
039359050408     c                   end
039360050729     c                   if        cevc47 = 243
039361050408     c                   add       cevi47        totale
039362050408     c                   end
039363050729     c                   if        cevc48 = 243
039364050408     c                   add       cevi48        totale
039365050408     c                   end
039366050729     c                   if        cevc49 = 243
039367050408     c                   add       cevi49        totale
039368050408     c                   end
039369050729     c                   if        cevc50 = 243
039370050408     c                   add       cevi50        totale
039371050408     c                   end
039372050408     c                   end
039373050408     C                   ENDSR
039401050408     C*--------------------------------------------------------------- ---------*
039402050408     C* *INZSR - OPERAZIONI INIZIALI
039403050408     C*------------------------------------------------------------------------*
039404050408     C     *INZSR        BEGSR
040400000810     C*
040500050408     C* LETTURA eccev30c
040600050408     C     Kcev          KLIST
040700050408     C                   KFLD                    aas                            *UTENTE
040800050408     C                   KFLD                    lnp                            *TABELLA
040801050408     C                   KFLD                    nrs                            *TABELLA
040802050408     C                   KFLD                    nsp                            *TABELLA
040803050408     C                   KFLD                    tbl                            *TABELLA
043400991214     C*
043500970526     C                   ENDSR
043600050408     osysprt    e            riga        1
043700050729c    o                                           +0 'Totale voce 243 mese'
043900050729c    o                       mese                +1
044000050729c    o                       totale        4     +1
