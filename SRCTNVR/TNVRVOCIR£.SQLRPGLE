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
001207050408     d totale          s             13  3
001208050408     c*
001209050408     C/EXEC SQL
001210050408     C+ DECLARE C1 CURSOR FOR SELECT TASAAS, TASLNP, TASNRS, TASNSP,
009100050408     C+ TASTBL FROM titas10f WHERE tasddc between 20050300 and 20050331
009200050408     C+ and (tasftc ='S' or tastc2='S')
009301050408     C/END-EXEC
009302050408     c*
009303050408     C/EXEC SQL
009304050408     C+ OPEN C1
009305050408     C/END-EXEC
009306050408
009307050408     C                   DO        *HIVAL
009308050408
009309050408     C/EXEC SQL
009310050408     C+ FETCH NEXT FROM C1 INTO :aas, :lnp, :nrs, :nsp, :tbl
009311050408     C/END-EXEC
009312050408
009313050408     C                   IF        SQLCod = 100
009314050408     C                   LEAVE
009315050408     C                   ENDIF
009316050408
009317050408     c                   EXSR      conta
009318050408
009319050408     C                   ENDDO
009328050408
009329050408     C/EXEC SQL
009330050408     C+ CLOSE C1
009331050408     C/END-EXEC
009332050408     c                   except    riga
009333050408     c                   seton                                        lr
039100050408     C*--------------------------------------------------------------- ---------*
039201050408     C     conta         BEGSR
039202050408     c     kcev          chain     eccev30c
039203050408     c                   if        %found(eccev30c)
039205050408     c                   if        cevc01 = 242 or cevc01= 247
039206050408     c                   add       cevi01        totale
039207050408     c                   end
039208050408     c                   if        cevc02 = 242 or cevc02= 247
039209050408     c                   add       cevi02        totale
039210050408     c                   end
039211050408     c                   if        cevc03 = 242 or cevc03= 247
039212050408     c                   add       cevi03        totale
039213050408     c                   end
039214050408     c                   if        cevc04 = 242 or cevc04= 247
039215050408     c                   add       cevi04        totale
039216050408     c                   end
039217050408     c                   if        cevc05 = 242 or cevc05= 247
039218050408     c                   add       cevi05        totale
039219050408     c                   end
039220050408     c                   if        cevc06 = 242 or cevc06= 247
039221050408     c                   add       cevi06        totale
039222050408     c                   end
039223050408     c                   if        cevc07 = 242 or cevc07= 247
039224050408     c                   add       cevi07        totale
039225050408     c                   end
039226050408     c                   if        cevc08 = 242 or cevc08= 247
039227050408     c                   add       cevi08        totale
039228050408     c                   end
039229050408     c                   if        cevc09 = 242 or cevc09= 247
039230050408     c                   add       cevi09        totale
039231050408     c                   end
039232050408     c                   if        cevc10 = 242 or cevc10= 247
039233050408     c                   add       cevi10        totale
039234050408     c                   end
039235050408     c                   if        cevc11 = 242 or cevc11= 247
039236050408     c                   add       cevi11        totale
039237050408     c                   end
039238050408     c                   if        cevc12 = 242 or cevc12= 247
039239050408     c                   add       cevi12        totale
039240050408     c                   end
039241050408     c                   if        cevc13 = 242 or cevc13= 247
039242050408     c                   add       cevi13        totale
039243050408     c                   end
039244050408     c                   if        cevc14 = 242 or cevc14= 247
039245050408     c                   add       cevi14        totale
039246050408     c                   end
039247050408     c                   if        cevc15 = 242 or cevc15= 247
039248050408     c                   add       cevi15        totale
039249050408     c                   end
039250050408     c                   if        cevc16 = 242 or cevc16= 247
039251050408     c                   add       cevi16        totale
039252050408     c                   end
039253050408     c                   if        cevc17 = 242 or cevc17= 247
039254050408     c                   add       cevi17        totale
039255050408     c                   end
039256050408     c                   if        cevc18 = 242 or cevc18= 247
039257050408     c                   add       cevi18        totale
039258050408     c                   end
039259050408     c                   if        cevc19 = 242 or cevc19= 247
039260050408     c                   add       cevi19        totale
039261050408     c                   end
039262050408     c                   if        cevc20 = 242 or cevc20= 247
039263050408     c                   add       cevi20        totale
039264050408     c                   end
039265050408     c                   if        cevc21 = 242 or cevc21= 247
039266050408     c                   add       cevi21        totale
039267050408     c                   end
039268050408     c                   if        cevc22 = 242 or cevc22= 247
039269050408     c                   add       cevi22        totale
039270050408     c                   end
039271050408     c                   if        cevc23 = 242 or cevc23= 247
039272050408     c                   add       cevi23        totale
039273050408     c                   end
039274050408     c                   if        cevc24 = 242 or cevc24= 247
039275050408     c                   add       cevi24        totale
039276050408     c                   end
039277050408     c                   if        cevc25 = 242 or cevc25= 247
039278050408     c                   add       cevi25        totale
039279050408     c                   end
039280050408     c                   if        cevc26 = 242 or cevc26= 247
039281050408     c                   add       cevi26        totale
039282050408     c                   end
039283050408     c                   if        cevc27 = 242 or cevc27= 247
039284050408     c                   add       cevi27        totale
039285050408     c                   end
039286050408     c                   if        cevc28 = 242 or cevc28= 247
039287050408     c                   add       cevi28        totale
039288050408     c                   end
039289050408     c                   if        cevc29 = 242 or cevc29= 247
039290050408     c                   add       cevi29        totale
039291050408     c                   end
039292050408     c                   if        cevc30 = 242 or cevc30= 247
039293050408     c                   add       cevi30        totale
039294050408     c                   end
039295050408     c                   if        cevc31 = 242 or cevc31= 247
039296050408     c                   add       cevi31        totale
039297050408     c                   end
039298050408     c                   if        cevc32 = 242 or cevc32= 247
039299050408     c                   add       cevi32        totale
039300050408     c                   end
039301050408     c                   if        cevc33 = 242 or cevc33= 247
039302050408     c                   add       cevi33        totale
039303050408     c                   end
039304050408     c                   if        cevc34 = 242 or cevc34= 247
039305050408     c                   add       cevi34        totale
039306050408     c                   end
039307050408     c                   if        cevc35 = 242 or cevc35= 247
039308050408     c                   add       cevi35        totale
039309050408     c                   end
039310050408     c                   if        cevc36 = 242 or cevc36= 247
039311050408     c                   add       cevi36        totale
039312050408     c                   end
039313050408     c                   if        cevc37 = 242 or cevc37= 247
039314050408     c                   add       cevi37        totale
039315050408     c                   end
039316050408     c                   if        cevc38 = 242 or cevc38= 247
039317050408     c                   add       cevi38        totale
039318050408     c                   end
039319050408     c                   if        cevc39 = 242 or cevc39= 247
039320050408     c                   add       cevi39        totale
039321050408     c                   end
039322050408     c                   if        cevc40 = 242 or cevc40= 247
039323050408     c                   add       cevi40        totale
039324050408     c                   end
039325050408     c                   if        cevc41 = 242 or cevc41= 247
039326050408     c                   add       cevi41        totale
039327050408     c                   end
039328050408     c                   if        cevc42 = 242 or cevc42= 247
039329050408     c                   add       cevi42        totale
039330050408     c                   end
039331050408     c                   if        cevc43 = 242 or cevc43= 247
039332050408     c                   add       cevi43        totale
039333050408     c                   end
039334050408     c                   if        cevc44 = 242 or cevc44= 247
039335050408     c                   add       cevi44        totale
039336050408     c                   end
039337050408     c                   if        cevc45 = 242 or cevc45= 247
039338050408     c                   add       cevi45        totale
039339050408     c                   end
039340050408     c                   if        cevc46 = 242 or cevc46= 247
039341050408     c                   add       cevi46        totale
039342050408     c                   end
039343050408     c                   if        cevc47 = 242 or cevc47= 247
039344050408     c                   add       cevi47        totale
039345050408     c                   end
039346050408     c                   if        cevc48 = 242 or cevc48= 247
039347050408     c                   add       cevi48        totale
039348050408     c                   end
039349050408     c                   if        cevc49 = 242 or cevc49= 247
039350050408     c                   add       cevi49        totale
039351050408     c                   end
039352050408     c                   if        cevc50 = 242 or cevc50= 247
039353050408     c                   add       cevi50        totale
039354050408     c                   end
039355050408     c                   end
039356050408     C                   ENDSR
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
043700050408c    o                                           +0 'Totale competenze supermer'
043800050408c    o                                           +0 'cati'
043900050408c    o                       totale        4     +1
