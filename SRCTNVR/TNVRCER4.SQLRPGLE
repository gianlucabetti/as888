000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000801050502     Feccev30c  IF   E           K DISK
001201050408     Fsysprt    o    f  132        printer
001202050408     d aas             s                   like(cevaas)
001203050408     d lnp             s                   like(cevlnp)
001204050408     d nrs             s                   like(cevnrs)
001205050408     d nsp             s                   like(cevnsp)
001206050408     d tbl             s                   like(cevtbl)
001207050502     d eccevds       e ds                  extname(eccev00f)
001208050408     d totale          s             13  3
001209050429     d totcet          s             13  3
001210050429     d totced          s             13  3
001211050429     d totcomp         s             13  3
001212050502     C* LETTURA eccev30c
001213050502     C     Kcev          KLIST
001214050502     C                   KFLD                    aas                            *UTENTE
001215050502     C                   KFLD                    lnp                            *TABELLA
001216050502     C                   KFLD                    nrs                            *TABELLA
001217050502     C                   KFLD                    nsp                            *TABELLA
001218050502     C                   KFLD                    tbl                            *TABELLA
001219050502     C/EXEC SQL
001220050502     C+ DECLARE C1 CURSOR FOR SELECT TASAAS, TASLNP, TASNRS, TASNSP,
001221050502     C+ TASTBL FROM titas00f WHERE tasmgs between 0300 and 0399 and
001222050502     C+ tasaas = 2005
001223050502     C/END-EXEC
001224050502     c*
001225050502     C/EXEC SQL
001226050502     C+ OPEN c1
001227050502     C/END-EXEC
001228050502
001230050502     C                   DO        *HIVAL
001231050502
001232050502     C/EXEC SQL
001233050502     C+ FETCH NEXT FROM c1 INTO :aas, :lnp, :nrs, :nsp, :tbl
001234050502     C/END-EXEC
001235050502
001236050502     C                   IF        SQLCod = 100
001237050502     C                   LEAVE
001238050502     C                   ENDIF
001239050502     c* voce 900 -----------------------------------------------------------
001240050502     c                   EXSR      conta
009360050429     c* competenze ---------------------------------------------------------
009361050429     c                   clear                   totcet
009362050429     C/EXEC SQL
009364050502     C+ SELECT sum( CETPAR+ CETPAM+ CETPAU+ CETPAV+ CETARD+ CETARM+
009365050502     C+ CETARU+ CETARV+ CETTRL+ CETPAG+ CETSEA+ CETSEC+ CETRCV+ CETMDE+
009366050502     C+ CETTDE+ CETMAF+ CETTAF+ CETSES) INTO :totcet FROM eccet00f WHERE
009367050502     C+ cetaas = :aas and cetlnp = :lnp and cetnrs = :nrs and cetnsp =
009368050502     C+ :nsp and cettbl = :tbl
009369050429     C/END-EXEC
009370050429     c*
009383050429     c                   clear                   totced
009384050429     C/EXEC SQL
009385050502     C+ SELECT sum( CEDIMP ) INTO :totced FROM ecced00f WHERE CEDAAS=
009386050502     C+ :aas and CEDlnp= :lnp and cednrs=:nrs and cednsp = :nsp and
009387050502     C+ cedcmp not in (11, 12, 205, 204, 53, 206, 54, 207, 55, 208)
009388050429     C/END-EXEC
009401050429     c                   eval      totcomp = totced + totcet
009402050502     c                   if        totcomp <> totale
009403050408     c                   except    riga
009404050502     c                   end
009405050502     C                   ENDDO
009406050502
009407050502     C/EXEC SQL
009408050502     C+ CLOSE c1
009409050502     C/END-EXEC
009410050408     c                   seton                                        lr
039100050408     C*--------------------------------------------------------------- ---------*
039201050408     C     conta         BEGSR
039202050502     c                   clear                   totale
039203050502     c     kcev          chain     eccev30c
039204050502     c                   if        %found(eccev30c)
039205050429     c                   if        cevc01 = 900
039206050408     c                   add       cevi01        totale
039207050408     c                   end
039208050429     c                   if        cevc02 = 900
039209050408     c                   add       cevi02        totale
039210050408     c                   end
039211050429     c                   if        cevc03 = 900
039212050408     c                   add       cevi03        totale
039213050408     c                   end
039214050429     c                   if        cevc04 = 900
039215050408     c                   add       cevi04        totale
039216050408     c                   end
039217050429     c                   if        cevc05 = 900
039218050408     c                   add       cevi05        totale
039219050408     c                   end
039220050429     c                   if        cevc06 = 900
039221050408     c                   add       cevi06        totale
039222050408     c                   end
039223050429     c                   if        cevc07 = 900
039224050408     c                   add       cevi07        totale
039225050408     c                   end
039226050429     c                   if        cevc08 = 900
039227050408     c                   add       cevi08        totale
039228050408     c                   end
039229050429     c                   if        cevc09 = 900
039230050408     c                   add       cevi09        totale
039231050408     c                   end
039232050429     c                   if        cevc10 = 900
039233050408     c                   add       cevi10        totale
039234050408     c                   end
039235050429     c                   if        cevc11 = 900
039236050408     c                   add       cevi11        totale
039237050408     c                   end
039238050429     c                   if        cevc12 = 900
039239050408     c                   add       cevi12        totale
039240050408     c                   end
039241050429     c                   if        cevc13 = 900
039242050408     c                   add       cevi13        totale
039243050408     c                   end
039244050429     c                   if        cevc14 = 900
039245050408     c                   add       cevi14        totale
039246050408     c                   end
039247050429     c                   if        cevc15 = 900
039248050408     c                   add       cevi15        totale
039249050408     c                   end
039250050429     c                   if        cevc16 = 900
039251050408     c                   add       cevi16        totale
039252050408     c                   end
039253050429     c                   if        cevc17 = 900
039254050408     c                   add       cevi17        totale
039255050408     c                   end
039256050429     c                   if        cevc18 = 900
039257050408     c                   add       cevi18        totale
039258050408     c                   end
039259050429     c                   if        cevc19 = 900
039260050408     c                   add       cevi19        totale
039261050408     c                   end
039262050429     c                   if        cevc20 = 900
039263050408     c                   add       cevi20        totale
039264050408     c                   end
039265050429     c                   if        cevc21 = 900
039266050408     c                   add       cevi21        totale
039267050408     c                   end
039268050429     c                   if        cevc22 = 900
039269050408     c                   add       cevi22        totale
039270050408     c                   end
039271050429     c                   if        cevc23 = 900
039272050408     c                   add       cevi23        totale
039273050408     c                   end
039274050429     c                   if        cevc24 = 900
039275050408     c                   add       cevi24        totale
039276050408     c                   end
039277050429     c                   if        cevc25 = 900
039278050408     c                   add       cevi25        totale
039279050408     c                   end
039280050429     c                   if        cevc26 = 900
039281050408     c                   add       cevi26        totale
039282050408     c                   end
039283050429     c                   if        cevc27 = 900
039284050408     c                   add       cevi27        totale
039285050408     c                   end
039286050429     c                   if        cevc28 = 900
039287050408     c                   add       cevi28        totale
039288050408     c                   end
039289050429     c                   if        cevc29 = 900
039290050408     c                   add       cevi29        totale
039291050408     c                   end
039292050429     c                   if        cevc30 = 900
039293050408     c                   add       cevi30        totale
039294050408     c                   end
039295050429     c                   if        cevc31 = 900
039296050408     c                   add       cevi31        totale
039297050408     c                   end
039298050429     c                   if        cevc32 = 900
039299050408     c                   add       cevi32        totale
039300050408     c                   end
039301050429     c                   if        cevc33 = 900
039302050408     c                   add       cevi33        totale
039303050408     c                   end
039304050429     c                   if        cevc34 = 900
039305050408     c                   add       cevi34        totale
039306050408     c                   end
039307050429     c                   if        cevc35 = 900
039308050408     c                   add       cevi35        totale
039309050408     c                   end
039310050429     c                   if        cevc36 = 900
039311050408     c                   add       cevi36        totale
039312050408     c                   end
039313050429     c                   if        cevc37 = 900
039314050408     c                   add       cevi37        totale
039315050408     c                   end
039316050429     c                   if        cevc38 = 900
039317050408     c                   add       cevi38        totale
039318050408     c                   end
039319050429     c                   if        cevc39 = 900
039320050408     c                   add       cevi39        totale
039321050408     c                   end
039322050429     c                   if        cevc40 = 900
039323050408     c                   add       cevi40        totale
039324050408     c                   end
039325050429     c                   if        cevc41 = 900
039326050408     c                   add       cevi41        totale
039327050408     c                   end
039328050429     c                   if        cevc42 = 900
039329050408     c                   add       cevi42        totale
039330050408     c                   end
039331050429     c                   if        cevc43 = 900
039332050408     c                   add       cevi43        totale
039333050408     c                   end
039334050429     c                   if        cevc44 = 900
039335050408     c                   add       cevi44        totale
039336050408     c                   end
039337050429     c                   if        cevc45 = 900
039338050408     c                   add       cevi45        totale
039339050408     c                   end
039340050429     c                   if        cevc46 = 900
039341050408     c                   add       cevi46        totale
039342050429     c                   end
039343050429     c                   if        cevc47 = 900
039344050408     c                   add       cevi47        totale
039345050408     c                   end
039346050429     c                   if        cevc48 = 900
039347050408     c                   add       cevi48        totale
039348050408     c                   end
039349050429     c                   if        cevc49 = 900
039350050408     c                   add       cevi49        totale
039351050408     c                   end
039352050429     c                   if        cevc50 = 900
039353050408     c                   add       cevi50        totale
039354050408     c                   end
039355050502     c                   end
039356050408     C                   ENDSR
043600050408     osysprt    e            riga        1
043700050502c    o                                           +0 'Spedizione:'
043900050502c    o                       aas                 +1
043901050502c    o                       lnp                 +1
043902050502c    o                       nrs           4     +1
043903050502c    o                       nsp           z     +1
043904050502c    o                       tbl                 +1
043905050502     o          e            riga        1
043906050502c    o                                           +0 'Totale voce 900'
043907050502c    o                       totale        4     +1
044000050429     o          e            riga        1
044100050429c    o                                           +0 'Totale competenze'
044200050429c    o                       totcomp       4     +1
