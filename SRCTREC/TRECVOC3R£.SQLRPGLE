000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
001201050408     Fsysprt    o    f  132        printer
001202110420     Fecevt01l  if   e           k disk
001203110420     Ftitas30c  if   e           k disk
001204050408     d aas             s                   like(cevaas)
001205050408     d lnp             s                   like(cevlnp)
001206050408     d nrs             s                   like(cevnrs)
001207050408     d nsp             s                   like(cevnsp)
001208050408     d tbl             s                   like(cevtbl)
001210100520     d totale          s             15  5
001211110420     d eccevds       e ds                  extname(eccev00f)
001212110420     D KPJBA         E DS
001213110420     d param           ds
001214110420     d voc                     1      3s 0
001216110420     d dta                     4     11s 0
001217110420     d aa                      4      7s 0
001218110420     d mm                      8      9s 0
001219110420     d gg                     10     11s 0
001220110420     d mmgg                    8     11s 0
001221110420     c* --------------------------------------------------------------------
001222110420     C     *ENTRY        PLIST
001223110420     C                   PARM                    KPJBA
001224110420     C                   MOVEL     KPJBU         param
001228110420     c     ktas          klist
001229110420     c                   kfld                    cevaas
001230110420     c                   kfld                    cevlnp
001231110420     c                   kfld                    cevnrs
001232110420     c                   kfld                    cevnsp
001233110420     c                   kfld                    cevtbl
001234110420     c                   seton                                        lr
001235110420     c     voc           chain     ecevt01l
001236110420     c                   if        not %found(ecevt01l)
001237110420     c                   return
001238100519     c                   end
001239110420     c                   if        evttvo <> 'V'
001240110420     c                   return
001242110420     c                   end
001247050502     c                   clear                   totale
001248110420     c                   if        mm <> 0
001249050502     C/EXEC SQL
001250110420     C+ DECLARE C1 CURSOR FOR SELECT * FROM eccev00f WHERE CEVAAS = :aa
001251110420     C+ and cevmes = :mm
001252050502     C/END-EXEC
001253110420     c                   else
001254110420     C/EXEC SQL
001255110420     C+ DECLARE d1 CURSOR FOR SELECT * FROM eccev00f WHERE CEVAAS = :aa
001257110420     C/END-EXEC
001258110420     c                   end
001259110420     c*
001260110420     c                   if        mm <> 0
001261050502     C/EXEC SQL
001262050502     C+ OPEN c1
001263050502     C/END-EXEC
001264110420     c                   else
001265110420     C/EXEC SQL
001266110420     C+ OPEN d1
001267110420     C/END-EXEC
001268110420     c                   end
001269110420     c*
001270050502
001271050502     C                   DO        *HIVAL
001272050502
001273110420     c                   if        mm <> 0
001274050502     C/EXEC SQL
001275050502     C+ FETCH NEXT FROM c1 INTO :eccevds
001276050502     C/END-EXEC
001277110420     c                   else
001278110420     C/EXEC SQL
001279110420     C+ FETCH NEXT FROM d1 INTO :eccevds
001280110420     C/END-EXEC
001281110420     c                   end
001282050502
001283110420     C                   IF        SQLCod = 100 or sqlcod < 0
001284050502     C                   LEAVE
001285050502     C                   ENDIF
001286110420
001287110420     c                   if        gg <> 0
001288110420     c     ktas          chain     titas30c
001289110420     c                   if        (%found(titas30c) and tasmgs <> mmgg)
001290110420     c                             or not %found(titas30c)
001291110420     c                   iter
001292110420     c                   end
001293110420     c                   end
001294110420
001295050502     c                   EXSR      conta
001296050502
001297050502     C                   ENDDO
001298050502
001299110420     c                   if        mm <> 0
001300050502     C/EXEC SQL
001301050502     C+ CLOSE c1
001302050502     C/END-EXEC
001303110420     c                   else
001304110420     C/EXEC SQL
001305110420     C+ CLOSE d1
001306110420     C/END-EXEC
001307110420     c                   end
001308100519     c                   except    riga
009306050408
009332050429     c*
039100050408     C*--------------------------------------------------------------- ---------
039201050408     C     conta         BEGSR
039205100519     c                   if        cevc01 = evtvoc
039206050408     c                   add       cevi01        totale
039207050408     c                   end
039208100519     c                   if        cevc02 = evtvoc
039209050408     c                   add       cevi02        totale
039210050408     c                   end
039211100519     c                   if        cevc03 = evtvoc
039212050408     c                   add       cevi03        totale
039213050408     c                   end
039214100519     c                   if        cevc04 = evtvoc
039215050408     c                   add       cevi04        totale
039216050408     c                   end
039217100519     c                   if        cevc05 = evtvoc
039218050408     c                   add       cevi05        totale
039219050408     c                   end
039220100519     c                   if        cevc06 = evtvoc
039221050408     c                   add       cevi06        totale
039222050408     c                   end
039223100519     c                   if        cevc07 = evtvoc
039224050408     c                   add       cevi07        totale
039225050408     c                   end
039226100519     c                   if        cevc08 = evtvoc
039227050408     c                   add       cevi08        totale
039228050408     c                   end
039229100519     c                   if        cevc09 = evtvoc
039230050408     c                   add       cevi09        totale
039231050408     c                   end
039232100519     c                   if        cevc10 = evtvoc
039233050408     c                   add       cevi10        totale
039234050408     c                   end
039235100519     c                   if        cevc11 = evtvoc
039236050408     c                   add       cevi11        totale
039237050408     c                   end
039238100519     c                   if        cevc12 = evtvoc
039239050408     c                   add       cevi12        totale
039240050408     c                   end
039241100519     c                   if        cevc13 = evtvoc
039242050408     c                   add       cevi13        totale
039243050408     c                   end
039244100519     c                   if        cevc14 = evtvoc
039245050408     c                   add       cevi14        totale
039246050408     c                   end
039247100519     c                   if        cevc15 = evtvoc
039248050408     c                   add       cevi15        totale
039249050408     c                   end
039250100519     c                   if        cevc16 = evtvoc
039251050408     c                   add       cevi16        totale
039252050408     c                   end
039253100519     c                   if        cevc17 = evtvoc
039254050408     c                   add       cevi17        totale
039255050408     c                   end
039256100519     c                   if        cevc18 = evtvoc
039257050408     c                   add       cevi18        totale
039258050408     c                   end
039259100519     c                   if        cevc19 = evtvoc
039260050408     c                   add       cevi19        totale
039261050408     c                   end
039262100519     c                   if        cevc20 = evtvoc
039263050408     c                   add       cevi20        totale
039264050408     c                   end
039265100519     c                   if        cevc21 = evtvoc
039266050408     c                   add       cevi21        totale
039267050408     c                   end
039268100519     c                   if        cevc22 = evtvoc
039269050408     c                   add       cevi22        totale
039270050408     c                   end
039271100519     c                   if        cevc23 = evtvoc
039272050408     c                   add       cevi23        totale
039273050408     c                   end
039274100519     c                   if        cevc24 = evtvoc
039275050408     c                   add       cevi24        totale
039276050408     c                   end
039277100519     c                   if        cevc25 = evtvoc
039278050408     c                   add       cevi25        totale
039279050408     c                   end
039280100519     c                   if        cevc26 = evtvoc
039281050408     c                   add       cevi26        totale
039282050408     c                   end
039283100519     c                   if        cevc27 = evtvoc
039284050408     c                   add       cevi27        totale
039285050408     c                   end
039286100519     c                   if        cevc28 = evtvoc
039287050408     c                   add       cevi28        totale
039288050408     c                   end
039289100519     c                   if        cevc29 = evtvoc
039290050408     c                   add       cevi29        totale
039291050408     c                   end
039292100519     c                   if        cevc30 = evtvoc
039293050408     c                   add       cevi30        totale
039294050408     c                   end
039295100519     c                   if        cevc31 = evtvoc
039296050408     c                   add       cevi31        totale
039297050408     c                   end
039298100519     c                   if        cevc32 = evtvoc
039299050408     c                   add       cevi32        totale
039300050408     c                   end
039301100519     c                   if        cevc33 = evtvoc
039302050408     c                   add       cevi33        totale
039303050408     c                   end
039304100519     c                   if        cevc34 = evtvoc
039305050408     c                   add       cevi34        totale
039306050408     c                   end
039307100519     c                   if        cevc35 = evtvoc
039308050408     c                   add       cevi35        totale
039309050408     c                   end
039310100519     c                   if        cevc36 = evtvoc
039311050408     c                   add       cevi36        totale
039312050408     c                   end
039313100519     c                   if        cevc37 = evtvoc
039314050408     c                   add       cevi37        totale
039315050408     c                   end
039316100519     c                   if        cevc38 = evtvoc
039317050408     c                   add       cevi38        totale
039318050408     c                   end
039319100519     c                   if        cevc39 = evtvoc
039320050408     c                   add       cevi39        totale
039321050408     c                   end
039322100519     c                   if        cevc40 = evtvoc
039323050408     c                   add       cevi40        totale
039324050408     c                   end
039325100519     c                   if        cevc41 = evtvoc
039326050408     c                   add       cevi41        totale
039327050408     c                   end
039328100519     c                   if        cevc42 = evtvoc
039329050408     c                   add       cevi42        totale
039330050408     c                   end
039331100519     c                   if        cevc43 = evtvoc
039332050408     c                   add       cevi43        totale
039333050408     c                   end
039334100519     c                   if        cevc44 = evtvoc
039335050408     c                   add       cevi44        totale
039336050408     c                   end
039337100519     c                   if        cevc45 = evtvoc
039338050408     c                   add       cevi45        totale
039339050408     c                   end
039340100519     c                   if        cevc46 = evtvoc
039341050408     c                   add       cevi46        totale
039342050429     c                   end
039343100519     c                   if        cevc47 = evtvoc
039344050408     c                   add       cevi47        totale
039345050408     c                   end
039346100519     c                   if        cevc48 = evtvoc
039347050408     c                   add       cevi48        totale
039348050408     c                   end
039349100519     c                   if        cevc49 = evtvoc
039350050408     c                   add       cevi49        totale
039351050408     c                   end
039352100519     c                   if        cevc50 = evtvoc
039353050408     c                   add       cevi50        totale
039354050408     c                   end
039356050408     C                   ENDSR
043600050408     osysprt    e            riga        1
043700110420c    o                                           +0 'ECCEV00F: Totale voce '
043900100519c    o                       evtvoc              +1
043901100519c    o                       totale        4     +1
