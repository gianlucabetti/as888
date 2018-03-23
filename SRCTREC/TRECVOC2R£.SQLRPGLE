000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
001201050408     Fsysprt    o    f  132        printer
001202100519     Fecevt00f  if   e             disk
001203110315     d aas             s                   like(c2vaas)
001204110315     d lnp             s                   like(c2vlnp)
001205110315     d nrs             s                   like(c2vnrs)
001206110315     d nsp             s                   like(c2vnsp)
001207110315     d tbl             s                   like(c2vtbl)
001208110315     d ecc2vds       e ds                  extname(ecc2v00f)
001209100520     d totale          s             15  5
001213050429     c* voce 900------------------------------------------------------------
001214100519     c                   do        *hival
001215110315     c                   read      ecevt00f
001216100519     c                   if        %eof(ecevt00f)
001217100519     c                   leave
001218100519     c                   end
001219110315     c*                  if        EVTDDV<= 20100120 and
001220110315     c*                            EVTDSV>= 20100120
001221110315     c*                  else
001222110315     c                   if        EVTvoc <> 900
001224100519     c                   iter
001225100519     c                   end
001226110315     c                   if        EVTDSV<= 20110315
001227110315     c                   iter
001228110315     c                   end
001229100519     c                   if        evttvo <> 'V'
001230100519     c                   iter
001231100519     c                   end
001232050502     c                   clear                   totale
001233050502     C/EXEC SQL
001234110315     C+ DECLARE C1 CURSOR FOR SELECT * FROM ecc2v00f WHERE C2VAAS = 2010
001235110315     C+ and C2VMES = 10
001236050502     C/END-EXEC
001237050502     c*
001238050502     C/EXEC SQL
001239050502     C+ OPEN c1
001240050502     C/END-EXEC
001241050502
001242050502     C                   DO        *HIVAL
001243050502
001244050502     C/EXEC SQL
001245110315     C+ FETCH NEXT FROM c1 INTO :ecc2vds
001246050502     C/END-EXEC
001247050502
001248050502     C                   IF        SQLCod = 100
001249050502     C                   LEAVE
001250050502     C                   ENDIF
001251050502
001252050502     c                   EXSR      conta
001253050502
001254050502     C                   ENDDO
001255050502
001256050502     C/EXEC SQL
001257050502     C+ CLOSE c1
001258050502     C/END-EXEC
001259100519     c                   except    riga
001260100519     C                   ENDDO
009306050408
009332050429     c*
009403050408     c                   seton                                        lr
039100050408     C*--------------------------------------------------------------- ---------
039201050408     C     conta         BEGSR
039205110315     c                   if        C2VC51 = evtvoc
039206110315     c                   add       C2Vi51        totale
039207050408     c                   end
039208110315     c                   if        C2VC52 = evtvoc
039209110315     c                   add       C2Vi52        totale
039210050408     c                   end
039211110315     c                   if        C2VC53 = evtvoc
039212110315     c                   add       C2Vi53        totale
039213050408     c                   end
039214110315     c                   if        C2VC54 = evtvoc
039215110315     c                   add       C2Vi54        totale
039216050408     c                   end
039217110315     c                   if        C2VC55 = evtvoc
039218110315     c                   add       C2Vi55        totale
039219050408     c                   end
039220110315     c                   if        C2VC56 = evtvoc
039221110315     c                   add       C2Vi56        totale
039222050408     c                   end
039223110315     c                   if        C2VC57 = evtvoc
039224110315     c                   add       C2Vi57        totale
039225050408     c                   end
039226110315     c                   if        C2VC58 = evtvoc
039227110315     c                   add       C2Vi58        totale
039228050408     c                   end
039229110315     c                   if        C2VC59 = evtvoc
039230110315     c                   add       C2Vi59        totale
039231050408     c                   end
039232110315     c                   if        C2VC60 = evtvoc
039233110315     c                   add       C2Vi60        totale
039234050408     c                   end
039235110315     c                   if        C2VC61 = evtvoc
039236110315     c                   add       C2Vi61        totale
039237050408     c                   end
039238110315     c                   if        C2VC62 = evtvoc
039239110315     c                   add       C2Vi62        totale
039240050408     c                   end
039241110315     c                   if        C2VC63 = evtvoc
039242110315     c                   add       C2Vi63        totale
039243050408     c                   end
039244110315     c                   if        C2VC64 = evtvoc
039245110315     c                   add       C2Vi64        totale
039246050408     c                   end
039247110315     c                   if        C2VC65 = evtvoc
039248110315     c                   add       C2Vi65        totale
039249050408     c                   end
039250110315     c                   if        C2VC66 = evtvoc
039251110315     c                   add       C2Vi66        totale
039252050408     c                   end
039253110315     c                   if        C2VC67 = evtvoc
039254110315     c                   add       C2Vi67        totale
039255050408     c                   end
039256110315     c                   if        C2VC68 = evtvoc
039257110315     c                   add       C2Vi68        totale
039258050408     c                   end
039259110315     c                   if        C2VC69 = evtvoc
039260110315     c                   add       C2Vi69        totale
039261050408     c                   end
039262110315     c                   if        C2VC70 = evtvoc
039263110315     c                   add       C2Vi70        totale
039264050408     c                   end
039265110315     c                   if        C2VC71 = evtvoc
039266110315     c                   add       C2Vi71        totale
039267050408     c                   end
039268110315     c                   if        C2VC72 = evtvoc
039269110315     c                   add       C2Vi72        totale
039270050408     c                   end
039271110315     c                   if        C2VC73 = evtvoc
039272110315     c                   add       C2Vi73        totale
039273050408     c                   end
039274110315     c                   if        C2VC74 = evtvoc
039275110315     c                   add       C2Vi74        totale
039276050408     c                   end
039277110315     c                   if        C2VC75 = evtvoc
039278110315     c                   add       C2Vi75        totale
039279050408     c                   end
039356050408     C                   ENDSR
043600050408     osysprt    e            riga        1
043700100519c    o                                           +0 'Totale voce '
043900100519c    o                       evtvoc              +1
043901100519c    o                       totale        4     +1
