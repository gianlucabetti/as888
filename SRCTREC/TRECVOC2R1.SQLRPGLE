000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
001201050408     Fsysprt    o    f  132        printer
001203110420     Fecevt01l  if   e           k disk
001204110420     Ftitas30c  if   e           k disk
001205110315     d aas             s                   like(c2vaas)
001206110315     d lnp             s                   like(c2vlnp)
001207110315     d nrs             s                   like(c2vnrs)
001208110315     d nsp             s                   like(c2vnsp)
001209110315     d tbl             s                   like(c2vtbl)
001210110315     d ecc2vds       e ds                  extname(ecc2v00f)
001211100520     d totale          s             15  5
001212110420     D KPJBA         E DS
001213110420     d param           ds
001214110420     d voc                     1      3s 0
001216110420     d dta                     4     11s 0
001217110420     d aa                      4      7s 0
001218110420     d mm                      8      9s 0
001219110420     d gg                     10     11s 0
001220110420     d mmgg                    8     11s 0
001221170705     d ksc                    12     18s 0
001222110420     C     *ENTRY        PLIST
001223110420     C                   PARM                    KPJBA
001224110420     C                   MOVEL     KPJBU         param
001227110420     c     ktas          klist
001228110420     c                   kfld                    c2vaas
001229110420     c                   kfld                    c2vlnp
001230110420     c                   kfld                    c2vnrs
001231110420     c                   kfld                    c2vnsp
001232110420     c                   kfld                    c2vtbl
001233110420     c                   seton                                        lr
001234110420     c     voc           chain     ecevt01l
001235110420     c                   if        not %found(ecevt01l)
001237110420     c                   return
001238110420     c                   end
001239110420     c                   if        evttvo <> 'V'
001240110420     c                   return
001241110420     c                   end
001242110420     c                   clear                   totale
001243110420     c                   if        mm <> 0
001244050502     C/EXEC SQL
001245110420     C+ DECLARE C1 CURSOR FOR SELECT * FROM ecc2v00f WHERE C2VAAS = :aa
001246110420     C+ and c2vmes = :mm
001247110420     C/END-EXEC
001248110420     c                   else
001249110420     C/EXEC SQL
001250110420     C+ DECLARE d1 CURSOR FOR SELECT * FROM ecc2v00f WHERE C2VAAS = :aa
001251110420     C/END-EXEC
001252110420     c                   end
001253110420     c*
001254110420     c                   if        mm <> 0
001255050502     C/EXEC SQL
001256050502     C+ OPEN c1
001257050502     C/END-EXEC
001258110420     c                   else
001259110420     C/EXEC SQL
001260110420     C+ OPEN d1
001261110420     C/END-EXEC
001262110420     c                   end
001263050502
001264050502     C                   DO        *HIVAL
001265050502
001266110420     c                   if        mm <> 0
001267050502     C/EXEC SQL
001268110315     C+ FETCH NEXT FROM c1 INTO :ecc2vds
001269050502     C/END-EXEC
001270110420     c                   else
001271110420     C/EXEC SQL
001272110420     C+ FETCH NEXT FROM d1 INTO :ecc2vds
001273110420     C/END-EXEC
001274110420     c                   end
001275050502
001276110420     C                   IF        SQLCod = 100 or sqlcod < 0
001277050502     C                   LEAVE
001278050502     C                   ENDIF
001279110420
001281110420     c     ktas          chain     titas30c
001282170705     c                   if        (%found(titas30c)
001283170705     c                             and gg <> 0  and tasmgs <> mmgg)
001284110420     c                             or not %found(titas30c)
001285170705     c                             or (%found(titas30c) and tasksc <> ksc)
001286110420     c                   iter
001288110420     c                   end
001289050502
001290050502     c                   EXSR      conta
001291050502
001292050502     C                   ENDDO
001293050502
001294110420     c*
001295110420     c                   if        mm <> 0
001296050502     C/EXEC SQL
001297050502     C+ CLOSE c1
001298050502     C/END-EXEC
001299110420     c                   else
001300110420     C/EXEC SQL
001301110420     C+ CLOSE d1
001302110420     C/END-EXEC
001303110420     c                   end
001304100519     c                   except    riga
009306050408
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
043700110420c    o                                           +0 'ECC2V00F: Totale voce '
043900100519c    o                       evtvoc              +1
043901100519c    o                       totale        4     +1
