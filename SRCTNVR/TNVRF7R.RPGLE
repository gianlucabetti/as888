000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200121003     Ftas_f7    uf   E             DISK    rename(tas_f7:titas0f7)
000201121003     F                                     prefix(tf7:3)
000202121003     FTITAS30c  if   E           k DISK
000203121003     ffnlbl01l  if   e           k disk
000204121003     ffnlbl02l  if   e           k disk    rename(fnlbl000:fnlbl002)
000205121003     ftigcp51l  if   e           k disk
000206121003     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000207091117     D KPJBA         E DS
000600011003     C*------------------------------------------------------------------------*
000601091117     c     *entry        plist
000602091117     c                   parm                    kpjba
000603121003     c
000604121003     c     ktaf7         klist
000605121003     c                   kfld                    tf7aas
000606121003     c                   kfld                    tf7lnp
000607121003     c                   kfld                    tf7nrs
000608121003     c                   kfld                    tf7nsp
000609121003     c     klblm         klist
000610121003     c                   kfld                    lblaap
000611121003     c                   kfld                    lbllpp
000612121003     c                   kfld                    lblnrp
000613121003     c                   kfld                    lblnsp
000614121003     c* leggo titas
000615121003     c                   read      titas0f7
000616121003     c                   dow       not %eof(tas_f7)
000617121003     c                   clear                   bomam             1
000618121003     c                   clear                   bofig             1
000619121003     c                   clear                   bogia             1
000620121003     c                   clear                   bogiamam          1
000621121003     c                   clear                   gcpcmc
000622121003     c                   clear                   bomamcca          1
000623121003     c
000624121003     c* Se bolla legata vedo come
000625121003     c     ktaf7         chain     fnlbl02l
000626121003     c                   if        %found(fnlbl02l)
000627121003     c                   eval      bomam='S'
000628121003     c                   endif
000629121003     c
000630121003     c     ktaf7         chain     fnlbl01l
000631121003     c                   if        %found(fnlbl01l)
000632121003     c                   eval      bofig='S'
000633121003     c                   endif
000634121003     c*
000635121003     c     ktaf7         chain     tigcp51l
000636121003     c                   if        %found(tigcp51l)
000637121003     c                   eval      bogia='S'
000638121003     c                   else
000639121003     c* se bolla figlia vedo se la mamma è giacente
000640121003     c                   if        bofig='S'
000641121003     c     klblm         chain     tigcp51l
000642121003     c                   if        %found(tigcp51l)
000643121003     c                   eval      bogiamam='S'
000644121003     c                   endif
000645121003     c                   endif
000646121003     c                   endif
000647121003     c
000648121003     c* se non giacente ma figlia --> prendo consegna anomala della mamma
000649121003     c                   if        bogia=' ' and bofig='S'
000650121003     c     klblm         chain     titas30c
000651121003     c                   if        %found(titas30c)
000652121003     c                   eval      bomamcca=tascca
000653121003     c                   endif
000654121003     c                   endif
000655121003     c
000656121003     c                   except    bolla
000657121003     c                   clear                   Tf7lod
000658121003     c
000659121003     c* updato la bolla col tipo motivo
000660121003     c                   select
000661121003     c                   when      bofig=' ' and bomam=' ' and bogia=' '
000662121003
000663121003     c                   if        tf7cca=' '
000664121003     c                   eval      Tf7lod='NOlegata  NOGIAC. SENZA CO.ANOM'
000665121003     c                   else
000666121003     c                   eval      Tf7lod='NOlegata  NOGIAC. CON   CO.ANOM: '+
000667121003     c                                     tf7cca
000668121003     c                   endif
000669121003     c*
000670121003     c                   when      bofig=' ' and bomam=' ' and bogia='S'
000671121003     c                   if        tf7cca=' '
000672121003     c                   eval      Tf7lod='NOlegata    GIAC. SENZA CO.ANOM  '
000673121003     C                   ELSE
000674121003     c                   eval      Tf7lod='NOlegata    GIAC. CON   CO.ANOM: '+
000675121003     c                                     tf7cca
000676121003     c                   endif
000677121003
000678121003     c                   when      bofig=' ' and bomam='S' and bogia='S'
000680121003     c                   if        tf7cca=' '
000681121003     c                   eval      Tf7lod='MAMMA       GIAC. SENZA CO.ANOM  '
000682121003     C                   ELSE
000683121003     c                   eval      Tf7lod='MAMMA       GIAC. CON   CO.ANOM: '+
000684121003     c                                     tf7cca
000685121003     c                   endif
000686121003
000687121003     c                   when      bofig='S' and bomam='S' and bogia='S'
000688121003     c                             and tf7cca='2' and tf7cca<>bomamcca
000691121003     c                   eval      Tf7lod='MAMMA       GIAC. CON   CO.ANOM: '+
000692121003     c                                     tf7cca
000694121003
000695121003     c                   when      bofig=' ' and bomam='S' and bogia=' '
000696121003     c                   if        tf7cca=' '
000697121003     c                   eval      Tf7lod='MAMMA     NOGIAC. SENZA CO.ANOM  '
000698121003     C                   ELSE
000699121003     c                   eval      Tf7lod='MAMMA     NOGIAC. CON   CO.ANOM: '+
000700121003     c                                     tf7cca
000701121003     c                   endif
000702121003
000703121003     c                   when      bofig='S' and bomam='S' and bogia=' '
000704121003     c                             and tf7cca= '2' and tf7cca<>bomamcca
000707121003     c                   eval      Tf7lod='MAMMA     NOGIAC. CON   CO.ANOM: '+
000708121003     c                                     tf7cca
000710121003     c
000711121003     c                   when      bofig='S' and bogiamam=' '
000712121003     c                   if        bomamcca=' '
000713121003     c                   eval      Tf7lod='FIGL-ORIG NOGIAC. SENZA CO.ANOM  '
000714121003     C                   ELSE
000715121003     c                   eval      Tf7lod='FIGL-ORIG NOGIAC. CON   CO.ANOM: '+
000716121003     c                                     bomamcca
000717121003     c                   endif
000718121003
000719121003     c                   when      bofig='S' and bogiamam='S'
000720121003     c                   if        bomamcca=' '
000721121003     c                   eval      Tf7lod='FIGL-ORIG   GIAC. SENZA CO.ANOM  '
000722121003     C                   ELSE
000723121003     c                   eval      Tf7lod='FIGL-ORIG   GIAC. CON   CO.ANOM: '+
000724121003     c                                     bomamcca
000725121003     c                   endif
000726121003     c
000727121003     c                   endsl
000728121003     c
000729121003     c                   except    lodta7
000730121003     c
000731121003     c                   read      titas0f7
000732121003     c                   enddo
000733121003     c
000734091117     c                   seton                                        lr
000735121003     OQSYSPRT   E            bolla       1
000900121003     O                       tf7aas           +   1
000901121003     O                       tf7lnp           +   1
000902121003     O                       tf7nrs        z  +   1
000903121003     O                       tf7nsp        2  +   1
000904121003     o                                        +   1 'cons ANOM'
000905121003     O                       tf7cca           +   1
000906121003     o                                        +   1 'Figlia'
001300121003     O                       bofig            +   1
001400121003     o                                        +   1 'Mamma'
001500121003     O                       bomam            +   1
001600121003     o                                        +   1 'giacente'
001700121003     O                       bogia            +   1
001800121003     o                                        +   1 'mamma giac'
001900121003     O                       bogiamam         +   1
002000121003     o                                        +   1 'manca consegna'
002100121003     o                       gcpcmc           +   1
002200121003     o                                        +   1 'cons ANOM mam'
002300121003     o                       bomamcca         +   1
002400121003     Otitas0f7  E            lodta7
002500121003     o                       tf7lod
