000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200110225     H* Trecs1r1*----------------------------------------------------*
000300110228     H* Richiede libreria sulla quale simulare e duplica l'ambiente
000400950517     H*--------------------------------------------------------------*
000500110225     Ftrecs1d1  CF   E             WORKSTN
004600950517     C* Loop di gestione videata
004602050426     C     *ENTRY        PLIST
004603110225     C                   PARM                    lib              10
004604110228     C                   PARM                    err               1
004605110225     C                   clear                   lib
004606110228     C                   clear                   err
004607050426     c*
004608110114     C                   SETON                                        LR
004609110228     C                   clear                   vlib
004610110228     C                   clear                   verr              1
004611110225     c                   eval      vdup  = 'S'
004612110228     c                   eval      vclr  = 'N'
004614110228     c                   do        *hival
004701110225     C                   EXFMT     trecs1
004702050426     C                   IF        *inkl = '1'
004703050426     c                   leave
004900950517     C                   END
004901110225     c                   if        vlib <> *blanks
004902110225     c                   call      'TRECS1C1'
004903110228     c                   parm                    vlib
004904110228     c                   parm                    vdup
004905110228     c                   parm                    vclr
004906110228     c                   parm                    verr
004907110228     c                   eval      err = verr
004908110228     c                   eval      lib = vlib
004909050426     c                   leave
004910050426     c                   end
005000050426     C                   ENDdo
