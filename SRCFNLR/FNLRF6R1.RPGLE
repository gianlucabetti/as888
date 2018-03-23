000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200030625     H* FNLRF6R *----------------------------------------------------*
000300030625     H*         Generazione e quadratura distinte AUT
000400950517     H*--------------------------------------------------------------*
000500050426     FFNLRF6D1  CF   E             WORKSTN
004600950517     C* Loop di gestione videata
004602050426     C     *ENTRY        PLIST
004603050426     C                   PARM                    laser             1
004604050426     C                   clear                   Vlaser
004605050426     C                   clear                   laser
004606050426     c*
004607110114     C                   SETON                                        LR
004608110114     c                   eval      laser = 'S'
004609110114     c                   return
004700050426     C                   DO        *hival
004701050426     C                   EXFMT     LRf6w01
004702050426     C                   IF        *inkl = '1'
004703050426     c                   leave
004900950517     C                   END
004901050426     c                   if        vlaser <> *blanks
004902050426     c                   eval      laser = vlaser
004903050426     c                   leave
004904050426     c                   end
005000050426     C                   ENDdo
