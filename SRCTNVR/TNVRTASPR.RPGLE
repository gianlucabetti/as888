000100940912     H*------------------------------------------------------------------------*
000200050705     H* storicizza immagine laguna per TITASP già deletati
000300940912     H*------------------------------------------------------------------------*
000800050705     FTITASp0f  if   E             DISK
020501050705     D tnsb34ds      E DS
020502050705     D dtasflo       E DS
020600940912     C*------------------------------------------------------------------------*
020700940912     C*?MAIN LINE
020800940912     C*------------------------------------------------------------------------*
020801050705     c                   do        *hival
020802050705     c                   read      titasp0f
020803050705     c                   if        %eof(titasp0f)
020804050705     c                   leave
020805050705     c                   end
024401050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
024403050705     c                   movel     tasflo        dtasflo
024404050705     c                   if        §floiml <> ' '
024405050705     c                   exsr      srlaguna
024406050705     c                   end
024407050705     c                   enddo
048901050705     c* chiudo il pgm di archiviazione Laguna
048903050705     C                   CLEAR                   Tnsb34ds
048904050705     C                   MOVEL     'C'           sb34tla
048905050705     C                   CALL      'TNSB34R'
048906050705     C                   PARM                    TNSB34ds
049000000000     C*
049100000000     C                   SETON                                        LR
089300940912     C*------------------------------------------------------------------------*
089400050705     C*? funzione per scivere file x Laguna
089500940912     C*------------------------------------------------------------------------*
089600050705     C     srlaguna      BEGSR
089601050705     c                   clear                   tnsb34ds
089602050705     c                   eval      sb34tla = 'R'
089603050705     c                   eval      sb34aas = tasaas
089604050705     c                   eval      sb34lnp = taslnp
089605050705     c                   eval      sb34nrs = tasnrs
089606050705     c                   eval      sb34nsp = tasnsp
089607050705     c                   eval      sb34tbl = tastbl
089608050705     c                   call      'TNSB34R'
089609050705     c                   parm                    tnsb34ds
089610050705     c                   if        sb34err <> ' '
089611050705     c                   end
089612050705     C                   ENDSR
