000100060301     FAZorg01l  UF   e           k disk
000200060301     Ftabel00f  IF   e           k disk
000300021118
000400060301     d  og141        e ds
000500060301     d  ds5p         e ds
000600021118
000700060301     C     KTAB1         KLIST
000800060301     C                   KFLD                    CODUT             1 0
000900060301     C                   KFLD                    KCOD              2
001000060301     C                   KFLD                    KKEY              8
001100060301     C     KTAB2         KLIST
001200060301     C                   KFLD                    CODUT
001300060301     C                   KFLD                    KCOD
001400060301     C* STAMPANTE TABULATI
001500060301     c                   z-add     1             codut
001600060301     C                   MOVEL     '5P'          KCOD
001700060301     C                   MOVEL(P)  'T'           KKEY
001800060301     c     ktab1         setll     tabel00f
001900060301
002000060301     c                   Do        *hival
002100060301
002200060301     c     ktab2         reade     tabel00f
002300060301
002400060301     c                   if        %eof(tabel00f)
002500060301     c                   leave
002600060301     c                   endif
002700060301
002800060301     c                   if        %subst(tblkey:1:1) <> 'T'
002900060301     c                   iter
003000060301     c                   endif
003100060301
003200060301     c                   clear                   codfil            3
003300060301     c                   eval      codfil= %subst(tblkey:2:3)
003400060301     c                   if        codfil > *zeros
003500060301     C                   MOVEL     TBLUNI        DS5P
003600060301     c                   move      codfil        keyfil            3 0
003700060301     c     keyfil        chain     azorg01l
003800060301     c                   If        %found(azorg01l)
003900060301     c                   movel     orgde1        og141
004000060302     c                   eval      §ogssc = §5pOTQ
004100060301     c                   movel     og141         orgde1
004200060301     c                   update    azorg
004300060301     c                   endif
004400060301
004500060301     c                   endif
004600060301
004700060301     c                   enddo
004800060301
004900060301     c                   seton                                        lr
