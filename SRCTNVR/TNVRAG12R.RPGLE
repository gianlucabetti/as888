000100121004      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200121004     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300121004
000400121004      *---------------------------------------------------------------*
000500121004
000600121005     FWFSER20F  IF   E             DISK
000700121005     fTitas40c  if   e           k disk
000800121005     FWFSER30F  O    E             DISK
000900121004
001000121004
001100121004     d kpjba         e ds
001200121004
001300121004
001400121004     C     *entry        plist
001500121004     C                   parm                    kpjba
001600121004     c*
001700121005     c*
001800121005     c     ktas          klist
001900121005     c                   kfld                    SERFIV
002000121005     c                   kfld                    SERNFT
002100121005     c                   kfld                    SERDFT
002200121004      *
002300121004
002400121004Do  1c                   Do        *hival
002500121004      *
002600121005     c                   clear                   wfser300
002700121004      *
002800121005     c                   read      wfser20f
002900121004      *
003000121005     c                   if        %eof(wfser20f)
003100121004     c                   leave
003200121004     c                   endif
003300121005      * per ogni fattura recupero il dettaglio spedizioni
003400121005     c     ktas          setll     titas40c
003500121005     c                   do        *hival
003600121005     c     ktas          reade     titas40c
003700121005     c                   if        %eof(titas40c)
003800121005     c                   leave
003900121005     c                   endif
004000121005     C                   IF        TASTBL = 'F7' OR TASTBL = 'FS' OR
004100121005     C                             TASTBL = 'FC' OR TASTBL = 'AS' OR
004200121005     C                             TASTBL = 'AP'
004300121005     c                   write     wfser300
004400121005     c                   endif
004500121005     c                   enddo
004600121004
004700121004     c                   enddo
004800121004
004900121004     c                   seton                                        lr
