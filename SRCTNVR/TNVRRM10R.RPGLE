000100940224     H*--------------------------------------------------------------*
000200060920     Fdppdc01l  uF   E           k DISK
000300060920     Ftabel00f  iF   E           k DISK
000400060920     d ds15          e ds
000500060920      * - Ds per Anagrafiche Clienti
000600060920     d TIBS69DS      E DS
000700060920     d DS_cnaco      E DS                  extname(CNACO00F)
000800060920     d DS_cnind      E DS                  extname(CNIND00F)
000900060920     d DS_cnclp      E DS                  extname(CNCLP00F)
001000060920     d DS_fncls      E DS                  extname(FNCLS00F)
001100900131     C*---------------------------------------------------------------*
001200060920     c     ktab          klist
001300060920     C                   KFLD                    tblKUT
001400060920     C                   KFLD                    tblCOD
001500060920     C                   KFLD                    tblkey
001600060920     C                   EVAL      tblKUT = 1
001700060920     C                   EVAL      tblCOD = '15'
001800060920     c*
001900060920     c     *loval        setll     dppdc01l
002000060920    1c                   do        *hival
002100060920     c                   read      dppdc01l
002200060920    2c                   if        %eof(dppdc01l)
002300060920     c                   leave
002400060920    2c                   endif
002500060920    2c                   if        pdcksc> 0
002600060920     c                   iter
002700060920    2c                   endif
002800060920     c                   movel(P)  pdcnar        tblkey
002900060920     c     ktab          chain     tabel00f
003000060920    2c                   if        %found(tabel00f)
003100060920     c                   movel     tbluni        ds15
003200060920     c                   clear                   tibs69ds
003300060920     c                   movel     §15lad        i69kac
003400060920     c                   move      pdcdpc        i69kac
003500060920     C                   call      'TIBS69R'
003600060920     C                   parm                    tibs69DS
003700060920     C                   parm                    DS_cnaco
003800060920     C                   parm                    DS_cnind
003900060920     C                   parm                    DS_cnclp
004000060920     C                   parm                    DS_fncls
004100060920    3c                   If         O69ERR =  ' '
004200060920     c                   z-add     i69kac        pdcksc
004300060920     c                   update    dppdc000
004400060920     c                   else
004500060920     c                   unlock    dppdc01l
004600060920    3c                   endif
004700060920    2c                   endif
004800060920    1c                   enddo
004900030205      *
005000000000     C                   SETON                                        LR
