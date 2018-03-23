000100920317     H DATEDIT(*YMD)
000200021017     F* TNVR40R *----------------------------------------------------*
000300021017     F*           TRASCODIFICA FILE TESTI                            *
000400021017      * -------------------------------------------------------------*
000500021022     fTabelsede IP   e             disk    rename(tabel:tabeSEDE)
000600021022     f                                     prefix(S_)
000700021022     fTabel00f  UF   e           k disk
000800021018      *
000900021018     d Ds1H          e ds
001000021022     d Ds1Hsede      e ds                  extname(DS1H) prefix(S_)
001100021022      *-------------------------------------------------------------
001200021022      * Tabel
001300021022     C     kTab          klist
001400021022     C                   kfld                    s_tblkut
001500021022     C                   kfld                    s_tblcod
001600021022     C                   kfld                    s_tblkey
001700021017      *
001800021025     c                   movel     s_tbluni      ds1Hsede
001900021022      *
002000021022     c     KTab          chain     tabel                              33
002100021021      *
002200021021    2c     *in33         IFEQ      *off
002300021022     c                   movel     tbluni        ds1H
002400021022     c                   movel     S_§1hdes      §1hdes
002500021024     c                   movel     ds1h          tbluni
002600021022     c                   update    tabel
002700021022     c                   ENDIF
