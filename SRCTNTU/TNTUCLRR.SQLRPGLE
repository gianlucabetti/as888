000401081015     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000402081015
000500081015      /TITLE pulizia tempi uffici
000501081015     ftabel00f  if   e           k disk
000602081015     dds5atu         e ds
000603081015     d DataOggi        s              8  0 Inz
000604081015     d Dateu           s               d   datfmt(*iso)
000605081106     D kpjba         e ds
002000030115
002001081106     c     *entry        plist
002002081106     c                   parm                    kpjba
002003081015     c* verifico quanti gg conservare
002004081015     c     ktab          klist
002005081015     c                   kfld                    TBLKUT
002006081015     c                   kfld                    TBLCOD
002007081015     c                   kfld                    TBLKEY
002008081015     c                   z-add     1             tblkut
002009081015     c                   movel(p)  '5A'          tblcod
002010081015     c                   movel(p)  'TU'          tblkey
002011081106     C     ktab          chain     tabel00f
002012081106     c                   if        %found(tabel00f)
002013081106     c                   movel     tbluni        ds5atu
002014081106     c                   else
002016081113     c                   eval      §5atu = 999
002017081106     c                   end
002018081015      * Data del Giorno
002019081015     c                   Movel     *date         DataOggi
002020081015     c     *iso          Movel     *date         Dateu
002021081015      * Data max per cancellazione record già ricevuti
002022081106     c                   Subdur    §5ATU:*D      Dateu
002023081106     C/EXEC SQL
002024081106     C+ DELETE FROM TNTUD00F WHERE tuddta <= :dateu
002025081106     C/END-EXEC
002026081106     C/EXEC SQL
002027081106     C+ DELETE FROM TNTUS00F WHERE tusdta <= :dateu
002028081106     C/END-EXEC
008700081106     c                   seton                                        lr
