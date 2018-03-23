000100950517     H DECEDIT('0,') DATEDIT(*DMY.)
000200140605     H*--------------------------------------------------------------*
000300140605     H* calcola importo voci x cdc
000400950517     H*--------------------------------------------------------------*
000500140605     Ftrecvoc0d CF   E             WORKSTN
000600110510     d kpjba         e ds
000700140605     d param           ds
000800140605     d aa                      1      4s 0
000900140605     d mmi                     5      6s 0
001000140605     d mmf                     7      8s 0
001100140605     d nbc                     9     13s 0
001200950517     C* Loop di gestione videata
001300050426     C     *ENTRY        PLIST
001400110510     C                   PARM                    kpjba
001500050426     c*
001600110114     C                   SETON                                        LR
001700110510     c                   move      *year         annov
001800110228     c                   do        *hival
001900110510     C                   EXFMT     trec00
002000110510     c                   setoff                                       010203
002100110510     C                   IF        *inkc = '1'
002200050426     c                   leave
002300950517     C                   END
002400110510     c* anno
002500110510     c                   if        annov = 0
002600110510     c                   seton                                        02
002700110510     c                   iter
002800110510     c                   end
002900140605     c* mese inizio
003000140605     c                   if        mmiv= 0
003100140605     c                   seton                                        01
003200110510     c                   iter
003300110510     c                   end
003400140605     c* mese fine
003500140605     c                   if        mmfv= 0
003600140605     c                   seton                                        03
003700140605     c                   iter
003800140605     c                   end
003900110510     c* periodo
004000140605     c                   if        mmiv> mmfv
004100110510     c                   seton                                        03
004200110510     c                   iter
004300110510     c                   end
004400140605     c*
004500140603     C                   IF        *inkf or *inkg
004600140605     c                   move      annov         aa
004700140605     c                   move      mmiv          mmi
004800140605     c                   move      mmfv          mmf
004900140605     c                   move      nbcv          nbc
005000140605     c                   movel(p)  param         kpjbu
005100140603     c                   if        *inkf
005200140605     c                   call      'TRECVOC5R'
005300110510     c                   parm                    kpjba
005400140603     c                   else
005500140605     C                   MOVEL     'ECV5'        KCOAZ
005600140603     C                   CALL      'BCH10'
005700140603     C                   PARM                    KPJBA
005800140603     c                   end
005900110511     c                   leave
006000110510     C                   END
006100050426     C                   ENDdo
