000100940224     H*--------------------------------------------------------------*
000200100408     FTNTBE01L  iF   E           K DISK
000300100408     FazUTE01l  uF   E           K DISK
000400100416     Fqsysprt   O    F  132        PRINTER
000500100408      *---------------------------------------------------------------*
000600100408     D DUte01        E DS
000700100408      *---------------------------------------------------------------*
000800100408     C     Tbe01L        Klist
000900100408     c                   Kfld                    TbeCOD
001000100408     c                   Kfld                    TbeKE1
001100100408      *
001200100416     c                   except    testata
001300100408      *---------------------------------------------------------------*
001400100416      *
001500100416     c                   eval      tbeCOD = 'AUT'
001600100416     C     TbeCOD        setll     TnTbe01l
001700100416     C     TbeCOD        reade     TnTbe01l
001800100416     c                   dow       not %EoF(TnTbe01l)
001900100416      *
002000100416      * Basta che ce ne sia 1 NON ANNULLATO
002100100416     c                   if        tbeATB = *blank
002200100416     c                   movel(p)  tbeKE1        Fil_Profilo      13
002300100416     c                   move      Fil_Profilo   uteUTE
002400100416      *
002500100416     C     uteUTE        chain     azUTE01l
002600100416      *
002700100416     c                   if        %Found(azUTE01l)
002800100416      *
002900100420     c                   movel     uteFAF        DUTE01
003000100416     c                   move      'S'           §UTETBEAUT
003100100416      *   aggiorna sempre
003200100416     c                   movel     DUTE01        uteFAF
003300100416     c                   update    azute000
003400100416      *
003500100416     c                   else
003600100416      *
003700100416     c                   except    riga
003800100416      *
003900100416     c                   end
004000100416      *
004100100416     c                   end
004200100416      *
004300100416     C     TbeCOD        reade     TnTbe01l
004400100416     c                   end
004500100416      *
004600100416      *
004700100420     c                   goto      fine
004800100416      *---------------------------------------------------------------*
004900100416     c     *loval        setll     azute01l
005000100408     c                   read      azute01l
005100100408      *
005200100408     C                   DOw       not %EoF(azute01l)
005300100408     c                   movel     uteFAF        DUTE01
005400100408      *
005500100408     c                   eval      tbeCOD = 'AUT'
005600100408     c                   movel(p)  uteFIL        Fil_Profilo      13
005700100408     c                   move      uteUTE        Fil_Profilo
005800100408     c                   eval      tbeKE1 = Fil_Profilo
005900100408      *
006000100408     C     Tbe01L        setll     TnTbe01l
006100100408     C     Tbe01L        reade     TnTbe01l
006200100408     c                   dow       not %EoF(TnTbe01l)
006300100408      *
006400100408      * Basta che ce ne sia 1 NON ANNULLATO
006500100408     c                   if        tbeATB = *blank
006600100408     c                   move      'S'           §UTETBEAUT
006700100408     c                   leave
006800100408     c                   end
006900100408      *
007000100408     C     Tbe01L        reade     TnTbe01l
007100100408     c                   end
007200100408      *
007300100408      *   aggiorna sempre
007400100408     c                   movel     DUTE01        uteFAF
007500100408     c                   update    azute000
007600100408      *
007700100408     c                   read      azute01l
007800100408     c                   endDO
007900100408      *
008000100420     c     fine          tag
008100000000     C                   SETON                                        LR
008200030926      **-------------------------------------------------------------**
008300100416      *---------------------------------------------------------------*
008400100416     Oqsysprt   E            testata        1 02
008500100416     o                                           10 'TNVRAB01R'
008600100416     Oqsysprt   E            Riga        1
008700100416     o                                            1 ' '
008800100416     O                       uteUTE             + 1
008900100416     O                                          + 2 'Non presente su AZUTE'
009000100416      **
