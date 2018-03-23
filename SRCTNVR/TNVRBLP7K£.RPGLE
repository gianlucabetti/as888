000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300091125     FFNblp31L  iF   E           K DISK
000400091125     FFNblt01l  iF   E           K DISK
000500091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600091125
000700091125     ddiffvol          s              9  3
000800091125     ddiffpes          s              9  3
000900091125     dtot              s             15  3
001000091125     dperc             s              9  2
001100091125     dpercs            s              4  0
001200091125     dpercsneg         s              4  0
001300021214      *--------------------------------------------------
001400070913     c     kblp          klist
001500091125     c                   kfld                    lnp               3 0
001600091125     c                   kfld                    aas               4 0
001700091125     c                   kfld                    mgs               4 0
001800100111     c                   z-add     255           lnp
001900100111     c                   z-add     2010          aas
002000100111     c                   z-add     0107          mgs
002100091125
002200091125     c     kblt          klist
002300091125     c                   kfld                    blpaas
002400091125     c                   kfld                    blplnp
002500091125     c                   kfld                    blpnrs
002600091125     c                   kfld                    blpnsp
002700070913     c
002800091125     c     kblp          setll     fnblp31l
002900091125     c     kblp          reade     fnblp31l
003000051110
003100091125    1c                   DOW       not %EOF(FNblp31l)
003200100111     c*
003300100111     c                   if        blpksc=0633315 and blpduc>20100107
003400100111     c*  Col  tnsd99 verifico che data cons richeista mettere
003500100111     c                   clear                   tnsd99ds
003600100111     c                   eval      d99lnp=blpaas
003700100111     c
003800091125     c* non controllo volumi automatici
003900091125     c                   if        blpfvb='R'
004000091125     C                   EVAL      DIFFVOL=BLPVLC-BLPVLB
004100091125     c
004200091125     c                   eval      tot=diffvol*100
004300091125     c                   eval(H)   perc=tot/blpvlb
004400091125     c
004500091125     c                   if        perc>percs or perc<percsneg
004600091125
004700091125     c* considero solo se rilevato in partenza
004800091125     c     kblt          setll     fnblt01l
004900091125     c     kblt          reade     fnblt01l
005000091125     c                   dow       not %eof(fnblt01l)
005100091125     c                   if        bltfvc='P'
005200091125     c                   except    errore
005300091125     c                   leave
005400091125     c                   endif
005500091125     c     kblt          reade     fnblt01l
005600091125     c                   enddo
005700091125     c
005800091125     c                   endif
005900091125     c                   endif
006000091125     c                   endif
006100091125
006200091125     c                   if        (blppkc>0  and blpncp=blpncl) or
006300091125     c                             (blppkc>blppkb)
006400091125     C                   EVAL      DIFFpes=BLPpkC-BLPpkB
006500091125     c                   eval      tot=diffpes*100
006600091125     c                   eval(H)   perc=tot/blppkb
006700091125     c
006800091125     c                   if        perc>percs or perc<percsneg
006900091125
007000091125     c* considero solo se rilevato in partenza
007100091125     c     kblt          setll     fnblt01l
007200091125     c     kblt          reade     fnblt01l
007300091125     c                   dow       not %eof(fnblt01l)
007400091125     c                   if        bltfpc='P'
007500091125     c                   except    errorep
007600091125     c                   leave
007700091125     c                   endif
007800091125     c     kblt          reade     fnblt01l
007900091125     c                   enddo
008000091125     c
008100091125     c                   endif
008200091125     c                   endif
008300091125     c
008400091125     c     kblp          reade     fnblp31l
008500091125     c                   enddo
008600021216      *
008700021214     C                   eval       *inlr = *on
008800091125     OQSYSPRT   E            errore      1
008900091125     O                                              'Bolla:'
009000091125     O                       blpaas           +   1
009100091125     O                       blplnp           +   1
009200091125     O                       blpnrs           +   1
009300091125     O                       blpnsp        2  +   1
009400091125     O                                        +   2 'volume bollettato'
009500091125     O                       blpvlb        2  +   1
009600091125     O                                        +   5 'VDL'
009700091125     O                       blpvlc        2  +   1
009800091125     O                                        +   5 'Perc scostamento'
009900091125     o                       perc          L  +   1
010000091125     OQSYSPRT   E            errorep     1
010100091125     O                                              'Bolla:'
010200091125     O                       blpaas           +   1
010300091125     O                       blplnp           +   1
010400091125     O                       blpnrs           +   1
010500091125     O                       blpnsp        2  +   1
010600091125     O                                        +   2 'peso   bollettato'
010700091125     O                       blppkb        2  +   1
010800091125     O                                        +   2 'VDL'
010900091125     O                       blppkc        2  +   1
011000091125     O                                        +   2 'Perc scostamento'
011100091125     o                       perc          L  +   1
