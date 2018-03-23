000100940224     H*--------------------------------------------------------------*
000200060705     FTIdp301L  UF   E           k DISK
000300060704     Ffiar401L  iF   E           k DISK
000400060704     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500060704      *
000600060704     d dsbl4i        e ds
000700060705     d dsbl4f        e ds
000800900131     C*---------------------------------------------------------------*
000900060704     c     kar401        Klist
001000060619     c                   Kfld                    dp3AAS
001100060619     c                   Kfld                    dp3LNP
001200060619     c                   Kfld                    dp3NRS
001300060619     c                   Kfld                    dp3NSP
001400060704     c                   Kfld                    tiprec
001500030205      *
001600030515     c                   do        *hival
001700060704     c                   read      tiDP301L
001800030515     c* fine file
001900060704     c                   if        %eoF(tiDP301L)
002000030515     c                   leave
002100060704     c                   end
002200060704     C*******
002300060705     c*******            if        dp3dtr >20060622
002400060704     c*******            if        dp3lnp >= 190
002500060704     c*******            if        dp3lnp <= 199
002600060704     c*******            if        dp3evd ='02 ' or
002700060704     c*******                      dp3evd ='03 ' or
002800060704     c*******                      dp3evd ='04 ' or
002900060704     c*******                      dp3evd ='08 ' or
003000060704     c*******                      dp3evd ='10 ' or
003100060704     c*******                      dp3evd ='13 '
003200060704     C*
003300060704     c                   clear                   dsbl4i
003400060705     c                   clear                   dsbl4f
003500060619     C*
003600060705     c                   move      'I'           tiprec            1
003700060704     c     kar401        chain     fiar401l
003800060704     c                   if        %Found(fiar401l)
003900060704     c                   movel     ar4not        dsbl4i
004000060705     c                   else
004100060705     c                   move      'F'           tiprec            1
004200060705     c     kar401        chain     fiar401l
004300060705     c                   if        %Found(fiar401l)
004400060705     c                   movel     ar4not        dsbl4f
004500060705     c                   movel     §B4FNP        §B4IPN
004600060705     c                   end
004700060705     c                   end
004800060705     c                   if        %subst(§B4IPN:1:11) <> *blank
004900060704     c                   if        %subst(§B4IPN:13:1) = ' '  AND
005000060704     c                             %subst(DP3PRN:1:11) <> %subst(§B4IPN:1:11)
005100060704     c                   except    diverso
005200060704     c**
005300060704     C                   EVAL      dp3PRN = §B4ipn
005400060705     c                   update    tidp3000
005500060704     c**
005600060705     c                   end
005700060704     c******             end
005800041122     C*
005900060704     c*******            end
006000060704     c*******            end
006100060704     c*******            end
006200060704     c                   end
006300060704     C*
006400030205     c                   enddo
006500000616     C*
006600000000     C                   SETON                                        LR
006700060704     C*
006800060704     C*
006900060704     OQSYSPRT   E            TESTA            01
007000060704     O*
007100060704     O          E            DIVERSO     1
007200060704     o                                           10 'Dp3 - Ar4'
007300060704     O                       dp3PRN             + 2
007400060704     O                       §B4IPN             + 5
007500060704     O                       dp3aas             + 5
007600060704     O                       dp3lnp             + 1
007700060704     O                       dp3nrs             + 1
007800060704     O                       dp3nsp             + 1
007900060704     O                       dp3evd             + 5
008000060704     O                       dp3dtr             + 2
008100060704     O                       dp3ftr             + 1
008200060704     o
