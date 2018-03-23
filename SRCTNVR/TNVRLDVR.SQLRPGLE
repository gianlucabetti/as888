000201130626     D tnsb30ds      E DS
000202130626     D KPJBA         E DS
000203130626     C     *ENTRY        PLIST                                                  *
000204130626     C                   PARM                    KPJBA                          *
011609130625     c*
011610130626     c                   do        11            y                 2 0
011611130626     c                   z-add     1             x                 2 0
011612130626     c                   do        21            x
011613130626     C                   CLEAR                   TNSB30DS
011614130626     c                   if        x = 1 and y = 1
011615130626     c                   iter
011616130626     c                   end
011617130626     c                   select
011618130626     c                   when      x= 1
011619130626     C                   EVAL      sb30ddc = 20130620
011620130626     c                   when      x= 2
011621130626     C                   EVAL      sb30ddc = 20130604
011622130626     c                   when      x= 3
011623130626     C                   EVAL      sb30ddc = 20130521
011624130626     c                   when      x= 4
011625130626     C                   EVAL      sb30ddc = 20130509
011626130626     c                   when      x= 5
011627130626     C                   EVAL      sb30ddc = 20130503
011628130626     c                   when      x= 6
011629130626     C                   EVAL      sb30ddc = 20130429
011630130626     c                   when      x= 7
011631130626     C                   EVAL      sb30ddc = 20130418
011632130626     c                   when      x= 8
011633130626     C                   EVAL      sb30ddc = 20130415
011634130626     c                   when      x= 9
011635130626     C                   EVAL      sb30ddc = 20130409
011636130626     c                   when      x= 10
011637130626     C                   EVAL      sb30ddc = 20130327
011638130626     c                   when      x= 11
011639130626     C                   EVAL      sb30ddc = 20130319
011640130626     c                   when      x= 12
011641130626     C                   EVAL      sb30ddc = 20130314
011642130626     c                   when      x= 13
011643130626     C                   EVAL      sb30ddc = 20130305
011644130626     c                   when      x= 14
011645130626     C                   EVAL      sb30ddc = 20130226
011646130626     c                   when      x= 15
011647130626     C                   EVAL      sb30ddc = 20130218
011648130626     c                   when      x= 16
011649130626     C                   EVAL      sb30ddc = 20130213
011650130626     c                   when      x= 17
011651130626     C                   EVAL      sb30ddc = 20130207
011652130626     c                   when      x= 18
011653130626     C                   EVAL      sb30ddc = 20130130
011654130626     c                   when      x= 19
011655130626     C                   EVAL      sb30ddc = 20130122
011656130626     c                   when      x= 20
011657130626     C                   EVAL      sb30ddc = 20130117
011658130626     c                   when      x= 21
011659130626     C                   EVAL      sb30ddc = 20130104
011660130626     c                   endsl
011661130626     c                   select
011662130626     c                   when      y= 1
011663130626     C                   EVAL      sb30pdr = 0020543
011664130626     c                   when      y= 2
011665130626     C                   EVAL      sb30pdr = 0020518
011666130626     c                   when      y= 3
011667130626     C                   EVAL      sb30pdr = 0020821
011668130626     c                   when      y= 4
011669130626     C                   EVAL      sb30pdr = 1350382
011670130626     c                   when      y= 5
011671130626     C                   EVAL      sb30pdr = 1350361
011672130626     c                   when      y= 6
011673130626     C                   EVAL      sb30pdr = 1350247
011674130626     c                   when      y= 7
011675130626     C                   EVAL      sb30pdr = 1350327
011676130626     c                   when      y= 8
011677130626     C                   EVAL      sb30pdr = 1350342
011678130626     c                   when      y= 9
011679130626     C                   EVAL      sb30pdr = 1350353
011680130626     c                   when      y= 10
011681130626     C                   EVAL      sb30pdr = 1350358
011682130626     c                   when      y= 11
011683130626     C                   EVAL      sb30pdr = 1350370
011684130626     c                   endsl
011685130626     c                   EVAL      SB30REC = 'S'
011686130626     c                   EVAL      SB30RES = 'S'
011687130626     c                   EVAL      SB30CSR = 'S'
011688130625     C                   EVAL      KPJBU = TNSB30DS
011689130625     C                   CALL      'TNSB31R'
011690130625     C                   PARM                    KPJBA
011691130626     c                   enddo
011692130626     c                   enddo
017000130625     C                   seton                                        lr
