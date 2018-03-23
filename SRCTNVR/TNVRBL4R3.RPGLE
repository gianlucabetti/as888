000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017      *---------------------------------------------------------------*
000300031017
000400041005     fFnbl400r  if   e           k Disk
000500041005     ftitas30c  if   e           k Disk
000600041005     ftita430c  uf a e           k Disk
000700041005     fQsysprt   o    f  132        Printer
000800041005     d ktrc            s                   like(ta4trc) inz('A')
000900041005     D DTA4A         E DS
001000041005     D DSBL4E        E DS
001100031017
001200041005     ITITAS000      05
001300041005     ITITAS010      06
001400041005     ITITASP00      07
001500041005     ITITA4000      08
001600041005     ITITA4P00      09
001700041005     c     kBl4          klist
001800031017     c                   kfld                    Bl4Aas
001900031017     c                   kfld                    Bl4Lnp
002000031017     c                   kfld                    Bl4Nrs
002100031017     c                   kfld                    Bl4Nsp
002200041005     c     kBl4a         klist
002300041005     c                   kfld                    Bl4Aas
002400041005     c                   kfld                    Bl4Lnp
002500041005     c                   kfld                    Bl4Nrs
002600041005     c                   kfld                    Bl4Nsp
002700041005     c                   kfld                    ktrc
002800041005     c     kBl4b         klist
002900041005     c                   kfld                    Bl4Aas
003000041005     c                   kfld                    Bl4Lnp
003100041005     c                   kfld                    Bl4Nrs
003200041005     c                   kfld                    Bl4Nsp
003300041005     c                   kfld                    bl4trc
003400031017
003500041005     c     *Loval        Setll     Fnbl400r
003600031021do  1c                   Do        *Hival
003700041005     c                   Read      Fnbl400r
003800041005     c                   If        %Eof(Fnbl400r)
003900031017     c                   Leave
004000031017     c                   EndIf
004100041005     c
004200041005     c* Cerco la bolla, se non c'e' non elaboro il record
004300041005     c                   setoff                                       050607
004400041005     c     kbl4          chain     titas30c
004500041005     c                   if        %found(titas30c)
004600041005     c
004700041005     c                   If        Bl4Trc = 'E'
004800041005     c                   movel     bl4not        dsbl4e
004900041005     c                   setoff                                       0809
005000041005     c     kbl4a         chain     tita430c
005100041005     c                   if        %found(tita430c)
005200041005     c                   movel     ta4not        dta4a
005300041005     c                   movel     §b4erp        §ta4arpt
005400041005     c                   movel     dta4a         ta4not
005500041005     c   08              update    tita4000
005600041005     c   09              update    tita4p00
005700041005     c   08              add       1             updta40           7 0
005800041005     c   09              add       1             updta4p           7 0
005900041005     c                   else
006000041005     c  n07              clear                   tita4000
006100041005     c   07              clear                   tita4p00
006200041005     C                   Z-ADD     BL4AAS        TA4AAS
006300041005     C                   Z-ADD     BL4LNP        TA4LNP
006400041005     C                   Z-ADD     BL4NRS        TA4NRS
006500041005     C                   Z-ADD     BL4NSP        TA4NSP
006600041005     C                   MOVEL     'A'           TA4TRC
006700041005     C                   MOVEL     §B4ERP        §TA4arpt
006800041005     C                   MOVEL     DTA4A         TA4NOT
006900041005     C  n07              WRITE     TITA4000
007000041005     C   07              WRITE     TITA4P00
007100041005     c  n07              add       1             receta40          7 0
007200041005     c   07              add       1             receta4p          7 0
007300031021     c                   EndIf
007400041005     c*
007500041005     c                   else
007600041005     c* tio recrod non E
007700041005     c* escludo tipo record A
007800041005     c                   if        bl4trc<>'A'
007900041005     c     kbl4b         chain     tita430c
008000041005     c                   if        not %found(tita430c)
008100041005     c  n07              clear                   tita4000
008200041005     c   07              clear                   tita4p00
008300041005     C                   Z-ADD     BL4AAS        TA4AAS
008400041005     C                   Z-ADD     BL4LNP        TA4LNP
008500041005     C                   Z-ADD     BL4NRS        TA4NRS
008600041005     C                   Z-ADD     BL4NSP        TA4NSP
008700041005     C                   MOVEL     bl4trc        TA4TRC
008800041005     C                   MOVEL     bl4not        TA4NOT
008900041005     C  n07              WRITE     TITA4000
009000041005     C   07              WRITE     TITA4P00
009100041005     c  n07              add       1             sitas0            7 0
009200041005     c   07              add       1             sitasp            7 0
009300041005     c                   else
009400041005     c                   add       1             giata4            7 0
009500041005     c                   endif
009600041005     c                   endif
009700041005     c
009800041005     c                   endif
009900041005     c                   endif
010000041005     c
010100031017
010200031021    1c                   EndDo
010300031017
010400041005     c                   except    testa
010500031017     c                   Eval      *InLr = *On
010600031017
010700031017     oQsysprt   e            Testa          2 02
010800041005     o                                         +  5 'Elabora record ta4 rimasti'
010900031017     o                       *Date         y    110
011000041005     o          e            testa          1
011100041005     o                                           +1 'Scrittura di record "E" :'
011200041005     o                                           +2 ' In TITA4000'
011300041005     o                       receta40      2     +1
011400041005     o                                           +2 ' In TITA4p00'
011500041005     o                       receta4p      2     +1
011600041005     o          e            testa          1
011700041005     o                                           +1 'AGGIORNAM di record "E" :'
011800041005     o                                           +2 ' In TITA4000'
011900041005     o                       updta40       2     +1
012000041005     o                                           +2 ' In TITA4P00'
012100041005     o                       updta4p       2     +1
012200041005     o          e            testa          1
012300041005     o                                           +1 'Scrittura di record non'
012400041005     o                                           +1 '"E" e non "A"'
012500041005     o                                           +2 ' In TITA4000'
012600041005     o                       sitas0        2     +1
012700041005     o                                           +2 ' In TITA4p00'
012800041005     o                       sitasp        2     +1
012900041005     o          e            testa          1
013000041005     o                                           +1 'Record già presenti non'
013100041005     o                                           +1 '"E" e non "A"'
013200041005     o                       giata4        2     +1
