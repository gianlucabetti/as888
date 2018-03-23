000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030625      *  solo Bolle Fatturate (per provare procedura)
000700030625     Ftitas38c  iF   E           k DISK
000800030625     FAzOrg01L  iF   E           k DISK
000900030620      *
001000030625     D OG143         E DS
001100030625      *
001200030625     D parDate         DS
001300030625     D   DaData                1      8  0
001400030625     D    aData                9     16  0
001500030625     D    aAnno                9     12  0
001600030625     D    aMesG               13     16  0
001700030703     D   TpNtw                17     19
001800030703     D   TpData               20     20
001900030625      *
002000030625     D parBolla        DS
002100030625     D   tasAAS                1      4  0
002200030625     D   tasLnp                5      7  0
002300030625     D   tasNrs                8      9  0
002400030625     D   tasNsp               10     16  0
002500030625     D   tasTbl               17     18
002600030703     D   KNtw                 19     21
002700030620      *---------------------------------------------------------------------*
002800030624     C     *entry        plist
002900040413     c                   parm                    pardate
003000030625      *
003100030625     c     Kdadata       klist
003200030625     c                   kfld                    daAnno            4 0
003300030625     c                   kfld                    daMgs             4 0
003400030625      *
003500030625     c                   movel     DaData        daAnno
003600030625     c                   move      DaData        daMgs
003700030624      *
003800030625     c     kdadata       setll     titas38c
003900030625     c                   read      titas38c
004000030624      *
004100030625     c                   dow       not %Eof(titas38c)
004200030625      *   oltre il limite
004300030625     c                   if        tasAAS > aAnno  or
004400030625     c                             tasMGS > aMesG
004500030625     C                   Leave
004600030625     C                   End
004700030625      *
004800030625     c                   clear                   ok                2
004900030625      *
005000030625     C                   clear                   OG143
005100030625     c     tasLNP        chain     AzOrg01L
005200030625     c                   if        %Found(AzOrg01L)
005300030625     C                   movel     ORGDE3        OG143
005400030625     c                   end
005500030625      *
005600030625     c                   if         §ogNTW = 'FED' or
005700030625     c                              §ogNTW = 'EEX' or
005800030625     c                              §ogNTW = 'DPD'
005900030625     c                   move      'OK'          ok
006000030625     C                   Else
006100030625      *
006200030625     C                   clear                   OG143
006300030625     c     tasLNA        chain     AzOrg01L
006400030625     c                   if        %Found(AzOrg01L)
006500030625     C                   movel     ORGDE3        OG143
006600030625     c                   if         §ogNTW = 'FED' or
006700030625     c                              §ogNTW = 'EEX' or
006800030625     c                              §ogNTW = 'DPD'
006900030625     c                   move      'OK'          ok
007000030625     c                   end
007100030625     c                   end
007200030625      *
007300030625     C                   EndIF
007400030625      * se tutto OK
007500030625     c                   if        ok = 'OK'
007600030703     c                   movel     TpNtw         KNtw
007700040819     c                   call      'FIEU68R'
007800040413     c                   parm                    parBolla
007900030625     C                   EndIF
008000030625      *
008100030625     c                   read      titas38c
008200030624     C                   Enddo
008300030624      *
008400161298      *----------------------------------------------------------------
008500030620     c                   seton                                        LR
