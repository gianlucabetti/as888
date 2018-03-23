000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030625      *  solo Bolle Fatturate (per provare procedura)
000700030625     Ftitas38c  iF   E           k DISK
000800050907     FAzOrg01L  iF   E           k DISK    usropn
000900040909      *****************************************************************
001000050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
001100030620      *
001200030624     D KPJBA         E DS
001300030625     D OG143         E DS
001400040909     D*
001500040909     D WLBDAT          DS
001600040909     D  G02DAT                 1      8  0
001700040909     D  G02INV                 9     16  0
001800040909     D  G02ERR                17     17
001900040909     D  G02TGI                18     22  0
002000040909      *
002100030625     D parDate         DS
002200030625     D   DaData                1      8  0
002300030625     D    aData                9     16  0
002400030625     D    aAnno                9     12  0
002500030625     D    aMesG               13     16  0
002600030703     D   TpNtw                17     19
002700030703     D   TpData               20     20
002800030625      *
002900030625     D parBolla        DS
003000050907     D   tasAASprm             1      4  0
003100050907     D   tasLnpprm             5      7  0
003200050907     D   tasNrsprm             8      9  0
003300050907     D   tasNspprm            10     16  0
003400050907     D   tasTblprm            17     18
003500030703     D   KNtw                 19     21
003600041013      *
003700050907     D  Ofil           s                   like(orgfil) DIM(999)
003800050907     D  Ode3           s                   like(orgde3) DIM(999)
003900050907     d  og             s              5  0
004000030620      *---------------------------------------------------------------------*
004100030624     C     *entry        plist
004200030624     C                   parm                    kpjba
004300041103      *--------------------------------------------------------------*
004400041103     c                   movel     kpjbu         pardate
004500041103      *
004600050907      *--------------------------------------------------------------*
004700050907      *  carica AZORG una volta per tutte:
004800050907     c                   clear                   oFil
004900050907     c                   clear                   oDe3
005000050907     c                   open      AzOrg01L
005100050907     c     *loval        setll     AzOrg01L
005200050907     c                   read      AzOrg01L
005300050907     c                   DOW       not %EoF(AzOrg01L)
005400050907     c                   add       1             og
005500050907     c                   move      orgfil        oFIL(og)
005600050907     c                   move      orgde3        oDE3(og)
005700050907     c                   read      AzOrg01L
005800050907     c                   enddo
005900050907     c                   close     AzOrg01L
006000050907      *--------------------------------------------------------------*
006100041103     c     Kdadata       klist
006200041103     c                   kfld                    daAnno            4 0
006300041103     c                   kfld                    daMgs             4 0
006400041103      *
006500041103     c                   movel     DaData        daAnno
006600041103     c                   move      DaData        daMgs
006700041103      *
006800041103     c     kdadata       setll     titas38c
006900050907     c                   read      titas38c
007000041103      *
007100041103     c                   dow       not %Eof(titas38c)
007200041103      *   oltre il limite
007300041103     c                   if        tasAAS > aAnno  or
007400041103     c                             tasMGS > aMesG
007500041103     C                   Leave
007600041103     C                   End
007700041103      *
007800041103     c                   clear                   ok                2
007900041103      *
008000041103     C                   clear                   OG143
008100050907     c                   z-add     1             og
008200050907     c     tasLNP        lookup    Ofil(og)                               99
008300080404     c                   if        %Equal
008400080404     c****               if        %Found
008500050907     C                   movel     Ode3(og)      OG143
008600041103     c                   end
008700041103      *
008800041103     c                   if         §ogNTW = 'FED' or
008900041103     c                              §ogNTW = 'EEX' or
009000041103     c                              §ogNTW = 'DPD'
009100041103     c                   if        §ogNTW <> 'EEX' and §ogNTW = TpNtw or
009200041103     c                             §ogNTW  = 'EEX' and TpNtw = %char(tasLNP)
009300041103     c                   move      'OK'          ok
009400041103     c                   end
009500041103     C                   Else
009600041103      *
009700041103     C                   clear                   OG143
009800050907     c                   z-add     1             og
009900050907     c     tasLNA        lookup    Ofil(og)                               99
010000080404     c                   if        %Equal
010100080404     c****               if        %Found
010200050907     C                   movel     Ode3(og)      OG143
010300041103     c                   if         §ogNTW = 'FED' or
010400041103     c                              §ogNTW = 'EEX' or
010500041103     c                              §ogNTW = 'DPD'
010600041103     c                   if        §ogNTW <> 'EEX' and §ogNTW = TpNtw or
010700041103     c                             §ogNTW  = 'EEX' and TpNtw = %char(tasLNA)
010800041103     c                   move      'OK'          ok
010900041103     c                   end
011000041103     c                   end
011100041103     c                   end
011200041103      *
011300041103     C                   EndIF
011400080407      *
011500080407      *  Controlla x DPD di non prendere le "A3" e le "F3"
011600080407     c                   if         §ogNTW = 'DPD'
011700080407     c                   if         tasTBL = 'A3' or
011800080407     c                              tasTBL = 'F3'
011900080407     c                   clear                   ok
012000080407     C                   EndIF
012100080617      *
012200080617      *   e le bolle con consegna anomala "7" di "Merce mai affidata"
012300080617     c                   if         tasCCA = '7'
012400080617     c                   clear                   ok
012500080617     C                   EndIF
012600080407     C                   EndIF
012700080407      *
012800041103      * se tutto OK
012900041103     c                   if        ok = 'OK'
013000041103     c                   clear                   kpjbu
013100041103     c                   movel     TpNtw         KNtw
013200050907     c                   movel     tasAAS        tasAASprm
013300050907     c                   movel     tasLnp        tasLnpprm
013400050907     c                   movel     tasNrs        tasNrsprm
013500050907     c                   movel     tasNsp        tasNspprm
013600050907     c                   movel     tasTbl        tasTblprm
013700041103     c                   movel     parBolla      kpjbu
013800041103     c                   call      'FIEU78R1'
013900041103     c                   parm                    kpjba
014000050907     c                   parm                    DsTAS
014100041103     C                   EndIF
014200041103      *
014300050907     c                   read      titas38c
014400041103     C                   Enddo
014500041103      *
014600041103     c                   clear                   kpjbu
014700041103     c                   movel     pardate       kpjbu
014800080619      *
014900080619      * Accorpamento dei Resi sulla bolla originale
015000080619     c                   call      'FIEU78R2'
015100080619     c                   parm                    kpjba
015200041103      *
015300161298      *----------------------------------------------------------------
015400040909     c     FINE          tag
015500030620     c                   seton                                        LR
