000100030620     ************************************************************************
000200150316      *   Genera Record di controllo Fatture D P D                          *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030625      *  solo Bolle Fatturate (per provare procedura)
000700150316     Ftitas26c  iF   E           k DISK
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
006800150316     c     DAdata        setll     titas26c
006900150316     c                   read      titas26c
007000041103      *
007100150316     c                   dow       not %Eof(titas26c)
007200041103      *   oltre il limite
007300150316     c                   if        tasDCM > aDATA
007400041103     C                   Leave
007500041103     C                   End
007600041103      *
007700041103     c                   clear                   ok                2
007800041103      *
007900041103     C                   clear                   OG143
008000050907     c                   z-add     1             og
008100050907     c     tasLNP        lookup    Ofil(og)                               99
008200080404     c                   if        %Equal
008300080404     c****               if        %Found
008400050907     C                   movel     Ode3(og)      OG143
008500041103     c                   end
008600041103      *
008700150316     c                   IF         §ogNTW = 'DPD'
008800041103     c                   move      'OK'          ok
008900041103     C                   Else
009000041103      *
009100041103     C                   clear                   OG143
009200050907     c                   z-add     1             og
009300050907     c     tasLNA        lookup    Ofil(og)                               99
009400080404     c                   if        %Equal
009500080404     c****               if        %Found
009600050907     C                   movel     Ode3(og)      OG143
009700150316     c                   IF         §ogNTW = 'DPD'
009800041103     c                   move      'OK'          ok
009900041103     c                   end
010000041103     c                   end
010100041103      *
010200041103     C                   EndIF
010300080407      *
010400080407      *  Controlla x DPD di non prendere le "A3" e le "F3"
010500080407     c                   if         §ogNTW = 'DPD'
010600080407     c                   if         tasTBL = 'A3' or
010700080407     c                              tasTBL = 'F3'
010800080407     c                   clear                   ok
010900080407     C                   EndIF
011000080617      *
011100080617      *   e le bolle con consegna anomala "7" di "Merce mai affidata"
011200150319      *    adesso anche "5"
011300080617     c                   if         tasCCA = '7'
011400150316     c                               or
011500150316     c                              tasCCA = '5'
011600080617     c                   clear                   ok
011700080617     C                   EndIF
011800080407     C                   EndIF
011900080407      *
012000041103      * se tutto OK
012100041103     c                   if        ok = 'OK'
012200041103     c                   clear                   kpjbu
012300041103     c                   movel     TpNtw         KNtw
012400050907     c                   movel     tasAAS        tasAASprm
012500050907     c                   movel     tasLnp        tasLnpprm
012600050907     c                   movel     tasNrs        tasNrsprm
012700050907     c                   movel     tasNsp        tasNspprm
012800050907     c                   movel     tasTbl        tasTblprm
012900041103     c                   movel     parBolla      kpjbu
013000041103     c                   call      'FIEU78R1'
013100041103     c                   parm                    kpjba
013200050907     c                   parm                    DsTAS
013300041103     C                   EndIF
013400041103      *
013500150316     c                   read      titas26c
013600041103     C                   Enddo
013700041103      *
013800041103     c                   clear                   kpjbu
013900041103     c                   movel     pardate       kpjbu
014000080619      *
014100080619      * Accorpamento dei Resi sulla bolla originale
014200080619     c                   call      'FIEU78R2'
014300080619     c                   parm                    kpjba
014400041103      *
014500161298      *----------------------------------------------------------------
014600040909     c     FINE          tag
014700030620     c                   seton                                        LR
