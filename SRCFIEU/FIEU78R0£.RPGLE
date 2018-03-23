000100030620     ************************************************************************
000200150316      *   Genera Record di controllo Fatture D P D                          *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600030625      *  solo Bolle Fatturate (per provare procedura)
000700150316     Ftitas26c  iF   E           k DISK
000800150403      *  x verificare se già presente
000900150403     Ftnecf02l  iF   E           k DISK
001000050907     FAzOrg01L  iF   E           k DISK    usropn
001100040909      *****************************************************************
001200050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
001300030620      *
001400030624     D KPJBA         E DS
001500030625     D OG143         E DS
001600040909     D*
001700040909     D WLBDAT          DS
001800040909     D  G02DAT                 1      8  0
001900040909     D  G02INV                 9     16  0
002000040909     D  G02ERR                17     17
002100040909     D  G02TGI                18     22  0
002200040909      *
002300030625     D parDate         DS
002400030625     D   DaData                1      8  0
002500030625     D    aData                9     16  0
002600030625     D    aAnno                9     12  0
002700030625     D    aMesG               13     16  0
002800030703     D   TpNtw                17     19
002900030703     D   TpData               20     20
003000030625      *
003100030625     D parBolla        DS
003200050907     D   tasAASprm             1      4  0
003300050907     D   tasLnpprm             5      7  0
003400050907     D   tasNrsprm             8      9  0
003500050907     D   tasNspprm            10     16  0
003600050907     D   tasTblprm            17     18
003700030703     D   KNtw                 19     21
003800041013      *
003900050907     D  Ofil           s                   like(orgfil) DIM(999)
004000050907     D  Ode3           s                   like(orgde3) DIM(999)
004100050907     d  og             s              5  0
004200030620      *---------------------------------------------------------------------*
004300030624     C     *entry        plist
004400030624     C                   parm                    kpjba
004500041103      *--------------------------------------------------------------*
004600041103     c                   movel     kpjbu         pardate
004700041103      *
004800050907      *--------------------------------------------------------------*
004900050907      *  carica AZORG una volta per tutte:
005000050907     c                   clear                   oFil
005100050907     c                   clear                   oDe3
005200050907     c                   open      AzOrg01L
005300050907     c     *loval        setll     AzOrg01L
005400050907     c                   read      AzOrg01L
005500050907     c                   DOW       not %EoF(AzOrg01L)
005600050907     c                   add       1             og
005700050907     c                   move      orgfil        oFIL(og)
005800050907     c                   move      orgde3        oDE3(og)
005900050907     c                   read      AzOrg01L
006000050907     c                   enddo
006100050907     c                   close     AzOrg01L
006200050907      *--------------------------------------------------------------*
006300041103     c     Kdadata       klist
006400041103     c                   kfld                    daAnno            4 0
006500041103     c                   kfld                    daMgs             4 0
006600041103      *
006700150403     c     KECF2         klist
006800150403     c                   kfld                    tasAAS
006900150403     c                   kfld                    tasLNP
007000150403     c                   kfld                    tasNRS
007100150403     c                   kfld                    tasNSP
007200150403     c                   kfld                    gruppo            3
007300150403     c                   move      '001'         gruppo
007400150403      *
007500041103     c                   movel     DaData        daAnno
007600041103     c                   move      DaData        daMgs
007700041103      *
007800150316     c     DAdata        setll     titas26c
007900150316     c                   read      titas26c
008000041103      *
008100150316     c                   dow       not %Eof(titas26c)
008200041103      *   oltre il limite
008300150316     c                   if        tasDCM > aDATA
008400041103     C                   Leave
008500041103     C                   End
008600041103      *
008700041103     c                   clear                   ok                2
008800041103      *
008900041103     C                   clear                   OG143
009000050907     c                   z-add     1             og
009100050907     c     tasLNP        lookup    Ofil(og)                               99
009200080404     c                   if        %Equal
009300080404     c****               if        %Found
009400050907     C                   movel     Ode3(og)      OG143
009500041103     c                   end
009600041103      *
009700150316     c                   IF         §ogNTW = 'DPD'
009800041103     c                   move      'OK'          ok
009900041103     C                   Else
010000041103      *
010100041103     C                   clear                   OG143
010200050907     c                   z-add     1             og
010300050907     c     tasLNA        lookup    Ofil(og)                               99
010400080404     c                   if        %Equal
010500080404     c****               if        %Found
010600050907     C                   movel     Ode3(og)      OG143
010700150316     c                   IF         §ogNTW = 'DPD'
010800041103     c                   move      'OK'          ok
010900041103     c                   end
011000041103     c                   end
011100041103      *
011200041103     C                   EndIF
011300080407      *
011400080407      *  Controlla x DPD di non prendere le "A3" e le "F3"
011500080407     c                   if         §ogNTW = 'DPD'
011600080407     c                   if         tasTBL = 'A3' or
011700080407     c                              tasTBL = 'F3'
011800080407     c                   clear                   ok
011900080407     C                   EndIF
012000080617      *
012100080617      *   e le bolle con consegna anomala "7" di "Merce mai affidata"
012200150319      *    adesso anche "5"
012300080617     c                   if         tasCCA = '7'
012400150316     c                               or
012500150316     c                              tasCCA = '5'
012600080617     c                   clear                   ok
012700080617     C                   EndIF
012800080407     C                   EndIF
012900080407      *
013000150403      *   DEVE controllare la presenza della bolla su TNECF
013100150403      *    già presente su TNECF per un precedente caricamento.
013200150403     c     KECF2         chain     tnECF02L
013300150403     c                   if        %Found(tnECF02L)
013400150403      * se c'è lo deve saltare
013500150403     c                   clear                   ok
013600150403     C                   EndIF
013700041103      * se tutto OK
013800041103     c                   if        ok = 'OK'
013900041103     c                   clear                   kpjbu
014000041103     c                   movel     TpNtw         KNtw
014100050907     c                   movel     tasAAS        tasAASprm
014200050907     c                   movel     tasLnp        tasLnpprm
014300050907     c                   movel     tasNrs        tasNrsprm
014400050907     c                   movel     tasNsp        tasNspprm
014500050907     c                   movel     tasTbl        tasTblprm
014600041103     c                   movel     parBolla      kpjbu
014700041103     c                   call      'FIEU78R1'
014800041103     c                   parm                    kpjba
014900050907     c                   parm                    DsTAS
015000041103     C                   EndIF
015100041103      *
015200150316     c                   read      titas26c
015300041103     C                   Enddo
015400041103      *
015500041103     c                   clear                   kpjbu
015600041103     c                   movel     pardate       kpjbu
015700080619      *
015800080619      * Accorpamento dei Resi sulla bolla originale
015900080619     c                   call      'FIEU78R2'
016000080619     c                   parm                    kpjba
016100041103      *
016200161298      *----------------------------------------------------------------
016300040909     c     FINE          tag
016400030620     c                   seton                                        LR
