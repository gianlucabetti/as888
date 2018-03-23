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
004700150319      *-- se IMPORTIAMO DPD   ------ Facciamo fare al FIEU78R0 ------*
004800150319      *--------------------------------------------------------------*
004900150319      *                         ====================
005000150319     c                   if        TpNtw = 'DPD'
005100150319      *                         ====================
005200150319      *
005300150319     c                   call      'FIEU78R0'
005400150319     c                   parm                    kpjba
005500150319      *
005600150319      *   e saltiamo questo programma
005700150319      *               ====================
005800150319     c                   goto      FINE
005900150319      *               ====================
006000150319     c                   end
006100150319      *--------------------------------------------------------------*
006200050907      *  carica AZORG una volta per tutte:
006300050907     c                   clear                   oFil
006400050907     c                   clear                   oDe3
006500050907     c                   open      AzOrg01L
006600050907     c     *loval        setll     AzOrg01L
006700050907     c                   read      AzOrg01L
006800050907     c                   DOW       not %EoF(AzOrg01L)
006900050907     c                   add       1             og
007000050907     c                   move      orgfil        oFIL(og)
007100050907     c                   move      orgde3        oDE3(og)
007200050907     c                   read      AzOrg01L
007300050907     c                   enddo
007400050907     c                   close     AzOrg01L
007500050907      *--------------------------------------------------------------*
007600041103     c     Kdadata       klist
007700041103     c                   kfld                    daAnno            4 0
007800041103     c                   kfld                    daMgs             4 0
007900041103      *
008000041103     c                   movel     DaData        daAnno
008100041103     c                   move      DaData        daMgs
008200041103      *
008300041103     c     kdadata       setll     titas38c
008400050907     c                   read      titas38c
008500041103      *
008600041103     c                   dow       not %Eof(titas38c)
008700041103      *   oltre il limite
008800041103     c                   if        tasAAS > aAnno  or
008900041103     c                             tasMGS > aMesG
009000041103     C                   Leave
009100041103     C                   End
009200041103      *
009300041103     c                   clear                   ok                2
009400041103      *
009500041103     C                   clear                   OG143
009600050907     c                   z-add     1             og
009700050907     c     tasLNP        lookup    Ofil(og)                               99
009800080404     c                   if        %Equal
009900080404     c****               if        %Found
010000050907     C                   movel     Ode3(og)      OG143
010100041103     c                   end
010200041103      *
010300041103     c                   if         §ogNTW = 'FED' or
010400041103     c                              §ogNTW = 'EEX' or
010500041103     c                              §ogNTW = 'DPD'
010600041103     c                   if        §ogNTW <> 'EEX' and §ogNTW = TpNtw or
010700041103     c                             §ogNTW  = 'EEX' and TpNtw = %char(tasLNP)
010800041103     c                   move      'OK'          ok
010900041103     c                   end
011000041103     C                   Else
011100041103      *
011200041103     C                   clear                   OG143
011300050907     c                   z-add     1             og
011400050907     c     tasLNA        lookup    Ofil(og)                               99
011500080404     c                   if        %Equal
011600080404     c****               if        %Found
011700050907     C                   movel     Ode3(og)      OG143
011800041103     c                   if         §ogNTW = 'FED' or
011900041103     c                              §ogNTW = 'EEX' or
012000041103     c                              §ogNTW = 'DPD'
012100041103     c                   if        §ogNTW <> 'EEX' and §ogNTW = TpNtw or
012200041103     c                             §ogNTW  = 'EEX' and TpNtw = %char(tasLNA)
012300041103     c                   move      'OK'          ok
012400041103     c                   end
012500041103     c                   end
012600041103     c                   end
012700041103      *
012800041103     C                   EndIF
012900080407      *
013000080407      *  Controlla x DPD di non prendere le "A3" e le "F3"
013100080407     c                   if         §ogNTW = 'DPD'
013200080407     c                   if         tasTBL = 'A3' or
013300080407     c                              tasTBL = 'F3'
013400080407     c                   clear                   ok
013500080407     C                   EndIF
013600080617      *
013700080617      *   e le bolle con consegna anomala "7" di "Merce mai affidata"
013800080617     c                   if         tasCCA = '7'
013900080617     c                   clear                   ok
014000080617     C                   EndIF
014100080407     C                   EndIF
014200080407      *
014300041103      * se tutto OK
014400041103     c                   if        ok = 'OK'
014500041103     c                   clear                   kpjbu
014600041103     c                   movel     TpNtw         KNtw
014700050907     c                   movel     tasAAS        tasAASprm
014800050907     c                   movel     tasLnp        tasLnpprm
014900050907     c                   movel     tasNrs        tasNrsprm
015000050907     c                   movel     tasNsp        tasNspprm
015100050907     c                   movel     tasTbl        tasTblprm
015200041103     c                   movel     parBolla      kpjbu
015300041103     c                   call      'FIEU78R1'
015400041103     c                   parm                    kpjba
015500050907     c                   parm                    DsTAS
015600041103     C                   EndIF
015700041103      *
015800050907     c                   read      titas38c
015900041103     C                   Enddo
016000041103      *
016100041103     c                   clear                   kpjbu
016200041103     c                   movel     pardate       kpjbu
016300080619      *
016400080619      * Accorpamento dei Resi sulla bolla originale
016500080619     c                   call      'FIEU78R2'
016600080619     c                   parm                    kpjba
016700041103      *
016800161298      *----------------------------------------------------------------
016900040909     c     FINE          tag
017000030620     c                   seton                                        LR
