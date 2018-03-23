000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400981216     H DECEDIT('0,') DATEDIT(*DMY.)
000500161298      *---------------------------------------------------------------------*
000600160204     FTitas00f  iF   E             DISK
000700160203      *
000800030624     D KPJBA         E DS
000900030625      *
001000030625     D WLBDAT          DS
001100030625     D  G02DAT                 1      8  0
001200030625     D  G02INV                 9     16  0
001300030625     D  G02ERR                17     17
001400030625     D  G02TGI                18     22  0
001500040902      *
001600040902     D  data_Oggi      s                   like(G02INV) INZ(0)
001700160204      *
001800160204     D parBolla        DS
001900160204     D   tasAASprm             1      4  0
002000160204     D   tasLnpprm             5      7  0
002100160204     D   tasNrsprm             8      9  0
002200160204     D   tasNspprm            10     16  0
002300160204     D   tasTblprm            17     18
002400160204     D   KNtw                 19     21
002500160204      *
002600060303      *
002700050907     D DsTAS         E DS                  EXTNAME(TITAS00F)
002800030721      *---------------------------------------------------------------------*
002900030624     C     *entry        plist
003000030624     C                   parm                    kpjba
003100030627      *
003200160204     c                   read      titas00f
003300160204      *
003400160204     c                   dow       not %Eof(titas00f)
003500160204      *
003600160204     c                   clear                   kpjbu
003700160204     c                   movel     'DPD'         KNtw
003800160204     c                   movel     tasAAS        tasAASprm
003900160204     c                   movel     tasLnp        tasLnpprm
004000160204     c                   movel     tasNrs        tasNrsprm
004100160204     c                   movel     tasNsp        tasNspprm
004200160204     c                   movel     tasTbl        tasTblprm
004300160204     c                   movel     parBolla      kpjbu
004400160204      *
004500160204     c                   call      'FIEU78R5'
004600160204     c                   parm                    kpjba
004700160204     c                   parm                    DsTAS
004800160204      *
004900160204     c                   read      titas00f
005000160204     c                   enddo
005100160204      *
005200160204     c                   seton                                        LR
005300160204      *---------------------------------------------------------------------*
