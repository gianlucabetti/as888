000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
001400060208      * Deve allocarsi il lavoro
001500060125      *****************************************************************
003100030429      * Per Controllare che non ci siano le trasmissioni in sede in corso
003200030429      *  durante l'apertura di questo programma si scrive un record su questo file.
003201060208     **
003300030430     Ffiwalc1l  UF A E           K DISK    commit
003301060125     **
003400030429     F*---------------------------------------------------------------------------------
003500000607     D kpjba         e ds
003501060208     D cncr80        E DS
003502060208     D UT§DSE0F      E DS
004603060125      **
005200030429     D WLBDAT          DS
005300030429     D  G02DAT                 1      8  0
005400030429     D  G02INV                 9     16  0
005500030429     D  G02ERR                17     17
005600030429     D  G02TGI                18     22  0
005700030429      *
006103060125     d dataoggi        s               d   datfmt(*eur)
006104060125     d data_gg1        s               d   datfmt(*iso)
007800020205     c**********************************************************************
007801060208      * ?Alloca scrivendo un record nel Fiwalc1L per impedire che ci siano dei
007802060208      * ? conflitti durante il consolidamento in sede (Ex trasmissione a sede)
007803060208     c**********************************************************************
007804060208     c     *entry        plist
007805060208     c                   parm                    kpjba
007806060208      *----------*
007807060208     C                   Z-ADD     1             CODUT
007808060208     C                   CALL      'X§PARUT'
007809060208     C                   PARM                    UT§DSE0F
007810060208     C                   MOVEL     REC80         CNCR80
007811060208      *
007812060208     C     Kwalc         KLIST
007813060208     C                   KFLD                    WalPGM
007814060208     C                   KFLD                    WalNOJ
007815060208     C                   KFLD                    WalNUS
007816060208     C                   KFLD                    WalNRJ
007817060208     c*
007818060208     C* GIRO DATA ODIERNA
007819060208     C                   TIME                    W0140            14 0
007820060208     C                   MOVE      W0140         UDATE8            8 0
007821060208     C     *eur          movel     UDATE8        dataoggi
007822060208     C     dataoggi      subdur    1:*d          data_gg1
007823060208     C                   move      data_gg1      datameno1         8 0
007824060208      **
007825060208     C                   Z-ADD     UDATE8        G02DAT
007826060208     C                   MOVE      *ZEROS        G02INV
007827060208     C                   MOVE      *BLANKS       G02ERR
007828060208     C                   CALL      'XSRDA8'
007829060208     C                   PARM                    WLBDAT
007830060208     C                   clear                   Fiwalc00
007831060208     C                   movel     w0140         walOrc
007832060208     C                   movel     g02inv        waldac
007833060208     C                   eval      WalSIF = KNSIF
007834060208     C                   eval      WalPGM = 'FICN43R   '
007835060208     C                   eval      WalNOJ = KNMEB
007836060208     C                   eval      WalNUS = KNMUS
007837060208     C                   eval      WalNRJ = KNRJO
007838060208     C                   write     Fiwalc00
007839060208     c                   feod      Fiwalc1l
007840060208     c                   commit
007841060208      *----------*
015000030429      *
015100000607     c                   seton                                        lr
031500020205     C**-------------------------------------------------------------**
