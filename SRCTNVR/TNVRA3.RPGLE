000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300050323     FFNARb25L  iF   E           K DISK    rename(fnarb000:fnarb025)
000400050323     FFNARb01L  uF   E           K DISK
000500050331     Ftabel00f  IF   E           K DISK
000600021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700050331     D DS3A          E DS
000800050331     D W0140           S             14  0 INZ
000900050331     D DATEU           S              8  0 INZ
001000050331     D WLBDAT          DS                  INZ
001100050331     D  G02DAT                 1      8  0
001200050331     D  G02INV                 9     16  0
001300050331     D  G02ERR                17     17
001400050331     D  G02TGI                18     22  0
001500021214      *--------------------------------------------------
001600050331     C* GIRO DATA DEL GIORNO
001700050331     C                   TIME                    W0140
001800050331     C                   MOVE      W0140         G02DAT
001900050331     C                   MOVEL     *BLANK        G02ERR
002000050331     C                   CALL      'XSRDA8'
002100050331     C                   PARM                    WLBDAT
002200050331     C                   Z-ADD     G02INV        DATEU
002300050323     c*
002400050331     c                   z-add     06            klna              3 0
002500050331    1c                   do        2
002600050331     c
002700050331     C     karb25        setll     fnarb25l
002800050331     C     karb25        reade     fnarb25l
002900050331    2c                   DOW       not %EOF(FNARb25l)
003000050331     C                   MOVEL     '3A'          COD
003100050331     C                   MOVEL(P)  ARBCBO        KEY
003200050331     C     KTAB          CHAIN     TABEL                              32
003300050331     C  N32              MOVEL     TBLUNI        DS3A
003400050331     C   32              CLEAR                   DS3A
003500050331     c
003600050322     c
003700050331    3c                   if        §3atb1='A3'
003800050331     c                   move      arbksc        wfineksc          4 0
003900050331    4c                   if        wfineksc<>9999 and wfineksc<>0
004000050323     c
004100050323     c     karb          chain     fnarb01l
004200050323     c
004300050331    5c                   if        %found(fnarb01l)
004400050331     c* Chiudo l'assegnato
004500050331     C                   MOVEL     'Y'           ARBICC
004600050331     C                   MOVEL     'S'           ARBICA
004700050331     C                   Z-ADD     DATEU         ARBDCM
004800050331     C                   time                    wora              6 0
004900050331     C                   movel     wora          ARBHMC
005000050331     C                   CLEAR                   ARBFT1
005100050331     C                   CLEAR                   ARBDT1
005200050331     C                   MOVEL     *ALL'9'       ARBNDC
005300050331     C                   Z-ADD     DATEU         ARBDDC
005400050331     C                   MOVEL     'S'           ARBFDC
005500050331     C                   MOVEL     *ALL'9'       ARBNMI
005600050331     C                   Z-ADD     DATEU         ARBDMI
005700050331     C                   MOVEL     *ALL'9'       ARBCUC
005800050331     C                   CLEAR                   ARBFBC
005900050331     c
006000050331     c                   except    bolladaagg
006100050331     c                   ADD       1             CONT              7 0
006200050331     c
006300050331     c                   update    fnarb000
006400050331    5c                   endif
006500050331    4c                   endif
006600050331    3c                   endif
006700050322     c
006800050331     C     karb25        reade     fnarb25l
006900050331    2c                   enddo
007000050331     c
007100050331     c                   z-add     28            klna
007200050331    1c                   enddo
007300050331     c                   except    TOTALE
007400050322     c
007500021214     C                   eval       *inlr = *on
007600021214      *---------------------------------------------------------
007700021214     c     *inzsr        BEGSR
007800021214      *
007900050331     c     Ktab          KLIST
008000050331     C                   KFLD                    codut             1 0
008100050331     C                   KFLD                    cod               2
008200050331     C                   KFLD                    key               8
008300050331     C     Karb25        KLIST
008400050331     C                   KFLD                    klna
008500050331     C                   KFLD                    kdcm              8 0
008600050331     C                   KFLD                    kaas              4 0
008700050331     c                   z-add     1             codut
008800050331     c                   z-add     0             kdcm
008900050331     c                   z-add     2005          kaas
009000050118     C     Karb          KLIST
009100050322     C                   KFLD                    arbaas
009200050322     C                   KFLD                    arblnp
009300050322     C                   KFLD                    arbnrs
009400050322     C                   KFLD                    arbnsp
009500021214      *
009600050331     c                   except    testa
009700021216     C*
009800021214     C                   ENDSR
009900021214      *---------------------------------------------------------
010000021214     OQSYSPRT   E            TESTA          1 02
010100021214     O                       *DATE            +   5 '  /  /    '
010200050331     O                                        +   5 'Bolle A3 da chiudere  '
010300050331     O                                        +   7 'TNVRA3'
010400050322     O          E            bolladaagg  1
010500050322     O                                              'SPED.'
010600050322     O                       ARbAAS            +  2
010700050322     O                       ARbLNP            +  1
010800050322     O                       ARbNRS            +  1
010900050322     O                       ARbNSP        Z   +  1
011000050322     O                       ARblna            +  1
011100050331     O                       ARbcbo            +  1
011200050118     O                       ARbmgs            +  1 '  /  '
011300050331     O                                         +  2 'ARBksc'
011400050331     O                       ARbksc            +  1
011500050331     O                       ARbctr            +  2
011600050331     O          E            TOTALE      1
011700050331     O                                              'TOTALE SPED.'
011800050331     O                       CONT          2   +  2
