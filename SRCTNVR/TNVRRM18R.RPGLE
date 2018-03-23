000100940224     H*--------------------------------------------------------------*
000200071009     Ffnblp45l  iF   E           K DISK
000300071009     Ffnlbl01l  iF   E           K DISK
000400071009     Ffiarg01l  uf   E           K DISK
000500071009     FSYSPRT    O    F  132        PRINTER
000600060928
000700071009     D tisit0ds      E DS
000800071009     D tisit0x1ds    E DS
000900900131     C*---------------------------------------------------------------*
001000071009     c     *entry        plist
001100071009     c                   parm                    aggio             1
001200071009     c*
001300071009     C     Karg          KLIST
001400071009     C                   KFLD                    blPAAS
001500071009     C                   KFLD                    bLPLNP
001600071009     C                   KFLD                    bLPNRS
001700071009     C                   KFLD                    bLPNSP
001800071009     c*
001900071009     c                   z-add     0             progr             4 0
002000071009     c*
002100071009     c     *loval        setll     fnblp45l
002200071009    1c                   do        *hival
002300071009     c                   read      fnblp45l
002400071009    2c                   if        %eof(fnblp45l)
002500071009     c                   leave
002600071009    2c                   endif
002700071009     c* scarto se figlia
002800071009     c     karg          setll     fnlbl01l
002900071009     c                   if        %equal(fnlbl01l)
003000071009     c                   iter
003100071009     c                   endif
003200071009     c*
003300071009     c                   clear                   tisit0ds
003400071009     c                   clear                   tisit0x1ds
003500071009     c                   movel     'E'           it0tla
003600071009     c                   movel     blpnzd        it0naz
003700071009     c                   movel     blpprd        it0prv
003800071009     c                   movel     blpcad        it0cap
003900071009     c                   movel     blplod        it0loc
004000071009     c                   movel     blpind        it0ind
004100071009     c                   movel     blprsd        it0rag
004200071009     c                   call      'TISIT5R'
004300071009     c                   parm                    tisit0ds
004400071009     c                   parm                    tisit0x1ds
004500071009    2c                   if        ot0err<>*blanks
004600071009     c     karg          chain(n)  fiarg01l
004700071009   2ac                   if        %found(fiarg01l)
004800071009     c                   if        argtgi<>'M'
004900071009     c                   iter
005000071009     c                   endif
005100071009     c*
005200071009    3c                   if        aggio='A'
005300071009     c     karg          delete(E) fiarg01l
005400071009    4c                   if        not %error
005500071009     c                   add       1             progr
005600071009     c                   except    record
005700071009    4c                   endif
005800071009    3c                   endif
005900071009     c*
006000071009    3c                   if        aggio<>'A'
006100071009     c                   add       1             progr
006200071009     c                   except    record
006300071009    3c                   endif
006400071009     c*
006500071009   2ac                   endif
006600071009    2c                   endif
006700071009    1c                   enddo
006800071009     c*
006900071009     c                   clear                   tisit0ds
007000071009     c                   movel     'C'           it0tla
007100071009     C                   CALL      'TISIT5R'
007200071009     C                   PARM                    tisit0ds
007300071009     C                   PARM                    tisit0x1ds
007400071009      *
007500071009     C                   SETON                                        LR
007600071009     Osysprt    e            record
007700071009     o                       progr         z  +   1
007800071009     O                       blpaas           +   2
007900071009     O                       blplnp           +   2
008000071009     O                       blpnrs        z  +   2
008100071009     O                       blpnsp        z  +   2
008200071009     o                       blprsd           +   2
008300071009     o                       ot0flo           +   2
008400071009     o                       blpdim           +   2
008500071009     o                       blpsop           +   2
