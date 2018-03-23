000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300040219     fFITGD01l  UF   E           K DISK
000400040219     fFITGR01L  UF   E           K DISK
000500040219     fFITGN01L  UF   E           K DISK
000600990820      * ---------------------------------------------------------
000700040219     D fidnC0ds      E DS
000800980625      * ---------------------------------------------------------
000900040219
001000040219      * se riga richiesta di cancellazione è uguale a 999 significa che devo cancellare tutto
001100040219      * il reclamo
001200040219     c                   if        ic0pdt = 999
001300040528     c     Ktgdp         setll     FITgd01l
001400040528      *
001500040528     c                   If        %equal
001600040528      *
001700040219     c                   do        *hival
001800040421     c     Ktgdp         reade     FITgd01l
001900040219      *
002000040219     c                   if        %eof(FITgd01l)
002100040219     c                   leave
002200040219     c                   endif
002300040219      *
002400040219     c                   delete    FITgd01l
002500040219      *
002600040219     c                   enddo
002700040614     c                   endif
002800040219      * singola riga
002900040219     c                   else
003000040219
003100040219     c     ktgd          delete    FITgd01l
003200040219
003300040528     c                   endif
003400040219      *
003500040219      * note
003600040219      *
003700040219     c                   if        ic0pdt = 999
003800040421     c     Ktgdp         setll     FITgn01l
003900040528      *
004000040528     c                   If        %equal
004100040528      *
004200040219     c                   do        *hival
004300040421     c     Ktgdp         reade     FITgn01l
004400040219      *
004500040219     c                   if        %eof(FITgn01l)
004600040219     c                   leave
004700040219     c                   endif
004800040219      *
004900040219     c                   delete    FITgn01l
005000040219      *
005100040219     c                   enddo
005200040614     c                   endif
005300040219      * singola riga
005400040219     c                   else
005500040219      *
005600040219     c     ktgd          setll     FITgn01l
005700040219     c                   do        *hival
005800040219     c     ktgd          reade     FITgn01l
005900040219
006000040219     c                   if        %eof(FITgn01l)
006100040219     c                   leave
006200040219     c                   endif
006300040219      *
006400040219     c                   delete    FITgn01l
006500040219      *
006600040219     c                   enddo
006700040219      *
006800040528     c                   endif
006900040219      *
007000040219      * riferimeti
007100040219      *
007200040219      * verifico se esistono delle altre righe per quel reclamo altrimenti cancello tutto
007300040219      *
007400040219     c                   if        ic0pdt <> 999
007500040219      *
007600040421     c     Ktgdp         setll     FITgd01l
007700040528      *
007800040614      * se non ci sono + righe è come se camncellassi tutto
007900040219     c                   if        not %equal(FITgd01l)
008000040219     c                   eval      ic0pdt = 999
008100040219     c                   endif
008200040219      *
008300040614     c                   endif
008400040219      *
008500040219     c                   if        ic0pdt =  999
008600040219
008700040421     c     Ktgdp         setll     FITgr01l
008800040219     c                   do        *hival
008900040421     c     Ktgdp         reade     FITgr01l
009000040219      *
009100040219     c                   if        %eof(FITgr01l)
009200040219     c                   leave
009300040219     c                   endif
009400040219      *
009500040219     c                   delete    FITgr01l
009600040219      *
009700040219     c                   enddo
009800040219      *
009900040528     c                   endif
010000040219
010100040219     c                   seton                                        lr
010200980722      *****************************************************************
010300980623      *  ROUTINE INIZIALE
010400980722      *****************************************************************
010500951009     C     *INZSR        BEGSR
010600980722
010700040219      *
010800040219     c     *ENTRY        PLIST
010900040219     c                   PARM                    fidnc0ds
011000980625      *
011100040219     c     ktgd          klist
011200040421     c                   kfld                    ic0ant
011300040219     c                   kfld                    ic0nut
011400040219     c                   kfld                    ic0pdt
011500040421      *
011600040421     c     ktgdp         klist
011700040421     c                   kfld                    ic0ant
011800040421     c                   kfld                    ic0nut
011900981224      *
012000951009     C                   ENDSR
012100980611      *------------------------------------------------------------
