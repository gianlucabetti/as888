000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300040219     fFITGD01l  UF   E           K DISK
000400040219     fFITGR01L  UF   E           K DISK
000500040219     fFITGN01L  UF   E           K DISK
000600150318     fFITGM01L  UF   E           K DISK
000700990820      * ---------------------------------------------------------
000800040219     D fidnC0ds      E DS
000900980625      * ---------------------------------------------------------
001000040219
001100040219      * se riga richiesta di cancellazione è uguale a 999 significa che devo cancellare tutto
001200040219      * il reclamo
001300040219     c                   if        ic0pdt = 999
001400040528     c     Ktgdp         setll     FITgd01l
001500040528      *
001600040528     c                   If        %equal
001700040528      *
001800040219     c                   do        *hival
001900040421     c     Ktgdp         reade     FITgd01l
002000040219      *
002100040219     c                   if        %eof(FITgd01l)
002200040219     c                   leave
002300040219     c                   endif
002400040219      *
002500040219     c                   delete    FITgd01l
002600040219      *
002700040219     c                   enddo
002800040614     c                   endif
002900040219      * singola riga
003000040219     c                   else
003100040219
003200040219     c     ktgd          delete    FITgd01l
003300040219
003400040528     c                   endif
003500040219      *
003600040219      * note
003700040219      *
003800040219     c                   if        ic0pdt = 999
003900040421     c     Ktgdp         setll     FITgn01l
004000040528      *
004100040528     c                   If        %equal
004200040528      *
004300040219     c                   do        *hival
004400040421     c     Ktgdp         reade     FITgn01l
004500040219      *
004600040219     c                   if        %eof(FITgn01l)
004700040219     c                   leave
004800040219     c                   endif
004900040219      *
005000040219     c                   delete    FITgn01l
005100040219      *
005200040219     c                   enddo
005300040614     c                   endif
005400040219      * singola riga
005500040219     c                   else
005600040219      *
005700040219     c     ktgd          setll     FITgn01l
005800040219     c                   do        *hival
005900040219     c     ktgd          reade     FITgn01l
006000040219
006100040219     c                   if        %eof(FITgn01l)
006200040219     c                   leave
006300040219     c                   endif
006400040219      *
006500040219     c                   delete    FITgn01l
006600040219      *
006700040219     c                   enddo
006800040219      *
006900040528     c                   endif
007000040219      *
007100040219      * riferimeti
007200040219      *
007300040219      * verifico se esistono delle altre righe per quel reclamo altrimenti cancello tutto
007400040219      *
007500040219     c                   if        ic0pdt <> 999
007600040219      *
007700040421     c     Ktgdp         setll     FITgd01l
007800040528      *
007900040614      * se non ci sono + righe è come se camncellassi tutto
008000040219     c                   if        not %equal(FITgd01l)
008100040219     c                   eval      ic0pdt = 999
008200040219     c                   endif
008300040219      *
008400040614     c                   endif
008500040219      *
008600040219     c                   if        ic0pdt =  999
008700040219
008800040421     c     Ktgdp         setll     FITgr01l
008900040219     c                   do        *hival
009000040421     c     Ktgdp         reade     FITgr01l
009100040219      *
009200040219     c                   if        %eof(FITgr01l)
009300040219     c                   leave
009400040219     c                   endif
009500040219      *
009600040219     c                   delete    FITgr01l
009700040219      *
009800040219     c                   enddo
009900040219      *
010000040528     c                   endif
010100150318      *
010200150318      * Modifiche
010300150318      *
010400150318     c                   if        ic0pdt = 999
010500150318     c     Ktgdp         setll     FITgm01l
010600150318      *
010700150318     c                   If        %equal
010800150318      *
010900150318     c                   do        *hival
011000150318     c     Ktgdp         reade     FITgm01l
011100150318      *
011200150318     c                   if        %eof(FITgm01l)
011300150318     c                   leave
011400150318     c                   endif
011500150318      *
011600150318     c                   delete    FITgm01l
011700150318      *
011800150318     c                   enddo
011900150318     c                   endif
012000150318      * singola riga
012100150318     c                   else
012200150318      *
012300150318     c     ktgd          setll     FITgm01l
012400150318     c                   do        *hival
012500150318     c     ktgd          reade     FITgm01l
012600150318
012700150318     c                   if        %eof(FITgm01l)
012800150318     c                   leave
012900150318     c                   endif
013000150318      *
013100150318     c                   delete    FITgm01l
013200150318      *
013300150318     c                   enddo
013400150318      *
013500150318     c                   endif
013600040219
013700040219     c                   seton                                        lr
013800980722      *****************************************************************
013900980623      *  ROUTINE INIZIALE
014000980722      *****************************************************************
014100951009     C     *INZSR        BEGSR
014200980722
014300040219      *
014400040219     c     *ENTRY        PLIST
014500040219     c                   PARM                    fidnc0ds
014600980625      *
014700040219     c     ktgd          klist
014800040421     c                   kfld                    ic0ant
014900040219     c                   kfld                    ic0nut
015000040219     c                   kfld                    ic0pdt
015100040421      *
015200040421     c     ktgdp         klist
015300040421     c                   kfld                    ic0ant
015400040421     c                   kfld                    ic0nut
015500981224      *
015600951009     C                   ENDSR
015700980611      *------------------------------------------------------------
