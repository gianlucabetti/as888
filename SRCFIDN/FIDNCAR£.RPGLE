000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300040219     fFITGD01l  UF   E           K DISK
000400040219     fFITGR01L  UF   E           K DISK
000500040219     fFITGN01L  UF   E           K DISK
000600150318     fFITGM01L  UF   E           K DISK
000700990820      * ---------------------------------------------------------
000800150623     D fidnCAds      E DS
000900980625      * ---------------------------------------------------------
001000040219
001100150623
001200150623      * Se Tipo lancio 'C' --> solo chiusura file
001300150623     c                   if        iC0tla <> 'C'
001400040219      * se riga richiesta di cancellazione è uguale a 999 significa che devo cancellare tutto
001500040219      * il reclamo
001600040219     c                   if        ic0pdt = 999
001700040528     c     Ktgdp         setll     FITgd01l
001800040528      *
001900040528     c                   If        %equal
002000040528      *
002100040219     c                   do        *hival
002200040421     c     Ktgdp         reade     FITgd01l
002300040219      *
002400040219     c                   if        %eof(FITgd01l)
002500040219     c                   leave
002600040219     c                   endif
002700040219      *
002800040219     c                   delete    FITgd01l
002900040219      *
003000040219     c                   enddo
003100040614     c                   endif
003200040219      * singola riga
003300040219     c                   else
003400040219
003500040219     c     ktgd          delete    FITgd01l
003600040219
003700040528     c                   endif
003800040219      *
003900040219      * note
004000040219      *
004100040219     c                   if        ic0pdt = 999
004200040421     c     Ktgdp         setll     FITgn01l
004300040528      *
004400040528     c                   If        %equal
004500040528      *
004600040219     c                   do        *hival
004700040421     c     Ktgdp         reade     FITgn01l
004800040219      *
004900040219     c                   if        %eof(FITgn01l)
005000040219     c                   leave
005100040219     c                   endif
005200040219      *
005300040219     c                   delete    FITgn01l
005400040219      *
005500040219     c                   enddo
005600040614     c                   endif
005700040219      * singola riga
005800040219     c                   else
005900040219      *
006000040219     c     ktgd          setll     FITgn01l
006100040219     c                   do        *hival
006200040219     c     ktgd          reade     FITgn01l
006300040219
006400040219     c                   if        %eof(FITgn01l)
006500040219     c                   leave
006600040219     c                   endif
006700040219      *
006800040219     c                   delete    FITgn01l
006900040219      *
007000040219     c                   enddo
007100040219      *
007200040528     c                   endif
007300040219      *
007400040219      * riferimeti
007500040219      *
007600040219      * verifico se esistono delle altre righe per quel reclamo altrimenti cancello tutto
007700040219      *
007800040219     c                   if        ic0pdt <> 999
007900040219      *
008000150623     c     Ktgdp         chain     FITgd01l
008100040528      *
008200040614      * se non ci sono + righe è come se camncellassi tutto
008300150623     c                   if        not %found(FITgd01l)
008400040219     c                   eval      ic0pdt = 999
008500040219     c                   endif
008600040219      *
008700040614     c                   endif
008800040219      *
008900040219     c                   if        ic0pdt =  999
009000040219
009100040421     c     Ktgdp         setll     FITgr01l
009200040219     c                   do        *hival
009300040421     c     Ktgdp         reade     FITgr01l
009400040219      *
009500040219     c                   if        %eof(FITgr01l)
009600040219     c                   leave
009700040219     c                   endif
009800040219      *
009900040219     c                   delete    FITgr01l
010000040219      *
010100040219     c                   enddo
010200040219      *
010300040528     c                   endif
010400150318      *
010500150318      * Modifiche
010600150318      *
010700150318     c                   if        ic0pdt = 999
010800150318     c     Ktgdp         setll     FITgm01l
010900150318      *
011000150318     c                   If        %equal
011100150318      *
011200150318     c                   do        *hival
011300150318     c     Ktgdp         reade     FITgm01l
011400150318      *
011500150318     c                   if        %eof(FITgm01l)
011600150318     c                   leave
011700150318     c                   endif
011800150318      *
011900150318     c                   delete    FITgm01l
012000150318      *
012100150318     c                   enddo
012200150318     c                   endif
012300150318      * singola riga
012400150318     c                   else
012500150318      *
012600150318     c     ktgd          setll     FITgm01l
012700150318     c                   do        *hival
012800150318     c     ktgd          reade     FITgm01l
012900150318
013000150318     c                   if        %eof(FITgm01l)
013100150318     c                   leave
013200150318     c                   endif
013300150318      *
013400150318     c                   delete    FITgm01l
013500150318      *
013600150318     c                   enddo
013700150318      *
013800150318     c                   endif
013900040219
014000150623     c                   endif
014100150623
014200150623     c                   if        iC0tla <> *blanks
014300150623     c                   eval      *inlr = *on
014400150623     c                   else
014500150623     c                   eval      *inrt = *on
014600150623     c                   endif
014700150623
014800980722      *****************************************************************
014900980623      *  ROUTINE INIZIALE
015000980722      *****************************************************************
015100951009     C     *INZSR        BEGSR
015200980722
015300040219      *
015400040219     c     *ENTRY        PLIST
015500150623     c                   PARM                    fidncads
015600980625      *
015700040219     c     ktgd          klist
015800040421     c                   kfld                    ic0ant
015900040219     c                   kfld                    ic0nut
016000040219     c                   kfld                    ic0pdt
016100040421      *
016200040421     c     ktgdp         klist
016300040421     c                   kfld                    ic0ant
016400040421     c                   kfld                    ic0nut
016500981224      *
016600951009     C                   ENDSR
016700980611      *------------------------------------------------------------
