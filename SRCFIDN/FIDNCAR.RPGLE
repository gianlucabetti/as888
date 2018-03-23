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
003900160210      * Note ---------------------------------------------------------*
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
007300160210      *
007400160210      * Modifiche ----------------------------------------------------*
007500160210      *
007600160210     c                   if        ic0pdt = 999
007700160210     c     Ktgdp         setll     FITgm01l
007800160210      *
007900160210     c                   If        %equal
008000160210      *
008100160210     c                   do        *hival
008200160210     c     Ktgdp         reade     FITgm01l
008300160210      *
008400160210     c                   if        %eof(FITgm01l)
008500160210     c                   leave
008600160210     c                   endif
008700160210      *
008800160210     c                   delete    FITgm01l
008900160210      *
009000160210     c                   enddo
009100160210     c                   endif
009200160210      * singola riga
009300160210     c                   else
009400160210      *
009500160210     c     ktgd          setll     FITgm01l
009600160210     c                   do        *hival
009700160210     c     ktgd          reade     FITgm01l
009800160210
009900160210     c                   if        %eof(FITgm01l)
010000160210     c                   leave
010100160210     c                   endif
010200160210      *
010300160210     c                   delete    FITgm01l
010400160210      *
010500160210     c                   enddo
010600160210      *
010700160210     c                   endif
010800040219      *
010900160210      *---------------------------------------------------------------*
011000160210      *
011100040219      * verifico se esistono delle altre righe per quel reclamo altrimenti cancello tutto
011200040219      *
011300040219     c                   if        ic0pdt <> 999
011400040219      *
011500150623     c     Ktgdp         chain     FITgd01l
011600040528      *
011700040614      * se non ci sono + righe è come se camncellassi tutto
011800150623     c                   if        not %found(FITgd01l)
011900040219     c                   eval      ic0pdt = 999
012000040219     c                   endif
012100040219      *
012200040614     c                   endif
012300160210      *
012400160210      * Riferimenti --------------------------------------------------*
012500040219      *
012600040219     c                   if        ic0pdt =  999
012700040219
012800040421     c     Ktgdp         setll     FITgr01l
012900040219     c                   do        *hival
013000040421     c     Ktgdp         reade     FITgr01l
013100040219      *
013200040219     c                   if        %eof(FITgr01l)
013300040219     c                   leave
013400040219     c                   endif
013500040219      *
013600040219     c                   delete    FITgr01l
013700040219      *
013800040219     c                   enddo
013900160210      *
014000160210      * singola riga
014100160210     c                   else
014200160210      *
014300160210     c     ktgd          setll     FITgr01l
014400160210     c                   do        *hival
014500160210     c     ktgd          reade     FITgr01l
014600160210      *
014700160210     c                   if        %eof(FITgr01l)
014800160210     c                   leave
014900160210     c                   endif
015000160210      *
015100160210     c                   delete    FITgr01l
015200160210      *
015300160210     c                   enddo
015400040219      *
015500040528     c                   endif
015600040219
015700150623     c                   endif
015800160210
015900160210      *
016000160210      * Fine ---------------------------------------------------------*
016100160210      *
016200150623
016300150623     c                   if        iC0tla <> *blanks
016400150623     c                   eval      *inlr = *on
016500150623     c                   else
016600150623     c                   eval      *inrt = *on
016700150623     c                   endif
016800150623
016900980722      *****************************************************************
017000980623      *  ROUTINE INIZIALE
017100980722      *****************************************************************
017200951009     C     *INZSR        BEGSR
017300980722
017400040219      *
017500040219     c     *ENTRY        PLIST
017600150623     c                   PARM                    fidncads
017700980625      *
017800040219     c     ktgd          klist
017900040421     c                   kfld                    ic0ant
018000040219     c                   kfld                    ic0nut
018100040219     c                   kfld                    ic0pdt
018200040421      *
018300040421     c     ktgdp         klist
018400040421     c                   kfld                    ic0ant
018500040421     c                   kfld                    ic0nut
018600981224      *
018700951009     C                   ENDSR
