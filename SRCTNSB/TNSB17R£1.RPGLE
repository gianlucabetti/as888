000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300040511      *------------------------------------------------------------------------*
000400050318      * Creazione bolle per addebito invio immagini a CLI
000500040511      *------------------------------------------------------------------------*
000600050318     ftilac02l  uf   e           k Disk
000700050318     ffirig00f  o    e             Disk
000800040511      *------------------------------------------------------------------------*
000900040511      *   V A R I A B I L I
001000040511      *------------------------------------------------------------------------*
001100050330     D kela            S                   like(lacela)                         Anno
001200050318     D kcop            S                   like(laccop) inz('CLI')              Anno
001300050318     d wora            s              6  0
001400050318     d woggi           s              8  0
001500050330     d s_lacela        s              3
001600050318      *------------------------------------------------------------------------*
001700050318      *   D S   I N T E R N E / E S T E R N E
001800050318      *------------------------------------------------------------------------*
001900050331     d kpjba         E DS
002000040511      *------------------------------------------------------------------------*
002100040511      *  RIEPILOGO INDICATORI
002200040511      *------------------------------------------------------------------------*
002300040511
002400040511      *------------------------------------------------------------------------*
002500040511
002600050330     c                   do        9             xx                2 0
002700050330     c                   move      xx            kela
002800050318     c     klac          Setll     tilac02l
002900040511     c                   Do        *Hival
003000050318     c     klac          Reade     tilac02l
003100040511
003200040511      * fine file
003300050318     c                   If        %Eof(tilac02l)
003400040511     c                   Leave
003500040511     c                   EndIf
003600050318     c* scrivo firig per addebito
003700090309     c                   select
003800090309     c                   when      lactad = *blanks
003900060419     c                   if        lacxx1 = *blanks or lacxx1 = 'V'
004000050318     c                   exsr      wrtrig
004100050318     c                   endif
004200090309     c                   when      lactad <> 'N'
004300090309     c                   exsr      wrtrig
004400090309     c                   endsl
004500050330     c* aggiorno stato da 01/02../09/ a 11/12../19
004600050330     c                   eval      S_lacela = %editw(xx+10:'0  ')
004700050330     c                   move      s_lacela      lacela
004800050318     c                   update    tilac000
004900040511
005000040511     c                   EndDo
005100050330     c*
005200050330     c                   enddo
005300040511
005400040511     c                   Eval      *InLr = *On
005500040511
005600040511      *------------------------------------------------------------------------*
005700040511      * Routine iniziale
005800040511      *------------------------------------------------------------------------*
005900040511     c     *Inzsr        BegSr
006000040511
006100050331     c     *entry        plist
006200050331     c                   parm                    kpjba
006300040511      * data/ora del job
006400040511     c                   Z-add     *date         woggi
006500040511     c                   Time                    wora
006600040511
006700050318     c     klac          klist
006800050318     c                   kfld                    kela
006900050318     c                   kfld                    kcop
007000040511     c                   EndSr
007100050318      *------------------------------------------------------------------------*
007200050318      * Scrittura firig per addebito spese invio immagini
007300050318      *------------------------------------------------------------------------*
007400050318     c     wrtrig        BegSr
007500050318     c                   clear                   firig000
007600050318     c                   z-add     2000          rigaas
007700050318     c                   move      lacaas        rigaas
007800050318     c                   movel     laclnp        riglnp
007900050318     c                   movel     laclnp        rigpor
008000050318     c                   movel     lacnrs        rignrs
008100050318     c                   movel     lacnsp        rignsp
008200050318     c                   movel     'I'           rigcar
008300050318     c                   movel     'E'           rigtra
008400090309     c                   select
008500090309     c                   when      lactad = *blanks
008600060419     c                   if        lacxx1 = *blanks
008700050318     c                   movel     'TILAC'       rigpru
008800060419     c                   endif
008900060419     c                   if        lacxx1 = 'V'
009000060419     c                   movel     'TILAC1'      rigpru
009100060419     c                   endif
009200090309     c                   when      lactad = 'V'
009300090309     c                   eval      rigpru = 'TILAC1'
009400090309     c                   when      lactad = 'S'
009500090309     c                   eval      rigpru = 'TILAC'
009600090309     c                   endsl
009700050318     c                   z-add     woggi         rigdim
009800050318     c                   movel     wora          righim
009900050318     c                   write     firig000
010000050318     c                   endsr
