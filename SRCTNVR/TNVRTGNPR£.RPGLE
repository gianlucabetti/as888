000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050128     ffignp01l  if   e           k disk    rename(fignp000:fignp) prefix(p)
000400050128     ffigna01l  if   e           k disk    rename(fignp000:figna) prefix(a)
000500050128     ffignpx1l  if   e           k disk    rename(fignp000:fignpx) prefix(s)
000600050128     ftignp01l  uf a e           k disk
000700050118
000800050118      *ds comodo per tracciati da trascodificare
000900050128     D Ptra            S               *
001000050128     D                                     INZ(%ADDR(agnpagc))
001100050128     D salvaa          S             73
001200050128     D Ptrp            S               *
001300050128     D                                     INZ(%ADDR(pgnpagc))
001400050128     D salvap          S             73
001500050128     D Ptrs            S               *
001600050128     D                                     INZ(%ADDR(sgnpagc))
001700050128     D salvas          S             73
001800050128     D Ptrt            S               *
001900050128     D                                     INZ(%ADDR(gnpagc))
002000050128     D salvat        e ds                  extname(tignp01l)
002100050128     D salvac        e ds                  extname(tignp01l) prefix(c)
002200050128     D ok              S              1
002300050128     d wgnpagc         s                   like(gnpagc)
002400050128     d wgnpfgc         s                   like(gnpfgc)
002500050128     d wgnpngc         s                   like(gnpngc)
002600050118
002700000607      *--------------------------------------------------------------------
002800050128      * NOTE ARRIVO
002900050128     c     *loval        setll     figna01l
003000000607     c                   do        *hival
003100050128     c                   read      figna01l
003200050117     c                   if        %eof(figca01l)
003300000607     c                   leave
003400000607     c                   end
003500050128     c                   eval      salvac = salvaa
003600050128     c* controllo giacenza
003700050128     c                   exsr      ctrgia
003800050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
003900050128     c                   if        ok = *on
004000050128     c     ktignp        setgt     figna01l
004100050128     c                   iter
004200050128     c                   end
004300050128      * prima di scrivere il record verifico che non sia un record già
004400050128      * trascodificato
004500050128     c                   exsr      srtst
004600050128     c                   if        ok = *on
004700050128     c                   iter
004800050128     c                   end
004900050128      *valorizza la ds del file tignp00f per la scrittura del rekord
005000050128     c                   eval      salvat = salvaa
005100050128      * scrivo il rekord nel nuovo archivio
005200050128     c                   write     tignp000
005300040507     c                   enddo
005400050117     c
005500050128      * NOTE PARTENZA
005600050128     c                   clear                   wgnpagc
005700050128     c                   clear                   wgnpfgc
005800050128     c                   clear                   wgnpngc
005900050128     c     *loval        setll     fignp01l
006000050117     c                   do        *hival
006100050128     c                   read      fignp01l
006200050128     c                   if        %eof(fignp01l)
006300050117     c                   leave
006400050117     c                   end
006500050128     c                   eval      salvac = salvap
006600050128     c* controllo giacenza
006700050128     c                   exsr      ctrgia
006800050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
006900050128     c                   if        ok = *on
007000050128     c     ktignp        setgt     fignp01l
007100050128     c                   iter
007200050128     c                   end
007300050128      * prima di scrivere il record verifico che non sia un record già
007400050128      * trascodificato
007500050128     c                   exsr      srtst
007600050128     c                   if        ok = *on
007700050128     c                   iter
007800050128     c                   end
007900050128      *valorizza la ds del file tignp00f per la scrittura del rekord
008000050128     c                   eval      salvat = salvap
008100050128      * scrivo il rekord nel nuovo archivio
008200050128     c                   write     tignp000
008300050117     c                   enddo
008400050128      * NOTE SEDE
008500050128     c                   clear                   wgnpagc
008600050128     c                   clear                   wgnpfgc
008700050128     c                   clear                   wgnpngc
008800050128     c     *loval        setll     fignpx1l
008900050128     c                   do        *hival
009000050128     c                   read      fignpx1l
009100050128     c                   if        %eof(fignpx1l)
009200050128     c                   leave
009300050128     c                   end
009400050128     c                   eval      salvac = salvas
009500050128     c* controllo giacenza
009600050128     c                   exsr      ctrgia
009700050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
009800050128     c                   if        ok = *on
009900050128     c     ktignp        setgt     fignpx1l
010000050128     c                   iter
010100050128     c                   end
010200050128      * prima di scrivere il record verifico che non sia un record già
010300050128      * trascodificato
010400050128     c                   exsr      srtst
010500050128     c                   if        ok = *on
010600050128     c                   iter
010700050128     c                   end
010800050128      *valorizza la ds del file tignp00f per la scrittura del rekord
010900050128     c                   eval      salvat = salvas
011000050128      * scrivo il rekord nel nuovo archivio
011100050128     c                   write     tignp000
011200050128     c                   enddo
011300050118
011400000607     c                   seton                                        lr
011500050128      *-------------------------------------------------
011600050128     c     ctrgia        begsr
011700050128      *-------------------------------------------------
011800050128     c                   eval      ok = *off
011900050128     c                   if        cgnpagc <> wgnpagc or
012000050128     c                             cgnpfgc <> wgnpfgc or
012100050128     c                             cgnpngc <> wgnpngc
012200050128     c* controllo se ho la giacenza, se non esiste non trascodifico nemmeno
012300050128     c* la nota
012400050128     c     ktigcp        setll     tigcp01l                               01
012500050128     c                   if        not *in01
012600050128     c                   eval      ok = *on
012700050128     c                   end
012800050128     c                   eval      wgnpagc = cgnpagc
012900050128     c                   eval      wgnpfgc = cgnpfgc
013000050128     c                   eval      wgnpngc = cgnpngc
013100050128     c                   end
013200050128     c                   endsr
013300050114      *-------------------------------------------------
013400050128     c     srtst         begsr
013500050114      *-------------------------------------------------
013600050128     c                   eval      ok = *off
013700050128     c     ktignp        setll     tignp01l
013800050128     c                   do        *hival
013900050128     c     ktignp        reade     tignp01l
014000050128     c                   if        %eof(tignp01l)
014100050128     c                   leave
014200050128     c                   end
014300050128     c* confronto i tracciati record
014400050128     c                   if        salvat = salvac
014500050128     c                   eval      ok = *on
014600050128     c                   leave
014700050128     c                   end
014800050128     c                   enddo
014900050128     c                   endsr
015000050128      *-------------------------------------------------
015100050128     c     *inzsr        begsr
015200050128      *-------------------------------------------------
015300050117
015400050128     c     ktignp        klist
015500050128     c                   kfld                    cgnpAGC
015600050128     c                   kfld                    cgnpFGC
015700050128     c                   kfld                    cgnpNGC
015800050128     c                   kfld                    cgnpFRG
015900050128     c                   kfld                    cgnpNPR
016000050128     c                   kfld                    cgnpFAS
016100050128     c     ktigcp        klist
016200050121     c                   kfld                    cgcpAGC
016300050121     c                   kfld                    cgcpFGC
016400050121     c                   kfld                    cgcpNGC
016500050114     c                   endsr
