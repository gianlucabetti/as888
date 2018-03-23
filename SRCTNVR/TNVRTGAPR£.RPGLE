000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050128     ffignp01l  if   e           k disk    rename(fignp000:fignp) prefix(p)
000400050128     ffigna01l  if   e           k disk    rename(fignp000:figna) prefix(a)
000500050128     ffignpx1l  if   e           k disk    rename(fignp000:fignpx) prefix(s)
000600050128     ftignp01l  uf a e           k disk
000700050128     ftigcp02l  if   e           k disk
000800050131     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
000900050118
001000050118      *ds comodo per tracciati da trascodificare
001100050128     D salvaa        e ds                  extname(figna00f) prefix(a)
001200050128     D salvap        e ds                  extname(fignp00f) prefix(p)
001300050128     D salvas        e ds                  extname(fignp00f) prefix(s)
001400050128     D salvac        e ds                  extname(fignp00f) prefix(c)
001500050128     D salvat        e ds                  extname(tignp00f)
001600050128     D ok              S              1
001700050128     d wgnpagc         s                   like(gnpagc)
001800050128     d wgnpfgc         s                   like(gnpfgc)
001900050128     d wgnpngc         s                   like(gnpngc)
002000050118
002100050131      *--------------------------------------------------------------------
002200050128      * NOTE ARRIVO
002300050128     c     *loval        setll     figna01l
002400000607     c                   do        *hival
002500050128     c                   read      figna01l
002600050128     c                   if        %eof(figna01l)
002700000607     c                   leave
002800000607     c                   end
002900050128     c                   eval      salvac = salvaa
003000050128     c* controllo giacenza
003100050128     c                   exsr      ctrgia
003200050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
003300050128     c                   if        ok = *on
003400050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
003500050131     C                   IF        CGNPAGC >= 2004
003600050131     c                   seton                                        20
003700050131     c                   exsr      stampa
003800050131     c                   end
003900050128     c     ktigcp        setgt     figna01l
004000050128     c                   iter
004100050131     c                   end
004200050128      * prima di scrivere il record verifico che non sia un record già
004300050128      * trascodificato
004400050128     c                   exsr      srtst
004500050128     c                   if        ok = *on
004600050128     c                   iter
004700050128     c                   end
004800050128      *valorizza la ds del file tignp00f per la scrittura del rekord
004900050128     c                   movel     salvaa        salvat
005000050128      * scrivo il rekord nel nuovo archivio
005100050128     c                   write     tignp000
005200040507     c                   enddo
005300050117     c
005400050128      * NOTE PARTENZA
005500050128     c                   clear                   wgnpagc
005600050128     c                   clear                   wgnpfgc
005700050128     c                   clear                   wgnpngc
005800050128     c     *loval        setll     fignp01l
005900050117     c                   do        *hival
006000050128     c                   read      fignp01l
006100050128     c                   if        %eof(fignp01l)
006200050117     c                   leave
006300050117     c                   end
006400050128     c                   eval      salvac = salvap
006500050128     c* controllo giacenza
006600050128     c                   exsr      ctrgia
006700050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
006800050128     c                   if        ok = *on
006900050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
007000050131     C                   IF        CGNPAGC >= 2004
007100050131     c                   seton                                        21
007200050131     c                   exsr      stampa
007300050131     C                   END
007400050128     c     ktigcp        setgt     fignp01l
007500050128     c                   iter
007600050128     c                   end
007700050128      * prima di scrivere il record verifico che non sia un record già
007800050128      * trascodificato
007900050128     c                   exsr      srtst
008000050128     c                   if        ok = *on
008100050128     c                   iter
008200050128     c                   end
008300050128      *valorizza la ds del file tignp00f per la scrittura del rekord
008400050128     c                   movel     salvap        salvat
008500050128      * scrivo il rekord nel nuovo archivio
008600050128     c                   write     tignp000
008700050117     c                   enddo
008800050128      * NOTE SEDE
008900050128     c                   clear                   wgnpagc
009000050128     c                   clear                   wgnpfgc
009100050128     c                   clear                   wgnpngc
009200050128     c     *loval        setll     fignpx1l
009300050128     c                   do        *hival
009400050128     c                   read      fignpx1l
009500050128     c                   if        %eof(fignpx1l)
009600050128     c                   leave
009700050128     c                   end
009800050128     c                   eval      salvac = salvas
009900050128     c* controllo giacenza
010000050128     c                   exsr      ctrgia
010100050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
010200050128     c                   if        ok = *on
010300050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
010400050131     C                   IF        CGNPAGC >= 2004
010500050131     c                   seton                                        22
010600050131     c                   exsr      stampa
010700050131     c                   end
010800050128     c     ktigcp        setgt     fignpx1l
010900050128     c                   iter
011000050128     c                   end
011100050128      * prima di scrivere il record verifico che non sia un record già
011200050128      * trascodificato
011300050128     c                   exsr      srtst
011400050128     c                   if        ok = *on
011500050128     c                   iter
011600050128     c                   end
011700050128      *valorizza la ds del file tignp00f per la scrittura del rekord
011800050128     c                   movel     salvas        salvat
011900050128      * scrivo il rekord nel nuovo archivio
012000050128     c                   write     tignp000
012100050128     c                   enddo
012200050131     c                   seton                                        lr
012300050118
012400050128      *-------------------------------------------------
012500050128     c     ctrgia        begsr
012600050128      *-------------------------------------------------
012700050128     c                   eval      ok = *off
012800050128     c                   if        cgnpagc <> wgnpagc or
012900050128     c                             cgnpfgc <> wgnpfgc or
013000050128     c                             cgnpngc <> wgnpngc
013100050128     c* controllo se ho la giacenza, se non esiste non trascodifico nemmeno
013200050128     c* la nota
013300050128     c     ktigcp        setll     tigcp02l                               01
013400050128     c                   if        not *in01
013500050128     c                   eval      ok = *on
013600050128     c                   end
013700050128     c                   eval      wgnpagc = cgnpagc
013800050128     c                   eval      wgnpfgc = cgnpfgc
013900050128     c                   eval      wgnpngc = cgnpngc
014000050128     c                   end
014100050128     c                   endsr
014200050114      *-------------------------------------------------
014300050128     c     srtst         begsr
014400050114      *-------------------------------------------------
014500050128     c                   eval      ok = *off
014600050128     c     ktignp        setll     tignp01l
014700050128     c                   do        *hival
014800050128     c     ktignp        reade     tignp01l
014900050128     c                   if        %eof(tignp01l)
015000050128     c                   leave
015100050128     c                   end
015200050128     c* confronto i tracciati record
015300050128     c                   if        salvat = %subst(salvac: 1: 66)
015400050128     c                   eval      ok = *on
015500050128     c                   leave
015600050128     c                   end
015700050128     c                   enddo
015800050128     c                   endsr
015900050128      *-------------------------------------------------
016000050131     c     stampa        begsr
016100050128      *-------------------------------------------------
016200050131     c   of              except    testa
016300050131     c                   eval      *inof = '0'
016400050131     c                   if        *in20 or *in21 or *in22
016500050131     c                   except    riga
016600050131     c                   end
016700050131     c                   setoff                                       202122
016800050131     c                   endsr
016900050131      *-------------------------------------------------
017000050131     c     *inzsr        begsr
017100050131      *-------------------------------------------------
017200050131     c                   seton                                        of
017300050131     c
017400050128     c     ktignp        klist
017500050128     c                   kfld                    cgnpAGC
017600050128     c                   kfld                    cgnpFGC
017700050128     c                   kfld                    cgnpNGC
017800050128     c                   kfld                    cgnpFRG
017900050128     c                   kfld                    cgnpFAS
018000050131     c                   kfld                    cgnpNPR
018100050128     c     ktigcp        klist
018200050128     c                   kfld                    cgnpAGC
018300050128     c                   kfld                    cgnpFGC
018400050128     c                   kfld                    cgnpNGC
018500050114     c                   endsr
018600050131     OSYSPRT    E            TESTa            01
018700050131     O                                           65 'ANOMALIE TRASCODIFICA NOTE'
018800050131     O                                           +1 'GIACENZE'
018900050131     O                       PAGE1         Z    132
019000050131     O          E            RIGA       01
019100050131     O                                           29 'Giacenza non trovata nel f-
019200050131     O                                              ile'
019300050131     O               20                          38 'FIGCA00F'
019400050131     O               21                          38 'FIGCP00F'
019500050131     O               22                          43 'FIGCP00F SEDE'
019600050131     O                       cgnpagc             +1
019700050131     O                       cgnpfgc             +1
019800050131     O                       cgnpngc             +1
019900050131     O                                           +1 'Nota non trascodificata'
