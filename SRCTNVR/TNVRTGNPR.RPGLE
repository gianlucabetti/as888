000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050128     ffignp01l  if   e           k disk    rename(fignp000:fignp) prefix(p)
000400050128     ffigna01l  if   e           k disk    rename(fignp000:figna) prefix(a)
000500050128     ffignpx1l  if   e           k disk    rename(fignp000:fignpx) prefix(s)
000600050128     ftignp01l  uf a e           k disk
000700050203     ftigcp52l  if   e           k disk
000800050211     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
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
002100050202     d contagca        s             10  0
002200050202     d contadupa       s             10  0
002300050202     d contanota       s             10  0
002400050202     d contagcp        s             10  0
002500050202     d contadupp       s             10  0
002600050202     d contanotp       s             10  0
002700050202     d contagcps       s             10  0
002800050202     d contadups       s             10  0
002900050202     d contanots       s             10  0
003000050202     d contanew        s             10  0
003100050211     d totarr          s             10  0
003200050211     d totpar          s             10  0
003300050211     d totsede         s             10  0
003400050131      *--------------------------------------------------------------------
003500050128      * NOTE ARRIVO
003600050128     c     *loval        setll     figna01l
003700000607     c                   do        *hival
003800050128     c                   read      figna01l
003900050128     c                   if        %eof(figna01l)
004000000607     c                   leave
004100000607     c                   end
004200050202     c                   add       1             contagca
004300050128     c                   eval      salvac = salvaa
004400050128     c* controllo giacenza
004500050128     c                   exsr      ctrgia
004600050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
004700050128     c                   if        ok = *on
004800050202     c                   add       1             contanota
004900050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
005000050131     C                   IF        CGNPAGC >= 2004
005100050131     c                   seton                                        20
005200050131     c                   exsr      stampa
005300050131     c                   end
005400050128     c     ktigcp        setgt     figna01l
005500050128     c                   iter
005600050131     c                   end
005700050128      * prima di scrivere il record verifico che non sia un record già
005800050128      * trascodificato
005900050128     c                   exsr      srtst
006000050128     c                   if        ok = *on
006100050202     c                   add       1             contadupa
006200050128     c                   iter
006300050128     c                   end
006400050128      *valorizza la ds del file tignp00f per la scrittura del rekord
006500050128     c                   movel     salvaa        salvat
006600050223     c* imposto la data di invio fax al patner prendendola dall data trasm.
006700050223     c* il flag = * è il primo record del gruppo di 5 che compone la nota
006800050223     c* il flag = ' ' nel caso non sia il primo record oppure nei record
006900050223     c* non trasmessi ma in quel caso la data è 0
007000050223     c                   if        agnpftf = ' ' or agnpftf = '*'
007100050223     c                   eval      gnpdif = agnpdtf
007200050223     c                   end
007300050128      * scrivo il rekord nel nuovo archivio
007400050202     c                   add       1             contanew
007500050128     c                   write     tignp000
007600040507     c                   enddo
007700050117     c
007800050128      * NOTE PARTENZA
007900050128     c                   clear                   wgnpagc
008000050128     c                   clear                   wgnpfgc
008100050128     c                   clear                   wgnpngc
008200050128     c     *loval        setll     fignp01l
008300050117     c                   do        *hival
008400050128     c                   read      fignp01l
008500050128     c                   if        %eof(fignp01l)
008600050117     c                   leave
008700050117     c                   end
008800050202     c                   add       1             contagcp
008900050128     c                   eval      salvac = salvap
009000050128     c* controllo giacenza
009100050128     c                   exsr      ctrgia
009200050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
009300050128     c                   if        ok = *on
009400050211     c                   add       1             contanotp
009500050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
009600050131     C                   IF        CGNPAGC >= 2004
009700050131     c                   seton                                        21
009800050131     c                   exsr      stampa
009900050131     C                   END
010000050128     c     ktigcp        setgt     fignp01l
010100050128     c                   iter
010200050128     c                   end
010300050128      * prima di scrivere il record verifico che non sia un record già
010400050128      * trascodificato
010500050128     c                   exsr      srtst
010600050128     c                   if        ok = *on
010700050211     c                   add       1             contadupp
010800050128     c                   iter
010900050128     c                   end
011000050128      *valorizza la ds del file tignp00f per la scrittura del rekord
011100050128     c                   movel     salvap        salvat
011200050223     c* imposto la data di invio fax al patner prendendola dall data trasm.
011300050223     c* il flag = * è il primo record del gruppo di 5 che compone la nota
011400050223     c* il flag = ' ' nel caso non sia il primo record oppure nei record
011500050223     c* non trasmessi ma in quel caso la data è 0
011600050223     c                   if        pgnpftf = ' ' or pgnpftf = '*'
011700050223     c                   eval      gnpdif = pgnpdtf
011800050223     c                   end
011900050128      * scrivo il rekord nel nuovo archivio
012000050202     c                   add       1             contanew
012100050128     c                   write     tignp000
012200050117     c                   enddo
012300050128      * NOTE SEDE
012400050128     c                   clear                   wgnpagc
012500050128     c                   clear                   wgnpfgc
012600050128     c                   clear                   wgnpngc
012700050128     c     *loval        setll     fignpx1l
012800050128     c                   do        *hival
012900050128     c                   read      fignpx1l
013000050128     c                   if        %eof(fignpx1l)
013100050128     c                   leave
013200050128     c                   end
013300050202     c                   add       1             contagcps
013400050128     c                   eval      salvac = salvas
013500050128     c* controllo giacenza
013600050128     c                   exsr      ctrgia
013700050128     c* se non trovo la giacenza sul file trascodificato salto a quella dopo
013800050128     c                   if        ok = *on
013900050211     c                   add       1             contanots
014000050131     C* Segnalo l'anomalia solo se la giacenza è del 2004 o 2005
014100050131     C                   IF        CGNPAGC >= 2004
014200050131     c                   seton                                        22
014300050131     c                   exsr      stampa
014400050131     c                   end
014500050128     c     ktigcp        setgt     fignpx1l
014600050128     c                   iter
014700050128     c                   end
014800050128      * prima di scrivere il record verifico che non sia un record già
014900050128      * trascodificato
015000050128     c                   exsr      srtst
015100050128     c                   if        ok = *on
015200050211     c                   add       1             contadups
015300050128     c                   iter
015400050128     c                   end
015500050128      *valorizza la ds del file tignp00f per la scrittura del rekord
015600050128     c                   movel     salvas        salvat
015700050223     c* imposto la data di invio fax al patner prendendola dall data trasm.
015800050223     c* il flag = * è il primo record del gruppo di 5 che compone la nota
015900050223     c* il flag = ' ' nel caso non sia il primo record oppure nei record
016000050223     c* non trasmessi ma in quel caso la data è 0
016100050223     c                   if        sgnpftf = ' ' or sgnpftf = '*'
016200050223     c                   eval      gnpdif = sgnpdtf
016300050223     c                   end
016400050128      * scrivo il rekord nel nuovo archivio
016500050202     c                   add       1             contanew
016600050128     c                   write     tignp000
016700050128     c                   enddo
016800050202     c*
016900050211     c                   eval      totarr = contagca - contanota - contadupa
017000050211     c                   eval      totpar = contagcp - contanotp - contadupp
017100050211     c                   eval      totsede= contagcps- contanots - contadups
017200050202     c                   except    totali
017300050131     c                   seton                                        lr
017400050118
017500050128      *-------------------------------------------------
017600050128     c     ctrgia        begsr
017700050128      *-------------------------------------------------
017800050128     c                   eval      ok = *off
017900050128     c                   if        cgnpagc <> wgnpagc or
018000050128     c                             cgnpfgc <> wgnpfgc or
018100050128     c                             cgnpngc <> wgnpngc
018200050128     c* controllo se ho la giacenza, se non esiste non trascodifico nemmeno
018300050128     c* la nota
018400050203     c     ktigcp        setll     tigcp52l                               01
018500050128     c                   if        not *in01
018600050128     c                   eval      ok = *on
018700050128     c                   end
018800050128     c                   eval      wgnpagc = cgnpagc
018900050128     c                   eval      wgnpfgc = cgnpfgc
019000050128     c                   eval      wgnpngc = cgnpngc
019100050128     c                   end
019200050128     c                   endsr
019300050114      *-------------------------------------------------
019400050128     c     srtst         begsr
019500050114      *-------------------------------------------------
019600050128     c                   eval      ok = *off
019700050128     c     ktignp        setll     tignp01l
019800050128     c                   do        *hival
019900050128     c     ktignp        reade     tignp01l
020000050128     c                   if        %eof(tignp01l)
020100050128     c                   leave
020200050128     c                   end
020300050128     c* confronto i tracciati record
020400050223     c                   if        salvat = %subst(salvac: 1: 73)
020500050128     c                   eval      ok = *on
020600050128     c                   leave
020700050128     c                   end
020800050128     c                   enddo
020900050128     c                   endsr
021000050128      *-------------------------------------------------
021100050131     c     stampa        begsr
021200050128      *-------------------------------------------------
021300050131     c   of              except    testa
021400050131     c                   eval      *inof = '0'
021500050131     c                   if        *in20 or *in21 or *in22
021600050131     c                   except    riga
021700050131     c                   end
021800050131     c                   setoff                                       202122
021900050131     c                   endsr
022000050131      *-------------------------------------------------
022100050131     c     *inzsr        begsr
022200050131      *-------------------------------------------------
022300050211     c                   except    testa
022400050131     c
022500050128     c     ktignp        klist
022600050128     c                   kfld                    cgnpAGC
022700050128     c                   kfld                    cgnpFGC
022800050128     c                   kfld                    cgnpNGC
022900050128     c                   kfld                    cgnpFRG
023000050128     c                   kfld                    cgnpFAS
023100050131     c                   kfld                    cgnpNPR
023200050128     c     ktigcp        klist
023300050128     c                   kfld                    cgnpAGC
023400050128     c                   kfld                    cgnpFGC
023500050128     c                   kfld                    cgnpNGC
023600050114     c                   endsr
023700050211     Oprtf198   E            TESTa            01
023800050131     O                                           65 'ANOMALIE TRASCODIFICA NOTE'
023900050131     O                                           +1 'GIACENZE'
024000050131     O                       PAGE1         Z    132
024100050131     O          E            RIGA       01
024200050202     O                                           38 'Giacenza non trovata nel f-
024300050202     O                                              ile TIGCP00F'
024400050211     O                       cgnpagc             43
024500050211     O                       cgnpfgc             47
024600050211     O                       cgnpngc       z     55
024700050211     O                                           60 'Nota'
024800050211     O               20                          67 'ARRIVO'
024900050211     O               21                          69 'PARTENZA'
025000050211     O               22                          65 'SEDE'
025100050211     O                                           88 'non trascodificata'
025200050211     O          E            totali     03
025300050202     o                                           +0 'Note ARRIVO'
025400050203     O                       contagca      z     +1
025500050202     o                                           +1 'Note ARRIVO non trasc.'
025600050202     o                                           +0 ' manca giacenza nel TI'
025700050202     o                                           +0 'GCP00F'
025800050203     O                       contanota     z     +1
025900050202     o                                           +1 'Note ARRIVO non trasc.'
026000050202     o                                           +0 ' record doppio'
026100050203     O                       contadupa     z     +1
026200050211     o                                           +1 'TOTALE ARRIVO ('
026300050211     O                       contagca      z     +0
026400050211     o                                           +0 '-'
026500050211     O                       contanota     z     +0
026600050211     o                                           +0 '-'
026700050211     O                       contadupa     z     +0
026800050211     o                                           +0 ')'
026900050211     O                       totarr        z     +1
027000050202     O          E            totali     01
027100050202     o                                           +0 'Note PART. '
027200050203     O                       contagcp      z     +1
027300050202     o                                           +1 'Note PART.  non trasc.'
027400050202     o                                           +0 ' manca giacenza nel TI'
027500050202     o                                           +0 'GCP00F'
027600050203     O                       contanotp     z     +1
027700050202     o                                           +1 'Note PART.  non trasc.'
027800050202     o                                           +0 ' record doppio'
027900050203     O                       contadupp     z     +1
028000050211     o                                           +1 'TOTALE PARTENZA ('
028100050211     O                       contagcp      z     +0
028200050211     o                                           +0 '-'
028300050211     O                       contanotp     z     +0
028400050211     o                                           +0 '-'
028500050211     O                       contadupp     z     +0
028600050211     o                                           +0 ')'
028700050211     O                       totpar        z     +1
028800050202     O          E            totali     01
028900050202     o                                           +0 'Note SEDE  '
029000050203     O                       contagcps     z     +1
029100050202     o                                           +1 'Note SEDE   non trasc.'
029200050202     o                                           +0 ' manca giacenza nel TI'
029300050202     o                                           +0 'GCP00F'
029400050203     O                       contanots     z     +1
029500050202     o                                           +1 'Note SEDE   non trasc.'
029600050202     o                                           +0 ' record doppio'
029700050203     O                       contadups     z     +1
029800050211     o                                           +1 'TOTALE SEDE ('
029900050211     O                       contagcps     z     +0
030000050211     o                                           +0 '-'
030100050211     O                       contanots     z     +0
030200050211     o                                           +0 '-'
030300050211     O                       contadups     z     +0
030400050211     o                                           +0 ')'
030500050211     O                       totsede       z     +1
030600050202     O          E            totali     01
030700050202     o                                           +0 'Note TRASCODIFICATE'
030800050211     o                                           +1 '('
030900050211     O                       totarr        z     +0
031000050211     o                                           +0 '+'
031100050211     O                       totpar        z     +0
031200050211     o                                           +0 '+'
031300050211     O                       totsede       z     +0
031400050211     o                                           +0 ')'
031500050203     O                       contanew      z     +1
