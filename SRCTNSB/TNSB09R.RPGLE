000100061024     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200061024
000300061206      *------------------------------------------------------------------------*
000400120709      * Cancellazione immagini DOC.Clienti - bolle eliminate                   *
000500061206      *------------------------------------------------------------------------*
000600061206
000700120709     ftijde00r  up   e             disk
000800061206     ftitas30c  if   e           k disk
000900120709     ftijdd01l  uf a e           k disk
001000061207
001100061207     d comora          s              6
001200061207     d comtim          s             14  0
001300070301     d kladaas         s                   like(ladaas)
001400070301     d kladlnp         s                   like(ladlnp)
001500070301     d kladnrs         s                   like(ladnrs)
001600070301     d kladnsp         s                   like(ladnsp)
001700070301     d wnoagg          s              1    inz(*off)
001800061207
001900061207     d comdta          ds
002000061207     d  comaa                  1      4
002100061207     d  commm                  5      6
002200061207     d  comgg                  7      8
002300061207
002400061207     d laeimm          ds
002500061207     d  waa                    1      2  0
002600061207     d  wlnp                   3      5  0
002700061207     d  wnrs                   6      7  0
002800061207     d  wnsp                   8     14  0
002900061207
003000061207     d wlbda8          ds
003100061207     d  g02dat                 1      8  0
003200061207     d  g02inv                 9     16  0
003300061207     d  g02err                17     17
003400061207     d  g02tgi                18     22  0
003500061204
003600061204     d kpjba         e ds
003700061204
003800061207      * non elaboro se cod.avanzamento 20 o 30
003900061207     c                   if        laeela <> '20' and laeela <> '30'
004000070301
004100070301     c                   eval      wnoagg = *off
004200061206
004300061207      * preparo la chiave per il file titas
004400061207     c                   z-add     waa           tasaas
004500061207     c                   if        waa < 40
004600061207     c                   add       2000          tasaas
004700061207     c                   else
004800061207     c                   add       1900          tasaas
004900061207     c                   endif
005000061207
005100061207     c                   move      wlnp          taslnp
005200061207     c                   move      wnrs          tasnrs
005300061207     c                   move      wnsp          tasnsp
005400061206
005500061207      * controllo se esiste la bolla
005600061207     c     ktitas        setll     titas30c
005700061207      * se trovo la bolla aggiorno il tilae con il cod. avanzamento 20 per trovato
005800061207     c                   if        %equal(titas30c)
005900061207     c                   move      '20'          laeela
006000120709     c                   update    tijde000
006100061207     c                   else
006200120709      * se non trovo la bolla devo scrivere un rcd sul tijdd
006300120709     c                   exsr      sr_tijdd
006400120709      * aggiorno il tijde con il cod. avanzamento 30 non trovato
006500120710      * se ho fatto aggiornamento o scrittura su tijdd
006600070301     c                   if        wnoagg = *off
006700061207     c                   move      '30'          laeela
006800120709     c                   update    tijde000
006900070301      * se non aggiorno rilascio il rcd
007000070301     c                   else
007100120709     c                   unlock    tijde00r
007200070301     c                   endif
007300061207     c                   endif
007400061206
007500061207     c                   endif
007600061206
007700061207      *------------------------------------------------------------------------*
007800120709      *  scrivo rcd su tijdd
007900061207      *------------------------------------------------------------------------*
008000120709     c     sr_tijdd      begsr
008100070301
008200070301      * imposto la chiave
008300070301     c                   move      waa           kladaas
008400070301     c                   move      wlnp          kladlnp
008500070301     c                   move      wnrs          kladnrs
008600070301     c                   move      wnsp          kladnsp
008700070301
008800120709      * aggancio TIJDD
008900120709     c     ktijdd        chain     tijdd01l
009000070301
009100070301      * se trovo il rcd
009200120709     c                   if        %found(tijdd01l)
009300070301
009400070301      * --> è ancora con ladela = '00' non devo fare niente
009500070301     c                   if        ladela = '00'
009600070301     c                   eval      wnoagg = *on
009700070301      *     e rilascio il rcd
009800120709     c                   unlock    tijdd01l
009900070301     c                   leavesr
010000070301     c                   endif
010100070301
010200070301      * --> aggiorno i dati se non è più con ladela = '00'
010300070301     c                   exsr      sr_movdati
010400120709     c                   update    tijdd000
010500070301
010600070301      * se non ho trovato il record
010700070301     c                   else
010800070301
010900070301      * imposto i dati del file
011000120709     c                   clear                   tijdd000
011100070301     c                   eval      ladaas = kladaas
011200070301     c                   eval      ladlnp = kladlnp
011300070301     c                   eval      ladnrs = kladnrs
011400070301     c                   eval      ladnsp = kladnsp
011500070301     c                   exsr      sr_movdati
011600070301      * scrivo il nuovo rcd
011700120709     c                   write     tijdd000
011800070301     c                   endif
011900061206
012000061207     c                   endsr
012100070301
012200070301      *------------------------------------------------------------------------*
012300120710      *  imposto i dati di tijdd
012400070301      *------------------------------------------------------------------------*
012500070301     c     sr_movdati    begsr
012600070301
012700070301      * imposto la data e ora del lavoro
012800070301     c                   time                    comtim
012900070301     c                   movel     comtim        comora
013000070301     c                   move      comtim        g02dat
013100070301     c                   move      *zeros        g02inv
013200070301     c                   movel     *blanks       g02err
013300070301     c                   call      'XSRDA8'
013400070301     c                   parm                    wlbda8
013500070301     c                   move      g02inv        comdta
013600070301     c                   eval      ladtim = comaa + commm+ comgg + comora
013700070301
013800070301      * imposto il codice operazione
013900070301     c                   move      'MOV'         ladcop
014000070301
014100070301      * imposto la directory
014200120921     c                   eval      laddir = 'DAELIMINARE_C'
014300070301
014400070301      * imposto il codice avanzamento
014500070301     c                   move      '00'          ladela
014600070301
014700070301     c                   endsr
014800061207
014900061207      *------------------------------------------------------------------------*
015000061207      *  Routine iniziale
015100061207      *------------------------------------------------------------------------*
015200061207     c     *inzsr        begsr
015300061207
015400061207     c     *entry        plist
015500061207     c                   parm                    kpjba
015600061207
015700061207     c     ktitas        klist
015800061207     c                   kfld                    tasaas
015900061207     c                   kfld                    taslnp
016000061207     c                   kfld                    tasnrs
016100061207     c                   kfld                    tasnsp
016200061207
016300120709     c     ktijdd        klist
016400070301     c                   kfld                    kladaas
016500070301     c                   kfld                    kladlnp
016600070301     c                   kfld                    kladnrs
016700070301     c                   kfld                    kladnsp
016800070301
016900061207     c                   endsr
