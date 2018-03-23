000100061024     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200061024
000300061206      *------------------------------------------------------------------------*
000400061206      * Cancellazione immagini LAGUNA - bolle eliminate                        *
000500061206      *------------------------------------------------------------------------*
000600061206
000700061206     ftilae00r  up   e             disk
000800061206     ftitas30c  if   e           k disk
000900061206     ftilad00f  o    e             disk
001000061207
001100061207     d comora          s              6
001200061207     d comtim          s             14  0
001300061207
001400061207     d comdta          ds
001500061207     d  comaa                  1      4
001600061207     d  commm                  5      6
001700061207     d  comgg                  7      8
001800061207
001900061207     d laeimm          ds
002000061207     d  waa                    1      2  0
002100061207     d  wlnp                   3      5  0
002200061207     d  wnrs                   6      7  0
002300061207     d  wnsp                   8     14  0
002400061207
002500061207     d wlbda8          ds
002600061207     d  g02dat                 1      8  0
002700061207     d  g02inv                 9     16  0
002800061207     d  g02err                17     17
002900061207     d  g02tgi                18     22  0
003000061204
003100061204     d kpjba         e ds
003200061204
003300061207      * non elaboro se cod.avanzamento 20 o 30
003400061207     c                   if        laeela <> '20' and laeela <> '30'
003500061206
003600061207      * preparo la chiave per il file titas
003700061207     c                   z-add     waa           tasaas
003800061207     c                   if        waa < 40
003900061207     c                   add       2000          tasaas
004000061207     c                   else
004100061207     c                   add       1900          tasaas
004200061207     c                   endif
004300061207
004400061207     c                   move      wlnp          taslnp
004500061207     c                   move      wnrs          tasnrs
004600061207     c                   move      wnsp          tasnsp
004700061206
004800061207      * controllo se esiste la bolla
004900061207     c     ktitas        setll     titas30c
005000061207      * se trovo la bolla aggiorno il tilae con il cod. avanzamento 20 per trovato
005100061207     c                   if        %equal(titas30c)
005200061207     c                   move      '20'          laeela
005300061207     c                   update    tilae000
005400061207     c                   else
005500061207      * se non trovo la bolla devo scrivere un rcd sul tilad
005600061207     c                   exsr      sr_tilad
005700061207      * aggiorno il tilae con il cod. avanzamento 30 non trovato
005800061207     c                   move      '30'          laeela
005900061207     c                   update    tilae000
006000061207     c                   endif
006100061206
006200061207     c                   endif
006300061206
006400061207      *------------------------------------------------------------------------*
006500061207      *  scrivo rcd su tilad
006600061207      *------------------------------------------------------------------------*
006700061207     c     sr_tilad      begsr
006800061206
006900061207     c                   clear                   tilad000
007000061206
007100061207      * imposto la data e ora del lavoro
007200061207     c                   time                    comtim
007300061207     c                   movel     comtim        comora
007400061207     c                   move      comtim        g02dat
007500061207     c                   move      *zeros        g02inv
007600061207     c                   movel     *blanks       g02err
007700061207     c                   call      'XSRDA8'
007800061207     c                   parm                    wlbda8
007900061207     c                   move      g02inv        comdta
008000061207     c                   eval      ladtim = comaa + commm+ comgg + comora
008100061206
008200061207      * imposto la spedizione
008300061207     c                   move      waa           ladaas
008400061207     c                   move      wlnp          ladlnp
008500061207     c                   move      wnrs          ladnrs
008600061207     c                   move      wnsp          ladnsp
008700061206
008800061207      * imposto il codice operazione
008900070122     c                   move      'MOV'         ladcop
009000061206
009100061207      * imposto la directory
009200061207     c                   movel     'DAELIMINARE' laddir
009300061206
009400061207      * imposto il codice avanzamento
009500061207     c                   move      '00'          ladela
009600061206
009700061207      * scrivo il rcd
009800061207     c                   write     tilad000                             99
009900061206
010000061207     c                   endsr
010100061207
010200061207      *------------------------------------------------------------------------*
010300061207      *  Routine iniziale
010400061207      *------------------------------------------------------------------------*
010500061207     c     *inzsr        begsr
010600061207
010700061207     c     *entry        plist
010800061207     c                   parm                    kpjba
010900061207
011000061207     c     ktitas        klist
011100061207     c                   kfld                    tasaas
011200061207     c                   kfld                    taslnp
011300061207     c                   kfld                    tasnrs
011400061207     c                   kfld                    tasnsp
011500061207
011600061207     c                   endsr
