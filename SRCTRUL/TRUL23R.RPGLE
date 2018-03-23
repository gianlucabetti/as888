000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300011030      *  Routine calcolo limiti valore merce
000400011030      * -------------------------------------
000500011011      *
000600011029      *  Il programma riceve l'importo da controllare e la divisa
000700011029      *  se la divisa non è euro converte l'importo in euro
000800011113
000900011113     ftabel00f  if   e           k disk    usropn
001000011113     fazorg01l  if   e           k disk    usropn
001100011030
001200011113     d og143         e ds
001300011113     d ds15          e ds
001400011030     d dlvm          e ds
001500011030     d trul23ds      e ds
001600011029     d tibs02DS      e ds
001700011030     d yeurcods      e ds
001800011030     d kpjba         e ds
001900011030
002000020207     d fil             s              3  0 dim(50)                              filiale dpd/FedEx
002100020207     d ntw             s              3    dim(50)                              Network
002200011217     d nzd             s              3    dim(500)                             Nazioni
002300011217     d eft             s              1    dim(500)                             dogana/attr.dogana
002400011217     d efd             s              1    dim(500)                             dogana/attr.dogana dpd
002500020207     d eff             s              1    dim(500)                             dogana/attr.dogana FedEx
002600011217     d ita             s              1    dim(500)                             ita/est
002700011113
002800011217     d xx              s              3  0
002900011217     d yy              s              3  0
003000011113     d w_key1          s              3
003100011113     d w_importo       s             17  3
003200011019
003300011019      *----------------------------------------------------------------
003400011019
003500011029     c     *entry        plist
003600011030     c                   parm                    trul23ds
003700011113
003800011113     c     ktabel00      klist
003900011113     c                   kfld                    tblkut
004000011113     c                   kfld                    tblcod
004100011019
004200011030     c                   clear                   o23lmn
004300011030     c                   clear                   o23fmn
004400011030     c                   clear                   o23lx1
004500011030     c                   clear                   o23fx1
004600011030     c                   clear                   o23lx2
004700011030     c                   clear                   o23fx2
004800011113
004900011113     c                   if        i23nzd = *blanks
005000011113     c                   eval      i23nzd = '&'
005100011113     c                   endif
005200011113
005300011113      * Controllo validità tipo lancio
005400011113     c                   if        i23tla <> 'C' and i23tla <> 'L' and
005500011113     c                             i23tla <> *blanks
005600011113     c                   clear                   i23tla
005700011113     c                   endif
005800011030
005900011019      * Se Tipo lancio 'C' --> solo chiusura file
006000011030     c                   if        i23tla <> 'C'
006100011029     c                   exsr      elabora
006200011029     c                   endif
006300011011
006400011030     c                   if        i23tla <> *blanks
006500011019     c                   eval      *inLR = *on
006600011029     c                   else
006700011019     c                   eval      *inRT = *on
006800011029     c                   endif
006900011011
007000011011      *----------------------------------------------------
007100011019      * ELABORAZIONE
007200011011      *----------------------------------------------------
007300011029     c     elabora       begsr
007400011113
007500011113      * Carico dati azorg in schiera se mai fatto
007600011113     c                   if        fil(1) = *zeros
007700020207     c                   exsr      caricafil
007800011113     c                   endif
007900011113
008000011113      * Carico tabelle in schiera se mai fatto
008100011113     c                   if        nzd(1) = *blanks
008200011113     c                   exsr      caricanzd
008300011113     c                   endif
008400011113
008500011113     c                   eval      xx = 1
008600011113     c                   eval      yy = 1
008700011113
008800011113     c                   eval      w_key1 = '001'
008900011113
009000011113      * creo le chiavi per accedere alla tabella LVM
009100011113      * destino estero con dogana o attraversamento dogana
009200011113      * destino estero
009300011113     c     i23nzd        lookup    nzd(xx)                                30
009400011113  b1 c                   if        *in30 and ita(xx) = 'E'
009500011113      * dpd
009600011211     c                   if        i23lna <> *zeros
009700011113     c     i23lna        lookup    fil(yy)                                31
009800011211     c                   endif
009900011113  b2 c                   if        *in31
010000020207     c                   select
010100020207     c                   when      ntw(yy) = 'DPD'
010200011113  b3 c                   if        efd(xx) <> *blanks
010300011113     c                   eval      w_key1 = '002'
010400011113  e3 c                   endif
010500020207      * FedEx
010600020207     c                   when      ntw(yy) = 'FED'
010700020207  b3 c                   if        eff(xx) <> *blanks
010800020207     c                   eval      w_key1 = '002'
010900020207  e3 c                   endif
011000020207     c                   endsl
011100020207
011200020207  x2 c                   else
011300011113      * no dpd
011400011113  b3 c                   if        eft(xx) <> *blanks
011500011113     c                   eval      w_key1 = '002'
011600011113  e3 c                   endif
011700011113  e2 c                   endif
011800011113  e1 c                   endif
011900011030
012000011030      * aggancio la tabella LVM
012100020123     c     rifai         tag
012200011030     c                   clear                   dlvm
012300011030     c                   clear                   tibs02ds
012400011029     c                   movel     'C'           T02mod
012500011029     c                   movel     knsif         t02sif
012600011030     c                   movel     'LVM'         t02cod
012700011030     c                   movel(p)  w_key1        T02ke1
012800011029     c                   call      'TIBS02R'
012900011030     c                   parm                    kpjba
013000011030     c                   parm                    tibs02ds
013100011030     c                   if        t02err = *blanks
013200011030     c                   movel     t02uni        dlvm
013300020123      * se errore e la tabella cercata non era la standard rifaccio con la standard
013400020123     c                   else
013500020123     c                   if        t02err = 'E' and w_key1 <> '001'
013600020123     c                   eval      w_key1 = '001'
013700020123     c                   goto      rifai
013800020123     c                   endif
013900011029     c                   endif
014000011030
014100011030      * converto l'importo ricevuto in EURO
014200011030     c                   z-add     i23imp        w_importo
014300011030     c                   if        i23div <> 'EUR'
014400011030     c                   clear                   yeurcods
014500011030     c                   movel     i23div        yecdvi
014600011030     c                   z-add     i23imp        yecimi
014700011030     c                   movel     'EUR'         yecdvo
014800011030     c                   call      'YEURCO'
014900011030     c                   parm                    yeurcods
015000011030     c                   if        yecesi = *blanks
015100011030     c                   z-add     yecimo        w_importo
015200011203      * se l'importo da controllare non è a 0 e se dalla conversione mi è
015300011203      * tornato 0 devo forzare a 0,01
015400011203     c                   if        i23imp > 0 and yecimo = 0
015500011203     c                   z-add     0,01          w_importo
015600011203     c                   endif
015700011030     c                   endif
015800011030     c                   endif
015900011030
016000011030     c                   z-add     §lvmmn        o23lmn
016100011030     c                   z-add     §lvmx1        o23lx1
016200011030     c                   z-add     §lvmx2        o23lx2
016300011030
016400011030      * controllo gli importi
016500011113     c                   if        w_importo < o23lmn
016600011030     c                   eval      o23fmn = '1'
016700011030     c                   endif
016800011030     c                   if        w_importo > o23lx1
016900011030     c                   eval      o23fx1 = '1'
017000011030     c                   endif
017100011030     c                   if        w_importo > o23lx2
017200011030     c                   eval      o23fx2 = '1'
017300011030     c                   endif
017400011029
017500011029     c                   endsr
017600011113      *----------------------------------------------------
017700011113      * Carico schiera azorg
017800011113      *----------------------------------------------------
017900020207     c     caricafil     begsr
018000011113
018100011113     c                   open      azorg01l
018200011113
018300011113     c                   clear                   xx
018400011113
018500011113     c     *loval        setll     azorg01l
018600011113     c                   do        *hival
018700011113     c                   read      azorg01l
018800011113     c                   if        %eof
018900011113     c                   leave
019000011113     c                   endif
019100011113     c                   if        orgfva <> *blanks
019200011113     c                   iter
019300011113     c                   endif
019400011113     c                   if        orgfag <> 'A' and orgfag <> 'F'
019500011113     c                   iter
019600011113     c                   endif
019700011113     c                   eval      og143 = orgde3
019800020207     c                   if        §ogntw = 'DPD' or §ogntw = 'FED'
019900011113     c                   eval      xx = xx + 1
020000011113     c                   eval      fil(xx) = orgfil
020100020207     c                   eval      ntw(xx) = §ogntw
020200011113     c                   endif
020300011113     c                   enddo
020400011113
020500011113     c                   close     azorg01l
020600011113
020700011113     c                   endsr
020800011113      *----------------------------------------------------
020900011113      * Carico schiere
021000011113      *----------------------------------------------------
021100011113     c     caricanzd     begsr
021200011113
021300011113     c                   eval      tblkut = 1
021400011113     c                   eval      tblcod = '15'
021500011113
021600011113     c                   open      tabel00f
021700011113
021800011113     c                   clear                   xx
021900011113
022000011113     c     ktabel00      setll     tabel00f
022100011113     c                   do        *hival
022200011113     c     ktabel00      reade     tabel00f
022300011113     c                   if        %eof
022400011113     c                   leave
022500011113     c                   endif
022600011113     c                   if        tblflg <> *blanks
022700011113     c                   iter
022800011113     c                   endif
022900011113     c                   eval      ds15 = tbluni
023000011113     c                   eval      xx = xx + 1
023100011113     c                   if        tblkey = *blanks
023200011113     c                   eval      tblkey = '&'
023300011113     c                   endif
023400011113     c                   eval      nzd(xx) = tblkey
023500011113     c                   eval      eft(xx) = §15eft
023600011113     c                   eval      efd(xx) = §15efd
023700020207     c                   eval      eff(xx) = §15eff
023800011113     c                   if        §15ita = 'I'
023900011113     c                   eval      ita(xx) = 'I'
024000011113     c                   else
024100011113     c                   eval      ita(xx) = 'E'
024200011113     c                   endif
024300011113     c                   enddo
024400011113
024500011113     c                   close     tabel00f
024600011113
024700011113     c                   endsr
