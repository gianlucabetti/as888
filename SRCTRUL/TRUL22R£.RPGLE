000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300011030      *  Routine calcolo limiti importo da assicurare
000400011030      * ----------------------------------------------
000500011011      *
000600011029      *  Il programma riceve l'importo da controllare e la divisa
000700011029      *  se la divisa non è euro converte l'importo in euro
000800011112
000900011112     ftabel00f  if   e           k disk    usropn
001000011112     fazorg01l  if   e           k disk    usropn
001100011030
001200011112     d og143         e ds
001300011112     d ds3a          e ds
001400011030     d dlia          e ds
001500011030     d trul22ds      e ds
001600011029     d tibs02DS      e ds
001700011030     d yeurcods      e ds
001800011030     d kpjba         e ds
001900011030
002000011113     d fil             s              3  0 dim(50)                              filiale
002100011113     d fl1             s              1    dim(50)                              imp/exp
002200011113     d cbo             s              2    dim(50)                              tipo bolla
002300011112     d tb1             s              1    dim(50)                              p.a./p.f.
002400011112
002500021209     d Ksc4            s              4  0
002600011113     d xx              s              2  0
002700011112     d yy              s              2  0
002800011113     d zz              s              2  0
002900011113     d w_key1          s              3
003000011112     d w_importo       s             17  3
003100011113     d w_paspfr        s              1
003200011019
003300011019      *----------------------------------------------------------------
003400011019
003500011029     c     *entry        plist
003600011030     c                   parm                    trul22ds
003700011112
003800011112     c     ktabel00      klist
003900011112     c                   kfld                    tblkut
004000011112     c                   kfld                    tblcod
004100011019
004200011030     c                   clear                   o22lmn
004300011030     c                   clear                   o22fmn
004400011030     c                   clear                   o22lx1
004500011030     c                   clear                   o22fx1
004600011030     c                   clear                   o22lx2
004700011030     c                   clear                   o22fx2
004800011030
004900011112      * Controllo validità tipo lancio
005000011112     c                   if        i22tla <> 'C' and i22tla <> 'L' and
005100011112     c                             i22tla <> *blanks
005200011112     c                   clear                   i22tla
005300011112     c                   endif
005400011112
005500020207      * Se Tipo lancio 'C' --> solo chiusura file
005600011030     c                   if        i22tla <> 'C'
005700011029     c                   exsr      elabora
005800011029     c                   endif
005900011011
006000011030     c                   if        i22tla <> *blanks
006100011019     c                   eval      *inLR = *on
006200011029     c                   else
006300011019     c                   eval      *inRT = *on
006400011029     c                   endif
006500011011
006600011011      *----------------------------------------------------
006700011019      * ELABORAZIONE
006800011011      *----------------------------------------------------
006900011029     c     elabora       begsr
007000020225
007100020225      * Se il programma chiamante è la manutenzione bolle in sede
007200020225      * non faccio nessun tipo di controllo e aggancio subito la tabella standard
007300020225     c                   if        i22pgm = 'TNSB52R'
007400020225     c                   eval      w_key1 = '001'
007500020225     c                   goto      rifai
007600020225     c                   endif
007700011112
007800011113      * Carico dati azorg in schiera se mai fatto
007900011113     c                   if        fil(1) = *zeros
008000011113     c                   exsr      caricafl1
008100011112     c                   endif
008200011112
008300011112      * Carico tabella in schiera se mai fatto
008400011112     c                   if        cbo(1) = *blanks
008500011113     c                   exsr      caricacbo
008600011112     c                   endif
008700011112
008800011112     c                   eval      xx = 1
008900011112     c                   eval      yy = 1
009000011113     c                   eval      zz = 1
009100011113
009200011113     c                   eval      w_key1 = '001'
009300011112
009400011113      * creo le chiavi per accedere alla tabella LIA
009500011113      * porti assegnati export
009600011211     c                   if        i22lna <> 0
009700011113     c     i22lna        lookup    fil(xx)                                30
009800011211     c                   endif
009900011113     c   30i22cbo        lookup    cbo(yy)                                31
010000011112     c                   if        *in30 and *in31
010100011112     c                   eval      w_key1 = '002'
010200011113     c                   else
010300011113      * import
010400011211     c                   if        i22lnp <> 0
010500011113     c     i22lnp        lookup    fil(zz)                                32
010600011211     c                   endif
010700011113     c                   if        *in32
010800011113     c                   eval      w_key1 = '002'
010900011113     c                   endif
011000011113     c                   endif
011100021209      * Clienti non codificati
011200021209     c                   Move      I22Ksc        Ksc4
011300021210     c                   If        Ksc4 = 8888
011400021209     c                   eval      w_key1 = '003'
011500021209     c                   EndIf
011600011030
011700011030      * aggancio la tabella LIA
011800020123     c     rifai         tag
011900011030     c                   clear                   dlia
012000011030     c                   clear                   tibs02ds
012100011029     c                   movel     'C'           T02mod
012200011029     c                   movel     knsif         t02sif
012300011030     c                   movel     'LIA'         t02cod
012400011030     c                   movel(p)  w_key1        T02ke1
012500011029     c                   call      'TIBS02R'
012600011030     c                   parm                    kpjba
012700011030     c                   parm                    tibs02ds
012800011030     c                   if        t02err = *blanks
012900011030     c                   movel     t02uni        dlia
013000020123      * se errore e la tabella cercata non era la standard rifaccio con la standard
013100020123     c                   else
013200020123     c                   if        t02err = 'E' and w_key1 <> '001'
013300020123     c                   eval      w_key1 = '001'
013400020123     c                   goto      rifai
013500020123     c                   endif
013600011029     c                   endif
013700011030
013800011030      * converto l'importo ricevuto in EURO
013900011030     c                   z-add     i22imp        w_importo
014000011030     c                   if        i22div <> 'EUR'
014100011030     c                   clear                   yeurcods
014200011030     c                   movel     i22div        yecdvi
014300011030     c                   z-add     i22imp        yecimi
014400011030     c                   movel     'EUR'         yecdvo
014500011030     c                   call      'YEURCO'
014600011030     c                   parm                    yeurcods
014700011030     c                   if        yecesi = *blanks
014800011030     c                   z-add     yecimo        w_importo
014900011203      * se l'importo da controllare non è a 0 e se dalla conversione mi è
015000011203      * tornato 0 devo forzare a 0,01
015100011203     c                   if        i22imp > 0 and yecimo = 0
015200011203     c                   z-add     0,01          w_importo
015300011203     c                   endif
015400011030     c                   endif
015500011030     c                   endif
015600011030
015700011030     c                   z-add     §liamn        o22lmn
015800011030     c                   z-add     §liax1        o22lx1
015900011030     c                   z-add     §liax2        o22lx2
016000011030
016100011030      * controllo gli importi
016200011113     c                   if        w_importo < o22lmn
016300011030     c                   eval      o22fmn = '1'
016400011030     c                   endif
016500011030     c                   if        w_importo > o22lx1
016600011030     c                   eval      o22fx1 = '1'
016700011030     c                   endif
016800011030     c                   if        w_importo > o22lx2
016900011030     c                   eval      o22fx2 = '1'
017000011030     c                   endif
017100011029
017200011029     c                   endsr
017300011112      *----------------------------------------------------
017400011112      * Carico schiera azorg
017500011112      *----------------------------------------------------
017600011112     c     caricafl1     begsr
017700011112
017800011112     c                   open      azorg01l
017900011112
018000011112     c                   clear                   xx
018100011112
018200011112     c     *loval        setll     azorg01l
018300011112     c                   do        *hival
018400011112     c                   read      azorg01l
018500011112     c                   if        %eof
018600011112     c                   leave
018700011112     c                   endif
018800011112     c                   if        orgfva <> *blanks
018900011112     c                   iter
019000011112     c                   endif
019100011112     c                   if        orgfag <> 'A' and orgfag <> 'F'
019200011112     c                   iter
019300011112     c                   endif
019400011112     c                   eval      og143 = orgde3
019500020213     c                   if        §ogntw = 'EEX' or §ogntw = 'DPD' or
019600020213     c                             §ogntw = 'FED' or §ogntw = 'EUP'
019700011112     c                   eval      xx = xx + 1
019800011112     c                   eval      fil(xx) = orgfil
019900011113     c                   eval      fl1(xx) = 'E'
020000011113     c                   endif
020100011112     c                   enddo
020200011112
020300011113     c                   close     azorg01l
020400011112
020500011112     c                   endsr
020600011112      *----------------------------------------------------
020700011112      * Carico schiera
020800011112      *----------------------------------------------------
020900011112     c     caricacbo     begsr
021000011112
021100011112     c                   eval      tblkut = 1
021200011112     c                   eval      tblcod = '3A'
021300011112
021400011112     c                   open      tabel00f
021500011112
021600011113     c                   clear                   xx
021700011112
021800011112     c     ktabel00      setll     tabel00f
021900011112     c                   do        *hival
022000011112     c     ktabel00      reade     tabel00f
022100011112     c                   if        %eof
022200011112     c                   leave
022300011112     c                   endif
022400011112     c                   if        tblflg <> *blanks
022500011112     c                   iter
022600011112     c                   endif
022700011112     c                   eval      ds3a = tbluni
022800011113     c                   movel     §3atb1        w_paspfr
022900011113     c                   if        w_paspfr = 'A'
023000011113     c                   eval      xx = xx + 1
023100011113     c                   eval      cbo(xx) = tblkey
023200011113     c                   eval      tb1(xx) = w_paspfr
023300011112     c                   endif
023400011112     c                   enddo
023500011112
023600011112     c                   close     tabel00f
023700011112
023800011112     c                   endsr
