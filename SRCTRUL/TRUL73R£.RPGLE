000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300050301      * ------------------------------------------
000400011030      *  Routine calcolo limiti importo c/assegno
000500011030      * ------------------------------------------
000600011011      *
000700011029      *  Il programma riceve l'importo da controllare e la divisa
000800011029      *  se la divisa non è euro converte l'importo in euro
000900011030
001000011106     ftabel00f  if   e           k disk    usropn
001100011019
001200011031     d ds15          e ds
001300011030     d ds1a          e ds
001400011029     d dlca          e ds
001500011029     d trul21ds      e ds
001600011029     d tibs02DS      e ds
001700011030     d yeurcods      e ds
001800011030     d kpjba         e ds
001900011106
002000011106     d tic             s              2    dim(50)                              Tipo incasso
002100011106     d fca             s              1    dim(50)                              Cont/Banc/Circ
002200050301     d fmb             s              1    dim(50)                              Cont/Banc/Circ
002300050301     d ttm             s              1    dim(50)                              Cont/Banc/Circ
002400011217     d nzd             s              3    dim(500)                             Nazioni
002500011217     d ita             s              1    dim(500)                             ita/est
002600011030
002700011217     d xx              s              3  0
002800011217     d yy              s              3  0
002900011113     d w_key1          s              3
003000011106     d w_importo       s             17  3
003100011019
003200011019      *----------------------------------------------------------------
003300011019
003400011029     c     *entry        plist
003500011029     c                   parm                    trul21ds
003600011106
003700011106     c     ktabel00      klist
003800011106     c                   kfld                    tblkut
003900011106     c                   kfld                    tblcod
004000011019
004100011029     c                   clear                   o21lmn
004200011029     c                   clear                   o21fmn
004300011029     c                   clear                   o21lx1
004400011029     c                   clear                   o21fx1
004500011029     c                   clear                   o21lx2
004600011029     c                   clear                   o21fx2
004700011029     c                   clear                   o21lca
004800011029     c                   clear                   o21fca
004900050301     c                   clear                   o21lb
005000050301     c                   clear                   o21fb
005100011106
005200011106     c                   if        i21tic = *blanks
005300011106     c                   eval      i21tic = '&'
005400011106     c                   endif
005500011106
005600011106     c                   if        i21nzd = *blanks
005700011106     c                   eval      i21nzd = '&'
005800011106     c                   endif
005900011031
006000011031      * Controllo validità tipo lancio
006100011031     c                   if        i21tla <> 'C' and i21tla <> 'L' and
006200011031     c                             i21tla <> *blanks
006300011031     c                   clear                   i21tla
006400011031     c                   endif
006500011019
006600011019      * Se Tipo lancio 'C' --> solo chiusura file
006700011030     c                   if        i21tla <> 'C'
006800011029     c                   exsr      elabora
006900011029     c                   endif
007000011011
007100011029     c                   if        i21tla <> *blanks
007200011031     c                   eval      *inlr = *on
007300011029     c                   else
007400011031     c                   eval      *inrt = *on
007500011029     c                   endif
007600011011
007700011011      *----------------------------------------------------
007800011019      * ELABORAZIONE
007900011011      *----------------------------------------------------
008000011029     c     elabora       begsr
008100011106
008200011106      * Carico tabelle in schiera se mai fatto
008300011106     c                   if        tic(1) = *blanks
008400011106     c                   exsr      Caricatic
008500011106     c                   endif
008600011106     c                   if        nzd(1) = *blanks
008700011106     c                   exsr      Caricanzd
008800011106     c                   endif
008900011106
009000011106     c                   eval      xx = 1
009100011106     c                   eval      yy = 1
009200011030
009300011030      * creo le chiavi per accedere alla tabella LCA
009400011106     c     i21tic        lookup    tic(xx)                                30
009500011106     c     i21nzd        lookup    nzd(yy)                                31
009600011106     c                   if        *in30 and *in31
009700011030
009800011106     c                   select
009900020123
010000020123      * servizio poste bancario
010100020123     c                   when      i21tsp = 'P' and fca(xx) = 'B'
010200020123     c                   eval      w_key1 = '008'
010300020123      * servizio poste circolare
010400020123     c                   when      i21tsp = 'P' and fca(xx) = 'C'
010500020123     c                   eval      w_key1 = '009'
010600020123      * servizio poste contanti
010700080625     c                   when      i21tsp = 'P' and (fca(xx) = 'S'  or
010800080625     c                                               fca(XX) = '2')
010900020123     c                   eval      w_key1 = '010'
011000080625     c
011100080625      * tipo incasso bancario  non export
011200011106     c                   when      fca(xx) = 'B' and ita(yy) <> 'E'
011300011030     c                   eval      w_key1 = '002'
011400011030      * tipo incasso circolare non export
011500011106     c                   when      fca(xx) = 'C' and ita(yy) <> 'E'
011600011030     c                   eval      w_key1 = '003'
011700080625      * tipo incasso contanti  non export
011800011106     c                   when      fca(xx) = 'S' and ita(yy) <> 'E'
011900011031     c                   eval      w_key1 = '004'
012000080625     c
012100080625      * tipo incasso bancario      export
012200011106     c                   when      fca(xx) = 'B' and ita(yy) = 'E'
012300011030     c                   eval      w_key1 = '005'
012400080625      * tipo incasso circolare     export
012500011106     c                   when      fca(xx) = 'C' and ita(yy) = 'E'
012600011030     c                   eval      w_key1 = '006'
012700080625      * tipo incasso contanti      export
012800011106     c                   when      fca(xx) = 'S' and ita(yy) = 'E'
012900011030     c                   eval      w_key1 = '007'
013000080625     c
013100080625      * tipo incasso contanti  non export  secondo limite
013200080625     c                   when      fca(xx) = '2' and ita(yy) <> 'E'
013300080625     c                   eval      w_key1 = '015'
013400080625      * tipo incasso contanti     export  secondo limite
013500080625     c                   when      fca(xx) = '2' and ita(yy) =  'E'
013600080625     c                   eval      w_key1 = '016'
013700011030
013800011113     c                   other
013900011113     c                   eval      w_key1 = '001'
014000011030     c                   endsl
014100011106     c                   endif
014200011030
014300011030      * aggancio la tabella LCA
014400020123     c     rifai         tag
014500011030     c                   clear                   dlca
014600011030     c                   clear                   tibs02ds
014700011031     c                   movel     'C'           t02mod
014800011029     c                   movel     knsif         t02sif
014900011030     c                   movel     'LCA'         t02cod
015000011031     c                   movel(p)  w_key1        t02ke1
015100011029     c                   call      'TIBS02R'
015200011030     c                   parm                    kpjba
015300011030     c                   parm                    tibs02ds
015400011030     c                   if        t02err = *blanks
015500011030     c                   movel     t02uni        dlca
015600020123      * se errore e la tabella cercata non era la standard rifaccio con la standard
015700020123     c                   else
015800020123     c                   if        t02err = 'E' and w_key1 <> '001'
015900020123     c                   eval      w_key1 = '001'
016000020123     c                   goto      rifai
016100020123     c                   endif
016200020123     c                   endif
016300011030
016400011031      * se la divisa dell'importo non è EURO converto l'importo in EURO
016500011030     c                   z-add     i21imp        w_importo
016600011030     c                   if        i21div <> 'EUR'
016700011030     c                   clear                   yeurcods
016800011030     c                   movel     i21div        yecdvi
016900011030     c                   z-add     i21imp        yecimi
017000011030     c                   movel     'EUR'         yecdvo
017100011030     c                   call      'YEURCO'
017200011030     c                   parm                    yeurcods
017300011203     c                   if        yecesi = *blanks
017400011030     c                   z-add     yecimo        w_importo
017500011203      * se l'importo da controllare non è a 0 e se dalla conversione mi è
017600011203      * tornato 0 devo forzare a 0,01
017700011203     c                   if        i21imp > 0 and yecimo = 0
017800011203     c                   z-add     0,01          w_importo
017900011203     c                   endif
018000011203     c                   endif
018100011030     c                   endif
018200011203
018300011030
018400011030     c                   z-add     §lcamn        o21lmn
018500011030     c                   z-add     §lcax1        o21lx1
018600011030     c                   z-add     §lcax2        o21lx2
018700011030     c                   z-add     §lcaco        o21lca
018800050301     c                   z-add     §lcab         o21lb
018900011030
019000011030      * controllo gli importi
019100011105     c                   if        w_importo < o21lmn
019200011030     c                   eval      o21fmn = '1'
019300011030     c                   endif
019400011030     c                   if        w_importo > o21lx1
019500011030     c                   eval      o21fx1 = '1'
019600011030     c                   endif
019700011030     c                   if        w_importo > o21lx2
019800011030     c                   eval      o21fx2 = '1'
019900011030     c                   endif
020000011030     c                   if        w_importo > o21lca
020100011030     c                   eval      o21fca = '1'
020200011030     c                   endif
020300050301     c
020400050301     c* Solo per C/A bancari intestati a bartolini non abilitati da sede
020500050301     c                   if        fca(XX)='B' and fmb(XX)='B' and
020600050301     c                             ttm(xx)=' '  and
020700050301     c                             w_importo > o21lb
020800050301     c                   eval      o21fb = '1'
020900050301     c                   endif
021000011029
021100011029     c                   endsr
021200011106      *----------------------------------------------------
021300011106      * Carico schiere
021400011106      *----------------------------------------------------
021500011106     c     caricatic     begsr
021600011106
021700011106     c                   eval      tblkut = 1
021800011106     c                   eval      tblcod = '1A'
021900011106
022000011106     c                   open      tabel00f
022100011106
022200011106     c                   clear                   xx
022300011106
022400011106     c     ktabel00      setll     tabel00f
022500011106     c                   do        *hival
022600011106     c     ktabel00      reade     tabel00f
022700011106     c                   if        %eof
022800011106     c                   leave
022900011106     c                   endif
023000011106     c                   if        tblflg <> *blanks
023100011106     c                   iter
023200011106     c                   endif
023300011106     c                   eval      ds1a = tbluni
023400011106     c                   eval      xx = xx + 1
023500011106     c                   if        tblkey = *blanks
023600011106     c                   eval      tblkey = '&'
023700011106     c                   endif
023800011106     c                   eval      tic(xx) = tblkey
023900080625     c* contante : 2 limite
024000080625     c                   if        §1afca='S'  and §1afs2='S'
024100080625     c                   eval      fca(xx) = '2'
024200080625     c                   else
024300080625     c                   eval      fca(xx) = §1afca
024400080625     c                   endif
024500050301     c                   eval      fmb(xx) = §1afmb
024600050301     c                   eval      ttm(xx) = §1attm
024700011106     c                   enddo
024800011106
024900011106     c                   close     tabel00f
025000011106
025100011106     c                   endsr
025200011106      *----------------------------------------------------
025300011106      * Carico schiere
025400011106      *----------------------------------------------------
025500011106     c     caricanzd     begsr
025600011106
025700011106     c                   eval      tblkut = 1
025800011106     c                   eval      tblcod = '15'
025900011106
026000011106     c                   open      tabel00f
026100011106
026200011106     c                   clear                   xx
026300011106
026400011106     c     ktabel00      setll     tabel00f
026500011106     c                   do        *hival
026600011106     c     ktabel00      reade     tabel00f
026700011106     c                   if        %eof
026800011106     c                   leave
026900011106     c                   endif
027000011106     c                   if        tblflg <> *blanks
027100011106     c                   iter
027200011106     c                   endif
027300011106     c                   eval      ds15 = tbluni
027400011106     c                   eval      xx = xx + 1
027500011106     c                   if        tblkey = *blanks
027600011106     c                   eval      tblkey = '&'
027700011106     c                   endif
027800011106     c                   eval      nzd(xx) = tblkey
027900011106     c                   if        §15ita = 'I'
028000011106     c                   eval      ita(xx) = 'I'
028100011106     c                   else
028200011106     c                   eval      ita(xx) = 'E'
028300011106     c                   endif
028400011106     c                   enddo
028500011106
028600011106     c                   close     tabel00f
028700011106
028800011106     c                   endsr
