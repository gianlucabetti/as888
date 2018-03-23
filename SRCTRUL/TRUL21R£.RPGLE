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
009900110928      ***
010000020123      * servizio poste bancario
010100110928     c***                when      i21tsp = 'P' and fca(xx) = 'B'
010200110928     c***                eval      w_key1 = '008'
010300020123      * servizio poste circolare
010400110928     c***                when      i21tsp = 'P' and fca(xx) = 'C'
010500110928     c***                eval      w_key1 = '009'
010600020123      * servizio poste contanti
010700110928     c***                when      i21tsp = 'P' and (fca(xx) = 'S'  or
010800110928     c***                                            fca(XX) = '2')
010900110928     c***                eval      w_key1 = '010'
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
013100110928      * tipo incasso contanti  non export  2°   limite
013200080625     c                   when      fca(xx) = '2' and ita(yy) <> 'E'
013300080625     c                   eval      w_key1 = '015'
013400110928      * tipo incasso contanti     export   2°   limite
013500080625     c                   when      fca(xx) = '2' and ita(yy) =  'E'
013600080625     c                   eval      w_key1 = '016'
013700011030
013800110928      * tipo incasso contanti  non export  3°   limite
013900110928     c                   when      fca(xx) = '3' and ita(yy) <> 'E'
014000110928     c                   eval      w_key1 = '017'
014100110928      * tipo incasso contanti     export   3°   limite
014200110928     c                   when      fca(xx) = '3' and ita(yy) =  'E'
014300110928     c                   eval      w_key1 = '018'
014400110928
014500011113     c                   other
014600011113     c                   eval      w_key1 = '001'
014700011030     c                   endsl
014800011106     c                   endif
014900011030
015000120413     c                   clear                   tentav            2 0
015100011030      * aggancio la tabella LCA
015200020123     c     rifai         tag
015300120413     c                   add       1             tentav
015400120413     c
015500011030     c                   clear                   dlca
015600011030     c                   clear                   tibs02ds
015700011031     c                   movel     'C'           t02mod
015800011029     c                   movel     knsif         t02sif
015900011030     c                   movel     'LCA'         t02cod
016000011031     c                   movel(p)  w_key1        t02ke1
016100120413     c
016200120413     c* Al primo tentativo , provo con la linea di arrivo
016300120413     c                   if        tentav=1
016400120413     c                   movel(p)  i21lna        t02ke2
016500120413     c                   endif
016600120413     c
016700011029     c                   call      'TIBS02R'
016800011030     c                   parm                    kpjba
016900011030     c                   parm                    tibs02ds
017000120413     c
017100011030     c                   if        t02err = *blanks
017200011030     c                   movel     t02uni        dlca
017300020123     c                   else
017400120413     c
017500120413     c* se errore e la tabella cercata era con linea di arrivo, ricerco senza linea di arrivo
017600120413     c                   if        i21lna>0 and tentav=1
017700120413     c                   goto      rifai
017800120413     c                   endif
017900120413     c
018000120413      * se errore e la tabella cercata non era la standard rifaccio con la standard
018100020123     c                   if        t02err = 'E' and w_key1 <> '001'
018200020123     c                   eval      w_key1 = '001'
018300020123     c                   goto      rifai
018400020123     c                   endif
018500020123     c                   endif
018600011030
018700011031      * se la divisa dell'importo non è EURO converto l'importo in EURO
018800011030     c                   z-add     i21imp        w_importo
018900011030     c                   if        i21div <> 'EUR'
019000011030     c                   clear                   yeurcods
019100011030     c                   movel     i21div        yecdvi
019200011030     c                   z-add     i21imp        yecimi
019300011030     c                   movel     'EUR'         yecdvo
019400011030     c                   call      'YEURCO'
019500011030     c                   parm                    yeurcods
019600011203     c                   if        yecesi = *blanks
019700011030     c                   z-add     yecimo        w_importo
019800011203      * se l'importo da controllare non è a 0 e se dalla conversione mi è
019900011203      * tornato 0 devo forzare a 0,01
020000011203     c                   if        i21imp > 0 and yecimo = 0
020100011203     c                   z-add     0,01          w_importo
020200011203     c                   endif
020300011203     c                   endif
020400011030     c                   endif
020500011203
020600011030
020700011030     c                   z-add     §lcamn        o21lmn
020800011030     c                   z-add     §lcax1        o21lx1
020900011030     c                   z-add     §lcax2        o21lx2
021000011030     c                   z-add     §lcaco        o21lca
021100050301     c                   z-add     §lcab         o21lb
021200011030
021300011030      * controllo gli importi
021400011105     c                   if        w_importo < o21lmn
021500011030     c                   eval      o21fmn = '1'
021600011030     c                   endif
021700011030     c                   if        w_importo > o21lx1
021800011030     c                   eval      o21fx1 = '1'
021900011030     c                   endif
022000011030     c                   if        w_importo > o21lx2
022100011030     c                   eval      o21fx2 = '1'
022200011030     c                   endif
022300011030     c                   if        w_importo > o21lca
022400011030     c                   eval      o21fca = '1'
022500011030     c                   endif
022600050301     c
022700050301     c* Solo per C/A bancari intestati a bartolini non abilitati da sede
022800050301     c                   if        fca(XX)='B' and fmb(XX)='B' and
022900050301     c                             ttm(xx)=' '  and
023000050301     c                             w_importo > o21lb
023100050301     c                   eval      o21fb = '1'
023200050301     c                   endif
023300011029
023400011029     c                   endsr
023500011106      *----------------------------------------------------
023600011106      * Carico schiere
023700011106      *----------------------------------------------------
023800011106     c     caricatic     begsr
023900011106
024000011106     c                   eval      tblkut = 1
024100011106     c                   eval      tblcod = '1A'
024200011106
024300011106     c                   open      tabel00f
024400011106
024500011106     c                   clear                   xx
024600011106
024700011106     c     ktabel00      setll     tabel00f
024800011106     c                   do        *hival
024900011106     c     ktabel00      reade     tabel00f
025000011106     c                   if        %eof
025100011106     c                   leave
025200011106     c                   endif
025300011106     c                   if        tblflg <> *blanks
025400011106     c                   iter
025500011106     c                   endif
025600011106     c                   eval      ds1a = tbluni
025700011106     c                   eval      xx = xx + 1
025800011106     c                   if        tblkey = *blanks
025900011106     c                   eval      tblkey = '&'
026000011106     c                   endif
026100011106     c                   eval      tic(xx) = tblkey
026200110928     c                   select
026300110928     c* contante : 2 limite
026400110928     c                   when      §1afca='S'  and §1afs2='S'
026500080625     c                   eval      fca(xx) = '2'
026600110928     c* contante : 3 limite
026700110928     c                   when      §1afca='S'  and §1afs2='3'
026800110928     c                   eval      fca(xx) = '3'
026900110928     c                   other
027000080625     c                   eval      fca(xx) = §1afca
027100110928     c                   endsl
027200110928     c
027300050301     c                   eval      fmb(xx) = §1afmb
027400050301     c                   eval      ttm(xx) = §1attm
027500011106     c                   enddo
027600011106
027700011106     c                   close     tabel00f
027800011106
027900011106     c                   endsr
028000011106      *----------------------------------------------------
028100011106      * Carico schiere
028200011106      *----------------------------------------------------
028300011106     c     caricanzd     begsr
028400011106
028500011106     c                   eval      tblkut = 1
028600011106     c                   eval      tblcod = '15'
028700011106
028800011106     c                   open      tabel00f
028900011106
029000011106     c                   clear                   xx
029100011106
029200011106     c     ktabel00      setll     tabel00f
029300011106     c                   do        *hival
029400011106     c     ktabel00      reade     tabel00f
029500011106     c                   if        %eof
029600011106     c                   leave
029700011106     c                   endif
029800011106     c                   if        tblflg <> *blanks
029900011106     c                   iter
030000011106     c                   endif
030100011106     c                   eval      ds15 = tbluni
030200011106     c                   eval      xx = xx + 1
030300011106     c                   if        tblkey = *blanks
030400011106     c                   eval      tblkey = '&'
030500011106     c                   endif
030600011106     c                   eval      nzd(xx) = tblkey
030700011106     c                   if        §15ita = 'I'
030800011106     c                   eval      ita(xx) = 'I'
030900011106     c                   else
031000011106     c                   eval      ita(xx) = 'E'
031100011106     c                   endif
031200011106     c                   enddo
031300011106
031400011106     c                   close     tabel00f
031500011106
031600011106     c                   endsr
