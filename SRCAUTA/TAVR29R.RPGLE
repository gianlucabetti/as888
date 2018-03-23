000100111107     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200111107      *------------------------------------------------------------------------*
000300111107      *
000400111107      *  Scrittura del file aumento fuel
000500111107      *
000600111107      *------------------------------------------------------------------------*
000700111107
000800111107     ftntam04l  if   e           k disk
000900111107     ftitpd01l  if   e           k disk
001000111108     fwfuni00f  if   e             disk
001100111108     fwffue00f  o    e             disk
001200111107
001300111107      *------------------------------------------------------------------------*
001400111107      *  RIEPILOGO INDICATORI
001500111107      *------------------------------------------------------------------------*
001600111107      * 02 - ho scritto almeno 1 rcd
001700111107      * 30 - comodo
001800111107
001900111107      *------------------------------------------------------------------------*
002000111107      *   V A R I A B I L I
002100111107      *------------------------------------------------------------------------*
002200111107     d kksc            s                   like(tamksc)
002300111107     d kftc            s                   like(tpdftc)
002400111107     d wctr            s                   like(tamctr)
002500111107     d wdata           s              8  0
002600111107     d wksc            s                   like(tamksc)
002700111107     d wora            s              6  0
002800111107     d w0030           s              3  0
002900111107     d w0140           s             14  0
003000111108     d contatore       s              5  0
003100111108     D dat_ultima      S              8  0
003200111109     D x               S              5  0
003300111108
003400111108     d $Scrivi_ok      s               n   inz(*off)
003500111108     d $Itr_0          s               n   inz(*off)
003600111109     d $Itr_5          s               n   inz(*off)
003700111107
003800111107      *------------------------------------------------------------------------*
003900111107      *   S C H I E R E
004000111107      *------------------------------------------------------------------------*
004100111107
004200111107      *------------------------------------------------------------------------*
004300111107      *   D S   I N T E R N E / E S T E R N E
004400111107      *------------------------------------------------------------------------*
004500111107
004600111107     d wlbdat          ds                  inz
004700111107     d  g02dat                 1      8  0
004800111107     d  g02inv                 9     16  0
004900111107     d  g02err                17     17
005000111107     d  g02tgi                18     22  0
005100111107
005200111108     d SKC07           S              7  0 DIM(500)
005300111108
005400111107     d kpjba         e ds
005500111107     d dsta01        e ds
005600111107     d tibs10ds      e ds
005700111108     D   skc11                21   5520  0 DIM(500)
005800111107
005900111107      *------------------------------------------------------------------------*
006000111107
006100111108     c                   eval      dat_ultima  = 20111031
006200111108
006300111108     c                   do        *hival
006400111108      *
006500111108     c                   read      WFUNI00F
006600111108      *
006700111108     c                   if        %eof(WFUNI00F)
006800111108     c                   leave
006900111108     c                   endif
007000111108      *
007100111108      * cerco i clienti figli del cliente unificante attraverso il tibs10
007200111108      *
007300111108     c                   clear                   tibs10ds
007400111108     c                   eval      d10tle = 'ST'
007500111108     c                   eval      d10paf = 'F'
007600111108     c                   z-add     wfuclu        d10cod
007700111108     c                   z-add     dat_ultima    d10drf
007800111108     c                   CALL      'TIBS10R'
007900111108     c                   parm                    tibs10ds
008000111108     c                   parm      'A'           partpexe          1
008100111108      * se non ci sono errori carico la schiera dei figli che contiene il codice del cliente padre
008200111108     c                   if        d10err = ' '
008300111108      * muovo in una schiera di 7
008400111108     c                   z-add     skc11         skc07
008500111108      *
008600111108      * elaboro tutti i clienti figli
008700111108      *
008800111108     c                   exsr      SR_FIGLI
008900111109     c                   else
009000111109     c
009100111109     c                   eval      Kksc = wfuclu
009200111109     c                   exsr      SR_TARIFFE
009300111109     c                   endif
009400111109      *
009500111109     c                   enddo
009600111107
009700111107     c                   eval      *inlr = *on
009800111108      *---------------------------------------------------------------------------------------------
009900111108      * SR_FIGLI Elaborazione di ogni codice figlio legato all'unificante
010000111108      *---------------------------------------------------------------------------------------------
010100111108     C     SR_FIGLI      BEGSR
010200111108      * per ogni figlio cerco le tariffe
010300111108      *
010400111108     c                   do        500           x
010500111108      * al primo cliente uguale a zero   esco dal ciclo
010600111108     c                   if        skc07(x) = *zeros
010700111108     c                   leave
010800111108     c                   endif
010900111108
011000111108      *
011100111109     c                   eval      Kksc = skc07(x)
011200111108
011300111108     c                   Exsr      Sr_tariffe
011400111108      *
011500111108     c                   enddo
011600111108      *
011700111108     c                   endsr
011800111108
011900111108      *---------------------------------------------------------------------------------------------
012000111108      * SR_Tariffe  Controllo e scrive tutte le tariffe legate al figlio
012100111108      *---------------------------------------------------------------------------------------------
012200111108     C     SR_Tariffe    BEGSR
012300111108      *
012400111108     c                   eval      wctr = 999
012500111108
012600111108     c     kksc          setll     tntam04l
012700111108    1c                   do        *hival
012800111109     c     Kksc          reade     tntam04l
012900111108
013000111108     c                   eval      $scrivi_ok = *off
013100111108      * fine file
013200111108     c                   if        %eof(tntam04l)
013300111108     c                   leave
013400111108     c                   endif
013500111108
013600111108      * cambio tariffa
013700111108    2c                   if        tamctr <> wctr or tamksc <> wksc
013800111108
013900111108     c                   movel     tamflo        dsta01
014000111108
014100111108      * escludo le annullate
014200111108     c                   if        tamatb <> *blanks
014300111108     c                   iter
014400111108     c                   endif
014500111108      * escludo le tariffe fedex
014600111108     c                   if        §tafed = 'S'
014700111108     c                   iter
014800111108     c                   endif
014900111108
015000111108      * controllo se Fuel presente
015100111108     c                   eval      kftc = 'f '
015200111108     c                   eval      contatore = 0
015300111108     c                   eval      $scrivi_ok = *on
015400111109     c                   eval      $itr_0 = *off
015500111109     c                   eval      $itr_5 = *off
015600111108      * cerco importo percentuale
015700111108     c     ktpt          setll     titpd01l
015800111108    3c                   do        *hival
015900111108     c     ktpt          reade     titpd01l
016000111108      * fine file
016100111108     c                   if        %eof(titpd01l)
016200111108     c                   leave
016300111108     c                   endif
016400111108      * non annullate
016500111108     c                   if        tpdatb <> *blanks
016600111108     c                   iter
016700111108     c                   endif
016800111108      * scaglioni
016900111108     c                   add       1             contatore
017000111108
017100111108      * se il primo fattore demoltiplicatore è uguale a zero non prendo in
017200111108      * considerazione la tariffa
017300111108     c                   if        contatore = 1   and
017400111108     c                             tpditr = 0
017500111108     c                   eval      $itr_0 = *on
017600111108     c                   leave
017700111108     c                   endif
017800111108      * se tutti i fattori demoltiplicatori sono uguali o maggiori 0,5 non prendo in
017900111108      * considerazione la tariffa
018000111108
018100111108     c                   if        tpditr >= 0,5
018200111108     c                   eval      $itr_5 = *on
018300111108     c                   iter
018400111108     c                   else
018500111108     c                   eval      $itr_5 = *off
018600111108     c                   leave
018700111108     c                   endif
018800111108
018900111108    3c                   enddo
019000111108      * scrivo il file
019100111108     c                   exsr      scrivi
019200111108
019300111108     c                   eval      $scrivi_ok = *off
019400111108
019500111108     c                   eval      wksc = tamksc
019600111108     c                   eval      wctr = tamctr
019700111108
019800111108      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
019900111108      * ma devo elaborare anche la tariffa precedente che è in vigore ora
020000111108     c                   if        tamddt > *date
020100111108     c                   eval      wctr = 999
020200111108     c                   endif
020300111108    2c                   endif
020400111108
020500111108    1c                   enddo
020600111108
020700111108    1c                   endsr
020800111108
020900111108      *------------------------------------------------------------------------*
021000111108      * SCRIVO IL FILE
021100111108      *------------------------------------------------------------------------*
021200111108     c     scrivi        begsr
021300111108
021400111108
021500111108     c                   clear                   wffue000
021600111108
021700111108      * cliente
021800111108     c                   eval      wffclu  = wfuclu
021900111108     c                   eval      wffksc  = tamksc
022000111108      * tariffa
022100111108     c                   eval      wffctrv = tamctr
022200111108     c                   eval      wffprgv = tamprg
022300111108     c                   eval      wffddtv = tamddt
022400111108     c                   eval      wffdstv = tamdst
022500111108      *
022600111108      * se contatore a zero nessuna tariffa
022700111108     c                   If        contatore = 0
022800111108     c                   eval      wffnofu = 'S'
022900111108     c                   endif
023000111108      * particolarità tariffa
023100111108     c                   If        $itr_0 = *on
023200111108     c                   eval      wffitr0 = 'S'
023300111108     c                   endif
023400111108      * particolarità tariffa
023500111108     c                   If        $itr_5 = *on
023600111108     c                   eval      wffmagg = 'S'
023700111108     c                   endif
023800111108      * flag aumenta tariffa
023900111108     c                   if        wffnofu = ' ' and wffitr0 = ' ' and
024000111108     c                             wffmagg = ' '
024100111108     c                   eval      wffaum = 'S'
024200111108     c                   endif
024300111108      * scrivo file
024400111108     c                   write     wffue000
024500111108
024600111108
024700111108     c                   endsr
024800111108
024900111107      *--------------------------------------------------------------*
025000111107      * ROUTINE INIZIALE
025100111107      *--------------------------------------------------------------*
025200111107     c     *inzsr        begsr
025300111107
025400111107     c     *entry        plist
025500111107     c                   parm                    kpjba
025600111107
025700111107      * reperisco data e ora del lancio
025800111107
025900111107     c     ktpt          klist
026000111107     c                   kfld                    tamksc
026100111107     c                   kfld                    tamctr
026200111107     c                   kfld                    tamprg
026300111107     c                   kfld                    kftc
026400111107
026500111107     c                   endsr
