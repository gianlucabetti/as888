000100041025     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200041020      *----------------------------------------------------*
000300060327      *
  Statistiche Tipo invio Fatture      ?
000400041020      *
000500041020      *----------------------------------------------------*
000600060329     ftiift01l  if   e           k disk
000700060327     ftiift11l  uf a e           k disk
000800060329
000900041020      *----------------------------------------------------*
001000041020      *  RIEPILOGO INDICATORI
001100041020      *----------------------------------------------------*
001200041020
001300041020      *   S C H I E R E
001400041020
001500060329     d kfile           s              9    dim(18000)                           file (key)
001600060329     d                                     varying
001700060329     d                                     inz
001800060705     d rfile           s            135    dim(18000)                           file (record)
001900060329     d                                     varying
002000060329     d                                     inz
002100041022
002200041022      *   V A R I A B I L I
002300041022
002400060329     d xk              s              5i 0 inz(*zeros)                          indice sk key
002500060330     d xf              s              5i 0 inz(*zeros)                          indice sk lookup
002600060330     d xw              s              5i 0 inz(*zeros)                          indice sk work
002700041026
002800060330     d keyfil          s                   like(iftpop)
002900060330     d sav_fil         s                   like(iftpop)
003000060330     d Keydta          s              6  0
003100060330     d sav_dta         s              6  0
003200060330
003300041020      *   D S   I N T E R N E / E S T E R N E
003400041020
003500060329
003600060329     d dskfile         ds                  inz
003700060329     d  dspop                              Like(f_iftpop)
003800060329     d  dsann                              like(f_iftann)
003900060329     d  dsmes                              like(f_iftmes)
004000060329
004100041020
004200060329     d dstiift       e ds                  extname(tiift10f)
004300060329     d                                     prefix(f_)
004400060329     d                                     inz
004500041022
004600060330     d dstiift10     e ds                  extname(tiift10f)
004700060330     d                                     inz
004800060330
004900041022     d Kpjba         e ds
005000041022
005100041227
005200041022      *---------------------------------------------------------------*
005300041022      *            Ciclo principale Statistica                        *
005400041022      *---------------------------------------------------------------*
005500060330
005600060330     c     *loval        Setll     Tiift01l
005700060330     c                   Do        *Hival
005800060330     c                   Read      Tiift01l
005900060330      * Fine file
006000060330 b1  c                   If        %Eof(Tiift01l)
006100060331      * se ho valorizzato l'indice work scrivo il record nella schiera dei totali
006200060331     c                   If        xw > 0
006300060331     c                   eval      rfile(xw) = dstiift
006400060331     c                   endif
006500060331
006600060330     c                   Leave
006700060330     c                   EndIf
006800060330
006900060330     c                   movel(p)  iftcbo        keyfil
007000060330     c                   movel(p)  iftdsp        Keydta
007100060330
007200060330      * verifico se stesso P.O. che ho salvato
007300060330 b2  c                   If        Sav_fil <> keyfil
007400060330      * carico il record in schiera con chiave
007500060330     c                   exsr      Carsch
007600060330     c                   eval      Sav_fil = keyfil
007700060330     c                   eval      Sav_dta = keydta
007800060330      * se stesso P.O. ma date diverse
007900060330     c                   else
008000060330      * verifico se stesso anno mese
008100060330     c                   If        Sav_dta <> Keydta
008200060330      * carico il record in schiera con chiave
008300060330     c                   exsr      Carsch
008400060330     c                   eval      Sav_dta = keydta
008500060330     c                   endif
008600060330     c                   endif
008700060330
008800060330     c                   Exsr      Elabora
008900060330
009000060330 b1  c                   EndDo
009100060330
009200060330      * fine lettura scrivo il file dei totali
009300060330
009400060330     c                   Exsr      Scrivo
009500060330
009600060330     c                   Seton                                        lr
009700060330      *---------------------------------------------------------------*
009800060330      *            Carico la schiera                                  *
009900060330      *---------------------------------------------------------------*
010000060330     c     Carsch        Begsr
010100060330
010200060330      * se ho valorizzato l'indice work scrivo il record nella schiera dei totali
010300060330     c                   If        xw > 0
010400060330     c                   eval      rfile(xw) = dstiift
010500060330     c                   endif
010600060330
010700060330     c                   movel(p)  keyfil        dspop
010800060330     c                   movel(p)  Keydta        dsann
010900060330     c                   move (p)  Keydta        dsmes
011000060330
011100060330     c                   z-add     1             xf
011200060330     c     dskfile       lookup    kfile(xf)                              30
011300060330     c                   if        %found
011400060330     c                   eval      dstiift = rfile(xf)
011500060330     c                   eval      xw = xf
011600060330
011700060330     c                   else
011800060330
011900060330     c                   add       1             xk
012000060330
012100060330     c                   eval      kfile(xk) = dskfile
012200060330      * pulisco la schiera
012300060330     c                   clear                   dstiift
012400060330      * imposto alcuni campi
012500060330     c                   eval      f_iftpop = dspop
012600060330     c                   eval      f_iftann = dsann
012700060330     c                   eval      f_iftmes = dsmes
012800060330
012900060330     c                   eval      xw = xk
013000060330     c                   endif
013100060330
013200060330     c                   endsr
013300060330      *---------------------------------------------------------------*
013400060330      *            Elaboro il dettaglio                               *
013500060330      *---------------------------------------------------------------*
013600060330     c     Elabora       Begsr
013700060330
013800060330      * verifico se distinta o fattura
013900060330     c                   If        ifttft = '0'
014000060330      * sommo fatture emesse
014100060330     c                   add       1             f_iftfat
014200060330     c                   else
014300060330      * sommo distinte emesse
014400060330     c                   add       1             f_iftdit
014500060330     c                   endif
014600060330
014700060330      * verifico tipo invio se si parla di fattura
014800060330
014900060330      * carta
015000060330     c                   If        ifttft = '0' and  ifttif = 'C'
015100060330     c                   add       1             f_iftcat
015200060330
015300060330      * verifico spese invio fattura
015400060330     c                   if        iftsif > 0
015500060330     c                   add       1             f_iftcad
015600060330     c                   add       iftsif        f_iftcai
015700060330
015800060330     c                   add       1             f_iftfad
015900060330     c                   add       iftsif        f_iftfai
016000060330     c                   endif
016100060330
016200060330     c                   endif
016300060330
016400060330      * postel
016500060330     c                   If        ifttft = '0' and  ifttif = 'P'
016600060330     c                   add       1             f_iftpot
016700060330
016800060330      * verifico spese invio fattura
016900060330     c                   if        iftsif > 0
017000060330     c                   add       1             f_iftpod
017100060330     c                   add       iftsif        f_iftpoi
017200060330
017300060330     c                   add       1             f_iftfad
017400060330     c                   add       iftsif        f_iftfai
017500060330     c                   endif
017600060330
017700060330     c                   endif
017800060330
017900060330      * mail
018000060330     c                   If        ifttft = '0' and  ifttif = 'M'
018100060330     c                   add       1             f_iftmat
018200060330
018300060330      * verifico spese invio fattura
018400060330     c                   if        iftsif > 0
018500060330     c                   add       1             f_iftmad
018600060330     c                   add       iftsif        f_iftmai
018700060330
018800060330     c                   add       1             f_iftfad
018900060330     c                   add       iftsif        f_iftfai
019000060330     c                   endif
019100060330
019200060330     c                   endif
019300060330
019400060330      * verifico spese invio fattura nelle distinte
019500060330     c                   if        iftsif > 0  and ifttft = '1'
019600060330     c                   add       1             f_iftdid
019700060330     c                   add       iftsif        f_iftdii
019800060330     c                   endif
019900060330
020000060330     c                   endsr
020100060330
020200060330      *---------------------------------------------------------------*
020300060330      *            Scrivo il file dei totali                          *
020400060330      *---------------------------------------------------------------*
020500060330     c     Scrivo        Begsr
020600060330
020700060330      * per ogni elemento della schiera veridifico se esiste già il record su Tiift10f
020800060330
020900060330     c                   z-add     1             xk
021000060330
021100060330     c                   dow       xk < 18000
021200060330
021300060330     c                   If        kfile(xk) = *zeros
021400060330     c                   leave
021500060330     c                   endif
021600060330
021700060330     c                   movel     kfile(xk)     dskfile
021800060330
021900060330     c     keyift        chain     Tiift11l
022000060330
022100060330     c                   movel     rfile(xk)     dstiift
022200060330     c                   eval      dstiift10 = dstiift
022300060330
022400060330     c                   if        %found(tiift11l)
022500060330     c                   update    tiift100
022600060330     c                   else
022700060330     c                   write     tiift100
022800060330     c                   endif
022900060330
023000060330     c                   add       1             xk
023100060330
023200060330     c                   enddo
023300060330
023400060330     c                   endsr
023500060330      *--------------------------------------------------------------*
023600060330      * ROUTINE INIZIALE
023700060330      *--------------------------------------------------------------*
023800060330     c     *inzsr        begsr
023900060330      *
024000060330     c     *entry        plist
024100060330     c                   parm                    kpjba
024200060330
024300060330      * Key TIIFT11L
024400060330     c     keyift        klist
024500060330     c                   kfld                    dspop
024600060330     c                   kfld                    dsann
024700060330     c                   kfld                    dsmes
024800060330
024900060330     c                   endsr
