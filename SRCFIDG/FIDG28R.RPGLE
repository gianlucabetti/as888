000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070326     H*
000300070327     h* Calc.x distinta tot.pes/vol/bolle assegnati da CML
000400000000     H*--------------------------------------------------------------*
000500070226     Ffidst03l  if   e           k disk
000600070226     Ffiarg03l  if   e           k disk
000700070228     Ffiarg01l  if   e           k disk    rename(fiarg000:fiarg1)
000800070226     Ffnarb01l  if   e           k disk
000900070226     Ffnarb70l  if   e           k disk    rename(fnarb000:fnarb70)
001000070226     Fazorg01l  if   e           k disk
001100950125     D*
001200070302     D kpjba         E DS
001300070302     D fidg28ds      E DS
001400070302     D fidg27ds      E DS
001500070302     D fnarbds       E DS                  extname(fnarb00f)
001600070226     D og146         E DS
001700070227     d como70          s             70
001800070227     d como70b         s             70
001900070227     d fer             s             70    dim(500)
002000070227     d x               s              3  0
002100070227     d como22          s             22
002200070227     d como22b         s             22
002300070227     d cor             s             22    dim(500)
002400070227     d y               s              3  0
002500070424     d dataiso         s               d   datfmt(*iso)
002600070424     d dataeur         s               d   datfmt(*eur)
002700070213      *___________________________________________________
002800070213     C     *entry        plist
002900070226     C                   parm                    fidg28ds
003000070226     c                   z-add     d28nfvi       nfvk
003100070424      *
003200070424     C                   time                    w0120            14 0
003300070424     C                   move      w0120         wdat              8 0
003400070424     C                   movel     w0120         wora              6 0
003500070424     c                   move      wdat          dataeur
003600070424     c                   move      dataeur       dataiso
003700070424     c                   move      dataiso       wdymd             8 0
003800070213
003900070226     c     d28fgsi       chain     azorg01l
004000070226     c                   if        %found(azorg01l)
004100070226     c                   movel     orgde6        og146
004200070226     c                   if        §ogdda = ' '
004300070226     c                   move      '1'           d28erro
004400070226     c                   goto      fine
004500070424     c                   else
004600070424     c                   move      §ogdda        ogddaN            8 0
004700070226     c                   endif
004800070226      * leggo rekord di DST per assumere i valori MASSIMI
004900070226     C     kdst          chain     fidst03l
005000070226     c                   if        %found(fidst03l)
005100070226     c                   z-add     DSTpkl        D28pklo
005200070226     c                   z-add     DSTvlu        D28vluo
005300070227     c                   z-add     DSTnftl       D28nftl
005400070227     c                   z-add     DSTnbl        D28nblo
005500070226      * se è fase di test elabora i dati da ARG per agganciare le
005600070226      * bolle della distinta altrimenti legge direttamente ARB
005700070424     c                   if        §ogdda = '20391231' or
005800070424     c                             wdymd  < ogddaN
005900070226     c                   exsr      elabARG
006000070226     c                   else
006100070226     c                   exsr      elabARB
006200070226     c                   endif
006300070227      * imposto numero fermate
006400070227     c                   z-add     1             x
006500070227     c     como70b       lookup    fer(x)                                 80
006600070227     c   80              eval      d28stpao = x - 1
006700070228      * imposto numero fermate
006800070228     c                   z-add     1             x
006900070228     c     como22b       lookup    cor(x)                                 80
007000070228     c   80              eval      d28stgeo = x - 1
007100070226      *non trovato DST
007200070226     c                   else
007300070226     c                   move      '1'           d28erro
007400070226     c                   goto      fine
007500070226     c                   endif
007600070226
007700070226     c                   endif
007800070226
007900070226     c     fine          tag
008000070226     c                   seton                                        lr
008100070213      *___________________________________________________
008200070226     C     elabARG       begsr
008300070213      *___________________________________________________
008400070226     c     karg          setll     fiarg03l
008500070226     c                   do        *hival
008600070226     c     karg          reade     fiarg03l
008700070226     c                   if        %eof(fiarg03l)
008800070226     c                   leave
008900070226     c                   endif
009000070227
009100070226     c     karb1         chain     fnarb01l
009200070226     c                   if        %found(fnarb01l)
009300070228      *memorizza fermate da ragione sociale/indirizzo
009400070227     c                   exsr      fermate
009500070228      *memorizza fermate GEOstazionarie
009600070228     c                   exsr      fermateGEO
009700070302      * richiamo funzione per calcolo pesi e volumi
009800070302     c                   exsr      calpesvol
009900070226      *bolle
010000070226     c                   add       1             D28nblao
010100070226     c
010200070226     c                   endif
010300070226     c                   enddo
010400070213      *
010500070213    1C                   endsr
010600070226      *___________________________________________________
010700070226     C     elabARB       begsr
010800070226      *___________________________________________________
010900070226     c     karb70        setll     fnarb70l
011000070226     c                   do        *hival
011100070226     c     karb70        reade     fnarb70l
011200070226     c                   if        %eof(fnarb70l)
011300070226     c                   leave
011400070226     c                   endif
011500070228      * aggancio ARG
011600070228     c     karg1         chain     fiarg01l
011700070228     c                   if        %found(fiarg01l)
011800070228      *memorizza fermate da ragione sociale/indirizzo
011900070227     c                   exsr      fermate
012000070228      *memorizza fermate GEOstazionarie
012100070228     c                   exsr      fermateGEO
012200070302      * richiamo funzione per calcolo pesi e volumi
012300070302     c                   exsr      calpesvol
012400070226      *bolle
012500070226     c                   add       1             D28nblao
012600070226     c
012700070228     c                   endif
012800070226     c                   enddo
012900070226      *
013000070226    1C                   endsr
013100070227      *___________________________________________________
013200070227     C     fermate       begsr
013300070227      *___________________________________________________
013400070227     c                   z-add     1             x
013500070227     c                   movel     arbrsd        como70
013600070227     c                   move      arbind        como70
013700070227     c     como70        lookup    fer                                    80
013800070227     c                   if        not *in80
013900070227     c     como70b       lookup    fer(x)                                 80
014000070227     c   80              movel     como70        fer(x)
014100070227     c                   endif
014200070227
014300070227    1C                   endsr
014400070228      *_____________________________________________________
014500070228     c     fermateGEO    begsr
014600070228      *_____________________________________________________
014700070228      * raggruppa fermata per latitudine longitudine
014800070228     c                   z-add     1             y
014900070228     c                   movel     argxlo        como22
015000070228     c                   move      argyla        como22
015100070228     c     como22        lookup    cor                                    81
015200070228     c                   if        not *in81
015300070228     c     como22b       lookup    cor(y)                                 81
015400070228     c   81              movel     como22        cor(y)
015500070228     c                   endif
015600070228      *
015700070228     c                   endsr
015800070226      *_______________________________________________
015900070302      *  Determinazione dei pesi e volumi da utilizzare
016000070226      *_______________________________________________
016100070302     C     calpesvol     begsr
016200070302     c                   clear                   fidg27ds
016300070302     c                   move      'R'           d27telai
016400070323     c                   z-add     d28dfvi       d27dfvi
016500070302     c                   movel     fidg27ds      kpjbu
016600070302     c                   call      'FIDG27R'
016700070302     c                   parm                    kpjba
016800070302     c                   parm                    fnarbds
016900070302     c
017000070302     c                   movel     kpjbu         fidg27ds
017100070302     c                   add       D27PKLDCO     D28pklao
017200070302     c                   add       D27VLUDCO     D28vluao
017300070226     C*
017400070226     C                   endsr
017500070226     c*-------------------------------------------------
017600070226     c     *inzsr        begsr
017700070226     c*-------------------------------------------------
017800070226     c     kdst          klist
017900070226     c                   kfld                    d28fgsi
018000070226     c                   kfld                    catk              1 0
018100070226     c                   kfld                    d28nfvi
018200070226     c                   z-add     4             catk
018300070226
018400070226     c     karb1         klist
018500070226     c                   kfld                    argaas
018600070226     c                   kfld                    arglnp
018700070226     c                   kfld                    argnrs
018800070226     c                   kfld                    argnsp
018900070226
019000070226     c     karg          klist
019100070226     c                   kfld                    d28fgsi
019200070226     c                   kfld                    nfvk              7 0
019300070226     c                   kfld                    d28dfvi
019400070228
019500070228     c     karg1         klist
019600070228     c                   kfld                    arbaas
019700070228     c                   kfld                    arblnp
019800070228     c                   kfld                    arbnrs
019900070228     c                   kfld                    arbnsp
020000070228
020100070226     c     karb70        klist
020200070226     c                   kfld                    ifpk
020300070226     c                   kfld                    nfvk
020400070226     c                   kfld                    d28dfvi
020500070226     c                   z-add     d28fgsi       ifpk              9 0
020600070226
020700070226     c                   endsr
