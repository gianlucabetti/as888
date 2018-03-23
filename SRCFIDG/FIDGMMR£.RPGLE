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
002500070213      *___________________________________________________
002600070213     C     *entry        plist
002700070226     C                   parm                    fidg28ds
002800070226     c                   z-add     d28nfvi       nfvk
002900070213
003000070226     c     d28fgsi       chain     azorg01l
003100070226     c                   if        %found(azorg01l)
003200070226     c                   movel     orgde6        og146
003300070226     c                   if        §ogdda = ' '
003400070226     c                   move      '1'           d28erro
003500070226     c                   goto      fine
003600070226     c                   endif
003700070226      * leggo rekord di DST per assumere i valori MASSIMI
003800070226     C     kdst          chain     fidst03l
003900070226     c                   if        %found(fidst03l)
004000070226     c                   z-add     DSTpkl        D28pklo
004100070226     c                   z-add     DSTvlu        D28vluo
004200070227     c                   z-add     DSTnftl       D28nftl
004300070227     c                   z-add     DSTnbl        D28nblo
004400070226      * se è fase di test elabora i dati da ARG per agganciare le
004500070226      * bolle della distinta altrimenti legge direttamente ARB
004600070226     c                   if        §ogdda = '20391231'
004700070226     c                   exsr      elabARG
004800070226     c                   else
004900070226     c                   exsr      elabARB
005000070226     c                   endif
005100070227      * imposto numero fermate
005200070227     c                   z-add     1             x
005300070227     c     como70b       lookup    fer(x)                                 80
005400070227     c   80              eval      d28stpao = x - 1
005500070228      * imposto numero fermate
005600070228     c                   z-add     1             x
005700070228     c     como22b       lookup    cor(x)                                 80
005800070228     c   80              eval      d28stgeo = x - 1
005900070226      *non trovato DST
006000070226     c                   else
006100070226     c                   move      '1'           d28erro
006200070226     c                   goto      fine
006300070226     c                   endif
006400070226
006500070226     c                   endif
006600070226
006700070226     c     fine          tag
006800070226     c                   seton                                        lr
006900070213      *___________________________________________________
007000070226     C     elabARG       begsr
007100070213      *___________________________________________________
007200070226     c     karg          setll     fiarg03l
007300070226     c                   do        *hival
007400070226     c     karg          reade     fiarg03l
007500070226     c                   if        %eof(fiarg03l)
007600070226     c                   leave
007700070226     c                   endif
007800070227
007900070226     c     karb1         chain     fnarb01l
008000070226     c                   if        %found(fnarb01l)
008100070228      *memorizza fermate da ragione sociale/indirizzo
008200070227     c                   exsr      fermate
008300070228      *memorizza fermate GEOstazionarie
008400070228     c                   exsr      fermateGEO
008500070302      * richiamo funzione per calcolo pesi e volumi
008600070302     c                   exsr      calpesvol
008700070226      *bolle
008800070226     c                   add       1             D28nblao
008900070226     c
009000070226     c                   endif
009100070226     c                   enddo
009200070213      *
009300070213    1C                   endsr
009400070226      *___________________________________________________
009500070226     C     elabARB       begsr
009600070226      *___________________________________________________
009700070226     c     karb70        setll     fnarb70l
009800070226     c                   do        *hival
009900070226     c     karb70        reade     fnarb70l
010000070226     c                   if        %eof(fnarb70l)
010100070226     c                   leave
010200070226     c                   endif
010300070228      * aggancio ARG
010400070228     c     karg1         chain     fiarg01l
010500070228     c                   if        %found(fiarg01l)
010600070228      *memorizza fermate da ragione sociale/indirizzo
010700070227     c                   exsr      fermate
010800070228      *memorizza fermate GEOstazionarie
010900070228     c                   exsr      fermateGEO
011000070302      * richiamo funzione per calcolo pesi e volumi
011100070302     c                   exsr      calpesvol
011200070226      *bolle
011300070226     c                   add       1             D28nblao
011400070226     c
011500070228     c                   endif
011600070226     c                   enddo
011700070226      *
011800070226    1C                   endsr
011900070227      *___________________________________________________
012000070227     C     fermate       begsr
012100070227      *___________________________________________________
012200070227     c                   z-add     1             x
012300070227     c                   movel     arbrsd        como70
012400070227     c                   move      arbind        como70
012500070227     c     como70        lookup    fer                                    80
012600070227     c                   if        not *in80
012700070227     c     como70b       lookup    fer(x)                                 80
012800070227     c   80              movel     como70        fer(x)
012900070227     c                   endif
013000070227
013100070227    1C                   endsr
013200070228      *_____________________________________________________
013300070228     c     fermateGEO    begsr
013400070228      *_____________________________________________________
013500070228      * raggruppa fermata per latitudine longitudine
013600070228     c                   z-add     1             y
013700070228     c                   movel     argxlo        como22
013800070228     c                   move      argyla        como22
013900070228     c     como22        lookup    cor                                    81
014000070228     c                   if        not *in81
014100070228     c     como22b       lookup    cor(y)                                 81
014200070228     c   81              movel     como22        cor(y)
014300070228     c                   endif
014400070228      *
014500070228     c                   endsr
014600070226      *_______________________________________________
014700070302      *  Determinazione dei pesi e volumi da utilizzare
014800070226      *_______________________________________________
014900070302     C     calpesvol     begsr
015000070302     c                   clear                   fidg27ds
015100070302     c                   move      'R'           d27telai
015200070323     c                   z-add     d28dfvi       d27dfvi
015300070302     c                   movel     fidg27ds      kpjbu
015400070302     c                   call      'FIDG27R'
015500070302     c                   parm                    kpjba
015600070302     c                   parm                    fnarbds
015700070302     c
015800070302     c                   movel     kpjbu         fidg27ds
015900070302     c                   add       D27PKLDCO     D28pklao
016000070302     c                   add       D27VLUDCO     D28vluao
016100070226     C*
016200070226     C                   endsr
016300070226     c*-------------------------------------------------
016400070226     c     *inzsr        begsr
016500070226     c*-------------------------------------------------
016600070226     c     kdst          klist
016700070226     c                   kfld                    d28fgsi
016800070226     c                   kfld                    catk              1 0
016900070226     c                   kfld                    d28nfvi
017000070226     c                   z-add     4             catk
017100070226
017200070226     c     karb1         klist
017300070226     c                   kfld                    argaas
017400070226     c                   kfld                    arglnp
017500070226     c                   kfld                    argnrs
017600070226     c                   kfld                    argnsp
017700070226
017800070226     c     karg          klist
017900070226     c                   kfld                    d28fgsi
018000070226     c                   kfld                    nfvk              7 0
018100070226     c                   kfld                    d28dfvi
018200070228
018300070228     c     karg1         klist
018400070228     c                   kfld                    arbaas
018500070228     c                   kfld                    arblnp
018600070228     c                   kfld                    arbnrs
018700070228     c                   kfld                    arbnsp
018800070228
018900070226     c     karb70        klist
019000070226     c                   kfld                    ifpk
019100070226     c                   kfld                    nfvk
019200070226     c                   kfld                    d28dfvi
019300070226     c                   z-add     d28fgsi       ifpk              9 0
019400070226
019500070226     c                   endsr
