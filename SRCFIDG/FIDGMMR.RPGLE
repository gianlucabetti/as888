000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070423     H*
000300070327     h* Calc.x distinta tot.pes/vol/bolle assegnati da CML
000400000000     H*--------------------------------------------------------------*
000500070226     Ffidst03l  if   e           k disk
000600070420     Ffidst32l  if   e           k disk
000700070226     Ffiarg03l  if   e           k disk
000800070228     Ffiarg01l  if   e           k disk    rename(fiarg000:fiarg1)
000900070226     Ffnarb01l  if   e           k disk
001000070226     Ffnarb70l  if   e           k disk    rename(fnarb000:fnarb70)
001100070226     Fazorg01l  if   e           k disk
001200070424     Ffidst41l  uf a e           k disk
001300950125     D*
001400070302     D kpjba         E DS
001500070302     D fidg28ds      E DS
001600070302     D fidg27ds      E DS
001700070302     D fnarbds       E DS                  extname(fnarb00f)
001800070226     D og146         E DS
001900070420     d como10          s             10
002000070227     d como70          s             70
002100070227     d como70b         s             70
002200070420     d fer             s             70    dim(500)
002300070420     d como80          s             80
002400070420     d como80b         s             80
002500070420     d ferg            s             80    dim(500)
002600070420     d ferg10          s             10    dim(500)
002700070420     d fergn           s              3  0 dim(500)
002800070227     d x               s              3  0
002900070420     d xx              s              3  0
003000070227     d como22          s             22
003100070227     d como22b         s             22
003200070227     d cor             s             22    dim(500)
003300070227     d y               s              3  0
003400070420      * skiere per riempimento file
003500070420     d giro            s             10    dim(500)
003600070420     d peso            s              7  2 dim(500)
003700070420     d volu            s              5  3 dim(500)
003800070420     d boll            s              3  0 dim(500)
003900070424     d dataiso         s               d   datfmt(*iso)
004000070424     d dataeur         s               d   datfmt(*eur)
004100070213      *___________________________________________________
004200070213     C     *entry        plist
004300070226     C                   parm                    fidg28ds
004400070226     c                   z-add     d28nfvi       nfvk
004500070424      *
004600070424     C                   time                    w0120            14 0
004700070424     C                   move      w0120         wdat              8 0
004800070424     C                   movel     w0120         wora              6 0
004900070424     c                   move      wdat          dataeur
005000070424     c                   move      dataeur       dataiso
005100070424     c                   move      dataiso       wdymd             8 0
005200070213
005300070226     c     d28fgsi       chain     azorg01l
005400070226     c                   if        %found(azorg01l)
005500070226     c                   movel     orgde6        og146
005600070226     c                   if        §ogdda = ' '
005700070226     c                   move      '1'           d28erro
005800070226     c                   goto      fine
005900070424     c                   else
006000070424     c                   move      §ogdda        ogddaN            8 0
006100070226     c                   endif
006200070226      * leggo rekord di DST per assumere i valori MASSIMI
006300070226     C     kdst          chain     fidst03l
006400070226     c                   if        %found(fidst03l)
006500070226     c                   z-add     DSTpkl        D28pklo
006600070226     c                   z-add     DSTvlu        D28vluo
006700070227     c                   z-add     DSTnftl       D28nftl
006800070227     c                   z-add     DSTnbl        D28nblo
006900070226      * se è fase di test elabora i dati da ARG per agganciare le
007000070226      * bolle della distinta altrimenti legge direttamente ARB
007100070424     c                   if        §ogdda = '20391231' or
007200070424     c                             wdymd  < ogddaN
007300070226     c                   exsr      elabARG
007400070226     c                   else
007500070226     c                   exsr      elabARB
007600070226     c                   endif
007700070227      * imposto numero fermate
007800070227     c                   z-add     1             x
007900070227     c     como70b       lookup    fer(x)                                 80
008000070227     c   80              eval      d28stpao = x - 1
008100070228      * imposto numero fermate
008200070228     c                   z-add     1             x
008300070228     c     como22b       lookup    cor(x)                                 80
008400070228     c   80              eval      d28stgeo = x - 1
008500070420      * scrive file con totali per distinta giro
008600070424     c                   exsr      scrividst4
008700070226      *non trovato DST
008800070226     c                   else
008900070226     c                   move      '1'           d28erro
009000070226     c                   goto      fine
009100070226     c                   endif
009200070226
009300070226     c                   endif
009400070226
009500070226     c     fine          tag
009600070226     c                   seton                                        lr
009700070213      *___________________________________________________
009800070226     C     elabARG       begsr
009900070213      *___________________________________________________
010000070226     c     karg          setll     fiarg03l
010100070226     c                   do        *hival
010200070226     c     karg          reade     fiarg03l
010300070226     c                   if        %eof(fiarg03l)
010400070226     c                   leave
010500070226     c                   endif
010600070227
010700070226     c     karb1         chain     fnarb01l
010800070226     c                   if        %found(fnarb01l)
010900070228      *memorizza fermate da ragione sociale/indirizzo
011000070227     c                   exsr      fermate
011100070228      *memorizza fermate GEOstazionarie
011200070228     c                   exsr      fermateGEO
011300070423      *memorizza fermate x giro
011400070423     c                   exsr      fermategiro
011500070302      * richiamo funzione per calcolo pesi e volumi
011600070302     c                   exsr      calpesvol
011700070226      *bolle
011800070226     c                   add       1             D28nblao
011900070423      *preparo skiere per distinta
012000070424     c                   exsr      carica
012100070226     c
012200070226     c                   endif
012300070226     c                   enddo
012400070213      *
012500070213    1C                   endsr
012600070226      *___________________________________________________
012700070226     C     elabARB       begsr
012800070226      *___________________________________________________
012900070226     c     karb70        setll     fnarb70l
013000070226     c                   do        *hival
013100070226     c     karb70        reade     fnarb70l
013200070226     c                   if        %eof(fnarb70l)
013300070226     c                   leave
013400070226     c                   endif
013500070228      * aggancio ARG
013600070228     c     karg1         chain     fiarg01l
013700070228     c                   if        %found(fiarg01l)
013800070228      *memorizza fermate da ragione sociale/indirizzo
013900070227     c                   exsr      fermate
014000070228      *memorizza fermate GEOstazionarie
014100070228     c                   exsr      fermateGEO
014200070423      *memorizza fermate x giro
014300070420     c                   exsr      fermategiro
014400070302      * richiamo funzione per calcolo pesi e volumi
014500070302     c                   exsr      calpesvol
014600070226      *bolle
014700070226     c                   add       1             D28nblao
014800070420      *preparo skiere per distinta
014900070420     c                   exsr      carica
015000070226     c
015100070228     c                   endif
015200070226     c                   enddo
015300070226      *
015400070226    1C                   endsr
015500070227      *___________________________________________________
015600070227     C     fermate       begsr
015700070227      *___________________________________________________
015800070227     c                   z-add     1             x
015900070227     c                   movel     arbrsd        como70
016000070227     c                   move      arbind        como70
016100070227     c     como70        lookup    fer                                    80
016200070227     c                   if        not *in80
016300070227     c     como70b       lookup    fer(x)                                 80
016400070227     c   80              movel     como70        fer(x)
016500070227     c                   endif
016600070227
016700070227    1C                   endsr
016800070228      *_____________________________________________________
016900070228     c     fermateGEO    begsr
017000070228      *_____________________________________________________
017100070228      * raggruppa fermata per latitudine longitudine
017200070228     c                   z-add     1             y
017300070228     c                   movel     argxlo        como22
017400070228     c                   move      argyla        como22
017500070228     c     como22        lookup    cor                                    81
017600070228     c                   if        not *in81
017700070228     c     como22b       lookup    cor(y)                                 81
017800070228     c   81              movel     como22        cor(y)
017900070228     c                   endif
018000070228      *
018100070228     c                   endsr
018200070420      *___________________________________________________
018300070420     C     fermategiro   begsr
018400070420      *___________________________________________________
018500070420      * totalizza fermate per giro
018600070423     c                   z-add     1             x
018700070420     c                   eval      como80 = argcgi + arbrsd + arbind
018800070420     c     como80        lookup    ferg(x)                                80
018900070420     c                   if        not *in80
019000070420     c                   z-add     1             x
019100070420     c     como80b       lookup    ferg(x)                                80
019200070420     c   80              movel     como80        ferg(x)
019300070420     c                   z-add     1             xx
019400070420     c     argcgi        lookup    ferg10(xx)                             80
019500070420     c   80              add       1             fergn(xx)
019600070420     c                   if        not *in80
019700070420     c                   z-add     1             xx
019800070420     c     como10        lookup    ferg10(xx)                             80
019900070420     c   80              movel     argcgi        ferg10(xx)
020000070420     c   80              z-add     1             fergn(xx)
020100070420     c                   endif
020200070420     c                   endif
020300070420
020400070420    1C                   endsr
020500070226      *_______________________________________________
020600070302      *  Determinazione dei pesi e volumi da utilizzare
020700070226      *_______________________________________________
020800070302     C     calpesvol     begsr
020900070302     c                   clear                   fidg27ds
021000070302     c                   move      'R'           d27telai
021100070323     c                   z-add     d28dfvi       d27dfvi
021200070302     c                   movel     fidg27ds      kpjbu
021300070302     c                   call      'FIDG27R'
021400070302     c                   parm                    kpjba
021500070302     c                   parm                    fnarbds
021600070302     c
021700070302     c                   movel     kpjbu         fidg27ds
021800070302     c                   add       D27PKLDCO     D28pklao
021900070302     c                   add       D27VLUDCO     D28vluao
022000070226     C*
022100070226     C                   endsr
022200070420      *___________________________________________________
022300070420     C     carica        begsr
022400070420      *___________________________________________________
022500070420      * totalizza pesi volumi bolle per giro
022600070420     c                   z-add     1             x
022700070420     c     argcgi        lookup    giro(x)                                80
022800070420     c                   if        *in80
022900070420     c                   add       D27PKLDCO     peso(x)
023000070420     c                   add       D27VLUDCO     volu(x)
023100070420     c                   add       1             boll(x)
023200070420     c                   else
023300070420     c     como10        lookup    giro(x)                                80
023400070420     c                   if        *in80
023500070420     c                   movel     argcgi        giro(x)
023600070420     c                   z-add     D27PKLDCO     peso(x)
023700070420     c                   z-add     D27VLUDCO     volu(x)
023800070420     c                   z-add     1             boll(x)
023900070420     c                   endif
024000070420     c                   endif
024100070420
024200070420    1C                   endsr
024300070420      *___________________________________________________
024400070424     C     scrividst4    begsr
024500070420      *___________________________________________________
024600070420      * pulizia file elimina i rekord esistenti per quella distinta
024700070424     c     kdst4         setll     fidst41l
024800070420     c                   do        *hival
024900070424     c     kdst4         reade     fidst41l
025000070424     c                   if        %eof(fidst41l)
025100070420     c                   leave
025200070420     c                   endif
025300070424     c                   delete    fidst400
025400070420     c                   enddo
025500070420      * scrive il nuovo dettaglio distinta giro
025600070420     c     1             do        500           x
025700070420     c                   if        giro(x) = *blank
025800070420     c                   leave
025900070420     c                   endif
026000070420     c                   move      giro(x)       girok
026100070420     c     kdst32        chain     fidst32l
026200070420     c                   if        %found(fidst32l)
026300070424     c                   z-add     dst3prgg      dst4PRGG
026400070424     c                   z-add     dst3sql       dst4SQL
026500070420     c                   endif
026600070424     c                   z-add     4             dst4npg
026700070424     c                   z-add     d28fgsi       dst4FGS
026800070424     c                   z-add     d28nfvi       dst4NDC
026900070424     c                   z-add     d28dfvi       dst4DDC
027000070424     c                   z-add     d28fgsi       dst4FGSG
027100070424     c                   movel     giro(x)       dst4cgi
027200070424     c                   z-add     peso(x)       dst4PKL
027300070424     c                   z-add     volu(x)       dst4VLU
027400070424     c                   z-add     boll(x)       dst4nbl
027500070424     c                   move      *blank        dst4flo
027600070420     c                   z-add     1             xx
027700070420     c     girok         lookup    ferg10(xx)                             80
027800070424     c   80              z-add     fergn(xx)     dst4NFTL
027900070424     c                   write     fidst400
028000070420     c                   enddo
028100070420
028200070420    1C                   endsr
028300070226     c*-------------------------------------------------
028400070226     c     *inzsr        begsr
028500070226     c*-------------------------------------------------
028600070226     c     kdst          klist
028700070226     c                   kfld                    d28fgsi
028800070226     c                   kfld                    catk              1 0
028900070226     c                   kfld                    d28nfvi
029000070226     c                   z-add     4             catk
029100070420     c     kdst32        klist
029200070420     c                   kfld                    d28fgsi
029300070420     c                   kfld                    d28dfvi
029400070420     c                   kfld                    d28fgsi
029500070420     c                   kfld                    girok            10
029600070420     c                   kfld                    d28nfvi
029700070226
029800070226     c     karb1         klist
029900070226     c                   kfld                    argaas
030000070226     c                   kfld                    arglnp
030100070226     c                   kfld                    argnrs
030200070226     c                   kfld                    argnsp
030300070226
030400070226     c     karg          klist
030500070226     c                   kfld                    d28fgsi
030600070226     c                   kfld                    nfvk              7 0
030700070226     c                   kfld                    d28dfvi
030800070228
030900070228     c     karg1         klist
031000070228     c                   kfld                    arbaas
031100070228     c                   kfld                    arblnp
031200070228     c                   kfld                    arbnrs
031300070228     c                   kfld                    arbnsp
031400070228
031500070226     c     karb70        klist
031600070226     c                   kfld                    ifpk
031700070226     c                   kfld                    nfvk
031800070226     c                   kfld                    d28dfvi
031900070226     c                   z-add     d28fgsi       ifpk              9 0
032000070424     c     kdst4         klist
032100070424     c                   kfld                    d28fgsi
032200070424     c                   kfld                    knpg              1 0
032300070424     c                   kfld                    nfvk
032400070424     c                   kfld                    d28dfvi
032500070424     c                   z-add     4             knpg
032600070424
032700070226
032800070226     c                   endsr
