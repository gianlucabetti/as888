000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101070516     h dftactgrp(*no) actgrp(*caller)
000200070423     H*
000300070516     h* Aggiorna x distinta tot.pes/vol/bolle assegnati da CML
000400000000     H*--------------------------------------------------------------*
000401070517     c* ATTENZIONE: ho fatto l'aggiornamento con il fisico perchè se il
000402070517     c* chiamante deve aggiornare lo stesso record questo deve essere condi-
000403070517     c* viso quindi: prima di aprire il file FIDST00F fare OVRDBF SHARE(*yes)
000404070517     c* e gli aggiornamenti devono essere fatti sul fisico.
000500070517     Ffidst03l  if   e           k disk    infds(dsinf)
000501070517     Ffidst00f  uf   e             disk    commit rename(fidst000: fidst00)
000600070420     Ffidst32l  if   e           k disk
000700070226     Ffiarg03l  if   e           k disk
000800070228     Ffiarg01l  if   e           k disk    rename(fiarg000:fiarg1)
000900070226     Ffnarb01l  if   e           k disk
001000070226     Ffnarb70l  if   e           k disk    rename(fnarb000:fnarb70)
001100070226     Fazorg01l  if   e           k disk
001200070516     Ffidst41l  uf a e           k disk    commit
001300950125     D*
001301070517     d dsinf           ds
001302070517     d  dst_nrr              397    400b 0
001303070523     d trul82ds      e ds
001400070302     D kpjba         E DS
001500071001     D fior67ds      E DS
001501071001     D fidg37ds      E DS
001600070302     D fidg27ds      E DS
001700070302     D fnarbds       E DS                  extname(fnarb00f)
001800070226     D og146         E DS
001801071001     D og147         E DS
001900070516     d como11b         s             11
001901070516     d como11          s             11
002000070227     d como70          s             70
002100070227     d como70b         s             70
002200070420     d fer             s             70    dim(500)
002300070516     d como81          s             81
002400070516     d como81b         s             81
002500070516     d ferg            s             81    dim(500)
002600070516     d ferg11          s             11    dim(500)
002700070420     d fergn           s              3  0 dim(500)
002800070227     d x               s              3  0
002900070420     d xx              s              3  0
003000070227     d como22          s             22
003100070227     d como22b         s             22
003200070227     d cor             s             22    dim(500)
003300070227     d y               s              3  0
003301071126     d wpeso           s             14  1 INZ
003302071122     d wvolume         s             10  3 INZ
003400070420      * skiere per riempimento file
003500070516     d giro            s             11    dim(500)
003600071126     d peso            s             14  1 dim(500)
003700071122     d volu            s             10  3 dim(500)
003800070420     d boll            s              3  0 dim(500)
003900070424     d dataiso         s               d   datfmt(*iso)
004000070424     d dataeur         s               d   datfmt(*eur)
004001070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
004100070213      *___________________________________________________
004200070213     C     *entry        plist
004201070517     C                   PARM                    KPJBA
004301070517     c                   if        kpjbu = ' '
004302070517     c                   clear                   fidg37ds
004303070517     c                   move      '1'           d37erro
004304070517     c                   goto      fine
004305070517     c                   else
004306070517     C                   MOVEL     KPJBU         fidg37ds
004307070517     c                   end
004400070516     c                   z-add     d37nfvi       nfvk
004401070517     c                   z-add     d37fgsi       ifpk              9 0
004402070517     c                   clear                   D37PKLO
004403070517     c                   clear                   D37VLUO
004404070517     c                   clear                   D37NFTLO
004405070517     c                   clear                   D37NBLO
004406070517     c                   clear                   D37PKLAO
004407070517     c                   clear                   D37VLUAO
004408070517     c                   clear                   D37STPAO
004409070517     c                   clear                   D37NBLAO
004410070517     c                   clear                   D37STGEO
004411070517     c                   clear                   D37ERRO
004500070424      *
004600070424     C                   time                    w0120            14 0
004700070424     C                   move      w0120         wdat              8 0
004800070424     C                   movel     w0120         wora              6 0
004900070424     c                   move      wdat          dataeur
005000070424     c                   move      dataeur       dataiso
005100070424     c                   move      dataiso       wdymd             8 0
005200070213
005300070516     c     d37fgsi       chain     azorg01l
005400070226     c                   if        %found(azorg01l)
005500070226     c                   movel     orgde6        og146
005501071001     c                   movel     orgde7        og147
005600070226     c                   if        §ogdda = ' '
005700070516     c                   move      '1'           d37erro
005800070226     c                   goto      fine
005900070424     c                   else
006000070424     c                   move      §ogdda        ogddaN            8 0
006100070226     c                   endif
006200070226      * leggo rekord di DST per assumere i valori MASSIMI
006300070517     C     kdst          chain     fidst03l
006301070517     c                   if        %found(fidst03l)
006302070517     C     dst_nrr       chain(e)  fidst00f
006400070516     c                   if        %error
006401070523     c                   exsr      geslck
006402070523     c                   if        ul82§sts = 'A'
006403070516     c                   move      '1'           d37erro
006404070516     c                   goto      fine
006405070528     c                   else
006406070528     C     dst_nrr       chain     fidst00f
006407070516     c                   endif
006408070523     c                   endif
006502070516     c                   z-add     DSTpkl        d37pklo
006600070516     c                   z-add     DSTvlu        d37vluo
006700070517     c                   z-add     DSTnftl       d37nftlo
006800070516     c                   z-add     DSTnbl        d37nblo
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
008000070516     c   80              eval      d37stpao = x - 1
008100070228      * imposto numero fermate
008200070228     c                   z-add     1             x
008300070228     c     como22b       lookup    cor(x)                                 80
008400070516     c   80              eval      d37stgeo = x - 1
008500070420      * scrive file con totali per distinta giro
008600070424     c                   exsr      scrividst4
008601070516     c* Aggiorno FIDST00F
008602070516     c                   eval      dstpkla = d37pklao
008603070516     c                   eval      dstVLUa = d37vluao
008604070516     c                   eval      dststpa = d37stpao
008605070516     c                   eval      DSTnblA = D37NBLAO
008606070517     c                   update    fidst00
008607071001     c* richaimo il pgm di agg. valori orm nel caso ho agginto o tolto un RC
008608071001     c* solo se p.o. abilitato
008609071001     c                   if        §OGDDAO <> ' ' and §OGDDAO <> '00000000'
008611071001     c                   clear                   fior67ds
008612071001     c                   eval      or67FGSI = D37FGSI
008613071001     c                   eval      or67DFVI = D37DFVI
008614071001     c                   eval      or67NFVI = D37NFVI
008615071001     c                   eval      or67cmtI = 'N'
008616071002     c                   eval      kpjbu = fior67ds
008617071001     c                   call      'FIOR67R'
008618071001     c                   parm                    kpjba
008619071001     c                   end
008620070516     c* se richiesto commit
008621070516     c                   if        d37cmti = 'S'
008622070516     c                   commit
008623070516     c                   end
008700070226      *non trovato DST
008800070226     c                   else
008900070516     c                   move      '1'           d37erro
009000070226     c                   goto      fine
009100070226     c                   endif
009200070226
009300070226     c                   endif
009400070226
009401070517     c     fine          tag
009402070517     c                   eval      kpjbu = fidg37ds
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
010501070516
010502070516     c                   if        argnftl = 99999
010503070516     c                   movel     'M'           como11
010504070516     c                   else
010505070516     c                   movel     'A'           como11
010506070516     c                   end
010507070516     c                   move      argcgi        como11
010600070227
010700070226     c     karb1         chain     fnarb01l
010800070226     c                   if        %found(fnarb01l)
010900070228      *memorizza fermate da ragione sociale/indirizzo
011000070227     c                   exsr      fermate
011100070228      *memorizza fermate GEOstazionarie
011200070228     c                   exsr      fermateGEO
011300070423      *memorizza fermate x giro
011400070516     c*                  exsr      fermategiro
011500070302      * richiamo funzione per calcolo pesi e volumi
011600070302     c                   exsr      calpesvol
011700070226      *bolle
011800070516     c                   add       1             d37nblao
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
013401070516     c                   if        arbngd <> 0
013402070516     c                   movel     'M'           como11
013403070516     c                   else
013404070516     c                   movel     'A'           como11
013405070516     c                   end
013500070228      * aggancio ARG
013600070228     c     karg1         chain     fiarg01l
013700070228     c                   if        %found(fiarg01l)
013701070516     c                   move      argcgi        como11
013800070228      *memorizza fermate da ragione sociale/indirizzo
013900070227     c                   exsr      fermate
014000070228      *memorizza fermate GEOstazionarie
014100070228     c                   exsr      fermateGEO
014200070423      *memorizza fermate x giro
014300070516     c*                  exsr      fermategiro
014400070302      * richiamo funzione per calcolo pesi e volumi
014500070302     c                   exsr      calpesvol
014600070226      *bolle
014700070516     c                   add       1             d37nblao
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
018700070516     c                   eval      como81 = como11 + arbrsd + arbind
018800070516     c     como81        lookup    ferg(x)                                80
018900070420     c                   if        not *in80
019000070420     c                   z-add     1             x
019100070516     c     como81b       lookup    ferg(x)                                80
019200070516     c   80              movel     como81        ferg(x)
019300070420     c                   z-add     1             xx
019400070516     c     como11        lookup    ferg11(xx)                             80
019500070420     c   80              add       1             fergn(xx)
019600070420     c                   if        not *in80
019700070420     c                   z-add     1             xx
019800070516     c     como11b       lookup    ferg11(xx)                             80
019900070516     c   80              movel     como11        ferg11(xx)
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
021100070516     c                   z-add     d37dfvi       d27dfvi
021101070827     c                   eval      D27dtaI = D37dtaI
021102070827     c                   eval      D27oraI = D37oraI
021200070302     c                   movel     fidg27ds      kpjbu
021300070302     c                   call      'FIDG27R'
021400070302     c                   parm                    kpjba
021500070302     c                   parm                    fnarbds
021600070302     c
021700070302     c                   movel     kpjbu         fidg27ds
021800071122     c                   add       D27PKLDCO     wpeso
021900071122     c                   add       D27VLUDCO     wvolume
021902071122     c                   if        wpeso > 999999,9
021903071122     c                   eval      d37pklao = 999999,9
021904071122     c                   else
021905071122     c                   eval      d37pklao = wpeso
021907071122     c                   endif
021908071122      *
021910071122     c                   if        wvolume > 99,999
021911071122     c                   eval      d37vluao = 99,999
021912071122     c                   else
021913071122     c                   eval      d37vluao = wvolume
021914071122     c                   endif
022000070226     C*
022100070226     C                   endsr
022200070420      *___________________________________________________
022300070420     C     carica        begsr
022400070420      *___________________________________________________
022500070420      * totalizza pesi volumi bolle per giro
022600070420     c                   z-add     1             x
022700070516     c     como11        lookup    giro(x)                                80
022800070420     c                   if        *in80
022900070420     c                   add       D27PKLDCO     peso(x)
023000070420     c                   add       D27VLUDCO     volu(x)
023100070420     c                   add       1             boll(x)
023200070420     c                   else
023300070516     c     como11b       lookup    giro(x)                                80
023400070420     c                   if        *in80
023500070516     c                   movel     como11        giro(x)
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
026001070530     c                   movel     giro(x)       tassok            1
026002070530     c                   clear                   dst4PRGG
026003070530     c                   clear                   dst4SQL
026004070530     c                   if        tassok = 'A'
026100070420     c     kdst32        chain     fidst32l
026200070420     c                   if        %found(fidst32l)
026300070424     c                   z-add     dst3prgg      dst4PRGG
026400070424     c                   z-add     dst3sql       dst4SQL
026500070420     c                   endif
026501070530     c                   endif
026600070424     c                   z-add     4             dst4npg
026700070516     c                   z-add     d37fgsi       dst4FGS
026800070516     c                   z-add     d37nfvi       dst4Nfv
026900070516     c                   z-add     d37dfvi       dst4Dfv
027000070516     c                   z-add     d37fgsi       dst4FGSG
027100070516     c                   movel     giro(x)       dst4tass
027101070516     c                   move      giro(x)       dst4cgi
027102071122     c                   if        peso(x)>999999,9
027103071122     c                   eval      dst4pkl = 999999,9
027104071122     c                   else
027106071122     c                   z-add     peso(x)       dst4PKL
027107071122     c                   endif
027108071122      *
027109071122     c                   if        wvolume > 99,999
027110071122     c                   eval      dst4vlu = 99,999
027111071122     c                   else
027113071122     c                   z-add     volu(x)       dst4VLU
027114071122     c                   endif
027400070424     c                   z-add     boll(x)       dst4nbl
027500070424     c                   move      *blank        dst4flo
027600070420     c                   z-add     1             xx
027700070516     c*    girok         lookup    ferg10(xx)                             80
027800070516     c*  80              z-add     fergn(xx)     dst4NFTL
027900070424     c                   write     fidst400
028000070420     c                   enddo
028100070420
028200070420    1C                   endsr
028300070226     c*-------------------------------------------------
028400070226     c     *inzsr        begsr
028500070226     c*-------------------------------------------------
028600070226     c     kdst          klist
028700070516     c                   kfld                    d37fgsi
028800070226     c                   kfld                    catk              1 0
028900070516     c                   kfld                    d37nfvi
029000070226     c                   z-add     4             catk
029100070420     c     kdst32        klist
029200070516     c                   kfld                    d37fgsi
029300070516     c                   kfld                    d37dfvi
029400070516     c                   kfld                    d37fgsi
029500070420     c                   kfld                    girok            10
029600070516     c                   kfld                    d37nfvi
029700070226
029800070226     c     karb1         klist
029900070226     c                   kfld                    argaas
030000070226     c                   kfld                    arglnp
030100070226     c                   kfld                    argnrs
030200070226     c                   kfld                    argnsp
030300070226
030400070226     c     karg          klist
030500070516     c                   kfld                    d37fgsi
030600070226     c                   kfld                    nfvk              7 0
030700070516     c                   kfld                    d37dfvi
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
031800070516     c                   kfld                    d37dfvi
032000070424     c     kdst4         klist
032100070516     c                   kfld                    d37fgsi
032200070424     c                   kfld                    knpg              1 0
032300070516     c                   kfld                    d37nfvi
032400070516     c                   kfld                    d37dfvi
032500070424     c                   z-add     4             knpg
032600070424
032700070226
032800070226     c                   endsr
032900070523     C*----------------------------------------------------*
033000070523     C     geslck        begsr
033100070523     C*----------------------------------------------------*
033200070523     c                   clear                   trul82ds
033300070523     c                   Eval      ul82§rrn = dst_nrr
033400070523     c                   Eval      UL82§FIL = 'FIDST00F'
033500070523     c                   Eval      UL82§WIN = 'S'
033700070523     c                   Eval      UL82§num = 2
033800070523     c                   Eval      UL82§att = 2
033900070523     c                   eval      ul82§mss = ErrMsg(1)
034000070523     c                   Eval      UL82§msw = 'La Distinta '
034100070523     c                             + %editc(d37nfvi:'Z') +  ' è già utilizzata -
034200070523     c                             da un altro lavoro e non è manutenzionabile'
034300070523      * Effettuo la chiamata al *PGM d utilità
034400070523     c                   Call(e)   'TRUL82R'
034500070523     c                   Parm                    Trul82ds
034600070523
034700070523     C                   ENDSR
034800070523** ErrMsg
034900070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
