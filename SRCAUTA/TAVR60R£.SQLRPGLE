000100040119      *===============================================================*
000200111129      * Tavr60r    * Controllo per aumento varie
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000060914     fTita730c  if   e           k disk
001100111129     fwfctv01l  uf a e           k disk
001200040119      *
001300050526     d kpjba         e ds
001400111129     d tibs10ds      e ds
001500050607      ** DS Calcolo tassazione  - Varie
001600111129     d Dtasv         e ds                  prefix(w_)
001700050607     d  sv                     1     20
001800050607     d                                     Dim(20)                              Sigle varie
001900050607     d  va                    21    140P 3
002000050607     d                                     Dim(20)                              Importi varie
002100111129      **
002200111129     d TIBS69DS      E DS                  INZ
002300111129     d DS_cnaco      E DS                  extname(CNACO00F)
002400111129     d DS_cnind      E DS                  extname(CNIND00F)
002500111129     d DS_cnclp      E DS                  extname(CNCLP00F)
002600111129     d DS_fncls      E DS                  extname(FNCLS00F)
002700111129      * - Dati di TITAS00F estratti via SQL
002800111129     d TITASds       e ds                  extname(TITAS10F)
002900111129      *
003000040331      *
003100111129
003200111129     D wlbda8          ds                  inz                                  *controlla data
003300111129     D  g08dat                        8  0                                       -data dritta
003400111129     D  g08inv                        8  0                                       -data invertita
003500111129     D  g08err                        1                                          -errore
003600111129     D  g08tgi                        5  0                                       -giorni fra date
003700050526     D***
003800040331      *
003900040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004000050526      *
004100040331      * - Indici di schiera
004200040331     d xx              s              3  0 inz
004300050607     D k               s              5  0                                      indice
004400040331      * - Campi di comodo
004500060914     d kmgs            s                   Like(TasMgs)
004600050607     D Flgta7          s              1  0
004700060914      *
004800060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004900060915      *
005000040119
005100111129     c/EXEC SQL
005200111129     c+ declare A1 cursor for
005300111129     c+ select  *
005400111129     c+ from    TITAS10F
005500111129     c+ where   TASDFT between 20111031 and 20111101
005600111129     c+   and   TASNFT > 0
005700111129     c+   and   TASFIV in (996 , 998)
005800111129     c+ for     FETCH  only
005900111129     c/END-EXEC
006000111129      *
006100111129     c/EXEC SQL
006200111129     c+ OPEN A1
006300111129     c/END-EXEC
006400111129      *
006500111129     c/EXEC SQL
006600111129     c+ fetch next from A1 into :TITASds
006700111129     c/END-EXEC
006800040119      *
006900111129sel 1c                   dow       SQLcod = 0
007000111129      *
007100050614     c
007200060914
007300060914      * carico la tassazione corrente
007400060914     c                   exsr      sr_corrente
007500111123
007600111129     c/EXEC SQL
007700111129     c+ fetch next from A1 into :TITASds
007800111129     c/END-EXEC
007900111129      *
008000050607     c                   enddo
008100050526      *
008200070207
008300111129      *
008400111129     c/EXEC SQL
008500111129     c+ CLOSE A1
008600111129     c/END-EXEC
008700111129      *
008800040119     c                   eval      *inLR = *on
008900060914      *
009000040331      *
009100050607      ****************************************************
009200050607      ** Carica importi varie da tita7000
009300050607      ****************************************************
009400050607     c     Carta7        Begsr
009500050607      * pulisco le schiere delle varie
009600050607     c                   Do        20            k
009700050607     c                   clear                   VA(K)
009800050607     c                   clear                   SV(K)
009900050607     c                   Enddo
010000060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
010100050607     c                   clear                   flgta7
010200111129     c                   eval      sv(1) = tassv1
010300111129     c                   eval      va(1) = tasva1
010400111129     c                   eval      sv(2) = tassv2
010500111129     c                   eval      va(2) = tasva2
010600111129     c                   eval      sv(3) = tassv3
010700111129     c                   eval      va(3) = tasva3
010800050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
010900050607     c                   If        tassv3 <> *blanks
011000050607      * mi posiziono su tita7
011100050607     c     Kta7          setll     tita730c
011200050607     c                   if        %equal(tita730c)
011300050607     c                   z-add     3             k
011400050607      **
011500050607     c                   do        *hival
011600050607     c     Kta7          reade     tita730c
011700050607      * fine file
011800050607     c                   if        %eof(tita730c)
011900050607     c                   leave
012000050607     c                   endif
012100050607      * carico le varie
012200050607     c                   add       1             k
012300050607     c                   movel     ta7svn        sv(k)
012400050607     c                   z-add     ta7van        va(k)
012500050607     c                   z-add     1             flgta7
012600050607     c                   enddo
012700050607
012800050607     c                   endif
012900050607
013000050607     c                   endif
013100050607
013200050607     c                   Endsr
013300060914      *
013400050607      *************************************************************************
013500060914      * scrittura w-file dati tassazione corrente
013600050607      *************************************************************************
013700060914     C     sr_corrente   BEGSR
013800060914      * carico eventuali varie presenti in TITA7
013900060914     c                   Exsr      Carta7
014000060915     c
014100111019
014200111129      * per ogni varia verifico se esiste già un record a fronte del cliente
014300111129     c                   Do        20            k
014400111129     c                   If        sv(k) = ' '
014500111129     c                   leave
014600111129     c                   endif
014700111129
014800111129     c                   eval      varia = sv(k)
014900111129     c     Kctv          chain     wfctv01l
015000111129     c                   If        not %found(wfctv01l)
015100111129     c                   exsr      Sr_Creactv
015200111129     c                   Endif
015300111129     c                   If        va(k) > 0
015400111129     c                   add       1             ctvnsp
015500111129     c                   add       va(k)         ctvvva
015600111129     c                   add       tasncl        ctvncl
015700111129     c                   add       taspkb        ctvpkb
015800111129     C                   Else
015900111129     c                   add       1             ctvnsp0
016000111129     c                   add       tasncl        ctvncl0
016100111129     c                   add       taspkb        ctvpkb0
016200111129     c                   endif
016300111129     c                   If        %found(wfctv01l)
016400111129     c                   update    wfctv000
016500111129     c                   else
016600111129     c                   write     wfctv000
016700111129     c                   endif
016800111129
016900111129     c                   Enddo
017000111129
017100111129     c                   add       1             totspe            9 0
017200111129
017300050607     c                   endsr
017400111129      *
017500111129      *************************************************************************
017600111129      * scrittura w_file con l'unificante
017700111129      *************************************************************************
017800111129     C     sr_Creactv    BEGSR
017900111129     c                   clear                   wfctv000
018000111129     c                   z-add     tasksc        ctvksc
018100111129     c                   eval      ctvsva = sv(k)
018200111129     c* reperimento cod.cliente unificante
018300111129     c                   clear                   tibs10ds
018400111129     c                   eval      d10tle = 'ST'
018500111129     c                   eval      d10paf = 'P'
018600111129     c                   z-add     tasksc        d10cod
018700111129     c                   z-add     datcor        d10drf
018800111129     c                   call      'TIBS10R'
018900111129     c                   Parm                    Tibs10ds
019000111129    1c                   If        d10err =  *Blanks
019100111129     c                   z-add     d10cop        ctvkun
019200111129   x1c                   else
019300111129     c                   z-add     tasksc        ctvkun
019400111129    1c                   endif
019500111129     C                   clear                   DS_cnaco
019600111129     C                   clear                   DS_cnind
019700111129     C                   clear                   DS_cnclp
019800111129     C                   clear                   DS_fncls
019900111129     c                   eval      I69kcp = ctvkun
020000111129     C                   CALL      'TIBS69R'
020100111129     C                   PARM                    tibs69DS
020200111129     C                   PARM                    DS_cnaco
020300111129     C                   PARM                    DS_cnind
020400111129     C                   PARM                    DS_cnclp
020500111129     C                   PARM                    DS_fncls
020600111129     c                   movel     clpclv        ctvclv
020700111129
020800111129     c                   Endsr
020900050609      *************************************************************************
021000050609      * Operazioni iniziali
021100050609      *************************************************************************
021200050609     C     *inzsr        BEGSR
021300050609     c*
021400060914
021500050609     c     kta7          klist
021600050609     c                   kfld                    tasaas
021700050609     c                   kfld                    taslnp
021800050609     c                   kfld                    tasnrs
021900050609     c                   kfld                    tasnsp
022000050609     c                   kfld                    tastbl
022100081105
022200111129     c     kctv          klist
022300111129     c                   kfld                    tasksc
022400111129     c                   kfld                    Varia             1
022500050609     c
022600050609     c     *entry        plist
022700050609     c                   parm                    kpjba
022800060915
022900090430     c                   time                    wtime            14 0
023000111019     c                   move      wtime         wdata             8 0
023100111019     c                   movel     wtime         wora              6 0
023200111129     C                   z-add     Wdata         G08DAT
023300111129     C                   z-add     *zeros        G08INV
023400111129     C                   movel     '0'           G08ERR
023500111129     C                   call      'XSRDA8'
023600111129     C                   parm                    WLBDA8
023700111129     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
023800060915
023900050609     c                   endsr
