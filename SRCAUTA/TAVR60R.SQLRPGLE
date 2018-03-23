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
001100111202     fFiar531c  if   e           k disk
001200111202     Ftncsb03l  if   e           k disk
001300111129     fwfctv01l  uf a e           k disk
001400111130     Ftabel00f  IF   E           K DISK
001500040119      *
001600050526     d kpjba         e ds
001700111129     d tibs10ds      e ds
001800050607      ** DS Calcolo tassazione  - Varie
001900111129     d Dtasv         e ds                  prefix(w_)
002000050607     d  sv                     1     20
002100050607     d                                     Dim(20)                              Sigle varie
002200050607     d  va                    21    140P 3
002300050607     d                                     Dim(20)                              Importi varie
002400111129      **
002500111129     d TIBS69DS      E DS                  INZ
002600111129     d DS_cnaco      E DS                  extname(CNACO00F)
002700111129     d DS_cnind      E DS                  extname(CNIND00F)
002800111129     d DS_cnclp      E DS                  extname(CNCLP00F)
002900111129     d DS_fncls      E DS                  extname(FNCLS00F)
003000111129      * - Dati di TITAS00F estratti via SQL
003100111129     d TITASds       e ds                  extname(TITAS10F)
003200111130     D*------------
003300111130     D* DS RIDEFINIZIONE TABELLA "TB" (TIPI BOLLA)
003400111130     D*------------
003500111130     D DSTB          E DS
003600111130     D*------------
003700111130     D* DS RIDEFINIZIONE TABELLA "CC" (VARIE)
003800111130     D*------------
003900111130     D DSCC          E DS
004000111130     D*------------
004100111202     d dAr5Ban       e ds
004200111202     D** CAMBIO DIVISA
004300111202     D YEUR          E DS                  EXTNAME(YEURCODS)
004400111202     D*------------
004500111202     D DSqt1         E DS
004600111129      *
004700040331      *
004800111129
004900111129     D wlbda8          ds                  inz                                  *controlla data
005000111129     D  g08dat                        8  0                                       -data dritta
005100111129     D  g08inv                        8  0                                       -data invertita
005200111129     D  g08err                        1                                          -errore
005300111129     D  g08tgi                        5  0                                       -giorni fra date
005400050526     D***
005500040331      *
005600040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005700050526      *
005800040331      * - Indici di schiera
005900040331     d xx              s              3  0 inz
006000050607     D k               s              5  0                                      indice
006100111130     D  i              s              3  0 inz(*zeros)
006200040331      * - Campi di comodo
006300060914     d kmgs            s                   Like(TasMgs)
006400111202     d kAr5Trd         s                   Like(Ar5Trd)
006500050607     D Flgta7          s              1  0
006600111130     d oktbl           s              3  0
006700060914      *
006800060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
006900060915      *
007000111130     D*------------
007100111130     D SKTBL           S              2    DIM(100)
007200111130     D*------------
007300111130     D* SCHIERE SIGLE E DESCRIZIONI VARIE
007400111130     D*------------
007500111130     D SKSVN           S              1    DIM(100)
007600111130     D SKSVNDES        S             20    DIM(100)
007700111130     D*------------
007800040119
007900111129     c/EXEC SQL
008000111129     c+ declare A1 cursor for
008100111129     c+ select  *
008200111129     c+ from    TITAS10F
008300111130     c+ where   (TASDFT between 20111001 and 20111099
008400111130     c+   and   TASfiv <> 0)
008500111130     c+   or    (TASDFT = 20111101
008600111201     c+   and   TASFIV in (996 , 998 , 0))
008700111201     c+   or    (TASDFT between 20111002 and 20111099
008800111201     c+   and   TASfiv =  0)
008900111129     c+ for     FETCH  only
009000111129     c/END-EXEC
009100111129      *
009200111129     c/EXEC SQL
009300111129     c+ OPEN A1
009400111129     c/END-EXEC
009500111129      *
009600111129     c/EXEC SQL
009700111129     c+ fetch next from A1 into :TITASds
009800111129     c/END-EXEC
009900040119      *
010000111129sel 1c                   dow       SQLcod = 0
010100111130      * verifico se tipo bolla da elaborare
010200111130     c                   eval      oktbl=(%lookup(tastbl : sktbl))
010300111130      * controllo se bolla da elaborare
010400111130     c                   select
010500111201      * NO fattura di filiale del 1° del mese da elaborare
010600111201     c                   When      tasdft = 20111101 and (tasfiv <> 998 and
010700111201     c                             tasfiv <> 996)
010800111201      * NO fattura 1° del mese da elaborare con 998 e 996 solo quelle di filiale
010900111201     c                   When      tasdft = 20111001 and (tasfiv = 998 or
011000111201     c                             tasfiv = 996 or tasfiv = 0)
011100111130      * no bolle che non si contabilizzano
011200111130     c                   When      oktbl = 0
011300060914
011400111130     c                   other
011500060914      * carico la tassazione corrente
011600060914     c                   exsr      sr_corrente
011700111123
011800111130     c                   endsl
011900111129     c/EXEC SQL
012000111129     c+ fetch next from A1 into :TITASds
012100111129     c/END-EXEC
012200111129      *
012300050607     c                   enddo
012400050526      *
012500070207
012600111129      *
012700111129     c/EXEC SQL
012800111129     c+ CLOSE A1
012900111129     c/END-EXEC
013000111129      *
013100040119     c                   eval      *inLR = *on
013200060914      *
013300040331      *
013400050607      ****************************************************
013500050607      ** Carica importi varie da tita7000
013600050607      ****************************************************
013700050607     c     Carta7        Begsr
013800050607      * pulisco le schiere delle varie
013900050607     c                   Do        20            k
014000050607     c                   clear                   VA(K)
014100050607     c                   clear                   SV(K)
014200050607     c                   Enddo
014300060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
014400050607     c                   clear                   flgta7
014500111129     c                   eval      sv(1) = tassv1
014600111129     c                   eval      va(1) = tasva1
014700111129     c                   eval      sv(2) = tassv2
014800111129     c                   eval      va(2) = tasva2
014900111129     c                   eval      sv(3) = tassv3
015000111129     c                   eval      va(3) = tasva3
015100050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
015200050607     c                   If        tassv3 <> *blanks
015300050607      * mi posiziono su tita7
015400050607     c     Kta7          setll     tita730c
015500050607     c                   if        %equal(tita730c)
015600050607     c                   z-add     3             k
015700050607      **
015800050607     c                   do        *hival
015900050607     c     Kta7          reade     tita730c
016000050607      * fine file
016100050607     c                   if        %eof(tita730c)
016200050607     c                   leave
016300050607     c                   endif
016400050607      * carico le varie
016500050607     c                   add       1             k
016600050607     c                   movel     ta7svn        sv(k)
016700050607     c                   z-add     ta7van        va(k)
016800050607     c                   z-add     1             flgta7
016900050607     c                   enddo
017000050607
017100050607     c                   endif
017200050607
017300050607     c                   endif
017400050607
017500050607     c                   Endsr
017600060914      *
017700050607      *************************************************************************
017800060914      * scrittura w-file dati tassazione corrente
017900050607      *************************************************************************
018000060914     C     sr_corrente   BEGSR
018100060914      * carico eventuali varie presenti in TITA7
018200060914     c                   Exsr      Carta7
018300060915     c
018400111202      * calcolo il peso utilizzato in fattura
018500111202     c                   clear                   wtara             7 2
018600111202     c                   clear                   wntara            7 1
018700111202     c                   clear                   wrkpkf            7 1
018800111202     c                   If        tasfpf = 'V'
018900111202     c                   eval      wtara = §qttpc * tasncp
019000111202     c                   eval      wntara = taspkc - wtara
019100111202     c                   z-add     wntara        wrkpkf
019200111202     c                   else
019300111202     c                   z-add     taspkf        wrkpkf
019400111202     c                   Endif
019500111019
019600111129      * per ogni varia verifico se esiste già un record a fronte del cliente
019700111129     c                   Do        20            k
019800111129     c                   If        sv(k) = ' '
019900111129     c                   leave
020000111129     c                   endif
020100111129
020200111129     c                   eval      varia = sv(k)
020300111129     c     Kctv          chain     wfctv01l
020400111129     c                   If        not %found(wfctv01l)
020500111129     c                   exsr      Sr_Creactv
020600111129     c                   Endif
020700111129     c                   If        va(k) > 0
020800111129     c                   add       1             ctvnsp
020900111129     c                   add       va(k)         ctvvva
021000111129     c                   add       tasncl        ctvncl
021100111202     c                   add       wrkpkf        ctvpkf
021200111202      * varia = bancali
021300111202     c                   If        sv(k) = '='
021400111202     c                   clear                   dAr5Ban
021500111202     c                   If        %Subst(TasGva:1:1) = 'B'
021600111202
021700111202     c                   eval      kAr5Trd  = 'BAN'
021800111202     c     kFiar5        Chain     Fiar531c
021900111202     c                   If        %Found(Fiar531c)
022000111202     c                   Movel     Ar5Uni        dAr5Ban
022100111202     c                   EndIf
022200111202      * Bancali
022300111202     c                   Eval      ctvBnl = ctvbnl +(§Ar5Nba + §Ar5Nb2)
022400111202     c                   EndIf
022500111202     c                   Endif
022600111202      * varia a Pod immage
022700111202     c                   If        sv(k) = 'a'
022800111202     c                   eval      ctvimm = ctvimm + tasncl
022900111202     c                   endif
023000111202      * varia G C/Assegno
023100111202     c                   If        sv(k) = 'G'
023200111202     c     Kcsb          Chain     Tncsb03l
023300111202     c                   If        %found(tncsb03l)
023400111202     c                   If        csbvca <> 'EUR'
023500111202     c                   clear                   YEUR
023600111202     c                   movel     csbvca        YECDVI
023700111202     c                   movel     'EUR'         YECDVO
023800111202     c                   z-add     csbcas        YECIMI
023900111202     c                   move      '3'           YECDCO
024000111202      *
024100111202     c                   CALL      'YEURCO'
024200111202     c                   PARM                    YEUR
024300111202      *
024400111202     c                   If        yecesi = ' '
024500111202     c                   eval      ctvvca= ctvvca + YECIMO
024600111202     c                   Endif
024700111202     c                   Else
024800111202     c                   eval      ctvvca= ctvvca + csbcas
024900111202     c                   Endif
025000111202
025100111202     c                   Endif
025200111202     c                   Endif
025300111202      * varia R AC Plus
025400111202     c                   If        sv(k) = 'R'
025500111202     c                   If        tasvas <> 'EUR' and tasvas <> '   '
025600111202     c                   clear                   YEUR
025700111202     c                   movel     tasvas        YECDVI
025800111202     c                   movel     'EUR'         YECDVO
025900111202     c                   z-add     tasias        YECIMI
026000111202     c                   move      '3'           YECDCO
026100111202      *
026200111202     c                   CALL      'YEURCO'
026300111202     c                   PARM                    YEUR
026400111202      *
026500111202     c                   If        yecesi = ' '
026600111202     c                   eval      ctvass= ctvass + YECIMO
026700111202     c                   Endif
026800111202     c                   Else
026900111202     c                   eval      ctvass= ctvass + tasias
027000111202     c                   Endif
027100111202     c                   Endif
027200111202
027300111129     C                   Else
027400111129     c                   add       1             ctvnsp0
027500111129     c                   add       tasncl        ctvncl0
027600111202     c                   add       wrkpkf        ctvpkf0
027700111129     c                   endif
027800111129     c                   If        %found(wfctv01l)
027900111129     c                   update    wfctv000
028000111129     c                   else
028100111129     c                   write     wfctv000
028200111129     c                   endif
028300111129
028400111129     c                   Enddo
028500111129
028600111129     c                   add       1             totspe            9 0
028700111129
028800050607     c                   endsr
028900111129      *
029000111129      *************************************************************************
029100111129      * scrittura w_file con l'unificante
029200111129      *************************************************************************
029300111129     C     sr_Creactv    BEGSR
029400111129     c                   clear                   wfctv000
029500111129     c                   z-add     tasksc        ctvksc
029600111129     c                   eval      ctvsva = sv(k)
029700111129     c* reperimento cod.cliente unificante
029800111129     c                   clear                   tibs10ds
029900111129     c                   eval      d10tle = 'ST'
030000111129     c                   eval      d10paf = 'P'
030100111129     c                   z-add     tasksc        d10cod
030200111129     c                   z-add     datcor        d10drf
030300111129     c                   call      'TIBS10R'
030400111129     c                   Parm                    Tibs10ds
030500111129    1c                   If        d10err =  *Blanks
030600111129     c                   z-add     d10cop        ctvkun
030700111129   x1c                   else
030800111129     c                   z-add     tasksc        ctvkun
030900111129    1c                   endif
031000111129     C                   clear                   DS_cnaco
031100111129     C                   clear                   DS_cnind
031200111129     C                   clear                   DS_cnclp
031300111129     C                   clear                   DS_fncls
031400111129     c                   eval      I69kcp = ctvkun
031500111129     C                   CALL      'TIBS69R'
031600111129     C                   PARM                    tibs69DS
031700111129     C                   PARM                    DS_cnaco
031800111129     C                   PARM                    DS_cnind
031900111129     C                   PARM                    DS_cnclp
032000111129     C                   PARM                    DS_fncls
032100111129     c                   movel     clpclv        ctvclv
032200111130     C* Reperisco la descrizione della VARIA
032300111130     C                   Z-ADD     1             i
032400111130     C     ctvsva        LOOKUP    SKSVN(i)                               55
032500111130     C                   IF        %found
032600111130     C                   EVAL      ctvDES = SKSVNDES(i)
032700111130     C                   ELSE
032800111130     C                   EVAL      ctvDES = *all'*'
032900111130     C                   ENDIF
033000111129
033100111129     c                   Endsr
033200050609      *************************************************************************
033300050609      * Operazioni iniziali
033400050609      *************************************************************************
033500050609     C     *inzsr        BEGSR
033600050609     c*
033700060914
033800050609     c     kta7          klist
033900050609     c                   kfld                    tasaas
034000050609     c                   kfld                    taslnp
034100050609     c                   kfld                    tasnrs
034200050609     c                   kfld                    tasnsp
034300050609     c                   kfld                    tastbl
034400081105
034500111129     c     kctv          klist
034600111129     c                   kfld                    tasksc
034700111129     c                   kfld                    Varia             1
034800111202
034900111202     c     kFiar5        Klist
035000111202     c                   Kfld                    TasAas
035100111202     c                   Kfld                    TasLnp
035200111202     c                   Kfld                    TasNrs
035300111202     c                   Kfld                    TasNsp
035400111202     c                   Kfld                    kAr5Trd
035500111202      *
035600111202     c     KCSB          KLIST
035700111202     c                   KFLD                    TASAAS
035800111202     c                   KFLD                    TASLNP
035900111202     c                   KFLD                    TASNRS
036000111202     c                   KFLD                    TASNSP
036100111202
036200111202     C* LETTURA tabel00f - completa
036300111202     C     KEYtab        KLIST
036400111202     C                   KFLD                    tblKUT
036500111202     C                   KFLD                    tblCOD
036600111202     C                   KFLD                    tblkey
036700111202     C*
036800111130     C* LETTURA tabel00f - PARZIALE
036900111130     C     KEYtab_1      KLIST
037000111130     C                   KFLD                    tblKUT                         *UTENTE
037100111130     C                   KFLD                    tblCOD                         *TABELLA
037200111130     C*
037300050609     c
037400050609     c     *entry        plist
037500050609     c                   parm                    kpjba
037600060915
037700090430     c                   time                    wtime            14 0
037800111019     c                   move      wtime         wdata             8 0
037900111019     c                   movel     wtime         wora              6 0
038000111129     C                   z-add     Wdata         G08DAT
038100111129     C                   z-add     *zeros        G08INV
038200111129     C                   movel     '0'           G08ERR
038300111129     C                   call      'XSRDA8'
038400111129     C                   parm                    WLBDA8
038500111129     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038600060915
038700111130     C*
038800111130     C* Reperisco i tipi bolla validi come spedizioni
038900111130     C                   Z-ADD     *zeros        i
039000111130     C                   Z-ADD     1             tblKUT
039100111130     C                   MOVEL(P)  'TB'          tblCOD
039200111130     C     KEYtab_1      SETLL     TABEL00F
039300111130     C                   IF        %equal(TABEL00F)
039400111130     C     KEYtab_1      READE     TABEL00F
039500111130     C                   DOW       not %eof(TABEL00F)
039600111130     C                   MOVEL(P)  TBLUNI        DSTB
039700111130     C                   IF        §TBFCB = '1'
039800111130     C                   ADD       1             i
039900111130     C                   MOVEL(P)  TBLKEY        SKTBL(i)
040000111130     C                   ENDIF
040100111130     C     KEYtab_1      READE     TABEL00F
040200111130     C                   ENDDO
040300111130     C                   ENDIF
040400111130     C* Reperisco le descrizioni delle varie
040500111130     C                   Z-ADD     *zeros        i
040600111130     C                   Z-ADD     1             tblKUT
040700111130     C                   MOVEL(P)  'CC'          tblCOD
040800111130     C     KEYtab_1      SETLL     TABEL00F
040900111130     C                   IF        %equal(TABEL00F)
041000111130     C     KEYtab_1      READE     TABEL00F
041100111130     C                   DOW       not %eof(TABEL00F)
041200111130     C                   ADD       1             i
041300111130     C                   MOVE(P)   TBLKEY        SKSVN(i)
041400111130     C                   MOVEL(P)  TBLUNI        DSCC
041500111130     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
041600111130     C     KEYtab_1      READE     TABEL00F
041700111130     C                   ENDDO
041800111130     C                   ENDIF
041900111202      **
042000111202     C                   Z-ADD     1             tblKUT
042100111202     C                   MOVEL     'QT'          tblcod
042200111202     C                   MOVEL(p)  '1'           tblkey
042300111202     C     KEYtab        CHAIN     TABEL00f
042400111202     c                   If        %found(tabel00f)
042500111202     C                   MOVEL     TBLUNI        DSQT1
042600111202     c                   Endif
042700111130     C*
042800050609     c                   endsr
