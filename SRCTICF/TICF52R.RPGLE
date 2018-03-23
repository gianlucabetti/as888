000800130913
000900040119      *===============================================================*
001000120511      * Ticf52r    * Confronto fatturazione solo assegnati
001100040119      *===============================================================*
001200040119
001300040119     h decedit('0,') datedit(*dmy/)
001400040119     h option(*nodebugio)
001500040119
001600040119      *---------------------------------------------------------------*
001700040119
001800120511     fwfcfa02l  if   e           k disk
001900120511     fwfcfa10f  uf a e             disk
002000050608     FTABEL00F  IF   E           K DISK
002100130913     fAZCMM01L  if   e           k disk
002200050609     fazorg01l  if   e           k disk
002300080117     ftntam01l  if   e           k disk
002400130913
002500130913      *---------------------------------------------------------------*
002600050609
002700101108     d param           ds
002800101108     D  paraam                 1      6  0
002900101108     d  paraa                  1      4  0
003000101108     d  parmm                  5      6  0
003100050614     d kpjba         e ds
003200050608     d ds17          e ds
003300050608     d ds05          e ds
003400080117     d dsta01        e ds
003500050609     d tibs10ds      e ds
003600070516     d tise70ds      e ds
003700050608     d TIBS69DS      E DS                  INZ
003800050608     d DS_cnaco      E DS                  extname(CNACO00F)
003900050608     d DS_cnind      E DS                  extname(CNIND00F)
004000050608     d DS_cnclp      E DS                  extname(CNCLP00F)
004100050608     d DS_fncls      E DS                  extname(FNCLS00F)
004200050609     D wlbda8          ds                  inz                                  *controlla data
004300050609     D  g08dat                        8  0                                       -data dritta
004400050609     D  g08inv                        8  0                                       -data invertita
004500050609     D  g08err                        1                                          -errore
004600050609     D  g08tgi                        5  0                                       -giorni fra date
004700130913
004800130913      *---------------------------------------------------------------*
004900050608
005000050608     d codut           s              1  0 Inz(1)
005100050608     D kcod            s                   like(tblcod)
005200050608     D kkey            s                   like(tblkey)
005300120511     D wnsp            s                   like(cfa1tos)
005400120514     D wpora           s                   like(cfa1toa)
005500120514     D wporr           s                   like(cfa1tor)
005600120511     D s_cfaksc        s                   like(cfaksc)
005700120511     D s_cfactr        s                   like(cfactr)
005800130913
005900130913      *---------------------------------------------------------------*
006000050608
006100050614     C     *entry        plist
006200050614     C                   parm                    kpjba
006300050614     c                   movel     kpjbu         param
006400050614     c*
006500050608     C     KTAB          KLIST
006600050608     C                   KFLD                    CODUT
006700050608     C                   KFLD                    KCOD
006800050608     C                   KFLD                    KKEY
006900080117      *
007000080117     C     KTAM          KLIST
007100120511     C                   KFLD                    s_cfaksc
007200120511     C                   KFLD                    s_cfactr
007300050609     C*--------------------
007400050609     C* CALCOLA LA DATA CORRENTE
007500050609     C*--------------------
007600050609     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
007700050609     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
007800050609     C                   z-add     WN8           G08DAT
007900050609     C                   z-add     *zeros        G08INV
008000050609     C                   movel     '0'           G08ERR
008100050609     C                   call      'XSRDA8'
008200050609     C                   parm                    WLBDA8
008300050609     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
008400130913      *
008500120511     C     *loval        setll     wfcfa02l
008600050608     C                   do        *hival
008700120511     C                   read      wfcfa02l
008800120511     C                   if        %eof(wfcfa02l)
008900050608     C                   leave
009000050608     C                   endif
009100050609     c* a rottura cliente/tariffa memorizzo dati nel file
009200120511     c                   if        cfaksc <> s_cfaksc
009300120511     c                   if        s_cfaksc > 0
009400120511     c                   exsr      sr_wrtcfa
009500050608     c                   endif
009600120511     c                   z-add     cfaksc        s_cfaksc
009700120511     c                   z-add     cfactr        s_cfactr
009800050608     c* azzero contatori
009900050608     c                   clear                   wnsp
010000120514     c                   clear                   wpora
010100120514     c                   clear                   wporr
010200050608     c                   endif
010300050609     c*
010400120511     c                   if        cfactr <> s_cfactr
010500120511     c                   exsr      sr_wrtcfa
010600120511     c                   z-add     cfactr        s_cfactr
010700050609     c* azzero contatori
010800050609     c                   clear                   wnsp
010900120514     c                   clear                   wpora
011000120514     c                   clear                   wporr
011100050609     c                   endif
011200050608     c
011300050608     c* INCREMENTO CONTATORI
011400050608     c* numero spedizioni elaborate
011500050608     c                   add       1             wnsp
011600120514     c                   add       cfapora       wpora
011700120514     c                   add       cfaporr       wporr
011800050608     c
011900050608     c                   enddo
012000050609     c* memorizzo ultimi dati nel file
012100120511     c                   exsr      sr_wrtcfa
012200050608     c*
012300050608     c                   seton                                        lr
012400130913      *
012500050608     C                   eval      I69TLA  = 'C'
012600050608     C                   call      'TIBS69R'
012700050608     C                   parm                    TIBS69DS
012800050608      **-------------------------------------------------------------**
012900050608      ** Scrittura file                                              **
013000050608      **-------------------------------------------------------------**
013100120511     c     sr_wrtcfa     BEGSR
013200120511     c                   clear                   wfcfa100
013300080129      * recupero anagrafica cliente
013400080129     C                   clear                   DS_cnaco
013500080129     C                   clear                   DS_cnind
013600080129     C                   clear                   DS_cnclp
013700080129     C                   clear                   DS_fncls
013800120511     c                   eval      I69kac = s_cfaksc
013900120511     c                   eval      I69kcp = s_cfaksc
014000080129     C                   CALL      'TIBS69R'
014100080129     C                   PARM                    tibs69DS
014200080129     C                   PARM                    DS_cnaco
014300080129     C                   PARM                    DS_cnind
014400080129     C                   PARM                    DS_cnclp
014500080129     C                   PARM                    DS_fncls
014600120511     c                   z-add     s_cfaksc      cfa1ksc
014700120511     c                   movel     acorag        cfa1kscd
014800120511     c                   z-add     s_cfactr      cfa1ctr
014900080129      * reperimento del tipo tariffa
015000080129     c     ktam          chain     tntam01l
015100080129     c                   if        %found(tntam01l)
015200080129     c                   eval      dsta01 = tamflo
015300080129      * carico se tariffa I - E - D - F
015400080129     c                   select
015500080129     c                   when      §tadpd = 'S'
015600120511     c                   eval      cfa1ttr = 'D'
015700080129     c                   when      §tafed = 'S'
015800120511     c                   eval      cfa1ttr = 'F'
015900080129     c                   other
016000120511     c                   eval      cfa1ttr = tamfie
016100080129     c                   endsl
016200130913      *
016300080129     c                   endif
016400130913      *
016500080129     c* reperimento cod.cliente unificante
016600080129     c                   clear                   tibs10ds
016700080129     c                   eval      d10tle = 'ST'
016800080129     c                   eval      d10paf = 'P'
016900120511     c                   z-add     cfa1ksc       d10cod
017000080129     c                   z-add     datcor        d10drf
017100080129     c                   call      'TIBS10R'
017200080129     c                   Parm                    Tibs10ds
017300080129    1c                   If        d10err =  *Blanks
017400120511     c                   z-add     d10cop        cfa1kun
017500080129   x1c                   else
017600120511     c                   z-add     cfa1ksc       cfa1kun
017700080129    1c                   endif
017800120511    1c                   if        cfa1ksc = cfa1kun
017900120511     c                   movel     cfa1kscd      cfa1kund
018000120511     c                   movel     clpclv        cfa1clv
018100080129   x1c                   else
018200080129     C                   clear                   DS_cnaco
018300080129     C                   clear                   DS_cnind
018400080129     C                   clear                   DS_cnclp
018500080129     C                   clear                   DS_fncls
018600120511     c                   eval      I69kac = cfa1kun
018700120511     c                   eval      I69kcp = cfa1kun
018800080129     C                   CALL      'TIBS69R'
018900080129     C                   PARM                    tibs69DS
019000080129     C                   PARM                    DS_cnaco
019100080129     C                   PARM                    DS_cnind
019200080129     C                   PARM                    DS_cnclp
019300080129     C                   PARM                    DS_fncls
019400120511     c                   movel     acorag        cfa1kund
019500120511     c                   movel     clpclv        cfa1clv
019600080129    1c                   endif
019700120514     c                   z-add     wpora         cfa1toa
019800120514     c                   z-add     wporr         cfa1tor
019900120511     c                   z-add     wnsp          cfa1tos
020000080219      * dal 2° confronto fatturazione recuperiamo il codice agente del cliente unificante e non
020100080219      * del figlio
020200120511     c                   z-add     clpage        cfa1age
020300130913     C     CFA1age       CHAIN     AZCMM000
020400130913    1c                   if        %found(AZCMM01L)
020500130913     c                   movel     CMMdes        cfa1aged
020600130913     c                   z-add     CMMuni        cfa1rgf
020700130913    1c                   endif
020800130913     C     CFA1rgf       CHAIN     AZCMM000
020900130913    1c                   if        %found(AZCMM01L)
021000130913     c                   movel     CMMdes        cfa1rgfd
021100130913    1c                   endif
021200080129      * recupero p.o. area e distretto del codice commerciale unificante e non + del cliente
021300080129      * come da richiesta del 2008 di Cuffaro Alessandro
021400120511     c                   movel     cfa1rgf       cfa1fil
021500080129     c* reperisco cod. distretto e area
021600120511     c     cfa1fil       chain     azorg01l
021700050608     C* Decodifico distretto
021800050608     C                   EXSR      DECDST
021900050608     C* Decodifico area
022000050608     C                   EXSR      DECCAR
022100050608     c* decodifica p.o.
022200120511     c                   movel     orgdes        cfa1fild
022300120511     c                   movel     orgfl3        cfa1dit
022400120511     c                   movel     §17des        cfa1ditd
022500120511     c                   z-add     orgcar        cfa1are
022600120511     c                   movel     §05des        cfa1ared
022700050614     c* richiamo pgm per reperimento numero di spedizioni da file saldi
022800070516     c                   clear                   tise70ds
022900050614     c                   movel     'S'           i70tla
023000120511     c                   z-add     cfa1ksc       i70ksc
023100120511     c                   move      cfa1ctr       i70ctr
023200050614     c                   z-add     paraam        i70dai
023300050614     c                   z-add     paraam        i70daf
023400070516     c                   call      'TISE70R'
023500070516     c                   parm                    tise70ds
023600120511     c                   z-add     o70nsp        cfa1nsp
023700130913     c*
023800120511     c                   write     wfcfa100
023900050608     c                   endsr
024000050608     C*****************************************************************
024100050608     C* DECODIFICA CODICE DISTRETTO
024200050608     C*****************************************************************
024300050608     C     DECDST        BEGSR
024400050608     C* Chaino tabella 17 per decodifica
024500050608     C                   MOVEL     '17'          KCOD
024600050608     C                   MOVEL(P)  ORGFL3        KKEY
024700050608     c                   clear                   ds17
024800050608     C     KTAB          CHAIN     TABEL00F                           30
024900050608    2C     *IN30         IFEQ      *OFF
025000050608     C                   MOVEL     TBLUNI        DS17
025100050608     c                   endif
025200050608     C                   ENDSR
025300050608     C*****************************************************************
025400050608     C* DECODIFICA CODICE AREA
025500050608     C*****************************************************************
025600050608     C     DECCAR        BEGSR
025700050608     C* Chaino tabella 05 per decodifica
025800050608     C                   MOVEL     '05'          KCOD
025900050608     C                   MOVEL(P)  ORGCAR        KKEY
026000050608     c                   clear                   ds05
026100050608     C     KTAB          CHAIN     TABEL00F                           30
026200050608    2C     *IN30         IFEQ      *OFF
026300050608     C                   MOVEL     TBLUNI        DS05
026400050608     c                   endif
026500050608     C                   ENDSR
