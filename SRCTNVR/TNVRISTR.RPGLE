000100050204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200050204      *--------------------------------------------------------------*
000300050204      *                 -- PRETASSAZIONE WURTH --                    *
000400050204      *--------------------------------------------------------------*
000500050204     FWFIST00f  uf A e           k disk
000600050204     FTitas30c  if   e           k disk    ignore(titasp00)
000700050204     FTita730c  if   e           k disk    ignore(tita7p00)
000800050204     FTncsb03l  if   e           k disk
000900050204     FTabel00f  if   e           k disk
001000050204     FFiar531c  if   e           k Disk
001100050207     FTnvristp  o    e             printer oflind(*in90)
001200050204
001300050204      *
001400050204      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
001500050204      *
001600050204
001700050204     d Kpjba         e ds
001800050204      * - Parametri x Controllo profilo utenti
001900050204     d TIBS34DS      e ds                  inz
002000050204      * - Ds di riferimento al file esterno AZUTE00F
002100050204     d AZUTEDS       e ds                  extname(AZUTE00F)
002200050204      * - Ds per dati organigramma
002300050204     d DDatiUte      e ds
002400050204      ** DS Calcolo tassazione
002500050204     d Dtas          e ds
002600050204     d  dstasflo     e                     extfld(tasflo)
002700050204      ** DS Fira5
002800050204     d dAr5Ban       e ds
002900050204     d dAr5Bnb       e ds
003000050204      ** DS Calcolo tassazione  - Varie
003100050204     d Dtasv         E DS
003200050204     d  SV                     1     20
003300050204     d                                     DIM(20)                              Sigle Varie
003400050204     d  VA                    21    140P 3
003500050204     d                                     DIM(20)                              Importi Varie
003600050204      * Importi limiti controllo  tassazione
003700050204     d Dgei          E DS                  occurs(5)
003800050204      * DS aggancio tabelle
003900050204     d Tibs02ds      E DS
004000050204
004100050204     d Ds3a          E DS
004200050204
004300050204      *
004400050204      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004500050204      *
004600050204
004700050204     D* codici bolla che prevedono un imponibile =0
004800050204     D IM0             S              2    DIM(50)                              LEGENDA VARIE SIGLE
004900050204     D* codici bolla che prevedono la tassazione di una singola varia
005000050204     D CBO             S              2    DIM(50)                              LEGENDA VARIE SIGLE
005100050204     D CBV             S              1    DIM(50)                              LEGENDA VARIE SIGLE
005200050204
005300050204      *
005400050204      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005500050204      *
005600050204     d Wok             s              1
005700050204     d annok           s              4  0
005800050204     d W0140           s             14  0
005900050204
006000050204     d kAr5Trd         s                   Like(Ar5Trd)
006100050204     d savncl          s                   Like(TasNcl)
006200050204
006300050204
006400050204      *
006500050204      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
006600050204      *
006700050204
006800050204      *
006900050204      *   I N D I C A T O R I   - - - - - - - - - - - - - - - - - - - *
007000050204      *
007100050204      * 90 - OF printer file
007200050204      * 99 - Stampa testata per la prima volta
007300050204
007400050204      *------------------------------------------------------------------------*
007500050204      *   C I C L O    P R I N C I P A L E
007600050204      *------------------------------------------------------------------------*
007700050204
007800050204     c                   eval      annok = 2005
007900050204     c     annok         setll     titas30c
008000050204     c                   do        *hival
008100050204
008200050204     c     annok         reade     titas30c
008300050204
008400050204     c                   if        %eof(titas30c)
008500050204     c                   leave
008600050204     c                   endif
008700050204
008800050204      * controllo validità del record
008900050204
009000050204     c                   exsr      SR_ctrrec
009100050204      * se non è ok leggo il sucessivo
009200050204     c                   If        Wok <> 'Y'
009300050204     c                   iter
009400050204     c                   endif
009500050204      * provo la tassazione
009600050204     c                   Exsr      tassaz
009700050204
009800050204     c                   Enddo
009900050204
010000050207     c                   seton                                        lr
010100050204      *
010200050204      *------------------------------------------------------------------------*
010300050204      *   Controllo se record valido
010400050204      *------------------------------------------------------------------------*
010500050204     c     Sr_ctrrec     Begsr
010600050204
010700050204     c                   eval      Wok = 'Y'
010800050204      *  controllo numeri  di serie  se diverso da 40  record non valido
010900050204     c                   If        tasfic = ' '
011000050204     c                   eval      Wok = 'N'
011100050204     c                   endif
011200050204      *  controllo IVA  FIV = 998 996
011300050207     c                   If        tasfiv <> 998 and tasfiv <> 996
011400050204     c                   eval      Wok = 'N'
011500050204     c                   endif
011600050204
011700050204     c                   endsr
011800050204
011900050204      *------------------------------------------------------------------------*
012000050204      *   Tassazione
012100050204      *------------------------------------------------------------------------*
012200050204     c     Tassaz        Begsr
012300050204
012400050204      * pulisco i campi variabili di DTAS
012500050204     c                   clear                   tastla
012600050204     c                   clear                   taserr
012700050204     c                   clear                   tasmsg
012800050204     c                   clear                   tasial
012900050204     c                   clear                   task88
013000050204     c                   clear                   tasfcaa
013100050204     c                   clear                   taspkcn
013200050204     c                   clear                   tasspc
013300050204     c                   clear                   tasBan
013400050207     C                   Z-ADD     0             TASCAS
013500050207     C                   MOVEL     *BLANKS       TASTIC
013600050207     C                   MOVEL     *BLANKS       TASVCA
013700050207     C                   Z-ADD     0             TASCMB
013800050207     C                   CLEAR                   TASSVA
013900050204      * pulisco campi di comodo
014000050204     c                   clear                   Savncl
014100050204     c                   clear                   dAr5Ban
014200050204     c                   clear                   dAr5Bnb
014300050204
014400050204      * imposto data spedizione
014500050204     c                   clear                   Tasdsp
014600050204     c                   movel     Tasaas        Tasdsp
014700050204     c                   move      Tasmgs        Tasdsp
014800050204
014900050204     c                   eval      tastc1 = tasftc
015000050204      * imposto la data recupero tariffa di cartello
015100050204     c                   z-add     tasdft        tasdct
015200050204      * Flag DDT Si No
015300050204     c                   If        Tasll1 = 'C' or Tasll1 = 'S'
015400050204     c                   eval      Tasddt =  'S'
015500050204     c                   else
015600050204     c                   clear                   Tasddt
015700050204     c                   endif
015800050204
015900050204      * Bancali
016000050204     c                   If        %Subst(TasGva:1:1) = 'B'
016100050204
016200050204     c                   eval      kAr5Trd  = 'BAN'
016300050204     c     kFiar5        Chain     Fiar531c
016400050204     c                   If        %Found(Fiar531c)
016500050204     c                   Movel     Ar5Uni        dAr5Ban
016600050204     c                   EndIf
016700050204      * Bancali
016800050204     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
016900050204     c                   EndIf
017000050204     C**
017100050204      * Colli Originali
017200050204     c                   If        %Subst(TasGva:1:1) = 'O'
017300050204
017400050204     c                   eval      SavNcl = TasNcl
017500050204     c                   eval      kAr5Trd  = 'BNB'
017600050204     c     kFiar5        Chain     Fiar531c
017700050204     c                   If        %Found(Fiar531c)
017800050204     c                   Movel     Ar5Uni        dAr5Bnb
017900050204     c                   EndIf
018000050204      * Colli
018100050204     c                   Eval      TasNcl = §Ar5bcor
018200050204     c                   EndIf
018300050204      * Contrassegno
018400050204     c                   If        Tasfca = ' '
018500050204     c     Kcsb          chain     Tncsb03l
018600050204     c                   If        %found(tncsb03l)  and tastbl = csbtbl
018700050204     c                   movel     csbsta        tassta
018800050204     c                   z-add     csbcas        tascas
018900050204     c                   movel     csbtpa        tastic
019000050204     c                   if        csbfus <> *blanks
019100050204     c                   move      csbfus        tastic
019200050204     c                   else
019300050204     c                   move      csbtpi        tastic
019400050204     c                   endif
019500050204
019600050204     c                   movel     csbvca        tasvca
019700050204     c                   z-add     csbcmb        tascmb
019800050204
019900050204     c                   endif
020000050204     c                   endif
020100050204      * carico le varie
020200050204     c                   DO        20            K                 2 0
020300050204     c     K             IFGT      3
020400050204     c                   Z-ADD     0             VA(K)
020500050204     c                   MOVEL     *BLANKS       SV(K)
020600050204     c                   END
020700050204     c                   END
020800050204      ** PULIZIA SCHIERE
020900050204     c                   Z-ADD     0             FLGTA7            1 0
021000050204     c     TASSV3        CABEQ     *BLANKS       ENDTA7
021100050204     c     KTA7          SETLL     TITA7000                               92
021200050204     c  N92              GOTO      ENDTA7
021300050204     c                   Z-ADD     3             K
021400050204      **
021500050204     c     SUTA7         TAG
021600050204     c     KTA7          READE     TITA7000                               92
021700050204     c   92              GOTO      ENDTA7
021800050204     c                   ADD       1             K
021900050204     c                   MOVEL     TA7SVN        SV(K)
022000050204     c                   Z-ADD     TA7VAN        VA(K)
022100050204     c                   Z-ADD     1             FLGTA7
022200050204     c                   GOTO      SUTA7
022300050204     c     ENDTA7        tag
022400050204      * mi salvo l'imponibile
022500050204     C                   z-add     tasimv        preimv           13 3
022600050204      * pulisco i campi di comodo
022700050204     c                   z-add     *zeros        risu             13 3
022800050204     c                   z-add     *zeros        diffe            13 3
022900050204     c** se si tratta di codice bolla che prevede solo una varia, passo solo
023000050204     c*  quella
023100050204     c                   z-add     1             kk
023200050204     c     tascbo        lookup    cbo(kk)                                05
023300050204     c                   if        *in05
023400050204     c                   evAL      tassva=cBV(kk)
023500050204     c                   endif
023600050204      *
023700050204      * azzero l'imponibile
023800050204     c                   clear                   tasimv
023900050204     c                   clear                   tasdiv
024000050207
024100050207     c                   clear                   impist
024200050204
024300050204      * TASSO
024400050204     c                   Call      'TNSF20R'
024500050204     c                   Parm                    Kpjba
024600050204     c                   Parm                    Dtas
024700050204     c                   Parm                    Dtasv
024800050207      * se non c'è manca tariffa da TNSF20R controllo l'imponibile senza varia "L" se sono
024900050207      * diversi
025000050207     c                   If        taserr = ' ' and preimv <> tasimv
025100050204      * cerco la varia "L"
025200050204     c                   z-add     1             il                2 0
025300050207     c     'L'           lookup    sv(il)                                 94
025400050204     c                   If        %found
025500050207     c                   z-add     va(il)        impist
025600050204     c                   eval      risu = tasimv - va(il)
025700050204      * verifico il vecchio imponibile con risu
025800050204     c                   eval      diffe = preimv - risu
025900050204      * se c'è poca differenza (+ o - l'euro ) scrivo il record di WORK
026000050207     c                   If        diffe <= 1 and diffe >= -1
026100050204      * creo il mio file di work  aggiungendo i record
026200050204     c                   exsr      SR_addwf
026300050204    2c                   endif
026400050204    1c*
026500050204     c                   endif
026600050204    1c*
026700050204     c                   endif
026800050207      * stampo
026900050207     c                   If        *in90 or not *in99
027000050207     c                   write     sistt1
027100050207     c                   write     sistt2
027200050207     c                   seton                                        99
027300050207     c                   setoff                                       90
027400050207     c                   endif
027500050207      *
027600050207      *
027700050207     c                   write     sistd01
027800050207
027900050207     c                   endsr
028000050204
028100050204      *---------------------------------------------------------------*
028200050204      * Routine di aggiunta record in work  file                      *
028300050204      *---------------------------------------------------------------*
028400050204     c     SR_addwf      BegSR
028500050204
028600050204      * verfiico se esiste già un record per quel cliente /tariffa / numero
028700050204      * e data fattura
028800050207     c     kist          chain     wfist00f
028900050207     c                   if        %found(wfist00f)
029000050207     c                   add       1             istnsp
029100050207     c                   add       va(il)        istist
029200050207     c                   update    wfist000
029300050207     c                   else
029400050207     c                   clear                   wfist000
029500050207     c                   eval      istksc    = tasksc
029600050207     c                   eval      istctr    = tasctr
029700050207     c                   eval      istnft    = tasnft
029800050207     c                   eval      istdft    = tasdft
029900050207     c                   eval      istfiv    = tasfiv
030000050207     c                   add       1             istnsp
030100050207     c                   add       va(il)        istist
030200050207     c                   write     wfist000
030300050207     c                   endif
030400050204      *
030500050207     c                   endsr
030600050204      *---------------------------------------------------------------*
030700050204      * Routine Iniziale                                              *
030800050204      *---------------------------------------------------------------*
030900050204     c     *InzSR        BegSR
031000050204      *
031100050204     c     *Entry        plist
031200050204     c                   parm                    KPJBA
031300050204      * Reperisco data corrente
031400050204     c                   time                    w0140
031500050204     c                   move      w0140         wDate
031600050204     c                   movel     w0140         wora
031700050204      *
031800050204      * Reperisco dati job
031900050204     c                   exsr      DatiJob
032000050204      * Reperisco dati tabella GEI
032100050204     c                   exsr      Cargei
032200050204      *
032300050204     c                   z-add     1             codut             1 0
032400050204      *
032500050204      * KLIST
032600050204      * Fiar531c
032700050204     c     kFiar5        Klist
032800050204     c                   Kfld                    TasAas
032900050204     c                   Kfld                    TasLnp
033000050204     c                   Kfld                    TasNrs
033100050204     c                   Kfld                    TasNsp
033200050204     c                   Kfld                    kAr5Trd
033300050204      * Tncsb03l
033400050204     c     KCSB          Klist
033500050204     c                   Kfld                    tasaas
033600050204     c                   Kfld                    taslnp
033700050204     c                   Kfld                    tasnrs
033800050204     c                   Kfld                    tasnsp
033900050204      * Tabella
034000050204     C     KTAB          KLIST
034100050204     C                   KFLD                    CODUT
034200050204     C                   KFLD                    COD               2
034300050204      * WFist00f
034400050204     c     Kist          Klist
034500050204     c                   Kfld                    tasksc
034600050204     c                   Kfld                    tasctr
034700050207     c                   Kfld                    tasdft
034800050204     c                   Kfld                    tasnft
034900050204     c                   Kfld                    tasfiv
035000050207     C*  KLIST PER ACCESSO FILES
035100050207     C     KTA7          KLIST
035200050207     C                   KFLD                    TASAAS
035300050207     C                   KFLD                    TASLNP
035400050207     C                   KFLD                    TASNRS
035500050207     C                   KFLD                    TASNSP
035600050207     C                   KFLD                    TASTBL
035700050207     c*
035800050204
035900050204     C                   ENDSR
036000050204      *---------------------------------------------------------------*
036100050204      * Reperimento Dati del job (Utente/Operativi)                   *
036200050204      *---------------------------------------------------------------*
036300050204     c     DatiJob       BegSr
036400050204      *
036500050204     c     *dtaara       define    §azute        azuteds
036600050204     c     *dtaara       define    §datiute      ddatiute
036700050204      *
036800050204     c                   in(E)     *dtaara
036900050204     c                   IF        %ERROR or RSUT = *blanks
037000050204     c                   clear                   Tibs34Ds
037100050204     c                   call      'TIBS34R'
037200050204     c                   parm                    Tibs34Ds
037300050204     c                   in        *dtaara
037400050204     c                   ENDIF
037500050204
037600050204     c                   Endsr
037700050204      *******************************************************************
037800050204      ** Carico tabella importi limite fatturazione
037900050204      *******************************************************************
038000050204     c     Cargei        Begsr
038100050204      *
038200050204     c                   clear                   tibs02ds
038300050204     c                   movel     'L'           T02tla
038400050204     c                   movel     'C'           T02mod
038500050204     c                   movel     KNSIF         T02sif
038600050204     c                   movel     'GEI'         T02cod
038700050204     c                   movel     'EUR'         T02ke1
038800050204      *
038900050204     c                   Call      'TIBS02R'
039000050204     c                   Parm                    Kpjba
039100050204     c                   Parm                    Tibs02ds
039200050204      *
039300050204     c                   movel     T02uni        Dgei
039400050204      *
039500050204     c                   Endsr
039600050204     C**********************************************************
039700050204     C** CARICAMENTO codici bolla che prevedono di tassare solo 1 bolla
039800050204     C*   e che permettono imponibile =0
039900050204     C**********************************************************
040000050204     C     CARCBO        BEGSR
040100050204     C** PULIZIA DS
040200050204     C                   MOVEL     '3A'          COD
040300050204     C                   Z-ADD     0             K
040400050204     C                   Z-ADD     0             KK                3 0
040500050204     C     KTAB          SETLL     TABEl00f                               07
040600050204     C     KTAB          READE     TABEL00f                               07
040700050204    1C     *IN07         DOWEQ     *OFF
040800050204    2C     TBLKEY        IFNE      *BLANKS
040900050204     C                   MOVEL     TBLUNI        DS3a
041000050204    3C     §3aim0        IFeq      'S'
041100050204     C                   ADD       1             K
041200050204     C                   MOVEL     TBLKEY        im0(k)
041300050204    3C                   ENDIF
041400050204     c     §3asva        ifne      *blanks
041500050204     C                   ADD       1             Kk
041600050204     C                   MOVEL     TBLKEY        cbo(kk)
041700050204     C                   MOVEL     §3asva        cbv(kk)
041800050204    3c                   endif
041900050204     c**
042000050204    2C                   ENDIF
042100050207     C     KTAB          READE     TABEL00f                               07
042200050204    1C                   ENDDO
042300050204     C                   ENDSR
