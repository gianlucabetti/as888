000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200110112      * TISTI1R *----------------------------------------------------*
000300110112      *         -Gestione statistiche clenti Roma dopo fatturazione
000400940915      *--------------------------------------------------------------*
000500110112     FTIsti1D   CF   E             WORKSTN
000600110112     f                                     sfile(sti1s03 : nrr1)
000700110112     FTNTBE01L  uf a E           k DISK
000800110112     Ftabel00f  if   E           k DISK
000900110112     Fcnaco00f  if   E           k DISK
001000110112     Fcnclp00f  if   E           k DISK
001100020805      *
001200020805      * DEFINIZIONE SCHIERE
001300960302     D*
001400080529     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001500110112     D*
001600040917     D KPJBA         E DS
001700110112     d TIBS02DS      E DS
001800110112      * - Parametri per richiamo al pgm di controllo profilo utenti
001900110112     d TIBS34ds      e ds
002000110112      * - Ds di riferimento al file esterno AzUte00F
002100110112     d AZUTEds       e ds                  ExtName(AzUte00F)
002200110112      * - Ds per dati organigramma
002300110112     d dDatiUte      e ds
002400110112      *
002500080529     d
002600080529     D WLBDAT          DS                  INZ
002700080529     D  G02DAT                 1      8  0
002800080529     D  G02INV                 9     16  0
002900080529     D  G02ERR                17     17
003000080529     D  G02TGI                18     22  0
003100080529     d
003200050103     d Dataiso         s               d   datfmt(*iso)
003300050103     d Dataeur         s               d   datfmt(*eur)
003400080529     d datadmy         s               d   datfmt(*dmy)
003500050103     d
003600080529     d nrr1            s              4  0 inz
003700110112     D kcod            S                   LIKE(tbecod) inz('SFR')
003800110112     D kkey1           S                   LIKE(tbeke1)
003900110112     D kkey2           S                   LIKE(tbeke2)
004000110112     D kkut            S                   LIKE(acokut)
004100110112     D kksc            S                   LIKE(acoksc)
004200110112     d c_SflPag        c                   const(18)
004300110112     d wpag            s              6  0
004400110112     d wresto          s              3  0
004500110112     d $Fine           s              1
004600041220     d
004700050103     C**********************************************************************
004800080529     c
004900080529     c     inizio        tag
005000110112     c                   clear                   v1cragr
005100040917     c
005200040917     C     FOR01         TAG
005300000828      *
005400110112     C                   EXFMT     sti1D01
005500080529     C                   SETOFF                                       289001
005600080529     c                   clear                   rivisual          1
005700110112     c                   clear                   v1cmsg
005800040917     C* F3 - FINE
005900040917     C   KC              GOTO      FINE
006000080529     c
006100041220      *  CONTROLLI
006200040917     C                   EXSR      CTRD01
006300110112     c
006400080529     c                   if        *in90
006500080529     c                   goto      for01
006600080529     c                   endif
006700110112     c
006800110112     c                   exsr      Carsfl
006900110112     c                   exsr      Gessfl
007000110112     c
007100110112     c                   if        $fine='1'
007200110112     c                   goto      fine
007300110112     c                   else
007400110112     c                   goto      inizio
007500110112     c                   endif
007600080529     c
007700900515      *
007800000000     C     FINE          TAG
007900960129     C*
008000000000     C                   SETON                                        LR
008100080529      *-----------------------------------------------------***********
008200110112      * Caricamento sfl
008300080529      *-----------------------------------------------------***********
008400110112     C     CarSFL        BEGSR
008500080529     C* PULIZIA SFL
008600080529     C                   SETON                                        3031
008700110112     C                   WRITE     sti1C03
008800080529     C                   SETOFF                                       3031
008900080529     c                   clear                   nrr1
009000080529     c                   clear                   totnrr            5 0
009100080529     c
009200110112      * caricamento subfile per codice raggruppamento
009300110112     c
009400110112     c                   eval      kkey1=v1cragr
009500110112     c
009600110112     c     ktbe          setll     tntbe01l
009700110112     c     ktbe          reade(n)  tntbe01l
009800110112     c
009900110112    1c                   dow       not %eof(tntbe01l)
010000110112     c
010100110112     c                   movel     tbeuni        v3cksc
010200110112     c* Chain su cnaco per la ragione sociale
010300110112    2c                   if        v3cksc>*zeros
010400110112     c                   movel     v3cksc        kksc
010500110112     c     kaco          chain     cnaco00f
010600110112    3c                   if        %found(cnaco00f)
010700110112     c                   movel     acorag        v3dksc
010800110112    3c                   endif
010900110112     c
011000110112     c     kaco          chain     cnclp00f
011100110112    3c                   if        %found(cnclp00f)
011200110112     c                   movel     clpage        v3ccmm
011300110112    3c                   endif
011400110112     c
011500110112    2c                   endif
011600110112     c*
011700110112     c                   add       1             nrr1
011800110112     c                   write     sti1s03
011900110112     c
012000110112     c     ktbe          reade(n)  tntbe01l
012100110112     c                   enddo
012200110112     c
012300110112     c                   exsr      righevuote
012400110112     c
012500110112     c                   ENDSR
012600080529      *-----------------------------------------------------***********
012700080529      * controllo data di elaborazione
012800080529      *-----------------------------------------------------***********
012900080529     C     Ctrd01        BEGSR
013000110112     C*
013100110112     c     '?'           scan      v1cragr                                90
013200110112     c                   if        *in90
013300110112     C                   MOVEL     'R'           t02mod
013400110112     C                   MOVEL     knsif         t02sif
013500110112     C                   MOVEL     'SFR'         t02cod
013600110112      *
013700110112     C                   CALL      'TIBS02R'
013800110112     C                   PARM                    KPJBA
013900110112     C                   PARM                    TIBS02DS
014000110112      *
014100110112     C                   IF        T02err = *BLANKS
014200110112     C                   movel     T02ke1        v1cragr
014300110112     c                   else
014400110112     c                   clear                   v1cragr
014500110112     c                   endif
014600110112     c
014700110112     c                   leavesr
014800110112     c                   endif
014900110112     c
015000110112     c*  controllo
015100110112     C                   MOVEL     'C'           t02mod
015200110112     C                   MOVEL     knsif         t02sif
015300110112     C                   MOVEL     'SFR'         t02cod
015400110112     C                   MOVEL     v1cragr       t02ke1
015500110112      *
015600110112     C                   CALL      'TIBS02R'
015700110112     C                   PARM                    KPJBA
015800110112     C                   PARM                    TIBS02DS
015900110112      *
016000110112     C                   IF        T02err<> *BLANKS
016100110112     c                   seton                                        2890
016200110112     c                   eval      v1cmsg=msg(1)
016300110112     c                   leavesr
016400110112     c                   endif
016500110112     c*
016600080529     c                   ENDSR
016700110112      *-----------------------------------------------------***********
016800110112     c*  Carico riighe vuote del sfl
016900110112      *-----------------------------------------------------***********
017000110112     c     Righevuote    BEGSR
017100110112     c* Carico le righe vuote fino a riempimento di pagina
017200110112    1c                   if        nrr1>=c_sflpag
017300110112     c                   eval      wpag=%div(nrr1:c_sflPag)
017400110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
017500110112    2c                   if        wresto>0
017600110112     c                   eval      c01csr=nrr1
017700110112     c                   else
017800110112     c                   eval      c01csr=(wpag*c_sflpag)+1
017900110112    2c                   endif
018000110112     c
018100110112   x1c                   else
018200110112     c                   clear                   wpag
018300110112     c                   eval      wresto=nrr1
018400110112     c                   eval      c01csr=nrr1
018500110112    2c                   if        c01csr=0
018600110112     c                   eval      c01csr=1
018700110112    2c                   endif
018800110112    1c                   endif
018900110112     C
019000110112     C                   DOW       wresto<c_sflpag
019100110112     c                   clear                   v3cksc
019200110112     c                   clear                   v3dksc
019300110112     c                   clear                   v3ccmm
019400110112     c                   clear                   v3dcmm
019500110112     c*
019600110112     c                   eval      wresto=wresto+1
019700110112     c                   add       1             nrr1
019800110112     c                   write     sti1s03
019900110112     c                   enddo
020000110112     c
020100110112     c                   z-add     nrr1          totnrr
020200110112     c                   seton                                        33
020300110112     c                   ENDSR
020400110112      *-----------------------------------------------------***********
020500110112     c*  Gestione SFL
020600110112      *-----------------------------------------------------***********
020700110112     c     Gessfl        BEGSR
020800110112     c
020900110112     c     for02         tag
021000110112     c                   exfmt     sti1c03
021100110112     C                   SETOFF                                       289001
021200110112     c                   clear                   v1cmsg
021300110112     C* F3 - FINE
021400110112     C   KC              eval      $fine='1'
021500110112     c   kc              leavesr
021600110112     c*
021700110112     c                   goto      for02
021800110112     c
021900110112     c                   ENDSR
022000110112      *****************************************************************
022100110112     C     *INZSR        BEGSR
022200110112      *
022300110112     C     *ENTRY        PLIST
022400110112     C                   PARM                    KPJBA
022500110112      *
022600110112      *
022700110112      * accesso tntbe
022800110112     c     ktbe2         klist
022900110112     C                   kfld                    kcod
023000110112     C                   kfld                    kkey1
023100110112     C                   kfld                    kkey2
023200110112     c     ktbe          klist
023300110112     C                   kfld                    kcod
023400110112     C                   kfld                    kkey1
023500110112     c     kaco          klist
023600110112     C                   kfld                    kkut
023700110112     C                   kfld                    dutkci
023800110112     C                   kfld                    kksc
023900110112     c
024000110112     c                   exsr      sr_DatiJob
024100110112     c*
024200110112      *
024300110112     C                   ENDSR
024400110112      *---------------------------------------------------------------*
024500110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
024600110112      *---------------------------------------------------------------*
024700110112     c     sr_DatiJob    BEGSR
024800110112      *
024900110112     c     *dtaara       define    §azute        AZUTEds
025000110112     c     *dtaara       define    §datiute      dDatiUte
025100110112      *
025200110112     c                   in(E)     *dtaara
025300110112     c                   IF        %Error or RSUT = *blanks
025400110112     c                   clear                   TIBS34ds
025500110112     c                   call      'TIBS34R'
025600110112     c                   parm                    TIBS34ds
025700110112     c                   in        *dtaara
025800110112     c                   ENDIF
025900110112      *
026000110112     c                   ENDSR
026100020805      **************************************************************************
026200960302** SCHIERA MSG - MESSAGGI DI ERRORE
026300110112Raggruppamento errato
