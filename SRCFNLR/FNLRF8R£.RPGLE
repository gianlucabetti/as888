000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400041007      *  Descrizione   :  gestione bancali affidati rientrati
000500041007      *                   colli ritirati
000600000000      *****************************************************************
000700041006     Ffnlrf8d   CF   E             WORKSTN
000800041006     Ffiqdt01l  uf   e           k disk
000900041006     Fazorg01l  if   e           k disk
001000050207     F*nfvv01l  if   e           k disk
001100050119     Ffiapd01l  if   e           k disk
001200000000      *
001300940926     D*
001400000000     D KPJBA         E DS
001500000000     D CNCR80        E DS
001600000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700940926     D*
001800940926     D* DS PER TRUL06R - CARICAMENTO £1
001900940926     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002000940926     D  LIN                    1     90  0
002100940926     D                                     DIM(30)                              SKI COMODO
002200041008     d fnlrf8ds      e ds
002300041008     d dslr01        e ds
002400940926     D*
002500000000     D WLBDAT          DS
002600940926     D  G02DAT                 1      8  0
002700940926     D  G02INV                 9     16  0
002800940926     D  G02ERR                17     17
002900940926     D  G02TGI                18     22  0
003000000000      *
003100000000     C     *ENTRY        PLIST
003200000000     C                   PARM                    KPJBA
003300041008     c                   if        kpjbu = *blank
003400041008     c                   clear                   fnlrf8ds
003500041008     c                   else
003600041008     c                   movel     kpjbu         fnlrf8ds
003700050204     c                   move      'X'           richiam           1
003800041008     c                   end
003900041006
004000041006     C                   Z-ADD     1             CODUT
004100041006     C                   CALL      'X§PARUT'
004200041006     C                   PARM                    UT§DSE
004300041006     C                   MOVEL     RAGUT         RSUT             20
004400041006     C                   MOVEL     RAGUT         RSUT2            20
004500041006     C                   MOVEL     REC80         CNCR80
004600041006
004700041006     c     kqdt          klist
004800041006     c                   kfld                    kpo               3 0
004900041006     c                   kfld                    kdis              6 0
005000041006
005100050119
005200050119     c     kapd          klist
005300050119     c                   kfld                    tipo              1
005400050119     c                   kfld                    qdtpdr
005500050119     c                   move      'A'           tipo
005600050119
006300041006     C                   time                    w0140            14 0
006400041006     C                   move      w0140         udate8            8 0
006500041006     C                   movel     W0140         hhmm              4 0          *ORA/MINUTI
006600041006     C*
006700041006     C                   z-add     udate8        g02dat                         *GG/MM/AAAA
006800041006     C                   move      *blank        g02err
006900041006     C                   call      'XSRDA8'
007000041006     C                   parm                    wlbdat
007100041006     C                   z-add     g02inv        dateu8            8 0          *AAAA/MM/GG
007200000000      *
007300000000      *--------------------------------------------------------------*
007400000000      *             M A I N      L I N E
007500000000      *--------------------------------------------------------------*
007600000000      *
007700041006     C                   exsr      defcam                                       INIZIALIZZ.
007800050119     c     nuovo         tag
007900050119     c                   clear                   viddes
008000041008     c                   setoff                                       3132
008100041008      * verifico se inserita una modalità come parametro nel caso
008200041008      * accendo indicatore di protezione dei campi non richiesti
008300041008     c     rf8mod        comp      'A'                                    32
008400041008     c     rf8mod        comp      'R'                                    31
008500000000      *
008600000000      * Inizializzo i campi a video
008700041006     C                   clear                   desfil                         COD.FILIALE
008800041006     C                   clear                   vidfbaf                        COD.FILIALE
008900041006     C                   clear                   vidfbar                        COD.FILIALE
009000041006     C                   clear                   vidfcor                        COD.FILIALE
009100041008     C                   z-add     rf8fgs        vidfil                         COD.FILIALE
009200041008     C                   move      rf8dis        viddis                         1' COD.PADRONC.
009300041008     C                   move      rf8dis        viddisn           6 0          1' COD.PADRONC.
009400041006     C                   z-add     0             vidbaf                         2' COD.PADRONC.
009500041006     C                   z-add     0             vidbar                         2' COD.PADRONC.
009600041006     C                   z-add     0             vidcor                         2' COD.PADRONC.
009700041008      * se ho ricevuto i parametri dall'esterno eseguo la chain per reperire
009800041008      * il rekord ed impostare i campi
009900041008     c                   if        rf8fgs > 0
010000041008     c                   z-add     vidfil        kpo
010100041008     c                   z-add     rf8dis        kdis
010200041008     c     kqdt          chain     fiqdt01l
010300041008     c                   if        %found(fiqdt01l)
010400041008     c                   move      qdtfnbaf      vidfbaf
010500041008     c                   z-add     qdtnbnaf      vidbaf
010600041008     c                   move      qdtfnbri      vidfbar
010700041008     c                   z-add     qdtnbnri      vidbar
010800041008     c                   move      qdtfncrit     vidfcor
010900041008     c                   z-add     qdtnclrit     vidcor
011000050225     c                   else
011100050225     c                   goto      fine
011200041008     c                   endif
011300050119     c                   else
011400050119     C                   EVAL      VidFil = SimPOU
011500050119     c* proteggo i campi con i valori
011600050119     c                   seton                                        33
011700041008     c                   endif
011800080313     c* se richiamato con parametri esegue controlli e decodifiche
011900080313     c                   if        richiam <> *blank
012000080313     c                   exsr      contr
012100080313     c                   endif
012200000000      *
012300000000      * Emetto il formato video
012400041006     C     emetto        tag
012500041006     C                   setoff                                       90
012600041006     C                   exfmt     lrf8d1
012700000000      *
012800041006     C   kc              goto      fine                                         F3=USCITA
012900000000      *
013000000000      * Eseguo i  controlli sui campi immessi a video
013100041006     C                   exsr      contr
013200000000      *
013300020308      * Se non vi sono errori ma non e' premuto F6 emetto video
013400020308     C  N90
013500041006     CanNkf              seton                                          90      F6=CONFERMA
013600050119     c                   setoff                                       33
013700000000      *
013800000000      * Per errore generico riemetto il formato video
013900041006     C   90              goto      emetto
014000000000      *
014100041006      * Aggiorno gli archivi per i campi variati
014200020308     c                   if        *inkf
014300041006      * affidati
014400041007     c                   if        vidbaf <> qdtnbnaf or
014500041007     c                             vidfbaf <> qdtfnbaf
014600041006     c                   z-add     vidbaf        qdtnbnaf
014700041006     c                   z-add     hhmm          qdtoraaf
014800041006     c                   z-add     dateu8        qdtdaiaf
014801110329     c                   move      'S'           vidfbaf
014900041006     c                   move      vidfbaf       qdtfnbaf
015000041006     c                   endif
015100041006      * rientrati
015200041007     c                   if        vidbar <> qdtnbnri or
015300041007     c                             vidfbar <> qdtfnbri
015400041006     c                   z-add     vidbar        qdtnbnri
015500041006     c                   z-add     hhmm          qdtorari
015600041006     c                   z-add     dateu8        qdtdairi
015700041006     c                   move      vidfbar       qdtfnbri
015800041006     c                   endif
015900041006      * colli ritirati
016000041007     c                   if        vidcor <> qdtnclrit or
016100041007     c                             vidfcor <> qdtfncrit
016200041006     c                   z-add     vidcor        qdtnclrit
016300041006     c                   move      vidfcor       qdtfncrit
016400020308     c                   endif
016500000000      *
016600050225     c                   if        %found(fiqdt01l)
016700041007     c                   update    fiqdt000
016800050225     c                   endif
016900050204      *
017000050204     c                   if        richiam <> *blank
017100050204     c                   goto      fine
017200050204     c                   endif
017300050204
017400050119     c                   goto      nuovo
017500041006     c                   endif
017600041006
017700000000     C     FINE          TAG
017800000000     C                   SETON                                        LR
017900000000      *---------------------------------------------------------------*
018000000000      * Definizioni/dichiarazioni iniziali                            *
018100000000      *---------------------------------------------------------------*
018200041006     C     defcam        begsr
018300940926     C***
018400940926     C* CARICO TABELLA FILIALI GESTITE £1
018500940926     C***
018600940926     C                   CLEAR                   DSUL06
018700940926     C                   MOVE      '£1'          D06COD
018800960913     C     SIMFEL        IFGT      0
018900940926     C                   MOVEL     SIMFEL        D06KEY
019000960913     C                   ENDIF
019100940926     C                   MOVEL     DSUL06        KPJBU
019200940926     C*
019300940926     C                   CALL      'TRUL06R'
019400940926     C                   PARM                    KPJBA
019500940926     C                   MOVEL     KPJBU         DSUL06
019600020402     C***
019700000000     C                   ENDSR
019800000000      *--------------------------------------------------------------*
019900000000      * Routine per i controlli sui campi del formato video          *
020000000000      *--------------------------------------------------------------*
020100000000     C     CONTR         BEGSR
020200000000      *
020300050119     c                   movea     '000000000'   *in(41)
020400000000      * Verifico il codice filiale
020500000000     C                   MOVE      *BLANKS       DESFIL
020600000000     C     VIDFIL        IFGT      0
020700000000     C     VIDFIL        CHAIN     AZORG01L                           41
020800000000     C  N41              MOVEL     ORGDES        DESFIL
020900000000     C   41              SETON                                        90
021000000000     C   41              GOTO      ENDCTL
021100020308     c                   else
021200020308     C                   SETON                                        9041
021300020308     C                   GOTO      ENDCTL
021400000000     C                   ENDIF
021500041006     c                   if        %subst(knmus:1:3) <>'EDP'
021600041006     C     VIDFIL        LOOKUP    lin                                    99
021700041006     C  N99              SETON                                        42  90
021800041006     C  N99              GOTO      ENDCTL
021900041006     C                   ENDIF
022000000000      *
022100041008     C     viddis        ifne      *zeros
022200041008     C     viddis        andne     *blanks
022300041008     C     '?'           scan      viddis                                 95
022400041008     C     *in95         ifeq      '1'
022500041008     C                   clear                   dslr01
022600041008     C                   MOVEL     'FNLRF8R'     DLRPGM
022700041008     C                   MOVEL     '2'           DLRTFV
022800041008     C                   Z-ADD     4             DLRNPG
022900041008     C                   Z-ADD     vidfil        DLRFGS
023000041008     C                   MOVEL     'S'           DLRRIC
023100041008     C                   Z-ADD     dateu8        DLRADA
023200041008     C                   MOVEL     DSLR01        KPJBU
023300041008     C                   CALL      'FNLR02R'
023400041008     C                   PARM                    KPJBA
023500041008     C                   MOVEL     KPJBU         DSLR01
023600041008     C                   MOVE      DLRNFV        Viddis
023700041008     c                   endif
023800041008     c                   endif
023900041008     C                   MOVE      viddis        Viddisn
024000041008      * Controlli di congruenza
024100041008      * Se immesso il numero distinta
024200041008     C     viddis        IFeq      *zero
024300041008     C     viddis        oreq      *blank
024400041008     C                   SETON                                        9045
024500041008     C   90              GOTO      ENDCTL
024600041008     C                   ENDIF
024700041007      * leggo il file solo se è stata inserita la distinta a video e solo
024800041007      * una volta per ogni distinta selezionata altrimenti sporco i campi
024900041008     c                   if        viddisn <> kdis or vidfil <> kpo
025000041006     c                   z-add     vidfil        kpo
025100041008     c                   z-add     viddisn       kdis
025200041006     c     kqdt          chain     fiqdt01l
025300041006     c                   if        %found(fiqdt01l)
025400041006     c                   move      qdtfnbaf      vidfbaf
025500041006     c                   z-add     qdtnbnaf      vidbaf
025600041007     c                   move      qdtfnbri      vidfbar
025700041006     c                   z-add     qdtnbnri      vidbar
025800041006     c                   move      qdtfncrit     vidfcor
025900041006     c                   z-add     qdtnclrit     vidcor
026000041006     c                   else
026100041013     c                   move      *zeros        viddis
026200041013     c                   move      *zeros        viddisn
026300041013     c                   move      *zeros        kdis
026400041006     c                   seton                                        9044
026500041006     C   90              goto      endctl
026600041006     c                   endif
026700041007     c                   endif
026800050119      *decodifica autista della distinta
026900050119     c     kapd          chain     fiapd01l
027000050119     c                   if        %found(fiapd01l)
027100050119     c                   movel     apdrsc        viddes
027104110329     c                   if        vidfbaf = *blank and apdptp  <> *blank
027105110304     c                   move      apdptp        vidbaf
027107110329     c                   end
027200050119     c                   else
027300050119     c                   clear                   viddes
027400050119     c                   end
027500050119      *
028500050119     c                   setoff                                       33
028600050119      *
028700041007      *verifico la congruenza dei flag relativi ai campi specifici
028800041007      * controllo solo se il valore è maggiore di 0 che il flag sia
028900041007      * impostato correttamente
029000110329     c                   if        vidbaf <> 0
029100110329     c                   move      'S'           vidfbaf
029200110329     c                   endif
029300000000      *
029400050119     c                   if        vidbar <> 0
029500050119     c                   move      'S'           vidfbar
029600041007     c                   endif
029700041007      *
029800050119     c                   if        vidcor <> 0
029900050119     c                   move      'S'           vidfcor
030000041007     c                   endif
030100041007      *
030200000000     C     ENDCTL        ENDSR
