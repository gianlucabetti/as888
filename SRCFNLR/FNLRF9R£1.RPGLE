000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400041007      *  Descrizione   :  gestione bancali affidati rientrati
000500041007      *                   colli ritirati
000600000000      *****************************************************************
000700041006     Ffnlrf8d   CF   E             WORKSTN
000800041006     Ffiqdt01l  uf   e           k disk
000900041006     Fazorg01l  if   e           k disk
001000000000      *
001100940926     D*
001200000000     D KPJBA         E DS
001300000000     D CNCR80        E DS
001400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500940926     D*
001600940926     D* DS PER TRUL06R - CARICAMENTO £1
001700940926     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001800940926     D  LIN                    1     90  0
001900940926     D                                     DIM(30)                              SKI COMODO
002000041008     d fnlrf8ds      e ds
002100041008     d dslr01        e ds
002200940926     D*
002300000000     D WLBDAT          DS
002400940926     D  G02DAT                 1      8  0
002500940926     D  G02INV                 9     16  0
002600940926     D  G02ERR                17     17
002700940926     D  G02TGI                18     22  0
002800000000      *
002900000000     C     *ENTRY        PLIST
003000000000     C                   PARM                    KPJBA
003100041008     c                   if        kpjbu = *blank
003200041008     c                   clear                   fnlrf8ds
003300041008     c                   else
003400041008     c                   movel     kpjbu         fnlrf8ds
003500041008     c                   end
003600041006
003700041006     C                   Z-ADD     1             CODUT
003800041006     C                   CALL      'X§PARUT'
003900041006     C                   PARM                    UT§DSE
004000041006     C                   MOVEL     RAGUT         RSUT             20
004100041006     C                   MOVEL     RAGUT         RSUT2            20
004200041006     C                   MOVEL     REC80         CNCR80
004300041006
004400041006     c     kqdt          klist
004500041006     c                   kfld                    kpo               3 0
004600041006     c                   kfld                    kdis              6 0
004700041006
004800041006     C                   time                    w0140            14 0
004900041006     C                   move      w0140         udate8            8 0
005000041006     C                   movel     W0140         hhmm              4 0          *ORA/MINUTI
005100041006     C*
005200041006     C                   z-add     udate8        g02dat                         *GG/MM/AAAA
005300041006     C                   move      *blank        g02err
005400041006     C                   call      'XSRDA8'
005500041006     C                   parm                    wlbdat
005600041006     C                   z-add     g02inv        dateu8            8 0          *AAAA/MM/GG
005700000000      *
005800000000      *--------------------------------------------------------------*
005900000000      *             M A I N      L I N E
006000000000      *--------------------------------------------------------------*
006100000000      *
006200041006     C                   exsr      defcam                                       INIZIALIZZ.
006300041008     c                   setoff                                       3132
006400041008      * verifico se inserita una modalità come parametro nel caso
006500041008      * accendo indicatore di protezione dei campi non richiesti
006600041008     c     rf8mod        comp      'A'                                    32
006700041008     c     rf8mod        comp      'R'                                    31
006800000000      *
006900000000      * Inizializzo i campi a video
007000041006     C                   clear                   desfil                         COD.FILIALE
007100041006     C                   clear                   vidfbaf                        COD.FILIALE
007200041006     C                   clear                   vidfbar                        COD.FILIALE
007300041006     C                   clear                   vidfcor                        COD.FILIALE
007400041008     C                   z-add     rf8fgs        vidfil                         COD.FILIALE
007500041008     C                   move      rf8dis        viddis                         1' COD.PADRONC.
007600041008     C                   move      rf8dis        viddisn           6 0          1' COD.PADRONC.
007700041006     C                   z-add     0             vidbaf                         2' COD.PADRONC.
007800041006     C                   z-add     0             vidbar                         2' COD.PADRONC.
007900041006     C                   z-add     0             vidcor                         2' COD.PADRONC.
008000041008      * se ho ricevuto i parametri dall'esterno eseguo la chain per reperire
008100041008      * il rekord ed impostare i campi
008200041008     c                   if        rf8fgs > 0
008300041008     c                   z-add     vidfil        kpo
008400041008     c                   z-add     rf8dis        kdis
008500041008     c     kqdt          chain     fiqdt01l
008600041008     c                   if        %found(fiqdt01l)
008700041008     c                   move      qdtfnbaf      vidfbaf
008800041008     c                   z-add     qdtnbnaf      vidbaf
008900041008     c                   move      qdtfnbri      vidfbar
009000041008     c                   z-add     qdtnbnri      vidbar
009100041008     c                   move      qdtfncrit     vidfcor
009200041008     c                   z-add     qdtnclrit     vidcor
009300041008     c                   endif
009400041008     c                   endif
009500000000      *
009600000000      * Emetto il formato video
009700041006     C     emetto        tag
009800041006     C                   setoff                                       90
009900041006     C                   exfmt     lrf8d1
010000000000      *
010100041006     C   kc              goto      fine                                         F3=USCITA
010200000000      *
010300000000      * Eseguo i  controlli sui campi immessi a video
010400041006     C                   exsr      contr
010500000000      *
010600020308      * Se non vi sono errori ma non e' premuto F6 emetto video
010700020308     C  N90
010800041006     CanNkf              seton                                          90      F6=CONFERMA
010900000000      *
011000000000      * Per errore generico riemetto il formato video
011100041006     C   90              goto      emetto
011200000000      *
011300041006      * Aggiorno gli archivi per i campi variati
011400020308     c                   if        *inkf
011500041006      * affidati
011600041007     c                   if        vidbaf <> qdtnbnaf or
011700041007     c                             vidfbaf <> qdtfnbaf
011800041006     c                   z-add     vidbaf        qdtnbnaf
011900041006     c                   z-add     hhmm          qdtoraaf
012000041006     c                   z-add     dateu8        qdtdaiaf
012100041006     c                   move      vidfbaf       qdtfnbaf
012200041006     c                   endif
012300041006      * rientrati
012400041007     c                   if        vidbar <> qdtnbnri or
012500041007     c                             vidfbar <> qdtfnbri
012600041006     c                   z-add     vidbar        qdtnbnri
012700041006     c                   z-add     hhmm          qdtorari
012800041006     c                   z-add     dateu8        qdtdairi
012900041006     c                   move      vidfbar       qdtfnbri
013000041006     c                   endif
013100041006      * colli ritirati
013200041007     c                   if        vidcor <> qdtnclrit or
013300041007     c                             vidfcor <> qdtfncrit
013400041006     c                   z-add     vidcor        qdtnclrit
013500041006     c                   move      vidfcor       qdtfncrit
013600020308     c                   endif
013700000000      *
013800041007     c                   update    fiqdt000
013900041006     c                   endif
014000041006
014100000000     C     FINE          TAG
014200000000     C                   SETON                                        LR
014300000000      *---------------------------------------------------------------*
014400000000      * Definizioni/dichiarazioni iniziali                            *
014500000000      *---------------------------------------------------------------*
014600041006     C     defcam        begsr
014700940926     C***
014800940926     C* CARICO TABELLA FILIALI GESTITE £1
014900940926     C***
015000940926     C                   CLEAR                   DSUL06
015100940926     C                   MOVE      '£1'          D06COD
015200960913     C     SIMFEL        IFGT      0
015300940926     C                   MOVEL     SIMFEL        D06KEY
015400960913     C                   ENDIF
015500940926     C                   MOVEL     DSUL06        KPJBU
015600940926     C*
015700940926     C                   CALL      'TRUL06R'
015800940926     C                   PARM                    KPJBA
015900940926     C                   MOVEL     KPJBU         DSUL06
016000020402     C* VEDO SE SONO SIMFEL O REMOTO
016100020402     C***
016200020701     C                   EVAL      VidFil = SimPOU
016300000000     C                   ENDSR
016400000000      *--------------------------------------------------------------*
016500000000      * Routine per i controlli sui campi del formato video          *
016600000000      *--------------------------------------------------------------*
016700000000     C     CONTR         BEGSR
016800000000      *
016900041006     c                   movea     '00000000'    *in(41)
017000000000      * Verifico il codice filiale
017100000000     C                   MOVE      *BLANKS       DESFIL
017200000000     C     VIDFIL        IFGT      0
017300000000     C     VIDFIL        CHAIN     AZORG01L                           41
017400000000     C  N41              MOVEL     ORGDES        DESFIL
017500000000     C   41              SETON                                        90
017600000000     C   41              GOTO      ENDCTL
017700020308     c                   else
017800020308     C                   SETON                                        9041
017900020308     C                   GOTO      ENDCTL
018000000000     C                   ENDIF
018100041006     c                   if        %subst(knmus:1:3) <>'EDP'
018200041006     C     VIDFIL        LOOKUP    lin                                    99
018300041006     C  N99              SETON                                        42  90
018400041006     C  N99              GOTO      ENDCTL
018500041006     C                   ENDIF
018600000000      *
018700041008     C     viddis        ifne      *zeros
018800041008     C     viddis        andne     *blanks
018900041008     C     '?'           scan      viddis                                 95
019000041008     C     *in95         ifeq      '1'
019100041008     C                   clear                   dslr01
019200041008     C                   MOVEL     'FNLRF8R'     DLRPGM
019300041008     C                   MOVEL     '2'           DLRTFV
019400041008     C                   Z-ADD     4             DLRNPG
019500041008     C                   Z-ADD     vidfil        DLRFGS
019600041008     C                   MOVEL     'S'           DLRRIC
019700041008     C                   Z-ADD     dateu8        DLRADA
019800041008     C                   MOVEL     DSLR01        KPJBU
019900041008     C                   CALL      'FNLR02R'
020000041008     C                   PARM                    KPJBA
020100041008     C                   MOVEL     KPJBU         DSLR01
020200041008     C                   MOVE      DLRNFV        Viddis
020300041008     c                   endif
020400041008     c                   endif
020500041008     C                   MOVE      viddis        Viddisn
020600041008      * Controlli di congruenza
020700041008      * Se immesso il numero distinta
020800041008     C     viddis        IFeq      *zero
020900041008     C     viddis        oreq      *blank
021000041008     C                   SETON                                        9045
021100041008     C   90              GOTO      ENDCTL
021200041008     C                   ENDIF
021300041007      * leggo il file solo se è stata inserita la distinta a video e solo
021400041007      * una volta per ogni distinta selezionata altrimenti sporco i campi
021500041008     c                   if        viddisn <> kdis or vidfil <> kpo
021600041006     c                   z-add     vidfil        kpo
021700041008     c                   z-add     viddisn       kdis
021800041006     c     kqdt          chain     fiqdt01l
021900041006     c                   if        %found(fiqdt01l)
022000041006     c                   move      qdtfnbaf      vidfbaf
022100041006     c                   z-add     qdtnbnaf      vidbaf
022200041007     c                   move      qdtfnbri      vidfbar
022300041006     c                   z-add     qdtnbnri      vidbar
022400041006     c                   move      qdtfncrit     vidfcor
022500041006     c                   z-add     qdtnclrit     vidcor
022600041006     c                   else
022700041006     c                   seton                                        9044
022800041006     C   90              goto      endctl
022900041006     c                   endif
023000041007     c                   endif
023100041007      *verifico la congruenza dei flag relativi ai campi specifici
023200041007      * controllo solo se il valore è maggiore di 0 che il flag sia
023300041007      * impostato correttamente
023400041007     c                   if        vidbaf <> 0 and vidfbaf <> 'S'
023500041007     c                   seton                                        9046
023600041007     C   90              goto      endctl
023700041007     c                   endif
023800000000      *
023900041007     c                   if        vidbar <> 0 and vidfbar <> 'S'
024000041007     c                   seton                                        9047
024100041007     C   90              goto      endctl
024200041007     c                   endif
024300041007      *
024400041007     c                   if        vidcor <> 0 and vidfcor <> 'S'
024500041007     c                   seton                                        9048
024600041007     C   90              goto      endctl
024700041007     c                   endif
024800041007      *
024900000000     C     ENDCTL        ENDSR
