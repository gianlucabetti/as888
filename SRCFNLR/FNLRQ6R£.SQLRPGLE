000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400081008      *  Descrizione   :  conferma ricezione stampa tariffe autisti AFF/DEF
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700000000     FAZORG01L  IF   E           K DISK
000900130329     Ffnfvv01l  if   e           k disk
001100130329     Ffnlrq6d   CF   E             WORKSTN
001200000000      *
001300940926     D*
001400000000     D KPJBA         E DS
002100940926     D*
002200000000     D WLBDAT          DS
002300940926     D  G02DAT                 1      8  0
002400940926     D  G02INV                 9     16  0
002500940926     D  G02ERR                17     17
002600940926     D  G02TGI                18     22  0
002700081008     d dataiso         s               d   datfmt(*iso)
002800081008     d dataeur         s               d   datfmt(*eur)
002801130329     d knpg            s                   like(fvvnpg)
002802130329     d knfv            s                   like(fvvnfv) inz(4)
002803130329     d kfil            s                   like(fvvfgs)
004100081008      *__________________________________________________________________
004200000000     C     *ENTRY        PLIST
004300000000     C                   PARM                    KPJBA
004400081008
004401130329     c     kfvv          klist
004402130329     c                   kfld                    knpg
004403130329     c                   kfld                    knfv
004404130329     c                   kfld                    kfil
004405130329      *
004500081008     C                   time                    w0120            14 0
004600081008     C                   move      w0120         wdat              8 0
004700081008      * data da impostare su data stampa e convalida se lancio = 'D'
004800081008     c                   move      wdat          dataeur
004900081008     c                   move      dataeur       dataiso
006000000000      *--------------------------------------------------------------*
006100000000      *             M A I N      L I N E
006200000000      *--------------------------------------------------------------*
007200000000      * Inizializzo i campi a video
007300130329     C                   Z-ADD     *ZERO         VIDFIL
007400130329     C                   Z-ADD     *ZERO         VIDDAt
007401130329     C                   Z-ADD     *ZERO         VIDDAtn
007500130329     C                   Z-ADD     0             viddis
007600000000      *
007700000000      * Emetto il formato video
007800130329     C                   do        *hival
007900020308     C                   SETOFF                                       90
008600130329     C                   EXFMT     lrq6D01
008800000000     C   KC              GOTO      FINE                                         F3=USCITA
012300090514     C                   EXSR      CONTR
013200000000      * Per errore generico riemetto il formato video
013300130329     C   90              iter
013500081008      * Richiamo programma di conferma
013600130329     c                   if        *inkf
013601130329     c                   exsr      aggiorna
014800091007     c                   end
015100081008     c                   endif
015200020308     c                   endif
015300000000      *
015400000000     C     FINE          TAG
015500000000     C                   SETON                                        LR
016800000000      *--------------------------------------------------------------*
016900000000      * Routine per i controlli sui campi del formato video          *
017000000000      *--------------------------------------------------------------*
017100000000     C     CONTR         BEGSR
017200000000      *
017300130329     c                   movea     '0000000'     *in(41)
020300000000      * Verifico il codice filiale
020400000000     C                   MOVE      *BLANKS       DESFIL
020500000000     C     VIDFIL        IFGT      0
020600000000     C     VIDFIL        CHAIN     AZORG01L                           41
020700000000     C  N41              MOVEL     ORGDES        DESFIL
020800000000     C   41              SETON                                        90
020900000000     C   41              GOTO      ENDCTL
021000020308     c                   else
021100020308     C                   SETON                                        9041
021200020308     C                   GOTO      ENDCTL
021300000000     C                   ENDIF
021400000000      *
021500000000      * Controlli di congruenza
021600020312      * Se immesso il cd.padroncino
021700130329     C     viddis        IFeq      *zero
021800020307     C                   SETON                                        9045
021900000000     C   90              GOTO      ENDCTL
022000000000     C                   ENDIF
022100000000      *
022200000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
022300130329     C     VIDDAt        ifeq      0
022301130329     C     VIDDAtn       oreq      0
022400020308     C                   SETON                                        4390
022500000000     C                   GOTO      ENDCTL
022600000000     C                   END
022700000000      * Controllo data iniziale
022800130329     C     VIDDAt        IFGT      0
022900130329     C                   Z-ADD     VIDDAt        G02DAT
023000000000     C                   MOVEL     *BLANK        G02ERR
023100940926     C                   CALL      'XSRDA8'
023200000000     C                   PARM                    WLBDAT
023300000000     C     G02ERR        IFEQ      '1'
023400090505     C                   SETON                                        44  90
023500000000     C                   GOTO      ENDCTL
023600000000     C                   ENDIF
023700020307     C                   Z-ADD     G02INV        DALRIB            8 0
023800130329     C                   Z-ADD     G02dat        viddat
023900020307     c                   endif
024000090505      * Controllo data conferma ricezione
024100130329     C     VIDdatn       ifgt      0
024101130329     C                   Z-ADD     VIDDAtn       G02DAT
024102130329     C                   MOVEL     *BLANK        G02ERR
024103130329     C                   CALL      'XSRDA8'
024104130329     C                   PARM                    WLBDAT
024105130329     C     G02ERR        IFEQ      '1'
024106130329     C                   SETON                                        51  90
024107130329     C                   GOTO      ENDCTL
024108130329     C                   ENDIF
024109130329     C                   Z-ADD     G02INV        DALRIBn           8 0
024110130329     C                   Z-ADD     G02dat        viddatn
024400090505     C                   END
025700130329     c                   if        dalribn >=dalrib
025800090505     C                   SETON                                        52  90
025900090505     C                   GOTO      ENDCTL
026000090505     C                   ENDIF
026100090505      *
026200020402     c                   movel     vidfil        kpdr              7 0
026300130329     c                   move      viddis        kfoglio
026400130329     c     kapd          chain     fnfvv01l
026401130329     c                   if        %found(fnfvv01l) and
026402130329     c                             fvvfcf = *blank and
026403130329     c                             fvvdfv > wdat   and
026404130329     c                             fvvatb = *blank
027000130329     c     kdst          chain     fidst01l
027100130329     c                   if        %found(fidst01l) and dstpda = 'N'
027101130329     c                   seton                                        4690
027200130329     c                   goto      endctl
027300081008     c                   endif
027301130329     c                   else
027302130329     c                   seton                                        4290
027303130329     c                   goto      endctl
027304130329     c                   endif
029900000000      *
030000000000     C     ENDCTL        ENDSR
030100090914     C*-----------------------------------------------------*
030200130329     C     aggiorna      begsr
030300090914     C*-----------------------------------------------------*
030301130329      * ?   tutti i file in cui è contenuto il numero e la data distinta
030302130329     C/EXEC SQL
030303130329     C+ update Fnfvv00F set fvvdfv = :dalribn
030304130329     c+ where fvvnfv = :viddis and fvvdfv = :dalrib
030305130329     c+ and fvvnpg = 4 and fvvfgs = :vidfil or
030306130329     c+ fvvnfv = :viddis and fvvdfv = :datadal
030307130329     c+ and fvvnpg = 8 and fvvfgs = :vidfil
030308130329     C/END-EXEC
030309130329     c
030310130329     C/EXEC SQL
030311130329     C+ update Fnarb00F set arbddc = :dalribn
030312130329     c+ where arbndc = :dstnfv and arbddc = :dalrib
030313130329     c+ and arbpdc = :dstpdr
030316130329     C/END-EXEC
031500090914     C                   ENDSR
