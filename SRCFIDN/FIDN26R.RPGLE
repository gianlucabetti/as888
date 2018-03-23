000100981118      *-------------------------------------------------------------------------*
000200981118      * Stampa lettera di accompagnamento alla copia denuncia per il cliente    *
000300060619      * solo per le CA con evento                                               *
000400981118      *-------------------------------------------------------------------------*
000500981118      * La stampa è possibile se è stato assegnato il n° pratica assicurativa   *
000600981118      * Questo pgm non avanza di fase la C.A.                                   *
000700981118      *-------------------------------------------------------------------------*
000800981118
000900981118      ****************************************************************
001000981118      *  FASI C.A. FISSATE A PROGRAMMA
001100981118      ****************************************************************
001200981118      ***************************************************************
001300981118
001400981118      ****************************************************************
001500981118      *  RIEPILOGO INDICATORI
001600981118      ***************************************************************
001700981118      * 28 - ERRORE GENERICO DSPF
001800981118      * 30 - COMODO
001900981118      * 31 - COMODO
002000981118      * 40 - ERRORE C.A. per stampa
002100981118      * 41 - ERRORE data stampa per ristampa
002200981118      ***************************************************************
002300981118
002400981118     H DECEDIT('0,') DATEDIT(*DMY.)
002500981119      *------------------------------------------------------------------------*
002600981118     fFIDN26D   CF   E             WORKSTN
002700981119     fFNDCT01L  UF   E           K DISK
002800981118      *------------------------------------------------------------------------*
002900981118      *
003000981118      *   C A M P I
003100981118     D Kaac            S                   LIKE(DCTaac)
003200981118     D Kfil            S                   LIKE(DCTfil)
003300981118     D Knca            S                   LIKE(DCTnca)
003400981118     D Wv1cdts         S                   LIKE(V1Cdts)
003500981118
003600981118     D xx              S              3  0
003700981118     D W0140           S             14  0
003800981118     D Wdtgio          S              8  0
003900981118     D dateu           S              8  0
004000981118     D WPTF            S              4
004100981118     D WFPTF           S              7
004200981118     D WAfca           S              3
004300981118     D W001A           S              1
004400981118     D Wdataeur        S             10
004500981118     D DATA_eur        S               D   DATFMT(*eur)
004600981118      *
004700981118      *   S C H I E R E
004800060619     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
004900981118      *
005000981118      *   D S   I N T E R N E / E S T E R N E
005100981118     D WLBDAT          DS                  INZ
005200981118     D  G02DAT                 1      8  0
005300981118     D  G02INV                 9     16  0
005400981118     D  G02ERR                17     17
005500981118     D  G02TGI                18     22  0
005600981118      *----------------------------------
005700981118     D FIDN00DS      E DS
005800981118     D FIDN26DS      E DS
005900981216     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006000981118     D KPJBA         E DS
006100981216      *
006200981118     D                SDS
006300981118     D  VTCPGM                 1     10
006400981118
006500981118      *------------------------------------------------------------------------*
006600981118      *---------------  C A L C O L O  ----------------------------------------*
006700981118      *------------------------------------------------------------------------*
006800981118
006900981118     C     for01         tag
007000981118
007100981118     c                   write     FI26t01
007200981118     c                   write     FI26z01
007300981118     c                   exfmt     FI26d01
007400981118
007500981119     c                   setoff                                       404128
007600981118     c                   clear                   v1cmsg
007700981118
007800981118      * f3=Fine
007900981118     c   kc              goto      fine
008000981118
008100981118      * Controlli
008200981118     c                   exsr      ctrd01
008300981118
008400981118      * Se non ho confermato o ci sono errori riemetto video
008500981118     c                   IF        *INKF = *ON  and  *IN28 = *OFF
008600981118     c                   exsr      SUB_confer
008700981118     c   28              goto      for01
008800981118     c                   ELSE
008900981118     c                   goto      for01
009000981118     c                   ENDIF
009100981118
009200981118     c     fine          tag
009300981118     c                   EVAL      *INLR = *ON
009400981118      **********************************************************************
009500981118      * CONTROLLI VIDEO 1
009600981118      **********************************************************************
009700981118     C     CTRD01        BEGSR
009800981118
009900981118     c                   SELECT
010000981118      *
010100981118     C                   WHEN      V1Cfil = *zeros  and  V1Cnca = *zeros
010200981118      *
010300981118     C                   WHEN      (V1Cfil = *zeros  and  V1Cnca > *zeros)  or
010400981118     C                             (V1Cfil > *zeros  and  V1Cnca = *zeros)
010500981118     C                   movel     MSG(2)        V1Cmsg
010600981118     C                   eval      *in28 = *on
010700981118      *
010800981118     C                   WHEN      V1Caac = *zeros
010900981118     C                   movel     MSG(2)        V1Cmsg
011000981118     C                   eval      *in28 = *on
011100981118      *
011200981118     C                   OTHER
011300981118      *       sistemo anno di due cifre
011400981118     C                   if        V1Caac < 100  and  V1Caac > 60
011500981119     C                   ADD       1900          V1Caac
011600981118     C                   endif
011700981118     C                   if        V1Caac < 100  and  V1Caac <= 60
011800981119     C                   ADD       2000          V1Caac
011900981118     C                   endif
012000981118      *       Controllo C.A.
012100981118     C                   z-add     V1Caac        kaac
012200981118     C                   z-add     V1Cfil        kfil
012300981118     C                   z-add     V1Cnca        knca
012400981118     C     Kdct          CHAIN     FNDCT000                           28
012500981118      *
012600981118     C                   IF        *in28 = *on
012700981118     C                   movel     MSG(2)        V1Cmsg
012800981118     C                   ELSE
012900981118      *
013000981118     C                   IF        DCTnde = *zeros
013100981118     C                   movel     MSG(3)        V1Cmsg
013200981118     C                   eval      *in28 = *on
013300981118     C                   ELSE
013400981118     C                   IF        DCTprn = *zeros
013500981118     C                   movel     MSG(4)        V1Cmsg
013600981118     C                   eval      *in28 = *on
013700060619     C                   ELSE
013800060619     C                   IF        DCTnev = *zeros
013900060619     C                   movel     MSG(6)        V1Cmsg
014000060619     C                   eval      *in28 = *on
014100060619     C                   ENDIF
014200060619     C                   ENDIF
014300981118     C                   ENDIF
014400981118     C                   ENDIF
014500981118      *
014600981118     C                   ENDSL
014700981118      *
014800981118     C                   eval      *in40 =  (*IN28 = *ON)
014900981118     c   28              goto      Ectrd01
015000981118
015100981118      * Controllo data stampa per ristampa
015200981118     C                   IF        V1Cdts > *zeros
015300981118     C                   clear                   WLBDAT
015400981118     C                   z-add     V1Cdts        G02DAT
015500981118     C                   CALL      'XSRDA8'
015600981118     C                   PARM                    WLBDAT
015700981118     C                   IF        G02ERR = '1'
015800981119     C                   seton                                        4128
015900981118     C                   movel     MSG(1)        V1CMSG
016000981118   X C                   ELSE
016100981118     C                   move      G02INV        WV1Cdts
016200981118     C                   z-add     G02DAT        V1Cdts
016300981118     C                   ENDIF
016400981118     C                   ENDIF
016500981118     c   28              goto      Ectrd01
016600981118
016700981118      * Controlli di congruità
016800981118     c                   IF        V1Cnca > 0  and
016900981118     c                               (V1Ctot = 'S'  or  V1Cdts > *zeros)
017000981118     C                   seton                                        4028
017100981118     C                   movel     MSG(5)        V1CMSG
017200981118     c                   ELSE
017300981118     c                   IF        V1Ctot = 'S'  and  V1Cdts > *zeros
017400981118     C                   seton                                        4028
017500981118     C                   movel     MSG(5)        V1CMSG
017600981118     C                   ENDIF
017700981118     C                   ENDIF
017800981118
017900981118     c     Ectrd01       ENDSR
018000981118      *****************************************************************
018100981118      * AGGIORNO ARCHIVI
018200981118      *****************************************************************
018300981118     C     SUB_confer    BEGSR
018400981118
018500981118     c                   clear                   kpjbu
018600981118      *
018700981118     c                   SELECT
018800981118      *
018900981118      * Richiamo PGM di stampa richieste
019000981119     c                   when      V1Ctot = 'S'
019100981118     c                   CALL      'FIDN26R1'
019200981118     c                   PARM                    KPJBA
019300981118      *
019400981118      * Richiamo PGM di ristampa lettera
019500981119     c                   when      V1Cdts > *zeros
019600981119     c                   MOVEL(p)  WV1Cdts       KPJBU
019700981119     c                   CALL      'FIDN26R2'
019800981118     c                   PARM                    KPJBA
019900981118      *
020000981119      * Stampa e aggiorna singola C. A.
020100981119     c                   when      V1Cnca > *zeros
020200981119     c                   EXSR      PRTAGG
020300981119      *
020400981118     c                   ENDSL
020500981118      *
020600981118     c                   ENDSR
020700981118      *****************************************************************
020800981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA E AGGIORNO C.A.
020900981118      *****************************************************************
021000981119     C     PRTAGG        BEGSR
021100981118      *
021200981119     c                   clear                   FIDN26DS
021300981119      *
021400981119     c                   z-add     DCTaac        I26aac
021500981119     c                   z-add     DCTfil        I26fil
021600981119     c                   z-add     DCTnca        I26nca
021700981119     c                   z-add     DCTaas        I26aas
021800981119     c                   z-add     DCTlnp        I26lnp
021900981119     c                   z-add     DCTnrs        I26nrs
022000981119     c                   z-add     DCTnsp        I26nsp
022100981119     c                   z-add     DCTaad        I26aad
022200981119     c                   z-add     DCTpod        I26pod
022300981119     c                   z-add     DCTnde        I26nde
022400981119     c                   movel     DCTdit        I26dit
022500981119     c                   z-add     DCTpra        I26pra
022600981119     c                   z-add     DCTprn        I26prn
022700981119     c                   movel     DCTptr        I26ptr
022800981119     c                   z-add     DCTkcc        I26kcc
022900981119     c                   z-add     DCTksc        I26ksc
023000081222     c                   move      DCTflo        I26flo
023100981119      *
023200981119     C                   IF        DCTdic > *zeros
023300981216     c                   z-add     DCTdic        I26dic
023400981119     c                   ELSE
023500981216     c                   z-add     dateu         I26dic
023600981119     c                   ENDIF
023700981119      *
023800981119     c                   movel(P)  FIDN26DS      KPJBU
023900981119      *
024000981216     c                   CALL      'FIDN26R3'
024100981119     c                   PARM                    KPJBA
024200981119      *
024300981119      *  Aggiorno archivi
024400981216     c                   z-add     I26dic        DCTdic
024500981119     c                   clear                   DCTrcd
024600981119     c                   except    aggioFNDCT
024700981119      *
024800981119     c                   ENDSR
024900981119      *****************************************************************
025000981119      * ROUTINE INIZIALE
025100981119      *****************************************************************
025200981119     C     *INZSR        BEGSR
025300981119      *
025400981118     C     *ENTRY        PLIST
025500981118     C                   PARM                    KPJBA
025600981118      *
025700981118      * data e ora
025800981118     C                   TIME                    W0140
025900981118      * UDATE IN GGMMAAAA
026000981118     C                   MOVE      W0140         WDTGIO
026100981119      *
026200981118     C                   Z-ADD     WDTGIO        G02DAT
026300981118     C                   MOVEL     *BLANK        G02ERR
026400981118     C                   CALL      'XSRDA8'
026500981118     C                   PARM                    WLBDAT
026600981119      * UDATE IN AAAAMMGG
026700981118     C                   MOVEL     G02INV        DATEU
026800981118      *
026900981216      * Preimposto dati video
027000981118     c                   move      wdtgio        V1Caac
027100981118     c                   move      'N'           V1Ctot
027200981216      *
027300981216      * Dati società
027400981216     C                   Z-ADD     1             CODUT
027500981216     C                   CALL      'X§PARUT'
027600981216     C                   PARM                    UT§DSE
027700981216      *
027800981216     C                   MOVEL     RAGUT         RSUT
027900981118      *
028000981118     C     KDCT          klist
028100981118     C                   KFLD                    kaac
028200981118     C                   KFLD                    kfil
028300981118     C                   KFLD                    knca
028400981118      *
028500981118     C                   ENDSR
028600981118      *---------------------------------------------------------------------------------------------
028700981119     oFNDCT000  E            aggioFNDCT
028800981119     o                       DCTrcd
028900981119     o                       DCTdic
029000981119      *---------------------------------------------------------------------------------------------
029100981118** MSG  Lungh. 78                                                            *
029200981118Inseriti dati non validi                                                       1
029300981118Non esiste nessuna C. A. con il numero immesso                                 2
029400981118Per la C. A. selezionata non è stata stampata denuncia                         3
029500060619La C.A. non ha ancora un numero di pratica Assicurativa                        4
029600981118Effettuare solo un tipo di selezione                                           5
029700060619La C.A. non è legata ad un evento                                              6
