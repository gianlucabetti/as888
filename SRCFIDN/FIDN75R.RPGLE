000100981118      *-------------------------------------------------------------------------*
000200050524      * Stampa progetto di liquidazione filtro                                  *
000300981118      *-------------------------------------------------------------------------*
000400050715      * La stampa è possibile se la ca è in fase 660 690 e non affidate ad AIG  *
000500050524      *                                                                         *
000600981118      *-------------------------------------------------------------------------*
000700981118
000800981118      ****************************************************************
000900981118      *  FASI C.A. FISSATE A PROGRAMMA
001000981118      ****************************************************************
001100050524      *    660 = Ricezione totale documenti                   (WRido)
001200050524      *    690 = Ristampa progetto di liqidazione             (WRspl)
001300050525      *    700 = Stampa progetto di liqidazione               (WFliq)
001400981118      ***************************************************************
001500981118
001600981118      ****************************************************************
001700981118      *  RIEPILOGO INDICATORI
001800981118      ***************************************************************
001900981118      * 28 - ERRORE GENERICO DSPF
002000981118      * 30 - COMODO
002100981118      * 31 - COMODO
002200050524      * 40 - ERRORE
002300981118      ***************************************************************
002400981118
002500981118     H DECEDIT('0,') DATEDIT(*DMY.)
002600981119      *------------------------------------------------------------------------*
002700050525     fFIDN75D   CF   E             WORKSTN
002800050524     fFNDCT01L  IF   E           K DISK
002900981118      *------------------------------------------------------------------------*
003000981118      *
003100981118      *   C A M P I
003200050524      *------------------------------------------------------------------------*
003300050524     D Kaac            S                   LIKE(DCTaac)
003400050524     D Kfil            S                   LIKE(DCTfil)
003500050524     D Knca            S                   LIKE(DCTnca)
003600050524     D WRspl           S                   LIKE(DCTfca)   INZ(690)
003700050524     D WRido           S                   LIKE(DCTfca)   INZ(660)
003800050525     D WFliq           S                   LIKE(DCTfca)   INZ(700)
003900981118
004000981118     D W0140           S             14  0
004100981118     D Wdtgio          S              8  0
004200981118     D dateu           S              8  0
004300981118      *
004400981118      *   S C H I E R E
004500981119     D MSG             S             78    DIM(5) CTDATA PERRCD(1)              MSG VIDEO
004600981118      *
004700981118      *   D S   I N T E R N E / E S T E R N E
004800981118     D WLBDAT          DS                  INZ
004900981118     D  G02DAT                 1      8  0
005000981118     D  G02INV                 9     16  0
005100981118     D  G02ERR                17     17
005200981118     D  G02TGI                18     22  0
005300981118      *----------------------------------
005400050524      * - Ds di riferimento al file esterno AZUTE00F
005500050524     d AZUTEds       e ds                  extname(AZUTE00F)
005600050524      * - Ds per dati organigramma
005700050524     d dDATIUTE      e ds
005800050524      *
005900050524     D Fidn00DS      E DS
006000981216     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100981118     D KPJBA         E DS
006200981216      *
006300050524     D DDCT01        E DS                  inz
006400981118     D                SDS
006500981118     D  VTCPGM                 1     10
006600981118
006700981118      *------------------------------------------------------------------------*
006800981118      *---------------  C A L C O L O  ----------------------------------------*
006900981118      *------------------------------------------------------------------------*
007000050524
007100050524     C                   do        *hival
007200050524      * Emissione VIDEO1
007300050525     c                   write     FI75t01
007400050525     c                   write     FI75z01
007500050525     c                   exfmt     FI75d01
007600050524
007700050524     c                   setoff                                       284041
007800050524     c                   setoff                                       422190
007900050524     c                   clear                   v1cmsg
008000050524
008100050524      * f03=Fine
008200050524     c   kc              leave
008300050524
008400050524      * Controlli  videata
008500050524     c                   exsr      ctrd01
008600050524      * se è acceso il  28 ritorno ad emettere il formato video
008700050524     c   28              iter
008800050719      * se dato KN f14 ritorno
008900050719     c   kn              iter
009000050524
009100050524      * Se non ho confermato o ci sono errori riemetto video
009200050524     c                   IF        *INKF
009300050524     c                   exsr      SUB_confer
009400050524     c                   leave
009500050524     c                   ELSE
009600050524     c                   iter
009700050524     c                   ENDIF
009800050524      *
009900050524     c                   enddo
010000050524
010100981118
010200981118     c                   EVAL      *INLR = *ON
010300981118      **********************************************************************
010400981118      * CONTROLLI VIDEO 1
010500981118      **********************************************************************
010600981118     C     CTRD01        BEGSR
010700050524
010800050524      * Controllo se inseriti dati validi
010900050524    1C                   IF        V1CFIL = *zeros  OR  V1Caac = *zeros
011000050524    1C                                              OR  V1Cnca = *zeros
011100050524     C                   movel     MSG(2)        V1Cmsg
011200050524     c                   eval      *in28 = *on
011300050524     c                   eval      *in40 = *on
011400050524    1C                   ENDIF
011500050524     c   28              goto      Endct1
011600050524
011700050524      * Controllo Anno C.A. sistemo anno di due cifre
011800050524     C                   if        V1Caac < 100  and  V1Caac > 60
011900050524     C                   ADD       1900          V1Caac
012000050524     C                   endif
012100050524     C                   if        V1Caac < 100  and  V1Caac <= 60
012200050524     C                   ADD       2000          V1Caac
012300050524     C                   endif
012400050524
012500050524      * Controllo N° C.A.
012600050524      *
012700050524     C                   z-add     V1Cfil        Kfil
012800050524     C                   z-add     V1Caac        Kaac
012900050524     C                   z-add     V1Cnca        Knca
013000050524     C     Keynca        CHAIN     FNDCT000                           31
013100050524      *
013200050524    2C                   IF        *in31 = *on
013300050524     C                   movel     MSG(1)        V1Cmsg
013400050524     c                   eval      *in28 = *on
013500050524     c                   eval      *in40 = *on
013600050524    2c                   else
013700050524      *
013800050719      * f14=Interrogazione C.A.
013900050719     C                   IF        *INKN
014000050719     c                   exsr      SUB_F14
014100050719     c                   goto      Endct1
014200050719     c                   ENDIF
014300050719
014400050524     c                   movel     DCTflo        DDCT01
014500050524      *
014600050524      * se la CA non è in fase 660 o 690
014700050524     c                   if        DCTfca <> WRido and DCTfca <> WRspl
014800050524     C                   movel     MSG(3)        V1Cmsg
014900050524     c                   eval      *in28 = *on
015000050524     c                   eval      *in40 = *on
015100050715     c                   goto      Endct1
015200050524     c                   endif
015300050928      *
015400050928     *** * se la CA è affidata ad aig errore non permettere
015500050928     *** * la stampa del progetto di liquidazione
015600050928     ***c                   if        §dctaffi= 'A'
015700050928     ***C                   movel     MSG(4)        V1Cmsg
015800050928     ***c                   eval      *in28 = *on
015900050928     ***c                   eval      *in40 = *on
016000050928     ***c                   goto      Endct1
016100050928     ***c                   endif
016200050715     c                   endif
016300050524      *
016400981118
016500050525     c     ENDCT1        ENDSR
016600981118      *****************************************************************
016700981118      * AGGIORNO ARCHIVI
016800981118      *****************************************************************
016900981118     C     SUB_confer    BEGSR
017000981118
017100981118     c                   clear                   kpjbu
017200981118      *
017300981118      *
017400050524      * Richiamo PGM di stampa progetto di liquidazione
017500050525     c                   exsr      val_ds
017600050525     c                   movel     'G'           I00mod
017700050525     c                   movel     wfliq         I00fca
017800050525     c                   movel     fidn00ds      kpjbu
017900130920     c                   call      'FIDN29R'
018000050525     c                   parm                    kpjba
018100981118      *
018200981118      *
018300981118     c                   ENDSR
018400981118      *****************************************************************
018500050525      * Valorizzo DS per pgm stampa PL
018600981118      *****************************************************************
018700050525     C     VAL_ds        BEGSR
018800981118      *
018900050525     c                   clear                   FIDN00DS
019000981119      *
019100050525     c                   z-add     dctgfc        I00fgs
019200050525     c                   z-add     dctaac        I00aac
019300050525     c                   z-add     dctfil        I00fil
019400050525     c                   z-add     dctnca        I00nca
019500050525     c                   z-add     dctaas        I00aas
019600050525     c                   z-add     dctlnp        I00lnp
019700050525     c                   z-add     dctnrs        I00nrs
019800050525     c                   z-add     dctnsp        I00nsp
019900050525     c                   z-add     dctlna        I00lna
020000050525     c                   movel     dcttad        I00tad
020100050525     c                   movel     'S'           I00tpb
020200981119      *
020300981119     c                   ENDSR
020400050525      ****************************************************************
020500050525      * F14 = INTERROGAZIONE C.A.
020600050525      ****************************************************************
020700050525     C     SUB_F14       BEGSR
020800050525      *
020900050525      *
021000050525     C                   EXSR      VAL_DS
021100050525     c                   movel     'I'           I00mod
021200050525     c                   movel     dctfca        I00fca
021300050525      *
021400050525     c                   movel     FIDN00DS      kpjbu
021500050525      *
021600050525     c                   call      'FIDN01R'
021700050525     c                   parm                    kpjba
021800050525      *
021900050525     c                   endsr
022000981119      *****************************************************************
022100981119      * ROUTINE INIZIALE
022200981119      *****************************************************************
022300981119     C     *INZSR        BEGSR
022400981119      *
022500981118     C     *ENTRY        PLIST
022600981118     C                   PARM                    KPJBA
022700981118      *
022800981118      * data e ora
022900981118     C                   TIME                    W0140
023000981118      * UDATE IN GGMMAAAA
023100981118     C                   MOVE      W0140         WDTGIO
023200981119      *
023300981118     C                   Z-ADD     WDTGIO        G02DAT
023400981118     C                   MOVEL     *BLANK        G02ERR
023500981118     C                   CALL      'XSRDA8'
023600981118     C                   PARM                    WLBDAT
023700981119      * UDATE IN AAAAMMGG
023800981118     C                   MOVEL     G02INV        DATEU
023900981118      *
024000981216      * Preimposto dati video
024100981118     c                   move      wdtgio        V1Caac
024200981216      *
024300981216      * Dati società
024400981216     C                   Z-ADD     1             CODUT
024500981216     C                   CALL      'X§PARUT'
024600981216     C                   PARM                    UT§DSE
024700981216      *
024800981216     C                   MOVEL     RAGUT         RSUT
024900981118      *
025000050525     C     KEYNCA        klist
025100981118     C                   KFLD                    kaac
025200981118     C                   KFLD                    kfil
025300981118     C                   KFLD                    knca
025400981118      *
025500981118     C                   ENDSR
025600981118      *---------------------------------------------------------------------------------------------
025700981119      *---------------------------------------------------------------------------------------------
025800981118** MSG  Lungh. 78                                                            *
025900050524Il numero C.A. immesso non esiste                                              1
026000050524Il valore immesso nel campo non è valido                                       2
026100050524La C.A. non è nella fase corretta per la ristampa progetto di liquidazione     3
026200050928»? LIBERO ?« La C.A. è affidata ad A.I.G. e quindi non si emette il P.L.                    4
