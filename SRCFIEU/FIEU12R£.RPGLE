000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200981217      ***************************************************************************
000300991129      *  CREAZIONE EVENTI FISSI PER DPD                                         *
000400971216      *-------------------------------------------------------------------------*
000500060606      * Il pgm crea tIDP300F file eventi DPD e aggiorna                         *
000600971216      ***************************************************************************
000700060606     FTIDP301L  UF A E           K DISK
000800991124      *
000900991129     FTNTBE01L  IF   E           K DISK    usropn
001000940321      *----------------------------------------------------*
001100000103     D kevd            S             15    DIM(30)                              CODICE NAZIONE
001200991129     D evd             S            256    DIM(30)                              CODICE NAZIONE
001300991129      *-------------------
001400991129     D XX              S              3  0
001500991129     D YY              S              3  0
001600000103     D W010A           S             10
001700000103     D W012A           S             12
001800971212      *-------------------
001900060606     D FIEU12DS      E DS
002000991129     D DEVD          E DS
002100991130     D KPJBA         E DS
002200911213      *----------------------------------------------------*
002300981106
002400060606      * TIPO LANCIO I12TLA =  "C" - NON ELABORO E CHIUDO CON LR
002500991129      *                       "L" - ELABORO E CHIUDO CON LR
002600991129      *                       " " - ELABORO E CHIUDO IN RETRN
002700991129
002800060606    0C                   IF        I12TLA = 'C'
002900991129     C                   eval      *INLR = *ON
003000991129     C                   ELSE
003100991129
003200000103      * Scrivo eventi fissi
003300060606     C     kspe          setll     tidp3000
003400991129     C                   if        NOT %equal
003500971215      *
003600991129     c                   clear                   YY
003700060606     c                   z-add     i12aas        dp3aas
003800060606     c                   z-add     i12lnp        dp3lnp
003900060606     c                   z-add     i12nrs        dp3nrs
004000060606     c                   z-add     i12nsp        dp3nsp
004100060630     c                   movel     i12parcel     dp3prn
004200991129     c                   movel     'R'           dp3ftr
004300991129      *
004400991129     C                   DO        XX            YY
004500991130     C                   movel     KEVD(YY)      dp3evd
004600991129     C                   movel     EVD(YY)       DEVD
004700991214     c                   movel     §evdice       dp3cev
004800060621     c                   write     Tidp3000
004900991129     C                   ENDDO
005000991129      *
005100991129     C                   ENDIF
005200971210      *
005300060606    0C                   IF        I12TLA = *blanks
005400991129     C                   return
005500991129     C                   ELSE
005600991129     C                   eval      *INLR = *ON
005700991129    0C                   ENDIF
005800991129      *
005900991129    0C                   ENDIF
006000930126      *----------------------------------------------------*
006100930126      *   DEFINIZIONE CHIAVI                               *
006200930126      *----------------------------------------------------*
006300940321     C     *INZSR        BEGSR
006400940321      *
006500991129     c     *ENTRY        PLIST
006600991130     c                   PARM                    KPJBA
006700060606     c                   PARM                    FIEU12DS
006800991129      *
006900991129     C     KSPE          KLIST
007000060606     C                   KFLD                    I12AAS
007100060606     C                   KFLD                    I12LNP
007200060606     C                   KFLD                    I12NRS
007300060606     C                   KFLD                    I12NSP
007400991124      *
007500991129      * Carico Tabella Eventi DPD, solo eventi fissi da tradurre
007600991129     c                   clear                   xx
007700991129     c                   clear                   KEVD
007800991129     c                   clear                   EVD
007900991129     c                   OPEN      TNTBE01L
008000991129      *
008100991129     c     'EVD'         chain     tntbe000                           31
008200991124      *
008300991124     c                   DOW       *in31 = *off
008400991124      * Se il S.I. è indicato deve essere uguale al mio
008500991124     c                   IF        TBEatb = *BLANKS  and
008600991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
008700991129     c                   movel     TBEuni        Devd
008800060608      * Se il codice è un evento fisso e deve essere scritto lo inserisco in
008900060608      * schiera per alimentare il loop
009000991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
009100991124     c                   add       1             xx
009200991129     c                   movel     TBEke1        KEVD(XX)
009300991129     c                   movel     TBEuni        EVD(XX)
009400991130     c                   ENDIF
009500060608     c                   endif
009600991130      *
009700991129     c     'EVD'         reade     tntbe000                               31
009800991130     c                   ENDDO
009900971216      *
010000991129     c                   CLOSE     TNTBE01L
010100991129      *
010200991124     C                   ENDSR
010300941230      *-------------------------------------------------------------------------
