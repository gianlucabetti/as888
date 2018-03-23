000100151211     H DECEDIT('0,') DATEDIT(*YMD/)
000200981217      ***************************************************************************
000300991129      *  CREAZIONE EVENTI FISSI PER DPD                                         *
000400971216      *-------------------------------------------------------------------------*
000500060606      * Il pgm crea tIDP300F file eventi DPD e aggiorna                         *
000600971216      ***************************************************************************
000700060606     FTIDP301L  UF A E           K DISK
000800151211     FFNBLP01L  IF   E           K DISK
000900991124      *
001000991129     FTNTBE01L  IF   E           K DISK    usropn
001100940321      *----------------------------------------------------*
001200000103     D kevd            S             15    DIM(30)                              CODICE NAZIONE
001300991129     D evd             S            256    DIM(30)                              CODICE NAZIONE
001400991129      *-------------------
001500991129     D XX              S              3  0
001600991129     D YY              S              3  0
001700000103     D W010A           S             10
001800000103     D W012A           S             12
001900971212      *-------------------
002000060606     D FIEU12DS      E DS
002100991129     D DEVD          E DS
002200991130     D KPJBA         E DS
002300911213      *----------------------------------------------------*
002400981106
002500060606      * TIPO LANCIO I12TLA =  "C" - NON ELABORO E CHIUDO CON LR
002600991129      *                       "L" - ELABORO E CHIUDO CON LR
002700991129      *                       " " - ELABORO E CHIUDO IN RETRN
002800991129
002900060606    0C                   IF        I12TLA = 'C'
003000991129     C                   eval      *INLR = *ON
003100991129     C                   ELSE
003200991129
003300000103      * Scrivo eventi fissi
003400060606     C     kspe          setll     tidp3000
003500991129     C                   if        NOT %equal
003600971215      *
003700991129     c                   clear                   YY
003800060606     c                   z-add     i12aas        dp3aas
003900060606     c                   z-add     i12lnp        dp3lnp
004000060606     c                   z-add     i12nrs        dp3nrs
004100060606     c                   z-add     i12nsp        dp3nsp
004200060630     c                   movel     i12parcel     dp3prn
004300991129     c                   movel     'R'           dp3ftr
004400151211     C     KSPE          chain     fnblp01l
004500151211     C                   if        %Found(fnblp01l)
004600151211     c                   movel     blpaas        DP3DTSPE
004700151211     c                   move      blpmgs        DP3DTSPE
004800151211     c                   else
004900151211     c                   movel     *date         DP3DTSPE
005000151211     c                   end
005100991129      *
005200991129     C                   DO        XX            YY
005300991130     C                   movel     KEVD(YY)      dp3evd
005400991129     C                   movel     EVD(YY)       DEVD
005500991214     c                   movel     §evdice       dp3cev
005600060621     c                   write     Tidp3000
005700991129     C                   ENDDO
005800991129      *
005900991129     C                   ENDIF
006000971210      *
006100060606    0C                   IF        I12TLA = *blanks
006200991129     C                   return
006300991129     C                   ELSE
006400991129     C                   eval      *INLR = *ON
006500991129    0C                   ENDIF
006600991129      *
006700991129    0C                   ENDIF
006800930126      *----------------------------------------------------*
006900930126      *   DEFINIZIONE CHIAVI                               *
007000930126      *----------------------------------------------------*
007100940321     C     *INZSR        BEGSR
007200940321      *
007300991129     c     *ENTRY        PLIST
007400991130     c                   PARM                    KPJBA
007500060606     c                   PARM                    FIEU12DS
007600991129      *
007700991129     C     KSPE          KLIST
007800060606     C                   KFLD                    I12AAS
007900060606     C                   KFLD                    I12LNP
008000060606     C                   KFLD                    I12NRS
008100060606     C                   KFLD                    I12NSP
008200991124      *
008300991129      * Carico Tabella Eventi DPD, solo eventi fissi da tradurre
008400991129     c                   clear                   xx
008500991129     c                   clear                   KEVD
008600991129     c                   clear                   EVD
008700991129     c                   OPEN      TNTBE01L
008800991129      *
008900991129     c     'EVD'         chain     tntbe000                           31
009000991124      *
009100991124     c                   DOW       *in31 = *off
009200991124      * Se il S.I. è indicato deve essere uguale al mio
009300991124     c                   IF        TBEatb = *BLANKS  and
009400991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
009500991129     c                   movel     TBEuni        Devd
009600060608      * Se il codice è un evento fisso e deve essere scritto lo inserisco in
009700060608      * schiera per alimentare il loop
009800991214     c                   IF        §evdifi = 'S'  and  §evditr = 'S'
009900991124     c                   add       1             xx
010000991129     c                   movel     TBEke1        KEVD(XX)
010100991129     c                   movel     TBEuni        EVD(XX)
010200991130     c                   ENDIF
010300060608     c                   endif
010400991130      *
010500991129     c     'EVD'         reade     tntbe000                               31
010600991130     c                   ENDDO
010700971216      *
010800991129     c                   CLOSE     TNTBE01L
010900991129      *
011000991124     C                   ENDSR
011100941230      *-------------------------------------------------------------------------
