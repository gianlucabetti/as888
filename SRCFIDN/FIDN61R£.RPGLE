000100981007      *-------------------------------------------------------------------------*
000200010403      *    CHIUSURA AUTOMATICA C.A. IN PRESCRIZIONE                             *
000300981007      *-------------------------------------------------------------------------*
000400981016      *
000500981014      ****************************************************************
000600981014      *  FASI C.A. FISSATE A PROGRAMMA
000700981014      ****************************************************************
000800010403      *    900 = Fase chiusura C.A.                                    (WFACH)
000900990311      ****************************************************************
001000981013      *
001100980521      ****************************************************************
001200980521      *  RIEPILOGO INDICATORI
001300980521      ***************************************************************
001400980904      * 30 - COMODO
001500980904      ***************************************************************
001600980623
001700980521     H DECEDIT('0,') DATEDIT(*DMY.)
001800980623
001900010403     fFNDCT09L  IF   E           K DISK
002000010403     fFNDCF01L  IF   E           K DISK
002100020627     ffndcl01l  if   e           k disk
002200980623
002300980521      *------------------------------------------------------------------------*
002400980923      *
002500980915      *----------------------------------
002600010403     D KPJBA         E DS
002700020502     D* rem                    1      3
002800020502     D* remfil                 4      6
002900980529     D UT§DSE0F      E DS
003000010403     D CNCR80        E DS
003100010403     D FIDN61DS      E DS
003200010403     D FIDN22DS      E DS
003300980521      *
003400010403     D WLBDAT          DS                  INZ
003500010403     D  G02DAT                 1      8  0
003600010403     D  G02INV                 9     16  0
003700010403     D  G02ERR                17     17
003800010403     D  G02TGI                18     22  0
003900980623
004000010403     D Wdataeur        S               D   DATFMT(*eur)
004100010403     D Wdataoggi       S               D   DATFMT(*eur)
004200010403     D w0140           s             14  0
004300010403     D dateu           s              8  0
004400010403     D wdtgio          s              8  0
004500010403
004600010403     D POGEST          S                   LIKE(DCTGFC)
004700990122     c*---------------------------------------------------------------------------------------------
004800010403     c*                   C   I   C   L   O
004900990122     c*---------------------------------------------------------------------------------------------
005000010403      *
005100010403      *
005200010403      * mi posiziono con la fase sul file testate C.A.
005300010403
005400010403     c     I61FCA        setll     fndct09l
005500010403      *
005600981013     C                   do        *hival
005700980623
005800010403     c     i61fca        reade     fndct09l
005900010403
006000010403      * fine file
006100010403     c                   if        %eof(fndct09l)
006200010403     c                   leave
006300010403     c                   endif
006400010403      *
006500010403      * controllo il P.O. gestione se non sono io leggo il successivo
006600010403      *
006700010403     C                   IF        POGEST <> DCTGFC
006800010403     C                   ITER
006900010403     C                   ENDIF
007000010403      *
007100010403      * recupero la data in cui è stata eseguita la fase
007200010403      *
007300010403     c                   exsr      SR_DTAFCA
007400010403      *
007500010403      * calclo se sono passati 365  giorni
007600010403      *
007700040310     c**!!!*iso          move      dcfdfc        Wdataeur
007800040310     c**!!!wdataoggi     subdur    wdataeur      num_gior:*D       5 0
007900010403      *
008000010403      * se NUM_GIOR maggiore o uguale a 365 richiamo il PGM di chiusura
008100010403      *
008200040310     c**!!!              if        num_gior < 365
008300040310      * ora la data viene impostata nel lancio
008400040310     c                   If        dcfdfc >= i61dfc
008500010403      * leggo il successivo record
008600010403     c                   iter
008700020627     c                   endif
008800020627      * controllo se la c.a. ha l'importo trattenuto
008900020627     c     kdcf          chain     fndcl01l
009000020627     c                   if        %found(fndcl01l)
009100020627      * se c'è l'importo trattenuto non chiudo la c.a.
009200020627     c                             and dclipt > 0
009300020627     c                   iter
009400020627     c                   endif
009500010403      *
009600020627     c**!!!              else
009700010403      * richiamo routine di chiusura
009800010404     c                   EXSR      SR_CHIUDO
009900010403      *
010000020627     c**!!!              endif
010100010403      *
010200010403     c                   enddo
010300980623
010400990122      *
010500980907     c                   EVAL      *INLR = *ON
010600010403      *****************************************************************
010700010403      *   RICERCO DATA ESECUZIONE FASE
010800010403      *****************************************************************
010900010403     C     SR_DTAFCA     BEGSR
011000010403      *
011100010403     c                   clear                   dcfdfc
011200010403      *
011300010403     c     kdcf          setgt     FNDCF01L
011400010403      *
011500010403     c                   do        *hival
011600010403      *
011700010403     c     kdcf          readpe    FNDCF01L
011800010403      * fine file
011900010403     c                   if        %eof(FNDCF01l)
012000010403     c                   leave
012100010403     c                   endif
012200010403      *
012300010403      * se la fase che leggo è uguale al quella della C.A.
012400010403     c                   if        dcffca =   dctfca
012500010403     c                   leave
012600010403     c                   endif
012700010403     c*
012800010403     c                   enddo
012900010403     c*
013000010403     c                   endsr
013100010403      *****************************************************************
013200010403      *   RICHIAMO IL PGM DI CHIUSURA  FIDN22R
013300010403      *****************************************************************
013400010403     C     SR_CHIUDO     BEGSR
013500010403      *
013600010403     c                   clear                   fidn22ds
013700010403     c                   eval      i22tri = 'R'                                 * richiamato
013800010403     c                   eval      i22mod = 'C'                                 * chiusura
013900010403     c                   eval      i22aac = DCTAAC                              * anno CA
014000010403     c                   eval      i22fil = DCTFIL                              * P.O. apertura CA
014100010403     c                   eval      i22nca = DCTNCA                              * Numero CA
014200010403     c                   eval      i22cch = I61CCH                              * causale chiusura
014300010403     c                   eval      i22fgs = DCTGFC
014400010403     c                   movel     fidn22ds      kpjbu
014500010403     c                   call      'FIDN22R'
014600010403     c                   parm                    kpjba
014700010403      *
014800010403     c                   endsr
014900980910      *****************************************************************
015000980527      * ROUTINE INIZIALE
015100980527      *****************************************************************
015200980527     C     *INZSR        BEGSR
015300980615      *
015400980521     C     *ENTRY        PLIST
015500980521     C                   PARM                    KPJBA
015600010403     C                   movel     kpjbu         fidn61ds
015700010403      *
015800010403     C                   Z-ADD     1             CODUT
015900010403     C                   CALL      'X§PARUT'
016000010403     C                   PARM                    UT§DSE0F
016100010403     C                   MOVEL     REC80         CNCR80
016200010403      *
016300010403      * recupero il P.O. gestione
016400020502     c                   movel     simpou        pogest
016500020502     C*                  IF        rem = 'REM'  AND  remfil > *ZEROS
016600020502     C*                  movel     REMFIL        POGEST
016700020502   X2c*                  ELSE
016800020502     c*                  movel     SIMFEL        POGEST
016900020502    2C*                  ENDIF
017000010403      *
017100010403      * Se sede
017200020502     C*                  IF        POGEST = *zeros
017300020502     C*                  z-add     46            POGEST
017400020502     C*                  ENDIF
017500010403
017600010403      * reperisco data e ora
017700010403      * ORA
017800010403     C                   TIME                    W0140
017900010403      * UDATE IN GGMMAAAA
018000010403     C                   MOVE      W0140         WDTGIO
018100010403      * UDATE IN AAAAMMGG
018200010403     C                   Z-ADD     WDTGIO        G02DAT
018300010403     C                   MOVEL     *BLANK        G02ERR
018400010403     C                   CALL      'XSRDA8'
018500010403     C                   PARM                    WLBDAT
018600010403     C                   MOVEL     G02INV        DATEU
018700010403     C     *iso          MOVEL     DATEU         Wdataoggi
018800010403
018900010403      * CHIAVI
019000010403
019100010403     C     Kdcf          klist
019200010403     C                   KFLD                    dctaac
019300010403     C                   KFLD                    dctfil
019400010403     C                   KFLD                    dctnca
019500010403      *
019600010403     c                   ENDSR
019700980923
