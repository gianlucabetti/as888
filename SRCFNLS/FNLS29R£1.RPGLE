000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200121211     H* FNLS29R *----------------------------------------------------*
000300121211     H*       Controllo congruenza linee dei fogli viaggio aperti    *
000400941007     H*--------------------------------------------------------------*
000500941006     FFNFGV01L  IF   E           K DISK
000600121211     FFNFGV06L  IF   E           K DISK    rename(fnfgv000:fnfgv006)
000700000427     FFNFGW01L  IF   E           K DISK
000800940916     FTABEL00F  IF   E           K DISK
000900121211     d
001000121211     D FE0             S              3    DIM(301)
001100121211     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001200940916     D*
001300121211
001400121211     D fnls29ds        DS
001500121211     d* Terminal di partenza
001600121211     D  I29TFP                 1      3  0
001700121211     c* Numero foglio viaggio: se = 0 tutti i fogli aperti del terminal
001800121211     D  I29nfv                 4      9  0
001900121211     D  o29err                10     10
002000121211     D  o29msg                11    256
002100121211     d*
002200121211     d
002300960319     D                 DS
002400121211     D  wFGVLNA                1      3
002500121211     D  FGVFFV                 4    243
002600121211     D  FGVFF2               244    450
002700121211     D  FGWFF3               451    690
002800121211     D  FGWFF4               691    900
002900121211     D  VUOTO                901    903
003000121211     D  FFV                    1    903
003100121211     D                                     DIM(301)
003200960319     D                 DS
003300121211     D  WFLPLNA                1      3
003400121211     D  FGVFLP                 4    243
003500121211     D  FGVFL2               244    450
003600121211     D  FGWFL3               451    690
003700121211     D  FGWFL4               691    900
003800121211     D  VUOTO2               901    903
003900121211     D  FLP                    1    903
004000121211     D                                     DIM(301)
004100020807      *
004200020807     D KPJBA         E DS
004300121211     D fnlv55ds      E DS
004400121211     D dstv          E DS
004500121211     d I               s              4  0
004600121211     d XX              s              4  0
004700121211     d w05nfv          s              5  0
004800121211     d w003a           s              3
004900121211     d wmsg            s             80
005000121211     d cod             s                   like(tblcod)
005100121211     d key             s                   like(tblkey)
005200020807      ****************************************************************
005300020807      *  RIEPILOGO INDICATORI
005400020807      *****************************************************************
005500000000     C     *ENTRY        PLIST
005600000000     C                   PARM                    KPJBA
005700121211     C                   MOVEL     KPJBU         fnls29ds
005800931209     C*
005900121211     c* Se passato il foglio controllo solo lui, altrimenti tutti gli aperti
006000121211     c                   if        i29tfp=0
006100121211     c                   eval      o29err='E'
006200121211     c                   eval      o29msg=msg(1)
006300121211     c                   goto      fine
006400121211     c                   endif
006500121211     c
006600121211     c                   if        i29nfv>0
006700121211     c                   eval      w05nfv=i29nfv
006800121211     c     kfgv01        chain     fnfgv01l
006900121211     c                   if        not %found(fnfgv01l)
007000121211     c                   eval      o29err='E'
007100121211     c                   eval      o29msg=msg(1)
007200121211     c                   goto      fine
007300121211     c                   endif
007400121211     c
007500121217     c     kfgw          chain     fnfgw01l
007600121217     c                   if        not %found(fnfgw01l)
007700121217     c                   clear                   fgwfF3
007800121217     c                   clear                   fgwfF4
007900121217     c                   clear                   fgwfL3
008000121217     c                   clear                   fgwfL4
008100121217     c                   endif
008200121217     C
008300121211     c                   exsr      contr_lin
008400121211
008500121211     c                   else
008600121211     c                   exsr      Readfgv06
008700121211     c                   endif
008800971202     C*
008900971202     C                   MOVEL     'C'           D55TLA
009000971202     C                   CALL      'FNLV55R'
009100020807     C                   PARM                    FNLV55DS
009200950710     C*
009300121211     c     fine          tag
009400121211     C                   MOVEL     FNLS29DS      KPJBU
009500020807     C                   MOVEL     *ON           *INLR
009600940916     C**************************************************************************
009700940916     C*   R O U T I N E     I N I Z I A L E
009800940916     C**************************************************************************
009900940916     C     *INZSR        BEGSR
010000940916     C*
010100121211     C     Kfgv01        klist
010200121211     C                   kfld                    w05nfv
010300121211     C                   kfld                    i29tfp
010400121217     C     Kfgw          klist
010500121217     C                   kfld                    fgvnfv
010600121217     C                   kfld                    fgvlnp
010700121211     C     KTAB          KLIST
010800940916     C                   KFLD                    CODUT
010900121211     C                   KFLD                    COD
011000121211     C                   KFLD                    KEY
011100121211     c
011200121211     c                   z-add     1             codut             1 0
011300941229     C*
011400940916     C                   ENDSR
011500121211     C**************************************************************************
011600121211     C*  Lettura LF    fgv06l che esclude gli annullati e i fogli chiusi
011700121211     C**************************************************************************
011800121211     C     Readfgv06     BEGSR
011900121211     c
012000121211     c     i29tfp        setll     fnfgv06l
012100121211     c     i29tfp        reade     fnfgv06l
012200121211     c
012300121211     c                   dow       not %eof(fnfgv06l)
012400121211     c
012500121217     c     kfgw          chain     fnfgw01l
012600121217     c                   if        not %found(fnfgw01l)
012700121217     c                   clear                   fgwfF3
012800121217     c                   clear                   fgwfF4
012900121217     c                   clear                   fgwfL3
013000121217     c                   clear                   fgwfL4
013100121217     c                   endif
013200121217     c
013300121211     c                   exsr      contr_lin
013400121211     c
013500121211     c     i29tfp        reade     fnfgv06l
013600121211     c                   enddo
013700121211     c
013800121211     C                   ENDSR
013900121211     C**************************************************************************
014000121211     C*  controllo linee
014100121211     C**************************************************************************
014200121211     C     Contr_lin     BEGSR
014300121211     c* Escludo i foggli inoltro e defluenza
014400121211     C                   MOVEL     'TV'          COD
014500121211     C                   MOVEL(P)  FGVTTR        KEY
014600121211     C     KTAB          CHAIN     TABEL                              83
014700121211     C  N83              MOVEL     TBLUNI        DSTV
014800121211      *
014900121211    0c                   if        §tvdef<>'S' and fgvttr<>'I'
015000121211     c                   eval      wflplna=%editc(fgvlna:'X')
015100121211     c                   eval      wfgvlna=%editc(fgvlna:'X')
015200121211     c
015300121211     C                   Z-ADD     1             I
015400121211     C*
015500121211    1C     I             DOWLE     301
015600121211     C     ffv(I)        ANDGT     *ZEROS
015700121211     c* llop sulle linee per determinare il terminal di arrivo
015800121211     C                   CLEAR                   FNLV55DS
015900121211     C                   MOVEL     'A'           D55TPT
016000121211     C                   MOVE      ffv(I)        D55LIN
016100121211     C                   MOVE      i29tfp        D55LNP
016200121211     C                   MOVE      fgvDFV        D55DRF
016300121211     C                   CALL      'FNLV55R'
016400121211     C                   PARM                    FNLV55DS
016500121211     c
016600121211     c                   eval      fe0(I)=%editc(d55tfa:'X')
016700121211     c                   add       1             I
016800121211     c                   enddo
016900121211     C*
017000121211     C*
017100121211     C                   Z-ADD     1             I
017200121211    1C     I             DOWLE     301
017300121211     C     ffv(I)        ANDGT     *ZEROS
017400121211     C**
017500121211     C* SE LA LINEA E' UN SECONDO LIVELLO IN ARRIVO E NON SCARICA A
017600121211     C* SE' STESSA DEVE ESSERE PRESENTE COME LINEA ANCHE IL SUO TER-ARR
017700121211   2AC     ffv(I)        IFNE      FE0(I)
017800121211     C     ffv(I)        ANDNE     flp(I)
017900121211     C     FE0(I)        LOOKUP    ffv                                    31
018000121211   2BC     *IN31         IFEQ      *OFF
018100121211     C* ERRORE
018200121211     C                   eval      wmsg=msg(04)
018300121211     C                   eval      %subst(wmsg:12:3)=ffv(I)
018400121211     C                   eval      %subst(wmsg:63:3)=fe0(I)
018500121211     c                   eval      o29msg=%trim(o29msg)+' Foglio '+
018600121211     c                             %editc(fgvlnp:'X') + ' ' +
018700121211     c                             %editc(fgvnfv:'4') +':'+
018800121211     c                             wmsg
018900121211     c
019000121211   2BC                   ENDIF
019100121211   2AC                   ENDIF
019200121211   c
019300121211     C                   Z-ADD     1             XX
019400121211   2AC     ffv(I)        IFNE      flp(I)
019500121211     C                   MOVEL     FE0(I)        W003A
019600121211     C     W003A         LOOKUP    FE0(XX)                                31
019700121211    3C     *IN31         DOWEQ     *ON
019800121211    4C     flp(XX)       IFNE      flp(I)
019900121211     C     ffv(XX)       ANDNE     flp(XX)
020000121211     C     flp(XX)       ORNE      flp(I)
020100121211     C     ffv(XX)       ANDEQ     FE0(XX)
020200121211     C* ERRORE
020300121211     C                   eval      wmsg=msg(03)
020400121211     C                   eval      %subst(wmsg:9:3)=ffv(I)
020500121211     C                   eval      %subst(wmsg:13:3)=ffv(XX)
020600121211     c                   eval      o29msg=%trim(o29msg)+' Foglio '+
020700121211     c                             %editc(fgvlnp:'X') + ' ' +
020800121211     c                             %editc(fgvnfv:'4') +':'+
020900121211     c                             wmsg
021000121211     C*
021100121211    cc                   setoff                                       31
021200121211     c                   else
021300121211     c                   add       1             xx
021400121211     C     W003A         LOOKUP    FE0(XX)                                31
021500121211    4C                   ENDIF
021600121211    3C                   ENDDO
021700121211   2AC                   ENDIF
021800121211     C*
021900121211     C                   ADD       1             I
022000121211    1C                   ENDDO
022100121211    0C                   ENDIF
022200121211     c
022300121211     C                   ENDSR
022400121211**
022500121211Non trovato foglio viaggio
022600121211Terminal di partenza non impostato
022700121211Filiali xxx/xxx hanno stesso ter.arrivo ma non stessa fil.scarico
022800121211La filiale XXX deve essere nel foglio col suo terminal arrivo xxx
