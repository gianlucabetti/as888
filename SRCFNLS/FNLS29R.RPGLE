000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200121211     H* FNLS29R *----------------------------------------------------*
000300121211     H*       Controllo congruenza linee dei fogli viaggio aperti    *
000400941007     H*--------------------------------------------------------------*
000500161220     FFNFGV01L  IF   E           K DISK    usropn extfile(WFGV1)
000600121211     FFNFGV06L  IF   E           K DISK    rename(fnfgv000:fnfgv006)
000700161220     f                                     usropn extfile(WFGV6)
000800161220     FFNFGW01L  IF   E           K DISK    usropn extfile(WFGW1)
000900940916     FTABEL00F  IF   E           K DISK
001000121211     d
001100121211     D FE0             S              3    DIM(301)
001200161220     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001300940916     D*
001400121211
001500121211     D fnls29ds        DS
001600121211     d* Terminal di partenza
001700121211     D  I29TFP                 1      3  0
001800121211     c* Numero foglio viaggio: se = 0 tutti i fogli aperti del terminal
001900121211     D  I29nfv                 4      9  0
002000121211     D  o29err                10     10
002100161220     D  I29tfa                11     13  0
002200161220     D  I29lna                14     16  0
002300161220     D  I29azione             17     17
002400161220
002500161220      * Parametri per richiamo a TRUL09R
002600161220     d Trul09ds      e ds
002700161220     d  w2liv                 19    108  0 dim(30)
002800121211     d*
002900121211     d
003000960319     D                 DS
003100121211     D  wFGVLNA                1      3
003200121211     D  FGVFFV                 4    243
003300121211     D  FGVFF2               244    450
003400121211     D  FGWFF3               451    690
003500121211     D  FGWFF4               691    900
003600121211     D  VUOTO                901    903
003700121211     D  FFV                    1    903
003800121211     D                                     DIM(301)
003900960319     D                 DS
004000121211     D  WFLPLNA                1      3
004100121211     D  FGVFLP                 4    243
004200121211     D  FGVFL2               244    450
004300121211     D  FGWFL3               451    690
004400121211     D  FGWFL4               691    900
004500121211     D  VUOTO2               901    903
004600121211     D  FLP                    1    903
004700121211     D                                     DIM(301)
004800020807      *
004900020807     D KPJBA         E DS
005000121211     D fnlv55ds      E DS
005100121211     D dstv          E DS
005200121211     d I               s              4  0
005300121211     d XX              s              4  0
005400121211     d w05nfv          s              5  0
005500121211     d w003a           s              3
005600121211     d wmsg            s             80
005700121211     d cod             s                   like(tblcod)
005800121211     d key             s                   like(tblkey)
005900161220     D hi              C                   CONST(X'22')
006000161220     D red             C                   CONST(X'28')
006100161220     d c_DspAtrNorm    c                   const(x'20')
006200161220     d WFGV1           s             21    inz('FILTRA201 /FNFGV01L')
006300161220     d WFGV6           s             21    inz('FILTRA201 /FNFGV06L')
006400161220     d WFGW1           s             21    inz('FILTRA201 /FNFGW01L')
006500020807      ****************************************************************
006600020807      *  RIEPILOGO INDICATORI
006700020807      *****************************************************************
006800000000     C     *ENTRY        PLIST
006900000000     C                   PARM                    KPJBA
007000161220     C                   PARM                    o29msg
007100121211     C                   MOVEL     KPJBU         fnls29ds
007200161220     c                   clear                   o29msg          250
007300161220     c                   open(e)   fnfgv01l
007400161220     c                   if        not %open(fnfgv01l)
007500161220     c                   eval      %subst(WFGV1:7:3)='PRD'
007600161220     c                   eval      %subst(WFGV6:7:3)='PRD'
007700161220     c                   eval      %subst(WFGW1:7:3)='PRD'
007800161220     c                   open(e)   fnfgv01l
007900161220     C                   ENDIF
008000161220     c                   open(e)   fnfgW01l
008100161220     c                   open(e)   fnfgv06l
008200161220     C
008300161220     c
008400931209     C*
008500121211     c* Se passato il foglio controllo solo lui, altrimenti tutti gli aperti
008600161220    1c                   if        i29tfp=0
008700121211     c                   eval      o29err='E'
008800121211     c                   eval      o29msg=msg(1)
008900121211     c                   goto      fine
009000161220    1c                   endif
009100121211     c
009200161220    1c                   if        i29nfv>0
009300121211     c                   eval      w05nfv=i29nfv
009400121211     c     kfgv01        chain     fnfgv01l
009500161220    2c                   if        not %found(fnfgv01l)
009600121211     c                   eval      o29err='E'
009700121211     c                   eval      o29msg=msg(1)
009800121211     c                   goto      fine
009900161220    2c                   endif
010000121211     c
010100121217     c     kfgw          chain     fnfgw01l
010200161220    2c                   if        not %found(fnfgw01l)
010300121217     c                   clear                   fgwfF3
010400121217     c                   clear                   fgwfF4
010500121217     c                   clear                   fgwfL3
010600121217     c                   clear                   fgwfL4
010700161220    2c                   endif
010800121217     C
010900121211     c                   exsr      contr_lin
011000121211
011100161220   x1c                   else
011200121211     c                   exsr      Readfgv06
011300161220    1c                   endif
011400971202     C*
011500971202     C                   MOVEL     'C'           D55TLA
011600971202     C                   CALL      'FNLV55R'
011700020807     C                   PARM                    FNLV55DS
011800161220     c*
011900161220     c                   eval      i09tla='C'
012000161220     c                   call      'TRUL09R'
012100161220     c                   parm                    trul09ds
012200161220     C*
012300950710     C*
012400121211     c     fine          tag
012500121211     C                   MOVEL     FNLS29DS      KPJBU
012600020807     C                   MOVEL     *ON           *INLR
012700940916     C**************************************************************************
012800940916     C*   R O U T I N E     I N I Z I A L E
012900940916     C**************************************************************************
013000940916     C     *INZSR        BEGSR
013100940916     C*
013200121211     C     Kfgv01        klist
013300121211     C                   kfld                    w05nfv
013400121211     C                   kfld                    i29tfp
013500121217     C     Kfgw          klist
013600121217     C                   kfld                    fgvnfv
013700121217     C                   kfld                    fgvlnp
013800121211     C     KTAB          KLIST
013900940916     C                   KFLD                    CODUT
014000121211     C                   KFLD                    COD
014100121211     C                   KFLD                    KEY
014200121211     c
014300121211     c                   z-add     1             codut             1 0
014400941229     C*
014500940916     C                   ENDSR
014600121211     C**************************************************************************
014700121211     C*  Lettura LF    fgv06l che esclude gli annullati e i fogli chiusi
014800121211     C**************************************************************************
014900121211     C     Readfgv06     BEGSR
015000121211     c
015100121211     c     i29tfp        setll     fnfgv06l
015200121211     c     i29tfp        reade     fnfgv06l
015300121211     c
015400121211     c                   dow       not %eof(fnfgv06l)
015500121211     c
015600121217     c     kfgw          chain     fnfgw01l
015700121217     c                   if        not %found(fnfgw01l)
015800121217     c                   clear                   fgwfF3
015900121217     c                   clear                   fgwfF4
016000121217     c                   clear                   fgwfL3
016100121217     c                   clear                   fgwfL4
016200121217     c                   endif
016300121217     c
016400121211     c                   exsr      contr_lin
016500121211     c
016600121211     c     i29tfp        reade     fnfgv06l
016700121211     c                   enddo
016800121211     c
016900121211     C                   ENDSR
017000121211     C**************************************************************************
017100121211     C*  controllo linee
017200121211     C**************************************************************************
017300121211     C     Contr_lin     BEGSR
017400161220     c                   clear                   wfoglio           1
017500121211     c* Escludo i foggli inoltro e defluenza
017600121211     C                   MOVEL     'TV'          COD
017700121211     C                   MOVEL(P)  FGVTTR        KEY
017800121211     C     KTAB          CHAIN     TABEL                              83
017900121211     C  N83              MOVEL     TBLUNI        DSTV
018000121211      *
018100121211    0c                   if        §tvdef<>'S' and fgvttr<>'I'
018200121211     c                   eval      wflplna=%editc(fgvlna:'X')
018300121211     c                   eval      wfgvlna=%editc(fgvlna:'X')
018400121211     c
018500121211     C                   Z-ADD     1             I
018600121211     C*
018700121211    1C     I             DOWLE     301
018800121211     C     ffv(I)        ANDGT     *ZEROS
018900121211     c* llop sulle linee per determinare il terminal di arrivo
019000121211     C                   CLEAR                   FNLV55DS
019100121211     C                   MOVEL     'A'           D55TPT
019200121211     C                   MOVE      ffv(I)        D55LIN
019300121211     C                   MOVE      i29tfp        D55LNP
019400121211     C                   MOVE      fgvDFV        D55DRF
019500121211     C                   CALL      'FNLV55R'
019600121211     C                   PARM                    FNLV55DS
019700121211     c
019800121211     c                   eval      fe0(I)=%editc(d55tfa:'X')
019900121211     c                   add       1             I
020000121211     c                   enddo
020100121211     C*
020200121211     C*
020300121211     C                   Z-ADD     1             I
020400121211    1C     I             DOWLE     301
020500121211     C     ffv(I)        ANDGT     *ZEROS
020600121211     C**
020700121211     C* SE LA LINEA E' UN SECONDO LIVELLO IN ARRIVO E NON SCARICA A
020800121211     C* SE' STESSA DEVE ESSERE PRESENTE COME LINEA ANCHE IL SUO TER-ARR
020900121211   2AC     ffv(I)        IFNE      FE0(I)
021000121211     C     ffv(I)        ANDNE     flp(I)
021100121211     C     FE0(I)        LOOKUP    ffv                                    31
021200121211   2BC     *IN31         IFEQ      *OFF
021300121211     C* ERRORE
021400121211     C                   eval      wmsg=msg(04)
021500161220     C                   eval      %subst(wmsg:6:3)=ffv(I)
021600161220     C                   eval      %subst(wmsg:53:3)=fe0(I)
021700161220     c                   if        wfoglio=' '
021800161220     c                   eval      o29msg=%trim(o29msg)+hi+'Foglio '+
021900121211     c                             %editc(fgvlnp:'X') + ' ' +
022000161220     c                             %editc(fgvnfv:'4') +':'+ c_DspAtrNorm+
022100121211     c                             wmsg
022200161220     c                   eval      wfoglio='1'
022300161220     c                   else
022400161220     c                   eval      o29msg=%trim(o29msg)+';'+wmsg
022500161220     c                   endif
022600121211     c
022700121211   2BC                   ENDIF
022800121211   2AC                   ENDIF
022900121211   c
023000121211     C                   Z-ADD     1             XX
023100121211   2AC     ffv(I)        IFNE      flp(I)
023200121211     C                   MOVEL     FE0(I)        W003A
023300121211     C     W003A         LOOKUP    FE0(XX)                                31
023400121211    3C     *IN31         DOWEQ     *ON
023500121211    4C     flp(XX)       IFNE      flp(I)
023600121211     C     ffv(XX)       ANDNE     flp(XX)
023700121211     C     flp(XX)       ORNE      flp(I)
023800121211     C     ffv(XX)       ANDEQ     FE0(XX)
023900121211     C* ERRORE
024000121211     C                   eval      wmsg=msg(03)
024100161220     C                   eval      %subst(wmsg:6:3)=ffv(I)
024200161220     C                   eval      %subst(wmsg:10:3)=ffv(XX)
024300161220     c                   if        wfoglio=' '
024400161220     c                   eval      o29msg=%trim(o29msg)+hi+'Foglio '+
024500121211     c                             %editc(fgvlnp:'X') + ' ' +
024600161220     c                             %editc(fgvnfv:'4') +':'+ c_DspAtrNorm+
024700121211     c                             wmsg
024800161220     c                   eval      wfoglio='1'
024900161220     c                   else
025000161220     c                   eval      o29msg=%trim(o29msg)+';'+wmsg
025100161220     c                   endif
025200121211     C*
025300121211    cc                   setoff                                       31
025400121211     c                   else
025500121211     c                   add       1             xx
025600121211     C     W003A         LOOKUP    FE0(XX)                                31
025700121211    4C                   ENDIF
025800121211    3C                   ENDDO
025900121211   2AC                   ENDIF
026000121211     C*
026100121211     C                   ADD       1             I
026200121211    1C                   ENDDO
026300161220
026400161220     c* se deve essere elimiata una linea, verifico se presente
026500161220     c*  (con i suoi secondi livelli)
026600161220    1c                   if        i29tfa>0 and i29azione='E'
026700161220     c                   clear                   trul09ds
026800161220     c                   eval      i09tfa=i29tfa
026900161220     c                   eval      i09dta=fgvdfv
027000161220     c                   eval      i09mod='TFA'
027100161220     c                   call      'TRUL09R'
027200161220     c                   parm                    trul09ds
027300161220     c
027400161220     c* verifico se presenti le filiali
027500161220     C                   Z-ADD     1             XX
027600161220    2C                   dow       w2liv(xx)>0
027700161220     C                   MOVEL     W2LIV(XX)     W003A
027800161220     C
027900161220    3C                   IF        %LOOKUP(W003A:FFV)>0
028000161220     c                   if        wfoglio=' '
028100161220     C                   eval      wmsg=msg(05)
028200161220     C                   eval      %subst(wmsg:10:3)=%editc(w2liv(xx):'X')
028300161220     c                   eval      o29msg=%trim(o29msg)+hi+'Foglio '+
028400161220     c                             %editc(fgvlnp:'X') + ' ' +
028500161220     c                             %editc(fgvnfv:'4') +':'+ c_DspAtrNorm+
028600161220     c                             wmsg
028700161220     c                   eval      wfoglio='1'
028800161220     c                   else
028900161220     c                   eval      o29msg=%trim(o29msg)+';'+
029000161220     c                             %editc(w2liv(xx):'X')
029100161220     c                   endif
029200161220    3c                   endif
029300161220     c                   add       1             xx
029400161220    2c                   enddo
029500161220     c                   endif
029600161220    1c                   if        i29lna>0 and i29azione='E'
029700161220     C                   MOVEL     i29lna        W003A
029800161220     C
029900161220    2C                   IF        %LOOKUP(W003A:FFV)>0
030000161220     C                   eval      wmsg=msg(05)
030100161220     C                   eval      %subst(wmsg:10:3)=%editc(i29lna:'X')
030200161220     c                   eval      o29msg=%trim(o29msg)+hi+'Foglio '+
030300161220     c                             %editc(fgvlnp:'X') + ' ' +
030400161220     c                             %editc(fgvnfv:'4') +':'+ c_DspAtrNorm+
030500161220     c                             wmsg
030600161220    2c                   endif
030700161220    1c                   endif
030800161220     C
030900161220    0C                   ENDIF
031000161220     c
031100121211     C                   ENDSR
031200121211**
031300121211Non trovato foglio viaggio
031400121211Terminal di partenza non impostato
031500161220Fil. xxx/xxx hanno stesso ter.arrivo ma non stessa fil.scarico
031600161220Fil. XXX deve essere nel foglio col suo term.arrivo xxx
031700161220Presente XXX
