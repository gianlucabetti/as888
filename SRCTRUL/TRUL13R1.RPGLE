000100030221     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200030221
000300030326      * TRUL13R  *---------------------------------------------------*
000400030326      * - CALCOLA PRIMO GIORNO LAVORATIVO DISPONIBILE
000500111206      * -  con controllo dei giorni di chiusura
000600020412      *--------------------------------------------------------------*
000700030221
000800030326     fAzcln01l  if   e           k disk
000900030224
001000111206     D MES             S              1    DIM(31)
001100020412
001200111206     d xgiolavds     e ds
001300111206     d Trul13ds      e ds
001400111206     d Trul13ds1     e ds
001500111206     d sav13ds       e ds                  EXTNAME(TRUL13DS) PREFIX(S_)
001600111206     d wgiro           s              2  0
001700020412
001800111206      *---------------------------------------------------------------
001900111206     C     KCLN          KLIST
002000111206     C                   KFLD                    CLNTFP
002100111206     C                   KFLD                    CLNTFA
002200111206     C                   KFLD                    CLNANN
002300111206     C                   KFLD                    CLNMES
002400020412      *---------------------------------------------------------------
002500030221     c     *Entry        Plist
002600030326     c                   Parm                    Trul13ds
002700111206     c                   Parm                    Trul13ds1
002800030326
002900020412
003000030326      * I13Tla = ' '  Elaboro e chiudo con RETURN
003100030326      * I13Tla = 'L'  Elaboro e chiudo con LR
003200030326      * I13Tla = 'C'            chiudo con LR  solo per chiusura file
003300030221
003400030326If  1c                   If        I13Tla = 'C'
003500111206     c                   call      'TRUL13R'
003600111206     c                   parm                    trul13ds
003700111206     c
003800030221     c                   Eval      *InLr = *On
003900020412
004000030221   x1c                   Else
004100111206     c
004200111206     c* Intanto richiamo TRUL13 per il calcolo della data
004300111206     c                   call      'TRUL13R'
004400111206     c                   parm                    trul13ds
004500111206     c
004600111206     c* Se  sono passati i giorni di chiusura
004700111206     c*     per ora eseguo solo per il "prima del"
004800111206     c
004900111206    2c                   if        (i13gc1<>*blanks or i13gc2<>*blanks)
005000111206     c                             and i13tcr='P'
005100111206     C                   EVAL      SAV13DS=TRUL13DS
005200111206     c                   eval      wgiro=1
005300111206     C
005400111206    3c                   do        2             wgiro
005500111206     c                   clear                   wturno            1
005600111206     c* eseguo controllo sul giorno precedente
005700111206     c                   Clear                   xgiolavds
005800111206     c                   Eval      Ixglpa  = S_I13MOD
005900111206     c                   Eval      IxglFil = S_I13FIL
006000111206     c                   Eval      Ixgldata = S_O13DFI
006100111206     c                   Eval      Ixglsub = 'S'
006200111206     c                   Eval      Ixgllav = 'S'
006300111206     c                   eval      ixglgg  = 1
006400111206     c                   Call      'XGIOLAV'
006500111206     c                   parm                    xgiolavds
006600111206     C
006700111206     c* Chain sul calendario con =000/000 per prendere la numerazione dei giorni della settimana del mese
006800111206     c*   della data ora calcolata
006900111206     C                   Z-ADD     oxgldata      WMMGG             4 0
007000111206     C                   Z-ADD     oxgldata      WG                2 0
007100111206     C                   MOVEL     oxgldata      clnann
007200111206     C                   Z-ADD     0             CLNTFP
007300111206     C                   Z-ADD     0             CLNTFA
007400111206     C                   MOVEL     WMMGG         clnmes
007500111206     C     KCLN          CHAIN     AZCLN01L                           31
007600111206     C                   MOVEA     CLNMAT        MES
007700111206     C                   MOVE      MES(WG)       WGG               1
007800111206     c
007900111206     C                   IF        i13GC1 <> *BLANKS
008000111206     C                   IF        %subst(i13gc1: 1: 1) = WGG and
008100111206     C                             %subst(i13gc1: 2: 1) = ' '
008200111206     c* Chiuso per turno
008300111206     c                   eval      Wturno='S'
008400111206     c                   endif
008500111206     c                   endif
008600111206     c
008700111206     c                   if        Wturno=' ' and i13gc2<>*blanks
008800111206     C                   IF        %subst(i13gc2: 1: 1) = WGG and
008900111206     C                             %subst(i13gc2: 2: 1) = ' '
009000111206     c* Chiuso per turno
009100111206     c                   eval      Wturno='S'
009200111206     c                   endif
009300111206     c                   endif
009400111206     c
009500111206     c* chiuso per turno--> come data finale devo tenere la data calcolata-1
009600111206     c                   if        Wturno='S'
009700111206     c* al primo giro memerizzo la data, al secondo non metto proprio la consegna richiesta
009800111206     c                   if        wgiro=1
009900111206     c                   eval      S_O13DFI=oxgldata
010000111206     c                   else
010100111206     c                   clear                   s_o13dfi
010200111206     c                   eval      s_o13err='E'
010300111206     c                   leave
010400111206     c                   endif
010500111206     c
010600111206     c                   else
010700111206     c* Se tutto ok esco dal ciclo
010800111206     c                   leave
010900111206     c                   endif
011000111206     c
011100111206    3C                   ENDdo
011200111206     c
011300111206     c                   eval      trul13ds=sav13ds
011400111206    2C                   ENDif
011500111206     c
011600111206     c
011700030326     c     Fine          Tag
011800030326
011900030326    2c                   If        I13Tla = 'L'
012000030326     c                   Eval      *InLR = *On
012100030326   X2c                   Else
012200030326     c                   Return
012300030326    2c                   EndIf
012400030326
012500030326    1c                   EndIf
