000100001120     H DECEDIT('0,') DATEDIT(*YMD.)
000200001120
000300001120      *****************************************************************
000400001120      *                                                               *
000500030210      *       Trascodifica spese incasso da LIT. a EURO               *
000600001120      *                                                               *
000700001120      *****************************************************************
000800001120
000900030210     fCnind00f  uf   e           k Disk
001000030210     fCnaco00f  uf   e           k Disk
001100030210
001200030210     d Kut             s                   Like(IndKut) Inz(1)
001300030210     d Kcc             s                   Like(IndKcc) Inz(151)
001400030210
001500030210     d w0030           s              3  0
001600030211     d w0052           s              5  2
001700030211     d w0053           s              5  3
001800030210
001900030210     d AzuteDs       e ds                  ExtName(AzUte00F)
002000030210     d DDatiUte      e ds
002100030210     d Tibs34Ds      e ds                  Inz
002200030210     d Tibs56Ds      e ds
002300030210     d  skfel                  7    756  0 dim(250)
002400030210
002500030210      *------------------------------------------------------------------------*
002600030210
002700030210      * Reperisco i dati utente
002800030210     c     *dtaara       define    §azute        AzuteDs
002900030210     c     *dtaara       define    §datiute      DDatiUte
003000030210
003100030210     c                   in(E)     *dtaara
003200030210     c                   IF        %Error or RSUT = *blanks
003300030210     c                   call      'TIBS34R'
003400030210     c                   parm                    Tibs34Ds
003500030210     c                   in        *dtaara
003600030210     c                   ENDIF
003700030210
003800030210      * Carico una sk con tutti i p.o. dell'AS
003900030210     c                   Clear                   Tibs56Ds
004000030210     c                   Eval      I56ppo = Simfel
004100030210     c                   Eval      I56mod = 'POA'
004200030210     c                   Call      'TIBS56R'
004300030210     c                   Parm                    Tibs56Ds
004400030210
004500030210     c     kInd          Klist
004600030210     c                   Kfld                    Kut
004700030210     c                   Kfld                    Kcc
004800030210
004900030210     c     kAco          Klist
005000030210     c                   Kfld                    IndKut
005100030210     c                   Kfld                    IndKcc
005200030210     c                   Kfld                    IndKsc
005300021216
005400030210     c     kInd          Setll     Cnind00f
005500021216     c                   Do        *Hival
005600030210     c     kInd          Reade     Cnind00f
005700030210      * Fine file
005800030210     c                   If        %Eof(Cnind00f)
005900021216     c                   Leave
006000021216     c                   EndIf
006100030210      * Rileggo se annullato
006200030210     c                   If        IndFlg <> *Blanks
006300030210     c                   Iter
006400030210     c                   EndIf
006500030210      * Rileggo se non ha spese incasso
006600030210     c                   If        IndSin = *Zeros
006700030210     c                   Iter
006800030210     c                   EndIf
006900030210      * Converto spese incasso
007000030211     c     IndSin        Div(h)    1936,27       w0052
007100030211     c                   z-add     w0052         w0053
007200030210     c                   move      w0053         IndSin
007300030210      * Sfleggo solo quelli dell'As
007400030210     c                   Movel     IndKsc        w0030
007500030210     c     w0030         Lookup    Skfel                                  30
007600030210     c                   If        *In30
007700030210     c     kAco          Chain     Cnaco00f
007800030210     c                   If        %Found(Cnaco00f)
007900030210     c                   Clear                   AcoFtr
008000030210     c                   Update    Cnaco000
008100030210     c                   EndIf
008200030210     c                   EndIf
008300030210      * Aggiorno Cnind00f
008400030210     c                   Update    Cnind000
008500021216     c                   EndDo
008600021216
008700021216     c                   Eval      *InLr = *On
