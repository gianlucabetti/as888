000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500030213      *   Scrivo note 05 06 del potenziale su ksc se non esistono              *
000600010213      *                                                                        *
000700010213      **************************************************************************
000800010213
000900030213     fCnaco00f  if   e           k Disk
001000030213     fTfntc01l  if a e           k Disk
001100030213     fQsysprt   o    f  132        Printer
001200030213
001300030213     d kApl            s                   Like(NtcApl)
001400030213     d kNk1            s                   Like(NtcNk1)
001500030213     d kNk2            s                   Like(NtcNk2) Inz
001600030213     d kTnt            s                   Like(NtcTnt)
001700030213
001800030213     d new05           s              5  0 Inz
001900030213     d new06           s              5  0 Inz
002000010213
002100030213     d AzuteDs       e ds                  ExtName(AzUte00F)
002200030213     d DDatiUte      e ds
002300030213     d Tibs34Ds      e ds                  Inz
002400030213     d Tibs56Ds      e ds
002500030213     d  skfel                  7    756  0 dim(250)
002600010213
002700010213
002800030213      *------------------------------------------------------------------------*
002900030213
003000030213      * Reperisco i dati utente
003100030213     c     *dtaara       define    §azute        AzuteDs
003200030213     c     *dtaara       define    §datiute      DDatiUte
003300030213
003400030213     c                   in(E)     *dtaara
003500030213     c                   IF        %Error or RSUT = *blanks
003600030213     c                   call      'TIBS34R'
003700030213     c                   parm                    Tibs34Ds
003800030213     c                   in        *dtaara
003900030213     c                   ENDIF
004000030213
004100030213      * Carico una sk con tutti i p.o. dell'AS
004200030213     c                   Clear                   Tibs56Ds
004300030213     c                   Eval      I56ppo = Simfel
004400030213     c                   Eval      I56mod = 'POA'
004500030213     c                   Call      'TIBS56R'
004600030213     c                   Parm                    Tibs56Ds
004700030213
004800030213     c     kNtc          Klist
004900030213     c                   Kfld                    kApl
005000030213     c                   Kfld                    kNk1
005100030213     c                   Kfld                    kNk2
005200030213     c                   Kfld                    kTnt
005300010213
005400030213Do  1c                   Do        *Hival
005500030213     c                   Read      Cnaco00f
005600030213      * Fine file
005700030213     c                   If        %Eof(Cnaco00f)
005800030213     c                   Leave
005900030213     c                   EndIf
006000030213      * Rileggo se annullato
006100030213     c                   If        Acoflg <> *Blanks
006200030213     c                   Iter
006300030213     c                   EndIf
006400030213      * Rileggo se non è un p.o. presente sull'AS
006500030213     c     AcoFlt        Lookup    Skfel                                  30
006600030213     c                   If        Not *In30
006700030213     c                   Iter
006800030213     c                   EndIf
006900030213      * Rileggo se non ha un codice potenziale
007000030213     c                   If        AcoLib = *Zeros
007100030213     c                   Iter
007200030213     c                   EndIf
007300030213
007400030213      * Scrivo la nota 05 se non esiste già
007500030213     c                   Eval      kApl = 'C'
007600030213     c                   Movel     AcoKcc        kNk1
007700030213     c                   Move      AcoKsc        kNk1
007800030213     c                   Eval      kTnt = '05'
007900030213     c     kNtc          Setll     Tfntc01l
008000030213If  2c                   If        Not %Equal(Tfntc01l)
008100030213     c                   Eval      kApl = 'P'
008200030213     c                   Movel     AcoLib        kNk1
008300030213     c     kNtc          Setll     Tfntc01l
008400030213Do  3c                   Do        *Hival
008500030213     c     kNtc          Reade     Tfntc01l
008600030213     c                   If        %Eof(Tfntc01l)
008700030213     c                   Leave
008800030213     c                   EndIf
008900030213     c                   If        NtcFlt = 'A'
009000030213     c                   Iter
009100030213     c                   EndIf
009200030213     c                   Eval      NtcApl = 'C'
009300030214     c                   Movel     AcoKcc        NtcNk1
009400030214     c                   Move      AcoKsc        NtcNk1
009500030214     c                   Clear                   NtcFlt
009600030214     c                   Clear                   NtcRnt
009700030213     c                   Write     Tfntc
009800030213     c                   Add       1             New05
009900030213    3c                   EndDo
010000030213    2c                   EndIf
010100030213      * Scrivo la nota 06 se non esiste già
010200030213     c                   Eval      kApl = 'C'
010300030213     c                   Movel     AcoKcc        kNk1
010400030213     c                   Move      AcoKsc        kNk1
010500030213     c                   Eval      kTnt = '06'
010600030213     c     kNtc          Setll     Tfntc01l
010700030213If  2c                   If        Not %Equal(Tfntc01l)
010800030213     c                   Eval      kApl = 'P'
010900030213     c                   Movel     AcoLib        kNk1
011000030213     c     kNtc          Setll     Tfntc01l
011100030213Do  3c                   Do        *Hival
011200030213     c     kNtc          Reade     Tfntc01l
011300030213     c                   If        %Eof(Tfntc01l)
011400030213     c                   Leave
011500030213     c                   EndIf
011600030213     c                   If        NtcFlt = 'A'
011700030213     c                   Iter
011800030213     c                   EndIf
011900030213     c                   Eval      NtcApl = 'C'
012000030214     c                   Movel     AcoKcc        NtcNk1
012100030214     c                   Move      AcoKsc        NtcNk1
012200030214     c                   Clear                   NtcFlt
012300030214     c                   Clear                   NtcRnt
012400030213     c                   Write     Tfntc
012500030213     c                   Add       1             New06
012600030213    3c                   EndDo
012700030213    2c                   EndIf
012800030213
012900030213     c                   EndDo
013000030213
013100030213     c                   Except    Stampa
013200030213
013300030213     c                   Eval      *InLr = *On
013400030213
013500030213     oQsysprt   e            Stampa         3 02
013600030211     o                       Rsut                20
013700030213     o                                         +  5 'Note 05 e 06 del'
013800030213     o                                         +  1 'potenziale riportate sul'
013900030213     o                                         +  1 'cliente'
014000030213     o                                         +  5 'P.O.'
014100030213     o                       Simfel            +  1
014200030213     o                                          113 'TNVRCPOR4'
014300030213     o                       Udate         y    127
014400030213     o          E            Stampa         2
014500030213     o                                           10 'Note 05'
014600030213     o                       New05         2   +  2
014700030213     o                                         +  2 'Note 06'
014800030213     o                       New06         2   +  1
