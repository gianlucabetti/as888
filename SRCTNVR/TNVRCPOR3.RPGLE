000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500030211      *          Metto il responsabile trasporti sul file note                 *
000600010213      *                                                                        *
000700010213      **************************************************************************
000800010213
000900030211     fTncpo00f  uf   e             Disk
001000030211     fTfntc01l  uf a e           k Disk
001100030211     fQsysprt   o    f  132        Printer
001200030211
001300030211     d kApl            s                   Like(NtcApl) Inz('P')
001400030211     d kNk1            s                   Like(NtcNk1)
001500030211     d kNk2            s                   Like(NtcNk2) Inz
001600030211     d kTnt            s                   Like(NtcTnt) Inz('05')
001700030211
001800030211     d new             s              5  0 Inz
001900030211     d old             s              5  0 Inz
002000010213
002100030211     d AzuteDs       e ds                  ExtName(AzUte00F)
002200030211     d DDatiUte      e ds
002300030211     d Tibs34Ds      e ds                  Inz
002400030211     d Tibs56Ds      e ds
002500030211     d  skfel                  7    756  0 dim(250)
002600010213
002700010213
002800030211      *------------------------------------------------------------------------*
002900030211
003000030211      * Reperisco i dati utente
003100030211     c     *dtaara       define    §azute        AzuteDs
003200030211     c     *dtaara       define    §datiute      DDatiUte
003300030211
003400030211     c                   in(E)     *dtaara
003500030211     c                   IF        %Error or RSUT = *blanks
003600030211     c                   call      'TIBS34R'
003700030211     c                   parm                    Tibs34Ds
003800030211     c                   in        *dtaara
003900030211     c                   ENDIF
004000030211
004100030211      * Carico una sk con tutti i p.o. dell'AS
004200030211     c                   Clear                   Tibs56Ds
004300030211     c                   Eval      I56ppo = Simfel
004400030211     c                   Eval      I56mod = 'POA'
004500030211     c                   Call      'TIBS56R'
004600030211     c                   Parm                    Tibs56Ds
004700030211
004800030211     c     kNtc          Klist
004900030211     c                   Kfld                    kApl
005000030211     c                   Kfld                    kNk1
005100030211     c                   Kfld                    kNk2
005200030211     c                   Kfld                    kTnt
005300010213
005400030211     c                   Do        *Hival
005500030211     c                   Read      Tncpo00f
005600030211      * Fine file
005700030211     c                   If        %Eof(Tncpo00f)
005800030211     c                   Leave
005900030211     c                   EndIf
006000030211      * Rileggo se annullato
006100030211     c                   If        CpoAtb <> *Blanks
006200030211     c                   Iter
006300030211     c                   EndIf
006400030211      * Rileggo se non ha il responsabile trasporti
006500030211     c                   If        CpoRst = *Blanks
006600030211     c                   Iter
006700030211     c                   EndIf
006800030211      * Rileggo se non è un p.o. presente sull'AS
006900030211     c     CpoFlt        Lookup    Skfel                                  30
007000030211     c                   If        Not *In30
007100030211     c                   Iter
007200030211     c                   EndIf
007300030211      * Scrivo la nota 05 se non esiste già
007400030211     c     kNtc          Chain(e)  Tfntc01l
007500030211If  5c                   If        Not %Found(Tfntc01l)
007600030211     c                   Clear                   Tfntc
007700030211     c                   Eval      NtcApl = 'P'
007800030211     c                   Movel     CpoCPo        NtcNk1
007900030211     c                   Eval      NtcTnt = '05'
008000030211     c                   Movel     CpoRst        NtcRnt
008100030211     c                   Eval      NtcSns = 'S'
008200030211     c                   Write     Tfntc
008300030211     c                   Add       1             New
008400030211     c                   Else
008500030211     c                   Add       1             Old
008600030211     c                   EndIf
008700030211      * Aggiorno Tncpo00f
008800030211     c                   Clear                   CpoRst
008900030211     c                   Clear                   CpoFtr
009000030211     c                   Update    Tncpo000
009100030211
009200030211     c                   EndDo
009300030211
009400030211     c                   Except    Stampa
009500030211
009600030211     c                   Eval      *InLr = *On
009700030211
009800030211     oQsysprt   e            Stampa         3 02
009900030211     o                       Rsut                20
010000030211     o                                         +  5 'Trascodifica Resp.Trasp.'
010100030211     o                                         +  5 'P.O.'
010200030211     o                       Simfel            +  1
010300030211     o                                          113 'TNVRCPOR3'
010400030211     o                       Udate         y    127
010500030211     o          E            Stampa         2
010600030211     o                                           10 'Nuove note'
010700030211     o                       New           2   +  2
010800030211     o                                         +  2 'Note già esistenti'
010900030211     o                       Old           2   +  1
