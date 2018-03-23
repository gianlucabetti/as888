000100050512     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200050511
000300050511      *------------------------------------------------------------------------*
000400050511      *
000500050511      *  Chiusura automatica C.A.
000600050511      *
000700050511      *------------------------------------------------------------------------*
000800050511
000900050511     fFndct01l  if   e           k Disk
001000050511     fFndcf01l  if   e           K Disk
001100050511     fQsysprt   o    f  132        printer oflind(*inof)
001200050511
001300050511      *------------------------------------------------------------------------*
001400050511      *  RIEPILOGO INDICATORI
001500050511      *------------------------------------------------------------------------*
001600050511      * *In99 = di comodo x stampa testata
001700050511
001800050511      *------------------------------------------------------------------------*
001900050511
002000050511      *------------------------------------------------------------------------*
002100050511      *   V A R I A B I L I
002200050511      *------------------------------------------------------------------------*
002300050511     d wdataca         s              8  0
002400050511     d wdataeur        s               d   datfmt(*eur)
002500050511     d wdatafase       s              8  0
002600050511     d wdataiso        s               d   datfmt(*iso)
002700050511     d wdtlimite       s              8  0
002800050511     d woggi           s              8  0
002900050511     d w0140           s             14  0
003000050511
003100050511      *------------------------------------------------------------------------*
003200050511      *   S C H I E R E
003300050511      *------------------------------------------------------------------------*
003400050511
003500050511      *------------------------------------------------------------------------*
003600050511      *   D S   I N T E R N E / E S T E R N E
003700050511      *------------------------------------------------------------------------*
003800050511     d parm01          ds
003900050511     d  paac                          4  0
004000050511     d  pcch                          2
004100050511
004200050511     d azuteds       e ds                  extname(AZUTE00F)
004300050511     d ddatiute      e ds
004400050511     d fidn22ds      e ds
004500050511     d kpjba         e ds
004600050511     d tibs34ds      e ds
004700050511
004800050511      *------------------------------------------------------------------------*
004900050511
005000050511      *------------------------------------------------------------------------*
005100050511
005200050511     c     paac          Setll     Fndct01l
005300050511     c                   Do        *Hival
005400050511     c     paac          Reade     Fndct01l
005500050511
005600050511     c                   If        %Eof(Fndct01l)
005700050511     c                   Leave
005800050511     c                   EndIf
005900050511
006000050511      * Elaboro solo le c.a. ancora aperte
006100050511     c                   If        dctdch <> *Zeros
006200050511     c                   Iter
006300050511     c                   EndIf
006400050511
006500050511      * Cerco la data in cui è stata eseguita l'ultima fase
006600050511     c     kfndct        Setgt     fndcf01l
006700050511     c     kfndct        Readpe    fndcf01l
006800050511     c     *iso          Move      dcfdfc        wdataeur
006900050511     c     *eur          Movel     wdataeur      wdatafase
007000050511
007100050511      * Se la fase è stata fatta prima di 13 mesi fa allora posso chiudere la c.a.
007200050511     c                   If        dcfdfc < wdtlimite
007300050511     c                   ExSr      Sr_Chiudi
007400050511      * Se la fase è stata fatta negli ultimi 13 mesi stampo la c.a. e non la chiudo
007500050511     c                   Else
007600050511     c                   ExSr      Sr_Stampa
007700050511     c                   EndIf
007800050511
007900050511     c                   EndDo
008000050511
008100050511     c                   Eval      *InLr = *On
008200050511
008300050511      *------------------------------------------------------------------------*
008400050511      * RICHIAMO IL PGM DI CHIUSURA  FIDN22R
008500050511      *------------------------------------------------------------------------*
008600050511     c     Sr_Chiudi     BegSr
008700050511
008800050511     c                   Clear                   fidn22ds
008900050511     c                   Eval      i22tri = 'R'
009000050511     c                   Eval      i22mod = 'C'
009100050511     c                   Eval      i22aac = dctaac
009200050511     c                   Eval      i22fil = dctfil
009300050511     c                   Eval      i22nca = dctnca
009400050511     c                   Eval      i22cch = pcch
009500170405     c*****              Eval      i22fgs = dctgfc
009600170405     c                   Eval      i22fgs = 046
009700050511     c                   Eval      kpjbu = fidn22ds
009800050516     c                   Call      'FIDN22R'
009900050516     c                   Parm                    kpjba
010000050511
010100050511     c                   EndSr
010200050511
010300050511      *------------------------------------------------------------------------*
010400050511      * STAMPO C.A.
010500050511      *------------------------------------------------------------------------*
010600050511     c     Sr_Stampa     BegSr
010700050511
010800050511     c                   If        Not *In99 or *InOf
010900050511     c                   Except    Testa
011000050511     c                   Eval      *In99 = *On
011100050511     c                   EndIf
011200050511
011300050511     c                   Move      dctmgc        wdataca
011400050511     c                   Movel     dctaac        wdataca
011500050511     c     *iso          Move      wdataca       wdataeur
011600050511     c     *eur          Movel     wdataeur      wdataca
011700050511
011800050511     c                   Except    Riga
011900050511
012000050511     c                   EndSr
012100050511
012200050511      *------------------------------------------------------------------------*
012300050511      * ROUTINE INIZIALE
012400050511      *------------------------------------------------------------------------*
012500050511     c     *Inzsr        BegSr
012600050511
012700050511     c     *Entry        Plist
012800050511     c                   Parm                    kpjba
012900050511
013000050511     c                   Movel     kpjbu         parm01
013100050511
013200050511     c     *dtaara       define    §azute        azuteds
013300050511     c     *dtaara       define    §datiute      ddatiute
013400050511
013500050511     c                   in(E)     *dtaara
013600050511     c                   IF        %ERROR or RSUT = *blanks
013700050511     c                   clear                   Tibs34Ds
013800050511     c                   call      'TIBS34R'
013900050511     c                   parm                    Tibs34Ds
014000050511     c                   in        *dtaara
014100050511     c                   ENDIF
014200050511
014300050511      * reperisco data e ora
014400050511     c                   Time                    w0140
014500050511     c                   Move      w0140         woggi
014600050511
014700050511     c     *eur          Movel     woggi         wdataeur
014800050511
014900050511      * alla data odierna devo togliere 13 mesi
015000050511     c                   subdur    13:*m         wdataeur
015100050511     c                   Movel     wdataeur      wdataiso
015200050511     c     *iso          Movel     wdataiso      wdtlimite
015300050511
015400050511     c     kfndct        klist
015500050511     c                   kfld                    dctaac
015600050511     c                   kfld                    dctfil
015700050511     c                   kfld                    dctnca
015800050511
015900050511     c                   EndSr
016000980923
016100050511     oqsysprt   e            Testa            01
016200050512     o                       rsut                20
016300050516     o                                           58 'Chiusura automatica '
016400050516     o                                        +   1 'C.A. per anno'
016500050516     o                       paac             +   1
016600050512     o                       *date         y    105
016700050512     o                                          120 'Pag.'
016800050512     o                       Page          z    125
016900050516     o          e            Testa            02
017000050516     o                                           58 'C.A. aperte con fasi'
017100050516     o                                        +   1 'eseguite'
017200050516     o                                        +   1 'negli ultimi 13 mesi'
017300050511     o          e            Testa          2 03
017400050511     o                                           11 'Numero C.A.'
017500050512     o                                         +  2 'Data C.A. '
017600050512     o                                         +  2 'Data Fase'
017700050511     o          e            Riga           1
017800050511     o                       dctfil               3
017900050511     o                       dctnca        z  +   1
018000050512     o                       wdataca          +   2 '  /  /    '
018100050512     o                       wdatafase        +   2 '  /  /    '
