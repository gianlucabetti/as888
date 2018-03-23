000100050406     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050406
000300050406      *------------------------------------------------------------------------*
000400050406      *
000500050406      *  Crea work file statistica danni per bilancio
000600050406      *
000700050406      *------------------------------------------------------------------------*
000800050406
000900050406     fFidn80d   cf   e             workstn
001000050406
001100050406      *------------------------------------------------------------------------*
001200050406      *  RIEPILOGO INDICATORI
001300050406      *------------------------------------------------------------------------*
001400050406      * 28 - ERRORE Generico su Dspf
001500050406      * 40 - PC wfdab10
001600050406      * 41 - PC wfdab20
001700050406      * 42 - PC data spedizione dal
001800050406      * 43 - PC data spedizione al
001900050406      * 44 - PC data apertura ca dal
002000050406      * 45 - PC data apertura ca al
002100050406      *------------------------------------------------------------------------*
002200050406
002300050406      *------------------------------------------------------------------------*
002400050406      *   V A R I A B I L I
002500050406      *------------------------------------------------------------------------*
002600050406     d wdtal           s              8  0
002700050406     d wdtalinv        s              8  0
002800050406     d wdtdal          s              8  0
002900050406     d wdtdalinv       s              8  0
003000050406
003100050406      *------------------------------------------------------------------------*
003200050406      *   S C H I E R E
003300050406      *------------------------------------------------------------------------*
003400050406     d msg             s             78    dim(10) ctdata perrcd(1)
003500050406
003600050406      *------------------------------------------------------------------------*
003700050406      *   D S   I N T E R N E / E S T E R N E
003800050406      *------------------------------------------------------------------------*
003900050406     d wlbdat          ds                  inz
004000050406     d  g02dat                 1      8  0
004100050406     d  g02inv                 9     16  0
004200050406     d  g02err                17     17
004300050406     d  g02tgi                18     22  0
004400050406
004500050408     d azuteds       e ds                  extname(AZUTE00F)
004600050408     d ddatiute      e ds
004700050406     d fidn80ds      e ds                  inz
004800050406     d kpjba         e ds
004900050408     d tibs34ds      e ds                  inz
005000050406
005100050406     d                sds
005200050406     d  Vtcpgm                 1     10
005300050406
005400050406      *------------------------------------------------------------------------*
005500050406
005600050406      * Pulizia Videata
005700050406     c                   ExSr      Sr_Pulvid
005800050406
005900050406      * Emissione Videata
006000050406     c                   Do        *Hival
006100050406     c                   Exfmt     dn80d01
006200050406
006300050406      * F3=Fine
006400050406     c   kc              Leave
006500050406
006600050406     c                   Setoff                                       28
006700050406
006800050406      * Controlli videata
006900050406     c                   ExSr      Sr_Contr
007000050406     c   28              Iter
007100050406
007200050406      * F6=Conferma
007300050406     c                   If        *Inkf
007400050406     c                   ExSr      Sr_Conferma
007500050406     c                   Leave
007600050406     c                   EndIf
007700050406
007800050406     c                   EndDo
007900050406
008000050406     c                   Eval      *InLr = *On
008100050406      *------------------------------------------------------------------------*
008200050406      * PULIZIA VIDEATA
008300050406      *------------------------------------------------------------------------*
008400050406     c     Sr_Pulvid     BegSr
008500050406
008600050406     c                   Clear                   v1dab1
008700050406     c                   Clear                   v1dab2
008800060426     c                   Clear                   v1dab3
008900060426     c                   Clear                   v1dab4
009000080325     c                   clear                   v1dab5
009100050406     c                   Clear                   v1dspd
009200050406     c                   Clear                   v1dspa
009300050406     c                   Clear                   v1dcad
009400050406     c                   Clear                   v1dcaa
009500050406
009600050406     c                   EndSr
009700050406      *------------------------------------------------------------------------*
009800050406      * CONTROLLI VIDEO
009900050406      *------------------------------------------------------------------------*
010000050406     c     Sr_Contr      BegSr
010100050406
010200050406      * Reset indicatori
010300050406     c                   setoff                                       4041
010400050406     c                   setoff                                       4243
010500050406     c                   setoff                                       4445
010600050406
010700050406      * DATA SPEDIZIONE
010800050406     c                   Clear                   wdtdalinv
010900050406     c                   Clear                   wdtalinv
011000050406      * --> controllo la data inserita 'dal'
011100050406     c                   Eval      wdtdal = v1dspd
011200050406     c                   ExSr      Sr_Datadal
011300050406     c   28              Eval      *In42 = *On
011400050406     c   28              Leavesr
011500050406     c                   Eval      v1dspd = wdtdal
011600050406
011700050406      * --> data 'al' se non impostata è = a data 'da'
011800050406     c                   If        v1dspa = *Zeros
011900050406     c                   Eval      v1dspa = v1dspd
012000050406     c                   Eval      wdtalinv = wdtdalinv
012100050406     c                   Else
012200050406
012300050406      * --> controllo la data inserita 'al'
012400050406     c                   Eval      wdtal = v1dspa
012500050406     c                   ExSr      Sr_Dataal
012600050406     c   28              Eval      *In43 = *On
012700050406     c   28              Leavesr
012800050406     c                   Eval      v1dspa = wdtal
012900050406     c                   EndIf
013000050406
013100050406      * --> range
013200050406     c                   If        wdtdalinv > wdtalinv
013300050406     c                   Eval      *In28 = *On
013400050406     c                   Eval      *In42 = *On
013500050406     c                   Eval      v1cmsg = msg(3)
013600050406     c                   Leavesr
013700050406     c                   EndIf
013800050406
013900050406      * --> salvo nella ds
014000050406     c                   Eval      d80spd = wdtdalinv
014100050406     c                   Eval      d80spa = wdtalinv
014200050406
014300050406      * DATA APERTURA CA
014400050406     c                   Clear                   wdtdalinv
014500050406     c                   Clear                   wdtalinv
014600050406      * --> controllo la data inserita 'dal'
014700050406     c                   Eval      wdtdal = v1dcad
014800050406     c                   ExSr      Sr_Datadal
014900050406     c   28              Eval      *In44 = *On
015000050406     c   28              Leavesr
015100050406     c                   Eval      v1dcad = wdtdal
015200050406
015300050406      * --> data 'al' se non impostata è = a data 'da'
015400050406     c                   If        v1dcaa = *Zeros
015500050406     c                   Eval      v1dcaa = v1dcad
015600050406     c                   Eval      wdtalinv = wdtdalinv
015700050406     c                   Else
015800050406
015900050406      * --> controllo la data inserita 'al'
016000050406     c                   Eval      wdtal = v1dcaa
016100050406     c                   ExSr      Sr_Dataal
016200050406     c   28              Eval      *In45 = *On
016300050406     c   28              Leavesr
016400050406     c                   Eval      v1dcaa = wdtal
016500050406     c                   EndIf
016600050406
016700050406      * --> range
016800050406     c                   If        wdtdalinv > wdtalinv
016900050406     c                   Eval      *In28 = *On
017000050406     c                   Eval      *In44 = *On
017100050406     c                   Eval      v1cmsg = msg(3)
017200050406     c                   Leavesr
017300050406     c                   EndIf
017400050406
017500050406      * --> salvo nella ds
017600050406     c                   Eval      d80cad = wdtdalinv
017700050406     c                   Eval      d80caa = wdtalinv
017800050406
017900050408      * FILE DA ELABORARE
018000050408      * --> almeno 1 scelta
018100050408     c                   If        v1dab1 = *Blanks and v1dab2 = *Blanks and
018200080325     c                             v1dab3 = *Blanks and v1dab4 = *Blanks and
018300080325     c                             v1dab5 = *blanks
018400050408     c                   Eval      *In28 = *On
018500050408     c                   Eval      *In40 = *On
018600050408     c                   Eval      v1cmsg = msg(4)
018700050408     c                   Leavesr
018800050408     c                   EndIf
018900050406
019000050406     c                   EndSr
019100050406      *------------------------------------------------------------------------*
019200050406      * CONTROLLO DATA DAL
019300050406      *------------------------------------------------------------------------*
019400050406     c     Sr_Datadal    BegSr
019500050406
019600050406     c                   If        wdtdal = *Zeros
019700050406     c                   Eval      *In28 = *On
019800050406     c                   Eval      v1cmsg = msg(2)
019900050406     c                   Leavesr
020000050406     c                   EndIf
020100050406
020200050406     c                   Clear                   wlbdat
020300050406     c                   Z-add     wdtdal        g02dat
020400050406     c                   Call      'XSRDA8'
020500050406     c                   Parm                    wlbdat
020600050406     c                   If        g02err = '1'
020700050406     c                   Eval      *In28 = *On
020800050406     c                   Eval      v1cmsg = msg(3)
020900050406     c                   Leavesr
021000050406     c                   EndIf
021100050406     c                   Z-add     g02dat        wdtdal
021200050406     c                   Z-add     g02inv        wdtdalinv
021300050406
021400050406     c                   EndSr
021500050406      *------------------------------------------------------------------------*
021600050406      * CONTROLLO DATA AL
021700050406      *------------------------------------------------------------------------*
021800050406     c     Sr_Dataal     BegSr
021900050406
022000050406     c                   Clear                   wlbdat
022100050406     c                   Z-add     wdtal         g02dat
022200050406     c                   call      'XSRDA8'
022300050406     c                   Parm                    wlbdat
022400050406     c                   If        g02err = '1'
022500050406     c                   Eval      *In28 = *On
022600050406     c                   Eval      v1cmsg = msg(3)
022700050406     c                   Leavesr
022800050406     c                   EndIf
022900050406     c                   Z-add     g02dat        wdtal
023000050406     c                   Z-add     g02inv        wdtalinv
023100050406
023200050406     c                   EndSr
023300050406      *------------------------------------------------------------------------*
023400050406      * RICHIAMA IL PROGRAMMA DI CREAZIONE FILE
023500050406      *------------------------------------------------------------------------*
023600050406     c     Sr_Conferma   BegSr
023700050406
023800060426     c                   eval      d80ab1 = v1dab1
023900060426     c                   eval      d80ab2 = v1dab2
024000060426     c                   eval      d80ab3 = v1dab3
024100060426     c                   eval      d80ab4 = v1dab4
024200080325     c                   eval      d80ab5 = v1dab5
024300060426
024400050406     c                   Movel(p)  Fidn80ds      kpjbu
024500050406     c                   Eval      kcoaz = 'DN81'
024600050408     c**!!!              Call      'FIDN81C'
024700050408     c**!!!              Parm                    kpjba
024800050408     c                   Call      'BCH10'
024900050408     c                   Parm                    kpjba
025000050406
025100050406     c                   EndSr
025200050406      *------------------------------------------------------------------------*
025300050406      * ROUTINE INIZIALE
025400050406      *------------------------------------------------------------------------*
025500050406     c     *Inzsr        BegSr
025600050406
025700050406     c     *Entry        Plist
025800050406     c                   Parm                    kpjba
025900050408
026000050408     c     *dtaara       define    §azute        azuteds
026100050408     c     *dtaara       define    §datiute      ddatiute
026200050408
026300050408     c                   in(E)     *dtaara
026400050408     c                   IF        %ERROR or RSUT = *blanks
026500050408     c                   clear                   Tibs34Ds
026600050408     c                   call      'TIBS34R'
026700050408     c                   parm                    Tibs34Ds
026800050408     c                   in        *dtaara
026900050408     c                   ENDIF
027000050406
027100050406     c                   EndSr
027200050406
027300050406** MSG
027400060426
027500050406Inserire data                                                                       2
027600050406Data errata                                                                         3
027700050406Effettuare almeno 1 scelta                                                          4
027800060426
