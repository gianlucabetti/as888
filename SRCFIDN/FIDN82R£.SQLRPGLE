000100050202      *PARMS CLOSQLCSR(*ENDACTGRP)  DBGVIEW(*SOURCE)
000200050202
000300050202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400050202     h alwnull(*inputonly)
000500050202
000600050202      *---------------------------------------------------------------*
000700050202
000800050202     fTITAS30C  if   e           k disk
000900111021     fTNTMD01L  if   e           k disk
001000050202     fFNDCL01L  if   e           k disk
001100050202      *
001200111021     fWFeex00F  o    e             disk
001300050202
001400050202      *---------------------------------------------------------------*
001500050202
001600050202      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700050202     d Status         sds
001800050202     d  SDSpgm           *proc
001900050202     D  SDSpar           *parms
002000050202     d  SDSusr               254    263
002100050202      * - Parametri x Controllo profilo utenti
002200050202     d TIBS34DS      e ds                  inz
002300050202      * - Ds di riferimento al file esterno AZUTE00F
002400050202     d AZUTEDS       e ds                  extname(AZUTE00F)
002500050202      * - Ds per dati organigramma
002600050202     d DDatiUte      e ds
002700050202      * - Dati di FNDCT00F estratti via SQL
002800050202     d FNDCTds       e ds                  extname(FNDCT00F)  inz
002900050202      * - Dati di TITAS30C
003000050202     d TITASds       e ds                  extname(TITAS00F)  inz
003100111021      * - Dati di TNTMD01L
003200111021     d TNTMDds       e ds                  extname(TNTMD00F)  inz
003300050202      * - Dati di FNDCL01L
003400050202     d FNDCLds       e ds                  extname(FNDCL00F)  inz
003500050202      *
003600050202      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
003700050202     d $EoF            s              1    inz(*off)
003800050202     d $Err            s              1    inz(*off)
003900050202      * - Campi di comodo
004000051028     d dataiso         s               d   datfmt(*iso)
004100050202
004200050202      *---------------------------------------------------------------*
004300050202
004400050202
004500050202      *---------------------------------------------------------------*
004600050202      *  RIEPILOGO INDICATORI                                         *
004700050202      *---------------------------------------------------------------*
004800050202      * 10    - Comodo                                                *
004900050202      *---------------------------------------------------------------*
005000050202      *
005100050202      * Operazioni Iniziali
005200050202     c                   exsr      OperazIniz
005300050202     c                   exsr      OpenCursor
005400050202      *
005500050202      * Ciclo di elaborazione
005600050202do  1c                   dou       $Eof  = *on
005700050202     c                   exsr      ReadCursor
005800050202e   1c                   enddo
005900050202      *
006000050202      * Fine programma
006100050202     c                   exsr      CloseCursor
006200050202     c                   eval      *inLR = *on
006300050202      *
006400050202      *****************************************************************
006500050202      **?Operazioni iniziali                                        ?**
006600050202      *****************************************************************
006700050202     c     OperazIniz    BEGSR
006800050202      *
006900050202      * Definizione Key-List
007000050202      * - TITAS30C
007100050202     c     K04TAS30      klist
007200050202     c                   kfld                    DCTaas
007300050202     c                   kfld                    DCTlnp
007400050202     c                   kfld                    DCTnrs
007500050202     c                   kfld                    DCTnsp
007600111021      * Definizione Key-List
007700111021      * - Tntmd01l
007800111021     c     K04Tmd01      klist
007900111021     c                   kfld                    DCTaas
008000111021     c                   kfld                    DCTlnp
008100111021     c                   kfld                    DCTnrs
008200111021     c                   kfld                    DCTnsp
008300050202      * - FNDCT01L, FNDCL01L, FNDPT01L, FNDCF01L
008400050202     c     K03DCT01      klist
008500050202     c                   kfld                    DCTaac
008600050202     c                   kfld                    DCTfil
008700050202     c                   kfld                    DCTnca
008800050202      *
008900050202      * Reperisco dati job
009000051028     c                   TIME                    dataiso
009100051028      *
009200050202     c     *dtaara       define    §azute        azuteds
009300050202     c     *dtaara       define    §datiute      ddatiute
009400050202      *
009500050202     c                   in(E)     *dtaara
009600050202if  1c                   if        %ERROR or RSUT = *blanks
009700050202     c                   clear                   Tibs34Ds
009800050202     c                   call      'TIBS34R'
009900050202     c                   parm                    Tibs34Ds
010000050202     c                   in        *dtaara
010100050202e   1c                   endif
010200050202      *
010300050202     c                   ENDSR
010400050202
010500050202      *****************************************************************
010600050202      **?Apertura cursore.                                          ?**
010700050202      *****************************************************************
010800050202     c     OpenCursor    BEGSR
010900050202      *
011000050202     c/EXEC SQL
011100050202     c+ declare A1 cursor for
011200050202     c+ select  *
011300050202     c+ from    FNDCT02L
011400050202     c+ where   DCTatb =  ' '
011500111021     c+   and   DCTaas >= 2010
011600111021     c+   and   DCTfpr = 'P'
011700111021     c+   and   DCTres = 'P'
011800111021     c+   and   DCTdch > 0
011900111021     c+   and   substr(dctflo, 1, 3) = 'FEX'
012000111021     c+   and   substr(dctflo, 7, 1) = 'P'
012100050202     c+ order   by DCTaas, DCTlnp, DCTnrs, DCTnsp
012200050202     c+ for     FETCH  only
012300050202     c/END-EXEC
012400050202      *
012500050202     c/EXEC SQL
012600050202     c+ open A1
012700050202     c/END-EXEC
012800050202      *
012900050202     c                   ENDSR
013000050202
013100050202      *****************************************************************
013200050202      **?Lettura cursore.                                           ?**
013300050202      *****************************************************************
013400050202     c     ReadCursor    BEGSR
013500050202      *
013600050202     c                   clear                   FNDCTds
013700050202      *
013800050202     c/EXEC SQL
013900050202     c+ fetch next from A1 into :FNDCTds
014000050202     c/END-EXEC
014100050202      *
014200050202sel 1c                   select
014300050202w   1c                   when      SQLcod = 100
014400050202     c                   eval      $EoF   = *on
014500050202w   1c                   when      SQLcod < 0
014600050202     c                   eval      $Err   = *on
014700050202     c                   eval      $Eof   = *on
014800050202x   1c                   other
014900050202     c                   exsr      ElabRec
015000050202e   1c                   endsl
015100050202      *
015200050202     c                   ENDSR
015300050202
015400050202      *****************************************************************
015500050202      **?Chiusura cursore.                                          ?**
015600050202      *****************************************************************
015700050202     c     CloseCursor   BEGSR
015800050202      *
015900050202     c/EXEC SQL
016000050202     c+ close A1
016100050202     c/END-EXEC
016200050202      *
016300050202     c                   ENDSR
016400050202
016500050202      *****************************************************************
016600050202      **?Elaborazione singolo record da FNDCT00F                    ?**
016700050202      *****************************************************************
016800050202     c     ElabRec       BEGSR
016900050202      *
017000111021     c                   eval      dctapc = (dctaac*10000) + dctmgc
017100111021
017200051017     c                   clear                   TITASds
017300111021     c                   clear                   TNTmdds
017400051017     c                   clear                   FNDCLds
017500051017      *
017600050202      * Aggancio della bolla x n° spedizione
017700050202     c     K04TAS30      chain     TITAS30C
017800111021      *
017900111021      * Verifico la data spedizione che deve essere maggiore del 1/7/2010
018000111021     c                   If        Tasaas = 2010  and tasmgs < 0701
018100111021     c                   leavesr
018200111021     c                   endif
018300111021     c                   eval      dctdsp = (tasaas*10000) + tasmgs
018400111021      *
018500111021      * Aggancio anagrafica  x n° spedizione
018600111021     c     K04TMD01      chain     TNTMD01L
018700050202      *
018800050202      * Aggancio i dati della liquidazione della C.A.
018900050202     c     K03DCT01      chain     FNDCL000
019000050202      *
019100051028     c                   eval      utente = DUTUTE
019200051028     c                   move      dataiso       dataelab
019300051028      *
019400050202      * Scrittura record nel WrkF
019500111021     c                   write     WFEEX000
019600050202      *
019700050202     c                   ENDSR
