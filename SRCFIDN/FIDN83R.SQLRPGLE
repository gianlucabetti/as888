000100050202      *PARMS CLOSQLCSR(*ENDACTGRP)  DBGVIEW(*SOURCE)
000200050202
000300050202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400050202     h alwnull(*inputonly)
000500050202
000600050202      *---------------------------------------------------------------*
000700050202
000800050202     fTITAS30C  if   e           k disk
000900111021     fTNTMD01L  if   e           k disk
001000050202      *
001100111117     fWFiex00F  o    e             disk
001200050202
001300050202      *---------------------------------------------------------------*
001400050202
001500050202      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001600050202     d Status         sds
001700050202     d  SDSpgm           *proc
001800050202     D  SDSpar           *parms
001900050202     d  SDSusr               254    263
002000050202      * - Parametri x Controllo profilo utenti
002100050202     d TIBS34DS      e ds                  inz
002200050202      * - Ds di riferimento al file esterno AZUTE00F
002300050202     d AZUTEDS       e ds                  extname(AZUTE00F)
002400050202      * - Ds per dati organigramma
002500050202     d DDatiUte      e ds
002600050202      * - Dati di FNDCT00F estratti via SQL
002700050202     d FNDCTds       e ds                  extname(FNDCT00F)  inz
002800050202      * - Dati di TITAS30C
002900050202     d TITASds       e ds                  extname(TITAS00F)  inz
003000111021      * - Dati di TNTMD01L
003100111021     d TNTMDds       e ds                  extname(TNTMD00F)  inz
003200050202      * - Dati di FNDCL01L
003300050202     d FNDCLds       e ds                  extname(FNDCL00F)  inz
003400050202      *
003500050202      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
003600050202     d $EoF            s              1    inz(*off)
003700050202     d $Err            s              1    inz(*off)
003800050202      * - Campi di comodo
003900051028     d dataiso         s               d   datfmt(*iso)
004000050202
004100050202      *---------------------------------------------------------------*
004200050202
004300050202
004400050202      *---------------------------------------------------------------*
004500050202      *  RIEPILOGO INDICATORI                                         *
004600050202      *---------------------------------------------------------------*
004700050202      * 10    - Comodo                                                *
004800050202      *---------------------------------------------------------------*
004900050202      *
005000050202      * Operazioni Iniziali
005100050202     c                   exsr      OperazIniz
005200050202     c                   exsr      OpenCursor
005300050202      *
005400050202      * Ciclo di elaborazione
005500050202do  1c                   dou       $Eof  = *on
005600050202     c                   exsr      ReadCursor
005700050202e   1c                   enddo
005800050202      *
005900050202      * Fine programma
006000050202     c                   exsr      CloseCursor
006100050202     c                   eval      *inLR = *on
006200050202      *
006300050202      *****************************************************************
006400050202      **?Operazioni iniziali                                        ?**
006500050202      *****************************************************************
006600050202     c     OperazIniz    BEGSR
006700050202      *
006800050202      * Definizione Key-List
006900050202      * - TITAS30C
007000050202     c     K04TAS30      klist
007100050202     c                   kfld                    DCTaas
007200050202     c                   kfld                    DCTlnp
007300050202     c                   kfld                    DCTnrs
007400050202     c                   kfld                    DCTnsp
007500111021      * Definizione Key-List
007600111021      * - Tntmd01l
007700111021     c     K04Tmd01      klist
007800111021     c                   kfld                    DCTaas
007900111021     c                   kfld                    DCTlnp
008000111021     c                   kfld                    DCTnrs
008100111021     c                   kfld                    DCTnsp
008200050202      *
008300050202      * Reperisco dati job
008400051028     c                   TIME                    dataiso
008500051028      *
008600050202     c     *dtaara       define    §azute        azuteds
008700050202     c     *dtaara       define    §datiute      ddatiute
008800050202      *
008900050202     c                   in(E)     *dtaara
009000050202if  1c                   if        %ERROR or RSUT = *blanks
009100050202     c                   clear                   Tibs34Ds
009200050202     c                   call      'TIBS34R'
009300050202     c                   parm                    Tibs34Ds
009400050202     c                   in        *dtaara
009500050202e   1c                   endif
009600050202      *
009700050202     c                   ENDSR
009800050202
009900050202      *****************************************************************
010000050202      **?Apertura cursore.                                          ?**
010100050202      *****************************************************************
010200050202     c     OpenCursor    BEGSR
010300050202      *
010400050202     c/EXEC SQL
010500050202     c+ declare A1 cursor for
010600050202     c+ select  *
010700050202     c+ from    FNDCT02L
010800050202     c+ where   DCTatb =  ' '
010900111021     c+   and   DCTaas >= 2010
011000111021     c+   and   DCTfpr = 'P'
011100111117     c+   and   DCTres = 'V'
011200111021     c+   and   DCTdch > 0
011300111117     c+   and   substr(dctflo, 1, 3) = 'FIX'
011400111117     c+   and   dctcch = '06'
011500050202     c+ order   by DCTaas, DCTlnp, DCTnrs, DCTnsp
011600050202     c+ for     FETCH  only
011700050202     c/END-EXEC
011800050202      *
011900050202     c/EXEC SQL
012000050202     c+ open A1
012100050202     c/END-EXEC
012200050202      *
012300050202     c                   ENDSR
012400050202
012500050202      *****************************************************************
012600050202      **?Lettura cursore.                                           ?**
012700050202      *****************************************************************
012800050202     c     ReadCursor    BEGSR
012900050202      *
013000050202     c                   clear                   FNDCTds
013100050202      *
013200050202     c/EXEC SQL
013300050202     c+ fetch next from A1 into :FNDCTds
013400050202     c/END-EXEC
013500050202      *
013600050202sel 1c                   select
013700050202w   1c                   when      SQLcod = 100
013800050202     c                   eval      $EoF   = *on
013900050202w   1c                   when      SQLcod < 0
014000050202     c                   eval      $Err   = *on
014100050202     c                   eval      $Eof   = *on
014200050202x   1c                   other
014300050202     c                   exsr      ElabRec
014400050202e   1c                   endsl
014500050202      *
014600050202     c                   ENDSR
014700050202
014800050202      *****************************************************************
014900050202      **?Chiusura cursore.                                          ?**
015000050202      *****************************************************************
015100050202     c     CloseCursor   BEGSR
015200050202      *
015300050202     c/EXEC SQL
015400050202     c+ close A1
015500050202     c/END-EXEC
015600050202      *
015700050202     c                   ENDSR
015800050202
015900050202      *****************************************************************
016000050202      **?Elaborazione singolo record da FNDCT00F                    ?**
016100050202      *****************************************************************
016200050202     c     ElabRec       BEGSR
016300050202      *
016400111021     c                   eval      dctapc = (dctaac*10000) + dctmgc
016500111021
016600051017     c                   clear                   TITASds
016700111021     c                   clear                   TNTmdds
016800051017      *
016900050202      * Aggancio della bolla x n° spedizione
017000050202     c     K04TAS30      chain     TITAS30C
017100111021      *
017200111021      * Verifico la data spedizione che deve essere maggiore del 1/7/2010
017300111021     c                   If        Tasaas = 2010  and tasmgs < 0701
017400111021     c                   leavesr
017500111021     c                   endif
017600111021     c                   eval      dctdsp = (tasaas*10000) + tasmgs
017700111021      *
017800111021      * Aggancio anagrafica  x n° spedizione
017900111021     c     K04TMD01      chain     TNTMD01L
018000050202      *
018100050202      *
018200051028     c                   eval      utente = DUTUTE
018300051028     c                   move      dataiso       dataelab
018400051028      *
018500050202      * Scrittura record nel WrkF
018600111117     c                   write     WFIEX000
018700050202      *
018800050202     c                   ENDSR
