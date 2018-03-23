000100050202      *PARMS CLOSQLCSR(*ENDACTGRP)  DBGVIEW(*SOURCE)
000200050202
000300050202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400050202     h alwnull(*inputonly)
000500050202
000600050202      *---------------------------------------------------------------*
000700050202
000800050202     fTITAS30C  if   e           k disk
000900050202     fFNDCL01L  if   e           k disk
001000050202     fFNDPT01L  if   e           k disk
001100050202     fFNDCF01L  if   e           k disk
001200050202      *
001300051017     fWFsca00F  o    e             disk
001400050202
001500050202      *---------------------------------------------------------------*
001600050202
001700050202      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001800050202     d Status         sds
001900050202     d  SDSpgm           *proc
002000050202     D  SDSpar           *parms
002100050202     d  SDSusr               254    263
002200050202      * - Parametri x Controllo profilo utenti
002300050202     d TIBS34DS      e ds                  inz
002400050202      * - Ds di riferimento al file esterno AZUTE00F
002500050202     d AZUTEDS       e ds                  extname(AZUTE00F)
002600050202      * - Ds per dati organigramma
002700050202     d DDatiUte      e ds
002800050202      * - Dati di FNDCT00F estratti via SQL
002900050202     d FNDCTds       e ds                  extname(FNDCT00F)  inz
003000050202      * - Dati di TITAS30C
003100050202     d TITASds       e ds                  extname(TITAS00F)  inz
003200050202      * - Dati di FNDCL01L
003300050202     d FNDCLds       e ds                  extname(FNDCL00F)  inz
003400050202      * - Dati di FNDPT01L
003500050202     d FNDPTds       e ds                  extname(FNDPT00F)  inz
003600050202      * - Dati di FNDCF01L
003700050202     d FNDCFds       e ds                  extname(FNDCF00F)  inz
003800050202      *
003900050202      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
004000050202     d $EoF            s              1    inz(*off)
004100050202     d $Err            s              1    inz(*off)
004200050202      * - Campi di comodo
004300051028     d dataiso         s               d   datfmt(*iso)
004400050202
004500050202      *---------------------------------------------------------------*
004600050202
004700050202      * Predisposizione x indicatori di tipo record del file TITAS
004800050202     iTITAS000      21
004900050202     iTITAS010      22
005000050202     iTITASP00      23
005100050202
005200050202      *---------------------------------------------------------------*
005300050202      *  RIEPILOGO INDICATORI                                         *
005400050202      *---------------------------------------------------------------*
005500050202      * 10    - Comodo                                                *
005600050202      *---------------------------------------------------------------*
005700050202      *
005800050202      * Operazioni Iniziali
005900050202     c                   exsr      OperazIniz
006000050202     c                   exsr      OpenCursor
006100050202      *
006200050202      * Ciclo di elaborazione
006300050202do  1c                   dou       $Eof  = *on
006400050202     c                   exsr      ReadCursor
006500050202e   1c                   enddo
006600050202      *
006700050202      * Fine programma
006800050202     c                   exsr      CloseCursor
006900050202     c                   eval      *inLR = *on
007000050202      *
007100050202      *****************************************************************
007200050202      **?Operazioni iniziali                                        ?**
007300050202      *****************************************************************
007400050202     c     OperazIniz    BEGSR
007500050202      *
007600050202      * Definizione Key-List
007700050202      * - TITAS30C
007800050202     c     K04TAS30      klist
007900050202     c                   kfld                    DCTaas
008000050202     c                   kfld                    DCTlnp
008100050202     c                   kfld                    DCTnrs
008200050202     c                   kfld                    DCTnsp
008300050202      * - FNDCT01L, FNDCL01L, FNDPT01L, FNDCF01L
008400050202     c     K03DCT01      klist
008500050202     c                   kfld                    DCTaac
008600050202     c                   kfld                    DCTfil
008700050202     c                   kfld                    DCTnca
008800050202      * - FNDCF01L
008900050202     c     K05DCF01      klist
009000050202     c                   kfld                    DCTaac
009100050202     c                   kfld                    DCTfil
009200050202     c                   kfld                    DCTnca
009300050202     c                   kfld                    DCFdfc
009400050202     c                   kfld                    DCFhfc
009500050202      *
009600050202      * Reperisco dati job
009700051028     c                   TIME                    dataiso
009800051028      *
009900050202     c     *dtaara       define    §azute        azuteds
010000050202     c     *dtaara       define    §datiute      ddatiute
010100050202      *
010200050202     c                   in(E)     *dtaara
010300050202if  1c                   if        %ERROR or RSUT = *blanks
010400050202     c                   clear                   Tibs34Ds
010500050202     c                   call      'TIBS34R'
010600050202     c                   parm                    Tibs34Ds
010700050202     c                   in        *dtaara
010800050202e   1c                   endif
010900050202      *
011000050202     c                   ENDSR
011100050202
011200050202      *****************************************************************
011300050202      **?Apertura cursore.                                          ?**
011400050202      *****************************************************************
011500050202     c     OpenCursor    BEGSR
011600050202      *
011700050202     c/EXEC SQL
011800050202     c+ declare A1 cursor for
011900050202     c+ select  *
012000050202     c+ from    FNDCT02L
012100050202     c+ where   DCTatb =  ' '
012200050202     c+   and   DCTaas >= 2002
012300050202     c+ order   by DCTaas, DCTlnp, DCTnrs, DCTnsp
012400050202     c+ for     FETCH  only
012500050202     c/END-EXEC
012600050202      *
012700050202     c/EXEC SQL
012800050202     c+ open A1
012900050202     c/END-EXEC
013000050202      *
013100050202     c                   ENDSR
013200050202
013300050202      *****************************************************************
013400050202      **?Lettura cursore.                                           ?**
013500050202      *****************************************************************
013600050202     c     ReadCursor    BEGSR
013700050202      *
013800050202     c                   clear                   FNDCTds
013900050202      *
014000050202     c/EXEC SQL
014100050202     c+ fetch next from A1 into :FNDCTds
014200050202     c/END-EXEC
014300050202      *
014400050202sel 1c                   select
014500050202w   1c                   when      SQLcod = 100
014600050202     c                   eval      $EoF   = *on
014700050202w   1c                   when      SQLcod < 0
014800050202     c                   eval      $Err   = *on
014900050202     c                   eval      $Eof   = *on
015000050202x   1c                   other
015100050202     c                   exsr      ElabRec
015200050202e   1c                   endsl
015300050202      *
015400050202     c                   ENDSR
015500050202
015600050202      *****************************************************************
015700050202      **?Chiusura cursore.                                          ?**
015800050202      *****************************************************************
015900050202     c     CloseCursor   BEGSR
016000050202      *
016100050202     c/EXEC SQL
016200050202     c+ close A1
016300050202     c/END-EXEC
016400050202      *
016500050202     c                   ENDSR
016600050202
016700050202      *****************************************************************
016800050202      **?Elaborazione singolo record da FNDCT00F                    ?**
016900050202      *****************************************************************
017000050202     c     ElabRec       BEGSR
017100050202      *
017200051017     c                   clear                   TITASds
017300051017     c                   clear                   FNDCLds
017400051017     c                   clear                   FNDPTds
017500051017     c                   clear                   FNDCFds
017600051017      *
017700050202      * Aggancio della bolla x n° spedizione
017800050202     c     K04TAS30      chain     TITAS30C
017900050202      *
018000050202      * Aggancio i dati della liquidazione della C.A.
018100050202     c     K03DCT01      chain     FNDCL000
018200050202      *
018300050202      * Aggancio i dati del mandato assicurativo
018400051017     c     K03DCT01      CHAIN     FNDPT000
018500051017if  1c                   if        %found(FNDPT01L) and DPTatb <> ' '
018600051017     c                   clear                   FNDPTds
018700050202e   1c                   endif
018800050202      *
018900050202      * Aggancio i dati dell'ultima fase della C.A.
019000050202     c                   eval      DCFdfc = *hival
019100050202     c                   eval      DCFhfc = *hival
019200050202     c     K05DCF01      setgt     FNDCF000
019300050202     c     K03DCT01      readpe    FNDCF000
019400050202      *
019500050202      * Impostazione altri campi del WrkF
019600051024      *
019700051024      *    Tipo gestione
019800051018     C                   IF        DCTfpr = 'P' and
019900051018     c                             %subst(DCTflo:12:1) = 'F'
020000051018     c                   eval      TipoGest = 'F'
020100051018     C                   else
020200051018     c                   eval      TipoGest = DCTfpr
020300051017     c                   ENDIF
020400051024      *    Affidata AIG
020500051024     C                   eval      AffidAIG = %subst(DCTflo:13:1)
020600051024      *    Tipo ass.ne
020700050202sel 1c                   select
020800051017w   1c                   when          DPTftc = *blanks
020900051017     c                             and TASias =  *zeros
021000051017     c                   eval      TipoAss = *blanks
021100051017w   1c                   when          TASias > *ZEROS
021200051017w   1c                             or  DPTftc = 'C'
021300051017     c                             or  DPTftc = '8'
021400060315     c                             or  DPTftc = 'd'
021500050202     c                   eval      TipoAss = 'A'
021600051017w   1c                   when          DPTftc = 'R'
021700051017     c                             or  DPTftc = '9'
021800050202     c                   eval      TipoAss = 'R'
021900050202e   1c                   endsl
022000050202      *
022100051028     c                   eval      utente = DUTUTE
022200051028     c                   move      dataiso       dataelab
022300051028      *
022400050202      * Scrittura record nel WrkF
022500051018     c                   write     WFSCA000
022600050202      *
022700050202     c                   ENDSR
