000100060808      *===============================================================*
000200060808      *?TISIDXR * Estrazione nostro cappario per DPD (batch)         ?*
000300060808      *===============================================================*
000400060808
000500060808     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060808
000700060808      *---------------------------------------------------------------*
000800060808
000900060808     fAZCPC01L  if   e           k disk
001000060808     fAZORG01L  if   e           k disk
001100060808      *
001200060808     fWFCAPXDPD o    e             disk
001300060808
001400060808      *---------------------------------------------------------------*
001500060808
001600060808      *
001700060808      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001800060808      *
001900060808      *
002000060808      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002100060808      *
002200060808      *
002300060808      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002400060808      *
002500060808     d KPJBA         e ds
002600060808      *
002700060808      * - Parametri x Controllo profilo utenti
002800060808     d TIBS34ds      e ds
002900060808      * - Ds di riferimento al file esterno AZUTE00F
003000060808     d AZUTEds       e ds                  extname(AZUTE00F)
003100060808      * - Ds per dati organigramma
003200060808     d DDatiUte      e ds
003300060808      *
003400060808      * DS di procedura
003500060808     d TISI00ds      e ds                  inz
003600060808      *
003700060808      * 143ª descrizione dell'organigramma
003800060808     d Og143         e ds                  inz
003900060808      *
004000060808     d Status         sds           333
004100060808     d   SDSpgm          *proc
004200060808      *
004300060808      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
004400060808      *
004500060808      * - Flags
004600060808     d $Fine           s              1    inz(*off)
004700060808     d $RecOK          s              1    inz(*off)
004800060808      *
004900060808      * Packing - List - - - - - - - - - - - - - - - - - - - - - - - -*
005000060808      *
005100060808     c     K02CPC01      klist
005200060808     c                   kfld                    D00ver
005300060808     c                   kfld                    D00nar
005400060808
005500060808      *---------------------------------------------------------------*
005600060808      * I N D I C A T O R I                                           *
005700060808      *---------------------------------------------------------------*
005800060808      *  10     - Comodo                                              *
005900060808      *---------------------------------------------------------------*
006000060808
006100060808     c     *Entry        plist
006200060808     c                   parm                    KPJBA
006300060808     c                   movel     KPJBU         TISI00ds
006400060808      *
006500060808      * Operazioni Iniziali
006600060808     c                   exsr      RoutInz
006700060808      *
006800060808      * Ciclo di lettura file AZCPC01L e scrittura in WrkF WFCAPXDPD
006900060808do  1c                   DOW       $Fine   = *off
007000060808     c                   exsr      Read_AZCPC
007100060808if  2c                   if        $RecOK  = *on
007200060808     c                   exsr      Wrt_CAPXDPD
007300060808e   2c                   endif
007400060808e   1c                   ENDDO
007500060808      *
007600060808      * Fine
007700060808     c                   movel     *on           *inLR
007800060808      *
007900060808      *---------------------------------------------------------------*
008000060808      * Operazioni Iniziali                                           *
008100060808      *---------------------------------------------------------------*
008200060808     c     RoutInz       BEGSR
008300060808      *
008400060808      * Reperisco dati job
008500060808     c                   exsr      DatiJob
008600060808      *
008700060808     c*** già così:      eval      $Fine   = *off
008800060808     c*** già così:      eval      $RecOK  = *off
008900060808      *
009000060808      * Posizionamento iniziale nel file AZCPC01L
009100060808     c     K02CPC01      setll     AZCPC000
009200060808     c                   if        NOT  %equal(AZCPC01L)
009300060808     c                   eval      $Fine   = *on
009400060808     c                   endif
009500060808      *
009600060808     c                   ENDSR
009700060808      *
009800060808      *---------------------------------------------------------------*
009900060808      * Reperimento Dati del job (Utente/Operativi)                   *
010000060808      *---------------------------------------------------------------*
010100060808     c     DatiJob       BEGSR
010200060808      *
010300060808     c     *dtaara       define    §azute        azuteds
010400060808     c     *dtaara       define    §datiute      ddatiute
010500060808      *
010600060808     c                   in(E)     *dtaara
010700060808     c                   IF        %ERROR or RSUT = *blanks
010800060808     c                   clear                   Tibs34Ds
010900060808     c                   call      'TIBS34R'
011000060808     c                   parm                    Tibs34Ds
011100060808     c                   in        *dtaara
011200060808     c                   ENDIF
011300060808      *
011400060808     c                   ENDSR
011500060808      *
011600060808      *---------------------------------------------------------------*
011700060808      * Lettura file AZCPC00F                                         *
011800060808      *---------------------------------------------------------------*
011900060808     c     Read_AZCPC    BEGSR
012000060808      *
012100060808     c                   eval      $RecOK  = *off
012200060808      *
012300060808     c     K02CPC01      reade     AZCPC000
012400060808      *
012500060808sel 1c                   SELECT
012600060808      * E.o.F.
012700060808w   1c                   WHEN      %eof(AZCPC01L)
012800060808     c                   eval      $Fine   = *on
012900060808     c                   leavesr
013000060808      * Scarta i records annullati
013100060808w   1c                   WHEN      CPCatb <> *blanks
013200060808     c                   leavesr
013300060808      * Scarta le nazioni diverse
013400060808      * (nazione - *blank - già in chiave di lettura!)
013500060808w   1c***                WHEN      CPCnar <> D00nar
013600060808     c***                leavesr
013700060808      * Omette i CAP non lunghi 5 bytes (se nazione Italia)
013800060808w   1c                   WHEN           D00nar  = *blanks
013900060808     c                             and  %len(%trim(CPCcap)) <> 5
014000060808     c                   leavesr
014100060808      * Seleziona CAP ..... (se impostato tra i parametri)
014200060808w   1c                   WHEN           D00cap <> *blanks
014300060808     c                             and  CPCcap <> D00cap
014400060808     c                   leavesr
014500060808      * Seleziona provincia (se impostata tra i parametri)
014600060808w   1c                   WHEN           D00prv <> *blanks
014700060808     c                             and  CPCprv <> D00prv
014800060808     c                             and  CPCpra <> D00prv
014900060808     c                   leavesr
015000060808e   1c                   ENDSL
015100060808      *
015200060808      * Selezione superata!
015300060808     c                   eval      $RecOK  = *on
015400060808      *
015500060808      * Reperimento codice Depot
015600060808     c                   clear                   Og143
015700060808     c     CPClna        chain     AZORG
015800060808if  1c                   if             %found(AZORG01L)
015900060808     c                             and  ORGfva = *blanks
016000060808     c                   movel     ORGde3        Og143
016100060808e   1c                   endif
016200060808      *
016300060808     c                   ENDSR
016400060808      *
016500060808      *---------------------------------------------------------------*
016600060808      * Scrittura record nel WrkF WFCAPXDPD                           *
016700060808      *---------------------------------------------------------------*
016800060808     c     Wrt_CAPXDPD   BEGSR
016900060808      *
017000060808     c                   clear                   WFCAPDPD
017100060808      *
017200060808     c                   eval      WCDvers  = CPCver
017300101125     c                   eval      WCDprv   = CPCprv
017400101125     c                   eval      WCDpra   = CPCpra
017500060808     c                   eval      WCDcap   = CPCcap
017600060808     c                   eval      WCDdepot = §OGdp1
017700101125     c                   eval      WCDfi1   = CPCfi1
017800060808      *                  __________________
017900060808     c                   WRITE     WFCAPDPD
018000060808      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
018100060808     c                   ENDSR
