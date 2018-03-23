000100060801      *PARMS CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER) DBGVIEW(*SOURCE)
000200060801      *===============================================================*
000300060803      *?TISITYR1 * Pulizia Destinatari Particolari & Sinonimi - batch?*
000400060801      *===============================================================*
000500060801      *
000600060801     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060801      *
000800060801      *?A R C H I V I?------------------------------------------------*
000900060801      *
001000060801      *
001100060801      *?C O S T A N T I?----------------------------------------------*
001200060801      *
001300060801      *
001400060801      *?S C H I E R E?------------------------------------------------*
001500060801      *
001600060801      *
001700060801      *?S T R U T T U R E   D A T I?----------------------------------*
001800060801      *
001900060801     d KPJBA         e ds
002000060801      *
002100060801      * - Parametri x Controllo profilo utenti
002200060801     d TIBS34ds      e ds                  inz
002300060801      * - Ds di riferimento al file esterno AZUTE00F
002400060801     d AZUTEds       e ds                  inz  extname(AZUTE00F)
002500060801      * - Ds per dati organigramma
002600060801     d DDatiUte      e ds                  inz
002700060801      *
002800060801      * - Dati estratti via SQL
002900060801     d AZDSTUds      e ds                  inz  extname(AZDSTU0F)
003000060803     d                                          prefix(U_)
003100060801      *
003200060801     d Status         sds
003300060801     d  SDSpgm           *proc
003400060801      *
003500060801      * Parametri ricevuti
003600060802     d TISITZds        ds                  inz
003700060802     d***PRMdos1                      1    inz
003800060802     d***PRMdos2                      1    inz
003900060802     d***PRMdos3                      1    inz
004000060802     d***PRMdos4                      1    inz
004100060802     d***PRMdos5                      1    inz
004200060802     d  $PRMdos                       1    inz  dim( 5)
004300060803     d  PRMnum                        7  0 inz
004400060913     d  PRMdiv                        8  0 inz
004500060801      *
004600060801      *?V A R I A B I L I?--------------------------------------------*
004700060801      *
004800060801      * - Flags
004900060801     d $EoF            s              1    inz(*off)
005000060801     d $Err            s              1    inz(*off)
005100060803     d $Upd            s              1    inz(*off)
005200060913     d $NoUpd1S        s              1    inz(*off)
005300060802      *
005400060802      * - Indici di schiera
005500060802     d xx              s              3  0 inz
005600060801      *
005700060801      * - Parametri per SQL
005800060802     d wDOS            s             20    inz
005900060802     d wSQL            s           2048    inz  VARYING
006000060922     d wCountRec       s              9  0 inz
006100060922     d W_DSTflo        s                   inz  like(U_DSTflo)
006200060802      *
006300060802      * - Codice destinatario
006400060803     d wSAVcod         s                   inz  like(U_DSTcod)
006500060801
006600060801      *===============================================================*
006700060802      *?I N D I C A T O R I                                          ?*
006800060802      *---------------------------------------------------------------*
006900060801      *  10     - Comodo                                              *
007000060801      *===============================================================*
007100060801      *
007200060801     c     *Entry        plist
007300060801     c                   parm                    KPJBA
007400060801      *
007500060801     c                   movel     KPJBU         TISITZds
007600060801      *
007700060801      * Operazioni iniziali
007800060801     c                   exsr      RoutInz
007900060802      *
008000060802      * Dichiarazione/Aperturea cursore
008100060802     c                   exsr      OpenCursor
008200060801      *
008300060801      * Ciclo di elaborazione fino a fine file
008400060801do  1c                   DOW       $EoF    = *off
008500060801      *
008600060802     c                   exsr      ReadCursor
008700060801      *
008800060801e   1c                   ENDDO                                                  *rottura serie
008900060802      *
009000060802      * Chiusura cursore
009100060802     c                   exsr      CloseCursor
009200060801      *
009300060801      * Fine
009400060801     c                   eval      *inLR = *on
009500060801      *
009600060801      *?S U B R O U T I N E S?----------------------------------------*
009700060801      *
009800060801      *---------------------------------------------------------------*
009900060801      *?Operazioni iniziali                                          ?*
010000060801      *---------------------------------------------------------------*
010100060801     c     RoutInz       BEGSR
010200060801      *
010300060801      * Reperimento dati utente
010400060801     c                   exsr      RepDatiUte
010500060801      *
010600060801      * Preparazione stringa SQL da eseguire
010700060802     c                   exsr      PrepareSQL
010800060801      *
010900060801     c                   ENDSR
011000060801      *
011100060801      *---------------------------------------------------------------*
011200060801      *?Reperimento Dati Utente                                      ?*
011300060801      *---------------------------------------------------------------*
011400060801     c     RepDatiUte    BEGSR
011500060801      *
011600060801     c     *dtaara       define    §azute        azuteds
011700060801     c     *dtaara       define    §datiute      ddatiute
011800060801     c                   in(e)     *dtaara
011900060801if  1c                   if        %error
012000060801     c                   eval      I34tla = 'L'
012100060801     c                   call      'TIBS34R'
012200060801     c                   parm                    TIBS34ds
012300060801     c                   in        *dtaara
012400060801e   1c                   endif
012500060801      *
012600060801     c                   ENDSR
012700060801      *
012800060801      *---------------------------------------------------------------*
012900060801      *?Preparazione stringa SQL da eseguire e Apertura cursore      ?*
013000060801      *---------------------------------------------------------------*
013100060802     c     PrepareSQL    BEGSR
013200060801      *
013300060803      * Elenco "tipi destinatario" da selezionare
013400060802     c                   clear                   wDOS
013500060802     c                   clear                   xx
013600060802do  1c                   DOW       xx   < %elem($PRMdos)
013700060802     c                   eval      xx   = xx + 1
013800060802if  2c                   if        $PRMdos(xx) = *blanks
013900060802     c                   iter
014000060802e   2c                   endif
014100060802if  2c                   if        wDOS = *blanks
014200060802     c                   eval      wDOS = '('''
014300060802x   2c                   else
014400060802     c                   eval      wDOS = %trim(wDOS) + ', '''
014500060802e   1c                   endif
014600060802     c                   eval      wDOS = %trim(wDOS)
014700060802     c                                  + $PRMdos(xx)
014800060802     c                                  + ''''
014900060802e   1c                   ENDDO
015000060802     c                   eval      wDOS = %trim(wDOS) + ')'
015100060802      *
015200060802      * Stringa SQL
015300060802     c                   eval      wSQL = 'select *'
015400060802     c                                  + ' from  AZDSTU0F'
015500060926     c                                  + ' where DSTtip = ''U'''
015600060922     c                                  +   ' and DSTdos in '
015700060922     c                                  +         %trim(wDOS)
015800060922     c                                  + ' order by DSTcod,'
015900060922     c                                  +          ' DSTnaz,'
016000060922     c                                  +          ' DSTprv,'
016100060922     c                                  +          ' DSTcap,'
016200060922     c                                  +          ' DSTloc,'
016300060922     c                                  +          ' DSTrag,'
016400060922     c                                  +          ' DSTkwi,'
016500060922     c                                  +          ' DSSsin desc'
016600060926     c                                  +   ' FOR UPDATE OF DSTflo'
016700060801      *
016800060801     c                   ENDSR
016900060801      *
017000060801      *---------------------------------------------------------------*
017100060802      *?Dichiarazione e Apertura cursore                             ?*
017200060801      *---------------------------------------------------------------*
017300060802     c     OpenCursor    BEGSR
017400060803      *
017500060803     c/exec sql
017600060803     c+     SET OPTION Commit    = *NONE,
017700060803     c+                CloSQLCsr = *ENDMOD,
017800060803     c+                DatFmt    = *ISO,
017900060803     c+                TimFmt    = *ISO
018000060803     c/end-exec
018100060801      *
018200060801      * Dichiarazione cursore
018300060801      *
018400060802     c/exec sql
018500060802     c+     PREPARE S1 FROM :wSQL
018600060802     c/end-exec
018700060802      *
018800060801     c/exec sql
018900060802     c+     DECLARE C1 CURSOR FOR S1
019000060801     c/end-exec
019100060801      *
019200060801      * Apertura del cursore
019300060801      *
019400060801     c/exec sql
019500060801     c+     OPEN C1
019600060801     c/end-exec
019700060801      *
019800060801     c                   ENDSR
019900060801      *
020000060801      *---------------------------------------------------------------*
020100060801      *?Lettura cursore                                              ?*
020200060801      *---------------------------------------------------------------*
020300060802     c     ReadCursor    BEGSR
020400060801      *
020500060802     c                   clear                   AZDSTUds
020600060801      *
020700060801     c/exec sql
020800060802     c+     FETCH NEXT FROM C1 INTO :AZDSTUds
020900060801     c/end-exec
021000060801      *
021100060802sel 1c                   SELECT
021200060802w   1c                   WHEN      SQLcod  =  100
021300060801     c                   eval      $EoF    =  *on
021400060802w   1c                   WHEN      SQLcod  <  *zeros
021500060801     c                   eval      $Err    =  *on
021600060801     c                   eval      $Eof    =  *on
021700060802x   1c                   OTHER
021800060802     c                   exsr      Update_DSTU
021900060802e   1c                   ENDSL
022000060801      *
022100060801     c                   ENDSR
022200060802      *
022300060802      *---------------------------------------------------------------*
022400060802      *?Chiusura cursore                                             ?*
022500060802      *---------------------------------------------------------------*
022600060802     c     CloseCursor   BEGSR
022700060802      *
022800060802     c/exec sql
022900060802     c+     CLOSE   C1
023000060802     c/end-exec
023100060802      *
023200060802     c                   ENDSR
023300060802      *
023400060802      *---------------------------------------------------------------*
023500060802      *?Annullamento records in esame negli archivi AZDST00F/AZDSS00F?*
023600060802      *---------------------------------------------------------------*
023700060802     c     Update_DSTU   BEGSR
023800060803      *
023900060803     c                   eval      $Upd     = *off
024000060922     c                   clear                   W_DSTflo
024100060802      *
024200060802      * Aggiornamento sommatoria spedizioni x destinatario
024300060919     c                   if        U_DSTcod   <> wSAVcod
024400060803     c                   eval      wSAVcod  = U_DSTcod
024500060913     c                   eval      $NoUpd1S = *off
024600060802     c                   endif
024700060802      *
024800060803      *?Annullamento record in AZDSS/AZDST se rientra nel limite?
024900060802      *
025000060802sel 1c                   SELECT
025100060919      * - Memo mancato Annullamento di un rec. in AZDSS00F (sinonimi)
025200060919w   1c                   WHEN           U_DSSsin <> *blanks
025300060919     c                             and  U_DSTnum >= PRMnum
025400060919     c                   eval      $NoUpd1S = *on
025500060802      * - Annullamento record in AZDSS00F (sinonimi)
025600060803w   1c                   WHEN           U_DSSsin <> *blanks
025700060803     c                             and  U_DSTnum <  PRMnum
025800060803     c                   exsr      Update_DSS
025900060802      * - Annullamento record in AZDST00F
026000060803w   1c                   WHEN           U_DSSsin =  *blanks
026100060913     c                             and  $NoUpd1S =  *off
026200060919     c                             and  U_DSTnum <  PRMnum
026300060803     c                   exsr      Update_DST
026400060802e   1c                   ENDSL
026500060802      *
026600060803      *?Annullamento record in AZDSTU0F se annullato in AZDST/AZDSS?
026700060802      *
026800060803if  1c                   if        $Upd     = *on
026900060803      *
027000060802     c/exec sql
027100060802     c+     UPDATE AZDSTU0F
027200060926     c+     set    DSTflo = :W_DSTflo
027300060802     c+     where  CURRENT  OF  C1
027400060802     c/end-exec
027500060802      *
027600060802e   1c                   endif
027700060802      *
027800060802     c                   ENDSR
027900060802      *
028000060802      *---------------------------------------------------------------*
028100060802      *?Preparazione SQL per annullamento record in AZDSS00F         ?*
028200060802      *---------------------------------------------------------------*
028300060803     c     Update_DSS    BEGSR
028400060802      *
028500060803     c/exec sql
028600060803     c+     UPDATE AZDSS00F
028700060922     c+     set    DSSftr = 'D'
028800060926     c+     where  DSSnaz = :U_DSTnaz
028900060803     c+       and  DSSprv = :U_DSTprv
029000060803     c+       and  DSScap = :U_DSTcap
029100060803     c+       and  DSSloc = :U_DSTloc
029200060803     c+       and  DSSrag = :U_DSTrag
029300060803     c+       and  DSSkwi = :U_DSTkwi
029400060803     c+       and  DSSsin = :U_DSSsin
029500060913     c+       and  DSSdiv < :PRMdiv
029600060803     c/end-exec
029700060913      *
029800060926      * Memorizzo l'eventuale MANCATO aggiornamento di almeno un
029900060926      *   sinonimo (SQLcod = 100 -> nessun record modificato) per
030000060926      *   NON annullare il potenziale a cui si riferisce
030100060913      *   (motivo supposto: DSSdiv >= :PRMdiv   -
030200060913      *    non dovrebbero essercene altri)
030300060926if  1c                   if        SQLcod   = 100
030400060913     c                   eval      $NoUpd1S = *on
030500060913e   1c                   endif
030600060802      *
030700060802     c                   ENDSR
030800060802      *
030900060802      *---------------------------------------------------------------*
031000060802      *?Preparazione SQL per annullamento record in AZDST00F         ?*
031100060802      *---------------------------------------------------------------*
031200060803     c     Update_DST    BEGSR
031300060802      *
031400060922      *?PRIMA?verifica l'esistenza di NUOVI record?non?"annullati"
031500060922      *   (DSSFTR <> 'D') in AZDSS00F
031600060922     c                   clear                   wCountRec
031700060922      *
031800060922     c/exec sql
031900060922     c+     DECLARE C2 CURSOR FOR
032000060922     c+     select  count(*)
032100060922     c+     from    AZDSS00F
032200060922     c+     where   DSSatb =  ' '
032300060922     c+       and   DSSftr <> 'D'
032400060922     c+       and   DSSnaz =  :U_DSTnaz
032500060922     c+       and   DSSprv =  :U_DSTprv
032600060922     c+       and   DSScap =  :U_DSTcap
032700060922     c+       and   DSSloc =  :U_DSTloc
032800060922     c+       and   DSSrag =  :U_DSTrag
032900060922     c+       and   DSSkwi =  :U_DSTkwi
033000060922     ***c+       and   DSSsin =  :U_DSSsin
033100060922     ***c+       and   DSSdiv <  :PRMdiv
033200060922     c+     for     READ   only
033300060922     c/end-exec
033400060922      *
033500060922     c/exec sql
033600060922     c+     open    C2
033700060922     c/end-exec
033800060922      *
033900060922     c/exec sql
034000060922     c+     FETCH   C2   into :wCountRec
034100060922     c/end-exec
034200060922      *
034300060922     c/exec sql
034400060922     c+     close   C2
034500060922     c/end-exec
034600060922      *
034700060922      * - Se reperito almeno un rec. con ATB=*blanks & FTR<>"D"
034800060926      *   NON aggiorna AZDST00F (DSTFTR="D")     &
034900060926      *   aggiorna     AZDSTU0F (U_DSTFLO="X    ")
035000060922if  1c                   if        wCountRec > *zeros
035100060922     c                   eval      $Upd      = *on
035200060922     c                   movel(p)  'X'           W_DSTflo
035300060922     c                   leavesr
035400060922e   1c                   endif
035500060922      *
035600060803     c/exec sql
035700060803     c+     UPDATE AZDST00F
035800060922     c+     set    DSTftr = 'D'
035900060926     c+     where  DSTtip = 'C'
036000060803     c+       and  DSTcod = :U_DSTcod
036100060803     c+       and  DSTnaz = :U_DSTnaz
036200060803     c+       and  DSTprv = :U_DSTprv
036300060803     c+       and  DSTcap = :U_DSTcap
036400060803     c+       and  DSTloc = :U_DSTloc
036500060803     c+       and  DSTrag = :U_DSTrag
036600060803     c+       and  DSTkwi = :U_DSTkwi
036700060803     c+       and  DSTdos = :U_DSTdos
036800060913     c+       and  DSTdiv < :PRMdiv
036900060803     c/end-exec
037000070904      *
037100070904     c/exec sql
037200070904     c+     UPDATE AZDSE00F
037300070904     c+     set    DSEatb = 'A'
037400070904     c+     where  DSEcod = :U_DSTcod
037500070904     c/end-exec
037600060802      *
037700060802     c                   ENDSR
