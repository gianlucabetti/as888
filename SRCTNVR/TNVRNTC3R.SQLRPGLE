000100051121      *PARMS DBGVIEW(*SOURCE)
000200051121      *===============================================================*
000300051202      *? Sistemazione note "tariffa" senza nota "anagrafica"         ?*
000400051121      *===============================================================*
000500051121     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600051121     h alwnull(*inputonly)
000700051121      *
000800051121      *?A R C H I V I?------------------------------------------------*
000900051121      *
001000051125     fTFNTC01L  Uf   e           k disk
001100051125      *
001200051125      *?C O S T A N T I?----------------------------------------------*
001300051125      *
001400051121      *
001500051121      *?S C H I E R E?------------------------------------------------*
001600051121      *
001700051125     d $SqlCmd3        s             60    dim(12) ctdata perrcd(1)
001800051121      *
001900051121      *?S T R U T T U R E   D A T I?----------------------------------*
002000051121      *
002100051121      * - Parametri x Controllo profilo utenti
002200051121     d TIBS34ds      e ds                  inz
002300051121      * - Ds di riferimento al file esterno AZUTE00F
002400051121     d AZUTEds       e ds                  inz extname(AZUTE00F)
002500051121      * - Ds per dati organigramma
002600051121     d DDatiUte      e ds                  inz
002700051118      *
002800051121      * Dati estratti da TFNTC00F
002900051202     d TFNTCds       e ds                  inz extname(TFNTC00F)
003000051121      *
003100051121     d Status         sds
003200051121     d  SDSpgm           *proc
003300051121     d  SDSusr               254    263
003400051202      *
003500051202     d ds_KEY          ds                  inz
003600051202     d  A_NTCapl                           like(NTCapl) inz
003700051202     d  A_NTCnk1                           like(NTCnk1) inz
003800051202     d  A_NTCtnt                           like(NTCtnt) inz
003900051202      *
004000051202     d SavKEY          s                   like(ds_KEY) inz
004100051121      *
004200051121      *?V A R I A B I L I?--------------------------------------------*
004300051121      *
004400051121     d WrkSqlCmd       s           1024    inz
004500051121     d Data_Eur        s               d   inz  datfmt(*eur)
004600051128     d W_NTCntr        s              8  0 inz
004700051128     d XX              s              3  0 inz
004800051125      *
004900051125      *?K E Y - L I S T?----------------------------------------------*
005000051125      *
005100051125     c     K04NTC01      klist
005200051202     c                   kfld                    NTCapl
005300051202     c                   kfld                    NTCnk1
005400051202     c                   kfld                    NTCnk2
005500051202     c                   kfld                    NTCtnt
005600051121      *
005700051121      *?I N D I C A T O R I?------------------------------------------*
005800051121      *                                                               *
005900051121      * 01 - overflow                                                 *
006000051121      * 04 - richiesta cancellazione records                          *
006100051121      * 06 - richiesta lista         records                          *
006200051121      *                                                               *
006300051121      *===============================================================*
006400051121
006500051121      * Operazioni iniziali
006600051121     c                   exsr      OperazIniz
006700051125      *
006800051125      * Gestione SQL
006900051125     c                   exsr      Exec_SQL
007000051121      *
007100051121      * Operazioni finali
007200051121     c                   eval      *inLR = *on
007300051121     c                   return
007400051125
007500051121      *
007600051121      *?S U B R O U T I N E S?----------------------------------------*
007700051121      *
007800051121
007900051121      *---------------------------------------------------------------*
008000051121      *?Operazioni iniziali                                          ?*
008100051121      *---------------------------------------------------------------*
008200051121     c     OperazIniz    BEGSR
008300051121      *
008400051121      * Reperisce dati job
008500051121     c     *dtaara       define    §azute        azuteds
008600051121     c     *dtaara       define    §datiute      ddatiute
008700051121      *
008800051121     c                   in(E)     *dtaara
008900051121     c                   IF        %ERROR or RSUT = *blanks
009000051121     c                   clear                   Tibs34Ds
009100051121     c                   call      'TIBS34R'
009200051121     c                   parm                    Tibs34Ds
009300051121     c                   in        *dtaara
009400051121     c                   ENDIF
009500051121      *
009600051121     c                   ENDSR
009700051125
009800051125      *---------------------------------------------------------------*
009900051125      *?Preparazione, esecuzione e chiusura SQL                      ?*
010000051125      *---------------------------------------------------------------*
010100051125     c     Exec_SQL      BegSr
010200051125      *
010300051125      * Preparazione comando SQL da eseguire
010400051125     c                   exsr      Prep_SQL
010500051125      *
010600051125     c/exec sql
010700051125     c+ prepare S1 from :WrkSqlCmd
010800051125     c/end-exec
010900051125      *
011000051125     c/exec sql
011100051125     c+ declare C1 cursor for S1
011200051125     c/end-exec
011300051125      *
011400051125     c/exec sql
011500051125     c+ open  C1
011600051125     c/end-exec
011700051121      *
011800051125do  1c                   DOU          SQLcod < *zeros
011900051121     c                             or SQLcod = 100
012000051202     c                   clear                   TFNTCds
012100051121      *
012200051121     c/exec sql
012300051202     c+ fetch C1 into :TFNTCds
012400051121     c/end-exec
012500051121      *
012600051121sel 2c                   select
012700051121w   2c                   when      SQLcod    < *zeros
012800051121     c                   exsr      SQLerror
012900051121     c                   leave
013000051121w   2c                   when      SQLcod    = 100
013100051121     c                   leave
013200051121x   2c                   other
013300051125     c                   exsr      sr_ElabRec
013400051121e   2c                   endsl
013500051121      *
013600051125e   1c                   ENDDO
013700051121      *
013800051121     c/exec sql
013900051121     c+ close C1
014000051121     c/end-exec
014100051121      *
014200051121     c                   EndSr
014300051125
014400051125      *---------------------------------------------------------------*
014500051125      *?Preparazione comando SQL da eseguire                         ?*
014600051125      *---------------------------------------------------------------*
014700051125     c     Prep_SQL      BegSr
014800051125      *
014900051202      * Elaborazione   Note "tariffa"   senza    nota "anagrafica"
015000051128     c                   clear                   xx
015100051128     c                   dow       xx        < %elem($SqlCmd3)
015200051128     c                   add       1             xx
015300051128     c                   eval      WrkSqlCmd = %trim(WrkSqlCmd) + ' '
015400051128     c                                       + $SqlCmd3(xx)
015500051128     c                   enddo
015600051125      *
015700051125     c                   EndSr
015800051125
015900051125      *---------------------------------------------------------------*
016000051125      *?Elaborazione singolo record estratto da TFNTC00F             ?*
016100051125      *---------------------------------------------------------------*
016200051125     c     sr_ElabRec    BegSr
016300051125      *
016400051125      *
016500051125     c     K04NTC01      chain     TFNTC
016600051202if  1c                   if        NOT %found(TFNTC01L)
016700051202     c                   leavesr
016800051202e   1c                   endif
016900051125      *
017000051202      *?RIDENOMINAZIONE?
017100051202     c                   eval      A_NTCapl = NTCapl
017200051202     c                   eval      A_NTCnk1 = NTCnk1
017300051202     c                   eval      A_NTCtnt = NTCtnt
017400051202if  1c                   if        ds_KEY <> SavKEY
017500051202     c                   movel     ds_KEY        SavKEY
017600051202     c                   clear                   NTCnk2
017700051202     c                   UPDATE    TFNTC
017800051202     c                   leavesr
017900051202e   1c                   endif
018000051202      *
018100051202      *?CANCELLAZIONE?
018200051202if  1c                   if        ds_KEY =  SavKEY
018300051125     c                   DELETE    TFNTC
018400051125e   1c                   endif
018500051125      *
018600051125     c                   EndSr
018700051118
018800051121      *---------------------------------------------------------------*
018900051121      *?Gestione errore SQL                                          ?*
019000051121      *---------------------------------------------------------------*
019100051121     c     SQLerror      BegSr
019200051121      *
019300051118      * Forzo la stampa del JOBLOG e chiude il pgm forzatamente
019400051118     c                   call      'X66CHGJOB'
019500051118     c                   eval      *inLR = *ON
019600051118     c                   return
019700051121      *
019800051118     c                   EndSr
019900051118
020000051202** - $SqlCmd3 +....2....+....3....+....4....+....5....+....6
020100051125select a.*                                                       1
020200051125from   TFNTC01L a   EXCEPTION JOIN   TFNTC01L b                  2
020300051125on     a.NTCapl =  b.NTCapl                                      3
020400051125  and  a.NTCnk1 =  b.NTCnk1                                      4
020500051125  and  a.NTCtnt =  b.NTCtnt                                      5
020600051125  and  a.NTCnk2 <> '    '                                        6
020700051125  and  b.NTCnk2 =  '    '                                        7
020800051125where  a.NTCapl in ('C', 'V')                                    8
020900051125  and  a.NTCtnt in ('02', '05', '07', '30')                      9
021000051125  and  a.NTCnk2 <> '    '                                       11
021100051125order  by  a.NTCapl, a.NTCnk1, a.NTCnk2, a.NTCtnt, a.NTCrnt     12
