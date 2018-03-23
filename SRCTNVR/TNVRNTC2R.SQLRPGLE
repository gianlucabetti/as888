000100051121      *PARMS DBGVIEW(*SOURCE)
000200051121      *===============================================================*
000300051121      *? Cancellazione note clienti "doppie"                         ?*
000400051121      *===============================================================*
000500051121     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600051121     h alwnull(*inputonly)
000700051121      *
000800051121      *?A R C H I V I?------------------------------------------------*
000900051121      *
001000051125     fTFNTC01L  Uf   e           k disk
001100051125      *
001200051121     fQPRINT    o    f  132        printer usropn
001300051121     f                                     oflind(*in01)
001400051125      *
001500051125      *?C O S T A N T I?----------------------------------------------*
001600051125      *
001700051121      *
001800051121      *?S C H I E R E?------------------------------------------------*
001900051121      *
002000051125     d $SqlCmd1        s             60    dim(11) ctdata perrcd(1)
002100051125     d $SqlCmd2        s             60    dim(11) ctdata perrcd(1)
002200051125     d $SqlCmd3        s             60    dim(12) ctdata perrcd(1)
002300051125     d $SqlCmd4        s             60    dim( 5) ctdata perrcd(1)
002400051125     d $Cmd            s                   like(Qcmd)
002500051125     d                                     dim( 3) ctdata perrcd(1)
002600051128     d $Txt            s             66    dim(12) ctdata perrcd(1)
002700051121      *
002800051121      *?S T R U T T U R E   D A T I?----------------------------------*
002900051121      *
003000051118     d KPJBA         e ds
003100051121     d TNVRNTC2ds      ds                  inz
003200051121     d   PRMprt                       1
003300051121     d   PRMdlt                       1
003400051121      *
003500051121      * - Parametri x Controllo profilo utenti
003600051121     d TIBS34ds      e ds                  inz
003700051121      * - Ds di riferimento al file esterno AZUTE00F
003800051121     d AZUTEds       e ds                  inz extname(AZUTE00F)
003900051121      * - Ds per dati organigramma
004000051121     d DDatiUte      e ds                  inz
004100051118      *
004200051121      * Dati estratti da TFNTC00F
004300051125     d TFNTCds_A     e ds                  inz extname(TFNTC00F)
004400051125     d                                         prefix(A_)
004500051125     d TFNTCds_B     e ds                  inz extname(TFNTC00F)
004600051125     d                                         prefix(B_)
004700051128      *
004800051128     d WLBdat          ds                  inz
004900051128     d  G02dat                 1      8  0 inz
005000051128     d  G02inv                 9     16  0 inz
005100051128     d  G02err                17     17    inz('3')
005200051128     d  G02tgi                18     22  0 inz
005300051121      *
005400051121     d Status         sds
005500051121     d  SDSpgm           *proc
005600051121     d  SDSusr               254    263
005700051121      *
005800051121      *?V A R I A B I L I?--------------------------------------------*
005900051121      *
006000051121     d WrkSqlCmd       s           1024    inz
006100051125     d wDESprt         s             45    inz
006200051121     d Data_Eur        s               d   inz  datfmt(*eur)
006300051128     d W_NTCntr        s              8  0 inz
006400051121     d wDate           s              8  0 inz
006500051121     d wTime           s              6  0 inz
006600051128     d XX              s              3  0 inz
006700051125     d SAVapl          s                   like(NTCapl) inz(*hival)
006800051125     d SAVnk1          s                   like(NTCnk1) inz(*hival)
006900051125     d SAVtnt          s                   like(NTCtnt) inz(*hival)
007000051121      * - Parametri per QCMDEXC
007100051121     d Qcmd            s             80    inz
007200051121     d Qlen            s             15  5 inz(80)
007300051125      *
007400051125      *?K E Y - L I S T?----------------------------------------------*
007500051125      *
007600051125     c     K04NTC01      klist
007700051125     c                   kfld                    A_NTCapl
007800051125     c                   kfld                    A_NTCnk1
007900051125     c                   kfld                    A_NTCnk2
008000051125     c                   kfld                    A_NTCtnt
008100051121      *
008200051121      *?I N D I C A T O R I?------------------------------------------*
008300051121      *                                                               *
008400051121      * 01 - overflow                                                 *
008500051121      * 04 - richiesta cancellazione records                          *
008600051121      * 06 - richiesta lista         records                          *
008700051121      *                                                               *
008800051121      *===============================================================*
008900051121
009000051118     c     *entry        plist
009100051118     c                   parm                    KPJBA
009200051121     c                   movel     KPJBU         TNVRNTC2ds
009300051121      *
009400051121      * Operazioni iniziali
009500051121     c                   exsr      OperazIniz
009600051125      *
009700051125      * Gestione SQL
009800051125     c                   exsr      Exec_SQL
009900051121      *
010000051121      * Operazioni finali
010100051121     c                   exsr      OperazFin
010200051121     c                   eval      *inLR = *on
010300051121     c                   return
010400051125
010500051121      *
010600051121      *?S U B R O U T I N E S?----------------------------------------*
010700051121      *
010800051121
010900051121      *---------------------------------------------------------------*
011000051121      *?Operazioni iniziali                                          ?*
011100051121      *---------------------------------------------------------------*
011200051121     c     OperazIniz    BEGSR
011300051121      *
011400051121      * Reperisce dati job
011500051121     c     *dtaara       define    §azute        azuteds
011600051121     c     *dtaara       define    §datiute      ddatiute
011700051121      *
011800051121     c                   in(E)     *dtaara
011900051121     c                   IF        %ERROR or RSUT = *blanks
012000051121     c                   clear                   Tibs34Ds
012100051121     c                   call      'TIBS34R'
012200051121     c                   parm                    Tibs34Ds
012300051121     c                   in        *dtaara
012400051121     c                   ENDIF
012500051121      *
012600051121if  1c                   if        PRMprt = 'N'
012700051121     c                   leavesr
012800051121e   1c                   endif
012900051121      *
013000051121      * Override al prtf
013100051121     c                   call      'QCMDEXC'
013200051121     c                   parm      $Cmd(1)       Qcmd
013300051121     c                   parm                    Qlen
013400051121      *
013500051121      * Apertura prtf
013600051121     c                   open      Qprint
013700051121      *
013800051121      * Stampa della testata iniziale
013900051121     c                   move      *date         Data_Eur
014000051121     c     *eur          movel     Data_Eur      wDate
014100051121     c                   time                    wTime
014200051121      *
014300051121     c                   movel     KPJBU         TNVRNTC2ds
014400051121     c                   eval      *in04 = (PRMdlt = 'S')
014500051128     c                   eval      *in06 = (PRMprt > '0')
014600051125      *
014700051125     c                   select
014800051125     c                   when      PRMprt  = '1'
014900051128     c                   movel     $Txt(1)       wDESprt
015000051125     c                   when      PRMprt  = '2'
015100051128     c                   movel     $Txt(2)       wDESprt
015200051125     c                   when      PRMprt  = '3'
015300051128     c                   movel     $Txt(3)       wDESprt
015400051125     c                   when      PRMprt  = '4'
015500051128     c                   movel     $Txt(4)       wDESprt
015600051125     c                   endsl
015700051121      *
015800051121     c                   except    PRTtxt
015900051121     c                   eval      *in01 = *off
016000051121      *
016100051121     c                   ENDSR
016200051125
016300051125      *---------------------------------------------------------------*
016400051125      *?Preparazione, esecuzione e chiusura SQL                      ?*
016500051125      *---------------------------------------------------------------*
016600051125     c     Exec_SQL      BegSr
016700051125      *
016800051125      * Preparazione comando SQL da eseguire
016900051125     c                   exsr      Prep_SQL
017000051125      *
017100051125     c/exec sql
017200051125     c+ prepare S1 from :WrkSqlCmd
017300051125     c/end-exec
017400051125      *
017500051125     c/exec sql
017600051125     c+ declare C1 cursor for S1
017700051125     c/end-exec
017800051125      *
017900051125     c/exec sql
018000051125     c+ open  C1
018100051125     c/end-exec
018200051121      *
018300051125do  1c                   DOU          SQLcod < *zeros
018400051121     c                             or SQLcod = 100
018500051125     c                   clear                   TFNTCds_A
018600051125     c                   clear                   TFNTCds_B
018700051121      *
018800051121     c/exec sql
018900051125     c+ fetch C1 into :TFNTCds_A, :TFNTCds_B
019000051121     c/end-exec
019100051121      *
019200051121sel 2c                   select
019300051121w   2c                   when      SQLcod    < *zeros
019400051121     c                   exsr      SQLerror
019500051121     c                   leave
019600051121w   2c                   when      SQLcod    = 100
019700051121     c                   leave
019800051121x   2c                   other
019900051125     c                   exsr      sr_ElabRec
020000051121e   2c                   endsl
020100051121      *
020200051125e   1c                   ENDDO
020300051121      *
020400051121     c/exec sql
020500051121     c+ close C1
020600051121     c/end-exec
020700051121      *
020800051121     c                   EndSr
020900051125
021000051125      *---------------------------------------------------------------*
021100051125      *?Preparazione comando SQL da eseguire                         ?*
021200051125      *---------------------------------------------------------------*
021300051125     c     Prep_SQL      BegSr
021400051125      *
021500051125     c                   SELECT
021600051125      * Stampa "1" = Note "tariffa" uguali  a  nota "anagrafica"
021700051125     c                   WHEN         PRMprt = '0'
021800051125     c                             or PRMprt = '1'
021900051128     c                   clear                   xx
022000051128     c                   dow       xx        < %elem($SqlCmd1)
022100051128     c                   add       1             xx
022200051128     c                   eval      WrkSqlCmd = %trim(WrkSqlCmd) + ' '
022300051128     c                                       + $SqlCmd1(xx)
022400051128     c                   enddo
022500051125      * Stampa "2" = Note "tariffa" diverse da nota "anagrafica"
022600051125     c                   WHEN      PRMprt = '2'
022700051128     c                   clear                   xx
022800051128     c                   dow       xx        < %elem($SqlCmd2)
022900051128     c                   add       1             xx
023000051128     c                   eval      WrkSqlCmd = %trim(WrkSqlCmd) + ' '
023100051128     c                                       + $SqlCmd2(xx)
023200051128     c                   enddo
023300051125      * Stampa "3" = Note "tariffa"   senza    nota "anagrafica"
023400051125     c                   WHEN      PRMprt = '3'
023500051128     c                   clear                   xx
023600051128     c                   dow       xx        < %elem($SqlCmd3)
023700051128     c                   add       1             xx
023800051128     c                   eval      WrkSqlCmd = %trim(WrkSqlCmd) + ' '
023900051128     c                                       + $SqlCmd3(xx)
024000051128     c                   enddo
024100051125      * Stampa "4" = Tutto l'archivio
024200051125     c                   WHEN      PRMprt = '4'
024300051128     c                   clear                   xx
024400051128     c                   dow       xx        < %elem($SqlCmd4)
024500051128     c                   add       1             xx
024600051128     c                   eval      WrkSqlCmd = %trim(WrkSqlCmd) + ' '
024700051128     c                                       + $SqlCmd4(xx)
024800051128     c                   enddo
024900051125     c                   ENDSL
025000051125      *
025100051125     c                   EndSr
025200051125
025300051125      *---------------------------------------------------------------*
025400051125      *?Elaborazione singolo record estratto da TFNTC00F             ?*
025500051125      *---------------------------------------------------------------*
025600051125     c     sr_ElabRec    BegSr
025700051125      *
025800051125      * Impostazione campi reperiti
025900051125if  1c                   if            PRMprt = '3'
026000051125     c                             or  PRMprt = '4'
026100051125     c                   clear                   TFNTCds_B
026200051125e   1c                   endif
026300051125      *
026400051125      *?STAMPA?
026500051125if  1c                   IF        PRMprt > '0'
026600051125      * Stampa nota "anagrafica"
026700051128if  2c                   if             PRMprt   <> '3'
026800051128     c                             and  PRMprt   <> '4'
026900051128     c                             and (B_NTCapl <> SAVapl
027000051128     c                              or  B_NTCnk1 <> SAVnk1
027100051128     c                              or  B_NTCtnt <> SAVtnt)
027200051125     c                   eval      SAVapl = B_NTCapl
027300051125     c                   eval      SAVnk1 = B_NTCnk1
027400051125     c                   eval      SAVtnt = B_NTCtnt
027500051125     c                   exsr      sr_StampaA
027600051125e   2c                   endif
027700051125      * Stampa nota "tariffa"
027800051125     c                   exsr      sr_StampaT
027900051125e   1c                   ENDIF
028000051125      *
028100051125      *?CANCELLAZIONE?
028200051125if  1c                   IF        PRMdlt = 'S'
028300051125     c                   exsr      sr_Delete
028400051125e   1c                   ENDIF
028500051125      *
028600051125     c                   EndSr
028700051118
028800051121      *---------------------------------------------------------------*
028900051125      *?Stampa dettaglio "anagrafica"                                ?*
029000051121      *---------------------------------------------------------------*
029100051125     c     sr_StampaA    BegSr
029200051128      *
029300051128     c                   reset                   WLBdat
029400051128     c                   z-add     A_NTCntr      G02inv
029500051128     c                   if        (G02inv / 10000) < 60
029600051128     c                   add       20000000      G02inv
029700051128     c                   else
029800051128     c                   add       19000000      G02inv
029900051128     c                   endif
030000051128     c                   call      'XSRDA8'
030100051128     c                   parm                    WLBdat
030200051128     c                   z-add     G02dat        W_NTCntr
030300051121      *
030400051121if  1c                   if        *in01 = *on
030500051118     c                   except    PRTtxt
030600051121     c                   eval      *in01 = *off
030700051121e   1c                   endif
030800051125     c                   except    PRTdetA
030900051121      *
031000051118     c                   EndSr
031100051125
031200051125      *---------------------------------------------------------------*
031300051125      *?Stampa dettaglio "tariffa"                                   ?*
031400051125      *---------------------------------------------------------------*
031500051125     c     sr_StampaT    BegSr
031600051128      *
031700051128     c                   reset                   WLBdat
031800051128     c                   z-add     A_NTCntr      G02inv
031900051128     c                   if        (G02inv / 10000) < 60
032000051128     c                   add       20000000      G02inv
032100051128     c                   else
032200051128     c                   add       19000000      G02inv
032300051128     c                   endif
032400051128     c                   call      'XSRDA8'
032500051128     c                   parm                    WLBdat
032600051128     c                   z-add     G02dat        W_NTCntr
032700051125      *
032800051125if  1c                   if        *in01 = *on
032900051125     c                   except    PRTtxt
033000051125     c                   eval      *in01 = *off
033100051125e   1c                   endif
033200051125     c                   except    PRTdetT
033300051125      *
033400051125     c                   EndSr
033500051125
033600051125      *---------------------------------------------------------------*
033700051125      *?Cancellazione singola nota "tariffa"                         ?*
033800051125      *---------------------------------------------------------------*
033900051125     c     sr_Delete     BegSr
034000051125      *
034100051125     c     K04NTC01      chain     TFNTC
034200051125      *
034300051125if  1c                   if        %found(TFNTC01L)
034400051125     c                   DELETE    TFNTC
034500051125e   1c                   endif
034600051125      *
034700051125     c                   EndSr
034800051118
034900051121      *---------------------------------------------------------------*
035000051121      *?Gestione errore SQL                                          ?*
035100051121      *---------------------------------------------------------------*
035200051121     c     SQLerror      BegSr
035300051121      *
035400051125     c                   except    PRTerr
035500051118      * Forzo la stampa del JOBLOG e chiude il pgm forzatamente
035600051118     c                   call      'X66CHGJOB'
035700051118     c                   eval      *inLR = *ON
035800051118     c                   return
035900051121      *
036000051118     c                   EndSr
036100051121
036200051121      *---------------------------------------------------------------*
036300051121      *?Operazioni finali                                            ?*
036400051121      *---------------------------------------------------------------*
036500051121     c     OperazFin     BEGSR
036600051121      *
036700051121if  1c                   if        PRMprt =  'N'
036800051121     c                   leavesr
036900051121e   1c                   endif
037000051121      *
037100051121      * Stampa "Fine Lista"
037200051121     c                   except    PRTend
037300051121      *
037400051121      * Chiusura prtf
037500051121     c                   close     Qprint
037600051121      *
037700051121      * Cancellazione override
037800051121     c                   call      'QCMDEXC'
037900051121     c                   parm      $Cmd(2)       Qcmd
038000051121     c                   parm                    Qlen
038100051121      *
038200051121     c                   ENDSR
038300051121
038400051121      *
038500051121      *?S T A M P A?--------------------------------------------------*
038600051121      *
038700051121     oQprint    e            PRTtxt           02
038800051121     o                       RSUT
038900051121     o               04                          45 'CANCELLAZIONE'
039000051121     o              N04 06                       45 'LISTA'
039100051121     o                                        +   1 'NOTE CLIENTI "DOPPIE"'
039200051121     o                       SDSpgm           +  20
039300051121     o                       *date         Y  +   5
039400051121     o                                        +   5 'Pag.'
039500051121     o                       Page          Z  +   0
039600051121     o          e            PRTtxt      0
039700051121     o               04                          45 'CANCELLAZIONE'
039800051121     o              N04 06                       45 'LISTA'
039900051121     o                                        +   1 'NOTE CLIENTI "DOPPIE"'
040000051121     o          e            PRTtxt      1
040100051121     o                       KNSIF
040200051121     o                       SDSusr           +   1
040300051121     o               04                          45 '-------------'
040400051121     o              N04 06                       45 '-----'
040500051121     o                                        +   0 '----------------------'
040600051121     o                       Wtime            +  35 '  :  :  '
040700051121     o          e            PRTtxt      0
040800051121     o               04                          45 '-------------'
040900051121     o              N04 06                       45 '-----'
041000051121     o                                        +   0 '----------------------'
041100051125      *
041200051125     o          e            PRTtxt      1
041300051128     o                       wDESprt             75
041400051121      *
041500051121     o          e            PRTtxt      3
041600051128     o                       $Txt(5)
041700051128     o                       $Txt(9)          +   0
041800051121     o          e            PRTtxt      1
041900051128     o                       $Txt(6)
042000051128     o                       $Txt(10)         +   0
042100051121     o          e            PRTtxt      1
042200051128     o                       $Txt(7)
042300051128     o                       $Txt(11)         +   0
042400051121      *
042500051125     o          e            PRTdetA     1
042600051125     o                       B_NTCapl         +   1
042700051125     o                       B_NTCnk1         +   4
042800051125     o                       B_NTCnk2         +   2
042900051125     o                       B_NTCtnt         +   3
043000051125     o                       B_NTCrnt         +   3
043100051128     o                       W_NTCntr      Y  +   2
043200051125      *
043300051125     o          e            PRTdetT     1
043400051125     o                       A_NTCapl         +   1
043500051125     o                       A_NTCnk1         +   4
043600051125     o                       A_NTCnk2         +   2
043700051125     o                       A_NTCtnt         +   3
043800051125     o                       A_NTCrnt         +   3
043900051128     o                       W_NTCntr      Y  +   2
044000051125      *
044100051125     o          e            PRTerr      3
044200051128     o                                        +  40 '»»»  ERRORE  «««'
044300051121      *
044400051121     o          e            PRTend      3
044500051128     o                                        +  40 '***  FINE  LISTA -
044600051128     o                                               ***'
044700051121     o          e            PRTend      0
044800051128     o                                        +  40 '***  FINE  LISTA -
044900051128     o                                               ***'
045000051118
045100051125** - $SqlCmd1 +....2....+....3....+....4....+....5....+....6
045200051125select *                                                         1
045300051125from   TFNTC01L a   INNER JOIN   TFNTC01L b                      2
045400051125on     a.NTCapl =  b.NTCapl                                      3
045500051125  and  a.NTCnk1 =  b.NTCnk1                                      4
045600051125  and  a.NTCtnt =  b.NTCtnt                                      5
045700051125  and  a.NTCrnt =  b.NTCrnt                                      6
045800051125  and  a.NTCnk2 <> '    '                                        7
045900051125  and  b.NTCnk2 =  '    '                                        8
046000051125where  a.NTCapl in ('C', 'V')                                    9
046100051125  and  a.NTCtnt in ('02', '05', '07', '30')                     10
046200051125order  by  a.NTCapl, a.NTCnk1, a.NTCnk2, a.NTCtnt, a.NTCrnt     11
046300051125** - $SqlCmd2 +....2....+....3....+....4....+....5....+....6
046400051125select *                                                         1
046500051125from   TFNTC01L a   INNER JOIN   TFNTC01L b                      2
046600051125on     a.NTCapl =  b.NTCapl                                      3
046700051125  and  a.NTCnk1 =  b.NTCnk1                                      4
046800051125  and  a.NTCtnt =  b.NTCtnt                                      5
046900051125  and  a.NTCrnt <> b.NTCrnt                                      6
047000051125  and  a.NTCnk2 <> '    '                                        7
047100051125  and  b.NTCnk2 =  '    '                                        8
047200051125where  a.NTCapl in ('C', 'V')                                    9
047300051125  and  a.NTCtnt in ('02', '05', '07', '30')                     10
047400051125order  by  a.NTCapl, a.NTCnk1, a.NTCnk2, a.NTCtnt, a.NTCrnt     11
047500051125** - $SqlCmd3 +....2....+....3....+....4....+....5....+....6
047600051125select a.*                                                       1
047700051125from   TFNTC01L a   EXCEPTION JOIN   TFNTC01L b                  2
047800051125on     a.NTCapl =  b.NTCapl                                      3
047900051125  and  a.NTCnk1 =  b.NTCnk1                                      4
048000051125  and  a.NTCtnt =  b.NTCtnt                                      5
048100051125  and  a.NTCnk2 <> '    '                                        6
048200051125  and  b.NTCnk2 =  '    '                                        7
048300051125where  a.NTCapl in ('C', 'V')                                    8
048400051125  and  a.NTCtnt in ('02', '05', '07', '30')                      9
048500051125  and  a.NTCnk2 <> '    '                                       11
048600051125order  by  a.NTCapl, a.NTCnk1, a.NTCnk2, a.NTCtnt, a.NTCrnt     12
048700051125** - $SqlCmd4 +....2....+....3....+....4....+....5....+....6
048800051125select *                                                         1
048900051125from   TFNTC01L a                                                2
049000051125where  a.NTCapl in ('C', 'V')                                    3
049100051125  and  a.NTCtnt in ('02', '05', '07', '30')                      4
049200051125order  by  a.NTCapl, a.NTCnk1, a.NTCnk2, a.NTCtnt, a.NTCrnt      5
049300051125** - $Cmd -------------------------------------------------------*
049400051125OVRPRTF FILE(QPRINT)  USRDTA('NoteDoppie')
049500051125DLTOVR  FILE(QPRINT)
049600051121** - $Txt -------------------------------------------------------*
049700051128  NOTE "TARIFFA" UGUALI A NOTA "ANAGRAFICA"
049800051128 NOTE "TARIFFA" DIVERSE DA NOTA "ANAGRAFICA"
049900051128   NOTE "TARIFFA" SENZA NOTA "ANAGRAFICA"
050000051128                TUTTE LE NOTE
050100051128COD.  KEY1         KEY2  TIPO  RIGA
050200051121APPL  NOTE         NOTE  NOTA  NOTA
050300051121----  -----------  ----  ----  -----------------------------------
050400051121 1    *...+....1.  1234   12   >...+....1....+....2....+....3....+
050500051128                              DATA
050600051128                           IMMISSIONE
050700051128-------------------------  ----------
050800051128....4....+....5....+....6  12/34/5678
