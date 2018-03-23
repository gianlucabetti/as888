000100050315      *---------------------------------------------------------------*
000200050315      *? Rilascio lavori in coda (precedentemente congelati)         ?*
000300050315      *---------------------------------------------------------------*
000400050315      *
000500050315     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600050315      *
000700050315      *?A R C H I V I?------------------------------------------------*
000800050315      *
000900050401     fTNSY12D   cf   e             workstn
001000050401     fWFHJQ00F  if   e             disk
001100050315      *
001200050315      *?C O S T A N T I?----------------------------------------------*
001300050315      *
001400050315     d k_ALL           c                   const('*ALL      ')
001500050317     ***d k_LIBL          c                   const('*LIBL     ')
001600050317     ***d k_CURLIB        c                   const('*CURLIB   ')
001700050315      *
001800050315      *?S C H I E R E?------------------------------------------------*
001900050315      *
002000050317     d $jobq           s             10    dim(99)    inz
002100050317     d $jobql          s             10    dim(99)    inz
002200050317     d $Msg            s             78    dim(8)  ctdata perrcd(1)
002300050315      *
002400050315      *?S T R U T T U R E   D A T I?----------------------------------*
002500050315      *
002600050315     d Tibs34ds      e ds
002700050315     d Azuteds       e ds                  extname(AZUTE00F)
002800050315     d dDatiute      e ds
002900050315      *
003000050315     d KPJBA         e ds
003100050315      *
003200050315     d Status         sds
003300050315     d  SDSpgm           *proc
003400050317     d  SDSprm           *parms
003500050317      *
003600050317     d ds_jbq          ds
003700050401     d  WHJjbqQ                      10
003800050401     d  WHJjbqL                      10
003900050315      *
004000050315      *?V A R I A B I L I?--------------------------------------------*
004100050315      *
004200050315     d wKPJBA          s                   like(KPJBA)
004300050315      *
004400050315      * flag booleani
004500050315     d $Fine           s              1    inz(*off)
004600050315      *
004700050317      * indici di schiera
004800050317     d xx              s              3  0 inz
004900050317     d yy              s              3  0 inz
005000050317      *
005100050315      * Parametri x pgm. QCMDEXC
005200050315     d Qcmd            s            100    inz
005300050315     d Qlen            s             15  5 inz(100)
005400050315      *
005500050401      * Parametri x pgm. TNSY13R
005600050406     d PRPjobq         s             10    inz
005700050406     d PRPjobqlib      s             10    inz
005800050315      *
005900050315      *?I N D I C A T O R I?------------------------------------------*
006000050315      *
006100050315      *  10 - Comodo
006200050315      *  28 - Visualizzazione V1CMSG
006300050315      *  51 - Posiz. cursore su campo V1Cjobq
006400050315      *  52 - Posiz. cursore su campo V1Cjobqlib
006500050315      *  90 - Generico di errore
006600050315      *
006700050315      *---------------------------------------------------------------*
006800050317      *
006900050317      * Caricamento elenco code lavori di cui sono stati congelati job
007000050317     c                   exsr      IntabJobQ
007100050315      *
007200050315      *?Inizializzazione videata D1?
007300050401     c                   clear                   tnsy12d1
007400050315      *
007500050315do  1c                   DOU       $Fine = *on
007600050315      *
007700050315      *?Emissione testata e riga tasti funzionali abilitati?
007800050315if  2c                   if        NOT *in90
007900050315      * - emissione testata (x rstdsp *no)
008000050401     c                   write     tnsy12t1
008100050315      * - emissione piede
008200050401     c                   write     tnsy12z1
008300050315e   2c                   endif
008400050315      *
008500050315      *?Emissione videata?
008600050401     c                   exfmt     tnsy12d1
008700050315     c                   setoff                                       28  90
008800050315     c                   clear                   V1Cmsg
008900050315      *
009000050315      * F3=Fine
009100050315if  2c                   if        *inKC
009200050315     c                   eval      $Fine  = *on
009300050315     c                   leave
009400050315e   2c                   endif
009500050713      *
009600050713      * F9=Interrogazione file WFHJQ00F
009700050713if  2c                   if        *inKI
009800050713     c                   eval      Qcmd    =  'DSPDBF FILE'
009900050713     c                                     +  '(GAITRAAZM/WFHJQ00F) '
010000050713     c                                     +  'MBR(*FIRST) '
010100050713     c                                     +  'RCDSLT(*YES) '
010200050713     c                                     +  'OUTPUT(*) '
010300050713     c                   call      'QCMDEXC'
010400050713     c                   parm                    Qcmd
010500050713     c                   parm                    Qlen
010600050713     c                   iter
010700050713e   2c                   endif
010800050315      *
010900050315      *?Controllo dati immessi a video?
011000050315     c                   exsr      sr_CtrD1
011100050315      *
011200050315      *?Richiamo cl se non rilevati errori?
011300050315if  2c                   if        NOT *in90 and *inKF
011400050315     c                   exsr      Rls_JobInQ
011500050315     c                   eval      $Fine  = *on
011600050315     c                   leave
011700050315e   2c                   endif
011800050315      *
011900050315e   1c                   ENDDO
012000050315      *
012100050315      *?Fine?
012200050315     c                   eval      *inLR = *on
012300050315      *
012400050315      *?S U B R O U T I N E S?----------------------------------------*
012500050315      *
012600050315      *---------------------------------------------------------------*
012700050315      *?Routine Iniziale                                             ?*
012800050315      *---------------------------------------------------------------*
012900050315     c     *InzSR        BegSR
013000050315      *
013100050315     c     *Entry        plist
013200050315     c                   parm                    Wkpjba
013300050315     c                   if        SDSprm > *zeros
013400050315     c                   movel     Wkpjba        KPJBA
013500050315     c                   endif
013600050315      *
013700050315      * Reperisco dati job
013800050401     c                   if        SDSprm > *zeros
013900050315     c                   exsr      DatiJob
014000050401     c                   endif
014100050315     c                   movel     SDSpgm        VTCpgm
014200050315      *
014300050315     c                   EndSr
014400050315      *
014500050315      *---------------------------------------------------------------*
014600050315      *?Reperimento Dati del job (Utente/Operativi)                  ?*
014700050315      *---------------------------------------------------------------*
014800050315     c     DatiJob       BegSr
014900050315      *
015000050315     c     *dtaara       define    §azute        azuteds
015100050315     c     *dtaara       define    §datiute      ddatiute
015200050315      *
015300050315     c                   in(E)     *dtaara
015400050315     c                   IF        %ERROR or RSUT = *blanks
015500050315     c                   clear                   Tibs34Ds
015600050315     c                   call      'TIBS34R'
015700050315     c                   parm                    Tibs34Ds
015800050315     c                   in        *dtaara
015900050315     c                   ENDIF
016000050315      *
016100050315     c                   EndSr
016200050317      *
016300050317      *---------------------------------------------------------------*
016400050317      *?Caricamento elenco JobQ di cui sono stati congelati Job      ?*
016500050317      *---------------------------------------------------------------*
016600050317     c     IntabJobQ     BEGSR
016700050317      *
016800050401     c                   read      WFHJQ000
016900050401do  1c                   DOW       NOT %eof(WFHJQ00F)
017000050317      *
017100050401if  2c                   if        WHJatb = *blanks
017200050317      *
017300050317     c                   eval      yy    = 1
017400050317     c                   eval      *in10 = *off
017500050401     c                   movel     WHJjbq        ds_jbq
017600050401     c     WHJjbqq       lookup    $jobq(yy)                              10
017700050317if  3c                   if        NOT *in10
017800050317     c                             or  *in10
017900050401     c                             and WHJjbql <> $jobql(yy)
018000050317     c                   add       1             xx
018100050401     c                   movel     WHJjbq        $jobq(xx)
018200050401     c                   move      WHJjbq        $jobql(xx)
018300050317e   3c                   endif
018400050317      *
018500050317e   2c                   endif
018600050317      *
018700050401     c                   read      WFHJQ000
018800050317e   1c                   ENDDO
018900050317      *
019000050317     c                   ENDSR
019100050315      *
019200050315      *---------------------------------------------------------------*
019300050315      *?Controllo dati in videata D1                                 ?*
019400050315      *---------------------------------------------------------------*
019500050315     c     sr_CtrD1      BEGSR
019600050315      *
019700050315     c                   movea     '0000000000'  *in(50)
019800050315      *
019900050315      * Coda lavori obbligatoria
020000050315if  1c                   if        V1Cjobq    =  *blanks
020100050315     c                   eval      V1Cmsg     =  $Msg(1)
020200050315     c                   seton                                        285190
020300050315     c                   leavesr
020400050315e   1c                   endif
020500050315      * Libreria coda lavori obbligatoria
020600050315if  1c                   if        V1Cjobqlib =  *blanks
020700050315     c                   eval      V1Cmsg     =  $Msg(2)
020800050315     c                   seton                                        285290
020900050315     c                   leavesr
021000050315e   1c                   endif
021100050315      * Verifica correttezza nome coda lavori
021200050315if  1c                   if        V1Cjobq    <> k_all
021300050712     c     '*'           scan      V1Cjobq
021400050712if  2c                   if        %found
021500050315     c                   eval      V1Cmsg     =  $Msg(3)
021600050315     c                   seton                                        285190
021700050315     c                   leavesr
021800050315e   2c                   endif
021900050315e   1c                   endif
022000050315      * Verifica correttezza nome libreria della coda lavori
022100050315if  1c                   if            V1Cjobqlib <> k_all
022200050317     c***                          and V1Cjobqlib <> k_libl
022300050315     c***                          and V1Cjobqlib <> k_curlib
022400050712     c     '*'           scan      V1Cjobqlib
022500050712if  2c                   if        %found
022600050315     c                   eval      V1Cmsg     =  $Msg(4)
022700050315     c                   seton                                        285290
022800050315     c                   leavesr
022900050315e   2c                   endif
023000050315e   1c                   endif
023100050315      * Verifica esistenza libreria della coda lavori
023200050315if  1c                   if        V1Cjobqlib <> k_all
023300050315     c                   eval      Qcmd    =  'CHKOBJ OBJ('
023400050315     c                                     +  %trim(V1Cjobqlib)
023500050315     c                                     +  ') OBJTYPE(*LIB)'
023600050315     c                   call      'QCMDEXC'                            90
023700050315     c                   parm                    Qcmd
023800050315     c                   parm                    Qlen
023900050315if  2c                   if        *in90
024000050315     c                   eval      V1Cmsg  =  $Msg(6)
024100050315     c                   seton                                        2852
024200050315     c                   leavesr
024300050315e   2c                   endif
024400050315e   1c                   endif
024500050315      * Verifica esistenza coda lavori nella libreria
024600050315if  1c                   if        V1Cjobq    <> k_all
024700050315     c                   eval      Qcmd    =  'CHKOBJ OBJ('
024800050315     c                                     +  %trim(V1Cjobqlib)
024900050315     c                                     +  '/'
025000050315     c                                     +  %trim(V1Cjobq)
025100050315     c                                     +  ') OBJTYPE(*JOBQ)'
025200050315     c                   call      'QCMDEXC'                            90
025300050315     c                   parm                    Qcmd
025400050315     c                   parm                    Qlen
025500050315if  2c                   if        *in90
025600050315     c                   eval      V1Cmsg  =  $Msg(5)
025700050315     c                   seton                                        2851
025800050315     c                   leavesr
025900050315e   2c                   endif
026000050315e   1c                   endif
026100050317      *
026200050317      * Verifica se sono stati congelati dei lavori in quella coda
026300050317sel 1c                   select
026400050317w   1c                   when      V1Cjobq    <> k_all
026500050317     c                   eval      xx    =  1
026600050317     c                   eval      *in10 = *off
026700050317     c     V1Cjobq       lookup    $jobq(xx)                              10
026800050317do  2c                   dow           *in10
026900050317     c***                          and V1Cjobqlib <> k_all
027000050317     c                             and V1Cjobqlib <> $jobql(xx)
027100050317     c                             and xx         <  %elem($jobq)
027200050317     c                   eval      xx    =  xx + 1
027300050317     c                   eval      *in10 = *off
027400050317     c     V1Cjobq       lookup    $jobq(xx)                              10
027500050317e   2c                   enddo
027600050317if  2c                   if        NOT *in10
027700050317     c                   eval      V1Cmsg     =  $Msg(7)
027800050317     c                   seton                                        285190
027900050317     c                   leavesr
028000050317e   2c                   endif
028100050317w   1c                   when      V1Cjobqlib <> k_all
028200050317     c                   eval      xx    =  1
028300050317     c                   eval      *in10 = *off
028400050317     c     V1Cjobqlib    lookup    $jobql(xx)                             10
028500050317if  2c                   if        NOT *in10
028600050317     c                   eval      V1Cmsg     =  $Msg(8)
028700050317     c                   seton                                        285290
028800050317     c                   leavesr
028900050317e   2c                   endif
029000050317e   1c                   endsl
029100050315      *
029200050315     c                   ENDSR
029300050315      *
029400050315      *---------------------------------------------------------------*
029500050315      *?Rls_JobInQ * Rilascio lavori nella coda                      ?*
029600050315      *---------------------------------------------------------------*
029700050315     c     Rls_JobInQ    BEGSR
029800050315      *
029900050401     c                   call      'TNSY13R'
030000050406     c                   parm      V1Cjobq       PRPjobq
030100050406     c                   parm      V1Cjobqlib    PRPjobqlib
030200050406      *
030300050407      * Richiesta pulizia del file WFHJQ00F
030400050713if  1c***                if        V1Cclrpf = 'S'
030500050713     c***                eval      Qcmd    =  'CLRPFM FILE'
030600050713     c***                                  +  '(GAITRAAZM/WFHJQ00F)'
030700050713     c***                call      'QCMDEXC'
030800050713     c***                parm                    Qcmd
030900050713     c***                parm                    Qlen
031000050713e   2c***                endif
031100050315      *
031200050315     c                   ENDSR
031300050315      *
031400050315**   $Msg
031500050315Immettere il nome della coda lavori                                            1
031600050315Immettere il nome della libreria della coda lavori                             2
031700050315Nome coda lavori non valido                                                    3
031800050315Nome libreria coda lavori non valido                                           4
031900050315Coda lavori inesistente                                                        5
032000050315Libreria coda lavori inesistente                                               6
032100050317Non sono stati congelati lavori in questa coda                                 7
032200050317Non sono stati congelati lavori nelle code di questa libreria                  8
