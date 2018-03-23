000100050311      *---------------------------------------------------------------*
000200050315      *? Congelamento lavori in coda                                 ?*
000300050311      *---------------------------------------------------------------*
000400050311      *
000500050311     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600050311      *
000700050315      *?A R C H I V I?------------------------------------------------*
000800050311      *
000900050311     fTNSY10D   cf   e             workstn
001000050311      *
001100050315      *?C O S T A N T I?----------------------------------------------*
001200050311      *
001300050311     d k_ALL           c                   const('*ALL      ')
001400050317     ***d k_LIBL          c                   const('*LIBL     ')
001500050317     ***d k_CURLIB        c                   const('*CURLIB   ')
001600050311      *
001700050315      *?S C H I E R E?------------------------------------------------*
001800050311      *
001900050713     d $Msg            s             78    dim(10) ctdata perrcd(1)
002000050311      *
002100050315      *?S T R U T T U R E   D A T I?----------------------------------*
002200050311      *
002300050311     d Tibs34ds      e ds
002400050311     d Azuteds       e ds                  extname(AZUTE00F)
002500050311     d dDatiute      e ds
002600050311      *
002700050311     d KPJBA         e ds
002800050311      *
002900050311     d Status         sds
003000050311     d  SDSpgm           *proc
003100050317     d  SDSprm           *parms
003200050311      *
003300050315      *?V A R I A B I L I?--------------------------------------------*
003400050311      *
003500050311     d wKPJBA          s                   like(KPJBA)
003600050311      *
003700050311      * flag booleani
003800050311     d $Fine           s              1    inz(*off)
003900050311      *
004000050311      * Parametri x pgm. QCMDEXC
004100050311     d Qcmd            s            100    inz
004200050311     d Qlen            s             15  5 inz(100)
004300050311      *
004400050406      * Parametri x pgm. TNSY11R
004500050406     d PRPjobq         s             10    inz
004600050406     d PRPjobqlib      s             10    inz
004700050406     d PRPejbq         s             10    inz
004800050406     d PRPejbqlib      s             10    inz
004900050317      *
005000050317      * Parametri x API QSPRJOBQ (retrieve jobq queue information)
005100050317      /copy QSYSINC/QRPGLESRC,QSPRJOBQ
005200050317      * - Lenght of receiver variable
005300050317     ***d Jqi01           s              4b 0 inz(%size(Qspq010000))
005400050329     d Jqi01           s              5i 0 inz(%size(Qspq010000))
005500050317      * - Format name
005600050317     d Jqi02           s              8    inz('JOBQ0100')
005700050317      * - Qualified job queue name
005800050317     d Jqi03           s             20    inz
005900050317      *
006000050317      * Common Structure for Error Code Parameter
006100050317      /copy QSYSINC/QRPGLESRC,QUSEC
006200050311      *
006300050315      *?I N D I C A T O R I?------------------------------------------*
006400050311      *
006500050315      *  10 - Comodo
006600050311      *  28 - Visualizzazione V1CMSG
006700050311      *  51 - Posiz. cursore su campo V1Cjobq
006800050311      *  52 - Posiz. cursore su campo V1Cjobqlib
006900050311      *  53 - Posiz. cursore su campo V1Cejbq
007000050311      *  54 - Posiz. cursore su campo V1Cejbqlib
007100050713      *  55 - Posiz. cursore su campo V1Cclrpf
007200050311      *  90 - Generico di errore
007300050311      *
007400050311      *---------------------------------------------------------------*
007500050311      *
007600050315      *?Inizializzazione videata D1?
007700050311     c                   clear                   tnsy10d1
007800050311      *
007900050311do  1c                   DOU       $Fine = *on
008000050311      *
008100050315      *?Emissione testata e riga tasti funzionali abilitati?
008200050311if  2c                   if        NOT *in90
008300050311      * - emissione testata (x rstdsp *no)
008400050311     c                   write     tnsy10t1
008500050311      * - emissione piede
008600050311     c                   write     tnsy10z1
008700050311e   2c                   endif
008800050311      *
008900050315      *?Emissione videata?
009000050315     c                   exfmt     tnsy10d1
009100050311     c                   setoff                                       28  90
009200050311     c                   clear                   V1Cmsg
009300050311      *
009400050311      * F3=Fine
009500050311if  2c                   if        *inKC
009600050311     c                   eval      $Fine  = *on
009700050311     c                   leave
009800050311e   2c                   endif
009900050407      *
010000050407      * F9=Interrogazione file WFHJQ00F
010100050407if  2c                   if        *inKI
010200050407     c                   eval      Qcmd    =  'DSPDBF FILE'
010300050407     c                                     +  '(GAITRAAZM/WFHJQ00F) '
010400050407     c                                     +  'MBR(*FIRST) '
010500050407     c                                     +  'RCDSLT(*YES) '
010600050407     c                                     +  'OUTPUT(*) '
010700050407     c                   call      'QCMDEXC'
010800050407     c                   parm                    Qcmd
010900050407     c                   parm                    Qlen
011000050407     c                   iter
011100050407e   2c                   endif
011200050311      *
011300050315      *?Controllo dati immessi a video?
011400050311     c                   exsr      sr_CtrD1
011500050311      *
011600050315      *?Richiamo cl se non rilevati errori?
011700050311if  2c                   if        NOT *in90 and *inKF
011800050311     c                   exsr      Hld_JobInQ
011900050311     c                   eval      $Fine  = *on
012000050311     c                   leave
012100050311e   2c                   endif
012200050311      *
012300050311e   1c                   ENDDO
012400050311      *
012500050315      *?Fine?
012600050311     c                   eval      *inLR = *on
012700050311      *
012800050315      *?S U B R O U T I N E S?----------------------------------------*
012900050311      *
013000050311      *---------------------------------------------------------------*
013100050315      *?Routine Iniziale                                             ?*
013200050311      *---------------------------------------------------------------*
013300050311     c     *InzSR        BegSR
013400050311      *
013500050311     c     *Entry        plist
013600050311     c                   parm                    Wkpjba
013700050311     c                   if        SDSprm > *zeros
013800050311     c                   movel     Wkpjba        KPJBA
013900050311     c                   endif
014000050311      *
014100050311      * Reperisco dati job
014200050401     c                   if        SDSprm > *zeros
014300050311     c                   exsr      DatiJob
014400050401     c                   endif
014500050311     c                   movel     SDSpgm        VTCpgm
014600050311      *
014700050311     c                   EndSr
014800050311      *
014900050311      *---------------------------------------------------------------*
015000050315      *?Reperimento Dati del job (Utente/Operativi)                  ?*
015100050311      *---------------------------------------------------------------*
015200050311     c     DatiJob       BegSr
015300050311      *
015400050311     c     *dtaara       define    §azute        azuteds
015500050311     c     *dtaara       define    §datiute      ddatiute
015600050311      *
015700050311     c                   in(E)     *dtaara
015800050311     c                   IF        %ERROR or RSUT = *blanks
015900050311     c                   clear                   Tibs34Ds
016000050311     c                   call      'TIBS34R'
016100050311     c                   parm                    Tibs34Ds
016200050311     c                   in        *dtaara
016300050311     c                   ENDIF
016400050311      *
016500050311     c                   EndSr
016600050311      *
016700050311      *---------------------------------------------------------------*
016800050315      *?Controllo dati in videata D1                                 ?*
016900050311      *---------------------------------------------------------------*
017000050311     c     sr_CtrD1      BEGSR
017100050311      *
017200050311     c                   movea     '0000000000'  *in(50)
017300050311      *
017400050311      * Coda lavori obbligatoria
017500050311if  1c                   if        V1Cjobq    =  *blanks
017600050311     c                   eval      V1Cmsg     =  $Msg(1)
017700050311     c                   seton                                        285190
017800050311     c                   leavesr
017900050311e   1c                   endif
018000050311      * Libreria coda lavori obbligatoria
018100050311if  1c                   if        V1Cjobqlib =  *blanks
018200050311     c                   eval      V1Cmsg     =  $Msg(2)
018300050311     c                   seton                                        285290
018400050311     c                   leavesr
018500050311e   1c                   endif
018600050311      * Verifica correttezza nome coda lavori
018700050311if  1c                   if        V1Cjobq    <> k_all
018800050316     c     '*'           scan      V1Cjobq
018900050316if  2c                   if        %found
019000050311     c                   eval      V1Cmsg     =  $Msg(3)
019100050315     c                   seton                                        285190
019200050311     c                   leavesr
019300050311e   2c                   endif
019400050311e   1c                   endif
019500050311      * Verifica correttezza nome libreria della coda lavori
019600050311if  1c                   if            V1Cjobqlib <> k_all
019700050316     c***                          and V1Cjobqlib <> k_libl
019800050311     c***                          and V1Cjobqlib <> k_curlib
019900050316     c     '*'           scan      V1Cjobqlib
020000050316if  2c                   if        %found
020100050311     c                   eval      V1Cmsg     =  $Msg(4)
020200050315     c                   seton                                        285290
020300050311     c                   leavesr
020400050311e   2c                   endif
020500050311e   1c                   endif
020600050316if  1c                   if            V1Cjobq    <> k_all
020700050316     c                             and V1Cjobqlib =  k_all
020800050316     c     '*'           scan      V1Cjobqlib
020900050316if  2c                   if        %found
021000050329      *
021100050329     c                   eval      V1Cmsg     =  %trim($Msg(4))
021200050329     c                                        +  ' per coda lavori +
021300050329     c                                        specifica (non "*ALL")'
021400050316     c                   seton                                        285290
021500050316     c                   leavesr
021600050316e   2c                   endif
021700050316e   1c                   endif
021800050311      * Verifica esistenza libreria della coda lavori
021900050311if  1c                   if        V1Cjobqlib <> k_all
022000050311     c                   eval      Qcmd    =  'CHKOBJ OBJ('
022100050311     c                                     +  %trim(V1Cjobqlib)
022200050311     c                                     +  ') OBJTYPE(*LIB)'
022300050311     c                   call      'QCMDEXC'                            90
022400050311     c                   parm                    Qcmd
022500050311     c                   parm                    Qlen
022600050311if  2c                   if        *in90
022700050311     c                   eval      V1Cmsg  =  $Msg(6)
022800050311     c                   seton                                        2852
022900050311     c                   leavesr
023000050311e   2c                   endif
023100050311e   1c                   endif
023200050311      * Verifica esistenza coda lavori nella libreria
023300050311if  1c                   if        V1Cjobq    <> k_all
023400050311     c                   eval      Qcmd    =  'CHKOBJ OBJ('
023500050311     c                                     +  %trim(V1Cjobqlib)
023600050311     c                                     +  '/'
023700050311     c                                     +  %trim(V1Cjobq)
023800050311     c                                     +  ') OBJTYPE(*JOBQ)'
023900050311     c                   call      'QCMDEXC'                            90
024000050311     c                   parm                    Qcmd
024100050311     c                   parm                    Qlen
024200050311if  2c                   if        *in90
024300050311     c                   eval      V1Cmsg  =  $Msg(5)
024400050311     c                   seton                                        2851
024500050311     c                   leavesr
024600050311e   2c                   endif
024700050311e   1c                   endif
024800050311      *
024900050311      * Coda lavori da escludere facoltativa
025000050311if  1c                   if            V1Cejbq    =  *blanks
025100050311     c                             and V1Cejbqlib <> *blanks
025200050311     c                   eval      V1Cmsg     =  $Msg(1)
025300050311     c                   seton                                        285390
025400050311     c                   leavesr
025500050311e   1c                   endif
025600050311      * Libreria coda lavori da escludere facoltativa
025700050311if  1c                   if            V1Cejbq    <> *blanks
025800050311     c                             and V1Cejbqlib =  *blanks
025900050311     c                   eval      V1Cmsg     =  $Msg(2)
026000050311     c                   seton                                        285490
026100050311     c                   leavesr
026200050311e   1c                   endif
026300050311      * Verifica correttezza nome coda lavori da escludere
026400050311if  1c                   if        V1Cejbq    <> *blanks
026500050316     c     '*'           scan      V1Cejbq
026600050316if  2c                   if        %found
026700050311     c                   eval      V1Cmsg     =  $Msg(3)
026800050315     c                   seton                                        285390
026900050311     c                   leavesr
027000050311e   2c                   endif
027100050311e   1c                   endif
027200050311      * Verifica correttezza nome libreria della coda lavori da escludere
027300050311if  1c                   if        V1Cejbqlib <> *blanks
027400050316     c     '*'           scan      V1Cejbqlib
027500050316if  2c                   if        %found
027600050311     c                   eval      V1Cmsg     =  $Msg(4)
027700050315     c                   seton                                        285490
027800050311     c                   leavesr
027900050311e   2c                   endif
028000050311e   1c                   endif
028100050311      * Verifica esistenza libreria della coda lavori da escludere
028200050311if  1c                   if            V1Cejbqlib <> *blanks
028300050316     c***                          and V1Cejbqlib <> k_libl
028400050311     c                   eval      Qcmd    =  'CHKOBJ OBJ('
028500050311     c                                     +  %trim(V1Cejbqlib)
028600050311     c                                     +  ') OBJTYPE(*LIB)'
028700050311     c                   call      'QCMDEXC'                            90
028800050311     c                   parm                    Qcmd
028900050311     c                   parm                    Qlen
029000050311if  2c                   if        *in90
029100050311     c                   eval      V1Cmsg  =  $Msg(6)
029200050311     c                   seton                                        2854
029300050311     c                   leavesr
029400050311e   2c                   endif
029500050311e   1c                   endif
029600050311      * Verifica esistenza coda lavori da escludere nella libreria
029700050311if  1c                   if        V1Cejbq    <> *blanks
029800050311     c                   eval      Qcmd    =  'CHKOBJ OBJ('
029900050311     c                                     +  %trim(V1Cejbqlib)
030000050311     c                                     +  '/'
030100050311     c                                     +  %trim(V1Cejbq)
030200050311     c                                     +  ') OBJTYPE(*JOBQ)'
030300050311     c                   call      'QCMDEXC'                            90
030400050311     c                   parm                    Qcmd
030500050311     c                   parm                    Qlen
030600050311if  2c                   if        *in90
030700050311     c                   eval      V1Cmsg  =  $Msg(5)
030800050311     c                   seton                                        2853
030900050311     c                   leavesr
031000050311e   2c                   endif
031100050311e   1c                   endif
031200050316      *
031300050316      * Verifica "senso" dell'esclusione
031400050316      * - coda di cui congelare i lavori e coda di cui escluderne il
031500050316      *   congelamento NON possono coincidere
031600050316if  1c                   if            V1Cjobq    =  V1Cejbq
031700050316     c                             and V1Cjobqlib =  V1Cejbqlib
031800050316     c                   eval      V1Cmsg     =  $Msg(7)
031900050316     c                   seton                                        285390
032000050316     c                   leavesr
032100050316e   1c                   endif
032200050316      * - congelando i lavori di una coda NON si può specificare una
032300050316      *   coda da escludere
032400050316if  1c                   if            V1Cjobq    <> k_all
032500050316     c                             and V1Cejbq    <> *blanks
032600050316      * - congelando i lavori di tutte le code di una libreria NON si
032700050316      *   può escludere una coda di un'altra libreria
032800050316     c                             or  V1Cjobq    =  k_all
032900050316     c                             and V1Cjobqlib <> k_all
033000050316     c                             and V1Cejbq    <> *blanks
033100050316     c                             and V1Cejbqlib <> V1Cjobqlib
033200050316      * - congelando i lavori di una coda in qualsiassi libreria NON si
033300050316      *   può escludere un'altra coda di una libreria
033400050316      *   (NON ammesso: jobq UNA libr. *ALL - fallisce già il controllo
033500050316      *    di tale coda lavori)
033600050316     c***                          or  V1Cjobq    <> k_all
033700050316     c***                          and V1Cjobqlib =  k_all
033800050316     c***                          and V1Cejbq    <> *blanks
033900050316     c***                          and V1Cejbq    <> V1Cjobq
034000050316     c                   eval      V1Cmsg     =  $Msg(8)
034100050316     c                   seton                                        285390
034200050316     c                   leavesr
034300050316e   1c                   endif
034400050317      *
034500050317      * Verifica se ci sono lavori nella coda specificata
034600050317if  1c                   if            V1Cjobq    <> *blanks
034700050317     c                             and V1Cjobq    <> k_all
034800050317     c                             and V1Cjobqlib <> k_all
034900050317     c                   clear                   Qspq010000
035000050317     c                   eval      Jqi03 = V1Cjobq + V1Cjobqlib
035100050317     c                   clear                   Qusec
035200050317     c                   eval      Qusbprv = %size(Qusec)
035300050317     c                   call      'QSPRJOBQ'                           90
035400050317     c                   parm                    Qspq010000
035500050317     c                   parm                    Jqi01
035600050317     c                   parm                    Jqi02
035700050317     c                   parm                    Jqi03
035800050317     c                   parm                    Qusec
035900050317if  2c                   if           *in90
036000050317     c                             or Qspnbrj = *zeros
036100050317     c*** coda congelata:          or Qspjqs  = 'HELD      '
036200050317     c                   eval      V1Cmsg     =  $Msg(9)
036300050317     c                   seton                                        285190
036400050317     c                   leavesr
036500050317e   2c                   endif
036600050317e   1c                   endif
036700050317if  1c                   if            V1Cejbq    <> *blanks
036800050317     c                             and V1Cejbq    <> k_all
036900050317     c                             and V1Cejbqlib <> k_all
037000050317     c                   clear                   Qspq010000
037100050317     c                   eval      Jqi03 = V1Cejbq + V1Cejbqlib
037200050317     c                   clear                   Qusec
037300050317     c                   eval      Qusbprv = %size(Qusec)
037400050317     c                   call      'QSPRJOBQ'                           90
037500050317     c                   parm                    Qspq010000
037600050317     c                   parm                    Jqi01
037700050317     c                   parm                    Jqi02
037800050317     c                   parm                    Jqi03
037900050317     c                   parm                    Qusec
038000050317if  2c                   if           *in90
038100050317     c                             or Qspnbrj = *zeros
038200050317     c*** coda congelata:          or Qspjqs  = 'HELD      '
038300050317     c                   eval      V1Cmsg     =  $Msg(9)
038400050317     c                   seton                                        285390
038500050317     c                   leavesr
038600050317e   2c                   endif
038700050317e   1c                   endif
038800050713      *
038900050713      * Pulizia anticipata del file: 'S' o 'N'
039000050713if  1c                   if            V1Cclrpf   <> 'S'
039100050713     c                             and V1Cclrpf   <> 'N'
039200050713     c                   eval      V1Cmsg     =  $Msg(10)
039300050713     c                   seton                                        285590
039400050713     c                   leavesr
039500050713e   1c                   endif
039600050311      *
039700050311     c                   ENDSR
039800050311      *
039900050311      *---------------------------------------------------------------*
040000050315      *?Hld_JobInQ * Congelamento lavori nella coda                  ?*
040100050311      *---------------------------------------------------------------*
040200050311     c     Hld_JobInQ    BEGSR
040300050311      *
040400050407      * Richiesta pulizia del file WFHJQ00F
040500050407if  1c                   if        V1Cclrpf = 'S'
040600050407     c                   eval      Qcmd     = 'CLRPFM FILE'
040700050407     c                                      + '(GAITRAAZM/WFHJQ00F)'
040800050407     c                   call      'QCMDEXC'
040900050407     c                   parm                    Qcmd
041000050407     c                   parm                    Qlen
041100050406e   2c                   endif
041200050406      *
041300050401     c                   call      'TNSY11R'
041400050406     c                   parm      V1Cjobq       PRPjobq
041500050406     c                   parm      V1Cjobqlib    PRPjobqlib
041600050406     c                   parm      V1Cejbq       PRPejbq
041700050406     c                   parm      V1Cejbqlib    PRPejbqlib
041800050311      *
041900050311     c                   ENDSR
042000050311      *
042100050316      *---------------------------------------------------------------*
042200050311**   $Msg
042300050311Immettere il nome della coda lavori                                            1
042400050311Immettere il nome della libreria della coda lavori                             2
042500050311Nome coda lavori non valido                                                    3
042600050311Nome libreria coda lavori non valido                                           4
042700050311Coda lavori inesistente                                                        5
042800050311Libreria coda lavori inesistente                                               6
042900050316La coda da escludere coincide con quella di cui congelare i lavori             7
043000050316La coda da escludere NON rientra tra quelle di cui congelare i lavori          8
043100050317Coda lavori "vuota"                                                            9
043200050713Immettere "S" (sì) o "N" (no)                                                 10
