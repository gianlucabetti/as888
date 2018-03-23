000100050311      *---------------------------------------------------------------*
000200050311      *? Congelamento lavori in coda                                 ?*
000300050311      *---------------------------------------------------------------*
000400050311     h decedit('0,') datedit(*dmy/)
000500050311     h dftactgrp(*no) actgrp(*caller)
000600050311      *
000700050401     fWFHJQ00F  o    e             disk
000800050315      *
000900050315      *?C O S T A N T I?---------------------------------------------*
001000050315      *
001100050315     d k_all           c                   const('*ALL      ')
001200050311      *
001300050315      *?S T R U T T U R E   D A T I?---------------------------------*
001400050311      *
001500050311     d Status         sds
001600050311     d  SDSpgm           *proc
001700050311     d  SDSprm           *parms
001800050311      *
001900050315      *?V A R I A B I L I?--------------------------------------------*
002000050311      *
002100050311      * Parametri ricevuti
002200050311     d PRRjobq         s             10
002300050311     d PRRjobqlib      s             10
002400050311     d PRRejbq         s             10
002500050311     d PRRejbqlib      s             10
002600050311      * Campi di lavoro
002700050315     d WRKcount        s              5  0 inz
002800050311      *
002900050311      * Common Structures for List APIs to A User Space
003000050317      /copy QSYSINC/QRPGLESRC,QUSGEN
003100050311      * List Job API
003200050317      /copy QSYSINC/QRPGLESRC,QUSLJOB
003300050315      * Retrieve Job Information API
003400050317      /copy QSYSINC/QRPGLESRC,QUSRJOBI
003500050315      * Common Structure for Error Code Parameter
003600050317      /copy QSYSINC/QRPGLESRC,QUSEC
003700050311      *
003800050311      * Parametri x API QUSCRTUS (creazione user space)
003900050311     d CrtUs01         s             20
004000050311     d CrtUs02         s             10
004100050317     ***d CrtUs03         s              9b 0
004200050317     d CrtUs03         s             10i 0
004300050311     d CrtUs04         s              1
004400050311     d CrtUs05         s             10
004500050311     d CrtUs06         s             50
004600050311     d CrtUs07         s             10
004700050311      * Parametri x API QUSRTVUS (reperimento user space)
004800050311     d RtvUs01         s             20
004900050317     ***d RtvUs02         s              9b 0
005000050317     ***d RtvUs03         s              9b 0
005100050317     d RtvUs02         s             10i 0
005200050317     d RtvUs03         s             10i 0
005300050311      * Parametri x API QUSDLTUS (cancellazione user space)
005400050311     d DltUs01         s             20
005500050311      * Parametri x API QUSLJOB  (reperimento lavori in coda)
005600050311      * - Qualified user space name
005700050311     d Ljob01          s             20
005800050311      * - Format name
005900050311     d Ljob02          s              8    inz('JOBL0100')
006000050311      * - Qualified job name
006100050311     d Ljob03          s             26    inz('*ALL      *ALL      *ALL  ')
006200050311      * - Status
006300050311     d Ljob04          s             10
006400050315      *
006500050315      * Parametri x API QUSRJOBI (reperimento informazioni sul lavoro)
006600050315      * - Lenght of receiver variable
006700050317     ***d Ljob12          s              4b 0 inz(%size(Qusi030000))
006800050329     d Ljob12          s              5i 0 inz(%size(Qusi030000))
006900050315      * - Format name
007000050315     d Ljob13          s              8    inz('JOBI0300')
007100050315      * - Qualified job name
007200050315     d Ljob14          s             26    inz
007300050315      * - Internal job identifier
007400050315     d Ljob15          s             16    inz
007500050311      * Parametri x API QMHSNDPM (invio messaggio programma)
007600050311     d SndPm01         s              7
007700050311     d SndPm02         s             20
007800050311     d SndPm03         s            256
007900050317     ***d SndPm04         s              9b 0 inz(%size(SndPm03))
008000050317     d SndPm04         s             10i 0 inz(%size(SndPm03))
008100050311     d SndPm05         s             10    inz('*INFO')
008200050311     d SndPm06         s             16    inz('*')
008300050317     ***d SndPm07         s              9b 0
008400050317     d SndPm07         s             10i 0
008500050311     d SndPm08         s              4
008600050315      *
008700050315      * Parametri x QCMDEXC
008800050315     d Qcmd            s             80    inz
008900050315     d Qlen            s             15  5 inz(80)
009000050311      *
009100050315      *?P A R A M E T R I?--------------------------------------------*
009200050311      *
009300050311     c     *Entry        plist
009400050311     c                   parm                    PRRjobq
009500050311     c                   parm                    PRRjobqlib
009600050311     c                   parm                    PRRejbq
009700050311     c                   parm                    PRRejbqlib
009800050311      *
009900050315      *?I N D I C A T O R I?------------------------------------------*
010000050311      *
010100050311      *  90  -  Errore
010200050311      *
010300050311      *---------------------------------------------------------------*
010400050311      *
010500050315      *?Controllo dei parametri ricevuti?
010600050311     c                   exsr      CtrParms
010700050311      *
010800050315      *?Creazione dello user space?
010900050311     c                   exsr      CrtUsrSpc
011000050311      *
011100050315      *?Gestione dello user space?
011200050311      * - Imposto la posizione iniziale del primo elemento e la lunghezza
011300050311     c                   eval      RtvUs02 = Qusold + 1
011400050311     c                   eval      RtvUs03 = Qussee
011500050311      * - Elaboro gli elementi
011600050311     c                   reset                   WRKcount
011700050311do  1c                   dow       Qusnbrle > WRKcount
011800050311     c                   exsr      InUsrSpc
011900050315      * - - Imposto il prossimo elemento e la sua posizione iniziale.
012000050315     c                   eval      RtvUs02 = RtvUs02 + Qussee
012100050311     c                   add       1             WRKcount
012200050311e   1c                   enddo
012300050311      *
012400050315      *?Cancellazione dello user space?
012500050311     c                   exsr      DltUsrSpc
012600050311      *
012700050315      *?Fine?
012800050311     c                   exsr      EndPgm
012900050311      *
013000050311      **------------------------------------------------------------**
013100050315      **?Controllo parametri ricevuti                              ?**
013200050311      **------------------------------------------------------------**
013300050311     c     CtrParms      BEGSR
013400050311      *
013500050311      * La JOBQ è obbligatoria (può essere anche in formato generico*)
013600050315if  1c                   if        PRRjobq    = *blanks
013700050311     c                   eval      SndPM03    = 'ERRORE: il parametro -
013800050311     c                             JOBQ è stato ricevuto vuoto. -
013900050311     c                             Elaborazione non eseguita.'
014000050311     c                   exsr      APIsndPM
014100050311     c                   exsr      EndPgm
014200050315e   1c                   endif
014300050315      * Se la libreria della JOBQ è vuota, assumo *LIBL
014400050315if  1c                   if        PRRjobqlib = *blanks
014500050311     c                   eval      PRRjobqlib = '*LIBL'
014600050311     c                   eval      SndPM03    = 'INFO: come libreria -
014700050311     c                             della JOBQ è stata assunta '
014800050315     c                             + %trim(PRRjobqlib)
014900050311     c                             + '.'
015000050311     c                   exsr      APIsndPM
015100050315e   1c                   endif
015200050315      * La JOBQ da escludere è facoltativa (non se immessa la libreria)
015300050315if  1c                   if            PRRejbq    = *blanks
015400050315     c                             and PRRejbqlib <> *blanks
015500050315     c                   eval      SndPM03    = 'ERRORE: il parametro -
015600050315     c                             JOBQ da escludere è stato ricevuto -
015700050315     c                             vuoto. Elaborazione non eseguita.'
015800050315     c                   exsr      APIsndPM
015900050315     c                   exsr      EndPgm
016000050315e   1c                   endif
016100050315      * Se la libreria dells JOBQ da escludere è vuota, assumo *LIBL
016200050315if  1c                   if            PRRejbq    <> *blanks
016300050315     c                             and PRRejbqlib =  *blanks
016400050315     c                   eval      PRRejbqlib = '*LIBL'
016500050315     c                   eval      SndPM03    = 'INFO: come libreria -
016600050315     c                             della JOBQ da escludere è stata -
016700050315     c                             assunta '  + %trim(PRRjobqlib)
016800050315     c                             + '.'
016900050315     c                   exsr      APIsndPM
017000050315e   1c                   endif
017100050311      *
017200050311     c                   ENDSR
017300050311      *
017400050311      **------------------------------------------------------------**
017500050315      **?Invio messaggio programma                                 ?**
017600050311      **------------------------------------------------------------**
017700050311     c     ApiSndPm      BEGSR
017800050311      *
017900050315     c                   clear                   Qusec
018000050315     c                   eval      Qusbprv = %size(Qusec)
018100050315     c                   call      'QMHSNDPM'
018200050315     c                   parm                    SndPM01
018300050315     c                   parm                    SndPM02
018400050315     c                   parm                    SndPM03
018500050315     c                   parm                    SndPM04
018600050315     c                   parm                    SndPM05
018700050315     c                   parm                    SndPM06
018800050315     c                   parm                    SndPM07
018900050315     c                   parm                    SndPM08
019000050315     c                   parm                    Qusec
019100050311      *
019200050311     c                   ENDSR
019300050311      *
019400050311      **------------------------------------------------------------**
019500050315      **?Creazione dello user space                                ?**
019600050311      **------------------------------------------------------------**
019700050311     c     CrtUsrSpc     BEGSR
019800050311      *
019900050311     c                   clear                   Qusec
020000050315     c                   eval      Qusbprv = %size(Qusec)
020100050311     c                   eval      CrtUS01 = SDSpgm + 'QTEMP'
020200050311      *
020300050315     c                   call      'QUSCRTUS'                           90
020400050315     c                   parm                    CrtUs01
020500050315     c                   parm                    CrtUs02
020600050315     c                   parm      1024          CrtUs03
020700050315     c                   parm                    CrtUs04
020800050315     c                   parm      '*CHANGE'     CrtUs05
020900050315     c                   parm                    CrtUs06
021000050315     c                   parm      '*YES'        CrtUs07
021100050311     c                   parm                    Qusec
021200050311      *
021300050315      * Riempimento dello user space con la lista dei JOB in JOBQ
021400050315     c                   clear                   Qusec
021500050315      *
021600050315     c                   call      'QUSLJOB'
021700050315     c                   parm      CrtUs01       Ljob01
021800050315     c                   parm      'JOBL0100'    Ljob02
021900050315     c                   parm                    Ljob03
022000050315     c                   parm      '*JOBQ'       Ljob04
022100050315     c                   parm                    Qusec
022200050311      *
022300050311      * Reperimento del numero di elementi e la lunghezza di un elemento
022400050311     c                   clear                   Qusec
022500050315     c                   eval      Qusbprv = %size(Qusec)
022600050315     c                   eval      RtvUs03 = %size(Qush0100)
022700050311      *
022800050311     c                   call      'QUSRTVUS'
022900050311     c                   parm      CrtUs01       RtvUs01
023000050311     c                   parm      1             RtvUs02
023100050311     c                   parm                    RtvUs03
023200050315     c                   parm                    Qush0100
023300050311     c                   parm                    Qusec
023400050311      *
023500050311      * Registrazione nel JOBLOG
023600050315if  1c                   if        Qusnbrle = *zeros
023700050311     c                   eval      SndPm03  = 'ERRORE: con i parametri '
023800050311     c                             + %trim(PRRjobqlib)
023900050311     c                             + '/'
024000050311     c                             + %trim(PRRjobq)
024100050315     c                             + ' non sono stati trovati dei lavori.'
024200050311     c                   exsr      APIsndPM
024300050311     c                   exsr      EndPgm
024400050315x   1c                   else
024500050315     c                   eval      SndPm03 =
024600050315     c                             'INFO: congelamento lavori in JOBQ '
024700050315     c                             + %trim(PRRjobqlib)
024800050315     c                             + '/'
024900050315     c                             + %trim(PRRjobq)
025000050315     c                             + '.'
025100050315     c                   exsr      APIsndPM
025200050311e   1c                   endif
025300050311      *
025400050311     c                   ENDSR
025500050311      *
025600050311      **------------------------------------------------------------**
025700050315      **?Reperimento dati dallo user space con la lista dei JOB    ?**
025800050311      **------------------------------------------------------------**
025900050311     c     InUsrSpc      BEGSR
026000050311      *
026100050315      * Reperimento dati dell'elemento
026200050311     c                   clear                   Qusec
026300050315     c                   eval      Qusbprv = %size(Qusec)
026400050315      *
026500050311     c                   call      'QUSRTVUS'
026600050311     c                   parm      CrtUs01       Rtvus01
026700050311     c                   parm                    Rtvus02
026800050311     c                   parm                    Rtvus03
026900050315     c                   parm                    Qusl010002
027000050311     c                   parm                    Qusec
027100050315      *
027200050315      * Reperimento coda lavori del job
027300050315     c                   clear                   Qusec
027400050315     c                   eval      Ljob12  = %size(Qusi030000)
027500050315     c                   eval      Ljob14  = Qusjnu
027600050315     c                                     + Qusunu
027700050315     c                                     + Qusjnbru
027800050315      *
027900050315     c                   call      'QUSRJOBI'
028000050315     c                   parm                    Qusi030000
028100050315     c                   parm                    Ljob12
028200050315     c                   parm      'JOBI0300'    Ljob13
028300050315     c                   parm                    Ljob14
028400050315     c                   parm      *blanks       Ljob15
028500050315     c                   parm                    Qusec
028600050315      *
028700050315      * Selezione dei soli lavori nella coda immessa
028800050315sel 1c                   select
028900050315      * - sel. lavori di tutte le code in tutte le librerie
029000050315w   1c***                when           PRRjobq    =  k_all
029100050315     c***                          and  PRRjobqlib =  k_all
029200050315      * - sel. lavori di tutte le code in una libreria
029300050315w   1c                   when           PRRjobq    =  k_all
029400050315     c                             and  PRRjobqlib <> k_all
029500050315     c                             and  Qusjl      <> PRRjobqlib
029600050315     c                   leavesr
029700050315      * - sel. lavori di una coda in una libreria
029800050315w   1c                   when           PRRjobq    <> k_all
029900050315     c                             and (Qusjn05    <> PRRjobq
030000050315     c                              or  Qusjl      <> PRRjobqlib)
030100050315     c                   leavesr
030200050315      * - lavori nella coda esclusa
030300050315w   1c                   when           Qusjn05    =  PRRejbq
030400050315     c                             and  Qusjl      =  PRRejbqlib
030500050315     c                   leavesr
030600050315      * - lavori di una coda compresa tra le "generiche*" da escludere
030700050315w   1c                   when           PRRejbq    <> *blanks
030800050315     c                             and  %subst(PRRejbq :
030900050315     c                                    %len(%trim(PRRejbq))-1 : 1)
031000050315     c                                    = '*'
031100050315     c                             and  %subst(Qusjn05 : 1 :
031200050315     c                                      %len(%trim(PRRejbq))-1)
031300050315     c                                    <> %subst(PRRejbq : 1 :
031400050315     c                                      %len(%trim(PRRejbq))-1)
031500050315     c                   leavesr
031600050315      * - lavori già congelati
031700050315w   1c                   when           Qussoj     =  'HLD       '
031800050315     c                   leavesr
031900050315e   1c                   endsl
032000050311      *
032100050311      * Congelamento lavori in coda
032200050311     c                   exsr      HldJobInQ
032300050311      *
032400050311     c                   ENDSR
032500050311      *
032600050311      **------------------------------------------------------------**
032700050315      **?Congelamento lavori in coda                               ?**
032800050311      **------------------------------------------------------------**
032900050315     c     HldJobInQ     BEGSR
033000050311      *
033100050311      * Memorizzo il job nel file
033200050401     c                   clear                   WFHJQ000
033300050401     c***                eval      WHJatb = *blanks
033400050401     c                   eval      WHJjbk = PRRjobq + PRRjobqlib
033500050401     c                   eval      WHJjbj = Qusjnu
033600050401     c                   eval      WHJjbu = Qusunu
033700050401     c                   eval      WHJjbn = Qusjnbru
033800050401     c                   eval      WHJjbq = Qusjn05 + Qusjl
033900050401     c***                eval      WHJjbs = Qussoj
034000050315      *
034100050401     c                   WRITE     WFHJQ000
034200050315      *
034300050315      * Congelo il lavoro
034400050315     c                   eval      Qcmd   = 'HLDJOB JOB('
034500050401     c                                    + %trim(WHJjbn) + '/'
034600050401     c                                    + %trim(WHJjbu) + '/'
034700050401     c                                    + %trim(WHJjbj) + ')'
034800050315     c                   call      'QCMDEXC'
034900050315     c                   parm                    Qcmd
035000050315     c                   parm                    Qlen
035100050311      *
035200050311     c                   ENDSR
035300050311      *
035400050311      **------------------------------------------------------------**
035500050315      **?Cancellazione dello user space                            ?**
035600050311      **------------------------------------------------------------**
035700050311     c     DltUsrSpc     BEGSR
035800050311      *
035900050311     c                   clear                   Qusec
036000050315     c                   eval      Qusbprv = %size(qusec)
036100050311     c                   call      'QUSDLTUS'
036200050311     c                   parm      CrtUS01       DltUS01
036300050311     c                   parm                    Qusec
036400050311      *
036500050311     c                   ENDSR
036600050315      *
036700050315      **------------------------------------------------------------**
036800050315      **?Fine Programma                                            ?**
036900050315      **------------------------------------------------------------**
037000050315     c     EndPgm        BEGSR
037100050315      *
037200050315     c                   eval      *inLR = *on
037300050315     c                   return
037400050315      *
037500050315     c                   ENDSR
