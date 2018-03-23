000100050315      *---------------------------------------------------------------*
000200050315      *? Rilascio lavori in coda (precedentemente congelati)         ?*
000300050315      *---------------------------------------------------------------*
000400050315     h decedit('0,') datedit(*dmy/)
000500050315     h dftactgrp(*no) actgrp(*caller)
000600050315      *
000700050401     fWFHJQ00F  uf   e             disk
000800050315      *
000900050315      *?C O S T A N T I?---------------------------------------------*
001000050315      *
001100050315     d k_all           c                   const('*ALL      ')
001200050315      *
001300050315      *?S T R U T T U R E   D A T I?---------------------------------*
001400050315      *
001500050315     d Status         sds
001600050315     d  SDSpgm           *proc
001700050315     d  SDSprm           *parms
001800050315      *
001900050315      * Common Structure for Error Code Parameter
002000050317      /copy QSYSINC/QRPGLESRC,QUSEC
002100050315      *
002200050315      *?V A R I A B I L I?--------------------------------------------*
002300050315      *
002400050315      * Parametri ricevuti
002500050315     d PRRjobq         s             10
002600050315     d PRRjobqlib      s             10
002700050315      *
002800050315      * Parametri x API QMHSNDPM (invio messaggio programma)
002900050315     d SndPm01         s              7
003000050315     d SndPm02         s             20
003100050315     d SndPm03         s            256
003200050317     ***d SndPm04         s              9b 0 inz(%size(SndPm03))
003300050317     d SndPm04         s             10i 0 inz(%size(SndPm03))
003400050315     d SndPm05         s             10    inz('*INFO')
003500050315     d SndPm06         s             16    inz('*')
003600050317     ***d SndPm07         s              9b 0
003700050317     d SndPm07         s             10i 0
003800050315     d SndPm08         s              4
003900050315      *
004000050315      * Parametri x QCMDEXC
004100050315     d Qcmd            s             80    inz
004200050315     d Qlen            s             15  5 inz(80)
004300050315      *
004400050315      *?P A R A M E T R I?--------------------------------------------*
004500050315      *
004600050315     c     *Entry        plist
004700050315     c                   parm                    PRRjobq
004800050315     c                   parm                    PRRjobqlib
004900050315      *
005000050315      *?I N D I C A T O R I?------------------------------------------*
005100050315      *
005200050315      *  90  -  Errore
005300050315      *
005400050315      *---------------------------------------------------------------*
005500050315      *
005600050315      *?Controllo dei parametri ricevuti?
005700050315     c                   exsr      CtrParms
005800050315      *
005900050401      *?Ciclo di elaborazione del file WFHJQ00F?
006000050401     ***c                   eval      WHJjbk = PRRjobq + PRRjobqlib
006100050401     ***c     WHJjbk        setll     WFHJQ000
006200050401     c                   read      WFHJQ000
006300050315      *
006400050401do  1c                   dow       not %eof(WFHJQ00F)
006500050316      *
006600050401if  2c                   if        WHJatb = *blanks
006700050315     c                   exsr      RlsJobInQ
006800050316e   2c                   endif
006900050315      *
007000050401     c                   read      WFHJQ000
007100050315e   1c                   enddo
007200050315      *
007300050315      *?Fine?
007400050315     c                   exsr      EndPgm
007500050315      *
007600050315      **------------------------------------------------------------**
007700050315      **?Controllo parametri ricevuti                              ?**
007800050315      **------------------------------------------------------------**
007900050315     c     CtrParms      BEGSR
008000050315      *
008100050315      * La JOBQ è obbligatoria (può essere anche in formato generico*)
008200050315if  1c                   if        PRRjobq    = *blanks
008300050315     c                   eval      SndPM03    = 'ERRORE: il parametro -
008400050315     c                             JOBQ è stato ricevuto vuoto. -
008500050315     c                             Elaborazione non eseguita.'
008600050315     c                   exsr      APIsndPM
008700050315     c                   exsr      EndPgm
008800050315e   1c                   endif
008900050315      * Se la libreria della JOBQ è vuota, assumo *LIBL
009000050315if  1c                   if        PRRjobqlib = *blanks
009100050315     c                   eval      PRRjobqlib = '*LIBL'
009200050315     c                   eval      SndPM03    = 'INFO: come libreria -
009300050315     c                             della JOBQ è stata assunta '
009400050315     c                             + %trim(PRRjobqlib)
009500050315     c                             + '.'
009600050315     c                   exsr      APIsndPM
009700050315e   1c                   endif
009800050315      *
009900050315     c                   ENDSR
010000050315      *
010100050315      **------------------------------------------------------------**
010200050315      **?Invio messaggio programma                                 ?**
010300050315      **------------------------------------------------------------**
010400050315     c     ApiSndPm      BEGSR
010500050315      *
010600050315     c                   clear                   Qusec
010700050315     c                   eval      Qusbprv = %size(Qusec)
010800050315     c                   call      'QMHSNDPM'
010900050315     c                   parm                    SndPM01
011000050315     c                   parm                    SndPM02
011100050315     c                   parm                    SndPM03
011200050315     c                   parm                    SndPM04
011300050315     c                   parm                    SndPM05
011400050315     c                   parm                    SndPM06
011500050315     c                   parm                    SndPM07
011600050315     c                   parm                    SndPM08
011700050315     c                   parm                    Qusec
011800050315      *
011900050315     c                   ENDSR
012000050315      *
012100050315      **------------------------------------------------------------**
012200050315      **?Rilascio lavori in coda (precedentemente congelati)       ?**
012300050315      **------------------------------------------------------------**
012400050315     c     RlsJobInQ     BEGSR
012500050315      *
012600050315      * Selezione
012700050315sel 1c                   select
012800050315      * - sel. lavori di tutte le code in tutte le librerie
012900050315w   1c***                when           PRRjobq    =  k_all
013000050315     c***                          and  PRRjobqlib =  k_all
013100050315      * - sel. lavori di tutte le code in una libreria
013200050315w   1c                   when           PRRjobq    =  k_all
013300050315     c                             and  PRRjobqlib <> k_all
013400050315     c                             and  PRRjobqlib <>
013500050401     c                                  %subst(WHJjbq : 11 : 10)
013600050315     c                   leavesr
013700050315      * - sel. lavori di una coda in una libreria
013800050315w   1c                   when           PRRjobq    <> k_all
013900050315     c                             and (PRRjobq    <>
014000050401     c                                  %subst(WHJjbq : 11 : 10)
014100050315     c                              or  PRRjobqlib <>
014200050401     c                                  %subst(WHJjbq : 01 : 10))
014300050315     c                   leavesr
014400050315e   1c                   endsl
014500050315      *
014600050315      * Rilascio il lavoro
014700050315     c                   eval      Qcmd   = 'RLSJOB JOB('
014800050401     c                                    + %trim(WHJjbn) + '/'
014900050401     c                                    + %trim(WHJjbu) + '/'
015000050401     c                                    + %trim(WHJjbj) + ')'
015100050315     c                   call      'QCMDEXC'                            90
015200050315     c                   parm                    Qcmd
015300050315     c                   parm                    Qlen
015400050315      *
015500050315      * Annullo il record relativo al job nel file
015600050315      * (anche se non è riuscito il rilascio: magari il lavoro era già
015700050315      *  stato rilasciato o cancellato...)
015800050315if  1c***                if        not *in90
015900050406     c                   eval      WHJatb = 'A'
016000050401     c                   UPDATE    WFHJQ000
016100050315e   1c***                endif
016200050315      *
016300050315     c                   ENDSR
016400050315      *
016500050315      **------------------------------------------------------------**
016600050315      **?Fine Programma.                                           ?**
016700050315      **------------------------------------------------------------**
016800050315     c     EndPgm        BEGSR
016900050315      *
017000050315     c                   eval      *inLR = *on
017100050315     c                   return
017200050315      *
017300050315     c                   ENDSR
