000100050404      /title Cancellazione vecchi spool da outq generiche.
000200050404      *---------------------------------------------------------------*
000300050404      *? Cancellazione vecchi spool da outq generiche                ?*
000400050404      *---------------------------------------------------------------*
000500050404      *
000600050404     h dftactgrp(*no) actgrp(*caller)
000700050404      *
000800050404      *?S T R U T T U R E   D A T I?----------------------------------*
000900050404      *
001000050405     d Status         sds
001100050405     d  SDSproc          *proc
001200050405     d  SDSparms         *parms
001300050404      *
001400050404      *?V A R I A B I L I?--------------------------------------------*
001500050404      *
001600050404      * - Parametri ricevuti
001700050404     d PRRuser         s             10
001800050404     d PRRoutq         s             10
001900050404     d PRRoutqlib      s             10
002000050404     d PRRformtype     s             10
002100050404     d PRRusrdta       s             10
002200051124     d PRRstatus       s              1
002300050404     d PRRdatpul       s              7
002400051123     d PRRrifdtp       s              1
002500050404     d PRRlista        s              1
002600050404      * - Parametri passati
002700080422     d PRPuser         s             10    inz
002800080422     d PRPoutq         s             10    inz
002900080422     d PRPoutqlib      s             10    inz
003000080422     d PRPformtype     s             10    inz
003100080422     d PRPusrdta       s             10    inz
003200080422     d PRPstatus       s              1    inz
003300080422     d PRPdatpul       s              7    inz
003400080422     d PRPrifdtp       s              1    inz
003500080422     d PRPlista        s              1    inz
003600050404      * - Campi di lavoro
003700080422     d WRKcount        s              5  0 inz
003800050404      *
003900050404      /copy QSYSINC/QRPGLESRC,QUSGEN
004000050404      /copy QSYSINC/QRPGLESRC,QUSLOBJ
004100050404      /copy QSYSINC/QRPGLESRC,QUSEC
004200080422     d Qused01                      256    inz
004300050404      *
004400080422     d Crtus01         s             20    inz
004500080422     d Crtus02         s             10    inz
004600080422     d Crtus03         s              9b 0 inz
004700080422     d Crtus04         s              1    inz
004800080422     d Crtus05         s             10    inz
004900080422     d Crtus06         s             50    inz
005000080422     d Crtus07         s             10    inz
005100050404      *
005200080422     d Rtvus01         s             20    inz
005300080422     d Rtvus02         s              9b 0 inz
005400080422     d Rtvus03         s              9b 0 inz
005500050404      *
005600080422     d Dltus01         s             20    inz
005700050404      *
005800080422     d Lobj01          s             20    inz
005900080422     d Lobj02          s              8    inz
006000080422     d Lobj03          s             20    inz
006100080422     d Lobj04          s             10    inz
006200050404      *
006300080422     d Sndpm01         s              7    inz
006400080422     d Sndpm02         s             20    inz
006500080422     d Sndpm03         s            256    inz
006600050404     d Sndpm04         s              9b 0 inz(%size(sndpm03))
006700050404     d Sndpm05         s             10    inz('*INFO')
006800050404     d Sndpm06         s             16    inz('*')
006900080422     d Sndpm07         s              9b 0 inz
007000080422     d Sndpm08         s              4    inz
007100050404      *
007200050404      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
007300050404      *
007400050406     c     *Entry        plist
007500050406     c                   parm                    PRRuser
007600050406     c                   parm                    PRRoutq
007700050406     c                   parm                    PRRoutqlib
007800050406     c                   parm                    PRRformtype
007900050406     c                   parm                    PRRusrdta
008000051124     c                   parm                    PRRstatus
008100050406     c                   parm                    PRRdatpul
008200051123     c                   parm                    PRRrifdtp
008300050406     c                   parm                    PRRlista
008400050406      * PRRlista: 1 = solo lista
008500050406      *           2 = solo cancellazione
008600050406      *           3 = lista e cancellazione
008700050406      *
008800050404     c                   exsr      CtrParms
008900050404      *
009000050404      *?Creazione dello user space?
009100050405     c                   eval      Crtus01 = SDSproc + 'QTEMP'
009200050404     c                   clear                   qusec
009300050404     c                   eval      Qusbprv = %size(Qusec)
009400050404     c                   call      'QUSCRTUS'
009500050404     c                   parm                    Crtus01
009600050404     c                   parm                    Crtus02
009700050404     c                   parm      1024          Crtus03
009800050404     c                   parm                    Crtus04
009900050404     c                   parm      '*CHANGE'     Crtus05
010000050404     c                   parm                    Crtus06
010100050404     c                   parm      '*YES'        Crtus07
010200050404     c                   parm                    Qusec
010300050404      *
010400050404      *?Riempimmento dello user space con la lista delle OUTQ?
010500050404     c                   clear                   Qusec
010600050404     c                   eval      Qusbprv = %size(Qusec)
010700050404     c                   eval      Lobj03 = PRRoutq + PRRoutqlib
010800050404     c                   call      'QUSLOBJ'
010900050404     c                   parm      Crtus01       Lobj01
011000050404     c                   parm      'OBJL0200'    Lobj02
011100050404     c                   parm                    Lobj03
011200050404     c                   parm      '*OUTQ'       Lobj04
011300050404     c                   parm                    Qusec
011400050404      *
011500050404      *?Reperimento del numero di elementi ?
011600050404      *?          e della lunghezza di un elemento?
011700050404     c                   clear                   qusec
011800050404     c                   eval      Qusbprv = %size(Qusec)
011900050404     c                   eval      Rtvus03 = %size(Qush0100)
012000050404     c                   call      'QUSRTVUS'
012100050404     c                   parm      Crtus01       Rtvus01
012200050404     c                   parm      1             Rtvus02
012300050404     c                   parm                    Rtvus03
012400050404     c                   parm                    Qush0100
012500050404     c                   parm                    Qusec
012600050404      *
012700050404      *?Registrazione nel JOBLOG?
012800050404if  1c                   if        Qusnbrle = 0
012900050404     c                   eval      Sndpm03 =
013000050404     c                             'ERRORE: con i parametri '
013100050406     c                             + %trim(PRRoutqlib)
013200050404     c                             + '/'
013300050406     c                             + %trim(PRRoutq)
013400050404     c                             + ' non sono state trovate delle OUTQ.'
013500050404     c                   exsr      ApiSndPm
013600050404     c                   exsr      Uscita
013700050404e   1c                   endif
013800050404      *
013900050404      *?Impostazione della posizione iniziale del primo elemento?
014000050404      *?           e della lunghezza?
014100050404     c                   eval      Rtvus02 = Qusold + 1
014200050404     c                   eval      Rtvus03 = Qussee
014300050404      *
014400050404      *?Ciclo di elaborazione degli elementi?
014500050404     c                   reset                   WRKcount
014600050404      *
014700050404do  1c                   DOW       Qusnbrle > WRKcount
014800050404      *
014900050404      *?Reperimento dell'elemento?
015000050404     c                   clear                   Qusec
015100050404     c                   eval      Qusbprv = %size(Qusec)
015200050404     c                   call      'QUSRTVUS'
015300050404     c                   parm      Crtus01       Rtvus01
015400050404     c                   parm                    Rtvus02
015500050404     c                   parm                    Rtvus03
015600050404     c                   parm                    Qusl020002
015700050404     c                   parm                    Qusec
015800050404      *?Registrazione nel JOBLOG?
015900050406if  2c                   if           (PRRlista =  '2'
016000050406     c                             or  PRRlista =  '3')
016100050404     c                   eval      Sndpm03 =
016200050404     c                             'INFO: cancellazione vecchi spool in '
016300050404     c                             + %trim(qusolnu00)
016400050404     c                             + '/'
016500050404     c                             + %trim(qusobjnu00)
016600080422      * - Indicazione dell'utente (PRRuser)
016700080422if  3c                   if        PRRuser     <> *blanks
016800080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
016900080422     c                                     + ', Utente: '
017000080422     c                                     + PRRuser
017100080422e   3c                   endif
017200080422      * - Indicazione del modulo di stampa (PRRformtype)
017300080422if  3c                   if        PRRformtype <> *blanks
017400080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
017500080422     c                                     + ', Modulo: '
017600080422     c                                     + PRRformtype
017700080422e   3c                   endif
017800080422      * - Indicazione dei dati utente (PRRusrdta)
017900080422if  3c                   if        PRRusrdta   <> *blanks
018000080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
018100080422     c                                     + ', Dati utente: '
018200080422     c                                     + PRRusrdta
018300080422e   3c                   endif
018400080422      * - Indicazione dello stato (PRRstatus)
018500080422if  3c                   if        PRRstatus   <> *blanks
018600080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
018700080422     c                                     + ', Stato: '
018800080422     c                                     + PRRstatus
018900080422e   3c                   endif
019000080422      * - Indicazione della data limite per la cancellazione (PRRdatpul)
019100080422if  3c                   if        PRRdatpul   <> *blanks
019200080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
019300080422     c                                     + ', Data limite: '
019400080422     c                                     + PRRdatpul
019500080422e   3c                   endif
019600080422      * - Indicazione della data di riferimento per la cancellazione (PRRrifdtp)
019700080422if  3c                   if        PRRrifdtp   <> *blanks
019800080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
019900080422     c                                     + ', Rif. data: '
020000080422     c                                     + PRRrifdtp
020100080422e   3c                   endif
020200080422      * - Chiusura
020300080422     c                   eval      Sndpm03 = %trimr(Sndpm03)
020400080422     c                             + '.'
020500050404     c                   exsr      ApiSndPm
020600050404e   2c                   endif
020700050404      *?Cancellazione vecchi spool?
020800050406     c                   call      'TRUL46R'
020900050404     c                   parm      PRRuser       PRPuser
021000050404     c                   parm      Qusobjnu00    PRPoutq
021100050404     c                   parm      Qusolnu00     PRPoutqlib
021200050404     c                   parm      PRRformtype   PRPformtype
021300050404     c                   parm      PRRusrdta     PRPusrdta
021400051124     c                   parm      PRRstatus     PRPstatus
021500050404     c                   parm      PRRdatpul     PRPdatpul
021600051123     c                   parm      PRRrifdtp     PRPrifdtp
021700050404     c                   parm      PRRlista      PRPlista
021800050404      *?Impostazione del prossimo elemento?
021900050404      *?           e della sua posizione iniziale?
022000050404     c                   eval      Rtvus02 = Rtvus02 + Qussee
022100050404     c                   add       1             WRKcount
022200050404      *
022300050404e   1c                   ENDDO
022400050404      *
022500050404      *?Cancellazione dello user space?
022600050404     c                   clear                   Qusec
022700050404     c                   eval      Qusbprv = %size(Qusec)
022800050404     c                   call      'QUSDLTUS'
022900050404     c                   parm      Crtus01       Dltus01
023000050404     c                   parm                    Qusec
023100050404      *
023200050404     c                   exsr      Uscita
023300050404      *
023400050404      *?S U B R O U T I N E S?----------------------------------------*
023500050404      *
023600050406      /title Controllo parametri ricevuti.
023700050404      *---------------------------------------------------------------*
023800050404      *?Controllo parametri ricevuti                                 ?*
023900050404      *---------------------------------------------------------------*
024000050404     c     CtrParms      BEGSR
024100050404      *
024200050404      * L'OUTQ è obbligatoria (può essere anche in formato generico*).
024300050404     c                   if        PRRoutq = *blanks
024400050404     c                   eval      Sndpm03 =
024500050404     c                             'ERRORE: il parametro OUTQ è stato ricevuto '
024600050404     c                             + 'vuoto. Elaborazione non eseguita.'
024700050404     c                   exsr      ApiSndPm
024800050404     c                   exsr      Uscita
024900050404     c                   endif
025000050404      * Se la libreria è vuota, assumo *LIBL.
025100050404     c                   if        PRRoutqlib = *blanks
025200050404     c                   eval      PRRoutqlib = '*LIBL'
025300050404     c                   eval      Sndpm03 =
025400050404     c                             'INFO: come libreria delle OUTQ è stata '
025500050404     c                             + 'assunta '
025600050406     c                             + %trim(PRRoutqlib)
025700050404     c                             + '.'
025800050404     c                   exsr      ApiSndPm
025900050404     c                   endif
026000050404      *
026100050404     c                   ENDSR
026200050404      *
026300050406      /title Invio messaggio programma.
026400050404      *---------------------------------------------------------------*
026500050404      *?Invio messaggio a programma                                  ?*
026600050404      *---------------------------------------------------------------*
026700050404     c     ApiSndPm      BEGSR
026800050404      *
026900050404     c                   clear                   Qusec
027000050404     c                   eval      Qusbprv = %size(qusec)
027100050404     c                   call      'QMHSNDPM'
027200050404     c                   parm                    Sndpm01
027300050404     c                   parm                    Sndpm02
027400050404     c                   parm                    Sndpm03
027500050404     c                   parm                    Sndpm04
027600050404     c                   parm                    Sndpm05
027700050404     c                   parm                    Sndpm06
027800050404     c                   parm                    Sndpm07
027900050404     c                   parm                    Sndpm08
028000050404     c                   parm                    Qusec
028100050404      *
028200050404     c                   ENDSR
028300050404      *
028400050406      /title Operazioni finali.
028500050404      *---------------------------------------------------------------*
028600050404      *?Operazioni Finali                                            ?*
028700050404      *---------------------------------------------------------------*
028800050404     c     Uscita        BEGSR
028900050404      *
029000050404     c                   eval      *inLR = *on
029100050404     c                   return
029200050404      *
029300050404     c                   ENDSR
