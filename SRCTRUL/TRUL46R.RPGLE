000100050405      /title Cancellazione spool file da outq
000200050405      *---------------------------------------------------------------*
000300050405      *? Cancellazione spool file da outq                            ?*
000400050405      *---------------------------------------------------------------*
000500050405      *
000600161201     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050405     h dftactgrp(*no) actgrp(*caller)
000800050405      *
000900050405      *?A R C H I V I?------------------------------------------------*
001000050304      *
001100080422     fQSYSPRT   o    f  132        printer oflind(*inOF) usropn
001200050405      *
001300050405      *?C O S T A N T I?----------------------------------------------*
001400050405      *
001500050405      *
001600050405      *?S T R U T T U R E   D A T I?----------------------------------*
001700050405      *
001800050405     d Status         sds
001900050405     d  SDSproc          *proc
002000050405     d  SDSparms         *parms
002100050405     d  SDSuser              254    263
002200050405      *
002300050405      * Common Structures for List APIs to A User Space
002400050405      /copy QSYSINC/QRPGLESRC,QUSGEN
002500050405      * List Spooled Files API
002600050405      /copy QSYSINC/QRPGLESRC,QUSLSPL
002700050405      * Retrieve Spooled File Attributes API
002800050405      /copy QSYSINC/QRPGLESRC,QUSRSPLA
002900050405      * Common Structure for Error Code Parameter
003000050405      /copy QSYSINC/QRPGLESRC,QUSEC
003100050405      *
003200050405      *?V A R I A B I L I?--------------------------------------------*
003300050405      *
003400050405      * Parametri ricevuti
003500050405     d PRRuser         s             10
003600050405     d PRRoutq         s             10
003700050405     d PRRoutqlib      s             10
003800050405     d PRRformtype     s             10
003900050405     d PRRusrdta       s             10
004000051124     d PRRstatus       s              1
004100050405     d PRRdatpul       s              7
004200051123     d PRRrifdtp       s              1
004300050405     d PRRlista        s              1
004400050405      *
004500050405      * Parametri per QUSCRTUS = Create User Space
004600050405      * - qualified user space name + library
004700050405     d Crtus01         s             20    inz
004800050405      * - extended attribute
004900050405     d Crtus02         s             10    inz
005000050405      * - initial size
005100050405     d*Crtus03***      s           ***9b*0*inz(100)
005200050405     d Crtus03         s             10i 0 inz(100)
005300050405      * - initial value
005400050405     d Crtus04         s              1    inz
005500050405      * - public authority
005600050405     d Crtus05         s             10    inz('*ALL      ')
005700050405      * - text description
005800050405     d Crtus06         s             50    inz
005900050405      * - replace
006000050405     d Crtus07         s             10    inz('*YES      ')
006100050405      *
006200050405      * Parametri per QUSLSPL = List Spooled Files API
006300050405      * - qualified user space name + library
006400050405     d Lspl01          s             20    inz
006500050405      * - format name
006600050405     d Lspl02          s              8    inz('SPLF0100')
006700050405      * - user name
006800050405     d Lspl03          s             10    inz
006900050405      * - qualified output queue name
007000050405     d Lspl04          s             20    inz
007100050405      * - form type
007200050405     d Lspl05          s             10    inz
007300050405      * - user-specified data
007400050405     d Lspl06          s             10    inz
007500050405      *
007600050405      * Parametri per QUSRTVUS = Retrieve User Space
007700050405      * - user space name
007800050405     d Rtvus01         s             20    inz
007900050405      * - start position
008000050405     d*Rtvus02***      s           ***9b*0*inz(125)
008100050405     d Rtvus02         s             10i 0 inz
008200050405      * - lenght of data
008300050405     d*Rtvus03***      s           ***9b*0*inz(16)
008400050405     d Rtvus03         s             10i 0 inz
008500050405      *
008600050405      * Parametri per QUSRSPLA = Retrieve Spooled File Attributes API
008700050405      * - Length of receiver variable Input Binary(4)
008800050405     d*Rtvus11***      s           ***4b*0*inz
008900050405     d Rtvus11         s             10i 0 inz
009000050405      * - Format name Input Char(8)
009100050405     d Rtvus12         s              8    inz('SPLA0200')
009200050405      * - Qualified job name Input Char(26)
009300050405     d Rtvus13         s             26    inz('*INT      ')
009400050405      * - Internal job identifier Input Char(16)
009500050405     d Rtvus14         s             16
009600050405      * - Internal spooled file identifier Input Char(16)
009700050405     d Rtvus15         s             16
009800050405      * - Spooled file name Input Char(10)
009900050405     d Rtvus16         s             10    inz('*INT      ')
010000050405      * - Spooled file number
010100050405     d*Rtvus17***      s           ***4b*0*inz
010200050405     d Rtvus17         s             10i 0 inz
010300050405      *
010400050405      * Parametri per QUSDLTUS = Delete User Space
010500050405     d Dltus01         s             20    inz
010600050405      *
010700050406      * Parametri per TRUL46C = cancellazione spool file
010800050406     d UL46file        s             10    inz
010900050406     d UL46jobnr       s              6    inz
011000050406     d UL46user        s             10    inz
011100050406     d UL46job         s             10    inz
011200050406     d UL46filenr      s              6    inz
011300050303      *
011400050406      * Campi di comodo
011500050406     d wStatus         s                   like(Qustatus04)
011600050405     d WRKcount        s             15  0 inz
011700050307     d wTime           s              6  0 inz
011800050307     d w006            s              6  0 inz
011900050405      *
012000050405      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
012100050405      *
012200050303     c     *entry        plist
012300050405     c                   parm                    PRRuser
012400050405     c                   parm                    PRRoutq
012500050405     c                   parm                    PRRoutqlib
012600050405     c                   parm                    PRRformtype
012700050405     c                   parm                    PRRusrdta
012800051124     c                   parm                    PRRstatus
012900050405     c                   parm                    PRRdatpul
013000051123     c                   parm                    PRRrifdtp
013100050405     c                   parm                    PRRlista
013200050405      *
013300050405      *?Operazioni iniziali?
013400050405     c                   exsr      Entrata
013500050303      *
013600050405      *?Ciclo di elaborazione degli elementi?
013700050405     c                   reset                   WRKcount
013800050405do  1c                   dow       Qusnbrle > WRKcount
013900050405      *
014000050405      *?- Reperimento dati del singolo spool?
014100050405     c                   exsr      Rep_1_spool
014200040915      *
014300050405      *?- Per la cancellazione dello spool testo:?
014400051206      *?  · la data creazione oppure la data ultimo utilizzo?
014500051123     c***                if             PRRdatpul > Qusdfilo
014600051123if  2c*****              if             PRRdatpul > Qusdfla
014700051206if  2c                   if            (PRRrifdtp = 'C'
014800051124     c                             and  PRRdatpul > Qusdfilo00
014900051206     c                             or   PRRrifdtp = 'U'
015000051123     c                             and  PRRdatpul > Qusdfla)
015100050405      *?  · la stato dello spool?
015200050406     c                             and (wStatus   = *blanks
015300050406     c                              or  wStatus   = Qustatus05)
015400050405      *
015500050405      *?  => Lista (se richiesta)?
015600050405if  3c                   if             PRRlista  = '1'
015700050405     c                             or   PRRlista  = '3'
015800050304     c                   exsr      Stampa
015900050307e   3c                   endif
016000050405      *?  => Cancellazione spool (se richiesta)?
016100050405if  3c                   if             PRRlista  = '2'
016200050405     c                             or   PRRlista  = '3'
016300050405     c                   exsr      Cancella
016400050307e   3c                   endif
016500050405      *
016600050304e   2c                   endif
016700050303      *
016800050405      *?- Impostazione del prossimo elemento?
016900050405      *?             e della sua posizione iniziale?
017000050405     c                   eval      Rtvus02 = Rtvus02 + Qussee
017100050405     c                   add       1             WRKcount
017200050303      *
017300050303e   1c                   enddo
017400050405      *
017500050405      *?Fine?
017600050405     c                   exsr      Uscita
017700050405      *
017800050405      *?S U B R O U T I N E S?----------------------------------------*
017900050405      *
018000050405      *---------------------------------------------------------------*
018100050405      *?Operazioni iniziali                                          ?*
018200050405      *---------------------------------------------------------------*
018300050405     c     Entrata       BegSr
018400080422      *
018500080422if  3c                   if             PRRlista  = '1'
018600080422     c                             or   PRRlista  = '3'
018700080422     c                   open      QSYSPRT
018800080422e   3c                   endif
018900050405      *
019000050405      *?Impostazione dei default?
019100050405if  1c                   if        PRRuser     = *blanks
019200050405     c                   eval      PRRuser     = '*ALL'
019300050405e   1c                   endif
019400050405if  1c                   if        PRRoutq     = *blanks
019500050405     c                   eval      PRRoutq     = '*ALL'
019600050405x   1c                   else
019700050405if  2c                   if        PRRoutqlib  = *blanks
019800050405     c                   eval      PRRoutqlib  = '*LIBL'
019900050405e   2c                   endif
020000050405e   1c                   endif
020100050405if  1c                   if        PRRformtype = *blanks
020200050405     c                   eval      PRRformtype = '*ALL'
020300050405e   1c                   endif
020400050405if  1c                   if        PRRdatpul   = *blanks
020500050405     c                   eval      PRRdatpul   = '*ALL'
020600050405e   1c                   endif
020700051123if  1c                   if        PRRrifdtp   = *blanks
020800051123     c                   eval      PRRrifdtp   = '2'
020900051123e   1c                   endif
021000050405if  1c                   if        PRRlista    = *blanks
021100050405     c                   eval      PRRlista    = '1'
021200050405e   1c                   endif
021300050405      *
021400050405      *   Lista: 1 = solo lista
021500050405      *          2 = solo cancellazione
021600050405      *          3 = lista e cancellazione
021700050405     c                   eval      *in41 = (PRRlista = '1')
021800050405      *
021900050405      * PRRstatus contiene un flag che indica lo stato della stampa
022000050405      *   da cancellare e può valere:
022100050405      *   - " " => TUTTE
022200050405      *   - "R" => *READY
022300050405      *   - "S" => *SAVED
022400050405      *   - "H" => *HELD
022500050405sel 1c                   select
022600050405w   1c                   when      PRRstatus = 'R'
022700050406     c                   movel     '*READY'      wStatus
022800050405w   1c                   when      PRRstatus = 'S'
022900050406     c                   movel     '*SAVED'      wStatus
023000050405w   1c                   when      PRRstatus = 'H'
023100050406     c                   movel     '*HELD'       wStatus
023200050405x   1c                   other
023300050406     c                   clear                   wStatus
023400050405e   1c                   endsl
023500050405      *
023600050405      *?Creazione dello user space?
023700050405     c                   eval      Crtus01 = SDSproc + 'QTEMP'
023800050405     c                   call      'QUSCRTUS'                           21
023900050405     c                   parm                    Crtus01
024000050405     c                   parm                    Crtus02
024100050406     c                   parm      256           Crtus03
024200050405     c                   parm      *blanks       Crtus04
024300050405     c                   parm      '*ALL   '     Crtus05
024400050405     c                   parm                    Crtus06
024500050405     c                   parm      '*YES'        Crtus07
024600050405     c                   parm                    Qusec
024700050405      *
024800050405      *?Riempimento dello user space con la lista degli spool file?
024900050405     c                   clear                   Qusec
025000050405     c                   eval      Lspl04 = PRRoutq + PRRoutqlib
025100050405     c                   call      'QUSLSPL'
025200050405     c                   parm      Crtus01       Lspl01
025300050405     c                   parm      'SPLF0100'    Lspl02
025400050405     c                   parm      PRRuser       Lspl03
025500050405     c                   parm                    Lspl04
025600050405     c                   parm      PRRformtype   Lspl05
025700050406     c                   parm      PRRusrdta     Lspl06
025800050405     c                   parm                    Qusec
025900050405      *
026000050405      *?Reperimento del numero di elementi ?
026100050405      *?          e della lunghezza di un elemento?
026200050405      * «Common Structures for List APIs to A User Space (QUSGEN)»
026300050405     c                   clear                   Qusec
026400050405     c                   eval      Qusbprv = %size(Qusec)
026500050405     c                   eval      Rtvus03 = %size(Qush0100)
026600050405     c                   call      'QUSRTVUS'
026700050405     c                   parm      Crtus01       Rtvus01
026800050405     c                   parm      1             Rtvus02
026900050405     c                   parm                    Rtvus03
027000050405     c                   parm                    Qush0100
027100050405     c                   parm                    Qusec
027200050405      *
027300050405      *?Impostazione della posizione iniziale del primo elemento?
027400050405      *?           e della lunghezza?
027500050405     c                   eval      Rtvus02 = Qusold + 1
027600050405     c                   eval      Rtvus03 = Qussee
027700050405      *
027800050405     c                   EndSr
027900050405      *
028000050405      *---------------------------------------------------------------*
028100050405      *?Reperimento dati del singolo spool                           ?*
028200050405      *---------------------------------------------------------------*
028300050405     c     Rep_1_spool   BegSr
028400050405      *
028500050405      *?- Reperimento dati del singlo elemento?
028600050405     c                   clear                   Qusec
028700050405     c                   eval      Qusbprv = %size(Qusec)
028800050405     c                   call      'QUSRTVUS'
028900050405     c                   parm      Crtus01       Rtvus01
029000050405     c                   parm                    Rtvus02
029100050405     c                   parm                    Rtvus03
029200050405     c                   parm                    Qusf0100
029300050405     c                   parm                    Qusec
029400050405      *
029500050405      *?- Reperimento degli attributi?
029600050405     c                   eval      Rtvus11 = %size(Qusa0200)
029700050405     c*** da QUSF0100:   eval      Rtvus14 = Qusijid07
029800050405     c*** da QUSF0100:   eval      Rtvus15 = Qusisid
029900050405     c                   call      'QUSRSPLA'                           21
030000050405     c                   parm                    Qusa0200
030100050405     c                   parm                    Rtvus11
030200050405     c                   parm      'SPLA0200'    Rtvus12
030300050405     c                   parm      '*INT'        Rtvus13
030400050405     c                   parm      Qusijid07     Rtvus14
030500050405     c                   parm      Qusisid       Rtvus15
030600050405     c                   parm      '*INT'        Rtvus16
030700050405     c                   parm      *zeros        Rtvus17
030800050405      *
030900050405     c                   EndSr
031000050405      *
031100050405      *---------------------------------------------------------------*
031200050405      *?Stampa elenco spool cancellati / cancellabili                ?*
031300050405      *---------------------------------------------------------------*
031400050304     c     Stampa        BegSr
031500050304      *
031600050304     c                   if           Page  = *zeros
031700050304     c                             or *inOF = *on
031800050307     c                   time                    wTime
031900050304     c                   except    PRTtxt
032000050304     c                   eval      *inOF = *off
032100050304     c                   endif
032200050304      *
032300050304     c                   except    PRTdet
032400050304      *
032500050304     c                   EndSr
032600050405      *
032700050405      *---------------------------------------------------------------*
032800050405      *?Cancellazione del singolo spool file                         ?*
032900050405      *---------------------------------------------------------------*
033000050405     c     Cancella      BegSr
033100050405      *
033200050405     c                   z-add     Qussnbr00     w006
033300050406     c                   move      w006          UL46filenr
033400050406     c                   call      'TRUL46C'
033500050406     c                   parm      Qussn02       UL46file         10
033600050406     c                   parm      Qusjnbr10     UL46jobnr         6
033700050406     c                   parm      Qusun13       UL46user         10
033800050406     c                   parm      Qusjn11       UL46job          10
033900050406     c                   parm                    UL46filenr        6
034000050405      *
034100050405     c                   EndSr
034200050405      *
034300050405      *---------------------------------------------------------------*
034400050405      *?Operazioni Finali                                            ?*
034500050405      *---------------------------------------------------------------*
034600050405     c     Uscita        BegSr
034700050405      *
034800050405      *?Chiusura della stampa?
034900080422if  1c                   if             PRRlista  = '1'
035000080422     c                             or   PRRlista  = '3'
035100080422if  2c                   if        Page > *zeros
035200050405     c                   except    PRTend
035300080422e   2c                   endif
035400080422     c                   close     QSYSPRT
035500080422e   1c                   endif
035600050405      *
035700050405      *?Cancellazione dello user space?
035800050405     c                   clear                   Qusec
035900050405     c                   eval      Qusbprv = %size(Qusec)
036000050405     c                   call      'QUSDLTUS'                           90
036100050405     c                   parm      Crtus01       Dltus01
036200050405     c                   parm                    Qusec
036300050405      *
036400050405     c                   eval      *inLR = *on
036500050405     c                   return
036600050405      *
036700050405     c                   EndSr
036800050304      *
036900050405      *?L I S T E   D I   O U T P U T?--------------------------------*
037000050405      *
037100050304     oQSYSPRT   E            PRTtxt           02
037200050405     o                       SDSuser
037300050405     o                       SDSproc           +  5
037400050304     o                       *date             +  5 '  /  /    '
037500050307     o                       wTime             +  5 '  :  :  '
037600050304     o                                         +  5 'ELENCO STAMPE'
037700050307     o              N41                        +  1 'CANCELLATE'
037800050307     o               41                          85 'CANCELLABILI'
037900050304     o                                         +  5 'Pag.'
038000050304     o                       Page          Z   +  0
038100050304     o          e            PRTtxt      2
038200050304     o                                              'File      '
038300050304     o                                         +  2 'Nome job  '
038400050304     o                                         +  2 'Utente    '
038500050304     o                                         +  2 'Nr.job'
038600050304     o                                         +  2 'Coda      '
038700050304     o                                         +  2 'Libreria  '
038800050304     o                                         +  2 'Nr. spool'
038900050304     o                                         +  2 'DatiUtente'
039000050304     o                                         +  2 'DtCreaz'
039100050304     o                                         +  2 'DtUltUt'
039200050304     o                                         +  2 'Status    '
039300050309     o                                         +  2 'Nr.pagine'
039400050304     o          E            PRTTXT      1  1
039500050304     o                                              '----------'
039600050304     o                                         +  2 '----------'
039700050304     o                                         +  2 '----------'
039800050304     o                                         +  2 '------'
039900050304     o                                         +  2 '----------'
040000050304     o                                         +  2 '----------'
040100050304     o                                         +  2 '---------'
040200050304     o                                         +  2 '----------'
040300050304     o                                         +  2 '-------'
040400050304     o                                         +  2 '-------'
040500050304     o                                         +  2 '----------'
040600050309     o                                         +  2 '---------'
040700050304     o          E            PRTDET      1
040800050304     o                       QUSSN02
040900050304     o                       QUSJN11           +  2
041000050304     o                       QUSUN13           +  2
041100050304     o                       QUSJNBR10         +  2
041200050304     o                       QUSON01           +  2
041300050304     o                       QUSOL01           +  2
041400050309     o                       QUSSNBR00     z   +  2
041500050304     o                       QUSUD02           +  2
041600050304     o                       QUSDFILO00        +  2
041700050304     o                       QUSDFLA           +  2
041800050304     o                       QUSTATUS05        +  2
041900050309     o                       QUSTP00       z   +  2
042000050304     o          E            PRTEND      2
042100050304     o                                         + 15 '***  Fine Lista  ***'
