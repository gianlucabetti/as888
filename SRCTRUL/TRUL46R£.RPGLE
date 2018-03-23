000100050405      /title Cancellazione spool file da outq
000200050405      *---------------------------------------------------------------*
000300050405      *? Cancellazione spool file da outq                            ?*
000400050405      *---------------------------------------------------------------*
000500050405      *
000600050405     h dftactgrp(*no) actgrp(*caller)
000700050405      *
000800050405      *?A R C H I V I?------------------------------------------------*
000900050304      *
001000080422     fQSYSPRT   o    f  132        printer oflind(*inOF) usropn
001100050405      *
001200050405      *?C O S T A N T I?----------------------------------------------*
001300050405      *
001400050405      *
001500050405      *?S T R U T T U R E   D A T I?----------------------------------*
001600050405      *
001700050405     d Status         sds
001800050405     d  SDSproc          *proc
001900050405     d  SDSparms         *parms
002000050405     d  SDSuser              254    263
002100050405      *
002200050405      * Common Structures for List APIs to A User Space
002300050405      /copy QSYSINC/QRPGLESRC,QUSGEN
002400050405      * List Spooled Files API
002500050405      /copy QSYSINC/QRPGLESRC,QUSLSPL
002600050405      * Retrieve Spooled File Attributes API
002700050405      /copy QSYSINC/QRPGLESRC,QUSRSPLA
002800050405      * Common Structure for Error Code Parameter
002900050405      /copy QSYSINC/QRPGLESRC,QUSEC
003000050405      *
003100050405      *?V A R I A B I L I?--------------------------------------------*
003200050405      *
003300050405      * Parametri ricevuti
003400050405     d PRRuser         s             10
003500050405     d PRRoutq         s             10
003600050405     d PRRoutqlib      s             10
003700050405     d PRRformtype     s             10
003800050405     d PRRusrdta       s             10
003900051124     d PRRstatus       s              1
004000050405     d PRRdatpul       s              7
004100051123     d PRRrifdtp       s              1
004200050405     d PRRlista        s              1
004300050405      *
004400050405      * Parametri per QUSCRTUS = Create User Space
004500050405      * - qualified user space name + library
004600050405     d Crtus01         s             20    inz
004700050405      * - extended attribute
004800050405     d Crtus02         s             10    inz
004900050405      * - initial size
005000050405     d*Crtus03***      s           ***9b*0*inz(100)
005100050405     d Crtus03         s             10i 0 inz(100)
005200050405      * - initial value
005300050405     d Crtus04         s              1    inz
005400050405      * - public authority
005500050405     d Crtus05         s             10    inz('*ALL      ')
005600050405      * - text description
005700050405     d Crtus06         s             50    inz
005800050405      * - replace
005900050405     d Crtus07         s             10    inz('*YES      ')
006000050405      *
006100050405      * Parametri per QUSLSPL = List Spooled Files API
006200050405      * - qualified user space name + library
006300050405     d Lspl01          s             20    inz
006400050405      * - format name
006500050405     d Lspl02          s              8    inz('SPLF0100')
006600050405      * - user name
006700050405     d Lspl03          s             10    inz
006800050405      * - qualified output queue name
006900050405     d Lspl04          s             20    inz
007000050405      * - form type
007100050405     d Lspl05          s             10    inz
007200050405      * - user-specified data
007300050405     d Lspl06          s             10    inz
007400050405      *
007500050405      * Parametri per QUSRTVUS = Retrieve User Space
007600050405      * - user space name
007700050405     d Rtvus01         s             20    inz
007800050405      * - start position
007900050405     d*Rtvus02***      s           ***9b*0*inz(125)
008000050405     d Rtvus02         s             10i 0 inz
008100050405      * - lenght of data
008200050405     d*Rtvus03***      s           ***9b*0*inz(16)
008300050405     d Rtvus03         s             10i 0 inz
008400050405      *
008500050405      * Parametri per QUSRSPLA = Retrieve Spooled File Attributes API
008600050405      * - Length of receiver variable Input Binary(4)
008700050405     d*Rtvus11***      s           ***4b*0*inz
008800050405     d Rtvus11         s             10i 0 inz
008900050405      * - Format name Input Char(8)
009000050405     d Rtvus12         s              8    inz('SPLA0200')
009100050405      * - Qualified job name Input Char(26)
009200050405     d Rtvus13         s             26    inz('*INT      ')
009300050405      * - Internal job identifier Input Char(16)
009400050405     d Rtvus14         s             16
009500050405      * - Internal spooled file identifier Input Char(16)
009600050405     d Rtvus15         s             16
009700050405      * - Spooled file name Input Char(10)
009800050405     d Rtvus16         s             10    inz('*INT      ')
009900050405      * - Spooled file number
010000050405     d*Rtvus17***      s           ***4b*0*inz
010100050405     d Rtvus17         s             10i 0 inz
010200050405      *
010300050405      * Parametri per QUSDLTUS = Delete User Space
010400050405     d Dltus01         s             20    inz
010500050405      *
010600050406      * Parametri per TRUL46C = cancellazione spool file
010700050406     d UL46file        s             10    inz
010800050406     d UL46jobnr       s              6    inz
010900050406     d UL46user        s             10    inz
011000050406     d UL46job         s             10    inz
011100050406     d UL46filenr      s              6    inz
011200050303      *
011300050406      * Campi di comodo
011400050406     d wStatus         s                   like(Qustatus04)
011500050405     d WRKcount        s             15  0 inz
011600050307     d wTime           s              6  0 inz
011700050307     d w006            s              6  0 inz
011800050405      *
011900050405      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
012000050405      *
012100050303     c     *entry        plist
012200050405     c                   parm                    PRRuser
012300050405     c                   parm                    PRRoutq
012400050405     c                   parm                    PRRoutqlib
012500050405     c                   parm                    PRRformtype
012600050405     c                   parm                    PRRusrdta
012700051124     c                   parm                    PRRstatus
012800050405     c                   parm                    PRRdatpul
012900051123     c                   parm                    PRRrifdtp
013000050405     c                   parm                    PRRlista
013100050405      *
013200050405      *?Operazioni iniziali?
013300050405     c                   exsr      Entrata
013400050303      *
013500050405      *?Ciclo di elaborazione degli elementi?
013600050405     c                   reset                   WRKcount
013700050405do  1c                   dow       Qusnbrle > WRKcount
013800050405      *
013900050405      *?- Reperimento dati del singolo spool?
014000050405     c                   exsr      Rep_1_spool
014100040915      *
014200050405      *?- Per la cancellazione dello spool testo:?
014300051206      *?  · la data creazione oppure la data ultimo utilizzo?
014400051123     c***                if             PRRdatpul > Qusdfilo
014500051123if  2c*****              if             PRRdatpul > Qusdfla
014600051206if  2c                   if            (PRRrifdtp = 'C'
014700051124     c                             and  PRRdatpul > Qusdfilo00
014800051206     c                             or   PRRrifdtp = 'U'
014900051123     c                             and  PRRdatpul > Qusdfla)
015000050405      *?  · la stato dello spool?
015100050406     c                             and (wStatus   = *blanks
015200050406     c                              or  wStatus   = Qustatus05)
015300050405      *
015400050405      *?  => Lista (se richiesta)?
015500050405if  3c                   if             PRRlista  = '1'
015600050405     c                             or   PRRlista  = '3'
015700050304     c                   exsr      Stampa
015800050307e   3c                   endif
015900050405      *?  => Cancellazione spool (se richiesta)?
016000050405if  3c                   if             PRRlista  = '2'
016100050405     c                             or   PRRlista  = '3'
016200050405     c                   exsr      Cancella
016300050307e   3c                   endif
016400050405      *
016500050304e   2c                   endif
016600050303      *
016700050405      *?- Impostazione del prossimo elemento?
016800050405      *?             e della sua posizione iniziale?
016900050405     c                   eval      Rtvus02 = Rtvus02 + Qussee
017000050405     c                   add       1             WRKcount
017100050303      *
017200050303e   1c                   enddo
017300050405      *
017400050405      *?Fine?
017500050405     c                   exsr      Uscita
017600050405      *
017700050405      *?S U B R O U T I N E S?----------------------------------------*
017800050405      *
017900050405      *---------------------------------------------------------------*
018000050405      *?Operazioni iniziali                                          ?*
018100050405      *---------------------------------------------------------------*
018200050405     c     Entrata       BegSr
018300080422      *
018400080422if  3c                   if             PRRlista  = '1'
018500080422     c                             or   PRRlista  = '3'
018600080422     c                   open      QSYSPRT
018700080422e   3c                   endif
018800050405      *
018900050405      *?Impostazione dei default?
019000050405if  1c                   if        PRRuser     = *blanks
019100050405     c                   eval      PRRuser     = '*ALL'
019200050405e   1c                   endif
019300050405if  1c                   if        PRRoutq     = *blanks
019400050405     c                   eval      PRRoutq     = '*ALL'
019500050405x   1c                   else
019600050405if  2c                   if        PRRoutqlib  = *blanks
019700050405     c                   eval      PRRoutqlib  = '*LIBL'
019800050405e   2c                   endif
019900050405e   1c                   endif
020000050405if  1c                   if        PRRformtype = *blanks
020100050405     c                   eval      PRRformtype = '*ALL'
020200050405e   1c                   endif
020300050405if  1c                   if        PRRdatpul   = *blanks
020400050405     c                   eval      PRRdatpul   = '*ALL'
020500050405e   1c                   endif
020600051123if  1c                   if        PRRrifdtp   = *blanks
020700051123     c                   eval      PRRrifdtp   = '2'
020800051123e   1c                   endif
020900050405if  1c                   if        PRRlista    = *blanks
021000050405     c                   eval      PRRlista    = '1'
021100050405e   1c                   endif
021200050405      *
021300050405      *   Lista: 1 = solo lista
021400050405      *          2 = solo cancellazione
021500050405      *          3 = lista e cancellazione
021600050405     c                   eval      *in41 = (PRRlista = '1')
021700050405      *
021800050405      * PRRstatus contiene un flag che indica lo stato della stampa
021900050405      *   da cancellare e può valere:
022000050405      *   - " " => TUTTE
022100050405      *   - "R" => *READY
022200050405      *   - "S" => *SAVED
022300050405      *   - "H" => *HELD
022400050405sel 1c                   select
022500050405w   1c                   when      PRRstatus = 'R'
022600050406     c                   movel     '*READY'      wStatus
022700050405w   1c                   when      PRRstatus = 'S'
022800050406     c                   movel     '*SAVED'      wStatus
022900050405w   1c                   when      PRRstatus = 'H'
023000050406     c                   movel     '*HELD'       wStatus
023100050405x   1c                   other
023200050406     c                   clear                   wStatus
023300050405e   1c                   endsl
023400050405      *
023500050405      *?Creazione dello user space?
023600050405     c                   eval      Crtus01 = SDSproc + 'QTEMP'
023700050405     c                   call      'QUSCRTUS'                           21
023800050405     c                   parm                    Crtus01
023900050405     c                   parm                    Crtus02
024000050406     c                   parm      256           Crtus03
024100050405     c                   parm      *blanks       Crtus04
024200050405     c                   parm      '*ALL   '     Crtus05
024300050405     c                   parm                    Crtus06
024400050405     c                   parm      '*YES'        Crtus07
024500050405     c                   parm                    Qusec
024600050405      *
024700050405      *?Riempimento dello user space con la lista degli spool file?
024800050405     c                   clear                   Qusec
024900050405     c                   eval      Lspl04 = PRRoutq + PRRoutqlib
025000050405     c                   call      'QUSLSPL'
025100050405     c                   parm      Crtus01       Lspl01
025200050405     c                   parm      'SPLF0100'    Lspl02
025300050405     c                   parm      PRRuser       Lspl03
025400050405     c                   parm                    Lspl04
025500050405     c                   parm      PRRformtype   Lspl05
025600050406     c                   parm      PRRusrdta     Lspl06
025700050405     c                   parm                    Qusec
025800050405      *
025900050405      *?Reperimento del numero di elementi ?
026000050405      *?          e della lunghezza di un elemento?
026100050405      * «Common Structures for List APIs to A User Space (QUSGEN)»
026200050405     c                   clear                   Qusec
026300050405     c                   eval      Qusbprv = %size(Qusec)
026400050405     c                   eval      Rtvus03 = %size(Qush0100)
026500050405     c                   call      'QUSRTVUS'
026600050405     c                   parm      Crtus01       Rtvus01
026700050405     c                   parm      1             Rtvus02
026800050405     c                   parm                    Rtvus03
026900050405     c                   parm                    Qush0100
027000050405     c                   parm                    Qusec
027100050405      *
027200050405      *?Impostazione della posizione iniziale del primo elemento?
027300050405      *?           e della lunghezza?
027400050405     c                   eval      Rtvus02 = Qusold + 1
027500050405     c                   eval      Rtvus03 = Qussee
027600050405      *
027700050405     c                   EndSr
027800050405      *
027900050405      *---------------------------------------------------------------*
028000050405      *?Reperimento dati del singolo spool                           ?*
028100050405      *---------------------------------------------------------------*
028200050405     c     Rep_1_spool   BegSr
028300050405      *
028400050405      *?- Reperimento dati del singlo elemento?
028500050405     c                   clear                   Qusec
028600050405     c                   eval      Qusbprv = %size(Qusec)
028700050405     c                   call      'QUSRTVUS'
028800050405     c                   parm      Crtus01       Rtvus01
028900050405     c                   parm                    Rtvus02
029000050405     c                   parm                    Rtvus03
029100050405     c                   parm                    Qusf0100
029200050405     c                   parm                    Qusec
029300050405      *
029400050405      *?- Reperimento degli attributi?
029500050405     c                   eval      Rtvus11 = %size(Qusa0200)
029600050405     c*** da QUSF0100:   eval      Rtvus14 = Qusijid07
029700050405     c*** da QUSF0100:   eval      Rtvus15 = Qusisid
029800050405     c                   call      'QUSRSPLA'                           21
029900050405     c                   parm                    Qusa0200
030000050405     c                   parm                    Rtvus11
030100050405     c                   parm      'SPLA0200'    Rtvus12
030200050405     c                   parm      '*INT'        Rtvus13
030300050405     c                   parm      Qusijid07     Rtvus14
030400050405     c                   parm      Qusisid       Rtvus15
030500050405     c                   parm      '*INT'        Rtvus16
030600050405     c                   parm      *zeros        Rtvus17
030700050405      *
030800050405     c                   EndSr
030900050405      *
031000050405      *---------------------------------------------------------------*
031100050405      *?Stampa elenco spool cancellati / cancellabili                ?*
031200050405      *---------------------------------------------------------------*
031300050304     c     Stampa        BegSr
031400050304      *
031500050304     c                   if           Page  = *zeros
031600050304     c                             or *inOF = *on
031700050307     c                   time                    wTime
031800050304     c                   except    PRTtxt
031900050304     c                   eval      *inOF = *off
032000050304     c                   endif
032100050304      *
032200050304     c                   except    PRTdet
032300050304      *
032400050304     c                   EndSr
032500050405      *
032600050405      *---------------------------------------------------------------*
032700050405      *?Cancellazione del singolo spool file                         ?*
032800050405      *---------------------------------------------------------------*
032900050405     c     Cancella      BegSr
033000050405      *
033100050405     c                   z-add     Qussnbr00     w006
033200050406     c                   move      w006          UL46filenr
033300050406     c                   call      'TRUL46C'
033400050406     c                   parm      Qussn02       UL46file         10
033500050406     c                   parm      Qusjnbr10     UL46jobnr         6
033600050406     c                   parm      Qusun13       UL46user         10
033700050406     c                   parm      Qusjn11       UL46job          10
033800050406     c                   parm                    UL46filenr        6
033900050405      *
034000050405     c                   EndSr
034100050405      *
034200050405      *---------------------------------------------------------------*
034300050405      *?Operazioni Finali                                            ?*
034400050405      *---------------------------------------------------------------*
034500050405     c     Uscita        BegSr
034600050405      *
034700050405      *?Chiusura della stampa?
034800080422if  1c                   if             PRRlista  = '1'
034900080422     c                             or   PRRlista  = '3'
035000080422if  2c                   if        Page > *zeros
035100050405     c                   except    PRTend
035200080422e   2c                   endif
035300080422     c                   close     QSYSPRT
035400080422e   1c                   endif
035500050405      *
035600050405      *?Cancellazione dello user space?
035700050405     c                   clear                   Qusec
035800050405     c                   eval      Qusbprv = %size(Qusec)
035900050405     c                   call      'QUSDLTUS'                           90
036000050405     c                   parm      Crtus01       Dltus01
036100050405     c                   parm                    Qusec
036200050405      *
036300050405     c                   eval      *inLR = *on
036400050405     c                   return
036500050405      *
036600050405     c                   EndSr
036700050304      *
036800050405      *?L I S T E   D I   O U T P U T?--------------------------------*
036900050405      *
037000050304     oQSYSPRT   E            PRTtxt           02
037100050405     o                       SDSuser
037200050405     o                       SDSproc           +  5
037300050304     o                       *date             +  5 '  /  /    '
037400050307     o                       wTime             +  5 '  :  :  '
037500050304     o                                         +  5 'ELENCO STAMPE'
037600050307     o              N41                        +  1 'CANCELLATE'
037700050307     o               41                          85 'CANCELLABILI'
037800050304     o                                         +  5 'Pag.'
037900050304     o                       Page          Z   +  0
038000050304     o          e            PRTtxt      2
038100050304     o                                              'File      '
038200050304     o                                         +  2 'Nome job  '
038300050304     o                                         +  2 'Utente    '
038400050304     o                                         +  2 'Nr.job'
038500050304     o                                         +  2 'Coda      '
038600050304     o                                         +  2 'Libreria  '
038700050304     o                                         +  2 'Nr. spool'
038800050304     o                                         +  2 'DatiUtente'
038900050304     o                                         +  2 'DtCreaz'
039000050304     o                                         +  2 'DtUltUt'
039100050304     o                                         +  2 'Status    '
039200050309     o                                         +  2 'Nr.pagine'
039300050304     o          E            PRTTXT      1  1
039400050304     o                                              '----------'
039500050304     o                                         +  2 '----------'
039600050304     o                                         +  2 '----------'
039700050304     o                                         +  2 '------'
039800050304     o                                         +  2 '----------'
039900050304     o                                         +  2 '----------'
040000050304     o                                         +  2 '---------'
040100050304     o                                         +  2 '----------'
040200050304     o                                         +  2 '-------'
040300050304     o                                         +  2 '-------'
040400050304     o                                         +  2 '----------'
040500050309     o                                         +  2 '---------'
040600050304     o          E            PRTDET      1
040700050304     o                       QUSSN02
040800050304     o                       QUSJN11           +  2
040900050304     o                       QUSUN13           +  2
041000050304     o                       QUSJNBR10         +  2
041100050304     o                       QUSON01           +  2
041200050304     o                       QUSOL01           +  2
041300050309     o                       QUSSNBR00     z   +  2
041400050304     o                       QUSUD02           +  2
041500050304     o                       QUSDFILO00        +  2
041600050304     o                       QUSDFLA           +  2
041700050304     o                       QUSTATUS05        +  2
041800050309     o                       QUSTP00       z   +  2
041900050304     o          E            PRTEND      2
042000050304     o                                         + 15 '***  Fine Lista  ***'
