000400030115
000500060525      /TITLE legge yfmdi in filiale settimanalmente
000600051129     ftntud01l  uf a e           k disk
000601051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000602051129     dkpjba          e ds
000603051229     dtntudtads      e ds
000604051129     dtntu05ds       e ds
000605060525     dyfmdids        e ds                  extname(yfmdi00f)
000606051214     D wrkGetLista     S           4096
000607051214     D                                     VARYING
000608051130     d dtaiso          s               d   inz
000612060525     d dta             s                   like(mdidmi) inz
000613060525     d fgs             s                   like(mdifle)
000615051223     d comfgs          s                   like(tudfil) inz
000616051129     d conta           s                   like(tudqta)
000617051214     d mult            s                   like(tudmult)
002000030115
002041051223     **?calcolo nr. chiusre operatore
002042051223     c                   if        t05mult = ' ' or t05mult = '380'
002044051223     c                   exsr      srdatadist
002045051219     c                   end
002046051229     c* aggiorno data ultima elaborazione nella tabella TUP
002047051229     c                   clear                   tntudtads
002048051229     c                   eval      §DTAazn = kcoaz
002049051229     c                   eval      §DTAdta = t05dtf
002050051229     c                   call      'TNTUDTAR'
002051051229     c                   parm                    tntudtads
002056051214     c*
002057051214     c                   seton                                        lr
004263051222     C**************************************************************************
004264051222     C* compongo istruzione sql
004265051222     C**************************************************************************
004266051222     C     istruzione    BEGSR
004267051222     **?Imposto la parte fissa dell'istruzione.
004268051222     C                   EVAL      wrkGetLista =
004269060525     C                             'SELECT mdiFLE, mdiDMI, count(*) -
004271060525     C                             FROM yfmdi01l where -
004275060525     C                             mditmi = ''O'' and  +
004276060525     C                             mdidmi between ' +
004277051223     C                             %EDITC(t05dti:'X') + ' AND ' +
004278051223     C                             %EDITC(t05dtf:'X')
004283051222     **?Se passato p.o.
004284051222     C                   if        t05fil <> 0
004285051222     C                   EVAL      wrkGetLista = wrkGetLista +
004286060525     C                             ' AND mdifle = ' +
004287051223     C                             %TRIML(%EDITC(t05fil:'Z'))
004288051222     C                   END
004289051222     **?Ordinamento.
004290051222     C                   EVAL      wrkGetLista = wrkGetLista +
004291060525     C                             ' GROUP BY mdiFLE, mdiDMI '
004293051222     **?Solo lettura.
004294051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004295051222     C
004296051222     C/EXEC SQL
004297051222     C+ PREPARE s0 FROM :wrkGetLista
004298051222     C/END-EXEC
004299051222     C
004300051222     C/EXEC SQL
004301051222     C+ DECLARE a0 CURSOR FOR S0
004302051222     C/END-EXEC
004303051222     C
004304051222     C/EXEC SQL
004305051222     C+ OPEN a0
004306051222     C/END-EXEC
004307051222     c                   endsr
004308051222     c**********************************************************************
004309051223     c     srdatadist    begsr
004310051222     c**********************************************************************
004311051222     c                   exsr      istruzione
004312051222     c                   clear                   conta
004313051222
004314051222     C                   DO        *hival
004315051222     C/EXEC SQL
004316051223     C+ FETCH NEXT FROM a0 INTO :fgs, :dta, :conta
004317051222     C/END-EXEC
004318051222     C                   SELECT
004319051222     c*
004320051222     C                   WHEN      SqlCod = 100
004324051222     c                   leave
004325051222     C                   WHEN      SqlCod < 0
004326051222     C                   seton                                        H1
004327051222     c                   exsr      srfine
004328051222     c                   other
004329051223     **?calcolo nr. chiusure operatore
004330051223     c                   eval      mult = '380'
004331051223     c                   move      dta           dtaiso
004332051223     c                   move      fgs           comfgs
004333051223     c                   exsr      aggio
004334051222     C                   ENDSL
004335051222      *
004336051222     C                   ENDDO
004337051222     C/EXEC SQL
004338051222     C+ CLOSE a0
004339051222     C/END-EXEC
004340051222     c                   endsr
004972051214     c**********************************************************************
004973051214     c     aggio         begsr
004974051214     c**********************************************************************
004977051129     c     keytud        chain     tntud01l
004978051129     c                   eval      tudqta = conta
004979051129     c                   if        %found(tntud01l)
004980051229     c                   exsr      srstampa
004981051129     c                   update    tntud000
004982051129     c                   else
004983051214     c                   eval      tudmult = mult
004984051130     c                   eval      tuddta  = dtaiso
004985051219     c                   eval      tudfil  = comfgs
004986051129     c                   clear                   tudcli
004987051129     c                   write     tntud000
004988051129     c                   end
005000051129     c                   endsr
005001051129     c**********************************************************************
005002051129     c     *inzsr        begsr
005003051129     c**********************************************************************
005100051129     C     *ENTRY        PLIST
005200051129     C                   PARM                    kpjba
005300051129     c                   movel     kpjbu         tntu05ds
005340051129     c     keytud        klist
005341051214     c                   kfld                    mult
005342051130     c                   kfld                    dtaiso
005343051219     c                   kfld                    comfgs
005344051229     c                   seton                                        of
005400051129     c                   endsr
005500051129     C**************************************************************************
005600051129     C* esco con codice d'errore
005700051129     C**************************************************************************
005800051129     C     srfine        BEGSR
007100051129     c                   seton                                        lr
007200051129     c                   return
007300051129     c*
007400051129     c                   endsr
007500051229     C**************************************************************************
007600051229     C* stampo anomalia
007700051229     C**************************************************************************
007800051229     C     srstampa      BEGSR
007900051229     c*
008000051229     C   OF              EXCEPT    TESTA
008100051229     C   OF              SETOFF                                       OF
008200051229     C                   EXCEPT    anomalia
008300051229     c*
008400051229     c                   endsr
008500051229     OQSYSPRT   E            TESTA            02
008600051229     O                                           41 'Anomalie nel pgm TNTU14R'
008700051229     O                       PAGE          Z    132
008800051229     O          E            anomalia    1
008900051229     O                                           +0 'Si è eseguito un UPDATE pe-
009000051229     O                                              r il moltiplicatore'
009100051229     O                       tudmult             +1
009200051229     O                                           +1 'P.O.'
009300051229     O                       tudfil              +1
009400051229     O                                           +1 'Data'
009500051229     O                       tuddta              +1
