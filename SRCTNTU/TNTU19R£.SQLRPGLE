000400030115
000500051228      /TITLE legge fiatt settimanalmente
000600051129     ftntud01l  uf a e           k disk
000601051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000602051129     dkpjba          e ds
000603051229     dtntudtads      e ds
000604051129     dtntu05ds       e ds
000605051228     dfiattds        e ds                  extname(fiatt00f)
000606051214     D wrkGetLista     S           4096
000607051214     D                                     VARYING
000608051130     d dtaiso          s               d   inz
000609051228     d ddc             s                   like(attddc) inz
000610051228     d fgs             s                   like(attter) inz
000613051129     d conta           s                   like(tudqta)
000614051214     d mult            s                   like(tudmult)
000620051228     d comfgs          s                   like(attter) inz
002000030115
002033051228     **?calcolo nr. inserimenti viaggi aff/defl
002035051228     c                   if        t05mult = ' ' or t05mult = '320'
002037051228     c                   exsr      srdata320
002038051220     c                   end
002039051229     c* aggiorno data ultima elaborazione nella tabella TUP
002040051229     c                   clear                   tntudtads
002041051229     c                   eval      §DTAazn = kcoaz
002042051229     c                   eval      §DTAdta = t05dtf
002043051229     c                   call      'TNTUDTAR'
002044051229     c                   parm                    tntudtads
002047051214     c*
002048051214     c                   seton                                        lr
002049051219     C**************************************************************************
002050051219     C* compongo istruzione sql
002051051219     C**************************************************************************
002052051219     C     istruzione    BEGSR
002053051219     **?Imposto la parte fissa dell'istruzione.
002054051219     C                   EVAL      wrkGetLista =
002055060113     C                             'SELECT attter, attddc, count(*) -
002056060110     C                             FROM Fiatt03l where -
002057051228     C                             attddc BETWEEN ' +
002058051219     C                             %EDITC(t05dti:'X') + ' AND ' +
002059051219     C                             %EDITC(t05dtf:'X')
002060051219     **?Se passato p.o.
002061051219     C                   if        t05fil <> 0
002062051219     C                   EVAL      wrkGetLista = wrkGetLista +
002063060113     C                             ' AND attter = ' + %TRIML(%EDITC(t05fil:'Z'))
002064051219     C                   END
002065051228     **?Raggruppamento
002066051219     C                   EVAL      wrkGetLista = wrkGetLista +
002067060113     C                             ' GROUP BY attter, attddc'
002068051219     **?Solo lettura.
002069051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
002070051219     C
002071051219     C/EXEC SQL
002072051219     C+ PREPARE s1 FROM :wrkGetLista
002073051219     C/END-EXEC
002074051219     C
002075051219     C/EXEC SQL
002076051219     C+ DECLARE A1 CURSOR FOR S1
002077051219     C/END-EXEC
002078051219     C
002079051219     C/EXEC SQL
002080051219     C+ OPEN a1
002081051219     C/END-EXEC
002082051219     c                   endsr
002083051214     c**********************************************************************
002084051228     c     srdata320     begsr
002085051214     c**********************************************************************
002086051214     c                   exsr      istruzione
004007051214
004011051129     C                   DO        *hival
004012051129     C/EXEC SQL
004013051228     C+ FETCH NEXT FROM a1 INTO :fgs, :ddc, :conta
004014051129     C/END-EXEC
004015051129     C                   SELECT
004016051129     c*
004017051129     C                   WHEN      SqlCod = 100
004022051129     c                   leave
004023051129     C                   WHEN      SqlCod < 0
004024051129     C                   seton                                        H1
004025051129     c                   exsr      srfine
004026051129     c                   other
004027051228     c                   eval      mult = '320'
004028051228     c                   move      ddc           dtaiso
004029051228     c                   move      fgs           comfgs
004030051228     c                   exsr      aggio
004041051129     C                   ENDSL
004042051129      *
004043051129     C                   ENDDO
004044051129     C/EXEC SQL
004045051219     C+ CLOSE a1
004046051129     C/END-EXEC
004047051214     c                   endsr
005039051214     c**********************************************************************
005040051214     c     aggio         begsr
005041051214     c**********************************************************************
005042051129     c     keytud        chain     tntud01l
005043051129     c                   eval      tudqta = conta
005044051129     c                   if        %found(tntud01l)
005045051229     c                   exsr      srstampa
005046051129     c                   update    tntud000
005047051129     c                   else
005048051214     c                   eval      tudmult = mult
005049051130     c                   eval      tuddta  = dtaiso
005050051219     c                   eval      tudfil  = comfgs
005051051129     c                   clear                   tudcli
005052051129     c                   write     tntud000
005053051129     c                   end
005054051129     c                   endsr
005055051129     c**********************************************************************
005056051129     c     *inzsr        begsr
005057051129     c**********************************************************************
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
008600051229     O                                           41 'Anomalie nel pgm TNTU19R'
008700051229     O                       PAGE          Z    132
008800051229     O          E            anomalia    1
008900051229     O                                           +0 'Si è eseguito un UPDATE pe-
009000051229     O                                              r il moltiplicatore'
009100051229     O                       tudmult             +1
009200051229     O                                           +1 'P.O.'
009300051229     O                       tudfil              +1
009400051229     O                                           +1 'Data'
009500051229     O                       tuddta              +1
