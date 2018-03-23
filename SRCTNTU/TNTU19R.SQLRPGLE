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
002039061114     **?calcolo nr. inserimenti viaggi affluenza
002040061114     c                   if        t05mult = ' ' or t05mult = '321'
002041061114     c                   exsr      srdata321
002042061114     c                   end
002043061114     **?calcolo nr. inserimenti viaggi defluenza
002044061114     c                   if        t05mult = ' ' or t05mult = '322'
002045061114     c                   exsr      srdata322
002046061114     c                   end
002047051229     c* aggiorno data ultima elaborazione nella tabella TUP
002048051229     c                   clear                   tntudtads
002049051229     c                   eval      §DTAazn = kcoaz
002050051229     c                   eval      §DTAdta = t05dtf
002051051229     c                   call      'TNTUDTAR'
002052051229     c                   parm                    tntudtads
002053051214     c*
002054051214     c                   seton                                        lr
002055051219     C**************************************************************************
002056061114     C* compongo istruzione sql x aff/defl
002057051219     C**************************************************************************
002058051219     C     istruzione    BEGSR
002059051219     **?Imposto la parte fissa dell'istruzione.
002060051219     C                   EVAL      wrkGetLista =
002061060113     C                             'SELECT attter, attddc, count(*) -
002062060110     C                             FROM Fiatt03l where -
002063051228     C                             attddc BETWEEN ' +
002064051219     C                             %EDITC(t05dti:'X') + ' AND ' +
002065051219     C                             %EDITC(t05dtf:'X')
002066051219     **?Se passato p.o.
002067051219     C                   if        t05fil <> 0
002068051219     C                   EVAL      wrkGetLista = wrkGetLista +
002069060113     C                             ' AND attter = ' + %TRIML(%EDITC(t05fil:'Z'))
002070051219     C                   END
002071051228     **?Raggruppamento
002072051219     C                   EVAL      wrkGetLista = wrkGetLista +
002073060113     C                             ' GROUP BY attter, attddc'
002074051219     **?Solo lettura.
002075051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
002076051219     C
002077051219     C/EXEC SQL
002078051219     C+ PREPARE s1 FROM :wrkGetLista
002079051219     C/END-EXEC
002080051219     C
002081051219     C/EXEC SQL
002082051219     C+ DECLARE A1 CURSOR FOR S1
002083051219     C/END-EXEC
002084051219     C
002085051219     C/EXEC SQL
002086051219     C+ OPEN a1
002087051219     C/END-EXEC
002088051219     c                   endsr
002089051214     c**********************************************************************
002090051228     c     srdata320     begsr
002091051214     c**********************************************************************
002092051214     c                   exsr      istruzione
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
004048061114     C**************************************************************************
004049061114     C* compongo istruzione sql x affluenza
004050061114     C**************************************************************************
004051061114     C     istruzione1   BEGSR
004052061114     **?Imposto la parte fissa dell'istruzione.
004053061114     C                   EVAL      wrkGetLista =
004054061114     C                             'SELECT attfga, attddc, count(*) -
004055061114     C                             FROM Fiatt03l where -
004056061114     C                             attddc BETWEEN ' +
004057061114     C                             %EDITC(t05dti:'X') + ' AND ' +
004058061114     C                             %EDITC(t05dtf:'X') + ' AND -
004059061114     C                             attvad = ''A'''
004060061114     **?Se passato p.o.
004061061114     C                   if        t05fil <> 0
004062061114     C                   EVAL      wrkGetLista = wrkGetLista +
004063061114     C                             ' AND attfga = ' + %TRIML(%EDITC(t05fil:'Z'))
004064061114     C                   END
004065061114     **?Raggruppamento
004066061114     C                   EVAL      wrkGetLista = wrkGetLista +
004067061114     C                             ' GROUP BY attfga, attddc'
004068061114     **?Solo lettura.
004069061114     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004070061114     C
004071061114     C/EXEC SQL
004072061114     C+ PREPARE x1 FROM :wrkGetLista
004073061114     C/END-EXEC
004074061114     C
004075061114     C/EXEC SQL
004076061114     C+ DECLARE b1 CURSOR FOR x1
004077061114     C/END-EXEC
004078061114     C
004079061114     C/EXEC SQL
004080061114     C+ OPEN b1
004081061114     C/END-EXEC
004082061114     c                   endsr
004083061114     c**********************************************************************
004084061114     c     srdata321     begsr
004085061114     c**********************************************************************
004086061114     c                   exsr      istruzione1
004087061114
004088061114     C                   DO        *hival
004089061114     C/EXEC SQL
004090061114     C+ FETCH NEXT FROM b1 INTO :fgs, :ddc, :conta
004091061114     C/END-EXEC
004092061114     C                   SELECT
004093061114     c*
004094061114     C                   WHEN      SqlCod = 100
004095061114     c                   leave
004096061114     C                   WHEN      SqlCod < 0
004097061114     C                   seton                                        H1
004098061114     c                   exsr      srfine
004099061114     c                   other
004100061114     c                   eval      mult = '321'
004101061114     c                   move      ddc           dtaiso
004102061114     c                   move      fgs           comfgs
004103061114     c                   exsr      aggio
004104061114     C                   ENDSL
004105061114      *
004106061114     C                   ENDDO
004107061114     C/EXEC SQL
004108061114     C+ CLOSE b1
004109061114     C/END-EXEC
004110061114     c                   endsr
004111061114     C**************************************************************************
004112061114     C* compongo istruzione sql x defluenza
004113061114     C**************************************************************************
004114061114     C     istruzione2   BEGSR
004115061114     **?Imposto la parte fissa dell'istruzione.
004116061114     C                   EVAL      wrkGetLista =
004117061114     C                             'SELECT attfgp, attddc, count(*) -
004118061114     C                             FROM Fiatt03l where -
004119061114     C                             attddc BETWEEN ' +
004120061114     C                             %EDITC(t05dti:'X') + ' AND ' +
004122061114     C                             %EDITC(t05dtf:'X') + ' AND -
004123061114     C                             attvad = ''D'''
004124061114     **?Se passato p.o.
004125061114     C                   if        t05fil <> 0
004126061114     C                   EVAL      wrkGetLista = wrkGetLista +
004127061114     C                             ' AND attfgp = ' + %TRIML(%EDITC(t05fil:'Z'))
004128061114     C                   END
004129061114     **?Raggruppamento
004130061114     C                   EVAL      wrkGetLista = wrkGetLista +
004131061114     C                             ' GROUP BY attfgp, attddc'
004132061114     **?Solo lettura.
004133061114     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004134061114     C
004135061114     C/EXEC SQL
004136061114     C+ PREPARE y1 FROM :wrkGetLista
004137061114     C/END-EXEC
004138061114     C
004139061114     C/EXEC SQL
004140061114     C+ DECLARE c1 CURSOR FOR y1
004141061114     C/END-EXEC
004142061114     C
004143061114     C/EXEC SQL
004144061114     C+ OPEN c1
004145061114     C/END-EXEC
004146061114     c                   endsr
004147061114     c**********************************************************************
004148061114     c     srdata322     begsr
004149061114     c**********************************************************************
004150061114     c                   exsr      istruzione2
004151061114
004152061114     C                   DO        *hival
004153061114     C/EXEC SQL
004154061114     C+ FETCH NEXT FROM c1 INTO :fgs, :ddc, :conta
004155061114     C/END-EXEC
004156061114     C                   SELECT
004157061114     c*
004158061114     C                   WHEN      SqlCod = 100
004159061114     c                   leave
004160061114     C                   WHEN      SqlCod < 0
004161061114     C                   seton                                        H1
004162061114     c                   exsr      srfine
004163061114     c                   other
004164061114     c                   eval      mult = '322'
004165061114     c                   move      ddc           dtaiso
004166061114     c                   move      fgs           comfgs
004167061114     c                   exsr      aggio
004168061114     C                   ENDSL
004169061114      *
004170061114     C                   ENDDO
004171061114     C/EXEC SQL
004172061114     C+ CLOSE c1
004173061114     C/END-EXEC
004174061114     c                   endsr
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
