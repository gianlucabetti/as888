000400030115
000500051227      /TITLE legge fiftt settimanalmente
000600051129     ftntud01l  uf a e           k disk
000601051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000602051129     dkpjba          e ds
000603051229     dtntudtads      e ds
000604051129     dtntu05ds       e ds
000605051219     dfifttds        e ds                  extname(fiftt00f)
000606051214     D wrkGetLista     S           4096
000607051214     D                                     VARYING
000608051130     d dtaiso          s               d   inz
000611051222     d dta             s                   like(fttddc) inz
000612051219     d fgs             s                   like(fttfgs) inz
000613051219     d nff             s                   like(fttnff) inz
000614051222     d comfgs          s                   like(fttfgs) inz
000615051129     d conta           s                   like(tudqta)
000616051214     d mult            s                   like(tudmult)
000617051222     d rotto           s             13
000618051219     d rottos          s                   like(rotto)
002000030115
002033051219     **?calcolo nr. distinte valorizzate
002034051228     c                   if        t05mult = ' ' or t05mult = '120'
002035051214     c                   exsr      srdatadist
002036051219     c                   end
002045051222     **?calcolo nr. distinte chiuse
002046051222     c                   if        t05mult = ' ' or t05mult = '140'
002047051222     c                   exsr      srdatachi
002048051222     c                   end
002049051229     c* aggiorno data ultima elaborazione nella tabella TUP
002050051229     c                   clear                   tntudtads
002051051229     c                   eval      §DTAazn = kcoaz
002052051229     c                   eval      §DTAdta = t05dtf
002053051229     c                   call      'TNTUDTAR'
002054051229     c                   parm                    tntudtads
002055051214     c*
002056051214     c                   seton                                        lr
002057051219     C**************************************************************************
002058051219     C* compongo istruzione sql
002059051219     C**************************************************************************
002060051219     C     istruzione    BEGSR
002061051219     **?Imposto la parte fissa dell'istruzione.
002062051219     C                   EVAL      wrkGetLista =
002063051220     C                             'SELECT fttfgs, fttdcv, count(*) -
002064060120     C                             FROM Fiftt01l where -
002065051219     C                             ftttsr <>'' '' AND fttdcv BETWEEN ' +
002066051219     C                             %EDITC(t05dti:'X') + ' AND ' +
002067051219     C                             %EDITC(t05dtf:'X')
002068051219     **?Se passato p.o.
002069051219     C                   if        t05fil <> 0
002070051219     C                   EVAL      wrkGetLista = wrkGetLista +
002071051219     C                             ' AND fttfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
002072051219     C                   END
002073051220     **?raggruppamento
002074051219     C                   EVAL      wrkGetLista = wrkGetLista +
002075051220     C                             ' GROUP BY fttfgs, fttdcv'
002076051220     **?Ordinamento.
002077051220     C                   EVAL      wrkGetLista = wrkGetLista +
002078051220     C                             ' ORDER BY fttfgs, fttdcv'
002079051219     **?Solo lettura.
002080051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
002081051219     C
002082051219     C/EXEC SQL
002083051219     C+ PREPARE s1 FROM :wrkGetLista
002084051219     C/END-EXEC
002085051219     C
002086051219     C/EXEC SQL
002087051219     C+ DECLARE A1 CURSOR FOR S1
002088051219     C/END-EXEC
002089051219     C
002090051219     C/EXEC SQL
002091051219     C+ OPEN a1
002092051219     C/END-EXEC
002093051219     c                   endsr
002094051214     c**********************************************************************
002095051214     c     srdatadist    begsr
002096051214     c**********************************************************************
002097051214     c                   exsr      istruzione
004007051214
004011051129     C                   DO        *hival
004012051129     C/EXEC SQL
004013051219     C+ FETCH NEXT FROM a1 INTO :fgs, :dta, :conta
004014051129     C/END-EXEC
004015051129     C                   SELECT
004016051129     c*
004017051129     C                   WHEN      SqlCod = 100
004022051129     c                   leave
004023051129     C                   WHEN      SqlCod < 0
004024051129     C                   seton                                        H1
004025051129     c                   exsr      srfine
004026051129     c                   other
004027051219     **?calcolo nr. distinte valorizzate
004028051228     c                   eval      mult = '120'
004029051219     c                   move      dta           dtaiso
004030051219     c                   move      fgs           comfgs
004031051219     c                   exsr      aggio
004041051129     C                   ENDSL
004042051129      *
004043051129     C                   ENDDO
004044051129     C/EXEC SQL
004045051219     C+ CLOSE a1
004046051129     C/END-EXEC
004047051214     c                   endsr
004253051222     C**************************************************************************
004254051222     C* compongo istruzione sql
004255051222     C**************************************************************************
004256051222     C     istruzione3   BEGSR
004257051222     **?Imposto la parte fissa dell'istruzione.
004258051222     C                   EVAL      wrkGetLista =
004259051222     C                             'SELECT fttfgs, fttdtr, count(*) -
004260060120     C                             FROM Fiftt01l where -
004261051222     C                             ftttsr = ''C'' AND fttdtr BETWEEN ' +
004262051222     C                             %EDITC(t05dti:'X') + ' AND ' +
004263051222     C                             %EDITC(t05dtf:'X')
004264051222     **?Se passato p.o.
004265051222     C                   if        t05fil <> 0
004266051222     C                   EVAL      wrkGetLista = wrkGetLista +
004267051222     C                             ' AND fttfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
004268051222     C                   END
004269051222     **?raggruppamento
004270051222     C                   EVAL      wrkGetLista = wrkGetLista +
004271051222     C                             ' GROUP BY fttfgs, fttdtr'
004272051222     **?Ordinamento.
004273051222     C                   EVAL      wrkGetLista = wrkGetLista +
004274051222     C                             ' ORDER BY fttfgs, fttdtr'
004275051222     **?Solo lettura.
004276051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004277051222     C
004278051222     C/EXEC SQL
004279051222     C+ PREPARE s3 FROM :wrkGetLista
004280051222     C/END-EXEC
004281051222     C
004282051222     C/EXEC SQL
004283051222     C+ DECLARE A3 CURSOR FOR S3
004284051222     C/END-EXEC
004285051222     C
004286051222     C/EXEC SQL
004287051222     C+ OPEN a3
004288051222     C/END-EXEC
004289051222     c                   endsr
004290051222     c**********************************************************************
004291051222     c     srdatachi     begsr
004292051222     c**********************************************************************
004293051222     c                   exsr      istruzione3
004294051222
004295051222     C                   DO        *hival
004296051222     C/EXEC SQL
004297051222     C+ FETCH NEXT FROM a3 INTO :fgs, :dta, :conta
004298051222     C/END-EXEC
004299051222     C                   SELECT
004300051222     c*
004301051222     C                   WHEN      SqlCod = 100
004302051222     c                   leave
004303051222     C                   WHEN      SqlCod < 0
004304051222     C                   seton                                        H1
004305051222     c                   exsr      srfine
004306051222     c                   other
004307051222     **?calcolo nr. distinte valorizzate
004308051222     c                   eval      mult = '140'
004309051222     c                   move      dta           dtaiso
004310051222     c                   move      fgs           comfgs
004311051222     c                   exsr      aggio
004312051222     C                   ENDSL
004313051222      *
004314051222     C                   ENDDO
004315051222     C/EXEC SQL
004316051222     C+ CLOSE a3
004317051222     C/END-EXEC
004318051222     c                   endsr
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
008600051229     O                                           41 'Anomalie nel pgm TNTU11R'
008700051229     O                       PAGE          Z    132
008800051229     O          E            anomalia    1
008900051229     O                                           +0 'Si è eseguito un UPDATE pe-
009000051229     O                                              r il moltiplicatore'
009100051229     O                       tudmult             +1
009200051229     O                                           +1 'P.O.'
009300051229     O                       tudfil              +1
009400051229     O                                           +1 'Data'
009500051229     O                       tuddta              +1
