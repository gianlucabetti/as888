000400030115
000500051227      /TITLE legge ynnct settimanalemente
000600051129     ftntud01l  uf a e           k disk
000601051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000602051129     dkpjba          e ds
000603051229     dtntudtads      e ds
000604051129     dtntu05ds       e ds
000605051227     dynnctds        e ds                  extname(ynnct00f)
000606051214     D wrkGetLista     S           4096
000607051214     D                                     VARYING
000608051222     d comdti          s             10
000609051222     d comdtf          s             10
000612051227     d dtaiso          s                   like(tuddta)
000613051227     d dta             s                   like(tuddta)
000614051227     d fgs             s                   like(tudfil)
000615051227     d comfgs          s                   like(tudfil)
000616051129     d conta           s                   like(tudqta)
000617051214     d mult            s                   like(tudmult)
000618060112
002000060112     C/EXEC SQL
002001060112     C+ SET OPTION DYNUSRPRF = *OWNER
002002060112     C/END-EXEC
002003060112
002004051222     c                   move      t05dti        dtaiso
002005051222     c                   move      dtaiso        comdti
002006051222     c                   move      t05dtf        dtaiso
002007051222     c                   move      dtaiso        comdtf
002041060112     **?Conto le richieste automatiche completate.
002042051227     c                   if        t05mult = ' ' or t05mult = '390'
002043051227     c                   eval      mult = '390'
002046051227     c                   exsr      srpocompl
002047051219     c                   end
002048060112     **?Conto le richieste manuali immesse.
002049060112     c                   if        t05mult = ' ' or t05mult = '395'
002050060112     c                   eval      mult = '395'
002051060112     c                   exsr      srfil
002053060112     c                   end
002054051229     c* aggiorno data ultima elaborazione nella tabella TUP
002055051229     c                   clear                   tntudtads
002056051229     c                   eval      §DTAazn = kcoaz
002057051229     c                   eval      §DTAdta = t05dtf
002058051229     c                   call      'TNTUDTAR'
002059051229     c                   parm                    tntudtads
002060051214     c*
002061051214     c                   seton                                        lr
004263051222     C**************************************************************************
004264051222     C* compongo istruzione sql
004265051222     C**************************************************************************
004266051222     C     istruzione    BEGSR
004267051222     **?Imposto la parte fissa dell'istruzione.
004268051222     C                   EVAL      wrkGetLista =
004269051227     C                             'SELECT nctfil, nctddc, count(*) -
004270051227     C                             FROM ynnct00f where -
004275060112     C                             ncttpimm = ''M'' -
004276051227     C                             AND nctddc between ' +
004277051222     C                             '''' + comdti + ''''
004278051222     C                             +
004279051222     C                             ' AND '
004280051222     C                             +
004281051222     C                             '''' + comdtf + ''''
004282051222     **?Se passato p.o.
004283051227     C                   if        t05fil <> 0
004284051227     C                   EVAL      wrkGetLista = wrkGetLista +
004285051227     C                             ' AND nctfil = ' + %TRIML(%EDITC(t05fil:'Z'))
004286051227     C                   END
004287051227     **?Ordinamento.
004288051227     C                   EVAL      wrkGetLista = wrkGetLista +
004289051227     C                             ' GROUP BY nctfil, nctddc'
004294051222     **?Solo lettura.
004295051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004296051222     C
004297051222     C/EXEC SQL
004298051222     C+ PREPARE s0 FROM :wrkGetLista
004299051222     C/END-EXEC
004300051222     C
004301051222     C/EXEC SQL
004302051222     C+ DECLARE a0 CURSOR FOR S0
004303051222     C/END-EXEC
004304051222     C
004305051222     C/EXEC SQL
004306051222     C+ OPEN a0
004307051222     C/END-EXEC
004308051222     c                   endsr
004309051222     c**********************************************************************
004310051227     c     srfil         begsr
004311051222     c**********************************************************************
004312051222     c                   exsr      istruzione
004313051222     c                   clear                   conta
004314051222
004315051222     C                   DO        *hival
004316051222     C/EXEC SQL
004317051227     C+ FETCH NEXT FROM a0 INTO :fgs, :dta, :conta
004318051222     C/END-EXEC
004319051222     C                   SELECT
004320051222     c*
004321051222     C                   WHEN      SqlCod = 100
004325051222     c                   leave
004326051222     C                   WHEN      SqlCod < 0
004327051222     C                   seton                                        H1
004328051222     c                   exsr      srfine
004329051222     c                   other
004330051227     **?calcolo nr. richieste note acc. x po richiedente
004332051227     c                   move      dta           dtaiso
004333051227     c                   move      fgs           comfgs
004334051227     c                   exsr      aggio
004335051222     C                   ENDSL
004336051222      *
004337051222     C                   ENDDO
004338051222     C/EXEC SQL
004339051222     C+ CLOSE a0
004340051222     C/END-EXEC
004341051222     c                   endsr
004342051227     C**************************************************************************
004343051227     C* compongo istruzione sql
004344051227     C**************************************************************************
004345051227     C     istruzione1   BEGSR
004346051227     **?Imposto la parte fissa dell'istruzione.
004347051227     C                   EVAL      wrkGetLista =
004348060112     C                             'SELECT nctpocompl, nctdin, count(*) -
004349051227     C                             FROM ynnct00f where -
004351051227     C                             nctpocompl <> 0 -
004352060112     C                             AND ncttpimm = ''A'' -
004353060112     C                             AND nctdin between ' +
004354051227     C                             '''' + comdti + ''''
004355051227     C                             +
004356051227     C                             ' AND '
004357051227     C                             +
004358051227     C                             '''' + comdtf + ''''
004359051227     **?Se passato p.o.
004360051227     C                   if        t05fil <> 0
004361051227     C                   EVAL      wrkGetLista = wrkGetLista +
004362051227     C                             ' AND nctpocompl = ' +
004363051227     C                             %TRIML(%EDITC(t05fil:'Z'))
004364051227     C                   END
004365051227     **?Ordinamento.
004366051227     C                   EVAL      wrkGetLista = wrkGetLista +
004367060112     C                             ' GROUP BY nctpocompl, nctdin'
004368051227     **?Solo lettura.
004369051227     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004370051227     C
004371051227     C/EXEC SQL
004372051227     C+ PREPARE s1 FROM :wrkGetLista
004373051227     C/END-EXEC
004374051227     C
004375051227     C/EXEC SQL
004376051227     C+ DECLARE a1 CURSOR FOR S1
004377051227     C/END-EXEC
004378051227     C
004379051227     C/EXEC SQL
004380051227     C+ OPEN a1
004381051227     C/END-EXEC
004382051227     c                   endsr
004383051227     c**********************************************************************
004384051227     c     srpocompl     begsr
004385051227     c**********************************************************************
004386051227     c                   exsr      istruzione1
004387051227     c                   clear                   conta
004389051227
004390051227     C                   DO        *hival
004391051227     C/EXEC SQL
004392051227     C+ FETCH NEXT FROM a1 INTO :fgs, :dta, :conta
004393051227     C/END-EXEC
004394051227     C                   SELECT
004395051227     c*
004396051227     C                   WHEN      SqlCod = 100
004397051227     c                   leave
004398051227     C                   WHEN      SqlCod < 0
004399051227     C                   seton                                        H1
004400051227     c                   exsr      srfine
004401051227     c                   other
004402051227     **?calcolo nr. richieste note acc. x po cpmpletante
004403051227     c                   move      dta           dtaiso
004404051227     c                   move      fgs           comfgs
004405051227     c                   exsr      aggio
004406051227     C                   ENDSL
004407051227      *
004408051227     C                   ENDDO
004409051227     C/EXEC SQL
004410051227     C+ CLOSE a1
004411051227     C/END-EXEC
004412051227     c                   endsr
004972051214     c**********************************************************************
004973051214     c     aggio         begsr
004974051214     c**********************************************************************
004977051129     c     keytud        chain     tntud01l
004981051227     c                   eval      tudqta = conta
004983051129     c                   if        %found(tntud01l)
004984051229     c                   exsr      srstampa
004985051129     c                   update    tntud000
004986051129     c                   else
004987051214     c                   eval      tudmult = mult
004988051130     c                   eval      tuddta  = dtaiso
004989051219     c                   eval      tudfil  = comfgs
004990051129     c                   clear                   tudcli
004991051129     c                   write     tntud000
004992051129     c                   end
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
005345051229     c                   seton                                        of
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
008600051229     O                                           41 'Anomalie nel pgm TNTU17R'
008700051229     O                       PAGE          Z    132
008800051229     O          E            anomalia    1
008900051229     O                                           +0 'Si è eseguito un UPDATE pe-
009000051229     O                                              r il moltiplicatore'
009100051229     O                       tudmult             +1
009200051229     O                                           +1 'P.O.'
009300051229     O                       tudfil              +1
009400051229     O                                           +1 'Data'
009500051229     O                       tuddta              +1
