000400030115
000500051222      /TITLE legge fiftt/fiatt/fictt UNA VOLTA AL MESE IN SEDE
000600051129     ftntud01l  uf a e           k disk
000601051129     dkpjba          e ds
000602051129     dtntu05ds       e ds
000603051219     dfifttds        e ds                  extname(fiftt00f)
000605051214     D wrkGetLista     S           4096
000606051214     D                                     VARYING
000607051130     d dtaiso          s               d   inz
000608051222     d comdti          s             10
000609051222     d comdtf          s             10
000610051222     d dtai            s                   like(dtaiso)
000611051222     d reg             s              9  0
000612051222     d dta             s                   like(fttddc) inz
000613051219     d fgs             s                   like(fttfgs) inz
000614051219     d nff             s                   like(fttnff) inz
000615051222     d comfgs          s                   like(fttfgs) inz
000616051129     d conta           s                   like(tudqta)
000617051214     d mult            s                   like(tudmult)
000618051222     d rotto           s             13
000619051219     d rottos          s                   like(rotto)
002000030115
002001051222     c                   move      t05dti        dtaiso
002002051222     c                   move      dtaiso        comdti
002003051222     c                   move      t05dtf        dtaiso
002004051222     c                   move      dtaiso        comdtf
002041051222     **?calcolo nr. fatture autotrasp.
002042051219     c                   if        t05mult = ' ' or t05mult = '290'
002043051222     c                   eval      mult = '290'
002044051222     c                   exsr      srdatafata
002045051219     c                   end
002046051222     **?calcolo nr. fatture aff/defl
002047051222     c                   if        t05mult = ' ' or t05mult = '300'
002048051222     c                   eval      mult = '300'
002049051222     c                   exsr      srdatafatd
002050051222     c                   end
002051051222     **?calcolo nr. fatture coop
002052051222     c                   if        t05mult = ' ' or t05mult = '310'
002053051222     c                   eval      mult = '310'
002054051222     c                   exsr      srdatafatc
002055051222     c                   end
002056051214     c*
002057051214     c                   seton                                        lr
004115051219     C**************************************************************************
004116051219     C* compongo istruzione sql
004117051219     C**************************************************************************
004118051219     C     istruzione2   BEGSR
004123051219     **?Imposto la parte fissa dell'istruzione.
004124051219     C                   EVAL      wrkGetLista =
004125051222     C                             'SELECT fttfgs, REGDTIMM, REGNRASREG -
004126051222     C                             FROM Fiftt00f join ndreg00f on  -
004127051222     C                             0 = regsys and -
004128051222     C                             substr(fttflr, 2, 9) =digits(REGNRASREG) -
004129051222     C                             where fttnff <> 0 and fttnff <> 999999999 -
004130051222     C                             and ftttsr ='' '' AND REGDTIMM between ' +
004131051222     C                             '''' + comdti + ''''
004132051222     C                             +
004133051222     C                             ' AND '
004134051222     C                             +
004135051222     C                             '''' + comdtf + ''''
004136051219     **?Se passato p.o.
004137051219     C                   if        t05fil <> 0
004138051219     C                   EVAL      wrkGetLista = wrkGetLista +
004139051219     C                             ' AND fttfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
004140051219     C                   END
004141051219     **?Ordinamento.
004142051219     C                   EVAL      wrkGetLista = wrkGetLista +
004143051222     C                             ' GROUP BY fttfgs, REGDTIMM, REGNRASREG'
004144051219     **?Solo lettura.
004145051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004146051219     C
004147051219     C/EXEC SQL
004148051222     C+ PREPARE s2 FROM :wrkGetLista
004149051219     C/END-EXEC
004150051219     C
004151051219     C/EXEC SQL
004152051222     C+ DECLARE a2 CURSOR FOR S2
004153051219     C/END-EXEC
004154051219     C
004155051219     C/EXEC SQL
004156051222     C+ OPEN a2
004157051219     C/END-EXEC
004158051219     c                   endsr
004159051219     c**********************************************************************
004160051222     c     srdatafata    begsr
004161051219     c**********************************************************************
004162051219     c                   exsr      istruzione2
004163051219     c                   clear                   conta
004164051219
004165051219     C                   DO        *hival
004166051219     C/EXEC SQL
004167051222     C+ FETCH NEXT FROM a2 INTO :fgs, :dtai, :reg
004168051219     C/END-EXEC
004169051219     C                   SELECT
004170051219     c*
004171051219     C                   WHEN      SqlCod = 100
004172051219     c                   if        conta <>0
004173051219     c                   exsr      sraggio
004174051219     c                   end
004175051219     c                   leave
004176051219     C                   WHEN      SqlCod < 0
004177051219     C                   seton                                        H1
004178051219     c                   exsr      srfine
004179051219     c                   other
004180051219     **?calcolo nr. fatture
004181051219     c                   exsr      srconta
004182051219     C                   ENDSL
004183051219      *
004184051219     C                   ENDDO
004185051219     C/EXEC SQL
004186051222     C+ CLOSE a2
004187051219     C/END-EXEC
004188051219     c                   endsr
004189051222     C**************************************************************************
004190051222     C* compongo istruzione sql
004191051222     C**************************************************************************
004192051222     C     istruzione1   BEGSR
004197051222     **?Imposto la parte fissa dell'istruzione.
004198051222     C                   EVAL      wrkGetLista =
004199051222     C                             'SELECT attter, REGDTIMM, REGNRASREG -
004200051222     C                             FROM Fiatt00f join ndreg00f on  -
004201051222     C                             0 = regsys and -
004202051222     C                             attnra=REGNRASREG   -
004203051222     C                             where attnra <> 0 -
004204051222     C                             AND REGDTIMM between ' +
004205051222     C                             '''' + comdti + ''''
004206051222     C                             +
004207051222     C                             ' AND '
004208051222     C                             +
004209051222     C                             '''' + comdtf + ''''
004210051222     **?Se passato p.o.
004211051222     C                   if        t05fil <> 0
004212051222     C                   EVAL      wrkGetLista = wrkGetLista +
004213051222     C                             ' AND attter = ' + %TRIML(%EDITC(t05fil:'Z'))
004214051222     C                   END
004215051222     **?Ordinamento.
004216051222     C                   EVAL      wrkGetLista = wrkGetLista +
004217051222     C                             ' GROUP BY attter, REGDTIMM, REGNRASREG'
004218051222     **?Solo lettura.
004219051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004220051222     C
004221051222     C/EXEC SQL
004222051222     C+ PREPARE s1 FROM :wrkGetLista
004223051222     C/END-EXEC
004224051222     C
004225051222     C/EXEC SQL
004226051222     C+ DECLARE a1 CURSOR FOR S1
004227051222     C/END-EXEC
004228051222     C
004229051222     C/EXEC SQL
004230051222     C+ OPEN a1
004231051222     C/END-EXEC
004232051222     c                   endsr
004233051222     c**********************************************************************
004234051222     c     srdatafatd    begsr
004235051222     c**********************************************************************
004236051222     c                   exsr      istruzione1
004237051222     c                   clear                   conta
004238051222
004239051222     C                   DO        *hival
004240051222     C/EXEC SQL
004241051222     C+ FETCH NEXT FROM a1 INTO :fgs, :dtai, :reg
004242051222     C/END-EXEC
004243051222     C                   SELECT
004244051222     c*
004245051222     C                   WHEN      SqlCod = 100
004246051222     c                   if        conta <>0
004247051222     c                   exsr      sraggio
004248051222     c                   end
004249051222     c                   leave
004250051222     C                   WHEN      SqlCod < 0
004251051222     C                   seton                                        H1
004252051222     c                   exsr      srfine
004253051222     c                   other
004254051222     **?calcolo nr. fatture
004255051222     c                   exsr      srconta
004256051222     C                   ENDSL
004257051222      *
004258051222     C                   ENDDO
004259051222     C/EXEC SQL
004260051222     C+ CLOSE a1
004261051222     C/END-EXEC
004262051222     c                   endsr
004263051222     C**************************************************************************
004264051222     C* compongo istruzione sql
004265051222     C**************************************************************************
004266051222     C     istruzione    BEGSR
004267051222     **?Imposto la parte fissa dell'istruzione.
004268051222     C                   EVAL      wrkGetLista =
004269051222     C                             'SELECT cttfgs, REGDTIMM, REGNRASREG -
004270051222     C                             FROM Fictt00f join ndreg00f on  -
004271051222     C                             0 = regsys and -
004272051222     C                             cttnra=REGNRASREG   -
004273051222     C                             where cttnra <> 0 -
004274051222     C                             AND REGDTIMM between ' +
004275051222     C                             '''' + comdti + ''''
004276051222     C                             +
004277051222     C                             ' AND '
004278051222     C                             +
004279051222     C                             '''' + comdtf + ''''
004280051222     **?Se passato p.o.
004281051222     C                   if        t05fil <> 0
004282051222     C                   EVAL      wrkGetLista = wrkGetLista +
004283051222     C                             ' AND cttfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
004284051222     C                   END
004285051222     **?Ordinamento.
004286051222     C                   EVAL      wrkGetLista = wrkGetLista +
004287051222     C                             ' GROUP BY cttfgs, REGDTIMM, REGNRASREG'
004288051222     **?Solo lettura.
004289051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004290051222     C
004291051222     C/EXEC SQL
004292051222     C+ PREPARE s0 FROM :wrkGetLista
004293051222     C/END-EXEC
004294051222     C
004295051222     C/EXEC SQL
004296051222     C+ DECLARE a0 CURSOR FOR S0
004297051222     C/END-EXEC
004298051222     C
004299051222     C/EXEC SQL
004300051222     C+ OPEN a0
004301051222     C/END-EXEC
004302051222     c                   endsr
004303051222     c**********************************************************************
004304051222     c     srdatafatc    begsr
004305051222     c**********************************************************************
004306051222     c                   exsr      istruzione
004307051222     c                   clear                   conta
004308051222
004309051222     C                   DO        *hival
004310051222     C/EXEC SQL
004311051222     C+ FETCH NEXT FROM a0 INTO :fgs, :dtai, :reg
004312051222     C/END-EXEC
004313051222     C                   SELECT
004314051222     c*
004315051222     C                   WHEN      SqlCod = 100
004316051222     c                   if        conta <>0
004317051222     c                   exsr      sraggio
004318051222     c                   end
004319051222     c                   leave
004320051222     C                   WHEN      SqlCod < 0
004321051222     C                   seton                                        H1
004322051222     c                   exsr      srfine
004323051222     c                   other
004324051222     **?calcolo nr. fatture
004325051222     c                   exsr      srconta
004326051222     C                   ENDSL
004327051222      *
004328051222     C                   ENDDO
004329051222     C/EXEC SQL
004330051222     C+ CLOSE a0
004331051222     C/END-EXEC
004332051222     c                   endsr
004333051219     c**********************************************************************
004334051219     c     srconta       begsr
004335051219     c**********************************************************************
004336051222     c* a rottura di p.o. e data immissione documento
004337051219     c                   movel     fgs           rotto
004338051222     c                   move      dtai          rotto
004339051222     c                   if        rotto <> rottos and rottos <> ' '
004341051219     c                   exsr      sraggio
004342051219     c                   clear                   conta
004343051219     c                   end
004344051219     c                   add       1             conta
004345051219     c                   eval      rottos = rotto
004346051219     c                   endsr
004347051219     c**********************************************************************
004348051219     c     sraggio       begsr
004349051219     c**********************************************************************
004350051219     c                   movel     rottos        comfgs
004351051222     c                   move      rottos        dtaiso
004352051219     c                   if        conta <> 0
004353051219     c                   exsr      aggio
004354051219     c                   end
004355051219     c                   endsr
004972051214     c**********************************************************************
004973051214     c     aggio         begsr
004974051214     c**********************************************************************
004977051129     c     keytud        chain     tntud01l
004978051129     c                   eval      tudqta = conta
004979051129     c                   if        %found(tntud01l)
004980051129     c                   update    tntud000
004981051129     c                   else
004982051214     c                   eval      tudmult = mult
004983051130     c                   eval      tuddta  = dtaiso
004984051219     c                   eval      tudfil  = comfgs
004985051129     c                   clear                   tudcli
004986051129     c                   write     tntud000
004987051129     c                   end
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
005400051129     c                   endsr
005500051129     C**************************************************************************
005600051129     C* esco con codice d'errore
005700051129     C**************************************************************************
005800051129     C     srfine        BEGSR
007100051129     c                   seton                                        lr
007200051129     c                   return
007300051129     c*
007400051129     c                   endsr
