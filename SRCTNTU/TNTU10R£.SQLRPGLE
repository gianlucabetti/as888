000400030115
000500051227      /TITLE legge fiftd settimanalmente
000600051129     ftntud01l  uf a e           k disk
000601051129     dkpjba          e ds
000602051129     dtntu05ds       e ds
000603051214     dfiftdds        e ds                  extname(fiftd00f)
000604051214     D wrkGetLista     S           4096
000605051214     D                                     VARYING
000606051130     d dtaiso          s               d   inz
000607051214     d dcm             s                   like(ftddcm) inz
000608051214     d ddc             s                   like(ftdddc) inz
000609051214     d ndc             s                   like(ftdndc) inz
000610051214     d fgs             s                   like(ftdfgs) inz
000611051214     d cas             s                   like(ftdcas) inz
000612051214     d ifp             s                   like(ftdifp) inz
000613051129     d conta           s                   like(tudqta)
000614051214     d mult            s                   like(tudmult)
000615051214     d rotto           s             11  0
000616051214     d rottos          s                   like(rotto)
000618051214     d dist            s             18  0
000619051219     d comdta          s              8  0
000620051219     d comfgs          s                   like(ftdfgs) inz
000621051219     d dists           s                   like(dist)
000622051219     d dist130         s                   like(tudqta)
000623051219     d sped60          s                   like(tudqta)
000624051219     d sped220         s                   like(tudqta)
002000030115
002033051214     **?calcolo nr. distinte in consegna, il nr. di sped. in consegna
002034051214     **?e il nr dei contrassegni incassati
002035051220     c                   if        t05mult = ' ' or t05mult = '060' or
002036051220     c                             t05mult = '220' or t05mult = '130'
002037051214     c                   exsr      srdatadist
002038051220     c                   end
002039051220     **?calcolo nr. delle spedizioni consegnate e il nr. delle distinte
002040051220     **?consegnate. Ho dovuto fare due istruz.
002041051220     **?differenti perchè cambia la data una è la data distinta l'altra è
002042051220     **?la data consegna
002043051222     c                   if        t05mult = ' ' or t05mult = '070'
002045051214     c                   exsr      srdatacons
002046051220     c                   end
002047051214     c*
002048051214     c                   seton                                        lr
002049051219     C**************************************************************************
002050051219     C* compongo istruzione sql
002051051219     C**************************************************************************
002052051219     C     istruzione    BEGSR
002053051219     **?Imposto la parte fissa dell'istruzione.
002054051219     C                   EVAL      wrkGetLista =
002055051220     C                             'SELECT ftdfgs, ftdddc, ftdndc,  -
002056051219     C                             ftdcas, ftdifp FROM Fiftd00f where -
002057051219     C                             ftdtsr = ''C'' AND ftdddc BETWEEN ' +
002058051219     C                             %EDITC(t05dti:'X') + ' AND ' +
002059051219     C                             %EDITC(t05dtf:'X')
002060051219     **?Se passato p.o.
002061051219     C                   if        t05fil <> 0
002062051219     C                   EVAL      wrkGetLista = wrkGetLista +
002063051219     C                             ' AND ftdfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
002064051219     C                   END
002065051219     **?Ordinamento.
002066051219     C                   EVAL      wrkGetLista = wrkGetLista +
002067051219     C                             ' ORDER BY ftdfgs, ftdddc, ftdndc'
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
002084051214     c     srdatadist    begsr
002085051214     c**********************************************************************
002086051214     c                   exsr      istruzione
004007051214
004011051129     C                   DO        *hival
004012051129     C/EXEC SQL
004013051220     C+ FETCH NEXT FROM a1 INTO :fgs, :ddc, :ndc, :cas, :ifp
004014051129     C/END-EXEC
004015051129     C                   SELECT
004016051129     c*
004017051129     C                   WHEN      SqlCod = 100
004018051219     c                   if        sped60<>0 or dist130<>0 or sped220<>0
004020051214     c                   exsr      sraggio
004021051219     c                   end
004022051129     c                   leave
004023051129     C                   WHEN      SqlCod < 0
004024051129     C                   seton                                        H1
004025051129     c                   exsr      srfine
004026051129     c                   other
004027051214     c                   exsr      srconta
004041051129     C                   ENDSL
004042051129      *
004043051129     C                   ENDDO
004044051129     C/EXEC SQL
004045051219     C+ CLOSE a1
004046051129     C/END-EXEC
004047051214     c                   endsr
004048051219     C**************************************************************************
004049051219     C* compongo istruzione sql
004050051219     C**************************************************************************
004051051219     C     istruzione1   BEGSR
004052051219     **?Imposto la parte fissa dell'istruzione.
004053051219     C                   EVAL      wrkGetLista =
004054051222     C                             'SELECT ftdfgs, ftddcm, count(*) -
004055051219     C                             FROM Fiftd00f where -
004057051219     C                             ftdtsr = ''C'' -
004058051219     C                             AND ftddcm BETWEEN ' +
004059051219     C                             %EDITC(t05dti:'X') + ' AND ' +
004060051219     C                             %EDITC(t05dtf:'X')
004061051219     **?Se passato p.o.
004062051219     C                   if        t05fil <> 0
004063051219     C                   EVAL      wrkGetLista = wrkGetLista +
004064051219     C                             ' AND ftdfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
004065051219     C                   END
004066051222     **?Raggruppamento
004067051219     C                   EVAL      wrkGetLista = wrkGetLista +
004068051222     C                             ' GROUP BY ftdfgs, ftddcm'
004069051222     **?Ordinamento.
004070051222     C                   EVAL      wrkGetLista = wrkGetLista +
004071051222     C                             ' ORDER BY ftdfgs, ftddcm'
004072051219     **?Solo lettura.
004073051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004074051219     C
004075051219     C/EXEC SQL
004076051219     C+ PREPARE s2 FROM :wrkGetLista
004077051219     C/END-EXEC
004078051219     C
004079051219     C/EXEC SQL
004080051219     C+ DECLARE a2 CURSOR FOR s2
004081051219     C/END-EXEC
004082051219     C
004083051219     C/EXEC SQL
004084051219     C+ OPEN a2
004085051219     C/END-EXEC
004086051219     c                   endsr
004087051214     c**********************************************************************
004088051214     c     srdatacons    begsr
004089051214     c**********************************************************************
004090051214     c                   exsr      istruzione1
004091051214
004092051214     C                   DO        *hival
004093051214     C/EXEC SQL
004094051222     C+ FETCH NEXT FROM a2 INTO :fgs, :dcm, :conta
004095051214     C/END-EXEC
004096051214     C                   SELECT
004097051214     c*
004098051214     C                   WHEN      SqlCod = 100
004099051214     c                   leave
004100051214     C                   WHEN      SqlCod < 0
004101051214     C                   seton                                        H1
004102051214     c                   exsr      srfine
004103051214     c                   other
004104051222     c                   eval      mult = '070'
004105051222     c                   move      dcm           dtaiso
004106051222     c                   eval      comfgs = fgs
004109051222     c                   exsr      aggio
004110051214     C                   ENDSL
004111051214      *
004112051214     C                   ENDDO
004113051214     C/EXEC SQL
004114051219     C+ CLOSE a2
004115051214     C/END-EXEC
004116051214     c                   endsr
004700051129     c**********************************************************************
004800051214     c     srconta       begsr
004900051129     c**********************************************************************
004901051214     c* a rottura di p.o. e data distinta aggiorno
004902051219     c                   movel     fgs           rotto
004903051219     c                   move      ddc           rotto
004904051219     c                   movel     rotto         dist
004905051219     c                   move      ndc           dist
004906051219     c                   if        rotto <> rottos and rottos <> 0
004911051214     c                   exsr      sraggio
004912051214     c                   clear                   sped60
004913051219     c                   clear                   dist130
004914051214     c                   clear                   sped220
004915051214     c                   end
004916051219     c                   eval      rottos = rotto
004917051214     c* calcolo il nr. sped. in consegnate e il nr. distinte in consegna
004919051214     c                   add       1             sped60
004920051214     c                   if        dist <> dists
004921051214     c                   add       1             dist130
004922051214     c                   eval      dists = dist
004923051220     c                   end
004925051214     c* calcolo il numero dei c/assegni incassati
004926051214     c                   if        cas > 0 or ifp > 0
004927051214     c                   add       1             sped220
004929051214     c                   end
004930051214     c                   endsr
004931051214     c**********************************************************************
004932051214     c     sraggio       begsr
004933051214     c**********************************************************************
004934051219     c                   movel     rottos        comfgs
004935051219     c                   move      rottos        comdta
004936051219     c                   move      comdta        dtaiso
004937051214     c                   if        t05mult <> ' '
004938051214     c                   eval      mult = t05mult
004939051214     c                   select
004940051219     c                   when      t05mult = '060'
004941051214     c                   eval      conta= sped60
004942051214     c                   when      t05mult = '130'
004943051214     c                   eval      conta= dist130
004944051214     c                   when      t05mult = '220'
004945051214     c                   eval      conta= sped220
004946051219     c                   endsl
004947051219     c                   if        conta <> 0
004948051219     c                   exsr      aggio
004949051219     c                   end
004950051214     c                   else
004951051214     c* sped. in consegna
004952051214     c                   eval      mult = '060'
004953051214     c                   eval      conta= sped60
004954051219     c                   if        conta <> 0
004955051214     c                   exsr      aggio
004956051219     c                   end
004957051214     c* dist. in consegna
004958051214     c                   eval      mult = '130'
004959051214     c                   eval      conta= dist130
004960051219     c                   if        conta <> 0
004961051214     c                   exsr      aggio
004962051219     c                   end
004963051214     c* c/ass. incassati
004964051214     c                   eval      mult = '220'
004965051214     c                   eval      conta= sped220
004966051219     c                   if        conta <> 0
004967051214     c                   exsr      aggio
004968051214     c                   end
004969051219     c                   end
004970051214     c* pulisco i contatori
004971051214     c                   endsr
005039051214     c**********************************************************************
005040051214     c     aggio         begsr
005041051214     c**********************************************************************
005042051129     c     keytud        chain     tntud01l
005043051129     c                   eval      tudqta = conta
005044051129     c                   if        %found(tntud01l)
005045051129     c                   update    tntud000
005046051129     c                   else
005047051214     c                   eval      tudmult = mult
005048051130     c                   eval      tuddta  = dtaiso
005049051219     c                   eval      tudfil  = comfgs
005050051129     c                   clear                   tudcli
005051051129     c                   write     tntud000
005052051129     c                   end
005053051129     c                   endsr
005054051129     c**********************************************************************
005055051129     c     *inzsr        begsr
005056051129     c**********************************************************************
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
