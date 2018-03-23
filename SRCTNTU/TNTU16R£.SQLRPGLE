000400030115
000500051227      /TITLE legge tigcp settimanalmente
000600051129     ftntud01l  uf a e           k disk
000601051129     dkpjba          e ds
000602051129     dtntu05ds       e ds
000603051227     dtigcpds        e ds                  extname(tigcp00f)
000604051227     ddgcpflr        e ds
000605051214     D wrkGetLista     S           4096
000606051214     D                                     VARYING
000607051130     d dtaiso          s               d   inz
000612051227     d dta             s                   like(gcpdur) inz
000613051227     d fgs             s                   like(gcplnp)
000615051223     d comfgs          s                   like(tudfil) inz
000616051129     d conta           s                   like(tudqta)
000617051214     d mult            s                   like(tudmult)
002000030115
002041051227     **?calcolo nr. giacenze aperte (arrivo)
002042051227     c                   if        t05mult = ' ' or t05mult = '240'
002044051227     c                   exsr      srdatadur
002045051219     c                   end
002046051227     **?calcolo nr. disposiz. giacenze immesse (no automatiche, partenza)
002047051227     c                   if        t05mult = ' ' or t05mult = '250'
002048051227     c                   exsr      srdatadld
002049051227     c                   end
002050051227     **?calcolo nr. chiusure giacenze addebito spese (partenza)
002051051227     c                   if        t05mult = ' ' or t05mult = '260'
002052051227     c                   exsr      srdatadbg
002053051227     c                   end
002054051227     **?calcolo nr. appuntamenti presi (arrivo)
002055051227     c                   if        t05mult = ' ' or t05mult = '270'
002056051227     c                   exsr      srdataded
002057051227     c                   end
002058051214     c*
002059051214     c                   seton                                        lr
004263051222     C**************************************************************************
004264051222     C* compongo istruzione sql
004265051222     C**************************************************************************
004266051222     C     istruzione    BEGSR
004267051222     **?Imposto la parte fissa dell'istruzione.
004268051222     C                   EVAL      wrkGetLista =
004269051227     C                             'SELECT gcpfgc, gcpdur, count(*) -
004271051227     C                             FROM tigcp00f where -
004275051227     C                             gcpdur between ' +
004276051223     C                             %EDITC(t05dti:'X') + ' AND ' +
004277051223     C                             %EDITC(t05dtf:'X')
004283051222     **?Se passato p.o.
004284051222     C                   if        t05fil <> 0
004285051222     C                   EVAL      wrkGetLista = wrkGetLista +
004286051227     C                             ' AND gcpfgc = ' +
004287051223     C                             %TRIML(%EDITC(t05fil:'Z'))
004288051222     C                   END
004289051222     **?Ordinamento.
004290051222     C                   EVAL      wrkGetLista = wrkGetLista +
004291051227     C                             ' GROUP BY gcpfgc, gcpdur '
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
004309051227     c     srdatadur     begsr
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
004329051227     **?calcolo nr. giacenze aperte
004330051227     c                   eval      mult = '240'
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
004341051227     C**************************************************************************
004342051227     C* compongo istruzione sql
004343051227     C**************************************************************************
004344051227     C     istruzione1   BEGSR
004345051227     **?Imposto la parte fissa dell'istruzione.
004346051227     C                   EVAL      wrkGetLista =
004347051227     C                             'SELECT gcplnp, gcpdld, count(*) -
004348051227     C                             FROM tigcp00f where -
004350051227     C                             substr(gcpflr, 2, 1) = '' ''  and -
004351051227     C                             gcpdld between ' +
004352051227     C                             %EDITC(t05dti:'X') + ' AND ' +
004353051227     C                             %EDITC(t05dtf:'X')
004354051227     **?Se passato p.o.
004355051227     C                   if        t05fil <> 0
004356051227     C                   EVAL      wrkGetLista = wrkGetLista +
004357051227     C                             ' AND gcplnp = ' +
004358051227     C                             %TRIML(%EDITC(t05fil:'Z'))
004359051227     C                   END
004360051227     **?Ordinamento.
004361051227     C                   EVAL      wrkGetLista = wrkGetLista +
004362051227     C                             ' GROUP BY gcplnp, gcpdld '
004363051227     **?Solo lettura.
004364051227     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004365051227     C
004366051227     C/EXEC SQL
004367051227     C+ PREPARE s1 FROM :wrkGetLista
004368051227     C/END-EXEC
004369051227     C
004370051227     C/EXEC SQL
004371051227     C+ DECLARE a1 CURSOR FOR S1
004372051227     C/END-EXEC
004373051227     C
004374051227     C/EXEC SQL
004375051227     C+ OPEN a1
004376051227     C/END-EXEC
004377051227     c                   endsr
004378051227     c**********************************************************************
004379051227     c     srdatadld     begsr
004380051227     c**********************************************************************
004381051227     c                   exsr      istruzione1
004382051227     c                   clear                   conta
004383051227
004384051227     C                   DO        *hival
004385051227     C/EXEC SQL
004386051227     C+ FETCH NEXT FROM a1 INTO :fgs, :dta, :conta
004387051227     C/END-EXEC
004388051227     C                   SELECT
004389051227     c*
004390051227     C                   WHEN      SqlCod = 100
004391051227     c                   leave
004392051227     C                   WHEN      SqlCod < 0
004393051227     C                   seton                                        H1
004394051227     c                   exsr      srfine
004395051227     c                   other
004396051227     **?calcolo nr. disposizioni immesse manualmente
004397051227     c                   eval      mult = '250'
004398051227     c                   move      dta           dtaiso
004399051227     c                   move      fgs           comfgs
004400051227     c                   exsr      aggio
004401051227     C                   ENDSL
004402051227      *
004403051227     C                   ENDDO
004404051227     C/EXEC SQL
004405051227     C+ CLOSE a1
004406051227     C/END-EXEC
004407051227     c                   endsr
004408051227     C**************************************************************************
004409051227     C* compongo istruzione sql
004410051227     C**************************************************************************
004411051227     C     istruzione2   BEGSR
004412051227     **?Imposto la parte fissa dell'istruzione.
004413051227     C                   EVAL      wrkGetLista =
004414051227     C                             'SELECT gcplnp, gcpdbg, count(*) -
004415051227     C                             FROM tigcp00f where -
004416051227     C                             gcpdbg between ' +
004417051227     C                             %EDITC(t05dti:'X') + ' AND ' +
004418051227     C                             %EDITC(t05dtf:'X')
004419051227     **?Se passato p.o.
004420051227     C                   if        t05fil <> 0
004421051227     C                   EVAL      wrkGetLista = wrkGetLista +
004422051227     C                             ' AND gcplnp = ' +
004423051227     C                             %TRIML(%EDITC(t05fil:'Z'))
004424051227     C                   END
004425051227     **?Ordinamento.
004426051227     C                   EVAL      wrkGetLista = wrkGetLista +
004427051227     C                             ' GROUP BY gcplnp, gcpdbg '
004428051227     **?Solo lettura.
004429051227     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004430051227     C
004431051227     C/EXEC SQL
004432051227     C+ PREPARE s2 FROM :wrkGetLista
004433051227     C/END-EXEC
004434051227     C
004435051227     C/EXEC SQL
004436051227     C+ DECLARE a2 CURSOR FOR S2
004437051227     C/END-EXEC
004438051227     C
004439051227     C/EXEC SQL
004440051227     C+ OPEN a2
004441051227     C/END-EXEC
004442051227     c                   endsr
004443051227     c**********************************************************************
004444051227     c     srdatadbg     begsr
004445051227     c**********************************************************************
004446051227     c                   exsr      istruzione2
004447051227     c                   clear                   conta
004448051227
004449051227     C                   DO        *hival
004450051227     C/EXEC SQL
004451051227     C+ FETCH NEXT FROM a2 INTO :fgs, :dta, :conta
004452051227     C/END-EXEC
004453051227     C                   SELECT
004454051227     c*
004455051227     C                   WHEN      SqlCod = 100
004456051227     c                   leave
004457051227     C                   WHEN      SqlCod < 0
004458051227     C                   seton                                        H1
004459051227     c                   exsr      srfine
004460051227     c                   other
004461051227     **?calcolo nr. giacenze chiuse addebito spese
004462051227     c                   eval      mult = '260'
004463051227     c                   move      dta           dtaiso
004464051227     c                   move      fgs           comfgs
004465051227     c                   exsr      aggio
004466051227     C                   ENDSL
004467051227      *
004468051227     C                   ENDDO
004469051227     C/EXEC SQL
004470051227     C+ CLOSE a2
004471051227     C/END-EXEC
004472051227     c                   endsr
004473051227     C**************************************************************************
004474051227     C* compongo istruzione sql
004475051227     C**************************************************************************
004476051227     C     istruzione3   BEGSR
004477051227     **?Imposto la parte fissa dell'istruzione.
004478051227     C                   EVAL      wrkGetLista =
004479051227     C                             'SELECT gcpfgc, gcpdld, count(*) -
004480051227     C                             FROM tigcp00f where -
004481051227     C                             gcpded <> 0 and -
004482051227     C                             substr(gcpflr, 1, 1) = ''A'' and -
004483051227     C                             gcpdld between ' +
004484051227     C                             %EDITC(t05dti:'X') + ' AND ' +
004485051227     C                             %EDITC(t05dtf:'X')
004486051227     **?Se passato p.o.
004487051227     C                   if        t05fil <> 0
004488051227     C                   EVAL      wrkGetLista = wrkGetLista +
004489051227     C                             ' AND gcpfgc = ' +
004490051227     C                             %TRIML(%EDITC(t05fil:'Z'))
004491051227     C                   END
004492051227     **?Ordinamento.
004493051227     C                   EVAL      wrkGetLista = wrkGetLista +
004494051227     C                             ' GROUP BY gcpfgc, gcpdld '
004495051227     **?Solo lettura.
004496051227     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004497051227     C
004498051227     C/EXEC SQL
004499051227     C+ PREPARE s3 FROM :wrkGetLista
004500051227     C/END-EXEC
004501051227     C
004502051227     C/EXEC SQL
004503051227     C+ DECLARE a3 CURSOR FOR S3
004504051227     C/END-EXEC
004505051227     C
004506051227     C/EXEC SQL
004507051227     C+ OPEN a3
004508051227     C/END-EXEC
004509051227     c                   endsr
004510051227     c**********************************************************************
004511051227     c     srdataded     begsr
004512051227     c**********************************************************************
004513051227     c                   exsr      istruzione3
004514051227     c                   clear                   conta
004515051227
004516051227     C                   DO        *hival
004517051227     C/EXEC SQL
004518051227     C+ FETCH NEXT FROM a3 INTO :fgs, :dta, :conta
004519051227     C/END-EXEC
004520051227     C                   SELECT
004521051227     c*
004522051227     C                   WHEN      SqlCod = 100
004523051227     c                   leave
004524051227     C                   WHEN      SqlCod < 0
004525051227     C                   seton                                        H1
004526051227     c                   exsr      srfine
004527051227     c                   other
004528051227     **?calcolo nr. giacenze con appuntamento
004529051227     c                   eval      mult = '270'
004530051227     c                   move      dta           dtaiso
004531051227     c                   move      fgs           comfgs
004532051227     c                   exsr      aggio
004533051227     C                   ENDSL
004534051227      *
004535051227     C                   ENDDO
004536051227     C/EXEC SQL
004537051227     C+ CLOSE a3
004538051227     C/END-EXEC
004539051227     c                   endsr
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
