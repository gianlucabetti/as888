000100030115
000200051227      /TITLE legge fiftd settimanalmente
000300051129     ftntud01l  uf a e           k disk
000400051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500051129     dkpjba          e ds
000600051229     dtntudtads      e ds
000700051229     dtntu05ds       e ds
000800051214     dfiftdds        e ds                  extname(fiftd00f)
000900051214     D wrkGetLista     S           4096
001000051214     D                                     VARYING
001100051130     d dtaiso          s               d   inz
001200051214     d dcm             s                   like(ftddcm) inz
001300051214     d ddc             s                   like(ftdddc) inz
001400051214     d ndc             s                   like(ftdndc) inz
001500051214     d fgs             s                   like(ftdfgs) inz
001600051214     d cas             s                   like(ftdcas) inz
001700051214     d ifp             s                   like(ftdifp) inz
001800051129     d conta           s                   like(tudqta)
001900051214     d mult            s                   like(tudmult)
002000051214     d rotto           s             11  0
002100051214     d rottos          s                   like(rotto)
002200051214     d dist            s             18  0
002300051219     d comdta          s              8  0
002400051219     d comfgs          s                   like(ftdfgs) inz
002500051219     d dists           s                   like(dist)
002600051219     d dist130         s                   like(tudqta)
002700051219     d sped60          s                   like(tudqta)
002800051219     d sped220         s                   like(tudqta)
002900030115
003000051214     **?calcolo nr. distinte in consegna, il nr. di sped. in consegna
003100051214     **?e il nr dei contrassegni incassati
003200051220     c                   if        t05mult = ' ' or t05mult = '060' or
003300051220     c                             t05mult = '220' or t05mult = '130'
003400051214     c                   exsr      srdatadist
003500051220     c                   end
003600051220     **?calcolo nr. delle spedizioni consegnate e il nr. delle distinte
003700051220     **?consegnate. Ho dovuto fare due istruz.
003800051220     **?differenti perchè cambia la data una è la data distinta l'altra è
003900051220     **?la data consegna
004000051222     c                   if        t05mult = ' ' or t05mult = '070'
004100051214     c                   exsr      srdatacons
004200051220     c                   end
004300051229     c* aggiorno data ultima elaborazione nella tabella TUP
004400051229     c                   clear                   tntudtads
004500051229     c                   eval      §DTAazn = kcoaz
004600051229     c                   eval      §DTAdta = t05dtf
004700051229     c                   call      'TNTUDTAR'
004800051229     c                   parm                    tntudtads
004900051214     c*
005000051214     c                   seton                                        lr
005100051219     C**************************************************************************
005200051219     C* compongo istruzione sql
005300051219     C**************************************************************************
005400051219     C     istruzione    BEGSR
005500051219     **?Imposto la parte fissa dell'istruzione.
005600051219     C                   EVAL      wrkGetLista =
005700051220     C                             'SELECT ftdfgs, ftdddc, ftdndc,  -
005800060120     C                             ftdcas, ftdifp FROM Fiftt01l join  -
005900060120     C                             fiftd00f on fttfgs = ftdfgs and  -
006000060120     C                             fttndc = ftdndc and fttddc = ftdddc -
006100060206     C                             where ftdfgs <> 0 and ftdddc <> 0 and -
006200060120     C                             ftdtsr = ''C'' AND fttdtr BETWEEN ' +
006300051219     C                             %EDITC(t05dti:'X') + ' AND ' +
006400051219     C                             %EDITC(t05dtf:'X')
006500051219     **?Se passato p.o.
006600051219     C                   if        t05fil <> 0
006700051219     C                   EVAL      wrkGetLista = wrkGetLista +
006800051219     C                             ' AND ftdfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
006900051219     C                   END
007000051219     **?Ordinamento.
007100051219     C                   EVAL      wrkGetLista = wrkGetLista +
007200051219     C                             ' ORDER BY ftdfgs, ftdddc, ftdndc'
007300051219     **?Solo lettura.
007400051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
007500051219     C
007600051219     C/EXEC SQL
007700051219     C+ PREPARE s1 FROM :wrkGetLista
007800051219     C/END-EXEC
007900051219     C
008000051219     C/EXEC SQL
008100051219     C+ DECLARE A1 CURSOR FOR S1
008200051219     C/END-EXEC
008300051219     C
008400051219     C/EXEC SQL
008500051219     C+ OPEN a1
008600051219     C/END-EXEC
008700051219     c                   endsr
008800051214     c**********************************************************************
008900051214     c     srdatadist    begsr
009000051214     c**********************************************************************
009100051214     c                   exsr      istruzione
009200051214
009300051129     C                   DO        *hival
009400051129     C/EXEC SQL
009500051220     C+ FETCH NEXT FROM a1 INTO :fgs, :ddc, :ndc, :cas, :ifp
009600051129     C/END-EXEC
009700051129     C                   SELECT
009800051129     c*
009900051129     C                   WHEN      SqlCod = 100
010000051219     c                   if        sped60<>0 or dist130<>0 or sped220<>0
010100051214     c                   exsr      sraggio
010200051219     c                   end
010300051129     c                   leave
010400051129     C                   WHEN      SqlCod < 0
010500051129     C                   seton                                        H1
010600051129     c                   exsr      srfine
010700051129     c                   other
010800051214     c                   exsr      srconta
010900051129     C                   ENDSL
011000051129      *
011100051129     C                   ENDDO
011200051129     C/EXEC SQL
011300051219     C+ CLOSE a1
011400051129     C/END-EXEC
011500051214     c                   endsr
011600051219     C**************************************************************************
011700051219     C* compongo istruzione sql
011800051219     C**************************************************************************
011900051219     C     istruzione1   BEGSR
012000051219     **?Imposto la parte fissa dell'istruzione.
012100051219     C                   EVAL      wrkGetLista =
012200051222     C                             'SELECT ftdfgs, ftddcm, count(*) -
012300060120     C                             FROM Fiftt01l join  -
012400060120     C                             fiftd00f on fttfgs = ftdfgs and  -
012500060120     C                             fttndc = ftdndc and fttddc = ftdddc -
012600060120     C                             where ftddcm <> 0 and ftdfgs <> 0 -
012700060206     C                             and ftdtsr = ''C'' -
012800060206     C                             AND fttdtr BETWEEN ' +
012900051219     C                             %EDITC(t05dti:'X') + ' AND ' +
013000051219     C                             %EDITC(t05dtf:'X')
013100051219     **?Se passato p.o.
013200051219     C                   if        t05fil <> 0
013300051219     C                   EVAL      wrkGetLista = wrkGetLista +
013400051219     C                             ' AND ftdfgs = ' + %TRIML(%EDITC(t05fil:'Z'))
013500051219     C                   END
013600051222     **?Raggruppamento
013700051219     C                   EVAL      wrkGetLista = wrkGetLista +
013800051222     C                             ' GROUP BY ftdfgs, ftddcm'
013900051222     **?Ordinamento.
014000051222     C                   EVAL      wrkGetLista = wrkGetLista +
014100051222     C                             ' ORDER BY ftdfgs, ftddcm'
014200051219     **?Solo lettura.
014300051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
014400051219     C
014500051219     C/EXEC SQL
014600051219     C+ PREPARE s2 FROM :wrkGetLista
014700051219     C/END-EXEC
014800051219     C
014900051219     C/EXEC SQL
015000051219     C+ DECLARE a2 CURSOR FOR s2
015100051219     C/END-EXEC
015200051219     C
015300051219     C/EXEC SQL
015400051219     C+ OPEN a2
015500051219     C/END-EXEC
015600051219     c                   endsr
015700051214     c**********************************************************************
015800051214     c     srdatacons    begsr
015900051214     c**********************************************************************
016000051214     c                   exsr      istruzione1
016100051214
016200051214     C                   DO        *hival
016300051214     C/EXEC SQL
016400051222     C+ FETCH NEXT FROM a2 INTO :fgs, :dcm, :conta
016500051214     C/END-EXEC
016600051214     C                   SELECT
016700051214     c*
016800051214     C                   WHEN      SqlCod = 100
016900051214     c                   leave
017000051214     C                   WHEN      SqlCod < 0
017100051214     C                   seton                                        H1
017200051214     c                   exsr      srfine
017300051214     c                   other
017400051222     c                   eval      mult = '070'
017500051222     c                   move      dcm           dtaiso
017600051222     c                   eval      comfgs = fgs
017700051222     c                   exsr      aggio
017800051214     C                   ENDSL
017900051214      *
018000051214     C                   ENDDO
018100051214     C/EXEC SQL
018200051219     C+ CLOSE a2
018300051214     C/END-EXEC
018400051214     c                   endsr
018500051129     c**********************************************************************
018600051214     c     srconta       begsr
018700051129     c**********************************************************************
018800051214     c* a rottura di p.o. e data distinta aggiorno
018900051219     c                   movel     fgs           rotto
019000051219     c                   move      ddc           rotto
019100051219     c                   movel     rotto         dist
019200051219     c                   move      ndc           dist
019300051219     c                   if        rotto <> rottos and rottos <> 0
019400051214     c                   exsr      sraggio
019500051214     c                   clear                   sped60
019600051219     c                   clear                   dist130
019700051214     c                   clear                   sped220
019800051214     c                   end
019900051219     c                   eval      rottos = rotto
020000051214     c* calcolo il nr. sped. in consegnate e il nr. distinte in consegna
020100051214     c                   add       1             sped60
020200051214     c                   if        dist <> dists
020300051214     c                   add       1             dist130
020400051214     c                   eval      dists = dist
020500051220     c                   end
020600051214     c* calcolo il numero dei c/assegni incassati
020700051214     c                   if        cas > 0 or ifp > 0
020800051214     c                   add       1             sped220
020900051214     c                   end
021000051214     c                   endsr
021100051214     c**********************************************************************
021200051214     c     sraggio       begsr
021300051214     c**********************************************************************
021400051219     c                   movel     rottos        comfgs
021500051219     c                   move      rottos        comdta
021600051219     c                   move      comdta        dtaiso
021700051214     c                   if        t05mult <> ' '
021800051214     c                   eval      mult = t05mult
021900051214     c                   select
022000051219     c                   when      t05mult = '060'
022100051214     c                   eval      conta= sped60
022200051214     c                   when      t05mult = '130'
022300051214     c                   eval      conta= dist130
022400051214     c                   when      t05mult = '220'
022500051214     c                   eval      conta= sped220
022600051219     c                   endsl
022700051219     c                   if        conta <> 0
022800051219     c                   exsr      aggio
022900051219     c                   end
023000051214     c                   else
023100051214     c* sped. in consegna
023200051214     c                   eval      mult = '060'
023300051214     c                   eval      conta= sped60
023400051219     c                   if        conta <> 0
023500051214     c                   exsr      aggio
023600051219     c                   end
023700051214     c* dist. in consegna
023800051214     c                   eval      mult = '130'
023900051214     c                   eval      conta= dist130
024000051219     c                   if        conta <> 0
024100051214     c                   exsr      aggio
024200051219     c                   end
024300051214     c* c/ass. incassati
024400051214     c                   eval      mult = '220'
024500051214     c                   eval      conta= sped220
024600051219     c                   if        conta <> 0
024700051214     c                   exsr      aggio
024800051214     c                   end
024900051219     c                   end
025000051214     c* pulisco i contatori
025100051214     c                   endsr
025200051214     c**********************************************************************
025300051214     c     aggio         begsr
025400051214     c**********************************************************************
025500060120     c* se esiste il record non è un errore in quanto elaboro per data
025600060120     c* chiusura distinta e aggiorno per data distinta.
025700060120     c* es: distinta del 01/01/06 chiusa il 20/01/06
025800060120     c* i record nel fiftd vengono scritti il 20/01/06 quando l'elaborazione
025900060120     c* del moltiplicatore con periodo 01/01/06 al 08/01/06 è già elaborata
026000051129     c     keytud        chain     tntud01l
026100051129     c                   if        %found(tntud01l)
026200060120     c*                  exsr      srstampa
026300060120     c                   add       conta         tudqta
026400051129     c                   update    tntud000
026500051129     c                   else
026600060120     c                   eval      tudqta = conta
026700051214     c                   eval      tudmult = mult
026800051130     c                   eval      tuddta  = dtaiso
026900051219     c                   eval      tudfil  = comfgs
027000051129     c                   clear                   tudcli
027100051129     c                   write     tntud000
027200051129     c                   end
027300051129     c                   endsr
027400051129     c**********************************************************************
027500051129     c     *inzsr        begsr
027600051129     c**********************************************************************
027700051129     C     *ENTRY        PLIST
027800051129     C                   PARM                    kpjba
027900051129     c                   movel     kpjbu         tntu05ds
028000051129     c     keytud        klist
028100051214     c                   kfld                    mult
028200051130     c                   kfld                    dtaiso
028300051219     c                   kfld                    comfgs
028400051229     c                   seton                                        of
028500051129     c                   endsr
028600051129     C**************************************************************************
028700051129     C* esco con codice d'errore
028800051129     C**************************************************************************
028900051129     C     srfine        BEGSR
029000051129     c                   seton                                        lr
029100051129     c                   return
029200051129     c*
029300051129     c                   endsr
029400051229     C**************************************************************************
029500051229     C* stampo anomalia
029600051229     C**************************************************************************
029700051229     C     srstampa      BEGSR
029800051229     c*
029900051229     C   OF              EXCEPT    TESTA
030000051229     C   OF              SETOFF                                       OF
030100051229     C                   EXCEPT    anomalia
030200051229     c*
030300051229     c                   endsr
030400051229     OQSYSPRT   E            TESTA            02
030500051229     O                                           41 'Anomalie nel pgm TNTU10R'
030600051229     O                       PAGE          Z    132
030700051229     O          E            anomalia    1
030800051229     O                                           +0 'Si è eseguito un UPDATE pe-
030900051229     O                                              r il moltiplicatore'
031000051229     O                       tudmult             +1
031100051229     O                                           +1 'P.O.'
031200051229     O                       tudfil              +1
031300051229     O                                           +1 'Data'
031400051229     O                       tuddta              +1
