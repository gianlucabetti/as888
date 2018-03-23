000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000301071127     Fanrco98j  if   e           k disk
000800030723     Fprtf198   O    F  198        PRINTER prtctl(prtds)
000801071127     d trulcffds     e ds
000802071128     d* Parametri x Controllo profilo utenti
000803071128     d DDatiUte      e ds
000804071128     d TIBS34ds      e ds
000805071128     d AZUTEds       e ds                  extname(AZUTE00F)
000900020304     dkpjba          e ds
001200030723     D PARAM           DS
001201071127     d  data1                         8  0
001202071127     d  data2                         8  0
001203071127     d prtds           ds
001204071127     d   spab                         3  0
001205071127     d   spaa                         3  0
001206071127     d   skab                         3  0
001207071127     d   skaa                         3  0
001208071127     d   line                         3  0
002900020305     d dataiso         s               d   datfmt(*iso)
003000020305     d dataeur         s               d   datfmt(*eur)
003001071127     d soc             s                   like(rcosocieta)
003002071127     d for             s                   like(rcoksc)
003003071127     d prestazione     s             20
003100000000     C*---------------------------------------------------------------*
003101071127     c*
003102071127     c* prestazioni residuali
003103071127     c*
003104071127     C/EXEC SQL
003105071127     C+ DECLARE A1 CURSOR FOR
003106071127     C+ SELECT ottsoc, ottcdf FROM fiott06l WHERE
003107071127     C+ ottddc BETWEEN :data1 AND :data2 and ottnff = 0 GROUP BY
003108071127     C+ ottsoc, ottcdf ORDER BY ottsoc, ottcdf
003109071127     C/END-EXEC
003110071127     C/EXEC SQL
003111071127     C+ OPEN a1
003112071127     C/END-EXEC
003113071127     C                   do        *hival
003114071127     C/EXEC SQL
003115071127     C+ FETCH NEXT FROM a1 INTO :soc, :for
003116071127     C/END-EXEC
003118071127     C                   select
003119071127     C                   when      sqlcod = 100
003120071127     c                   leave
003121071127     C                   WHEN      SqlCod < 0
003129071127     c                   other
003130071127     c                   exsr      srctr
003137071127     C                   endsl
003139071127     C                   enddo
003141071127     C/EXEC SQL
003142071127     C+ CLOSE a1
003143071127     C/END-EXEC
003144071127     c*
003145071127     c                   z-add     99            line
003146071127     c                   eval      prestazione = 'AUTOTRASPORTATORI'
003147071127     c* autotrasportatori
003148071127     c*
003149071127     C/EXEC SQL
003150071127     C+ DECLARE b1 CURSOR FOR
003151071127     C+ SELECT fttsoc, fttcdf  FROM fiftt06l WHERE fttddc
003152071127     C+ BETWEEN :data1 AND :data2  and fttnff = 0 and fttfvl = 'C' and
003153071127     C+ ftttsr = ' ' GROUP BY fttsoc, fttcdf ORDER BY fttsoc, fttcdf
003154071127     C/END-EXEC
003155071127     C/EXEC SQL
003156071127     C+ OPEN b1
003157071127     C/END-EXEC
003158071127     C                   do        *hival
003159071127     C/EXEC SQL
003160071127     C+ FETCH NEXT FROM b1 INTO :soc, :for
003161071127     C/END-EXEC
003163071127     C                   select
003164071127     C                   when      sqlcod = 100
003165071127     c                   leave
003166071127     C                   WHEN      SqlCod < 0
003167071127     c                   other
003168071127     c                   exsr      srctr
003169071127     C                   endsl
003170071127     C                   enddo
003172071127     C/EXEC SQL
003173071127     C+ CLOSE b1
003174071127     C/END-EXEC
003175071127     c                   z-add     99            line
003176071127     c                   eval      prestazione = 'COOPERATIVE'
003177071127     c*
003178071127     c* cooperative
003179071127     c*
003180071127     C/EXEC SQL
003181071127     C+ DECLARE c1 CURSOR FOR
003182071127     C+ SELECT cttsoc, cttcdf FROM fictt06l WHERE cttddc
003183071127     C+ BETWEEN :data1 AND :data2 and cttnff = 0 and cttfvl = 'C' GROUP
003184071127     C+ BY cttsoc, cttcdf ORDER BY cttsoc, cttcdf
003185071127     C/END-EXEC
003186071127     C/EXEC SQL
003187071127     C+ OPEN c1
003188071127     C/END-EXEC
003189071127     C                   do        *hival
003190071127     C/EXEC SQL
003191071127     C+ FETCH NEXT FROM c1 INTO :soc, :for
003192071127     C/END-EXEC
003194071127     C                   select
003195071127     C                   when      sqlcod = 100
003196071127     c                   leave
003197071127     C                   WHEN      SqlCod < 0
003198071127     c                   other
003199071127     c                   exsr      srctr
003200071127     C                   endsl
003201071127     C                   enddo
003203071127     C/EXEC SQL
003204071127     C+ CLOSE c1
003205071127     C/END-EXEC
003206071127     c                   z-add     99            line
003207071127     c                   eval      prestazione = 'AFFLUENZE/DEFLUENZE'
003208071127     c*
003209071127     c* aff/defl.
003210071127     c*
003211071127     C/EXEC SQL
003212071127     C+ DECLARE d1 CURSOR FOR
003213071127     C+ SELECT attsoc, attcdf FROM fiatt06l WHERE attddc
003214071127     C+ BETWEEN :data1 AND :data2 and attnff = 0 and attdco<> 0 GROUP
003215071127     C+ BY attsoc, attcdf ORDER BY attsoc, attcdf
003216071127     C/END-EXEC
003217071127     C/EXEC SQL
003218071127     C+ OPEN d1
003219071127     C/END-EXEC
003220071127     C                   do        *hival
003221071127     C/EXEC SQL
003222071127     C+ FETCH NEXT FROM d1 INTO :soc, :for
003223071127     C/END-EXEC
003225071127     C                   select
003226071127     C                   when      sqlcod = 100
003227071127     c                   leave
003228071127     C                   WHEN      SqlCod < 0
003229071127     c                   other
003230071127     c                   exsr      srctr
003231071127     C                   endsl
003232071127     C                   enddo
003234071127     C/EXEC SQL
003235071127     C+ CLOSE d1
003236071127     C/END-EXEC
012500020304
012601071127     c                   seton                                        lr
012700981029      *
012800011026     c*-------------------------------------------------
012900071127     c     srctr         begsr
013000011026     c*-------------------------------------------------
013001071127     c* ctr il fornitore di fatturazione
013002071127     c                   clear                   trulcffds
013003071127     c                   eval      icfftpa = 'C'
013004071127     c                   eval      ICFFSOC = soc
013005071127     c                   eval      ICFFKSC = for
013006071127     c                   call      'TRULCFFR'
013007071127     c                   parm                    trulcffds
013008071127     c                   if        ocfferr <> ' '
013009071127     c     krco          chain     anrco98j
013010071127     c                   if        not %found(anrco98j)
013011071127     c                   eval      sogdes = *all'?'
013012071127     c                   eval      sogpartiva = *all'?'
013013071127     c                   end
013014071127     c                   if        line > 60
013015071127     c                   except    testata
013016071127     c                   endif
013017071127     c                   except    detta
013018071127     c                   endif
013100071127     c                   endsr
013102071127     c*-------------------------------------------------
013103071127     c     *inzsr        begsr
013104071127     c*-------------------------------------------------
013105071127     c     *entry        plist
013200020304     c                   parm                    kpjba
013300030723     c                   movel     kpjbu         param
013301071127     c                   move      data1         dataiso
013302071127     c                   move      dataiso       dataeur
013303071127     c                   move      dataeur       datadal           8 0
013304071127     c                   move      data2         dataiso
013305071127     c                   move      dataiso       dataeur
013306071127     c                   move      dataeur       dataal            8 0
013307071127     c                   z-add     99            line
013308071127     c                   eval      prestazione = 'PRESTAZ.OCCASIONALI'
013309071127     c     krco          klist
013310071127     c                   kfld                    soc
013311071127     c                   kfld                    knatura           1
013312071127     c                   kfld                    for               8
013313071127     c                   movel     'F'           knatura
016400030725
016401071128     c     *dtaara       define    §azute        azuteds
016402071128     c     *dtaara       define    §datiute      ddatiute
016403071128      *
016404071128     c                   in(E)     *dtaara
016405071128     c                   IF        %ERROR or RSUT = *blanks
016406071128     c                   clear                   Tibs34Ds
016407071128     c                   call      'TIBS34R'
016408071128     c                   parm                    Tibs34Ds
016409071128     c                   in        *dtaara
016410071128     c                   ENDIF
017000011026     c                   endsr
017100020404     C*--------------------------------------------------------------------
017200030723     Oprtf198   E            testata        1 02
017300071127     o                                            7 'FICN87R'
017400020404     o                       knmus               22
017500071127     o                                        +   5 'Errori codici fornitori'
017600071127     o                                        +   1 'di fatturazione dal'
018300030723     o                       datadal          +   2 '  /  /    '
018400030723     o                                        +   2 'al'
018500030723     o                       dataal           +   2 '  /  /    '
018501071127     o                       udate         y  +  12
018502071127     o                                        +   1 'Pag.'
018503071127     O                       page          z  +   1
018504071127     O          E            testata     1
018505071127     o                       prestazione      +   0
018506071127     O          E            testata     1
018507071127     o                                            9 'Fornitore'
018508071127     o                                           24 'Descrizione'
018509071127     o                                           69 'Partita IVA'
020000071127     O          E            detta       1
020100071127     o                       soc              +   0
020200071127     o                       for              +   1
020300071127     o                       sogdes           +   1
020400071127     o                       sogpartiva       +   1
