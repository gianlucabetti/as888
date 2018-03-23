000100070727     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070206     h dftactgrp(*no) actgrp(*caller)
000300070727      * FIOR70R *----------------------------------------------------*
000400070727      *         Assegnaz.distinta ORM automatica
000500070727      *         Aggiorna sequenza lettura ORM e fermata x giro
000600070727      *--------------------------------------------------------------*
000700070913     ffior63p   o    e             PRINTER
000800070727     ffnorg00f  uf   e             disk    commit
000900070727     fazorg01l  if   e           k disk
001000070727     ftntbe01l  if   e           k disk
001100070206      *--------------------------------------------------------------*
001200070206      *   DS
001300070206      *--------------------------------------------------------------*
001400070727     D fnormds       e ds                  extname(fnorm00f)
001500070727     D fnorgds       e ds                  extname(fnorg00f)
001600050520     D ddatiute      e ds
001700030910     D azuteds       e ds                  extname(AZUTE00F)
001800030910     D tibs34ds      E DS                  inz
001900950524     D KPJBA         E DS
002000070727     d Fior70ds      E DS
002100070727     d Fior64ds      E DS
002200070523     d trul82ds      e ds
002300070727     D OG147         E DS                  INZ
002400070919     D dslo          E DS                  inz
002500070919     dcomdar           s               d
002600070919     dcomdfv           s               d
002700070727     drrnorg           s              9  0
002800070727     drrnorm           s              9  0
002900070206     ddtaiso           s               d
003000070206     ddtaeur           s               d   datfmt(*eur)
003100070727     dcomnftl          s                   like(orgnftl)
003200070727     dcomslo           s                   like(orgslo)
003300070727     dsavnftl          s                   like(orgnftl)
003400070727     dsavslo           s                   like(orgslo)
003500070206     ddsrsdind         s             70
003600070206     dsavrsdind        s                   like(dsrsdind)
003700070919     d DAR             s              4  0 dim(100)
003800070919     d STO             s              3  0 dim(100)
003900070919     d PR              s              4  0 dim(100)
004000070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
004100950524     C*--------------------------------------------------------------*
004200950524     C*  Ciclo principale
004300950524     C*--------------------------------------------------------------*
004400070727     c* prima attibuisco la sequenza di lettura orm   e poi il numero della
004500070209     c* fermata
004600070209     c*
004700070727     c* se il p.o. è in parallelo testo la distinta su ORG altrimenti
004800070727     c* se il p.o. è partito testo anche la distinta su ORM
004900070209     c*
005000070727     c* per il momento aggiorno solo ORG perchè ORM lo aggiornerò alla fine
005100070727     c                   if        comdda > dateu8
005200061003     C/EXEC SQL
005300070727     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*,
005400070727     C+ fnorm01l.* FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
005500070727     C+ ormnsr=orgnsr and ormnor= orgnor and ormnrv= orgnrv WHERE orgndc
005600070727     C+ = 0 and orgcgi=:or70cgii and orgpocgi = :or70fgsi and orgslo = 0
005700071029     C+ and ormeti <> ' '
005800061003     C/END-EXEC
005900061003
006000061003     C/EXEC SQL
006100070205     C+ OPEN a1
006200061003     C/END-EXEC
006300070205     c                   else
006400070206     C/EXEC SQL
006500070727     C+ DECLARE B1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*,
006600070727     C+ fnorm01l.* FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
006700070727     C+ ormnsr=orgnsr and ormnor= orgnor and ormnrv= orgnrv WHERE ormndc
006800070727     C+ = 0 and orgcgi=:or70cgii and orgndc = 0 and orgslo = 0
006900071029     C+ and orgpocgi = :or70fgsi and ormeti <> ' '
007000070205     C/END-EXEC
007100070205
007200070205     C/EXEC SQL
007300070205     C+ OPEN b1
007400070205     C/END-EXEC
007500070205     c                   end
007600061003
007700061003     C                   do        *hival
007800061003
007900070727     c                   if        comdda > dateu8
008000061003     C/EXEC SQL
008100070727     C+ FETCH NEXT FROM a1 INTO :rrnorg, :fnorgds, :fnormds
008200061003     C/END-EXEC
008300070205     c                   else
008400070205     C/EXEC SQL
008500070727     C+ FETCH NEXT FROM b1 INTO :rrnorg, :fnorgds, :fnormds
008600070205     C/END-EXEC
008700070205     c                   end
008800061003
008900061003     C                   select
009000061003     C                   when      sqlcod = 100
009100061003     c                   leave
009200070205     C                   WHEN      SqlCod < 0
009300070205     c                   if        sqlcod <> -913
009400070205     C                   seton                                        H1
009500070209     c                   end
009600070222     c                   seton                                        83
009700070222     c                   exsr      srstampa
009800070206     c                   exsr      srfine
009900070206     C                   other
010000070727     C                   exsr      gesorm
010100061003     C                   endsl
010200061003     C*
010300061003     C                   enddo
010400061003
010500070727     c                   if        comdda > dateu8
010600061003     C/EXEC SQL
010700070205     C+ CLOSE a1
010800061003     C/END-EXEC
010900070205     c                   else
011000070205     C/EXEC SQL
011100070205     C+ CLOSE b1
011200070205     C/END-EXEC
011300070205     c                   end
011400070222     c*
011500070727     c* ora che ho la sequenza dell'orm calcolo il numero della fermata
011600070222     c*
011700070209     c                   eval      comnftl = 1
011800070727     c                   if        comdda > dateu8
011900070209     C/EXEC SQL
012000070727     C+ DECLARE C1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*,
012100070727     C+ fnorm01l.* FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
012200070727     C+ ormnsr=orgnsr and ormnor= orgnor and orgnrv= orgnrv WHERE orgndc
012300070727     C+ = 0 and orgcgi=:or70cgii and orgslo <> 0 and orgpocgi = :or70fgsi
012400070727     C+ ORDER BY ormrsr, orminr
012500070209     C/END-EXEC
012600070209
012700070209     C/EXEC SQL
012800070209     C+ OPEN c1
012900070209     C/END-EXEC
013000070209     c                   else
013100070209     C/EXEC SQL
013200070727     C+ DECLARE D1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*,
013300070727     C+ fnorm01l.* FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
013400070727     C+ ormnsr=orgnsr and ormnor= orgnor and ormnrv= orgnrv WHERE ormndc
013500070727     C+ = 0 and orgcgi=:or70cgii and orgslo <>0 and orgndc = 0
013600070727     C+ and orgpocgi = :or70fgsi
013700070727     C+ ORDER BY ormrsr, orminr
013800070209     C/END-EXEC
013900070209
014000070209     C/EXEC SQL
014100070209     C+ OPEN d1
014200070209     C/END-EXEC
014300070209     c                   end
014400070209
014500070209     C                   do        *hival
014600070209
014700070727     c                   if        comdda > dateu8
014800070209     C/EXEC SQL
014900070727     C+ FETCH NEXT FROM c1 INTO :rrnorg, :fnorgds, :fnormds
015000070209     C/END-EXEC
015100070209     c                   else
015200070209     C/EXEC SQL
015300070727     C+ FETCH NEXT FROM d1 INTO :rrnorg, :fnorgds, :fnormds
015400070209     C/END-EXEC
015500070209     c                   end
015600070209
015700070209     C                   select
015800070209     C                   when      sqlcod = 100
015900070209     c                   leave
016000070209     C                   WHEN      SqlCod < 0
016100070209     c                   if        sqlcod <> -913
016200070209     C                   seton                                        H1
016300070209     c                   end
016400070222     c                   seton                                        83
016500070222     c                   exsr      srstampa
016600070209     c                   exsr      srfine
016700070209     C                   other
016800070727     C                   exsr      gesorm1
016900070209     C                   endsl
017000070209     C*
017100070209     C                   enddo
017200070209
017300070727     c                   if        comdda > dateu8
017400070209     C/EXEC SQL
017500070209     C+ CLOSE c1
017600070209     C/END-EXEC
017700070209     c                   else
017800070209     C/EXEC SQL
017900070209     C+ CLOSE d1
018000070209     C/END-EXEC
018100070209     c                   end
018200070222     c*
018300070727     c* ora che ho la sequenza dell'ORM    e il numero della fermata
018400070727     c* attribuisco la stessa sequenza ORM   a parità di fermata
018500070222     c*
018600070222     c                   clear                   savnftl
018700070727     c                   clear                   savslo
018800070727     c                   if        comdda > dateu8
018900070222     C/EXEC SQL
019000070727     C+ DECLARE e1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*
019100070727     C+ FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
019200070727     C+ ormnsr=orgnsr and ormnor= orgnor and ormnrv= orgnrv WHERE orgndc
019300070727     C+ = 0 and orgcgi=:or70cgii and orgslo <> 0 and orgpocgi= :or70fgsi
019400070727     C+ and orgnftl <> 0
019500070919     C+ ORDER BY orgnftl, orgslo desc
019600070222     C/END-EXEC
019700070222
019800070222     C/EXEC SQL
019900070222     C+ OPEN e1
020000070222     C/END-EXEC
020100070222     c                   else
020200070222     C/EXEC SQL
020300070727     C+ DECLARE f1 CURSOR FOR SELECT rrn(fnorg01l), fnorg01l.*
020400070727     C+ FROM fnorg01l join fnorm01l on ormpoe= orgpoe and
020500070727     C+ ormnsr=orgnsr and ormnor= orgnor and ormnrv= orgnrv WHERE ormndc
020600070727     C+ = 0 and orgcgi=:or70cgii and orgslo <>0 and orgndc = 0
020700070727     C+ and orgpocgi = :or70fgsi
020800070727     C+ and orgnftl <> 0
020900070919     C+ ORDER BY orgnftl, orgslo desc
021000070222     C/END-EXEC
021100070222
021200070222     C/EXEC SQL
021300070222     C+ OPEN f1
021400070222     C/END-EXEC
021500070222     c                   end
021600070222
021700070222     C                   do        *hival
021800070222
021900070727     c                   if        comdda > dateu8
022000070222     C/EXEC SQL
022100070727     C+ FETCH NEXT FROM e1 INTO :rrnorg, :fnorgds
022200070222     C/END-EXEC
022300070222     c                   else
022400070222     C/EXEC SQL
022500070727     C+ FETCH NEXT FROM f1 INTO :rrnorg, :fnorgds
022600070222     C/END-EXEC
022700070222     c                   end
022800070222
022900070222     C                   select
023000070222     C                   when      sqlcod = 100
023100070222     c                   leave
023200070222     C                   WHEN      SqlCod < 0
023300070222     c                   if        sqlcod <> -913
023400070222     C                   seton                                        H1
023500070222     c                   end
023600070222     c                   seton                                        83
023700070222     c                   exsr      srstampa
023800070222     c                   exsr      srfine
023900070222     C                   other
024000070727     c                   if        savnftl <> orgnftl
024100070727     c                   eval      savslo = orgslo
024200070727     c                   eval      savnftl = orgnftl
024300070222     c                   else
024400070924     c*                  if        savslo <> 9999
024500070727     C                   exsr      gesorm2
024600070924     c*                  end
024700070222     c                   end
024800070222     C                   endsl
024900070727
025000070222     C                   enddo
025100070222
025200070727     c                   if        comdda > dateu8
025300070222     C/EXEC SQL
025400070222     C+ CLOSE e1
025500070222     C/END-EXEC
025600070222     c                   else
025700070222     C/EXEC SQL
025800070222     C+ CLOSE f1
025900070222     C/END-EXEC
026000070222     c                   end
026100070727
026200070727     c                   if        or70cmti= 'S'
026300070206     c                   commit
026400070206     c                   end
026500070727     c                   eval      or70pag = pages
026600070727     c                   eval      or70riga= riga
026700070727
026800070727     c                   eval      kpjbu = fior70ds
026900070727     c                   seton                                        LR
027000070727      *--------------------------------------------------------------*
027100070727      *  Gestione singolo ORM
027200070727      *--------------------------------------------------------------*
027300070727     c     GESorm        BEGSR
027400070727      * chiamo pgm per vedere se ORM da mettere in distinta
027500070727     c                   exsr      srcheck
027600070727      * stampo l'ORM escluso dall'assegnazione
027700070801     c                   if        or64asso = 'N'
027800070727     c                   if        or64msgo <> ' '
027900070315     c                   seton                                        85
028000070727     c                   eval      prtpoe = or64poei
028100070727     c                   eval      prtnsr = or64nsri
028200070727     c                   eval      prtnor = or64nori
028300070727     c                   eval      prtnrv = or64nrvi
028400070727     c                   eval      prtmsg = or64msgo
028500070315     c                   exsr      srstampa
028600070329     c                   end
028700070206     c                   else
028800070727      * se ORM da mettere in distinta
028900070727      * aggancio ORG
029000070727     c     RRNORG        chain(e)  fnorg00f
029100070412     c                   if        %error
029200070523     c                   exsr      geslck
029300070523     c                   if        ul82§sts = 'A'
029400070704     c                   seton                                        85
029500070727     c                   eval      prtpoe = orgpoe
029600070727     c                   eval      prtnsr = orgnsr
029700070727     c                   eval      prtnor = orgnor
029800070727     c                   eval      prtnrv = orgnrv
029900070727     C                   eval      prtmsg = 'L''ORM non è stata aggiorna-
030000070727     C                             to correttamente.VERIFICARLO'
030100070412     c                   exsr      srstampa
030200070412     c                   leavesr
030300070528     c                   else
030400070727     c     RRNorg        chain     fnorg00f
030500070412     c                   end
030600070523     c                   end
030700070727     c                   if        not %found(fnorg00f)
030800070412     c                   seton                                        84
030900070412     c                   exsr      srstampa
031000070412     c                   leavesr
031100070412     c                   else
031200070727
031300070924      * calcolo la sequenza di lettura ORM
031400070727     c                   exsr      srslo
031500070727     c                   eval      orgslo = comslo
031600070727     c                   eval      orgndc = 0
031700070727     c                   eval      orgpdc = 0
031800070727     c                   eval      orgfgs = 0
031900070727     c                   eval      orgddc = 0
032000070727     c                   eval      orgnftl= 0
032100070727     c                   eval      orgdtvdis= 0
032200070727     c                   eval      orghvdis= 0
032300070727     c                   update    fnorg000
032400070205     c                   end
032500070206     c                   end
032600070727      *
032700070727     c                   ENDSR
032800070727      *--------------------------------------------------------------*
032900070727      *  Gestione singolo ORM
033000070727      *--------------------------------------------------------------*
033100070727     c     GESorm1       BEGSR
033200070727      * calcolo la fermata
033300070727     c                   movel     ormrsr        dsrsdind
033400070727     c                   move      orminr        dsrsdind
033500070209     c                   if        dsrsdind<>savrsdind and savrsdind<>' '
033600070209     c                   add       1             comnftl
033700070209     c                   end
033800070209     c                   eval      savrsdind = dsrsdind
033900070727      * aggiorno ORG
034000070727     c     RRNORG        chain(e)  fnorg00f
034100070209     c                   if        %error
034200070523     c                   exsr      geslck
034300070523     c                   if        ul82§sts = 'A'
034400070704     c                   seton                                        85
034500070727     c                   eval      prtpoe = orgpoe
034600070727     c                   eval      prtnsr = orgnsr
034700070727     c                   eval      prtnor = orgnor
034800070727     c                   eval      prtnrv = orgnrv
034900070727     C                   eval      prtmsg = 'L''ORM non è stata aggiorna-
035000070727     C                             to correttamente.VERIFICARLO'
035100070222     c                   exsr      srstampa
035200070209     c                   leavesr
035300070528     c                   else
035400070727     c     RRNORG        chain     fnorg00f
035500070209     c                   end
035600070523     c                   end
035700070727
035800070727     c                   if        %found(fnorg00f)
035900070727     c                   eval      orgnftl= comnftl
036000070727     c                   update    fnorg000
036100070209     c                   else
036200070222     c                   seton                                        84
036300070209     c                   exsr      srstampa
036400070209     c                   leavesr
036500070209     c                   end
036600070727
036700070727     c                   endsr
036800070727      *--------------------------------------------------------------*
036900070727      *  Gestione singolo ORM
037000070727      *--------------------------------------------------------------*
037100070727     c     GESorm2       BEGSR
037200070727      * aggiorno ORG
037300070727     c     RRNORG        chain(e)  fnorg00f
037400070222     c                   if        %error
037500070523     c                   exsr      geslck
037600070523     c                   if        ul82§sts = 'A'
037700070704     c                   seton                                        85
037800070727     c                   eval      prtpoe = orgpoe
037900070727     c                   eval      prtnsr = orgnsr
038000070727     c                   eval      prtnor = orgnor
038100070727     c                   eval      prtnrv = orgnrv
038200070727     C                   eval      prtmsg = 'L''ORM non è stata aggiorna-
038300070727     C                             to correttamente.VERIFICARLO'
038400070222     c                   exsr      srstampa
038500070222     c                   leavesr
038600070528     c                   else
038700070727     c     RRNORG        chain     fnorg00f
038800070222     c                   end
038900070523     c                   end
039000070727
039100070727     c                   if        %found(fnorg00f)
039200070727     c                   eval      orgslo = savslo
039300070727     c                   update    fnorg000
039400070222     c                   else
039500070222     c                   seton                                        84
039600070222     c                   exsr      srstampa
039700070222     c                   leavesr
039800070222     c                   end
039900070727
040000070222     C                   ENDSR
040100061003      *-----------------------------------------------
040200070727     C     srcheck       begsr
040300061003      *-----------------------------------------------
040400070727
040500070727      * per controllare se ORM assegnabile richiamo pgm esterno
040600070727     c                   clear                   fior64ds
040700070727     c                   eval      or64poei = ormpoe
040800070727     c                   eval      or64nsri = ormnsr
040900070727     c                   eval      or64nori = ormnor
041000070727     c                   eval      or64nrvi = ormnrv
041100070801      * deve essere impostata la data
041200070801     c                   eval      or64dfvi = or70dfvi
041300070801     c                   eval      or64dari = 'S'
041400070727     c                   if        dateu8 >= comdda
041500071011     c                   eval      or64ndci = 'E'
041600070727     c                   else
041700071011     c                   eval      or64ndci = 'G'
041800071011     c                   eval      or64etii = 'S'
041900070727     c                   endif
042000070727     c                   call      'FIOR64R'
042100070727     c                   parm                    kpjba
042200070727     c                   parm                    fior64ds
042300070727
042400070727     c                   endsr
042500070205      *-----------------------------------------------
042600070727     C     srslo         begsr
042700070205      *-----------------------------------------------
042800070920     c                   eval      comslo = 1
042900070919      * imposto sequenza in base alla priorità dell'ORM
043000070919     c                   z-add     1             x
043100070919     c     ormsto        lookup    sto(x)                                 30
043200070919     c                   If        %found
043300070920     c                   eval      comslo = (comslo + pr(x))
043400070919     c                   endif
043500070919      * verifico quanti giorni intercorrono da data ritiro e data distinta
043600070919     c                   if        ormdar > 0 and ormdar < or70dfvi
043700070919     c                   z-add     1             y
043800070919     c                   move      or70dfvi      comdfv
043900070919     c                   move      ormdar        comdar
044000070919     c     comdfv        subdur    comdar        gg:*D             4 0
044100070919      * xx = nr. elementi tabella DAR
044200070919     c                   if        gg > xx
044300070919     c                   eval      y = xx
044400070919     c                   else
044500070919     c                   eval      y = gg
044600070919     c                   end
044700070919     c* al massimo 9999
044800070919     c                   if        comslo + dar(y) <= 9999
044900070919     c                   add       dar(y)        comslo
045000070919     c                   else
045100070919     c                   z-add     9999          comslo
045200070919     c                   end
045300070919     c                   end
045400070727
045500070727     c                   ENDSR
045600070727      *--------------------------------------------------------------*
045700070727      *  Operazioni finali in caso di errore
045800070727      *--------------------------------------------------------------*
045900070727     c     srfine        BEGSR
046000070727     c                   if        or70cmti= 'S'
046100070206     c                   rolbk
046200070206     c                   end
046300070727     c                   eval      or70erro= '1'
046400070727     c                   eval      kpjbu = fior70ds
046500070206     c                   seton                                        lr
046600070206     c                   return
046700070206     c                   endsr
046800070727      *------------------------------------------------------------------------*
046900070727      * stampa
047000070727      *------------------------------------------------------------------------*
047100070727     c     srstampa      BEGSR
047200070727     c                   move      or70dfvi      dtaiso
047300070222     c                   move      dtaiso        dtaeur
047400070222     c                   move      dtaeur        prtdfv
047500070727      * PAGINA IN STAMPA
047600070323     C                   IF        *IN80 OR RIGA >=60
047700070323     C                   SETON                                        80
047800070323     C                   ADD       1             PAGES
047900070323     C                   Z-ADD     3             RIGA              3 0
048000070323     C                   END
048100070727
048200070222     c   80              write     testa
048300070222     c                   setoff                                       80
048400070222     c   83              write     riga3
048500070323     c   83              add       1             riga
048600070222     c   84              write     riga4
048700070323     c   84              add       1             riga
048800070305     c   85              write     riga5
048900070323     c   85              add       1             riga
049000070305     c                   setoff                                       838485
049100070727
049200070222     c                   endsr
049300070727      *--------------------------------------------------------------*
049400070727      *  Operazioni iniziali
049500070727      *--------------------------------------------------------------*
049600070727     c     *INZSR        BEGSR
049700070727
049800070727     c     *ENTRY        PLIST
049900070727     c                   PARM                    KPJBA
050000070727     c                   MOVEL(P)  KPJBU         Fior70ds
050100070727     c                   eval      pages = or70pag
050200070727     c                   eval      riga  = or70riga
050300070727     c                   clear                   or70erro
050400070727
050500030718     c     *dtaara       define    §azute        azuteds
050600030718     c     *dtaara       define    §datiute      ddatiute
050700070727     c                   in(E)     *dtaara
050800070727     c                   IF        %Error  or  RSUT = *blanks
050900070727     c                   call      'TIBS34R'
051000070727     c                   parm                    Tibs34Ds
051100070727     c                   in        *dtaara
051200030718     c                   ENDIF
051300070727
051400070727     c                   TIME                    WHHUDT           14 0
051500070727     c                   MOVEL     WHHUDT        ORA               6 0
051600070727     c                   MOVE      WHHUDT        UDATE8            8 0
051700070510     c                   move      udate8        dtaeur
051800070510     c                   move      dtaeur        dtaiso
051900070510      *
052000070206     c                   move      dtaiso        dateu8            8 0
052100070727     c     or70fgsi      CHAIN     AZORG01l
052200070727    1c                   IF        %found(azorg01l)
052300070727     c                   MOVEL     ORGde7        og147
052400070727      * se data distinte automatiche < oggi p.o. partito
052500070727     c                   if        §ogddao = ' '
052600070205     c                   exsr      srfine
052700070205     c                   else
052800070727     c                   move      §ogddao       comdda            8 0
052900070205     c                   end
053000070205     c                   end
053100070919      *  tntbe01l
053200070919     c     Ktbe1         klist
053300070919     c                   kfld                    tbecod
053400070919     c                   kfld                    tbeke1
053500070919
053600070919     c     Ktbe          klist
053700070919     c                   kfld                    tbecod
053800070919     c                   kfld                    tbeke1
053900070919     c                   kfld                    tbeke2
054000070919
054100070919      * carico schiera DAR e PR
054200070919     c                   exsr      srsch
054300070727
054400070727     c                   endsr
054500070919      *----------------------------------------------------*
054600070919     c     srsch         begsr
054700070919      *----------------------------------------------------*
054800070919      * STO + PR
054900070919     c                   z-add     0             y                 4 0
055000070919     c                   eval      tbecod = 'SLO'
055100070919     c                   eval      tbeke1 = 'PR '
055200070919     c     ktbe1         setll     tntbe01l
055300070919     c                   do        *hival
055400070919     c     ktbe1         reade     tntbe01l
055500070919     c                   if        %eof(tntbe01l)
055600070919     c                   leave
055700070919     c                   end
055800070919     c                   if        tbeatb <> ' '
055900070919     c                   iter
056000070919     c                   end
056100070919     c                   eval      dslo = tbeuni
056200070919     c                   z-add     1             x                 4 0
056300070919     c                   movel     tbeke2        comsto            3 0
056400070919     c     comsto        lookup    sto(x)                                 30
056500070919     c                   If        not %found
056600070919     c                   add       1             y
056700070920     c                   eval      sto(y) = comsto
056800070919     c                   eval      pr(y)  = d§sloprg
056900070919     c                   endif
057000070919     c                   enddo
057100070919     c* DAR
057200070919     c                   eval      tbecod = 'SLO'
057300070919     c                   eval      tbeke1 = 'DAR'
057400070919     c                   clear                   xx                4 0
057500070919     c     ktbe1         setll     tntbe01l
057600070919     c                   do        *hival
057700070919     c     ktbe1         reade     tntbe01l
057800070919     c                   if        %eof(tntbe01l)
057900070919     c                   leave
058000070919     c                   end
058100070919     c                   if        tbeatb <> ' '
058200070919     c                   iter
058300070919     c                   end
058400070919     c                   add       1             xx
058500070919     c                   eval      dslo = tbeuni
058600070919     c                   eval      dar(xx) = d§sloprg
058700070919     c                   enddo
058800070919
058900070919     c                   ENDSR
059000070727      *----------------------------------------------------*
059100070919     c     geslck        begsr
059200070727      *----------------------------------------------------*
059300070523     c                   clear                   trul82ds
059400070727     c                   Eval      ul82§rrn = rrnorg
059500070727     c                   Eval      UL82§FIL = 'FNORG00F'
059600070523     c                   Eval      UL82§WIN = 'S'
059700070523     c                   Eval      UL82§num = 2
059800070523     c                   Eval      UL82§att = 2
059900070523     c                   eval      ul82§mss = ErrMsg(1)
060000070727     c                   Eval      UL82§msw = 'L''ORM '
060100070727     c                             + %editc(orgpoe:'Z') +  ' '
060200070727     c                             + %editc(orgnsr:'Z') +  ' '
060300070727     c                             + %editc(orgnor:'Z') +  ' '
060400070727     c                             + %editc(orgnrv:'Z') +  ' è già utilizzato -
060500070523     c                             da un altro lavoro e non è manutenzionabile'
060600070523      * Effettuo la chiamata al *PGM d utilità
060700070523     c                   Call(e)   'TRUL82R'
060800070523     c                   Parm                    Trul82ds
060900070523
061000070523     C                   ENDSR
061100070523** ErrMsg
061200070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
