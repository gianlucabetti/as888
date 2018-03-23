000100031030     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200071019     h dftactgrp(*no) actgrp(*caller)
000300031017
000400071018      *------------------------------------------------------------------------*
000500071018      *                                                                        *
000600071018      *       Sistema dati per GEO ORM                                         *
000700071018      *                                                                        *
000800071018      *------------------------------------------------------------------------*
000900071019     fazorg01l  if   e           k disk
001000071018     ffidst01l  if   e           k disk
001100071018     ffnorm01l  uf   e           k disk
001200071018     ffnorg01l  uf   e           k disk
001300080206     ffnorf01l  if   e           k disk    prefix(w)
001400070308
001500070308      *------------------------------------------------------------------------*
001600070926      *?I N D I C A T O R I
001700070926
001800070926      *?V A R I A B I L I
001900070927     d data_org        s              8  0 inz
002000071011     d oggi            s              8  0 inz
002100080205     d savfao          s                   like(ormfao)
002200080205     d savdfo          s                   like(ormdfo)
002300080205     d savofo          s                   like(ormofo)
002400031017
002500070926      *?D S   I N T E R N E / E S T E R N E
002600071018     d fidstds       e ds                  extname(fidst00f)
002700071018     d fior67ds      e ds
002800071018     d fnorfds       e ds                  extname(fnorf00f)
002900071011     d fnorgds       e ds                  extname(fnorg00f)
003000070927     d fnormds       e ds                  extname(fnorm00f)
003100071018     d kpjba         e ds
003200070927     d og147         e ds
003300070926
003400070308      *------------------------------------------------------------------------*
003500071019
003600071019     c/exec sql
003700071019     c+ set option dynusrprf = *owner, closqlcsr = *endmod
003800071019     c/end-exec
003900071018
004000071018      *?leggo organigramma per partenza filiale
004100071019     c     wfgs          chain     azorg01l
004200071019     c                   if        %found(azorg01l)
004300071018     c                   eval      og147 = orgde7
004400071018     c                   if        §ogddao = *blanks or §ogddao = *zeros
004500071019     c                   goto      fine
004600071018     c                   endif
004700071018      *?data partenza procedura in organigramma
004800071018     c                   move      §ogddao       data_org
004900071018      *?se data del giorno uguale alla data partenza procedura automatica ORM
005000071018      *?sistemo i dati
005100071018     c                   if        oggi = data_org
005200071018     c                   exsr      sr_sistema
005300071018      *?e aggiorno i totali delle distinte
005400071018     c                   exsr      sr_distinte
005500071018     c                   endif
005600071019     c                   endif
005700070308
005800070927     c     fine          tag
005900070927
006000070308     c                   eval      *inlr = *on
006100070308
006200070308      *------------------------------------------------------------------------*
006300071018      *? sistemo i dati degli ORM per la partenza
006400070308      *------------------------------------------------------------------------*
006500071018     c     sr_sistema    begsr
006600071018
006700071018      *?pulisco i dati se ORM non in distinta
006800070926     C/EXEC SQL
006900071018     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
007000071018     C+ FROM fnorm01l join fnorg01l on
007100071018     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
007200071018     C+ ormnrv=orgnrv
007300071018     C+ WHERE ormndc=0 and ormeti = 'S' and
007400071018     C+ ormpor =:orgfil
007500071018     C/END-EXEC
007600071018
007700071018     C/EXEC SQL
007800071018     C+ OPEN B1
007900071018     C/END-EXEC
008000071018
008100071018     c                   do        *hival
008200071018
008300071018     C/EXEC SQL
008400071018     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
008500071018     C/END-EXEC
008600071018
008700071018     c                   if        sqlcod = 100 or
008800071018     c                             sqlcod < 0
008900071023     c                   leave
009000071018     c                   endif
009100071018
009200071018     c                   exsr      sr_pulisci
009300071018     c                   enddo
009400071018
009500071018     C/EXEC SQL
009600071018     C+ CLOSE B1
009700071018     C/END-EXEC
009800070926
009900071018      *?aggiorno i dati se ORM in distinta
010000071018     C/EXEC SQL
010100071018     C+ DECLARE B2 CURSOR FOR SELECT fnorm01l.*, fnorf01l.*,
010200071018     C+ fnorg01l.*
010300071018     C+ FROM (fnorm01l join fnorf01l on
010400071018     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
010500071018     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
010600071018     C+ ormofo=orfore) join fnorg01l on
010700071018     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
010800071018     C+ ormnrv=orgnrv
010900071018     C+ WHERE ormndc>0 and ormddc=:oggi and
011000071018     C+ ormpor =:orgfil
011100071018     C/END-EXEC
011200071018
011300071018     C/EXEC SQL
011400071018     C+ OPEN B2
011500071018     C/END-EXEC
011600071018
011700071018     c                   do        *hival
011800071018
011900071018     C/EXEC SQL
012000071018     C+ FETCH NEXT FROM B2 INTO :fnormds, :fnorfds, :fnorgds
012100071018     C/END-EXEC
012200071018
012300071018     c                   if        sqlcod = 100 or
012400071018     c                             sqlcod < 0
012500071023     c                   leave
012600071018     c                   endif
012700071018
012800071018     c                   exsr      sr_aggiorna
012900071018     c                   enddo
013000071018
013100071018     C/EXEC SQL
013200071018     C+ CLOSE B2
013300071018     C/END-EXEC
013400080128
013500080128      *?pulisco i dati NFTL e SLO impostati in simulazione
013600080128     C/EXEC SQL
013700080128     C+ DECLARE B4 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
013800080128     C+ FROM fnorm01l join fnorg01l on
013900080128     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
014000080128     C+ ormnrv=orgnrv
014100080128     C+ WHERE orgndc=0 and ormeti = ' ' and
014200080128     C+ ormpor =:orgfil and orgnftl <> 0
014300080128     C/END-EXEC
014400080128
014500080128     C/EXEC SQL
014600080128     C+ OPEN B4
014700080128     C/END-EXEC
014800080128
014900080128     c                   do        *hival
015000080128
015100080128     C/EXEC SQL
015200080128     C+ FETCH NEXT FROM B4 INTO :fnormds, :fnorgds
015300080128     C/END-EXEC
015400080128
015500080128     c                   if        sqlcod = 100 or
015600080128     c                             sqlcod < 0
015700080128     c                   leave
015800080128     c                   endif
015900080128
016000080128     c                   exsr      sr_clear
016100080128     c                   enddo
016200080128
016300080128     C/EXEC SQL
016400080128     C+ CLOSE B4
016500080128     C/END-EXEC
016600071011
016700071011     c                   endsr
016800071018
016900071018      *------------------------------------------------------------------------*
017000071018      *? pulisco i dati da ORM e ORG
017100071018      *------------------------------------------------------------------------*
017200071018     c     sr_pulisci    begsr
017300071018
017400071018     c     korm          chain     fnorm01l
017500071018     c                   if        %found(fnorm01l)
017600071018     c                   clear                   ormdst
017700071018     c                   update    fnorm000
017800071018     c                   endif
017900071018
018000071018     c     korm          chain     fnorg01l
018100071018     c                   if        %found(fnorg01l)
018200071018     c                   clear                   orgpdc
018300071018     c                   clear                   orgfgs
018400071018     c                   clear                   orgndc
018500071018     c                   clear                   orgddc
018600071018     c                   clear                   orgnftl
018700071018     c                   clear                   orgslo
018800071018     c                   clear                   orgdtvdis
018900071018     c                   clear                   orghvdis
019000071018     c                   update    fnorg000
019100071018     c                   endif
019200071018
019300071018     c                   endsr
019400071018
019500071018      *------------------------------------------------------------------------*
019600071018      *? aggiorno ORM e ORG
019700071018      *------------------------------------------------------------------------*
019800071018     c     sr_aggiorna   begsr
019900071018
020000071018     c                   eval      dstnpg = 4
020100071018     c                   eval      dstnfv = ormndc
020200071018     c                   eval      dstfgs = orffgs
020300071018     c     kdst          chain     fidst01l
020400071018     c                   if        %found(fidst01l)
020500071018
020600071018     c     korm          chain     fnorm01l
020700071018     c                   if        %found(fnorm01l)
020800080205     c                   eval      savfao = ormfao
020900080205     c                   eval      savdfo = ormdfo
021000080205     c                   eval      savofo = ormofo
021100071018     c                   eval      ormdst = oggi
021200071023     c                   update    fnorm000
021300071018     c                   endif
021400080205
021500080205      * cerco la fase precedente alla 400
021600080205     c                   if        ormfao = 400
021700080205     c     korf          setll     fnorf01l
021800080205     c     korm          readpe    fnorf01l
021900080205     c                   if        not %eof(fnorf01l)
022000080206     c                   eval      savfao = worffar
022100080206     c                   eval      savdfo = worfdae
022200080206     c                   eval      savofo = worfore
022300080205     c                   endif
022400080205     c                   endif
022500071018
022600071018     c     korm          chain     fnorg01l
022700071018     c                   if        %found(fnorg01l)
022800071018     c                   eval      orgpdc = dstpdr
022900071018     c                   eval      orgfgs = orffgs
023000071018     c                   eval      orgndc = ormndc
023100071018     c                   eval      orgddc = ormddc
023200071018     c                   eval      orgnftl = *all'9'
023300071018     c                   clear                   orgslo
023400071018     c                   eval      orgdtvdis = ormddc
023500071018     c                   eval      orghvdis = 100000
023600080205     c                   eval      orgfao = savfao
023700080205     c                   eval      orgdfo = savdfo
023800080205     c                   eval      orgofo = savofo
023900071018     c                   update    fnorg000
024000071018     c                   endif
024100071018     c                   endif
024200071018
024300071018     c                   endsr
024400080128
024500080128      *------------------------------------------------------------------------*
024600080128      *? pulisco i dati NFTL e SLO
024700080128      *------------------------------------------------------------------------*
024800080128     c     sr_clear      begsr
024900080128
025000080128     c     korm          chain     fnorg01l
025100080128     c                   if        %found(fnorg01l)
025200080128     c                   clear                   orgnftl
025300080128     c                   clear                   orgslo
025400080128     c                   update    fnorg000
025500080128     c                   endif
025600080128
025700080128     c                   endsr
025800071018
025900071018      *------------------------------------------------------------------------*
026000071018      *? leggo le distinte del giorno per aggiornarne i totali
026100071018      *------------------------------------------------------------------------*
026200071018     c     sr_distinte   begsr
026300071018
026400071018     C/EXEC SQL
026500071018     C+ DECLARE B3 CURSOR FOR SELECT fidst00f.*
026600071018     C+ FROM fidst00f
026700071018     C+ WHERE dstdfv =:oggi and dstatb = ' ' and
026800071023     C+ dstfcf = ' ' and dstfgs =:orgfil
026900071018     C/END-EXEC
027000071018
027100071018     C/EXEC SQL
027200071018     C+ OPEN B3
027300071018     C/END-EXEC
027400071018
027500071018     c                   do        *hival
027600071018
027700071018     C/EXEC SQL
027800071018     C+ FETCH NEXT FROM B3 INTO :fidstds
027900071018     C/END-EXEC
028000071018
028100071018     c                   if        sqlcod = 100 or
028200071018     c                             sqlcod < 0
028300071023     c                   leave
028400071018     c                   endif
028500071018
028600071018     c                   exsr      sr_totali
028700071018     c                   enddo
028800071018
028900071018     C/EXEC SQL
029000071018     C+ CLOSE B3
029100071018     C/END-EXEC
029200071018
029300071018     c                   endsr
029400071018
029500071018      *------------------------------------------------------------------------*
029600071018      *? richiamo pgm per aggiornamento totali distinta
029700071018      *------------------------------------------------------------------------*
029800071018     c     sr_totali     begsr
029900071018
030000071018     c                   clear                   fior67ds
030100071018     c                   eval      or67fgsi = dstfgs
030200071018     c                   eval      or67dfvi = dstdfv
030300071018     c                   eval      or67nfvi = dstnfv
030400071018     c                   eval      or67cmti = 'S'
030500071018     c                   eval      kpjbu  = fior67ds
030600071018     c                   call      'FIOR67R'
030700071018     c                   parm                    kpjba
030800071018
030900071018     c                   endsr
031000070926
031100070926      *------------------------------------------------------------------------*
031200071018      *?ROUTINE INIZIALE
031300070926      *------------------------------------------------------------------------*
031400070926     c     *inzsr        begsr
031500071019
031600071019     c     *entry        plist
031700071019     c                   parm                    wfil              3
031800071019
031900071019     c                   move      wfil          wfgs              3 0
032000071011
032100071011     c                   eval      oggi = *date
032200071018
032300071018     c     korm          klist
032400071018     c                   kfld                    ormpoe
032500071018     c                   kfld                    ormnsr
032600071018     c                   kfld                    ormnor
032700071018     c                   kfld                    ormnrv
032800080205
032900080205     c     korf          klist
033000080205     c                   kfld                    ormpoe
033100080205     c                   kfld                    ormnsr
033200080205     c                   kfld                    ormnor
033300080205     c                   kfld                    ormnrv
033400080205     c                   kfld                    ormdfo
033500080205     c                   kfld                    ormofo
033600080205     c                   kfld                    ormfao
033700071018
033800071018     c     kdst          klist
033900071018     c                   kfld                    dstnpg
034000071018     c                   kfld                    dstnfv
034100071018     c                   kfld                    dstfgs
034200070926
034300070926     c                   endsr
