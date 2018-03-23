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
001300070308
001400070308      *------------------------------------------------------------------------*
001500070926      *?I N D I C A T O R I
001600070926
001700070926      *?V A R I A B I L I
001800070927     d data_org        s              8  0 inz
001900071011     d oggi            s              8  0 inz
002000031017
002100070926      *?D S   I N T E R N E / E S T E R N E
002200071018     d fidstds       e ds                  extname(fidst00f)
002300071018     d fior67ds      e ds
002400071018     d fnorfds       e ds                  extname(fnorf00f)
002500071011     d fnorgds       e ds                  extname(fnorg00f)
002600070927     d fnormds       e ds                  extname(fnorm00f)
002700071018     d kpjba         e ds
002800070927     d og147         e ds
002900070926
003000070308      *------------------------------------------------------------------------*
003100071019
003200071019     c/exec sql
003300071019     c+ set option dynusrprf = *owner, closqlcsr = *endmod
003400071019     c/end-exec
003500071018
003600071018      *?leggo organigramma per partenza filiale
003700071019     c     wfgs          chain     azorg01l
003800071019     c                   if        %found(azorg01l)
003900071018     c                   eval      og147 = orgde7
004000071018     c                   if        §ogddao = *blanks or §ogddao = *zeros
004100071019     c                   goto      fine
004200071018     c                   endif
004300071018      *?data partenza procedura in organigramma
004400071018     c                   move      §ogddao       data_org
004500071018      *?se data del giorno uguale alla data partenza procedura automatica ORM
004600071018      *?sistemo i dati
004700071018     c                   if        oggi = data_org
004800071018     c                   exsr      sr_sistema
004900071018      *?e aggiorno i totali delle distinte
005000071018     c                   exsr      sr_distinte
005100071018     c                   endif
005200071019     c                   endif
005300070308
005400070927     c     fine          tag
005500070927
005600070308     c                   eval      *inlr = *on
005700070308
005800070308      *------------------------------------------------------------------------*
005900071018      *? sistemo i dati degli ORM per la partenza
006000070308      *------------------------------------------------------------------------*
006100071018     c     sr_sistema    begsr
006200071018
006300071018      *?pulisco i dati se ORM non in distinta
006400070926     C/EXEC SQL
006500071018     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
006600071018     C+ FROM fnorm01l join fnorg01l on
006700071018     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
006800071018     C+ ormnrv=orgnrv
006900071018     C+ WHERE ormndc=0 and ormeti = 'S' and
007000071018     C+ ormpor =:orgfil
007100071018     C/END-EXEC
007200071018
007300071018     C/EXEC SQL
007400071018     C+ OPEN B1
007500071018     C/END-EXEC
007600071018
007700071018     c                   do        *hival
007800071018
007900071018     C/EXEC SQL
008000071018     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
008100071018     C/END-EXEC
008200071018
008300071018     c                   if        sqlcod = 100 or
008400071018     c                             sqlcod < 0
008500071023     c                   leave
008600071018     c                   endif
008700071018
008800071018     c                   exsr      sr_pulisci
008900071018     c                   enddo
009000071018
009100071018     C/EXEC SQL
009200071018     C+ CLOSE B1
009300071018     C/END-EXEC
009400070926
009500071018      *?aggiorno i dati se ORM in distinta
009600071018     C/EXEC SQL
009700071018     C+ DECLARE B2 CURSOR FOR SELECT fnorm01l.*, fnorf01l.*,
009800071018     C+ fnorg01l.*
009900071018     C+ FROM (fnorm01l join fnorf01l on
010000071018     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
010100071018     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
010200071018     C+ ormofo=orfore) join fnorg01l on
010300071018     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
010400071018     C+ ormnrv=orgnrv
010500071018     C+ WHERE ormndc>0 and ormddc=:oggi and
010600071018     C+ ormpor =:orgfil
010700071018     C/END-EXEC
010800071018
010900071018     C/EXEC SQL
011000071018     C+ OPEN B2
011100071018     C/END-EXEC
011200071018
011300071018     c                   do        *hival
011400071018
011500071018     C/EXEC SQL
011600071018     C+ FETCH NEXT FROM B2 INTO :fnormds, :fnorfds, :fnorgds
011700071018     C/END-EXEC
011800071018
011900071018     c                   if        sqlcod = 100 or
012000071018     c                             sqlcod < 0
012100071023     c                   leave
012200071018     c                   endif
012300071018
012400071018     c                   exsr      sr_aggiorna
012500071018     c                   enddo
012600071018
012700071018     C/EXEC SQL
012800071018     C+ CLOSE B2
012900071018     C/END-EXEC
013000080128
013100080128      *?pulisco i dati NFTL e SLO impostati in simulazione
013200080128     C/EXEC SQL
013300080128     C+ DECLARE B4 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
013400080128     C+ FROM fnorm01l join fnorg01l on
013500080128     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
013600080128     C+ ormnrv=orgnrv
013700080128     C+ WHERE orgndc=0 and ormeti = ' ' and
013800080128     C+ ormpor =:orgfil and orgnftl <> 0
013900080128     C/END-EXEC
014000080128
014100080128     C/EXEC SQL
014200080128     C+ OPEN B4
014300080128     C/END-EXEC
014400080128
014500080128     c                   do        *hival
014600080128
014700080128     C/EXEC SQL
014800080128     C+ FETCH NEXT FROM B4 INTO :fnormds, :fnorgds
014900080128     C/END-EXEC
015000080128
015100080128     c                   if        sqlcod = 100 or
015200080128     c                             sqlcod < 0
015300080128     c                   leave
015400080128     c                   endif
015500080128
015600080128     c                   exsr      sr_clear
015700080128     c                   enddo
015800080128
015900080128     C/EXEC SQL
016000080128     C+ CLOSE B4
016100080128     C/END-EXEC
016200071011
016300071011     c                   endsr
016400071018
016500071018      *------------------------------------------------------------------------*
016600071018      *? pulisco i dati da ORM e ORG
016700071018      *------------------------------------------------------------------------*
016800071018     c     sr_pulisci    begsr
016900071018
017000071018     c     korm          chain     fnorm01l
017100071018     c                   if        %found(fnorm01l)
017200071018     c                   clear                   ormdst
017300071018     c                   update    fnorm000
017400071018     c                   endif
017500071018
017600071018     c     korm          chain     fnorg01l
017700071018     c                   if        %found(fnorg01l)
017800071018     c                   clear                   orgpdc
017900071018     c                   clear                   orgfgs
018000071018     c                   clear                   orgndc
018100071018     c                   clear                   orgddc
018200071018     c                   clear                   orgnftl
018300071018     c                   clear                   orgslo
018400071018     c                   clear                   orgdtvdis
018500071018     c                   clear                   orghvdis
018600071018     c                   update    fnorg000
018700071018     c                   endif
018800071018
018900071018     c                   endsr
019000071018
019100071018      *------------------------------------------------------------------------*
019200071018      *? aggiorno ORM e ORG
019300071018      *------------------------------------------------------------------------*
019400071018     c     sr_aggiorna   begsr
019500071018
019600071018     c                   eval      dstnpg = 4
019700071018     c                   eval      dstnfv = ormndc
019800071018     c                   eval      dstfgs = orffgs
019900071018     c     kdst          chain     fidst01l
020000071018     c                   if        %found(fidst01l)
020100071018
020200071018     c     korm          chain     fnorm01l
020300071018     c                   if        %found(fnorm01l)
020400071018     c                   eval      ormdst = oggi
020500071023     c                   update    fnorm000
020600071018     c                   endif
020700071018
020800071018     c     korm          chain     fnorg01l
020900071018     c                   if        %found(fnorg01l)
021000071018     c                   eval      orgpdc = dstpdr
021100071018     c                   eval      orgfgs = orffgs
021200071018     c                   eval      orgndc = ormndc
021300071018     c                   eval      orgddc = ormddc
021400071018     c                   eval      orgnftl = *all'9'
021500071018     c                   clear                   orgslo
021600071018     c                   eval      orgdtvdis = ormddc
021700071018     c                   eval      orghvdis = 100000
021800071114     c                   eval      orgfao = ormfao
021900071114     c                   eval      orgdfo = ormdfo
022000071114     c                   eval      orgofo = ormofo
022100071018     c                   update    fnorg000
022200071018     c                   endif
022300071018     c                   endif
022400071018
022500071018     c                   endsr
022600080128
022700080128      *------------------------------------------------------------------------*
022800080128      *? pulisco i dati NFTL e SLO
022900080128      *------------------------------------------------------------------------*
023000080128     c     sr_clear      begsr
023100080128
023200080128     c     korm          chain     fnorg01l
023300080128     c                   if        %found(fnorg01l)
023400080128     c                   clear                   orgnftl
023500080128     c                   clear                   orgslo
023600080128     c                   update    fnorg000
023700080128     c                   endif
023800080128
023900080128     c                   endsr
024000071018
024100071018      *------------------------------------------------------------------------*
024200071018      *? leggo le distinte del giorno per aggiornarne i totali
024300071018      *------------------------------------------------------------------------*
024400071018     c     sr_distinte   begsr
024500071018
024600071018     C/EXEC SQL
024700071018     C+ DECLARE B3 CURSOR FOR SELECT fidst00f.*
024800071018     C+ FROM fidst00f
024900071018     C+ WHERE dstdfv =:oggi and dstatb = ' ' and
025000071023     C+ dstfcf = ' ' and dstfgs =:orgfil
025100071018     C/END-EXEC
025200071018
025300071018     C/EXEC SQL
025400071018     C+ OPEN B3
025500071018     C/END-EXEC
025600071018
025700071018     c                   do        *hival
025800071018
025900071018     C/EXEC SQL
026000071018     C+ FETCH NEXT FROM B3 INTO :fidstds
026100071018     C/END-EXEC
026200071018
026300071018     c                   if        sqlcod = 100 or
026400071018     c                             sqlcod < 0
026500071023     c                   leave
026600071018     c                   endif
026700071018
026800071018     c                   exsr      sr_totali
026900071018     c                   enddo
027000071018
027100071018     C/EXEC SQL
027200071018     C+ CLOSE B3
027300071018     C/END-EXEC
027400071018
027500071018     c                   endsr
027600071018
027700071018      *------------------------------------------------------------------------*
027800071018      *? richiamo pgm per aggiornamento totali distinta
027900071018      *------------------------------------------------------------------------*
028000071018     c     sr_totali     begsr
028100071018
028200071018     c                   clear                   fior67ds
028300071018     c                   eval      or67fgsi = dstfgs
028400071018     c                   eval      or67dfvi = dstdfv
028500071018     c                   eval      or67nfvi = dstnfv
028600071018     c                   eval      or67cmti = 'S'
028700071018     c                   eval      kpjbu  = fior67ds
028800071018     c                   call      'FIOR67R'
028900071018     c                   parm                    kpjba
029000071018
029100071018     c                   endsr
029200070926
029300070926      *------------------------------------------------------------------------*
029400071018      *?ROUTINE INIZIALE
029500070926      *------------------------------------------------------------------------*
029600070926     c     *inzsr        begsr
029700071019
029800071019     c     *entry        plist
029900071019     c                   parm                    wfil              3
030000071019
030100071019     c                   move      wfil          wfgs              3 0
030200071011
030300071011     c                   eval      oggi = *date
030400071018
030500071018     c     korm          klist
030600071018     c                   kfld                    ormpoe
030700071018     c                   kfld                    ormnsr
030800071018     c                   kfld                    ormnor
030900071018     c                   kfld                    ormnrv
031000071018
031100071018     c     kdst          klist
031200071018     c                   kfld                    dstnpg
031300071018     c                   kfld                    dstnfv
031400071018     c                   kfld                    dstfgs
031500070926
031600070926     c                   endsr
