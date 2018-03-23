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
013000071011
013100071011     c                   endsr
013200071018
013300071018      *------------------------------------------------------------------------*
013400071018      *? pulisco i dati da ORM e ORG
013500071018      *------------------------------------------------------------------------*
013600071018     c     sr_pulisci    begsr
013700071018
013800071018     c     korm          chain     fnorm01l
013900071018     c                   if        %found(fnorm01l)
014000071018     c                   clear                   ormdst
014100071018     c                   update    fnorm000
014200071018     c                   endif
014300071018
014400071018     c     korm          chain     fnorg01l
014500071018     c                   if        %found(fnorg01l)
014600071018     c                   clear                   orgpdc
014700071018     c                   clear                   orgfgs
014800071018     c                   clear                   orgndc
014900071018     c                   clear                   orgddc
015000071018     c                   clear                   orgnftl
015100071018     c                   clear                   orgslo
015200071018     c                   clear                   orgdtvdis
015300071018     c                   clear                   orghvdis
015400071018     c                   update    fnorg000
015500071018     c                   endif
015600071018
015700071018     c                   endsr
015800071018
015900071018      *------------------------------------------------------------------------*
016000071018      *? aggiorno ORM e ORG
016100071018      *------------------------------------------------------------------------*
016200071018     c     sr_aggiorna   begsr
016300071018
016400071018     c                   eval      dstnpg = 4
016500071018     c                   eval      dstnfv = ormndc
016600071018     c                   eval      dstfgs = orffgs
016700071018     c     kdst          chain     fidst01l
016800071018     c                   if        %found(fidst01l)
016900071018
017000071018     c     korm          chain     fnorm01l
017100071018     c                   if        %found(fnorm01l)
017200071018     c                   eval      ormdst = oggi
017300071023     c                   update    fnorm000
017400071018     c                   endif
017500071018
017600071018     c     korm          chain     fnorg01l
017700071018     c                   if        %found(fnorg01l)
017800071018     c                   eval      orgpdc = dstpdr
017900071018     c                   eval      orgfgs = orffgs
018000071018     c                   eval      orgndc = ormndc
018100071018     c                   eval      orgddc = ormddc
018200071018     c                   eval      orgnftl = *all'9'
018300071018     c                   clear                   orgslo
018400071018     c                   eval      orgdtvdis = ormddc
018500071018     c                   eval      orghvdis = 100000
018600071114     c                   eval      orgfao = ormfao
018700071114     c                   eval      orgdfo = ormdfo
018800071114     c                   eval      orgofo = ormofo
018900071018     c                   update    fnorg000
019000071018     c                   endif
019100071018     c                   endif
019200071018
019300071018     c                   endsr
019400071018
019500071018      *------------------------------------------------------------------------*
019600071018      *? leggo le distinte del giorno per aggiornarne i totali
019700071018      *------------------------------------------------------------------------*
019800071018     c     sr_distinte   begsr
019900071018
020000071018     C/EXEC SQL
020100071018     C+ DECLARE B3 CURSOR FOR SELECT fidst00f.*
020200071018     C+ FROM fidst00f
020300071018     C+ WHERE dstdfv =:oggi and dstatb = ' ' and
020400071023     C+ dstfcf = ' ' and dstfgs =:orgfil
020500071018     C/END-EXEC
020600071018
020700071018     C/EXEC SQL
020800071018     C+ OPEN B3
020900071018     C/END-EXEC
021000071018
021100071018     c                   do        *hival
021200071018
021300071018     C/EXEC SQL
021400071018     C+ FETCH NEXT FROM B3 INTO :fidstds
021500071018     C/END-EXEC
021600071018
021700071018     c                   if        sqlcod = 100 or
021800071018     c                             sqlcod < 0
021900071023     c                   leave
022000071018     c                   endif
022100071018
022200071018     c                   exsr      sr_totali
022300071018     c                   enddo
022400071018
022500071018     C/EXEC SQL
022600071018     C+ CLOSE B3
022700071018     C/END-EXEC
022800071018
022900071018     c                   endsr
023000071018
023100071018      *------------------------------------------------------------------------*
023200071018      *? richiamo pgm per aggiornamento totali distinta
023300071018      *------------------------------------------------------------------------*
023400071018     c     sr_totali     begsr
023500071018
023600071018     c                   clear                   fior67ds
023700071018     c                   eval      or67fgsi = dstfgs
023800071018     c                   eval      or67dfvi = dstdfv
023900071018     c                   eval      or67nfvi = dstnfv
024000071018     c                   eval      or67cmti = 'S'
024100071018     c                   eval      kpjbu  = fior67ds
024200071018     c                   call      'FIOR67R'
024300071018     c                   parm                    kpjba
024400071018
024500071018     c                   endsr
024600070926
024700070926      *------------------------------------------------------------------------*
024800071018      *?ROUTINE INIZIALE
024900070926      *------------------------------------------------------------------------*
025000070926     c     *inzsr        begsr
025100071019
025200071019     c     *entry        plist
025300071019     c                   parm                    wfil              3
025400071019
025500071019     c                   move      wfil          wfgs              3 0
025600071011
025700071011     c                   eval      oggi = *date
025800071018
025900071018     c     korm          klist
026000071018     c                   kfld                    ormpoe
026100071018     c                   kfld                    ormnsr
026200071018     c                   kfld                    ormnor
026300071018     c                   kfld                    ormnrv
026400071018
026500071018     c     kdst          klist
026600071018     c                   kfld                    dstnpg
026700071018     c                   kfld                    dstnfv
026800071018     c                   kfld                    dstfgs
026900070926
027000070926     c                   endsr
