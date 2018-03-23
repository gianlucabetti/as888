000100070724     H DECEDIT('0,') DATEDIT(*DMY.)  option(*nodebugio)
000200070724     h dftactgrp(*no) actgrp(*caller)
000300070724      *
000400070724      * Aggiorna x distinta tot.pes/vol/Orm/bancali assegnati
000500070724      *--------------------------------------------------------------*
000600070724      * ATTENZIONE: ho fatto l'aggiornamento con il fisico perchè se il
000700070724      * chiamante deve aggiornare lo stesso record questo deve essere condi-
000800070724      * viso quindi: prima di aprire il file FIDST00F fare OVRDBF SHARE(*yes)
000900070724      * e gli aggiornamenti devono essere fatti sul fisico.
001000070724      *--------------------------------------------------------------*
001100070724     Ffidst03l  if   e           k disk    infds(dsinf)
001200070724     Ffidst00f  uf   e             disk    commit rename(fidst000: fidst00)
001300070724     Ffidst52l  if   e           k disk
001400070724     Ffnorg03l  if   e           k disk
001500070724     Ffnorg01l  if   e           k disk    rename(fnorg000:fnorg1)
001600070724     Ffnorm01l  if   e           k disk
001700070724     Fazorg01l  if   e           k disk
001800070724     Ffidst61l  uf a e           k disk    commit
001900070724      *--------------------------------------------------------------*
002000070724      *   DS
002100070724      *--------------------------------------------------------------*
002200070724     d dsinf           ds
002300070724     d  dst_nrr              397    400b 0
002400070724     d trul82ds      e ds
002500070724     D kpjba         E DS
002600070724     D fior67ds      E DS
002700070724     D fnormds       E DS                  extname(fnorm00f)
002800070724     D fnorgds       E DS                  extname(fnorg00f)
002900070724     D fnorfds       E DS                  extname(fnorf00f)
003000070724     D dorf01        E DS
003100070724     D og147         E DS
003200070724      *--------------------------------------------------------------*
003300070724      *   schiere
003400070724      *--------------------------------------------------------------*
003500070724
003600070724     d Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
003700070724
003800070724     d fer             s             70    dim(500)
003900070724      * skiere per riempimento file
004000070724     d giro            s             11    dim(500)
004100070724     d peso            s              7  2 dim(500)
004200070724     d volu            s              5  3 dim(500)
004300070724     d orm             s              5  0 dim(500)
004400070724     d bnc             s              5  0 dim(500)
004500070724      *--------------------------------------------------------------*
004600070724      *   variabili
004700070724      *--------------------------------------------------------------*
004800070724
004900070724     d como11b         s             11
005000070724     d como11          s             11
005100070724     d como70          s             70
005200070724     d como70b         s             70
005300070724
005400070724     d x               s              3  0
005500070724     d xx              s              3  0
005600070724     d catk            s              1  0 inz(4)
005700070724     d nfvk            s              7  0
005800070724     d girok           s             10
005801071122     d wpeso           s             14  1 INZ
005803071122     d wvolume         s             10  3 INZ
005900070724     d w0140           s             14  0
006000070724     d wdat            s              8  0
006100070724     d wdymd           s              8  0
006200070724     d wora            s              6  0
006300070724     d ogddaon         s              8  0
006400070724     d dataiso         s               d   datfmt(*iso)
006500070724     d dataeur         s               d   datfmt(*eur)
006600070724      *___________________________________________________
006700070724     C     *entry        plist
006800070724     C                   PARM                    KPJBA
006900070724     c                   if        kpjbu = ' '
007000070724     c                   clear                   fior67ds
007100070724     c                   move      '1'           or67erro
007200070724     c                   goto      fine
007300070724     c                   else
007400070724     C                   MOVEL     KPJBU         fior67ds
007500070724     c                   end
007600070724     c                   z-add     or67nfvi      nfvk
007700070724     c                   clear                   or67rpklo
007800070724     c                   clear                   or67rvluo
007900070724     c                   clear                   or67rnftlo
008000070724     c                   clear                   or67rnblo
008100070724     c                   clear                   or67rpklao
008200070724     c                   clear                   or67rvluao
008300070724     c                   clear                   or67rstpao
008400070724     c                   clear                   or67rnblao
008500070724     c                   clear                   or67rnbaao
008600070724     c                   clear                   or67erro
008700070724      *
008800070724     C                   time                    w0140
008900070724     C                   move      w0140         wdat
009000070724     C                   movel     w0140         wora
009100070724     c                   move      wdat          dataeur
009200070724     c                   move      dataeur       dataiso
009300070724     c                   move      dataiso       wdymd
009400070724
009500070724     c     or67fgsi      chain     azorg01l
009600070724     c                   if        %found(azorg01l)
009700070724     c                   movel     orgde7        og147
009800070724     c                   if        §ogddao = ' '
009900070724     c                   move      '1'           or67erro
010000070724     c                   goto      fine
010100070724     c                   else
010200070724     c                   move      §ogddao       ogddaon
010300070724     c                   endif
010400070724      * leggo rekord di DST per assumere i valori MASSIMI
010500070724     C     kdst          chain     fidst03l
010600070724     c                   if        %found(fidst03l)
010700070724     C     dst_nrr       chain(e)  fidst00f
010800070724     c                   if        %error
010900070724     c                   exsr      geslck
011000070724     c                   if        ul82§sts = 'A'
011100070724     c                   move      '1'           or67erro
011200070724     c                   goto      fine
011300070724     c                   else
011400070724     C     dst_nrr       chain     fidst00f
011500070724     c                   endif
011600070724     c                   endif
011700070724     c                   z-add     DSTrpkl       or67rpklo
011800070724     c                   z-add     DSTrvlu       or67rvluo
011900070724     c                   z-add     DSTrnftl      or67rnftlo
012000070724     c                   z-add     DSTrnbl       or67rnblo
012100070724      * se è fase di test elabora i dati da ORG per agganciare gli
012200070724      * ORM   della distinta altrimenti legge direttamente ORM
012300070724     c                   if        wdymd  < ogddaon
012400070724     c                   exsr      elabORG
012500070724     c                   else
012600070724     c                   exsr      elabORM
012700070724     c                   endif
012800070724      * imposto numero fermate
012900070227     c                   z-add     1             x
013000070724     c     como70b       lookup    fer(x)                                 80
013100070724     c   80              eval      or67rstpao = x - 1
013200070420      * scrive file con totali per distinta giro
013300071001      * solo se viene richiesto di essere aggiornato
013400071001     c                   If        or67updi = ' '
013500070724     c                   exsr      scrividst6
013600070724      * Aggiorno FIDST00F
013700070724     c                   eval      dstrpkla = or67rpklao
013800070724     c                   eval      dstrVLUa = or67rvluao
013900070724     c                   eval      dstrstpa = or67rstpao
014000070724     c                   eval      DSTrnblA = or67rnblao
014100070724     c                   eval      DSTrnbaA = or67rnbaao
014200070517     c                   update    fidst00
014300070724      * se richiesto commit
014400070724     c                   if        or67cmti = 'S'
014500070516     c                   commit
014600070516     c                   end
014700071001
014800071001     c                   endif
014900070226      *non trovato DST
015000070226     c                   else
015100070724     c                   move      '1'           or67erro
015200070226     c                   goto      fine
015300070226     c                   endif
015400070226
015500070226     c                   endif
015600070226
015700070517     c     fine          tag
015800070724     c                   eval      kpjbu = fior67ds
015900070226     c                   seton                                        lr
016000070724      *___________________________________________________
016100070724     c     elabORG       begsr
016200070724      *___________________________________________________
016300070724     c     korg          setll     fnorg03l
016400070724     c                   do        *hival
016500070724     c     korg          reade     fnorg03l
016600070724     c                   if        %eof(fnorg03l)
016700070724     c                   leave
016800070724     c                   endif
016900070724
017000070724     c                   if        orgnftl = 99999
017100070724     c                   movel     'M'           como11
017200070724     c                   else
017300070724     c                   movel     'A'           como11
017400070724     c                   end
017500070724     c                   move      orgcgi        como11
017600070724
017700070724     c     korm1         chain     fnorm01l
017800070724     c                   if        %found(fnorm01l)
017900070724      * memorizza fermate da ragione sociale/indirizzo
018000070724     c                   exsr      fermate
018100070724      * sommo peso e volume ai totali valori assegnati
018200071122     c******             add       orgpkg        or67rpklao
018201071122     c     or67rpklao    add       orgpkg        wpeso
018202071122     c                   If        Wpeso >  999999,9
018203071122     c                   eval      or67rpklao = 999999,9
018204071122     c                   else
018205071122     c                   eval      or67rPKLao = or67rpklao + orgpkg
018206071122     c                   endif
018300071122     c*****              add       orgvlm        or67rvluao
018301071122     c     or67rvluao    add       orgvlm        wvolume
018302071122     c                   If        Wvolume >  99,999
018303071122     c                   eval      or67rvluao = 99,999
018304071122     c                   else
018305071122     c                   eval      or67rVLUao = or67rvluao + orgvlm
018306071122     c                   endif
018400070724      * ORM
018500070724     c                   add       1             or67rnblao
018600070724      * Bancali
018700070724     c                   add       ormbnc        or67rnbaao
018800070724      *preparo skiere per distinta
018900070724     c                   exsr      carica
019000070724     c
019100070724     c                   endif
019200070724     c                   enddo
019300070724      *
019400070724    1C                   endsr
019500070724      *___________________________________________________
019600070724     c     elabORM       begsr
019700070724      *___________________________________________________
019800070724
019900070724     C/EXEC SQL
020000070724     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorf01l.*,
020100070724     C+ fnorg01l.*
020200070724     C+ FROM (fnorm01l join fnorf01l on
020300070724     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
020400070724     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
020500070724     C+ ormofo=orfore) join fnorg01l on
020600070724     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
020700070724     C+ ormnrv=orgnrv
020800070724     C+ WHERE orfndc=:or67nfvi and orfddc=:or67dfvi and
020900070724     C+ orffgs =:or67fgsi
021000070724     C+ ORDER BY fnorm01l.ormpoe, fnorm01l.ormnsr, fnorm01l.ormnor,
021100070724     C+ fnorm01l.ormnrv
021200070724     C/END-EXEC
021300070724
021400070724     C/EXEC SQL
021500070724     C+ OPEN A1
021600070724     C/END-EXEC
021700070724
021800070724     c                   do        *hival
021900070724
022000070724     C/EXEC SQL
022100070724     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorfds, :fnorgds
022200070724     C/END-EXEC
022300070724
022400070724     c                   select
022500070724     c                   when      sqlcod = 100
022600070724     c                   leave
022700070724     c                   when      sqlcod < 0
022800070724      * record allocato
022900070724     c                   if        sqlcod <> -913
023000070724     c                   eval      *inh1 = *on
023100070724     c                   endif
023200070724     c                   move      '1'           or67erro
023300070724     c                   goto      fine
023400070724
023500070724     c                   other
023600070724
023700070724     c                   movel     orfflo        dorf01
023800070724     c                   if        §orfass = 'M'
023900070724     c                   movel     'M'           como11
024000070724     c                   end
024100070724     c                   if        §orfass = 'A'
024200070724     c                   movel     'A'           como11
024300070724     c                   end
024400070724     c                   move      orgcgi        como11
024500070724      * memorizza fermate da ragione sociale/indirizzo
024600070724     c                   exsr      fermate
024700070724      * sommo peso e volume ai totali valori assegnati
024800071123     c****               add       orgpkg        or67rpklao
024900071123     c***                add       orgvlm        or67rvluao
024901071123     c     or67rpklao    add       orgpkg        wpeso
024902071123     c                   If        Wpeso >  999999,9
024903071123     c                   eval      or67rpklao = 999999,9
024904071123     c                   else
024905071123     c                   eval      or67rPKLao = or67rpklao + orgpkg
024906071123     c                   endif
024908071123     c     or67rvluao    add       orgvlm        wvolume
024909071123     c                   If        Wvolume >  99,999
024910071123     c                   eval      or67rvluao = 99,999
024911071123     c                   else
024912071123     c                   eval      or67rVLUao = or67rvluao + orgvlm
024913071123     c                   endif
025000070724      * ORM
025100070724     c                   add       1             or67rnblao
025200070724      * Bancali
025300070724     c                   add       ormbnc        or67rnbaao
025400070724      *preparo skiere per distinta
025500070724     c                   exsr      carica
025600070724     c
025700070724     c                   endsl
025800070724
025900070724     c                   enddo
026000070724
026100070724     C/EXEC SQL
026200070724     C+ CLOSE a1
026300070724     C/END-EXEC
026400070724
026500070724     c                   endsr
026600070724      *___________________________________________________
026700070724     c     fermate       begsr
026800070724      *___________________________________________________
026900070724     c                   z-add     1             x
027000070724     c                   movel     ormrsr        como70
027100070724     c                   move      orminr        como70
027200070724     c     como70        lookup    fer                                    80
027300070724     c                   if        not *in80
027400070724     c     como70b       lookup    fer(x)                                 80
027500070724     c   80              movel     como70        fer(x)
027600070724     c                   endif
027700070724
027800070724    1c                   endsr
027900070724      *___________________________________________________
028000070724     c     carica        begsr
028100070724      *___________________________________________________
028200070724      * totalizza pesi volumi orm   per giro
028300070420     c                   z-add     1             x
028400070516     c     como11        lookup    giro(x)                                80
028500070420     c                   if        *in80
028600070724     c                   add       orgpkg        peso(x)
028700070724     c                   add       orgvlm        volu(x)
028800070724     c                   add       1             orm(x)
028900070724     c                   add       ormbnc        bnc(x)
029000070420     c                   else
029100070516     c     como11b       lookup    giro(x)                                80
029200070420     c                   if        *in80
029300070516     c                   movel     como11        giro(x)
029400070724     c                   add       orgpkg        peso(x)
029500070724     c                   add       orgvlm        volu(x)
029600070724     c                   add       1             orm(x)
029700070724     c                   add       ormbnc        bnc(x)
029800070420     c                   endif
029900070420     c                   endif
030000070420
030100070420    1C                   endsr
030200070420      *___________________________________________________
030300070724     C     scrividst6    begsr
030400070420      *___________________________________________________
030500070420      * pulizia file elimina i rekord esistenti per quella distinta
030600070724     c     kdst6         setll     fidst61l
030700070420     c                   do        *hival
030800070724     c     kdst6         reade     fidst61l
030900070724     c                   if        %eof(fidst61l)
031000070420     c                   leave
031100070420     c                   endif
031200070724     c                   delete    fidst600
031300070420     c                   enddo
031400070420      * scrive il nuovo dettaglio distinta giro
031500070420     c     1             do        500           x
031600070420     c                   if        giro(x) = *blank
031700070420     c                   leave
031800070420     c                   endif
031900070420     c                   move      giro(x)       girok
032000070530     c                   movel     giro(x)       tassok            1
032100070724     c                   clear                   dst6PRGG
032200070724     c                   clear                   dst6SQL
032300070530     c                   if        tassok = 'A'
032400071031     c     kdst52        chain     fidst52l
032500071031     c                   if        %found(fidst52l)
032600070724     c                   z-add     dst5prgg      dst6PRGG
032700070724     c                   z-add     dst5sql       dst6SQL
032800070420     c                   endif
032900070530     c                   endif
033000070724     c                   z-add     4             dst6npg
033100070724     c                   z-add     or67fgsi      dst6FGS
033200070724     c                   z-add     or67nfvi      dst6Nfv
033300070724     c                   z-add     or67dfvi      dst6Dfv
033400070724     c                   z-add     or67fgsi      dst6FGSG
033500070724     c                   movel     giro(x)       dst6tass
033600070724     c                   move      giro(x)       dst6cgi
033700070724     c                   z-add     peso(x)       dst6PKL
033800070724     c                   z-add     volu(x)       dst6VLU
033900070724     c                   z-add     orm(x)        dst6nbl
034000070724     c                   z-add     bnc(x)        dst6nba
034100070724     c                   move      *blank        dst6flo
034200070724     c                   write     fidst600
034300070724     c                   enddo
034400070724
034500070724    1C                   endsr
034600070724      *-------------------------------------------------
034700070724     c     *inzsr        begsr
034800070724      *-------------------------------------------------
034900070724     c     kdst          klist
035000070724     c                   kfld                    or67fgsi
035100070724     c                   kfld                    catk
035200070724     c                   kfld                    or67nfvi
035300070724
035400070724     c     kdst52        klist
035500070724     c                   kfld                    or67fgsi
035600070724     c                   kfld                    or67dfvi
035700070724     c                   kfld                    or67fgsi
035800070724     c                   kfld                    girok
035900070724     c                   kfld                    or67nfvi
036000070724
036100070724     c     korm1         klist
036200070724     c                   kfld                    orgpoe
036300070724     c                   kfld                    orgnsr
036400070724     c                   kfld                    orgnor
036500070724     c                   kfld                    orgnrv
036600070724
036700070724     c     korg          klist
036800070724     c                   kfld                    or67fgsi
036900070724     c                   kfld                    nfvk
037000070724     c                   kfld                    or67dfvi
037100070724
037200070724     c     korg1         klist
037300070724     c                   kfld                    ormpoe
037400070724     c                   kfld                    ormnsr
037500070724     c                   kfld                    ormnor
037600070724     c                   kfld                    ormnrv
037700070724
037800070724     c     kdst6         klist
037900070724     c                   kfld                    or67fgsi
038000070724     c                   kfld                    catk
038100070724     c                   kfld                    or67nfvi
038200070724     c                   kfld                    or67dfvi
038300070724
038400070724
038500070724     c                   endsr
038600070724      *----------------------------------------------------*
038700070724     c     geslck        begsr
038800070724      *----------------------------------------------------*
038900070724     c                   clear                   trul82ds
039000070724     c                   Eval      ul82§rrn = dst_nrr
039100070724     c                   Eval      UL82§FIL = 'FIDST00F'
039200070724     c                   Eval      UL82§WIN = 'S'
039300070724     c                   Eval      UL82§num = 2
039400070724     c                   Eval      UL82§att = 2
039500070724     c                   eval      ul82§mss = ErrMsg(1)
039600070724     c                   Eval      UL82§msw = 'La Distinta '
039700070724     c                             + %editc(or67nfvi:'Z') + ' è già utilizzata -
039800070724     c                             da un altro lavoro e non è manutenzionabile'
039900070724      * Effettuo la chiamata al *PGM d utilità
040000070724     c                   Call(e)   'TRUL82R'
040100070724     c                   Parm                    Trul82ds
040200070724
040300070724     C                   ENDSR
040400070724** ErrMsg
040500070724Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
