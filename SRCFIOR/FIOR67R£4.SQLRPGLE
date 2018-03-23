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
002400080314
002500080314     d fnorfds         ds
002600080314     d orffgs                  1      3  0
002700080314     d orfndc                  4      9  0
002800080314     d orfddc                 10     17  0
002900080314     d §orfass                18     18
003000080314
003100070724     d trul82ds      e ds
003200070724     D kpjba         E DS
003300070724     D fior67ds      E DS
003400070724     D fnormds       E DS                  extname(fnorm00f)
003500070724     D fnorgds       E DS                  extname(fnorg00f)
003600070724     D og147         E DS
003700070724      *--------------------------------------------------------------*
003800070724      *   schiere
003900070724      *--------------------------------------------------------------*
004000070724
004100070724     d Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
004200070724
004300070724     d fer             s             70    dim(500)
004400070724      * skiere per riempimento file
004500070724     d giro            s             11    dim(500)
004600070724     d peso            s              7  2 dim(500)
004700070724     d volu            s              5  3 dim(500)
004800070724     d orm             s              5  0 dim(500)
004900070724     d bnc             s              5  0 dim(500)
005000070724      *--------------------------------------------------------------*
005100070724      *   variabili
005200070724      *--------------------------------------------------------------*
005300070724
005400070724     d como11b         s             11
005500070724     d como11          s             11
005600070724     d como70          s             70
005700070724     d como70b         s             70
005800070724
005900070724     d x               s              3  0
006000070724     d xx              s              3  0
006100070724     d catk            s              1  0 inz(4)
006200070724     d nfvk            s              7  0
006300070724     d girok           s             10
006400071122     d wpeso           s             14  1 INZ
006500071122     d wvolume         s             10  3 INZ
006600070724     d w0140           s             14  0
006700070724     d wdat            s              8  0
006800070724     d wdymd           s              8  0
006900070724     d wora            s              6  0
007000070724     d ogddaon         s              8  0
007100070724     d dataiso         s               d   datfmt(*iso)
007200070724     d dataeur         s               d   datfmt(*eur)
007300070724      *___________________________________________________
007400070724     C     *entry        plist
007500070724     C                   PARM                    KPJBA
007600070724     c                   if        kpjbu = ' '
007700070724     c                   clear                   fior67ds
007800070724     c                   move      '1'           or67erro
007900070724     c                   goto      fine
008000070724     c                   else
008100070724     C                   MOVEL     KPJBU         fior67ds
008200070724     c                   end
008300070724     c                   z-add     or67nfvi      nfvk
008400070724     c                   clear                   or67rpklo
008500070724     c                   clear                   or67rvluo
008600070724     c                   clear                   or67rnftlo
008700070724     c                   clear                   or67rnblo
008800070724     c                   clear                   or67rpklao
008900070724     c                   clear                   or67rvluao
009000070724     c                   clear                   or67rstpao
009100070724     c                   clear                   or67rnblao
009200070724     c                   clear                   or67rnbaao
009300070724     c                   clear                   or67erro
009400070724      *
009500070724     C                   time                    w0140
009600070724     C                   move      w0140         wdat
009700070724     C                   movel     w0140         wora
009800070724     c                   move      wdat          dataeur
009900070724     c                   move      dataeur       dataiso
010000070724     c                   move      dataiso       wdymd
010100070724
010200070724     c     or67fgsi      chain     azorg01l
010300070724     c                   if        %found(azorg01l)
010400070724     c                   movel     orgde7        og147
010500070724     c                   if        §ogddao = ' '
010600070724     c                   move      '1'           or67erro
010700070724     c                   goto      fine
010800070724     c                   else
010900070724     c                   move      §ogddao       ogddaon
011000070724     c                   endif
011100070724      * leggo rekord di DST per assumere i valori MASSIMI
011200070724     C     kdst          chain     fidst03l
011300070724     c                   if        %found(fidst03l)
011400070724     C     dst_nrr       chain(e)  fidst00f
011500070724     c                   if        %error
011600070724     c                   exsr      geslck
011700070724     c                   if        ul82§sts = 'A'
011800070724     c                   move      '1'           or67erro
011900070724     c                   goto      fine
012000070724     c                   else
012100070724     C     dst_nrr       chain     fidst00f
012200070724     c                   endif
012300070724     c                   endif
012400070724     c                   z-add     DSTrpkl       or67rpklo
012500070724     c                   z-add     DSTrvlu       or67rvluo
012600070724     c                   z-add     DSTrnftl      or67rnftlo
012700070724     c                   z-add     DSTrnbl       or67rnblo
012800070724      * se è fase di test elabora i dati da ORG per agganciare gli
012900070724      * ORM   della distinta altrimenti legge direttamente ORM
013000070724     c                   if        wdymd  < ogddaon
013100070724     c                   exsr      elabORG
013200070724     c                   else
013300070724     c                   exsr      elabORM
013400070724     c                   endif
013500070724      * imposto numero fermate
013600070227     c                   z-add     1             x
013700070724     c     como70b       lookup    fer(x)                                 80
013800070724     c   80              eval      or67rstpao = x - 1
013900070420      * scrive file con totali per distinta giro
014000071001      * solo se viene richiesto di essere aggiornato
014100071001     c                   If        or67updi = ' '
014200070724     c                   exsr      scrividst6
014300070724      * Aggiorno FIDST00F
014400070724     c                   eval      dstrpkla = or67rpklao
014500070724     c                   eval      dstrVLUa = or67rvluao
014600070724     c                   eval      dstrstpa = or67rstpao
014700070724     c                   eval      DSTrnblA = or67rnblao
014800070724     c                   eval      DSTrnbaA = or67rnbaao
014900070517     c                   update    fidst00
015000070724      * se richiesto commit
015100070724     c                   if        or67cmti = 'S'
015200070516     c                   commit
015300070516     c                   end
015400071001
015500071001     c                   endif
015600070226      *non trovato DST
015700070226     c                   else
015800070724     c                   move      '1'           or67erro
015900070226     c                   goto      fine
016000070226     c                   endif
016100070226
016200070226     c                   endif
016300070226
016400070517     c     fine          tag
016500070724     c                   eval      kpjbu = fior67ds
016600070226     c                   seton                                        lr
016700070724      *___________________________________________________
016800070724     c     elabORG       begsr
016900070724      *___________________________________________________
017000070724     c     korg          setll     fnorg03l
017100070724     c                   do        *hival
017200070724     c     korg          reade     fnorg03l
017300070724     c                   if        %eof(fnorg03l)
017400070724     c                   leave
017500070724     c                   endif
017600070724
017700070724     c                   if        orgnftl = 99999
017800070724     c                   movel     'M'           como11
017900070724     c                   else
018000070724     c                   movel     'A'           como11
018100070724     c                   end
018200070724     c                   move      orgcgi        como11
018300070724
018400070724     c     korm1         chain     fnorm01l
018500070724     c                   if        %found(fnorm01l)
018600070724      * memorizza fermate da ragione sociale/indirizzo
018700070724     c                   exsr      fermate
018800070724      * sommo peso e volume ai totali valori assegnati
018900071122     c******             add       orgpkg        or67rpklao
019000071122     c     or67rpklao    add       orgpkg        wpeso
019100071122     c                   If        Wpeso >  999999,9
019200071122     c                   eval      or67rpklao = 999999,9
019300071122     c                   else
019400071122     c                   eval      or67rPKLao = or67rpklao + orgpkg
019500071122     c                   endif
019600071122     c*****              add       orgvlm        or67rvluao
019700071122     c     or67rvluao    add       orgvlm        wvolume
019800071122     c                   If        Wvolume >  99,999
019900071122     c                   eval      or67rvluao = 99,999
020000071122     c                   else
020100071122     c                   eval      or67rVLUao = or67rvluao + orgvlm
020200071122     c                   endif
020300070724      * ORM
020400070724     c                   add       1             or67rnblao
020500070724      * Bancali
020600070724     c                   add       ormbnc        or67rnbaao
020700070724      *preparo skiere per distinta
020800070724     c                   exsr      carica
020900070724     c
021000070724     c                   endif
021100070724     c                   enddo
021200070724      *
021300070724    1C                   endsr
021400070724      *___________________________________________________
021500070724     c     elabORM       begsr
021600070724      *___________________________________________________
021700070724
021800070724     C/EXEC SQL
021900080314     C+ DECLARE A1 CURSOR FOR SELECT distinct fnorm01l.*,
022000080314     C+ fnorf01l.orffgs, fnorf01l.orfndc,
022100080314     C+ fnorf01l.orfddc, substr(fnorf01l.orfflo, 1, 1) as §orfass,
022200070724     C+ fnorg01l.*
022300070724     C+ FROM (fnorm01l join fnorf01l on
022400070724     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
022500070724     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
022600070724     C+ ormofo=orfore) join fnorg01l on
022700070724     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
022800070724     C+ ormnrv=orgnrv
022900070724     C+ WHERE orfndc=:or67nfvi and orfddc=:or67dfvi and
023000070724     C+ orffgs =:or67fgsi
023100070724     C+ ORDER BY fnorm01l.ormpoe, fnorm01l.ormnsr, fnorm01l.ormnor,
023200070724     C+ fnorm01l.ormnrv
023300070724     C/END-EXEC
023400070724
023500070724     C/EXEC SQL
023600070724     C+ OPEN A1
023700070724     C/END-EXEC
023800070724
023900070724     c                   do        *hival
024000070724
024100070724     C/EXEC SQL
024200070724     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorfds, :fnorgds
024300070724     C/END-EXEC
024400070724
024500070724     c                   select
024600070724     c                   when      sqlcod = 100
024700070724     c                   leave
024800070724     c                   when      sqlcod < 0
024900070724      * record allocato
025000070724     c                   if        sqlcod <> -913
025100070724     c                   eval      *inh1 = *on
025200070724     c                   endif
025300070724     c                   move      '1'           or67erro
025400070724     c                   goto      fine
025500070724
025600070724     c                   other
025700070724
025800070724     c                   if        §orfass = 'M'
025900070724     c                   movel     'M'           como11
026000070724     c                   end
026100070724     c                   if        §orfass = 'A'
026200070724     c                   movel     'A'           como11
026300070724     c                   end
026400070724     c                   move      orgcgi        como11
026500070724      * memorizza fermate da ragione sociale/indirizzo
026600070724     c                   exsr      fermate
026700070724      * sommo peso e volume ai totali valori assegnati
026800071123     c****               add       orgpkg        or67rpklao
026900071123     c***                add       orgvlm        or67rvluao
027000071123     c     or67rpklao    add       orgpkg        wpeso
027100071123     c                   If        Wpeso >  999999,9
027200071123     c                   eval      or67rpklao = 999999,9
027300071123     c                   else
027400071123     c                   eval      or67rPKLao = or67rpklao + orgpkg
027500071123     c                   endif
027600071123     c     or67rvluao    add       orgvlm        wvolume
027700071123     c                   If        Wvolume >  99,999
027800071123     c                   eval      or67rvluao = 99,999
027900071123     c                   else
028000071123     c                   eval      or67rVLUao = or67rvluao + orgvlm
028100071123     c                   endif
028200070724      * ORM
028300070724     c                   add       1             or67rnblao
028400070724      * Bancali
028500070724     c                   add       ormbnc        or67rnbaao
028600070724      *preparo skiere per distinta
028700070724     c                   exsr      carica
028800070724     c
028900070724     c                   endsl
029000070724
029100070724     c                   enddo
029200070724
029300070724     C/EXEC SQL
029400070724     C+ CLOSE a1
029500070724     C/END-EXEC
029600070724
029700070724     c                   endsr
029800070724      *___________________________________________________
029900070724     c     fermate       begsr
030000070724      *___________________________________________________
030100070724     c                   z-add     1             x
030200070724     c                   movel     ormrsr        como70
030300070724     c                   move      orminr        como70
030400070724     c     como70        lookup    fer                                    80
030500070724     c                   if        not *in80
030600070724     c     como70b       lookup    fer(x)                                 80
030700070724     c   80              movel     como70        fer(x)
030800070724     c                   endif
030900070724
031000070724    1c                   endsr
031100070724      *___________________________________________________
031200070724     c     carica        begsr
031300070724      *___________________________________________________
031400070724      * totalizza pesi volumi orm   per giro
031500070420     c                   z-add     1             x
031600070516     c     como11        lookup    giro(x)                                80
031700070420     c                   if        *in80
031800070724     c                   add       orgpkg        peso(x)
031900070724     c                   add       orgvlm        volu(x)
032000070724     c                   add       1             orm(x)
032100070724     c                   add       ormbnc        bnc(x)
032200070420     c                   else
032300070516     c     como11b       lookup    giro(x)                                80
032400070420     c                   if        *in80
032500070516     c                   movel     como11        giro(x)
032600070724     c                   add       orgpkg        peso(x)
032700070724     c                   add       orgvlm        volu(x)
032800070724     c                   add       1             orm(x)
032900070724     c                   add       ormbnc        bnc(x)
033000070420     c                   endif
033100070420     c                   endif
033200070420
033300070420    1C                   endsr
033400070420      *___________________________________________________
033500070724     C     scrividst6    begsr
033600070420      *___________________________________________________
033700070420      * pulizia file elimina i rekord esistenti per quella distinta
033800070724     c     kdst6         setll     fidst61l
033900070420     c                   do        *hival
034000070724     c     kdst6         reade     fidst61l
034100070724     c                   if        %eof(fidst61l)
034200070420     c                   leave
034300070420     c                   endif
034400070724     c                   delete    fidst600
034500070420     c                   enddo
034600070420      * scrive il nuovo dettaglio distinta giro
034700070420     c     1             do        500           x
034800070420     c                   if        giro(x) = *blank
034900070420     c                   leave
035000070420     c                   endif
035100070420     c                   move      giro(x)       girok
035200070530     c                   movel     giro(x)       tassok            1
035300070724     c                   clear                   dst6PRGG
035400070724     c                   clear                   dst6SQL
035500070530     c                   if        tassok = 'A'
035600071031     c     kdst52        chain     fidst52l
035700071031     c                   if        %found(fidst52l)
035800070724     c                   z-add     dst5prgg      dst6PRGG
035900070724     c                   z-add     dst5sql       dst6SQL
036000070420     c                   endif
036100070530     c                   endif
036200070724     c                   z-add     4             dst6npg
036300070724     c                   z-add     or67fgsi      dst6FGS
036400070724     c                   z-add     or67nfvi      dst6Nfv
036500070724     c                   z-add     or67dfvi      dst6Dfv
036600070724     c                   z-add     or67fgsi      dst6FGSG
036700070724     c                   movel     giro(x)       dst6tass
036800070724     c                   move      giro(x)       dst6cgi
036900070724     c                   z-add     peso(x)       dst6PKL
037000070724     c                   z-add     volu(x)       dst6VLU
037100070724     c                   z-add     orm(x)        dst6nbl
037200070724     c                   z-add     bnc(x)        dst6nba
037300070724     c                   move      *blank        dst6flo
037400070724     c                   write     fidst600
037500070724     c                   enddo
037600070724
037700070724    1C                   endsr
037800070724      *-------------------------------------------------
037900070724     c     *inzsr        begsr
038000070724      *-------------------------------------------------
038100070724     c     kdst          klist
038200070724     c                   kfld                    or67fgsi
038300070724     c                   kfld                    catk
038400070724     c                   kfld                    or67nfvi
038500070724
038600070724     c     kdst52        klist
038700070724     c                   kfld                    or67fgsi
038800070724     c                   kfld                    or67dfvi
038900070724     c                   kfld                    or67fgsi
039000070724     c                   kfld                    girok
039100070724     c                   kfld                    or67nfvi
039200070724
039300070724     c     korm1         klist
039400070724     c                   kfld                    orgpoe
039500070724     c                   kfld                    orgnsr
039600070724     c                   kfld                    orgnor
039700070724     c                   kfld                    orgnrv
039800070724
039900070724     c     korg          klist
040000070724     c                   kfld                    or67fgsi
040100070724     c                   kfld                    nfvk
040200070724     c                   kfld                    or67dfvi
040300070724
040400070724     c     korg1         klist
040500070724     c                   kfld                    ormpoe
040600070724     c                   kfld                    ormnsr
040700070724     c                   kfld                    ormnor
040800070724     c                   kfld                    ormnrv
040900070724
041000070724     c     kdst6         klist
041100070724     c                   kfld                    or67fgsi
041200070724     c                   kfld                    catk
041300070724     c                   kfld                    or67nfvi
041400070724     c                   kfld                    or67dfvi
041500070724
041600070724
041700070724     c                   endsr
041800070724      *----------------------------------------------------*
041900070724     c     geslck        begsr
042000070724      *----------------------------------------------------*
042100070724     c                   clear                   trul82ds
042200070724     c                   Eval      ul82§rrn = dst_nrr
042300070724     c                   Eval      UL82§FIL = 'FIDST00F'
042400070724     c                   Eval      UL82§WIN = 'S'
042500070724     c                   Eval      UL82§num = 2
042600070724     c                   Eval      UL82§att = 2
042700070724     c                   eval      ul82§mss = ErrMsg(1)
042800070724     c                   Eval      UL82§msw = 'La Distinta '
042900070724     c                             + %editc(or67nfvi:'Z') + ' è già utilizzata -
043000070724     c                             da un altro lavoro e non è manutenzionabile'
043100070724      * Effettuo la chiamata al *PGM d utilità
043200070724     c                   Call(e)   'TRUL82R'
043300070724     c                   Parm                    Trul82ds
043400070724
043500070724     C                   ENDSR
043600070724** ErrMsg
043700070724Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
