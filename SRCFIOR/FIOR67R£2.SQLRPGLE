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
013201071001      * solo se viene richiesto di essere aggiornato
013202071001     c                   If        or67updi = ' '
013300070724     c                   exsr      scrividst6
013400070724      * Aggiorno FIDST00F
013500070724     c                   eval      dstrpkla = or67rpklao
013600070724     c                   eval      dstrVLUa = or67rvluao
013700070724     c                   eval      dstrstpa = or67rstpao
013800070724     c                   eval      DSTrnblA = or67rnblao
013900070724     c                   eval      DSTrnbaA = or67rnbaao
014000070517     c                   update    fidst00
014100070724      * se richiesto commit
014200070724     c                   if        or67cmti = 'S'
014300070516     c                   commit
014400070516     c                   end
014401071001
014402071001     c                   endif
014500070226      *non trovato DST
014600070226     c                   else
014700070724     c                   move      '1'           or67erro
014800070226     c                   goto      fine
014900070226     c                   endif
015000070226
015100070226     c                   endif
015200070226
015300070517     c     fine          tag
015400070724     c                   eval      kpjbu = fior67ds
015500070226     c                   seton                                        lr
015600070724      *___________________________________________________
015700070724     c     elabORG       begsr
015800070724      *___________________________________________________
015900070724     c     korg          setll     fnorg03l
016000070724     c                   do        *hival
016100070724     c     korg          reade     fnorg03l
016200070724     c                   if        %eof(fnorg03l)
016300070724     c                   leave
016400070724     c                   endif
016500070724
016600070724     c                   if        orgnftl = 99999
016700070724     c                   movel     'M'           como11
016800070724     c                   else
016900070724     c                   movel     'A'           como11
017000070724     c                   end
017100070724     c                   move      orgcgi        como11
017200070724
017300070724     c     korm1         chain     fnorm01l
017400070724     c                   if        %found(fnorm01l)
017500070724      * memorizza fermate da ragione sociale/indirizzo
017600070724     c                   exsr      fermate
017700070724      * sommo peso e volume ai totali valori assegnati
017800070724     c                   add       orgpkg        or67rpklao
017900070724     c                   add       orgvlm        or67rvluao
018000070724      * ORM
018100070724     c                   add       1             or67rnblao
018200070724      * Bancali
018300070724     c                   add       ormbnc        or67rnbaao
018400070724      *preparo skiere per distinta
018500070724     c                   exsr      carica
018600070724     c
018700070724     c                   endif
018800070724     c                   enddo
018900070724      *
019000070724    1C                   endsr
019100070724      *___________________________________________________
019200070724     c     elabORM       begsr
019300070724      *___________________________________________________
019400070724
019500070724     C/EXEC SQL
019600070724     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorf01l.*,
019700070724     C+ fnorg01l.*
019800070724     C+ FROM (fnorm01l join fnorf01l on
019900070724     C+ ormpoe=orfpoe and ormnsr=orfnsr and ormnor=orfnor and
020000070724     C+ ormnrv=orfnrv and ormfao=orffar and ormdfo=orfdae and
020100070724     C+ ormofo=orfore) join fnorg01l on
020200070724     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
020300070724     C+ ormnrv=orgnrv
020400070724     C+ WHERE orfndc=:or67nfvi and orfddc=:or67dfvi and
020500070724     C+ orffgs =:or67fgsi
020600070724     C+ ORDER BY fnorm01l.ormpoe, fnorm01l.ormnsr, fnorm01l.ormnor,
020700070724     C+ fnorm01l.ormnrv
020800070724     C/END-EXEC
020900070724
021000070724     C/EXEC SQL
021100070724     C+ OPEN A1
021200070724     C/END-EXEC
021300070724
021400070724     c                   do        *hival
021500070724
021600070724     C/EXEC SQL
021700070724     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorfds, :fnorgds
021800070724     C/END-EXEC
021900070724
022000070724     c                   select
022100070724     c                   when      sqlcod = 100
022200070724     c                   leave
022300070724     c                   when      sqlcod < 0
022400070724      * record allocato
022500070724     c                   if        sqlcod <> -913
022600070724     c                   eval      *inh1 = *on
022700070724     c                   endif
022800070724     c                   move      '1'           or67erro
022900070724     c                   goto      fine
023000070724
023100070724     c                   other
023200070724
023300070724     c                   movel     orfflo        dorf01
023400070724     c                   if        §orfass = 'M'
023500070724     c                   movel     'M'           como11
023600070724     c                   end
023700070724     c                   if        §orfass = 'A'
023800070724     c                   movel     'A'           como11
023900070724     c                   end
024000070724     c                   move      orgcgi        como11
024100070724      * memorizza fermate da ragione sociale/indirizzo
024200070724     c                   exsr      fermate
024300070724      * sommo peso e volume ai totali valori assegnati
024400070724     c                   add       orgpkg        or67rpklao
024500070724     c                   add       orgvlm        or67rvluao
024600070724      * ORM
024700070724     c                   add       1             or67rnblao
024800070724      * Bancali
024900070724     c                   add       ormbnc        or67rnbaao
025000070724      *preparo skiere per distinta
025100070724     c                   exsr      carica
025200070724     c
025300070724     c                   endsl
025400070724
025500070724     c                   enddo
025600070724
025700070724     C/EXEC SQL
025800070724     C+ CLOSE a1
025900070724     C/END-EXEC
026000070724
026100070724     c                   endsr
026200070724      *___________________________________________________
026300070724     c     fermate       begsr
026400070724      *___________________________________________________
026500070724     c                   z-add     1             x
026600070724     c                   movel     ormrsr        como70
026700070724     c                   move      orminr        como70
026800070724     c     como70        lookup    fer                                    80
026900070724     c                   if        not *in80
027000070724     c     como70b       lookup    fer(x)                                 80
027100070724     c   80              movel     como70        fer(x)
027200070724     c                   endif
027300070724
027400070724    1c                   endsr
027500070724      *___________________________________________________
027600070724     c     carica        begsr
027700070724      *___________________________________________________
027800070724      * totalizza pesi volumi orm   per giro
027900070420     c                   z-add     1             x
028000070516     c     como11        lookup    giro(x)                                80
028100070420     c                   if        *in80
028200070724     c                   add       orgpkg        peso(x)
028300070724     c                   add       orgvlm        volu(x)
028400070724     c                   add       1             orm(x)
028500070724     c                   add       ormbnc        bnc(x)
028600070420     c                   else
028700070516     c     como11b       lookup    giro(x)                                80
028800070420     c                   if        *in80
028900070516     c                   movel     como11        giro(x)
029000070724     c                   add       orgpkg        peso(x)
029100070724     c                   add       orgvlm        volu(x)
029200070724     c                   add       1             orm(x)
029300070724     c                   add       ormbnc        bnc(x)
029400070420     c                   endif
029500070420     c                   endif
029600070420
029700070420    1C                   endsr
029800070420      *___________________________________________________
029900070724     C     scrividst6    begsr
030000070420      *___________________________________________________
030100070420      * pulizia file elimina i rekord esistenti per quella distinta
030200070724     c     kdst6         setll     fidst61l
030300070420     c                   do        *hival
030400070724     c     kdst6         reade     fidst61l
030500070724     c                   if        %eof(fidst61l)
030600070420     c                   leave
030700070420     c                   endif
030800070724     c                   delete    fidst600
030900070420     c                   enddo
031000070420      * scrive il nuovo dettaglio distinta giro
031100070420     c     1             do        500           x
031200070420     c                   if        giro(x) = *blank
031300070420     c                   leave
031400070420     c                   endif
031500070420     c                   move      giro(x)       girok
031600070530     c                   movel     giro(x)       tassok            1
031700070724     c                   clear                   dst6PRGG
031800070724     c                   clear                   dst6SQL
031900070530     c                   if        tassok = 'A'
032000070724     c     kdst6         chain     fidst61l
032100070724     c                   if        %found(fidst61l)
032200070724     c                   z-add     dst5prgg      dst6PRGG
032300070724     c                   z-add     dst5sql       dst6SQL
032400070420     c                   endif
032500070530     c                   endif
032600070724     c                   z-add     4             dst6npg
032700070724     c                   z-add     or67fgsi      dst6FGS
032800070724     c                   z-add     or67nfvi      dst6Nfv
032900070724     c                   z-add     or67dfvi      dst6Dfv
033000070724     c                   z-add     or67fgsi      dst6FGSG
033100070724     c                   movel     giro(x)       dst6tass
033200070724     c                   move      giro(x)       dst6cgi
033300070724     c                   z-add     peso(x)       dst6PKL
033400070724     c                   z-add     volu(x)       dst6VLU
033500070724     c                   z-add     orm(x)        dst6nbl
033600070724     c                   z-add     bnc(x)        dst6nba
033700070724     c                   move      *blank        dst6flo
033800070724     c                   write     fidst600
033900070724     c                   enddo
034000070724
034100070724    1C                   endsr
034200070724      *-------------------------------------------------
034300070724     c     *inzsr        begsr
034400070724      *-------------------------------------------------
034500070724     c     kdst          klist
034600070724     c                   kfld                    or67fgsi
034700070724     c                   kfld                    catk
034800070724     c                   kfld                    or67nfvi
034900070724
035000070724     c     kdst52        klist
035100070724     c                   kfld                    or67fgsi
035200070724     c                   kfld                    or67dfvi
035300070724     c                   kfld                    or67fgsi
035400070724     c                   kfld                    girok
035500070724     c                   kfld                    or67nfvi
035600070724
035700070724     c     korm1         klist
035800070724     c                   kfld                    orgpoe
035900070724     c                   kfld                    orgnsr
036000070724     c                   kfld                    orgnor
036100070724     c                   kfld                    orgnrv
036200070724
036300070724     c     korg          klist
036400070724     c                   kfld                    or67fgsi
036500070724     c                   kfld                    nfvk
036600070724     c                   kfld                    or67dfvi
036700070724
036800070724     c     korg1         klist
036900070724     c                   kfld                    ormpoe
037000070724     c                   kfld                    ormnsr
037100070724     c                   kfld                    ormnor
037200070724     c                   kfld                    ormnrv
037300070724
037400070724     c     kdst6         klist
037500070724     c                   kfld                    or67fgsi
037600070724     c                   kfld                    catk
037700070724     c                   kfld                    or67nfvi
037800070724     c                   kfld                    or67dfvi
037900070724
038000070724
038100070724     c                   endsr
038200070724      *----------------------------------------------------*
038300070724     c     geslck        begsr
038400070724      *----------------------------------------------------*
038500070724     c                   clear                   trul82ds
038600070724     c                   Eval      ul82§rrn = dst_nrr
038700070724     c                   Eval      UL82§FIL = 'FIDST00F'
038800070724     c                   Eval      UL82§WIN = 'S'
038900070724     c                   Eval      UL82§num = 2
039000070724     c                   Eval      UL82§att = 2
039100070724     c                   eval      ul82§mss = ErrMsg(1)
039200070724     c                   Eval      UL82§msw = 'La Distinta '
039300070724     c                             + %editc(or67nfvi:'Z') + ' è già utilizzata -
039400070724     c                             da un altro lavoro e non è manutenzionabile'
039500070724      * Effettuo la chiamata al *PGM d utilità
039600070724     c                   Call(e)   'TRUL82R'
039700070724     c                   Parm                    Trul82ds
039800070724
039900070724     C                   ENDSR
040000070724** ErrMsg
040100070724Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
