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
001100130206     Ffidst09l  if   e           k disk    infds(dsinf)
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
002600170118     d orffgs                  1      3  0
002700170118     d orfndc                  4      9  0
002800170118     d orfddc                 10     17  0
002900170118     d §orfass                18     18
003000080314
003100070724     d trul82ds      e ds
003200070724     D kpjba         E DS
003300070724     D fior67ds      E DS
003400070724     D fnormds       E DS                  extname(fnorm00f)
003500070724     D fnorgds       E DS                  extname(fnorg00f)
003600070724     D og147         E DS
003700170111     d FIOR90DS      e ds
003800070724      *--------------------------------------------------------------*
003900070724      *   schiere
004000070724      *--------------------------------------------------------------*
004100070724
004200070724     d Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
004300070724
004400070724     d fer             s             70    dim(500)
004500070724      * skiere per riempimento file
004600070724     d giro            s             11    dim(500)
004700070724     d peso            s              7  2 dim(500)
004800070724     d volu            s              5  3 dim(500)
004900070724     d orm             s              5  0 dim(500)
005000070724     d bnc             s              5  0 dim(500)
005100070724      *--------------------------------------------------------------*
005200070724      *   variabili
005300070724      *--------------------------------------------------------------*
005400070724
005500070724     d como11b         s             11
005600070724     d como11          s             11
005700070724     d como70          s             70
005800070724     d como70b         s             70
005900070724
006000070724     d x               s              3  0
006100070724     d xx              s              3  0
006200070724     d catk            s              1  0 inz(4)
006300070724     d nfvk            s              7  0
006400070724     d girok           s             10
006500071122     d wpeso           s             14  1 INZ
006600071122     d wvolume         s             10  3 INZ
006700070724     d w0140           s             14  0
006800070724     d wdat            s              8  0
006900070724     d wdymd           s              8  0
007000070724     d wora            s              6  0
007100070724     d ogddaon         s              8  0
007200070724     d dataiso         s               d   datfmt(*iso)
007300070724     d dataeur         s               d   datfmt(*eur)
007400170111
007500170111      *------------------------------------------------------------------------*
007600170111      // - Pgm controllo fasi
007700170111     d FIOR90R         pr                  extpgm('FIOR90R')
007800170111     d  kpjba                              likeds(kpjba)
007900170111     d  fior90ds                           likeds(fior90ds)
008000070724      *___________________________________________________
008100070724     C     *entry        plist
008200070724     C                   PARM                    KPJBA
008300070724     c                   if        kpjbu = ' '
008400070724     c                   clear                   fior67ds
008500070724     c                   move      '1'           or67erro
008600070724     c                   goto      fine
008700070724     c                   else
008800070724     C                   MOVEL     KPJBU         fior67ds
008900070724     c                   end
009000070724     c                   z-add     or67nfvi      nfvk
009100070724     c                   clear                   or67rpklo
009200070724     c                   clear                   or67rvluo
009300070724     c                   clear                   or67rnftlo
009400070724     c                   clear                   or67rnblo
009500070724     c                   clear                   or67rpklao
009600070724     c                   clear                   or67rvluao
009700070724     c                   clear                   or67rstpao
009800070724     c                   clear                   or67rnblao
009900070724     c                   clear                   or67rnbaao
010000070724     c                   clear                   or67erro
010100170111
010200170111      /free
010300170111      //?Come prima cosa verifico se la distinta ha già la fase
010400170111      //?'RCH' o 'RQE'
010500170111      //?Prima provo con la 'RCH'
010600170111        clear FIOR90DS;
010700170111        OR90tla = 'C';
010800170111        OR90fas = 'RCH';
010900170111        OR90fgs = OR67fgsi;
011000170111        OR90ndc = OR67nfvi;
011100170111        fior90r (kpjba:fior90ds);
011200170111      //?Poi provo con la fase 'RQE'
011300170111        IF  OR90ndcok = *blanks;
011400170111          clear FIOR90DS;
011500170111          OR90tla = 'C';
011600170111          OR90fas = 'RQE';
011700170111          OR90fgs = OR67fgsi;
011800170111          OR90ndc = OR67nfvi;
011900170111          fior90r (kpjba:fior90ds);
012000170111        ENDIF;
012100170111      /end-free
012200170111      //?Ho trovato una delle due fasi esco dal pgm
012300170111     c                   IF        OR90ndcok <> *blanks
012400170111     c                   goto      fine
012500170111     c                   ENDIF
012600070724      *
012700170111     c                   time                    w0140
012800070724     C                   move      w0140         wdat
012900070724     C                   movel     w0140         wora
013000070724     c                   move      wdat          dataeur
013100070724     c                   move      dataeur       dataiso
013200070724     c                   move      dataiso       wdymd
013300070724
013400070724     c     or67fgsi      chain     azorg01l
013500070724     c                   if        %found(azorg01l)
013600070724     c                   movel     orgde7        og147
013700070724     c                   if        §ogddao = ' '
013800070724     c                   move      '1'           or67erro
013900070724     c                   goto      fine
014000070724     c                   else
014100070724     c                   move      §ogddao       ogddaon
014200070724     c                   endif
014300070724      * leggo rekord di DST per assumere i valori MASSIMI
014400130206     C     kdst          chain     fidst09l
014500130206     c                   if        %found(fidst09l)
014600070724     C     dst_nrr       chain(e)  fidst00f
014700070724     c                   if        %error
014800070724     c                   exsr      geslck
014900070724     c                   if        ul82§sts = 'A'
015000070724     c                   move      '1'           or67erro
015100070724     c                   goto      fine
015200070724     c                   else
015300070724     C     dst_nrr       chain     fidst00f
015400070724     c                   endif
015500070724     c                   endif
015600070724     c                   z-add     DSTrpkl       or67rpklo
015700070724     c                   z-add     DSTrvlu       or67rvluo
015800070724     c                   z-add     DSTrnftl      or67rnftlo
015900070724     c                   z-add     DSTrnbl       or67rnblo
016000070724      * se è fase di test elabora i dati da ORG per agganciare gli
016100070724      * ORM   della distinta altrimenti legge direttamente ORM
016200070724     c                   if        wdymd  < ogddaon
016300070724     c                   exsr      elabORG
016400070724     c                   else
016500070724     c                   exsr      elabORM
016600070724     c                   endif
016700070724      * imposto numero fermate
016800070227     c                   z-add     1             x
016900070724     c     como70b       lookup    fer(x)                                 80
017000070724     c   80              eval      or67rstpao = x - 1
017100070420      * scrive file con totali per distinta giro
017200071001      * solo se viene richiesto di essere aggiornato
017300071001     c                   If        or67updi = ' '
017400070724     c                   exsr      scrividst6
017500070724      * Aggiorno FIDST00F
017600070724     c                   eval      dstrpkla = or67rpklao
017700070724     c                   eval      dstrVLUa = or67rvluao
017800070724     c                   eval      dstrstpa = or67rstpao
017900070724     c                   eval      DSTrnblA = or67rnblao
018000070724     c                   eval      DSTrnbaA = or67rnbaao
018100070517     c                   update    fidst00
018200070724      * se richiesto commit
018300070724     c                   if        or67cmti = 'S'
018400070516     c                   commit
018500070516     c                   end
018600071001
018700071001     c                   endif
018800070226      *non trovato DST
018900070226     c                   else
019000070724     c                   move      '1'           or67erro
019100070226     c                   goto      fine
019200070226     c                   endif
019300070226
019400070226     c                   endif
019500070226
019600070517     c     fine          tag
019700070724     c                   eval      kpjbu = fior67ds
019800070226     c                   seton                                        lr
019900070724      *___________________________________________________
020000070724     c     elabORG       begsr
020100070724      *___________________________________________________
020200070724     c     korg          setll     fnorg03l
020300070724     c                   do        *hival
020400070724     c     korg          reade     fnorg03l
020500070724     c                   if        %eof(fnorg03l)
020600070724     c                   leave
020700070724     c                   endif
020800070724
020900070724     c                   if        orgnftl = 99999
021000070724     c                   movel     'M'           como11
021100070724     c                   else
021200070724     c                   movel     'A'           como11
021300070724     c                   end
021400070724     c                   move      orgcgi        como11
021500070724
021600070724     c     korm1         chain     fnorm01l
021700070724     c                   if        %found(fnorm01l)
021800070724      * memorizza fermate da ragione sociale/indirizzo
021900070724     c                   exsr      fermate
022000070724      * sommo peso e volume ai totali valori assegnati
022100071122     c     or67rpklao    add       orgpkg        wpeso
022200071122     c                   If        Wpeso >  999999,9
022300071122     c                   eval      or67rpklao = 999999,9
022400071122     c                   else
022500071122     c                   eval      or67rPKLao = or67rpklao + orgpkg
022600071122     c                   endif
022700071122     c     or67rvluao    add       orgvlm        wvolume
022800071122     c                   If        Wvolume >  99,999
022900071122     c                   eval      or67rvluao = 99,999
023000071122     c                   else
023100071122     c                   eval      or67rVLUao = or67rvluao + orgvlm
023200071122     c                   endif
023300070724      * ORM
023400070724     c                   add       1             or67rnblao
023500070724      * Bancali
023600070724     c                   add       ormbnc        or67rnbaao
023700070724      *preparo skiere per distinta
023800070724     c                   exsr      carica
023900070724     c
024000070724     c                   endif
024100070724     c                   enddo
024200070724      *
024300070724    1C                   endsr
024400070724      *___________________________________________________
024500070724     c     elabORM       begsr
024600070724      *___________________________________________________
024700070724
024800170112      //?Leggo gli ORM che hanno almeno una fase 400 con la distinta passata
024900170112      /free
025000170112        exec sql
025100170112        DECLARE A1 cursor for
025200170118        SELECT distinct FNORM01L.*,
025300170112                        FNORF01L.ORFfgs, FNORF01L.ORFndc,
025400170112                        FNORF01L.ORFddc,
025500170112                        substr(FNORF01L.ORFflo, 1, 1) as §ORFass,
025600170118                        FNORG01L.*
025700170118        FROM (FNORm01L join FNORf01L on
025800170118              ORMpoe = ORFpoe and ORMnsr = ORFnsr and
025900170118              ORMnor = ORFnor and ORMnrv = ORFnrv and
026000170118              ORMfao = ORFfar and ORMdfo = ORFdae and
026100170118              ORMofo = ORFore)
026200170112              join FNORG01L on
026300170118              ORMpoe = ORGpoe and ORMnsr = ORGnsr and
026400170118              ORMnor = ORGnor and ORMnrv = ORGnrv
026500170112        WHERE ORFndc = :OR67nfvi and ORFddc = :OR67dfvi and
026600170118              ORFfgs = :OR67fgsi
026700170118        ORDER BY FNORM01L.ORMpoe, FNORM01L.ORMnsr,
026800170118                 FNORM01L.ORMnor, FNORM01L.ORMnrv;
026900170112      /end-free
027000070724
027100070724     C/EXEC SQL
027200070724     C+ OPEN A1
027300070724     C/END-EXEC
027400070724
027500070724     c                   do        *hival
027600070724
027700070724     C/EXEC SQL
027800170118     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorfds, :fnorgds
027900070724     C/END-EXEC
028000070724
028100070724     c                   select
028200070724     c                   when      sqlcod = 100
028300070724     c                   leave
028400070724     c                   when      sqlcod < 0
028500070724      * record allocato
028600070724     c                   if        sqlcod <> -913
028700070724     c                   eval      *inh1 = *on
028800070724     c                   endif
028900070724     c                   move      '1'           or67erro
029000070724     c                   goto      fine
029100070724
029200070724     c                   other
029300070724
029400070724     c                   if        §orfass = 'M'
029500070724     c                   movel     'M'           como11
029600070724     c                   end
029700070724     c                   if        §orfass = 'A'
029800070724     c                   movel     'A'           como11
029900070724     c                   end
030000070724     c                   move      orgcgi        como11
030100070724      * memorizza fermate da ragione sociale/indirizzo
030200070724     c                   exsr      fermate
030300070724      * sommo peso e volume ai totali valori assegnati
030400071123     c     or67rpklao    add       orgpkg        wpeso
030500071123     c                   If        Wpeso >  999999,9
030600071123     c                   eval      or67rpklao = 999999,9
030700071123     c                   else
030800071123     c                   eval      or67rPKLao = or67rpklao + orgpkg
030900071123     c                   endif
031000071123     c     or67rvluao    add       orgvlm        wvolume
031100071123     c                   If        Wvolume >  99,999
031200071123     c                   eval      or67rvluao = 99,999
031300071123     c                   else
031400071123     c                   eval      or67rVLUao = or67rvluao + orgvlm
031500071123     c                   endif
031600070724      * ORM
031700070724     c                   add       1             or67rnblao
031800070724      * Bancali
031900070724     c                   add       ormbnc        or67rnbaao
032000070724      *preparo skiere per distinta
032100070724     c                   exsr      carica
032200070724     c
032300070724     c                   endsl
032400070724
032500070724     c                   enddo
032600070724
032700070724     C/EXEC SQL
032800070724     C+ CLOSE a1
032900070724     C/END-EXEC
033000070724
033100070724     c                   endsr
033200070724      *___________________________________________________
033300070724     c     fermate       begsr
033400070724      *___________________________________________________
033500070724     c                   z-add     1             x
033600070724     c                   movel     ormrsr        como70
033700070724     c                   move      orminr        como70
033800070724     c     como70        lookup    fer                                    80
033900070724     c                   if        not *in80
034000070724     c     como70b       lookup    fer(x)                                 80
034100070724     c   80              movel     como70        fer(x)
034200070724     c                   endif
034300070724
034400070724    1c                   endsr
034500070724      *___________________________________________________
034600070724     c     carica        begsr
034700070724      *___________________________________________________
034800070724      * totalizza pesi volumi orm   per giro
034900070420     c                   z-add     1             x
035000070516     c     como11        lookup    giro(x)                                80
035100070420     c                   if        *in80
035200070724     c                   add       orgpkg        peso(x)
035300070724     c                   add       orgvlm        volu(x)
035400070724     c                   add       1             orm(x)
035500070724     c                   add       ormbnc        bnc(x)
035600070420     c                   else
035700070516     c     como11b       lookup    giro(x)                                80
035800070420     c                   if        *in80
035900070516     c                   movel     como11        giro(x)
036000070724     c                   add       orgpkg        peso(x)
036100070724     c                   add       orgvlm        volu(x)
036200070724     c                   add       1             orm(x)
036300070724     c                   add       ormbnc        bnc(x)
036400070420     c                   endif
036500070420     c                   endif
036600070420
036700070420    1C                   endsr
036800070420      *___________________________________________________
036900070724     C     scrividst6    begsr
037000070420      *___________________________________________________
037100070420      * pulizia file elimina i rekord esistenti per quella distinta
037200070724     c     kdst6         setll     fidst61l
037300070420     c                   do        *hival
037400070724     c     kdst6         reade     fidst61l
037500070724     c                   if        %eof(fidst61l)
037600070420     c                   leave
037700070420     c                   endif
037800070724     c                   delete    fidst600
037900070420     c                   enddo
038000070420      * scrive il nuovo dettaglio distinta giro
038100070420     c     1             do        500           x
038200070420     c                   if        giro(x) = *blank
038300070420     c                   leave
038400070420     c                   endif
038500070420     c                   move      giro(x)       girok
038600070530     c                   movel     giro(x)       tassok            1
038700070724     c                   clear                   dst6PRGG
038800070724     c                   clear                   dst6SQL
038900070530     c                   if        tassok = 'A'
039000071031     c     kdst52        chain     fidst52l
039100071031     c                   if        %found(fidst52l)
039200070724     c                   z-add     dst5prgg      dst6PRGG
039300070724     c                   z-add     dst5sql       dst6SQL
039400070420     c                   endif
039500070530     c                   endif
039600070724     c                   z-add     4             dst6npg
039700070724     c                   z-add     or67fgsi      dst6FGS
039800070724     c                   z-add     or67nfvi      dst6Nfv
039900070724     c                   z-add     or67dfvi      dst6Dfv
040000070724     c                   z-add     or67fgsi      dst6FGSG
040100070724     c                   movel     giro(x)       dst6tass
040200070724     c                   move      giro(x)       dst6cgi
040300070724     c                   z-add     peso(x)       dst6PKL
040400070724     c                   z-add     volu(x)       dst6VLU
040500070724     c                   z-add     orm(x)        dst6nbl
040600070724     c                   z-add     bnc(x)        dst6nba
040700070724     c                   move      *blank        dst6flo
040800070724     c                   write     fidst600
040900070724     c                   enddo
041000070724
041100070724    1C                   endsr
041200070724      *-------------------------------------------------
041300070724     c     *inzsr        begsr
041400070724      *-------------------------------------------------
041500070724     c     kdst          klist
041600070724     c                   kfld                    or67fgsi
041700070724     c                   kfld                    catk
041800070724     c                   kfld                    or67nfvi
041900070724
042000070724     c     kdst52        klist
042100070724     c                   kfld                    or67fgsi
042200070724     c                   kfld                    or67dfvi
042300070724     c                   kfld                    or67fgsi
042400070724     c                   kfld                    girok
042500070724     c                   kfld                    or67nfvi
042600070724
042700070724     c     korm1         klist
042800070724     c                   kfld                    orgpoe
042900070724     c                   kfld                    orgnsr
043000070724     c                   kfld                    orgnor
043100070724     c                   kfld                    orgnrv
043200070724
043300070724     c     korg          klist
043400070724     c                   kfld                    or67fgsi
043500070724     c                   kfld                    nfvk
043600070724     c                   kfld                    or67dfvi
043700070724
043800070724     c     korg1         klist
043900070724     c                   kfld                    ormpoe
044000070724     c                   kfld                    ormnsr
044100070724     c                   kfld                    ormnor
044200070724     c                   kfld                    ormnrv
044300070724
044400070724     c     kdst6         klist
044500070724     c                   kfld                    or67fgsi
044600070724     c                   kfld                    catk
044700070724     c                   kfld                    or67nfvi
044800070724     c                   kfld                    or67dfvi
044900070724
045000070724
045100070724     c                   endsr
045200070724      *----------------------------------------------------*
045300070724     c     geslck        begsr
045400070724      *----------------------------------------------------*
045500070724     c                   clear                   trul82ds
045600070724     c                   Eval      ul82§rrn = dst_nrr
045700070724     c                   Eval      UL82§FIL = 'FIDST00F'
045800070724     c                   Eval      UL82§WIN = 'S'
045900070724     c                   Eval      UL82§num = 2
046000070724     c                   Eval      UL82§att = 2
046100070724     c                   eval      ul82§mss = ErrMsg(1)
046200070724     c                   Eval      UL82§msw = 'La Distinta '
046300070724     c                             + %editc(or67nfvi:'Z') + ' è già utilizzata -
046400070724     c                             da un altro lavoro e non è manutenzionabile'
046500070724      * Effettuo la chiamata al *PGM d utilità
046600070724     c                   Call(e)   'TRUL82R'
046700070724     c                   Parm                    Trul82ds
046800070724
046900070724     C                   ENDSR
047000070724** ErrMsg
047100070724Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
