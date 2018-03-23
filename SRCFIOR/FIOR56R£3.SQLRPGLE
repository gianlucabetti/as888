000100071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000200080313     h dftactgrp(*no) actgrp(*caller)
000300061227
000400061227      *------------------------------------------------------------------------*
000500061227      *
000600061227      *          PREPARAZIONE DATI PER PDA AUT
000700061227      *
000800061227      *------------------------------------------------------------------------*
000900131107     ffnore01l  if   e           k disk
001000080313     ffnorg01l  uf   e           k disk    commit(or56comit) usropn
001100071205     ffnorm01l  if   e           k disk
001200071220     ffnblp01l  if   e           k disk
001300061229     ffiar401l  if   e           k disk
001400061229     ffiar404l  if   e           k disk    rename(fiar4000:fiar404)
001500061227     ffiar601l  if   e           k disk
001600070301     ffidst01l  if   e           k disk
001700130513     ffidst03l  uf   e           k disk    rename(FIDST000:FIDST03)
001800140527     f                                     prefix(w) usropn
001900160331     fFNORF03L  Uf   e           k disk
002000061228     ffipdo01t  uf a e           k disk
002100140122     fFNORT11L  if   e           k disk
002200061227
002300061227      *------------------------------------------------------------------------*
002400061227      *  RIEPILOGO INDICATORI
002500061227      *------------------------------------------------------------------------*
002600071205
002700061227      *------------------------------------------------------------------------*
002800061227
002900061227      *   V A R I A B I L I
003000071220     d parm01          s                   like(fior56ds)
003100080327     d oldorgndc       s                   like(orgndc) inz
003200071220     d $cdfisc         s               n
003300071220     d $piva           s               n
003400160405     d $UpdFNORF       s               n   inz
003500131107     d wPOS            s              2s 0
003600131108     d wNOTE           s             70
003700140120     d wNOpda          s               n   inz(*off)
003800140122     d wORTgst         s                   like(ORTgst)
003900140122     d xx              s              3s 0 inz
004000140123     d wPDOvar         s                   like(PDOvar)
004100140527     d cmd             s            512a   varying
004200161021     d wdpm            s                   like(ORMdar)
004300061227
004400061227      *   S C H I E R E
004500140122     d FLGvar          ds            10    inz
004600140122     d  wVAR                          1    inz  dim(10)
004700061227
004800061227      *   D S   I N T E R N E / E S T E R N E
004900080317     d ddstflr       e ds
005000080313     d dorg01        e ds
005100061227     d dorm01        e ds
005200061229     d dta4q         e ds
005300070115     d fior56ds      e ds
005400080327     d fior90ds      e ds
005500071205     d fnorgds       e ds                  extname(fnorg00f)
005600071205     d fnormds       e ds                  extname(fnorm00f)
005700061227     d kpjba         e ds
005800131107     d dOREorari     e ds
005900161116     d dOREdt        e ds
006000140130     D TIBS02DS      E DS
006100140319     d TRULORSds     e ds
006200140319     d TRULOR2ds     e ds
006300140318
006400140519      // - Driver per reperire mail/sms
006500140519     d FIOR900DS     e ds                  qualified inz
006600140130
006700140122       //?Tabella TCV - Tipologia variazione
006800140122     d dTCV          e ds                  inz
006900160331
007000160331       // -?Flag operativi di FNORF00F.ORFflo?
007100160331     d dORF01        e ds                  inz  qualified
007200140527
007300140527      //---------------------------------------------------------------
007400140527      //?Definizione costanti.
007500140527      //---------------------------------------------------------------
007600140527      // - Comando di override al file distinte
007700140527     d c_CmdOvrDbF     c                   const('OVRDBF +
007800140527     d                                           file(FIDST03L) +
007900140527     d                                           waitrcd(*immed)')
008000140130
008100140130      //---------------------------------------------------------------
008200140130      //?Definizione procedure utilizzate.
008300140130      //---------------------------------------------------------------
008400140130
008500140130       //?Recupera orari di servizio
008600140319     d trulorsr        pr                  extpgm('TRULORSR')
008700140130     d  kpjba                              likeds(KPJBA)
008800140319     d  trulorsds                          likeds(TRULORSDS)
008900140319     d  trulor2ds                          likeds(TRULOR2DS)
009000140130     d                                     options (*nopass)
009100140519
009200140519      // - Driver per reperire mail/sms
009300140519     d FIOR900R        pr                  extpgm('FIOR900R')
009400140519     d  fior900ds                          likeds(fior900ds)
009500140318
009600140130
009700140130      //---------------------------------------------------------------
009800140130      //?Definizione prototipi utilizzati.
009900140130      //---------------------------------------------------------------
010000140130
010100140130      /copy gaitrasrc/srcprotopr,tibs02r
010200140527
010300140527       // Esecuzione comando
010400140527      /copy gaitrasrc/srcprotopr,SYSTEM
010500140130
010600160331       //--------------------------------------------------------------
010700160331       //?Definizione key-list.                                        ?
010800160331       //--------------------------------------------------------------
010900160331
011000160331     c     keyFNORF03    klist
011100160331     c                   kfld                    ORFpoe
011200160331     c                   kfld                    ORFnsr
011300160331     c                   kfld                    ORFnor
011400160331     c                   kfld                    ORFnrv
011500160331     c                   kfld                    ORFfar
011600160331     c                   kfld                    ORFdae
011700160331     c                   kfld                    ORFore
011800061227
011900061227      *------------------------------------------------------------------------*
012000140120
012100140120
012200140120     c                   eval      wNOpda = *off
012300070115
012400080314      * se elaborazione non totale e non quadratura ORM forzata
012500080627      * e non è cancellazione
012600080314     c                   if        or56tla <> 'T' and or56tla <> 'Q'
012700080707     c                             and or56tla <> 'D'
012800160331
012900070115      * controllo se ok tutti i dati
013000070115     c                   if        or56poe = *zeros
013100070115     c                   eval      or56err = 'E'
013200071205     c                   eval      or56msg = 'Manca fil. emissione ORM'
013300070115     c                   goto      fine
013400070115     c                   endif
013500070115      * l'orm deve esistere
013600070115     c     kfnorm        chain     fnorm01l
013700070115     c                   if        not %found(fnorm01l)
013800070115     c                   eval      or56err = 'E'
013900070115     c                   eval      or56msg = 'ORM non trovato'
014000070115     c                   goto      fine
014100070115     c                   endif
014200080313     c     kfnorm        chain(n)  fnorg01l
014300071205     c                   if        not %found(fnorg01l)
014400071205     c                   eval      or56err = 'E'
014500071205     c                   eval      or56msg = 'ORM non trovato'
014600071205     c                   goto      fine
014700071205     c                   endif
014800070119      * deve essere assegnato se elaborazione per bolla RC
014900070122     c                   if        or56tla = 'R' and ormfao <> 400
015000070115     c                   eval      or56err = 'E'
015100070115     c                   eval      or56msg = 'ORM non assegnato'
015200070115     c                   goto      fine
015300070115     c                   endif
015400070301      * controllo se è una distinta da scaricare su PDA
015500070307     c                   eval      dstnpg = 4
015600071221     c                   eval      dstnfv = or56ndcd
015700071221     c                   eval      orgfgs = or56fgs
015800080317     c                   clear                   ddstflr
015900070301     c     kfidst        chain     fidst01l
016000140120      * se non trovo su fidst
016100140120     c                   if        not %found(fidst01l)
016200070301     c                   eval      or56err = 'E'
016300070301     c                   eval      or56msg = 'Distinta non per PDA'
016400070301     c                   goto      fine
016500070301     c                   endif
016600080317     c                   eval      ddstflr = dstflr
016700140120      * verifico se è previsto lo scarico dati a PDA
016800140120     c                   IF        dstpda <> 'E' and dstpda <> 'O'
016900140120     c                   eval      wNOpda = *on
017000140120     c                   ENDIF
017100140121      * controllo se esiste il rcd su FIPDO
017200140121      * se non è inserimento RC e se è previsto lo scarico a PDA
017300140121     c                   if        or56tla <> 'R' and not wNOpda
017400140121     c     kfipdo1       chain(n)  fipdo01t
017500140121      * se non esiste su FIPDO torno errore ai programmi chiamanti
017600140121     c                   if        not %found(fipdo01t)
017700140121     c                   eval      or56err = 'E'
017800140121     c                   eval      or56msg = 'ORM non scaricato sul PDA'
017900140121     c                   goto      fine
018000140121     c                   endif
018100140121     c                   endif
018200070115      * preparo i dati per PDA
018300071221      * ma prima imposto dei dati per la chain su FIPDO
018400071221     c                   eval      ormndc = or56ndcd
018500071221     c                   eval      orgfgs = or56fgs
018600070115     c                   exsr      sr_elabora
018700140527
018800140527      /free
018900140527       //?Se ho scritto disalloco la distinta
019000140527         unlock FIDST03L;
019100140527      /end-free
019200140527
019300070115     c                   goto      fine
019400160331
019500070115     c                   endif
019600160331
019700160331      *-----------------*
019800071205
019900071205     c/exec sql
020000071205     c+ set option dynusrprf = *owner, closqlcsr = *endmod
020100071205     c/end-exec
020200061227
020300080314      * elaborazione totale
020400080314     c                   if        or56tla = 'T'
020500160331
020600071205      * leggo ORM per elaborazione totale solo per data distinta
020700071205     c                   if        or56ndcd = *zeros
020800071205     C/EXEC SQL
020900071205     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
021000071205     C+ FROM fnorm01l join fnorg03l on
021100071205     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
021200071205     C+ ormnrv=orgnrv
021300071205     C+ WHERE orgpor=:or56fgs and orgddc=:or56ddc and
021400071205     c+ ormfao = 400
021500120703     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc
021600071205     C/END-EXEC
021700071205
021800071205      * leggo ORM per elaborazione totale con distinta richiesta
021900071205     c                   else
022000071205     C/EXEC SQL
022100071205     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
022200071205     C+ FROM fnorm01l join fnorg03l on
022300071205     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
022400071205     C+ ormnrv=orgnrv
022500071205     C+ WHERE orgpor=:or56fgs and orgddc=:or56ddc and
022600071205     c+ ormfao = 400  and
022700071205     C+ orgndc between :or56ndcd and :or56ndca
022800120703     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc
022900071205     C/END-EXEC
023000071205     c                   endif
023100071205
023200071205     c                   if        or56ndcd = *zeros
023300071205     C/EXEC SQL
023400071205     C+ OPEN A1
023500071205     C/END-EXEC
023600071205     c                   else
023700071205     C/EXEC SQL
023800071205     C+ OPEN B1
023900071205     C/END-EXEC
024000071205     c                   endif
024100071205
024200071205     c                   do        *hival
024300071205
024400071205     c                   if        or56ndcd = *zeros
024500071205     C/EXEC SQL
024600071205     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
024700071205     C/END-EXEC
024800071205     c                   else
024900071205     C/EXEC SQL
025000071205     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
025100071205     C/END-EXEC
025200071205     c                   endif
025300071205
025400071205     c                   if        sqlcod = 100 or sqlcod < 0
025500071205     c                   leave
025600071205     c                   endif
025700070301      * controllo se è una distinta da scaricare su PDA
025800070307     c                   eval      dstnpg = 4
025900071205     c                   eval      dstnfv = orgndc
026000080319     c                   clear                   ddstflr
026100070301     c     kfidst        chain     fidst01l
026200070301      * se non trovo su fidst o non è previsto lo scarico leggo ORM sucessivo
026300070301     c                   if        not %found(fidst01l) or
026400080207     c                             (dstpda <> 'E' and dstpda <> 'O')
026500070301     c                   iter
026600070301     c                   endif
026700080319     c                   eval      ddstflr = dstflr
026800080201      * se richiesto il tipo uscita controllo la congruenza
026900080201     c                   if        or56tusc <> *blanks
027000080201     c                             and dstfpp <> or56tusc
027100080201      * se non congruente leggo ORM sucessivo
027200080201     c                   iter
027300080201     c                   endif
027400120224      * se richiesto il tipo distinta PARCEL
027500120224     c                   if        or56tdis = 'P' and dsttpm <> *blanks
027600120224     c                   iter
027700120224     c                   endif
027800120224      * se richiesto il tipo distinta PARCEL
027900120224     c                   if        or56tdis = 'M'
028000080201     c                             and dsttpm <> or56tdis
028100080201      * se non congruente leggo ORM sucessivo
028200080201     c                   iter
028300080201     c                   endif
028400080724      * controllo che non ci sia la fase RCH
028500080724     c                   clear                   fior90ds
028600080724     c                   eval      or90tla = 'C'
028700080724     c                   eval      or90fas = 'RCH'
028800080724     c                   eval      or90fgs = orgfgs
028900080724     c                   eval      or90ndc = orgndc
029000080724     c                   call      'FIOR90R'
029100080724     c                   parm                    kpjba
029200080724     c                   parm                    fior90ds
029300080724      * fase RCH leggo rcd sucessivo
029400080724     c                   if        or90ndcok <> *blanks
029500080724     c                   iter
029600080724     c                   endif
029700061227      * preparo il file
029800061227     c                   exsr      sr_elabora
029900140527
030000140527      /free
030100140527       //?Se ho scritto disalloco la distinta
030200140527         unlock FIDST03L;
030300140527      /end-free
030400140527
030500061227     c                   enddo
030600071205
030700071205     c                   if        or56ndcd = *zeros
030800071205     C/EXEC SQL
030900071205     C+ CLOSE A1
031000071205     C/END-EXEC
031100071205     c                   else
031200071205     C/EXEC SQL
031300071205     C+ CLOSE B1
031400071205     C/END-EXEC
031500071205     c                   endif
031600160331
031700080314     c                   endif
031800160331
031900160331      *-----------------*
032000080314
032100080404      * elaborazione per quadratura forzata ORM
032200080314     c                   if        or56tla = 'Q'
032300080403     c                   exsr      sr_annulla
032400080314     c                   endif
032500080404
032600080404      * elaborazione per quadratura ORM in test
032700080404     c                   if        or56tla = 'D'
032800080404     c                   exsr      sr_cancella
032900080404     c                   endif
033000061227
033100070115     c     fine          tag
033200070301
033300140120       //?Richiamo sempre (se non ci sono stati errori)
033400140120       //?il pgm per la scrittura delle telefonate
033500140120       //?i controlli se scrivere o meno la telefonata viene fatta
033600140120       //?all'interno del FIOR54R
033700140120     c                   If        OR56err <> 'E'
033800131219     c                   call      'FIOR54R'
033900131219     c                   parm                    KPJBA
034000131219     c                   parm                    FIOR56ds
034100131219     c                   EndIf
034200131219
034300070115     c                   eval      kpjbu = fior56ds
034400061227     c                   eval      *inlr = *on
034500061227
034600061227      *------------------------------------------------------------------------*
034700061227      * ELABORA
034800061227      *------------------------------------------------------------------------*
034900061227     c     sr_elabora    begsr
035000130513
035100130513      /free
035200140120       //?Se distinta NO a pda vado via
035300140120         IF  wNOpda;
035400140120           leavesr;
035500140120         ENDIF;
035600140603
035700140603       //?Se richiesta elaborazione totale
035800140603         IF  or56tla = 'T';
035900130513       //?Come prima cosa alloco la distinta
036000130513       //?questo perchè è capitato che 2 utenti abbiamo lanciato la generazione
036100130513       //?dei dati a PDA in contemporanea e così sono stati creati rcd doppi su
036200130513       //?FIPDO con problemi poi nella SYNC dei PDA
036300130513         chain(e) (DSTfgs:DSTnpg:DSTnfv) FIDST03L;
036400130513         IF  %error;
036500130513           leavesr;
036600130513         ENDIF;
036700140528         //?Prima di procedere con l'aggiornamento o la scrittura di FIPDO
036800140528         //?verifico che la distinta di ORM/ORG sia ancora quella del file caricato con SQL
036900140528         //?se non lo è vado via
037000140528           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORM01L;
037100140528           IF  %found(FNORM01L) and ORMndc <> DSTnfv;
037200140528             leavesr;
037300140528           ENDIF;
037400140527         ENDIF;
037500140527
037600130513      /end-free
037700061227
037800061229      * flag operativi (ORMFLO)
037900061227     c                   eval      dorm01 = ormflo
038000061227
038100061227      * controllo se esiste già il rcd
038200071205      * se elaborazione totale non aggiorno il record
038300080415      * se ORM non annullato e se non è dello stesso AUT
038400080415     c     kfipdo        setll     fipdo01t
038500080415     c                   if        %equal(fipdo01t)
038600080415     c                   do        *hival
038700080415     c     kfipdo        reade     fipdo01t
038800080415     c                   if        %eof(fipdo01t)
038900080415     c                   leave
039000080415     c                   endif
039100140124     c                   eval      wPDOvar = PDOvar
039200080415     c                   if        or56tla <> 'T' and dstpdr = pdocodaut
039300061227     c                   exsr      sr_aggiorna
039400071205     c                   endif
039500080227      * se elaborazione totale e ORM annullato devo aggiornare il rcd togliendo
039600080415      * la 'A' di annullato se stesso AUT
039700080415     c                   if        or56tla = 'T' and dstpdr = pdocodaut and
039800080227     c                             pdoatb = 'A'
039900080227     c                   exsr      sr_aggiorna
040000080227     c                   endif
040100080415     c                   enddo
040200080415     c                   endif
040300080415
040400080415     c                   if        not %equal(fipdo01t)
040500061227     c                   exsr      sr_scrivi
040600061227     c                   endif
040700061227     c                   endsr
040800061227
040900061227      *------------------------------------------------------------------------*
041000061227      * AGGIORNA
041100061227      *------------------------------------------------------------------------*
041200061227     c     sr_aggiorna   begsr
041300061227
041400160405     c                   clear                   $UpdFNORF
041500071205      * se tipo lancio 'A' devo annullare l'ORM xchè l'ho tolto all'AUT
041600080206      * oppure l'ho dirottato (caso di ORM RC)
041700080403     c                   if        or56tla = 'A'
041800070123     c                   eval      pdoatb = 'A'
041900070427      * aggiorno anche la data così il PDA recupera la modifica
042000080311     c                   eval      pdodatora = %char(%timestamp:*iso0)
042100070123     c                   else
042200071205      * aggiorno i dati del PDA
042300160405     c                   eval      $UpdFNORF = (PDOatb = 'A')
042400071205     c                   exsr      sr_movdati
042500070123     c                   endif
042600080313      * incremento IDROW
042700080313     c                   add       1             pdoidrow
042800071205     c                   update    fipdo000
042900160405      *
043000160405      * -?Aggiornamento fase 400 nel file FNORF00F?
043100160405     c                   if        $UpdFNORF
043200160405     c                   exsr      sr_UpdFNORF
043300160405     c                   clear                   $UpdFNORF
043400160405     c                   endif
043500080313
043600080313      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
043700080317      * solo se la distinta non è in test
043800110523      * e se l'ORM non è RC
043900080317     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
044000110523     c                             and ormnsr = 0
044100080313     c     korm          chain     fnorg01l
044200080313     c                   if        %found(fnorg01l)
044300080313     c                   eval      dorg01 = orgflo
044400080403     c                   if        or56tla = 'A'
044500080313     c                   clear                   §orgpda
044600080313     c                   else
044700080313     c                   eval      §orgpda = 'S'
044800080313     c                   endif
044900080313     c                   eval      orgflo = dorg01
045000080313     c                   update    fnorg000
045100080313     c                   endif
045200080317     c                   endif
045300061227
045400061227     c                   endsr
045500061227
045600061227      *------------------------------------------------------------------------*
045700061227      * SCRIVI
045800061227      *------------------------------------------------------------------------*
045900061227     c     sr_scrivi     begsr
046000061227
046100061228     c                   clear                   fipdo000
046200071205     c                   eval      pdofgs = orgfgs
046300071205     c                   eval      pdondc = orgndc
046400071205     c                   eval      pdopoe = ormpoe
046500071205     c                   eval      pdonsr = ormnsr
046600071205     c                   eval      pdonor = ormnor
046700071205     c                   eval      pdonrv = ormnrv
046800061228     c                   exsr      sr_movdati
046900080310     c                   eval      pdoidrow = 1
047000061227     c                   write     fipdo000
047100160331      *
047200160331      * -?Aggiornamento fase 400 nel file FNORF00F?
047300160331     c                   exsr      sr_UpdFNORF
047400080313
047500080314      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
047600080317      * solo se la distinta non è in test
047700080317     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
047800080313     c     korm          chain     fnorg01l
047900080313     c                   if        %found(fnorg01l)
048000080313     c                   eval      dorg01 = orgflo
048100080313     c                   eval      §orgpda = 'S'
048200080313     c                   eval      orgflo = dorg01
048300080313     c                   update    fnorg000
048400080313     c                   endif
048500080317     c                   endif
048600080327
048700080327      * per ogni distinta devo scrivere la relativa fase 'PDR'
048800080327     c                   if        orgndc <> oldorgndc
048900080327     c                   exsr      sr_fase
049000080327     c                   eval      oldorgndc = orgndc
049100080327     c                   endif
049200061227
049300061227     c                   endsr
049400160331
049500160331      *------------------------------------------------------------------------*
049600160331      *?Aggiornamento fase 400 nel file FNORF00F (Storico Fasi O.R.M.)        ?*
049700160331      *------------------------------------------------------------------------*
049800160331     c     sr_UpdFNORF   BEGSR
049900160331      *
050000160331      * -?Solo SE O.R.M. in fase "400"?
050100160331      *  ?(controllo già eseguito in fase di lettura del file FNORM)?
050200160331     c*//                if        ORMfao <> 400
050300160331     c*//                leavesr
050400160331     c*//                endif
050500160331      *
050600160331      * -?Aggiornamento dati della fase "400" nello storico fasi?
050700160331     c                   eval      ORFpoe = ORMpoe
050800160331     c                   eval      ORFnsr = ORMnsr
050900160331     c                   eval      ORFnor = ORMnor
051000160331     c                   eval      ORFnrv = ORMnrv
051100160331     c                   eval      ORFfar = ORMfao
051200160331     c                   eval      ORFdae = ORMdfo
051300160331     c                   eval      ORFore = ORMofo
051400160331      *
051500160331     c     keyFNORF03    chain     FNORF000
051600160331     c                   if        %found(FNORF03L)
051700160331     c                   eval      dORF01   = ORFflo
051800160331     c                   eval      dORF01.§ORFesito = %subst( %char( %dec(
051900160331     c                                        %timestamp() ) ) : 1 : 14 )
052000160331     c                   eval      ORFflo   = dORF01
052100160331      *                  __________________
052200160331     c                   update    FNORF000
052300160331      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052400160331     c                   endif
052500160331      *
052600160331     c                   ENDSR
052700061228
052800061228      *------------------------------------------------------------------------*
052900061228      * IMPOSTA I DATI NEL FILE
053000061228      *------------------------------------------------------------------------*
053100061228     c     sr_movdati    begsr
053200061229
053300061229      * pulisco campi che possono variare di volta in volta e che ricalcolo
053400061229     c                   clear                   pdocommis
053500061229     c                   clear                   pdocodfisc
053600061229     c                   clear                   pdoimpprep
053700061229     c                   clear                   pdodivprep
053800061229     c                   clear                   pdobollarc
053900080215     c                   clear                   pdoiconasx
054000090211     c                   clear                   pdonote
054100140130     c                   clear                   PDOritenta
054200140130     c                   clear                   PDOnotaut
054300140130     c                   clear                   PDOsepara
054400140130     c                   clear                   PDOvar
054500140130     c                   clear                   PDOalert
054600140130     c                   clear                   PDOorastd
054700140130     c                   clear                   PDOorariap
054800140130     c                   clear                   PDOnote1
054900071220
055000071220     c                   eval      $cdfisc = *off
055100071220     c                   eval      $piva = *off
055200061228
055300061229      * dati da ORM
055400061228     c                   eval      pdoatb = *blanks
055500061228     c                   eval      pdorso = ormrso
055600061228     c                   eval      pdorsr = ormrsr
055700061228     c                   eval      pdoinr = orminr
055800061228     c                   eval      pdolor = ormlor
055900061228     c                   eval      pdoprr = ormprr
056000061228     c                   eval      pdonar = ormnar
056100061228     c                   eval      pdorer = ormrer
056200061228     c                   eval      pdoter = ormter
056300061228     c                   eval      pdoorr = ormorr
056400061228     c                   eval      pdonam = ormnam
056500061228     c                   eval      pdoncl = ormncl
056600061228     c                   eval      pdopkg = ormpkg
056700061228     c                   eval      pdovlm = ormvlm
056800061228     c                   eval      pdobnc = ormbnc
056900061228     c                   eval      pdorfa = ormrfa
057000061228     c                   eval      pdosto = ormsto
057100061228     c                   eval      pdoricrit = §orsrm
057200080215      * se ricevuta ritiro imposto l'icona a sinistra
057300080215     c                   if        pdoricrit = 'S'
057400080215     c                   eval      pdoiconasx = '05'
057500080215     c                   endif
057600061229      * commissionato
057700061229     c                   if        §orcom = 'S'
057800061229     c                   eval      pdocommis = §orcom
057900061229     c                   endif
058000131107      /free
058100131107       //?Imposto orari apertura + note ORM
058200131107        OREtrc = 'O';
058300131108        chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
058400131107        IF  %found(FNORE01L);
058500131107          dOREorari = OREdati;
058600131107          SELECT;
058700131107            WHEN  §OREoramda <> 0 and §OREorapda <> 0;
058800140519              PDOorariap = 'AP ' +
058900140122                       %editc(§OREoramda:'X') + ' ' +
059000140122                       %editc(§OREorama:'X') + ' ' +
059100140122                       %editc(§OREorapda:'X') + ' ' +
059200140122                       %editc(§OREorapa:'X');
059300131107            WHEN  §OREoramda <> 0 and §OREorapda = 0;
059400140519              PDOorariap = 'AP ' +
059500140122                       %editc(§OREoramda:'X') + ' ' +
059600140122                       %editc(§OREorama:'X');
059700131107            WHEN  §OREorama <> 0 and §OREorapa = 0;
059800140519              PDOorariap = 'ENTRO LE ' +
059900140122                       %editc(§OREorama:'X');
060000131107            WHEN  §OREorama = 0 and §OREorapa <> 0;
060100140519              PDOorariap = 'ENTRO LE ' +
060200140122                       %editc(§OREorapa:'X');
060300131107          ENDSL;
060400140519        ENDIF;
060500140519
060600140519       //?Carico le note
060700140519        PDOno1 = ORMno1;
060800140519        PDOno2 = ORMno2;
060900140130
061000140130       //?Imposto orari servizio se filiale gestione attiva
061100140319           clear trulorsds;
061200140319           clear trulor2ds;
061300140130           IOREfil = OR56fgs;
061400140130           IOREcap = ORMcar;
061500140130           IOREloc = ORMlor;
061600140130           IOREnar = ORMnar;
061700140130           IOREdta = ORMdar;
061800140130           IOREtser = 'R';
061900140319           trulorsr (kpjba:trulorsds:trulor2ds);
062000140319           IF  OOREerr = *blanks and OOR2err = *blanks;
062100161115         //?Se non ho visualizzato orari estesi prendo orari servizio standard
062200161115             IF  ORMrmp = *blanks;
062300140130             PDOorastd = 'Servizio RITIRI di norma dalle ' +
062400140319             %subst(%editc(OOR2stis:'X'):1:2) + ':' +
062500140319             %subst(%editc(OOR2stis:'X'):3:2) + ' alle ' +
062600140319             %subst(%editc(OOR2stfs:'X'):1:2) + ':' +
062700140319             %subst(%editc(OOR2stfs:'X'):3:2);
062800161115             ELSE;
062900161115         //?Altimenti prendo minimo e massimo
063000161115               PDOorastd = 'Servizio RITIRI di norma dalle ' +
063100161115               %subst(%editc(OOR2miis:'X'):1:2) + ':' +
063200161115               %subst(%editc(OOR2miis:'X'):3:2) + ' alle ' +
063300161115               %subst(%editc(OOR2mxfs:'X'):1:2) + ':' +
063400161115               %subst(%editc(OOR2mxfs:'X'):3:2);
063500161115             ENDIF;
063600140130           ENDIF;
063700140519
063800140519       //?Imposto il flag per icona 'bustina' in caso di Alert al mittente
063900140519         fior900ds.IOR900trc = 'A';
064000140519         fior900ds.IOR900rcdm = 'MA';
064100140519         fior900ds.IOR900rcds = 'S ';
064200140519         fior900ds.IOR900poe = ORMpoe;
064300140519         fior900ds.IOR900nsr = ORMnsr;
064400140519         fior900ds.IOR900nor = ORMnor;
064500140519         fior900ds.IOR900nrv = ORMnrv;
064600140519         fior900r (fior900ds);
064700140519       //?Se torna che OK dati imposto il flag
064800140519         IF  fior900ds.OOR900err = *blanks;
064900140519           pdoalert = '01';
065000140519         ENDIF;
065100161021
065200161021       //?Cerco la data pronta merce
065300161116       //?se data ritiro > data pronta merce non passo l'ora pronta merce al PDA
065400161116        clear dOREdt;
065500161021        OREtrc = 'DT';
065600161021        chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
065700161021        IF  %found(FNORE01L) and %subst(OREdati:1:8) > *zeros;
065800161116          dOREdt = OREdati;
065900161021        ENDIF;
066000161116        IF  ORMdar > §OREdpm and §OREdpm > *zeros;
066100161021          clear PDOorr;
066200161021        ENDIF;
066300140519
066400131107      /end-free
066500131107
066600061229      * tipo distinta mattina/pomeriggio
066700070301     c                   eval      pdotdc = dstfpp
066800061228
066900061229      * codice autotrasportatore
067000061228     c                   eval      pdocodaut = dstpdr
067100061228
067200061229      * ORM prepagato
067300061228     c                   if        ormtor = 'P'
067400061229      * recupero la bolla
067500061228     c                   eval      ar4trc = 'M'
067600061228     c                   eval      ar4n14 = %editc(ormpoe:'X') +
067700061228     c                                      %editc(ormnsr:'X') +
067800061228     c                                      %editc(ormnor:'X') +
067900061228     c                                      %editc(ormnrv:'X')
068000061228     c     kfiar4        chain     fiar404l
068100061228     c                   if        %found(fiar404l)
068200120504
068300120504       //?Recupero la P.Iva e/o il codice fiscale
068400120504      * controllo se ha la partita IVA  (non accetto $$)
068500071220     c     kfnblp        chain     fnblp01l
068600120504     c                   if        %found(fnblp01l) and
068700120504     c                             BLPcpi <> *blanks and
068800120504     c                             %subst(BLPcpi:1:2) <> '$$'
068900120504     c                   eval      $piva = *on
069000120509       //?se ho la partita IVA faccio finta di avere anche il codice fiscale
069100120509     c                   eval      $cdfisc = *on
069200071220     c                   endif
069300071220      * controllo se ha il codice fiscale
069400120504      * solo se non ho già la partita IVA
069500120504     c                   IF        not $piva
069600061229     c                   clear                   dta4q
069700061229     c                   eval      ar4trc = 'Q'
069800061229     c     kfiar41       chain     fiar401l
069900061229     c                   if        %found(fiar401l)
070000061229      * uso la ds di tita4 xchè su fiar4 non è stata fatta ...ma tanto sono uguali
070100120504      * (non accetto PRIVATO e Cod.Fiscale lungo 11)
070200061229     c                   eval      dta4q = ar4not
070300061229     c                   endif
070400120504     c                   if        §ta4qcfm <> *blanks and
070500120504     c                             %scan('PRIVATO':§ta4qcfm) = 0
070600071220     c                   eval      $cdfisc = *on
070700061229     c                   endif
070800120504     c                   ENDIF
070900071220      * richiedo quello che manca
071000071220     c                   select
071100071220      * non ci sono tutti e due richiedo tutti e due
071200120504     c                   when      not $cdfisc and not $piva
071300120504     c                   eval      pdocodfisc = '3'
071400120504      * se non c'è la partita iva la richiedo
071500120504      * anche se ho codice fiscale lungo 11
071600120504     c                   when      not $piva and
071700120504     c                             %subst(§TA4qcfm:12:5) = *blanks
071800120504     c                   eval      pdocodfisc = '2'
071900071220      * se non c'è il codice fiscale lo richiedo
072000120509     c                   when      not $cdfisc
072100120504     c                   eval      pdocodfisc = '1'
072200071220     c                   endsl
072300120504
072400061229      * cerco l'importo del prepagato
072500061228     c     kfiar6        chain     fiar601l
072600061228     c                   if        %found(fiar601l)
072700061228     c                   eval      pdoimpprep = ar6ift
072800061228     c                   eval      pdodivprep = ar6div
072900061228     c                   endif
073000080215      * imposto l'icona a sinistra
073100080215     c                   eval      pdoiconasx = '05'
073200061228     c                   endif
073300061228     c                   endif
073400061228
073500061228      * bolla RC
073600061229      * (per ora va bene il controllo solo con la serie ORM, in quanto ora è utilizzata
073700061229      *  solo per gli ORM RC)
073800061228     c                   if        ormnsr <> *zeros
073900061228     c                   eval      pdobollarc = 'S'
074000061228     c                   endif
074100070312
074200070312      * imposto flag icona info
074300070312      * per ora imposto solo '00' per avvisare di presenza note
074400070312     c                   if        ormno1 <> *blanks or ormno2 <> *blanks
074500070312     c                   eval      pdoflginfo = '00'
074600070312     c                   endif
074700090211
074800090211      * se orm con destinatario inserito lo riporto sul campo pdonote
074900090211     c                   if        ormrsc <> *blanks
075000140319     c                   eval      PDOnote =  %trim(ORMrsc) + ' - ' +
075100140319     c                             %trim(ormloc)
075200090211     c                   if        ormnac <> *blanks
075300131203     c                   eval      pdonote = %trim(pdonote) + ' - ' +
075400131203     c                             %trim(ormnac)
075500090211     c                   else
075600131203     c                   eval      pdonote = %trim(pdonote) + ' (' +
075700131203     c                             %trim(ormprc) + ')'
075800090211     c                   endif
075900090211     c                   endif
076000140122
076100140122      /free
076200140122       //?Controllo se ci sono Note AUT
076300140122         wORTgst = 'S';
076400140122         setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
076500140122         reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
076600140122         DOW  not %eof(FNORT11L);
076700140122           PDOnotaut = %trim(PDOnotaut) + ORTnob;
076800140122           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
076900140122         ENDDO;
077000140122
077100140122       //?Imposto anche le variazioni
077200140122         IF  OR56var <> *blanks;
077300140122           FLGvar = OR56var;
077400140122           xx = 1;
077500140122           FOR xx by 1 to %elem(wVar);
077600140122             IF  wVar(xx) <> *blanks;
077700140122               exsr tab_TCV;
077800140123               IF  wPDOvar = *blanks;
077900140203                 wPDOvar = 'Variato: ' + §TCVdesb;
078000140122               ELSE;
078100140124                 IF  %scan(%trim(§TCVdesb):wPDOvar) = 0;
078200140124                   wPDOvar = %trim(wPDOvar) + '+' + §TCVdesb;
078300140124                 ENDIF;
078400140122               ENDIF;
078500140122             ENDIF;
078600140122           ENDFOR;
078700140123           PDOvar = wPDOvar;
078800140122         ENDIF;
078900140122      /end-free
079000061228
079100061228      * imposto la data e ora del lavoro
079200080311     c                   eval      pdodatora = %char(%timestamp:*iso0)
079300061228
079400061228     c                   endsr
079500080403
079600080403      *------------------------------------------------------------------------*
079700080403      * ANNULLA FIPDO00T quando quadrata la distinta
079800080403      *------------------------------------------------------------------------*
079900080403     c     sr_annulla    begsr
080000080403
080100080403      * aggancio la distinta
080200080403     c                   eval      dstnpg = 4
080300080403     c                   clear                   ddstflr
080400080403     c     kfidst1       chain     fidst01l
080500080403     c                   if        %found(fidst01l)
080600080403     c                   eval      ddstflr = dstflr
080700080403     c                   endif
080800080403      * leggo FIPDO00T
080900080403     c     kfipdo2       setll     fipdo01t
081000080403     c                   do        *hival
081100080403     c     kfipdo2       reade     fipdo01t
081200080403      * fine file esco
081300080403     c                   if        %eof(fipdo01t)
081400080403     c                   leave
081500080403     c                   endif
081600080403      * escludo gli annullati
081700080403     c                   if        pdoatb = 'A'
081800080403     c                   iter
081900080403     c                   endif
082000080403      * annullo
082100080403     c                   eval      pdoatb = 'A'
082200080403      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
082300080403      * solo se la distinta non è in test
082400080403     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
082500080403     c     korg          chain     fnorg01l
082600080403     c                   if        %found(fnorg01l)
082700080403     c                   eval      dorg01 = orgflo
082800080403     c                   clear                   §orgpda
082900080403     c                   eval      orgflo = dorg01
083000080403     c                   update    fnorg000
083100080403     c                   endif
083200080403     c                   endif
083300080403      * aggiorno fipdo
083400080403     c                   update    fipdo000
083500080403     c                   enddo
083600080403
083700140211      * cancello anche i record presente su FIPRO10F
083800080403     C/EXEC SQL
083900140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
084000080403     C+ and prondc = :or56ndcd
084100080403     C/END-EXEC
084200080403
084300080403     c                   endsr
084400080327
084500080404      *------------------------------------------------------------------------*
084600080404      * CNACELLA FIPDO00T quando quadrata la distinta
084700080404      *------------------------------------------------------------------------*
084800080404     c     sr_cancella   begsr
084900080404
085000080404      * aggancio la distinta
085100080404     c                   eval      dstnpg = 4
085200080404     c                   clear                   ddstflr
085300080404     c     kfidst1       chain     fidst01l
085400080404     c                   if        %found(fidst01l)
085500080404     c                   eval      ddstflr = dstflr
085600080404     c                   endif
085700080404      * leggo FIPDO00T
085800080404     c     kfipdo2       setll     fipdo01t
085900080404     c                   do        *hival
086000080404     c     kfipdo2       reade     fipdo01t
086100080404      * fine file esco
086200080404     c                   if        %eof(fipdo01t)
086300080404     c                   leave
086400080404     c                   endif
086500080404      * escludo gli annullati
086600080404     c                   if        pdoatb = 'A'
086700080404     c                   iter
086800080404     c                   endif
086900080404      * annullo
087000080404     c                   delete    fipdo000
087100080404     c                   enddo
087200080404
087300140211      * cancello anche i record presente su FIPRO10F
087400080404     C/EXEC SQL
087500140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
087600080404     C+ and prondc = :or56ndcd
087700080404     C/END-EXEC
087800080404
087900080404     c                   endsr
088000080404
088100080327      *------------------------------------------------------------------------*
088200080327      * Scrivo fase PDR sulla distinta
088300080327      *------------------------------------------------------------------------*
088400080327     c     sr_fase       begsr
088500080327
088600080327     c                   clear                   fior90ds
088700080327     c                   eval      or90tla = 'W'
088800080327     c                   eval      or90fas = 'PDR'
088900080327     c                   eval      or90fgs = orgfgs
089000080327     c                   eval      or90ndc = orgndc
089100080327     c                   eval      or90flgins = 'A'
089200080327     c                   eval      or90comit = or56comit
089300080327     c                   call      'FIOR90R'
089400080327     c                   parm                    kpjba
089500080327     c                   parm                    fior90ds
089600080327      * non controllo l'errore di ritorno tanto sono alla fine del PGM
089700080327      * al massimo non ho scritto la fase
089800080327
089900080327     c                   endsr
090000140122
090100140122       //--------------------------------------------------------------
090200140122       //?Aggancio tabella TCV.                                   ?
090300140122       //--------------------------------------------------------------
090400140122     c     tab_TCV       begsr
090500140122
090600140122     c                   clear                   dTCV
090700140122     c                   clear                   TIBS02DS
090800140122     c                   eval      T02mod = 'C'
090900140122     c                   eval      T02sif = knsif
091000140122     c                   eval      T02cod = 'TCV'
091100140122     c                   eval      T02ke1 = wVar(xx)
091200140122     c                   call      'TIBS02R'
091300140122     c                   parm                    kpjba
091400140122     c                   parm                    tibs02ds
091500140122     c                   IF        T02err = *blanks
091600140122     c                   eval      dTCV = T02uni
091700140122     c                   ENDIF
091800140122
091900140122     c                   ENDSR
092000061227
092100061227      *------------------------------------------------------------------------*
092200061227      * ROUTINE INIZIALE
092300061227      *------------------------------------------------------------------------*
092400061227     c     *inzsr        begsr
092500061227
092600061227     c     *entry        plist
092700061227     c                   parm                    kpjba
092800070124     c                   parm                    parm01
092900061227
093000070124     c                   clear                   fior56ds
093100070124
093200070122      * Richiamato da preparazione dati di massa
093300070124     c                   if        %parms = 1
093400070124     c                   eval      fior56ds = kpjbu
093500070124     c                   else
093600070124     c                   eval      fior56ds = parm01
093700070124     c                   endif
093800070115
093900070115     c                   clear                   or56err
094000070115     c                   clear                   or56msg
094100061227
094200080313      * disabilito comit su FNORG e FIPDO a meno che non sia espressamente
094300080313      * richiesto ('1')
094400080313     c                   if        or56comit <> '1'
094500080313     c                   eval      or56comit = '0'
094600080313     c                   end
094700080313
094800080313     c                   open      fnorg01L
094900140527
095000140527      /free
095100140527       // Eseguo OVRDBF sul file distinte per NON aspettare se RCD è allocato
095200140527         cmd = c_CmdOvrDbF;
095300140527         ExecuteCommand (cmd);
095400140527         Open FIDST03L;
095500140527      /end-free
095600080313
095700061227      * definizione chiavi
095800070115     c     kfnorm        klist
095900070115     c                   kfld                    or56poe
096000070115     c                   kfld                    or56nsr
096100070115     c                   kfld                    or56nor
096200070115     c                   kfld                    or56nrv
096300080403
096400080403     c     kfipdo2       klist
096500080403     c                   kfld                    or56fgs
096600080403     c                   kfld                    or56ndcd
096700080221
096800080221     c     kfipdo1       klist
096900080221     c                   kfld                    or56fgs
097000080221     c                   kfld                    or56ndcd
097100080221     c                   kfld                    or56poe
097200080221     c                   kfld                    or56nsr
097300080221     c                   kfld                    or56nor
097400080221     c                   kfld                    or56nrv
097500061227
097600061227     c     kfipdo        klist
097700071205     c                   kfld                    orgfgs
097800071205     c                   kfld                    ormndc
097900071205     c                   kfld                    ormpoe
098000071205     c                   kfld                    ormnsr
098100071205     c                   kfld                    ormnor
098200071205     c                   kfld                    ormnrv
098300061227
098400070301     c     kfidst        klist
098500070307     c                   kfld                    dstnpg
098600070301     c                   kfld                    dstnfv
098700071205     c                   kfld                    orgfgs
098800080403
098900080403     c     kfidst1       klist
099000080403     c                   kfld                    dstnpg
099100080403     c                   kfld                    or56ndcd
099200080403     c                   kfld                    or56fgs
099300061227
099400061227     c     kfiar4        klist
099500061227     c                   kfld                    ar4trc
099600061227     c                   kfld                    ar4n14
099700061229
099800061229     c     kfiar41       klist
099900061229     c                   kfld                    ar4aas
100000061229     c                   kfld                    ar4lnp
100100061229     c                   kfld                    ar4nrs
100200061229     c                   kfld                    ar4nsp
100300061229     c                   kfld                    ar4trc
100400061227
100500061227     c     kfiar6        klist
100600061227     c                   kfld                    ar4aas
100700061227     c                   kfld                    ar4lnp
100800061227     c                   kfld                    ar4nrs
100900061227     c                   kfld                    ar4nsp
101000071220
101100071220     c     kfnblp        klist
101200071220     c                   kfld                    ar4aas
101300071220     c                   kfld                    ar4lnp
101400071220     c                   kfld                    ar4nrs
101500071220     c                   kfld                    ar4nsp
101600061227
101700080313     c     korm          klist
101800080313     c                   kfld                    ormpoe
101900080313     c                   kfld                    ormnsr
102000080313     c                   kfld                    ormnor
102100080313     c                   kfld                    ormnrv
102200080403
102300080403     c     korg          klist
102400080403     c                   kfld                    pdopoe
102500080403     c                   kfld                    pdonsr
102600080403     c                   kfld                    pdonor
102700080403     c                   kfld                    pdonrv
102800080313
102900061227     c                   endsr
