000100160331     /*END
000200160331
000300071205     h Decedit('0,') Datedit(*ymd/) Option(*nodebugio)
000400080313     h dftactgrp(*no) actgrp(*caller)
000500061227
000600061227      *------------------------------------------------------------------------*
000700061227      *
000800061227      *          PREPARAZIONE DATI PER PDA AUT
000900061227      *
001000061227      *------------------------------------------------------------------------*
001100131107     ffnore01l  if   e           k disk
001200080313     ffnorg01l  uf   e           k disk    commit(or56comit) usropn
001300071205     ffnorm01l  if   e           k disk
001400071220     ffnblp01l  if   e           k disk
001500061229     ffiar401l  if   e           k disk
001600061229     ffiar404l  if   e           k disk    rename(fiar4000:fiar404)
001700061227     ffiar601l  if   e           k disk
001800070301     ffidst01l  if   e           k disk
001900130513     ffidst03l  uf   e           k disk    rename(FIDST000:FIDST03)
002000140527     f                                     prefix(w) usropn
002100160331     fFNORF03L  Uf   e           k disk
002200061228     ffipdo01t  uf a e           k disk
002300140122     fFNORT11L  if   e           k disk
002400061227
002500061227      *------------------------------------------------------------------------*
002600061227      *  RIEPILOGO INDICATORI
002700061227      *------------------------------------------------------------------------*
002800071205
002900061227      *------------------------------------------------------------------------*
003000061227
003100061227      *   V A R I A B I L I
003200071220     d parm01          s                   like(fior56ds)
003300080327     d oldorgndc       s                   like(orgndc) inz
003400071220     d $cdfisc         s               n
003500071220     d $piva           s               n
003600160405     d $UpdFNORF       s               n   inz
003700131107     d wPOS            s              2s 0
003800131108     d wNOTE           s             70
003900140120     d wNOpda          s               n   inz(*off)
004000140122     d wORTgst         s                   like(ORTgst)
004100140122     d xx              s              3s 0 inz
004200140123     d wPDOvar         s                   like(PDOvar)
004300140527     d cmd             s            512a   varying
004400061227
004500061227      *   S C H I E R E
004600140122     d FLGvar          ds            10    inz
004700140122     d  wVAR                          1    inz  dim(10)
004800061227
004900061227      *   D S   I N T E R N E / E S T E R N E
005000080317     d ddstflr       e ds
005100080313     d dorg01        e ds
005200061227     d dorm01        e ds
005300061229     d dta4q         e ds
005400070115     d fior56ds      e ds
005500080327     d fior90ds      e ds
005600071205     d fnorgds       e ds                  extname(fnorg00f)
005700071205     d fnormds       e ds                  extname(fnorm00f)
005800061227     d kpjba         e ds
005900131107     d dOREorari     e ds
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
062100140130             PDOorastd = 'Servizio RITIRI di norma dalle ' +
062200140319             %subst(%editc(OOR2stis:'X'):1:2) + ':' +
062300140319             %subst(%editc(OOR2stis:'X'):3:2) + ' alle ' +
062400140319             %subst(%editc(OOR2stfs:'X'):1:2) + ':' +
062500140319             %subst(%editc(OOR2stfs:'X'):3:2);
062600140130           ENDIF;
062700140519
062800140519       //?Imposto il flag per icona 'bustina' in caso di Alert al mittente
062900140519         fior900ds.IOR900trc = 'A';
063000140519         fior900ds.IOR900rcdm = 'MA';
063100140519         fior900ds.IOR900rcds = 'S ';
063200140519         fior900ds.IOR900poe = ORMpoe;
063300140519         fior900ds.IOR900nsr = ORMnsr;
063400140519         fior900ds.IOR900nor = ORMnor;
063500140519         fior900ds.IOR900nrv = ORMnrv;
063600140519         fior900r (fior900ds);
063700140519       //?Se torna che OK dati imposto il flag
063800140519         IF  fior900ds.OOR900err = *blanks;
063900140519           pdoalert = '01';
064000140519         ENDIF;
064100140519
064200131107      /end-free
064300131107
064400061229      * tipo distinta mattina/pomeriggio
064500070301     c                   eval      pdotdc = dstfpp
064600061228
064700061229      * codice autotrasportatore
064800061228     c                   eval      pdocodaut = dstpdr
064900061228
065000061229      * ORM prepagato
065100061228     c                   if        ormtor = 'P'
065200061229      * recupero la bolla
065300061228     c                   eval      ar4trc = 'M'
065400061228     c                   eval      ar4n14 = %editc(ormpoe:'X') +
065500061228     c                                      %editc(ormnsr:'X') +
065600061228     c                                      %editc(ormnor:'X') +
065700061228     c                                      %editc(ormnrv:'X')
065800061228     c     kfiar4        chain     fiar404l
065900061228     c                   if        %found(fiar404l)
066000120504
066100120504       //?Recupero la P.Iva e/o il codice fiscale
066200120504      * controllo se ha la partita IVA  (non accetto $$)
066300071220     c     kfnblp        chain     fnblp01l
066400120504     c                   if        %found(fnblp01l) and
066500120504     c                             BLPcpi <> *blanks and
066600120504     c                             %subst(BLPcpi:1:2) <> '$$'
066700120504     c                   eval      $piva = *on
066800120509       //?se ho la partita IVA faccio finta di avere anche il codice fiscale
066900120509     c                   eval      $cdfisc = *on
067000071220     c                   endif
067100071220      * controllo se ha il codice fiscale
067200120504      * solo se non ho già la partita IVA
067300120504     c                   IF        not $piva
067400061229     c                   clear                   dta4q
067500061229     c                   eval      ar4trc = 'Q'
067600061229     c     kfiar41       chain     fiar401l
067700061229     c                   if        %found(fiar401l)
067800061229      * uso la ds di tita4 xchè su fiar4 non è stata fatta ...ma tanto sono uguali
067900120504      * (non accetto PRIVATO e Cod.Fiscale lungo 11)
068000061229     c                   eval      dta4q = ar4not
068100061229     c                   endif
068200120504     c                   if        §ta4qcfm <> *blanks and
068300120504     c                             %scan('PRIVATO':§ta4qcfm) = 0
068400071220     c                   eval      $cdfisc = *on
068500061229     c                   endif
068600120504     c                   ENDIF
068700071220      * richiedo quello che manca
068800071220     c                   select
068900071220      * non ci sono tutti e due richiedo tutti e due
069000120504     c                   when      not $cdfisc and not $piva
069100120504     c                   eval      pdocodfisc = '3'
069200120504      * se non c'è la partita iva la richiedo
069300120504      * anche se ho codice fiscale lungo 11
069400120504     c                   when      not $piva and
069500120504     c                             %subst(§TA4qcfm:12:5) = *blanks
069600120504     c                   eval      pdocodfisc = '2'
069700071220      * se non c'è il codice fiscale lo richiedo
069800120509     c                   when      not $cdfisc
069900120504     c                   eval      pdocodfisc = '1'
070000071220     c                   endsl
070100120504
070200061229      * cerco l'importo del prepagato
070300061228     c     kfiar6        chain     fiar601l
070400061228     c                   if        %found(fiar601l)
070500061228     c                   eval      pdoimpprep = ar6ift
070600061228     c                   eval      pdodivprep = ar6div
070700061228     c                   endif
070800080215      * imposto l'icona a sinistra
070900080215     c                   eval      pdoiconasx = '05'
071000061228     c                   endif
071100061228     c                   endif
071200061228
071300061228      * bolla RC
071400061229      * (per ora va bene il controllo solo con la serie ORM, in quanto ora è utilizzata
071500061229      *  solo per gli ORM RC)
071600061228     c                   if        ormnsr <> *zeros
071700061228     c                   eval      pdobollarc = 'S'
071800061228     c                   endif
071900070312
072000070312      * imposto flag icona info
072100070312      * per ora imposto solo '00' per avvisare di presenza note
072200070312     c                   if        ormno1 <> *blanks or ormno2 <> *blanks
072300070312     c                   eval      pdoflginfo = '00'
072400070312     c                   endif
072500090211
072600090211      * se orm con destinatario inserito lo riporto sul campo pdonote
072700090211     c                   if        ormrsc <> *blanks
072800140319     c                   eval      PDOnote =  %trim(ORMrsc) + ' - ' +
072900140319     c                             %trim(ormloc)
073000090211     c                   if        ormnac <> *blanks
073100131203     c                   eval      pdonote = %trim(pdonote) + ' - ' +
073200131203     c                             %trim(ormnac)
073300090211     c                   else
073400131203     c                   eval      pdonote = %trim(pdonote) + ' (' +
073500131203     c                             %trim(ormprc) + ')'
073600090211     c                   endif
073700090211     c                   endif
073800140122
073900140122      /free
074000140122       //?Controllo se ci sono Note AUT
074100140122         wORTgst = 'S';
074200140122         setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
074300140122         reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
074400140122         DOW  not %eof(FNORT11L);
074500140122           PDOnotaut = %trim(PDOnotaut) + ORTnob;
074600140122           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
074700140122         ENDDO;
074800140122
074900140122       //?Imposto anche le variazioni
075000140122         IF  OR56var <> *blanks;
075100140122           FLGvar = OR56var;
075200140122           xx = 1;
075300140122           FOR xx by 1 to %elem(wVar);
075400140122             IF  wVar(xx) <> *blanks;
075500140122               exsr tab_TCV;
075600140123               IF  wPDOvar = *blanks;
075700140203                 wPDOvar = 'Variato: ' + §TCVdesb;
075800140122               ELSE;
075900140124                 IF  %scan(%trim(§TCVdesb):wPDOvar) = 0;
076000140124                   wPDOvar = %trim(wPDOvar) + '+' + §TCVdesb;
076100140124                 ENDIF;
076200140122               ENDIF;
076300140122             ENDIF;
076400140122           ENDFOR;
076500140123           PDOvar = wPDOvar;
076600140122         ENDIF;
076700140122      /end-free
076800140122
076900140122      * imposto la data e ora del lavoro
077000061228
077100061228      * imposto la data e ora del lavoro
077200080311     c                   eval      pdodatora = %char(%timestamp:*iso0)
077300061228
077400061228     c                   endsr
077500080403
077600080403      *------------------------------------------------------------------------*
077700080403      * ANNULLA FIPDO00T quando quadrata la distinta
077800080403      *------------------------------------------------------------------------*
077900080403     c     sr_annulla    begsr
078000080403
078100080403      * aggancio la distinta
078200080403     c                   eval      dstnpg = 4
078300080403     c                   clear                   ddstflr
078400080403     c     kfidst1       chain     fidst01l
078500080403     c                   if        %found(fidst01l)
078600080403     c                   eval      ddstflr = dstflr
078700080403     c                   endif
078800080403      * leggo FIPDO00T
078900080403     c     kfipdo2       setll     fipdo01t
079000080403     c                   do        *hival
079100080403     c     kfipdo2       reade     fipdo01t
079200080403      * fine file esco
079300080403     c                   if        %eof(fipdo01t)
079400080403     c                   leave
079500080403     c                   endif
079600080403      * escludo gli annullati
079700080403     c                   if        pdoatb = 'A'
079800080403     c                   iter
079900080403     c                   endif
080000080403      * annullo
080100080403     c                   eval      pdoatb = 'A'
080200080403      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
080300080403      * solo se la distinta non è in test
080400080403     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
080500080403     c     korg          chain     fnorg01l
080600080403     c                   if        %found(fnorg01l)
080700080403     c                   eval      dorg01 = orgflo
080800080403     c                   clear                   §orgpda
080900080403     c                   eval      orgflo = dorg01
081000080403     c                   update    fnorg000
081100080403     c                   endif
081200080403     c                   endif
081300080403      * aggiorno fipdo
081400080403     c                   update    fipdo000
081500080403     c                   enddo
081600080403
081700140211      * cancello anche i record presente su FIPRO10F
081800080403     C/EXEC SQL
081900140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
082000080403     C+ and prondc = :or56ndcd
082100080403     C/END-EXEC
082200080403
082300080403     c                   endsr
082400080327
082500080404      *------------------------------------------------------------------------*
082600080404      * CNACELLA FIPDO00T quando quadrata la distinta
082700080404      *------------------------------------------------------------------------*
082800080404     c     sr_cancella   begsr
082900080404
083000080404      * aggancio la distinta
083100080404     c                   eval      dstnpg = 4
083200080404     c                   clear                   ddstflr
083300080404     c     kfidst1       chain     fidst01l
083400080404     c                   if        %found(fidst01l)
083500080404     c                   eval      ddstflr = dstflr
083600080404     c                   endif
083700080404      * leggo FIPDO00T
083800080404     c     kfipdo2       setll     fipdo01t
083900080404     c                   do        *hival
084000080404     c     kfipdo2       reade     fipdo01t
084100080404      * fine file esco
084200080404     c                   if        %eof(fipdo01t)
084300080404     c                   leave
084400080404     c                   endif
084500080404      * escludo gli annullati
084600080404     c                   if        pdoatb = 'A'
084700080404     c                   iter
084800080404     c                   endif
084900080404      * annullo
085000080404     c                   delete    fipdo000
085100080404     c                   enddo
085200080404
085300140211      * cancello anche i record presente su FIPRO10F
085400080404     C/EXEC SQL
085500140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
085600080404     C+ and prondc = :or56ndcd
085700080404     C/END-EXEC
085800080404
085900080404     c                   endsr
086000080404
086100080327      *------------------------------------------------------------------------*
086200080327      * Scrivo fase PDR sulla distinta
086300080327      *------------------------------------------------------------------------*
086400080327     c     sr_fase       begsr
086500080327
086600080327     c                   clear                   fior90ds
086700080327     c                   eval      or90tla = 'W'
086800080327     c                   eval      or90fas = 'PDR'
086900080327     c                   eval      or90fgs = orgfgs
087000080327     c                   eval      or90ndc = orgndc
087100080327     c                   eval      or90flgins = 'A'
087200080327     c                   eval      or90comit = or56comit
087300080327     c                   call      'FIOR90R'
087400080327     c                   parm                    kpjba
087500080327     c                   parm                    fior90ds
087600080327      * non controllo l'errore di ritorno tanto sono alla fine del PGM
087700080327      * al massimo non ho scritto la fase
087800080327
087900080327     c                   endsr
088000140122
088100140122       //--------------------------------------------------------------
088200140122       //?Aggancio tabella TCV.                                   ?
088300140122       //--------------------------------------------------------------
088400140122     c     tab_TCV       begsr
088500140122
088600140122     c                   clear                   dTCV
088700140122     c                   clear                   TIBS02DS
088800140122     c                   eval      T02mod = 'C'
088900140122     c                   eval      T02sif = knsif
089000140122     c                   eval      T02cod = 'TCV'
089100140122     c                   eval      T02ke1 = wVar(xx)
089200140122     c                   call      'TIBS02R'
089300140122     c                   parm                    kpjba
089400140122     c                   parm                    tibs02ds
089500140122     c                   IF        T02err = *blanks
089600140122     c                   eval      dTCV = T02uni
089700140122     c                   ENDIF
089800140122
089900140122     c                   ENDSR
090000061227
090100061227      *------------------------------------------------------------------------*
090200061227      * ROUTINE INIZIALE
090300061227      *------------------------------------------------------------------------*
090400061227     c     *inzsr        begsr
090500061227
090600061227     c     *entry        plist
090700061227     c                   parm                    kpjba
090800070124     c                   parm                    parm01
090900061227
091000070124     c                   clear                   fior56ds
091100070124
091200070122      * Richiamato da preparazione dati di massa
091300070124     c                   if        %parms = 1
091400070124     c                   eval      fior56ds = kpjbu
091500070124     c                   else
091600070124     c                   eval      fior56ds = parm01
091700070124     c                   endif
091800070115
091900070115     c                   clear                   or56err
092000070115     c                   clear                   or56msg
092100061227
092200080313      * disabilito comit su FNORG e FIPDO a meno che non sia espressamente
092300080313      * richiesto ('1')
092400080313     c                   if        or56comit <> '1'
092500080313     c                   eval      or56comit = '0'
092600080313     c                   end
092700080313
092800080313     c                   open      fnorg01L
092900140527
093000140527      /free
093100140527       // Eseguo OVRDBF sul file distinte per NON aspettare se RCD è allocato
093200140527         cmd = c_CmdOvrDbF;
093300140527         ExecuteCommand (cmd);
093400140527         Open FIDST03L;
093500140527      /end-free
093600080313
093700061227      * definizione chiavi
093800070115     c     kfnorm        klist
093900070115     c                   kfld                    or56poe
094000070115     c                   kfld                    or56nsr
094100070115     c                   kfld                    or56nor
094200070115     c                   kfld                    or56nrv
094300080403
094400080403     c     kfipdo2       klist
094500080403     c                   kfld                    or56fgs
094600080403     c                   kfld                    or56ndcd
094700080221
094800080221     c     kfipdo1       klist
094900080221     c                   kfld                    or56fgs
095000080221     c                   kfld                    or56ndcd
095100080221     c                   kfld                    or56poe
095200080221     c                   kfld                    or56nsr
095300080221     c                   kfld                    or56nor
095400080221     c                   kfld                    or56nrv
095500061227
095600061227     c     kfipdo        klist
095700071205     c                   kfld                    orgfgs
095800071205     c                   kfld                    ormndc
095900071205     c                   kfld                    ormpoe
096000071205     c                   kfld                    ormnsr
096100071205     c                   kfld                    ormnor
096200071205     c                   kfld                    ormnrv
096300061227
096400070301     c     kfidst        klist
096500070307     c                   kfld                    dstnpg
096600070301     c                   kfld                    dstnfv
096700071205     c                   kfld                    orgfgs
096800080403
096900080403     c     kfidst1       klist
097000080403     c                   kfld                    dstnpg
097100080403     c                   kfld                    or56ndcd
097200080403     c                   kfld                    or56fgs
097300061227
097400061227     c     kfiar4        klist
097500061227     c                   kfld                    ar4trc
097600061227     c                   kfld                    ar4n14
097700061229
097800061229     c     kfiar41       klist
097900061229     c                   kfld                    ar4aas
098000061229     c                   kfld                    ar4lnp
098100061229     c                   kfld                    ar4nrs
098200061229     c                   kfld                    ar4nsp
098300061229     c                   kfld                    ar4trc
098400061227
098500061227     c     kfiar6        klist
098600061227     c                   kfld                    ar4aas
098700061227     c                   kfld                    ar4lnp
098800061227     c                   kfld                    ar4nrs
098900061227     c                   kfld                    ar4nsp
099000071220
099100071220     c     kfnblp        klist
099200071220     c                   kfld                    ar4aas
099300071220     c                   kfld                    ar4lnp
099400071220     c                   kfld                    ar4nrs
099500071220     c                   kfld                    ar4nsp
099600061227
099700080313     c     korm          klist
099800080313     c                   kfld                    ormpoe
099900080313     c                   kfld                    ormnsr
100000080313     c                   kfld                    ormnor
100100080313     c                   kfld                    ormnrv
100200080403
100300080403     c     korg          klist
100400080403     c                   kfld                    pdopoe
100500080403     c                   kfld                    pdonsr
100600080403     c                   kfld                    pdonor
100700080403     c                   kfld                    pdonrv
100800080313
100900061227     c                   endsr
101000061227
