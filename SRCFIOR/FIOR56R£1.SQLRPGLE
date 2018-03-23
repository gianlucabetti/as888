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
001900061228     ffipdo01t  uf a e           k disk
002000140122     fFNORT11L  if   e           k disk
002100061227
002200061227      *------------------------------------------------------------------------*
002300061227      *  RIEPILOGO INDICATORI
002400061227      *------------------------------------------------------------------------*
002500071205
002600061227      *------------------------------------------------------------------------*
002700061227
002800061227      *   V A R I A B I L I
002900071220     d parm01          s                   like(fior56ds)
003000080327     d oldorgndc       s                   like(orgndc) inz
003100071220     d $cdfisc         s               n
003200071220     d $piva           s               n
003300131107     d wPOS            s              2s 0
003400131108     d wNOTE           s             70
003500140120     d wNOpda          s               n   inz(*off)
003600140122     d wORTgst         s                   like(ORTgst)
003700140122     d xx              s              3s 0 inz
003800140123     d wPDOvar         s                   like(PDOvar)
003900140527     d cmd             s            512a   varying
004000061227
004100061227      *   S C H I E R E
004200140122     d FLGvar          ds            10    inz
004300140122     d  wVAR                          1    inz  dim(10)
004400061227
004500061227      *   D S   I N T E R N E / E S T E R N E
004600080317     d ddstflr       e ds
004700080313     d dorg01        e ds
004800061227     d dorm01        e ds
004900061229     d dta4q         e ds
005000070115     d fior56ds      e ds
005100080327     d fior90ds      e ds
005200071205     d fnorgds       e ds                  extname(fnorg00f)
005300071205     d fnormds       e ds                  extname(fnorm00f)
005400061227     d kpjba         e ds
005500131107     d dOREorari     e ds
005600140130     D TIBS02DS      E DS
005700140319     d TRULORSds     e ds
005800140319     d TRULOR2ds     e ds
005900140318
006000140519      // - Driver per reperire mail/sms
006100140519     d FIOR900DS     e ds                  qualified inz
006200140130
006300140122       //?Tabella TCV - Tipologia variazione
006400140122     d dTCV          e ds                  inz
006500140527
006600140527      //---------------------------------------------------------------
006700140527      //?Definizione costanti.
006800140527      //---------------------------------------------------------------
006900140527      // - Comando di override al file distinte
007000140527     d c_CmdOvrDbF     c                   const('OVRDBF +
007100140527     d                                           file(FIDST03L) +
007200140527     d                                           waitrcd(*immed)')
007300140130
007400140130      //---------------------------------------------------------------
007500140130      //?Definizione procedure utilizzate.
007600140130      //---------------------------------------------------------------
007700140130
007800140130       //?Recupera orari di servizio
007900140319     d trulorsr        pr                  extpgm('TRULORSR')
008000140130     d  kpjba                              likeds(KPJBA)
008100140319     d  trulorsds                          likeds(TRULORSDS)
008200140319     d  trulor2ds                          likeds(TRULOR2DS)
008300140130     d                                     options (*nopass)
008400140519
008500140519      // - Driver per reperire mail/sms
008600140519     d FIOR900R        pr                  extpgm('FIOR900R')
008700140519     d  fior900ds                          likeds(fior900ds)
008800140318
008900140130
009000140130      //---------------------------------------------------------------
009100140130      //?Definizione prototipi utilizzati.
009200140130      //---------------------------------------------------------------
009300140130
009400140130      /copy gaitrasrc/srcprotopr,tibs02r
009500140527
009600140527       // Esecuzione comando
009700140527      /copy gaitrasrc/srcprotopr,SYSTEM
009800140130
009900140130      //---------------------------------------------------------------
010000061227
010100061227      *------------------------------------------------------------------------*
010200140120
010300140120
010400140120     c                   eval      wNOpda = *off
010500070115
010600080314      * se elaborazione non totale e non quadratura ORM forzata
010700080627      * e non è cancellazione
010800080314     c                   if        or56tla <> 'T' and or56tla <> 'Q'
010900080707     c                             and or56tla <> 'D'
011000070115      * controllo se ok tutti i dati
011100070115     c                   if        or56poe = *zeros
011200070115     c                   eval      or56err = 'E'
011300071205     c                   eval      or56msg = 'Manca fil. emissione ORM'
011400070115     c                   goto      fine
011500070115     c                   endif
011600070115      * l'orm deve esistere
011700070115     c     kfnorm        chain     fnorm01l
011800070115     c                   if        not %found(fnorm01l)
011900070115     c                   eval      or56err = 'E'
012000070115     c                   eval      or56msg = 'ORM non trovato'
012100070115     c                   goto      fine
012200070115     c                   endif
012300080313     c     kfnorm        chain(n)  fnorg01l
012400071205     c                   if        not %found(fnorg01l)
012500071205     c                   eval      or56err = 'E'
012600071205     c                   eval      or56msg = 'ORM non trovato'
012700071205     c                   goto      fine
012800071205     c                   endif
012900070119      * deve essere assegnato se elaborazione per bolla RC
013000070122     c                   if        or56tla = 'R' and ormfao <> 400
013100070115     c                   eval      or56err = 'E'
013200070115     c                   eval      or56msg = 'ORM non assegnato'
013300070115     c                   goto      fine
013400070115     c                   endif
013500070301      * controllo se è una distinta da scaricare su PDA
013600070307     c                   eval      dstnpg = 4
013700071221     c                   eval      dstnfv = or56ndcd
013800071221     c                   eval      orgfgs = or56fgs
013900080317     c                   clear                   ddstflr
014000070301     c     kfidst        chain     fidst01l
014100140120      * se non trovo su fidst
014200140120     c                   if        not %found(fidst01l)
014300070301     c                   eval      or56err = 'E'
014400070301     c                   eval      or56msg = 'Distinta non per PDA'
014500070301     c                   goto      fine
014600070301     c                   endif
014700080317     c                   eval      ddstflr = dstflr
014800140120      * verifico se è previsto lo scarico dati a PDA
014900140120     c                   IF        dstpda <> 'E' and dstpda <> 'O'
015000140120     c                   eval      wNOpda = *on
015100140120     c                   ENDIF
015200140121      * controllo se esiste il rcd su FIPDO
015300140121      * se non è inserimento RC e se è previsto lo scarico a PDA
015400140121     c                   if        or56tla <> 'R' and not wNOpda
015500140121     c     kfipdo1       chain(n)  fipdo01t
015600140121      * se non esiste su FIPDO torno errore ai programmi chiamanti
015700140121     c                   if        not %found(fipdo01t)
015800140121     c                   eval      or56err = 'E'
015900140121     c                   eval      or56msg = 'ORM non scaricato sul PDA'
016000140121     c                   goto      fine
016100140121     c                   endif
016200140121     c                   endif
016300070115      * preparo i dati per PDA
016400071221      * ma prima imposto dei dati per la chain su FIPDO
016500071221     c                   eval      ormndc = or56ndcd
016600071221     c                   eval      orgfgs = or56fgs
016700070115     c                   exsr      sr_elabora
016800140527
016900140527      /free
017000140527       //?Se ho scritto disalloco la distinta
017100140527         unlock FIDST03L;
017200140527      /end-free
017300140527
017400070115     c                   goto      fine
017500070115     c                   endif
017600071205
017700071205     c/exec sql
017800071205     c+ set option dynusrprf = *owner, closqlcsr = *endmod
017900071205     c/end-exec
018000061227
018100080314      * elaborazione totale
018200080314     c                   if        or56tla = 'T'
018300071205      * leggo ORM per elaborazione totale solo per data distinta
018400071205     c                   if        or56ndcd = *zeros
018500071205     C/EXEC SQL
018600071205     C+ DECLARE A1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
018700071205     C+ FROM fnorm01l join fnorg03l on
018800071205     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
018900071205     C+ ormnrv=orgnrv
019000071205     C+ WHERE orgpor=:or56fgs and orgddc=:or56ddc and
019100071205     c+ ormfao = 400
019200120703     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc
019300071205     C/END-EXEC
019400071205
019500071205      * leggo ORM per elaborazione totale con distinta richiesta
019600071205     c                   else
019700071205     C/EXEC SQL
019800071205     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg03l.*
019900071205     C+ FROM fnorm01l join fnorg03l on
020000071205     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
020100071205     C+ ormnrv=orgnrv
020200071205     C+ WHERE orgpor=:or56fgs and orgddc=:or56ddc and
020300071205     c+ ormfao = 400  and
020400071205     C+ orgndc between :or56ndcd and :or56ndca
020500120703     C+ ORDER BY fnorg03l.orgfgs, fnorg03l.orgndc
020600071205     C/END-EXEC
020700071205     c                   endif
020800071205
020900071205     c                   if        or56ndcd = *zeros
021000071205     C/EXEC SQL
021100071205     C+ OPEN A1
021200071205     C/END-EXEC
021300071205     c                   else
021400071205     C/EXEC SQL
021500071205     C+ OPEN B1
021600071205     C/END-EXEC
021700071205     c                   endif
021800071205
021900071205     c                   do        *hival
022000071205
022100071205     c                   if        or56ndcd = *zeros
022200071205     C/EXEC SQL
022300071205     C+ FETCH NEXT FROM A1 INTO :fnormds, :fnorgds
022400071205     C/END-EXEC
022500071205     c                   else
022600071205     C/EXEC SQL
022700071205     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
022800071205     C/END-EXEC
022900071205     c                   endif
023000071205
023100071205     c                   if        sqlcod = 100 or sqlcod < 0
023200071205     c                   leave
023300071205     c                   endif
023400070301      * controllo se è una distinta da scaricare su PDA
023500070307     c                   eval      dstnpg = 4
023600071205     c                   eval      dstnfv = orgndc
023700080319     c                   clear                   ddstflr
023800070301     c     kfidst        chain     fidst01l
023900070301      * se non trovo su fidst o non è previsto lo scarico leggo ORM sucessivo
024000070301     c                   if        not %found(fidst01l) or
024100080207     c                             (dstpda <> 'E' and dstpda <> 'O')
024200070301     c                   iter
024300070301     c                   endif
024400080319     c                   eval      ddstflr = dstflr
024500080201      * se richiesto il tipo uscita controllo la congruenza
024600080201     c                   if        or56tusc <> *blanks
024700080201     c                             and dstfpp <> or56tusc
024800080201      * se non congruente leggo ORM sucessivo
024900080201     c                   iter
025000080201     c                   endif
025100120224      * se richiesto il tipo distinta PARCEL
025200120224     c                   if        or56tdis = 'P' and dsttpm <> *blanks
025300120224     c                   iter
025400120224     c                   endif
025500120224      * se richiesto il tipo distinta PARCEL
025600120224     c                   if        or56tdis = 'M'
025700080201     c                             and dsttpm <> or56tdis
025800080201      * se non congruente leggo ORM sucessivo
025900080201     c                   iter
026000080201     c                   endif
026100080724      * controllo che non ci sia la fase RCH
026200080724     c                   clear                   fior90ds
026300080724     c                   eval      or90tla = 'C'
026400080724     c                   eval      or90fas = 'RCH'
026500080724     c                   eval      or90fgs = orgfgs
026600080724     c                   eval      or90ndc = orgndc
026700080724     c                   call      'FIOR90R'
026800080724     c                   parm                    kpjba
026900080724     c                   parm                    fior90ds
027000080724      * fase RCH leggo rcd sucessivo
027100080724     c                   if        or90ndcok <> *blanks
027200080724     c                   iter
027300080724     c                   endif
027400061227      * preparo il file
027500061227     c                   exsr      sr_elabora
027600140527
027700140527      /free
027800140527       //?Se ho scritto disalloco la distinta
027900140527         unlock FIDST03L;
028000140527      /end-free
028100140527
028200061227     c                   enddo
028300071205
028400071205     c                   if        or56ndcd = *zeros
028500071205     C/EXEC SQL
028600071205     C+ CLOSE A1
028700071205     C/END-EXEC
028800071205     c                   else
028900071205     C/EXEC SQL
029000071205     C+ CLOSE B1
029100071205     C/END-EXEC
029200071205     c                   endif
029300080314     c                   endif
029400080314
029500080404      * elaborazione per quadratura forzata ORM
029600080314     c                   if        or56tla = 'Q'
029700080403     c                   exsr      sr_annulla
029800080314     c                   endif
029900080404
030000080404      * elaborazione per quadratura ORM in test
030100080404     c                   if        or56tla = 'D'
030200080404     c                   exsr      sr_cancella
030300080404     c                   endif
030400061227
030500070115     c     fine          tag
030600070301
030700140120       //?Richiamo sempre (se non ci sono stati errori)
030800140120       //?il pgm per la scrittura delle telefonate
030900140120       //?i controlli se scrivere o meno la telefonata viene fatta
031000140120       //?all'interno del FIOR54R
031100140120     c                   If        OR56err <> 'E'
031200131219     c                   call      'FIOR54R'
031300131219     c                   parm                    KPJBA
031400131219     c                   parm                    FIOR56ds
031500131219     c                   EndIf
031600131219
031700070115     c                   eval      kpjbu = fior56ds
031800061227     c                   eval      *inlr = *on
031900061227
032000061227      *------------------------------------------------------------------------*
032100061227      * ELABORA
032200061227      *------------------------------------------------------------------------*
032300061227     c     sr_elabora    begsr
032400130513
032500130513      /free
032600140120       //?Se distinta NO a pda vado via
032700140120         IF  wNOpda;
032800140120           leavesr;
032900140120         ENDIF;
033000140603
033100140603       //?Se richiesta elaborazione totale
033200140603         IF  or56tla = 'T';
033300130513       //?Come prima cosa alloco la distinta
033400130513       //?questo perchè è capitato che 2 utenti abbiamo lanciato la generazione
033500130513       //?dei dati a PDA in contemporanea e così sono stati creati rcd doppi su
033600130513       //?FIPDO con problemi poi nella SYNC dei PDA
033700130513         chain(e) (DSTfgs:DSTnpg:DSTnfv) FIDST03L;
033800130513         IF  %error;
033900130513           leavesr;
034000130513         ENDIF;
034100140528         //?Prima di procedere con l'aggiornamento o la scrittura di FIPDO
034200140528         //?verifico che la distinta di ORM/ORG sia ancora quella del file caricato con SQL
034300140528         //?se non lo è vado via
034400140528           chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORM01L;
034500140528           IF  %found(FNORM01L) and ORMndc <> DSTnfv;
034600140528             leavesr;
034700140528           ENDIF;
034800140527         ENDIF;
034900140527
035000130513      /end-free
035100061227
035200061229      * flag operativi (ORMFLO)
035300061227     c                   eval      dorm01 = ormflo
035400061227
035500061227      * controllo se esiste già il rcd
035600071205      * se elaborazione totale non aggiorno il record
035700080415      * se ORM non annullato e se non è dello stesso AUT
035800080415     c     kfipdo        setll     fipdo01t
035900080415     c                   if        %equal(fipdo01t)
036000080415     c                   do        *hival
036100080415     c     kfipdo        reade     fipdo01t
036200080415     c                   if        %eof(fipdo01t)
036300080415     c                   leave
036400080415     c                   endif
036500140124     c                   eval      wPDOvar = PDOvar
036600080415     c                   if        or56tla <> 'T' and dstpdr = pdocodaut
036700061227     c                   exsr      sr_aggiorna
036800071205     c                   endif
036900080227      * se elaborazione totale e ORM annullato devo aggiornare il rcd togliendo
037000080415      * la 'A' di annullato se stesso AUT
037100080415     c                   if        or56tla = 'T' and dstpdr = pdocodaut and
037200080227     c                             pdoatb = 'A'
037300080227     c                   exsr      sr_aggiorna
037400080227     c                   endif
037500080415     c                   enddo
037600080415     c                   endif
037700080415
037800080415     c                   if        not %equal(fipdo01t)
037900061227     c                   exsr      sr_scrivi
038000061227     c                   endif
038100061227     c                   endsr
038200061227
038300061227      *------------------------------------------------------------------------*
038400061227      * AGGIORNA
038500061227      *------------------------------------------------------------------------*
038600061227     c     sr_aggiorna   begsr
038700061227
038800071205      * se tipo lancio 'A' devo annullare l'ORM xchè l'ho tolto all'AUT
038900080206      * oppure l'ho dirottato (caso di ORM RC)
039000080403     c                   if        or56tla = 'A'
039100070123     c                   eval      pdoatb = 'A'
039200070427      * aggiorno anche la data così il PDA recupera la modifica
039300080311     c                   eval      pdodatora = %char(%timestamp:*iso0)
039400070123     c                   else
039500071205      * aggiorno i dati del PDA
039600071205     c                   exsr      sr_movdati
039700070123     c                   endif
039800080313      * incremento IDROW
039900080313     c                   add       1             pdoidrow
040000071205     c                   update    fipdo000
040100080313
040200080313      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
040300080317      * solo se la distinta non è in test
040400110523      * e se l'ORM non è RC
040500080317     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
040600110523     c                             and ormnsr = 0
040700080313     c     korm          chain     fnorg01l
040800080313     c                   if        %found(fnorg01l)
040900080313     c                   eval      dorg01 = orgflo
041000080403     c                   if        or56tla = 'A'
041100080313     c                   clear                   §orgpda
041200080313     c                   else
041300080313     c                   eval      §orgpda = 'S'
041400080313     c                   endif
041500080313     c                   eval      orgflo = dorg01
041600080313     c                   update    fnorg000
041700080313     c                   endif
041800080317     c                   endif
041900061227
042000061227     c                   endsr
042100061227
042200061227      *------------------------------------------------------------------------*
042300061227      * SCRIVI
042400061227      *------------------------------------------------------------------------*
042500061227     c     sr_scrivi     begsr
042600061227
042700061228     c                   clear                   fipdo000
042800071205     c                   eval      pdofgs = orgfgs
042900071205     c                   eval      pdondc = orgndc
043000071205     c                   eval      pdopoe = ormpoe
043100071205     c                   eval      pdonsr = ormnsr
043200071205     c                   eval      pdonor = ormnor
043300071205     c                   eval      pdonrv = ormnrv
043400061228     c                   exsr      sr_movdati
043500080310     c                   eval      pdoidrow = 1
043600061227     c                   write     fipdo000
043700080313
043800080314      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
043900080317      * solo se la distinta non è in test
044000080317     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
044100080313     c     korm          chain     fnorg01l
044200080313     c                   if        %found(fnorg01l)
044300080313     c                   eval      dorg01 = orgflo
044400080313     c                   eval      §orgpda = 'S'
044500080313     c                   eval      orgflo = dorg01
044600080313     c                   update    fnorg000
044700080313     c                   endif
044800080317     c                   endif
044900080327
045000080327      * per ogni distinta devo scrivere la relativa fase 'PDR'
045100080327     c                   if        orgndc <> oldorgndc
045200080327     c                   exsr      sr_fase
045300080327     c                   eval      oldorgndc = orgndc
045400080327     c                   endif
045500061227
045600061227     c                   endsr
045700061228
045800061228      *------------------------------------------------------------------------*
045900061228      * IMPOSTA I DATI NEL FILE
046000061228      *------------------------------------------------------------------------*
046100061228     c     sr_movdati    begsr
046200061229
046300061229      * pulisco campi che possono variare di volta in volta e che ricalcolo
046400061229     c                   clear                   pdocommis
046500061229     c                   clear                   pdocodfisc
046600061229     c                   clear                   pdoimpprep
046700061229     c                   clear                   pdodivprep
046800061229     c                   clear                   pdobollarc
046900080215     c                   clear                   pdoiconasx
047000090211     c                   clear                   pdonote
047100140130     c                   clear                   PDOritenta
047200140130     c                   clear                   PDOnotaut
047300140130     c                   clear                   PDOsepara
047400140130     c                   clear                   PDOvar
047500140130     c                   clear                   PDOalert
047600140130     c                   clear                   PDOorastd
047700140130     c                   clear                   PDOorariap
047800140130     c                   clear                   PDOnote1
047900071220
048000071220     c                   eval      $cdfisc = *off
048100071220     c                   eval      $piva = *off
048200061228
048300061229      * dati da ORM
048400061228     c                   eval      pdoatb = *blanks
048500061228     c                   eval      pdorso = ormrso
048600061228     c                   eval      pdorsr = ormrsr
048700061228     c                   eval      pdoinr = orminr
048800061228     c                   eval      pdolor = ormlor
048900061228     c                   eval      pdoprr = ormprr
049000061228     c                   eval      pdonar = ormnar
049100061228     c                   eval      pdorer = ormrer
049200061228     c                   eval      pdoter = ormter
049300061228     c                   eval      pdoorr = ormorr
049400061228     c                   eval      pdonam = ormnam
049500061228     c                   eval      pdoncl = ormncl
049600061228     c                   eval      pdopkg = ormpkg
049700061228     c                   eval      pdovlm = ormvlm
049800061228     c                   eval      pdobnc = ormbnc
049900061228     c                   eval      pdorfa = ormrfa
050000061228     c                   eval      pdosto = ormsto
050100061228     c                   eval      pdoricrit = §orsrm
050200080215      * se ricevuta ritiro imposto l'icona a sinistra
050300080215     c                   if        pdoricrit = 'S'
050400080215     c                   eval      pdoiconasx = '05'
050500080215     c                   endif
050600061229      * commissionato
050700061229     c                   if        §orcom = 'S'
050800061229     c                   eval      pdocommis = §orcom
050900061229     c                   endif
051000131107      /free
051100131107       //?Imposto orari apertura + note ORM
051200131107        OREtrc = 'O';
051300131108        chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
051400131107        IF  %found(FNORE01L);
051500131107          dOREorari = OREdati;
051600131107          SELECT;
051700131107            WHEN  §OREoramda <> 0 and §OREorapda <> 0;
051800140519              PDOorariap = 'AP ' +
051900140122                       %editc(§OREoramda:'X') + ' ' +
052000140122                       %editc(§OREorama:'X') + ' ' +
052100140122                       %editc(§OREorapda:'X') + ' ' +
052200140122                       %editc(§OREorapa:'X');
052300131107            WHEN  §OREoramda <> 0 and §OREorapda = 0;
052400140519              PDOorariap = 'AP ' +
052500140122                       %editc(§OREoramda:'X') + ' ' +
052600140122                       %editc(§OREorama:'X');
052700131107            WHEN  §OREorama <> 0 and §OREorapa = 0;
052800140519              PDOorariap = 'ENTRO LE ' +
052900140122                       %editc(§OREorama:'X');
053000131107            WHEN  §OREorama = 0 and §OREorapa <> 0;
053100140519              PDOorariap = 'ENTRO LE ' +
053200140122                       %editc(§OREorapa:'X');
053300131107          ENDSL;
053400140519        ENDIF;
053500140519
053600140519       //?Carico le note
053700140519        PDOno1 = ORMno1;
053800140519        PDOno2 = ORMno2;
053900140130
054000140130       //?Imposto orari servizio se filiale gestione attiva
054100140319           clear trulorsds;
054200140319           clear trulor2ds;
054300140130           IOREfil = OR56fgs;
054400140130           IOREcap = ORMcar;
054500140130           IOREloc = ORMlor;
054600140130           IOREnar = ORMnar;
054700140130           IOREdta = ORMdar;
054800140130           IOREtser = 'R';
054900140319           trulorsr (kpjba:trulorsds:trulor2ds);
055000140319           IF  OOREerr = *blanks and OOR2err = *blanks;
055100140130             PDOorastd = 'Servizio RITIRI di norma dalle ' +
055200140319             %subst(%editc(OOR2stis:'X'):1:2) + ':' +
055300140319             %subst(%editc(OOR2stis:'X'):3:2) + ' alle ' +
055400140319             %subst(%editc(OOR2stfs:'X'):1:2) + ':' +
055500140319             %subst(%editc(OOR2stfs:'X'):3:2);
055600140130           ENDIF;
055700140519
055800140519       //?Imposto il flag per icona 'bustina' in caso di Alert al mittente
055900140519         fior900ds.IOR900trc = 'A';
056000140519         fior900ds.IOR900rcdm = 'MA';
056100140519         fior900ds.IOR900rcds = 'S ';
056200140519         fior900ds.IOR900poe = ORMpoe;
056300140519         fior900ds.IOR900nsr = ORMnsr;
056400140519         fior900ds.IOR900nor = ORMnor;
056500140519         fior900ds.IOR900nrv = ORMnrv;
056600140519         fior900r (fior900ds);
056700140519       //?Se torna che OK dati imposto il flag
056800140519         IF  fior900ds.OOR900err = *blanks;
056900140519           pdoalert = '01';
057000140519         ENDIF;
057100140519
057200131107      /end-free
057300131107
057400061229      * tipo distinta mattina/pomeriggio
057500070301     c                   eval      pdotdc = dstfpp
057600061228
057700061229      * codice autotrasportatore
057800061228     c                   eval      pdocodaut = dstpdr
057900061228
058000061229      * ORM prepagato
058100061228     c                   if        ormtor = 'P'
058200061229      * recupero la bolla
058300061228     c                   eval      ar4trc = 'M'
058400061228     c                   eval      ar4n14 = %editc(ormpoe:'X') +
058500061228     c                                      %editc(ormnsr:'X') +
058600061228     c                                      %editc(ormnor:'X') +
058700061228     c                                      %editc(ormnrv:'X')
058800061228     c     kfiar4        chain     fiar404l
058900061228     c                   if        %found(fiar404l)
059000120504
059100120504       //?Recupero la P.Iva e/o il codice fiscale
059200120504      * controllo se ha la partita IVA  (non accetto $$)
059300071220     c     kfnblp        chain     fnblp01l
059400120504     c                   if        %found(fnblp01l) and
059500120504     c                             BLPcpi <> *blanks and
059600120504     c                             %subst(BLPcpi:1:2) <> '$$'
059700120504     c                   eval      $piva = *on
059800120509       //?se ho la partita IVA faccio finta di avere anche il codice fiscale
059900120509     c                   eval      $cdfisc = *on
060000071220     c                   endif
060100071220      * controllo se ha il codice fiscale
060200120504      * solo se non ho già la partita IVA
060300120504     c                   IF        not $piva
060400061229     c                   clear                   dta4q
060500061229     c                   eval      ar4trc = 'Q'
060600061229     c     kfiar41       chain     fiar401l
060700061229     c                   if        %found(fiar401l)
060800061229      * uso la ds di tita4 xchè su fiar4 non è stata fatta ...ma tanto sono uguali
060900120504      * (non accetto PRIVATO e Cod.Fiscale lungo 11)
061000061229     c                   eval      dta4q = ar4not
061100061229     c                   endif
061200120504     c                   if        §ta4qcfm <> *blanks and
061300120504     c                             %scan('PRIVATO':§ta4qcfm) = 0
061400071220     c                   eval      $cdfisc = *on
061500061229     c                   endif
061600120504     c                   ENDIF
061700071220      * richiedo quello che manca
061800071220     c                   select
061900071220      * non ci sono tutti e due richiedo tutti e due
062000120504     c                   when      not $cdfisc and not $piva
062100120504     c                   eval      pdocodfisc = '3'
062200120504      * se non c'è la partita iva la richiedo
062300120504      * anche se ho codice fiscale lungo 11
062400120504     c                   when      not $piva and
062500120504     c                             %subst(§TA4qcfm:12:5) = *blanks
062600120504     c                   eval      pdocodfisc = '2'
062700071220      * se non c'è il codice fiscale lo richiedo
062800120509     c                   when      not $cdfisc
062900120504     c                   eval      pdocodfisc = '1'
063000071220     c                   endsl
063100120504
063200061229      * cerco l'importo del prepagato
063300061228     c     kfiar6        chain     fiar601l
063400061228     c                   if        %found(fiar601l)
063500061228     c                   eval      pdoimpprep = ar6ift
063600061228     c                   eval      pdodivprep = ar6div
063700061228     c                   endif
063800080215      * imposto l'icona a sinistra
063900080215     c                   eval      pdoiconasx = '05'
064000061228     c                   endif
064100061228     c                   endif
064200061228
064300061228      * bolla RC
064400061229      * (per ora va bene il controllo solo con la serie ORM, in quanto ora è utilizzata
064500061229      *  solo per gli ORM RC)
064600061228     c                   if        ormnsr <> *zeros
064700061228     c                   eval      pdobollarc = 'S'
064800061228     c                   endif
064900070312
065000070312      * imposto flag icona info
065100070312      * per ora imposto solo '00' per avvisare di presenza note
065200070312     c                   if        ormno1 <> *blanks or ormno2 <> *blanks
065300070312     c                   eval      pdoflginfo = '00'
065400070312     c                   endif
065500090211
065600090211      * se orm con destinatario inserito lo riporto sul campo pdonote
065700090211     c                   if        ormrsc <> *blanks
065800140319     c                   eval      PDOnote =  %trim(ORMrsc) + ' - ' +
065900140319     c                             %trim(ormloc)
066000090211     c                   if        ormnac <> *blanks
066100131203     c                   eval      pdonote = %trim(pdonote) + ' - ' +
066200131203     c                             %trim(ormnac)
066300090211     c                   else
066400131203     c                   eval      pdonote = %trim(pdonote) + ' (' +
066500131203     c                             %trim(ormprc) + ')'
066600090211     c                   endif
066700090211     c                   endif
066800140122
066900140122      /free
067000140122       //?Controllo se ci sono Note AUT
067100140122         wORTgst = 'S';
067200140122         setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
067300140122         reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
067400140122         DOW  not %eof(FNORT11L);
067500140122           PDOnotaut = %trim(PDOnotaut) + ORTnob;
067600140122           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wORTgst) FNORT11L;
067700140122         ENDDO;
067800140122
067900140122       //?Imposto anche le variazioni
068000140122         IF  OR56var <> *blanks;
068100140122           FLGvar = OR56var;
068200140122           xx = 1;
068300140122           FOR xx by 1 to %elem(wVar);
068400140122             IF  wVar(xx) <> *blanks;
068500140122               exsr tab_TCV;
068600140123               IF  wPDOvar = *blanks;
068700140203                 wPDOvar = 'Variato: ' + §TCVdesb;
068800140122               ELSE;
068900140124                 IF  %scan(%trim(§TCVdesb):wPDOvar) = 0;
069000140124                   wPDOvar = %trim(wPDOvar) + '+' + §TCVdesb;
069100140124                 ENDIF;
069200140122               ENDIF;
069300140122             ENDIF;
069400140122           ENDFOR;
069500140123           PDOvar = wPDOvar;
069600140122         ENDIF;
069700140122      /end-free
069800140122
069900140122      * imposto la data e ora del lavoro
070000061228
070100061228      * imposto la data e ora del lavoro
070200080311     c                   eval      pdodatora = %char(%timestamp:*iso0)
070300061228
070400061228     c                   endsr
070500080403
070600080403      *------------------------------------------------------------------------*
070700080403      * ANNULLA FIPDO00T quando quadrata la distinta
070800080403      *------------------------------------------------------------------------*
070900080403     c     sr_annulla    begsr
071000080403
071100080403      * aggancio la distinta
071200080403     c                   eval      dstnpg = 4
071300080403     c                   clear                   ddstflr
071400080403     c     kfidst1       chain     fidst01l
071500080403     c                   if        %found(fidst01l)
071600080403     c                   eval      ddstflr = dstflr
071700080403     c                   endif
071800080403      * leggo FIPDO00T
071900080403     c     kfipdo2       setll     fipdo01t
072000080403     c                   do        *hival
072100080403     c     kfipdo2       reade     fipdo01t
072200080403      * fine file esco
072300080403     c                   if        %eof(fipdo01t)
072400080403     c                   leave
072500080403     c                   endif
072600080403      * escludo gli annullati
072700080403     c                   if        pdoatb = 'A'
072800080403     c                   iter
072900080403     c                   endif
073000080403      * annullo
073100080403     c                   eval      pdoatb = 'A'
073200080403      * devo aggiornare il flag di ORM scaricato a PDA presente su FNORG
073300080403      * solo se la distinta non è in test
073400080403     c                   if        §dsttstpda <> 'O' and §dsttstpda <> 'E'
073500080403     c     korg          chain     fnorg01l
073600080403     c                   if        %found(fnorg01l)
073700080403     c                   eval      dorg01 = orgflo
073800080403     c                   clear                   §orgpda
073900080403     c                   eval      orgflo = dorg01
074000080403     c                   update    fnorg000
074100080403     c                   endif
074200080403     c                   endif
074300080403      * aggiorno fipdo
074400080403     c                   update    fipdo000
074500080403     c                   enddo
074600080403
074700140211      * cancello anche i record presente su FIPRO10F
074800080403     C/EXEC SQL
074900140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
075000080403     C+ and prondc = :or56ndcd
075100080403     C/END-EXEC
075200080403
075300080403     c                   endsr
075400080327
075500080404      *------------------------------------------------------------------------*
075600080404      * CNACELLA FIPDO00T quando quadrata la distinta
075700080404      *------------------------------------------------------------------------*
075800080404     c     sr_cancella   begsr
075900080404
076000080404      * aggancio la distinta
076100080404     c                   eval      dstnpg = 4
076200080404     c                   clear                   ddstflr
076300080404     c     kfidst1       chain     fidst01l
076400080404     c                   if        %found(fidst01l)
076500080404     c                   eval      ddstflr = dstflr
076600080404     c                   endif
076700080404      * leggo FIPDO00T
076800080404     c     kfipdo2       setll     fipdo01t
076900080404     c                   do        *hival
077000080404     c     kfipdo2       reade     fipdo01t
077100080404      * fine file esco
077200080404     c                   if        %eof(fipdo01t)
077300080404     c                   leave
077400080404     c                   endif
077500080404      * escludo gli annullati
077600080404     c                   if        pdoatb = 'A'
077700080404     c                   iter
077800080404     c                   endif
077900080404      * annullo
078000080404     c                   delete    fipdo000
078100080404     c                   enddo
078200080404
078300140211      * cancello anche i record presente su FIPRO10F
078400080404     C/EXEC SQL
078500140211     C+ DELETE FROM FIPRO10F WHERE profgs = :or56fgs
078600080404     C+ and prondc = :or56ndcd
078700080404     C/END-EXEC
078800080404
078900080404     c                   endsr
079000080404
079100080327      *------------------------------------------------------------------------*
079200080327      * Scrivo fase PDR sulla distinta
079300080327      *------------------------------------------------------------------------*
079400080327     c     sr_fase       begsr
079500080327
079600080327     c                   clear                   fior90ds
079700080327     c                   eval      or90tla = 'W'
079800080327     c                   eval      or90fas = 'PDR'
079900080327     c                   eval      or90fgs = orgfgs
080000080327     c                   eval      or90ndc = orgndc
080100080327     c                   eval      or90flgins = 'A'
080200080327     c                   eval      or90comit = or56comit
080300080327     c                   call      'FIOR90R'
080400080327     c                   parm                    kpjba
080500080327     c                   parm                    fior90ds
080600080327      * non controllo l'errore di ritorno tanto sono alla fine del PGM
080700080327      * al massimo non ho scritto la fase
080800080327
080900080327     c                   endsr
081000140122
081100140122       //--------------------------------------------------------------
081200140122       //?Aggancio tabella TCV.                                   ?
081300140122       //--------------------------------------------------------------
081400140122     c     tab_TCV       begsr
081500140122
081600140122     c                   clear                   dTCV
081700140122     c                   clear                   TIBS02DS
081800140122     c                   eval      T02mod = 'C'
081900140122     c                   eval      T02sif = knsif
082000140122     c                   eval      T02cod = 'TCV'
082100140122     c                   eval      T02ke1 = wVar(xx)
082200140122     c                   call      'TIBS02R'
082300140122     c                   parm                    kpjba
082400140122     c                   parm                    tibs02ds
082500140122     c                   IF        T02err = *blanks
082600140122     c                   eval      dTCV = T02uni
082700140122     c                   ENDIF
082800140122
082900140122     c                   ENDSR
083000061227
083100061227      *------------------------------------------------------------------------*
083200061227      * ROUTINE INIZIALE
083300061227      *------------------------------------------------------------------------*
083400061227     c     *inzsr        begsr
083500061227
083600061227     c     *entry        plist
083700061227     c                   parm                    kpjba
083800070124     c                   parm                    parm01
083900061227
084000070124     c                   clear                   fior56ds
084100070124
084200070122      * Richiamato da preparazione dati di massa
084300070124     c                   if        %parms = 1
084400070124     c                   eval      fior56ds = kpjbu
084500070124     c                   else
084600070124     c                   eval      fior56ds = parm01
084700070124     c                   endif
084800070115
084900070115     c                   clear                   or56err
085000070115     c                   clear                   or56msg
085100061227
085200080313      * disabilito comit su FNORG e FIPDO a meno che non sia espressamente
085300080313      * richiesto ('1')
085400080313     c                   if        or56comit <> '1'
085500080313     c                   eval      or56comit = '0'
085600080313     c                   end
085700080313
085800080313     c                   open      fnorg01L
085900140527
086000140527      /free
086100140527       // Eseguo OVRDBF sul file distinte per NON aspettare se RCD è allocato
086200140527         cmd = c_CmdOvrDbF;
086300140527         ExecuteCommand (cmd);
086400140527         Open FIDST03L;
086500140527      /end-free
086600080313
086700061227      * definizione chiavi
086800070115     c     kfnorm        klist
086900070115     c                   kfld                    or56poe
087000070115     c                   kfld                    or56nsr
087100070115     c                   kfld                    or56nor
087200070115     c                   kfld                    or56nrv
087300080403
087400080403     c     kfipdo2       klist
087500080403     c                   kfld                    or56fgs
087600080403     c                   kfld                    or56ndcd
087700080221
087800080221     c     kfipdo1       klist
087900080221     c                   kfld                    or56fgs
088000080221     c                   kfld                    or56ndcd
088100080221     c                   kfld                    or56poe
088200080221     c                   kfld                    or56nsr
088300080221     c                   kfld                    or56nor
088400080221     c                   kfld                    or56nrv
088500061227
088600061227     c     kfipdo        klist
088700071205     c                   kfld                    orgfgs
088800071205     c                   kfld                    ormndc
088900071205     c                   kfld                    ormpoe
089000071205     c                   kfld                    ormnsr
089100071205     c                   kfld                    ormnor
089200071205     c                   kfld                    ormnrv
089300061227
089400070301     c     kfidst        klist
089500070307     c                   kfld                    dstnpg
089600070301     c                   kfld                    dstnfv
089700071205     c                   kfld                    orgfgs
089800080403
089900080403     c     kfidst1       klist
090000080403     c                   kfld                    dstnpg
090100080403     c                   kfld                    or56ndcd
090200080403     c                   kfld                    or56fgs
090300061227
090400061227     c     kfiar4        klist
090500061227     c                   kfld                    ar4trc
090600061227     c                   kfld                    ar4n14
090700061229
090800061229     c     kfiar41       klist
090900061229     c                   kfld                    ar4aas
091000061229     c                   kfld                    ar4lnp
091100061229     c                   kfld                    ar4nrs
091200061229     c                   kfld                    ar4nsp
091300061229     c                   kfld                    ar4trc
091400061227
091500061227     c     kfiar6        klist
091600061227     c                   kfld                    ar4aas
091700061227     c                   kfld                    ar4lnp
091800061227     c                   kfld                    ar4nrs
091900061227     c                   kfld                    ar4nsp
092000071220
092100071220     c     kfnblp        klist
092200071220     c                   kfld                    ar4aas
092300071220     c                   kfld                    ar4lnp
092400071220     c                   kfld                    ar4nrs
092500071220     c                   kfld                    ar4nsp
092600061227
092700080313     c     korm          klist
092800080313     c                   kfld                    ormpoe
092900080313     c                   kfld                    ormnsr
093000080313     c                   kfld                    ormnor
093100080313     c                   kfld                    ormnrv
093200080403
093300080403     c     korg          klist
093400080403     c                   kfld                    pdopoe
093500080403     c                   kfld                    pdonsr
093600080403     c                   kfld                    pdonor
093700080403     c                   kfld                    pdonrv
093800080313
093900061227     c                   endsr
094000061227
