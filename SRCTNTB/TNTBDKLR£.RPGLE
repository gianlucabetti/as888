000100060201      *------------------------------------------------------------------------*
000200060201      * Gestione tabella "ABC" = Abilitazioni clienti                          *
000300060201      *------------------------------------------------------------------------*
000400110804
000500110804     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000600110804     h dftactgrp(*no)
000700110804     h bnddir('UBBNDDIR')
000800110804
000900110804      *------------------------------------------------------------------------*
001000110804     ftntbe01l  uf a e           k disk    extfile(wLibFile)  usropn
001100060201     ftntb44d   cf   e             workstn
001200110804
001300110804      *---------------------------------------------------------------*
001400110804      *   C O S T A N T I                                             *
001500110804      *---------------------------------------------------------------*
001600110804       // -?Costanti per la definizione delle schiere con i nomi?
001700110804       //  ?degli iSeries da elaborare e delle relative librerie?
001800110804     d c_NrSyst        c                   const(2)
001900110804     d c_NrLibr        c                   const(2)
002000060201
002100060201      *------------------------------------------------------------------------*
002200060201      *   C A M P I   D I   L A V O R O
002300060201      *------------------------------------------------------------------------*
002400060201     d xx              s              2  0 inz
002500060201     d $fine           s              1a   inz(*off)
002600060201     d $carv1          s              1a   inz(*off)
002700060201     d $carv2          s              1a   inz(*off)
002800060201     d $carw1          s              1a   inz(*off)
002900060201     d tipvid          s              1a   inz('1')
003000060201     d win             s             99a   inz(*zeros)
003100060201     d wtasto          s              2a   inz(*zeros)
003200110804
003300110804       // -?Nome del sistema?
003400110804     d currSysNeta     s              8a   inz
003500110804       // -?Nome esteso Libreria/File del file tabella?
003600110804     d wLibFile        s             21a   inz
003700110804       // -?Campi di comodo?
003800110804     d w_iSystem       s              1  0 inz
003900110804     d w_SisInf        s              3  0 inz
004000060201
004100060201      *------------------------------------------------------------------------*
004200060201      *   S C H I E R E
004300060201      *------------------------------------------------------------------------*
004400110804      *
004500110804       // -?iSeries  &  Librerie con entrambi i file tabelle?
004600110804     d $iSystem        s                   like(currSysNetA)
004700110804     d                                     dim(c_NrSyst)
004800110804     d                                     ctdata   perrcd( 1)
004900110804     d $SisInf         s                   like(ds_Libr)
005000110804     d                                     dim(c_NrSyst)
005100110804     d                                     alt($iSystem)
005200110804
005300060201     d opz             s             15    dim(06) ctdata perrcd(1)
005400060201     d msg             s             78    dim(17) ctdata perrcd(1)
005500060201     d nmfl            s             10    dim(10)
005600060201
005700060201      *------------------------------------------------------------------------*
005800060201      *   D S   I N T E R N E / E S T E R N E
005900060201      *------------------------------------------------------------------------*
006000060201     d wlbdat          ds                  inz
006100060201     d  g02dat                 1      8  0
006200060201     d  g02inv                 9     16  0
006300060201     d  g02err                17     17
006400060201     d  g02tgi                18     22  0
006500060201
006600060201     d param01         ds
006700060201     d  p01abc                       15
006800060201     d  p01ord                        1
006900060201     d  p01ke1                       15
007000060201     d  p01rit                        1
007100060201
007200060201     d azuteds       e ds                  inz extname(AZUTE00F)
007300060201     d dabc          e ds                  inz
007400060201     d ddatiute      e ds                  inz
007500060201     d kpjba         e ds
007600060201     d tibs02ds      e ds                  inz
007700060201     d tibs34ds      e ds                  inz
007800060201     d tntbeds       e ds                  extname(TNTBE00F) inz
007900060201     d xtntbeds      e ds                  extname(TNTBE00F) inz
008000060201     d                                     prefix(TBX:3)
008100110804       //
008200110804       // -?Ridefinizione elenco librerie in elaborare le tabelle?
008300110804     d ds_Libr         ds                  inz
008400110804     d  $Libr                        10    dim(c_NrLibr) inz
008500110804
008600110804       //--------------------------------------------------------------
008700110804       //?Definizione prototipi procedure.                             ?
008800110804       //--------------------------------------------------------------
008900110804
009000110804       // -?Reperimento NETA sistema AS/400 corrente?
009100110804      /copy gaitrasrc/srcProtoPr,UBRTVNETA
009200060201
009300060201      *------------------------------------------------------------------------*
009400060201      * RIEPILOGO INDICATORI
009500060201      *------------------------------------------------------------------------*
009600060201      *  01 - Record inesistente (inserimento)
009700060201      *  02 - Record esistente   (modifica)
009800060201      *  04 - Record annullato   (ripristino)
009900060201      *  20 - Comodo
010000060201      *  22 - Errori in scrittura record (WRITE)
010100060201      *------------------------------------------------------------------------*
010200060201
010300060201      * Operazioni iniziali
010400060201     c                   exsr      rutinz
010500060201
010600060201      * Gestione video
010700110804     c                   dow       $fine = *off
010800060201     c     tipvid        caseq     '1'           gesv1
010900060201     c     tipvid        caseq     '2'           gesv2
011000060201     c     tipvid        caseq     'A'           gesw1
011100060201     c                   endcs
011200060201     c                   enddo
011300060201
011400110804     c                   return
011500060131
011600060201      *------------------------------------------------------------------------*
011700060201      * RutInz - Operazioni Iniziali
011800060201      *------------------------------------------------------------------------*
011900060201     c     rutinz        begsr
012000060201
012100060131      * Ricezione parametri
012200060201     c     *entry        plist
012300060201     c                   parm                    kpjba
012400060201
012500060201      * Definizioni chiavi di accesso
012600060201     c     k05tbe01      klist                                                  *tntbe01l
012700060201     c                   kfld                    tbecod                         -tabella
012800060201     c                   kfld                    tbeke1                         -chiave uno
012900060201     c                   kfld                    tbeke2                         -chiave due
013000060201     c                   kfld                    tbelin                         -lingua
013100060201     c                   kfld                    tbesif                         -s.informativo
013200060201
013300060201      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
013400060201     c     *dtaara       define    §azute        azuteds
013500060201     c     *dtaara       define    §datiute      ddatiute
013600060201
013700060201     c                   clear                   azuteds
013800060201     c                   clear                   ddatiute
013900060201     c                   clear                   tibs34ds
014000060201     c                   in(e)     *dtaara
014100060201     c                   if        %error or rsut = *blanks
014200060201     c                   call      'TIBS34R'
014300060201     c                   parm                    tibs34ds
014400060201     c                   in        *dtaara
014500060201     c                   endif
014600110804      /free
014700110804
014800110804         *inLR = *on;
014900110804
015000110804         // -?Verifica del sistema AS/400 corrente?
015100110804         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
015200110804           $Fine = *on;
015300110804           leavesr;
015400110804         endif;
015500110804
015600110804         // -?Impostazione elenco librerie in cui gestire le tabelle?
015700110804         //  ?(a seconda del sistema in cui si stà lavorando)?
015800110804         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
015900110804         if  w_iSystem = *zero;
016000110804           $Fine = *on;
016100110804           leavesr;
016200110804         endif;
016300110804
016400110804         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
016500110804         w_SisInf = 1;
016600110804         exsr  sr_OpenFileTab;
016700110804
016800110804      /end-free
016900110804
017000060201      * Verifica errori e autorità profilo
017100060201     c                   select
017200060201      * controllo se ho errori nei dati utente
017300060201      * nel qual caso NON risulta un profilo abilitato
017400060201     c                   when      duterr = 'E'
017500110804     c                   eval      $fine  = *on
017600060201
017700060201      * CONTROLLO AUTORITA'
017800060201      * POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
017900060201      * se il chiamante non richiede autorità specifica verificare
018000060201      * quella del profilo
018100060201      * se il chiamante richiede autorità specifica verificarla,
018200060201      * se è blank verificare quella del profilo
018300060131      * se UTEAUT = *BLANK non siamo in sede
018400060201     c                   when      uteaut = *blank
018500060201     c                   other
018600060201     c                   endsl
018700081204
018800081204     c                   eval      $carv1 = *on
018900060131
019000060201     c                   endsr
019100060201      *------------------------------------------------------------------------*
019200060201      * GESV1  - Gestione videata selezione codice tabella
019300060201      *------------------------------------------------------------------------*
019400060201     c     gesv1         begsr
019500060201
019600060201      * Inizializzazione videata
019700060201     c                   if        $carv1 = *on
019800060201     c                   exsr      carv1
019900060201     c                   movel     *off          $carv1
020000060201     c                   endif
020100060201      * Scrivo la testata
020200060201     c                   clear                   t1opz
020300060201     c                   write     tb44t1
020400060201
020500060201      * Se esistono errori sulla videata
020600060201      * emetto la write del formato a indicatori spenti per vedere
020700060201      * le eventuali decodifiche
020800060201     c                   if        *in99
020900060201     c                   movea     *in           win
021000060201     c                   movea     *zeros        *in(50)
021100060201     c                   write     tb44v1
021200060201     c                   movea     win           *in
021300060201     c                   endif
021400060201
021500060201     c                   exfmt     tb44v1
021600060201     c                   eval      *in99 = *off
021700060201     c                   clear                   v1msg
021800060201
021900060201      * f03=fine
022000060201     c                   if        *inkc
022100060201     c                   exsr      f03v1
022200060201     c                   goto      endgesv1
022300060201     c                   endif
022400060201
022500060201      * Ricerche
022600060201     c                   clear                   xx
022700060201     c                   clear                   nmfl
022800060201     c     '?'           scan      v1cabc                                 20
022900060201     c                   if        *in20
023000060201     c                   add       1             xx
023100060201     c                   eval      nmfl(xx) = 'V1CABC    '
023200060201     c                   exsr      search
023300060201     c                   endif
023400060201
023500060201      * Controllo e decodifiche dati immessi a video
023600060201     c                   if        not *in99
023700060201     c                   exsr      ctrv1
023800060201     c                   endif
023900060201
024000060201      * Passaggio alla videata di dettaglio
024100060201     c                   if        not *in99
024200060201     c                   eval      $carv2 = *on
024300060201     c                   eval      tipvid = '2'
024400060201     c                   endif
024500060201
024600060201     c     endgesv1      endsr
024700060201
024800060201      *------------------------------------------------------------------------*
024900060201      * CARV1  - Caricamento dati prima videata
025000060201      *------------------------------------------------------------------------*
025100060201     c     carv1         begsr
025200060201
025300060201     c                   movea     *zeros        *in(50)
025400060201     c                   clear                   tb44v1
025500060131
025600060201     c                   movel     '?'           V1CABC
025700060201
025800060201     c                   endsr
025900060131
026000060201      *------------------------------------------------------------------------*
026100060201      * CTRV1  - Controllo e decodifica prima videata
026200060201      *------------------------------------------------------------------------*
026300060201     c     ctrv1         begsr
026400060201
026500060201     c                   movea     *zeros        *in(50)
026600060201     c                   clear                   v1msg
026700060131      *
026800060201      * Azione obbligatoria
026900060201     c                   if        v1cabc = *blanks
027000060201     c                   seton                                        51  99
027100060201     c                   eval      v1msg = msg(01)
027200060201     c                   goto      endctrv1
027300060201     c                   else
027400060201      * Controllo/Decodifica abilitazione
027500060201     c                   reset                   tibs02ds
027600111027     c                   eval      T02mod = 'C'
027700111027     c                   eval      T02cod = 'ABC'
027800060201     c                   movel     tbxSif        t02sif
027900060201     c                   movel     v1cabc        t02ke1
028000060201     c                   call      'TIBS02R'
028100060201     c                   parm                    kpjba
028200060201     c                   parm                    tibs02ds
028300060201     c                   if        t02err = *blanks
028400060201     c                   movel     t02uni        dabc
028500060201     c                   movel     d§abcdes      v1dabc
028600060201     c                   else
028700060201     c                   clear                   dabc
028800060201     c                   endif
028900060201     c                   endif
029000060201
029100060201     c     endctrv1      endsr
029200060131
029300060201      *------------------------------------------------------------------------*
029400060201      * F03V1  - Tasto funzionale F03 -> Fine programma
029500060201      *------------------------------------------------------------------------*
029600060201     c     f03v1         begsr
029700060201
029800110804     c                   eval      $fine = *on
029900060201
030000060201     c                   endsr
030100060131
030200060201      *------------------------------------------------------------------------*
030300060201      * SEARCH - Ricerche
030400060201      *------------------------------------------------------------------------*
030500060201     c     search        begsr
030600060201
030700060201     c                   eval      *in99 = *on
030800060201
030900060201     c                   do        10            xx
031000060201
031100060201     c                   if        nmfl(xx) = *blanks
031200060201     c                   leave
031300060201     c                   endif
031400060201
031500060201     c                   select
031600060201      * Abilitazione
031700060201     c                   when      nmfl(xx) =  'V1CABC    '
031800060201     c                   reset                   param01
031900060201     c                   movel     v1cabc        p01abc
032000060201     c                   movel     '0'           p01ord
032100060201     c                   movel     param01       kpjbu
032200060201     c                   call      'TNTB441R'
032300060201     c                   parm                    kpjba
032400060201     c                   movel     kpjbu         param01
032500060201     c                   if        p01ke1 <> *blanks
032600060201     c                   movel     p01ke1        v1cabc
032700060201     c                   endif
032800060201
032900060201     c                   endsl
033000060201
033100060201     c                   enddo
033200060201
033300060201     c                   endsr
033400060201
033500060201      *------------------------------------------------------------------------*
033600060201      * GESV2  - Gestione videata dettaglio dati
033700060201      *------------------------------------------------------------------------*
033800060201     c     gesv2         begsr
033900060201
034000060201      * Inizializzazione videata
034100060201     c                   if        $carv2 = *on
034200060201     c                   exsr      carv2
034300060201     c                   move      *off          $carv2
034400060201     c                   endif
034500060201      * Scrivo la testata
034600060201     c                   write     tb44t1
034700060201
034800060201      * Se esistono errori sulla videata
034900060201      * emetto la write del formato a indicatori spenti per vedere
035000060201      * le eventuali decodifiche
035100060201     c                   if        *in99
035200060201     c                   movea     *in           win
035300060201     c                   movea     *zeros        *in(50)
035400060201     c                   write     tb44v2
035500060201     c                   movea     win           *in
035600060201     c                   endif
035700060201
035800060201     c                   if        *in05
035900060201     c                   write     tb44v2
036000060201     c                   exfmt     protect
036100060201     c                   else
036200060201     c                   exfmt     tb44v2
036300060201     c                   endif
036400060201     c                   eval      *in99 = *off
036500060201     c                   clear                   v2msg
036600060201     c                   clear                   wtasto
036700060201
036800060201     c                   select
036900060201      * f03=fine
037000060201     c                   when      *inkc
037100060201     c                   exsr      f03v1
037200060201     c                   goto      endgesv2
037300060201      * f12=ritorno
037400060201     c                   when      *inkl
037500060201     c                   exsr      f12v2
037600060201     c                   goto      endgesv2
037700060201     c                   endsl
037800060201
037900060201      * Controllo dati immessi a video
038000060201     c                   exsr      ctrv2
038100060201
038200060201      * Aggiornamento se non ci sono errori
038300060201     c                   if        not *in99
038400060201     c                             and (*inkf or *inke or *inkq)
038500060201     c                   select
038600060201     c                   when      *inke
038700060201     c                   eval      wtasto = '05'
038800060201     c                   when      *inkf
038900060201     c                   eval      wtasto = '06'
039000060201     c                   when      *inkq
039100060201     c                   eval      wtasto = '16'
039200060201     c                   endsl
039300110804       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
039400110804       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
039500110804       //  ?NON ha più senso gestire la richiesta di trasmissione!?
039600110804     c**!!!              eval      $carw1 = *on
039700110804     c**!!!              eval      tipvid = 'A'
039800110804     c                   exsr      AggTBE
039900060201     c                   endif
040000060201
040100060201     c     endgesv2      endsr
040200060201
040300060201      *------------------------------------------------------------------------*
040400060201      * CARV2  - Caricamento dati seconda videata
040500060201      *------------------------------------------------------------------------*
040600060201     c     carv2         begsr
040700060201
040800060201     c                   clear                   t1opz
040900060201     c                   movea     '00000'       *in(01)
041000110804      /free
041100110804
041200110804         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
041300110804         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
041400110804           w_SisInf = 1;
041500110804           exsr  sr_OpenFileTab;
041600110804         endif;
041700110804
041800110804      /end-free
041900060201
042000060201      * Aggancio la tabella, se trovo il codice sono in modifica
042100060201      * o ripristino (se record annullato), altrimenti in immissione
042200060201     c                   exsr      chntbe
042300060201
042400060201     c                   if        %found(tntbe01l)
042500060201
042600060201      * MODIFICA/RIPRISTINO
042700060201     c                   movel     tbeuni        dabc
042800060201     c                   if        TBEatb = *blanks
042900060201     c                   eval      *in02  = *on
043000060201     c                   eval      t1opz  = opz(02)
043100060201     c                   else
043200060201     c                   eval      *in04  = *on
043300060201     c                   eval      t1opz  = opz(06)
043400060201     c                   endif
043500060201
043600060201     c                   else
043700060201
043800060201      * IMMISSIONE
043900060201     c                   clear                   dabc
044000060201     c                   eval      *in01  = *on
044100060201     c                   eval      t1opz  = opz(01)
044200060201
044300060201     c                   endif
044400060201
044500060201     c                   move      v1cabc        v2cabc
044600110722     c                   eval      v2cdes  = d§abcdes
044700110722     c                   eval      v2caut  = d§abcaut
044800110722     c                   eval      v2cord  = d§abcord
044900110722     c                   eval      v2cfile = d§abcfile
045000110722     c                   eval      v2cpgm  = d§abcpgm
045100110804     c                   eval      v2crpers = d§abcrpers
045200111118     c                   eval      v2cdesp  = d§abcdesp
045300110804     c                   eval      v2cipers = d§abcipers
045400110808     c                   eval      v2cfiles = d§abcfiles
045500111027
045600111027      * Decodifico chi autorizza
045700111027     c                   clear                   tibs02ds
045800111027     c                   eval      T02mod = 'C'
045900111027     c                   eval      T02cod = 'RAC'
046000111027     c                   movel     V2Caut        t02ke1
046100111027     c                   call      'TIBS02R'
046200111027     c                   parm                    kpjba
046300111027     c                   parm                    tibs02ds
046400111027     c                   if        t02err <> *blanks
046500111027     c                   eval      V2Daut = *all'? '
046600111027     c                   else
046700111027     c                   movel     t02uni        V2Daut
046800111027     c                   endif
046900060201
047000060201     c                   endsr
047100060201
047200060201      *------------------------------------------------------------------------*
047300060201      * F12V2  - Tasto funzionale F12 -> Ritorno
047400060201      *------------------------------------------------------------------------*
047500060201     c     f12v2         begsr
047600060201
047700060201     c                   eval      tipvid = '1'
047800081204     c                   eval      $carv1 = *on
047900060201     c                   unlock    tntbe01l
048000060201
048100060201     c                   endsr
048200060131
048300060201      *------------------------------------------------------------------------*
048400060201      * CTRV2  - Controllo e decodifica seconda videata
048500060201      *------------------------------------------------------------------------*
048600060201     c     ctrv2         begsr
048700060201
048800060201     c                   movea     *zeros        *in(50)
048900060201     c                   clear                   v2msg
049000060201
049100060201      * Non si fanno i controlli se richisto l'annullamento
049200060201     c     *inkq         cabeq     *on           endctrv2
049300060201
049400060201      * Descrizione obbligatoria
049500060201     c                   if        v2cdes =  *blanks
049600060201     c                   seton                                        51  99
049700060201     c                   movel     msg(02)       v2msg
049800060201     c                   leavesr
049900060201     c                   endif
050000111027
050100111027      * Chi autorizza
050200111027     c                   IF        V2Caut = '?'
050300111027     c                   clear                   tibs02ds
050400111027     c                   eval      T02mod = 'R'
050500111027     c                   eval      T02cod = 'RAC'
050600111027     c                   call      'TIBS02R'
050700111027     c                   parm                    kpjba
050800111027     c                   parm                    tibs02ds
050900111027     c                   IF        T02err = *blanks
051000111027     c                   eval      V2Caut = T02ke1
051100111027     c                   eval      V2Daut = T02uni
051200111027     c                   ENDIF
051300111027     c                   ENDIF
051400111111
051500111111     c                   IF        V2Caut = *blanks
051600111111     c                   eval      *in56 = *on
051700111111     c                   eval      *in99 = *on
051800111111     c                   eval      V2msg = msg(08)
051900111111     c                   leavesr
052000111111     c                   ENDIF
052100111111
052200111027     c                   clear                   tibs02ds
052300111027     c                   eval      T02mod = 'C'
052400111027     c                   eval      T02cod = 'RAC'
052500111027     c                   eval      T02ke1 = V2Caut
052600111027     c                   call      'TIBS02R'
052700111027     c                   parm                    kpjba
052800111027     c                   parm                    tibs02ds
052900111027     c                   IF        T02err <> *blanks
053000111027     c                   eval      *in56 = *on
053100111027     c                   eval      *in99 = *on
053200111027     c                   eval      V2msg = msg(08)
053300111027     c                   leavesr
053400111027     c                   ENDIF
053500111027     c                   eval      V2Daut = T02uni
053600060201
053700060201      * Ordinamento obbligatorio
053800060209     c                   If        v2cord  = *Zeros
053900060201     c                   seton                                        52  99
054000060201     c                   movel     msg(03)       v2msg
054100060201     c                   leavesr
054200060201     c                   endif
054300060201
054400110802      * File obbligatorio se non ho la ricerca personalizzata
054500110804     c                   if        v2cfile = *blanks and V2Crpers = *blanks
054600110802     c                   seton                                        53  99
054700060201     c                   movel     msg(04)       v2msg
054800060131     c                   leavesr
054900060201     c                   endif
055000110914
055100110914      * Non posso immettere il file da leggere se lettura personalizzata
055200110914     c                   if        V2Crpers <> *blanks and
055300110914     c                             V2Cfile  <> *blanks
055400110914     c                   seton                                        53  99
055500110914     c                   movel     msg(07)       v2msg
055600110914     c                   leavesr
055700110914     c                   endif
055800110808
055900110808      * Non posso immettere il flag di lettura file se lettura personalizzata
056000110914      * e nemmeno il nome del file
056100110808     c                   if        V2Crpers <> *blanks and
056200110914     c                             V2Cfiles <> *blanks
056300110808     c                   seton                                        55  99
056400110808     c                   movel     msg(06)       v2msg
056500110808     c                   leavesr
056600110808     c                   endif
056700110804
056800110804      * Non posso immettere il pgm e la personalizzazione
056900110804     c                   if        v2cpgm <> *blanks and V2Cipers <> *blanks
057000110804     c                   seton                                        54  99
057100110804     c                   movel     msg(05)       v2msg
057200110804     c                   leavesr
057300110804     c                   endif
057400060201
057500060201     c     *in99         cabeq     *on           endctrv2
057600060201
057700060201     c     endctrv2      endsr
057800060131
057900060201      *------------------------------------------------------------------------*
058000060201      * GESW1  - Gestione videata dati relativi alla trasmissione
058100060201      *------------------------------------------------------------------------*
058200060201     c     gesw1         begsr
058300060201
058400060201      * Inizializzazione videata
058500060201     c                   if        $carw1 = *on
058600060201     c                   exsr      carw1
058700060201     c                   movel     *off          $carw1
058800060201     c                   endif
058900060201
059000060201     c                   if        *in05
059100060201     c                   write     tb44W1
059200060201     c                   exfmt     protect
059300060201     c                   else
059400060201     c                   exfmt     tb44W1
059500060201     c                   endif
059600060201     c                   eval      *in99 = *off
059700060201     c                   clear                   w1msg
059800060201
059900060201     c                   select
060000060201      * f12=ritorno
060100060201     c                   when      *inkl
060200060201     c                   exsr      f12w1
060300060201     c                   goto      endgesw1
060400060201     c                   endsl
060500060201
060600060201      * Controllo dati immessi a video
060700060201     c                   exsr      ctrw1
060800060201
060900060201      * Aggiornamento se non ci sono errori
061000060201     c                   if        not *in99 and *inkf
061100060201     c                   exsr      aggtbe
061200060201     c                   endif
061300060201
061400060201     c     endgesw1      endsr
061500060201
061600060201      *------------------------------------------------------------------------*
061700060201      * CARW1  - Caricamento dati window
061800060201      *------------------------------------------------------------------------*
061900060201     c     carw1         begsr
062000060201
062100060201     c                   movea     *zeros        *in(50)
062200060201
062300060201     c                   select
062400060201
062500060201      * f5=ripristino
062600060201     c                   when      *inke   and  *in04
062700060201     c                   eval      w1ftt = tbeftt
062800060201
062900060201      * f6=conferma
063000060201     c                   when      *inkf
063100060201     c                   select
063200060201      *   Immissione
063300060201     c                   when      *in01
063400060201     c                   eval      w1ftt = tbxftt
063500060201      *   Modifica / Ripristino
063600060201     c                   when      *in02   or    *in04
063700060201     c                   eval      w1ftt = tbeftt
063800060201     c                   endsl
063900060201
064000060201      * F16=Annullamento
064100060201     c                   when      *inkq   and  not *in04
064200060201     c                   eval      w1ftt = tbeftt
064300060201
064400060201     c                   endsl
064500060201
064600060201      * Se NON immissione: visualizzo i dati relativi all'ultima
064700060201      * trasmissione
064800060201     c                   if        not *in01
064900060201     c                   eval      w1flt = tbeflt
065000060201     c                   eval      w1ftr = tbeftr
065100060201     c                   if        tbedtr <> 0
065200060201     c                   clear                   wlbdat
065300060201     c                   z-add     tbedtr        g02inv
065400060201     c                   movel     '3'           g02err
065500060201     c                   call      'XSRDA8'
065600060201     c                   parm                    wlbdat
065700060201     c                   z-add     g02dat        w1dtr
065800060201     c                   endif
065900060201     c                   endif
066000060201
066100060201     c                   endsr
066200060201
066300060201      *------------------------------------------------------------------------*
066400060201      * CTRW1  - Controllo e decodifica window
066500060201      *------------------------------------------------------------------------*
066600060201     c     ctrw1         begsr
066700060201
066800060201     c                   movea     *zeros        *in(50)
066900060201
067000060201     c     endctrw1      endsr
067100060201
067200060201      *------------------------------------------------------------------------*
067300060201      * F21W1  - Tasto funzionale F12 -> Ritorno
067400060201      *------------------------------------------------------------------------*
067500060201     c     f12w1         begsr
067600060201
067700060201     c                   eval      tipvid = '2'
067800060201
067900060201     c                   endsr
068000060201
068100060201      *------------------------------------------------------------------------*
068200060201      * CHNTBE * Aggancio tabella
068300060201      *------------------------------------------------------------------------*
068400060201     c     chntbe        begsr
068500060201
068600111027     c                   movel     'ABC'         tbecod
068700060201     c                   movel(p)  v1cabc        tbeke1
068800060201     c                   clear                   tbeke2
068900060201     c                   clear                   tbelin
069000060201     c                   movel     tbxsif        tbesif
069100060201     c     k05tbe01      chain     tntbe01l
069200060201      * Se non ho trovato il record con il sistema informativo
069300060201      * che ho in linea, lo abblenco
069400060201     c                   if        not %found(tntbe01l)
069500060201     c                   clear                   tbesif
069600060201     c     k05tbe01      chain     tntbe01l
069700060201     c                   endif
069800060201
069900060201     c                   endsr
070000060201
070100060201      *------------------------------------------------------------------------*
070200060201      * AGGTBE * Aggiornamento tabella
070300060201      *------------------------------------------------------------------------*
070400060201     c     AGGTBE        BEGSR
070500110804      *
070600110804      /free
070700110804
070800110804         // -?Ciclo di elaborazione per ogni sistema informativo?
070900110804         For  w_SisInf = 1  To  %elem($Libr);
071000110804
071100110804           // -?Apertura (eventuale) del file ed aggancio record?
071200110804           if  w_SisInf > 1;
071300110804             exsr  sr_OpenFileTab;
071400110804             exsr  ChnTBE;
071500110804           endif;
071600110804
071700110804         //? N.B.                                                      ?
071800110804         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
071900110804         //   trasmissione (vedi flag TBEFTR).                        ?
072000110804         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
072100110804         //   subito nello stesso file di entrambi i S.I. - in due    ?
072200110804         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
072300110804
072400110804      /end-free
072500060201
072600060201     c                   select
072700060201
072800060201      * f5=ripristino
072900060201     c                   when      wtasto='05'  and  *in04
073000110804      /free
073100110804               if  %found(TNTBE01L);
073200110804      /end-free
073300060201     c                   clear                   tbeatb
073400060201     c                   clear                   tbeftr
073500060201     c                   update    tntbe000
073600110804      /free
073700110804               else;
073800110804                 // - Record appena cancellato fisicamente
073900110804                 exsr  RieTBE;
074000110804                 WRITE  TNTBE000;
074100110804               endif;
074200110804      /end-free
074300060201
074400060201      * f6=conferma
074500060201     c                   when      wtasto='06'
074600060201     c                   select
074700060201      *   Immissione
074800060201     c                   when      *in01
074900060201     c                   exsr      rietbe
075000060201     c                   clear                   tbeflt
075100060201     c                   clear                   tbedtr
075200060201     c                   write     tntbe000                             22
075300060201      *   Modifica / Ripristino
075400060201     c                   when      *in02   or    *in04
075500060201     c                   exsr      rietbe
075600060201     c                   update    tntbe000
075700060201     c                   endsl
075800060201
075900060201      * f16=annullamento
076000060201     c                   when      wtasto='16'  and  not *in04
076100060201     c                   movel     'A'           tbeatb
076200060201     c                   clear                   tbeftr
076300060201     c                   update    tntbe000
076400060201
076500060201     c                   endsl
076600110804      /free
076700110804
076800110804         EndFor;
076900110804
077000110804      /end-free
077100060201
077200060201      * Torno alla prima videata che carico come da inizio pgm
077300060201     c                   movel     '1'           tipvid
077400060201     c                   movel     *on           $carv1
077500060201     c                   movel     *on           $carv2
077600060201     c                   movel     *on           $carw1
077700060201
077800060201     c                   endsr
077900060201
078000060201      *------------------------------------------------------------------------*
078100060131      * Riempimento file tabella
078200060201      *------------------------------------------------------------------------*
078300060201     c     rietbe        begsr
078400060201
078500060201     c                   clear                   tbeatb
078600060201     c                   if        tbxsif <> *blanks
078700060201     c                   movel     tbxsif        tbesif
078800060201     c                   else
078900060201     c                   clear                   tbesif
079000060201     c                   endif
079100060201     c                   movel     tbxapl        tbeapl
079200111027     c                   movel     'ABC'         tbecod
079300060201     c                   movel     v2cabc        tbeke1
079400060201     c                   clear                   tbeke2
079500060201
079600060201     c                   clear                   dabc
079700060201     c                   eval      d§abcdes  =  v2cdes
079800060201     c                   eval      d§abcaut  =  v2caut
079900060201     c                   eval      d§abcord  =  v2cord
080000060201     c                   eval      d§abcfile =  v2cfile
080100110722     c                   eval      d§abcpgm  =  v2cpgm
080200110804     c                   eval      d§abcrpers =  v2crpers
080300111118     c                   eval      d§abcdesp  =  v2cdesp
080400110804     c                   eval      d§abcipers =  v2cipers
080500110808     c                   eval      d§abcfiles =  v2cfiles
080600060201     c                   movel(p)  dabc          tbeuni
080700060201
080800110804     c**!!!              movel     w1ftt         tbeftt
080900110804     c**!!!              z-add     tbxflt        tbeflt
081000110804     c**!!!              clear                   tbeftr
081100110804      /free
081200110804
081300110804         TBEftt = 'S';
081400110804         TBEflt = TBXflt;
081500110804
081600110804         select;
081700110804           // - Annullamento o Ripristino
081800110804           when  (wTasto='05'  and  *in04)   or
081900110804                 (wTasto='16'  and  not *in04);
082000110804             clear  TBEftr;
082100110804             clear  TBEdtr;
082200110804           // - Aggiornamento o Scrittura in sede
082300110804           when  w_SisInf = 1;
082400110804             TBEftr = 'T';
082500110804             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
082600110804                                     : 1 : 8 ) );
082700110804           // - Aggiornamento o Scrittura in filiale
082800110804           other;
082900110804             TBEftr = 'R';
083000110804             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
083100110804                                     : 1 : 8 ) );
083200110804         endsl;
083300110804
083400110804      /end-free
083500110804      *
083600060201
083700060201     c                   endsr
083800110804      /free
083900110804
084000110804       //--------------------------------------------------------------
084100110804       //?Apertura dei files tabelle nel sistema informativo impostato.?
084200110804       //--------------------------------------------------------------
084300110804       BEGSR  sr_OpenFileTab;
084400110804
084500110804         // -?Chiusura (eventuale) archivio?
084600110804         if  %open(TNTBE01L);
084700110804           close  TNTBE01L;
084800110804         endif;
084900110804
085000110804         // -?Apertura archivio?
085100110804         ds_Libr  = $SisInf(w_iSystem);
085200110804         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
085300110804         open  TNTBE01L;
085400110804
085500110804       ENDSR;
085600110804
085700110804      /end-free
085800110804
085900110804       //--------------------------------------------------------------
086000110804       //?Schiere a tempo di compilazione.                             ?
086100110804       //--------------------------------------------------------------
086200110804
086300110804** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con il file TABEL00F?
086400110804SETRAS  GAITRAGRU FILTRAGRU
086500110804AS888   GAITRAGRPSFILTRAGRPF
086600060201** -OPZ-      *
086700060201  Inserimento
086800060201    Modifica
086900060201     Copia
087000060201  Annullamento
087100060201Visualizzazione
087200060201   ANNULLATO
087300060201** -MSG-                                                                     *
087400060201Immettere l'abilitazione                                                       01
087500060201Immettere la descrizione                                                       02
087600060201Immettere l'ordinamento di visualizzazione                                     03
087700110802Immettere il nome del file (TABEL/TNTBE) o la ricerca personalizzata           04
087800110804Il programma di interrogazione è in alternativa alla personalizzazione         05
087900110808E' stata indicata la ricerca personalizzata non impostare il flag di lettura   06
088000110914E' stata indicata la ricerca personalizzata non impostare il nome del file     07
088100111027Codice "Chi Autorizza" errato                                                  08
