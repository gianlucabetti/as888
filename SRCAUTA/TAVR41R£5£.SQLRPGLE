000100070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400081125      *  Creazione archivio aumento tariffe
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800070712     ftntam04l  if   e           k disk
000900070712     fazorg01l  if   e           k disk
001000070712     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001100070712     ftabel00f  if   e           k disk
001200081128     fwfaut00f  o    e             disk    usropn extfile('EDPAUMENTI/WFAUT00F')
001300070712
001400070712      *------------------------------------------------------------------------*
001500070712      *  RIEPILOGO INDICATORI
001600070712      *------------------------------------------------------------------------*
001700070712      * 02 - ho scritto almeno 1 rcd
001800070712
001900070712      *------------------------------------------------------------------------*
002000070712      *   V A R I A B I L I
002100070712      *------------------------------------------------------------------------*
002200070712     d comman          s             80
002300070712     d codut           s              1  0 inz(1)
002400070712     d kcar            s                   like(orgcar)
002500070712     d kcod            s                   like(tblcod)
002600070712     d kksc            s                   like(tamksc)
002700070712     d kkey            s                   like(tblkey)
002800070712     d lenght          s             15  5 inz(80)
002900070712     d kkcc            s                   like(acokcc) inz(151)
003000070712     d kkut            s                   like(acokut) inz(1)
003100070712     d wctr            s                   like(tamctr)
003200070712     d wdata           s              8  0
003300070712     d wksc            s                   like(tamksc)
003400070712     d wlib            s              9
003500070712     d wok             s              1    inz(*off)
003600070712     d wora            s              6  0
003700070712     d w0030           s              3  0
003800070712     d w0140           s             14  0
003900081127     d savclu          s                   like(wfaclu)
004000081128     d W_wfafbl        S                   like(WFAfblc)
004100081128     d W_istat         S              1
004200070712
004300070712      *------------------------------------------------------------------------*
004400070712      *   S C H I E R E
004500070712      *------------------------------------------------------------------------*
004600070712     d cmd             s             80    dim(1) ctdata perrcd(1)
004700070712     d dia             s              8  0 dim(12)
004800070712
004900070712      *------------------------------------------------------------------------*
005000070712      *   D S   I N T E R N E / E S T E R N E
005100070712      *------------------------------------------------------------------------*
005200080924
005300080924     d wlbdat          ds                  inz
005400080924     d  g02dat                 1      8  0
005500080924     d  g02inv                 9     16  0
005600080924     d  g02err                17     17
005700080924     d  g02tgi                18     22  0
005800080924
005900070712
006000070712     d azuteds       e ds                  extname(azute00f)
006100070712     d ddatiute      e ds
006200070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
006300070712     d ds_cnind      e ds                  inz extname(cnind00f)
006400070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
006500070712     d ds_fncls      e ds                  inz extname(fncls00f)
006600070712     d kpjba         e ds
006700070712     d dsta01        e ds
006800070712     d ds01          e ds
006900081127     d ds15          e ds
007000070712     d og143         e ds
007100070712     d tibs02ds      e ds
007200070712     d tibs10ds      e ds
007300070712     d tibs34ds      e ds
007400070712     d tibs69ds      e ds
007500070712     d tise70ds      e ds
007600081127     d wfautds       e ds                  extname(wfaut00f)
007700081127     d                                     prefix(DS_)
007800070712
007900070712      *------------------------------------------------------------------------*
008000081126      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008100081126     c
008200081126     c/EXEC SQL
008300081126     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008400081126     c/END-EXEC
008500081126     c*
008600070712
008700070712     c                   clear                   wksc
008800070712     c                   eval      wctr = 999
008900070712     c                   move      *all'0'       kksc
009000070712
009100070712     c     kksc          setll     tntam04l
009200070712    1c                   do        *hival
009300070712     c                   read      tntam04l
009400070712      * fine file
009500070712     c                   if        %eof(tntam04l)
009600070712     c                   leave
009700070712     c                   endif
009800081128      * se tariffe di cartello leggo il successivo
009900081128     c                   movel     tamksc        w0030
010000081128     c                   if        w0030 = 888
010100081128     c                   iter
010200081128     c                   endif
010300081125      * cambio tariffa
010400070712    2c                   if        tamctr <> wctr or tamksc <> wksc
010500070712     c                   movel     tamflo        dsta01
010600070712
010700070712      * escludo le annullate
010800070712     c                   if        tamatb <> *blanks
010900070712     c                   iter
011000070712     c                   endif
011100070712      * scrivo il file
011200070712     c                   exsr      scrivi
011300070712
011400070712     c                   eval      wksc = tamksc
011500070712     c                   eval      wctr = tamctr
011600070712
011700111209      * se la tariffa che sto leggendo andrà in vigore dopo il 31/12/11 la elaboro
011800070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
011900111209     c                   if        tamddt > 20111231
012000070712     c                   eval      wctr = 999
012100070712     c                   endif
012200070712    2c                   endif
012300070712
012400070712    1c                   enddo
012500081126     c                   exsr      SR_Dettaglio
012600081126
012700070712     c                   eval      *inlr = *on
012800081126
012900070712      *------------------------------------------------------------------------*
013000081127      * SCRIVO IL FILE PER GLI AUMENTI
013100070712      *------------------------------------------------------------------------*
013200070712     c     scrivi        begsr
013300070712
013400081127     c                   clear                   wfaut000
013500070712
013600081127      * tipo record cliente/tariffa
013700081127     c                   eval      wfatcl = 'C'
013800081127      * cerco il codice cliente unificante
013900081127     c                   clear                   tibs10ds
014000081127     c                   eval      d10tle = 'ST'
014100081127     c                   eval      d10paf = 'P'
014200081127     c                   z-add     tamksc        d10cod
014300111209     c                   z-add     20111130      d10drf
014400081202     c                   call      'TIBS10R'
014500081127     c                   parm                    tibs10ds
014600081127     c                   parm      'A'           partpexe          1
014700081127      * se ci sono errori valorizzo come unificante il cliente
014800081127     c                   if        d10err <> ' '
014900081128     c                   eval      wfaclu = tamksc
015000081127      * se tutto ok valorizzo l'unificante trovato
015100081127     c                   else
015200081127     c                   eval      wfaclu = d10cop
015300081127     c                   endif
015400081127      * cliente
015500081127     c                   eval      wfaclf = tamksc
015600081127      * cerco i dati anagrafici
015700081127     c                   clear                   tibs69ds
015800081127     c                   move      tamksc        i69kac
015900081127     c                   move      tamksc        i69kcp
016000081127     c                   move      tamksc        i69kin
016100081127     c                   call      'TIBS69R'
016200081127     c                   parm                    tibs69ds
016300081127     c                   parm                    ds_cnaco
016400081127     c                   parm                    ds_cnind
016500081127     c                   parm                    ds_cnclp
016600081127     c                   parm                    ds_fncls
016700081127
016800081127     c                   IF        acoabl <> ' '
016900081127     c                   eval      Wfafblc = 'S'
017000081127     c                   endif
017100081127      * ragione sociale e indirizzo
017200081127     c                   eval      wfarag = acorag
017300081127     c                   eval      Wfaind = Indvia
017400081127     c                   eval      Wfacit = Indcit
017500081127     c                   eval      Wfacap = Indcae
017600081127     c                   eval      Wfasta = Indsta
017700081127     c                   eval      Wfaprv = Indprv
017800081127      * decodifca della nazione
017900081127     c                   If        wfasta <> *blanks
018000081128     c                   eval      kcod = '15'
018100081127     c                   movel(p)  wfasta        kkey
018200081128     c     Ktabel        chain     tabel00f
018300081127     c                   If        %found(tabel00f)
018400081127     c                   eval      ds15 = tbluni
018500081127     c                   eval      wfanaz = §15des
018600081127     c                   endif
018700081128     c                   else
018800081128     c                   clear                   wfanaz
018900081127     c                   endif
019000070712      * importanza cliente
019100081127     c                   eval      wfaclv = clpclv
019200081209      * agente del cliente
019300081209     c                   eval      wfaagec= clpage
019400081209      * decofico agente del cliente
019500081209     c                   eval      kcod = '01'
019600081209     c                   movel     clpage        kkey
019700081209     c     ktabel        chain     tabel00f
019800081209     c                   if        not %found(tabel00f) or tblflg <> *blanks
019900081209     c                   eval      wfaagdc = *all'*'
020000081209     c                   else
020100081209     c                   eval      ds01    = tbluni
020200081209     c                   eval      wfaagdc = §01age
020300081209     c                   endif
020400070712      * tariffa
020500081127     c                   eval      wfactrv = tamctr
020600081127     c                   eval      wfaprgv = tamprg
020700081127     c                   eval      wfaddtv = tamddt
020800081127     c                   eval      wfadstv = tamdst
020900070712      * istat presente
021000081125      * escludo le bloccate
021100081125     c                   if        tambap = 'B'
021200081127     c                   eval      wfabap = tambap
021300081125     c                   else
021400101130     c                   if        tamrct <> 0
021500081209     c                   eval      wfaistv = 'S'
021600070712      * istat assente
021700070712     c                   else
021800081209     c                   eval      wfaistv = 'N'
021900070712     c                   endif
022000081125     c                   endif
022100081127      * percentuali addizionali di gestione
022200081127     c                   eval      wfapagv = tampag
022300081127     c                   eval      wfappav = tamppa
022400081125
022500081127     c                   write     wfaut000
022600070712
022700070712     c                   endsr
022800070712
022900081126
023000081126      *------------------------------------------------------------------------*
023100081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
023200081126      *------------------------------------------------------------------------*
023300081126     c     Sr_dettaglio  begsr
023400081126
023500081126     C/EXEC SQL
023600081126     C+ declare C1 cursor for
023700081127     C+ SELECT * FROM EDPAUMENTI/WFaut00F
023800081127     C+ WHERE WFATCL = 'C'
023900081127     C+ ORDER BY wfaclu , wfaclf
024000081128     c+ for read only
024100081126     C/END-EXEC
024200081126
024300081126     C/EXEC SQL
024400081126     C+ open C1
024500081126     C/END-EXEC
024600081126     C
024700081126     C/EXEC SQL
024800081127     C+ Fetch C1 into :wfautds
024900081126     C/END-EXEC
025000081126     C*
025100081126
025200081126     C*
025300081126     C                   dow       sqlcod = *zeros
025400081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
025500081127     c                   If        savclu <> ds_WFACLU
025600081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
025700081127     c                   If        savclu <> *zeros
025800081126     c                   exsr      Sr_unific
025900081126     c                   endif
026000081127     c                   movel     ds_wfaclu     savclu
026100081127      * imposto il campo di lavoro per verificare se il cliente è bloccato
026200081126     c                   eval      W_wfafbl = 'S'
026300081126      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
026400081126     c                   eval      W_istat = ' '
026500081127     c                   endif
026600081127      * per ogni tariffa memorizzo le caratteristiche
026700081126      * per ogni tariffa cliente verifico istat
026800081126     c                   select
026900081126      * tariffa con   istat
027000081209     c                   when      DS_wfaistv = 'S'
027100081126
027200081126      * imposto il flag dell'unificante
027300081126     c                   if        w_istat = ' ' or w_istat = 'S'
027400081126     c                   eval      w_istat = 'S'                                si istat
027500081126     c                   else
027600081126     c                   eval      W_istat = 'M'                                misto
027700081126     c                   endif
027800081126
027900081126      * tariffa senza istat
028000081209     c                   when      DS_wfaistv = 'N'
028100081126      * imposto il flag dell'unificante
028200081126     c                   if        w_istat = ' ' or w_istat = 'N'
028300081126     c                   eval      w_istat = 'N'                                no istat
028400081126     c                   else
028500081126     c                   eval      W_istat = 'M'                                misto
028600081126     c                   endif
028700081126      *
028800081126     c                   endsl
028900081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
029000081127     c                   if        DS_wfafblc = ' '
029100081127     c                   clear                   w_wfafbl
029200081127     c                   endif
029300081126     C*
029400081126     C*
029500081126     C/EXEC SQL
029600081127     C+ Fetch C1 into :wfautds
029700081126     C/END-EXEC
029800081126     C*
029900081126     C                   enddo
030000081128      * se è fine file scrivo l'ultimo unificante
030100081128     c                   If        sqlcod = 100 and savclu > *zero
030200081128     c                   exsr      sr_unific
030300081128     c                   endif
030400081126     C*
030500081126     C/EXEC SQL
030600081126     C+ close C1
030700081126     C/END-EXEC
030800081126     C
030900081126
031000081126     c                   endsr
031100081126      *---------------------------------------------------------------------------------------------
031200081126      * SR_UNIFIC    Elaborazione del cliente unificante
031300081126      *---------------------------------------------------------------------------------------------
031400081126     C     SR_UNIFIC     BEGSR
031500081126      *
031600081128     c                   clear                   wfaut000
031700081126      * aggancio anagrafiche
031800081126     C                   clear                   TIBS69ds
031900081128     C                   clear                   ds_CNACO
032000081128     C                   clear                   ds_CNIND
032100081128     C                   clear                   ds_CNCLP
032200081128     C                   clear                   ds_FNCLS
032300081127     C                   eval      I69kac =  savclu
032400081127     C                   eval      I69kin =  savclu
032500081127     C                   eval      I69kcp =  savclu
032600081126     C                   call      'TIBS69R'
032700081126     C                   parm                    TIBS69ds
032800081128     C                   parm                    ds_CNACO
032900081128     C                   parm                    ds_CNIND
033000081128     C                   parm                    ds_CNCLP
033100081128     C                   parm                    ds_FNCLS
033200081126      * valorizzo i campi del file
033300081126     c                   eval      Wfatcl = 'U'
033400081127     c                   eval      Wfaclu = savclu
033500081127     c                   eval      Wfacun = clpclv
033600081127     c                   eval      Wfaclv = clpclv
033700081127
033800081127     c                   clear                   Wfaclf
033900081126     c                   eval      Wfarag = Acorag
034000081209     c                   eval      Wfarsu = Acorag
034100081127      * cerco codice commerciale
034200081127      * verifico il codice commerciale unificante
034300081127     c                   eval      kcod = '01'
034400081127     c                   movel     clpage        kkey
034500081127     c     ktabel        chain     tabel00f
034600081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
034700081127     c                   eval      wfaage  = clpage
034800081127     c                   eval      wfaagd  = *all'*'
034900081127     c                   else
035000081127     c                   eval      ds01    = tbluni
035100081127     c                   eval      wfaage  = §01rgf
035200081127     c                   movel     wfaage        kkey
035300081127     c     ktabel        chain     tabel00f
035400081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
035500081127     c                   eval      wfaagd  = *all'*'
035600081127     c                   else
035700081127     c                   eval      ds01    = tbluni
035800081127     c                   eval      wfaagd  = §01age
035900081127     c                   endif
036000081127     c                   endif
036100081127
036200081127      * albero commerciale unificante
036300081127     c                   movel     wfaage        w0030
036400081127     c     w0030         chain     azorg01l
036500081127
036600081127      * distretto
036700081127     c                   eval      wfadiv = orgfl3
036800081127     c                   eval      kcod = '17'
036900081127     c                   eval      kkey = wfadiv
037000081127     c     ktabel        chain     tabel00f
037100081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
037200081127     c                   eval      wfadid  = *all'*'
037300081127     c                   else
037400081127     c                   eval      wfadid  = tbluni
037500081127     c                   endif
037600081128      * se divisione 7 cancello tutti i clienti legati all'unificante
037700081128     c                   If        wfadiv = '7'
037800081128     C/EXEC SQL
037900081128     C+ delete from EDPAUMENTI/WFaut00F as a
038000081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
038100081128     C/END-EXEC
038200081128     c                   leavesr
038300081128     c                   endif
038400081127      * area
038500081127     c                   eval      wfaare = orgcar
038600081127     c                   eval      kcod = '05'
038700081127     c                   movel     wfaare        kkey
038800081127     c     ktabel        chain     tabel00f
038900081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
039000081127     c                   eval      wfaard  = *all'*'
039100081127     c                   else
039200081127     c                   eval      wfaard  = tbluni
039300081127     c                   endif
039400081127      * filiale
039500081127     c                   eval      wfafil = orgfil
039600081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
039700081127     c                   eval      wfafid  = *all'*'
039800081127     c                   else
039900081127     c                   eval      wfafid  = orgdes
040000081127     c                   endif
040100081128     c                   eval      Wfainf = orgind
040200081127     c                   eval      Wfacpf = orgcpf
040300081127     c                   eval      Wfaloc = orgloc
040400081127     c                   eval      Wfapro = orgpro
040500081127     c                   eval      Wfatel = orgtel
040600081127     c                   eval      Wfafax = orgfax
040700081127     c                   eval      Wfaind = Indvia
040800081127     c                   eval      Wfacit = Indcit
040900081127     c                   eval      Wfacap = Indcae
041000081127     c                   eval      Wfasta = Indsta
041100081127     c                   eval      Wfaprv = Indprv
041200081127      * decodifca della nazione
041300081127     c                   If        wfasta <> *blanks
041400081128     c                   eval      kcod = '15'
041500081127     c                   movel(p)  wfasta        kkey
041600081128     c     Ktabel        chain     tabel00f
041700081127     c                   If        %found(tabel00f)
041800081127     c                   eval      ds15 = tbluni
041900081127     c                   eval      wfanaz = §15des
042000081127     c                   endif
042100081128     c                   else
042200081128     c                   clear                   wfanaz
042300081127     c                   endif
042400081127      * valorizzo se il cliente  è bloccato
042500081128     c                   eval      wfafblc = w_wfafbl
042600081127      * flag ISTAT
042700081127     c                   eval      wfafisc = w_istat
042800081204      * delta del cliente  -- RIMOSSO - Nicola 4-12-2008 --
042900081204     c                   clear                   wfadel
043000091126     C*   /EXEC SQL
043100091126     C*   +  select waprr_10ac ,waprp_10ac  into
043200091126     C*   +  :waprr_10ac ,:waprp_10ac from WADELPCT
043300091126     C*   +  where wapcli =  :savclu  and wapaad = 2008
043400091126     C*   /END-EXEC
043500081202      * se non trovo il record metto il delta a zero
043600081204     c*                  if        sqlcod <> 0
043700081204     c*                  clear                   wfadel
043800081202
043900081204     c*                  else
044000081202
044100081204     c*                  if        waprr_10ac > 0
044200081204     c*                  eval      wfadel =((waprr_10ac - waprp_10ac)*100)/
044300081204     c*                                     waprr_10ac
044400081204     c*                  else
044500081204     c*                  clear                   wfadel
044600081204     c*                  endif
044700081203
044800081204     c*                  endif
044900081127      * pulisco i flag aumento e lettera
045000081128     c                   clear                   wfafltu
045100081128     c                   clear                   wfafauc
045200081127      * verifico se devo fare l'aumento oppure no
045300081126     c                   SELECT
045400081126      * distretto  6/7 scartato
045500081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
045600081127     c                   eval      Wfafblc= 'S'
045700081127      * categoria 'C' controllo ISTAT
045800081127     c                   When      Wfacun = 'C'
045900081126      * no bloccato  lettera di aumento   se almeno uno dei figli ha le tariffe e se non ha istat
046000081127     c                   If        wfafblc  = ' '
046100111212      * ISTAT misto   (anno 2011 x 2012 non vogliono istat  automatico x clienti C)
046200111212     c                   If        wfafisc = 'M'
046300111212     c******             eval      wfafauc = 'I'
046400081127     c                   endif
046500081127
046600091201      * ISTAT NO   NON SI FA NULLA
046700091201     c*                  If        wfafisc = 'N'
046800091201     c*                  eval      wfafauc = 'A'
046900091201     c*                  eval      wfafltu = '1'
047000091201     c*                  endif
047100081126      *
047200081127     c                   endif
047300081126      *
047400081126     c                   endsl
047500081126      * scrivo il record
047600081126     c                   write     wfaut000
047700081128
047800081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
047900081209      * i dati relativi all'area di appartenenza dell'agente unificante , la ragione sociale
048000081209      * dell'unificante
048100081128     C/EXEC SQL
048200081128     C+ update EDPAUMENTI/WFaut00F as a
048300081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
048400081209     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun,
048500081209     c+ a.wfarsu  = :wfarsu , a.wfadiv = :wfadiv,
048600081209     c+ a.wfadid  = :wfadid , a.wfaare = :wfaare,
048700081209     c+ a.wfaard  = :wfaard , a.wfafil = :wfafil,
048800081209     c+ a.wfafid  = :wfafid , a.wfaage = :wfaage,
048900081209     c+ a.wfaagd  = :wfaagd
049000081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
049100081128     C/END-EXEC
049200081128     c
049300081128
049400081126     c                   endsr
049500081126      *
049600070712      *--------------------------------------------------------------*
049700070712      * ROUTINE INIZIALE
049800070712      *--------------------------------------------------------------*
049900070712     c     *inzsr        begsr
050000070712
050100070712     c     *entry        plist
050200070712     c                   parm                    kpjba
050300070712
050400070712     c     *dtaara       define    §azute        azuteds
050500070712     c     *dtaara       define    §datiute      ddatiute
050600070712
050700070712     c                   in(e)     *dtaara
050800070712     c                   if        %error or rsut = *blanks
050900070712     c                   clear                   tibs34ds
051000070712     c                   call      'TIBS34R'
051100070712     c                   parm                    tibs34ds
051200070712     c                   in        *dtaara
051300070712     c                   endif
051400070712
051500070712      * pulisco il file
051600070712     c                   clear                   comman
051700070712     c                   movel(p)  cmd(1)        comman
051800070712     c                   call      'QCMDEXC'
051900070712     c                   parm                    comman
052000070712     c                   parm                    lenght
052100070712
052200081128     c                   open      wfaut00f
052300070712
052400070712      * reperisco data e ora del lancio
052500070712     c                   time                    w0140
052600070712     c                   move      w0140         wdata
052700070712     c                   movel     w0140         wora
052800070712
052900070712     c     kazorg        klist
053000070712     c                   kfld                    dutdis
053100070712     c                   kfld                    kcar
053200070712
053300070712     c     kcli          klist
053400070712     c                   kfld                    kkut
053500070712     c                   kfld                    kkcc
053600070712     c                   kfld                    tamksc
053700070712
053800070712     c     ktabel        klist
053900070712     c                   kfld                    codut
054000070712     c                   kfld                    kcod
054100070712     c                   kfld                    kkey
054200070712
054300070712     c                   endsr
054400070712      *--------------------------------------------------------------*
054500070712**   cmd - comando
054600081128CLRPFM FILE(EDPAUMENTI/WFAUT00F)
