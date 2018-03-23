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
006700070712     d dsqi1         e ds
006800070712     d dsqi2         e ds
006900070712     d dsta01        e ds
007000070712     d ds01          e ds
007100081127     d ds15          e ds
007200070712     d og143         e ds
007300070712     d tibs02ds      e ds
007400070712     d tibs10ds      e ds
007500070712     d tibs34ds      e ds
007600070712     d tibs69ds      e ds
007700070712     d tise70ds      e ds
007800081202     d wadelpc       e ds                  extname(wadelpct)
007900081127     d wfautds       e ds                  extname(wfaut00f)
008000081127     d                                     prefix(DS_)
008100070712
008200070712      *------------------------------------------------------------------------*
008300081126      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008400081126     c
008500081126     c/EXEC SQL
008600081126     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008700081126     c/END-EXEC
008800081126     c*
008900070712
009000070712     c                   clear                   wksc
009100070712     c                   eval      wctr = 999
009200070712     c                   move      *all'0'       kksc
009300070712
009400070712     c     kksc          setll     tntam04l
009500070712    1c                   do        *hival
009600070712     c                   read      tntam04l
009700070712      * fine file
009800070712     c                   if        %eof(tntam04l)
009900070712     c                   leave
010000070712     c                   endif
010100081128      * se tariffe di cartello leggo il successivo
010200081128     c                   movel     tamksc        w0030
010300081128     c                   if        w0030 = 888
010400081128     c                   iter
010500081128     c                   endif
010600081125      * cambio tariffa
010700070712    2c                   if        tamctr <> wctr or tamksc <> wksc
010800070712     c                   movel     tamflo        dsta01
010900070712
011000070712      * escludo le annullate
011100070712     c                   if        tamatb <> *blanks
011200070712     c                   iter
011300070712     c                   endif
011400070712      * scrivo il file
011500070712     c                   exsr      scrivi
011600070712
011700070712     c                   eval      wksc = tamksc
011800070712     c                   eval      wctr = tamctr
011900070712
012000081127      * se la tariffa che sto leggendo andrà in vigore dopo il 31/12/08 la elaboro
012100070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
012200081127     c                   if        tamddt > 20081231
012300070712     c                   eval      wctr = 999
012400070712     c                   endif
012500070712    2c                   endif
012600070712
012700070712    1c                   enddo
012800081126     c                   exsr      SR_Dettaglio
012900081126
013000070712     c                   eval      *inlr = *on
013100081126
013200070712      *------------------------------------------------------------------------*
013300081127      * SCRIVO IL FILE PER GLI AUMENTI
013400070712      *------------------------------------------------------------------------*
013500070712     c     scrivi        begsr
013600070712
013700081127     c                   clear                   wfaut000
013800070712
013900081127      * tipo record cliente/tariffa
014000081127     c                   eval      wfatcl = 'C'
014100081127      * cerco il codice cliente unificante
014200081127     c                   clear                   tibs10ds
014300081127     c                   eval      d10tle = 'ST'
014400081127     c                   eval      d10paf = 'P'
014500081127     c                   z-add     tamksc        d10cod
014600081202     c                   z-add     20081130      d10drf
014700081202     c                   call      'TIBS10R'
014800081127     c                   parm                    tibs10ds
014900081127     c                   parm      'A'           partpexe          1
015000081127      * se ci sono errori valorizzo come unificante il cliente
015100081127     c                   if        d10err <> ' '
015200081128     c                   eval      wfaclu = tamksc
015300081127      * se tutto ok valorizzo l'unificante trovato
015400081127     c                   else
015500081127     c                   eval      wfaclu = d10cop
015600081127     c                   endif
015700081127      * cliente
015800081127     c                   eval      wfaclf = tamksc
015900081127      * cerco i dati anagrafici
016000081127     c                   clear                   tibs69ds
016100081127     c                   move      tamksc        i69kac
016200081127     c                   move      tamksc        i69kcp
016300081127     c                   move      tamksc        i69kin
016400081127     c                   call      'TIBS69R'
016500081127     c                   parm                    tibs69ds
016600081127     c                   parm                    ds_cnaco
016700081127     c                   parm                    ds_cnind
016800081127     c                   parm                    ds_cnclp
016900081127     c                   parm                    ds_fncls
017000081127
017100081127     c                   IF        acoabl <> ' '
017200081127     c                   eval      Wfafblc = 'S'
017300081127     c                   endif
017400081127      * ragione sociale e indirizzo
017500081127     c                   eval      wfarag = acorag
017600081127     c                   eval      Wfaind = Indvia
017700081127     c                   eval      Wfacit = Indcit
017800081127     c                   eval      Wfacap = Indcae
017900081127     c                   eval      Wfasta = Indsta
018000081127     c                   eval      Wfaprv = Indprv
018100081127      * decodifca della nazione
018200081127     c                   If        wfasta <> *blanks
018300081128     c                   eval      kcod = '15'
018400081127     c                   movel(p)  wfasta        kkey
018500081128     c     Ktabel        chain     tabel00f
018600081127     c                   If        %found(tabel00f)
018700081127     c                   eval      ds15 = tbluni
018800081127     c                   eval      wfanaz = §15des
018900081127     c                   endif
019000081128     c                   else
019100081128     c                   clear                   wfanaz
019200081127     c                   endif
019300070712      * importanza cliente
019400081127     c                   eval      wfaclv = clpclv
019500070712      * tariffa
019600081127     c                   eval      wfactrv = tamctr
019700081127     c                   eval      wfaprgv = tamprg
019800081127     c                   eval      wfaddtv = tamddt
019900081127     c                   eval      wfadstv = tamdst
020000070712      * istat presente
020100081125      * escludo le bloccate
020200081125     c                   if        tambap = 'B'
020300081127     c                   eval      wfabap = tambap
020400081125     c                   else
020500070712     c                   if        tamfli <> 0
020600081127     c                   eval      wfaistv = *on
020700070712      * istat assente
020800070712     c                   else
020900081127     c                   eval      wfaistv = *off
021000070712     c                   endif
021100081125     c                   endif
021200081127      * percentuali addizionali di gestione
021300081127     c                   eval      wfapagv = tampag
021400081127     c                   eval      wfappav = tamppa
021500081125
021600081127     c                   write     wfaut000
021700070712
021800070712     c                   endsr
021900070712
022000081126
022100081126      *------------------------------------------------------------------------*
022200081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
022300081126      *------------------------------------------------------------------------*
022400081126     c     Sr_dettaglio  begsr
022500081126
022600081126     C/EXEC SQL
022700081126     C+ declare C1 cursor for
022800081127     C+ SELECT * FROM EDPAUMENTI/WFaut00F
022900081127     C+ WHERE WFATCL = 'C'
023000081127     C+ ORDER BY wfaclu , wfaclf
023100081128     c+ for read only
023200081126     C/END-EXEC
023300081126
023400081126     C/EXEC SQL
023500081126     C+ open C1
023600081126     C/END-EXEC
023700081126     C
023800081126     C/EXEC SQL
023900081127     C+ Fetch C1 into :wfautds
024000081126     C/END-EXEC
024100081126     C*
024200081126
024300081126     C*
024400081126     C                   dow       sqlcod = *zeros
024500081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
024600081127     c                   If        savclu <> ds_WFACLU
024700081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
024800081127     c                   If        savclu <> *zeros
024900081126     c                   exsr      Sr_unific
025000081126     c                   endif
025100081127     c                   movel     ds_wfaclu     savclu
025200081127      * imposto il campo di lavoro per verificare se il cliente è bloccato
025300081126     c                   eval      W_wfafbl = 'S'
025400081126      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
025500081126     c                   eval      W_istat = ' '
025600081127     c                   endif
025700081127      * per ogni tariffa memorizzo le caratteristiche
025800081126      * per ogni tariffa cliente verifico istat
025900081126     c                   select
026000081126      * tariffa con   istat
026100081127     c                   when      DS_wfaistv = '1'
026200081126
026300081126      * imposto il flag dell'unificante
026400081126     c                   if        w_istat = ' ' or w_istat = 'S'
026500081126     c                   eval      w_istat = 'S'                                si istat
026600081126     c                   else
026700081126     c                   eval      W_istat = 'M'                                misto
026800081126     c                   endif
026900081126
027000081126      * tariffa senza istat
027100081128     c                   when      DS_wfaistv = '0'
027200081126      * imposto il flag dell'unificante
027300081126     c                   if        w_istat = ' ' or w_istat = 'N'
027400081126     c                   eval      w_istat = 'N'                                no istat
027500081126     c                   else
027600081126     c                   eval      W_istat = 'M'                                misto
027700081126     c                   endif
027800081126      *
027900081126     c                   endsl
028000081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
028100081127     c                   if        DS_wfafblc = ' '
028200081127     c                   clear                   w_wfafbl
028300081127     c                   endif
028400081126     C*
028500081126     C*
028600081126     C/EXEC SQL
028700081127     C+ Fetch C1 into :wfautds
028800081126     C/END-EXEC
028900081126     C*
029000081126     C                   enddo
029100081128      * se è fine file scrivo l'ultimo unificante
029200081128     c                   If        sqlcod = 100 and savclu > *zero
029300081128     c                   exsr      sr_unific
029400081128     c                   endif
029500081126     C*
029600081126     C/EXEC SQL
029700081126     C+ close C1
029800081126     C/END-EXEC
029900081126     C
030000081126
030100081126     c                   endsr
030200081126      *---------------------------------------------------------------------------------------------
030300081126      * SR_UNIFIC    Elaborazione del cliente unificante
030400081126      *---------------------------------------------------------------------------------------------
030500081126     C     SR_UNIFIC     BEGSR
030600081126      *
030700081128     c                   clear                   wfaut000
030800081126      * aggancio anagrafiche
030900081126     C                   clear                   TIBS69ds
031000081128     C                   clear                   ds_CNACO
031100081128     C                   clear                   ds_CNIND
031200081128     C                   clear                   ds_CNCLP
031300081128     C                   clear                   ds_FNCLS
031400081127     C                   eval      I69kac =  savclu
031500081127     C                   eval      I69kin =  savclu
031600081127     C                   eval      I69kcp =  savclu
031700081126     C                   call      'TIBS69R'
031800081126     C                   parm                    TIBS69ds
031900081128     C                   parm                    ds_CNACO
032000081128     C                   parm                    ds_CNIND
032100081128     C                   parm                    ds_CNCLP
032200081128     C                   parm                    ds_FNCLS
032300081126      * valorizzo i campi del file
032400081126     c                   eval      Wfatcl = 'U'
032500081127     c                   eval      Wfaclu = savclu
032600081127     c                   eval      Wfacun = clpclv
032700081127     c                   eval      Wfaclv = clpclv
032800081127
032900081127     c                   clear                   Wfaclf
033000081126     c                   eval      Wfarag = Acorag
033100081127      * cerco codice commerciale
033200081127      * verifico il codice commerciale unificante
033300081127     c                   eval      kcod = '01'
033400081127     c                   movel     clpage        kkey
033500081127     c     ktabel        chain     tabel00f
033600081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
033700081127     c                   eval      wfaage  = clpage
033800081127     c                   eval      wfaagd  = *all'*'
033900081127     c                   else
034000081127     c                   eval      ds01    = tbluni
034100081127     c                   eval      wfaage  = §01rgf
034200081127     c                   movel     wfaage        kkey
034300081127     c     ktabel        chain     tabel00f
034400081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
034500081127     c                   eval      wfaagd  = *all'*'
034600081127     c                   else
034700081127     c                   eval      ds01    = tbluni
034800081127     c                   eval      wfaagd  = §01age
034900081127     c                   endif
035000081127     c                   endif
035100081127
035200081127      * albero commerciale unificante
035300081127     c                   movel     wfaage        w0030
035400081127     c     w0030         chain     azorg01l
035500081127
035600081127      * distretto
035700081127     c                   eval      wfadiv = orgfl3
035800081127     c                   eval      kcod = '17'
035900081127     c                   eval      kkey = wfadiv
036000081127     c     ktabel        chain     tabel00f
036100081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
036200081127     c                   eval      wfadid  = *all'*'
036300081127     c                   else
036400081127     c                   eval      wfadid  = tbluni
036500081127     c                   endif
036600081128      * se divisione 7 cancello tutti i clienti legati all'unificante
036700081128     c                   If        wfadiv = '7'
036800081128     C/EXEC SQL
036900081128     C+ delete from EDPAUMENTI/WFaut00F as a
037000081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
037100081128     C/END-EXEC
037200081128     c                   leavesr
037300081128     c                   endif
037400081127      * area
037500081127     c                   eval      wfaare = orgcar
037600081127     c                   eval      kcod = '05'
037700081127     c                   movel     wfaare        kkey
037800081127     c     ktabel        chain     tabel00f
037900081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
038000081127     c                   eval      wfaard  = *all'*'
038100081127     c                   else
038200081127     c                   eval      wfaard  = tbluni
038300081127     c                   endif
038400081127      * filiale
038500081127     c                   eval      wfafil = orgfil
038600081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
038700081127     c                   eval      wfafid  = *all'*'
038800081127     c                   else
038900081127     c                   eval      wfafid  = orgdes
039000081127     c                   endif
039100081128     c                   eval      Wfainf = orgind
039200081127     c                   eval      Wfacpf = orgcpf
039300081127     c                   eval      Wfaloc = orgloc
039400081127     c                   eval      Wfapro = orgpro
039500081127     c                   eval      Wfatel = orgtel
039600081127     c                   eval      Wfafax = orgfax
039700081127     c                   eval      Wfaind = Indvia
039800081127     c                   eval      Wfacit = Indcit
039900081127     c                   eval      Wfacap = Indcae
040000081127     c                   eval      Wfasta = Indsta
040100081127     c                   eval      Wfaprv = Indprv
040200081127      * decodifca della nazione
040300081127     c                   If        wfasta <> *blanks
040400081128     c                   eval      kcod = '15'
040500081127     c                   movel(p)  wfasta        kkey
040600081128     c     Ktabel        chain     tabel00f
040700081127     c                   If        %found(tabel00f)
040800081127     c                   eval      ds15 = tbluni
040900081127     c                   eval      wfanaz = §15des
041000081127     c                   endif
041100081128     c                   else
041200081128     c                   clear                   wfanaz
041300081127     c                   endif
041400081127      * valorizzo se il cliente  è bloccato
041500081128     c                   eval      wfafblc = w_wfafbl
041600081127      * flag ISTAT
041700081127     c                   eval      wfafisc = w_istat
041800081202      * delta del cliente
041900081202     C/EXEC SQL
042000081203     C+ select waprr_10ac ,waprp_10ac  into
042100081203     C+ :waprr_10ac ,:waprp_10ac from WADELPCT
042200081203     C+ where wapcli =  :savclu  and wapaad = 2008
042300081202     C/END-EXEC
042400081202      * se non trovo il record metto il delta a zero
042500081202     c                   if        sqlcod <> 0
042600081202     c                   clear                   wfadel
042700081202
042800081202     c                   else
042900081202
043000081203     c                   if        waprr_10ac > 0
043100081203     c                   eval      wfadel =((waprr_10ac - waprp_10ac)*100)/
043200081203     c                                      waprr_10ac
043300081203     c                   else
043400081203     c                   clear                   wfadel
043500081203     c                   endif
043600081203
043700081202     c                   endif
043800081127      * pulisco i flag aumento e lettera
043900081128     c                   clear                   wfafltu
044000081128     c                   clear                   wfafauc
044100081127      * verifico se devo fare l'aumento oppure no
044200081126     c                   SELECT
044300081126      * distretto  6/7 scartato
044400081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
044500081127     c                   eval      Wfafblc= 'S'
044600081127      * categoria 'C' controllo ISTAT
044700081127     c                   When      Wfacun = 'C'
044800081126      * no bloccato  lettera di aumento   se almeno uno dei figli ha le tariffe e se non ha istat
044900081127     c                   If        wfafblc  = ' '
045000081127      * ISTAT misto
045100081127     c                   If        wfafisc = 'M'
045200081127     c                   eval      wfafauc = 'I'
045300081127     c                   endif
045400081127
045500081127      * ISTAT NO
045600081127     c                   If        wfafisc = 'N'
045700081127     c                   eval      wfafauc = 'A'
045800081127     c                   eval      wfafltu = '1'
045900081127     c                   endif
046000081126      *
046100081127     c                   endif
046200081126      *
046300081126     c                   endsl
046400081126      * scrivo il record
046500081126     c                   write     wfaut000
046600081128
046700081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
046800081128     C/EXEC SQL
046900081128     C+ update EDPAUMENTI/WFaut00F as a
047000081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
047100081128     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun
047200081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
047300081128     C/END-EXEC
047400081128     c
047500081128
047600081126      *
047700081126     c                   endsr
047800081126      *
047900070712      *--------------------------------------------------------------*
048000070712      * ROUTINE INIZIALE
048100070712      *--------------------------------------------------------------*
048200070712     c     *inzsr        begsr
048300070712
048400070712     c     *entry        plist
048500070712     c                   parm                    kpjba
048600070712
048700070712     c     *dtaara       define    §azute        azuteds
048800070712     c     *dtaara       define    §datiute      ddatiute
048900070712
049000070712     c                   in(e)     *dtaara
049100070712     c                   if        %error or rsut = *blanks
049200070712     c                   clear                   tibs34ds
049300070712     c                   call      'TIBS34R'
049400070712     c                   parm                    tibs34ds
049500070712     c                   in        *dtaara
049600070712     c                   endif
049700070712
049800070712      * tabella scatti istat
049900070712     c                   clear                   dsqi1
050000070712     c                   clear                   dsqi2
050100070712     c                   eval      kcod = 'QI'
050200070712     c                   eval      kkey = '1'
050300070712     c     ktabel        chain     tabel00f
050400070712     c                   if        %found(tabel00f) and tblflg = *blanks
050500070712     c                   eval      dsqi1 = tbluni
050600070712     c                   eval      kkey = '2'
050700070712     c     ktabel        chain     tabel00f
050800070712     c                   if        %found(tabel00f) and tblflg = *blanks
050900070712     c                   eval      dsqi2 = tbluni
051000070712     c                   z-add     §qid1t        dia(1)
051100070712     c                   z-add     §qid2t        dia(2)
051200070712     c                   z-add     §qid3t        dia(3)
051300070712     c                   z-add     §qid4t        dia(4)
051400070712     c                   z-add     §qid5t        dia(5)
051500070712     c                   z-add     §qid6t        dia(6)
051600070712     c                   z-add     §qid7t        dia(7)
051700070712     c                   z-add     §qid8t        dia(8)
051800070712     c                   z-add     §qid9t        dia(9)
051900070712     c                   z-add     §qid0t        dia(10)
052000070712     c                   z-add     §qid11        dia(11)
052100070712     c                   z-add     §qid12        dia(12)
052200070712     c                   endif
052300070712     c                   endif
052400070712
052500081125      *
052600070712
052700070712      * pulisco il file
052800070712     c                   clear                   comman
052900070712     c                   movel(p)  cmd(1)        comman
053000070712     c                   call      'QCMDEXC'
053100070712     c                   parm                    comman
053200070712     c                   parm                    lenght
053300070712
053400081128     c                   open      wfaut00f
053500070712
053600070712      * reperisco data e ora del lancio
053700070712     c                   time                    w0140
053800070712     c                   move      w0140         wdata
053900070712     c                   movel     w0140         wora
054000070712
054100070712     c     kazorg        klist
054200070712     c                   kfld                    dutdis
054300070712     c                   kfld                    kcar
054400070712
054500070712     c     kcli          klist
054600070712     c                   kfld                    kkut
054700070712     c                   kfld                    kkcc
054800070712     c                   kfld                    tamksc
054900070712
055000070712     c     ktabel        klist
055100070712     c                   kfld                    codut
055200070712     c                   kfld                    kcod
055300070712     c                   kfld                    kkey
055400070712
055500070712     c                   endsr
055600070712      *--------------------------------------------------------------*
055700070712**   cmd - comando
055800081128CLRPFM FILE(EDPAUMENTI/WFAUT00F)
