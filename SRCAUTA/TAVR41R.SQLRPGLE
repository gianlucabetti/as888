000100130830     /*PRM  dbgview(*source)
000200130830     /*CMD  ovrdbf file(WFAUT00F) tofile(EDPAUMENTI/WFAUT00F) +
000300130830     /*CMD         ovrscope(*calllvl)
000400130830     /*END  dltovr file(WFAUT00F) lvl(*)
000500130830     /*END
000600070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700070712      *------------------------------------------------------------------------*
000800070712      *
000900081125      *  Creazione archivio aumento tariffe
001000070712      *
001100070712      *------------------------------------------------------------------------*
001200070712
001300070712     ftntam04l  if   e           k disk
001400070712     fazorg01l  if   e           k disk
001500070712     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001600130830     fAZCMM01L  if   e           k disk
001700070712     ftabel00f  if   e           k disk
001800081128     fwfaut00f  o    e             disk    usropn extfile('EDPAUMENTI/WFAUT00F')
001900070712
002000070712      *------------------------------------------------------------------------*
002100070712      *  RIEPILOGO INDICATORI
002200070712      *------------------------------------------------------------------------*
002300070712      * 02 - ho scritto almeno 1 rcd
002400070712
002500070712      *------------------------------------------------------------------------*
002600070712      *   V A R I A B I L I
002700070712      *------------------------------------------------------------------------*
002800070712     d comman          s             80
002900070712     d codut           s              1  0 inz(1)
003000070712     d kcar            s                   like(orgcar)
003100070712     d kcod            s                   like(tblcod)
003200070712     d kksc            s                   like(tamksc)
003300070712     d kkey            s                   like(tblkey)
003400070712     d lenght          s             15  5 inz(80)
003500070712     d kkcc            s                   like(acokcc) inz(151)
003600070712     d kkut            s                   like(acokut) inz(1)
003700070712     d wctr            s                   like(tamctr)
003800070712     d wdata           s              8  0
003900070712     d wksc            s                   like(tamksc)
004000070712     d wora            s              6  0
004100070712     d w0030           s              3  0
004200070712     d w0140           s             14  0
004300081127     d savclu          s                   like(wfaclu)
004400081128     d W_wfafbl        S                   like(WFAfblc)
004500081128     d W_istat         S              1
004600070712
004700070712      *------------------------------------------------------------------------*
004800070712      *   S C H I E R E
004900070712      *------------------------------------------------------------------------*
005000070712     d cmd             s             80    dim(1) ctdata perrcd(1)
005100070712
005200070712      *------------------------------------------------------------------------*
005300070712      *   D S   I N T E R N E / E S T E R N E
005400070712      *------------------------------------------------------------------------*
005500070712
005600070712     d azuteds       e ds                  extname(azute00f)
005700070712     d ddatiute      e ds
005800070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
005900070712     d ds_cnind      e ds                  inz extname(cnind00f)
006000070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
006100070712     d ds_fncls      e ds                  inz extname(fncls00f)
006200070712     d kpjba         e ds
006300130830     d*// dsta01        e ds
006400130830     d ds15          e ds
006500070712     d tibs10ds      e ds
006600070712     d tibs34ds      e ds
006700070712     d tibs69ds      e ds
006800081127     d wfautds       e ds                  extname(wfaut00f)
006900081127     d                                     prefix(DS_)
007000070712
007100070712      *------------------------------------------------------------------------*
007200081126      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300081126     c
007400081126     c/EXEC SQL
007500081126     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600081126     c/END-EXEC
007700081126     c*
007800070712
007900070712     c                   clear                   wksc
008000070712     c                   eval      wctr = 999
008100070712     c                   move      *all'0'       kksc
008200070712
008300070712     c     kksc          setll     tntam04l
008400070712    1c                   do        *hival
008500070712     c                   read      tntam04l
008600070712      * fine file
008700070712     c                   if        %eof(tntam04l)
008800070712     c                   leave
008900070712     c                   endif
009000081128      * se tariffe di cartello leggo il successivo
009100081128     c                   movel     tamksc        w0030
009200081128     c                   if        w0030 = 888
009300081128     c                   iter
009400081128     c                   endif
009500081125      * cambio tariffa
009600070712    2c                   if        tamctr <> wctr or tamksc <> wksc
009700130830     c*//                movel     tamflo        dsta01
009800070712
009900070712      * escludo le annullate
010000070712     c                   if        tamatb <> *blanks
010100070712     c                   iter
010200070712     c                   endif
010300070712      * scrivo il file
010400070712     c                   exsr      scrivi
010500070712
010600070712     c                   eval      wksc = tamksc
010700070712     c                   eval      wctr = tamctr
010800070712
010900131209      * se la tariffa che sto leggendo andrà in vigore dopo il 31/12/13 la elaboro
011000070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
011100131209     c                   if        tamddt > 20131231
011200070712     c                   eval      wctr = 999
011300070712     c                   endif
011400070712    2c                   endif
011500070712
011600070712    1c                   enddo
011700081126     c                   exsr      SR_Dettaglio
011800081126
011900070712     c                   eval      *inlr = *on
012000081126
012100070712      *------------------------------------------------------------------------*
012200081127      * SCRIVO IL FILE PER GLI AUMENTI
012300070712      *------------------------------------------------------------------------*
012400070712     c     scrivi        begsr
012500070712
012600081127     c                   clear                   wfaut000
012700070712
012800081127      * tipo record cliente/tariffa
012900081127     c                   eval      wfatcl = 'C'
013000081127      * cerco il codice cliente unificante
013100081127     c                   clear                   tibs10ds
013200081127     c                   eval      d10tle = 'ST'
013300081127     c                   eval      d10paf = 'P'
013400081127     c                   z-add     tamksc        d10cod
013500131209     c                   z-add     20131130      d10drf
013600081202     c                   call      'TIBS10R'
013700081127     c                   parm                    tibs10ds
013800081127     c                   parm      'A'           partpexe          1
013900081127      * se ci sono errori valorizzo come unificante il cliente
014000081127     c                   if        d10err <> ' '
014100081128     c                   eval      wfaclu = tamksc
014200081127      * se tutto ok valorizzo l'unificante trovato
014300081127     c                   else
014400081127     c                   eval      wfaclu = d10cop
014500081127     c                   endif
014600081127      * cliente
014700081127     c                   eval      wfaclf = tamksc
014800081127      * cerco i dati anagrafici
014900081127     c                   clear                   tibs69ds
015000081127     c                   move      tamksc        i69kac
015100081127     c                   move      tamksc        i69kcp
015200081127     c                   move      tamksc        i69kin
015300081127     c                   call      'TIBS69R'
015400081127     c                   parm                    tibs69ds
015500081127     c                   parm                    ds_cnaco
015600081127     c                   parm                    ds_cnind
015700081127     c                   parm                    ds_cnclp
015800081127     c                   parm                    ds_fncls
015900081127
016000081127     c                   IF        acoabl <> ' '
016100081127     c                   eval      Wfafblc = 'S'
016200081127     c                   endif
016300081127      * ragione sociale e indirizzo
016400081127     c                   eval      wfarag = acorag
016500081127     c                   eval      Wfaind = Indvia
016600081127     c                   eval      Wfacit = Indcit
016700081127     c                   eval      Wfacap = Indcae
016800081127     c                   eval      Wfasta = Indsta
016900081127     c                   eval      Wfaprv = Indprv
017000081127      * decodifca della nazione
017100081127     c                   If        wfasta <> *blanks
017200081128     c                   eval      kcod = '15'
017300081127     c                   movel(p)  wfasta        kkey
017400081128     c     Ktabel        chain     tabel00f
017500081127     c                   If        %found(tabel00f)
017600081127     c                   eval      ds15 = tbluni
017700081127     c                   eval      wfanaz = §15des
017800081127     c                   endif
017900081128     c                   else
018000081128     c                   clear                   wfanaz
018100081127     c                   endif
018200070712      * importanza cliente
018300081127     c                   eval      wfaclv = clpclv
018400130830      * comm.le del cliente
018500081209     c                   eval      wfaagec= clpage
018600130830      * decodifico comm.le del cliente
018700130830     c     CLPage        chain     AZCMM000
018800130830     c                   if        not %found(AZCMM01L)
018900081209     c                   eval      wfaagdc = *all'*'
019000081209     c                   else
019100130830     c                   eval      wfaagdc = CMMdes
019200081209     c                   endif
019300070712      * tariffa
019400081127     c                   eval      wfactrv = tamctr
019500081127     c                   eval      wfaprgv = tamprg
019600081127     c                   eval      wfaddtv = tamddt
019700081127     c                   eval      wfadstv = tamdst
019800070712      * istat presente
019900081125      * escludo le bloccate
020000081125     c                   if        tambap = 'B'
020100081127     c                   eval      wfabap = tambap
020200081125     c                   else
020300101130     c                   if        tamrct <> 0
020400081209     c                   eval      wfaistv = 'S'
020500070712      * istat assente
020600070712     c                   else
020700081209     c                   eval      wfaistv = 'N'
020800070712     c                   endif
020900081125     c                   endif
021000081127      * percentuali addizionali di gestione
021100081127     c                   eval      wfapagv = tampag
021200081127     c                   eval      wfappav = tamppa
021300081125
021400081127     c                   write     wfaut000
021500070712
021600070712     c                   endsr
021700070712
021800081126
021900081126      *------------------------------------------------------------------------*
022000081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
022100081126      *------------------------------------------------------------------------*
022200081126     c     Sr_dettaglio  begsr
022300081126
022400081126     C/EXEC SQL
022500081126     C+ declare C1 cursor for
022600081127     C+ SELECT * FROM EDPAUMENTI/WFaut00F
022700081127     C+ WHERE WFATCL = 'C'
022800081127     C+ ORDER BY wfaclu , wfaclf
022900081128     c+ for read only
023000081126     C/END-EXEC
023100081126
023200081126     C/EXEC SQL
023300081126     C+ open C1
023400081126     C/END-EXEC
023500081126     C
023600081126     C/EXEC SQL
023700081127     C+ Fetch C1 into :wfautds
023800081126     C/END-EXEC
023900081126     C*
024000081126
024100081126     C*
024200081126     C                   dow       sqlcod = *zeros
024300081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
024400081127     c                   If        savclu <> ds_WFACLU
024500081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
024600081127     c                   If        savclu <> *zeros
024700081126     c                   exsr      Sr_unific
024800081126     c                   endif
024900081127     c                   movel     ds_wfaclu     savclu
025000081127      * imposto il campo di lavoro per verificare se il cliente è bloccato
025100081126     c                   eval      W_wfafbl = 'S'
025200081126      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
025300081126     c                   eval      W_istat = ' '
025400081127     c                   endif
025500081127      * per ogni tariffa memorizzo le caratteristiche
025600081126      * per ogni tariffa cliente verifico istat
025700081126     c                   select
025800081126      * tariffa con   istat
025900081209     c                   when      DS_wfaistv = 'S'
026000081126
026100081126      * imposto il flag dell'unificante
026200081126     c                   if        w_istat = ' ' or w_istat = 'S'
026300081126     c                   eval      w_istat = 'S'                                si istat
026400081126     c                   else
026500081126     c                   eval      W_istat = 'M'                                misto
026600081126     c                   endif
026700081126
026800081126      * tariffa senza istat
026900081209     c                   when      DS_wfaistv = 'N'
027000081126      * imposto il flag dell'unificante
027100081126     c                   if        w_istat = ' ' or w_istat = 'N'
027200081126     c                   eval      w_istat = 'N'                                no istat
027300081126     c                   else
027400081126     c                   eval      W_istat = 'M'                                misto
027500081126     c                   endif
027600081126      *
027700081126     c                   endsl
027800081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
027900081127     c                   if        DS_wfafblc = ' '
028000081127     c                   clear                   w_wfafbl
028100081127     c                   endif
028200081126     C*
028300081126     C*
028400081126     C/EXEC SQL
028500081127     C+ Fetch C1 into :wfautds
028600081126     C/END-EXEC
028700081126     C*
028800081126     C                   enddo
028900081128      * se è fine file scrivo l'ultimo unificante
029000081128     c                   If        sqlcod = 100 and savclu > *zero
029100081128     c                   exsr      sr_unific
029200081128     c                   endif
029300081126     C*
029400081126     C/EXEC SQL
029500081126     C+ close C1
029600081126     C/END-EXEC
029700081126     C
029800081126
029900081126     c                   endsr
030000081126      *---------------------------------------------------------------------------------------------
030100081126      * SR_UNIFIC    Elaborazione del cliente unificante
030200081126      *---------------------------------------------------------------------------------------------
030300081126     C     SR_UNIFIC     BEGSR
030400081126      *
030500081128     c                   clear                   wfaut000
030600081126      * aggancio anagrafiche
030700081126     C                   clear                   TIBS69ds
030800081128     C                   clear                   ds_CNACO
030900081128     C                   clear                   ds_CNIND
031000081128     C                   clear                   ds_CNCLP
031100081128     C                   clear                   ds_FNCLS
031200081127     C                   eval      I69kac =  savclu
031300081127     C                   eval      I69kin =  savclu
031400081127     C                   eval      I69kcp =  savclu
031500081126     C                   call      'TIBS69R'
031600081126     C                   parm                    TIBS69ds
031700081128     C                   parm                    ds_CNACO
031800081128     C                   parm                    ds_CNIND
031900081128     C                   parm                    ds_CNCLP
032000081128     C                   parm                    ds_FNCLS
032100081126      * valorizzo i campi del file
032200081126     c                   eval      Wfatcl = 'U'
032300081127     c                   eval      Wfaclu = savclu
032400081127     c                   eval      Wfacun = clpclv
032500081127     c                   eval      Wfaclv = clpclv
032600081127
032700081127     c                   clear                   Wfaclf
032800081126     c                   eval      Wfarag = Acorag
032900081209     c                   eval      Wfarsu = Acorag
033000081127      * cerco codice commerciale
033100081127      * verifico il codice commerciale unificante
033200130830     c     CLPage        chain     AZCMM000
033300130830     c                   if        not %found(AZCMM01L)
033400081127     c                   eval      wfaage  = clpage
033500081127     c                   eval      wfaagd  = *all'*'
033600081127     c                   else
033700130830     c                   eval      wfaage  = CMMuni
033800130830     c     WFAage        chain     AZCMM000
033900130830     c                   if        not %found(AZCMM01L)
034000081127     c                   eval      wfaagd  = *all'*'
034100081127     c                   else
034200130830     c                   eval      wfaagd  = CMMdes
034300081127     c                   endif
034400081127     c                   endif
034500081127
034600081127      * albero commerciale unificante
034700081127     c                   movel     wfaage        w0030
034800081127     c     w0030         chain     azorg01l
034900081127
035000081127      * distretto
035100081127     c                   eval      wfadiv = orgfl3
035200081127     c                   eval      kcod = '17'
035300081127     c                   eval      kkey = wfadiv
035400081127     c     ktabel        chain     tabel00f
035500081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
035600081127     c                   eval      wfadid  = *all'*'
035700081127     c                   else
035800081127     c                   eval      wfadid  = tbluni
035900081127     c                   endif
036000081128      * se divisione 7 cancello tutti i clienti legati all'unificante
036100081128     c                   If        wfadiv = '7'
036200081128     C/EXEC SQL
036300081128     C+ delete from EDPAUMENTI/WFaut00F as a
036400081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
036500081128     C/END-EXEC
036600081128     c                   leavesr
036700081128     c                   endif
036800081127      * area
036900081127     c                   eval      wfaare = orgcar
037000081127     c                   eval      kcod = '05'
037100081127     c                   movel     wfaare        kkey
037200081127     c     ktabel        chain     tabel00f
037300081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
037400081127     c                   eval      wfaard  = *all'*'
037500081127     c                   else
037600081127     c                   eval      wfaard  = tbluni
037700081127     c                   endif
037800081127      * filiale
037900081127     c                   eval      wfafil = orgfil
038000081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
038100081127     c                   eval      wfafid  = *all'*'
038200081127     c                   else
038300081127     c                   eval      wfafid  = orgdes
038400081127     c                   endif
038500081128     c                   eval      Wfainf = orgind
038600081127     c                   eval      Wfacpf = orgcpf
038700081127     c                   eval      Wfaloc = orgloc
038800081127     c                   eval      Wfapro = orgpro
038900081127     c                   eval      Wfatel = orgtel
039000081127     c                   eval      Wfafax = orgfax
039100081127     c                   eval      Wfaind = Indvia
039200081127     c                   eval      Wfacit = Indcit
039300081127     c                   eval      Wfacap = Indcae
039400081127     c                   eval      Wfasta = Indsta
039500081127     c                   eval      Wfaprv = Indprv
039600081127      * decodifca della nazione
039700081127     c                   If        wfasta <> *blanks
039800081128     c                   eval      kcod = '15'
039900081127     c                   movel(p)  wfasta        kkey
040000081128     c     Ktabel        chain     tabel00f
040100081127     c                   If        %found(tabel00f)
040200081127     c                   eval      ds15 = tbluni
040300081127     c                   eval      wfanaz = §15des
040400081127     c                   endif
040500081128     c                   else
040600081128     c                   clear                   wfanaz
040700081127     c                   endif
040800081127      * valorizzo se il cliente  è bloccato
040900081128     c                   eval      wfafblc = w_wfafbl
041000081127      * flag ISTAT
041100081127     c                   eval      wfafisc = w_istat
041200081204      * delta del cliente  -- RIMOSSO - Nicola 4-12-2008 --
041300081204     c                   clear                   wfadel
041400091126     C*   /EXEC SQL
041500091126     C*   +  select waprr_10ac ,waprp_10ac  into
041600091126     C*   +  :waprr_10ac ,:waprp_10ac from WADELPCT
041700091126     C*   +  where wapcli =  :savclu  and wapaad = 2008
041800091126     C*   /END-EXEC
041900081202      * se non trovo il record metto il delta a zero
042000081204     c*                  if        sqlcod <> 0
042100081204     c*                  clear                   wfadel
042200081202
042300081204     c*                  else
042400081202
042500081204     c*                  if        waprr_10ac > 0
042600081204     c*                  eval      wfadel =((waprr_10ac - waprp_10ac)*100)/
042700081204     c*                                     waprr_10ac
042800081204     c*                  else
042900081204     c*                  clear                   wfadel
043000081204     c*                  endif
043100081203
043200081204     c*                  endif
043300081127      * pulisco i flag aumento e lettera
043400081128     c                   clear                   wfafltu
043500081128     c                   clear                   wfafauc
043600081127      * verifico se devo fare l'aumento oppure no
043700081126     c                   SELECT
043800081126      * distretto  6/7 scartato
043900081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
044000081127     c                   eval      Wfafblc= 'S'
044100081127      * categoria 'C' controllo ISTAT
044200081127     c                   When      Wfacun = 'C'
044300081126      * no bloccato  lettera di aumento   se almeno uno dei figli ha le tariffe e se non ha istat
044400081127     c                   If        wfafblc  = ' '
044500121129      * ISTAT misto   (anno 2011 x 2012 non vogliono istat  automatico x clienti C)anche 2013
044600111212     c                   If        wfafisc = 'M'
044700111212     c******             eval      wfafauc = 'I'
044800081127     c                   endif
044900081127
045000091201      * ISTAT NO   NON SI FA NULLA
045100091201     c*                  If        wfafisc = 'N'
045200091201     c*                  eval      wfafauc = 'A'
045300091201     c*                  eval      wfafltu = '1'
045400091201     c*                  endif
045500081126      *
045600081127     c                   endif
045700081126      *
045800081126     c                   endsl
045900081126      * scrivo il record
046000081126     c                   write     wfaut000
046100081128
046200081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
046300081209      * i dati relativi all'area di appartenenza dell'agente unificante , la ragione sociale
046400081209      * dell'unificante
046500081128     C/EXEC SQL
046600081128     C+ update EDPAUMENTI/WFaut00F as a
046700081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
046800081209     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun,
046900081209     c+ a.wfarsu  = :wfarsu , a.wfadiv = :wfadiv,
047000081209     c+ a.wfadid  = :wfadid , a.wfaare = :wfaare,
047100081209     c+ a.wfaard  = :wfaard , a.wfafil = :wfafil,
047200081209     c+ a.wfafid  = :wfafid , a.wfaage = :wfaage,
047300081209     c+ a.wfaagd  = :wfaagd
047400081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
047500081128     C/END-EXEC
047600081128     c
047700081128
047800081126     c                   endsr
047900081126      *
048000070712      *--------------------------------------------------------------*
048100070712      * ROUTINE INIZIALE
048200070712      *--------------------------------------------------------------*
048300070712     c     *inzsr        begsr
048400070712
048500070712     c     *entry        plist
048600070712     c                   parm                    kpjba
048700070712
048800070712     c     *dtaara       define    §azute        azuteds
048900070712     c     *dtaara       define    §datiute      ddatiute
049000070712
049100070712     c                   in(e)     *dtaara
049200070712     c                   if        %error or rsut = *blanks
049300070712     c                   clear                   tibs34ds
049400070712     c                   call      'TIBS34R'
049500070712     c                   parm                    tibs34ds
049600070712     c                   in        *dtaara
049700070712     c                   endif
049800070712
049900070712      * pulisco il file
050000070712     c                   clear                   comman
050100070712     c                   movel(p)  cmd(1)        comman
050200070712     c                   call      'QCMDEXC'
050300070712     c                   parm                    comman
050400070712     c                   parm                    lenght
050500070712
050600081128     c                   open      wfaut00f
050700070712
050800070712      * reperisco data e ora del lancio
050900070712     c                   time                    w0140
051000070712     c                   move      w0140         wdata
051100070712     c                   movel     w0140         wora
051200070712
051300070712     c     kazorg        klist
051400070712     c                   kfld                    dutdis
051500070712     c                   kfld                    kcar
051600070712
051700070712     c     kcli          klist
051800070712     c                   kfld                    kkut
051900070712     c                   kfld                    kkcc
052000070712     c                   kfld                    tamksc
052100070712
052200070712     c     ktabel        klist
052300070712     c                   kfld                    codut
052400070712     c                   kfld                    kcod
052500070712     c                   kfld                    kkey
052600070712
052700070712     c                   endsr
052800070712      *--------------------------------------------------------------*
052900070712**   cmd - comando
053000081128CLRPFM FILE(EDPAUMENTI/WFAUT00F)
