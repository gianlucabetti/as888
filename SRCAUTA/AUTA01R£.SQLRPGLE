000100141210      //--------------------------------------------------------------
000200141210      //?TAVR44R - Aumento tariffe 2015
000300141210      //--------------------------------------------------------------
000400141210
000500141210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141210
000700141210      //---------------------------------------------------------------
000800141210      //?Dichiarazione file.
000900141210      //---------------------------------------------------------------
001000070712
001100141210     fTNTAM04L  if   e           k disk
001200141210     fWFAUT00F  o    e             disk    usropn extfile('EDPAUMENTI/WFAUT00F')
001300141210
001400141210      //---------------------------------------------------------------
001500141210      //?Definizione costanti.
001600141210      //---------------------------------------------------------------
001700141210
001800141210      //---------------------------------------------------------------
001900141210      //?Definizione schiere.
002000141210      //---------------------------------------------------------------
002100141210
002200141210      //---------------------------------------------------------------
002300141210      //?Definizione aree dati.
002400141210      //---------------------------------------------------------------
002500141210
002600141210      // - Parametri ricevuti
002700141210     d KPJBA         e ds
002800141210
002900141210      //---------------------------------------------------------------
003000141210      //?Definizione variabili globali.
003100141210      //---------------------------------------------------------------
003200141210
003300141210      // - Flags booleani
003400141210     d Fine            s               n   inz(*off)
003500141210
003600141210      // - Campi di comodo
003700141210     d Oggi            s              8s 0 inz
003800141210
003900141210
004000141210      //---------------------------------------------------------------
004100141210      //?Definizione procedure usate.
004200141210      //---------------------------------------------------------------
004300141210
004400141210      //---------------------------------------------------------------
004500141210      //?Definizione Prototipi.
004600141210      //---------------------------------------------------------------
004700141210      /copy gaitrasrc/srcprotopr,TIBS34R
004800141210
004900141210      //---------------------------------------------------------------
005000141210      //?Definizione key-list.
005100141210      //---------------------------------------------------------------
005200141210
005300141210      //---------------------------------------------------------------
005400141210      //?M A I N - L I N E
005500141210      //---------------------------------------------------------------
005600141210
005700141210     c     *Entry        plist
005800141210     c                   parm                    kpjba
005900141210
006000141210      /free
006100141210
006200141210       //?Operazioni iniziali
006300141210       exsr RoutInz;
006400141210
006500141210       //?Operazioni finali
006600141210       exsr RoutEnd;
006700141210
006800141210       //--------------------------------------------------------------
006900141210       //?Operazioni iniziali.
007000141210       //--------------------------------------------------------------
007100141210       BEGSR RoutInz;
007200141210
007300141210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
007400141210
007500141210
007600141210      // - Dati utente
007700141210     d §AzUte        e ds                  extname(AZUTE00F)
007800141210     d                                     dtaara
007900141210     d §DatiUte      e ds                  extname(dDatiUte)
008000141210     d                                     dtaara
008100141210
008200141210      //---------------------------------------------------------------
008300141210      //?Definizione strutture dati.
008400141210      //---------------------------------------------------------------
008500070712
008600070712      *------------------------------------------------------------------------*
008700070712      *  RIEPILOGO INDICATORI
008800070712      *------------------------------------------------------------------------*
008900070712      * 02 - ho scritto almeno 1 rcd
009000070712
009100070712      *------------------------------------------------------------------------*
009200070712      *   V A R I A B I L I
009300070712      *------------------------------------------------------------------------*
009400070712     d comman          s             80
009500070712     d codut           s              1  0 inz(1)
009600070712     d kcar            s                   like(orgcar)
009700070712     d kcod            s                   like(tblcod)
009800070712     d kksc            s                   like(tamksc)
009900070712     d kkey            s                   like(tblkey)
010000070712     d lenght          s             15  5 inz(80)
010100070712     d kkcc            s                   like(acokcc) inz(151)
010200070712     d kkut            s                   like(acokut) inz(1)
010300070712     d wctr            s                   like(tamctr)
010400070712     d wdata           s              8  0
010500070712     d wksc            s                   like(tamksc)
010600070712     d wora            s              6  0
010700070712     d w0030           s              3  0
010800070712     d w0140           s             14  0
010900081127     d savclu          s                   like(wfaclu)
011000081128     d W_wfafbl        S                   like(WFAfblc)
011100081128     d W_istat         S              1
011200070712
011300070712      *------------------------------------------------------------------------*
011400070712      *   S C H I E R E
011500070712      *------------------------------------------------------------------------*
011600070712     d cmd             s             80    dim(1) ctdata perrcd(1)
011700070712
011800070712      *------------------------------------------------------------------------*
011900070712      *   D S   I N T E R N E / E S T E R N E
012000070712      *------------------------------------------------------------------------*
012100070712
012200070712     d azuteds       e ds                  extname(azute00f)
012300070712     d ddatiute      e ds
012400070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
012500070712     d ds_cnind      e ds                  inz extname(cnind00f)
012600070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
012700070712     d ds_fncls      e ds                  inz extname(fncls00f)
012800070712     d kpjba         e ds
012900130830     d*// dsta01        e ds
013000130830     d ds15          e ds
013100070712     d tibs10ds      e ds
013200070712     d tibs34ds      e ds
013300070712     d tibs69ds      e ds
013400081127     d wfautds       e ds                  extname(wfaut00f)
013500081127     d                                     prefix(DS_)
013600070712
013700070712      *------------------------------------------------------------------------*
013800081126      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013900081126     c
014000081126     c/EXEC SQL
014100081126     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014200081126     c/END-EXEC
014300081126     c*
014400070712
014500070712     c                   clear                   wksc
014600070712     c                   eval      wctr = 999
014700070712     c                   move      *all'0'       kksc
014800070712
014900070712     c     kksc          setll     tntam04l
015000070712    1c                   do        *hival
015100070712     c                   read      tntam04l
015200070712      * fine file
015300070712     c                   if        %eof(tntam04l)
015400070712     c                   leave
015500070712     c                   endif
015600081128      * se tariffe di cartello leggo il successivo
015700081128     c                   movel     tamksc        w0030
015800081128     c                   if        w0030 = 888
015900081128     c                   iter
016000081128     c                   endif
016100081125      * cambio tariffa
016200070712    2c                   if        tamctr <> wctr or tamksc <> wksc
016300070712
016400070712      * escludo le annullate
016500070712     c                   if        tamatb <> *blanks
016600070712     c                   iter
016700070712     c                   endif
016800141209      * Se la tariffa NON ha l'addizionale di gestione ANNO CORRENTE
016900141209      * leggo altra tariffa
017000141209     c                   IF        TAMppa = 0
017100141209     c                   eval      wksc = tamksc
017200141209     c                   eval      wctr = tamctr
017300141209     c                   iter
017400141209     c                   ENDIF
017500141209      * Se la tariffa ha come data decorrenza 2015
017600141209      * leggo altra tariffa
017700141209     c                   IF        TAMddt > 20141231
017800141209     c                   eval      wksc = tamksc
017900141209     c                   eval      wctr = tamctr
018000141209     c                   iter
018100141209     c                   ENDIF
018200141209
018300070712      * scrivo il file
018400070712     c                   exsr      scrivi
018500070712
018600070712     c                   eval      wksc = tamksc
018700070712     c                   eval      wctr = tamctr
018800070712
018900141209      * se la tariffa che sto leggendo andrà in vigore dopo il 31/12/14 la elaboro
019000070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
019100141209     c*******            if        tamddt > 20141231
019200141209     c*******            eval      wctr = 999
019300141209     c*******            endif
019400070712    2c                   endif
019500070712
019600070712    1c                   enddo
019700081126     c                   exsr      SR_Dettaglio
019800081126
019900070712     c                   eval      *inlr = *on
020000081126
020100070712      *------------------------------------------------------------------------*
020200081127      * SCRIVO IL FILE PER GLI AUMENTI
020300070712      *------------------------------------------------------------------------*
020400070712     c     scrivi        begsr
020500070712
020600081127     c                   clear                   wfaut000
020700070712
020800081127      * tipo record cliente/tariffa
020900081127     c                   eval      wfatcl = 'C'
021000081127      * cerco il codice cliente unificante
021100081127     c                   clear                   tibs10ds
021200081127     c                   eval      d10tle = 'ST'
021300081127     c                   eval      d10paf = 'P'
021400081127     c                   z-add     tamksc        d10cod
021500141209     c                   z-add     20141130      d10drf
021600081202     c                   call      'TIBS10R'
021700081127     c                   parm                    tibs10ds
021800081127     c                   parm      'A'           partpexe          1
021900081127      * se ci sono errori valorizzo come unificante il cliente
022000081127     c                   if        d10err <> ' '
022100081128     c                   eval      wfaclu = tamksc
022200081127      * se tutto ok valorizzo l'unificante trovato
022300081127     c                   else
022400081127     c                   eval      wfaclu = d10cop
022500081127     c                   endif
022600081127      * cliente
022700081127     c                   eval      wfaclf = tamksc
022800081127      * cerco i dati anagrafici
022900081127     c                   clear                   tibs69ds
023000081127     c                   move      tamksc        i69kac
023100081127     c                   move      tamksc        i69kcp
023200081127     c                   move      tamksc        i69kin
023300081127     c                   call      'TIBS69R'
023400081127     c                   parm                    tibs69ds
023500081127     c                   parm                    ds_cnaco
023600081127     c                   parm                    ds_cnind
023700081127     c                   parm                    ds_cnclp
023800081127     c                   parm                    ds_fncls
023900081127
024000081127     c                   IF        acoabl <> ' '
024100081127     c                   eval      Wfafblc = 'S'
024200081127     c                   endif
024300081127      * ragione sociale e indirizzo
024400081127     c                   eval      wfarag = acorag
024500081127     c                   eval      Wfaind = Indvia
024600081127     c                   eval      Wfacit = Indcit
024700081127     c                   eval      Wfacap = Indcae
024800081127     c                   eval      Wfasta = Indsta
024900081127     c                   eval      Wfaprv = Indprv
025000081127      * decodifca della nazione
025100081127     c                   If        wfasta <> *blanks
025200081128     c                   eval      kcod = '15'
025300081127     c                   movel(p)  wfasta        kkey
025400081128     c     Ktabel        chain     tabel00f
025500081127     c                   If        %found(tabel00f)
025600081127     c                   eval      ds15 = tbluni
025700081127     c                   eval      wfanaz = §15des
025800081127     c                   endif
025900081128     c                   else
026000081128     c                   clear                   wfanaz
026100081127     c                   endif
026200070712      * importanza cliente
026300081127     c                   eval      wfaclv = clpclv
026400130830      * comm.le del cliente
026500081209     c                   eval      wfaagec= clpage
026600130830      * decodifico comm.le del cliente
026700130830     c     CLPage        chain     AZCMM000
026800130830     c                   if        not %found(AZCMM01L)
026900081209     c                   eval      wfaagdc = *all'*'
027000081209     c                   else
027100130830     c                   eval      wfaagdc = CMMdes
027200081209     c                   endif
027300070712      * tariffa
027400081127     c                   eval      wfactrv = tamctr
027500081127     c                   eval      wfaprgv = tamprg
027600081127     c                   eval      wfaddtv = tamddt
027700081127     c                   eval      wfadstv = tamdst
027800070712      * istat presente
027900081125      * escludo le bloccate
028000081125     c                   if        tambap = 'B'
028100081127     c                   eval      wfabap = tambap
028200081125     c                   else
028300101130     c                   if        tamrct <> 0
028400081209     c                   eval      wfaistv = 'S'
028500070712      * istat assente
028600070712     c                   else
028700081209     c                   eval      wfaistv = 'N'
028800070712     c                   endif
028900081125     c                   endif
029000081127      * percentuali addizionali di gestione
029100081127     c                   eval      wfapagv = tampag
029200081127     c                   eval      wfappav = tamppa
029300081125
029400081127     c                   write     wfaut000
029500070712
029600070712     c                   endsr
029700070712
029800081126
029900081126      *------------------------------------------------------------------------*
030000081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
030100081126      *------------------------------------------------------------------------*
030200081126     c     Sr_dettaglio  begsr
030300081126
030400081126     C/EXEC SQL
030500081126     C+ declare C1 cursor for
030600081127     C+ SELECT * FROM EDPAUMENTI/WFaut00F
030700081127     C+ WHERE WFATCL = 'C'
030800081127     C+ ORDER BY wfaclu , wfaclf
030900081128     c+ for read only
031000081126     C/END-EXEC
031100081126
031200081126     C/EXEC SQL
031300081126     C+ open C1
031400081126     C/END-EXEC
031500081126     C
031600081126     C/EXEC SQL
031700081127     C+ Fetch C1 into :wfautds
031800081126     C/END-EXEC
031900081126     C*
032000081126
032100081126     C*
032200081126     C                   dow       sqlcod = *zeros
032300081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
032400081127     c                   If        savclu <> ds_WFACLU
032500081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
032600081127     c                   If        savclu <> *zeros
032700081126     c                   exsr      Sr_unific
032800081126     c                   endif
032900081127     c                   movel     ds_wfaclu     savclu
033000081127      * imposto il campo di lavoro per verificare se il cliente è bloccato
033100081126     c                   eval      W_wfafbl = 'S'
033200081126      * imposto il campo di lavoro per verificare se tutti i clienti figli hanno tariffe con ISTAT
033300081126     c                   eval      W_istat = ' '
033400081127     c                   endif
033500081127      * per ogni tariffa memorizzo le caratteristiche
033600081126      * per ogni tariffa cliente verifico istat
033700081126     c                   select
033800081126      * tariffa con   istat
033900081209     c                   when      DS_wfaistv = 'S'
034000081126
034100081126      * imposto il flag dell'unificante
034200081126     c                   if        w_istat = ' ' or w_istat = 'S'
034300081126     c                   eval      w_istat = 'S'                                si istat
034400081126     c                   else
034500081126     c                   eval      W_istat = 'M'                                misto
034600081126     c                   endif
034700081126
034800081126      * tariffa senza istat
034900081209     c                   when      DS_wfaistv = 'N'
035000081126      * imposto il flag dell'unificante
035100081126     c                   if        w_istat = ' ' or w_istat = 'N'
035200081126     c                   eval      w_istat = 'N'                                no istat
035300081126     c                   else
035400081126     c                   eval      W_istat = 'M'                                misto
035500081126     c                   endif
035600081126      *
035700081126     c                   endsl
035800081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
035900081127     c                   if        DS_wfafblc = ' '
036000081127     c                   clear                   w_wfafbl
036100081127     c                   endif
036200081126     C*
036300081126     C*
036400081126     C/EXEC SQL
036500081127     C+ Fetch C1 into :wfautds
036600081126     C/END-EXEC
036700081126     C*
036800081126     C                   enddo
036900081128      * se è fine file scrivo l'ultimo unificante
037000081128     c                   If        sqlcod = 100 and savclu > *zero
037100081128     c                   exsr      sr_unific
037200081128     c                   endif
037300081126     C*
037400081126     C/EXEC SQL
037500081126     C+ close C1
037600081126     C/END-EXEC
037700081126     C
037800081126
037900081126     c                   endsr
038000081126      *---------------------------------------------------------------------------------------------
038100081126      * SR_UNIFIC    Elaborazione del cliente unificante
038200081126      *---------------------------------------------------------------------------------------------
038300081126     C     SR_UNIFIC     BEGSR
038400081126      *
038500081128     c                   clear                   wfaut000
038600081126      * aggancio anagrafiche
038700081126     C                   clear                   TIBS69ds
038800081128     C                   clear                   ds_CNACO
038900081128     C                   clear                   ds_CNIND
039000081128     C                   clear                   ds_CNCLP
039100081128     C                   clear                   ds_FNCLS
039200081127     C                   eval      I69kac =  savclu
039300081127     C                   eval      I69kin =  savclu
039400081127     C                   eval      I69kcp =  savclu
039500081126     C                   call      'TIBS69R'
039600081126     C                   parm                    TIBS69ds
039700081128     C                   parm                    ds_CNACO
039800081128     C                   parm                    ds_CNIND
039900081128     C                   parm                    ds_CNCLP
040000081128     C                   parm                    ds_FNCLS
040100081126      * valorizzo i campi del file
040200081126     c                   eval      Wfatcl = 'U'
040300081127     c                   eval      Wfaclu = savclu
040400081127     c                   eval      Wfacun = clpclv
040500081127     c                   eval      Wfaclv = clpclv
040600081127
040700081127     c                   clear                   Wfaclf
040800081126     c                   eval      Wfarag = Acorag
040900081209     c                   eval      Wfarsu = Acorag
041000081127      * cerco codice commerciale
041100081127      * verifico il codice commerciale unificante
041200130830     c     CLPage        chain     AZCMM000
041300130830     c                   if        not %found(AZCMM01L)
041400081127     c                   eval      wfaage  = clpage
041500081127     c                   eval      wfaagd  = *all'*'
041600081127     c                   else
041700130830     c                   eval      wfaage  = CMMuni
041800130830     c     WFAage        chain     AZCMM000
041900130830     c                   if        not %found(AZCMM01L)
042000081127     c                   eval      wfaagd  = *all'*'
042100081127     c                   else
042200130830     c                   eval      wfaagd  = CMMdes
042300081127     c                   endif
042400081127     c                   endif
042500081127
042600081127      * albero commerciale unificante
042700081127     c                   movel     wfaage        w0030
042800081127     c     w0030         chain     azorg01l
042900081127
043000081127      * distretto
043100081127     c                   eval      wfadiv = orgfl3
043200081127     c                   eval      kcod = '17'
043300081127     c                   eval      kkey = wfadiv
043400081127     c     ktabel        chain     tabel00f
043500081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
043600081127     c                   eval      wfadid  = *all'*'
043700081127     c                   else
043800081127     c                   eval      wfadid  = tbluni
043900081127     c                   endif
044000081128      * se divisione 7 cancello tutti i clienti legati all'unificante
044100081128     c                   If        wfadiv = '7'
044200081128     C/EXEC SQL
044300081128     C+ delete from EDPAUMENTI/WFaut00F as a
044400081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
044500081128     C/END-EXEC
044600081128     c                   leavesr
044700081128     c                   endif
044800081127      * area
044900081127     c                   eval      wfaare = orgcar
045000081127     c                   eval      kcod = '05'
045100081127     c                   movel     wfaare        kkey
045200081127     c     ktabel        chain     tabel00f
045300081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
045400081127     c                   eval      wfaard  = *all'*'
045500081127     c                   else
045600081127     c                   eval      wfaard  = tbluni
045700081127     c                   endif
045800081127      * filiale
045900081127     c                   eval      wfafil = orgfil
046000081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
046100081127     c                   eval      wfafid  = *all'*'
046200081127     c                   else
046300081127     c                   eval      wfafid  = orgdes
046400081127     c                   endif
046500081128     c                   eval      Wfainf = orgind
046600081127     c                   eval      Wfacpf = orgcpf
046700081127     c                   eval      Wfaloc = orgloc
046800081127     c                   eval      Wfapro = orgpro
046900081127     c                   eval      Wfatel = orgtel
047000081127     c                   eval      Wfafax = orgfax
047100081127     c                   eval      Wfaind = Indvia
047200081127     c                   eval      Wfacit = Indcit
047300081127     c                   eval      Wfacap = Indcae
047400081127     c                   eval      Wfasta = Indsta
047500081127     c                   eval      Wfaprv = Indprv
047600081127      * decodifca della nazione
047700081127     c                   If        wfasta <> *blanks
047800081128     c                   eval      kcod = '15'
047900081127     c                   movel(p)  wfasta        kkey
048000081128     c     Ktabel        chain     tabel00f
048100081127     c                   If        %found(tabel00f)
048200081127     c                   eval      ds15 = tbluni
048300081127     c                   eval      wfanaz = §15des
048400081127     c                   endif
048500081128     c                   else
048600081128     c                   clear                   wfanaz
048700081127     c                   endif
048800081127      * valorizzo se il cliente  è bloccato
048900081128     c                   eval      wfafblc = w_wfafbl
049000081127      * flag ISTAT
049100081127     c                   eval      wfafisc = w_istat
049200081204      * delta del cliente  -- RIMOSSO - Nicola 4-12-2008 --
049300081204     c                   clear                   wfadel
049400091126     C*   /EXEC SQL
049500091126     C*   +  select waprr_10ac ,waprp_10ac  into
049600091126     C*   +  :waprr_10ac ,:waprp_10ac from WADELPCT
049700091126     C*   +  where wapcli =  :savclu  and wapaad = 2008
049800091126     C*   /END-EXEC
049900081202      * se non trovo il record metto il delta a zero
050000081204     c*                  if        sqlcod <> 0
050100081204     c*                  clear                   wfadel
050200081202
050300081204     c*                  else
050400081202
050500081204     c*                  if        waprr_10ac > 0
050600081204     c*                  eval      wfadel =((waprr_10ac - waprp_10ac)*100)/
050700081204     c*                                     waprr_10ac
050800081204     c*                  else
050900081204     c*                  clear                   wfadel
051000081204     c*                  endif
051100081203
051200081204     c*                  endif
051300081127      * pulisco i flag aumento e lettera
051400081128     c                   clear                   wfafltu
051500081128     c                   clear                   wfafauc
051600081127      * verifico se devo fare l'aumento oppure no
051700081126     c                   SELECT
051800081126      * distretto  6/7 scartato
051900081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
052000081127     c                   eval      Wfafblc= 'S'
052100081127      * categoria 'C' controllo ISTAT
052200081127     c                   When      Wfacun = 'C'
052300081126      * no bloccato  lettera di aumento   se almeno uno dei figli ha le tariffe e se non ha istat
052400081127     c                   If        wfafblc  = ' '
052500141209      * ISTAT misto   (anno 2011/12/13 non vogliono istat  automatico x clienti C)anche 2014
052600111212     c                   If        wfafisc = 'M'
052700111212     c******             eval      wfafauc = 'I'
052800081127     c                   endif
052900081127
053000091201      * ISTAT NO   NON SI FA NULLA
053100091201     c*                  If        wfafisc = 'N'
053200091201     c*                  eval      wfafauc = 'A'
053300091201     c*                  eval      wfafltu = '1'
053400091201     c*                  endif
053500081126      *
053600081127     c                   endif
053700081126      *
053800081126     c                   endsl
053900081126      * scrivo il record
054000081126     c                   write     wfaut000
054100081128
054200081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
054300081209      * i dati relativi all'area di appartenenza dell'agente unificante , la ragione sociale
054400081209      * dell'unificante
054500081128     C/EXEC SQL
054600081128     C+ update EDPAUMENTI/WFaut00F as a
054700081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
054800081209     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun,
054900081209     c+ a.wfarsu  = :wfarsu , a.wfadiv = :wfadiv,
055000081209     c+ a.wfadid  = :wfadid , a.wfaare = :wfaare,
055100081209     c+ a.wfaard  = :wfaard , a.wfafil = :wfafil,
055200081209     c+ a.wfafid  = :wfafid , a.wfaage = :wfaage,
055300081209     c+ a.wfaagd  = :wfaagd
055400081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
055500081128     C/END-EXEC
055600081128     c
055700081128
055800081126     c                   endsr
055900081126      *
056000070712      *--------------------------------------------------------------*
056100070712      * ROUTINE INIZIALE
056200070712      *--------------------------------------------------------------*
056300070712     c     *inzsr        begsr
056400070712
056500070712     c     *entry        plist
056600070712     c                   parm                    kpjba
056700070712
056800070712     c     *dtaara       define    §azute        azuteds
056900070712     c     *dtaara       define    §datiute      ddatiute
057000070712
057100070712     c                   in(e)     *dtaara
057200070712     c                   if        %error or rsut = *blanks
057300070712     c                   clear                   tibs34ds
057400070712     c                   call      'TIBS34R'
057500070712     c                   parm                    tibs34ds
057600070712     c                   in        *dtaara
057700070712     c                   endif
057800070712
057900070712      * pulisco il file
058000070712     c                   clear                   comman
058100070712     c                   movel(p)  cmd(1)        comman
058200070712     c                   call      'QCMDEXC'
058300070712     c                   parm                    comman
058400070712     c                   parm                    lenght
058500070712
058600081128     c                   open      wfaut00f
058700070712
058800070712      * reperisco data e ora del lancio
058900070712     c                   time                    w0140
059000070712     c                   move      w0140         wdata
059100070712     c                   movel     w0140         wora
059200070712
059300070712     c     kazorg        klist
059400070712     c                   kfld                    dutdis
059500070712     c                   kfld                    kcar
059600070712
059700070712     c     kcli          klist
059800070712     c                   kfld                    kkut
059900070712     c                   kfld                    kkcc
060000070712     c                   kfld                    tamksc
060100070712
060200070712     c     ktabel        klist
060300070712     c                   kfld                    codut
060400070712     c                   kfld                    kcod
060500070712     c                   kfld                    kkey
060600070712
060700070712     c                   endsr
060800070712      *--------------------------------------------------------------*
060900070712**   cmd - comando
061000081128CLRPFM FILE(EDPAUMENTI/WFAUT00F)
