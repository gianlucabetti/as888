000100070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400081125      *  Creazione archivio aumento tariffe
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800070712     ftntam04l  if   e           k disk
000801101216     ftitad04l  if   e           k disk
000900070712     fazorg01l  if   e           k disk
001000070712     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001100070712     ftabel00f  if   e           k disk
001200101216     fwfauf00f  o    e             disk    usropn extfile('EDPAUMENTI/WFAUF00F')
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
006900070712     d dsta01        e ds
007000070712     d ds01          e ds
007100081127     d ds15          e ds
007200070712     d og143         e ds
007300070712     d tibs02ds      e ds
007400070712     d tibs10ds      e ds
007500070712     d tibs34ds      e ds
007600070712     d tibs69ds      e ds
007700070712     d tise70ds      e ds
007800091126     d****** wadelpc       e ds                  extname(wadelpct)
007900101216     d wfautds       e ds                  extname(wfauf00f)
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
009200101216     c                   move      *all'0'       kksc
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
010501101216     c                   movel     tamflo        dsta01
010502101216      * se tariffe non FEDEX   leggo il successivo
010504101216     c                   if        �tafed <> 'S'
010505101216     c                   iter
010506101216     c                   endif
010507101216      * se tariffe non FEDEX  MERCI leggo il successivo
010508101216     c                   if        tamctr > 349 or tamctr < 300
010509101216     c                   iter
010510101216     c                   endif
010511101216      * se tariffa non valida per il 2011 leggo la successiva
010512101216     c                   If        tamddt < 20110101
010513101216     c                   iter
010514101216     c                   endif
010600081125      * cambio tariffa
010700070712    2c                   if        tamctr <> wctr or tamksc <> wksc
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
012000101216      * se la tariffa che sto leggendo andr� in vigore dopo il 01/01/11 la elaboro
012100070712      * ma devo elaborare anche la tariffa precedente che � in vigore ora
012200101216     c                   if        tamddt > 20110101
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
014600101130     c                   z-add     20101130      d10drf
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
018800081127     c                   eval      wfanaz = �15des
018900081127     c                   endif
019000081128     c                   else
019100081128     c                   clear                   wfanaz
019200081127     c                   endif
019300070712      * importanza cliente
019400081127     c                   eval      wfaclv = clpclv
019500081209      * agente del cliente
019600081209     c                   eval      wfaagec= clpage
019700081209      * decofico agente del cliente
019800081209     c                   eval      kcod = '01'
019900081209     c                   movel     clpage        kkey
020000081209     c     ktabel        chain     tabel00f
020100081209     c                   if        not %found(tabel00f) or tblflg <> *blanks
020200081209     c                   eval      wfaagdc = *all'*'
020300081209     c                   else
020400081209     c                   eval      ds01    = tbluni
020500081209     c                   eval      wfaagdc = �01age
020600081209     c                   endif
020700070712      * tariffa
020800081127     c                   eval      wfactrv = tamctr
020900081127     c                   eval      wfaprgv = tamprg
021000081127     c                   eval      wfaddtv = tamddt
021100081127     c                   eval      wfadstv = tamdst
021200101216      * RPV   presente
021300081125      * escludo le bloccate
021400081125     c                   if        tambap = 'B'
021500081127     c                   eval      wfabap = tambap
021600081125     c                   else
021601101216     c     ktad          setll     titad04l
021602101216     c     ktad          reade     titad04l
021603101216     c                   If        not %eof(titad04l)
021604101216      * scrivo il record solo se rpv minore di 2
021605101216     c                   if        tadrpv < 2
021800101216     c                   eval      wfarpvv = tadrpv
021801101216     c                   else
021802101216     c                   leavesr
022000101216     c                   endif
022300081125     c                   endif
022301101216     c                   endif
022700081125
022800081127     c                   write     wfaut000
022900070712
023000070712     c                   endsr
023100070712
023200081126
023300081126      *------------------------------------------------------------------------*
023400081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
023500081126      *------------------------------------------------------------------------*
023600081126     c     Sr_dettaglio  begsr
023700081126
023800081126     C/EXEC SQL
023900081126     C+ declare C1 cursor for
024000101216     C+ SELECT * FROM EDPAUMENTI/WFauf00F
024100081127     C+ WHERE WFATCL = 'C'
024200081127     C+ ORDER BY wfaclu , wfaclf
024300081128     c+ for read only
024400081126     C/END-EXEC
024500081126
024600081126     C/EXEC SQL
024700081126     C+ open C1
024800081126     C/END-EXEC
024900081126     C
025000081126     C/EXEC SQL
025100081127     C+ Fetch C1 into :wfautds
025200081126     C/END-EXEC
025300081126     C*
025400081126
025500081126     C*
025600081126     C                   dow       sqlcod = *zeros
025700081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
025800081127     c                   If        savclu <> ds_WFACLU
025900081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
026000081127     c                   If        savclu <> *zeros
026100081126     c                   exsr      Sr_unific
026200081126     c                   endif
026300081127     c                   movel     ds_wfaclu     savclu
026400081127      * imposto il campo di lavoro per verificare se il cliente � bloccato
026500081126     c                   eval      W_wfafbl = 'S'
026800081127     c                   endif
026900081127      * per ogni tariffa memorizzo le caratteristiche
027000081126      * per ogni tariffa cliente verifico istat
029200081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
029300081127     c                   if        DS_wfafblc = ' '
029400081127     c                   clear                   w_wfafbl
029500081127     c                   endif
029600081126     C*
029700081126     C*
029800081126     C/EXEC SQL
029900081127     C+ Fetch C1 into :wfautds
030000081126     C/END-EXEC
030100081126     C*
030200081126     C                   enddo
030300081128      * se � fine file scrivo l'ultimo unificante
030400081128     c                   If        sqlcod = 100 and savclu > *zero
030500081128     c                   exsr      sr_unific
030600081128     c                   endif
030700081126     C*
030800081126     C/EXEC SQL
030900081126     C+ close C1
031000081126     C/END-EXEC
031100081126     C
031200081126
031300081126     c                   endsr
031400081126      *---------------------------------------------------------------------------------------------
031500081126      * SR_UNIFIC    Elaborazione del cliente unificante
031600081126      *---------------------------------------------------------------------------------------------
031700081126     C     SR_UNIFIC     BEGSR
031800081126      *
031900081128     c                   clear                   wfaut000
032000081126      * aggancio anagrafiche
032100081126     C                   clear                   TIBS69ds
032200081128     C                   clear                   ds_CNACO
032300081128     C                   clear                   ds_CNIND
032400081128     C                   clear                   ds_CNCLP
032500081128     C                   clear                   ds_FNCLS
032600081127     C                   eval      I69kac =  savclu
032700081127     C                   eval      I69kin =  savclu
032800081127     C                   eval      I69kcp =  savclu
032900081126     C                   call      'TIBS69R'
033000081126     C                   parm                    TIBS69ds
033100081128     C                   parm                    ds_CNACO
033200081128     C                   parm                    ds_CNIND
033300081128     C                   parm                    ds_CNCLP
033400081128     C                   parm                    ds_FNCLS
033500081126      * valorizzo i campi del file
033600081126     c                   eval      Wfatcl = 'U'
033700081127     c                   eval      Wfaclu = savclu
033800081127     c                   eval      Wfacun = clpclv
033900081127     c                   eval      Wfaclv = clpclv
034000081127
034100081127     c                   clear                   Wfaclf
034200081126     c                   eval      Wfarag = Acorag
034300081209     c                   eval      Wfarsu = Acorag
034400081127      * cerco codice commerciale
034500081127      * verifico il codice commerciale unificante
034600081127     c                   eval      kcod = '01'
034700081127     c                   movel     clpage        kkey
034800081127     c     ktabel        chain     tabel00f
034900081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
035000081127     c                   eval      wfaage  = clpage
035100081127     c                   eval      wfaagd  = *all'*'
035200081127     c                   else
035300081127     c                   eval      ds01    = tbluni
035400081127     c                   eval      wfaage  = �01rgf
035500081127     c                   movel     wfaage        kkey
035600081127     c     ktabel        chain     tabel00f
035700081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
035800081127     c                   eval      wfaagd  = *all'*'
035900081127     c                   else
036000081127     c                   eval      ds01    = tbluni
036100081127     c                   eval      wfaagd  = �01age
036200081127     c                   endif
036300081127     c                   endif
036400081127
036500081127      * albero commerciale unificante
036600081127     c                   movel     wfaage        w0030
036700081127     c     w0030         chain     azorg01l
036800081127
036900081127      * distretto
037000081127     c                   eval      wfadiv = orgfl3
037100081127     c                   eval      kcod = '17'
037200081127     c                   eval      kkey = wfadiv
037300081127     c     ktabel        chain     tabel00f
037400081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
037500081127     c                   eval      wfadid  = *all'*'
037600081127     c                   else
037700081127     c                   eval      wfadid  = tbluni
037800081127     c                   endif
037900081128      * se divisione 7 cancello tutti i clienti legati all'unificante
038000081128     c                   If        wfadiv = '7'
038100081128     C/EXEC SQL
038200101222     C+ delete from EDPAUMENTI/WFauf00F as a
038300081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
038400081128     C/END-EXEC
038500081128     c                   leavesr
038600081128     c                   endif
038700081127      * area
038800081127     c                   eval      wfaare = orgcar
038900081127     c                   eval      kcod = '05'
039000081127     c                   movel     wfaare        kkey
039100081127     c     ktabel        chain     tabel00f
039200081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
039300081127     c                   eval      wfaard  = *all'*'
039400081127     c                   else
039500081127     c                   eval      wfaard  = tbluni
039600081127     c                   endif
039700081127      * filiale
039800081127     c                   eval      wfafil = orgfil
039900081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
040000081127     c                   eval      wfafid  = *all'*'
040100081127     c                   else
040200081127     c                   eval      wfafid  = orgdes
040300081127     c                   endif
040400081128     c                   eval      Wfainf = orgind
040500081127     c                   eval      Wfacpf = orgcpf
040600081127     c                   eval      Wfaloc = orgloc
040700081127     c                   eval      Wfapro = orgpro
040800081127     c                   eval      Wfatel = orgtel
040900081127     c                   eval      Wfafax = orgfax
041000081127     c                   eval      Wfaind = Indvia
041100081127     c                   eval      Wfacit = Indcit
041200081127     c                   eval      Wfacap = Indcae
041300081127     c                   eval      Wfasta = Indsta
041400081127     c                   eval      Wfaprv = Indprv
041500081127      * decodifca della nazione
041600081127     c                   If        wfasta <> *blanks
041700081128     c                   eval      kcod = '15'
041800081127     c                   movel(p)  wfasta        kkey
041900081128     c     Ktabel        chain     tabel00f
042000081127     c                   If        %found(tabel00f)
042100081127     c                   eval      ds15 = tbluni
042200081127     c                   eval      wfanaz = �15des
042300081127     c                   endif
042400081128     c                   else
042500081128     c                   clear                   wfanaz
042600081127     c                   endif
042700081127      * valorizzo se il cliente  � bloccato
042800081128     c                   eval      wfafblc = w_wfafbl
043101101216     c                   clear                   wfafisc
043200081204     c                   clear                   wfadel
045200081127      * pulisco i flag aumento e lettera
045300081128     c                   clear                   wfafltu
045400081128     c                   clear                   wfafauc
045500081127      * verifico se devo fare l'aumento oppure no
045600081126     c                   SELECT
045700081126      * distretto  6/7 scartato
045800081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
045900081127     c                   eval      Wfafblc= 'S'
047400081126      *
047600081126      *
047700081126     c                   endsl
047800081126      * scrivo il record
047900081126     c                   write     wfaut000
048000081128
048100081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
048200081209      * i dati relativi all'area di appartenenza dell'agente unificante , la ragione sociale
048300081209      * dell'unificante
048400081128     C/EXEC SQL
048500101216     C+ update EDPAUMENTI/WFauf00F as a
048600081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
048700081209     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun,
048800081209     c+ a.wfarsu  = :wfarsu , a.wfadiv = :wfadiv,
048900081209     c+ a.wfadid  = :wfadid , a.wfaare = :wfaare,
049000081209     c+ a.wfaard  = :wfaard , a.wfafil = :wfafil,
049100081209     c+ a.wfafid  = :wfafid , a.wfaage = :wfaage,
049200081209     c+ a.wfaagd  = :wfaagd
049300081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
049400081128     C/END-EXEC
049500081128     c
049600081128
049700081126     c                   endsr
049800081126      *
049900070712      *--------------------------------------------------------------*
050000070712      * ROUTINE INIZIALE
050100070712      *--------------------------------------------------------------*
050200070712     c     *inzsr        begsr
050300070712
050400070712     c     *entry        plist
050500070712     c                   parm                    kpjba
050600070712
050700070712     c     *dtaara       define    �azute        azuteds
050800070712     c     *dtaara       define    �datiute      ddatiute
050900070712
051000070712     c                   in(e)     *dtaara
051100070712     c                   if        %error or rsut = *blanks
051200070712     c                   clear                   tibs34ds
051300070712     c                   call      'TIBS34R'
051400070712     c                   parm                    tibs34ds
051500070712     c                   in        *dtaara
051600070712     c                   endif
054400070712
054500070712      * pulisco il file
054600070712     c                   clear                   comman
054700070712     c                   movel(p)  cmd(1)        comman
054800070712     c                   call      'QCMDEXC'
054900070712     c                   parm                    comman
055000070712     c                   parm                    lenght
055100070712
055200101216     c                   open      wfauf00f
055300070712
055400070712      * reperisco data e ora del lancio
055500070712     c                   time                    w0140
055600070712     c                   move      w0140         wdata
055700070712     c                   movel     w0140         wora
055800070712
055900070712     c     kazorg        klist
056000070712     c                   kfld                    dutdis
056100070712     c                   kfld                    kcar
056200070712
056300070712     c     kcli          klist
056400070712     c                   kfld                    kkut
056500070712     c                   kfld                    kkcc
056600070712     c                   kfld                    tamksc
056700070712
056701101216     c     ktad          klist
056702101216     c                   kfld                    tamksc
056703101216     c                   kfld                    tamctr
056704101216     c                   kfld                    tamprg
056705101216
056800070712     c     ktabel        klist
056900070712     c                   kfld                    codut
057000070712     c                   kfld                    kcod
057100070712     c                   kfld                    kkey
057200070712
057300070712     c                   endsr
057400070712      *--------------------------------------------------------------*
057500070712**   cmd - comando
057600101216CLRPFM FILE(EDPAUMENTI/WFAUF00F)
