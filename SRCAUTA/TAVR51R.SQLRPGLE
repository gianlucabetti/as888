000100130830     /*PRM  dbgview(*source)
000200130830     /*CMD  ovrdbf file(WFAUF00F) tofile(EDPAUMENTI/WFAUF00F) +
000300130830     /*CMD         ovrscope(*calllvl)
000400130830     /*END  dltovr file(WFAUF00F) lvl(*)
000500130830     /*END
000600070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000700070712      *------------------------------------------------------------------------*
000800070712      *
000900081125      *  Creazione archivio aumento tariffe
001000070712      *
001100070712      *------------------------------------------------------------------------*
001200070712
001300070712     ftntam04l  if   e           k disk
001400101216     ftitad04l  if   e           k disk
001500070712     fazorg01l  if   e           k disk
001600070712     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001700130830     fAZCMM01L  if   e           k disk
001800070712     ftabel00f  if   e           k disk
001900101216     fwfauf00f  o    e             disk    usropn extfile('EDPAUMENTI/WFAUF00F')
002000070712
002100070712      *------------------------------------------------------------------------*
002200070712      *  RIEPILOGO INDICATORI
002300070712      *------------------------------------------------------------------------*
002400070712      * 02 - ho scritto almeno 1 rcd
002500070712
002600070712      *------------------------------------------------------------------------*
002700070712      *   V A R I A B I L I
002800070712      *------------------------------------------------------------------------*
002900070712     d comman          s             80
003000070712     d codut           s              1  0 inz(1)
003100070712     d kcar            s                   like(orgcar)
003200070712     d kcod            s                   like(tblcod)
003300070712     d kksc            s                   like(tamksc)
003400070712     d kkey            s                   like(tblkey)
003500070712     d lenght          s             15  5 inz(80)
003600070712     d kkcc            s                   like(acokcc) inz(151)
003700070712     d kkut            s                   like(acokut) inz(1)
003800070712     d wctr            s                   like(tamctr)
003900070712     d wdata           s              8  0
004000070712     d wksc            s                   like(tamksc)
004100070712     d wlib            s              9
004200070712     d wok             s              1    inz(*off)
004300070712     d wora            s              6  0
004400070712     d w0030           s              3  0
004500070712     d w0140           s             14  0
004600081127     d savclu          s                   like(wfaclu)
004700081128     d W_wfafbl        S                   like(WFAfblc)
004800081128     d W_istat         S              1
004900070712
005000070712      *------------------------------------------------------------------------*
005100070712      *   S C H I E R E
005200070712      *------------------------------------------------------------------------*
005300070712     d cmd             s             80    dim(1) ctdata perrcd(1)
005400070712     d dia             s              8  0 dim(12)
005500070712
005600070712      *------------------------------------------------------------------------*
005700070712      *   D S   I N T E R N E / E S T E R N E
005800070712      *------------------------------------------------------------------------*
005900070712
006000070712     d azuteds       e ds                  extname(azute00f)
006100070712     d ddatiute      e ds
006200070712     d ds_cnaco      e ds                  inz extname(cnaco00f)
006300070712     d ds_cnind      e ds                  inz extname(cnind00f)
006400070712     d ds_cnclp      e ds                  inz extname(cnclp00f)
006500070712     d ds_fncls      e ds                  inz extname(fncls00f)
006600070712     d kpjba         e ds
006700070712     d dsta01        e ds
006800081127     d ds15          e ds
006900070712     d og143         e ds
007000070712     d tibs02ds      e ds
007100070712     d tibs10ds      e ds
007200070712     d tibs34ds      e ds
007300070712     d tibs69ds      e ds
007400070712     d tise70ds      e ds
007500091126     d****** wadelpc       e ds                  extname(wadelpct)
007600101216     d wfautds       e ds                  extname(wfauf00f)
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
008900101216     c                   move      *all'0'       kksc
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
010300101216     c                   movel     tamflo        dsta01
010400101216      * se tariffe non FEDEX   leggo il successivo
010500101216     c                   if        §tafed <> 'S'
010600101216     c                   iter
010700101216     c                   endif
010800101216      * se tariffe non FEDEX  MERCI leggo il successivo
010900101216     c                   if        tamctr > 349 or tamctr < 300
011000101216     c                   iter
011100101216     c                   endif
011200101216      * se tariffa non valida per il 2011 leggo la successiva
011300101216     c                   If        tamddt < 20110101
011400101216     c                   iter
011500101216     c                   endif
011600081125      * cambio tariffa
011700070712    2c                   if        tamctr <> wctr or tamksc <> wksc
011800070712
011900070712      * escludo le annullate
012000070712     c                   if        tamatb <> *blanks
012100070712     c                   iter
012200070712     c                   endif
012300070712      * scrivo il file
012400070712     c                   exsr      scrivi
012500070712
012600070712     c                   eval      wksc = tamksc
012700070712     c                   eval      wctr = tamctr
012800070712
012900101216      * se la tariffa che sto leggendo andrà in vigore dopo il 01/01/11 la elaboro
013000070712      * ma devo elaborare anche la tariffa precedente che è in vigore ora
013100101216     c                   if        tamddt > 20110101
013200070712     c                   eval      wctr = 999
013300070712     c                   endif
013400070712    2c                   endif
013500070712
013600070712    1c                   enddo
013700081126     c                   exsr      SR_Dettaglio
013800081126
013900070712     c                   eval      *inlr = *on
014000081126
014100070712      *------------------------------------------------------------------------*
014200081127      * SCRIVO IL FILE PER GLI AUMENTI
014300070712      *------------------------------------------------------------------------*
014400070712     c     scrivi        begsr
014500070712
014600081127     c                   clear                   wfaut000
014700070712
014800081127      * tipo record cliente/tariffa
014900081127     c                   eval      wfatcl = 'C'
015000081127      * cerco il codice cliente unificante
015100081127     c                   clear                   tibs10ds
015200081127     c                   eval      d10tle = 'ST'
015300081127     c                   eval      d10paf = 'P'
015400081127     c                   z-add     tamksc        d10cod
015500101130     c                   z-add     20101130      d10drf
015600081202     c                   call      'TIBS10R'
015700081127     c                   parm                    tibs10ds
015800081127     c                   parm      'A'           partpexe          1
015900081127      * se ci sono errori valorizzo come unificante il cliente
016000081127     c                   if        d10err <> ' '
016100081128     c                   eval      wfaclu = tamksc
016200081127      * se tutto ok valorizzo l'unificante trovato
016300081127     c                   else
016400081127     c                   eval      wfaclu = d10cop
016500081127     c                   endif
016600081127      * cliente
016700081127     c                   eval      wfaclf = tamksc
016800081127      * cerco i dati anagrafici
016900081127     c                   clear                   tibs69ds
017000081127     c                   move      tamksc        i69kac
017100081127     c                   move      tamksc        i69kcp
017200081127     c                   move      tamksc        i69kin
017300081127     c                   call      'TIBS69R'
017400081127     c                   parm                    tibs69ds
017500081127     c                   parm                    ds_cnaco
017600081127     c                   parm                    ds_cnind
017700081127     c                   parm                    ds_cnclp
017800081127     c                   parm                    ds_fncls
017900081127
018000081127     c                   IF        acoabl <> ' '
018100081127     c                   eval      Wfafblc = 'S'
018200081127     c                   endif
018300081127      * ragione sociale e indirizzo
018400081127     c                   eval      wfarag = acorag
018500081127     c                   eval      Wfaind = Indvia
018600081127     c                   eval      Wfacit = Indcit
018700081127     c                   eval      Wfacap = Indcae
018800081127     c                   eval      Wfasta = Indsta
018900081127     c                   eval      Wfaprv = Indprv
019000081127      * decodifca della nazione
019100081127     c                   If        wfasta <> *blanks
019200081128     c                   eval      kcod = '15'
019300081127     c                   movel(p)  wfasta        kkey
019400081128     c     Ktabel        chain     tabel00f
019500081127     c                   If        %found(tabel00f)
019600081127     c                   eval      ds15 = tbluni
019700081127     c                   eval      wfanaz = §15des
019800081127     c                   endif
019900081128     c                   else
020000081128     c                   clear                   wfanaz
020100081127     c                   endif
020200070712      * importanza cliente
020300081127     c                   eval      wfaclv = clpclv
020400130830      * comm.le del cliente
020500081209     c                   eval      wfaagec= clpage
020600130830      * decodifico comm.le del cliente
020700130830     c     CLPage        chain     AZCMM000
020800130830     c                   if        not %found(AZCMM01L)
020900081209     c                   eval      wfaagdc = *all'*'
021000081209     c                   else
021100130830     c                   eval      wfaagdc = CMMdes
021200081209     c                   endif
021300070712      * tariffa
021400081127     c                   eval      wfactrv = tamctr
021500081127     c                   eval      wfaprgv = tamprg
021600081127     c                   eval      wfaddtv = tamddt
021700081127     c                   eval      wfadstv = tamdst
021800101216      * RPV   presente
021900081125      * escludo le bloccate
022000081125     c                   if        tambap = 'B'
022100081127     c                   eval      wfabap = tambap
022200081125     c                   else
022300101216     c     ktad          setll     titad04l
022400101216     c     ktad          reade     titad04l
022500101216     c                   If        not %eof(titad04l)
022600101216      * scrivo il record solo se rpv minore di 2
022700101216     c                   if        tadrpv < 2
022800101216     c                   eval      wfarpvv = tadrpv
022900101216     c                   else
023000101216     c                   leavesr
023100101216     c                   endif
023200081125     c                   endif
023300101216     c                   endif
023400081125
023500081127     c                   write     wfaut000
023600070712
023700070712     c                   endsr
023800070712
023900081126
024000081126      *------------------------------------------------------------------------*
024100081126      * SCRIVO IL FILE PER LA GESTIONE DEGLI AUMENTI
024200081126      *------------------------------------------------------------------------*
024300081126     c     Sr_dettaglio  begsr
024400081126
024500081126     C/EXEC SQL
024600081126     C+ declare C1 cursor for
024700101216     C+ SELECT * FROM EDPAUMENTI/WFauf00F
024800081127     C+ WHERE WFATCL = 'C'
024900081127     C+ ORDER BY wfaclu , wfaclf
025000081128     c+ for read only
025100081126     C/END-EXEC
025200081126
025300081126     C/EXEC SQL
025400081126     C+ open C1
025500081126     C/END-EXEC
025600081126     C
025700081126     C/EXEC SQL
025800081127     C+ Fetch C1 into :wfautds
025900081126     C/END-EXEC
026000081126     C*
026100081126
026200081126     C*
026300081126     C                   dow       sqlcod = *zeros
026400081127      * Per ogni unificante elaboro i suoi figli per poi aggiornare l'unificante
026500081127     c                   If        savclu <> ds_WFACLU
026600081127      * a cambio unificante scrivo l'unificante ed aggiorno i suoi figli
026700081127     c                   If        savclu <> *zeros
026800081126     c                   exsr      Sr_unific
026900081126     c                   endif
027000081127     c                   movel     ds_wfaclu     savclu
027100081127      * imposto il campo di lavoro per verificare se il cliente è bloccato
027200081126     c                   eval      W_wfafbl = 'S'
027300081127     c                   endif
027400081127      * per ogni tariffa memorizzo le caratteristiche
027500081126      * per ogni tariffa cliente verifico istat
027600081127      * controllo il flag di cliente bloccato .. se tutti bloccati unificante bloccato
027700081127     c                   if        DS_wfafblc = ' '
027800081127     c                   clear                   w_wfafbl
027900081127     c                   endif
028000081126     C*
028100081126     C*
028200081126     C/EXEC SQL
028300081127     C+ Fetch C1 into :wfautds
028400081126     C/END-EXEC
028500081126     C*
028600081126     C                   enddo
028700081128      * se è fine file scrivo l'ultimo unificante
028800081128     c                   If        sqlcod = 100 and savclu > *zero
028900081128     c                   exsr      sr_unific
029000081128     c                   endif
029100081126     C*
029200081126     C/EXEC SQL
029300081126     C+ close C1
029400081126     C/END-EXEC
029500081126     C
029600081126
029700081126     c                   endsr
029800081126      *---------------------------------------------------------------------------------------------
029900081126      * SR_UNIFIC    Elaborazione del cliente unificante
030000081126      *---------------------------------------------------------------------------------------------
030100081126     C     SR_UNIFIC     BEGSR
030200081126      *
030300081128     c                   clear                   wfaut000
030400081126      * aggancio anagrafiche
030500081126     C                   clear                   TIBS69ds
030600081128     C                   clear                   ds_CNACO
030700081128     C                   clear                   ds_CNIND
030800081128     C                   clear                   ds_CNCLP
030900081128     C                   clear                   ds_FNCLS
031000081127     C                   eval      I69kac =  savclu
031100081127     C                   eval      I69kin =  savclu
031200081127     C                   eval      I69kcp =  savclu
031300081126     C                   call      'TIBS69R'
031400081126     C                   parm                    TIBS69ds
031500081128     C                   parm                    ds_CNACO
031600081128     C                   parm                    ds_CNIND
031700081128     C                   parm                    ds_CNCLP
031800081128     C                   parm                    ds_FNCLS
031900081126      * valorizzo i campi del file
032000081126     c                   eval      Wfatcl = 'U'
032100081127     c                   eval      Wfaclu = savclu
032200081127     c                   eval      Wfacun = clpclv
032300081127     c                   eval      Wfaclv = clpclv
032400081127
032500081127     c                   clear                   Wfaclf
032600081126     c                   eval      Wfarag = Acorag
032700081209     c                   eval      Wfarsu = Acorag
032800081127      * cerco codice commerciale
032900081127      * verifico il codice commerciale unificante
033000130830     c     CLPage        chain     AZCMM000
033100130830     c                   if        not %found(AZCMM01L)
033200081127     c                   eval      wfaage  = clpage
033300081127     c                   eval      wfaagd  = *all'*'
033400081127     c                   else
033500130830     c                   eval      wfaage  = CMMuni
033600130830     c     WFAage        chain     AZCMM000
033700130830     c                   if        not %found(AZCMM01L)
033800081127     c                   eval      wfaagd  = *all'*'
033900081127     c                   else
034000130830     c                   eval      wfaagd  = CMMdes
034100081127     c                   endif
034200081127     c                   endif
034300081127
034400081127      * albero commerciale unificante
034500081127     c                   movel     wfaage        w0030
034600081127     c     w0030         chain     azorg01l
034700081127
034800081127      * distretto
034900081127     c                   eval      wfadiv = orgfl3
035000081127     c                   eval      kcod = '17'
035100081127     c                   eval      kkey = wfadiv
035200081127     c     ktabel        chain     tabel00f
035300081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
035400081127     c                   eval      wfadid  = *all'*'
035500081127     c                   else
035600081127     c                   eval      wfadid  = tbluni
035700081127     c                   endif
035800081128      * se divisione 7 cancello tutti i clienti legati all'unificante
035900081128     c                   If        wfadiv = '7'
036000081128     C/EXEC SQL
036100101222     C+ delete from EDPAUMENTI/WFauf00F as a
036200081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
036300081128     C/END-EXEC
036400081128     c                   leavesr
036500081128     c                   endif
036600081127      * area
036700081127     c                   eval      wfaare = orgcar
036800081127     c                   eval      kcod = '05'
036900081127     c                   movel     wfaare        kkey
037000081127     c     ktabel        chain     tabel00f
037100081127     c                   if        not %found(tabel00f) or tblflg <> *blanks
037200081127     c                   eval      wfaard  = *all'*'
037300081127     c                   else
037400081127     c                   eval      wfaard  = tbluni
037500081127     c                   endif
037600081127      * filiale
037700081127     c                   eval      wfafil = orgfil
037800081127     c                   if        not %found(azorg01l) or orgfva <> *blanks
037900081127     c                   eval      wfafid  = *all'*'
038000081127     c                   else
038100081127     c                   eval      wfafid  = orgdes
038200081127     c                   endif
038300081128     c                   eval      Wfainf = orgind
038400081127     c                   eval      Wfacpf = orgcpf
038500081127     c                   eval      Wfaloc = orgloc
038600081127     c                   eval      Wfapro = orgpro
038700081127     c                   eval      Wfatel = orgtel
038800081127     c                   eval      Wfafax = orgfax
038900081127     c                   eval      Wfaind = Indvia
039000081127     c                   eval      Wfacit = Indcit
039100081127     c                   eval      Wfacap = Indcae
039200081127     c                   eval      Wfasta = Indsta
039300081127     c                   eval      Wfaprv = Indprv
039400081127      * decodifca della nazione
039500081127     c                   If        wfasta <> *blanks
039600081128     c                   eval      kcod = '15'
039700081127     c                   movel(p)  wfasta        kkey
039800081128     c     Ktabel        chain     tabel00f
039900081127     c                   If        %found(tabel00f)
040000081127     c                   eval      ds15 = tbluni
040100081127     c                   eval      wfanaz = §15des
040200081127     c                   endif
040300081128     c                   else
040400081128     c                   clear                   wfanaz
040500081127     c                   endif
040600081127      * valorizzo se il cliente  è bloccato
040700081128     c                   eval      wfafblc = w_wfafbl
040800101216     c                   clear                   wfafisc
040900081204     c                   clear                   wfadel
041000081127      * pulisco i flag aumento e lettera
041100081128     c                   clear                   wfafltu
041200081128     c                   clear                   wfafauc
041300081127      * verifico se devo fare l'aumento oppure no
041400081126     c                   SELECT
041500081126      * distretto  6/7 scartato
041600081126     c                   When      wfadiv =  '6' or  Wfadiv =  '7'
041700081127     c                   eval      Wfafblc= 'S'
041800081126      *
041900081126      *
042000081126     c                   endsl
042100081126      * scrivo il record
042200081126     c                   write     wfaut000
042300081128
042400081128      * per ogni cliente legato all'unificante vado ad aggiornare wfafauc e wfafisc e wfafblc
042500081209      * i dati relativi all'area di appartenenza dell'agente unificante , la ragione sociale
042600081209      * dell'unificante
042700081128     C/EXEC SQL
042800101216     C+ update EDPAUMENTI/WFauf00F as a
042900081128     c+ set a.wfafblc = :wfafblc , a.wfafauc = :wfafauc ,
043000081209     c+ a.wfafisc = :wfafisc , a.wfacun = :wfacun,
043100081209     c+ a.wfarsu  = :wfarsu , a.wfadiv = :wfadiv,
043200081209     c+ a.wfadid  = :wfadid , a.wfaare = :wfaare,
043300081209     c+ a.wfaard  = :wfaard , a.wfafil = :wfafil,
043400081209     c+ a.wfafid  = :wfafid , a.wfaage = :wfaage,
043500081209     c+ a.wfaagd  = :wfaagd
043600081128     C+ WHERE a.wfatcl = 'C' and a.WFAclu = :wfaclu
043700081128     C/END-EXEC
043800081128     c
043900081128
044000081126     c                   endsr
044100081126      *
044200070712      *--------------------------------------------------------------*
044300070712      * ROUTINE INIZIALE
044400070712      *--------------------------------------------------------------*
044500070712     c     *inzsr        begsr
044600070712
044700070712     c     *entry        plist
044800070712     c                   parm                    kpjba
044900070712
045000070712     c     *dtaara       define    §azute        azuteds
045100070712     c     *dtaara       define    §datiute      ddatiute
045200070712
045300070712     c                   in(e)     *dtaara
045400070712     c                   if        %error or rsut = *blanks
045500070712     c                   clear                   tibs34ds
045600070712     c                   call      'TIBS34R'
045700070712     c                   parm                    tibs34ds
045800070712     c                   in        *dtaara
045900070712     c                   endif
046000070712
046100070712      * pulisco il file
046200070712     c                   clear                   comman
046300070712     c                   movel(p)  cmd(1)        comman
046400070712     c                   call      'QCMDEXC'
046500070712     c                   parm                    comman
046600070712     c                   parm                    lenght
046700070712
046800101216     c                   open      wfauf00f
046900070712
047000070712      * reperisco data e ora del lancio
047100070712     c                   time                    w0140
047200070712     c                   move      w0140         wdata
047300070712     c                   movel     w0140         wora
047400070712
047500070712     c     kazorg        klist
047600070712     c                   kfld                    dutdis
047700070712     c                   kfld                    kcar
047800070712
047900070712     c     kcli          klist
048000070712     c                   kfld                    kkut
048100070712     c                   kfld                    kkcc
048200070712     c                   kfld                    tamksc
048300070712
048400101216     c     ktad          klist
048500101216     c                   kfld                    tamksc
048600101216     c                   kfld                    tamctr
048700101216     c                   kfld                    tamprg
048800101216
048900070712     c     ktabel        klist
049000070712     c                   kfld                    codut
049100070712     c                   kfld                    kcod
049200070712     c                   kfld                    kkey
049300070712
049400070712     c                   endsr
049500070712      *--------------------------------------------------------------*
049600070712**   cmd - comando
049700101216CLRPFM FILE(EDPAUMENTI/WFAUF00F)
