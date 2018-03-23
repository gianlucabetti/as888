000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200050513     H* FNLRP7R *----------------------------------------------------*
000300050513     H*  - Batch elenco spedizioni in distinta di diverso tipo zona cons
000400940221      *--------------------------------------------------------------*
000500080303     fAZORG01L  if   e           k disk
000600040305     FFNARB70L  IF   E           K DISK
000700090309     FFiARt01L  IF   E           K DISK
000800050513     Ftabel00f  IF   E           K DISK
000900050513     Ffnfvv01l  IF   E           K DISK
001000070122     Ffnbrv07l  IF   E           K DISK
001100040305     Ffiapd01L  IF   E           K DISK
001200080303      *
001300080303     fWFTPM01L  Uf A e           k disk
001400080303     fFNLRP7P   o    e             printer oflind(*in90)
001500941230      *--------------------------------------------------------------*
001600080303     d CMESSAG         c                   const('M E S S A G G E R I E')
001700080303     d Cparcel         c                   const('     P A R C E L     ')
001800080303      *--------------------------------------------------------------*
001900050517     D NPS             S              2    DIM(102)
002000080303     D zone            S              2    DIM(102)
002100080303      *
002200070123     D Wimm            S             14  0
002300050513     D Wtrov           S              1
002400050513     D wdfv            S                   LIKE(fvvdfv)
002500050513     D wspg            S                   LIKE(fvvspg)
002600050513     D cod             S                   LIKE(tblcod)
002700050516     D key             S                   LIKE(tblkey)
002800050513     D Kfgs            S                   LIKE(arbifp)
002900050513     D kfvvfgs         S                   LIKE(fvvfgs)
003000050513     D knfv            S                   LIKE(fvvnfv)
003100050513     D knpg            S                   LIKE(fvvnpg) inz(4)
003200050513     D segnazona       S                   LIKE(brvznc)
003300050513     D sav_arbndc      S                   LIKE(arbndc)
003400050517     D savSPE          S                   LIKE(WTPMSPE)
003500040305     d data_eur        s               d   datfmt(*eur)
003600040305      *--------------------------------------------------------------*
003700080303      *
003800080303     d Status         sds           333
003900080303     d   SDSpgm          *proc
004000080303      *
004100080303     d KPJBA         e ds
004200080303      * DS RICEZIONE DATI DA FILTRO
004300080303     d FNLRP7DS      e ds
004400080303      *
004500080303      * - Parametri per richiamo al pgm di controllo profilo utenti
004600080303     d Tibs34Ds      e ds                  inz
004700080303      * - Ds di riferimento al file esterno AzUte00F
004800080303     d AzuteDs       e ds                  ExtName(AzUte00F)
004900080303      * - Ds per dati organigramma
005000080303     d DDatiUte      e ds
005100050513     d* DS codici pistola
005200050513     D DS7J          E DS
005300050513     d* DS per campo fvvspn
005400050513     D DFVVSPN       E DS
005500050513     D* REPERISCE DATI FOGLI
005600050513     D DSLV53        E DS                  EXTNAME(FNLV53DS)
005700050516     D ds03          E DS
005800070122     D RECBRV        E DS                  EXTNAME(FNBRV00F)
005900070122     D WSAVB         E DS                  EXTNAME(FNBRV00F)
006000050513     d                                     prefix(S_)
006100080303     D                 DS
006200080303     D  WTPMLNP                1      3  0
006300080303     D  WTPMNRS                4      5  0
006400080303     D  WTPMNSP                6     12  0
006500080303     D  WTPMSPE                1     12  0
006600080303      *---------------------------------------------------------------*
006700080303      *
006800050513     c                   z-add     ip7fil        kfgs
006900040305     c                   clear                   sav_arbndc
007000080303      *
007100080303      *S C R I T T U R A   W O R K - F I L E?
007200080303      *
007300080303      *?- Ciclo di elaborazione file FNARB70L?
007400080303     c     karb          setll     FNARB000
007500080303      *
007600080303do  1c                   DO        *hival
007700080303      *
007800080303     c     kfgs          reade     FNARB000
007900080303     c                   if        %eof(FNARB70L)
008000080303     c                   leave
008100080303     c                   endif
008200040305     c* se numero distinta > "numero distinta al" esco dal ciclo
008300050513     c     arbndc        ifgt      ip7nra
008400040305     c                   leave
008500040305     c                   endif
008600040305     c* se data distinta diversa da quella passata ignoro il record
008700050513     c     arbddc        ifne      ip7dat
008800040305     c                   iter
008900040305     c                   endif
009000080303     c*
009100050516     c* Escludo spedizione di zona non richiesta
009200050516     c                   movel     arbznc        alfznc            2
009300050516     c     alfznc        lookup    zone                                   35
009400050516     c                   if        not *in35
009500050516     c                   iter
009600050516     c                   endif
009700080303     c*
009800050513     c* Escludo spedizione se su distinta di tipo non richiesto
009900050516     c                   z-add     arbifp        kfvvfgs
010000050513     c                   z-add     arbndc        knfv
010100050513     c     kfvv          chain     fnfvv01l
010200080303     c*
010300050513     c                   if        not %found(fnfvv01l)
010400050513     c                   iter
010500050513     c                   endif
010600080303     c*
010700050513     c                   movel     fvvspn        dfvvspn
010800050513     c                   if        (ip7tpm='M' and §fvvtpm='M') or
010900050513     c                             (ip7tpm='P' and §fvvtpm<>'M')
011000050513     c                   iter
011100050513     c                   endif
011200080303      *
011300090309      *?- Ciclo di elaborazione file FiART01L?
011400050513     c* Verifico se la zona spunta di ogni segnacollo è <> da zona bolla
011500090309     c     kart          setll     FiART000
011600080303      *
011700080303do  2c                   DO        *hival
011800080303      *
011900090309     c     kart          reade     FiART000
012000090309     c                   if        %eof(FiART01L)
012100080303     c                   leave
012200080303     c                   endif
012300080303     c*
012400050513     c* Cerco ultima spunta effettuata se zona segnacollo se pistola reale
012500080303     c*
012600050513     c* Imposto la zona bolla = zna segnacollo
012700050513     c                   eval      segnazona=arbznc
012800080303     c*
012900050513     c                   exsr      RICSPU
013000080303     c*
013100050513     c* Memorizzo in file di wrk il segnacollo da stampare e la bolla
013200080303     c                   clear                   WFTPM000
013300050513     c                   movel     segnazona     wtpmzns
013400050513     c                   movel     arblna        wtpmlna
013500050513     c                   movel     artnsc        wtpmnsc
013600050513     c                   movel     arbaas        wtpmaas
013700050516     c                   movel     arbmgs        wtpmmgs
013800050513     c                   movel     arblnp        wtpmlnp
013900050513     c                   movel     arbnrs        wtpmnrs
014000050513     c                   movel     arbnsp        wtpmnsp
014100050516     c                   movel     arbznc        wtpmznb
014200050513     c                   movel     arbrsm        wtpmrsm
014300050513     c                   movel     arbinm        wtpminm
014400050513     c                   movel     arbcam        wtpmcam
014500050513     c                   movel     arblom        wtpmlom
014600050513     c                   movel     arbprm        wtpmprm
014700050513     c                   movel     arbnzm        wtpmnzm
014800050513     c                   movel     arbrsd        wtpmrsd
014900050513     c                   movel     arbind        wtpmind
015000050513     c                   movel     arbcad        wtpmcad
015100050513     c                   movel     arblod        wtpmlod
015200050513     c                   movel     arbprd        wtpmprd
015300050513     c                   movel     arbnzd        wtpmnzd
015400080303     c                   eval      wTPMpkc = ARBpkc
015500080303     c                   eval      wTPMpkf = ARBpkf
015600050513     c                   movel     arbncl        wtpmncl
015700050513     c                   z-add     arbifp        wtpmifp
015800050513     c                   z-add     arbndc        wtpmndc
015900050513     c                   z-add     arbddc        wtpmddc
016000050513     c                   z-add     arbpdc        wtpmpdc
016100050513     c* ultima spunta effettuata
016200080303if  3c                   if        wtrov='S'
016300050513     c                   z-add     brvnfv        wtpmnfv
016400050513     c                   z-add     wdfv          wtpmdfv
016500050513     c                   z-add     brvfgs        wtpmfgs
016600050513     c                   movel     wspg          wtpmspg
016700050513     c                   z-add     brvnpg        wtpmnpg
016800050513     c                   z-add     brvdfs        wtpmdfs
016900050513     c                   z-add     brvhms        wtpmhms
017000080303e   3c                   endif
017100080303      *
017200080303     c                   write     WFTPM000
017300080303      *
017400080303e   2c                   ENDDO
017500080303      *
017600080303e   1c                   ENDDO
017700050516     c
017800080303      *
017900080303      *S T A M P A?
018000080303      *
018100050516     c* stampo testata
018200080303     c                   write     LRP7T1
018300080303     c                   eval      *in90 = *off
018400080303      *?- Ciclo di lettura work-file WFTPM01L?
018500080303     c     *loval        setll     WFTPM000
018600080303      *
018700080303do  1c                   DO        *hival
018800080303      *
018900080303     c                   read      WFTPM000
019000080303     c                   if        %eof(WFTPM01L)
019100080303     c                   leave
019200080303     c                   endif
019300080303     c*
019400050516     c* ..stampo intestazione
019500080303     c                   if        *in90
019600080303     c                   write     LRP7T1
019700050517     c                   seton                                        02
019800080303     c                   eval      *in90 = *off
019900050516     c                   endif
020000080303     c*
020100050516     c* Imposto campi per la stampa
020200050516     c                   clear                   tra01
020300050516     c                   if        wtpmnrs > 0
020400050516     c                   eval      tra01='-'
020500050516     c                   endif
020600050516     c                   movel     wtpmmgs       wmm               2 0
020700050516     c                   move      wtpmmgs       wgg               2 0
020800050516     c                   movel     wgg           stpmgs
020900050516     c                   move      wmm           stpmgs
021000080303     c                   move      wTPMaas       STPaas
021100080303     c*
021200050516     c                   movel     wtpmrsm       stprsm
021300050516     c                   movel     wtpmrsd       stprsd
021400050516     c                   movel     wtpmind       stpind
021500050516     c                   movel     wtpmlod       stplod
021600050516     c                   if        wtpmnzd<>'   '
021700050516     c                   movel     wtpmnzd       stpprd
021800050516     c                   else
021900050516     c                   movel     wtpmprd       stpprd
022000050516     c                   endif
022100080303      *
022200080303     c                   eval      STPpkx  = *hival
022300080303     c                   select
022400080303     c                   when      wTPMpkf > wTPMpkc
022500080303     c                   if        wTPMpkf < STPpkx
022600080303     c                   eval      STPpkx  = wTPMpkf
022700080303     c                   endif
022800080303     c                   when      wTPMpkc < STPpkx
022900080303     c                   eval      STPpkx  = wTPMpkc
023000080303     c                   endsl
023100080303     c*
023200080303     c                   z-add     wTPMpdc       STPpdc
023300050516     C     kapd          CHAIN     fiapd01L                           31
023400080303     C                   MOVEL     APDRSC        STPpdcD
023500080303     c*
023600050516     c                   z-add     wtpmndc       stpndc
023700080303     c*
023800050516     c                   move      wtpmddc       wmmgg             4 0
023900050516     c                   movel     wmmgg         wmm               2 0
024000050516     c                   move      wmmgg         wgg               2 0
024100050516     c                   movel     wgg           stpdts
024200050516     c                   move      wmm           stpdts
024300080303     c*
024400050517     c* Stampo l'ultima spunta effettuata del collo
024500050516     c                   movel     wtpmfgs       stpbrvfgs
024600050516     c                   z-add     wtpmnfv       stpbrvnfv
024700050516     c                   movel     wtpmnpg       stpbrvnpg
024800050516     c                   move      wtpmdfs       wmmgg             4 0
024900050516     c                   movel     wmmgg         wmm               2 0
025000050516     c                   move      wmmgg         wgg               2 0
025100050516     c                   movel     wgg           stpbrvdfs
025200050516     c                   move      wmm           stpbrvdfs
025300050517     C* DECODIFICO LA CATEGORIA SPUNTA
025400050517    2C     wtpmnpg       IFEQ      3
025500050517     C                   MOVEL     'IMA'         stpbrvcat
025600050517     C                   MOVE      Wtpmspg       stpbrvcat
025700050517   X2C                   ELSE
025800050517     C                   MOVEL     '7N'          COD
025900050517     C                   MOVEL(P)  wtpmnpg       KEY
026000050517     C     KTAB          CHAIN     TABEL                              32
026100050517     C  N32              MOVEL     TBLUNI        stpbrvcat
026200050517     C   32              CLEAR                   STpbrvcat
026300050517    2C                   ENDIF
026400080303     C*
026500050517     C                   if        wtpmspe<>savspe
026600050517     c                   seton                                        02
026700050517     c                   eval      savspe=wtpmspe
026800050517     c                   endif
026900050517     c* Se zona segnacollo<> zona bolla evidenzio
027000050517     c                   if        wtpmzns<>wtpmznb
027100050517     c                   seton                                        03
027200050517     c                   endif
027300080303     c*
027400050517     c* Stampo dettaglio
027500080303     c                   write     LRP7D1
027600050517     c                   setoff                                       0203
027700080303      *
027800080303e   1c                   ENDDO
027900080303      *
028000080303      *F I N E?
028100080303      *
028200080303      * stampo dicitura "fine stampa"
028300080303     c                   write     LRP7F1
028400080303      *
028500080303      * CHIUSURA PGM
028600050513     C                   CLEAR                   DSLV53
028700050513     C                   MOVEL     'C'           D53TLA
028800050513     C                   CALL      'FNLV53R'
028900050513     C                   PARM                    DSLV53
029000080303      *
029100080303     c                   eval      *inLR = *on
029200050513     c
029300080303      * Rountine iniziale --------------------------------------------*
029400050513     c     *inzsr        begsr
029500080303      *
029600080303     C     *ENTRY        PLIST
029700080303     C                   PARM                    KPJBA
029800080303     c                   movel     kpjbu         fnlrp7ds
029900080303      *
030000080303      * impostazione campi fissi
030100080303     C                   Z-ADD     1             CODUT             1 0
030200080303     c                   movel     'A'           apdtip
030300080303      *
030400080303      * reperimento dati dell'utente
030500080303     c                   exsr      DatiJob
030600080303      *
030700080303      * carico pistole valide, spunte reali
030800050513     C                   Z-ADD     1             YY                4 0
030900050513     C                   MOVEL     '7J'          COD
031000050513     C     KTAB2         CHAIN     TABEL00F                           31
031100050513     C*
031200050513     C     *IN31         DOWEQ     *OFF
031300050513     C                   MOVEL     TBLUNI        DS7J
031400050513     C     §7JRPS        IFEQ      'L'
031500050513     C                   MOVEL     TBLKEY        NPS(yy)
031600050513     C                   ADD       1             yy
031700050513     c                   endif
031800080303     c*
031900050513     C     KTAB2         READE     TABEL00F                               31
032000050513     C                   ENDDO
032100080303      *
032200080303      * Carico le sone del tipo richiesto
032300050516     c                   clear                   xx                4 0
032400050516     c                   movel     ip7fil        alffil            3
032500050516     c                   movel     ip7fil        wpo
032600050516     c                   movel(P)  '03'          cod
032700050516     c                   movel(P)  ip7fil        key
032800050516     c     ktab          setll     tabel
032900050516     c     ktab2         reade     tabel
033000050516     c                   dow       not %eof(tabel00f) and  wpo=alffil
033100050516     c                   if        tblflg=' '
033200050516     c                   movel     tblkey        wpo               3
033300050516     c                   if        wpo=alffil
033400050516     c                   movel     tbluni        ds03
033500050516     c                   if        (ip7tpm='M' and §03tpm='M') or
033600050516     c                             (ip7tpm<>'M' and §03tpm<>'M')
033700050516     c                   add       1             xx
033800050516     c                   movel     tblkey        w005a             5
033900050516     c                   move      w005a         zone(xx)
034000050516     c                   endif
034100050516     c                   endif
034200050516     c                   endif
034300050516     c     ktab2         reade     tabel
034400050516     c                   enddo
034500080303      *
034600080303      * IMPOSTO CAMPI PER TESTATA STAMPA
034700080303      *
034800080303      * nome pgm
034900080303     c                   eval      STPpgm = SDSpgm
035000080303      *
035100080303      * decodifica filiale
035200080303     c     IP7fil        chain     AZORG
035300080303     c                   if             %found(AZORG01L)
035400080303     c                             and  ORGfva <> *blanks
035500080303     c                   eval      STPpouD = ORGdes
035600080303     c                   endif
035700080303      *
035800080303      * inverto la data distinta
035900080303     c     *iso          move      ip7dat        data_eur
036000080303     c                   movel     data_eur      stpdata
036100080303      *
036200080303      * Se numero dal <> 1 o numero al <> 9999999 li stampo in testata
036300050517     c                   if        ip7nrd>1 or ip7nra<9999999
036400050516     c                   seton                                        01
036500050516     c                   endif
036600080303      *
036700050516     c                   if        ip7tpm='M'
036800050516     c                   eval      STPTPM=CMESSAG
036900050516     c                   eval      STPtipdis='  Parcel   '
037000050516     C                   ELSE
037100050516     c                   eval      STPTPM=Cparcel
037200050516     c                   eval      STPtipdis='Messaggerie'
037300050516     C                   endif
037400050513     c*
037500050513     c     ktab2         klist
037600050513     c                   kfld                    codut
037700050513     c                   kfld                    cod
037800050516     c     ktab          klist
037900050516     c                   kfld                    codut
038000050516     c                   kfld                    cod
038100050516     c                   kfld                    key
038200050513     c     karb          klist
038300050513     c                   kfld                    kfgs
038400050513     c                   kfld                    ip7nrd
038500050513     c     kfvv          klist
038600050513     c                   kfld                    knpg
038700050513     c                   kfld                    knfv
038800050513     c                   kfld                    kfvvfgs
038900050513     c     kart          klist
039000050513     c                   kfld                    arbaas
039100050513     c                   kfld                    arblnp
039200050513     c                   kfld                    arbnrs
039300050513     c                   kfld                    arbnsp
039400050513     c     kbrv          klist
039500050513     c                   kfld                    artfls
039600050513     c                   kfld                    artlna
039700050513     c                   kfld                    artnrs
039800050513     c                   kfld                    artnsc
039900050513     C     Kapd          KLIST
040000050513     C                   KFLD                    apdtip
040100050516     C                   KFLD                    wtpmpdc
040200050513     c                   endsr
040300080303
040400080303      *---------------------------------------------------------------*
040500080303      *?Reperimento dati del job (Utente/Operativi)                  ?*
040600080303      *---------------------------------------------------------------*
040700080303     c     DatiJob       BEGSR
040800080303      *
040900080303     c     *dtaara       define    §azute        azuteds
041000080303     c     *dtaara       define    §datiute      ddatiute
041100080303      *
041200080303     c                   in(E)     *dtaara
041300080303     c                   IF        %Error or RSUT = *blanks
041400080303     c                   clear                   Tibs34Ds
041500080303     c                   call      'TIBS34R'
041600080303     c                   parm                    Tibs34Ds
041700080303     c                   in        *dtaara
041800080303     c                   ENDIF
041900080303      *
042000080303     c                   ENDSR
042100050513     C**************************************************************************
042200050513     C* cerco spunta del collo in qualsiasi categoria
042300050513     C**************************************************************************
042400050513     C     RICSPU        BEGSR
042500050513     C                   CLEAR                   Wimm
042600050513     C                   CLEAR                   WSAVB
042700050513     C                   CLEAR                   WDFV
042800050513     C                   CLEAR                   WTROV
042900050513     C                   CLEAR                   WSPG
043000070122RM*  C     Kbrv          CHAIN     FNBRV07l                           33
043100050513RM*  C*
043200050513RM* 1C     *IN33         DOWEQ     *OFF
043300050513     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
043400050513     c                   clear                   dslv53
043500050513     C                   z-add     brvnFV        D53NFV
043600050513     C                   MOVEL     brvNPG        D53NPG
043700050513     C                   MOVEL     brvFGS        D53FGS
043800050513     C                   MOVEL     brvfle        D53flf
043900050513     C                   CALL      'FNLV53R'
044000050513     C                   PARM                    DSLV53
044100050513     C* ERRORE
044200050513    2C     D53ERR        IFeq      ' '
044300070122     c                   movel     brvdfs        brvimm           14 0
044400050513     c                   move      brvhms        brvimm
044500050513     c* Imposto zoan segnacollo se pistola reale
044600050513     c                   movel     brvnps        alfnps            2
044700050513     c     alfnps        lookup    nps                                    35
044800050513     c                   if        *in35
044900050513     c                   eval      segnazona=brvznc
045000050513     c                   endif
045100050513     c
045200050513    3c                   select
045300050513     c* Se maggiore memorizzo
045400050513     c                   when      d53dfv>wdfv
045500050513     C                   MOVEL     RECBRV        WSAVB
045600070123     C                   MOVEL     brvimm        Wimm
045700050513     C                   MOVEL     d53DFV        WDFV
045800050513     C                   MOVEL     d53spg        WSPG
045900050513     C                   MOVEL     'S'           WTROV             1
046000050513     c
046100050513     c* Se uguale tengo la data ora immissione + alta
046200050513     c                   when      d53dfv=wdfv  and brvimm>wimm
046300070123     C                   MOVEL     brvimm        Wimm
046400050513     C                   MOVEL     RECBRV        WSAVB
046500050513     C                   MOVEL     d53DFV        WDFV
046600050513     C                   MOVEL     d53spg        WSPG
046700050513     C                   MOVEL     'S'           WTROV             1
046800050513     C                   eval      wimm=brvimm
046900050513     c* Se stessa data e ora immissione, tengo cat 3 o 4
047000050513     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2 and
047100050513     c                                                             brvnpg<>6
047200070123     C                   MOVEL     brvimm        Wimm
047300050513     C                   MOVEL     RECBRV        WSAVB
047400050513     C                   MOVEL     d53DFV        WDFV
047500050513     C                   MOVEL     d53spg        WSPG
047600050513     C                   MOVEL     'S'           WTROV             1
047700050513     C                   eval      wimm=brvimm
047800050513    3c                   endsl
047900050513    2c                   endif
048000050513     c
048100070122RM*  C     Kbrv          reade     FNBRV07l                               33
048200050513    1c                   enddo
048300050513     c
048400050513     C     WTROV         IFEQ      'S'
048500050513     C                   MOVEL     WSAVB         RECBRV
048600050513     C                   ENDIF
048700050513     c                   endsr
