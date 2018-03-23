000100990507     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020412     H*PARMS ACTGRP(YFICN51)
000300030827     H/TITLE  contabilizzazione conteggi aut/coop/aff/defl.
000400951009?     *--------------------------------------------------------------*
000500020404      * ficn51R                                                      *
000600951009      *     *----------------------------------------------*         *
000700020404      *         CONTABILIZZAZIONE conteggi                           *
000800951009      *     *---------------------------------------------*          *
000900951009      *                                                              *
001000951009?     *--------------------------------------------------------------*
001100980916      * KA - SELEZIONA TUTTO                                        *
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009      * KL - RITORNO                                                 *
001500951009?     *--------------------------------------------------------------*
001600951009      * 20 - SFLDSP                                                  *
001700951009      * 21 - SFLDSPCTL/CLEAR                                         *
001800951009      * 22 - SFLNXTCHG                                               *
001900951009      * 28 - Visualizzazione messaggio di errore                     *
002000951011      * 29 - READC su subfile                                        *
002100020404      * 30 - READ e CHAIN su fiftt01L                                *
002200951011      * 32 - Indicatore di comodo per READ/CHAIN/SCAN/LOKUP          *
002300951009?     *--------------------------------------------------------------*
002301080103     Ffiftt06L  iF   E           K DISK
002302080103     Ffictt06L  iF   E           K DISK
002303080103     Ffiatt06L  iF   E           K DISK
002304080103     Ffiott06L  iF   E           K DISK
002800020724     Ftntbe01l  if   E           K DISK
002900030213     Ftabel00f  if   E           K DISK
003500951011     F*--------
003600020423     Fanuni01l  IF   E           K DISK
003700020423     Fansog01l  IF   E           K DISK
003800020405     Fanfrn01l  IF   E           K DISK
003900020405     Fanrco01l  IF   E           K DISK
004000020514     Ftntlz01l  IF   E           K DISK
004100020708     Fanope01l  IF   E           K DISK
004200020708     Fandft01l  IF   E           K DISK
004300020405     F*--------
004400080103     Ftnvrficn1dCF   E             WORKSTN
004500980911     F                                     SFILE(y350S01:NRR)
004600030204     F                                     SFILE(y350S02:NRR2)
004700951009     D*---------------------------------------------------------------*
004800951009     D* SCHIERA
004900951009     D*---------------------------------------------------------------*
005000951011      * Schiere per errori e stampa
005100030620     D ERR             S             70    DIM(3) CTDATA PERRCD(1)              Errori
005200030204     D tipf            S              4    DIM(10) inz                          Errori
005300030204     D impf            S             13  2 DIM(10) inz                          Errori
005400030204     D impt            S             13  2 DIM(10) inz                          Errori
005500020411     D* Reperimento nome PGM
005600020411     D STATUS         SDS           333
005700020411     D  nompgm           *PROC
005800020411     D*---------------------------------------------------------------*
005900020411     D* CAMPI INTERNI
006000020411     D*---------------------------------------------------------------*
006100020411     D dataiso         s               d   datfmt(*iso)
006200020411     D dataeur         s               d   datfmt(*eur)
006300020409     D WDTFINE         S               D
006600020418     D ksys            S              3  0 inz
007300040914     D impmin          S                   like(§blcmin)
007400000204     D* ------ Definizioni parametri standard
007500020411     D Esito           S              1
007600020411    >D Unita           S              8A
007700020411    >D Ctb             S              2A
007800020411    >D Keygest         S              8A
007900020411    >D Tipdat          S              1A
008000020411    >D Tpregz          S              1A
008100020411    >D DtCoan          S             10A
008200020411    >D DtRif           S             10A
008300020411    >D Operazione      S              2A
008400020411     D GesMsg          S              1
008500020411    >D Reper           S              1
008600020411     D XnmRic          S              1
008700020411     D XnmSoc          S              3
008800020411     D XnmUni          S              8
008900020411     D XnmCtb          S              2
009000020411     D XnmKey          S              8
009100020411     D XnmSer          S              4
009200020411     D XnmCmt          S              1
009300020411     D XnmDat          S               D
009400020411     D XnmIva          S              1
009500020411     D XnmReg          S              1
009600020415     D XnmDag          S               D
009700020411     D XnmErr          S              1
009800020411     D XnmNum          S              9  0
009900020411     D XnmAut          S              1
010000020418     D imponibile      S             18  2
010100030205     D impcom          S             18  2
010200030204     D totimpt         S             18  2
010300030206     D totimpf         S             18  2
010400030206     D totimp          S             18  2
010500030206     D diffe           S             13  2
010600030206     D impiva          S             13  2
010700020527     d v1tips          s                   like(v1tipf)
010800030206     d v1tip           s                   like(v1tipf)
011000030128     d commes          s              2
011100030416     d com03           s              3
011200030204     d x               s              3  0
011300030204     d xy              s              3  0
011400030204     d xyz             s              3  0
011500030620     d noconf          s              1    inz
011600951009?     *--------------------------------------------------------------*
011700951009?     *  DS                                                          *
011800951009?     *--------------------------------------------------------------*
011900020606     D dftt01        e DS
012000020606     D xatbds        e DS
012100980911     D soc001        E DS                  EXTNAME(xsoc001ds)
012200980911     D xsocds          DS          1000
012300030213     D ds1z          e ds                  inz
012400031121     D dcaf          e ds                  inz
012500040914     D dBLC          e ds                  inz
012600040914     D dBLD          e ds                  inz
012700040914     D dBLO          e ds                  inz
012800020411     D* libro iva acquisti
012900020724     D dyiv          e ds                  inz
013000020409     D* Parametri per reperimento data inizio esercizio
013100020411    >D x10ese        E DS                  EXTNAME(x10eseDS)
013200951009     D KPJBA         E DS
013300020411?     *  Parametri registrazioni contabili
013400980914     D ndc071ds      E DS
013500020411     D NDC050DS      E DS                  INZ
013600020411     D  RET050       E                     INZ('0')
013700020411     D  OPR050       E                     INZ('0')
013800020411     D  ERR050       E                     INZ('0')
013900020514     D dtlz01        E DS
014000020411     d* parametri ricevuti
014100020404     D ficn50ds      E DS
014200020415     D salds         E DS                  extname(ficn50ds) prefix(£)
014300020411?     *  Tabella Y4Z
014400020408     D angy4z        E DS                  extname(angy4zds)
014500020411?     *  Tabella IVA
014600020411     DTABG48         E DS                  EXTNAME(ANGG48DS)
014700020411     D X06DS         E DS                  EXTNAME(X06G48DS)
014800020411     D  X06SOC       E                     EXTFLD(X06SOCIETA)
014900020411     D  X06CIV       E                     EXTFLD(X06CDIVA)
015000020411     D  X06DAT       E                     EXTFLD(X06DATA)
015100020411     D  X06LIN       E                     EXTFLD(X06LINGUA)
015200020415     d* cancella NDBH*
015300020415     D ycodel1ds     e DS
015400020411     c* SQL
015500020405     D WrkSqlCmd       S           1024
015600030827     c* sfl 1 aut.
015700020405     D fiftt           DS
015800020405     d cdfs                                like(fttcdf)
015900020405     d tots                                like(ftttim)
016000030827     c* sfl 1 coop accorpate
016100030128     D fictt           DS
016200030128     d cdfsc                               like(cttcdf)
016300030128     d totsc                               like(cttitc)
016400030827     c* sfl 1 coop non accorpate
016500030128     D fictttsr        DS
016600030128     d cdfsc1                              like(cttcdf)
016700030128     d totsc1                              like(cttitc)
016800030128     d tsrsc1                              like(ctttsr)
016900030827     c* sfl 1 aff/defl
017000030827     D fiatt           DS
017100030827     d cdfsa                               like(attcdf)
017200030827     d totsa                               like(attimpp)
017300031121     c* sfl 1 prestaz. occasionali
017400031121     D fiott           DS
017500031121     d cdfso                               like(ottcdf)
017600031121     d totso                               like(otttim)
017700030827     c* sfl 2 aut.
017800030204     D fiftt2          DS
017900030204     d tots2                               like(ftttim)
018000030204     d meses2                         2
018100030827     c* sfl 2 coop
018200030204     D fictt2          DS
018300030204     d tsrsc2                              like(ctttsr)
018400030204     d totsc2                              like(cttitc)
018500030204     d mesesc2                        2
018600030827     c* sfl 2 aff/defl
018700030827     D fiatt2          DS
018800031007     d*tsrsa2                              like(attvad)
018900030827     d totsa2                              like(attimpp)
019000030827     d mesesa2                        2
019100031121     c* sfl 2 prestaz. occasionali
019200031121     D fiott2          DS
019300031121     d totso2                              like(otttim)
019400031121     d meseso2                        2
019500990507     D*  Prototipazione delle procedure
019600990507     D/COPY *LIBL/SRCCPY,PJX002PR
019700951009?     *--------------------------------------------------------------*
019800951009?     *  CICLO PRINCIPALE                                            *
019900951009?     *--------------------------------------------------------------*
020000951009      *  Impostazione parametri prima videata
020100951010     C                   EXSR      CARSFL
020200951009      *  Loop gestione videata
020300951009     C     WFINE         DOUEQ     'S'
020400000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
020500000426     C     WTPVID        CASEQ     '2'           GESD02                         sfl vuoto
020600951009     C                   END
020700951012     C                   END
020800980312     C*
020900020419     C                   MOVEL(p)  ficn50ds      KPJBU
021000951009     C                   SETON                                        LR
021100951009?     *--------------------------------------------------------------*
021200951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
021300951009?     *--------------------------------------------------------------*
021400951012     C     CARSFL        BEGSR
021500951009      *
021600951009      *  Pulisco dati SFL
021700020405     C                   SETOFF                                       2120
021800980911     C                   WRITE     y350C01
021900951009     C                   SETON                                        2021
022000951009     C                   MOVEL     '1'           WTPVID
022100020415     C                   Z-ADD     0             NRR               6 0
022200020409     c* questo sql mi restituisce per ogni fornitore e mese di competenza
022300020412     c* la sommatoria dei conteggi.
022400020412     c*
022500020412     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
022600020412     c* precedente ed è possibile contabilizzare ancora i ratei,
022700020412     c* altrimenti se il conteggio è dell'anno precedente
022800020412     c* ma non si possono più registrare ratei sarà forzato a 1
022900020412     c* Per i conteggi dell'anno in corso il mese di competenza è
023000020412     c* quello del conteggio
023100020412     c*
023200031121     c* Lettura fiftt00f/fictt00f/fiatt00f/fiott00f
023300020405     C/EXEC SQL
023400020405     C+ PREPARE S1 FROM :WrkSqlCmd
023500020405     C/END-EXEC
023600020405
023700020405     C/EXEC SQL
023800020405     C+ DECLARE A1 CURSOR FOR S1
023900020405     C/END-EXEC
024000020405
024100020405     C/EXEC SQL
024200020405     C+ OPEN A1
024300020405     C/END-EXEC
024400020405
024500020405     C                   DOU       SqlCod <> 0
024600030827     C                   SELECT
024700031121     c* prestaz. occasionali
024800031121     c                   when      f50cau <> ' '
024900031121     C/EXEC SQL
025000031121     C+ FETCH NEXT FROM A1 INTO :fiott
025100031121     C/END-EXEC
025200031121     c* autotrasportatori
025300031121     c                   when      f50tip = 'A'
025400020405     C/EXEC SQL
025500020405     C+ FETCH NEXT FROM A1 INTO :fiftt
025600020405     C/END-EXEC
025700030128     c* cooperative      senza tipo servizio
025800030827     c                   when      f50tip = 'C'
025900030128     c                   if        f50tsr = 'S'
026000030128     C/EXEC SQL
026100030128     C+ FETCH NEXT FROM A1 INTO :fictt
026200030128     C/END-EXEC
026300030128     c                   else
026400030128     C/EXEC SQL
026500030128     C+ FETCH NEXT FROM A1 INTO :fictttsr
026600030128     C/END-EXEC
026700030128     c* cooperative      con tipo servizio
026800030128     c                   end
026900030827     c* aff/defl.
027000030827     c                   when      f50tip = 'D'
027100030827     C/EXEC SQL
027200030827     C+ FETCH NEXT FROM A1 INTO :fiatt
027300030827     C/END-EXEC
027400030827     c                   endsl
027500020405     c*
027600020405     C                   SELECT
027700020405     C                   WHEN      SqlCod = 0
027800951010     C                   EXSR      CARTRN
027900020405     C                   WHEN      SqlCod = 100
028000020405     C                   WHEN      SqlCod <> 0
028100020417      * Forzo la stampa del JOBLOG.
028200020417     C                   CALL      'X66CHGJOB'
028300020405     C                   ENDSL
028400020404     C                   ENDDO
028500020417     C/EXEC SQL
028600020417     C+ CLOSE A1
028700020417     C/END-EXEC
028800020405      *  Imposto dati SFLCTL
028900020405     C                   EXSR      INZ01
029000951009      *  Se non ho dati da caricare emetto videata vuota
029100951009     C     NRR           IFEQ      0
029200020405     C                   MOVEL     '2'           WTPVID
029300951009     C                   END
029400951009     C*
029500951009     C                   ENDSR
029600951013?     *--------------------------------------------------------------*
029700020415?     *  INZ01: Inizializzazione videata                             *
029800951013?     *--------------------------------------------------------------*
029900951013     C     INZ01         BEGSR
030000951013      *
030100000426      *  Inizializzo testata sfl in base a quanto
030200000426      *  passato dal programma chiamante
030300951013     C                   MOVEL     '1'           WTPVID
030400951016     C                   MOVEL     NOMPGM        V1CPGM
030500020405     C                   MOVEL     F50DP1        V1DPDR
030600020405     C                   move      f50PD1        V1CPDR
030700020405     C                   MOVEL     F50DP2        V2DPDR
030800020405     C                   move      f50PD2        V2CPDR
030900020405     C                   Z-ADD     f50NFT        V1CNFT
031000020423     c     kuni          chain     anuni01l
031100020423     c                   if        %found
031200020423     c                   movel     unidesbrev    v1cdun
031300020423     c                   else
031400020423     c                   movel     *blanks       v1cdun
031500020423     c                   end
031600020405     C                   if        f50DTI > 0
031700020405     C                   move      f50DTI        dataiso
031800000911     C                   else
031900000911     C                   move      '0001-01-01'  dataiso
032000000911     C                   end
032100980911     C                   MOVEL     dataiso       dataeur
032200980911     C                   movel     dataeur       V1CDTI
032300020405     C                   if        f50DTF > 0
032400020405     C                   movel     f50DTF        dataiso
032500000911     C                   else
032600000911     C                   move      '0001-01-01'  dataiso
032700000911     C                   end
032800980911     C                   MOVEL     dataiso       dataeur
032900980914     C                   movel     dataeur       V1CDTF
033000020405     C                   if        f50DFT > 0
033100020405     C                   movel     f50DFT        dataiso
033200000911     C                   else
033300000911     C                   move      '0001-01-01'  dataiso
033400000911     C                   end
033500980911     C                   MOVEL     dataiso       dataeur
033600980911     C                   movel     dataeur       V1CDFT
033700020405     C                   if        f50DPR > 0
033800020405     C                   movel     f50DPR        dataiso
033900000911     C                   else
034000000911     C                   move      '0001-01-01'  dataiso
034100000911     C                   end
034200980911     C                   MOVEL     dataiso       dataeur
034300980911     C                   movel     dataeur       V1CDPR
034400951013      *
034500951013     C                   ENDSR
034600951010?     *--------------------------------------------------------------*
034700020405?     *  CARTRN: Gestione caricamento dati                           *
034800951010?     *--------------------------------------------------------------*
034900951010     C     CARTRN        BEGSR
035000030827     c                   select
035100031121     c* prestazioni occasionali
035200031121     c                   when      f50cau <> ' '
035300031121     C                   MOVE      cdfso         V1Cfor
035400031121     C                   Z-ADD     Totso         v1cico
035500031121     c* autotrasportatore
035600031121     c                   when      f50tip = 'A'
035700031121     C                   MOVE      cdfs          V1Cfor
035800031121     C                   Z-ADD     Tots          v1cico
035900030128     c* cooperativa  accorpam.
036000030827     c                   when      f50tip = 'C'
036100030128     C                   if        f50tsr = 'S'
036200030128     C                   Z-ADD     Totsc         v1cico
036300030129     C                   MOVE      cdfsc         V1Cfor
036400030204     c                   clear                   v1ctsr
036500030213     c                   clear                   v1dtsr
036600030128     c                   else
036700030128     c* cooperativa  no accorpam.
036800030128     C                   Z-ADD     Totsc1        v1cico
036900030129     C                   MOVE      cdfsc1        V1Cfor
037000030128     C                   MOVEL     tsrsc1        V1Ctsr
037100030213     C                   MOVEL(p)  tsrsc1        tblkey
037200030213     c* decodifico tipo servizio
037300030213     c     ktab          chain     tabel00f
037400030213     c                   if        %found(tabel00f)
037500030213     c                   movel     tbluni        ds1z
037600030213     c                   movel     §1zdes        v1dtsr
037700030213     c                   else
037800030213     c                   clear                   v1dtsr
037900030213     c                   end
038000030128     c                   end
038100030827     c* aff/defl.
038200030827     c                   when      f50tip = 'D'
038300030827     C                   MOVE      cdfsa         V1Cfor
038400030827     C                   Z-ADD     Totsa         v1cico
038500030827     c                   endsl
038600951010     C                   MOVEL     *BLANKS       V1CSCE
038700021015     c* 23 ind. che protegge il campo di scelta per i fornitori senza p.IVA
038800021015     c                   setoff                                       23
038900020405     C     Kfrn          CHAIN     anfrn01l                           30
039000020405     C  N30Krco          CHAIN     anrco01l                           30
039100020423     c                   if        not *in30 and rcounita <> f50uni
039200020423     c                   seton                                        30
039300020423     c                   end
039400030128     c                   if        not *in30 and f50tip = 'A'
039500030128     C                   if        rcoctgan02 <> 'N   ' and
039600020604     C                             rcoctgan02 <> 'I   ' and
039700020604     C                             rcoctgan02 <> 'M   '
039800020604     C                   SETON                                        30
039900020604     c                   end
040000030128     c                   end
040100020514     C  N30rcosogg       CHAIN     ansog01l                           30
040200020514     C* escludo i fornitore che non fanno autofatturazione
040300020514     c                   if        not *in30
040400030827     c                   select
040500030827     c* autotrasportatori
040600030827     c                   when      f50tip = 'A'
040700030128     c                   movel(p)  'P'           tlztip
040800030827     c                   when      f50tip = 'C'
040900030204     c                   movel(p)  'C'           tlztip
041000030827     c                   when      f50tip = 'D'
041100030827     c                   movel(p)  'D'           tlztip
041200030827     c                   endsl
041300030128     c                   move      v1cfor        tlzpdr
041400020514     C     ktlz          CHAIN     tntlz01l                           30
041500020514     c                   if        not *in30
041600020514     c                   movel     tlzflo        dtlz01
041700020515     c                   if        f50nft = 0
041800020515     c                   if        §tlzfl1 = 'S'
041900020514     c                   seton                                        30
042000020514     c                   end
042100020515     c                   else
042200020515     c                   if        §tlzfl1 <>'S'
042300020515     c                   seton                                        30
042400020515     c                   end
042500020515     c                   end
042600030205     c                   else
042700030827     c* se non trova il record di tntlz per la coop o x aff/defl. no errore
042800030827     c* esiste solo se la coop o aff/defl. non è o non è stata in autofat.
042900030827     c                   if        tlztip = 'C' or tlztip = 'D'
043000030205     c                   setoff                                       30
043100030205     c                   end
043200020514     c                   end
043300020514     c                   end
043400020514     c*
043500020423     c                   if        not *in30
043600021015     c     sogpartiva    comp      *blanks                                23
043700021015     c                   if        *in23
043800021015     C                   SETON                                        28
043900021016     c                   move      '1'           ind23
044000021015     C                   MOVEL     ERR(2)        $MSG
044100021016     c                   else
044200021016     c                   move      '0'           ind23
044300021015     c                   end
044400020405     C                   MOVEL     sogdes        v1dksc
044500030827     c                   select
044600031121     c                   when      f50cau <> ' '
044700031121     C                   MOVEL     §caftab       v1tipf
044800031121     c                   when      f50tip = 'A'
044900031121     C                   MOVEL     rcoctgan02    v1tipf
045000030827     c                   when      f50tip = 'C'
045100030203     c                   if        f50tsr = 'N'
045200030203     c                   movel     tsrsc1        v1tipf
045300030203     c                   else
045400030203     c                   movel     'X'           v1tipf
045500030203     c                   end
045600030827     c                   when      f50tip = 'D'
045700030827     C                   MOVEL     'D'           v1tipf
045800030827     c                   endsl
045900020405     c                   if        frncdiva <> *blanks
046000020405     c                   movel     frncdiva      v1civc
046100020405     c                   else
046200020405     c                   movel     *blanks       v1civc
046300020405     c                   end
046400020509     c* fornitore di riferimento per numerazione fattura
046500020509     c                   movel     FRNFORFATT    v1cfat
046600020509     c                   if        v1cfat = *blanks
046700020509     c                   movel     v1cfor        v1cfat
046800020509     c                   end
046900020509     c                   movel     rcounita      v1cuni
047000030203     c                   if        f50cdp <> *blanks
047100030203     c                   MOVEL     f50cdp        v1ccdp
047200030203     c                   else
047300030203     c                   MOVEL     FRNCONDPAG    v1ccdp
047400030203     c                   end
047500030620     c* controllo se esistono dei conteggi non confermati in tal cas
047600030827     c* segnalo l'anmalia ma non blocco solo per coop e aff/defl
047700040112     c* NO X PRESTAZIONI RESIDUALI
047800040112     c                   if        f50cau = ' '
047900030827     c                   select
048000030827     c                   when      f50tip = 'C'
048100030620     c                   exsr      ctrctt
048200030827     c                   when      f50tip = 'D'
048300030827     c                   exsr      ctratt
048400030827     c                   endsl
048500030827     c                   if        noconf = '1'
048600030827     c                   seton                                        28
048700030827     c                   if        f50tip = 'C'
048800030827     C                   MOVEL     ERR(3)        $MSG
048900030827     c                   else
049000030827     C                   MOVEL     ERR(3)        com52            52
049100030827     c                   eval      $msg = com52 + '(FOR.' + attcdf + ')'
049200030827     c                   end
049300030827     c                   end
049400040112     c                   end
049500020408     c*
049600020405     C                   ADD       1             NRR
049700020405     C                   WRITE     y350S01
049800020423     c                   end
049900951010      *
050000951010     C                   ENDSR
050100951009?     *--------------------------------------------------------------*
050200020405?     *  srcmd   Gestione comandi                                    *
050300951009?     *--------------------------------------------------------------*
050400020405     C     srcmd         BEGSR
050500020415     c                   clear                   f50cmd
050600020405      *  Fine Lavoro
050700020405     C     *INKC         IFEQ      '1'
050800020405     C                   MOVEL     'S'           WFINE
050900020405     C                   MOVEL     '03'          f50CMD
051000020405     C                   END
051100020405      *  Ritorno
051200020405     C     *INKL         IFEQ      '1'
051300020405     C                   MOVEL     'S'           WFINE
051400020405     C                   MOVEL     '12'          f50CMD
051500020405     C                   END
051600951010      *
051700020405     C                   ENDSR
051800020405?     *--------------------------------------------------------------*
051900020405?     *  GESS01: Gestione prima videata                              *
052000020405?     *--------------------------------------------------------------*
052100020405     C     GESS01        BEGSR
052200020405      *
052300980914     C                   WRITE     y350T01
052400980914     C                   EXFMT     y350C01
052500020405     c* gestione comandi
052600020405     c                   exsr      srcmd
052700020415     c                   if        f50cmd <> *blanks
052800020415     c                   goto      finvd1
052900020415     c                   end
053000951009      *  Controlli
053100951009     C                   EXSR      CTR01
053200020405     C   28              GOTO      FINVD1
053300020408      *  CONFERMA
053400951013     C     *INKF         IFEQ      '1'
053500000204      * Richiamo pgm prima nota fatture
053600951013     C                   EXSR      CALPNF
054000980911     C                   END                                                    V3CTOT = y35FAT
054100951009      *
054200951009     C     FINVD1        ENDSR
054300951010?     *--------------------------------------------------------------*
054400951010?     *  GESD02: Gestione seconda videata                            *
054500951010?     *--------------------------------------------------------------*
054600951010     C     GESD02        BEGSR
054700951010      *
054800980914     C                   EXFMT     y350D02
054900020405     c* gestione comandi
055000020405     c                   exsr      srcmd
055100951010      *
055200951010     C                   ENDSR
055300951009?     *--------------------------------------------------------------*
055400951009?     *  CTR01: Controlli prima videata                              *
055500951009?     *--------------------------------------------------------------*
055600951009     C     CTR01         BEGSR
055700021016     c                   setoff                                       28
055800951009      *
055900020405     C                   Z-ADD     0             V1CITT
056000951013     C                   MOVEL     'N'           WSELTR            1
056100020610     C                   MOVEL     *BLANKS       $MSG
056200020415      *  Controllo se devo selezionare tutte i fornitori
056300951010     C     *INKA         IFEQ      '1'
056400951010     C                   Z-ADD     1             NRR
056500980911     C     NRR           CHAIN     y350S01                            29
056600951010     C     *IN29         DOWEQ     '0'
056700021016     c* non scelgo i record con ind23 = 1 xchè manca p.iva
056800021016     c                   if        ind23 = '0'
056900951013     C                   MOVEL     'S'           WSELTR            1
057000951010     C                   MOVEL     '1'           V1CSCE
057100951010     C                   ADD       V1CICO        V1CITT
057200951010     C                   SETON                                        22
057300021016     c                   movea     ind23         *in(23)
057400980911     C                   UPDATE    y350S01
057500951010     C                   SETOFF                                       22
057600021016     c                   end
057700951010     C                   ADD       1             NRR
057800980911     C     NRR           CHAIN     y350S01                            29
057900011017     C                   ENDdo
058000951010     C                   ELSE
058100951010      *  Solo i selezionati
058200980911     C                   READC     y350S01                                29
058300951010     C     *IN29         DOWEQ     '0'
058400020405     C                   select
058500030128     C                   when      v1csce = 'D'
058600020405     c                   movel     ficn50ds      salds
058700020528     c                   movel     v1cfor        f50pd2
058800020417     c                   movel     v1dksc        f50dp2
058900020415     c                   movel     v1cfor        f50pd1
059000020415     c                   movel     v1dksc        f50dp1
059100020528     c*
059200020417     c                   movel(p)  ficn50ds      kpjbu
059800020417     c                   movel     kpjbu         ficn50ds
059900020415     c                   movel     f50cmd        £f50cmd
060000020408     c                   movel     salds         ficn50ds
060100020408     c                   movel     *blanks       v1csce
060200020610     C                   SETOFF                                       22
060300021016     c                   movea     ind23         *in(23)
060400020408     C                   UPDATE    y350S01
060500020415     c                   if        f50cmd = '03'
060600020415     c                   move      *on           *inkc
060700020405     c                   exsr      srcmd
060800020415     c                   goto      finct1
060900020415     c                   end
061000020405     c*
061100020405     C     V1CSCE        wheneq    '1'
061200020405     C                   MOVEL     'S'           WSELTR            1
061300951010     C                   ADD       V1CICO        V1CITT
061400951010     C                   SETON                                        22
061500021016     c                   movea     ind23         *in(23)
061600980911     C                   UPDATE    y350S01
061700951010     C                   SETOFF                                       22
061800020610     C                   OTHER
061900020610     C                   SETOFF                                       22
062000021016     c                   movea     ind23         *in(23)
062100020610     C                   UPDATE    y350S01
062200020405     C                   ENDsl
062300980911     C                   READC     Y350S01                                29
062400011017     C                   ENDdo
062500951010     C                   END
062600020415      *  Nessun fornitore selezionato
062700020408     C     WSELTR        IFeq      'N'
062800020405     C     *INKF         andeq     '1'
062900951013     C                   SETON                                        28
063000020415     C                   MOVEL     ERR(1)        $MSG
063100951013     C                   GOTO      FINCT1
063200951013     C                   END
063300951010      *
063400951010     C     FINCT1        ENDSR
066600020409?     *--------------------------------------------------------------*
066700020409      * Chiamata a XNMR.
066800020409?     *--------------------------------------------------------------*
066900020409     C     CalXnmr       BEGSR
067000020409      *
067100020409      *
067200020409     C                   CALLB     'XNMR'
067300020409     C                   PARM                    XnmRic
067400020412     C                   PARM      xscsoc        Xnmsoc
067500020409     C                   PARM                    XnmUni
067600020409     C                   PARM                    XnmCtb
067700020409     C                   PARM                    XnmKey
067800020409     C                   PARM                    XnmSer
067900020409     C                   PARM                    XnmCmt
068000020409     C                   PARM                    XnmDat
068100020409     C                   PARM                    XnmIva
068200020409     C                   PARM                    XnmReg
068300020409     C                   PARM                    XnmErr
068400020409     C                   PARM                    XnmNum
068500020409     C                   PARM                    XnmAut
068600020409     C                   PARM                    XnmDag                         opzionale
068700020409      *
068800020409     C                   ENDSR
068900951011?     *--------------------------------------------------------------*
069000951011?     *  CALPNF: Carico subfile e richiamo prima nota fatture        *
069100951011?     *--------------------------------------------------------------*
069200951012     C     CALPNF        BEGSR
069300980914      *
069400020408     C                   READC     y350S01                                29
069500020408     C     *IN29         DOWEQ     '0'
069600020408     C     V1CSCE        IFEQ      '1'
069700040914     c* se importo > = importo mnimo da fatturare preso dalla tab. BLC/O/D
069800080103     C     V1Cico        andge     impmin
069900030203     c* scrivo il sfl di comodo dove memorizzo gli importi per ogni mese
070000030203     c* di competenza
070100030203     c                   exsr      srsfl2
070200030206     c* per ogni tipologia fornitore/servizio aggancio la tabella Y4Z
070300030203     c                   if        v1tipf <> v1tips
070400030206     c                   eval      v1tip = v1tipf
070500020527     c                   exsr      sry4z
070600020527     c                   movel     v1tipf        v1tips
070700020527     c                   end
070800020417     c* imponibile 18,2
070900030206     c                   eval(h)   imponibile= v1cico
071000030206     c* verifico eventuale squadratura di arrotondamento e la metto nel
071100030206     c* costo dell'anno in corso
071200030206     c                   xfoot     impt          totimp
071300030206     c                   xfoot     impf          totimpf
071400030206     c                   add       totimpf       totimp
071500030206     c                   eval      diffe = imponibile - totimp
071600030206     c                   if        diffe <> 0
071700030206     c                   z-add     1             xy
071800030206     c     v1tipf        lookup    tipf(xy)                               56
071900030206     c                   if        *in56
072000030206     c                   add       diffe         impf(xy)
072100030206     c                   end
072200030206     c* aggiorno anche il sfl 2
072300030206     c                   exsr      upsfl2
072400030206     c                   end
076700020527     c* data e nr fattura documento fornitore:
076800020409     c* se impostata a video tengo buona quella altrimenti la reperisco
076900020409     c* dal numeratore YPDR del fornitore
078800020415     c                   clear                   xnmuni
078900020415     c                   clear                   xnmctb
079000020417     c                   movel     'YPDR'        xnmser
079100020415     c                   move      *off          xnmcmt
079200020415     c                   move      *off          xnmiva
079300020415     c                   move      *off          xnmreg
079400020415     c                   clear                   xnmerr
079500020415     c                   clear                   xnmnum
079600020415     c                   clear                   xnmaut
079700080103     c                   movel     '4'           xnmric
079800020509     c                   movel     v1cfat        xnmkey
079900020417     c                   move      f50dpr        xnmdat
080000020417     C                   move      f50dpr        XnmDag                         opzionale
080100080103     c                   exsr      calxnmr
080300020417     c                   move      xnmnum        com9              9
080400020417     c                   eval      %subst(com9: 3: 3) = xscsoc
092100020408      *  Lettura record successivo
092200020610     C                   SETOFF                                       22
092300021016     c                   movea     ind23         *in(23)
092400020408     C                   UPDATE    y350S01
092500020614     c                   end
092600020408     C                   READC     y350S01                                29
092700020409     C                   enddo                                                  *IN29 = 0
092800951011     C                   ENDSR
131800030620?     *--------------------------------------------------------------*
131900030620?     *  ctrctt: Controllo se ho dei conteggi non confermati
132000030620      *          intal caso segnalo l'anomalia
132100030620?     *--------------------------------------------------------------*
132200030620     C     ctrctt        BEGSR
132300030620     c                   move      ' '           cttfvl
132400030620     c                   eval      noconf = ' '
132500030620     c* se trovata registrazione
132600030620     c                   if        f50tsr = 'S'
132700030620     C     Kctt          setll     fictt06L
132800030620     c                   else
132900030620     C     Kctt1         setll     fictt06L
133000030620     c                   end
133100030620     c                   do        *hival
133200030620     c                   if        f50tsr = 'S'
133300030620     C     Kctt          reade     fictt06L
133400030620     c                   else
133500030620     C     Kctt1         reade     fictt06L
133600030620     c                   end
133700030620     C                   if        %eof(fictt06l)
133800030620     c                   leave
133900030620     c                   end
134000030620     c* data conteggio compresa nei limiti richiesti
134100030620     c                   if        cttddc > f50dtf or cttddc < f50dti
134200030620     c                   iter
134300030620     c                   end
134400030620     c* se già fatturato iter
134500030620     c                   if        cttnff <> 0
134600030620     c                   iter
134700030620     c                   end
134800030620     c                   eval      noconf = '1'
134900030827     c                   leave
135000030620     C                   enddo
135100030620      *
135200030620     C                   ENDSR
141300030827?     *--------------------------------------------------------------*
141400030827?     *  ctratt: Controllo se ho dei conteggi non confermati
141500030827      *          intal caso segnalo l'anomalia
141600030827?     *--------------------------------------------------------------*
141700030827     C     ctratt        BEGSR
141800030828     c                   eval      attflg = ' '
141900030827     c                   eval      noconf = ' '
142000030827     c* se trovata registrazione
142100030827     C     Katt          setll     fiatt06L
142200030827     c                   do        *hival
142300030827     C     Katt          reade     fiatt06L
142400030827     C                   if        %eof(fiatt06l)
142500030827     c                   leave
142600030827     c                   end
142700030827     c* data conteggio compresa nei limiti richiesti
142800030827     c                   if        attddc > f50dtf or attddc < f50dti
142900030827     c                   iter
143000030827     c                   end
143100030827     c                   eval      noconf = '1'
143200030827     c                   leave
143300030827     C                   enddo
143400030827      *
143500030827     C                   ENDSR
147300980911     C*----------------------------------------------------*
147400980911     C* Reperimento dati società
147500980911     C*----------------------------------------------------*
147600980911     C     REPSOC        BEGSR
147700980911     C*
147800980911     C                   CALLB     'XSOC'
147900980911     C                   PARM                    TIPXSC            6
148000980911     C                   PARM                    SOCXSC            3
148100980911     C                   PARM                    CDSXSC            9 0
148200980911     C                   PARM                    MODXSC            3
148300980911     C                   PARM      *blanks       RTNXSC            1
148400980911     C                   PARM                    XSOCDS
148500980911     C                   PARM                    KPJBA
148600980911     C*
148700980911     C                   ENDSR
148800030206     c*----------------------------------------------------------------
148900030206     c     upsfl2        begsr
149000030206     c*----------------------------------------------------------------
149100030206     c                   do        10            xyz
149200030206     c     xyz           chain     y350s02                            56
149300030206     c                   if        *in56
149400030206     c                   leave
149500030206     c                   else
149600030206     c* seleziono il tipo forn./servizio
149700030206     c                   if        h1tipf <> v1tipf
149800030206     c                   iter
149900030206     c                   end
150000030206     c                   if        h1ccmp = 13
150100030206     c                   iter
150200030206     c                   end
150300030206     c                   add       diffe         h1cico
150400030206     c                   update    y350s02
150500030206     c                   leave
150600030206     c                   end
150700030206     c                   enddo
150800030206     c                   endsr
150900951011?     *--------------------------------------------------------------*
151000020527?     *  lettura tabella Y4Z                                         *
151100951011?     *--------------------------------------------------------------*
151200020527     C     sry4z         BEGSR
151300020527     c*  lettura tabella Y4Z
151400020527     c                   clear                   xatbds
151500020527     c                   move      '1'           xtbric
151600020527     c                   move      *blank        xtbkey
151700020527     c                   move      'Y4Z'         xtbcod
151800030206     c                   movel     v1tip         xtbkey
151900020527     c                   call      'XATB'
152000020527     c                   parm                    xatbds
152100020527     c                   if        xtberr = '0'
152200020527     c                   movel     xtbuni        angy4z
152300020527     c                   else
152400020527     c                   clear                   angy4z
152500020527     c                   end
152600031204     C*
152700031204     C                   ENDSR
154400020527?     *--------------------------------------------------------------*
154500030128?     *  istruz: Operazioni di inizializzazione dati                 *
154600020527?     *--------------------------------------------------------------*
154700030128     C     istruz        BEGSR
154800030827     c                   select
154900031121     c* prestazioni occasionali
155000031121     c                   when      f50cau<> ' '
155100030128     C                   EVAL      WrkSqlCmd
155200030128     C                             =
155300031121     C                             'SELECT ottcdf,'
155400030128     C                             +
155500031121     C                             ' sum(otttim) '
155600030128     C                             +
155700031121     C                             ' from fiott06l where ottsoc = '
155800030128     C                             +
155900030128     C                             '''' + f50soc  + ''''
156000030128     C                             +
156100030128     C                             ' AND '
156200030128     C                             +
156300031121     C                             ' ottcdf between '
156400030128     C                             +
156500030128     C                             '''' + f50pd1 + ''''
156600030128     C                             +
156700030129     C                             ' AND '
156800030128     C                             +
156900030128     C                             '''' + f50pd2 + ''''
157000030128     C                             +
157100030128     C                             ' AND '
157200030128     C                             +
157300031121     C                             ' otttip = '
157400031121     C                             +
157500031121     C                             '''' + f50tip + ''''
157600031121     C                             +
157700031121     C                             ' AND '
157800031121     C                             +
157900031121     C                             ' otttab = '
158000031121     C                             +
158100031121     C                             '''' + f50cau + ''''
158200030128     C                             +
158300030128     C                             ' AND '
158400030128     C                             +
158500031121     C                             ' ottddc BETWEEN '
158600030129     C                             + %editc(f50dti:'X')
158700030129     C                             +
158800030129     C                             ' AND ' + %editc(f50dtf:'X')
158900030128     C                             +
159000030129     C                             ' AND '
159100030128     C                             +
159200031121     C                             'ottnff = 0  group by ottcdf '
159300030203     c                             +
159400031121     C                             ' ORDER BY ottcdf'
159500031121     c* autotrasportatori
159600031121     c                   when      f50tip = 'A'
159700031121     C                   EVAL      WrkSqlCmd
159800031121     C                             =
159900031121     C                             'SELECT fttcdf,'
160000031121     C                             +
160100031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
160200031121     C                             +
160300031121     C                             'fttmnt)'
160400031121     C                             +
160500031121     C                             ' from fiftt06l where fttsoc = '
160600031121     C                             +
160700031121     C                             '''' + f50soc  + ''''
160800031121     C                             +
160900031121     C                             ' AND '
161000031121     C                             +
161100031121     C                             ' fttcdf between '
161200031121     C                             +
161300031121     C                             '''' + f50pd1 + ''''
161400031121     C                             +
161500031121     C                             ' AND '
161600031121     C                             +
161700031121     C                             '''' + f50pd2 + ''''
161800031121     C                             +
161900031121     C                             ' AND '
162000031121     C                             +
162100031121     C                             ' ftttsr = '' '''
162200031121     C                             +
162300031121     C                             ' AND '
162400031121     C                             +
162500031121     C                             ' fttfvl = ''C'''
162600031121     C                             +
162700031121     C                             ' AND '
162800031121     C                             +
162900031121     C                             ' Fttddc BETWEEN '
163000031121     C                             + %editc(f50dti:'X')
163100031121     C                             +
163200031121     C                             ' AND ' + %editc(f50dtf:'X')
163300031121     C                             +
163400031121     C                             ' AND '
163500031121     C                             +
163600031121     C                             'fttnff = 0  group by fttcdf '
163700031121     c                             +
163800031121     C                             ' ORDER BY fttcdf'
163900030128     c* cooperative
164000030827     c                   when      f50tip = 'C'
164100030128     C                   EVAL      WrkSqlCmd
164200030128     C                             =
164300030128     C                             'SELECT cttcdf,'
164400030128     C                             +
164500030205     C                             ' sum(cttitc+cttitma+cttitmp)'
164600030128     C                   IF        f50tsr = 'N'
164700030128     C                   EVAL      WrkSqlCmd
164800030128     C                             =
164900030128     C                             %TRIMR(WrkSqlCmd)
165000030128     C                             +
165100030205     C                             ', ctttsr '
165200030128     C                   ENDIF
165300030128     C                   EVAL      WrkSqlCmd
165400030128     C                             =
165500030128     C                             %TRIMR(WrkSqlCmd)
165600030128     C                             +
165700030128     C                             ' from fictt06l where cttsoc = '
165800030128     C                             +
165900030128     C                             '''' + f50soc  + ''''
166000030128     C                             +
166100030128     C                             ' AND '
166200030128     C                             +
166300030129     C                             ' cttcdf between '
166400030128     C                             +
166500030128     C                             '''' + f50pd1 + ''''
166600030128     C                             +
166700030129     C                             ' AND '
166800030128     C                             +
166900030128     C                             '''' + f50pd2 + ''''
167000030128     C                             +
167100030128     C                             ' AND '
167200030128     C                             +
167300030128     C                             ' cttfvl = ''C'''
167400030128     C                             +
167500030128     C                             ' AND '
167600030128     C                             +
167700030128     C                             ' cttddc BETWEEN '
167800030129     C                             + %editc(f50dti:'X')
167900030129     C                             +
168000030129     C                             ' AND ' + %editc(f50dtf:'X')
168100030128     C                             +
168200030128     C                             ' AND'
168300030128     C                             +
168400030203     C                             ' cttnff = 0  group by cttcdf '
168500030128     C                   IF        f50tsr = 'N'
168600030128     C                   EVAL      WrkSqlCmd
168700030128     C                             =
168800030128     C                             %TRIMR(WrkSqlCmd)
168900030128     C                             +
169000030203     C                             ', ctttsr'
169100030128     C                   ENDIF
169200030128     C                   EVAL      WrkSqlCmd
169300030128     C                             =
169400030128     C                             %TRIMR(WrkSqlCmd)
169500030128     C                             +
169600030203     C                             ' ORDER BY cttcdf '
169700030128     C                   IF        f50tsr = 'N'
169800030128     C                   EVAL      WrkSqlCmd
169900030128     C                             =
170000030128     C                             %TRIMR(WrkSqlCmd)
170100030128     C                             +
170200030203     C                             ', ctttsr '
170300030128     C                   ENDIF
170400030827     c* aff/defl.
170500030827     c                   when      f50tip = 'D'
170600030827     C                   EVAL      WrkSqlCmd
170700030827     C                             =
170800030827     C                             'SELECT attcdf,'
170900030827     C                             +
171000030827     C                             ' sum(attimpp)'
171100030827     C                             +
171200030827     C                             ' from fiatt06l where attsoc = '
171300030827     C                             +
171400030827     C                             '''' + f50soc  + ''''
171500030827     C                             +
171600030827     C                             ' AND '
171700030827     C                             +
171800030827     C                             ' attcdf between '
171900030827     C                             +
172000030827     C                             '''' + f50pd1 + ''''
172100030827     C                             +
172200030827     C                             ' AND '
172300030827     C                             +
172400030827     C                             '''' + f50pd2 + ''''
172500030827     C                             +
172600030827     C                             ' AND '
172700030827     C                             +
172800030827     C                             ' attdco <> 0'
172900030827     C                             +
173000030827     C                             ' AND '
173100030827     C                             +
173200030827     C                             ' attddc BETWEEN '
173300030827     C                             + %editc(f50dti:'X')
173400030827     C                             +
173500030827     C                             ' AND ' + %editc(f50dtf:'X')
173600030827     C                             +
173700030827     C                             ' AND '
173800030827     C                             +
173900030827     C                             'attnff = 0  group by attcdf '
174000030827     c                             +
174100030827     C                             ' ORDER BY attcdf'
174200030827     c                   endsl
174300030128     C                   ENDSR
174400030204?     *--------------------------------------------------------------*
174500030204?     *  srsfl2: CARICAMENNTO SUBFILE 2                              *
174600030204?     *--------------------------------------------------------------*
174700030204     C     srsfl2        BEGSR
174800030204      *
174900030204     c                   exsr      istruz2
175000030204      *  Pulisco dati SFL
175100030204     C                   SETOFF                                       24
175200030204     C                   WRITE     y350C02
175300030204     C                   SETON                                        24
175400030204     C                   Z-ADD     0             NRR2              6 0
175500030204     c                   clear                   tredici           1
175600030204     c                   clear                   tipf
175700030204     c                   clear                   impt
175800030204     c                   clear                   impf
175900030204     c                   clear                   totimpt
176000030206     c                   clear                   totimpf
176100030206     c                   clear                   totimp
176200030206     c                   clear                   diffe
176300030204     c* questo sql mi restituisce per ogni fornitore, mese di competenza,
176400030204     c* tipo servizio la sommatoria dei conteggi
176500030204     c*
176600030204     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
176700030204     c* precedente ed è possibile contabilizzare ancora i ratei,
176800030204     c* altrimenti se il conteggio è dell'anno precedente
176900030204     c* ma non si possono più registrare ratei sarà forzato a 1
177000030204     c* Per i conteggi dell'anno in corso il mese di competenza è
177100030204     c* quello del conteggio
177200030204     c*
177300030827     c* Lettura fiftt00f/fictt00f/fiatt00f
177400030204     C/EXEC SQL
177500030204     C+ PREPARE S2 FROM :WrkSqlCmd
177600030204     C/END-EXEC
177700030204
177800030204     C/EXEC SQL
177900030204     C+ DECLARE A2 CURSOR FOR S2
178000030204     C/END-EXEC
178100030204
178200030204     C/EXEC SQL
178300030204     C+ OPEN A2
178400030204     C/END-EXEC
178500030204
178600030204     C                   DOU       SqlCod <> 0
178700030827     c                   select
178800031121     c* prestazioni occasionali
178900031121     c                   when      f50cau<> ' '
179000030204     C/EXEC SQL
179100031121     C+ FETCH NEXT FROM A2 INTO :fiott2
179200030204     C/END-EXEC
179300031121     c* autotrasportatori
179400031121     c                   when      f50tip = 'A'
179500031121     C/EXEC SQL
179600031121     C+ FETCH NEXT FROM A2 INTO :fiftt2
179700031121     C/END-EXEC
179800030827     c                   when      f50tip = 'C'
179900030204     c* cooperative
180000030204     C/EXEC SQL
180100030204     C+ FETCH NEXT FROM A2 INTO :fictt2
180200030204     C/END-EXEC
180300030827     c                   when      f50tip = 'D'
180400030827     c* aff/defl.
180500030827     C/EXEC SQL
180600030827     C+ FETCH NEXT FROM A2 INTO :fiatt2
180700030827     C/END-EXEC
180800030827     c                   endsl
180900030204     c*
181000030204     C                   SELECT
181100030204     C                   WHEN      SqlCod = 0
181200030204     C                   EXSR      CARTRN2
181300030204     C                   WHEN      SqlCod = 100
181400030204     C                   WHEN      SqlCod <> 0
181500030204      * Forzo la stampa del JOBLOG.
181600030204     C                   CALL      'X66CHGJOB'
181700030204     C                   ENDSL
181800030204     C                   ENDDO
181900030204     C/EXEC SQL
182000030204     C+ CLOSE A2
182100030204     C/END-EXEC
182200030204     c* rileggo il sfl e mi memorizzo la sommatoria per ogni tipo servizio
182300030204     c* tenendo separati gli importi dell'anno in corso con quelli dell'anno
182400030204     c* precedente
182500030204     c                   clear                   x
182600030205     c                   do        10            xyz
182700030204     c     xyz           chain     y350s02                            56
182800030205     c                   if        *in56
182900030205     c                   leave
183000030205     c                   else
183100030204     c                   z-add     1             xy
183200030204     c     h1tipf        lookup    tipf(xy)                               55
183300030204     c                   if        *in55
183400030204     c                   if        h1ccmp = 13
183500030204     c                   add       h1cico        impt(xy)
183600030204     c                   else
183700030204     c                   add       h1cico        impf(xy)
183800030204     c                   end
183900030204     c                   else
184000030204     c                   add       1             x
184100030204     c                   movel     h1tipf        tipf(x)
184200030204     c                   if        h1ccmp = 13
184300030204     c                   z-add     h1cico        impt(x)
184400030204     c                   else
184500030205     c                   z-add     h1cico        impf(x)
184600030204     c                   end
184700030204     c                   end
184800030204     c                   end
184900030204     c                   enddo
185000030204     C*
185100030204     C                   ENDSR
185200030204?     *--------------------------------------------------------------*
185300030204?     *  istruz2 Operazioni di inizializzazione istruzione sql       *
185400030204?     *--------------------------------------------------------------*
185500030204     C     istruz2       BEGSR
185600030827     c                   select
185700031121     c* prestazioni occasionali
185800031121     c                   when      f50cau<> ' '
185900030204     C                   EVAL      WrkSqlCmd
186000030204     C                             =
186100030204     C                             'SELECT '
186200030204     C                             +
186300031121     C                             ' sum(otttim), '
186400030204     C                             +
186500031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
186600030204     C                             +
186700030204     C                             '''' + meseprt + ''''
186800030204     C                             +
186900030204     C                             ' then '
187000030204     C                             +
187100030204     C                             '''' + commes + ''''
187200030204     C                             +
187300030204     C                             ' else'
187400030204     C                             +
187500031121     C                             ' substr(digits(ottddc), 5, 2) end as pippo '
187600030204     C                             +
187700031121     C                             ' from fiott06l where ottsoc = '
187800030204     C                             +
187900030204     C                             '''' + f50soc  + ''''
188000030204     C                             +
188100030204     C                             ' AND '
188200030204     C                             +
188300031121     C                             ' ottcdf = '
188400030204     C                             +
188500030204     C                             '''' + v1cfor + ''''
188600030204     C                             +
188700030204     C                             ' AND '
188800030204     C                             +
188900031121     C                             ' otttip = '
189000031121     C                             +
189100031121     C                             '''' + f50tip + ''''
189200031121     C                             +
189300031121     C                             ' AND '
189400031121     C                             +
189500031121     C                             ' otttab = '
189600031121     C                             +
189700031121     C                             '''' + f50cau + ''''
189800030204     C                             +
189900030204     C                             ' AND '
190000030204     C                             +
190100031121     C                             ' ottddc BETWEEN '
190200030204     C                             + %editc(f50dti:'X')
190300030204     C                             +
190400030204     C                             ' AND ' + %editc(f50dtf:'X')
190500030204     C                             +
190600030204     C                             ' AND '
190700030204     C                             +
190800031121     C                             'ottnff = 0  group by '
190900030204     C                             +
191000031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
191100030204     C                             +
191200030204     C                             '''' + meseprt + ''''
191300030204     C                             +
191400030204     C                             ' then '
191500030204     C                             +
191600030204     C                             '''' + commes + ''''
191700030204     C                             +
191800030204     C                             ' else'
191900030204     C                             +
192000031121     C                             ' substr(digits(ottddc), 5, 2) end '
192100030204     C                             +
192200030204     C                             ' ORDER BY pippo'
192300031121     c* autotrasportatori
192400031121     c                   when      f50tip = 'A'
192500031121     C                   EVAL      WrkSqlCmd
192600031121     C                             =
192700031121     C                             'SELECT '
192800031121     C                             +
192900031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
193000031121     C                             +
193100031121     C                             'fttmnt),'
193200031121     C                             +
193300031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
193400031121     C                             +
193500031121     C                             '''' + meseprt + ''''
193600031121     C                             +
193700031121     C                             ' then '
193800031121     C                             +
193900031121     C                             '''' + commes + ''''
194000031121     C                             +
194100031121     C                             ' else'
194200031121     C                             +
194300031121     C                             ' substr(digits(fttddc), 5, 2) end as pippo '
194400031121     C                             +
194500031121     C                             ' from fiftt06l where fttsoc = '
194600031121     C                             +
194700031121     C                             '''' + f50soc  + ''''
194800031121     C                             +
194900031121     C                             ' AND '
195000031121     C                             +
195100031121     C                             ' fttcdf = '
195200031121     C                             +
195300031121     C                             '''' + v1cfor + ''''
195400031121     C                             +
195500031121     C                             ' AND '
195600031121     C                             +
195700031121     C                             ' ftttsr = '' '''
195800031121     C                             +
195900031121     C                             ' AND '
196000031121     C                             +
196100031121     C                             ' fttfvl = ''C'''
196200031121     C                             +
196300031121     C                             ' AND '
196400031121     C                             +
196500031121     C                             ' Fttddc BETWEEN '
196600031121     C                             + %editc(f50dti:'X')
196700031121     C                             +
196800031121     C                             ' AND ' + %editc(f50dtf:'X')
196900031121     C                             +
197000031121     C                             ' AND '
197100031121     C                             +
197200031121     C                             'fttnff = 0  group by '
197300031121     C                             +
197400031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
197500031121     C                             +
197600031121     C                             '''' + meseprt + ''''
197700031121     C                             +
197800031121     C                             ' then '
197900031121     C                             +
198000031121     C                             '''' + commes + ''''
198100031121     C                             +
198200031121     C                             ' else'
198300031121     C                             +
198400031121     C                             ' substr(digits(fttddc), 5, 2) end '
198500031121     C                             +
198600031121     C                             ' ORDER BY pippo'
198700030204     c* cooperative
198800030827     c                   when      f50tip = 'C'
198900030204     C                   EVAL      WrkSqlCmd
199000030204     C                             =
199100030204     C                             'SELECT ctttsr,'
199200030204     C                             +
199300030204     C                             ' sum(cttitc+cttitma+cttitmp),'
199400030204     C                             +
199500030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
199600030204     C                             +
199700030204     C                             '''' + meseprt + ''''
199800030204     C                             +
199900030204     C                             ' then '
200000030204     C                             +
200100030204     C                             '''' + commes + ''''
200200030204     C                             +
200300030204     C                             ' else'
200400030204     C                             +
200500030204     C                             ' substr(digits(cttddc), 5, 2) end as pippo '
200600030204     C                             +
200700030204     C                             ' from fictt06l where cttsoc = '
200800030204     C                             +
200900030204     C                             '''' + f50soc  + ''''
201000030204     C                   IF        f50tsr = 'N'
201100030204     C                   EVAL      WrkSqlCmd
201200030204     C                             =
201300030204     C                             %TRIMR(WrkSqlCmd)
201400030206     C                             +
201500030206     C                             ' AND '
201600030204     C                             +
201700030204     C                             ' ctttsr = '
201800030204     C                             +
201900030204     C                             '''' + v1tipf + ''''
202000030204     C                   ENDIF
202100030204     C                   EVAL      WrkSqlCmd
202200030204     C                             =
202300030204     C                             %TRIMR(WrkSqlCmd)
202400030204     C                             +
202500030204     C                             ' AND '
202600030204     C                             +
202700030204     C                             ' cttcdf = '
202800030204     C                             +
202900030204     C                             '''' + v1cfor + ''''
203000030204     C                             +
203100030204     C                             ' AND '
203200030204     C                             +
203300030204     C                             ' cttfvl = ''C'''
203400030204     C                             +
203500030204     C                             ' AND '
203600030204     C                             +
203700030204     C                             ' cttddc BETWEEN '
203800030204     C                             + %editc(f50dti:'X')
203900030204     C                             +
204000030204     C                             ' AND ' + %editc(f50dtf:'X')
204100030204     C                             +
204200030204     C                             ' AND'
204300030204     C                             +
204400030204     C                             ' cttnff = 0  group by ctttsr, '
204500030204     C                             +
204600030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
204700030204     C                             +
204800030204     C                             '''' + meseprt + ''''
204900030204     C                             +
205000030204     C                             ' then '
205100030204     C                             +
205200030204     C                             '''' + commes + ''''
205300030204     C                             +
205400030204     C                             ' else'
205500030204     C                             +
205600030204     C                             ' substr(digits(cttddc), 5, 2) end '
205700030204     C                             +
205800030204     C                             ' ORDER BY ctttsr, pippo'
205900030827     c* aff/defl
206000030827     c                   when      f50tip = 'D'
206100030827     C                   EVAL      WrkSqlCmd
206200030827     C                             =
206300031007     C*                            'SELECT attvad,'
206400031007     C                             'SELECT '
206500030827     C                             +
206600030827     C                             ' sum(attimpp),'
206700030827     C                             +
206800030827     C                             ' case when substr(digits(attddc), 5, 2) > '
206900030827     C                             +
207000030827     C                             '''' + meseprt + ''''
207100030827     C                             +
207200030827     C                             ' then '
207300030827     C                             +
207400030827     C                             '''' + commes + ''''
207500030827     C                             +
207600030827     C                             ' else'
207700030827     C                             +
207800030827     C                             ' substr(digits(attddc), 5, 2) end as pippo '
207900030827     C                             +
208000030827     C                             ' from fiatt06l where attsoc = '
208100030827     C                             +
208200030827     C                             '''' + f50soc  + ''''
208300030827     C                             +
208400030827     C                             ' AND '
208500030827     C                             +
208600030827     C                             ' attcdf = '
208700030827     C                             +
208800030827     C                             '''' + v1cfor + ''''
208900030827     C                             +
209000030827     C                             ' AND '
209100030827     C                             +
209200030827     C                             ' attdco <> 0'
209300030827     C                             +
209400030827     C                             ' AND '
209500030827     C                             +
209600030827     C                             ' attddc BETWEEN '
209700030827     C                             + %editc(f50dti:'X')
209800030827     C                             +
209900030827     C                             ' AND ' + %editc(f50dtf:'X')
210000030827     C                             +
210100030827     C                             ' AND'
210200030827     C                             +
210300031007     C*                            ' attnff = 0  group by attvad, '
210400031007     C                             ' attnff = 0  group by '
210500030827     C                             +
210600030827     C                             ' case when substr(digits(attddc), 5, 2) > '
210700030827     C                             +
210800030827     C                             '''' + meseprt + ''''
210900030827     C                             +
211000030827     C                             ' then '
211100030827     C                             +
211200030827     C                             '''' + commes + ''''
211300030827     C                             +
211400030827     C                             ' else'
211500030827     C                             +
211600030827     C                             ' substr(digits(attddc), 5, 2) end '
211700030827     C                             +
211800031007     C*                            ' ORDER BY attvad, pippo'
211900031007     C                             ' ORDER BY pippo'
212000030827     c                   endsl
212100030204     C                   ENDSR
212200030204?     *--------------------------------------------------------------*
212300030204?     *  CARTRN2 Gestione caricamento dati sfl2                      *
212400030204?     *--------------------------------------------------------------*
212500030204     C     CARTRN2       BEGSR
212600030827     c                   select
212700031121     c* prestazioni occasionali
212800031121     c                   when      f50cau <>' '
212900030204     C                   MOVE      v1tipf        h1tipf
213000031121     C                   eval(h)   h1cico = Totso2
213100031121     C                   if        meseso2 > meseprt
213200031121     C                   move      '13'          meseso2
213300030204     C                   END
213400031121     C                   move      meseso2       h1ccmp
213500031121     c* autotrasportatore
213600031121     c                   when      f50tip = 'A'
213700031121     C                   MOVE      v1tipf        h1tipf
213800031121     C                   eval(h)   h1cico = Tots2
213900031121     C                   if        meses2 > meseprt
214000031121     C                   move      '13'          meses2
214100031121     C                   END
214200031121     C                   move      meses2        h1ccmp
214300030204     c* cooperativa
214400030827     c                   when      f50tip = 'C'
214500030205     C                   eval(h)   h1cico = Totsc2
214600030204     C                   MOVEL     tsrsc2        h1tipf
214700030204     C                   if        mesesc2 > meseprt
214800030204     C                   move      '13'          mesesc2
214900030204     C                   END
215000030204     C                   move      mesesc2       h1ccmp
215100030827     c* aff/defl
215200030827     c                   when      f50tip = 'D'
215300030827     C                   eval(h)   h1cico = Totsa2
215400030827     c* ora il conto per aff/defl è lo stesso quindi non importa spaccare
215500030827     c* le contropartite, se un domani si vogliono tenere separati invece
215600031007     c* di chiodare D x le affluenze si metterà il codice di tabella nuovo
215700031007     c*                  if        tsrsa2 = 'A'
215800030827     C                   MOVEL(p)  'D'           h1tipf
215900031007     c*                  else
216000031007     C*                  MOVEL     tsrsa2        h1tipf
216100031007     c*                  end
216200030827     C                   if        mesesa2 > meseprt
216300030827     C                   move      '13'          mesesa2
216400030827     C                   END
216500030827     C                   move      mesesa2       h1ccmp
216600030827     c                   endsl
216700030204     c*
216800030204     c                   if        h1ccmp = 13
216900030204     c                   eval      tredici = *on
217000030204     c                   end
217100030204     c*
217200030204     C                   ADD       1             NRR2
217300030204     C                   WRITE     y350S02
217400030204      *
217500030204     C                   ENDSR
217600030416?     *--------------------------------------------------------------*
217700030416?     *  *INZSR: Operazioni di inizializzazione dati                 *
217800030416?     *--------------------------------------------------------------*
217900030416     C     *INZSR        BEGSR
218000020527      *
218100951011     C     *ENTRY        PLIST
218200951011     C                   PARM                    KPJBA
218300020408     C                   MOVEL     KPJBU         ficn50ds
218400980911     C*---------- RICERCA DITTA :
218500980911     C                   MOVEL     'SOC001'      TIPXSC
218600020412     C                   MOVEL     f50soc        SOCXSC
218700980911     C                   EXSR      REPSOC
218800980911     C     RTNXSC        IFNE      '1'
218900980911     C                   MOVEL     XSOCDS        SOC001
219000980911     C                   MOVEL     xscrgs        RSUT             20
219100980911     c                   end
219200020417      *  Definizione chiave di accesso
219300020423     C     Kuni          KLIST
219400020417     C                   KFLD                    xscsoc
219500020423     C                   KFLD                    f50uni
220200031121     C     Kott          KLIST
220300031121     C                   KFLD                    xscsoc
220400031121     C                   KFLD                    v1cfor
220500031121     C                   KFLD                    f50tip
220600031121     C                   KFLD                    f50cau
220700020423     C     Kftt          KLIST
220800020423     C                   KFLD                    xscsoc
220900020423     C                   KFLD                    v1cfor
221000020417     C                   KFLD                    ftttsr
221100020417     C                   KFLD                    fttfvl
221200020417     c                   move      ' '           ftttsr
221300020417     c                   move      'C'           fttfvl
221400030128     C     Kctt          KLIST
221500030128     C                   KFLD                    xscsoc
221600030128     C                   KFLD                    v1cfor
221700030128     C                   KFLD                    cttfvl
221800030128     C     Kctt1         KLIST
221900030128     C                   KFLD                    xscsoc
222000030128     C                   KFLD                    v1cfor
222100030128     C                   KFLD                    cttfvl
222200030128     C                   KFLD                    v1ctsr
222300030827     C     Katt          KLIST
222400030827     C                   KFLD                    xscsoc
222500030827     C                   KFLD                    v1cfor
222600030828     C                   KFLD                    attflg
222700031121     C     Ktbe1         KLIST
222800031121     C                   KFLD                    tbecod
222900031121     C                   KFLD                    tbeke1
223000020724     C     Ktbe          KLIST
223100020724     C                   KFLD                    tbecod
223200020724     C                   KFLD                    tbeke1
223300020724     C                   KFLD                    tbeke2
223400030213     C     Ktab          KLIST
223500030213     C                   KFLD                    tblkut
223600030213     C                   KFLD                    tblcod
223700030213     C                   KFLD                    tblkey
223800030213     c                   eval      tblkut = 1
223900030213     c                   eval      tblcod = '1Z'
224000020417     C     Kfrn          KLIST
224100020417     C                   KFLD                    f50soc
224200030129     C                   KFLD                    v1cfor
224300020417     C     Krco          KLIST
224400020417     C                   KFLD                    f50soc
224500020417     C                   KFLD                    forita
224600030129     C                   KFLD                    v1cfor
224700020514     C     Ktlz          KLIST
224800020514     C                   KFLD                    tlztip
224900020514     C                   KFLD                    tlzpdr
225000020514     C                   KFLD                    f50soc
225100020408     C* Reperisco i libri IVA ammessi per la filiale.
225200020724     C                   eval      tbecod = 'YIV'
225300020724     C                   eval      tbeke1 = xscsoc
225400020724     C                   eval      tbeke2 = %subst(f50UNI: 1: 3)
225500020724     C     ktbe          chain     tntbe01l
225600020408     C                   if        %found
225700020724     C                   eval      dyiv = tbeuni
225800020408     C                   else
225900020724     C                   clear                   dyiv
226000020408     C                   endif
226100031121     c* aggancio la tabella CAF
226200031121     c                   if        f50cau <> ' '
226300031121     c                   movel(p)  f50cau        tbeke1
226400031121     c                   movel(p)  'CAF'         tbecod
226500031121     c     ktbe1         chain     tntbe01l
226600031121     c                   if        %found(tntbe01l) and tbeatb = ' '
226700031121     c                   movel     tbeuni        dcaf
226800031121     c                   else
226900031121     c                   clear                   dcaf
227000031121     c                   end
227100031121     c                   end
227200020708     C* controllo la data di PROTOCOLLO
227300020708     c                   clear                   xnmkey
227400020708     c                   clear                   xnmser
227500020708     c                   move      *off          xnmcmt
227600020708     c                   clear                   xnmerr
227700020708     c                   clear                   xnmnum
227800020708     c                   clear                   xnmaut
227900020708     C                   MOVE      F50DPR        XnmDat
228000020708     C                   MOVE      F50DPR        XnmDag                         opzionale
228100020708     C                   MOVE      §IVLAQ        COM3              3            opzionale
228200020708     C                   EVAL      XNMSER = '1' + COM3
228300020708     C                   movel(p)  f50uni        Xnmuni
228400020708     C                   movel     '1'           XnmRic
228500020708     C                   movel     'CG'          XnmCtb
228600020708     C                   movel     *on           XnmIva
228700020708     C                   movel     *ON           XnmReg
228800020708     C                   EXSR      CALXNMR
228900020708     c                   if        xnmerr = '7'
229000020708     c                   seton                                        lr
229100020708     c                   return
229200020708     c                   end
229300020411     c* se non ho il documento fornitore non visualizzo la riga in testata
229400020411     c     f50nft        comp      0                                      06
229500020409      * Imposto la data di competenza analitica a fine anno
229600980916      * per verificare se posso impostare la competenza x
229700980916      * l'anno precedente
229800020409     C                   move      XSCSOC        X10societa
229900020409     C                   move      'CG'          X10ctb
230000020415     C                   move      udate         x10data
230100020409     C                   call      'X10ESER'
230200020409     C                   parm                    x10ese
230300020409     c                   if        x10dtines <> *loval
230400020409     C     X10DTINES     SUBDUR    1:*D          WDTFINE
230500020409     c                   endif
230600980916     C                   move      wdtfine       DTCOAN
230700020408     C                   move      f50dpr        Dataiso
230800020408     C                   move      dataiso       DTrif
230900980916      * Controllo se si può inserire ancora data competenza 13
231000980916     C                   Callb     'NDMVC113'
231100020412    >C                   PARM                    xscsoc
231200020411     c                   parm                    unita
231300980916    >C                   PARM      'CG'          Ctb
231400980916    >C                   PARM      *Blank        Keygest
231500020502    >C                   PARM      'R'           Tipdat
231600980916    >C                   PARM      'D'           Tpregz
231700980916    >C                   PARM                    DtCoAn
231800980916     C                   PARM                    DtRif
231900980916    >C                   PARM      'I'           Operazione
232000980916     C                   PARM      '1'           GesMsg
232100980916    >C                   PARM                    Reper
232200980916     C                   PARM                    Esito
232300020408     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
232400020408     C                   movel     'F     '      $kcc              6
232500020408     C                   movel     *blanks       $ksc              8
232600020408     C                   callb     'XCAPCLIFOR'
232700020412     C                   parm                    xscsoc
232800020408     C                   parm                    $kcc
232900020408     C                   parm                    $ksc
233000020408     C                   movel     $kcc          forita            6
233100020409     c* NSYS
233200020415     c                   clear                   xnmuni
233300020415     c                   clear                   xnmctb
233400020415     c                   clear                   xnmkey
233500020415     c                   clear                   xnmerr
233600020415     c                   clear                   xnmnum
233700020415     c                   clear                   xnmaut
233800020409     C                   MOVE      *DATE         XnmDat
233900020409     C                   MOVE      *DATE         XnmDag                         opzionale
234000020409     C                   move      *OFF          XnmCmt
234100020409     C                   move      *OFF          XnmIva
234200020409     C                   move      *OFF          XnmReg
234300020409     C                   EVAL      XnmRic = '1'
234400020409     C                   EVAL      XnmSer = 'NSYS'
234500020409     C                   EXSR      CalXnmr
234600030128      * Se si può inserire ancora data competenza 13
234700030128     c                   if        esito = *off
234800030128     C                   EVAL      commes = '13'
234900030128     C* Se non posso immettere registrazioni aventi data competenza
235000030128     C* dell'anno precedente imposto 01 il mese di competenza
235100030128     C* ed eseguo la normale procedura di contabilizz.
235200030128     c                   else
235300030128     C                   EVAL      commes = '01'
235400030128     c                   end
235500020412     c* estraggo il mese della data protocollo
235600020417     c                   move      f50dpr        dataiso
235700020417     c                   extrct    dataiso:*m    mm                2 0
235800020418     c                   move      mm            meseprt           2
235900031121     c* MI COMPONGO L'ISTRUZIONE SQL
236000030128     c                   exsr      istruz
236100031121     c                   if        f50cau <> ' '
236200031121     c                   seton                                        60
236300031121     c                   end
236400030827     c                   select
236500030128     c* decodifica a video
236600031121     c                   when      f50tip = 'A'
236700030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
236800040914     c                   clear                   dblc
236900040914     c                   movel(p)  '1'           tbeke1
237000040914     c                   movel     'BLC'         tbecod
237100040914     c     ktbe1         chain     tntbe01l
237200040914     c                   if        %found(tntbe01l)
237300040914     c                   movel     tbeuni        dblc
237400040914     c                   end
237500040914     c                   z-add     §blcmin       impmin
237600030827     c                   when      f50tip = 'C'
237700030128     c                   seton                                        60
237800030128     c                   eval      dectip = '   COOPERATIVE   '
238200040914     c                   clear                   dblo
238300040914     c                   movel(p)  '1'           tbeke1
238400040914     c                   movel     'BLO'         tbecod
238500040914     c     ktbe1         chain     tntbe01l
238600040914     c                   if        %found(tntbe01l)
238700040914     c                   movel     tbeuni        dblo
238800040914     c                   end
238900040914     c                   z-add     §blomin       impmin
239000030827     c                   when      f50tip = 'D'
239100030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
239500040914     c                   clear                   dbld
239600040914     c                   movel(p)  '1'           tbeke1
239700040914     c                   movel     'BLD'         tbecod
239800040914     c     ktbe1         chain     tntbe01l
239900040914     c                   if        %found(tntbe01l)
240000040914     c                   movel     tbeuni        dbld
240100040914     c                   end
240200040914     c                   z-add     §bldmin       impmin
240300030827     c                   endsl
240400020408      *  Inizializzo variabili
240500020408     C                   MOVEL     '1'           WTPVID            1
240600020408     C                   MOVEL     'N'           WFINE             1
240800951012     C                   ENDSR
243600951012      *--------------------------------------------------------------*
243700951012** ERR
243800020415Selezionare almeno un fornitore                                       1
243900021015Esistono fornitori senza partita IVA                                  2
244000030620ATTENZIONE!! Esistono valorizzazioni non confermate                   3
