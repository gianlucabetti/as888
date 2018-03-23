000100150108     H OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP('YFICN51')
000200150108     H BNDDIR('PJXBND' : 'PJCBND')
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
002400030128     Ffiftt06L  UF   E           K DISK    USROPN
002500030128     Ffictt06L  UF   E           K DISK    USROPN
002600030827     Ffiatt06L  UF   E           K DISK    USROPN
002700031121     Ffiott06L  UF   E           K DISK    USROPN
002800160108     Ftnsre01L  UF   E           K DISK
002900080924     FTNanf01L  UF a E           K DISK    commit
003000080924     Ftntbe01l  if   E           K DISK
003100030213     Ftabel00f  if   E           K DISK
003200951009     F*--------
003300020708     Fndbhp00f  o    E             DISK    commit
003400020708     Fndbhm00f  o    E             DISK    commit
003500020411     fndbhi00f  o    e             disk    commit
003600020411     fndbha00f  o    e             disk    commit
003700951011     F*--------
003800020423     Fanuni01l  IF   E           K DISK
003900020423     Fansog01l  IF   E           K DISK
004000020405     Fanfrn01l  IF   E           K DISK
004100020405     Fanrco01l  IF   E           K DISK
004200020514     Ftntlz01l  IF   E           K DISK
004300020708     Fanope01l  IF   E           K DISK
004400020708     Fandft01l  IF   E           K DISK
004500020405     F*--------
004600020404     Fficn51D   CF   E             WORKSTN
004700980911     F                                     SFILE(y350S01:NRR)
004800030204     F                                     SFILE(y350S02:NRR2)
004900951009     D*---------------------------------------------------------------*
005000951009     D* SCHIERA
005100951009     D*---------------------------------------------------------------*
005200951011      * Schiere per errori e stampa
005300160108     D ERR             S             70    DIM(6) CTDATA PERRCD(1)              Errori
005400030204     D tipf            S              4    DIM(10) inz                          Errori
005500030204     D impf            S             13  2 DIM(10) inz                          Errori
005600030204     D impt            S             13  2 DIM(10) inz                          Errori
005700020411     D* Reperimento nome PGM
005800020411     D STATUS         SDS           333
005900020411     D  nompgm           *PROC
006000020411     D*---------------------------------------------------------------*
006100020411     D* CAMPI INTERNI
006200020411     D*---------------------------------------------------------------*
006300020411     D dataiso         s               d   datfmt(*iso)
006400020411     D dataeur         s               d   datfmt(*eur)
006500020409     D WDTFINE         S               D
006600080924     D sqlcodsav       S                   like(sqlcod)
006700080924     D riga1           S                   like(BhmNrAsReg)
006800020411     D riga2           S                   like(BhmNrAsReg)
006900020418     D ksys            S              3  0 inz
007000020503     d subnum          S                   like(bhmsubnum)
007100020415     D savsubnum       S                   like(bhmsubnum)
007200020415     D savsubrig       S                   like(bhmsubrig)
007300020503     D savdtdoc        S                   like(bhmdtdoc)
007400020503     D savnrdoc        S                   like(bhmnrdoc)
007500021127     D savdcn          S                   like(fttdcn)
007600040914     D impmin          S                   like(§blcmin)
007700000204     D* ------ Definizioni parametri standard
007800020411     D Esito           S              1
007900020411    >D Unita           S              8A
008000020411    >D Ctb             S              2A
008100020411    >D Keygest         S              8A
008200020411    >D Tipdat          S              1A
008300020411    >D Tpregz          S              1A
008400020411    >D DtCoan          S             10A
008500020411    >D DtRif           S             10A
008600020411    >D Operazione      S              2A
008700020411     D GesMsg          S              1
008800020411    >D Reper           S              1
008900020411     D XnmRic          S              1
009000020411     D XnmSoc          S              3
009100020411     D XnmUni          S              8
009200020411     D XnmCtb          S              2
009300020411     D XnmKey          S              8
009400020411     D XnmSer          S              4
009500020411     D XnmCmt          S              1
009600020411     D XnmDat          S               D
009700020411     D XnmIva          S              1
009800020411     D XnmReg          S              1
009900020415     D XnmDag          S               D
010000020411     D XnmErr          S              1
010100020411     D XnmNum          S              9  0
010200020411     D XnmAut          S              1
010300020418     D imponibile      S             18  2
010400030205     D impcom          S             18  2
010500030204     D totimpt         S             18  2
010600030206     D totimpf         S             18  2
010700030206     D totimp          S             18  2
010800030206     D diffe           S             13  2
010900080925     D comsfl2         S             13  2
011000030206     D impiva          S             13  2
011100020527     d v1tips          s                   like(v1tipf)
011200030206     d v1tip           s                   like(v1tipf)
011300031204     d segnofor        s                   like(bhmsegno)
011400030128     d commes          s              2
011500030416     d com03           s              3
011600030204     d x               s              3  0
011700030204     d xy              s              3  0
011800030204     d xyz             s              3  0
011900030620     d noconf          s              1    inz
012000150108      *-----
012100150108      * servirà x gestire x le COOP l'accorpamento excludendo le PULIZIE
012200150108      * che x legge (dal 2015) devono essere tenute a parte.
012300150108      * Serve x fare 2 volte il giro sull'accorpamento.
012400150108      * 1) primo con solo le PULIZIE
012500150108      * 2) secondo con le altre senza le PULIZIE
012600150108      *-----
012700150108     d Coop_PULIZIE_a_parte...
012800150108     d                 s              1    inz
012900951009?     *--------------------------------------------------------------*
013000951009?     *  DS                                                          *
013100951009?     *--------------------------------------------------------------*
013200080923     D ficn74ds      e DS                  inz
013300080924     D danf2         e DS
013400080924     D dapdflr       e DS
013500080924     D dftt01        e DS
013600020606     D xatbds        e DS
013700980911     D soc001        E DS                  EXTNAME(xsoc001ds)
013800980911     D xsocds          DS          1000
013900030213     D ds1z          e ds                  inz
014000031121     D dcaf          e ds                  inz
014100040914     D dBLC          e ds                  inz
014200040914     D dBLD          e ds                  inz
014300040914     D dBLO          e ds                  inz
014400020411     D* libro iva acquisti
014500020724     D dyiv          e ds                  inz
014600020409     D* Parametri per reperimento data inizio esercizio
014700020411    >D x10ese        E DS                  EXTNAME(x10eseDS)
014800951009     D KPJBA         E DS
014900020411?     *  Parametri registrazioni contabili
015000980914     D ndc071ds      E DS
015100020411     D NDC050DS      E DS                  INZ
015200020411     D  RET050       E                     INZ('0')
015300020411     D  OPR050       E                     INZ('0')
015400020411     D  ERR050       E                     INZ('0')
015500020514     D dtlz01        E DS
015600020411     d* parametri ricevuti
015700020404     D ficn50ds      E DS
015800020415     D salds         E DS                  extname(ficn50ds) prefix(£)
015900020411?     *  Tabella Y4Z
016000020408     D angy4z        E DS                  extname(angy4zds)
016100020411?     *  Tabella IVA
016200020411     DTABG48         E DS                  EXTNAME(ANGG48DS)
016300020411     D X06DS         E DS                  EXTNAME(X06G48DS)
016400020411     D  X06SOC       E                     EXTFLD(X06SOCIETA)
016500020411     D  X06CIV       E                     EXTFLD(X06CDIVA)
016600020411     D  X06DAT       E                     EXTFLD(X06DATA)
016700020411     D  X06LIN       E                     EXTFLD(X06LINGUA)
016800020415     d* cancella NDBH*
016900020415     D ycodel1ds     e DS
017000020411     c* SQL
017100020405     D WrkSqlCmd       S           1024
017200080924     D WrkSqladg       S           1024
017300030827     c* sfl 1 aut.
017400020405     D fiftt           DS
017500020405     d cdfs                                like(fttcdf)
017600020405     d tots                                like(ftttim)
017700030827     c* sfl 1 coop accorpate
017800030128     D fictt           DS
017900030128     d cdfsc                               like(cttcdf)
018000030128     d totsc                               like(cttitc)
018100030827     c* sfl 1 coop non accorpate
018200030128     D fictttsr        DS
018300030128     d cdfsc1                              like(cttcdf)
018400030128     d totsc1                              like(cttitc)
018500030128     d tsrsc1                              like(ctttsr)
018600030827     c* sfl 1 aff/defl
018700030827     D fiatt           DS
018800030827     d cdfsa                               like(attcdf)
018900030827     d totsa                               like(attimpp)
019000031121     c* sfl 1 prestaz. occasionali
019100031121     D fiott           DS
019200031121     d cdfso                               like(ottcdf)
019300031121     d totso                               like(otttim)
019400030827     c* sfl 2 aut.
019500030204     D fiftt2          DS
019600030204     d tots2                               like(ftttim)
019700030204     d meses2                         2
019800080924     c* adeguamento carburante
019900080924     D fttatt3         DS
020000080924     d pdr3                                like(fttpdr)
020100080924     d tot3                                like(ftttim)
020200030827     c* sfl 2 coop
020300030204     D fictt2          DS
020400030204     d tsrsc2                              like(ctttsr)
020500030204     d totsc2                              like(cttitc)
020600030204     d mesesc2                        2
020700030827     c* sfl 2 aff/defl
020800030827     D fiatt2          DS
020900031007     d*tsrsa2                              like(attvad)
021000030827     d totsa2                              like(attimpp)
021100030827     d mesesa2                        2
021200031121     c* sfl 2 prestaz. occasionali
021300031121     D fiott2          DS
021400031121     d totso2                              like(otttim)
021500031121     d meseso2                        2
021600990507     D*  Prototipazione delle procedure
021700990507     D/COPY *LIBL/SRCCPY,PJX002PR
021800951009?     *--------------------------------------------------------------*
021900951009?     *  CICLO PRINCIPALE                                            *
022000951009?     *--------------------------------------------------------------*
022100951009      *  Impostazione parametri prima videata
022200951010     C                   EXSR      CARSFL
022300951009      *  Loop gestione videata
022400951009     C     WFINE         DOUEQ     'S'
022500000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
022600000426     C     WTPVID        CASEQ     '2'           GESD02                         sfl vuoto
022700951009     C                   END
022800951012     C                   END
022900980312     C*
023000020419     C                   MOVEL(p)  ficn50ds      KPJBU
023100951009     C                   SETON                                        LR
023200951009?     *--------------------------------------------------------------*
023300951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
023400951009?     *--------------------------------------------------------------*
023500951012     C     CARSFL        BEGSR
023600951009      *
023700951009      *  Pulisco dati SFL
023800020405     C                   SETOFF                                       2120
023900980911     C                   WRITE     y350C01
024000951009     C                   SETON                                        2021
024100150108      *
024200951009     C                   MOVEL     '1'           WTPVID
024300020415     C                   Z-ADD     0             NRR               6 0
024400150108      *
024500150108      *  Accorpamento x COOP dopo aver scorporato le PULIZIE
024600150108      *    deve eseguire un secondo giro con l'SQL modificato
024700150108      *   *-----------------*
024800150108     c     secondo_giro  tag
024900150108      *   *-----------------*
025000150108      *
025100020409     c* questo sql mi restituisce per ogni fornitore e mese di competenza
025200020412     c* la sommatoria dei conteggi.
025300020412     c*
025400020412     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
025500020412     c* precedente ed è possibile contabilizzare ancora i ratei,
025600020412     c* altrimenti se il conteggio è dell'anno precedente
025700020412     c* ma non si possono più registrare ratei sarà forzato a 1
025800020412     c* Per i conteggi dell'anno in corso il mese di competenza è
025900020412     c* quello del conteggio
026000020412     c*
026100031121     c* Lettura fiftt00f/fictt00f/fiatt00f/fiott00f
026200020405     C/EXEC SQL
026300020405     C+ PREPARE S1 FROM :WrkSqlCmd
026400020405     C/END-EXEC
026500020405
026600020405     C/EXEC SQL
026700020405     C+ DECLARE A1 CURSOR FOR S1
026800020405     C/END-EXEC
026900020405
027000020405     C/EXEC SQL
027100020405     C+ OPEN A1
027200020405     C/END-EXEC
027300020405     C                   DOU       SqlCod <> 0
027400150108
027500030827     C                   SELECT
027600031121     c* prestaz. occasionali
027700031121     c                   when      f50cau <> ' '
027800031121     C/EXEC SQL
027900031121     C+ FETCH NEXT FROM A1 INTO :fiott
028000031121     C/END-EXEC
028100031121     c* autotrasportatori
028200031121     c                   when      f50tip = 'A'
028300020405     C/EXEC SQL
028400020405     C+ FETCH NEXT FROM A1 INTO :fiftt
028500020405     C/END-EXEC
028600150108     c* cooperative
028700030827     c                   when      f50tip = 'C'
028800150108     c* non Accorpate
028900150108     c*  oppure le Pulizie estrapolate dall'accorpamento
029000150108     c                   if        f50tsr = 'N'
029100150108     c                              or
029200150108     c                             Coop_PULIZIE_a_parte ='S'
029300150108     c*      con tipo servizio
029400030128     C/EXEC SQL
029500030128     C+ FETCH NEXT FROM A1 INTO :fictttsr
029600030128     C/END-EXEC
029700150108     c                   else
029800150108     c*     Accorpate senza le PULIZIE
029900150108     C/EXEC SQL
030000150108     C+ FETCH NEXT FROM A1 INTO :fictt
030100150108     C/END-EXEC
030200030128     c* cooperative      con tipo servizio
030300030128     c                   end
030400030827     c* aff/defl.
030500030827     c                   when      f50tip = 'D'
030600030827     C/EXEC SQL
030700030827     C+ FETCH NEXT FROM A1 INTO :fiatt
030800030827     C/END-EXEC
030900030827     c                   endsl
031000150108      *
031100020405     C                   SELECT
031200150108      *
031300020405     C                   WHEN      SqlCod = 0
031400150108     c*                *------------------*
031500951010     C                   EXSR      CARTRN
031600150108     c*                *------------------*
031700020405     C                   WHEN      SqlCod = 100
031800020405     C                   WHEN      SqlCod <> 0
031900020417      * Forzo la stampa del JOBLOG.
032000020417     C                   CALL      'X66CHGJOB'
032100020405     C                   ENDSL
032200020404     C                   ENDDO
032300020417     C/EXEC SQL
032400020417     C+ CLOSE A1
032500020417     C/END-EXEC
032600150108      *
032700150108      *  solo x le COOP:
032800150108      * DOVENDO accorpare ma dovendo tenere OBBLIGATORIAMENTE separata la
032900150108      *  scrittura contabile delle PULIZIE (questo dal 2015)
033000150108      *  si è gestito il campo --> Coop_PULIZIE_a_parte i cui valori sono:
033100150108      * (*BLK) non si deve accorpare o il pgm non è chiamato per le COOP
033200150108      * (S) x le COOP in accorpamento e estrapolare le PULIZIE dai servizi
033300150108      * (N) x le COOP in accorpamento e totalizzando i servizi senza le PULIZIE
033400150108      *   "S" ha fatto il primo giro dove ha preso solo le PULIZIE (come in dettaglio)
033500150108      *       viene messa a "N"
033600150108      *  per eseguire un 2°giro estraendo gli altri servizi accorpati NO PULIZIE.
033700150108      *   dopo il 2° giro va oltre
033800150108     c                   if        Coop_PULIZIE_a_parte ='S'
033900150108     c                   eval          Coop_PULIZIE_a_parte ='N'
034000150108      *  reimposta l'SQL modificato.
034100150108      *   e carica nuove righe sul SFL.
034200150108     c                   exsr      ISTRUZ
034300150108      *                         *-----------------*
034400150108     c                   goto      secondo_giro
034500150108      *                         *-----------------*
034600150108     c                   end
034700150108      *
034800020405      *  Imposto dati SFLCTL
034900020405     C                   EXSR      INZ01
035000150108      *
035100951009      *  Se non ho dati da caricare emetto videata vuota
035200951009     C     NRR           IFEQ      0
035300020405     C                   MOVEL     '2'           WTPVID
035400951009     C                   END
035500951009     C*
035600951009     C                   ENDSR
035700951013?     *--------------------------------------------------------------*
035800020415?     *  INZ01: Inizializzazione videata                             *
035900951013?     *--------------------------------------------------------------*
036000951013     C     INZ01         BEGSR
036100951013      *
036200000426      *  Inizializzo testata sfl in base a quanto
036300000426      *  passato dal programma chiamante
036400951013     C                   MOVEL     '1'           WTPVID
036500951016     C                   MOVEL     NOMPGM        V1CPGM
036600020405     C                   MOVEL     F50DP1        V1DPDR
036700020405     C                   move      f50PD1        V1CPDR
036800020405     C                   MOVEL     F50DP2        V2DPDR
036900020405     C                   move      f50PD2        V2CPDR
037000020405     C                   Z-ADD     f50NFT        V1CNFT
037100020423     c     kuni          chain     anuni01l
037200020423     c                   if        %found
037300020423     c                   movel     unidesbrev    v1cdun
037400020423     c                   else
037500020423     c                   movel     *blanks       v1cdun
037600020423     c                   end
037700020405     C                   if        f50DTI > 0
037800020405     C                   move      f50DTI        dataiso
037900000911     C                   else
038000000911     C                   move      '0001-01-01'  dataiso
038100000911     C                   end
038200980911     C                   MOVEL     dataiso       dataeur
038300980911     C                   movel     dataeur       V1CDTI
038400020405     C                   if        f50DTF > 0
038500020405     C                   movel     f50DTF        dataiso
038600000911     C                   else
038700000911     C                   move      '0001-01-01'  dataiso
038800000911     C                   end
038900980911     C                   MOVEL     dataiso       dataeur
039000980914     C                   movel     dataeur       V1CDTF
039100020405     C                   if        f50DFT > 0
039200020405     C                   movel     f50DFT        dataiso
039300000911     C                   else
039400000911     C                   move      '0001-01-01'  dataiso
039500000911     C                   end
039600980911     C                   MOVEL     dataiso       dataeur
039700980911     C                   movel     dataeur       V1CDFT
039800020405     C                   if        f50DPR > 0
039900020405     C                   movel     f50DPR        dataiso
040000000911     C                   else
040100000911     C                   move      '0001-01-01'  dataiso
040200000911     C                   end
040300980911     C                   MOVEL     dataiso       dataeur
040400980911     C                   movel     dataeur       V1CDPR
040500951013      *
040600951013     C                   ENDSR
040700951010?     *--------------------------------------------------------------*
040800020405?     *  CARTRN: Gestione caricamento dati                           *
040900951010?     *--------------------------------------------------------------*
041000951010     C     CARTRN        BEGSR
041100150108      *
041200030827     c                   select
041300031121     c* prestazioni occasionali
041400031121     c                   when      f50cau <> ' '
041500031121     C                   MOVE      cdfso         V1Cfor
041600161116     C                   Z-ADD(h)  Totso         v1cico
041700031121     c* autotrasportatore
041800031121     c                   when      f50tip = 'A'
041900031121     C                   MOVE      cdfs          V1Cfor
042000161116     C                   Z-ADD(h)  Tots          v1cico
042100150108     c* cooperativa
042200030827     c                   when      f50tip = 'C'
042300150108      * Non accorpate
042400150108      *   oppure
042500150108      * x PULIZIE scorporate dall'accorpamento. (nuova legge 2015)
042600150108     C                   if        f50tsr = 'N'
042700150108     c                               or
042800150108     c                             Coop_PULIZIE_a_parte ='S'
042900161116     C                   Z-ADD(h)  Totsc1        v1cico
043000030129     C                   MOVE      cdfsc1        V1Cfor
043100030128     C                   MOVEL     tsrsc1        V1Ctsr
043200030213     C                   MOVEL(p)  tsrsc1        tblkey
043300030213     c* decodifico tipo servizio
043400030213     c     ktab          chain     tabel00f
043500030213     c                   if        %found(tabel00f)
043600030213     c                   movel     tbluni        ds1z
043700030213     c                   movel     §1zdes        v1dtsr
043800030213     c                   else
043900030213     c                   clear                   v1dtsr
044000030213     c                   end
044100150108     c                   else
044200150108      *  oppure  in accorpamento
044300150108      *  ma senza le PULIZIE (x nuova legge 2015)
044400161116     C                   Z-ADD(h)  Totsc         v1cico
044500150108     C                   MOVE      cdfsc         V1Cfor
044600150108     c                   clear                   v1ctsr
044700150108     c                   clear                   v1dtsr
044800030128     c                   end
044900030827     c* aff/defl.
045000030827     c                   when      f50tip = 'D'
045100030827     C                   MOVE      cdfsa         V1Cfor
045200161116     C                   Z-ADD(h)  Totsa         v1cico
045300030827     c                   endsl
045400951010     C                   MOVEL     *BLANKS       V1CSCE
045500021015     c* 23 ind. che protegge il campo di scelta per i fornitori senza p.IVA
045600120816     c*    oppure i fornitori di cui non è specificata la tipologia
045700021015     c                   setoff                                       23
045800020405     C     Kfrn          CHAIN     anfrn01l                           30
045900020405     C  N30Krco          CHAIN     anrco01l                           30
046000020423     c                   if        not *in30 and rcounita <> f50uni
046100020423     c                   seton                                        30
046200020423     c                   end
046300030128     c                   if        not *in30 and f50tip = 'A'
046400030128     C                   if        rcoctgan02 <> 'N   ' and
046500020604     C                             rcoctgan02 <> 'I   ' and
046600020604     C                             rcoctgan02 <> 'M   '
046700120816      * tipologia fornitore se non presente
046800120816      * modificato prima accendeva il 30 per non scrivere la riga ora il 23 per proteggere scelta
046900120816     C                   SETON                                        23
047000020604     c                   end
047100030128     c                   end
047200020514     C  N30rcosogg       CHAIN     ansog01l                           30
047300020514     C* escludo i fornitore che non fanno autofatturazione
047400020514     c                   if        not *in30
047500150108      *
047600030827     c                   select
047700030827     c* autotrasportatori
047800030827     c                   when      f50tip = 'A'
047900030128     c                   movel(p)  'P'           tlztip
048000030827     c                   when      f50tip = 'C'
048100030204     c                   movel(p)  'C'           tlztip
048200030827     c                   when      f50tip = 'D'
048300030827     c                   movel(p)  'D'           tlztip
048400030827     c                   endsl
048500150108      *
048600030128     c                   move      v1cfor        tlzpdr
048700020514     C     ktlz          CHAIN     tntlz01l                           30
048800020514     c                   if        not *in30
048900020514     c                   movel     tlzflo        dtlz01
049000020515     c                   if        f50nft = 0
049100020515     c                   if        §tlzfl1 = 'S'
049200020514     c                   seton                                        30
049300020514     c                   end
049400020515     c                   else
049500020515     c                   if        §tlzfl1 <>'S'
049600020515     c                   seton                                        30
049700020515     c                   end
049800020515     c                   end
049900030205     c                   else
050000030827     c* se non trova il record di tntlz per la coop o x aff/defl. no errore
050100030827     c* esiste solo se la coop o aff/defl. non è o non è stata in autofat.
050200030827     c                   if        tlztip = 'C' or tlztip = 'D'
050300030205     c                   setoff                                       30
050400030205     c                   end
050500020514     c                   end
050600020514     c                   end
050700020514     c*
050800020423     c                   if        not *in30
050900120816      * se non trovata tipologia il 23 è già acceso e do errore per la tipologia
051000120816      * se spento verifica la presenza della partita iva
051100120816     c                   if        *in23
051200120816     C                   SETON                                        28
051300120816     c                   move      '1'           ind23
051400120816     C                   MOVEL     ERR(5)        $MSG
051500120816     c                   else
051600021015     c     sogpartiva    comp      *blanks                                23
051700021015     c                   if        *in23
051800021015     C                   SETON                                        28
051900021016     c                   move      '1'           ind23
052000021015     C                   MOVEL     ERR(2)        $MSG
052100021016     c                   else
052200021016     c                   move      '0'           ind23
052300021015     c                   end
052400120816     c                   endif
052500020405     C                   MOVEL     sogdes        v1dksc
052600030827     c                   select
052700031121     c                   when      f50cau <> ' '
052800031121     C                   MOVEL     §caftab       v1tipf
052900031121     c                   when      f50tip = 'A'
053000031121     C                   MOVEL     rcoctgan02    v1tipf
053100150108      * coop
053200030827     c                   when      f50tip = 'C'
053300150108      * Non accorpate
053400150108      *   oppure
053500150108      * x PULIZIE scorporate dall'accorpamento. (nuova legge 2015)
053600150108     c                   if        f50tsr = 'N'
053700150108     c                               or
053800150108     c                             Coop_PULIZIE_a_parte ='S'
053900150108      * imposta il Tipo Servizio sia se a Dettaglio
054000150108     c                   movel     tsrsc1        v1tipf
054100150108     c                   else
054200150108      * gli accorpati
054300150108     c                   movel     'X'           v1tipf
054400030203     c                   end
054500030827     c                   when      f50tip = 'D'
054600030827     C                   MOVEL     'D'           v1tipf
054700030827     c                   endsl
054800020405     c                   if        frncdiva <> *blanks
054900020405     c                   movel     frncdiva      v1civc
055000020405     c                   else
055100020405     c                   movel     *blanks       v1civc
055200020405     c                   end
055300020509     c* fornitore di riferimento per numerazione fattura
055400020509     c                   movel     FRNFORFATT    v1cfat
055500020509     c                   if        v1cfat = *blanks
055600020509     c                   movel     v1cfor        v1cfat
055700020509     c                   end
055800020509     c                   movel     rcounita      v1cuni
055900030203     c                   if        f50cdp <> *blanks
056000030203     c                   MOVEL     f50cdp        v1ccdp
056100030203     c                   else
056200030203     c                   MOVEL     FRNCONDPAG    v1ccdp
056300030203     c                   end
056400030620     c* controllo se esistono dei conteggi non confermati in tal cas
056500030827     c* segnalo l'anmalia ma non blocco solo per coop e aff/defl
056600040112     c* NO X PRESTAZIONI RESIDUALI
056700040112     c                   if        f50cau = ' '
056800030827     c                   select
056900030827     c                   when      f50tip = 'C'
057000030620     c                   exsr      ctrctt
057100030827     c                   when      f50tip = 'D'
057200030827     c                   exsr      ctratt
057300030827     c                   endsl
057400030827     c                   if        noconf = '1'
057500161012     c                   seton                                        2823
057600161012     c                   move      '1'           ind23
057700161012      **
057800030827     c                   if        f50tip = 'C'
057900030827     C                   MOVEL     ERR(3)        $MSG
058000030827     c                   else
058100030827     C                   MOVEL     ERR(3)        com52            52
058200030827     c                   eval      $msg = com52 + '(FOR.' + attcdf + ')'
058300030827     c                   end
058400161012      **
058500030827     c                   end
058600040112     c                   end
058700080924     c* controllo se tutti gli aut hanno massa complessiva
058800080924     c                   if        (f50tip = 'A' or f50tip = 'D') and
058900080924     c                             ind23 = '0'
059000080924     c                   exsr      ctradg
059100080924     c                   if        erradg <> 0
059200080923     c                   seton                                        2823
059300080924     c                   move      '1'           ind23
059400080924     C                   MOVEL     err(4)        $MSG
059500080923     c                   end
059600080924     c                   end
059700160108     c* controllo se presenti rettifiche di sede e nel caso ci siano
059800160108     c* devono essere minori dell'importo da fatturarare
059900160108     c                   clear                   v1cret
060000160108     c                   exsr      srretsede
060100160115     c                   if        v1cret <> 0 and
060200160115     c                             (v1cico+v1cret) <= impmin
060300160115     C                   SETON                                        2823
060400160108     c                   move      '1'           ind23
060500160108     C                   MOVEL     ERR(6)        $MSG
060600160108     c                   end
060700020408     c*
060800020405     C                   ADD       1             NRR
060900020405     C                   WRITE     y350S01
061000020423     c                   end
061100951010      *
061200951010     C                   ENDSR
061300951009?     *--------------------------------------------------------------*
061400160108?     *  srretsede rettifiche di sede
061500951009?     *--------------------------------------------------------------*
061600160108     C     srretsede     BEGSR
061700160108     c                   eval      sqlcodsav = sqlcod
061800160108     c* con tipo servizio
061900160108     c                   if        v1ctsr <> ' '
062000160108     C/EXEC SQL
062100161116     C+ SELECT round(sum(sretim), 2) INTO :v1cret FROM tnsre00f
062200161116     C+ WHERE sresoc =
062300160108     C+ :f50soc and srecdf = :v1cfor and sretsr = :v1ctsr and sreddc
062400160108     C+ between :f50dti and :f50dtf and srenff = 0
062500160108     C/END-EXEC
062600160108     c                   else
062700160108     c* senza tipo servizio
062800160108     C/EXEC SQL
062900161116     C+ SELECT round(sum(sretim), 2) INTO :v1cret FROM tnsre00f
063000161116     C+ WHERE sresoc =
063100160108     C+ :f50soc and srecdf = :v1cfor and sretsr <> 'P' and sreddc
063200160108     C+ between :f50dti and :f50dtf and srenff = 0
063300160108     C/END-EXEC
063400160108     c                   end
063500160108     c                   eval      sqlcod = sqlcodsav
063600160108      *
063700160108     C                   ENDSR
063800160108?     *--------------------------------------------------------------*
063900160108?     *  srcmd   Gestione comandi                                    *
064000160108?     *--------------------------------------------------------------*
064100160108     C     srcmd         BEGSR
064200020415     c                   clear                   f50cmd
064300020405      *  Fine Lavoro
064400020405     C     *INKC         IFEQ      '1'
064500020405     C                   MOVEL     'S'           WFINE
064600020405     C                   MOVEL     '03'          f50CMD
064700020405     C                   END
064800020405      *  Ritorno
064900020405     C     *INKL         IFEQ      '1'
065000020405     C                   MOVEL     'S'           WFINE
065100020405     C                   MOVEL     '12'          f50CMD
065200020405     C                   END
065300951010      *
065400020405     C                   ENDSR
065500020405?     *--------------------------------------------------------------*
065600020405?     *  GESS01: Gestione prima videata                              *
065700020405?     *--------------------------------------------------------------*
065800020405     C     GESS01        BEGSR
065900020405      *
066000980914     C                   WRITE     y350T01
066100980914     C                   EXFMT     y350C01
066200020405     c* gestione comandi
066300020405     c                   exsr      srcmd
066400020415     c                   if        f50cmd <> *blanks
066500020415     c                   goto      finvd1
066600020415     c                   end
066700951009      *  Controlli
066800951009     C                   EXSR      CTR01
066900020405     C   28              GOTO      FINVD1
067000020408      *  CONFERMA
067100951013     C     *INKF         IFEQ      '1'
067200000204      * Richiamo pgm prima nota fatture
067300951013     C                   EXSR      CALPNF
067400951013      *  Se quando esco dal programma di prima nota è tutto O.K
067500980914     C                   MOVEL     'S'           WFINE
067600020415     C                   MOVEL     '06'          f50CMD
067700980911     C                   END                                                    V3CTOT = y35FAT
067800951009      *
067900951009     C     FINVD1        ENDSR
068000951010?     *--------------------------------------------------------------*
068100951010?     *  GESD02: Gestione seconda videata                            *
068200951010?     *--------------------------------------------------------------*
068300951010     C     GESD02        BEGSR
068400951010      *
068500980914     C                   EXFMT     y350D02
068600020405     c* gestione comandi
068700020405     c                   exsr      srcmd
068800951010      *
068900951010     C                   ENDSR
069000951009?     *--------------------------------------------------------------*
069100951009?     *  CTR01: Controlli prima videata                              *
069200951009?     *--------------------------------------------------------------*
069300951009     C     CTR01         BEGSR
069400021016     c                   setoff                                       28
069500951009      *
069600020405     C                   Z-ADD     0             V1CITT
069700160108     C                   Z-ADD     0             V1Crett
069800951013     C                   MOVEL     'N'           WSELTR            1
069900020610     C                   MOVEL     *BLANKS       $MSG
070000020415      *  Controllo se devo selezionare tutte i fornitori
070100951010     C     *INKA         IFEQ      '1'
070200951010     C                   Z-ADD     1             NRR
070300980911     C     NRR           CHAIN     y350S01                            29
070400951010     C     *IN29         DOWEQ     '0'
070500021016     c* non scelgo i record con ind23 = 1 xchè manca p.iva
070600021016     c                   if        ind23 = '0'
070700951013     C                   MOVEL     'S'           WSELTR            1
070800951010     C                   MOVEL     '1'           V1CSCE
070900951010     C                   ADD       V1CICO        V1CITT
071000160108     C                   ADD       V1Cret        V1Crett
071100951010     C                   SETON                                        22
071200021016     c                   movea     ind23         *in(23)
071300980911     C                   UPDATE    y350S01
071400951010     C                   SETOFF                                       22
071500021016     c                   end
071600951010     C                   ADD       1             NRR
071700980911     C     NRR           CHAIN     y350S01                            29
071800011017     C                   ENDdo
071900951010     C                   ELSE
072000951010      *  Solo i selezionati
072100980911     C                   READC     y350S01                                29
072200951010     C     *IN29         DOWEQ     '0'
072300020405     C                   select
072400030128     C                   when      v1csce = 'D'
072500020405     c                   movel     ficn50ds      salds
072600020528     c                   movel     v1cfor        f50pd2
072700020417     c                   movel     v1dksc        f50dp2
072800020415     c                   movel     v1cfor        f50pd1
072900020415     c                   movel     v1dksc        f50dp1
073000020528     c*
073100020417     c                   movel(p)  ficn50ds      kpjbu
073200030827     c* dettaglio aut.
073300031121     C                   if        (F50TIP='A' or f50tip='D') and f50cau=' '
073400020405     c                   call      'FICN52R'
073500020417     c                   parm                    kpjba
073600030827     C                   END
073700020417     c                   movel     kpjbu         ficn50ds
073800020415     c                   movel     f50cmd        £f50cmd
073900020408     c                   movel     salds         ficn50ds
074000020408     c                   movel     *blanks       v1csce
074100020610     C                   SETOFF                                       22
074200021016     c                   movea     ind23         *in(23)
074300020408     C                   UPDATE    y350S01
074400020415     c                   if        f50cmd = '03'
074500020415     c                   move      *on           *inkc
074600020405     c                   exsr      srcmd
074700020415     c                   goto      finct1
074800020415     c                   end
074900020405     c*
075000020405     C     V1CSCE        wheneq    '1'
075100020405     C                   MOVEL     'S'           WSELTR            1
075200951010     C                   ADD       V1CICO        V1CITT
075300160108     C                   ADD       V1Cret        V1Crett
075400951010     C                   SETON                                        22
075500021016     c                   movea     ind23         *in(23)
075600980911     C                   UPDATE    y350S01
075700951010     C                   SETOFF                                       22
075800020610     C                   OTHER
075900020610     C                   SETOFF                                       22
076000021016     c                   movea     ind23         *in(23)
076100020610     C                   UPDATE    y350S01
076200020405     C                   ENDsl
076300980911     C                   READC     Y350S01                                29
076400011017     C                   ENDdo
076500951010     C                   END
076600020415      *  Nessun fornitore selezionato
076700020408     C     WSELTR        IFeq      'N'
076800020405     C     *INKF         andeq     '1'
076900951013     C                   SETON                                        28
077000020415     C                   MOVEL     ERR(1)        $MSG
077100951013     C                   GOTO      FINCT1
077200951013     C                   END
077300951010      *
077400951010     C     FINCT1        ENDSR
077500000204     C*------------------------------------------------------*
077600020408     C*  Reperisco il numero assoluto di registrazione       *
077700000204     C*------------------------------------------------------*
077800000204     C     prenotA       BEGSR
077900030206     c                   z-add     0             riga1
078000030206     c                   z-add     0             riga2
078100000204     C* Prima riga
078200020415     c                   clear                   xnmuni
078300020415     c                   clear                   xnmctb
078400020415     c                   clear                   xnmkey
078500020415     c                   clear                   xnmerr
078600020415     c                   clear                   xnmnum
078700020415     c                   clear                   xnmaut
078800020409     C                   MOVE      *DATE         XnmDat
078900020409     C                   MOVE      *DATE         XnmDag                         opzionale
079000020409     C                   move      *OFF          XnmCmt
079100020409     C                   move      *OFF          XnmIva
079200020409     C                   move      *OFF          XnmReg
079300020409     C                   EVAL      XnmRic = '2'
079400020409     C                   EVAL      XnmSer = 'NRAS'
079500020409     C                   EXSR      CalXnmr
079600020409     c                   z-add     xnmnum        riga1
079700000204     C* Seconda riga
079800030204     c                   if        tredici = *on
079900020415     c                   clear                   xnmerr
080000020415     c                   clear                   xnmnum
080100020409     C                   EXSR      CalXnmr
080200020409     c                   z-add     xnmnum        riga2
080300020409     c                   end
080400000204     C*
080500000204     C                   ENDSR
080600020409?     *--------------------------------------------------------------*
080700020409      * Chiamata a XNMR.
080800020409?     *--------------------------------------------------------------*
080900020409     C     CalXnmr       BEGSR
081000020409      *
081100020409      *
081200020409     C                   CALLB     'XNMR'
081300020409     C                   PARM                    XnmRic
081400020412     C                   PARM      xscsoc        Xnmsoc
081500020409     C                   PARM                    XnmUni
081600020409     C                   PARM                    XnmCtb
081700020409     C                   PARM                    XnmKey
081800020409     C                   PARM                    XnmSer
081900020409     C                   PARM                    XnmCmt
082000020409     C                   PARM                    XnmDat
082100020409     C                   PARM                    XnmIva
082200020409     C                   PARM                    XnmReg
082300020409     C                   PARM                    XnmErr
082400020409     C                   PARM                    XnmNum
082500020409     C                   PARM                    XnmAut
082600020409     C                   PARM                    XnmDag                         opzionale
082700020409      *
082800020409     C                   ENDSR
082900951011?     *--------------------------------------------------------------*
083000951011?     *  CALPNF: Carico subfile e richiamo prima nota fatture        *
083100951011?     *--------------------------------------------------------------*
083200951012     C     CALPNF        BEGSR
083300980914      *
083400020408     C                   READC     y350S01                                29
083500020408     C     *IN29         DOWEQ     '0'
083600020408     C     V1CSCE        IFEQ      '1'
083700040914     c* se importo > = importo mnimo da fatturare preso dalla tab. BLC/O/D
083800160108     C                   if        (v1cico+v1cret) >= impmin
083900030203     c* scrivo il sfl di comodo dove memorizzo gli importi per ogni mese
084000030203     c* di competenza
084100030203     c                   exsr      srsfl2
084200030206     c* per ogni tipologia fornitore/servizio aggancio la tabella Y4Z
084300030203     c                   if        v1tipf <> v1tips
084400030206     c                   eval      v1tip = v1tipf
084500020527     c                   exsr      sry4z
084600020527     c                   movel     v1tipf        v1tips
084700020527     c                   end
084800020417     c* imponibile 18,2
084900160115     c                   eval(h)   imponibile= (v1cico+v1cret)
085000030206     c* verifico eventuale squadratura di arrotondamento e la metto nel
085100030206     c* costo dell'anno in corso
085200030206     c                   xfoot     impt          totimp
085300030206     c                   xfoot     impf          totimpf
085400030206     c                   add       totimpf       totimp
085500030206     c                   eval      diffe = imponibile - totimp
085600030206     c                   if        diffe <> 0
085700030206     c                   z-add     1             xy
085800030206     c     v1tipf        lookup    tipf(xy)                               56
085900030206     c                   if        *in56
086000030206     c                   add       diffe         impf(xy)
086100030206     c                   end
086200030206     c* aggiorno anche il sfl 2
086300080925     c                   z-add     diffe         comsfl2
086400030206     c                   exsr      upsfl2
086500030206     c                   end
086600030206     c* Se devo gestire la competenza 13 prelievo il nr. assoluto
086700030206     c* per poi riaggiornare i record in modo da collegare le due registraz.
086800020408     C                   EXSR      PRENOTA
086900020409     C* REPERISCO IL NUMERO DI PROTOCOLLO
087000020415     c                   clear                   xnmkey
087100020415     c                   clear                   xnmser
087200020415     c                   move      *off          xnmcmt
087300020415     c                   clear                   xnmerr
087400020415     c                   clear                   xnmnum
087500020415     c                   clear                   xnmaut
087600020409     C                   MOVE      F50DPR        XnmDat
087700020409     C                   MOVE      F50DPR        XnmDag                         opzionale
087800020409     C                   MOVE      §IVLAQ        COM3              3            opzionale
087900150204      *****
088000150205      *****  per il Reverse Charge
088100150204     c                   if        v1tipf = 'P'
088200150205     C                   MOVE      §IVLAR        COM3              3            opzionale
088300150204     c                   end
088400150204      *****
088500020409     C                   EVAL      XNMSER = '1' + COM3
088600020411     C                   movel(p)  v1cuni        Xnmuni
088700020409     C                   movel     '3'           XnmRic
088800020409     C                   movel     'CG'          XnmCtb
088900020409     C                   movel     *ON           XnmIva
089000020409     C                   movel     *ON           XnmReg
089100020409     C                   EXSR      CALXNMR
089200020409     c                   move      xnmnum        f50npr            9 0
089300030204      *  FORNITORE
089400980914     c                   clear                   ndbhm000
089500150119      *-----
089600150119     c* calcolo iva
089700150119     c                   exsr      sriva
089800150119     c     impiva        add       imponibile    bhmimporto
089900150119     C                   EVAL      bhmserreg = '1' + COM3
090000150119     c* se si tratta di PULIZIE delle COOP
090100150119     c*  deve essere passata senza l'IVA e senza serie per reverse charge
090200150119     c                   if        v1tipf = 'P' and f50TIP = 'C' and
090300150119     c                             impiva <> 0
090400150119     c                   clear                   bhmserreg
090500150119     c                   z-add     imponibile    bhmimporto
090600150119     c                   end
090700150119      *-----
090800020417     c                   z-add     riga1         bhmnrasreg
090900020415     c                   add       1             subnum
091000020409     c                   z-add     subnum        bhmsubnum
091100020409     c                   z-add     1             bhmsubrig
091200020409     c                   move      knraz         bhmrifint
091300020409     c                   movel     'CP'          bhmrifint
091400150109      **
091500020417     c                   move      f50dpr        bhmdtreg
091600020417     c                   z-add     f50npr        bhmnrreg
091700020417     c                   movel     forita        bhmkcc
091800020417     c                   movel     v1cfor        bhmksc
091900020417     c                   movel     v1ccdp        bhmcondpag
092000020417     c                   movel     xscsoc        bhmsocieta
092100020417     c                   movel     'CG'          bhmctb
092200020417     c                   movel     'CG'          bhmclasse
092300020411     c                   movel(p)  v1cuni        bhmunita
092400020417     c                   movel     'D'           bhmtpregz
092500020527     c                   movel     'EUR'         bhmdivisa
092600020527     c* data e nr fattura documento fornitore:
092700020409     c* se impostata a video tengo buona quella altrimenti la reperisco
092800020409     c* dal numeratore YPDR del fornitore
092900020408     c                   if        f50dft <> 0
093000020527     c* Imposto le causali contabili relative alla NON autofatturazione
093100030203     c                   if        tredici = *on
093200020527     c                   movel     §4zcrp        bhmcaustes
093300020527     c                   else
093400020527     c                   movel     §4zcrc        bhmcaustes
093500150116     c* causale per iva esente
093600150119     c                   if        §4zcrce <> ' '  and impiva = 0
093700150116     c                   movel     §4zcrce       bhmcaustes
093800150116     c                   end
093900150116     c                   end
094000020417     c                   move      f50dft        bhmdtdoc
094100020417     c                   z-add     f50nft        bhmnrdoc
094200020417     c                   move      f50dft        bhmdtpar
094300020417     c                   z-add     f50nft        bhmnrpar
094400020408     c                   else
094500020527     c* Imposto le causali contabili relative alla autofatturazione
094600030203     c                   if        tredici = *on
094700020527     c                   movel     §4zcrpa       bhmcaustes
094800020527     c                   else
094900020527     c                   movel     §4zcrca       bhmcaustes
095000150116     c* causale per iva esente
095100150119     c                   if        §4zcrcae <> ' ' and impiva = 0
095200150116     c                   movel     §4zcrcae      bhmcaustes
095300020527     c                   end
095400150116     c                   end
095500020415     c                   clear                   xnmuni
095600020415     c                   clear                   xnmctb
095700020417     c                   movel     'YPDR'        xnmser
095800020415     c                   move      *off          xnmcmt
095900020415     c                   move      *off          xnmiva
096000020415     c                   move      *off          xnmreg
096100020415     c                   clear                   xnmerr
096200020415     c                   clear                   xnmnum
096300020415     c                   clear                   xnmaut
096400020408     c                   movel     '3'           xnmric
096500020509     c                   movel     v1cfat        xnmkey
096600020417     c                   move      f50dpr        xnmdat
096700020417     C                   move      f50dpr        XnmDag                         opzionale
096800020409     c                   exsr      calxnmr
096900020417     c                   move      xnmdat        bhmdtdoc
097000020417     c                   move      xnmnum        com9              9
097100020417     c                   eval      %subst(com9: 3: 3) = xscsoc
097200020417     c                   move      com9          bhmnrdoc
097300020410     c                   move      xnmdat        bhmdtpar
097400020417     c                   move      com9          bhmnrpar
097500020408     c                   end
097600031204     c* reperisco segno contabile dalla causale
097700031204     c                   exsr      srope
097800031204     c                   eval      bhmsegno = segnofor
097900020503     c* salvo data e numero documento fornitore
098000020503     c                   move      bhmdtdoc      savdtdoc
098100020503     c                   move      bhmnrdoc      savnrdoc
098200080924     c* calcolo adeguamento l'eventuale carburante
098300080924     c                   if        f50tip = 'A' or f50tip = 'D'
098400080924     c                   exsr      caradg
098500080924     c                   end
098600150108      **
098700020417     c                   z-add     bhmimporto    bhmimportd
098800990507     c                   z-add     1             bhmcambio
098900980914     c                   write     ndbhm000
099000030205     c* scrittura NDBHP00F
099100030205     c                   exsr      writebhp
099200030204     c* scrittura riga iva / esenzione
099300030203     c                   exsr      writebhi
099400030204     c* CONTROPARTITA (conti di costo --> bhm)
099500030204     c                   do        10            x
099600030204     c                   if        tipf(x) = *blanks
099700030204     c                   leave
099800030204     c                   end
099900030204     c                   if        impf(x) = 0
100000030204     c                   iter
100100030204     c                   end
100200030204     c* per ogni tipologia servizio aggancio la tabella Y4Z
100300030204     c* solo se sono stati accorpati i servizi altrimenti l'ho già letta
100400030204     c* inizialmente
100500030204     c                   if        f50tsr = 'S' and tipf(x) <> v1tips
100600030206     c                   eval      v1tip = tipf(x)
100700030204     c                   exsr      sry4z
100800030204     c                   movel     tipf(x)       v1tips
100900030204     c                   end
101000980914     c                   add       1             bhmsubrig
101100020417     C                   movel     §4zkcc        bhmkcc
101200020417     C                   movel     §4zksc        bhmksc
101300020417     c                   movel     *blank        bhmcondpag
101400031204     c                   if        segnofor = 'A'
101500980914     C                   MOVEL     'D'           bhmsegno
101600031204     c                   else
101700031204     C                   MOVEL     'A'           bhmsegno
101800031204     c                   end
101900030204     c                   movel     forita        bhmkcccpu
102000980914     c                   move      *zeros        bhmksccpu
102100020417     c                   move      v1cfor        bhmksccpu
102200161116     c                   z-add     impf(x)       bhmimporto
102300020417     c                   z-add     bhmimporto    bhmimportd
102400020409     c                   write     ndbhm000
102500030204     c* analitica
102600030204     c                   exsr      writebha
102700030204     c                   enddo
102800030204     c* FATTURE DA RICEVERE (unico indipendentemente dal tipo servizio)
102900030206     c                   xfoot     impt          totimpt
103000030204     c                   if        totimpt <> 0
103100030204     c                   add       1             bhmsubrig
103200030210     c                   movel     *blank        bhmcondpag
103300031204     c                   if        segnofor = 'A'
103400030210     C                   MOVEL     'D'           bhmsegno
103500031204     c                   else
103600031204     C                   MOVEL     'A'           bhmsegno
103700031204     c                   end
103800030210     c                   movel     forita        bhmkcccpu
103900030210     c                   move      *zeros        bhmksccpu
104000030210     c                   move      v1cfor        bhmksccpu
104100030204     c                   movel     §4zkcr        bhmkcc
104200030204     c                   movel     §4zksr        bhmksc
104300030204     c                   z-add     totimpt       bhmimporto
104400030204     c                   z-add     bhmimporto    bhmimportd
104500030204     c                   write     ndbhm000
104600030204     c* REGISTRAZIONE ANNO PRECEDENTE
104700980916     c                   exsr      competenza
104800020409     c                   z-add     savsubrig     bhmsubrig
104900020410     c                   z-add     savsubnum     bhmsubnum
105000020417     c                   move      f50dpr        bhmdtreg
105100020409     c                   end
105200020411     c                   commit
105300020614      * richiamo pgm x ctb
105400980914     c                   exsr      primanota
105500021127     c                   z-add     0             savdcn
105600020614     c                   else
105700040914     c* se IMPORTO < importo minimo da fatturare
105800020614     c                   z-add     0             riga1
105900020614     c                   z-add     999999999     savnrdoc
106000020614     c                   move      f50dpr        savdtdoc
106100021127     c                   move      f50dpr        savdcn
106200020614     c                   end
106300031121      * imposto il numero della fattura nel file FIFTT00F/FICTT00F/FIATT00F
106400031121      * FIOTT00F se ok contabilizzazione
106500030204     c* (se i record cancellati dalla funzione sono tutti 0 vuol dire che
106600030204     c* la contabilizzaz. è andata bene)
106700030204     c                   clear                   ycodel1ds
106800030204     c                   if        riga1 <> 0
106900030204     c                   move      bhmsocieta    del1sc
107000030204     c                   move      bhmrifint     del1ri
107100030204     c                   move      *on           del1ct
107200030204     c                   movel(p)  ycodel1ds     kpjbu
107300030204     c                   call      'YCODEL1R'
107400030204     c                   parm                    kpjba
107500030204     c                   movel     kpjbu         ycodel1ds
107600030204     c                   end
107700030204     c*
107800030204     c                   if        del1fl = ' ' and del1np = 0 and
107900030204     c                             del1nm=0 and del1ni=0 and del1na=0
108000030827     c                   select
108100031121     c                   when      f50cau <>' '
108200031121     C                   EXSR      AGGott
108300031121     c                   when      f50tip = 'A'
108400031121     C                   EXSR      AGGftt
108500030827     c                   when      f50tip = 'C'
108600030128     C                   EXSR      AGGctt
108700030827     c                   when      f50tip = 'D'
108800030827     C                   EXSR      AGGatt
108900030827     c                   endsl
109000160108     c                   exsr      aggsre
109100030204     c                   end
109200020408      *  Lettura record successivo
109300020610     C                   SETOFF                                       22
109400021016     c                   movea     ind23         *in(23)
109500020408     C                   UPDATE    y350S01
109600020614     c                   end
109700020408     C                   READC     y350S01                                29
109800020409     C                   enddo                                                  *IN29 = 0
109900951011     C                   ENDSR
110000000426     c*----------------------------------------------------------------
110100080924     c     ctradg        begsr
110200000426     c*----------------------------------------------------------------
110300081006     c                   clear                   erradg            2 0
110400080924     c                   eval      sqlcodsav = sqlcod
110500080924     C/EXEC SQL
110600081006     C+ SELECT count(*) INTO :erradg FROM fiapd00f
110700081006     C+ WHERE apdatb= ' ' and apdtip = :f50tip and apdcsf
110800161206     C+ = :f50soc and apdksc = :v1cfor and (substr(apdflr, 9, 7)
110900161206     C+ ='0000000' or substr(apdflr, 9, 7)= ' ') and apdPDD <>'S'
111000080924     C/END-EXEC
111100080924     c                   eval      sqlcod = sqlcodsav
111200080924     C                   ENDSR
111300080924     c*----------------------------------------------------------------
111400080924     c     caradg        begsr
111500080924     c*----------------------------------------------------------------
111600080925     c* autotrasportatori di città
111700080924     c                   if        f50tip = 'A'
111800080924     C                   EVAL      WrkSqladg
111900080924     C                             =
112000080924     C                             'SELECT fttpdr,'
112100080924     C                             +
112200080924     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
112300080924     C                             +
112400080924     C                             'fttmnt)'
112500080924     C                             +
112600080924     C                             ' from fiftt06l where fttsoc = '
112700080924     C                             +
112800080924     C                             '''' + f50soc  + ''''
112900080924     C                             +
113000080924     C                             ' AND '
113100080924     C                             +
113200080924     C                             ' fttcdf between '
113300080924     C                             +
113400080924     C                             '''' + v1cfor + ''''
113500080924     C                             +
113600080924     C                             ' AND '
113700080924     C                             +
113800080924     C                             '''' + v1cfor + ''''
113900080924     C                             +
114000080924     C                             ' AND '
114100080924     C                             +
114200080924     C                             ' ftttsr = '' '''
114300080924     C                             +
114400080924     C                             ' AND '
114500080924     C                             +
114600080924     C                             ' fttfvl = ''C'''
114700080924     C                             +
114800080924     C                             ' AND '
114900080924     C                             +
115000080924     C                             ' Fttddc BETWEEN '
115100080924     C                             + %editc(f50dti:'X')
115200080924     C                             +
115300080924     C                             ' AND ' + %editc(f50dtf:'X')
115400080924     C                             +
115500080924     C                             ' AND '
115600080924     C                             +
115700080924     C                             'fttnff = 0  group by fttpdr '
115800080925     c                   end
115900080925     c                   if        f50tip = 'D'
116000080924     C                   EVAL      WrkSqladg
116100080924     C                             =
116200080924     C                             'SELECT attpdr,'
116300080924     C                             +
116400080924     C                             ' sum(attimpp)'
116500080924     C                             +
116600080924     C                             ' from fiatt06l where attsoc = '
116700080924     C                             +
116800080924     C                             '''' + f50soc  + ''''
116900080924     C                             +
117000080924     C                             ' AND '
117100080924     C                             +
117200080924     C                             ' attcdf between '
117300080924     C                             +
117400080924     C                             '''' + v1cfor + ''''
117500080924     C                             +
117600080924     C                             ' AND '
117700080924     C                             +
117800080924     C                             '''' + v1cfor + ''''
117900080924     C                             +
118000080924     C                             ' AND '
118100080924     C                             +
118200080924     C                             ' attdco <> 0'
118300080924     C                             +
118400080924     C                             ' AND '
118500080924     C                             +
118600080924     C                             ' attddc BETWEEN '
118700080924     C                             + %editc(f50dti:'X')
118800080924     C                             +
118900080924     C                             ' AND ' + %editc(f50dtf:'X')
119000080924     C                             +
119100080924     C                             ' AND '
119200080924     C                             +
119300080924     C                             'attnff = 0  group by attpdr '
119400080924     c                   end
119500080924     C/EXEC SQL
119600080924     C+ PREPARE S3 FROM :WrkSqladg
119700080924     C/END-EXEC
119800080924
119900080924     C/EXEC SQL
120000080924     C+ DECLARE A3 CURSOR FOR S3
120100080924     C/END-EXEC
120200080924
120300080924     C/EXEC SQL
120400080924     C+ OPEN A3
120500080924     C/END-EXEC
120600080924
120700080924     C                   DOU       SqlCod <> 0
120800080924     C/EXEC SQL
120900080924     C+ FETCH NEXT FROM A3 INTO :fttatt3
121000080924     C/END-EXEC
121100080924     c*
121200080924     C                   SELECT
121300080924     C                   WHEN      SqlCod = 0
121400080924     c                   clear                   ficn74ds
121500080924     c                   move      pdr3          i74pdr
121600080924     c                   eval      i74tip = f50tip
121700161116     c                   z-add(h)  tot3          comtot3           9 2
121800081002     c                   z-add     comtot3       i74imp
121900080924     C                   if        f50dpr > 0
122000080924     C                   move      f50dpr        I74DTPRE
122100080924     c                   end
122200080924     c                   call      'FICN74R'
122300080924     c                   parm                    ficn74ds
122400080926     C     o74err        ifeq      ' '
122500080924     c                   exsr      agganf2
122600080924     C                   END
122700080924     C                   WHEN      SqlCod = 100
122800080924     C                   WHEN      SqlCod <> 0
122900080924      * Forzo la stampa del JOBLOG.
123000080924     C                   CALL      'X66CHGJOB'
123100080924     C                   ENDSL
123200080924     C                   ENDDO
123300080924     C/EXEC SQL
123400080924     C+ CLOSE A3
123500080924     C/END-EXEC
123600080923     C                   ENDsr
123700080923     c*----------------------------------------------------------------
123800080923     c     agganf2       begsr
123900080923     c*----------------------------------------------------------------
124000080923     C                   EVAL      ANFATB = ' '
124100080923     C                   EVAL      ANFTRC = '2'
124200080924     C                   eval      ANFTIP = f50tip
124300080924     C                   EVAL      ANFTSR = f50tip
124400080923     C                   EVAL      ANFSOC = xscSoc
124500080923     c                   eval      anfcdf = v1cfor
124600080925     c                   move      bhmdtdoc      anfdft
124700080925     c                   move      bhmnrdoc      anfnff
124800080924     C                   eval      ANFPDR = pdr3
124900080923     C     Kanf2         chain     TNanf01L
125000080923     c                   clear                   danf2
125100080923     C                   EVAL      §ANF2COR = i74imp
125200080924     c                   z-add     o74impco      comico           11 2
125300080924     C                   z-add     comico        §ANF2TGC
125400080923     C                   EVAL      §ANF2GPI = o74gpi
125500080923     C                   EVAL      §ANF2VAR = o74percau
125600080924     c                   z-add     o74impa       comico
125700080924     C                   z-add     comico        §ANF2ADG
125800080924     c                   add       comico        imponibile
125900080925     c* aggiorno l'importo della contropartita con l'adeguamento carburante
126000080924     c                   z-add     1             xy
126100080924     c     v1tipf        lookup    tipf(xy)                               56
126200080924     c                   if        *in56
126300080924     c                   add       comico        impf(xy)
126400080925     c* aggiorno anche il sfl 2
126500080925     c                   z-add     comico        comsfl2
126600080925     c                   exsr      upsfl2
126700080924     c                   end
126800080923     C                   EVAL      ANFUNI = danf2
126900080923     c                   if        %found(tnanf01l)
127000080923     C                   update    tnanf000
127100080923     c                   else
127200080923     C                   write     tnanf000
127300080923     c                   end
127400080923     C                   ENDSR
127500080923     c*----------------------------------------------------------------
127600080923     c     writebhp      begsr
127700080923     c*----------------------------------------------------------------
127800031204     c*    kope          chain     anope01l
127900031204     c*                  if        %found(anope01l)
128000020708     c     kdft          chain     andft01l
128100020709     c                   if        %found(andft01l)
128200020708     c                   move      bhmrifint     bhprifint
128300020708     c                   move      bhmsubnum     bhpsubnum
128400020708     c                   move      bhmsubrig     bhpsubrig
128500020708     c                   z-add     1             bhpsubsubr
128600020708     c                   move      bhmdtpar      bhpdtpar
128700020708     c                   z-add     bhmnrpar      bhpnrpar
128800020709    >C                   EVAL      BHPimporto = bhmimporto
128900020709    >C                   EVAL      BHPimportd = bhmimportd
129000020709    >C                   EVAL      BHPdivisa  = bhmdivisa
129100020709    >C                   EVAL      BHPsocieta = BHMsocieta
129200020709    >C                   EVAL      BHPsegno   = Bhmsegno
129300020709    >C                   EVAL      BHPcambio  = Bhmcambio
129400020708     c                   eval      bhppagman = *off
129500020708     c                   eval      bhpbkamm = dftbkamm
129600020708     c                   write     ndbhp000
129700031204     c*                  end
129800020708     c                   end
129900020708     C                   ENDSR
130000020708     c*----------------------------------------------------------------
130100020708     c     writebha      begsr
130200020708     c*----------------------------------------------------------------
130300030205     c                   z-add     0             bhasubsubr
130400030205     c                   do        10            xyz
130500030204     c     xyz           chain     y350s02                            56
130600030205     c                   if        *in56
130700030205     c                   leave
130800030205     c                   else
130900030204     c* seleziono il tipo forn./servizio
131000030204     c                   if        tipf(x) <> h1tipf
131100030204     c                   iter
131200030204     c                   end
131300030204     c* seleziono le competenze dell'anno che sto analizzando
131400030204     c                   if        totimpt <> 0
131500030204     c                   if        h1ccmp <> 13
131600030204     c                   iter
131700030204     c                   end
131800030204     c                   else
131900030204     c                   if        h1ccmp = 13
132000030204     c                   iter
132100030204     c                   end
132200030204     c                   end
132300030204     c*
132400030205     c                   add       1             bhasubsubr
132500980914     c                   move      bhmrifint     bharifint
132600980914     c                   move      bhmsubnum     bhasubnum
132700980914     c                   move      bhmsubrig     bhasubrig
132800980914     c                   move      'D1'          bhadimen
132900980914     c                   move      xscsoc        bhasocieta
133000980914     c                   add       1             bhasubsubr
133100030204     c                   z-add     h1cico        bhaimporto
133200020417     c                   z-add     bhaimporto    bhaimportd
133300031204     c                   if        segnofor = 'A'
133400980914     c                   move      'D'           bhasegno
133500031204     c                   else
133600031204     c                   move      'A'           bhasegno
133700031204     c                   end
133800030204     c                   if        §4zcdb <> *blanks
133900030204     c                   movel     §4zcdb        bhaentit1
134000030204     c                   else
134100030204     c                   movel     v1cuni        bhaentit1
134200030204     c                   end
134300020408     c                   movel     §4zvoc        bhavoce
134400000426      * Se devo gestire la comp. 13 imposto data analitica anno preced.
134500030204     c                   if        h1ccmp = 13
134600980916     c                   move      dtcoan        bhadtcoan
134700020415     c                   else
134800000426      * Se non devo gestire la comp. 13 imposto data analitica=data reg.
134900020417     c                   move      bhmdtreg      campo10          10
135000020409     c* e gg. data reg. = primo del mese
135100020417     c                   move      '-01'         campo5            5
135200030204     c                   movel     h1ccmp        campo5
135300020417     c                   move      campo5        campo10
135400020417     c                   move      campo10       bhadtcoan
135500020415     c                   end
135600980916     c                   write     ndbha000
135700030204     c                   end
135800030204     c                   enddo
135900980914     c                   endsr
136000980914     c*-----------------------------------------
136100980914     c     writebhi      begsr
136200980914     c*-----------------------------------------
136300980914      * scrivo eventuali righe iva
136400031204     c                   if        segnofor = 'A'
136500020410     c                   move      'D'           bhisegno
136600031204     c                   else
136700031204     c                   move      'A'           bhisegno
136800031204     c                   end
136900020410     c                   move      xscsoc        bhisocieta
137000020410     c                   add       1             bhmsubrig
137100020410     c                   z-add     bhmsubrig     bhisubrig
137200020410     c                   move      bhmrifint     bhirifint
137300020410     c                   move      bhmsubnum     bhisubnum
137400020418     c                   z-add     imponibile    bhiimpond
137500020417     c                   z-add     bhiimpond     bhiimpon
137600020410     c                   move      '1'           bhitpreg
137700150108      **
137800150108     c* se si tratta di PULIZIE delle COOP
137900150119     c*  NON deve essere passato il libro IVA (dal 2015) perchè lo
138000150119     c* prende dall'anagrafica della causale
138100150108     c                   if        v1tipf = 'P' and f50TIP = 'C'
138200150119     c                             and impiva <> 0
138300150108     c                   clear                   bhilibro
138400150108     c                   else
138500020410     c                   move      §ivlaq        bhilibro
138600150108     c                   end
138700150108      **
138800020418     c                   z-add     impiva        bhiimportd
138900020417     c                   z-add     bhiimportd    bhiimporto
139000031204     c                   clear                   bhirifiva
139100031204     c                   clear                   bhiali
139200031204     c* se NON presente iva sull'anagrafico del fornitore prendo dalla tab.
139300031204      * y4z
139400031204     c                   if        v1civc = *blanks
139500031204     c                   if        §4ziva <>0
139600031204      * iva
139700020417     c                   z-add     §4ziva        bhiali
139800020417     c                   else
139900031204      * esenzioni
140000031204     c                   move      §4zivc        bhirifiva
140100020417     c                   end
140200020417     c                   else
140300031204      * se presente iva nell'anagrafico fornitore prendo dalla tabella
140400031204     c                   if        alig48 <> 0
140500031204      * iva
140600031204     c                   z-add     alig48        bhiali
140700031204     c                   else
140800031204      * esenzioni
140900020417     c                   move      kscg48        bhirifiva
141000020417     c                   endif
141100031204     c                   endif
141200020410     c                   write     ndbhi000
141300020408     c*
141400980914     c                   endsr
141500980916     c*-----------------------------------------
141600030206     c     sriva         begsr
141700980916     c*-----------------------------------------
141800030206     c* iva 13,2
141900030206     c                   z-add     0             impiva
142000020417     c* se il fornitore ha un codice IVA
142100020417     C                   CLEAR                   tabg48
142200020417     c                   if        v1civc <> *blanks
142300020417     C                   CLEAR                   X06DS
142400020417     C                   MOVE      xscsoc        X06SOC
142500020417     C                   MOVE      XSCLIN        X06LIN
142600020417     C                   MOVE      v1civc        X06CIV
142700020417     C                   MOVE      f50dpr        X06DAT
142800020417     C                   CALL      'X06G48'
142900020417     C                   PARM                    X06DS
143000020417     C     X06ERR        IFEQ      '0'
143100020417     C                   MOVEL     X06UNI        TABG48
143200020417     C                   end
143300020417     C                   end
143400020417     c* aliquota IVA
143500020417     c                   if        v1civc = *blanks or alig48 <> 0
143600020418     c                   if        alig48 = 0
143700020418     c                   z-add     §4ziva        bhiali
143800020418     c                   else
143900020418     c                   z-add     alig48        bhiali
144000020418     c                   end
144100020418     c     imponibile    MULT      bhiali        WTOTIV           17 5
144200020418     C     WTOTIV        DIV(H)    100           impiva
144300020417     c                   endif
144400020417     c*
144500020417     c                   endsr
144600020417     c*-----------------------------------------
144700020417     c     competenza    begsr
144800020417     c*-----------------------------------------
144900000426      * scrittura righe registrazione anno precedente
145000020409     c                   z-add     bhmsubnum     savsubnum
145100020409     c                   move      bhmsubrig     savsubrig
145200020409     c                   z-add     riga2         bhmnrasreg
145300020409     c                   z-add     1             bhmsubrig
145400020409     c                   add       1             subnum
145500020409     c                   z-add     subnum        bhmsubnum
145600980916     c                   move      dtcoan        bhmdtreg
145700020409     c                   z-add     f50npr        bhmnrreg
145800020527     c* Imposto le causali contabili relative alla NON autofatturazione
145900020527     c                   if        f50dft <> 0
146000020527     c                   move      §4zcrr        bhmcaustes
146100020527     c                   else
146200020527     c                   move      §4zcrra       bhmcaustes
146300020527     c                   end
146400020418     c                   moveL     'A'           bhmfungen
146500020418     c                   clear                   bhmserreg
146600020418     c                   clear                   bhmdtdoc
146700980916     c                   clear                   bhmnrdoc
146800980916     c                   clear                   bhmdtpar
146900980916     c                   clear                   bhmnrpar
147000031204     c                   move      segnofor      bhmsegno
147100030204     c                   z-add     totimpt       bhmimporto
147200020417     c                   z-add     bhmimporto    bhmimportd
147300980916     c                   write     ndbhm000
147400980916      *
147500030204     c                   do        10            x
147600030204     c                   if        tipf(x) = *blanks
147700030204     c                   leave
147800030204     c                   end
147900030204     c                   if        impt(x) = 0
148000030204     c                   iter
148100030204     c                   end
148200030204     c* per ogni tipologia servizio aggancio la tabella Y4Z
148300030204     c* solo se sono stati accorpati i servizi altrimenti l'ho già letta
148400030204     c* inizialmente
148500030204     c                   if        f50tsr = 'S' and tipf(x) <> v1tips
148600030206     c                   eval      v1tip = tipf(x)
148700030204     c                   exsr      sry4z
148800030204     c                   movel     tipf(x)       v1tips
148900030204     c                   end
149000020408     c                   move      §4zkcc        bhmkcc
149100020408     c                   move      §4zksc        bhmksc
149200031204     c                   if        segnofor = 'A'
149300980916     c                   move      'D'           bhmsegno
149400031204     c                   else
149500031204     c                   move      'A'           bhmsegno
149600031204     c                   end
149700980916     c                   add       1             bhmsubrig
149800030204     c                   z-add     impt(x)       bhmimporto
149900030204     c                   z-add     bhmimporto    bhmimportd
150000980916     c                   write     ndbhm000
150100030204     c* analitica
150200030204     c                   exsr      writebha
150300030204     c                   enddo
150400980916     c                   endsr
150500951011?     *--------------------------------------------------------------*
150600031121?     *  AGGOtt: Aggiorno prestaz.occasionali con numero e data fatt.
150700020411      *          e numero assoluto
150800951011?     *--------------------------------------------------------------*
150900031121     C     aggott        BEGSR
151000031121     c                   open      fiott06l
151100020411     c* se trovata registrazione
151200031121     C     Kott          setll     fiott06L
151300020411     c                   do        *hival
151400031121     C     Kott          reade     fiott06L
151500031121     C                   if        %eof(fiott06l)
151600020411     c                   leave
151700020411     c                   end
151800020418     c* data conteggio compresa nei limiti richiesti
151900031121     c                   if        ottddc > f50dtf or ottddc < f50dti
152000020418     c                   iter
152100020418     c                   end
152200020418     c* se già fatturato iter
152300031121     c                   if        ottnff <> 0
152400020418     c                   iter
152500020418     c                   end
152600031121     C                   move      savdtdoc      ottDFT
152700031121     C                   Z-ADD     savnrdoc      ottnff
152800031121     C                   Z-ADD     savdcn        ottdcn
152900031121     c                   z-add     riga1         ottnra
153000031121     c                   move      ottddc        dataiso
153100031121     c                   extrct    dataiso:*m    mm
153200031121     c                   move      mm            com02             2
153300031121     c                   if        com02 > meseprt
153400031121     C                   move      dtcoan        dataiso
153500031121     C                   move      dataiso       ottdtr
153600031121     c                   else
153700031121     C                   z-add     f50dpr        ottdtr
153800031121     c                   end
153900041111     c                   if        riga1 <> 0
154000031121     C                   z-add     f50npr        ottnrg
154100041111     c                   else
154200041111     C                   clear                   ottnrg
154300041111     c                   end
154400031121     C                   EXCEPT    UPDott
154500020411     C                   enddo
154600020411     c*
154700030204     C                   if        riga2 <> 0 and riga1 <> 0
154800020418      * Se ho effettuato le registrazioni impostando la compet. 13
154900020418      * aggancio Ndreg e imposto il numero assoluto di registraz.
155000020418     C                   CALL      'YNRASCOL'
155100020418     C                   PARM                    KSYS
155200020418     C                   PARM                    riga2
155300020418     C                   PARM                    riga1
155400020418     C                   PARM      *off          CMT               1
155500020418     C                   PARM      ' '           ESE               1
155600020418     C                   end
155700031121     c                   close     fiott06l
155800000204      *
155900951011     C                   ENDSR
156000031121?     *--------------------------------------------------------------*
156100031121?     *  AGGftt: Aggiorno autotrasportatori con numero e data fatt.
156200031121      *          e numero assoluto
156300031121?     *--------------------------------------------------------------*
156400031121     C     AGGftt        BEGSR
156500031121     c                   open      fiftt06l
156600031121     c*                  setoff                                       4041
156700031121     c* se trovata registrazione
156800031121     C     Kftt          setll     fiftt06L
156900031121     c                   do        *hival
157000031121     C     Kftt          reade     fiftt06L
157100031121     C                   if        %eof(fiftt06l)
157200031121     c                   leave
157300031121     c                   end
157400031121     c* data conteggio compresa nei limiti richiesti
157500031121     c                   if        fttddc > f50dtf or fttddc < f50dti
157600031121     c                   iter
157700031121     c                   end
157800031121     c* se già fatturato iter
157900031121     c                   if        fttnff <> 0
158000031121     c                   iter
158100031121     c                   end
158200031121     C                   move      savdtdoc      fttDFT
158300031121     C                   Z-ADD     savnrdoc      fttnff
158400031121     C                   Z-ADD     savdcn        fttdcn
158500031121     c                   if        riga1 <> 0
158600031121     c                   movel     fttflr        dftt01
158700031121     c                   z-add     riga1         §fttnrg
158800031121     C                   movel(p)  dftt01        fttflr
158900031121     c                   end
159000031121     C                   EXCEPT    UPDftt
159100031121     C                   enddo
159200031121     c*
159300031121     C                   if        riga2 <> 0 and riga1 <> 0
159400031121      * Se ho effettuato le registrazioni impostando la compet. 13
159500031121      * aggancio Ndreg e imposto il numero assoluto di registraz.
159600031121     C                   CALL      'YNRASCOL'
159700031121     C                   PARM                    KSYS
159800031121     C                   PARM                    riga2
159900031121     C                   PARM                    riga1
160000031121     C                   PARM      *off          CMT               1
160100031121     C                   PARM      ' '           ESE               1
160200031121     C                   end
160300031121     c                   close     fiftt06l
160400031121      *
160500031121     C                   ENDSR
160600030128?     *--------------------------------------------------------------*
160700160108?     *  AGGsre: Aggiorno rettifiche  con numero e data fatt.
160800030128      *          e numero assoluto
160900030128?     *--------------------------------------------------------------*
161000160108     C     AGGsre        BEGSR
161100160108     c* se trovata registrazione
161200160108      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
161300160108     c                   if        f50tsr = 'S'
161400160108     c                               and v1tipf <>'P'
161500160108     C     Ksre          setll     tnsre01L
161600160108     c                   else
161700160108     C     Ksre1         setll     tnsre01L
161800160108     c                   end
161900160108     c                   do        *hival
162000160108     c                   if        f50tsr = 'S'
162100160108     c                               and v1tipf <>'P'
162200160108     C     Ksre          reade     tnsre01L
162300160108     c                   else
162400160108     C     Ksre1         reade     tnsre01L
162500160108     c                   end
162600160108     C                   if        %eof(tnsre01l)
162700160108     c                   leave
162800160108     c                   end
162900160108     c* data conteggio compresa nei limiti richiesti
163000160108     c                   if        sreddc > f50dtf or sreddc < f50dti
163100160108     c                   iter
163200160108     c                   end
163300160108     c* se già fatturato iter
163400160108     c                   if        srenff <> 0
163500160108     c                   iter
163600160108     c                   end
163700160108ab   c* Non deve accorpare le Pulizie
163800160108ab   c                   if        f50tsr = 'S'
163900160108ab   c                               and v1tipf <>'P'
164000160108ab   c                               and sreTSR  ='P'
164100160108ab   c                   iter
164200160108ab   c                   end
164300160108     C                   move      savdtdoc      sreDFT
164400160108     C                   Z-ADD     savnrdoc      srenff
164500160108     C                   Z-ADD     savdcn        sredcn
164600160108     c                   z-add     riga1         srenra
164700160108     c                   move      sreddc        dataiso
164800160108     c                   extrct    dataiso:*m    mm
164900160108     c                   move      mm            com02             2
165000160108     c                   if        com02 > meseprt
165100160108     C                   move      dtcoan        dataiso
165200160108     C                   move      dataiso       sredtr
165300160108     c                   else
165400160108     C                   z-add     f50dpr        sredtr
165500160108     c                   end
165600160108     c                   if        riga1 <> 0
165700160108     C                   z-add     f50npr        srenrg
165800160108     C                   else
165900160108     C                   clear                   srenrg
166000160108     c                   end
166100160108     C                   EXCEPT    UPDsre
166200160108     C                   enddo
166300160108     c*
166400160108     C                   if        riga2 <> 0 and riga1 <> 0
166500160108      * Se ho effettuato le registrazioni impostando la compet. 13
166600160108      * aggancio Ndreg e imposto il numero assoluto di registraz.
166700160108     C                   CALL      'YNRASCOL'
166800160108     C                   PARM                    KSYS
166900160108     C                   PARM                    riga2
167000160108     C                   PARM                    riga1
167100160108     C                   PARM      *off          CMT               1
167200160108     C                   PARM      ' '           ESE               1
167300160108     C                   end
167400160108      *
167500160108     C                   ENDSR
167600160108?     *--------------------------------------------------------------*
167700160108?     *  AGGctt: Aggiorno cooperative con numero e data fatt.
167800160108      *          e numero assoluto
167900160108?     *--------------------------------------------------------------*
168000160108     C     AGGctt        BEGSR
168100030704     C                   IF        NOT %OPEN(FICTT06L)
168200030704     C                   OPEN      FICTT06L
168300030704     C                   END
168400030620     c                   move      'C'           cttfvl
168500030128     c* se trovata registrazione
168600150109      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
168700030128     c                   if        f50tsr = 'S'
168800150109     c                               and v1tipf <>'P'
168900030128     C     Kctt          setll     fictt06L
169000030128     c                   else
169100030128     C     Kctt1         setll     fictt06L
169200030128     c                   end
169300030128     c                   do        *hival
169400030128     c                   if        f50tsr = 'S'
169500150109     c                               and v1tipf <>'P'
169600030128     C     Kctt          reade     fictt06L
169700030128     c                   else
169800030128     C     Kctt1         reade     fictt06L
169900030128     c                   end
170000030128     C                   if        %eof(fictt06l)
170100030128     c                   leave
170200030128     c                   end
170300030128     c* data conteggio compresa nei limiti richiesti
170400030128     c                   if        cttddc > f50dtf or cttddc < f50dti
170500030128     c                   iter
170600030128     c                   end
170700030128     c* se già fatturato iter
170800030128     c                   if        cttnff <> 0
170900030128     c                   iter
171000030128     c                   end
171100150204ab   c* Non deve accorpare le Pulizie
171200150204ab   c                   if        f50tsr = 'S'
171300150204ab   c                               and v1tipf <>'P'
171400150204ab   c                               and cttTSR  ='P'
171500150204ab   c                   iter
171600150204ab   c                   end
171700030128     C                   move      savdtdoc      cttDFT
171800030128     C                   Z-ADD     savnrdoc      cttnff
171900030128     C                   Z-ADD     savdcn        cttdcn
172000030128     c                   z-add     riga1         cttnra
172100030204     c                   move      cttddc        dataiso
172200030204     c                   extrct    dataiso:*m    mm
172300030204     c                   move      mm            com02             2
172400030204     c                   if        com02 > meseprt
172500030211     C                   move      dtcoan        dataiso
172600030211     C                   move      dataiso       cttdtr
172700030128     c                   else
172800030128     C                   z-add     f50dpr        cttdtr
172900030128     c                   end
173000041111     c                   if        riga1 <> 0
173100030128     C                   z-add     f50npr        cttnrg
173200041111     C                   else
173300041111     C                   clear                   cttnrg
173400041111     c                   end
173500030128     C                   EXCEPT    UPDctt
173600030128     C                   enddo
173700030128     c*
173800030204     C                   if        riga2 <> 0 and riga1 <> 0
173900030128      * Se ho effettuato le registrazioni impostando la compet. 13
174000030128      * aggancio Ndreg e imposto il numero assoluto di registraz.
174100030128     C                   CALL      'YNRASCOL'
174200030128     C                   PARM                    KSYS
174300030128     C                   PARM                    riga2
174400030128     C                   PARM                    riga1
174500030128     C                   PARM      *off          CMT               1
174600030128     C                   PARM      ' '           ESE               1
174700030128     C                   end
174800030704     C                   IF        %OPEN(FICTT06L)
174900030128     c                   close     fictt06l
175000030704     C                   END
175100030128      *
175200030128     C                   ENDSR
175300030620?     *--------------------------------------------------------------*
175400030620?     *  ctrctt: Controllo se ho dei conteggi non confermati
175500030620      *          intal caso segnalo l'anomalia
175600030620?     *--------------------------------------------------------------*
175700030620     C     ctrctt        BEGSR
175800030620     c                   move      ' '           cttfvl
175900030620     c                   eval      noconf = ' '
176000030620     c* se trovata registrazione
176100150109      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
176200030620     c                   if        f50tsr = 'S'
176300150109     c                               and v1tipf <>'P'
176400030620     C     Kctt          setll     fictt06L
176500030620     c                   else
176600030620     C     Kctt1         setll     fictt06L
176700030620     c                   end
176800030620     c                   do        *hival
176900030620     c                   if        f50tsr = 'S'
177000150109     c                               and v1tipf <>'P'
177100030620     C     Kctt          reade     fictt06L
177200030620     c                   else
177300030620     C     Kctt1         reade     fictt06L
177400030620     c                   end
177500030620     C                   if        %eof(fictt06l)
177600030620     c                   leave
177700030620     c                   end
177800030620     c* data conteggio compresa nei limiti richiesti
177900030620     c                   if        cttddc > f50dtf or cttddc < f50dti
178000030620     c                   iter
178100030620     c                   end
178200030620     c* se già fatturato iter
178300030620     c                   if        cttnff <> 0
178400030620     c                   iter
178500030620     c                   end
178600150204ab   c* Non deve accorpare le Pulizie
178700150204ab   c                   if        f50tsr = 'S'
178800150204ab   c                               and v1tipf <>'P'
178900150204ab   c                               and cttTSR  ='P'
179000150204ab   c                   iter
179100150204ab   c                   end
179200150204      *
179300030620     c                   eval      noconf = '1'
179400030827     c                   leave
179500030620     C                   enddo
179600030620      *
179700030620     C                   ENDSR
179800030827?     *--------------------------------------------------------------*
179900030827?     *  AGGatt: Aggiorno aff/defl. con numero e data fatt.
180000030827      *          e numero assoluto
180100030827?     *--------------------------------------------------------------*
180200030827     C     AGGatt        BEGSR
180300030827     C                   IF        NOT %OPEN(FIaTT06L)
180400030827     C                   OPEN      FIaTT06L
180500030827     C                   END
180600030828     c                   eval      attflg = 'C'
180700030827     c* se trovata registrazione
180800030827     C     Katt          setll     fiatt06L
180900030827     c                   do        *hival
181000030828     C     Katt          reade     fiatt06L
181100030827     C                   if        %eof(fiatt06l)
181200030827     c                   leave
181300030827     c                   end
181400030827     c* data conteggio compresa nei limiti richiesti
181500030827     c                   if        attddc > f50dtf or attddc < f50dti
181600030827     c                   iter
181700030827     c                   end
181800030827     c* se già fatturato iter
181900030827     c                   if        attnff <> 0
182000030827     c                   iter
182100030827     c                   end
182200030827     C                   move      savdtdoc      attDFT
182300030827     C                   Z-ADD     savnrdoc      attnff
182400030827     C                   Z-ADD     savdcn        attdcn
182500030827     c                   z-add     riga1         attnra
182600030827     c                   move      attddc        dataiso
182700030827     c                   extrct    dataiso:*m    mm
182800030827     c                   move      mm            com02             2
182900030827     c                   if        com02 > meseprt
183000030827     C                   move      dtcoan        dataiso
183100030827     C                   move      dataiso       attdtr
183200030827     c                   else
183300030827     C                   z-add     f50dpr        attdtr
183400030827     c                   end
183500041111     c                   if        riga1 <> 0
183600030827     C                   z-add     f50npr        attnrg
183700041111     c                   else
183800041111     c                   clear                   attnrg
183900041111     c                   end
184000030827     C                   EXCEPT    UPDatt
184100030827     C                   enddo
184200030827     c*
184300030827     C                   if        riga2 <> 0 and riga1 <> 0
184400030827      * Se ho effettuato le registrazioni impostando la compet. 13
184500030827      * aggancio Ndreg e imposto il numero assoluto di registraz.
184600030827     C                   CALL      'YNRASCOL'
184700030827     C                   PARM                    KSYS
184800030827     C                   PARM                    riga2
184900030827     C                   PARM                    riga1
185000030827     C                   PARM      *off          CMT               1
185100030827     C                   PARM      ' '           ESE               1
185200030827     C                   end
185300030827     C                   IF        %OPEN(FIaTT06L)
185400030827     c                   close     fiatt06l
185500030827     C                   END
185600030827      *
185700030827     C                   ENDSR
185800030827?     *--------------------------------------------------------------*
185900030827?     *  ctratt: Controllo se ho dei conteggi non confermati
186000030827      *          intal caso segnalo l'anomalia
186100030827?     *--------------------------------------------------------------*
186200030827     C     ctratt        BEGSR
186300030828     c                   eval      attflg = ' '
186400030827     c                   eval      noconf = ' '
186500030827     c* se trovata registrazione
186600030827     C     Katt          setll     fiatt06L
186700030827     c                   do        *hival
186800030827     C     Katt          reade     fiatt06L
186900030827     C                   if        %eof(fiatt06l)
187000030827     c                   leave
187100030827     c                   end
187200030827     c* data conteggio compresa nei limiti richiesti
187300030827     c                   if        attddc > f50dtf or attddc < f50dti
187400030827     c                   iter
187500030827     c                   end
187600030827     c                   eval      noconf = '1'
187700030827     c                   leave
187800030827     C                   enddo
187900030827      *
188000030827     C                   ENDSR
188100980914     c*-----------------------------------------
188200980914     c     primanota     begsr
188300980914     c*-----------------------------------------
188400980914     C* contabilizzazione batch
188500980916     c                   clear                   ndc071ds
188600980914     C                   MOVE      xscsoc        SOCieta071
188700020411     C                   eval      rifint071=bhmrifint
188800980915     c                   move      *loval        dtregda071
188900980915     c                   move      *hival        dtrega071
189000980915     c                   move      *loval        subnumd071
189100980915     c                   move      *hival        subnuma071
189200980914     C                   MOVE      *off          solcont071
189300980916    >C                   MOVE      *off          interat071
189400980916     C                   move      *off          err071
189500980915     c                   move      *off          control071
189600020411     c                   move      *on           unocmt071
189700020411     C                   MOVEL(P)  NDC071DS      kpjbu
189800020411     C*
189900980916     C                   CALL      'NDC071C'
190000980914     C                   parm                    kpjba
190100980914     C                   movel     kpjbu         ndc071ds
190200020411     C* in caso di errore emettere video con segnalazione
190300020411     C                   if        err071=*on
190400020411     C* avviso il pgm chiamante che ci sono stati errori e gli do la possibi
190500020411     C* lità di manutenzione p. n. batch in interattivo
190600020411     c                   reset                   NDC050DS
190700020411     c                   move      xscsoc        societa050
190800020411     C                   eval      rifint050=bhmrifint
190900020411     c                   movel(P)  ndc050ds      kpjbu
191000020411     c     ret050        downe     '2'
191100020411JRN  C                   CALL      'NDC050R'
191200020411JRN  C                   PARM                    kpjba
191300020411     c                   movel(P)  kpjbu         ndc050ds
191400020411     c                   enddo
191500020411-4   C                   EndIf
191600980914     C*
191700980914     c                   endsr
191800980911     C*----------------------------------------------------*
191900980911     C* Reperimento dati società
192000980911     C*----------------------------------------------------*
192100980911     C     REPSOC        BEGSR
192200980911     C*
192300980911     C                   CALLB     'XSOC'
192400980911     C                   PARM                    TIPXSC            6
192500980911     C                   PARM                    SOCXSC            3
192600980911     C                   PARM                    CDSXSC            9 0
192700980911     C                   PARM                    MODXSC            3
192800980911     C                   PARM      *blanks       RTNXSC            1
192900980911     C                   PARM                    XSOCDS
193000980911     C                   PARM                    KPJBA
193100980911     C*
193200980911     C                   ENDSR
193300030206     c*----------------------------------------------------------------
193400030206     c     upsfl2        begsr
193500030206     c*----------------------------------------------------------------
193600030206     c                   do        10            xyz
193700030206     c     xyz           chain     y350s02                            56
193800030206     c                   if        *in56
193900030206     c                   leave
194000030206     c                   else
194100030206     c* seleziono il tipo forn./servizio
194200030206     c                   if        h1tipf <> v1tipf
194300030206     c                   iter
194400030206     c                   end
194500030206     c                   if        h1ccmp = 13
194600030206     c                   iter
194700030206     c                   end
194800080925     c                   add       comsfl2       h1cico
194900030206     c                   update    y350s02
195000030206     c                   leave
195100030206     c                   end
195200030206     c                   enddo
195300030206     c                   endsr
195400951011?     *--------------------------------------------------------------*
195500020527?     *  lettura tabella Y4Z                                         *
195600951011?     *--------------------------------------------------------------*
195700020527     C     sry4z         BEGSR
195800020527     c*  lettura tabella Y4Z
195900020527     c                   clear                   xatbds
196000020527     c                   move      '1'           xtbric
196100020527     c                   move      *blank        xtbkey
196200020527     c                   move      'Y4Z'         xtbcod
196300030206     c                   movel     v1tip         xtbkey
196400020527     c                   call      'XATB'
196500020527     c                   parm                    xatbds
196600020527     c                   if        xtberr = '0'
196700020527     c                   movel     xtbuni        angy4z
196800020527     c                   else
196900020527     c                   clear                   angy4z
197000020527     c                   end
197100031204     C*
197200031204     C                   ENDSR
197300031204?     *--------------------------------------------------------------*
197400031204     c* aggancio la causale contabile per reperire il segno
197500031204?     *--------------------------------------------------------------*
197600031204     C     srope         BEGSR
197700031204     c                   clear                   segnofor
197800031204     c     kope          chain     anope01l
197900031204     c                   if        %found(anope01l) and opetpreg = '1'
198000031204     c                   select
198100031204     c                   when      opetpdoci = '0'
198200031204     c                   eval      segnofor = 'A'
198300031204     c                   when      opetpdoci = '1'
198400031204     c                   eval      segnofor = 'D'
198500031204     c                   endsl
198600031204     c                   end
198700020527     C*
198800020527     C                   ENDSR
198900020527?     *--------------------------------------------------------------*
199000030128?     *  istruz: Operazioni di inizializzazione dati                 *
199100020527?     *--------------------------------------------------------------*
199200030128     C     istruz        BEGSR
199300030827     c                   select
199400031121     c* prestazioni occasionali
199500031121     c                   when      f50cau<> ' '
199600030128     C                   EVAL      WrkSqlCmd
199700030128     C                             =
199800031121     C                             'SELECT ottcdf,'
199900030128     C                             +
200000031121     C                             ' sum(otttim) '
200100030128     C                             +
200200031121     C                             ' from fiott06l where ottsoc = '
200300030128     C                             +
200400030128     C                             '''' + f50soc  + ''''
200500030128     C                             +
200600030128     C                             ' AND '
200700030128     C                             +
200800031121     C                             ' ottcdf between '
200900030128     C                             +
201000030128     C                             '''' + f50pd1 + ''''
201100030128     C                             +
201200030129     C                             ' AND '
201300030128     C                             +
201400030128     C                             '''' + f50pd2 + ''''
201500030128     C                             +
201600030128     C                             ' AND '
201700030128     C                             +
201800031121     C                             ' otttip = '
201900031121     C                             +
202000031121     C                             '''' + f50tip + ''''
202100031121     C                             +
202200031121     C                             ' AND '
202300031121     C                             +
202400031121     C                             ' otttab = '
202500031121     C                             +
202600031121     C                             '''' + f50cau + ''''
202700030128     C                             +
202800030128     C                             ' AND '
202900030128     C                             +
203000031121     C                             ' ottddc BETWEEN '
203100030129     C                             + %editc(f50dti:'X')
203200030129     C                             +
203300030129     C                             ' AND ' + %editc(f50dtf:'X')
203400030128     C                             +
203500030129     C                             ' AND '
203600030128     C                             +
203700031121     C                             'ottnff = 0  group by ottcdf '
203800030203     c                             +
203900031121     C                             ' ORDER BY ottcdf'
204000031121     c* autotrasportatori
204100031121     c                   when      f50tip = 'A'
204200031121     C                   EVAL      WrkSqlCmd
204300031121     C                             =
204400031121     C                             'SELECT fttcdf,'
204500031121     C                             +
204600031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
204700031121     C                             +
204800031121     C                             'fttmnt)'
204900031121     C                             +
205000031121     C                             ' from fiftt06l where fttsoc = '
205100031121     C                             +
205200031121     C                             '''' + f50soc  + ''''
205300031121     C                             +
205400031121     C                             ' AND '
205500031121     C                             +
205600031121     C                             ' fttcdf between '
205700031121     C                             +
205800031121     C                             '''' + f50pd1 + ''''
205900031121     C                             +
206000031121     C                             ' AND '
206100031121     C                             +
206200031121     C                             '''' + f50pd2 + ''''
206300031121     C                             +
206400031121     C                             ' AND '
206500031121     C                             +
206600031121     C                             ' ftttsr = '' '''
206700031121     C                             +
206800031121     C                             ' AND '
206900031121     C                             +
207000031121     C                             ' fttfvl = ''C'''
207100031121     C                             +
207200031121     C                             ' AND '
207300031121     C                             +
207400031121     C                             ' Fttddc BETWEEN '
207500031121     C                             + %editc(f50dti:'X')
207600031121     C                             +
207700031121     C                             ' AND ' + %editc(f50dtf:'X')
207800031121     C                             +
207900031121     C                             ' AND '
208000031121     C                             +
208100031121     C                             'fttnff = 0  group by fttcdf '
208200031121     c                             +
208300031121     C                             ' ORDER BY fttcdf'
208400030128     c* cooperative
208500030827     c                   when      f50tip = 'C'
208600030128     C                   EVAL      WrkSqlCmd
208700030128     C                             =
208800030128     C                             'SELECT cttcdf,'
208900030128     C                             +
209000030205     C                             ' sum(cttitc+cttitma+cttitmp)'
209100150108      * se Tipo serviz.in dettaglio
209200150108      *  oppure se in accorpamento e primo giro per le PULIZIE a parte extra accorpati
209300030128     C                   IF        f50tsr = 'N'
209400150108     c                                or
209500150108     c                             Coop_PULIZIE_a_parte = 'S'
209600030128     C                   EVAL      WrkSqlCmd
209700030128     C                             =
209800030128     C                             %TRIMR(WrkSqlCmd)
209900030128     C                             +
210000030205     C                             ', ctttsr '
210100030128     C                   ENDIF
210200150108      * from
210300030128     C                   EVAL      WrkSqlCmd
210400030128     C                             =
210500030128     C                             %TRIMR(WrkSqlCmd)
210600030128     C                             +
210700030128     C                             ' from fictt06l where cttsoc = '
210800030128     C                             +
210900030128     C                             '''' + f50soc  + ''''
211000030128     C                             +
211100030128     C                             ' AND '
211200030128     C                             +
211300030129     C                             ' cttcdf between '
211400030128     C                             +
211500030128     C                             '''' + f50pd1 + ''''
211600030128     C                             +
211700030129     C                             ' AND '
211800030128     C                             +
211900030128     C                             '''' + f50pd2 + ''''
212000030128     C                             +
212100030128     C                             ' AND '
212200030128     C                             +
212300030128     C                             ' cttfvl = ''C'''
212400030128     C                             +
212500030128     C                             ' AND '
212600030128     C                             +
212700030128     C                             ' cttddc BETWEEN '
212800030129     C                             + %editc(f50dti:'X')
212900030129     C                             +
213000030129     C                             ' AND ' + %editc(f50dtf:'X')
213100030128     C                             +
213200030128     C                             ' AND'
213300030128     C                             +
213400150108     C                             ' cttnff = 0 '
213500150108      *
213600150108      *  se Accorpato e Pulizie a parte (1°giro)
213700150108      *   filtra SOLO le PULIZIE
213800150108     c                   IF        Coop_PULIZIE_a_parte = 'S'
213900150108     C                   EVAL      WrkSqlCmd
214000150108     C                             =
214100150108     C                             %TRIMR(WrkSqlCmd)
214200150108     C                             +
214300150108     C                             ' AND ctttsr = ''P''  '
214400150108     C                   ENDIF
214500150108      *
214600150108      *  se Accorpato e accorpa (2°giro) a parte
214700150108     c                   IF        Coop_PULIZIE_a_parte = 'N'
214800150108     C                   EVAL      WrkSqlCmd
214900150108     C                             =
215000150108     C                             %TRIMR(WrkSqlCmd)
215100150108     C                             +
215200150108     C                             ' AND ctttsr <> ''P''  '
215300150108     C                   ENDIF
215400150108      *
215500150108      *
215600150108      * Group By
215700150108     C                   EVAL      WrkSqlCmd
215800150108     C                             =
215900150108     C                             %TRIMR(WrkSqlCmd)
216000150108     C                             +
216100150108     C                             ' group by cttcdf '
216200150108      *
216300150108      *  se Accorpato e Pulizie a parte (1°giro)
216400030128     C                   IF        f50tsr = 'N'
216500150108     c                                or
216600150108     c                             Coop_PULIZIE_a_parte = 'S'
216700030128     C                   EVAL      WrkSqlCmd
216800030128     C                             =
216900030128     C                             %TRIMR(WrkSqlCmd)
217000030128     C                             +
217100030203     C                             ', ctttsr'
217200030128     C                   ENDIF
217300150108      * Order By
217400030128     C                   EVAL      WrkSqlCmd
217500030128     C                             =
217600030128     C                             %TRIMR(WrkSqlCmd)
217700030128     C                             +
217800030203     C                             ' ORDER BY cttcdf '
217900150108      *
218000150108      *  se Accorpato e Pulizie a parte (1°giro)
218100030128     C                   IF        f50tsr = 'N'
218200150108     c                                or
218300150108     c                             Coop_PULIZIE_a_parte = 'S'
218400030128     C                   EVAL      WrkSqlCmd
218500030128     C                             =
218600030128     C                             %TRIMR(WrkSqlCmd)
218700030128     C                             +
218800030203     C                             ', ctttsr '
218900030128     C                   ENDIF
219000030827     c* aff/defl.
219100030827     c                   when      f50tip = 'D'
219200030827     C                   EVAL      WrkSqlCmd
219300030827     C                             =
219400030827     C                             'SELECT attcdf,'
219500030827     C                             +
219600030827     C                             ' sum(attimpp)'
219700030827     C                             +
219800030827     C                             ' from fiatt06l where attsoc = '
219900030827     C                             +
220000030827     C                             '''' + f50soc  + ''''
220100030827     C                             +
220200030827     C                             ' AND '
220300030827     C                             +
220400030827     C                             ' attcdf between '
220500030827     C                             +
220600030827     C                             '''' + f50pd1 + ''''
220700030827     C                             +
220800030827     C                             ' AND '
220900030827     C                             +
221000030827     C                             '''' + f50pd2 + ''''
221100030827     C                             +
221200030827     C                             ' AND '
221300030827     C                             +
221400030827     C                             ' attdco <> 0'
221500030827     C                             +
221600030827     C                             ' AND '
221700030827     C                             +
221800030827     C                             ' attddc BETWEEN '
221900030827     C                             + %editc(f50dti:'X')
222000030827     C                             +
222100030827     C                             ' AND ' + %editc(f50dtf:'X')
222200030827     C                             +
222300030827     C                             ' AND '
222400030827     C                             +
222500030827     C                             'attnff = 0  group by attcdf '
222600030827     c                             +
222700030827     C                             ' ORDER BY attcdf'
222800030827     c                   endsl
222900030128     C                   ENDSR
223000030204?     *--------------------------------------------------------------*
223100030204?     *  srsfl2: CARICAMENNTO SUBFILE 2                              *
223200030204?     *--------------------------------------------------------------*
223300030204     C     srsfl2        BEGSR
223400030204      *
223500030204     c                   exsr      istruz2
223600030204      *  Pulisco dati SFL
223700030204     C                   SETOFF                                       24
223800030204     C                   WRITE     y350C02
223900030204     C                   SETON                                        24
224000030204     C                   Z-ADD     0             NRR2              6 0
224100030204     c                   clear                   tredici           1
224200030204     c                   clear                   tipf
224300030204     c                   clear                   impt
224400030204     c                   clear                   impf
224500030204     c                   clear                   totimpt
224600030206     c                   clear                   totimpf
224700030206     c                   clear                   totimp
224800030206     c                   clear                   diffe
224900030204     c* questo sql mi restituisce per ogni fornitore, mese di competenza,
225000030204     c* tipo servizio la sommatoria dei conteggi
225100030204     c*
225200030204     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
225300030204     c* precedente ed è possibile contabilizzare ancora i ratei,
225400030204     c* altrimenti se il conteggio è dell'anno precedente
225500030204     c* ma non si possono più registrare ratei sarà forzato a 1
225600030204     c* Per i conteggi dell'anno in corso il mese di competenza è
225700030204     c* quello del conteggio
225800030204     c*
225900030827     c* Lettura fiftt00f/fictt00f/fiatt00f
226000030204     C/EXEC SQL
226100030204     C+ PREPARE S2 FROM :WrkSqlCmd
226200030204     C/END-EXEC
226300030204
226400030204     C/EXEC SQL
226500030204     C+ DECLARE A2 CURSOR FOR S2
226600030204     C/END-EXEC
226700030204
226800030204     C/EXEC SQL
226900030204     C+ OPEN A2
227000030204     C/END-EXEC
227100030204
227200030204     C                   DOU       SqlCod <> 0
227300030827     c                   select
227400031121     c* prestazioni occasionali
227500031121     c                   when      f50cau<> ' '
227600030204     C/EXEC SQL
227700031121     C+ FETCH NEXT FROM A2 INTO :fiott2
227800030204     C/END-EXEC
227900031121     c* autotrasportatori
228000031121     c                   when      f50tip = 'A'
228100031121     C/EXEC SQL
228200031121     C+ FETCH NEXT FROM A2 INTO :fiftt2
228300031121     C/END-EXEC
228400030827     c                   when      f50tip = 'C'
228500030204     c* cooperative
228600030204     C/EXEC SQL
228700030204     C+ FETCH NEXT FROM A2 INTO :fictt2
228800030204     C/END-EXEC
228900030827     c                   when      f50tip = 'D'
229000030827     c* aff/defl.
229100030827     C/EXEC SQL
229200030827     C+ FETCH NEXT FROM A2 INTO :fiatt2
229300030827     C/END-EXEC
229400030827     c                   endsl
229500030204     c*
229600030204     C                   SELECT
229700030204     C                   WHEN      SqlCod = 0
229800030204     C                   EXSR      CARTRN2
229900030204     C                   WHEN      SqlCod = 100
230000030204     C                   WHEN      SqlCod <> 0
230100030204      * Forzo la stampa del JOBLOG.
230200030204     C                   CALL      'X66CHGJOB'
230300030204     C                   ENDSL
230400030204     C                   ENDDO
230500030204     C/EXEC SQL
230600030204     C+ CLOSE A2
230700030204     C/END-EXEC
230800030204     c* rileggo il sfl e mi memorizzo la sommatoria per ogni tipo servizio
230900030204     c* tenendo separati gli importi dell'anno in corso con quelli dell'anno
231000030204     c* precedente
231100030204     c                   clear                   x
231200030205     c                   do        10            xyz
231300030204     c     xyz           chain     y350s02                            56
231400030205     c                   if        *in56
231500030205     c                   leave
231600030205     c                   else
231700030204     c                   z-add     1             xy
231800030204     c     h1tipf        lookup    tipf(xy)                               55
231900030204     c                   if        *in55
232000030204     c                   if        h1ccmp = 13
232100030204     c                   add       h1cico        impt(xy)
232200030204     c                   else
232300030204     c                   add       h1cico        impf(xy)
232400030204     c                   end
232500030204     c                   else
232600030204     c                   add       1             x
232700030204     c                   movel     h1tipf        tipf(x)
232800030204     c                   if        h1ccmp = 13
232900030204     c                   z-add     h1cico        impt(x)
233000030204     c                   else
233100030205     c                   z-add     h1cico        impf(x)
233200030204     c                   end
233300030204     c                   end
233400030204     c                   end
233500030204     c                   enddo
233600030204     C*
233700030204     C                   ENDSR
233800030204?     *--------------------------------------------------------------*
233900030204?     *  istruz2 Operazioni di inizializzazione istruzione sql       *
234000030204?     *--------------------------------------------------------------*
234100030204     C     istruz2       BEGSR
234200030827     c                   select
234300031121     c* prestazioni occasionali
234400031121     c                   when      f50cau<> ' '
234500030204     C                   EVAL      WrkSqlCmd
234600030204     C                             =
234700030204     C                             'SELECT '
234800030204     C                             +
234900031121     C                             ' sum(otttim), '
235000030204     C                             +
235100031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
235200030204     C                             +
235300030204     C                             '''' + meseprt + ''''
235400030204     C                             +
235500030204     C                             ' then '
235600030204     C                             +
235700030204     C                             '''' + commes + ''''
235800030204     C                             +
235900030204     C                             ' else'
236000030204     C                             +
236100031121     C                             ' substr(digits(ottddc), 5, 2) end as pippo '
236200030204     C                             +
236300031121     C                             ' from fiott06l where ottsoc = '
236400030204     C                             +
236500030204     C                             '''' + f50soc  + ''''
236600030204     C                             +
236700030204     C                             ' AND '
236800030204     C                             +
236900031121     C                             ' ottcdf = '
237000030204     C                             +
237100030204     C                             '''' + v1cfor + ''''
237200030204     C                             +
237300030204     C                             ' AND '
237400030204     C                             +
237500031121     C                             ' otttip = '
237600031121     C                             +
237700031121     C                             '''' + f50tip + ''''
237800031121     C                             +
237900031121     C                             ' AND '
238000031121     C                             +
238100031121     C                             ' otttab = '
238200031121     C                             +
238300031121     C                             '''' + f50cau + ''''
238400030204     C                             +
238500030204     C                             ' AND '
238600030204     C                             +
238700031121     C                             ' ottddc BETWEEN '
238800030204     C                             + %editc(f50dti:'X')
238900030204     C                             +
239000030204     C                             ' AND ' + %editc(f50dtf:'X')
239100030204     C                             +
239200030204     C                             ' AND '
239300030204     C                             +
239400031121     C                             'ottnff = 0  group by '
239500030204     C                             +
239600031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
239700030204     C                             +
239800030204     C                             '''' + meseprt + ''''
239900030204     C                             +
240000030204     C                             ' then '
240100030204     C                             +
240200030204     C                             '''' + commes + ''''
240300030204     C                             +
240400030204     C                             ' else'
240500030204     C                             +
240600031121     C                             ' substr(digits(ottddc), 5, 2) end '
240700030204     C                             +
240800030204     C                             ' ORDER BY pippo'
240900031121     c* autotrasportatori
241000031121     c                   when      f50tip = 'A'
241100031121     C                   EVAL      WrkSqlCmd
241200031121     C                             =
241300031121     C                             'SELECT '
241400031121     C                             +
241500031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
241600031121     C                             +
241700031121     C                             'fttmnt),'
241800031121     C                             +
241900031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
242000031121     C                             +
242100031121     C                             '''' + meseprt + ''''
242200031121     C                             +
242300031121     C                             ' then '
242400031121     C                             +
242500031121     C                             '''' + commes + ''''
242600031121     C                             +
242700031121     C                             ' else'
242800031121     C                             +
242900031121     C                             ' substr(digits(fttddc), 5, 2) end as pippo '
243000031121     C                             +
243100031121     C                             ' from fiftt06l where fttsoc = '
243200031121     C                             +
243300031121     C                             '''' + f50soc  + ''''
243400031121     C                             +
243500031121     C                             ' AND '
243600031121     C                             +
243700031121     C                             ' fttcdf = '
243800031121     C                             +
243900031121     C                             '''' + v1cfor + ''''
244000031121     C                             +
244100031121     C                             ' AND '
244200031121     C                             +
244300031121     C                             ' ftttsr = '' '''
244400031121     C                             +
244500031121     C                             ' AND '
244600031121     C                             +
244700031121     C                             ' fttfvl = ''C'''
244800031121     C                             +
244900031121     C                             ' AND '
245000031121     C                             +
245100031121     C                             ' Fttddc BETWEEN '
245200031121     C                             + %editc(f50dti:'X')
245300031121     C                             +
245400031121     C                             ' AND ' + %editc(f50dtf:'X')
245500031121     C                             +
245600031121     C                             ' AND '
245700031121     C                             +
245800031121     C                             'fttnff = 0  group by '
245900031121     C                             +
246000031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
246100031121     C                             +
246200031121     C                             '''' + meseprt + ''''
246300031121     C                             +
246400031121     C                             ' then '
246500031121     C                             +
246600031121     C                             '''' + commes + ''''
246700031121     C                             +
246800031121     C                             ' else'
246900031121     C                             +
247000031121     C                             ' substr(digits(fttddc), 5, 2) end '
247100031121     C                             +
247200031121     C                             ' ORDER BY pippo'
247300030204     c* cooperative
247400030827     c                   when      f50tip = 'C'
247500030204     C                   EVAL      WrkSqlCmd
247600030204     C                             =
247700030204     C                             'SELECT ctttsr,'
247800030204     C                             +
247900030204     C                             ' sum(cttitc+cttitma+cttitmp),'
248000030204     C                             +
248100030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
248200030204     C                             +
248300030204     C                             '''' + meseprt + ''''
248400030204     C                             +
248500030204     C                             ' then '
248600030204     C                             +
248700030204     C                             '''' + commes + ''''
248800030204     C                             +
248900030204     C                             ' else'
249000030204     C                             +
249100030204     C                             ' substr(digits(cttddc), 5, 2) end as pippo '
249200030204     C                             +
249300030204     C                             ' from fictt06l where cttsoc = '
249400030204     C                             +
249500030204     C                             '''' + f50soc  + ''''
249600150108      * O sono NON accorpate
249700150108      *  oppure
249800150108      * ACCORPATE e si tratta delle PULIZIE
249900030204     C                   IF        f50tsr = 'N'
250000150108     c                                 or
250100150108     c                             v1tipf = 'P' and Coop_PULIZIE_a_Parte <>' '
250200030204     C                   EVAL      WrkSqlCmd
250300030204     C                             =
250400030204     C                             %TRIMR(WrkSqlCmd)
250500030206     C                             +
250600030206     C                             ' AND '
250700030204     C                             +
250800030204     C                             ' ctttsr = '
250900030204     C                             +
251000030204     C                             '''' + v1tipf + ''''
251100150108      *  Attenzione le accorpate
251200150108      *    sono escluse le PULIZIE
251300150108     C                   ElsE
251400150108      * quindi per le accorpate: TUTTE tranne le "P"=PULIZIE
251500150108     C                   EVAL      WrkSqlCmd
251600150108     C                             =
251700150108     C                             %TRIMR(WrkSqlCmd)
251800150108     C                             +
251900150108     C                             ' AND '
252000150108     C                             +
252100150108     C                             ' ctttsr <> ''P'' '
252200030204     C                   ENDIF
252300030204     C                   EVAL      WrkSqlCmd
252400030204     C                             =
252500030204     C                             %TRIMR(WrkSqlCmd)
252600030204     C                             +
252700030204     C                             ' AND '
252800030204     C                             +
252900030204     C                             ' cttcdf = '
253000030204     C                             +
253100030204     C                             '''' + v1cfor + ''''
253200030204     C                             +
253300030204     C                             ' AND '
253400030204     C                             +
253500030204     C                             ' cttfvl = ''C'''
253600030204     C                             +
253700030204     C                             ' AND '
253800030204     C                             +
253900030204     C                             ' cttddc BETWEEN '
254000030204     C                             + %editc(f50dti:'X')
254100030204     C                             +
254200030204     C                             ' AND ' + %editc(f50dtf:'X')
254300030204     C                             +
254400030204     C                             ' AND'
254500030204     C                             +
254600030204     C                             ' cttnff = 0  group by ctttsr, '
254700030204     C                             +
254800030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
254900030204     C                             +
255000030204     C                             '''' + meseprt + ''''
255100030204     C                             +
255200030204     C                             ' then '
255300030204     C                             +
255400030204     C                             '''' + commes + ''''
255500030204     C                             +
255600030204     C                             ' else'
255700030204     C                             +
255800030204     C                             ' substr(digits(cttddc), 5, 2) end '
255900030204     C                             +
256000030204     C                             ' ORDER BY ctttsr, pippo'
256100030827     c* aff/defl
256200030827     c                   when      f50tip = 'D'
256300030827     C                   EVAL      WrkSqlCmd
256400030827     C                             =
256500031007     C*                            'SELECT attvad,'
256600031007     C                             'SELECT '
256700030827     C                             +
256800030827     C                             ' sum(attimpp),'
256900030827     C                             +
257000030827     C                             ' case when substr(digits(attddc), 5, 2) > '
257100030827     C                             +
257200030827     C                             '''' + meseprt + ''''
257300030827     C                             +
257400030827     C                             ' then '
257500030827     C                             +
257600030827     C                             '''' + commes + ''''
257700030827     C                             +
257800030827     C                             ' else'
257900030827     C                             +
258000030827     C                             ' substr(digits(attddc), 5, 2) end as pippo '
258100030827     C                             +
258200030827     C                             ' from fiatt06l where attsoc = '
258300030827     C                             +
258400030827     C                             '''' + f50soc  + ''''
258500030827     C                             +
258600030827     C                             ' AND '
258700030827     C                             +
258800030827     C                             ' attcdf = '
258900030827     C                             +
259000030827     C                             '''' + v1cfor + ''''
259100030827     C                             +
259200030827     C                             ' AND '
259300030827     C                             +
259400030827     C                             ' attdco <> 0'
259500030827     C                             +
259600030827     C                             ' AND '
259700030827     C                             +
259800030827     C                             ' attddc BETWEEN '
259900030827     C                             + %editc(f50dti:'X')
260000030827     C                             +
260100030827     C                             ' AND ' + %editc(f50dtf:'X')
260200030827     C                             +
260300030827     C                             ' AND'
260400030827     C                             +
260500031007     C*                            ' attnff = 0  group by attvad, '
260600031007     C                             ' attnff = 0  group by '
260700030827     C                             +
260800030827     C                             ' case when substr(digits(attddc), 5, 2) > '
260900030827     C                             +
261000030827     C                             '''' + meseprt + ''''
261100030827     C                             +
261200030827     C                             ' then '
261300030827     C                             +
261400030827     C                             '''' + commes + ''''
261500030827     C                             +
261600030827     C                             ' else'
261700030827     C                             +
261800030827     C                             ' substr(digits(attddc), 5, 2) end '
261900030827     C                             +
262000031007     C*                            ' ORDER BY attvad, pippo'
262100031007     C                             ' ORDER BY pippo'
262200030827     c                   endsl
262300030204     C                   ENDSR
262400030204?     *--------------------------------------------------------------*
262500030204?     *  CARTRN2 Gestione caricamento dati sfl2                      *
262600030204?     *--------------------------------------------------------------*
262700030204     C     CARTRN2       BEGSR
262800030827     c                   select
262900031121     c* prestazioni occasionali
263000031121     c                   when      f50cau <>' '
263100030204     C                   MOVE      v1tipf        h1tipf
263200031121     C                   eval(h)   h1cico = Totso2
263300031121     C                   if        meseso2 > meseprt
263400031121     C                   move      '13'          meseso2
263500030204     C                   END
263600031121     C                   move      meseso2       h1ccmp
263700031121     c* autotrasportatore
263800031121     c                   when      f50tip = 'A'
263900031121     C                   MOVE      v1tipf        h1tipf
264000031121     C                   eval(h)   h1cico = Tots2
264100031121     C                   if        meses2 > meseprt
264200031121     C                   move      '13'          meses2
264300031121     C                   END
264400031121     C                   move      meses2        h1ccmp
264500030204     c* cooperativa
264600030827     c                   when      f50tip = 'C'
264700030205     C                   eval(h)   h1cico = Totsc2
264800030204     C                   MOVEL     tsrsc2        h1tipf
264900030204     C                   if        mesesc2 > meseprt
265000030204     C                   move      '13'          mesesc2
265100030204     C                   END
265200030204     C                   move      mesesc2       h1ccmp
265300030827     c* aff/defl
265400030827     c                   when      f50tip = 'D'
265500030827     C                   eval(h)   h1cico = Totsa2
265600030827     c* ora il conto per aff/defl è lo stesso quindi non importa spaccare
265700030827     c* le contropartite, se un domani si vogliono tenere separati invece
265800031007     c* di chiodare D x le affluenze si metterà il codice di tabella nuovo
265900031007     c*                  if        tsrsa2 = 'A'
266000030827     C                   MOVEL(p)  'D'           h1tipf
266100031007     c*                  else
266200031007     C*                  MOVEL     tsrsa2        h1tipf
266300031007     c*                  end
266400030827     C                   if        mesesa2 > meseprt
266500030827     C                   move      '13'          mesesa2
266600030827     C                   END
266700030827     C                   move      mesesa2       h1ccmp
266800030827     c                   endsl
266900030204     c*
267000030204     c                   if        h1ccmp = 13
267100030204     c                   eval      tredici = *on
267200030204     c                   end
267300030204     c*
267400030204     C                   ADD       1             NRR2
267500030204     C                   WRITE     y350S02
267600030204      *
267700030204     C                   ENDSR
267800030416?     *--------------------------------------------------------------*
267900030416?     *  *INZSR: Operazioni di inizializzazione dati                 *
268000030416?     *--------------------------------------------------------------*
268100030416     C     *INZSR        BEGSR
268200020527      *
268300951011     C     *ENTRY        PLIST
268400951011     C                   PARM                    KPJBA
268500020408     C                   MOVEL     KPJBU         ficn50ds
268600980911     C*---------- RICERCA DITTA :
268700980911     C                   MOVEL     'SOC001'      TIPXSC
268800020412     C                   MOVEL     f50soc        SOCXSC
268900980911     C                   EXSR      REPSOC
269000980911     C     RTNXSC        IFNE      '1'
269100980911     C                   MOVEL     XSOCDS        SOC001
269200980911     C                   MOVEL     xscrgs        RSUT             20
269300980911     c                   end
269400020417      *  Definizione chiave di accesso
269500020423     C     Kuni          KLIST
269600020417     C                   KFLD                    xscsoc
269700020423     C                   KFLD                    f50uni
269800020708     C     Kope          KLIST
269900031204     C                   KFLD                    xscsoc
270000020708     C                   KFLD                    bhmcaustes
270100020708     C     Kdft          KLIST
270200020708     C                   KFLD                    bhmsocieta
270300020708     C                   KFLD                    operepdft
270400031121     C     Kott          KLIST
270500031121     C                   KFLD                    xscsoc
270600031121     C                   KFLD                    v1cfor
270700031121     C                   KFLD                    f50tip
270800031121     C                   KFLD                    f50cau
270900160108     C     Ksre          KLIST
271000160108     C                   KFLD                    xscsoc
271100160108     C                   KFLD                    v1cfor
271200160108     C                   KFLD                    f50tip
271300160108     C     Ksre1         KLIST
271400160108     C                   KFLD                    xscsoc
271500160108     C                   KFLD                    v1cfor
271600160108     C                   KFLD                    f50tip
271700160108     C                   KFLD                    v1ctsr
271800020423     C     Kftt          KLIST
271900020423     C                   KFLD                    xscsoc
272000020423     C                   KFLD                    v1cfor
272100020417     C                   KFLD                    ftttsr
272200020417     C                   KFLD                    fttfvl
272300020417     c                   move      ' '           ftttsr
272400020417     c                   move      'C'           fttfvl
272500030128     C     Kctt          KLIST
272600030128     C                   KFLD                    xscsoc
272700030128     C                   KFLD                    v1cfor
272800030128     C                   KFLD                    cttfvl
272900030128     C     Kctt1         KLIST
273000030128     C                   KFLD                    xscsoc
273100030128     C                   KFLD                    v1cfor
273200030128     C                   KFLD                    cttfvl
273300030128     C                   KFLD                    v1ctsr
273400030827     C     Katt          KLIST
273500030827     C                   KFLD                    xscsoc
273600030827     C                   KFLD                    v1cfor
273700030828     C                   KFLD                    attflg
273800031121     C     Ktbe1         KLIST
273900031121     C                   KFLD                    tbecod
274000031121     C                   KFLD                    tbeke1
274100020724     C     Ktbe          KLIST
274200020724     C                   KFLD                    tbecod
274300020724     C                   KFLD                    tbeke1
274400020724     C                   KFLD                    tbeke2
274500030213     C     Ktab          KLIST
274600030213     C                   KFLD                    tblkut
274700030213     C                   KFLD                    tblcod
274800030213     C                   KFLD                    tblkey
274900030213     c                   eval      tblkut = 1
275000030213     c                   eval      tblcod = '1Z'
275100020417     C     Kfrn          KLIST
275200020417     C                   KFLD                    f50soc
275300030129     C                   KFLD                    v1cfor
275400020417     C     Krco          KLIST
275500020417     C                   KFLD                    f50soc
275600020417     C                   KFLD                    forita
275700030129     C                   KFLD                    v1cfor
275800020514     C     Ktlz          KLIST
275900020514     C                   KFLD                    tlztip
276000020514     C                   KFLD                    tlzpdr
276100020514     C                   KFLD                    f50soc
276200080924     C     Kanf2         KLIST
276300080924     C                   KFLD                    anftip
276400080924     C                   KFLD                    anftsr
276500080924     C                   KFLD                    anftrc
276600080924     C                   KFLD                    anfsoc
276700080924     C                   KFLD                    anfcdf
276800080924     C                   KFLD                    anfdft
276900080924     C                   KFLD                    anfnff
277000080924     C                   KFLD                    anfpdr
277100020408     C* Reperisco i libri IVA ammessi per la filiale.
277200020724     C                   eval      tbecod = 'YIV'
277300020724     C                   eval      tbeke1 = xscsoc
277400020724     C                   eval      tbeke2 = %subst(f50UNI: 1: 3)
277500020724     C     ktbe          chain     tntbe01l
277600020408     C                   if        %found
277700020724     C                   eval      dyiv = tbeuni
277800020408     C                   else
277900020724     C                   clear                   dyiv
278000020408     C                   endif
278100031121     c* aggancio la tabella CAF
278200031121     c                   if        f50cau <> ' '
278300031121     c                   movel(p)  f50cau        tbeke1
278400031121     c                   movel(p)  'CAF'         tbecod
278500031121     c     ktbe1         chain     tntbe01l
278600031121     c                   if        %found(tntbe01l) and tbeatb = ' '
278700031121     c                   movel     tbeuni        dcaf
278800031121     c                   else
278900031121     c                   clear                   dcaf
279000031121     c                   end
279100031121     c                   end
279200020708     C* controllo la data di PROTOCOLLO
279300020708     c                   clear                   xnmkey
279400020708     c                   clear                   xnmser
279500020708     c                   move      *off          xnmcmt
279600020708     c                   clear                   xnmerr
279700020708     c                   clear                   xnmnum
279800020708     c                   clear                   xnmaut
279900020708     C                   MOVE      F50DPR        XnmDat
280000020708     C                   MOVE      F50DPR        XnmDag                         opzionale
280100020708     C                   MOVE      §IVLAQ        COM3              3            opzionale
280200020708     C                   EVAL      XNMSER = '1' + COM3
280300020708     C                   movel(p)  f50uni        Xnmuni
280400020708     C                   movel     '1'           XnmRic
280500020708     C                   movel     'CG'          XnmCtb
280600020708     C                   movel     *on           XnmIva
280700020708     C                   movel     *ON           XnmReg
280800020708     C                   EXSR      CALXNMR
280900020708     c                   if        xnmerr = '7'
281000020708     c                   seton                                        lr
281100020708     c                   return
281200020708     c                   end
281300020411     c* se non ho il documento fornitore non visualizzo la riga in testata
281400020411     c     f50nft        comp      0                                      06
281500020409      * Imposto la data di competenza analitica a fine anno
281600980916      * per verificare se posso impostare la competenza x
281700980916      * l'anno precedente
281800020409     C                   move      XSCSOC        X10societa
281900020409     C                   move      'CG'          X10ctb
282000020415     C                   move      udate         x10data
282100020409     C                   call      'X10ESER'
282200020409     C                   parm                    x10ese
282300020409     c                   if        x10dtines <> *loval
282400020409     C     X10DTINES     SUBDUR    1:*D          WDTFINE
282500020409     c                   endif
282600980916     C                   move      wdtfine       DTCOAN
282700020408     C                   move      f50dpr        Dataiso
282800020408     C                   move      dataiso       DTrif
282900980916      * Controllo se si può inserire ancora data competenza 13
283000980916     C                   Callb     'NDMVC113'
283100020412    >C                   PARM                    xscsoc
283200020411     c                   parm                    unita
283300980916    >C                   PARM      'CG'          Ctb
283400980916    >C                   PARM      *Blank        Keygest
283500020502    >C                   PARM      'R'           Tipdat
283600980916    >C                   PARM      'D'           Tpregz
283700980916    >C                   PARM                    DtCoAn
283800980916     C                   PARM                    DtRif
283900980916    >C                   PARM      'I'           Operazione
284000980916     C                   PARM      '1'           GesMsg
284100980916    >C                   PARM                    Reper
284200980916     C                   PARM                    Esito
284300020408     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
284400020408     C                   movel     'F     '      $kcc              6
284500020408     C                   movel     *blanks       $ksc              8
284600020408     C                   callb     'XCAPCLIFOR'
284700020412     C                   parm                    xscsoc
284800020408     C                   parm                    $kcc
284900020408     C                   parm                    $ksc
285000020408     C                   movel     $kcc          forita            6
285100020409     c* NSYS
285200020415     c                   clear                   xnmuni
285300020415     c                   clear                   xnmctb
285400020415     c                   clear                   xnmkey
285500020415     c                   clear                   xnmerr
285600020415     c                   clear                   xnmnum
285700020415     c                   clear                   xnmaut
285800020409     C                   MOVE      *DATE         XnmDat
285900020409     C                   MOVE      *DATE         XnmDag                         opzionale
286000020409     C                   move      *OFF          XnmCmt
286100020409     C                   move      *OFF          XnmIva
286200020409     C                   move      *OFF          XnmReg
286300020409     C                   EVAL      XnmRic = '1'
286400020409     C                   EVAL      XnmSer = 'NSYS'
286500020409     C                   EXSR      CalXnmr
286600030128      * Se si può inserire ancora data competenza 13
286700030128     c                   if        esito = *off
286800030128     C                   EVAL      commes = '13'
286900030128     C* Se non posso immettere registrazioni aventi data competenza
287000030128     C* dell'anno precedente imposto 01 il mese di competenza
287100030128     C* ed eseguo la normale procedura di contabilizz.
287200030128     c                   else
287300030128     C                   EVAL      commes = '01'
287400030128     c                   end
287500020412     c* estraggo il mese della data protocollo
287600020417     c                   move      f50dpr        dataiso
287700020417     c                   extrct    dataiso:*m    mm                2 0
287800020418     c                   move      mm            meseprt           2
287900150108      *
288000150108      *  contatore serve solo per sbinare x le COOP l'accorpamento e le pulizie
288100150108      *   che devono essere tenute a parte
288200150108     c                   eval        Coop_PULIZIE_a_Parte = *blank
288300150108     c                   if        f50tip = 'C' and f50tsr ='S'
288400150108     c                   eval        Coop_PULIZIE_a_Parte = 'S'
288500150108     c                   end
288600150108      *
288700031121     c* MI COMPONGO L'ISTRUZIONE SQL
288800150108      *                 *----------------*
288900030128     c                   exsr      istruz
289000150108      *                 *----------------*
289100031121     c                   if        f50cau <> ' '
289200031121     c                   seton                                        60
289300031121     c                   end
289400030827     c                   select
289500030128     c* decodifica a video
289600031121     c                   when      f50tip = 'A'
289700030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
289800040914     c                   clear                   dblc
289900040914     c                   movel(p)  '1'           tbeke1
290000040914     c                   movel     'BLC'         tbecod
290100040914     c     ktbe1         chain     tntbe01l
290200040914     c                   if        %found(tntbe01l)
290300040914     c                   movel     tbeuni        dblc
290400040914     c                   end
290500040914     c                   z-add     §blcmin       impmin
290600030827     c                   when      f50tip = 'C'
290700030128     c                   seton                                        60
290800030128     c                   eval      dectip = '   COOPERATIVE   '
290900030704     C                   IF        NOT %OPEN(FICTT06L)
291000030620     c                   open      fiCTT06l
291100030704     C                   END
291200040914     c                   clear                   dblo
291300040914     c                   movel(p)  '1'           tbeke1
291400040914     c                   movel     'BLO'         tbecod
291500040914     c     ktbe1         chain     tntbe01l
291600040914     c                   if        %found(tntbe01l)
291700040914     c                   movel     tbeuni        dblo
291800040914     c                   end
291900040914     c                   z-add     §blomin       impmin
292000030827     c                   when      f50tip = 'D'
292100030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
292200030827     C                   IF        NOT %OPEN(FIATT06L)
292300030827     c                   open      fiaTT06l
292400030827     C                   END
292500040914     c                   clear                   dbld
292600040914     c                   movel(p)  '1'           tbeke1
292700040914     c                   movel     'BLD'         tbecod
292800040914     c     ktbe1         chain     tntbe01l
292900040914     c                   if        %found(tntbe01l)
293000040914     c                   movel     tbeuni        dbld
293100040914     c                   end
293200040914     c                   z-add     §bldmin       impmin
293300030827     c                   endsl
293400020408      *  Inizializzo variabili
293500020408     C                   MOVEL     '1'           WTPVID            1
293600020408     C                   MOVEL     'N'           WFINE             1
293700020411     c                   z-add     0             subnum
293800951012     C                   ENDSR
293900951012     O*-----------------------------------------------------*
294000031121     Ofiott000  E            UPDott
294100031121     O                       ottdft
294200031121     O                       ottnff
294300031121     O                       ottnra
294400031121     O                       ottdcn
294500031121     O                       ottdtr
294600031121     O                       ottnrg
294700031121     Ofiftt000  E            UPDftt
294800031121     O                       fttdft
294900031121     O                       fttnff
295000031121     O                       fttflr
295100031121     O                       fttdcn
295200030128     Ofictt000  E            UPDctt
295300030128     O                       cttdft
295400030128     O                       cttnff
295500030128     O                       cttnra
295600030128     O                       cttdcn
295700030128     O                       cttdtr
295800030128     O                       cttnrg
295900030827     Ofiatt000  E            UPDatt
296000030827     O                       attdft
296100030827     O                       attnff
296200030827     O                       attnra
296300030827     O                       attdcn
296400030827     O                       attdtr
296500030827     O                       attnrg
296600160108     Otnsre000  E            UPDsre
296700160108     O                       sredft
296800160108     O                       srenff
296900160108     O                       srenra
297000160108     O                       sredcn
297100160108     O                       sredtr
297200160108     O                       srenrg
297300951012      *--------------------------------------------------------------*
297400951012** ERR
297500020415Selezionare almeno un fornitore                                       1
297600021015Esistono fornitori senza partita IVA                                  2
297700030620ATTENZIONE!! Esistono valorizzazioni non confermate                   3
297800080924Manca la massa complessiva su uno o + autotrasportatori               4
297900120816Manca tipologia fornitore in una o più anagrafiche                    5
298000160115Importo fattura errato, avvisare l'uff.Fornitori Operativi di SEDE    6
