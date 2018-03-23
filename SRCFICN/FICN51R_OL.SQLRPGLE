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
002400170123     Ffiftt06L  iF   E           K DISK    USROPN infds(fttinf)
002500170123     Ffictt06L  iF   E           K DISK    USROPN infds(cttinf)
002600170123     Ffiatt06L  iF   E           K DISK    USROPN infds(attinf)
002700170123     Ffiott06L  iF   E           K DISK    USROPN infds(ottinf)
002800170123     Ftnsre01L  iF   E           K DISK           infds(sreinf)
002900170123     Ffiftt00f  UF   E             DISK    USROPN rename(fiftt000:fiftt00)
003000170123     Ffictt00f  UF   E             DISK    USROPN rename(fictt000:fictt00)
003100170123     Ffiatt00f  UF   E             DISK    USROPN rename(fiatt000:fiatt00)
003200170123     Ffiott00f  UF   E             DISK    USROPN rename(fiott000:fiott00)
003300170123     Ftnsre00f  UF   E             DISK           rename(tnsre000:tnsre00)
003400080924     FTNanf01L  UF a E           K DISK    commit
003500080924     Ftntbe01l  if   E           K DISK
003600030213     Ftabel00f  if   E           K DISK
003700951009     F*--------
003800020708     Fndbhp00f  o    E             DISK    commit
003900020708     Fndbhm00f  o    E             DISK    commit
004000020411     fndbhi00f  o    e             disk    commit
004100020411     fndbha00f  o    e             disk    commit
004200951011     F*--------
004300020423     Fanuni01l  IF   E           K DISK
004400020423     Fansog01l  IF   E           K DISK
004500020405     Fanfrn01l  IF   E           K DISK
004600020405     Fanrco01l  IF   E           K DISK
004700020514     Ftntlz01l  IF   E           K DISK
004800020708     Fanope01l  IF   E           K DISK
004900020708     Fandft01l  IF   E           K DISK
005000020405     F*--------
005100020404     Fficn51D   CF   E             WORKSTN
005200980911     F                                     SFILE(y350S01:NRR)
005300030204     F                                     SFILE(y350S02:NRR2)
005400951009     D*---------------------------------------------------------------*
005500951009     D* SCHIERA
005600951009     D*---------------------------------------------------------------*
005700951011      * Schiere per errori e stampa
005800160108     D ERR             S             70    DIM(6) CTDATA PERRCD(1)              Errori
005900030204     D tipf            S              4    DIM(10) inz                          Errori
006000030204     D impf            S             13  2 DIM(10) inz                          Errori
006100030204     D impt            S             13  2 DIM(10) inz                          Errori
006200020411     D* Reperimento nome PGM
006300020411     D STATUS         SDS           333
006400020411     D  nompgm           *PROC
006500020411     D*---------------------------------------------------------------*
006600020411     D* CAMPI INTERNI
006700020411     D*---------------------------------------------------------------*
006800020411     D dataiso         s               d   datfmt(*iso)
006900020411     D dataeur         s               d   datfmt(*eur)
007000020409     D WDTFINE         S               D
007100080924     D sqlcodsav       S                   like(sqlcod)
007200080924     D riga1           S                   like(BhmNrAsReg)
007300020411     D riga2           S                   like(BhmNrAsReg)
007400020418     D ksys            S              3  0 inz
007500020503     d subnum          S                   like(bhmsubnum)
007600020415     D savsubnum       S                   like(bhmsubnum)
007700020415     D savsubrig       S                   like(bhmsubrig)
007800020503     D savdtdoc        S                   like(bhmdtdoc)
007900020503     D savnrdoc        S                   like(bhmnrdoc)
008000021127     D savdcn          S                   like(fttdcn)
008100040914     D impmin          S                   like(§blcmin)
008200000204     D* ------ Definizioni parametri standard
008300020411     D Esito           S              1
008400020411    >D Unita           S              8A
008500020411    >D Ctb             S              2A
008600020411    >D Keygest         S              8A
008700020411    >D Tipdat          S              1A
008800020411    >D Tpregz          S              1A
008900020411    >D DtCoan          S             10A
009000020411    >D DtRif           S             10A
009100020411    >D Operazione      S              2A
009200020411     D GesMsg          S              1
009300020411    >D Reper           S              1
009400020411     D XnmRic          S              1
009500020411     D XnmSoc          S              3
009600020411     D XnmUni          S              8
009700020411     D XnmCtb          S              2
009800020411     D XnmKey          S              8
009900020411     D XnmSer          S              4
010000020411     D XnmCmt          S              1
010100020411     D XnmDat          S               D
010200020411     D XnmIva          S              1
010300020411     D XnmReg          S              1
010400020415     D XnmDag          S               D
010500020411     D XnmErr          S              1
010600020411     D XnmNum          S              9  0
010700020411     D XnmAut          S              1
010800020418     D imponibile      S             18  2
010900030205     D impcom          S             18  2
011000030204     D totimpt         S             18  2
011100030206     D totimpf         S             18  2
011200030206     D totimp          S             18  2
011300030206     D diffe           S             13  2
011400080925     D comsfl2         S             13  2
011500030206     D impiva          S             13  2
011600020527     d v1tips          s                   like(v1tipf)
011700030206     d v1tip           s                   like(v1tipf)
011800031204     d segnofor        s                   like(bhmsegno)
011900030128     d commes          s              2
012000030416     d com03           s              3
012100030204     d x               s              3  0
012200030204     d xy              s              3  0
012300030204     d xyz             s              3  0
012400030620     d noconf          s              1    inz
012500150108      *-----
012600150108      * servirà x gestire x le COOP l'accorpamento excludendo le PULIZIE
012700150108      * che x legge (dal 2015) devono essere tenute a parte.
012800150108      * Serve x fare 2 volte il giro sull'accorpamento.
012900150108      * 1) primo con solo le PULIZIE
013000150108      * 2) secondo con le altre senza le PULIZIE
013100150108      *-----
013200150108     d Coop_PULIZIE_a_parte...
013300150108     d                 s              1    inz
013400951009?     *--------------------------------------------------------------*
013500951009?     *  DS                                                          *
013600951009?     *--------------------------------------------------------------*
013700170124     d trul82ds      e ds
013800080923     D ficn74ds      e DS                  inz
013900080924     D danf2         e DS
014000080924     D dapdflr       e DS
014100080924     D dftt01        e DS
014200020606     D xatbds        e DS
014300980911     D soc001        E DS                  EXTNAME(xsoc001ds)
014400980911     D xsocds          DS          1000
014500030213     D ds1z          e ds                  inz
014600031121     D dcaf          e ds                  inz
014700040914     D dBLC          e ds                  inz
014800040914     D dBLD          e ds                  inz
014900040914     D dBLO          e ds                  inz
015000020411     D* libro iva acquisti
015100020724     D dyiv          e ds                  inz
015200020409     D* Parametri per reperimento data inizio esercizio
015300020411    >D x10ese        E DS                  EXTNAME(x10eseDS)
015400951009     D KPJBA         E DS
015500020411?     *  Parametri registrazioni contabili
015600980914     D ndc071ds      E DS
015700020411     D NDC050DS      E DS                  INZ
015800020411     D  RET050       E                     INZ('0')
015900020411     D  OPR050       E                     INZ('0')
016000020411     D  ERR050       E                     INZ('0')
016100020514     D dtlz01        E DS
016200020411     d* parametri ricevuti
016300020404     D ficn50ds      E DS
016400020415     D salds         E DS                  extname(ficn50ds) prefix(£)
016500020411?     *  Tabella Y4Z
016600020408     D angy4z        E DS                  extname(angy4zds)
016700020411?     *  Tabella IVA
016800020411     DTABG48         E DS                  EXTNAME(ANGG48DS)
016900020411     D X06DS         E DS                  EXTNAME(X06G48DS)
017000020411     D  X06SOC       E                     EXTFLD(X06SOCIETA)
017100020411     D  X06CIV       E                     EXTFLD(X06CDIVA)
017200020411     D  X06DAT       E                     EXTFLD(X06DATA)
017300020411     D  X06LIN       E                     EXTFLD(X06LINGUA)
017400020415     d* cancella NDBH*
017500020415     D ycodel1ds     e DS
017600020411     c* SQL
017700020405     D WrkSqlCmd       S           1024
017800080924     D WrkSqladg       S           1024
017900030827     c* sfl 1 aut.
018000020405     D fiftt           DS
018100020405     d cdfs                                like(fttcdf)
018200020405     d tots                                like(ftttim)
018300030827     c* sfl 1 coop accorpate
018400030128     D fictt           DS
018500030128     d cdfsc                               like(cttcdf)
018600030128     d totsc                               like(cttitc)
018700030827     c* sfl 1 coop non accorpate
018800030128     D fictttsr        DS
018900030128     d cdfsc1                              like(cttcdf)
019000030128     d totsc1                              like(cttitc)
019100030128     d tsrsc1                              like(ctttsr)
019200030827     c* sfl 1 aff/defl
019300030827     D fiatt           DS
019400030827     d cdfsa                               like(attcdf)
019500030827     d totsa                               like(attimpp)
019600031121     c* sfl 1 prestaz. occasionali
019700031121     D fiott           DS
019800031121     d cdfso                               like(ottcdf)
019900031121     d totso                               like(otttim)
020000030827     c* sfl 2 aut.
020100030204     D fiftt2          DS
020200030204     d tots2                               like(ftttim)
020300030204     d meses2                         2
020400080924     c* adeguamento carburante
020500080924     D fttatt3         DS
020600080924     d pdr3                                like(fttpdr)
020700080924     d tot3                                like(ftttim)
020800030827     c* sfl 2 coop
020900030204     D fictt2          DS
021000030204     d tsrsc2                              like(ctttsr)
021100030204     d totsc2                              like(cttitc)
021200030204     d mesesc2                        2
021300030827     c* sfl 2 aff/defl
021400030827     D fiatt2          DS
021500031007     d*tsrsa2                              like(attvad)
021600030827     d totsa2                              like(attimpp)
021700030827     d mesesa2                        2
021800031121     c* sfl 2 prestaz. occasionali
021900031121     D fiott2          DS
022000031121     d totso2                              like(otttim)
022100031121     d meseso2                        2
022200990507     D*  Prototipazione delle procedure
022300990507     D/COPY *LIBL/SRCCPY,PJX002PR
022400951009?     *--------------------------------------------------------------*
022500170123     d fttinf          ds
022600170123     d  fttnrr               397    400i 0
022700170123     d cttinf          ds
022800170123     d  cttnrr               397    400i 0
022900170123     d attinf          ds
023000170123     d  attnrr               397    400i 0
023100170123     d ottinf          ds
023200170123     d  ottnrr               397    400i 0
023300170123     d sreinf          ds
023400170123     d  srenrr               397    400i 0
023500951009?     *  CICLO PRINCIPALE                                            *
023600951009?     *--------------------------------------------------------------*
023700951009      *  Impostazione parametri prima videata
023800951010     C                   EXSR      CARSFL
023900951009      *  Loop gestione videata
024000951009     C     WFINE         DOUEQ     'S'
024100000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
024200000426     C     WTPVID        CASEQ     '2'           GESD02                         sfl vuoto
024300951009     C                   END
024400951012     C                   END
024500980312     C*
024600020419     C                   MOVEL(p)  ficn50ds      KPJBU
024700951009     C                   SETON                                        LR
024800951009?     *--------------------------------------------------------------*
024900951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
025000951009?     *--------------------------------------------------------------*
025100951012     C     CARSFL        BEGSR
025200951009      *
025300951009      *  Pulisco dati SFL
025400020405     C                   SETOFF                                       2120
025500980911     C                   WRITE     y350C01
025600951009     C                   SETON                                        2021
025700150108      *
025800951009     C                   MOVEL     '1'           WTPVID
025900020415     C                   Z-ADD     0             NRR               6 0
026000150108      *
026100150108      *  Accorpamento x COOP dopo aver scorporato le PULIZIE
026200150108      *    deve eseguire un secondo giro con l'SQL modificato
026300150108      *   *-----------------*
026400150108     c     secondo_giro  tag
026500150108      *   *-----------------*
026600150108      *
026700020409     c* questo sql mi restituisce per ogni fornitore e mese di competenza
026800020412     c* la sommatoria dei conteggi.
026900020412     c*
027000020412     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
027100020412     c* precedente ed è possibile contabilizzare ancora i ratei,
027200020412     c* altrimenti se il conteggio è dell'anno precedente
027300020412     c* ma non si possono più registrare ratei sarà forzato a 1
027400020412     c* Per i conteggi dell'anno in corso il mese di competenza è
027500020412     c* quello del conteggio
027600020412     c*
027700031121     c* Lettura fiftt00f/fictt00f/fiatt00f/fiott00f
027800020405     C/EXEC SQL
027900020405     C+ PREPARE S1 FROM :WrkSqlCmd
028000020405     C/END-EXEC
028100020405
028200020405     C/EXEC SQL
028300020405     C+ DECLARE A1 CURSOR FOR S1
028400020405     C/END-EXEC
028500020405
028600020405     C/EXEC SQL
028700020405     C+ OPEN A1
028800020405     C/END-EXEC
028900020405     C                   DOU       SqlCod <> 0
029000150108
029100030827     C                   SELECT
029200031121     c* prestaz. occasionali
029300031121     c                   when      f50cau <> ' '
029400031121     C/EXEC SQL
029500031121     C+ FETCH NEXT FROM A1 INTO :fiott
029600031121     C/END-EXEC
029700031121     c* autotrasportatori
029800031121     c                   when      f50tip = 'A'
029900020405     C/EXEC SQL
030000020405     C+ FETCH NEXT FROM A1 INTO :fiftt
030100020405     C/END-EXEC
030200150108     c* cooperative
030300030827     c                   when      f50tip = 'C'
030400150108     c* non Accorpate
030500150108     c*  oppure le Pulizie estrapolate dall'accorpamento
030600150108     c                   if        f50tsr = 'N'
030700150108     c                              or
030800150108     c                             Coop_PULIZIE_a_parte ='S'
030900150108     c*      con tipo servizio
031000030128     C/EXEC SQL
031100030128     C+ FETCH NEXT FROM A1 INTO :fictttsr
031200030128     C/END-EXEC
031300150108     c                   else
031400150108     c*     Accorpate senza le PULIZIE
031500150108     C/EXEC SQL
031600150108     C+ FETCH NEXT FROM A1 INTO :fictt
031700150108     C/END-EXEC
031800030128     c* cooperative      con tipo servizio
031900030128     c                   end
032000030827     c* aff/defl.
032100030827     c                   when      f50tip = 'D'
032200030827     C/EXEC SQL
032300030827     C+ FETCH NEXT FROM A1 INTO :fiatt
032400030827     C/END-EXEC
032500030827     c                   endsl
032600150108      *
032700020405     C                   SELECT
032800150108      *
032900020405     C                   WHEN      SqlCod = 0
033000150108     c*                *------------------*
033100951010     C                   EXSR      CARTRN
033200150108     c*                *------------------*
033300020405     C                   WHEN      SqlCod = 100
033400020405     C                   WHEN      SqlCod <> 0
033500020417      * Forzo la stampa del JOBLOG.
033600020417     C                   CALL      'X66CHGJOB'
033700020405     C                   ENDSL
033800020404     C                   ENDDO
033900020417     C/EXEC SQL
034000020417     C+ CLOSE A1
034100020417     C/END-EXEC
034200150108      *
034300150108      *  solo x le COOP:
034400150108      * DOVENDO accorpare ma dovendo tenere OBBLIGATORIAMENTE separata la
034500150108      *  scrittura contabile delle PULIZIE (questo dal 2015)
034600150108      *  si è gestito il campo --> Coop_PULIZIE_a_parte i cui valori sono:
034700150108      * (*BLK) non si deve accorpare o il pgm non è chiamato per le COOP
034800150108      * (S) x le COOP in accorpamento e estrapolare le PULIZIE dai servizi
034900150108      * (N) x le COOP in accorpamento e totalizzando i servizi senza le PULIZIE
035000150108      *   "S" ha fatto il primo giro dove ha preso solo le PULIZIE (come in dettaglio)
035100150108      *       viene messa a "N"
035200150108      *  per eseguire un 2°giro estraendo gli altri servizi accorpati NO PULIZIE.
035300150108      *   dopo il 2° giro va oltre
035400150108     c                   if        Coop_PULIZIE_a_parte ='S'
035500150108     c                   eval          Coop_PULIZIE_a_parte ='N'
035600150108      *  reimposta l'SQL modificato.
035700150108      *   e carica nuove righe sul SFL.
035800150108     c                   exsr      ISTRUZ
035900150108      *                         *-----------------*
036000150108     c                   goto      secondo_giro
036100150108      *                         *-----------------*
036200150108     c                   end
036300150108      *
036400020405      *  Imposto dati SFLCTL
036500020405     C                   EXSR      INZ01
036600150108      *
036700951009      *  Se non ho dati da caricare emetto videata vuota
036800951009     C     NRR           IFEQ      0
036900020405     C                   MOVEL     '2'           WTPVID
037000951009     C                   END
037100951009     C*
037200951009     C                   ENDSR
037300951013?     *--------------------------------------------------------------*
037400020415?     *  INZ01: Inizializzazione videata                             *
037500951013?     *--------------------------------------------------------------*
037600951013     C     INZ01         BEGSR
037700951013      *
037800000426      *  Inizializzo testata sfl in base a quanto
037900000426      *  passato dal programma chiamante
038000951013     C                   MOVEL     '1'           WTPVID
038100951016     C                   MOVEL     NOMPGM        V1CPGM
038200020405     C                   MOVEL     F50DP1        V1DPDR
038300020405     C                   move      f50PD1        V1CPDR
038400020405     C                   MOVEL     F50DP2        V2DPDR
038500020405     C                   move      f50PD2        V2CPDR
038600020405     C                   Z-ADD     f50NFT        V1CNFT
038700020423     c     kuni          chain     anuni01l
038800020423     c                   if        %found
038900020423     c                   movel     unidesbrev    v1cdun
039000020423     c                   else
039100020423     c                   movel     *blanks       v1cdun
039200020423     c                   end
039300020405     C                   if        f50DTI > 0
039400020405     C                   move      f50DTI        dataiso
039500000911     C                   else
039600000911     C                   move      '0001-01-01'  dataiso
039700000911     C                   end
039800980911     C                   MOVEL     dataiso       dataeur
039900980911     C                   movel     dataeur       V1CDTI
040000020405     C                   if        f50DTF > 0
040100020405     C                   movel     f50DTF        dataiso
040200000911     C                   else
040300000911     C                   move      '0001-01-01'  dataiso
040400000911     C                   end
040500980911     C                   MOVEL     dataiso       dataeur
040600980914     C                   movel     dataeur       V1CDTF
040700020405     C                   if        f50DFT > 0
040800020405     C                   movel     f50DFT        dataiso
040900000911     C                   else
041000000911     C                   move      '0001-01-01'  dataiso
041100000911     C                   end
041200980911     C                   MOVEL     dataiso       dataeur
041300980911     C                   movel     dataeur       V1CDFT
041400020405     C                   if        f50DPR > 0
041500020405     C                   movel     f50DPR        dataiso
041600000911     C                   else
041700000911     C                   move      '0001-01-01'  dataiso
041800000911     C                   end
041900980911     C                   MOVEL     dataiso       dataeur
042000980911     C                   movel     dataeur       V1CDPR
042100951013      *
042200951013     C                   ENDSR
042300951010?     *--------------------------------------------------------------*
042400020405?     *  CARTRN: Gestione caricamento dati                           *
042500951010?     *--------------------------------------------------------------*
042600951010     C     CARTRN        BEGSR
042700150108      *
042800030827     c                   select
042900031121     c* prestazioni occasionali
043000031121     c                   when      f50cau <> ' '
043100031121     C                   MOVE      cdfso         V1Cfor
043200161116     C                   Z-ADD(h)  Totso         v1cico
043300031121     c* autotrasportatore
043400031121     c                   when      f50tip = 'A'
043500031121     C                   MOVE      cdfs          V1Cfor
043600161116     C                   Z-ADD(h)  Tots          v1cico
043700150108     c* cooperativa
043800030827     c                   when      f50tip = 'C'
043900150108      * Non accorpate
044000150108      *   oppure
044100150108      * x PULIZIE scorporate dall'accorpamento. (nuova legge 2015)
044200150108     C                   if        f50tsr = 'N'
044300150108     c                               or
044400150108     c                             Coop_PULIZIE_a_parte ='S'
044500161116     C                   Z-ADD(h)  Totsc1        v1cico
044600030129     C                   MOVE      cdfsc1        V1Cfor
044700030128     C                   MOVEL     tsrsc1        V1Ctsr
044800030213     C                   MOVEL(p)  tsrsc1        tblkey
044900030213     c* decodifico tipo servizio
045000030213     c     ktab          chain     tabel00f
045100030213     c                   if        %found(tabel00f)
045200030213     c                   movel     tbluni        ds1z
045300030213     c                   movel     §1zdes        v1dtsr
045400030213     c                   else
045500030213     c                   clear                   v1dtsr
045600030213     c                   end
045700150108     c                   else
045800150108      *  oppure  in accorpamento
045900150108      *  ma senza le PULIZIE (x nuova legge 2015)
046000161116     C                   Z-ADD(h)  Totsc         v1cico
046100150108     C                   MOVE      cdfsc         V1Cfor
046200150108     c                   clear                   v1ctsr
046300150108     c                   clear                   v1dtsr
046400030128     c                   end
046500030827     c* aff/defl.
046600030827     c                   when      f50tip = 'D'
046700030827     C                   MOVE      cdfsa         V1Cfor
046800161116     C                   Z-ADD(h)  Totsa         v1cico
046900030827     c                   endsl
047000951010     C                   MOVEL     *BLANKS       V1CSCE
047100021015     c* 23 ind. che protegge il campo di scelta per i fornitori senza p.IVA
047200120816     c*    oppure i fornitori di cui non è specificata la tipologia
047300021015     c                   setoff                                       23
047400020405     C     Kfrn          CHAIN     anfrn01l                           30
047500020405     C  N30Krco          CHAIN     anrco01l                           30
047600020423     c                   if        not *in30 and rcounita <> f50uni
047700020423     c                   seton                                        30
047800020423     c                   end
047900030128     c                   if        not *in30 and f50tip = 'A'
048000030128     C                   if        rcoctgan02 <> 'N   ' and
048100020604     C                             rcoctgan02 <> 'I   ' and
048200020604     C                             rcoctgan02 <> 'M   '
048300120816      * tipologia fornitore se non presente
048400120816      * modificato prima accendeva il 30 per non scrivere la riga ora il 23 per proteggere scelta
048500120816     C                   SETON                                        23
048600020604     c                   end
048700030128     c                   end
048800020514     C  N30rcosogg       CHAIN     ansog01l                           30
048900020514     C* escludo i fornitore che non fanno autofatturazione
049000020514     c                   if        not *in30
049100150108      *
049200030827     c                   select
049300030827     c* autotrasportatori
049400030827     c                   when      f50tip = 'A'
049500030128     c                   movel(p)  'P'           tlztip
049600030827     c                   when      f50tip = 'C'
049700030204     c                   movel(p)  'C'           tlztip
049800030827     c                   when      f50tip = 'D'
049900030827     c                   movel(p)  'D'           tlztip
050000030827     c                   endsl
050100150108      *
050200030128     c                   move      v1cfor        tlzpdr
050300020514     C     ktlz          CHAIN     tntlz01l                           30
050400020514     c                   if        not *in30
050500020514     c                   movel     tlzflo        dtlz01
050600020515     c                   if        f50nft = 0
050700020515     c                   if        §tlzfl1 = 'S'
050800020514     c                   seton                                        30
050900020514     c                   end
051000020515     c                   else
051100020515     c                   if        §tlzfl1 <>'S'
051200020515     c                   seton                                        30
051300020515     c                   end
051400020515     c                   end
051500030205     c                   else
051600030827     c* se non trova il record di tntlz per la coop o x aff/defl. no errore
051700030827     c* esiste solo se la coop o aff/defl. non è o non è stata in autofat.
051800030827     c                   if        tlztip = 'C' or tlztip = 'D'
051900030205     c                   setoff                                       30
052000030205     c                   end
052100020514     c                   end
052200020514     c                   end
052300020514     c*
052400020423     c                   if        not *in30
052500120816      * se non trovata tipologia il 23 è già acceso e do errore per la tipologia
052600120816      * se spento verifica la presenza della partita iva
052700120816     c                   if        *in23
052800120816     C                   SETON                                        28
052900120816     c                   move      '1'           ind23
053000120816     C                   MOVEL     ERR(5)        $MSG
053100120816     c                   else
053200021015     c     sogpartiva    comp      *blanks                                23
053300021015     c                   if        *in23
053400021015     C                   SETON                                        28
053500021016     c                   move      '1'           ind23
053600021015     C                   MOVEL     ERR(2)        $MSG
053700021016     c                   else
053800021016     c                   move      '0'           ind23
053900021015     c                   end
054000120816     c                   endif
054100020405     C                   MOVEL     sogdes        v1dksc
054200030827     c                   select
054300031121     c                   when      f50cau <> ' '
054400031121     C                   MOVEL     §caftab       v1tipf
054500031121     c                   when      f50tip = 'A'
054600031121     C                   MOVEL     rcoctgan02    v1tipf
054700150108      * coop
054800030827     c                   when      f50tip = 'C'
054900150108      * Non accorpate
055000150108      *   oppure
055100150108      * x PULIZIE scorporate dall'accorpamento. (nuova legge 2015)
055200150108     c                   if        f50tsr = 'N'
055300150108     c                               or
055400150108     c                             Coop_PULIZIE_a_parte ='S'
055500150108      * imposta il Tipo Servizio sia se a Dettaglio
055600150108     c                   movel     tsrsc1        v1tipf
055700150108     c                   else
055800150108      * gli accorpati
055900150108     c                   movel     'X'           v1tipf
056000030203     c                   end
056100030827     c                   when      f50tip = 'D'
056200030827     C                   MOVEL     'D'           v1tipf
056300030827     c                   endsl
056400020405     c                   if        frncdiva <> *blanks
056500020405     c                   movel     frncdiva      v1civc
056600020405     c                   else
056700020405     c                   movel     *blanks       v1civc
056800020405     c                   end
056900020509     c* fornitore di riferimento per numerazione fattura
057000020509     c                   movel     FRNFORFATT    v1cfat
057100020509     c                   if        v1cfat = *blanks
057200020509     c                   movel     v1cfor        v1cfat
057300020509     c                   end
057400020509     c                   movel     rcounita      v1cuni
057500030203     c                   if        f50cdp <> *blanks
057600030203     c                   MOVEL     f50cdp        v1ccdp
057700030203     c                   else
057800030203     c                   MOVEL     FRNCONDPAG    v1ccdp
057900030203     c                   end
058000030620     c* controllo se esistono dei conteggi non confermati in tal cas
058100030827     c* segnalo l'anmalia ma non blocco solo per coop e aff/defl
058200040112     c* NO X PRESTAZIONI RESIDUALI
058300040112     c                   if        f50cau = ' '
058400030827     c                   select
058500030827     c                   when      f50tip = 'C'
058600030620     c                   exsr      ctrctt
058700030827     c                   when      f50tip = 'D'
058800030827     c                   exsr      ctratt
058900030827     c                   endsl
059000030827     c                   if        noconf = '1'
059100161012     c                   seton                                        2823
059200161012     c                   move      '1'           ind23
059300161012      **
059400030827     c                   if        f50tip = 'C'
059500030827     C                   MOVEL     ERR(3)        $MSG
059600030827     c                   else
059700030827     C                   MOVEL     ERR(3)        com52            52
059800030827     c                   eval      $msg = com52 + '(FOR.' + attcdf + ')'
059900030827     c                   end
060000161012      **
060100030827     c                   end
060200040112     c                   end
060300080924     c* controllo se tutti gli aut hanno massa complessiva
060400080924     c                   if        (f50tip = 'A' or f50tip = 'D') and
060500080924     c                             ind23 = '0'
060600080924     c                   exsr      ctradg
060700080924     c                   if        erradg <> 0
060800080923     c                   seton                                        2823
060900080924     c                   move      '1'           ind23
061000080924     C                   MOVEL     err(4)        $MSG
061100080923     c                   end
061200080924     c                   end
061300160108     c* controllo se presenti rettifiche di sede e nel caso ci siano
061400160108     c* devono essere minori dell'importo da fatturarare
061500160108     c                   clear                   v1cret
061600160108     c                   exsr      srretsede
061700160115     c                   if        v1cret <> 0 and
061800160115     c                             (v1cico+v1cret) <= impmin
061900160115     C                   SETON                                        2823
062000160108     c                   move      '1'           ind23
062100160108     C                   MOVEL     ERR(6)        $MSG
062200160108     c                   end
062300020408     c*
062400020405     C                   ADD       1             NRR
062500020405     C                   WRITE     y350S01
062600020423     c                   end
062700951010      *
062800951010     C                   ENDSR
062900951009?     *--------------------------------------------------------------*
063000160108?     *  srretsede rettifiche di sede
063100951009?     *--------------------------------------------------------------*
063200160108     C     srretsede     BEGSR
063300160108     c                   eval      sqlcodsav = sqlcod
063400160108     c* con tipo servizio
063500160108     c                   if        v1ctsr <> ' '
063600160108     C/EXEC SQL
063700161116     C+ SELECT round(sum(sretim), 2) INTO :v1cret FROM tnsre00f
063800161116     C+ WHERE sresoc =
063900160108     C+ :f50soc and srecdf = :v1cfor and sretsr = :v1ctsr and sreddc
064000160108     C+ between :f50dti and :f50dtf and srenff = 0
064100160108     C/END-EXEC
064200160108     c                   else
064300160108     c* senza tipo servizio
064400160108     C/EXEC SQL
064500161116     C+ SELECT round(sum(sretim), 2) INTO :v1cret FROM tnsre00f
064600161116     C+ WHERE sresoc =
064700160108     C+ :f50soc and srecdf = :v1cfor and sretsr <> 'P' and sreddc
064800160108     C+ between :f50dti and :f50dtf and srenff = 0
064900160108     C/END-EXEC
065000160108     c                   end
065100160108     c                   eval      sqlcod = sqlcodsav
065200160108      *
065300160108     C                   ENDSR
065400160108?     *--------------------------------------------------------------*
065500160108?     *  srcmd   Gestione comandi                                    *
065600160108?     *--------------------------------------------------------------*
065700160108     C     srcmd         BEGSR
065800020415     c                   clear                   f50cmd
065900020405      *  Fine Lavoro
066000020405     C     *INKC         IFEQ      '1'
066100020405     C                   MOVEL     'S'           WFINE
066200020405     C                   MOVEL     '03'          f50CMD
066300020405     C                   END
066400020405      *  Ritorno
066500020405     C     *INKL         IFEQ      '1'
066600020405     C                   MOVEL     'S'           WFINE
066700020405     C                   MOVEL     '12'          f50CMD
066800020405     C                   END
066900951010      *
067000020405     C                   ENDSR
067100020405?     *--------------------------------------------------------------*
067200020405?     *  GESS01: Gestione prima videata                              *
067300020405?     *--------------------------------------------------------------*
067400020405     C     GESS01        BEGSR
067500020405      *
067600980914     C                   WRITE     y350T01
067700980914     C                   EXFMT     y350C01
067800020405     c* gestione comandi
067900020405     c                   exsr      srcmd
068000020415     c                   if        f50cmd <> *blanks
068100020415     c                   goto      finvd1
068200020415     c                   end
068300951009      *  Controlli
068400951009     C                   EXSR      CTR01
068500020405     C   28              GOTO      FINVD1
068600020408      *  CONFERMA
068700951013     C     *INKF         IFEQ      '1'
068800000204      * Richiamo pgm prima nota fatture
068900951013     C                   EXSR      CALPNF
069000951013      *  Se quando esco dal programma di prima nota è tutto O.K
069100980914     C                   MOVEL     'S'           WFINE
069200020415     C                   MOVEL     '06'          f50CMD
069300980911     C                   END                                                    V3CTOT = y35FAT
069400951009      *
069500951009     C     FINVD1        ENDSR
069600951010?     *--------------------------------------------------------------*
069700951010?     *  GESD02: Gestione seconda videata                            *
069800951010?     *--------------------------------------------------------------*
069900951010     C     GESD02        BEGSR
070000951010      *
070100980914     C                   EXFMT     y350D02
070200020405     c* gestione comandi
070300020405     c                   exsr      srcmd
070400951010      *
070500951010     C                   ENDSR
070600951009?     *--------------------------------------------------------------*
070700951009?     *  CTR01: Controlli prima videata                              *
070800951009?     *--------------------------------------------------------------*
070900951009     C     CTR01         BEGSR
071000021016     c                   setoff                                       28
071100951009      *
071200020405     C                   Z-ADD     0             V1CITT
071300160108     C                   Z-ADD     0             V1Crett
071400951013     C                   MOVEL     'N'           WSELTR            1
071500020610     C                   MOVEL     *BLANKS       $MSG
071600020415      *  Controllo se devo selezionare tutte i fornitori
071700951010     C     *INKA         IFEQ      '1'
071800951010     C                   Z-ADD     1             NRR
071900980911     C     NRR           CHAIN     y350S01                            29
072000951010     C     *IN29         DOWEQ     '0'
072100021016     c* non scelgo i record con ind23 = 1 xchè manca p.iva
072200021016     c                   if        ind23 = '0'
072300951013     C                   MOVEL     'S'           WSELTR            1
072400951010     C                   MOVEL     '1'           V1CSCE
072500951010     C                   ADD       V1CICO        V1CITT
072600160108     C                   ADD       V1Cret        V1Crett
072700951010     C                   SETON                                        22
072800021016     c                   movea     ind23         *in(23)
072900980911     C                   UPDATE    y350S01
073000951010     C                   SETOFF                                       22
073100021016     c                   end
073200951010     C                   ADD       1             NRR
073300980911     C     NRR           CHAIN     y350S01                            29
073400011017     C                   ENDdo
073500951010     C                   ELSE
073600951010      *  Solo i selezionati
073700980911     C                   READC     y350S01                                29
073800951010     C     *IN29         DOWEQ     '0'
073900020405     C                   select
074000030128     C                   when      v1csce = 'D'
074100020405     c                   movel     ficn50ds      salds
074200020528     c                   movel     v1cfor        f50pd2
074300020417     c                   movel     v1dksc        f50dp2
074400020415     c                   movel     v1cfor        f50pd1
074500020415     c                   movel     v1dksc        f50dp1
074600020528     c*
074700020417     c                   movel(p)  ficn50ds      kpjbu
074800030827     c* dettaglio aut.
074900031121     C                   if        (F50TIP='A' or f50tip='D') and f50cau=' '
075000020405     c                   call      'FICN52R'
075100020417     c                   parm                    kpjba
075200030827     C                   END
075300020417     c                   movel     kpjbu         ficn50ds
075400020415     c                   movel     f50cmd        £f50cmd
075500020408     c                   movel     salds         ficn50ds
075600020408     c                   movel     *blanks       v1csce
075700020610     C                   SETOFF                                       22
075800021016     c                   movea     ind23         *in(23)
075900020408     C                   UPDATE    y350S01
076000020415     c                   if        f50cmd = '03'
076100020415     c                   move      *on           *inkc
076200020405     c                   exsr      srcmd
076300020415     c                   goto      finct1
076400020415     c                   end
076500020405     c*
076600020405     C     V1CSCE        wheneq    '1'
076700020405     C                   MOVEL     'S'           WSELTR            1
076800951010     C                   ADD       V1CICO        V1CITT
076900160108     C                   ADD       V1Cret        V1Crett
077000951010     C                   SETON                                        22
077100021016     c                   movea     ind23         *in(23)
077200980911     C                   UPDATE    y350S01
077300951010     C                   SETOFF                                       22
077400020610     C                   OTHER
077500020610     C                   SETOFF                                       22
077600021016     c                   movea     ind23         *in(23)
077700020610     C                   UPDATE    y350S01
077800020405     C                   ENDsl
077900980911     C                   READC     Y350S01                                29
078000011017     C                   ENDdo
078100951010     C                   END
078200020415      *  Nessun fornitore selezionato
078300020408     C     WSELTR        IFeq      'N'
078400020405     C     *INKF         andeq     '1'
078500951013     C                   SETON                                        28
078600020415     C                   MOVEL     ERR(1)        $MSG
078700951013     C                   GOTO      FINCT1
078800951013     C                   END
078900951010      *
079000951010     C     FINCT1        ENDSR
079100000204     C*------------------------------------------------------*
079200020408     C*  Reperisco il numero assoluto di registrazione       *
079300000204     C*------------------------------------------------------*
079400000204     C     prenotA       BEGSR
079500030206     c                   z-add     0             riga1
079600030206     c                   z-add     0             riga2
079700000204     C* Prima riga
079800020415     c                   clear                   xnmuni
079900020415     c                   clear                   xnmctb
080000020415     c                   clear                   xnmkey
080100020415     c                   clear                   xnmerr
080200020415     c                   clear                   xnmnum
080300020415     c                   clear                   xnmaut
080400020409     C                   MOVE      *DATE         XnmDat
080500020409     C                   MOVE      *DATE         XnmDag                         opzionale
080600020409     C                   move      *OFF          XnmCmt
080700020409     C                   move      *OFF          XnmIva
080800020409     C                   move      *OFF          XnmReg
080900020409     C                   EVAL      XnmRic = '2'
081000020409     C                   EVAL      XnmSer = 'NRAS'
081100020409     C                   EXSR      CalXnmr
081200020409     c                   z-add     xnmnum        riga1
081300000204     C* Seconda riga
081400030204     c                   if        tredici = *on
081500020415     c                   clear                   xnmerr
081600020415     c                   clear                   xnmnum
081700020409     C                   EXSR      CalXnmr
081800020409     c                   z-add     xnmnum        riga2
081900020409     c                   end
082000000204     C*
082100000204     C                   ENDSR
082200020409?     *--------------------------------------------------------------*
082300020409      * Chiamata a XNMR.
082400020409?     *--------------------------------------------------------------*
082500020409     C     CalXnmr       BEGSR
082600020409      *
082700020409      *
082800020409     C                   CALLB     'XNMR'
082900020409     C                   PARM                    XnmRic
083000020412     C                   PARM      xscsoc        Xnmsoc
083100020409     C                   PARM                    XnmUni
083200020409     C                   PARM                    XnmCtb
083300020409     C                   PARM                    XnmKey
083400020409     C                   PARM                    XnmSer
083500020409     C                   PARM                    XnmCmt
083600020409     C                   PARM                    XnmDat
083700020409     C                   PARM                    XnmIva
083800020409     C                   PARM                    XnmReg
083900020409     C                   PARM                    XnmErr
084000020409     C                   PARM                    XnmNum
084100020409     C                   PARM                    XnmAut
084200020409     C                   PARM                    XnmDag                         opzionale
084300020409      *
084400020409     C                   ENDSR
084500951011?     *--------------------------------------------------------------*
084600951011?     *  CALPNF: Carico subfile e richiamo prima nota fatture        *
084700951011?     *--------------------------------------------------------------*
084800951012     C     CALPNF        BEGSR
084900980914      *
085000020408     C                   READC     y350S01                                29
085100020408     C     *IN29         DOWEQ     '0'
085200020408     C     V1CSCE        IFEQ      '1'
085300040914     c* se importo > = importo mnimo da fatturare preso dalla tab. BLC/O/D
085400160108     C                   if        (v1cico+v1cret) >= impmin
085500030203     c* scrivo il sfl di comodo dove memorizzo gli importi per ogni mese
085600030203     c* di competenza
085700030203     c                   exsr      srsfl2
085800030206     c* per ogni tipologia fornitore/servizio aggancio la tabella Y4Z
085900030203     c                   if        v1tipf <> v1tips
086000030206     c                   eval      v1tip = v1tipf
086100020527     c                   exsr      sry4z
086200020527     c                   movel     v1tipf        v1tips
086300020527     c                   end
086400020417     c* imponibile 18,2
086500160115     c                   eval(h)   imponibile= (v1cico+v1cret)
086600030206     c* verifico eventuale squadratura di arrotondamento e la metto nel
086700030206     c* costo dell'anno in corso
086800030206     c                   xfoot     impt          totimp
086900030206     c                   xfoot     impf          totimpf
087000030206     c                   add       totimpf       totimp
087100030206     c                   eval      diffe = imponibile - totimp
087200030206     c                   if        diffe <> 0
087300030206     c                   z-add     1             xy
087400030206     c     v1tipf        lookup    tipf(xy)                               56
087500030206     c                   if        *in56
087600030206     c                   add       diffe         impf(xy)
087700030206     c                   end
087800030206     c* aggiorno anche il sfl 2
087900080925     c                   z-add     diffe         comsfl2
088000030206     c                   exsr      upsfl2
088100030206     c                   end
088200030206     c* Se devo gestire la competenza 13 prelievo il nr. assoluto
088300030206     c* per poi riaggiornare i record in modo da collegare le due registraz.
088400020408     C                   EXSR      PRENOTA
088500020409     C* REPERISCO IL NUMERO DI PROTOCOLLO
088600020415     c                   clear                   xnmkey
088700020415     c                   clear                   xnmser
088800020415     c                   move      *off          xnmcmt
088900020415     c                   clear                   xnmerr
089000020415     c                   clear                   xnmnum
089100020415     c                   clear                   xnmaut
089200020409     C                   MOVE      F50DPR        XnmDat
089300020409     C                   MOVE      F50DPR        XnmDag                         opzionale
089400020409     C                   MOVE      §IVLAQ        COM3              3            opzionale
089500150204      *****
089600150205      *****  per il Reverse Charge
089700150204     c                   if        v1tipf = 'P'
089800150205     C                   MOVE      §IVLAR        COM3              3            opzionale
089900150204     c                   end
090000150204      *****
090100020409     C                   EVAL      XNMSER = '1' + COM3
090200020411     C                   movel(p)  v1cuni        Xnmuni
090300020409     C                   movel     '3'           XnmRic
090400020409     C                   movel     'CG'          XnmCtb
090500020409     C                   movel     *ON           XnmIva
090600020409     C                   movel     *ON           XnmReg
090700020409     C                   EXSR      CALXNMR
090800020409     c                   move      xnmnum        f50npr            9 0
090900030204      *  FORNITORE
091000980914     c                   clear                   ndbhm000
091100150119      *-----
091200150119     c* calcolo iva
091300150119     c                   exsr      sriva
091400150119     c     impiva        add       imponibile    bhmimporto
091500150119     C                   EVAL      bhmserreg = '1' + COM3
091600150119     c* se si tratta di PULIZIE delle COOP
091700150119     c*  deve essere passata senza l'IVA e senza serie per reverse charge
091800150119     c                   if        v1tipf = 'P' and f50TIP = 'C' and
091900150119     c                             impiva <> 0
092000150119     c                   clear                   bhmserreg
092100150119     c                   z-add     imponibile    bhmimporto
092200150119     c                   end
092300150119      *-----
092400020417     c                   z-add     riga1         bhmnrasreg
092500020415     c                   add       1             subnum
092600020409     c                   z-add     subnum        bhmsubnum
092700020409     c                   z-add     1             bhmsubrig
092800020409     c                   move      knraz         bhmrifint
092900020409     c                   movel     'CP'          bhmrifint
093000150109      **
093100020417     c                   move      f50dpr        bhmdtreg
093200020417     c                   z-add     f50npr        bhmnrreg
093300020417     c                   movel     forita        bhmkcc
093400020417     c                   movel     v1cfor        bhmksc
093500020417     c                   movel     v1ccdp        bhmcondpag
093600020417     c                   movel     xscsoc        bhmsocieta
093700020417     c                   movel     'CG'          bhmctb
093800020417     c                   movel     'CG'          bhmclasse
093900020411     c                   movel(p)  v1cuni        bhmunita
094000020417     c                   movel     'D'           bhmtpregz
094100020527     c                   movel     'EUR'         bhmdivisa
094200020527     c* data e nr fattura documento fornitore:
094300020409     c* se impostata a video tengo buona quella altrimenti la reperisco
094400020409     c* dal numeratore YPDR del fornitore
094500020408     c                   if        f50dft <> 0
094600020527     c* Imposto le causali contabili relative alla NON autofatturazione
094700030203     c                   if        tredici = *on
094800020527     c                   movel     §4zcrp        bhmcaustes
094900020527     c                   else
095000020527     c                   movel     §4zcrc        bhmcaustes
095100150116     c* causale per iva esente
095200150119     c                   if        §4zcrce <> ' '  and impiva = 0
095300150116     c                   movel     §4zcrce       bhmcaustes
095400150116     c                   end
095500150116     c                   end
095600020417     c                   move      f50dft        bhmdtdoc
095700020417     c                   z-add     f50nft        bhmnrdoc
095800020417     c                   move      f50dft        bhmdtpar
095900020417     c                   z-add     f50nft        bhmnrpar
096000020408     c                   else
096100020527     c* Imposto le causali contabili relative alla autofatturazione
096200030203     c                   if        tredici = *on
096300020527     c                   movel     §4zcrpa       bhmcaustes
096400020527     c                   else
096500020527     c                   movel     §4zcrca       bhmcaustes
096600150116     c* causale per iva esente
096700150119     c                   if        §4zcrcae <> ' ' and impiva = 0
096800150116     c                   movel     §4zcrcae      bhmcaustes
096900020527     c                   end
097000150116     c                   end
097100020415     c                   clear                   xnmuni
097200020415     c                   clear                   xnmctb
097300020417     c                   movel     'YPDR'        xnmser
097400020415     c                   move      *off          xnmcmt
097500020415     c                   move      *off          xnmiva
097600020415     c                   move      *off          xnmreg
097700020415     c                   clear                   xnmerr
097800020415     c                   clear                   xnmnum
097900020415     c                   clear                   xnmaut
098000020408     c                   movel     '3'           xnmric
098100020509     c                   movel     v1cfat        xnmkey
098200020417     c                   move      f50dpr        xnmdat
098300020417     C                   move      f50dpr        XnmDag                         opzionale
098400020409     c                   exsr      calxnmr
098500020417     c                   move      xnmdat        bhmdtdoc
098600020417     c                   move      xnmnum        com9              9
098700020417     c                   eval      %subst(com9: 3: 3) = xscsoc
098800020417     c                   move      com9          bhmnrdoc
098900020410     c                   move      xnmdat        bhmdtpar
099000020417     c                   move      com9          bhmnrpar
099100020408     c                   end
099200031204     c* reperisco segno contabile dalla causale
099300031204     c                   exsr      srope
099400031204     c                   eval      bhmsegno = segnofor
099500020503     c* salvo data e numero documento fornitore
099600020503     c                   move      bhmdtdoc      savdtdoc
099700020503     c                   move      bhmnrdoc      savnrdoc
099800080924     c* calcolo adeguamento l'eventuale carburante
099900080924     c                   if        f50tip = 'A' or f50tip = 'D'
100000080924     c                   exsr      caradg
100100080924     c                   end
100200150108      **
100300020417     c                   z-add     bhmimporto    bhmimportd
100400990507     c                   z-add     1             bhmcambio
100500980914     c                   write     ndbhm000
100600030205     c* scrittura NDBHP00F
100700030205     c                   exsr      writebhp
100800030204     c* scrittura riga iva / esenzione
100900030203     c                   exsr      writebhi
101000030204     c* CONTROPARTITA (conti di costo --> bhm)
101100030204     c                   do        10            x
101200030204     c                   if        tipf(x) = *blanks
101300030204     c                   leave
101400030204     c                   end
101500030204     c                   if        impf(x) = 0
101600030204     c                   iter
101700030204     c                   end
101800030204     c* per ogni tipologia servizio aggancio la tabella Y4Z
101900030204     c* solo se sono stati accorpati i servizi altrimenti l'ho già letta
102000030204     c* inizialmente
102100030204     c                   if        f50tsr = 'S' and tipf(x) <> v1tips
102200030206     c                   eval      v1tip = tipf(x)
102300030204     c                   exsr      sry4z
102400030204     c                   movel     tipf(x)       v1tips
102500030204     c                   end
102600980914     c                   add       1             bhmsubrig
102700020417     C                   movel     §4zkcc        bhmkcc
102800020417     C                   movel     §4zksc        bhmksc
102900020417     c                   movel     *blank        bhmcondpag
103000031204     c                   if        segnofor = 'A'
103100980914     C                   MOVEL     'D'           bhmsegno
103200031204     c                   else
103300031204     C                   MOVEL     'A'           bhmsegno
103400031204     c                   end
103500030204     c                   movel     forita        bhmkcccpu
103600980914     c                   move      *zeros        bhmksccpu
103700020417     c                   move      v1cfor        bhmksccpu
103800161116     c                   z-add     impf(x)       bhmimporto
103900020417     c                   z-add     bhmimporto    bhmimportd
104000020409     c                   write     ndbhm000
104100030204     c* analitica
104200030204     c                   exsr      writebha
104300030204     c                   enddo
104400030204     c* FATTURE DA RICEVERE (unico indipendentemente dal tipo servizio)
104500030206     c                   xfoot     impt          totimpt
104600030204     c                   if        totimpt <> 0
104700030204     c                   add       1             bhmsubrig
104800030210     c                   movel     *blank        bhmcondpag
104900031204     c                   if        segnofor = 'A'
105000030210     C                   MOVEL     'D'           bhmsegno
105100031204     c                   else
105200031204     C                   MOVEL     'A'           bhmsegno
105300031204     c                   end
105400030210     c                   movel     forita        bhmkcccpu
105500030210     c                   move      *zeros        bhmksccpu
105600030210     c                   move      v1cfor        bhmksccpu
105700030204     c                   movel     §4zkcr        bhmkcc
105800030204     c                   movel     §4zksr        bhmksc
105900030204     c                   z-add     totimpt       bhmimporto
106000030204     c                   z-add     bhmimporto    bhmimportd
106100030204     c                   write     ndbhm000
106200030204     c* REGISTRAZIONE ANNO PRECEDENTE
106300980916     c                   exsr      competenza
106400020409     c                   z-add     savsubrig     bhmsubrig
106500020410     c                   z-add     savsubnum     bhmsubnum
106600020417     c                   move      f50dpr        bhmdtreg
106700020409     c                   end
106800020411     c                   commit
106900020614      * richiamo pgm x ctb
107000980914     c                   exsr      primanota
107100021127     c                   z-add     0             savdcn
107200020614     c                   else
107300040914     c* se IMPORTO < importo minimo da fatturare
107400020614     c                   z-add     0             riga1
107500020614     c                   z-add     999999999     savnrdoc
107600020614     c                   move      f50dpr        savdtdoc
107700021127     c                   move      f50dpr        savdcn
107800020614     c                   end
107900031121      * imposto il numero della fattura nel file FIFTT00F/FICTT00F/FIATT00F
108000031121      * FIOTT00F se ok contabilizzazione
108100030204     c* (se i record cancellati dalla funzione sono tutti 0 vuol dire che
108200030204     c* la contabilizzaz. è andata bene)
108300030204     c                   clear                   ycodel1ds
108400030204     c                   if        riga1 <> 0
108500030204     c                   move      bhmsocieta    del1sc
108600030204     c                   move      bhmrifint     del1ri
108700030204     c                   move      *on           del1ct
108800030204     c                   movel(p)  ycodel1ds     kpjbu
108900030204     c                   call      'YCODEL1R'
109000030204     c                   parm                    kpjba
109100030204     c                   movel     kpjbu         ycodel1ds
109200030204     c                   end
109300030204     c*
109400030204     c                   if        del1fl = ' ' and del1np = 0 and
109500030204     c                             del1nm=0 and del1ni=0 and del1na=0
109600030827     c                   select
109700031121     c                   when      f50cau <>' '
109800031121     C                   EXSR      AGGott
109900031121     c                   when      f50tip = 'A'
110000031121     C                   EXSR      AGGftt
110100030827     c                   when      f50tip = 'C'
110200030128     C                   EXSR      AGGctt
110300030827     c                   when      f50tip = 'D'
110400030827     C                   EXSR      AGGatt
110500030827     c                   endsl
110600160108     c                   exsr      aggsre
110700030204     c                   end
110800020408      *  Lettura record successivo
110900020610     C                   SETOFF                                       22
111000021016     c                   movea     ind23         *in(23)
111100020408     C                   UPDATE    y350S01
111200020614     c                   end
111300020408     C                   READC     y350S01                                29
111400020409     C                   enddo                                                  *IN29 = 0
111500951011     C                   ENDSR
111600000426     c*----------------------------------------------------------------
111700080924     c     ctradg        begsr
111800000426     c*----------------------------------------------------------------
111900081006     c                   clear                   erradg            2 0
112000080924     c                   eval      sqlcodsav = sqlcod
112100080924     C/EXEC SQL
112200081006     C+ SELECT count(*) INTO :erradg FROM fiapd00f
112300081006     C+ WHERE apdatb= ' ' and apdtip = :f50tip and apdcsf
112400161206     C+ = :f50soc and apdksc = :v1cfor and (substr(apdflr, 9, 7)
112500161206     C+ ='0000000' or substr(apdflr, 9, 7)= ' ') and apdPDD <>'S'
112600080924     C/END-EXEC
112700080924     c                   eval      sqlcod = sqlcodsav
112800080924     C                   ENDSR
112900080924     c*----------------------------------------------------------------
113000080924     c     caradg        begsr
113100080924     c*----------------------------------------------------------------
113200080925     c* autotrasportatori di città
113300080924     c                   if        f50tip = 'A'
113400080924     C                   EVAL      WrkSqladg
113500080924     C                             =
113600080924     C                             'SELECT fttpdr,'
113700080924     C                             +
113800080924     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
113900080924     C                             +
114000080924     C                             'fttmnt)'
114100080924     C                             +
114200080924     C                             ' from fiftt06l where fttsoc = '
114300080924     C                             +
114400080924     C                             '''' + f50soc  + ''''
114500080924     C                             +
114600080924     C                             ' AND '
114700080924     C                             +
114800080924     C                             ' fttcdf between '
114900080924     C                             +
115000080924     C                             '''' + v1cfor + ''''
115100080924     C                             +
115200080924     C                             ' AND '
115300080924     C                             +
115400080924     C                             '''' + v1cfor + ''''
115500080924     C                             +
115600080924     C                             ' AND '
115700080924     C                             +
115800080924     C                             ' ftttsr = '' '''
115900080924     C                             +
116000080924     C                             ' AND '
116100080924     C                             +
116200080924     C                             ' fttfvl = ''C'''
116300080924     C                             +
116400080924     C                             ' AND '
116500080924     C                             +
116600080924     C                             ' Fttddc BETWEEN '
116700080924     C                             + %editc(f50dti:'X')
116800080924     C                             +
116900080924     C                             ' AND ' + %editc(f50dtf:'X')
117000080924     C                             +
117100080924     C                             ' AND '
117200080924     C                             +
117300080924     C                             'fttnff = 0  group by fttpdr '
117400080925     c                   end
117500080925     c                   if        f50tip = 'D'
117600080924     C                   EVAL      WrkSqladg
117700080924     C                             =
117800080924     C                             'SELECT attpdr,'
117900080924     C                             +
118000080924     C                             ' sum(attimpp)'
118100080924     C                             +
118200080924     C                             ' from fiatt06l where attsoc = '
118300080924     C                             +
118400080924     C                             '''' + f50soc  + ''''
118500080924     C                             +
118600080924     C                             ' AND '
118700080924     C                             +
118800080924     C                             ' attcdf between '
118900080924     C                             +
119000080924     C                             '''' + v1cfor + ''''
119100080924     C                             +
119200080924     C                             ' AND '
119300080924     C                             +
119400080924     C                             '''' + v1cfor + ''''
119500080924     C                             +
119600080924     C                             ' AND '
119700080924     C                             +
119800080924     C                             ' attdco <> 0'
119900080924     C                             +
120000080924     C                             ' AND '
120100080924     C                             +
120200080924     C                             ' attddc BETWEEN '
120300080924     C                             + %editc(f50dti:'X')
120400080924     C                             +
120500080924     C                             ' AND ' + %editc(f50dtf:'X')
120600080924     C                             +
120700080924     C                             ' AND '
120800080924     C                             +
120900080924     C                             'attnff = 0  group by attpdr '
121000080924     c                   end
121100080924     C/EXEC SQL
121200080924     C+ PREPARE S3 FROM :WrkSqladg
121300080924     C/END-EXEC
121400080924
121500080924     C/EXEC SQL
121600080924     C+ DECLARE A3 CURSOR FOR S3
121700080924     C/END-EXEC
121800080924
121900080924     C/EXEC SQL
122000080924     C+ OPEN A3
122100080924     C/END-EXEC
122200080924
122300080924     C                   DOU       SqlCod <> 0
122400080924     C/EXEC SQL
122500080924     C+ FETCH NEXT FROM A3 INTO :fttatt3
122600080924     C/END-EXEC
122700080924     c*
122800080924     C                   SELECT
122900080924     C                   WHEN      SqlCod = 0
123000080924     c                   clear                   ficn74ds
123100080924     c                   move      pdr3          i74pdr
123200080924     c                   eval      i74tip = f50tip
123300161116     c                   z-add(h)  tot3          comtot3           9 2
123400081002     c                   z-add     comtot3       i74imp
123500080924     C                   if        f50dpr > 0
123600080924     C                   move      f50dpr        I74DTPRE
123700080924     c                   end
123800080924     c                   call      'FICN74R'
123900080924     c                   parm                    ficn74ds
124000080926     C     o74err        ifeq      ' '
124100080924     c                   exsr      agganf2
124200080924     C                   END
124300080924     C                   WHEN      SqlCod = 100
124400080924     C                   WHEN      SqlCod <> 0
124500080924      * Forzo la stampa del JOBLOG.
124600080924     C                   CALL      'X66CHGJOB'
124700080924     C                   ENDSL
124800080924     C                   ENDDO
124900080924     C/EXEC SQL
125000080924     C+ CLOSE A3
125100080924     C/END-EXEC
125200080923     C                   ENDsr
125300080923     c*----------------------------------------------------------------
125400080923     c     agganf2       begsr
125500080923     c*----------------------------------------------------------------
125600080923     C                   EVAL      ANFATB = ' '
125700080923     C                   EVAL      ANFTRC = '2'
125800080924     C                   eval      ANFTIP = f50tip
125900080924     C                   EVAL      ANFTSR = f50tip
126000080923     C                   EVAL      ANFSOC = xscSoc
126100080923     c                   eval      anfcdf = v1cfor
126200080925     c                   move      bhmdtdoc      anfdft
126300080925     c                   move      bhmnrdoc      anfnff
126400080924     C                   eval      ANFPDR = pdr3
126500080923     C     Kanf2         chain     TNanf01L
126600080923     c                   clear                   danf2
126700080923     C                   EVAL      §ANF2COR = i74imp
126800080924     c                   z-add     o74impco      comico           11 2
126900080924     C                   z-add     comico        §ANF2TGC
127000080923     C                   EVAL      §ANF2GPI = o74gpi
127100080923     C                   EVAL      §ANF2VAR = o74percau
127200080924     c                   z-add     o74impa       comico
127300080924     C                   z-add     comico        §ANF2ADG
127400080924     c                   add       comico        imponibile
127500080925     c* aggiorno l'importo della contropartita con l'adeguamento carburante
127600080924     c                   z-add     1             xy
127700080924     c     v1tipf        lookup    tipf(xy)                               56
127800080924     c                   if        *in56
127900080924     c                   add       comico        impf(xy)
128000080925     c* aggiorno anche il sfl 2
128100080925     c                   z-add     comico        comsfl2
128200080925     c                   exsr      upsfl2
128300080924     c                   end
128400080923     C                   EVAL      ANFUNI = danf2
128500080923     c                   if        %found(tnanf01l)
128600080923     C                   update    tnanf000
128700080923     c                   else
128800080923     C                   write     tnanf000
128900080923     c                   end
129000080923     C                   ENDSR
129100080923     c*----------------------------------------------------------------
129200080923     c     writebhp      begsr
129300080923     c*----------------------------------------------------------------
129400031204     c*    kope          chain     anope01l
129500031204     c*                  if        %found(anope01l)
129600020708     c     kdft          chain     andft01l
129700020709     c                   if        %found(andft01l)
129800020708     c                   move      bhmrifint     bhprifint
129900020708     c                   move      bhmsubnum     bhpsubnum
130000020708     c                   move      bhmsubrig     bhpsubrig
130100020708     c                   z-add     1             bhpsubsubr
130200020708     c                   move      bhmdtpar      bhpdtpar
130300020708     c                   z-add     bhmnrpar      bhpnrpar
130400020709    >C                   EVAL      BHPimporto = bhmimporto
130500020709    >C                   EVAL      BHPimportd = bhmimportd
130600020709    >C                   EVAL      BHPdivisa  = bhmdivisa
130700020709    >C                   EVAL      BHPsocieta = BHMsocieta
130800020709    >C                   EVAL      BHPsegno   = Bhmsegno
130900020709    >C                   EVAL      BHPcambio  = Bhmcambio
131000020708     c                   eval      bhppagman = *off
131100020708     c                   eval      bhpbkamm = dftbkamm
131200020708     c                   write     ndbhp000
131300031204     c*                  end
131400020708     c                   end
131500020708     C                   ENDSR
131600020708     c*----------------------------------------------------------------
131700020708     c     writebha      begsr
131800020708     c*----------------------------------------------------------------
131900030205     c                   z-add     0             bhasubsubr
132000030205     c                   do        10            xyz
132100030204     c     xyz           chain     y350s02                            56
132200030205     c                   if        *in56
132300030205     c                   leave
132400030205     c                   else
132500030204     c* seleziono il tipo forn./servizio
132600030204     c                   if        tipf(x) <> h1tipf
132700030204     c                   iter
132800030204     c                   end
132900030204     c* seleziono le competenze dell'anno che sto analizzando
133000030204     c                   if        totimpt <> 0
133100030204     c                   if        h1ccmp <> 13
133200030204     c                   iter
133300030204     c                   end
133400030204     c                   else
133500030204     c                   if        h1ccmp = 13
133600030204     c                   iter
133700030204     c                   end
133800030204     c                   end
133900030204     c*
134000030205     c                   add       1             bhasubsubr
134100980914     c                   move      bhmrifint     bharifint
134200980914     c                   move      bhmsubnum     bhasubnum
134300980914     c                   move      bhmsubrig     bhasubrig
134400980914     c                   move      'D1'          bhadimen
134500980914     c                   move      xscsoc        bhasocieta
134600980914     c                   add       1             bhasubsubr
134700030204     c                   z-add     h1cico        bhaimporto
134800020417     c                   z-add     bhaimporto    bhaimportd
134900031204     c                   if        segnofor = 'A'
135000980914     c                   move      'D'           bhasegno
135100031204     c                   else
135200031204     c                   move      'A'           bhasegno
135300031204     c                   end
135400030204     c                   if        §4zcdb <> *blanks
135500030204     c                   movel     §4zcdb        bhaentit1
135600030204     c                   else
135700030204     c                   movel     v1cuni        bhaentit1
135800030204     c                   end
135900020408     c                   movel     §4zvoc        bhavoce
136000000426      * Se devo gestire la comp. 13 imposto data analitica anno preced.
136100030204     c                   if        h1ccmp = 13
136200980916     c                   move      dtcoan        bhadtcoan
136300020415     c                   else
136400000426      * Se non devo gestire la comp. 13 imposto data analitica=data reg.
136500020417     c                   move      bhmdtreg      campo10          10
136600020409     c* e gg. data reg. = primo del mese
136700020417     c                   move      '-01'         campo5            5
136800030204     c                   movel     h1ccmp        campo5
136900020417     c                   move      campo5        campo10
137000020417     c                   move      campo10       bhadtcoan
137100020415     c                   end
137200980916     c                   write     ndbha000
137300030204     c                   end
137400030204     c                   enddo
137500980914     c                   endsr
137600980914     c*-----------------------------------------
137700980914     c     writebhi      begsr
137800980914     c*-----------------------------------------
137900980914      * scrivo eventuali righe iva
138000031204     c                   if        segnofor = 'A'
138100020410     c                   move      'D'           bhisegno
138200031204     c                   else
138300031204     c                   move      'A'           bhisegno
138400031204     c                   end
138500020410     c                   move      xscsoc        bhisocieta
138600020410     c                   add       1             bhmsubrig
138700020410     c                   z-add     bhmsubrig     bhisubrig
138800020410     c                   move      bhmrifint     bhirifint
138900020410     c                   move      bhmsubnum     bhisubnum
139000020418     c                   z-add     imponibile    bhiimpond
139100020417     c                   z-add     bhiimpond     bhiimpon
139200020410     c                   move      '1'           bhitpreg
139300150108      **
139400150108     c* se si tratta di PULIZIE delle COOP
139500150119     c*  NON deve essere passato il libro IVA (dal 2015) perchè lo
139600150119     c* prende dall'anagrafica della causale
139700150108     c                   if        v1tipf = 'P' and f50TIP = 'C'
139800150119     c                             and impiva <> 0
139900150108     c                   clear                   bhilibro
140000150108     c                   else
140100020410     c                   move      §ivlaq        bhilibro
140200150108     c                   end
140300150108      **
140400020418     c                   z-add     impiva        bhiimportd
140500020417     c                   z-add     bhiimportd    bhiimporto
140600031204     c                   clear                   bhirifiva
140700031204     c                   clear                   bhiali
140800031204     c* se NON presente iva sull'anagrafico del fornitore prendo dalla tab.
140900031204      * y4z
141000031204     c                   if        v1civc = *blanks
141100031204     c                   if        §4ziva <>0
141200031204      * iva
141300020417     c                   z-add     §4ziva        bhiali
141400020417     c                   else
141500031204      * esenzioni
141600031204     c                   move      §4zivc        bhirifiva
141700020417     c                   end
141800020417     c                   else
141900031204      * se presente iva nell'anagrafico fornitore prendo dalla tabella
142000031204     c                   if        alig48 <> 0
142100031204      * iva
142200031204     c                   z-add     alig48        bhiali
142300031204     c                   else
142400031204      * esenzioni
142500020417     c                   move      kscg48        bhirifiva
142600020417     c                   endif
142700031204     c                   endif
142800020410     c                   write     ndbhi000
142900020408     c*
143000980914     c                   endsr
143100980916     c*-----------------------------------------
143200030206     c     sriva         begsr
143300980916     c*-----------------------------------------
143400030206     c* iva 13,2
143500030206     c                   z-add     0             impiva
143600020417     c* se il fornitore ha un codice IVA
143700020417     C                   CLEAR                   tabg48
143800020417     c                   if        v1civc <> *blanks
143900020417     C                   CLEAR                   X06DS
144000020417     C                   MOVE      xscsoc        X06SOC
144100020417     C                   MOVE      XSCLIN        X06LIN
144200020417     C                   MOVE      v1civc        X06CIV
144300020417     C                   MOVE      f50dpr        X06DAT
144400020417     C                   CALL      'X06G48'
144500020417     C                   PARM                    X06DS
144600020417     C     X06ERR        IFEQ      '0'
144700020417     C                   MOVEL     X06UNI        TABG48
144800020417     C                   end
144900020417     C                   end
145000020417     c* aliquota IVA
145100020417     c                   if        v1civc = *blanks or alig48 <> 0
145200020418     c                   if        alig48 = 0
145300020418     c                   z-add     §4ziva        bhiali
145400020418     c                   else
145500020418     c                   z-add     alig48        bhiali
145600020418     c                   end
145700020418     c     imponibile    MULT      bhiali        WTOTIV           17 5
145800020418     C     WTOTIV        DIV(H)    100           impiva
145900020417     c                   endif
146000020417     c*
146100020417     c                   endsr
146200020417     c*-----------------------------------------
146300020417     c     competenza    begsr
146400020417     c*-----------------------------------------
146500000426      * scrittura righe registrazione anno precedente
146600020409     c                   z-add     bhmsubnum     savsubnum
146700020409     c                   move      bhmsubrig     savsubrig
146800020409     c                   z-add     riga2         bhmnrasreg
146900020409     c                   z-add     1             bhmsubrig
147000020409     c                   add       1             subnum
147100020409     c                   z-add     subnum        bhmsubnum
147200980916     c                   move      dtcoan        bhmdtreg
147300020409     c                   z-add     f50npr        bhmnrreg
147400020527     c* Imposto le causali contabili relative alla NON autofatturazione
147500020527     c                   if        f50dft <> 0
147600020527     c                   move      §4zcrr        bhmcaustes
147700020527     c                   else
147800020527     c                   move      §4zcrra       bhmcaustes
147900020527     c                   end
148000020418     c                   moveL     'A'           bhmfungen
148100020418     c                   clear                   bhmserreg
148200020418     c                   clear                   bhmdtdoc
148300980916     c                   clear                   bhmnrdoc
148400980916     c                   clear                   bhmdtpar
148500980916     c                   clear                   bhmnrpar
148600031204     c                   move      segnofor      bhmsegno
148700030204     c                   z-add     totimpt       bhmimporto
148800020417     c                   z-add     bhmimporto    bhmimportd
148900980916     c                   write     ndbhm000
149000980916      *
149100030204     c                   do        10            x
149200030204     c                   if        tipf(x) = *blanks
149300030204     c                   leave
149400030204     c                   end
149500030204     c                   if        impt(x) = 0
149600030204     c                   iter
149700030204     c                   end
149800030204     c* per ogni tipologia servizio aggancio la tabella Y4Z
149900030204     c* solo se sono stati accorpati i servizi altrimenti l'ho già letta
150000030204     c* inizialmente
150100030204     c                   if        f50tsr = 'S' and tipf(x) <> v1tips
150200030206     c                   eval      v1tip = tipf(x)
150300030204     c                   exsr      sry4z
150400030204     c                   movel     tipf(x)       v1tips
150500030204     c                   end
150600020408     c                   move      §4zkcc        bhmkcc
150700020408     c                   move      §4zksc        bhmksc
150800031204     c                   if        segnofor = 'A'
150900980916     c                   move      'D'           bhmsegno
151000031204     c                   else
151100031204     c                   move      'A'           bhmsegno
151200031204     c                   end
151300980916     c                   add       1             bhmsubrig
151400030204     c                   z-add     impt(x)       bhmimporto
151500030204     c                   z-add     bhmimporto    bhmimportd
151600980916     c                   write     ndbhm000
151700030204     c* analitica
151800030204     c                   exsr      writebha
151900030204     c                   enddo
152000980916     c                   endsr
152100951011?     *--------------------------------------------------------------*
152200031121?     *  AGGOtt: Aggiorno prestaz.occasionali con numero e data fatt.
152300020411      *          e numero assoluto
152400951011?     *--------------------------------------------------------------*
152500031121     C     aggott        BEGSR
152600031121     c                   open      fiott06l
152700170123     c                   open      fiott00f
152800020411     c* se trovata registrazione
152900031121     C     Kott          setll     fiott06L
153000020411     c                   do        *hival
153100031121     C     Kott          reade     fiott06L
153200031121     C                   if        %eof(fiott06l)
153300020411     c                   leave
153400020411     c                   end
153500020418     c* data conteggio compresa nei limiti richiesti
153600031121     c                   if        ottddc > f50dtf or ottddc < f50dti
153700020418     c                   iter
153800020418     c                   end
153900020418     c* se già fatturato iter
154000031121     c                   if        ottnff <> 0
154100020418     c                   iter
154200020418     c                   end
154300170124      * se il record è allocato mando messaggio info all'utente che alloca il rcd
154400170202     c                   do        *Hival
154500170202     c     ottnrr        chain(e)  fiott00f
154600170124     c                   if        %Error
154700170124     c                   clear                   trul82ds
154800170124     c                   eval      ul82§rrn = ottnrr
154900170124     c                   eval      ul82§fil = 'FIOTT00F'
155000170124     c                   eval      ul82§win = 'S'
155100170124     c                   eval      ul82§f7  = 'S'
155200170124     c                   eval      ul82§num = 2
155300170124     c                   eval      ul82§att = 2
155400170124     c                   eval      ul82§mss = 'Si sta bloccando la << Contabili-
155500170124     c                             zzazione >> ' +
155600170124     c                             ' SI PREGA DI USCIRE dal lavoro!'
155700170124     c                   Eval      UL82§msw = 'ATTENZIONE La Contabilizzazione -
155800170124     c                             è BLOCCATA F12 x Riprovare'
155900170124      * chiamo il pgm che manda il messaggio info all'utente
156000170124     c                   call(e)   'TRUL82R'
156100170124     c                   parm                    trul82ds
156200170124      *  Deve sempre rileggere il record  sia x provare se ancora allocato
156300170124      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
156400170124      *  senza lettura del record.
156500170202     c                   else
156600170202     c*  deve proseguire
156700170202     c                   leave
156800170124     c                   end
156900170124     c                   enddo
157000031121     C                   move      savdtdoc      ottDFT
157100031121     C                   Z-ADD     savnrdoc      ottnff
157200031121     C                   Z-ADD     savdcn        ottdcn
157300031121     c                   z-add     riga1         ottnra
157400031121     c                   move      ottddc        dataiso
157500031121     c                   extrct    dataiso:*m    mm
157600031121     c                   move      mm            com02             2
157700031121     c                   if        com02 > meseprt
157800031121     C                   move      dtcoan        dataiso
157900031121     C                   move      dataiso       ottdtr
158000031121     c                   else
158100031121     C                   z-add     f50dpr        ottdtr
158200031121     c                   end
158300041111     c                   if        riga1 <> 0
158400031121     C                   z-add     f50npr        ottnrg
158500041111     c                   else
158600041111     C                   clear                   ottnrg
158700041111     c                   end
158800170123     C                   update    fiott00
158900020411     C                   enddo
159000020411     c*
159100030204     C                   if        riga2 <> 0 and riga1 <> 0
159200020418      * Se ho effettuato le registrazioni impostando la compet. 13
159300020418      * aggancio Ndreg e imposto il numero assoluto di registraz.
159400020418     C                   CALL      'YNRASCOL'
159500020418     C                   PARM                    KSYS
159600020418     C                   PARM                    riga2
159700020418     C                   PARM                    riga1
159800020418     C                   PARM      *off          CMT               1
159900020418     C                   PARM      ' '           ESE               1
160000020418     C                   end
160100170123     c                   close     fiott06l
160200170123     c                   close     fiott00f
160300000204      *
160400951011     C                   ENDSR
160500031121?     *--------------------------------------------------------------*
160600031121?     *  AGGftt: Aggiorno autotrasportatori con numero e data fatt.
160700031121      *          e numero assoluto
160800031121?     *--------------------------------------------------------------*
160900031121     C     AGGftt        BEGSR
161000031121     c                   open      fiftt06l
161100170123     c                   open      fiftt00f
161200031121     c*                  setoff                                       4041
161300031121     c* se trovata registrazione
161400031121     C     Kftt          setll     fiftt06L
161500031121     c                   do        *hival
161600031121     C     Kftt          reade     fiftt06L
161700031121     C                   if        %eof(fiftt06l)
161800031121     c                   leave
161900031121     c                   end
162000031121     c* data conteggio compresa nei limiti richiesti
162100031121     c                   if        fttddc > f50dtf or fttddc < f50dti
162200031121     c                   iter
162300031121     c                   end
162400031121     c* se già fatturato iter
162500031121     c                   if        fttnff <> 0
162600031121     c                   iter
162700031121     c                   end
162800170124      * se il record è allocato mando messaggio info all'utente che alloca il rcd
162900170202     c                   do        *hival
163000170202     c     fttnrr        chain(e)  fiftt00f
163100170124     c                   if        %Error
163200170124     c                   clear                   trul82ds
163300170124     c                   eval      ul82§rrn = fttnrr
163400170124     c                   eval      ul82§fil = 'FIFTT00F'
163500170124     c                   eval      ul82§win = 'S'
163600170124     c                   eval      ul82§f7  = 'S'
163700170124     c                   eval      ul82§num = 2
163800170124     c                   eval      ul82§att = 2
163900170124     c                   eval      ul82§mss = 'Si sta bloccando la << Contabili-
164000170124     c                             zzazione >> ' +
164100170124     c                             ' SI PREGA DI USCIRE dal lavoro!'
164200170124     c                   Eval      UL82§msw = 'ATTENZIONE La Contabilizzazione -
164300170124     c                             è BLOCCATA F12 x Riprovare'
164400170124      * chiamo il pgm che manda il messaggio info all'utente
164500170124     c                   call(e)   'TRUL82R'
164600170124     c                   parm                    trul82ds
164700170124      *  Deve sempre rileggere il record  sia x provare se ancora allocato
164800170124      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
164900170124      *  senza lettura del record.
165000170202     c                   else
165100170202     c*  deve proseguire
165200170202     c                   leave
165300170124     c                   end
165400170124     c                   enddo
165500170124     c*
165600031121     C                   move      savdtdoc      fttDFT
165700031121     C                   Z-ADD     savnrdoc      fttnff
165800031121     C                   Z-ADD     savdcn        fttdcn
165900031121     c                   if        riga1 <> 0
166000031121     c                   movel     fttflr        dftt01
166100031121     c                   z-add     riga1         §fttnrg
166200031121     C                   movel(p)  dftt01        fttflr
166300031121     c                   end
166400170123     C                   update    fiftt00
166500031121     C                   enddo
166600031121     c*
166700031121     C                   if        riga2 <> 0 and riga1 <> 0
166800031121      * Se ho effettuato le registrazioni impostando la compet. 13
166900031121      * aggancio Ndreg e imposto il numero assoluto di registraz.
167000031121     C                   CALL      'YNRASCOL'
167100031121     C                   PARM                    KSYS
167200031121     C                   PARM                    riga2
167300031121     C                   PARM                    riga1
167400031121     C                   PARM      *off          CMT               1
167500031121     C                   PARM      ' '           ESE               1
167600031121     C                   end
167700031121     c                   close     fiftt06l
167800170123     c                   close     fiftt00f
167900031121      *
168000031121     C                   ENDSR
168100030128?     *--------------------------------------------------------------*
168200160108?     *  AGGsre: Aggiorno rettifiche  con numero e data fatt.
168300030128      *          e numero assoluto
168400030128?     *--------------------------------------------------------------*
168500160108     C     AGGsre        BEGSR
168600160108     c* se trovata registrazione
168700160108      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
168800160108     c                   if        f50tsr = 'S'
168900160108     c                               and v1tipf <>'P'
169000160108     C     Ksre          setll     tnsre01L
169100160108     c                   else
169200160108     C     Ksre1         setll     tnsre01L
169300160108     c                   end
169400160108     c                   do        *hival
169500160108     c                   if        f50tsr = 'S'
169600160108     c                               and v1tipf <>'P'
169700160108     C     Ksre          reade     tnsre01L
169800160108     c                   else
169900160108     C     Ksre1         reade     tnsre01L
170000160108     c                   end
170100160108     C                   if        %eof(tnsre01l)
170200160108     c                   leave
170300160108     c                   end
170400160108     c* data conteggio compresa nei limiti richiesti
170500160108     c                   if        sreddc > f50dtf or sreddc < f50dti
170600160108     c                   iter
170700160108     c                   end
170800160108     c* se già fatturato iter
170900160108     c                   if        srenff <> 0
171000160108     c                   iter
171100160108     c                   end
171200160108ab   c* Non deve accorpare le Pulizie
171300160108ab   c                   if        f50tsr = 'S'
171400160108ab   c                               and v1tipf <>'P'
171500160108ab   c                               and sreTSR  ='P'
171600160108ab   c                   iter
171700160108ab   c                   end
171800170124      * se il record è allocato mando messaggio info all'utente che alloca il rcd
171900170202     c                   do        *hival
172000170202     c     srenrr        chain(e)  tnsre00f
172100170124     c                   if        %Error
172200170124     c                   clear                   trul82ds
172300170124     c                   eval      ul82§rrn = srenrr
172400170124     c                   eval      ul82§fil = 'TNSRE00F'
172500170124     c                   eval      ul82§win = 'S'
172600170124     c                   eval      ul82§f7  = 'S'
172700170124     c                   eval      ul82§num = 2
172800170124     c                   eval      ul82§att = 2
172900170124     c                   eval      ul82§mss = 'Si sta bloccando la << Contabili-
173000170124     c                             zzazione >> ' +
173100170124     c                             ' SI PREGA DI USCIRE dal lavoro!'
173200170124     c                   Eval      UL82§msw = 'ATTENZIONE La Contabilizzazione -
173300170124     c                             è BLOCCATA F12 x Riprovare'
173400170124      * chiamo il pgm che manda il messaggio info all'utente
173500170124     c                   call(e)   'TRUL82R'
173600170124     c                   parm                    trul82ds
173700170124      *  Deve sempre rileggere il record  sia x provare se ancora allocato
173800170124      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
173900170124      *  senza lettura del record.
174000170202     c                   else
174100170202     c*  deve proseguire
174200170202     c                   leave
174300170124     c                   end
174400170124     c                   enddo
174500160108     C                   move      savdtdoc      sreDFT
174600160108     C                   Z-ADD     savnrdoc      srenff
174700160108     C                   Z-ADD     savdcn        sredcn
174800160108     c                   z-add     riga1         srenra
174900160108     c                   move      sreddc        dataiso
175000160108     c                   extrct    dataiso:*m    mm
175100160108     c                   move      mm            com02             2
175200160108     c                   if        com02 > meseprt
175300160108     C                   move      dtcoan        dataiso
175400160108     C                   move      dataiso       sredtr
175500160108     c                   else
175600160108     C                   z-add     f50dpr        sredtr
175700160108     c                   end
175800160108     c                   if        riga1 <> 0
175900160108     C                   z-add     f50npr        srenrg
176000160108     C                   else
176100160108     C                   clear                   srenrg
176200160108     c                   end
176300170123     C                   update    tnsre00
176400160108     C                   enddo
176500160108     c*
176600160108     C                   if        riga2 <> 0 and riga1 <> 0
176700160108      * Se ho effettuato le registrazioni impostando la compet. 13
176800160108      * aggancio Ndreg e imposto il numero assoluto di registraz.
176900160108     C                   CALL      'YNRASCOL'
177000160108     C                   PARM                    KSYS
177100160108     C                   PARM                    riga2
177200160108     C                   PARM                    riga1
177300160108     C                   PARM      *off          CMT               1
177400160108     C                   PARM      ' '           ESE               1
177500160108     C                   end
177600160108      *
177700160108     C                   ENDSR
177800160108?     *--------------------------------------------------------------*
177900160108?     *  AGGctt: Aggiorno cooperative con numero e data fatt.
178000160108      *          e numero assoluto
178100160108?     *--------------------------------------------------------------*
178200160108     C     AGGctt        BEGSR
178300030704     C                   IF        NOT %OPEN(FICTT06L)
178400030704     C                   OPEN      FICTT06L
178500170123     C                   OPEN      FICTT00f
178600030704     C                   END
178700030620     c                   move      'C'           cttfvl
178800030128     c* se trovata registrazione
178900150109      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
179000030128     c                   if        f50tsr = 'S'
179100150109     c                               and v1tipf <>'P'
179200030128     C     Kctt          setll     fictt06L
179300030128     c                   else
179400030128     C     Kctt1         setll     fictt06L
179500030128     c                   end
179600030128     c                   do        *hival
179700030128     c                   if        f50tsr = 'S'
179800150109     c                               and v1tipf <>'P'
179900030128     C     Kctt          reade     fictt06L
180000030128     c                   else
180100030128     C     Kctt1         reade     fictt06L
180200030128     c                   end
180300030128     C                   if        %eof(fictt06l)
180400030128     c                   leave
180500030128     c                   end
180600030128     c* data conteggio compresa nei limiti richiesti
180700030128     c                   if        cttddc > f50dtf or cttddc < f50dti
180800030128     c                   iter
180900030128     c                   end
181000030128     c* se già fatturato iter
181100030128     c                   if        cttnff <> 0
181200030128     c                   iter
181300030128     c                   end
181400150204ab   c* Non deve accorpare le Pulizie
181500150204ab   c                   if        f50tsr = 'S'
181600150204ab   c                               and v1tipf <>'P'
181700150204ab   c                               and cttTSR  ='P'
181800150204ab   c                   iter
181900150204ab   c                   end
182000170124      * se il record è allocato mando messaggio info all'utente che alloca il rcd
182100170202     c                   do        *Hival
182200170309     c     cttnrr        chain(e)  fictt00f
182300170124     c                   if        %Error
182400170124     c                   clear                   trul82ds
182500170124     c                   eval      ul82§rrn = cttnrr
182600170124     c                   eval      ul82§fil = 'FICTT00F'
182700170124     c                   eval      ul82§win = 'S'
182800170124     c                   eval      ul82§f7  = 'S'
182900170124     c                   eval      ul82§num = 2
183000170124     c                   eval      ul82§att = 2
183100170124     c                   eval      ul82§mss = 'Si sta bloccando la << Contabili-
183200170124     c                             zzazione >> ' +
183300170124     c                             ' SI PREGA DI USCIRE dal lavoro!'
183400170124     c                   Eval      UL82§msw = 'ATTENZIONE La Contabilizzazione -
183500170124     c                             è BLOCCATA F12 x Riprovare'
183600170124      * chiamo il pgm che manda il messaggio info all'utente
183700170124     c                   call(e)   'TRUL82R'
183800170124     c                   parm                    trul82ds
183900170124      *  Deve sempre rileggere il record  sia x provare se ancora allocato
184000170124      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
184100170124      *  senza lettura del record.
184200170202     c                   else
184300170202     c*  deve proseguire
184400170202     c                   leave
184500170124     c                   end
184600170124     c                   enddo
184700030128     C                   move      savdtdoc      cttDFT
184800030128     C                   Z-ADD     savnrdoc      cttnff
184900030128     C                   Z-ADD     savdcn        cttdcn
185000030128     c                   z-add     riga1         cttnra
185100030204     c                   move      cttddc        dataiso
185200030204     c                   extrct    dataiso:*m    mm
185300030204     c                   move      mm            com02             2
185400030204     c                   if        com02 > meseprt
185500030211     C                   move      dtcoan        dataiso
185600030211     C                   move      dataiso       cttdtr
185700030128     c                   else
185800030128     C                   z-add     f50dpr        cttdtr
185900030128     c                   end
186000041111     c                   if        riga1 <> 0
186100030128     C                   z-add     f50npr        cttnrg
186200041111     C                   else
186300041111     C                   clear                   cttnrg
186400041111     c                   end
186500170123     C                   update    fictt00
186600030128     C                   enddo
186700030128     c*
186800030204     C                   if        riga2 <> 0 and riga1 <> 0
186900030128      * Se ho effettuato le registrazioni impostando la compet. 13
187000030128      * aggancio Ndreg e imposto il numero assoluto di registraz.
187100030128     C                   CALL      'YNRASCOL'
187200030128     C                   PARM                    KSYS
187300030128     C                   PARM                    riga2
187400030128     C                   PARM                    riga1
187500030128     C                   PARM      *off          CMT               1
187600030128     C                   PARM      ' '           ESE               1
187700030128     C                   end
187800030704     C                   IF        %OPEN(FICTT06L)
187900030128     c                   close     fictt06l
188000170123     c                   close     fictt00f
188100030704     C                   END
188200030128      *
188300030128     C                   ENDSR
188400030620?     *--------------------------------------------------------------*
188500030620?     *  ctrctt: Controllo se ho dei conteggi non confermati
188600030620      *          intal caso segnalo l'anomalia
188700030620?     *--------------------------------------------------------------*
188800030620     C     ctrctt        BEGSR
188900030620     c                   move      ' '           cttfvl
189000030620     c                   eval      noconf = ' '
189100030620     c* se trovata registrazione
189200150109      ** se accorpate e non è un record PUILIZIE (devono essere contabilizzate a parte)
189300030620     c                   if        f50tsr = 'S'
189400150109     c                               and v1tipf <>'P'
189500030620     C     Kctt          setll     fictt06L
189600030620     c                   else
189700030620     C     Kctt1         setll     fictt06L
189800030620     c                   end
189900030620     c                   do        *hival
190000030620     c                   if        f50tsr = 'S'
190100150109     c                               and v1tipf <>'P'
190200030620     C     Kctt          reade     fictt06L
190300030620     c                   else
190400030620     C     Kctt1         reade     fictt06L
190500030620     c                   end
190600030620     C                   if        %eof(fictt06l)
190700030620     c                   leave
190800030620     c                   end
190900030620     c* data conteggio compresa nei limiti richiesti
191000030620     c                   if        cttddc > f50dtf or cttddc < f50dti
191100030620     c                   iter
191200030620     c                   end
191300030620     c* se già fatturato iter
191400030620     c                   if        cttnff <> 0
191500030620     c                   iter
191600030620     c                   end
191700150204ab   c* Non deve accorpare le Pulizie
191800150204ab   c                   if        f50tsr = 'S'
191900150204ab   c                               and v1tipf <>'P'
192000150204ab   c                               and cttTSR  ='P'
192100150204ab   c                   iter
192200150204ab   c                   end
192300150204      *
192400030620     c                   eval      noconf = '1'
192500030827     c                   leave
192600030620     C                   enddo
192700030620      *
192800030620     C                   ENDSR
192900030827?     *--------------------------------------------------------------*
193000030827?     *  AGGatt: Aggiorno aff/defl. con numero e data fatt.
193100030827      *          e numero assoluto
193200030827?     *--------------------------------------------------------------*
193300030827     C     AGGatt        BEGSR
193400030827     C                   IF        NOT %OPEN(FIaTT06L)
193500030827     C                   OPEN      FIaTT06L
193600170123     C                   OPEN      FIaTT00f
193700030827     C                   END
193800030828     c                   eval      attflg = 'C'
193900030827     c* se trovata registrazione
194000030827     C     Katt          setll     fiatt06L
194100030827     c                   do        *hival
194200030828     C     Katt          reade     fiatt06L
194300030827     C                   if        %eof(fiatt06l)
194400030827     c                   leave
194500030827     c                   end
194600030827     c* data conteggio compresa nei limiti richiesti
194700030827     c                   if        attddc > f50dtf or attddc < f50dti
194800030827     c                   iter
194900030827     c                   end
195000030827     c* se già fatturato iter
195100030827     c                   if        attnff <> 0
195200030827     c                   iter
195300030827     c                   end
195400170124      * se il record è allocato mando messaggio info all'utente che alloca il rcd
195500170202     c                   do        *Hival
195600170202     c     attnrr        chain(e)  fiatt00f
195700170124     c                   if        %Error
195800170124     c                   clear                   trul82ds
195900170124     c                   eval      ul82§rrn = attnrr
196000170124     c                   eval      ul82§fil = 'FIATT00F'
196100170124     c                   eval      ul82§win = 'S'
196200170124     c                   eval      ul82§f7  = 'S'
196300170124     c                   eval      ul82§num = 2
196400170124     c                   eval      ul82§att = 2
196500170124     c                   eval      ul82§mss = 'Si sta bloccando la << Contabili-
196600170124     c                             zzazione >> ' +
196700170124     c                             ' SI PREGA DI USCIRE dal lavoro!'
196800170124     c                   Eval      UL82§msw = 'ATTENZIONE La Contabilizzazione -
196900170124     c                             è BLOCCATA F12 x Riprovare'
197000170124      * chiamo il pgm che manda il messaggio info all'utente
197100170124     c                   call(e)   'TRUL82R'
197200170124     c                   parm                    trul82ds
197300170124      *  Deve sempre rileggere il record  sia x provare se ancora allocato
197400170124      *  che se liberato dall'altro lavoro andrebbe avanti facendo un aggiornamento
197500170124      *  senza lettura del record.
197600170202     c                   else
197700170202     c*  deve proseguire
197800170202     c                   leave
197900170124     c                   end
198000170124     C                   enddo
198100030827     C                   move      savdtdoc      attDFT
198200030827     C                   Z-ADD     savnrdoc      attnff
198300030827     C                   Z-ADD     savdcn        attdcn
198400030827     c                   z-add     riga1         attnra
198500030827     c                   move      attddc        dataiso
198600030827     c                   extrct    dataiso:*m    mm
198700030827     c                   move      mm            com02             2
198800030827     c                   if        com02 > meseprt
198900030827     C                   move      dtcoan        dataiso
199000030827     C                   move      dataiso       attdtr
199100030827     c                   else
199200030827     C                   z-add     f50dpr        attdtr
199300030827     c                   end
199400041111     c                   if        riga1 <> 0
199500030827     C                   z-add     f50npr        attnrg
199600041111     c                   else
199700041111     c                   clear                   attnrg
199800041111     c                   end
199900170123     C                   update    fiatt00
200000030827     C                   enddo
200100030827     c*
200200030827     C                   if        riga2 <> 0 and riga1 <> 0
200300030827      * Se ho effettuato le registrazioni impostando la compet. 13
200400030827      * aggancio Ndreg e imposto il numero assoluto di registraz.
200500030827     C                   CALL      'YNRASCOL'
200600030827     C                   PARM                    KSYS
200700030827     C                   PARM                    riga2
200800030827     C                   PARM                    riga1
200900030827     C                   PARM      *off          CMT               1
201000030827     C                   PARM      ' '           ESE               1
201100030827     C                   end
201200030827     C                   IF        %OPEN(FIaTT06L)
201300030827     c                   close     fiatt06l
201400170123     c                   close     fiatt00f
201500030827     C                   END
201600030827      *
201700030827     C                   ENDSR
201800030827?     *--------------------------------------------------------------*
201900030827?     *  ctratt: Controllo se ho dei conteggi non confermati
202000030827      *          intal caso segnalo l'anomalia
202100030827?     *--------------------------------------------------------------*
202200030827     C     ctratt        BEGSR
202300030828     c                   eval      attflg = ' '
202400030827     c                   eval      noconf = ' '
202500030827     c* se trovata registrazione
202600030827     C     Katt          setll     fiatt06L
202700030827     c                   do        *hival
202800030827     C     Katt          reade     fiatt06L
202900030827     C                   if        %eof(fiatt06l)
203000030827     c                   leave
203100030827     c                   end
203200030827     c* data conteggio compresa nei limiti richiesti
203300030827     c                   if        attddc > f50dtf or attddc < f50dti
203400030827     c                   iter
203500030827     c                   end
203600030827     c                   eval      noconf = '1'
203700030827     c                   leave
203800030827     C                   enddo
203900030827      *
204000030827     C                   ENDSR
204100980914     c*-----------------------------------------
204200980914     c     primanota     begsr
204300980914     c*-----------------------------------------
204400980914     C* contabilizzazione batch
204500980916     c                   clear                   ndc071ds
204600980914     C                   MOVE      xscsoc        SOCieta071
204700020411     C                   eval      rifint071=bhmrifint
204800980915     c                   move      *loval        dtregda071
204900980915     c                   move      *hival        dtrega071
205000980915     c                   move      *loval        subnumd071
205100980915     c                   move      *hival        subnuma071
205200980914     C                   MOVE      *off          solcont071
205300980916    >C                   MOVE      *off          interat071
205400980916     C                   move      *off          err071
205500980915     c                   move      *off          control071
205600020411     c                   move      *on           unocmt071
205700020411     C                   MOVEL(P)  NDC071DS      kpjbu
205800020411     C*
205900980916     C                   CALL      'NDC071C'
206000980914     C                   parm                    kpjba
206100980914     C                   movel     kpjbu         ndc071ds
206200020411     C* in caso di errore emettere video con segnalazione
206300020411     C                   if        err071=*on
206400020411     C* avviso il pgm chiamante che ci sono stati errori e gli do la possibi
206500020411     C* lità di manutenzione p. n. batch in interattivo
206600020411     c                   reset                   NDC050DS
206700020411     c                   move      xscsoc        societa050
206800020411     C                   eval      rifint050=bhmrifint
206900020411     c                   movel(P)  ndc050ds      kpjbu
207000020411     c     ret050        downe     '2'
207100020411JRN  C                   CALL      'NDC050R'
207200020411JRN  C                   PARM                    kpjba
207300020411     c                   movel(P)  kpjbu         ndc050ds
207400020411     c                   enddo
207500020411-4   C                   EndIf
207600980914     C*
207700980914     c                   endsr
207800980911     C*----------------------------------------------------*
207900980911     C* Reperimento dati società
208000980911     C*----------------------------------------------------*
208100980911     C     REPSOC        BEGSR
208200980911     C*
208300980911     C                   CALLB     'XSOC'
208400980911     C                   PARM                    TIPXSC            6
208500980911     C                   PARM                    SOCXSC            3
208600980911     C                   PARM                    CDSXSC            9 0
208700980911     C                   PARM                    MODXSC            3
208800980911     C                   PARM      *blanks       RTNXSC            1
208900980911     C                   PARM                    XSOCDS
209000980911     C                   PARM                    KPJBA
209100980911     C*
209200980911     C                   ENDSR
209300030206     c*----------------------------------------------------------------
209400030206     c     upsfl2        begsr
209500030206     c*----------------------------------------------------------------
209600030206     c                   do        10            xyz
209700030206     c     xyz           chain     y350s02                            56
209800030206     c                   if        *in56
209900030206     c                   leave
210000030206     c                   else
210100030206     c* seleziono il tipo forn./servizio
210200030206     c                   if        h1tipf <> v1tipf
210300030206     c                   iter
210400030206     c                   end
210500030206     c                   if        h1ccmp = 13
210600030206     c                   iter
210700030206     c                   end
210800080925     c                   add       comsfl2       h1cico
210900030206     c                   update    y350s02
211000030206     c                   leave
211100030206     c                   end
211200030206     c                   enddo
211300030206     c                   endsr
211400951011?     *--------------------------------------------------------------*
211500020527?     *  lettura tabella Y4Z                                         *
211600951011?     *--------------------------------------------------------------*
211700020527     C     sry4z         BEGSR
211800020527     c*  lettura tabella Y4Z
211900020527     c                   clear                   xatbds
212000020527     c                   move      '1'           xtbric
212100020527     c                   move      *blank        xtbkey
212200020527     c                   move      'Y4Z'         xtbcod
212300030206     c                   movel     v1tip         xtbkey
212400020527     c                   call      'XATB'
212500020527     c                   parm                    xatbds
212600020527     c                   if        xtberr = '0'
212700020527     c                   movel     xtbuni        angy4z
212800020527     c                   else
212900020527     c                   clear                   angy4z
213000020527     c                   end
213100031204     C*
213200031204     C                   ENDSR
213300031204?     *--------------------------------------------------------------*
213400031204     c* aggancio la causale contabile per reperire il segno
213500031204?     *--------------------------------------------------------------*
213600031204     C     srope         BEGSR
213700031204     c                   clear                   segnofor
213800031204     c     kope          chain     anope01l
213900031204     c                   if        %found(anope01l) and opetpreg = '1'
214000031204     c                   select
214100031204     c                   when      opetpdoci = '0'
214200031204     c                   eval      segnofor = 'A'
214300031204     c                   when      opetpdoci = '1'
214400031204     c                   eval      segnofor = 'D'
214500031204     c                   endsl
214600031204     c                   end
214700020527     C*
214800020527     C                   ENDSR
214900020527?     *--------------------------------------------------------------*
215000030128?     *  istruz: Operazioni di inizializzazione dati                 *
215100020527?     *--------------------------------------------------------------*
215200030128     C     istruz        BEGSR
215300030827     c                   select
215400031121     c* prestazioni occasionali
215500031121     c                   when      f50cau<> ' '
215600030128     C                   EVAL      WrkSqlCmd
215700030128     C                             =
215800031121     C                             'SELECT ottcdf,'
215900030128     C                             +
216000031121     C                             ' sum(otttim) '
216100030128     C                             +
216200031121     C                             ' from fiott06l where ottsoc = '
216300030128     C                             +
216400030128     C                             '''' + f50soc  + ''''
216500030128     C                             +
216600030128     C                             ' AND '
216700030128     C                             +
216800031121     C                             ' ottcdf between '
216900030128     C                             +
217000030128     C                             '''' + f50pd1 + ''''
217100030128     C                             +
217200030129     C                             ' AND '
217300030128     C                             +
217400030128     C                             '''' + f50pd2 + ''''
217500030128     C                             +
217600030128     C                             ' AND '
217700030128     C                             +
217800031121     C                             ' otttip = '
217900031121     C                             +
218000031121     C                             '''' + f50tip + ''''
218100031121     C                             +
218200031121     C                             ' AND '
218300031121     C                             +
218400031121     C                             ' otttab = '
218500031121     C                             +
218600031121     C                             '''' + f50cau + ''''
218700030128     C                             +
218800030128     C                             ' AND '
218900030128     C                             +
219000031121     C                             ' ottddc BETWEEN '
219100030129     C                             + %editc(f50dti:'X')
219200030129     C                             +
219300030129     C                             ' AND ' + %editc(f50dtf:'X')
219400030128     C                             +
219500030129     C                             ' AND '
219600030128     C                             +
219700031121     C                             'ottnff = 0  group by ottcdf '
219800030203     c                             +
219900031121     C                             ' ORDER BY ottcdf'
220000031121     c* autotrasportatori
220100031121     c                   when      f50tip = 'A'
220200031121     C                   EVAL      WrkSqlCmd
220300031121     C                             =
220400031121     C                             'SELECT fttcdf,'
220500031121     C                             +
220600031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
220700031121     C                             +
220800031121     C                             'fttmnt)'
220900031121     C                             +
221000031121     C                             ' from fiftt06l where fttsoc = '
221100031121     C                             +
221200031121     C                             '''' + f50soc  + ''''
221300031121     C                             +
221400031121     C                             ' AND '
221500031121     C                             +
221600031121     C                             ' fttcdf between '
221700031121     C                             +
221800031121     C                             '''' + f50pd1 + ''''
221900031121     C                             +
222000031121     C                             ' AND '
222100031121     C                             +
222200031121     C                             '''' + f50pd2 + ''''
222300031121     C                             +
222400031121     C                             ' AND '
222500031121     C                             +
222600031121     C                             ' ftttsr = '' '''
222700031121     C                             +
222800031121     C                             ' AND '
222900031121     C                             +
223000031121     C                             ' fttfvl = ''C'''
223100031121     C                             +
223200031121     C                             ' AND '
223300031121     C                             +
223400031121     C                             ' Fttddc BETWEEN '
223500031121     C                             + %editc(f50dti:'X')
223600031121     C                             +
223700031121     C                             ' AND ' + %editc(f50dtf:'X')
223800031121     C                             +
223900031121     C                             ' AND '
224000031121     C                             +
224100031121     C                             'fttnff = 0  group by fttcdf '
224200031121     c                             +
224300031121     C                             ' ORDER BY fttcdf'
224400030128     c* cooperative
224500030827     c                   when      f50tip = 'C'
224600030128     C                   EVAL      WrkSqlCmd
224700030128     C                             =
224800030128     C                             'SELECT cttcdf,'
224900030128     C                             +
225000030205     C                             ' sum(cttitc+cttitma+cttitmp)'
225100150108      * se Tipo serviz.in dettaglio
225200150108      *  oppure se in accorpamento e primo giro per le PULIZIE a parte extra accorpati
225300030128     C                   IF        f50tsr = 'N'
225400150108     c                                or
225500150108     c                             Coop_PULIZIE_a_parte = 'S'
225600030128     C                   EVAL      WrkSqlCmd
225700030128     C                             =
225800030128     C                             %TRIMR(WrkSqlCmd)
225900030128     C                             +
226000030205     C                             ', ctttsr '
226100030128     C                   ENDIF
226200150108      * from
226300030128     C                   EVAL      WrkSqlCmd
226400030128     C                             =
226500030128     C                             %TRIMR(WrkSqlCmd)
226600030128     C                             +
226700030128     C                             ' from fictt06l where cttsoc = '
226800030128     C                             +
226900030128     C                             '''' + f50soc  + ''''
227000030128     C                             +
227100030128     C                             ' AND '
227200030128     C                             +
227300030129     C                             ' cttcdf between '
227400030128     C                             +
227500030128     C                             '''' + f50pd1 + ''''
227600030128     C                             +
227700030129     C                             ' AND '
227800030128     C                             +
227900030128     C                             '''' + f50pd2 + ''''
228000030128     C                             +
228100030128     C                             ' AND '
228200030128     C                             +
228300030128     C                             ' cttfvl = ''C'''
228400030128     C                             +
228500030128     C                             ' AND '
228600030128     C                             +
228700030128     C                             ' cttddc BETWEEN '
228800030129     C                             + %editc(f50dti:'X')
228900030129     C                             +
229000030129     C                             ' AND ' + %editc(f50dtf:'X')
229100030128     C                             +
229200030128     C                             ' AND'
229300030128     C                             +
229400150108     C                             ' cttnff = 0 '
229500150108      *
229600150108      *  se Accorpato e Pulizie a parte (1°giro)
229700150108      *   filtra SOLO le PULIZIE
229800150108     c                   IF        Coop_PULIZIE_a_parte = 'S'
229900150108     C                   EVAL      WrkSqlCmd
230000150108     C                             =
230100150108     C                             %TRIMR(WrkSqlCmd)
230200150108     C                             +
230300150108     C                             ' AND ctttsr = ''P''  '
230400150108     C                   ENDIF
230500150108      *
230600150108      *  se Accorpato e accorpa (2°giro) a parte
230700150108     c                   IF        Coop_PULIZIE_a_parte = 'N'
230800150108     C                   EVAL      WrkSqlCmd
230900150108     C                             =
231000150108     C                             %TRIMR(WrkSqlCmd)
231100150108     C                             +
231200150108     C                             ' AND ctttsr <> ''P''  '
231300150108     C                   ENDIF
231400150108      *
231500150108      *
231600150108      * Group By
231700150108     C                   EVAL      WrkSqlCmd
231800150108     C                             =
231900150108     C                             %TRIMR(WrkSqlCmd)
232000150108     C                             +
232100150108     C                             ' group by cttcdf '
232200150108      *
232300150108      *  se Accorpato e Pulizie a parte (1°giro)
232400030128     C                   IF        f50tsr = 'N'
232500150108     c                                or
232600150108     c                             Coop_PULIZIE_a_parte = 'S'
232700030128     C                   EVAL      WrkSqlCmd
232800030128     C                             =
232900030128     C                             %TRIMR(WrkSqlCmd)
233000030128     C                             +
233100030203     C                             ', ctttsr'
233200030128     C                   ENDIF
233300150108      * Order By
233400030128     C                   EVAL      WrkSqlCmd
233500030128     C                             =
233600030128     C                             %TRIMR(WrkSqlCmd)
233700030128     C                             +
233800030203     C                             ' ORDER BY cttcdf '
233900150108      *
234000150108      *  se Accorpato e Pulizie a parte (1°giro)
234100030128     C                   IF        f50tsr = 'N'
234200150108     c                                or
234300150108     c                             Coop_PULIZIE_a_parte = 'S'
234400030128     C                   EVAL      WrkSqlCmd
234500030128     C                             =
234600030128     C                             %TRIMR(WrkSqlCmd)
234700030128     C                             +
234800030203     C                             ', ctttsr '
234900030128     C                   ENDIF
235000030827     c* aff/defl.
235100030827     c                   when      f50tip = 'D'
235200030827     C                   EVAL      WrkSqlCmd
235300030827     C                             =
235400030827     C                             'SELECT attcdf,'
235500030827     C                             +
235600030827     C                             ' sum(attimpp)'
235700030827     C                             +
235800030827     C                             ' from fiatt06l where attsoc = '
235900030827     C                             +
236000030827     C                             '''' + f50soc  + ''''
236100030827     C                             +
236200030827     C                             ' AND '
236300030827     C                             +
236400030827     C                             ' attcdf between '
236500030827     C                             +
236600030827     C                             '''' + f50pd1 + ''''
236700030827     C                             +
236800030827     C                             ' AND '
236900030827     C                             +
237000030827     C                             '''' + f50pd2 + ''''
237100030827     C                             +
237200030827     C                             ' AND '
237300030827     C                             +
237400030827     C                             ' attdco <> 0'
237500030827     C                             +
237600030827     C                             ' AND '
237700030827     C                             +
237800030827     C                             ' attddc BETWEEN '
237900030827     C                             + %editc(f50dti:'X')
238000030827     C                             +
238100030827     C                             ' AND ' + %editc(f50dtf:'X')
238200030827     C                             +
238300030827     C                             ' AND '
238400030827     C                             +
238500030827     C                             'attnff = 0  group by attcdf '
238600030827     c                             +
238700030827     C                             ' ORDER BY attcdf'
238800030827     c                   endsl
238900030128     C                   ENDSR
239000030204?     *--------------------------------------------------------------*
239100030204?     *  srsfl2: CARICAMENNTO SUBFILE 2                              *
239200030204?     *--------------------------------------------------------------*
239300030204     C     srsfl2        BEGSR
239400030204      *
239500030204     c                   exsr      istruz2
239600030204      *  Pulisco dati SFL
239700030204     C                   SETOFF                                       24
239800030204     C                   WRITE     y350C02
239900030204     C                   SETON                                        24
240000030204     C                   Z-ADD     0             NRR2              6 0
240100030204     c                   clear                   tredici           1
240200030204     c                   clear                   tipf
240300030204     c                   clear                   impt
240400030204     c                   clear                   impf
240500030204     c                   clear                   totimpt
240600030206     c                   clear                   totimpf
240700030206     c                   clear                   totimp
240800030206     c                   clear                   diffe
240900030204     c* questo sql mi restituisce per ogni fornitore, mese di competenza,
241000030204     c* tipo servizio la sommatoria dei conteggi
241100030204     c*
241200030204     c* Il mese di competenza sarà = 13 se il conteggio è dell'anno
241300030204     c* precedente ed è possibile contabilizzare ancora i ratei,
241400030204     c* altrimenti se il conteggio è dell'anno precedente
241500030204     c* ma non si possono più registrare ratei sarà forzato a 1
241600030204     c* Per i conteggi dell'anno in corso il mese di competenza è
241700030204     c* quello del conteggio
241800030204     c*
241900030827     c* Lettura fiftt00f/fictt00f/fiatt00f
242000030204     C/EXEC SQL
242100030204     C+ PREPARE S2 FROM :WrkSqlCmd
242200030204     C/END-EXEC
242300030204
242400030204     C/EXEC SQL
242500030204     C+ DECLARE A2 CURSOR FOR S2
242600030204     C/END-EXEC
242700030204
242800030204     C/EXEC SQL
242900030204     C+ OPEN A2
243000030204     C/END-EXEC
243100030204
243200030204     C                   DOU       SqlCod <> 0
243300030827     c                   select
243400031121     c* prestazioni occasionali
243500031121     c                   when      f50cau<> ' '
243600030204     C/EXEC SQL
243700031121     C+ FETCH NEXT FROM A2 INTO :fiott2
243800030204     C/END-EXEC
243900031121     c* autotrasportatori
244000031121     c                   when      f50tip = 'A'
244100031121     C/EXEC SQL
244200031121     C+ FETCH NEXT FROM A2 INTO :fiftt2
244300031121     C/END-EXEC
244400030827     c                   when      f50tip = 'C'
244500030204     c* cooperative
244600030204     C/EXEC SQL
244700030204     C+ FETCH NEXT FROM A2 INTO :fictt2
244800030204     C/END-EXEC
244900030827     c                   when      f50tip = 'D'
245000030827     c* aff/defl.
245100030827     C/EXEC SQL
245200030827     C+ FETCH NEXT FROM A2 INTO :fiatt2
245300030827     C/END-EXEC
245400030827     c                   endsl
245500030204     c*
245600030204     C                   SELECT
245700030204     C                   WHEN      SqlCod = 0
245800030204     C                   EXSR      CARTRN2
245900030204     C                   WHEN      SqlCod = 100
246000030204     C                   WHEN      SqlCod <> 0
246100030204      * Forzo la stampa del JOBLOG.
246200030204     C                   CALL      'X66CHGJOB'
246300030204     C                   ENDSL
246400030204     C                   ENDDO
246500030204     C/EXEC SQL
246600030204     C+ CLOSE A2
246700030204     C/END-EXEC
246800030204     c* rileggo il sfl e mi memorizzo la sommatoria per ogni tipo servizio
246900030204     c* tenendo separati gli importi dell'anno in corso con quelli dell'anno
247000030204     c* precedente
247100030204     c                   clear                   x
247200030205     c                   do        10            xyz
247300030204     c     xyz           chain     y350s02                            56
247400030205     c                   if        *in56
247500030205     c                   leave
247600030205     c                   else
247700030204     c                   z-add     1             xy
247800030204     c     h1tipf        lookup    tipf(xy)                               55
247900030204     c                   if        *in55
248000030204     c                   if        h1ccmp = 13
248100030204     c                   add       h1cico        impt(xy)
248200030204     c                   else
248300030204     c                   add       h1cico        impf(xy)
248400030204     c                   end
248500030204     c                   else
248600030204     c                   add       1             x
248700030204     c                   movel     h1tipf        tipf(x)
248800030204     c                   if        h1ccmp = 13
248900030204     c                   z-add     h1cico        impt(x)
249000030204     c                   else
249100030205     c                   z-add     h1cico        impf(x)
249200030204     c                   end
249300030204     c                   end
249400030204     c                   end
249500030204     c                   enddo
249600030204     C*
249700030204     C                   ENDSR
249800030204?     *--------------------------------------------------------------*
249900030204?     *  istruz2 Operazioni di inizializzazione istruzione sql       *
250000030204?     *--------------------------------------------------------------*
250100030204     C     istruz2       BEGSR
250200030827     c                   select
250300031121     c* prestazioni occasionali
250400031121     c                   when      f50cau<> ' '
250500030204     C                   EVAL      WrkSqlCmd
250600030204     C                             =
250700030204     C                             'SELECT '
250800030204     C                             +
250900031121     C                             ' sum(otttim), '
251000030204     C                             +
251100031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
251200030204     C                             +
251300030204     C                             '''' + meseprt + ''''
251400030204     C                             +
251500030204     C                             ' then '
251600030204     C                             +
251700030204     C                             '''' + commes + ''''
251800030204     C                             +
251900030204     C                             ' else'
252000030204     C                             +
252100031121     C                             ' substr(digits(ottddc), 5, 2) end as pippo '
252200030204     C                             +
252300031121     C                             ' from fiott06l where ottsoc = '
252400030204     C                             +
252500030204     C                             '''' + f50soc  + ''''
252600030204     C                             +
252700030204     C                             ' AND '
252800030204     C                             +
252900031121     C                             ' ottcdf = '
253000030204     C                             +
253100030204     C                             '''' + v1cfor + ''''
253200030204     C                             +
253300030204     C                             ' AND '
253400030204     C                             +
253500031121     C                             ' otttip = '
253600031121     C                             +
253700031121     C                             '''' + f50tip + ''''
253800031121     C                             +
253900031121     C                             ' AND '
254000031121     C                             +
254100031121     C                             ' otttab = '
254200031121     C                             +
254300031121     C                             '''' + f50cau + ''''
254400030204     C                             +
254500030204     C                             ' AND '
254600030204     C                             +
254700031121     C                             ' ottddc BETWEEN '
254800030204     C                             + %editc(f50dti:'X')
254900030204     C                             +
255000030204     C                             ' AND ' + %editc(f50dtf:'X')
255100030204     C                             +
255200030204     C                             ' AND '
255300030204     C                             +
255400031121     C                             'ottnff = 0  group by '
255500030204     C                             +
255600031121     C                             ' case when substr(digits(ottddc), 5, 2) > '
255700030204     C                             +
255800030204     C                             '''' + meseprt + ''''
255900030204     C                             +
256000030204     C                             ' then '
256100030204     C                             +
256200030204     C                             '''' + commes + ''''
256300030204     C                             +
256400030204     C                             ' else'
256500030204     C                             +
256600031121     C                             ' substr(digits(ottddc), 5, 2) end '
256700030204     C                             +
256800030204     C                             ' ORDER BY pippo'
256900031121     c* autotrasportatori
257000031121     c                   when      f50tip = 'A'
257100031121     C                   EVAL      WrkSqlCmd
257200031121     C                             =
257300031121     C                             'SELECT '
257400031121     C                             +
257500031121     C                             ' sum(fttitt+fttita+ftttpe+ftttbn+ftttim+'
257600031121     C                             +
257700031121     C                             'fttmnt),'
257800031121     C                             +
257900031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
258000031121     C                             +
258100031121     C                             '''' + meseprt + ''''
258200031121     C                             +
258300031121     C                             ' then '
258400031121     C                             +
258500031121     C                             '''' + commes + ''''
258600031121     C                             +
258700031121     C                             ' else'
258800031121     C                             +
258900031121     C                             ' substr(digits(fttddc), 5, 2) end as pippo '
259000031121     C                             +
259100031121     C                             ' from fiftt06l where fttsoc = '
259200031121     C                             +
259300031121     C                             '''' + f50soc  + ''''
259400031121     C                             +
259500031121     C                             ' AND '
259600031121     C                             +
259700031121     C                             ' fttcdf = '
259800031121     C                             +
259900031121     C                             '''' + v1cfor + ''''
260000031121     C                             +
260100031121     C                             ' AND '
260200031121     C                             +
260300031121     C                             ' ftttsr = '' '''
260400031121     C                             +
260500031121     C                             ' AND '
260600031121     C                             +
260700031121     C                             ' fttfvl = ''C'''
260800031121     C                             +
260900031121     C                             ' AND '
261000031121     C                             +
261100031121     C                             ' Fttddc BETWEEN '
261200031121     C                             + %editc(f50dti:'X')
261300031121     C                             +
261400031121     C                             ' AND ' + %editc(f50dtf:'X')
261500031121     C                             +
261600031121     C                             ' AND '
261700031121     C                             +
261800031121     C                             'fttnff = 0  group by '
261900031121     C                             +
262000031121     C                             ' case when substr(digits(fttddc), 5, 2) > '
262100031121     C                             +
262200031121     C                             '''' + meseprt + ''''
262300031121     C                             +
262400031121     C                             ' then '
262500031121     C                             +
262600031121     C                             '''' + commes + ''''
262700031121     C                             +
262800031121     C                             ' else'
262900031121     C                             +
263000031121     C                             ' substr(digits(fttddc), 5, 2) end '
263100031121     C                             +
263200031121     C                             ' ORDER BY pippo'
263300030204     c* cooperative
263400030827     c                   when      f50tip = 'C'
263500030204     C                   EVAL      WrkSqlCmd
263600030204     C                             =
263700030204     C                             'SELECT ctttsr,'
263800030204     C                             +
263900030204     C                             ' sum(cttitc+cttitma+cttitmp),'
264000030204     C                             +
264100030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
264200030204     C                             +
264300030204     C                             '''' + meseprt + ''''
264400030204     C                             +
264500030204     C                             ' then '
264600030204     C                             +
264700030204     C                             '''' + commes + ''''
264800030204     C                             +
264900030204     C                             ' else'
265000030204     C                             +
265100030204     C                             ' substr(digits(cttddc), 5, 2) end as pippo '
265200030204     C                             +
265300030204     C                             ' from fictt06l where cttsoc = '
265400030204     C                             +
265500030204     C                             '''' + f50soc  + ''''
265600150108      * O sono NON accorpate
265700150108      *  oppure
265800150108      * ACCORPATE e si tratta delle PULIZIE
265900030204     C                   IF        f50tsr = 'N'
266000150108     c                                 or
266100150108     c                             v1tipf = 'P' and Coop_PULIZIE_a_Parte <>' '
266200030204     C                   EVAL      WrkSqlCmd
266300030204     C                             =
266400030204     C                             %TRIMR(WrkSqlCmd)
266500030206     C                             +
266600030206     C                             ' AND '
266700030204     C                             +
266800030204     C                             ' ctttsr = '
266900030204     C                             +
267000030204     C                             '''' + v1tipf + ''''
267100150108      *  Attenzione le accorpate
267200150108      *    sono escluse le PULIZIE
267300150108     C                   ElsE
267400150108      * quindi per le accorpate: TUTTE tranne le "P"=PULIZIE
267500150108     C                   EVAL      WrkSqlCmd
267600150108     C                             =
267700150108     C                             %TRIMR(WrkSqlCmd)
267800150108     C                             +
267900150108     C                             ' AND '
268000150108     C                             +
268100150108     C                             ' ctttsr <> ''P'' '
268200030204     C                   ENDIF
268300030204     C                   EVAL      WrkSqlCmd
268400030204     C                             =
268500030204     C                             %TRIMR(WrkSqlCmd)
268600030204     C                             +
268700030204     C                             ' AND '
268800030204     C                             +
268900030204     C                             ' cttcdf = '
269000030204     C                             +
269100030204     C                             '''' + v1cfor + ''''
269200030204     C                             +
269300030204     C                             ' AND '
269400030204     C                             +
269500030204     C                             ' cttfvl = ''C'''
269600030204     C                             +
269700030204     C                             ' AND '
269800030204     C                             +
269900030204     C                             ' cttddc BETWEEN '
270000030204     C                             + %editc(f50dti:'X')
270100030204     C                             +
270200030204     C                             ' AND ' + %editc(f50dtf:'X')
270300030204     C                             +
270400030204     C                             ' AND'
270500030204     C                             +
270600030204     C                             ' cttnff = 0  group by ctttsr, '
270700030204     C                             +
270800030204     C                             ' case when substr(digits(cttddc), 5, 2) > '
270900030204     C                             +
271000030204     C                             '''' + meseprt + ''''
271100030204     C                             +
271200030204     C                             ' then '
271300030204     C                             +
271400030204     C                             '''' + commes + ''''
271500030204     C                             +
271600030204     C                             ' else'
271700030204     C                             +
271800030204     C                             ' substr(digits(cttddc), 5, 2) end '
271900030204     C                             +
272000030204     C                             ' ORDER BY ctttsr, pippo'
272100030827     c* aff/defl
272200030827     c                   when      f50tip = 'D'
272300030827     C                   EVAL      WrkSqlCmd
272400030827     C                             =
272500031007     C*                            'SELECT attvad,'
272600031007     C                             'SELECT '
272700030827     C                             +
272800030827     C                             ' sum(attimpp),'
272900030827     C                             +
273000030827     C                             ' case when substr(digits(attddc), 5, 2) > '
273100030827     C                             +
273200030827     C                             '''' + meseprt + ''''
273300030827     C                             +
273400030827     C                             ' then '
273500030827     C                             +
273600030827     C                             '''' + commes + ''''
273700030827     C                             +
273800030827     C                             ' else'
273900030827     C                             +
274000030827     C                             ' substr(digits(attddc), 5, 2) end as pippo '
274100030827     C                             +
274200030827     C                             ' from fiatt06l where attsoc = '
274300030827     C                             +
274400030827     C                             '''' + f50soc  + ''''
274500030827     C                             +
274600030827     C                             ' AND '
274700030827     C                             +
274800030827     C                             ' attcdf = '
274900030827     C                             +
275000030827     C                             '''' + v1cfor + ''''
275100030827     C                             +
275200030827     C                             ' AND '
275300030827     C                             +
275400030827     C                             ' attdco <> 0'
275500030827     C                             +
275600030827     C                             ' AND '
275700030827     C                             +
275800030827     C                             ' attddc BETWEEN '
275900030827     C                             + %editc(f50dti:'X')
276000030827     C                             +
276100030827     C                             ' AND ' + %editc(f50dtf:'X')
276200030827     C                             +
276300030827     C                             ' AND'
276400030827     C                             +
276500031007     C*                            ' attnff = 0  group by attvad, '
276600031007     C                             ' attnff = 0  group by '
276700030827     C                             +
276800030827     C                             ' case when substr(digits(attddc), 5, 2) > '
276900030827     C                             +
277000030827     C                             '''' + meseprt + ''''
277100030827     C                             +
277200030827     C                             ' then '
277300030827     C                             +
277400030827     C                             '''' + commes + ''''
277500030827     C                             +
277600030827     C                             ' else'
277700030827     C                             +
277800030827     C                             ' substr(digits(attddc), 5, 2) end '
277900030827     C                             +
278000031007     C*                            ' ORDER BY attvad, pippo'
278100031007     C                             ' ORDER BY pippo'
278200030827     c                   endsl
278300030204     C                   ENDSR
278400030204?     *--------------------------------------------------------------*
278500030204?     *  CARTRN2 Gestione caricamento dati sfl2                      *
278600030204?     *--------------------------------------------------------------*
278700030204     C     CARTRN2       BEGSR
278800030827     c                   select
278900031121     c* prestazioni occasionali
279000031121     c                   when      f50cau <>' '
279100030204     C                   MOVE      v1tipf        h1tipf
279200031121     C                   eval(h)   h1cico = Totso2
279300031121     C                   if        meseso2 > meseprt
279400031121     C                   move      '13'          meseso2
279500030204     C                   END
279600031121     C                   move      meseso2       h1ccmp
279700031121     c* autotrasportatore
279800031121     c                   when      f50tip = 'A'
279900031121     C                   MOVE      v1tipf        h1tipf
280000031121     C                   eval(h)   h1cico = Tots2
280100031121     C                   if        meses2 > meseprt
280200031121     C                   move      '13'          meses2
280300031121     C                   END
280400031121     C                   move      meses2        h1ccmp
280500030204     c* cooperativa
280600030827     c                   when      f50tip = 'C'
280700030205     C                   eval(h)   h1cico = Totsc2
280800030204     C                   MOVEL     tsrsc2        h1tipf
280900030204     C                   if        mesesc2 > meseprt
281000030204     C                   move      '13'          mesesc2
281100030204     C                   END
281200030204     C                   move      mesesc2       h1ccmp
281300030827     c* aff/defl
281400030827     c                   when      f50tip = 'D'
281500030827     C                   eval(h)   h1cico = Totsa2
281600030827     c* ora il conto per aff/defl è lo stesso quindi non importa spaccare
281700030827     c* le contropartite, se un domani si vogliono tenere separati invece
281800031007     c* di chiodare D x le affluenze si metterà il codice di tabella nuovo
281900031007     c*                  if        tsrsa2 = 'A'
282000030827     C                   MOVEL(p)  'D'           h1tipf
282100031007     c*                  else
282200031007     C*                  MOVEL     tsrsa2        h1tipf
282300031007     c*                  end
282400030827     C                   if        mesesa2 > meseprt
282500030827     C                   move      '13'          mesesa2
282600030827     C                   END
282700030827     C                   move      mesesa2       h1ccmp
282800030827     c                   endsl
282900030204     c*
283000030204     c                   if        h1ccmp = 13
283100030204     c                   eval      tredici = *on
283200030204     c                   end
283300030204     c*
283400030204     C                   ADD       1             NRR2
283500030204     C                   WRITE     y350S02
283600030204      *
283700030204     C                   ENDSR
283800030416?     *--------------------------------------------------------------*
283900030416?     *  *INZSR: Operazioni di inizializzazione dati                 *
284000030416?     *--------------------------------------------------------------*
284100030416     C     *INZSR        BEGSR
284200020527      *
284300951011     C     *ENTRY        PLIST
284400951011     C                   PARM                    KPJBA
284500020408     C                   MOVEL     KPJBU         ficn50ds
284600980911     C*---------- RICERCA DITTA :
284700980911     C                   MOVEL     'SOC001'      TIPXSC
284800020412     C                   MOVEL     f50soc        SOCXSC
284900980911     C                   EXSR      REPSOC
285000980911     C     RTNXSC        IFNE      '1'
285100980911     C                   MOVEL     XSOCDS        SOC001
285200980911     C                   MOVEL     xscrgs        RSUT             20
285300980911     c                   end
285400020417      *  Definizione chiave di accesso
285500020423     C     Kuni          KLIST
285600020417     C                   KFLD                    xscsoc
285700020423     C                   KFLD                    f50uni
285800020708     C     Kope          KLIST
285900031204     C                   KFLD                    xscsoc
286000020708     C                   KFLD                    bhmcaustes
286100020708     C     Kdft          KLIST
286200020708     C                   KFLD                    bhmsocieta
286300020708     C                   KFLD                    operepdft
286400031121     C     Kott          KLIST
286500031121     C                   KFLD                    xscsoc
286600031121     C                   KFLD                    v1cfor
286700031121     C                   KFLD                    f50tip
286800031121     C                   KFLD                    f50cau
286900160108     C     Ksre          KLIST
287000160108     C                   KFLD                    xscsoc
287100160108     C                   KFLD                    v1cfor
287200160108     C                   KFLD                    f50tip
287300160108     C     Ksre1         KLIST
287400160108     C                   KFLD                    xscsoc
287500160108     C                   KFLD                    v1cfor
287600160108     C                   KFLD                    f50tip
287700160108     C                   KFLD                    v1ctsr
287800020423     C     Kftt          KLIST
287900020423     C                   KFLD                    xscsoc
288000020423     C                   KFLD                    v1cfor
288100020417     C                   KFLD                    ftttsr
288200020417     C                   KFLD                    fttfvl
288300020417     c                   move      ' '           ftttsr
288400020417     c                   move      'C'           fttfvl
288500030128     C     Kctt          KLIST
288600030128     C                   KFLD                    xscsoc
288700030128     C                   KFLD                    v1cfor
288800030128     C                   KFLD                    cttfvl
288900030128     C     Kctt1         KLIST
289000030128     C                   KFLD                    xscsoc
289100030128     C                   KFLD                    v1cfor
289200030128     C                   KFLD                    cttfvl
289300030128     C                   KFLD                    v1ctsr
289400030827     C     Katt          KLIST
289500030827     C                   KFLD                    xscsoc
289600030827     C                   KFLD                    v1cfor
289700030828     C                   KFLD                    attflg
289800031121     C     Ktbe1         KLIST
289900031121     C                   KFLD                    tbecod
290000031121     C                   KFLD                    tbeke1
290100020724     C     Ktbe          KLIST
290200020724     C                   KFLD                    tbecod
290300020724     C                   KFLD                    tbeke1
290400020724     C                   KFLD                    tbeke2
290500030213     C     Ktab          KLIST
290600030213     C                   KFLD                    tblkut
290700030213     C                   KFLD                    tblcod
290800030213     C                   KFLD                    tblkey
290900030213     c                   eval      tblkut = 1
291000030213     c                   eval      tblcod = '1Z'
291100020417     C     Kfrn          KLIST
291200020417     C                   KFLD                    f50soc
291300030129     C                   KFLD                    v1cfor
291400020417     C     Krco          KLIST
291500020417     C                   KFLD                    f50soc
291600020417     C                   KFLD                    forita
291700030129     C                   KFLD                    v1cfor
291800020514     C     Ktlz          KLIST
291900020514     C                   KFLD                    tlztip
292000020514     C                   KFLD                    tlzpdr
292100020514     C                   KFLD                    f50soc
292200080924     C     Kanf2         KLIST
292300080924     C                   KFLD                    anftip
292400080924     C                   KFLD                    anftsr
292500080924     C                   KFLD                    anftrc
292600080924     C                   KFLD                    anfsoc
292700080924     C                   KFLD                    anfcdf
292800080924     C                   KFLD                    anfdft
292900080924     C                   KFLD                    anfnff
293000080924     C                   KFLD                    anfpdr
293100020408     C* Reperisco i libri IVA ammessi per la filiale.
293200020724     C                   eval      tbecod = 'YIV'
293300020724     C                   eval      tbeke1 = xscsoc
293400020724     C                   eval      tbeke2 = %subst(f50UNI: 1: 3)
293500020724     C     ktbe          chain     tntbe01l
293600020408     C                   if        %found
293700020724     C                   eval      dyiv = tbeuni
293800020408     C                   else
293900020724     C                   clear                   dyiv
294000020408     C                   endif
294100031121     c* aggancio la tabella CAF
294200031121     c                   if        f50cau <> ' '
294300031121     c                   movel(p)  f50cau        tbeke1
294400031121     c                   movel(p)  'CAF'         tbecod
294500031121     c     ktbe1         chain     tntbe01l
294600031121     c                   if        %found(tntbe01l) and tbeatb = ' '
294700031121     c                   movel     tbeuni        dcaf
294800031121     c                   else
294900031121     c                   clear                   dcaf
295000031121     c                   end
295100031121     c                   end
295200020708     C* controllo la data di PROTOCOLLO
295300020708     c                   clear                   xnmkey
295400020708     c                   clear                   xnmser
295500020708     c                   move      *off          xnmcmt
295600020708     c                   clear                   xnmerr
295700020708     c                   clear                   xnmnum
295800020708     c                   clear                   xnmaut
295900020708     C                   MOVE      F50DPR        XnmDat
296000020708     C                   MOVE      F50DPR        XnmDag                         opzionale
296100020708     C                   MOVE      §IVLAQ        COM3              3            opzionale
296200020708     C                   EVAL      XNMSER = '1' + COM3
296300020708     C                   movel(p)  f50uni        Xnmuni
296400020708     C                   movel     '1'           XnmRic
296500020708     C                   movel     'CG'          XnmCtb
296600020708     C                   movel     *on           XnmIva
296700020708     C                   movel     *ON           XnmReg
296800020708     C                   EXSR      CALXNMR
296900020708     c                   if        xnmerr = '7'
297000020708     c                   seton                                        lr
297100020708     c                   return
297200020708     c                   end
297300020411     c* se non ho il documento fornitore non visualizzo la riga in testata
297400020411     c     f50nft        comp      0                                      06
297500020409      * Imposto la data di competenza analitica a fine anno
297600980916      * per verificare se posso impostare la competenza x
297700980916      * l'anno precedente
297800020409     C                   move      XSCSOC        X10societa
297900020409     C                   move      'CG'          X10ctb
298000020415     C                   move      udate         x10data
298100020409     C                   call      'X10ESER'
298200020409     C                   parm                    x10ese
298300020409     c                   if        x10dtines <> *loval
298400020409     C     X10DTINES     SUBDUR    1:*D          WDTFINE
298500020409     c                   endif
298600980916     C                   move      wdtfine       DTCOAN
298700020408     C                   move      f50dpr        Dataiso
298800020408     C                   move      dataiso       DTrif
298900980916      * Controllo se si può inserire ancora data competenza 13
299000980916     C                   Callb     'NDMVC113'
299100020412    >C                   PARM                    xscsoc
299200020411     c                   parm                    unita
299300980916    >C                   PARM      'CG'          Ctb
299400980916    >C                   PARM      *Blank        Keygest
299500020502    >C                   PARM      'R'           Tipdat
299600980916    >C                   PARM      'D'           Tpregz
299700980916    >C                   PARM                    DtCoAn
299800980916     C                   PARM                    DtRif
299900980916    >C                   PARM      'I'           Operazione
300000980916     C                   PARM      '1'           GesMsg
300100980916    >C                   PARM                    Reper
300200980916     C                   PARM                    Esito
300300020408     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
300400020408     C                   movel     'F     '      $kcc              6
300500020408     C                   movel     *blanks       $ksc              8
300600020408     C                   callb     'XCAPCLIFOR'
300700020412     C                   parm                    xscsoc
300800020408     C                   parm                    $kcc
300900020408     C                   parm                    $ksc
301000020408     C                   movel     $kcc          forita            6
301100020409     c* NSYS
301200020415     c                   clear                   xnmuni
301300020415     c                   clear                   xnmctb
301400020415     c                   clear                   xnmkey
301500020415     c                   clear                   xnmerr
301600020415     c                   clear                   xnmnum
301700020415     c                   clear                   xnmaut
301800020409     C                   MOVE      *DATE         XnmDat
301900020409     C                   MOVE      *DATE         XnmDag                         opzionale
302000020409     C                   move      *OFF          XnmCmt
302100020409     C                   move      *OFF          XnmIva
302200020409     C                   move      *OFF          XnmReg
302300020409     C                   EVAL      XnmRic = '1'
302400020409     C                   EVAL      XnmSer = 'NSYS'
302500020409     C                   EXSR      CalXnmr
302600030128      * Se si può inserire ancora data competenza 13
302700030128     c                   if        esito = *off
302800030128     C                   EVAL      commes = '13'
302900030128     C* Se non posso immettere registrazioni aventi data competenza
303000030128     C* dell'anno precedente imposto 01 il mese di competenza
303100030128     C* ed eseguo la normale procedura di contabilizz.
303200030128     c                   else
303300030128     C                   EVAL      commes = '01'
303400030128     c                   end
303500020412     c* estraggo il mese della data protocollo
303600020417     c                   move      f50dpr        dataiso
303700020417     c                   extrct    dataiso:*m    mm                2 0
303800020418     c                   move      mm            meseprt           2
303900150108      *
304000150108      *  contatore serve solo per sbinare x le COOP l'accorpamento e le pulizie
304100150108      *   che devono essere tenute a parte
304200150108     c                   eval        Coop_PULIZIE_a_Parte = *blank
304300150108     c                   if        f50tip = 'C' and f50tsr ='S'
304400150108     c                   eval        Coop_PULIZIE_a_Parte = 'S'
304500150108     c                   end
304600150108      *
304700031121     c* MI COMPONGO L'ISTRUZIONE SQL
304800150108      *                 *----------------*
304900030128     c                   exsr      istruz
305000150108      *                 *----------------*
305100031121     c                   if        f50cau <> ' '
305200031121     c                   seton                                        60
305300031121     c                   end
305400030827     c                   select
305500030128     c* decodifica a video
305600031121     c                   when      f50tip = 'A'
305700030128     c                   eval      dectip = 'AUTOTRASPORTATORI'
305800040914     c                   clear                   dblc
305900040914     c                   movel(p)  '1'           tbeke1
306000040914     c                   movel     'BLC'         tbecod
306100040914     c     ktbe1         chain     tntbe01l
306200040914     c                   if        %found(tntbe01l)
306300040914     c                   movel     tbeuni        dblc
306400040914     c                   end
306500040914     c                   z-add     §blcmin       impmin
306600030827     c                   when      f50tip = 'C'
306700030128     c                   seton                                        60
306800030128     c                   eval      dectip = '   COOPERATIVE   '
306900030704     C                   IF        NOT %OPEN(FICTT06L)
307000030620     c                   open      fiCTT06l
307100030704     C                   END
307200040914     c                   clear                   dblo
307300040914     c                   movel(p)  '1'           tbeke1
307400040914     c                   movel     'BLO'         tbecod
307500040914     c     ktbe1         chain     tntbe01l
307600040914     c                   if        %found(tntbe01l)
307700040914     c                   movel     tbeuni        dblo
307800040914     c                   end
307900040914     c                   z-add     §blomin       impmin
308000030827     c                   when      f50tip = 'D'
308100030827     c                   eval      dectip = 'AFFLUEN./DEFLUEN.'
308200030827     C                   IF        NOT %OPEN(FIATT06L)
308300030827     c                   open      fiaTT06l
308400030827     C                   END
308500040914     c                   clear                   dbld
308600040914     c                   movel(p)  '1'           tbeke1
308700040914     c                   movel     'BLD'         tbecod
308800040914     c     ktbe1         chain     tntbe01l
308900040914     c                   if        %found(tntbe01l)
309000040914     c                   movel     tbeuni        dbld
309100040914     c                   end
309200040914     c                   z-add     §bldmin       impmin
309300030827     c                   endsl
309400020408      *  Inizializzo variabili
309500020408     C                   MOVEL     '1'           WTPVID            1
309600020408     C                   MOVEL     'N'           WFINE             1
309700020411     c                   z-add     0             subnum
309800951012     C                   ENDSR
309900951012      *--------------------------------------------------------------*
310000951012** ERR
310100020415Selezionare almeno un fornitore                                       1
310200021015Esistono fornitori senza partita IVA                                  2
310300030620ATTENZIONE!! Esistono valorizzazioni non confermate                   3
310400080924Manca la massa complessiva su uno o + autotrasportatori               4
310500120816Manca tipologia fornitore in una o più anagrafiche                    5
310600160115Importo fattura errato, avvisare l'uff.Fornitori Operativi di SEDE    6
