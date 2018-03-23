000100000612     H ALWNULL(*USRCTL)
000200000000     H*------------------------------------------------------------------------*
000300010904     H* VERIFICA LIVELLI DI GESTIONE CAP/LOC
000400010904     F*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900061211     Ftiala03l  IF   E           K DISK
001000000508     Fazcpc01l  UF   E           K DISK
001100011119     Fazcpl01l  IF   E           K DISK    PREFIX(i_)
001200061211     Fazcplxxl  UF   E           K DISK    USROPN
001300061211     F                                     RENAME(azcpl000:azcplxxx)
001400011119     Ftisiu2p   O    E             PRINTER OFLIND(*IN01)
001500011119     Fprtf198   O    f  198        PRINTER
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900991124     D*------------------
002000010905     D* COMANDI
002100011119     D*------------------
002200010905     D CMD             S             80    DIM(2) CTDATA PERRCD(1)
002300000000     D*------------------------------------------------------------------------*
002400000000     D* INPUT
002500000000     D*------------------------------------------------------------------------*
002600000505     D*--------------------
002700000505     D* DS "XSRDAT" - CONTROLLO DATA (8)
002800000505     D*--------------------
002900000505     DWLBDA8           DS                  INZ
003000000505     D  G08DAT                 1      8  0
003100000505     D  G08INV                 9     16  0
003200000505     D  G08ERR                17     17
003300000505     D  G08TGI                18     22  0
003400000000     D*-------------------
003500000000     D* ARCHITETTURA
003600000000     D*-------------------
003700000000     D KPJBA         E DS
003800011119     D*-------------------
003900011119     D* DS PER PASSAGGIO PARAMETRI A *PGM DI ELABORAZIONE
004000011119     D*-------------------
004100011119     D param           DS
004200011119     D  parver                        5S 0 inz(*zeros)
004300011119     D  parcap                        9    inz(*blanks)
004400011119     D  parsim                        1    inz('N')
004500000509     D*-------------------
004600000509     D* Variabili di lavoro
004700000509     D*-------------------
004800000509     D  okcap          s              1n
004900000509     D  okestcap       s              1n
005000000509     D  okloc          s              1n
005100000509     D  okestloc       s              1n
005200000530     D  WrkAlfa4       s              4a   INZ(*blanks)
005300000531     D  WCap           s            100a   INZ(*blanks)
005400000531     D  WECap          s            100a   INZ(*blanks)
005500000531     D  WLoc           s            100a   INZ(*blanks)
005600000531     D  WELoc          s            100a   INZ(*blanks)
005700011119     D  wrktesto       s            198a   INZ(*blanks)
005800000000     C*------------------------------------------------------------------------*
005900000000     C* MAIN LINE
006000000000     C*------------------------------------------------------------------------*
006100000000     C*
006200011119     C                   EXSR      RUNMAIN
006300991118     C*
006400000000     C                   SETON                                        LR
006500000000     C*------------------------------------------------------------------------*
006600011119     C* RUNMAIN - ROUTINE DI ELABORAZIONE
006700000000     C*------------------------------------------------------------------------*
006800011119     C     RUNMAIN       BEGSR
006900011119     C*
007000011119     C* Verifico innanzitutto i parametri in ingresso
007100011119     C     PARSIM        IFEQ      'S'
007200011119     C                   SETOFF                                       50
007300011119     C                   EVAL      STPSIM='(SIMULAZIONE)'
007400011119     C                   ELSE
007500011119     C                   EXCEPT    t_listing
007600011119     C                   SETON                                        50
007700011119     C                   EVAL      STPSIM='(AGGIORNAMENTO)'
007800011119     C                   ENDIF
007900010906     C*
008000010906     C* Eseguo le override sul file delle località (azcpl01l -> azcplxxl)
008100010906     C                   Z-ADD     80            LENGH            15 5
008200010906     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300010906     C                   PARM                    CMD(1)
008400010906     C                   PARM                    LENGH
008500010906     C*
008600010906     C* Dopo l'override apro il file
008700010906     C                   OPEN      azcplxxl
008800991105     C*
008900010905     C* Lancia l'elaborazoine
009000010905     C                   EXSR      EXEOPE
009100010906     C*
009200010906     C* Al termine dell'elaborazione chiudo il file
009300010906     C                   CLOSE     azcplxxl
009400010906     C*
009500010906     C* Cancello le override sul file delle località (azcpl01l -> azcplxxl)
009600010906     C                   Z-ADD     80            LENGH            15 5
009700010906     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800010906     C                   PARM                    CMD(2)
009900010906     C                   PARM                    LENGH
010000991105     C*
010100000000     C                   ENDSR
010200010905     C*------------------------------------------------------------------------*
010300010905     C*  EXEOPE - ESEGUE L'EFFETTIVA ELABORAZIONE
010400010905     C*------------------------------------------------------------------------*
010500010905     C     EXEOPE        BEGSR
010600010905     C*
010700010905     C* PRIMA DI TUTTO STAMPO SUBITO LA TESTATA
010800011119     C                   MOVEL     PARVER        STPVER
010900011119     C                   WRITE     SIU2TE
011000010905     C*
011100011119     C* SCORRO TUTTO IL FILE LOCALITA' PER LA VERSIONE (E CAP OPZ.) SCELTA
011200011119     C                   MOVE(p)   PARVER        kplver
011300010905     C                   MOVEL     *blanks       kplnar
011400011119     C                   MOVEL     PARCAP        kplcap
011500010905     C                   MOVEL     *blanks       kplloc
011600010905     C*
011700010905     C* EFFETTUO IL POSIZIONAMENTO INIZIALE
011800010905     C     keyxpl        SETLL     azcpl01l
011900010905     C* INIZIALIZZO I CAMPI DI WRK X IL DEPOSITO ROTTURA DI CODICE
012000010905     C                   MOVEL     *blanks       depCAP            9
012100010905     C* CICLO SULL'ARCHIVIO PER CHIAVE UGUALE
012200010905     C                   DOW       not %eof(azcpl01l)
012300010905     C* PROSEGUO SCORRENDO IL FILE
012400010905     C                   READ      azcpl01l
012500070514     C                   IF        not %eof(azcpl01l)
012600070514     C*
012700070514     C* A SUPERAMENTO VERSIONE RICHIESTA A VIDEO => ESCO
012800070514     C                   IF        i_cplver > PARVER
012900070514     C                   LEAVE
013000070514     C                   ELSE
013100070514     C*
013200010905     C* VERIFICO SE E' STATA EFFETTUATA UNA EVENTUALE SELEZIONE ANCHE A LIVELLO DI CAP
013300011119     C                   IF        PARCAP   <> *blanks and
013400011119     C                             i_cplcap <> PARCAP
013500010905     C                   leave
013600010905     C                   ELSE
013700010905     C* VERIFICO L'AVVENUTA ROTTURA DI CODICE
013800010906     C                   IF        i_cplcap <> depCAP
013900010905     C* SE IL FLAG DI CONTROLLO DEI LIVELLI DI GESTIONE E' ACCESO...
014000010905     C* ...ESEGUO LA ROUTINE DI CONSIDERAZIONE
014100010905     C                   IF        flgLIVL = 1
014200010905     C                   EXSR      EXEMOD
014300010905     C                   ENDIF
014400010905     C* SALVO IL CAP CORRENTE
014500010906     C                   MOVEL     i_cplcap      depCAP
014600010905     C* INIZIALIZZO IL FLAG DI "TUTTI LIVELLI L"
014700010905     C                   Z-ADD     1             flgLIVL           1 0
014800010905     C                   ENDIF
014900010905     C* VERIFICO IL LIVELLO DI GESTIONE DELLA LOCALITA' CORRENTE
015000010906     C                   IF        i_cplliv = 'C'
015100010905     C                   Z-ADD     *zeros        flgLIVL
015200010905     C                   ENDIF
015300010905     C                   ENDIF
015400070514     C                   ENDIF
015500010905     C                   ENDIF
015600010905     C                   ENDDO
015700010905     C*
015800010905     C* ED INFINE STAMPO LA CHIUSURA
015900011119     C                   WRITE     SIU2RI
016000011119     C                   WRITE     SIU2NO
016100011119     C                   WRITE     SIU2RB
016200011119     C                   WRITE     SIU2FI
016300010905     C*
016400010905     C                   ENDSR
016500010905     C*------------------------------------------------------------------------*
016600010905     C*  EXEMOD - ESEGUE LE OPERAZIONI DI MODIFICA/LISTING
016700010905     C*------------------------------------------------------------------------*
016800010905     C     EXEMOD        BEGSR
016900010905     C*
017000010905     C                   MOVEL     *blanks       wrknar            3
017100010905     C                   Z-ADD     *zeros        totASS           18 0
017200010905     C                   Z-ADD     *zeros        maxASS           18 0
017300010905     C                   Z-ADD     *zeros        locASS           18 0
017400010906     C                   Z-ADD     *zeros        tmpASS           18 0
017500010905     C                   MOVEL     *blanks       depLOC           35
017600010906     C                   MOVEL     *blanks       tmpLOC           35
017700010905     C*
017800061211     C     keyala        SETLL     tiala03l
017900061211     C     keyala        READE     tiala03l
018000010905     C*
018100061211     C                   DOW       not %eof(tiala03l)
018200010905     C*
018300061211     C                   ADD       alatos        totass
018400061211     C                   IF        alaloc = tmpLOC
018500061211     C                   ADD       alatos        tmpASS
018600010906     C                   ELSE
018700010906     C                   Z-ADD     tmpASS        locASS
018800061211     C                   Z-ADD     alatos        tmpASS
018900010905     C*
019000010905     C                   IF        locASS > maxASS
019100010905     C                   Z-ADD     locASS        maxASS
019200010906     C                   MOVEL     tmpLOC        depLOC
019300010905     C                   ENDIF
019400061211     C                   MOVEL     alaloc        tmpLOC
019500010906     C                   ENDIF
019600010905     C*
019700061211     C     keyala        READE     tiala03l
019800010905     C                   ENDDO
019900010906     C*
020000010906     C* EFFETTUO LO STESSO TEST ALLA FINE DEL FILE PER VERIFICARE QUANTO RIMASTO "IN CANNA"
020100010906     C                   IF        tmpASS > maxASS
020200010906     C                   Z-ADD     tmpASS        maxASS
020300010906     C                   MOVEL     tmpLOC        depLOC
020400010906     C                   ENDIF
020500010905     C*
020600010905     C* CALCOLO LA PERVCENTUALE DI ASSORBIMENTO DELLA LOCALITA' PRESCELTA SUL TOTALE DEL CAP...
020700010905     C* ...E VALORIZZO I CAMPI IN STAMPA
020800010905     C                   IF        totASS > 0
020900010906     C* INIZIALIZZO IL FORMATO RECORD DI STAMPA
021000011119     C                   CLEAR                   SIU2RA
021100010906     C*
021200010906     C                   EVAL      STPPER = maxASS * 100 / totASS
021300061211     C                   EVAL      STPCAP = alacap
021400061211     C                   EVAL      STPPRV = alaprv
021500010906     C                   EVAL      STPLOC = depLOC
021600010910     C* UNA VOLTA PRESCELTA LA LOCALITA' CANDIDATA REPERISCO DI NUOVO I DATI CPL E ANCHE I DATI PPL
021700010906     C     keyxpl        CHAIN     azcplxxl
021800010906     C                   IF        %found(azcplxxl)
021900010910     C* SE TROVO LA LOCALITA' LEGGO ANCHE IL CAP RELATIVO
022000061211     C     keycpc        CHAIN     azcpc01l
022100010910     C*
022200120522     C                   IF        CPLPis = 'S'
022300120522     C                   EVAL      STPPIS = cplpis
022400120522     C                   ENDIF
022500120522     C                   EVAL      STPISO = cpliso
022600010906     C                   EVAL      STPLNA = cpllna
022700010906     C                   EVAL      STPZNC = cplznc
022800010906     C                   EVAL      STPLOS = cpllos
022900010906     C                   EVAL      STPZOS = cplzos
023000010906     C                   EVAL      STPLOL = cpllol
023100010906     C                   EVAL      STPZOL = cplzol
023200010906     C                   EVAL      STPLFD = cpllna
023300010906     C                   EVAL      STPZFD = cplzfd
023400010906     C                   EVAL      STPLKS = cpllks
023500010906     C                   EVAL      STPLMS = cpllms
023600010906     C                   EVAL      STPLKG = cpllkg
023700010906     C                   EVAL      STPLMC = cpllmc
023800010906     C                   EVAL      STPTTC = cplttc
023900010906     C                   MOVEL     *all'9'       STPTTR
024000010906     C                   MOVE      cplfi2        STPTTR
024100010906     C*
024200010906     C                   ENDIF
024300010905     C*
024400010905     C* INFINE LISTO QUANTO ELUCUBRATO
024500010906     C                   IF        *in01 = *on
024600011119     C                   WRITE     SIU2TE
024700010906     C                   SETOFF                                       01
024800010905     C                   ENDIF
024900010910     C*
025000011119     C* EFFETTUO OPERAZIONI DI COMPARAZIONE DATI CAP/LOC BAR/PT
025100010910     C                   EXSR      EXECMP
025200010910     C*
025300011119     C                   WRITE     SIU2RA
025400010906     C                   ENDIF
025500011119     C*
025600011119     C* EFFETTUO OPERAZIONI DI AGGIORNAMENTO ARCHIVI E LISTING SE RICHIESTO NEL LANCIO
025700011119     C                   IF        PARSIM='N'
025800061211     C                   IF        STPNOT='1'
025900011123     C*                  EXSR      EXEAGGBAR
026000061211     C                   ENDIF
026100011123     C                   IF        STPNOT='2'
026200011123     C*                  EXSR      EXEAGGBAR
026300011123     C                   ENDIF
026400061211     C*                  IF        STPNOT='3'
026500061211     C*                  ENDIF
026600011123     C                   IF        STPNOT='2'
026700011119     C                   EVAL      wrktesto = STPNOT + ' ' + CPCCAP + ' ' +
026800011119     C                                        'aggiornato CAP da località: ' +
026900011119     C                                        STPLOC
027000011119     C                   EXCEPT    d_listing
027100011119     C                   ENDIF
027200011119     C                   ENDIF
027300010905     C*
027400010905     C                   ENDSR
027500010910     C*------------------------------------------------------------------------*
027600010910     C*  EXECMP - ESEGUE LE OPERAZIONI DI COMPARAZIONE
027700010910     C*------------------------------------------------------------------------*
027800010910     C     EXECMP        BEGSR
027900010910     C*
028000010910     C                   SETOFF                                       27
028100010910     C*
028200010910     C                   IF        cpllna = cpclna AND
028300010910     C                             cplznc = cpcznc AND
028400010910     C                             cpliso = cpciso AND
028500010910     C                             cplzfd = cpczfd AND
028600010910     C                             cpllkg = cpclkg AND
028700010910     C                             cpllmc = cpclmc AND
028800010910     C                             cpllol = cpclol AND
028900010910     C                             cplzol = cpczol AND
029000010910     C                             cpllks = cpclks AND
029100010910     C                             cpllms = cpclms AND
029200010910     C                             cpllos = cpclos AND
029300010910     C                             cplzos = cpczos AND
029400010910     C                             cpllk2 = cpclk2 AND
029500010910     C                             cpllm2 = cpclm2 AND
029600010910     C                             cpllo2 = cpclo2 AND
029700010910     C                             cplzo2 = cpczo2 AND
029800010910     C                             cplcts = cpccts AND
029900010910     C                             cplcte = cpccte AND
030000010910     C                             cplttc = cpcttc AND
030100010910     C                             cplpii = cpcpii AND
030200010910     C                             cplpid = cpcpid AND
030300010910     C                             cplpis = cpcpis AND
030400010910     C                             cplgdd = cpcgdd AND
030500010910     C                             cplfi2 = cpcfi2
030600061211     C                   EVAL      STPNOT = '1'
030700010910     C*
030800010910     C                   ELSE
030900061211     C*
031000061211     C                   EVAL      STPNOT = '2'
031100010910     C*
031200010910     C                   ENDIF
031300010910     C*
031400010910     C* VERIFICO IL TIPO DI SITUAZIONE X EVIDENZIARLO IN STAMPA
031500011123     C                   IF        STPNOT='2'
031600010910     C                   SETON                                        27
031700010910     C                   ENDIF
031800010910     C*
031900010910     C                   ENDSR
032000011119     C*------------------------------------------------------------------------*
032100011119     C*  EXEAGGBAR - ESEGUE LE OPERAZIONI DI AGGIORNAMENTO SUGLI ARCHIVI BARTOLINI
032200011119     C*------------------------------------------------------------------------*
032300011119     C     EXEAGGBAR     BEGSR
032400011119     C*
032500011119     C* Copio i dati instradamenti dalla località al cap ...
032600011119     C                   Z-ADD     CPLLNA        CPCLNA
032700011119     C                   Z-ADD     CPLLOL        CPCLOL
032800011119     C                   Z-ADD     CPLLO2        CPCLO2
032900011119     C                   Z-ADD     CPLLOS        CPCLOS
033000011119     C                   Z-ADD     CPLZNC        CPCZNC
033100011119     C                   Z-ADD     CPLZOL        CPCZOL
033200011119     C                   Z-ADD     CPLZO2        CPCZO2
033300011119     C                   Z-ADD     CPLZOS        CPCZOS
033400011119     C                   Z-ADD     CPLZFD        CPCZFD
033500011119     C                   Z-ADD     CPLLKG        CPCLKG
033600011119     C                   Z-ADD     CPLLMC        CPCLMC
033700011119     C                   Z-ADD     CPLLKS        CPCLKS
033800011119     C                   Z-ADD     CPLLMS        CPCLMS
033900011119     C                   Z-ADD     CPLLK2        CPCLK2
034000011119     C                   Z-ADD     CPLLM2        CPCLM2
034100011119     C                   MOVEL     CPLCTS        CPCCTS
034200011119     C                   MOVEL     CPLCTE        CPCCTE
034300011119     C                   MOVEL     CPLISO        CPCISO
034400011119     C                   MOVEL     CPLPII        CPCPII
034500011119     C                   MOVEL     CPLPID        CPCPID
034600011119     C                   MOVEL     CPLPIS        CPCPIS
034700011119     C                   MOVEL     CPLGDD        CPCGDD
034800011119     C                   Z-ADD     CPLTTC        CPCTTC
034900011119     C                   MOVEL     CPLFI2        CPCFI2
035000011119     C*
035100011119     C* ... e azzero quelli sulla località ...
035200011119     C
035300011119     C                   Z-ADD     *ZEROS        CPLLNA
035400011119     C                   Z-ADD     *ZEROS        CPLLOL
035500011119     C                   Z-ADD     *ZEROS        CPLLO2
035600011119     C                   Z-ADD     *ZEROS        CPLLOS
035700011119     C                   Z-ADD     *ZEROS        CPLZNC
035800011119     C                   Z-ADD     *ZEROS        CPLZOL
035900011119     C                   Z-ADD     *ZEROS        CPLZO2
036000011119     C                   Z-ADD     *ZEROS        CPLZOS
036100011119     C                   Z-ADD     *ZEROS        CPLZFD
036200011119     C                   Z-ADD     *ZEROS        CPLLKG
036300011119     C                   Z-ADD     *ZEROS        CPLLMC
036400011119     C                   Z-ADD     *ZEROS        CPLLKS
036500011119     C                   Z-ADD     *ZEROS        CPLLMS
036600011119     C                   Z-ADD     *ZEROS        CPLLK2
036700011119     C                   Z-ADD     *ZEROS        CPLLM2
036800011119     C                   MOVEL     *BLANKS       CPLCTS
036900011119     C                   MOVEL     *BLANKS       CPLCTE
037000011119     C                   MOVEL     *BLANKS       CPLISO
037100011119     C                   MOVEL     *BLANKS       CPLPII
037200011119     C                   MOVEL     *BLANKS       CPLPID
037300011119     C                   MOVEL     *BLANKS       CPLPIS
037400011119     C                   MOVEL     *BLANKS       CPLGDD
037500011119     C                   Z-ADD     *ZEROS        CPLTTC
037600011119     C                   MOVEL     *BLANKS       CPLFI2
037700011119     C*
037800011119     C* ... che diventa gestita a livello di cap
037900011119     C                   MOVEL     'C'           CPLLIV
038000011119     C*
038100011119     C* Quindi sfleggo i campi di trasmissione ...
038200011119     C                   MOVEL     *blanks       CPCFTR
038300011119     C                   MOVEL     *blanks       CPLFTR
038400011119     C*
038500011119     C* ... e aggiorno
038600011119     C                   UPDATE    azcpc000
038700011119     C                   UPDATE    azcplxxx
038800011119     C*
038900011119     C                   ENDSR
039000000000     C*------------------------------------------------------------------------*
039100980403     C* *INZSR - OPERAZIONI INIZIALI
039200000000     C*------------------------------------------------------------------------*
039300980403     C     *INZSR        BEGSR
039400000000     C*
039500000000     C* RICEVIMENTO PARAMETRI
039600000000     C     *ENTRY        PLIST
039700000000     C                   PARM                    KPJBA
039800011119     C*
039900011119     C                   MOVEL     kpjbu         param
040000000508     C*
040100000508     C* DEFINISCE LE VARIABILI RIFERITE AL DATABASE
040200010905     C     *LIKE         DEFINE    cpcver        kpcver
040300010905     C     *LIKE         DEFINE    cpcnar        kpcnar
040400010905     C     *LIKE         DEFINE    cpccap        kpccap
040500010905     C     *LIKE         DEFINE    cplver        kplver
040600010905     C     *LIKE         DEFINE    cplnar        kplnar
040700010905     C     *LIKE         DEFINE    cplcap        kplcap
040800010905     C     *LIKE         DEFINE    cplloc        kplloc
040900000508     C*
041000000508     C*------------------------------
041100000508     C* DEFINISCO LE CHIAVI DI ACCESSO INDICIZZATO AI FILES
041200000508     C*------------------------------
041300010905     C* CHIAVE SU AZCPC01L - COMPLETA
041400010906     C     keycpc        KLIST
041500010910     C                   KFLD                    kplver
041600010910     C                   KFLD                    kplnar
041700061211     C                   KFLD                    depCAP
041800010905     C* CHIAVE SU AZCPL01L - COMPLETA
041900010906     C     keycpl        KLIST
042000000508     C                   KFLD                    kplver
042100000508     C                   KFLD                    kplnar
042200000509     C                   KFLD                    kplcap
042300000509     C                   KFLD                    kplloc
042400010906     C* CHIAVE SU AZPPL01L/AZCPLXXL - COMPLETA
042500010906     C     keyxpl        KLIST
042600010906     C                   KFLD                    kplver
042700010906     C                   KFLD                    kplnar
042800010906     C                   KFLD                    depCAP
042900010906     C                   KFLD                    depLOC
043000061211     C* CHIAVE SU TIALA03L - PARZIALE
043100061211     C     keyala        KLIST
043200010905     C                   KFLD                    wrknar
043300010905     C                   KFLD                    depCAP
043400010906     C*
043500010906     C* REPERISCO LE DATE DI RIFERIMENTO ARCHIVIO ASSORBIMENTI
043600061211     C                   READ(e)   tiala03l
043700061211     C                   IF        not %eof(tiala03l)
043800010906     C* GIRO LA DATA INIZIALE
043900010906     C                   Z-ADD     *ZEROS        G08DAT
044000061211     C                   Z-ADD     aladai        G08INV
044100010906     C                   MOVEL     '3'           G08ERR
044200010906     C                   CALL      'XSRDA8'
044300010906     C                   PARM                    WLBDA8
044400010906     C                   Z-ADD     G08DAT        STPDDA
044500010906     C* GIRO LA DATA FINALE
044600010906     C                   Z-ADD     *ZEROS        G08DAT
044700061211     C                   Z-ADD     aladaf        G08INV
044800010906     C                   MOVEL     '3'           G08ERR
044900010906     C                   CALL      'XSRDA8'
045000010906     C                   PARM                    WLBDA8
045100010906     C                   Z-ADD     G08DAT        STPDAA
045200010906     C                   ENDIF
045300000505     C*
045400000505     C* CALCOLA LA DATA CORRENTE
045500000505     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
045600000505     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
045700000505     C                   Z-ADD     WN8           G08DAT
045800000505     C                   Z-ADD     *ZEROS        G08INV
045900000505     C                   MOVEL     '0'           G08ERR
046000000505     C                   CALL      'XSRDA8'
046100000505     C                   PARM                    WLBDA8
046200000505     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
046300000000     C*
046400000000     C                   ENDSR
046500011119
046600011119     Oprtf198   E            t_listing         2
046700011119     O                                              'Controllo LIVELLI GESTIONE'
046800011119     O          E            d_listing   1
046900011119     O                       wrktesto
047000010905** CMD - COMANDI CL
047100010906OVRDBF FILE(AZCPLXXL) TOFILE(AZCPL01L)
047200010906DLTOVR FILE(AZCPLXXL) LVL(*)
