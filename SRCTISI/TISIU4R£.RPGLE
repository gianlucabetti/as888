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
001200140616     Fazcplxxl  IF   E           K DISK    USROPN
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
013900140616     C* ESEGUO LA ROUTINE DI CONSIDERAZIONE SU ASSORBIMENTO
014000010905     C                   EXSR      EXEMOD
014100010905     C* SALVO IL CAP CORRENTE
014200010906     C                   MOVEL     i_cplcap      depCAP
014300010905     C                   ENDIF
014400010905     C                   ENDIF
014500010905     C                   ENDIF
014600010905     C                   ENDIF
014700010905     C                   ENDDO
014800010905     C*
014900010905     C* ED INFINE STAMPO LA CHIUSURA
015000011119     C                   WRITE     SIU2RI
015100011119     C                   WRITE     SIU2NO
015200011119     C                   WRITE     SIU2RB
015300011119     C                   WRITE     SIU2FI
015400010905     C*
015500010905     C                   ENDSR
015600010905     C*------------------------------------------------------------------------*
015700010905     C*  EXEMOD - ESEGUE LE OPERAZIONI DI MODIFICA/LISTING
015800010905     C*------------------------------------------------------------------------*
015900010905     C     EXEMOD        BEGSR
016000010905     C*
016100010905     C                   MOVEL     *blanks       wrknar            3
016200010905     C                   Z-ADD     *zeros        totASS           18 0
016300010905     C                   Z-ADD     *zeros        maxASS           18 0
016400010905     C                   Z-ADD     *zeros        locASS           18 0
016500010906     C                   Z-ADD     *zeros        tmpASS           18 0
016600010905     C                   MOVEL     *blanks       depLOC           35
016700010906     C                   MOVEL     *blanks       tmpLOC           35
016800010905     C*
016900061211     C     keyala        SETLL     tiala03l
017000061211     C     keyala        READE     tiala03l
017100010905     C*
017200061211     C                   DOW       not %eof(tiala03l)
017300010905     C*
017400061211     C                   ADD       alatos        totass
017500061211     C                   IF        alaloc = tmpLOC
017600061211     C                   ADD       alatos        tmpASS
017700010906     C                   ELSE
017800010906     C                   Z-ADD     tmpASS        locASS
017900061211     C                   Z-ADD     alatos        tmpASS
018000010905     C*
018100010905     C                   IF        locASS > maxASS
018200010905     C                   Z-ADD     locASS        maxASS
018300010906     C                   MOVEL     tmpLOC        depLOC
018400010905     C                   ENDIF
018500061211     C                   MOVEL     alaloc        tmpLOC
018600010906     C                   ENDIF
018700010905     C*
018800061211     C     keyala        READE     tiala03l
018900010905     C                   ENDDO
019000010906     C*
019100010906     C* EFFETTUO LO STESSO TEST ALLA FINE DEL FILE PER VERIFICARE QUANTO RIMASTO "IN CANNA"
019200010906     C                   IF        tmpASS > maxASS
019300010906     C                   Z-ADD     tmpASS        maxASS
019400010906     C                   MOVEL     tmpLOC        depLOC
019500010906     C                   ENDIF
019600010905     C*
019700010905     C* CALCOLO LA PERVCENTUALE DI ASSORBIMENTO DELLA LOCALITA' PRESCELTA SUL TOTALE DEL CAP...
019800010905     C* ...E VALORIZZO I CAMPI IN STAMPA
019900010905     C                   IF        totASS > 0
020000010906     C* INIZIALIZZO IL FORMATO RECORD DI STAMPA
020100011119     C                   CLEAR                   SIU2RA
020200010906     C*
020300010906     C                   EVAL      STPPER = maxASS * 100 / totASS
020400140616     C*
020500140616     C* VERIFICO SE PERCENTUALE ASSORBIMENTO LOCALITA' PRESCELTA > 70% ASSORB. TOTALE CAP
020600140616     C                   IF        STPPER >= 70
020700140616     C*
020800061211     C                   EVAL      STPCAP = alacap
020900061211     C                   EVAL      STPPRV = alaprv
021000010906     C                   EVAL      STPLOC = depLOC
021100140616     C* UNA VOLTA PRESCELTA LA LOCALITA' CANDIDATA REPERISCO DI NUOVO I DATI CPL
021200010906     C     keyxpl        CHAIN     azcplxxl
021300010906     C                   IF        %found(azcplxxl)
021400140616     C*
021500140616     C* CONSIDERO UNICAMENTE LOCALITA' A LIVELLO 'L'
021600140616     C                   IF        cplLIV = 'L'
021700140616     C*
021800010910     C* SE TROVO LA LOCALITA' LEGGO ANCHE IL CAP RELATIVO
021900061211     C     keycpc        CHAIN     azcpc01l
022000010910     C*
022100120522     C                   IF        CPLPis = 'S'
022200120522     C                   EVAL      STPPIS = cplpis
022300120522     C                   ENDIF
022400120522     C                   EVAL      STPISO = cpliso
022500010906     C                   EVAL      STPLNA = cpllna
022600010906     C                   EVAL      STPZNC = cplznc
022700010906     C                   EVAL      STPLOS = cpllos
022800010906     C                   EVAL      STPZOS = cplzos
022900010906     C                   EVAL      STPLOL = cpllol
023000010906     C                   EVAL      STPZOL = cplzol
023100010906     C                   EVAL      STPLFD = cpllna
023200010906     C                   EVAL      STPZFD = cplzfd
023300010906     C                   EVAL      STPLKS = cpllks
023400010906     C                   EVAL      STPLMS = cpllms
023500010906     C                   EVAL      STPLKG = cpllkg
023600010906     C                   EVAL      STPLMC = cpllmc
023700010906     C                   EVAL      STPTTC = cplttc
023800010906     C                   MOVEL     *all'9'       STPTTR
023900010906     C                   MOVE      cplfi2        STPTTR
024000010905     C*
024100010905     C* INFINE LISTO QUANTO ELUCUBRATO
024200010906     C                   IF        *in01 = *on
024300011119     C                   WRITE     SIU2TE
024400010906     C                   SETOFF                                       01
024500010905     C                   ENDIF
024600140616     C*
024700140616     C* EFFETTUO OPERAZIONI DI COMPARAZIONE DATI CAP/LOC BAR
024800140616     C                   EXSR      EXECMP
024900010910     C*
025000011119     C                   WRITE     SIU2RA
025100011119     C*
025200140616     C* SE OK EFFETTUO OPERAZIONI DI AGGIORNAMENTO ARCHIVI E LISTING SE RICHIESTO NEL LANCIO
025300140616     C                   IF        STPNOT='3'
025400011119     C                   IF        PARSIM='N'
025500140616     C                   EXSR      EXEAGGBAR
025600061211     C                   ENDIF
025700011119     C                   EVAL      wrktesto = STPNOT + ' ' + CPCCAP + ' ' +
025800140616     C                                   'aggiornati tempi CAP da località: ' +
025900140616     C                                   STPLOC
026000011119     C                   EXCEPT    d_listing
026100011119     C                   ENDIF
026200140616     C*
026300140616     C                   ENDIF
026400140616     C                   ENDIF
026500140616     C*
026600140616     C                   ENDIF
026700140616     C                   ENDIF
026800010905     C*
026900010905     C                   ENDSR
027000140616     C*------------------------------------------------------------------------*
027100140616     C*  EXECMP - ESEGUE LE OPERAZIONI DI COMPARAZIONE
027200140616     C*------------------------------------------------------------------------*
027300140616     C     EXECMP        BEGSR
027400140616     C*
027500140616     C                   SETOFF                                       27
027600140616     C                   EVAL      STPNOT = *blanks
027700140616     C*
027800140616     C                   IF        cpcTTC = cplTTC AND
027900140616     C                             cpcFI2 = cplFI2
028000140616     C                   ELSE
028100140616     C                   EVAL      STPNOT = '3'
028200140616     C                   ENDIF
028300140616     C*
028400140616     C                   ENDSR
028500011119     C*------------------------------------------------------------------------*
028600011119     C*  EXEAGGBAR - ESEGUE LE OPERAZIONI DI AGGIORNAMENTO SUGLI ARCHIVI BARTOLINI
028700011119     C*------------------------------------------------------------------------*
028800011119     C     EXEAGGBAR     BEGSR
028900011119     C*
029000011119     C* Copio i dati instradamenti dalla località al cap ...
029100011119     C                   Z-ADD     CPLTTC        CPCTTC
029200011119     C                   MOVEL     CPLFI2        CPCFI2
029300011119     C*
029400011119     C* Quindi sfleggo i campi di trasmissione ...
029500011119     C                   MOVEL     *blanks       CPCFTR
029600011119     C                   MOVEL     *blanks       CPLFTR
029700011119     C*
029800011119     C* ... e aggiorno
029900011119     C                   UPDATE    azcpc000
030000011119     C*
030100011119     C                   ENDSR
030200000000     C*------------------------------------------------------------------------*
030300980403     C* *INZSR - OPERAZIONI INIZIALI
030400000000     C*------------------------------------------------------------------------*
030500980403     C     *INZSR        BEGSR
030600000000     C*
030700000000     C* RICEVIMENTO PARAMETRI
030800000000     C     *ENTRY        PLIST
030900000000     C                   PARM                    KPJBA
031000011119     C*
031100011119     C                   MOVEL     kpjbu         param
031200000508     C*
031300000508     C* DEFINISCE LE VARIABILI RIFERITE AL DATABASE
031400010905     C     *LIKE         DEFINE    cpcver        kpcver
031500010905     C     *LIKE         DEFINE    cpcnar        kpcnar
031600010905     C     *LIKE         DEFINE    cpccap        kpccap
031700010905     C     *LIKE         DEFINE    cplver        kplver
031800010905     C     *LIKE         DEFINE    cplnar        kplnar
031900010905     C     *LIKE         DEFINE    cplcap        kplcap
032000010905     C     *LIKE         DEFINE    cplloc        kplloc
032100000508     C*
032200000508     C*------------------------------
032300000508     C* DEFINISCO LE CHIAVI DI ACCESSO INDICIZZATO AI FILES
032400000508     C*------------------------------
032500010905     C* CHIAVE SU AZCPC01L - COMPLETA
032600010906     C     keycpc        KLIST
032700010910     C                   KFLD                    kplver
032800010910     C                   KFLD                    kplnar
032900061211     C                   KFLD                    depCAP
033000010905     C* CHIAVE SU AZCPL01L - COMPLETA
033100010906     C     keycpl        KLIST
033200000508     C                   KFLD                    kplver
033300000508     C                   KFLD                    kplnar
033400000509     C                   KFLD                    kplcap
033500000509     C                   KFLD                    kplloc
033600010906     C* CHIAVE SU AZPPL01L/AZCPLXXL - COMPLETA
033700010906     C     keyxpl        KLIST
033800010906     C                   KFLD                    kplver
033900010906     C                   KFLD                    kplnar
034000010906     C                   KFLD                    depCAP
034100010906     C                   KFLD                    depLOC
034200061211     C* CHIAVE SU TIALA03L - PARZIALE
034300061211     C     keyala        KLIST
034400010905     C                   KFLD                    wrknar
034500010905     C                   KFLD                    depCAP
034600010906     C*
034700010906     C* REPERISCO LE DATE DI RIFERIMENTO ARCHIVIO ASSORBIMENTI
034800061211     C                   READ(e)   tiala03l
034900061211     C                   IF        not %eof(tiala03l)
035000010906     C* GIRO LA DATA INIZIALE
035100010906     C                   Z-ADD     *ZEROS        G08DAT
035200061211     C                   Z-ADD     aladai        G08INV
035300010906     C                   MOVEL     '3'           G08ERR
035400010906     C                   CALL      'XSRDA8'
035500010906     C                   PARM                    WLBDA8
035600010906     C                   Z-ADD     G08DAT        STPDDA
035700010906     C* GIRO LA DATA FINALE
035800010906     C                   Z-ADD     *ZEROS        G08DAT
035900061211     C                   Z-ADD     aladaf        G08INV
036000010906     C                   MOVEL     '3'           G08ERR
036100010906     C                   CALL      'XSRDA8'
036200010906     C                   PARM                    WLBDA8
036300010906     C                   Z-ADD     G08DAT        STPDAA
036400010906     C                   ENDIF
036500000505     C*
036600000505     C* CALCOLA LA DATA CORRENTE
036700000505     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
036800000505     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
036900000505     C                   Z-ADD     WN8           G08DAT
037000000505     C                   Z-ADD     *ZEROS        G08INV
037100000505     C                   MOVEL     '0'           G08ERR
037200000505     C                   CALL      'XSRDA8'
037300000505     C                   PARM                    WLBDA8
037400000505     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037500000000     C*
037600000000     C                   ENDSR
037700011119
037800011119     Oprtf198   E            t_listing         2
037900011119     O                                              'Controllo LIVELLI GESTIONE'
038000011119     O          E            d_listing   1
038100011119     O                       wrktesto
038200010905** CMD - COMANDI CL
038300010906OVRDBF FILE(AZCPLXXL) TOFILE(AZCPL01L)
038400010906DLTOVR FILE(AZCPLXXL) LVL(*)
