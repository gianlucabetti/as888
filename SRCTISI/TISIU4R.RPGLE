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
013700140924     C* IGNORO LE LOCALITA CON TIPO TEMPO "1000"
013800140924     C                   IF        i_cplttc > 1000
013900140924     C                   ELSE
014000010905     C* VERIFICO L'AVVENUTA ROTTURA DI CODICE
014100010906     C                   IF        i_cplcap <> depCAP
014200140616     C* ESEGUO LA ROUTINE DI CONSIDERAZIONE SU ASSORBIMENTO
014300010905     C                   EXSR      EXEMOD
014400010905     C* SALVO IL CAP CORRENTE
014500010906     C                   MOVEL     i_cplcap      depCAP
014600010905     C                   ENDIF
014700140924     C                   ENDIF
014800010905     C                   ENDIF
014900010905     C                   ENDIF
015000010905     C                   ENDIF
015100010905     C                   ENDDO
015200010905     C*
015300010905     C* ED INFINE STAMPO LA CHIUSURA
015400011119     C                   WRITE     SIU2RI
015500011119     C                   WRITE     SIU2NO
015600011119     C                   WRITE     SIU2RB
015700011119     C                   WRITE     SIU2FI
015800010905     C*
015900010905     C                   ENDSR
016000010905     C*------------------------------------------------------------------------*
016100010905     C*  EXEMOD - ESEGUE LE OPERAZIONI DI MODIFICA/LISTING
016200010905     C*------------------------------------------------------------------------*
016300010905     C     EXEMOD        BEGSR
016400010905     C*
016500010905     C                   MOVEL     *blanks       wrknar            3
016600010905     C                   Z-ADD     *zeros        totASS           18 0
016700010905     C                   Z-ADD     *zeros        maxASS           18 0
016800010905     C                   Z-ADD     *zeros        locASS           18 0
016900010906     C                   Z-ADD     *zeros        tmpASS           18 0
017000010905     C                   MOVEL     *blanks       depLOC           35
017100010906     C                   MOVEL     *blanks       tmpLOC           35
017200010905     C*
017300061211     C     keyala        SETLL     tiala03l
017400061211     C     keyala        READE     tiala03l
017500010905     C*
017600061211     C                   DOW       not %eof(tiala03l)
017700010905     C*
017800061211     C                   ADD       alatos        totass
017900061211     C                   IF        alaloc = tmpLOC
018000061211     C                   ADD       alatos        tmpASS
018100010906     C                   ELSE
018200010906     C                   Z-ADD     tmpASS        locASS
018300061211     C                   Z-ADD     alatos        tmpASS
018400010905     C*
018500010905     C                   IF        locASS > maxASS
018600010905     C                   Z-ADD     locASS        maxASS
018700010906     C                   MOVEL     tmpLOC        depLOC
018800010905     C                   ENDIF
018900061211     C                   MOVEL     alaloc        tmpLOC
019000010906     C                   ENDIF
019100010905     C*
019200061211     C     keyala        READE     tiala03l
019300010905     C                   ENDDO
019400010906     C*
019500010906     C* EFFETTUO LO STESSO TEST ALLA FINE DEL FILE PER VERIFICARE QUANTO RIMASTO "IN CANNA"
019600010906     C                   IF        tmpASS > maxASS
019700010906     C                   Z-ADD     tmpASS        maxASS
019800010906     C                   MOVEL     tmpLOC        depLOC
019900010906     C                   ENDIF
020000010905     C*
020100010905     C* CALCOLO LA PERVCENTUALE DI ASSORBIMENTO DELLA LOCALITA' PRESCELTA SUL TOTALE DEL CAP...
020200010905     C* ...E VALORIZZO I CAMPI IN STAMPA
020300010905     C                   IF        totASS > 0
020400010906     C* INIZIALIZZO IL FORMATO RECORD DI STAMPA
020500011119     C                   CLEAR                   SIU2RA
020600010906     C*
020700010906     C                   EVAL      STPPER = maxASS * 100 / totASS
020800140616     C*
020900140616     C* VERIFICO SE PERCENTUALE ASSORBIMENTO LOCALITA' PRESCELTA > 70% ASSORB. TOTALE CAP
021000140616     C                   IF        STPPER >= 70
021100140616     C*
021200061211     C                   EVAL      STPCAP = alacap
021300061211     C                   EVAL      STPPRV = alaprv
021400010906     C                   EVAL      STPLOC = depLOC
021500140616     C* UNA VOLTA PRESCELTA LA LOCALITA' CANDIDATA REPERISCO DI NUOVO I DATI CPL
021600010906     C     keyxpl        CHAIN     azcplxxl
021700010906     C                   IF        %found(azcplxxl)
021800140616     C*
021900140616     C* CONSIDERO UNICAMENTE LOCALITA' A LIVELLO 'L'
022000140616     C                   IF        cplLIV = 'L'
022100140616     C*
022200010910     C* SE TROVO LA LOCALITA' LEGGO ANCHE IL CAP RELATIVO
022300061211     C     keycpc        CHAIN     azcpc01l
022400010910     C*
022500120522     C                   IF        CPLPis = 'S'
022600120522     C                   EVAL      STPPIS = cplpis
022700120522     C                   ENDIF
022800120522     C                   EVAL      STPISO = cpliso
022900010906     C                   EVAL      STPLNA = cpllna
023000010906     C                   EVAL      STPZNC = cplznc
023100010906     C                   EVAL      STPLOS = cpllos
023200010906     C                   EVAL      STPZOS = cplzos
023300010906     C                   EVAL      STPLOL = cpllol
023400010906     C                   EVAL      STPZOL = cplzol
023500010906     C                   EVAL      STPLFD = cpllna
023600010906     C                   EVAL      STPZFD = cplzfd
023700010906     C                   EVAL      STPLKS = cpllks
023800010906     C                   EVAL      STPLMS = cpllms
023900010906     C                   EVAL      STPLKG = cpllkg
024000010906     C                   EVAL      STPLMC = cpllmc
024100010906     C                   EVAL      STPTTC = cplttc
024200010906     C                   MOVEL     *all'9'       STPTTR
024300010906     C                   MOVE      cplfi2        STPTTR
024400010905     C*
024500010905     C* INFINE LISTO QUANTO ELUCUBRATO
024600010906     C                   IF        *in01 = *on
024700011119     C                   WRITE     SIU2TE
024800010906     C                   SETOFF                                       01
024900010905     C                   ENDIF
025000140616     C*
025100140616     C* EFFETTUO OPERAZIONI DI COMPARAZIONE DATI CAP/LOC BAR
025200140616     C                   EXSR      EXECMP
025300010910     C*
025400011119     C                   WRITE     SIU2RA
025500011119     C*
025600140616     C* SE OK EFFETTUO OPERAZIONI DI AGGIORNAMENTO ARCHIVI E LISTING SE RICHIESTO NEL LANCIO
025700140616     C                   IF        STPNOT='3'
025800011119     C                   IF        PARSIM='N'
025900140616     C                   EXSR      EXEAGGBAR
026000061211     C                   ENDIF
026100011119     C                   EVAL      wrktesto = STPNOT + ' ' + CPCCAP + ' ' +
026200140616     C                                   'aggiornati tempi CAP da località: ' +
026300140616     C                                   STPLOC
026400011119     C                   EXCEPT    d_listing
026500011119     C                   ENDIF
026600140616     C*
026700140616     C                   ENDIF
026800140616     C                   ENDIF
026900140616     C*
027000140616     C                   ENDIF
027100140616     C                   ENDIF
027200010905     C*
027300010905     C                   ENDSR
027400140616     C*------------------------------------------------------------------------*
027500140616     C*  EXECMP - ESEGUE LE OPERAZIONI DI COMPARAZIONE
027600140616     C*------------------------------------------------------------------------*
027700140616     C     EXECMP        BEGSR
027800140616     C*
027900140616     C                   SETOFF                                       27
028000140616     C                   EVAL      STPNOT = *blanks
028100140616     C*
028200140616     C                   IF        cpcTTC = cplTTC AND
028300140616     C                             cpcFI2 = cplFI2
028400140616     C                   ELSE
028500140616     C                   EVAL      STPNOT = '3'
028600140616     C                   ENDIF
028700140616     C*
028800140616     C                   ENDSR
028900011119     C*------------------------------------------------------------------------*
029000011119     C*  EXEAGGBAR - ESEGUE LE OPERAZIONI DI AGGIORNAMENTO SUGLI ARCHIVI BARTOLINI
029100011119     C*------------------------------------------------------------------------*
029200011119     C     EXEAGGBAR     BEGSR
029300011119     C*
029400011119     C* Copio i dati instradamenti dalla località al cap ...
029500011119     C                   Z-ADD     CPLTTC        CPCTTC
029600011119     C                   MOVEL     CPLFI2        CPCFI2
029700011119     C*
029800011119     C* Quindi sfleggo i campi di trasmissione ...
029900011119     C                   MOVEL     *blanks       CPCFTR
030000011119     C                   MOVEL     *blanks       CPLFTR
030100011119     C*
030200011119     C* ... e aggiorno
030300011119     C                   UPDATE    azcpc000
030400011119     C*
030500011119     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700980403     C* *INZSR - OPERAZIONI INIZIALI
030800000000     C*------------------------------------------------------------------------*
030900980403     C     *INZSR        BEGSR
031000000000     C*
031100000000     C* RICEVIMENTO PARAMETRI
031200000000     C     *ENTRY        PLIST
031300000000     C                   PARM                    KPJBA
031400011119     C*
031500011119     C                   MOVEL     kpjbu         param
031600000508     C*
031700000508     C* DEFINISCE LE VARIABILI RIFERITE AL DATABASE
031800010905     C     *LIKE         DEFINE    cpcver        kpcver
031900010905     C     *LIKE         DEFINE    cpcnar        kpcnar
032000010905     C     *LIKE         DEFINE    cpccap        kpccap
032100010905     C     *LIKE         DEFINE    cplver        kplver
032200010905     C     *LIKE         DEFINE    cplnar        kplnar
032300010905     C     *LIKE         DEFINE    cplcap        kplcap
032400010905     C     *LIKE         DEFINE    cplloc        kplloc
032500000508     C*
032600000508     C*------------------------------
032700000508     C* DEFINISCO LE CHIAVI DI ACCESSO INDICIZZATO AI FILES
032800000508     C*------------------------------
032900010905     C* CHIAVE SU AZCPC01L - COMPLETA
033000010906     C     keycpc        KLIST
033100010910     C                   KFLD                    kplver
033200010910     C                   KFLD                    kplnar
033300061211     C                   KFLD                    depCAP
033400010905     C* CHIAVE SU AZCPL01L - COMPLETA
033500010906     C     keycpl        KLIST
033600000508     C                   KFLD                    kplver
033700000508     C                   KFLD                    kplnar
033800000509     C                   KFLD                    kplcap
033900000509     C                   KFLD                    kplloc
034000010906     C* CHIAVE SU AZPPL01L/AZCPLXXL - COMPLETA
034100010906     C     keyxpl        KLIST
034200010906     C                   KFLD                    kplver
034300010906     C                   KFLD                    kplnar
034400010906     C                   KFLD                    depCAP
034500010906     C                   KFLD                    depLOC
034600061211     C* CHIAVE SU TIALA03L - PARZIALE
034700061211     C     keyala        KLIST
034800010905     C                   KFLD                    wrknar
034900010905     C                   KFLD                    depCAP
035000010906     C*
035100010906     C* REPERISCO LE DATE DI RIFERIMENTO ARCHIVIO ASSORBIMENTI
035200061211     C                   READ(e)   tiala03l
035300061211     C                   IF        not %eof(tiala03l)
035400010906     C* GIRO LA DATA INIZIALE
035500010906     C                   Z-ADD     *ZEROS        G08DAT
035600061211     C                   Z-ADD     aladai        G08INV
035700010906     C                   MOVEL     '3'           G08ERR
035800010906     C                   CALL      'XSRDA8'
035900010906     C                   PARM                    WLBDA8
036000010906     C                   Z-ADD     G08DAT        STPDDA
036100010906     C* GIRO LA DATA FINALE
036200010906     C                   Z-ADD     *ZEROS        G08DAT
036300061211     C                   Z-ADD     aladaf        G08INV
036400010906     C                   MOVEL     '3'           G08ERR
036500010906     C                   CALL      'XSRDA8'
036600010906     C                   PARM                    WLBDA8
036700010906     C                   Z-ADD     G08DAT        STPDAA
036800010906     C                   ENDIF
036900000505     C*
037000000505     C* CALCOLA LA DATA CORRENTE
037100000505     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
037200000505     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
037300000505     C                   Z-ADD     WN8           G08DAT
037400000505     C                   Z-ADD     *ZEROS        G08INV
037500000505     C                   MOVEL     '0'           G08ERR
037600000505     C                   CALL      'XSRDA8'
037700000505     C                   PARM                    WLBDA8
037800000505     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
037900000000     C*
038000000000     C                   ENDSR
038100011119
038200011119     Oprtf198   E            t_listing         2
038300011119     O                                              'Controllo LIVELLI GESTIONE'
038400011119     O          E            d_listing   1
038500011119     O                       wrktesto
038600010905** CMD - COMANDI CL
038700010906OVRDBF FILE(AZCPLXXL) TOFILE(AZCPL01L)
038800010906DLTOVR FILE(AZCPLXXL) LVL(*)
