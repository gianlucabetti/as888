000100000612     H ALWNULL(*USRCTL)
000200000000     H*------------------------------------------------------------------------*
000300011120     H* VERIFICA LIVELLI DI GESTIONE CAP/LOC -Step2-
000400010904     F*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900080414     Fazcpc01l  IF   E           K DISK
001000011119     Fazcpl01l  IF   E           K DISK    PREFIX(i_)
001100011119     Fazcplxxl  UF   E           K DISK    USROPN
001200010906     F                                     RENAME(azcpl000:azcplxxx)
001300011119     Fprtf198   O    f  198        PRINTER
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700991124     D*------------------
001800010905     D* COMANDI
001900011119     D*------------------
002000010905     D CMD             S             80    DIM(2) CTDATA PERRCD(1)
002100000000     D*------------------------------------------------------------------------*
002200000000     D* INPUT
002300000000     D*------------------------------------------------------------------------*
002400000000     D*-------------------
002500000000     D* ARCHITETTURA
002600000000     D*-------------------
002700000000     D KPJBA         E DS
002800011119     D*-------------------
002900011119     D* DS PER PASSAGGIO PARAMETRI A *PGM DI ELABORAZIONE
003000011119     D*-------------------
003100011119     D param           DS
003200011119     D  parver                        5S 0 inz(*zeros)
003300011119     D  parcap                        9    inz(*blanks)
003400011119     D  parsim                        1    inz('N')
003500000509     D*-------------------
003600000509     D* Variabili di lavoro
003700000509     D*-------------------
003800011120     D  STPELA         s             15a   INZ(*blanks)
003900011120     D  STPNOT         s              1a   INZ(*blanks)
004000011120     D  wrktesto       s            198a   INZ(*blanks)
004100000000     C*------------------------------------------------------------------------*
004200000000     C* MAIN LINE
004300000000     C*------------------------------------------------------------------------*
004400000000     C*
004500011119     C                   EXSR      RUNMAIN
004600991118     C*
004700000000     C                   SETON                                        LR
004800000000     C*------------------------------------------------------------------------*
004900011119     C* RUNMAIN - ROUTINE DI ELABORAZIONE
005000000000     C*------------------------------------------------------------------------*
005100011119     C     RUNMAIN       BEGSR
005200010906     C*
005300010906     C* Eseguo le override sul file delle località (azcpl01l -> azcplxxl)
005400010906     C                   Z-ADD     80            LENGH            15 5
005500010906     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005600010906     C                   PARM                    CMD(1)
005700010906     C                   PARM                    LENGH
005800010906     C*
005900010906     C* Dopo l'override apro il file
006000010906     C                   OPEN      azcplxxl
006100991105     C*
006200010905     C* Lancia l'elaborazoine
006300010905     C                   EXSR      EXEOPE
006400010906     C*
006500010906     C* Al termine dell'elaborazione chiudo il file
006600010906     C                   CLOSE     azcplxxl
006700010906     C*
006800010906     C* Cancello le override sul file delle località (azcpl01l -> azcplxxl)
006900010906     C                   Z-ADD     80            LENGH            15 5
007000010906     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007100010906     C                   PARM                    CMD(2)
007200010906     C                   PARM                    LENGH
007300991105     C*
007400000000     C                   ENDSR
007500010905     C*------------------------------------------------------------------------*
007600010905     C*  EXEOPE - ESEGUE L'EFFETTIVA ELABORAZIONE
007700010905     C*------------------------------------------------------------------------*
007800010905     C     EXEOPE        BEGSR
007900010905     C*
008000011119     C* SCORRO TUTTO IL FILE LOCALITA' PER LA VERSIONE (E CAP OPZ.) SCELTA
008100011119     C                   MOVE(p)   PARVER        kplver
008200010905     C                   MOVEL     *blanks       kplnar
008300011119     C                   MOVEL     PARCAP        kplcap
008400010905     C                   MOVEL     *blanks       kplloc
008500010905     C*
008600010905     C* EFFETTUO IL POSIZIONAMENTO INIZIALE
008700010905     C     keyxpl        SETLL     azcpl01l
008800010905     C* CICLO SULL'ARCHIVIO PER CHIAVE UGUALE
008900010905     C                   DOW       not %eof(azcpl01l)
009000010905     C* PROSEGUO SCORRENDO IL FILE
009100010905     C                   READ      azcpl01l
009200010905     C                   IF        not %eof(azcpl01l)
009300070514     C*
009400070514     C* A SUPERAMENTO VERSIONE RICHIESTA A VIDEO => ESCO
009500070514     C                   IF        i_cplver > PARVER
009600070514     C                   LEAVE
009700070514     C                   ELSE
009800070514     C*
009900010905     C* VERIFICO SE E' STATA EFFETTUATA UNA EVENTUALE SELEZIONE ANCHE A LIVELLO DI CAP
010000011119     C                   IF        PARCAP   <> *blanks and
010100011119     C                             i_cplcap <> PARCAP
010200010905     C                   leave
010300010905     C                   ELSE
010400011120     C* VERIFICO IL GRADO DI "UGUAGLIANZA" DEGLI ARCHIVI DEL CAPPARIO CORRELATI
010500011120     C                   EXSR      EXEMOD
010600010905     C                   ENDIF
010700070514     C                   ENDIF
010800010905     C                   ENDIF
010900010905     C                   ENDDO
011000010905     C*
011100010905     C                   ENDSR
011200010905     C*------------------------------------------------------------------------*
011300010905     C*  EXEMOD - ESEGUE LE OPERAZIONI DI MODIFICA/LISTING
011400010905     C*------------------------------------------------------------------------*
011500010905     C     EXEMOD        BEGSR
011600010905     C*
011700061211     C* UNA VOLTA PRESCELTA LA LOCALITA' CANDIDATA REPERISCO DI NUOVO I DATI CPL
011800010906     C     keyxpl        CHAIN     azcplxxl
011900010906     C                   IF        %found(azcplxxl)
012000061211     C*
012100010910     C* SE TROVO LA LOCALITA' LEGGO ANCHE IL CAP RELATIVO
012200011119     C     keyxpc        CHAIN     azcpc01l
012300061211     C                   IF        %found(azcpc01l)
012400011120     C*
012500011120     C* EFFETTUO OPERAZIONI DI COMPARAZIONE DATI CAP/LOC BAR/PT
012600011120     C                   EXSR      EXECMP
012700061211     C                   ENDIF
012800010906     C                   ENDIF
012900011119     C*
013000011119     C* EFFETTUO OPERAZIONI DI AGGIORNAMENTO ARCHIVI E LISTING SE RICHIESTO NEL LANCIO
013100011119     C                   IF        STPNOT='1'
013200011120     C                   IF        PARSIM='N'
013300011119     C                   EXSR      EXEAGGBAR
013400011120     C                   EVAL      STPELA = 'AGGIORNAMENTO'
013500011120     C                   ELSE
013600011120     C                   EVAL      STPELA = 'SIMULAZIONE'
013700011120     C                   ENDIF
013800011120     C                   EVAL      wrktesto = STPELA + ' ' + STPNOT + ' ' +
013900011120     C                                        CPCCAP + ' ' +
014000080414     C                                        'portata località: ' +
014100080414     C                                        CPLLOC + ' a livello C'
014200011119     C                   EXCEPT    d_listing
014300011119     C                   ENDIF
014400010905     C*
014500010905     C                   ENDSR
014600061211     C*------------------------------------------------------------------------*
014700061211     C*  EXECMP - ESEGUE LE OPERAZIONI DI COMPARAZIONE
014800061211     C*------------------------------------------------------------------------*
014900061211     C     EXECMP        BEGSR
015000061211     C*
015100061211     C* INIZIALIZZO IL FLAG DI LIVELLO "UGUAGLIANZA" ARCHIVI CAPPARIO
015200061211     C                   MOVEL     *blanks       STPNOT            1
015300061211     C*
015400061211     C                   IF        cpllna = cpclna AND
015500061211     C                             cplznc = cpcznc AND
015600061211     C                             cpliso = cpciso AND
015700061211     C                             cplzfd = cpczfd AND
015800061211     C                             cpllkg = cpclkg AND
015900061211     C                             cpllmc = cpclmc AND
016000061211     C                             cpllol = cpclol AND
016100061211     C                             cplzol = cpczol AND
016200061211     C                             cpllks = cpclks AND
016300061211     C                             cpllms = cpclms AND
016400061211     C                             cpllos = cpclos AND
016500061211     C                             cplzos = cpczos AND
016600061211     C                             cpllk2 = cpclk2 AND
016700061211     C                             cpllm2 = cpclm2 AND
016800061211     C                             cpllo2 = cpclo2 AND
016900061211     C                             cplzo2 = cpczo2 AND
017000061211     C                             cplcts = cpccts AND
017100061211     C                             cplcte = cpccte AND
017200061211     C                             cplttc = cpcttc AND
017300061211     C                             cplpii = cpcpii AND
017400061211     C                             cplpid = cpcpid AND
017500061211     C                             cplpis = cpcpis AND
017600061211     C                             cplgdd = cpcgdd AND
017700080414     C                             cplfi1 = cpcfi1 AND
017800061211     C                             cplfi2 = cpcfi2
017900061211     C                   EVAL      STPNOT = '1'
018000061211     C*
018100061211     C                   ELSE
018200061211     C*
018300061211     C                   EVAL      STPNOT = '2'
018400061211     C*
018500061211     C                   ENDIF
018600061211     C*
018700061211     C                   ENDSR
018800011119     C*------------------------------------------------------------------------*
018900011119     C*  EXEAGGBAR - ESEGUE LE OPERAZIONI DI AGGIORNAMENTO SUGLI ARCHIVI BARTOLINI
019000011119     C*------------------------------------------------------------------------*
019100011119     C     EXEAGGBAR     BEGSR
019200011119     C*
019300080414     C* ... azzero i dati sulla località ...
019400011119     C
019500011119     C                   Z-ADD     *ZEROS        CPLLNA
019600011119     C                   Z-ADD     *ZEROS        CPLLOL
019700011119     C                   Z-ADD     *ZEROS        CPLLO2
019800011119     C                   Z-ADD     *ZEROS        CPLLOS
019900011119     C                   Z-ADD     *ZEROS        CPLZNC
020000011119     C                   Z-ADD     *ZEROS        CPLZOL
020100011119     C                   Z-ADD     *ZEROS        CPLZO2
020200011119     C                   Z-ADD     *ZEROS        CPLZOS
020300011119     C                   Z-ADD     *ZEROS        CPLZFD
020400011119     C                   Z-ADD     *ZEROS        CPLLKG
020500011119     C                   Z-ADD     *ZEROS        CPLLMC
020600011119     C                   Z-ADD     *ZEROS        CPLLKS
020700011119     C                   Z-ADD     *ZEROS        CPLLMS
020800011119     C                   Z-ADD     *ZEROS        CPLLK2
020900011119     C                   Z-ADD     *ZEROS        CPLLM2
021000011119     C                   MOVEL     *BLANKS       CPLCTS
021100011119     C                   MOVEL     *BLANKS       CPLCTE
021200011119     C                   MOVEL     *BLANKS       CPLISO
021300011119     C                   MOVEL     *BLANKS       CPLPII
021400011119     C                   MOVEL     *BLANKS       CPLPID
021500011119     C                   MOVEL     *BLANKS       CPLPIS
021600011119     C                   MOVEL     *BLANKS       CPLGDD
021700011119     C                   Z-ADD     *ZEROS        CPLTTC
021800011119     C                   MOVEL     *BLANKS       CPLFI2
021900011119     C*
022000011119     C* ... che diventa gestita a livello di cap
022100011119     C                   MOVEL     'C'           CPLLIV
022200011119     C*
022300011119     C* Quindi sfleggo i campi di trasmissione ...
022400011119     C                   MOVEL     *blanks       CPCFTR
022500011119     C                   MOVEL     *blanks       CPLFTR
022600011119     C*
022700011119     C* ... e aggiorno
022800011120     C                   UPDATE    azcplxxx
022900011119     C*
023000011119     C                   ENDSR
023100000000     C*------------------------------------------------------------------------*
023200980403     C* *INZSR - OPERAZIONI INIZIALI
023300000000     C*------------------------------------------------------------------------*
023400980403     C     *INZSR        BEGSR
023500000000     C*
023600000000     C* RICEVIMENTO PARAMETRI
023700000000     C     *ENTRY        PLIST
023800000000     C                   PARM                    KPJBA
023900011119     C*
024000011119     C                   MOVEL     kpjbu         param
024100000508     C*
024200000508     C* DEFINISCE LE VARIABILI RIFERITE AL DATABASE
024300010905     C     *LIKE         DEFINE    cpcver        kpcver
024400010905     C     *LIKE         DEFINE    cpcnar        kpcnar
024500010905     C     *LIKE         DEFINE    cpccap        kpccap
024600010905     C     *LIKE         DEFINE    cplver        kplver
024700010905     C     *LIKE         DEFINE    cplnar        kplnar
024800010905     C     *LIKE         DEFINE    cplcap        kplcap
024900010905     C     *LIKE         DEFINE    cplloc        kplloc
025000000508     C*
025100000508     C*------------------------------
025200000508     C* DEFINISCO LE CHIAVI DI ACCESSO INDICIZZATO AI FILES
025300000508     C*------------------------------
025400010905     C* CHIAVE SU AZCPC01L - COMPLETA
025500010906     C     keycpc        KLIST
025600010910     C                   KFLD                    kplver
025700010910     C                   KFLD                    kplnar
025800010910     C                   KFLD                    kplcap
025900010905     C* CHIAVE SU AZCPL01L - COMPLETA
026000010906     C     keycpl        KLIST
026100000508     C                   KFLD                    kplver
026200000508     C                   KFLD                    kplnar
026300000509     C                   KFLD                    kplcap
026400000509     C                   KFLD                    kplloc
026500061211     C* CHIAVE SU AZCPLXXL - COMPLETA
026600010906     C     keyxpl        KLIST
026700010906     C                   KFLD                    kplver
026800010906     C                   KFLD                    kplnar
026900011120     C                   KFLD                    i_cplcap
027000011120     C                   KFLD                    i_cplloc
027100061211     C* CHIAVE SU AZCPC01L - COMPLETA
027200010910     C     keyxpc        KLIST
027300010910     C                   KFLD                    kplver
027400010910     C                   KFLD                    kplnar
027500011120     C                   KFLD                    i_cplcap
027600000000     C*
027700000000     C                   ENDSR
027800011119
027900011119     Oprtf198   E            t_listing         2
028000011119     O                                              'Controllo LIVELLI GESTIONE'
028100011119     O          E            d_listing   1
028200011119     O                       wrktesto
028300010905** CMD - COMANDI CL
028400010906OVRDBF FILE(AZCPLXXL) TOFILE(AZCPL01L)
028500010906DLTOVR FILE(AZCPLXXL) LVL(*)
