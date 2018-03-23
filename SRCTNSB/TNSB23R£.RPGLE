000100941102     H*------------------------------------------------------------------------*
000200941122     H* TRASFERIMENTO BOLLE DA FILE CORRENTE A STORICO                         *
000300941102     H*------------------------------------------------------------------------*
000400971209     H*------------------------------------------------------------------------*
000500971209     H*------------------------------------------------------------------------*
000600971209     H*-----------    NON RICOMPILARE MAI QUESTO PGM MENTRE     ---------------*
000700971209     H*-----------    STA GIRANDO: I PGM DEL GUFO TESTANO SE    ---------------*
000800971209     H*-----------    PGM = TNSB23R MA SE RICOMPILATO IL NOME   ---------------*
000900971209     H*-----------    DEL PGM CAMBIA IN QRPL..                  ---------------*
001000971209     H*------------------------------------------------------------------------*
001100971209     H*------------------------------------------------------------------------*
001200971209     H*------------------------------------------------------------------------*
001300971127     H DECEDIT('0,') DATEDIT(*DMY.)
001400030502     F*TISAV00L  O    E           K DISK
001500990802     FTITAS10F  O  A E           K DISK
001600990802     FTITAS38C  UF   E           K DISK
001700990802     F                                     IGNORE(TITASP00)
001800990802     F                                     IGNORE(TITAS010)
001900020619     Ftitasque  O    E             DISK    USROPN
002000971127     FQSYSPRT   O    F  132        PRINTER
002100950518     D* COMANDI
002200950518     D CMD1            S             45    DIM(1) CTDATA PERRCD(1)              QCAEXEC
002300950705     D CMD2            S             45    DIM(1) CTDATA PERRCD(1)              QCAEXEC
002400000000     D KPJBA         E DS
002500020906     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002600980130     D* DATA AREA PER DATI VARI SALDI
002700980130     D TREC00        E DS
002800950717     D DS§DAT          DS
002900950717     D  DSSEC                  1      2  0
003000950717     D  DSANN                  3      4  0
003100950717     D  DSMES                  5      6  0
003200950717     D  DSGIO                  7      8  0
003300950717     D  DSAAS                  1      4  0
003400950717     D  DSMGS                  5      8  0
003500950717     D  DSDAT6                 3      8  0
003600950717     D  DSDAT8                 1      8  0
003700950717     D DS§TIM          DS
003800950717     D  DTORA                  1      2  0
003900950717     D  DTMIN                  3      4  0
004000950717     D  DTSCN                  5      6  0
004100950717     D  DTGIO                  7      8  0
004200950717     D  DTMES                  9     10  0
004300950717     D  DTSEC                 11     12  0
004400950717     D  DTANN                 13     14  0
004500950717     D  DTORA6                 1      6  0
004600950717     D  DTTIME                 1     14  0
004700991223     D* DS COMPOSIZIONE DATA SPEDIZIONE
004800991223     D                 DS
004900991223     D  TASAAS                 1      4  0
005000991223     D  TASMGS                 5      8  0
005100991223     D  TASDSP                 1      8  0
005200991223     D* PARAMETRI DI LANCIO
005300991223     D PARAM           DS
005400991223     D  PARDAT                 1      8  0
005500020619      * DS lancio scrittura come trigger
005600020619     D DS£ECO        E DS
005700020619      * Informazioni lavoro
005800020619     d                SDS
005900020619     d  JOB                  244    253                                         *lavoro
006000020619     d  USER                 254    263                                         *utente
006100011002      * Variabili a programma
006200011002     d n6              S              6  0                                      *numerico 14
006300011002     d $lenght         S             15  5 INZ(80)                              *comando -lunghezza-
006400011002     d $command        S             80                                         *comando -contenuto-
006500020618      * API per reperire numero record
006600020618     d RMbrD01         S                   LIKE(QUSM0200)                       Receiver variable
006700020618     d RMbrD02         S              9B 0 INZ(%SIZE(RMbrD01))                  Length of receiver v
006800020618     d RMbrD03         S              8    INZ('MBRD0200')                      Format name
006900020618     d RMbrD04         S             20    INZ('TITASQUE  *LIBL     ')          Qualified database f
007000020618     d RMbrD05         S             10    INZ('*FIRST')                        Database member name
007100020618     d RMbrD06         S              1    INZ('0')                             Override processing
007200020618
007300020618      /COPY QSYSINC/QRPGLESRC,QUSRMBRD
007400020618      /COPY QSYSINC/QRPGLESRC,QUSEC
007500020618      *
007600941102     C*------------------------------------------------------------------------*
007700941102     C* MAIN LINE
007800941102     C*------------------------------------------------------------------------*
007900000000     C     INIZIO        TAG
008000941102     C*
008100990802     C     *LOVAL        SETLL     TITAS000
008200000000     C     LOOP          TAG
008300990802     C                   READ      TITAS000                               21
008400941203     C   21              GOTO      FINE
008500941102     C*
008600941102     C* ESCLUDO QUELLI SENZA DATA DI FATTURA E NON CONTABILIZZATE
008700000000     C     TASDFT        CABEQ     0             LOOP
008800941102     C     TASFIC        CABEQ     *BLANKS       LOOP
008900991223     C     TASDSP        CABGT     PARDAT        LOOP
009000030502     C                   SETOFF                                       40
009100030502     C*---               SETOFF                                       50
009200030502     C*---               WRITE     TISAV000                             50
009300030502     C*--50              EXCEPT    ERRSAV
009400030502     C*--50              GOTO      LOOP
009500941203     C*
009600990802     C                   DELETE    TITAS000
009700941203     C*
009800990802     C                   WRITE     TITAS010                             40
009900000000     C   40              EXCEPT    ERRBOL
010000981006     C   40              GOTO      FINE                                         *ERRORE -> ESCE
010100950518     C*
010200020618     C                   EXSR      RTVRCDTASQUE                                 *CONTR.REC IN TASQUE
010300950518     C                   ADD       1             NUMBOL
010400000000     C*
010500980326     C                   SHTDN                                        98        *RICH.CHIUSURA PGM
010600980326     C  N98              GOTO      LOOP
010700941102     C*
010800000000     C     FINE          TAG
010900020619     C*
011000020619     C* Esegue operazioni finali
011100121207     C*                  EXSR      FinSr
011200020619     C*
011300950517     C* STAMPA RIGA DI FINE STAMPA
011400950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
011500950517     C                   EXCEPT    STAFIN
011600950517     C*
011700000000     C                   SETON                                        LR
011800020618     C*------------------------------------------------------------------------*
011900020618     C* Reperisce il numero di record in TITASQUE
012000020618     C*------------------------------------------------------------------------*
012100020618     C     RTVRCDTASQUE  BEGSR
012200020618     C*
012300020618     C* Ogni tot bolle conta quanto è pieno TITASQUE
012400020618     C                   ADD       1             NUMDTA
012500020618IF  1C     NUMDTA        IFGT      1000                                         *OGNI 1000 BOLLE
012600020618     C                   CLEAR                   QUSEC
012700020618     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
012800020618     C                   CALL      'QUSRMBRD'
012900020618     C     QUSM0200      PARM      *BLANK        RMbrD01
013000020618     C                   PARM                    RMbrD02
013100020618     C                   PARM                    RMbrD03
013200020618     C                   PARM                    RMbrD04
013300020618     C                   PARM                    RMbrD05
013400020618     C                   PARM                    RMbrD06
013500020618     C                   PARM                    QUSEC
013600020618     C                   Z-ADD     QUSNCRU       n6                             *N° record TASQUE
013700020618     C*
013800020618     C* Ogni tot bolle si ferma tot secondi -molto piena-
013900020618if  2c     n6            IFGT      10000
014000020618     C                   Z-ADD     45            LUNG             15 5
014100020618     C                   MOVEL     *BLANKS       COMMAN
014200020618     C                   MOVEA     CMD2(1)       COMMAN           80            *Attende 30 secondi
014300020618     C                   CALL      'QCMDEXC'
014400020618     C                   PARM                    COMMAN
014500020618     C                   PARM                    LUNG
014600101206     C                   Z-ADD     600           NUMDTA                         *check dopo 200 sp
014700020618x   2c                   ELSE
014800020618     C*
014900020618     C* Ogni tot bolle si ferma tot secondi -meno piena-
015000020618if  3C     n6            IFGT      1000
015100020618     C                   Z-ADD     45            LUNG             15 5
015200020618     C                   MOVEL     *BLANKS       COMMAN
015300020618     C                   MOVEA     CMD1(1)       COMMAN           80            *Attende 1 secondo
015400020618     C                   CALL      'QCMDEXC'
015500020618     C                   PARM                    COMMAN
015600020618     C                   PARM                    LUNG
015700101206     C                   Z-ADD     600           NUMDTA                         *check dopo 200 sp
015800020618x   3C                   ELSE
015900020618     C                   Z-ADD     *ZEROS        NUMDTA                         RIazzera il check
016000020618e   3C                   ENDIF
016100020618e   2C                   ENDIF
016200020618e   1C                   ENDIF
016300020618     C*
016400020618     C                   ENDSR
016500020619     C*------------------------------------------------------------------------*
016600020619     C* Esegue operazioni finali
016700020619     C*------------------------------------------------------------------------*
016800020619     C     FinSr         BEGSR
016900020619     c*
017000020619     c* apre il file delle scritture dal trigger su TITAS00F/10F
017100020619     C                   OPEN      titasque
017200020619     c*
017300020619     c* riempe a mano la DS che normalmente viene passata piena dal trigger perchè
017400020619     c* si vuole simulare una scrittura di "chiusura" di TITAS10F che invece
017500020619     c* non viene intercettata dal trigger, ma che invece serve perchè con questa
017600020619     c* il conto economico (TRECDrR) rilascia la KECONOM per le statistiche
017700020619     C                   CLEAR                   DS£ECO
017800020619     C                   eval      queflg = *blanks
017900020619     C                   MOVEL     'C'           D£GSTS
018000020619     C                   Z-ADD     UDATE         D£GDAT
018100020619     C                   TIME                    D£GTIM
018200020619     C                   MOVEL     USER          D£GUSR
018300020619     C                   MOVEL     'TNSB23R'     D£GPGF
018400020619     C                   MOVEL     'TITAS10F'    D£GFIL
018500020619     C                   MOVEL     'TITAS10F'    D£GMBR
018600020619     C                   MOVEL     'GAITRAGRU '  D£GLIB
018700020619     c*
018800020619     c* scrive il file
018900020619     C                   EVAL      quedta = DS£ECO
019000020619     C                   WRITE     titasq00
019100020619     c*
019200020619     c* chiude il file
019300020619     C                   CLOSE     titasque
019400020619     c*
019500020619     C                   ENDSR
019600941102     C*------------------------------------------------------------------------*
019700941102     C* *INZSR - ROUTINE INIZIALE
019800941102     C*------------------------------------------------------------------------*
019900941102     C     *INZSR        BEGSR
020000941102     C*
020100941102     C     *ENTRY        PLIST
020200941102     C                   PARM                    KPJBA
020300991223     C                   MOVEL     KPJBU         PARAM
020400991223     C*
020500980130     C     *DTAARA       DEFINE                  TREC00
020600941102     C                   Z-ADD     1             CODUT
020700020830     C                   CALL      'X§PARUT'
020800941102     C                   PARM                    UTEDSE
020900941102     C     CODUT         CABEQ     -1            FINE
021000941102     C                   MOVEL     RAGUT         RSUT             20
021100971127     C*
021200950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
021300950518     C                   Z-ADD     *ZEROS        NUMBOL            9 0
021400950518     C                   Z-ADD     *ZEROS        NUMDTA            9 0
021500990803     C*
021600980130     C* IMPOSTA A 'NO' IL RILASCIO DELLA KECONOM, SARA POI IL TRECD6R (GUFO)
021700980130     C* CHE QUANDO RICEVERA' LA CHIUSURA DI QUESTO PGM RILASCIERA' LA KECONOM
021800980130     C     *LOCK         IN        TREC00
021900980130     C                   MOVEL     'N'           D00RCB
022000980130     C                   OUT       TREC00
022100950517     C*
022200941203     C                   EXCEPT    TESTA
022300011002     C*
022400941102     C                   ENDSR
022500950717     C*------------------------------------------------------------------------*
022600971127     C* SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
022700950717     C*------------------------------------------------------------------------*
022800950717     C     SUBDAT        BEGSR
022900950717     C                   TIME                    DTTIME
023000950717     C                   Z-ADD     DTANN         DSANN
023100950717     C                   Z-ADD     DTMES         DSMES
023200950717     C                   Z-ADD     DTGIO         DSGIO
023300950717     C                   Z-ADD     DTSEC         DSSEC
023400950717     C                   ENDSR
023500941102     O*------------------------------------------------------------------------*
023600941102     O* STAMPA ANOMALIA
023700941102     O*------------------------------------------------------------------------*
023800941203     OQSYSPRT   E            TESTA          1 02
023900941203     O                       RSUT              +  1
024000950517     O                                         +  2 'ERRORI TRASFERIM.'
024100990802     O                                         +  1 'DA TITAS00F A TITAS10F'
024200950517     O                                         +  3 'Data:'
024300950717     O                       DSDAT6            +  1 '  /  /  '
024400950517     O                                         +  3 'Ora:'
024500950717     O                       DTORA6            +  1 '  .  .  '
024600950517     O                                         +  3 '*TNSB23R*'
024700030502     O*---      E            ERRSAV         1
024800030502     O*---                   TASLNP               3
024900030502     O*---                   TASNSP              11
025000030502     O*---                   TASTBL              15
025100030502     O*---                   TASNRS              18
025200030502     O*---                                     +  2 'GIA ESISTENTE SU TISAV'
025300941203     O          E            ERRBOL         1
025400891012     O                       TASLNP               3
025500891012     O                       TASNSP              11
025600891012     O                       TASTBL              15
025700981006     O                       TASNRS              18
025800990802     O                                         +  2 'GIA ESISTENTE SU TITAS'
025900950517     O          E            STAFIN           60
026000950517     O                                           26 '*** FINE STAMPA ***'
026100950518     O                                           50 'BOLLE TRASFERITE:'
026200990901     O                       NUMBOL        Z     60
026300950518     O                                          100 'Data:'
026400950717     O                       DSDAT6             109 '  /  /  '
026500950518     O                                          117 'Ora:'
026600950717     O                       DTORA6             128 '  :  :  '
026700010514** ======== COMANDI                                        ================
026800010514DLYJOB DLY(001)
026900950705** ======== COMANDI                                        ================
027000010514DLYJOB DLY(030)
