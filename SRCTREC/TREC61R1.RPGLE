000100000000     F*------------------------------------------------------------------------*
000200000000     F* DATA BASE
000300000000     F*------------------------------------------------------------------------*
000400140114     FECEVD06L  UF A E           K DISK
000500010111     D*------------------------------------------------------------------------*
000600010111     D* SCHIERE
000700010111     D*------------------------------------------------------------------------*
000800010111     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
000900010111     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* INPUT
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
001500000000     D*-------------------
001600000000     D                SDS
001700000000     D  USER                 254    263
001800000000     D*---------------------
001900000000     D* DS ESTERNA DI PROCEDURA
002000000000     D*---------------------
002100000000     D DS60          E DS                  EXTNAME(TREC60DS)
002200010111     D DS61          E DS                  EXTNAME(TREC61DS)
002300000000     D*-------------------
002400000000     D* ARCHITETTURA
002500000000     D*-------------------
002600000000     D KPJBA         E DS
002700000000     D*------------------
002800000000     D* CALL XSRDA8 - CONTROLLO DATE
002900000000     D*------------------
003000000000     D WLBDAT          DS                  INZ
003100000000     D  G02DAT                 1      8  0
003200000000     D  G02INV                 9     16  0
003300000000     D  G02ERR                17     17
003400000000     D  G02TGI                18     22  0
003500000000     C*------------------------------------------------------------------------*
003600000000     C* MAIN LINE
003700000000     C*------------------------------------------------------------------------*
003800000000     C*
003900010110     C* VEERIFICO INNANZITUTTO L'OPZIONE IN ENTRATA
004000010110     C                   EXSR      EXEOPE
004100010110     C*
004200000000     C                   SETON                                        LR
004300000000     C*------------------------------------------------------------------------*
004400010110     C* EXEOPE - VERIFICA E LANCIO OPERAZIONE DELL'OPZIONE RICHIESTA IN ENTRATA
004500000000     C*------------------------------------------------------------------------*
004600010110     C     EXEOPE        BEGSR
004700010112     C*
004800010112     C* INIZIALIZZO FLAG DUI ERRORE
004900010112     C                   MOVEL     '0'           D60ERR
005000000000     C*
005100000000     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
005200010111     C     D60OP1        IFEQ      'O03'
005300010110     C                   EXSR      DUPLIC
005400010111     C                   ENDIF
005500010112     C     D60OP1        IFEQ      'CHK'
005600010112     C                   EXSR      MODCHK
005700010111     C                   ENDIF
005800010112     C     D60OP1        IFEQ      'O02'
005900010112     C                   EXSR      MODEXE
006000010112     C                   ENDIF
006100000000     C*
006200000000     C                   ENDSR
006300000000     C*------------------------------------------------------------------------*
006400010112     C* MODCHK - FUNZIONE DI CONTROLLO PER MODIFICA
006500000000     C*------------------------------------------------------------------------*
006600010112     C     MODCHK        BEGSR
006700000000     C*
006800010112     C* ESEGUO UN PRIMO GIRO DI VERIFICA CONGRUITA' DELLE DATE PER CUI...
006900010112     C*
007000010112     C* ... COSTRUISCO LA CHIAVE
007100010112     C                   Z-ADD     I61VOC        KVDVOC
007200010112     C                   Z-ADD     I61PRG        KVDPRG
007300010112     C*
007400010112     C* ... ESEGUO IL POSIZIONAMENTO E CICLO FINO A FINE O FINO A PRIMA NON OK
007500140114     C     KEYEVD        SETLL     ECEVD06L                           91
007600140114     C     KEYEVD        READE     ECEVD06L                               91
007700010112     C     *IN91         DOWEQ     *OFF
007800010112     C     EVDATB        IFEQ      *BLANKS
007900010112     C     EVDDDT        ANDGE     I61DAF
008000010112     C                   MOVEL     '1'           D60ERR
008100010112     C                   LEAVE
008200010112     C                   ENDIF
008300140114     C     KEYEVD        READE     ECEVD06L                               91
008400010112     C                   ENDDO
008500010112     C*
008600000000     C                   ENDSR
008700010112     C*------------------------------------------------------------------------*
008800010112     C* MODEXE - FUNZIONE DI MODIFICA
008900010112     C*------------------------------------------------------------------------*
009000010112     C     MODEXE        BEGSR
009100010112     C*
009200010112     C* ESEGUO IL SECONDO GIRO PER ESEGUIRE LE MODFICHE PER CUI...
009300010112     C*
009400010112     C* ... COSTRUISCO LA CHIAVE
009500010112     C                   Z-ADD     I61VOC        KVDVOC
009600010112     C                   Z-ADD     I61PRG        KVDPRG
009700010112     C*
009800010112     C* ... ESEGUO IL POSIZIONAMENTO E CICLO FINO A FINE VOCE/PROG
009900140114     C     KEYEVD        SETLL     ECEVD06L                           91
010000140114     C     KEYEVD        READE     ECEVD06L                               91
010100010112     C     *IN91         DOWEQ     *OFF
010200010112     C     EVDATB        IFEQ      *BLANKS
010300010112     C     EVDDST        ANDGT     I61DAF
010400010112     C                   Z-ADD     I61DAF        EVDDST
010500010808     C                   Z-ADD     DATCOR        EVDDUV
010600010808     C                   UPDATE    ECEVD000
010700010112     C                   ENDIF
010800140114     C     KEYEVD        READE     ECEVD06L                               91
010900010112     C                   ENDDO
011000010112     C*
011100010112     C                   ENDSR
011200010110     C*------------------------------------------------------------------------*
011300010111     C* DUPLIC - FUNZIONE DI DUPLICAZIONE
011400010110     C*------------------------------------------------------------------------*
011500010110     C     DUPLIC        BEGSR
011600010110     C*
011700010110     C* COSTRUISCO LA CHIAVE
011800010111     C                   Z-ADD     O61VOC        KVDVOC
011900010111     C                   Z-ADD     O61PRG        KVDPRG
012000010110     C*
012100010111     C* CONTROLLO PREV. CHE NON VI SIANO GIA' TARIFFE PER LA VOCE RICHIESTA
012200140114     C     KEYEVD        CHAIN     ECEVD06L                           91
012300010110     C     *IN91         IFEQ      *ON
012400010111     C* SE NON ESISTONO => ESEGUO LA RICHIESTA
012500010111     C*
012600010111     C* PER CUI COSTRUISCO LA CHIAVE (DA CUI LEGGERE)...
012700010111     C                   Z-ADD     I61VOC        KVDVOC
012800010111     C                   Z-ADD     I61PRG        KVDPRG
012900010110     C*
013000010111     C* ESEGUO IL POSIZIONAMENTO E CICLO...
013100140114     C     KEYEVD        SETLL     ECEVD06L                           91
013200140114     C     KEYEVD        READE     ECEVD06L                               91
013300010111     C     *IN91         DOWEQ     *OFF
013400010111     C     EVDATB        IFEQ      *BLANKS
013500010111     C     EVDDDT        ANDLE     I61DRI
013600010111     C     EVDDST        ANDGE     I61DRI
013700010111     C                   Z-ADD     O61VOC        EVDVOC
013800010111     C                   Z-ADD     O61PRG        EVDPRG
013900010111     C                   Z-ADD     I61DAI        EVDDDT
014000010111     C                   Z-ADD     I61DAF        EVDDST
014100010115     C                   MOVEL     *BLANKS       EVDFTR
014200010115     C                   Z-ADD     *ZEROS        EVDDTR
014300010808     C                   Z-ADD     DATCOR        EVDDIR
014400010813     C                   Z-ADD     DATCOR        EVDDIR
014500010813     C                   Z-ADD     *ZEROS        EVDDUV
014600010808     C                   WRITE     ECEVD000
014700010111     C                   ENDIF
014800140114     C     KEYEVD        READE     ECEVD06L                               91
014900010111     C                   ENDDO
015000010111     C                   ELSE
015100010111     C                   MOVEL     '1'           D60ERR
015200010111     C                   MOVEL     MSG(1)        D60MSG
015300010111     C                   ENDIF
015400010110     C*
015500010110     C                   ENDSR
015600000000     C*------------------------------------------------------------------------*
015700000000     C*  *INZSR - ROUTINE INIZIALE
015800000000     C*------------------------------------------------------------------------*
015900000000     C     *INZSR        BEGSR
016000000000     C*
016100000000     C* RICEVIMENTO PARAMETRI
016200000000     C     *ENTRY        PLIST
016300010111     C                   PARM                    DS60
016400010111     C                   PARM                    DS61
016500000000     C*
016600000000     C* DEFINIZIONE VARIABILI
016700000000     C     *LIKE         DEFINE    EVDVOC        KVDVOC
016800000000     C     *LIKE         DEFINE    EVDPRG        KVDPRG
016900000000     C*
017000140114     C* CHIAVE LETTURA ECEVD06L - PARZIALE
017100000000     C     KEYEVD        KLIST
017200000000     C                   KFLD                    KVDVOC
017300000000     C                   KFLD                    KVDPRG
017400010808     C*
017500010808     C* IMPOSTA DATA E ORA CORRENTE
017600010808     C                   TIME                    N14              14 0
017700010808     C                   MOVE      N14           N8                8 0
017800010808     C                   Z-ADD     N8            G02DAT
017900010808     C                   Z-ADD     *ZEROS        G02INV
018000010808     C                   MOVEL     '0'           G02ERR
018100010808     C                   CALL      'XSRDA8'
018200010808     C                   PARM                    WLBDAT
018300010808     C                   Z-ADD     G02INV        DATCOR            8 0
018400010808     C                   MOVE      00            DATCO0            8 0
018500000000     C*
018600000000     C                   ENDSR
018700010111** ======== SCHIERA: MSG  (MESSAGGI)                       ================
018800010111TREC61R1 - ERRORE esistono già tariffe per la nuova voce richiesta!!!          1
0189000101112
0190000101113
0191000101114
0192000101115
