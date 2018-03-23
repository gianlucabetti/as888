000100971120     H*------------------------------------------------------------------------*
000200971120     H* TRASMISSIONE CALENDARIO
000300971120     H*------------------------------------------------------------------------*
000400971120     H DECEDIT('0,') DATEDIT(*DMY.)
000500971120     F*------------------------------------------------------------------------*
000600971120     F* DATA BASE
000700971120     F*------------------------------------------------------------------------*
000800971120     FAZCLN01L  UF   E           K DISK
000900971120     FAZCLN00T  O  A E             DISK    USROPN
001000971120     F                                     RENAME(AZCLN000:AZCLNT)
001100971120     D*------------------------------------------------------------------------*
001200971120     D* SCHIERE
001300971120     D*------------------------------------------------------------------------*
001400971120     D CMD             S             80    DIM(4) CTDATA PERRCD(1)              COMANDI CLP
001500971120     D*------------------------------------------------------------------------*
001600971120     D* INPUT
001700971120     D*------------------------------------------------------------------------*
001800971120     D*-------------
001900971120     D* DS TRASMISSIONE
002000971120     D*-------------
002100971120     D DSCMZ         E DS
002200971120     D*-------------
002300971120     D* DS "TIBS55R" - REPERIMENTO LIBRERIE JOB
002400971120     D*-------------
002500020806     D TIBS55DS      E DS
002600971120     D*-------------
002700971120     D* DATI UTENTE
002800020814     D*-------------
002900020814     D CNCR80        E DS
003000020814     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003100971120     D*-------------
003200971120     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300971120     D*-------------
003400971120     D WLBDA8          DS
003500971120     D  G08DAT                 1      8  0
003600971120     D  G08INV                 9     16  0
003700971120     D  G08ERR                17     17
003800971120     D  G08TGI                18     22  0
003900971120     D*-------------
004000971120     D* ARCHITETTURA
004100971120     D*-------------
004200971120     D KPJBA         E DS
004300020806     D*-------------
004400020814     D TIBS56DS      E DS
004500020814     D  skpi                   7    756  0 dim(250)
004600020806     D*----------------
004700020806     D* Variabili a programma
004800020806     D*----------------
004900020806     D wrkTER          S              3  0                                      *terminal di deposit
005000971120     C*------------------------------------------------------------------------*
005100971120     C* MAIN LINE
005200971120     C*------------------------------------------------------------------------*
005300971120     C*
005400971120     C* APERTURA MEMBRO DI TRASMISSIONE
005500971120     C                   EXSR      MEMBRO
005600971120     C*
005700971120     C* LETTRURA RECORD DA TRASMETTERE
005800971120     C                   EXSR      LEGGI
005900971120     C*
006000971120     C* PER TUTTE LE FILIALI MECCANIZZATE
006100971120IF  1C     $REC          IFGT      *ZEROS                                       *REC DA TRASMETTERE
006200020806     C                   Z-ADD     1             C                 5 0
006300020806     C                   MOVEL     *zeros        wrkTER
006400020814do  1C                   DOW       SKPI(C) > *ZEROS
006500020924     C                   IF        SKPI(C) <>  946
006600020814     C                   IF        SKPI(C) <> wrkTER
006700020814     C                   Z-ADD     SKPI(C)       wrkTER
006800971120     C*
006900971120     C* ESEGUE LA TRASMISSIONE
007000971120     C                   EXSR      X25
007100020806     C                   ENDIF
007200020924     C                   ENDIF
007300971120     C                   ADD       1             C
007400971120E   2C                   ENDDO
007500971120E   1C                   ENDIF
007600971120     C* OPERAZIONI FINALI
007700971120     C                   EXSR      FINSR
007800971120     C*
007900971120     C                   SETON                                        LR
008000971120     C*------------------------------------------------------------------------*
008100971120     C* MEMBRO - ESEGUE QCMDEXC SUL MEMBRO DEL FILE DI TRASMISSIONE
008200971120     C*------------------------------------------------------------------------*
008300971120     C     MEMBRO        BEGSR
008400971120     C*
008500971120     C                   Z-ADD     80            LUNG
008600971120     C*
008700971120     C* CONTROLLA ESISTENZA MEMBRO
008800971120     C                   MOVEL     *BLANKS       COMMAN
008900971120     C                   MOVEL     CMD(1)        COMMAN
009000971120     C                   CALL      'QCMDEXC'                            20
009100971120     C                   PARM                    COMMAN           80
009200971120     C                   PARM                    LUNG             15 5
009300971120     C*
009400971120     C* AGGIUNGE IL MEMBRO DI TRASMISSIONE
009500971120DO  1C   20              DO
009600971120     C                   MOVEL     *BLANKS       COMMAN
009700971120     C                   MOVEL     CMD(2)        COMMAN
009800971120     C                   CALL      'QCMDEXC'
009900971120     C                   PARM                    COMMAN
010000971120     C                   PARM                    LUNG
010100971120E   1C                   ENDDO
010200971120     C*
010300971120     C* ESEGUE OVRDBF DEL MEMBRO
010400971120     C                   MOVEL     *BLANKS       COMMAN
010500971120     C                   MOVEL     CMD(3)        COMMAN
010600971120     C                   CALL      'QCMDEXC'
010700971120     C                   PARM                    COMMAN
010800971120     C                   PARM                    LUNG
010900971120     C*
011000971120     C                   ENDSR
011100971120     C*------------------------------------------------------------------------*
011200971120     C* LEGGI -  LETTURA E SCRITTURA FILE TRASMISSIONE
011300971120     C*------------------------------------------------------------------------*
011400971120     C     LEGGI         BEGSR
011500971120     C*
011600971120     C                   OPEN      AZCLN00T
011700971120     C*
011800971120     C     *LOVAL        SETLL     AZCLN01L
011900971120     C                   READ      AZCLN01L                               30
012000971120DO  1C     *IN30         DOWEQ     *OFF
012100971120IF  2C     CLNFTR        IFEQ      *BLANKS                                      *MAI TRASMESSO
012200971120     C*
012300971120     C* SCRIVE IL RECORD DA TRASMETTERE
012400971120     C                   WRITE     AZCLNT                                       *FILE TRASMISSIONE
012500971120     C                   ADD       1             $REC                           *REC DA TRASMETTERE
012600971120     C*
012700971120     C* AGGIORNA IL RECORD TRASMESSO
012800971120     C                   MOVEL     'T'           CLNFTR                         *FLAG TRASMISSIONE
012900971120     C                   Z-ADD     DATCOR        CLNDTR                         *DATA TRASMISSIONE
013000971120     C                   EXCEPT    AGGIOR                                       * ...00F
013100971120X   2C                   ELSE                                                   *GIA' TRASMESSO
013200971120IF  3C     CLNATB        IFNE      *BLANKS                                      *ANNULLATO
013300971120     C                   DELETE    AZCLN000                                     *FILE TRASMISSIONE
013400971120E   3C                   ENDIF
013500971120E   2C                   ENDIF
013600971120     C  N30              READ      AZCLN01L                               30
013700971120E   1C                   ENDDO
013800971120     C*
013900971120     C* CHIUDE IL MEMBRO CREATO
014000971120     C                   CLOSE     AZCLN00T
014100971120     C*
014200971120     C                   ENDSR
014300971120     C*------------------------------------------------------------------------*
014400971120     C* X25 - SOTTOMETTE TRASMISSIONE
014500971120     C*------------------------------------------------------------------------*
014600971120     C     X25           BEGSR
014700020806     C*
014800020806     C* reperimento librerie job
014900020806     C                   CLEAR                   tibs55ds
015000020806     C                   MOVEL     'L'           i50tla
015100020806     C                   MOVEL     knsif         i50psi
015200020924     C                   Z-ADD     simfel        i50ppo
015300020814     C                   MOVEL     SKPI(C)       i50apo
015400020806     C                   CALL      'TIBS55R'
015500020806     C                   PARM                    tibs55ds
015600971120     C*
015700020814     C                   MOVEL     SKPI(C)       CMZDST                          DESTINATARIO
015800971120     C                   MOVEL     'AZCLN00T'    CMZFLD                          DA: >>>
015900971120     C                   MOVEL     *BLANK        CMZMBD                          DAL MEMBRO
016000020806     C                   MOVEL     'M000000'     CMZMBD                           "     "
016100020924     C                   MOVEL     O50PLU        CMZLBD                          DALLA LIBRERIA
016200971120     C                   MOVEL     'AZCLN00R'    CMZFLA                          A: >>>
016300971120     C                   MOVEL     *BLANK        CMZMBA                          AL MEMBRO
016400020806     C                   MOVEL     'M000000'     CMZMBA                           "    "
016500020924     C                   MOVEL     O50ALU        CMZLBA                          ALLA LIBRERIA
016600971120     C                   MOVE      '0000'        CMZORI                          ORA INVIO
016700971120     C                   MOVE      '0000000'     CMZDTI                          DATA INVIO
016800020806     C                   MOVE      'R'           CMZURG                          'T'=NOTTE
016900971120     C                   MOVE      'A'           CMZADD                          'A'=IN AGGIUNTA
017000971120     C                   MOVE      'N'           CMZCLR                          'N'=NO PULIZ.DOPO I
017100971120     C                   MOVE      'S'           CMZFTR                          'S'=INVIO DA TRANSI
017200971120     C                   MOVE      *BLANKS       CMZERR                          1=ERRORE
017300971120     C                   MOVE      'TRC4'        CMZAZN                          AZIONE DI RICEZIONE
017400020806     C                   MOVEL     O50ASI        CMZSYF                          SIST.INFORMATIVO
017500971120     C                   MOVE      $REC          CMZNRR                          NR. RECORD INVIATI
017600971120     C                   MOVE      *BLANKS       CMZPRR                          NO PRE-REQUISITO
017700020806     C                   CALL      'TIBS53C'
017800020806     C                   PARM                    o50lcl
017900971120     C                   PARM                    CMZDST
018000971120     C                   PARM                    CMZFLD
018100971120     C                   PARM                    CMZMBD
018200971120     C                   PARM                    CMZLBD
018300971120     C                   PARM                    CMZFLA
018400971120     C                   PARM                    CMZMBA
018500971120     C                   PARM                    CMZLBA
018600971120     C                   PARM                    CMZORI
018700971120     C                   PARM                    CMZDTI
018800971120     C                   PARM                    CMZURG
018900971120     C                   PARM                    CMZADD
019000971120     C                   PARM                    CMZCLR
019100971120     C                   PARM                    CMZFTR
019200971120     C                   PARM                    CMZERR
019300971120     C                   PARM                    CMZAZN
019400971120     C                   PARM                    CMZSYF
019500971120     C                   PARM                    CMZNRR
019600971120     C                   PARM                    CMZPRR
019700020806     C                   PARM      *blanks       par236          236
019800971120     C*
019900971120     C                   ENDSR
020000971120     C*------------------------------------------------------------------------*
020100971120     C* CARTAB - CARICA TABELLE OCCORRENTI
020200971120     C*------------------------------------------------------------------------*
020300971120     C     CARTAB        BEGSR
020400020806     C*
020500020806     C* REPERIMENTO TUTTI TERMINAL E RELATIVI P.O.
020600020814     C                   clear                   TIBS56DS
020700020814     C                   eval      i56PPO = simfel
020800020814     C                   eval      i56mod = 'GRU'
020900020814     C                   call      'TIBS56R'
021000020814     C                   parm                    TIBS56DS
021100971120     C*
021200971120     C                   ENDSR
021300971120     C*------------------------------------------------------------------------*
021400971120     C* FINSR - OPERAZIONI FINALI
021500971120     C*------------------------------------------------------------------------*
021600971120     C     FINSR         BEGSR
021700971120     C*
021800971120     C* PULISCE IL MEMBRO CREATO
021900971120     C                   MOVEL     *BLANKS       COMMAN
022000971120     C                   MOVEL     CMD(4)        COMMAN
022100971120     C                   CALL      'QCMDEXC'
022200971120     C                   PARM                    COMMAN
022300971120     C                   PARM                    LUNG
022400971120     C*
022500971120     C                   ENDSR
022600971120     C*------------------------------------------------------------------------*
022700971120     C* *INZSR - IMPOSTAZIONI INIZIALI
022800971120     C*------------------------------------------------------------------------*
022900971120     C     *INZSR        BEGSR
023000971120     C*
023100971120     C* RICEVIMENTO PARAMETRI
023200971120     C     *ENTRY        PLIST
023300971120     C                   PARM                    KPJBA
023400020814     C*
023500020814     C* REPERIMENTO DATI UTENTE
023600020814     C                   Z-ADD     1             CODUT
023700020814     C                   CALL      'X§PARUT'
023800020814     C                   PARM                    UT§DSE
023900020814     C                   MOVEL     REC80         CNCR80
024000971120     C*
024100971120     C* VARIABILI DI CONTROLLO
024200971120     C                   Z-ADD     *ZEROS        $REC              7 0          *N°REC SCRITTI
024300971120     C*
024400971120     C* IMPOSTA LA DATA CORRENTE
024500971120     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
024600971120     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
024700971120     C                   Z-ADD     N8            G08DAT
024800971120     C                   Z-ADD     *ZEROS        G08INV
024900971120     C                   MOVEL     '0'           G08ERR
025000971120     C                   CALL      'XSRDA8'
025100971120     C                   PARM                    WLBDA8
025200971120     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
025300971120     C*
025400971120     C* CARICA TABELLE OCCORRENTI
025500971120     C                   EXSR      CARTAB
025600971120     C*
025700971120     C                   ENDSR
025800971120     O*------------------------------------------------------------------------*
025900971120     O* AGGIORNAMENTO FILE TRASMESSO
026000971120     O*------------------------------------------------------------------------*
026100971120     OAZCLN000  E            AGGIOR
026200971120     O                       CLNFTR
026300971120     O                       CLNDTR
026400971120     O*------------------------------------------------------------------------*
026500971120**  COMANDI CL
026600020806CHKOBJ OBJ(AZCLN00T) OBJTYPE(*FILE) MBR(M000000)
026700020806ADDPFM FILE(AZCLN00T)               MBR(M000000)
026800020806OVRDBF FILE(AZCLN00T)               MBR(M000000)
026900020806CLRPFM FILE(AZCLN00T)               MBR(M000000)
