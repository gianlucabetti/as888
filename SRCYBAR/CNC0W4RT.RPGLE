000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300930510      * CNC0V4RT                                                     *
000400930510      *        TRASMISSIONE                                          *
000500911030      *        CHIUSURA PER OPERATORE                                *
000600900606?     *--------------------------------------------------------------*
000700940224      * 01 -ON--> PRIMO LIVELLO/OFF --> SECONDO LIVELLO              *
000800940224      * 02 -ON--> SIMFEL UGUALE A 0                                  *
000900940224?     *--------------------------------------------------------------*
001000930521     FQSYSPRT   O    F  132        PRINTER USROPN
001100971121?     *--------------------------------------------------------------*
001200910708     D CNCR80        E DS
001300911104     D DSCMZ         E DS
001400900514     D KPJBA         E DS
001500900514     D  LIBSYS                92    100
001600900516      *
001700900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800020805     d tibs55ds      E DS
001900020805
002000020920     d   §membro       ds
002100020920     d  §emme                  1      1    inz('M')
002200020920     d  §fle                   2      4s 0
002300020920     d  §sed                   5      7s 0 inz(046)
002400020920     d  §mbr                   2      7
002500971121?     *--------------------------------------------------------------*
002600930521     C                   OPEN      QSYSPRT
002700930510      *  CONTROLLO SE HO RECORD DA TRASMETTERE
002800930510     C                   EXSR      TSTOBJ
002900971121      *  REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE MOV-C/ASS
003000971121     C                   EXSR      DATMOV
003100911104      *  TRASMISSIONE CONTRASSEGNI INCASSATI
003200911104     C     NRARC         IFNE      *ZEROS
003300911104     C                   EXSR      X25ARC
003400911104     C                   END
003500991018     C*  TRASMISSIONE     FNMOV00T
003600911104     C     NRMOV         IFNE      *ZEROS
003700911104     C                   EXSR      X25MOV
003800911104     C                   END
003900911105      * STAMPO SEGNALAZIONE CHE CHIUSURA è OK
004000930513     C     NRARC         IFNE      *ZEROS
004100930513     C     NRMOV         ORNE      *ZEROS
004200911105     C                   EXCEPT    FINNMI
004300931220     C                   ELSE
004400931220     C                   EXCEPT    FINNMN
004500930513     C                   END
004600930510     C     FINE          TAG
004700910521     C                   SETON                                        LR
004800930426      *----------------------------------------------------------------
004900930426      * PULIZIA PARAMETRI X TRUL51C
005000930426      *----------------------------------------------------------------
005100930426     C     TRUL51        BEGSR
005200930426     C                   MOVEL     KNSIF         LIBR             10
005300930426     C                   MOVEL     *BLANK        FILE             10
005400930426     C                   MOVEL     *BLANK        MBR              10
005500930426     C                   Z-ADD     0             NRR              10 0
005600930426     C                   ENDSR
005700971121      *----------------------------------------------------------------
005800971121      * REPERISCO DATI DA IMPOSTARE IN TRASMISSIONE
005900971121      *----------------------------------------------------------------
006000971121     C     DATMOV        BEGSR
006100971121     C*
006200020805     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
006300971121     C* da/a cui spedire
006400020805     C                   CLEAR                   TIBS55ds
006500971121     C                   MOVEL     'L'           I50TLA
006600971121     C                   MOVEL     KNSIF         I50PSI
006700971121     C                   MOVEL     §FLE          I50PPO
006800971121     C                   MOVEL     '046'         I50APO
006900020805     C                   CALL      'TIBS55R'
007000020805     C                   PARM                    TIBS55ds
007100971121     C*
007200971121     C                   ENDSR
007300930510      *-----------------------------------------------------***********
007400041029      * TEST NR RECORD FILE DA TRASMETTE                    *  TSTOBJ *
007500930510      *-----------------------------------------------------***********
007600930510     C     TSTOBJ        BEGSR
007700930510     C** FILIALE CHE CHIUDE    PER APERTURA MEMBRO
007800020920     C* MEMBRO = §FLE + 046
007900991018     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
008000930510     C                   EXSR      TRUL51
008100991018     C                   MOVEL     'FNMOV00T'    FILE
008200020920     C                   MOVEL(p)  §membro       MBR
008300930510     C                   CALL      'TRUL51C'
008400930510     C                   PARM                    LIBR
008500930510     C                   PARM                    FILE
008600930510     C                   PARM                    MBR
008700930510     C                   PARM                    NRR
008800930512     C     NRR           IFNE      *ALL'9'
008900930510     C                   Z-ADD     NRR           NRMOV            10 0
009000930512     C                   END
009100941018     C* GUARDO SE IL MEMBRO è VUOTO - FNCAS00T
009200930510     C                   EXSR      TRUL51
009300941018     C                   MOVEL     'FNCAS00T'    FILE
009400020920     C                   MOVEL     §membro       MBR
009500930510     C                   CALL      'TRUL51C'
009600930510     C                   PARM                    LIBR
009700930510     C                   PARM                    FILE
009800930510     C                   PARM                    MBR
009900930510     C                   PARM                    NRR
010000930512     C     NRR           IFNE      *ALL'9'
010100930510     C                   Z-ADD     NRR           NRARC            10 0
010200930512     C                   END
010300930510     C                   ENDSR
010400991012     C**----------------------------------
010500911104     C** SOTTOMENTE TRASMISSIONE
010600911104     C**----------------------------------
010700911104     C     X25ARC        BEGSR
010800911104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
010900941018     C                   MOVEL     'FNCAS00T'    CMZFLD                          DA: >>>
011000020920     C                   MOVEL(p)  §membro       CMZMBD
011100971121     C                   MOVEL     O50PLA        CMZLBD
011200941018     C                   MOVEL     'FNCAS00R'    CMZFLA                          A: >>>
011300020920     C                   MOVEL(P)  §membro       CMZMBA
011400971121     C                   MOVEL     O50ALA        CMZLBA
011500010312     C                   MOVEL     'GAITRA'      CMZLBA
011600010312     C                   MOVE      O50ALA        WQUATR            4
011700010312     C     WQUATR        IFEQ      '101 '
011800010312     C                   MOVE      '201 '        CMZLBA
011900010312     C                   ENDIF
012000911104     C                   MOVE      '0000'        CMZORI                          ORA
012100911104     C                   MOVE      '0000000'     CMZDTI                          DATA
012200981210     C                   MOVE      'N'           CMZURG                          NOTTE
012300911104     C                   MOVE      'A'           CMZADD                          IN ADD
012400911104     C                   MOVE      'S'           CMZCLR                          CLEAR
012500911104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
012600911104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
012700941018     C                   MOVE      'TR87'        CMZAZN                          AZIONE
012800971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
012900010312     C                   MOVEL     'GAITRA'      CMZSYF
013000010312     C                   MOVE      O50ASI        WQUATR
013100010312     C     WQUATR        IFEQ      '101 '
013200010312     C                   MOVE      '201 '        CMZSYF
013300010312     C                   ENDIF
013400911104     C                   MOVE      NRARC         CMZNRR                          N.REC.
013500990830     C                   MOVEL     *BLANKS       CMZPRR                          PREREQ
013600920213     C                   EXSR      CALCMZ
013700911104     C                   ENDSR
013800911104     C*
013900911104     C     X25MOV        BEGSR
014000911104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
014100991018     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
014200020920     C                   MOVEL(p)  §membro       CMZMBD
014300971121     C                   MOVEL     O50PLA        CMZLBD
014400991018     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
014500020920     C                   MOVEL(p)  §membro       CMZMBA
014600971121     C                   MOVEL     O50ALA        CMZLBA
014700010312     C                   MOVEL     'GAITRA'      CMZLBA
014800010312     C                   MOVE      O50ALA        WQUATR            4
014900010312     C     WQUATR        IFEQ      '101 '
015000010312     C                   MOVE      '201 '        CMZLBA
015100010312     C                   ENDIF
015200911104     C                   MOVE      '0000'        CMZORI                          ORA
015300911104     C                   MOVE      '0000000'     CMZDTI                          DATA
015400981210     C                   MOVE      'N'           CMZURG                          NOTTE
015500911104     C                   MOVE      'A'           CMZADD                          IN ADD
015600911104     C                   MOVE      'S'           CMZCLR                          CLEAR
015700911104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
015800911104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
015900991117     C                   MOVE      'Y029'        CMZAZN                          AZIONE
016000971121     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
016100010312     C                   MOVEL     'GAITRA'      CMZSYF
016200010312     C                   MOVE      O50ASI        WQUATR
016300010312     C     WQUATR        IFEQ      '101 '
016400010312     C                   MOVE      '201 '        CMZSYF
016500010312     C                   ENDIF
016600911104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
016700941018     C****** >>>>>>  SE NON CI SONO C/ASSEGNI DA TRASMETTERE   (FNCAS00T)
016800911104     C****** >>>>>>  ABBLENCO FILE PRE-REQUISITO
016900911104     C     NRARC         IFEQ      *ZEROS
017000911104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
017100911104     C                   END
017200920213     C                   EXSR      CALCMZ
017300911104     C                   ENDSR
017400900606      *----------------------------------------------------------------
017500930510     C**----------------------------------
017600930510     C** CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
017700930510     C**----------------------------------
017800930510     C     CALCMZ        BEGSR
017900971121     C*
018000971121     C                   CLEAR                   W236
018100020805     C                   CALL      'TIBS53C'
018200971121     C                   PARM                    O50LCL
018300971121     C                   PARM                    CMZDST
018400930510     C                   PARM                    CMZFLD
018500930510     C                   PARM                    CMZMBD
018600930510     C                   PARM                    CMZLBD
018700930510     C                   PARM                    CMZFLA
018800930510     C                   PARM                    CMZMBA
018900930510     C                   PARM                    CMZLBA
019000930510     C                   PARM                    CMZORI
019100930510     C                   PARM                    CMZDTI
019200930510     C                   PARM                    CMZURG
019300930510     C                   PARM                    CMZADD
019400930510     C                   PARM                    CMZCLR
019500930510     C                   PARM                    CMZFTR
019600930510     C                   PARM                    CMZERR
019700930510     C                   PARM                    CMZAZN
019800930510     C                   PARM                    CMZSYF
019900930510     C                   PARM                    CMZNRR
020000930510     C                   PARM                    CMZPRR
020100971121     C                   PARM                    W236            236
020200930510     C                   ENDSR
020300941018      **
020400941018     C     *INZSR        BEGSR
020500941018     C     *ENTRY        PLIST
020600941018     C                   PARM                    KPJBA
020700941018      *
020800941018     C                   Z-ADD     1             CODUT
020900941018     C                   CALL      'X§PARUT'
021000941018     C                   PARM                    UT§DSE
021100941018     C                   MOVEL     REC80         CNCR80
021200020508     C     SIMTPP        IFEQ      '2'
021300020508     C     SIMTPP        OREQ      *BLANKS
021400020508     C                   MOVE      SIMPOU        §FLE              3 0
021500020508     C                   ELSE
021600941018     C     SIMFEL        COMP      0                                      02
021700941018     C  N02              MOVE      SIMFEL        §FLE
021800941018     C                   END
021900941018     C                   ENDSR
022000930521     OQSYSPRT   E            FINNMI      3  2
022100911105     O                                         +  1 'CHIUSURA TERMINATA'
022200911105     O                                         +  1 'REGOLARMENTE !!!!!'
022300931220     O          E            FINNMN      3  2
022400931220     O                                         +  1 'CHIUSURA TERMINATA'
022500931220     O                                         +  1 'REGOLARMENTE !!!!!'
022600931220     O                                         +  1 '- SENZA INCASSI -'
