000100990120     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990120     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* ANG1152R *-AN/38-*-------------------------------------------*
000400000000     F*----------*                                                   *
000500000000     F*          - MANUTENZIONE CAUSALI MAGAZZINO -                  *
000600000000     F*          ----------------------------------                  *
000700000000     F*          FILE : ANTAB00F-ANG1152D                            *
000800000000     F*--------------------------------------------------------------*
000900000000     FANTAB00F  UF A E           K DISK
001000990317     Ftii002d   CF   E             WORKSTN
001100000000     F                                     SFILE(SFL1:REC1)
001200841217     D ERR             S             30    DIM(7) CTDATA PERRCD(1)              *ERRORI
001300841210     D                 DS
001400841210     D  PROCE1                 1      2
001500850318     D  EU                     3      3
001600841210     D  DESPRC                 1      3
001700841210     D DESENT          DS
001800841210     D  E11                    1      3
001900841210     D  E21                    4      6
002000841210     D  E31                    7      9
002100841210     D  E12                   10     12
002200841210     D  E22                   13     15
002300841210     D  E32                   16     18
002400841210     D  E13                   19     21
002500841210     D  E23                   22     24
002600841210     D  E33                   25     27
002700841210     D  E14                   28     30
002800841210     D  E24                   31     33
002900841210     D  E34                   34     36
003000841210     D  E15                   37     39
003100841210     D  E25                   40     42
003200841210     D  E35                   43     45
003300841210     D DESUSC          DS
003400841210     D  U11                    1      3
003500841210     D  U21                    4      6
003600841210     D  U31                    7      9
003700841210     D  U12                   10     12
003800841210     D  U22                   13     15
003900841210     D  U32                   16     18
004000841210     D  U13                   19     21
004100841210     D  U23                   22     24
004200841210     D  U33                   25     27
004300841210     D  U14                   28     30
004400841210     D  U24                   31     33
004500841210     D  U34                   34     36
004600841210     D  U15                   37     39
004700841210     D  U25                   40     42
004800841210     D  U35                   43     45
004900841210     D TAB152        E DS                  EXTNAME(ANTAB152)
005000841210     D  ENTRAT                30     34
005100841210     D  USCITE                39     43
005200841210     D  GESTIO                48     52
005300850318     D FLGOPE          DS
005400850318     D  OPE01                  1      1
005500850318     D  OPE02                  2      2
005600850318     D  OPE03                  3      3
005700850318     D  OPE04                  4      4
005800850318     D  OPE05                  5      5
005900850318     D  OPE06                  6      6
006000850318     D  OPE07                  7      7
006100850318     D  OPE08                  8      8
006200850318     D  OPE09                  9      9
006300850318     D  OPE10                 10     10
006400841210     D                 DS
006500841210     D  E                      1      5
006600841210     D  U                      6     10
006700841210     D  G                     11     15
006800841210     D  AZIOPE                 1     15
006900000000     D KPJBA         E DS
007000841210     D  MSGERR               453    502
007100000000     C/EJECT
007200000000     C*----------------------------------------------------*
007300000000     C     *ENTRY        PLIST                                                  *
007400000000     C                   PARM                    KPJBA                          *
007500841210     C                   PARM                    PROCAU            2            *
007600990120     C                   PARM                    CODCAU            3            *
007700841210     C                   PARM                    DESCAU           25            *
007800841210     C                   PARM                    AZICAU           15            *
007900841210     C*----------                                          *
008000841210     C     KEYTAB        KLIST                                                  *
008100841210     C                   KFLD                    TABGRU                         *
008200841210     C                   KFLD                    TABCOD                         *
008300841210     C*----------                                          *
008400841210     C                   Z-ADD     1             CURSOR                         *
008500850709     C                   Z-ADD     1             RECTOT            5 0          *
008600841210     C     EMISPT        TAG                                                    *
008700841210     C                   SETOFF                                       010203    *
008800841210     C                   MOVE      ' '           SCEPF                          *PRD.FINITI
008900841210     C                   MOVE      ' '           SCEMP                          *MAT.PRIME
009000841210     C*---------- INTERROGAZIONE :                         *
009100841210     C     PROCAU        IFNE      *BLANKS                                      *
009200850318     C                   SETON                                            03    *
009300841210     C                   MOVE      PROCAU        PROCE1                         *
009400990120     C                   MOVEl     '999'         CODCAU                         *
009500841210     C                   MOVE      *ALL'*'       DESCAU                         *
009600841210     C                   MOVE      *BLANKS       AZICAU                         *
009700841210     C                   ELSE                                                   *
009800841210     C*---------- EMISSIONE PROMPT :                       *
009900841210     C                   EXFMT     ANG1152P                                     *
010000990409     C   Kc              GOTO      FINE                                         *
010100841210     C                   MOVE      *BLANKS       ERRORE                         *
010200841210     C                   MOVEA     *ZEROS        *IN(1)                         *
010300841210     C     SCEPF         IFEQ      'X'                                          *
010400841210     C                   MOVE      'PF'          PROCE1                         *
010500841210     C                   GOTO      INIZIO                                       *
010600841210     C                   END                                                    *
010700841210     C     SCEMP         IFEQ      'X'                                          *
010800841210     C                   MOVE      'MP'          PROCE1                         *
010900841210     C                   GOTO      INIZIO                                       *
011000841210     C                   END                                                    *
011100841210     C                   SETON                                            90    *
011200841210     C   90              MOVEL     ERR(1)        ERRORE                         *
011300841210     C   90              GOTO      EMISPT                                       *
011400841210     C                   END                                                    *
011500841210     C*----------------------------------------------------*
011600841210     C/EJECT
011700841210     C*----------------------------------------------------*
011800841210     C     INIZIO        TAG                                                    *
011900841210     C     PROCE1        COMP      'PF'                                   01    *
012000841210     C     PROCE1        COMP      'MP'                                   02    *
012100841210     C                   MOVE      '151'         TABGRU                         *
012200841210     C                   MOVE      *ZEROS        TABCOD                         *
012300841210     C*----------                                          *
012400841210     C                   MOVE      'E'           EU                             *
012500841210     C                   MOVE      DESPRC        TABCOD                         *
012600841210     C     KEYTAB        CHAIN     ANTAB00F                           90        *
012700841210     C   90              MOVEL     ERR(2)        ERRORE                         *
012800841210     C   90              GOTO      EMISPT                                       *
012900841210     C                   MOVEL     TABDES        DESENT                         *ENTRATE
013000841210     C*----------                                          *
013100841210     C                   MOVE      'U'           EU                             *
013200841210     C                   MOVE      DESPRC        TABCOD                         *
013300841210     C     KEYTAB        CHAIN     ANTAB00F                           90        *
013400841210     C   90              MOVEL     ERR(2)        ERRORE                         *
013500841210     C   90              GOTO      EMISPT                                       *
013600841210     C                   MOVEL     TABDES        DESUSC                         *USCITE
013700841210     C*----------                                          *
013800841210     C     LETTAB        TAG                                                    *
013900841210     C                   SETON                                        70        *PULIZIA
014000841210     C                   WRITE     SFL1CT                                       *
014100850318     C                   MOVEA     *ZEROS        *IN(4)                         *
014200841210     C*----------                                          *
014300850318     C                   Z-ADD     0             REC1              3 0          *
014400841210     C                   MOVE      *BLANKS       AZIOPE                         *
014500850318     C                   MOVE      *BLANKS       SCEIVA                         *
014600841210     C                   MOVE      '152'         TABGRU                         *
014700841210     C     TABGRU        SETLL     ANTAB00F                           10  11    *
014800841210     C   10                                                                     *
014900850318     CORN11              DO                                                     *
015000850318     C                   MOVE      'I'           SCEIVA                         *
015100850318     C                   SETON                                          08      *
015200850318     C                   GOTO      EMISF1                                       *
015300850318     C                   END                                                    *
015400841210     C*----------------------------------------------------*
015500841210     C/EJECT
015600841210     C*---------- EMISSIONE SUBFILE -----------------------*
015700000000     C     SOPRA         TAG                                                    *
015800841210     C     TABGRU        READE     ANTAB00F                               11    *FINE FILE
015900841210     C   11              GOTO      EMISF1                                       *
016000841210     C                   MOVE      TABCOD        CODICE            5            *
016100841210     C                   MOVEL     CODICE        PROCE2            2            *
016200841210     C     PROCE2        CABNE     PROCE1        SOPRA                          *
016300841210     C     REC1          ADD       1             REC1                 71        *
016400841210     C                   MOVE      *BLANKS       SCELTA                         *
016500841210     C                   MOVE      TABCOD        TABCAU                         *
016600841210     C                   MOVEL     TABDES        TAB152                         *
016700850318     C                   Z-ADD     REC1          RELVID                         *
016800000000     C                   WRITE     SFL1                                         *
016900000000     C                   GOTO      SOPRA                                        *
017000841210     C*----------                                          *
017100841210     C     EMISF1        TAG                                                    *
017200850709     C                   Z-ADD     RECTOT        CURSOR                         *
017300850709     C     CURSOR        DIV       10            CURSOR                         *
017400850709     C                   MVR                     RESTO             5 009        *
017500850709     C     CURSOR        MULT      10            CURSOR                         *
017600850709     C   09CURSOR        ADD       1             CURSOR                         *
017700850318     C                   SETOFF                                           08    *
017800850318     C                   WRITE     ANG1152T                                     *
017900850318     C                   MOVE      *BLANKS       TABCAU                         *
018000850318     C                   MOVE      *BLANKS       TAB152                         *
018100850318     C                   MOVE      *BLANKS       FLGOPE                         *
018200841210     C     EMIERR        TAG                                                    *
018300000000     C                   EXFMT     SFL1CT                                       *
018400850318     C                   MOVEA     *ZEROS        *IN(9)                         *
018500841210     C                   MOVE      *BLANKS       ERRORE                         *
018600850318     C   KA              GOTO      LETTAB                                       *
018700990120     C   Kl
018800841210     CAN 03              GOTO      FINE                                         *
018900990120     C   kl              GOTO      EMISPT                                       *
019000841210     C*----------------------------------------------------*
019100850318     C/EJECT
019200850318     C*---------- LETTURA SUBFILE -------------------------*
019300841210     C     SUREAD        TAG                                                    *
019400841210     C                   READC     SFL1                                 1312    *
019500841210     C  N12
019600841210     CANN13              DO                                                     *
019700850318     C                   MOVE      *BLANKS       SCEIVA                         *
019800850318     C   03SCELTA        IFEQ      'X'                                          *
019900990120     C                   MOVE      TABCAU        CODCAU                         *
020000841210     C                   MOVEL     DES152        DESCAU                         *
020100841210     C                   MOVE      ENTRAT        E                              *
020200841210     C                   MOVE      USCITE        U                              *
020300841210     C                   MOVE      GESTIO        G                              *
020400841210     C                   MOVE      AZIOPE        AZICAU                         *
020500841210     C                   GOTO      FINE                                         *
020600841210     C                   END                                                    *
020700850318     C     SCELTA        CABEQ     'X'           EMISF1                         *
020800850318     C     SCELTA        CABEQ     ' '           EMISF1                         *
020900850318     C     SCELTA        COMP      'I'                                0707      *
021000850318     C                   MOVE      SCELTA        SCEIVA                         *
021100850318     C  N07              MOVE      *BLANKS       TABCAU                         *
021200850318     C                   MOVE      OPE152        FLGOPE                         *
021300850318     C*---------- IMPOSTO DEFAULT IN FLAG OPERATIVI :      *
021400990317     C  N07              MOVE      'M'           OPE01                          * X
021500990317     C  N07              MOVE      'S'           OPE02                          * X
021600990317     C  N07              MOVE      'N'           OPE03                          * X
021700850626     C  N07              MOVE      '5'           OPE04                          * X MFM306R1
021800861022     C  N07              MOVE      'S'           OPE09                          * X MFM119R1
021900861022     C  N07              MOVE      'N'           OPE10                          * X MFM311R2
022000850709     C                   Z-ADD     RELVID        RECTOT                         *
022100850318     C                   SETON                                            08    *
022200850318     C                   GOTO      EMIERR                                       *
022300850318     C                   END                                                    *
022400841210     C*----------------------------------------------------*
022500841210     C/EJECT
022600841210     C*---------- MANUTENZIONE ----------------------------*
022700850318     C     SCEIVA        COMP      *BLANKS                                90    *
022800850318     C  N90TABCAU        COMP      *BLANKS                                90    *
022900841210     C   90              MOVEL     ERR(3)        ERRORE                         *ERR. -3-
023000841210     C   90              GOTO      EMIERR                                       *
023100841217     C*----------                                          *
023200841210     C   05GE5152        COMP      '*'                                    05    *INVENTARIO
023300841210     C   05              MOVE      *BLANKS       ENTRAT                         *
023400841210     C   05              MOVE      *BLANKS       USCITE                         *
023500841210     C   05              MOVE      *BLANKS       GESTIO                         *
023600841217     C   05              MOVE      '    *'       GESTIO                         *
023700850318     C   05              MOVE      *BLANKS       FLGOPE                         *
023800841217     C*----------------------------------------------------*
023900841217     C     ENTRAT        IFNE      *BLANKS                                      *
024000841217     C                   MOVE      *BLANKS       USCITE                         *
024100841217     C                   END                                                    *
024200841217     C     USCITE        IFNE      *BLANKS                                      *
024300841217     C                   MOVE      *BLANKS       ENTRAT                         *
024400841217     C                   END                                                    *
024500841217     C*----------                                          *
024600841210     C                   MOVE      ENTRAT        E                              *
024700841210     C                   MOVE      USCITE        U                              *
024800841210     C                   MOVE      GESTIO        G                              *
024900841210     C*----------                                          *
025000841210     C                   MOVE      '152'         TABGRU                         *
025100841210     C                   MOVE      *ZEROS        TABCOD                         *
025200841210     C                   MOVEL     PROCE1        CODICE                         *
025300841210     C                   MOVE      TABCAU        CODICE                         *
025400841210     C                   MOVE      CODICE        TABCOD                         *
025500841210     C     KEYTAB        CHAIN     ANTAB00F                           11        *
025600841210     C*---------- ANNULLAMENTO :                           *
025700841210     C     SCEIVA        IFEQ      'A'                                          *ANNULLAMENTO
025800841210     C  N11              DELETE    ANTAB000                                     *
025900841210     C                   GOTO      LETTAB                                       *
026000841210     C                   END                                                    *
026100841217     C*----------                                          *
026200841217     C     GE5152        COMP      *BLANKS                                90    *Giacenza
026300841217     C   90              MOVEL     ERR(7)        ERRORE                         *ERR. -7-
026400841217     C   90              GOTO      EMIERR                                       *
026500841210     C*----------                                          *
026600841217     C  N05ENTRAT        COMP      *BLANKS                                90    *
026700841217     C  N05
026800841217     CAN 90USCITE        COMP      *BLANKS                                90    *
026900841217     C   90              MOVEL     ERR(4)        ERRORE                         *ERR. -4-
027000841217     C   90              GOTO      EMIERR                                       *
027100841217     C*----------                                          *
027200841210     C                   MOVE      '<-->'        FI1152                         *
027300841210     C                   MOVE      '<-->'        FI2152                         *
027400841210     C                   MOVE      '<-->'        FI3152                         *
027500841210     C                   MOVEL     '<-->'        FI4152                         *
027600850318     C                   MOVE      FLGOPE        OPE152                         *
027700841210     C                   MOVEL     TAB152        TABDES                         *
027800841217     C*----------------------------------------------------*
027900841217     C/EJECT
028000841217     C*---------- INSERIMENTO -----------------------------*
028100841210     C     SCEIVA        IFEQ      'I'                                          *
028200841210     C  N11              SETON                                            90    *
028300841210     C   90              MOVEL     ERR(5)        ERRORE                         *ERR. -5-
028400841210     C   90              GOTO      EMIERR                                       *
028500841210     C                   WRITE     ANTAB000                                     *
028600841210     C                   END                                                    *
028700841217     C*---------- AGGIORNAMENTO ---------------------------*
028800841210     C     SCEIVA        IFEQ      'V'                                          *VARIAZIONE
028900841210     C   11              SETON                                            90    *
029000841210     C   90              MOVEL     ERR(6)        ERRORE                         *ERR. -6-
029100841210     C   90              GOTO      EMIERR                                       *
029200841210     C  N11              UPDATE    ANTAB000                                     *
029300841210     C                   END                                                    *
029400841210     C                   GOTO      LETTAB                                       *
029500841217     C*----------------------------------------------------*
029600000000     C     FINE          TAG                                                    *
029700000000     C                   SETON                                        LR        *
029800000000     C*----------------------------------------------------*
029900000000     C/EJECT
030000841217**    ERR                    ERRORI
030100841210*- Eseguire scelta Procedura-*  Err,1
030200841210*- Manca Tabella D/A Magazz.-*  Err,2
030300841210*- Battere Codice Causale !!-*  ERR,3
030400841210*- Inserire AZIONI Causale !-*  Err,4
030500841210*- CAUSALE  già Esistente ! -*  Err,5
030600841210*- CAUSALE  non TROVATA !!! -*  Err,6
030700841217*Manca Operazione su Giacenza*  Err,7
