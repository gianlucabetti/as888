000100990119     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990119     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300860305     F* ANU015R  *-AN/38-*-------------------------------------------*
000400860305     F*------------------*                                           *
000500860305     F*       - RICERCA ANAGRAFICO MATERIE PRIME      -              *
000600860305     F*       -----------------------------------------              *
000700860305     F*                                                              *
000800990119     F*         FILE : tmamp01L-tmamp02L-tmamp03L-tmamp04L           *
000900990119     F*                tmamp05L-ANTAB00F-ANU015D                     *
001000860305     F*                                                              *
001100990317     F*         PGM. : TII110R - RICERCA X CLASSE                    *
001200990317     F*              : TII111R - RICERCA X S/CLASSE                  *
001300990317     F*              : TII118R - RICERCA X CAT.FISCALE               *
001400860305     F*                                                              *
001500860305     F*--------------------------------------------------------------*
001600990317     Ftii104D   CF   E             WORKSTN USROPN
001700860305     F                                     SFILE(SFL1:REC1)
001800860305     FANTAB00F  IF   E           K DISK    USROPN
001900860305     F*---------
002000990119     Ftmamp01L  IF   E           K DISK    USROPN
002100990119     F                                     RENAME(tmamp000:ANACOD)
002200860305     F*---------
002300990119     Ftmamp02L  IF   E           K DISK    USROPN
002400990119     F                                     RENAME(tmamp000:ANADES)
002500860305     F*---------
002600990119     Ftmamp03L  IF   E           K DISK    USROPN
002700990119     F                                     RENAME(tmamp000:ANACLM)
002800860305     F*---------
002900990119     Ftmamp04L  IF   E           K DISK    USROPN
003000990119     F                                     RENAME(tmamp000:ANASCM)
003100860305     F*---------
003200990119     Ftmamp05L  IF   E           K DISK    USROPN
003300990119     F                                     RENAME(tmamp000:ANACTF)
003400860305     F*---------
003500860305     D ERR             S             30    DIM(6) CTDATA PERRCD(1)              *ERRORI
003600860305     D ARTIC           DS
003700860305     D*---- KEY PRODOTTO / VERSIONE
003800871005     D  AMPMAT                 1     12
003900871005     D  CODICE                 1     12
004000990119     d dataiso         s               d   datfmt(*iso)
004100990119     d dataeur         s               d   datfmt(*eur)
004200860305     C*----------------------------------------------------*
004300860305     C     *ENTRY        PLIST                                                  *
004400871005     C                   PARM                    CODART           12            *
004500860305     C                   PARM                    DESART                         *
004600860305     C*----------                                          *
004700860305     C     KEYART        KLIST                                                  *
004800860305     C                   KFLD                    CODART                         *
004900860305     C*----------                                          *
005000860305     C     KEY2          KLIST                                                  *
005100860305     C                   KFLD                    KK                2            *
005200860305     C                   KFLD                    DESK2            30            *
005300860305     C*----------                                          *
005400860305     C     KEY3          KLIST                                                  *
005500860305     C                   KFLD                    KKK               3            *
005600860305     C                   KFLD                    DESK2                          *
005700860305     C*----------                                          *
005800860305     C     KEY5          KLIST                                                  *
005900860305     C                   KFLD                    KKKKK             5            *
006000860305     C                   KFLD                    DESK2                          *
006100860305     C*----------                                          *
006200860305     C     KEYDES        KLIST                                                  *
006300860305     C                   KFLD                    DESART                         *
006400860305     C*----------                                          *
006500860305     C     KEYTAB        KLIST                                                  *
006600860305     C                   KFLD                    TABGRU                         *
006700860305     C                   KFLD                    TABCOD                         *
006800860305     C                   Z-ADD     0             TOTART                         *
006900860924     C*---------- GIRA UDATE                               *
007000871005     C                   MOVEL     *BLANKS       INTERR           12
007100861211     C                   MOVEL     '?'           INTERR
007200871005     C                   MOVEL     '99999999'    NOVE             12
007300861211     C                   MOVE      '99999999'    NOVE
007400990119     C                   time                    como14           14 0          *
007500990119     C                   move      como14        ymd               8 0          *
007600990119     c                   move      ymd           dataeur
007700990119     c                   move      dataeur       dataiso
007800990119     C                   move      dataiso       YMD                            *
007900860305     C*----------RICERCA X CODICE -------------------------*
008000861211     C     CODART        IFNE      INTERR                                       *
008100860305     C     CODART        IFNE      *BLANKS                                      *
008200861211     C     CODART        CABEQ     NOVE          FINE                     90    *
008300990119     C                   OPEN      tmamp01L                             90      *
008400990119     C  N90KEYART        CHAIN     tmamp01L                           90        *
008500860305     C  N90              MOVE      AMPDMT        DESART                         *
008600860305     C                   GOTO      FINE                                         *
008700860305     C                   END                                                    *
008800860305     C                   END                                                    *
008900860305     C*---------- RICERCA ALFABETICA ----------------------*
009000990317     C                   OPEN      tii104D                                      *
009100860305     C                   OPEN      ANTAB00F                                     *
009200860305     C                   MOVE      'XXX'         CLM                            *
009300860305     C                   MOVE      'XXXXX'       SCM                            *
009400860305     C                   MOVE      'XX'          CTF                            *
009500860305     C     DESART        COMP      *BLANKS                            0202      *RICHIAMATO
009600860305     C   02              GOTO      ALFA                                         *
009700860305     C*--------- EMISSIONE PROMPT -------------------------*
009800860305     C     EMISPT        TAG                                                    *
009900860305     C                   SETOFF                                           01    *
010000860305     C                   WRITE     ANU015TS                                     *TESTATA
010100860305     C                   EXFMT     ANU015PT                                     *
010200860305     C                   MOVE      *BLANKS       ERRORE                         *
010300860305     C                   MOVEA     *ZEROS        *IN(10)                        *
010400860305     C                   SETOFF                                           90    *
010500860305     C   KJDESART        COMP      *BLANKS                                90    *
010600990114     C   Kc              GOTO      FINE                                         *
010700860305     C     ALFA          TAG                                                    *
010800860305     C     DESART        IFNE      *BLANKS                                      *
010900860305     C                   SETON                                            50    *
011000860305     C                   MOVEL     DESART        UNO1              1            *
011100860305     C                   MOVEL     DESART        DUE1              2            *
011200860305     C                   MOVEL     ' '           DUE1                           *
011300860305     C                   MOVEL     DESART        TRE1              3            *
011400860305     C                   MOVEL     '  '          TRE1                           *
011500860305     C                   MOVEL     DESART        DESK2                          *
011600860305     C                   END                                                    *
011700860305     C                   MOVEL     *BLANKS       DESCLM                         *
011800860305     C                   MOVEL     *BLANKS       DESSCM                         *
011900860305     C                   MOVEL     *BLANKS       DESCTF                         *
012000860305     C                   MOVEL     *BLANKS       DESKKK                         *
012100860305     C     CLM           CASNE     'XXX'         RUTCLM               9191      *
012200860305     C     SCM           CASNE     'XXXXX'       RUTSCM               9292      *
012300860305     C     CTF           CASNE     'XX'          RUTCTF               9393      *
012400860305     C     DESART        CASNE     *BLANKS       RUTALF               9797      *
012500860305     C                   END                                                    *
012600860305     C                   SETON                                            90    *
012700860305     C                   MOVEL     ERR(5)        ERRORE                         *
012800860305     C                   GOTO      EMISPT                                       *
012900860305     C*----------                                          *
013000860305     C     FINE          TAG                                                    *
013100861211     C   90              MOVEL     NOVE          CODART                         *
013200860305     C   90              MOVE      *BLANKS       DESART                         *
013300860305     C   90              MOVEL     ERR(6)        DESART                         *
013400860305     C                   SETON                                            LR    *
013500860305     C*---------- RICERCA CLASSE MERCEOLOGICA -------------*
013600860305     C***------------------------------------------------***
013700860305     CSR   RUTCLM        BEGSR                                                  *
013800860305     C*----------                                          *
013900860305     C     CLM           IFEQ      '?  '                                        *
014000860305     C                   MOVE      *BLANKS       CLM                            *
014100990317     C                   CALL      'TII110R'                                    *
014200860305     C                   PARM                    CLM                            *
014300860305     C                   PARM                    DESCLM                         *
014400860305     C                   GOTO      EMISPT                                       *
014500860305     C                   END                                                    *
014600860305     C*----------                                          *
014700860305     C     CLM           IFNE      '999'                                        *
014800860305     C                   MOVE      '155'         TABGRU                         *
014900860305     C                   MOVE      *ZEROS        TABCOD                         *
015000860305     C                   MOVE      CLM           TABCOD                         *
015100860305     C     KEYTAB        CHAIN     ANTAB00F                           90        *
015200860305     C   90              MOVEL     ERR(1)        ERRORE                         *
015300860305     C   90              GOTO      EMISPT                                       *
015400860305     C                   MOVEL     TABDES        DESKKK                         *
015500860305     C                   SETON                                            80    *
015600860305     C                   MOVE      CLM           KKK                            *
015700860305     C                   ELSE                                                   *
015800860305     C                   MOVE      *BLANKS       KKK                            *
015900860305     C                   MOVE      *ALL'*'       DESKKK                         *
016000860305     C                   END                                                    *
016100860305     C*----------                                          *
016200990119     C                   OPEN      tmamp03L                             90      *
016300990119     C  N90KEY3          SETLL     tmamp03L                           90        *
016400860305     C   90
016500860305     CAN 02              GOTO      FINE                                         *
016600860305     C   90              MOVEL     ERR(4)        ERRORE                         *
016700860305     C   90              GOTO      EMISPT                                       *
016800860305     C                   EXSR      CARSFL                                       *
016900860305     CSR                 ENDSR                                                  *
017000860305     C*---------- RICERCA S/CLASSE MERCEOLOGICA -----------*
017100860305     C***------------------------------------------------***
017200860305     CSR   RUTSCM        BEGSR                                                  *
017300860305     C*----------                                          *
017400860305     C     SCM           IFEQ      '?    '                                      *
017500860305     C                   MOVE      *BLANKS       SCM                            *
017600990317     C                   CALL      'TII111R'                                    *
017700860305     C                   PARM                    SCM                            *
017800860305     C                   PARM                    DESSCM                         *
017900860305     C                   GOTO      EMISPT                                       *
018000860305     C                   END                                                    *
018100860305     C*----------                                          *
018200860305     C     SCM           IFNE      '99999'                                      *
018300860305     C                   MOVE      '156'         TABGRU                         *
018400860305     C                   MOVE      *ZEROS        TABCOD                         *
018500860305     C                   MOVE      SCM           TABCOD                         *
018600860305     C     KEYTAB        CHAIN     ANTAB00F                           90        *
018700860305     C   90              MOVEL     ERR(2)        ERRORE                         *
018800860305     C   90              GOTO      EMISPT                                       *
018900860305     C                   MOVEL     TABDES        DESKKK                         *
019000860305     C                   SETON                                            80    *
019100860305     C                   MOVE      SCM           KKKKK                          *
019200860305     C                   ELSE                                                   *
019300860305     C                   MOVE      *BLANKS       KKKKK                          *
019400860305     C                   MOVE      *ALL'*'       DESKKK                         *
019500860305     C                   END                                                    *
019600860305     C*----------                                          *
019700990119     C                   OPEN      tmamp04L                             90      *
019800990119     C  N90KEY5          SETLL     tmamp04L                           90        *
019900860305     C   90
020000860305     CAN 02              GOTO      FINE                                         *
020100860305     C   90              MOVEL     ERR(4)        ERRORE                         *
020200860305     C   90              GOTO      EMISPT                                       *
020300860305     C                   EXSR      CARSFL                                       *
020400860305     CSR                 ENDSR                                                  *
020500860305     C*---------- RICERCA CATEGORIA FISCALE ---------------*
020600860305     C***------------------------------------------------***
020700860305     CSR   RUTCTF        BEGSR                                                  *
020800860305     C*----------                                          *
020900860305     C     CTF           IFEQ      '? '                                         *
021000860305     C                   MOVE      *BLANKS       CTF                            *
021100990317     C                   CALL      'TII118R'                                    *
021200860305     C                   PARM                    CTF                            *
021300860305     C                   PARM                    DESCTF                         *
021400860305     C                   GOTO      EMISPT                                       *
021500860305     C                   END                                                    *
021600860305     C*----------                                          *
021700860305     C     CTF           IFNE      '99'                                         *
021800860305     C                   MOVE      '163'         TABGRU                         *
021900860305     C                   MOVE      *ZEROS        TABCOD                         *
022000860305     C                   MOVE      CTF           TABCOD                         *
022100860305     C     KEYTAB        CHAIN     ANTAB00F                           90        *
022200860305     C   90              MOVEL     ERR(3)        ERRORE                         *
022300860305     C   90              GOTO      EMISPT                                       *
022400860305     C                   MOVEL     TABDES        DESKKK                         *
022500860305     C                   SETON                                            80    *
022600860305     C                   MOVE      CTF           KK                             *
022700860305     C                   ELSE                                                   *
022800860305     C                   MOVE      *BLANKS       KK                             *
022900860305     C                   MOVE      *ALL'*'       DESKKK                         *
023000860305     C                   END                                                    *
023100860305     C*----------                                          *
023200990119     C                   OPEN      tmamp05L                             90      *
023300990119     C  N90KEY2          SETLL     tmamp05L                           90        *
023400860305     C   90
023500860305     CAN 02              GOTO      FINE                                         *
023600860305     C   90              MOVEL     ERR(4)        ERRORE                         *
023700860305     C   90              GOTO      EMISPT                                       *
023800860305     C                   EXSR      CARSFL                                       *
023900860305     CSR                 ENDSR                                                  *
024000860305     C*---------- RICERCA ALFABETICA PRODOTTO -------------*
024100860305     C***------------------------------------------------***
024200860305     CSR   RUTALF        BEGSR                                                  *
024300990119     C                   OPEN      tmamp02L                             90      *
024400860305     C  N90KEYDES        SETLL     ANADES                             90        *
024500860305     C   90
024600860305     CAN 02              GOTO      FINE                                         *
024700860305     C   90              MOVEL     ERR(4)        ERRORE                         *
024800860305     C   90              GOTO      EMISPT                                       *
024900860305     C                   EXSR      CARSFL                                       *
025000860305     CSR                 ENDSR                                                  *
025100860305     C**--------- GESTIONE SUBFILE -----------------------**
025200860305     C***------------------------------------------------***
025300860305     CSR   CARSFL        BEGSR                                                  *
025400860305     C                   Z-ADD     0             REC1              5 0          *
025500860305     C                   MOVE      *BLANKS       SCELTA                         *
025600860305     C*---------- PULIZIA SUBFILE :                        *
025700860305     C                   SETON                                            70    *
025800860305     C                   WRITE     SFL1CT                                       *
025900860305     C                   SETOFF                                       7071      *
026000860305     C*---------- CARICAMENTO SFL :                        *
026100860305     C     SOPRA         TAG                                                    *
026200860305     C   91
026300860305     CAN 80KKK           READE     ANACLM                                 99    *CLASSE
026400860305     C   91
026500860305     CANN80              READ      ANACLM                                 99    *
026600860305     C   92
026700860305     CAN 80KKKKK         READE     ANASCM                                 99    *S/CLASSE
026800860305     C   92
026900860305     CANN80              READ      ANASCM                                 99    *
027000860305     C   93
027100860305     CAN 80KK            READE     ANACTF                                 99    *CAT/FISCALE
027200860305     C   93
027300860305     CANN80              READ      ANACTF                                 99    *
027400860305     C   97              READ      ANADES                                 99    *DESCRIZ.
027500860305     C   99              GOTO      EMISF1                                       *
027600860305     C   50              MOVEL     AMPDMT        UNO2              1            *
027700860305     C   50              MOVEL     AMPDMT        DUE2              2            *
027800860305     C   50              MOVEL     ' '           DUE2                           *
027900860305     C   50              MOVEL     AMPDMT        TRE2              3            *
028000860305     C   50              MOVEL     '  '          TRE2                           *
028100860305     C   50UNO1          CABNE     UNO2          EMISF1                         *
028200860305     C   50DUE1          IFNE      *BLANKS                                      *
028300860305     C     DUE1          CABNE     DUE2          EMISF1                         *
028400860305     C                   END                                                    *
028500860305     C   50TRE1          IFNE      *BLANKS                                      *
028600860305     C     TRE1          CABNE     TRE2          EMISF1                         *
028700860305     C                   END                                                    *
028800860305     C*----------SELEZIONA CLASSE MERCEOLOGICA ------------*
028900860305     C     CLM           IFNE      'XXX'                                        *
029000860305     C     AMPCLM        CABNE     CLM           SOPRA                          *
029100860305     C                   END                                                    *
029200860305     C*----------SELEZIONA S/CLASSE MERCEOLOGICA ----------*
029300860305     C     SCM           IFNE      'XXXXX'                                      *
029400860305     C     AMPSCM        CABNE     SCM           SOPRA                          *
029500860305     C                   END                                                    *
029600860305     C*----------SELEZIONA CATEGORIA FISCALE --------------*
029700860305     C     CTF           IFNE      'XX'                                         *
029800860305     C     AMPCTF        CABNE     CTF           SOPRA                          *
029900860305     C                   END                                                    *
030000860924     C*---------- CONTROLLA DATA FINE GESTIONE             *
030100860924     C     AMPDFG        IFNE      *ZEROS                                       *
030200860924     C     AMPDFG        COMP      YMD                                  30      *
030300860924     C                   END                                                    *
030400860924     C*---------- SCRIVO RECORD SU SUBFILE ----------------*
030500860305     C     REC1          ADD       1             REC1                 71        *
030600860305     C     TOTART        ADD       1             TOTART                         *
030700860305     C                   WRITE     SFL1                                         *
030800860924     C                   SETOFF                                         30      *
030900860305     C                   GOTO      SOPRA                                        *
031000860305     C*---------- EMISSIONE SFL ---------------------------*
031100860305     C     EMISF1        TAG                                                    *
031200860305     C                   SETON                                            01    *
031300860305     C                   WRITE     ANU015TS                                     *TESTATA
031400860305     C                   EXFMT     SFL1CT                                       *
031500990114     C   Kc              SETON                                            90    *RICHIAMATO
031600990114     C   Kc              GOTO      FINE                                         *
031700860305     C*---------- LETTURA RECORD VARIATI :                 *
031800860305     C   71              READC     SFL1                                   52    *
031900860305     C  N71                                                                     *
032000860305     COR 52              GOTO      EMISF1                                       *
032100860305     C     SCELTA        CABNE     'X'           EMISF1                         *
032200860305     C                   MOVEL     CODICE        CODART                         *
032300860305     C                   MOVE      AMPDMT        DESART                         *
032400860305     C                   GOTO      FINE                                         *
032500860305     CSR                 ENDSR                                                  *
032600860924     C***------------------------------------------------***
032700860305**    ERR
032800860305*- Cod. CLASSE   MERC.Errato-* ERR,1
032900860305*- Cod. S/CLASSE MERC.Errato-* ERR,2
033000860305*- Cod. CAT.FISCALE   Errato-* ERR,3
033100860305*- Non Esistono PRODOTTI !! -* ERR,4
033200860305*- Inserire Dati x Ricerca  -* ERR,5
033300860305*- Errore - Errore - Errore -* ERR,6
