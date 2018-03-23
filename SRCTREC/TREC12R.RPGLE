000100001205     H*------------------------------------------------------------------------*
000200001205     H* SIMULAZIONE CONTO ECONOMICO - SITEMA SALDI CLIENTE CON NOTE E RETTIFICHE
000300001205     H*------------------------------------------------------------------------*
000400001205     H DECEDIT('0,') DATEDIT(*DMY.)
000500001205     F*------------------------------------------------------------------------*
000600001205     F* DATA BASE
000700001205     F*------------------------------------------------------------------------*
000800001205     F*---
000900001205     F* Saldi cliente in BUDGET
001000001205     F*---
001100021216     FSISDOB    UF A E           K DISK
001200021216     F                                     RENAME(SISDO000:SISDOBDG)
001300001205     f                                     PREFIX(b_)
001400021216     FSISDCB    UF A E           K DISK
001500001205     F                                     RENAME(SISDC000:SISDCBDG)
001600001205     f                                     PREFIX(b_)
001700001205     F*---
001800021219     F* Saldi cliente in UNITRAGRU
001900001205     F*---
002000021219     FSISDOU    IF   E           K DISK
002100021219     F                                     RENAME(SISDO000:SISDOUNI)
002200021216     f                                     PREFIX(u_)
002300021219     FSISDCU    IF   E           K DISK
002400021216     F                                     RENAME(SISDC000:SISDCUNI)
002500021216     f                                     PREFIX(u_)
002600001205     D*------------------------------------------------------------------------*
002700001205     D* DATA STRUCTURE
002800001205     D*------------------------------------------------------------------------*
002900001205     D KDOKSC          S                   LIKE(B_SDOKSC)
003000001205     D KDOANN          S                   LIKE(B_SDOANN)
003100001205     D KDOMES          S                   LIKE(B_SDOMES)
003200001205     D KDCKSC          S                   LIKE(B_SDCKSC)
003300001205     D KDCANN          S                   LIKE(B_SDCANN)
003400001205     D KDCMES          S                   LIKE(B_SDCMES)
003500001205     D KDCREP          S                   LIKE(B_SDCREP)
003600001205     D KDCREA          S                   LIKE(B_SDCREA)
003700001205     D KDCFPE          S                   LIKE(B_SDCFPE)
003800001205     D KDCTSP          S                   LIKE(B_SDCTSP)
003900001205     D KDCCTR          S                   LIKE(B_SDCCTR)
004000001205     D KDCTPO          S                   LIKE(B_SDCTPO)
004100001205     D KDCRBL          S                   LIKE(B_SDCRBL)
004200001205     D KDCTPR          S                   LIKE(B_SDCTPR)
004300021216     D*-------------------
004400021216     D* ARCHITETTURA
004500021216     D*-------------------
004600021216     D KPJBA         E DS
004700021216     D*-------------------
004800021216     D* PARAMETRI IN ENTRATA
004900021216     D*-------------------
005000021216     D PARAM           DS
005100021216     D  PARORD                 1      1
005200021216     D  PARFIL                 2      4  0
005300021216     D  PARKS1                 5     11  0
005400021216     D  PARDS1                12     19  0
005500021216     D  PARAA1                12     15  0
005600021216     D  PARMG1                16     19  0
005700021218     D  PARME1                16     17  0
005800021216     D  PARDS2                20     27  0
005900021216     D  PARAA2                20     23  0
006000021216     D  PARMG2                24     27  0
006100021219     D  PARME2                24     25  0
006200021216     D  PARDF1                28     35  0
006300021216     D  PARDF2                36     43  0
006400021216     D  PARKS2                44     50  0
006500021216     D  PARASI                51     54  0
006600021216     D  PARCLR                55     55
006700001205     C*------------------------------------------------------------------------*
006800001205     C* MAIN LINE
006900001205     C*------------------------------------------------------------------------*
007000001205     C*----
007100001205     C* SALDI CLIENTE TESTATA
007200001205     C*----
007300021219     C                   READ      SISDOU                                 99
007400001205DO  1C     *IN99         DOWEQ     *OFF
007500021219IF  2C                   IF        (U_SDOANN = PARAA1) AND
007600021219     C                             (U_SDOMES>= PARME1) AND
007700021219     C                             (U_SDOMES<= PARME2)
007800021219IF  3C                   IF        U_SDOIRR <> 0 OR
007900021219     C                             U_SDOINC <> 0 OR
008000021219     C                             U_SDOIRC <> 0
008100021216     C                   Z-ADD     U_SDOKSC      KDOKSC
008200021216     C                   EVAL      KDOANN = U_SDOANN + 1
008300021216     C                   Z-ADD     U_SDOMES      KDOMES
008400021216     C     KEYSDO        CHAIN     SISDOB                             98
008500021219IF  4C     *IN98         IFEQ      *OFF
008600021219     C                   Z-ADD     U_SDOIRR      b_SDOIRR
008700021219     C                   Z-ADD     U_SDOINC      b_SDOINC
008800021219     C                   Z-ADD     U_SDOIRC      b_SDOIRC
008900001207     C                   UPDATE    SISDOBDG
009000021219X   4C                   ELSE
009100021219     C                   CLEAR                   SISDOBDG
009200021216     C                   EVAL      b_SDOANN = KDOANN
009300021216     C                   EVAL      b_SDOMES = KDOMES
009400021216     C                   EVAL      b_SDOKSC = KDOKSC
009500021216     C                   EVAL      b_SDOIRR = U_SDOIRR
009600021216     C                   EVAL      b_SDOINC = U_SDOINC
009700021216     C                   EVAL      b_SDOIRC = U_SDOIRC
009800021216     C                   WRITE     SISDOBDG
009900021219E   4C                   ENDIF
010000021219E   3C                   ENDIF
010100021219E   2C                   ENDIF
010200021219     C                   READ      SISDOU                                 99
010300001205E   1C                   ENDDO
010400001205     C*----
010500001205     C* SALDI CLIENTE DETTAGLIO
010600001205     C*----
010700021219     C                   READ      SISDCU                                 99
010800001205DO  1C     *IN99         DOWEQ     *OFF
010900021219IF  2C                   IF        (U_SDCANN = PARAA1) AND
011000021219     C                             (U_SDCMES>= PARME1) AND
011100021219     C                             (U_SDCMES<= PARME2)
011200021219IF  3C                   IF        U_SDCIRR <> 0 OR
011300021219     C                             U_SDCINC <> 0 OR
011400021219     C                             U_SDCIRC <> 0
011500021216     C                   Z-ADD     U_SDCKSC      KDCKSC
011600021216     C                   EVAL      KDCANN = U_SDCANN + 1
011700021216     C                   Z-ADD     U_SDCMES      KDCMES
011800021216     C                   Z-ADD     U_SDCREP      KDCREP
011900021216     C                   Z-ADD     U_SDCREA      KDCREA
012000021216     C                   MOVEL     U_SDCFPE      KDCFPE
012100021216     C                   MOVEL     U_SDCTSP      KDCTSP
012200021216     C                   Z-ADD     U_SDCCTR      KDCCTR
012300021216     C                   MOVEL     U_SDCTPO      KDCTPO
012400021216     C                   MOVEL     U_SDCRBL      KDCRBL
012500021216     C                   MOVEL     U_SDCTPR      KDCTPR
012600021216     C     KEYSDC        CHAIN     SISDCB                             98
012700021219IF  4C     *IN98         IFEQ      *OFF
012800021219     C                   Z-ADD     U_SDCIRR      b_SDCIRR
012900021219     C                   Z-ADD     U_SDCINC      b_SDCINC
013000021219     C                   Z-ADD     U_SDCIRC      b_SDCIRC
013100001205     C                   UPDATE    SISDCBDG
013200021219X   4C                   ELSE
013300021219     C                   CLEAR                   SISDCBDG
013400021219     C                   EVAL      b_SDCANN = KDCANN
013500021219     C                   EVAL      b_SDCMES = KDCMES
013600021219     C                   EVAL      b_SDCKSC = KDCKSC
013700021219     C                   EVAL      b_SDCREP = KDCREP
013800021219     C                   EVAL      b_SDCREA = KDCREA
013900021219     C                   EVAL      b_SDCFPE = KDCFPE
014000021219     C                   EVAL      b_SDCTSP = KDCTSP
014100021219     C                   EVAL      b_SDCCTR = KDCCTR
014200021219     C                   EVAL      b_SDCTPO = KDCTPO
014300021219     C                   EVAL      b_SDCRBL = KDCRBL
014400021219     C                   EVAL      b_SDCTPR = KDCTPR
014500021219     C                   EVAL      b_SDCIRR = U_SDCIRR
014600021219     C                   EVAL      b_SDCINC = U_SDCINC
014700021219     C                   EVAL      b_SDCIRC = U_SDCIRC
014800021216     C                   WRITE     SISDCBDG
014900021219E   4C                   ENDIF
015000021219E   3C                   ENDIF
015100021219E   2C                   ENDIF
015200021219     C                   READ      SISDCU                                 99
015300001205E   1C                   ENDDO
015400001205     C*
015500001205     C                   SETON                                            LR
015600001205     C*------------------------------------------------------------------------*
015700001205     C* *INZSR - OPERAZIONI INIZIALI
015800001205     C*------------------------------------------------------------------------*
015900001205     C     *INZSR        BEGSR
016000021216     C*
016100021216     C* RICEVIMENTO PARAMETRI
016200021216     C     *ENTRY        PLIST
016300021216     C                   PARM                    KPJBA
016400021216     C                   MOVEL     KPJBU         PARAM
016500001205     C*
016600001205     C* CHIAVI DI LETTURA
016700001205     C     KEYSDO        KLIST
016800001205     C                   KFLD                    KDOKSC
016900001205     C                   KFLD                    KDOANN
017000001205     C                   KFLD                    KDOMES
017100001205     C     KEYSDC        KLIST
017200001205     C                   KFLD                    KDCKSC
017300001205     C                   KFLD                    KDCANN
017400001205     C                   KFLD                    KDCMES
017500001205     C                   KFLD                    KDCREP
017600001205     C                   KFLD                    KDCREA
017700001205     C                   KFLD                    KDCFPE
017800001205     C                   KFLD                    KDCTSP
017900001205     C                   KFLD                    KDCCTR
018000001205     C                   KFLD                    KDCTPO
018100001205     C                   KFLD                    KDCRBL
018200001205     C                   KFLD                    KDCTPR
018300001205     C*
018400001205     C                   ENDSR
