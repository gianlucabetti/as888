000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114      * FICNC3R ******************************************************
000300000704      *                                                              *
000400000705      *         SVALORIZZAZIONE CONTEGGI PADRONCINI      - BATCH -   *
000500000705      *         -----------------------------------      ---------   *
000600000704      *                                                              *
000700000704      ****************************************************************
000800021202     Ffiapd01L  IF   E           K DISK
000900011114     Ffiftt02L  UF   E           K DISK
001000011114     F                                     RENAME(fiftt000:fiftt2)
001100011114     Ffifst02L  UF   E           K DISK
001200011114     F                                     RENAME(fiftt000:fifst2)
001300011114     Ffiftt01L  UF   E           K DISK
001400011114     F                                     RENAME(fiftt000:fiftt1)
001500011114     Ffifst01L  UF   E           K DISK
001600011114     F                                     RENAME(fiftt000:fifst1)
001700000705      *
001800000705     D* RICEZIONE DATI DA FILTRO
001900000704     D PARAM           DS
002000000704     D  VIDSIM                 1      1
002100000704     D  VIDLID                 2      4  0
002200000704     D  VIDPAD                 5      8
002300000704     D  PARCPD                 2      8  0
002400000704     D  VIDLIA                 9     11  0
002500000704     D  VIDPAA                12     15
002600000704     D  PARCPA                 9     15  0
002700000705     D  PARDTR                16     23  0
002800001124     D  PARDTA                24     31  0
002900000705     D* PASSAGGIO PARAMETRI A PGM SVALORIZZAZIONE DETTAGLIO
003000000705     D PARAM2          DS
003100000705     D  PA2PDR                 1      7  0
003200000705     D  PA2TSR                 8      8
003300000705     D  PA2NDC                 9     15  0
003400000705     D  PA2FCT                16     16
003500000705     D  PA2FVD                17     17
003600000705     D  PA2FVT                18     18
003700000705     D  PA2DDC                19     26  0
003800000704     D KPJBA         E DS
003900000704      *
004000000705      ****************************************************************
004100000704      *                                                              *
004200000704      *         O P E R A Z I O N I   I N I Z I A L I                *
004300000704      *                                                              *
004400000704      ****************************************************************
004500000704      *
004600000704     C     *ENTRY        PLIST
004700000704     C                   PARM                    KPJBA
004800000705     C                   MOVEL     KPJBU         PARAM
004900000705      *
005000000705     C     *LIKE         DEFINE    PARCPA        PARCPW
005100000705      *
005200000705     C     *LIKE         DEFINE    FTTPDR        KPDR
005300000705     C     *LIKE         DEFINE    FTTFVL        KFVL
005400000705     C     *LIKE         DEFINE    FTTDDC        KDDC
005500000705     C     *LIKE         DEFINE    FTTTSR        KTSR
005600000705     C     *LIKE         DEFINE    FTTNDC        KNDC
005700020404     C     *LIKE         DEFINE    FTTPDR        savpdr
005800020404     C     *LIKE         DEFINE    FTTDDC        savddc
005900000705      *
006000000705     C     *LIKE         DEFINE    FTTDDC        WDDC
006100000704      *
006200000705     C     KFTT          KLIST
006300000705     C                   KFLD                    KPDR
006400000705     C                   KFLD                    KFVL
006500000705     C                   KFLD                    KDDC
006600000705      *
006700000705     C     KFTT2         KLIST
006800000705     C                   KFLD                    KPDR
006900000705     C                   KFLD                    KFVL
007000000705      *
007100000705     C     KFTT3         KLIST
007200000705     C                   KFLD                    KPDR
007300000705     C                   KFLD                    KTSR
007400000705     C                   KFLD                    KNDC
007500000705     C                   KFLD                    WDDC
007501021202      *
007502021202     C     Kapd          KLIST
007503021202     C                   KFLD                    apdtip
007504021202     C                   KFLD                    parcpd
007505021202     c                   movel     'A'           apdtip
007600000705      *
007700000705     C                   Z-ADD     0             KNDC
007800000705     C                   MOVE      ' '           KTSR
007900000705      *
008000000705     C                   MOVE      'C'           KFVL
008100000705      *
008200000705      * INIZIALIZZO CODICE PADRONCINO "A"
008300000705      *
008400000705     C     VIDPAA        IFEQ      *BLANKS
008500000705     C     VIDPAA        OREQ      *ALL'0'
008600000705     C                   MOVE      PARCPD        PARCPW
008700000705     C                   ELSE
008800000705     C                   MOVE      PARCPA        PARCPW
008900000705     C                   ENDIF
009000000705      *
009100000705      * SIMULAZIONE
009200000705      *
009300000705     C     VIDSIM        IFEQ      'S'
009400000705     C                   SETON                                        38        SIMULAZIONE
009500000705     C                   ENDIF
009600000704      *
009700000705      ****************************************************************
009800000704      *                                                              *
009900000704      *         R O U T I N E   P R I N C I P A L E                  *
010000000704      *                                                              *
010100000704      ****************************************************************
010200000705      *
010300000705      * LEGGO L'ANAGRAFICO PADRONCINI E CONSIDERO SOLO QUELLI VALIDI
010400000705      *
010500021202     C     kapd          SETLL     fiapd01L
010600000705     C                   DO        *HIVAL
010700021202     C     apdtip        READE     fiapd01L                               35
010800000705     C   35              LEAVE
010900000705     C     APDPDR        IFGT      PARCPW
011000000705     C                   LEAVE
011100000705     C                   ENDIF
011200000705     C     APDATB        IFNE      ' '
011300000705     C     APDPDD        OREQ      'S'
011400020524     C     VIDSIM        andne     'S'
011500000705     C                   ITER
011600000705     C                   ENDIF
011700000705      *
011800000705      * MI POSIZIONO DALLA DATA DISTINTA (PER OGNI PADRONCINO)
011900000705      *
012000000705     C                   MOVE      APDPDR        KPDR
012100000705     C                   Z-ADD     PARDTR        KDDC
012200000705      *
012300001124      * LEGGO DALLA "DATA DAL" IN POI ED ESCO PER SUPERO "DATA AL"
012400000705      *
012500011114     C   38KFTT          SETLL     fifst02L
012600011114     C  N38KFTT          SETLL     fiftt02L
012700000705     C                   DO        *HIVAL
012800011114     C   38KFTT2         READE     fifst02L                               30
012900011114     C  N38KFTT2         READE     fiftt02L                               30
013000000705     C   30              LEAVE
013100001124     C     FTTDDC        IFGT      PARDTA
013200001124     C                   LEAVE
013300001124     C                   ENDIF
013400000705     C     FTTNDC        IFGT      0
013500000705     C     FTTTSR        IFEQ      'C'
013600000705     C     FTTTSR        OREQ      'R'
013700000705      *
013800000705      * AGGIORNO RECORD CORRENTE E LANCIO PGM DI
013900000705      * SVALORIZZAZIONE DETTAGLIO
014000000705      *
014100000705     C                   MOVE      'V'           FTTFVL
014200011114     C   38              UPDATE    fifst2
014300011114     C  N38              UPDATE    fiftt2
014400011114     C   38              FEOD      fifst01L
014500011114     C  N38              FEOD      fiftt01L
014600000705      *
014700000705     C                   Z-ADD     FTTPDR        PA2PDR
014800000705     C                   MOVE      FTTTSR        PA2TSR
014900000705     C                   Z-ADD     FTTNDC        PA2NDC
015000000705     C                   Z-ADD     FTTDDC        PA2DDC
015100000705     C                   MOVE      'S'           PA2FCT
015200000705     C                   MOVE      'S'           PA2FVD
015300000705     C                   MOVE      'S'           PA2FVT
015400000705     C                   MOVEL     PARAM2        KPJBU
015500020404      *
015600000705     C     *IN38         IFEQ      *ON
015700011114     C                   CALL      'FICNE9C'                                    PER SIMULAZIONI
015800000705     C                   PARM                    KPJBA
015900020404      *
016000000705     C                   ELSE
016100020404      ****
016200020404      *   deve inviare un record in sede per segnalare la svalorizzazione
016300020404      *    solo a cambio di padroncino/data documento.
016400020404      ***
016500020404     c                   if        fttpdr <> savpdr or
016600020404     c                             fttddc <> savddc
016700020404     c                   eval      savpdr = fttpdr
016800020404     c                   eval      savddc = fttddc
016900020406     C                   CALL      'FICNE3R'                                    PER EFFETTIVI
017000020406     C                   PARM                    KPJBA
017100020404     c                   end
017200020404      *
017300011114     C                   CALL      'FICNE2R'                                    PER EFFETTIVI
017400000705     C                   PARM                    KPJBA
017500000705     C                   ENDIF
017600000705      *
017700000705     C                   Z-ADD     FTTDDC        WDDC
017800000705      *
017900000705      * CANCELLO EVENTUALE RECORD DI TOTALE
018000000705      *
018100011114     C   38KFTT3         CHAIN     fifst01L                           42
018200011114     C  N38KFTT3         CHAIN     fiftt01L                           42
018300000705     C     *IN42         IFEQ      *OFF
018400011114     C   38              DELETE    fifst1
018500011114     C  N38              DELETE    fiftt1
018600000705     C                   ENDIF
018700000705      *
018800000705     C                   ENDIF
018900000705     C                   ENDIF
019000000705     C                   ENDDO
019100000705      *
019200000705     C                   ENDDO
019300000704      *
019400000704      * FINE PROGRAMMA
019500000704      *
019600000704     C                   SETON                                        LR
