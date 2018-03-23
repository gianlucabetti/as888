000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011212     H* TRmz21R *-OC/38-*--------------------------------------------*
000300000000     H*-----------------*                                            *
000400000000     H*             - PULIZIA PICCOLA MANUTENZIONE - FILTRO -        *
000500000000     H*--------------------------------------------------------------*
000600011212     FTRmz21D   CF   E             WORKSTN
000700000000     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
000800000000     D PARAM2          DS
000900000000     D  VDAT                   1      6  0
001000000000     D KPJBA         E DS
001100000000     D  LIBSYS                92    100
001200000000     D  MSGERR               453    502
001300000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001400000000     D  TCU                  398    697
001500000000     D                                     DIM(50)                              TAB FLAG CNT
001600000000     D  KCU                  698    847P 0
001700000000     D                                     DIM(50)                              TAB CAPO CONTO
001800000000     D                                     PACKEVEN
001900000000     D  DTU                  848    895P 0
002000000000     D                                     DIM(12)                              DATE PARAM
002100000000     D                                     PACKEVEN
002200000000     I/SPACE 3
002300000000     C*---------------------------------------------------------------*
002400000000     C     *ENTRY        PLIST
002500000000     C                   PARM                    KPJBA
002600000000     C                   Z-ADD     1             CODUT
002700000000     C                   CALL      'XPARUT'
002800000000     C                   PARM                    UTEDSE
002900000000     C     CODUT         CABEQ     -1            FINE
003000000000     C                   MOVEL     RAGUT         RSUT             20
003100000000     C                   MOVEL     RAGUT         RSUT2            25
003200000000     C*---------------------------------------------------------------*
003300000000     C                   Z-ADD     UDATE         VDAT
003400000000     C     SU            TAG
003500000000     C                   EXFMT     GA21D001
003600000000     C   KL              GOTO      FINE
003700000000     C     VDAT          CABEQ     0             SU
003800000000     C     VDAT          IFNE      0
003900000000     C                   Z-ADD     VDAT          XDTGMA
004000000000     C                   Z-ADD     3             XDTSTA
004100000000     C                   EXSR      XDATA
004200000000     C     XDTSTA        IFEQ      -1
004300000000     C                   SETON                                        70
004400000000     C                   GOTO      SU
004500000000     C                   END
004600000000     C                   END
004700000000     C*CONTROLLI SUPERATI
004800000000     C                   SETON                                        98
004900000000     C   98
005000000000     CANNKA              GOTO      SU
005100011212     C                   MOVEL     'MZ22'        KCOAZ
005200000000     C                   MOVEL     PARAM2        KPJBU
005300000000     C                   CALL      'BCH09'
005400000000     C                   PARM                    KPJBA
005500000000     C     KNMUS         IFEQ      *ALL'1'
005600011212     C                   CALL      'TRMZ22C'
005700000000     C                   PARM                    KPJBA
005800000000     C                   ELSE
005900000000     C                   CALL      'BCH10'
006000000000     C                   PARM                    KPJBA
006100000000     C                   END
006200000000     C     FINE          TAG
006300000000     C                   SETON                                        LR
006400000000     C******************************************************
006500000000     C     XDATA         BEGSR
006600000000     C**------------------------
006700000000     C** ROUTINE CONTROLLO DATA
006800000000     C**------------------------
00690000000001020C     XDTSTA        CABGT     4             XDTERR
00700000000001040C     XDTSTA        CABLT     1             XDTERR
00710000000001060C     XDTSTA        CABGT     2             XDTEI
00720000000001080C*    GIRA DATA INTERNA IN ESTERNA
00730000000001100C     XDTAMG        DIV       100           XDTMM             2 0          * M-->XDTMM
00740000000001120C     XDTAMG        DIV       10000         XDTAA             2 0          * A-->XDTAA
00750000000001140C                   MOVE      XDTAMG        XDTGG             2 0          * G-->XDTGG
00760000000001160C                   MOVE      XDTAMG        XDTGMA                         * M--> XDTGMA
00770000000001180C                   MOVEL     XDTGG         XDTGMA            6 0          * G--> XDTGMA
00780000000001200C                   MOVE      XDTAA         XDTGMA                         * A--> XDTGMA
00790000000002020C                   GOTO      XDTCON
00800000000002030C     XDTEI         TAG
00810000000002040C*    GIRA DATA ESTERNA IN INTERNA
00820000000002060C     XDTGMA        DIV       100           XDTMM                          * M--> XDTMM
00830000000002080C     XDTGMA        DIV       10000         XDTGG                          * G--> XDTGG
00840000000002100C                   MOVE      XDTGMA        XDTAA                          * A--> XDTAA
00850000000002120C                   MOVE      XDTGMA        XDTAMG            6 0          * M--> XDTAMG
00860000000002140C                   MOVEL     XDTAA         XDTAMG                         * A--> XDTAMG
00870000000002160C                   MOVE      XDTGG         XDTAMG                         * G--> XDTAMG
00880000000002180C                   SUB       2             XDTSTA            1 0          *PREPARA XDTSTA
00890000000002190C*                                                    * PER TEST CONT
00900000000002200C     XDTCON        TAG
00910000000003020C     XDTSTA        CABNE     1             XDTOK                          * XDTSTA NON 1
00920000000003030C*                                                    *SALTA CONTROL
00930000000003040C*
00940000000003050C*    CONTROLLA LA VALIDITA* DELLA DATA
00950000000003070C     XDTGG         CABLT     1             XDTERR
00960000000003090C     XDTMM         CABLT     1             XDTERR
00970000000003110C     XDTMM         CABGT     12            XDTERR
00980000000003130C     XDTAA         DIV       4             XDTRES            2 0          * INDIVIDUA
00990000000003150C                   MVR                     XDTRES                         * ANNO BISEST.
01000000000003170C     XDTRES        IFEQ      0
01010000000003190C                   Z-ADD     29            XDTF(2)                        * AGGIUSTA
01020000000004020C                   ELSE                                                   * FINE FEBBRAIO
01030000000004040C                   Z-ADD     28            XDTF(2)                        * IN ARRAY XDTF
01040000000004060C                   END
01050000000004080C     XDTGG         CABGT     XDTF(XDTMM)   XDTERR                         * GIORNO DATA >
01060000000004090C*                                                    * FINE MESE
01070000000004100C     XDTOK         TAG
01080000000004110C*    FINE NORMALE
01090000000004120C                   SUB       XDTSTA        XDTSTA
01100000000004140C                   GOTO      XDTFIN
01110000000004160C     XDTERR        TAG
01120000000004170C*    DATA O RICHIESTA ERRATA
01130000000004190C                   Z-SUB     1             XDTSTA
01140000000015020C     XDTFIN        TAG
01150000000005050C                   ENDSR
011600000000**
011700000000312831303130313130313031
