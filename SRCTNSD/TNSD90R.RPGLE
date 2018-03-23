000100950222     H* TNSD90R *-----------------------------------------------------*
000200930225     H*        - PULIZIA  STAMPA VARIAZIONI NON EFFETTUATE -          *
000300930225     H*---------------------------------------------------------------*
000400930225     H DECEDIT('0,') DATEDIT(*DMY.)
000500991123     FFIARBE1C  UF   E           K DISK
000600021219     FFIARBU1E  UF   E           K DISK
000700021206     ***FAZORG01L  IF   E           K DISK
000800930225     D*
000900930225     D KPJBA         E DS
001000000710     D OG143         E DS
001100930225     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200930225     D*
001300920113     D WLBDAT          DS
001400950222     D  G02DAT                 1      8  0
001500950222     D  G02INV                 9     16  0
001600950222     D  G02ERR                17     17
001700950222     D  G02TGI                18     22  0
001800920602     D WGIDAT          DS
001900950222     D  GIODAT                 1      8  0
002000950222     D  GIOINV                 9     16  0
002100950222     D  GIOTGI                17     21  0
002200930225     I*
002300950222     IFNARBD00      11
002400950222     IFNARBK00      12
002500991123     IFIARBT00      13
002600950222     IFNARBM00      15
002700950222     IFNARBV00      16
002800950222     IFNARBG00      17
002900991123     IFNARBP00      14
003000930225     C*---------------------------------------------------------------*
003100930225     C* INDICATORI USATI                                              *
003200930225     C*---------------------------------------------------------------*
003300991123     C* 11/17 - CONDIZIONA OPERAZIONI SUI DIVERSI FORMATI DI FIARBE1C
003400991123     C* 30    - LETTURA FIARBE1C PER FILIALE UGUALE A ZERO
003500991123     C* 32    - LETTURA FIARBE1C PER FINE FILE
003600000710     C* 39    - CHAIN AZORG PER VEDERE SE LNP = POSTE
003700930225     C*---------------------------------------------------------------*
003800021219      *
003900021219     c     K05ARBU0E     klist
004000021219     c                   kfld                    ARBaas
004100021219     c                   kfld                    ARBlnp
004200021219     c                   kfld                    ARBnrs
004300021219     c                   kfld                    ARBnsp
004400021219     c                   kfld                    ARBtrc
004500021219      *
004600920113     C     *ENTRY        PLIST
004700920113     C                   PARM                    KPJBA
004800920219     C*
004900920113     C                   Z-ADD     1             CODUT
005000920113     C                   CALL      'X§PARUT'
005100920113     C                   PARM                    UT§DSE
005200920113     C                   MOVEL     RAGUT         RSUT             20
005300920218     C*
005400950222     C* DATA PULIZIA  = DATA GIORNO - 40
005500950222     C                   TIME                    W0140            14 0
005600950222     C                   MOVE      W0140         G02DAT
005700950222     C                   MOVE      *ZEROS        G02INV
005800920602     C                   MOVEL     *BLANK        G02ERR
005900950222     C                   CALL      'XSRDA8'
006000920602     C                   PARM                    WLBDAT
006100920602     C*
006200931231     C     G02TGI        SUB       40            GIOTGI
006300920602     C*
006400950222     C                   CALL      'XSRGI8'
006500920602     C                   PARM                    WGIDAT
006600950222     C                   MOVEL     GIOINV        DATPUL            8 0
006700930225     C*
006800920602     C                   Z-ADD     0             VIDFEV            3 0
006900920218     C*
007000930225     C* A R R I V I
007100930225     C*
007200920218     C* INIZIO LETTURA SE NON C'E FIL ELABORATORE
007300920218     C     *IN32         DOWEQ     '0'
007400920602     C                   SETOFF                                       111213
007500920602     C                   SETOFF                                       141530
007600950222     C                   SETOFF                                       1617
007700920602     C*
007800991123     C     VIDFEV        SETGT     FIARBE1C
007900991123     C                   READ      FIARBE1C                               32
008000920218     C*
008100920218     C  N32              DO
008200991123     C                   Z-ADD     ARBFEV        VIDFEV
008300920123     C*
008400920305     C*
008500950222     C     *IN30         DOWEQ     '0'
008600991123     C                   Z-ADD     ARBDAT        COMDAT            8 0
008700950222     C     COMDAT        IFGT      DATPUL
008800920218     C                   SETON                                            30
008900920602     C                   ELSE
009000920602     C*
009100000710     C* .. PULISCO SOLO SE MINORE DI ZERO
009200950222     C     COMDAT        IFNE      0
009300021206     *** * .. E SOLO LE LNP NON POSTE
009400021206     *** * Non più questa "esclusione" (dal 06/12/2002) !!!
009500021206     ***C                   CLEAR     §OGPT
009600021206     ***C     ARBLNP        CHAIN     AZORG01L                           39
009700021206     ***C  N39              MOVEL     ORGDE3        OG143
009800021206     ***C     §OGPT         IFNE      'S'
009900950222     C   11              DELETE    FNARBD00
010000950222     C   12              DELETE    FNARBK00
010100991123     C   13              DELETE    FIARBT00
010200991123     C   14              DELETE    FNARBP00
010300950222     C   15              DELETE    FNARBM00
010400950222     C   16              DELETE    FNARBV00
010500950222     C   17              DELETE    FNARBG00
010600021219     c                   if        *in13
010700021219     c                   exsr      DelArbU0E
010800021219     c                   endif
010900021206     ***C                   ENDIF
011000000710     C                   ENDIF
011100920113     C*
011200920113     C                   SETOFF                                       111213
011300920602     C                   SETOFF                                       1415
011400950222     C                   SETOFF                                       1617
011500920602     C*
011600991123     C     VIDFEV        READE     FIARBE1C                               30
011700950222     C                   END
011800950222     C                   ENDDO
011900920218     C                   END
012000920602     C                   END
012100930225     C*
012200930225     C                   SETON                                        LR
012300021219      *****************************************************************
012400021219      * Cancellazione record di KEY-bolla dal file FIARBU0E           *
012500021219      *****************************************************************
012600021219     c     DelArbU0E     BEGSR
012700021219      *
012800021219     c     K05ARBU0E     chain     FIARBU00                           53
012900021219      *
013000021219     c                   dow       not *in53
013100021219     c                   delete    FIARBU00
013200021219      *
013300021219     c     K05ARBU0E     reade     FIARBU00                               53
013400021219     c                   enddo
013500021219      *
013600021219     c                   ENDSR
