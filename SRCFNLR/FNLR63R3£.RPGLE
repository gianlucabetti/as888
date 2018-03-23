000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300950315      * FNLR63R3*                                                    *
000400950315      *---------*                                                    *
000500950315      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *--------------------------------------------------------------*
000700950320     FFNCDE01L  UF   E           K DISK    USROPN
000800950315     F                                     COMMIT
000900950320     FFNARB01L  UF   E           K DISK    USROPN
001000950315     F                                     COMMIT
001100950320     FFNFVV01L  UF   E           K DISK    USROPN
001200950315     F                                     COMMIT
001300980324     FFNCDS03L  UF   E           K DISK    USROPN
001400980324     F                                     COMMIT
001500950315      *--------------------------------------------------------------*
001600950315      *  DS
001700950315      *--------------------------------------------------------------*
001800950315     D FNLR63        E DS                  EXTNAME(FNLR63DS)
001801020902     D FNLv55ds      E DS
001900950315     D*
002000950315     D KPJBA         E DS
002100950315     D*
002200950315     D                SDS
002300950315     D  NOMPGM                 1     10
002301020904     D  dataiso        s               d
002400950315      *---------------------------------------------------------------*
002500950315      * FLUSSO PRINCIPALE                                             *
002600950315      *---------------------------------------------------------------*
002700950315     C     *ENTRY        PLIST
002800950315     C                   PARM                    KPJBA
002900950315     C                   MOVEL     KPJBU         FNLR63
002901020902     c* controllo se il po che mi arriva è gestito da qualcun'altro
002902020902     c                   exsr      srfgs
003000980324      *
003100980324      *  Se ultima chimata chiudo, altrimenti elimino da FNCDE, FNCDS  e FNARB
003200980324      *   il numero distinta
003300980324      *
003400980324     C     D63TRI        IFEQ      'U'
003500980324     C                   COMMIT
003600980324     C                   SETON                                        LR
003700980324     C                   ELSE
003800980324      *
003900950315     C                   Z-ADD     D63LNA        KLNA
004000950315     C                   Z-ADD     0             KNDC
004100950315     C                   Z-ADD     0             SAVNDC
004200950317     C     KCDE1         SETGT     FNCDE01L
004300950315     C     KLNA          READE     FNCDE01L                               31
004400980324      *
004500980324      *  Cancello record FNFVV  - fogli vari -
004600980402     C  N31              EXSR      DLTFVV
004700980324      *  Loop per aggiornamenti su FNARB, FNCDE e FNCDS
004800980324     C     *IN31         DOWEQ     '0'
004900950315     C     SAVNDC        ANDEQ     CDENDC
005000950315     C                   EXSR      UPDARB
005100950315     C     KLNA          READE     FNCDE01L                               31
005200980324     C                   END
005300980324      *
005400950317     C                   COMMIT
005500980324      *
005600950315     C     FINE          TAG
005700950315     C                   SETON                                        RT
005800980324     C                   END
005900950315      *---------------------------------------------------------------*
006000980324      * Aggiorno i record  di FNARB/FNCDE/FNCDS                       *
006100950315      *---------------------------------------------------------------*
006200950315     C     UPDARB        BEGSR
006300950315      *
006400950315      * Aggiorno FNARB
006500950315     C                   Z-ADD     CDEAAS        KAAS
006600950315     C                   Z-ADD     CDELNP        KLNP
006700950315     C                   Z-ADD     CDENRS        KNRS
006800950315     C                   Z-ADD     CDENSP        KNSP
006900950315     C     KARB          CHAIN     FNARB01L                           32
007000980324      *
007100950315     C     *IN32         IFEQ      '0'
007200950315     C     ARBNDC        ANDEQ     CDENDC
007300020830     C                   Z-ADD     0             ARBIFP
007400020830     C                   Z-ADD     0             ARBNDC
007500950315     C                   Z-ADD     0             ARBDDC
007600950315     C                   Z-ADD     0             ARBPDC
007700950315     C                   EXCEPT    AGGARB
007800950315     C                   END
007900980324      *
008000980401      * Se CDEFL3 = 1 "normalizzo" FNCDE in caso contrario aggiorno FNCDE
008100980401     C     CDEFL3        IFEQ      '1'
008200980401     C     *IN32         ANDEQ     '0'
008300980401     C                   EXSR      MUOVI
008400980401     C                   ELSE
008500950315     C                   Z-ADD     0             CDENDC
008600950315     C                   EXCEPT    AGGCDE
008700980401     C                   END
008800980401      *
008900980324      *
009000980324      * Aggiorno FNCDS
009100980401      *
009200980401     C     KARB          CHAIN     FNCDS000                           32
009300980324     C     *IN32         DOWEQ     '0'
009400980324     C     SAVNDC        IFEQ      CDSNDC
009500980324     C                   Z-ADD     0             CDSNDC
009600980324     C                   EXCEPT    AGGCDS
009700980324     C                   END
009800980324     C     KARB          READE     FNCDS000                               32
009900980324     C                   END
010000980324      *
010100950315     C                   ENDSR
010200950315      *---------------------------------------------------------------*
010300950315      * Deleto i record di FNFVV                                      *
010400950315      *---------------------------------------------------------------*
010500950315     C     DLTFVV        BEGSR
010600950315      *
010700950315     C                   MOVEL     '4'           KNPG
010800950315     C                   Z-ADD     CDENDC        KNFV
010900020902     C                   Z-ADD     d55tfa        KFGS
011000950320     C     KFVV          CHAIN     FNFVV01L                           32
011100950320     C  N32              DELETE    FNFVV01L
011200950315     C                   Z-ADD     CDENDC        SAVNDC
011300980324      *
011400950315     C                   ENDSR
011500980401      *---------------------------------------------------------------*
011600980401      * Muovo da FNCDS ad FNCDE per aggiornamento
011700980401      *---------------------------------------------------------------*
011800980401     C     MUOVI         BEGSR
011900980401      *
012000980401     C                   Z-ADD     *ZEROS        CDEDCM
012100980401     C                   Z-ADD     *ZEROS        CDEHMC
012200980401     C                   Z-ADD     *ZEROS        CDEDLA
012300980401     C                   Z-ADD     *ZEROS        CDEDAG
012400980401     C                   Z-ADD     *ZEROS        CDEDRC
012500980402     C                   Z-ADD     *ZEROS        CDENDC
012600980401     C                   MOVEL     *BLANKS       CDENO1
012700980401     C                   MOVEL     *BLANKS       CDENO2
012800980401     C                   MOVEL     *BLANKS       CDECMC
012900980401     C                   MOVEL     *BLANKS       CDECCA
013000000210     C                   MOVEL     *BLANKS       CDETC1
013100980401     C                   MOVEL     *BLANKS       CDEERR
013200980401     C                   MOVEL     *BLANKS       CDESTS
013300980401     C                   MOVEL     *BLANKS       CDECOD
013400980401     C                   MOVEL     *BLANKS       CDEMOT
013500980401     C                   MOVEL     *BLANKS       CDEFL1
013600980401     C                   MOVEL     *BLANKS       CDEFL2
013700980401     C                   MOVEL     *BLANKS       CDEFL3
013800980401      *
013900980401     C                   UPDATE    FNCDE000
014000980401      *
014100980401     C                   ENDSR
014200950315     C*-----------------------------------------------------*
014300950315     C*  Operazioni iniziali
014400950315     C*-----------------------------------------------------*
014500950315     C     *INZSR        BEGSR
014600980324      *
014700980324      * Apertura controllata dei files
014800950315     C                   OPEN      FNCDE01L                             16
014900950320     C  N16              OPEN      FNFVV01L                             16
015000950315     C  N16              OPEN      FNARB01L                             16
015100980324     C  N16              OPEN      FNCDS03L                             16
015200950315     C   16              GOTO      FINE                                         OPEN vado a fine
015300980324      *
015400980324      *  Definizione chiavi
015500950315     C     KARB          KLIST
015600950315     C                   KFLD                    KAAS
015700950315     C                   KFLD                    KLNP
015800950315     C                   KFLD                    KNRS
015900950315     C                   KFLD                    KNSP
016000980324      *
016100950315     C     KFVV          KLIST
016200950315     C                   KFLD                    KNPG
016300950315     C                   KFLD                    KNFV
016400950315     C                   KFLD                    KFGS
016500980324      *
016600950317     C     KCDE1         KLIST
016700950315     C                   KFLD                    KLNA
016800950315     C                   KFLD                    KNDC
016900980324      *
017000980324      *  Definizione delle variabili
017100950315     C     *LIKE         DEFINE    ARBAAS        KAAS
017200950315     C     *LIKE         DEFINE    ARBLNP        KLNP
017300950315     C     *LIKE         DEFINE    ARBNRS        KNRS
017400950315     C     *LIKE         DEFINE    ARBNSP        KNSP
017500950315     C     *LIKE         DEFINE    FVVNPG        KNPG
017600950315     C     *LIKE         DEFINE    FVVNFV        KNFV
017700950315     C     *LIKE         DEFINE    FVVFGS        KFGS
017800950315     C     *LIKE         DEFINE    CDELNA        KLNA
017900950315     C     *LIKE         DEFINE    CDENDC        KNDC
018000950317     C     *LIKE         DEFINE    CDENDC        KNDC0
018100950315     C     *LIKE         DEFINE    CDENDC        SAVNDC
018200980324      *
018300950315     C                   ENDSR
018301020902      *---------------------------------------------------------*
018302020902      *  reperisce il po di gestione                            *
018303020902      *---------------------------------------------------------*
018304020902     C     srfgs         BEGSR
018305020902     C*
018306020902     c                   clear                   fnlv55ds
018307020902     c                   eval      d55tpt = '6'
018308020902     c                   movel     d63lna        d55lin
018309020902     c                   movel     *date         dataiso
018310020902     c                   movel     dataiso       d55drf
018311020902     c                   call      'FNLV55R'
018312020902     c                   parm                    fnlv55ds
018313020902     C*
018314020902     C                   ENDSR
018400980324      *-----------------------------------------------------*
018500950315     OFNCDE000  E            AGGCDE
018600950315     O                       CDENDC
018700980324     OFNCDS000  E            AGGCDS
018800980324     O                       CDSNDC
018900950315     OFNARB000  E            AGGARB
019000020830     O                       ARBIFP
019100020830     O                       ARBNDC
019200950315     O                       ARBPDC
019300950315     O                       ARBDDC
