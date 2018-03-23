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
001300080116     FFidst01L  UF   E           K DISK    USROPN
001400080116     F                                     COMMIT
001500980324     FFNCDS03L  UF   E           K DISK    USROPN
001600980324     F                                     COMMIT
001700950315      *--------------------------------------------------------------*
001800950315      *  DS
001900950315      *--------------------------------------------------------------*
002000950315     D FNLR63        E DS                  EXTNAME(FNLR63DS)
002100020902     D FNLv55ds      E DS
002200950315     D*
002300950315     D KPJBA         E DS
002400950315     D*
002500950315     D                SDS
002600950315     D  NOMPGM                 1     10
002700020904     D  dataiso        s               d
002800950315      *---------------------------------------------------------------*
002900950315      * FLUSSO PRINCIPALE                                             *
003000950315      *---------------------------------------------------------------*
003100950315     C     *ENTRY        PLIST
003200950315     C                   PARM                    KPJBA
003300950315     C                   MOVEL     KPJBU         FNLR63
003400020902     c* controllo se il po che mi arriva è gestito da qualcun'altro
003500020902     c                   exsr      srfgs
003600980324      *
003700980324      *  Se ultima chimata chiudo, altrimenti elimino da FNCDE, FNCDS  e FNARB
003800980324      *   il numero distinta
003900980324      *
004000980324     C     D63TRI        IFEQ      'U'
004100980324     C                   COMMIT
004200980324     C                   SETON                                        LR
004300980324     C                   ELSE
004400980324      *
004500950315     C                   Z-ADD     D63LNA        KLNA
004600950315     C                   Z-ADD     0             KNDC
004700950315     C                   Z-ADD     0             SAVNDC
004800950317     C     KCDE1         SETGT     FNCDE01L
004900950315     C     KLNA          READE     FNCDE01L                               31
005000980324      *
005100080116      *  Cancello record FNFVV  - fogli vari - + fidst nuovo distinte
005200980402     C  N31              EXSR      DLTFVV
005300980324      *  Loop per aggiornamenti su FNARB, FNCDE e FNCDS
005400980324     C     *IN31         DOWEQ     '0'
005500950315     C     SAVNDC        ANDEQ     CDENDC
005600950315     C                   EXSR      UPDARB
005700950315     C     KLNA          READE     FNCDE01L                               31
005800980324     C                   END
005900980324      *
006000950317     C                   COMMIT
006100980324      *
006200950315     C     FINE          TAG
006300950315     C                   SETON                                        RT
006400980324     C                   END
006500950315      *---------------------------------------------------------------*
006600980324      * Aggiorno i record  di FNARB/FNCDE/FNCDS                       *
006700950315      *---------------------------------------------------------------*
006800950315     C     UPDARB        BEGSR
006900950315      *
007000950315      * Aggiorno FNARB
007100950315     C                   Z-ADD     CDEAAS        KAAS
007200950315     C                   Z-ADD     CDELNP        KLNP
007300950315     C                   Z-ADD     CDENRS        KNRS
007400950315     C                   Z-ADD     CDENSP        KNSP
007500950315     C     KARB          CHAIN     FNARB01L                           32
007600980324      *
007700950315     C     *IN32         IFEQ      '0'
007800950315     C     ARBNDC        ANDEQ     CDENDC
007900020830     C                   Z-ADD     0             ARBIFP
008000020830     C                   Z-ADD     0             ARBNDC
008100950315     C                   Z-ADD     0             ARBDDC
008200950315     C                   Z-ADD     0             ARBPDC
008300950315     C                   EXCEPT    AGGARB
008400950315     C                   END
008500980324      *
008600980401      * Se CDEFL3 = 1 "normalizzo" FNCDE in caso contrario aggiorno FNCDE
008700980401     C     CDEFL3        IFEQ      '1'
008800980401     C     *IN32         ANDEQ     '0'
008900980401     C                   EXSR      MUOVI
009000980401     C                   ELSE
009100950315     C                   Z-ADD     0             CDENDC
009200950315     C                   EXCEPT    AGGCDE
009300980401     C                   END
009400980401      *
009500980324      *
009600980324      * Aggiorno FNCDS
009700980401      *
009800980401     C     KARB          CHAIN     FNCDS000                           32
009900980324     C     *IN32         DOWEQ     '0'
010000980324     C     SAVNDC        IFEQ      CDSNDC
010100980324     C                   Z-ADD     0             CDSNDC
010200980324     C                   EXCEPT    AGGCDS
010300980324     C                   END
010400980324     C     KARB          READE     FNCDS000                               32
010500980324     C                   END
010600980324      *
010700950315     C                   ENDSR
010800950315      *---------------------------------------------------------------*
010900950315      * Deleto i record di FNFVV                                      *
011000950315      *---------------------------------------------------------------*
011100950315     C     DLTFVV        BEGSR
011200950315      *
011300950315     C                   MOVEL     '4'           KNPG
011400080116     C                   Z-ADD     CDENDC        KNFV6
011500950315     C                   Z-ADD     CDENDC        KNFV
011600020902     C                   Z-ADD     d55tfa        KFGS
011700950320     C     KFVV          CHAIN     FNFVV01L                           32
011800950320     C  N32              DELETE    FNFVV01L
011900080116     C     Kdst          CHAIN     Fidst01L                           32
012000080116     C  N32              DELETE    Fidst01L
012100950315     C                   Z-ADD     CDENDC        SAVNDC
012200980324      *
012300950315     C                   ENDSR
012400980401      *---------------------------------------------------------------*
012500980401      * Muovo da FNCDS ad FNCDE per aggiornamento
012600980401      *---------------------------------------------------------------*
012700980401     C     MUOVI         BEGSR
012800980401      *
012900980401     C                   Z-ADD     *ZEROS        CDEDCM
013000980401     C                   Z-ADD     *ZEROS        CDEHMC
013100980401     C                   Z-ADD     *ZEROS        CDEDLA
013200980401     C                   Z-ADD     *ZEROS        CDEDAG
013300980401     C                   Z-ADD     *ZEROS        CDEDRC
013400980402     C                   Z-ADD     *ZEROS        CDENDC
013500980401     C                   MOVEL     *BLANKS       CDENO1
013600980401     C                   MOVEL     *BLANKS       CDENO2
013700980401     C                   MOVEL     *BLANKS       CDECMC
013800980401     C                   MOVEL     *BLANKS       CDECCA
013900000210     C                   MOVEL     *BLANKS       CDETC1
014000980401     C                   MOVEL     *BLANKS       CDEERR
014100980401     C                   MOVEL     *BLANKS       CDESTS
014200980401     C                   MOVEL     *BLANKS       CDECOD
014300980401     C                   MOVEL     *BLANKS       CDEMOT
014400980401     C                   MOVEL     *BLANKS       CDEFL1
014500980401     C                   MOVEL     *BLANKS       CDEFL2
014600980401     C                   MOVEL     *BLANKS       CDEFL3
014700980401      *
014800980401     C                   UPDATE    FNCDE000
014900980401      *
015000980401     C                   ENDSR
015100950315     C*-----------------------------------------------------*
015200950315     C*  Operazioni iniziali
015300950315     C*-----------------------------------------------------*
015400950315     C     *INZSR        BEGSR
015500980324      *
015600980324      * Apertura controllata dei files
015700950315     C                   OPEN      FNCDE01L                             16
015800950320     C  N16              OPEN      FNFVV01L                             16
015900080116     C  N16              OPEN      Fidst01L                             16
016000950315     C  N16              OPEN      FNARB01L                             16
016100980324     C  N16              OPEN      FNCDS03L                             16
016200950315     C   16              GOTO      FINE                                         OPEN vado a fine
016300980324      *
016400980324      *  Definizione chiavi
016500950315     C     KARB          KLIST
016600950315     C                   KFLD                    KAAS
016700950315     C                   KFLD                    KLNP
016800950315     C                   KFLD                    KNRS
016900950315     C                   KFLD                    KNSP
017000980324      *
017100950315     C     KFVV          KLIST
017200950315     C                   KFLD                    KNPG
017300950315     C                   KFLD                    KNFV
017400950315     C                   KFLD                    KFGS
017500080116     C     Kdst          KLIST
017600080116     C                   KFLD                    KNPG
017700080116     C                   KFLD                    KNFV6             6 0
017800080116     C                   KFLD                    KFGS
017900980324      *
018000950317     C     KCDE1         KLIST
018100950315     C                   KFLD                    KLNA
018200950315     C                   KFLD                    KNDC
018300980324      *
018400980324      *  Definizione delle variabili
018500950315     C     *LIKE         DEFINE    ARBAAS        KAAS
018600950315     C     *LIKE         DEFINE    ARBLNP        KLNP
018700950315     C     *LIKE         DEFINE    ARBNRS        KNRS
018800950315     C     *LIKE         DEFINE    ARBNSP        KNSP
018900950315     C     *LIKE         DEFINE    FVVNPG        KNPG
019000950315     C     *LIKE         DEFINE    FVVNFV        KNFV
019100950315     C     *LIKE         DEFINE    FVVFGS        KFGS
019200950315     C     *LIKE         DEFINE    CDELNA        KLNA
019300950315     C     *LIKE         DEFINE    CDENDC        KNDC
019400950317     C     *LIKE         DEFINE    CDENDC        KNDC0
019500950315     C     *LIKE         DEFINE    CDENDC        SAVNDC
019600980324      *
019700950315     C                   ENDSR
019800020902      *---------------------------------------------------------*
019900020902      *  reperisce il po di gestione                            *
020000020902      *---------------------------------------------------------*
020100020902     C     srfgs         BEGSR
020200020902     C*
020300020902     c                   clear                   fnlv55ds
020400020902     c                   eval      d55tpt = '6'
020500020902     c                   movel     d63lna        d55lin
020600020902     c                   movel     *date         dataiso
020700020902     c                   movel     dataiso       d55drf
020800020902     c                   call      'FNLV55R'
020900020902     c                   parm                    fnlv55ds
021000020902     C*
021100020902     C                   ENDSR
021200980324      *-----------------------------------------------------*
021300950315     OFNCDE000  E            AGGCDE
021400950315     O                       CDENDC
021500980324     OFNCDS000  E            AGGCDS
021600980324     O                       CDSNDC
021700950315     OFNARB000  E            AGGARB
021800020830     O                       ARBIFP
021900020830     O                       ARBNDC
022000950315     O                       ARBPDC
022100950315     O                       ARBDDC
