000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200980410      *--------------------------------------------------------------*
000300980410      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980410      *                - SBLOCCO DISTINTA -                          *
000500950315      *--------------------------------------------------------------*
000600980410     FFNCDA01L  UF   E           K DISK
000700950315     F                                     COMMIT
000800980422     FFNCDA03L  UF   E           K DISK
000900980422     F                                     COMMIT
001000980422     F                                     RENAME(FNCDA000:FNCDA003)
001100980428     FFNCDAW1L  UF   E           K DISK
001200950315      *--------------------------------------------------------------*
001300980410     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
001400980410      *
001500950315     D KPJBA         E DS
001600950315     D                SDS
001700950315     D  NOMPGM                 1     10
001800950315      *---------------------------------------------------------------*
001900950315     C     *ENTRY        PLIST
002000950315     C                   PARM                    KPJBA
002100980410     C                   MOVEL     KPJBU         FNLRK1
002200980324      *
002300980410      * Se ultima chimata chiudo, altrimenti elimino da FNCDA il numero distinta
002400980410     C     DK1TRI        IFEQ      'U'
002500980324     C                   COMMIT
002600980324     C                   SETON                                        LR
002700980324     C                   ELSE
002800980324      *
002900980410     C                   Z-ADD     DK1LNA        KLNA
003000980410     C                   Z-ADD     *ZEROS        KNDC
003100980422      *
003200980422      * TESTATA
003300980410     C     KCDA1         SETGT     FNCDA01L
003400980410     C     KLNA          READE     FNCDA01L                               31
003500980324      *
003600980324     C     *IN31         DOWEQ     '0'
003700980410     C                   Z-ADD     *ZEROS        CDANDC
003800980422     C                   MOVEL     *BLANKS       CDAFL3
003900980422     C                   MOVEL     *BLANKS       CDACCA
004000980410     C                   EXCEPT    AGGCDA
004100980410     C     KLNA          READE     FNCDA01L                               31
004200980324     C                   END
004300980422      *
004400980422      * DETTAGLIO
004500980422     C     KLNA          CHAIN     FNCDA03L                           31
004600980422      *
004700980422     C     *IN31         DOWEQ     '0'
004800980422     C                   Z-ADD     *ZEROS        CDANDC
004900980422     C                   MOVEL     *BLANKS       CDAFL3
005000980422     C                   MOVEL     *BLANKS       CDACCA
005100980422     C                   EXCEPT    AGGCD3
005200980422     C     KLNA          READE     FNCDA03L                               31
005300980422     C                   END
005400980324      *
005500980428      * Pulisco il file di work
005600980428     C     *LOVAL        SETLL     FNCDW000
005700980428      *
005800980428     C                   DO        *HIVAL
005900980428     C                   READ      FNCDW000                               31
006000980428     C  N31              DELETE    FNCDW000
006100980428     C  N31              END
006200980428      *
006300950317     C                   COMMIT
006400980324      *
006500950315     C     FINE          TAG
006600950315     C                   SETON                                        RT
006700980324     C                   END
006800980410      *-----------------------------------------------------*
006900980410      *  Operazioni iniziali
007000980410      *-----------------------------------------------------*
007100950315     C     *INZSR        BEGSR
007200980324      *
007300980410     C     KCDA1         KLIST
007400950315     C                   KFLD                    KLNA
007500950315     C                   KFLD                    KNDC
007600980324      *
007700980410     C     *LIKE         DEFINE    CDALNA        KLNA
007800980410     C     *LIKE         DEFINE    CDANDC        KNDC
007900980324      *
008000950315     C                   ENDSR
008100980324      *-----------------------------------------------------*
008200980410     OFNCDA000  E            AGGCDA
008300980410     O                       CDANDC
008400980422     O                       CDAFL3
008500980422     O                       CDACCA
008600980422     OFNCDA003  E            AGGCD3
008700980422     O                       CDANDC
008800980422     O                       CDAFL3
008900980422     O                       CDACCA
