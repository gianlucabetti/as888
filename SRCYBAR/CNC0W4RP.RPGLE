000100931110     H DECEDIT('0,') DATEDIT(*DMY.)
000200931110?     *--------------------------------------------------------------*
000300931110      * CNC0V4RP                                                     *
000400931110      *        CHIUSURA OPERATORE: SBLOCCO RECORD TASSAZIONE         *
000500931110      *                            PADRONCINI                        *
000600931110?     *--------------------------------------------------------------*
000700931110?     * I N D I C A T O R I                                          *
000800931110?     *--------------------------------------------------------------*
000900011121      * 80 - LOOP DI LETTURE SU FIFTT03L                             *
001000931110?     *--------------------------------------------------------------*
001100931110?     *   D A T A         B A S E                                    *
001200931110?     *--------------------------------------------------------------*
001300140212     FFIFTT00f  UF   E             DISK
001400140212     FFIFTT03L  iF   E           K DISK    infds(infttds) rename(fiftt000:ftt0)
001500931110?     *--------------------------------------------------------------*
001600931110?     *   DS                                                         *
001700931110?     *--------------------------------------------------------------*
001800140212     d infttds         ds
001900140212     d  rrnftt               397    400b 0
002000931110     D KPJBA         E DS
002001140225     D dftt01        E DS
002100931110     D*
002200931110     D*  DS x scomposizione parametri passati
002300931110     D PARAM           DS
002400931110     D  PFLE                   1      3
002500931110     D  PCUC                   4      6
002600931110     D  PNMI                   7     13
002700931110?     *--------------------------------------------------------------*
002800931110?     *   C I C L O         P R I N C I P A L E                      *
002900931110?     *--------------------------------------------------------------*
003000931110      *  Operazioni iniziali
003100931110     C                   EXSR      DEFCAM
003200931110      *
003300931110      *  Loop di letture su FLFTT03L x codice operatore
003400931110      *  tipo servizio 'C' (consegna)
003500140212     C     KEYFTT        setll     FIFTT03L
003600140212     c                   do        *hival
003700140212     C     KEYFTT        reade     FIFTT03L
003800140212     C                   if        %eof(fiftt03l)
003900140212     c                   leave
004000140212     c                   end
004001140225     c                   movel     fttflr        dftt01
004100140225     c                   if        §fttpou <> pfle
004200140212     c                   iter
004300140212     c                   end
004400140212     c     rrnftt        chain     fiftt00f
004500140212     c                   if        %found(fiftt00f)
004600140212     C                   MOVE      ' '           FTTFVL
004700931110     C                   EXCEPT    AGGFVL
004800140212     c                   end
004900140212     C                   ENDdo                                                  *IN80 DOWE
005000931110     C*
005100931110     C                   SETON                                        LR
005200931110?     *--------------------------------------------------------------*
005300931110?     *  DEFCAM: Operazioni iniziali                                 *
005400931110?     *--------------------------------------------------------------*
005500931110     C     DEFCAM        BEGSR
005600931110      *
005700931110     C     *ENTRY        PLIST
005800931110     C                   PARM                    KPJBA
005900931110     C                   MOVEL     KPJBU         PARAM
006100931110      *
006200931110     C     KEYFTT        KLIST
006300931110     C                   KFLD                    FTWCUC
006400931110     C                   KFLD                    FTWTSR
006500931110      *
006600931110      *  Definizione variabili
006700931110     C     *LIKE         DEFINE    FTTCUC        FTWCUC
006800931110     C     *LIKE         DEFINE    FTTTSR        FTWTSR
006900140212     C     *LIKE         DEFINE    FTTfgs        FTWfgs
007000931110      *
007100931110      *  Inizializzazione variabili
007200931110     C                   MOVE      PCUC          FTWCUC
007300931110     C                   MOVE      'C'           FTWTSR
007400931110      *
007500931110     C                   ENDSR
007600931110?     *--------------------------------------------------------------*
007700931110?     *  EXCPT x aggiornamento numero mandato di introito            *
007800931110?     *--------------------------------------------------------------*
007900011121     OFIFTT000  E            AGGFVL
008000931110     O                       FTTFVL
