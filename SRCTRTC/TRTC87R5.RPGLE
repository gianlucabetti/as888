000100961007     H DECEDIT('0,') DATEDIT(*DMY.)
000200961007     H*---------------------------------------------------------------*
000300961007     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -          *
000400961007     F*---------------------------------------------------------------*
000500961007     F*  DATA BASE                                                    *
000600961007     F*---------------------------------------------------------------*
000700961011     FEDSUM01L  UF   E           K DISK
000800961007     F*---------
000900980320     FEDSUM04L  UF   E           K DISK
001000980320     F                                     RENAME(EDSUM000:EDSUMLNP)
001100980320     F*---------
001200961025     FEDBRV02L  UF   E           K DISK
001300980320     F*---------
001400980320     FEDBRV03L  UF   E           K DISK
001500980320     F                                     RENAME(EDBRV000:EDBRVLNP)
001600961007     D*---------------------------------------------------------------------*
001700961007     D* DS
001800961007     D*---------------------------------------------------------------------*
001900961007     D KPJBA         E DS
002000961007     D TRTC87        E DS                  EXTNAME(TRTC87DS)
002100961007     C*---------------------------------------------------------------------*
002200961007     C*       C I C L O         P R I N C I P A L E
002300961007     C*---------------------------------------------------------------------*
002400980320     C     D87LNP        IFEQ      0
002500961007     C*  Eseguo posizionamento iniziale in base al codice cliente
002600961007     C                   Z-ADD     D87CCM        KCCM
002700961011     C     KCCM          CHAIN     EDSUM01L                           30
002800961007     C     *IN30         DOWEQ     '0'
002900961011     C*  Sfleggo record annullati
003000961011     C*  Sfleggo ERRORI
003100961011     C     SUMFLG        IFNE      *BLANKS
003200961011     C     SUMSTS        ORNE      *BLANKS
003300961011     C                   Z-ADD     0             SUMNFE
003400961011     C                   MOVEL     ' '           SUMFLG
003500961011     C                   MOVEL     *BLANKS       SUMSTS
003600961011     C                   EXCEPT    AGGANN
003700961011     C                   END
003800961011     C     KCCM          READE     EDSUM01L                               30
003900961011     C                   END
004000961011     C*
004100961011     C*  Eseguo posizionamento iniziale in base al codice cliente
004200961011     C*  foglio - entrata
004300961011     C                   Z-ADD     D87CCM        KCCM
004400961011     C                   Z-ADD     D87NFE        KNFE
004500961011     C                   MOVEL     '*'           KFLG
004600961025     C     KBRV          CHAIN     EDBRV02L                           30
004700961011     C     *IN30         DOWEQ     '0'
004800961030     C                   MOVEL     ' '           BRVFLG
004900961030     C     BRVSTS        IFEQ      '002'
005000980326     C***                  Z-ADD0         BRVNSP
005100980326     C***                  Z-ADD0         BRVAAS
005200980326     C***                  Z-ADD0         BRVLNP
005300961030     C                   END
005400961011     C                   EXCEPT    ANNBRV
005500961025     C     KBRV          READE     EDBRV02L                               30
005600961011     C                   END
005700980320     C*
005800980320     C* Se è impostata la lnp lavoro x essa
005900980320     C                   ELSE
006000980320     C*  Eseguo posizionamento iniziale in base alla lnp
006100980320     C                   Z-ADD     D87LNP        KLNP
006200980320     C     KLNP          CHAIN     EDSUM04L                           30
006300980320     C     *IN30         DOWEQ     '0'
006400980320     C*  Sfleggo record annullati
006500980320     C*  Sfleggo ERRORI
006600980320     C     SUMFLG        IFNE      *BLANKS
006700980320     C     SUMSTS        ORNE      *BLANKS
006800980320     C                   Z-ADD     0             SUMNFE
006900980320     C                   MOVEL     ' '           SUMFLG
007000980320     C                   MOVEL     *BLANKS       SUMSTS
007100980320     C                   EXCEPT    ANNLNP
007200980320     C                   END
007300980324     C     KLNP          READE     EDSUM04L                               30
007400980320     C                   END
007500980320     C*
007600980320     C*  Eseguo posizionamento iniziale in base al codice cliente
007700980320     C*  foglio - entrata
007800980320     C                   Z-ADD     D87LNP        KLNP
007900980320     C                   Z-ADD     D87NFE        KNFE
008000980320     C                   MOVEL     '*'           KFLG
008100980320     C     KBRV2         CHAIN     EDBRV03L                           30
008200980320     C     *IN30         DOWEQ     '0'
008300980320     C                   MOVEL     ' '           BRVFLG
008400980320     C     BRVSTS        IFEQ      '002'
008500980326     C***                  Z-ADD0         BRVNSP
008600980326     C***                  Z-ADD0         BRVAAS
008700980326     C***                  Z-ADD0         BRVLNP
008800980320     C                   END
008900980320     C                   EXCEPT    BRVALP
009000980320     C     KBRV2         READE     EDBRV03L                               30
009100980320     C                   END
009200961007     C*
009300980324     C                   END
009400980324     C*
009500961007     C*  Fine Lavoro
009600961007     C                   SETON                                        LR
009700961008     C*-----------------------------------------------------------*
009800961008     C* *INZSR: Operazioni iniziali
009900961008     C*-----------------------------------------------------------*
010000961008     C     *INZSR        BEGSR
010100961008     C*
010200961008     C     *ENTRY        PLIST
010300961008     C                   PARM                    KPJBA
010400961008     C                   MOVEL     KPJBU         TRTC87
010500961008     C*
010600961008     C* Chiavi di accesso
010700961011     C     KBRV          KLIST
010800961009     C                   KFLD                    KCCM
010900961009     C                   KFLD                    KNFE
011000961011     C                   KFLD                    KFLG
011100980320     C     KBRV2         KLIST
011200980320     C                   KFLD                    KLNP
011300980320     C                   KFLD                    KNFE
011400980320     C                   KFLD                    KFLG
011500961008     C*
011600961008     C* Definizione chiave di accesso
011700961008     C     *LIKE         DEFINE    SUMCCM        KCCM
011800980320     C     *LIKE         DEFINE    SUMLNP        KLNP
011900961011     C     *LIKE         DEFINE    BRVNFE        KNFE
012000961011     C     *LIKE         DEFINE    BRVFLG        KFLG
012100961008     C*
012200961008     C                   ENDSR
012300961009     O*-------------------------------------------------------------*
012400961009     OEDSUM000  E            AGGANN
012500961009     O                       SUMFLG
012600961011     O                       SUMSTS
012700961011     O                       SUMNFE
012800961011     OEDBRV000  E            ANNBRV
012900961011     O                       BRVFLG
013000961030     O                       BRVAAS
013100961030     O                       BRVLNP
013200961030     O                       BRVNSP
013300980320     OEDSUMLNP  E            ANNLNP
013400980320     O                       SUMFLG
013500980320     O                       SUMSTS
013600980320     O                       SUMNFE
013700980320     OEDBRVLNP  E            BRVALP
013800980320     O                       BRVFLG
013900980320     O                       BRVAAS
014000980320     O                       BRVLNP
014100980320     O                       BRVNSP
