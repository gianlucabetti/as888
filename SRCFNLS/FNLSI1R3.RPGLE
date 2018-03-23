000100970612     H DECEDIT('0,') DATEDIT(*DMY.)
000200970616     H* FNLSI1R3 *---------------------------------------------------*
000300970612     H*                                                              *
000400970612     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500970616     H*       - - - - - -   AGGIORNO FNBLP/FNBLT    - - - - - - -    *
000600970612     H*--------------------------------------------------------------*
000700970616     FFNBLP01L  UF   E           K DISK
000800970612     D*--------------------------------------------------------------*
000900970612     D* DS
001000970612     D*--------------------------------------------------------------*
001100970616     D WLBDA8          DS
001200970616     D  G02DAT                 1      8  0
001300970616     D  G02INV                 9     16  0
001400970616     D  G02ERR                17     17
001500970616     D  G02TGI                18     22  0
001600970612     D KPJBA         E DS
001700970616     D DS7O          E DS
001800970612     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
001900970612     C*--------------------------------------------------------------*
002000970612     C*  C I C L O     P R I N C I P A L E
002100970612     C*--------------------------------------------------------------*
002200970613     C     *ENTRY        PLIST
002300970613     C                   PARM                    KPJBA
002400970613     C                   MOVEL     KPJBU         FNLSI1
002500970616     C* Eseguo chiusura bolla
002600970616     C                   EXSR      CHIUDI
002700970616     C*
002800970616     C                   SETON                                        RT
002900970616     C*--------------------------------------------------------------*
003000970616     C* CHIUDI: Chiusura bolla e dettaglio segnacolli con IDD partenza
003100970616     C*--------------------------------------------------------------*
003200970616     C     CHIUDI        BEGSR
003300970616     C*
003400970616     C* Aggiorno campi bolla partenze
003500970616     C                   Z-ADD     DI1AAS        KAAS
003600970616     C                   Z-ADD     DI1LNP        KLNP
003700970616     C                   Z-ADD     DI1NRS        KNRS
003800970616     C                   Z-ADD     DI1NSP        KNSP
003900970616     C     KBLP          CHAIN     FNBLP01L                           31
004000970616     C                   Z-ADD     0             BLPDCE
004100970616     C                   Z-ADD     0             BLPNPI
004200970616     C                   Z-ADD     0             BLPNCI
004300970616     C                   Z-ADD     0             BLPNRC
004400970616     C                   Z-ADD     OGGI          BLPDBR
004500970616     C                   Z-ADD     OGGI          BLPDPC
004600970616     C                   Z-ADD     OGGI          BLPDUC
004700970616     C                   Z-ADD     OGGI          BLPDDC
004800970616     C                   Z-ADD     OGGI          BLPDCM
004900970616     C                   TIME                    WTIME             6 0
005000970616     C                   MOVEL     WTIME         BLPHMC
005100970616     C                   Z-ADD     99999         BLPNFV
005200970616     C                   Z-ADD     9999999       BLPNDC
005300970616     C                   Z-ADD     OGGI          BLPDT1
005400970616     C                   Z-ADD     OGGI          BLPDT2
005500970616     C                   MOVEL     'N'           BLPFT1
005600970616     C                   MOVEL     'N'           BLPFT2
005700970616     C* Per idd metto codice consegna anomala sulla bolla
005800980203     C     DI1CCA        IFEQ      'S'
005900970616     C                   MOVEL     '7'           BLPCCA
006000970616     C                   ELSE
006100970616     C                   MOVEL     *BLANKS       BLPCCA
006200970616     C                   END
006300051011      ** per reso consegnato
006400051011     C     DI1STS        ifeq      '21 '
006500051011     C                   move      'S'           blpfbr
006600051011     C                   end
006700970616     C* devo inviare date consegna cliente
006800970616     C                   Z-ADD     OGGI          BLPDT3
006900970616     C                   MOVEL     'T'           BLPFT3
007000970616     C* Update bolla
007100970616     C                   UPDATE    FNBLP000
007200970616     C*
007300970616     C                   ENDSR
007400970613     C*--------------------------------------------------------------*
007500970613     C* *INZSR: Operazioni iniziali
007600970613     C*--------------------------------------------------------------*
007700970613     C     *INZSR        BEGSR
007800970613     C*
007900970613      *  Definizione chiavi
008000970616     C     KBLT          KLIST
008100970613     C                   KFLD                    KAAS
008200970613     C                   KFLD                    KLNP
008300970613     C                   KFLD                    KNRS
008400970613     C                   KFLD                    KNSP
008500970616     C     KBLP          KLIST
008600970616     C                   KFLD                    KAAS
008700970616     C                   KFLD                    KLNP
008800970616     C                   KFLD                    KNRS
008900970616     C                   KFLD                    KNSP
009000970613      *
009100970613      *  Definizione variabili
009200970616     C     *LIKE         DEFINE    BLPAAS        KAAS
009300970616     C     *LIKE         DEFINE    BLPLNP        KLNP
009400970616     C     *LIKE         DEFINE    BLPNRS        KNRS
009500970616     C     *LIKE         DEFINE    BLPNSP        KNSP
009600970616      *
009700970616      *  Reperisco la data del giorno
009800970616     C                   TIME                    WHHDAT           14 0
009900970616     C                   MOVE      WHHDAT        G02DAT
010000970616     C                   MOVEL     *BLANK        G02ERR
010100970616     C                   CALL      'XSRDA8'
010200970616     C                   PARM                    WLBDA8
010300970616     C                   Z-ADD     G02INV        OGGI              8 0
010400970616     C*
010500970613     C                   ENDSR
