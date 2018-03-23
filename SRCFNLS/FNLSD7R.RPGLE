000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300030422     FAZORG01L  IF   E           K DISK
000400130211     FFNLSD7D   CF   E             WORKSTN
000500001218     D*--------------------
000600001218     D* DS ESTERNE
000700001218     D*--------------------
000800900517     D KPJBA         E DS
000900030422     D*--------------------
001000030422     D* SCHIERE DI MEMORIZZAZIONE
001100030422     D*--------------------
001200040430     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001300040430     D wdsd            s                   like(v1cdsd)
001400040430     D wdsa            s                   like(v1cdsa)
001500030414     D*---
001600030414     D* DS REPERIMENTO DATI UTENTE
001700030414     D*---
001800030414     D TIBS34DS      E DS
001900030414     D DDATIUTE      E DS
002000030414     D AZUTEDS       E DS                  extname(AZUTE00F)
002100030423     D*---
002200030521     D*------------------
002300030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
002400030521     D*------------------
002500030521     D WLBDA8          DS                  INZ
002600030521     D  G08DAT                 1      8  0
002700030521     D  G08INV                 9     16  0
002800030521     D  G08ERR                17     17
002900030521     D  G08TGI                18     22  0
003000001218
003100130211     D*------------------
003200130211     D FNLSD9DS        DS
003300130211     D  ilsd9dsd               1      8  0
003400130211     D  ilsd9dsa               9     16  0
003500130211     d* S=stampa F=file E=entrambi
003600130211     D  ilsd9RIC              17     17
003700130211     D  ilsd9TFP              18     20  0
003800130211
003900920812     C*---------------------------------------------------------------*
004000001218     C* MAIN
004100001218     C*---------------------------------------------------------------*
004200040430     c* Gestione videata richiesta data dal/al
004300040430     c                   clear                   v1cdsd
004400040430     c                   z-add     wn8           v1cdsa
004500040430     c                   clear                   v1cmsg
004600130211     c
004700040430     c                   do        *hival
004800130211     c                   exfmt     lsd7D01
004900040430     c     *inkc         cabeq     *on           fine
005000040430     c                   exsr      chkd01
005100040430     c     *in28         ifeq      *off
005200040430     c     *inkf         andeq     *on
005300040430     c                   leave
005400040430     c                   endif
005500040430     c                   enddo
005600040430     c*
005700040430     c     wdsa          ifeq      *zeros
005800040430     c                   z-add     wdsd          wdsa
005900040430     c                   z-add     v1cdsd        v1cdsa
006000040430     c                   endif
006100130211     c                   clear                   fnlsd9ds
006200130211     c                   eval      ilsd9dsd=wdsd
006300130211     c                   eval      ilsd9dsa=wdsa
006400130211     c* Solo stampa
006500130211     c                   eval      ilsd9ric='S'
006600130211     c                   eval      ilsd9tfp=simfel
006700130211     c                   movel     fnlsd9ds      kpjbu
006800040430     c****
006900130211     C                   call      'FNLSD9R'
007000130211     c                   parm                    kpjba
007100001218     C*
007200040430     c     fine          tag
007300001218     C                   seton                                        LR
007400010509     C*---------------------------------------------------------------*
007500001218
007600040430     C*------------------------------------------------------------------------*
007700040430     C* CHKD01  - Controlli videata
007800040430     C*------------------------------------------------------------------------*
007900040430     C     CHKD01        BEGSR
008000040430     C*
008100040430     c                   setoff                                       284344
008200040430     c                   clear                   v1cmsg
008300040430     c* controllo data dal se immessa
008400040430     c                   clear                   wdsd
008500040430    1C     V1CDSD        IFGT      0
008600040430     C                   MOVE      V1CdSd        G08DAT
008700040430     C                   MOVEL     *ZERO         G08INV
008800040430     C                   MOVEL     *BLANKS       G08ERR
008900040430     C                   CALL      'XSRDA8'
009000040430     C                   PARM                    WLBDA8
009100040430     C     G08ERR        IFEQ      '1'
009200040430     C                   MOVEL     MSG(1)        V1CMSG
009300040430     C                   SETON                                        4328
009400040430     C                   GOTO      ENDCTR
009500040430     C                   ENDIF
009600040430     C                   Z-ADD     G08DAT        V1CDSD
009700040430     C                   Z-ADD     G08INV        WDSD
009800040430     c                   endif
009900040430     C*
010000040430     c* controllo data al se immessa
010100040430     c                   clear                   wdsa
010200040430    2C     V1CDSA        IFNE      0
010300040430     C                   MOVE      V1CDSA        G08DAT
010400040430     C                   MOVEL     *ZERO         G08INV
010500040430     C                   MOVEL     *BLANKS       G08ERR
010600040430     C                   CALL      'XSRDA8'
010700040430     C                   PARM                    WLBDA8
010800040430     C     G08ERR        IFEQ      '1'
010900040430     C                   MOVEL     MSG(1)        V1CMSG
011000040430     C                   SETON                                        4428
011100040430     C                   GOTO      ENDCTR
011200040430     C                   ENDIF
011300040430     C                   Z-ADD     G08DAT        V1CDSA
011400040430     C                   Z-ADD     G08INV        WDSA
011500040430     c                   endif
011600040430     C*
011700040430     c* almeno una delle due date ci deve essere
011800040430     c     v1cdsd        ifeq      *zeros
011900040430     c     v1cdsa        andeq     *zeros
012000040430     C                   MOVEL     MSG(3)        V1CMSG
012100040430     c                   seton                                        4328
012200040430     c                   goto      endctr
012300040430     c                   endif
012400040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
012500040430    3C     WDSD          IFGT      WDSA
012600040430     c     wdsa          andgt     *zeros
012700040430     C                   MOVEL     MSG(2)        V1CMSG
012800040430     C                   SETON                                        4328
012900040430     C                   GOTO      ENDCTR
013000040430    3C                   ENDIF
013100001218
013200040430     c     endctr        ENDSR
013300030414     C*------------------------------------------------------------------------*
013400030414     C* REPERISCE I DATI UTENTE
013500030414     C*------------------------------------------------------------------------*
013600030414     C     REPDATIUTE    BEGSR
013700030414     C*
013800030414     C* INIZIALIZZA VARIABILI DI WRK
013900030414     C                   CLEAR                   TIBS34DS
014000030414     C                   CLEAR                   AZUTEDS
014100030414     C                   CLEAR                   DDATIUTE
014200030414     C*
014300030414     C     *DTAARA       DEFINE    §azute        azuteds
014400030414     C     *DTAARA       DEFINE    §datiute      ddatiute
014500030414     C                   IN(E)     *DTAARA
014600030422     C                   IF        %Error
014700030422     C                   EVAL      I34Tla = 'L'
014800030414     C                   CALL      'TIBS34R'
014900030414     C                   PARM                    Tibs34Ds
015000030414     C                   IN        *DTAARA
015100030422     C                   ENDIF
015200030414     C*
015300030414     C                   ENDSR
015400030414     C*------------------------------------------------------------------------*
015500010509
015600010509
015700001218
015800001218     C*------------------------------------------------------------------------*
015900001218     C* *INZSR - ROUTINE INIZIALE
016000001218     C*------------------------------------------------------------------------*
016100001218     C     *INZSR        BEGSR
016200001218     C*
016300030422     C* Ricevo in la DS di architettura
016400010726     C     *ENTRY        PLIST
016500020613     C                   PARM                    KPJBA
016600030521     C*
016700030521     C* Reperisco l'UDATE del job
016800030521     C                   TIME                    WN14             14 0
016900030521     C                   MOVE      WN14          WN8               8 0
017000030521     C                   Z-ADD     WN8           G08DAT
017100030521     C                   Z-ADD     *zeros        G08INV
017200030521     C                   MOVEL     '0'           G08ERR
017300030521     C                   CALL      'XSRDA8'
017400030521     C                   PARM                    WLBDA8
017500030521     C                   Z-ADD     G08INV        DATCOR            8 0
017600030414     C****
017700030414     C* REPERIMEMTO DATI UTENTE
017800030414     C                   EXSR      REPDATIUTE
017900030422     C*
018000001218     C*
018100001218     C                   ENDSR
018200040430** MSG
018300040430Data errata                                                                   1
018400040430La data spedizione DAL non puo' essere maggiore della data AL                 2
018500040430Immettere almeno una delle due date                                           3
