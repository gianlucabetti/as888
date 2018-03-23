000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300030422     FAZORG01L  IF   E           K DISK
000400130215     Ffiors1D   CF   E             WORKSTN
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
003200130215     D FIORS1DS        DS
003300130215     D  iors1dsd               1      8  0
003400130215     D  iors1dsa               9     16  0
003500130211
003600920812     C*---------------------------------------------------------------*
003700001218     C* MAIN
003800001218     C*---------------------------------------------------------------*
003900040430     c* Gestione videata richiesta data dal/al
004000040430     c                   clear                   v1cdsd
004100040430     c                   z-add     wn8           v1cdsa
004200040430     c                   clear                   v1cmsg
004300130211     c
004400040430     c                   do        *hival
004500130215     c                   exfmt     ors1D01
004600040430     c     *inkc         cabeq     *on           fine
004700040430     c                   exsr      chkd01
004800040430     c     *in28         ifeq      *off
004900040430     c     *inkf         andeq     *on
005000040430     c                   leave
005100040430     c                   endif
005200040430     c                   enddo
005300040430     c*
005400040430     c     wdsa          ifeq      *zeros
005500040430     c                   z-add     wdsd          wdsa
005600040430     c                   z-add     v1cdsd        v1cdsa
005700040430     c                   endif
005800130215     c                   clear                   fiors1ds
005900130215     c                   eval      iors1dsd=wdsd
006000130215     c                   eval      iors1dsa=wdsa
006100130215     c                   movel     fiors1ds      kpjbu
006200040430     c****
006300130215     C                   call      'FIORS2R'
006400130211     c                   parm                    kpjba
006500001218     C*
006600040430     c     fine          tag
006700001218     C                   seton                                        LR
006800010509     C*---------------------------------------------------------------*
006900001218
007000040430     C*------------------------------------------------------------------------*
007100040430     C* CHKD01  - Controlli videata
007200040430     C*------------------------------------------------------------------------*
007300040430     C     CHKD01        BEGSR
007400040430     C*
007500040430     c                   setoff                                       284344
007600040430     c                   clear                   v1cmsg
007700040430     c* controllo data dal se immessa
007800040430     c                   clear                   wdsd
007900040430    1C     V1CDSD        IFGT      0
008000040430     C                   MOVE      V1CdSd        G08DAT
008100040430     C                   MOVEL     *ZERO         G08INV
008200040430     C                   MOVEL     *BLANKS       G08ERR
008300040430     C                   CALL      'XSRDA8'
008400040430     C                   PARM                    WLBDA8
008500040430     C     G08ERR        IFEQ      '1'
008600040430     C                   MOVEL     MSG(1)        V1CMSG
008700040430     C                   SETON                                        4328
008800040430     C                   GOTO      ENDCTR
008900040430     C                   ENDIF
009000040430     C                   Z-ADD     G08DAT        V1CDSD
009100040430     C                   Z-ADD     G08INV        WDSD
009200040430     c                   endif
009300040430     C*
009400040430     c* controllo data al se immessa
009500040430     c                   clear                   wdsa
009600040430    2C     V1CDSA        IFNE      0
009700040430     C                   MOVE      V1CDSA        G08DAT
009800040430     C                   MOVEL     *ZERO         G08INV
009900040430     C                   MOVEL     *BLANKS       G08ERR
010000040430     C                   CALL      'XSRDA8'
010100040430     C                   PARM                    WLBDA8
010200040430     C     G08ERR        IFEQ      '1'
010300040430     C                   MOVEL     MSG(1)        V1CMSG
010400040430     C                   SETON                                        4428
010500040430     C                   GOTO      ENDCTR
010600040430     C                   ENDIF
010700040430     C                   Z-ADD     G08DAT        V1CDSA
010800040430     C                   Z-ADD     G08INV        WDSA
010900040430     c                   endif
011000040430     C*
011100040430     c* almeno una delle due date ci deve essere
011200040430     c     v1cdsd        ifeq      *zeros
011300040430     c     v1cdsa        andeq     *zeros
011400040430     C                   MOVEL     MSG(3)        V1CMSG
011500040430     c                   seton                                        4328
011600040430     c                   goto      endctr
011700040430     c                   endif
011800040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
011900040430    3C     WDSD          IFGT      WDSA
012000040430     c     wdsa          andgt     *zeros
012100040430     C                   MOVEL     MSG(2)        V1CMSG
012200040430     C                   SETON                                        4328
012300040430     C                   GOTO      ENDCTR
012400040430    3C                   ENDIF
012500001218
012600040430     c     endctr        ENDSR
012700030414     C*------------------------------------------------------------------------*
012800030414     C* REPERISCE I DATI UTENTE
012900030414     C*------------------------------------------------------------------------*
013000030414     C     REPDATIUTE    BEGSR
013100030414     C*
013200030414     C* INIZIALIZZA VARIABILI DI WRK
013300030414     C                   CLEAR                   TIBS34DS
013400030414     C                   CLEAR                   AZUTEDS
013500030414     C                   CLEAR                   DDATIUTE
013600030414     C*
013700030414     C     *DTAARA       DEFINE    §azute        azuteds
013800030414     C     *DTAARA       DEFINE    §datiute      ddatiute
013900030414     C                   IN(E)     *DTAARA
014000030422     C                   IF        %Error
014100030422     C                   EVAL      I34Tla = 'L'
014200030414     C                   CALL      'TIBS34R'
014300030414     C                   PARM                    Tibs34Ds
014400030414     C                   IN        *DTAARA
014500030422     C                   ENDIF
014600030414     C*
014700030414     C                   ENDSR
014800030414     C*------------------------------------------------------------------------*
014900010509
015000010509
015100001218
015200001218     C*------------------------------------------------------------------------*
015300001218     C* *INZSR - ROUTINE INIZIALE
015400001218     C*------------------------------------------------------------------------*
015500001218     C     *INZSR        BEGSR
015600001218     C*
015700030422     C* Ricevo in la DS di architettura
015800010726     C     *ENTRY        PLIST
015900020613     C                   PARM                    KPJBA
016000030521     C*
016100030521     C* Reperisco l'UDATE del job
016200030521     C                   TIME                    WN14             14 0
016300030521     C                   MOVE      WN14          WN8               8 0
016400030521     C                   Z-ADD     WN8           G08DAT
016500030521     C                   Z-ADD     *zeros        G08INV
016600030521     C                   MOVEL     '0'           G08ERR
016700030521     C                   CALL      'XSRDA8'
016800030521     C                   PARM                    WLBDA8
016900030521     C                   Z-ADD     G08INV        DATCOR            8 0
017000030414     C****
017100030414     C* REPERIMEMTO DATI UTENTE
017200030414     C                   EXSR      REPDATIUTE
017300030422     C*
017400001218     C*
017500001218     C                   ENDSR
017600040430** MSG
017700040430Data errata                                                                   1
017800130215La data  DAL non puo' essere maggiore della data AL
017900040430Immettere almeno una delle due date                                           3
