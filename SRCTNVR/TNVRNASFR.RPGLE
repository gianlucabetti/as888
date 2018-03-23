000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300030422     FAZORG01L  IF   E           K DISK
000400160413     Ftnvrnasfd CF   E             WORKSTN
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
003200160413     D param           DS
003300130304     D  ilsw3dsd               1      8  0
003400130304     D  ilsw3dsa               9     16  0
003500130211
003600920812     C*---------------------------------------------------------------*
003700001218     C* MAIN
003800001218     C*---------------------------------------------------------------*
003900040430     c* Gestione videata richiesta data dal/al
004000130305     c                   clear                   v1cdsa
004100130305     c                   z-add     wn8           v1cdsd
004200040430     c                   clear                   v1cmsg
004300130211     c
004400040430     c                   do        *hival
004500160413     c                   exfmt     vrnasd1
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
005800160413     c                   clear                   param
005900130304     c                   eval      ilsw3dsd=wdsd
006000130304     c                   eval      ilsw3dsa=wdsa
006100160413     c                   movel     param         kpjbu
006200040430     c****
006300160413     C*****              call      'TNVRNASR'
006400160414     c                   eval      kcoaz='VNAS'
006500130306     C                   call      'BCH10'
006600130211     c                   parm                    kpjba
006700150909     c
006800001218     C*
006900040430     c     fine          tag
007000001218     C                   seton                                        LR
007100010509     C*---------------------------------------------------------------*
007200001218
007300040430     C*------------------------------------------------------------------------*
007400040430     C* CHKD01  - Controlli videata
007500040430     C*------------------------------------------------------------------------*
007600040430     C     CHKD01        BEGSR
007700040430     C*
007800040430     c                   setoff                                       284344
007900040430     c                   clear                   v1cmsg
008000040430     c* controllo data dal se immessa
008100040430     c                   clear                   wdsd
008200040430    1C     V1CDSD        IFGT      0
008300040430     C                   MOVE      V1CdSd        G08DAT
008400040430     C                   MOVEL     *ZERO         G08INV
008500040430     C                   MOVEL     *BLANKS       G08ERR
008600040430     C                   CALL      'XSRDA8'
008700040430     C                   PARM                    WLBDA8
008800040430     C     G08ERR        IFEQ      '1'
008900040430     C                   MOVEL     MSG(1)        V1CMSG
009000040430     C                   SETON                                        4328
009100040430     C                   GOTO      ENDCTR
009200040430     C                   ENDIF
009300040430     C                   Z-ADD     G08DAT        V1CDSD
009400040430     C                   Z-ADD     G08INV        WDSD
009500040430     c                   endif
009600040430     C*
009700040430     c* controllo data al se immessa
009800040430     c                   clear                   wdsa
009900040430    2C     V1CDSA        IFNE      0
010000040430     C                   MOVE      V1CDSA        G08DAT
010100040430     C                   MOVEL     *ZERO         G08INV
010200040430     C                   MOVEL     *BLANKS       G08ERR
010300040430     C                   CALL      'XSRDA8'
010400040430     C                   PARM                    WLBDA8
010500040430     C     G08ERR        IFEQ      '1'
010600040430     C                   MOVEL     MSG(1)        V1CMSG
010700040430     C                   SETON                                        4428
010800040430     C                   GOTO      ENDCTR
010900040430     C                   ENDIF
011000040430     C                   Z-ADD     G08DAT        V1CDSA
011100040430     C                   Z-ADD     G08INV        WDSA
011200040430     c                   endif
011300040430     C*
011400040430     c* almeno una delle due date ci deve essere
011500040430     c     v1cdsd        ifeq      *zeros
011600040430     c     v1cdsa        andeq     *zeros
011700040430     C                   MOVEL     MSG(3)        V1CMSG
011800040430     c                   seton                                        4328
011900040430     c                   goto      endctr
012000040430     c                   endif
012100040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
012200040430    3C     WDSD          IFGT      WDSA
012300040430     c     wdsa          andgt     *zeros
012400040430     C                   MOVEL     MSG(2)        V1CMSG
012500040430     C                   SETON                                        4328
012600040430     C                   GOTO      ENDCTR
012700040430    3C                   ENDIF
012800001218
012900040430     c     endctr        ENDSR
013000030414     C*------------------------------------------------------------------------*
013100030414     C* REPERISCE I DATI UTENTE
013200030414     C*------------------------------------------------------------------------*
013300030414     C     REPDATIUTE    BEGSR
013400030414     C*
013500030414     C* INIZIALIZZA VARIABILI DI WRK
013600030414     C                   CLEAR                   TIBS34DS
013700030414     C                   CLEAR                   AZUTEDS
013800030414     C                   CLEAR                   DDATIUTE
013900030414     C*
014000030414     C     *DTAARA       DEFINE    §azute        azuteds
014100030414     C     *DTAARA       DEFINE    §datiute      ddatiute
014200030414     C                   IN(E)     *DTAARA
014300030422     C                   IF        %Error
014400030422     C                   EVAL      I34Tla = 'L'
014500030414     C                   CALL      'TIBS34R'
014600030414     C                   PARM                    Tibs34Ds
014700030414     C                   IN        *DTAARA
014800030422     C                   ENDIF
014900030414     C*
015000030414     C                   ENDSR
015100030414     C*------------------------------------------------------------------------*
015200010509
015300010509
015400001218
015500001218     C*------------------------------------------------------------------------*
015600001218     C* *INZSR - ROUTINE INIZIALE
015700001218     C*------------------------------------------------------------------------*
015800001218     C     *INZSR        BEGSR
015900001218     C*
016000030422     C* Ricevo in la DS di architettura
016100010726     C     *ENTRY        PLIST
016200020613     C                   PARM                    KPJBA
016300030521     C*
016400030521     C* Reperisco l'UDATE del job
016500030521     C                   TIME                    WN14             14 0
016600030521     C                   MOVE      WN14          WN8               8 0
016700030521     C                   Z-ADD     WN8           G08DAT
016800030521     C                   Z-ADD     *zeros        G08INV
016900030521     C                   MOVEL     '0'           G08ERR
017000030521     C                   CALL      'XSRDA8'
017100030521     C                   PARM                    WLBDA8
017200030521     C                   Z-ADD     G08INV        DATCOR            8 0
017300030414     C****
017400030414     C* REPERIMEMTO DATI UTENTE
017500030414     C                   EXSR      REPDATIUTE
017600030422     C*
017700001218     C*
017800001218     C                   ENDSR
017900040430** MSG
018000040430Data errata                                                                   1
018100040430La data spedizione DAL non puo' essere maggiore della data AL                 2
018200040430Immettere almeno una delle due date                                           3
