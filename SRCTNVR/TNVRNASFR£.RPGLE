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
003200130304     D FNLSW3DS        DS
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
004500130304     c                   exfmt     lsw3D01
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
005800130304     c                   clear                   fnlsw3ds
005900130304     c                   eval      ilsw3dsd=wdsd
006000130304     c                   eval      ilsw3dsa=wdsa
006100130304     c                   eval      ilsw3tfp=simfel
006200131001     c                   eval      ilsw3co =v1cconteg
006300131001     c                   eval      ilsw3sp =v1cspareg
006400130304     c                   movel     fnlsw3ds      kpjbu
006500040430     c****
006600130306     C*****              call      'FNLSW4R'
006700130306     c                   eval      kcoaz='LSW4'
006800130306     C                   call      'BCH10'
006900130211     c                   parm                    kpjba
007000150909     c
007100150909     c****
007200150910     C*                  call      'FNLSW6R'
007300150910     c*                  parm                    kpjba
007400150909     c                   eval      kcoaz='LSW6'
007500150909
007600150910     C                   call      'BCH10'
007700150910     c                   parm                    kpjba
007800001218     C*
007900040430     c     fine          tag
008000001218     C                   seton                                        LR
008100010509     C*---------------------------------------------------------------*
008200001218
008300040430     C*------------------------------------------------------------------------*
008400040430     C* CHKD01  - Controlli videata
008500040430     C*------------------------------------------------------------------------*
008600040430     C     CHKD01        BEGSR
008700040430     C*
008800040430     c                   setoff                                       284344
008900040430     c                   clear                   v1cmsg
009000040430     c* controllo data dal se immessa
009100040430     c                   clear                   wdsd
009200040430    1C     V1CDSD        IFGT      0
009300040430     C                   MOVE      V1CdSd        G08DAT
009400040430     C                   MOVEL     *ZERO         G08INV
009500040430     C                   MOVEL     *BLANKS       G08ERR
009600040430     C                   CALL      'XSRDA8'
009700040430     C                   PARM                    WLBDA8
009800040430     C     G08ERR        IFEQ      '1'
009900040430     C                   MOVEL     MSG(1)        V1CMSG
010000040430     C                   SETON                                        4328
010100040430     C                   GOTO      ENDCTR
010200040430     C                   ENDIF
010300040430     C                   Z-ADD     G08DAT        V1CDSD
010400040430     C                   Z-ADD     G08INV        WDSD
010500040430     c                   endif
010600040430     C*
010700040430     c* controllo data al se immessa
010800040430     c                   clear                   wdsa
010900040430    2C     V1CDSA        IFNE      0
011000040430     C                   MOVE      V1CDSA        G08DAT
011100040430     C                   MOVEL     *ZERO         G08INV
011200040430     C                   MOVEL     *BLANKS       G08ERR
011300040430     C                   CALL      'XSRDA8'
011400040430     C                   PARM                    WLBDA8
011500040430     C     G08ERR        IFEQ      '1'
011600040430     C                   MOVEL     MSG(1)        V1CMSG
011700040430     C                   SETON                                        4428
011800040430     C                   GOTO      ENDCTR
011900040430     C                   ENDIF
012000040430     C                   Z-ADD     G08DAT        V1CDSA
012100040430     C                   Z-ADD     G08INV        WDSA
012200040430     c                   endif
012300040430     C*
012400040430     c* almeno una delle due date ci deve essere
012500040430     c     v1cdsd        ifeq      *zeros
012600040430     c     v1cdsa        andeq     *zeros
012700040430     C                   MOVEL     MSG(3)        V1CMSG
012800040430     c                   seton                                        4328
012900040430     c                   goto      endctr
013000040430     c                   endif
013100040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
013200040430    3C     WDSD          IFGT      WDSA
013300040430     c     wdsa          andgt     *zeros
013400040430     C                   MOVEL     MSG(2)        V1CMSG
013500040430     C                   SETON                                        4328
013600040430     C                   GOTO      ENDCTR
013700040430    3C                   ENDIF
013800001218
013900040430     c     endctr        ENDSR
014000030414     C*------------------------------------------------------------------------*
014100030414     C* REPERISCE I DATI UTENTE
014200030414     C*------------------------------------------------------------------------*
014300030414     C     REPDATIUTE    BEGSR
014400030414     C*
014500030414     C* INIZIALIZZA VARIABILI DI WRK
014600030414     C                   CLEAR                   TIBS34DS
014700030414     C                   CLEAR                   AZUTEDS
014800030414     C                   CLEAR                   DDATIUTE
014900030414     C*
015000030414     C     *DTAARA       DEFINE    §azute        azuteds
015100030414     C     *DTAARA       DEFINE    §datiute      ddatiute
015200030414     C                   IN(E)     *DTAARA
015300030422     C                   IF        %Error
015400030422     C                   EVAL      I34Tla = 'L'
015500030414     C                   CALL      'TIBS34R'
015600030414     C                   PARM                    Tibs34Ds
015700030414     C                   IN        *DTAARA
015800030422     C                   ENDIF
015900030414     C*
016000030414     C                   ENDSR
016100030414     C*------------------------------------------------------------------------*
016200010509
016300010509
016400001218
016500001218     C*------------------------------------------------------------------------*
016600001218     C* *INZSR - ROUTINE INIZIALE
016700001218     C*------------------------------------------------------------------------*
016800001218     C     *INZSR        BEGSR
016900001218     C*
017000030422     C* Ricevo in la DS di architettura
017100010726     C     *ENTRY        PLIST
017200020613     C                   PARM                    KPJBA
017300030521     C*
017400030521     C* Reperisco l'UDATE del job
017500030521     C                   TIME                    WN14             14 0
017600030521     C                   MOVE      WN14          WN8               8 0
017700030521     C                   Z-ADD     WN8           G08DAT
017800030521     C                   Z-ADD     *zeros        G08INV
017900030521     C                   MOVEL     '0'           G08ERR
018000030521     C                   CALL      'XSRDA8'
018100030521     C                   PARM                    WLBDA8
018200030521     C                   Z-ADD     G08INV        DATCOR            8 0
018300030414     C****
018400030414     C* REPERIMEMTO DATI UTENTE
018500030414     C                   EXSR      REPDATIUTE
018600030422     C*
018700001218     C*
018800001218     C                   ENDSR
018900040430** MSG
019000040430Data errata                                                                   1
019100040430La data spedizione DAL non puo' essere maggiore della data AL                 2
019200040430Immettere almeno una delle due date                                           3
