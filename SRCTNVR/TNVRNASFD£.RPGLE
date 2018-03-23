000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130305     H*  CONTEGGIO colli di bolle abbinate    filtro
000400130305     H*-------------------------------------------------------------------------
000500030422     FAZORG01L  IF   E           K DISK
000600130304     FFNLSw3D   CF   E             WORKSTN
000700001218     D*--------------------
000800001218     D* DS ESTERNE
000900001218     D*--------------------
001000900517     D KPJBA         E DS
001100030422     D*--------------------
001200030422     D* SCHIERE DI MEMORIZZAZIONE
001300030422     D*--------------------
001400040430     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001500040430     D wdsd            s                   like(v1cdsd)
001600040430     D wdsa            s                   like(v1cdsa)
001700030414     D*---
001800030414     D* DS REPERIMENTO DATI UTENTE
001900030414     D*---
002000030414     D TIBS34DS      E DS
002100030414     D DDATIUTE      E DS
002200030414     D AZUTEDS       E DS                  extname(AZUTE00F)
002300030423     D*---
002400030521     D*------------------
002500030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
002600030521     D*------------------
002700030521     D WLBDA8          DS                  INZ
002800030521     D  G08DAT                 1      8  0
002900030521     D  G08INV                 9     16  0
003000030521     D  G08ERR                17     17
003100030521     D  G08TGI                18     22  0
003200001218
003300130211     D*------------------
003400130304     D FNLSW3DS        DS
003500130304     D  ilsw3dsd               1      8  0
003600130304     D  ilsw3dsa               9     16  0
003700130304     D  ilsW3TFP              18     20  0
003800131001     D  ilsw3co               21     21
003900131001     D  ilsw3sp               22     22
004000130211
004100920812     C*---------------------------------------------------------------*
004200001218     C* MAIN
004300001218     C*---------------------------------------------------------------*
004400040430     c* Gestione videata richiesta data dal/al
004500130305     c                   clear                   v1cdsa
004600130305     c                   z-add     wn8           v1cdsd
004700040430     c                   clear                   v1cmsg
004800130926     c                   eval      v1cconteg='S'
004900130211     c
005000040430     c                   do        *hival
005100130304     c                   exfmt     lsw3D01
005200040430     c     *inkc         cabeq     *on           fine
005300040430     c                   exsr      chkd01
005400040430     c     *in28         ifeq      *off
005500040430     c     *inkf         andeq     *on
005600040430     c                   leave
005700040430     c                   endif
005800040430     c                   enddo
005900040430     c*
006000040430     c     wdsa          ifeq      *zeros
006100040430     c                   z-add     wdsd          wdsa
006200040430     c                   z-add     v1cdsd        v1cdsa
006300040430     c                   endif
006400130304     c                   clear                   fnlsw3ds
006500130304     c                   eval      ilsw3dsd=wdsd
006600130304     c                   eval      ilsw3dsa=wdsa
006700130304     c                   eval      ilsw3tfp=simfel
006800131001     c                   eval      ilsw3co =v1cconteg
006900131001     c                   eval      ilsw3sp =v1cspareg
007000130304     c                   movel     fnlsw3ds      kpjbu
007100040430     c****
007200130306     C*****              call      'FNLSW4R'
007300130306     c                   eval      kcoaz='LSW4'
007400130306     C                   call      'BCH10'
007500130211     c                   parm                    kpjba
007600150909     c
007700150909     c****
007800150910     C*                  call      'FNLSW6R'
007900150910     c*                  parm                    kpjba
008000150909     c                   eval      kcoaz='LSW6'
008100150909
008200150910     C                   call      'BCH10'
008300150910     c                   parm                    kpjba
008400001218     C*
008500040430     c     fine          tag
008600001218     C                   seton                                        LR
008700010509     C*---------------------------------------------------------------*
008800001218
008900040430     C*------------------------------------------------------------------------*
009000040430     C* CHKD01  - Controlli videata
009100040430     C*------------------------------------------------------------------------*
009200040430     C     CHKD01        BEGSR
009300040430     C*
009400040430     c                   setoff                                       284344
009500040430     c                   clear                   v1cmsg
009600040430     c* controllo data dal se immessa
009700040430     c                   clear                   wdsd
009800040430    1C     V1CDSD        IFGT      0
009900040430     C                   MOVE      V1CdSd        G08DAT
010000040430     C                   MOVEL     *ZERO         G08INV
010100040430     C                   MOVEL     *BLANKS       G08ERR
010200040430     C                   CALL      'XSRDA8'
010300040430     C                   PARM                    WLBDA8
010400040430     C     G08ERR        IFEQ      '1'
010500040430     C                   MOVEL     MSG(1)        V1CMSG
010600040430     C                   SETON                                        4328
010700040430     C                   GOTO      ENDCTR
010800040430     C                   ENDIF
010900040430     C                   Z-ADD     G08DAT        V1CDSD
011000040430     C                   Z-ADD     G08INV        WDSD
011100040430     c                   endif
011200040430     C*
011300040430     c* controllo data al se immessa
011400040430     c                   clear                   wdsa
011500040430    2C     V1CDSA        IFNE      0
011600040430     C                   MOVE      V1CDSA        G08DAT
011700040430     C                   MOVEL     *ZERO         G08INV
011800040430     C                   MOVEL     *BLANKS       G08ERR
011900040430     C                   CALL      'XSRDA8'
012000040430     C                   PARM                    WLBDA8
012100040430     C     G08ERR        IFEQ      '1'
012200040430     C                   MOVEL     MSG(1)        V1CMSG
012300040430     C                   SETON                                        4428
012400040430     C                   GOTO      ENDCTR
012500040430     C                   ENDIF
012600040430     C                   Z-ADD     G08DAT        V1CDSA
012700040430     C                   Z-ADD     G08INV        WDSA
012800040430     c                   endif
012900040430     C*
013000040430     c* almeno una delle due date ci deve essere
013100040430     c     v1cdsd        ifeq      *zeros
013200040430     c     v1cdsa        andeq     *zeros
013300040430     C                   MOVEL     MSG(3)        V1CMSG
013400040430     c                   seton                                        4328
013500040430     c                   goto      endctr
013600040430     c                   endif
013700040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
013800040430    3C     WDSD          IFGT      WDSA
013900040430     c     wdsa          andgt     *zeros
014000040430     C                   MOVEL     MSG(2)        V1CMSG
014100040430     C                   SETON                                        4328
014200040430     C                   GOTO      ENDCTR
014300040430    3C                   ENDIF
014400001218
014500040430     c     endctr        ENDSR
014600030414     C*------------------------------------------------------------------------*
014700030414     C* REPERISCE I DATI UTENTE
014800030414     C*------------------------------------------------------------------------*
014900030414     C     REPDATIUTE    BEGSR
015000030414     C*
015100030414     C* INIZIALIZZA VARIABILI DI WRK
015200030414     C                   CLEAR                   TIBS34DS
015300030414     C                   CLEAR                   AZUTEDS
015400030414     C                   CLEAR                   DDATIUTE
015500030414     C*
015600030414     C     *DTAARA       DEFINE    §azute        azuteds
015700030414     C     *DTAARA       DEFINE    §datiute      ddatiute
015800030414     C                   IN(E)     *DTAARA
015900030422     C                   IF        %Error
016000030422     C                   EVAL      I34Tla = 'L'
016100030414     C                   CALL      'TIBS34R'
016200030414     C                   PARM                    Tibs34Ds
016300030414     C                   IN        *DTAARA
016400030422     C                   ENDIF
016500030414     C*
016600030414     C                   ENDSR
016700030414     C*------------------------------------------------------------------------*
016800010509
016900010509
017000001218
017100001218     C*------------------------------------------------------------------------*
017200001218     C* *INZSR - ROUTINE INIZIALE
017300001218     C*------------------------------------------------------------------------*
017400001218     C     *INZSR        BEGSR
017500001218     C*
017600030422     C* Ricevo in la DS di architettura
017700010726     C     *ENTRY        PLIST
017800020613     C                   PARM                    KPJBA
017900030521     C*
018000030521     C* Reperisco l'UDATE del job
018100030521     C                   TIME                    WN14             14 0
018200030521     C                   MOVE      WN14          WN8               8 0
018300030521     C                   Z-ADD     WN8           G08DAT
018400030521     C                   Z-ADD     *zeros        G08INV
018500030521     C                   MOVEL     '0'           G08ERR
018600030521     C                   CALL      'XSRDA8'
018700030521     C                   PARM                    WLBDA8
018800030521     C                   Z-ADD     G08INV        DATCOR            8 0
018900030414     C****
019000030414     C* REPERIMEMTO DATI UTENTE
019100030414     C                   EXSR      REPDATIUTE
019200030422     C*
019300001218     C*
019400001218     C                   ENDSR
019500040430** MSG
019600040430Data errata                                                                   1
019700040430La data spedizione DAL non puo' essere maggiore della data AL                 2
019800040430Immettere almeno una delle due date                                           3
