000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941202     H* FNLG87R *----------------------------------------------------*
000300941202     H*   - STAMPA ELENCO MATERIALE X MAGAZZINO GIACENZE  -FILTRO-   *
000400900418     H*--------------------------------------------------------------*
000500170221     FAZORG01L  IF   E           K DISK
000600941202     FFNLG87D   CF   E             WORKSTN
000700941202      *
000800941202     D WLBDA8          DS
000900941202     D  G08DAT                 1      8  0
001000941202     D  G08INV                 9     16  0
001100941202     D  G08ERR                17     17
001200941202     D  G08TGI                18     22  0
001300941202      *
001400941202     D                SDS
001500941202     D  §PGM                   1     10
001600921012      *
001700170221     D CNCR80        E DS
001800921012     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001900941202      *
002000170302     D og143         E DS
002100170302     D og150         E DS
002200170302     D KPJBA         E DS
002300170221     D fnlv55ds      E DS
002400941202      *
002500941202     D FNLG87        E DS                  EXTNAME(FNLG87DS)
002600170221     d Trul09ds      e ds
002700170221     d  T09                   19    108  0 dim(30)
002800170221     d
002900170302     d savfgs          s                   like(vfgs) inz
003000170221     d Datasys         s               d   datfmt(*iso) inz(*sys)
003100170221     C*--------------------------------------------------------------
003200170221     C* F3 - FINE
003300920522     C     *INKC         DOWEQ     '0'
003400921012     C*
003500941202     C                   EXFMT     LG8701D
003600170221
003700920522     C  NKC              DO
003800920522     C                   SETOFF                                       99
003900170221     C* F18 - CAMBIO P.O. IN GESTIONE
004000170221     c                   if        *inks
004100170221     C                   SETON                                        0599
004200170221     c                   LEAVE
004300170221     c                   endif
004400170221
004500170221     C* 05 ON - CONTROLLO P.O. IN  GESTIONE
004600170221     C   05              EXSR      CTRFGS
004700170221     C   05
004800170221     CAN 99              leave
004900920522     C* CONTROLLO DATA
005000941202     C     VDATA         IFNE      *ZEROS
005100941202     C     VRIST         IFEQ      'R'
005200941202     C                   Z-ADD     VDATA         G08DAT
005300941202     C                   MOVE      *ZEROS        G08INV
005400941202     C                   MOVEL     *BLANK        G08ERR
005500941202     C                   CALL      'XSRDA8'
005600941202     C                   PARM                    WLBDA8
005700941202     C     G08ERR        COMP      '1'                                    10
005800941202     C   10              SETON                                        1499
005900941202     C  N10              Z-ADD     G08DAT        VDATA
006000930519     C                   ELSE
006100930519     C* SE IMMESSA LA DATA CI VUOLE "R"
006200930519     C                   SETON                                        1699
006300941202     C                   END
006400941202     C                   ELSE
006500930519     C* SE IMMESSA "R" CI VUOLE LA DATA
006600941202     C     VRIST         IFEQ      'R'
006700930519     C                   SETON                                        1599
006800941202     C                   END
006900941202     C                   END
007000170221
007100920522     C*  SE NON CI SONO ERRORI SOTTOMETTO LA STAMPA
007200920522     C  N99
007300920522     CAN KF              DO
007400941202     C                   MOVEL     VSEL          D87TST
007500170222     C                   MOVEL     Vord          D87ord
007600941202     C                   MOVEL     VRIST         D87RIS
007700941202     C     VRIST         IFEQ      'R'
007800941202     C                   Z-ADD     G08INV        D87DAT
007900941202     C                   ELSE
008000941202     C                   MOVE      *ZEROS        D87DAT
008100941202     C                   ENDIF
008200050322     C                   MOVE      'S'           D87COMm
008300170221     c                   movel     vfgs          d87fgs
008400941202     C                   MOVEL     FNLG87        KPJBU
008500941202     C                   MOVEL     'LG86'        KCOAZ
008600071011     C                   CALL      'BCH10'
008700920522     C                   PARM                    KPJBA
008800170221     c
008900170221     c                   setoff                                       05
009000920522     C                   END
009100170221     c
009200170221     C                   ENDdo
009300170221     C                   ENDdo
009400920522     C*
009500920522     C                   SETON                                        LR
009600941202     C*---------------------------------------------------------------*
009700941202     C     *INZSR        BEGSR
009800941202     C     *ENTRY        PLIST
009900941202     C                   PARM                    KPJBA
010000941202     C                   Z-ADD     1             CODUT
010100941202     C                   CALL      'X§PARUT'
010200170221     C                   PARM                    UTEDSE
010300170221     C                   MOVEL     REC80         CNCR80
010400941202     C                   MOVEL     RAGUT         RSUT             20
010500170221     c
010600170221     c                   movel     simpou        vfgs
010700170221     c     simpou        chain     azorg01l
010800170221     c                   if        %found(azorg01l)
010900170221     c                   movel     orgdes        v1dfgs
011000170221     c                   endif
011100170221     c
011200170221     c* il cambio filiale gestione lo do a chi è terminal di arrivo non partenza
011300170221     c*
011400170221     c                   clear                   trul09ds
011500170221     c                   eval      i09mod='TFA'
011600170221     c                   eval      i09tfa=simpou
011700170221     c                   call      'TRUL09R'
011800170221     c                   parm                    trul09ds
011900170221     c
012000170221     c                   if        o09err<>' ' or t09(1)=000
012100170221     c                   seton                                        06
012200170221     c                   endif
012300170221     c* se ce ne è solo uno ed è uguale a se stesso non F18
012400170221     c                   if        t09(2)=000 and t09(1)=simpou
012500170221     c                   seton                                        06
012600170221     c                   endif
012700941202     C*----------------------------------------------------*
012800941202     C                   Z-ADD     0             VDATA
012900941202     C                   MOVEL     '1'           VSEL
013000170302     c                   exsr      Impo_ord
013100170302     c
013200170302     c
013300941202     C                   CLEAR                   FNLG87
013400941202     C                   ENDSR
013500170221
013600170302      * imposto il tipo ordinamento in base all'organigramma ---------*
013700170302     c     Impo_ord      BEGSR
013800170302     C***                MOVEL     'Z'           Vord
013900170302     c* imposto "L" escluse le filiali messaggerie e le filiali senza pick
014000170302     C                   MOVEL     'L'           Vord
014100170302     c                   movel     orgde3        og143
014200170302     c                   if        §ogntw='MES'
014300170302     C                   MOVEL     'Z'           Vord
014400170302     c                   else
014500170302     c                   movel     orgdf0        og150
014600170302     c                   if        §OGPCK=' '
014700170302     C                   MOVEL     'Z'           Vord
014800170302     c                   endif
014900170302     c                   endif
015000170302     c
015100170302     c                   eval      savfgs=vfgs
015200170302     c                   ENDSR
015300170302     c*
015400170221      * CONTROLLO IL P.O. IN IN GESTIONE -----------------------------*
015500170221     C     CTRFGS        BEGSR
015600170221     C*
015700170221     c     vfgs          lookup    t09                                    30
015800170221     C     *IN30         IFEQ      *OFF
015900170221     C     Vfgs          OREQ      0
016000170221     C                   CLEAR                   V1DFGS
016100170221     C                   SETON                                        4099
016200170221     C                   GOTO      ENDCTF
016300170221     C                   ENDIF
016400170221     C* DECODIFICA
016500170221     C     Vfgs          CHAIN     AZORG                              31
016600170221     C  N31              MOVEL     ORGDES        V1DFGS
016700170221     C*
016800170221     C* non GESTITA DA ALTRO P.O.
016900170221     c                   clear                   fnlv55ds
017000170221     c                   eval      d55tpt='6'
017100170221     c                   eval      d55lin=vfgs
017200170221     c     *iso          movel     datasys       d55drf
017300170221     c                   call      'FNLV55R'
017400170221     c                   parm                    fnlv55ds
017500170221     C                   IF        D55tfa<>d55lin
017600170221     C                   SETON                                        4099
017700170221     C                   ENDIF
017800170302     c* reimposto l'ordinamentio
017900170302     c                   if        vfgs<>savfgs
018000170302     c                   EXSR      Impo_ord
018100170302     c                   endif
018200170221     C*
018300170221     C     ENDCTF        ENDSR
