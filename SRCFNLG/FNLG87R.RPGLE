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
007500180119     c                   if        vsel='2'
007600170222     C                   MOVEL     Vord          D87ord
007700180119     c                   else
007800180119     C                   MOVEL     Vordport      D87ord
007900180119     c                   endif
008000180119     c
008100170307     C                   MOVEL     Vfdep         D87fdep
008200170331     C                   MOVEL     Vmes          D87mes
008300941202     C                   MOVEL     VRIST         D87RIS
008400941202     C     VRIST         IFEQ      'R'
008500941202     C                   Z-ADD     G08INV        D87DAT
008600941202     C                   ELSE
008700941202     C                   MOVE      *ZEROS        D87DAT
008800941202     C                   ENDIF
008900050322     C                   MOVE      'S'           D87COMm
009000170221     c                   movel     vfgs          d87fgs
009100941202     C                   MOVEL     FNLG87        KPJBU
009200941202     C                   MOVEL     'LG86'        KCOAZ
009300071011     C                   CALL      'BCH10'
009400920522     C                   PARM                    KPJBA
009500170221     c
009600170221     c                   setoff                                       05
009700920522     C                   END
009800170221     c
009900170221     C                   ENDdo
010000170221     C                   ENDdo
010100920522     C*
010200920522     C                   SETON                                        LR
010300941202     C*---------------------------------------------------------------*
010400941202     C     *INZSR        BEGSR
010500941202     C     *ENTRY        PLIST
010600941202     C                   PARM                    KPJBA
010700941202     C                   Z-ADD     1             CODUT
010800941202     C                   CALL      'X§PARUT'
010900170221     C                   PARM                    UTEDSE
011000170221     C                   MOVEL     REC80         CNCR80
011100941202     C                   MOVEL     RAGUT         RSUT             20
011200170221     c
011300170221     c                   movel     simpou        vfgs
011400170221     c     simpou        chain     azorg01l
011500170221     c                   if        %found(azorg01l)
011600170221     c                   movel     orgdes        v1dfgs
011700170221     c                   endif
011800170221     c
011900170221     c* il cambio filiale gestione lo do a chi è terminal di arrivo non partenza
012000170221     c*
012100170221     c                   clear                   trul09ds
012200170221     c                   eval      i09mod='TFA'
012300170221     c                   eval      i09tfa=simpou
012400170221     c                   call      'TRUL09R'
012500170221     c                   parm                    trul09ds
012600170221     c
012700170221     c                   if        o09err<>' ' or t09(1)=000
012800170221     c                   seton                                        06
012900170221     c                   endif
013000170221     c* se ce ne è solo uno ed è uguale a se stesso non F18
013100170221     c                   if        t09(2)=000 and t09(1)=simpou
013200170221     c                   seton                                        06
013300170221     c                   endif
013400941202     C*----------------------------------------------------*
013500941202     C                   Z-ADD     0             VDATA
013600941202     C                   MOVEL     '1'           VSEL
013700170302     c                   exsr      Impo_ord
013800170302     c
013900170302     c
014000941202     C                   CLEAR                   FNLG87
014100941202     C                   ENDSR
014200170221
014300170302      * imposto il tipo ordinamento in base all'organigramma ---------*
014400170331     c     Impo_ord      BEGSR
014500170331     c                   movel     orgdf0        og150
014600170331
014700170302     C***                MOVEL     'Z'           Vord
014800170302     c* imposto "L" escluse le filiali messaggerie e le filiali senza pick
014900170302     C                   MOVEL     'L'           Vord
015000180119     C                   MOVEL     'L'           Vordport
015100170302     c                   movel     orgde3        og143
015200170302     c                   if        §ogntw='MES'
015300170302     C                   MOVEL     'Z'           Vord
015400180119     C                   MOVEL     'Z'           Vordport
015500170302     c                   else
015600170302     c                   if        §OGPCK=' '
015700170302     C                   MOVEL     'Z'           Vord
015800180119     C                   MOVEL     'Z'           Vordport
015900170302     c                   endif
016000170302     c                   endif
016100170331     c
016200170331     c* visualizzo o meno parzializzazione per zone messaggerie
016300170331     c                   if        §ogpkm='S'
016400170331     c                   seton                                        01
016500170331     c                   else
016600170331     c                   setoff                                       01
016700170331     c                   endif
016800170307     c                   clear                   vfdep
016900170331     c                   clear                   vmes
017000170302     c
017100170302     c                   eval      savfgs=vfgs
017200170302     c                   ENDSR
017300170302     c*
017400170221      * CONTROLLO IL P.O. IN IN GESTIONE -----------------------------*
017500170221     C     CTRFGS        BEGSR
017600170221     C*
017700170221     c     vfgs          lookup    t09                                    30
017800170221     C     *IN30         IFEQ      *OFF
017900170221     C     Vfgs          OREQ      0
018000170221     C                   CLEAR                   V1DFGS
018100170221     C                   SETON                                        4099
018200170221     C                   GOTO      ENDCTF
018300170221     C                   ENDIF
018400170221     C* DECODIFICA
018500170221     C     Vfgs          CHAIN     AZORG                              31
018600170221     C  N31              MOVEL     ORGDES        V1DFGS
018700170221     C*
018800170221     C* non GESTITA DA ALTRO P.O.
018900170221     c                   clear                   fnlv55ds
019000170221     c                   eval      d55tpt='6'
019100170221     c                   eval      d55lin=vfgs
019200170221     c     *iso          movel     datasys       d55drf
019300170221     c                   call      'FNLV55R'
019400170221     c                   parm                    fnlv55ds
019500170221     C                   IF        D55tfa<>d55lin
019600170221     C                   SETON                                        4099
019700170221     C                   ENDIF
019800170331     c* reimposto l'ordinamento
019900170302     c                   if        vfgs<>savfgs
020000170302     c                   EXSR      Impo_ord
020100170302     c                   endif
020200170221     C*
020300170221     C     ENDCTF        ENDSR
