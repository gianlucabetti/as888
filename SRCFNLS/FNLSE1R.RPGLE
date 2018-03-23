000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010326      * FNLSE1R *-----------------------------------------------------*
000300010312      *            - LISTA PREPAGATI PARTITI -        FILTRO          *
000400010312      *---------------------------------------------------------------*
000500010327      *___________________________________________________________________*
000600010327      *                                                                   *
000700010329      * ATTENZIONE!!!!!! NELL'ULTIMA POSIZIONE DELLA KPJBA (502)          *
000800010329      *                  PASSO UN FLAG PER LA STAMPA DEI PREPAGATI        *
000900010329      *                  ANCORA DA CONFERMARE                             *
001000010327      *___________________________________________________________________*
001100010327      *
001200010326     FFNLSE1D   CF   E             WORKSTN
001300121205     FTABEL00F  IF   E           K DISK
001400121206     Fazorg01l  IF   E           K DISK
001500010312      *
001600010312     D L1              S              3  0 DIM(30)
001700121206     D MSG             S             78    DIM(8) CTDATA PERRCD(1)
001800010312      *
001900950308     D DSLS01        E DS                  INZ
002000930913     D KPJBA         E DS
002100010329     D  SINO                 502    502
002200010312      * DS PER TRUL06R - CARICAMENTO £X
002300970901     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002400970901     D  LIN                    1     90  0
002500970901     D                                     DIM(30)
002600010312      *----------------------------------
002700010312      * PARAMETRI CONTROLLO/INVERSIONE/CALCOLO GIORNI DATA   - XSRDA8 -
002800010312      *----------------------------------
002900921002     D WLBDAT          DS
003000941227     D  G02DAT                 1      8  0
003100941227     D  G02INV                 9     16  0
003200941227     D  G02ERR                17     17
003300941227     D  G02TGI                18     22  0
003400121205     D*----------------------------------
003500121205     D* PARAMETRI RICERCA DATI UTENTI
003600121205     D*----------------------------------
003700121205     d Azuteds       e ds                  extname(Azute00f)
003800121205     d dDatiute      e ds
003900121205     D TIBS34DS      E DS
004000121206     D tibs02ds      E DS
004100121205     d
004200121205     d ds05          e ds
004300121206      *
004400121206     d codut           s              1  0 inz(1)
004500121206     D*
004600010312      *---------------------------------------------------------------*
004700010312      *                    MAIN                                       *
004800010312      * PROGRAMMA FILTRO STAMPA BOLLE PER OPERATORE                   *
004900010312      *---------------------------------------------------------------*
005000010312      *
005100010312      * DEFINIZIONE VARIABILI, KLIST E PLIST
005200930913     C                   EXSR      DEFVAR
005300010312      *
005400010312      * CARICAMENTO FILIALI MECCANIZZATE
005500921002     C                   EXSR      CARTAB
005600010312      *
005700010312     C                   Z-ADD     0             V1DTAL
005800010312      *
005900900509     C     FOR01         TAG
006000010312      *
006100010326     C                   EXFMT     LSE1D01
006200010312     C                   SETOFF                                       28
006300010312      *
006400010312      * F03 - FINE LAVORO
006500900509     C   KC              GOTO      FINE
006600010312      *
006700010312      * CONTROLLO
006800900524     C                   EXSR      CONTR
006900941227     C  NKF
007000010312     COR 28              GOTO      FOR01
007100010312      *
007200010312      * F06 - ELABORAZIONE
007300950308     C                   MOVEL     *BLANKS       DS1FPG
007400010329     C                   Z-ADD     V1DLNP        DS1LNP
007500010312     C                   Z-ADD     V1DTDA        DS1GDA
007600010312     C                   Z-ADD     V1DTAL        DS1GAL
007700010312     C                   Z-ADD     VDTDA         DS1ADA
007800010312     C                   Z-ADD     VDTAL         DS1AAL
007900121206     c                   movel(P)  v1dcar        ds1flo
008000950308     C                   MOVEL     DSLS01        KPJBU
008100121206     c
008200010327     C                   MOVEL     V1CONF        SINO
008300010329     C                   MOVEL     'LSE2'        KCOAZ
008400010329     C                   CALL      'BCH10'
008500010329     C******!!!!!!!!!!     CALL 'FNLSE2R'
008600900523     C                   PARM                    KPJBA
008700010312      *
008800000000     C     FINE          TAG
008900010312      *
009000000000     C                   SETON                                        LR
009100010312      *---------------------------------------------------------------*
009200010312      *                    CONTR                                      *
009300010312      * ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
009400010312      *---------------------------------------------------------------*
009500900524     C     CONTR         BEGSR
009600010312      *
009700010312     C                   SETOFF                                       4142
009800010312      *
009900010312      * DATA DAL OBBLIGATORIA
010000010312     C     V1DTDA        IFEQ      *ZEROS
010100010312     C                   SETON                                        41  28
010200010312     C                   MOVEL     MSG(1)        V1CMSG
010300010312     C                   GOTO      ENDCTR
010400010312     C                   ENDIF
010500010312      *
010600010312      * CONTROLLO DATA DAL
010700010312     C                   CLEAR                   WLBDAT
010800010312     C                   MOVE      V1DTDA        G02DAT
010900921002     C                   MOVEL     *BLANK        G02ERR
011000941227     C                   CALL      'XSRDA8'
011100921002     C                   PARM                    WLBDAT
011200010312     C     G02ERR        IFEQ      '1'
011300010312     C                   SETON                                        41  28
011400010312     C                   MOVEL     MSG(2)        V1CMSG
011500921002     C                   GOTO      ENDCTR
011600010312     C                   ENDIF
011700010312     C                   Z-ADD     G02INV        VDTDA
011800010312     C                   Z-ADD     G02DAT        V1DTDA
011900010312      *
012000010312      * CONTROLLO DATA AL
012100010312     C     V1DTAL        IFNE      0
012200010312     C                   CLEAR                   WLBDAT
012300010312     C                   MOVE      V1DTAL        G02DAT
012400921002     C                   MOVEL     *BLANK        G02ERR
012500941227     C                   CALL      'XSRDA8'
012600921002     C                   PARM                    WLBDAT
012700010312     C     G02ERR        IFEQ      '1'
012800010312     C                   SETON                                        42  28
012900010312     C                   MOVEL     MSG(2)        V1CMSG
013000921002     C                   GOTO      ENDCTR
013100010312     C                   ENDIF
013200010312     C                   Z-ADD     G02INV        VDTAL             8 0
013300010312     C                   Z-ADD     G02DAT        V1DTAL
013400010312     C                   ENDIF
013500010312      *
013600010312      * DATA DAL MINORE DATA AL
013700010312     C     VDTDA         IFGT      VDTAL
013800010312     C     VDTAL         ANDNE     *ZEROS
013900010312     C                   SETON                                        4128
014000010312     C                   MOVEL     MSG(4)        V1CMSG
014100930913     C                   GOTO      ENDCTR
014200010312     C                   ENDIF
014300121205     c
014400121210     c* Immettere filiale o area
014500121210     c                   if        (v1dcar<=*zeros and v1dlnp=0) or
014600121210     c                             (v1dcar>*zeros and v1dlnp>0)
014700121210     c                   seton                                        904628
014800121210     c                   movel     msg(8)        v1cmsg
014900121210     c                   goto      endctr
015000121210    2c                   endif
015100121210
015200121205     c                   clear                   v1ddar
015300121205    1c                   if        v1dcar<>*blanks
015400121210     c                   exsr      deccar
015500121210    2c                   if        *in28
015600121210     c                   seton                                        9046
015700121206     c                   movel     msg(6)        v1cmsg
015800121205     c                   goto      endctr
015900121205    2c                   endif
016000121205    1c                   endif
016100121206     c
016200121206     c                   if        v1dlnp>0
016300121210     c     v1dlnp        chain     azorg01l
016400121210     c                   if        not %found(azorg01l) or
016500121210     c                             orgfva<>' ' or (orgfag<>'F'
016600121210     c                             and orgfag<>'A')
016700121206     C                   SETON                                        439028
016800121206     c                   movel     msg(7)        v1cmsg
016900121206     C                   GOTO      ENDCTR
017000121206     c                   endif
017100121210     c                   endif
017200121206     c
017300010312      *
017400900524     C     ENDCTR        ENDSR
017500010312      *---------------------------------------------------------------*
017600010312      *                    CARTAB                                     *
017700010312      * ROUTINE DI CARICAMENTO FILIALI MECCANIZZATE                   *
017800010312      *---------------------------------------------------------------*
017900921002     C     CARTAB        BEGSR
018000010312      *
018100970901     C                   CLEAR                   DSUL06
018200970901     C                   MOVE      '£1'          D06COD
018300970901     C                   MOVEL     SIMFEL        D06KEY
018400970901     C                   MOVEL     'L'           D06TLA
018500000103     C                   MOVEL     DSUL06        KPJBU
018600010312      *
018700970901     C                   CALL      'TRUL06R'
018800970901     C                   PARM                    KPJBA
018900970901     C                   MOVEL     KPJBU         DSUL06
019000010312     C                   MOVEA     LIN           L1
019100010312      *
019200020429      * VEDO SE SONO PRIMO O SECONDO LIVELLO
019300121205     c*  se ono abilitato ad un'area --> non imposto il campo della linea
019400121206     C                   Z-ADD     dutPOU        V1DLNP
019500121206     C                   Z-ADD     dutPOU        DS1FGS
019600121206     C     dutlpo        IFEQ      '2'
019700121206     C     dutlpo        OREQ      *BLANKS
019800020429     C                   SETON                                        05
019900121205     c                   endif
020000010312      *
020100930913     C                   ENDSR
020200121205     C*---------------------------------------------------------------*
020300121205     C*                    DECCAR                                     *
020400121205     C* Decodifica codice area                                        *
020500121205     C*---------------------------------------------------------------*
020600121205     c     deccar        begsr
020700121205     c                   eval      cod='05'
020800121206     c                   eval      key=v1dcar
020900121205     c     ktab          chain     tabel00f
021000121205    2c                   if        %found(tabel00f)
021100121205     c                   movel     tbluni        ds05
021200121206     c                   eval      v1ddar=§05des
021300121210     c                   else
021400121210     c                   seton                                        2890
021500121205    2c                   endif
021600121205     c                   endsr
021700010312      *---------------------------------------------------------------*
021800010312      *                    DEFVAR                                     *
021900010312      * ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
022000010312      *---------------------------------------------------------------*
022100930913     C     DEFVAR        BEGSR
022200010312      *
022300930913     C     *ENTRY        PLIST
022400930913     C                   PARM                    KPJBA
022500010312      *
022600121205     c     *dtaara       define    §azute        azuteds
022700121205     c     *dtaara       define    §datiute      ddatiute
022800121205     c                   in(E)     *dtaara
022900121205    1c                   If        %error  or RSUT = *blanks
023000121205     c                   Clear                   Tibs34ds
023100121205     c                   Call      'TIBS34R'
023200121205     c                   Parm                    Tibs34ds
023300121205     c                   In        *dtaara
023400121205    1c                   EndIf
023500121205
023600010312      * Reperisce data e ora
023700010312     C                   TIME                    W0140            14 0
023800010312      * UDATE IN GGMMAAAA
023900010312     C                   MOVE      W0140         WDTGIO            8 0
024000010312      * UDATE IN AAAAMMGG
024100010312     C                   CLEAR                   WLBDAT
024200010312     C                   Z-ADD     WDTGIO        G02DAT
024300010312     C                   MOVEL     *BLANK        G02ERR
024400010312     C                   CALL      'XSRDA8'
024500010312     C                   PARM                    WLBDAT
024600010312     C                   Z-ADD     G02INV        VDTDA             8 0
024700010312     C                   MOVEL     G02DAT        V1DTDA
024800121205     C* ACCESSO TABEL00F
024900121205     C     KTAB          KLIST
025000121205     C                   KFLD                    CODUT
025100121205     C                   KFLD                    COD
025200121205     C                   KFLD                    KEY
025300121205     C* DEFINIZIONE CAMPI
025400121205     C     *LIKE         DEFINE    TBLCOD        COD
025500121205     C     *LIKE         DEFINE    TBLKEY        KEY
025600010312      *
025700930913     C                   ENDSR
025800010312      *-------------------------------------------------------------------------
025900010312** MSG  Lungh. 78                                                            *
026000010312Inserire la data
026100010312Data errata
026200010312P.O. gestione errato
026300010312Range data errato
026400121210
026500121210Codice area errato                                                            06
026600121210Linea di partenza errata                                                      07
026700121210Immettere la linea di partenza o l'area                                       08
