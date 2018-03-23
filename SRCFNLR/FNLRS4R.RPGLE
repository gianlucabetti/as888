000100031023     H DECEDIT('0,') DATEDIT(*ymd.)
000200040520     H* FNLRS4R *----------------------------------------------------*
000300040520     H*         - STAMPA SEGNACOLLI ESTERI DPD IN ARRIVO
000400960404     H*--------------------------------------------------------------*
000500960329     FAZORG01L  IF   E           K DISK
000600000328     Ffnfvv01l  IF   E           K DISK
000700040520     FFNLRS4D   CF   E             WORKSTN
000800030611
000900040520     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
001000040520     D FNLRS4DS      E DS                  inz
001100040109     D FNLv54ds      E DS                  inz
001200000327     D dslr01        E DS                  inz
001300960326     D KPJBA         E DS
001400960329     D CNCR80        E DS
001500030611     D ut§dse0f      E DS
001600040520      * Parametri per stampa BOLLE ed ETICHETTE
001700040520     d TRUL90DS      e ds                  inz
001800040520     d   D90rse      e                     inz('S')
001900031028     d og150         e ds
002000040520
002100040520     D WLBDAT          DS
002200040520     D  G02DAT                 1      8  0
002300040520     D  G02INV                 9     16  0
002400040520     D  G02ERR                17     17
002500040520     D  G02TGI                18     22  0
002600030611
002700030611      ****************************************************************
002800030611      *  RIEPILOGO INDICATORI FILE VIDEO
002900030611      ***************************************************************
003000030611      * 28 - Video - MSG Errore
003100040520      * 30 - comodo
003200040520      * 31 - comodo
003300030611      * 46 - Video - numero foglio
003400040520      * 47 - Video - lna
003500040520      * 90 - Errore
003600030611      ***************************************************************
003700040520      * Richiesta stampanti
003800040520     c                   reset                   TRUL90DS
003900040520     c                   call      'TRUL90R'
004000040520     c                   parm                    KPJBA
004100040520     c                   parm                    TRUL90DS
004200040520     C**
004300040520     C* F3 - FINE
004400040520     C     D90F3         IFEQ      '1'
004500040520     C                   GOTO      FINE
004600040520     C                   ENDIF
004700040520     c                   clear                   fnlrs4ds
004800040520      * Parametri x ovrprtf
004900040520      * · stampante per etichette
005000040520     c                   movel     D90pre        ds1D90pre
005100040520      * · modulo per stampa etichette
005200040520     c                   movel     D90mde        ds1d90mde
005300040520      * · pgm. di stampa etichette segnacolli
005400040520     c                   movel     D90pss        ds1D90pss
005500040520      * GESTIONE VIDEATA - RICHIESTA FOGLIO ARRIVI
005600030611      * Impostazione campi
005700040520     c                   z-add     2             v1ccfa
005800040520     c                   z-add     simpou        v1cpfa
005900040520     c                   clear                   v1cnfa
006000040520     c                   clear                   v1cdfv
006100040520     c                   clear                   v1cdel
006200000327     c     emd01         tag
006300040520     c                   exfmt     lrs4d01
006400000327     c                   clear                   v1zmsg
006500040520     c                   setoff                                       9028
006600000329     c     *inkc         cabeq     '1'           fine
006700000327     c*
006800000327     c                   exsr      ctrd01
006900000329     c   90              goto      emd01
007000040520     c  nkf              goto      emd01
007100040520     c* richiamo pgm batch
007200040520     c                   eval      ds1npg=v1ccfa
007300040520     c                   eval      ds1fgs=v1cpfa
007400040520     c                   move      v1cnfa        ds1nfa
007500040520     c                   eval      ds1lna=v1clna
007600040521     c                   eval      ds1dfa=fvvdfv
007700040520     c                   clear                   kpjbu
007800040520     c                   movel     fnlrs4ds      kpjbu
007900040521     c                   movel     'LRX5'        kcoaz
008000040520     c                   call      'BCH10'
008100040520     c                   parm                    kpjba
008200960327     C*
008300960326     C     FINE          TAG
008400960521     C*
008500040112     C                   MOVEL     'C'           I54TLA
008600040112     C                   CALL      'FNLV54R'
008700040112     C                   PARM                    fnlv54ds
008800960521     C                   SETON                                        LR
008900031031
009000000327     C**************  Controlli prima videata   ***********************
009100000327     C     CTRd01        BEGSR
009200040520     c                   setoff                                       4647
009300040520     c                   clear                   v1cdfv
009400040520     c                   clear                   v1cdel
009500040109     c
009600000327     c* numero foglio arrivi
009700000328    1c     v1cnfa        ifeq      *blanks
009800000327     c     v1cnfa        oreq      *zeros
009900000327     c                   seton                                        462890
010000000327     c                   movel     msg(4)        v1zmsg
010100000327     c                   goto      endct1
010200000328   x1c                   else
010300000327     c     '?'           scan      v1cnfa                                 30
010400000328    2c   30              do
010500000327     c                   clear                   dslr01
010600000327     c                   movel     'S'           dlrric
010700040520     c                   movel     v1ccfa        dlrnpg
010800000327     c                   movel     '2'           dlrtfv
010900040520     C                   MOVEL     'FNLRS4R'     DLRPGM
011000000327     c                   z-add     31129999      dlrgal
011100000327     c                   z-add     99991231      dlraal
011200040109     c                   movel     v1cpfa        dlrfgs
011300000327     c                   movel     dslr01        kpjbu
011400000327     c                   call      'FNLR02R'
011500000327     c                   parm                    kpjba
011600000327     c                   movel     kpjbu         dslr01
011700000327     c                   movel     dlrnfv        v1cnfa
011800000327     c                   seton                                        90
011900000327     c                   goto      endct1
012000000328    2c                   end
012100000327     c                   testn                   v1cnfa               31
012200000328    2c     *in31         ifeq      *on
012300000328     c                   movel     v1cnfa        w0050             5 0
012400000328     c     kfvv1         chain     fnfvv01l                           30
012500000328    2c                   endif
012600000328    2c     *in31         ifeq      *off
012700000328     c     *in30         oreq      *on
012800000328     c     fvvatb        orne      *blanks
012900000328     c                   seton                                        469028
013000000329     c                   movel     msg(4)        v1zmsg
013100000328     c                   goto      endct1
013200000328    2c                   endif
013300000328    1c                   endif
013400040520     C* DATA FOGLIO ARRIVI
013500040520     C                   MOVE      FVVDFV        G02INV
013600040520     C                   MOVE      *ZEROS        G02DAT
013700040520     C                   MOVEL     3             G02ERR
013800040520     C                   CALL      'XSRDA8'
013900040520     C                   PARM                    WLBDAT
014000040520     C                   Z-ADD     G02DAT        V1cdfv
014100040520     c                   movel     'Del'         v1cdel
014200040520     c* controllo la lna di arrivo se immessa
014300040520    1c     v1clna        ifgt      *zeros
014400040520     c     v1clna        chain     azorg01l                           30
014500040520     c  n30              movel     orgdf0        og150
014600040520     c   30              clear                   og150
014700040520    2c     §ogsea        ifne      'S'
014800040520     c                   movel     msg(2)        v1zmsg
014900040520     c                   seton                                        479028
015000040520     c                   goto      endct1
015100040520    2c                   endif
015200040520     c                   clear                   fnlv54ds
015300040520     c                   eval      i54tpt = 'A'
015400040520     c                   eval      i54lin = v1clna
015500040520     c                   eval      i54drf = fvvdfv
015600040520     c                   call      'FNLV54R'
015700040520     c                   parm                    fnlv54ds
015800040520    2c     o54tfa        ifne      v1cpfa
015900040520     c                   seton                                        479028
016000040520     c                   movel     msg(1)        v1zmsg
016100040520     c                   goto      endct1
016200040520    2c                   endif
016300040520    1c                   endif
016400000327     c     endct1        endsr
016500960326     C**************  OPERAZIONI INIZIALI  ****************************
016600960326     C     *INZSR        BEGSR
016700960326     C*
016800000324     C     *ENTRY        PLIST
016900000324     C                   PARM                    KPJBA
017000000324     c*
017100960326     C                   Z-ADD     1             CODUT
017200960326     C                   CALL      'X§PARUT'
017300030611     C                   PARM                    ut§dse0f
017400000324     C                   MOVEL     RAGUT         RSUT
017500960329     C                   MOVEL     REC80         CNCR80
017600000328     c*
017700000328     c     kfvv1         klist
017800040520     c                   kfld                    v1ccfa
017900040520     c                   kfld                    w0050
018000040109     c                   kfld                    v1cpfa
018100960326     C*
018200990513     C                   ENDSR
018300030611      *----------------------------------------------------------------
018400990512** MSG
018500040520Linea di arrivo appartenente ad altra area di arrivo                                              1
018600040520Linea di arrivo inesistente o errata                                                              2
018700040520                                                                                                  3
018800000329Foglio errato                                                                                     4
