000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500010109      *       MODIFICA DATA ELABORAZIONE RITIRI FISSI                 *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200010108      * 05 - SPROTEGGO P.O. IN GESTIONE
001300001010      * 28 - ERRORE GENERICO DSPF
001400001010      * 30 - Comodo
001500010108      * 40 - ERRORE p.o. in gestione
001600010109      * 46 - ERRORE Data elaborazione
001700001010      ****************************************************************
001800001010
001900010109     FFNORS01L  UF   E           K DISK
002000001011     FAZORG01L  IF   E           K DISK
002100010109     FFIOR14D   CF   E             WORKSTN
002200001010
002300001010      *------------------------------------------------------------------------*
002400001026     D kazorg          S                   LIKE(ORGfil)
002500001030
002600010109     D vdela           S                   LIKE(v1dela)
002700010108
002800010109     D §doggi          S                   LIKE(v1dela)
002900001025     D §kpjbu          S                   LIKE(KPJBU)
003000001009
003100001006     D W0140           S             14  0
003200001006     D Wdtgio          S              8  0
003300001006     D dateu           S              8  0
003400001009
003500001006     D DATA_oggi       S               D   DATFMT(*eur)
003600001006
003700001010      *  titolo videata (lunghezza massima 34)
003800010109     D TIT_A           C                   CONST('***  Mod.data elaborazione   -
003900001010     D                                       ***')
004000001031
004100001031      *   S C H I E R E
004200010109     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
004300010108     D L1              S              3  0 DIM(30)                              P.O. Gestiti
004400001006
004500001010      *   D S   I N T E R N E / E S T E R N E
004600001009
004700001009     D WLBDAT          DS                  INZ
004800001009     D  G02DAT                 1      8  0
004900001009     D  G02INV                 9     16  0
005000001009     D  G02ERR                17     17
005100001009     D  G02TGI                18     22  0
005200001009
005300001016     D UT§DSE0F      E DS
005400001016     D CNCR80        E DS
005500010108
005600010108     D TRUL06DS      E DS
005700010108     D  LIN                    1     90  0
005800010108     D                                     DIM(30)
005900001010
006000001010      *
006100001010     D KPJBA         E DS
006200020502     D* rem                    1      3
006300020502     D* remfil                 4      6
006400001010      *
006500001010     D                SDS
006600001010     D  VTCPGM                 1     10
006700001010
006800001006      *------------------------------------------------------------------------*
006900010109
007000010108
007100010108      * Emissione 1° Videata
007200010108
007300010108     C                   do        *hival
007400010109      * Pulizia Videata
007500010109     C                   if        (*in28 = *off  and  *in05 = *off)
007600010109     C                   exsr      Sr_Pulvid
007700010108     C                   endif
007800010108
007900010109     C                   exfmt     fior141
008000010108
008100010108      * F3=Fine
008200010108     C   kc              leave
008300010109     C                   setoff                                       28
008400010108
008500010108      * F18=Cambio P.O. in gestione
008600010108     C   ks              eval      *in05 = *on
008700010108     C   ks              iter
008800010108
008900010108      * 05 on --> controllo P.O. in gestione
009000010108     C                   if        *in05 = *on
009100010108     C                   exsr      Sr_Contrpo
009200010108     C                   eval      *in05 = *off
009300010108     C                   endif
009400010109
009500010109      * Controlli
009600010109     C                   exsr      Sr_Contr
009700010109
009800010109      * F6=Elabora
009900010109     C   kf              exsr      Sr_Modifica
010000010109     C   kf
010100010109     Cann28              leave
010200010108
010300010108     C   28              iter
010400010108
010500010108     C                   enddo
010600001010
010700001006     C                   eval      *inlr = *on
010800010108      *****************************************************************
010900010109      * PULIZIA VIDEATA
011000010108      *****************************************************************
011100010109     C     Sr_Pulvid     BEGSR
011200010108
011300010109     C                   movel     data_oggi     v1dela
011400010108
011500010108     C                   endsr
011600010108      **********************************************************************
011700010108      * CONTROLLO IL P.O. IN IN GESTIONE
011800010108      **********************************************************************
011900010108     C     Sr_Contrpo    begsr
012000010108
012100010108     C                   setoff                                       40
012200010108     C                   clear                   v1dfgs
012300010108
012400010108     C     v1cfgs        lookup    L1                                     30
012500010108     C     *in30         ifeq      *off
012600010108     C                   seton                                        40  28
012700010109     C                   movel     msg(3)        v1cmsg
012800010108     C                   else
012900010108
013000010108      * Decodifica
013100010108     C     v1cfgs        chain     azorg01L
013200010108     C                   if        %found(azorg01l) and ORGfva = *blanks
013300010108     C                   movel     orgDES        v1dfgs
013400010108     C                   else
013500010108     C                   seton                                        40  28
013600010109     C                   movel     msg(3)        v1cmsg
013700010108     C                   endif
013800010108     C                   endif
013900010108
014000010108     C                   endsr
014100010108      **********************************************************************
014200010109      * CONTROLLI VIDEO
014300010108      **********************************************************************
014400010109     C     Sr_Contr      begsr
014500010108
014600010108      * Reset indicatori
014700010108
014800010109     C                   setoff                                       41
014900010108
015000010109     C                   if        v1dela =  *zeros
015100010109     C                   seton                                        41  28
015200010109     C                   movel     msg(1)        v1cmsg
015300010109     C                   else
015400010109     C                   exsr      Sr_Contrdela                                 controllo data ela.
015500010108     C                   endif
015600010108
015700010109     C                   endsr
015800010108      **********************************************************************
015900010109      * Controlla data elaborazione
016000010108      **********************************************************************
016100010109     C     Sr_Contrdela  BEGSR
016200010108
016300010108     C                   clear                   wlbdat
016400010109     C                   z-add     v1dela        G02dat
016500010108     C                   call      'XSRDA8'
016600010108     C                   parm                    wlbdat
016700010108     C                   if        G02err = '1'
016800010109     C                   eval      *in41 = *ON
016900010108     C                   eval      *in28 = *ON
017000010109     C                   movel     msg(2)        v1cmsg
017100010108     C                   else
017200010109     C                   z-add     G02dat        v1dela
017300010109     C                   z-add     G02inv        vdela
017400010108     C                   endif
017500010108
017600010109     C                   endsr
017700001031      **********************************************************************
017800010109      * Aggiorno data elaborazione nei ritiri fissi
017900001031      **********************************************************************
018000010109     C     Sr_Modifica   begsr
018100001031
018200010109     C     v1cfgs        setll     fnors01l
018300010109     C                   do        *hival
018400010109     C     v1cfgs        reade     fnors01l
018500010109     C                   if        %eof(fnors01l)
018600010109     C                   leave
018700010109     C                   endif
018800010109
018900010109     C                   if        ORSdte = vdela
019000010109     C                   clear                   ORSdte
019100040818     c                   Z-add     dateu         OrsDtt
019200010109     C                   update    fnors000
019300010109     C                   endif
019400010109
019500010109     C                   enddo
019600001031
019700001031     C                   endsr
019800001010      *****************************************************************
019900001010      * ROUTINE INIZIALE
020000001010      *****************************************************************
020100001010     C     *INZSR        BEGSR
020200001009
020300001010     C     *ENTRY        PLIST
020400001010     C                   PARM                    KPJBA
020500001011
020600001011     C                   movel     TIT_A         VTCtit
020700001016
020800001016     C                   Z-ADD     1             CODUT
020900001016     C                   CALL      'X§PARUT'
021000001016     C                   PARM                    UT§DSE0F
021100001016     C                   MOVEL     RAGUT         RSUT
021200001016     C                   MOVEL     REC80         CNCR80
021300010108
021400010108      * IMPOSTO IL P.O. DI GESTIONE
021500020502     C*                  if        rem = 'REM'  AND  remfil > *ZEROS
021600020502     C*                  movel     REMFIL        V1CFGS
021700020502     C                   if        simtpp = '2' or simtpp = *blanks
021800020502     C                   movel     simpou        V1CFGS
021900010108     C                   eval      *in06 = *on
022000010108     C                   else
022100010108     C                   movel     SIMFEL        V1CFGS
022200010108     C                   endif
022300001016
022400001019     C                   movel     v1cfgs        kazorg
022500001019     C     kazorg        chain     azorg01L
022600001016     C                   if        %found(azorg01l)
022700001016     C                   movel     orgDES        v1dfgs
022800001016     C                   else
022900001016     C                   clear                   v1dfgs
023000001016     C                   endif
023100010108
023200010108      * Carico schiera P.O. gestiti
023300010108     C                   CLEAR                   TRUL06ds
023400010108     C                   MOVE      '£1'          D06COD
023500010108     C                   MOVEL     SIMFEL        D06KEY
023600010108     C                   MOVEL     TRUL06DS      KPJBU
023700010108     C                   CALL      'TRUL06R'
023800010108     C                   PARM                    KPJBA
023900010108     C                   MOVEL     KPJBU         TRUL06DS
024000010108     C                   MOVEA     LIN           L1
024100001009
024200001023      * reperisce data e ora
024300001010     C                   TIME                    W0140
024400001010      * UDATE IN GGMMAAAA
024500001010     C                   MOVE      W0140         WDTGIO
024600001010      * UDATE IN AAAAMMGG
024700001010     C                   Z-ADD     WDTGIO        G02DAT
024800001010     C                   MOVEL     *BLANK        G02ERR
024900001010     C                   CALL      'XSRDA8'
025000001010     C                   PARM                    WLBDAT
025100001025     C                   Z-ADD     G02INV        DATEU
025200001010     C     *iso          MOVEL     DATEU         DATA_oggi
025300001010
025400001025     C                   z-add     dateu         §doggi
025500001006
025600001006     C                   endsr
025700001010      *---------------------------------------------------------------------------------------------
025800001031** MSG  Lungh. 78                                                            *
025900010109Inserire la data elaborazione                                                       1
026000010109Data elaborazione errata                                                            2
026100070418Filiale gestione errata                                                             3
