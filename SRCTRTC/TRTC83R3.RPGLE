000100960530      ***********************************************************************
000200960530      *   ESPORTA DATI BOLLE VERSO  E.D.I. ESTERO                           *
000300960530      ***********************************************************************
000400960530     H DECEDIT('0,') DATEDIT(*DMY/)
000500960530     F*---------------------------------------------------------------------*
000600960530     F*  DATA BASE                                                          *
000700960530     F*---------------------------------------------------------------------*
000800960530     FEDTAB01L  IF   E           K DISK
000900960530     F*---------
001000050204     FEDIFCSUM  UF A E             DISK
001100030709     F*---------
001200030709     fAzcln01l  if   e           k disk
001300960530     D*---------------------------------------------------------------------*
001400960530     D* Schiere
001500960530     D*---------------------------------------------------------------------*
001600960530     D* Schiera per ricezione testi liberi record TT05
001700960628     D* Schiera per trasmissione totali
001800960530     D* Schiera per reperimento località record TT20
001900960530     D* Schiera per reperimento nomi ed indirizzi spedizionieri
002000960530     D* Tabella partner
002100121105     D CPT             S             35    DIM(200)
002200121105     D DPT             S             90    DIM(%elem(CPT))
002300121105     D KSC             S              7  0 DIM(%elem(CPT))
002400121105     D DPU             S             90    DIM(%elem(CPT))
002500121105     D TRUL0SDS      E DS
002600121112     d kpjbusav        S                   like(Kpjbu)
002700960530     D*---------------------------------------------------------------------*
002800960530     D* DS
002900960530     D*---------------------------------------------------------------------*
003000960530     D* .. per scompozione dati ricevuti a seconda del tipo record
003100960530     D EDTT00        E DS                  EXTNAME(EDTT00DS)
003200960530     D EDTT05        E DS                  EXTNAME(EDTT05DS)
003300960530     D  T05                   13    362
003400960530     D                                     DIM(5)
003500960530     D EDTT10        E DS                  EXTNAME(EDTT10DS)
003600960628     D  T10                   10     53
003700960628     D                                     DIM(2)
003800960530     D EDTT15        E DS                  EXTNAME(EDTT15DS)
003900960530     D EDTT20        E DS                  EXTNAME(EDTT20DS)
004000960726     D  T20                  106    249
004100960726     D                                     DIM(3)
004200960726     D  T20D                 250    303
004300960530     D                                     DIM(3)
004400960530     D EDTT25        E DS                  EXTNAME(EDTT25DS)
004500960530     D  T25                   68    123
004600960530     D                                     DIM(2)
004700960530     D EDTT30        E DS                  EXTNAME(EDTT30DS)
004800960530     D*  Ds per reperimento dati peso/Volume intera spedizione
004900960530     D WTT10           DS
005000960530     D  WCTR                   1      3
005100960530     D  WVLU                   4     19  0
005200960530     D  WTPM                  20     22
005300960530     D*  Ds per reperimento località
005400960530     D WTT20           DS
005500960726     D  WTPL                   1      3
005600960726     D  WLOC                   4     28
005700960726     D  WTP1                  29     31
005800960726     D  WLO1                  32     48
005900960530     D*  Ds per reperimento date partenza/arrivo...
006000960530     D WTT20D          DS
006100960530     D  WTPD                   1      3
006200960530     D  WDTA                   4     15
006300960530     D  WFOR                  16     18
006400960530     D*  Ds per reperimento dati comunicazioni
006500960530     D WTT25           DS
006600960530     D  WTPC                   1      3
006700960530     D  WCOM                   4     28
006800030709     D*
006900030709     d gio             S              1    DIM(31)
007000030709     d fes             S              1    DIM(31)
007100030709      *
007200030709     D AnMeGi          DS
007300030709     d  Kanno                         4s 0
007400030709     d  Kmese                         2s 0
007500030709     d  gg                            2s 0
007600030708      *
007700030708     d  DataAmg        s              8s 0
007800030708     d dataisoP        s               d   datfmt(*iso)
007900030708     d dataisoA        s               d   datfmt(*iso)
008000960530     D*---------------------------------------------------------------------*
008100960530     D* .. per reperimento tipo record di EDIFTMIN letto
008200960530     D*                    .. 5 - 8  : tipo record letto
008300960705     D WDAT            DS          1950
008400960726     D  SUMPRG                 1      4  0
008500960530     D  SUMTPR                 5      8
008600960530     D*---------------------------------------------------------------------*
008700960530     D KPJBA         E DS
008800960530     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
008900960530     D CNCR80        E DS
009000960530     D TRTC82        E DS                  EXTNAME(TRTC82DS)
009100960530     D DSPT          E DS                  EXTNAME(EDIDSPT)
009200030708     D DSPU          E DS                  EXTNAME(EDIDSPU)
009300960530     D WLBDA8          DS
009400960530     D  G02DAT                 1      8  0
009500960530     D  G02INV                 9     16  0
009600960530     D  G02ERR                17     17
009700960530     D  G02TGI                18     22  0
009800960626     D*
009900960807     D COST1           C                   CONST('IT/BAR/IT1   ')
010000960530     I*---------------------------------------------------------------------*
010100960530     I* FILE
010200960530     I*---------------------------------------------------------------------*
010300960704     IRRIFCSUM
010400960530     I              RRIFCSUM                    SUMDAT
010500960530     C*---------------------------------------------------------------------*
010600960530     C* Ciclo principale
010700960530     C*---------------------------------------------------------------------*
010800961023     C*  Se il numero spedizioni è a zero vado a fine pgm
010900961023     C     D82TSP        IFNE      0
011000960530     C*  Scrivo record testata T00
011100960530     C                   EXSR      WRTT00
011200960530     C*  Scrivo record testata T05
011300960530     C                   EXSR      WRTT05
011400960530     C*  Scrivo record testata T10
011500960530     C                   EXSR      WRTT10
011600960530     C*  Scrivo record testata T15
011700960530     C                   EXSR      WRTT15
011800960530     C*  Scrivo record testata T20
011900960530     C                   EXSR      WRTT20
012000960530     C*  Scrivo record testata T25
012100960530     C                   EXSR      WRTT25
012200960530     C*  Scrivo record testata T30
012300960530     C                   EXSR      WRTT30
012400961023     C                   END
012500960530     C*
012600960530     C                   SETON                                        LR
012700960530     C*--------------------------------------------------------------*
012800960530     C* WRTT00: scirvo tipo record T00 in IFCSUM                    -*
012900960530     C*--------------------------------------------------------------*
013000960530     C     WRTT00        BEGSR
013100960530     C*
013200960530     C                   CLEAR                   EDTT00
013300960530     C*  Mittente: Bartolini  (ITBAR + AS..)
013400960530     C                   MOVEL     'ITBAR'       WMITT             8
013500960829     C* MOMENTANEAMENTE FORZO 005
013600960829     C                   MOVE      '005'         WMITT
013700960829     C*******              MOVE WAS400    WMITT   8
013800960530     C                   MOVEL     WMITT         TA0004
013900960726     C                   MOVEL     'ZZ'          TA0007
014000960530     C*  In base al codice del PARTNER reperisco identificativo
014100960530     C                   Z-ADD     1             XX                3 0
014200960530     C                   CLEAR                   DSPT
014300960530     C     D82CPT        LOOKUP    KSC(XX)                                32
014400960530     C     *IN32         IFEQ      '1'
014500120328      **
014600960530     C                   MOVEL     DPT(XX)       DSPT
014700030709     C                   MOVEL     DPU(XX)       DSPU
014800030714     C********           MOVEL     CPT(XX)       TA0010
014900030714     C                   MOVE      '    '        CPT(XX)
015000030714     C                   MOVEL     CPT(XX)       TA0010
015100960827     C                   MOVEL     §PTMSQ        TAA007
015200960530     C                   END
015300960530     C*  Data/Ora invio messagio: adesso
015400050518     C                   MOVE      WOGGI6        TA0017
015500050518     C                   movel     '20'          TA0017
015600960530     C                   MOVEL     WHHMM         TA0019
015700960530     C*  Codice documento - nome documento
015800960530     C                   MOVEL     '785'         TA1001
015900960712     C                   MOVEL     D82NFV        TA1004
016000960829     C                   MOVEL     '      '      TA1000
016100960530     C                   MOVEL     '9  '         TA1225
016200960530     C*  Scivo record IFCSUM
016300960705     C                   CLEAR                   WDAT
016400960705     C                   MOVEL     EDTT00        WDAT
016500960726     C                   Z-ADD     D82PRG        SUMPRG
016600960705     C                   MOVEL     'TT00'        SUMTPR
016700960705     C                   MOVEL     WDAT          SUMDAT
016800960704     C                   WRITE     RRIFCSUM
016900960530     C*
017000960530     C                   ENDSR
017100960530     C*--------------------------------------------------------------*
017200960530     C* WRTT05: scirvo tipo record T05 in IFCSUM                    -*
017300960530     C*--------------------------------------------------------------*
017400960530     C     WRTT05        BEGSR
017500960530     C*
017600960530     C                   CLEAR                   EDTT05
017700960530     C*
017800960530     C                   ENDSR
017900960530     C*--------------------------------------------------------------*
018000960530     C* WRTT10: scirvo tipo record T10 in IFCSUM                    -*
018100960530     C*--------------------------------------------------------------*
018200960530     C     WRTT10        BEGSR
018300960530     C*
018400960530     C                   CLEAR                   EDTT10
018500960628     C                   Z-ADD     0             X
018600960530     C*  Totale spedizioni
018700960628     C     §PTCN1        IFEQ      'S'
018800960628     C                   MOVEL     '10 '         WCTR
018900960701     C                   MOVEL     '   '         WTPM
019000960701     C                   Z-ADD     D82TSP        WVLU
019100960628     C                   ADD       1             X
019200960703     C                   MOVEL     WTT10         T10(X)
019300960628     C                   END
019400960530     C*  Totale colli
019500960628     C     §PTCN2        IFEQ      'S'
019600960628     C                   MOVEL     '11 '         WCTR
019700960701     C                   Z-ADD     D82TNC        WVLU
019800960701     C                   MOVEL     *BLANKS       WTPM
019900960628     C                   ADD       1             X
020000960703     C                   MOVEL     WTT10         T10(X)
020100960628     C*  Scivo record IFCSUM
020200960628     C     X             IFEQ      2
020300960705     C                   CLEAR                   WDAT
020400960705     C                   MOVEL     EDTT10        WDAT
020500960705     C                   MOVEL     'TT10'        SUMTPR
020600960726     C                   Z-ADD     D82PRG        SUMPRG
020700960705     C                   MOVEL     WDAT          SUMDAT
020800960704     C                   WRITE     RRIFCSUM
020900960628     C                   CLEAR                   EDTT10
021000960705     C                   Z-ADD     0             X
021100960628     C                   END
021200960628     C                   END
021300960628     C*  Totale peso lordo
021400960628     C     §PTCN3        IFEQ      'S'
021500960628     C                   MOVEL     '7  '         WCTR
021600961230     C****                 MOVEL'KGM'     WTPM
021700961230     C                   Z-ADD     D82TPK        WVLU
021800960628     C                   ADD       1             X
021900960703     C                   MOVEL     WTT10         T10(X)
022000960628     C*  Scivo record IFCSUM
022100960726     C     X             IFEQ      2
022200960705     C                   CLEAR                   WDAT
022300960705     C                   MOVEL     EDTT10        WDAT
022400960705     C                   MOVEL     'TT10'        SUMTPR
022500960726     C                   Z-ADD     D82PRG        SUMPRG
022600960705     C                   MOVEL     WDAT          SUMDAT
022700960705     C                   WRITE     RRIFCSUM
022800960628     C                   CLEAR                   EDTT10
022900960705     C                   Z-ADD     0             X
023000960628     C                   END
023100960628     C                   END
023200960628     C*  Totale peso lordo
023300960628     C     §PTCN4        IFEQ      'S'
023400960628     C                   MOVEL     'ZCW'         WCTR
023500961230     C***                  MOVEL'KGM'     WTPM
023600961230     C                   Z-ADD     D82TPK        WVLU
023700960628     C                   ADD       1             X
023800960703     C                   MOVEL     WTT10         T10(X)
023900960628     C*  Scivo record IFCSUM
024000960628     C     X             IFEQ      2
024100960705     C                   CLEAR                   WDAT
024200960705     C                   MOVEL     EDTT10        WDAT
024300960705     C                   MOVEL     'TT10'        SUMTPR
024400960726     C                   Z-ADD     D82PRG        SUMPRG
024500960705     C                   MOVEL     WDAT          SUMDAT
024600960704     C                   WRITE     RRIFCSUM
024700960628     C                   CLEAR                   EDTT10
024800960726     C                   Z-ADD     0             X
024900960628     C                   END
025000960726     C                   END
025100960726     C*
025200960726     C     X             IFGT      0
025300960726     C                   CLEAR                   WDAT
025400960726     C                   MOVEL     EDTT10        WDAT
025500960726     C                   MOVEL     'TT10'        SUMTPR
025600960726     C                   Z-ADD     D82PRG        SUMPRG
025700960726     C                   MOVEL     WDAT          SUMDAT
025800960726     C                   WRITE     RRIFCSUM
025900960726     C                   CLEAR                   EDTT10
026000960726     C                   Z-ADD     0             X
026100960726     C                   END
026200960530     C*
026300960530     C                   ENDSR
026400960530     C*--------------------------------------------------------------*
026500960530     C* WRTT15: scirvo tipo record T15 in IFCSUM                    -*
026600960530     C*--------------------------------------------------------------*
026700960530     C     WRTT15        BEGSR
026800960530     C*
026900960530     C                   CLEAR                   EDTT15
027000960628     C*  Scivo data e numero CMR
027100960827     C     §PTCM2        IFNE      'N'
027200960626     C                   MOVEL     'CMR'         TD1153
027300960626     C                   MOVEL     D82CMR        TD1154                         Numero
027400960530     C                   MOVEL     '137'         TD2005
027500960701     C                   MOVEL     D82DCM        TD2380                         Data
027600960705     C                   MOVEL     '102'         TD2379
027700960530     C*  Scivo record IFCSUM
027800960705     C                   CLEAR                   WDAT
027900960705     C                   MOVEL     EDTT15        WDAT
028000960705     C                   MOVEL     'TT15'        SUMTPR
028100960726     C                   Z-ADD     D82PRG        SUMPRG
028200960705     C                   MOVEL     WDAT          SUMDAT
028300960704     C                   WRITE     RRIFCSUM
028400960628     C                   END
028500960628     C*  Scivo data e numero foglio viaggio
028600960827     C     §PTNF2        IFEQ      'S'
028700960829     C                   CLEAR                   EDTT15
028800960705     C                   MOVEL     'AFB'         TD1153
028900960628     C                   MOVEL     D82NFV        TD1154                         Numero
029000960628     C                   MOVEL     '137'         TD2005
029100960628     C                   MOVEL     D82DFV        TD2380                         Data
029200960705     C                   MOVEL     '102'         TD2379
029300960628     C*  Scivo record IFCSUM
029400960705     C                   CLEAR                   WDAT
029500960705     C                   MOVEL     EDTT15        WDAT
029600960705     C                   MOVEL     'TT15'        SUMTPR
029700960726     C                   Z-ADD     D82PRG        SUMPRG
029800960705     C                   MOVEL     WDAT          SUMDAT
029900960704     C                   WRITE     RRIFCSUM
030000960628     C                   END
030100960530     C*
030200960530     C                   ENDSR
030300960530     C*--------------------------------------------------------------*
030400960530     C* WRTT20: scirvo tipo record T20 in IFCSUM                    -*
030500960530     C*--------------------------------------------------------------*
030600960530     C     WRTT20        BEGSR
030700960530     C*
030800960530     C                   CLEAR                   EDTT20
030900960530     C*  Dati traporto
031000960530     C                   MOVEL     '20 '         TE8051
031100960530     C                   MOVEL     '3  '         TE8067
031200960628     C*  Controllo se devo scrivere punto di partenza
031300960628     C                   Z-ADD     0             X
031400960702     C     §PTLC1        IFEQ      'S'
031500960628     C                   MOVEL     '5  '         WTPL
031600960628     C                   MOVEL     'MILANO'      WLOC
031700960628     C                   ADD       1             X
031800960628     C                   MOVEL     WTT20         T20(X)
031900960628     C                   END
032000960628     C*  Controllo se devo scrivere punto di arrivo
032100960702     C     §PTLC2        IFEQ      'S'
032200960628     C                   MOVEL     '8  '         WTPL
032300960628     C                   MOVEL     §PTLOC        WLOC
032400960628     C                   ADD       1             X
032500960628     C                   MOVEL     WTT20         T20(X)
032600960628     C                   END
032700030708      *******
032800960628     C*  Controllo se devo scrivere data partenza
032900960628     C     §PTDT1        IFEQ      'S'
033000960628     C                   MOVEL     '136'         WTPD
033100960628     C                   MOVEL     '102'         WFOR
033200960628     C                   MOVEL     D82DFV        WDTA
033300030708     C                   Z-ADD     1             X
033400960705     C                   MOVEL     WTT20D        T20D(X)
033500030708      * su richiesta Euroexpress (Marlène e Villa Luglio 2003)
033600030708      *  impostato su PU i giorni di viaggio per arrivare dal Partner
033700030708      *   quando sono (gg.1) il trasporto arriva il giorno stesso.
033800030708      *   quando sono (gg.2) il trasporto arriva il giorno seguente lavorativo. etc. etc.
033900030708      *
034000030708      * Data arrivo Presunto al Partner - data stimata di Arrivo
034100030708     C                   MOVEL     '132'         WTPD
034200030708     C                   MOVEL     '102'         WFOR
034300030708      * il giorno stesso
034400030710     c                   if        §puGGV = 0
034500030708      *
034600030708     C                   MOVEL     D82DFV        WDTA
034700030708      *
034800030708     c                   else
034900030708      *
035000030708     C                   MOVE      D82DFV        dataisoP
035100030708     c     dataisoP      adddur    §puGGV:*d     dataisoA
035200030708     C                   MOVE      dataisoA      dataAMG
035300030709      *
035400030709      * controllo che il giorno di arrivo non sia un sabato o una domenica
035500030709     c     ri_Controlla  tag
035600030709     C                   movel     DataAMG       AnMeGi
035700030709     c                   clear                   clntfp
035800030709     c                   z-add     §PTlnp        clntfa
035900030709     c                   z-add     kanno         clnann
036000030709     c                   z-add     kmese         clnmes
036100030709     c     kcln          chain     azcln01l
036200030709     c                   if        %found(azcln01l)
036300030709     c                   movea     clnmat        gio
036400030709     c                   movea     clnpom        fes
036500030709      *  se sabato o domenica aggiunge un giorno e riprova
036600030709     c                   if        gio(gg) = '6' or gio(gg) = '7' or
036700030709     c                             fes(gg) = 'F'
036800030709     c     dataisoA      adddur    1:*d          dataisoA
036900030709     C                   MOVE      dataisoA      dataAMG
037000030709     c                   goto      ri_Controlla
037100030709     c                   end
037200030709     c                   endIF
037300030709      *
037400030708     C                   MOVEL     dataAMG       WDTA
037500030708      *
037600030708     c                   end
037700030708      *
037800030708     C                   ADD       1             X
037900030708     C                   MOVEL     WTT20D        T20D(X)
038000960628     C                   END
038100960530     C*  Scivo record IFCSUM
038200960705     C                   CLEAR                   WDAT
038300960705     C                   MOVEL     EDTT20        WDAT
038400960726     C                   Z-ADD     D82PRG        SUMPRG
038500960705     C                   MOVEL     'TT20'        SUMTPR
038600960705     C                   MOVEL     WDAT          SUMDAT
038700960704     C                   WRITE     RRIFCSUM
038800960530     C*
038900960530     C                   ENDSR
039000960530     C*--------------------------------------------------------------*
039100960530     C* WRTT25: scirvo tipo record T25 in IFCSUM                    -*
039200960530     C*--------------------------------------------------------------*
039300960530     C     WRTT25        BEGSR
039400960530     C*
039500960530     C                   CLEAR                   EDTT25
039600960530     C*  Ship from:
039700960530     C                   MOVEL     'SF '         TF3035
039800960626     C                   MOVEL     COST1         TF3039
039900960530     C*  Scivo record IFCSUM
040000960705     C                   CLEAR                   WDAT
040100960705     C                   MOVEL     EDTT25        WDAT
040200960726     C                   Z-ADD     D82PRG        SUMPRG
040300960705     C                   MOVEL     'TT25'        SUMTPR
040400960705     C                   MOVEL     WDAT          SUMDAT
040500960704     C                   WRITE     RRIFCSUM
040600960530     C*  Ship to:
040700960530     C                   CLEAR                   EDTT25
040800960530     C                   MOVEL     'ST '         TF3035
040900960530     C                   MOVEL     §PTPLT        TF3039
041000960530     C*  Scivo record IFCSUM
041100960705     C                   CLEAR                   WDAT
041200960705     C                   MOVEL     EDTT25        WDAT
041300960726     C                   Z-ADD     D82PRG        SUMPRG
041400960705     C                   MOVEL     'TT25'        SUMTPR
041500960705     C                   MOVEL     WDAT          SUMDAT
041600960704     C                   WRITE     RRIFCSUM
041700960530     C*
041800960530     C                   ENDSR
041900960530     C*--------------------------------------------------------------*
042000960530     C* WRTT30: scirvo tipo record T30 in IFCSUM                    -*
042100960530     C*--------------------------------------------------------------*
042200960530     C     WRTT30        BEGSR
042300960530     C*
042400960530     C                   CLEAR                   EDTT30
042500960726     C     §PTEQD        IFEQ      'S'
042600960712     C                   MOVEL     'TE '         TG8053
042700960712     C                   MOVEL     D82TRM        TG8260
042800960712     C*  Scivo record IFCSUM
042900960712     C                   CLEAR                   WDAT
043000960712     C                   MOVEL     EDTT30        WDAT
043100960726     C                   Z-ADD     D82PRG        SUMPRG
043200960712     C                   MOVEL     'TT30'        SUMTPR
043300960712     C                   MOVEL     WDAT          SUMDAT
043400960712     C                   WRITE     RRIFCSUM
043500960726     C                   END
043600960530     C*
043700960530     C                   ENDSR
043800960530     C*----------------------------------------------------------------
043900960530     C*  *INZSR - OPERAZIONI INIZIALI
044000960530     C*----------------------------------------------------------------
044100960530     C     *INZSR        BEGSR
044200030709      *
044300030709     c     kcln          klist
044400030709     c                   kfld                    clntfp
044500030709     c                   kfld                    clntfa
044600030709     c                   kfld                    clnann
044700030709     c                   kfld                    clnmes
044800960530     C*
044900960530     C     *ENTRY        PLIST
045000960530     C                   PARM                    KPJBA
045100960704     C                   MOVEL     KPJBU         TRTC82
045200960530     C* Recupero codice AS
045300960530     C                   Z-ADD     1             CODUT             1 0
045400960704     C                   CALL      'X§PARUT'
045500960530     C                   PARM                    UTEDSE
045600960530     C                   MOVEL     REC80         CNCR80
045700960530     C                   MOVEL     SIMFEL        WAS400            3
045800960530     C* Recupero data e ora x messaggio
045900960530     C                   TIME                    WHHDAT           14 0
046000960530     C                   MOVE      WHHDAT        G02DAT
046100960530     C                   MOVE      WHHDAT        DATA              6 0
046200960530     C                   MOVEL     *BLANK        G02ERR
046300960530     C                   CALL      'XSRDA8'
046400960530     C                   PARM                    WLBDA8
046500960530     C                   MOVEL     WHHDAT        WHHMM             4 0
046600960530     C                   MOVE      G02INV        WOGGI6            6 0
046700960530     C                   MOVE      WHHDAT        WANNO             2 0
046800960530     C                   MOVEL     WANNO         WOGGI6
046900030709      *
047000030709      *controllo se udate è festivo non lancio l'elaborazione
047100030709      * ed esce subito
047200960530     C* Caricamento Tabella Partner esteri
047300960530     C                   Z-ADD     0             X                 3 0
047400960530     C                   MOVEL     'PT'          TABCOD
047500960530     C     TABCOD        CHAIN     EDTAB01L                           30
047600960530     C     *IN30         DOWEQ     '0'
047700960530     C     TABFLG        IFEQ      *BLANKS
047800960530     C                   ADD       1             X
047900960530     C                   MOVEL     TABKEY        CPT(X)
048000030714     C******             MOVE      '    '        CPT(X)
048100960530     C                   MOVEL     TABUNI        DPT(X)
048200960530     C                   MOVEL     TABUNI        KSC(X)
048300960530     C                   END
048400960530     C     TABCOD        READE     EDTAB01L                               30
048500960530     C                   END
048600121105      *
048700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
048800121105     c                   exsr      ChecK_PT
048900030708      *------------
049000030708     C* Caricamento Tabella Partner esteri aggiuntiva
049100030708     C                   MOVEL     'PU'          TABCOD
049200030708     C     TABCOD        CHAIN     EDTAB01L                           30
049300030708     C     *IN30         DOWEQ     '0'
049400030708     C     TABFLG        IFEQ      *BLANKS
049500030708     C                   MOVEL     TABKEY        CODPU            35
049600030708     C                   Z-ADD     1             XU                3 0
049700030708     C     CODPU         LOOKUP    CPT(XU)                                32
049800030708     C   32              MOVEL     TABUNI        DPU(XU)
049900030708     C                   END
050000030708     C     TABCOD        READE     EDTAB01L                               30
050100030708     C                   END
050200030708      *------------
050300960530     C* finta read x IFCSUM
050400960530     C   01
050500960530     CANN01              READ      EDIFCSUM                               01
050600960530     C*
050700960530     C                   ENDSR
050800121105      *---------------------------------------------------------------*
050900121105      * CTRL caricamento schiere    PT                               -*
051000121105      *---------------------------------------------------------------*
051100121105     C     Check_PT      begsr
051200121105     C*
051300121105     c* Controllo riempimento schiera
051400121105     c                   clear                   trul0sds
051500121105     c                   eval      i0sski='CPT'
051600121105     c                   eval      i0sele=%elem(CPT)
051700121105     c                   eval      i0spie=x
051800121105     c                   eval      i0sfile='EDTAB00F'
051900121105     c                   eval      i0ssif=knsif
052000121105     c                   eval      i0spgm='TRTC83R3'
052100121112     c                   movel     kpjbu         kpjbusav
052200121112     c                   clear                   kpjbu
052300121105     c                   movel     trul0sds      kpjbu
052400121105     c                   call      'TRUL0SR'
052500121105     c                   parm                    kpjba
052600121112     c                   movel     kpjbusav      kpjbu
052700121105     C*
052800121105     C                   ENDSR
052900121105     O*---------------------------------------------------------*
