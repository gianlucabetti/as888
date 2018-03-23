000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200071227      * FNLSD2R *-----------------------------------------------------*
000300071227      *           ELENCO SEGNACOLLI NON SPUNTATI                      *
000400071227      *---------------------------------------------------------------*
000500000217     FFNLSD2D   CF   E             WORKSTN
000600021203     FFIAPD01L  IF   E           K DISK
000700071227      *
000800071227      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
000900071227      *
001000080110     d $Msg            s             78    dim(11)  ctdata  perrcd(1)
001100071227     d L1              s              3  0 dim(30)
001200071227     d L6S             s              3  0 dim(30)
001300071227     d COM             s              4    dim(5)
001400071227      *
001500071227      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
001600071227      *
001700071227     d Status         sds
001800071227     d  SDSpgm           *proc
001900071227      *
002000071227      * - PASSAGGIO DATI ALL'ELABORAZIONE BATCH           - FNLSD3R -
002100071227     d KPJBA         e ds
002200071227     d FNLSD3ds      e ds                  inz
002300071227      *
002400071227      * - Parametri x Controllo profilo utenti
002500071227     d TIBS34ds      e ds
002600071227      * - Ds di riferimento al file esterno AZUTE00F
002700071227     d AZUTEds       e ds                       extname(AZUTE00F)
002800071227      * - Ds per dati organigramma
002900071227     d dDatiUte      e ds
003000071227      *
003100071227      * - DS per TRUL06R - Caricamento £X
003200071227     d TRUL06ds      e ds                  inz
003300071227     d  Lin                    1     90  0 inz  dim(30)
003400071227     d
003500071227      *
003600071227      * - Ds per decodifica cliente
003700071227     d TIBS69ds      e ds                  inz
003800071227     d ds_CNACO      e ds                  inz       extname(CNACO00F)
003900071227     d ds_CNIND      e ds                  inz       extname(CNIND00F)
004000071227     d ds_CNCLP      e ds                  inz       extname(CNCLP00F)
004100071227     d ds_FNCLS      e ds                  inz       extname(FNCLS00F)
004200071227      *
004300071227      * - Gestione anagrafica autotrasportatori/cooperative
004400071227     d FNLV24ds      e ds                  inz
004500071227      *
004600071227      * - Parametri per pgm. controllo/inversione data
004700071227     d WLBdat          ds                  inz
004800071227     d  G02dat                 1      8  0 inz
004900071227     d  G02inv                 9     16  0 inz
005000071227     d  G02err                17     17    inz
005100071227     d  G02tgi                18     22  0 inz
005200071227      *
005300071227      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
005400071227      *
005500071227     D kpdpdr          S                   LIKE(APDpdr) inz
005600071227     D kpdtip          S                   LIKE(APDtip) inz
005700071227     d Wdvd            s                   like(DD3dvd) inz
005800071227     d Wdva            s                   like(DD3dva) inz
005900071227      *
006000071227     d xx              s              3  0 inz
006100071227      *
006200071227      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
006300071227      *
006400071227     C* ACCESSO CNACO00F
006500071227     C     Kfapd         klist
006600071227     C                   kfld                    kpdtip
006700071227     C                   kfld                    kpdpdr
006800071227
006900960321     C*****************************************************************
007000960321     C* RIEPILOGO INDICATORI
007100960321     C*****************************************************************
007200960321     C* 05    - SONO UN "REMXXX"
007300960321     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
007400960321     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
007500960327     C* 30/31 - DI COMODO
007600961015     C* 40/41 - ERRORI
007700961015     C* 43/45 - ERRORI
007800960321     C* 90    - INDICATORE DI ERRORE GENERICO
007900960321     C*****************************************************************
008000071227
008100000000     C     *ENTRY        PLIST
008200000000     C                   PARM                    KPJBA
008300900511     C*---------------------------------------------------------------*
008400960321     C* PULIZIA CAMPI VIDEATA
008500071227     c                   clear                   LSD2D01
008600960321     C*
008700900509     C     FOR01         TAG
008800071227     c                   if        not *in90
008900071227     c                   write     LSD2T01
009000071227     c                   endif
009100000217     C                   EXFMT     LSD2D01
009200960321     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
009300071227     C                   CLEAR                   V1DMSG
009400071227     C                   SETOFF                                       28  90
009500941223     C*
009600960321     C* CMD3 - FINE LAVORO
009700900509     C   KC              GOTO      FINE
009800960321     C*
009900960321     C* CONTROLLI FORMATO
010000960321     C                   EXSR      CTRD01
010100941223     C  NKF
010200900524     COR 90              GOTO      FOR01
010300960321     C*
010400000217     C* F6 - ELABORAZIONE
010500071227     C                   CLEAR                   FNLSD3ds
010600010522     C                   Z-ADD     V1CPOP        DD3PO
010700000217     C                   Z-ADD     V1CKSC        DD3KSC
010800000217     C                   Z-ADD     WDVD          DD3DVD
010900000217     C                   Z-ADD     WDVA          DD3DVA
011000010528     C                   MOVEL     V1CBRK        DD3BRK
011100071227     c                   eval      DD3sst = V1Csst
011200000224     C                   MOVE      V1CPDR        COM(1)
011300000224     C                   MOVE      V1CPD2        COM(2)
011400000224     C                   MOVE      V1CPD3        COM(3)
011500000224     C                   MOVE      V1CPD4        COM(4)
011600000224     C                   MOVE      V1CPD5        COM(5)
011700000224     C                   SORTA     COM
011800000224     C*
011900071227     C                   DO        5             xx
012000071227     C     COM(xx)       IFGT      *ZEROS
012100000225     C                   SELECT
012200000225     C     DD3PDR        WHENEQ    0
012300071227     C                   MOVE      COM(xx)       DD3PDR
012400010522     C                   MOVEL     DD3PO         DD3PDR
012500000225     C     DD3PD2        WHENEQ    0
012600071227     C                   MOVE      COM(xx)       DD3PD2
012700010522     C                   MOVEL     DD3PO         DD3PD2
012800000225     C     DD3PD3        WHENEQ    0
012900071227     C                   MOVE      COM(xx)       DD3PD3
013000010522     C                   MOVEL     DD3PO         DD3PD3
013100000225     C     DD3PD4        WHENEQ    0
013200071227     C                   MOVE      COM(xx)       DD3PD4
013300010522     C                   MOVEL     DD3PO         DD3PD4
013400000225     C     DD3PD5        WHENEQ    0
013500071227     C                   MOVE      COM(xx)       DD3PD5
013600010522     C                   MOVEL     DD3PO         DD3PD5
013700000225     C                   ENDSL
013800000224     C*
013900000224     C                   ENDIF
014000000224     C                   ENDDO
014100960321     C*
014200960327     C                   CLEAR                   KPJBU
014300071227     C                   MOVEL     FNLSD3ds      KPJBU
014400000217     C                   MOVEL     'LSD3'        KCOAZ
014500071227     c                   if        KNMUS = *all'1'
014600071228     c                   call      'FNLSD3R'
014700071227     c                   parm                    KPJBA
014800071227     c                   else
014900900523     C                   CALL      'BCH10'
015000900523     C                   PARM                    KPJBA
015100071227     c                   endif
015200960401     C*
015300920925     C     FINE          TAG
015400000000     C                   SETON                                        LR
015500960321     C*
015600960321     C*--- CONTROLLI FORMATO -----------------------------------------*
015700960321     C     CTRD01        BEGSR
015800071227     C                   movea     *zeros        *in(40)
015900071227     C*
016000071227     C**** FIL. DI PARTENZA ****
016100071227     C     V1CPOP        IFGT      *ZEROS
016200071227     C     V1CPOP        LOOKUP    L1                                     31
016300071227     C     *IN31         IFEQ      *OFF
016400071227     C                   MOVEL     $MSG(1)       V1DMSG
016500071227     C                   SETON                                        452890
016600071227     C                   GOTO      ENDCTR
016700071227     C                   ENDIF
016800071227     C* NON DEVE ESSERE UNA FIL. GESTITO DA ALTRA FIL.
016900071227     C     V1CPOP        LOOKUP    L6S                                    31
017000071227     C     *IN31         IFEQ      *ON
017100071227     C                   MOVEL     $MSG(8)       V1DMSG
017200071227     C                   SETON                                        452890
017300071227     C                   GOTO      ENDCTR
017400071227     C                   ENDIF
017500071227     C                   ENDIF
017600071227     C*
017700071227     C**** COD.AUTOTRASPORTATORE ****
017800071227     C                   CLEAR                   V1DPDR
017900071227     C     '?'           SCAN      V1CPDR                                 90
018000071227     C     *IN90         IFEQ      *ON
018100071227      *
018200071227      * imposto i campi della ds esterna fnlv24ds
018300071227     C                   clear                   fnlv24ds
018400071227     c                   if        V1Cpop   > *zeros
018500071227     C                   eval      d24fil   = v1cpop
018600071227     c                   else
018700071227     c                   eval      D24fil   = DUTpou
018800071227     c                   endif
018900071227     C                   eval      d24tip   = 'A'
019000071227     C                   eval      d24flg   = 'R'
019100071227     C                   movel     fnlv24ds      kpjbu
019200071227     C                   CALL      'FNLV24R'
019300071227     C                   PARM                    KPJBA
019400071227      *
019500071227      * valorizzo i campi a video
019600071227     C                   movel     kpjbu         fnlv24ds
019700071227     C                   if        d24pdr   <> *zeros
019800071227     C                   move      d24pdr        v1cpdr
019900071227     C                   movel     d24pdr        v1cpop
020000071227     C                   movel     d24rsc        v1dpdr
020100071227     C                   endif
020200071227     C                   SETON                                        42
020300071227     C                   GOTO      ENDCTR
020400071227     C                   ENDIF
020500071227     C*
020600071227     C     V1CPDR        IFNE      *ZEROS
020700071227     C     V1CPDR        ANDNE     *BLANKS
020800071227     C                   TESTN                   V1CPDR               30
020900071227     C     *IN30         IFEQ      *OFF
021000071227     C                   MOVEL     $MSG(3)       V1DMSG
021100071227     C                   SETON                                        422890
021200071227     C                   GOTO      ENDCTR
021300071227     C                   ENDIF
021400071227     C                   ENDIF
021500071227     C* ERRORE SE IMMESSO AUTOTRASPORTATORE MA NON P.O. DI PARTENZA
021600071227     C     V1CPDR        IFGT      *ZEROS
021700071227     C     V1CPOP        ANDEQ     *ZEROS
021800071227     C                   MOVEL     $MSG(7)       V1DMSG
021900071227     C                   SETON                                        452890
022000071227     C                   GOTO      ENDCTR
022100071227     C                   ENDIF
022200071227     C*
022300071227     C     V1CPDR        IFGT      *ZEROS
022400071227     C                   MOVEL     V1CPOP        W0070
022500071227     C                   MOVE      V1CPDR        W0070
022600071227     C*    W0070         CHAIN     FNAPD01L                           30
022700071227     C                   move      w0070         kpdpdr
022800071227     C                   move      'A'           kpdtip
022900071227     C     kfapd         chain     fiapd000                           30
023000071227     C     *IN30         IFEQ      *ON
023100071227     C                   MOVEL     $MSG(3)       V1DMSG
023200071227     C                   SETON                                        422890
023300071227     C                   GOTO      ENDCTR
023400071227     C                   ENDIF
023500071227     C                   MOVEL     APDRSC        V1DPDR
023600071227     C                   ENDIF
023700071227     C* CODICE 2
023800071227     C                   CLEAR                   V1DPD2
023900071227     C                   CLEAR                   WPDR
024000071227     C                   MOVE      V1CPD2        WPDR
024100071227     C                   EXSR      CTRPDR
024200071227     C                   MOVE      WPDR          V1CPD2
024300071227     C                   MOVEL     WDPDR         V1DPD2
024400071227     C     WELAB         IFEQ      '?'
024500071227     C                   SETON                                        46
024600071227     C                   GOTO      ENDCTR
024700071227     C                   ENDIF
024800071227     C     WELAB         IFEQ      'E'
024900071227     C                   SETON                                        462890
025000071227     C                   GOTO      ENDCTR
025100071227     C                   ENDIF
025200071227     C* CODICE 3
025300071227     C                   CLEAR                   V1DPD3
025400071227     C                   CLEAR                   WPDR
025500071227     C                   MOVE      V1CPD3        WPDR
025600071227     C                   EXSR      CTRPDR
025700071227     C                   MOVE      WPDR          V1CPD3
025800071227     C                   MOVEL     WDPDR         V1DPD3
025900071227     C     WELAB         IFEQ      '?'
026000071227     C                   SETON                                        47
026100071227     C                   GOTO      ENDCTR
026200071227     C                   ENDIF
026300071227     C     WELAB         IFEQ      'E'
026400071227     C                   SETON                                        472890
026500071227     C                   GOTO      ENDCTR
026600071227     C                   ENDIF
026700071227     C* CODICE 4
026800071227     C                   CLEAR                   V1DPD4
026900071227     C                   CLEAR                   WPDR
027000071227     C                   MOVE      V1CPD4        WPDR
027100071227     C                   EXSR      CTRPDR
027200071227     C                   MOVE      WPDR          V1CPD4
027300071227     C                   MOVEL     WDPDR         V1DPD4
027400071227     C     WELAB         IFEQ      '?'
027500071227     C                   SETON                                        48
027600071227     C                   GOTO      ENDCTR
027700071227     C                   ENDIF
027800071227     C     WELAB         IFEQ      'E'
027900071227     C                   SETON                                        482890
028000071227     C                   GOTO      ENDCTR
028100071227     C                   ENDIF
028200071227     C* CODICE 5
028300071227     C                   CLEAR                   V1DPD5
028400071227     C                   CLEAR                   WPDR
028500071227     C                   MOVE      V1CPD5        WPDR
028600071227     C                   EXSR      CTRPDR
028700071227     C                   MOVE      WPDR          V1CPD5
028800071227     C                   MOVEL     WDPDR         V1DPD5
028900071227     C     WELAB         IFEQ      '?'
029000071227     C                   SETON                                        49
029100071227     C                   GOTO      ENDCTR
029200071227     C                   ENDIF
029300071227     C     WELAB         IFEQ      'E'
029400071227     C                   SETON                                        492890
029500071227     C                   GOTO      ENDCTR
029600071227     C                   ENDIF
029700960321     C*
029800960321     C****  CODICE CLIENTE  ****
029900960321    1C     V1CKSC        IFGT      0
030000960321     C*
030100960321     C* CONTROLLO VALIDITA' CODICE CLIENTE
030200071227     c                   clear                   TIBS69ds
030300071227     c                   eval      I69kac    = V1Cksc
030400071227     c                   call      'TIBS69R'
030500071227     c                   parm                    TIBS69ds
030600071227     c                   parm                    ds_CNACO
030700071227     c                   parm                    ds_CNIND
030800071227     c                   parm                    ds_CNCLP
030900071227     c                   parm                    ds_FNCLS
031000960321     C*
031100961016     C* SE NON ESISTE NEL P.d.C. ---> ERRORE
031200071227    2c                   if        O69err = *on
031300071227     C                   MOVEL     $MSG(2)       V1DMSG
031400961016     C                   SETON                                        412890
031500961016     C                   GOTO      ENDCTR
031600960327   X2C                   ELSE
031700960327     C* DECODIFICA CODICE CLIENTE
031800960327     C                   MOVEL     ACORAG        V1DKSC
031900960321    2C                   ENDIF
032000960321     C*
032100960321   X1C                   ELSE
032200000217     C* SE CODICE CLIENTE A ZERO MA MESSA RAGIONE SOCIALE -->
032300000217     C*  RIERCA ALFABETICA
032400960321     C*
032500960321    2C     V1DKSC        IFNE      *BLANKS
032600080129     C* PARSTA = 9 --> ESCLUDO ANNULLATI
032700000217     C                   CALL      'XALFA3BR'
032800080129     C                   PARM      RSut          PARDUT           30
032900080129     C                   PARM      1             PARCUT            1 0
033000080129     C                   PARM      V1Dksc        PARRAG           48
033100080129     C                   PARM      DUTkci        PARKCI            4 0
033200080129     C                   PARM      9             PARSTA            1 0
033300980702     C                   PARM                    PARFLR           90
033400080129     C                   PARM      *blanks       PARDIT            3
033500000217     C                   PARM                    PARNUM            2 0
033600000217     C                   PARM                    PARKCM           80
033700000217     C                   PARM                    PARKSM          140
033800000217     C                   PARM                    PARKDM           60
033900960321     C     PARSTA        IFNE      -1
034000000217     C                   MOVEL     PARKSM        W0070             7 0
034100000217     C                   Z-ADD     W0070         V1CKSC
034200960321     C                   ENDIF
034300960321     C*
034400960321     C                   SETON                                        90
034500960321     C                   GOTO      ENDCTR
034600960321    2C                   ENDIF
034700960321    1C                   ENDIF
034800960321     C*
034900000217     C****  DATA SPEDIZIONE DAL  ****
035000961015    1C     V1CDVD        IFGT      0
035100960321     C                   MOVE      V1CDVD        G02DAT
035200960321     C                   MOVEL     *ZERO         G02INV
035300960321     C                   MOVEL     *BLANKS       G02ERR
035400941223     C                   CALL      'XSRDA8'
035500920925     C                   PARM                    WLBDAT
035600920925     C     G02ERR        IFEQ      '1'
035700071227     C                   MOVEL     $MSG(4)       V1DMSG
035800960321     C                   SETON                                        432890
035900920925     C                   GOTO      ENDCTR
036000960321     C                   ENDIF
036100960321     C                   Z-ADD     G02DAT        V1CDVD
036200960321     C                   Z-ADD     G02INV        WDVD
036300900524     C*
036400000217     C****  DATA SPEDIZIONE  AL  ****
036500960326    2C     V1CDVA        IFNE      0
036600960321     C                   MOVE      V1CDVA        G02DAT
036700960321     C                   MOVEL     *ZERO         G02INV
036800960321     C                   MOVEL     *BLANKS       G02ERR
036900941223     C                   CALL      'XSRDA8'
037000920925     C                   PARM                    WLBDAT
037100920925     C     G02ERR        IFEQ      '1'
037200071227     C                   MOVEL     $MSG(4)       V1DMSG
037300960321     C                   SETON                                        442890
037400920925     C                   GOTO      ENDCTR
037500960321     C                   ENDIF
037600960321     C                   Z-ADD     G02DAT        V1CDVA
037700960321     C                   Z-ADD     G02INV        WDVA
037800941223     C*
037900900524     C* DATA DAL NO MAGGIORE DATA AL
038000960326    3C     WDVD          IFGT      WDVA
038100071227     C                   MOVEL     $MSG(5)       V1DMSG
038200960321     C                   SETON                                        432890
038300900524     C                   GOTO      ENDCTR
038400960326    3C                   ENDIF
038500960321     C*
038600960321     C* SE INSERITA DATA AL OCCORRE INSERIRE ANCHE DATA DAL
038700960326    3C     WDVD          IFEQ      0
038800071227     C                   MOVEL     $MSG(4)       V1DMSG
038900960321     C                   SETON                                        432890
039000960321     C                   GOTO      ENDCTR
039100960326    3C                   ENDIF
039200960326    2C                   ENDIF
039300961025    1C                   ENDIF
039400080110      *
039500080110      * "Stampa solo totali" inibisce parzializzazione per autotrasp.
039600080110     c                   if             V1Csst = 'S'
039700080110     c                             and (V1Cpdr > *zeros
039800080110     c                              or  V1Cpd2 > *zeros
039900080110     c                              or  V1Cpd3 > *zeros
040000080110     c                              or  V1Cpd4 > *zeros
040100080110     c                              or  V1Cpd5 > *zeros)
040200080110     c                   movel     $Msg(9)       V1Dmsg
040300080110     c                   seton                                        502890
040400080110     c                   leavesr
040500080110     c                   endif
040600080110      *
040700080110      * "Stampa solo totali" inibisce selezione del cod. cliente
040800080110     c                   if             V1Csst = 'S'
040900080110     c                             and  V1Cksc > *zeros
041000080110     c                   movel     $Msg(10)      V1Dmsg
041100080110     c                   seton                                        502890
041200080110     c                   leavesr
041300080110     c                   endif
041400071227      *
041500071227      * "Stampa solo totali" inibisce "Stampa separata per filiale"
041600071227     c                   if             V1Csst = 'S'
041700071227     c                             and  V1Cbrk = 'S'
041800080110     c                   movel     $Msg(11)      V1Dmsg
041900071227     c                   seton                                        502890
042000071227     c                   leavesr
042100071227     c                   endif
042200960321     C*
042300000224     C     ENDCTR        ENDSR
042400000224     C*
042500000224     C* CONTROLLO COD.AUTOTRASPORTATORE ------------------------------*
042600000224     C     CTRPDR        BEGSR
042700000224     C                   CLEAR                   WELAB
042800000224     C                   CLEAR                   WDPDR
042900000224     C* COD.AUTOTRASPORTATORE
043000000224     C     '?'           SCAN      WPDR                                   90
043100000224     C*
043200000224     C     *IN90         IFEQ      *ON
043300021203      *
043400021203      * imposto i campi della ds esterna fnlv24ds
043500021203     C                   clear                   fnlv24ds
043600021203     C                   eval      d24fil   = v1cpop
043700021203     C                   eval      d24tip   = 'A'
043800021203     C                   eval      d24flg   = 'R'
043900021203     C                   movel     fnlv24ds      kpjbu
044000021203
044100000224     C                   CALL      'FNLV24R'
044200000224     C                   PARM                    KPJBA
044300000224     C                   MOVEL     '?'           WELAB
044400021203      *
044500021203      * valorizzo i campi a video
044600021203     C                   movel     kpjbu         fnlv24ds
044700021203     C                   if        d24pdr   <> *zeros
044800021203     C                   move      d24pdr        wpdr              4
044900021203     C                   movel     d24pdr        v1cpop
045000021203     C                   movel     d24rsc        wdpdr            20
045100021203     C                   endif
045200000224     C                   GOTO      ENDPDR
045300000224     C                   ENDIF
045400000224     C*
045500000224     C     WPDR          IFNE      *ZEROS
045600000224     C     WPDR          ANDNE     *BLANKS
045700000224     C                   TESTN                   WPDR                 30
045800000224     C     *IN30         IFEQ      *OFF
045900071227     C                   MOVEL     $MSG(3)       V1DMSG
046000000224     C                   MOVEL     'E'           WELAB             1
046100000224     C                   GOTO      ENDPDR
046200000224     C                   ENDIF
046300000224     C                   ENDIF
046400010522     C* ERRORE SE IMMESSO COD.AUTOTRASPORTATORE SENZA P.O. DI PARTENZA
046500010522     C     WPDR          IFGT      *ZEROS
046600010522     C     V1CPOP        ANDEQ     *ZEROS
046700071227     C                   MOVEL     $MSG(7)       V1DMSG
046800010522     C                   MOVEL     'E'           WELAB
046900010522     C                   SETON                                        45
047000010522     C                   GOTO      ENDPDR
047100010522     C                   ENDIF
047200000224     C*
047300000224     C     WPDR          IFGT      *ZEROS
047400000224     C                   MOVE      WPDR          W0070
047500010522     C                   MOVEL     V1CPOP        W0070
047600021203     C*    W0070         CHAIN     FNAPD01L                           30
047700021205     C                   move      w0070         kpdpdr
047800021203     C                   move      'A'           kpdtip
047900071227     C     kfapd         chain     fiapd000                           30
048000000224     C     *IN30         IFEQ      *ON
048100071227     C                   MOVEL     $MSG(3)       V1DMSG
048200000224     C                   MOVEL     'E'           WELAB             1
048300000224     C                   GOTO      ENDPDR
048400000224     C                   ENDIF
048500000224     C                   MOVEL     APDRSC        WDPDR
048600000224     C                   ENDIF
048700000224     C*
048800000224     C     ENDPDR        ENDSR
048900920925     C*
049000960321     C*--- SR INIZIALE -----------------------------------------------*
049100960321     C     *INZSR        BEGSR
049200071227      *
049300071227      * Reperimento dati job
049400071227     c     *dtaara       define    §azute        AZUTEds
049500071227     c     *dtaara       define    §datiute      dDATIUTE
049600071227      *
049700071227     c                   in(E)     *dtaara
049800071227     c                   IF        %ERROR or RSUT = *blanks
049900071227     c                   clear                   Tibs34Ds
050000071227     c                   call      'TIBS34R'
050100071227     c                   parm                    Tibs34Ds
050200071227     c                   in        *dtaara
050300071227     c                   ENDIF
050400071227      *
050500071227     c                   eval      V1Cpgm = SDSpgm
050600960321     C*
050700000217     C***
050800000217     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
050900000217     C***
051000071227     C                   CLEAR                   TRUL06ds
051100000217     C                   MOVE      '£1'          D06COD
051200000217     C                   MOVEL     SIMFEL        D06KEY
051300071227     C                   MOVEL     TRUL06ds      KPJBU
051400000217     C*
051500000217     C                   CALL      'TRUL06R'
051600000217     C                   PARM                    KPJBA
051700071227     C                   MOVEL     KPJBU         TRUL06ds
051800000217     C                   MOVEA     LIN           L1
051900010608     C***
052000010608     C* CARICO TABELLA PUNTI OPERATIVI GESTITI da altri £6
052100010608     C***
052200071227     C                   CLEAR                   TRUL06ds
052300010608     C                   MOVE      '£6'          D06COD
052400010608     C                   MOVEL     'L'           D06TLA
052500020509     c                   eval      d06esc = 'G'
052600020509     c                   movel     simfel        d06key
052700071227     C                   MOVEL     TRUL06ds      KPJBU
052800010608     C*
052900010608     C                   CALL      'TRUL06R'
053000010608     C                   PARM                    KPJBA
053100071227     C                   MOVEL     KPJBU         TRUL06ds
053200010608     C                   MOVEA     LIN           L6S
053300920925     C                   ENDSR
053400071227      *---------------------------------------------------------------*
053500071227** - $MSG
053600071227Filiale di partenza errata                                                    1
053700071227Cliente inesistente                                                           2
053800000217Codice autotrasportatore errato                                               3
053900960321Data errata                                                                   4
054000000217La data spedizione DAL non puo' essere maggiore della data AL                 5
054100071227Filiale inesistente                                                           6
054200071227Se immesso cod.autotrasportatore immettere la filiale di partenza             7
054300071227Filiale gestita da altra filiale                                              8
054400080110"Stampa solo totali" non consente parzializzazione per autotrasportatori      9
054500080110"Stampa solo totali" non consente la selezione di un codice cliente           10
054600080110"Stampa solo totali" non consente "Stampa separata per filiale"               11
