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
003400081202      *
003500081202      * - Ds per decodifica cliente
003600081202     d TIBS69ds      e ds                  inz
003700081202     d ds_CNACO      e ds                  inz  extname(CNACO00F)
003800081202     d ds_CNIND      e ds                  inz  extname(CNIND00F)
003900081202     d ds_CNCLP      e ds                  inz  extname(CNCLP00F)
004000081202     d ds_FNCLS      e ds                  inz  extname(FNCLS00F)
004100071227      *
004200071227      * - Gestione anagrafica autotrasportatori/cooperative
004300071227     d FNLV24ds      e ds                  inz
004400071227      *
004500071227      * - Parametri per pgm. controllo/inversione data
004600071227     d WLBdat          ds                  inz
004700071227     d  G02dat                 1      8  0 inz
004800071227     d  G02inv                 9     16  0 inz
004900071227     d  G02err                17     17    inz
005000071227     d  G02tgi                18     22  0 inz
005100081202       //
005200081202       // - Ridefinizione codice clienti in schiere
005300081202     d ds_Ksc          ds                  inz
005400081202     d  V1Cksc                             inz
005500081202     d  V1Cks2                             inz
005600081202     d  V1Cks3                             inz
005700081202     d  V1Cks4                             inz
005800081202     d  V1Cks5                             inz
005900081202     d  V1Cks6                             inz
006000081202     d  V1Cks7                             inz
006100081202     d  V1Cks8                             inz
006200081202     d  V1Cks9                             inz
006300081202     d  V1CksA                             inz
006400081202     d   $Ksc                  1     70  0 inz  dim(10)
006500081202       // - Ridefinizione ragione sociale clienti in schiere
006600081202     d ds_KscD         ds                  inz
006700081202     d  V1Dksc                             inz
006800081202     d  V1Dks2                             inz
006900081202     d  V1Dks3                             inz
007000081202     d  V1Dks4                             inz
007100081202     d  V1Dks5                             inz
007200081202     d  V1Dks6                             inz
007300081202     d  V1Dks7                             inz
007400081202     d  V1Dks8                             inz
007500081202     d  V1Dks9                             inz
007600081202     d  V1DksA                             inz
007700081202     d   $KscD                 1    350    inz  dim(%elem($Ksc))
007800071227      *
007900071227      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
008000071227      *
008100071227     D kpdpdr          S                   LIKE(APDpdr) inz
008200071227     D kpdtip          S                   LIKE(APDtip) inz
008300071227     d Wdvd            s                   like(DD3dvd) inz
008400071227     d Wdva            s                   like(DD3dva) inz
008500081202      *
008600081202     d w0070           s              7  0 inz
008700071227      *
008800071227     d xx              s              3  0 inz
008900081202       //
009000081202       // - Parametri per XALFA3BR
009100081202      /copy gaitrasrc/srcprotopr,xalfa3brds
009200081202       //
009300081202       //?P R O T O T I P I   P R O C E D U R E   - - - - - - - - - -?*
009400081202       //
009500081202       // - Controllo/Decodifica cliente
009600081202      /copy gaitrasrc/srcprotopr,tibs69r
009700081202
009800081202       // - Ricerca clienti per ragione sociale
009900081202      /copy gaitrasrc/srcprotopr,xalfa3br
010000071227      *
010100071227      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
010200071227      *
010300081202     C* ACCESSO FIAPD00F
010400071227     C     Kfapd         klist
010500071227     C                   kfld                    kpdtip
010600071227     C                   kfld                    kpdpdr
010700071227
010800960321     C*****************************************************************
010900960321     C* RIEPILOGO INDICATORI
011000960321     C*****************************************************************
011100960321     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
011200960327     C* 30/31 - DI COMODO
011300081202     C* 40/60 - ERRORI
011400960321     C* 90    - INDICATORE DI ERRORE GENERICO
011500960321     C*****************************************************************
011600071227
011700000000     C     *ENTRY        PLIST
011800000000     C                   PARM                    KPJBA
011900900511     C*---------------------------------------------------------------*
012000960321     C* PULIZIA CAMPI VIDEATA
012100071227     c                   clear                   LSD2D01
012200960321     C*
012300900509     C     FOR01         TAG
012400071227     c                   if        not *in90
012500071227     c                   write     LSD2T01
012600071227     c                   endif
012700000217     C                   EXFMT     LSD2D01
012800960321     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
012900071227     C                   CLEAR                   V1DMSG
013000071227     C                   SETOFF                                       28  90
013100941223     C*
013200960321     C* CMD3 - FINE LAVORO
013300900509     C   KC              GOTO      FINE
013400960321     C*
013500960321     C* CONTROLLI FORMATO
013600960321     C                   EXSR      CTRD01
013700941223     C  NKF
013800900524     COR 90              GOTO      FOR01
013900960321     C*
014000000217     C* F6 - ELABORAZIONE
014100071227     C                   CLEAR                   FNLSD3ds
014200010522     C                   Z-ADD     V1CPOP        DD3PO
014300000217     C                   Z-ADD     V1CKSC        DD3KSC
014400081202     c                   eval      DD3ks2 = V1Cks2
014500081202     c                   eval      DD3ks3 = V1Cks3
014600081202     c                   eval      DD3ks4 = V1Cks4
014700081202     c                   eval      DD3ks5 = V1Cks5
014800081202     c                   eval      DD3ks6 = V1Cks6
014900081202     c                   eval      DD3ks7 = V1Cks7
015000081202     c                   eval      DD3ks8 = V1Cks8
015100081202     c                   eval      DD3ks9 = V1Cks9
015200081202     c                   eval      DD3ksA = V1CksA
015300000217     C                   Z-ADD     WDVD          DD3DVD
015400000217     C                   Z-ADD     WDVA          DD3DVA
015500010528     C                   MOVEL     V1CBRK        DD3BRK
015600071227     c                   eval      DD3sst = V1Csst
015700000224     C                   MOVE      V1CPDR        COM(1)
015800000224     C                   MOVE      V1CPD2        COM(2)
015900000224     C                   MOVE      V1CPD3        COM(3)
016000000224     C                   MOVE      V1CPD4        COM(4)
016100000224     C                   MOVE      V1CPD5        COM(5)
016200000224     C                   SORTA     COM
016300000224     C*
016400071227     C                   DO        5             xx
016500071227     C     COM(xx)       IFGT      *ZEROS
016600000225     C                   SELECT
016700000225     C     DD3PDR        WHENEQ    0
016800071227     C                   MOVE      COM(xx)       DD3PDR
016900010522     C                   MOVEL     DD3PO         DD3PDR
017000000225     C     DD3PD2        WHENEQ    0
017100071227     C                   MOVE      COM(xx)       DD3PD2
017200010522     C                   MOVEL     DD3PO         DD3PD2
017300000225     C     DD3PD3        WHENEQ    0
017400071227     C                   MOVE      COM(xx)       DD3PD3
017500010522     C                   MOVEL     DD3PO         DD3PD3
017600000225     C     DD3PD4        WHENEQ    0
017700071227     C                   MOVE      COM(xx)       DD3PD4
017800010522     C                   MOVEL     DD3PO         DD3PD4
017900000225     C     DD3PD5        WHENEQ    0
018000071227     C                   MOVE      COM(xx)       DD3PD5
018100010522     C                   MOVEL     DD3PO         DD3PD5
018200000225     C                   ENDSL
018300000224     C*
018400000224     C                   ENDIF
018500000224     C                   ENDDO
018600960321     C*
018700960327     C                   CLEAR                   KPJBU
018800071227     C                   MOVEL     FNLSD3ds      KPJBU
018900000217     C                   MOVEL     'LSD3'        KCOAZ
019000071227     c                   if        KNMUS = *all'1'
019100071228     c                   call      'FNLSD3R'
019200071227     c                   parm                    KPJBA
019300071227     c                   else
019400900523     C                   CALL      'BCH10'
019500900523     C                   PARM                    KPJBA
019600071227     c                   endif
019700960401     C*
019800920925     C     FINE          TAG
019900000000     C                   SETON                                        LR
020000960321     C*
020100960321     C*--- CONTROLLI FORMATO -----------------------------------------*
020200960321     C     CTRD01        BEGSR
020300071227     C                   movea     *zeros        *in(40)
020400071227     C*
020500071227     C**** FIL. DI PARTENZA ****
020600071227     C     V1CPOP        IFGT      *ZEROS
020700071227     C     V1CPOP        LOOKUP    L1                                     31
020800071227     C     *IN31         IFEQ      *OFF
020900071227     C                   MOVEL     $MSG(1)       V1DMSG
021000071227     C                   SETON                                        452890
021100071227     C                   GOTO      ENDCTR
021200071227     C                   ENDIF
021300071227     C* NON DEVE ESSERE UNA FIL. GESTITO DA ALTRA FIL.
021400071227     C     V1CPOP        LOOKUP    L6S                                    31
021500071227     C     *IN31         IFEQ      *ON
021600071227     C                   MOVEL     $MSG(8)       V1DMSG
021700071227     C                   SETON                                        452890
021800071227     C                   GOTO      ENDCTR
021900071227     C                   ENDIF
022000071227     C                   ENDIF
022100071227     C*
022200071227     C**** COD.AUTOTRASPORTATORE ****
022300071227     C                   CLEAR                   V1DPDR
022400071227     C     '?'           SCAN      V1CPDR                                 90
022500071227     C     *IN90         IFEQ      *ON
022600071227      *
022700071227      * imposto i campi della ds esterna fnlv24ds
022800071227     C                   clear                   fnlv24ds
022900071227     c                   if        V1Cpop   > *zeros
023000071227     C                   eval      d24fil   = v1cpop
023100071227     c                   else
023200071227     c                   eval      D24fil   = DUTpou
023300071227     c                   endif
023400071227     C                   eval      d24tip   = 'A'
023500071227     C                   eval      d24flg   = 'R'
023600071227     C                   movel     fnlv24ds      kpjbu
023700071227     C                   CALL      'FNLV24R'
023800071227     C                   PARM                    KPJBA
023900071227      *
024000071227      * valorizzo i campi a video
024100071227     C                   movel     kpjbu         fnlv24ds
024200071227     C                   if        d24pdr   <> *zeros
024300071227     C                   move      d24pdr        v1cpdr
024400071227     C                   movel     d24pdr        v1cpop
024500071227     C                   movel     d24rsc        v1dpdr
024600071227     C                   endif
024700071227     C                   SETON                                        42
024800071227     C                   GOTO      ENDCTR
024900071227     C                   ENDIF
025000071227     C*
025100071227     C     V1CPDR        IFNE      *ZEROS
025200071227     C     V1CPDR        ANDNE     *BLANKS
025300071227     C                   TESTN                   V1CPDR               30
025400071227     C     *IN30         IFEQ      *OFF
025500071227     C                   MOVEL     $MSG(3)       V1DMSG
025600071227     C                   SETON                                        422890
025700071227     C                   GOTO      ENDCTR
025800071227     C                   ENDIF
025900071227     C                   ENDIF
026000071227     C* ERRORE SE IMMESSO AUTOTRASPORTATORE MA NON P.O. DI PARTENZA
026100071227     C     V1CPDR        IFGT      *ZEROS
026200071227     C     V1CPOP        ANDEQ     *ZEROS
026300071227     C                   MOVEL     $MSG(7)       V1DMSG
026400071227     C                   SETON                                        452890
026500071227     C                   GOTO      ENDCTR
026600071227     C                   ENDIF
026700071227     C*
026800071227     C     V1CPDR        IFGT      *ZEROS
026900071227     C                   MOVEL     V1CPOP        W0070
027000071227     C                   MOVE      V1CPDR        W0070
027100071227     C*    W0070         CHAIN     FNAPD01L                           30
027200071227     C                   move      w0070         kpdpdr
027300071227     C                   move      'A'           kpdtip
027400071227     C     kfapd         chain     fiapd000                           30
027500071227     C     *IN30         IFEQ      *ON
027600071227     C                   MOVEL     $MSG(3)       V1DMSG
027700071227     C                   SETON                                        422890
027800071227     C                   GOTO      ENDCTR
027900071227     C                   ENDIF
028000071227     C                   MOVEL     APDRSC        V1DPDR
028100071227     C                   ENDIF
028200071227     C* CODICE 2
028300071227     C                   CLEAR                   V1DPD2
028400071227     C                   CLEAR                   WPDR
028500071227     C                   MOVE      V1CPD2        WPDR
028600071227     C                   EXSR      CTRPDR
028700071227     C                   MOVE      WPDR          V1CPD2
028800071227     C                   MOVEL     WDPDR         V1DPD2
028900071227     C     WELAB         IFEQ      '?'
029000071227     C                   SETON                                        46
029100071227     C                   GOTO      ENDCTR
029200071227     C                   ENDIF
029300071227     C     WELAB         IFEQ      'E'
029400071227     C                   SETON                                        462890
029500071227     C                   GOTO      ENDCTR
029600071227     C                   ENDIF
029700071227     C* CODICE 3
029800071227     C                   CLEAR                   V1DPD3
029900071227     C                   CLEAR                   WPDR
030000071227     C                   MOVE      V1CPD3        WPDR
030100071227     C                   EXSR      CTRPDR
030200071227     C                   MOVE      WPDR          V1CPD3
030300071227     C                   MOVEL     WDPDR         V1DPD3
030400071227     C     WELAB         IFEQ      '?'
030500071227     C                   SETON                                        47
030600071227     C                   GOTO      ENDCTR
030700071227     C                   ENDIF
030800071227     C     WELAB         IFEQ      'E'
030900071227     C                   SETON                                        472890
031000071227     C                   GOTO      ENDCTR
031100071227     C                   ENDIF
031200071227     C* CODICE 4
031300071227     C                   CLEAR                   V1DPD4
031400071227     C                   CLEAR                   WPDR
031500071227     C                   MOVE      V1CPD4        WPDR
031600071227     C                   EXSR      CTRPDR
031700071227     C                   MOVE      WPDR          V1CPD4
031800071227     C                   MOVEL     WDPDR         V1DPD4
031900071227     C     WELAB         IFEQ      '?'
032000071227     C                   SETON                                        48
032100071227     C                   GOTO      ENDCTR
032200071227     C                   ENDIF
032300071227     C     WELAB         IFEQ      'E'
032400071227     C                   SETON                                        482890
032500071227     C                   GOTO      ENDCTR
032600071227     C                   ENDIF
032700071227     C* CODICE 5
032800071227     C                   CLEAR                   V1DPD5
032900071227     C                   CLEAR                   WPDR
033000071227     C                   MOVE      V1CPD5        WPDR
033100071227     C                   EXSR      CTRPDR
033200071227     C                   MOVE      WPDR          V1CPD5
033300071227     C                   MOVEL     WDPDR         V1DPD5
033400071227     C     WELAB         IFEQ      '?'
033500071227     C                   SETON                                        49
033600071227     C                   GOTO      ENDCTR
033700071227     C                   ENDIF
033800071227     C     WELAB         IFEQ      'E'
033900071227     C                   SETON                                        492890
034000071227     C                   GOTO      ENDCTR
034100071227     C                   ENDIF
034200960321     C*
034300000217     C****  DATA SPEDIZIONE DAL  ****
034400961015    1C     V1CDVD        IFGT      0
034500960321     C                   MOVE      V1CDVD        G02DAT
034600960321     C                   MOVEL     *ZERO         G02INV
034700960321     C                   MOVEL     *BLANKS       G02ERR
034800941223     C                   CALL      'XSRDA8'
034900920925     C                   PARM                    WLBDAT
035000920925     C     G02ERR        IFEQ      '1'
035100071227     C                   MOVEL     $MSG(4)       V1DMSG
035200960321     C                   SETON                                        432890
035300920925     C                   GOTO      ENDCTR
035400960321     C                   ENDIF
035500960321     C                   Z-ADD     G02DAT        V1CDVD
035600960321     C                   Z-ADD     G02INV        WDVD
035700900524     C*
035800000217     C****  DATA SPEDIZIONE  AL  ****
035900960326    2C     V1CDVA        IFNE      0
036000960321     C                   MOVE      V1CDVA        G02DAT
036100960321     C                   MOVEL     *ZERO         G02INV
036200960321     C                   MOVEL     *BLANKS       G02ERR
036300941223     C                   CALL      'XSRDA8'
036400920925     C                   PARM                    WLBDAT
036500920925     C     G02ERR        IFEQ      '1'
036600071227     C                   MOVEL     $MSG(4)       V1DMSG
036700960321     C                   SETON                                        442890
036800920925     C                   GOTO      ENDCTR
036900960321     C                   ENDIF
037000960321     C                   Z-ADD     G02DAT        V1CDVA
037100960321     C                   Z-ADD     G02INV        WDVA
037200941223     C*
037300900524     C* DATA DAL NO MAGGIORE DATA AL
037400960326    3C     WDVD          IFGT      WDVA
037500071227     C                   MOVEL     $MSG(5)       V1DMSG
037600960321     C                   SETON                                        432890
037700900524     C                   GOTO      ENDCTR
037800960326    3C                   ENDIF
037900960321     C*
038000960321     C* SE INSERITA DATA AL OCCORRE INSERIRE ANCHE DATA DAL
038100960326    3C     WDVD          IFEQ      0
038200071227     C                   MOVEL     $MSG(4)       V1DMSG
038300960321     C                   SETON                                        432890
038400960321     C                   GOTO      ENDCTR
038500960326    3C                   ENDIF
038600960326    2C                   ENDIF
038700961025    1C                   ENDIF
038800081202      *
038900081202      /free
039000081202         // **** CODICE CLIENTE  ****
039100081202
039200081202         FOR  xx = 1   TO   %elem($Ksc);
039300081202
039400081202           Select;
039500081202
039600081202             // Se Codice cliente immesso => controllo sottoconto
039700081202             When  $Ksc(xx) > *zero;
039800081202               clear  TIBS69ds;
039900081202               I69kac = $Ksc(xx);
040000081202               tibs69r (TIBS69ds : ds_CNACO :
040100081202                                   ds_CNIND :
040200081202                                   ds_CNCLP :
040300081202                                   ds_FNCLS);
040400081202               // - Se non esiste nel P.d.C. ---> ERRORE
040500081202               if  O69err = *on;
040600081202                 V1Dmsg = $Msg(2);
040700081202                 *in28 = *on;
040800081202                 *in90 = *on;
040900081202                 *in(50+xx) = *on;
041000081202                 leavesr;
041100081202               endif;
041200081202               // - Decodifica codice cliente
041300081202               $KscD(xx) = ACOrag;
041400081202
041500081202             // Se Codice cliente a zero ma messa Ragione Sociale
041600081202             // => Ricerca alfabetica (escludendo annullati)
041700081202             When  $KscD(xx) <> *blank;
041800081202               xpardut = RSut;
041900081202               xparkut = 1;
042000081202               xparrag = $KscD(xx);
042100081202               xparkcc = DUTkci;
042200081202               xparsta = 9;          // esclusione annullati
042300081202               clear xparflr;
042400081202               xparnum = 1;
042500081202               clear xparkcm;
042600081202               clear xparksm;
042700081202               clear xparkdm;
042800081202               clear xparesci;
042900081202               clear xparerr;
043000081202               clear xpariva;
043100081202               clear xparcdf;
043200081202               callp XALFA3BR (xpardut : xparkut : xparrag :
043300081202                               xparkcc : xparsta : xparflr :
043400081202                               xpardit : xparnum : xparkcm :
043500081202                               xparksm : xparkdm : xparesci :
043600081202                               xparerr : xpariva : xparcdf);
043700081202               if  xparsta <> -1;
043800081202                 $Ksc(xx)  = %int( %subst( xparksm : 1 : 7 ));
043900081202                 $KscD(xx) = xparrag   ;
044000081202               endif;
044100081202               *in90 = *on;
044200081202               //leavesr;
044300081202
044400081202           EndSl;
044500081202
044600081202         ENDFOR;
044700081202
044800081202      /end-free
044900080110      *
045000080110      * "Stampa solo totali" inibisce parzializzazione per autotrasp.
045100080110     c                   if             V1Csst = 'S'
045200080110     c                             and (V1Cpdr > *zeros
045300080110     c                              or  V1Cpd2 > *zeros
045400080110     c                              or  V1Cpd3 > *zeros
045500080110     c                              or  V1Cpd4 > *zeros
045600080110     c                              or  V1Cpd5 > *zeros)
045700080110     c                   movel     $Msg(9)       V1Dmsg
045800080110     c                   seton                                        502890
045900080110     c                   leavesr
046000080110     c                   endif
046100080110      *
046200080110      * "Stampa solo totali" inibisce selezione del cod. cliente
046300080110     c                   if             V1Csst = 'S'
046400080110     c                             and  V1Cksc > *zeros
046500080110     c                   movel     $Msg(10)      V1Dmsg
046600080110     c                   seton                                        502890
046700080110     c                   leavesr
046800080110     c                   endif
046900071227      *
047000071227      * "Stampa solo totali" inibisce "Stampa separata per filiale"
047100071227     c                   if             V1Csst = 'S'
047200071227     c                             and  V1Cbrk = 'S'
047300080110     c                   movel     $Msg(11)      V1Dmsg
047400071227     c                   seton                                        502890
047500071227     c                   leavesr
047600071227     c                   endif
047700960321     C*
047800000224     C     ENDCTR        ENDSR
047900000224     C*
048000000224     C* CONTROLLO COD.AUTOTRASPORTATORE ------------------------------*
048100000224     C     CTRPDR        BEGSR
048200000224     C                   CLEAR                   WELAB
048300000224     C                   CLEAR                   WDPDR
048400000224     C* COD.AUTOTRASPORTATORE
048500000224     C     '?'           SCAN      WPDR                                   90
048600000224     C*
048700000224     C     *IN90         IFEQ      *ON
048800021203      *
048900021203      * imposto i campi della ds esterna fnlv24ds
049000021203     C                   clear                   fnlv24ds
049100021203     C                   eval      d24fil   = v1cpop
049200021203     C                   eval      d24tip   = 'A'
049300021203     C                   eval      d24flg   = 'R'
049400021203     C                   movel     fnlv24ds      kpjbu
049500021203
049600000224     C                   CALL      'FNLV24R'
049700000224     C                   PARM                    KPJBA
049800000224     C                   MOVEL     '?'           WELAB
049900021203      *
050000021203      * valorizzo i campi a video
050100021203     C                   movel     kpjbu         fnlv24ds
050200021203     C                   if        d24pdr   <> *zeros
050300021203     C                   move      d24pdr        wpdr              4
050400021203     C                   movel     d24pdr        v1cpop
050500021203     C                   movel     d24rsc        wdpdr            20
050600021203     C                   endif
050700000224     C                   GOTO      ENDPDR
050800000224     C                   ENDIF
050900000224     C*
051000000224     C     WPDR          IFNE      *ZEROS
051100000224     C     WPDR          ANDNE     *BLANKS
051200000224     C                   TESTN                   WPDR                 30
051300000224     C     *IN30         IFEQ      *OFF
051400071227     C                   MOVEL     $MSG(3)       V1DMSG
051500000224     C                   MOVEL     'E'           WELAB             1
051600000224     C                   GOTO      ENDPDR
051700000224     C                   ENDIF
051800000224     C                   ENDIF
051900010522     C* ERRORE SE IMMESSO COD.AUTOTRASPORTATORE SENZA P.O. DI PARTENZA
052000010522     C     WPDR          IFGT      *ZEROS
052100010522     C     V1CPOP        ANDEQ     *ZEROS
052200071227     C                   MOVEL     $MSG(7)       V1DMSG
052300010522     C                   MOVEL     'E'           WELAB
052400010522     C                   SETON                                        45
052500010522     C                   GOTO      ENDPDR
052600010522     C                   ENDIF
052700000224     C*
052800000224     C     WPDR          IFGT      *ZEROS
052900000224     C                   MOVE      WPDR          W0070
053000010522     C                   MOVEL     V1CPOP        W0070
053100021203     C*    W0070         CHAIN     FNAPD01L                           30
053200021205     C                   move      w0070         kpdpdr
053300021203     C                   move      'A'           kpdtip
053400071227     C     kfapd         chain     fiapd000                           30
053500000224     C     *IN30         IFEQ      *ON
053600071227     C                   MOVEL     $MSG(3)       V1DMSG
053700000224     C                   MOVEL     'E'           WELAB             1
053800000224     C                   GOTO      ENDPDR
053900000224     C                   ENDIF
054000000224     C                   MOVEL     APDRSC        WDPDR
054100000224     C                   ENDIF
054200000224     C*
054300000224     C     ENDPDR        ENDSR
054400920925     C*
054500960321     C*--- SR INIZIALE -----------------------------------------------*
054600960321     C     *INZSR        BEGSR
054700071227      *
054800071227      * Reperimento dati job
054900071227     c     *dtaara       define    §azute        AZUTEds
055000071227     c     *dtaara       define    §datiute      dDATIUTE
055100071227      *
055200071227     c                   in(E)     *dtaara
055300071227     c                   IF        %ERROR or RSUT = *blanks
055400071227     c                   clear                   Tibs34Ds
055500071227     c                   call      'TIBS34R'
055600071227     c                   parm                    Tibs34Ds
055700071227     c                   in        *dtaara
055800071227     c                   ENDIF
055900071227      *
056000071227     c                   eval      V1Cpgm = SDSpgm
056100960321     C*
056200000217     C***
056300000217     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
056400000217     C***
056500071227     C                   CLEAR                   TRUL06ds
056600000217     C                   MOVE      '£1'          D06COD
056700000217     C                   MOVEL     SIMFEL        D06KEY
056800071227     C                   MOVEL     TRUL06ds      KPJBU
056900000217     C*
057000000217     C                   CALL      'TRUL06R'
057100000217     C                   PARM                    KPJBA
057200071227     C                   MOVEL     KPJBU         TRUL06ds
057300000217     C                   MOVEA     LIN           L1
057400010608     C***
057500010608     C* CARICO TABELLA PUNTI OPERATIVI GESTITI da altri £6
057600010608     C***
057700071227     C                   CLEAR                   TRUL06ds
057800010608     C                   MOVE      '£6'          D06COD
057900010608     C                   MOVEL     'L'           D06TLA
058000020509     c                   eval      d06esc = 'G'
058100020509     c                   movel     simfel        d06key
058200071227     C                   MOVEL     TRUL06ds      KPJBU
058300010608     C*
058400010608     C                   CALL      'TRUL06R'
058500010608     C                   PARM                    KPJBA
058600071227     C                   MOVEL     KPJBU         TRUL06ds
058700010608     C                   MOVEA     LIN           L6S
058800920925     C                   ENDSR
058900071227      *---------------------------------------------------------------*
059000071227** - $MSG
059100071227Filiale di partenza errata                                                    1
059200071227Cliente inesistente                                                           2
059300000217Codice autotrasportatore errato                                               3
059400960321Data errata                                                                   4
059500000217La data spedizione DAL non puo' essere maggiore della data AL                 5
059600071227Filiale inesistente                                                           6
059700071227Se immesso cod.autotrasportatore immettere la filiale di partenza             7
059800071227Filiale gestita da altra filiale                                              8
059900080110"Stampa solo totali" non consente parzializzazione per autotrasportatori      9
060000080110"Stampa solo totali" non consente la selezione di un codice cliente           10
060100080110"Stampa solo totali" non consente "Stampa separata per filiale"               11
