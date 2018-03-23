000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000125     H*
000400030623     FTITAS30C  IF   E           K DISK
000500030623     FTITAA30C  IF   E           K DISK
000600120601     FTITA430C  IF   E           K DISK
000700170119     Ftivgdtmp  if   E             DISK
000800120601     FTIVAWWWT  O    E             DISK    commit
000900030623     D*------------------
001000030623     D* DS REPERIMENTO DATI CLIENTE
001100030623     D*-------------------
001200030623     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001300030623     D ACODS         E DS                  EXTNAME(CNACO00F)
001400030623     D INDDS         E DS                  EXTNAME(CNIND00F)
001500030623     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001600030623     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001700030623     D*-------------------
001800030623     D* VARIABILI DI WRK
001900030623     D*-------------------
002000991027     D prmppt          s             50
002100991027     D prmesito        s              1
002200991027     D wrkesito        s                   like(prmesito)
002300170119     D wVAWDTA         s           2048A   inz varying
002400120601     D DTA44         e ds
002500170119     D tivafDS       e ds                  extname(tivaf00t)
002600170119     D tivafAsciDS   e ds                  extname(tivafwwt) prefix(A_)
002700170119     D tivafEbcdDS   e ds                  extname(tivaf1wt) prefix(E_)
002800170119     D iASCI           s               N   inz
002900170119     D iEBCD           s               N   inz
003000170119     D iEXT            s               N   inz
003100170119     D iPVL            s               N   inz
003200170119     D iCIG            s               N   inz
003300170120     D iDCM            s               N   inz
003400170119     D iBON            s               N   inz
003500170206     D iPUNTO          s               N   inz
003600170119     D pSEP            s              1A   inz
003700170119     D wRSD            s             35A   inz
003800170119     D wIND            s             35A   inz
003900170119     D wRSM            s             35A   inz
004000170119     D wINM            s             35A   inz
004100170119     D wCAM            s              9A   inz
004200170119     D wLOM            s             35A   inz
004300170119     D wPRM            s              2A   inz
004400170119     D wNZM            s              3A   inz
004500170119     D wRMO            s             35A   inz
004600170119     D wCMO            s              9A   inz
004700170119     D wNMO            s              3A   inz
004800170119     D wPVL_A          s              9A   inz
004900170119     D wPVL_E          s              7A   inz
005000170120     D wCCA            s              1A   inz
005100170120     D wDCM_A          s              9A   inz
005200170120     D wDCM_E          s              8A   inz
005300170119
005400030623     D*-------------------
005500941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005600030623     D*-------------------
005700000125     D*  8+3
005800000125     D                 DS
005900000125     D  WS13                   1      1
006000000125     D  WI13                   2      9
006100170119     D  WV13                  10     10    inz(',')
006200000125     D  WD13                  11     13
006300000125     D  WASC13                 1     13
006400941221     D* 10+3
006500941221     D                 DS
006600941221     D  WS15                   1      1
006700941221     D  WI15                   2     11
006800170119     D  WV15                  12     12    inz(',')
006900941221     D  WD15                  13     15
007000941221     D  WASC15                 1     15
007100941221     D*  6+1
007200941221     D                 DS
007300941221     D  WS09                   1      1
007400941221     D  WI09                   2      7
007500170119     D  WV09                   8      8    inz(',')
007600941221     D  WD09                   9      9
007700941221     D  WASC09                 1      9
007800941221     D*  2+3
007900941221     D                 DS
008000941221     D  WS07                   1      1
008100941221     D  WI07                   2      3
008200170119     D  WV07                   4      4    inz(',')
008300941221     D  WD07                   5      7
008400941221     D  WASC07                 1      7
008500040317
008600040317     D*-------------------
008700040317     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
008800040317     D*-------------------
008900040317     D TxtInOut        S           2048
009000040317     D ElencoChar      S            256
009100040317     D TipoElenco      S              1
009200040317     D CharSost        S              1
009300040317     D UpperCase       S              1
009400040317     D ChkNull         S              1
009500040317     D CharNull        S              1
009600040317     D Esito           S              1
009700040317
009800040317
009900000904
010000170119     C*
010100170119     C* Avvio il monitoring del intero flusso
010200170119     C                   monitor
010300170119     C*
010400170119     C* Effettuo traduzione
010500991027     C                   EXSR      traduci
010600170119     C*
010700170119     C* Gestisco eventuale errore
010800170119     C                   on-error
010900170119     C*
011000170119     C* Imposto esito transazione a errore
011100170119     C                   EVAL      wrkesito = '2'
011200170119     C*
011300170119     C* Se errore => emetto operazione d ROLLBACK
011400170119     C                   ROLBK
011500170119     C*
011600170119     C* Arresto il monitoring
011700170119     C                   endmon
011800170119     C*
011900921023     C                   SETON                                        LR
012000170119
012100170119
012200170119
012300991027     C     traduci       BEGSR
012400170119     C*
012500170119     C* Inizializzazione indicatori di procedure per parametri input
012600170119     C                   eval      iASCI = *off
012700170119     C                   eval      iEBCD = *off
012800170119     C                   eval      iEXT  = *off
012900170119     C                   eval      iPVL  = *off
013000170119     C                   eval      iCIG  = *off
013100170120     C                   eval      iDCM  = *off
013200170119     C                   eval      iBON  = *off
013300170206     C                   eval      iPUNTO= *off
013400170119     C*
013500170119     C* Se richiesto ritorno dati in formato EBCDIC altrimenti ASCII
013600170119     C                   if        %subst(prmppt:1:1) = 'S'
013700170119     C                   eval      iEBCD = *on
013800170119     C                   else
013900170119     C                   eval      iASCI = *on
014000170119     C                   endif
014100170119     C*
014200170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
014300170119     C                   if        %subst(prmppt:2:1) = 'S'
014400170119     C                   eval      iEXT = *on
014500170119     C                   endif
014600170119     C*
014700170119     C* Se richiesti dati di estensione relativi al Peso Tassabile
014800170119     C                   if        %subst(prmppt:3:1) = 'S'
014900170119     C                   eval      iPVL = *on
015000170119     C                   endif
015100170119     C*
015200170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
015300170119     C                   if        %subst(prmppt:4:1) = 'S'
015400170119     C                   eval      iCIG = *on
015500170119     C                   endif
015600170119     C*
015700170119     C* Verifico il carattere separatore decimale indicato nei parametri del cliente/tipo file
015800170119     C                   select
015900170119     C                   when      %subst(prmppt:5:1) = '.'
016000170119     C                   eval      pSEP = '.'
016100170119     C                   other
016200170119     C                   eval      pSEP = ','
016300170119     C                   endsl
016400170120     C*
016500170120     C* Se richiesti dati di estensione relativi al Codice Cons. Anomala e Data Consegna Merce
016600170120     C                   if        %subst(prmppt:6:1) = 'S'
016700170120     C                   eval      iDCM = *on
016800170120     C                   endif
016900170119     C*
017000170119     C* Se richiesto di "bonificare" i dati di output da eventuali caratteri "strani"
017100170119     C                   if        %subst(prmppt:7:1) = 'S'
017200170119     C                   eval      iBON = *on
017300170119     C                   endif
017400170206     C*
017500170206     C* Se richiesto di "terminare" i dati di output con un punto
017600170206     C                   if        %subst(prmppt:8:1) = 'S'
017700170206     C                   eval      iPUNTO = *on
017800170206     C                   endif
017900170119     C*
018000170119     C* Scorro tutto il file
018100170119     C                   read      tivgdtmp
018200170119     C                   dow       not %eof(tivgdtmp)
018300170119     C*
018400170119     C* Inizializzo DS di output
018500170119     C                   clear                   TIVAW000
018600170119     C                   clear                   tivafAsciDS
018700170119     C                   clear                   tivafEbcdDS
018800170119     C*
018900170119     C                   eval      tivafDS = vgdDTA
019000170119     C*
019100170119     C* Gestisco formato richiesto ASCII/EBCDIC
019200170119     C                   select
019300170119     C                   when      iASCI
019400170119     C                   exsr      fmtVAF_ASCI
019500170119     C                   when      iEBCD
019600170119     C                   exsr      fmtVAF_EBCD
019700170119     C                   other
019800170119     C                   eval      wrkesito = '2'
019900170119     C                   endsl
020000170119     C*
020100170119     C* Se tutto ok => procedo
020200170119     C                   if        wrkesito = *blanks
020300170119     C*
020400170119     C* Inizializzo eventuali dati possibilmente aggiuntivi
020500170119     C                   exsr      inzvar
020600170119     C*
020700170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
020800170120     C                   if        iEXT or iPVL or iDCM
020900170119     C                   exsr      repext
021000170119     C                   endif
021100170119     C*
021200170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
021300170119     C                   if        iCIG
021400170119     C                   exsr      repcig
021500170119     C                   endif
021600170119     C*
021700170119     C* Scarico il buffer di output formato richiesto ASCII/EBCDIC
021800170119     C                   select
021900170119     C                   when      iASCI
022000170119     C                   eval      wVAWDTA = tivafAsciDS
022100170119     C                   when      iEBCD
022200170119     C                   eval      wVAWDTA = tivafEbcdDS
022300170119     C                   endsl
022400170119     C*
022500170119     C                   if        iEXT
022600170119     C                   eval      wVAWDTA = wVAWDTA + wRSD
022700170119     C                                               + wIND
022800170119     C                                               + wRSM
022900170119     C                                               + wINM
023000170119     C                                               + wCAM
023100170119     C                                               + wLOM
023200170119     C                                               + wPRM
023300170119     C                                               + wNZM
023400170119     C                                               + wRMO
023500170119     C                                               + wCMO
023600170119     C                                               + wNMO
023700170119     C                   endif
023800170119     C*
023900170119     C                   if        iPVL
024000170119     C                   select
024100170119     C                   when      iEBCD
024200170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_E
024300170119     C                   other
024400170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_A
024500170119     C                   endsl
024600170119     C                   endif
024700170119     C*
024800170119     C                   if        iCIG
024900170119     C                   eval      wVAWDTA = wVAWDTA + §TA4CUP
025000170119     C                                               + §TA4CIG
025100170119     C                   endif
025200170120     C*
025300170120     C                   if        iDCM
025400170120     C                   select
025500170120     C                   when      iEBCD
025600170120     C                   eval      wVAWDTA = wVAWDTA + wCCA + wDCM_E
025700170120     C                   other
025800170120     C                   eval      wVAWDTA = wVAWDTA + wCCA + wDCM_A
025900170120     C                   endsl
026000170120     C                   endif
026100170119     C*
026200170119     C                   if        iBON
026300170119     C                   eval      TxtInOut = wVAWDTA
026400170119     C                   exsr      exeBon
026500170119     C                   eval      wVAWDTA = TxtInOut
026600170119     C                   endif
026700170206     C*
026800170206     C                   if        iPUNTO
026900170206     C                   eval      wVAWDTA = wVAWDTA + '.'
027000170206     C                   endif
027100170119     C*
027200170119     C                   eval      VAWDTA = wVAWDTA
027300170119     C                   write     TIVAW000
027400170119     C                   endif
027500170119     C*
027600170119     C* Proseguo con il record successivo
027700170119     C                   read      tivgdtmp
027800170119     C                   enddo
027900170119     C*
028000170119     C                   if        wrkesito <> '2'
028100170119     C                   eval      wrkesito  = '0'
028200170119     C                   endif
028300991027     C*
028400991027     C                   eval      wrkesito = '0'
028500991027     C*
028600910830     C                   ENDSR
028700170119
028800170119
028900170119
029000040317     C     exeBon        BEGSR
029100040317     C*
029200040317     C* Imposto i parametri x il *pgm d "bonifica"
029300040317     C                   EVAL      ElencoChar = *blanks
029400040317     C                   EVAL      TipoElenco = *blanks
029500040317     C                   EVAL      CharSost   = *blanks
029600040317     C                   EVAL      UpperCase  = *blanks
029700040317     C                   EVAL      ChkNull    = '1'
029800040317     C                   EVAL      CharNull   = *blanks
029900040317     C                   EVAL      Esito      = *blanks
030000040317     C*
030100040317     C* Effettuo la chiamata al *pgm d "bonifica"
030200040317     C                   CALL      'XCHKCHAR'
030300040317     C                   PARM                    TxtInOut
030400040317     C                   PARM                    ElencoChar
030500040317     C                   PARM                    TipoElenco
030600040317     C                   PARM                    CharSost
030700040317     C                   PARM                    UpperCase
030800040317     C                   PARM                    ChkNull
030900040317     C                   PARM                    CharNull
031000040317     C                   PARM                    Esito
031100030623     C*
031200991027     C                   ENDSR
031300170119
031400170119
031500170119
031600170119     C*
031700170119     C     fmtVAF_ASCI   BEGSR
031800170119     C*
031900170119     C                   MOVE      VAFAAS        A_VHFAAS
032000170119     C                   MOVE      VAFLNP        A_VHFLNP
032100170119     C                   MOVE      VAFNRS        A_VHFNRS
032200170119     C                   MOVE      VAFNSP        A_VHFNSP
032300170119     C                   MOVE      VAFTBL        A_VHFTBL
032400170119     C                   MOVE      VAFMGS        A_VHFMGS
032500170119     C                   MOVE      VAFKSC        A_VHFKSC
032600170119     C                   MOVE      VAFLNA        A_VHFLNA
032700170119     C                   MOVE      VAFNCL        A_VHFNCL
032800170119     C*
032900170119     C                   MOVE      VAFPKB        WD09
033000170119     C                   MOVEL     VAFPKB        WI09
033100170119     C                   MOVE      WASC09        A_VHFPKB
033200170119     C*
033300170119     C                   MOVE      VAFVLF        WD07
033400170119     C                   MOVEL     VAFVLF        WI07
033500170119     C                   MOVE      WASC07        A_VHFVLF
033600170119     C*
033700170119     C                   MOVE      VAFQFT        WD15
033800170119     C                   MOVEL     VAFQFT        WI15
033900170119     C                   MOVE      WASC15        A_VHFQFT
034000170119     C*
034100170119     C                   MOVE      VAFPOR        WD13
034200170119     C                   MOVEL     VAFPOR        WI13
034300170119     C                   MOVE      WASC13        A_VHFPOR
034400170119     C*
034500170119     C                   MOVE      VAFSV1        A_VHFSV1
034600170119     C                   MOVE      VAFVA1        WD13
034700170119     C                   MOVEL     VAFVA1        WI13
034800170119     C                   MOVE      WASC13        A_VHFVA1
034900170119     C*
035000170119     C                   MOVE      VAFSV2        A_VHFSV2
035100170119     C                   MOVE      VAFVA2        WD13
035200170119     C                   MOVEL     VAFVA2        WI13
035300170119     C                   MOVE      WASC13        A_VHFVA2
035400170119     C*
035500170119     C                   MOVE      VAFSV3        A_VHFSV3
035600170119     C                   MOVE      VAFVA3        WD13
035700170119     C                   MOVEL     VAFVA3        WI13
035800170119     C                   MOVE      WASC13        A_VHFVA3
035900170119     C*
036000170119     C                   MOVE      VAFSV4        A_VHFSV4
036100170119     C                   MOVE      VAFVA4        WD13
036200170119     C                   MOVEL     VAFVA4        WI13
036300170119     C                   MOVE      WASC13        A_VHFVA4
036400170119     C*
036500170119     C                   MOVE      VAFSV5        A_VHFSV5
036600170119     C                   MOVE      VAFVA5        WD13
036700170119     C                   MOVEL     VAFVA5        WI13
036800170119     C                   MOVE      WASC13        A_VHFVA5
036900170119     C*
037000170119     C                   MOVE      VAFSV6        A_VHFSV6
037100170119     C                   MOVE      VAFVA6        WD13
037200170119     C                   MOVEL     VAFVA6        WI13
037300170119     C                   MOVE      WASC13        A_VHFVA6
037400170119     C*
037500170119     C                   MOVE      VAFSV7        A_VHFSV7
037600170119     C                   MOVE      VAFVA7        WD13
037700170119     C                   MOVEL     VAFVA7        WI13
037800170119     C                   MOVE      WASC13        A_VHFVA7
037900170119     C*
038000170119     C                   MOVE      VAFSV8        A_VHFSV8
038100170119     C                   MOVE      VAFVA8        WD13
038200170119     C                   MOVEL     VAFVA8        WI13
038300170119     C                   MOVE      WASC13        A_VHFVA8
038400170119     C*
038500170119     C                   MOVE      VAFVAX        WD13
038600170119     C                   MOVEL     VAFVAX        WI13
038700170119     C                   MOVE      WASC13        A_VHFVAX
038800170119     C*
038900170119     C                   MOVE      VAFIMV        WD15
039000170119     C                   MOVEL     VAFIMV        WI15
039100170119     C                   MOVE      WASC15        A_VHFIMV
039200170119     C*
039300170119     C                   MOVE      VAFDFT        A_VHFDFT
039400170119     C                   MOVE      VAFNFT        A_VHFNFT
039500170119     C                   MOVE      VAFFIV        A_VHFFIV
039600170119     C                   MOVE      VAFDIV        A_VHFDIV
039700170119     C                   MOVE      VAFDRT        A_VHFDRT
039800170119     C                   MOVE      VAFRMN        A_VHFRMN
039900170119     C                   MOVE      VAFRMA        A_VHFRMA
040000170119     C                   MOVE      VAFRMX        A_VHFRMX
040100170119     C                   MOVE      VAFFTC        A_VHFFTC
040200170119     C                   MOVE      VAFTC2        A_VHFTC2
040300170119     C                   MOVE      VAFTSP        A_VHFTSP
040400170119     C                   MOVE      VAFFAP        A_VHFFAP
040500170119     C                   MOVE      VAFFIN        A_VHFFIN
040600170119     C                   MOVE      VAFCTR        A_VHFCTR
040700170119     C                   MOVE      VAFNAS        A_VHFNAS
040800170119     C                   MOVE      VAFCTS        A_VHFCTS
040900170119     C                   MOVE      VAFCAD        A_VHFCAD
041000170119     C                   MOVE      VAFLOD        A_VHFLOD
041100170119     C                   MOVE      VAFPRD        A_VHFPRD
041200170119     C                   MOVE      VAFNZD        A_VHFNZD
041300170119     C                   MOVE      VAFVAS        A_VHFVAS
041400170119     C*
041500170119     C                   MOVE      VAFIAS        WD15
041600170119     C                   MOVEL     VAFIAS        WI15
041700170119     C                   MOVE      WASC15        A_VHFIAS
041800170119     C*
041900170119     C                   MOVE      VAFFPC        A_VHFFPC
042000170119     C*
042100170119     C                   MOVE      VAFPKC        WD09
042200170119     C                   MOVEL     VAFPKC        WI09
042300170119     C                   MOVE      WASC09        A_VHFPKC
042400170119     C*
042500170119     C                   MOVE      VAFFVC        A_VHFFVC
042600170119     C*
042700170119     C                   MOVE      VAFVLC        WD07
042800170119     C                   MOVEL     VAFVLC        WI07
042900170119     C                   MOVE      WASC07        A_VHFVLC
043000170119     C*
043100170119     C                   ENDSR
043200170119
043300170119
043400170119
043500170119     C*
043600170119     C     fmtVAF_EBCD   BEGSR
043700170119     C*
043800170119     C                   MOVE      VAFAAS        E_VHFAAS
043900170119     C                   MOVE      VAFLNP        E_VHFLNP
044000170119     C                   MOVE      VAFNRS        E_VHFNRS
044100170119     C                   MOVE      VAFNSP        E_VHFNSP
044200170119     C                   MOVE      VAFTBL        E_VHFTBL
044300170119     C                   MOVE      VAFMGS        E_VHFMGS
044400170119     C                   MOVE      VAFKSC        E_VHFKSC
044500170119     C                   MOVE      VAFLNA        E_VHFLNA
044600170119     C                   MOVE      VAFNCL        E_VHFNCL
044700170119     C                   MOVE      VAFPKB        E_VHFPKB
044800170119     C                   MOVE      VAFVLF        E_VHFVLF
044900170119     C                   MOVE      VAFQFT        E_VHFQFT
045000170119     C                   MOVE      VAFPOR        E_VHFPOR
045100170119     C                   MOVE      VAFSV1        E_VHFSV1
045200170119     C                   MOVE      VAFVA1        E_VHFVA1
045300170119     C                   MOVE      VAFSV2        E_VHFSV2
045400170119     C                   MOVE      VAFVA2        E_VHFVA2
045500170119     C                   MOVE      VAFSV3        E_VHFSV3
045600170119     C                   MOVE      VAFVA3        E_VHFVA3
045700170119     C                   MOVE      VAFSV4        E_VHFSV4
045800170119     C                   MOVE      VAFVA4        E_VHFVA4
045900170119     C                   MOVE      VAFSV5        E_VHFSV5
046000170119     C                   MOVE      VAFVA5        E_VHFVA5
046100170119     C                   MOVE      VAFSV6        E_VHFSV6
046200170119     C                   MOVE      VAFVA6        E_VHFVA6
046300170119     C                   MOVE      VAFSV7        E_VHFSV7
046400170119     C                   MOVE      VAFVA7        E_VHFVA7
046500170119     C                   MOVE      VAFSV8        E_VHFSV8
046600170119     C                   MOVE      VAFVA8        E_VHFVA8
046700170119     C                   MOVE      VAFVAX        E_VHFVAX
046800170119     C                   MOVE      VAFIMV        E_VHFIMV
046900170119     C                   MOVE      VAFDFT        E_VHFDFT
047000170119     C                   MOVE      VAFNFT        E_VHFNFT
047100170119     C                   MOVE      VAFFIV        E_VHFFIV
047200170119     C                   MOVE      VAFDIV        E_VHFDIV
047300170119     C                   MOVE      VAFDRT        E_VHFDRT
047400170119     C                   MOVE      VAFRMN        E_VHFRMN
047500170119     C                   MOVE      VAFRMA        E_VHFRMA
047600170119     C                   MOVE      VAFRMX        E_VHFRMX
047700170119     C                   MOVE      VAFFTC        E_VHFFTC
047800170119     C                   MOVE      VAFTC2        E_VHFTC2
047900170119     C                   MOVE      VAFTSP        E_VHFTSP
048000170119     C                   MOVE      VAFFAP        E_VHFFAP
048100170119     C                   MOVE      VAFFIN        E_VHFFIN
048200170119     C                   MOVE      VAFCTR        E_VHFCTR
048300170119     C                   MOVE      VAFNAS        E_VHFNAS
048400170119     C                   MOVE      VAFCTS        E_VHFCTS
048500170119     C                   MOVE      VAFCAD        E_VHFCAD
048600170119     C                   MOVE      VAFLOD        E_VHFLOD
048700170119     C                   MOVE      VAFPRD        E_VHFPRD
048800170119     C                   MOVE      VAFNZD        E_VHFNZD
048900170119     C                   MOVE      VAFVAS        E_VHFVAS
049000170119     C                   MOVE      VAFIAS        E_VHFIAS
049100170119     C                   MOVE      VAFFPC        E_VHFFPC
049200170119     C                   MOVE      VAFPKC        E_VHFPKC
049300170119     C                   MOVE      VAFFVC        E_VHFFVC
049400170119     C                   MOVE      VAFVLC        E_VHFVLC
049500170119     C*
049600170119     C                   ENDSR
049700170119
049800170119
049900170119
050000170119     C     INZVAR        BEGSR
050100170119     C*
050200170119     C                   clear                   wRSD
050300170119     C                   clear                   wIND
050400170119     C                   clear                   wRSM
050500170119     C                   clear                   wINM
050600170119     C                   clear                   wCAM
050700170119     C                   clear                   wLOM
050800170119     C                   clear                   wPRM
050900170119     C                   clear                   wNZM
051000170119     C                   clear                   wRMO
051100170119     C                   clear                   wCMO
051200170119     C                   clear                   wNMO
051300170119     C                   clear                   wPVL_A
051400170119     C                   clear                   wPVL_E
051500170120     C                   clear                   wCCA
051600170120     C                   clear                   wDCM_A
051700170120     C                   clear                   wDCM_E
051800170119     C*
051900170119     C                   eval      WV07 = pSEP
052000170119     C                   eval      WV09 = pSEP
052100170119     C                   eval      WV13 = pSEP
052200170119     C                   eval      WV15 = pSEP
052300170119     C*
052400170119     C                   ENDSR
052500170119
052600170119
052700170119
052800170119     C     REPCIG        BEGSR
052900170119     C*
053000170119     C* Reperisco i dati del CUP e CIG (da TITA4)
053100170119     C                   CLEAR                   DTA44
053200170119     C                   EVAL      ta4TRC = '4'
053300170119     C     KEYta430C     CHAIN     tita430c
053400170119     C                   IF        %found(tita430c)
053500170119     C                   EVAL      DTA44 = ta4NOT
053600170119     C                   ENDIF
053700170119     C*
053800170119     C                   ENDSR
053900170119
054000170119
054100170119
054200170119     C     REPEXT        BEGSR
054300170119     C*
054400170119     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
054500170119     C     KEYtas30C     CHAIN     titas30c
054600170119     C                   IF        %found(titas30c)
054700170119     C*
054800170119     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
054900170119     C                   EVAL      wRSD = TASRSD
055000170119     C                   EVAL      wIND = TASIND
055100170119     C*
055200170119     C* Peso tassabile
055300170119     C                   Z-ADD     TASPVL        wPVL              7 1
055400170119     C                   MOVE      wPVL          WD09
055500170119     C                   MOVEL     wPVL          WI09
055600170119     C                   MOVE      WASC09        wPVL_A
055700170119     C                   MOVE      wPVL          wPVL_E
055800170120     C                   MOVEL     TASCCA        wCCA
055900170120     C                   MOVE      TASDCM        wDCM_A
056000170120     C                   MOVE      TASDCM        wDCM_E
056100170119     C*
056200170119     C* Per reperire i dati del mittente verifico il codice del cliente mittente:
056300170119     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
056400170119     C                   MOVE(P)   tasCCM        wCLI              4 0
056500170119     C                   IF        wCLI =    0 OR
056600170119     C                             wCLI = 8888 OR
056700170119     C                             wCLI = 9999
056800170119     C                   EVAL      taaTRC = 'M'
056900170119     C     KEYtaa30C     CHAIN     titaa30c
057000170119     C                   IF        %found(titaa30c)
057100170119     C                   EVAL      wRSM = TAARSC
057200170119     C                   EVAL      wINM = TAAIND
057300170119     C                   EVAL      wCAM = TAACAP
057400170119     C                   EVAL      wLOM = TAALOC
057500170119     C                   EVAL      wPRM = TAAPRV
057600170119     C                   EVAL      wNZM = TAANAZ
057700170119     C                   ENDIF
057800170119     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
057900170119     C                   ELSE
058000170119     C                   CLEAR                   BS69DS
058100170119     C                   CLEAR                   ACODS
058200170119     C                   CLEAR                   INDDS
058300170119     C                   CLEAR                   CLPDS
058400170119     C                   CLEAR                   CLSDS
058500170119     C                   Z-ADD     tasCCM        I69KAC
058600170119     C                   Z-ADD     tasCCM        I69KIN
058700170119     C                   CALL      'TIBS69R'
058800170119     C                   PARM                    BS69DS
058900170119     C                   PARM                    ACODS
059000170119     C                   PARM                    INDDS
059100170119     C                   PARM                    CLPDS
059200170119     C                   PARM                    CLSDS
059300170119     C     O69ERR        IFNE      '1'
059400170119     C                   EVAL      wRSM = ACORAG
059500170119     C                   EVAL      wINM = INDVIA
059600170119     C                   MOVEL(P)  INDCAP        wCAM
059700170119     C                   EVAL      wLOM = INDCIT
059800170119     C                   EVAL      wPRM = INDPRV
059900170119     C                   EVAL      wNZM = INDSTA
060000170119     C                   ENDIF
060100170119     C                   ENDIF
060200170119     C*
060300170119     C* Per reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O'
060400170119     C                   EVAL      taaTRC = 'O'
060500170119     C     KEYtaa30C     CHAIN     titaa30c
060600170119     C                   IF        %found(titaa30c)
060700170119     C                   EVAL      wRMO = TAARSC
060800170119     C                   EVAL      wCMO = TAACAP
060900170119     C                   EVAL      wNMO = TAANAZ
061000170119     C                   ENDIF
061100170119     C*
061200170119     C                   ENDIF
061300170119     C*
061400170119     C                   ENDSR
061500170119
061600170119
061700170119
061800040317      /TITLE Operazioni iniziali.
061900040317     C     *inzsr        BEGSR
062000040317     C*
062100040317     C     *ENTRY        PLIST
062200040317     C                   PARM                    prmppt
062300040317     C     wrkesito      PARM      wrkesito      prmesito
062400040317     C*
062500040317     C* Definizione chiavi
062600040317     C*
062700040317     C* Chiave su TITAS30C - Completa
062800040317     C     KEYtas30C     KLIST
062900040317     C                   KFLD                    vafAAS
063000040317     C                   KFLD                    vafLNP
063100040317     C                   KFLD                    vafNRS
063200040317     C                   KFLD                    vafNSP
063300040317     C                   KFLD                    vafTBL
063400040317     C*
063500040317     C* Chiave su TITAA30C - Completa
063600040317     C     KEYtaa30C     KLIST
063700040317     C                   KFLD                    vafAAS
063800040317     C                   KFLD                    vafLNP
063900040317     C                   KFLD                    vafNRS
064000040317     C                   KFLD                    vafNSP
064100040317     C                   KFLD                    taaTRC
064200120601     C*
064300120601     C* Chiave su TITA430C - Completa
064400120601     C     KEYta430C     KLIST
064500120601     C                   KFLD                    vafAAS
064600120601     C                   KFLD                    vafLNP
064700120601     C                   KFLD                    vafNRS
064800120601     C                   KFLD                    vafNSP
064900120601     C                   KFLD                    ta4TRC
065000040317     C*
065100040317     C                   ENDSR
