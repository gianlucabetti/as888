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
003300170119     D iBON            s               N   inz
003400170119     D pSEP            s              1A   inz
003500170119     D wRSD            s             35A   inz
003600170119     D wIND            s             35A   inz
003700170119     D wRSM            s             35A   inz
003800170119     D wINM            s             35A   inz
003900170119     D wCAM            s              9A   inz
004000170119     D wLOM            s             35A   inz
004100170119     D wPRM            s              2A   inz
004200170119     D wNZM            s              3A   inz
004300170119     D wRMO            s             35A   inz
004400170119     D wCMO            s              9A   inz
004500170119     D wNMO            s              3A   inz
004600170119     D wPVL_A          s              9A   inz
004700170119     D wPVL_E          s              7A   inz
004800170119
004900030623     D*-------------------
005000941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005100030623     D*-------------------
005200000125     D*  8+3
005300000125     D                 DS
005400000125     D  WS13                   1      1
005500000125     D  WI13                   2      9
005600170119     D  WV13                  10     10    inz(',')
005700000125     D  WD13                  11     13
005800000125     D  WASC13                 1     13
005900941221     D* 10+3
006000941221     D                 DS
006100941221     D  WS15                   1      1
006200941221     D  WI15                   2     11
006300170119     D  WV15                  12     12    inz(',')
006400941221     D  WD15                  13     15
006500941221     D  WASC15                 1     15
006600941221     D*  6+1
006700941221     D                 DS
006800941221     D  WS09                   1      1
006900941221     D  WI09                   2      7
007000170119     D  WV09                   8      8    inz(',')
007100941221     D  WD09                   9      9
007200941221     D  WASC09                 1      9
007300941221     D*  2+3
007400941221     D                 DS
007500941221     D  WS07                   1      1
007600941221     D  WI07                   2      3
007700170119     D  WV07                   4      4    inz(',')
007800941221     D  WD07                   5      7
007900941221     D  WASC07                 1      7
008000040317
008100040317     D*-------------------
008200040317     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
008300040317     D*-------------------
008400040317     D TxtInOut        S           2048
008500040317     D ElencoChar      S            256
008600040317     D TipoElenco      S              1
008700040317     D CharSost        S              1
008800040317     D UpperCase       S              1
008900040317     D ChkNull         S              1
009000040317     D CharNull        S              1
009100040317     D Esito           S              1
009200040317
009300040317
009400000904
009500170119     C*
009600170119     C* Avvio il monitoring del intero flusso
009700170119     C                   monitor
009800170119     C*
009900170119     C* Effettuo traduzione
010000991027     C                   EXSR      traduci
010100170119     C*
010200170119     C* Gestisco eventuale errore
010300170119     C                   on-error
010400170119     C*
010500170119     C* Imposto esito transazione a errore
010600170119     C                   EVAL      wrkesito = '2'
010700170119     C*
010800170119     C* Se errore => emetto operazione d ROLLBACK
010900170119     C                   ROLBK
011000170119     C*
011100170119     C* Arresto il monitoring
011200170119     C                   endmon
011300170119     C*
011400921023     C                   SETON                                        LR
011500170119
011600170119
011700170119
011800991027     C     traduci       BEGSR
011900170119     C*
012000170119     C* Inizializzazione indicatori di procedure per parametri input
012100170119     C                   eval      iASCI = *off
012200170119     C                   eval      iEBCD = *off
012300170119     C                   eval      iEXT  = *off
012400170119     C                   eval      iPVL  = *off
012500170119     C                   eval      iCIG  = *off
012600170119     C                   eval      iBON  = *off
012700170119     C*
012800170119     C* Se richiesto ritorno dati in formato EBCDIC altrimenti ASCII
012900170119     C                   if        %subst(prmppt:1:1) = 'S'
013000170119     C                   eval      iEBCD = *on
013100170119     C                   else
013200170119     C                   eval      iASCI = *on
013300170119     C                   endif
013400170119     C*
013500170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
013600170119     C                   if        %subst(prmppt:2:1) = 'S'
013700170119     C                   eval      iEXT = *on
013800170119     C                   endif
013900170119     C*
014000170119     C* Se richiesti dati di estensione relativi al Peso Tassabile
014100170119     C                   if        %subst(prmppt:3:1) = 'S'
014200170119     C                   eval      iPVL = *on
014300170119     C                   endif
014400170119     C*
014500170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
014600170119     C                   if        %subst(prmppt:4:1) = 'S'
014700170119     C                   eval      iCIG = *on
014800170119     C                   endif
014900170119     C*
015000170119     C* Verifico il carattere separatore decimale indicato nei parametri del cliente/tipo file
015100170119     C                   select
015200170119     C                   when      %subst(prmppt:5:1) = '.'
015300170119     C                   eval      pSEP = '.'
015400170119     C                   other
015500170119     C                   eval      pSEP = ','
015600170119     C                   endsl
015700170119     C*
015800170119     C* Se richiesto di "bonificare" i dati di output da eventuali caratteri "strani"
015900170119     C                   if        %subst(prmppt:7:1) = 'S'
016000170119     C                   eval      iBON = *on
016100170119     C                   endif
016200170119     C*
016300170119     C* Scorro tutto il file
016400170119     C                   read      tivgdtmp
016500170119     C                   dow       not %eof(tivgdtmp)
016600170119     C*
016700170119     C* Inizializzo DS di output
016800170119     C                   clear                   TIVAW000
016900170119     C                   clear                   tivafAsciDS
017000170119     C                   clear                   tivafEbcdDS
017100170119     C*
017200170119     C                   eval      tivafDS = vgdDTA
017300170119     C*
017400170119     C* Gestisco formato richiesto ASCII/EBCDIC
017500170119     C                   select
017600170119     C                   when      iASCI
017700170119     C                   exsr      fmtVAF_ASCI
017800170119     C                   when      iEBCD
017900170119     C                   exsr      fmtVAF_EBCD
018000170119     C                   other
018100170119     C                   eval      wrkesito = '2'
018200170119     C                   endsl
018300170119     C*
018400170119     C* Se tutto ok => procedo
018500170119     C                   if        wrkesito = *blanks
018600170119     C*
018700170119     C* Inizializzo eventuali dati possibilmente aggiuntivi
018800170119     C                   exsr      inzvar
018900170119     C*
019000170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
019100170119     C                   if        iEXT or iPVL
019200170119     C                   exsr      repext
019300170119     C                   endif
019400170119     C*
019500170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
019600170119     C                   if        iCIG
019700170119     C                   exsr      repcig
019800170119     C                   endif
019900170119     C*
020000170119     C* Scarico il buffer di output formato richiesto ASCII/EBCDIC
020100170119     C                   select
020200170119     C                   when      iASCI
020300170119     C                   eval      wVAWDTA = tivafAsciDS
020400170119     C                   when      iEBCD
020500170119     C                   eval      wVAWDTA = tivafEbcdDS
020600170119     C                   endsl
020700170119     C*
020800170119     C                   if        iEXT
020900170119     C                   eval      wVAWDTA = wVAWDTA + wRSD
021000170119     C                                               + wIND
021100170119     C                                               + wRSM
021200170119     C                                               + wINM
021300170119     C                                               + wCAM
021400170119     C                                               + wLOM
021500170119     C                                               + wPRM
021600170119     C                                               + wNZM
021700170119     C                                               + wRMO
021800170119     C                                               + wCMO
021900170119     C                                               + wNMO
022000170119     C                   endif
022100170119     C*
022200170119     C                   if        iPVL
022300170119     C                   select
022400170119     C                   when      iEBCD
022500170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_E
022600170119     C                   other
022700170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_A
022800170119     C                   endsl
022900170119     C                   endif
023000170119     C*
023100170119     C                   if        iCIG
023200170119     C                   eval      wVAWDTA = wVAWDTA + §TA4CUP
023300170119     C                                               + §TA4CIG
023400170119     C                   endif
023500170119     C*
023600170119     C                   if        iBON
023700170119     C                   eval      TxtInOut = wVAWDTA
023800170119     C                   exsr      exeBon
023900170119     C                   eval      wVAWDTA = TxtInOut
024000170119     C                   endif
024100170119     C*
024200170119     C                   eval      VAWDTA = wVAWDTA
024300170119     C                   write     TIVAW000
024400170119     C                   endif
024500170119     C*
024600170119     C* Proseguo con il record successivo
024700170119     C                   read      tivgdtmp
024800170119     C                   enddo
024900170119     C*
025000170119     C                   if        wrkesito <> '2'
025100170119     C                   eval      wrkesito  = '0'
025200170119     C                   endif
025300991027     C*
025400991027     C                   eval      wrkesito = '0'
025500991027     C*
025600910830     C                   ENDSR
025700170119
025800170119
025900170119
026000040317     C     exeBon        BEGSR
026100040317     C*
026200040317     C* Imposto i parametri x il *pgm d "bonifica"
026300040317     C                   EVAL      ElencoChar = *blanks
026400040317     C                   EVAL      TipoElenco = *blanks
026500040317     C                   EVAL      CharSost   = *blanks
026600040317     C                   EVAL      UpperCase  = *blanks
026700040317     C                   EVAL      ChkNull    = '1'
026800040317     C                   EVAL      CharNull   = *blanks
026900040317     C                   EVAL      Esito      = *blanks
027000040317     C*
027100040317     C* Effettuo la chiamata al *pgm d "bonifica"
027200040317     C                   CALL      'XCHKCHAR'
027300040317     C                   PARM                    TxtInOut
027400040317     C                   PARM                    ElencoChar
027500040317     C                   PARM                    TipoElenco
027600040317     C                   PARM                    CharSost
027700040317     C                   PARM                    UpperCase
027800040317     C                   PARM                    ChkNull
027900040317     C                   PARM                    CharNull
028000040317     C                   PARM                    Esito
028100030623     C*
028200991027     C                   ENDSR
028300170119
028400170119
028500170119
028600170119     C*
028700170119     C     fmtVAF_ASCI   BEGSR
028800170119     C*
028900170119     C                   MOVE      VAFAAS        A_VHFAAS
029000170119     C                   MOVE      VAFLNP        A_VHFLNP
029100170119     C                   MOVE      VAFNRS        A_VHFNRS
029200170119     C                   MOVE      VAFNSP        A_VHFNSP
029300170119     C                   MOVE      VAFTBL        A_VHFTBL
029400170119     C                   MOVE      VAFMGS        A_VHFMGS
029500170119     C                   MOVE      VAFKSC        A_VHFKSC
029600170119     C                   MOVE      VAFLNA        A_VHFLNA
029700170119     C                   MOVE      VAFNCL        A_VHFNCL
029800170119     C*
029900170119     C                   MOVE      VAFPKB        WD09
030000170119     C                   MOVEL     VAFPKB        WI09
030100170119     C                   MOVE      WASC09        A_VHFPKB
030200170119     C*
030300170119     C                   MOVE      VAFVLF        WD07
030400170119     C                   MOVEL     VAFVLF        WI07
030500170119     C                   MOVE      WASC07        A_VHFVLF
030600170119     C*
030700170119     C                   MOVE      VAFQFT        WD15
030800170119     C                   MOVEL     VAFQFT        WI15
030900170119     C                   MOVE      WASC15        A_VHFQFT
031000170119     C*
031100170119     C                   MOVE      VAFPOR        WD13
031200170119     C                   MOVEL     VAFPOR        WI13
031300170119     C                   MOVE      WASC13        A_VHFPOR
031400170119     C*
031500170119     C                   MOVE      VAFSV1        A_VHFSV1
031600170119     C                   MOVE      VAFVA1        WD13
031700170119     C                   MOVEL     VAFVA1        WI13
031800170119     C                   MOVE      WASC13        A_VHFVA1
031900170119     C*
032000170119     C                   MOVE      VAFSV2        A_VHFSV2
032100170119     C                   MOVE      VAFVA2        WD13
032200170119     C                   MOVEL     VAFVA2        WI13
032300170119     C                   MOVE      WASC13        A_VHFVA2
032400170119     C*
032500170119     C                   MOVE      VAFSV3        A_VHFSV3
032600170119     C                   MOVE      VAFVA3        WD13
032700170119     C                   MOVEL     VAFVA3        WI13
032800170119     C                   MOVE      WASC13        A_VHFVA3
032900170119     C*
033000170119     C                   MOVE      VAFSV4        A_VHFSV4
033100170119     C                   MOVE      VAFVA4        WD13
033200170119     C                   MOVEL     VAFVA4        WI13
033300170119     C                   MOVE      WASC13        A_VHFVA4
033400170119     C*
033500170119     C                   MOVE      VAFSV5        A_VHFSV5
033600170119     C                   MOVE      VAFVA5        WD13
033700170119     C                   MOVEL     VAFVA5        WI13
033800170119     C                   MOVE      WASC13        A_VHFVA5
033900170119     C*
034000170119     C                   MOVE      VAFSV6        A_VHFSV6
034100170119     C                   MOVE      VAFVA6        WD13
034200170119     C                   MOVEL     VAFVA6        WI13
034300170119     C                   MOVE      WASC13        A_VHFVA6
034400170119     C*
034500170119     C                   MOVE      VAFSV7        A_VHFSV7
034600170119     C                   MOVE      VAFVA7        WD13
034700170119     C                   MOVEL     VAFVA7        WI13
034800170119     C                   MOVE      WASC13        A_VHFVA7
034900170119     C*
035000170119     C                   MOVE      VAFSV8        A_VHFSV8
035100170119     C                   MOVE      VAFVA8        WD13
035200170119     C                   MOVEL     VAFVA8        WI13
035300170119     C                   MOVE      WASC13        A_VHFVA8
035400170119     C*
035500170119     C                   MOVE      VAFVAX        WD13
035600170119     C                   MOVEL     VAFVAX        WI13
035700170119     C                   MOVE      WASC13        A_VHFVAX
035800170119     C*
035900170119     C                   MOVE      VAFIMV        WD15
036000170119     C                   MOVEL     VAFIMV        WI15
036100170119     C                   MOVE      WASC15        A_VHFIMV
036200170119     C*
036300170119     C                   MOVE      VAFDFT        A_VHFDFT
036400170119     C                   MOVE      VAFNFT        A_VHFNFT
036500170119     C                   MOVE      VAFFIV        A_VHFFIV
036600170119     C                   MOVE      VAFDIV        A_VHFDIV
036700170119     C                   MOVE      VAFDRT        A_VHFDRT
036800170119     C                   MOVE      VAFRMN        A_VHFRMN
036900170119     C                   MOVE      VAFRMA        A_VHFRMA
037000170119     C                   MOVE      VAFRMX        A_VHFRMX
037100170119     C                   MOVE      VAFFTC        A_VHFFTC
037200170119     C                   MOVE      VAFTC2        A_VHFTC2
037300170119     C                   MOVE      VAFTSP        A_VHFTSP
037400170119     C                   MOVE      VAFFAP        A_VHFFAP
037500170119     C                   MOVE      VAFFIN        A_VHFFIN
037600170119     C                   MOVE      VAFCTR        A_VHFCTR
037700170119     C                   MOVE      VAFNAS        A_VHFNAS
037800170119     C                   MOVE      VAFCTS        A_VHFCTS
037900170119     C                   MOVE      VAFCAD        A_VHFCAD
038000170119     C                   MOVE      VAFLOD        A_VHFLOD
038100170119     C                   MOVE      VAFPRD        A_VHFPRD
038200170119     C                   MOVE      VAFNZD        A_VHFNZD
038300170119     C                   MOVE      VAFVAS        A_VHFVAS
038400170119     C*
038500170119     C                   MOVE      VAFIAS        WD15
038600170119     C                   MOVEL     VAFIAS        WI15
038700170119     C                   MOVE      WASC15        A_VHFIAS
038800170119     C*
038900170119     C                   MOVE      VAFFPC        A_VHFFPC
039000170119     C*
039100170119     C                   MOVE      VAFPKC        WD09
039200170119     C                   MOVEL     VAFPKC        WI09
039300170119     C                   MOVE      WASC09        A_VHFPKC
039400170119     C*
039500170119     C                   MOVE      VAFFVC        A_VHFFVC
039600170119     C*
039700170119     C                   MOVE      VAFVLC        WD07
039800170119     C                   MOVEL     VAFVLC        WI07
039900170119     C                   MOVE      WASC07        A_VHFVLC
040000170119     C*
040100170119     C                   ENDSR
040200170119
040300170119
040400170119
040500170119     C*
040600170119     C     fmtVAF_EBCD   BEGSR
040700170119     C*
040800170119     C                   MOVE      VAFAAS        E_VHFAAS
040900170119     C                   MOVE      VAFLNP        E_VHFLNP
041000170119     C                   MOVE      VAFNRS        E_VHFNRS
041100170119     C                   MOVE      VAFNSP        E_VHFNSP
041200170119     C                   MOVE      VAFTBL        E_VHFTBL
041300170119     C                   MOVE      VAFMGS        E_VHFMGS
041400170119     C                   MOVE      VAFKSC        E_VHFKSC
041500170119     C                   MOVE      VAFLNA        E_VHFLNA
041600170119     C                   MOVE      VAFNCL        E_VHFNCL
041700170119     C                   MOVE      VAFPKB        E_VHFPKB
041800170119     C                   MOVE      VAFVLF        E_VHFVLF
041900170119     C                   MOVE      VAFQFT        E_VHFQFT
042000170119     C                   MOVE      VAFPOR        E_VHFPOR
042100170119     C                   MOVE      VAFSV1        E_VHFSV1
042200170119     C                   MOVE      VAFVA1        E_VHFVA1
042300170119     C                   MOVE      VAFSV2        E_VHFSV2
042400170119     C                   MOVE      VAFVA2        E_VHFVA2
042500170119     C                   MOVE      VAFSV3        E_VHFSV3
042600170119     C                   MOVE      VAFVA3        E_VHFVA3
042700170119     C                   MOVE      VAFSV4        E_VHFSV4
042800170119     C                   MOVE      VAFVA4        E_VHFVA4
042900170119     C                   MOVE      VAFSV5        E_VHFSV5
043000170119     C                   MOVE      VAFVA5        E_VHFVA5
043100170119     C                   MOVE      VAFSV6        E_VHFSV6
043200170119     C                   MOVE      VAFVA6        E_VHFVA6
043300170119     C                   MOVE      VAFSV7        E_VHFSV7
043400170119     C                   MOVE      VAFVA7        E_VHFVA7
043500170119     C                   MOVE      VAFSV8        E_VHFSV8
043600170119     C                   MOVE      VAFVA8        E_VHFVA8
043700170119     C                   MOVE      VAFVAX        E_VHFVAX
043800170119     C                   MOVE      VAFIMV        E_VHFIMV
043900170119     C                   MOVE      VAFDFT        E_VHFDFT
044000170119     C                   MOVE      VAFNFT        E_VHFNFT
044100170119     C                   MOVE      VAFFIV        E_VHFFIV
044200170119     C                   MOVE      VAFDIV        E_VHFDIV
044300170119     C                   MOVE      VAFDRT        E_VHFDRT
044400170119     C                   MOVE      VAFRMN        E_VHFRMN
044500170119     C                   MOVE      VAFRMA        E_VHFRMA
044600170119     C                   MOVE      VAFRMX        E_VHFRMX
044700170119     C                   MOVE      VAFFTC        E_VHFFTC
044800170119     C                   MOVE      VAFTC2        E_VHFTC2
044900170119     C                   MOVE      VAFTSP        E_VHFTSP
045000170119     C                   MOVE      VAFFAP        E_VHFFAP
045100170119     C                   MOVE      VAFFIN        E_VHFFIN
045200170119     C                   MOVE      VAFCTR        E_VHFCTR
045300170119     C                   MOVE      VAFNAS        E_VHFNAS
045400170119     C                   MOVE      VAFCTS        E_VHFCTS
045500170119     C                   MOVE      VAFCAD        E_VHFCAD
045600170119     C                   MOVE      VAFLOD        E_VHFLOD
045700170119     C                   MOVE      VAFPRD        E_VHFPRD
045800170119     C                   MOVE      VAFNZD        E_VHFNZD
045900170119     C                   MOVE      VAFVAS        E_VHFVAS
046000170119     C                   MOVE      VAFIAS        E_VHFIAS
046100170119     C                   MOVE      VAFFPC        E_VHFFPC
046200170119     C                   MOVE      VAFPKC        E_VHFPKC
046300170119     C                   MOVE      VAFFVC        E_VHFFVC
046400170119     C                   MOVE      VAFVLC        E_VHFVLC
046500170119     C*
046600170119     C                   ENDSR
046700170119
046800170119
046900170119
047000170119     C     INZVAR        BEGSR
047100170119     C*
047200170119     C                   clear                   wRSD
047300170119     C                   clear                   wIND
047400170119     C                   clear                   wRSM
047500170119     C                   clear                   wINM
047600170119     C                   clear                   wCAM
047700170119     C                   clear                   wLOM
047800170119     C                   clear                   wPRM
047900170119     C                   clear                   wNZM
048000170119     C                   clear                   wRMO
048100170119     C                   clear                   wCMO
048200170119     C                   clear                   wNMO
048300170119     C                   clear                   wPVL_A
048400170119     C                   clear                   wPVL_E
048500170119     C*
048600170119     C                   eval      WV07 = pSEP
048700170119     C                   eval      WV09 = pSEP
048800170119     C                   eval      WV13 = pSEP
048900170119     C                   eval      WV15 = pSEP
049000170119     C*
049100170119     C                   ENDSR
049200170119
049300170119
049400170119
049500170119     C     REPCIG        BEGSR
049600170119     C*
049700170119     C* Reperisco i dati del CUP e CIG (da TITA4)
049800170119     C                   CLEAR                   DTA44
049900170119     C                   EVAL      ta4TRC = '4'
050000170119     C     KEYta430C     CHAIN     tita430c
050100170119     C                   IF        %found(tita430c)
050200170119     C                   EVAL      DTA44 = ta4NOT
050300170119     C                   ENDIF
050400170119     C*
050500170119     C                   ENDSR
050600170119
050700170119
050800170119
050900170119     C     REPEXT        BEGSR
051000170119     C*
051100170119     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
051200170119     C     KEYtas30C     CHAIN     titas30c
051300170119     C                   IF        %found(titas30c)
051400170119     C*
051500170119     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
051600170119     C                   EVAL      wRSD = TASRSD
051700170119     C                   EVAL      wIND = TASIND
051800170119     C*
051900170119     C* Peso tassabile
052000170119     C                   Z-ADD     TASPVL        wPVL              7 1
052100170119     C                   MOVE      wPVL          WD09
052200170119     C                   MOVEL     wPVL          WI09
052300170119     C                   MOVE      WASC09        wPVL_A
052400170119     C                   MOVE      wPVL          wPVL_E
052500170119     C*
052600170119     C* Per reperire i dati del mittente verifico il codice del cliente mittente:
052700170119     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
052800170119     C                   MOVE(P)   tasCCM        wCLI              4 0
052900170119     C                   IF        wCLI =    0 OR
053000170119     C                             wCLI = 8888 OR
053100170119     C                             wCLI = 9999
053200170119     C                   EVAL      taaTRC = 'M'
053300170119     C     KEYtaa30C     CHAIN     titaa30c
053400170119     C                   IF        %found(titaa30c)
053500170119     C                   EVAL      wRSM = TAARSC
053600170119     C                   EVAL      wINM = TAAIND
053700170119     C                   EVAL      wCAM = TAACAP
053800170119     C                   EVAL      wLOM = TAALOC
053900170119     C                   EVAL      wPRM = TAAPRV
054000170119     C                   EVAL      wNZM = TAANAZ
054100170119     C                   ENDIF
054200170119     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
054300170119     C                   ELSE
054400170119     C                   CLEAR                   BS69DS
054500170119     C                   CLEAR                   ACODS
054600170119     C                   CLEAR                   INDDS
054700170119     C                   CLEAR                   CLPDS
054800170119     C                   CLEAR                   CLSDS
054900170119     C                   Z-ADD     tasCCM        I69KAC
055000170119     C                   Z-ADD     tasCCM        I69KIN
055100170119     C                   CALL      'TIBS69R'
055200170119     C                   PARM                    BS69DS
055300170119     C                   PARM                    ACODS
055400170119     C                   PARM                    INDDS
055500170119     C                   PARM                    CLPDS
055600170119     C                   PARM                    CLSDS
055700170119     C     O69ERR        IFNE      '1'
055800170119     C                   EVAL      wRSM = ACORAG
055900170119     C                   EVAL      wINM = INDVIA
056000170119     C                   MOVEL(P)  INDCAP        wCAM
056100170119     C                   EVAL      wLOM = INDCIT
056200170119     C                   EVAL      wPRM = INDPRV
056300170119     C                   EVAL      wNZM = INDSTA
056400170119     C                   ENDIF
056500170119     C                   ENDIF
056600170119     C*
056700170119     C* Per reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O'
056800170119     C                   EVAL      taaTRC = 'O'
056900170119     C     KEYtaa30C     CHAIN     titaa30c
057000170119     C                   IF        %found(titaa30c)
057100170119     C                   EVAL      wRMO = TAARSC
057200170119     C                   EVAL      wCMO = TAACAP
057300170119     C                   EVAL      wNMO = TAANAZ
057400170119     C                   ENDIF
057500170119     C*
057600170119     C                   ENDIF
057700170119     C*
057800170119     C                   ENDSR
057900170119
058000170119
058100170119
058200040317      /TITLE Operazioni iniziali.
058300040317     C     *inzsr        BEGSR
058400040317     C*
058500040317     C     *ENTRY        PLIST
058600040317     C                   PARM                    prmppt
058700040317     C     wrkesito      PARM      wrkesito      prmesito
058800040317     C*
058900040317     C* Definizione chiavi
059000040317     C*
059100040317     C* Chiave su TITAS30C - Completa
059200040317     C     KEYtas30C     KLIST
059300040317     C                   KFLD                    vafAAS
059400040317     C                   KFLD                    vafLNP
059500040317     C                   KFLD                    vafNRS
059600040317     C                   KFLD                    vafNSP
059700040317     C                   KFLD                    vafTBL
059800040317     C*
059900040317     C* Chiave su TITAA30C - Completa
060000040317     C     KEYtaa30C     KLIST
060100040317     C                   KFLD                    vafAAS
060200040317     C                   KFLD                    vafLNP
060300040317     C                   KFLD                    vafNRS
060400040317     C                   KFLD                    vafNSP
060500040317     C                   KFLD                    taaTRC
060600120601     C*
060700120601     C* Chiave su TITA430C - Completa
060800120601     C     KEYta430C     KLIST
060900120601     C                   KFLD                    vafAAS
061000120601     C                   KFLD                    vafLNP
061100120601     C                   KFLD                    vafNRS
061200120601     C                   KFLD                    vafNSP
061300120601     C                   KFLD                    ta4TRC
061400040317     C*
061500040317     C                   ENDSR
