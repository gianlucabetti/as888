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
003600170406     D iFIN_I          s               N   inz
003700170119     D pSEP            s              1A   inz
003800170119     D wRSD            s             35A   inz
003900170119     D wIND            s             35A   inz
004000170119     D wRSM            s             35A   inz
004100170119     D wINM            s             35A   inz
004200170119     D wCAM            s              9A   inz
004300170119     D wLOM            s             35A   inz
004400170119     D wPRM            s              2A   inz
004500170119     D wNZM            s              3A   inz
004600170119     D wRMO            s             35A   inz
004700170119     D wCMO            s              9A   inz
004800170119     D wNMO            s              3A   inz
004900170119     D wPVL_A          s              9A   inz
005000170119     D wPVL_E          s              7A   inz
005100170120     D wCCA            s              1A   inz
005200170120     D wDCM_A          s              9A   inz
005300170120     D wDCM_E          s              8A   inz
005400170119
005500030623     D*-------------------
005600941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
005700030623     D*-------------------
005800000125     D*  8+3
005900000125     D                 DS
006000000125     D  WS13                   1      1
006100000125     D  WI13                   2      9
006200170119     D  WV13                  10     10    inz(',')
006300000125     D  WD13                  11     13
006400000125     D  WASC13                 1     13
006500941221     D* 10+3
006600941221     D                 DS
006700941221     D  WS15                   1      1
006800941221     D  WI15                   2     11
006900170119     D  WV15                  12     12    inz(',')
007000941221     D  WD15                  13     15
007100941221     D  WASC15                 1     15
007200941221     D*  6+1
007300941221     D                 DS
007400941221     D  WS09                   1      1
007500941221     D  WI09                   2      7
007600170119     D  WV09                   8      8    inz(',')
007700941221     D  WD09                   9      9
007800941221     D  WASC09                 1      9
007900941221     D*  2+3
008000941221     D                 DS
008100941221     D  WS07                   1      1
008200941221     D  WI07                   2      3
008300170119     D  WV07                   4      4    inz(',')
008400941221     D  WD07                   5      7
008500941221     D  WASC07                 1      7
008600040317
008700040317     D*-------------------
008800040317     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
008900040317     D*-------------------
009000040317     D TxtInOut        S           2048
009100040317     D ElencoChar      S            256
009200040317     D TipoElenco      S              1
009300040317     D CharSost        S              1
009400040317     D UpperCase       S              1
009500040317     D ChkNull         S              1
009600040317     D CharNull        S              1
009700040317     D Esito           S              1
009800040317
009900040317
010000000904
010100170119     C*
010200170119     C* Avvio il monitoring del intero flusso
010300170119     C                   monitor
010400170119     C*
010500170119     C* Effettuo traduzione
010600991027     C                   EXSR      traduci
010700170119     C*
010800170119     C* Gestisco eventuale errore
010900170119     C                   on-error
011000170119     C*
011100170119     C* Imposto esito transazione a errore
011200170119     C                   EVAL      wrkesito = '2'
011300170119     C*
011400170119     C* Se errore => emetto operazione d ROLLBACK
011500170119     C                   ROLBK
011600170119     C*
011700170119     C* Arresto il monitoring
011800170119     C                   endmon
011900170119     C*
012000921023     C                   SETON                                        LR
012100170119
012200170119
012300170119
012400991027     C     traduci       BEGSR
012500170119     C*
012600170119     C* Inizializzazione indicatori di procedure per parametri input
012700170119     C                   eval      iASCI = *off
012800170119     C                   eval      iEBCD = *off
012900170119     C                   eval      iEXT  = *off
013000170119     C                   eval      iPVL  = *off
013100170119     C                   eval      iCIG  = *off
013200170120     C                   eval      iDCM  = *off
013300170119     C                   eval      iBON  = *off
013400170206     C                   eval      iPUNTO= *off
013500170406     C                   eval      iFIN_I= *off
013600170119     C*
013700170119     C* Se richiesto ritorno dati in formato EBCDIC altrimenti ASCII
013800170119     C                   if        %subst(prmppt:1:1) = 'S'
013900170119     C                   eval      iEBCD = *on
014000170119     C                   else
014100170119     C                   eval      iASCI = *on
014200170119     C                   endif
014300170119     C*
014400170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
014500170119     C                   if        %subst(prmppt:2:1) = 'S'
014600170119     C                   eval      iEXT = *on
014700170119     C                   endif
014800170119     C*
014900170119     C* Se richiesti dati di estensione relativi al Peso Tassabile
015000170119     C                   if        %subst(prmppt:3:1) = 'S'
015100170119     C                   eval      iPVL = *on
015200170119     C                   endif
015300170119     C*
015400170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
015500170119     C                   if        %subst(prmppt:4:1) = 'S'
015600170119     C                   eval      iCIG = *on
015700170119     C                   endif
015800170119     C*
015900170119     C* Verifico il carattere separatore decimale indicato nei parametri del cliente/tipo file
016000170119     C                   select
016100170119     C                   when      %subst(prmppt:5:1) = '.'
016200170119     C                   eval      pSEP = '.'
016300170119     C                   other
016400170119     C                   eval      pSEP = ','
016500170119     C                   endsl
016600170120     C*
016700170120     C* Se richiesti dati di estensione relativi al Codice Cons. Anomala e Data Consegna Merce
016800170120     C                   if        %subst(prmppt:6:1) = 'S'
016900170120     C                   eval      iDCM = *on
017000170120     C                   endif
017100170119     C*
017200170119     C* Se richiesto di "bonificare" i dati di output da eventuali caratteri "strani"
017300170119     C                   if        %subst(prmppt:7:1) = 'S'
017400170119     C                   eval      iBON = *on
017500170119     C                   endif
017600170206     C*
017700170206     C* Se richiesto di "terminare" i dati di output con un punto
017800170206     C                   if        %subst(prmppt:8:1) = 'S'
017900170206     C                   eval      iPUNTO = *on
018000170206     C                   endif
018100170406     C*
018200170406     C* Se richiesto filtro per DPD solo Tipo Inoltro Isole
018300170406     C                   if        %subst(prmppt:9:1) = 'I'
018400170406     C                   eval      iFIN_I = *on
018500170406     C                   endif
018600170119     C*
018700170119     C* Scorro tutto il file
018800170119     C                   read      tivgdtmp
018900170119     C                   dow       not %eof(tivgdtmp)
019000170406     C*
019100170406     C* Inizializzo indicatore di "filtro" scrittura output
019200170406     C                   setoff                                       31
019300170119     C*
019400170119     C* Inizializzo DS di output
019500170119     C                   clear                   TIVAW000
019600170119     C                   clear                   tivafAsciDS
019700170119     C                   clear                   tivafEbcdDS
019800170119     C*
019900170119     C                   eval      tivafDS = vgdDTA
020000170406     C*
020100170406     C* Come primissima cosa verifico eventuali filtri richiesti
020200170406     C* - filtro per solo Isole
020300170406     C                   if        iFIN_I AND vafFIN <> 'I'
020400170406     C                   seton                                        31
020500170406     C                   endif
020600170406     C*
020700170406     C* Se record corrente valido ai fini "filtri"
020800170406     C                   if        not *in31
020900170119     C*
021000170119     C* Gestisco formato richiesto ASCII/EBCDIC
021100170119     C                   select
021200170119     C                   when      iASCI
021300170119     C                   exsr      fmtVAF_ASCI
021400170119     C                   when      iEBCD
021500170119     C                   exsr      fmtVAF_EBCD
021600170119     C                   other
021700170119     C                   eval      wrkesito = '2'
021800170119     C                   endsl
021900170119     C*
022000170119     C* Se tutto ok => procedo
022100170119     C                   if        wrkesito = *blanks
022200170119     C*
022300170119     C* Inizializzo eventuali dati possibilmente aggiuntivi
022400170119     C                   exsr      inzvar
022500170119     C*
022600170119     C* Se richiesti dati di estensione relativi al Destinatario / Mittente / Mittente Originale
022700170120     C                   if        iEXT or iPVL or iDCM
022800170119     C                   exsr      repext
022900170119     C                   endif
023000170119     C*
023100170119     C* Se richiesti dati di estensione relativi alla tracciabilità fiscale CUP/CIG
023200170119     C                   if        iCIG
023300170119     C                   exsr      repcig
023400170119     C                   endif
023500170119     C*
023600170119     C* Scarico il buffer di output formato richiesto ASCII/EBCDIC
023700170119     C                   select
023800170119     C                   when      iASCI
023900170119     C                   eval      wVAWDTA = tivafAsciDS
024000170119     C                   when      iEBCD
024100170119     C                   eval      wVAWDTA = tivafEbcdDS
024200170119     C                   endsl
024300170119     C*
024400170119     C                   if        iEXT
024500170119     C                   eval      wVAWDTA = wVAWDTA + wRSD
024600170119     C                                               + wIND
024700170119     C                                               + wRSM
024800170119     C                                               + wINM
024900170119     C                                               + wCAM
025000170119     C                                               + wLOM
025100170119     C                                               + wPRM
025200170119     C                                               + wNZM
025300170119     C                                               + wRMO
025400170119     C                                               + wCMO
025500170119     C                                               + wNMO
025600170119     C                   endif
025700170119     C*
025800170119     C                   if        iPVL
025900170119     C                   select
026000170119     C                   when      iEBCD
026100170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_E
026200170119     C                   other
026300170119     C                   eval      wVAWDTA = wVAWDTA + wPVL_A
026400170119     C                   endsl
026500170119     C                   endif
026600170119     C*
026700170119     C                   if        iCIG
026800170119     C                   eval      wVAWDTA = wVAWDTA + §TA4CUP
026900170119     C                                               + §TA4CIG
027000170119     C                   endif
027100170120     C*
027200170120     C                   if        iDCM
027300170120     C                   select
027400170120     C                   when      iEBCD
027500170120     C                   eval      wVAWDTA = wVAWDTA + wCCA + wDCM_E
027600170120     C                   other
027700170120     C                   eval      wVAWDTA = wVAWDTA + wCCA + wDCM_A
027800170120     C                   endsl
027900170120     C                   endif
028000170119     C*
028100170119     C                   if        iBON
028200170119     C                   eval      TxtInOut = wVAWDTA
028300170119     C                   exsr      exeBon
028400170119     C                   eval      wVAWDTA = TxtInOut
028500170119     C                   endif
028600170206     C*
028700170206     C                   if        iPUNTO
028800170206     C                   eval      wVAWDTA = wVAWDTA + '.'
028900170206     C                   endif
029000170119     C*
029100170119     C                   eval      VAWDTA = wVAWDTA
029200170119     C                   write     TIVAW000
029300170119     C                   endif
029400170406     C*
029500170406     C                   endif
029600170119     C*
029700170119     C* Proseguo con il record successivo
029800170119     C                   read      tivgdtmp
029900170119     C                   enddo
030000170119     C*
030100170119     C                   if        wrkesito <> '2'
030200170119     C                   eval      wrkesito  = '0'
030300170119     C                   endif
030400991027     C*
030500991027     C                   eval      wrkesito = '0'
030600991027     C*
030700910830     C                   ENDSR
030800170119
030900170119
031000170119
031100040317     C     exeBon        BEGSR
031200040317     C*
031300040317     C* Imposto i parametri x il *pgm d "bonifica"
031400040317     C                   EVAL      ElencoChar = *blanks
031500040317     C                   EVAL      TipoElenco = *blanks
031600040317     C                   EVAL      CharSost   = *blanks
031700040317     C                   EVAL      UpperCase  = *blanks
031800040317     C                   EVAL      ChkNull    = '1'
031900040317     C                   EVAL      CharNull   = *blanks
032000040317     C                   EVAL      Esito      = *blanks
032100040317     C*
032200040317     C* Effettuo la chiamata al *pgm d "bonifica"
032300040317     C                   CALL      'XCHKCHAR'
032400040317     C                   PARM                    TxtInOut
032500040317     C                   PARM                    ElencoChar
032600040317     C                   PARM                    TipoElenco
032700040317     C                   PARM                    CharSost
032800040317     C                   PARM                    UpperCase
032900040317     C                   PARM                    ChkNull
033000040317     C                   PARM                    CharNull
033100040317     C                   PARM                    Esito
033200030623     C*
033300991027     C                   ENDSR
033400170119
033500170119
033600170119
033700170119     C*
033800170119     C     fmtVAF_ASCI   BEGSR
033900170119     C*
034000170119     C                   MOVE      VAFAAS        A_VHFAAS
034100170119     C                   MOVE      VAFLNP        A_VHFLNP
034200170119     C                   MOVE      VAFNRS        A_VHFNRS
034300170119     C                   MOVE      VAFNSP        A_VHFNSP
034400170119     C                   MOVE      VAFTBL        A_VHFTBL
034500170119     C                   MOVE      VAFMGS        A_VHFMGS
034600170119     C                   MOVE      VAFKSC        A_VHFKSC
034700170119     C                   MOVE      VAFLNA        A_VHFLNA
034800170119     C                   MOVE      VAFNCL        A_VHFNCL
034900170119     C*
035000170119     C                   MOVE      VAFPKB        WD09
035100170119     C                   MOVEL     VAFPKB        WI09
035200170119     C                   MOVE      WASC09        A_VHFPKB
035300170119     C*
035400170119     C                   MOVE      VAFVLF        WD07
035500170119     C                   MOVEL     VAFVLF        WI07
035600170119     C                   MOVE      WASC07        A_VHFVLF
035700170119     C*
035800170119     C                   MOVE      VAFQFT        WD15
035900170119     C                   MOVEL     VAFQFT        WI15
036000170119     C                   MOVE      WASC15        A_VHFQFT
036100170119     C*
036200170119     C                   MOVE      VAFPOR        WD13
036300170119     C                   MOVEL     VAFPOR        WI13
036400170119     C                   MOVE      WASC13        A_VHFPOR
036500170119     C*
036600170119     C                   MOVE      VAFSV1        A_VHFSV1
036700170119     C                   MOVE      VAFVA1        WD13
036800170119     C                   MOVEL     VAFVA1        WI13
036900170119     C                   MOVE      WASC13        A_VHFVA1
037000170119     C*
037100170119     C                   MOVE      VAFSV2        A_VHFSV2
037200170119     C                   MOVE      VAFVA2        WD13
037300170119     C                   MOVEL     VAFVA2        WI13
037400170119     C                   MOVE      WASC13        A_VHFVA2
037500170119     C*
037600170119     C                   MOVE      VAFSV3        A_VHFSV3
037700170119     C                   MOVE      VAFVA3        WD13
037800170119     C                   MOVEL     VAFVA3        WI13
037900170119     C                   MOVE      WASC13        A_VHFVA3
038000170119     C*
038100170119     C                   MOVE      VAFSV4        A_VHFSV4
038200170119     C                   MOVE      VAFVA4        WD13
038300170119     C                   MOVEL     VAFVA4        WI13
038400170119     C                   MOVE      WASC13        A_VHFVA4
038500170119     C*
038600170119     C                   MOVE      VAFSV5        A_VHFSV5
038700170119     C                   MOVE      VAFVA5        WD13
038800170119     C                   MOVEL     VAFVA5        WI13
038900170119     C                   MOVE      WASC13        A_VHFVA5
039000170119     C*
039100170119     C                   MOVE      VAFSV6        A_VHFSV6
039200170119     C                   MOVE      VAFVA6        WD13
039300170119     C                   MOVEL     VAFVA6        WI13
039400170119     C                   MOVE      WASC13        A_VHFVA6
039500170119     C*
039600170119     C                   MOVE      VAFSV7        A_VHFSV7
039700170119     C                   MOVE      VAFVA7        WD13
039800170119     C                   MOVEL     VAFVA7        WI13
039900170119     C                   MOVE      WASC13        A_VHFVA7
040000170119     C*
040100170119     C                   MOVE      VAFSV8        A_VHFSV8
040200170119     C                   MOVE      VAFVA8        WD13
040300170119     C                   MOVEL     VAFVA8        WI13
040400170119     C                   MOVE      WASC13        A_VHFVA8
040500170119     C*
040600170119     C                   MOVE      VAFVAX        WD13
040700170119     C                   MOVEL     VAFVAX        WI13
040800170119     C                   MOVE      WASC13        A_VHFVAX
040900170119     C*
041000170119     C                   MOVE      VAFIMV        WD15
041100170119     C                   MOVEL     VAFIMV        WI15
041200170119     C                   MOVE      WASC15        A_VHFIMV
041300170119     C*
041400170119     C                   MOVE      VAFDFT        A_VHFDFT
041500170119     C                   MOVE      VAFNFT        A_VHFNFT
041600170119     C                   MOVE      VAFFIV        A_VHFFIV
041700170119     C                   MOVE      VAFDIV        A_VHFDIV
041800170119     C                   MOVE      VAFDRT        A_VHFDRT
041900170119     C                   MOVE      VAFRMN        A_VHFRMN
042000170119     C                   MOVE      VAFRMA        A_VHFRMA
042100170119     C                   MOVE      VAFRMX        A_VHFRMX
042200170119     C                   MOVE      VAFFTC        A_VHFFTC
042300170119     C                   MOVE      VAFTC2        A_VHFTC2
042400170119     C                   MOVE      VAFTSP        A_VHFTSP
042500170119     C                   MOVE      VAFFAP        A_VHFFAP
042600170119     C                   MOVE      VAFFIN        A_VHFFIN
042700170119     C                   MOVE      VAFCTR        A_VHFCTR
042800170119     C                   MOVE      VAFNAS        A_VHFNAS
042900170119     C                   MOVE      VAFCTS        A_VHFCTS
043000170119     C                   MOVE      VAFCAD        A_VHFCAD
043100170119     C                   MOVE      VAFLOD        A_VHFLOD
043200170119     C                   MOVE      VAFPRD        A_VHFPRD
043300170119     C                   MOVE      VAFNZD        A_VHFNZD
043400170119     C                   MOVE      VAFVAS        A_VHFVAS
043500170119     C*
043600170119     C                   MOVE      VAFIAS        WD15
043700170119     C                   MOVEL     VAFIAS        WI15
043800170119     C                   MOVE      WASC15        A_VHFIAS
043900170119     C*
044000170119     C                   MOVE      VAFFPC        A_VHFFPC
044100170119     C*
044200170119     C                   MOVE      VAFPKC        WD09
044300170119     C                   MOVEL     VAFPKC        WI09
044400170119     C                   MOVE      WASC09        A_VHFPKC
044500170119     C*
044600170119     C                   MOVE      VAFFVC        A_VHFFVC
044700170119     C*
044800170119     C                   MOVE      VAFVLC        WD07
044900170119     C                   MOVEL     VAFVLC        WI07
045000170119     C                   MOVE      WASC07        A_VHFVLC
045100170119     C*
045200170119     C                   ENDSR
045300170119
045400170119
045500170119
045600170119     C*
045700170119     C     fmtVAF_EBCD   BEGSR
045800170119     C*
045900170119     C                   MOVE      VAFAAS        E_VHFAAS
046000170119     C                   MOVE      VAFLNP        E_VHFLNP
046100170119     C                   MOVE      VAFNRS        E_VHFNRS
046200170119     C                   MOVE      VAFNSP        E_VHFNSP
046300170119     C                   MOVE      VAFTBL        E_VHFTBL
046400170119     C                   MOVE      VAFMGS        E_VHFMGS
046500170119     C                   MOVE      VAFKSC        E_VHFKSC
046600170119     C                   MOVE      VAFLNA        E_VHFLNA
046700170119     C                   MOVE      VAFNCL        E_VHFNCL
046800170119     C                   MOVE      VAFPKB        E_VHFPKB
046900170119     C                   MOVE      VAFVLF        E_VHFVLF
047000170119     C                   MOVE      VAFQFT        E_VHFQFT
047100170119     C                   MOVE      VAFPOR        E_VHFPOR
047200170119     C                   MOVE      VAFSV1        E_VHFSV1
047300170119     C                   MOVE      VAFVA1        E_VHFVA1
047400170119     C                   MOVE      VAFSV2        E_VHFSV2
047500170119     C                   MOVE      VAFVA2        E_VHFVA2
047600170119     C                   MOVE      VAFSV3        E_VHFSV3
047700170119     C                   MOVE      VAFVA3        E_VHFVA3
047800170119     C                   MOVE      VAFSV4        E_VHFSV4
047900170119     C                   MOVE      VAFVA4        E_VHFVA4
048000170119     C                   MOVE      VAFSV5        E_VHFSV5
048100170119     C                   MOVE      VAFVA5        E_VHFVA5
048200170119     C                   MOVE      VAFSV6        E_VHFSV6
048300170119     C                   MOVE      VAFVA6        E_VHFVA6
048400170119     C                   MOVE      VAFSV7        E_VHFSV7
048500170119     C                   MOVE      VAFVA7        E_VHFVA7
048600170119     C                   MOVE      VAFSV8        E_VHFSV8
048700170119     C                   MOVE      VAFVA8        E_VHFVA8
048800170119     C                   MOVE      VAFVAX        E_VHFVAX
048900170119     C                   MOVE      VAFIMV        E_VHFIMV
049000170119     C                   MOVE      VAFDFT        E_VHFDFT
049100170119     C                   MOVE      VAFNFT        E_VHFNFT
049200170119     C                   MOVE      VAFFIV        E_VHFFIV
049300170119     C                   MOVE      VAFDIV        E_VHFDIV
049400170119     C                   MOVE      VAFDRT        E_VHFDRT
049500170119     C                   MOVE      VAFRMN        E_VHFRMN
049600170119     C                   MOVE      VAFRMA        E_VHFRMA
049700170119     C                   MOVE      VAFRMX        E_VHFRMX
049800170119     C                   MOVE      VAFFTC        E_VHFFTC
049900170119     C                   MOVE      VAFTC2        E_VHFTC2
050000170119     C                   MOVE      VAFTSP        E_VHFTSP
050100170119     C                   MOVE      VAFFAP        E_VHFFAP
050200170119     C                   MOVE      VAFFIN        E_VHFFIN
050300170119     C                   MOVE      VAFCTR        E_VHFCTR
050400170119     C                   MOVE      VAFNAS        E_VHFNAS
050500170119     C                   MOVE      VAFCTS        E_VHFCTS
050600170119     C                   MOVE      VAFCAD        E_VHFCAD
050700170119     C                   MOVE      VAFLOD        E_VHFLOD
050800170119     C                   MOVE      VAFPRD        E_VHFPRD
050900170119     C                   MOVE      VAFNZD        E_VHFNZD
051000170119     C                   MOVE      VAFVAS        E_VHFVAS
051100170119     C                   MOVE      VAFIAS        E_VHFIAS
051200170119     C                   MOVE      VAFFPC        E_VHFFPC
051300170119     C                   MOVE      VAFPKC        E_VHFPKC
051400170119     C                   MOVE      VAFFVC        E_VHFFVC
051500170119     C                   MOVE      VAFVLC        E_VHFVLC
051600170119     C*
051700170119     C                   ENDSR
051800170119
051900170119
052000170119
052100170119     C     INZVAR        BEGSR
052200170119     C*
052300170119     C                   clear                   wRSD
052400170119     C                   clear                   wIND
052500170119     C                   clear                   wRSM
052600170119     C                   clear                   wINM
052700170119     C                   clear                   wCAM
052800170119     C                   clear                   wLOM
052900170119     C                   clear                   wPRM
053000170119     C                   clear                   wNZM
053100170119     C                   clear                   wRMO
053200170119     C                   clear                   wCMO
053300170119     C                   clear                   wNMO
053400170119     C                   clear                   wPVL_A
053500170119     C                   clear                   wPVL_E
053600170120     C                   clear                   wCCA
053700170120     C                   clear                   wDCM_A
053800170120     C                   clear                   wDCM_E
053900170119     C*
054000170119     C                   eval      WV07 = pSEP
054100170119     C                   eval      WV09 = pSEP
054200170119     C                   eval      WV13 = pSEP
054300170119     C                   eval      WV15 = pSEP
054400170119     C*
054500170119     C                   ENDSR
054600170119
054700170119
054800170119
054900170119     C     REPCIG        BEGSR
055000170119     C*
055100170119     C* Reperisco i dati del CUP e CIG (da TITA4)
055200170119     C                   CLEAR                   DTA44
055300170119     C                   EVAL      ta4TRC = '4'
055400170119     C     KEYta430C     CHAIN     tita430c
055500170119     C                   IF        %found(tita430c)
055600170119     C                   EVAL      DTA44 = ta4NOT
055700170119     C                   ENDIF
055800170119     C*
055900170119     C                   ENDSR
056000170119
056100170119
056200170119
056300170119     C     REPEXT        BEGSR
056400170119     C*
056500170119     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
056600170119     C     KEYtas30C     CHAIN     titas30c
056700170119     C                   IF        %found(titas30c)
056800170119     C*
056900170119     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
057000170119     C                   EVAL      wRSD = TASRSD
057100170119     C                   EVAL      wIND = TASIND
057200170119     C*
057300170119     C* Peso tassabile
057400170119     C                   Z-ADD     TASPVL        wPVL              7 1
057500170119     C                   MOVE      wPVL          WD09
057600170119     C                   MOVEL     wPVL          WI09
057700170119     C                   MOVE      WASC09        wPVL_A
057800170119     C                   MOVE      wPVL          wPVL_E
057900170120     C                   MOVEL     TASCCA        wCCA
058000170120     C                   MOVE      TASDCM        wDCM_A
058100170120     C                   MOVE      TASDCM        wDCM_E
058200170119     C*
058300170119     C* Per reperire i dati del mittente verifico il codice del cliente mittente:
058400170119     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
058500170119     C                   MOVE(P)   tasCCM        wCLI              4 0
058600170119     C                   IF        wCLI =    0 OR
058700170119     C                             wCLI = 8888 OR
058800170119     C                             wCLI = 9999
058900170119     C                   EVAL      taaTRC = 'M'
059000170119     C     KEYtaa30C     CHAIN     titaa30c
059100170119     C                   IF        %found(titaa30c)
059200170119     C                   EVAL      wRSM = TAARSC
059300170119     C                   EVAL      wINM = TAAIND
059400170119     C                   EVAL      wCAM = TAACAP
059500170119     C                   EVAL      wLOM = TAALOC
059600170119     C                   EVAL      wPRM = TAAPRV
059700170119     C                   EVAL      wNZM = TAANAZ
059800170119     C                   ENDIF
059900170119     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
060000170119     C                   ELSE
060100170119     C                   CLEAR                   BS69DS
060200170119     C                   CLEAR                   ACODS
060300170119     C                   CLEAR                   INDDS
060400170119     C                   CLEAR                   CLPDS
060500170119     C                   CLEAR                   CLSDS
060600170119     C                   Z-ADD     tasCCM        I69KAC
060700170119     C                   Z-ADD     tasCCM        I69KIN
060800170119     C                   CALL      'TIBS69R'
060900170119     C                   PARM                    BS69DS
061000170119     C                   PARM                    ACODS
061100170119     C                   PARM                    INDDS
061200170119     C                   PARM                    CLPDS
061300170119     C                   PARM                    CLSDS
061400170119     C     O69ERR        IFNE      '1'
061500170119     C                   EVAL      wRSM = ACORAG
061600170119     C                   EVAL      wINM = INDVIA
061700170119     C                   MOVEL(P)  INDCAP        wCAM
061800170119     C                   EVAL      wLOM = INDCIT
061900170119     C                   EVAL      wPRM = INDPRV
062000170119     C                   EVAL      wNZM = INDSTA
062100170119     C                   ENDIF
062200170119     C                   ENDIF
062300170119     C*
062400170119     C* Per reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O'
062500170119     C                   EVAL      taaTRC = 'O'
062600170119     C     KEYtaa30C     CHAIN     titaa30c
062700170119     C                   IF        %found(titaa30c)
062800170119     C                   EVAL      wRMO = TAARSC
062900170119     C                   EVAL      wCMO = TAACAP
063000170119     C                   EVAL      wNMO = TAANAZ
063100170119     C                   ENDIF
063200170119     C*
063300170119     C                   ENDIF
063400170119     C*
063500170119     C                   ENDSR
063600170119
063700170119
063800170119
063900040317      /TITLE Operazioni iniziali.
064000040317     C     *inzsr        BEGSR
064100040317     C*
064200040317     C     *ENTRY        PLIST
064300040317     C                   PARM                    prmppt
064400040317     C     wrkesito      PARM      wrkesito      prmesito
064500040317     C*
064600040317     C* Definizione chiavi
064700040317     C*
064800040317     C* Chiave su TITAS30C - Completa
064900040317     C     KEYtas30C     KLIST
065000040317     C                   KFLD                    vafAAS
065100040317     C                   KFLD                    vafLNP
065200040317     C                   KFLD                    vafNRS
065300040317     C                   KFLD                    vafNSP
065400040317     C                   KFLD                    vafTBL
065500040317     C*
065600040317     C* Chiave su TITAA30C - Completa
065700040317     C     KEYtaa30C     KLIST
065800040317     C                   KFLD                    vafAAS
065900040317     C                   KFLD                    vafLNP
066000040317     C                   KFLD                    vafNRS
066100040317     C                   KFLD                    vafNSP
066200040317     C                   KFLD                    taaTRC
066300120601     C*
066400120601     C* Chiave su TITA430C - Completa
066500120601     C     KEYta430C     KLIST
066600120601     C                   KFLD                    vafAAS
066700120601     C                   KFLD                    vafLNP
066800120601     C                   KFLD                    vafNRS
066900120601     C                   KFLD                    vafNSP
067000120601     C                   KFLD                    ta4TRC
067100040317     C*
067200040317     C                   ENDSR
