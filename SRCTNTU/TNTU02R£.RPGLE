000100011228     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400020214     H*             - anagrafico contratti -                         *
000500000000     H*--------------------------------------------------------------*
000600020214     Fanent01l  iF   E           k disk
000700020214     Fanuni01l  iF   E           k disk
000800011228     Fansif01L  IF   E           K DISK
000900020214     Fyco3091d  CF   E             WORKSTN
001000020214     Fynfea00f  UF a E             DISK
001100030116
001200030116     D Errore          C                   '1'
001300030116     D Eseguito        C                   '0'
001400030116
001500020214     D anfrn00f      E DS                  extname(anfrn00f)
001600011127      *
001700020103     D Psds           SDS
001800020103     D  PgmName          *PROC
001900011228     D KPJBA         E DS
002000011228    >D xsoc001ds     E DS
002100011228     D ana007ds      e ds                  inz
002200020214     D ana009ds      E DS                  inz
002300020214     D ana032ds      E DS                  inz
002400020214     D ana201ds      E DS                  inz
002500030114     D ana964ds      E DS                  inz
002600030114     D x07valds      E DS                  inz
002700030120     D ND002DS       E DS                  inz
002800011228     D* DS Interna per dati di output di XSOC
002900011228     D XSOCDS          DS          1000
003000020214     D* Definizioni parametri driver soggetti
003100020214     DDvaric           S              1
003200020214     DDvaSocieta       S              3
003300020214     DDvaUnita         S              8
003400020214     DDvaStrutt        S             10
003500020214     DDvaDtRif         S               D
003600020214     DDvaLenOut        S              9B 0
003700020214     DDvaSnatura       S              1
003800020214     DDvaCodice        S              8
003900020214     DDvaLineav        S              3
004000020214     DDvaFiliale       S              3  0
004100020214     DDvaSogg          S              8
004200020214     DDvaTpInd         S              2
004300020214     DDvaCdInd         S              3
004400020214     DDvaErrore        S              1
004500020214     DDvaOutput        S           4000
004600011127     D DTAiso          S               D   INZ
004700011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
004800011127     D DTADAL          S               D   INZ
004900011127     D DTAAL           S               D   INZ
005000011228     d data            ds            10
005100011228     d aa                      3      4
005200011228     d mm                      6      7
005300011228     d gg                      9     10
005400011228     D com6            S              6
005500030122     D Mod             S
005600030122     D                                     LIKE(PrmMod)
005700030123     D XalCodice       S             31
005800030123     D XalLength       S              5  0
005900030116     D PrmEsito        S              1
006000030116     D PrmYnFea      E DS
006100030116     D                                     EXTNAME(YnFea00F)
006200030116     D                                     PREFIX(Prm:3)
006300030117     D Ana399DS      E DS
006400030117     D                                     INZ
006500030120
006600030120     D Refresh         S              1
006700030120     D Esito           S              1
006800030120     D GesMsg          S              1
006900030120     D Output          S           4000
007000030120     D LenOut          S              9B 0
007100030120     D StrutturaO      S             10
007200030116
007300000000     C                   EXSR      AZZER
007400011228     c                   if        nrr <> 0
007500011228     c                   exsr      srcarica
007600011228     c                   end
007700011127     C*
007800011228     C  nlr              DO        *HIVAL
007900011127     c                   EXSR      CLR00
008000030117
008100030117     ** Abilito F15 se il record è della stessa società del lavoro.
008200030117     C                   EVAL      *IN15 = (SocV = XscSoc)
008300030117
008400011228     c                   if        *in99
008500011228     c                   setoff                                       99
008600011228     c                   write     r01
008700011228     c                   seton                                        99
008800011228     c                   end
008900030117
009000011228     C                   IF        mod = '4 '
009100011228     C                             OR
009200011228     C                             mod = '5 '
009300011228     C                   WRITE     R01
009400011228     C                   EXFMT     R01Protect
009500011228     C                   ELSE
009600011228     C                   EXFMT     R01
009700011228     C                   ENDIF
009800000000     C                   SETOFF                                       99
009900011127     C* FINE LAVORO
010000011228     C                   IF        *INKC
010100011228     C                   MOVE      'C'           CMD
010200011228     C                   LEAVE
010300011228     c                   end
010400011228     C* GUIDA
010500011228     C                   IF        *INKL
010600011228     C                   MOVE      'L'           CMD
010700011228     C                   LEAVE
010800011228     c                   end
010900020128     C* ripristino
011000020128     C                   IF        *INKe
011100020128     C                   exsr      srcarica
011200020128     C                   iter
011300020128     c                   end
011400030123     C*
011500030123     C                   EXSR      AllineaR01
011600011127     C* RICERCA
011700011127     C   KD              EXSR      RIC01
011800011228     C* controlli
011900011127     C                   EXSR      CTR01
012000030117
012100030117     ** F15 Interrogazioni conto.
012200030117     C                   IF        *INKP AND NOT *IN05
012300030117     C                   CLEAR                   Ana399DS
012400030117     C                   EVAL      A39Societa = SocV
012500030117     C                   EVAL      A39Unita = UnitaV
012600030117     C                   EVAL      A39Ctb = 'CG'
012700030117     C                   EVAL      A39Kcc = KccV
012800030117     C                   EVAL      A39Ksc = KscV
012900030117     C                   EVAL      KPJBU = Ana399DS
013000030117     C                   CALL      'ANA399R'
013100030117     C                   PARM                    KPJBA
013200030117     C                   ENDIF
013300030117
013400011127     C   99              ITER
013500030117
013600011228     C* AGGIORNA
013700011228     C                   IF        *INKF
013800011228     C                   EXSR      SRAGGIO
013900011228     C                   IF        *IN41
014000011228     C                   SETON                                        99
014100011228     c                   iter
014200011228     c                   end
014300011228     C                   MOVE      'F'           CMD
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014700011127     c*
014800000000     C                   SETON                                        LR
014900011127     c**********************************************************************
015000011228     c* carica 1° formato
015100011127     c**********************************************************************
015200011228     C     srcarica      BEGSR
015300020218     c                   clear                   msgv
015400030122     C                   EVAL      Mod = PrmMod
015500020214     C     nrr           chain     ynfea00f
015600020214     C                   if        %found(ynfea00f)
015700020214     c                   if        (mod = '2 ' and feadtdlt <> *loval) or
015800030116     c                             (mod <> '5 ' and feasocieta <> xscsoc)
015900020218     c* messaggio d'errore
016000020218     c                   select
016100020218     c                   when      mod <> '5 ' and feasocieta <> xscsoc
016200020220     c                   eval      msgv = 'Non è possibile gestire il record-
016300020218     c                              se la società non è la propria'
016400020218     c                   when      mod = '2 ' and feadtdlt <> *loval
016500020218     c                   eval      msgv = 'Per poter modificare il record occor-
016600020218     c                             re prima ripristinarlo'
016700020218     c                   endsl
016800020218     c* forzo la visualizzazione
016900030122     c                   eval      modv = 'VISUALIZZAZIONE'
017000020214     c                   eval      mod = '5 '
017100020218     c                   seton                                        40
017200020218     c                   end
017300020214     c                   if        mod = '4 ' and feadtdlt <> *loval
017400030122     c                   eval      modv = '   RIPRISTINO  '
017500030122     c                   eval      mod = '2 '
017600020214     c                   end
017700030114     c                   movel     featpfor      r01tpfor
017800020214     c                   movel     feacontra     contrav
017900030114     c                   movel     feasocieta    socv
018000020214     c                   movel     feakccfor     kccv
018100020214     c                   movel     feakscfor     kscv
018200020214     c                   movel     feaentit1     entitv
018300030116     c                   movel     feaentit2     R01entit2
018400020214     c                   movel     feaunita      unitav
018500011228     c                   z-add     0             dtadalv
018600020214     c                   if        feadtinco <> *loval
018700020214     c                   move      feadtinco     data
018800011228     c                   eval      com6 = gg + mm + aa
018900011228     c                   move      com6          dtadalv
019000011228     c                   end
019100020103     c                   z-add     999999        dtaalv
019200020214     c                   if        feadtfico <> *hival
019300020214     c                   move      feadtfico     data
019400011228     c                   eval      com6 = gg + mm + aa
019500011228     c                   move      com6          dtaalv
019600011228     c                   end
019700030116     C                   IF        MsgV = *BLANK
019800011228     c                   exsr      ctr01
019900030116     C                   ENDIF
020000011228     C                   end
020100011228     C                   ENDSR
020200011228     c**********************************************************************
020300011228     c* pulisce 1° formato
020400011228     c**********************************************************************
020500011127     C     clr00         BEGSR
020600011127     C                   Z-ADD     0             §RIGA
020700011127     C                   Z-ADD     0             §COLON
020800011127     C                   CLEAR                   §FLD
020900011127     C                   ENDSR
021000011127     C******************************************************
021100011127     C* CONTROLLI PRIMO VIDEO
021200011127     C******************************************************
021300011127     C     CTR01         BEGSR
021400011228     c                   setoff                                       99
021500020214     c                   if        mod <> '4 '
021600030114     c* tipo fornitore obligatorio
021700030114     C                   CLEAR                   R01TpForD
021800030114     c     r01tpfor      ifeq      *blanks
021900030114     c                   seton                                        0899
022000030114     C                   ELSE
022100030114     c                   clear                   x07valds
022200030114     C                   EVAL      X07RIC = '1'
022300030114     C                   EVAL      X07TRC = 'YNFEA000'
022400030114     C                   EVAL      X07CAM = 'FEATPFOR'
022500030114     C                   EVAL      X07VAL =  r01tpfor
022600030114     c*
022700030114     C                   CALLB     'X07VALR'
022800030114     C                   PARM                    X07VALDS
022900030114     c*
023000030114     C                   IF        x07err = '1'
023100030114     C                   SETON                                        9908
023200030114     C                   ELSE
023300030114     C                   EVAL      R01TpForD = X07Des
023400030114     C                   END
023500030114     c                   end
023600020214     c* contratto obbligatorio
023700020214     c     contrav       ifeq      *blanks
023800020214     c                   seton                                        0199
023900011228     c                   end
024000020214     c* Società
024100020214     c                   movel     *blanks       dsocv
024200020214     C     socv          chain     ansif01l                           02
024300020214     c                   if        not *in02
024400020214     c                   movel     sifdesbrev    dsocv
024500030116     C                   ELSE
024600030116     C                   EVAL      *IN99 = *ON
024700020214     C                   ENDIF
024800020214     c* Fornitore
024900020214     c                   movel     *blanks       dforv
025000020214     C                   IF        kscv = *blanks
025100030120     C                             OR
025200030120     C                             kccv = *blanks
025300020214     c                   seton                                        9905
025400020214     c                   else
025500030120      *
025600030120     C*                  eval      dvastrutt = 'ANFRN'
025700030120     C*                  eval      dvalenout = %Size(anfrn00f)
025800030120     C*                  CALLB     'NDDVASOG'
025900030120     C*                  PARM      '1'           DVARIC
026000030120     C*                  PARM      socv          DVASOCIETA
026100030120     C*                  PARM                    DVAUNITA
026200030120     C*                  PARM                    DVASTRUTT
026300030120     C*                  PARM                    DVADTRIF
026400030120     C*                  PARM                    DVALENOUT
026500030120     C*                  PARM      'F'           DVASNATURA
026600030120     C*                  PARM      kscv          DVACODICE
026700030120     C*                  PARM                    DVALINEAV
026800030120     C*                  PARM                    DVAFILIALE
026900030120     C*                  PARM                    *omit
027000030120     C*                  PARM                    *omit
027100030120     C*                  PARM                    *omit
027200030120     C*                  PARM      *blanks       DVAERRORE
027300030120     C*                  PARM                    DVAOUTPUT
027400030120
027500030120     C                   EVAL      LenOut = %SIZE(ND002DS)
027600030120     C                   CALLB     'NDMVC002'
027700030120     C                   PARM                    SocV
027800030120     C                   PARM                    KccV
027900030120     C                   PARM                    KscV
028000030120     C                   PARM                    Data
028100030120     C                   PARM      *OFF          GesMsg
028200030120     C                   PARM      *BLANK        Esito
028300030120     C                   PARM      'ND002DS'     StrutturaO
028400030120     C     ND002DS       PARM      *BLANK        Output
028500030120     C                   PARM                    LenOut
028600030120     C                   PARM      *OFF          Refresh
028700030120     C*
028800030120   B3C     Esito         IFEQ      *OFF
028900030120     c                   movel     DESBREV002    dforv
029000020214     c                   else
029100020214     c                   seton                                        9905
029200020214     C                   ENDIF
029300020214     C                   ENDIF
029400030116     c* Centro di costo.
029500020214     c                   movel     *blanks       dentv
029600020214     C                   IF        entitv = *blanks
029700030114     C                   IF        NOT *INKF
029800020214     c                   seton                                        9903
029900030114     C                   ENDIF
030000020214     c                   else
030100030116     C                   EVAL      EntTpRifAn = cdcv
030200030116     C                   EVAL      EntEntita  = entitv
030300020214     C     keyent        chain     anent01l                           03
030400020214     c                   if        not *in03
030500020214     c                   movel     entdesbrev    dentv
030600020214     c                   else
030700020214     c                   seton                                        99
030800020214     C                   ENDIF
030900020214     C                   ENDIF
031000030116     c* Mansione.
031100030116     c                   movel     *blanks       R01DesEnt2
031200030116     C                   IF        R01Entit2 <> *blanks
031300030116     C                   EVAL      EntTpRifAn = R01TpRif2
031400030116     C                   EVAL      EntEntita  = R01Entit2
031500030116     C     keyent        chain     anent01l                           09
031600030116     c                   if        not *in09
031700030116     c                   movel     entdesbrev    R01DesEnt2
031800030116     c                   else
031900030116     c                   seton                                        99
032000030116     C                   ENDIF
032100030116     C                   ENDIF
032200020214     c* Unità
032300020214     c                   movel     *blanks       duniv
032400020214     C                   IF        unitav = *blanks
032500020214     c                   seton                                        9904
032600020214     c                   else
032700020214     C     keyuni        chain     anuni01l                           04
032800020214     c                   if        not *in04
032900020214     c                   movel     unidesbrev    duniv
033000020214     c                   else
033100020214     c                   seton                                        9904
033200020214     C                   ENDIF
033300020214     C                   ENDIF
033400011127     C* CONTROLLO DATE
033500011127     C                   MOVEL     *LOVAL        DTADAL
033600011228     C                   IF        dtadalv <> 0
033700011228     C     *dmy          test(d)                 dtadalv                06
033800011228     c                   if        *in06
033900020103     C                   SETON                                        99
034000020103     c                   else
034100011228     C     *dmy          MOVE      dtadalv       DTAEUR
034200011127     C                   MOVEL     DTAEUR        DTADAL
034300020103     C                   END
034400011127     C                   END
034500011127     C* CONTROLLO AL
034600011228     C                   MOVEL     *hival        DTAAL
034700020103     C                   IF        dtaALv <> 999999
034800011228     C     *dmy          test(d)                 dtaalv                 07
034900011228     c                   if        *in07
035000011127     C                   SETON                                        99
035100020103     c                   else
035200011228     C     *dmy          MOVE      dtaalv        DTAEUR
035300011127     C                   MOVEL     DTAEUR        DTAAL
035400020103     C                   END
035500011127     C                   END
035600011127     C*
035700020103     C                   IF        (dtaDALv <> 0 OR dtaALv <> 999999) AND
035800011127     C                              DTADAL > DTAAL
035900011228     C                   SETON                                        060799
036000011127     C                   END
036100011228     C                   END
036200030116
036300030116     ** Controllo sovrapposizione periodo validità.
036400030116     C                   IF        NOT *IN99
036500030116     C                             AND
036600030116     C                             (Mod = '1' OR Mod = '3')
036700030116
036800030116     C                   CLEAR                   PrmYnFea
036900030116     C                   CALL      'YCOYNFEAR'
037000030116     C                   PARM                    R01TpFor
037100030116     C                   PARM                    ContraV
037200030116     C                   PARM                    DtaDal
037300030116     C                   PARM      *BLANK        PrmEsito
037400030116     C                   PARM                    PrmYnFea
037500030116
037600030116     C                   IF        PrmEsito = Eseguito
037700030116     C                   EVAL      *IN99 = *ON
037800030116     C                   EVAL      *IN06 = *ON
037900030116     C                   ENDIF
038000030116
038100030116     C                   CLEAR                   PrmYnFea
038200030116     C                   CALL      'YCOYNFEAR'
038300030116     C                   PARM                    R01TpFor
038400030116     C                   PARM                    ContraV
038500030116     C                   PARM                    DtaAl
038600030116     C                   PARM      *BLANK        PrmEsito
038700030116     C                   PARM                    PrmYnFea
038800030116
038900030116     C                   IF        PrmEsito = Eseguito
039000030116     C                   EVAL      *IN99 = *ON
039100030116     C                   EVAL      *IN07 = *ON
039200030116     C                   ENDIF
039300030116
039400030116     C                   ENDIF
039500030116
039600011127     C                   ENDSR
039700020107     C******************************************************
039800020107     C* RICERCA PRIMO VIDEO
039900020107     C******************************************************
040000020107     C     RIC01         BEGSR
040100011130     c                   select
040200020214     c* fornitore
040300020214     C                   when      §fld = 'KSCV'
040400020214     C                   RESET                   Ana201DS
040500020214     c                   if        socv <> *blanks and socv <> '*'
040600020214     C                   MOVE      socv          soc201
040700020214     c                   end
040800030120     c                   movel(p)  KccV          kcc201
040900020214     C                   EVAL      Kpjbu = Ana201DS
041000020214     C                   CALL      'ANA201R'
041100020214     C                   PARM                    Kpjba
041200020214     C                   EVAL      Ana201DS = Kpjbu
041300020214     C                   IF        Err201 = '0' and ksc201 <> *blanks
041400030120     c                   movel     kcc201        KcCV
041500020214     c                   movel     ksc201        KSCV
041600020214     c                   end
041700030116     c* Centro di costo.
041800030116     C                   when      §fld = 'ENTITV'
041900020214    >C                   RESET                   ANA032DS
042000020214    >C                   MOVEL     '01'          OPZ032
042100020214     C                   MOVE      'S'           FSC032
042200020214     C                   MOVE      'CDC'         TPE032
042300020214     c                   if        socv <> *blanks and socv <> '*'
042400020214     C                   MOVE      socv          soc032
042500020214     c                   end
042600020214    >C                   MOVEL     ANA032Ds      KPJBU
042700020214    >C                   CALL      'ANA032R'
042800020214     C                   PARM                    KPJBA
042900020214    >C                   MOVEL     KPJBU         ANA032DS
043000020214     c                   if        ent032 <> *blanks
043100020214     C                   MOVEL     ENT032        entitv
043200020214     c                   end
043300030116     c* Mansione.
043400030116     C                   when      §fld = 'R01ENTIT2'
043500030116    >C                   RESET                   ANA032DS
043600030116    >C                   MOVEL     '01'          OPZ032
043700030116     C                   MOVE      'S'           FSC032
043800030116     C                   MOVE      'MAN'         TPE032
043900030116     c                   if        socv <> *blanks and socv <> '*'
044000030116     C                   MOVE      socv          soc032
044100030116     c                   end
044200030116    >C                   MOVEL     ANA032Ds      KPJBU
044300030116    >C                   CALL      'ANA032R'
044400030116     C                   PARM                    KPJBA
044500030116    >C                   MOVEL     KPJBU         ANA032DS
044600030116     c                   if        ent032 <> *blanks
044700030116     C                   MOVEL     ENT032        R01Entit2
044800030116     c                   end
044900030114     c* tipo fornitore
045000030114     C                   when      §fld = 'R01TPFOR'
045100030114     c                   clear                   ana964ds
045200030114     C                   EVAL      opz964 = '01'
045300030114     C                   EVAL      cmt964 = '0'
045400030114     C                   EVAL      all964 = '1'
045500030114     C                   EVAL      trc964 = 'YNFEA000'
045600030114     C                   EVAL      CAM964 = 'FEATPFOR'
045700030114     C                   EVAL      kpjbu = ana964ds
045800030114     C                   CALL      'ANA964R'
045900030114     C                   PARM                    kpjba
046000030114     C                   EVAL      ana964ds = kpjbu
046100030114     c*
046200030114     C                   IF        ret964 = *blanks
046300030114     C                   EVAL      r01tpfor = val964
046400030114     c                   end
046500020214     c* unità
046600020214     C                   when      §fld= 'UNITAV'
046700020214    >C                   RESET                   ANA009DS
046800020214    >C                   MOVEL     '01'          OPZ009
046900020214     c                   if        socv <> *blanks and socv <> '*'
047000020214     C                   MOVE      socv          soc009
047100020214     c                   end
047200020214    >C                   MOVEL     ANA009Ds      KPJBU
047300020214    >C                   CALL      'ANA009R'
047400020214     C                   PARM                    KPJBA
047500020214    >C                   MOVEL     KPJBU         ANA009DS
047600020214     c                   if        uni009 <> *blanks
047700020214     C                   MOVEL     uni009        unitav
047800020214     c                   end
047900011130     C                   ENDsl
048000020214     c                   exsr      ctr01
048100011130     C* POSIZIONAMENTO CURSORE
048200011130     C                   Z-ADD     §RIGA         §RIG
048300011130     C                   Z-ADD     §COLON        §COL
048400011127     C                   ENDSR
048500011228     c**********************************************************************
048600011228     c* AGGIORNA ARCHIVIO
048700011228     c**********************************************************************
048800011228     C     srAGGIO       BEGSR
048900011228     C                   SETOFF                                       41
049000011228     c                   seton                                        90
049100011228     c                   if        mod <> '3 '
049200020214     C     nrr           chain     ynfea00f                           90
049300011228     c                   end
049400030114     c                   movel     r01tpfor      featpfor
049500020214     c                   movel     contrav       feacontra
049600020214     c                   movel     socv          feasocieta
049700020214     c                   movel     kccv          feakccfor
049800020214     c                   movel     kscv          feakscfor
049900020214     c                   movel     cdcv          featprif1
050000030116     c                   movel     entitv        feaentit1
050100030116     c                   movel     R01TpRif2     featprif2
050200030116     c                   movel     R01Entit2     feaentit2
050300030116     c                   movel     unitav        feaunita
050400020214     C* Date validità
050500020214     c                   if        dtadalv <> 0
050600020214     c     *dmy          movel     dtadalv       feadtinco
050700020214     c                   else
050800020214     c                   movel     '0001-01-01'  feadtinco
050900020214     c                   end
051000020214     c                   if        dtaalv <> 999999
051100020214     c     *dmy          movel     dtaalv        feadtfico
051200020214     c                   else
051300020214     c                   movel     '9999-12-31'  feadtfico
051400020214     c                   end
051500020214     c                   if        mod <> '4 '
051600020214     c                   move      *loval        feadtdlt
051700020214     c                   move      *blanks       feautdlt
051800020214     c                   else
051900020214     c                   movel     udate         feadtdlt
052000020214     c                   movel     xsccut        feautdlt
052100020214     c                   end
052200020214     c* aggiorno
052300020214     c                   if        not *in90
052400020214     c                   movel     udate         feadtupd
052500020214     c                   movel     xsccut        feautupd
052600020214     c                   update    ynfea000                             41
052700020214     c                   else
052800020214     c* scrivo
052900020214     c                   movel     udate         feadtwrt
053000020214     c                   movel     xsccut        feautwrt
053100020214     c                   write     ynfea000                             41
053200020214     c                   end
053300020214     c* cancello
053400011228     C                   ENDSR
053500011127     C******************************************************
053600000000     C** PULIZIA CAMPI DS
053700011127     C******************************************************
053800011127     C     AZZER         BEGSR
053900030114     C                   MOVEL     *BLANKS       r01tpfor
054000020214     C                   MOVEL     *BLANKS       contrav
054100020214     C                   MOVEL     xscsoc        socv
054200020214     C                   MOVEL     *BLANKS       dsocv
054300020214     C                   MOVEL     *blanks       kccv
054400020214     C                   MOVEL     *blanks       kscv
054500020214     C                   MOVEL     *blanks       dforv
054600020214     C                   MOVEL     'CDC'         cdcv
054700030116     C                   MOVEL     'MAN'         R01TpRif2
054800020214     C                   MOVEL     *BLANKS       entitv
054900020214     C                   MOVEL     *BLANKS       dentv
055000030116     C                   MOVEL     *BLANKS       R01Entit2
055100030116     C                   MOVEL     *BLANKS       R01DesEnt2
055200020214     C                   MOVEL     *BLANKS       unitav
055300020214     C                   MOVEL     *BLANKS       duniv
055400011228     C                   Z-ADD     0             dtadalv
055500020103     C                   Z-ADD     999999        dtaalv
055600000000     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200020214     C                   PARM                    nrr              15 0
056300030122     C                   PARM                    PrmMod            2
056400011228     C                   PARM                    cmd               1
056500030122     C                   EVAL      Mod = PrmMod
056600020214     C     Keyent        KLIST
056700020214     C                   KFLD                    socv
056800030116     C                   KFLD                    ENTTPRIFAN
056900030116     C                   KFLD                    ENTENTITA
057000020214     C     Keyuni        KLIST
057100020214     C                   KFLD                    socv
057200020214     C                   KFLD                    unitav
057300011228     c     mod           comp      '5 '                                   40
057400020103     c                   select
057500020103     c                   when      mod = '1 '
057600030122     c                   eval      modv = '  INSERIMENTO  '
057700020103     c                   when      mod = '2 '
057800030122     c                   eval      modv = '   MODIFICA    '
057900020103     c                   when      mod = '3 '
058000030122     c                   eval      modv = '     COPIA     '
058100020103     c                   when      mod = '4 '
058200030122     c                   eval      modv = '    ANNULLA    '
058300020103     c                   when      mod = '5 '
058400020103     c                   eval      modv = 'VISUALIZZAZIONE'
058500020103     c                   endsl
058600011228      * Reperimento dati società
058700011228     C                   clear                   SocXsc
058800011228     C                   eval      tipxsc = 'SOC001'
058900011228     C                   exsr      rtvsocieta
059000011228     C                   if        rtnxsc = '1'
059100011228     C                   seton                                        lr
059200011228     C                   else
059300011228     C                   MOVEL     XSOCDS        xsoc001ds
059400011228     C                   endif
059500011127     C                   ENDSR
059600011228     C*---------------------------------------------------------------------
059700011228     c*  Reperisco società.
059800011228     C*---------------------------------------------------------------------
059900011228     C     rtvsocieta    begsr
060000011228     C*
060100011228     C                   CALLb     'XSOC'
060200011228   >>C                   Parm                    TipXsc            6
060300011228   >>C                   Parm                    SocXsc            3
060400011228   >>C                   Parm                    CdsXsc            9 0
060500011228   >>C                   Parm                    ModXsc            3
060600011228   >>C                   Parm      *BLANK        RtnXsc            1
060700011228     C                   Parm                    XsocDS
060800011228   >>C                   Parm                    Kpjba
060900011228     C*
061000011228     C                   endsr
061100030123
061200030123     ***********************************************************************
061300030123     **
061400030123     ** Allineamento codici.
061500030123     **
061600030123     ***********************************************************************
061700030123     C     AllineaR01    BEGSR
061800030123
061900030123     ** Allineo capo conto soggetto.
062000030123     C                   EVAL      XalLength = %SIZE(KccV)
062100030123     C                   CALLB     'XALLINEA'
062200030123     C     KccV          PARM      KccV          XalCodice
062300030123     C                   PARM                    XalLength
062400030123     C                   PARM                    XscInp
062500030123
062600030123     ** Allineo sotto conto soggetto.
062700030123     C                   EVAL      XalLength = %SIZE(KscV)
062800030123     C                   CALLB     'XALLINEA'
062900030123     C     KscV          PARM      KscV          XalCodice
063000030123     C                   PARM                    XalLength
063100030123     C                   PARM                    XscInp
063200030123
063300030123     C                   ENDSR
