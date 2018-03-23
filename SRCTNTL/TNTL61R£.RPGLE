000100060707     H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000401060704     H*--------------------------------------------------------------*
000500060704     H*       Rettifiche autofatturazione trazionisti
000600000000     H*--------------------------------------------------------------*
000700060704     Ftntl61D   CF   E             WORKSTN
000701060704     F                                     SFILE(S01:NRR)
001200021113      *
001201060704     FAnFrn01l  if   E           K DISK
001202060707     Ftntst02l  if   E           K DISK
001203060704     FAnRco01l  if   E           K DISK
001204060704     FAnSog01l  if   E           K DISK
001400060705     Ftntre01l  uf a E           k DISK
001500021111     FTnTbe01l  if   E           K DISK
001501061025     FTabel00f  if   E           K DISK
001600021108      *
001601061025     D ddatiute      e ds
001602061025     D azuteds       e ds                  extname(AZUTE00F)
001603061025     D tibs34ds      E DS                  inz
001700021108     D soc001        E DS                  EXTNAME(xsoc001ds)
001800021108     D xsocds          DS          1000
001900060704     D dblt          E DS
001901061025     D ds3i          E DS
001902060803     D tntl61ds      E DS
001903061201     D x10eseds      E DS                  inz
001904060707     D angy4z        E DS                  extname(angy4zds)
001905060704     Ddataeur                          d   datfmt(*eur)
001906060704     Ddataiso                          d   datfmt(*iso)
001908060706     D* Definizioni parametri chiamata moduli NDMVC104
001909060706    >D Societa         S              3
001910060706    >D Ctb             S              2
001911060706     D                                     INZ('CG')
001912060706    >D Unita           S              8
001913060706     D TpReg           S              1
001914060706     D DtReg           S             10
001915060706     D DtLGio          S             10
001916060706     D RegIVA          S              1
001917060706     D TpRegIVA        S              1
001918060706     D Stato           S              2
001919060706     D Esito           S              1
001920060706     D Gesmsg          S              1
001921060706     D xnmRic          S              1                                         Richiesta
001922060706     D                                     INZ('1')
001923060706     D xnmKey          S              8                                         Chiave gestionale
001924060706     D xnmCmt          S              1                                         Commit
001925060706     D                                     INZ(*OFF)
001926060706     D xnmDat          S               D                                        Data riferimento
001927060706     D xnmIva          S              1                                         Abilitazione IVA
001928060706     D                                     INZ(*ON)
001929060706     D xnmReg          S              1                                         Abilitazione registr
001930060706     D                                     INZ(*ON)
001931060706     D xnmErr          S              1                                         Esito
001932060706     D xnmNum          S              9  0                                      Numero
001933060706     D xnmAut          S              1                                         Automatico
001934060706     D xnmDag          S               D                                        Data aggiornamento
001935060706     D* Definizione DS per recupero msg generati dai moduli
001936060706     D QLenMsg         S              9B 0
001937060706     D QFormat         S              8
001938060706     D QCallMsg        S             10
001939060706     D QCallStack      S              9B 0
001940060706     D QMsgTyp         S             10
001941060706     D QMsgKey         S              4
001942060706     D QWait           S              9B 0
001943060706     D QAction         S             10
001944060706     D dvopedta        S            512
001945060804     D PARAM           DS                  INZ
001946060804     D* TIPOLOGIA T_=TRAZIONISTA
001947060804     D  PARTIP                 1      2
001948060804     D* RAGIONE SOCIALE TRAZIONISTA
001949060804     D  PARRSC                 3     37
001950060804     D* S.I. TRAZIONISTA
001951060804     D  PARCSF                38     40
001952060804     D* CODICE TRAZIONISTA
001953060804     D  PARPDR                41     47  0
001954060804     D* PARFLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
001955060804     D*                  ALFABETICA RICHIESTA
001956060804     D  PARFLG                48     48
001957060706     D QErrCod         DS
001958060706     D  Q1ErrLen                      9B 0 INZ(16)
001959060706     D  Q1LenRet                      9B 0 INZ(16)
001960060706     D  Q1ErrId                       7
001961060706     D*-------
001962060706     D QInfo           DS
001963060706     D  Q2BytRet                      9B 0
001964060706     D  Q2BytAva                      9B 0
001965060706     D  Q2Severity                    9B 0
001966060706     D  Q2MsgId                       7
001967060706     D  Q2MsgTyp                      2
001968060706     D  Q2MsgKey                      4
001969060706     D  Q2Reserve                    15
001970060706     D  Q2MsgLen                      9B 0
001971060706     D  Q2MsgLenAv                    9B 0
001972060706     D  Q2MsgTxt                     80
001973060707     D xatb            PR                                                       Tabelle Proj
001974060707     D                                     EXTPROC('XATB')
001975060707     D  a
001976060707     D                                     LIKE(xatbds)
002000021108      *
002100021112     D KPJBA         E DS
002101060707     D xatbds        E DS
002102060706     D WData0          C                   CONST('0001-01-01')
002103060804     D DSFMT           DS
002104060804     D  $TASTO               369    369
002105060804     D  NRG                  370    370
002106060804     D  NCL                  371    371
002107060804     D*-------------
002108060804     D* posizione cursore
002109060804     D CURSOR          DS
002110060804     D  RIRI                   1      2B 0 INZ
002111060804     D  R$$                    2      2
002112060804     D  COCO                   3      4B 0 INZ
002113060804     D  C$$                    4      4
002300930811     C*---------------------------------------------------------------*
002301060704     c     inizio        tag
002500060704     C                   setoff                                       9910
002501060707     C                   seton                                        44
002502060803     c* ricevuto parametri
002700021111     c                   clear                   vfrn
002701060707     c                   clear                   vfrnd
002800021108     c                   clear                   vimp
002900060704     c                   clear                   vddc
002901060706     c                   clear                   vdcm
002902060803     c                   EVAL      VCAU = 'A'
002904060803     c                   if        d61frn <>' '
002905060803     c                   move      d61frn        trepdr
002906060804     c                   move      d61frn        vfrn
002907061110     c                   eval      vsoc = d61soc
002908060803     c                   move      d61ddc        dataiso
002909060804     c                   move      dataiso       treddc
002910060804     c                   if        dataiso <> *loval
002911060804     c                   move      dataiso       dataeur
002912060804     c                   move      dataeur       vddc
002913060804     c                   end
002914060803     c                   eval      trecre = d61ret
002915060803     c     ktre          chain     tntre01l
002916060803     c                   if        %found(tntre01l)
002917060803     c                   movel     d61frn        vfrn
002918061110     c                   eval      vsoc = d61soc
002919060803     c                   eval      vimp = tretim
002920060803     c                   movel     treddc        dataiso
002921060803     c                   movel     dataiso       dataeur
002922060803     c                   movel     dataeur       vddc
002923060803     c                   movel     tredcm        dataiso
002924060803     c                   movel     dataiso       dataeur
002925060803     c                   movel     dataeur       vdcm
002926060803     c                   EVAL      VCAU = trecau
002927060803     c                   end
002928060803     c                   end
003100060704     c                   z-add     §bltmax       maxret
003200930907     C*
003300060704     C                   do        *hival
003401060705     C                   EXFMT     D01
003600060704     C** FINE LAVORO
003700060704     C   KC              leave
003701060804     C*  effettua ricerche per trazionista
003702060804     c                   if        *inkd = *on
003703060804     c                   exsr      search
003704060804     C   98              iter
003705060804     c                   end
005000021108     C* controlli sui campi a video
005100021108     C                   EXSR      Contr1
005200021108     c  nkf
005201060720     cannkq
005300060704     Cor 99              iter
005301060720     c* cancello
005302060720     c     *inkq         ifeq      *on
005303060720     C                   exsr      srdel
005304060720     c                   goto      inizio
005305060720     c                   end
005306060720     c* confermo
005307060704     c     *inkf         ifeq      *on
005308060704     c                   exsr      srsfl
005309060704     C   KC              leave
005310060704     c                   goto      inizio
005311060704     c                   end
005800060704     c                   enddo
005900060804     c                   SETON                                        LR
005901060804     C************************************************************
005902060804     C* RICERCHE
005903060804     C************************************************************
005904060804     C     SEARCH        BEGSR
005905060804     C*
005906060804     C* determino Riga/Colonna del cursore
005907060804     C                   MOVE      nrg           R$$
005908060804     C                   MOVE      ncl           C$$
005909060804     C                   Z-ADD     RIRI          CURR
005910060804     C                   Z-ADD     COCO          CURC
005911060804     C*
0059120608041    C                   SELECT
005913060804     C* FMTD1  - ricerca trazionista
005914060804    >C     H1NMFL        WHEneq    'VFRN'
005915060804     C*  deposita la KPJBU
005916060804     C                   MOVEL     KPJBU         DKPJBU          256
005917060804     C                   MOVEl     *blanks       PARRSC
005918060804     C                   Z-ADD     *ZEROS        PARPDR
005919060804     C                   eval      partip = 'T '
005920060804     C                   movel     vsoc          parcsf
005921060804     C                   MOVEL     *BLANKS       PARFLG
005922060804     C                   MOVEL     PARAM         KPJBU
005923060804     C                   CALL      'TRUL16R'
005924060804     C                   PARM                    KPJBA
005925060804     C                   MOVEL     KPJBU         PARAM
005926060804IF  2C     PARFLG        IFEQ      '3'
005927060804     C                   MOVEL     *BLANKS       VFRN
005928060804     C                   MOVEL     *BLANKS       VFRND
005929060804     C                   SETON                                        0299
005930060804X   2C                   ELSE
005931060804     C                   move      PARPDR        vfrn
005932060804     C                   movel     '0'           vfrn
005933060804     C                   MOVEL     PARRSC        vFRND
005934060804E   2C                   ENDIF
005935060804     C* REIMPOSTA LA KPJBU
005936060804     C                   MOVEL     DKPJBU        KPJBU
005937060804     C                   SETON                                        98
005938060804     C                   ENDsl
005939060804     C                   ENDsr
006000021108     C*---------------------------------------------------------------*
006100021108      *    CONTROLLI
006200021108     C*---------------------------------------------------------------*
006300021108     C     Contr1        BEGSR
006400021108     C*
006401060704     C                   setoff                                       99
006402060704     c                   do
006500021108     C* Controlla Società
006600021111     C     Vsoc          Ifeq      *blank
006700021111     C                   SETON                                        0199
006800060704     C                   leave
007500021111     C                   Endif
007501061025     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
007502061025     C                   movel     'F     '      $kcc              6
007503061025     C                   movel     *blanks       $ksc              8
007504061025     C                   callb     'XCAPCLIFOR'
007505061025     C                   parm                    vsoc
007506061025     C                   parm                    $kcc
007507061025     C                   parm                    $ksc
007508061025     C                   movel     $kcc          forita            6
007600021108     C*
007700021108     C* Controlla Fornitore
007800021111     c                   clear                   vfrnd
007900021111     C     Vfrn          Ifeq      *blank
008000021111     C                   SETON                                        0299
008100060704     C                   leave
008200021111     C                   Else
008201060704     C                   move      vfrn          kksc
008202060704     c                   movel     *blank        sogdes
008203060704     C     Kfrn          CHAIN     anfrn01l                           21
008204060704     C  N21Krco          CHAIN     anrco01l                           21
008205060704     C  N21rcosogg       CHAIN     ansog01l                           21
008206060704     C                   MOVEL(p)  sogdes        vfrnd
008207060704     C     *IN21         IFEQ      '1'
008208060704     C                   SETON                                        0299
008209060704     C                   leave
008210060704     C                   END
008211060704     C                   if        rcoctgan02 <> 'T   '
008214060704     C                   SETON                                        0299
008215060704     C                   leave
008216060704     c                   end
010500021111     C                   Endif
010700060706     C* Controlla  data rettifica
010701060704     c     *eur          test(d)                 vddc                   03
010702060704     c                   if        *in03
010703060704     c     *dmy          test(d)                 vddc                   03
010709060704     c  n03*dmy          move      vddc          dataeur
010710060704     c  n03              move      dataeur       vddc
010711060706     c                   else
010712060706     c                   move      vddc          dataeur
010713060705     c                   end
010714060704     c                   if        *in03
010715060704     c                   seton                                          99
010716060704     c                   leave
010717060704     c                   end
010718090204     c                   EXTRCT    dataeur:*y    annoddc           4 0
010719060706     c                   move      dataeur       dataiso
010720060707     c                   move      dataiso       com08             8 0
010721060707     c                   move      01            com08
010722060707     c                   movel     com08         comini            6 0
010723060707     c                   move      vfrn          tstpdr
010724060706     C* Controllo che esista un foglio da contabilizzare nel mese
010725060706     c                   clear                   ok                1 0
010726060707     c     ktst          setll     tntst02l
010727060706     c                   do        *hival
010728060707     c     tstpdr        reade     tntst02l
010729060707     c                   if        %eof(tntst02l)
010730060706     c                   leave
010731060706     c                   end
010732060706     c* cerco il primo traino valido da contab.
010733060707     c                   movel     tstdtn        comdtn            6 0
010734060707     C                   if        comdtn > comini
010735060707     c                   leave
010736060707     c                   end
010737060706     C                   IF        tstcnf <> ' 'and tstico > 1 and tstnft =0
010738060706     C                             and comdtn = comini
010739060706     c                   z-add     1             ok
010740060706     c                   leave
010741060706     c                   end
010742060706     c                   enddo
010752060706     c* controllo che esista un traino da contabilizzare nel mese della ret.
010753060706     c                   if        ok = 0
010754060706     c                   seton                                        0799
010755060706     c                   leave
010756060706     c                   end
010757060706     C* Controlla  data competenza
010758060720     c                   if        not *inkq
010759060706     c     *eur          test(d)                 vdcm                   04
010760060706     c                   if        *in04
010761060706     c     *dmy          test(d)                 vdcm                   04
010762060706     c  n04*dmy          move      vdcm          dataeur
010763060706     c  n04              move      dataeur       vdcm
010764060706     c                   else
010765060706     c                   move      vdcm          dataeur
010766060706     c                   end
010767060706     c                   if        *in04
010768060706     c                   seton                                          99
010769060706     c                   leave
010770060706     c                   end
010771060706      *  Data competenza < data bollato
010772061130     c                   exsr      mvc104
010773060706     c                   if        esito = *on
010774060706     C                   leave
010775060706     c                   end
010776090204     c* se rettifica negativa la competenza non può essere dell'anno preced.
010777090204     c                   EXTRCT    dataeur:*y    annocmp           4 0
010778090204     c                   if        vimp < 0 and annoddc > annocmp
010779090204     C                   SETON                                        0899
010780090204     C                   leave
010781090204     c                   end
010782060706     c* causale contabile
010783060707     c                   clear                   xatbds
010784060707     c                   move      '1'           xtbric
010785060707     c                   move      *blank        xtbkey
010786060707     c                   move      'Y4Z'         xtbcod
010787060707     c                   movel(p)  vcau          xtbkey
010788060707     c                   call      'XATB'
010789060707     c                   parm                    xatbds
010790060707     c                   if        xtberr = '0'
010791060707     c                   movel     xtbuni        angy4z
010792060707     c* causale trazionisti
010793060707     C                   if        §4ZTA <> 'T'
010794061110     C                   SETON                                        0599
010795060707     C                   leave
010796060707     c                   end
010797060707     c                   else
010798061110     C                   SETON                                        0599
010799060707     C                   leave
010800060707     c                   end
014800021111     C*  se l'importo da rettificare supera la soglia impostata in tabella BLC
014900060704     c                   if        vimp > §BLtMAX or vimp = 0
015000021111     C                   SETON                                        0699
015100060704     C                   leave
015200021111     c                   end
015201060720     c                   end
015202060704     c                   enddo
015300930908     C*
015400060704     C                   ENDSR
015401060706     C************************************************************
015402061201     C* Chiamata al modulo di controllo data competenza
015403060706     C************************************************************
015404060706     C     MVC104        BEGSR
015405070117     C* non devo controllare l'esercizio quindi la chiamata X10ESER non
015406070117     c* servirebbe + ma la lascio per sicurezza
015413061201     c                   clear                   x10eseds
015414061201     c                   eval      X10DATA = dataiso
015415061201     c                   eval      X10SOCIETA = vsoc
015416061201     c                   eval      X10CTB = 'CG'
015417061201     C                   CALL      'X10ESER'
015418061201     C                   PARM                    x10eseds
015421061201     c                   if        dataiso < X10DTINES or
015422061201     c                             dataiso > X10DTFIES
015424061201     c                   end
015425070117     c* controllo la data di analitica
015428070118     C                   move      vdcm          Dataeur
015429070118     C                   move      dataeur       Dataiso
015431070118     C                   CALLB     'NDMVC104'
015432070117     C                   PARM                    vsoc
015433070118     C                   PARM                    ctb
015434070118     C                   PARM      '        '    Unita
015435070118     C                   PARM      'D'           TpReg
015437070117    >C                   PARM                    dataiso
015439070118     C                   PARM                    RegIVA
015440070118     C                   PARM                    TpRegIVA
015441070118     C                   PARM      '01'          Stato
015442070117     C                   PARM      *Off          GesMsg            1
015443070117     C                   PARM                    Esito             1
015444070117     c*
015445070117     c                   if        esito = '1'
015446070117     c                   seton                                        0499
015447070117     c                   end
015461060706     C*
015462060706     C                   ENDSR
015463060706     C************************************************************
015464060706     C* Recupero i msg sono stati generati dal modulo chiamato
015465060706     C************************************************************
015466060706     C     RestoreMsg    BEGSR
015467060706     C*
015468060706     C                   CALL      'QMHRCVPM'
015469060706     C                   PARM                    QInfo
015470060706     C                   PARM      113           QLenMsg
015471060706     C                   PARM      'RCVM0100'    QFormat
015472060706     C                   PARM      '*'           QCallMsg
015473060706     C                   PARM      0             QCallStack
015474060706     C                   PARM      '*DIAG'       QMsgTyp
015475060706     C                   PARM                    QMsgKey
015476060706     C                   PARM      0             QWait
015477060706     C                   PARM      '*REMOVE'     QAction
015478060706     C                   PARM                    QErrCod
015479060706     C*
015480060706     C                   ENDSR
015481060706     C************************************************************
015482060706     C* Gestione messaggi per il campo causale di testata
015483060706     C************************************************************
015484060706     C     GestErr       BEGSR
015485060706     C*
015486060706     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
015487060706     C                   MOVE      Q2MsgId       IdMsg
015488060706     C                   CALLB     'XRTVM'
015489060706     C                   PARM                    IDMSG            27
015490060706     C                   PARM                    TXTMSG          100
015491060706     C                   PARM                    ERRMSG            1
015492060706     C                   PARM                    *omit
015493060706     C                   PARM                    *omit
015494060706     C                   PARM      Q2msgtxt      Msgdta          100
015495060706     C                   IF        ErrMsg <> *on
015496060707     C*                  CAT       TxtMsg:1      $Msg
015497060707     C                   SETON                                        99
015498060706     C                   ENDIF
015499060706     C*
015500060706     C                   ENDSR
015501021108     C*---------------------------------------------------------------*
015600021108     C*  esegue la rettifica dopo doppia convalida F6
015700021108     C*---------------------------------------------------------------*
015800060704     c     srsfl         begsr
015900021108     C*
015901060704      *  Pulisco dati SFL
015902060704     C                   SETOFF                                       202102
015903060704     C                   WRITE     c01
015904060707     C                   SETON                                        202144
015905060705     c                   z-add     0             nrr               6 0
015906060705     C* CARICA RECORD da file
015907060705     c                   move      vddc          dataeur
015908060705     c                   move      dataeur       dataiso
015909060705     c                   move      dataiso       treddc
015910060705     c                   move      vfrn          trepdr
015911060705     c     ktre          setll     tntre01l
015912060705     c                   do        *hival
015913060705     c     ktre          reade     tntre01l
015914060705     c                   if        %eof(tntre01l)
015915060705     c                   leave
015916060705     c                   end
015917060705     c                   movel     trenot        vnote
015918060705     C                   ADD       1             nrr
015919060705     C                   WRITE     s01
015920060705     c                   enddo
015922060705     C* CARICA RECORD VUOTI NEL SUBFILE
015923060705     c                   clear                   vnote
015924060704     C                   DO        50
015925060704     C                   ADD       1             nrr
015926060704     C                   WRITE     s01
015927060704     C                   ENDdo
015928060704     c*
015929060704     c                   do        *hival
015930060705     C                   WRITE     d02
015931060704     C                   EXFMT     c01
015932060704     C*
015933060704     c                   if        *inkc or *inkl
015934060704     c                   leave
015935060704     c                   end
015936060704     c*
015937060704     C                   EXSR      CONTR
015938060704     c                   if        *in99 or not *inkf
015939060704     C                   iter
015940060704     c                   end
015941060704     c* conferma
015942060704     c                   if        *inkf
015943060704     c                   exsr      esegui
015944060704     c                   leave
015945060704     c                   end
0159460607043    C                   ENDdo
015947060704     C*
015948060704     C                   ENDSR
015949060720     C*---------------------------------------------------------------*
015950060720     C*  cancella  rettifica non contabilizzate
015951060720     C*---------------------------------------------------------------*
015952060720     c     srdel         begsr
015953060720     C*
015959060720     C* CARICA RECORD da file
015960060720     c                   move      vddc          dataeur
015961060720     c                   move      dataeur       dataiso
015962060720     c                   move      dataiso       treddc
015963060720     c                   move      vfrn          trepdr
015964060720     c     ktre          setll     tntre01l
015965060720     c                   do        *hival
015966060720     c     ktre          reade     tntre01l
015967060720     c                   if        %eof(tntre01l)
015968060720     c                   leave
015969060720     c                   end
015970060720     c                   if        trenrg <> 0
015971060720     c                   iter
015972060720     c                   end
015975060720     C                   delete    tntre000
015976060720     c                   enddo
016002060720     C*
016003060720     C                   ENDSR
016004060704     C*----------------------------------------------------*
016005060704     C* Controlli i dati del subfile
016006060704     C*----------------------------------------------------*
016007060704     C     CONTR         BEGSR
016008060704      *
016009060704     C                   SETOn                                        99
016010060705     C                   DO        *hival        CNT               6 0
016011060705     C     CNT           CHAIN     s01                                10
016012060705     c                   if        *in10
016013060704     c                   leave
016014060704     c                   end
016015060705     c                   if        vnote <> ' '
016016060705     c                   setoff                                       99
016017060705     c                   leave
016018060705     c                   end
016019060704     C                   ENDdo
016020060704     c*
016021060704     C                   ENDSR
016022060704     C*---------------------------------------------------------------*
016023060704     C*  esegue la rettifica dopo doppia convalida F6
016024060704     C*---------------------------------------------------------------*
016025060704     c     esegui        begsr
016026060705     c                   move      vddc          dataeur
016027060705     c                   move      dataeur       dataiso
016028060705     c                   move      dataiso       treddc
016029060705     c                   move      vfrn          trepdr
016030060705     c* pulisco eventualmente i record precedentemente scritti
016031060705     c     ktre          setll     tntre01l
016032060705     c                   do        *hival
016033060705     c     ktre          reade     tntre01l
016034060705     c                   if        %eof(tntre01l)
016035060705     c                   leave
016036060705     c                   end
016037060705     c                   delete    tntre000
016038060705     c                   enddo
016039060704     C*
016040060704     c                   z-add     0             trepno
016041060705     C                   DO        *hival        CNT
016042060705     C     CNT           CHAIN     s01                                10
016043060705     c                   if        *in10
016044060705     c                   leave
016045060704     c                   end
016046060705     c                   if        vnote = ' '
016047060705     c                   iter
016048060705     c                   end
016049021108     C*  rettifica un padroncino come fornitore
016050060803     c                   clear                   TRESOC
016051060803     c                   clear                   TRECTB
016052060803     c                   clear                   TREDRG
016053060803     c                   clear                   TRENRG
016054060803     c                   clear                   TRESRG
016056060705     c                   move      vfrn          trepdr
016057060705     c                   move      vddc          dataeur
016058060705     c                   move      dataeur       dataiso
016059060705     c                   move      dataiso       treddc
016060060707     c                   move      vdcm          dataeur
016061060707     c                   move      dataeur       dataiso
016062060707     c                   move      dataiso       tredcm
016063060707     c                   movel     vcau          trecau
016064060707     c                   movel     §bltcau       trecre
016065060705     c                   add       1             trepno
016066060705     c                   movel     vnote         trenot
016067060705     c                   if        trepno = 1
016068060705     c                   z-add     vimp          tretim
016069060705     c                   else
016070060705     c                   clear                   tretim
016071060705     c                   end
016072060705     c                   z-add     0             tredds
016073060705     c                   write     tntre000
016902060704     c                   enddo
017700021111     C*
017800021108     C                   Endsr
017900021108     C*---------------------------------------------------------------*
018000021108     c     *inzsr        begsr
018100021108     C*---------------------------------------------------------------*
018200020125     C     *ENTRY        PLIST
018300020125     C                   PARM                    KPJBA
018301060803     c                   movel     kpjbu         tntl61ds
018302060804     C                   Z-ADD     0             CURR              2 0
018303060804     C                   Z-ADD     0             CURC              2 0
018304061025     c     *dtaara       define    §azute        azuteds
018305061025     c     *dtaara       define    §datiute      ddatiute
018306061025     C                   in(E)     *dtaara
018307061025     C                   IF        %Error  or  RSUT = *blanks
018308061025     C                   call      'TIBS34R'
018309061025     C                   parm                    Tibs34Ds
018310061025     C                   in        *dtaara
018311061025     c                   ENDIF
018312061025     c* non è protetto solo se EDP xchè in tst il sistema inf. è GAITRAPSP
018313061025     c                   if        %subst(knmus: 1: 3) = 'EDP'
018314061025     c                   seton                                        88
018315061025     c                   else
018316061025     c                   setoff                                       88
018317061025     c                   end
018700021108      *
018800021108     C*---------- RICERCA DITTA :
018900061025     C*                  MOVEL     'SOC001'      TIPXSC
019000061025     C*                  MOVEL     *BLANK        SOCXSC
019100061025     C*                  EXSR      REPSOC
019200061025     C*    RTNXSC        IFNE      '1'
019300061025     C*                  MOVEL     XSOCDS        SOC001
019400061025     C*                  MOVEL     xscrgs        RSUT             20
019500061025     c*                  else
019600061025     c*                  return
019700061025     c*                  end
019701061025     c* reperisco la società dal s.i. nel quale si contabilizza
019702061025     c                   clear                   ds3i
019703061025     c                   z-add     1             tblkut
019704061025     c                   movel     '3I'          tblcod
019705061025     c                   movel(p)  '1'           tblkey
019706061025     c     ktab          chain     tabel00f
019707061025     c                   if        %found(tabel00f)
019708061025     c                   movel     tbluni        ds3i
019709061025     c                   eval      vsoc = %subst(§3IBST: 7: 3)
019710061025     c                   end
020600060704      *  lettura tabella BLT
020700060704     c                   clear                   dblt
020800060704     c                   movel     'BLT'         tbecod
020900021111     c                   movel(p)  '1'           tbeke1
021000021111     c                   clear                   tbeke2
021100021111     c     ktbe          chain     tntbe01l
021200021111     c                   if        %found(tntbe01l)
021300060704     c                   movel     tbeuni        dblt
021900021111     c                   end
024001061025     C*
024002061025     C     Ktab          KLIST
024003061025     C                   KFLD                    tblkut
024004061025     C                   KFLD                    tblcod
024005061025     C                   KFLD                    tblkey
024006061025     C*
024100021111     C     Kfrn          KLIST
024200061025     C                   KFLD                    vsoc
024300021111     C                   KFLD                    kksc
024301060707     C*
024302060707     C     Ktst          KLIST
024303060707     C                   KFLD                    tstpdr
024304060707     C                   KFLD                    com08
024305060705     C*
024306060705     C     Ktre          KLIST
024307060705     C                   KFLD                    trepdr
024308060705     C                   KFLD                    treddc
024309060705     C                   KFLD                    §bltcau
024400021111     C*
024500021111     C     Krco          KLIST
024600061025     C                   KFLD                    vsoc
024700021111     C                   KFLD                    forita
024800021111     C                   KFLD                    KKSC              8
024900021111     C*
025000021111     C     Ktbe          KLIST
025100021111     C                   KFLD                    tbecod
025200021111     C                   KFLD                    tbeke1
025300021111     C                   KFLD                    tbeke2
025400021111     C*
025500020125     c                   endsr
025600021108     C*---------------------------------------------------------------*
025700021108     C* Reperimento dati società
025800021108     C*---------------------------------------------------------------*
025900021108     C     REPSOC        BEGSR
026000021108     C*
026100021111     C                   CALLb     'XSOC'
026200021108     C                   PARM                    TIPXSC            6
026300021108     C                   PARM                    SOCXSC            3
026400021108     C                   PARM                    CDSXSC            9 0
026500021108     C                   PARM                    MODXSC            3
026600021108     C                   PARM      *blanks       RTNXSC            1
026700021108     C                   PARM                    XSOCDS
026800021108     C                   PARM                    KPJBA
026900021108     C*
027000021108     C                   ENDSR
