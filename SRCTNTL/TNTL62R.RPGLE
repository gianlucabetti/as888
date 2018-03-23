000100060707     H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND')
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000401060704     H*--------------------------------------------------------------*
000500060704     H*       Rettifiche autofatturazione trazionisti
000600000000     H*--------------------------------------------------------------*
000700060802     Ftntl62D   CF   E             WORKSTN
000701060704     F                                     SFILE(S01:NRR)
001200021113      *
001201060704     FAnFrn01l  if   E           K DISK
001203060704     FAnRco01l  if   E           K DISK
001204060704     FAnSog01l  if   E           K DISK
001400060802     Ftntre01l  if   E           k DISK
001401060802     FTnTbe01l  if   E           K DISK
001402061025     FTabel00f  if   E           K DISK
001600021108      *
001601061024     D ddatiute      e ds
001602061024     D azuteds       e ds                  extname(AZUTE00F)
001603061024     D tibs34ds      E DS                  inz
001700021108     D soc001        E DS                  EXTNAME(xsoc001ds)
001701061025     D ds3i          E DS
001702061025     D dblt          E DS
001703060802     D tntl61ds      E DS
001800021108     D xsocds          DS          1000
001902060704     Ddataeur                          d   datfmt(*eur)
001903060704     Ddataiso                          d   datfmt(*iso)
001904060706     D* Definizioni parametri chiamata moduli NDMVC104
001905060706    >D Societa         S              3
001906060706    >D Ctb             S              2
001907060706     D                                     INZ('CG')
001908060706    >D Unita           S              8
001909060706     D TpReg           S              1
001910060706     D DtReg           S             10
001911060706     D DtLGio          S             10
001912060706     D RegIVA          S              1
001913060706     D TpRegIVA        S              1
001914060706     D Stato           S              2
001915060706     D Esito           S              1
001916060706     D Gesmsg          S              1
001917060706     D xnmRic          S              1                                         Richiesta
001918060706     D                                     INZ('1')
001919060706     D xnmKey          S              8                                         Chiave gestionale
001920060706     D xnmCmt          S              1                                         Commit
001921060706     D                                     INZ(*OFF)
001922060706     D xnmDat          S               D                                        Data riferimento
001923060706     D xnmIva          S              1                                         Abilitazione IVA
001924060706     D                                     INZ(*ON)
001925060706     D xnmReg          S              1                                         Abilitazione registr
001926060706     D                                     INZ(*ON)
001927060706     D xnmErr          S              1                                         Esito
001928060706     D xnmNum          S              9  0                                      Numero
001929060706     D xnmAut          S              1                                         Automatico
001930060706     D xnmDag          S               D                                        Data aggiornamento
001931060706     D* Definizione DS per recupero msg generati dai moduli
001932060706     D QLenMsg         S              9B 0
001933060706     D QFormat         S              8
001934060706     D QCallMsg        S             10
001935060706     D QCallStack      S              9B 0
001936060706     D QMsgTyp         S             10
001937060706     D QMsgKey         S              4
001938060706     D QWait           S              9B 0
001939060706     D QAction         S             10
001940060706     D dvopedta        S            512
001941060803     D PARAM           DS                  INZ
001942060803     D* TIPOLOGIA T_=TRAZIONISTA
001943060803     D  PARTIP                 1      2
001944060803     D* RAGIONE SOCIALE TRAZIONISTA
001945060803     D  PARRSC                 3     37
001946060803     D* S.I. TRAZIONISTA
001947060803     D  PARCSF                38     40
001948060803     D* CODICE TRAZIONISTA
001949060803     D  PARPDR                41     47  0
001950060803     D* PARFLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
001951060803     D*                  ALFABETICA RICHIESTA
001952060803     D  PARFLG                48     48
001953060706     D QErrCod         DS
001954060706     D  Q1ErrLen                      9B 0 INZ(16)
001955060706     D  Q1LenRet                      9B 0 INZ(16)
001956060706     D  Q1ErrId                       7
001957060706     D*-------
001958060706     D QInfo           DS
001959060706     D  Q2BytRet                      9B 0
001960060706     D  Q2BytAva                      9B 0
001961060706     D  Q2Severity                    9B 0
001962060706     D  Q2MsgId                       7
001963060706     D  Q2MsgTyp                      2
001964060706     D  Q2MsgKey                      4
001965060706     D  Q2Reserve                    15
001966060706     D  Q2MsgLen                      9B 0
001967060706     D  Q2MsgLenAv                    9B 0
001968060706     D  Q2MsgTxt                     80
001969060707     D xatb            PR                                                       Tabelle Proj
001970060707     D                                     EXTPROC('XATB')
001971060707     D  a
001972060707     D                                     LIKE(xatbds)
002000021108      *
002100021112     D KPJBA         E DS
002101060707     D xatbds        E DS
002102060804     D DSFMT           DS
002103060804     D  $TASTO               369    369
002104060804     D  NRG                  370    370
002105060804     D  NCL                  371    371
002106060804     D*-------------
002107060804     D* posizione cursore
002108060804     D CURSOR          DS
002109060804     D  RIRI                   1      2B 0 INZ
002110060804     D  R$$                    2      2
002111060804     D  COCO                   3      4B 0 INZ
002112060804     D  C$$                    4      4
002300930811     C*---------------------------------------------------------------*
002301060704     c     inizio        tag
002500060704     C                   setoff                                       9910
002700021111     c                   clear                   vfrn
002701060707     c                   clear                   vfrnd
003200930907     C*
003300060704     C                   do        *hival
003401060705     C                   EXFMT     D01
003600060704     C** FINE LAVORO
003700060704     C   KC              leave
003701060803     C*  effettua ricerche per trazionista
003702060803     c                   if        *inkd = *on
003703060803     c                   exsr      search
003704060803     C   98              iter
003705060803     c                   end
005000021108     C* controlli sui campi a video
005100021108     C                   EXSR      Contr1
005300060802     C   99              iter
005301060804     C   kj              exsr      srins
005302060802     c                   exsr      srsfl
005303060802     C   kc              leave
005800060704     c                   enddo
005900021108     C                   SETON                                        LR
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
015202060704     c                   enddo
015300930908     C*
015400060704     C                   ENDSR
015500021108     C*---------------------------------------------------------------*
015600021108     C*  esegue la rettifica dopo doppia convalida F6
015700021108     C*---------------------------------------------------------------*
015800060704     c     srsfl         begsr
015900021108     C*
015901060704      *  Pulisco dati SFL
015902060704     C                   SETOFF                                       202102
015903060704     C                   WRITE     c01
015904060707     C                   SETON                                        202144
015905060705     c                   z-add     0             nrr               6 0
015907060705     C* CARICA RECORD da file
015908060803     c                   move      vfrn          trepdr
015909060803     c     trepdr        setll     tntre01l
015910060802     c                   do        *hival
015911060803     c     trepdr        reade     tntre01l
015912060802     c                   if        %eof(tntre01l)
015913060802     c                   leave
015914060802     c                   end
015915060802     c                   if        trepno = 1
015916060802     c                   clear                   vsce
015917060802     c                   move      treddc        dataiso
015918060802     c                   move      dataiso       dataeur
015919060802     c                   move      dataeur       vddc
015920060803     c                   move      tretim        vimp
015921060705     c                   movel     trenot        vnote
015922060802     c                   movel     trecre        cauret
015923060705     C                   ADD       1             nrr
015924060705     C                   WRITE     s01
015925060802     c                   end
015926060705     c                   enddo
015929060704     c*
015930060803     c                   if        nrr > 0
015931060704     c                   do        *hival
015932060705     C                   WRITE     d02
015933060704     C                   EXFMT     c01
015934060704     C*
015935060704     c                   if        *inkc or *inkl
015936060704     c                   leave
015937060704     c                   end
015938060704     c*
015939060704     C                   EXSR      CONTR
015940060803     c                   if        ok = '1'
015941060803     c                   leave
015942060803     c                   end
0159470607043    C                   ENDdo
015948060803     c                   end
015949060704     C*
015950060704     C                   ENDSR
015951060804     C*---------------------------------------------------------------*
015952060804     C*  inserimento
015953060804     C*---------------------------------------------------------------*
015954060804     c     srins         begsr
015955060804     C*
015956060804     c                   clear                   tntl61ds
015959060804     c                   move      '0001-01-01'  d61ddc
015960061110     c                   eval      d61soc = vsoc
015961061110     c                   move      vfrn          d61frn
015962060804     c                   move      §BLTCAU       d61ret
015963060804     c                   movel(p)  tntl61ds      kpjbu
015964060804     c                   call      'TNTL61R'
015965060804     c                   parm                    kpjba
015997060804     C*
015998060804     C                   ENDSR
016004060704     C*----------------------------------------------------*
016005060704     C* Controlli i dati del subfile
016006060704     C*----------------------------------------------------*
016007060704     C     CONTR         BEGSR
016008060803     c                   clear                   ok                1
016009060704      *
016010060705     C                   DO        *hival        CNT               6 0
016011060705     C     CNT           CHAIN     s01                                10
016012060705     c                   if        *in10
016013060704     c                   leave
016014060704     c                   end
016016060802     c                   if        vsce <> ' '
016017061110     c                   clear                   tntl61ds
016018060802     c                   move      vddc          dataeur
016019060802     c                   move      dataeur       dataiso
016020060802     c                   move      dataiso       d61ddc
016021061110     c                   eval      d61soc = vsoc
016022060802     c                   move      vfrn          d61frn
016023060802     c                   move      cauret        d61ret
016024060802     c                   movel(p)  tntl61ds      kpjbu
016025060802     c                   call      'TNTL61R'
016026060802     c                   parm                    kpjba
016027060803     c                   movel     ' '           vsce
016028060803     c                   update    s01
016029060803     c                   eval      ok = '1'
016030060803     c                   leave
016031060705     c                   end
016032060704     C                   ENDdo
016033060704     c*
016034060704     C                   ENDSR
016035060803     C************************************************************
016036060803     C* RICERCHE
016037060803     C************************************************************
016038060803     C     SEARCH        BEGSR
016039060803     C*
016040060803     C* determino Riga/Colonna del cursore
016041060803     C                   MOVE      nrg           R$$
016042060803     C                   MOVE      ncl           C$$
016043060803     C                   Z-ADD     RIRI          CURR
016044060803     C                   Z-ADD     COCO          CURC
016045060803     C*
0160480608031    C                   SELECT
016049060803     C* FMTD1  - ricerca trazionista
016050060804    >C     H1NMFL        WHEneq    'VFRN'
016055060803     C*  deposita la KPJBU
016056060803     C                   MOVEL     KPJBU         DKPJBU          256
016057060804     C                   MOVEl     *blanks       PARRSC
016058060803     C                   Z-ADD     *ZEROS        PARPDR
016059060803     C                   eval      partip = 'T '
016060060804     C                   movel     vsoc          parcsf
016061060803     C                   MOVEL     *BLANKS       PARFLG
016062060803     C                   MOVEL     PARAM         KPJBU
016063060803     C                   CALL      'TRUL16R'
016064060803     C                   PARM                    KPJBA
016065060803     C                   MOVEL     KPJBU         PARAM
016066060803IF  2C     PARFLG        IFEQ      '3'
016067060804     C                   MOVEL     *BLANKS       VFRN
016068060804     C                   MOVEL     *BLANKS       VFRND
016069060803     C                   SETON                                        0299
016070060803X   2C                   ELSE
016071060803     C                   move      PARPDR        vfrn
016072060803     C                   movel     '0'           vfrn
016073060803     C                   MOVEL     PARRSC        vFRND
016074060803E   2C                   ENDIF
016075060803     C* REIMPOSTA LA KPJBU
016076060803     C                   MOVEL     DKPJBU        KPJBU
016077060803     C                   SETON                                        98
016093060803     C                   ENDsl
016094060803     C                   ENDsr
017900021108     C*---------------------------------------------------------------*
018000021108     c     *inzsr        begsr
018100021108     C*---------------------------------------------------------------*
018200020125     C     *ENTRY        PLIST
018300020125     C                   PARM                    KPJBA
018301060804     C                   Z-ADD     0             CURR              2 0
018302060804     C                   Z-ADD     0             CURC              2 0
018303061024     C*
018304061024     c     *dtaara       define    §azute        azuteds
018305061024     c     *dtaara       define    §datiute      ddatiute
018306061024     C                   in(E)     *dtaara
018307061024     C                   IF        %Error  or  RSUT = *blanks
018308061024     C                   call      'TIBS34R'
018309061024     C                   parm                    Tibs34Ds
018310061024     C                   in        *dtaara
018311061024     c                   ENDIF
018312061025     c* non è protetto solo se EDP xchè in tst il sistema inf. è GAITRAPSP
018313061025     c                   if        %subst(knmus: 1: 3) = 'EDP'
018314061025     c                   seton                                        88
018315061025     c                   else
018316061025     c                   setoff                                       88
018317061025     c                   end
018700021108      *
018800021108     C*---------- RICERCA DITTA :
018900061024     C*                  MOVEL     'SOC001'      TIPXSC
019000061024     C*                  MOVEL     *BLANK        SOCXSC
019100061024     C*                  EXSR      REPSOC
019200061024     C*    RTNXSC        IFNE      '1'
019300061024     C*                  MOVEL     XSOCDS        SOC001
019400061024     C*                  MOVEL     xscrgs        RSUT             20
019500061024     c*                  else
019600061024     c*                  return
019700061024     c*                  end
019707061024     c* reperisco la società dal s.i. nel quale si contabilizza
019708061025     c                   clear                   ds3i
019709061025     c                   z-add     1             tblkut
019710061025     c                   movel     '3I'          tblcod
019711061025     c                   movel(p)  '1'           tblkey
019712061025     c     ktab          chain     tabel00f
019713061025     c                   if        %found(tabel00f)
019714061025     c                   movel     tbluni        ds3i
019715061025     c                   eval      vsoc = %subst(§3IBST: 7: 3)
019716061025     c                   end
020501060802      *  lettura tabella BLT
020502060802     c                   clear                   dblt
020503060802     c                   movel     'BLT'         tbecod
020504060802     c                   movel(p)  '1'           tbeke1
020505060802     c                   clear                   tbeke2
020506060802     c     ktbe          chain     tntbe01l
020507060802     c                   if        %found(tntbe01l)
020508060802     c                   movel     tbeuni        dblt
020509060802     c                   end
020510060802     C*
020511061025     C     Ktab          KLIST
020512061025     C                   KFLD                    tblkut
020513061025     C                   KFLD                    tblcod
020514061025     C                   KFLD                    tblkey
020515061025     C*
020516061025     C     Ktbe          KLIST
020517061025     C                   KFLD                    tbecod
020518061025     C                   KFLD                    tbeke1
020519061025     C                   KFLD                    tbeke2
024000021111     C*
024100021111     C     Kfrn          KLIST
024200061025     C                   KFLD                    vsoc
024300021111     C                   KFLD                    kksc
024400021111     C*
024500021111     C     Krco          KLIST
024600061025     C                   KFLD                    vsoc
024700021111     C                   KFLD                    forita
024800021111     C                   KFLD                    KKSC              8
025400021111     C*
025500020125     c                   endsr
025600021108     C*---------------------------------------------------------------*
025700021108     C* Reperimento dati società
025800021108     C*---------------------------------------------------------------*
025900021108     C     REPSOC        BEGSR
026000021108     C*
026100061024     C                   CALLb     'XSOC'
026200061024     C                   PARM                    TIPXSC            6
026300061024     C                   PARM                    SOCXSC            3
026400061024     C                   PARM                    CDSXSC            9 0
026500061024     C                   PARM                    MODXSC            3
026600061024     C                   PARM      *blanks       RTNXSC            1
026700061024     C                   PARM                    XSOCDS
026800061024     C                   PARM                    KPJBA
026900021108     C*
027000021108     C                   ENDSR
