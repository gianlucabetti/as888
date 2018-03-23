000200110719     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) Option(*nodebugio)
000300110803     ftmmoc03l  uf   e           k disk
000301110803     ftmsmp01l  uf   e           k disk
000406110803     fxx2liv2p  uf   e           k disk
000407110803     fprtf198   o    f  198        printer infds(stamp1) usropn
000500030110
000501110719     d kpjba         e ds
000504110719
001100030108     d dataiso         s               d   datfmt(*iso)
001200030108     d dataeur         s               d   datfmt(*eur)
001201110719     D WLBDAT          DS
001202110719     D  G06DAT                 1      6  0
001203110719     D  G06INV                 7     12  0
001204110719     D  G06ERR                13     13
001205110719     D  G06TGI                14     18  0
001900030109     d
001901110803     d stamp1          ds
001902110803     d  lin1                 367    368b 0
002000000607      *--------------------------------------------------------------------
002100000607     c
002200110719      * lettura file ordini da trasmettere per prelievi a logistica
002300110803     c     *loval        setll     xx2liv2p
002301110719     c                   do        *hival
002302110803     c                   read      xx2liv2p
002303110803     c                   if        %eof(xx2liv2p)
002304110719     c                   leave
002305110719     c                   endif
002306110803     c                   movel     x2spec        wcomo9            9
002307110803     c                   testn                   wcomo9               30
002308110803     c  n30              except    errAssol
002309110803     c  n30              iter
002310110803     c                   movel     wcomo9        numriga           9 0
002311110803     c
002312110803      * riga per riga verifica se è stata evasa
002314110803     C     numriga       chain     tmmoc03l
002315110803     c                   if        not %found(tmmoc03l)
002316110803     c                   except    errCorr
002317110803     c                   iter
002318110803     c                   endif
002319110803     c                   select
002320110803      *riga non evasa
002321110803     c                   when      x2qluc = 0
002322110803     c                   exsr      rstsal
002323110803
002324110803      *riga evasa
002325110803     c                   when      x2qluc = mocqta
002326110803     c                   exsr      aggsal
002327110803     c                   other
002328110803      *quantità non corrispondenti errore
002329110803     c                   except    errQta
002330110803     c                   iter
002331110803     c                   endsl
002332110803
002337110803     c                   enddo
002338110803     c
002900020521     c                   seton                                        lr
002901110803     C**----------------------------------------------------
002902110803      * aggiorna saldi
002903110803     C*-----------------------------------------------------
002904110803     C     aggsal        begsr
002905110803
002907110803     C     KEYSAL        CHAIN     tmsmp000                           5226
002909110803     C     SMPQIM        SUB       MOCQTA        SMPQIM
002910110803     C*--------- AGGIORNAMENTO ENTRATA --------------------*
002911110803     C     1             DO        5             X                 2 0          *
002912110803     C     AZE(X)        COMP      '+'                                    92    *
002913110803     C     AZE(X)        COMP      '-'                                    91    *
002914110803     C   92EN(X)         ADD       MOCQTA        EN(X)                          *
002915110803     C   91EN(X)         SUB       MOCQTA        EN(X)                          *
002916110803     C                   END                                                    *
002917110803     C*--------- AGGIORNAMENTO USCITA ---------------------*
002918110803     C     1             DO        5             X                              *
002919110803     C     AZU(X)        COMP      '+'                                    92    *
002920110803     C     AZU(X)        COMP      '-'                                    91    *
002921110803     C   92US(X)         ADD       MOCQTA        US(X)                          *
002922110803     C   91US(X)         SUB       MOCQTA        US(X)                          *
002923110803     C                   END                                                    *
002924110803     C     AZG(5)        COMP      '+'                                    92    *
002925110803     C     AZG(5)        COMP      '-'                                    91    *
002926110803     C   92SMPQGF        ADD       MOCQTA        SMPQGF
002927110803     C   91SMPQGF        SUB       MOCQTA        SMPQGF
002928110803     C                   UPDATE    tmsmp000
002929110803     C                   Z-ADD     NUMBOL        MOCNBL
002930110803     C                   Z-ADD     DATDEM        MOCDBL
002931110803     C                   Z-ADD     NUMMAG        MOCCRG
002932110803     c                   move      'FA'          mocfas
002933110803     c                   update    tmmoc000
002934110803     C                   ENDSR
002935110803     C***------------------------------------------------***
002936110803      * riga non evasa storno sul saldo
002937110803     C*-----------------------------------------------------
002938110803     C     Rstsal        begsr                                                  *
002939110803     c                   if        anndta4 = 0
002940110803     c                   z-add     anndta        anndta4           4 0
002941110803     c                   if        anndta4 > 40
002942110803     c                   add       1900          anndta4
002943110803     c                   else
002944110803     c                   add       2000          anndta4
002945110803     c                   end
002946110803     c                   end
002947110803     C     KEYSAL        CHAIN     tmsmp01L                           39        *
002948110803     C   39              DO                                                     *
002949110803     C                   MOVE      ANNDTA4       SMPAAC                         *
002950110803     C                   MOVE      SC2MAG        SMPMAG                         *
002951110803     C                   MOVE      SC2MAT        SMPMAT                         *
002952110803     C                   MOVE      AMPUMA        SMPUMA                         *
002953110803     C                   Z-ADD     0             SMPPLF                         *
002954110803     C                   Z-ADD     0             SMPPLV                         *
002955110803     C                   Z-ADD     0             SMPQGI                         *
002956110803     C                   Z-ADD     0             SMPQOR                         *
002957110803     C                   Z-ADD     0             SMPQPR                         *
002958110803     C                   Z-ADD     0             SMPQIM                         *
002959110803     C                   Z-ADD     0             SMPQIP                         *
002960110803     C                   Z-ADD     0             SMPQGF                         *
002961110803     C                   Z-ADD     0             SMPQE1                         *
002962110803     C                   Z-ADD     0             SMPQE2                         *
002963110803     C                   Z-ADD     0             SMPQE3                         *
002964110803     C                   Z-ADD     0             SMPQE4                         *
002965110803     C                   Z-ADD     0             SMPQE5                         *
002966110803     C                   Z-ADD     0             SMPQU1                         *
002967110803     C                   Z-ADD     0             SMPQU2                         *
002968110803     C                   Z-ADD     0             SMPQU3                         *
002969110803     C                   Z-ADD     0             SMPQU4                         *
002970110803     C                   Z-ADD     0             SMPQU5                         *
002971110803     C                   Z-ADD     woggig        SMPDMS                         *
002972110803     C                   Z-ADD     0             SMPNMS                         *
002973110803     C                   Z-ADD     0             SMPPMC                         *
002974110803     C                   MOVE      *BLANKS       SMPFLG                         *
002975110803     C                   END                                                    *
002976110803     C*---                                                 *
002977110803     C                   Z-ADD     0             QTSALD           11 2          *
002978110803     C     MOCQTA        MULT(H)   AMPFCQ        QTSALD                         *
002979110803     C     MOCFAS        IFEQ      'OR'                                         *
002980110803     C  N39SMPQPR        ADD       QTSALD        SMPQPR                         *
002981110803     C   39              Z-ADD     QTSALD        SMPQPR                         *
002982110803     C                   END                                                    *
002983110803     C     MOCFAS        IFEQ      'AS'                                         *
002984110803     C  N39SMPQIM        ADD       QTSALD        SMPQIM                         *
002985110803     C   39              Z-ADD     QTSALD        SMPQIM                         *
002986110803     C                   END                                                    *
002987110803     C     MOCFAS        IFEQ      'BB'                                         *
002988110803     C                   GOTO      ENDSAL
002989110803     C                   END                                                    *
002990110803     C     MOCFAS        IFEQ      'FA'                                         *
002991110803     C                   GOTO      ENDSAL
002992110803     C                   END                                                    *
002993110803     C**--- AGGIORNO SALDI M.P. --------------------------**
002994110803     C                   MOVE      ' '           SMPATT                         *
002995110803     C                   move      xsccut        SMPUTE                         *
002996110803     C                   Z-ADD     woggig        SMPDMN                         *
002997110803     C  N39              UPDATE    tmsmp000                                     *
002998110803     C   39              WRITE     tmsmp000                                     *
002999110803     C                   Z-ADD     NUMBOL        MOCNBL
003000110803     C                   Z-ADD     DATDEM        MOCDBL
003001110803     C                   Z-ADD     NUMMAG        MOCCRG
003002110803     c                   move      'FA'          mocfas
003003110803     c                   update    tmmoc000
003004110803     CSR   ENDSAL        ENDSR                                                  *
009700020521      *-------------------------------------------------
009800020521     c     *inzsr        begsr
009900020521      *-------------------------------------------------
010000020626     c     *entry        plist
010100110719     c                   parm                    kpjba
010200030414
014600020521     c                   endsr
014700030414      *-------------------------------------------------
