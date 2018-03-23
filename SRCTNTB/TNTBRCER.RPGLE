000201150929     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJCBND')
000202150929     H BNDDIR('PJXBND')
000300020726     H decedit(',') datedit(*dmy)
000400891030     F*--------------------------------------------------------------*
000500020726     F* GESTIONE TABELLA Y4O
000600891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000700891030     F*  01        INSERIMENTO                                       *
000800891030     F*  02        VARIAZIONE                                        *
000900020726     F*  03        annullamento                                      *
001000891107     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001100891030     F*--------------------------------------------------------------*
001200150928     FTntbRCED  CF   E             WORKSTN
001300020726     Fanuni01l  IF   E           K DISK
001301150929     Fanvci01l  IF   E           K DISK
001400020726     Fansif01l  IF   E           K DISK
001500020726     Fanope01l  IF   E           K DISK
001600020726     FTntbe01l  UF A E           K DISK
001700891030     D*--------------------------------------------------------------*
001800891030     D*                        I N P U T                             *
001900891030     D*--------------------------------------------------------------*
002000020726     D TIBS02DS      E DS
002200891030     D KPJBA         E DS
002300020726     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400020726     D                SDS
002500020726     D  VTCPGM                 1     10
002600020726     D* Dati di ambiente ottenuti da XSOC
002700150928    >D dtce          E DS
002701150928     d dsemail       e ds
002702150928    >D xsoc001ds     E DS
002800020726     D* DS Interna per dati di output di XSOC
002900020726     D XSOCDS          DS          1000
003000020726     D* Descrizione conti
003100020726     D ND002DS       E DS
003101150929     D ana200ds      E DS
003102150929     D ana250ds      E DS
003103150929     D ana280ds      E DS
003104150929     D ana032ds      E DS
003105150928     D ana009ds      E DS
003106150929     D ana070ds      E DS
003200020726     D*-------------
003300020726     D* Definizioni parametri standard chiamate moduli
003400020726     D $Esito          S              1
003500020726     D $gesmsg         S              1
003600020726     D $lenout         S              9B 0
003700020726     D $struttura      S             10
003800020726     D $kcc            S              6
003900020726     D $ksc            S              8
004000020726     D Wdate           S               D
004001150929     D valore          S                   like(tbeuni)
004100971210     C******************************************************
004200020726     C                   DO        *HIVAL
004300150928     C                   MOVEL     *BLANKS       Vtre
004400150928     C                   MOVEL     *blanks       destre
004600911115     C                   EXFMT     TB31D1
004700020726     C   KC              LEAVE
004800020726     C* CONTROLLI 1° VIDEO
004900020726     C                   EXSR      CTR01
005000150928     C   99              iter
005100020726     C*
005200020726     C                   SETOFF                                       010203
005300020726     C                   EXSR      PULVID
005400020726     C*
005401150928     c                   movel(p)  'RCE'         tbecod
005402150928     c                   movel(p)  vtre          tbeke1
005500150928     C     key1          CHAIN     Tntbe01L
005600020726     C* INSERIMENTO
005700020726     C                   IF        NOT %FOUND(TNTBE01L)
005800020726     C                   SETON                                        01        *IMMISSIONE
005900020726     C                   ELSE
005901150929     c                   exsr      sriempi
005902150929     C                   EXSR      CTRFMT
006000020726     C*
006500020726     C* RECORD ANNULLATO
006600020726     C     TBEATB        IFNE      ' '
006700020726     C                   SETON                                        03
006800020726     C                   ELSE
006900020726     C* MANUTENZIONE
007000020726     C                   SETON                                        02        *VARIAZIONE
007100020726     C                   END
007200020726     C                   END
007300020726     C* 2° VIDEO
007400150928     C                   do        *hival
007500911115     C                   EXFMT     TB31D2
007600150929     C   KC              exsr      uscita
007700150929     C   KL              leave
007701150929     c*
007703150929     C   Kd              exsr      search
007704150929     C   Kd              iter
007800891030     C**--- ANNULLAMENTO
007801150929     c                   if        *inkq or *inke
007900150929     C   KQ              eval      valore = 'A'
008600020726     C** RIPRISTINO
008601150929     c   KE              clear                   valore
008700150929     C                   exsr      srannrip
008701150929     c                   leave
008702150929     c                   end
009400900608      *
009500891102     C                   EXSR      CTRFMT
009800150929     C   kf              EXSR      srSCRIVI
009801150928     c   kf              leave
009900911119     C**
010000020726     C                   ENDDO
010001150928     C                   ENDDO
010200891030     C                   SETON                                        LR
010300891030     C**------------------------
010400020726     C** CONTROLLI 1° VIDEO
010500891030     C**------------------------
010600020726     C     CTR01         BEGSR
010700020726     C*
010800150929     C                   SETOFF                                       9929
010801150928     C   kd              if        §fld = 'VTRE'
010900020726     C*§  RICERCA CODICE
011100020726     c                   clear                   TIBS02DS
011200020726     c                   movel     'R'           t02mod
011300150929     c*                  movel     knsif         t02sif
011400150929     c                   movel     vtre          t02ke1
011500150928     C                   movel     'TCE'         t02cod
011600020726     C                   CALL      'TIBS02R'
011700020726     c                   parm                    KPJBA
011800020726     c                   parm                    TIBS02DS
012000150928    2C                   IF        T02err = *blanks
012200150928     C                   movel     T02ke1        Vtre
012300020726     C                   ENDIF
012400020726     C                   ENDIF
012500020726     C* CAMPI OBBLIGATORI
012600150928     C     Vtre          IFEQ      *BLANKS
012800020726     C                   SETON                                        2999
012900150928     C                   Else
012901150929     c                   movel(p)  'TCE'         tbecod
012902150928     c                   movel(p)  vtre          tbeke1
012903150928     c     key1          chain     tntbe01l
012904150928     c                   if        %found(tntbe01l)
012905150928     C                   MOVEL     TBeUNI        dtce
012906150928     C                   MOVEL     §TCEDES       destre
012907150928     c                   else
012908150928     C                   SETON                                        2999
012909150928     c                   endif
012910150928     c                   endif
013600020726     C*
013700020726     C                   ENDSR
013800020726     C**------------------------
013900020726     C** PULISCI CAMPI VIDEO
014000020726     C**------------------------
014100020726     C     PULVID        BEGSR
014200900608      *
014300150928     C                   MOVEL     *BLANKS       vcau
014400150928     C                   MOVEL     *BLANKS       descau
014500150928     C                   MOVEL     *BLANKS       vuni
014600150928     C                   MOVEL     *BLANKS       desuni
014700150928     C                   MOVEL     *BLANKS       vkccd
014800150928     C                   MOVEL     *BLANKS       vkscd
014900150928     C                   MOVEL     *BLANKS       desdare
015000150928     C                   MOVEL     *BLANKS       vkcca
015100150928     C                   MOVEL     *BLANKS       vksca
015200150928     C                   MOVEL     *BLANKS       desavere
015300150928     C                   MOVEL     *BLANKS       vvoca
015400150928     C                   MOVEL     *BLANKS       desvoca
015500150928     C                   MOVEL     *BLANKS       vvocd
015600150928     C                   MOVEL     *BLANKS       desvocd
015700150928     C                   MOVEL     *BLANKS       vmail
015800150928     C                   MOVEL     *BLANKS       vcdca
015900150928     C                   MOVEL     *BLANKS       descdca
015901150929     C                   SETOFF                                       99
015902150929     C                   MOVEL     '0000000000'  *IN(51)
018500900608      *
018600891030     C                   ENDSR
018601150929     C**------------------------
018602150929     C** imposto i campi a video
018603150929     C**------------------------
018604150929     C     sriempi       BEGSR
018605150929     c* causale
018607150929     c                   movel(p)  'CAUSALE'     §ke2
018608150929     c                   exsr      leggi
018609150929     c                   eval      Vcau   =      valore
018610150929     c* unità
018612150929     c                   movel(p)  'UNITA'       §ke2
018613150929     c                   exsr      leggi
018614150929     c                   eval      Vuni   =      valore
018615150929     c* unità
018617150929     c                   movel(p)  'MAILTO'      §ke2
018618150929     c                   exsr      leggi
018619150929     c                   eval      Vmail  =      valore
018620150929     c* capoconto dare
018622150929     c                   movel(p)  'CAPODARE'    §ke2
018623150929     c                   exsr      leggi
018624150929     c                   eval      Vkccd      =  valore
018627150929     c* conto dare
018628150929     c                   movel(p)  'CONTODARE'   §ke2
018629150929     c                   exsr      leggi
018630150929     c                   if        valore <> ' '
018631150929     C                   eval      vkccd = %subst(valore:1:6)
018632150929     C                   eval      vkscd = %subst(valore:7:8)
018634150929     c                   end
018635150929     c* voce dare
018636150929     c                   movel(p)  'VOCEDARE'    §ke2
018637150929     c                   exsr      leggi
018638150929     c                   eval      Vvocd   =     valore
018639150929     c* conto avere
018646150929     c                   movel(p)  'CONTOAVERE'  §ke2
018647150929     c                   exsr      leggi
018649150929     C                   eval      vkcca = %subst(valore:1:6)
018650150929     C                   eval      vksca = %subst(valore:7:8)
018651150929     c* CdC avere
018655150929     c                   movel(p)  'CDCAVERE'    §ke2
018656150929     c                   exsr      leggi
018657150929     c                   eval      Vcdca   =     valore
018660150929     c* voce avere
018664150929     c                   movel(p)  'VOCEAVERE'   §ke2
018665150929     c                   exsr      leggi
018666150929     c                   eval      Vvoca   =     valore
018667150929     c                   ENDSR
018700891030     C**------------------------
018800891030     C** CONTROLLI 2 VIDEATA
018900891030     C**------------------------
019000891030     C     CTRFMT        BEGSR
019100900608      *
019200020726     C                   SETOFF                                       99
019300020726     C                   MOVEL     '0000000000'  *IN(51)
019305150928      * causale
019306150928     c                   clear                   descau
019307150929     C                   MOVEL     vcau          OPECAUSALE                                 ER.
019308150929     C     Kope          CHAIN     ANOPE01L                           57
019309150928     C  N57              MOVEL     OPEDESBREV    descau
019310150928     C   57              SETON                                        99
019311150928     c* unità
019312150928     c                   clear                   desuni
019313150928     C                   EVAL      UNIUNITA = VUNI
019314150928     c     kuni          chain     anuni01l                           59
019315150928     C  N59              MOVEL     UNIDESBREV    DESuni
019316150928     C   59              SETON                                        99
019317150928     c* DARE
019320150928     c                   clear                   desdare
019321150928     C                   move      vkccd         $KCC
019322150929     c                   if        vkscd <> ' '
019323150928     C                   move      vkscd         $KSC
019324150929     c                   else
019325150929     c                   clear                   $ksc
019326150929     c                   end
019800150929     C*
019900020726     C                   EXSR      CALLMVC002
020000020726     C*
020100020726     C                   If        $Esito =*off
020200150928     C                   eval      desdare        = Descr002
020300020726     c                   else
020400020726     c                   seton                                        5199
020600020726     C                   Endif
020603150928     c* analitica voce
020604150928     c                   clear                   desvocd
020605150928     c                   eval      vcivoce = vvocd
020606150928     C     kvci          chain     anvci01l                           53
020607150928     c   53              seton                                        99
020608150928     c  n53              movel     VCIDESCR      desvocd
020609150929     c*
020700150929     c* AVERE
020701150929     c                   clear                   desavere
020702150929     c                   if        vksca <> ' ' or vkcca <> ' '
020800150928     C                   move      vkcca         $KCC
020900150928     C                   move      vksca         $KSC
021000150928     C* conto
021100020726     C                   EXSR      CALLMVC002
021200020726     C*
021300020726     C                   If        $Esito =*off
021400150928     C                   eval      desavere       = Descr002
021500020726     c                   else
021600020726     c                   seton                                        5299
021800020726     C                   Endif
021801150929     C                   Endif
021802150928     c* analitica voce
021803150928     c                   clear                   desvoca
021804150929     c                   if        vvoca <> ' '
021805150928     c                   eval      vcivoce = vvoca
021806150928     C     kvci          chain     anvci01l                           54
021807150928     c   54              seton                                        99
021808150928     c  n54              movel     VCIDESCR      desvoca
021809150929     c                   end
021810150928     c* CDC
021811150928     c                   clear                   descdca
021812150929     c                   if        vcdca <> ' '
021813150928     C                   EVAL      UNIUNITA = VCDCA
021814150928     c     kuni          chain     anuni01l                           58
021815150928     C  N58              MOVEL     UNIDESBREV    DESCDCA
021816150928     C   58              SETON                                        99
021817150929     c                   end
021818150928     c* mail
021819150929     c                   if        vmail <> ' '
021820150928     c                   clear                   DSEMAIL
021821150928     c                   eval      §emlindi = vmail
021822151022     c                   eval      §emlindi=%trim(§emlindi)
021823150928     c                   call      'XEMAIL'
021824150928     c                   parm                    DSEMAIL
021825150928      * errore
021826150928     c                   If        §emlerro = '1'
021827150928     c                   seton                                        5599
021828150928     c                   endif
021829150928     c                   else
021830150928     c                   seton                                        5599
021831150928     c                   endif
034300020726     C*
034400020726     C                   ENDSR
034401150928     c**********************************************************************
034402150928     c* ricerca
034403150928     c**********************************************************************
034404150928     C     Search        BEGSR
034405150929     C* causale
034406150929     C                   IF        §FLD = 'VCAU'
034407150929     C                   RESET                   ANA070DS
034408150929     C                   MOVEL     '01'          OPZ070
034411150929     C                   MOVEL     ANA070Ds      KPJBU
034412150929     C                   CALL      'ANA070R'
034413150928     C                   PARM                    KPJBA
034414150929     C                   MOVEL     KPJBU         ANA070DS
034415150929     C                   IF        cau070 <>*BLANK
034416150929     C                   EVAL      Vcau= cau070
034418150928     C                   ENDIF
034419150929     C                   ENDIF
034420150929     C* UNITÀ
034421150929     C                   IF        §FLD = 'VUNI'
034422150929     C                   RESET                   ANA009DS
034423150929     C                   MOVEL     '01'          OPZ009
034424150929     C                   MOVEL     xscsoc        soc009
034425150929     C                   MOVEL     ANA009Ds      KPJBU
034426150929     C                   CALL      'ANA009R'
034427150929     C                   PARM                    KPJBA
034428150929     C                   MOVEL     KPJBU         ANA009DS
034429150929     C                   IF        UNI009 <>*BLANK
034430150929     C                   EVAL      VUNI= UNI009
034431150929     C                   ENDIF
034432150929     C                   ENDIF
034448150928      *Capoconto
034450150928     C                   if        §fld = 'VKCCD' or
034451150928     C                             §fld = 'VKCCA'
034452150928     C                   RESET                   ANA200DS
034468150928
034469150928     C                   EVAL      OPZ200 = '01'
034470150928     C                   EVAL      Kpjbu = ANA200DS
034471150928     C                   CALL      'ANA200R'
034472150928     C                   PARM                    Kpjba
034473150928     C                   EVAL      ANA200DS = Kpjbu
034474150928
034475150928     C                   IF        RET200 = *BLANKS
034476150928     C                   SELECT
034477150928     C                   when      §fld = 'VKCCD'
034478150928     C                   MOVE      KCC200        VKCCD
034479150928     C                   when      §fld = 'VKCCA'
034480150928     C                   MOVE      KCC200        VKCCA
034481150928     C                   ENDSL
034482150928     C                   ENDIF
034483150928     C                   ENDIF
034485150928      *Sottoconti
034488150928     C                   if        §FLD      = 'VKSCD' or
034489150928     C                             §FLD      = 'VKSCA'
034491150928     C                   RESET                   ANA250DS
034493150928     C                   SELECT
034494150929     C                   when      §FLD      = 'VKSCD'
034495150929     C                   IF        VKCCD = ' '
034496150928     C                   MOVE      *BLANKS       KCC250
034497150928     C                   ELSE
034498150929     C                   MOVE      VKCCD         KCC250
034499150928     C                   ENDIF
034500150928     C                   when      §FLD      = 'VKSCA'
034501150928     C                   IF        VKSCA = ' '
034502150928     C                   MOVE      *BLANKS       KCC250
034503150928     C                   ELSE
034504150929     C                   MOVE      VKCCA         KCC250
034505150928     C                   ENDIF
034506150928     C                   ENDSL
034507150928
034508150928     C                   EVAL      OPZ250 = '01'
034509150928     C                   EVAL      KPJBU = ANA250DS
034510150928     C                   CALL      'ANA250R'
034511150928     C                   PARM                    KPJBA
034512150928     C                   EVAL      ANA250DS = KPJBU
034513150928
034514150928     C                   IF        RET250 = *BLANKS
034515150928     C                   SELECT
034516150928     C                   when      §FLD      = 'VKSCD'
034517150928     C                   MOVE      KSC250        VKSCD
034518150928     C                   MOVE      KCC250        VKCCD
034519150928     C                   when      §FLD      = 'VKSCA'
034520150928     C                   MOVE      KSC250        VKSCA
034521150928     C                   MOVE      KCC250        VKCCA
034522150928     C                   ENDSL
034523150928     C                   ENDIF
034524150928     C                   ENDIF
034526150928      *Voci
034528150929     C                   if        §FLD      = 'VVOCD'
034529150928    >C                             OR
034530150928     C                             §FLD      = 'VVOCA'
034531150928     C                   RESET                   ANA280DS
034546150928
034547150928     C                   EVAL      OPZ280 = '01'
034548150928     C                   EVAL      KPJBU = ANA280DS
034549150928     C                   CALL      'ANA280R'
034550150928     C                   PARM                    KPJBA
034551150928     C                   EVAL      ANA280DS = KPJBU
034552150928
034553150928     C                   IF        RET280 = *BLANKS
034554150928     C                   SELECT
034555150928     C                   when      §FLD      = 'VVOCD'
034556150928     C                   MOVE      VOC280        VVOCD
034557150928     C                   when      §FLD      = 'VVOCA'
034558150928     C                   MOVE      VOC280        VVOCA
034559150928     C                   ENDSL
034560150928     C                   ENDIF
034561150928     C                   ENDif
034562150928     C* CDC
034563150928     C                   IF        §FLD = 'VCDCA'
034564150928     C                   RESET                   ANA032DS
034565150928     C                   MOVEL     '01'          OPZ032
034566150928     C                   MOVE      'S'           FSC032
034567150928     C                   MOVE      'CDC'         TPE032
034568150928     C                   MOVEL     ANA032Ds      KPJBU
034569150928     C                   CALL      'ANA032R'
034570150928     C                   PARM                    KPJBA
034571150928     C                   MOVEL     KPJBU         ANA032DS
034572150928     C                   IF        Ret032 = *BLANK
034573150928     C                   EVAL      VCdcA= Ent032
034574150928     C                   ENDIF
034575150928     C                   ENDIF
034576150928     C                   endsr
034577891030     C**----------------------------
034600891030     C** AGGIORNAMENTO / INSERIMENTO
034700891030     C**----------------------------
034800150929     C     srSCRIVI      BEGSR
034801150929     c* causale
034803150929     c                   movel(p)  Vcau          valore
034804150929     c                   movel(p)  'CAUSALE'     §ke2
034805150929     c                   exsr      scrivi
034806150929     c* unità
034808150929     c                   movel(p)  Vuni          valore
034809150929     c                   movel(p)  'UNITA'       §ke2
034810150929     c                   exsr      scrivi
034811150929     c* unità
034812150929     c                   movel(p)  Vmail         valore
034813150929     c                   movel(p)  'MAILTO'      §ke2
034814150929     c                   exsr      scrivi
034815150929     c* capoconto dare
034816150929     c                   movel(p)  Vkccd         valore
034817150929     c                   movel(p)  'CAPODARE'    §ke2
034818150929     c                   exsr      scrivi
034819150929     c* conto dare
034820150929     c                   if        vkscd <> ' '
034821150929     c                   movel(p)  Vkccd         valore
034822150929     c                   eval      valore = %trim(valore)+%triml(Vkscd)
034823150929     c                   else
034824150929     c                   clear                   valore
034825150929     c                   end
034826150929     c                   movel(p)  'CONTODARE'   §ke2
034827150929     c                   exsr      scrivi
034828150929     c* voce dare
034829150929     c                   movel(p)  Vvocd         valore
034830150929     c                   movel(p)  'VOCEDARE'    §ke2
034831150929     c                   exsr      scrivi
034832150929     c* conto avere
034833150929     c                   if        vkcca <> ' '
034834150929     c                   movel(p)  Vkcca         valore
034836150929     c                   eval      valore = %trim(valore)+%triml(Vksca)
034837150929     c                   else
034838150929     c                   clear                   valore
034839150929     c                   end
034840150929     c                   movel(p)  'CONTOAVERE'  §ke2
034841150929     c                   exsr      scrivi
034842150929     c* CdC avere
034843150929     c                   if        vcdca <> ' '
034844150929     c                   movel(p)  Vcdca         valore
034845150929     c                   else
034846150929     c                   clear                   valore
034847150929     c                   end
034848150929     c                   movel(p)  'CDCAVERE'    §ke2
034849150929     c                   exsr      scrivi
034850150929     c* voce avere
034851150929     c                   if        vvoca <> ' '
034852150929     c                   movel(p)  Vvoca         valore
034853150929     c                   else
034854150929     c                   clear                   valore
034855150929     c                   end
034856150929     c                   movel(p)  'VOCEAVERE'   §ke2
034857150929     c                   exsr      scrivi
034858150929     c*
034859150929     C                   ENDSR
034860150929     C**----------------------------
034861150929     C** leggi i record  RCE
034862150929     C**----------------------------
034863150929     C     leggi         BEGSR
034864150929     C                   MOVEL     'RCE'         TBECOD
034866150929     c                   movel     VTRE          tbeke1
034867150929     C                   MOVEL     §KE2          TBEKE2
034868150929     C     KTAB          CHAIN     TNTBE01L
034869150929     c                   if        %found(tntbe01l)
034870150929     c                   eval      valore = tbeuni
034871150929     c                   else
034872150929     c                   clear                   valore
034873150929     c                   end
034874150929     C                   ENDSR
034875150929     C**----------------------------
034876150929     C** AGGIORNAMENTO / INSERIMENTO
034877150929     C**----------------------------
034878150929     C     SCRIVI        BEGSR
034880150929     C                   MOVEL     'RCE'         TBECOD
034881150929     c                   movel     VTRE          tbeke1
034883150929     C                   MOVEL     §KE2          TBEKE2
034900020726     C     KTAB          CHAIN     TNTBE01L                           80
035300150929     C                   MOVEL     valore        TBEUNI
035301150929     c                   if        §TCEUTE <> 'S'
035600020726     C                   MOVE      'S'           TBEFTT                          A TUTTI
035601150929     c                   else
035602150929     C                   MOVE      ' '           TBEFTT                          A TUTTI
035603150929     c                   end
035700150929     C                   MOVEL     *blanks       TBEFLT                          FILIALE
035900020726     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
036000020726     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
036100900420     C*§
036200020726     C   80              WRITE     TNTBE000                             98
036300020726     C  N80              UPDATE    TNTBE000                             98
036400020726     C*
036500891030     C                   ENDSR
036501150929     C**----------------------------
036502150929     C** ANNULLAMENTO / RIPRISTINO
036503150929     C**----------------------------
036504150929     C     srannrip      BEGSR
036505150929     c                   movel(p)  'RCE'         tbecod
036506150929     c                   movel(p)  vtre          tbeke1
036507150929     C     key1          setll     Tntbe01L
036508150929     c                   do        *hival
036509150929     C     key1          reade     Tntbe01L
036510150929     c                   if        %eof(tntbe01l)
036511150929     c                   leave
036512150929     c                   end
036518150929     C                   MOVEL     valore        TBEatb
036520150929     c                   if        §TCEUTE <> 'S'
036521150929     C                   MOVE      'S'           TBEFTT                          A TUTTI
036522150929     c                   else
036523150929     C                   MOVE      ' '           TBEFTT                          A TUTTI
036524150929     c                   end
036525150929     C                   MOVEL     *blanks       TBEFLT                          FILIALE
036526150929     C                   MOVE      *BLANK        TBEFTR                          TIPO TRASM.
036527150929     C                   MOVE      *ZERO         TBEDTR                          N.TRASM.
036528150929     C*§
036529150929     C                   update    TNTBE000
036530150929     c                   enddo
036531150929     C*
036532150929     C                   ENDSR
036600020726     C******************************************************
036700020726     C     *INZSR        BEGSR
036800020726     C******************************************************
036900020726     C     *ENTRY        PLIST
037000020726     C                   PARM                    KPJBA
037100020726     C                   CALL      'X§PARUT'
037200020726     C                   PARM                    UT§DSE
037300020726     C                   MOVEL     RAGUT         RSUT
037400020726     C                   clear                   SocXsc
037500020726     C                   eval      tipxsc = 'SOC001'
037600020726     C                   exsr      rtvsocieta
037700020726     C                   if        rtnxsc = '1'
037800020726     C                   exsr      uscita
037900020726     C                   else
038000020726     C                   MOVEL     XSOCDS        xsoc001ds
038100020726     C                   endif
038200020726     C******************************************************
038300020726     C* POSIZIONAMENTO TABELLE                             *
038400020726     C     KTAB          KLIST                                                  *
038500150929     C                   KFLD                    tbecod                         *
038600150929     C                   KFLD                    tbeke1                         *
038700020726     C                   KFLD                    §KE2                           *
038800020726     C*                                                    *
038900020726     C     KEY1          KLIST                                                  *           STA
039000150928     C                   KFLD                    tbecod                         *
039100150928     C                   KFLD                    tbeke1                         *
039200020726     C*                                                    *
039300020726     C     Kuni          KLIST
039400020726     C                   KFLD                    xscsoc
039500150928     C                   KFLD                    UNIUNITA
039501150929     C     Kope          KLIST
039502150929     C                   KFLD                    xscsoc
039503150929     C                   KFLD                    OPECAUSALE
039504150929     C     Kvci          KLIST
039505150928     C                   KFLD                    xscsoc
039506150928     C                   KFLD                    vcivoce
039900020726     C     *LIKE         DEFINE    TBEKE2        §KE2
040000020726     C******************************************************
040100020726     C                   SETOFF                                       79
040200020726     C                   MOVEL     KNMUS         PROF              3
040300020726     C     PROF          IFNE      'EDP'
040400020726     C                   SETON                                        79
040500020726     C                   END
040700020726     C                   ENDSR
040800020726     C*---------------------------------------------------------------------
040900020726      /TITLE Reperisco società.
041000020726     C     rtvsocieta    begsr
041100020726     C*
041200020726     C                   CALLb     'XSOC'
041300020726   >>C                   Parm                    TipXsc            6
041400020726   >>C                   Parm                    SocXsc            3
041500020726   >>C                   Parm                    CdsXsc            9 0
041600020726   >>C                   Parm                    ModXsc            3
041700020726   >>C                   Parm      *BLANK        RtnXsc            1
041800020726     C                   Parm                    XsocDS
041900020726   >>C                   Parm                    Kpjba
042000020726     C*
042100020726     C                   endsr
042200020726      *-------------------------------------------------------------------
042300020726      /TITLE Uscita.
042400020726     C     uscita        begsr
042500020726     C*
042600020726     C                   seton                                        lr
042700020726     C*
042800020726     C                   return
042900020726     C*
043000020726     C                   endsr
043100020726     C*----------------------------------------------------------
043200020726     C* Chiamata NDMVC002
043300020726     C*----------------------------------------------------------
043400020726     C     CallMvc002    BEGSR
043500020726     C*
043600020726     C* Controllo conto
043700020726     C                   Clear                   ND002DS
043800020726     C                   EVAL      $LenOut = %Size(ND002DS)
043900020726     C                   CallB     'NDMVC002'
044000020726     C                   PARM                    xscsoc
044100020726     C                   PARM                    $kcc
044200020726     C                   PARM                    $ksc
044300020726     C                   PARM                    Wdate
044400020726     C                   PARM      *OFF          $GesMsg
044500020726     C                   PARM      *OFF          $Esito
044600020726     C                   PARM      'ND002DS'     $Struttura
044700020726     C                   PARM                    ND002DS
044800020726     C                   PARM                    $LenOut
044900020726     C*
045000020726     C                   ENDSR
