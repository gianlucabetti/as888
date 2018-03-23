000100990507     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020412     H*PARMS ACTGRP(YFICN51)
002100021203     Ffiapd01L  IF   E           K DISK
002101031119     Ftntbe01L  IF   E           K DISK
002102031119     Fansog01l  IF   E           K DISK
002103031119     Fanfrn01l  IF   E           K DISK
002104031119     Fanrco01l  IF   E           K DISK
002200020415     F*--------
002300031119     Fficn93D   CF   E             WORKSTN
002400980911     F                                     SFILE(y350S01:NRR)
002700951009     D*---------------------------------------------------------------*
002800020411     D* Reperimento nome PGM
002900020411     D STATUS         SDS           333
003000020411     D  nompgm           *PROC
003100020411     D*---------------------------------------------------------------*
003200020411     D* CAMPI INTERNI
003300020411     D*---------------------------------------------------------------*
003400020411     D dataiso         s               d   datfmt(*iso)
003500020411     D dataeur         s               d   datfmt(*eur)
003501031119     D ric             s              1  0 inz(1)
003600951009?     *--------------------------------------------------------------*
003700951009?     *  DS                                                          *
003800951009?     *--------------------------------------------------------------*
003900951009     D KPJBA         E DS
003901031119     D xsocds          DS          1000
003902031119     D soc001        E DS                  EXTNAME(xsoc001ds)
004000031120     d* parametri passati
004001031120     D param           DS
004002031120     d parrrn                         9s 0
004003031120     d opz                            2
004004031120     d cmd                            2
004005031120     d* parametri ricevuti
004100031119     D ficn92ds      E DS
004101031119     D dcaf          E DS
004102031119     d*
004400031119     D fiott           DS
004401031119     d pdrs                           7  0
004402031119     d ddcs                           8  0
004403031119     d caus                           4
004404031119     d imps                          10  3
004500031119     d tips                           1
004701031119     d socs                           3
004702031119     d fors                           8
004703031125     d dtfs                           8  0
004704031125     d nffs                           9  0
004705031125     d nras                           9  0
004706031119     d rrns                           9  0
005300951009?     *--------------------------------------------------------------*
005400951009?     *  CICLO PRINCIPALE                                            *
005500951009?     *--------------------------------------------------------------*
005501031119     c                   do        *hival
005502031119     c                   if        ric = 1
005700951010     C                   EXSR      CARSFL
005701031119     c                   end
006000031119     C                   exsr      GESS01                                       sfl pieno
006200031119     C                   ENDdo
006300980312     C*
006600951009?     *--------------------------------------------------------------*
006700951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
006800951009?     *--------------------------------------------------------------*
006900951012     C     CARSFL        BEGSR
007000951009      *
007100951009      *  Pulisco dati SFL
007200020405     C                   SETOFF                                       2120
007300980911     C                   WRITE     y350C01
007400951009     C                   SETON                                        2021
007600020415     C                   Z-ADD     0             NRR               6 0
007601031119     C/EXEC SQL
007602031119     C+ DECLARE A1 CURSOR FOR SELECT OTTPDR, OTTDDC, OTTTAB, OTTTIM,
007603031125     C+ OTTTIP, OTTSOC, OTTCDF, OTTDFT, OTTNFF, OTTNRA, rrn(fiott00f)
007604031125     C+ FROM fiott00f WHERE ottpdr between :f92pd1 and :f92pd2 and ottcdf
007605031125     C+ between :f92fo1 and :f92fo2 and ottddc between :f92dti and
007606031125     C+ :f92dtf ORDER BY OTTTAB, OTTTIP, OTTPDR, OTTDDC
007607031119     C/END-EXEC
008100031119     c* Lettura fiott00f
008101031119
009000020405     C/EXEC SQL
009100020405     C+ OPEN A1
009200020405     C/END-EXEC
009300020405
009400020405     C                   DOU       SqlCod <> 0
010100030827     C/EXEC SQL
010200031119     C+ FETCH NEXT FROM A1 INTO :fiott
010300030827     C/END-EXEC
010500020405     c*
010600020405     C                   SELECT
010700020405     C                   WHEN      SqlCod = 0
010701031119     C* controlli record letto
010800031119     c                   if        f92soc<> ' ' and socs <> f92soc
010801031119     c                   iter
010802031119     c                   end
010803031119     c                   if        f92tip<> ' ' and tips <> f92tip
010804031119     c                   iter
010805031119     c                   end
010806031119     c                   if        f92tab<> ' ' and caus <> f92tab
010807031119     c                   iter
010808031119     c                   end
010809040107     c* se caricati 9000 record di sfl metto un messaggio di parzializzare
010810040107     c                   if        nrr = 9000
010811040107     c                   seton                                        23
010812040107     c                   eval      $msg = 'Raggiunto massimo visualizzabile. -
010813040107     c                             F12 e parzializzare.'
010814040107     c                   leave
010815040107     c                   end
010816031119     c*
010900951010     C                   EXSR      CARTRN
011100020405     C                   WHEN      SqlCod = 100
011200020405     C                   WHEN      SqlCod <> 0
011300020417      * Forzo la stampa del JOBLOG.
011400020417     C                   CALL      'X66CHGJOB'
011500020405     C                   ENDSL
011600020404     C                   ENDDO
011700020417     C/EXEC SQL
011800020417     C+ CLOSE A1
011900020417     C/END-EXEC
011901031121     c                   clear                   ric
012200951009     C*
012300020415     C                   ENDSR
015000951010?     *--------------------------------------------------------------*
015100020405?     *  CARTRN: Gestione caricamento dati                           *
015200951010?     *--------------------------------------------------------------*
015300951010     C     CARTRN        BEGSR
015400951010      *
015401031121     c                   movel     *blanks       v1csce
016101031119     c                   MOVE      tips          V1Ctip
016102031119     c                   MOVE      pdrs          V1Cpdr
016200031119     C                   Z-ADD     rrns          h1rrn
016201031119     C                   Z-ADD     nras          h1nra
016202031119     C                   Z-ADD     imps          v1cimp
016300031119     c                   move      ddcs          dataiso
017100030827     c                   move      dataiso       dataeur
017200031119     c                   move      dataeur       v1cdta
017201031125     C                   IF        dtfs <> 0
017202031125     c                   move      dtfs          dataiso
017203031125     c                   move      dataiso       dataeur
017204031125     c                   move      dataeur       v1cdft
017205031125     c                   move      nffs          v1cnff
017206031125     c                   else
017207031125     c                   clear                   v1cnff
017208031125     c                   clear                   v1cdft
017209031125     c                   end
017300030827     C     kapd          CHAIN     fiapd01L                           30
017400030827     C                   MOVEL     apdrsf        v1dpdr
017401031119     C                   MOVEL(p)  caus          tbeke1
017402031119     C     ktbe          CHAIN     tntbe01L                           30
017403031119     C                   MOVEL     tbeuni        dcaf
017404031119     C                   MOVEL     §cafdes       v1dtab
017405031119     c                   MOVEl     socs          V1Csoc
017406031119     c                   MOVEl     fors          V1Cfor
017408031119     c                   movel     *blank        sogdes
017409031119     C     Kfrn          CHAIN     anfrn01l                           30
017410031119     C  N30Krco          CHAIN     anrco01l                           30
017411031119     C  N30rcosogg       CHAIN     ansog01l                           30
017412031119     C                   MOVEL(p)  sogdes        v1dfor
017500020408     c*
017600020405     C                   ADD       1             NRR
017700020405     C                   WRITE     y350S01
017800951010      *
017900951010     C                   ENDSR
019700020405?     *--------------------------------------------------------------*
019800020405?     *  GESS01: Gestione prima videata                              *
019900020405?     *--------------------------------------------------------------*
020000020405     C     GESS01        BEGSR
020100020405      *
020101031120     c                   if        nrr = 0
020102031120     c                   exfmt     y350D01
020103031120     c                   else
020200980914     C                   WRITE     y350T01
020300980914     C                   EXFMT     y350C01
020301031120     c                   end
020400020405     c* gestione comandi
020500020405     c                   exsr      srcmd
020501031119     c* gestione opzioni
020502031120     c                   if        nrr > 0
020503031119     c                   exsr      sropz
020504031120     c                   end
020600951009      *
020700020417     C                   ENDSR
020701031119?     *--------------------------------------------------------------*
020702031119?     *  srcmd   Gestione comandi                                    *
020703031119?     *--------------------------------------------------------------*
020704031119     C     srcmd         BEGSR
020706031119      *  Fine Lavoro
020707031119      *  Ritorno
020708031119     C     *INKC         IFEQ      '1'
020709031119     C     *INKL         orEQ      '1'
020710031119     C                   exsr      fine
020711031119     C                   END
020716031119      *  Inserimento
020717031119     C     *INKj         IFEQ      '1'
020718031120     c                   clear                   h1rrn
020719031119     c                   exsr      srcall
020721031119     C                   END
020722031119      *
020723031119     C                   ENDSR
020724031119?     *--------------------------------------------------------------*
020725031119?     *  sropz: Controlli prima videata                              *
020726031119?     *--------------------------------------------------------------*
020727031119     C     sropz         BEGSR
020729031119      *
020730031119     c                   clear                   $msg
020732031119      *  Solo i selezionati
020733031120     c                   do        *hival
020734031119     C                   READC     y350S01                                29
020735031120     C                   if        *in29
020736031120     c                   leave
020737031120     c                   end
020738031121     c                   setoff                                       23
020739031120     C                   if        v1csce = '4' or v1csce = '3'
020740031121     C                             or v1csce = '5' or v1csce = '2'
020741031121     c* posso fare l'opzione 2 o 4 solo se non ho ancora contabilizzato
020742031120     c* invece l'opzione 3 e 5 sono sempre possibile
020743031121     C                   if        h1nra<>0 and (v1csce='2' or v1csce='4')
020747031121     c                   seton                                        23
020748031121     c                   eval      $msg = 'Opzione non ammessa: riga già -
020749031121     c                             contabilizzata'
020752031120     c                   else
020754031120     c                   exsr      srcall
020755031119     c                   end
020756031121     c                   end
020758031121     c                   movel     *blanks       v1csce
020759031121     c                   if        *in23
020760031121     c                   seton                                        22
020761031121     c                   else
020762031121     C                   SETOFF                                       22
020763031121     c                   end
020764031119     C                   UPDATE    y350S01
020765031121     c   23              leave
020766031119     c                   enddo
020782031119      *
020783031119     C                   ENDSR
020800951011?     *--------------------------------------------------------------*
020900951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
021000951011?     *--------------------------------------------------------------*
021100951011     C     *INZSR        BEGSR
021200951011      *
021300951011     C     *ENTRY        PLIST
021400951011     C                   PARM                    KPJBA
021500031119     C                   MOVEL     KPJBU         ficn92ds
022001031119     C*---------- RICERCA DITTA :
022002031119     C                   MOVEL     'SOC001'      TIPXSC
022003031119     C                   MOVEL     '201'         SOCXSC
022004031119     C                   EXSR      REPSOC
022005031120     C     RTNXSC        IFNE      '1'
022006031120     C                   MOVEL     XSOCDS        SOC001
022007031120     C                   MOVEL     xscrgs        RSUT             20
022008031120     c                   else
022009031120     c                   exsr      fine
022010031120     c                   end
022011031119     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
022012031119     C                   movel     'F     '      $kcc              6
022013031119     C                   movel     *blanks       $ksc              8
022014031119     C                   callb     'XCAPCLIFOR'
022015031119     C                   parm                    xscsoc
022016031119     C                   parm                    $kcc
022017031119     C                   parm                    $ksc
022018031119     C                   movel     $kcc          forita            6
022019031119     C     Kfrn          KLIST
022020031119     C                   KFLD                    v1csoc
022021031119     C                   KFLD                    v1cfor
022022031119     C     Krco          KLIST
022023031119     C                   KFLD                    v1csoc
022024031119     C                   KFLD                    forita
022025031119     C                   KFLD                    v1cfor
022100021203     c     kapd          klist
022200031119     c                   kfld                    v1ctip
022300030827     c                   kfld                    v1cpdr
022301031119     c     ktbe          klist
022302031119     c                   kfld                    tbecod
022303031119     c                   kfld                    tbeke1
022304031119     c                   eval      tbecod = 'CAF'
022500951012     C                   ENDSR
022501031119     C*----------------------------------------------------*
022502031119     C* Reperimento dati società
022503031119     C*----------------------------------------------------*
022504031119     C     REPSOC        BEGSR
022505031119     C*
022506031119     C                   CALLB     'XSOC'
022507031119     C                   PARM                    TIPXSC            6
022508031119     C                   PARM                    SOCXSC            3
022509031119     C                   PARM                    CDSXSC            9 0
022510031119     C                   PARM                    MODXSC            3
022511031119     C                   PARM      *blanks       RTNXSC            1
022512031119     C                   PARM                    XSOCDS
022513031119     C                   PARM                    KPJBA
022514031119     C*
031400030827     C                   ENDSR
031403031119     C*----------------------------------------------------*
031404031119     C     fine          BEGSR
031405031119     C*----------------------------------------------------*
031406031120     c                   clear                   f92cmd
031500031119     C     *INKL         IFEQ      '1'
031700031119     C                   MOVEL     '12'          f92CMD
031800031119     C                   END
031801031120     C     *INKC         IFEQ      '1'
031802031119     C                   MOVEL     '03'          f92CMD
031803031119     C                   END
031804031119     C                   MOVEL(P)  ficn92ds      KPJBU
031805031119     c                   seton                                        lr
031806031119     c                   return
031900031119     C                   ENDSR
031901031119     C*----------------------------------------------------*
031902031119     C     srcall        BEGSR
031903031119     C*----------------------------------------------------*
031904031119     c                   clear                   ric
031905031120     c                   eval      parrrn = h1rrn
031906031120     c                   move      *all'0'       opz
031907031120     c                   if        not *inkj
031908031120     c                   move      v1csce        opz
031909031120     c                   else
031910031120     c                   eval      opz ='06'
031911031120     c                   end
031912031120     c                   clear                   cmd
031913031120     c                   movel(p)  param         kpjbu
032000031119     C                   call      'FICN94R'
032100031119     c                   parm                    kpjba
032101031120     c                   movel     kpjbu         param
032102031120     c*
032103031120     c                   select
032104031120     c* conferma allora ricarico x aggionamenti
032105031120     c                   when      cmd ='06'
032106031119     c                   eval      ric = 1
032107031120     c                   when      cmd ='03'
032108031120     c                   move      '1'           *inkc
032109031120     c                   exsr      fine
032110031120     c                   endsl
032200031119     C                   ENDSR
