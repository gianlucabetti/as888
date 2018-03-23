000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000101070301     h dftactgrp(*no) actgrp(*caller)
000102070322     H* FIDG22R *----------------------------------------------------*
000103070322     H*         Assegnaz.distinta di consegna auto.
000104070322     H*         Aggiorno rif. distinta su ARB partendo da ARG
000105070322     H*--------------------------------------------------------------*
000201070228     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000600070523     FFnarb01l  uF   E           K DISK    commit infds(arbinfds)
000603070319     FFIdst03L  IF   E           K DISK
000604070319     FFIAR601L  IF   E           K DISK
000605070316     FFNARBZ0F  IF A E           K DISK    prefix(Z)
000606100517     FTABEL00F  IF   E           K DISK
000607070323     Fazorg01L  IF   E           K DISK
000800950524      *--------------------------------------------------------------*
000900950524      *   DS
001000950524      *--------------------------------------------------------------*
001001070523     d arbinfds        ds
001002070523     d  arbnrr               397    400i 0
001003070228     ddtaiso           s               d
001004070228     ddtaeur           s               d   datfmt(*eur)
001100070316     D fnarbds£      e ds                  extname(fnarb00f) prefix(£)
001101070316     D fnarbds       e ds                  extname(fnarb00f)
001102070228     D fiargds       e ds                  extname(fiarg00f)
001103050520     D ddatiute      e ds
001104070227     D fnlr99ds      e ds
001105070523     d trul82ds      e ds
001200030910     D azuteds       e ds                  extname(AZUTE00F)
001300030910     D tibs34ds      E DS                  inz
001400950524     D KPJBA         E DS
001500070316     D Fidg23ds      E DS
001501100514     D ds7r          E DS
001502070323     D og146         E DS
001503071107     D og147         E DS
001504071107     D og150         E DS
001505070323     D DS3A          E DS
001904070319     D wnpg            S                   INZ like(dstnpg)
001905070319     D wndc            S                   INZ like(dstnfv)
001906070319     D wstp            S                   INZ like(arbstp)
001907070228     D savstp          S                   INZ like(arbstp)
001908070301     d distinta        ds
001909070301     d  dfv                                like(argddc)
001910070301     d  nfv                                like(argndc)
001911070301     d  fgs                                like(argfgs)
001912070301     d savdist         s                   like(distinta)
001913080115     d cgii            s                   like(argcgi)
001914080115     d cgif            s                   like(argcgi)
001915070323     D WLBDA8          DS
001916070323     D  G02DAT                 1      8  0
001917070323     D  G02INV                 9     16  0
001918070323     D  G02ERR                17     17
001919070323     D  G02TGI                18     22  0
001920070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
001921070228     D*--------------------------------------------------------------*
001922070228     D*  SCHIERE
001923070228     D*--------------------------------------------------------------*
001924070228     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
001925070228     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
002000950524     C*--------------------------------------------------------------*
002100950524     C*  Ciclo principale
002200950524     C*--------------------------------------------------------------*
002201080115     c* se passato il codice giro lo testo altrimenti prendo tutto
002202080115     c                   if        d23cgii <> ' '
002203080115     c                   eval      cgii = d23cgii
002204080115     c                   eval      cgif = d23cgii
002206080115     c                   else
002207080115     c                   eval      cgii = *loval
002208080115     c                   eval      cgif = *hival
002210080115     c                   end
002211080115
003800070410      * leggo le spedizioni messe in distinta su ARG e le inserisco su ARB
004021061003     C/EXEC SQL
004022070301     C+ DECLARE A1 CURSOR FOR SELECT fnarb01l.*, fiarg03l.* FROM fiarg03l
004023070228     C+ join fnarb01l on arbaas= argaas and arblnp=arglnp and arbnrs=
004024070316     C+ argnrs and arbnsp= argnsp WHERE argndc between :D23NFDI and
004025070316     C+ :D23NFaI and argddc=:d23dfvi and argfgs = :d23fgsi and arbndc= 0
004028080115     C+ and argcgi between :cgii and :cgif ORDER BY FIARG03L.ARGFGS,
004029080115     C+ FIARG03L.ARGNDC, FIARG03L.ARGDDC
004030061003     C/END-EXEC
004034061003
004035061003     C/EXEC SQL
004036070228     C+ OPEN a1
004037061003     C/END-EXEC
004038061003
004039061003     C                   do        *hival
004040061003
004041061003     C/EXEC SQL
004042070228     C+ FETCH NEXT FROM a1 INTO :fnarbds, :fiargds
004043061003     C/END-EXEC
004044061003
004045061003     C                   select
004046061003     C                   when      sqlcod = 100
004047061003     c                   leave
004048070206     C                   WHEN      SqlCod < 0
004049070206     c* record allocato
004050070206     c                   if        sqlcod <> -913
004051070206     C                   seton                                        H1
004052070206     c                   end
004053070227     c                   exsr      srstampa
004054070206     c                   exsr      srfine
004055070206     c                   other
004056070413     c* controllo se richiesto passaggio solo le manuali (d23nftl=99999) o
004057070413     c* solo automatiche (d23nftl = 0)
004058070413     c                   if        d23nftl = 99999
004059070413     c                   if        argnftl <> 99999
004060070413     c                   iter
004061070413     c                   end
004062070413     c                   else
004063070413     c                   if        argnftl = 99999
004064070413     c                   iter
004065070413     c                   end
004066070413     c                   end
004067070301     c* a rottura di distinta pulisco il file fnarbz0 in qtemp e azzero
004068070301     c* contatore stop
004069070301     c                   eval      dfv = argddc
004070070301     c                   eval      nfv = argndc
004071070301     c                   eval      fgs = argfgs
004072070301     c                   if        distinta <> savdist
004073070301     c                   exsr      srclear
004074070301     c                   eval      savdist = distinta
004075070301     c                   end
004076070206     C                   exsr      aggarb
004077061003     C                   endsl
004078061003     C*
004079061003     C                   enddo
004080061003
004081061003     C/EXEC SQL
004082070228     C+ CLOSE a1
004083061003     C/END-EXEC
006200950524     C*
006201070316     c                   if        d23cmti= 'S'
006301070228     c                   commit
006302070228     c                   end
006303070323     c                   eval      d23pag = pages
006304070323     c                   eval      d23riga= riga
006305070323     c                   eval      kpjbu = fidg23ds
006404070206     c*
006500950524     C                   SETON                                        LR
006948070316     C*--------------------------------------------------------------*
006949070316     C*  aggiorna spedizioni con rif. distinta
006950070316     C*--------------------------------------------------------------*
006951070316     C     aggarb        BEGSR
006952070228     c* reperisco stop
006953070228     c                   exsr      srstp
007000030721     c*
007105070206     c     kspe          chain(e)  fnarb01l
007106070206     c                   if        %error
007107070523     c* reperisco il RRN del file
007108070523     c     kspe          chain(n)  Fnarb01l
007109070523     c                   if        %found(Fnarb01l)
007110070523     c                   exsr      geslck
007111070523     c                   if        ul82§sts = 'A'
007112070704     c                   seton                                        85
007113070227     c                   exsr      srstampa
007114070206     c                   leavesr
007115070528     c                   else
007116070528     c     kspe          chain     fnarb01l
007117070206     c                   end
007118070523     c                   end
007119070523     c                   end
007120070206     c                   if        not %found(fnarb01l)
007121070227     c                   seton                                        84
007122070227     c                   exsr      srstampa
007123070206     c                   else
007124070222     c*controllo arb
007125070222     c                   if        arbndc <> 0
007126070704     c                   seton                                        85
007127070227     c                   exsr      srstampa
007128070222     c                   leavesr
007129070222     c                   end
007130070206     c                   eval      arbifp = argFGS
007131070206     c                   eval      arbddc = argddc
007132070206     c                   eval      arbndc = argndc
007133070206     c                   eval      arbpdc = argpdc
007134070206     C                   Z-ADD     0             ARBNMI
007135070206     C                   MOVEL     *BLANKS       ARBFDC
007136070206     C                   Z-ADD     0             ARBNSS
007137070419     c* 500 ngd = manuale
007138070419     c                   if        d23nftl = 99999
007139070419     c                   z-add     500           arbngd
007140070419     c                   else
007141070316     C                   Z-ADD     d23nftl       ARBNGD
007142070419     c                   end
007143070228     c                   eval      arbstp = savstp
010207070206     c                   update    fnarb000
010208070227     c                   exsr      srorm
010210070202     c                   end
010600950524     C*
010700070205     C                   ENDSR
010701070227     C*------------------------------------------------------------------------*
010702070227     C* stampa
010703070227     C*------------------------------------------------------------------------*
010704070227     C     srstampa      BEGSR
010705071107     c* stampo solo per assegnaz. automatica
010706071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
010707070316     c                   move      d23dfvi       dtaiso
010708070227     c                   move      dtaiso        dtaeur
010709070227     c                   move      dtaeur        prtdfv
010710070319     c                   move      argndc        prtnfv
010711070704     c                   if        *in85
010712070704     C                   eval      prtaas =      argaas
010713070704     C                   eval      prtlnp =      arglnp
010714070704     C                   eval      prtnrs =      argnrs
010715070704     C                   eval      prtnsp =      argnsp
010716070704     C                   eval      prtmsg = 'La spedizione non è stata aggiorna-
010717070704     C                             ta correttamente.VERIFICARLA'
010718070704     c                   end
010719071107     c* decodifico parametri di lancio
010720071210     c                   if        d23ldcri <> 0
010721071219     c                   move      d23LDCRI      dtaiso
010722071107     c                   move      dtaiso        dtaeur
010723071210     c                   move      dtaeur        decldcr
010724071210     c                   else
010725071210     c                   clear                   decldcr
010726071210     c                   end
010727071210     c                   if        *in71
010728071210     c                   move      d23dtai       dtaiso
010729071210     c                   move      dtaiso        dtaeur
010730071210     c                   move      dtaeur        decdta
010731071107     c                   eval      decora = d23orai
010732071107     c                   end
010733071107     c                   if        d23cleari = 'S'
010734071107     c                   eval      decclear='SI'
010735071107     c                   else
010736071107     c                   eval      decclear='NO'
010737071107     c                   end
010738150430     c                   if        d23p88i = 'N'
010743150515     c                   eval      decp88r='Spediz. pistola 88 NO'
010744150515     c                   else
010745150515     c                   clear                   decp88r
010746150430     c                   end
010747071107     c                   if        *in70
010748071107     c                   select
010749071107     c                   when      d23tpmi ='M'
010750071107     c                   eval      dectpm ='MESSAGGERIE'
010751071107     c                   when      d23tpmi ='P'
010752071107     c                   eval      dectpm ='PARCEL'
010753071107     c                   when      d23tpmi =' '
010754071107     c                   eval      dectpm ='TUTTE'
010755071107     c                   endsl
010756071107     c                   end
010757071107     c                   select
010758071107     c                   when      d23fppi ='M'
010759071107     c                   eval      decfpp ='MAT'
010760071107     c                   when      d23fppi ='P'
010761071107     c                   eval      decfpp ='POM'
010762071107     c                   when      d23fppi =' '
010763071107     c                   eval      decfpp ='TUTTE'
010764071107     c                   endsl
010765070323     C* PAGINA IN STAMPA
010766070323     C                   IF        *IN80 OR RIGA >=60
010767070323     C                   SETON                                        80
010768070323     C                   ADD       1             PAGES
010769071107     C                   Z-ADD     5             RIGA              3 0
010770070323     C                   END
010771070227     c*
010772070227     c   80              write     testa
010773071107     c   80              write     testap
010774070227     c                   setoff                                       80
010775070319     c   82              write     riga2
010776070323     C   82              ADD       1             RIGA
010777070227     c   84              write     riga4
010778070323     C   84              ADD       1             RIGA
010779070704     c   85              write     riga5
010780070704     C   85              ADD       1             RIGA
010781070704     c                   setoff                                       828485
010782071107     c                   end
010783070227     c*
010784070227     c                   endsr
013239070206     C**************************************************************************
013240070301     C* pulisce fnarbz0f e contatore stop
013241070206     C**************************************************************************
013242070301     C     srclear       BEGSR
013243070319     c* controllo che la distinta esista e non sia annullata
013244070319     c                   eval      wndc = argndc
013245070523     c     kdst          chain     fidst03l
013246070523     c                   if        not %found(fidst03l) or
013247070319     c                             (%found(fidst03l) and (dstatb <> ' ' or
013248070319     c                             dstfcf <> ' '))
013249070319     c                   seton                                        82
013250070319     c                   exsr      srstampa
013251070319     c                   exsr      srfine
013252070319     c                   end
013253070206     c*
013254070301     C/EXEC SQL
013255070301     C+ DELETE FROM QTEMP/FNARBZ0F
013256070301     C/END-EXEC
013257070301     c*
013258070301     c                   clear                   wstp
013259070316     c* salvo la ds di ARB perchè mi si sporca nella subroutine SRSTPARB
013260070316     c                   eval      fnarbds£ = fnarbds
013261070316     c* scrivo il file di lavoro FNARBZ x calcolo stop
013262070316     C* per le spedizione già inserite in ARB
013263070316     c                   exsr      srstparb
013264070316     c                   eval      fnarbds = fnarbds£
013265070316     c*
013266070301     c                   endsr
013267070301     C**************************************************************************
013268070301     C* calcolo gli stop
013269070301     C**************************************************************************
013270070301     C     srstp         BEGSR
013271070301     c*
013272070228     C                   Z-ADD     0             AR6KSC
013273070228     C                   Z-ADD     1             X                 2 0
013274070228     C                   MOVEL     *BLANKS       WTBOL1            1
013275070228     C                   CLEAR                   DS3A
013276070228     C                   CLEAR                   savstp
013277070228     C*  Controllo se cliente è assegnato
013278070228     C     ARBCBO        LOOKUP    C3A(X)                                 13
013279070228     C     *IN13         IFEQ      '1'
013280070228     C                   MOVEL     D3A(X)        DS3A
013281070228     C                   MOVEL     §3ATB1        WTBOL1            1
013282070228     C                   END
013283070228     C                   Z-ADD     ARBKSC        KKSC              7 0
013284070228     C     WTBOL1        IFEQ      'A'
013285070228     C     §3ATB2        ORNE      *BLANKS
013286070228     C     KARB          CHAIN     FIAR601L                           74
013287070228     C  N74              Z-ADD     AR6KSC        KKSC
013288070228     C                   END
013289070228     C*  Calcolo stop in base cod.cliente assegnato
013290070228     C                   Z-ADD     KKSC          APPOG             4 0
013291070228     C     APPOG         IFNE      8888
013292070228     C     APPOG         ANDNE     9999
013293070228     C     WTBOL1        ANDNE     'F'
013294070228     C     APPOG         ORNE      8888
013295070228     C     APPOG         ANDNE     9999
013296070228     C     AR6KSC        ANDNE     0
013297070301     C                   MOVEL(p)  KKSC          KRAG
013298070228     C     Krag          CHAIN     FNARBZ0F                           32
013299070228     C*  Se non trovato scrivo nuovo stop
013300070228     C     *IN32         IFEQ      '1'
013301070316     C                   MOVEL(p)  KKSC          ZARBRAG
013302070316     c                   if        arbstp = 0
013303070228     C                   add       1             wstp
013304070316     C                   eval      Zarbstp= wstp
013305070228     C                   eval      savstp= wstp
013306070316     c                   else
013307070316     c                   eval      Zarbstp = arbstp
013308070416     C                   eval      savstp= arbstp
013309070416     C                   eval      wstp = arbstp
013310070316     c                   end
013311070228     C                   WRITE     FNARBZ00
013312070228     c                   else
013313070316     c                   eval      savstp = Zarbstp
013314070228     C                   END
013315070228     C*  Calcolo stop in base rag.sociale + indirizzo destinatario
013316070228     C                   ELSE
013317070301     C                   MOVEL(p)  ARBRSD        KRAG
013318070228     C                   MOVE      ARBIND        KRAG
013319070228     C     Krag          CHAIN     FNARBZ0F                           32
013320070228     C*  Se non trovato scrivo nuovo stop
013321070228     C     *IN32         IFEQ      '1'
013322070316     C                   MOVEL(p)  KRAG          ZARBRAG
013323070316     c                   if        arbstp = 0
013324070228     C                   add       1             wstp
013325070316     C                   eval      Zarbstp= wstp
013326070228     C                   eval      savstp= wstp
013327070316     c                   else
013328070316     c                   eval      Zarbstp = arbstp
013329070416     C                   eval      savstp= arbstp
013330070416     C                   eval      wstp = arbstp
013331070316     c                   end
013332070228     C                   WRITE     FNARBZ00
013333070228     c                   else
013334070316     c                   eval      savstp = Zarbstp
013335070228     C                   END
013336070228     C                   END
013337070222     C*
013338070222     C                   ENDSR
013339070316     C*--------------------------------------------------------------*
013340070316     C*  aggiorna spedizioni con rif. distinta
013341070316     C*--------------------------------------------------------------*
013342070316     C     srstparb      BEGSR
013343070316     C/EXEC SQL
013344070531     C+ DECLARE B1 CURSOR FOR SELECT * FROM fnarb70l WHERE arbndc =
013345070531     C+ :argndc and arbddc= :argddc and arbifp = :argfgs ORDER BY ARbifp,
013346070531     C+ ARbndc, ARbDDC, arbstp
013348070316     C/END-EXEC
013349070316
013350070316     C/EXEC SQL
013351070316     C+ OPEN b1
013352070316     C/END-EXEC
013353070316
013354070316     C                   do        *hival
013355070316
013356070316     C/EXEC SQL
013357070316     C+ FETCH NEXT FROM b1 INTO :fnarbds
013358070316     C/END-EXEC
013359070316
013360070316     C                   select
013361070316     C                   when      sqlcod = 100
013362070316     c                   leave
013363070316     C                   WHEN      SqlCod < 0
013364070316     c* record allocato
013365070316     c                   if        sqlcod <> -913
013366070316     C                   seton                                        H1
013367070316     c                   end
013368070316     c                   exsr      srstampa
013369070316     c                   exsr      srfine
013370070316     c                   other
013371070316     c* scrivo file FNARBZ x stop
013372070316     c                   exsr      srstp
013373070316     C                   endsl
013374070316     C*
013375070316     C                   enddo
013376070316
013377070316     C/EXEC SQL
013378070316     C+ CLOSE b1
013379070316     C/END-EXEC
013380070316     C                   endsr
013381070222     C**************************************************************************
013382070222     C* esco con codice d'errore
013383070222     C**************************************************************************
013384070222     C     srfine        BEGSR
013385070222     c*
013386070316     c                   if        d23cmti= 'S'
013387070206     c                   rolbk
013388070206     c                   end
013389070206     c*
013390070316     c                   eval      d23erro= '1'
013391070316     c                   eval      kpjbu = fidg23ds
013392070206     c                   seton                                        lr
013393070206     c                   return
013394070206     c*
013395070206     c                   endsr
013396070227     C*-----------------------------------------------------*
013397070227     C*  mette distinta sull'orm x particolarità RC
013398070227     C*-----------------------------------------------------*
013399070227     C     srorm         BEGSR
013400100514     c                   if        arbgma <>' '
013401100514     c                   clear                   ds7r
013402100514     C                   eval      tblcod = '7R'
013403100514     C                   eval      tblkey = arbgma
013404100517     C     ktab1         chain     Tabel00f
013405100514     c                   if        %found(tabel00f)
013406100514     C                   MOVEL     TBLUNI        DS7R
013407100514     c                   end
013408100514     c                   if        §7RRC <>'S'
013409100514     c                   leavesr
013410100514     c                   end
013411070227     c                   clear                   fnlr99ds
013412070227     c                   eval      i99tla = 'L'
013413070227     c                   eval      i99aas = arbaas
013414070227     c                   eval      i99lnp = arblnp
013415070227     c                   eval      i99nrs = arbnrs
013416070227     c                   eval      i99nsp = arbnsp
013417071001     c                   move      arbifp        i99fgs
013418070227     c                   eval      i99ndc = arbndc
013419070227     c                   eval      i99ddc = arbddc
013420070227     c                   eval      i99fao = 400
013421070227     c                   eval      i99npg = 4
013422071001     c                   eval      i99comit ='1'
013423071001     c                   eval      i99cmt ='N'
013424071002     c* manuale
013425071002     c                   if        d23nftl = 99999
013426071002     c                   eval      i99nftl = d23nftl
013427071002     c                   end
013428070227     c                   call      'FNLR99R'
013429070227     c                   parm                    kpjba
013430070227     c                   parm                    fnlr99ds
013431070227     c                   end
013432070227     C                   ENDSR
013433950525     C*--------------------------------------------------------------*
013434950525     C*  Operazioni iniziali
013500950525     C*--------------------------------------------------------------*
013600950525     C     *INZSR        BEGSR
013700950525     C*
013800950525     C     *ENTRY        PLIST
013900950525     C                   PARM                    KPJBA
014000070316     C                   MOVEL(P)  KPJBU         Fidg23ds
014001070323     c                   eval      pages = d23pag
014002070323     c                   eval      riga  = d23riga
014003070323     c                   clear                   d23erro
014004070323     C* Inverto la data del giorno
014005070323     C                   TIME                    WHHDAT           14 0
014006070323     C                   MOVE      WHHDAT        G02DAT
014007070323     C                   MOVE      *BLANKS       G02ERR
014008070323     C                   CALL      'XSRDA8'
014009070323     C                   PARM                    WLBDA8
014010070323     C                   Z-ADD     G02INV        DATEU             8 0
014011070323     c     d23fgsi       chain     azorg01l
014012070323     c                   if        %found(azorg01l)
014014071107     c                   movel     orgdf0        og150
014015071107     C                   MOVEL     ORGde6        og146
014016071107     C                   MOVEL     ORGDE7        og147
014017071107     c     §OGPKM        comp      'S'                                    70
014018071107     c     §OGdoada      comp      'S'                                    71
014019070323     c* se p.o. non partito esco
014020070323     c                   if        §ogdda <>' '
014021070323     c                   movel     §ogdda        comdda            8 0
014022070323     c                   else
014023070323     c                   clear                   comdda
014024070323     c                   end
014025070323     c                   if        comdda <= dateu and comdda <> 0
014026070323     c                   else
014027070323     c                   seton                                        lr
014028070323     c                   return
014029070323     c                   end
014030070323     c                   end
014100030718     c*
014200030718     c     *dtaara       define    §azute        azuteds
014300030718     c     *dtaara       define    §datiute      ddatiute
014400030718     C                   in(E)     *dtaara
014500030718     C                   IF        %Error  or  RSUT = *blanks
014600030718     C                   call      'TIBS34R'
014700030718     C                   parm                    Tibs34Ds
014800030718     C                   in        *dtaara
014900030718     c                   ENDIF
015000950525     C*  Definisco chiavi di accesso
015001070319     C     Kdst          KLIST
015002070319     C                   KFLD                    argfgs
015003070319     C                   KFLD                    wnpg
015004070319     C                   KFLD                    wndc
015100070227     C     Kspe          KLIST
015200070227     C                   KFLD                    argaas
015300070227     C                   KFLD                    arglnp
015400070227     C                   KFLD                    argnrs
015401070227     C                   KFLD                    argnsp
015402070228     C     KARB          KLIST
015403070228     C                   KFLD                    arbAAS
015404070228     C                   KFLD                    arbLNP
015405070228     C                   KFLD                    arbNRS
015406070228     C                   KFLD                    arbNSP
015414070228     C     KTAB          KLIST
015415070228     C                   KFLD                    KKUT
015416070228     C                   KFLD                    KCOD
015417100517     C     KTAB1         KLIST
015418100517     C                   KFLD                    kkut
015419100517     C                   KFLD                    tblcod
015420100517     C                   KFLD                    tblkey
015421070228     C*
015425070316     C     *LIKE         DEFINE    ZARBRAG       KRAG
015427070228     C     *LIKE         DEFINE    TBLKUT        KKUT
015428070228     C     *LIKE         DEFINE    TBLCOD        KCOD
015430070319     c                   eval      wnpg = 4
015431070228     C*
015432070228     C*  Carico dati tabella 3A
015434070228     C                   Z-ADD     1             KKUT
015435070228     C                   MOVEL     '3A'          KCOD
015436070228     C                   Z-ADD     0             X
015437070228     C     KTAB          CHAIN     TABEL00F                           31
015438070228     C     *IN31         DOWEQ     '0'
015439070228     C     X             ANDLT     100
015440070228     C                   ADD       1             X
015441070228     C                   MOVEL     TBLKEY        C3A(X)
015442070228     C                   MOVEL     TBLUNI        D3A(X)
015443070228     C     KTAB          READE     TABEL00F                               31
015444070228     C                   END
017100950525     C                   ENDSR
017200070523     C*----------------------------------------------------*
017300070523     C     geslck        begsr
017400070523     C*----------------------------------------------------*
017500070523     c                   clear                   trul82ds
017600070523     c                   Eval      ul82§rrn = arbnrr
017700070523     c                   Eval      UL82§FIL = 'FNARB00F'
017800070523     c                   Eval      UL82§WIN = 'S'
018000070523     c                   Eval      UL82§num = 2
018100070523     c                   Eval      UL82§att = 2
018200070523     c                   eval      ul82§mss = ErrMsg(1)
018201070523     c                   Eval      UL82§msw = 'La Spedizione '
018202070523     c                             + %editc(arblnp:'Z') +  ' '
018203070523     c                             + %editc(arbnrs:'Z') +  ' '
018204070523     c                             + %editc(arbnsp:'Z') +  ' è già utilizza ta -
018205070523     c                             da un altro lavoro e non è manutenzionabile'
018600070523      * Effettuo la chiamata al *PGM d utilità
018700070523     c                   Call(e)   'TRUL82R'
018800070523     c                   Parm                    Trul82ds
018900070523
019000070523     C                   ENDSR
019100070523** ErrMsg
019200070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
