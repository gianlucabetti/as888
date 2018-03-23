000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200121218     H* FNLVV1R *----------------------------------------------------*
000300121218     H*  Gestione file recupero variazioni post-fatturazione         *
000301130124     h*
000302130124     h* N.b.: Per una corretta gestione il chiamante deve passare
000303130124     h*       il parametro contenente il record di TITAS prima della variazione
000304130124     h*       (partitas) tutte le
000305130124     h*       volte che titas viene aggiornato PRIMA del richiamo a
000306130124     h*       questo pgm.
000307130124     h*       Se invece l'aggiornamento di titas da parte del chiamante
000308130124     h*       avviene DOPO il richiamo è possibile sia passare il record (partit
000309130222     h*       as) sia la key spedizione
000400000000     H*--------------------------------------------------------------*
000401130204     FTITAS30C  iF   E           K DISK    usropn extfile(tas30)
000402130204     FTNCSB03L  iF   E           K DISK    usropn extfile(csb03)
003101130204     fTitaI01L  if   e           k disk    usropn extfile(tai01)
003102130204     ftntbe01L  if   e           k disk    usropn extfile(tbe01)
003200000000     FTABEL00F  IF   E           K DISK
003300900426     FAZORG01L  IF   E           K DISK
004001130204     Ffiar531c  if   e           k disk    usropn extfile(ar531)
004002120319     Ffnevb04l  if   e           k disk
004004130204     Ftivrb01l  uf a e           k disk    usropn extfile(vrb01)
004005121218
007000920113     D KPJBA         E DS
007001121219     d fnlvv1ds      e ds
007002121220      * ATTENZIONE
007003130118     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
007004121220     d* riverificare la definizione del campo parvar
007005121220     d dsarbk        e ds                  extname(fnarbk0r) qualified
007006121220     d dsarbd        e ds                  extname(fnarbd0r) qualified
007007121220     d dsarbt        e ds                  extname(fiarbt0r) qualified
007008121220     d dsarbg        e ds                  extname(fnarbg0r) qualified
007009130116     d dsarbv        e ds
007010130124     d darbt         e ds
007011121220      * Definizione del seguente campo in base alla dsarbd che è la più lunga
007012121220      * di tutte.
007013130206     dparvar           s            224
007014130122     dparfin           s                   like(tasfin)
007015140123     dparric           s                   like(dtas_tasric)
007016130114
007017130114     dpartitas         s                   like(titasds)
007018130118     dparcsb           s                   like(dscsb)
007600970714     D* DS PER TISI95R - CONTROLLO CAP
007700970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
007800021014     D ddatiute      e ds
007900021014     D azuteds       e ds                  extname(AZUTE00F)
008000020604     D tibs34ds      E DS
009503121116     D titasds       E DS                  EXTNAME(titas00f)
009504121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
009505121130     D dsvrb         E DS                  EXTNAME(tivrb00f)
009506121130     D dsVA          E DS                  EXTNAME(tivrb00f) qualified
009801130118     D og143         E DS
009802130118     D ds3a          E DS
009803121114     d drpfd         e ds
009804120424     d dar5ban       e ds
009805120424     d dar5bnb       e ds
009806120424     d dtasflo       e ds
009807120424     d dtas01        e ds
009808120313      ** DS Calcolo tassazione
009809120424     d dtas          e ds                  prefix(dtas_)
009810120424     d* §tasflo      e                     extfld(tasflo)
009811120424     d dtasv         e ds                  prefix(dtasv_)
009812120313     d  sv                     1     20    dim(20)
009813120313     d  va                    21    140P 3 dim(20)
009814121220     D DSTB          E DS
009815121220     D DS1A          E DS
009816150108     D DS7R          E DS
009900071102     d
011301120313     D ksvt            s                   like(taisvt)
011303121107     D w_vrbdimp       s                   like(vrbdimp)
011304120424     D w_vrbflg        s                   like(vrbflg)
011305120319     d kAr5Trd         s                   Like(Ar5Trd)
011306120612     d savdtas         s                   Like(dtas)
011307120612     d savdtasv        s                   Like(dtasv)
011308120612     d antepo          s                   Like(tasimv)
011309120319     D Wcev            s                   like(evbcev) inz('RIC')
011310121108     D Wcvb            s                   like(vrbcvb) inz('  ')
011311121130     D Wdiffe          s              1
011312140128     D Wnotf           s              1
011400011218
011500011218     d flgriorg        s              1
011600011218     d xx              s              2  0
011700070119     d commt           s             80
011800070119     d lenght          s             15  5 inz(80)
011801120424     D k               s              5  0
011802120424     D wsf20           s              1
011803130204     d kcod            S                   LIKE(tbecod)
011804130204     d kke1            S                   LIKE(tbeke1)
011805130204     d TAS30           s             21    inz('GAITRAGRPS/TITAS30C')
011806130204     d csb03           s             21    inz('GAITRAGRPS/TNCSB03L')
011807130204     d tai01           s             21    inz('GAITRAGRPS/TITAI01L')
011808130204     d ar531           s             21    inz('GAITRAGRPS/FIAR531C')
011809130204     d tbe01           s             21    inz('GAITRAGRPS/TNTBE01L')
011810130204     d vrb01           s             21    inz('GAITRAPRV/TIVRB01L')
011900941130     I*
013500920113     I/SPACE 3
013600920113     C****************************************************************
013700920113     C*  RIEPILOGO INDICATORI
013800920113     C***************************************************************
013900121218     C* 01    -
016400920113     C*****************************************************************
016500920113     C*
016501121219     C     *ENTRY        PLIST
016502121219     C                   PARM                    KPJBA
016503121220     c                   parm                    Parvar
016504121219     c                   parm                    partitas
016505121219     c                   parm                    parcsb
016506121219
016507121219     c                   movel     kpjbu         fnlvv1ds
016508130204
016510130114
016511130117    0c                   if        iv1tla<>'C'
016512130222
016513130222     c                   exsr      openfile
016514121220     c* Impostazione
016515121220     c                   exsr      impo
016528121218     c* 0 - Proseguo solo se la bolla di sede non è un C/servizio
016529130117     c*     e se è già stata contabilizzata
016530140128     c*     e se trovato record su titas se era da chainare
016531130117    1c                   if        §tbfcb='1' and tasfic<>*blanks
016532140128     c                             and wnotf=' '
016533130114      * 1 - bolla fatturata in filiale --> scarto
016534130117    2c                   if        tasfiv>0 and tasfiv <> 998 and tasfiv <> 996
016535130117   x2c                   else
016536130114      * 2 - bolla tassata fino all'imponibile  --> scarto
016537121218     c                   eval      ksvt = '£'
016538121218     c     Ktai06        setll     titai01l
016539121218    3c                   If        %equal(titai01l)
016540121218     c                   eval      w_vrbflg='I'
016541121218    3c                   endif
016546130207    3c     w_vrbflg      ifne      'I'
016548121218     c                   exsr      ges_vrb
016551130207    3c                   endif
016552130207    2c                   endif
016553130117    1c                   endif
016554130117    0c                   endif
016555130114      *  Chiusura PGM
016556130114     c                   if        iv1tla =' '
016557130114     c                   return
016558130114     c                   else
016559130114     c                   exsr      chiusura
035600920113     C                   SETON                                        LR
035602130114     c                   endif
035603130204     C**---------------------------------------------------------
035604130204     C** Apertura file
035605130204     C**---------------------------------------------------------
035606130204     C     openfile      BEGSR
035607130204     c* Se non aperto apro il file
035608130204     c* TITAS
035609130204     c                   if        not %open(titas30c)
035610130204     C                   OPEN(e)   titas30c
035611130204     c
035612130204     c                   if        not %open(titas30c)
035613130204     c                   eval      %subst(tas30:7:4)='GRU '
035614130204     C                   OPEN      titas30c
035615130204     c                   endif
035616130204     c                   endif
035617130204     c* TNCSB
035618130204     c                   if        not %open(tncsb03l)
035619130204     C                   OPEN(e)   tncsb03l
035620130204     c
035621130204     c                   if        not %open(tncsb03l)
035622130204     c                   eval      %subst(csb03:7:4)='GRU '
035623130204     C                   OPEN      tncsb03l
035624130204     c                   endif
035625130204     c                   endif
035626130204     c* TITAI
035627130204     c                   if        not %open(titai01l)
035628130204     C                   OPEN(e)   titai01l
035629130204     c
035630130204     c                   if        not %open(titai01l)
035631130204     c                   eval      %subst(tai01:7:4)='GRU '
035632130204     C                   OPEN      titai01l
035633130204     c                   endif
035634130204     c                   endif
035635130204     c* fiar5
035636130204     c                   if        not %open(fiar531c)
035637130204     C                   OPEN(e)   fiar531c
035638130204     c
035639130204     c                   if        not %open(fiar531c)
035640130204     c                   eval      %subst(ar531:7:4)='GRU '
035641130204     C                   OPEN      fiar531c
035642130204     c                   endif
035643130204     c                   endif
035644130204     c* tntbe
035645130204     c                   if        not %open(tntbe01l)
035646130204     C                   OPEN(e)   tntbe01l
035647130204     c
035648130204     c                   if        not %open(tntbe01l)
035649130207     c                   eval      %subst(tbe01:7:4)='GRU '
035650130204     C                   OPEN      tntbe01l
035651130204     c                   endif
035652130204     c                   endif
035653130204     c* tivrb
035654130204     c                   if        not %open(tivrb01l)
035655130204     C                   OPEN(e)   tivrb01l
035656130204     c
035657130204     c                   if        not %open(tivrb01l)
035658130204     c                   eval      %subst(vrb01:7:3)='201'
035659130204     C                   OPEN      tivrb01l
035660130204     c                   endif
035661130204     c                   endif
035662130204     c                   endsr
035663121220     C**---------------------------------------------------------
035664121220     C**
035665121220     C**---------------------------------------------------------
035666121220     C     impo          BEGSR
035667121220     c*
035668121220     c* Valorizzazione DS delle variazioni effettuate:
035669121220     c* in base alla casuale variazione ricevuta valorizzo la ds relativa
035670121220     c                   select
035671130123     c                   when      %subst(iv1cvb:1:1)='I'
035672121220     c                   eval      dsarbd=parvar
035673121220     c                   when      %subst(iv1cvb:1:1)='K'
035674121220     c                   eval      dsarbk=parvar
035675130124     c                   when      %subst(iv1cvb:1:1)='T'
035676130124     c                   if        iv1pgm='TRTR39R'
035677121220     c                   eval      dsarbt=parvar
035678130124     c                   else
035679130124     c                   eval      darbt=parvar
035680130124     c                   z-add     §btias        dsarbt.arbias
035681130124     c                   movel     §btvas        dsarbt.arbvas
035682130124     c                   endif
035683121220     c                   when      %subst(iv1cvb:1:1)='C'
035684121220     c                   eval      dsarbg=parvar
035685130118     c                   when      iv1cvb='VV' or iv1cvb='PV'
035686130118     c                   eval      dsarbv=parvar
035687130123     c                   when      iv1cvb='ZT'
035688130122     c                   eval      parfin=%subst(parvar:1:1)
035689140123     c                   when      iv1cvb='RI'
035690140123     c                   eval      parric=%subst(parvar:1:1)
035691121220     c                   endsl
035692121220     c* se ricevuta la key spedizione la utilizzo per chain titas e csb
035693121220     c* altrimenti utilizzo 3° e 4° parametro
035694140128     c                   clear                   wnotf
035695121220     c                   if        iv1aas<>0
035696140128     c* se non ricevuto il tipo bolla eseguo ricerca
035697140128     c                   if        iv1tbl=*blanks
035698140128     c     kbols         setgt     titas30c
035699140128     c     kbols         readpe    titas30c
035700140128     c                   else
035701121220     c     kbol          chain     titas30c
035702140128     c                   endif
035703140128     c                   if        %eof (titas30c)
035704140128     c                   eval      wnotf='1'
035705140128     c                   endif
035706121220     c                   else
035707130114     c                   eval      titasds=partitas
035708130114     c                   if        %subst(iv1cvb:1:1)='K'
035709130114     c                   eval      dscsb=parcsb
035710130114     c                   endif
035711121220     c                   endif
035712121220     c*
035713121220     c                   clear                   w_vrbdimp
035714121220     c                   clear                   w_vrbflg
035715130117     c*
035716121220     C                   MOVEL     'TB'          COD
035717121220     C                   MOVEL(P)  tastbl        KEY
035718121220     c                   clear                   dstb
035719121220     C     KTAB          CHAIN     TABEL
035720121220     c                   if        %found(tabel00f)
035721121220     C                   MOVEL     TBLUNI        DSTB
035722121220     c                   endif
035723130118     C                   MOVEL     '3A'          COD
035724130118     C                   MOVEL(P)  tascbo        KEY
035725130118     c                   clear                   ds3a
035726130118     C     KTAB          CHAIN     TABEL
035727130118     c                   if        %found(tabel00f)
035728130118     C                   MOVEL     TBLUNI        DS3a
035729130118     c                   endif
035730121220     c                   endsr
035731130114     C**---------------------------------------------------------
035732130114     C**
035733130114     C**---------------------------------------------------------
035734130114     C     chiusura      BEGSR
035735130114     C*
035736130114     C                   CLEAR                   DSSI95
035737130114     C                   MOVEL     'C'           I95TLA
035738130114     C                   CALL      'TISI95R'
035739130114     C                   PARM                    DSSI95
035740130114
035741130114     C     WSF20         IFNE      *BLANKS
035742130114     C                   MOVEL     'C'           dtas_TASTLA
035743130206     C                   CALL      'TNSF20R'                            99
035744130114     C                   PARM                    KPJBA
035745130114     C                   PARM                    DTAS
035746130114     C                   PARM                    DTASV
035747130114     C                   ENDIF
035748130114
035749130114     c                   endsr
102901120322     C**---------------------------------------------------------
102902120322     C** Richiamo tisi95 per determinazione codice tassazione
102903120322     C**---------------------------------------------------------
102904120322     C     C_tisi95      BEGSR
102906120322     C                   CLEAR                   DSSI95
102907130118     C                   MOVEL     dsarbd.ARBCAD I95CAP
102908130118     C                   MOVEL     dsarbd.ARBNZD I95NAR
102909130118     C                   MOVEL     dsarbd.ARBLOD I95LOC
102910130118     C                   MOVEL     dsarbd.ARBPRD I95PRV
102911121116     c                   movel     tasaas        i95dat
102912121116     c                   move      tasmgs        i95dat
102915120322     C                   MOVEL     '7'           I95TCN
102916120322      * Se network FedEx passo flag apposito per dire di prendere i dati
102917120322      * dalla tabella della nazione
102918120322     c     taslna        chain     azorg01l
102919120322     c                   if        %found(azorg01l) and orgfva = *blanks
102920120322     c                   eval      og143 = orgde3
102921120322     c     §ogntw        ifeq      'FED'
102922120322     c                   eval      i95fi1 = 'S'
102923120322     c                   endif
102924120322     c                   endif
102925120322
102926120322     C                   MOVEL     §3ATB1        I95TPO
102927120322    5C     §3AFCA        IFNE      0
102928120322     C                   MOVEL     'S'           I95FCA
102929120322    5C                   ENDIF
102930120322     C**
102931120322     C                   CALL      'TISI95R'
102932120322     C                   PARM                    DSSI95
102933120322     c                   endsr
347700941130     C**---------------------------------------------------------
347800941130     C* ROUTINE INIZIALE
347900941130     C**---------------------------------------------------------
348000941130     C     *INZSR        BEGSR
348100021014
348400021014
348500941130     C                   Z-ADD     1             CODUT             1 0
348600021014
348700021014     c     *dtaara       define    §azute        azuteds
348800021014     c     *dtaara       define    §datiute      ddatiute
348900021014     C                   in(E)     *dtaara
349000021014     C                   IF        %Error  or  RSUT = *blanks
349100021014     C                   CLEAR                   TIBS34DS
349200021014     C                   call      'TIBS34R'
349300021014     C                   parm                    Tibs34Ds
349400021014     C                   in        *dtaara
349500021014     c                   ENDIF
349600021014
349700021014     C                   Z-ADD     DUTKCI        KCI               4 0
349800021014
349901121218     C     KTAB          KLIST
349902121218     C                   KFLD                    CODUT
349903121218     C                   KFLD                    COD               2
349904121218     C                   KFLD                    KEY               8
349905130118     c     kbol          klist
349906130118     c                   kfld                    iv1aas
349907130118     c                   kfld                    iv1lnp
349908130118     c                   kfld                    iv1nrs
349909130118     c                   kfld                    iv1nsp
349910130118     c                   kfld                    iv1tbl
349911140128     c     kbols         klist
349912140128     c                   kfld                    iv1aas
349913140128     c                   kfld                    iv1lnp
349914140128     c                   kfld                    iv1nrs
349915140128     c                   kfld                    iv1nsp
349917121218     c     ktai06        klist
349918121218     c                   kfld                    tasaas
349919121218     c                   kfld                    taslnp
349920121218     c                   kfld                    tasnrs
349921121218     c                   kfld                    tasnsp
349922121218     c                   kfld                    tastbl
349923121218     c                   kfld                    Ksvt
349924121218      * Chiave per setll su TIVRB01l
349925121218     c     Kvrb          Klist
349926121218     c                   kfld                    tasaas
349927121218     c                   kfld                    taslnp
349928121218     c                   kfld                    tasnrs
349929121218     c                   kfld                    tasnsp
349930121218     c                   kfld                    tastbl
349931121218     c                   kfld                    wcvb
351900941130     C     KANN          KLIST
352000130118     C                   KFLD                    tasAAS
352100130118     C                   KFLD                    tasLNP
352200130118     C                   KFLD                    tasNRS
352300130118     C                   KFLD                    tasNSP
358701120424     c*
358702120424     c     ktai05        klist
358703120424     c                   kfld                    tasaas
358704120424     c                   kfld                    taslnp
358705120424     c                   kfld                    tasnrs
358706120424     c                   kfld                    tasnsp
358707120424     c                   kfld                    tastbl
358708120313
358716120319     c     kFiar5        Klist
358717120319     c                   Kfld                    TasAas
358718120319     c                   Kfld                    TasLnp
358719120319     c                   Kfld                    TasNrs
358720120319     c                   Kfld                    TasNsp
358721120319     c                   Kfld                    kAr5Trd
358722120319      * Chiave file eventi FNEVB
358723120319     c     Kevb4         Klist
358724120319     c                   Kfld                    Tasaas
358725120319     c                   Kfld                    Taslnp
358726120319     c                   Kfld                    Tasnrs
358727120319     c                   Kfld                    Tasnsp
358728120319     c                   Kfld                    Wcev
358729130204
358730130204     c     ktbe          klist
358731130204     c                   kfld                    kcod
358732130204     c                   kfld                    kke1
367200071102
367300941130     C                   ENDSR
367400930804     C*
375974121129     C**-------------------------------------------------------------
375975121129     C** Gestione della bolla per verificare se da scrivere
375976121129     c*  nel file per la creazione della bolla di recupero (TIVRB)
375978121129     C**-------------------------------------------------------------
375979121129     C     ges_vrb       BEGSR
375980121129     c* Verifico se bolla già presente su file delle variazioni
375981121130     c                   eval      wcvb='VA'
375982121203     c     kvrb          chain(N)  tivrb01l
375983121130     c* BOLLA NON PRESENTE: tasso e memorizzo solo se delta tassazione > 0
375984121203    1c                   if        not %found(tivrb01l)
375985121130     c                   exsr      sr_tassa1
376094121129    2c                   if        dtas_taserr=*blanks
376095121129     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
376096121129    3c                   if        w_vrbdimp>0
376097121129     c* recupero la data penultima fatturazione mensile per vedere se
376098121129     c* la variazione è da scrivere nel file di lavoro
376099121129     c                   exsr      rec_RPFdata
376100121129     c                   if        tasdft>§RPFDTA and §rpfdta>0
376101121129     c                   exsr      wrtvrb
376102121129     c                   endif
376103121129    3c                   endif
376104121129    2c                   endif
376107121130   x1c                   else
376108121130     c* BOLLA PRESENTE: tasso e verifico se aggiornare il file delle variazioni
376109121130     c* recuperare
376110121130     c                   exsr      sr_tassa2
376111121130    2c                   if        dtas_taserr=*blanks
376112121130     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
376113121130    3c                   select
376114121130     c* DELTA TASSAZIONE >0 : Aggiorno record "VA"
376115121203     c                   when      W_vrbdimp>0  and w_vrbdimp>vrbdimp
376116121130     c* recupero la data penultima fatturazione mensile per vedere se
376117121130     c* la variazione deve aggiornare il file di lavoro
376118121130     c                   exsr      rec_RPFdata
376119121130     c                   if        tasdft>§RPFDTA and §rpfdta>0
376122121130     c                   exsr      aggvrb
376123121130     c                   endif
376124121130     c                   other
376125121130     c* DELTA TASSAZIONE <=0: Verifico se differenze da aggiornare
376126121130     c* mi salvo il record "VA" prima di chainare il record "  "
376127121130     c                   eval      dsva=dsvrb
376128121130     c                   clear                   wcvb
376129121130     c                   clear                   wdiffe
376130121203     c     kvrb          chain(N)  tivrb01l
376131121130     c                   exsr      ver_diffe
376132121130    4c                   if        wdiffe='1'
376133121130     c* rieseguo chain del record "VA" per aggiornarlo
376134121130     c                   eval      wcvb='VA'
376135121203     c     kvrb          chain     tivrb01l
376136121203   4ac                   if        %found(tivrb01l)
376137121130     c* recupero la data penultima fatturazione mensile per vedere se
376138121130     c* la variazione deve aggiornare il file di lavoro
376139121130     c                   exsr      rec_RPFdata
376140121130    5c                   if        tasdft>§RPFDTA and §rpfdta>0
376141121130     c                   eval      dsvrb=dsva
376142121130     c                   time                    vrbtmst
376143121130     c                   eval      vrbdimp=w_vrbdimp
376144121130     c                   update    tivrb000
376145121130    5c                   endif
376146121130   4ac                   endif
376147121130    4c                   endif
376148121130    3c                   endsl
376149121130    2c                   endif
376150121129    1c                   endif
376151121130     C                   MOVEL     '1'           WSF20
376152121129     c                   endsr
376153121130     C**-------------------------------------------------------------
376154121130     C** Tassazione - I volta
376155121130     C**-------------------------------------------------------------
376156121130     C     sr_tassa1     BEGSR
376157121130     c* campi della DTAS da prendere da titas
376158121130     c                   exsr      riedtasUNI
376159121130     c                   exsr      riedtas
376160121130     c* campi della dtas da impostare con campi della variazione in ricezione
376161121130     c                   exsr      riedtasvar
376162121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
376163121130     c                   exsr      riedtasALTRI
376164121130      * C/Assegno
376165121130    3c                   IF        Tasfca <> *blanks
376166130116     c                             and %subst(iv1cvb:1:1)<>'K'
376167121130     c                   Exsr      Carcsb
376168121130    3c                   Endif
376220121130      * recupero i valori  già tassati
376221121130     c                   exsr      Ric_tai
376222121130
376223130206     C                   CALL      'TNSF20R'
376224121130     C                   PARM                    KPJBA
376225121130     C                   PARM                    DTAS
376226121130     C                   PARM                    DTASV
376227121130     c                   endsr
376228121130     C**-------------------------------------------------------------
376229121130     C** Tassazione - Bolla già presente nelle variazioni da recuperare
376230121130     C**-------------------------------------------------------------
376231121130     C     sr_tassa2     BEGSR
376232121130     c* campi della dtas da prendere da titas
376233121130     c                   exsr      riedtasUNI
376234121130     c* campi della dtas da prendere dal record "VA" di TIVRB
376235121130     c                   exsr      riedtasVRB
376236121130     c* campi della dtas da impostare con campi della variazione in ricezione
376237121130     c                   exsr      riedtasVAR
376238121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
376240121130     c                   exsr      riedtasALTRI
376333121130      * recupero i valori  già tassati
376334121130     c                   exsr      Ric_tai
376335121130
376336130206     C                   CALL      'TNSF20R'
376337121130     C                   PARM                    KPJBA
376338121130     C                   PARM                    DTAS
376339121130     C                   PARM                    DTASV
376340121130     c                   endsr
376341121130      *****************************************************************
376342121130      * Impostazione campi della dtas da campi di titas
376343121130      *****************************************************************
376344121130     c     RiedtasUNI    Begsr
376345121130     c                   clear                   dtas
376346121130     c                   clear                   dtasv
376347121130     c                   eval      dtas_TASKSC=tasksc
376352121130     c                   eval      dtas_TASNCL=tasncl
376353121130     c                   eval      dtas_TASCTR=tasctr
376358121130     c                   eval      dtas_TASFDN=tasfdn
376359121130     c                   eval      dtas_TASMCT=tasmct
376360121130     c                   eval      dtas_TASNZM=tasnzm
376361121130     c                   eval      dtas_TASCAM=tascam
376362121130     c                   eval      dtas_TASFAP=tasfap
376363121130     c                   eval      dtas_TASTSP=tastsp
376367121130     c                   eval      dtas_taslna=taslna
376372121130     c                   eval      dtas_TASDIV=tasdiv
376373121130     c                   eval      dtas_TASPVL=taspvl
376379121130     c                   endsr
376380121130      *****************************************************************
376381121130      * Impostazione campi della dtas da campi di tivrb
376382121130      *****************************************************************
376383121130     c     RiedtasVRB    Begsr
376384121130     c                   eval      dtas_taspkf=VRBPKF
376385121130     c                   eval      dtas_taspkc=vRBPKC
376386121130     c                   eval      dtas_tasncp=VRBNCP
376387121130     c                   eval      dtas_tasvlf=VRBVLF
376388121130     c                   eval      dtas_tascts=VRBCTS
376389121130     c                   eval      dtas_tasnzd=VRBNZD
376390121130     c                   eval      dtas_tascad=VRBCAD
376391121130     c                   eval      dtas_tasfin=VRBFIN
376392121130     c                   eval      dtas_tastc1=VRBTC1
376393121130     c                   eval      dtas_tastc2=VRBTC2
376394121130     c                   eval      dtas_taslnp=vrblnp
376395121130     c                   eval      dtas_TASTBL=vrbtbl
376396121130     c                   eval      dtas_tasvas=VRBVAS
376397121130     c                   eval      dtas_tasias=VRBIAS
376398121130     c                   eval      dtas_tasqft=VRBQFT
376399121130     c                   eval      dtas_tastic=VRBTIC
376400121130     c                   eval      dtas_tascas=VRBCAS
376401121130     c                   eval      dtas_tasvca=VRBVCA
376402121130     c                   eval      dtas_tascmb=VRBCMB
376403121130     c                   move      VRBSTA        dtas_tassta
376404121130     c                   endsr
376405121130      *****************************************************************
376406121130      * Campi della DTAS da impostare con la variazione in ricezione
376407121130      *****************************************************************
376408121130     c     RiedtasVAR    Begsr
376409121130    2c                   select
376410121130     c* Variazioni Destinatario/peso/volume
376411130116     c                   when      %subst(iv1cvb:1:1)='I'
376412130118     c                   z-add     dsarbd.arbpkf dtas_taspkf
376413130118     c                   z-add     dsarbd.arbvlf dtas_tasvlf
376414130118     c                   movel     dsarbd.arbfin dtas_tasfin
376415130118     c                   movel     dsarbd.arbcad dtas_tascad
376416130118     c                   movel     dsarbd.arbnzd dtas_tasnzd
376417130118    3C     iv1cvb        ifeq      'I0'
376418121130     c                   exsr      c_tisi95
376419121130     c                   if        o95err = *blanks
376420121130     c                   eval      dtas_tascts=o95cts
376421121130     c                   endif
376422121130    3c                   endif
376423121130     c* Variazioni codice bolla/contrassegno
376424130116     c                   when      %subst(iv1cvb:1:1)='K'
376425121205     c* Contrassegno ANNULLATO: prendo i campi da tncsb
376426121205     c*                         e metto stato 9
376427130116    3c                   if        iv1can='S'
376428121205     c                   eval      dtas_tascas=dscsb.csbcas
376429121205     c                   eval      dtas_tasvca=dscsb.csbvca
376430121205     c                   eval      dtas_tascmb=dscsb.csbcmb
376431121205    4c                   if        dscsb.csbfus<>' '
376432121210     c****               eval      dtas_tastic=dscsb.csbfus
376433121210     c                   move      dscsb.csbfus  dtas_tastic
376434121205   x4c                   else
376435121210     c****               eval      dtas_tastic=dscsb.csbtpi
376436121210     c                   move      dscsb.csbtpi  dtas_tastic
376437121205    4c                   endif
376438121205     c                   eval      dtas_Tassta='9'
376439121205   x3c                   else
376440121205     c* Contrassegno NON Annullato: prendo i campi dalla variazione
376441130118     c                   eval      dtas_tascas=dsarbk.arbcas
376442130118     c                   eval      dtas_tasvca=dsarbk.arbvca
376443130118     c                   eval      dtas_tascmb=dsarbk.arbcmb
376444121205     c                   clear                   dtas_tastic
376445121205     C                   MOVEL     '1A'          COD
376446130118     C                   MOVEL(P)  dsarbk.arbtic KEY
376447121205     C     KTAB          CHAIN     TABEL
376448121205    4c                   if        %found(tabel00F)
376449121205     C                   MOVEL     TBLUNI        DS1A
376450121205     c                   move      §1afmb        dtas_tastic
376451121205    4c                   endif
376452121205     c* .......... flag stato: metto 0 o 3 in base a quello che c'è su TNCSB
376453121205    4c                   if        dscsb.csbsta<>3
376454121205     c                   eval      dtas_Tassta='0'
376455121205     c                   else
376456121205     c                   eval      dtas_tassta='3'
376457121205    4c                   endif
376458121205    3c                   endif
376471121130     c* Variazioni Consegne particolari
376472130116     c                   when      %subst(iv1cvb:1:1)='C'
376473130118     c                   movel     dsarbg.arbtc1 dtas_tastc1
376474130118     c                   movel     dsarbg.arbtc2 dtas_tastc2
376475121130     c* Variazione Importo da assicurare
376476130116     c                   when      %subst(iv1cvb:1:1)='T'
376478130118     c                   movel     dsarbt.arbias dtas_tasias
376479130118     c                   movel     dsarbt.arbvas dtas_tasvas
376484130116     c* Variazione Volume VDL
376485130116     c                   when      iv1cvb='VV'
376486130118     c                   z-add     §bvvlf        dtas_tasvlf
376487130122     c* Variazione Peso   VDL
376488130122     c                   when      iv1cvb='PV'
376489130122     c                   z-add     §bvpkf        dtas_taspkf
376490130122     c                   z-add     §bvpkc        dtas_taspkc
376491130122     c                   z-add     §bvncp        dtas_tasncp
376492130122     c* Variazione inoltro Z/T
376493130123     c                   when      iv1cvb='ZT'
376494130122     c                   eval      dtas_tasfin=parfin
376495121130    2c                   endsl
376496121130     c                   endsr
376497120424      *****************************************************************
376498121130      * Impostazione campi della dtas da campi di titas
376499120424      *****************************************************************
376500120424     c     Riedtas       Begsr
376501120424     c                   eval      dtas_TASPKF=taspkf
376502120424     c                   eval      dtas_TASPKC=taspkc
376503120424     c                   eval      dtas_TASNCP=tasncp
376504120424     c                   eval      dtas_TASVLF=tasvlf
376505120424     c                   eval      dtas_TASCTS=tascts
376506120424     c                   eval      dtas_TASNZD=tasnzd
376507120424     c                   eval      dtas_TASCAD=tascad
376508120424     c                   eval      dtas_TASFIN=tasfin
376509120424     c                   eval      dtas_TASTC1=tasftc
376510120424     c                   eval      dtas_TASTC2=tastc2
376511120424     c                   eval      dtas_taslnp=taslnp
376512121130     c                   eval      dtas_TASTBL =tastbl
376513120424     c                   eval      dtas_TASVAS =tasvas
376514120424     c                   eval      dtas_TASIAS =tasias
376515120424     c                   eval      dtas_TASQFT =tasqft
376516120424     c                   endsr
376517121130      *****************************************************************
376518121130      * Altri campi della dtas da prendere da titas
376519121130      *****************************************************************
376520121130     c     RiedtasAltri  Begsr
376521121130     c                   clear                   dAr5Ban
376522121130     c                   Clear                   dAr5Bnb
376523121130     c                   Clear                   kpjbu
376524121130     c                   movel     tasaas        dtas_tasdsp
376525121130     c                   move      tasmgs        dtas_tasdsp
376526121130     c                   eval      dtas_tasdct=dtas_tasdsp
376527121130
376528121130      * Flag SI NO DDT
376529121130    2C                   If        tasll1 = 'C' or tasll1 = 'S'
376530121130     C                   movel     'S'           dtas_Tasddt
376531121130   x2C                   Else
376532121130     C                   Clear                   dtas_Tasddt
376533121130    2C                   Endif
376534121130     c                   clear                   dtas01
376535121130     c                   movel     tasfbr        §asfbr
376536121130     c                   movel     tascca        §ascca
376537121130      * valorizzo flag email al destinatario
376538121130     c                   movel     tasflo        dtasflo
376539121130     c                   move      §floemd       §asemd
376540150108     c* flag pin code
376541150108     c                   if        tasgma<>*blanks
376542150108     c                   clear                   ds7r
376543150108     c                   movel     '7R'          cod
376544150108     c                   movel(p)  tasgma        key
376545150108     c     ktab          chain     tabel
376546150108    8c                   if        %found(tabel00f)
376547150108     c                   movel     tbluni        ds7r
376548150108    8c                   endif
376549150108     c                   if        §7rpincode='S'
376550150108     c                   movel     'S'           §aspin
376551150108     c                   endif
376552150108     c                   endif
376553121130
376554121130     c                   eval      dtas_tasflo = dtas01
376555121130
376556121130     c
376557121130      * Bancali
376558121130    2c                   If        %Subst(TasGva:1:1) = 'B'
376559121130
376560121130     c                   eval      kAr5Trd  = 'BAN'
376561121130     c     kFiar5        Chain     Fiar531c
376562121130If  3c                   If        %Found(Fiar531c)
376563121130     c                   Movel     Ar5Uni        dAr5Ban
376564121130    3c                   EndIf
376565121130      * Bancali
376566121130     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
376567121130    2c                   EndIf
376568121130      **
376569121130      * Colli Originali
376570121130    2c                   If        %Subst(TasGva:1:1) = 'O'
376571121130     c                   eval      kAr5Trd  = 'BNB'
376572121130     c     kFiar5        Chain     Fiar531c
376573121130If  3c                   If        %Found(Fiar531c)
376574121130     c                   Movel     Ar5Uni        dAr5Bnb
376575121130    3c                   EndIf
376576121130      * Colli
376577121130     c                   Eval      dtas_TasNcl = §Ar5bcor
376578121130    2c                   EndIf
376579121130
376580121130      **** verifico se devo calcolare l'addebito di lasciato avviso
376581121130      **** verifico se c'è evento
376582140127      **** Se Variazione "RI" non ho bisogno di verificare la presenza dell'
376583140127      **** evento
376584140127     c                   if        iv1cvb='RI'
376585140127     c                   eval      dtas_tasric = 'S'
376586140127     c                   else
376587140127     c                   clear                   evbdtr
376588140127     c     Kevb4         setll     Fnevb04l
376589140127     c     Kevb4         reade     Fnevb04l
376590140127     c                   dow       not %eof(fnevb04l) and evbdtr=0
376591140127     c     Kevb4         reade     Fnevb04l
376592140127     c                   enddo
376593140127     c                   if        evbdtr>0
376594140127     c                   eval      dtas_tasric = 'S'
376595140127     c                   endif
376596140127     c                   endif
376597121130
376598121130      * Assicurazione
376599121130    2c                   IF        Tasfcm = 'F'
376600121130     c                   clear                   dtas_tasias
376601121130     c                   clear                   dtas_tasvas
376602121130    2c                   Endif
376603121130     c                   endsr
376605121130
376606120316      *****************************************************************
376607120316      *  Recupero importi di tassazione dal file titai
376608120316      *****************************************************************
376609120316     c     Ric_tai       Begsr
376610120316      *
376611120316     c                   z-add     0             k
376612120316
376613120316     c     ktai05        setll     titai01l
376614120316
376615120316     c                   If        %equal(titai01l)
376616120316
376617120316     c                   do        *hival
376618120316
376619120316     c     ktai05        reade     titai01l
376620120316
376621120316     c                   If        %eof(titai01l)
376622120316     c                   leave
376623120316     c                   endif
376624120316      * imponibile
376625120316     c                   if        taisvt = '£'
376626120424     c                   eval      dtas_tasimv = taivat
376627120316     c                   endif
376628120316      * porto
376629120316     c                   if        taisvt = ' '
376630120424     c                   eval      dtas_taspor = taivat
376631120316     c                   endif
376632120316      * varie
376633120316     c                   if        taisvt <> ' ' and taisvt <> '£'
376634120612     c* se presente varia N 88.888.888 devo prima tassarla
376635120612    4c                   if        taisvt='N' and taivat=88888888
376636120612     c                   eval      savdtas=dtas
376637120612     c                   eval      savdtasv=dtasv
376638120612     c                   exsr      tassan
376639120612     c                   eval      dtas=savdtas
376640120612     c                   eval      dtasv=savdtasv
376641120612     c                   add       1             k
376642120612     c                   eval      sv(k) ='N'
376643120613     c                   eval      va(k) =antepo
376644120612   x4c                   else
376645120316     c                   add       1             k
376646120316     c                   eval      sv(k) = taisvt
376647120316     c                   eval      va(k) = taivat
376648120612    4c                   endif
376649120316     c                   endif
376650120316
376651120316     c                   enddo
376652120316
376653120316     c                   else
376654120316      * se non è pretassata pulisco la divisa
376655120424     c                   clear                   dtas_tasdiv
376656120316     c                   endif
376657120424     c                   endsr
376658120612      ****************************************************
376659120612      ** Tassazione varia N 88888888 (Anteporto)
376660120612      ****************************************************
376661120612     c     tassan        Begsr
376662120613     C                   CLEAR                   DTAS_tasimv
376663120613     C                   CLEAR                   DTAS_taspor
376664120613     C                   CLEAR                   DTASV
376665120612     c                   clear                   antepo
376666120612      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
376667150108      * idem per flag Pin Code
376668120612     c                   eval      dtas01 = dtas_tasflo
376669120612     c                   clear                   §asemd
376670150108     c                   clear                   §aspin
376671120612     c                   eval      dtas_tasflo = dtas01
376672120612      *
376673130206     C                   CALL      'TNSF20R'
376674120612     C                   PARM                    KPJBA
376675120612     C                   PARM                    DTAS
376676120612     C                   PARM                    DTASV
376677120612      *
376678120612      * Tassazione andata buon fine
376679120613     C     dtas_taserr   IFEQ      *BLANKS
376680120613     c                   eval      antepo=dtas_tasimv
376681120612     C                   ENDIF
376682120612     c                   endsr
376683120316      ****************************************************
376684120316      ** Ricerca Contrassegno
376685120316      ****************************************************
376686120316     c     Carcsb        Begsr
376687120316
376688130118     c     kann          chain     Tncsb03l
376689120316     c                   If        %found(tncsb03l) and
376690120316      * solo per stesso tipo bolla
376691120316     c                             tastbl = csbtbl
376692120424     c                   movel     csbsta        dtas_Tassta
376693120424     c                   z-add     csbcas        dtas_Tascas
376694120316     c                   if        csbfus <> *blanks
376695120424     c                   move      csbfus        dtas_tastic
376696120316     c                   else
376697120424     c                   move      csbtpi        dtas_tastic
376698120316     c                   end
376699120316      * mittente
376700120424     c                   movel     csbvca        dtas_tasvca
376701120424     c                   z-add     csbcmb        dtas_tascmb
376702120316
376703120316     c                   endif
376704120316
376705120316     c                   endsr
376706120319      ****************************************************
376707120319      ** Scrittura work-file delle bolle già fatturate
376708120319      ****************************************************
376709120319     c     wrtvrb        Begsr
376710121130     c                   clear                   tivrb000
376711121130     c                   eval      vrbaas=tasaas
376712121130     c                   eval      vrblnp=taslnp
376713121130     c                   eval      vrbnrs=tasnrs
376714121130     c                   eval      vrbnsp=tasnsp
376715121130     c                   eval      vrbtbl=tastbl
376716121130     c                   time                    vrbtmst
376717121130     c                   eval      vrbdft=tasdft
376718130115     c                   eval      VRBPKC=taspkc
376719130115     c                   eval      VRBNCP=tasncp
376720130115     c                   eval      VRBPKF=taspkf
376721130115     c                   eval      VRBFPF=tasfpf
376722130115     c                   eval      VRBVLC=tasvlc
376723130115     c                   eval      VRBNCR=tasncr
376724130115     c                   eval      VRBVLF=tasvlf
376725130115     c                   eval      VRBFVF=tasfvf
376726130115     c                   eval      VRBQFT=tasqft
376727130115     c                   eval      VRBIAS=tasias
376728130115     c                   eval      VRBVAS=tasvas
376729130115     c                   eval      VRBCTS=tascts
376730130115     c                   eval      VRBFIN=tasfin
376731130115     c                   eval      VRBCAD=tascad
376732130115     c                   eval      VRBNZD=tasnzd
376733130115     c                   eval      VRBFFD=tasffd
376734130115     c                   eval      VRBTC1=tasftc
376735130115     c                   eval      VRBTC2=tastc2
376736130116    2c                   if        %subst(iv1cvb:1:1)<>'K'
376737121109     c                   eval      VRBTIC=dtas_tastic
376738121109     c                   eval      VRBCAS=dtas_tascas
376739121109     c                   eval      VRBVCA=dtas_tasvca
376740121109     c                   eval      VRBCMB=dtas_tascmb
376741121115     c                   move      dtas_tassta   vrbsta
376742121109   x2c                   else
376743121115    3c                   if        dscsb.csbtbl=tastbl
376744121115    4c                   if        dscsb.csbfus <> *blanks
376745121115     c                   move      dscsb.csbfus  vrbtic
376746121109   x4c                   else
376747121115     c                   move      dscsb.csbtpi  vrbtic
376748121109    4c                   end
376749121115     c                   eval      VRBCAS=dscsb.csbcas
376750121115     c                   eval      VRBVCA=dscsb.csbvca
376751121115     c                   eval      VRBCMB=dscsb.csbcmb
376752121115     c                   eval      VRBsta=dscsb.csbsta
376753121109     c                   endif
376754121109    2c                   endif
376755140123    2c                   if        iv1cvb<>'RI'
376756140123     c                   eval      vrbric=dtas_tasric
376757140123    2c                   endif
376758121108     c                   write     tivrb000
376759121129     c* SCRITTURA RECORD "VA" (Prima volta)
376760121129     c                   eval      vrbcvb='VA'
376761130118     c                   exsr      RieVrbUni
376791121129     c                   write     tivrb000
376792121129     c*
376814120319     c                   endsr
376829121130      ****************************************************
376830121130      ** Aggiornamento record "VA" del file delle variazioni
376831121130      ** da recuperare per delta tassazione >0
376832121130      ****************************************************
376833121130     c     aggvrb        Begsr
376834121130     c                   eval      wcvb='VA'
376835121203     c     kvrb          chain     tivrb01l
376836121203     c                   if        %found(tivrb01l)
376837130118     c                   exsr      RieVrbUni
376873121130     c                   update    tivrb000
376874121130     c                   endif
376875121130     c                   endsr
376876130118      ****************************************************
376877130118      ** Riempimento campi vrb per l'aggiornamento/write
376878130118      ** Record "VA"
376879130118      ****************************************************
376880130118     c     RieVrbUni     Begsr
376881130118     c                   time                    vrbtmst
376882130118     c                   eval      vrbdimp=w_vrbdimp
376883130118    3c                   select
376884130118     c* Variazioni Destinatario/peso/volume
376885130118     c                   when      %subst(iv1cvb:1:1)='I'
376886130118     c                   z-add     dtas_taspkf   vrbpkf
376887130118     c                   z-add     dtas_tasvlf   vrbvlf
376888130118     c                   movel     dtas_tasfin   vrbfin
376889130118     c                   movel     dtas_tascad   vrbcad
376890130118     c                   movel     dtas_tasnzd   vrbnzd
376891130118     c                   movel     dtas_tascts   vrbcts
376892130118     c* Variazioni codice bolla/contrassegno
376893130118     c                   when      %subst(iv1cvb:1:1)='K'
376894130118     c                   eval      vrbcas=dtas_tascas
376895130118     c                   eval      vrbvca=dtas_tasvca
376896130118     c                   eval      vrbtic=dtas_tastic
376897130118     c                   eval      vrbcmb=dtas_tascmb
376898130118     c                   move      dtas_tassta   vrbsta
376899130118     c* Variazioni Consegne particolari
376900130118     c                   when      %subst(iv1cvb:1:1)='C'
376901130118     c                   movel     dtas_tastc1   vrbtc1
376902130118     c                   movel     dtas_tastc2   vrbtc2
376903130118     c* Variazione Importo da assicurare
376904130118     c                   when      %subst(iv1cvb:1:1)='T'
376905130118     c                   movel     dtas_tasias   vrbias
376906130118     c                   movel     dtas_tasvas   vrbvas
376907130118     c* Variazione Volume VDL
376908130118     c                   when      iv1cvb='VV'
376909130118     c                   z-add     dtas_tasvlf   vrbvlf
376910130118     c                   move      §bvfvf        vrbfvf
376911130118     c                   z-add     §bvncr        vrbncr
376912130118     c                   z-add     §bvvlc        vrbvlc
376913130122     c* Variazione Peso   VDL
376914130122     c                   when      iv1cvb='PV'
376915130122     c                   z-add     dtas_taspkf   vrbpkf
376916130122     c                   move      §bvfpf        vrbfpf
376917130122     c                   z-add     §bvncp        vrbncp
376918130122     c                   z-add     §bvpkc        vrbpkc
376919130122     c* Variazione Inoltro Z/T
376920130123     c                   when      iv1cvb='ZT'
376921140123     c                   movel     dtas_tasfin   vrbfin
376922140123     c* Evento "RIC"
376923140123     c                   when      iv1cvb='RI'
376924140123     c                   movel     dtas_tasric   vrbric
376928130118    3c                   endsl
376929130118     c                   endsr
376930121130      ****************************************************
376931121130      ** Verifica differenze fra record "VA" e immagine precedente
376932121130      ****************************************************
376933121130     c     ver_diffe     Begsr
376934121130    1c                   select
376935121130     c* Variazioni Destinatario/peso/volume
376936130116     c                   when      %subst(iv1cvb:1:1)='I'
376937121130     c* . . . . Peso e volume da fatturare
376938121130    2c                   if        vrbpkf<>dsva.vrbpkf or
376939121130     c                             vrbvlf<>dsva.vrbvlf
376940130118     C                   if        dsarbd.arbpkf<vrbpkf  or
376941130118     C                             dsarbd.arbvlf<vrbvlf
376942121130     c                   eval      dsva.vrbpkf=vrbpkf
376943121130     c                   eval      dsva.vrbvlf=vrbvlf
376944121130     c                   else
376945130118     c                   eval      dsva.vrbpkf=dsarbd.arbpkf
376946130118     c                   eval      dsva.vrbvlf=dsarbd.arbvlf
376947121130     c                   endif
376948121130     c                   eval      wdiffe='1'
376949121130    2c                   endif
376950121130     c* . . . . Flag inoltro CAP e Nazione e Codice Tassazione
376951121130    2c                   if        vrbfin<>dsva.vrbfin or
376952121130     c                             vrbcad<>dsva.vrbcad or
376953121130     c                             vrbnzd<>dsva.vrbnzd or
376954121130     c                             vrbcts<>dsva.vrbcts
376955130118     c                   eval      dsva.vrbfin=dsarbd.arbfin
376956130118     c                   eval      dsva.vrbcad=dsarbd.arbcad
376957130118     c                   eval      dsva.vrbnzd=dsarbd.arbnzd
376958121130     c                   eval      dsva.vrbcts=dtas_tascts
376959121130     c                   eval      wdiffe='1'
376960121130    2c                   endif
376961121130     c* Variazioni codice bolla/contrassegno
376962130116     c                   when      %subst(iv1cvb:1:1)='K'
376963121203    2c                   if        vrbtic<>dsva.vrbtic or
376964121203     c                             vrbcas<>dsva.vrbcas or
376965121203     c                             vrbvca<>dsva.vrbvca or
376966121203     c                             vrbcmb<>dsva.vrbcmb or
376967121203     c                             vrbsta<>dsva.vrbsta
376968121203     c* stessa divisa o non c'era il contrassegno
376969121205    3c                   if        dtas_tasvca=vrbvca or vrbvca=*blanks
376970121130     c* se importo più basso del valore iniziale --> tengo l'iniziale
376971121205    4c                   if        (dtas_tassta='9' and vrbsta<>9) or
376972121205     c                             (dtas_tascas<vrbcas and
376973121205     c                             ((dtas_tassta='9' and vrbsta=9) or
376974121205     c                              (dtas_tassta<>'9' and vrbsta<>9)))
376975121130     c                   z-add     vrbcas        dsva.vrbcas
376976121130     c                   eval      dsva.vrbtic=vrbtic
376977121130     c                   eval      dsva.vrbvca=vrbvca
376978121130     c                   eval      dsva.vrbcmb=vrbcmb
376979121130     c                   eval      dsva.vrbsta=vrbsta
376980121130   x4c                   else
376981121130     c* se importo maggiore o uguale tengo la variazione
376982121205     c                   z-add     dtas_tascas   dsva.vrbcas
376983121205     c                   eval      dsva.vrbtic=dtas_tastic
376984121205     c                   eval      dsva.vrbvca=dtas_tasvca
376985121205     c                   eval      dsva.vrbcmb=dtas_tascmb
376986121205     c                   move      dtas_tassta   dsva.vrbsta
376987121130    4c                   endif
376988121130   x3c                   else
376989121130     c* divisa diversa imposto sempre come l'originale
376990121130     c                   z-add     vrbcas        dsva.vrbcas
376991121130     c                   eval      dsva.vrbtic=vrbtic
376992121130     c                   eval      dsva.vrbvca=vrbvca
376993121130     c                   eval      dsva.vrbcmb=vrbcmb
376994121130     c                   eval      dsva.vrbsta=vrbsta
376995121130    3c                   endif
376996121130     c                   eval      wdiffe='1'
376997121130    2c                   endif
376998121130     c* Variazioni Consegne particolari
376999130116     c                   when      %subst(iv1cvb:1:1)='C'
377000121130    2c                   if        (vrbtc1=dsva.vrbtc1 and vrbtc2=dsva.vrbtc2)
377001121130     c                              or
377002121130     c                             (vrbtc1=dsva.vrbtc2 and vrbtc2=dsva.vrbtc1)
377003121130   x2c                   else
377004130118     c                   eval      dsva.vrbtc1=dsarbg.arbtc1
377005130118     c                   eval      dsva.vrbtc2=dsarbg.arbtc2
377006121130     c                   eval      wdiffe='1'
377007121130    2c                   endif
377008121130     c* Variazione Importo da assicurare
377009130116     c                   when      %subst(iv1cvb:1:1)='T'
377010121203    2c                   if        vrbias<>dsva.vrbias or
377011121203     c                             vrbvas<>dsva.vrbvas
377012121203     c* stessa divisa o non c'era importo da assicurare
377013130124    3c                   if        dsarbt.arbvas=vrbvas or vrbvas=*blanks
377014130118    4c                   if        dsarbt.arbias<vrbias
377015121130     c                   eval      dsva.vrbias=vrbias
377016121130     c                   eval      dsva.vrbvas=vrbvas
377017121130   x4c                   else
377018130118     c                   eval      dsva.vrbias=dsarbt.arbias
377019130118     c                   eval      dsva.vrbvas=dsarbt.arbvas
377020121130    4c                   endif
377021121130     c* divisa diversa imposto sempre come record originale
377022121130   x3c                   else
377023121130     c                   eval      dsva.vrbias=vrbias
377024121130     c                   eval      dsva.vrbvas=vrbvas
377025121130    3c                   endif
377026121130     c                   eval      wdiffe='1'
377027121130    2c                   endif
377028130117     c* Variazioni Volume VDL
377029130117     c                   when      iv1cvb='VV'
377030130117     c                   if        vrbvlf<>dsva.vrbvlf
377031130118     C                   IF        §bvvlf<vrbvlf
377032130117     c                   eval      dsva.vrbvlf=vrbvlf
377033130118     c                   eval      dsva.vrbfvf=vrbfvf
377034130118     c                   eval      dsva.vrbvlc=vrbvlc
377035130118     c                   eval      dsva.vrbncr=vrbncr
377036130117     c                   else
377037130118     c                   eval      dsva.vrbvlf=§bvvlf
377038130118     c                   eval      dsva.vrbfvf=§bvfvf
377039130118     c                   eval      dsva.vrbvlc=§bvvlc
377040130118     c                   eval      dsva.vrbncr=§bvncr
377041130117     c                   endif
377042130117     c                   eval      wdiffe='1'
377043130117    2c                   endif
377044130122     c* Variazioni Peso   VDL
377045130122     c                   when      iv1cvb='PV'
377046130122     c                   if        vrbpkf<>dsva.vrbpkf  or
377047130122     c                             vrbpkc<>dsva.vrbpkc  or
377048130122     c                             vrbncp<>dsva.vrbncp
377049130122     C                   IF        §bvpkf<vrbpkf or
377050130122     C                             §bvpkc<vrbpkc or
377051130122     C                             §bvncp<vrbncp
377052130122     c                   eval      dsva.vrbpkf=vrbvlf
377053130122     c                   eval      dsva.vrbfpf=vrbfpf
377054130122     c                   eval      dsva.vrbpkc=vrbpkc
377055130122     c                   eval      dsva.vrbncp=vrbncp
377056130122     c                   else
377057130122     c                   eval      dsva.vrbpkf=§bvpkf
377058130122     c                   eval      dsva.vrbfpf=§bvfpf
377059130122     c                   eval      dsva.vrbpkc=§bvpkc
377060130122     c                   eval      dsva.vrbncp=§bvncp
377061130122     c                   endif
377062130122     c                   eval      wdiffe='1'
377063130122    2c                   endif
377064130122     c* Variazioni Inoltro Z/T
377065130123     c                   when      iv1cvb='ZT'
377066130122     c                   if        vrbfin<>dsva.vrbfin
377067130122     c                   eval      dsva.vrbfin=parfin
377068130206     c                   eval      wdiffe='1'
377069130122     c                   endif
377070140123     c* Variazioni evento RIC
377071140123     c                   when      iv1cvb='RI'
377072140123     c                   if        vrbric<>dsva.vrbric
377073140123     c                   eval      dsva.vrbric=parric
377074140123     c                   eval      wdiffe='1'
377075140123     c                   endif
377076121130    1c                   endsl
377077121130     c                   endsr
377078121114      ****************************************************
377079121114      ** Recupero data penultima fatturazione mensile
377080121114      ****************************************************
377081121114     c     rec_RPFdata   Begsr
377082121114     c                   Clear                   Drpfd
377083130204     c                   Eval      KCod = 'RPF'
377084130204     c                   Movel(p)  'DATA'        KKe1
377085130204     c     ktbe          chain     tntbe01l
377086130204     c                   if        %found(tntbe01l)
377087130204     c                   Movel     Tbeuni        Drpfd
377088130204     c                   endif
377089121114     c                   endsr
377090981001     C*
