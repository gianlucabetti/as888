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
009806160108     d dar5gen       e ds
009807151007     d dar5fat       e ds
009808120424     d dtasflo       e ds
009809120424     d dtas01        e ds
009810120313      ** DS Calcolo tassazione
009811120424     d dtas          e ds                  prefix(dtas_)
009812120424     d* §tasflo      e                     extfld(tasflo)
009813120424     d dtasv         e ds                  prefix(dtasv_)
009814120313     d  sv                     1     20    dim(20)
009815120313     d  va                    21    140P 3 dim(20)
009816121220     D DSTB          E DS
009817121220     D DS1A          E DS
009818150108     D DS7R          E DS
009900071102     d
011301120313     D ksvt            s                   like(taisvt)
011303121107     D w_vrbdimp       s                   like(vrbdimp)
011304120424     D w_vrbflg        s                   like(vrbflg)
011305120319     d kAr5Trd         s                   Like(Ar5Trd)
011306120612     d savdtas         s                   Like(dtas)
011307120612     d savdtasv        s                   Like(dtasv)
011308151007     d warbdpkf        s                   Like(§ar5pktas)
011309120612     d antepo          s                   Like(tasimv)
011310120319     D Wcev            s                   like(evbcev) inz('RIC')
011311121108     D Wcvb            s                   like(vrbcvb) inz('  ')
011312121130     D Wdiffe          s              1
011313140128     D Wnotf           s              1
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
016533150330     c                             and §tbrbl='N' and taslnp<>102
016534150330     c                             and (§3asva=*blanks or §3aim0<>'S')
016535130114      * 1 - bolla fatturata in filiale --> scarto
016536130117    2c                   if        tasfiv>0 and tasfiv <> 998 and tasfiv <> 996
016537130117   x2c                   else
016538130114      * 2 - bolla tassata fino all'imponibile  --> scarto
016539121218     c                   eval      ksvt = '£'
016540121218     c     Ktai06        setll     titai01l
016541121218    3c                   If        %equal(titai01l)
016542121218     c                   eval      w_vrbflg='I'
016543121218    3c                   endif
016546130207    3c     w_vrbflg      ifne      'I'
016547151007     c* Verifico se presente record "FAT"
016548151007     c                   clear                   dar5fat
016549151007     c                   eval      kAr5Trd  = 'FAT'
016550151007     c     kFiar5        Chain     Fiar531c
016551151008If  4c                   If        %Found(Fiar531c)
016552151007     c                   eval      dar5fat=ar5uni
016553151008    4c                   endif
016554151007      * 3 - bolla tassata con peso desunto e ricevuta variazione di Peso VDL ->S
016555151007      *     carto
016556151008    4c                   if        §AR5FPTAS<>'D' or  iv1cvb<>'PV'
016557151007     c                   exsr      ges_vrb
016558151008    4c                   endif
016559151008    3c                   endif
016560130207    2c                   endif
016561130117    1c                   endif
016562130117    0c                   endif
016563130114      *  Chiusura PGM
016564130114     c                   if        iv1tla =' '
016565130114     c                   return
016566130114     c                   else
016567130114     c                   exsr      chiusura
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
035716121220     c*
035717121220     c                   clear                   w_vrbdimp
035718121220     c                   clear                   w_vrbflg
035719130117     c*
035720121220     C                   MOVEL     'TB'          COD
035721121220     C                   MOVEL(P)  tastbl        KEY
035722121220     c                   clear                   dstb
035723121220     C     KTAB          CHAIN     TABEL
035724121220     c                   if        %found(tabel00f)
035725121220     C                   MOVEL     TBLUNI        DSTB
035726121220     c                   endif
035727130118     C                   MOVEL     '3A'          COD
035728130118     C                   MOVEL(P)  tascbo        KEY
035729130118     c                   clear                   ds3a
035730130118     C     KTAB          CHAIN     TABEL
035731130118     c                   if        %found(tabel00f)
035732130118     C                   MOVEL     TBLUNI        DS3a
035733130118     c                   endif
035734121220     c                   endsr
035735130114     C**---------------------------------------------------------
035736130114     C**
035737130114     C**---------------------------------------------------------
035738130114     C     chiusura      BEGSR
035739130114     C*
035740130114     C                   CLEAR                   DSSI95
035741130114     C                   MOVEL     'C'           I95TLA
035742130114     C                   CALL      'TISI95R'
035743130114     C                   PARM                    DSSI95
035744130114
035745130114     C     WSF20         IFNE      *BLANKS
035746130114     C                   MOVEL     'C'           dtas_TASTLA
035747130206     C                   CALL      'TNSF20R'                            99
035748130114     C                   PARM                    KPJBA
035749130114     C                   PARM                    DTAS
035750130114     C                   PARM                    DTASV
035751130114     C                   ENDIF
035752130114
035753130114     c                   endsr
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
376222150330
376223150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
376224150330     c                   if        §3asva<>*blanks
376225150330     c                   eval      dtas_tassva=§3asva
376226150330     c                   endif
376227121130
376228130206     C                   CALL      'TNSF20R'
376229121130     C                   PARM                    KPJBA
376230121130     C                   PARM                    DTAS
376231121130     C                   PARM                    DTASV
376232121130     c                   endsr
376233121130     C**-------------------------------------------------------------
376234121130     C** Tassazione - Bolla già presente nelle variazioni da recuperare
376235121130     C**-------------------------------------------------------------
376236121130     C     sr_tassa2     BEGSR
376237121130     c* campi della dtas da prendere da titas
376238121130     c                   exsr      riedtasUNI
376239121130     c* campi della dtas da prendere dal record "VA" di TIVRB
376240121130     c                   exsr      riedtasVRB
376241121130     c* campi della dtas da impostare con campi della variazione in ricezione
376242121130     c                   exsr      riedtasVAR
376243121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
376244121130     c                   exsr      riedtasALTRI
376333121130      * recupero i valori  già tassati
376334121130     c                   exsr      Ric_tai
376335150330
376336150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
376337150330     c                   if        §3asva<>*blanks
376338150330     c                   eval      dtas_tassva=§3asva
376339150330     c                   endif
376340121130
376341130206     C                   CALL      'TNSF20R'
376342121130     C                   PARM                    KPJBA
376343121130     C                   PARM                    DTAS
376344121130     C                   PARM                    DTASV
376345121130     c                   endsr
376346121130      *****************************************************************
376347121130      * Impostazione campi della dtas da campi di titas
376348121130      *****************************************************************
376349121130     c     RiedtasUNI    Begsr
376350121130     c                   clear                   dtas
376351121130     c                   clear                   dtasv
376352121130     c                   eval      dtas_TASKSC=tasksc
376353121130     c                   eval      dtas_TASNCL=tasncl
376354121130     c                   eval      dtas_TASCTR=tasctr
376358121130     c                   eval      dtas_TASFDN=tasfdn
376359121130     c                   eval      dtas_TASMCT=tasmct
376360121130     c                   eval      dtas_TASNZM=tasnzm
376361121130     c                   eval      dtas_TASCAM=tascam
376362121130     c                   eval      dtas_TASFAP=tasfap
376363121130     c                   eval      dtas_TASTSP=tastsp
376367121130     c                   eval      dtas_taslna=taslna
376372121130     c                   eval      dtas_TASDIV=tasdiv
376373151012     c* tnsf20r usa taspvl come campo di ouptut quindi asterisco
376374151012     c***                eval      dtas_TASPVL=taspvl
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
376412151007     c* considero il peso ricevuto solo se bolla non tassata con peso desunto
376417151007     c                   if        §ar5fptas<>'D'
376418130118     c                   z-add     dsarbd.arbpkf dtas_taspkf
376419151007     c                   endif
376420130118     c                   z-add     dsarbd.arbvlf dtas_tasvlf
376421130118     c                   movel     dsarbd.arbfin dtas_tasfin
376422130118     c                   movel     dsarbd.arbcad dtas_tascad
376423130118     c                   movel     dsarbd.arbnzd dtas_tasnzd
376424130118    3C     iv1cvb        ifeq      'I0'
376425151008     c     iv1cvb        oreq      'I7'
376426151008     c     iv1cvb        oreq      'I8'
376427151008     c     iv1cvb        oreq      'I9'
376430121130     c                   exsr      c_tisi95
376431121130     c                   if        o95err = *blanks
376432121130     c                   eval      dtas_tascts=o95cts
376433121130     c                   endif
376434121130    3c                   endif
376435121130     c* Variazioni codice bolla/contrassegno
376436130116     c                   when      %subst(iv1cvb:1:1)='K'
376437121205     c* Contrassegno ANNULLATO: prendo i campi da tncsb
376438121205     c*                         e metto stato 9
376439130116    3c                   if        iv1can='S'
376440121205     c                   eval      dtas_tascas=dscsb.csbcas
376441121205     c                   eval      dtas_tasvca=dscsb.csbvca
376442121205     c                   eval      dtas_tascmb=dscsb.csbcmb
376443121205    4c                   if        dscsb.csbfus<>' '
376444121210     c****               eval      dtas_tastic=dscsb.csbfus
376445121210     c                   move      dscsb.csbfus  dtas_tastic
376446121205   x4c                   else
376447121210     c****               eval      dtas_tastic=dscsb.csbtpi
376448121210     c                   move      dscsb.csbtpi  dtas_tastic
376449121205    4c                   endif
376450121205     c                   eval      dtas_Tassta='9'
376451121205   x3c                   else
376452121205     c* Contrassegno NON Annullato: prendo i campi dalla variazione
376453130118     c                   eval      dtas_tascas=dsarbk.arbcas
376454130118     c                   eval      dtas_tasvca=dsarbk.arbvca
376455130118     c                   eval      dtas_tascmb=dsarbk.arbcmb
376456121205     c                   clear                   dtas_tastic
376457121205     C                   MOVEL     '1A'          COD
376458130118     C                   MOVEL(P)  dsarbk.arbtic KEY
376459121205     C     KTAB          CHAIN     TABEL
376460121205    4c                   if        %found(tabel00F)
376461121205     C                   MOVEL     TBLUNI        DS1A
376462121205     c                   move      §1afmb        dtas_tastic
376463121205    4c                   endif
376464121205     c* .......... flag stato: metto 0 o 3 in base a quello che c'è su TNCSB
376465121205    4c                   if        dscsb.csbsta<>3
376466121205     c                   eval      dtas_Tassta='0'
376467121205     c                   else
376468121205     c                   eval      dtas_tassta='3'
376469121205    4c                   endif
376470121205    3c                   endif
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
376501151008     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
376502151008     c                   if        §AR5PKTAS=0
376503120424     c                   eval      dtas_TASPKF=taspkf
376504120424     c                   eval      dtas_TASPKC=taspkc
376505120424     c                   eval      dtas_TASNCP=tasncp
376508120424     c                   eval      dtas_TASVLF=tasvlf
376509151008     c                   else
376510151008     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
376511151008     c                   eval      dtas_TASPKF=§ar5pktas
376512151008     c                   eval      dtas_TASVLF=§AR5VLTAS
376513151008     c                   endif
376514120424     c                   eval      dtas_TASCTS=tascts
376515120424     c                   eval      dtas_TASNZD=tasnzd
376516120424     c                   eval      dtas_TASCAD=tascad
376517120424     c                   eval      dtas_TASFIN=tasfin
376518120424     c                   eval      dtas_TASTC1=tasftc
376519120424     c                   eval      dtas_TASTC2=tastc2
376520120424     c                   eval      dtas_taslnp=taslnp
376521121130     c                   eval      dtas_TASTBL =tastbl
376522120424     c                   eval      dtas_TASVAS =tasvas
376523120424     c                   eval      dtas_TASIAS =tasias
376524120424     c                   eval      dtas_TASQFT =tasqft
376525120424     c                   endsr
376526121130      *****************************************************************
376527121130      * Altri campi della dtas da prendere da titas
376528121130      *****************************************************************
376529121130     c     RiedtasAltri  Begsr
376530121130     c                   clear                   dAr5Ban
376531121130     c                   Clear                   dAr5Bnb
376532121130     c                   Clear                   kpjbu
376533121130     c                   movel     tasaas        dtas_tasdsp
376534121130     c                   move      tasmgs        dtas_tasdsp
376535121130     c                   eval      dtas_tasdct=dtas_tasdsp
376536121130
376537121130      * Flag SI NO DDT
376538121130    2C                   If        tasll1 = 'C' or tasll1 = 'S'
376539121130     C                   movel     'S'           dtas_Tasddt
376540121130   x2C                   Else
376541121130     C                   Clear                   dtas_Tasddt
376542121130    2C                   Endif
376543121130     c                   clear                   dtas01
376544121130     c                   movel     tasfbr        §asfbr
376545121130     c                   movel     tascca        §ascca
376546121130      * valorizzo flag email al destinatario
376547121130     c                   movel     tasflo        dtasflo
376548121130     c                   move      §floemd       §asemd
376552150108     c* flag pin code
376553150108     c                   if        tasgma<>*blanks
376554150108     c                   clear                   ds7r
376555150108     c                   movel     '7R'          cod
376556150108     c                   movel(p)  tasgma        key
376557150108     c     ktab          chain     tabel
376558150108    8c                   if        %found(tabel00f)
376559150108     c                   movel     tbluni        ds7r
376560150108    8c                   endif
376561150108     c                   if        §7rpincode='S'
376562150108     c                   movel     'S'           §aspin
376563150108     c                   endif
376564150108     c                   endif
376565121130
376566121130     c                   eval      dtas_tasflo = dtas01
376567160107     c* solo per prima bolla
376568160107     c                   if        §3atb2=*blanks or %subst(tastbl:1:1)
376569160107     c                             <>%subst(§3atb2:1:1)
376570160107     c
376571160107     c* flag DIRITTO DI CHIAMATA  da 10° byte di TASFLO di TITAS
376572160108     c                   move      §floado       dtas_tasprt
376573160107     c* flag per ADDEBITO PACKING LIST da fiar5 "GEN"
376574160107     c                   clear                   dar5gen
376575160107     c                   eval      kAr5Trd  = 'GEN'
376576160107     c     kFiar5        Chain     Fiar531c
376577160107If  3c                   If        %Found(Fiar531c)
376578160107     c                   Movel     Ar5Uni        dAr5gen
376579160107    3c                   EndIf
376580160107     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
376581160108     c                   Eval      dtas_TasSpl = 'S'
376582160107     c                   endif
376583160107     c                   endif
376584121130
376585121130     c
376586121130      * Bancali
376587121130    2c                   If        %Subst(TasGva:1:1) = 'B'
376588121130
376589121130     c                   eval      kAr5Trd  = 'BAN'
376590121130     c     kFiar5        Chain     Fiar531c
376591121130If  3c                   If        %Found(Fiar531c)
376592121130     c                   Movel     Ar5Uni        dAr5Ban
376593121130    3c                   EndIf
376594121130      * Bancali
376595121130     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
376596121130    2c                   EndIf
376597121130      **
376598121130      * Colli Originali
376599121130    2c                   If        %Subst(TasGva:1:1) = 'O'
376600121130     c                   eval      kAr5Trd  = 'BNB'
376601121130     c     kFiar5        Chain     Fiar531c
376602121130If  3c                   If        %Found(Fiar531c)
376603121130     c                   Movel     Ar5Uni        dAr5Bnb
376604121130    3c                   EndIf
376605121130      * Colli
376606121130     c                   Eval      dtas_TasNcl = §Ar5bcor
376607121130    2c                   EndIf
376608121130
376609121130      **** verifico se devo calcolare l'addebito di lasciato avviso
376610121130      **** verifico se c'è evento
376611140127      **** Se Variazione "RI" non ho bisogno di verificare la presenza dell'
376612140127      **** evento
376613140127     c                   if        iv1cvb='RI'
376614140127     c                   eval      dtas_tasric = 'S'
376615140127     c                   else
376616140127     c                   clear                   evbdtr
376617140127     c     Kevb4         setll     Fnevb04l
376618140127     c     Kevb4         reade     Fnevb04l
376619140127     c                   dow       not %eof(fnevb04l) and evbdtr=0
376620140127     c     Kevb4         reade     Fnevb04l
376621140127     c                   enddo
376622140127     c                   if        evbdtr>0
376623140127     c                   eval      dtas_tasric = 'S'
376624140127     c                   endif
376625140127     c                   endif
376626121130
376627121130      * Assicurazione
376628121130    2c                   IF        Tasfcm = 'F'
376629121130     c                   clear                   dtas_tasias
376630121130     c                   clear                   dtas_tasvas
376631121130    2c                   Endif
376632121130     c                   endsr
376633121130
376634120316      *****************************************************************
376635120316      *  Recupero importi di tassazione dal file titai
376636120316      *****************************************************************
376637120316     c     Ric_tai       Begsr
376638120316      *
376639120316     c                   z-add     0             k
376640120316
376641120316     c     ktai05        setll     titai01l
376642120316
376643120316     c                   If        %equal(titai01l)
376644120316
376645120316     c                   do        *hival
376646120316
376647120316     c     ktai05        reade     titai01l
376648120316
376649120316     c                   If        %eof(titai01l)
376650120316     c                   leave
376651120316     c                   endif
376652120316      * imponibile
376653120316     c                   if        taisvt = '£'
376654120424     c                   eval      dtas_tasimv = taivat
376655120316     c                   endif
376656120316      * porto
376657120316     c                   if        taisvt = ' '
376658120424     c                   eval      dtas_taspor = taivat
376659120316     c                   endif
376660120316      * varie
376661120316     c                   if        taisvt <> ' ' and taisvt <> '£'
376662120612     c* se presente varia N 88.888.888 devo prima tassarla
376663120612    4c                   if        taisvt='N' and taivat=88888888
376664120612     c                   eval      savdtas=dtas
376665120612     c                   eval      savdtasv=dtasv
376666120612     c                   exsr      tassan
376667120612     c                   eval      dtas=savdtas
376668120612     c                   eval      dtasv=savdtasv
376669120612     c                   add       1             k
376670120612     c                   eval      sv(k) ='N'
376671120613     c                   eval      va(k) =antepo
376672120612   x4c                   else
376673120316     c                   add       1             k
376674120316     c                   eval      sv(k) = taisvt
376675120316     c                   eval      va(k) = taivat
376676120612    4c                   endif
376677120316     c                   endif
376678120316
376679120316     c                   enddo
376680120316
376681120316     c                   else
376682120316      * se non è pretassata pulisco la divisa
376683120424     c                   clear                   dtas_tasdiv
376684120316     c                   endif
376685120424     c                   endsr
376686120612      ****************************************************
376687120612      ** Tassazione varia N 88888888 (Anteporto)
376688120612      ****************************************************
376689120612     c     tassan        Begsr
376690120613     C                   CLEAR                   DTAS_tasimv
376691120613     C                   CLEAR                   DTAS_taspor
376692120613     C                   CLEAR                   DTASV
376693120612     c                   clear                   antepo
376694120612      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
376695150108      * idem per flag Pin Code
376696120612     c                   eval      dtas01 = dtas_tasflo
376697120612     c                   clear                   §asemd
376698150108     c                   clear                   §aspin
376699120612     c                   eval      dtas_tasflo = dtas01
376700120612      *
376701130206     C                   CALL      'TNSF20R'
376702120612     C                   PARM                    KPJBA
376703120612     C                   PARM                    DTAS
376704120612     C                   PARM                    DTASV
376705120612      *
376706120612      * Tassazione andata buon fine
376707120613     C     dtas_taserr   IFEQ      *BLANKS
376708120613     c                   eval      antepo=dtas_tasimv
376709120612     C                   ENDIF
376710120612     c                   endsr
376711120316      ****************************************************
376712120316      ** Ricerca Contrassegno
376713120316      ****************************************************
376714120316     c     Carcsb        Begsr
376715120316
376716130118     c     kann          chain     Tncsb03l
376717120316     c                   If        %found(tncsb03l) and
376718120316      * solo per stesso tipo bolla
376719120316     c                             tastbl = csbtbl
376720120424     c                   movel     csbsta        dtas_Tassta
376721120424     c                   z-add     csbcas        dtas_Tascas
376722120316     c                   if        csbfus <> *blanks
376723120424     c                   move      csbfus        dtas_tastic
376724120316     c                   else
376725120424     c                   move      csbtpi        dtas_tastic
376726120316     c                   end
376727120316      * mittente
376728120424     c                   movel     csbvca        dtas_tasvca
376729120424     c                   z-add     csbcmb        dtas_tascmb
376730120316
376731120316     c                   endif
376732120316
376733120316     c                   endsr
376734120319      ****************************************************
376735120319      ** Scrittura work-file delle bolle già fatturate
376736120319      ****************************************************
376737120319     c     wrtvrb        Begsr
376738121130     c                   clear                   tivrb000
376739121130     c                   eval      vrbaas=tasaas
376740121130     c                   eval      vrblnp=taslnp
376741121130     c                   eval      vrbnrs=tasnrs
376742121130     c                   eval      vrbnsp=tasnsp
376743121130     c                   eval      vrbtbl=tastbl
376744121130     c                   time                    vrbtmst
376745121130     c                   eval      vrbdft=tasdft
376746151008     c* Presente record "FAT" --> memorizzo peso e volume di questo
376747151008     c                   if        §AR5PKTAS>0
376748151008     c                   eval      VRBPKF=§ar5pktas
376749151008     c                   eval      VRBfpf=§ar5fptas
376750151008     c                   eval      vrbvlf=§AR5VLTAS
376751151008     c                   eval      VRBFVF=§AR5FVTAS
376752151008     c* . . . se bolla tassata con peso desunto non memorizzo il peso VDL
376753151008     c                   if        §ar5fptas<>'D'
376754151008     c                   eval      VRBPKC=taspkc
376755151008     c                   eval      VRBNCP=tasncp
376756151008     c                   endif
376757151008     c                   else
376758151008     c* Non presente il record "FAT" --> peso e volume da titas
376759130115     c                   eval      VRBPKC=taspkc
376760130115     c                   eval      VRBNCP=tasncp
376761130115     c                   eval      VRBPKF=taspkf
376762130115     c                   eval      VRBFPF=tasfpf
376763130115     c                   eval      VRBVLF=tasvlf
376764130115     c                   eval      VRBFVF=tasfvf
376765151008     c                   endif
376766151008     c                   eval      VRBVLC=tasvlc
376767151008     c                   eval      VRBNCR=tasncr
376768130115     c                   eval      VRBQFT=tasqft
376769130115     c                   eval      VRBIAS=tasias
376770130115     c                   eval      VRBVAS=tasvas
376771130115     c                   eval      VRBCTS=tascts
376772130115     c                   eval      VRBFIN=tasfin
376773130115     c                   eval      VRBCAD=tascad
376774130115     c                   eval      VRBNZD=tasnzd
376775130115     c                   eval      VRBFFD=tasffd
376776130115     c                   eval      VRBTC1=tasftc
376777130115     c                   eval      VRBTC2=tastc2
376778130116    2c                   if        %subst(iv1cvb:1:1)<>'K'
376779121109     c                   eval      VRBTIC=dtas_tastic
376780121109     c                   eval      VRBCAS=dtas_tascas
376781121109     c                   eval      VRBVCA=dtas_tasvca
376782121109     c                   eval      VRBCMB=dtas_tascmb
376783121115     c                   move      dtas_tassta   vrbsta
376784121109   x2c                   else
376785121115    3c                   if        dscsb.csbtbl=tastbl
376786121115    4c                   if        dscsb.csbfus <> *blanks
376787121115     c                   move      dscsb.csbfus  vrbtic
376788121109   x4c                   else
376789121115     c                   move      dscsb.csbtpi  vrbtic
376790121109    4c                   end
376791121115     c                   eval      VRBCAS=dscsb.csbcas
376792121115     c                   eval      VRBVCA=dscsb.csbvca
376793121115     c                   eval      VRBCMB=dscsb.csbcmb
376794121115     c                   eval      VRBsta=dscsb.csbsta
376795121109     c                   endif
376796121109    2c                   endif
376797140123    2c                   if        iv1cvb<>'RI'
376798140123     c                   eval      vrbric=dtas_tasric
376799140123    2c                   endif
376800121108     c                   write     tivrb000
376801121129     c* SCRITTURA RECORD "VA" (Prima volta)
376802121129     c                   eval      vrbcvb='VA'
376803130118     c                   exsr      RieVrbUni
376804121129     c                   write     tivrb000
376805121129     c*
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
376938151008     c*          N.B:Se tassato con peso desunto il peso (VRBPKF) del record " "
376939151008      *              e del record "VA" è sempre lo stesso e devo anche ingnorare
376940151008      *              il peso ricevuto nel record di variazione
376941151008     c*
376942121130    2c                   if        vrbpkf<>dsva.vrbpkf or
376943121130     c                             vrbvlf<>dsva.vrbvlf
376945151008     C                   if        (§AR5FPTAS<>'D' and dsarbd.arbpkf<vrbpkf)
376946151008     C                             or dsarbd.arbvlf<vrbvlf
376947121130     c                   eval      dsva.vrbpkf=vrbpkf
376948121130     c                   eval      dsva.vrbvlf=vrbvlf
376949121130     c                   else
376950151008     c                   if        §AR5FPTAS<>'D'
376951151008     c                   eval      dsva.vrbpkf=dsarbd.arbpkf
376952151008     c                   endif
376953130118     c                   eval      dsva.vrbvlf=dsarbd.arbvlf
376954121130     c                   endif
376955121130     c                   eval      wdiffe='1'
376956121130    2c                   endif
376957121130     c* . . . . Flag inoltro CAP e Nazione e Codice Tassazione
376958121130    2c                   if        vrbfin<>dsva.vrbfin or
376959121130     c                             vrbcad<>dsva.vrbcad or
376960121130     c                             vrbnzd<>dsva.vrbnzd or
376961121130     c                             vrbcts<>dsva.vrbcts
376962130118     c                   eval      dsva.vrbfin=dsarbd.arbfin
376963130118     c                   eval      dsva.vrbcad=dsarbd.arbcad
376964130118     c                   eval      dsva.vrbnzd=dsarbd.arbnzd
376965121130     c                   eval      dsva.vrbcts=dtas_tascts
376966121130     c                   eval      wdiffe='1'
376967121130    2c                   endif
376968121130     c* Variazioni codice bolla/contrassegno
376969130116     c                   when      %subst(iv1cvb:1:1)='K'
376970121203    2c                   if        vrbtic<>dsva.vrbtic or
376971121203     c                             vrbcas<>dsva.vrbcas or
376972121203     c                             vrbvca<>dsva.vrbvca or
376973121203     c                             vrbcmb<>dsva.vrbcmb or
376974121203     c                             vrbsta<>dsva.vrbsta
376975121203     c* stessa divisa o non c'era il contrassegno
376976121205    3c                   if        dtas_tasvca=vrbvca or vrbvca=*blanks
376977121130     c* se importo più basso del valore iniziale --> tengo l'iniziale
376978121205    4c                   if        (dtas_tassta='9' and vrbsta<>9) or
376979121205     c                             (dtas_tascas<vrbcas and
376980121205     c                             ((dtas_tassta='9' and vrbsta=9) or
376981121205     c                              (dtas_tassta<>'9' and vrbsta<>9)))
376982121130     c                   z-add     vrbcas        dsva.vrbcas
376983121130     c                   eval      dsva.vrbtic=vrbtic
376984121130     c                   eval      dsva.vrbvca=vrbvca
376985121130     c                   eval      dsva.vrbcmb=vrbcmb
376986121130     c                   eval      dsva.vrbsta=vrbsta
376987121130   x4c                   else
376988121130     c* se importo maggiore o uguale tengo la variazione
376989121205     c                   z-add     dtas_tascas   dsva.vrbcas
376990121205     c                   eval      dsva.vrbtic=dtas_tastic
376991121205     c                   eval      dsva.vrbvca=dtas_tasvca
376992121205     c                   eval      dsva.vrbcmb=dtas_tascmb
376993121205     c                   move      dtas_tassta   dsva.vrbsta
376994121130    4c                   endif
376995121130   x3c                   else
376996121130     c* divisa diversa imposto sempre come l'originale
376997121130     c                   z-add     vrbcas        dsva.vrbcas
376998121130     c                   eval      dsva.vrbtic=vrbtic
376999121130     c                   eval      dsva.vrbvca=vrbvca
377000121130     c                   eval      dsva.vrbcmb=vrbcmb
377001121130     c                   eval      dsva.vrbsta=vrbsta
377002121130    3c                   endif
377003121130     c                   eval      wdiffe='1'
377004121130    2c                   endif
377005121130     c* Variazioni Consegne particolari
377006130116     c                   when      %subst(iv1cvb:1:1)='C'
377007121130    2c                   if        (vrbtc1=dsva.vrbtc1 and vrbtc2=dsva.vrbtc2)
377008121130     c                              or
377009121130     c                             (vrbtc1=dsva.vrbtc2 and vrbtc2=dsva.vrbtc1)
377010121130   x2c                   else
377011130118     c                   eval      dsva.vrbtc1=dsarbg.arbtc1
377012130118     c                   eval      dsva.vrbtc2=dsarbg.arbtc2
377013121130     c                   eval      wdiffe='1'
377014121130    2c                   endif
377015121130     c* Variazione Importo da assicurare
377016130116     c                   when      %subst(iv1cvb:1:1)='T'
377017121203    2c                   if        vrbias<>dsva.vrbias or
377018121203     c                             vrbvas<>dsva.vrbvas
377019121203     c* stessa divisa o non c'era importo da assicurare
377020130124    3c                   if        dsarbt.arbvas=vrbvas or vrbvas=*blanks
377021130118    4c                   if        dsarbt.arbias<vrbias
377022121130     c                   eval      dsva.vrbias=vrbias
377023121130     c                   eval      dsva.vrbvas=vrbvas
377024121130   x4c                   else
377025130118     c                   eval      dsva.vrbias=dsarbt.arbias
377026130118     c                   eval      dsva.vrbvas=dsarbt.arbvas
377027121130    4c                   endif
377028121130     c* divisa diversa imposto sempre come record originale
377029121130   x3c                   else
377030121130     c                   eval      dsva.vrbias=vrbias
377031121130     c                   eval      dsva.vrbvas=vrbvas
377032121130    3c                   endif
377033121130     c                   eval      wdiffe='1'
377034121130    2c                   endif
377035130117     c* Variazioni Volume VDL
377036130117     c                   when      iv1cvb='VV'
377037130117     c                   if        vrbvlf<>dsva.vrbvlf
377038130118     C                   IF        §bvvlf<vrbvlf
377039130117     c                   eval      dsva.vrbvlf=vrbvlf
377040130118     c                   eval      dsva.vrbfvf=vrbfvf
377041130118     c                   eval      dsva.vrbvlc=vrbvlc
377042130118     c                   eval      dsva.vrbncr=vrbncr
377043130117     c                   else
377044130118     c                   eval      dsva.vrbvlf=§bvvlf
377045130118     c                   eval      dsva.vrbfvf=§bvfvf
377046130118     c                   eval      dsva.vrbvlc=§bvvlc
377047130118     c                   eval      dsva.vrbncr=§bvncr
377048130117     c                   endif
377049130117     c                   eval      wdiffe='1'
377050130117    2c                   endif
377051130122     c* Variazioni Peso   VDL
377052130122     c                   when      iv1cvb='PV'
377053130122     c                   if        vrbpkf<>dsva.vrbpkf  or
377054130122     c                             vrbpkc<>dsva.vrbpkc  or
377055130122     c                             vrbncp<>dsva.vrbncp
377056130122     C                   IF        §bvpkf<vrbpkf or
377057130122     C                             §bvpkc<vrbpkc or
377058130122     C                             §bvncp<vrbncp
377059130122     c                   eval      dsva.vrbpkf=vrbvlf
377060130122     c                   eval      dsva.vrbfpf=vrbfpf
377061130122     c                   eval      dsva.vrbpkc=vrbpkc
377062130122     c                   eval      dsva.vrbncp=vrbncp
377063130122     c                   else
377064130122     c                   eval      dsva.vrbpkf=§bvpkf
377065130122     c                   eval      dsva.vrbfpf=§bvfpf
377066130122     c                   eval      dsva.vrbpkc=§bvpkc
377067130122     c                   eval      dsva.vrbncp=§bvncp
377068130122     c                   endif
377069130122     c                   eval      wdiffe='1'
377070130122    2c                   endif
377071130122     c* Variazioni Inoltro Z/T
377072130123     c                   when      iv1cvb='ZT'
377073130122     c                   if        vrbfin<>dsva.vrbfin
377074130122     c                   eval      dsva.vrbfin=parfin
377075130206     c                   eval      wdiffe='1'
377076130122     c                   endif
377077140123     c* Variazioni evento RIC
377078140123     c                   when      iv1cvb='RI'
377079140123     c                   if        vrbric<>dsva.vrbric
377080140123     c                   eval      dsva.vrbric=parric
377081140123     c                   eval      wdiffe='1'
377082140123     c                   endif
377083121130    1c                   endsl
377084121130     c                   endsr
377085121114      ****************************************************
377086121114      ** Recupero data penultima fatturazione mensile
377087121114      ****************************************************
377088121114     c     rec_RPFdata   Begsr
377089121114     c                   Clear                   Drpfd
377090130204     c                   Eval      KCod = 'RPF'
377091130204     c                   Movel(p)  'DATA'        KKe1
377092130204     c     ktbe          chain     tntbe01l
377093130204     c                   if        %found(tntbe01l)
377094130204     c                   Movel     Tbeuni        Drpfd
377095130204     c                   endif
377096121114     c                   endsr
377097981001     C*
