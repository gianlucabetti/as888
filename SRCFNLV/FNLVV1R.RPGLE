000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200121218     H* FNLVV1R *----------------------------------------------------*
000300121218     H*  Gestione file recupero variazioni post-fatturazione         *
000400130124     h*
000500130124     h* N.b.: Per una corretta gestione il chiamante deve passare
000600130124     h*       il parametro contenente il record di TITAS prima della variazione
000700130124     h*       (partitas) tutte le
000800130124     h*       volte che titas viene aggiornato PRIMA del richiamo a
000900130124     h*       questo pgm.
001000130124     h*       Se invece l'aggiornamento di titas da parte del chiamante
001100130124     h*       avviene DOPO il richiamo è possibile sia passare il record (partit
001200130222     h*       as) sia la key spedizione
001300000000     H*--------------------------------------------------------------*
001400130204     FTITAS30C  iF   E           K DISK    usropn extfile(tas30)
001500130204     FTNCSB03L  iF   E           K DISK    usropn extfile(csb03)
001600130204     fTitaI01L  if   e           k disk    usropn extfile(tai01)
001700130204     ftntbe01L  if   e           k disk    usropn extfile(tbe01)
001800000000     FTABEL00F  IF   E           K DISK
001900900426     FAZORG01L  IF   E           K DISK
002000130204     Ffiar531c  if   e           k disk    usropn extfile(ar531)
002100120319     Ffnevb04l  if   e           k disk
002200130204     Ftivrb01l  uf a e           k disk    usropn extfile(vrb01)
002300121218
002400920113     D KPJBA         E DS
002500121219     d fnlvv1ds      e ds
002600121220      * ATTENZIONE
002700130118     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
002800121220     d* riverificare la definizione del campo parvar
002900170306     d dsarbk        e ds                  extname(fnarbk2t) qualified
003000170306     d dsarbd        e ds                  extname(fnarbd2t) qualified
003100170306     d dsarbt        e ds                  extname(fiarbt2t) qualified
003200170306     d dsarbg        e ds                  extname(fnarbg2t) qualified
003300130116     d dsarbv        e ds
003400130124     d darbt         e ds
003500121220      * Definizione del seguente campo in base alla dsarbd che è la più lunga
003600121220      * di tutte.
003700130206     dparvar           s            224
003800130122     dparfin           s                   like(tasfin)
003900140123     dparric           s                   like(dtas_tasric)
004000130114
004100130114     dpartitas         s                   like(titasds)
004200130118     dparcsb           s                   like(dscsb)
004300970714     D* DS PER TISI95R - CONTROLLO CAP
004400970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
004500021014     D ddatiute      e ds
004600021014     D azuteds       e ds                  extname(AZUTE00F)
004700020604     D tibs34ds      E DS
004800121116     D titasds       E DS                  EXTNAME(titas00f)
004900121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
005000121130     D dsvrb         E DS                  EXTNAME(tivrb00f)
005100121130     D dsVA          E DS                  EXTNAME(tivrb00f) qualified
005200130118     D og143         E DS
005300130118     D ds3a          E DS
005400121114     d drpfd         e ds
005500120424     d dar5ban       e ds
005600120424     d dar5bnb       e ds
005700160108     d dar5gen       e ds
005800151007     d dar5fat       e ds
005900120424     d dtasflo       e ds
006000120424     d dtas01        e ds
006100120313      ** DS Calcolo tassazione
006200120424     d dtas          e ds                  prefix(dtas_)
006300120424     d* §tasflo      e                     extfld(tasflo)
006400120424     d dtasv         e ds                  prefix(dtasv_)
006500120313     d  sv                     1     20    dim(20)
006600120313     d  va                    21    140P 3 dim(20)
006700121220     D DSTB          E DS
006800121220     D DS1A          E DS
006900150108     D DS7R          E DS
007000071102     d
007100120313     D ksvt            s                   like(taisvt)
007200121107     D w_vrbdimp       s                   like(vrbdimp)
007300120424     D w_vrbflg        s                   like(vrbflg)
007400120319     d kAr5Trd         s                   Like(Ar5Trd)
007500120612     d savdtas         s                   Like(dtas)
007600120612     d savdtasv        s                   Like(dtasv)
007700151007     d warbdpkf        s                   Like(§ar5pktas)
007800120612     d antepo          s                   Like(tasimv)
007900120319     D Wcev            s                   like(evbcev) inz('RIC')
008000121108     D Wcvb            s                   like(vrbcvb) inz('  ')
008100121130     D Wdiffe          s              1
008200140128     D Wnotf           s              1
008300011218
008400011218     d flgriorg        s              1
008500011218     d xx              s              2  0
008600070119     d commt           s             80
008700070119     d lenght          s             15  5 inz(80)
008800120424     D k               s              5  0
008900120424     D wsf20           s              1
009000130204     d kcod            S                   LIKE(tbecod)
009100130204     d kke1            S                   LIKE(tbeke1)
009200130204     d TAS30           s             21    inz('GAITRAGRPS/TITAS30C')
009300130204     d csb03           s             21    inz('GAITRAGRPS/TNCSB03L')
009400130204     d tai01           s             21    inz('GAITRAGRPS/TITAI01L')
009500130204     d ar531           s             21    inz('GAITRAGRPS/FIAR531C')
009600130204     d tbe01           s             21    inz('GAITRAGRPS/TNTBE01L')
009700130204     d vrb01           s             21    inz('GAITRAPRV/TIVRB01L')
009800170411
009900170411     d Dataiso         s               d   datfmt(*iso)
010000170411     d wdataRPF        s              8  0
010100941130     I*
010200920113     I/SPACE 3
010300920113     C****************************************************************
010400920113     C*  RIEPILOGO INDICATORI
010500920113     C***************************************************************
010600121218     C* 01    -
010700920113     C*****************************************************************
010800920113     C*
010900121219     C     *ENTRY        PLIST
011000121219     C                   PARM                    KPJBA
011100121220     c                   parm                    Parvar
011200121219     c                   parm                    partitas
011300121219     c                   parm                    parcsb
011400121219
011500121219     c                   movel     kpjbu         fnlvv1ds
011600130204
011700130114
011800130117    0c                   if        iv1tla<>'C'
011900130222
012000130222     c                   exsr      openfile
012100121220     c* Impostazione
012200121220     c                   exsr      impo
012300121218     c* 0 - Proseguo solo se la bolla di sede non è un C/servizio
012400130117     c*     e se è già stata contabilizzata
012500140128     c*     e se trovato record su titas se era da chainare
012600130117    1c                   if        §tbfcb='1' and tasfic<>*blanks
012700140128     c                             and wnotf=' '
012800150330     c                             and §tbrbl='N' and taslnp<>102
012900150330     c                             and (§3asva=*blanks or §3aim0<>'S')
013000130114      * 1 - bolla fatturata in filiale --> scarto
013100170717    2c***                if        tasfiv>0 and tasfiv <> 998 and tasfiv <> 996
013200170717    2c                   if        tasfiv>0 and tasfiv <  900
013300130117   x2c                   else
013400130114      * 2 - bolla tassata fino all'imponibile  --> scarto
013500121218     c                   eval      ksvt = '£'
013600121218     c     Ktai06        setll     titai01l
013700121218    3c                   If        %equal(titai01l)
013800121218     c                   eval      w_vrbflg='I'
013900121218    3c                   endif
014000130207    3c     w_vrbflg      ifne      'I'
014100151007     c* Verifico se presente record "FAT"
014200151007     c                   clear                   dar5fat
014300151007     c                   eval      kAr5Trd  = 'FAT'
014400151007     c     kFiar5        Chain     Fiar531c
014500151008If  4c                   If        %Found(Fiar531c)
014600151007     c                   eval      dar5fat=ar5uni
014700151008    4c                   endif
014800160426      * 3 - bolla tassata con peso/vol desunto e ricevo variaz. di Peso/vol VDL
014900160426      *     --> Scarto
015000160426    4c*****              if        §AR5FPTAS<>'D' or  iv1cvb<>'PV'
015100160426     c                   if        (§AR5FPTAS<>'D' and §AR5FVTAS<>'D')
015200160426     c                             or
015300160426     c                             (iv1cvb<>'PV' and iv1cvb<>'VV')
015400151007     c                   exsr      ges_vrb
015500151008    4c                   endif
015600151008    3c                   endif
015700130207    2c                   endif
015800130117    1c                   endif
015900130117    0c                   endif
016000130114      *  Chiusura PGM
016100130114     c                   if        iv1tla =' '
016200130114     c                   return
016300130114     c                   else
016400130114     c                   exsr      chiusura
016500920113     C                   SETON                                        LR
016600130114     c                   endif
016700130204     C**---------------------------------------------------------
016800130204     C** Apertura file
016900130204     C**---------------------------------------------------------
017000130204     C     openfile      BEGSR
017100130204     c* Se non aperto apro il file
017200130204     c* TITAS
017300130204     c                   if        not %open(titas30c)
017400130204     C                   OPEN(e)   titas30c
017500130204     c
017600130204     c                   if        not %open(titas30c)
017700130204     c                   eval      %subst(tas30:7:4)='GRU '
017800130204     C                   OPEN      titas30c
017900130204     c                   endif
018000130204     c                   endif
018100130204     c* TNCSB
018200130204     c                   if        not %open(tncsb03l)
018300130204     C                   OPEN(e)   tncsb03l
018400130204     c
018500130204     c                   if        not %open(tncsb03l)
018600130204     c                   eval      %subst(csb03:7:4)='GRU '
018700130204     C                   OPEN      tncsb03l
018800130204     c                   endif
018900130204     c                   endif
019000130204     c* TITAI
019100130204     c                   if        not %open(titai01l)
019200130204     C                   OPEN(e)   titai01l
019300130204     c
019400130204     c                   if        not %open(titai01l)
019500130204     c                   eval      %subst(tai01:7:4)='GRU '
019600130204     C                   OPEN      titai01l
019700130204     c                   endif
019800130204     c                   endif
019900130204     c* fiar5
020000130204     c                   if        not %open(fiar531c)
020100130204     C                   OPEN(e)   fiar531c
020200130204     c
020300130204     c                   if        not %open(fiar531c)
020400130204     c                   eval      %subst(ar531:7:4)='GRU '
020500130204     C                   OPEN      fiar531c
020600130204     c                   endif
020700130204     c                   endif
020800130204     c* tntbe
020900130204     c                   if        not %open(tntbe01l)
021000130204     C                   OPEN(e)   tntbe01l
021100130204     c
021200130204     c                   if        not %open(tntbe01l)
021300130207     c                   eval      %subst(tbe01:7:4)='GRU '
021400130204     C                   OPEN      tntbe01l
021500130204     c                   endif
021600130204     c                   endif
021700130204     c* tivrb
021800130204     c                   if        not %open(tivrb01l)
021900130204     C                   OPEN(e)   tivrb01l
022000130204     c
022100130204     c                   if        not %open(tivrb01l)
022200130204     c                   eval      %subst(vrb01:7:3)='201'
022300130204     C                   OPEN      tivrb01l
022400130204     c                   endif
022500130204     c                   endif
022600130204     c                   endsr
022700121220     C**---------------------------------------------------------
022800121220     C**
022900121220     C**---------------------------------------------------------
023000121220     C     impo          BEGSR
023100121220     c*
023200121220     c* Valorizzazione DS delle variazioni effettuate:
023300121220     c* in base alla casuale variazione ricevuta valorizzo la ds relativa
023400121220     c                   select
023500130123     c                   when      %subst(iv1cvb:1:1)='I'
023600121220     c                   eval      dsarbd=parvar
023700121220     c                   when      %subst(iv1cvb:1:1)='K'
023800121220     c                   eval      dsarbk=parvar
023900130124     c                   when      %subst(iv1cvb:1:1)='T'
024000130124     c                   if        iv1pgm='TRTR39R'
024100121220     c                   eval      dsarbt=parvar
024200130124     c                   else
024300130124     c                   eval      darbt=parvar
024400130124     c                   z-add     §btias        dsarbt.arbias
024500130124     c                   movel     §btvas        dsarbt.arbvas
024600130124     c                   endif
024700121220     c                   when      %subst(iv1cvb:1:1)='C'
024800121220     c                   eval      dsarbg=parvar
024900130118     c                   when      iv1cvb='VV' or iv1cvb='PV'
025000130118     c                   eval      dsarbv=parvar
025100130123     c                   when      iv1cvb='ZT'
025200130122     c                   eval      parfin=%subst(parvar:1:1)
025300140123     c                   when      iv1cvb='RI'
025400140123     c                   eval      parric=%subst(parvar:1:1)
025500121220     c                   endsl
025600121220     c* se ricevuta la key spedizione la utilizzo per chain titas e csb
025700121220     c* altrimenti utilizzo 3° e 4° parametro
025800140128     c                   clear                   wnotf
025900121220     c                   if        iv1aas<>0
026000140128     c* se non ricevuto il tipo bolla eseguo ricerca
026100140128     c                   if        iv1tbl=*blanks
026200140128     c     kbols         setgt     titas30c
026300140128     c     kbols         readpe    titas30c
026400140128     c                   else
026500121220     c     kbol          chain     titas30c
026600140128     c                   endif
026700140128     c                   if        %eof (titas30c)
026800140128     c                   eval      wnotf='1'
026900140128     c                   endif
027000121220     c                   else
027100130114     c                   eval      titasds=partitas
027200130114     c                   if        %subst(iv1cvb:1:1)='K'
027300130114     c                   eval      dscsb=parcsb
027400130114     c                   endif
027500121220     c                   endif
027600121220     c*
027700121220     c                   clear                   w_vrbdimp
027800121220     c                   clear                   w_vrbflg
027900130117     c*
028000121220     C                   MOVEL     'TB'          COD
028100121220     C                   MOVEL(P)  tastbl        KEY
028200121220     c                   clear                   dstb
028300121220     C     KTAB          CHAIN     TABEL
028400121220     c                   if        %found(tabel00f)
028500121220     C                   MOVEL     TBLUNI        DSTB
028600121220     c                   endif
028700130118     C                   MOVEL     '3A'          COD
028800130118     C                   MOVEL(P)  tascbo        KEY
028900130118     c                   clear                   ds3a
029000130118     C     KTAB          CHAIN     TABEL
029100130118     c                   if        %found(tabel00f)
029200130118     C                   MOVEL     TBLUNI        DS3a
029300130118     c                   endif
029400121220     c                   endsr
029500130114     C**---------------------------------------------------------
029600130114     C**
029700130114     C**---------------------------------------------------------
029800130114     C     chiusura      BEGSR
029900130114     C*
030000130114     C                   CLEAR                   DSSI95
030100130114     C                   MOVEL     'C'           I95TLA
030200130114     C                   CALL      'TISI95R'
030300130114     C                   PARM                    DSSI95
030400130114
030500130114     C     WSF20         IFNE      *BLANKS
030600130114     C                   MOVEL     'C'           dtas_TASTLA
030700130206     C                   CALL      'TNSF20R'                            99
030800130114     C                   PARM                    KPJBA
030900130114     C                   PARM                    DTAS
031000130114     C                   PARM                    DTASV
031100130114     C                   ENDIF
031200130114
031300130114     c                   endsr
031400120322     C**---------------------------------------------------------
031500120322     C** Richiamo tisi95 per determinazione codice tassazione
031600120322     C**---------------------------------------------------------
031700120322     C     C_tisi95      BEGSR
031800120322     C                   CLEAR                   DSSI95
031900130118     C                   MOVEL     dsarbd.ARBCAD I95CAP
032000130118     C                   MOVEL     dsarbd.ARBNZD I95NAR
032100130118     C                   MOVEL     dsarbd.ARBLOD I95LOC
032200130118     C                   MOVEL     dsarbd.ARBPRD I95PRV
032300121116     c                   movel     tasaas        i95dat
032400121116     c                   move      tasmgs        i95dat
032500120322     C                   MOVEL     '7'           I95TCN
032600120322      * Se network FedEx passo flag apposito per dire di prendere i dati
032700120322      * dalla tabella della nazione
032800120322     c     taslna        chain     azorg01l
032900120322     c                   if        %found(azorg01l) and orgfva = *blanks
033000120322     c                   eval      og143 = orgde3
033100120322     c     §ogntw        ifeq      'FED'
033200120322     c                   eval      i95fi1 = 'S'
033300120322     c                   endif
033400120322     c                   endif
033500120322
033600120322     C                   MOVEL     §3ATB1        I95TPO
033700120322    5C     §3AFCA        IFNE      0
033800120322     C                   MOVEL     'S'           I95FCA
033900120322    5C                   ENDIF
034000120322     C**
034100120322     C                   CALL      'TISI95R'
034200120322     C                   PARM                    DSSI95
034300120322     c                   endsr
034400941130     C**---------------------------------------------------------
034500941130     C* ROUTINE INIZIALE
034600941130     C**---------------------------------------------------------
034700941130     C     *INZSR        BEGSR
034800021014
034900021014
035000941130     C                   Z-ADD     1             CODUT             1 0
035100021014
035200021014     c     *dtaara       define    §azute        azuteds
035300021014     c     *dtaara       define    §datiute      ddatiute
035400021014     C                   in(E)     *dtaara
035500021014     C                   IF        %Error  or  RSUT = *blanks
035600021014     C                   CLEAR                   TIBS34DS
035700021014     C                   call      'TIBS34R'
035800021014     C                   parm                    Tibs34Ds
035900021014     C                   in        *dtaara
036000021014     c                   ENDIF
036100021014
036200021014     C                   Z-ADD     DUTKCI        KCI               4 0
036300021014
036400121218     C     KTAB          KLIST
036500121218     C                   KFLD                    CODUT
036600121218     C                   KFLD                    COD               2
036700121218     C                   KFLD                    KEY               8
036800130118     c     kbol          klist
036900130118     c                   kfld                    iv1aas
037000130118     c                   kfld                    iv1lnp
037100130118     c                   kfld                    iv1nrs
037200130118     c                   kfld                    iv1nsp
037300130118     c                   kfld                    iv1tbl
037400140128     c     kbols         klist
037500140128     c                   kfld                    iv1aas
037600140128     c                   kfld                    iv1lnp
037700140128     c                   kfld                    iv1nrs
037800140128     c                   kfld                    iv1nsp
037900121218     c     ktai06        klist
038000121218     c                   kfld                    tasaas
038100121218     c                   kfld                    taslnp
038200121218     c                   kfld                    tasnrs
038300121218     c                   kfld                    tasnsp
038400121218     c                   kfld                    tastbl
038500121218     c                   kfld                    Ksvt
038600121218      * Chiave per setll su TIVRB01l
038700121218     c     Kvrb          Klist
038800121218     c                   kfld                    tasaas
038900121218     c                   kfld                    taslnp
039000121218     c                   kfld                    tasnrs
039100121218     c                   kfld                    tasnsp
039200121218     c                   kfld                    tastbl
039300121218     c                   kfld                    wcvb
039400941130     C     KANN          KLIST
039500130118     C                   KFLD                    tasAAS
039600130118     C                   KFLD                    tasLNP
039700130118     C                   KFLD                    tasNRS
039800130118     C                   KFLD                    tasNSP
039900120424     c*
040000120424     c     ktai05        klist
040100120424     c                   kfld                    tasaas
040200120424     c                   kfld                    taslnp
040300120424     c                   kfld                    tasnrs
040400120424     c                   kfld                    tasnsp
040500120424     c                   kfld                    tastbl
040600120313
040700120319     c     kFiar5        Klist
040800120319     c                   Kfld                    TasAas
040900120319     c                   Kfld                    TasLnp
041000120319     c                   Kfld                    TasNrs
041100120319     c                   Kfld                    TasNsp
041200120319     c                   Kfld                    kAr5Trd
041300120319      * Chiave file eventi FNEVB
041400120319     c     Kevb4         Klist
041500120319     c                   Kfld                    Tasaas
041600120319     c                   Kfld                    Taslnp
041700120319     c                   Kfld                    Tasnrs
041800120319     c                   Kfld                    Tasnsp
041900120319     c                   Kfld                    Wcev
042000130204
042100130204     c     ktbe          klist
042200130204     c                   kfld                    kcod
042300130204     c                   kfld                    kke1
042400071102
042500941130     C                   ENDSR
042600930804     C*
042700121129     C**-------------------------------------------------------------
042800121129     C** Gestione della bolla per verificare se da scrivere
042900121129     c*  nel file per la creazione della bolla di recupero (TIVRB)
043000121129     C**-------------------------------------------------------------
043100121129     C     ges_vrb       BEGSR
043200121129     c* Verifico se bolla già presente su file delle variazioni
043300121130     c                   eval      wcvb='VA'
043400121203     c     kvrb          chain(N)  tivrb01l
043500121130     c* BOLLA NON PRESENTE: tasso e memorizzo solo se delta tassazione > 0
043600121203    1c                   if        not %found(tivrb01l)
043700121130     c                   exsr      sr_tassa1
043800121129    2c                   if        dtas_taserr=*blanks
043900121129     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
044000121129    3c                   if        w_vrbdimp>0
044100121129     c* recupero la data penultima fatturazione mensile per vedere se
044200121129     c* la variazione è da scrivere nel file di lavoro
044300121129     c                   exsr      rec_RPFdata
044400170411     c***                if        tasdft>§RPFDTA and §rpfdta>0
044500170411     c                   if        tasdft>wdataRPF and wdataRPF>0
044600121129     c                   exsr      wrtvrb
044700121129     c                   endif
044800121129    3c                   endif
044900121129    2c                   endif
045000121130   x1c                   else
045100121130     c* BOLLA PRESENTE: tasso e verifico se aggiornare il file delle variazioni
045200121130     c* recuperare
045300121130     c                   exsr      sr_tassa2
045400121130    2c                   if        dtas_taserr=*blanks
045500121130     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
045600121130    3c                   select
045700121130     c* DELTA TASSAZIONE >0 : Aggiorno record "VA"
045800121203     c                   when      W_vrbdimp>0  and w_vrbdimp>vrbdimp
045900121130     c* recupero la data penultima fatturazione mensile per vedere se
046000121130     c* la variazione deve aggiornare il file di lavoro
046100121130     c                   exsr      rec_RPFdata
046200170411     c****               if        tasdft>§RPFDTA and §rpfdta>0
046300170411     c                   if        tasdft>wdataRPF and wdataRPF>0
046400121130     c                   exsr      aggvrb
046500121130     c                   endif
046600121130     c                   other
046700121130     c* DELTA TASSAZIONE <=0: Verifico se differenze da aggiornare
046800121130     c* mi salvo il record "VA" prima di chainare il record "  "
046900121130     c                   eval      dsva=dsvrb
047000121130     c                   clear                   wcvb
047100121130     c                   clear                   wdiffe
047200121203     c     kvrb          chain(N)  tivrb01l
047300121130     c                   exsr      ver_diffe
047400121130    4c                   if        wdiffe='1'
047500121130     c* rieseguo chain del record "VA" per aggiornarlo
047600121130     c                   eval      wcvb='VA'
047700121203     c     kvrb          chain     tivrb01l
047800121203   4ac                   if        %found(tivrb01l)
047900121130     c* recupero la data penultima fatturazione mensile per vedere se
048000121130     c* la variazione deve aggiornare il file di lavoro
048100121130     c                   exsr      rec_RPFdata
048200170411    5c****               if        tasdft>§RPFDTA and §rpfdta>0
048300170411     c                   if        tasdft>wdataRPF and wdataRPF>0
048400121130     c                   eval      dsvrb=dsva
048500121130     c                   time                    vrbtmst
048600121130     c                   eval      vrbdimp=w_vrbdimp
048700121130     c                   update    tivrb000
048800121130    5c                   endif
048900121130   4ac                   endif
049000121130    4c                   endif
049100121130    3c                   endsl
049200121130    2c                   endif
049300121129    1c                   endif
049400121130     C                   MOVEL     '1'           WSF20
049500121129     c                   endsr
049600121130     C**-------------------------------------------------------------
049700121130     C** Tassazione - I volta
049800121130     C**-------------------------------------------------------------
049900121130     C     sr_tassa1     BEGSR
050000121130     c* campi della DTAS da prendere da titas
050100121130     c                   exsr      riedtasUNI
050200121130     c                   exsr      riedtas
050300121130     c* campi della dtas da impostare con campi della variazione in ricezione
050400121130     c                   exsr      riedtasvar
050500121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
050600121130     c                   exsr      riedtasALTRI
050700121130      * C/Assegno
050800121130    3c                   IF        Tasfca <> *blanks
050900130116     c                             and %subst(iv1cvb:1:1)<>'K'
051000121130     c                   Exsr      Carcsb
051100121130    3c                   Endif
051200121130      * recupero i valori  già tassati
051300121130     c                   exsr      Ric_tai
051400150330
051500150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
051600150330     c                   if        §3asva<>*blanks
051700150330     c                   eval      dtas_tassva=§3asva
051800150330     c                   endif
051900121130
052000130206     C                   CALL      'TNSF20R'
052100121130     C                   PARM                    KPJBA
052200121130     C                   PARM                    DTAS
052300121130     C                   PARM                    DTASV
052400121130     c                   endsr
052500121130     C**-------------------------------------------------------------
052600121130     C** Tassazione - Bolla già presente nelle variazioni da recuperare
052700121130     C**-------------------------------------------------------------
052800121130     C     sr_tassa2     BEGSR
052900121130     c* campi della dtas da prendere da titas
053000121130     c                   exsr      riedtasUNI
053100121130     c* campi della dtas da prendere dal record "VA" di TIVRB
053200121130     c                   exsr      riedtasVRB
053300121130     c* campi della dtas da impostare con campi della variazione in ricezione
053400121130     c                   exsr      riedtasVAR
053500121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
053600121130     c                   exsr      riedtasALTRI
053700121130      * recupero i valori  già tassati
053800121130     c                   exsr      Ric_tai
053900150330
054000150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
054100150330     c                   if        §3asva<>*blanks
054200150330     c                   eval      dtas_tassva=§3asva
054300150330     c                   endif
054400121130
054500130206     C                   CALL      'TNSF20R'
054600121130     C                   PARM                    KPJBA
054700121130     C                   PARM                    DTAS
054800121130     C                   PARM                    DTASV
054900121130     c                   endsr
055000121130      *****************************************************************
055100121130      * Impostazione campi della dtas da campi di titas
055200121130      *****************************************************************
055300121130     c     RiedtasUNI    Begsr
055400121130     c                   clear                   dtas
055500121130     c                   clear                   dtasv
055600121130     c                   eval      dtas_TASKSC=tasksc
055700121130     c                   eval      dtas_TASNCL=tasncl
055800160530     c                   eval      dtas_TASNCLB=tasncl
055900121130     c                   eval      dtas_TASCTR=tasctr
056000121130     c                   eval      dtas_TASFDN=tasfdn
056100121130     c                   eval      dtas_TASMCT=tasmct
056200121130     c                   eval      dtas_TASNZM=tasnzm
056300121130     c                   eval      dtas_TASCAM=tascam
056400121130     c                   eval      dtas_TASFAP=tasfap
056500121130     c                   eval      dtas_TASTSP=tastsp
056600121130     c                   eval      dtas_taslna=taslna
056700121130     c                   eval      dtas_TASDIV=tasdiv
056800151012     c* tnsf20r usa taspvl come campo di ouptut quindi asterisco
056900151012     c***                eval      dtas_TASPVL=taspvl
057000160531     c* Peso Bollettatto (DTAS_taspkb): non lo valorizzo in quanto
057100160531     c* serve per la tassazioe con peso desunto (TNSF22R)
057200160531     c* e in filiale non possono variare TASPKB
057300160531     c* Se fosse possibile variarlo bisognerebbe probabilmente aggiungere questo
057400160531     c* campo nel TIVRB
057500121130     c                   endsr
057600121130      *****************************************************************
057700121130      * Impostazione campi della dtas da campi di tivrb
057800121130      *****************************************************************
057900121130     c     RiedtasVRB    Begsr
058000121130     c                   eval      dtas_taspkf=VRBPKF
058100121130     c                   eval      dtas_taspkc=vRBPKC
058200121130     c                   eval      dtas_tasncp=VRBNCP
058300121130     c                   eval      dtas_tasvlf=VRBVLF
058400160531     c                   eval      dtas_tasfvf=VRBfvf
058500160531     c                   eval      dtas_tasvlc=vRBvlc
058600160531     c                   eval      dtas_tasncr=VRBNcr
058700121130     c                   eval      dtas_tascts=VRBCTS
058800121130     c                   eval      dtas_tasnzd=VRBNZD
058900121130     c                   eval      dtas_tascad=VRBCAD
059000121130     c                   eval      dtas_tasfin=VRBFIN
059100121130     c                   eval      dtas_tastc1=VRBTC1
059200121130     c                   eval      dtas_tastc2=VRBTC2
059300121130     c                   eval      dtas_taslnp=vrblnp
059400121130     c                   eval      dtas_TASTBL=vrbtbl
059500121130     c                   eval      dtas_tasvas=VRBVAS
059600121130     c                   eval      dtas_tasias=VRBIAS
059700121130     c                   eval      dtas_tasqft=VRBQFT
059800121130     c                   eval      dtas_tastic=VRBTIC
059900121130     c                   eval      dtas_tascas=VRBCAS
060000121130     c                   eval      dtas_tasvca=VRBVCA
060100121130     c                   eval      dtas_tascmb=VRBCMB
060200121130     c                   move      VRBSTA        dtas_tassta
060300121130     c                   endsr
060400121130      *****************************************************************
060500121130      * Campi della DTAS da impostare con la variazione in ricezione
060600121130      *****************************************************************
060700121130     c     RiedtasVAR    Begsr
060800121130    2c                   select
060900121130     c* Variazioni Destinatario/peso/volume
061000130116     c                   when      %subst(iv1cvb:1:1)='I'
061100160426     c* considero peso/vol ricevuti solo se bolla non tassata con peso desunto
061200160426     c                   if        §ar5fptas<>'D' and §AR5FVTAS<>'D'
061300130118     c                   z-add     dsarbd.arbpkf dtas_taspkf
061400160426     c                   z-add     dsarbd.arbvlf dtas_tasvlf
061500160531     c                   movel     dsarbd.arbfvf dtas_tasfvf
061600151007     c                   endif
061700160426     c***                z-add     dsarbd.arbvlf dtas_tasvlf
061800130118     c                   movel     dsarbd.arbfin dtas_tasfin
061900130118     c                   movel     dsarbd.arbcad dtas_tascad
062000130118     c                   movel     dsarbd.arbnzd dtas_tasnzd
062100130118    3C     iv1cvb        ifeq      'I0'
062200151008     c     iv1cvb        oreq      'I7'
062300151008     c     iv1cvb        oreq      'I8'
062400151008     c     iv1cvb        oreq      'I9'
062500121130     c                   exsr      c_tisi95
062600121130     c                   if        o95err = *blanks
062700121130     c                   eval      dtas_tascts=o95cts
062800121130     c                   endif
062900121130    3c                   endif
063000121130     c* Variazioni codice bolla/contrassegno
063100130116     c                   when      %subst(iv1cvb:1:1)='K'
063200121205     c* Contrassegno ANNULLATO: prendo i campi da tncsb
063300121205     c*                         e metto stato 9
063400130116    3c                   if        iv1can='S'
063500121205     c                   eval      dtas_tascas=dscsb.csbcas
063600121205     c                   eval      dtas_tasvca=dscsb.csbvca
063700121205     c                   eval      dtas_tascmb=dscsb.csbcmb
063800121205    4c                   if        dscsb.csbfus<>' '
063900121210     c****               eval      dtas_tastic=dscsb.csbfus
064000121210     c                   move      dscsb.csbfus  dtas_tastic
064100121205   x4c                   else
064200121210     c****               eval      dtas_tastic=dscsb.csbtpi
064300121210     c                   move      dscsb.csbtpi  dtas_tastic
064400121205    4c                   endif
064500121205     c                   eval      dtas_Tassta='9'
064600121205   x3c                   else
064700121205     c* Contrassegno NON Annullato: prendo i campi dalla variazione
064800130118     c                   eval      dtas_tascas=dsarbk.arbcas
064900130118     c                   eval      dtas_tasvca=dsarbk.arbvca
065000130118     c                   eval      dtas_tascmb=dsarbk.arbcmb
065100121205     c                   clear                   dtas_tastic
065200121205     C                   MOVEL     '1A'          COD
065300130118     C                   MOVEL(P)  dsarbk.arbtic KEY
065400121205     C     KTAB          CHAIN     TABEL
065500121205    4c                   if        %found(tabel00F)
065600121205     C                   MOVEL     TBLUNI        DS1A
065700121205     c                   move      §1afmb        dtas_tastic
065800121205    4c                   endif
065900121205     c* .......... flag stato: metto 0 o 3 in base a quello che c'è su TNCSB
066000121205    4c                   if        dscsb.csbsta<>3
066100121205     c                   eval      dtas_Tassta='0'
066200121205     c                   else
066300121205     c                   eval      dtas_tassta='3'
066400121205    4c                   endif
066500121205    3c                   endif
066600121130     c* Variazioni Consegne particolari
066700130116     c                   when      %subst(iv1cvb:1:1)='C'
066800130118     c                   movel     dsarbg.arbtc1 dtas_tastc1
066900130118     c                   movel     dsarbg.arbtc2 dtas_tastc2
067000121130     c* Variazione Importo da assicurare
067100130116     c                   when      %subst(iv1cvb:1:1)='T'
067200130118     c                   movel     dsarbt.arbias dtas_tasias
067300130118     c                   movel     dsarbt.arbvas dtas_tasvas
067400130116     c* Variazione Volume VDL
067500130116     c                   when      iv1cvb='VV'
067600130118     c                   z-add     §bvvlf        dtas_tasvlf
067700160601     c                   movel     §bvfvf        dtas_tasfvf
067800160531     c                   z-add     §BVNCR        dtas_tasncr
067900160531     c                   z-add     §BVVLC        dtas_tasvlc
068000130122     c* Variazione Peso   VDL
068100130122     c                   when      iv1cvb='PV'
068200130122     c                   z-add     §bvpkf        dtas_taspkf
068300130122     c                   z-add     §bvpkc        dtas_taspkc
068400130122     c                   z-add     §bvncp        dtas_tasncp
068500130122     c* Variazione inoltro Z/T
068600130123     c                   when      iv1cvb='ZT'
068700130122     c                   eval      dtas_tasfin=parfin
068800121130    2c                   endsl
068900121130     c                   endsr
069000120424      *****************************************************************
069100121130      * Impostazione campi della dtas da campi di titas
069200120424      *****************************************************************
069300120424     c     Riedtas       Begsr
069400160531     c* non presente il record "FAT" --> tasso con peso e volume presi da titas
069500151008     c                   if        §AR5PKTAS=0
069600120424     c                   eval      dtas_TASPKF=taspkf
069700120424     c                   eval      dtas_TASPKC=taspkc
069800120424     c                   eval      dtas_TASNCP=tasncp
069900120424     c                   eval      dtas_TASVLF=tasvlf
070000160531     c                   eval      dtas_TASfvf=tasfvf
070100160531     c                   eval      dtas_TASvlc=tasvlc
070200160531     c                   eval      dtas_TASncr=tasncr
070300151008     c                   else
070400151008     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
070500151008     c                   eval      dtas_TASPKF=§ar5pktas
070600151008     c                   eval      dtas_TASVLF=§AR5VLTAS
070700160531     c                   eval      dtas_TASfvf=§AR5FVTAS
070800151008     c                   endif
070900120424     c                   eval      dtas_TASCTS=tascts
071000120424     c                   eval      dtas_TASNZD=tasnzd
071100120424     c                   eval      dtas_TASCAD=tascad
071200120424     c                   eval      dtas_TASFIN=tasfin
071300120424     c                   eval      dtas_TASTC1=tasftc
071400120424     c                   eval      dtas_TASTC2=tastc2
071500120424     c                   eval      dtas_taslnp=taslnp
071600121130     c                   eval      dtas_TASTBL =tastbl
071700120424     c                   eval      dtas_TASVAS =tasvas
071800120424     c                   eval      dtas_TASIAS =tasias
071900120424     c                   eval      dtas_TASQFT =tasqft
072000120424     c                   endsr
072100121130      *****************************************************************
072200121130      * Altri campi della dtas da prendere da titas
072300121130      *****************************************************************
072400121130     c     RiedtasAltri  Begsr
072500121130     c                   clear                   dAr5Ban
072600121130     c                   Clear                   dAr5Bnb
072700121130     c                   Clear                   kpjbu
072800121130     c                   movel     tasaas        dtas_tasdsp
072900121130     c                   move      tasmgs        dtas_tasdsp
073000121130     c                   eval      dtas_tasdct=dtas_tasdsp
073100121130
073200121130      * Flag SI NO DDT
073300121130    2C                   If        tasll1 = 'C' or tasll1 = 'S'
073400121130     C                   movel     'S'           dtas_Tasddt
073500121130   x2C                   Else
073600121130     C                   Clear                   dtas_Tasddt
073700121130    2C                   Endif
073800121130     c                   clear                   dtas01
073900121130     c                   movel     tasfbr        §asfbr
074000121130     c                   movel     tascca        §ascca
074100121130      * valorizzo flag email al destinatario
074200121130     c                   movel     tasflo        dtasflo
074300121130     c                   move      §floemd       §asemd
074400150108     c* flag pin code
074500150108     c                   if        tasgma<>*blanks
074600150108     c                   clear                   ds7r
074700150108     c                   movel     '7R'          cod
074800150108     c                   movel(p)  tasgma        key
074900150108     c     ktab          chain     tabel
075000150108    8c                   if        %found(tabel00f)
075100150108     c                   movel     tbluni        ds7r
075200150108    8c                   endif
075300150108     c                   if        §7rpincode='S'
075400150108     c                   movel     'S'           §aspin
075500150108     c                   endif
075600150108     c                   endif
075700121130
075800121130     c                   eval      dtas_tasflo = dtas01
075900160107     c* solo per prima bolla
076000160107     c                   if        §3atb2=*blanks or %subst(tastbl:1:1)
076100160107     c                             <>%subst(§3atb2:1:1)
076200160107     c
076300160107     c* flag DIRITTO DI CHIAMATA  da 10° byte di TASFLO di TITAS
076400160108     c                   move      §floado       dtas_tasprt
076500160107     c* flag per ADDEBITO PACKING LIST da fiar5 "GEN"
076600160107     c                   clear                   dar5gen
076700160107     c                   eval      kAr5Trd  = 'GEN'
076800160107     c     kFiar5        Chain     Fiar531c
076900160107If  3c                   If        %Found(Fiar531c)
077000160107     c                   Movel     Ar5Uni        dAr5gen
077100160107    3c                   EndIf
077200160107     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
077300160108     c                   Eval      dtas_TasSpl = 'S'
077400160107     c                   endif
077500160107     c                   endif
077600121130
077700121130     c
077800121130      * Bancali
077900121130    2c                   If        %Subst(TasGva:1:1) = 'B'
078000121130
078100121130     c                   eval      kAr5Trd  = 'BAN'
078200121130     c     kFiar5        Chain     Fiar531c
078300121130If  3c                   If        %Found(Fiar531c)
078400121130     c                   Movel     Ar5Uni        dAr5Ban
078500121130    3c                   EndIf
078600121130      * Bancali
078700121130     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
078800121130    2c                   EndIf
078900121130      **
079000121130      * Colli Originali
079100121130    2c                   If        %Subst(TasGva:1:1) = 'O'
079200121130     c                   eval      kAr5Trd  = 'BNB'
079300121130     c     kFiar5        Chain     Fiar531c
079400121130If  3c                   If        %Found(Fiar531c)
079500121130     c                   Movel     Ar5Uni        dAr5Bnb
079600121130    3c                   EndIf
079700121130      * Colli
079800121130     c                   Eval      dtas_TasNcl = §Ar5bcor
079900121130    2c                   EndIf
080000121130
080100121130      **** verifico se devo calcolare l'addebito di lasciato avviso
080200121130      **** verifico se c'è evento
080300140127      **** Se Variazione "RI" non ho bisogno di verificare la presenza dell'
080400140127      **** evento
080500140127     c                   if        iv1cvb='RI'
080600140127     c                   eval      dtas_tasric = 'S'
080700140127     c                   else
080800140127     c                   clear                   evbdtr
080900140127     c     Kevb4         setll     Fnevb04l
081000140127     c     Kevb4         reade     Fnevb04l
081100140127     c                   dow       not %eof(fnevb04l) and evbdtr=0
081200140127     c     Kevb4         reade     Fnevb04l
081300140127     c                   enddo
081400140127     c                   if        evbdtr>0
081500140127     c                   eval      dtas_tasric = 'S'
081600140127     c                   endif
081700140127     c                   endif
081800121130
081900121130      * Assicurazione
082000121130    2c                   IF        Tasfcm = 'F'
082100121130     c                   clear                   dtas_tasias
082200121130     c                   clear                   dtas_tasvas
082300121130    2c                   Endif
082400121130     c                   endsr
082500121130
082600120316      *****************************************************************
082700120316      *  Recupero importi di tassazione dal file titai
082800120316      *****************************************************************
082900120316     c     Ric_tai       Begsr
083000120316      *
083100120316     c                   z-add     0             k
083200120316
083300120316     c     ktai05        setll     titai01l
083400120316
083500120316     c                   If        %equal(titai01l)
083600120316
083700120316     c                   do        *hival
083800120316
083900120316     c     ktai05        reade     titai01l
084000120316
084100120316     c                   If        %eof(titai01l)
084200120316     c                   leave
084300120316     c                   endif
084400120316      * imponibile
084500120316     c                   if        taisvt = '£'
084600120424     c                   eval      dtas_tasimv = taivat
084700120316     c                   endif
084800120316      * porto
084900120316     c                   if        taisvt = ' '
085000120424     c                   eval      dtas_taspor = taivat
085100120316     c                   endif
085200120316      * varie
085300120316     c                   if        taisvt <> ' ' and taisvt <> '£'
085400120612     c* se presente varia N 88.888.888 devo prima tassarla
085500120612    4c                   if        taisvt='N' and taivat=88888888
085600120612     c                   eval      savdtas=dtas
085700120612     c                   eval      savdtasv=dtasv
085800120612     c                   exsr      tassan
085900120612     c                   eval      dtas=savdtas
086000120612     c                   eval      dtasv=savdtasv
086100120612     c                   add       1             k
086200120612     c                   eval      sv(k) ='N'
086300120613     c                   eval      va(k) =antepo
086400120612   x4c                   else
086500120316     c                   add       1             k
086600120316     c                   eval      sv(k) = taisvt
086700120316     c                   eval      va(k) = taivat
086800120612    4c                   endif
086900120316     c                   endif
087000120316
087100120316     c                   enddo
087200120316
087300120316     c                   else
087400120316      * se non è pretassata pulisco la divisa
087500120424     c                   clear                   dtas_tasdiv
087600120316     c                   endif
087700120424     c                   endsr
087800120612      ****************************************************
087900120612      ** Tassazione varia N 88888888 (Anteporto)
088000120612      ****************************************************
088100120612     c     tassan        Begsr
088200120613     C                   CLEAR                   DTAS_tasimv
088300120613     C                   CLEAR                   DTAS_taspor
088400120613     C                   CLEAR                   DTASV
088500120612     c                   clear                   antepo
088600120612      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
088700150108      * idem per flag Pin Code
088800160113      * e per  diritto di chiamata e packing list
088900120612     c                   eval      dtas01 = dtas_tasflo
089000120612     c                   clear                   §asemd
089100150108     c                   clear                   §aspin
089200120612     c                   eval      dtas_tasflo = dtas01
089300160113     c                   clear                   dtas_tasprt
089400160113     c                   clear                   dtas_tasspl
089500120612      *
089600130206     C                   CALL      'TNSF20R'
089700120612     C                   PARM                    KPJBA
089800120612     C                   PARM                    DTAS
089900120612     C                   PARM                    DTASV
090000120612      *
090100120612      * Tassazione andata buon fine
090200120613     C     dtas_taserr   IFEQ      *BLANKS
090300120613     c                   eval      antepo=dtas_tasimv
090400120612     C                   ENDIF
090500120612     c                   endsr
090600120316      ****************************************************
090700120316      ** Ricerca Contrassegno
090800120316      ****************************************************
090900120316     c     Carcsb        Begsr
091000120316
091100130118     c     kann          chain     Tncsb03l
091200120316     c                   If        %found(tncsb03l) and
091300120316      * solo per stesso tipo bolla
091400120316     c                             tastbl = csbtbl
091500120424     c                   movel     csbsta        dtas_Tassta
091600120424     c                   z-add     csbcas        dtas_Tascas
091700120316     c                   if        csbfus <> *blanks
091800120424     c                   move      csbfus        dtas_tastic
091900120316     c                   else
092000120424     c                   move      csbtpi        dtas_tastic
092100120316     c                   end
092200120316      * mittente
092300120424     c                   movel     csbvca        dtas_tasvca
092400120424     c                   z-add     csbcmb        dtas_tascmb
092500120316
092600120316     c                   endif
092700120316
092800120316     c                   endsr
092900120319      ****************************************************
093000120319      ** Scrittura work-file delle bolle già fatturate
093100120319      ****************************************************
093200120319     c     wrtvrb        Begsr
093300121130     c                   clear                   tivrb000
093400121130     c                   eval      vrbaas=tasaas
093500121130     c                   eval      vrblnp=taslnp
093600121130     c                   eval      vrbnrs=tasnrs
093700121130     c                   eval      vrbnsp=tasnsp
093800121130     c                   eval      vrbtbl=tastbl
093900121130     c                   time                    vrbtmst
094000121130     c                   eval      vrbdft=tasdft
094100151008     c* Presente record "FAT" --> memorizzo peso e volume di questo
094200151008     c                   if        §AR5PKTAS>0
094300151008     c                   eval      VRBPKF=§ar5pktas
094400151008     c                   eval      VRBfpf=§ar5fptas
094500151008     c                   eval      vrbvlf=§AR5VLTAS
094600151008     c                   eval      VRBFVF=§AR5FVTAS
094700151008     c* . . . se bolla tassata con peso desunto non memorizzo il peso VDL
094800151008     c                   if        §ar5fptas<>'D'
094900151008     c                   eval      VRBPKC=taspkc
095000151008     c                   eval      VRBNCP=tasncp
095100151008     c                   endif
095200160427     c* . . . se bolla tassata con vol. desunto non memorizzo il vol. VDL
095300160427     c                   if        §ar5fvtas<>'D'
095400160427     c                   eval      VRBvlC=tasvlc
095500160427     c                   eval      VRBNCr=tasncr
095600160427     c                   endif
095700151008     c                   else
095800151008     c* Non presente il record "FAT" --> peso e volume da titas
095900130115     c                   eval      VRBPKC=taspkc
096000130115     c                   eval      VRBNCP=tasncp
096100130115     c                   eval      VRBPKF=taspkf
096200130115     c                   eval      VRBFPF=tasfpf
096300130115     c                   eval      VRBVLF=tasvlf
096400130115     c                   eval      VRBFVF=tasfvf
096500160427     c                   eval      VRBVLC=tasvlc
096600160427     c                   eval      VRBNCR=tasncr
096700151008     c                   endif
096800130115     c                   eval      VRBQFT=tasqft
096900130115     c                   eval      VRBIAS=tasias
097000130115     c                   eval      VRBVAS=tasvas
097100130115     c                   eval      VRBCTS=tascts
097200130115     c                   eval      VRBFIN=tasfin
097300130115     c                   eval      VRBCAD=tascad
097400130115     c                   eval      VRBNZD=tasnzd
097500130115     c                   eval      VRBFFD=tasffd
097600130115     c                   eval      VRBTC1=tasftc
097700130115     c                   eval      VRBTC2=tastc2
097800130116    2c                   if        %subst(iv1cvb:1:1)<>'K'
097900121109     c                   eval      VRBTIC=dtas_tastic
098000121109     c                   eval      VRBCAS=dtas_tascas
098100121109     c                   eval      VRBVCA=dtas_tasvca
098200121109     c                   eval      VRBCMB=dtas_tascmb
098300121115     c                   move      dtas_tassta   vrbsta
098400121109   x2c                   else
098500121115    3c                   if        dscsb.csbtbl=tastbl
098600121115    4c                   if        dscsb.csbfus <> *blanks
098700121115     c                   move      dscsb.csbfus  vrbtic
098800121109   x4c                   else
098900121115     c                   move      dscsb.csbtpi  vrbtic
099000121109    4c                   end
099100121115     c                   eval      VRBCAS=dscsb.csbcas
099200121115     c                   eval      VRBVCA=dscsb.csbvca
099300121115     c                   eval      VRBCMB=dscsb.csbcmb
099400121115     c                   eval      VRBsta=dscsb.csbsta
099500121109     c                   endif
099600121109    2c                   endif
099700140123    2c                   if        iv1cvb<>'RI'
099800140123     c                   eval      vrbric=dtas_tasric
099900140123    2c                   endif
100000121108     c                   write     tivrb000
100100121129     c* SCRITTURA RECORD "VA" (Prima volta)
100200121129     c                   eval      vrbcvb='VA'
100300130118     c                   exsr      RieVrbUni
100400121129     c                   write     tivrb000
100500121129     c*
100600120319     c                   endsr
100700121130      ****************************************************
100800121130      ** Aggiornamento record "VA" del file delle variazioni
100900121130      ** da recuperare per delta tassazione >0
101000121130      ****************************************************
101100121130     c     aggvrb        Begsr
101200121130     c                   eval      wcvb='VA'
101300121203     c     kvrb          chain     tivrb01l
101400121203     c                   if        %found(tivrb01l)
101500130118     c                   exsr      RieVrbUni
101600121130     c                   update    tivrb000
101700121130     c                   endif
101800121130     c                   endsr
101900130118      ****************************************************
102000130118      ** Riempimento campi vrb per l'aggiornamento/write
102100130118      ** Record "VA"
102200130118      ****************************************************
102300130118     c     RieVrbUni     Begsr
102400130118     c                   time                    vrbtmst
102500130118     c                   eval      vrbdimp=w_vrbdimp
102600130118    3c                   select
102700170412     c* Distinguo peso/volume da destinatario perchè
102800170412     c* a causa della scelta in fatturazione del peso da tassare (vdl o
102900170412     c* bollettato/variato),memorizzare nel record "VA" il peso ricevuto per
103000170412     c* una variazione "I0" potrebbe generare una segnalazione di peso variato
103100170412     c* errata in quanto il peso non è stato variato ma la tassazione è avvenuta
103200170412     c* con il peso VDL
103300170412     c                   when      iv1cvb ='I0' or
103400170412     c                             iv1cvb ='I7' or
103500170412     c                             iv1cvb ='I8' or
103600170412     c                             iv1cvb ='I9'
103700170412     c                   movel     dtas_tasfin   vrbfin
103800170412     c                   movel     dtas_tascad   vrbcad
103900170412     c                   movel     dtas_tasnzd   vrbnzd
104000170412     c                   movel     dtas_tascts   vrbcts
104100130118     c                   when      %subst(iv1cvb:1:1)='I'
104200130118     c                   z-add     dtas_taspkf   vrbpkf
104300130118     c                   z-add     dtas_tasvlf   vrbvlf
104400160531     c                   movel     dtas_tasfvf   vrbfvf
104500170412     c****               movel     dtas_tasfin   vrbfin
104600170412     c****               movel     dtas_tascad   vrbcad
104700170412     c****               movel     dtas_tasnzd   vrbnzd
104800170412     c***                movel     dtas_tascts   vrbcts
104900130118     c* Variazioni codice bolla/contrassegno
105000130118     c                   when      %subst(iv1cvb:1:1)='K'
105100130118     c                   eval      vrbcas=dtas_tascas
105200130118     c                   eval      vrbvca=dtas_tasvca
105300130118     c                   eval      vrbtic=dtas_tastic
105400130118     c                   eval      vrbcmb=dtas_tascmb
105500130118     c                   move      dtas_tassta   vrbsta
105600130118     c* Variazioni Consegne particolari
105700130118     c                   when      %subst(iv1cvb:1:1)='C'
105800130118     c                   movel     dtas_tastc1   vrbtc1
105900130118     c                   movel     dtas_tastc2   vrbtc2
106000130118     c* Variazione Importo da assicurare
106100130118     c                   when      %subst(iv1cvb:1:1)='T'
106200130118     c                   movel     dtas_tasias   vrbias
106300130118     c                   movel     dtas_tasvas   vrbvas
106400130118     c* Variazione Volume VDL
106500130118     c                   when      iv1cvb='VV'
106600130118     c                   z-add     dtas_tasvlf   vrbvlf
106700130118     c                   move      §bvfvf        vrbfvf
106800130118     c                   z-add     §bvncr        vrbncr
106900130118     c                   z-add     §bvvlc        vrbvlc
107000130122     c* Variazione Peso   VDL
107100130122     c                   when      iv1cvb='PV'
107200130122     c                   z-add     dtas_taspkf   vrbpkf
107300130122     c                   move      §bvfpf        vrbfpf
107400130122     c                   z-add     §bvncp        vrbncp
107500130122     c                   z-add     §bvpkc        vrbpkc
107600130122     c* Variazione Inoltro Z/T
107700130123     c                   when      iv1cvb='ZT'
107800140123     c                   movel     dtas_tasfin   vrbfin
107900140123     c* Evento "RIC"
108000140123     c                   when      iv1cvb='RI'
108100140123     c                   movel     dtas_tasric   vrbric
108200130118    3c                   endsl
108300130118     c                   endsr
108400121130      ****************************************************
108500121130      ** Verifica differenze fra record "VA" e immagine precedente
108600121130      ****************************************************
108700121130     c     ver_diffe     Begsr
108800121130    1c                   select
108900121130     c* Variazioni Destinatario/peso/volume
109000130116     c                   when      %subst(iv1cvb:1:1)='I'
109100121130     c* . . . . Peso e volume da fatturare
109200151008     c*          N.B:Se tassato con peso desunto il peso (VRBPKF) del record " "
109300151008      *              e del record "VA" è sempre lo stesso e devo anche ingnorare
109400151008      *              il peso ricevuto nel record di variazione
109500160531
109600160531      *          IL TEST SU §AR5FPTAS<>'D' NON SERVE PIU' DA QUANDO SONO STATE
109700160531      *          AGGIUNTE LE VARIAZIONI PER IL VOLUME DESUNTO PER CUI NON
109800160531      *          RECEPIAMO PIÙ VARIAZIONI DI PESO/VOLUME SE UNO DEI DUE HA IL DE
109900160531      *          SUNTO. QUINDI IN QUESTI CASI LA PRIMA IF SEGUENTE NON SARÀ MAI
110000160531      *          SODDISFATTA
110100151008     c*
110200121130    2c                   if        vrbpkf<>dsva.vrbpkf or
110300121130     c                             vrbvlf<>dsva.vrbvlf
110400151008     C                   if        (§AR5FPTAS<>'D' and dsarbd.arbpkf<vrbpkf)
110500151008     C                             or dsarbd.arbvlf<vrbvlf
110600121130     c                   eval      dsva.vrbpkf=vrbpkf
110700121130     c                   eval      dsva.vrbvlf=vrbvlf
110800160531     c                   eval      dsva.vrbfvf=vrbfvf
110900121130     c                   else
111000151008     c                   if        §AR5FPTAS<>'D'
111100151008     c                   eval      dsva.vrbpkf=dsarbd.arbpkf
111200151008     c                   endif
111300130118     c                   eval      dsva.vrbvlf=dsarbd.arbvlf
111400160531     c                   eval      dsva.vrbfvf=dsarbd.arbfvf
111500121130     c                   endif
111600121130     c                   eval      wdiffe='1'
111700121130    2c                   endif
111800121130     c* . . . . Flag inoltro CAP e Nazione e Codice Tassazione
111900121130    2c                   if        vrbfin<>dsva.vrbfin or
112000121130     c                             vrbcad<>dsva.vrbcad or
112100121130     c                             vrbnzd<>dsva.vrbnzd or
112200121130     c                             vrbcts<>dsva.vrbcts
112300130118     c                   eval      dsva.vrbfin=dsarbd.arbfin
112400130118     c                   eval      dsva.vrbcad=dsarbd.arbcad
112500130118     c                   eval      dsva.vrbnzd=dsarbd.arbnzd
112600121130     c                   eval      dsva.vrbcts=dtas_tascts
112700121130     c                   eval      wdiffe='1'
112800121130    2c                   endif
112900121130     c* Variazioni codice bolla/contrassegno
113000130116     c                   when      %subst(iv1cvb:1:1)='K'
113100121203    2c                   if        vrbtic<>dsva.vrbtic or
113200121203     c                             vrbcas<>dsva.vrbcas or
113300121203     c                             vrbvca<>dsva.vrbvca or
113400121203     c                             vrbcmb<>dsva.vrbcmb or
113500121203     c                             vrbsta<>dsva.vrbsta
113600121203     c* stessa divisa o non c'era il contrassegno
113700121205    3c                   if        dtas_tasvca=vrbvca or vrbvca=*blanks
113800121130     c* se importo più basso del valore iniziale --> tengo l'iniziale
113900121205    4c                   if        (dtas_tassta='9' and vrbsta<>9) or
114000121205     c                             (dtas_tascas<vrbcas and
114100121205     c                             ((dtas_tassta='9' and vrbsta=9) or
114200121205     c                              (dtas_tassta<>'9' and vrbsta<>9)))
114300121130     c                   z-add     vrbcas        dsva.vrbcas
114400121130     c                   eval      dsva.vrbtic=vrbtic
114500121130     c                   eval      dsva.vrbvca=vrbvca
114600121130     c                   eval      dsva.vrbcmb=vrbcmb
114700121130     c                   eval      dsva.vrbsta=vrbsta
114800121130   x4c                   else
114900121130     c* se importo maggiore o uguale tengo la variazione
115000121205     c                   z-add     dtas_tascas   dsva.vrbcas
115100121205     c                   eval      dsva.vrbtic=dtas_tastic
115200121205     c                   eval      dsva.vrbvca=dtas_tasvca
115300121205     c                   eval      dsva.vrbcmb=dtas_tascmb
115400121205     c                   move      dtas_tassta   dsva.vrbsta
115500121130    4c                   endif
115600121130   x3c                   else
115700121130     c* divisa diversa imposto sempre come l'originale
115800121130     c                   z-add     vrbcas        dsva.vrbcas
115900121130     c                   eval      dsva.vrbtic=vrbtic
116000121130     c                   eval      dsva.vrbvca=vrbvca
116100121130     c                   eval      dsva.vrbcmb=vrbcmb
116200121130     c                   eval      dsva.vrbsta=vrbsta
116300121130    3c                   endif
116400121130     c                   eval      wdiffe='1'
116500121130    2c                   endif
116600121130     c* Variazioni Consegne particolari
116700130116     c                   when      %subst(iv1cvb:1:1)='C'
116800121130    2c                   if        (vrbtc1=dsva.vrbtc1 and vrbtc2=dsva.vrbtc2)
116900121130     c                              or
117000121130     c                             (vrbtc1=dsva.vrbtc2 and vrbtc2=dsva.vrbtc1)
117100121130   x2c                   else
117200130118     c                   eval      dsva.vrbtc1=dsarbg.arbtc1
117300130118     c                   eval      dsva.vrbtc2=dsarbg.arbtc2
117400121130     c                   eval      wdiffe='1'
117500121130    2c                   endif
117600121130     c* Variazione Importo da assicurare
117700130116     c                   when      %subst(iv1cvb:1:1)='T'
117800121203    2c                   if        vrbias<>dsva.vrbias or
117900121203     c                             vrbvas<>dsva.vrbvas
118000121203     c* stessa divisa o non c'era importo da assicurare
118100130124    3c                   if        dsarbt.arbvas=vrbvas or vrbvas=*blanks
118200130118    4c                   if        dsarbt.arbias<vrbias
118300121130     c                   eval      dsva.vrbias=vrbias
118400121130     c                   eval      dsva.vrbvas=vrbvas
118500121130   x4c                   else
118600130118     c                   eval      dsva.vrbias=dsarbt.arbias
118700130118     c                   eval      dsva.vrbvas=dsarbt.arbvas
118800121130    4c                   endif
118900121130     c* divisa diversa imposto sempre come record originale
119000121130   x3c                   else
119100121130     c                   eval      dsva.vrbias=vrbias
119200121130     c                   eval      dsva.vrbvas=vrbvas
119300121130    3c                   endif
119400121130     c                   eval      wdiffe='1'
119500121130    2c                   endif
119600130117     c* Variazioni Volume VDL
119700130117     c                   when      iv1cvb='VV'
119800130117     c                   if        vrbvlf<>dsva.vrbvlf
119900130118     C                   IF        §bvvlf<vrbvlf
120000130117     c                   eval      dsva.vrbvlf=vrbvlf
120100130118     c                   eval      dsva.vrbfvf=vrbfvf
120200130118     c                   eval      dsva.vrbvlc=vrbvlc
120300130118     c                   eval      dsva.vrbncr=vrbncr
120400130117     c                   else
120500130118     c                   eval      dsva.vrbvlf=§bvvlf
120600130118     c                   eval      dsva.vrbfvf=§bvfvf
120700130118     c                   eval      dsva.vrbvlc=§bvvlc
120800130118     c                   eval      dsva.vrbncr=§bvncr
120900130117     c                   endif
121000130117     c                   eval      wdiffe='1'
121100130117    2c                   endif
121200130122     c* Variazioni Peso   VDL
121300130122     c                   when      iv1cvb='PV'
121400130122     c                   if        vrbpkf<>dsva.vrbpkf  or
121500130122     c                             vrbpkc<>dsva.vrbpkc  or
121600130122     c                             vrbncp<>dsva.vrbncp
121700130122     C                   IF        §bvpkf<vrbpkf or
121800130122     C                             §bvpkc<vrbpkc or
121900130122     C                             §bvncp<vrbncp
122000130122     c                   eval      dsva.vrbpkf=vrbvlf
122100130122     c                   eval      dsva.vrbfpf=vrbfpf
122200130122     c                   eval      dsva.vrbpkc=vrbpkc
122300130122     c                   eval      dsva.vrbncp=vrbncp
122400130122     c                   else
122500130122     c                   eval      dsva.vrbpkf=§bvpkf
122600130122     c                   eval      dsva.vrbfpf=§bvfpf
122700130122     c                   eval      dsva.vrbpkc=§bvpkc
122800130122     c                   eval      dsva.vrbncp=§bvncp
122900130122     c                   endif
123000130122     c                   eval      wdiffe='1'
123100130122    2c                   endif
123200130122     c* Variazioni Inoltro Z/T
123300130123     c                   when      iv1cvb='ZT'
123400130122     c                   if        vrbfin<>dsva.vrbfin
123500130122     c                   eval      dsva.vrbfin=parfin
123600130206     c                   eval      wdiffe='1'
123700130122     c                   endif
123800140123     c* Variazioni evento RIC
123900140123     c                   when      iv1cvb='RI'
124000140123     c                   if        vrbric<>dsva.vrbric
124100140123     c                   eval      dsva.vrbric=parric
124200140123     c                   eval      wdiffe='1'
124300140123     c                   endif
124400121130    1c                   endsl
124500121130     c                   endsr
124600121114      ****************************************************
124700121114      ** Recupero data penultima fatturazione mensile
124800121114      ****************************************************
124900121114     c     rec_RPFdata   Begsr
125000170411     c                   clear                   wdataRPF
125100121114     c                   Clear                   Drpfd
125200130204     c                   Eval      KCod = 'RPF'
125300130204     c                   Movel(p)  'DATA'        KKe1
125400130204     c     ktbe          chain     tntbe01l
125500130204     c                   if        %found(tntbe01l)
125600130204     c                   Movel     Tbeuni        Drpfd
125700130204     c                   endif
125800170411     c* verifico se sped. export DpD
125900170411    1c                   if        §rpfdta>0
126000170411     c                   clear                   og143
126100170411     c     taslna        chain     azorg01l
126200170411    2c                   if        %found(azorg01l) and orgfva = *blanks
126300170411     c                   eval      og143 = orgde3
126400170411    2c                   endif
126500170411    2c     §ogntw        ifeq      'DPD'
126600170411     c* calcolo data di riferimento per bolle export dpd:
126700170411     c* data penultima fatturazione - 1 mese
126800170411       //   Decremento la data di un mese
126900170411             dataiso=%date(§rpfdta);
127000170411             dataiso=dataiso-%months(1);
127100170411             wdataRPF=%dec(dataiso);
127200170411     c                   else
127300170411     c                   eval      wdataRPF=§rpfdta
127400170411    2c                   endif
127500170411    1c                   endif
127600121114     c                   endsr
127700981001     C*
