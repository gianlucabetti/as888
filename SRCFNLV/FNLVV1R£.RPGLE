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
009701170411
009702170411     d Dataiso         s               d   datfmt(*iso)
009703170411     d wdataRPF        s              8  0
009800941130     I*
009900920113     I/SPACE 3
010000920113     C****************************************************************
010100920113     C*  RIEPILOGO INDICATORI
010200920113     C***************************************************************
010300121218     C* 01    -
010400920113     C*****************************************************************
010500920113     C*
010600121219     C     *ENTRY        PLIST
010700121219     C                   PARM                    KPJBA
010800121220     c                   parm                    Parvar
010900121219     c                   parm                    partitas
011000121219     c                   parm                    parcsb
011100121219
011200121219     c                   movel     kpjbu         fnlvv1ds
011300130204
011400130114
011500130117    0c                   if        iv1tla<>'C'
011600130222
011700130222     c                   exsr      openfile
011800121220     c* Impostazione
011900121220     c                   exsr      impo
012000121218     c* 0 - Proseguo solo se la bolla di sede non è un C/servizio
012100130117     c*     e se è già stata contabilizzata
012200140128     c*     e se trovato record su titas se era da chainare
012300130117    1c                   if        §tbfcb='1' and tasfic<>*blanks
012400140128     c                             and wnotf=' '
012500150330     c                             and §tbrbl='N' and taslnp<>102
012600150330     c                             and (§3asva=*blanks or §3aim0<>'S')
012700130114      * 1 - bolla fatturata in filiale --> scarto
012800130117    2c                   if        tasfiv>0 and tasfiv <> 998 and tasfiv <> 996
012900130117   x2c                   else
013000130114      * 2 - bolla tassata fino all'imponibile  --> scarto
013100121218     c                   eval      ksvt = '£'
013200121218     c     Ktai06        setll     titai01l
013300121218    3c                   If        %equal(titai01l)
013400121218     c                   eval      w_vrbflg='I'
013500121218    3c                   endif
013600130207    3c     w_vrbflg      ifne      'I'
013700151007     c* Verifico se presente record "FAT"
013800151007     c                   clear                   dar5fat
013900151007     c                   eval      kAr5Trd  = 'FAT'
014000151007     c     kFiar5        Chain     Fiar531c
014100151008If  4c                   If        %Found(Fiar531c)
014200151007     c                   eval      dar5fat=ar5uni
014300151008    4c                   endif
014400160426      * 3 - bolla tassata con peso/vol desunto e ricevo variaz. di Peso/vol VDL
014500160426      *     --> Scarto
014600160426    4c*****              if        §AR5FPTAS<>'D' or  iv1cvb<>'PV'
014700160426     c                   if        (§AR5FPTAS<>'D' and §AR5FVTAS<>'D')
014800160426     c                             or
014900160426     c                             (iv1cvb<>'PV' and iv1cvb<>'VV')
015000151007     c                   exsr      ges_vrb
015100151008    4c                   endif
015200151008    3c                   endif
015300130207    2c                   endif
015400130117    1c                   endif
015500130117    0c                   endif
015600130114      *  Chiusura PGM
015700130114     c                   if        iv1tla =' '
015800130114     c                   return
015900130114     c                   else
016000130114     c                   exsr      chiusura
016100920113     C                   SETON                                        LR
016200130114     c                   endif
016300130204     C**---------------------------------------------------------
016400130204     C** Apertura file
016500130204     C**---------------------------------------------------------
016600130204     C     openfile      BEGSR
016700130204     c* Se non aperto apro il file
016800130204     c* TITAS
016900130204     c                   if        not %open(titas30c)
017000130204     C                   OPEN(e)   titas30c
017100130204     c
017200130204     c                   if        not %open(titas30c)
017300130204     c                   eval      %subst(tas30:7:4)='GRU '
017400130204     C                   OPEN      titas30c
017500130204     c                   endif
017600130204     c                   endif
017700130204     c* TNCSB
017800130204     c                   if        not %open(tncsb03l)
017900130204     C                   OPEN(e)   tncsb03l
018000130204     c
018100130204     c                   if        not %open(tncsb03l)
018200130204     c                   eval      %subst(csb03:7:4)='GRU '
018300130204     C                   OPEN      tncsb03l
018400130204     c                   endif
018500130204     c                   endif
018600130204     c* TITAI
018700130204     c                   if        not %open(titai01l)
018800130204     C                   OPEN(e)   titai01l
018900130204     c
019000130204     c                   if        not %open(titai01l)
019100130204     c                   eval      %subst(tai01:7:4)='GRU '
019200130204     C                   OPEN      titai01l
019300130204     c                   endif
019400130204     c                   endif
019500130204     c* fiar5
019600130204     c                   if        not %open(fiar531c)
019700130204     C                   OPEN(e)   fiar531c
019800130204     c
019900130204     c                   if        not %open(fiar531c)
020000130204     c                   eval      %subst(ar531:7:4)='GRU '
020100130204     C                   OPEN      fiar531c
020200130204     c                   endif
020300130204     c                   endif
020400130204     c* tntbe
020500130204     c                   if        not %open(tntbe01l)
020600130204     C                   OPEN(e)   tntbe01l
020700130204     c
020800130204     c                   if        not %open(tntbe01l)
020900130207     c                   eval      %subst(tbe01:7:4)='GRU '
021000130204     C                   OPEN      tntbe01l
021100130204     c                   endif
021200130204     c                   endif
021300130204     c* tivrb
021400130204     c                   if        not %open(tivrb01l)
021500130204     C                   OPEN(e)   tivrb01l
021600130204     c
021700130204     c                   if        not %open(tivrb01l)
021800130204     c                   eval      %subst(vrb01:7:3)='201'
021900130204     C                   OPEN      tivrb01l
022000130204     c                   endif
022100130204     c                   endif
022200130204     c                   endsr
022300121220     C**---------------------------------------------------------
022400121220     C**
022500121220     C**---------------------------------------------------------
022600121220     C     impo          BEGSR
022700121220     c*
022800121220     c* Valorizzazione DS delle variazioni effettuate:
022900121220     c* in base alla casuale variazione ricevuta valorizzo la ds relativa
023000121220     c                   select
023100130123     c                   when      %subst(iv1cvb:1:1)='I'
023200121220     c                   eval      dsarbd=parvar
023300121220     c                   when      %subst(iv1cvb:1:1)='K'
023400121220     c                   eval      dsarbk=parvar
023500130124     c                   when      %subst(iv1cvb:1:1)='T'
023600130124     c                   if        iv1pgm='TRTR39R'
023700121220     c                   eval      dsarbt=parvar
023800130124     c                   else
023900130124     c                   eval      darbt=parvar
024000130124     c                   z-add     §btias        dsarbt.arbias
024100130124     c                   movel     §btvas        dsarbt.arbvas
024200130124     c                   endif
024300121220     c                   when      %subst(iv1cvb:1:1)='C'
024400121220     c                   eval      dsarbg=parvar
024500130118     c                   when      iv1cvb='VV' or iv1cvb='PV'
024600130118     c                   eval      dsarbv=parvar
024700130123     c                   when      iv1cvb='ZT'
024800130122     c                   eval      parfin=%subst(parvar:1:1)
024900140123     c                   when      iv1cvb='RI'
025000140123     c                   eval      parric=%subst(parvar:1:1)
025100121220     c                   endsl
025200121220     c* se ricevuta la key spedizione la utilizzo per chain titas e csb
025300121220     c* altrimenti utilizzo 3° e 4° parametro
025400140128     c                   clear                   wnotf
025500121220     c                   if        iv1aas<>0
025600140128     c* se non ricevuto il tipo bolla eseguo ricerca
025700140128     c                   if        iv1tbl=*blanks
025800140128     c     kbols         setgt     titas30c
025900140128     c     kbols         readpe    titas30c
026000140128     c                   else
026100121220     c     kbol          chain     titas30c
026200140128     c                   endif
026300140128     c                   if        %eof (titas30c)
026400140128     c                   eval      wnotf='1'
026500140128     c                   endif
026600121220     c                   else
026700130114     c                   eval      titasds=partitas
026800130114     c                   if        %subst(iv1cvb:1:1)='K'
026900130114     c                   eval      dscsb=parcsb
027000130114     c                   endif
027100121220     c                   endif
027200121220     c*
027300121220     c                   clear                   w_vrbdimp
027400121220     c                   clear                   w_vrbflg
027500130117     c*
027600121220     C                   MOVEL     'TB'          COD
027700121220     C                   MOVEL(P)  tastbl        KEY
027800121220     c                   clear                   dstb
027900121220     C     KTAB          CHAIN     TABEL
028000121220     c                   if        %found(tabel00f)
028100121220     C                   MOVEL     TBLUNI        DSTB
028200121220     c                   endif
028300130118     C                   MOVEL     '3A'          COD
028400130118     C                   MOVEL(P)  tascbo        KEY
028500130118     c                   clear                   ds3a
028600130118     C     KTAB          CHAIN     TABEL
028700130118     c                   if        %found(tabel00f)
028800130118     C                   MOVEL     TBLUNI        DS3a
028900130118     c                   endif
029000121220     c                   endsr
029100130114     C**---------------------------------------------------------
029200130114     C**
029300130114     C**---------------------------------------------------------
029400130114     C     chiusura      BEGSR
029500130114     C*
029600130114     C                   CLEAR                   DSSI95
029700130114     C                   MOVEL     'C'           I95TLA
029800130114     C                   CALL      'TISI95R'
029900130114     C                   PARM                    DSSI95
030000130114
030100130114     C     WSF20         IFNE      *BLANKS
030200130114     C                   MOVEL     'C'           dtas_TASTLA
030300130206     C                   CALL      'TNSF20R'                            99
030400130114     C                   PARM                    KPJBA
030500130114     C                   PARM                    DTAS
030600130114     C                   PARM                    DTASV
030700130114     C                   ENDIF
030800130114
030900130114     c                   endsr
031000120322     C**---------------------------------------------------------
031100120322     C** Richiamo tisi95 per determinazione codice tassazione
031200120322     C**---------------------------------------------------------
031300120322     C     C_tisi95      BEGSR
031400120322     C                   CLEAR                   DSSI95
031500130118     C                   MOVEL     dsarbd.ARBCAD I95CAP
031600130118     C                   MOVEL     dsarbd.ARBNZD I95NAR
031700130118     C                   MOVEL     dsarbd.ARBLOD I95LOC
031800130118     C                   MOVEL     dsarbd.ARBPRD I95PRV
031900121116     c                   movel     tasaas        i95dat
032000121116     c                   move      tasmgs        i95dat
032100120322     C                   MOVEL     '7'           I95TCN
032200120322      * Se network FedEx passo flag apposito per dire di prendere i dati
032300120322      * dalla tabella della nazione
032400120322     c     taslna        chain     azorg01l
032500120322     c                   if        %found(azorg01l) and orgfva = *blanks
032600120322     c                   eval      og143 = orgde3
032700120322     c     §ogntw        ifeq      'FED'
032800120322     c                   eval      i95fi1 = 'S'
032900120322     c                   endif
033000120322     c                   endif
033100120322
033200120322     C                   MOVEL     §3ATB1        I95TPO
033300120322    5C     §3AFCA        IFNE      0
033400120322     C                   MOVEL     'S'           I95FCA
033500120322    5C                   ENDIF
033600120322     C**
033700120322     C                   CALL      'TISI95R'
033800120322     C                   PARM                    DSSI95
033900120322     c                   endsr
034000941130     C**---------------------------------------------------------
034100941130     C* ROUTINE INIZIALE
034200941130     C**---------------------------------------------------------
034300941130     C     *INZSR        BEGSR
034400021014
034500021014
034600941130     C                   Z-ADD     1             CODUT             1 0
034700021014
034800021014     c     *dtaara       define    §azute        azuteds
034900021014     c     *dtaara       define    §datiute      ddatiute
035000021014     C                   in(E)     *dtaara
035100021014     C                   IF        %Error  or  RSUT = *blanks
035200021014     C                   CLEAR                   TIBS34DS
035300021014     C                   call      'TIBS34R'
035400021014     C                   parm                    Tibs34Ds
035500021014     C                   in        *dtaara
035600021014     c                   ENDIF
035700021014
035800021014     C                   Z-ADD     DUTKCI        KCI               4 0
035900021014
036000121218     C     KTAB          KLIST
036100121218     C                   KFLD                    CODUT
036200121218     C                   KFLD                    COD               2
036300121218     C                   KFLD                    KEY               8
036400130118     c     kbol          klist
036500130118     c                   kfld                    iv1aas
036600130118     c                   kfld                    iv1lnp
036700130118     c                   kfld                    iv1nrs
036800130118     c                   kfld                    iv1nsp
036900130118     c                   kfld                    iv1tbl
037000140128     c     kbols         klist
037100140128     c                   kfld                    iv1aas
037200140128     c                   kfld                    iv1lnp
037300140128     c                   kfld                    iv1nrs
037400140128     c                   kfld                    iv1nsp
037500121218     c     ktai06        klist
037600121218     c                   kfld                    tasaas
037700121218     c                   kfld                    taslnp
037800121218     c                   kfld                    tasnrs
037900121218     c                   kfld                    tasnsp
038000121218     c                   kfld                    tastbl
038100121218     c                   kfld                    Ksvt
038200121218      * Chiave per setll su TIVRB01l
038300121218     c     Kvrb          Klist
038400121218     c                   kfld                    tasaas
038500121218     c                   kfld                    taslnp
038600121218     c                   kfld                    tasnrs
038700121218     c                   kfld                    tasnsp
038800121218     c                   kfld                    tastbl
038900121218     c                   kfld                    wcvb
039000941130     C     KANN          KLIST
039100130118     C                   KFLD                    tasAAS
039200130118     C                   KFLD                    tasLNP
039300130118     C                   KFLD                    tasNRS
039400130118     C                   KFLD                    tasNSP
039500120424     c*
039600120424     c     ktai05        klist
039700120424     c                   kfld                    tasaas
039800120424     c                   kfld                    taslnp
039900120424     c                   kfld                    tasnrs
040000120424     c                   kfld                    tasnsp
040100120424     c                   kfld                    tastbl
040200120313
040300120319     c     kFiar5        Klist
040400120319     c                   Kfld                    TasAas
040500120319     c                   Kfld                    TasLnp
040600120319     c                   Kfld                    TasNrs
040700120319     c                   Kfld                    TasNsp
040800120319     c                   Kfld                    kAr5Trd
040900120319      * Chiave file eventi FNEVB
041000120319     c     Kevb4         Klist
041100120319     c                   Kfld                    Tasaas
041200120319     c                   Kfld                    Taslnp
041300120319     c                   Kfld                    Tasnrs
041400120319     c                   Kfld                    Tasnsp
041500120319     c                   Kfld                    Wcev
041600130204
041700130204     c     ktbe          klist
041800130204     c                   kfld                    kcod
041900130204     c                   kfld                    kke1
042000071102
042100941130     C                   ENDSR
042200930804     C*
042300121129     C**-------------------------------------------------------------
042400121129     C** Gestione della bolla per verificare se da scrivere
042500121129     c*  nel file per la creazione della bolla di recupero (TIVRB)
042600121129     C**-------------------------------------------------------------
042700121129     C     ges_vrb       BEGSR
042800121129     c* Verifico se bolla già presente su file delle variazioni
042900121130     c                   eval      wcvb='VA'
043000121203     c     kvrb          chain(N)  tivrb01l
043100121130     c* BOLLA NON PRESENTE: tasso e memorizzo solo se delta tassazione > 0
043200121203    1c                   if        not %found(tivrb01l)
043300121130     c                   exsr      sr_tassa1
043400121129    2c                   if        dtas_taserr=*blanks
043500121129     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
043600121129    3c                   if        w_vrbdimp>0
043700121129     c* recupero la data penultima fatturazione mensile per vedere se
043800121129     c* la variazione è da scrivere nel file di lavoro
043900121129     c                   exsr      rec_RPFdata
044000170411     c***                if        tasdft>§RPFDTA and §rpfdta>0
044001170411     c                   if        tasdft>wdataRPF and wdataRPF>0
044100121129     c                   exsr      wrtvrb
044200121129     c                   endif
044300121129    3c                   endif
044400121129    2c                   endif
044500121130   x1c                   else
044600121130     c* BOLLA PRESENTE: tasso e verifico se aggiornare il file delle variazioni
044700121130     c* recuperare
044800121130     c                   exsr      sr_tassa2
044900121130    2c                   if        dtas_taserr=*blanks
045000121130     c                   eval      W_vrbdimp=dtas_tasimv-tasimv
045100121130    3c                   select
045200121130     c* DELTA TASSAZIONE >0 : Aggiorno record "VA"
045300121203     c                   when      W_vrbdimp>0  and w_vrbdimp>vrbdimp
045400121130     c* recupero la data penultima fatturazione mensile per vedere se
045500121130     c* la variazione deve aggiornare il file di lavoro
045600121130     c                   exsr      rec_RPFdata
045700170411     c****               if        tasdft>§RPFDTA and §rpfdta>0
045701170411     c                   if        tasdft>wdataRPF and wdataRPF>0
045800121130     c                   exsr      aggvrb
045900121130     c                   endif
046000121130     c                   other
046100121130     c* DELTA TASSAZIONE <=0: Verifico se differenze da aggiornare
046200121130     c* mi salvo il record "VA" prima di chainare il record "  "
046300121130     c                   eval      dsva=dsvrb
046400121130     c                   clear                   wcvb
046500121130     c                   clear                   wdiffe
046600121203     c     kvrb          chain(N)  tivrb01l
046700121130     c                   exsr      ver_diffe
046800121130    4c                   if        wdiffe='1'
046900121130     c* rieseguo chain del record "VA" per aggiornarlo
047000121130     c                   eval      wcvb='VA'
047100121203     c     kvrb          chain     tivrb01l
047200121203   4ac                   if        %found(tivrb01l)
047300121130     c* recupero la data penultima fatturazione mensile per vedere se
047400121130     c* la variazione deve aggiornare il file di lavoro
047500121130     c                   exsr      rec_RPFdata
047600170411    5c****               if        tasdft>§RPFDTA and §rpfdta>0
047601170411     c                   if        tasdft>wdataRPF and wdataRPF>0
047700121130     c                   eval      dsvrb=dsva
047800121130     c                   time                    vrbtmst
047900121130     c                   eval      vrbdimp=w_vrbdimp
048000121130     c                   update    tivrb000
048100121130    5c                   endif
048200121130   4ac                   endif
048300121130    4c                   endif
048400121130    3c                   endsl
048500121130    2c                   endif
048600121129    1c                   endif
048700121130     C                   MOVEL     '1'           WSF20
048800121129     c                   endsr
048900121130     C**-------------------------------------------------------------
049000121130     C** Tassazione - I volta
049100121130     C**-------------------------------------------------------------
049200121130     C     sr_tassa1     BEGSR
049300121130     c* campi della DTAS da prendere da titas
049400121130     c                   exsr      riedtasUNI
049500121130     c                   exsr      riedtas
049600121130     c* campi della dtas da impostare con campi della variazione in ricezione
049700121130     c                   exsr      riedtasvar
049800121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
049900121130     c                   exsr      riedtasALTRI
050000121130      * C/Assegno
050100121130    3c                   IF        Tasfca <> *blanks
050200130116     c                             and %subst(iv1cvb:1:1)<>'K'
050300121130     c                   Exsr      Carcsb
050400121130    3c                   Endif
050500121130      * recupero i valori  già tassati
050600121130     c                   exsr      Ric_tai
050700150330
050800150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
050900150330     c                   if        §3asva<>*blanks
051000150330     c                   eval      dtas_tassva=§3asva
051100150330     c                   endif
051200121130
051300130206     C                   CALL      'TNSF20R'
051400121130     C                   PARM                    KPJBA
051500121130     C                   PARM                    DTAS
051600121130     C                   PARM                    DTASV
051700121130     c                   endsr
051800121130     C**-------------------------------------------------------------
051900121130     C** Tassazione - Bolla già presente nelle variazioni da recuperare
052000121130     C**-------------------------------------------------------------
052100121130     C     sr_tassa2     BEGSR
052200121130     c* campi della dtas da prendere da titas
052300121130     c                   exsr      riedtasUNI
052400121130     c* campi della dtas da prendere dal record "VA" di TIVRB
052500121130     c                   exsr      riedtasVRB
052600121130     c* campi della dtas da impostare con campi della variazione in ricezione
052700121130     c                   exsr      riedtasVAR
052800121130     c* campi della dtas restanti da prendere da TITAS e suoi collegati
052900121130     c                   exsr      riedtasALTRI
053000121130      * recupero i valori  già tassati
053100121130     c                   exsr      Ric_tai
053200150330
053300150330     c* se devo tassare solo una varia, da tipo bolla, la imposto
053400150330     c                   if        §3asva<>*blanks
053500150330     c                   eval      dtas_tassva=§3asva
053600150330     c                   endif
053700121130
053800130206     C                   CALL      'TNSF20R'
053900121130     C                   PARM                    KPJBA
054000121130     C                   PARM                    DTAS
054100121130     C                   PARM                    DTASV
054200121130     c                   endsr
054300121130      *****************************************************************
054400121130      * Impostazione campi della dtas da campi di titas
054500121130      *****************************************************************
054600121130     c     RiedtasUNI    Begsr
054700121130     c                   clear                   dtas
054800121130     c                   clear                   dtasv
054900121130     c                   eval      dtas_TASKSC=tasksc
055000121130     c                   eval      dtas_TASNCL=tasncl
055001160530     c                   eval      dtas_TASNCLB=tasncl
055100121130     c                   eval      dtas_TASCTR=tasctr
055200121130     c                   eval      dtas_TASFDN=tasfdn
055300121130     c                   eval      dtas_TASMCT=tasmct
055400121130     c                   eval      dtas_TASNZM=tasnzm
055500121130     c                   eval      dtas_TASCAM=tascam
055600121130     c                   eval      dtas_TASFAP=tasfap
055700121130     c                   eval      dtas_TASTSP=tastsp
055800121130     c                   eval      dtas_taslna=taslna
055900121130     c                   eval      dtas_TASDIV=tasdiv
056000151012     c* tnsf20r usa taspvl come campo di ouptut quindi asterisco
056100151012     c***                eval      dtas_TASPVL=taspvl
056101160531     c* Peso Bollettatto (DTAS_taspkb): non lo valorizzo in quanto
056102160531     c* serve per la tassazioe con peso desunto (TNSF22R)
056103160531     c* e in filiale non possono variare TASPKB
056104160531     c* Se fosse possibile variarlo bisognerebbe probabilmente aggiungere questo
056105160531     c* campo nel TIVRB
056200121130     c                   endsr
056300121130      *****************************************************************
056400121130      * Impostazione campi della dtas da campi di tivrb
056500121130      *****************************************************************
056600121130     c     RiedtasVRB    Begsr
056700121130     c                   eval      dtas_taspkf=VRBPKF
056800121130     c                   eval      dtas_taspkc=vRBPKC
056900121130     c                   eval      dtas_tasncp=VRBNCP
057000121130     c                   eval      dtas_tasvlf=VRBVLF
057001160531     c                   eval      dtas_tasfvf=VRBfvf
057002160531     c                   eval      dtas_tasvlc=vRBvlc
057003160531     c                   eval      dtas_tasncr=VRBNcr
057100121130     c                   eval      dtas_tascts=VRBCTS
057200121130     c                   eval      dtas_tasnzd=VRBNZD
057300121130     c                   eval      dtas_tascad=VRBCAD
057400121130     c                   eval      dtas_tasfin=VRBFIN
057500121130     c                   eval      dtas_tastc1=VRBTC1
057600121130     c                   eval      dtas_tastc2=VRBTC2
057700121130     c                   eval      dtas_taslnp=vrblnp
057800121130     c                   eval      dtas_TASTBL=vrbtbl
057900121130     c                   eval      dtas_tasvas=VRBVAS
058000121130     c                   eval      dtas_tasias=VRBIAS
058100121130     c                   eval      dtas_tasqft=VRBQFT
058200121130     c                   eval      dtas_tastic=VRBTIC
058300121130     c                   eval      dtas_tascas=VRBCAS
058400121130     c                   eval      dtas_tasvca=VRBVCA
058500121130     c                   eval      dtas_tascmb=VRBCMB
058600121130     c                   move      VRBSTA        dtas_tassta
058700121130     c                   endsr
058800121130      *****************************************************************
058900121130      * Campi della DTAS da impostare con la variazione in ricezione
059000121130      *****************************************************************
059100121130     c     RiedtasVAR    Begsr
059200121130    2c                   select
059300121130     c* Variazioni Destinatario/peso/volume
059400130116     c                   when      %subst(iv1cvb:1:1)='I'
059500160426     c* considero peso/vol ricevuti solo se bolla non tassata con peso desunto
059600160426     c                   if        §ar5fptas<>'D' and §AR5FVTAS<>'D'
059700130118     c                   z-add     dsarbd.arbpkf dtas_taspkf
059800160426     c                   z-add     dsarbd.arbvlf dtas_tasvlf
059801160531     c                   movel     dsarbd.arbfvf dtas_tasfvf
059900151007     c                   endif
060000160426     c***                z-add     dsarbd.arbvlf dtas_tasvlf
060100130118     c                   movel     dsarbd.arbfin dtas_tasfin
060200130118     c                   movel     dsarbd.arbcad dtas_tascad
060300130118     c                   movel     dsarbd.arbnzd dtas_tasnzd
060400130118    3C     iv1cvb        ifeq      'I0'
060500151008     c     iv1cvb        oreq      'I7'
060600151008     c     iv1cvb        oreq      'I8'
060700151008     c     iv1cvb        oreq      'I9'
060800121130     c                   exsr      c_tisi95
060900121130     c                   if        o95err = *blanks
061000121130     c                   eval      dtas_tascts=o95cts
061100121130     c                   endif
061200121130    3c                   endif
061300121130     c* Variazioni codice bolla/contrassegno
061400130116     c                   when      %subst(iv1cvb:1:1)='K'
061500121205     c* Contrassegno ANNULLATO: prendo i campi da tncsb
061600121205     c*                         e metto stato 9
061700130116    3c                   if        iv1can='S'
061800121205     c                   eval      dtas_tascas=dscsb.csbcas
061900121205     c                   eval      dtas_tasvca=dscsb.csbvca
062000121205     c                   eval      dtas_tascmb=dscsb.csbcmb
062100121205    4c                   if        dscsb.csbfus<>' '
062200121210     c****               eval      dtas_tastic=dscsb.csbfus
062300121210     c                   move      dscsb.csbfus  dtas_tastic
062400121205   x4c                   else
062500121210     c****               eval      dtas_tastic=dscsb.csbtpi
062600121210     c                   move      dscsb.csbtpi  dtas_tastic
062700121205    4c                   endif
062800121205     c                   eval      dtas_Tassta='9'
062900121205   x3c                   else
063000121205     c* Contrassegno NON Annullato: prendo i campi dalla variazione
063100130118     c                   eval      dtas_tascas=dsarbk.arbcas
063200130118     c                   eval      dtas_tasvca=dsarbk.arbvca
063300130118     c                   eval      dtas_tascmb=dsarbk.arbcmb
063400121205     c                   clear                   dtas_tastic
063500121205     C                   MOVEL     '1A'          COD
063600130118     C                   MOVEL(P)  dsarbk.arbtic KEY
063700121205     C     KTAB          CHAIN     TABEL
063800121205    4c                   if        %found(tabel00F)
063900121205     C                   MOVEL     TBLUNI        DS1A
064000121205     c                   move      §1afmb        dtas_tastic
064100121205    4c                   endif
064200121205     c* .......... flag stato: metto 0 o 3 in base a quello che c'è su TNCSB
064300121205    4c                   if        dscsb.csbsta<>3
064400121205     c                   eval      dtas_Tassta='0'
064500121205     c                   else
064600121205     c                   eval      dtas_tassta='3'
064700121205    4c                   endif
064800121205    3c                   endif
064900121130     c* Variazioni Consegne particolari
065000130116     c                   when      %subst(iv1cvb:1:1)='C'
065100130118     c                   movel     dsarbg.arbtc1 dtas_tastc1
065200130118     c                   movel     dsarbg.arbtc2 dtas_tastc2
065300121130     c* Variazione Importo da assicurare
065400130116     c                   when      %subst(iv1cvb:1:1)='T'
065500130118     c                   movel     dsarbt.arbias dtas_tasias
065600130118     c                   movel     dsarbt.arbvas dtas_tasvas
065700130116     c* Variazione Volume VDL
065800130116     c                   when      iv1cvb='VV'
065900130118     c                   z-add     §bvvlf        dtas_tasvlf
065901160601     c                   movel     §bvfvf        dtas_tasfvf
065902160531     c                   z-add     §BVNCR        dtas_tasncr
065903160531     c                   z-add     §BVVLC        dtas_tasvlc
066000130122     c* Variazione Peso   VDL
066100130122     c                   when      iv1cvb='PV'
066200130122     c                   z-add     §bvpkf        dtas_taspkf
066300130122     c                   z-add     §bvpkc        dtas_taspkc
066400130122     c                   z-add     §bvncp        dtas_tasncp
066500130122     c* Variazione inoltro Z/T
066600130123     c                   when      iv1cvb='ZT'
066700130122     c                   eval      dtas_tasfin=parfin
066800121130    2c                   endsl
066900121130     c                   endsr
067000120424      *****************************************************************
067100121130      * Impostazione campi della dtas da campi di titas
067200120424      *****************************************************************
067300120424     c     Riedtas       Begsr
067400160531     c* non presente il record "FAT" --> tasso con peso e volume presi da titas
067500151008     c                   if        §AR5PKTAS=0
067600120424     c                   eval      dtas_TASPKF=taspkf
067700120424     c                   eval      dtas_TASPKC=taspkc
067800120424     c                   eval      dtas_TASNCP=tasncp
067900120424     c                   eval      dtas_TASVLF=tasvlf
067901160531     c                   eval      dtas_TASfvf=tasfvf
067902160531     c                   eval      dtas_TASvlc=tasvlc
067903160531     c                   eval      dtas_TASncr=tasncr
068000151008     c                   else
068100151008     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
068200151008     c                   eval      dtas_TASPKF=§ar5pktas
068300151008     c                   eval      dtas_TASVLF=§AR5VLTAS
068301160531     c                   eval      dtas_TASfvf=§AR5FVTAS
068400151008     c                   endif
068500120424     c                   eval      dtas_TASCTS=tascts
068600120424     c                   eval      dtas_TASNZD=tasnzd
068700120424     c                   eval      dtas_TASCAD=tascad
068800120424     c                   eval      dtas_TASFIN=tasfin
068900120424     c                   eval      dtas_TASTC1=tasftc
069000120424     c                   eval      dtas_TASTC2=tastc2
069100120424     c                   eval      dtas_taslnp=taslnp
069200121130     c                   eval      dtas_TASTBL =tastbl
069300120424     c                   eval      dtas_TASVAS =tasvas
069400120424     c                   eval      dtas_TASIAS =tasias
069500120424     c                   eval      dtas_TASQFT =tasqft
069600120424     c                   endsr
069700121130      *****************************************************************
069800121130      * Altri campi della dtas da prendere da titas
069900121130      *****************************************************************
070000121130     c     RiedtasAltri  Begsr
070100121130     c                   clear                   dAr5Ban
070200121130     c                   Clear                   dAr5Bnb
070300121130     c                   Clear                   kpjbu
070400121130     c                   movel     tasaas        dtas_tasdsp
070500121130     c                   move      tasmgs        dtas_tasdsp
070600121130     c                   eval      dtas_tasdct=dtas_tasdsp
070700121130
070800121130      * Flag SI NO DDT
070900121130    2C                   If        tasll1 = 'C' or tasll1 = 'S'
071000121130     C                   movel     'S'           dtas_Tasddt
071100121130   x2C                   Else
071200121130     C                   Clear                   dtas_Tasddt
071300121130    2C                   Endif
071400121130     c                   clear                   dtas01
071500121130     c                   movel     tasfbr        §asfbr
071600121130     c                   movel     tascca        §ascca
071700121130      * valorizzo flag email al destinatario
071800121130     c                   movel     tasflo        dtasflo
071900121130     c                   move      §floemd       §asemd
072000150108     c* flag pin code
072100150108     c                   if        tasgma<>*blanks
072200150108     c                   clear                   ds7r
072300150108     c                   movel     '7R'          cod
072400150108     c                   movel(p)  tasgma        key
072500150108     c     ktab          chain     tabel
072600150108    8c                   if        %found(tabel00f)
072700150108     c                   movel     tbluni        ds7r
072800150108    8c                   endif
072900150108     c                   if        §7rpincode='S'
073000150108     c                   movel     'S'           §aspin
073100150108     c                   endif
073200150108     c                   endif
073300121130
073400121130     c                   eval      dtas_tasflo = dtas01
073500160107     c* solo per prima bolla
073600160107     c                   if        §3atb2=*blanks or %subst(tastbl:1:1)
073700160107     c                             <>%subst(§3atb2:1:1)
073800160107     c
073900160107     c* flag DIRITTO DI CHIAMATA  da 10° byte di TASFLO di TITAS
074000160108     c                   move      §floado       dtas_tasprt
074100160107     c* flag per ADDEBITO PACKING LIST da fiar5 "GEN"
074200160107     c                   clear                   dar5gen
074300160107     c                   eval      kAr5Trd  = 'GEN'
074400160107     c     kFiar5        Chain     Fiar531c
074500160107If  3c                   If        %Found(Fiar531c)
074600160107     c                   Movel     Ar5Uni        dAr5gen
074700160107    3c                   EndIf
074800160107     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
074900160108     c                   Eval      dtas_TasSpl = 'S'
075000160107     c                   endif
075100160107     c                   endif
075200121130
075300121130     c
075400121130      * Bancali
075500121130    2c                   If        %Subst(TasGva:1:1) = 'B'
075600121130
075700121130     c                   eval      kAr5Trd  = 'BAN'
075800121130     c     kFiar5        Chain     Fiar531c
075900121130If  3c                   If        %Found(Fiar531c)
076000121130     c                   Movel     Ar5Uni        dAr5Ban
076100121130    3c                   EndIf
076200121130      * Bancali
076300121130     c                   Eval      dtas_TasBan = §Ar5Nba + §Ar5Nb2
076400121130    2c                   EndIf
076500121130      **
076600121130      * Colli Originali
076700121130    2c                   If        %Subst(TasGva:1:1) = 'O'
076800121130     c                   eval      kAr5Trd  = 'BNB'
076900121130     c     kFiar5        Chain     Fiar531c
077000121130If  3c                   If        %Found(Fiar531c)
077100121130     c                   Movel     Ar5Uni        dAr5Bnb
077200121130    3c                   EndIf
077300121130      * Colli
077400121130     c                   Eval      dtas_TasNcl = §Ar5bcor
077500121130    2c                   EndIf
077600121130
077700121130      **** verifico se devo calcolare l'addebito di lasciato avviso
077800121130      **** verifico se c'è evento
077900140127      **** Se Variazione "RI" non ho bisogno di verificare la presenza dell'
078000140127      **** evento
078100140127     c                   if        iv1cvb='RI'
078200140127     c                   eval      dtas_tasric = 'S'
078300140127     c                   else
078400140127     c                   clear                   evbdtr
078500140127     c     Kevb4         setll     Fnevb04l
078600140127     c     Kevb4         reade     Fnevb04l
078700140127     c                   dow       not %eof(fnevb04l) and evbdtr=0
078800140127     c     Kevb4         reade     Fnevb04l
078900140127     c                   enddo
079000140127     c                   if        evbdtr>0
079100140127     c                   eval      dtas_tasric = 'S'
079200140127     c                   endif
079300140127     c                   endif
079400121130
079500121130      * Assicurazione
079600121130    2c                   IF        Tasfcm = 'F'
079700121130     c                   clear                   dtas_tasias
079800121130     c                   clear                   dtas_tasvas
079900121130    2c                   Endif
080000121130     c                   endsr
080100121130
080200120316      *****************************************************************
080300120316      *  Recupero importi di tassazione dal file titai
080400120316      *****************************************************************
080500120316     c     Ric_tai       Begsr
080600120316      *
080700120316     c                   z-add     0             k
080800120316
080900120316     c     ktai05        setll     titai01l
081000120316
081100120316     c                   If        %equal(titai01l)
081200120316
081300120316     c                   do        *hival
081400120316
081500120316     c     ktai05        reade     titai01l
081600120316
081700120316     c                   If        %eof(titai01l)
081800120316     c                   leave
081900120316     c                   endif
082000120316      * imponibile
082100120316     c                   if        taisvt = '£'
082200120424     c                   eval      dtas_tasimv = taivat
082300120316     c                   endif
082400120316      * porto
082500120316     c                   if        taisvt = ' '
082600120424     c                   eval      dtas_taspor = taivat
082700120316     c                   endif
082800120316      * varie
082900120316     c                   if        taisvt <> ' ' and taisvt <> '£'
083000120612     c* se presente varia N 88.888.888 devo prima tassarla
083100120612    4c                   if        taisvt='N' and taivat=88888888
083200120612     c                   eval      savdtas=dtas
083300120612     c                   eval      savdtasv=dtasv
083400120612     c                   exsr      tassan
083500120612     c                   eval      dtas=savdtas
083600120612     c                   eval      dtasv=savdtasv
083700120612     c                   add       1             k
083800120612     c                   eval      sv(k) ='N'
083900120613     c                   eval      va(k) =antepo
084000120612   x4c                   else
084100120316     c                   add       1             k
084200120316     c                   eval      sv(k) = taisvt
084300120316     c                   eval      va(k) = taivat
084400120612    4c                   endif
084500120316     c                   endif
084600120316
084700120316     c                   enddo
084800120316
084900120316     c                   else
085000120316      * se non è pretassata pulisco la divisa
085100120424     c                   clear                   dtas_tasdiv
085200120316     c                   endif
085300120424     c                   endsr
085400120612      ****************************************************
085500120612      ** Tassazione varia N 88888888 (Anteporto)
085600120612      ****************************************************
085700120612     c     tassan        Begsr
085800120613     C                   CLEAR                   DTAS_tasimv
085900120613     C                   CLEAR                   DTAS_taspor
086000120613     C                   CLEAR                   DTASV
086100120612     c                   clear                   antepo
086200120612      * tolgo il flag dell'invio mail al destinatario nell'eventualità ci fosse
086300150108      * idem per flag Pin Code
086400160113      * e per  diritto di chiamata e packing list
086500120612     c                   eval      dtas01 = dtas_tasflo
086600120612     c                   clear                   §asemd
086700150108     c                   clear                   §aspin
086800120612     c                   eval      dtas_tasflo = dtas01
086900160113     c                   clear                   dtas_tasprt
087000160113     c                   clear                   dtas_tasspl
087100120612      *
087200130206     C                   CALL      'TNSF20R'
087300120612     C                   PARM                    KPJBA
087400120612     C                   PARM                    DTAS
087500120612     C                   PARM                    DTASV
087600120612      *
087700120612      * Tassazione andata buon fine
087800120613     C     dtas_taserr   IFEQ      *BLANKS
087900120613     c                   eval      antepo=dtas_tasimv
088000120612     C                   ENDIF
088100120612     c                   endsr
088200120316      ****************************************************
088300120316      ** Ricerca Contrassegno
088400120316      ****************************************************
088500120316     c     Carcsb        Begsr
088600120316
088700130118     c     kann          chain     Tncsb03l
088800120316     c                   If        %found(tncsb03l) and
088900120316      * solo per stesso tipo bolla
089000120316     c                             tastbl = csbtbl
089100120424     c                   movel     csbsta        dtas_Tassta
089200120424     c                   z-add     csbcas        dtas_Tascas
089300120316     c                   if        csbfus <> *blanks
089400120424     c                   move      csbfus        dtas_tastic
089500120316     c                   else
089600120424     c                   move      csbtpi        dtas_tastic
089700120316     c                   end
089800120316      * mittente
089900120424     c                   movel     csbvca        dtas_tasvca
090000120424     c                   z-add     csbcmb        dtas_tascmb
090100120316
090200120316     c                   endif
090300120316
090400120316     c                   endsr
090500120319      ****************************************************
090600120319      ** Scrittura work-file delle bolle già fatturate
090700120319      ****************************************************
090800120319     c     wrtvrb        Begsr
090900121130     c                   clear                   tivrb000
091000121130     c                   eval      vrbaas=tasaas
091100121130     c                   eval      vrblnp=taslnp
091200121130     c                   eval      vrbnrs=tasnrs
091300121130     c                   eval      vrbnsp=tasnsp
091400121130     c                   eval      vrbtbl=tastbl
091500121130     c                   time                    vrbtmst
091600121130     c                   eval      vrbdft=tasdft
091700151008     c* Presente record "FAT" --> memorizzo peso e volume di questo
091800151008     c                   if        §AR5PKTAS>0
091900151008     c                   eval      VRBPKF=§ar5pktas
092000151008     c                   eval      VRBfpf=§ar5fptas
092100151008     c                   eval      vrbvlf=§AR5VLTAS
092200151008     c                   eval      VRBFVF=§AR5FVTAS
092300151008     c* . . . se bolla tassata con peso desunto non memorizzo il peso VDL
092400151008     c                   if        §ar5fptas<>'D'
092500151008     c                   eval      VRBPKC=taspkc
092600151008     c                   eval      VRBNCP=tasncp
092700151008     c                   endif
092800160427     c* . . . se bolla tassata con vol. desunto non memorizzo il vol. VDL
092900160427     c                   if        §ar5fvtas<>'D'
093000160427     c                   eval      VRBvlC=tasvlc
093100160427     c                   eval      VRBNCr=tasncr
093200160427     c                   endif
093300151008     c                   else
093400151008     c* Non presente il record "FAT" --> peso e volume da titas
093500130115     c                   eval      VRBPKC=taspkc
093600130115     c                   eval      VRBNCP=tasncp
093700130115     c                   eval      VRBPKF=taspkf
093800130115     c                   eval      VRBFPF=tasfpf
093900130115     c                   eval      VRBVLF=tasvlf
094000130115     c                   eval      VRBFVF=tasfvf
094100160427     c                   eval      VRBVLC=tasvlc
094200160427     c                   eval      VRBNCR=tasncr
094300151008     c                   endif
094400130115     c                   eval      VRBQFT=tasqft
094500130115     c                   eval      VRBIAS=tasias
094600130115     c                   eval      VRBVAS=tasvas
094700130115     c                   eval      VRBCTS=tascts
094800130115     c                   eval      VRBFIN=tasfin
094900130115     c                   eval      VRBCAD=tascad
095000130115     c                   eval      VRBNZD=tasnzd
095100130115     c                   eval      VRBFFD=tasffd
095200130115     c                   eval      VRBTC1=tasftc
095300130115     c                   eval      VRBTC2=tastc2
095400130116    2c                   if        %subst(iv1cvb:1:1)<>'K'
095500121109     c                   eval      VRBTIC=dtas_tastic
095600121109     c                   eval      VRBCAS=dtas_tascas
095700121109     c                   eval      VRBVCA=dtas_tasvca
095800121109     c                   eval      VRBCMB=dtas_tascmb
095900121115     c                   move      dtas_tassta   vrbsta
096000121109   x2c                   else
096100121115    3c                   if        dscsb.csbtbl=tastbl
096200121115    4c                   if        dscsb.csbfus <> *blanks
096300121115     c                   move      dscsb.csbfus  vrbtic
096400121109   x4c                   else
096500121115     c                   move      dscsb.csbtpi  vrbtic
096600121109    4c                   end
096700121115     c                   eval      VRBCAS=dscsb.csbcas
096800121115     c                   eval      VRBVCA=dscsb.csbvca
096900121115     c                   eval      VRBCMB=dscsb.csbcmb
097000121115     c                   eval      VRBsta=dscsb.csbsta
097100121109     c                   endif
097200121109    2c                   endif
097300140123    2c                   if        iv1cvb<>'RI'
097400140123     c                   eval      vrbric=dtas_tasric
097500140123    2c                   endif
097600121108     c                   write     tivrb000
097700121129     c* SCRITTURA RECORD "VA" (Prima volta)
097800121129     c                   eval      vrbcvb='VA'
097900130118     c                   exsr      RieVrbUni
098000121129     c                   write     tivrb000
098100121129     c*
098200120319     c                   endsr
098300121130      ****************************************************
098400121130      ** Aggiornamento record "VA" del file delle variazioni
098500121130      ** da recuperare per delta tassazione >0
098600121130      ****************************************************
098700121130     c     aggvrb        Begsr
098800121130     c                   eval      wcvb='VA'
098900121203     c     kvrb          chain     tivrb01l
099000121203     c                   if        %found(tivrb01l)
099100130118     c                   exsr      RieVrbUni
099200121130     c                   update    tivrb000
099300121130     c                   endif
099400121130     c                   endsr
099500130118      ****************************************************
099600130118      ** Riempimento campi vrb per l'aggiornamento/write
099700130118      ** Record "VA"
099800130118      ****************************************************
099900130118     c     RieVrbUni     Begsr
100000130118     c                   time                    vrbtmst
100100130118     c                   eval      vrbdimp=w_vrbdimp
100200130118    3c                   select
100201170412     c* Distinguo peso/volume da destinatario perchè
100300170412     c* a causa della scelta in fatturazione del peso da tassare (vdl o
100301170412     c* bollettato/variato),memorizzare nel record "VA" il peso ricevuto per
100302170412     c* una variazione "I0" potrebbe generare una segnalazione di peso variato
100303170412     c* errata in quanto il peso non è stato variato ma la tassazione è avvenuta
100304170412     c* con il peso VDL
100309170412     c                   when      iv1cvb ='I0' or
100310170412     c                             iv1cvb ='I7' or
100311170412     c                             iv1cvb ='I8' or
100312170412     c                             iv1cvb ='I9'
100313170412     c                   movel     dtas_tasfin   vrbfin
100314170412     c                   movel     dtas_tascad   vrbcad
100315170412     c                   movel     dtas_tasnzd   vrbnzd
100316170412     c                   movel     dtas_tascts   vrbcts
100400130118     c                   when      %subst(iv1cvb:1:1)='I'
100500130118     c                   z-add     dtas_taspkf   vrbpkf
100600130118     c                   z-add     dtas_tasvlf   vrbvlf
100601160531     c                   movel     dtas_tasfvf   vrbfvf
100700170412     c****               movel     dtas_tasfin   vrbfin
100800170412     c****               movel     dtas_tascad   vrbcad
100900170412     c****               movel     dtas_tasnzd   vrbnzd
101000170412     c***                movel     dtas_tascts   vrbcts
101100130118     c* Variazioni codice bolla/contrassegno
101200130118     c                   when      %subst(iv1cvb:1:1)='K'
101300130118     c                   eval      vrbcas=dtas_tascas
101400130118     c                   eval      vrbvca=dtas_tasvca
101500130118     c                   eval      vrbtic=dtas_tastic
101600130118     c                   eval      vrbcmb=dtas_tascmb
101700130118     c                   move      dtas_tassta   vrbsta
101800130118     c* Variazioni Consegne particolari
101900130118     c                   when      %subst(iv1cvb:1:1)='C'
102000130118     c                   movel     dtas_tastc1   vrbtc1
102100130118     c                   movel     dtas_tastc2   vrbtc2
102200130118     c* Variazione Importo da assicurare
102300130118     c                   when      %subst(iv1cvb:1:1)='T'
102400130118     c                   movel     dtas_tasias   vrbias
102500130118     c                   movel     dtas_tasvas   vrbvas
102600130118     c* Variazione Volume VDL
102700130118     c                   when      iv1cvb='VV'
102800130118     c                   z-add     dtas_tasvlf   vrbvlf
102900130118     c                   move      §bvfvf        vrbfvf
103000130118     c                   z-add     §bvncr        vrbncr
103100130118     c                   z-add     §bvvlc        vrbvlc
103200130122     c* Variazione Peso   VDL
103300130122     c                   when      iv1cvb='PV'
103400130122     c                   z-add     dtas_taspkf   vrbpkf
103500130122     c                   move      §bvfpf        vrbfpf
103600130122     c                   z-add     §bvncp        vrbncp
103700130122     c                   z-add     §bvpkc        vrbpkc
103800130122     c* Variazione Inoltro Z/T
103900130123     c                   when      iv1cvb='ZT'
104000140123     c                   movel     dtas_tasfin   vrbfin
104100140123     c* Evento "RIC"
104200140123     c                   when      iv1cvb='RI'
104300140123     c                   movel     dtas_tasric   vrbric
104400130118    3c                   endsl
104500130118     c                   endsr
104600121130      ****************************************************
104700121130      ** Verifica differenze fra record "VA" e immagine precedente
104800121130      ****************************************************
104900121130     c     ver_diffe     Begsr
105000121130    1c                   select
105100121130     c* Variazioni Destinatario/peso/volume
105200130116     c                   when      %subst(iv1cvb:1:1)='I'
105300121130     c* . . . . Peso e volume da fatturare
105400151008     c*          N.B:Se tassato con peso desunto il peso (VRBPKF) del record " "
105500151008      *              e del record "VA" è sempre lo stesso e devo anche ingnorare
105600151008      *              il peso ricevuto nel record di variazione
105601160531
105602160531      *          IL TEST SU §AR5FPTAS<>'D' NON SERVE PIU' DA QUANDO SONO STATE
105603160531      *          AGGIUNTE LE VARIAZIONI PER IL VOLUME DESUNTO PER CUI NON
105604160531      *          RECEPIAMO PIÙ VARIAZIONI DI PESO/VOLUME SE UNO DEI DUE HA IL DE
105605160531      *          SUNTO. QUINDI IN QUESTI CASI LA PRIMA IF SEGUENTE NON SARÀ MAI
105606160531      *          SODDISFATTA
105700151008     c*
105800121130    2c                   if        vrbpkf<>dsva.vrbpkf or
105900121130     c                             vrbvlf<>dsva.vrbvlf
106000151008     C                   if        (§AR5FPTAS<>'D' and dsarbd.arbpkf<vrbpkf)
106100151008     C                             or dsarbd.arbvlf<vrbvlf
106200121130     c                   eval      dsva.vrbpkf=vrbpkf
106300121130     c                   eval      dsva.vrbvlf=vrbvlf
106301160531     c                   eval      dsva.vrbfvf=vrbfvf
106400121130     c                   else
106500151008     c                   if        §AR5FPTAS<>'D'
106600151008     c                   eval      dsva.vrbpkf=dsarbd.arbpkf
106700151008     c                   endif
106800130118     c                   eval      dsva.vrbvlf=dsarbd.arbvlf
106801160531     c                   eval      dsva.vrbfvf=dsarbd.arbfvf
106900121130     c                   endif
107000121130     c                   eval      wdiffe='1'
107100121130    2c                   endif
107200121130     c* . . . . Flag inoltro CAP e Nazione e Codice Tassazione
107300121130    2c                   if        vrbfin<>dsva.vrbfin or
107400121130     c                             vrbcad<>dsva.vrbcad or
107500121130     c                             vrbnzd<>dsva.vrbnzd or
107600121130     c                             vrbcts<>dsva.vrbcts
107700130118     c                   eval      dsva.vrbfin=dsarbd.arbfin
107800130118     c                   eval      dsva.vrbcad=dsarbd.arbcad
107900130118     c                   eval      dsva.vrbnzd=dsarbd.arbnzd
108000121130     c                   eval      dsva.vrbcts=dtas_tascts
108100121130     c                   eval      wdiffe='1'
108200121130    2c                   endif
108300121130     c* Variazioni codice bolla/contrassegno
108400130116     c                   when      %subst(iv1cvb:1:1)='K'
108500121203    2c                   if        vrbtic<>dsva.vrbtic or
108600121203     c                             vrbcas<>dsva.vrbcas or
108700121203     c                             vrbvca<>dsva.vrbvca or
108800121203     c                             vrbcmb<>dsva.vrbcmb or
108900121203     c                             vrbsta<>dsva.vrbsta
109000121203     c* stessa divisa o non c'era il contrassegno
109100121205    3c                   if        dtas_tasvca=vrbvca or vrbvca=*blanks
109200121130     c* se importo più basso del valore iniziale --> tengo l'iniziale
109300121205    4c                   if        (dtas_tassta='9' and vrbsta<>9) or
109400121205     c                             (dtas_tascas<vrbcas and
109500121205     c                             ((dtas_tassta='9' and vrbsta=9) or
109600121205     c                              (dtas_tassta<>'9' and vrbsta<>9)))
109700121130     c                   z-add     vrbcas        dsva.vrbcas
109800121130     c                   eval      dsva.vrbtic=vrbtic
109900121130     c                   eval      dsva.vrbvca=vrbvca
110000121130     c                   eval      dsva.vrbcmb=vrbcmb
110100121130     c                   eval      dsva.vrbsta=vrbsta
110200121130   x4c                   else
110300121130     c* se importo maggiore o uguale tengo la variazione
110400121205     c                   z-add     dtas_tascas   dsva.vrbcas
110500121205     c                   eval      dsva.vrbtic=dtas_tastic
110600121205     c                   eval      dsva.vrbvca=dtas_tasvca
110700121205     c                   eval      dsva.vrbcmb=dtas_tascmb
110800121205     c                   move      dtas_tassta   dsva.vrbsta
110900121130    4c                   endif
111000121130   x3c                   else
111100121130     c* divisa diversa imposto sempre come l'originale
111200121130     c                   z-add     vrbcas        dsva.vrbcas
111300121130     c                   eval      dsva.vrbtic=vrbtic
111400121130     c                   eval      dsva.vrbvca=vrbvca
111500121130     c                   eval      dsva.vrbcmb=vrbcmb
111600121130     c                   eval      dsva.vrbsta=vrbsta
111700121130    3c                   endif
111800121130     c                   eval      wdiffe='1'
111900121130    2c                   endif
112000121130     c* Variazioni Consegne particolari
112100130116     c                   when      %subst(iv1cvb:1:1)='C'
112200121130    2c                   if        (vrbtc1=dsva.vrbtc1 and vrbtc2=dsva.vrbtc2)
112300121130     c                              or
112400121130     c                             (vrbtc1=dsva.vrbtc2 and vrbtc2=dsva.vrbtc1)
112500121130   x2c                   else
112600130118     c                   eval      dsva.vrbtc1=dsarbg.arbtc1
112700130118     c                   eval      dsva.vrbtc2=dsarbg.arbtc2
112800121130     c                   eval      wdiffe='1'
112900121130    2c                   endif
113000121130     c* Variazione Importo da assicurare
113100130116     c                   when      %subst(iv1cvb:1:1)='T'
113200121203    2c                   if        vrbias<>dsva.vrbias or
113300121203     c                             vrbvas<>dsva.vrbvas
113400121203     c* stessa divisa o non c'era importo da assicurare
113500130124    3c                   if        dsarbt.arbvas=vrbvas or vrbvas=*blanks
113600130118    4c                   if        dsarbt.arbias<vrbias
113700121130     c                   eval      dsva.vrbias=vrbias
113800121130     c                   eval      dsva.vrbvas=vrbvas
113900121130   x4c                   else
114000130118     c                   eval      dsva.vrbias=dsarbt.arbias
114100130118     c                   eval      dsva.vrbvas=dsarbt.arbvas
114200121130    4c                   endif
114300121130     c* divisa diversa imposto sempre come record originale
114400121130   x3c                   else
114500121130     c                   eval      dsva.vrbias=vrbias
114600121130     c                   eval      dsva.vrbvas=vrbvas
114700121130    3c                   endif
114800121130     c                   eval      wdiffe='1'
114900121130    2c                   endif
115000130117     c* Variazioni Volume VDL
115100130117     c                   when      iv1cvb='VV'
115200130117     c                   if        vrbvlf<>dsva.vrbvlf
115300130118     C                   IF        §bvvlf<vrbvlf
115400130117     c                   eval      dsva.vrbvlf=vrbvlf
115500130118     c                   eval      dsva.vrbfvf=vrbfvf
115600130118     c                   eval      dsva.vrbvlc=vrbvlc
115700130118     c                   eval      dsva.vrbncr=vrbncr
115800130117     c                   else
115900130118     c                   eval      dsva.vrbvlf=§bvvlf
116000130118     c                   eval      dsva.vrbfvf=§bvfvf
116100130118     c                   eval      dsva.vrbvlc=§bvvlc
116200130118     c                   eval      dsva.vrbncr=§bvncr
116300130117     c                   endif
116400130117     c                   eval      wdiffe='1'
116500130117    2c                   endif
116600130122     c* Variazioni Peso   VDL
116700130122     c                   when      iv1cvb='PV'
116800130122     c                   if        vrbpkf<>dsva.vrbpkf  or
116900130122     c                             vrbpkc<>dsva.vrbpkc  or
117000130122     c                             vrbncp<>dsva.vrbncp
117100130122     C                   IF        §bvpkf<vrbpkf or
117200130122     C                             §bvpkc<vrbpkc or
117300130122     C                             §bvncp<vrbncp
117400130122     c                   eval      dsva.vrbpkf=vrbvlf
117500130122     c                   eval      dsva.vrbfpf=vrbfpf
117600130122     c                   eval      dsva.vrbpkc=vrbpkc
117700130122     c                   eval      dsva.vrbncp=vrbncp
117800130122     c                   else
117900130122     c                   eval      dsva.vrbpkf=§bvpkf
118000130122     c                   eval      dsva.vrbfpf=§bvfpf
118100130122     c                   eval      dsva.vrbpkc=§bvpkc
118200130122     c                   eval      dsva.vrbncp=§bvncp
118300130122     c                   endif
118400130122     c                   eval      wdiffe='1'
118500130122    2c                   endif
118600130122     c* Variazioni Inoltro Z/T
118700130123     c                   when      iv1cvb='ZT'
118800130122     c                   if        vrbfin<>dsva.vrbfin
118900130122     c                   eval      dsva.vrbfin=parfin
119000130206     c                   eval      wdiffe='1'
119100130122     c                   endif
119200140123     c* Variazioni evento RIC
119300140123     c                   when      iv1cvb='RI'
119400140123     c                   if        vrbric<>dsva.vrbric
119500140123     c                   eval      dsva.vrbric=parric
119600140123     c                   eval      wdiffe='1'
119700140123     c                   endif
119800121130    1c                   endsl
119900121130     c                   endsr
120000121114      ****************************************************
120100121114      ** Recupero data penultima fatturazione mensile
120200121114      ****************************************************
120300121114     c     rec_RPFdata   Begsr
120301170411     c                   clear                   wdataRPF
120400121114     c                   Clear                   Drpfd
120500130204     c                   Eval      KCod = 'RPF'
120600130204     c                   Movel(p)  'DATA'        KKe1
120700130204     c     ktbe          chain     tntbe01l
120800130204     c                   if        %found(tntbe01l)
120900130204     c                   Movel     Tbeuni        Drpfd
121000130204     c                   endif
121001170411     c* verifico se sped. export DpD
121002170411    1c                   if        §rpfdta>0
121003170411     c                   clear                   og143
121004170411     c     taslna        chain     azorg01l
121005170411    2c                   if        %found(azorg01l) and orgfva = *blanks
121006170411     c                   eval      og143 = orgde3
121007170411    2c                   endif
121008170411    2c     §ogntw        ifeq      'DPD'
121009170411     c* calcolo data di riferimento per bolle export dpd:
121010170411     c* data penultima fatturazione - 1 mese
121011170411       //   Decremento la data di un mese
121012170411             dataiso=%date(§rpfdta);
121013170411             dataiso=dataiso-%months(1);
121014170411             wdataRPF=%dec(dataiso);
121015170411     c                   else
121016170411     c                   eval      wdataRPF=§rpfdta
121017170411    2c                   endif
121018170411    1c                   endif
121100121114     c                   endsr
121200981001     C*
