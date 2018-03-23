000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060207      * Il pgm deve isolare i conteggi confermati dal 2006/02/03 al 2006/02/06
000500060125      *
000501060207      * ?SELECT * from FIFTT00F where .....
000900060125      *
000901060207     F*---------------------------------------------------------------------------------
000902060207     fWxftdarb  o  a e           k disk
000903060207     F*---------------------------------------------------------------------------------
000904060207     ffiftt01l  if   e           k disk
000905060207     ffiftd01l  if   e           k disk
000906060207     ffnarb01l  if   e           k disk
000907060207     fFIAR901L  if   e           k disk
000908060207     fFIARi01L  if   e           k disk
000909060207     fFNEVB01L  if   e           k disk
000910060207     ffnblp01l  if   e           k disk
000911060207     FTABEL00F  IF   E           K DISK
003400030429     F*---------------------------------------------------------------------------------
007206060207     D pdr             S                   like(fttpdr)
007207060207     D ndc             S                   like(fttndc)
007208060207     D ddc             S                   like(fttddc)
007209060207     D tsr             S                   like(ftttsr)
007210060207     D wtic            S                   like(AR9TIC)
007211060207     D wcas            S                   like(AR9CAS)
007212060207     D wvca            S                   like(AR9VCA)
007213060207     D wifp            S                   like(ARIIMP)
007214060207     D wtmc            S                   like(ftdtmc)
007215060207     D wcmc            S                   like(evbcev)
007216060207     D ds2a          e ds
007800020205     c**********************************************************************
007801060207     c     *entry        plist
007802060207     c                   parm                    pdr
007803060207     c                   parm                    tsr
007805060207     c                   parm                    ddc
007806060207     c                   parm                    ndc
007807060207      *
007808060207     c                   if        tsr <> *blank
007809060207      *
007824060207     c     kftt          setll     fiftt01l
007825060207     c     kftt          reade     fiftt01l
007826060207     C                   DOw       not %EoF(fiftt01l)
007827060207      *
007828060207     c                   exsr      bolle
007829060207      *
007830060207     c     kftt          reade     fiftt01l
007831060207     c                   enddo
015000030429      *
015001060207     c                   end
015002060207      *
015100000607     c                   seton                                        lr
031500020205     C**-------------------------------------------------------------**
034900060207     c     Bolle         begsr
034901060207     c*
034902060207      * ?      Dettagli  x PDR/TSR/NDC/DDC
034903060207     C     Kftd          setll     Fiftd01l
034904060207     C     Kftd          reade     Fiftd01l
034905060207     c                   dow       not %EoF(Fiftd01l)
034907060207      *
034908060207     c                   if        tsr = 'C'
034909060207     c                   exsr      Bolle_arrivi
034910060207     c                   end
034911060207      *
034912060207     c                   if        tsr = 'R'
034913060207     c                   exsr      Bolle_parten
034914060207     c                   end
034915060207      *
034916060207     C     Kftd          reade     Fiftd01l
034917060207     c                   EndDO
034918060207      *
034919060207     c                   endsr
034920060207     C**-------------------------------------------------------------**
034921060207     c     Bolle_arrivi  begsr
034922060207      *
034923060207      *  se cambia qualcosa --> flag di controllo
034924060207     c                   move      'N'           cambia            1
034925060207      *
034926060207      * aggancia la bolla
034927060207     c     karb          chain     fnarb01l
034928060207     c                   if        %Found(fnarb01l)
034929060207      **
034930060207     c                   clear                   wtic
034931060207     c                   clear                   wcas
034932060207     c                   clear                   wvca
034933060207     c                   clear                   wifp
034935060207      **  evento di non consegna con nr.distinta
034936060207     c                   clear                   evbndc            7
034937060207     c                   clear                   evbpdr            7
034938060207     c                   clear                   wcmc
034939060207     c                   clear                   wtmc
034940060207      **
034941060207      *  se consegnata con stesso nr.distinta
034942060207     c                   if        arbdcm > 0 and arbndc = ftdndc
034943060207      **
034944060207      *  tipo incasso/contrassegno/valore
034945060207     c     karb          chain     FIAR901L
034946060207     c                   if        %Found(FIAR901L)
034947060207     c                   move      AR9TIC        wtic
034948060207     c                   z-add     AR9CAS        wcas
034949060207     c                   move      AR9VCA        wvca
034950060207     c                   end
034951060207      **
034952060207      *  importo
034953060207     c     kari          chain     FIARi01L
034954060207     c                   if        %Found(FIARi01L)
034955060207     c                   z-add     ARiIMP        wifp
034956060207     c                   end
034957060207      **
034958060207      *  la data distinta ??
034959060207     c                   if        arbdcm <> ftddcm
034960060207     c                   move      'S'           cambia
034961060207     c                   end
034962060207      **
034963060207      *  l'ora   distinta ??
034964060207     c                   if        arbhmc <> ftdhmc
034965060207     c                   move      'S'           cambia
034966060207     c                   end
034967060207      **
034968060207      *  il nr.stop       ??
034969060207     c                   if        arbstp <> ftdstp
034970060207     c                   move      'S'           cambia
034971060207     c                   end
034972060207      **
034973060207      *  la sequenza      ??
034974060207     c                   if        arbnss <> ftdnss
034975060207     c                   move      'S'           cambia
034976060207     c                   end
034977060207      **
034978060207     c                   else
034979060207      **
034980060207      **  SE invece non è stata consegnata o ha un altro nr.distinta.
034981060207      **
034982060207     C     Karb          setll     Fnevb01l
034983060207     C     Karb          reade     Fnevb01l
034984060207     c                   dow       not %EoF(Fnevb01l)
034985060207      *
034986060207     c                   if        evbcev <> 'MIC' and evbnot <> *blank
034987060207      *
034988060207     c                   movel     evbnot        field14          14
034989060207     c                   movel     field14       evbndc            7
034990060207     c                   move      field14       evbpdr            7
034991060207      *
034992060207     c                   move      ftdpdr        pdralf            7
034993060207     c                   move      ftdndc        ndcalf            7
034994060207      *
034995060207     c                   if        pdralf = evbpdr and ndcalf = evbndc
034996060207     c                   if        evbcev = 'RIC'
034997060207     c                   move      'AVV'         evbcev
034998060207     c                   end
034999060207     c                   move      evbcev        wcmc
035000060207     c                   leave
035001060207     c                   end
035002060207      *
035003060207     c                   end
035004060207      *
035005060207     C     Karb          reade     Fnevb01l
035006060207     c                   endDO
035007060207      **
035008060207      * codice di non consegna ??
035009060207     c                   if        wcmc   <> ftdcmc
035010060207     c                   move      'S'           cambia
035011060207     c                   end
035012060207      **
035013060207     c                   clear                   wtmc
035014060207     c                   if        wcmc   <> *blank
035015060207     C                   MOVEL     '2A'          KCOD
035016060207     C                   MOVEL(p)  wcmc          Kkey
035017060207     C     KTAB          CHAIN     TABEL00F
035018060207     c                   if        %Found(TABEL00F)
035019060207     c                   movel     tbluni        ds2a
035020060207     c                   move      §2AFTC        wtmc
035021060207     c                   end
035022060207     c                   end
035023060207      **
035024060207      * Tipo mancata  consegna ??
035025060207     c                   if        wtmc   <> ftdtmc
035026060207     c                   move      'S'           cambia
035027060207     c                   end
035028060207      **
035029060207     c                   end
035030060207      **-----
035031060207      **
035032060207     c                   clear                   wpes
035033060207     c                   clear                   wvol
035034060207     C* Calcolo Volume
035035060207     C                   EXSR      CALVOL
035036060207     C* Calcolo peso
035037060207     C                   exsr      calPES
035042060207      *
035043060207     c                   if        ftdrsc <> arbrsd or
035044060207     c                             ftdset <> 1      or
035045060207     c                             ftdncl <> arbncl or
035046060207     c                             ftdpkl <> wpes   or
035047060207     c                             ftdvlu <> wvol   or
035048060207     c                             ftdTIC <> wTIC   or
035049060207     c                             ftdCAS <> wCAS   or
035050060207     c                             ftdVCA <> wVCA   or
035051060207     c                             ftdifp <> wifp   or
035052060207     c                             ftdcap <> arbcad or
035053060207     c                             ftdfin <> arbfin or
035054060207     c                             ftdatb <> *blank
035055060207     c                   move      'S'           cambia
035056060207     c                   end
035057060207      *
035058060207      *  Solo se consegna parziale
035059060207     c                   if        arbdcp > 0
035060060207     c                   if        ftdncl <  arbncl or
035061060207     c                             ftdpkl <  wpes   or
035062060207     c                             ftdvlu <  wvol
035063060207     c                   move      'N'           cambia
035064060207     c                   end
035065060207     c                   end
035066060207      *
035067060207     c                   if        cambia ='S'
035068060207     c                   write     ftdarb00
035069060207     c                   end
035070060207      *
035071060207     c                   end
035072060207      *
035073060207     c                   endsr
035074060207     C**-------------------------------------------------------------**
035075060207     c     Bolle_parten  begsr
035076060207      *
035077060207     c     kblp          chain     fnblp01l
035078060207     c                   if        %Found(fnblp01l)
035079060207     c                   end
035080060207      *
035081060207     c                   endsr
035082060207     C**-------------------------------------------------------------**
035083060207     c     *inzsr        begsr
035303060127      *----------*
035304060207     c     karb          klist
035305060207     c                   kfld                    ftdaas
035306060207     c                   kfld                    ftdlnp
035307060207     c                   kfld                    ftdnrs
035308060207     c                   kfld                    ftdnsp
035309060207      *----------*
035310060207     c     kari          klist
035311060207     c                   kfld                    ftdaas
035312060207     c                   kfld                    ftdlnp
035313060207     c                   kfld                    ftdnrs
035314060207     c                   kfld                    ftdnsp
035315060207     c                   kfld                    tipoa             1
035316060207     c                   move      'A'           tipoa
035317060207      *----------*
035318060207     c     kblp          klist
035319060207     c                   kfld                    ftdaas
035320060207     c                   kfld                    ftdlnp
035321060207     c                   kfld                    ftdnrs
035322060207     c                   kfld                    ftdnsp
035323060207      *----------*
035324060207     c     kftd          klist
035325060207     c                   kfld                    fttpdr
035326060207     c                   kfld                    ftttsr
035327060207     c                   kfld                    fttndc
035328060207     c                   kfld                    fttddc
037600030429      *----------*
040713060207     c     kftt          klist
040714060207     c                   kfld                    pdr
040715060207     c                   kfld                    tsr
040716060207     c                   kfld                    ndc
040717060207     c                   kfld                    ddc
040718060207     C*
040719060207     C     KTAB          KLIST
040720060207     C                   KFLD                    KKUT
040721060207     C                   KFLD                    KCOD
040722060207     C                   KFLD                    KKEY
040723060207     C                   Z-ADD     1             KKUT
040724060207      *----------*
040725060207     C     *LIKE         DEFINE    TBLKUT        KKUT
040726060207     C     *LIKE         DEFINE    TBLCOD        KCOD
040727060207     C     *LIKE         DEFINE    TBLKEY        KKEY
040728060207     C     *LIKE         DEFINE    ARBVLF        WVOL
040729060207     C     *like         define    arbpkF        wpes
041001060127      *----------*
041400020402     c                   endsr
041401060207      *---------------------------------------------------------*
041402060207      *  Determinazione del volume da utilizzare
041403060207      *---------------------------------------------------------*
041404060207     C     CALVOL        BEGSR
041405060207      *
041406060207     C*  determino volume da utilizzare
041407060207     C     ARBFVF        IFEQ      'T'
041408060207     C                   Z-ADD     ARBVLF        WVOL
041409060207     C                   ELSE
041410060207     C     ARBNCR        ifeq      ARBNCL
041411060207     C                   Z-ADD     ARBVLC        WVOL
041412060207     C                   ELSE
041413060207     C     ARBVLC        IFGE      ARBVLF
041414060207     C                   Z-ADD     ARBVLC        WVOL
041415060207     C                   ELSE
041416060207     C                   Z-ADD     ARBVLF        WVOL
041417060207     C                   ENDIF
041418060207     C                   ENDIF
041419060207     C                   ENDIF
041420060207     C*
041421060207     C                   ENDSR
041422060207      *---------------------------------------------------------*
041423060207      *  Determinazione del peso da utilizzare
041424060207      *---------------------------------------------------------*
041425060207     C     calPES        BEGSR
041426060207      *
041427060207      * se è considerata un'unica spedizione
041428060207     C     arbncp        ifeq      arbncl
041429060207     C                   Z-ADD     arbPKC        wpes
041430060207     C*
041431060207     C                   ELSE
041432060207     C*
041433060207     C     arbPKC        ifge      arbPKF
041434060207     C                   Z-ADD     arbPKC        wpes
041435060207     C                   else
041436060207     C                   Z-ADD     arbPKF        wpes
041437060207     C                   end
041438060207     C*
041439060207     C                   ENDIF
041440060207     C*
041441060207     C                   ENDSR
041442060207      *---------------------------------------------------------*
