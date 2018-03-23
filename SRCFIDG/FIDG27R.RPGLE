000200920914     H DECEDIT('0,') DATEDIT(*DMY.)
000201070322     H* FIDG27R *----------------------------------------------------*
000202070322     H* Calcola peso/vol./colli Cons./da Cons. in base CML x spedizione
000203070322     H*--------------------------------------------------------------*
002000070302     FFNARB01L  iF   E           K DISK    USROPN prefix(£)
002001070515     Ffnlbl01l  IF   E           K DISK    rename(fnlbl000: fnlbl001)
015200070302     D kpjba         E DS
015201070302     D fidg27ds      E DS
015202070322     d fnlr76ds      e ds
015203070302     d fnarbds       e ds                  extname(fnarb00F)
015204070302     d fnarbds£      e ds                  extname(fnarb00F) prefix(£)
015205070302     d fnarbds$      e ds                  extname(fnarb00F) prefix($)
015206070827     d fnarbdsc      e ds                  extname(fnarb00F) prefix(c)
015208070302     d wpes            s                   like(arbpkf)
015209070302     d wvol            s                   like(arbvlf)
015210070322     D dtaiso          S               d
015212070322     D oggi            S              8  0
024200911028      *---------------------------------------------------------------*
024300911028      * FLUSSO PRINCIPALE                                             *
024400911028      *---------------------------------------------------------------*
024401070302     c                   exsr      srinz
024402070322     c* chiamo FNLR76R per sapere con quanti colli vado in consegna
024403070322     c                   clear                   fnlr76ds
024404070830     c                   movel     d27dtai       dssoglia
024405070830     c                   move      d27orai       dssoglia
024406070515     c                   eval      ILR76DCO= oggi
024407070515     c                   eval      ILR76SPPAR='S'
024408070515     c* controllo se ho un legame e se stessa linea di arrivo,
024409070515     c* nel caso passo la chiave della mamma ma non la ds di arb. Sarà
024410070515     c* il pgm a recuperarsela
024411070515     C     Karb          CHAIN     FNLBL01L
024412070515     C                   IF        %found(fnlbl01l) and lbllap = lbllan
024413071129     c                             and arbfns = ' ' and arbncd = 0
024414070515     c                   eval      ILR76AAS= LBLAAP
024415070515     c                   eval      ILR76LNP= LBLLPP
024416070515     c                   eval      ILR76NRS= LBLNRP
024417070515     c                   eval      ILR76NSP= LBLNSP
024418070827     c                   clear                   fnarbdsc
024419070515     c                   call      'FNLR76R'
024420070515     c                   parm                    fnlr76ds
024421070827     c                   parm                    fnarbdsc
024422070830     C                   PARM                    dsSoglia         12 0
024423070515     c                   else
024424070322     c                   eval      ILR76AAS= arbaas
024425070322     c                   eval      ILR76LNP= arblnp
024426070322     c                   eval      ILR76NRS= arbnrs
024427070322     c                   eval      ILR76NSP= arbnsp
024428070322     c                   call      'FNLR76R'
024429070322     c                   parm                    fnlr76ds
024430070322     c                   parm                    fnarbds
024432070830     C                   PARM                    dsSoglia         12 0
024433070515     c                   end
024434070322     c*
024453070322     C                   eval      d27ncldco = OLR76CDACO
024455070322     C                   eval      d27nclco = OLR76CDCM
024456150515     c* solo se tutti i colli sono in pistola 88
024457150515     c                   if        olr76cdc88 = olr76cdaco and
024458150515     c                             olr76cdc88 <> 0
024459150515     C                   eval      d27ncl88O = OLR76CDC88
024460150515     c                   end
024461070306     c* per il peso e il volume in caso di bancali ... ce ne freghiamo
024468070302     C* Calcolo Volume
024473070302     c                   if        arbncl <> 0
024474070302     C                   EXSR      CALVOL
024475070307     c                   select
024476070307     c                   when      d27nclco = arbncl
024477070307     c                   eval      d27vluco = wvol
024478070307     c                   when      d27ncldco = arbncl
024479070307     c                   eval      d27vludco = wvol
024482070307     c                   other
024483070302     C     wvol          div(h)    arbncl        APPOG            18 3
024484070302     C     APPOG         mult      d27NCLco      d27Vluco
024485070302     C     APPOG         mult      d27NCLdco     d27Vludco
024486070307     c                   endsl
024487150515     c* volume colli pistola 88
024488150515     c                   if        d27ncl88o <> 0
024489150515     C     wvol          div(h)    arbncl        APPOG
024491150515     C     APPOG         mult      d27NCL88o     d27Vlu88o
024492070307     c                   end
024493150515     c                   end
024494070302     C* Calcolo peso
024495070302     c                   if        arbncl <> 0
024496070302     C                   exsr      calPES
024497070307     c                   select
024498070307     c                   when      d27nclco = arbncl
024499070307     c                   eval      d27pklco = wpes
024500070307     c                   when      d27ncldco = arbncl
024501070307     c                   eval      d27pkldco = wpes
024502070307     c                   other
024503070302     C     wpes          div(h)    arbncl        APPOG            18 3
024504070302     C     APPOG         mult      d27NCLco      d27pklco
024505070302     C     APPOG         mult      d27NCLdco     d27pkldco
024506070307     c                   endsl
024507150515     c* peso colli pistola 88
024508150515     c                   if        d27ncl88o <> 0
024509150515     C     wpes          div(h)    arbncl        APPOG
024510150515     C     APPOG         mult      d27NCL88o     d27pkl88o
024511150515     c                   end
024512070302     c                   end
024513070302     c*
024514070302     c                   eval      kpjbu = fidg27ds
024515070302     c                   if        d27telai= 'C' or d27telai= ' '
024516070302     c                   seton                                        lr
024517070302     c                   else
024518070302     c                   seton                                        rt
024519070302     c                   end
024520070302      *---------------------------------------------------------------*
024521070302     c     srinz         begsr
024522070302      *---------------------------------------------------------------*
024523941123      *  Operazioni iniziali
024600941124     C     *ENTRY        PLIST
024700941124     C                   PARM                    KPJBA
024800070302     C                   PARM                    fnarbds$
024801070302     c                   movel     kpjbu         fidg27ds
024802070302     C     Karb          KLIST
024803070302     C                   KFLD                    d27aasi
024804070302     C                   KFLD                    d27lnpi
024805070302     C                   KFLD                    d27nrsi
024806070302     C                   KFLD                    d27nspi
024807070302     c* pulisco i dati di output
024808070302     c                   clear                   d27pklco
024809070302     c                   clear                   d27vluco
024810070302     c                   clear                   d27nclco
024811070302     c                   clear                   d27pkldco
024812070302     c                   clear                   d27vludco
024813070302     c                   clear                   d27ncldco
024814150515     c                   clear                   d27pkl88o
024815150515     c                   clear                   d27vlu88o
024816150515     c                   clear                   d27ncl88o
024817070302     c                   if        %parms=1 or
024818070302     c                             $arbnsp = 0
024819070302     c                   if        not %open(fnarb01l)
024820070302     c                   open      fnarb01l
024821070302     c                   end
024822070302     c     karb          chain     fnarb01l
024823070302     c                   if        not %found(fnarb01l)
024824070302     c                   clear                   fnarbds
024825070302     c                   else
024826070302     c                   eval      fnarbds = fnarbds£
024827070302     c                   end
024828070302     c                   end
024829070302     c                   if        (%parms > 1 and $arbnsp <>0)
024830070302     c                   eval      fnarbds = fnarbds$
024831070302     c                   eval      d27aasi = arbaas
024832070302     c                   eval      d27lnpi = arblnp
024833070302     c                   eval      d27nrsi = arbnrs
024834070302     c                   eval      d27nspi = arbnsp
024835070302     c                   end
024836070322     c* data
024837070322     c                   if        d27dfvi= 0
024838070322     c                   move      udate         dtaiso
024839070322     c                   else
024840070322     c                   move      d27dfvi       dtaiso
024841070322     c                   end
024842070322     c                   move      dtaiso        oggi
024843070302     c                   endsr
418200941206      *---------------------------------------------------------*
418300941206      *  Determinazione del volume da utilizzare
418400941206      *---------------------------------------------------------*
418500941206     C     CALVOL        BEGSR
418501070305     c* t=volume da fatturare
418700941206     C     ARBFVF        IFEQ      'T'
418800941206     C                   Z-ADD     ARBVLF        WVOL
418900941206     C                   ELSE
419000050208     C     ARBNCR        ifeq      ARBNCL
419100941206     C                   Z-ADD     ARBVLC        WVOL
419200941206     C                   ELSE
419300941206     C     ARBVLC        IFGE      ARBVLF
419400941206     C                   Z-ADD     ARBVLC        WVOL
419500941206     C                   ELSE
419600941206     C                   Z-ADD     ARBVLF        WVOL
419700941206     C                   ENDIF
419800941206     C                   ENDIF
419900941206     C                   ENDIF
420000941206     C*
420100941206     C                   ENDSR
420200021107      *---------------------------------------------------------*
420300021107      *  Determinazione del peso da utilizzare
420400021107      *---------------------------------------------------------*
420500021107     C     calPES        BEGSR
420800050208     C     arbncp        ifeq      arbncl
420900021107     C                   Z-ADD     arbPKC        wpes
421100021107     C                   ELSE
421300021107     C     arbPKC        ifge      arbPKF
421400021107     C                   Z-ADD     arbPKC        wpes
421500021107     C                   else
421600021107     C                   Z-ADD     arbPKF        wpes
421700021107     C                   end
421900021107     C                   ENDIF
422100021107     C                   ENDSR
