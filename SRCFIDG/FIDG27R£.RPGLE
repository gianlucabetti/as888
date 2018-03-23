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
024458070306     c* per il peso e il volume in caso di bancali ... ce ne freghiamo
024468070302     C* Calcolo Volume
024473070302     c                   if        arbncl <> 0
024474070302     C                   EXSR      CALVOL
024475070307     c                   select
024476070307     c                   when      d27nclco = arbncl
024477070307     c                   eval      d27vluco = wvol
024478070307     c                   when      d27ncldco = arbncl
024479070307     c                   eval      d27vludco = wvol
024480070307     c                   other
024481070302     C     wvol          div(h)    arbncl        APPOG            18 3
024482070302     C     APPOG         mult      d27NCLco      d27Vluco
024483070302     C     APPOG         mult      d27NCLdco     d27Vludco
024484070307     c                   endsl
024485070307     c                   end
024487070302     C* Calcolo peso
024492070302     c                   if        arbncl <> 0
024494070302     C                   exsr      calPES
024495070307     c                   select
024496070307     c                   when      d27nclco = arbncl
024497070307     c                   eval      d27pklco = wpes
024498070307     c                   when      d27ncldco = arbncl
024499070307     c                   eval      d27pkldco = wpes
024500070307     c                   other
024501070302     C     wpes          div(h)    arbncl        APPOG            18 3
024502070302     C     APPOG         mult      d27NCLco      d27pklco
024503070302     C     APPOG         mult      d27NCLdco     d27pkldco
024504070307     c                   endsl
024506070302     c                   end
024507070302     c*
024508070302     c                   eval      kpjbu = fidg27ds
024509070302     c                   if        d27telai= 'C' or d27telai= ' '
024510070302     c                   seton                                        lr
024511070302     c                   else
024512070302     c                   seton                                        rt
024513070302     c                   end
024514070302      *---------------------------------------------------------------*
024515070302     c     srinz         begsr
024516070302      *---------------------------------------------------------------*
024517941123      *  Operazioni iniziali
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
024814070302     c                   if        %parms=1 or
024815070302     c                             $arbnsp = 0
024816070302     c                   if        not %open(fnarb01l)
024817070302     c                   open      fnarb01l
024818070302     c                   end
024819070302     c     karb          chain     fnarb01l
024820070302     c                   if        not %found(fnarb01l)
024821070302     c                   clear                   fnarbds
024822070302     c                   else
024823070302     c                   eval      fnarbds = fnarbds£
024824070302     c                   end
024825070302     c                   end
024826070302     c                   if        (%parms > 1 and $arbnsp <>0)
024827070302     c                   eval      fnarbds = fnarbds$
024828070302     c                   eval      d27aasi = arbaas
024829070302     c                   eval      d27lnpi = arblnp
024830070302     c                   eval      d27nrsi = arbnrs
024831070302     c                   eval      d27nspi = arbnsp
024832070302     c                   end
024833070322     c* data
024834070322     c                   if        d27dfvi= 0
024835070322     c                   move      udate         dtaiso
024836070322     c                   else
024837070322     c                   move      d27dfvi       dtaiso
024838070322     c                   end
024839070322     c                   move      dtaiso        oggi
024840070302     c                   endsr
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
