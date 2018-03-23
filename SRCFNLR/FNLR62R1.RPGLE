000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000915     ffncde00f  uf a e             disk    usropn
000300000901     ffnarb54l  if   e           k disk
000400001018     ftabel00f  if   e           k disk
000500000901     fazorg01l  if   e           k disk
000600000517      *--------------------------------------------------------------------
000700000522     D kpjba         e ds
000800000901     D ds3a          e ds
000900000907     D DS4G          E DS
001000000901     D og143         e ds
001100000907     d dsarbd        e ds
001200000907     d dsarbg        e ds
001300000907     d darbt         e ds
001400000901     d dsarbk        e ds
001500000901     d ds48          e ds                           extname(FNLR48DS)
001600000907     D FNLR63DS      E DS
001700000912     D Fidn43ds      E DS
001800020827     d trul33ds      e ds
001900020903     d fnlv55ds      e ds
002000020903     d fnlr6ds       e ds
002100040716     d fidn12ds      e ds
002200020827     d trul06ds      e ds
002300020506     d  lin                    1     90  0 dim(30)
002400001018      *ds entrata parametri
002500000901     D parin           ds
002600000901     d  parfil                        3s 0
002700000901     d  paropz                        1
002800000901     d  pardat                        8s 0
002900000922     d  paranm                        1
003000001018     d  pardtf                        8s 0
003100001026     d  parcmc                        3
003200001206     d  parpog                        3s 0
003300001206     d  parnfv                        6s 0
003400001018      *variabili
003500001018     d
003600001018     d parkey          s                   like(parfil)
003700000907     d datasped        s              8  0
003800000901     d data0           s              8  0 inz(0)
003900000901     d x               s              3  0 inz(0)
004000001018     d y               s              3  0 inz(0)
004100000901     d fip             s              3  0 dim(50)
004200000907     d c3a             s              2    dim(100)
004300000901     d d3a             s             71    dim(100)
004400020904     D*L6              S              3  0 DIM(30)                              FIL GEST ARRIV
004500020506     D*FIL             S              3    DIM(29)                              FIL. COMODO
004600001018     d datalow         s              8  0 inz(00010101)
004700000922     d wdatag          s               d   datfmt(*iso)
004800000922     d parfilcda       s              6    inz('   CDA')
004900000925     d flgerr          s                   like(cdeerr)
005000001019     d record          s              4  0 inz(0)
005100001026     d aperta          s                   like(cdeerr)
005200071018     D Avvisa_CED      C                   CONST('Avvisa CED: Manca tab.:4G -
005300071018     D                                     x Filiale. Risp.->Enter')
005400000517      *--------------------------------------------------------------------
005500000517     c
005600000912      *parzializzazione lancio tipo 4 C.A.
005700001018      *richiama programma caricamento fncde da gestione dei danni
005800000918     c                   if        paropz = '4'
005900000918     c                   clear                   fidn43ds
006000000918     c                   move      '§'           i43dst
006100000918     c                   z-add     parfil        i43fil
006200000918     c                   z-add     pardat        i43amgdha
006300001018     c                   z-add     pardtf        i43amgdhd
006400001026     c                   move      paranm        i43ind
006500001026     c                   move      parcmc        i43are
006600000918     c                   movel     fidn43ds      kpjbu
006700000918     c                   call      'FIDN46R'
006800000918     c                   parm                    kpjba
006900001027      * rientro da programma chiamato e verifico se ho scritto record avendo
007000001027      * forzato dall'altra parte il numero record scritti nel campo filiale
007100001027     c                   movel     kpjbu         fidn43ds
007200001027     c                   z-add     i43fil        record
007300000912
007400000912      * altre parzializzazioni
007500000912     c                   else
007600001018      *estraggo da fnarb le bolle da chiudere riportandole su fncde
007700000901     c     karb          setll     fnarb54l
007800000517     c                   do        *hival
007900000901     c     karb          reade     fnarb54l                               99
008000001019      *Nr massimo di record scritti su fncde da riportare in sfl o fine file
008100001019     c                   if        *in99 or record = 9999
008200001019     c                   leave
008300001019     c                   endif
008400000901     c                   movel     arbaas        datasped
008500000901     c                   move      arbmgs        datasped
008600001018      * numero distinta già presente
008700001018     c                   if        arbndc > 0
008800001018     c                   iter
008900001018     c                   endif
009000010109      * data spedizione oltre il limite non per poste
009100000901     c                   if        datasped > pardat
009200000901     c                   iter
009300000901     c                   endif
009400010109
009500001026      *
009600071128      * RICERCA DELLE CA LEGATE
009700040716      * SIA COME MAMMA CHE COME FIGLIA
009800071128     c                   move      *blank        aperta
009900040716     c                   clear                   fidn12ds
010000040716     c                   eval      i12aas = arbaas
010100040716     c                   eval      i12lnp = arblnp
010200040716     c                   eval      i12nrs = arbnrs
010300040716     c                   eval      i12nsp = arbnsp
010400040716     c                   eval      i12fel = 'E'
010500040716     c                   eval      i12fan = 'E'
010600040716     c                   eval      i12fch = 'E'
010700040716     c                   call      'FIDN12R'
010800040716     c                   parm                    fidn12ds
010900040716      **
011000040716      * se c'è almeno una C.A. aperta
011100040716      **
011200040716     c     o12nca        ifgt      0
011300040716     c                   move      'X'           aperta
011400040716     c                   endif
011500001026     c                   if        aperta <> *blank
011600001026     c                   iter
011700001026     c                   end
011800000901      *
011900000901     c                   select
012000000901      *selezione 1
012100001018      *gestione filiali DpD
012200000901     c                   when      paropz = '1'
012300000901      * test sono buone solo le bolle di rientro
012400000911     c                   if        arbfbr <> 'S'
012500000901     c                   iter
012600000901     c                   end
012700000901      *selezione 3
012800001018      *gestione filiali estere
012900000901     c                   when      paropz = '3'
013000000901      *nessuna parzializzazione oltre alla data e alla filiale estera
013100000912      * già filtrata nel lancio
013200011211
013300011211      *selezione 5
013400011211      *gestione filiali DpD
013500011211     c                   when      paropz = '5'
013600011211      * test sono buone solo le bolle di rientro
013700011211     c                   if        arbfbr = 'S'
013800011211     c                   iter
013900011211     c                   end
014000071108      *selezione 6
014100071108      *gestione rimpalli sede filiale 101
014200071108     c                   when      paropz = '6'
014300071108     c                   move      *blank        flgerr
014400071108     c                   if        arbcbo = '1 ' or arbcbo = '2 '
014500071108     c                   exsr      trasforma
014600071108     c                   if        d48err <> *blank
014700071108     c                   move      'C'           flgerr
014800071108     c                   endif
014900071108     c                   endif
015000000901     c                   endsl
015100000901      * record ok da scrivere su fncde
015200001019     c                   add       1             record
015300000907     c                   exsr      scrivi
015400000901     c                   enddo
015500001018      *fine loop filiali in gestione
015600020903     c*                  enddo
015700000912     c                   endif
015800000907     c                   close     fncde00f
015900000901     c*
016000001026     c                   if        record > 0
016100000912     c                   exsr      richiama
016200001026     c                   else
016300001026     c                   movel(p)  'A'           kpjbu
016400001026     c                   end
016500000912     c*
016600000901     c                   seton                                        lr
016700071108?    C*-----------------------------------------------------------
016800071108?    C* trasforma .. bolla da franco/assegnato a C/S
016900071108?    C*-----------------------------------------------------------
017000071108     C     trasforma     begsr
017100071108     C                   CLEAR                   DS48
017200071108     C                   CLEAR                   DSARBK
017300071108     C                   MOVE      arbaas        D48AAS
017400071108     C                   MOVE      arblnp        D48LNP
017500071108     C                   MOVE      arbnrs        D48NRS
017600071108     C                   MOVE      arbnsp        D48NSP
017700071108     C                   MOVE      'K8'          D48CVB
017800071108     C                   MOVE      'E'           D48FFR
017900071108     C                   MOVE      'K'           D48TRC
018000071108     C                   MOVE      'A'           D48TBO
018100071108      * franco / assegnato
018200071108     c                   if        arbcbo = '1 '
018300071112     C                   MOVE      'A '          §bkcbn
018400071108     c                   else
018500071112     C                   MOVE      'Z '          §bkcbn
018600071108     c                   endif
018700071108     C                   MOVEL     DS48          KPJBU
018800071108     C                   CALL      'FNLR48R'
018900071108     C                   PARM                    KPJBA
019000071108     C                   PARM                    DSARBD
019100071108     C                   PARM                    DSARBK
019200071108     C                   PARM                    DARBT
019300071108     C                   PARM                    DSARBG
019400071108     C                   MOVEL     KPJBU         DS48
019500071108?    C*
019600071108     c                   endsr
019700000901?    C*-----------------------------------------------------------
019800000901?    C* scrivi - scrive record fncde00f da fnarb
019900000901?    C*-----------------------------------------------------------
020000000901     C     scrivi        begsr
020100000901     c                   clear                   fncde000
020200000901     c                   z-add     arbaas        cdeaas
020300000901     c                   z-add     arblnp        cdelnp
020400000901     c                   z-add     arbnrs        cdenrs
020500000901     c                   z-add     arbnsp        cdensp
020600000901     c                   z-add     arblna        cdelna
020700000901     c                   z-add     arbrmn        cdermn
020800001026     c                   move      parcmc        cdecmc
020900000925     c                   move      flgerr        cdeerr
021000000901     c                   move      arbcca        cdecca
021100000922      * forzo data consegna
021200001018     c                   if        pardtf > datalow
021300001018     c                   move      pardtf        cdedcm
021400001018     c                   else
021500001018     c                   move      wdatag        cdedcm
021600001018     c                   end
021700000901     c                   write     fncde000
021800000901     c                   endsr
021900000907      *---------------------------------------------------------*
022000000912      *  richiama pgm gestione chiusura distinta                *
022100000907      *---------------------------------------------------------*
022200000912     C     richiama      BEGSR
022300020904     c* reperisce il po di gestione
022400020904     c                   exsr      srfgs
022500000907      *
022600000907     C* Aggancio la tabella 4G x reperire i dati del padroncino,
022700000907     C* l'ora di consegna, ecc.. per le distinte delle linee
022800000907     C* estere
022900000907     C                   Z-ADD     1             KKUT
023000000907     C                   MOVEL     '4G'          KCOD
023100000922     C                   MOVEL     *BLANKS       KKEY
023200020904     c                   movel     D55TFA        parfilcda
023300000922     c                   movel     parfilcda     kkey
023400000922     c     ktab          chain     tabel00f                           38
023500000922     c                   if        *in38
023600000907     C                   MOVEL     *BLANKS       KKEY
023700020904     C                   MOVEL     D55TFA        KKEY
023800000907     C                   Z-ADD     parfil        D63LNA
023900000907     C     KTAB          CHAIN     TABEL00F                           3433
024000071018     c                   if        not %Found(TABEL00F)
024100071018     c     Avvisa_CED    DSPLY                   risposta          2
024200071018     c                   end
024300000922     c                   else
024400000922     c                   setoff                                       3433
024500000922     c                   end
024600000907     C     *IN33         IFEQ      '0'
024700000907     C     *IN34         andeq     '0'
024800000907     C                   MOVEL     TBLUNI        DS4G
024900000907     C                   EXSR      distinta
025000000907     C                   MOVEL     'S'           WRICH             1
025100000907     C                   END
025200000907     C*
025300000907     C                   ENDSR
025400000907?    C*-----------------------------------------------------------
025500000907?    C* distinta richiama programma per gestione distinta
025600000907?    C*-----------------------------------------------------------
025700000907     C     distinta      begsr
025800000907      *  Aggiorno il numeratore delle distinte
025900000907     C     D63TRI        IFNE      'U'
026000020903     c*
026100020827     c                   do        *hival
026200020827     C                   CLEAR                   TRUL33DS
026300020829     c                   eval      i33tla = 'L'
026400020827     C                   Z-ADD     0             I33AAA
026500020827     C                   Z-ADD     12            I33CNU
026600020903     C                   Z-ADD     d55tfa        I33PO1
026700020827     C                   Z-ADD     1             I33NUM
026800020829     c                   movel(p)  trul33ds      kpjbu
026900020827     c                   call      'TRUL33R'
027000020829     c                   parm                    kpjba
027100020829     c                   movel     kpjbu         trul33ds
027200020827     C                   IF        O33ERR = 0
027300020827     c* controllo che il numero non è già stato usato
027400020827     c                   clear                   fnlr6ds
027500020828     c                   eval      d6tla = 'L'
027600020828     C                   Z-ADD     8             D6npg
027700020903     C                   Z-ADD     d55tfa        D6fgs
027800020827     C                   Z-ADD     O33Nrf        D6NDT
027900020829     c                   movel(p)  fnlr6ds       kpjbu
028000020827     c                   call      'FNLR6R'
028100020829     c                   parm                    kpjba
028200020829     c                   movel     kpjbu         fnlr6ds
028300020828     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
028400020828     c* come foglio o in una bolla
028500020828     c                   if        d6flg <> 'N'
028600020827     c                   iter
028700020827     c                   else
028800020827     C                   Z-ADD     O33Nrf        D63NDT
028900020827     c                   leave
029000020827     C                   END
029100020827     C                   END
029200020827     C                   ENDdo
029300020827     C                   END
029400000907     C*
029500000907      *  Passo i dati reperiti dalla 4G al pgm di caricamento dati
029600020827     c                   z-add     parfil        d63lna
029700000907     C                   MOVEL     *BLANKS       D63F03
029800000907     C                   MOVEL     *BLANKS       D63F12
029900000907     C                   Z-ADD     §4GPDR        D63PDR
030000000907     C                   Z-ADD     §4GHMC        D63HMC
030100000907     C                   MOVEL     §4GFPP        D63FPP
030200000907     C                   MOVEL     §4GPEP        D63PEP
030300001019     c                   z-add     record        d63nrr
030400000907     C                   MOVEL     FNLR63DS      KPJBU
030500000915     C                   CALL      'FNLR63R1'
030600000907     C                   PARM                    KPJBA
030700000907     C                   MOVEL     KPJBU         FNLR63DS
030800000907     C*  Fine Lavoro
030900000907     C     D63F03        IFEQ      '1'
031000000907     C                   MOVEL     'S'           WFINE
031100000907     C                   SETON                                        31
031200000907     C                   ELSE
031300000907     C                   MOVEL     'S'           WRICA             1
031400000907     C                   END
031500000907     C*
031600000907     C                   CLEAR                   FNLR63DS
031700000907     C*
031800000907     c                   endsr
031900000517?    C*-------------------------------------------------
032000000517?    C* *INZSR - Settaggio iniziale programma STANDARD
032100000517?    C*-------------------------------------------------
032200000517     C     *inzsr        begsr
032300000517?    C*
032400000517     C     *entry        plist
032500000517     C                   parm                    kpjba
032600000901     c                   movel     kpjbu         parin
032700000901      *
032800000517     c     ktab          klist
032900000907     c                   kfld                    kkut
033000000907     c                   kfld                    kcod
033100000907     c                   kfld                    kkey
033200000517      *
033300000517     c     ktab1         klist
033400000907     c                   kfld                    kkut
033500000907     c                   kfld                    kcod
033600000907     c                   move      '3A'          kcod
033700000907     c                   z-add     1             kkut
033800001026
033900000901     c     karb          klist
034000020903     c                   kfld                    parfil
034100000901     c                   kfld                    data0
034200001026
034300001206     C     KSPED         KLIST
034400001206     C                   KFLD                    ARBAAS
034500001206     C                   KFLD                    ARBLNP
034600001206     C                   KFLD                    ARBNRS
034700001206     C                   KFLD                    ARBNSP
034800001206      *
034900001026
035000000907     C     *LIKE         DEFINE    TBLKUT        KKUT
035100000907     C     *LIKE         DEFINE    TBLCOD        KCOD
035200000907     C     *LIKE         DEFINE    TBLKEY        KKEY
035300000907     c                   open      fncde00f
035400000907      * ds richiamo fnlr63
035500000907     C                   CLEAR                   DS4G
035600000907     C                   clear                   fnlr63ds
035700000907     C                   TIME                    WHHDAT           12 0
035800000922     c                   move      *date         wdatag
035900000907     C                   MOVEL     ' '           WFINE             1
036000000907     C                   MOVEL     '1'           WTPVID            1
036100000907     C                   MOVEL     'P'           D63TRI
036200000907     C                   MOVEL     'N'           WRICH
036300000901     c
036400000901      * caricamento filiali poste
036500000901     c                   clear                   fip
036600000901     c     *loval        setll     azorg01l
036700000901     c                   do        *hival
036800000901     c                   read      azorg01l                               98
036900000901     c   98              leave
037000000901     c                   movel     orgde3        og143
037100020729     c     §ogntw        ifeq      'PPT'
037200000901     c                   z-add     1             x
037300000901     c     *zeros        lookup    fip(x)                                 91
037400000901     c   91              z-add     orgfil        fip(x)
037500000901     c                   endif
037600000901     c                   enddo
037700000901
037800000901      * caricamento tabella 3A
037900000901     c                   z-add     0             x
038000000901     c     ktab1         setll     tabel00f
038100000901     c                   do        *hival
038200000901     c     ktab1         reade     tabel00f                               98
038300000901     c   98              leave
038400000901     c                   if        tblflg = ' ' and x < 100
038500000901     c                   add       1             x
038600000901     c                   movel     tblkey        c3a(x)
038700000901     c                   movel     tbluni        d3a(x)
038800000901     c                   endif
038900000901     c                   enddo
039000001018      *caricamento tabella filiali in gestione
039100020904     c*                  exsr      carl6
039200000901
039300000517     c                   endsr
039400020903      *---------------------------------------------------------*
039500020903      *  reperisce il po di gestione                            *
039600020903      *---------------------------------------------------------*
039700020903     C     srfgs         BEGSR
039800020903     C*
039900020903     c                   clear                   fnlv55ds
040000020903     c                   eval      d55tpt = '6'
040100020903     c                   movel     parfil        d55lin
040200020903     c                   move      wdatag        d55drf
040300020903     c                   call      'FNLV55R'
040400020903     c                   parm                    fnlv55ds
040500020903     C*
040600020903     C                   ENDSR
