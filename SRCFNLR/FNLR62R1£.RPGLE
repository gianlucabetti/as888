000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200000915     ffncde00f  uf a e             disk    usropn
000300000901     ffnarb54l  if   e           k disk
000400040716     f****fndct02l  if   e           k disk
000500001018     ftabel00f  if   e           k disk
000600000901     fazorg01l  if   e           k disk
000700001206     FFNART01L  IF   E           K DISK
000800070122     FFNBRV01L  IF   E           K DISK
000900000517      *--------------------------------------------------------------------
001000000522     D kpjba         e ds
001100000901     D ds3a          e ds
001200000907     D DS4G          E DS
001300000901     D og143         e ds
001400000907     d dsarbd        e ds
001500000907     d dsarbg        e ds
001600000907     d darbt         e ds
001700000901     d dsarbk        e ds
001800000901     d ds48          e ds                           extname(FNLR48DS)
001900000907     D FNLR63DS      E DS
002000000912     D Fidn43ds      E DS
002100020827     d trul33ds      e ds
002200020903     d fnlv55ds      e ds
002300020903     d fnlr6ds       e ds
002400040716     d fidn12ds      e ds
002500020827     d trul06ds      e ds
002600020506     d  lin                    1     90  0 dim(30)
002700001018      *ds entrata parametri
002800000901     D parin           ds
002900000901     d  parfil                        3s 0
003000000901     d  paropz                        1
003100000901     d  pardat                        8s 0
003200000922     d  paranm                        1
003300001018     d  pardtf                        8s 0
003400001026     d  parcmc                        3
003500001206     d  parpog                        3s 0
003600001206     d  parnfv                        6s 0
003700001018      *variabili
003800001018     d
003900001018     d parkey          s                   like(parfil)
004000000907     d datasped        s              8  0
004100000901     d data0           s              8  0 inz(0)
004200000901     d x               s              3  0 inz(0)
004300001018     d y               s              3  0 inz(0)
004400000901     d fip             s              3  0 dim(50)
004500000907     d c3a             s              2    dim(100)
004600000901     d d3a             s             71    dim(100)
004700020904     D*L6              S              3  0 DIM(30)                              FIL GEST ARRIV
004800020506     D*FIL             S              3    DIM(29)                              FIL. COMODO
004900001018     d datalow         s              8  0 inz(00010101)
005000000922     d wdatag          s               d   datfmt(*iso)
005100000922     d parfilcda       s              6    inz('   CDA')
005200000925     d flgerr          s                   like(cdeerr)
005300001019     d record          s              4  0 inz(0)
005400001026     d aperta          s                   like(cdeerr)
005500071018     D Avvisa_CED      C                   CONST('Avvisa CED: Manca tab.:4G -
005600071018     D                                     x Filiale. Risp.->Enter')
005700000517      *--------------------------------------------------------------------
005800000517     c
005900000912      *parzializzazione lancio tipo 4 C.A.
006000001018      *richiama programma caricamento fncde da gestione dei danni
006100000918     c                   if        paropz = '4'
006200000918     c                   clear                   fidn43ds
006300000918     c                   move      '§'           i43dst
006400000918     c                   z-add     parfil        i43fil
006500000918     c                   z-add     pardat        i43amgdha
006600001018     c                   z-add     pardtf        i43amgdhd
006700001026     c                   move      paranm        i43ind
006800001026     c                   move      parcmc        i43are
006900000918     c                   movel     fidn43ds      kpjbu
007000000918     c                   call      'FIDN46R'
007100000918     c                   parm                    kpjba
007200001027      * rientro da programma chiamato e verifico se ho scritto record avendo
007300001027      * forzato dall'altra parte il numero record scritti nel campo filiale
007400001027     c                   movel     kpjbu         fidn43ds
007500001027     c                   z-add     i43fil        record
007600000912
007700000912      * altre parzializzazioni
007800000912     c                   else
007900001018      *loop sulla schiera delle filiali in gestione per impostare la chiave
008000020903     c*                  do        30            y
008100020903     c*                  if        l6(y) = 0 or record = 9999
008200020903     c*                  leave
008300020903     c*                  else
008400020903     c*                  z-add     l6(y)         parkey
008500020903     c*                  endif
008600001018      *estraggo da fnarb le bolle da chiudere riportandole su fncde
008700000901     c     karb          setll     fnarb54l
008800000517     c                   do        *hival
008900000901     c     karb          reade     fnarb54l                               99
009000001019      *Nr massimo di record scritti su fncde da riportare in sfl o fine file
009100001019     c                   if        *in99 or record = 9999
009200001019     c                   leave
009300001019     c                   endif
009400000901     c                   movel     arbaas        datasped
009500000901     c                   move      arbmgs        datasped
009600001018      * numero distinta già presente
009700001018     c                   if        arbndc > 0
009800001018     c                   iter
009900001018     c                   endif
010000010109      * data spedizione oltre il limite non per poste
010100010109     c                   if        paropz <> '2'
010200000901     c                   if        datasped > pardat
010300000901     c                   iter
010400000901     c                   endif
010500010109     c                   else
010600010109      *per spedizioni poste controllo la data di borderò e non la data sped.
010700010109     c                   if        arbdbr > pardat
010800010109     c                   iter
010900010109     c                   endif
011000010109     c                   endif
011100010109
011200001026      *controllo su fndct se esistono C.A. aperte per la spedizione se
011300001026      *esistono scarto il record
011400001026      *
011500001026     c                   move      *blank        aperta
011600040716     c*    kdct          setll     fndct02l
011700040716     c*                  do        *hival
011800040716     c*    kdct          reade     fndct02l                               97
011900001026      *Nr massimo di record scritti su fncde da riportare in sfl o fine file
012000040716     c*  97              leave
012100040716     c*                  if        dctdch = 0
012200040716      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
012300040716      * SIA COME MAMMA CHE COME FIGLIA
012400040716     c                   clear                   fidn12ds
012500040716     c                   eval      i12aas = arbaas
012600040716     c                   eval      i12lnp = arblnp
012700040716     c                   eval      i12nrs = arbnrs
012800040716     c                   eval      i12nsp = arbnsp
012900040716     c                   eval      i12fel = 'E'
013000040716     c                   eval      i12fan = 'E'
013100040716     c                   eval      i12fch = 'E'
013200040716     c                   call      'FIDN12R'
013300040716     c                   parm                    fidn12ds
013400040716      **
013500040716      * se c'è almeno una C.A. aperta
013600040716      **
013700040716     c     o12nca        ifgt      0
013800040716     c                   move      'X'           aperta
013900040716     c                   endif
014000001026     c                   if        aperta <> *blank
014100001026     c                   iter
014200001026     c                   end
014300000901      *
014400000901     c                   select
014500000901      *selezione 1
014600001018      *gestione filiali DpD
014700000901     c                   when      paropz = '1'
014800000901      * test sono buone solo le bolle di rientro
014900000911     c                   if        arbfbr <> 'S'
015000000901     c                   iter
015100000901     c                   end
015200000901      *selezione 2
015300001018      *gestione per spedizioni poste
015400000901     c                   when      paropz = '2'
015500000901      * considero solo linee di partenza poste
015600000901     c     arblnp        lookup    fip                                    80
015700000901     c  n80              iter
015800001206      * controllo se la spedizione ha una spunta in inventario IPP
015900001206      * se la trovo spuntata la scarto dall'estrazione
016000001206
016100001206     c                   move      *blank        scarta
016200001206     c                   exsr      ceipp
016300001206     c                   if        scarta <> *blank
016400001206     c                   iter
016500001206     c                   end
016600001206
016700000901      * testa tipo bolla per vedere se c'è incasso
016800000925     c                   move      *blank        flgerr
016900000907     c                   z-add     1             x
017000000901     c     arbcbo        lookup    c3a(x)                                 80
017100000901     c                   if        *in80
017200000901     c                   movel     d3a(x)        ds3a
017300000901      *esiste contrassegno lo annullo
017400000901     c                   if        §3afca > 0
017500000901     c                   exsr      annulla
017600000901     c                   if        d48err <> *blank
017700000925     c                   move      'C'           flgerr
017800000901     c                   endif
017900000901     c                   endif
018000000901     c                   end
018100000901      *selezione 3
018200001018      *gestione filiali estere
018300000901     c                   when      paropz = '3'
018400000901      *nessuna parzializzazione oltre alla data e alla filiale estera
018500000912      * già filtrata nel lancio
018600011211
018700011211      *selezione 5
018800011211      *gestione filiali DpD
018900011211     c                   when      paropz = '5'
019000011211      * test sono buone solo le bolle di rientro
019100011211     c                   if        arbfbr = 'S'
019200011211     c                   iter
019300011211     c                   end
019400071108      *selezione 6
019500071108      *gestione rimpalli sede filiale 101
019600071108     c                   when      paropz = '6'
019700071108     c                   move      *blank        flgerr
019800071108     c                   if        arbcbo = '1 ' or arbcbo = '2 '
019900071108     c                   exsr      trasforma
020000071108     c                   if        d48err <> *blank
020100071108     c                   move      'C'           flgerr
020200071108     c                   endif
020300071108     c                   endif
020400000901     c                   endsl
020500000901      * record ok da scrivere su fncde
020600001019     c                   add       1             record
020700000907     c                   exsr      scrivi
020800000901     c                   enddo
020900001018      *fine loop filiali in gestione
021000020903     c*                  enddo
021100000912     c                   endif
021200000907     c                   close     fncde00f
021300000901     c*
021400001026     c                   if        record > 0
021500000912     c                   exsr      richiama
021600001026     c                   else
021700001026     c                   movel(p)  'A'           kpjbu
021800001026     c                   end
021900000912     c*
022000000901     c                   seton                                        lr
022100000901?    C*-----------------------------------------------------------
022200000901?    C* annulla- elimina contrassegno
022300000901?    C*-----------------------------------------------------------
022400000901     C     annulla       begsr
022500000901     C                   CLEAR                   DS48
022600000901     C                   CLEAR                   DSARBK
022700000901     C                   MOVE      arbaas        D48AAS
022800000901     C                   MOVE      arblnp        D48LNP
022900000901     C                   MOVE      arbnrs        D48NRS
023000000901     C                   MOVE      arbnsp        D48NSP
023100000901     C                   MOVE      'KB'          D48CVB
023200000901     C                   MOVE      'E'           D48FFR
023300000901     C                   MOVE      'K'           D48TRC
023400000901     C                   MOVE      'A'           D48TBO
023500000901     C                   MOVE      *BLANK        §BKCBN
023600000901     C                   MOVE      *BLANK        §BKTIC
023700000901     C                   Z-ADD     0             §BKCAS
023800000901     C                   MOVEL     DS48          KPJBU
023900000901     C                   CALL      'FNLR48R'
024000000901     C                   PARM                    KPJBA
024100000901     C                   PARM                    DSARBD
024200000901     C                   PARM                    DSARBK
024300000901     C                   PARM                    DARBT
024400000901     C                   PARM                    DSARBG
024500000901     C                   MOVEL     KPJBU         DS48
024600000901?    C*
024700000901     c                   endsr
024800071108?    C*-----------------------------------------------------------
024900071108?    C* trasforma .. bolla da franco/assegnato a C/S
025000071108?    C*-----------------------------------------------------------
025100071108     C     trasforma     begsr
025200071108     C                   CLEAR                   DS48
025300071108     C                   CLEAR                   DSARBK
025400071108     C                   MOVE      arbaas        D48AAS
025500071108     C                   MOVE      arblnp        D48LNP
025600071108     C                   MOVE      arbnrs        D48NRS
025700071108     C                   MOVE      arbnsp        D48NSP
025800071108     C                   MOVE      'K8'          D48CVB
025900071108     C                   MOVE      'E'           D48FFR
026000071108     C                   MOVE      'K'           D48TRC
026100071108     C                   MOVE      'A'           D48TBO
026200071108      * franco / assegnato
026300071108     c                   if        arbcbo = '1 '
026400071112     C                   MOVE      'A '          §bkcbn
026500071108     c                   else
026600071112     C                   MOVE      'Z '          §bkcbn
026700071108     c                   endif
026800071108     C                   MOVEL     DS48          KPJBU
026900071108     C                   CALL      'FNLR48R'
027000071108     C                   PARM                    KPJBA
027100071108     C                   PARM                    DSARBD
027200071108     C                   PARM                    DSARBK
027300071108     C                   PARM                    DARBT
027400071108     C                   PARM                    DSARBG
027500071108     C                   MOVEL     KPJBU         DS48
027600071108?    C*
027700071108     c                   endsr
027800000901?    C*-----------------------------------------------------------
027900000901?    C* scrivi - scrive record fncde00f da fnarb
028000000901?    C*-----------------------------------------------------------
028100000901     C     scrivi        begsr
028200000901     c                   clear                   fncde000
028300000901     c                   z-add     arbaas        cdeaas
028400000901     c                   z-add     arblnp        cdelnp
028500000901     c                   z-add     arbnrs        cdenrs
028600000901     c                   z-add     arbnsp        cdensp
028700000901     c                   z-add     arblna        cdelna
028800000901     c                   z-add     arbrmn        cdermn
028900001026     c                   move      parcmc        cdecmc
029000000925     c                   move      flgerr        cdeerr
029100000922      *forzo anomalia 5 per bolle poste
029200000922     c                   if        paropz <> '2'
029300000901     c                   move      arbcca        cdecca
029400000922     c                   else
029500000922     c                   move      paranm        cdecca
029600000922     c                   end
029700000922      * forzo data consegna
029800001018     c                   if        pardtf > datalow
029900001018     c                   move      pardtf        cdedcm
030000001018     c                   else
030100001018     c                   move      wdatag        cdedcm
030200001018     c                   end
030300000901     c                   write     fncde000
030400000901     c                   endsr
030500000907      *---------------------------------------------------------*
030600000912      *  richiama pgm gestione chiusura distinta                *
030700000907      *---------------------------------------------------------*
030800000912     C     richiama      BEGSR
030900020904     c* reperisce il po di gestione
031000020904     c                   exsr      srfgs
031100000907      *
031200000907     C* Aggancio la tabella 4G x reperire i dati del padroncino,
031300000907     C* l'ora di consegna, ecc.. per le distinte delle linee
031400000907     C* estere
031500000907     C                   Z-ADD     1             KKUT
031600000907     C                   MOVEL     '4G'          KCOD
031700000922     C                   MOVEL     *BLANKS       KKEY
031800020904     c                   movel     D55TFA        parfilcda
031900000922     c                   movel     parfilcda     kkey
032000000922     c     ktab          chain     tabel00f                           38
032100000922     c                   if        *in38
032200000907     C                   MOVEL     *BLANKS       KKEY
032300020904     C                   MOVEL     D55TFA        KKEY
032400000907     C                   Z-ADD     parfil        D63LNA
032500000907     C     KTAB          CHAIN     TABEL00F                           3433
032600071018     c                   if        not %Found(TABEL00F)
032700071018     c     Avvisa_CED    DSPLY                   risposta          2
032800071018     c                   end
032900000922     c                   else
033000000922     c                   setoff                                       3433
033100000922     c                   end
033200000907     C     *IN33         IFEQ      '0'
033300000907     C     *IN34         andeq     '0'
033400000907     C                   MOVEL     TBLUNI        DS4G
033500000907     C                   EXSR      distinta
033600000907     C                   MOVEL     'S'           WRICH             1
033700000907     C                   END
033800000907     C*
033900000907     C                   ENDSR
034000000907?    C*-----------------------------------------------------------
034100000907?    C* distinta richiama programma per gestione distinta
034200000907?    C*-----------------------------------------------------------
034300000907     C     distinta      begsr
034400000907      *  Aggiorno il numeratore delle distinte
034500000907     C     D63TRI        IFNE      'U'
034600020903     c*
034700020827     c                   do        *hival
034800020827     C                   CLEAR                   TRUL33DS
034900020829     c                   eval      i33tla = 'L'
035000020827     C                   Z-ADD     0             I33AAA
035100020827     C                   Z-ADD     12            I33CNU
035200020903     C                   Z-ADD     d55tfa        I33PO1
035300020827     C                   Z-ADD     1             I33NUM
035400020829     c                   movel(p)  trul33ds      kpjbu
035500020827     c                   call      'TRUL33R'
035600020829     c                   parm                    kpjba
035700020829     c                   movel     kpjbu         trul33ds
035800020827     C                   IF        O33ERR = 0
035900020827     c* controllo che il numero non è già stato usato
036000020827     c                   clear                   fnlr6ds
036100020828     c                   eval      d6tla = 'L'
036200020828     C                   Z-ADD     8             D6npg
036300020903     C                   Z-ADD     d55tfa        D6fgs
036400020827     C                   Z-ADD     O33Nrf        D6NDT
036500020829     c                   movel(p)  fnlr6ds       kpjbu
036600020827     c                   call      'FNLR6R'
036700020829     c                   parm                    kpjba
036800020829     c                   movel     kpjbu         fnlr6ds
036900020828     c* se ritorno <>'N' vuol dire che ha trovato il numero già usato o
037000020828     c* come foglio o in una bolla
037100020828     c                   if        d6flg <> 'N'
037200020827     c                   iter
037300020827     c                   else
037400020827     C                   Z-ADD     O33Nrf        D63NDT
037500020827     c                   leave
037600020827     C                   END
037700020827     C                   END
037800020827     C                   ENDdo
037900020827     C                   END
038000000907     C*
038100000907      *  Passo i dati reperiti dalla 4G al pgm di caricamento dati
038200020827     c                   z-add     parfil        d63lna
038300000907     C                   MOVEL     *BLANKS       D63F03
038400000907     C                   MOVEL     *BLANKS       D63F12
038500000907     C                   Z-ADD     §4GPDR        D63PDR
038600000907     C                   Z-ADD     §4GHMC        D63HMC
038700000907     C                   MOVEL     §4GFPP        D63FPP
038800000907     C                   MOVEL     §4GPEP        D63PEP
038900001019     c                   z-add     record        d63nrr
039000000907     C                   MOVEL     FNLR63DS      KPJBU
039100000915     C                   CALL      'FNLR63R1'
039200000907     C                   PARM                    KPJBA
039300000907     C                   MOVEL     KPJBU         FNLR63DS
039400000907     C*  Fine Lavoro
039500000907     C     D63F03        IFEQ      '1'
039600000907     C                   MOVEL     'S'           WFINE
039700000907     C                   SETON                                        31
039800000907     C                   ELSE
039900000907     C                   MOVEL     'S'           WRICA             1
040000000907     C                   END
040100000907     C*
040200000907     C                   CLEAR                   FNLR63DS
040300000907     C*
040400000907     c                   endsr
040500001206?    C*-----------------------------------------------------------
040600001206?    C* controllo per giro poste le spedizioni spuntate in ipp
040700001206?    C*-----------------------------------------------------------
040800001206     C     ceipp         begsr
040900001206     C     ksped         setll     fnart01l
041000001206     C                   do        *hival
041100001206     C     ksped         reade     fnart01l                               84
041200001206     C   84              leave
041300001206      *
041400001206      * 85 = Off: COLLO TROVATO IN IPP (devo scartare)
041500001206      *
041600070122     C     kfibrv        chain     fnbrv01l                           85
041700001206B35  C  n85              move      '1'           scarta            1
041800001206     c                   enddo
041900001206     c                   endsr
042000001018     C**************************************************************************
042100001018     C* CARICO L6
042200001018     C**************************************************************************
042300020904     C*    CARL6         BEGSR
042400001018     C*
042500020904     c*                  clear                   trul06ds
042600020904     c*                  eval      d06cod = '£6'
042700020904     c*                  movel     parfil        d06key
042800020904     c*                  movel     trul06ds      kpjbu
042900020904     c*                  call      'TRUL06R'
043000020904     c*                  parm                    kpjba
043100020904     c*                  movel     kpjbu         trul06ds
043200020904     c*                  movea     lin           l6
043300001018     C*
043400020904     C*                  ENDSR
043500000517?    C*-------------------------------------------------
043600000517?    C* *INZSR - Settaggio iniziale programma STANDARD
043700000517?    C*-------------------------------------------------
043800000517     C     *inzsr        begsr
043900000517?    C*
044000000517     C     *entry        plist
044100000517     C                   parm                    kpjba
044200000901     c                   movel     kpjbu         parin
044300000901      *
044400000517     c     ktab          klist
044500000907     c                   kfld                    kkut
044600000907     c                   kfld                    kcod
044700000907     c                   kfld                    kkey
044800000517      *
044900000517     c     ktab1         klist
045000000907     c                   kfld                    kkut
045100000907     c                   kfld                    kcod
045200000907     c                   move      '3A'          kcod
045300000907     c                   z-add     1             kkut
045400001026
045500000901     c     karb          klist
045600020903     c                   kfld                    parfil
045700000901     c                   kfld                    data0
045800001026
045900001026     c     kdct          klist
046000001026     c                   kfld                    arbaas
046100001026     c                   kfld                    arblnp
046200001026     c                   kfld                    arbnrs
046300001026     c                   kfld                    arbnsp
046400001206      *
046500001206     C     KSPED         KLIST
046600001206     C                   KFLD                    ARBAAS
046700001206     C                   KFLD                    ARBLNP
046800001206     C                   KFLD                    ARBNRS
046900001206     C                   KFLD                    ARBNSP
047000001206      *
047100070122     C     KFIBRV        KLIST                                                  FNBRV01L
047200001206     C                   KFLD                    KNPG              1 0
047300001206     C                   KFLD                    KNFV              6 0
047400001206     C                   KFLD                    parpog
047500001206     C                   KFLD                    ARTFLS
047600001206     C                   KFLD                    ARTLNA
047700001206     C                   KFLD                    ARTNRS
047800001206     C                   KFLD                    ARTNSC
047900001206     C                   Z-ADD     3             KNPG
048000001206     C                   z-add     parnfv        KNFV
048100001026
048200000907     C     *LIKE         DEFINE    TBLKUT        KKUT
048300000907     C     *LIKE         DEFINE    TBLCOD        KCOD
048400000907     C     *LIKE         DEFINE    TBLKEY        KKEY
048500000907     c                   open      fncde00f
048600000907      * ds richiamo fnlr63
048700000907     C                   CLEAR                   DS4G
048800000907     C                   clear                   fnlr63ds
048900000907     C                   TIME                    WHHDAT           12 0
049000000922     c                   move      *date         wdatag
049100000907     C                   MOVEL     ' '           WFINE             1
049200000907     C                   MOVEL     '1'           WTPVID            1
049300000907     C                   MOVEL     'P'           D63TRI
049400000907     C                   MOVEL     'N'           WRICH
049500000901     c
049600000901      * caricamento filiali poste
049700000901     c                   clear                   fip
049800000901     c     *loval        setll     azorg01l
049900000901     c                   do        *hival
050000000901     c                   read      azorg01l                               98
050100000901     c   98              leave
050200000901     c                   movel     orgde3        og143
050300020729     c     §ogntw        ifeq      'PPT'
050400000901     c                   z-add     1             x
050500000901     c     *zeros        lookup    fip(x)                                 91
050600000901     c   91              z-add     orgfil        fip(x)
050700000901     c                   endif
050800000901     c                   enddo
050900000901
051000000901      * caricamento tabella 3A
051100000901     c                   z-add     0             x
051200000901     c     ktab1         setll     tabel00f
051300000901     c                   do        *hival
051400000901     c     ktab1         reade     tabel00f                               98
051500000901     c   98              leave
051600000901     c                   if        tblflg = ' ' and x < 100
051700000901     c                   add       1             x
051800000901     c                   movel     tblkey        c3a(x)
051900000901     c                   movel     tbluni        d3a(x)
052000000901     c                   endif
052100000901     c                   enddo
052200001018      *caricamento tabella filiali in gestione
052300020904     c*                  exsr      carl6
052400000901
052500000517     c                   endsr
052600020903      *---------------------------------------------------------*
052700020903      *  reperisce il po di gestione                            *
052800020903      *---------------------------------------------------------*
052900020903     C     srfgs         BEGSR
053000020903     C*
053100020903     c                   clear                   fnlv55ds
053200020903     c                   eval      d55tpt = '6'
053300020903     c                   movel     parfil        d55lin
053400020903     c                   move      wdatag        d55drf
053500020903     c                   call      'FNLV55R'
053600020903     c                   parm                    fnlv55ds
053700020903     C*
053800020903     C                   ENDSR
