000100930527     H DECEDIT('0,') DATEDIT(*DMY.)
000200050712     H* FNLV58R *-----------------------------------------------------*
000300050926     H*     Cerca legami bolla
000400930322     H*---------------------------------------------------------------*
000500930322     H* DATA BASE                                                     *
000600930322     H*---------------------------------------------------------------*
000700050714     Fazorg01l  IF   E           K DISK
000800050714     Ffnlbl01l  IF   E           K DISK
000900050712     Ffnlbl02l  iF   E           K DISK
001000050712     F                                     RENAME(fnlbl000:fnlbl002)
001100050712     Ffnblp01l  IF   E           K DISK
001200050712     f                                     prefix(ARB:3)
001300050712     Ffnarb01l  iF   E           K DISK
001400050712     Ftabel00f  iF   E           K DISK
001500940323     D*
001600050714     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
001700050923     D CCA             S              1    DIM(50)
001800050923     D TLE             S              1    DIM(50)
001900050714     D*
002000050712     D fnlv58ds      E DS
002100050714      * DS PER TISI95R - CONTROLLO DI CAP
002200050714     D Tisi95DS      E DS
002300050712     D DS3A          E DS
002400050923     D ds7O          E DS
002500050714     D og143         E DS
002600050923     d
002700050923     D boOriginale     DS
002800050923     D  lblaao                 1      4  0
002900050923     D  lbllpo                 5      7  0
003000050923     D  lblnro                 8      9  0
003100050923     D  lblnso                10     16  0
003200050923     D boMamma         DS
003300050923     D  lblaap                 1      4  0
003400050923     D  lbllpp                 5      7  0
003500050923     D  lblnrp                 8      9  0
003600050923     D  lblnsp                10     16  0
003700050712     d
003800050712     d kcod            s                   Like(tblcod)
003900050712     d kkey            s                   Like(tblkey)
004000050714     d kaas            s                   Like(arbaas)
004100050714     d klnp            s                   Like(arblnp)
004200050714     d knrs            s                   Like(arbnrs)
004300050714     d knsp            s                   Like(arbnsp)
004400050714     d wcbo            s                   Like(arbcbo)
004500050714     d wlom            s                   Like(arblom)
004600050714     d wmgs            s                   Like(arbmgs)
004700050923     d wccaprec        s                   Like(arbcca)
004800050923     d xx              s              3  0
004900050712     D codut           s              1  0 inz(1)
005000050909     D tipolbl         s              1
005100050923     D Wcarica         s              1
005200050923     D bollaInput      s              2
005300050923     D Wok             s              1
005400930322     C*---------------------------------------------------------------*
005500970902     C                   EXSR      DEFVAR
005600970902     C**
005700970902     C* TIPO LANCIO, TLA "C" -           CHIUSO CON LR
005800970902     C* TIPO LANCIO, TLA "L" - ELABORO E CHIUDO CON LR
005900970902     C* TIPO LANCIO, TLA " " - ELABORO E CHIUDO IN RETRN
006000050714    0C     Ilv58TLA      IFNE      'C'
006100970902     C**
006200050712     C                   CLEAR                   olv58aas
006300050712     C                   CLEAR                   olv58lnp
006400050714     C                   CLEAR                   olv58tfp
006500050712     C                   CLEAR                   olv58nrs
006600050712     C                   CLEAR                   olv58nsp
006700050923     C                   CLEAR                   olv58inmfi
006800050923     C                   CLEAR                   olv58oumfi
006900050923     C                   CLEAR                   olv58inpor
007000050923     C                   CLEAR                   olv58tle
007100050712     C                   CLEAR                   olv58err
007200050923     C                   CLEAR                   olv58msg
007300050712     C                   CLEAR                   olv58mct
007400050712     C                   CLEAR                   olv58cam
007500050712     C                   CLEAR                   olv58fap
007600050712     C                   CLEAR                   olv58nzm
007700050712     C                   CLEAR                   olv58cts
007800050712     C                   CLEAR                   olv58fin
007900050712     C                   CLEAR                   olv58cad
008000050712     C                   CLEAR                   olv58nzd
008100060123     C                   CLEAR                   olv58fdn
008200050923     C                   CLEAR                   wok
008300050923     C                   CLEAR                   bollainput
008400050712     C* ERRORI
008500050926    1C                   IF        ILV58TLE<>' '
008600050926     c     ilv58tle      lookup    tle                                    30
008700050926     c                   if        not *in30
008800050926     C                   MOVEL     'E'           OLV58ERR
008900050926     C                   MOVEL     MSG(1)        OLV58MSG
009000050926     C                   GOTO      FINE
009100050926    1c                   endif
009200050926    1c                   endif
009300050712     c
009400050712     c* Se non passato il porto lo determino
009500050714    1c                   if        ilv58por=' '
009600050712     c* chaino la bolla
009700050923     c     kbollA        CHAIN     FNarb01L
009800050923    3C                   if        not %found(fnarb01l)
009900050712     c     kbollA        CHAIN     FNBLP01L
010000050712    2C                   if        not %found(fnblp01l)
010100050712     C                   MOVEL     'E'           OLV58ERR
010200050712     C                   MOVEL     MSG(2)        OLV58MSG
010300050712     C                   GOTO      FINE
010400050712    3c                   endif
010500050712    2c                   endif
010600050712     c
010700050923     c                   eval      wccaprec=arbcca
010800050923     c                   eval      wcbo=arbcbo
010900050714     c                   EXSR      CERTBO
011000050714    c
011100050714     c                   if        olv58err<>' '
011200050714     c                   goto      fine
011300050714     c                   endif
011400050714     c
011500050909     c                   movel     §3atb1        olv58inpor
011600050712   x1c                   else
011700050909     c                   movel     ilv58por      olv58inpor
011800050712    1c                   endif
011900050712     c
012000050712     C*******************+
012100050909     c* Richiesto specifico legame bolla
012200050712     C*******************+
012300050909    1C                   IF        ILV58TLE<>' '
012400050923     c                   EXSR      CERTipoLeg
012500050923     c                   else
012600050923     c                   EXSR      LeggiLegami
012700050923     c                   endif
012800050923     c
012900050923    0C                   ENDIF
013000940323     C*
013100050712     c     fine          tag
013200050712     c
013300050714     C     Ilv58tla      IFNE      ' '
013400970902     C                   SETON                                        LR
013500050714     C**
013600050714     C                   CLEAR                   Tisi95DS
013700050714     C                   MOVEL     'C'           I95TLA
013800050714     C                   CALL      'TISI95R'
013900050714     C                   PARM                    Tisi95DS
014000970902     C                   ELSE
014100970902     C                   SETON                                        RT
014200970902     C                   ENDIF
014300050909     c*---------------------------------------------------------------------
014400050909     c* Impostazine campi per ricerca legami bolla
014500050909     c*---------------------------------------------------------------------
014600050909     c     IMPOCERCA     BEGSR
014700050909     c                   eval      kaas=ilv58aas
014800050909     c                   eval      klnp=ilv58lnp
014900050909     c                   eval      knrs=ilv58nrs
015000050909     c                   eval      knsp=ilv58nsp
015100050909     c*
015200050909     c                   clear                   lblnsp
015300050909     c                   clear                   lblnsN
015400050909     c                   clear                   esistelbl
015500050923     C                   ENDSR
015600050714     c*---------------------------------------------------------------------
015700050714     c* cerco legami bolla
015800050714     c*---------------------------------------------------------------------
015900050909     c     CERLEG        BEGSR
016000050714     c                   clear                   esistelbl         1
016100050909     c                   if        tipolbl='M'
016200050714     c     klegame       chain     fnlbl01l
016300050909     c                   else
016400050909     c     klegame       chain     fnlbl02l
016500050909     c                   endif
016600050714     c
016700050909    1c                   if        not %found
016800050714     c                   eval      esistelbl='NL'
016900050714     c                   goto      endcerleg
017000050714    1c                   endif
017100050714     c
017200050909     c* Bolla precedente trovata
017300050909     c                   if        tipolbl='M'
017400050714     c                   eval      kaas=lblaap
017500050714     c                   eval      klnp=lbllpp
017600050714     c                   eval      knrs=lblnrp
017700050714     c                   eval      knsp=lblnsp
017800050909     c                   else
017900050909     c* Bolla seguente   trovata
018000050909     c                   eval      kaas=lblaan
018100050909     c                   eval      klnp=lbllpn
018200050909     c                   eval      knrs=lblnrn
018300050909     c                   eval      knsp=lblnsn
018400050909     c                   endif
018500050909     c
018600050923     c     klegame       CHAIN     FNarb01L
018700050923    2C                   if        not %found(fnarb01l)
018800050714     c     klegame       CHAIN     FNBLP01L
018900050714    1C                   if        not %found(fnblp01l)
019000050714     c                   eval      esistelbl='NB'
019100050714     c                   goto      endcerleg
019200050714    2c                   endif
019300050714    1c                   endif
019400050714     c
019500050714     c     endcerleg     endsr
019600950105     C*---------------------------------------------------------------*
019700950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
019800950105     C*---------------------------------------------------------------*
019900950105     C     DEFVAR        BEGSR
020000950105     C*
020100950105     C     *ENTRY        PLIST
020200050712     C                   PARM                    fnlv58ds
020300950105     C*
020400050712     C     Kbolla        KLIST
020500050712     C                   KFLD                    Ilv58AAS
020600050712     C                   KFLD                    Ilv58LNP
020700050712     C                   KFLD                    Ilv58NRS
020800050712     C                   KFLD                    Ilv58NSP
020900050712     C     KLEGAmE       KLIST
021000050712     C                   KFLD                    KAAS
021100050712     C                   KFLD                    kLNP
021200050712     C                   KFLD                    kNRS
021300050712     C                   KFLD                    kNSP
021400050712     C     KTAB          KLIST
021500050712     C                   KFLD                    codut
021600050712     C                   KFLD                    kcod
021700050712     C                   KFLD                    kkey
021800050923     C     KTAB2         KLIST
021900050923     C                   KFLD                    codut
022000050923     C                   KFLD                    kcod
022100050923     c
022200050923     c* Carico solo la prima volta la tabella dei tipi legami, com relatica
022300050923     c*  consegna anomala
022400050923     c                   if        Wcarica=' '
022500060119     c                   movea     *all'X'       CCA
022600050923     c                   clear                   xx
022700050923     c                   movel     '7O'          kcod
022800050923     c     ktab2         setll     tabel00f
022900050923     c     ktab2         reade     tabel00f
023000050923     c                   dow       not %eof(tabel00f)
023100050923     c                   if        tblflg=' '
023200050923     c                   movel     tbluni        ds7o
023300050923     c                   if        §7otle<>' '
023400050923     c                   add       1             xx
023500050923     c                   movel     tblkey        cca(xx)
023600050923     c                   movel     §7otle        tle(xx)
023700050923     c                   endif
023800050923     c                   endif
023900050923     c     ktab2         reade     tabel00f
024000050923     c                   enddo
024100060117     c
024200060117     c* Aggiungo di dft il tipo legame giacenza che non precede una
024300060117     c*  consegna anomala nella bolla mamma
024400060117     c                   add       1             xx
024500060117     c                   movel     ' '           cca(xx)
024600060117     c                   movel     'G'           tle(xx)
024700050923     c
024800050923     c                   eval      wcarica='S'
024900050923     c                   endif
025000950105     C*
025100950105     C                   ENDSR
025200050714     C*---------------------------------------------------------------*
025300050909     C* Certo tipo bolla per avere il porto
025400050714     C*---------------------------------------------------------------*
025500050714     c     CERTBO        BEGSR
025600050714     c                   movel     '3A'          kcod
025700050714     c                   movel(P)  wcbo          kkey
025800050714     c     ktab          chain     tabel00f
025900050714    2c                   if        %found(tabel00f) and tblflg=' '
026000050714     c                   movel     tbluni        ds3a
026100050714     c                   else
026200050714     c                   clear                   ds3a
026300050714     C                   MOVEL     'E'           OLV58ERR
026400050714     C                   MOVEL     MSG(3)        OLV58MSG
026500050714    2c                   endif
026600050714     c                   endSR
026700050714     C*---------------------------------------------------------------*
026800050714      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
026900050714     C*---------------------------------------------------------------*
027000050714     c     CERMCT        BEGSR
027100050714     C                   CLEAR                   Tisi95DS
027200050714     C                   MOVEL     wlom          I95LOC
027300050714     C                   MOVEL     olv58cam      I95CAP
027400050714     C                   MOVEL     olv58nzm      I95NAR
027500050714     C                   MOVEL     '3'           I95TCN
027600050714     C                   MOVEL     olv58aas      I95DAT
027700050714     C                   MOVE      wmgs          I95DAT
027800050714     c                   exsr      CERTBO
027900050714     c
028000050714     C     §3AFCA        IFNE      0
028100050714     C                   MOVEL     'S'           I95FCA
028200050714     C                   ENDIF
028300050714     C                   MOVEL     §3ATB1        I95TPO
028400050714     c* Certo ntw della lnp
028500050714     c     olv58lnp      chain     azorg01l
028600050714     c                   if        %found(azorg01l)
028700050714     c                   movel     orgde3        og143
028800050714     c                   else
028900050714     c                   clear                   og143
029000050714     c                   endif
029100050714     c
029200050714     c     §ogntw        ifeq      'FED'
029300050714     c                   movel     'S'           i95fi1
029400050714     c                   endif
029500050714     c**
029600050714     C                   CALL      'TISI95R'
029700050714     C                   PARM                    Tisi95DS
029800050714     C     O95ERR        IFEQ      *BLANKS
029900050714     C                   MOVEL     O95CTS        olv58mct
030000050714     C                   ENDIF
030100050714     c
030200050714     c                   ENDSR
030300050923     c*---------------------------------------------------------------------
030400050923     c* cerco uno specifico legame bolla passato in input
030500050923     c*---------------------------------------------------------------------
030600050923     c     CERTipoLeg    BEGSR
030700050923     c                   eval      olv58tle=ilv58tle
030800050923     c
030900050923     c* Impostazioni per ricerca legami
031000050923     c                   exsr      impocerca
031100050923     c                   eval      tipolbl='M'
031200050923     c
031300050923     c* Cerco bolla PRECEDENTE col tipo legame richiesto
031400060119    2c                   dou       tle(xx)<>ilv58tle  or esistelbl<>'  '
031500050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
031600050923    3c                   if        lblnsp>0
031700050923     c                   if        BoOriginale=BoMamma
031800050923     c                   eval      olv58oumfi='O '
031900050923     c                   else
032000050923     c                   eval      olv58oumfi='MP'
032100050923     c                   endif
032200050923     c                   eval      olv58inmfi='F '
032300050923     c                   eval      olv58aas=lblaap
032400050923     c                   eval      olv58lnp=lbllpp
032500050923     c                   eval      olv58nrs=lblnrp
032600050923     c                   eval      olv58nsp=lblnsp
032700050923     c                   eval      olv58tfp=arbtfp
032800050923     c                   eval      olv58fap=arbfap
032900050923     c                   eval      olv58cam=arbcam
033000050923     c                   eval      olv58nzm=arbnzm
033100050923     c                   eval      olv58cts=arbcts
033200050923     c                   eval      olv58fin=arbfin
033300050923     c                   eval      olv58cad=arbcad
033400050923     c                   eval      olv58nzd=arbnzd
033500060123     c                   eval      olv58fdn=arbfdn
033600050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
033700050923     c                   eval      wlom=arblom
033800050923     c                   eval      wcbo=arbcbo
033900050923     c                   eval      wmgs=arbmgs
034000050923     c* memorizzo di aver trovato una bolla mamma giusta
034100050923     c                   eval      wok='S'
034200050923    3C                   ENDIF
034300050923     c*
034400050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
034500050923     c*  il giro trattandola da figlia solo se non ho letto l'originale
034600050923    3c                   if        olv58oumfi<>'O'
034700050923     c                   exsr      CERLEG
034800050923     c
034900050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
035000050923    4c                   if        esistelbl='NB'
035100050923     C                   MOVEL     'E'           OLV58ERR
035200050923     C                   MOVEL     MSG(4)        OLV58MSG
035300050923    4c                   endif
035400050923     c*  Se ho trovato la bolla mamma, che sia del tipo legame richiesto
035500050923     c*  o meno, significa che la bolla passata è figlia: lo memorizzo
035600050923     c                   if        esistelbl='  '
035700050923     c                   eval      Bollainput='F '
035800050923     c                   endif
035900050923
036000050923   x3c                   else
036100050923     c* Per uscire dal ciclo imposto "XX" in esistelbl
036200050923     c                   eval      esistelbl='XX'
036300050923    3c                   endif
036400050923     c
036500050923     c                   z-add     1             xx
036600050923     c     arbcca        lookup    cca(xx)                                30
036700050923     c
036800050923     c                   if        not *in30
036900050923     c                   z-add     50            xx
037000050923     c                   endif
037100050923     c
037200050923    2c                   enddo
037300050923     c
037400050923     c* Calcolo il cod.tassazione mittente se trovato legame
037500050923    2c                   if        olv58nsp>0
037600050923     c                   EXSR      CERMCT
037700050923    2c                   endif
037800050923     c
037900050923     c                   EXSR      Impocerca
038000050923     c                   eval      tipolbl='F'
038100050923     c
038200050923     c**********************
038300050923     c* Cerco bolla SEGUENTE  col tipo legame richiesto
038400050923     c**********************
038500050923     c* Solo se la bolla passata presenta il tipo legame richeisto
038600050923   1ac                   if        wccaprec=ilv58tle
038700050923     c
038800050923    2c                   dou       cca(xx)<>ilv58tle  or  esistelbl<>'  '
038900050923     c
039000050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
039100050923    3c                   if        lblnsn>0
039200050923     c* do precedenza ai dati della mamma se trovo anche quelli
039300050923     c*  della figlia (bolla in mezzo!!)
039400050923    4c                   if        Wok=' '
039500050923     c                   eval      olv58oumfi='F'
039600050923     c                   eval      olv58aas=lblaan
039700050923     c                   eval      olv58lnp=lbllpn
039800050923     c                   eval      olv58nrs=lblnrn
039900050923     c                   eval      olv58nsp=lblnsn
040000050923     c                   eval      olv58tfp=arbtfp
040100050923     c                   eval      olv58fap=arbfap
040200050923     c                   eval      olv58cam=arbcam
040300050923     c                   eval      olv58nzm=arbnzm
040400050923     c                   eval      olv58cts=arbcts
040500050923     c                   eval      olv58fin=arbfin
040600050923     c                   eval      olv58cad=arbcad
040700050923     c                   eval      olv58nzd=arbnzd
040800060123     c                   eval      olv58fdn=arbfdn
040900060123     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
041000050923     c                   eval      wlom=arblom
041100050923     c                   eval      wcbo=arbcbo
041200050923     c                   eval      wmgs=arbmgs
041300050923    4C                   ENDIF
041400050923    3C                   ENDIF
041500050923     c*
041600050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
041700050923     c*  il giro trattandola da figlia
041800050923     c                   exsr      CERLEG
041900050923     c
042000050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
042100050923    3c                   if        esistelbl='NB'
042200050923     C                   MOVEL     'E'           OLV58ERR
042300050923     C                   MOVEL     MSG(4)        OLV58MSG
042400050923    3c                   endif
042500050923
042600050923     c*  Se ho trovato la bolla figlia, che sia del tipo legame richiesto
042700050923     c*  o meno, significa che la bolla passata è mamma: lo memorizzo
042800050923    3c                   if        esistelbl='  '
042900050923     c
043000050923    4c                   if        BollaInput='F '
043100050923     c* sono mamma seguente se ho memorizzato la precedente nei campi di
043200050923     c*  output quindi --> Wok='S'
043300050923    5c                   if        wok='S'
043400050923     c                   eval      Bollainput='MS'
043500050923     c                   else
043600050923     c                   eval      Bollainput='MP'
043700050923    5c                   endif
043800050923    4c                   endif
043900050923    4c                   if        Bollainput='  '
044000050923     c                   eval      Bollainput='O '
044100050923    4c                   endif
044200050923     c
044300050923     c* al secondo passaggio, anche se non esatto il tipo legame, la
044400050923     c*  bolla legata non è solo figlia ma anche mamma
044500050923    4c                   if        olv58oumfi='F'
044600050923     c                   eval      olv58oumfi='MS'
044700050923    4c                   endif
044800050923     c
044900050923    3c                   endif
045000050923     c
045100050923     c                   z-add     1             xx
045200050923     c     wccaprec      lookup    cca(xx)                                30
045300050923     c                   movel     arbcca        wccaprec
045400050923     c
045500050923     c                   if        not *in30
045600050923     c                   z-add     50            xx
045700050923     c                   endif
045800050923     c
045900050923    2c                   enddo
046000050923   1aC                   ENDIF
046100050923     c*
046200050923     C*
046300050923     c*
046400050923     c                   if        olv58nsp=0
046500050923     c* Nessun legame specificato trovato
046600050923     c                   eval      olv58inmfi='NN'
046700050923     c                   else
046800050923     c                   movel     BollaInput    olv58inmfi
046900050923     c
047000050923     c                   exsr      CERMCT
047100050923     c                   endif
047200050923     c                   ENDSR
047300050923     c
047400050923     c*---------------------------------------------------------------------
047500050923     c* cerco tutti i legami bolla
047600050923     c*---------------------------------------------------------------------
047700050923     c     LeggiLegami   BEGSR
047800050923     c
047900050923     c* Impostazioni per ricerca legami
048000050923     c                   exsr      impocerca
048100050923     c                   eval      tipolbl='M'
048200050923     c
048300050923     c************************
048400050923     c* Cerco bolla PRECEDENTE
048500050923     c************************
048600050923    2c                   dou       esistelbl<>'  '
048700050923     c
048800050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
048900050923    3c                   if        lblnsp>0
049000050923     c                   if        BoOriginale=BoMamma
049100050923     c                   eval      olv58oumfi='O '
049200050923     c                   else
049300050923     c                   eval      olv58oumfi='MP'
049400050923     c                   endif
049500050923     c                   eval      olv58inmfi='F '
049600050923     c                   eval      olv58aas=lblaap
049700050923     c                   eval      olv58lnp=lbllpp
049800050923     c                   eval      olv58nrs=lblnrp
049900050923     c                   eval      olv58nsp=lblnsp
050000050923     c                   eval      olv58tfp=arbtfp
050100050923     c                   eval      olv58fap=arbfap
050200050923     c                   eval      olv58cam=arbcam
050300050923     c                   eval      olv58nzm=arbnzm
050400050923     c                   eval      olv58cts=arbcts
050500050923     c                   eval      olv58fin=arbfin
050600050923     c                   eval      olv58cad=arbcad
050700050923     c                   eval      olv58nzd=arbnzd
050800060123     c                   eval      olv58fdn=arbfdn
050900050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
051000050923     c                   eval      wlom=arblom
051100050923     c                   eval      wcbo=arbcbo
051200050923     c                   eval      wmgs=arbmgs
051300050923     c* memorizzo di aver trovato una bolla mamma giusta
051400050923     c                   eval      wok='S'
051500050926     c* Imposto il tipo legame
051600050926     c                   z-add     1             xx
051700050926     c     arbcca        lookup    cca(XX)                                30
051800050926    4c   30              if        olv58tle=' '
051900050926     c                   movel     tle(XX)       olv58tle
052000050926   x4c                   else
052100050926    5c                   if        olv58tle<>tle(XX)
052200050926     c                   eval      olv58tle='N'
052300050926    5c                   endif
052400050926    4c                   endif
052500050926     c
052600050923    3C                   ENDIF
052700050923     c*
052800050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
052900050923     c*  il giro trattandola da figlia solo se non ho letto l'originale
053000050923    3c                   if        olv58oumfi<>'O'
053100050923     c                   exsr      CERLEG
053200050923     c
053300050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
053400050923    4c                   if        esistelbl='NB'
053500050923     C                   MOVEL     'E'           OLV58ERR
053600050923     C                   MOVEL     MSG(4)        OLV58MSG
053700050923    4c                   endif
053800050926     c*  Se ho trovato la bolla mamma,
053900050926     c*   significa che la bolla passata è figlia: lo memorizzo
054000050923     c                   if        esistelbl='  '
054100050923     c                   eval      Bollainput='F '
054200050923     c                   endif
054300050923
054400050923   x3c                   else
054500050923     c* Per uscire dal ciclo imposto "XX" in esistelbl
054600050923     c                   eval      esistelbl='XX'
054700050923    3c                   endif
054800050923    2c                   enddo
054900050923     c
055000050923     c* Calcolo il cod.tassazione mittente se trovato legame
055100050923    2c                   if        olv58nsp>0
055200050923     c                   EXSR      CERMCT
055300050923    2c                   endif
055400050923     c
055500050923     c                   EXSR      Impocerca
055600050923     c                   eval      tipolbl='F'
055700050923     c
055800050923     c**********************
055900050923     c* Cerco bolla SEGUENTE
056000050923     c**********************
056100050923     c
056200050923    2c                   dou       esistelbl<>'  '
056300050923     c
056400050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
056500050923    3c                   if        lblnsn>0
056600050923     c* do precedenza ai dati della mamma se trovo anche quelli
056700050923     c*  della figlia (bolla in mezzo!!)
056800050923    4c                   if        Wok=' '
056900050923     c                   eval      olv58oumfi='F'
057000050923     c                   eval      olv58aas=lblaan
057100050923     c                   eval      olv58lnp=lbllpn
057200050923     c                   eval      olv58nrs=lblnrn
057300050923     c                   eval      olv58nsp=lblnsn
057400050923     c                   eval      olv58tfp=arbtfp
057500050923     c                   eval      olv58fap=arbfap
057600050923     c                   eval      olv58cam=arbcam
057700050923     c                   eval      olv58nzm=arbnzm
057800050923     c                   eval      olv58cts=arbcts
057900050923     c                   eval      olv58fin=arbfin
058000050923     c                   eval      olv58cad=arbcad
058100050923     c                   eval      olv58nzd=arbnzd
058200060123     c                   eval      olv58fdn=arbfdn
058300060123     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
058400050923     c                   eval      wlom=arblom
058500050923     c                   eval      wcbo=arbcbo
058600050923     c                   eval      wmgs=arbmgs
058700050923    4C                   ENDIF
058800050926     c
058900050926     c* Imposto il tipo legame,anche se già trovata una bolla mamma
059000050926     c                   z-add     1             xx
059100050926     c     arbcca        lookup    cca(XX)                                30
059200050926    4c   30              if        olv58tle=' '
059300050926     c                   movel     tle(XX)       olv58tle
059400050926   x4c                   else
059500050926    5c                   if        olv58tle<>tle(XX)
059600050926     c                   eval      olv58tle='N'
059700050926    5c                   endif
059800050926    4c                   endif
059900050923    3C                   ENDIF
060000050923     c*
060100050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
060200050923     c*  il giro trattandola da figlia
060300050923     c                   exsr      CERLEG
060400050923     c
060500050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
060600050923    3c                   if        esistelbl='NB'
060700050923     C                   MOVEL     'E'           OLV58ERR
060800050923     C                   MOVEL     MSG(4)        OLV58MSG
060900050923    3c                   endif
061000050923
061100050926     c*  Se ho trovato la bolla figlia,
061200050926     c*   significa che la bolla passata è mamma: lo memorizzo
061300050923    3c                   if        esistelbl='  '
061400050923     c
061500050923    4c                   if        BollaInput='F '
061600050923     c* sono mamma seguente se ho memorizzato la precedente nei campi di
061700050923     c*  output quindi --> Wok='S'
061800050923    5c                   if        wok='S'
061900050923     c                   eval      Bollainput='MS'
062000050923     c                   else
062100050923     c                   eval      Bollainput='MP'
062200050923    5c                   endif
062300050923    4c                   endif
062400050923    4c                   if        Bollainput='  '
062500050923     c                   eval      Bollainput='O '
062600050923    4c                   endif
062700050923     c
062800050926     c* al secondo passaggio labolla legata non è solo figlia ma anche mamma
062900050923    4c                   if        olv58oumfi='F'
063000050923     c                   eval      olv58oumfi='MS'
063100050923    4c                   endif
063200050923     c
063300050923    3c                   endif
063400050923     c
063500050923    2c                   enddo
063600050923     C*
063700050923     c*
063800050923     c                   if        olv58nsp=0
063900050923     c* Nessun legame specificato trovato
064000050923     c                   eval      olv58inmfi='NN'
064100050923     c                   else
064200050923     c                   movel     BollaInput    olv58inmfi
064300050923     c
064400050923     c                   exsr      CERMCT
064500050923     c                   endif
064600050923     c                   ENDSR
064700050712**
064800050712Tipo legame non gestito
064900050926Bolla in input inesistente sia in partenza che in arrivo
065000050712Codice bolla inesistente
065100050926non trovata bolla legata sia in partenza che in arrivo
