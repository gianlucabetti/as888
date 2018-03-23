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
008100050923     C                   CLEAR                   wok
008200050923     C                   CLEAR                   bollainput
008300050712     C* ERRORI
008400050926    1C                   IF        ILV58TLE<>' '
008500050926     c     ilv58tle      lookup    tle                                    30
008600050926     c                   if        not *in30
008700050926     C                   MOVEL     'E'           OLV58ERR
008800050926     C                   MOVEL     MSG(1)        OLV58MSG
008900050926     C                   GOTO      FINE
009000050926    1c                   endif
009100050926    1c                   endif
009200050712     c
009300050712     c* Se non passato il porto lo determino
009400050714    1c                   if        ilv58por=' '
009500050712     c* chaino la bolla
009600050923     c     kbollA        CHAIN     FNarb01L
009700050923    3C                   if        not %found(fnarb01l)
009800050712     c     kbollA        CHAIN     FNBLP01L
009900050712    2C                   if        not %found(fnblp01l)
010000050712     C                   MOVEL     'E'           OLV58ERR
010100050712     C                   MOVEL     MSG(2)        OLV58MSG
010200050712     C                   GOTO      FINE
010300050712    3c                   endif
010400050712    2c                   endif
010500050712     c
010600050923     c                   eval      wccaprec=arbcca
010700050923     c                   eval      wcbo=arbcbo
010800050714     c                   EXSR      CERTBO
010900050714    c
011000050714     c                   if        olv58err<>' '
011100050714     c                   goto      fine
011200050714     c                   endif
011300050714     c
011400050909     c                   movel     §3atb1        olv58inpor
011500050712   x1c                   else
011600050909     c                   movel     ilv58por      olv58inpor
011700050712    1c                   endif
011800050712     c
011900050712     C*******************+
012000050909     c* Richiesto specifico legame bolla
012100050712     C*******************+
012200050909    1C                   IF        ILV58TLE<>' '
012300050923     c                   EXSR      CERTipoLeg
012400050923     c                   else
012500050923     c                   EXSR      LeggiLegami
012600050923     c                   endif
012700050923     c
012800050923    0C                   ENDIF
012900940323     C*
013000050712     c     fine          tag
013100050712     c
013200050714     C     Ilv58tla      IFNE      ' '
013300970902     C                   SETON                                        LR
013400050714     C**
013500050714     C                   CLEAR                   Tisi95DS
013600050714     C                   MOVEL     'C'           I95TLA
013700050714     C                   CALL      'TISI95R'
013800050714     C                   PARM                    Tisi95DS
013900970902     C                   ELSE
014000970902     C                   SETON                                        RT
014100970902     C                   ENDIF
014200050909     c*---------------------------------------------------------------------
014300050909     c* Impostazine campi per ricerca legami bolla
014400050909     c*---------------------------------------------------------------------
014500050909     c     IMPOCERCA     BEGSR
014600050909     c                   eval      kaas=ilv58aas
014700050909     c                   eval      klnp=ilv58lnp
014800050909     c                   eval      knrs=ilv58nrs
014900050909     c                   eval      knsp=ilv58nsp
015000050909     c*
015100050909     c                   clear                   lblnsp
015200050909     c                   clear                   lblnsN
015300050909     c                   clear                   esistelbl
015400050923     C                   ENDSR
015500050714     c*---------------------------------------------------------------------
015600050714     c* cerco legami bolla
015700050714     c*---------------------------------------------------------------------
015800050909     c     CERLEG        BEGSR
015900050714     c                   clear                   esistelbl         1
016000050909     c                   if        tipolbl='M'
016100050714     c     klegame       chain     fnlbl01l
016200050909     c                   else
016300050909     c     klegame       chain     fnlbl02l
016400050909     c                   endif
016500050714     c
016600050909    1c                   if        not %found
016700050714     c                   eval      esistelbl='NL'
016800050714     c                   goto      endcerleg
016900050714    1c                   endif
017000050714     c
017100050909     c* Bolla precedente trovata
017200050909     c                   if        tipolbl='M'
017300050714     c                   eval      kaas=lblaap
017400050714     c                   eval      klnp=lbllpp
017500050714     c                   eval      knrs=lblnrp
017600050714     c                   eval      knsp=lblnsp
017700050909     c                   else
017800050909     c* Bolla seguente   trovata
017900050909     c                   eval      kaas=lblaan
018000050909     c                   eval      klnp=lbllpn
018100050909     c                   eval      knrs=lblnrn
018200050909     c                   eval      knsp=lblnsn
018300050909     c                   endif
018400050909     c
018500050923     c     klegame       CHAIN     FNarb01L
018600050923    2C                   if        not %found(fnarb01l)
018700050714     c     klegame       CHAIN     FNBLP01L
018800050714    1C                   if        not %found(fnblp01l)
018900050714     c                   eval      esistelbl='NB'
019000050714     c                   goto      endcerleg
019100050714    2c                   endif
019200050714    1c                   endif
019300050714     c
019400050714     c     endcerleg     endsr
019500950105     C*---------------------------------------------------------------*
019600950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
019700950105     C*---------------------------------------------------------------*
019800950105     C     DEFVAR        BEGSR
019900950105     C*
020000950105     C     *ENTRY        PLIST
020100050712     C                   PARM                    fnlv58ds
020200950105     C*
020300050712     C     Kbolla        KLIST
020400050712     C                   KFLD                    Ilv58AAS
020500050712     C                   KFLD                    Ilv58LNP
020600050712     C                   KFLD                    Ilv58NRS
020700050712     C                   KFLD                    Ilv58NSP
020800050712     C     KLEGAmE       KLIST
020900050712     C                   KFLD                    KAAS
021000050712     C                   KFLD                    kLNP
021100050712     C                   KFLD                    kNRS
021200050712     C                   KFLD                    kNSP
021300050712     C     KTAB          KLIST
021400050712     C                   KFLD                    codut
021500050712     C                   KFLD                    kcod
021600050712     C                   KFLD                    kkey
021700050923     C     KTAB2         KLIST
021800050923     C                   KFLD                    codut
021900050923     C                   KFLD                    kcod
022000050923     c
022100050923     c* Carico solo la prima volta la tabella dei tipi legami, com relatica
022200050923     c*  consegna anomala
022300050923     c                   if        Wcarica=' '
022400050923     c                   clear                   xx
022500050923     c                   movel     '7O'          kcod
022600050923     c     ktab2         setll     tabel00f
022700050923     c     ktab2         reade     tabel00f
022800050923     c                   dow       not %eof(tabel00f)
022900050923     c                   if        tblflg=' '
023000050923     c                   movel     tbluni        ds7o
023100050923     c                   if        §7otle<>' '
023200050923     c                   add       1             xx
023300050923     c                   movel     tblkey        cca(xx)
023400050923     c                   movel     §7otle        tle(xx)
023500050923     c                   endif
023600050923     c                   endif
023700050923     c     ktab2         reade     tabel00f
023800050923     c                   enddo
023900050923     c
024000050923     c                   eval      wcarica='S'
024100050923     c                   endif
024200950105     C*
024300950105     C                   ENDSR
024400050714     C*---------------------------------------------------------------*
024500050909     C* Certo tipo bolla per avere il porto
024600050714     C*---------------------------------------------------------------*
024700050714     c     CERTBO        BEGSR
024800050714     c                   movel     '3A'          kcod
024900050714     c                   movel(P)  wcbo          kkey
025000050714     c     ktab          chain     tabel00f
025100050714    2c                   if        %found(tabel00f) and tblflg=' '
025200050714     c                   movel     tbluni        ds3a
025300050714     c                   else
025400050714     c                   clear                   ds3a
025500050714     C                   MOVEL     'E'           OLV58ERR
025600050714     C                   MOVEL     MSG(3)        OLV58MSG
025700050714    2c                   endif
025800050714     c                   endSR
025900050714     C*---------------------------------------------------------------*
026000050714      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
026100050714     C*---------------------------------------------------------------*
026200050714     c     CERMCT        BEGSR
026300050714     C                   CLEAR                   Tisi95DS
026400050714     C                   MOVEL     wlom          I95LOC
026500050714     C                   MOVEL     olv58cam      I95CAP
026600050714     C                   MOVEL     olv58nzm      I95NAR
026700050714     C                   MOVEL     '3'           I95TCN
026800050714     C                   MOVEL     olv58aas      I95DAT
026900050714     C                   MOVE      wmgs          I95DAT
027000050714     c                   exsr      CERTBO
027100050714     c
027200050714     C     §3AFCA        IFNE      0
027300050714     C                   MOVEL     'S'           I95FCA
027400050714     C                   ENDIF
027500050714     C                   MOVEL     §3ATB1        I95TPO
027600050714     c* Certo ntw della lnp
027700050714     c     olv58lnp      chain     azorg01l
027800050714     c                   if        %found(azorg01l)
027900050714     c                   movel     orgde3        og143
028000050714     c                   else
028100050714     c                   clear                   og143
028200050714     c                   endif
028300050714     c
028400050714     c     §ogntw        ifeq      'FED'
028500050714     c                   movel     'S'           i95fi1
028600050714     c                   endif
028700050714     c**
028800050714     C                   CALL      'TISI95R'
028900050714     C                   PARM                    Tisi95DS
029000050714     C     O95ERR        IFEQ      *BLANKS
029100050714     C                   MOVEL     O95CTS        olv58mct
029200050714     C                   ENDIF
029300050714     c
029400050714     c                   ENDSR
029500050923     c*---------------------------------------------------------------------
029600050923     c* cerco uno specifico legame bolla passato in input
029700050923     c*---------------------------------------------------------------------
029800050923     c     CERTipoLeg    BEGSR
029900050923     c                   eval      olv58tle=ilv58tle
030000050923     c
030100050923     c* Impostazioni per ricerca legami
030200050923     c                   exsr      impocerca
030300050923     c                   eval      tipolbl='M'
030400050923     c
030500050923     c* Cerco bolla PRECEDENTE col tipo legame richiesto
030600050923    2c                   dou       cca(xx)<>ilv58tle  or esistelbl<>'  '
030700050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
030800050923    3c                   if        lblnsp>0
030900050923     c                   if        BoOriginale=BoMamma
031000050923     c                   eval      olv58oumfi='O '
031100050923     c                   else
031200050923     c                   eval      olv58oumfi='MP'
031300050923     c                   endif
031400050923     c                   eval      olv58inmfi='F '
031500050923     c                   eval      olv58aas=lblaap
031600050923     c                   eval      olv58lnp=lbllpp
031700050923     c                   eval      olv58nrs=lblnrp
031800050923     c                   eval      olv58nsp=lblnsp
031900050923     c                   eval      olv58tfp=arbtfp
032000050923     c                   eval      olv58fap=arbfap
032100050923     c                   eval      olv58cam=arbcam
032200050923     c                   eval      olv58nzm=arbnzm
032300050923     c                   eval      olv58cts=arbcts
032400050923     c                   eval      olv58fin=arbfin
032500050923     c                   eval      olv58cad=arbcad
032600050923     c                   eval      olv58nzd=arbnzd
032700050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
032800050923     c                   eval      wlom=arblom
032900050923     c                   eval      wcbo=arbcbo
033000050923     c                   eval      wmgs=arbmgs
033100050923     c* memorizzo di aver trovato una bolla mamma giusta
033200050923     c                   eval      wok='S'
033300050923    3C                   ENDIF
033400050923     c*
033500050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
033600050923     c*  il giro trattandola da figlia solo se non ho letto l'originale
033700050923    3c                   if        olv58oumfi<>'O'
033800050923     c                   exsr      CERLEG
033900050923     c
034000050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
034100050923    4c                   if        esistelbl='NB'
034200050923     C                   MOVEL     'E'           OLV58ERR
034300050923     C                   MOVEL     MSG(4)        OLV58MSG
034400050923    4c                   endif
034500050923     c*  Se ho trovato la bolla mamma, che sia del tipo legame richiesto
034600050923     c*  o meno, significa che la bolla passata è figlia: lo memorizzo
034700050923     c                   if        esistelbl='  '
034800050923     c                   eval      Bollainput='F '
034900050923     c                   endif
035000050923
035100050923   x3c                   else
035200050923     c* Per uscire dal ciclo imposto "XX" in esistelbl
035300050923     c                   eval      esistelbl='XX'
035400050923    3c                   endif
035500050923     c
035600050923     c                   z-add     1             xx
035700050923     c     arbcca        lookup    cca(xx)                                30
035800050923     c
035900050923     c                   if        not *in30
036000050923     c                   z-add     50            xx
036100050923     c                   endif
036200050923     c
036300050923    2c                   enddo
036400050923     c
036500050923     c* Calcolo il cod.tassazione mittente se trovato legame
036600050923    2c                   if        olv58nsp>0
036700050923     c                   EXSR      CERMCT
036800050923    2c                   endif
036900050923     c
037000050923     c                   EXSR      Impocerca
037100050923     c                   eval      tipolbl='F'
037200050923     c
037300050923     c**********************
037400050923     c* Cerco bolla SEGUENTE  col tipo legame richiesto
037500050923     c**********************
037600050923     c* Solo se la bolla passata presenta il tipo legame richeisto
037700050923   1ac                   if        wccaprec=ilv58tle
037800050923     c
037900050923    2c                   dou       cca(xx)<>ilv58tle  or  esistelbl<>'  '
038000050923     c
038100050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
038200050923    3c                   if        lblnsn>0
038300050923     c* do precedenza ai dati della mamma se trovo anche quelli
038400050923     c*  della figlia (bolla in mezzo!!)
038500050923    4c                   if        Wok=' '
038600050923     c                   eval      olv58oumfi='F'
038700050923     c                   eval      olv58aas=lblaan
038800050923     c                   eval      olv58lnp=lbllpn
038900050923     c                   eval      olv58nrs=lblnrn
039000050923     c                   eval      olv58nsp=lblnsn
039100050923     c                   eval      olv58tfp=arbtfp
039200050923     c                   eval      olv58fap=arbfap
039300050923     c                   eval      olv58cam=arbcam
039400050923     c                   eval      olv58nzm=arbnzm
039500050923     c                   eval      olv58cts=arbcts
039600050923     c                   eval      olv58fin=arbfin
039700050923     c                   eval      olv58cad=arbcad
039800050923     c                   eval      olv58nzd=arbnzd
039900050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bollla
040000050923     c                   eval      wlom=arblom
040100050923     c                   eval      wcbo=arbcbo
040200050923     c                   eval      wmgs=arbmgs
040300050923    4C                   ENDIF
040400050923    3C                   ENDIF
040500050923     c*
040600050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
040700050923     c*  il giro trattandola da figlia
040800050923     c                   exsr      CERLEG
040900050923     c
041000050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
041100050923    3c                   if        esistelbl='NB'
041200050923     C                   MOVEL     'E'           OLV58ERR
041300050923     C                   MOVEL     MSG(4)        OLV58MSG
041400050923    3c                   endif
041500050923
041600050923     c*  Se ho trovato la bolla figlia, che sia del tipo legame richiesto
041700050923     c*  o meno, significa che la bolla passata è mamma: lo memorizzo
041800050923    3c                   if        esistelbl='  '
041900050923     c
042000050923    4c                   if        BollaInput='F '
042100050923     c* sono mamma seguente se ho memorizzato la precedente nei campi di
042200050923     c*  output quindi --> Wok='S'
042300050923    5c                   if        wok='S'
042400050923     c                   eval      Bollainput='MS'
042500050923     c                   else
042600050923     c                   eval      Bollainput='MP'
042700050923    5c                   endif
042800050923    4c                   endif
042900050923    4c                   if        Bollainput='  '
043000050923     c                   eval      Bollainput='O '
043100050923    4c                   endif
043200050923     c
043300050923     c* al secondo passaggio, anche se non esatto il tipo legame, la
043400050923     c*  bolla legata non è solo figlia ma anche mamma
043500050923    4c                   if        olv58oumfi='F'
043600050923     c                   eval      olv58oumfi='MS'
043700050923    4c                   endif
043800050923     c
043900050923    3c                   endif
044000050923     c
044100050923     c                   z-add     1             xx
044200050923     c     wccaprec      lookup    cca(xx)                                30
044300050923     c                   movel     arbcca        wccaprec
044400050923     c
044500050923     c                   if        not *in30
044600050923     c                   z-add     50            xx
044700050923     c                   endif
044800050923     c
044900050923    2c                   enddo
045000050923   1aC                   ENDIF
045100050923     c*
045200050923     C*
045300050923     c*
045400050923     c                   if        olv58nsp=0
045500050923     c* Nessun legame specificato trovato
045600050923     c                   eval      olv58inmfi='NN'
045700050923     c                   else
045800050923     c                   movel     BollaInput    olv58inmfi
045900050923     c
046000050923     c                   exsr      CERMCT
046100050923     c                   endif
046200050923     c                   ENDSR
046300050923     c
046400050923     c*---------------------------------------------------------------------
046500050923     c* cerco tutti i legami bolla
046600050923     c*---------------------------------------------------------------------
046700050923     c     LeggiLegami   BEGSR
046800050923     c
046900050923     c* Impostazioni per ricerca legami
047000050923     c                   exsr      impocerca
047100050923     c                   eval      tipolbl='M'
047200050923     c
047300050923     c************************
047400050923     c* Cerco bolla PRECEDENTE
047500050923     c************************
047600050923    2c                   dou       esistelbl<>'  '
047700050923     c
047800050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
047900050923    3c                   if        lblnsp>0
048000050923     c                   if        BoOriginale=BoMamma
048100050923     c                   eval      olv58oumfi='O '
048200050923     c                   else
048300050923     c                   eval      olv58oumfi='MP'
048400050923     c                   endif
048500050923     c                   eval      olv58inmfi='F '
048600050923     c                   eval      olv58aas=lblaap
048700050923     c                   eval      olv58lnp=lbllpp
048800050923     c                   eval      olv58nrs=lblnrp
048900050923     c                   eval      olv58nsp=lblnsp
049000050923     c                   eval      olv58tfp=arbtfp
049100050923     c                   eval      olv58fap=arbfap
049200050923     c                   eval      olv58cam=arbcam
049300050923     c                   eval      olv58nzm=arbnzm
049400050923     c                   eval      olv58cts=arbcts
049500050923     c                   eval      olv58fin=arbfin
049600050923     c                   eval      olv58cad=arbcad
049700050923     c                   eval      olv58nzd=arbnzd
049800050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bolla
049900050923     c                   eval      wlom=arblom
050000050923     c                   eval      wcbo=arbcbo
050100050923     c                   eval      wmgs=arbmgs
050200050923     c* memorizzo di aver trovato una bolla mamma giusta
050300050923     c                   eval      wok='S'
050400050926     c* Imposto il tipo legame
050500050926     c                   z-add     1             xx
050600050926     c     arbcca        lookup    cca(XX)                                30
050700050926    4c   30              if        olv58tle=' '
050800050926     c                   movel     tle(XX)       olv58tle
050900050926   x4c                   else
051000050926    5c                   if        olv58tle<>tle(XX)
051100050926     c                   eval      olv58tle='N'
051200050926    5c                   endif
051300050926    4c                   endif
051400050926     c
051500050923    3C                   ENDIF
051600050923     c*
051700050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
051800050923     c*  il giro trattandola da figlia solo se non ho letto l'originale
051900050923    3c                   if        olv58oumfi<>'O'
052000050923     c                   exsr      CERLEG
052100050923     c
052200050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
052300050923    4c                   if        esistelbl='NB'
052400050923     C                   MOVEL     'E'           OLV58ERR
052500050923     C                   MOVEL     MSG(4)        OLV58MSG
052600050923    4c                   endif
052700050926     c*  Se ho trovato la bolla mamma,
052800050926     c*   significa che la bolla passata è figlia: lo memorizzo
052900050923     c                   if        esistelbl='  '
053000050923     c                   eval      Bollainput='F '
053100050923     c                   endif
053200050923
053300050923   x3c                   else
053400050923     c* Per uscire dal ciclo imposto "XX" in esistelbl
053500050923     c                   eval      esistelbl='XX'
053600050923    3c                   endif
053700050923    2c                   enddo
053800050923     c
053900050923     c* Calcolo il cod.tassazione mittente se trovato legame
054000050923    2c                   if        olv58nsp>0
054100050923     c                   EXSR      CERMCT
054200050923    2c                   endif
054300050923     c
054400050923     c                   EXSR      Impocerca
054500050923     c                   eval      tipolbl='F'
054600050923     c
054700050923     c**********************
054800050923     c* Cerco bolla SEGUENTE
054900050923     c**********************
055000050923     c
055100050923    2c                   dou       esistelbl<>'  '
055200050923     c
055300050923     c* Mi salvo i dati che mi servono prima di leggerne un'altra
055400050923    3c                   if        lblnsn>0
055500050923     c* do precedenza ai dati della mamma se trovo anche quelli
055600050923     c*  della figlia (bolla in mezzo!!)
055700050923    4c                   if        Wok=' '
055800050923     c                   eval      olv58oumfi='F'
055900050923     c                   eval      olv58aas=lblaan
056000050923     c                   eval      olv58lnp=lbllpn
056100050923     c                   eval      olv58nrs=lblnrn
056200050923     c                   eval      olv58nsp=lblnsn
056300050923     c                   eval      olv58tfp=arbtfp
056400050923     c                   eval      olv58fap=arbfap
056500050923     c                   eval      olv58cam=arbcam
056600050923     c                   eval      olv58nzm=arbnzm
056700050923     c                   eval      olv58cts=arbcts
056800050923     c                   eval      olv58fin=arbfin
056900050923     c                   eval      olv58cad=arbcad
057000050923     c                   eval      olv58nzd=arbnzd
057100050923     c* Per calcolare il MCT servono: data sped/loc mitt/cod.bollla
057200050923     c                   eval      wlom=arblom
057300050923     c                   eval      wcbo=arbcbo
057400050923     c                   eval      wmgs=arbmgs
057500050923    4C                   ENDIF
057600050926     c
057700050926     c* Imposto il tipo legame,anche se già trovata una bolla mamma
057800050926     c                   z-add     1             xx
057900050926     c     arbcca        lookup    cca(XX)                                30
058000050926    4c   30              if        olv58tle=' '
058100050926     c                   movel     tle(XX)       olv58tle
058200050926   x4c                   else
058300050926    5c                   if        olv58tle<>tle(XX)
058400050926     c                   eval      olv58tle='N'
058500050926    5c                   endif
058600050926    4c                   endif
058700050923    3C                   ENDIF
058800050923     c*
058900050923     c* La key KLEGAME è impostata con la bolla mamma: rifaccio
059000050923     c*  il giro trattandola da figlia
059100050923     c                   exsr      CERLEG
059200050923     c
059300050923     c* Se non trovata bolla legata negli archivi bolle: errore!!
059400050923    3c                   if        esistelbl='NB'
059500050923     C                   MOVEL     'E'           OLV58ERR
059600050923     C                   MOVEL     MSG(4)        OLV58MSG
059700050923    3c                   endif
059800050923
059900050926     c*  Se ho trovato la bolla figlia,
060000050926     c*   significa che la bolla passata è mamma: lo memorizzo
060100050923    3c                   if        esistelbl='  '
060200050923     c
060300050923    4c                   if        BollaInput='F '
060400050923     c* sono mamma seguente se ho memorizzato la precedente nei campi di
060500050923     c*  output quindi --> Wok='S'
060600050923    5c                   if        wok='S'
060700050923     c                   eval      Bollainput='MS'
060800050923     c                   else
060900050923     c                   eval      Bollainput='MP'
061000050923    5c                   endif
061100050923    4c                   endif
061200050923    4c                   if        Bollainput='  '
061300050923     c                   eval      Bollainput='O '
061400050923    4c                   endif
061500050923     c
061600050926     c* al secondo passaggio labolla legata non è solo figlia ma anche mamma
061700050923    4c                   if        olv58oumfi='F'
061800050923     c                   eval      olv58oumfi='MS'
061900050923    4c                   endif
062000050923     c
062100050923    3c                   endif
062200050923     c
062300050923    2c                   enddo
062400050923     C*
062500050923     c*
062600050923     c                   if        olv58nsp=0
062700050923     c* Nessun legame specificato trovato
062800050923     c                   eval      olv58inmfi='NN'
062900050923     c                   else
063000050923     c                   movel     BollaInput    olv58inmfi
063100050923     c
063200050923     c                   exsr      CERMCT
063300050923     c                   endif
063400050923     c                   ENDSR
063500050712**
063600050712Tipo legame non gestito
063700050926Bolla in input inesistente sia in partenza che in arrivo
063800050712Codice bolla inesistente
063900050926non trovata bolla legata sia in partenza che in arrivo
