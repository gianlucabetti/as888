000100040119      *===============================================================*
000200060914      * Tist92r    * Confronto fatturazione (progetto 581)
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000060914     fTitas38c  if   e           k disk
001100060914     fTita730c  if   e           k disk
001200060914     fTitaI01L  if   e           k disk
001300060914     Ffiar531c  if   e           k disk
001400060914     Ftncsb03l  if   e           k disk
001500060914     fwfcft01l  uf a e           k disk
001600060914     Ftabel00f  if   e           k disk
001700070207     ftist92p   o    e             printer oflind(*in90)
001800040119      *
001900050603     d dAr5Ban       e ds
002000050603     d dAr5Bnb       e ds
002100050526     d kpjba         e ds
002200050609     d dstb          e ds
002300060915     d ds3a          e ds
002400050527     d dtas          e ds                  inz
002500050527     d  §tasflo      e                     extfld(tasflo)
002600060926      ** DS Flag operativi DS DTAS
002700060926     d Dtas01        e ds
002800050607      ** DS Calcolo tassazione  - Varie
002900050607     d Dtasv         e ds
003000050607     d  sv                     1     20
003100050607     d                                     Dim(20)                              Sigle varie
003200050607     d  va                    21    140P 3
003300050607     d                                     Dim(20)                              Importi varie
003400050607     d param           ds
003500060914     D  pardrf                 1      8  0
003600060914     D  paraam                 9     14  0
003700050614     d  paraa                  9     12  0
003800050614     d  parmm                 13     14  0
003900080714     d  parrif                15     15                                         'S' = rifatturazione
004000050607     d
004100050607     dwfcftxxds      e ds                  extname(wfcft00f)
004200050607     dsvpre                   31     50    dim(20)
004300050607     dvapre                   51    170p 3 dim(20)
004400050607     dsvcor                  184    203    dim(20)
004500050607     dvacor                  204    323p 3 dim(20)
004600050607     d*
004700040331      *
004800050526     D***
004900040331      *
005000040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005100050526      *
005200040331      * - Indici di schiera
005300040331     d xx              s              3  0 inz
005400050607     D k               s              5  0                                      indice
005500040331      * - Campi di comodo
005600050603     d kAr5Trd         s                   Like(Ar5Trd)
005700060914     d kmgs            s                   Like(TasMgs)
005800050607     D Flgta7          s              1  0
005900050609     d codut           s              1  0 Inz(1)
006000060915     D kcod            s                   like(tblcod)
006100060914     D ksvt            s                   like(taisvt)
006200060914     D wmm             s              2  0
006300070207     D totspe          s              9  0
006400070207     D tottas          s              9  0
006500060914      *
006600060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
006700060914      *
006800060914      * codici bolla da scartare
006900060914     D tbs             s              2    dim(50)
007000060915      * codici bolla che prevedono la tassazione di una singola varia
007100060915     D cbo             s              2    dim(50)
007200060915      *
007300040119
007400040119      *
007500060914      * mi posiziono con anno mese da elaborare
007600060914     c                   movel     parmm         kmgs
007700060914     c     ktas          setll     titas38c
007800050607     c                   do        *hival
007900060914     c                   clear                   dtas
008000060914     c                   clear                   dtasv
008100060914     c     paraa         reade     titas38c
008200060914     c                   if        %eof(titas38c)
008300050607     c                   leave
008400050607     c                   endif
008500050614     c
008600060914      * vado a fine se bolla con mese <> da quello passato
008700050614     c                   movel     tasmgs        wmm               2 0
008800060914     c                   if        parmm <> wmm
008900060914     c                   leave
009000050614     c                   endif
009100050614     c
009200060914      * scarto bolle che non sono state contabilizzate
009300060915     c                   If        tasfic = ' '
009400060914     c                   iter
009500060914     c                   endif
009600060914     c
009700060915
009800060915      * scarto  se si tratta di codice bolla che prevede solo una varia
009900060918     c                   z-add     1             kk                2 0
010000060915     c     tascbo        lookup    cbo(kk)                                05
010100060915     c                   if        *in05
010200060915     c                   iter
010300060915     c                   endif
010400060915     c
010500060914
010600060914      * scarto le bolle che sono state fatturate in filiale
010700060914     c                   If        tasfiv <> 998 and tasfiv <> 996
010800060914     c                   iter
010900060914     c                   endif
011000060914     c
011100060914
011200060914      * verifico se tassata fino all'imponibile in TITAI
011300060914     c                   eval      ksvt = '£'
011400060914     c     Ktai06        chain     titai01l
011500060914      * se trovato record imponibile  scarto la bolla
011600060914     c                   If        %found(titai01l)
011700060914     c                   iter
011800060914     c                   endif
011900060914     c
012000060914      * se presenti varia "N" o "&" + 88888888 ignoro il record
012100060914     c                   eval      ksvt = 'N'
012200060914     c     Ktai06        chain     titai01l
012300060914      * se trovato record imponibile  scarto la bolla
012400060914     c                   If        %found(titai01l)  and taivat = 88888888
012500060914     c                   iter
012600060914     c                   endif
012700060914
012800060914     c                   eval      ksvt = '&'
012900060914     c     Ktai06        chain     titai01l
013000060914      * se trovato record imponibile  scarto la bolla
013100060914     c                   If        %found(titai01l)  and taivat = 88888888
013200060914     c                   iter
013300060914     c                   endif
013400060914
013500060914      * scarto bolle di recupero, c/servizi, storni
013600060914      * scarto bolle raggruppate
013700050609     c     tastbl        lookup    tbs                                    30
013800060914     c                   if        *in30 = *off and tasxco = *blanks
013900060914
014000060914      * carico la tassazione corrente
014100060914     c                   exsr      sr_corrente
014200060914
014300060914      * eseguo la tassazione precedente
014400050526     c                   exsr      sr_sf20
014500050603     c                   endif
014600060914
014700050607     c                   enddo
014800050526      *
014900060914      *
015000070207      * stampo  la fine elaborazione
015100070207     c                   write     st92s1
015200070207
015300040119     c                   eval      *inLR = *on
015400060914      *
015500050603     c                   movel     'C'           tastla
015600050603     C                   CALL      'TNSF20R'
015700050603     C                   PARM                    KPJBA
015800050603     C                   PARM                    DTAS
015900050603     C                   PARM                    DTASV
016000050614     c*
016100050614     c                   movel(p)  param         kpjbu
016200040331      *
016300050526      **-------------------------------------------------------------**
016400050603      ** Richiamo pgm tassaz. tnsf20r                                **
016500050526      **-------------------------------------------------------------**
016600050526     c     sr_sf20       BEGSR
016700050526      *
016800050601     c                   movel     *Blanks       tastla
016900050601     c                   clear                   dAr5Ban
017000050601     c                   Clear                   dAr5Bnb
017100050603     c                   Clear                   kpjbu
017200060914     c                   clear                   dtasv
017300060914     c                   Clear                   taspor
017400060914     c                   Clear                   tasimv
017500050603     c                   eval      tassva = *blanks
017600080714      * se si tratta di ritassazione non imposto la data di riferimento
017700080714     c                   if        parrif = 'S'
017800080714     c                   movel(p)  tasaas        tasdsp
017900080714     c                   move      tasmgs        tasdsp
018000080714     c                   z-add     20080630      tasdct
018100080714     c                   else
018200080714      * se si tratta di confronto di fatturazione imposto le date di riferimento
018300050614     c                   z-add     pardrf        tasdsp
018400050614     c                   z-add     pardrf        tasdct
018500080714     c                   endif
018600080714
018700050601     c                   movel     Tasftc        tastc1
018800060914      * recupero i valori  già tassati
018900060914     c                   exsr      Ric_tai
019000050601      * Flag SI NO DDT
019100050601    1C                   If        tasll1 = 'C' or tasll1 = 'S'
019200050601     C                   movel     'S'           Tasddt
019300050601   x1C                   Else
019400050601     C                   Clear                   Tasddt
019500050601    1C                   Endif
019600060926     c***                movel     tasfbr        §tasflo
019700060926     c                   clear                   dtas01
019800060926     c                   movel     tasfbr        §asfbr
019900060926     c                   movel     tascca        §ascca
020000060926     c                   eval      §tasflo = dtas01
020100060926
020200060424     c
020300060424     c                   clear                   tasban
020400050601      * Bancali
020500050601    1c                   If        %Subst(TasGva:1:1) = 'B'
020600050601
020700050601     c                   eval      kAr5Trd  = 'BAN'
020800050601     c     kFiar5        Chain     Fiar531c
020900050601If  1c                   If        %Found(Fiar531c)
021000050601     c                   Movel     Ar5Uni        dAr5Ban
021100050601    1c                   EndIf
021200050601      * Bancali
021300050601     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
021400050601    1c                   EndIf
021500050601      **
021600050601      * Colli Originali
021700050601    1c                   If        %Subst(TasGva:1:1) = 'O'
021800050601     c                   eval      kAr5Trd  = 'BNB'
021900050601     c     kFiar5        Chain     Fiar531c
022000050601If  1c                   If        %Found(Fiar531c)
022100050601     c                   Movel     Ar5Uni        dAr5Bnb
022200050601    1c                   EndIf
022300050601      * Colli
022400050601     c                   Eval      TasNcl = §Ar5bcor
022500050601    1c                   EndIf
022600060914
022700050607     c                   clear                   tascas
022800050607     c                   clear                   tastic
022900050607     c                   clear                   tasvca
023000050607     c                   clear                   tascmb
023100070406      * C/Assegno
023200050607    2c                   IF        Tasfca <> *blanks
023300050607     c                   Exsr      Carcsb
023400050607    2c                   Endif
023500070406      * Assicurazione
023600070406    2c                   IF        Tasfcm = 'F'
023700070406     c                   clear                   tasias
023800070406     c                   clear                   tasvas
023900070406    2c                   Endif
024000060914
024100050526     c                   call      'TNSF20R'
024200060914     c                   PARM                    KPJBA
024300060914     c                   PARM                    DTAS
024400060914     c                   PARM                    Dtasv
024500060914
024600070207     c                   add       1             totspe
024700050607     c                   select
024800060914     c                   when      taserr = *blanks
024900080714      * se si tratta di confronto fatturazione tolgo la varia "f"
025000050607     c                   exsr      SR_riewf
025100080714
025200050607     c                   write     wfcft000
025300070207     c                   add       1             tottas
025400050607     c                   endsl
025500050607     c     e_sf20        endsr
025600050607      ****************************************************
025700050607      ** Carica importi varie da tita7000
025800050607      ****************************************************
025900050607     c     Carta7        Begsr
026000050607      * pulisco le schiere delle varie
026100050607     c                   Do        20            k
026200050607     c                   If        k > 3
026300050607     c                   clear                   VA(K)
026400050607     c                   clear                   SV(K)
026500050607     c                   Endif
026600050607     c                   Enddo
026700060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
026800050607     c                   clear                   flgta7
026900050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
027000050607     c                   If        tassv3 <> *blanks
027100050607      * mi posiziono su tita7
027200050607     c     Kta7          setll     tita730c
027300050607     c                   if        %equal(tita730c)
027400050607     c                   z-add     3             k
027500050607      **
027600050607     c                   do        *hival
027700050607     c     Kta7          reade     tita730c
027800050607      * fine file
027900050607     c                   if        %eof(tita730c)
028000050607     c                   leave
028100050607     c                   endif
028200050607      * carico le varie
028300050607     c                   add       1             k
028400050607     c                   movel     ta7svn        sv(k)
028500050607     c                   z-add     ta7van        va(k)
028600050607     c                   z-add     1             flgta7
028700050607     c                   enddo
028800050607
028900050607     c                   endif
029000050607
029100050607     c                   endif
029200050607
029300050607     c                   Endsr
029400060914      *
029500060914      *****************************************************************
029600060914      *  Recupero importi di tassazione dal file titai
029700060914      *****************************************************************
029800060914     c     Ric_tai       Begsr
029900060914      *
030000060914     c                   z-add     0             k
030100060914
030200060914     c     ktai05        setll     titai01l
030300060914
030400060914     c                   If        %equal(titai01l)
030500060914
030600060914     c                   do        *hival
030700060914
030800060914     c     ktai05        reade     titai01l
030900060914
031000060914     c                   If        %eof(titai01l)
031100060914     c                   leave
031200060914     c                   endif
031300060914      * imponibile
031400060914     c                   if        taisvt = '£'
031500060914     c                   eval      tasimv = taivat
031600060914     c                   endif
031700060914      * porto
031800060914     c                   if        taisvt = ' '
031900060914     c                   eval      taspor = taivat
032000060914     c                   endif
032100060914      * varie
032200060914     c                   if        taisvt <> ' ' and taisvt <> '£'
032300060914     c                   add       1             k
032400060914     c                   eval      sv(k) = taisvt
032500060914     c                   eval      va(k) = taivat
032600060914     c                   endif
032700060914
032800060914     c                   enddo
032900060914
033000060914     c                   else
033100060914      * se non è pretassata pulisco la divisa
033200060914     c                   clear                   tasdiv
033300060914     c                   endif
033400060914
033500060914     c                   endsr
033600050607      ****************************************************
033700050609      ** Ricerca Contrassegno
033800050607      ****************************************************
033900050607     c     Carcsb        Begsr
034000050607
034100050607     c     Kcsb          chain     Tncsb03l
034200050607     c                   If        %found(tncsb03l) and
034300050607      * solo per stesso tipo bolla
034400050607     c                             tastbl = csbtbl
034500050607     c                   movel     csbsta        Tassta
034600050607     c                   z-add     csbcas        Tascas
034700061016     c****               movel     csbtpa        Tastic
034800050607     c                   if        csbfus <> *blanks
034900050607     c                   move      csbfus        tastic
035000050607     c                   else
035100050607     c                   move      csbtpi        tastic
035200050607     c                   end
035300050607      * mittente
035400050607     c                   movel     csbvca        tasvca
035500050607     c                   z-add     csbcmb        tascmb
035600050607
035700050607     c                   endif
035800050607
035900050607     c                   endsr
036000050607      *************************************************************************
036100060914      * scrittura w-file dati tassazione corrente
036200050607      *************************************************************************
036300060914     C     sr_corrente   BEGSR
036400050607     c                   clear                   wfcft000
036500050607     c                   z-add     tasaas        CFTAAS
036600050607     c                   z-add     taslnp        CFTLNP
036700050607     c                   z-add     tasnrs        CFTNRS
036800050607     c                   z-add     tasnsp        CFTNSP
036900050607     c                   z-add     tasksc        CFTKSC
037000050607     c                   z-add     tasctr        CFTCTR
037100060914     c                   z-add     tasimv        CFTIMVC
037200060914     c                   z-add     taspor        CFTPORC
037300060914      * carico eventuali varie presenti in TITA7
037400060914     c                   Exsr      Carta7
037500060914
037600060915      * cerco la varia "D" (diritto di fatturazione)
037700060915      * se c'è la tolgo dall'imponibile e dalle varie
037800060915     c                   z-add     1             k
037900060915     c     'D'           lookup    sv(k)                                  30
038000060915     c                   if        %found
038100080121     c                   eval      cftimvc = cftimvc - va(k)
038200060915     c                   clear                   va(k)
038300060915     c                   clear                   sv(k)
038400060915     c                   endif
038500061218
038600080714      * se si tratta di confronto fatturazione
038700061218      * cerco la varia "f" (supplemento carburante)
038800061218      * se c'è la tolgo dall'imponibile e dalle varie
038900080714     c                   If        parrif = ' '
039000061218     c                   z-add     1             k
039100061218     c     'f'           lookup    sv(k)                                  30
039200061218     c                   if        %found
039300080121     c                   eval      cftimvc = cftimvc - va(k)
039400061218     c                   clear                   va(k)
039500061218     c                   clear                   sv(k)
039600061218     c                   endif
039700080714     c                   endif
039800060915     c
039900060914     c                   movea     va            vacor
040000060914     c                   movea     sv            svcor
040100050607     c                   endsr
040200050607      *************************************************************************
040300060914      * scrittura w-file dati tassazione precdeente
040400050607      *************************************************************************
040500060914     C     sr_riewf      BEGSR
040600061218
040700080714     c                   If        parrif = ' '
040800061218      * cerco la varia "f" (supplemento carburante)
040900061218      * se c'è la tolgo dall'imponibile e dalle varie
041000061218     c                   z-add     1             k
041100061218     c     'f'           lookup    sv(k)                                  30
041200061218     c                   if        %found
041300061218     c                   eval      tasimv  = tasimv - va(k)
041400061218     c                   clear                   va(k)
041500061218     c                   clear                   sv(k)
041600061218     c                   endif
041700080714     c                   endif
041800061218
041900060914     c                   z-add     tasimv        CFTIMVP
042000060914     c                   z-add     taspor        CFTPORP
042100060914     c                   movea     va            vapre
042200060914     c                   movea     sv            svpre
042300050607     c                   endsr
042400050609      *************************************************************************
042500050609      * Operazioni iniziali
042600050609      *************************************************************************
042700050609     C     *inzsr        BEGSR
042800050609     c*
042900060914     c     ktas          klist
043000060914     c                   kfld                    paraa
043100060914     c                   kfld                    Kmgs
043200060914
043300060914     c     ktai05        klist
043400060914     c                   kfld                    tasaas
043500060914     c                   kfld                    taslnp
043600060914     c                   kfld                    tasnrs
043700060914     c                   kfld                    tasnsp
043800060914     c                   kfld                    tastbl
043900060914
044000060914     c     ktai06        klist
044100060914     c                   kfld                    tasaas
044200060914     c                   kfld                    taslnp
044300060914     c                   kfld                    tasnrs
044400060914     c                   kfld                    tasnsp
044500060914     c                   kfld                    tastbl
044600060914     c                   kfld                    Ksvt
044700060914
044800050609     c     kta7          klist
044900050609     c                   kfld                    tasaas
045000050609     c                   kfld                    taslnp
045100050609     c                   kfld                    tasnrs
045200050609     c                   kfld                    tasnsp
045300050609     c                   kfld                    tastbl
045400050609
045500050609     c     kFiar5        Klist
045600050609     c                   Kfld                    TasAas
045700050609     c                   Kfld                    TasLnp
045800050609     c                   Kfld                    TasNrs
045900050609     c                   Kfld                    TasNsp
046000050609     c                   Kfld                    kAr5Trd
046100050609
046200050609     c     KCSB          KLIST
046300050609     c                   KFLD                    TASAAS
046400050609     c                   KFLD                    TASLNP
046500050609     c                   KFLD                    TASNRS
046600050609     c                   KFLD                    TASNSP
046700050609
046800050609     c     Kcft          KLIST
046900050609     c                   KFLD                    TASAAS
047000050609     c                   KFLD                    TASLNP
047100050609     c                   KFLD                    TASNRS
047200050609     c                   KFLD                    TASNSP
047300050609
047400050609     C     KTAB          KLIST
047500050609     C                   KFLD                    CODUT
047600050609     C                   KFLD                    KCOD
047700050609     c
047800050609     c     *entry        plist
047900050609     c                   parm                    kpjba
048000050609     c                   movel     kpjbu         param
048100060915
048200060915      * caricamento schiera dei tipi bolla da scartare
048300060915     c                   eval      Kcod  = 'TB'
048400050609     c                   clear                   tbs
048500050609     c                   z-add     0             xx
048600050609     c     ktab          setll     tabel00f
048700050609     c                   do        *hival
048800050609     c     ktab          reade     tabel00f
048900050609     c                   if        %eof(tabel00f)
049000050609     c                   leave
049100050609     c                   endif
049200050609     c                   movel     tbluni        dstb
049300050609     c                   if        §tbrbl = 'R' or §tbfcb <> '1'
049400050609     c                   add       1             xx
049500050609     c                   movel     tblkey        tbs(xx)
049600050609     c                   endif
049700050609     c                   enddo
049800060915      * caricamento schiera dei codici bolla da scartare
049900060915     c                   eval      Kcod  = '3A'
050000060915     c                   z-add     0             xx
050100060915     c     ktab          setll     tabel00f
050200060915     c                   do        *hival
050300060915     c     ktab          reade     tabel00f
050400060915     c                   if        %eof(tabel00f)
050500060915     c                   leave
050600060915     c                   endif
050700060915     c                   movel     tbluni        ds3a
050800060915     c                   if        §3asva <> *blanks
050900060915     c                   add       1             xx
051000060915     c                   movel     tblkey        cbo(xx)
051100060915    3c                   endif
051200060915     c                   enddo
051300060915
051400050609     c                   endsr
