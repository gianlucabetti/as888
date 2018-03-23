000100040119      *===============================================================*
000200150713      * TicfV1r    * Tassazione bolle con peso maggiorato
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000060914     fTitaI01L  if   e           k disk
001100060914     Ffiar531c  if   e           k disk
001200060914     Ftncsb03l  if   e           k disk
001300150713     fwfvtp00F  o  a e             disk
001400150713     Ftabel00f  if   e           k disk
001500150713     Fsipes01l  if   e           k disk
001600081105     Ffnevb04l  if   e           k disk
001700150713     fticfv1p   o    e             printer oflind(*in90)
001800040119      *
001900050603     d dAr5Ban       e ds
002000050603     d dAr5Bnb       e ds
002100050526     d kpjba         e ds
002200050609     d dstb          e ds
002300060915     d ds3a          e ds
002400110701      ** DS del tasflo del file TITAS00F
002500110701     D DTASFLO       E DS
002600050527     d dtas          e ds                  inz
002700050527     d  §tasflo      e                     extfld(tasflo)
002800060926      ** DS Flag operativi DS DTAS
002900060926     d Dtas01        e ds
003000050607      ** DS Calcolo tassazione  - Varie
003100050607     d Dtasv         e ds
003200050607     d  sv                     1     20
003300050607     d                                     Dim(20)                              Sigle varie
003400050607     d  va                    21    140P 3
003500050607     d                                     Dim(20)                              Importi varie
003600050607     d param           ds
003700120314     D  paraam                 1      6  0
003800120314     d  paraa                  1      4  0
003900120314     d  parmm                  5      6  0
004000050607     d
004100050607     d*
004200120323     dTitasds        e ds                  extname(TITAS00F) prefix(W_)
004300120209      * - Controllo/Inversione date
004400120209     D WLBDAT          ds                  inz
004500120209     D  G02DAT                 1      8  0
004600120209     D  G02INV                 9     16  0
004700120209     D  G02ERR                17     17
004800120209     D  G02TGI                18     22  0
004900040331      *
005000050526     D***
005100040331      *
005200040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005300050526      *
005400040331      * - Indici di schiera
005500040331     d xx              s              3  0 inz
005600050607     D k               s              5  0                                      indice
005700040331      * - Campi di comodo
006000150713     d Flg_pes         s              1                                         flag di scarto x PES
006100150713     d Colli_anal      s              5  2                                      % colli analizzati
006200150713     d Scost_peso      s              7  2                                      % scostamento peso
006300050603     d kAr5Trd         s                   Like(Ar5Trd)
006400120323     d kmgs            s                   Like(W_TasMgs)
006500160129     D Flgta7          s              1  0
006600050609     d codut           s              1  0 Inz(1)
006700060915     D kcod            s                   like(tblcod)
006800060914     D ksvt            s                   like(taisvt)
006900081105     D Wcev            s                   like(evbcev) inz('RIC')
007000060914     D wmm             s              2  0
007100070207     D totspe          s              9  0
007200070207     D tottas          s              9  0
007300150713     D tipo_tar        s              1  0
007400120314     D perce           s              3  0
007500120321     D WDataFatDA      s              8  0
007600120321     D WDataFatA       s              8  0
007700120321     D WDataFatInMes   s              8  0
007800120326     D WDataFatFiMes   s              8  0
007900120321     d dataiso         s               d   datfmt(*iso)
007901160129     D Dtastat         s               d   datfmt(*iso)
007903160129     D dataeur         s               d   datfmt(*eur)
008000120315     d savpkf          s                   like(taspkf)
008100120315     d savpkc          s                   like(taspkc)
008200120315     d savvlf          s                   like(tasvlf)
008300150713     d sav_KSC         s                   like(tasksc)
008400150713     d sav_CTR         s                   like(tasctr)
008401160129
008402160129     d                 ds
008403160129     d Mese                    3      4  0
008404160129     d Anno                    5      8  0
008405160129     d dataamg                 1      8  0
008500060914      *
008600060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
008700060914      *
008800060914      * codici bolla da scartare
008900060914     D tbs             s              2    dim(50)
009000060915      * codici bolla che prevedono la tassazione di una singola varia
009100060915     D cbo             s              2    dim(50)
009200060915      *
009300040119
009400040119      *
009500120321      * costruisco le data fattura da a nel mese
009600120323     c                   eval      WdatafatDA = paraa*10000+parmm*100+01
009700120323     c                   eval      WdatafatA  = paraa*10000+parmm*100+99
009800120321      * costruisco la data inizio mese
009900120321     c                   move      WdatafatDA    Dataiso
010000120321     c                   adddur    1:*m          Dataiso
010100120323     c                   move      Dataiso       WdataFatInMes
010200120326     c                   subdur    1:*d          Dataiso
010300120326     c                   move      Dataiso       WdataFatFiMes
010600120323     C/EXEC SQL
010700120323     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010800120323     C/END-EXEC
010900120323     C*
011000150714      * se tipo bolla è franco
011100150714      * se bolla senza nessun collo rilevato da VDL
011200150714      * se bolla con peso bollettato diverso dal peso fatturato
011300120321
011400120321     C/EXEC SQL
011500120321     C+ declare C1 cursor for
011600150713     C+ select * from titas10f where
011700120321     C+ ((tasdft between :wDataFatDa AND :wDataFatA and tasfiv <> 0) OR
011800120323     C+  (tasdft = :wDataFatInMes AND tasfiv = 998 ))
011900150715     C+  and tastbl = 'F1' and
012000150714     C+  tasncp = 0  and taspkb = taspkf
012100150713     C+ order by tasksc , tasctr
012200120321     C+ for read only
012300120321     C/END-EXEC
012400120321     C
012500120321     C/EXEC SQL
012600120321     C+ open C1
012700120321     C/END-EXEC
012800120321     C
012900120321     C/EXEC SQL
013000120321     C+ Fetch C1 into :Titasds
013100120321     C/END-EXEC
013200120321     C*
013300120321     C                   dow       sqlcod = *zeros
013400120321
013500150713      * tipo tariffa
013600150713     c                   movel     W_tasctr      tipo_tar
013700120321     c                   select
013800120321      * verifico se fattura primo giorno del mese se fa parte della fatturazione del mese precedente
013900120323      * la escludo
014000120323     c                   When      W_tasdft = wDataFatDA and W_tasfiv = 998
014100150714      * se tariffa  è a peso  elaboro
014200150714     c                   When      tipo_tar =  0 or  tipo_tar =  3
014300120321      *
014400120321     C                   exsr      Sr_Elabora
014500120321     C                   endsl
014600120321     C*
014700120321     C/EXEC SQL
014800120321     C+ Fetch C1 into :Titasds
014900120321     C/END-EXEC
015000120321     C*
015100120321     C                   enddo
015200120321     C*
015300120321     C/EXEC SQL
015400120321     C+ close C1
015500120321     C/END-EXEC
015600120321     C*
015700120323      * stampo  la fine elaborazione
015800120323     c                   write     cf16s1
015900120323
016000120323     c                   eval      *inLR = *on
016100120323      *
016200120323     c                   movel     'C'           tastla
016300150713     C                   CALL      'TICFV2R'
016400120323     C                   PARM                    KPJBA
016500120323     C                   PARM                    DTAS
016600120323     C                   PARM                    DTASV
016700120323     c*
016800120323     c                   movel(p)  param         kpjbu
016900120321      *
017000120321      **-------------------------------------------------------------**
017100120321      ** Elabora ogni singola bolla                                  **
017200120321      **-------------------------------------------------------------**
017300120321     c     sr_Elabora    BEGSR
017400120321      *
017500060915
017600060914
017700060914      * scarto le bolle che sono state fatturate in filiale
017800120323     c                   If        W_tasfiv <> 998 and W_tasfiv <> 996
017900120323     c                   leavesr
018000060914     c                   endif
018100060914     c
018200060914
018300060914      * verifico se tassata fino all'imponibile in TITAI
018400060914     c                   eval      ksvt = '£'
018500060914     c     Ktai06        chain     titai01l
018600060914      * se trovato record imponibile  scarto la bolla
018700060914     c                   If        %found(titai01l)
018800120323      * carico il record come scartato
018900120323     c                   leavesr
019000060914     c                   endif
019100060914     c
019200060914      * se presenti varia "N" o "&" + 88888888 ignoro il record
019300060914     c                   eval      ksvt = 'N'
019400060914     c     Ktai06        chain     titai01l
019500060914      * se trovato record imponibile  scarto la bolla
019600060914     c                   If        %found(titai01l)  and taivat = 88888888
019700120323      * carico il record come scartato
019800120323     c                   leavesr
019900060914     c                   endif
020000060914
020100060914     c                   eval      ksvt = '&'
020200060914     c     Ktai06        chain     titai01l
020300060914      * se trovato record imponibile  scarto la bolla
020400060914     c                   If        %found(titai01l)  and taivat = 88888888
020500120323      * carico il record come scartato
020600120323     c                   leavesr
020700060914     c                   endif
020800060914
020900150713      * a rottura di cliente e tariffa aggancio la Statistica pesi VDL
021000150713     c                   if        SAV_ksc <> W_tasksc or SAV_ctr <> W_tasctr
021100150713     c                   eval      sav_ksc = W_tasksc
021200150713     c                   eval      sav_ctr = W_tasctr
021300150713     c                   clear                   flg_pes
021400150713      * aggancio SIPES
021500150713     c                   exsr      SR_ctrpesVDL
021600150713     c                   endif
021700150713      * preparo i dati da registrare su wfvtp per record non scartati
021800120323     c                   exsr      sr_prepara
021900120323
022000150713      * se il nuovo peso è uguale all'attuale taspkf non tasso
022100150713     c                   If        vtppes <> W_taspkf
022200150713
022300120323     c                   clear                   dtas
022400120323     c                   clear                   dtasv
022500120323
022600150713     c                   eval      taspkf = vtppes
022700120323     c                   eval      taspkc = W_taspkc
022800120323     c                   eval      tasvlf = W_tasvlf
022900120323     c                   eval      tasksc = W_tasksc
023000120323     c                   eval      tasncp = W_tasncp
023100120323     c                   eval      tasctr = W_tasctr
023200120323     c                   eval      tascts = W_tascts
023300120323     c                   eval      tasnzd = W_tasnzd
023400120323     c                   eval      tascad = W_tascad
023500120323     c                   eval      tasfin = W_tasfin
023600120323     c                   eval      tasfdn = W_tasfdn
023700120323     c                   eval      tasmct = W_tasmct
023800120323     c                   eval      tasnzm = W_tasnzm
023900120323     c                   eval      tascam = W_tascam
024000120323     c                   eval      tasfap = W_tasfap
024100120323     c                   eval      tastsp = W_tastsp
024200120323     c                   eval      tastc2 = W_tastc2
024300120323     c                   eval      taslnp = W_taslnp
024400120323     c                   eval      taslna = W_taslna
024500120323     c                   eval      tastbl = W_tastbl
024600120323     c                   eval      tasqft = W_tasqft
024700120323     c                   eval      tasdiv = W_tasdiv
024800120323
024900120314      * eseguo la tassazione con i vari pesi
025000120314     c                   exsr      sr_tassa
025100150714      *
025200150714     c                   else
025300150714      * anche se i pesi sono uguali valorizzo lo stesso l'imponibile ma indico nel flag
025400150714      * di tassazione che non sono passata dalla tassazione
025500150714     c                   eval      vtpimvr = W_tasimv
025600150714     c                   eval      vtptassa = 'N'
025700150713      *
025800150713     c                   endif
025900150713     c                   write     wfvtp000
026000120314      *
026100120323     c                   endsr
026200120314      **-------------------------------------------------------------**
026300120314      ** Richiamo pgm tassaz. modificando il peso                    **
026400120314      **-------------------------------------------------------------**
026500120314     c     sr_tassa      BEGSR
026600120314      *
026700120314     c                   movel     *Blanks       tastla
026800120314     c                   clear                   dAr5Ban
026900120314     c                   Clear                   dAr5Bnb
027000120314     c                   Clear                   kpjbu
027100120323     c                   movel(p)  W_tasaas      tasdsp
027200120323     c                   move      W_tasmgs      tasdsp
027300120326     c                   z-add     WDataFatFiMes tasdct
027400120319
027500120323     c                   movel     W_Tasftc      tastc1
027600120314      * Flag SI NO DDT
027700120323    1C                   If        W_tasll1 = 'C' or W_tasll1 = 'S'
027800120314     C                   movel     'S'           Tasddt
027900120314   x1C                   Else
028000120314     C                   Clear                   Tasddt
028100120314    1C                   Endif
028200120314     c                   clear                   dtas01
028300120323     c                   movel     W_tasfbr      §asfbr
028400120323     c                   movel     W_tascca      §ascca
028500120314      * valorizzo flag email al destinatario
028600120323     c                   movel     W_tasflo      dtasflo
028700120314     c                   move      §floemd       §asemd
028800120314
028900120314     c                   eval      §tasflo = dtas01
029000120314
029100120323     c                   eval      tasncl = W_tasncl
029200120314     c                   clear                   tasban
029300120314      * Bancali
029400120323    1c                   If        %Subst(W_TasGva:1:1) = 'B'
029500120314
029600120314     c                   eval      kAr5Trd  = 'BAN'
029700120314     c     kFiar5        Chain     Fiar531c
029800120314If  1c                   If        %Found(Fiar531c)
029900120314     c                   Movel     Ar5Uni        dAr5Ban
030000120314    1c                   EndIf
030100120314      * Bancali
030200120314     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
030300120314    1c                   EndIf
030400120314      **
030500120314      * Colli Originali
030600120323    1c                   If        %Subst(W_TasGva:1:1) = 'O'
030700120314     c                   eval      kAr5Trd  = 'BNB'
030800120314     c     kFiar5        Chain     Fiar531c
030900120314If  1c                   If        %Found(Fiar531c)
031000120314     c                   Movel     Ar5Uni        dAr5Bnb
031100120314    1c                   EndIf
031200120314      * Colli
031300120314     c                   Eval      TasNcl = §Ar5bcor
031400120314    1c                   EndIf
031500120314
031600120314     c                   clear                   tascas
031700120314     c                   clear                   tastic
031800120314     c                   clear                   tasvca
031900120314     c                   clear                   tascmb
032000120314      * C/Assegno
032100120323    2c                   IF        W_Tasfca <> *blanks
032200120314     c                   Exsr      Carcsb
032300120314    2c                   Endif
032400120314     c     Kevb4         chain     Fnevb04l
032500120314
032600120314     c                   If        %found(Fnevb04l)
032700120314     c                   eval      tasric = 'S'
032800120314     c                   endif
032900120323     c
033000120314      * richiamo il programma di tassazione con peso uguale
033100120314     c                   exsr      sr_sf20
033200120314
033300120314     c                   add       1             totspe
033400120314     c                   select
033500120314     c                   when      taserr = *blanks
033600120323      * se non ci sono errori nella tassazione scrivo il record con tipo record " "
033700150713     c                   if        tasbap <> 'M'
033800150713     c                   eval      vtptpr = ' '
033900150713     c                   else
034000150713     c                   eval      vtptpr = 'N'
034100150713     c                   endif
034200150713     c                   eval      vtpimvr = tasimv
034300150714      * se imponibili uguali metto che vengono dalla tassazione
034400150714     c                   if        W_tasimv = vtpimvr
034500150714     c                   eval      vtptassa = 'T'
034600150714     c                   endif
034700120314
034800120323     c                   other
034900120323      * errore di manca tariffa
035000150713     c                   eval      vtptpr = 'M'
035100120314     c                   endsl
035200120323
035300120314     c                   Endsr
035400040331      *
035500050526      **-------------------------------------------------------------**
035600150713      ** Richiamo pgm tassaz. ticfv2r  copia del tnsf20r             **
035700050526      **-------------------------------------------------------------**
035800050526     c     sr_sf20       BEGSR
035900050526      *
036000120314     c                   clear                   dtasv
036100120314     c                   Clear                   taspor
036200120314     c                   Clear                   tasimv
036300120314     c                   eval      tassva = *blanks
036400120314      * Assicurazione
036500120323    2c                   IF        W_Tasfcm = 'F'
036600120314     c                   clear                   tasias
036700120314     c                   clear                   tasvas
036800120323     c                   else
036900120323     c                   eval      tasias = W_tasias
037000120323     c                   eval      tasvas = W_Tasvas
037100120314    2c                   Endif
037200120323
037300120323      * recupero i valori  già tassati
037400120323     c                   exsr      Ric_tai
037500060914
037600150714     c                   call      'TICFV2R'
037700060914     c                   PARM                    KPJBA
037800060914     c                   PARM                    DTAS
037900060914     c                   PARM                    Dtasv
038000060914
038100050607     c     e_sf20        endsr
038200120314      *
038300060914      *****************************************************************
038400060914      *  Recupero importi di tassazione dal file titai
038500060914      *****************************************************************
038600060914     c     Ric_tai       Begsr
038700060914      *
038800060914     c                   z-add     0             k
038900060914
039000060914     c     ktai05        setll     titai01l
039100060914
039200060914     c                   If        %equal(titai01l)
039300060914
039400060914     c                   do        *hival
039500060914
039600060914     c     ktai05        reade     titai01l
039700060914
039800060914     c                   If        %eof(titai01l)
039900060914     c                   leave
040000060914     c                   endif
040100060914      * imponibile
040200060914     c                   if        taisvt = '£'
040300060914     c                   eval      tasimv = taivat
040400060914     c                   endif
040500060914      * porto
040600060914     c                   if        taisvt = ' '
040700060914     c                   eval      taspor = taivat
040800060914     c                   endif
040900060914      * varie
041000060914     c                   if        taisvt <> ' ' and taisvt <> '£'
041100060914     c                   add       1             k
041200060914     c                   eval      sv(k) = taisvt
041300060914     c                   eval      va(k) = taivat
041400060914     c                   endif
041500060914
041600060914     c                   enddo
041700060914
041800060914     c                   else
041900060914      * se non è pretassata pulisco la divisa
042000060914     c                   clear                   tasdiv
042100060914     c                   endif
042200060914
042300060914     c                   endsr
042400050607      ****************************************************
042500050609      ** Ricerca Contrassegno
042600050607      ****************************************************
042700050607     c     Carcsb        Begsr
042800050607
042900050607     c     Kcsb          chain     Tncsb03l
043000050607     c                   If        %found(tncsb03l) and
043100050607      * solo per stesso tipo bolla
043200120323     c                             W_tastbl = csbtbl
043300050607     c                   movel     csbsta        Tassta
043400050607     c                   z-add     csbcas        Tascas
043500050607     c                   if        csbfus <> *blanks
043600050607     c                   move      csbfus        tastic
043700050607     c                   else
043800050607     c                   move      csbtpi        tastic
043900050607     c                   end
044000050607      * mittente
044100050607     c                   movel     csbvca        tasvca
044200050607     c                   z-add     csbcmb        tascmb
044300050607
044400050607     c                   endif
044500050607
044600050607     c                   endsr
044700150713      **-------------------------------------------------------------**
044800150713      ** Controllo statistica pesi VDL                               **
044900150713      **-------------------------------------------------------------**
045000150713     c     sr_ctrpesVDL  BEGSR
045100150713      *
045200150713     c                   clear                   flg_pes
045300150713      *
045301160129      * imposto anno e mese ricerca statistica se non trovato si
045302160129      * cerca indietro fino ad un massimo di mesi stabilito nella tabellla delle variabili calcolo
045303160129      * peso desunto
045304160129     c                   movel(p)  W_tasaas      tasdsp
045305160129     c                   move      W_tasmgs      tasdsp
045306160129     c                   move      tasdsp        dtastat
045307160129     c                   do        5
045308160129     c                   move      Dtastat       dataeur
045309160129     c                   move      dataeur       dataamg
045310160129      * aggancio la statistica
045400150713     c     Kpes          chain     Sipes01l
045401160129     c                   if        %found(Sipes01l)
045402160129     c                   eval      flg_pes = ' '                                Si file pes
045403160129     c                   leave
045404160129     c                   endif
045405160129     c                   subdur    1:*M          Dtastat
045406160129     c                   enddo
045500150713      * se il file non esiste registro le bolle senza corrispondenza cliente/Tariffa in statistica
045600150713     c                   If        not %found(Sipes01l)
045700150713     c                   eval      flg_pes = 'N'                                No file pes
045800150714     c                   clear                   Colli_anal
045900150714     c                   clear                   Scost_peso
046000150714     c                   clear                   pesnsp
046100150713     c                   leavesr
046200150713     c                   endif
046300150713      * verifico numero spedizioni rilevate il  mese precedente
046400150713     c                   if        Pesnsp < 5
046500150713     c                   eval      flg_pes = 'S'                                poche spedizioni
046600150713     c                   endif
046700150713      * verifico se analizzate più del 60% dei colli bollettati  mese precedente
046800150713     c                   eval      Colli_Anal = (pesncv*100)/pestcl
046900150713     c                   If        Colli_anal < 60                              pochi colli analizz.
047000150713     c                   eval      flg_pes = 'C'                                pochi colli
047100150713     c                   endif
047200150713      * verifico se scostamento è negativo è compreso tra -3 e -100
047300150713     c                   eval      Scost_Peso = ((pespsb-pespsc)*100)/pespsb
047400150714     c                   If        Scost_Peso > -3                              Scostamento pesi
047500150714     c                   eval      flg_pes = 'P'                                scostamento basso
047600150713     c                   endif
047700150713      *
047800150713     c                   If        Scost_Peso < -100                            pochi colli analizz.
047900150713     c                   eval      flg_pes = 'A'                                scostamento alto
048000150713     c                   endif
048100150713      *
048200150713     c                   endsr
048300120323      *
048400120323      *************************************************************************
048500120323      * scrittura w-file dati tassazione corrente delle considerate
048600120323      *************************************************************************
048700120323     C     sr_prepara    BEGSR
048800150713     c                   clear                   wfvtp000
048900150713     c                   z-add     w_tasaas      VTPAAS
049000150713     c                   z-add     w_taslnp      VTPLNP
049100150713     c                   z-add     w_tasnrs      VTPNRS
049200150713     c                   z-add     w_tasnsp      VTPNSP
049300150713     c                   z-add     w_tasksc      VTPKSC
049400150713     c                   z-add     w_tasctr      VTPCTR
049500150713     c                   z-add     w_tasimv      VTPIMV
049600150713     c                   eval      vtppkf = w_taspkf
049700150713     c                   eval      vtpsipes = flg_pes
049800150713     c                   eval      vtpcolli = Colli_anal
049900150713     c                   eval      vtpscost = Scost_peso
050000150713     c                   eval      vtpnspr = pesnsp
050100150714      * se la % di scostamento è > 0 non faccio la maggiorazione
050200150714     c                   if        scost_peso <  0
050300150713     c                   eval      vtppes =
050400150713     c                              (w_taspkf+((W_taspkf*Scost_peso*-1)/100))
050500150713     c                   else
050600150713     c                   eval      vtppes = W_taspkf
050700150713     c                   endif
050800120323
050900120323     c                   endsr
051000120323      *
051100120323      *
051200050609      *************************************************************************
051300050609      * Operazioni iniziali
051400050609      *************************************************************************
051500050609     C     *inzsr        BEGSR
051600050609     c*
051700060914     c     ktas          klist
051800060914     c                   kfld                    paraa
051900060914     c                   kfld                    Kmgs
052000060914
052100060914     c     ktai05        klist
052200120323     c                   kfld                    w_tasaas
052300120323     c                   kfld                    w_taslnp
052400120323     c                   kfld                    w_tasnrs
052500120323     c                   kfld                    w_tasnsp
052600120323     c                   kfld                    w_tastbl
052700060914
052800060914     c     ktai06        klist
052900120323     c                   kfld                    w_tasaas
053000120323     c                   kfld                    w_taslnp
053100120323     c                   kfld                    w_tasnrs
053200120323     c                   kfld                    w_tasnsp
053300120323     c                   kfld                    w_tastbl
053400060914     c                   kfld                    Ksvt
053500060914
053600050609     c     kta7          klist
053700120323     c                   kfld                    w_tasaas
053800120323     c                   kfld                    w_taslnp
053900120323     c                   kfld                    w_tasnrs
054000120323     c                   kfld                    w_tasnsp
054100120323     c                   kfld                    w_tastbl
054200050609
054300050609     c     kFiar5        Klist
054400120323     c                   kfld                    w_tasaas
054500120323     c                   kfld                    w_taslnp
054600120323     c                   kfld                    w_tasnrs
054700120323     c                   kfld                    w_tasnsp
054800050609     c                   Kfld                    kAr5Trd
054900050609
055000050609     c     KCSB          KLIST
055100120323     c                   kfld                    w_tasaas
055200120323     c                   kfld                    w_taslnp
055300120323     c                   kfld                    w_tasnrs
055400120323     c                   kfld                    w_tasnsp
055500050609
055600050609     C     KTAB          KLIST
055700050609     C                   KFLD                    CODUT
055800050609     C                   KFLD                    KCOD
055900081105
056000081105      * Chiave file eventi FNEVB
056100081105     c     Kevb4         Klist
056200120323     c                   kfld                    w_tasaas
056300120323     c                   kfld                    w_taslnp
056400120323     c                   kfld                    w_tasnrs
056500120323     c                   kfld                    w_tasnsp
056600081105     c                   Kfld                    Wcev
056700150713      * Chiave file statistica pesi VDL
056800150713     c     Kpes          Klist
056900150713     c                   kfld                    Sav_ksc
057000150713     c                   kfld                    Anno
057100150713     c                   kfld                    Mese
057200150713     c                   kfld                    Sav_ctr
057300050609     c
057400050609     c     *entry        plist
057500050609     c                   parm                    kpjba
057600050609     c                   movel     kpjbu         param
057700060915
057800090430     c                   time                    wtime            14 0
057900090430     c                   move      wtime         wdata
058000090430     c                   movel     wtime         wora
058100060915      * caricamento schiera dei tipi bolla da scartare
058200060915     c                   eval      Kcod  = 'TB'
058300050609     c                   clear                   tbs
058400050609     c                   z-add     0             xx
058500050609     c     ktab          setll     tabel00f
058600050609     c                   do        *hival
058700050609     c     ktab          reade     tabel00f
058800050609     c                   if        %eof(tabel00f)
058900050609     c                   leave
059000050609     c                   endif
059100050609     c                   movel     tbluni        dstb
059200080715     c****               if        §tbrbl = 'R' or §tbfcb <> '1'
059300080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
059400080715      * anche le bolle di recupero  §tbrbl = 'R'
059500080715     c                   if        §tbfcb <> '1'
059600050609     c                   add       1             xx
059700050609     c                   movel     tblkey        tbs(xx)
059800050609     c                   endif
059900050609     c                   enddo
060000060915      * caricamento schiera dei codici bolla da scartare
060100060915     c                   eval      Kcod  = '3A'
060200060915     c                   z-add     0             xx
060300060915     c     ktab          setll     tabel00f
060400060915     c                   do        *hival
060500060915     c     ktab          reade     tabel00f
060600060915     c                   if        %eof(tabel00f)
060700060915     c                   leave
060800060915     c                   endif
060900060915     c                   movel     tbluni        ds3a
061000060915     c                   if        §3asva <> *blanks
061100060915     c                   add       1             xx
061200060915     c                   movel     tblkey        cbo(xx)
061300060915    3c                   endif
061400060915     c                   enddo
061500050609     c                   endsr
