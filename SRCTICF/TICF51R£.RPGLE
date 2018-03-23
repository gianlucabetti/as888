000100040119      *===============================================================*
000200120511      * Ticf51r    * Confronto fatturazione Assegnati
000300110701      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000120511     fTitas38c  if   e           k disk    prefix(w_)
001100060914     fTitaI01L  if   e           k disk
001200060914     Ffiar531c  if   e           k disk
001300060914     Ftncsb03l  if   e           k disk
001400120511     fwfcfa01l  uf a e           k disk
001500060914     Ftabel00f  if   e           k disk
001600081105     Ffnevb04l  if   e           k disk
001700120511     fticf51p   o    e             printer oflind(*in90)
001800120511
001900050603     d dAr5Ban       e ds
002000050603     d dAr5Bnb       e ds
002100160408     d dAr5Fat       e ds
002200160509     d dAr5Gen       e ds
002300050526     d kpjba         e ds
002400050609     d dstb          e ds
002500060915     d ds3a          e ds
002600160509     d ds7r          e ds
002700120511
002800110701      ** DS del tasflo del file TITAS00F
002900110701     D DTASFLO       E DS
003000050527     d dtas          e ds                  inz
003100050527     d  §tasflo      e                     extfld(tasflo)
003200120511
003300060926      ** DS Flag operativi DS DTAS
003400060926     d Dtas01        e ds
003500120511
003600050607      ** DS Calcolo tassazione  - Varie
003700050607     d Dtasv         e ds
003800050607     d  sv                     1     20
003900050607     d                                     Dim(20)                              Sigle varie
004000050607     d  va                    21    140P 3
004100050607     d                                     Dim(20)                              Importi varie
004200050607     d param           ds
004300101108     D  paraam                 1      6  0
004400101108     d  paraa                  1      4  0
004500101108     d  parmm                  5      6  0
004600120511
004700120511     dwfcfaxxds      e ds                  extname(wfcfa00f)
004800120514     dsvatt                   33     52    dim(20)
004900120514     dvaatt                   53    172p 3 dim(20)
005000120514     dsvrev                  186    205    dim(20)
005100120514     dvarev                  206    325p 3 dim(20)
005200120511
005300120511      *===============================================================*
005400040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005500120511      *===============================================================*
005600120511
005700040331      * - Indici di schiera
005800040331     d xx              s              3  0 inz
005900050607     D k               s              5  0                                      indice
006000120511
006100040331      * - Campi di comodo
006200050603     d kAr5Trd         s                   Like(Ar5Trd)
006300120511     d kmgs            s                   Like(w_TasMgs)
006400050609     d codut           s              1  0 Inz(1)
006500060915     D kcod            s                   like(tblcod)
006600060914     D ksvt            s                   like(taisvt)
006700081105     D Wcev            s                   like(evbcev) inz('RIC')
006800060914     D wmm             s              2  0
006900070207     D totspe          s              9  0
007000070207     D tottas          s              9  0
007100120511
007200120511      *===============================================================*
007300060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007400120511      *===============================================================*
007500120511
007600060914      * codici bolla da scartare
007700060914     D tbs             s              2    dim(50)
007800060915      * codici bolla che prevedono la tassazione di una singola varia
007900060915     D cbo             s              2    dim(50)
008000160509       //?Sk part.consegna con abilitazione PIN Code
008100160509     d skGMA           s              2a   dim(10)
008200160509
008300120511
008400120511      *===============================================================*
008500120511
008600060914      * mi posiziono con anno mese da elaborare
008700060914     c                   movel     parmm         kmgs
008800060914     c     ktas          setll     titas38c
008900050607     c                   do        *hival
009000060914     c                   clear                   dtas
009100060914     c                   clear                   dtasv
009200060914     c     paraa         reade     titas38c
009300060914     c                   if        %eof(titas38c)
009400050607     c                   leave
009500050607     c                   endif
009600120511
009700060914      * vado a fine se bolla con mese <> da quello passato
009800120511     c                   movel     w_tasmgs      wmm               2 0
009900060914     c                   if        parmm <> wmm
010000060914     c                   leave
010100050614     c                   endif
010200120511
010300060914      * scarto bolle che non sono state contabilizzate
010400120511     c                   If        w_tasfic = ' '
010500060914     c                   iter
010600060914     c                   endif
010700060915
010800060915      * scarto  se si tratta di codice bolla che prevede solo una varia
010900060918     c                   z-add     1             kk                2 0
011000120511     c     w_tascbo      lookup    cbo(kk)                                05
011100060915     c                   if        *in05
011200060915     c                   iter
011300060915     c                   endif
011400060914
011500060914      * scarto le bolle che sono state fatturate in filiale
011600170707     c                   If        w_tasfiv < 900
011700060914     c                   iter
011800060914     c                   endif
011900060914
012000060914      * verifico se tassata fino all'imponibile in TITAI
012100060914     c                   eval      ksvt = '£'
012200060914     c     Ktai06        chain     titai01l
012300060914      * se trovato record imponibile  scarto la bolla
012400060914     c                   If        %found(titai01l)
012500060914     c                   iter
012600060914     c                   endif
012700060914     c
012800060914      * se presenti varia "N" o "&" + 88888888 ignoro il record
012900060914     c                   eval      ksvt = 'N'
013000060914     c     Ktai06        chain     titai01l
013100060914      * se trovato record imponibile  scarto la bolla
013200060914     c                   If        %found(titai01l)  and taivat = 88888888
013300060914     c                   iter
013400060914     c                   endif
013500060914
013600060914     c                   eval      ksvt = '&'
013700060914     c     Ktai06        chain     titai01l
013800060914      * se trovato record imponibile  scarto la bolla
013900060914     c                   If        %found(titai01l)  and taivat = 88888888
014000060914     c                   iter
014100060914     c                   endif
014200060914
014300111025      * scarto bolle di c/servizi, storni e i FRANCHI
014400120511     c     w_tastbl      lookup    tbs                                    30
014500111025     c                   if        *in30 = *off
014600120511
014700120511      * preparo i dati comuni per il WF
014800120511     c                   clear                   wfcfa000
014900120511     c                   z-add     w_tasaas      CFAAAS
015000120511     c                   z-add     w_taslnp      CFALNP
015100120511     c                   z-add     w_tasnrs      CFANRS
015200120511     c                   z-add     w_tasnsp      CFANSP
015300120511     c                   z-add     w_tasksc      CFAKSC
015400120511     c                   z-add     w_tasctr      CFACTR
015500120511     c                   z-add     w_taslna      CFALNA
015600120511
015700120511      * imposto i dati della bolla da passare ai pgm di tassazione
015800120511     c                   eval      taspkf = W_taspkf
015900120511     c                   eval      taspkc = W_taspkc
016000120511     c                   eval      tasvlf = W_tasvlf
016100120514     c                   eval      tasncl = W_tasncl
016200120511     c                   eval      tasksc = W_tasksc
016300120511     c                   eval      tasncp = W_tasncp
016400120511     c                   eval      tasctr = W_tasctr
016500120511     c                   eval      tascts = W_tascts
016600120511     c                   eval      tasnzd = W_tasnzd
016700120511     c                   eval      tascad = W_tascad
016800120511     c                   eval      tasfin = W_tasfin
016900120511     c                   eval      tasfdn = W_tasfdn
017000120511     c                   eval      tasmct = W_tasmct
017100120511     c                   eval      tasnzm = W_tasnzm
017200120511     c                   eval      tascam = W_tascam
017300120511     c                   eval      tasfap = W_tasfap
017400120511     c                   eval      tastsp = W_tastsp
017500120511     c                   eval      tastc2 = W_tastc2
017600120511     c                   eval      taslnp = W_taslnp
017700120511     c                   eval      taslna = W_taslna
017800120511     c                   eval      tastbl = W_tastbl
017900120511     c                   eval      tasqft = W_tasqft
018000120511     c                   eval      tasdiv = W_tasdiv
018100060914
018200160408      * se ho record FAT nel file Fiar5 imposto il peso e volume usato in tassazione
018300160408      * nel campo taspkf in modo da tassare con lo stesso peso
018400160408     c                   clear                   dAr5Fat
018500160408     c                   eval      kAr5Trd  = 'FAT'
018600160408     c     kFiar5        Chain     Fiar531c
018700160408If  3c                   If        %Found(Fiar531c)
018800160408     c                   Movel     Ar5Uni        dAr5Fat
018900160408    3c                   EndIf
019000160408     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
019100160408     c                   if        §AR5PKTAS>0
019200160408     c                   clear                   TASPKC
019300160408     c                   clear                   tasncp
019400160408     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
019500160408     c                   eval      TASPKF=§ar5pktas
019600160408     c                   eval      TASVLF=§AR5VLTAS
019700160408     c                   endif
019800160408
019900120511      * eseguo la tassazione attuale
020000120511     c                   exsr      sr_SF20
020100120511     c                   IF        taserr <> *blanks
020200120511     c                   ITER
020300120511     c                   ENDIF
020400060914
020500120511      * eseguo la tassazione con reversibilità modificata
020600120511     c                   exsr      sr_CF53
020700120511
020800120511      * scrivo WF
020900120511     c                   write     wfcfa000
021000050603     c                   endif
021100120511
021200050607     c                   enddo
021300120511
021400070207      * stampo  la fine elaborazione
021500120511     c                   write     cf51s1
021600070207
021700040119     c                   eval      *inLR = *on
021800120511
021900050603     c                   movel     'C'           tastla
022000120511     C                   CALL      'TNSF20R'
022100050603     C                   PARM                    KPJBA
022200050603     C                   PARM                    DTAS
022300050603     C                   PARM                    DTASV
022400120511
022500120511     c                   movel     'C'           tastla
022600120511     C                   CALL      'TICF53R'
022700120511     C                   PARM                    KPJBA
022800120511     C                   PARM                    DTAS
022900120511     C                   PARM                    DTASV
023000120511
023100050614     c                   movel(p)  param         kpjbu
023200120511
023300120511      *************************************************************************
023400120511      * Tassazione attuale da memorizzare su WF
023500120511      *************************************************************************
023600120511     C     sr_SF20       BEGSR
023700120511
023800120511     c                   movel     *Blanks       tastla
023900120511     c                   clear                   dAr5Ban
024000120511     c                   Clear                   dAr5Bnb
024100120511     c                   Clear                   kpjbu
024200120511     c                   clear                   dtasv
024300120511     c                   Clear                   taspor
024400120511     c                   Clear                   tasimv
024500120511     c                   eval      tassva = *blanks
024600120511     c                   movel(p)  w_tasaas      tasdsp
024700120511     c                   move      w_tasmgs      tasdsp
024800120511     c                   eval      tasdct = tasdsp
024900120511
025000120511     c                   movel     w_Tasftc      tastc1
025100120511
025200120511      * Assicurazione
025300120511    2c                   IF        W_Tasfcm = 'F'
025400120511     c                   clear                   tasias
025500120511     c                   clear                   tasvas
025600120511     c                   else
025700120511     c                   eval      tasias = W_tasias
025800120511     c                   eval      tasvas = W_Tasvas
025900120511    2c                   Endif
026000120511
026100120511      * recupero i valori  già tassati
026200120511     c                   exsr      Ric_tai
026300120511
026400120511      * Flag SI NO DDT
026500120511    1C                   If        w_tasll1 = 'C' or w_tasll1 = 'S'
026600120511     C                   movel     'S'           Tasddt
026700120511   x1C                   Else
026800120511     C                   Clear                   Tasddt
026900120511    1C                   Endif
027000120511     c                   clear                   dtas01
027100120511     c                   movel     w_tasfbr      §asfbr
027200120511     c                   movel     w_tascca      §ascca
027300120511
027400120511      * valorizzo flag email al destinatario
027500120511     c                   movel     w_tasflo      dtasflo
027600120511     c                   move      §floemd       §asemd
027700120511
027800160509
027900160509       //?Imposto se Prenotazione Ritiro Telefonico
028000160509         TASprt = §FLOado;
028100160509
028200160509       //?Imposto se part.consegna con PinCode
028300160509         IF  W_TASgma <> *blanks and %lookup (W_TASgma:skGMA) > 0;
028400160509           §ASpin = 'S';
028500160509         ENDIF;
028600160509
028700160509       //?Imposto se Stampa Packing List
028800160509         clear dAR5gen;
028900160509         chain (W_TASaas:W_TASlnp:W_TASnrs:W_TASnsp:'GEN') FIAR531C;
029000160509         IF  %found(FIAR531C);
029100160509           dAR5gen = AR5uni;
029200160509         ENDIF;
029300160509         IF  §AR5als = 'S' or §AR5alx = 'S';
029400160509           TASspl = 'S';
029500160509         ENDIF;
029600160509      /end-free
029700120511     c                   eval      §tasflo = dtas01
029800120511
029900120511      * Bancali
030000120511     c                   clear                   tasban
030100120511    1c                   If        %Subst(w_TasGva:1:1) = 'B'
030200120511     c                   eval      kAr5Trd  = 'BAN'
030300120511     c     kFiar5        Chain     Fiar531c
030400120511If  1c                   If        %Found(Fiar531c)
030500120511     c                   Movel     Ar5Uni        dAr5Ban
030600120511    1c                   EndIf
030700120511     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
030800120511    1c                   EndIf
030900120511
031000120511      * Colli Originali
031100120511    1c                   If        %Subst(w_TasGva:1:1) = 'O'
031200120511     c                   eval      kAr5Trd  = 'BNB'
031300120511     c     kFiar5        Chain     Fiar531c
031400120511If  1c                   If        %Found(Fiar531c)
031500120511     c                   Movel     Ar5Uni        dAr5Bnb
031600120511    1c                   EndIf
031700120511     c                   Eval      TasNcl = §Ar5bcor
031800120511    1c                   EndIf
031900120511
032000120511      * C/Assegno
032100120511     c                   clear                   tascas
032200120511     c                   clear                   tastic
032300120511     c                   clear                   tasvca
032400120511     c                   clear                   tascmb
032500120511    2c                   IF        w_Tasfca <> *blanks
032600120511     c                   Exsr      Carcsb
032700120511    2c                   Endif
032800120511     c     Kevb4         chain     Fnevb04l
032900120511     c                   If        %found(Fnevb04l)
033000120511     c                   eval      tasric = 'S'
033100120511     c                   endif
033200120511
033300120511      * TASSA
033400120511     c                   call      'TNSF20R'
033500120511     c                   PARM                    KPJBA
033600120511     c                   PARM                    DTAS
033700120511     c                   PARM                    Dtasv
033800120511
033900120511      * Se ok tassazione
034000120511     c                   If        taserr = *blanks
034100120511      * conta
034200120511     c                   add       1             totspe
034300120511      * imposta dati per WF
034400120511     c                   z-add     tasimv        CFAIMVA
034500120511     c                   z-add     taspor        CFAPORA
034600120511     c                   movea     va            vaatt
034700120511     c                   movea     sv            svatt
034800120511     c                   endif
034900120511
035000120511     c                   endsr
035100120511
035200120511      **-------------------------------------------------------------**
035300120511      * Tassazione attuale con reversibilità mod.da memorizzare su WF
035400120511      **-------------------------------------------------------------**
035500120511     c     sr_CF53       BEGSR
035600120511
035700120511     c                   movel     *Blanks       tastla
035800120511     c                   clear                   dAr5Ban
035900120511     c                   Clear                   dAr5Bnb
036000120511     c                   Clear                   kpjbu
036100120511     c                   clear                   dtasv
036200120511     c                   Clear                   taspor
036300120511     c                   Clear                   tasimv
036400120511     c                   eval      tassva = *blanks
036500120511     c                   movel(p)  w_tasaas      tasdsp
036600120511     c                   move      w_tasmgs      tasdsp
036700120511     c                   eval      tasdct = tasdsp
036800120511
036900120511     c                   movel     w_Tasftc      tastc1
037000120511
037100120511      * Assicurazione
037200120511    2c                   IF        W_Tasfcm = 'F'
037300120511     c                   clear                   tasias
037400120511     c                   clear                   tasvas
037500120511     c                   else
037600120511     c                   eval      tasias = W_tasias
037700120511     c                   eval      tasvas = W_Tasvas
037800120511    2c                   Endif
037900120511
038000120511      * recupero i valori  già tassati
038100120511     c                   exsr      Ric_tai
038200120511
038300120511      * Flag SI NO DDT
038400120511    1C                   If        w_tasll1 = 'C' or w_tasll1 = 'S'
038500120511     C                   movel     'S'           Tasddt
038600120511   x1C                   Else
038700120511     C                   Clear                   Tasddt
038800120511    1C                   Endif
038900120511     c                   clear                   dtas01
039000120511     c                   movel     w_tasfbr      §asfbr
039100120511     c                   movel     w_tascca      §ascca
039200120511
039300120511      * valorizzo flag email al destinatario
039400120511     c                   movel     w_tasflo      dtasflo
039500120511     c                   move      §floemd       §asemd
039600120511
039700160509       //?Imposto se Prenotazione Ritiro Telefonico
039800160509         TASprt = §FLOado;
039900160509
040000160509       //?Imposto se part.consegna con PinCode
040100160509         IF  W_TASgma <> *blanks and %lookup (W_TASgma:skGMA) > 0;
040200160509           §ASpin = 'S';
040300160509         ENDIF;
040400160509
040500160509       //?Imposto se Stampa Packing List
040600160509         clear dAR5gen;
040700160509         chain (W_TASaas:W_TASlnp:W_TASnrs:W_TASnsp:'GEN') FIAR531C;
040800160509         IF  %found(FIAR531C);
040900160509           dAR5gen = AR5uni;
041000160509         ENDIF;
041100160509         IF  §AR5als = 'S' or §AR5alx = 'S';
041200160509           TASspl = 'S';
041300160509         ENDIF;
041400160509      /end-free
041500120511     c                   eval      §tasflo = dtas01
041600120511
041700120511      * Bancali
041800120511     c                   clear                   tasban
041900120511    1c                   If        %Subst(w_TasGva:1:1) = 'B'
042000120511     c                   eval      kAr5Trd  = 'BAN'
042100120511     c     kFiar5        Chain     Fiar531c
042200120511If  1c                   If        %Found(Fiar531c)
042300120511     c                   Movel     Ar5Uni        dAr5Ban
042400120511    1c                   EndIf
042500120511     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
042600120511    1c                   EndIf
042700120511
042800120511      * Colli Originali
042900120511    1c                   If        %Subst(w_TasGva:1:1) = 'O'
043000120511     c                   eval      kAr5Trd  = 'BNB'
043100120511     c     kFiar5        Chain     Fiar531c
043200120511If  1c                   If        %Found(Fiar531c)
043300120511     c                   Movel     Ar5Uni        dAr5Bnb
043400120511    1c                   EndIf
043500120511     c                   Eval      TasNcl = §Ar5bcor
043600120511    1c                   EndIf
043700120511
043800120511      * C/Assegno
043900120511     c                   clear                   tascas
044000120511     c                   clear                   tastic
044100120511     c                   clear                   tasvca
044200120511     c                   clear                   tascmb
044300120511    2c                   IF        w_Tasfca <> *blanks
044400120511     c                   Exsr      Carcsb
044500120511    2c                   Endif
044600120511     c     Kevb4         chain     Fnevb04l
044700120511     c                   If        %found(Fnevb04l)
044800120511     c                   eval      tasric = 'S'
044900120511     c                   endif
045000120511
045100120511      * TASSA
045200120511     c                   call      'TICF53R'
045300120511     c                   PARM                    KPJBA
045400120511     c                   PARM                    DTAS
045500120511     c                   PARM                    Dtasv
045600120511
045700120511      * Se ok tassazione
045800120511     c                   If        taserr = *blanks
045900120511      * conta
046000120511     c                   add       1             tottas
046100120511      * imposta dati per WF
046200120511     c                   z-add     tasimv        CFAIMVR
046300120511     c                   z-add     taspor        CFAPORR
046400120511     c                   movea     va            varev
046500120511     c                   movea     sv            svrev
046600120511     c                   endif
046700120511
046800120511     c                   endsr
046900120511
047000060914      *****************************************************************
047100060914      *  Recupero importi di tassazione dal file titai
047200060914      *****************************************************************
047300060914     c     Ric_tai       Begsr
047400060914      *
047500060914     c                   z-add     0             k
047600060914
047700060914     c     ktai05        setll     titai01l
047800060914
047900060914     c                   If        %equal(titai01l)
048000060914
048100060914     c                   do        *hival
048200060914
048300060914     c     ktai05        reade     titai01l
048400060914
048500060914     c                   If        %eof(titai01l)
048600060914     c                   leave
048700060914     c                   endif
048800060914      * imponibile
048900060914     c                   if        taisvt = '£'
049000060914     c                   eval      tasimv = taivat
049100060914     c                   endif
049200060914      * porto
049300060914     c                   if        taisvt = ' '
049400060914     c                   eval      taspor = taivat
049500060914     c                   endif
049600060914      * varie
049700060914     c                   if        taisvt <> ' ' and taisvt <> '£'
049800060914     c                   add       1             k
049900060914     c                   eval      sv(k) = taisvt
050000060914     c                   eval      va(k) = taivat
050100060914     c                   endif
050200060914
050300060914     c                   enddo
050400060914
050500060914     c                   else
050600060914      * se non è pretassata pulisco la divisa
050700060914     c                   clear                   tasdiv
050800060914     c                   endif
050900060914
051000060914     c                   endsr
051100050607      ****************************************************
051200050609      ** Ricerca Contrassegno
051300050607      ****************************************************
051400050607     c     Carcsb        Begsr
051500050607
051600050607     c     Kcsb          chain     Tncsb03l
051700050607     c                   If        %found(tncsb03l) and
051800050607      * solo per stesso tipo bolla
051900120511     c                             w_tastbl = csbtbl
052000050607     c                   movel     csbsta        Tassta
052100050607     c                   z-add     csbcas        Tascas
052200050607     c                   if        csbfus <> *blanks
052300050607     c                   move      csbfus        tastic
052400050607     c                   else
052500050607     c                   move      csbtpi        tastic
052600050607     c                   end
052700050607      * mittente
052800050607     c                   movel     csbvca        tasvca
052900050607     c                   z-add     csbcmb        tascmb
053000050607
053100050607     c                   endif
053200050607
053300050607     c                   endsr
053400050609      *************************************************************************
053500050609      * Operazioni iniziali
053600050609      *************************************************************************
053700050609     C     *inzsr        BEGSR
053800050609     c*
053900060914     c     ktas          klist
054000060914     c                   kfld                    paraa
054100060914     c                   kfld                    Kmgs
054200060914
054300060914     c     ktai05        klist
054400120511     c                   kfld                    w_tasaas
054500120511     c                   kfld                    w_taslnp
054600120511     c                   kfld                    w_tasnrs
054700120511     c                   kfld                    w_tasnsp
054800120511     c                   kfld                    w_tastbl
054900060914
055000060914     c     ktai06        klist
055100120511     c                   kfld                    w_tasaas
055200120511     c                   kfld                    w_taslnp
055300120511     c                   kfld                    w_tasnrs
055400120511     c                   kfld                    w_tasnsp
055500120511     c                   kfld                    w_tastbl
055600060914     c                   kfld                    Ksvt
055700050609
055800050609     c     kFiar5        Klist
055900120511     c                   Kfld                    w_TasAas
056000120511     c                   Kfld                    w_TasLnp
056100120511     c                   Kfld                    w_TasNrs
056200120511     c                   Kfld                    w_TasNsp
056300050609     c                   Kfld                    kAr5Trd
056400050609
056500050609     c     KCSB          KLIST
056600120511     c                   KFLD                    w_TASAAS
056700120511     c                   KFLD                    w_TASLNP
056800120511     c                   KFLD                    w_TASNRS
056900120511     c                   KFLD                    w_TASNSP
057000050609
057100050609     C     KTAB          KLIST
057200050609     C                   KFLD                    CODUT
057300050609     C                   KFLD                    KCOD
057400081105
057500081105      * Chiave file eventi FNEVB
057600081105     c     Kevb4         Klist
057700120511     c                   Kfld                    w_Tasaas
057800120511     c                   Kfld                    w_Taslnp
057900120511     c                   Kfld                    w_Tasnrs
058000120511     c                   Kfld                    w_Tasnsp
058100081105     c                   Kfld                    Wcev
058200081105      *
058300050609     c
058400050609     c     *entry        plist
058500050609     c                   parm                    kpjba
058600050609     c                   movel     kpjbu         param
058700060915
058800090430     c                   time                    wtime            14 0
058900090430     c                   move      wtime         wdata
059000090430     c                   movel     wtime         wora
059100060915      * caricamento schiera dei tipi bolla da scartare
059200060915     c                   eval      Kcod  = 'TB'
059300050609     c                   clear                   tbs
059400050609     c                   z-add     0             xx
059500050609     c     ktab          setll     tabel00f
059600050609     c                   do        *hival
059700050609     c     ktab          reade     tabel00f
059800050609     c                   if        %eof(tabel00f)
059900050609     c                   leave
060000050609     c                   endif
060100050609     c                   movel     tbluni        dstb
060200101108      * con questo confronto facciamo solo gli assegnati
060300101108     c                   if        §tbfcb <> '1' or  §tbtpo = 'F'
060400050609     c                   add       1             xx
060500050609     c                   movel     tblkey        tbs(xx)
060600050609     c                   endif
060700050609     c                   enddo
060800060915      * caricamento schiera dei codici bolla da scartare
060900060915     c                   eval      Kcod  = '3A'
061000060915     c                   z-add     0             xx
061100060915     c     ktab          setll     tabel00f
061200060915     c                   do        *hival
061300060915     c     ktab          reade     tabel00f
061400060915     c                   if        %eof(tabel00f)
061500060915     c                   leave
061600060915     c                   endif
061700060915     c                   movel     tbluni        ds3a
061800060915     c                   if        §3asva <> *blanks
061900060915     c                   add       1             xx
062000060915     c                   movel     tblkey        cbo(xx)
062100060915    3c                   endif
062200060915     c                   enddo
062300160509
062400160509      /free
062500160509         Kcod = '7R';
062600160509         clear kk;
062700160509         setll (Codut:Kcod) TABEL00F;
062800160509         reade (Codut:Kcod) TABEL00F;
062900160509         DOW  not %eof(TABEL00F);
063000160509           IF  TBLflg = *blanks;
063100160509             ds7R = TBLuni;
063200160509             IF  §7Rpincode = 'S';
063300160509               kk += 1;
063400160509               skGMA(kk) = TBLkey;
063500160509             ENDIF;
063600160509           ENDIF;
063700160509           reade (Codut:Kcod) TABEL00F;
063800160509         ENDDO;
063900160509
064000160509      /end-free
064100060915
064200050609     c                   endsr
