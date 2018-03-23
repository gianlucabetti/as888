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
002001160408     d dAr5Fat       e ds
002002160509     d dAr5Gen       e ds
002100050526     d kpjba         e ds
002200050609     d dstb          e ds
002300060915     d ds3a          e ds
002301160509     d ds7r          e ds
002400120511
002500110701      ** DS del tasflo del file TITAS00F
002600110701     D DTASFLO       E DS
002700050527     d dtas          e ds                  inz
002800050527     d  §tasflo      e                     extfld(tasflo)
002900120511
003000060926      ** DS Flag operativi DS DTAS
003100060926     d Dtas01        e ds
003200120511
003300050607      ** DS Calcolo tassazione  - Varie
003400050607     d Dtasv         e ds
003500050607     d  sv                     1     20
003600050607     d                                     Dim(20)                              Sigle varie
003700050607     d  va                    21    140P 3
003800050607     d                                     Dim(20)                              Importi varie
003900050607     d param           ds
004000101108     D  paraam                 1      6  0
004100101108     d  paraa                  1      4  0
004200101108     d  parmm                  5      6  0
004300120511
004400120511     dwfcfaxxds      e ds                  extname(wfcfa00f)
004500120514     dsvatt                   33     52    dim(20)
004600120514     dvaatt                   53    172p 3 dim(20)
004700120514     dsvrev                  186    205    dim(20)
004800120514     dvarev                  206    325p 3 dim(20)
004900120511
005000120511      *===============================================================*
005100040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
005200120511      *===============================================================*
005300120511
005400040331      * - Indici di schiera
005500040331     d xx              s              3  0 inz
005600050607     D k               s              5  0                                      indice
005700120511
005800040331      * - Campi di comodo
005900050603     d kAr5Trd         s                   Like(Ar5Trd)
006000120511     d kmgs            s                   Like(w_TasMgs)
006100050609     d codut           s              1  0 Inz(1)
006200060915     D kcod            s                   like(tblcod)
006300060914     D ksvt            s                   like(taisvt)
006400081105     D Wcev            s                   like(evbcev) inz('RIC')
006500060914     D wmm             s              2  0
006600070207     D totspe          s              9  0
006700070207     D tottas          s              9  0
006800120511
006900120511      *===============================================================*
007000060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007100120511      *===============================================================*
007200120511
007300060914      * codici bolla da scartare
007400060914     D tbs             s              2    dim(50)
007500060915      * codici bolla che prevedono la tassazione di una singola varia
007600060915     D cbo             s              2    dim(50)
007601160509       //?Sk part.consegna con abilitazione PIN Code
007602160509     d skGMA           s              2a   dim(10)
007603160509
007700120511
007800120511      *===============================================================*
007900120511
008000060914      * mi posiziono con anno mese da elaborare
008100060914     c                   movel     parmm         kmgs
008200060914     c     ktas          setll     titas38c
008300050607     c                   do        *hival
008400060914     c                   clear                   dtas
008500060914     c                   clear                   dtasv
008600060914     c     paraa         reade     titas38c
008700060914     c                   if        %eof(titas38c)
008800050607     c                   leave
008900050607     c                   endif
009000120511
009100060914      * vado a fine se bolla con mese <> da quello passato
009200120511     c                   movel     w_tasmgs      wmm               2 0
009300060914     c                   if        parmm <> wmm
009400060914     c                   leave
009500050614     c                   endif
009600120511
009700060914      * scarto bolle che non sono state contabilizzate
009800120511     c                   If        w_tasfic = ' '
009900060914     c                   iter
010000060914     c                   endif
010100060915
010200060915      * scarto  se si tratta di codice bolla che prevede solo una varia
010300060918     c                   z-add     1             kk                2 0
010400120511     c     w_tascbo      lookup    cbo(kk)                                05
010500060915     c                   if        *in05
010600060915     c                   iter
010700060915     c                   endif
010800060914
010900060914      * scarto le bolle che sono state fatturate in filiale
011000120511     c                   If        w_tasfiv <> 998 and w_tasfiv <> 996
011100060914     c                   iter
011200060914     c                   endif
011300060914
011400060914      * verifico se tassata fino all'imponibile in TITAI
011500060914     c                   eval      ksvt = '£'
011600060914     c     Ktai06        chain     titai01l
011700060914      * se trovato record imponibile  scarto la bolla
011800060914     c                   If        %found(titai01l)
011900060914     c                   iter
012000060914     c                   endif
012100060914     c
012200060914      * se presenti varia "N" o "&" + 88888888 ignoro il record
012300060914     c                   eval      ksvt = 'N'
012400060914     c     Ktai06        chain     titai01l
012500060914      * se trovato record imponibile  scarto la bolla
012600060914     c                   If        %found(titai01l)  and taivat = 88888888
012700060914     c                   iter
012800060914     c                   endif
012900060914
013000060914     c                   eval      ksvt = '&'
013100060914     c     Ktai06        chain     titai01l
013200060914      * se trovato record imponibile  scarto la bolla
013300060914     c                   If        %found(titai01l)  and taivat = 88888888
013400060914     c                   iter
013500060914     c                   endif
013600060914
013700111025      * scarto bolle di c/servizi, storni e i FRANCHI
013800120511     c     w_tastbl      lookup    tbs                                    30
013900111025     c                   if        *in30 = *off
014000120511
014100120511      * preparo i dati comuni per il WF
014200120511     c                   clear                   wfcfa000
014300120511     c                   z-add     w_tasaas      CFAAAS
014400120511     c                   z-add     w_taslnp      CFALNP
014500120511     c                   z-add     w_tasnrs      CFANRS
014600120511     c                   z-add     w_tasnsp      CFANSP
014700120511     c                   z-add     w_tasksc      CFAKSC
014800120511     c                   z-add     w_tasctr      CFACTR
014900120511     c                   z-add     w_taslna      CFALNA
015000120511
015100120511      * imposto i dati della bolla da passare ai pgm di tassazione
015200120511     c                   eval      taspkf = W_taspkf
015300120511     c                   eval      taspkc = W_taspkc
015400120511     c                   eval      tasvlf = W_tasvlf
015500120514     c                   eval      tasncl = W_tasncl
015600120511     c                   eval      tasksc = W_tasksc
015700120511     c                   eval      tasncp = W_tasncp
015800120511     c                   eval      tasctr = W_tasctr
015900120511     c                   eval      tascts = W_tascts
016000120511     c                   eval      tasnzd = W_tasnzd
016100120511     c                   eval      tascad = W_tascad
016200120511     c                   eval      tasfin = W_tasfin
016300120511     c                   eval      tasfdn = W_tasfdn
016400120511     c                   eval      tasmct = W_tasmct
016500120511     c                   eval      tasnzm = W_tasnzm
016600120511     c                   eval      tascam = W_tascam
016700120511     c                   eval      tasfap = W_tasfap
016800120511     c                   eval      tastsp = W_tastsp
016900120511     c                   eval      tastc2 = W_tastc2
017000120511     c                   eval      taslnp = W_taslnp
017100120511     c                   eval      taslna = W_taslna
017200120511     c                   eval      tastbl = W_tastbl
017300120511     c                   eval      tasqft = W_tasqft
017400120511     c                   eval      tasdiv = W_tasdiv
017500060914
017501160408      * se ho record FAT nel file Fiar5 imposto il peso e volume usato in tassazione
017502160408      * nel campo taspkf in modo da tassare con lo stesso peso
017503160408     c                   clear                   dAr5Fat
017504160408     c                   eval      kAr5Trd  = 'FAT'
017505160408     c     kFiar5        Chain     Fiar531c
017506160408If  3c                   If        %Found(Fiar531c)
017507160408     c                   Movel     Ar5Uni        dAr5Fat
017508160408    3c                   EndIf
017509160408     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
017510160408     c                   if        §AR5PKTAS>0
017511160408     c                   clear                   TASPKC
017512160408     c                   clear                   tasncp
017513160408     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
017514160408     c                   eval      TASPKF=§ar5pktas
017515160408     c                   eval      TASVLF=§AR5VLTAS
017516160408     c                   endif
017517160408
017600120511      * eseguo la tassazione attuale
017700120511     c                   exsr      sr_SF20
017800120511     c                   IF        taserr <> *blanks
017900120511     c                   ITER
018000120511     c                   ENDIF
018100060914
018200120511      * eseguo la tassazione con reversibilità modificata
018300120511     c                   exsr      sr_CF53
018400120511
018500120511      * scrivo WF
018600120511     c                   write     wfcfa000
018700050603     c                   endif
018800120511
018900050607     c                   enddo
019000120511
019100070207      * stampo  la fine elaborazione
019200120511     c                   write     cf51s1
019300070207
019400040119     c                   eval      *inLR = *on
019500120511
019600050603     c                   movel     'C'           tastla
019700120511     C                   CALL      'TNSF20R'
019800050603     C                   PARM                    KPJBA
019900050603     C                   PARM                    DTAS
020000050603     C                   PARM                    DTASV
020100120511
020200120511     c                   movel     'C'           tastla
020300120511     C                   CALL      'TICF53R'
020400120511     C                   PARM                    KPJBA
020500120511     C                   PARM                    DTAS
020600120511     C                   PARM                    DTASV
020700120511
020800050614     c                   movel(p)  param         kpjbu
020900120511
021000120511      *************************************************************************
021100120511      * Tassazione attuale da memorizzare su WF
021200120511      *************************************************************************
021300120511     C     sr_SF20       BEGSR
021400120511
021500120511     c                   movel     *Blanks       tastla
021600120511     c                   clear                   dAr5Ban
021700120511     c                   Clear                   dAr5Bnb
021800120511     c                   Clear                   kpjbu
021900120511     c                   clear                   dtasv
022000120511     c                   Clear                   taspor
022100120511     c                   Clear                   tasimv
022200120511     c                   eval      tassva = *blanks
022300120511     c                   movel(p)  w_tasaas      tasdsp
022400120511     c                   move      w_tasmgs      tasdsp
022500120511     c                   eval      tasdct = tasdsp
022600120511
022700120511     c                   movel     w_Tasftc      tastc1
022800120511
022900120511      * Assicurazione
023000120511    2c                   IF        W_Tasfcm = 'F'
023100120511     c                   clear                   tasias
023200120511     c                   clear                   tasvas
023300120511     c                   else
023400120511     c                   eval      tasias = W_tasias
023500120511     c                   eval      tasvas = W_Tasvas
023600120511    2c                   Endif
023700120511
023800120511      * recupero i valori  già tassati
023900120511     c                   exsr      Ric_tai
024000120511
024100120511      * Flag SI NO DDT
024200120511    1C                   If        w_tasll1 = 'C' or w_tasll1 = 'S'
024300120511     C                   movel     'S'           Tasddt
024400120511   x1C                   Else
024500120511     C                   Clear                   Tasddt
024600120511    1C                   Endif
024700120511     c                   clear                   dtas01
024800120511     c                   movel     w_tasfbr      §asfbr
024900120511     c                   movel     w_tascca      §ascca
025000120511
025100120511      * valorizzo flag email al destinatario
025200120511     c                   movel     w_tasflo      dtasflo
025300120511     c                   move      §floemd       §asemd
025400120511
025401160509
025402160509       //?Imposto se Prenotazione Ritiro Telefonico
025403160509         TASprt = §FLOado;
025404160509
025405160509       //?Imposto se part.consegna con PinCode
025406160509         IF  W_TASgma <> *blanks and %lookup (W_TASgma:skGMA) > 0;
025407160509           §ASpin = 'S';
025408160509         ENDIF;
025409160509
025410160509       //?Imposto se Stampa Packing List
025411160509         clear dAR5gen;
025412160509         chain (W_TASaas:W_TASlnp:W_TASnrs:W_TASnsp:'GEN') FIAR531C;
025413160509         IF  %found(FIAR531C);
025414160509           dAR5gen = AR5uni;
025415160509         ENDIF;
025416160509         IF  §AR5als = 'S' or §AR5alx = 'S';
025417160509           TASspl = 'S';
025418160509         ENDIF;
025419160509      /end-free
025500120511     c                   eval      §tasflo = dtas01
025600120511
025700120511      * Bancali
025800120511     c                   clear                   tasban
025900120511    1c                   If        %Subst(w_TasGva:1:1) = 'B'
026000120511     c                   eval      kAr5Trd  = 'BAN'
026100120511     c     kFiar5        Chain     Fiar531c
026200120511If  1c                   If        %Found(Fiar531c)
026300120511     c                   Movel     Ar5Uni        dAr5Ban
026400120511    1c                   EndIf
026500120511     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
026600120511    1c                   EndIf
026700120511
026800120511      * Colli Originali
026900120511    1c                   If        %Subst(w_TasGva:1:1) = 'O'
027000120511     c                   eval      kAr5Trd  = 'BNB'
027100120511     c     kFiar5        Chain     Fiar531c
027200120511If  1c                   If        %Found(Fiar531c)
027300120511     c                   Movel     Ar5Uni        dAr5Bnb
027400120511    1c                   EndIf
027500120511     c                   Eval      TasNcl = §Ar5bcor
027600120511    1c                   EndIf
027700120511
027800120511      * C/Assegno
027900120511     c                   clear                   tascas
028000120511     c                   clear                   tastic
028100120511     c                   clear                   tasvca
028200120511     c                   clear                   tascmb
028300120511    2c                   IF        w_Tasfca <> *blanks
028400120511     c                   Exsr      Carcsb
028500120511    2c                   Endif
028600120511     c     Kevb4         chain     Fnevb04l
028700120511     c                   If        %found(Fnevb04l)
028800120511     c                   eval      tasric = 'S'
028900120511     c                   endif
029000120511
029100120511      * TASSA
029200120511     c                   call      'TNSF20R'
029300120511     c                   PARM                    KPJBA
029400120511     c                   PARM                    DTAS
029500120511     c                   PARM                    Dtasv
029600120511
029700120511      * Se ok tassazione
029800120511     c                   If        taserr = *blanks
029900120511      * conta
030000120511     c                   add       1             totspe
030100120511      * imposta dati per WF
030200120511     c                   z-add     tasimv        CFAIMVA
030300120511     c                   z-add     taspor        CFAPORA
030400120511     c                   movea     va            vaatt
030500120511     c                   movea     sv            svatt
030600120511     c                   endif
030700120511
030800120511     c                   endsr
030900120511
031000120511      **-------------------------------------------------------------**
031100120511      * Tassazione attuale con reversibilità mod.da memorizzare su WF
031200120511      **-------------------------------------------------------------**
031300120511     c     sr_CF53       BEGSR
031400120511
031500120511     c                   movel     *Blanks       tastla
031600120511     c                   clear                   dAr5Ban
031700120511     c                   Clear                   dAr5Bnb
031800120511     c                   Clear                   kpjbu
031900120511     c                   clear                   dtasv
032000120511     c                   Clear                   taspor
032100120511     c                   Clear                   tasimv
032200120511     c                   eval      tassva = *blanks
032300120511     c                   movel(p)  w_tasaas      tasdsp
032400120511     c                   move      w_tasmgs      tasdsp
032500120511     c                   eval      tasdct = tasdsp
032600120511
032700120511     c                   movel     w_Tasftc      tastc1
032800120511
032900120511      * Assicurazione
033000120511    2c                   IF        W_Tasfcm = 'F'
033100120511     c                   clear                   tasias
033200120511     c                   clear                   tasvas
033300120511     c                   else
033400120511     c                   eval      tasias = W_tasias
033500120511     c                   eval      tasvas = W_Tasvas
033600120511    2c                   Endif
033700120511
033800120511      * recupero i valori  già tassati
033900120511     c                   exsr      Ric_tai
034000120511
034100120511      * Flag SI NO DDT
034200120511    1C                   If        w_tasll1 = 'C' or w_tasll1 = 'S'
034300120511     C                   movel     'S'           Tasddt
034400120511   x1C                   Else
034500120511     C                   Clear                   Tasddt
034600120511    1C                   Endif
034700120511     c                   clear                   dtas01
034800120511     c                   movel     w_tasfbr      §asfbr
034900120511     c                   movel     w_tascca      §ascca
035000120511
035100120511      * valorizzo flag email al destinatario
035200120511     c                   movel     w_tasflo      dtasflo
035300120511     c                   move      §floemd       §asemd
035400120511
035401160509       //?Imposto se Prenotazione Ritiro Telefonico
035402160509         TASprt = §FLOado;
035403160509
035404160509       //?Imposto se part.consegna con PinCode
035405160509         IF  W_TASgma <> *blanks and %lookup (W_TASgma:skGMA) > 0;
035406160509           §ASpin = 'S';
035407160509         ENDIF;
035408160509
035409160509       //?Imposto se Stampa Packing List
035410160509         clear dAR5gen;
035411160509         chain (W_TASaas:W_TASlnp:W_TASnrs:W_TASnsp:'GEN') FIAR531C;
035412160509         IF  %found(FIAR531C);
035413160509           dAR5gen = AR5uni;
035414160509         ENDIF;
035415160509         IF  §AR5als = 'S' or §AR5alx = 'S';
035416160509           TASspl = 'S';
035417160509         ENDIF;
035418160509      /end-free
035500120511     c                   eval      §tasflo = dtas01
035600120511
035700120511      * Bancali
035800120511     c                   clear                   tasban
035900120511    1c                   If        %Subst(w_TasGva:1:1) = 'B'
036000120511     c                   eval      kAr5Trd  = 'BAN'
036100120511     c     kFiar5        Chain     Fiar531c
036200120511If  1c                   If        %Found(Fiar531c)
036300120511     c                   Movel     Ar5Uni        dAr5Ban
036400120511    1c                   EndIf
036500120511     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
036600120511    1c                   EndIf
036700120511
036800120511      * Colli Originali
036900120511    1c                   If        %Subst(w_TasGva:1:1) = 'O'
037000120511     c                   eval      kAr5Trd  = 'BNB'
037100120511     c     kFiar5        Chain     Fiar531c
037200120511If  1c                   If        %Found(Fiar531c)
037300120511     c                   Movel     Ar5Uni        dAr5Bnb
037400120511    1c                   EndIf
037500120511     c                   Eval      TasNcl = §Ar5bcor
037600120511    1c                   EndIf
037700120511
037800120511      * C/Assegno
037900120511     c                   clear                   tascas
038000120511     c                   clear                   tastic
038100120511     c                   clear                   tasvca
038200120511     c                   clear                   tascmb
038300120511    2c                   IF        w_Tasfca <> *blanks
038400120511     c                   Exsr      Carcsb
038500120511    2c                   Endif
038600120511     c     Kevb4         chain     Fnevb04l
038700120511     c                   If        %found(Fnevb04l)
038800120511     c                   eval      tasric = 'S'
038900120511     c                   endif
039000120511
039100120511      * TASSA
039200120511     c                   call      'TICF53R'
039300120511     c                   PARM                    KPJBA
039400120511     c                   PARM                    DTAS
039500120511     c                   PARM                    Dtasv
039600120511
039700120511      * Se ok tassazione
039800120511     c                   If        taserr = *blanks
039900120511      * conta
040000120511     c                   add       1             tottas
040100120511      * imposta dati per WF
040200120511     c                   z-add     tasimv        CFAIMVR
040300120511     c                   z-add     taspor        CFAPORR
040400120511     c                   movea     va            varev
040500120511     c                   movea     sv            svrev
040600120511     c                   endif
040700120511
040800120511     c                   endsr
040900120511
041000060914      *****************************************************************
041100060914      *  Recupero importi di tassazione dal file titai
041200060914      *****************************************************************
041300060914     c     Ric_tai       Begsr
041400060914      *
041500060914     c                   z-add     0             k
041600060914
041700060914     c     ktai05        setll     titai01l
041800060914
041900060914     c                   If        %equal(titai01l)
042000060914
042100060914     c                   do        *hival
042200060914
042300060914     c     ktai05        reade     titai01l
042400060914
042500060914     c                   If        %eof(titai01l)
042600060914     c                   leave
042700060914     c                   endif
042800060914      * imponibile
042900060914     c                   if        taisvt = '£'
043000060914     c                   eval      tasimv = taivat
043100060914     c                   endif
043200060914      * porto
043300060914     c                   if        taisvt = ' '
043400060914     c                   eval      taspor = taivat
043500060914     c                   endif
043600060914      * varie
043700060914     c                   if        taisvt <> ' ' and taisvt <> '£'
043800060914     c                   add       1             k
043900060914     c                   eval      sv(k) = taisvt
044000060914     c                   eval      va(k) = taivat
044100060914     c                   endif
044200060914
044300060914     c                   enddo
044400060914
044500060914     c                   else
044600060914      * se non è pretassata pulisco la divisa
044700060914     c                   clear                   tasdiv
044800060914     c                   endif
044900060914
045000060914     c                   endsr
045100050607      ****************************************************
045200050609      ** Ricerca Contrassegno
045300050607      ****************************************************
045400050607     c     Carcsb        Begsr
045500050607
045600050607     c     Kcsb          chain     Tncsb03l
045700050607     c                   If        %found(tncsb03l) and
045800050607      * solo per stesso tipo bolla
045900120511     c                             w_tastbl = csbtbl
046000050607     c                   movel     csbsta        Tassta
046100050607     c                   z-add     csbcas        Tascas
046200050607     c                   if        csbfus <> *blanks
046300050607     c                   move      csbfus        tastic
046400050607     c                   else
046500050607     c                   move      csbtpi        tastic
046600050607     c                   end
046700050607      * mittente
046800050607     c                   movel     csbvca        tasvca
046900050607     c                   z-add     csbcmb        tascmb
047000050607
047100050607     c                   endif
047200050607
047300050607     c                   endsr
047400050609      *************************************************************************
047500050609      * Operazioni iniziali
047600050609      *************************************************************************
047700050609     C     *inzsr        BEGSR
047800050609     c*
047900060914     c     ktas          klist
048000060914     c                   kfld                    paraa
048100060914     c                   kfld                    Kmgs
048200060914
048300060914     c     ktai05        klist
048400120511     c                   kfld                    w_tasaas
048500120511     c                   kfld                    w_taslnp
048600120511     c                   kfld                    w_tasnrs
048700120511     c                   kfld                    w_tasnsp
048800120511     c                   kfld                    w_tastbl
048900060914
049000060914     c     ktai06        klist
049100120511     c                   kfld                    w_tasaas
049200120511     c                   kfld                    w_taslnp
049300120511     c                   kfld                    w_tasnrs
049400120511     c                   kfld                    w_tasnsp
049500120511     c                   kfld                    w_tastbl
049600060914     c                   kfld                    Ksvt
049700050609
049800050609     c     kFiar5        Klist
049900120511     c                   Kfld                    w_TasAas
050000120511     c                   Kfld                    w_TasLnp
050100120511     c                   Kfld                    w_TasNrs
050200120511     c                   Kfld                    w_TasNsp
050300050609     c                   Kfld                    kAr5Trd
050400050609
050500050609     c     KCSB          KLIST
050600120511     c                   KFLD                    w_TASAAS
050700120511     c                   KFLD                    w_TASLNP
050800120511     c                   KFLD                    w_TASNRS
050900120511     c                   KFLD                    w_TASNSP
051000050609
051100050609     C     KTAB          KLIST
051200050609     C                   KFLD                    CODUT
051300050609     C                   KFLD                    KCOD
051400081105
051500081105      * Chiave file eventi FNEVB
051600081105     c     Kevb4         Klist
051700120511     c                   Kfld                    w_Tasaas
051800120511     c                   Kfld                    w_Taslnp
051900120511     c                   Kfld                    w_Tasnrs
052000120511     c                   Kfld                    w_Tasnsp
052100081105     c                   Kfld                    Wcev
052200081105      *
052300050609     c
052400050609     c     *entry        plist
052500050609     c                   parm                    kpjba
052600050609     c                   movel     kpjbu         param
052700060915
052800090430     c                   time                    wtime            14 0
052900090430     c                   move      wtime         wdata
053000090430     c                   movel     wtime         wora
053100060915      * caricamento schiera dei tipi bolla da scartare
053200060915     c                   eval      Kcod  = 'TB'
053300050609     c                   clear                   tbs
053400050609     c                   z-add     0             xx
053500050609     c     ktab          setll     tabel00f
053600050609     c                   do        *hival
053700050609     c     ktab          reade     tabel00f
053800050609     c                   if        %eof(tabel00f)
053900050609     c                   leave
054000050609     c                   endif
054100050609     c                   movel     tbluni        dstb
054200101108      * con questo confronto facciamo solo gli assegnati
054300101108     c                   if        §tbfcb <> '1' or  §tbtpo = 'F'
054400050609     c                   add       1             xx
054500050609     c                   movel     tblkey        tbs(xx)
054600050609     c                   endif
054700050609     c                   enddo
054800060915      * caricamento schiera dei codici bolla da scartare
054900060915     c                   eval      Kcod  = '3A'
055000060915     c                   z-add     0             xx
055100060915     c     ktab          setll     tabel00f
055200060915     c                   do        *hival
055300060915     c     ktab          reade     tabel00f
055400060915     c                   if        %eof(tabel00f)
055500060915     c                   leave
055600060915     c                   endif
055700060915     c                   movel     tbluni        ds3a
055800060915     c                   if        §3asva <> *blanks
055900060915     c                   add       1             xx
056000060915     c                   movel     tblkey        cbo(xx)
056100060915    3c                   endif
056200060915     c                   enddo
056202160509
056203160509      /free
056204160509         Kcod = '7R';
056205160509         clear kk;
056206160509         setll (Codut:Kcod) TABEL00F;
056207160509         reade (Codut:Kcod) TABEL00F;
056208160509         DOW  not %eof(TABEL00F);
056209160509           IF  TBLflg = *blanks;
056210160509             ds7R = TBLuni;
056211160509             IF  §7Rpincode = 'S';
056212160509               kk += 1;
056213160509               skGMA(kk) = TBLkey;
056214160509             ENDIF;
056215160509           ENDIF;
056216160509           reade (Codut:Kcod) TABEL00F;
056217160509         ENDDO;
056218160509
056219160509      /end-free
056300060915
056400050609     c                   endsr
