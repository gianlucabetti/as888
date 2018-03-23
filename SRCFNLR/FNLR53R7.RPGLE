000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300080508      * FNLR53R7*                                                    *
000400950315      *---------*                                                    *
000500080508      *         - GESTIONE CHIUSURA DISTINTA DA PDA -                *
000600000426      *                                                              *
000700080508      *  Il programma prevede il caricamento a video degli errori    *
000800080508      *  di incongruenza fra i dati ricevuto dal pda e quelli        *
000900080526      *  presenti sugli archivi gestionali oppure incongruenza fra   *
001000080526      *  le spunte.                                                  *
001100950315      *--------------------------------------------------------------*
001200080707     FFNLR53D7  CF   E             WORKSTN SFILE(LR53S01:NRR3)
001300120529     ffnarbj1l  uf a e           k disk    prefix(£)
001400080515     ffnfvv01l  if   e           k disk
001500080515     ffnbrv01l  if   e           k disk
001600080515     ffnbrv03l  if   e           k disk    rename(fnbrv000:fnbrv3) prefix(x)
001700080515     ffnart27l  if   e           k disk
001800110912     ffnblt27l  if   e           k disk
001900080707     ffnarb01l  if   e           k disk
002000080805     ffidst01l  if   e           k disk
002100080707     ffnarb70l  if   e           k disk    rename(fnarb000:fnarb70)
002200080805     ffipcd03l  if   e           k disk
002300080707     ffnlbl02l  if   e           k disk
002400080710     ffipct01l  uf   e           k disk
002500080707     ffipct02l  if   e           k disk    rename(fipct000:fipct2)
002600080707     ftabel00f  if   e           k disk
002700080711     ftntbe01l  if   e           k disk
002800080710     fazorg01l  if   e           k disk
002900111123     ffiar901l  if   e           k disk
003000111123     ffiar601l  if   e           k disk
003100111123     ffnlbl01l  if   e           k disk    rename(fnlbl000:fnlbl1)
003200950320      *----------------------------------------------------------------*
003300950320      *    s c h i e r e                                               *
003400950320      *----------------------------------------------------------------*
003500950315     D KPJBA         E DS
003600080508     D fnlr53ds7     E DS
003700080728     D fidg46ds      E DS
003800080509     D fipctcetds    E DS
003900080707     D fipcdcedds    E DS
004000080707     D fnarbds       E DS                  extname(FNARB00F)
004100110912     D fnartds       E DS                  extname(FNART00F)
004200080707     D fipctds       E DS                  extname(Fipct00F)
004300120529     D fnarbjds      E DS                  prefix(£) extname(fnarbj0F)
004400080707     d trul06ds      e ds
004500080707     d  lin£                   1     90  0 dim(30)
004600080526     d ksped           ds
004700080526     d  kaas                          4  0
004800080526     d  klnp                          3  0
004900080526     d  knrs                          2  0
005000080526     d  knsp                          7  0
005100101109     d korac           ds
005200101116     d  kndc                          7  0
005300101116     d  kdata                         8  0
005400101109     d  kora                          6  0
005500080515     D ddatiute      e ds
005600080515     D azuteds       e ds                  extname(AZUTE00F)
005700080515     D tibs34ds      E DS                  inz
005800120229     D ds1a          e ds
005900080707     D ds2a          e ds
006000120229     D ds3a          e ds
006100080711     D decd          e ds
006200141125ab   D ds7R          e ds
006300081117     D                SDS
006400081117     D  NOMPGM                 1     10
006500080701      * DS per richiamo pgm FNLR36R
006600080701     D DSLR36          DS
006700080701     D  P36AAS                14     17  0
006800080701     D  P36LNP                18     20  0
006900080701     D  P36NRS                21     22  0
007000080701     D  P36NSP                23     29  0
007100080701     D  P36F03                30     30
007200080701     D  P36FLG                31     31
007300080707     d conta           s                   like(£arbprg)
007400080707     d knfvv           s                   like(fvvnfv)
007500080707     d knfv            s                   like(brvnfv)
007600080707     d knpg            s                   like(brvnpg)
007700080707     d knpg4           s                   like(brvnpg) inz(4)
007800080515     d knpg8           s                   like(brvnpg) inz(8)
007900101116     d ora             s             21    dim(9999)
008000101116     d stop            s             24    dim(9999)
008100080526     d sped            s             16    dim(9999)
008200080710     d spu8            s             16    dim(9999)
008300080707     d ora412          s             14  0 inz(0)
008400080707     d ora812          s             14  0 inz(0)
008500141125     d Memorizza_Distinta...
008600141125     d                 s              1
008700950315      *---------------------------------------------------------------*
008800950315      * FLUSSO PRINCIPALE                                             *
008900950315      *---------------------------------------------------------------*
009000080508     C*  Caricamento subfile errori
009100080508     C                   EXSR      INZS01
009200080526     c*  Se il subfile degli errori è vuoto forzo la scrittura
009300120529     c*  del file di lavoro fnarbj0f ed esco
009400080526     C     NRR3          IFEQ      0
009500080526     c                   exsr      sraggio
009600080701     c                   clear                   d537erro
009700080526     c                   exsr      srfine
009800080526     C                   END
009900080701     c*
010000080707     C                   Z-ADD     D537fgsi      v3Clna
010100080710     c                   clear                   v3dlna
010200080710     c     v3clna        chain     azorg01l
010300080710     c                   if        %found(azorg01l)
010400080710     c                   eval      v3dlna = orgdes
010500080710     c                   end
010600080707     C                   Z-ADD     D537nd1i      v3Cnd1
010700080707     C                   Z-ADD     D537nd2i      v3Cnd2
010800080707     C                   Z-ADD     D537nd3i      v3Cnd3
010900080707     C                   Z-ADD     D537nd4i      v3Cnd4
011000080707     C                   Z-ADD     D537nd5i      v3Cnd5
011100080512     C                   Z-ADD     1             NRR3
011200950315      *  Loop gestione subfile degli errori
011300080508     C                   DO        *hival
011400080508      *
011500081117     C                   WRITE     LR53Z01
011600080508     C                   EXFMT     LR53C01
011700080508      *  Fine Lavoro
011800080508     C     *INKC         IFEQ      '1'
011900080701     C                   MOVEL     'F'           d537erro
012000080508     C                   leave
012100080508     C                   END
012200080508      *  Guida
012300080711     C     *INKL         IFEQ      '1'
012400080701     C                   MOVEL     'R'           d537erro
012500080508     C                   leave
012600080508     C                   END
012700080701      *  Interrogazione bolle
012800080701     C     *INKG         IFEQ      '1'
012900080701     C                   MOVEL     ' '           P36FLG
013000080701     C                   Z-ADD     V3CAAS        P36AAS
013100080701     C                   Z-ADD     V3CLNP        P36LNP
013200080701     C                   Z-ADD     V3CNSP        P36NSP
013300080701     C                   Z-ADD     V3CNRS        P36NRS
013400080701     C                   MOVEL     DSLR36        KPJBU
013500080701     C                   CALL      'FNLR36R'
013600080701     C                   PARM                    KPJBA
013700080701     C                   END
013800080508      *  Loop di controlli
013900080508     C                   EXSR      CTR01
014000080701     c   28              iter
014100080526      * se non ci sono + errori scrivo le spedizione nel file di
014200080526      * lavoro ed esco
014300080526     C     *INKF         IFEQ      '1'
014400080701     c                   exsr      sraggio
014500080701     c                   clear                   d537erro
014600080526     c                   exsr      srfine
014700080526     C                   END
014800080508     c
014900080508     C                   ENDdo
015000080508     c*
015100080508     c                   exsr      srfine
015200950317      *---------------------------------------------------------------*
015300950317      * Gestione videata subfile errori                               *
015400950317      *---------------------------------------------------------------*
015500080508     C     INZS01        BEGSR
015600080508     C                   SETOFF                                       202190
015700080508     C                   WRITE     LR53C01
015800950317     C                   SETON                                        2021
015900080512     C                   Z-ADD     0             NRR3              4 0
016000080523     c                   clear                   sped
016100101109     c                   clear                   x                 4 0
016200101109     c                   clear                   ora
016300101109     c                   clear                   z                 4 0
016400101109     c                   clear                   stop
016500101109     c                   clear                   zz                4 0
016600080710     c                   clear                   spu8
016700080710     c                   clear                   y                 4 0
016800080707     c                   do        5             xx                2 0
016900080707     c                   select
017000080707     c                   when      xx = 1 and d537nd1i <> 0
017100080707     c                   z-add     d537nd1i      knfvv
017200080707     c                   z-add     d537nd1i      knfv
017300080707     c                   when      xx = 2 and d537nd2i <> 0
017400080707     c                   z-add     d537nd2i      knfvv
017500080707     c                   z-add     d537nd2i      knfv
017600080707     c                   when      xx = 3 and d537nd3i <> 0
017700080707     c                   z-add     d537nd3i      knfvv
017800080707     c                   z-add     d537nd3i      knfv
017900080707     c                   when      xx = 4 and d537nd4i <> 0
018000080707     c                   z-add     d537nd4i      knfvv
018100080707     c                   z-add     d537nd4i      knfv
018200080707     c                   when      xx = 5 and d537nd5i <> 0
018300080707     c                   z-add     d537nd5i      knfvv
018400080707     c                   z-add     d537nd5i      knfv
018500080707     c                   other
018600080707     c                   leave
018700080707     c                   endsl
018800080805     c* aggancio la distinta per reperire codice autista
018900080805     c     kdst          chain     fidst01l
019000080707     c* verifico se ci anomalie partendo dalle spunte
019100080523     c                   exsr      srspunte
019200080526     c* verifico se ci sono spedizioni senza esiti
019300080523     c                   exsr      srbolle
019400080523     c* verifico se ci sono esiti senza che la spedizione sia in distinta
019500080707     c* e carico tutte le spedizioni che hanno l'esito di consegna forzato
019600080707     c* su as400 cioè tutte quelle inserite perchè il pda si è rotto.
019700080523     c                   exsr      srpda
019800080707     c                   enddo
019900080523     c                   endsr
020000080523     c**********************************************************************
020100080711     c* memorizzo spedizione errata x non ricaricarla + volte nel sfl
020200080523     c**********************************************************************
020300080526     c     srsped        begsr
020400080711     c* att.ne nel campo nascosto del sfl il nr. distinta deve essere
020500080711     c* riempito solamente quando la spedizione è nella distinta che sto
020600080711     c* chiudendo altrimenti lasciarlo vuoto
020700080711     c                   seton                                        9022
020800101215     c* se errore firma proteggo il cod. mancata consegna
020900101215     c                   if        tbeke1 = '011'
021000101215     c                   seton                                        88
021100101215     c                   ELSE
021200101215     c                   setoff                                       88
021300101215     c                   end
021400110114     c                   move      *in88         v3h88
021500101215     c*
021600080526     c                   if        x < 9999
021700080526     c                   add       1             x
021800080526     c                   eval      sped(x) = ksped
021900080526     c                   end
022000080526     C                   Z-ADD     kAAS          v3caas
022100080526     C                   Z-ADD     kLNP          v3clnp
022200080526     C                   Z-ADD     kNSP          v3cnsp
022300080526     C                   Z-ADD     kNRS          v3cnrs
022400080526     C                   ADD       1             NRR3
022500080526     C                   WRITE     LR53S01
022600080526     c                   endsr
022700080526     c**********************************************************************
022800080526     c* verifico se ci anomalie partendo dalle spunte
022900080526     c**********************************************************************
023000080526     c     srspunte      begsr
023100080526     c* errori bloccanti
023200080523     c*
023300080523     c     kfvv          chain     fnfvv01l
023400080523     c                   if        %found(fnfvv01l)
023500080523      * lettura spunte partendo dal foglio 4 con il nr.distinta da controllare
023600080707     c                   move      4             knpg
023700080707     c     kfib1         setll     fnbrv01l
023800080523     c                   do        *hival
023900080707     c     kfib1         reade     fnbrv01l
024000080523     c                   if        %eof(fnbrv01l)
024100080523     c                   leave
024200080523     c                   end
024300080523      * cerca corrispondenza su fnarb se non lo trovo segnalo manca bolla
024400080523     c     kfnart        chain     fnart27l
024500110912     c                   if        not %found(fnart27l)
024600110912     c                   clear                   fipctcetds
024700110912     c                   clear                   fnartds
024800110912     c                   clear                   fnarbds
024900110912     c     kfnart        chain     fnblt27l
025000110912     c                   if        %found(fnblt27l)
025100110912     C                   eval      kAAS= bltaas
025200110912     C                   eval      kLNP= bltlnp
025300110912     C                   eval      kNSP= bltnsp
025400110912     C                   eval      kNRS= bltnrs
025500110912     c                   else
025600110912     C                   CLEAR                   kAAS
025700110912     C                   CLEAR                   kLNP
025800110912     C                   CLEAR                   kNSP
025900110912     C                   CLEAR                   kNRS
026000110912     c                   end
026100110912     c* controllo se ho già caricato la spedizione nel sfl
026200110912     c     ksped         lookup    sped                                   91
026300110912     c                   if        *in91
026400110912     c                   ITER
026500110912     c                   end
026600141125     c                   eval      tbeke1 = '002'
026700141125ab   c                   eval      Memorizza_Distinta = *blank
026800110912     c                   exsr      srmemo
026900110912     c                   iter
027000110912     c                   else
027100080526     c* prima verifico se bolla legata
027200080526     c     klbl          chain     fnlbl02l
027300080707     c                   if        %found(fnlbl02l) and artlna = lbllap
027400080526     c                   eval      kaas =  LBLAAN
027500080526     c                   eval      klnp =  LBLLPN
027600080526     c                   eval      knrs =  LBLNRN
027700080526     c                   eval      knsp =  LBLNSN
027800080526     c                   else
027900080526     c                   eval      kaas =  artaas
028000080526     c                   eval      klnp =  artlnp
028100080526     c                   eval      knrs =  artnrs
028200080526     c                   eval      knsp =  artnsp
028300080526     c                   end
028400080711     c* controllo se ho già caricato la spedizione nel sfl
028500080526     c     ksped         lookup    sped                                   91
028600080526     c                   if        *in91
028700080526     c                   ITER
028800080526     c                   end
028900101215     c                   setoff                                       90
029000080523     c                   clear                   §pctcmc
029100080711     c* reperisco i dati di consegna
029200081117     c     kpct          chain     fipct02l
029300080805     c                   if        %found(fipct02l)
029400080711     c                   eval      fipctcetds = pctdati
029500110912     c                   else
029600110912     c                   clear                   fipctcetds
029700080711     c                   end
029800110103     c     karb          chain     fnarb01l
029900110912     c                   if        %found(fnarb01l)
030000080523      * trovato arb confronta se la linea arrivo è del terminal
030100080526     c     arblna        lookup    lin£                                   99
030200080526     c                   if        not *in99
030300110324     c*                  eval      tbeke1 = '001'
030400141125ab   c                   eval      Memorizza_Distinta = *blank
030500110324     c*                  exsr      srmemo
030600080710     c                   iter
030700080526     c                   endif
030800080523     c                   else
030900110912     c                   clear                   fnarbds
031000110103     c* dirottamento
031100110103     c                   if        artlna <> lbllan
031200110103     c                   eval      tbeke1 = '012'
031300141125ab   c                   eval      Memorizza_Distinta = *blank
031400101111     c                   exsr      srmemo
031500080710     c                   iter
031600110103     c                   else
031700110103      * non trovata bolla
031800110103     c                   eval      tbeke1 = '002'
031900141125ab   c                   eval      Memorizza_Distinta = *blank
032000110103     c                   exsr      srmemo
032100110103     c                   iter
032200080523     c                   endif
032300110103     c                   endif
032400080523     c                   endif
032500080523      *memorizzo data e ora spunta del foglio 4
032600080523     c                   movel     brvdfs        ora412
032700080523     c                   move      brvhms        ora412
032800080523      * congruenza con spunte su foglio 8                                   su
032900080523     c     kfib3         setll     fnbrv03l
033000080523     c                   do        *hival
033100080523     c     kfib3         reade     fnbrv03l
033200080523     c                   if        %eof(fnbrv03l)
033300080523     c                   leave
033400080523     c                   end
033500080523      *memorizzo data e ora spunta del foglio 8
033600080523     c                   if        xbrvdfs =  0
033700080523     c                   movel     xbrvdcs       ora812
033800080523     c                   move      xbrvhcs       ora812
033900080523     c                   else
034000080523     c                   movel     xbrvdfs       ora812
034100080523     c                   move      xbrvhms       ora812
034200080523     c                   end
034300080801     c* memorizzo nella schiera le spedizioni con spunte di rientro
034400080801     c* mi servirà quando controllo gli esiti del pda
034500080801     c                   if        xbrvnfv = knfv
034600080801     c     ksped         lookup    spu8                                   91
034700080801     c                   if        not *in91 and
034800080801     c                             y < 9999
034900080801     c                   add       1             y
035000080801     c                   eval      spu8(y) = ksped
035100080801     c                   end
035200080801     c                   end
035300080523      * estrae se la spunta 4 e 8 corrispondono e la sped. in altra dist.
035400080523     c                   if        xbrvnfv = knfv and
035500080523     c                             ora812 >= ora412 and
035600080523     c                             arbndc <> knfv and arbndc <> 0
035700080711     c                   eval      tbeke1 = '003'
035800141125ab   c                   eval      Memorizza_Distinta = *blank
035900110103     c                   exsr      srmemo
036000080523     c                   leave
036100080523     c                   end
036200080805      * estrae se la spunta 4 e 8 corrispondono e la sped. non è in dist.
036300080523     c                   if        xbrvnfv = knfv and
036400080523     c                             ora812 >= ora412 and
036500080523     c                             arbndc <> knfv and arbndc = 0
036600110103     c                   eval      tbeke1 = '004'
036700141125ab   c                   eval      Memorizza_Distinta = *blank
036800101111     c                   exsr      srmemo
036900110304     c*                  eval      v3cter = 'B'
037000080523     c                   leave
037100080523     c                   end
037200080523     c* spunta di rientro in altra distinta
037300080523     c                   if        xbrvnfv <> knfv and
037400110304     c                             ora812 >= ora412 and arbndc = knfv
037500080711     c                   eval      tbeke1 = '005'
037600141125ab   c                   eval      Memorizza_Distinta = 'S'
037700141125     c                   exsr      srmemo
037800080523     c                   leave
037900080523     c                   endif
038000080710     c                   enddo
038100080523     c                   enddo
038200080523      * lettura spunte partendo dal foglio 8 con il nr.distinta da controllare
038300080523     c                   move      8             knpg
038400080523     c     kfib1         setll     fnbrv01l
038500080523     c                   do        *hival
038600080523     c     kfib1         reade     fnbrv01l
038700080523     c                   if        %eof(fnbrv01l)
038800080523     c                   leave
038900080523     c                   end
039000080523      * controlla se la spedizione è inserita nella stessa distinta
039100080523     c     kfnart        chain     fnart27l
039200110912     c                   if        not %found(fnart27l)
039300110912     c                   clear                   fipctcetds
039400110912     c                   clear                   fnartds
039500110912     c                   clear                   fnarbds
039600110912     c     kfnart        chain     fnblt27l
039700110912     c                   if        %found(fnblt27l)
039800110912     C                   eval      kAAS= bltaas
039900110912     C                   eval      kLNP= bltlnp
040000110912     C                   eval      kNSP= bltnsp
040100110912     C                   eval      kNRS= bltnrs
040200110912     c                   else
040300110912     C                   CLEAR                   kAAS
040400110912     C                   CLEAR                   kLNP
040500110912     C                   CLEAR                   kNSP
040600110912     C                   CLEAR                   kNRS
040700110912     c                   end
040800110912     c* controllo se ho già caricato la spedizione nel sfl
040900110912     c     ksped         lookup    sped                                   91
041000110912     c                   if        *in91
041100110912     c                   ITER
041200110912     c                   end
041300110912     c                   eval      tbeke1 = '002'
041400141125ab   c                   eval      Memorizza_Distinta = *blank
041500110912     c                   exsr      srmemo
041600110912     c                   iter
041700110912     c                   else
041800080526     c* prima verifico se bolla legata
041900080526     c     klbl          chain     fnlbl02l
042000080707     c                   if        %found(fnlbl02l) and artlna = lbllap
042100080526     c                   eval      kaas =  LBLAAN
042200080526     c                   eval      klnp =  LBLLPN
042300080526     c                   eval      knrs =  LBLNRN
042400080526     c                   eval      knsp =  LBLNSN
042500080526     c                   else
042600080526     c                   eval      kaas =  artaas
042700080526     c                   eval      klnp =  artlnp
042800080526     c                   eval      knrs =  artnrs
042900080526     c                   eval      knsp =  artnsp
043000080526     c                   end
043100080711     c* controllo se ho già caricato la spedizione nel sfl
043200080526     c     ksped         lookup    sped                                   91
043300080523     c                   if        *in91
043400080523     c                   ITER
043500080523     c                   end
043600080711     c* memorizzo nella schiera le spedizioni con spunte di rientro
043700080711     c* mi servirà quando controllo gli esiti del pda
043800080711     c     ksped         lookup    spu8                                   91
043900080711     c                   if        not *in91 and
044000080711     c                             y < 9999
044100080711     c                   add       1             y
044200080711     c                   eval      spu8(y) = ksped
044300080711     c                   end
044400080711     c* reperisco i dati di consegna
044500081117     c     kpct          chain     fipct02l
044600080805     c                   if        %found(fipct02l)
044700080711     c                   eval      fipctcetds = pctdati
044800080711     c                   else
044900080711     c                   clear                   fipctcetds
045000080711     c                   end
045100080523     c*
045200080523     c     karb          chain     fnarb01l
045300080526      * incongruenza nr. foglio rientro e distinta su bolla
045400080523     c                   if        %found(fnarb01l) and arbndc <> brvnfv
045500080805      * controllo se la spunta tipo 4 è su un'altra distinta
045600080523     c     kfib3b        chain     fnbrv03l
045700080523     c                   if        %found(fnbrv03l) and xbrvnfv <> brvnfv
045800080523     c                   movel     brvdfs        ora812
045900080523     c                   move      brvhms        ora812
046000080523     c                   movel     xbrvdfs       ora412
046100080523     c                   move      xbrvhms       ora412
046200080523      * se è su un'altro foglio controllo data e ora per vedere se è stata
046300080523      * rimessa in un giro di consegna dopo la spunta di rientro
046400080523     c                   if        ora812 > ora412
046500110304     c                   eval      tbeke1 = '013'
046600141125ab   c                   eval      Memorizza_Distinta = *blank
046700101111     c                   exsr      srmemo
046800080710     c                   iter
046900080523     c                   endif
047000080710     c                   endif
047100080523     c                   endif
047200080523     c                   endif
047300080523     c                   enddo
047400080523     c                   endif
047500980325      *
047600080526     c                   ENDSR
047700080526     c**********************************************************************
047800141125     c* memorizzo errore con o senza numero distinta
047900080526     c**********************************************************************
048000101111     c     srmemo        begsr
048100141125      **
048200141125ab   C                   clear                   v3hndc
048300141125ab   C                   clear                   v3hpdr
048400141125ab    **  in Hidden
048500141125ab    *  memorizzo errore con numero distinta e autista
048600141125ab   c                   if        Memorizza_Distinta = 'S'
048700141125ab   C                   eval      v3hndc = arbndc
048800141125ab   C                   eval      v3hpdr = arbpdc
048900141125ab   c                   end
049000141125ab    **
049100141125ab    ** decodifica tab ECD - Errori pre-chiusura distinta
049200101111     c                   exsr      srtbe
049300141125      **
049400101111     c* se evento forzato da AS400 non memorizzo ora consegna perchè non è
049500101111     c* quella reale
049600101111     c                   if        PCTPDAAS4 = 'A'
049700101111     c                   clear                   v3hdata
049800101111     c                   clear                   v3hora
049900101111     c                   else
050000101111     c                   eval      v3hdata= §PCTdata
050100101111     c                   eval      v3hora = §PCTora
050200101111     c                   end
050300141125      *
050400101111     c                   eval      v3hcmc = §PCTCMC
050500101111     c                   eval      v3Ccmc = §PCTCMC
050600141125      *
050700141125ab    ** decodifica la Mancata Consegna
050800101111     c                   exsr      srtab
050900141125ab    ** scrive la riga del SFL
051000101111     c                   exsr      srsped
051100141125      *
051200101111     c                   ENDSR
051300101111     c**********************************************************************
051400101111     c* verifico se ci sono bolle senza esiti di consegna
051500101111     c**********************************************************************
051600101111     c     srbolle       begsr
051700080526     c* errori forzabili
051800080526     c*
051900080808     c                   z-add     d537fgsi      arbifp
052000080808     c                   z-add     knfv          arbndc
052100080526     c     kdisarb       setll     fnarb70l
052200080526     c                   do        *hival
052300080526     c     kdisarb       reade     fnarb70l
052400080526     c                   if        %eof(fnarb70l)
052500080526     c                   leave
052600080526     c                   end
052700080805     c                   eval      kaas = arbaas
052800080707     c                   eval      klnp = arblnp
052900080707     c                   eval      knrs = arbnrs
053000080707     c                   eval      knsp = arbnsp
053100080711     c* controllo se ho già caricato la spedizione nel sfl
053200080707     c     ksped         lookup    sped                                   91
053300080707     c                   if        *in91
053400080707     c                   ITER
053500080707     c                   end
053600080526     c* verifico se la spedizione ha esiti di consegna
053700081117     c     kpct          chain     fipct02l
053800080805     c                   if        not %found(fipct02l)
053900110524     c                   clear                   fipctcetds
054000080711     c                   eval      tbeke1 = '006'
054100141125ab   c                   eval      Memorizza_Distinta = 'S'
054200141125     c                   exsr      srmemo
054300080710     c                   iter
054400080710     c                   else
054500080710     c* verifico se ho spunta 8
054600080710     c     ksped         lookup    spu8                                   91
054700080710     c                   eval      fipctcetds = pctdati
054800080710     c                   select
054900080805     c* errore se spedizione consegnata e spunta 8
055000080710     c                   when      §PCTCMC = ' ' and *in91
055100080711     c                   eval      tbeke1 = '007'
055200141125ab   c                   eval      Memorizza_Distinta = 'S'
055300141125     c                   exsr      srmemo
055400080710     c                   iter
055500080711     c* verifico il dettaglio ricevuto
055600080711     c* se parziale devo avere almeno una spunta di consegna
055700101007     c                   when      §PCTCMC ='P' and *in91
055800080710     c                   clear                   okcon             1
055900080805     c     kpcd          setll     fipcd03l
056000080710     c                   do        *hival
056100080805     c     kpcd          reade     fipcd03l
056200080805     c                   if        %eof(fipcd03l)
056300080710     c                   leave
056400080710     c                   end
056500080710     c                   eval      fipcdcedds = pcddati
056600080710     c                   if        §PCDSPUCON = 'S'
056700080710     c                   eval      okcon = *on
056800080710     c                   leave
056900080710     c                   end
057000080710     c                   enddo
057100101007     c* non esiste la spunta di consegna solo se esito non forzato da AS400
057200101007     c* (caso di rottura PDA)
057300101007     c                   if        okcon = ' ' and PCTPDAAS4 <> 'A'
057400080711     c                   eval      tbeke1 = '009'
057500141125ab   c                   eval      Memorizza_Distinta = 'S'
057600141125     c                   exsr      srmemo
057700080710     c                   iter
057800080710     c                   end
057900080805     c* spedizione non consegnata e no spunte 8
058000080711     c                   when      §PCTCMC <>' ' and not *in91
058100080711     c                   eval      tbeke1 = '008'
058200141125ab   c                   eval      Memorizza_Distinta = 'S'
058300141125     c                   exsr      srmemo
058400080711     c                   iter
058500080710     c                   endsl
058600080526     c                   end
058700080526     c                   enddo
058800080526     C                   ENDSR
058900080710     c**********************************************************************
059000080710      *  controllo congruenza dati ricevuti dal PDA                   *
059100080710     c**********************************************************************
059200080710     C     srpda         BEGSR
059300080526     c* errori bloccanti
059400080805     c     kdispct       setll     fipct02l
059500080526     c                   do        *hival
059600080707     c     kdispct       reade     fipct02l
059700080526     c                   if        %eof(fipct02l)
059800080526     c                   leave
059900080526     c                   end
060000080526     c                   eval      fipctcetds = pctdati
060100080526     c* verifico se la spedizione è in distinta
060200080707     c                   eval      kaas = pctaas
060300080707     c                   eval      klnp = pctlnp
060400080707     c                   eval      knrs = pctnrs
060500080707     c                   eval      knsp = pctnsp
060600080711     c* controllo se ho già caricato la spedizione nel sfl
060700080707     c     ksped         lookup    sped                                   91
060800080707     c                   if        *in91
060900080707     c                   ITER
061000080707     c                   end
061100080707     c     karb          chain     fnarb01l
061200080711     c                   if        %found(fnarb01l)
061300080711     c* verifico se la spedizione è stata messa in altra distinta
061400080715     c                   if        arbndc <>knfv and arbndc <> 0
061500080711     c                   eval      tbeke1 = '003'
061600141125ab   c                   eval      Memorizza_Distinta = *blank
061700101111     c                   exsr      srmemo
061800080710     c                   iter
061900080526     c                   end
062000080805     c* la spedizione è stata tolta dalla distinta
062100080715     c                   if        arbndc = 0
062200080711     c                   eval      tbeke1 = '004'
062300141125ab   c                   eval      Memorizza_Distinta = *blank
062400101111     c                   exsr      srmemo
062500080711     c                   iter
062600080711     c                   end
062700141125      *
062800141125ab   c* decodifica eventuale Particolarità di consegna
062900141125ab   c                   exsr      srPartCons
063000141125ab    *
063100141125ab    *  controlli su spedizione con PIN code
063200141125ab   c                   if        §7RPINCODE = 'S'
063300141125ab    **   consegnata e
063400141125ab    *  Se NON esitata da AUT ma da SPC
063500141125ab    *     oppure
063600141125ab    *  se Forzata fase CRI
063700141201ab   c                   if        §PCTCMC = *blank  and
063800141125ab   c                             (§PCTFLGSPC='S' or PCTPDAAS4='S')
063900141125ab   c                   eval      tbeke1 = '014'
064000141125ab   c                   eval      Memorizza_Distinta = 'S'
064100141125ab   c                   exsr      srmemo
064200141125ab   c                   iter
064300141125ab   c                   end
064400141125ab    *
064500141125ab   c                   else
064600141125ab    *
064700141125ab    *  Spedizione Senza Pin CODE ma si è
064800141125ab    *   Ricevuto una mancata consegna x "044" - Destinatario senza il PinCode di verifica
064900141201ab   c                   if        §PCTCMC = '044'
065000141125ab   c                   eval      tbeke1 = '015'
065100141125ab   c                   eval      Memorizza_Distinta = 'S'
065200141125ab   c                   exsr      srmemo
065300141125ab   c                   iter
065400141125ab   c                   end
065500141125ab    *
065600141125ab   c                   end
065700141125ab    *
065800101028     c*  carico spedizioni autoconfermate su as400
065900080711     c                   if        arbndc = knfv
066000080707     c                             and PCTPDAAS4 = 'A'
066100080711     c                   eval      tbeke1 = '010'
066200141125ab   c                   eval      Memorizza_Distinta = 'S'
066300141125     c                   exsr      srmemo
066400101116     c                   end
066500101028     c* verifico la firma LDV. Se ho spuntato la LDV deve essrci il record
066600120529     c* nel file di work fnarbj e viceversa, se firmato su pda non deve
066700101028     c* esserci il record nel file di work
066800101028     c                   if        arbndc = knfv
066900120529     c     karb          chain     fnarbj1l
067000120529     c                   if        %found(fnarbj1l)
067100101102     c                   if        (§PCTLDVFIR = ' ' and £arbcmc<>'  §') or
067200101102     c                             (§PCTLDVFIR = 'S' and £arbcmc='  §')
067300101028     c                   eval      tbeke1 = '011'
067400141125ab   c                   eval      Memorizza_Distinta = 'S'
067500141125     c                   exsr      srmemo
067600101028     c                   iter
067700101028     c                   end
067800101116     c                   end
067900101028     c                   end
068000101116     c*
068100080526     c                   end
068200080526     c                   enddo
068300080707     C                   ENDSR
068400080508      *---------------------------------------------------------------*
068500080508      *  Loop di errori  subfile errori                               *
068600080508      *---------------------------------------------------------------*
068700080509     C     CTR01         BEGSR
068800080715     c                   setoff                                       2840
068900950315      *
069000080509     C                   READC     LR53S01                                32
069100980325      *
069200950317     C     *IN32         DOWEQ     '0'
069300950320     C     *IN28         ANDEQ     '0'
069400080704     c*
069500080701     c                   select
069600080509      *  Interrogazione bolla
069700080512     C                   when      v3csce = 'V'
069800950317     C                   MOVEL     '1'           P36FLG
069900950317     C                   Z-ADD     V3CAAS        P36AAS
070000950317     C                   Z-ADD     V3CLNP        P36LNP
070100950317     C                   Z-ADD     V3CNSP        P36NSP
070200950317     C                   Z-ADD     V3CNRS        P36NRS
070300080509     C                   MOVEL(P)  DSLR36        KPJBU
070400950317     C                   CALL      'FNLR36R'
070500080701     c                   PARM                    KPJBA
070600080730     c                   MOVEL     ' '           v3CSCE
070700080728      *  Interrogazione esiti PDA
070800080728     C                   when      v3csce = 'P'
070900080728     C                   clear                   fidg46ds
071000080730     C                   Z-ADD     V3Clna        dg46fgsi
071100080730     C                   Z-ADD     V3hndc        dg46ndci
071200080805     C                   Z-ADD     v3hpdr        dg46codaut
071300080730     C                   Z-ADD     V3caas        dg46aasi
071400080728     C                   Z-ADD     V3CLNP        dg46LNPi
071500080728     C                   Z-ADD     V3CNSP        dg46NSPi
071600080728     C                   Z-ADD     V3CNRS        dg46NRSi
071700080728     C                   MOVEL(P)  fidg46ds      KPJBU
071800080730     C                   CALL      'FIDG49R'
071900080728     c                   PARM                    KPJBA
072000080728     c                   movel     kpjbu         fidg46ds
072100080728     C                   if        dg46erro = '1'
072200080728     C                   MOVEL     'F'           d537erro
072300080730     c                   exsr      srfine
072400080728     c                   end
072500080730     c                   MOVEL     ' '           v3CSCE
072600080728     C                   ENDsl
072700080704     c* controllo il codice consegna
072800080704     c                   exsr      srcmc
072900080704      *  Controllo se l'opzione è ammessa
073000080715      * ATTENZIONE a rendere un errore bloccante perchè non è ammessa
073100080715      * alcuna opzione per proseguire. Se l'errore è bloccante si deve
073200080715      * sistemare da fuori pgm
073300080728     C                   if        ((v3csce = 'I' or
073400080716     C                             v3csce = 'F') and v3cter = 'B') or
073500080728     C                             (v3csce ='F' and v3cter = 'I')  or
073600080728     C                             (v3csce ='I' and v3cter = 'F')
073700080715     c                   seton                                        2840
073800080704     C                   eval      $MSG = 'Opzione non ammessa'
073900080704     c                   end
074000080710     c* obbligo a una scelta
074100080704     C                   if        v3csce = ' ' and *inkf
074200080715     c                   seton                                        2840
074300080707     C                   eval      $MSG = 'Obbligatorio effettuare una scelta'
074400080704     c                   end
074500080716     c* se spedizione in distinta non accetto l'esclusione
074600080728     C                   if        v3csce = 'I' and v3hndc <> 0
074700080716     c                   seton                                        2840
074800080716     C                   eval      $MSG = 'Spedizione in distinta. Opzione non-
074900080716     c                              ammessa'
075000080716     c                   end
075100980325      *  Aggiorno subfile
075200110114     c     v3h88         comp      '1'                                    88
075300950320     C                   SETON                                        22
075400080707     C                   UPDATE    LR53s01
075500950320     C                   SETOFF                                       22
075600080711     C  N28              READC     LR53S01                                32
075700080509     C                   ENDdo
075800080715     c*
075900080715     c                   z-add     1             nrr3
076000950317     C*
076100950317     C                   ENDSR
076200080704      *---------------------------------------------------------------*
076300080704      *  Loop di aggiornamento                                        *
076400080704      *---------------------------------------------------------------*
076500080704     C     sraggio       BEGSR
076600080704     c                   clear                   conta
076700080707      * loop prima sfl errori e poi rimanenti arb senza errori
076800080707     c                   if        nrr3 <> 0
076900080707     C                   READC     LR53S01                                32
077000080707      *
077100080707     C     *IN32         DOWEQ     '0'
077200080707     c                   exsr      sraggiod
077300080711     C                   READC     LR53S01                                32
077400080707     C                   ENDdo
077500080707     c                   end
077600080707     c* loop su spedizioni in distinta senza errori
077700101007     c                   do        5             xx                2 0
077800101007     c                   select
077900101007     c                   when      xx = 1 and d537nd1i <> 0
078000101007     c                   z-add     d537nd1i      knfv
078100101007     c                   when      xx = 2 and d537nd2i <> 0
078200101007     c                   z-add     d537nd2i      knfv
078300101007     c                   when      xx = 3 and d537nd3i <> 0
078400101007     c                   z-add     d537nd3i      knfv
078500101007     c                   when      xx = 4 and d537nd4i <> 0
078600101007     c                   z-add     d537nd4i      knfv
078700101007     c                   when      xx = 5 and d537nd5i <> 0
078800101007     c                   z-add     d537nd5i      knfv
078900101007     c                   other
079000101007     c                   leave
079100101007     c                   endsl
079200101007     c                   z-add     d537fgsi      arbifp
079300101007     c                   z-add     knfv          arbndc
079400080707     c     kdisarb       setll     fnarb70l
079500080704     c                   do        *hival
079600080707     c     kdisarb       reade     fnarb70l
079700080704     c                   if        %eof(fnarb70l)
079800080704     c                   leave
079900080704     c                   end
080000080711     c                   eval      kaas = arbaas
080100080711     c                   eval      klnp = arblnp
080200080711     c                   eval      knrs = arbnrs
080300080711     c                   eval      knsp = arbnsp
080400080707     c                   eval      v3caas = arbaas
080500080707     c                   eval      v3clnp = arblnp
080600080707     c                   eval      v3cnrs = arbnrs
080700080707     c                   eval      v3cnsp = arbnsp
080800080707     c* reperisco i dati di consegna
080900081117     c     kpct          chain     fipct02l
081000080805     c                   if        %found(fipct02l)
081100080707     c                   eval      fipctcetds = pctdati
081200080715     c                   else
081300080715     c                   clear                   fipctcetds
081400080715     c                   end
081500080707     c                   eval      v3csce = 'F'
081600080711     c                   eval      v3hcmc = §pctcmc
081700080711     c                   eval      v3ccmc = §pctcmc
081800110302     c                   if        PCTPDAAS4 = 'A'
081900110302     c                   clear                   v3hdata
082000110302     c                   clear                   v3hora
082100110302     c                   else
082200101111     c                   eval      v3hdata= §pctdata
082300101111     c                   eval      v3hora = §pctora
082400110302     c                   end
082500080707     c                   exsr      sraggiod
082600080704     c                   enddo
082700101007     c                   enddo
082800080704     C*
082900080704     C                   ENDSR
083000950317     C*-----------------------------------------------------*
083100080509     c* scrivo oppure annullo la spedizione nel work file
083200080509     C*-----------------------------------------------------*
083300080704     C     sraggiod      BEGSR
083400120529     c     karbd         chain     fnarbj1l
083500080512     c* forzo la spedizione nel work file
083600080512     c                   if        v3csce = 'F'
083700080707     c     karbd         chain     fnarb01l
083800120529     c                   movel     fnarbds       fnarbjds
083900080704     c* bisogna visualizzare per prima le spedizioni con dei motivi di non
084000080711     c* consegna quindi imposto il cod.manc.cons. nell'ordinamento di ARBS
084100080711     c                   if        v3ccmc <> ' '
084200080704     c                   movel     v3ccmc        £arbtpr
084300080711     c                   else
084400080711     c                   eval      £arbtpr = *hival
084500111123      *imposto valore senza incasso eventualmente lo modifico durante i controlli
084600111123     c                   eval      £arbfinc= '9'
084700111123     c                   exsr      contrass
084800080711     c                   end
084900101109     c* verifico se ora consegna già memorizzata
085000101111     c                   if        v3hdata <> 0
085100101116     c                   eval      kndc  = v3hndc
085200101116     c                   eval      kdata = v3hdata
085300101111     c                   eval      kora = v3hora
085400101110     c                   z-add     1             z
085500101109     c     korac         lookup    ora(z)                                 44
085600101109     c                   if        *in44
085700101109     c                   move      stop(z)       £arbstp
085800101109     c                   else
085900101109     c                   add       1             zz
086000101116     c                   movel     korac         ora(zz)
086100101110     c                   movel     korac         stop(zz)
086200101109     c                   move      £arbstp       stop(zz)
086300101109     c                   end
086400101111     c                   end
086500080711     c* correggo il codice di mancata consegna sul fipct00f se diverso
086600080710     c                   if        v3hcmc <> v3ccmc
086700080710     c                   eval      kaas =  v3caas
086800080710     c                   eval      klnp =  v3clnp
086900080710     c                   eval      knrs =  v3cnrs
087000080710     c                   eval      knsp =  v3cnsp
087100081117     c     kpct01        chain     fipct01l
087200081117     c                   if        %found(fipct01l)
087300080710     c                   eval      fipctcetds = pctdati
087400080710     c                   movel     v3ccmc        §pctcmc
087500080710     c                   eval      pctdati = fipctcetds
087600080710     c                   update    fipct000
087700080704     c                   end
087800080710     c                   end
087900080710     c                   end
088000080707     c* annulllo la spedizione nel work file se già presente
088100120529     c                   if        %found(fnarbj1l)
088200080728     c                   if        v3csce = 'I'
088300080707     c                   eval      £arbatb = 'A'
088400080707     c                   end
088500120529     c                   update    fnarbj00
088600080512     c                   else
088700080715     c                   if        v3csce = 'F'
088800080704     c                   eval      £arbprg = conta + 1
088900080711     c                   eval      conta = £arbprg
089000120529     c                   write     fnarbj00
089100080512     c                   end
089200080715     c                   end
089300080509     C                   ENDSR
089400080509     C*-----------------------------------------------------*
089500080509     C*  Controllo codice di mancata consegna
089600080509     C*-----------------------------------------------------*
089700080509     C     srcmc         BEGSR
089800080701     c* ricerca
089900080512     C     '?'           SCAN      v3ccmc                                 95
090000080509     C     *IN95         IFEQ      '1'                                          --- 03 ---
090100080509     C                   MOVEL     'GNP*****'    §TIP
090200080707     C                   MOVEL     *BLANKS       V3ccmc
090300080509     C                   CALL      'TRUL40R'
090400080509     C                   PARM                    §TIP              8
090500080509     C                   PARM                    §KEY              8
090600080509     C                   PARM                    §DES             30
090700080512     C                   MOVEL     §KEY          v3ccmc
090800080509     C                   END                                                    --- 03 ---
090900080701     c*
091000080707     c                   exsr      srtab
091100080701     c*
091200080509     C                   ENDSR
091300080526      *---------------------------------------------------------------*
091400080707      * codice mancata consegna
091500080526      *---------------------------------------------------------------*
091600080707     C     srtab         BEGSR
091700080711     c                   clear                   v3dcmc
091800080711     c                   setoff                                       41
091900080711     c                   if        v3ccmc <> ' '
092000111123     c                   eval      tblcod = '2A'
092100080711     c                   movel(p)  v3ccmc        tblkey
092200080707     c     ktab          chain     tabel00f
092300080707     c                   if        not %found(tabel00f) or
092400080707     c                             (%found(tabel00f) and tblflg <> ' ')
092500080707     c                   seton                                        4128
092600080707     c                   else
092700080707     c                   movel(p)  tbluni        ds2a
092800080707     c                   movel(p)  §2ades        v3dcmc
092900080707     c                   end
093000080711     c                   end
093100080707     C                   ENDSR
093200141125ab    *---------------------------------------------------------------*
093300141125ab    * codice Particolarità di Consegna  decodificato se presente in bolla
093400141125ab    *---------------------------------------------------------------*
093500141125ab   C     srPartCons    BEGSR
093600141125ab    * x controllo PINCODE
093700141125ab   c                   clear                   ds7R
093800141125ab   c                   if        arbGMA <> ' '
093900141125ab   c                   eval        tblcod = '7R'
094000141125ab   c                   eval        tblkey = arbGMA
094100141125ab   c     ktab          chain     tabel00f
094200141201ab   c                   if        %found(tabel00f) and tblflg = ' '
094300141125ab   c                   movel(p)  tbluni        ds7R
094400141125ab   c                   end
094500141125ab   c                   end
094600141125ab   C                   ENDSR
094700111123      *---------------------------------------------------------------*
094800111123      * Verifica se presente contrassegno o assegnato per impostare campi ordinamento
094900111123      *---------------------------------------------------------------*
095000120229     C     contrass      begsr
095100120229     c                   move      '3A'          tblcod
095200120229     c                   movel(p)  arbcbo        tblkey
095300120229     c     ktab          chain     tabel00f
095400120229     c                   if        %found(tabel00f)
095500120229     c                   movel     tbluni        ds3a
095600120229     c                   if        §3afca > 0
095700120229     c     kar9          chain     fiar901l
095800120229     c                   if        not %found(fiar901l)
095900120229     c     kar9          chain     fnlbl01l
096000120229     c                   if        %found(fnlbl01l)
096100120229     c     kar9m         chain     fiar901l
096200120229     c                   if        not %found(fiar901l)
096300120229     c                   leavesr
096400120229     c                   endif
096500120229     c                   endif
096600120229     c                   endif
096700120229     c                   z-add     ar9cas        £arbimpa
096800120229     c                   move      '1A'          tblcod
096900120229     c                   movel(p)  ar9tic        tblkey
097000120229     c     ktab          chain     tabel00f
097100120229     c                   if        %found(tabel00f)
097200120229     c                   movel     tbluni        ds1a
097300120229     c                   select
097400120229     C                   when      §1AFMB = 'M'
097500120229     c                   move      '1'           £arbfinc
097600120229     C                   when      §1AFMB = 'B'
097700120229     c                   move      '5'           £arbfinc
097800120229     c                   other
097900120229     c                   move      '7'           £arbfinc
098000120229     c                   endsl
098100120229     c                   endif
098200120229     c                   else
098300120229      *solo assegnato
098400120229     c     kar9          chain     fiar601l
098500120229     c                   if        %found(fiar601l)
098600120229     c                   z-add     ar6ift        £arbimpa
098700120229     c                   move      '8'           £arbfinc
098800120229     c                   endif
098900120229     c                   endif
099000120229     c                   endif
099100120229     C                   ENDSR
099200080711      *---------------------------------------------------------------*
099300080711      * decodifica errore
099400080711      *---------------------------------------------------------------*
099500080711     C     srtbe         BEGSR
099600080711     c                   eval      v3cmsg = *all'?'
099700080711     c                   eval      v3cter = 'B'
099800080711     c     ktbe          chain     tntbe01l
099900080711     c                   if        (%found(tntbe01l) and tbeatb = ' ')
100000080711     c                   movel(p)  tbeuni        decd
100100080711     c                   movel(p)  §ecddes       v3cmsg
100200080711     c                   movel(p)  §ecdter       v3cter
100201180102      * per errore MANCA BOLLA riporta riferimenti ultima spunta rilevata per capire
100202180102      * a chi si riferisce anomalia riscontrata
100203180102      *
100204180102     c                   if        tbeke1 = '002'
100205180102     c                   Eval      V3cmsg =
100206180102     c                                     %trim(V3cmsg) + ' ' +
100207180108     c                                     'Spunta collo ' +
100208180102     c                                     %trim(%editc(brvnpg:'Z')) + ' ' +
100209180102     c                                     %trim(%editc(brvlnp:'Z')) + ' ' +
100210180102     c                                     %trim(%editc(brvnrs:'Z')) + ' ' +
100211180102     c                                     %trim(%editc(brvnsc:'Z'))
100212180102     c                   endif
100213180102      *
100300080711     c                   end
100400080711     C                   ENDSR
100500080707      *---------------------------------------------------------------*
100600080707      * fine
100700080707      *---------------------------------------------------------------*
100800080707     C     srfine        BEGSR
100900080730     c                   movel(p)  fnlr53ds7     kpjbu
101000080526     C*
101100080526     C                   SETON                                        LR
101200080710     c                   return
101300080707     c*
101400080707     C                   ENDSR
101500080509     C*-----------------------------------------------------*
101600080509     C     *INZSR        BEGSR
101700080509     C*-----------------------------------------------------*
101800080508      *
101900080508     C     *ENTRY        PLIST
102000080707     C                   PARM                    fnlr53ds7
102100080515     C* Decodifica Elaboratore
102200080515     c     *dtaara       define    §azute        azuteds
102300080515     c     *dtaara       define    §datiute      ddatiute
102400080515     C                   in(E)     *dtaara
102500080515     C                   IF        %Error  or  RSUT = *blanks
102600080515     C                   call      'TIBS34R'
102700080515     C                   parm                    Tibs34Ds
102800080515     C                   in        *dtaara
102900080515     c                   ENDIF
103000080515     c*
103100080515     c     kfvv          klist
103200080707     c                   kfld                    knpg4
103300080515     c                   kfld                    knfvv
103400080701     c                   kfld                    d537fgsi
103500080805     c     kdst          klist
103600080805     c                   kfld                    knpg4
103700080805     c                   kfld                    knfv
103800080805     c                   kfld                    d537fgsi
103900080515     c     kfnart        klist
104000080515     c                   kfld                    brvlnp
104100080515     c                   kfld                    brvlna
104200080515     c                   kfld                    brvnrs
104300080515     c                   kfld                    brvnsc
104400080707     c     klbl          klist
104500080707     c                   kfld                    artaas
104600080707     c                   kfld                    artlnp
104700080707     c                   kfld                    artnrs
104800080707     c                   kfld                    artnsp
104900111123     c     kar9          klist
105000111123     c                   kfld                    arbaas
105100111123     c                   kfld                    arblnp
105200111123     c                   kfld                    arbnrs
105300111123     c                   kfld                    arbnsp
105400111123     c     kar9m         klist
105500111123     c                   kfld                    lblaap
105600111123     c                   kfld                    lbllpp
105700111123     c                   kfld                    lblnrp
105800111123     c                   kfld                    lblnsp
105900080707     c     karb          klist
106000080526     c                   kfld                    kaas
106100080526     c                   kfld                    klnp
106200080526     c                   kfld                    knrs
106300080526     c                   kfld                    knsp
106400080707     c     karbd         klist
106500080707     c                   kfld                    v3caas
106600080707     c                   kfld                    v3clnp
106700080707     c                   kfld                    v3cnrs
106800080707     c                   kfld                    v3cnsp
106900080707     c     kpcd          klist
107000080805     c                   kfld                    dstfgs
107100080805     c                   kfld                    dstnfv
107200080805     c                   kfld                    dstpdr
107300080805     c                   kfld                    pcdtrd
107400080707     c                   kfld                    kaas
107500080707     c                   kfld                    klnp
107600080707     c                   kfld                    knrs
107700080707     c                   kfld                    knsp
107800080805     c                   eval      pcdtrd = 'CED'
107900080707     c     kpct          klist
108000080805     c                   kfld                    dstfgs
108100080805     c                   kfld                    dstnfv
108200080805     c                   kfld                    dstpdr
108300080805     c                   kfld                    pcttrd
108400080707     c                   kfld                    kaas
108500080707     c                   kfld                    klnp
108600080707     c                   kfld                    knrs
108700080707     c                   kfld                    knsp
108800080707     c                   eval      pcttrd = 'CET'
108900081117     c     kpct01        klist
109000081117     c                   kfld                    kaas
109100081117     c                   kfld                    klnp
109200081117     c                   kfld                    knrs
109300081117     c                   kfld                    knsp
109400081117     c                   kfld                    pcttrd
109500080515     c     kfib1         klist
109600080515     c                   kfld                    knpg
109700080515     c                   kfld                    knfv
109800080701     c                   kfld                    d537fgsi
109900080515
110000080515     c     kfib3         klist
110100080515     c                   kfld                    knpg8
110200080515     c                   kfld                    brvlnp
110300080515     c                   kfld                    brvlna
110400080515     c                   kfld                    brvnrs
110500080515     c                   kfld                    brvnsc
110600080515     c                   kfld                    brvfgs
110700080515     c     kfib3b        klist
110800080707     c                   kfld                    knpg4
110900080515     c                   kfld                    brvlnp
111000080515     c                   kfld                    brvlna
111100080515     c                   kfld                    brvnrs
111200080515     c                   kfld                    brvnsc
111300080515     c                   kfld                    brvfgs
111400080707     c     kdisarb       klist
111500080707     c                   kfld                    arbifp
111600080707     c                   kfld                    arbndc
111700080707     c     kdispct       klist
111800080805     c                   kfld                    dstfgs
111900080805     c                   kfld                    dstnfv
112000080805     c                   kfld                    dstpdr
112100080707     c                   kfld                    pcttrd
112200080707     c     ktab          klist
112300080707     c                   kfld                    TBLKUT
112400080707     c                   kfld                    TBLCOD
112500080707     c                   kfld                    TBLKEY
112600080707     c                   eval      tblkut = 1
112700080711     c     ktbe          klist
112800080711     c                   kfld                    TBeCOD
112900080711     c                   kfld                    TBeKE1
113000080711     c                   eval      tbecod = 'ECD'
113100080515     C*  Caricamento £1
113200080515     c                   clear                   trul06ds
113300080515     c                   eval      d06cod = '£1'
113400080515     c                   movel     dutpou        d06key
113500080515     c                   movel     trul06ds      kpjbu
113600080515     c                   call      'TRUL06R'
113700080515     c                   parm                    kpjba
113800080515     c                   movel     kpjbu         trul06ds
113900080515     c*
114000080509     C                   ENDSR
114100080515
