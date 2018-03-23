000100040721      *===============================================================*
000200050317      * TNSB16R    * Rigenerazione immagine LdV per clienti           *
000300040721      *===============================================================*
000400040721
000500040721     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000600040721
000700040721      *===============================================================*
000800040721
000900050318     fTITAS31C  uf   e           k disk
001000050325     fTITAS30C  If   e           k disk    prefix(O_)
001100050325     f                                     rename(titas000:tas00)
001200050325     f                                     rename(titas010:tas10)
001300050325     f                                     rename(titasP00:tas0P)
001400121022     fTILAC00F  o    e             disk
001500050324     fTABEL00F  if   e           k disk
001600050324     fFNLBL01L  if   e           k disk
001700040721
001800040721      *===============================================================*
001900040721
002000040721      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
002100040721      *
002200040721     d KPJBA         e ds
002300040721      * - Parametri x Controllo profilo utenti
002400040721     d TIBS34DS      e ds                  inz
002500040721      * - Ds di riferimento al file esterno AZUTE00F
002600040721     d AZUTEDS       e ds                  extname(AZUTE00F)
002700040721      * - Ds per dati organigramma
002800040721     d DDatiUte      e ds
002900050318      * - Ds per campo TASFLO del file TITAS31C
003000040721     d dTASflo       e ds                  inz
003100050325      * - Ds per controlli bolla (serve x gestione unico test)
003200050325     d titasds       e ds                  extname(TITAS00F)
003300050325     d titasdsO      e ds                  extname(TITAS00F) prefix(O_)
003400050325     d titasdsChk    e ds                  extname(TITAS00F) prefix(Chk_)
003500050317      *
003600050317     d dlac          e ds                  inz
003700050324     d dstb          e ds                  inz
003800080314      * DS PER TIBS02R - GESTIONE TNTBE00F
003900080314     d TIBS02ds      e ds                  inz
004000090209
004100090209       // - Parametri x TRUL55R
004200090209     d TRUL55ds      e ds                  inz
004300090209     d   I55tla      e                     inz('C')
004400090209
004500040721      *
004600040721     d Status         sds           333
004700040721     d   SDSpgm          *proc
004800040721     d   SDSjob              244    253                                         Job name
004900040721      *
005000050317     d TNSB16ds        ds                  inz
005100050317     d   D16dcd                       8  0 inz
005200050317     d   D16dca                       8  0 inz
005300050317     d   d16ksc                       7  0 inz
005400050317     d   d16imm                       1    inz
005500090310     d   d16tad                       1    inz
005600050324     d   D16dsd                       8  0 inz
005700050324     d   D16dsa                       8  0 inz
005800050616     d   D16dir                      30    inz
005900050617     d   D16fmi                       2    inz
006000090310     d   d16ksu                       7  0 inz
006100090310     d   d16tadu                      1    inz
006200090310     d   d16fimp                      1    inz
006300090310     d   d16imp                      10  3 inz
006400090310     d   d16kscf                      7  0 inz
006500090310     d   d16ctr                       3    inz
006600090310     d   d16job                      16    inz
006700090310     d   d16tpt                       1    inz
006800090310     d   d16res                       1    inz
006900090310     d   d16rec                       1    inz
007000090310     d   d16csr                       1    inz
007100090310     d   d16ssr                       1    inz
007200090310     d   d16lnp                       3  0 inz
007300040721      *
007400040721     d DS_Time1        ds            14    inz
007500040721     d   dsT1_hms                     6  0 inz
007600040721     d   dsT1_dmy                     8  0 inz
007700040721     d DS_Time2        ds            14    inz
007800040721     d   dsT2_ymd                     8  0 inz
007900040721     d   dsT2_hms                     6  0 inz
008000040721      *
008100040721      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
008200040721      *
008300040721      * - Campi di comodo
008400040721     d WdateISO        s               d   datfmt(*ISO) inz
008500040721     d Wtime           s             14  0              inz
008600040721     d WtasDCM         s              8                 inz(*zeros)
008700050330     d WtasAAS         s              4    inz(*zeros)
008800050617     d WtasDSP         s              8    inz(*zeros)
008900050324     d savtbl          s                   like(tastbl) inz('  ')
009000050324     d kcod            s                   like(tblcod) inz('TB')
009100050324     d kkey            s                   like(tblkey)
009200090209
009300090209       //--------------------------------------------------------------
009400090209       //?Definizione procedure usate.                                 ?
009500090209       //--------------------------------------------------------------
009600090209
009700090209       // - Determinazione nome dell'immagine per TILAC
009800090209     d trul55r         pr                  extpgm('TRUL55R')
009900090209     d   trul55ds...
010000090209     d                                     likeds(trul55ds)
010100090209
010200050318      * Predisposizione indicatori per riconoscere i tipi record
010300050318      *   reperiti dal file TITAS31C
010400050318     iTITAS000      31
010500050318     iTITAS010      32
010600050318     iTITASP00      33
010700040721
010800040721      *===============================================================*
010900040721      *  RIEPILOGO INDICATORI                                         *
011000040721      *---------------------------------------------------------------*
011100040721      * 10    - Comodo                                                *
011200040721      *===============================================================*
011300040721
011400040721     c     *Entry        plist
011500040721     c                   parm                    KPJBA
011600050317     c                   movel     KPJBU         TNSB16ds
011700040721      *
011800040721      * Operazioni iniziali
011900040721     c                   exsr      OperazIniz
012000040721      *
012100050318      * Ciclo di lettura del file TITAS31C
012200050318     c                   setoff                                       313233
012300050318     c     d16ksc        setll     TITAS31C
012400050318     c                   if        d16imm = 'G'
012500050318     c     d16ksc        reade(N)  TITAS31C
012600050318     c                   else
012700050318     c     d16ksc        reade     TITAS31C
012800050318     c                   endif
012900040721      *
013000050318do  1c                   dow       NOT %eof(TITAS31C)
013100050324     c                   select
013200050324     c                   when      d16dcd > 0
013300050324     c                   if        tasdcm < d16dcd or tasdcm > d16dca
013400050324     c                   goto      leggi
013500050324     c                   endif
013600050324     c                   when      d16dsd > 0
013700050325     c                   if        (tasaas*10000+tasmgs) < d16dsd or
013800050325     c                             (tasaas*10000+tasmgs) > d16dsa
013900050324     c                   goto      leggi
014000050324     c                   endif
014100050324     c                   endsl
014200040721      *
014300050324     c* controllo se la bolla è da inviare in base a tabella "LAC"
014400050324     c                   exsr      chkbol
014500050324     c                   if        cntbol = 'E'
014600050324     c                   goto      leggi
014700050324     c                   endif
014800050324     c*
014900040721     c                   movel     TASflo        dTASflo
015000050325      * elaborazione delle sole LdV con immagine
015100050317     c                   select
015200050317if  2c                   when      §FLOiml  = 'I' and (d16imm = 'G' or
015300050317     c                             d16imm = 'E')
015400050317     c                   exsr      Wrt_TILAC
015500050317     c*
015600050317if  2c                   when      §FLOiml  = 'S' and (d16imm = 'M' or
015700050317     c                             d16imm = 'E')
015800050317     c                   exsr      Wrt_TILAC
015900050317e   2c                   endsl
016000050324     c*
016100050324     c     leggi         tag
016200050324     c                   if        d16imm <> 'G'
016300050324     c                   unlock    TITAS31C
016400050324     c                   endif
016500040721      *
016600050318      * - Lettura del record successivo nel file TITAS31C
016700050318     c                   setoff                                       313233
016800050318     c                   if        d16imm = 'G'
016900050318     c     d16ksc        reade(N)  TITAS31C
017000050318     c                   else
017100050318     c     d16ksc        reade     TITAS31C
017200050318     c                   endif
017300040721      *
017400040721e   1c                   enddo
017500040721      *
017600040721      * Fine
017700040721     c                   eval      *inLR = *on
017800090209      /free
017900090209           reset  TRUL55ds;
018000090209           trul55r ( TRUL55ds );
018100090209      /end-free
018200040721      *
018300040721      *---------------------------------------------------------------*
018400040721      * Operazioni iniziali                                           *
018500040721      *---------------------------------------------------------------*
018600040721     c     OperazIniz    BEGSR
018700040721      *
018800040721      * Definizione Key-List
018900050325     c*
019000050324     c     Ktab          klist
019100050324     C                   KFLD                    CODUT
019200050324     C                   KFLD                    kcod
019300050324     c                   kfld                    kkey
019400050324     c                   z-add     1             codut             1 0
019500050324     c*
019600050324     c     Klbl          klist
019700050324     c                   kfld                    TASaas
019800050324     c                   kfld                    TASlnp
019900050324     c                   kfld                    TASnrs
020000050324     c                   kfld                    TASnsp
020100050325     c*
020200050325     c     Ktas          klist
020300050325     c                   kfld                    LBLaao
020400050325     c                   kfld                    LBLlpo
020500050325     c                   kfld                    LBLnro
020600050325     c                   kfld                    LBLnso
020700040721      *
020800040721      * Reperisco dati job
020900040721     c                   exsr      DatiJob
021000040721      *
021100050317      * Impostazione del campo LAcTIM = aaaa/mm/gg+hh:mm:ss
021200040721     c                   time                    Wtime
021300040721     c                   movel     Wtime         DS_Time1
021400040721     c                   eval      dsT2_hms   = dsT1_hms
021500040721     c     *eur          movel     dsT1_dmy      WdateISO
021600040721     c                   move      WdateISO      dsT2_ymd
021700050317     c*
021800050317     c* Reperisco dati cliente da tabella "LAC"
021900050325     C                   clear                   tibs02ds
022000050317     c                   clear                   dlac
022100050317     C                   MOVEL     'C'           T02MOD
022200050317     C                   MOVEL     KNSIF         T02SIF
022300050317     C                   MOVEL     'LAC'         T02COD
022400050317     c                   movel(P)  d16ksc        t02ke1
022500050317     C                   CALL      'TIBS02R'
022600050317     C                   PARM                    KPJBA
022700050325     C                   PARM                    tibs02ds
022800050317     c                   eval      dlac = t02uni
022900040721      *
023000040721     c                   ENDSR
023100040721      *
023200040721      *---------------------------------------------------------------*
023300040721      * Reperimento Dati del job (Utente/Operativi)                   *
023400040721      *---------------------------------------------------------------*
023500040721     c     DatiJob       BEGSR
023600040721      *
023700040721     c     *dtaara       define    §azute        azuteds
023800040721     c     *dtaara       define    §datiute      ddatiute
023900040721      *
024000040721     c                   in(E)     *dtaara
024100040721     c                   IF        %ERROR or RSUT = *blanks
024200040721     c                   clear                   Tibs34Ds
024300040721     c                   call      'TIBS34R'
024400040721     c                   parm                    Tibs34Ds
024500040721     c                   in        *dtaara
024600040721     c                   ENDIF
024700040721      *
024800040721     c                   ENDSR
024900050324      *
025000050324      *---------------------------------------------------------------*
025100050324      * Controllo se immagine bolla da inviare                        *
025200050324      *---------------------------------------------------------------*
025300050324     c     Chkbol        BEGSR
025400050325     c*
025500050325     c                   clear                   cntbol            1
025600050325     c*
025700050325     c* Parto dal presupposto che la bolla corrente nn abbia legami
025800050325     c                   eval      titasdsChk = titasds
025900050325     c*
026000050325     c* Cerco sempre d reperire la bolla originale x effettuare i controllo su d essa
026100050325     c     Klbl          chain     fnlbl01l
026200050325     c                   if        %found(fnlbl01l)
026300050325     c     Ktas          chain     titas30c
026400050325     c                   if        %found(titas30c)
026500050325     c                   eval      titasdsChk = titasdsO
026600050325     c                   endif
026700050325     c                   endif
026800050325     c*
026900050325     c* A questo punto effettuo le verifiche d congruità rispetto alla tabella LAC del cliente
027000050325     c                   if        Chk_tastbl <> savtbl
027100050325     c                   movel(p)  Chk_tastbl    kkey
027200050324     c                   clear                   dstb
027300050325     c     ktab          chain     tabel00f
027400050324     c                   if        %found(tabel00f)
027500050324     c                   movel     tbluni        dstb
027600050324     c                   endif
027700050325     c                   eval      savtbl = Chk_tastbl
027800050324     c                   endif
027900050325     c* controllo porto (rispetto alla bolla originale)
028000090310     c                   if        d16tpt <> 'E' and §tbtpo <> d16tpt
028100050324     c                   eval      cntbol = 'E'
028200050324     c                   goto      end_chkbol
028300050324     c                   endif
028400090313     c* reso (rispetto alla bolla corrente, ovvero diciamo "figlia")
028500090310     c                   if        d16res <> 'S' and tasfbr = 'S'
028600050324     c                   eval      cntbol = 'E'
028700050324     c                   goto      end_chkbol
028800050324     c                   endif
028900050325     c* recupero (rispetto alla bolla originale)
029000090310     c                   if        d16rec <> 'S' and §tbrbl = 'R'
029100050324     c                   eval      cntbol = 'E'
029200050324     c                   goto      end_chkbol
029300050324     c                   endif
029400050325     c* c/servizio (rispetto alla bolla originale)
029500090310     c                   if        d16csr <> 'S' and §tbrbl = 'C'
029600050324     c                   eval      cntbol = 'E'
029700050324     c                   goto      end_chkbol
029800050324     c                   endif
029900050325     c* serie (rispetto alla bolla originale)
030000090310     c                   if        d16ssr = 'S' and Chk_tasnrs = *zeros
030100050324     c                   eval      cntbol = 'E'
030200050324     c                   goto      end_chkbol
030300050324     c                   endif
030400090310      * linea di partenza (rispetto alla bolla originale)
030500090310     c                   if        d16lnp <> *zeros and
030600090310     c                             d16lnp <> chk_taslnp
030700090310     c                   eval      cntbol = 'E'
030800090310     c                   goto      end_chkbol
030900090310     c                   ENDIF
031000050325     c*
031100050324     c     end_chkbol    endsr
031200040721      *
031300040721      *---------------------------------------------------------------*
031400050317      * Registrazione dati nel file TILAC00F x cliente                *
031500040721      *---------------------------------------------------------------*
031600050317     c     Wrt_TILAC     BEGSR
031700040721      *
031800040721      * Impostazione dei campi numerici in campi alfanumerici di comodo
031900040721     c                   movel     TASdcm        WtasDCM
032000050330     c                   move      TASaas        WtasAAS
032100050617     c                   movel     TASaas        WtasDSP
032200050617     c                   move      TASMGS        WtasDSP
032300040721      *
032400050317     c                   clear                   TILAC000
032500050317     c                   move      TASaas        LACaas
032600050317     c                   move      TASlnp        LAClnp
032700050317     c                   move      TASnrs        LACnrs
032800050317     c                   move      TASnsp        LACnsp
032900050317     c                   movel     'CLI'         LACcop
033000040721      *
033100040721      * Impostazione nuovi dati
033200050317     c                   eval      LACtim  = DS_Time2
033300090310     c                   if        d16kscf <> *zeros
033400090310     c                   eval      lacidl = %editc(d16kscf:'X') + d16ctr
033500090310     c                   else
033600090313     c                   eval      lacidl = %editc(tasksc:'X') +
033700090313     c                                      %editc(tasctr:'X')
033800090310     c                   endif
033900050317     c                   eval      LACela  = *zeros
034000050616     c                   eval      LACdir  = d16dir
034100050317     c                   eval      LACtpi  = §lactpi
034200050324     c* flag nome immagine:
034300050324     c* se = "S "=significa che deve essere impostato col n.sped (ci pe
034400050324     c*           sa Laguna) --> quindi lascio = blanks
034500090209      /free
034600090209         clear  LACnim;
034700090209         if  D16fmi <> 'S ';
034800090209           clear  TRUL55ds;
034900090209           I55fmi = D16fmi;
035000090209           I55lnp = TASlnp;
035100090209           I55nrs = TASnrs;
035200090209           I55nsp = TASnsp;
035300090209           I55aas = TASaas;
035400090209           I55mgs = TASmgs;
035500090209           I55rmn = TASrmn;
035600090209           trul55r ( TRUL55ds );
035700090209           if  O55err = *off;
035800090209             LACnim = O55nim;
035900090209           endif;
036000090209         endif;
036100090209      /end-free
036200090310     c                   eval      lactad = d16tad
036300090310      * imposto i nuovi campi di TILAC
036400090310     c                   eval      lactela = 'S'
036500090323     c                   eval      lacksc = d16ksc
036600090310     c                   eval      lacksu = d16ksu
036700090310     c                   eval      lactadu = d16tadu
036800090310     c                   eval      lacfimp = d16fimp
036900090310     c                   eval      lacimp = d16imp
037000090310     c                   eval      lacidjob = d16job
037100040721      *
037200050317     c                   WRITE     TILAC000
037300050318     c*
037400050318     c* Aggiorno flag immagine su titas se immagine mai inviata
037500050318if  2c                   if        §FLOiml  = 'S'
037600050318     c                   eval      §FLOiml = 'I'
037700050318     c                   movel     dTASflo       TASflo
037800050318     c   31              UPDATE    TITAS000
037900050318     c   32              UPDATE    TITAS010
038000050318     c   33              UPDATE    TITASP00
038100050318     c                   else
038200050318     c                   unlock    TITAS31C
038300050318     c                   endif
038400040721      *
038500040721     c                   ENDSR
