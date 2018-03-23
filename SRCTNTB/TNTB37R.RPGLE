000100050721      *---------------------------------------------------------------*
000200160325      * Gestione tabella "MVP" = Blocco caricamento Peso/Volume       *
000300160325      *                          (NON più solo VDL)                   *
000400050721      *---------------------------------------------------------------*
000500050721
000600050721     h decedit('0,') datedit(*dmy/)
000700050721
000800050721      *---------------------------------------------------------------*
000900050721      *   A R C H I V I                                               *
001000050721      *---------------------------------------------------------------*
001100050721      *
001200050721     fAZORG01L  if   e           k disk
001300050721      *
001400050721     fTNTBE01L  uf a e           k disk
001500050721      *
001600050721     fTNTB37D   cf   e             workstn
001700050721
001800050721      *---------------------------------------------------------------*
001900050721      *   C O S T A N T I                                             *
002000050721      *---------------------------------------------------------------*
002100050721      *
002200050721     d DigitN          c                   const('0123456789')
002300050721
002400050721      *---------------------------------------------------------------*
002500050721      *   S C H I E R E                                               *
002600050721      *---------------------------------------------------------------*
002700050721      *
002800050721     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002900050721     d $Msg            s             78    dim(09) ctdata perrcd(1)             Messaggi video
003000050721
003100050721      *---------------------------------------------------------------*
003200050721      *   S T R U T T U R E   D A T I                                 *
003300050721      *---------------------------------------------------------------*
003400050721      *
003500050721      * Parametri
003600050721     d KPJBA         e ds
003700050721      *
003800050721      * Passaggio Parametri al pgm TIBS02R
003900050721     d TIBS02DS      e ds                  inz
004000050721     d  T02mod       e                     inz('R')
004100050721     d  T02cod       e                     inz('MVP')
004200050721      *
004300050721      * Tabella MVP = VDL - Blocco caricamento Peso/Volume
004400050721     d dMVP          e ds                  inz
004500050721      *
004600050721      * Tracciato record file TNTBE00F
004700050721     d TNTBEds       e ds                  extname(TNTBE00F) inz
004800050721     d xTNTBEds      e ds                  extname(TNTBE00F) inz
004900050721     d                                     prefix(TBX:3)
005000050721      *
005100050721     d TIBS34DS      e ds                  inz
005200050721     d DDATIUTE      e ds                  inz
005300050721     d AZUTEDS       e ds                  extname(AZUTE00F) inz
005400050721      *
005500050721     d WLBdat          ds                  inz
005600050721     d  G02dat                 1      8  0 inz
005700050721     d  G02inv                 9     16  0 inz
005800050721     d  G02err                17     17    inz('3')
005900050721     d  G02tgi                18     22  0 inz
006000050721      *
006100050721      * Ora (minuti:secondi)
006200050721     d Wtime           ds             4    inz
006300050721     d  Whh                    1      2  0 inz
006400050721     d  Wmm                    3      4  0 inz
006500050721      *
006600050721     d                sds
006700050721     d  VTCpgm           *proc
006800050721
006900050721      *---------------------------------------------------------------*
007000050721      *   V A R I A B I L I                                           *
007100050721      *---------------------------------------------------------------*
007200050721      *
007300050721     d $Err            s              1a   inz(*off)
007400050721     d $Fine           s              1a   inz(*off)
007500050721     d $CarV1          s              1a   inz(*on)
007600050721     d $CarV2          s              1a   inz(*off)
007700050721     d $CarW1          s              1a   inz(*off)
007800050721     d $TipVid         s              1a   inz('1')
007900050721     d $Tibs02         s              1a   inz(*off)
008000050721     d Win             s             99a   inz(*zeros)
008100050721     d wTasto          s              2a   inz(*zeros)
008200050721      *
008300160325      * -?Date per blocco VDL?
008400160325     d W2Cvdi          s                   like(V2Cvdi)  inz
008500160325     d W2Cvdf          s                   like(V2Cvdf)  inz
008600160325     d W2Cpdi          s                   like(V2Cpdi)  inz
008700160325     d W2Cpdf          s                   like(V2Cpdf)  inz
008800160325      * -?Date per blocco RPVP?
008900160325     d W2CvdiR         s                   like(V2CvdiR) inz
009000160325     d W2CvdfR         s                   like(V2CvdfR) inz
009100160325     d W2CpdiR         s                   like(V2CpdiR) inz
009200160325     d W2CpdfR         s                   like(V2CpdfR) inz
009300160325      * -?Date per blocco TRANSPALLET?
009400160325     d W2CpdiT         s                   like(V2CpdiT) inz
009500160325     d W2CpdfT         s                   like(V2CpdfT) inz
009600050721
009700050721      *---------------------------------------------------------------*
009800050721      *   M A I N   L I N E                                           *
009900050721      *---------------------------------------------------------------*
010000050721      *  Riepilogo indicatori utilizzati:                             *
010100050721      *  --------------------------------                             *
010200050721      *  01 - Record inesistente (inserimento)                        *
010300050721      *  02 - Record esistente   (modifica)                           *
010400050721      *  04 - Record annullato   (ripristino)                         *
010500050721      *  20 - Comodo                                                  *
010600050721      *  22 - Errori in scrittura record (WRITE)                      *
010700050721      *  50 - Posizionamento cursore su "Codice P.O."                 *
010800160325      *  51 - Posizionamento cursore su data inizio blocco volume VDL *
010900160325      *  52 - Posizionamento cursore su ora  inizio blocco volume VDL *
011000160325      *  53 - Posizionamento cursore su data  fine  blocco volume VDL *
011100160325      *  54 - Posizionamento cursore su ora   fine  blocco volume VDL *
011200160325      *  55 - Posizionamento cursore su data inizio blocco peso VDL   *
011300160325      *  56 - Posizionamento cursore su ora  inizio blocco peso VDL   *
011400160325      *  57 - Posizionamento cursore su data  fine  blocco peso VDL   *
011500160325      *  58 - Posizionamento cursore su ora   fine  blocco peso VDL   *
011600160325      *  59 - Posizionam. cursore su data inizio blocco volume RPVP   *
011700160325      *  60 - Posizionam. cursore su ora  inizio blocco volume RPVP   *
011800160325      *  61 - Posizionam. cursore su data  fine  blocco volume RPVP   *
011900160325      *  62 - Posizionam. cursore su ora   fine  blocco volume RPVP   *
012000160325      *  63 - Posizionam. cursore su data inizio blocco peso RPVP     *
012100160325      *  64 - Posizionam. cursore su ora  inizio blocco peso RPVP     *
012200160325      *  65 - Posizionam. cursore su data  fine  blocco peso RPVP     *
012300160325      *  66 - Posizionam. cursore su ora   fine  blocco peso RPVP     *
012400160325      *  67 - Posiz. cursore su data inizio blocco peso TRANSPALLET   *
012500160325      *  68 - Posiz. cursore su ora  inizio blocco peso TRANSPALLET   *
012600160325      *  69 - Posiz. cursore su data  fine  blocco peso TRANSPALLET   *
012700160325      *  70 - Posiz. cursore su ora   fine  blocco peso TRANSPALLET   *
012800050721      *  99 - Visualizzazione messaggio di errore                     *
012900050721      *---------------------------------------------------------------*
013000050721      *
013100050721      * Operazioni iniziali
013200050721     c                   exsr      RutInz
013300050721      *
013400050721      * Gestione video
013500050721     c                   dow       $Fine = *off
013600050721     c     $TipVid       caseq     '1'           GesV1
013700050721     c     $TipVid       caseq     '2'           GesV2
013800050721     c     $TipVid       caseq     'A'           GesW1
013900050721     c                   endcs
014000050721     c                   enddo
014100050721      *
014200050721      * Fine
014300050721     c                   if        $Tibs02 = *on
014400050721     c                   clear                   TIBS02ds
014500050721     c                   movel     'C'           T02tla
014600050721     c                   call      'TIBS02R'
014700050721     c                   parm                    KPJBA
014800050721     c                   parm                    TIBS02ds
014900050721     c                   endif
015000050721      *
015100050721     c                   movel     *on           *inLR
015200050721
015300050721      *---------------------------------------------------------------*
015400050721      * RutInz - Operazioni Iniziali                                  *
015500050721      *---------------------------------------------------------------*
015600050721     c     RutInz        BEGSR
015700050721      *
015800050721      * Ricezione parametri
015900050721     c     *entry        plist
016000050721     c                   parm                    KPJBA
016100050721      *
016200050721      * Definizioni chiavi di accesso
016300050721     c     K05TBE01      klist                                                  *tntbe01l
016400050721     c                   kfld                    TBEcod                         -tabella
016500050721     c                   kfld                    TBEke1                         -chiave uno
016600050721     c                   kfld                    TBEke2                         -chiave due
016700050721     c                   kfld                    TBElin                         -lingua
016800050721     c                   kfld                    TBEsif                         -s.informativo
016900050721      *
017000050721      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
017100050721     c     *dtaara       define    §azute        azuteds
017200050721     c     *dtaara       define    §datiute      ddatiute
017300050721      *
017400050721     c                   clear                   AzUteDs
017500050721     c                   clear                   DDatiUte
017600050721     c                   clear                   Tibs34Ds
017700050721     c                   in(E)     *dtaara
017800050721if  1c                   if        %Error  or  RSUT = *blanks
017900050721     c                   call      'TIBS34R'
018000050721     c                   parm                    Tibs34Ds
018100050721     c                   in        *dtaara
018200050721e   1c                   endif
018300050721      *-- Verifica errori e autorità profilo
018400050721sel 1c                   SELECT
018500050721      *-- controllo se ho errori nei dati utente
018600050721      *--   nel qual caso NON risulta un profilo abilitato
018700050721w   1c                   WHEN      DUTerr = 'E'
018800050721     c                   eval      $Fine  = *on
018900050721      *
019000160325      *-- CONTROLLO AUTORIZZAZIONI
019100050721      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
019200050721      *-- se il chiamante non richiede autorità specifica verificare
019300050721      *--   quella del profilo
019400050721      *-- se il chiamante richiede autorità specifica verificarla,
019500050721      *--  se è blank verificare quella del profilo
019600050721      *
019700050721      * se UTEAUT = *BLANK non siamo in sede
019800050721w   1c                   WHEN      UTEaut = *blank
019900050721      *
020000050721x   1c                   OTHER
020100050721      *
020200050721e   1c                   ENDSL
020300050721      *
020400050721      * Aggancio dati generali della tabella in esame
020500050721     c                   clear                   TBEcod
020600050721     c                   move      *zeros        TBEke1
020700050721     c                   move      T02cod        TBEke1
020800050721     c                   clear                   TBEke2
020900050721     c                   clear                   TBElin
021000050721     c                   movel     KNSIF         TBEsif
021100050721     c     K05TBE01      chain     TNTBE01L
021200050721     c                   if        not %found(TNTBE01L)
021300050721     c                   clear                   TBEsif
021400050721     c     K05TBE01      chain     TNTBE01L
021500050721     c                   endif
021600050721     c                   if        %found(TNTBE01L)
021700050721     c                   movel     TNTBEds       xTNTBEds
021800050721     c                   else
021900050721     c                   clear                   xTNTBEds
022000050721     c                   endif
022100050721      *
022200050721     c                   clear                   TB37V1
022300050721      *
022400050721     c                   ENDSR
022500050721
022600050721      *---------------------------------------------------------------*
022700050721      * GESV1  - Gestione videata selezione codice tabella            *
022800050721      *---------------------------------------------------------------*
022900050721     c     GesV1         BEGSR
023000050721      *
023100050721      * Inizializzazione videata
023200050721if  1c                   if        $CarV1 = *on
023300050721     c                   exsr      CarV1
023400050721     c                   movel     *off          $CarV1
023500050721e   1c                   endif
023600050721      *
023700050721      * Scrivo la testata
023800050721     c                   clear                   T1opz
023900050721     c                   write     TB37T1
024000050721      *
024100050721      * Se esistono errori sulla videata
024200050721      * emetto la write del formato a indicatori spenti per vedere
024300050721      * le eventuali decodifiche
024400050721if  1c                   if        *in99
024500050721     c                   movea     *in           Win
024600050721     c                   movea     *zeros        *in(50)
024700050721     c                   write     TB37V1
024800050721     c                   movea     Win           *in
024900050721e   1c                   endif
025000050721      *
025100050721     c                   exfmt     TB37V1
025200050721     c                   eval      *in99 = *off
025300050721     c                   clear                   V1Dmsg
025400050721      *
025500050721      * Verifica se richiesta ricerca
025600050721     c                   eval      *in20 = *off
025700050721     c     '?'           scan      V1Cfgs                                 20
025800050721      *
025900050721sel 1c                   select
026000050721      * F3=Fine
026100050721w   1c                   when      *inKC
026200050721     c                   exsr      F03V1
026300050721      * '?'=Ricerche
026400050721w   1c                   when      *in20
026500050721     c                   exsr      Search
026600050721      *
026700050721      * Controllo dati immessi a video
026800050721x   1c                   other
026900050721     c                   exsr      CtrV1
027000050721      *
027100050721      * Passaggio alla videata di dettaglio
027200050721if  2c                   if        not *in99
027300050721     c                   eval      $CarV2 = *on
027400050721     c                   eval      $TipVid = '2'
027500050721e   2c                   endif
027600050721      *
027700050721e   1c                   endsl
027800050721      *
027900050721     c                   ENDSR
028000050721
028100050721      *---------------------------------------------------------------*
028200050721      * CARV1  - Caricamento dati prima videata                       *
028300050721      *---------------------------------------------------------------*
028400050721     c     CarV1         BEGSR
028500050721      *
028600050721     c                   movea     *zeros        *in(50)
028700050721     c                   movea     '00000'       *in(01)
028800050721      *
028900050721     c                   move (p)  '?'           V1Cfgs
029000050721     c                   clear                   V1Dfgs
029100050721      *
029200050721     c                   ENDSR
029300050721
029400050721      *---------------------------------------------------------------*
029500050721      * CTRV1  - Controllo e decodifica prima videata                 *
029600050721      *---------------------------------------------------------------*
029700050721     c     CtrV1         BEGSR
029800050721      *
029900050721     c                   movea     *zeros        *in(50)
030000050721     c                   clear                   V1Dmsg
030100050721      *
030200050721      * - Punto Operativo obbligatorio
030300050721if  1c                   if        V1Cfgs = *blanks
030400050721     c                   seton                                        50  99
030500050721     c                   eval      V1Dmsg = $Msg(01)
030600050721     c                   goto      EndCtrV1
030700050721e   1c                   endif
030800050721      * - Punto Operativo errato
030900050721     c                   exsr      CtrPO
031000050721if  1c                   if        *in99
031100050721     c                   eval      V1Dmsg = $Msg(02)
031200050721     c                   seton                                        50  99
031300050721     c                   goto      EndCtrV1
031400050721e   1c                   endif
031500050721      *
031600050721     c                   movel     ORGdes        V1Dfgs
031700050721      *
031800050721     c     EndCtrV1      ENDSR
031900050721
032000050721      *---------------------------------------------------------------*
032100050721      * F03V1  - Tasto funzionale F03 -> Fine programma               *
032200050721      *---------------------------------------------------------------*
032300050721     c     F03V1         BEGSR
032400050721      *
032500050721     c                   movel     *on           $Fine                          fine pgm
032600050721      *
032700050721     c                   ENDSR
032800050721
032900050721      *---------------------------------------------------------------*
033000050721      * CTRPO  - Controllo Punto Operativo                            *
033100050721      *---------------------------------------------------------------*
033200050721     c     CtrPO         BEGSR
033300050721      *
033400050721     c                   clear                   ORGdes
033500050721     c                   move      V1Cfgs        ORGfil
033600050721     c     ORGfil        chain     AZORG01L
033700050721if  1c                   if        not %found(AZORG01L)
033800050721     c                             or  ORGfva =  '*'
033900050721     c                   seton                                            99
034000050721e   1c                   endif
034100050721      *
034200050721     c                   ENDSR
034300050721
034400050721      *---------------------------------------------------------------*
034500050721      * SEARCH - Ricerche                                             *
034600050721      *---------------------------------------------------------------*
034700050721     c     Search        BEGSR
034800050721      *
034900050721     c                   reset                   TIBS02ds
035000050721     c                   movel     KNSIF         T02sif
035100050721     c                   call      'TIBS02R'
035200050721     c                   parm                    KPJBA
035300050721     c                   parm                    TIBS02ds
035400050721     c                   if        T02err  = *blanks
035500050721     c                   movel     T02ke1        V1Cfgs
035600050721     c                   endif
035700050721     c                   eval      $Tibs02 = *on
035800050721      *
035900050721     c                   ENDSR
036000050721
036100050721      *---------------------------------------------------------------*
036200050721      * GESV2  - Gestione videata dettaglio dati                      *
036300050721      *---------------------------------------------------------------*
036400050721     c     GesV2         BEGSR
036500050721      *
036600050721      * Inizializzazione videata
036700050721     c                   if        $CarV2 = *on
036800050721     c                   exsr      CarV2
036900050721     c                   move      *off          $CarV2
037000050721     c                   endif
037100050721      * Scrivo la testata
037200050721     c                   write     TB37T1
037300050721      *
037400050721      * Se esistono errori sulla videata
037500050721      * emetto la write del formato a indicatori spenti per vedere
037600050721      * le eventuali decodifiche
037700050721     c                   if        *in99
037800050721     c                   movea     *in           Win
037900050721     c                   movea     *zeros        *in(50)
038000050721     c                   write     TB37V2
038100050721     c                   movea     Win           *in
038200050721     c                   endif
038300050721      *
038400050721if  1c                   if        *in05
038500050721     c                   write     TB37V2
038600050721     c                   exfmt     PROTECT
038700050721x   1c                   else
038800050721     c                   exfmt     TB37V2
038900050721e   1c                   endif
039000050721     c                   eval      *in99 = *off
039100050721     c                   clear                   V1Dmsg
039200050721     c                   clear                   wTasto
039300050721      *
039400050721sel 1c                   select
039500050721      * F03=Fine
039600050721w   1c                   when      *inKC
039700050721     c                   exsr      F03V1
039800050721     c                   goto      EndGesV2
039900050721      * F12=Ritorno
040000050721w   1c                   when      *inKL
040100050721     c                   exsr      F12V2
040200050721     c                   goto      EndGesV2
040300050721e   1c                   endsl
040400050721      *
040500050721      * Controllo dati immessi a video
040600050721      * (non si fanno se richisto l'annullamento)
040700050721if  1c                   if        not *inKQ
040800050721     c                   exsr      CtrV2
040900050721e   1c                   endif
041000050721      *
041100050721      * Aggiornamento se non ci sono errori
041200050721if  1c                   if        not *in99
041300050721     c                             and (*inKF or *inKE or *inKQ)
041400050721sel 2c                   select
041500050721w   2c                   when      *inKE
041600050721     c                   eval      wTasto = '05'
041700050721w   2c                   when      *inKF
041800050721     c                   eval      wTasto = '06'
041900050721w   2c                   when      *inKQ
042000050721     c                   eval      wTasto = '16'
042100050721e   2c                   endsl
042200050721     c                   eval      $CarW1 = *on
042300050721     c                   eval      $TipVid = 'A'
042400050721e   1c                   endif
042500050721      *
042600050721     c     EndGesV2      ENDSR
042700050721
042800050721      *---------------------------------------------------------------*
042900050721      * CARV2  - Caricamento dati seconda videata                     *
043000050721      *---------------------------------------------------------------*
043100050721     c     CarV2         BEGSR
043200050721      *
043300050721     c                   clear                   T1opz
043400050721     c                   movea     '00000'       *in(01)
043500050721      *
043600050721      * Aggancio la tabella, se trovo il codice sono in modifica
043700050721      * o ripristino (se record annullato), altrimenti in immissione
043800050721     c                   exsr      ChnTBE
043900050721      *
044000050721if  1c                   if        %found(TNTBE01L)
044100050721      *
044200050721      * MODIFICA/RIPRISTINO
044300050721     c                   movel     TBEuni        dMVP
044400050721if  2c                   if        TBEatb = *blanks
044500050721     c                   eval      *in02  = *on
044600050721     c                   eval      T1opz  = $Opz(02)
044700050721x   2c                   else
044800050721     c                   eval      *in04  = *on
044900050721     c                   eval      T1opz  = $Opz(06)
045000050721e   2c                   endif
045100050721      *
045200050721x   1c                   else
045300050721      *
045400050721      * IMMISSIONE
045500050721     c                   clear                   dMVP
045600050721     c                   eval      *in01  = *on
045700050721     c                   eval      T1opz  = $Opz(01)
045800050721      *
045900050721e   1c                   endif
046000050721      *
046100050721      * DATI IN VIDEATA "V2":
046200050721      *
046300050721     c                   move      V1Cfgs        V2Cfgs
046400050721     c                   eval      V2Dfgs  = V1Dfgs
046500160325      *
046600160325      * -?Data/Ora blocco Volume VDL?
046700050721     c                   eval      V2Cvbl  = §MVPvbl
046800050721if  1c                   if        §MVPvdi > *zeros
046900050721     c                   reset                   WLBdat
047000050721     c                   z-add     §MVPvdi       G02inv
047100050721     c                   call      'XSRDA8'
047200050721     c                   parm                    WLBdat
047300050721     c                   z-add     G02dat        V2Cvdi
047400050721x   1c                   else
047500050721     c                   clear                   V2Cvdi
047600050721e   1c                   endif
047700050721if  1c                   if        §MVPvhi > *zeros
047800050721     c                   eval      V2Cvhi  = §MVPvhi
047900050721x   1c                   else
048000050721     c                   clear                   V2Cvhi
048100050721e   1c                   endif
048200050721if  1c                   if        §MVPvdf > *zeros
048300050721     c                   reset                   WLBdat
048400050721     c                   z-add     §MVPvdf       G02inv
048500050721     c                   call      'XSRDA8'
048600050721     c                   parm                    WLBdat
048700050721     c                   z-add     G02dat        V2Cvdf
048800050721x   1c                   else
048900050721     c                   clear                   V2Cvdf
049000050721e   1c                   endif
049100050721if  1c                   if        §MVPvhf > *zeros
049200050721     c                   eval      V2Cvhf  = §MVPvhf
049300050721x   1c                   else
049400050721     c                   clear                   V2Cvhf
049500050721e   1c                   endif
049600160325      * -?Data/Ora blocco Peso VDL?
049700050721     c                   eval      V2Cpbl  = §MVPpbl
049800050721if  1c                   if        §MVPpdi > *zeros
049900050721     c                   reset                   WLBdat
050000050721     c                   z-add     §MVPpdi       G02inv
050100050721     c                   call      'XSRDA8'
050200050721     c                   parm                    WLBdat
050300050721     c                   z-add     G02dat        V2Cpdi
050400050721x   1c                   else
050500050721     c                   clear                   V2Cpdi
050600050721e   1c                   endif
050700050721if  1c                   if        §MVPphi > *zeros
050800050721     c                   eval      V2Cphi  = §MVPphi
050900050721x   1c                   else
051000050721     c                   clear                   V2Cphi
051100050721e   1c                   endif
051200050721if  1c                   if        §MVPpdf > *zeros
051300050721     c                   reset                   WLBdat
051400050721     c                   z-add     §MVPpdf       G02inv
051500050721     c                   call      'XSRDA8'
051600050721     c                   parm                    WLBdat
051700050721     c                   z-add     G02dat        V2Cpdf
051800050721x   1c                   else
051900050721     c                   clear                   V2Cpdf
052000050721e   1c                   endif
052100050721if  1c                   if        §MVPphf > *zeros
052200050721     c                   eval      V2Cphf  = §MVPphf
052300050721x   1c                   else
052400050721     c                   clear                   V2Cphf
052500050721e   1c                   endif
052600160325      *
052700160325      * -?Data/Ora blocco Volume RPVP?
052800160325     c                   eval      V2CvblR  = §MVPvblR
052900160325if  1c                   if        §MVPvdiR > *zeros
053000160325     c                   reset                   WLBdat
053100160325     c                   z-add     §MVPvdiR      G02inv
053200160325     c                   call      'XSRDA8'
053300160325     c                   parm                    WLBdat
053400160325     c                   z-add     G02dat        V2CvdiR
053500160325x   1c                   else
053600160325     c                   clear                   V2CvdiR
053700160325e   1c                   endif
053800160325if  1c                   if        §MVPvhiR > *zeros
053900160325     c                   eval      V2CvhiR  = §MVPvhiR
054000160325x   1c                   else
054100160325     c                   clear                   V2CvhiR
054200160325e   1c                   endif
054300160325if  1c                   if        §MVPvdfR > *zeros
054400160325     c                   reset                   WLBdat
054500160325     c                   z-add     §MVPvdfR      G02inv
054600160325     c                   call      'XSRDA8'
054700160325     c                   parm                    WLBdat
054800160325     c                   z-add     G02dat        V2CvdfR
054900160325x   1c                   else
055000160325     c                   clear                   V2CvdfR
055100160325e   1c                   endif
055200160325if  1c                   if        §MVPvhfR > *zeros
055300160325     c                   eval      V2CvhfR  = §MVPvhfR
055400160325x   1c                   else
055500160325     c                   clear                   V2CvhfR
055600160325e   1c                   endif
055700160325      * -?Data/Ora blocco Peso RPVP?
055800160325     c                   eval      V2CpblR  = §MVPpblR
055900160325if  1c                   if        §MVPpdiR > *zeros
056000160325     c                   reset                   WLBdat
056100160325     c                   z-add     §MVPpdiR      G02inv
056200160325     c                   call      'XSRDA8'
056300160325     c                   parm                    WLBdat
056400160325     c                   z-add     G02dat        V2CpdiR
056500160325x   1c                   else
056600160325     c                   clear                   V2CpdiR
056700160325e   1c                   endif
056800160325if  1c                   if        §MVPphiR > *zeros
056900160325     c                   eval      V2CphiR  = §MVPphiR
057000160325x   1c                   else
057100160325     c                   clear                   V2CphiR
057200160325e   1c                   endif
057300160325if  1c                   if        §MVPpdfR > *zeros
057400160325     c                   reset                   WLBdat
057500160325     c                   z-add     §MVPpdfR      G02inv
057600160325     c                   call      'XSRDA8'
057700160325     c                   parm                    WLBdat
057800160325     c                   z-add     G02dat        V2CpdfR
057900160325x   1c                   else
058000160325     c                   clear                   V2CpdfR
058100160325e   1c                   endif
058200160325if  1c                   if        §MVPphfR > *zeros
058300160325     c                   eval      V2CphfR  = §MVPphfR
058400160325x   1c                   else
058500160325     c                   clear                   V2CphfR
058600160325e   1c                   endif
058700160325      *
058800160325      * -?Data/Ora blocco Peso TRANSPALLET?
058900160325     c                   eval      V2CpblT  = §MVPpblT
059000160325if  1c                   if        §MVPpdiT > *zeros
059100160325     c                   reset                   WLBdat
059200160325     c                   z-add     §MVPpdiT      G02inv
059300160325     c                   call      'XSRDA8'
059400160325     c                   parm                    WLBdat
059500160325     c                   z-add     G02dat        V2CpdiT
059600160325x   1c                   else
059700160325     c                   clear                   V2CpdiT
059800160325e   1c                   endif
059900160325if  1c                   if        §MVPphiT > *zeros
060000160325     c                   eval      V2CphiT  = §MVPphiT
060100160325x   1c                   else
060200160325     c                   clear                   V2CphiT
060300160325e   1c                   endif
060400160325if  1c                   if        §MVPpdfT > *zeros
060500160325     c                   reset                   WLBdat
060600160325     c                   z-add     §MVPpdfT      G02inv
060700160325     c                   call      'XSRDA8'
060800160325     c                   parm                    WLBdat
060900160325     c                   z-add     G02dat        V2CpdfT
061000160325x   1c                   else
061100160325     c                   clear                   V2CpdfT
061200160325e   1c                   endif
061300160325if  1c                   if        §MVPphfT > *zeros
061400160325     c                   eval      V2CphfT  = §MVPphfT
061500160325x   1c                   else
061600160325     c                   clear                   V2CphfT
061700160325e   1c                   endif
061800050721      *
061900050721     c                   ENDSR
062000050721
062100050721      *---------------------------------------------------------------*
062200050721      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
062300050721      *---------------------------------------------------------------*
062400050721     c     F12V2         BEGSR
062500050721      *
062600050721     c                   eval      $TipVid = '1'
062700050721      *
062800050721     c                   ENDSR
062900050721
063000050721      *---------------------------------------------------------------*
063100050721      * CTRV2  - Controllo e decodifica seconda videata               *
063200050721      *---------------------------------------------------------------*
063300050721     c     CtrV2         BEGSR
063400050721      *
063500050721     c                   movea     *zeros        *in(50)
063600050721     c                   clear                   V1Dmsg
063700050721      *
063800160325      * -?Flag blocco Volume VDL => Ctrl eseguito nel dspf?
063900050721      *
064000160325      * -?Data Inizio blocco Volume VDL?
064100050721     c                   clear                   W2Cvdi
064200050721if  1c                   if            V2Cvbl =  'S'
064300050721     c                             and V2Cvdi =  *zeros
064400050721     c                   seton                                        51  99
064500050721     c                   eval      V1Dmsg =  $Msg(03)
064600050721     c                   leavesr
064700050721e   1c                   endif
064800050721if  1c                   if        V2Cvdi >  *zeros
064900050721     c                   clear                   WLBdat
065000050721     c                   z-add     V2Cvdi        G02dat
065100050721     c                   call      'XSRDA8'
065200050721     c                   parm                    WLBdat
065300050721if  2c                   if        G02err =  *on
065400050721     c                   seton                                        51  99
065500050721     c                   eval      V1Dmsg =  $Msg(04)
065600050721     c                   leavesr
065700050721x   2c                   else
065800050721     c                   z-add     G02dat        V2Cvdi
065900050721     c                   z-add     G02inv        W2Cvdi
066000050721e   2c                   endif
066100050721e   1c                   endif
066200050721      *
066300160325      * -?Ora  Inizio blocco Volume VDL?
066400050721     c                   move      V2Cvhi        Wtime
066500050721if  1c                   if            Whh    <  *zeros
066600050721     c                             or  Whh    >  24
066700050721     c                             or  Wmm    <  *zeros
066800050721     c                             or  Wmm    >  59
066900050721     c                   seton                                        52  99
067000050721     c                   eval      V1Dmsg =  $Msg(05)
067100050721     c                   leavesr
067200050721e   1c                   endif
067300050721      *
067400160325      * -?Data  Fine  blocco Volume VDL?
067500050721     c                   clear                   W2Cvdf
067600050721if  1c                   if            V2Cvbl =  'S'
067700050721     c                             and V2Cvdf =  *zeros
067800050721     c                   seton                                        53  99
067900050721     c                   eval      V1Dmsg =  $Msg(06)
068000050721     c                   leavesr
068100050721e   1c                   endif
068200050721if  1c                   if        V2Cvdf >  *zeros
068300050721     c                   clear                   WLBdat
068400050721     c                   z-add     V2Cvdf        G02dat
068500050721     c                   call      'XSRDA8'
068600050721     c                   parm                    WLBdat
068700050721if  2c                   if        G02err =  *on
068800050721     c                   seton                                        53  99
068900050721     c                   eval      V1Dmsg =  $Msg(07)
069000050721     c                   leavesr
069100050721x   2c                   else
069200050721     c                   z-add     G02dat        V2Cvdf
069300050721     c                   z-add     G02inv        W2Cvdf
069400050721e   2c                   endif
069500050721e   1c                   endif
069600050721      *
069700160325      * -?Ora   Fine  blocco Volume VDL?
069800050721     c                   move      V2Cvhf        Wtime
069900050721if  1c                   if            Whh    <  *zeros
070000050721     c                             or  Whh    >  24
070100050721     c                             or  Wmm    <  *zeros
070200050721     c                             or  Wmm    >  59
070300050721     c                   seton                                        54  99
070400050721     c                   eval      V1Dmsg =  $Msg(08)
070500050721     c                   leavesr
070600050721e   1c                   endif
070700050721      *
070800160325      * -?Corretta sequenza data/ora_inizio <= data/ora_fine?
070900050721if  1c                   if        W2Cvdi * 10000 + V2Cvhi >
071000050721     c                             W2Cvdf * 10000 + V2Cvhf
071100160325     c                   seton                                        53  99
071200050721     c                   eval      V1Dmsg =  $Msg(09)
071300050721     c                   leavesr
071400050721e   1c                   endif
071500050721      *
071600160325      * -?Flag blocco Peso VDL => Ctrl eseguito nel dspf?
071700050721      *
071800160325      * -?Data Inizio blocco Peso VDL?
071900050721     c                   clear                   W2Cpdi
072000050721if  1c                   if            V2Cpbl =  'S'
072100050721     c                             and V2Cpdi =  *zeros
072200160325     c                   seton                                        55  99
072300050721     c                   eval      V1Dmsg =  $Msg(03)
072400050721     c                   leavesr
072500050721e   1c                   endif
072600050721if  1c                   if        V2Cpdi >  *zeros
072700050721     c                   clear                   WLBdat
072800050721     c                   z-add     V2Cpdi        G02dat
072900050721     c                   call      'XSRDA8'
073000050721     c                   parm                    WLBdat
073100050721if  2c                   if        G02err =  *on
073200160325     c                   seton                                        55  99
073300050721     c                   eval      V1Dmsg =  $Msg(04)
073400050721     c                   leavesr
073500050721x   2c                   else
073600050721     c                   z-add     G02dat        V2Cpdi
073700050721     c                   z-add     G02inv        W2Cpdi
073800050721e   2c                   endif
073900050721e   1c                   endif
074000050721      *
074100160325      * -?Ora  Inizio blocco Peso VDL?
074200050721     c                   move      V2Cphi        Wtime
074300050721if  1c                   if            Whh    <  *zeros
074400050721     c                             or  Whh    >  24
074500050721     c                             or  Wmm    <  *zeros
074600050721     c                             or  Wmm    >  59
074700050721     c                   seton                                        56  99
074800050721     c                   movel     $Msg(05)      V1Dmsg
074900050721     c                   leavesr
075000050721e   1c                   endif
075100050721      *
075200160325      * -?Data  Fine  blocco Peso VDL?
075300050721     c                   clear                   W2Cpdf
075400050721if  1c                   if            V2Cpbl =  'S'
075500050721     c                             and V2Cpdf =  *zeros
075600160325     c                   seton                                        57  99
075700050721     c                   eval      V1Dmsg =  $Msg(06)
075800050721     c                   leavesr
075900050721e   1c                   endif
076000050721if  1c                   if        V2Cpdf >  *zeros
076100050721     c                   clear                   WLBdat
076200050721     c                   z-add     V2Cpdf        G02dat
076300050721     c                   call      'XSRDA8'
076400050721     c                   parm                    WLBdat
076500050721if  2c                   if        G02err =  *on
076600160325     c                   seton                                        57  99
076700050721     c                   eval      V1Dmsg =  $Msg(07)
076800050721     c                   leavesr
076900050721x   2c                   else
077000050721     c                   z-add     G02dat        V2Cpdf
077100050721     c                   z-add     G02inv        W2Cpdf
077200050721e   2c                   endif
077300050721e   1c                   endif
077400050721      *
077500160325      * -?Ora   Fine  blocco Peso VDL?
077600050721     c                   move      V2Cphf        Wtime
077700050721if  1c                   if            Whh    <  *zeros
077800050721     c                             or  Whh    >  24
077900050721     c                             or  Wmm    <  *zeros
078000050721     c                             or  Wmm    >  59
078100050721     c                   seton                                        58  99
078200050721     c                   movel     $Msg(08)      V1Dmsg
078300050721     c                   leavesr
078400050721e   1c                   endif
078500050721      *
078600160325      * -?Corretta sequenza data/ora_inizio <= data/ora_fine?
078700050721if  1c                   if        W2Cpdi * 10000 + V2Cphi >
078800050721     c                             W2Cpdf * 10000 + V2Cphf
078900160325     c                   seton                                        57  99
079000050721     c                   eval      V1Dmsg =  $Msg(09)
079100050721     c                   leavesr
079200050721e   1c                   endif
079300160325      *
079400160325      * -?Data Inizio blocco Volume RPVP?
079500160325     c                   clear                   W2CvdiR
079600160325if  1c                   if            V2CvblR =  'S'
079700160325     c                             and V2CvdiR =  *zeros
079800160325     c                   seton                                        59  99
079900160325     c                   eval      V1Dmsg  =  $Msg(03)
080000160325     c                   leavesr
080100160325e   1c                   endif
080200160325if  1c                   if        V2CvdiR >  *zeros
080300160325     c                   clear                   WLBdat
080400160325     c                   z-add     V2CvdiR       G02dat
080500160325     c                   call      'XSRDA8'
080600160325     c                   parm                    WLBdat
080700160325if  2c                   if        G02err  =  *on
080800160325     c                   seton                                        59  99
080900160325     c                   eval      V1Dmsg  =  $Msg(04)
081000160325     c                   leavesr
081100160325x   2c                   else
081200160325     c                   z-add     G02dat        V2CvdiR
081300160325     c                   z-add     G02inv        W2CvdiR
081400160325e   2c                   endif
081500160325e   1c                   endif
081600160325      *
081700160325      * -?Ora  Inizio blocco Volume RPVP?
081800160325     c                   move      V2CvhiR       Wtime
081900160325if  1c                   if            Whh    <  *zeros
082000160325     c                             or  Whh    >  24
082100160325     c                             or  Wmm    <  *zeros
082200160325     c                             or  Wmm    >  59
082300160325     c                   seton                                        60  99
082400160325     c                   eval      V1Dmsg  =  $Msg(05)
082500160325     c                   leavesr
082600160325e   1c                   endif
082700160325      *
082800160325      * -?Data  Fine  blocco Volume RPVP?
082900160325     c                   clear                   W2CvdfR
083000160325if  1c                   if            V2CvblR =  'S'
083100160325     c                             and V2CvdfR =  *zeros
083200160325     c                   seton                                        61  99
083300160325     c                   eval      V1Dmsg  =  $Msg(06)
083400160325     c                   leavesr
083500160325e   1c                   endif
083600160325if  1c                   if        V2CvdfR >  *zeros
083700160325     c                   clear                   WLBdat
083800160325     c                   z-add     V2CvdfR       G02dat
083900160325     c                   call      'XSRDA8'
084000160325     c                   parm                    WLBdat
084100160325if  2c                   if        G02err  =  *on
084200160325     c                   seton                                        61  99
084300160325     c                   eval      V1Dmsg  =  $Msg(07)
084400160325     c                   leavesr
084500160325x   2c                   else
084600160325     c                   z-add     G02dat        V2CvdfR
084700160325     c                   z-add     G02inv        W2CvdfR
084800160325e   2c                   endif
084900160325e   1c                   endif
085000160325      *
085100160325      * -?Ora   Fine  blocco Volume RPVP?
085200160325     c                   move      V2CvhfR       Wtime
085300160325if  1c                   if            Whh    <  *zeros
085400160325     c                             or  Whh    >  24
085500160325     c                             or  Wmm    <  *zeros
085600160325     c                             or  Wmm    >  59
085700160325     c                   seton                                        62  99
085800160325     c                   eval      V1Dmsg  =  $Msg(08)
085900160325     c                   leavesr
086000160325e   1c                   endif
086100160325      *
086200160325      * -?Corretta sequenza data/ora_inizio <= data/ora_fine?
086300160325if  1c                   if        W2CvdiR * 10000 + V2CvhiR >
086400160325     c                             W2CvdfR * 10000 + V2CvhfR
086500160325     c                   seton                                        61  99
086600160325     c                   eval      V1Dmsg  =  $Msg(09)
086700160325     c                   leavesr
086800160325e   1c                   endif
086900160325      *
087000160325      * -?Flag blocco Peso RPVP => Ctrl eseguito nel dspf?
087100160325      *
087200160325      * -?Data Inizio blocco Peso RPVP?
087300160325     c                   clear                   W2CpdiR
087400160325if  1c                   if            V2CpblR =  'S'
087500160325     c                             and V2CpdiR =  *zeros
087600160325     c                   seton                                        63  99
087700160325     c                   eval      V1Dmsg  =  $Msg(03)
087800160325     c                   leavesr
087900160325e   1c                   endif
088000160325if  1c                   if        V2CpdiR >  *zeros
088100160325     c                   clear                   WLBdat
088200160325     c                   z-add     V2CpdiR       G02dat
088300160325     c                   call      'XSRDA8'
088400160325     c                   parm                    WLBdat
088500160325if  2c                   if        G02err  =  *on
088600160325     c                   seton                                        63  99
088700160325     c                   eval      V1Dmsg  =  $Msg(04)
088800160325     c                   leavesr
088900160325x   2c                   else
089000160325     c                   z-add     G02dat        V2CpdiR
089100160325     c                   z-add     G02inv        W2CpdiR
089200160325e   2c                   endif
089300160325e   1c                   endif
089400160325      *
089500160325      * -?Ora  Inizio blocco Peso RPVP?
089600160325     c                   move      V2CphiR       Wtime
089700160325if  1c                   if            Whh    <  *zeros
089800160325     c                             or  Whh    >  24
089900160325     c                             or  Wmm    <  *zeros
090000160325     c                             or  Wmm    >  59
090100160325     c                   seton                                        64  99
090200160325     c                   movel     $Msg(05)      V1Dmsg
090300160325     c                   leavesr
090400160325e   1c                   endif
090500160325      *
090600160325      * -?Data  Fine  blocco Peso RPVP?
090700160325     c                   clear                   W2CpdfR
090800160325if  1c                   if            V2CpblR =  'S'
090900160325     c                             and V2CpdfR =  *zeros
091000160325     c                   seton                                        65  99
091100160325     c                   eval      V1Dmsg  =  $Msg(06)
091200160325     c                   leavesr
091300160325e   1c                   endif
091400160325if  1c                   if        V2CpdfR >  *zeros
091500160325     c                   clear                   WLBdat
091600160325     c                   z-add     V2CpdfR       G02dat
091700160325     c                   call      'XSRDA8'
091800160325     c                   parm                    WLBdat
091900160325if  2c                   if        G02err  =  *on
092000160325     c                   seton                                        65  99
092100160325     c                   eval      V1Dmsg  =  $Msg(07)
092200160325     c                   leavesr
092300160325x   2c                   else
092400160325     c                   z-add     G02dat        V2CpdfR
092500160325     c                   z-add     G02inv        W2CpdfR
092600160325e   2c                   endif
092700160325e   1c                   endif
092800160325      *
092900160325      * -?Ora   Fine  blocco Peso RPVP?
093000160325     c                   move      V2CphfR       Wtime
093100160325if  1c                   if            Whh    <  *zeros
093200160325     c                             or  Whh    >  24
093300160325     c                             or  Wmm    <  *zeros
093400160325     c                             or  Wmm    >  59
093500160325     c                   seton                                        66  99
093600160325     c                   movel     $Msg(08)      V1Dmsg
093700160325     c                   leavesr
093800160325e   1c                   endif
093900160325      *
094000160325      * -?Corretta sequenza data/ora_inizio <= data/ora_fine?
094100160325if  1c                   if        W2CpdiR * 10000 + V2CphiR >
094200160325     c                             W2CpdfR * 10000 + V2CphfR
094300160325     c                   seton                                        65  99
094400160325     c                   eval      V1Dmsg  =  $Msg(09)
094500160325     c                   leavesr
094600160325e   1c                   endif
094700160325      *
094800160325      * -?Flag blocco Peso TRANSPALLET => Ctrl eseguito nel dspf?
094900160325      *
095000160325      * -?Data Inizio blocco Peso TRANSPALLET?
095100160325     c                   clear                   W2CpdiT
095200160325if  1c                   if            V2CpblT =  'S'
095300160325     c                             and V2CpdiT =  *zeros
095400160325     c                   seton                                        67  99
095500160325     c                   eval      V1Dmsg  =  $Msg(03)
095600160325     c                   leavesr
095700160325e   1c                   endif
095800160325if  1c                   if        V2CpdiT >  *zeros
095900160325     c                   clear                   WLBdat
096000160325     c                   z-add     V2CpdiT       G02dat
096100160325     c                   call      'XSRDA8'
096200160325     c                   parm                    WLBdat
096300160325if  2c                   if        G02err  =  *on
096400160325     c                   seton                                        67  99
096500160325     c                   eval      V1Dmsg  =  $Msg(04)
096600160325     c                   leavesr
096700160325x   2c                   else
096800160325     c                   z-add     G02dat        V2CpdiT
096900160325     c                   z-add     G02inv        W2CpdiT
097000160325e   2c                   endif
097100160325e   1c                   endif
097200160325      *
097300160325      * -?Ora  Inizio blocco Peso TRANSPALLET?
097400160325     c                   move      V2CphiT       Wtime
097500160325if  1c                   if            Whh    <  *zeros
097600160325     c                             or  Whh    >  24
097700160325     c                             or  Wmm    <  *zeros
097800160325     c                             or  Wmm    >  59
097900160325     c                   seton                                        68  99
098000160325     c                   movel     $Msg(05)      V1Dmsg
098100160325     c                   leavesr
098200160325e   1c                   endif
098300160325      *
098400160325      * -?Data  Fine  blocco Peso TRANSPALLET?
098500160325     c                   clear                   W2CpdfT
098600160325if  1c                   if            V2CpblT =  'S'
098700160325     c                             and V2CpdfT =  *zeros
098800160325     c                   seton                                        69  99
098900160325     c                   eval      V1Dmsg  =  $Msg(06)
099000160325     c                   leavesr
099100160325e   1c                   endif
099200160325if  1c                   if        V2CpdfT >  *zeros
099300160325     c                   clear                   WLBdat
099400160325     c                   z-add     V2CpdfT       G02dat
099500160325     c                   call      'XSRDA8'
099600160325     c                   parm                    WLBdat
099700160325if  2c                   if        G02err  =  *on
099800160325     c                   seton                                        69  99
099900160325     c                   eval      V1Dmsg  =  $Msg(07)
100000160325     c                   leavesr
100100160325x   2c                   else
100200160325     c                   z-add     G02dat        V2CpdfT
100300160325     c                   z-add     G02inv        W2CpdfT
100400160325e   2c                   endif
100500160325e   1c                   endif
100600160325      *
100700160325      * -?Ora   Fine  blocco Peso TRANSPALLET?
100800160325     c                   move      V2CphfT       Wtime
100900160325if  1c                   if            Whh    <  *zeros
101000160325     c                             or  Whh    >  24
101100160325     c                             or  Wmm    <  *zeros
101200160325     c                             or  Wmm    >  59
101300160325     c                   seton                                        70  99
101400160325     c                   movel     $Msg(08)      V1Dmsg
101500160325     c                   leavesr
101600160325e   1c                   endif
101700160325      *
101800160325      * -?Corretta sequenza data/ora_inizio <= data/ora_fine?
101900160325if  1c                   if        W2CpdiT * 10000 + V2CphiT >
102000160325     c                             W2CpdfT * 10000 + V2CphfT
102100160325     c                   seton                                        69  99
102200160325     c                   eval      V1Dmsg  =  $Msg(09)
102300160325     c                   leavesr
102400160325e   1c                   endif
102500050721      *
102600050721     c     EndCtrV2      ENDSR
102700050721
102800050721      *---------------------------------------------------------------*
102900050721      * GESW1  - Gestione videata dati relativi alla trasmissione     *
103000050721      *---------------------------------------------------------------*
103100050721     c     GesW1         BEGSR
103200050721      *
103300050721      * Inizializzazione videata
103400050721if  1c                   if        $CarW1 = *on
103500050721     c                   exsr      CarW1
103600050721     c                   movel     *off          $CarW1
103700050721e   1c                   endif
103800050721      *
103900050721if  1c                   if        *in05
104000050721     c                   write     TB37W1
104100050721     c                   exfmt     PROTECT
104200050721x   1c                   else
104300050721     c                   exfmt     TB37W1
104400050721e   1c                   endif
104500050721     c                   eval      *in99 = *off
104600050721     c                   clear                   W1MSG
104700050721      *
104800050721sel 1c                   select
104900050721      * F12=Ritorno
105000050721w   1c                   when      *inKL
105100050721     c                   exsr      F12W1
105200050721     c                   goto      EndGesW1
105300050721e   1c                   endsl
105400050721      *
105500050721      * Controllo dati immessi a video
105600050721     c                   exsr      CtrW1
105700050721      *
105800050721      * Aggiornamento se non ci sono errori
105900050721if  1c                   if        not *in99 and *inKF
106000050721     c                   exsr      AggTBE
106100050721e   1c                   endif
106200050721      *
106300050721     c     EndGesW1      ENDSR
106400050721
106500050721      *---------------------------------------------------------------*
106600050721      * CARW1  - Caricamento dati window                              *
106700050721      *---------------------------------------------------------------*
106800050721     c     CarW1         BEGSR
106900050721      *
107000050721     c                   movea     *zeros        *in(50)
107100050721      *
107200050721sel 1c                   select
107300050721      *
107400050721      * F5=Ripristino
107500050721w   1c                   when      *inKE   and  *in04
107600050721     c                   eval      W1ftt = TBEftt
107700050721      *
107800050721      * F6=Conferma
107900050721w   1c                   when      *inKF
108000050721sel 2c                   select
108100050721      *   Immissione
108200050721w   2c                   when      *in01
108300050721     c                   eval      W1ftt = TBXftt
108400050721      *   Modifica / Ripristino
108500050721w   2c                   when      *in02   or    *in04
108600050721     c                   eval      W1ftt = TBEftt
108700050721e   2c                   endsl
108800050721      *
108900050721      * F16=Annullamento
109000050721w   1c                   when      *inKQ   and  not *in04
109100050721     c                   eval      W1ftt = TBEftt
109200050721      *
109300050721e   1c                   endsl
109400050721      *
109500050721      * Se NON immissione: visualizzo i dati relativi all'ultima
109600050721      *   trasmissione
109700050721if  1c                   if        not *in01
109800050721     c                   eval      W1flt = TBEflt
109900050721     c                   eval      W1ftr = TBEftr
110000050721if  2c                   if        TBEdtr <> 0
110100050721     c                   reset                   WLBdat
110200050721     c                   z-add     TBEdtr        G02inv
110300050721     c                   call      'XSRDA8'
110400050721     c                   parm                    WLBdat
110500050721     c                   z-add     G02dat        W1dtr
110600050721e   2c                   endif
110700050721e   1c                   endif
110800050721      *
110900050721     c                   ENDSR
111000050721
111100050721      *---------------------------------------------------------------*
111200050721      * CTRW1  - Controllo e decodifica window                        *
111300050721      *---------------------------------------------------------------*
111400050721     c     CtrW1         BEGSR
111500050721      *
111600050721     c                   movea     *zeros        *in(50)
111700050721      *
111800050721     c     EndCtrW1      ENDSR
111900050721
112000050721      *---------------------------------------------------------------*
112100050721      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
112200050721      *---------------------------------------------------------------*
112300050721     c     F12W1         BEGSR
112400050721      *
112500050721     c                   eval      $TipVid = '2'
112600050721      *
112700050721     c                   ENDSR
112800050721
112900050721      *---------------------------------------------------------------*
113000050721      * CHNTBE * Aggancio tabella                                     *
113100050721      *---------------------------------------------------------------*
113200050721     c     ChnTBE        BEGSR
113300050721      *
113400050721     c                   movel     T02cod        TBEcod
113500050721     c                   movel(p)  V1Cfgs        TBEke1
113600050721     c                   clear                   TBEke2
113700050721     c                   clear                   TBElin
113800050721     c                   movel     KNSIF         TBEsif
113900050721     c     K05TBE01      chain     TNTBE01L
114000050721      * Se non ho trovato il record con il sistema informativo
114100050721      * che ho in linea, lo abblenco
114200050721if  1c                   if        not %found(TNTBE01L)
114300050721     c                   clear                   TBEsif
114400050721     c     K05TBE01      chain     TNTBE01L
114500050721e   1c                   endif
114600050721      *
114700050721     c                   ENDSR
114800050721
114900050721      *---------------------------------------------------------------*
115000050721      * AGGTBE * Aggiornamento tabella                                *
115100050721      *---------------------------------------------------------------*
115200050721     c     AggTBE        BEGSR
115300050721      *
115400050721sel 1c                   SELECT
115500050721      *
115600050721      * F5=Ripristino
115700050721w   1c                   WHEN      wTasto='05'  and  *in04
115800050721     c                   clear                   TBEatb
115900050721     c                   clear                   TBEftr
116000050721     c                   UPDATE    TNTBE000
116100050721      *
116200050721      * F6=Conferma
116300050721w   1c                   WHEN      wTasto='06'
116400050721     c                   exsr      RieTBE
116500050721sel 2c                   select
116600050721      *   Immissione
116700050721w   2c                   when      *in01
116800050721     c                   clear                   TBEflt
116900050721     c                   clear                   TBEdtr
117000050721     c                   WRITE     TNTBE000                             22
117100050721      *   Modifica / Ripristino
117200050721w   2c                   when      *in02   or    *in04
117300050721     c                   UPDATE    TNTBE000
117400050721e   2c                   endsl
117500050721      *
117600050721      * F16=Annullamento
117700050721w   1c                   WHEN      wTasto='16'  and  not *in04
117800050721     c                   movel     'A'           TBEatb
117900050721     c                   clear                   TBEftr
118000050721     c                   UPDATE    TNTBE000
118100050721      *
118200050721e   1c                   ENDSL
118300050721      *
118400050721      * Torno alla prima videata che carico come da inizio pgm
118500050721     c                   movel     '1'           $TipVid
118600050721     c                   movel     *on           $CarV1
118700050721     c                   movel     *on           $CarV2
118800050721     c
118900050721     c                   ENDSR
119000050721
119100050721      *---------------------------------------------------------------*
119200050721      * RIETBE * Riempimento dati tabella                             *
119300050721      *---------------------------------------------------------------*
119400050721     c     RieTBE        BEGSR
119500050721      *
119600050721     c                   clear                   TBEatb
119700050721     c                   if        TBXsif <> *blanks
119800050721     c                   movel     KNSIF         TBEsif
119900050721     c                   else
120000050721     c                   clear                   TBEsif
120100050721     c                   endif
120200050721     c                   movel     TBXapl        TBEapl
120300050721     c                   movel     T02cod        TBEcod
120400050721     c                   movel(p)  V1Cfgs        TBEke1
120500050721     c***                clear                   TBEke2
120600050721      *
120700050721     c                   clear                   dMVP
120800160325      *
120900160325      * -?Blocco Volume VDL?
121000050721     c                   eval      §MVPvbl = V2Cvbl
121100050721if  1c                   if        V2Cvdi  > *zeros
121200050721     c                   clear                   WLBdat
121300050721     c                   eval      G02dat  = V2Cvdi
121400050721     c                   call      'XSRDA8'
121500050721     c                   parm                    WLBdat
121600050721     c                   eval      §MVPvdi = G02inv
121700050721e   1c                   endif
121800050721     c                   eval      §MVPvhi = V2Cvhi
121900050721if  1c                   if        V2Cvdf  > *zeros
122000050721     c                   clear                   WLBdat
122100050721     c                   eval      G02dat  = V2Cvdf
122200050721     c                   call      'XSRDA8'
122300050721     c                   parm                    WLBdat
122400050721     c                   eval      §MVPvdf = G02inv
122500050721e   1c                   endif
122600050721     c                   eval      §MVPvhf = V2Cvhf
122700160325      * -?Blocco Peso VDL?
122800050721     c                   eval      §MVPpbl = V2Cpbl
122900050721if  1c                   if        V2Cpdi  > *zeros
123000050721     c                   clear                   WLBdat
123100050721     c                   eval      G02dat  = V2Cpdi
123200050721     c                   call      'XSRDA8'
123300050721     c                   parm                    WLBdat
123400050721     c                   eval      §MVPpdi = G02inv
123500050721e   1c                   endif
123600050721     c                   eval      §MVPphi = V2Cphi
123700050721if  1c                   if        V2Cpdf  > *zeros
123800050721     c                   clear                   WLBdat
123900050721     c                   eval      G02dat  = V2Cpdf
124000050721     c                   call      'XSRDA8'
124100050721     c                   parm                    WLBdat
124200050721     c                   eval      §MVPpdf = G02inv
124300050721e   1c                   endif
124400050721     c                   eval      §MVPphf = V2Cphf
124500160325      *
124600160325      * -?Blocco Volume RPVP?
124700160325     c                   eval      §MVPvblR = V2CvblR
124800160325     c                   eval      §MVPvdiR = W2CvdiR
124900160325     c                   eval      §MVPvhiR = V2CvhiR
125000160325     c                   eval      §MVPvdfR = W2CvdfR
125100160325     c                   eval      §MVPvhfR = V2CvhfR
125200160325      * -?Blocco Peso RPVP?
125300160325     c                   eval      §MVPpblR = V2CpblR
125400160325     c                   eval      §MVPpdiR = W2CpdiR
125500160325     c                   eval      §MVPphiR = V2CphiR
125600160325     c                   eval      §MVPpdfR = W2CpdfR
125700160325     c                   eval      §MVPphfR = V2CphfR
125800160325      *
125900160325      * -?Blocco Peso TRANSPALLET?
126000160325     c                   eval      §MVPpblT = V2CpblT
126100160325     c                   eval      §MVPpdiT = W2CpdiT
126200160325     c                   eval      §MVPphiT = V2CphiT
126300160325     c                   eval      §MVPpdfT = W2CpdfT
126400160325     c                   eval      §MVPphfT = V2CphfT
126500160325      *
126600050721     c                   movel(p)  dMVP          TBEuni
126700050721      *
126800050721     c                   movel     W1ftt         TBEftt
126900050721     c                   z-add     TBXflt        TBEflt
127000050721     c                   clear                   TBEftr
127100050721      *
127200050721     c                   ENDSR
127300050721
127400050721**   -$OPZ-   *
127500050721  Inserimento
127600050721    Modifica
127700050721     Copia
127800050721  Annullamento
127900050721Visualizzazione
128000050721   ANNULLATO
128100050721**   -$MSG-                                                                  *
128200050721Immettere il Punto Operativo                                                   01
128300050721Punto operativo inesistente o annullato                                        02
128400050721Immettete la data di inizio blocco                                             03
128500050721Data inizio blocco errata                                                      04
128600050721Ora inizio blocco errata                                                       05
128700050721Immettete la data di fine blocco                                               06
128800050721Data fine blocco errata                                                        07
128900050721Ora fine blocco errata                                                         08
129000050721Data / Ora fine blocco   NON può precedere   Data / Ora inizio blocco          09
