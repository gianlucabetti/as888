000100040107      *---------------------------------------------------------------*
000200040610      * MANUTENZIONE FILE  NOTE  DI  RICHIESTE ASS.CLIENTI            *
000300040107      *---------------------------------------------------------------*
000400040107
000500040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040107
000700040107      *---------------------------------------------------------------*
000800040107
000900040213     fFITGN01L  Uf A e           k disk
001000040112      *
001100040112     fFIDNB0D   cf   e             workstn sfile(FIB0S01:S01nrr)
001200040112     f                                     infds(dspINFOds)
001300130403     f                                     usropn
001400040107
001500040107      *---------------------------------------------------------------*
001600040107
001700040107      *
001800040107      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
001900040107      *
002000040108     d K_tit1          c                   const('  IMMISSIONE  ')
002100040108     d K_tit2          c                   const(' MANUTENZIONE ')
002200040108     d K_tit3          c                   const('INTERROGAZIONE')
002300040109      * - nr. di righe del sfl (SFLPAG)
002400040109     d K_SflPag        c                   const(17)
002500040109      * - nr. riga del 1° rec. del sfl
002600040109     d K_SflNrr1       c                   const(6)
002700040120      * - tasto "Invio"
002800040120     d K_Enter         c                   const(X'F1')
002900040107      *
003000040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003100040107      *
003200040107      *
003300040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003400040107      *
003500040107     d KPJBA         e ds
003600040108      * - Parametri
003700040107     d FIDNB0DS      e ds
003800040213     d SaveB0ds      e ds                  extname(FIDNB0ds) inz
003900040213     d                                     prefix(X:1)
004000040108      *
004100040108      * - Parametri x Controllo profilo utenti
004200040213     d TIBS34DS      e ds
004300040108      * - Ds di riferimento al file esterno AZUTE00F
004400040108     d AZUTEDS       e ds                  extname(AZUTE00F)
004500040108      * - Ds per dati organigramma
004600040108     d DDatiUte      e ds
004700040123      *
004800040123     d FITGNds       e ds                  extname(FITGN00F) inz
004900040130      * - Salvataggio dati dei 1' 4 records immessi o modificati
005000040130     d SAVrec1       e ds                  extname(FITGN00F) inz
005100040130     d                                     prefix(SR1:3)
005200040130     d SAVrec2       e ds                  extname(FITGN00F) inz
005300040130     d                                     prefix(SR2:3)
005400040130     d SAVrec3       e ds                  extname(FITGN00F) inz
005500040130     d                                     prefix(SR3:3)
005600040130     d SAVrec4       e ds                  extname(FITGN00F) inz
005700040130     d                                     prefix(SR4:3)
005800040109      *
005900040109     d Status         sds
006000040109     d  SDSpgm           *proc
006100040107      *
006200040120     d dspINFOds       ds           512
006300040120     d  di_TASTO             369    369
006400040120     d  di_NRG               370    370
006500040120     d  di_SFLnrr            378    379B 0
006600040107      *
006700040107     d Cursor          ds
006800040107     d  RIRI                   1      2B 0
006900040107     d  R$$                    2      2
007000040107     d  COCO                   3      4B 0
007100040107     D  C$$                    4      4
007200040109      *
007300040109     d WLBdat          ds                  inz
007400040109     d  G02dat                 1      8  0 inz
007500040109     d  G02inv                 9     16  0 inz
007600040109     d  G02err                17     17    inz('3')
007700040109     d  G02tgi                18     22  0 inz
007800040112      *
007900040112     d dsSFLrec        ds
008000040123     d  V1Hpot
008100040421     d  V1Hant
008200040123     d  V1Hnut
008300040123     d  V1Hpdt
008400040123     d  V1Htin
008500040112     d  V1Hpgn
008600040112     d  H1in35
008700040224     d  H1in36
008800040112     d  V1Odin
008900040112     d  V1Ooin
009000040112     d  V1Cnot
009100040112     d  V1Outn
009200040112     d  V1Opon
009300040112     d  V1Osts
009400040107      *
009500040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009600040107      *
009700040109      * - Campi di appoggio per il subfile
009800040123     d S01nrr          s                   like(V1Cnrr)   inz
009900040213     d wNrr1Free       s                   like(V1Cnrr)   inz
010000040213     d $Nrr            s                   like(V1Cpos)   inz
010100040213     d wNrr            s                   like(V1Cpos)   inz
010200040213     d wPag            s                   like(V1Cpos)   inz
010300040213     d wRig            s                   like(V1Cpos)   inz
010400040109      *
010500040109      * - Campi di salvataggio temporaneo dati
010600040123     d LastS01         s                   like(dsSFLrec) inz
010700040123     d SaveS01         s                   like(dsSFLrec) inz
010800040213     d SAVpgn          s                   like(TGNpgn)   inz
010900040219     d SAVflm          s                   like(IB0flm)   inz
011000040109      *
011100040109      * - Flags booleani
011200040108     d $Fine           s              1    inz(*off)
011300040108     d $InzS01         s              1    inz(*on)
011400040130     d $SavRec         s              1    inz(*on)
011500040213     d $PiuRec         s              1    inz(*off)
011600040213      *
011700040213      * - Indici di schiera / Contatori
011800040213     d xx              s              3  0 inz
011900040109      *
012000040109      * - Campi di comodo
012100040108     d w0140           s             14  0
012200040123     d wDataAMG        s              8  0
012300040123     d wDataGMA        s              8  0
012400040120     d wOra            s              6  0
012500081023      *
012600081023      * -Campi per il controllo di caratteri non validi
012700081023     d TxtInOut        S           2048
012800081023     d ElencoChar      S            256
012900081023     d TipoElenco      S              1
013000081023     d CharSost        S              1
013100081023     d UpperCase       S              1
013200081023     d ChkNull         S              1    Inz('1')
013300081023     d CharNull        S              1
013400081023     d Esito           S              1
013500081023
013600040108      *
013700040108      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
013800040108      *
013900040112      * - file FITGN01L
014000040421     c     K05TGN01      klist
014100040421     c                   kfld                    IB0ant
014200040108     c                   kfld                    IB0nut
014300040108     c                   kfld                    IB0pdt
014400040108     c                   kfld                    IB0tin
014500040108     c                   kfld                    IB0pgn
014600040421     c     K04TGN01      klist
014700040421     c                   kfld                    IB0ant
014800040112     c                   kfld                    IB0nut
014900040112     c                   kfld                    IB0pdt
015000040112     c                   kfld                    IB0tin
015100040421     c     K03TGN01      klist
015200040421     c                   kfld                    IB0ant
015300040108     c                   kfld                    IB0nut
015400040108     c                   kfld                    IB0pdt
015500040421     c     K02TGN01      klist
015600040421     c                   kfld                    IB0ant
015700040421     c                   kfld                    IB0nut
015800040107
015900040107      *===============================================================*
016000040107      *  RIEPILOGO INDICATORI                                         *
016100040107      *---------------------------------------------------------------*
016200040109      *  01    - Flag modalità DIVERSO da *blanks e da "V"            *
016300040224      *          (né "manutenzione" né "variazione")                  *
016400040224      *  05    - Flag modalità UGUALE  a  "I" ("interrogazione")      *
016500040224      *  09    - Abilita RolUp nel subfile                            *
016600040109      *  20    - SFLDSP    in C01                                     *
016700040109      * N21    - SFLCLR    in C01                                     *
016800040109      *  21    - SFLDSPCTL in C01                                     *
016900040109      *  23    - SFLEND    in C01                                     *
017000040109      *  25    - Roll Up da subfile                                   *
017100040109      *  28    - Errore generico dspf                                 *
017200040428      *  34    - NON visualizzazione data/ora immissione e stato note *
017300040109      *  35    - Protezione campo note                                *
017400040224      *  36    - Evidenzia note non lette                             *
017500040224      *  41    - Posizionamento cursore su campo note                 *
017600040109      *  90    - Errore                                               *
017700040107      *===============================================================*
017800040107
017900040107     c     *Entry        plist
018000040107     c                   parm                    KPJBA
018100040130     c                   parm                    FIDNB0ds
018200040123      *
018300040123      * Pulizia campi di output
018400040213     c                   clear                   OB0piu
018500040121     c                   clear                   OB0fxx
018600040121     c                   clear                   OB0err
018700040121     c                   clear                   OB0msg
018800040107      *
018900040108     c     IB0tla        cabeq     'C'           Fine
019000040120      *
019100040120      * Reperisco data corrente
019200040121      * (se conferma di dati precedentemente immessi:
019300040121      *  mantengo l'orario precedentemente memorizzato)
019400040123if  1c                   if        IB0flm <> 'C'
019500040120     c                   time                    w0140
019600040123     c                   move      w0140         wDataGMA
019700040120     c                   movel     w0140         wOra
019800040120     c                   clear                   WLBdat
019900040213     c                   eval      G02dat  = wDataGMA
020000040120     c                   call      'XSRDA8'
020100040120     c                   parm                    WLBdat
020200040123     c                   z-add     G02inv        wDataAMG
020300040121e   1c                   endif
020400040108      *
020500040224      * Nel caso di tipo elaboraz. RECUPERO o SCRITTURA o CANCELLAZIONE
020600040224      *   note: NON gestisco il subfile
020700040108sel 1c                   select
020800040224      *
020900040224      * - Recupero
021000040213w   1c                   when      IB0flm  = 'R'
021100040108     c                   exsr      sr_CHAIN
021200040224      *
021300040224      * - Aggiornamento/Scrittura
021400040213w   1c                   when      IB0flm  = 'W'
021500040108     c                   exsr      sr_UPDATE
021600040224      *
021700040224      * - Cancellazione
021800040224w   1c                   when      IB0flm  = 'D'
021900040224     c                   exsr      sr_DELETE
022000040224      *
022100040224      * - Conferma dati precedentemente gestiti nel sfl
022200040213w   1c                   when      IB0flm  = 'C'
022300040112     c                   exsr      Upd_FITGN
022400040224      *
022500040224      * - Manutenzione dati a video (in sfl)
022600040108x   1c                   other
022700040224     c                   reset                   $Fine
022800040213do  2c                   dow       $Fine   = *off
022900040109     c                   exsr      GesS01
023000040108e   2c                   enddo
023100040224      * - - Restituisco i dati elaborati al chiamante
023200040213if  2c                   if        $PiuRec = *on
023300040213     c                   eval      OB0piu  = '+'
023400040213e   2c                   endif
023500040213if  2c                   if            OB0fxx =  *blanks
023600040219     c                             and IB0flm <> *blanks
023700040219     c                             and IB0flm <> 'M'
023800040213     c                   eval      DB0no1  = SR1not
023900040213     c                   eval      DB0no2  = SR2not
024000040213     c                   eval      DB0no3  = SR3not
024100040213     c                   eval      DB0no4  = SR4not
024200040213     c                   eval      DB0din  = SR1din
024300040213     c                   eval      DB0oin  = SR1oin
024400040213     c                   eval      DB0utn  = SR1utn
024500040213     c                   eval      DB0pon  = SR1pon
024600040213     c                   eval      DB0sts  = SR1sts
024700040213e   2c                   endif
024800040224      *
024900040108e   1c                   endsl
025000040108      *
025100040108     c     Fine          tag
025200040123      *
025300040123if  1c                   if        IB0tla <> *blanks
025400040213     c                   eval      *inLR   = *on
025500130403     c                   if        %open(FIDNB0D)
025600130403     c                   close     FIDNB0D
025700130403     c                   endif
025800130403     c
025900040123x   1c                   else
026000040213     c                   eval      *inRT   = *on
026100040123e   1c                   endif
026200040108     c                   return
026300040108
026400040108      *---------------------------------------------------------------*
026500040108      * Routine Iniziale                                              *
026600040108      *---------------------------------------------------------------*
026700040108     c     *InzSR        BegSR
026800040108      *
026900040108      * Reperisco dati job
027000040108     c                   exsr      DatiJob
027100040108     c                   movel     SDSpgm        VTCpgm
027200040108      *
027300040108     c                   EndSr
027400040108
027500040108      *---------------------------------------------------------------*
027600040108      * Reperimento Dati del job (Utente/Operativi)                   *
027700040108      *---------------------------------------------------------------*
027800040108     c     DatiJob       BegSr
027900040108      *
028000040108     c     *dtaara       define    §azute        azuteds
028100040108     c     *dtaara       define    §datiute      ddatiute
028200040108      *
028300040108     c                   in(E)     *dtaara
028400040108     c                   IF        %ERROR or RSUT = *blanks
028500040108     c                   clear                   Tibs34Ds
028600040108     c                   call      'TIBS34R'
028700040108     c                   parm                    Tibs34Ds
028800040108     c                   in        *dtaara
028900040108     c                   ENDIF
029000040108      *
029100040108     c                   EndSr
029200040108
029300040108      *---------------------------------------------------------------*
029400040224      * sr_CHAIN   * Aggancio record(s) specifico/i                   *
029500040108      *---------------------------------------------------------------*
029600040108     c     sr_CHAIN      BEGSR
029700040108      *
029800040218if  1c                   if        IB0pgn > *zeros
029900040421     c     K05TGN01      chain(n)  FITGN000
030000040218x   1c                   else
030100040421     c     K04TGN01      chain(n)  FITGN000
030200040218e   1c                   endif
030300040108      *
030400040108if  1c                   if        NOT %found(FITGN01L)
030500040213      *
030600040108     c                   eval      OB0err = 'E'
030700040213      *
030800040108x   1c                   else
030900040213      *
031000040218     c                   eval      IB0pot = TGNpot
031100040130     c                   eval      DB0no1 = TGNnot
031200040130     c                   eval      DB0din = TGNdin
031300040130     c                   eval      DB0oin = TGNoin
031400040130     c                   eval      DB0utn = TGNutn
031500040130     c                   eval      DB0pon = TGNpon
031600040130     c                   eval      DB0sts = TGNsts
031700040213      *
031800040218do  2c     2             do        5             xx
031900040421     c     K04TGN01      reade(n)  FITGN000
032000040213if  3c                   if        %eof(FITGN01L)
032100040213     c                   leave
032200040213e   3c                   endif
032300040213sel 3c                   select
032400040213w   3c                   when      xx     = 2
032500040213     c                   eval      DB0no2 = TGNnot
032600040213w   3c                   when      xx     = 3
032700040213     c                   eval      DB0no3 = TGNnot
032800040213w   3c                   when      xx     = 4
032900040213     c                   eval      DB0no4 = TGNnot
033000040218w   3c                   when      xx     > 4
033100040218     c                   eval      OB0piu = '+'
033200040213e   3c                   endsl
033300040213e   2c                   enddo
033400040213      *
033500040108e   1c                   endif
033600040108      *
033700040108     c                   ENDSR
033800040108
033900040108      *---------------------------------------------------------------*
034000040224      * sr_UPDATE  * Aggiornamento record(s) specifico/i              *
034100040108      *---------------------------------------------------------------*
034200040108     c     sr_UPDATE     BEGSR
034300040108      *
034400040224     c                   eval      SAVpgn =  IB0pgn
034500040213      *
034600040224     c                   select
034700040224      * Reimpostazione del progressivo note
034800040224w   1c                   when      IB0pgn <= *zeros
034900040224     c                   clear                   IB0pgn
035000040213      * Reperimento del 1° progressivo note libero
035100040224w   1c                   when      IB0pgn =  *hival
035200040421     c     K04TGN01      setgt     FITGN000
035300040421     c     K04TGN01      readpe(n) FITGN000
035400040213if  2c                   if        %eof(FITGN01L)
035500040219     c                   clear                   IB0pgn
035600040213x   2c                   else
035700040219     c                   eval      IB0pgn = TGNpgn
035800040213e   2c                   endif
035900040224x   1c                   other
036000040224     c                   sub       1             IB0pgn
036100040224e   1c                   endsl
036200040213      *
036300040213      * Ciclo di registrazione delle note passate
036400040213do  1c                   do        4             xx
036500040213      *
036600040213if  2c                   if            xx     =  1
036700040213     c                             and DB0no1 <> *blanks
036800040213     c                             OR  xx     =  2
036900040213     c                             and DB0no2 <> *blanks
037000040213     c                             OR  xx     =  3
037100040213     c                             and DB0no3 <> *blanks
037200040213     c                             OR  xx     =  4
037300040213     c                             and DB0no4 <> *blanks
037400040213     c                   exsr      sr_Update1
037500040213e   2c                   endif
037600040213      *
037700040213e   1c                   enddo
037800040213     c                   eval      IB0pgn = SAVpgn
037900040408      *
038000040408      * - Re-imposto il flag per la inizializzazione del (nuovo) sfl
038100040408     c                   reset                   $InzS01
038200040408     c                   reset                   SaveB0ds
038300040219     c                   clear                   SAVpgn
038400040219     c                   clear                   SAVflm
038500040213      *
038600040213     c                   ENDSR
038700040213      *---------------------------------------------------------------*
038800040213     c     sr_Update1    BEGSR
038900040213      *
039000040220     c                   eval      IB0pgn  = IB0pgn + 1
039100040219      *
039200040421     c     K05TGN01      chain     FITGN000
039300040108      *
039400040108if  1c                   if        NOT %found(FITGN01L)
039500040108     c                   clear                   FITGN000
039600040421     c                   eval      TGNant  = IB0ant
039700040220     c                   eval      TGNnut  = IB0nut
039800040220     c                   eval      TGNpdt  = IB0pdt
039900040220     c                   eval      TGNtin  = IB0tin
040000040220     c                   eval      TGNpgn  = IB0pgn
040100040108e   1c                   endif
040200040108      *
040300040220     c                   eval      TGNpot  = IB0pot
040400040213sel 1c                   select
040500040220w   1c                   when      xx      = 1
040600040220     c                   eval      TGNnot  = DB0no1
040700040220w   1c                   when      xx      = 2
040800040220     c                   eval      TGNnot  = DB0no2
040900040220w   1c                   when      xx      = 3
041000040220     c                   eval      TGNnot  = DB0no3
041100040220w   1c                   when      xx      = 4
041200040220     c                   eval      TGNnot  = DB0no4
041300040213e   1c                   endsl
041400040220if  1c                   if        DB0din <> *zeros
041500040220     c                   eval      TGNdin  = DB0din
041600040220x   1c                   else
041700040220     c                   eval      TGNdin  = wDataAMG
041800040220e   1c                   endif
041900040220if  1c                   if        DB0oin <> *zeros
042000040220     c                   eval      TGNoin  = DB0oin
042100040220x   1c                   else
042200040220     c                   eval      TGNoin  = wOra
042300040220e   1c                   endif
042400040220if  1c                   if        DB0utn <> *blanks
042500040220     c                   eval      TGNutn  = DB0utn
042600040220x   1c                   else
042700040220     c                   eval      TGNutn  = KNMUS
042800040220e   1c                   endif
042900040220if  1c                   if        DB0pon <> *zeros
043000040220     c                   eval      TGNpon  = DB0pon
043100040220x   1c                   else
043200040220     c                   eval      TGNpon  = DUTpou
043300040220e   1c                   endif
043400040220     c                   eval      TGNsts  = DB0sts
043500081023
043600081023      * controllo il campo nota che non contenga caratteri particolari
043700081023
043800081023     c                   exsr      Sr_Chk
043900081023
044000040108if  1c                   if        NOT %found(FITGN01L)
044100040108     c                   WRITE     FITGN000
044200040108x   1c                   else
044300040108     c                   UPDATE    FITGN000
044400040108e   1c                   endif
044500040108      *
044600040108     c                   ENDSR
044700081023      *---------------------------------------------------------------*
044800081023      * sr_Chk     * Controllo campo nota                             *
044900081023      *---------------------------------------------------------------*
045000081023     c     sr_Chk        BEGSR
045100081023      *
045200040224
045300081023     c                   clear                   TxtInOut
045400081023     c                   eval      TxtInOut = TGNnot
045500081023     c                   Call      'XCHKCHAR'
045600081023     c                   Parm                    TxtInOut
045700081023     c                   Parm                    ElencoChar
045800081023     c                   Parm                    TipoElenco
045900081023     c                   Parm                    CharSost
046000081023     c                   Parm                    UpperCase
046100081023     c                   Parm                    ChkNull
046200081023     c                   Parm                    CharNull
046300081023     c                   Parm                    Esito
046400081023     c                   If        Esito = '1'
046500081023     c                   Movel     TxtInOut      TGNnot
046600081023     c                   EndIf
046700081023      *
046800081023     c                   ENDSR
046900040224      *---------------------------------------------------------------*
047000040224      * sr_DELETE  * Cancellazione record(s) specifico/i              *
047100040224      *---------------------------------------------------------------*
047200040224     c     sr_DELETE     BEGSR
047300040225      *
047400040225sel 2c                   select
047500040225w   2c                   when      IB0pgn <> *zeros
047600040421     c     K05TGN01      setll     FITGN000
047700040225w   2c                   when      IB0tin <> *blanks
047800040421     c     K04TGN01      setll     FITGN000
047900040225w   2c                   when      IB0pdt <> *zeros
048000040421     c     K03TGN01      setll     FITGN000
048100040225w   2c                   when      IB0nut <> *zeros
048200040421     c     K02TGN01      setll     FITGN000
048300040225e   2c                   endsl
048400040224      *
048500040224      * Ciclo di cancellazione record in base alla chiave passata
048600040224do  1c                   dou       NOT %found(FITGN01L)
048700040224      *
048800040224sel 2c                   select
048900040224w   2c                   when      IB0pgn <> *zeros
049000040421     c     K05TGN01      DELETE    FITGN000
049100040224w   2c                   when      IB0tin <> *blanks
049200040421     c     K04TGN01      DELETE    FITGN000
049300040224w   2c                   when      IB0pdt <> *zeros
049400040421     c     K03TGN01      DELETE    FITGN000
049500040224w   2c                   when      IB0nut <> *zeros
049600040421     c     K02TGN01      DELETE    FITGN000
049700040225x   2c                   other
049800040225     c                   leave
049900040224e   2c                   endsl
050000040224      *
050100040224e   1c                   enddo
050200040224      *
050300040408      * - Re-imposto il flag per la inizializzazione del (nuovo) sfl
050400040408     c                   reset                   $InzS01
050500040408     c                   reset                   SaveB0ds
050600040224     c                   clear                   SAVpgn
050700040224     c                   clear                   SAVflm
050800040224      *
050900040224     c                   ENDSR
051000040108
051100040108      *---------------------------------------------------------------*
051200040109      * GesS01     * Gestione dati a video                            *
051300040108      *---------------------------------------------------------------*
051400040109     c     GesS01        BEGSR
051500130403     c                   if        not %open(fidnb0d)
051600130403     c                   open      FIDNB0D
051700130403     c                   endif
051800040108      *
051900040213      * Controllo se devo inizializzare la videata
052000040213if  1c                   if            IB0tla <> XB0tla
052100040213     c                             or  IB0flm <> XB0flm
052200040421     c                             or  IB0ant <> XB0ant
052300040213     c                             or  IB0nut <> XB0nut
052400040213     c                             or  IB0pdt <> XB0pdt
052500040213     c                             or  IB0tin <> XB0tin
052600040213     c                             or  IB0pgn <> XB0pgn
052700040213     c                   reset                   $InzS01
052800040213     c                   movel     FIDNB0ds      SaveB0ds
052900040213e   1c                   endif
053000040213      *
053100040108      * Inizializzo la videata
053200040213sel 1c                   select
053300040213w   1c                   when      $InzS01 = *on
053400040108     c                   exsr      InzS01
053500040108     c                   eval      $InzS01 = *off
053600040219w   1c                   when          IB0flm  = *blanks
053700040219     c                             or  IB0flm  = 'M'
053800040213     c                   exsr      UpdS01_4
053900040213e   1c                   endsl
054000040108      *
054100040108      * Scrivo la testata e la riga tasti funzionali abilitati
054200040108if  1c                   if        NOT *in90
054300040108     c                   write     FIB0T00
054400040108     c                   write     FIB0Z01
054500040108e   1c                   endif
054600040112      *
054700040112      * Emetto il messaggio in caso di assenza di record
054800040213     c                   if        S01nrr  = *zeros
054900040112     c                   write     FIB0D01
055000040112     c                   endif
055100040108      *
055200040108      * Posiziono il cursore nel sfl
055300160406     c                   if        V1Cpos > *zeros
055400040108     c                   z-add     V1Cpos        V1Cnrr
055500160406     c                   endif
055600040108      *
055700040108      * Emetto la videata
055800040428     c*** sempre *off:   eval      *in34 = (IB0flm <> 'I')
055900040108     c                   exfmt     FIB0C01
056000040108     c                   setoff                                       28  90
056100040108     c                   clear                   V1Cmsg
056200040108      *
056300040108sel 1c                   select
056400040108      * F3=Fine
056500040108w   1c                   when      *inKC
056600040109     c                   eval      $Fine   = *on
056700040422     c                   eval      $InzS01 = *on
056800040109     c                   eval      OB0fxx  = '03'
056900040109     c                   goto      EndGesS01
057000040108      * F12=Ritorno
057100040108w   1c                   when      *inKL
057200040109     c                   eval      $Fine   = *on
057300040422     c                   eval      $InzS01 = *on
057400040109     c                   eval      OB0fxx  = '12'
057500040109     c                   goto      EndGesS01
057600040109      * RollUp
057700040109w   1c                   when      *in25
057800040123      * - Caricamento righe vuote (se NON interrogazione e
057900040123      *                            se passata chiave completa)
058000040123if  2c                   if            IB0flm <> 'I'
058100040421     c                             and IB0ant <> *zeros
058200040123     c                             and IB0nut <> *zeros
058300040123     c                             and IB0pdt <> *zeros
058400040213     c*****?????                   and IB0tin <> *blanks
058500040120     c                   eval      V1Cpos  = di_SFLnrr
058600040109     c                   exsr      sr_FreeRow
058700040123e   2c                   endif
058800040109      *
058900040109e   1c                   endsl
059000040109      *
059100040109      * F14=Cancella riga
059200040109if  1c                   if            *inKN
059300040219     c                             and (IB0flm = 'X'
059400040109     c                              or  IB0flm = 'V')
059500040109     c                   exsr      DltS01rec
059600040109e   1c                   endif
059700040108      *
059800040108      * F6=Aggiornamento
059900040109if  1c                   if            *inKF
060000040219     c                             and (IB0flm = 'X'
060100040109     c                              or  IB0flm = 'V')
060200040108     c                   exsr      Upd_FITGN
060300040224     c*****              eval      OB0fxx  = '06'
060400040109     c                   eval      $Fine   = *on
060500040108e   1c                   endif
060600040120      *
060700040219      * Enter=Prosegui (se manutenzione/immissione - Modalità = "_/M")
060800040219if  1c                   if           (IB0flm   = *blanks
060900040219     c                             or  IB0flm   = 'M')
061000040130     c                             and di_TASTO = K_Enter
061100040213     c                   exsr      RepS01_4
061200040120     c                   eval      $Fine   = *on
061300040120e   1c                   endif
061400040108      *
061500040109     c     EndGesS01     ENDSR
061600040108
061700040108      *---------------------------------------------------------------*
061800040109      * Inizializzazione subfile S01                                  *
061900040108      *---------------------------------------------------------------*
062000040108     c     InzS01        BEGSR
062100040108      *
062200040108      * Preparazione dati di "testata"
062300040108sel 1c                   select
062400040108w   1c                   when      IB0flm = 'M'
062500040108     c                   movel     K_tit1        VTCmod
062600040219w   1c                   when      IB0flm = 'V'  or
062700040219     c                             IB0flm = *blanks
062800040108     c                   movel     K_tit2        VTCmod
062900040108w   1c                   when      IB0flm = 'I'
063000040108     c                   movel     K_tit3        VTCmod
063100040108e   1c                   endsl
063200040108      *
063300040224     c                   eval      *in01  = (IB0flm <> 'X'  and
063400040224     c                                       IB0flm <> 'V')
063500040224     c                   eval      *in05  = (IB0flm =  'I')
063600040108      *
063700040108      * Visualizzazione del reclamo in esame
063800040218     c                   eval      VTCpot = IB0pot
063900040421     c                   eval      VTCant = IB0ant
064000040108     c                   eval      VTCnut = IB0nut
064100040212     c                   eval      VTCpdt = IB0pdt
064200040108     c                   reset                   WLBdat
064300040218     c                   eval      G02inv = IB0dar
064400040108     c                   call      'XSRDA8'
064500040108     c                   parm                    WLBdat
064600040217     c                   eval      VTCdta = G02dat
064700040421     c                   eval      VTCore = IB0orr / 100
064800040218     c                   eval      VTCogg = IB0ogg
064900040220     c*****              eval      VTCmad = IB0mad
065000040218     c                   eval      VTDmad = IB0dma
065100040108      *
065200040108      * Pulizia subfile
065300040108     c                   setoff                                       20  21
065400040108     c                   write     FIB0C01
065500040108     c                   seton                                        20  21
065600040108      *
065700040108     c                   clear                   RiRi
065800040108     c                   clear                   CoCo
065900040108     c                   clear                   V1Cnrr
066000040108     c                   clear                   V1Cpos
066100040108     c                   clear                   S01nrr
066200040112     c                   clear                   V1Cmsg
066300040108     c                   setoff                                       28  90
066400040224     c                   movea     *zeros        *in(41)
066500040213      *
066600040213     c                   reset                   $SavRec
066700040213     c                   reset                   $PiuRec
066800040213     c                   reset                   SAVrec1
066900040213     c                   reset                   SAVrec2
067000040213     c                   reset                   SAVrec3
067100040213     c                   reset                   SAVrec4
067200040213     c                   clear                   wNrr1Free
067300040213     c                   clear                   SAVpgn
067400040219      *
067500040219      * Memorizzazione ultimo tipo di caricamento dati
067600040219     c                   eval      SAVflm = IB0flm
067700040108      *
067800040108      * Caricamento dei dati da presentare nel sfl
067900040109     c                   exsr      RieS01
068000040108      *
068100040108     c                   ENDSR
068200040109
068300040109      *---------------------------------------------------------------*
068400040109      * Riempimento subfile S01                                       *
068500040109      *---------------------------------------------------------------*
068600040109     c     RieS01        BEGSR
068700040109      *
068800040109      * Caricamento dati da file note FITGN01L
068900040109      *
069000040109      * Posizionamento iniziale
069100040109sel 1c                   select
069200040109w   1c                   when      IB0pgn <> *zeros
069300040421     c     K05TGN01      setll     FITGN000
069400040421     c     K05TGN01      reade     FITGN000
069500040109w   1c                   when      IB0tin <> *blanks
069600040421     c     K04TGN01      setll     FITGN000
069700040421     c     K04TGN01      reade     FITGN000
069800040109w   1c                   when      IB0pdt <> *zeros
069900040421     c     K03TGN01      setll     FITGN000
070000040421     c     K03TGN01      reade     FITGN000
070100040109w   1c                   when      IB0nut <> *zeros
070200040421     c     K02TGN01      setll     FITGN000
070300040421     c     K02TGN01      reade     FITGN000
070400040421w   1c                   when      IB0ant <> *zeros
070500040421     c     IB0ant        setll     FITGN000
070600040421     c     IB0ant        reade     FITGN000
070700040109w   1c                   other
070800040129     c     *loval        setll     FITGN000
070900040129     c                   read      FITGN000
071000040109e   1c                   endsl
071100040109      *
071200040109      * Ciclo di lettura
071300040109do  1c                   dow       NOT %eof(FITGN01L)
071400040109      *
071500040109if  2c                   if        TGNatb =  *blanks
071600040109      *
071700040109     c                   clear                   FIB0S01
071800040109      *
071900040109      * - impostazione campi hidden
072000040109     c                   eval      V1Hpot =  TGNpot
072100040421     c                   eval      V1Hant =  TGNant
072200040109     c                   eval      V1Hnut =  TGNnut
072300040109     c                   eval      V1Hpdt =  TGNpdt
072400040109     c                   eval      V1Htin =  TGNtin
072500040112     c                   eval      V1Hpgn =  TGNpgn
072600040216     c                   eval      V1Hoin =  TGNoin
072700040109      *
072800040109      * - impostazione campi visualizzati
072900040219if  3c                   if             S01nrr <  4
073000040219     c                             and (DB0no1 <> *blanks
073100040219     c                              or  DB0no2 <> *blanks
073200040219     c                              or  DB0no3 <> *blanks
073300040219     c                              or  DB0no4 <> *blanks)
073400040219sel 4c                   select
073500040219w   4c                   when      S01nrr <  1
073600040219     c                   eval      V1Cnot =  DB0no1
073700040219w   4c                   when      S01nrr <  2
073800040219     c                   eval      V1Cnot =  DB0no2
073900040219w   4c                   when      S01nrr <  3
074000040219     c                   eval      V1Cnot =  DB0no3
074100040219w   4c                   when      S01nrr <  4
074200040219     c                   eval      V1Cnot =  DB0no4
074300040219e   4c                   endsl
074400040219x   3c                   else
074500040130     c                   eval      V1Cnot =  TGNnot
074600040219e   3c                   endif
074700040109     c                   reset                   WLBdat
074800040109     c                   eval      G02inv =  TGNdin
074900040109     c                   call      'XSRDA8'
075000040109     c                   parm                    WLBdat
075100040109     c                   eval      V1Odin =  G02dat
075200040109     c                   eval      V1Ooin =  TGNoin
075300040109     c                   eval      V1Outn =  TGNutn
075400040109     c                   eval      V1Opon =  TGNpon
075500040109if  3c                   if        TGNsts =  *blanks
075600040109     c                   eval      V1Osts =  'NUOVA '
075700040109x   3c                   else
075800040109     c                   eval      V1Osts =  'LETTA '
075900040109e   3c                   endif
076000040109      *
076100040109      * - impostazione indicatori
076200040427      *   - visualizzazione data e ora di immissione note
076300040428     c*** sempre off:    eval      *in34 = (IB0flm <> 'I')
076400040213      *   - protezione
076500040218if  3c                   if            *in05  =  *on
076600040218     c                             or  KNMUS  <> V1Outn
076700040218     c                             or  IB0pot <> V1Opon
076800040218     c                             or  IB0flm =  'M'
076900040218     c                             or  IB0flm =  'I'
077000040309     c                             or  IB0flm =  'V'
077100040309     c                             or  IB0flm =  *blanks
077200040109     c                   eval      *in35  =  *on
077300040218x   3c                   else
077400040218     c                   eval      *in35  =  *off
077500040218e   3c                   endif
077600040109     c                   eval      H1in35 =  *in35
077700040109      *   - alta intensità
077800040213if  3c                   if        TGNsts =  *blanks
077900040224     c                   eval      *in36  =  *on
078000040109x   3c                   else
078100040224     c                   eval      *in36  =  *off
078200040109e   3c                   endif
078300040224     c                   eval      H1in36 =  *in36
078400040225      *   - posizionamento cursore
078500040528      *     sul 1° campo "note" vuoto e non protetto
078600040528     c                   eval      *in41  =  (*in35  = *off)   and
078700040528     c                                       (V1Cnot = *blanks)
078800040109      *
078900040109      * - scrittura record nel sfl
079000040109     c                   add       1             S01nrr
079100040109     c                   WRITE     FIB0S01
079200040219      *
079300040219      *
079400040219if  3c                   IF        IB0flm <> *blanks
079500040219      * - agggiornamento del flag "letto" nel file
079600040219if  4c                   if        TGNsts =  *blanks
079700040219     c                   eval      TGNsts =  'L'
079800040219     c                   UPDATE    FITGN000
079900040219e   4c                   endif
080000040219x   3c                   ELSE
080100040219      * - cancellazione del record visualizzato nel subfile
080200040219      *   (se le note saranno poi confermate verranno poi re-inserite
080300040219      *    alla conferma)
080400040219     c                   DELETE    FITGN000
080500040219e   3c                   ENDIF
080600040213      *
080700040213      * - memorizzazione dei 1' 4 rec. da restituire al pgm. chiamante
080800040213if  3c                   if        $SavRec = *on
080900040213sel 4c                   select
081000040213w   4c                   when      SR1nut  = *zeros
081100040213     c                   movel     FITGNds       SAVrec1
081200040213w   4c                   when      SR2nut  = *zeros
081300040213     c                   movel     FITGNds       SAVrec2
081400040213w   4c                   when      SR3nut  = *zeros
081500040213     c                   movel     FITGNds       SAVrec3
081600040213w   4c                   when      SR4nut  = *zeros
081700040213     c                   movel     FITGNds       SAVrec4
081800040213     c                   eval      $SavRec = *off
081900040213e   4c                   endsl
082000040213x   3c                   else
082100040213     c                   eval      $PiuRec = *on
082200040213e   3c                   endif
082300040109      *
082400040109e   2c                   endif
082500040109      *
082600040109sel 2c                   select
082700040109w   2c                   when      IB0pgn <> *zeros
082800040421     c     K05TGN01      reade     FITGN000
082900040109w   2c                   when      IB0tin <> *blanks
083000040421     c     K04TGN01      reade     FITGN000
083100040109w   2c                   when      IB0pdt <> *zeros
083200040421     c     K03TGN01      reade     FITGN000
083300040109w   2c                   when      IB0nut <> *zeros
083400040421     c     K02TGN01      reade     FITGN000
083500040421w   2c                   when      IB0ant <> *zeros
083600040421     c     IB0ant        reade     FITGN000
083700040109w   2c                   other
083800040129     c                   read      FITGN000
083900040109e   2c                   endsl
084000040109      *
084100040109e   1c                   enddo
084200040213      *
084300040213      * Memorizzo l'ultimo n° progressivo note già registrato
084400040219      *  (se NON in variazione)
084500040219if  1c                   if        IB0flm <> *blanks
084600040213     c                   eval      SAVpgn     =  TGNpgn
084700040219e   1c                   endif
084800040213      *
084900040213      * Memorizzo il nrr del primo record vuoto (inseribile)
085000040213     c                   eval      wNrr1Free  =  S01nrr + 1
085100040109      *
085200040109      * Caricamento righe vuote (se NON interrogazione)
085300040112if  1c                   if            IB0flm <> 'I'
085400040421     c                             and IB0ant <> *zeros
085500040112     c                             and IB0nut <> *zeros
085600040112     c                             and IB0pdt <> *zeros
085700040213     c*****?????                   and IB0tin <> *blanks
085800040109     c                   eval      wNrr   =  S01nrr
085900040109     c                   exsr      sr_FreeRow
086000040109x   1c                   else
086100040109     c                   eval      *in09  =  (S01nrr > K_SflPag)
086200040109     c                   eval      *in23  =  *on
086300040109e   1c                   endif
086400040109      *
086500040109     c                   eval      *in20  =  (S01nrr > *zeros)
086600040109     c                   eval      V1Cpos =  1
086700040109      *
086800040109     c                   ENDSR
086900040213
087000040213      *---------------------------------------------------------------*
087100040213      * Aggiornamento prime 4 righe del subfile con le note ricevute  *
087200040213      *---------------------------------------------------------------*
087300040213     c     UpdS01_4      BEGSR
087400040213      *
087500040213     *** * Se è stato richiesto (forse) un aggiornamento...
087600040213if  1***c                   if             DB0no1 <> *blanks
087700040213     ***c                              or  DB0no2 <> *blanks
087800040213     ***c                              or  DB0no3 <> *blanks
087900040213     ***c                              or  DB0no4 <> *blanks
088000040213      *
088100040213do  2c                   DO        4             $Nrr
088200040213      *
088300040213     c     $Nrr          chain     FIB0S01
088400040213      *
088500040213if  3c                   if            %found(FIDNB0D)
088600040213     c                             and H1in35 = *off
088700040213      *
088800040213sel 4c                   select
088900040213w   4c                   when      $Nrr   =  1
089000040213     c                   eval      V1Cnot =  DB0no1
089100040213w   4c                   when      $Nrr   =  2
089200040213     c                   eval      V1Cnot =  DB0no2
089300040213w   4c                   when      $Nrr   =  3
089400040213     c                   eval      V1Cnot =  DB0no3
089500040213w   4c                   when      $Nrr   =  4
089600040213     c                   eval      V1Cnot =  DB0no4
089700040213e   4c                   endsl
089800040213      *
089900040213      * - impostazione indicatori
090000040427      *   - visualizzazione data e ora di immissione note
090100040428     c*** sempre *off:   eval      *in34 = (IB0flm <> 'I')
090200040213      *   - protezione
090300040213     c                   eval      *in35 = H1in35
090400040213      *   - alta intensità
090500040224     c                   eval      *in36 = H1in36
090600040528      *   - posizionamento cursore
090700040528      *     sul 1° campo "note" vuoto e non protetto
090800040528     c                   eval      *in41 = (*in35  = *off)   and
090900040528     c                                     (V1Cnot = *blanks)
091000040213      *
091100040213      * - aggiornamento record nel sfl
091200040213     c                   update    FIB0S01
091300040213      *
091400040213e   3c                   endif
091500040213      *
091600040213e   2c                   ENDDO
091700040213     *** *
091800040213e   1***c                   endif
091900040213      *
092000040213     c                   ENDSR
092100040213
092200040213      *---------------------------------------------------------------*
092300040213      * Reperimento prime 4 righe del subfile con le note             *
092400040213      *---------------------------------------------------------------*
092500040213     c     RepS01_4      BEGSR
092600040213      *
092700040218do  1c                   do        *hival        $Nrr
092800040213      *
092900040213     c     $Nrr          chain     FIB0S01
093000040213      *
093100040218if  2c                   if        NOT %found(FIDNB0D)
093200040218     c                   leave
093300040218e   2c                   endif
093400040213      *
093500040218sel 3c                   select
093600040218w   3c                   when      $Nrr    =  1
093700040213     c                   eval      DB0no1  = V1Cnot
093800040213     c                   eval      DB0din  = wDataAMG
093900040218if  4c                   if        V1Hoin  = *zeros
094000040213     c                   eval      DB0oin  = wOra
094100040218x   4c                   else
094200040216     c                   eval      DB0oin  = V1Hoin
094300040218e   3c                   endif
094400040213     c                   eval      DB0utn  = KNMUS
094500040219     c                   eval      DB0pon  = DUTpou
094600040213     c                   eval      DB0sts  = *blanks
094700040218w   2c                   when      $Nrr    =  2
094800040213     c                   eval      DB0no2  = V1Cnot
094900040218w   2c                   when      $Nrr    =  3
095000040213     c                   eval      DB0no3  = V1Cnot
095100040218w   2c                   when      $Nrr    =  4
095200040213     c                   eval      DB0no4  = V1Cnot
095300040220w   2c                   when      $Nrr    >  4     and
095400040220     c                             V1Cnot <> *blanks
095500040220     c                   eval      $PiuRec = *on
095600040218e   2c                   endsl
095700040213      *
095800040213      * - impostazione indicatori
095900040427      *   - visualizzazione data e ora di immissione note
096000040428     c*** sempre *off:   eval      *in34 = (IB0flm <> 'I')
096100040213      *   - protezione
096200040213     c                   eval      *in35   = H1in35
096300040213      *   - alta intensità
096400040224     c                   eval      *in36   = H1in36
096500040213      *
096600040213      * - aggiornamento record nel sfl
096700040213     c                   update    FIB0S01
096800040218      *
096900040220      * - se ho già rilevato più di 4 note: esco dal ciclo di lettura
097000040220if  2c                   if        $PiuRec = *on
097100040220     c                   leave
097200040220e   2c                   endif
097300040213      *
097400040213e   1c                   ENDDO
097500040213      *
097600040213     c                   ENDSR
097700040109
097800040109      *---------------------------------------------------------------*
097900040109      * Riempimento pagina con righe vuote nel sfl                    *
098000040109      *---------------------------------------------------------------*
098100040109     c     sr_FreeRow    BEGSR
098200040109      *
098300040120     c                   eval      *in09  = *on
098400040109     c                   eval      S01nrr = wNrr
098500040213     c     wNrr          div       K_SflPag      wPag
098600040213     c                   mvr                     wRig
098700040213     c                   add       1             wRig
098800040109      *
098900040213do  1c     wRig          do        K_SflPag      wNrr
099000040109      *
099100160406      * - Già caricato nel sfl il n° max di righe gestibili nel
099200160406      *   file FITGN00F (max 999)
099300160406if  1c                   if        S01nrr >= 999
099400160406     c                   eval      *in09   = *off
099500160406     c                   eval      *in23   = *on
099600160406     c                   leave
099700160406e   1c                   endif
099800160406      *
099900040224     c                   add       1             S01nrr
100000040224     c                   clear                   FIB0S01
100100040218     c                   eval      V1Hpot = IB0pot
100200040421     c                   eval      V1Hant = IB0ant
100300040109     c                   eval      V1Hnut = IB0nut
100400040109     c                   eval      V1Hpdt = IB0pdt
100500040109     c                   eval      V1Htin = IB0tin
100600040112     c                   eval      V1Hpgn = IB0pgn
100700040216     c                   eval      V1Hoin = wOra
100800040224if  2c                   if        S01nrr < 6
100900040213sel 3c                   select
101000040224w   3c                   when      S01nrr = 1
101100040213     c                   eval      V1Cnot = DB0no1
101200040224w   3c                   when      S01nrr = 2
101300040213     c                   eval      V1Cnot = DB0no2
101400040224w   3c                   when      S01nrr = 3
101500040213     c                   eval      V1Cnot = DB0no3
101600040224w   3c                   when      S01nrr = 4
101700040213     c                   eval      V1Cnot = DB0no4
101800040224w   3c                   other
101900040224     c                   eval      V1Cnot = *blanks
102000040213e   3c                   endsl
102100040213e   2c                   endif
102200040109     c                   eval      H1in35 = *off
102300040224     c                   eval      H1in36 = *off
102400040224if  2c                   if        S01nrr = wNrr1Free
102500040123     c                   eval      V1Odin = wDataGMA
102600040120     c                   eval      V1Ooin = wOra
102700040120     c                   eval      V1Outn = KNMUS
102800040617      * se viene passato al programma il P.O. di scrittura note lo imposto
102900040617     c                   if        DB0pon = *zeros
103000040219     c                   eval      V1Opon = DUTpou
103100040617     c                   else
103200040617     c                   eval      V1Opon = DB0pon
103300040617     c                   endif
103400040120e   2c                   endif
103500040109      *
103600040528     c                   eval      *in41  = (V1Cnot = *blanks)
103700040120     c     wNrr          comp      K_SflPag                               23
103800040224      *
103900040109     c                   WRITE     FIB0S01
104000040109      *
104100040109e   1c                   enddo
104200040109      *
104300040109     c                   eval      V1Cpos = S01nrr - K_SflPag + 1
104400040109     c                   eval      wNrr   = S01nrr
104500040109      *
104600040109     c                   ENDSR
104700040108
104800040109      *---------------------------------------------------------------*
104900040109      * Cancellazione record dal subfile S01                          *
105000040109      *---------------------------------------------------------------*
105100040109     c     DltS01rec     BEGSR
105200040109      *
105300040109      * Reperisco il numero relativo del record da cancellare dalla
105400040109      *   posizione del cursore
105500040108     c                   exsr      RepNrrSfl
105600040213     c     $Nrr          cable     *zeros        NO_dlt
105700040109      *
105800040112      * Controllo che NON sia record protetto
105900040213     c     $Nrr          chain     FIB0S01
106000040213if  1c                   if        NOT %found(FIDNB0D)
106100040112     c                             or  H1in35 = *on
106200040109     c                   goto      NO_dlt
106300040109e   1c                   endif
106400040109      *
106500040109      * Preparo record vuoto da emettere per ultimo
106600040109     c                   clear                   FIB0S01
106700040219     c                   eval      V1Hpot = IB0pot
106800040421     c                   eval      V1Hant = IB0ant
106900040109     c                   eval      V1Hnut = IB0nut
107000040109     c                   eval      V1Hpdt = IB0pdt
107100040109     c                   eval      V1Htin = IB0tin
107200040112     c                   eval      V1Hpgn = IB0pgn
107300040216     c                   eval      V1Hoin = wOra
107400040109     c                   eval      H1in35 = *off
107500040224     c                   eval      H1in36 = *off
107600040112     c                   movel     dsSFLrec      LastS01
107700040109      *
107800040109      * Aggiorno dall'ultimo record caricato nel sfl (da "ripulire")
107900040109      *   a "salire"
108000040112     c                   eval      S01nrr =  wNrr
108100040213do  1c                   dow       S01nrr >= $Nrr
108200040109      *
108300040213     c     S01nrr        chain     FIB0S01
108400040213if  2c                   if        %found(FIDNB0D)
108500040112     c                   movel     dsSFLrec      SaveS01
108600040112     c                   movel     LastS01       dsSFLrec
108700040428     c*** sempre *off:   eval      *in34 = (IB0flm <> 'I')
108800040112     c                   eval      *in35 = (H1in35 = *on)
108900040224     c                   eval      *in36 = (H1in36 = *on)
109000040112     c                   UPDATE    FIB0S01
109100040112     c                   movel     SaveS01       LastS01
109200040112e   2c                   endif
109300040112      *
109400040112     c                   sub       1             S01nrr
109500040112e   1c                   enddo
109600040109      *
109700040109     c     NO_dlt        ENDSR
109800040109
109900040109      *---------------------------------------------------------------*
110000040213      * Inserimento dei dati a video in records del file FITGN        *
110100040109      *---------------------------------------------------------------*
110200040112     c     Upd_FITGN     BEGSR
110300040219      *
110400040219      * Se consentita la modifica anche dei record già immessi
110500040219      *   forzo l'aggiornamento completo (da sfl a pf)
110600040219if  1c                   if            IB0flm = 'X'
110700040224     c                             or  IB0flm = 'C'
110800040224     ***c                             or (IB0flm = 'C'
110900040224     ***c                             and SAVflm = *blanks)
111000040224     ***c                             or (IB0flm = 'C'
111100040224     ***c                             and SAVflm = 'M')
111200040219     c                   z-add     1             wNrr1Free
111300040219     c                   clear                   SAVpgn
111400040224     c                   reset                   $SavRec
111500040224     c                   reset                   SAVrec1
111600040224     c                   reset                   SAVrec2
111700040224     c                   reset                   SAVrec3
111800040224     c                   reset                   SAVrec4
111900040219e   1c                   endif
112000040112      *
112100040130      * Cerco l'ultimo record valido nel subfile
112200040130do  1c                   DOW       wNrr > *zeros
112300040213     c     wNrr          chain     FIB0S01
112400040213if  2c                   if            %found(FIDNB0D)
112500040130     c                             and V1Cnot = *blanks
112600040130     c                   sub       1             wNrr
112700040130x   2c                   else
112800040130     c                   leave
112900040130e   2c                   endif
113000040130e   1c                   ENDDO
113100040112      *
113200040213      * Copio i nuovi records dal video al file
113300040213      *  (proseguendo dall'ultimo progressivo note reperito in fase di
113400040213      *   caricamento del subfile)
113500040213     c                   clear                   FITGN000
113600040213     c                   eval      TGNpgn  = SAVpgn
113700040130      *
113800040213do  1c     wNrr1Free     DO        wNrr          S01nrr
113900040112      *
114000040213     c     S01nrr        chain     FIB0S01
114100040130      *
114200040219      * - Elaboro records NON vuoti (anche se protetti)
114300040213if  2c                   IF            %found(FIDNB0D)
114400040112     c                             and V1Cnot <> *blanks
114500040219     c*****                        and H1in35 =  *off
114600040130      *
114700040130     c                   eval      TGNpot  = V1Hpot
114800040421     c                   eval      TGNant  = V1Hant
114900040130     c                   eval      TGNnut  = V1Hnut
115000040130     c                   eval      TGNpdt  = V1Hpdt
115100040130     c                   eval      TGNtin  = V1Htin
115200040112     c                   add       1             TGNpgn
115300040130     c                   eval      TGNnot  = V1Cnot
115400040216if  3c                   if        V1Odin  > *zeros
115500040123     c                   clear                   WLBdat
115600040216     c                   eval      G02dat  = V1Odin
115700040120     c                   call      'XSRDA8'
115800040120     c                   parm                    WLBdat
115900040130     c                   eval      TGNdin  = G02inv
116000040123x   3c                   else
116100040130     c                   eval      TGNdin  = wDataAMG
116200040123e   3c                   endif
116300040216if  3c                   if        V1Hoin  > *zeros
116400040216     c                   eval      TGNoin  = V1Hoin
116500040120x   3c                   else
116600040130     c                   eval      TGNoin  = wOra
116700040120e   3c                   endif
116800040130if  3c                   if        V1Outn <> *blanks
116900040130     c                   eval      TGNutn  = V1Outn
117000040130x   3c                   else
117100040130     c                   eval      TGNutn  = KNMUS
117200040130e   3c                   endif
117300040130if  3c                   if        V1Opon <> *zeros
117400040130     c                   eval      TGNpon  = V1Opon
117500040130x   3c                   else
117600040617      * se passato dal PGM lo imposto
117700040617     c                   if        db0pon = *zeros
117800040219     c                   eval      TGNpon  = DUTpou
117900040617     c                   else
118000040617     c                   eval      TGNpon  = Db0pon
118100040617     c                   endif
118200040130e   3c                   endif
118300040112      *
118400081023
118500081023      * controllo il campo nota che non contenga caratteri particolari
118600081023
118700081023     c                   exsr      Sr_Chk
118800081023
118900040112     c                   WRITE     FITGN000
119000040123      *
119100040213      * - Memorizzo i dati da restituire al pgm. chiamante
119200040224if  3c                   if        $SavRec = *on
119300040130sel 4c                   select
119400040130w   4c                   when      SR1nut  = *zeros
119500040130     c                   movel     FITGNds       SAVrec1
119600040130w   4c                   when      SR2nut  = *zeros
119700040130     c                   movel     FITGNds       SAVrec2
119800040130w   4c                   when      SR3nut  = *zeros
119900040130     c                   movel     FITGNds       SAVrec3
120000040130w   4c                   when      SR4nut  = *zeros
120100040130     c                   movel     FITGNds       SAVrec4
120200040130     c                   eval      $SavRec = *off
120300040130e   4c                   endsl
120400040213x   3c                   else
120500040213     c                   eval      $PiuRec = *on
120600040130e   3c                   endif
120700040112      *
120800040213e   2c                   ENDIF
120900040112      *
121000040213e   1c                   ENDDO
121100040213      *
121200040213      * - Re-imposto il flag per la inizializzazione del (nuovo) sfl
121300040213     c                   reset                   $InzS01
121400040219     c                   reset                   SaveB0ds
121500040219     c                   clear                   SAVflm
121600040112      *
121700040112     c                   ENDSR
121800040108
121900040108      *---------------------------------------------------------------*
122000040109      * Reperimento numero relativo record in cui è posizionato il    *
122100040109      * cursore nel subfile.                                          *
122200040108      *---------------------------------------------------------------*
122300040108     c     RepNrrSfl     BegSr
122400040108      *
122500040213     c                   z-add     di_SFLnrr     $Nrr
122600040120     c                   move      di_NRG        R$$
122700040213     c                   add       RiRi          $Nrr
122800040213     c                   sub       K_SflNrr1     $Nrr
122900040109      *
123000040213if  1c                   if           $Nrr < *zeros
123100040109     c                             or RiRi < K_SflNrr1
123200040213     c                   clear                   $Nrr
123300040108e   1c                   endif
123400040108      *
123500040108     c                   EndSr
