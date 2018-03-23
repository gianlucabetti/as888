000100071025      *PARMS OPTION(*NOXREF)
000200071025      *PARMS CLOSQLCSR(*ENDMOD) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
000300071025      * FIOR39R *-----------------------------------------------------*
000400071025      *?ATTRIBUZIONE GIRO AD ANAGRAFICA CLIENTI RITIRO               ?*
000500071025      *---------------------------------------------------------------*
000600071025
000700071025     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000800071025     h alwnull(*inputonly)
000900071025
001000071025      *---------------------------------------------------------------*
001100071025
001200071025     fAZORG01L  if   e           k disk
001300080110     fTABEL00F  if   e           k disk
001400071025      *
001500071026     fFNACR11L  if A e           k disk
001600071025      *
001700071025     fFIOR39D   cf   e             workstn sfile(OR39S01:S01nrr)
001800071025     f                                     infds(dsFMT)
001900071025
002000071025      *
002100071025      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002200071025      *
002300071025      * - Nr. di righe del sfl (SFLPAG)
002400071025     d C_SflPag        c                   const(08)
002500071025      * - Titoli in testata
002600071026     d C_OrderBy       c                   const(' order by ACRlor, +
002700071026     d                                                      ACRnar, +
002800071025     d                                                      ACRprr, +
002900071025     d                                                      ACRinr, +
003000071025     d                                                      ACRcro')
003100071025     d C_ForFetch      c                   const(' FOR FETCH ONLY')
003200071025      *
003300071025      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003400071025      *
003500071025      * - Stringhe SQL
003600080110     D $Sql            S             41    dim(13)  ctdata  perrcd(1)
003700071025      * - Messaggi di errore
003800080110     D $Msg            S             78    dim( 8)  ctdata  perrcd(1)
003900071025      * - Filiali in £1
004000071025     d $L1             s              3  0 dim(30)  inz
004100071025      * - Filiali in £6
004200071025     d $L6             s              3  0 dim(30)  inz
004300071031      * - Codici giro da inserire in FNACR10F
004400071031     d $Cgi            s                   like(ACR1cgi)
004500071031     d                                     dim(03)  inz
004600071031      * - Ambito instradamento giri da inserire in FNACR10F
004700071031     d $Ain            s                   like(ACR1ain)
004800071031     d                                     dim(%elem($Cgi)) inz
004900071025      *
005000071025      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
005100071025      *
005200071025      * - Parametri
005300071025     d KPJBA         e ds
005400071025      *
005500071025      * - Reperimento dati utente
005600071025     d TIBS34ds      e ds
005700071025     d AZUTEds       e ds                  extname(AZUTE00F)
005800071025     d dDatiUte      e ds
005900071025      *
006000071026      * - Dati estratti via SQL
006100071025     d FNACR0ds      e ds                  inz  extname(FNACR00F)
006200071025      *
006300071025      * - Controllo filiale in gestione
006400071025     d FNLV50ds      e ds                  inz
006500071025     d  D50tla       e                     inz('L')
006600071025      *
006700071025      * - Caricamento schiera filiali
006800071025     d TRUL06ds      e ds                  inz
006900071025     d  D06cod       e                     inz('£6')
007000071025     d  D06tla       e                     inz('L')
007100071025     d  D06lin                 1     90  0 inz  dim(30)
007200071025      *
007300071025      * - Dati dell'organigramma
007400071025     d Og147         e ds                  inz
007500071025     d Og148         e ds                  inz
007600080110      *
007700080110      * - Tab. 5A/SEDE1 (Giorni pulizia in Sede, per i gg. pulizia ORM)
007800080110     d ds5AS1        e ds                  inz
007900071025      *
008000071025      * - Gestione anagrafica clienti ritiro (con giro)
008100071025     d FIOR37ds      e ds                  inz
008200071025      *
008300071025     d Status         sds           333
008400071025     d   SDSpgm          *proc
008500071025      *
008600071025     d dsFMT           ds           512
008700071025     d  £Tasto               369    369
008800071025     d  £SFLnrr              378    379b 0
008900071025      *
009000071025      * - Codice cliente ritiro
009100071025     d ds_CRO          ds            10    inz
009200071025     d  dsCRO1                        3  0 inz
009300071025     d  dsCRO2                        4  0 inz
009400071025     d  dsCRO3                        3  0 inz
009500071025      *
009600071025      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
009700071025      *
009800071025      * - Flags
009900071025     d $InzD01         s              1    inz(*on)
010000071025     d $InzS01         s              1    inz(*on)
010100071025     d $Fine           s              1    inz(*off)
010200071025     d $EoF            s              1    inz(*off)
010300071025     d $Err            s              1    inz(*off)
010400071025     d $SelMult        s              1    inz(*off)
010500071025      * - Indici di schiera
010600071025     d xx              s              3  0 inz
010700071025      * - Gestione video
010800071025     d S01nrr          s                   inz  like(C01rcd)
010900071025     d $Video          s              2    inz('D1')
011000071025     ***d wPag            s              4  0 inz
011100071025     ***d wRig            s              3  0 inz
011200071025     ***d W_SflPag        s              3  0 inz
011300071025      * - Comodo
011400071025     d wSQL            s           1000    inz  varying
011500071026     d SAVcgi          s                   inz  like(ACR1cgi)
011600080110     d wDLIO           s              8  0 inz                                  like(ORMdao)
011700080110      *
011800080110     d wDate_Iso       s               d   inz  datfmt(*iso)
011900071025      *
012000071025      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
012100071025      *
012200080110     c     k03tabel      klist
012300080110     c                   kfld                    TBLkut
012400080110     c                   kfld                    TBLcod
012500080110     c                   kfld                    TBLkey
012600071026     c     k02acr11      klist
012700071026     c                   kfld                    S1Hcro
012800071026     c                   kfld                    V1Cpor
012900071025
013000071025      *---------------------------------------------------------------*
013100071025      *?RIEPILOGO INDICATORI                                         ?*
013200071025      *---------------------------------------------------------------*
013300071025      *  25    - Comodo                                               *
013400071025      *  28    - Emissione del messaggio di errore a video            *
013500071026      *  30    - NO emissione del subfile                       - C01 *
013600071026      *  31    - NO controllo del subfile                       - C01 *
013700071026      *  30 31 - Pulizia del subile                             - C01 *
013800071026      *  32    - Modifica record del subfile   (sflnxtchg)      - C01 *
013900071025      *  33    - Fine dati nel subfile         (sflend)         - C01 *
014000071026      *  50    - Opzione errata                                 - S01 *
014100071026      *  51    - Filiale ritiro        errata                   - D01 *
014200071026      *  52    - Località - da         errata                   - D01 *
014300071026      *  53    - Località - a          errata                   - D01 *
014400071026      *  54    - Ragione sociale       errata                   - D01 *
014500071025      *  90    - Generico di errore                                   *
014600071025      *---------------------------------------------------------------*
014700071025
014800071025     c     *Entry        plist
014900071025     c                   parm                    KPJBA
015000071025      *
015100071025      * Operazioni iniziali
015200071025     c                   exsr      RoutInz
015300071025      *
015400071025      * Gestione video
015500071025do  1c                   dow       $Fine   = *off
015600071025     c     $Video        caseq     'D1'          GesD01
015700071025     c     $Video        caseq     'S1'          GesS01
015800071025     c                   endcs
015900071025e   1c                   enddo
016000071025      *
016100071025      * Operazioni finali
016200071025     c                   eval      *inLR    = *on
016300071025
016400071025      *---------------------------------------------------------------*
016500071025      *?Operazioni Iniziali                                          ?*
016600071025      *---------------------------------------------------------------*
016700071025     c     RoutInz       BEGSR
016800071025      *
016900071025      * Reperimento dati job
017000071025     c                   exsr      DatiJob
017100071025      *
017200071025      * Impostazione nome programma a video
017300071025     c                   movel     SDSpgm        V1Tpgm
017400071025      *
017500071025      * Reperimento filiali gestite (£1)
017600071025     c                   clear                   TRUL06ds
017700071025     c                   eval      D06cod   = '£1'
017800071025     c                   movel(p)  DUTpou        D06key
017900071025     c                   movel(p)  TRUL06ds      KPJBU
018000071025     c                   call      'TRUL06R'
018100071025     c                   parm                    KPJBA
018200071025     c                   movel     KPJBU         TRUL06ds
018300071025     c                   movea     D06lin        $L1
018400071025     c                   clear                   TRUL06ds
018500080110      *
018600080110      * Reperimento tab. 5A/SEDE1 (Date pulizia sede)
018700080110     c                   clear                   ds5as1
018800080110     c                   eval      TBLkut = 1
018900080110     c                   eval      TBLcod = '5A'
019000080110     c                   eval      TBLkey = 'SEDE1'
019100080110     c     k03tabel      chain     TABEL
019200080110if  1c                   if        %found(TABEL00F) and TBLflg = *blanks
019300080110     c                   eval      ds5AS1 = TBLuni
019400080110e   1c                   endif
019500080110      *
019600080110      * Impostazione Data Limite Imissione ORM
019700080110     c                   movel     *date         wDate_Iso
019800080110     c                   subdur    §5Aors : *d   wDate_Iso
019900080110     c     *iso          movel     wDate_Iso     wDLIO
020000071025      *
020100071025     c                   ENDSR
020200071025
020300071025      *---------------------------------------------------------------*
020400071025      *?Reperimento Dati del job (Utente/Operativi)                  ?*
020500071025      *---------------------------------------------------------------*
020600071025     c     DatiJob       BEGSR
020700071025      *
020800071025     c     *dtaara       define    §azute        azuteds
020900071025     c     *dtaara       define    §datiute      ddatiute
021000071025      *
021100071025     c                   in(E)     *dtaara
021200071025     c                   IF        %ERROR or RSUT = *blanks
021300071025     c                   clear                   Tibs34Ds
021400071025     c                   call      'TIBS34R'
021500071025     c                   parm                    Tibs34Ds
021600071025     c                   in        *dtaara
021700071025     c                   ENDIF
021800071025      *
021900071025     c                   ENDSR
022000071025
022100071025      *---------------------------------------------------------------*
022200071025      *?Gestione videata D01                                         ?*
022300071025      *---------------------------------------------------------------*
022400071025     c     GesD01        BEGSR
022500071025      *
022600071025      * Inizializzazione videata
022700071025if  1c                   if        $InzD01  = *on
022800071025     c                   exsr      InzD01
022900071025     c                   eval      $InzD01  = *off
023000071025e   1c                   endif
023100071025      *
023200071025      * Emissione testata
023300071025if  1c                   if        NOT *in90
023400071025     c                   write     OR39T01
023500071025e   1c                   endif
023600071025      *
023700071025      * Emissione videata
023800071025     c                   exfmt     OR39D01
023900071025     c                   setoff                                       28  90
024000071025     c                   clear                   V1Dmsg
024100071025      *
024200071025sel 1c                   SELECT
024300071025      * F3=Fine
024400071025w   1c                   WHEN      *inKC
024500071025     c                   exsr      F03D01
024600071025      * Enter
024700071025x   1c                   OTHER
024800071025     c                   exsr      CtrD01
024900071025if  2c                   if        *in90
025000071025     c                   leavesr
025100071025e   2c                   endif
025200071025     c                   eval      $Video   = 'S1'
025300071025     c                   eval      $InzS01  = *on
025400071025      *
025500071025e   1c                   ENDSL
025600071025      *
025700071025     c                   ENDSR
025800071025
025900071025      *---------------------------------------------------------------*
026000071025      *?Inizializzazione videata D01                                 ?*
026100071025      *---------------------------------------------------------------*
026200071025     c     InzD01        BEGSR
026300071025      *
026400071025     c                   clear                   OR39D01
026500071025      *
026600071025     c                   eval      V1Cpor   = DUTpou
026700071025      *
026800071025      * Decodifica filiale ritiro
026900071025     c                   exsr      CtrPOR
027000071025     c                   clear                   V1Dmsg
027100071025     c                   eval      *in28    = *off
027200071025     c                   movea     *zeros        *in(50)
027300071025      *
027400071025     c                   ENDSR
027500071025
027600071025      *---------------------------------------------------------------*
027700071025      *?Controllo filiale ritiro (V1CPOR)                            ?*
027800071025      *---------------------------------------------------------------*
027900071025     c     CtrPOR        BEGSR
028000071025      *
028100071025      * Controllo/Decodifica filiale di gestione
028200071025     c     V1Cpor        chain     AZORG
028300071025if  1c                   IF        NOT  %found(AZORG01L)
028400071025     c                   eval      V1Dpor   = *all'? '
028500071025     c                   seton                                        285190
028600071025     c                   eval      V1Dmsg   = $Msg(1)
028700071025     c                   leavesr
028800071025e   1c                   ENDIF
028900071025     c                   movel     ORGdes        V1Dpor
029000071025     c                   movel     ORGde7        Og147
029100071025     c                   movel     ORGde8        Og148
029200071025if  1c                   IF        §OGorm   = *blanks
029300071026     c                   seton                                        285190
029400071026     c                   eval      V1Dmsg   = $Msg(2)
029500071025     c                   leavesr
029600071025e   1c                   ENDIF
029700071025if  1c                   IF        §OGcgiO  = *blanks
029800071026     c                   seton                                        285190
029900071026     c                   eval      V1Dmsg   = %trimr($Msg(2))
030000071025     c                                      + '. Usare la funzione +
030100071025     c                                        NON GEO'
030200071025     c                   leavesr
030300071025e   1c                   ENDIF
030400071025      *
030500071025      * Controllo se filiale in gestione all'utente
030600071025     c                   reset                   FNLV50ds
030700071025     c                   eval      D50pru   = KNMUS
030800071026     c                   eval      D50fgs   = V1Cpor
030900071025     c                   call      'FNLV50R'
031000071025     c                   parm                    FNLV50ds
031100071025if  1c                   if        D50err  <> *blanks
031200071026     c                   eval      V1Dmsg   = $Msg(3)
031300071025     c                   seton                                        285190
031400071025     c                   leavesr
031500071025e   1c                   endif
031600071025      *
031700071025      * Controllo se filiale ritiro in £1
031800071025     c     V1Cpor        lookup    $L1                                    25
031900071025if  1c                   if        NOT  *in25
032000071025     c                   seton                                        285190
032100071026     c                   eval      V1Dmsg   = $Msg(3)
032200071025     c                   leavesr
032300071025e   1c                   endif
032400071025      *
032500071025      * Reperimento filiali gestione Arrivi (£6)
032600071025     c                   reset                   TRUL06ds
032700071025     c*** già così       eval      D06cod   = '£6'
032800071025     c                   movel(p)  V1Cpor        D06key
032900071025     c                   movel(p)  TRUL06ds      KPJBU
033000071025     c                   call      'TRUL06R'
033100071025     c                   parm                    KPJBA
033200071025     c                   movel     KPJBU         TRUL06ds
033300071025     c                   movea     D06lin        $L6
033400071025      *
033500071025     c                   ENDSR
033600071025
033700071025      *---------------------------------------------------------------*
033800071025      *?Controllo videata D01                                        ?*
033900071025      *---------------------------------------------------------------*
034000071025     c     CtrD01        BEGSR
034100071025      *
034200071025     c                   movea     *zeros        *in(50)
034300071025      *
034400071025      * Controllo filiale ritiro
034500071026     c                   exsr      CtrPOR
034600071025if  1c                   if        *in90
034700071025     c                   leavesr
034800071025e   1c                   endif
034900071025      *
035000071025      * Auto-Impostazione località "a"
035100071025      * - se impostata località "DA" e non località "A":
035200071025      *   si imposta località "A" uguale alla "DA"+'ZZZ'
035300071025if  1c                   if             V1ClorD <> *blanks
035400071025     c                             and  V1ClorA  = *blanks
035500071025     c                   eval      V1ClorA   = V1ClorD
035600071025     c                   eval      %subst( V1ClorA :
035700071025     c                                %len( %trimr( V1ClorA )) + 1)
035800071025     c                                    = *all'Z'
035900071025e   1c                   endif
036000071025      *
036100071025      * - se impostata località "DA" e località "A" con solo iniziale:
036200071025      *   si imposta dal secondo byte 'ZZZ' x la selezione
036300071025if  1c                   if             V1ClorD <> *blanks
036400071025     c                             and  V1ClorA <> *blanks
036500071025     c                             and  %subst(V1ClorA:2:1) = *blanks
036600071025     c                   eval      %subst( V1ClorA :
036700071025     c                                %len( %trimr( V1ClorA )) + 1)
036800071025     c                                    = *all'Z'
036900071025e   1c                   endif
037000071025      *
037100071025      * - se impostata località "A" e non località "DA":
037200071025      *   si imposta località "DA" uguale alla "A"
037300071025if  1c                   if             V1ClorD  = *blanks
037400071025     c                             and  V1ClorA <> *blanks
037500071025     c                   eval      V1ClorD  = V1ClorA
037600071025e   1c                   endif
037700071025      *
037800071025      * - verifica che località 'DA' sia congruente con località 'A'
037900071025if  1c                   if        V1ClorD  > V1ClorA
038000071025     c                   seton                                        285290
038100071026     c                   eval      V1Dmsg   = $Msg(4)
038200071025     c                   leavesr
038300071025e   1c                   endif
038400080110      *
038500080110      * Selezione delle sole anagrafiche con ORM negli ultimi xxx gg.
038600080110     c                   select
038700080110     c                   when      V1Coug   < 1
038800080110     c                   seton                                        285590
038900080110     c                   eval      V1Dmsg   = $Msg(7)
039000080110     c                   leavesr
039100080110     c                   when      V1Coug   > §5Aors
039200080110     c                   seton                                        285590
039300080111     c                   eval      V1Dmsg   = %trimr($Msg(8))
039400080111     c                                      + ' ' + %editc(§5Aors:'J')
039500080110     c                   leavesr
039600080110     c                   endsl
039700071025      *
039800071025     c                   ENDSR
039900071025
040000071025      *---------------------------------------------------------------*
040100071025      *?Gestione tasto funzionale F3 da videata D01                  ?*
040200071025      *?F3=Fine                                                      ?*
040300071025      *---------------------------------------------------------------*
040400071025     c     F03D01        BEGSR
040500071025      *
040600071025      * Chiusura del programma
040700071025     c                   eval      $Fine    = *on
040800071025      *
040900071025     c                   ENDSR
041000071025
041100071025      *---------------------------------------------------------------*
041200071025      *?Gestione videata S01                                         ?*
041300071025      *---------------------------------------------------------------*
041400071025     c     GesS01        BEGSR
041500071025      *
041600071025      * Inizializzazione videata
041700071025if  1c                   if        $InzS01  = *on
041800071025     c                   exsr      InzS01
041900071025     c                   eval      $InzS01  = *off
042000071025e   1c                   endif
042100071025      *
042200071025      * Emissione testata e riga tasti funzionali abilitati
042300071025     c                   write     OR39T01
042400071025     c                   write     OR39Z01
042500071025      *
042600071025      * Posizionamento del cursore
042700071025sel 1c                   select
042800071025w   1c                   when      S01nrr  <= *zeros
042900071025     c                   write     OR39D02
043000071025w   1c                   when      C01csr   > *zeros
043100071025     c                   z-add     C01csr        C01rcd
043200071025e   1c                   endsl
043300071025      *
043400071025      * Emissione videata
043500071025     c                   exfmt     OR39C01
043600071025     c                   z-add     £SFLnrr       C01rcd
043700071025     c                   setoff                                       28  90
043800071025     c                   clear                   V1Dmsg
043900071025      *
044000071025sel 1c                   SELECT
044100071025      * F1=Seleziona tutti singolarmente ("1")
044200071025w   1c                   WHEN      *inKA
044300071025     c                   exsr      F01S01
044400071025      *
044500071025      * F3=Fine
044600071025w   1c                   WHEN      *inKC
044700071025     c                   exsr      F03D01
044800071025      *
044900071025      * F5=Rivisualizzazione
045000071025w   2c                   WHEN      *inKE
045100071025     c                   eval      $InzS01  = *on
045200071025      *
045300071025      * F9=Altri dati (SflDrop)
045400071025w   1c***                WHEN      *inKI
045500071025      *
045600071025      * F12=Ritorno
045700071025w   2c                   WHEN      *inKL
045800071025     c                   exsr      F12S01
045900071025      *
046000071025      * Gestione opzioni
046100071025x   1c                   OTHER
046200071025     c                   exsr      OpzS01
046300071025      *
046400071025e   1c                   ENDSL
046500071025      *
046600071025     c                   ENDSR
046700071025
046800071025      *---------------------------------------------------------------*
046900071025      *?Inizializzazione videata S01                                 ?*
047000071025      *---------------------------------------------------------------*
047100071025     c     InzS01        BEGSR
047200071025      *
047300071025      * Pulizia subfile
047400071025     c                   seton                                        3031
047500071025     c                   write     OR39C01
047600071025     c                   setoff                                         3133
047700071025      *
047800071025     c                   reset                   $EoF
047900071026     c***                clear                   W_SflPag
048000071025     c                   clear                   C01rcd
048100071025     c                   clear                   C01csr
048200071025     c                   clear                   S01nrr
048300071025     c                   clear                   V1Dmsg
048400071025     c                   setoff                                       28  90
048500071025     c                   movea     *zeros        *in(40)
048600071025      *
048700071025      * Preparazione stringa SQL
048800071025     c                   exsr      PrepSQL
048900071025      *
049000071025      * Ciclo di elaborazione
049100071025      * per caricamento dei dati da presentare nel sfl
049200071025     c                   exsr      OpenCursor
049300071025      *
049400071026do  1c                   dou            $Eof   = *on
049500071026     c*** completo!                and  S01nrr < C_SflPag
049600071025     c                   exsr      ReadCursor
049700071025e   1c                   enddo
049800071025      *
049900071025     c                   exsr      CloseCursor
050000071025      *
050100071025      * Posizionamento cursore al 1° rcd della pagina
050200071025if  1c***                if        S01nrr   > *zeros
050300071025     c***  S01nrr        div       C_SflPag      wPag
050400071025     c***                mvr                     wRig
050500071025     c***                eval      C01rcd   = wPag * C_SflPag
050600071025     c***                if        wRig     > *zeros
050700071025     c***                eval      C01rcd   = C01rcd + 1
050800071025     c***                else
050900071025     c***                eval      C01rcd   = C01rcd - C_SflPag + 1
051000071025     c***                endif
051100071025x   1c***                else
051200071025     c***                clear                   C01rcd
051300071025e   1c***                endif
051400071025     c***                eval      C01csr   = C01rcd
051500071025      *
051600071025      * Impaginazione subfile
051700071025      * -> forza l'impaginazione sul 1° rec. del subfile
051800071025if  1c                   if        S01nrr   > *zeros
051900071025     c                   eval      C01rcd   = 1
052000071025     c                   eval      C01csr   = 1
052100071025x   1c                   else
052200071025     c                   clear                   C01rcd
052300071025e   1c                   endif
052400071026      *
052500071026      * Visualizzazione del SFL (se ci sono dati)
052600071026     c                   eval      *in30    = (S01nrr <= *zeros)
052700071026      *
052800071026      * Attivazione (eventuale) del SFLEND
052900071026     c                   eval      *in33    = ($EoF = *on)
053000071025      *
053100071025     c                   ENDSR
053200071025
053300071025      *---------------------------------------------------------------*
053400071025      *?Preparazione stringa SQL                                     ?*
053500071025      *---------------------------------------------------------------*
053600071025     c     PrepSQL       BEGSR
053700080114      *
053800080114      * Impostazione Data Limite Imissione ORM
053900080114     c                   movel     *date         wDate_Iso
054000080114     c                   subdur    V1Coug : *d   wDate_Iso
054100080114     c     *iso          movel     wDate_Iso     wDLIO
054200071025      *
054300071025     c                   clear                   wSQL
054400071026      *
054500071026      *SELEZIONE DEI CLIENTI CON ORM? SENZA RECORD IN FNACR10F?
054600071025      *
054700071025      *? E S E M P I O : ?
054800071025      *
054900071026      * select distinct FNACR00F.*                 1
055000071026      *   from FNORM00F inner join FNACR00F        2
055100071026      *     on ORMcra = ACRcro                     3
055200071026      *    and ORMatb = ACRatb                     4
055300071026      *             exception join FNACR10F        5
055400071026      *     on ORMcra = ACR1cro                    6
055500071026      *     on ORMpor = ACR1pocgi                  7
055600071026      *    and ORMatb = ACR1atb                    8
055700071026      *  where ORMatb = ' ' and ACRtcr <> 'M'      9
055800071026      *    and ORMpor = 123                       10
055900071026      *    and ACRlor between 'YYYYY' and 'WWWWW' 11
056000071026      *    and ACRrsr like '%ZZZZZ%'              12
056100071025      *  order by ACRlor, ACRcro
056200071025      *  FOR FETCH ONLY
056300071025      *
056400071026     c                   eval      wSQL     = %trimr($Sql(1))
056500071026     c                                      + %trimr($Sql(2))
056600071026     c                                      + %trimr($Sql(3))
056700071026     c                                      + %trimr($Sql(4))
056800071026     c                                      + %trimr($Sql(5))
056900071026     c                                      + %trimr($Sql(6))
057000071026     c                                      + %trimr($Sql(7))
057100071026     c                                      + %trimr($Sql(8))
057200071026     c                                      + %trimr($Sql(9))
057300071026     c                                      + %trimr($Sql(10))
057400071026     c                                      + ' ' + %char(V1Cpor)
057500071025      *
057600071026      * Impostazione parzializzazioni
057700071025      * - località da - a
057800071026if  1c                   if        V1ClorD  > *blanks
057900071025     c                   eval      wSQL     = wSQL
058000071026     c                                      + %trimr($Sql(11))
058100071025     c                                      + %trimr(V1ClorD)
058200071025     c                                      + ''' and '''
058300071025     c                                      + %trimr(V1ClorA)
058400071025     c                                      + ''''
058500071026e   1c                   endif
058600071025      * - ragione sociale (anche parziale)
058700071026if  1c                   if        V1Crsr   > *blanks
058800071025     c                   eval      wSQL     = wSQL
058900071026     c                                      + %trimr($Sql(12))
059000071025     c                                      + %trimr(V1Crsr)  + '%'''
059100071026e   1c                   endif
059200080110      * - data immissione (anche parziale)
059300080110if  1c                   if        V1Coug   > *zeros
059400080110     c                   eval      wSQL     = wSQL
059500080110     c                                      + %trimr($Sql(13))
059600080110     c                                      + ' ' + %char(wDLIO)
059700080110e   1c                   endif
059800071025      *
059900071025      * Order By
060000071025     c                   eval      wSQL     = %trimr(wSQL)
060100071026     c                                      + C_OrderBy
060200071025      * For Fetch Dnly
060300071025     c                                      + C_ForFetch
060400071025      *
060500071025     c                   ENDSR
060600071025
060700071025      *---------------------------------------------------------------*
060800071025      *?Apertura cursore.                                            ?*
060900071025      *---------------------------------------------------------------*
061000071025     c     OpenCursor    BEGSR
061100071025      *
061200071025      * Impostazione opzioni SQL
061300071025      *
061400071025     c/exec sql
061500071025     c+     set  option  DynUsrPrf = *Owner,
061600071025     c+                  CloSqlCsr = *EndMod
061700071025     c/end-exec
061800071025      *
061900071025      * Preparazione stringa SQL da eseguire
062000071025      *
062100071025     c/exec sql
062200071025     c+     prepare S1 from :wSQL
062300071025     c/end-exec
062400071025      *
062500071025      * Dichiarazione cursore
062600071025      *
062700071025     c/exec sql
062800071025     c+     declare C1 cursor for S1
062900071025     c/end-exec
063000071025      *
063100071025      * Apertura del cursore
063200071025      *
063300071025     c/exec sql
063400071025     c+     open C1
063500071025     c/end-exec
063600071025      *
063700071025     c                   ENDSR
063800071025
063900071025      *---------------------------------------------------------------*
064000071025      *?Chiusura cursore.                                            ?*
064100071025      *---------------------------------------------------------------*
064200071025     c     CloseCursor   BEGSR
064300071025      *
064400071025     c/exec sql
064500071025     c+     close C1
064600071025     c/end-exec
064700071025      *
064800071025     c                   ENDSR
064900071025
065000071025      *---------------------------------------------------------------*
065100071025      *?Lettura cursore.                                             ?*
065200071025      *---------------------------------------------------------------*
065300071025     c     ReadCursor    BEGSR
065400071025      *
065500071025     c                   eval      *in32    = *off
065600071025     c                   clear                   FNACR0ds
065700071025      *
065800071025     c/exec sql
065900071026     c+     fetch next from C1 into :FNACR0ds
066000071025     c/end-exec
066100071025      *
066200071025sel 1c                   select
066300071025w   1c                   when      SQLcod   = 100
066400071025     c                   eval      $EoF     = *on
066500071025w   1c                   when      SQLcod   < *zeros
066600071025     c                   eval      $Err     = *on
066700071025     c                   eval      $Eof     = *on
066800071025x   1c                   other
066900071025     c                   exsr      sr_Elab
067000071025e   1c                   endsl
067100071025      *
067200071025     c                   ENDSR
067300071025
067400071025      *---------------------------------------------------------------*
067500071026      *?Elaborazione singolo record da FNACR00F (da FNORM00F)        ?*
067600071025      *---------------------------------------------------------------*
067700071025     c     sr_Elab       BEGSR
067800071025      *
067900071025     c                   exsr      CarS01
068000071025      *
068100071025     c                   add       1             S01nrr
068200071025     c                   write     OR39S01
068300071025      *
068400071025     c                   ENDSR
068500071025
068600071025      *---------------------------------------------------------------*
068700071025      *?Caricamento singolo record nel SubFile S01                   ?*
068800071025      *---------------------------------------------------------------*
068900071025     c     CarS01        BEGSR
069000071025      *
069100071025     c                   clear                   OR39S01
069200071025      * Campi hidden
069300071025     c                   eval      S1Hcro   = ACRcro
069400071025      *
069500071025      * Campi di solo output
069600071026      * - Località
069700071025     c                   movel     ACRlor        S1Clor
069800071026      * - Provincia / Nazione
069900071025if  1c                   if        ACRprr  <> *blanks
070000071025     c                   eval      S1Cprr   = ACRprr
070100071025x   1c                   else
070200071025     c                   eval      S1Cprr   = ACRnar
070300071025e   1c                   endif
070400071026      * - Indirizzo
070500071025     c                   eval      S1Cinr   = ACRinr
070600071026      * - Sponda Idraulica
070700071025if  1c                   if        ACRspi   = 'S'
070800071025     c                   eval      S1Dspi   = 'SpI'
070900071025e   1c                   endif
071000071026      * - Codice cliente ritiro
071100071025     c                   movel     ACRcro        ds_CRO
071200071025     c                   eval      S1Ccr1   = dsCRO1
071300071025     c                   eval      S1Ccr2   = dsCRO2
071400071025     c                   eval      S1Ccr3   = dsCRO3
071500071026      * - Ragione sociale cliente ritiro
071600071025     c                   movel     ACRrsr        S1Crsr
071700071025      *
071800071025     c                   ENDSR
071900071025
072000071025      *---------------------------------------------------------------*
072100071025      *?Gestione tasto funzionale F1 da videata S01                  ?*
072200071026      *?F1=Selezione di tutti i record nel subfile                   ?*
072300071025      *---------------------------------------------------------------*
072400071025     c     F01S01        BEGSR
072500071025      *
072600071025if  1c                   if        S01nrr  <= *zeros
072700071025     c                   leavesr
072800071025e   1c                   endif
072900071025      *
073000071025     c                   eval      C01csr   = C01rcd
073100071025do  1c                   DO        *hival        C01rcd
073200071025      *
073300071025     c     C01rcd        chain     OR39S01
073400071025if  2c                   if        NOT  %found(FIOR39D)
073500071025     c                   leave
073600071025e   2c                   endif
073700071025      *
073800071025     c                   eval      S1Copz   = 1
073900071025     c                   eval      *in32    = *on
074000071025     c                   update    OR39S01
074100071025      *
074200071025e   1c                   ENDDO
074300071025     c                   eval      C01rcd   = C01csr
074400071025      *
074500071025     c                   ENDSR
074600071025
074700071025      *---------------------------------------------------------------*
074800071025      *?Gestione tasto funzionale F12 da videata S01                 ?*
074900071025      *?F12=Ritorno                                                  ?*
075000071025      *---------------------------------------------------------------*
075100071025     c     F12S01        BEGSR
075200071025      *
075300071025      * Ritorno alla videata D01
075400071025     c                   eval      $Video   = 'D1'
075500071025      *
075600071025     c                   ENDSR
075700071025
075800071025      *---------------------------------------------------------------*
075900071025      *?Controllo opzioni subfile                                    ?*
076000071025      *---------------------------------------------------------------*
076100071025     c     OpzS01        BEGSR
076200071025      *
076300071025     c                   eval      $SelMult = *off
076400071026     c                   eval      SAVcgi   = *blanks
076500071025      *
076600071025     c                   readc     OR39S01
076700071025      *
076800071025do  1c                   DOW       NOT %eof(FIOR39D)
076900071025      *
077000071025     c                   eval      *in32    = *off
077100071025     c                   movea     *zeros        *in(50)
077200071025     c                   z-add     S01nrr        C01rcd
077300071025      *
077400071025sel 2c                   SELECT
077500071026      *
077600071025      * - Nessuna opzione
077700071025w   2c                   WHEN      S1Copz   = *zeros
077800071026      *
077900071025      * - 1 = Selezione singola
078000071025w   2c                   WHEN      S1Copz   = 1
078100071026     c                   exsr      Call_FIOR37R2
078200071026      *
078300071025      * - 9 = Selezione multipla
078400071025w   2c                   WHEN      S1Copz   = 9
078500071025if  3c                   if        $SelMult = *off
078600071026     c                   exsr      Call_FIOR37R2
078700071025x   3c                   else
078800071031     c                   exsr      Wrt_FNACR1
078900071025e   3c                   endif
079000071026      *
079100071025      * - 5 = Visualizzazione Anagrafica Cliente Ritiro
079200071025w   2c                   WHEN      S1Copz   = 5
079300071025     c                   exsr      Call_FIOR37R1
079400071026      *
079500071025      * - ? = Opzione NON valida
079600071025x   2c                   OTHER
079700071025     c                   seton                                        285090
079800071025     c                   eval      V1Dmsg   = $Msg(5)
079900071026      *
080000071025e   2c                   ENDSL
080100071025      *
080200071026      * Memorizzazione Nrr del Sfl con la 1ª opzione
080300071025      * per riposizionarvici il cursore dopo il tasto "Invio"
080400071025if  2c***                if             S1Copz   <> *blanks
080500071025     c***                          and (SavS01csr = *zeros
080600071025     c***                           or  SavS01csr < C01rcd)
080700071025     c***                eval      SavS01csr   = C01rcd
080800071025e   2c***                endif
080900071025      *
081000071025      * Aggiornamento sfl
081100071025sel 2c                   select
081200071025w   2c                   when      *in28
081300071025     c                   eval      *in32   = *on
081400071025     c                   z-add     C01rcd        C01csr
081500071025w   2c                   when      *in90   = *on
081600071025     c                   z-add     C01rcd        C01csr
081700071025     c                   clear                   S1Copz
081800071025x   2c                   other
081900071025     c                   clear                   S1Copz
082000071025e   2c                   endsl
082100071025     c                   UPDATE    OR39S01
082200071025if  2c                   if        *in28  OR  *in90
082300071025     c                   eval      $InzS01 = *off
082400071025     c                   leave
082500071025e   2c                   endif
082600071025      *
082700071025     c                   readc     OR39S01
082800071025      *
082900071025e   1c                   ENDDO
083000071025      *
083100071025      * Riposiziono il cursore sul record contenente la prima opzione
083200071025      *  (se non sono stati rilevati errori)
083300071025if  1c***                if        NOT *in28  and  NOT *in90
083400071025     c***                          and SavS01csr  > *zeros
083500071025     c***                z-add     SavS01csr     C01csr
083600071025e   1c***                endif
083700071025      *
083800071025     c                   ENDSR
083900071025
084000071025      *---------------------------------------------------------------*
084100071025      *?Richiamo programma di Visualizzaz. Anagrafica Clienti Ritiro ?*
084200071025      *---------------------------------------------------------------*
084300071025     c     Call_FIOR37R1 BEGSR
084400071025      *
084500071025      * Impostazione parametri
084600071025     c                   clear                   FIOR37ds
084700071025     c                   eval      I37fgs   = V1Cpor
084800071025     c                   eval      I37opz   = '5'
084900071025     c                   eval      I37cro   = S1Hcro
085000071025      *
085100071025     c                   movel(p)  FIOR37ds      KPJBU
085200071025     c                   call      'FIOR37R1'
085300071025     c                   parm                    KPJBA
085400071025     c                   movel     KPJBU         FIOR37ds
085500071025      *
085600071025sel 1c                   SELECT
085700071025      * - Errore
085800071025w   1c                   WHEN      O37err   = 'E'
085900071025     c                   eval      V1Dmsg   = O37msg
086000071025     c                   seton                                        28  90
086100071025      * - F3=Fine
086200071025w   1c                   WHEN      O37ret   = '1'
086300071025     c***                eval      $Fine    = *on
086400071025     c                   eval      *in90    = *on
086500071025      * - F12=Ritorno
086600071025w   1c                   WHEN      O37ret   = '2'
086700071025e   1c                   ENDSL
086800071025      *
086900071025     c                   ENDSR
087000071026
087100071026      *---------------------------------------------------------------*
087200071026      *?Richiamo programma di Attribuzione Giro ad un Cliente Ritiro ?*
087300071026      *---------------------------------------------------------------*
087400071026     c     Call_FIOR37R2 BEGSR
087500071026      *
087600071026      * Impostazione parametri
087700071026     c                   clear                   FIOR37ds
087800071026     c                   eval      I37fgs   = V1Cpor
087900071026if  1c                   if             S1Copz   = 9
088000071026     c                             and  $SelMult = *off
088100071026     c                   eval      I37opz   = '9'
088200071026e   1c                   endif
088300071026     c                   eval      I37cro   = S1Hcro
088400071026     c                   eval      I37pocgi = V1Cpor
088500071026      *
088600071026     c                   movel(p)  FIOR37ds      KPJBU
088700071026     c                   call      'FIOR37R2'
088800071026     c                   parm                    KPJBA
088900071026     c                   movel     KPJBU         FIOR37ds
089000071026      *
089100071026sel 1c                   SELECT
089200071026      * - Errore
089300071026w   1c                   WHEN      O37err   = 'E'
089400071026     c                   eval      V1Dmsg   = O37msg
089500071026     c                   seton                                        28  90
089600071026      * - F3=Fine
089700071026w   1c                   WHEN      O37ret   = '1'
089800071026     c***                eval      $Fine    = *on
089900071026     c                   eval      *in90    = *on
090000071026      * - F12=Ritorno
090100071026w   1c                   WHEN      O37ret   = '2'
090200071031      * - Ok => memo giri inseriti, che non verranno più richiesti
090300071026x   1c                   OTHER
090400080124     c                   clear                   xx
090500071026     c                   eval      $InzS01  = *on
090600071026if  2c                   if             S1Copz   = 9
090700071026     c                             and  $SelMult = *off
090800071031     c     k02acr11      setll     FNACR100
090900071031     c     k02acr11      reade     FNACR100
091000071031do  3c                   dow       NOT  %eof(FNACR11L)
091100071031     c                   add       1             xx
091200071031     c                   eval      $Cgi(xx) = ACR1cgi
091300071031     c                   eval      $Ain(xx) = ACR1ain
091400071026     c                   eval      $SelMult = *on
091500071031     c     k02acr11      reade     FNACR100
091600071031e   3c                   enddo
091700071026e   2c                   endif
091800071026e   1c                   ENDSL
091900071026      *
092000071026     c                   ENDSR
092100071025
092200071031      *---------------------------------------------------------------*
092300071031      *?Inserimento records file FNACR10F                            ?*
092400071025      *---------------------------------------------------------------*
092500071031     c     Wrt_FNACR1    BEGSR
092600071025      *
092700071025      * Impostazione campi nel record
092800071025     c                   clear                   FNACR100
092900071025      *
093000071026     c*** già così:      clear                   ACR1atb
093100071025     c                   eval      ACR1cro   = S1Hcro
093200071026     c                   eval      ACR1pocgi = V1Cpor
093300071025     c                   eval      ACR1dtv   = *Date
093400071025     c                   eval      ACR1ute   = KNMUS
093500071031      *
093600071031      * Ciclo di inserimento record nel file FNACR10F
093700071031     c                   eval      xx        = 1
093800071031      *
093900071031do  1c                   DOW            xx       <= %elem($Cgi)
094000071031     c                             and  $Cgi(xx) <> *blanks
094100071031      *
094200071031     c                   eval      ACR1cgi   = $Cgi(xx)
094300071031     c                   eval      ACR1ain   = $Ain(xx)
094400071031      *
094500071031     c                   WRITE(e)  FNACR100
094600071025      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
094700071031      * Errore in fase di scrittura record
094800071031if  2c                   if        %error
094900071031     c                   seton                                        28  90
095000071031     c                   eval      V1Dmsg   = %trimr($Msg(6))
095100071031     c                                      + ' FNACR10F'
095200071031     c*** in sr GesS01:  eval      $InzS01  = *off
095300071031     c                   leavesr
095400071031e   2c                   endif
095500071031      *
095600071031     c                   add       1             xx
095700071031      *
095800071031e   3c                   ENDDO
095900071026      *
096000071025     c                   eval      $InzS01  = *on
096100071025      *
096200071025     c                   ENDSR
096300071025
096400071025** - $SQL ------------------------------*
096500071025select distinct FNACR00F.*                 1
096600071026 from FNORM00F inner join FNACR00F         2
096700071026  on  ORMcra = ACRcro                      3
096800071026  and ORMatb = ACRatb                      4
096900071026 exception join FNACR10F                   5
097000071026  on  ORMcra = ACR1cro                     6
097100071026  and ORMpor = ACR1pocgi                   7
097200071026  and ORMatb = ACR1atb                     8
097300071025 where ORMatb = ' ' and ACRtcr <> 'M'      9
097400071026  and ORMpor =                            10
097500071026  and ACRlor between '                    11
097600071026  and ACRrsr like '%                      12
097700080110  and ORMdao >=                           13
097800071025** - $MSG -------------------------------------------------------------------*
097900071025Filiale errata                                                                  1
098000071026Procedura ORM non attivata                                                      2
098100080110La filiale non è in gestione all'utente                                         3
098200071026Parzializzazione per località errata                                            4
098300071025Opzione errata                                                                  5
098400071026Errore in fase di scrittura record nel file                                     6
098500080110Numero giorni obbligatorio                                                      7
098600080111Numero giorni superiore a quello previsto nella pulizia degli ORM:              8
