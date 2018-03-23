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
003900071031      * - Codici giro da inserire in FNACR10F
004000071031     d $Cgi            s                   like(ACR1cgi)
004100071031     d                                     dim(03)  inz
004200071031      * - Ambito instradamento giri da inserire in FNACR10F
004300071031     d $Ain            s                   like(ACR1ain)
004400071031     d                                     dim(%elem($Cgi)) inz
004500071025      *
004600071025      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
004700071025      *
004800071025      * - Parametri
004900071025     d KPJBA         e ds
005000071025      *
005100071025      * - Reperimento dati utente
005200071025     d TIBS34ds      e ds
005300071025     d AZUTEds       e ds                  extname(AZUTE00F)
005400071025     d dDatiUte      e ds
005500071025      *
005600071026      * - Dati estratti via SQL
005700071025     d FNACR0ds      e ds                  inz  extname(FNACR00F)
005800071025      *
005900071025      * - Controllo filiale in gestione
006000071025     d FNLV50ds      e ds                  inz
006100071025     d  D50tla       e                     inz('L')
006200071025      *
006300071025      * - Dati dell'organigramma
006400071025     d Og147         e ds                  inz
006500071025     d Og148         e ds                  inz
006600080110      *
006700080110      * - Tab. 5A/SEDE1 (Giorni pulizia in Sede, per i gg. pulizia ORM)
006800080110     d ds5AS1        e ds                  inz
006900071025      *
007000071025      * - Gestione anagrafica clienti ritiro (con giro)
007100071025     d FIOR37ds      e ds                  inz
007200071025      *
007300071025     d Status         sds           333
007400071025     d   SDSpgm          *proc
007500071025      *
007600071025     d dsFMT           ds           512
007700071025     d  £Tasto               369    369
007800071025     d  £SFLnrr              378    379b 0
007900071025      *
008000071025      * - Codice cliente ritiro
008100071025     d ds_CRO          ds            10    inz
008200071025     d  dsCRO1                        3  0 inz
008300071025     d  dsCRO2                        4  0 inz
008400071025     d  dsCRO3                        3  0 inz
008500071025      *
008600071025      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
008700071025      *
008800071025      * - Flags
008900071025     d $InzD01         s              1    inz(*on)
009000071025     d $InzS01         s              1    inz(*on)
009100071025     d $Fine           s              1    inz(*off)
009200071025     d $EoF            s              1    inz(*off)
009300071025     d $Err            s              1    inz(*off)
009400071025     d $SelMult        s              1    inz(*off)
009500071025      * - Indici di schiera
009600071025     d xx              s              3  0 inz
009700071025      * - Gestione video
009800071025     d S01nrr          s                   inz  like(C01rcd)
009900071025     d $Video          s              2    inz('D1')
010000071025     ***d wPag            s              4  0 inz
010100071025     ***d wRig            s              3  0 inz
010200071025     ***d W_SflPag        s              3  0 inz
010300071025      * - Comodo
010400071025     d wSQL            s           1000    inz  varying
010500071026     d SAVcgi          s                   inz  like(ACR1cgi)
010600080110     d wDLIO           s              8  0 inz                                  like(ORMdao)
010700080110      *
010800080110     d wDate_Iso       s               d   inz  datfmt(*iso)
010900071025      *
011000071025      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
011100071025      *
011200080110     c     k03tabel      klist
011300080110     c                   kfld                    TBLkut
011400080110     c                   kfld                    TBLcod
011500080110     c                   kfld                    TBLkey
011600071026     c     k02acr11      klist
011700071026     c                   kfld                    S1Hcro
011800071026     c                   kfld                    V1Cpor
011900071025
012000071025      *---------------------------------------------------------------*
012100071025      *?RIEPILOGO INDICATORI                                         ?*
012200071025      *---------------------------------------------------------------*
012300071025      *  25    - Comodo                                               *
012400071025      *  28    - Emissione del messaggio di errore a video            *
012500071026      *  30    - NO emissione del subfile                       - C01 *
012600071026      *  31    - NO controllo del subfile                       - C01 *
012700071026      *  30 31 - Pulizia del subile                             - C01 *
012800071026      *  32    - Modifica record del subfile   (sflnxtchg)      - C01 *
012900071025      *  33    - Fine dati nel subfile         (sflend)         - C01 *
013000071026      *  50    - Opzione errata                                 - S01 *
013100071026      *  51    - Filiale ritiro        errata                   - D01 *
013200071026      *  52    - Località - da         errata                   - D01 *
013300071026      *  53    - Località - a          errata                   - D01 *
013400071026      *  54    - Ragione sociale       errata                   - D01 *
013500071025      *  90    - Generico di errore                                   *
013600071025      *---------------------------------------------------------------*
013700071025
013800071025     c     *Entry        plist
013900071025     c                   parm                    KPJBA
014000071025      *
014100071025      * Operazioni iniziali
014200071025     c                   exsr      RoutInz
014300071025      *
014400071025      * Gestione video
014500071025do  1c                   dow       $Fine   = *off
014600071025     c     $Video        caseq     'D1'          GesD01
014700071025     c     $Video        caseq     'S1'          GesS01
014800071025     c                   endcs
014900071025e   1c                   enddo
015000071025      *
015100071025      * Operazioni finali
015200071025     c                   eval      *inLR    = *on
015300071025
015400071025      *---------------------------------------------------------------*
015500071025      *?Operazioni Iniziali                                          ?*
015600071025      *---------------------------------------------------------------*
015700071025     c     RoutInz       BEGSR
015800071025      *
015900071025      * Reperimento dati job
016000071025     c                   exsr      DatiJob
016100071025      *
016200071025      * Impostazione nome programma a video
016300071025     c                   movel     SDSpgm        V1Tpgm
016400080110      *
016500080110      * Reperimento tab. 5A/SEDE1 (Date pulizia sede)
016600080110     c                   clear                   ds5as1
016700080110     c                   eval      TBLkut = 1
016800080110     c                   eval      TBLcod = '5A'
016900080110     c                   eval      TBLkey = 'SEDE1'
017000080110     c     k03tabel      chain     TABEL
017100080110if  1c                   if        %found(TABEL00F) and TBLflg = *blanks
017200080110     c                   eval      ds5AS1 = TBLuni
017300080110e   1c                   endif
017400080110      *
017500080110      * Impostazione Data Limite Imissione ORM
017600080110     c                   movel     *date         wDate_Iso
017700080110     c                   subdur    §5Aors : *d   wDate_Iso
017800080110     c     *iso          movel     wDate_Iso     wDLIO
017900071025      *
018000071025     c                   ENDSR
018100071025
018200071025      *---------------------------------------------------------------*
018300071025      *?Reperimento Dati del job (Utente/Operativi)                  ?*
018400071025      *---------------------------------------------------------------*
018500071025     c     DatiJob       BEGSR
018600071025      *
018700071025     c     *dtaara       define    §azute        azuteds
018800071025     c     *dtaara       define    §datiute      ddatiute
018900071025      *
019000071025     c                   in(E)     *dtaara
019100071025     c                   IF        %ERROR or RSUT = *blanks
019200071025     c                   clear                   Tibs34Ds
019300071025     c                   call      'TIBS34R'
019400071025     c                   parm                    Tibs34Ds
019500071025     c                   in        *dtaara
019600071025     c                   ENDIF
019700071025      *
019800071025     c                   ENDSR
019900071025
020000071025      *---------------------------------------------------------------*
020100071025      *?Gestione videata D01                                         ?*
020200071025      *---------------------------------------------------------------*
020300071025     c     GesD01        BEGSR
020400071025      *
020500071025      * Inizializzazione videata
020600071025if  1c                   if        $InzD01  = *on
020700071025     c                   exsr      InzD01
020800071025     c                   eval      $InzD01  = *off
020900071025e   1c                   endif
021000071025      *
021100071025      * Emissione testata
021200071025if  1c                   if        NOT *in90
021300071025     c                   write     OR39T01
021400071025e   1c                   endif
021500071025      *
021600071025      * Emissione videata
021700071025     c                   exfmt     OR39D01
021800071025     c                   setoff                                       28  90
021900071025     c                   clear                   V1Dmsg
022000071025      *
022100071025sel 1c                   SELECT
022200071025      * F3=Fine
022300071025w   1c                   WHEN      *inKC
022400071025     c                   exsr      F03D01
022500071025      * Enter
022600071025x   1c                   OTHER
022700071025     c                   exsr      CtrD01
022800071025if  2c                   if        *in90
022900071025     c                   leavesr
023000071025e   2c                   endif
023100071025     c                   eval      $Video   = 'S1'
023200071025     c                   eval      $InzS01  = *on
023300071025      *
023400071025e   1c                   ENDSL
023500071025      *
023600071025     c                   ENDSR
023700071025
023800071025      *---------------------------------------------------------------*
023900071025      *?Inizializzazione videata D01                                 ?*
024000071025      *---------------------------------------------------------------*
024100071025     c     InzD01        BEGSR
024200071025      *
024300071025     c                   clear                   OR39D01
024400071025      *
024500071025     c                   eval      V1Cpor   = DUTpou
024600071025      *
024700071025      * Decodifica filiale ritiro
024800071025     c                   exsr      CtrPOR
024900071025     c                   clear                   V1Dmsg
025000071025     c                   eval      *in28    = *off
025100071025     c                   movea     *zeros        *in(50)
025200071025      *
025300071025     c                   ENDSR
025400071025
025500071025      *---------------------------------------------------------------*
025600071025      *?Controllo filiale ritiro (V1CPOR)                            ?*
025700071025      *---------------------------------------------------------------*
025800071025     c     CtrPOR        BEGSR
025900071025      *
026000071025      * Controllo/Decodifica filiale di gestione
026100071025     c     V1Cpor        chain     AZORG
026200071025if  1c                   IF        NOT  %found(AZORG01L)
026300071025     c                   eval      V1Dpor   = *all'? '
026400071025     c                   seton                                        285190
026500071025     c                   eval      V1Dmsg   = $Msg(1)
026600071025     c                   leavesr
026700071025e   1c                   ENDIF
026800071025     c                   movel     ORGdes        V1Dpor
026900071025     c                   movel     ORGde7        Og147
027000071025     c                   movel     ORGde8        Og148
027100071025if  1c                   IF        §OGorm   = *blanks
027200071026     c                   seton                                        285190
027300071026     c                   eval      V1Dmsg   = $Msg(2)
027400071025     c                   leavesr
027500071025e   1c                   ENDIF
027600071025if  1c                   IF        §OGcgiO  = *blanks
027700071026     c                   seton                                        285190
027800071026     c                   eval      V1Dmsg   = %trimr($Msg(2))
027900071025     c                                      + '. Usare la funzione +
028000071025     c                                        NON GEO'
028100071025     c                   leavesr
028200071025e   1c                   ENDIF
028300071025      *
028400071025      * Controllo se filiale in gestione all'utente
028500071025     c                   reset                   FNLV50ds
028600071025     c                   eval      D50pru   = KNMUS
028700071026     c                   eval      D50fgs   = V1Cpor
028800071025     c                   call      'FNLV50R'
028900071025     c                   parm                    FNLV50ds
029000071025if  1c                   if        D50err  <> *blanks
029100071026     c                   eval      V1Dmsg   = $Msg(3)
029200071025     c                   seton                                        285190
029300071025     c                   leavesr
029400071025e   1c                   endif
029500071025      *
029600071025     c                   ENDSR
029700071025
029800071025      *---------------------------------------------------------------*
029900071025      *?Controllo videata D01                                        ?*
030000071025      *---------------------------------------------------------------*
030100071025     c     CtrD01        BEGSR
030200071025      *
030300071025     c                   movea     *zeros        *in(50)
030400071025      *
030500071025      * Controllo filiale ritiro
030600071026     c                   exsr      CtrPOR
030700071025if  1c                   if        *in90
030800071025     c                   leavesr
030900071025e   1c                   endif
031000071025      *
031100071025      * Auto-Impostazione località "a"
031200071025      * - se impostata località "DA" e non località "A":
031300071025      *   si imposta località "A" uguale alla "DA"+'ZZZ'
031400071025if  1c                   if             V1ClorD <> *blanks
031500071025     c                             and  V1ClorA  = *blanks
031600071025     c                   eval      V1ClorA   = V1ClorD
031700071025     c                   eval      %subst( V1ClorA :
031800071025     c                                %len( %trimr( V1ClorA )) + 1)
031900071025     c                                    = *all'Z'
032000071025e   1c                   endif
032100071025      *
032200071025      * - se impostata località "DA" e località "A" con solo iniziale:
032300071025      *   si imposta dal secondo byte 'ZZZ' x la selezione
032400071025if  1c                   if             V1ClorD <> *blanks
032500071025     c                             and  V1ClorA <> *blanks
032600071025     c                             and  %subst(V1ClorA:2:1) = *blanks
032700071025     c                   eval      %subst( V1ClorA :
032800071025     c                                %len( %trimr( V1ClorA )) + 1)
032900071025     c                                    = *all'Z'
033000071025e   1c                   endif
033100071025      *
033200071025      * - se impostata località "A" e non località "DA":
033300071025      *   si imposta località "DA" uguale alla "A"
033400071025if  1c                   if             V1ClorD  = *blanks
033500071025     c                             and  V1ClorA <> *blanks
033600071025     c                   eval      V1ClorD  = V1ClorA
033700071025e   1c                   endif
033800071025      *
033900071025      * - verifica che località 'DA' sia congruente con località 'A'
034000071025if  1c                   if        V1ClorD  > V1ClorA
034100071025     c                   seton                                        285290
034200071026     c                   eval      V1Dmsg   = $Msg(4)
034300071025     c                   leavesr
034400071025e   1c                   endif
034500080110      *
034600080110      * Selezione delle sole anagrafiche con ORM negli ultimi xxx gg.
034700080110     c                   select
034800080110     c                   when      V1Coug   < 1
034900080110     c                   seton                                        285590
035000080110     c                   eval      V1Dmsg   = $Msg(7)
035100080110     c                   leavesr
035200080110     c                   when      V1Coug   > §5Aors
035300080110     c                   seton                                        285590
035400080111     c                   eval      V1Dmsg   = %trimr($Msg(8))
035500080111     c                                      + ' ' + %editc(§5Aors:'J')
035600080110     c                   leavesr
035700080110     c                   endsl
035800071025      *
035900071025     c                   ENDSR
036000071025
036100071025      *---------------------------------------------------------------*
036200071025      *?Gestione tasto funzionale F3 da videata D01                  ?*
036300071025      *?F3=Fine                                                      ?*
036400071025      *---------------------------------------------------------------*
036500071025     c     F03D01        BEGSR
036600071025      *
036700071025      * Chiusura del programma
036800071025     c                   eval      $Fine    = *on
036900071025      *
037000071025     c                   ENDSR
037100071025
037200071025      *---------------------------------------------------------------*
037300071025      *?Gestione videata S01                                         ?*
037400071025      *---------------------------------------------------------------*
037500071025     c     GesS01        BEGSR
037600071025      *
037700071025      * Inizializzazione videata
037800071025if  1c                   if        $InzS01  = *on
037900071025     c                   exsr      InzS01
038000071025     c                   eval      $InzS01  = *off
038100071025e   1c                   endif
038200071025      *
038300071025      * Emissione testata e riga tasti funzionali abilitati
038400071025     c                   write     OR39T01
038500071025     c                   write     OR39Z01
038600071025      *
038700071025      * Posizionamento del cursore
038800071025sel 1c                   select
038900071025w   1c                   when      S01nrr  <= *zeros
039000071025     c                   write     OR39D02
039100071025w   1c                   when      C01csr   > *zeros
039200071025     c                   z-add     C01csr        C01rcd
039300071025e   1c                   endsl
039400071025      *
039500071025      * Emissione videata
039600071025     c                   exfmt     OR39C01
039700071025     c                   z-add     £SFLnrr       C01rcd
039800071025     c                   setoff                                       28  90
039900071025     c                   clear                   V1Dmsg
040000071025      *
040100071025sel 1c                   SELECT
040200071025      * F1=Seleziona tutti singolarmente ("1")
040300071025w   1c                   WHEN      *inKA
040400071025     c                   exsr      F01S01
040500071025      *
040600071025      * F3=Fine
040700071025w   1c                   WHEN      *inKC
040800071025     c                   exsr      F03D01
040900071025      *
041000071025      * F5=Rivisualizzazione
041100071025w   2c                   WHEN      *inKE
041200071025     c                   eval      $InzS01  = *on
041300071025      *
041400071025      * F9=Altri dati (SflDrop)
041500071025w   1c***                WHEN      *inKI
041600071025      *
041700071025      * F12=Ritorno
041800071025w   2c                   WHEN      *inKL
041900071025     c                   exsr      F12S01
042000071025      *
042100071025      * Gestione opzioni
042200071025x   1c                   OTHER
042300071025     c                   exsr      OpzS01
042400071025      *
042500071025e   1c                   ENDSL
042600071025      *
042700071025     c                   ENDSR
042800071025
042900071025      *---------------------------------------------------------------*
043000071025      *?Inizializzazione videata S01                                 ?*
043100071025      *---------------------------------------------------------------*
043200071025     c     InzS01        BEGSR
043300071025      *
043400071025      * Pulizia subfile
043500071025     c                   seton                                        3031
043600071025     c                   write     OR39C01
043700071025     c                   setoff                                         3133
043800071025      *
043900071025     c                   reset                   $EoF
044000071026     c***                clear                   W_SflPag
044100071025     c                   clear                   C01rcd
044200071025     c                   clear                   C01csr
044300071025     c                   clear                   S01nrr
044400071025     c                   clear                   V1Dmsg
044500071025     c                   setoff                                       28  90
044600071025     c                   movea     *zeros        *in(40)
044700071025      *
044800071025      * Preparazione stringa SQL
044900071025     c                   exsr      PrepSQL
045000071025      *
045100071025      * Ciclo di elaborazione
045200071025      * per caricamento dei dati da presentare nel sfl
045300071025     c                   exsr      OpenCursor
045400071025      *
045500071026do  1c                   dou            $Eof   = *on
045600071026     c*** completo!                and  S01nrr < C_SflPag
045700071025     c                   exsr      ReadCursor
045800071025e   1c                   enddo
045900071025      *
046000071025     c                   exsr      CloseCursor
046100071025      *
046200071025      * Posizionamento cursore al 1° rcd della pagina
046300071025if  1c***                if        S01nrr   > *zeros
046400071025     c***  S01nrr        div       C_SflPag      wPag
046500071025     c***                mvr                     wRig
046600071025     c***                eval      C01rcd   = wPag * C_SflPag
046700071025     c***                if        wRig     > *zeros
046800071025     c***                eval      C01rcd   = C01rcd + 1
046900071025     c***                else
047000071025     c***                eval      C01rcd   = C01rcd - C_SflPag + 1
047100071025     c***                endif
047200071025x   1c***                else
047300071025     c***                clear                   C01rcd
047400071025e   1c***                endif
047500071025     c***                eval      C01csr   = C01rcd
047600071025      *
047700071025      * Impaginazione subfile
047800071025      * -> forza l'impaginazione sul 1° rec. del subfile
047900071025if  1c                   if        S01nrr   > *zeros
048000071025     c                   eval      C01rcd   = 1
048100071025     c                   eval      C01csr   = 1
048200071025x   1c                   else
048300071025     c                   clear                   C01rcd
048400071025e   1c                   endif
048500071026      *
048600071026      * Visualizzazione del SFL (se ci sono dati)
048700071026     c                   eval      *in30    = (S01nrr <= *zeros)
048800071026      *
048900071026      * Attivazione (eventuale) del SFLEND
049000071026     c                   eval      *in33    = ($EoF = *on)
049100071025      *
049200071025     c                   ENDSR
049300071025
049400071025      *---------------------------------------------------------------*
049500071025      *?Preparazione stringa SQL                                     ?*
049600071025      *---------------------------------------------------------------*
049700071025     c     PrepSQL       BEGSR
049800080114      *
049900080114      * Impostazione Data Limite Imissione ORM
050000080114     c                   movel     *date         wDate_Iso
050100080114     c                   subdur    V1Coug : *d   wDate_Iso
050200080114     c     *iso          movel     wDate_Iso     wDLIO
050300071025      *
050400071025     c                   clear                   wSQL
050500071026      *
050600071026      *SELEZIONE DEI CLIENTI CON ORM? SENZA RECORD IN FNACR10F?
050700071025      *
050800071025      *? E S E M P I O : ?
050900071025      *
051000071026      * select distinct FNACR00F.*                 1
051100071026      *   from FNORM00F inner join FNACR00F        2
051200071026      *     on ORMcra = ACRcro                     3
051300071026      *    and ORMatb = ACRatb                     4
051400071026      *             exception join FNACR10F        5
051500071026      *     on ORMcra = ACR1cro                    6
051600071026      *     on ORMpor = ACR1pocgi                  7
051700071026      *    and ORMatb = ACR1atb                    8
051800071026      *  where ORMatb = ' ' and ACRtcr <> 'M'      9
051900071026      *    and ORMpor = 123                       10
052000071026      *    and ACRlor between 'YYYYY' and 'WWWWW' 11
052100071026      *    and ACRrsr like '%ZZZZZ%'              12
052200071025      *  order by ACRlor, ACRcro
052300071025      *  FOR FETCH ONLY
052400071025      *
052500071026     c                   eval      wSQL     = %trimr($Sql(1))
052600071026     c                                      + %trimr($Sql(2))
052700071026     c                                      + %trimr($Sql(3))
052800071026     c                                      + %trimr($Sql(4))
052900071026     c                                      + %trimr($Sql(5))
053000071026     c                                      + %trimr($Sql(6))
053100071026     c                                      + %trimr($Sql(7))
053200071026     c                                      + %trimr($Sql(8))
053300071026     c                                      + %trimr($Sql(9))
053400071026     c                                      + %trimr($Sql(10))
053500071026     c                                      + ' ' + %char(V1Cpor)
053600071025      *
053700071026      * Impostazione parzializzazioni
053800071025      * - località da - a
053900071026if  1c                   if        V1ClorD  > *blanks
054000071025     c                   eval      wSQL     = wSQL
054100071026     c                                      + %trimr($Sql(11))
054200071025     c                                      + %trimr(V1ClorD)
054300071025     c                                      + ''' and '''
054400071025     c                                      + %trimr(V1ClorA)
054500071025     c                                      + ''''
054600071026e   1c                   endif
054700071025      * - ragione sociale (anche parziale)
054800071026if  1c                   if        V1Crsr   > *blanks
054900071025     c                   eval      wSQL     = wSQL
055000071026     c                                      + %trimr($Sql(12))
055100071025     c                                      + %trimr(V1Crsr)  + '%'''
055200071026e   1c                   endif
055300080110      * - data immissione (anche parziale)
055400080110if  1c                   if        V1Coug   > *zeros
055500080110     c                   eval      wSQL     = wSQL
055600080110     c                                      + %trimr($Sql(13))
055700080110     c                                      + ' ' + %char(wDLIO)
055800080110e   1c                   endif
055900071025      *
056000071025      * Order By
056100071025     c                   eval      wSQL     = %trimr(wSQL)
056200071026     c                                      + C_OrderBy
056300071025      * For Fetch Dnly
056400071025     c                                      + C_ForFetch
056500071025      *
056600071025     c                   ENDSR
056700071025
056800071025      *---------------------------------------------------------------*
056900071025      *?Apertura cursore.                                            ?*
057000071025      *---------------------------------------------------------------*
057100071025     c     OpenCursor    BEGSR
057200071025      *
057300071025      * Impostazione opzioni SQL
057400071025      *
057500071025     c/exec sql
057600071025     c+     set  option  DynUsrPrf = *Owner,
057700071025     c+                  CloSqlCsr = *EndMod
057800071025     c/end-exec
057900071025      *
058000071025      * Preparazione stringa SQL da eseguire
058100071025      *
058200071025     c/exec sql
058300071025     c+     prepare S1 from :wSQL
058400071025     c/end-exec
058500071025      *
058600071025      * Dichiarazione cursore
058700071025      *
058800071025     c/exec sql
058900071025     c+     declare C1 cursor for S1
059000071025     c/end-exec
059100071025      *
059200071025      * Apertura del cursore
059300071025      *
059400071025     c/exec sql
059500071025     c+     open C1
059600071025     c/end-exec
059700071025      *
059800071025     c                   ENDSR
059900071025
060000071025      *---------------------------------------------------------------*
060100071025      *?Chiusura cursore.                                            ?*
060200071025      *---------------------------------------------------------------*
060300071025     c     CloseCursor   BEGSR
060400071025      *
060500071025     c/exec sql
060600071025     c+     close C1
060700071025     c/end-exec
060800071025      *
060900071025     c                   ENDSR
061000071025
061100071025      *---------------------------------------------------------------*
061200071025      *?Lettura cursore.                                             ?*
061300071025      *---------------------------------------------------------------*
061400071025     c     ReadCursor    BEGSR
061500071025      *
061600071025     c                   eval      *in32    = *off
061700071025     c                   clear                   FNACR0ds
061800071025      *
061900071025     c/exec sql
062000071026     c+     fetch next from C1 into :FNACR0ds
062100071025     c/end-exec
062200071025      *
062300071025sel 1c                   select
062400071025w   1c                   when      SQLcod   = 100
062500071025     c                   eval      $EoF     = *on
062600071025w   1c                   when      SQLcod   < *zeros
062700071025     c                   eval      $Err     = *on
062800071025     c                   eval      $Eof     = *on
062900071025x   1c                   other
063000071025     c                   exsr      sr_Elab
063100071025e   1c                   endsl
063200071025      *
063300071025     c                   ENDSR
063400071025
063500071025      *---------------------------------------------------------------*
063600071026      *?Elaborazione singolo record da FNACR00F (da FNORM00F)        ?*
063700071025      *---------------------------------------------------------------*
063800071025     c     sr_Elab       BEGSR
063900071025      *
064000071025     c                   exsr      CarS01
064100071025      *
064200071025     c                   add       1             S01nrr
064300071025     c                   write     OR39S01
064400071025      *
064500071025     c                   ENDSR
064600071025
064700071025      *---------------------------------------------------------------*
064800071025      *?Caricamento singolo record nel SubFile S01                   ?*
064900071025      *---------------------------------------------------------------*
065000071025     c     CarS01        BEGSR
065100071025      *
065200071025     c                   clear                   OR39S01
065300071025      * Campi hidden
065400071025     c                   eval      S1Hcro   = ACRcro
065500071025      *
065600071025      * Campi di solo output
065700071026      * - Località
065800071025     c                   movel     ACRlor        S1Clor
065900071026      * - Provincia / Nazione
066000071025if  1c                   if        ACRprr  <> *blanks
066100071025     c                   eval      S1Cprr   = ACRprr
066200071025x   1c                   else
066300071025     c                   eval      S1Cprr   = ACRnar
066400071025e   1c                   endif
066500071026      * - Indirizzo
066600071025     c                   eval      S1Cinr   = ACRinr
066700071026      * - Sponda Idraulica
066800071025if  1c                   if        ACRspi   = 'S'
066900071025     c                   eval      S1Dspi   = 'SpI'
067000071025e   1c                   endif
067100071026      * - Codice cliente ritiro
067200071025     c                   movel     ACRcro        ds_CRO
067300071025     c                   eval      S1Ccr1   = dsCRO1
067400071025     c                   eval      S1Ccr2   = dsCRO2
067500071025     c                   eval      S1Ccr3   = dsCRO3
067600071026      * - Ragione sociale cliente ritiro
067700071025     c                   movel     ACRrsr        S1Crsr
067800071025      *
067900071025     c                   ENDSR
068000071025
068100071025      *---------------------------------------------------------------*
068200071025      *?Gestione tasto funzionale F1 da videata S01                  ?*
068300071026      *?F1=Selezione di tutti i record nel subfile                   ?*
068400071025      *---------------------------------------------------------------*
068500071025     c     F01S01        BEGSR
068600071025      *
068700071025if  1c                   if        S01nrr  <= *zeros
068800071025     c                   leavesr
068900071025e   1c                   endif
069000071025      *
069100071025     c                   eval      C01csr   = C01rcd
069200071025do  1c                   DO        *hival        C01rcd
069300071025      *
069400071025     c     C01rcd        chain     OR39S01
069500071025if  2c                   if        NOT  %found(FIOR39D)
069600071025     c                   leave
069700071025e   2c                   endif
069800071025      *
069900071025     c                   eval      S1Copz   = 1
070000071025     c                   eval      *in32    = *on
070100071025     c                   update    OR39S01
070200071025      *
070300071025e   1c                   ENDDO
070400071025     c                   eval      C01rcd   = C01csr
070500071025      *
070600071025     c                   ENDSR
070700071025
070800071025      *---------------------------------------------------------------*
070900071025      *?Gestione tasto funzionale F12 da videata S01                 ?*
071000071025      *?F12=Ritorno                                                  ?*
071100071025      *---------------------------------------------------------------*
071200071025     c     F12S01        BEGSR
071300071025      *
071400071025      * Ritorno alla videata D01
071500071025     c                   eval      $Video   = 'D1'
071600071025      *
071700071025     c                   ENDSR
071800071025
071900071025      *---------------------------------------------------------------*
072000071025      *?Controllo opzioni subfile                                    ?*
072100071025      *---------------------------------------------------------------*
072200071025     c     OpzS01        BEGSR
072300071025      *
072400071025     c                   eval      $SelMult = *off
072500071026     c                   eval      SAVcgi   = *blanks
072600071025      *
072700071025     c                   readc     OR39S01
072800071025      *
072900071025do  1c                   DOW       NOT %eof(FIOR39D)
073000071025      *
073100071025     c                   eval      *in32    = *off
073200071025     c                   movea     *zeros        *in(50)
073300071025     c                   z-add     S01nrr        C01rcd
073400071025      *
073500071025sel 2c                   SELECT
073600071026      *
073700071025      * - Nessuna opzione
073800071025w   2c                   WHEN      S1Copz   = *zeros
073900071026      *
074000071025      * - 1 = Selezione singola
074100071025w   2c                   WHEN      S1Copz   = 1
074200071026     c                   exsr      Call_FIOR37R2
074300071026      *
074400071025      * - 9 = Selezione multipla
074500071025w   2c                   WHEN      S1Copz   = 9
074600071025if  3c                   if        $SelMult = *off
074700071026     c                   exsr      Call_FIOR37R2
074800071025x   3c                   else
074900071031     c                   exsr      Wrt_FNACR1
075000071025e   3c                   endif
075100071026      *
075200071025      * - 5 = Visualizzazione Anagrafica Cliente Ritiro
075300071025w   2c                   WHEN      S1Copz   = 5
075400071025     c                   exsr      Call_FIOR37R1
075500071026      *
075600071025      * - ? = Opzione NON valida
075700071025x   2c                   OTHER
075800071025     c                   seton                                        285090
075900071025     c                   eval      V1Dmsg   = $Msg(5)
076000071026      *
076100071025e   2c                   ENDSL
076200071025      *
076300071026      * Memorizzazione Nrr del Sfl con la 1ª opzione
076400071025      * per riposizionarvici il cursore dopo il tasto "Invio"
076500071025if  2c***                if             S1Copz   <> *blanks
076600071025     c***                          and (SavS01csr = *zeros
076700071025     c***                           or  SavS01csr < C01rcd)
076800071025     c***                eval      SavS01csr   = C01rcd
076900071025e   2c***                endif
077000071025      *
077100071025      * Aggiornamento sfl
077200071025sel 2c                   select
077300071025w   2c                   when      *in28
077400071025     c                   eval      *in32   = *on
077500071025     c                   z-add     C01rcd        C01csr
077600071025w   2c                   when      *in90   = *on
077700071025     c                   z-add     C01rcd        C01csr
077800071025     c                   clear                   S1Copz
077900071025x   2c                   other
078000071025     c                   clear                   S1Copz
078100071025e   2c                   endsl
078200071025     c                   UPDATE    OR39S01
078300071025if  2c                   if        *in28  OR  *in90
078400071025     c                   eval      $InzS01 = *off
078500071025     c                   leave
078600071025e   2c                   endif
078700071025      *
078800071025     c                   readc     OR39S01
078900071025      *
079000071025e   1c                   ENDDO
079100071025      *
079200071025      * Riposiziono il cursore sul record contenente la prima opzione
079300071025      *  (se non sono stati rilevati errori)
079400071025if  1c***                if        NOT *in28  and  NOT *in90
079500071025     c***                          and SavS01csr  > *zeros
079600071025     c***                z-add     SavS01csr     C01csr
079700071025e   1c***                endif
079800071025      *
079900071025     c                   ENDSR
080000071025
080100071025      *---------------------------------------------------------------*
080200071025      *?Richiamo programma di Visualizzaz. Anagrafica Clienti Ritiro ?*
080300071025      *---------------------------------------------------------------*
080400071025     c     Call_FIOR37R1 BEGSR
080500071025      *
080600071025      * Impostazione parametri
080700071025     c                   clear                   FIOR37ds
080800071025     c                   eval      I37fgs   = V1Cpor
080900071025     c                   eval      I37opz   = '5'
081000071025     c                   eval      I37cro   = S1Hcro
081100071025      *
081200071025     c                   movel(p)  FIOR37ds      KPJBU
081300071025     c                   call      'FIOR37R1'
081400071025     c                   parm                    KPJBA
081500071025     c                   movel     KPJBU         FIOR37ds
081600071025      *
081700071025sel 1c                   SELECT
081800071025      * - Errore
081900071025w   1c                   WHEN      O37err   = 'E'
082000071025     c                   eval      V1Dmsg   = O37msg
082100071025     c                   seton                                        28  90
082200071025      * - F3=Fine
082300071025w   1c                   WHEN      O37ret   = '1'
082400071025     c***                eval      $Fine    = *on
082500071025     c                   eval      *in90    = *on
082600071025      * - F12=Ritorno
082700071025w   1c                   WHEN      O37ret   = '2'
082800071025e   1c                   ENDSL
082900071025      *
083000071025     c                   ENDSR
083100071026
083200071026      *---------------------------------------------------------------*
083300071026      *?Richiamo programma di Attribuzione Giro ad un Cliente Ritiro ?*
083400071026      *---------------------------------------------------------------*
083500071026     c     Call_FIOR37R2 BEGSR
083600071026      *
083700071026      * Impostazione parametri
083800071026     c                   clear                   FIOR37ds
083900071026     c                   eval      I37fgs   = V1Cpor
084000071026if  1c                   if             S1Copz   = 9
084100071026     c                             and  $SelMult = *off
084200071026     c                   eval      I37opz   = '9'
084300071026e   1c                   endif
084400071026     c                   eval      I37cro   = S1Hcro
084500071026     c                   eval      I37pocgi = V1Cpor
084600071026      *
084700071026     c                   movel(p)  FIOR37ds      KPJBU
084800071026     c                   call      'FIOR37R2'
084900071026     c                   parm                    KPJBA
085000071026     c                   movel     KPJBU         FIOR37ds
085100071026      *
085200071026sel 1c                   SELECT
085300071026      * - Errore
085400071026w   1c                   WHEN      O37err   = 'E'
085500071026     c                   eval      V1Dmsg   = O37msg
085600071026     c                   seton                                        28  90
085700071026      * - F3=Fine
085800071026w   1c                   WHEN      O37ret   = '1'
085900071026     c***                eval      $Fine    = *on
086000071026     c                   eval      *in90    = *on
086100071026      * - F12=Ritorno
086200071026w   1c                   WHEN      O37ret   = '2'
086300071031      * - Ok => memo giri inseriti, che non verranno più richiesti
086400071026x   1c                   OTHER
086500080124     c                   clear                   xx
086600071026     c                   eval      $InzS01  = *on
086700071026if  2c                   if             S1Copz   = 9
086800071026     c                             and  $SelMult = *off
086900071031     c     k02acr11      setll     FNACR100
087000071031     c     k02acr11      reade     FNACR100
087100071031do  3c                   dow       NOT  %eof(FNACR11L)
087200071031     c                   add       1             xx
087300071031     c                   eval      $Cgi(xx) = ACR1cgi
087400071031     c                   eval      $Ain(xx) = ACR1ain
087500071026     c                   eval      $SelMult = *on
087600071031     c     k02acr11      reade     FNACR100
087700071031e   3c                   enddo
087800071026e   2c                   endif
087900071026e   1c                   ENDSL
088000071026      *
088100071026     c                   ENDSR
088200071025
088300071031      *---------------------------------------------------------------*
088400071031      *?Inserimento records file FNACR10F                            ?*
088500071025      *---------------------------------------------------------------*
088600071031     c     Wrt_FNACR1    BEGSR
088700071025      *
088800071025      * Impostazione campi nel record
088900071025     c                   clear                   FNACR100
089000071025      *
089100071026     c*** già così:      clear                   ACR1atb
089200071025     c                   eval      ACR1cro   = S1Hcro
089300071026     c                   eval      ACR1pocgi = V1Cpor
089400071025     c                   eval      ACR1dtv   = *Date
089500071025     c                   eval      ACR1ute   = KNMUS
089600071031      *
089700071031      * Ciclo di inserimento record nel file FNACR10F
089800071031     c                   eval      xx        = 1
089900071031      *
090000071031do  1c                   DOW            xx       <= %elem($Cgi)
090100071031     c                             and  $Cgi(xx) <> *blanks
090200071031      *
090300071031     c                   eval      ACR1cgi   = $Cgi(xx)
090400071031     c                   eval      ACR1ain   = $Ain(xx)
090500071031      *
090600071031     c                   WRITE(e)  FNACR100
090700071025      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
090800071031      * Errore in fase di scrittura record
090900071031if  2c                   if        %error
091000071031     c                   seton                                        28  90
091100071031     c                   eval      V1Dmsg   = %trimr($Msg(6))
091200071031     c                                      + ' FNACR10F'
091300071031     c*** in sr GesS01:  eval      $InzS01  = *off
091400071031     c                   leavesr
091500071031e   2c                   endif
091600071031      *
091700071031     c                   add       1             xx
091800071031      *
091900071031e   3c                   ENDDO
092000071026      *
092100071025     c                   eval      $InzS01  = *on
092200071025      *
092300071025     c                   ENDSR
092400071025
092500071025** - $SQL ------------------------------*
092600071025select distinct FNACR00F.*                 1
092700071026 from FNORM00F inner join FNACR00F         2
092800071026  on  ORMcra = ACRcro                      3
092900071026  and ORMatb = ACRatb                      4
093000071026 exception join FNACR10F                   5
093100071026  on  ORMcra = ACR1cro                     6
093200071026  and ORMpor = ACR1pocgi                   7
093300071026  and ORMatb = ACR1atb                     8
093400071025 where ORMatb = ' ' and ACRtcr <> 'M'      9
093500071026  and ORMpor =                            10
093600071026  and ACRlor between '                    11
093700071026  and ACRrsr like '%                      12
093800080110  and ORMdao >=                           13
093900071025** - $MSG -------------------------------------------------------------------*
094000071025Filiale errata                                                                  1
094100071026Procedura ORM non attivata                                                      2
094200080110La filiale non è in gestione all'utente                                         3
094300071026Parzializzazione per località errata                                            4
094400071025Opzione errata                                                                  5
094500071026Errore in fase di scrittura record nel file                                     6
094600080110Numero giorni obbligatorio                                                      7
094700080111Numero giorni superiore a quello previsto nella pulizia degli ORM:              8
