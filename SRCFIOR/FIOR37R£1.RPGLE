000100070730      * FIOR37R *-----------------------------------------------------*
000200070730      *?GESTIONE ANAGRAFICA CLIENTI RITIRO (CON GIRO)                ?*
000300070730      *---------------------------------------------------------------*
000400070730
000500070731     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600070730
000700070730      *---------------------------------------------------------------*
000800000927
000900070731     fAZORG01L  if   e           k disk
001000070801      *
001100070801     fFNACR01L  if   e           k disk    rename(FNACR000:FNACR001)
001200070730     fFNACR02L  if   e           k disk    rename(FNACR000:FNACR002)
001300071030     fFNACR13L  if   e           k disk    rename(FNACR100:FNACR103)
001400071024     fFNACR12L  if   e           k disk    rename(FNACR100:FNACR102)
001500070730      *
001600070731     fFIOR37D   cf   e             workstn sfile(OR37S01:S01nrr)
001700070731     f                                     infds(DSFMT)
001800000927
001900070730      *
002000070730      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002100070730      *
002200070731      * - Nr. di righe del sfl (SFLPAG)
002300070731     d C_SflPag        c                   const(14)
002400070731      * - Tasti funzionali
002500070731     d C_Enter         c                   const(x'F1')
002600070731     d C_RollUp        c                   const(x'F5')
002700070801      * - Attributi di visualizzazione
002800070801     d C_RI            c                   const(x'21')
002900070801     d C_HI            c                   const(x'22')
003000070801     d C_HI_RI         c                   const(x'23')
003100070801     d C_UL            c                   const(x'24')
003200070801     d C_UL_RI         c                   const(x'25')
003300070801     d C_UL_HI         c                   const(x'26')
003400070801     d C_ND            c                   const(x'27')
003500070730      *
003600070730      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
003700070730      *
003800070730      * - Messaggi di errore
003900080303     d $Msg            s             78    dim(14) ctdata perrcd(1)             MSG VIDEO
004000080303      * - Filiali gestibili dall'utente
004100080303     d $Fgs            s              3    dim(250)       inz
004200070730      *
004300070730      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
004400070730      *
004500070730      * - Parametri
004600070730     d KPJBA         e ds
004700070730     d FIOR37ds      e ds                  inz
004800070730      *
004900070731      * - Reperimento dati utente
005000060203     d TIBS34ds      e ds
005100060203     d AZUTEds       e ds                  extname(AZUTE00F)
005200070731     d dDatiUte      e ds
005300080303     d dUte01        e ds                  inz
005400070730      *
005500080303      * - Caricamento schiera filiali gestibili
005600080303     d TRUL31ds      e ds                  inz
005700070731      *
005800070731      * - Lettura anagrafiche cliente
005900070731     d TIBS69ds      e ds                  inz
006000070731     d ds_CNACO      e ds                  inz extname(CNACO00F)
006100070731     d ds_CNIND      e ds                  inz extname(CNIND00F)
006200070731     d ds_CNCLP      e ds                  inz extname(CNCLP00F)
006300070731     d ds_FNCLS      e ds                  inz extname(FNCLS00F)
006400060203      *
006500070730      * - Gestione anagrafica giri
006600070730     d FIDG09ds      e ds                  inz
006700070730      *
006800070731     d Og148         e ds                  inz
006900070920     d Og147         e ds                  inz
007000070731     d Og143         e ds                  inz
007100080303      *
007200080303      * - Lettura tabelle
007300080303     d TIBS02ds      e ds
007400080303     d  T02mod       e                     inz('C')
007500080303     d  T02cod       e                     inz('LAT')
007600080303      *
007700080303      * - Tab. "LAT" = Livello autorità in funzioni aziendali
007800080303     d dLAT          e ds                  inz
007900070730      *
008000070730     d Status         sds           333
008100070730     d   SDSpgm          *proc
008200070731      *
008300070731     d DSFMT           ds           512
008400070731     d  £Tasto               369    369
008500070731     d  £SFLnrr              378    379b 0
008600070731      *
008700070731      * - Codice cliente ritiro
008800070731     d ds_CRO          ds            10    inz
008900070731     d  dsCRO1                        3  0 inz
009000070731     d  dsCRO2                        4  0 inz
009100070731     d  dsCRO3                        3  0 inz
009200070730      *
009300070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
009400070730      *
009500080303      * - Flags booleani
009600080303     d $InzD01         s               n   inz(*on)
009700080303     d $InzS01         s               n   inz(*on)
009800080303     d $Fine           s               n   inz(*off)
009900080303     d $EoF            s               n   inz(*off)
010000080303     d $OK             s               n   inz(*off)
010100080303     d $Giro           s               n   inz(*off)
010200080303     d $UteNoAbil      s               n   inz(*off)
010300080303     d SavIn18         s               n   inz(*off)
010400080303     d SavIn40         s               n   inz(*off)
010500070730      * - Indici di schiera
010600070730     d xx              s              3  0 inz
010700070731      * - Gestione video
010800070801     d S01nrr          s                   like(C01rcd)   inz
010900070731     d $Video          s              2    inz('D1')
011000070731     d wPag            s              4  0 inz
011100070731     d wRig            s              3  0 inz
011200070731     d W_SflPag        s              3  0 inz
011300070801      * - Comodo
011400080303     d wAbi            s                   like(UTEaut)   inz
011500070801     d W7_ACRcro       s                   like(ACRksc)   inz
011600070801     d W7_I37cro       s                   like(ACRksc)   inz
011700070801     d SavOR37ds       s                   like(FIOR37ds) inz
011800071024     d SavFGS          s                   like(V1Cfgs)   inz
011900080303     d w003a           s              3    inz
012000070730      *
012100070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
012200070801      *
012300071024      * - FNACR02L
012400071024     c     k02acr02      klist
012500071024     c                   kfld                    ACRrsr
012600071024     c                   kfld                    ACRcro
012700071030      * - FNACR13L
012800071024     c     k02acr11      klist
012900071024     c                   kfld                    ACRcro
013000071025     c                   kfld                    ACR1pocgi
013100071024      * - FNACR12L
013200071024     c     k02acr12      klist
013300071024     c                   kfld                    V1Cpocgi
013400071024     c                   kfld                    V1Ccgi
013500000927
013600070730      *---------------------------------------------------------------*
013700070730      *?RIEPILOGO INDICATORI                                         ?*
013800070730      *---------------------------------------------------------------*
013900070801      *  10    - Abilitazione tasto funzionale F10=Inserimento  - D01 *
014000070801      *  18    - Abilitazione tasto funzionale F18=Cambio FGS   - D01 *
014100070801      *  25    - Comodo                                               *
014200070801      *  28    - Emissione del messaggio di errore a video            *
014300070801      *  30    - Pulizia del subfile                            - C01 *
014400070801      *  31    - NO emissione del subfile                       - C01 *
014500070801      *  33    - Fine dati nel subfile         (sflend)         - C01 *
014600070801      *  41    - Pgm richiamato con opz. "1"                    - C01 *
014700070801      *  45    - Pgm richiamato con opz. "5" o utente di sede   - C01 *
014800070801      *  50    - Filiale in gestione errata                     - D01 *
014900070801      *  51    - Cliente ritiro      errato                     - D01 *
015000070801      *  52    - Ragione sociale     errata                     - D01 *
015100070801      *  53    - Tipo anagrafica     errato                     - D01 *
015200070801      *  54    - Frequenza ritiro    errata                     - D01 *
015300070801      *  55    - Codice PdC          errato                     - D01 *
015400070801      *  56    - Giro                errato                     - D01 *
015500070801      *  90    - Generico di errore                                   *
015600000927      *---------------------------------------------------------------*
015700070730
015800070730     c     *Entry        plist
015900070730     c                   parm                    KPJBA
016000070730      *
016100070730      * Operazioni iniziali
016200070731     c                   exsr      RoutInz
016300070731      *
016400070731      * Gestione video
016500070731do  1c                   dow       $Fine   = *off
016600070731     c     $Video        caseq     'D1'          GesD01
016700070731     c     $Video        caseq     'S1'          GesS01
016800070731     c                   endcs
016900070731e   1c                   enddo
017000070731      *
017100070731      * Operazioni finali
017200070731     c                   clear                   TIBS69ds
017300070731     c                   eval      I69tla   = 'C'
017400070731     c                   call      'TIBS69R'
017500070731     c                   parm                    TIBS69ds
017600070731     c                   movel(p)  FIOR37ds      KPJBU
017700070731     c                   eval      *inLR   = *on
017800070731
017900070731      *---------------------------------------------------------------*
018000070731      *?Operazioni Iniziali                                          ?*
018100070731      *---------------------------------------------------------------*
018200070731     c     RoutInz       BEGSR
018300070802      *
018400070802     c                   if        KPJBU   <> *blanks
018500070802     c                   movel     KPJBU         FIOR37ds
018600070802     c                   else
018700070802     c                   clear                   FIOR37ds
018800070802     c                   endif
018900070731      *
019000070731      * Pulizia campi di output
019100070731     c                   clear                   O37cro
019200070731     c                   clear                   O37err
019300070731     c                   clear                   O37msg
019400070731      *
019500070731      * Controllo parametri ricevuti
019600070731sel 1c                   SELECT
019700070731      * - Controllo inserimento filiale gestione per Immissione/Variaz.
019800070802w   1c                   WHEN          (I37opz   = '2'
019900070803     c                             or   I37opz   = '3'
020000070803     c                             or   I37opz   = '7')
020100070731     c                             and  I37fgs   = *zeros
020200070731     c                   eval      O37err   = 'E'
020300070731     c                   eval      O37msg   = $Msg(01)
020400070731     c                   eval      $Fine    = *on
020500070731     c                   leavesr
020600070731e   1c                   ENDSL
020700071031      *
020800071105      * - Forzatura filiale cliente ritiro (SE richiesta)
020900071031if  1c                   if             I37opz  <> *blanks
021000071031     c                             and  I37cro   = *zeros
021100071105     c                             and  I37rag  <> *blanks
021200071105     c                   eval      I37cro   = I37fgs * 10000000
021300071031e   1c                   endif
021400070731      *
021500070731      * Reperimento dati job
021600070731     c                   exsr      DatiJob
021700080303     c                   eval      $UteNoAbil   =  (*in50 = *on)
021800080303      * -> se rilevato errore e pgm richiamato x mod.: passa il msg.
021900080303if  1c                   if        $UteNoAbil   =  *on
022000080303     c                             and  I37opz  <> *blanks
022100080303     c                   eval      O37err   = 'E'
022200080303     c                   eval      O37msg   = V1Dmsg
022300080303     c                   eval      $Fine    = *on
022400080303     c                   leavesr
022500080303e   1c                   endif
022600070731      *
022700070731      * Impostazione nome programma a video
022800070731     c                   movel     SDSpgm        V1Tpgm
022900070801      *
023000070801      * Impostazione attributi di visualizzazione in testata
023100070801sel 1c                   select
023200070801w   1c                   when      I37opz   = *blanks
023300070801     c                   eval      DA01     = C_ND
023400070801w   1c                   when      I37opz   = '1'
023500070801     c                   eval      V1Topz   = '  Selezione   '
023600070801     c                   eval      DA01     = C_HI_RI
023700080303w   1c                   when      I37opz   = '5'  or  DUTlpo = 'S'
023800070801     c                   eval      V1Topz   = 'Interrogazione'
023900070801     c                   eval      DA01     = C_HI_RI
024000070801x   1c                   other
024100070801     c                   eval      V1Topz   = '   Gestione   '
024200070801     c                   eval      DA01     = C_HI_RI
024300070801e   1c                   endsl
024400070808      *
024500070809      * RICEVUTI PARAMETRI DI SELEZIONE (codice o rag.soc.):
024600070808if  1c                   if             I37opz  <> *blanks
024700070808     c                             and (I37cro  <> *zeros
024800070808     c                              or  I37rag  <> *blanks)
024900070809      * - inizializzazione e controllo della 1ª videasta
025000070808     c                   exsr      InzD01
025100070808     c                   eval      $InzD01  = *off
025200070808     c                   exsr      CtrD01
025300070809sel 2c                   select
025400080303w   2c                   when      $UteNoAbil  = *on
025500080303     c                   seton                                        285090
025600080303     c                   eval      V1Dmsg  = $Msg(13)
025700070809      * - > visualizzazione 1ª videata se rilevati errori
025800070809w   2c                   when      *in90
025900070810      * - > richiamo pgm. FIOR37R1 se richiesta immissione/copia da
026000070810      *     TNTA60R
026100070809w   2c                   when      I37opz   = '3'
026200070809     c                   exsr      Call_FIOR37R1
026300070810     c                   eval      $Video   = 'S1'
026400070809      * - > visualizzazione subfile altrimenti
026500070809x   2c                   other
026600070808     c                   eval      $Video   = 'S1'
026700070809e   2c                   endsl
026800070808e   2c                   endif
026900070731      *
027000070731     c                   ENDSR
027100070731
027200070731      *---------------------------------------------------------------*
027300070731      *?Reperimento Dati del job (Utente/Operativi)                  ?*
027400070731      *---------------------------------------------------------------*
027500070731     c     DatiJob       BEGSR
027600070731      *
027700070731     c     *dtaara       define    §azute        azuteds
027800070731     c     *dtaara       define    §datiute      ddatiute
027900070731      *
028000070731     c                   in(E)     *dtaara
028100070731     c                   IF        %ERROR or RSUT = *blanks
028200070731     c                   clear                   Tibs34Ds
028300070731     c                   call      'TIBS34R'
028400070731     c                   parm                    Tibs34Ds
028500070731     c                   in        *dtaara
028600070731     c                   ENDIF
028700080303      *
028800080303     c                   clear                   wAbi
028900080303     c                   clear                   dLAT
029000080303      *
029100080303      * Verifica errori e autorità profilo
029200080303s   1c                   SELECT
029300080303      * - se ho errori nei dati utente esco dal pgm
029400080303w   1c                   WHEN      DUTerr  = 'E'
029500080303     c                   seton                                        285090
029600080303     c                   eval      V1Dmsg  = $Msg(13)
029700080303     c                   leavesr
029800080303      * - se non c'è l'abilitazione
029900080303      *   -> se 1° livello, abilitazioni al terminal
030000080303      *      se 2° livello, abilitazioni al punto operativo
030100080303      *      se sede è impossibile ma se capita mando a fine il pgm
030200080303w   1c                   WHEN      UTEaut  = *blanks
030300080303if  2c                   if        DUTlpo  = '1'
030400080303     c                   eval      wAbi    = 'TP'
030500080303e   2c                   endif
030600080303if  2c                   if        DUTlpo  = '2'
030700080303     c                   eval      wAbi    = 'PO'
030800080303e   2c                   endif
030900080303if  2c                   if        DUTlpo  = 'S'
031000080303     c                   seton                                        285090
031100080303     c                   eval      V1Dmsg  = $Msg(13)
031200080303     c                   leavesr
031300080303e   2c                   endif
031400080303      * - caricamento abilitazioni del profilo
031500080303x   1c                   OTHER
031600080303     c                   movel     UTEfaf        dUTE01
031700080303if  2c                   if        §UTEcli = *blanks
031800080303     c                   eval      wAbi    = UTEaut
031900080303x   2c                   else
032000080303     c                   eval      wAbi    = §UTEcli
032100080303e   2c                   endif
032200080303e   1c                   ENDSL
032300080303      *
032400080303      * Controllo se ok l'abilitazione dell'utente
032500080303     c                   reset                   TIBS02ds
032600080303     c                   eval      T02sif  = KNSIF
032700080303     c                   eval      T02ke1  = wAbi
032800080303     c                   call      'TIBS02R'
032900080303     c                   parm                    KPJBA
033000080303     c                   parm                    TIBS02ds
033100080303if  1c                   if        T02err  = *blanks
033200080303     c                   eval      dLat    = T02uni
033300080303e   1c                   endif
033400080303      *
033500080303      * Errore o non abilitato
033600080303if  1c                   if             T02err <> *blanks
033700080303     c                             or   §LATabi = 'S'
033800080303     c                   seton                                        285090
033900080303     c                   eval      V1Dmsg  = $Msg(13)
034000080303     c                   leavesr
034100080303e   1c                   endif
034200080303      *
034300080303      * Reperimento delle fil. gestibili dall'utente per i cod.clienti
034400080303     c                   clear                   TRUL31ds
034500080303     c                   eval      I31abi  = wAbi
034600080303     c                   eval      I31cdi  = DUTdis
034700080303     c                   eval      I31car  = DUTare
034800080303     c                   eval      I31cpo  = DUTpou
034900080303     c                   call      'TRUL31R'
035000080303     c                   parm                    KPJBA
035100080303     c                   parm                    TRUL31ds
035200080303if  1c                   IF        O31pog  > *zeros
035300080303     c                   movea     O31pog        $Fgs
035400080303x   1c                   ELSE
035500080303     c                   seton                                        285090
035600080303     c                   eval      V1Dmsg  = $Msg(13)
035700080303     c                   leavesr
035800080303e   1c                   ENDIF
035900070731      *
036000070731     c                   ENDSR
036100070731
036200070731      *---------------------------------------------------------------*
036300070731      *?Gestione videata D01                                         ?*
036400070731      *---------------------------------------------------------------*
036500070731     c     GesD01        BEGSR
036600070731      *
036700070731      * Inizializzazione videata
036800070731if  1c                   if        $InzD01  = *on
036900070731     c                   exsr      InzD01
037000070731     c                   eval      $InzD01  = *off
037100070731e   1c                   endif
037200070731      *
037300070731      * Emissione testata e riga tasti funzionali abilitati
037400070731     c                   write     OR37T01
037500070731     c                   write     OR37Z01
037600070731      *
037700070731      * Emissione videata
037800070731     c                   exfmt     OR37D01
037900070731     c                   setoff                                       28  90
038000070731     c                   clear                   V1Dmsg
038100070731      *
038200070731sel 1c                   SELECT
038300070731      * F3=Fine
038400070731w   1c                   WHEN      *inKC
038500070731     c                   exsr      F03D01
038600070731      * F10=Immissione
038700070731w   1c                   WHEN      *inKJ
038800070807     c                   exsr      CtrFGS
038900070807if  2c                   if        NOT  *in90
039000070801     c                   exsr      Call_FIOR37R1
039100070807e   2c                   endif
039200070731      * F18=Cambio filiale gestione
039300070731w   1c                   WHEN      *inKS
039400070731     c                   eval      *in18    = *off
039500070810     c                   eval      *in50    = *on
039600070731      * Enter
039700070731x   1c                   OTHER
039800070731     c                   exsr      CtrD01
039900070731if  2c                   if        *in90
040000070731     c                   leavesr
040100070731e   2c                   endif
040200071025     c                   eval      *in18    = (SavIn18 = *on)
040300070731     c                   eval      $Video   = 'S1'
040400070801     c                   eval      $InzS01  = *on
040500070801      *
040600070801e   1c                   ENDSL
040700070731      *
040800070731     c                   ENDSR
040900070731
041000070731      *---------------------------------------------------------------*
041100070731      *?Inizializzazione videata D01                                 ?*
041200070731      *---------------------------------------------------------------*
041300070731     c     InzD01        BEGSR
041400070731      *
041500070731     c                   clear                   OR37D01
041600070731      *
041700070731      * Impostazione automatica della filiale di gestione
041800070731sel 1c                   select
041900070731      * - forzatura filiale gestione SE richiamato
042000070801w   1c                   when           I37opz  <> *blanks
042100070731     c                             and  I37fgs  <> *zeros
042200070731     c                   eval      V1Cfgs   = I37fgs
042300070731      * - impostazione di default per filiali di 2° livello
042400070905w   1c                   when           I37opz   = *blanks
042500070731     c                             and (DUTlpo   = '2'
042600070731     c                              or  DUTlpo   = *blanks)
042700070731     c                   eval      V1Cfgs   = DUTpou
042800070731      * - impostazione di default per filiali di 1° livello
042900070801x   1c                   other
043000070801     c                   eval      V1Cfgs   = DUTtfp
043100070731e   1c                   endsl
043200070731      *
043300070731      * Decodifica filiale di gestione
043400070731     c                   exsr      CtrFGS
043500070731      *
043600070731      * Impostazione campi restanti
043700070801if  1c                   if        I37opz   = *blanks                           NON Richiamato
043800071023     c                   eval      V1Ccd1   = V1Cfgs
043900070731     c                   eval      V1Ccgi   = *all'9'
044000070731x   1c                   else
044100070731     c                   movel     I37cro        ds_CRO
044200070731     c                   eval      V1Ccd1   = dsCRO1
044300070731     c                   eval      V1Ccd2   = dsCRO2
044400070731     c                   eval      V1Ccd3   = dsCRO3
044500070731     c                   eval      V1Crag   = I37rag
044600070731     c                   eval      V1Ctcr   = I37tcr
044700070731     c                   eval      V1Cksc   = I37ksc
044800070731     c                   eval      V1Cpocgi = I37pocgi
044900070731     c                   eval      V1Ccgi   = I37cgi
045000070731e   1c                   endif
045100070809      *
045200070809      * Posizionamento cursore sulla ragione sociale
045300070809      *   SE NON ricevuto il codice
045400070809if  1c                   if             V1Ccd1  = *zeros
045500070809     c                             and  V1Ccd2  = *zeros
045600070809     c                             and  V1Ccd3  = *zeros
045700070809     c                   eval      *in52    = *on
045800070809e   1c                   endif
045900071025      *
046000071025      * Impostazione attributi di visualizzazione
046100071025      * - Consentire modifica del campo "filiale gestione"
046200071025     c                   eval      *in40    = (DUTlpo  = '1')
046300071025     c                   eval      SavIn40  = *in40
046400070731      *
046500070731      * Abilitazione tasti funzionali:
046600070731      * - F10=Inserimento
046700070918     c                   eval      *in10    = (I37opz <> '1'      and
046800070918     c                                         I37opz <> '3'      and
046900070810     c                                         I37opz <> '5'      and
047000070809     c                                         DUTlpo <> 'S')
047100070731      * - F18=Cambio filiale gestione
047200071025     c                   eval      *in18    = (I37opz  = *blanks  or
047300071025     c                                         I37opz  = '1'      or
047400071025     c                                         DUTlpo  = 'S')
047500070803     c                   eval      SavIn18  = *in18
047600070731      *
047700070731     c                   ENDSR
047800070731
047900070731      *---------------------------------------------------------------*
048000070731      *?Controllo filiale gestione (V1CFGS)                          ?*
048100070731      *---------------------------------------------------------------*
048200070731     c     CtrFGS        BEGSR
048300070731      *
048400070731      * Controllo/Decodifica filiale di gestione
048500070731      * - 0=Tutte
048600070807if  1c                   IF        V1Cfgs   = *zeros
048700070731      *
048800070803if  2c                   if        DUTlpo   = 'S'
048900070731     c                   movel(p)  '0=Tutte'     V1Dfgs
049000070803x   2c                   else
049100070803     c                   eval      V1Dfgs   = *all'? '
049200070803     c                   seton                                        285090
049300070803     c                   eval      V1Dmsg   = $Msg(01)
049400070803     c                   leavesr
049500070803e   2c                   endif
049600070731      *
049700070803x   1c                   ELSE
049800070731      *
049900070731      * - Controllo singola filiale
050000071024if  2c                   IF        V1Cfgs  <> SAVfgs
050100080526     c                   clear                   og143
050200071024     c                   clear                   V1Dfgs
050300070731     c     V1Cfgs        chain     AZORG
050400071024if  3c                   if        NOT  %found(AZORG01L)
050500070731     c                   eval      V1Dfgs   = *all'? '
050600070731     c                   seton                                        285090
050700070731     c                   eval      V1Dmsg   = $Msg(02)
050800070803     c                   leavesr
050900071024e   3c                   endif
051000070731     c                   movel     ORGdes        V1Dfgs
051100080526     c                   movel     ORGde3        Og143
051200070920     c                   movel     ORGde7        Og147
051300070731     c                   movel     ORGde8        Og148
051400071024     c                   eval      SAVfgs   = V1Cfgs
051500071024e   2c                   ENDIF
051600071024      *
051700070803if  2c                   IF        §OGorm   = *blanks
051800070731     c                   seton                                        285090
051900070731     c                   eval      V1Dmsg   = $Msg(03)
052000070803     c                   leavesr
052100070803e   2c                   ENDIF
052200080303      *
052300080303if  2c                   IF             DUTlpo  <> 'S'
052400080303     c                             and  I37opz  <> '1'
052500080303     c                             and  I37opz  <> 'A'
052600100914      *?Aggiungo anche opzione <> '3' tanto l'unico programma che richiama il 37R
052700100914      *?con opzione '3' è il TNTA60R in immissione.
052800100914     c                             and  I37opz  <> '3'
052900110516      *?Aggiungo anche opzione <> '5' è interrogazione, da ta60 si può interrogare
053000110516      *?qualsiasi cliente, da or39 viene fatto il controllo già da lui su cosa si può
053100110516      *?interrogare
053200110516     c                             and  I37opz  <> '5'
053300080303     c                   movel     V1Cfgs        w003a
053400080303     c     w003a         lookup    $Fgs                                   25
053500080303if  3c                   if        NOT  *in25
053600080303     c                   seton                                        285090
053700080303     c                   eval      V1Dmsg   = $Msg(02)
053800080303     c                   leavesr
053900080303e   3c                   endif
054000080303e   2c                   ENDIF
054100070731      *
054200070803e   1c                   ENDIF
054300070731      *
054400070731     c                   ENDSR
054500070731
054600070731      *---------------------------------------------------------------*
054700070731      *?Controllo videata D01                                        ?*
054800070731      *---------------------------------------------------------------*
054900070731     c     CtrD01        BEGSR
055000070801      *
055100070801     c                   movea     *zeros        *in(50)
055200070731      *
055300070731      * Controllo filiale di gestione
055400070731     c                   exsr      CtrFGS
055500070731if  1c                   if        *in90
055600070731     c                   leavesr
055700070731e   1c                   endif
055800071024      *
055900071024sel 1c                   SELECT
056000071024      * Filiale del codice cliente ritiro obbligatoria
056100071024      * (se non selezionato giro)
056200071024w   1c                   WHEN           V1Ccd1   = *zeros
056300071105     c                             and  V1Cpocgi < *all'9'
056400071024     c                             and (V1Ccgi   = *blanks
056500071024     c                              or  V1Ccgi   = *all'9')
056600071024     c                   seton                                        285190
056700071024     c                   eval      V1Dmsg    = $Msg(04)
056800071024     c                   leavesr
056900070731      * Controllo frequenza ritiro
057000071024w   1c                   WHEN           V1Ctcr <> *blanks
057100070731     c                             and  V1Ctcr <> 'F'
057200070731     c                             and  V1Ctcr <> 'M'
057300070731     c                             and  V1Ctcr <> 'O'
057400070731     c                             and  V1Ctcr <> 'R'
057500070731     c                   seton                                        285490
057600070801     c                   eval      V1Dmsg    = $Msg(06)
057700070731     c                   leavesr
057800071024e   1c                   ENDSL
057900070731      *
058000070731      * Controllo codice PdC
058100070801if  1c                   IF        V1Cksc   <> *zeros
058200070731     c                   clear                   TIBS69ds
058300070801     c                   eval      I69kac    =  V1Cksc
058400070731     c                   exsr      Call_TIBS69
058500070731if  2c                   if        *in28
058600070731     c                   leavesr
058700070731e   2c                   endif
058800070731     c                   clear                   Og143
058900070731     c                   movel     V1Cksc        ORGfil
059000070731     c     ORGfil        chain     AZORG
059100070731if  2c                   if        %found(AZORG01L)
059200070801     c                   eval      Og143     = ORGde3
059300070731e   2c                   endif
059400080303if  2c                   if             §OGntw   = 'LOG'
059500070731     c                             or   §OGntw   = 'XXX'
059600070731     c                   seton                                        285590
059700070801     c                   eval      V1Dmsg    = $Msg(07)
059800070731     c                   leavesr
059900070731e   2c                   endif
060000100914      *?Controllo se filiale cliente KSC gestita da filiale gestione utente solo se
060100100914      *?I37opz <> '3'.con opzione '3' è il TNTA60R in immissione.
060200100914      *?L'unico programma che richiama il FIOR37R con opz '3' è il TNTA60R in immissione
060300100914      *?ed ora con il nuovo CRM possiamo aprire clienti anche di altre filiali
060400100914     c                   IF        I37opz  <> '3'
060500080303     c                   eval      *in25     = *off
060600080303     c                   movel     V1Cksc        w003a
060700080303     c     w003a         lookup    $Fgs                                   25
060800080303if  2c                   if        NOT  *in25
060900080303     c                   seton                                        285590
061000080303     c                   eval      V1Dmsg    = $Msg(14)
061100080303     c                   leavesr
061200080303e   2c                   endif
061300100914     c                   ENDIF
061400070731e   1c                   ENDIF
061500070731      *
061600070731      * Controllo codice giro di ritiro
061700070829if  1c                   IF             V1Ccgi <> *blanks
061800070829     c                             and  V1Ccgi <> *all'9'
061900070731      * - Ricerca codice giro (nessun controllo)
062000070731     c                   eval      xx        = %scan('?' : V1Ccgi)
062100070731if  2c                   IF        xx        > *zeros
062200070731     c                   clear                   V1Ccgi
062300070731     c                   clear                   FIDG09ds
062400070731     c                   eval      D09iop0   = 'P01'
062500070731if  3c                   if        V1Cpocgi <> *zeros
062600070731     c                   eval      D09ifgs   = V1Cpocgi
062700070731x   3c                   else
062800071025     c                   eval      D09ifgs   = V1Ccd1
062900070731e   3c                   endif
063000070731     c                   eval      D09idat   = *date
063100070810     c                   eval      D09itug   = 'R'
063200070731     c                   movel(p)  FIDG09ds      KPJBU
063300070731     c                   call      'FIDG09R'
063400070731     c                   parm                    KPJBA
063500070731     c                   movel     KPJBU         FIDG09ds
063600070731if  3c                   if             D09Of03 <> *on
063700070731     c                             and  D09Of12 <> *on
063800070731     c                             and  D09Oerr <> *on
063900070801     c                   eval      V1Cpocgi  = D09ofgs
064000070801     c                   eval      V1Ccgi    = D09ocgi
064100070731e   3c                   endif
064200070801     c                   eval      *in90     = *on
064300070731     c                   leavesr
064400070731e   2c                   ENDIF
064500070829      * - Filiale obbligatoria
064600070829if  2c                   if        V1Cpocgi  = *zeros
064700070803     c                   seton                                        285690
064800070803     c                   eval      V1Dmsg    = $Msg(08)
064900070803     c                   leavesr
065000070828e   2c                   endif
065100070731e   1c                   ENDIF
065200071029      *
065300071029      * - Controllo ambito instradamento
065400071029if  1c                   if             V1Cain <> *blanks
065500071029     c                             and  V1Cain <> '='
065600071029     c                             and  V1Cain <> '>'
065700071029     c                             and  V1Cain <> '<'
065800071029     c                   seton                                        285790
065900071029     c                   eval      V1Dmsg    = $Msg(09)
066000071029     c                   leavesr
066100071029e   1c                   endif
066200071024      *
066300071024      * Se richiesti clienti ritiro SENZA giro ritiro
066400071024      *  e NON inserita alcuna filiale di ritiro
066500071024      * => viene forzata la filiale gestione
066600070731      *
066700070731      * Se NON vi sono errori, riposiziona normalmente il cursore sul
066800070731      *   campo "Ragione Sociale"
066900070801     c                   eval      *in52    = *on
067000070731      *
067100070731     c                   ENDSR
067200070731
067300070731      *---------------------------------------------------------------*
067400070731      *?Reperimento dati anagrafici                                  ?*
067500070731      *---------------------------------------------------------------*
067600070731     c     Call_TIBS69   BEGSR
067700070731      *
067800070731     c                   clear                   ds_CNACO
067900070731     c                   clear                   ds_CNIND
068000070731     c                   clear                   ds_CNCLP
068100070731     c                   clear                   ds_FNCLS
068200070731      *
068300070731     c                   call      'TIBS69R'
068400070731     c                   parm                    TIBS69ds
068500070731     c                   parm                    ds_CNACO
068600070731     c                   parm                    ds_CNIND
068700070731     c                   parm                    ds_CNCLP
068800070731     c                   parm                    ds_FNCLS
068900070731      *
069000070731     c                   eval      *in28    = (O69err = *on)
069100070731if  1c                   if        *in28
069200070731     c                   eval      *in90    = *on
069300070731     c                   eval      V1Dmsg   = O69msg
069400070731e   1c                   endif
069500070731      *
069600070731     c                   ENDSR
069700070731
069800070731      *---------------------------------------------------------------*
069900070731      *?Gestione tasto funzionale F3 da videata D01                  ?*
070000070731      *?F3=Fine                                                      ?*
070100070731      *---------------------------------------------------------------*
070200070731     c     F03D01        BEGSR
070300070731      *
070400070809     c                   eval      O37ret   = '1'
070500070809      *
070600070731      * Chiusura del programma
070700070801     c                   eval      $Fine    = *on
070800070731      *
070900070731     c                   ENDSR
071000070731
071100070731      *---------------------------------------------------------------*
071200070731      *?Gestione videata S01                                         ?*
071300070731      *---------------------------------------------------------------*
071400070731     c     GesS01        BEGSR
071500070731      *
071600070731      * Inizializzazione videata
071700070731if  1c                   if        $InzS01  = *on
071800070731     c                   exsr      InzS01
071900070731     c                   eval      $InzS01  = *off
072000070731e   1c                   endif
072100070731      *
072200070731      * Emissione testata e riga tasti funzionali abilitati
072300081211if  1c                   if        NOT *in28
072400070731     c                   write     OR37T01
072500070731     c                   write     OR37Z01
072600081211e   1c                   endif
072700070731      *
072800070731      * Posizionamento del cursore
072900070731sel 1c                   select
073000070731w   1c                   when      S01nrr  <= *zeros
073100070731     c                   write     OR37D02
073200070731w   1c                   when      C01csr   > *zeros
073300070731     c                   z-add     C01csr        C01rcd
073400070731e   1c                   endsl
073500070731      *
073600070731      * Emissione videata
073700070731     c                   exfmt     OR37C01
073800070731     c                   z-add     £SFLnrr       C01rcd
073900070731     c                   setoff                                       28  90
074000070731     c                   clear                   V1Dmsg
074100070731      *
074200070731sel 1c                   SELECT
074300070731      * F3=Fine
074400070731w   1c                   WHEN      *inKC
074500070731     c                   exsr      F03D01
074600070731      *
074700070731      * F5=Rivisualizzazione
074800070731w   2c                   WHEN      *inKE
074900070731     c                   eval      $InzS01  = *on
075000070731      *
075100070801      * F10=Inserimento
075200070801w   2c                   WHEN      *inKJ
075300070801     c                   exsr      Call_FIOR37R1
075400070731      *
075500070731      * F12=Ritorno
075600070731w   2c                   WHEN      *inKL
075700070801     c                   exsr      F12S01
075800070731      *
075900070731      * Roll-UP
076000070731w   1c                   WHEN      £Tasto   = C_RollUp
076100070731     c                   exsr      RollUpS01
076200080409      *
076300080409      * Nessun record nel SubFile (-> nessuna opzione)
076400080409w   1c                   when      S01nrr  <= *zeros
076500070731      *
076600070731      * Controllo opzioni
076700070731x   1c                   OTHER
076800070731     c                   exsr      OpzS01
076900070731      *
077000070731e   1c                   ENDSL
077100070731      *
077200070731     c                   ENDSR
077300070731
077400070731      *---------------------------------------------------------------*
077500070731      *?Inizializzazione videata S01                                 ?*
077600070731      *---------------------------------------------------------------*
077700070731     c     InzS01        BEGSR
077800070731      *
077900070731      * Pulizia subfile
078000070731     c                   seton                                        3031
078100070731     c                   write     OR37C01
078200070731     c                   setoff                                         3133
078300070731      *
078400070731     c                   reset                   $EoF
078500070731     c                   clear                   W_SflPag
078600070731     c                   clear                   C01rcd
078700070731     c                   clear                   C01csr
078800070731     c                   clear                   S01nrr
078900070731     c                   clear                   V1Dmsg
079000070731     c                   setoff                                       28  90
079100070801     c                   movea     *zeros        *in(40)
079200070802      *
079300070802      * Abilitazione opzioni
079400070802      * - Opz. 1 = Selezione
079500070802     c                   eval      *in41    = (I37opz  = '1')
079600070802      * - Opz. 5 = Visualizzazione
079700070802     c                   eval      *in45    = (I37opz  = '5'   or
079800070802     c                                         DUTlpo  = 'S')
079900070731      *
080000070731      * Caricamento dei dati da presentare nel sfl
080100071024sel 1c                   SELECT
080200071024      * - per codice giro
080300071024w   1c                   WHEN           V1Ccgi <> *blanks
080400071024     c                             and  V1Ccgi <> *all'9'
080500071024     c     k02acr12      setll     FNACR102
080600071024     c     k02acr12      reade     FNACR102
080700071024     c                   eval      $EoF     = (%eof(FNACR12L))
080800071024      * - per codice cliente ritiro
080900071024w   1c                   WHEN      V1Crag   = *blanks
081000070731     c                   eval      dsCRO1   = V1Ccd1
081100070731     c                   eval      dsCRO2   = V1Ccd2
081200070731     c                   eval      dsCRO3   = V1Ccd3
081300070731     c                   move      ds_CRO        ACRcro
081400070801     c     ACRcro        setll     FNACR001
081500070731     c                   read      FNACR001
081600070731     c                   eval      $EoF     = (%eof(FNACR01L))
081700071024      * - per ragione sociale cliente ritiro
081800071024x   1c                   OTHER
081900070731     c                   movel(p)  V1Crag        ACRrsr
082000071024     c                   eval      dsCRO1   = V1Ccd1
082100071024     c                   eval      dsCRO2   = V1Ccd2
082200071024     c                   eval      dsCRO3   = V1Ccd3
082300071024     c                   move      ds_CRO        ACRcro
082400071024     c     k02acr02      setll     FNACR002
082500070731     c                   read      FNACR002
082600070731     c                   eval      $EoF     = (%eof(FNACR02L))
082700071024e   1c                   ENDSL
082800071024do  1c                   DOW            $EoF   = *off
082900071024     c                             and  S01nrr < C_SflPag
083000070731     c                   exsr      RollUpS01
083100070731e   1c                   ENDDO
083200070731      *
083300070731      * Impaginazione subfile
083400070731      * -> forza l'impaginazione sul 1° rec. del subfile
083500070731if  1c                   if        S01nrr   > *zeros
083600070731     c                   eval      C01rcd   = 1
083700070731     c                   eval      C01csr   = 1
083800070731x   1c                   else
083900070731     c                   clear                   C01rcd
084000070731e   1c                   endif
084100070731      *
084200070731     c                   ENDSR
084300070731
084400070731      *---------------------------------------------------------------*
084500070731      *?Inizializzazione videata S01                                 ?*
084600070731      *---------------------------------------------------------------*
084700070731     c     RollUpS01     BEGSR
084800070731      *
084900070731     c                   eval      S01nrr   = (W_SflPag * C_SflPag)
085000070731     c                   add       1             W_SflPag
085100070731     c                   eval      *in32    = *off
085200070731      *
085300070731      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
085400070731do  1c                   DOW            $EoF   = *off
085500070731     c                             and  S01nrr < (W_SflPag * C_SflPag)
085600070731      *
085700070731      * - Selezione del record dell'archivio
085800070731     c                   exsr      sr_SelRec
085900070731      *
086000070731      *   e Caricamento dati nel record del subfile
086100070731if  2c                   if        $OK      = *on
086200070731     c                   exsr      CarS01
086300070802     c                   add       1             S01nrr
086400070802     c                   write     OR37S01
086500070731e   2c                   endif
086600070731      *
086700070731      * - Lettura record successivo nell'archivio
086800070809      *   (se non rilevato stop record elaborabili in selezione)
086900070809sel 2c                   select
087000070809w   2c                   when      $EoF     = *on
087100071024w   1c                   when           V1Ccgi <> *blanks
087200071024     c                             and  V1Ccgi <> *all'9'
087300071024     c     k02acr12      reade     FNACR102
087400071024     c                   eval      $EoF     = (%eof(FNACR12L))
087500070809w   2c                   when      V1Crag   = *blanks
087600070731     c                   read      FNACR001
087700071024     c                   eval      $EoF     = (%eof(FNACR01L))
087800070809x   2c                   other
087900070731     c                   read      FNACR002
088000070731     c                   eval      $EoF     = (%eof(FNACR02L))
088100070809e   2c                   endsl
088200070731      *
088300070731e   1c                   ENDDO
088400070731      *
088500070731      * Visualizzazione del SFL (se ci sono dati)
088600070731     c                   eval      *in30    = (S01nrr <= *zeros)
088700070731      *
088800070731      * Attivazione (eventuale) del SFLEND
088900070731     c                   eval      *in33    = ($EoF = *on)
089000070731      *
089100070731      * Posizionamento cursore al 1° rcd della pagina
089200070731if  1c                   if        S01nrr   > *zeros
089300070731     c     S01nrr        div       C_SflPag      wPag
089400070731     c                   mvr                     wRig
089500070731     c                   eval      C01rcd   = wPag * C_SflPag
089600070731     c                   if        wRig     > *zeros
089700070731     c                   eval      C01rcd   = C01rcd + 1
089800070731     c                   else
089900070731     c                   eval      C01rcd   = C01rcd - C_SflPag + 1
090000070731     c                   endif
090100070731x   1c                   else
090200070731     c                   clear                   C01rcd
090300070731e   1c                   endif
090400070731     c                   eval      C01csr   = C01rcd
090500070731      *
090600070731     c                   ENDSR
090700070731
090800070731      *---------------------------------------------------------------*
090900070731      *?Selezione record in base alle selezioni                      ?*
091000070731      *---------------------------------------------------------------*
091100070731     c     sr_SelRec     BEGSR
091200070731      *
091300070731     c                   eval      $OK      = *off
091400071024      *
091500071024      * Reperimento dati da FNACR01L se lettura di FNACR12L
091600071024if  1c                   if             V1Ccgi <> *blanks
091700071024     c                             and  V1Ccgi <> *all'9'
091800071024     c                             and  ACRcro <> ACR1cro
091900071024     c     ACR1cro       chain     FNACR001
092000071024e   1c                   endif
092100070801      *
092200070801      * - Se interrogazione ("5") o manutenzione del PdC ("A")
092300130417      *   o immissione da convalida trattative
092400070801      *   si devono caricare solo i codici uguali ai primi sette byte
092500070802if  1c                   if            (I37opz  = '5'
092600130417     c                             or   I37opz  = '3'
092700070802     c                             or   I37opz  = 'A')
092800070802     c                             and  I37cro <> *zeros
092900070801     c                   movel     ACRcro        W7_ACRcro
093000070801     c                   movel     I37cro        W7_I37cro
093100070801sel 2c                   select
093200070801      * - - se minore si deve tornare a leggere
093300070801w   2c                   when      W7_ACRcro  < W7_I37cro
093400070801     c                   leavesr
093500070801      * - - se maggiore si deve uscire
093600070801w   2c                   when      W7_ACRcro  > W7_I37cro
093700070809     c                   eval      $EoF       = *on
093800070801     c                   leavesr
093900070801e   2c                   endsl
094000070801e   1c                   endif
094100070801      *
094200071031sel 1c                   select
094300071024      * Filiale codice cliente ritiro
094400150909w   1c                   when      V1Ccd1 <> *zeros and
094500150909     c                             V1Ccd1 <>
094600150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
094700071024     c                   leavesr
094800070801      * Frequenza ritiro
094900071031w   1c                   when           V1Ctcr <> *blanks
095000070801     c                             and  ACRtcr <> V1Ctcr
095100070801     c                   leavesr
095200070801      * Codice Piano dei Conti
095300070801w   1c                   when           V1Cksc <> *zeros
095400070801     c                             and  ACRksc <> V1Cksc
095500070801     c                   leavesr
095600071031      * Tipo anagrafica (NON richiesto a video)
095700071031     c                   when           I37opz <> *blanks
095800071031     c                             and  I37tac <> *blanks
095900071031     c                             and  ACRtac <> I37tac
096000071031     c                   leavesr
096100071031e   1c                   endsl
096200070731      *
096300071024      * Codice giro di ritiro
096400071024      * - se richiesto con/senza giro ("99999"): nessun controllo
096500071024      * - se richiesto    con    giro: è già stato letto FNACR12L
096600071024      * - se richiesto   senza   giri: da scartare se rilevato un giro
096700071030sel 1c                   SELECT
096800071030w   1c                   WHEN      V1Ccgi   = *all'9'
096900071030w   1c                   WHEN      V1Ccgi   = *blanks
097000071029     c                   exsr      sr_ChkCGI
097100071030if  2c                   if        $Giro    = *on
097200071029     c                   leavesr
097300071029e   2c                   endif
097400071030x   1c                   OTHER
097500071030      * Ambito instradamento giro SE lettura di FNACR12L
097600071030if  2c                   if             V1Cain <> *blanks
097700071030     c                             and  V1Cain <> ACR1ain
097800071030     c                   leavesr
097900071030e   2c                   endif
098000071030e   1c                   ENDSL
098100071029      *
098200071029      * Ambito instradamento giro SE lettura di FNACR12L
098300071029if  1c                   if            (V1Ccgi <> *blanks
098400071029     c                             and  V1Ccgi <> *all'9')
098500071029     c                             and  V1Cain <> *blanks
098600071029     c                             and  V1Cain <> ACR1ain
098700071029     c                   leavesr
098800071029e   1c                   endif
098900150909
099000150909      * Filiale ritiro diversa da cod.Cliente
099100150909     c                   IF        V1fdiff = 'S' and
099200150909     c                             ACRpoa = V1Ccd1
099300150909     c                   leavesr
099400150909     c                   ENDIF
099500070731      *
099600071030     c                   eval      $OK      = *on
099700070731      *
099800070731     c                   ENDSR
099900071029
100000071029      *---------------------------------------------------------------*
100100071029      *?Verifica/Selezione giro ritiro per cliente ritiro            ?*
100200071029      *---------------------------------------------------------------*
100300071029     c     sr_ChkCGI     BEGSR
100400071029      *
100500071030     c                   eval      $Giro    = *on
100600071029      *
100700071029      * Posizionamento iniziale per verifica esistenza giri
100800071029sel 1c                   select
100900071029w   1c                   when      V1Cpocgi  <> *zeros
101000071029     c                   eval      ACR1pocgi  = V1Cpocgi
101100071030     c     k02acr11      setll     FNACR103
101200071029w   1c                   when      DUTlpo     = '2'
101300071029     c                   eval      ACR1pocgi  = DUTpou
101400071030     c     k02acr11      setll     FNACR103
101500071029x   1c                   other
101600071030     c     ACRcro        setll     FNACR103
101700071029e   1c                   endsl
101800071029      *
101900071030if  1c                   if        NOT  %equal(FNACR13L)
102000071030     c                   eval      $Giro    = *off
102100071029     c                   leavesr
102200071029e   1c                   endif
102300071029      *
102400071029      * TROVATO ALMENO UN RECORD:
102500071029      * - Se richiesto con/senza giro ("99999"): nessun controllo
102600071029      * - Se richiesto    con    giro: è già stato letto FNACR12L
102700071029      *                                (non viene eseguita questa subr.)
102800071029      * -?Se richiesto   senza   giri: da scartare se rilevato un giro?
102900071029      *                               ?dell'ambito inserito in D01?
103000071029      *
103100071029      * Richiesto senza giri di qualsiasi ambito e trovato giro
103200071030if  1c                   IF        V1Cain   =  *blanks
103300071029     c                   leavesr
103400071029      *
103500071029      * Richiesto senza giri di un ambito specifico: ricerca ambito
103600071029x   1c                   ELSE
103700071029      *
103800071030do  2c                   DOU       %eof(FNACR13L)
103900071030sel 3c                   select
104000071030w   3c                   when      V1Cpocgi  <> *zeros
104100071030     c     k02acr11      reade     FNACR103
104200071030w   3c                   when      DUTlpo     = '2'
104300071030     c     k02acr11      reade     FNACR103
104400071030x   3c                   other
104500071030     c     ACRcro        reade     FNACR103
104600071030e   3c                   endsl
104700071030sel 3c                   select
104800071030w   3c                   when      %eof(FNACR13L)
104900071030     c                   eval      $Giro      = *off
105000071030w   3c                   when      ACR1ain    = V1Cain
105100071029     c                   leavesr
105200071030e   3c                   endsl
105300071029e   2c                   ENDDO
105400071029      *
105500071029e   1c                   ENDIF
105600071029      *
105700071029     c                   ENDSR
105800070731
105900070731      *---------------------------------------------------------------*
106000070731      *?Caricamento singolo record nel SubFile S01                   ?*
106100070731      *---------------------------------------------------------------*
106200070731     c     CarS01        BEGSR
106300070731      *
106400071024      * Ricerca eventuali giri per cliente ritiro
106500071030     c     ACRcro        setll     FNACR103
106600071024      *
106700070731     c                   clear                   OR37S01
106800070731      * Campi hidden
106900070731      * Campi di solo output
107000070801     c                   eval      S1Catb   = ACRatb
107100070801     c                   move      ACRcro        ds_CRO
107200070801     c                   eval      S1Ccd1   = dsCRO1
107300070801     c                   eval      S1Ccd2   = dsCRO2
107400070801     c                   eval      S1Ccd3   = dsCRO3
107500070801     c                   movel     ACRrsr        S1Crag
107600071030if  1c                   if        %equal(FNACR13L)
107700071030     c                   eval      S1Csng   = 'S'
107800070903e   1c                   endif
107900070801     c                   if        ACRksc  <> *zeros
108000070801     c                   move      ACRksc        S1Cksc
108100070801     c                   endif
108200070801     c                   if        ACRccc  <> 999
108300070801     c                   move      ACRccc        S1Cctr
108400070801     c                   endif
108500070801     c                   eval      S1Ctcr   = ACRtcr
108600070801      *
108700070801     c                   movel     ACRinr        S1Cind
108800070801     c                   movel     ACRlor        S1Cloc
108900070801     c                   eval      S1Cprv   = ACRprr
109000070801     c                   eval      S1Cnaz   = ACRnar
109100150909
109200150909      * Filiale ritiro solo se diversa dal codice cliente
109300150909     c                   IF        %dec(%subst(%editc(ACRcro:'X'):1:3):3:0) <>
109400150909     c                             ACRpoa
109500150909     c                   eval      S1cpoa = %editc(ACRpoa:'X')
109600150909     c                   ELSE
109700150909     c                   clear                   S1cpoa
109800150909     c                   ENDIF
109900070731      *
110000070731     c                   ENDSR
110100070801
110200070801      *---------------------------------------------------------------*
110300070801      *?Gestione tasto funzionale F12 da videata S01                 ?*
110400070801      *?F12=Ritorno                                                  ?*
110500070801      *---------------------------------------------------------------*
110600070801     c     F12S01        BEGSR
110700070801      *
110800070801      * Chiusura del programma  SE  richiamato da altro programma
110900070801     c                   if             I37opz  <> *blanks
111000070801     c                             and (I37cro  <> *zeros
111100070801     c                              or  I37rag  <> *blanks)
111200070809     c                   eval      O37ret   = '2'
111300070801     c                   eval      $Fine    = *on
111400070801      * Ritorno alla videata D01  SE  richiamato da menù
111500070801     c                   else
111600070801     c                   eval      $Video   = 'D1'
111700071025     c                   eval      *in40    = (SavIn40 = *on)
111800071025      *
111900070801     c                   endif
112000070801      *
112100070801     c                   ENDSR
112200070731
112300070731      *---------------------------------------------------------------*
112400070731      *?Controllo opzioni subfile                                    ?*
112500070731      *---------------------------------------------------------------*
112600070731     c     OpzS01        BEGSR
112700070731      *
112800070731     c                   readc     OR37S01
112900070731      *
113000070731do  1c                   DOW       NOT %eof(FIOR37D)
113100070731      *
113200070801     c                   eval      *in32    = *off
113300070801     c                   movea     *zeros        *in(50)
113400070731     c                   z-add     S01nrr        C01rcd
113500070731      *
113600070731sel 2c                   SELECT
113700070731      * - Nessuna opzione
113800070801w   2c                   WHEN      S1Copz   = *zeros
113900070801      * - 1 = Selezione
114000070801w   2c                   WHEN           S1Copz  = 1
114100070801     c                             and  I37opz  = '1'
114200070808     c                   eval      dsCRO1   = S1Ccd1
114300070808     c                   eval      dsCRO2   = S1Ccd2
114400070808     c                   eval      dsCRO3   = S1Ccd3
114500070808     c                   move      ds_CRO        O37cro
114600070808     c                   eval      $Fine    = *on
114700070808     c                   eval      *in90    = *on
114800070731      * - 2 = Modifica
114900070801w   2c                   WHEN           S1Copz  = 2
115000070801     c                             and (I37opz <> '1'
115100070801     c                             and  I37opz <> '5')
115200070802     c                             and  DUTlpo <> 'S'
115300071026if  3c                   if        S1Ccd1   = V1Cfgs
115400070801     c                   exsr      Call_FIOR37R1
115500071026x   3c                   else
115600071024     c                   exsr      Call_FIOR37R2
115700071026e   3c                   endif
115800070807      * - 3 = Copia
115900070801w   2c                   WHEN           S1Copz  = 3
116000070801     c                             and (I37opz <> '1'
116100070801     c                             and  I37opz <> '5')
116200070801     c                             and  DUTlpo <> 'S'
116300071026if  3c                   if        S1Ccd1   = V1Cfgs
116400070801     c                   exsr      Call_FIOR37R1
116500071026x   3c                   else
116600071026     c                   seton                                        285090
116700071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
116800071026     c                                      + ' per un cliente ritiro +
116900071026     c                                        di altra filiale ('+
117000071026     c                                        %trim( %editw(+
117100071026     c                                              S1Ccd1:'0   '))
117200071026     c                                      + ')'
117300071026e   3c                   endif
117400070801      * - 4 = Annullamento
117500070801w   2c                   WHEN           S1Copz  = 4
117600070801     c                             and (I37opz <> '1'
117700070801     c                             and  I37opz <> '5')
117800070802     c                             and  DUTlpo <> 'S'
117900071026sel 3c                   select
118000071026w   3c                   when      S1Catb  <> *blanks
118100070801     c                   seton                                        285090
118200070801     c                   eval      V1Dmsg   = $Msg(10)
118300071026w   3c                   when      S1Ccd1   = V1Cfgs
118400140723w   3c                   IF        S1Copz  = 4 and
118500140807w   3c                             S1Ccd3  = 0 and
118600140807     c                             S1Ccd2  <> 9999 and
118700140807     c                             S1Ccd2  <> 8888
118800140723     c                   seton                                        285090
118900140723     c                   eval      V1Dmsg   = %trimr($Msg(12))
119000140723     c                                      + ' per luogo 000'
119100140723     c                   ELSE
119200070801     c                   exsr      Call_FIOR37R1
119300140723     c                   ENDIF
119400071026x   4c                   other
119500071026     c                   seton                                        285090
119600071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
119700071026     c                                      + ' per un cliente ritiro +
119800071026     c                                        di altra filiale ('+
119900071026     c                                        %trim( %editw(+
120000071026     c                                              S1Ccd1:'0   '))
120100071026     c                                      + ')'
120200071026e   3c                   endsl
120300070731      * - 5 = Visualizzazione
120400070801w   2c                   WHEN           S1Copz  = 5
120500070801     c                   exsr      Call_FIOR37R1
120600070801      * - 7 = Ripristino
120700070801w   2c                   WHEN           S1Copz  = 7
120800070801     c                             and (I37opz <> '1'
120900070801     c                             and  I37opz <> '5')
121000070801     c                             and  DUTlpo <> 'S'
121100071026sel 3c                   select
121200071026w   3c                   when      S1Catb   = *blanks
121300070801     c                   seton                                        285090
121400070801     c                   eval      V1Dmsg   = $Msg(11)
121500071026w   3c                   when      S1Ccd1   = V1Cfgs
121600070801     c                   exsr      Call_FIOR37R1
121700071026x   3c                   other
121800071026     c                   seton                                        285090
121900071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
122000071026     c                                      + ' per un cliente ritiro +
122100071026     c                                        di altra filiale ('+
122200071026     c                                        %trim( %editw(+
122300071026     c                                              S1Ccd1:'0   '))
122400071026     c                                      + ')'
122500071026e   3c                   endsl
122600070731      * - ? = Opzione NON valida
122700070731x   2c                   OTHER
122800070731     c                   seton                                        285090
122900070801     c                   eval      V1Dmsg   = $Msg(12)
123000070731e   2c                   ENDSL
123100070731      *
123200070731      * Aggiornamento sfl
123300070731sel 2c                   select
123400070731w   2c                   when      *in28
123500070807     c                   eval      *in32    = *on
123600070731     c                   z-add     C01rcd        C01csr
123700070807w   2c                   when      *in90    = *on
123800070731     c                   z-add     C01rcd        C01csr
123900070731     c                   clear                   S1Copz
124000070731x   2c                   other
124100070926     c                   z-add     C01rcd        C01csr
124200070731     c                   clear                   S1Copz
124300070731e   2c                   endsl
124400070801     c                   UPDATE    OR37S01
124500070731if  2c                   if        *in28  OR  *in90
124600070731     c                   leave
124700070731e   2c                   endif
124800070731      *
124900070731     c                   readc     OR37S01
125000070731      *
125100070731e   1c                   ENDDO
125200070731      *
125300070731     c                   ENDSR
125400070801
125500070801      *---------------------------------------------------------------*
125600070801      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
125700070801      *---------------------------------------------------------------*
125800070801     c     Call_FIOR37R1 BEGSR
125900070801      *
126000070801      * Salvataggio area dati
126100070801     c                   movel     FIOR37ds      SavOR37ds
126200070801      *
126300070801      * Impostazione parametri
126400070801     c                   clear                   FIOR37ds
126500070801     c                   eval      I37fgs   = V1Cfgs
126600070801sel 1c                   SELECT
126700070809      * - F10 = Immissione
126800070801w   1c                   WHEN           *inKJ   = *on
126900070801     c                   eval      I37opz   = '1'
127000070809      * - I37opz = '3' (Immissione/Copia - obbligatorio!)
127100070809w   1c                   WHEN           $Video  = 'D1'
127200070809     c                             and  %subst(SavOR37ds : 1 : 1) = '3'
127300070809     c                   eval      I37opz   = 'A'
127400070809     c                   eval      dsCRO1   = V1Ccd1
127500070809     c                   eval      dsCRO2   = V1Ccd2
127600070809     c                   eval      dsCRO3   = V1Ccd3
127700070809     c                   move      ds_CRO        I37cro
127800070809     c                   eval      I37ksc   = V1Cksc
127900070801      * - 2 = Modifica
128000070801w   1c                   WHEN           $Video  = 'S1'
128100070801     c                             and  S1Copz  = 2
128200070801     c                   eval      I37opz   = '2'
128300070801     c                   eval      dsCRO1   = S1Ccd1
128400070801     c                   eval      dsCRO2   = S1Ccd2
128500070801     c                   eval      dsCRO3   = S1Ccd3
128600070801     c                   move      ds_CRO        I37cro
128700070801      * - 3 = Copia (non in sede)
128800070801w   1c                   WHEN           $Video  = 'S1'
128900070801     c                             and  S1Copz  = 3
129000070801     c                   eval      I37opz   = '3'
129100070801     c                   eval      dsCRO1   = S1Ccd1
129200070801     c                   eval      dsCRO2   = S1Ccd2
129300070801     c                   eval      dsCRO3   = S1Ccd3
129400070801     c                   move      ds_CRO        I37cro
129500070801      * - 4 = Annullamento
129600070801w   1c                   WHEN           $Video  = 'S1'
129700070801     c                             and  S1Copz  = 4
129800070801     c                   eval      I37opz   = '4'
129900070801     c                   eval      dsCRO1   = S1Ccd1
130000070801     c                   eval      dsCRO2   = S1Ccd2
130100070801     c                   eval      dsCRO3   = S1Ccd3
130200070801     c                   move      ds_CRO        I37cro
130300070801      * - 5 = Visualizzazione
130400070801w   1c                   WHEN           $Video  = 'S1'
130500070801     c                             and  S1Copz  = 5
130600070801     c                   eval      I37opz   = '5'
130700070801     c                   eval      dsCRO1   = S1Ccd1
130800070801     c                   eval      dsCRO2   = S1Ccd2
130900070801     c                   eval      dsCRO3   = S1Ccd3
131000070801     c                   move      ds_CRO        I37cro
131100070801      * - 7 = Ripristino
131200070801w   1c                   WHEN           $Video  = 'S1'
131300070801     c                             and  S1Copz  = 7
131400070801     c                   eval      I37opz   = '7'
131500070801     c                   eval      dsCRO1   = S1Ccd1
131600070801     c                   eval      dsCRO2   = S1Ccd2
131700070801     c                   eval      dsCRO3   = S1Ccd3
131800070801     c                   move      ds_CRO        I37cro
131900070801e   1c                   ENDSL
132000070801      *
132100070801     c                   movel(p)  FIOR37ds      KPJBU
132200070801     c                   call      'FIOR37R1'
132300070801     c                   parm                    KPJBA
132400070801     c                   movel     KPJBU         FIOR37ds
132500070801      *
132600070801sel 1c                   SELECT
132700070801      * - Errore
132800070801w   1c                   WHEN      O37err   = 'E'
132900070801     c                   eval      V1Dmsg   = O37msg
133000070801     c                   seton                                        28  90
133100070802      * - F3=Fine
133200070803w   1c                   WHEN      O37ret   = '1'
133300070809if  2c                   if             S1Copz  = 5
133400070809     c                             and  S01nrr  = 1
133500070809     c                   eval      $Fine    = *on
133600070809e   2c                   endif
133700070806     c                   eval      *in90    = *on
133800070803      * - F12=Ritorno
133900070803w   1c                   WHEN      O37ret   = '2'
134000070801      * - Inserito record
134100070803w   1c                   WHEN           O37ret  = *off
134200070803     c                             and  O37cro  > *zeros
134300070801     c                   eval      $InzS01  = *on
134400070802      * - Modificato record
134500070802w   1c                   OTHER
134600071024     c     I37cro        chain     FNACR001
134700070802if  2c                   if        %found(FNACR01L)
134800070802     c                   exsr      CarS01
134900070802e   2c                   endif
135000070801e   1c                   ENDSL
135100070801      *
135200070801      * Ripristino area dati
135300070801     c                   movel     SavOR37ds     FIOR37ds
135400070801      *
135500070801     c                   ENDSR
135600071023
135700071023      *---------------------------------------------------------------*
135800071023      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
135900071023      *---------------------------------------------------------------*
136000071024     c     Call_FIOR37R2 BEGSR
136100071023      *
136200071023      * Salvataggio area dati
136300071023     c                   movel     FIOR37ds      SavOR37ds
136400071023      *
136500071023      * Impostazione parametri
136600071024     c                   clear                   FIOR37ds
136700071024     c                   eval      I37fgs   = V1Cfgs
136800071023     c                   eval      dsCRO1   = S1Ccd1
136900071023     c                   eval      dsCRO2   = S1Ccd2
137000071023     c                   eval      dsCRO3   = S1Ccd3
137100071024     c                   move      ds_CRO        I37cro
137200071024     c                   eval      I37pocgi = V1Cfgs
137300071023      *
137400071024     c                   movel(p)  FIOR37ds      KPJBU
137500071024     c                   call      'FIOR37R2'
137600071023     c                   parm                    KPJBA
137700071024     c                   movel     KPJBU         FIOR37ds
137800071023      *
137900071023sel 1c                   SELECT
138000071023      * - Errore
138100071024w   1c                   WHEN      O37err   = 'E'
138200071023     c                   eval      V1Dmsg   = O37msg
138300071023     c                   seton                                        28  90
138400071023      * - F3=Fine
138500071024w   1c                   WHEN      O37ret   = '1'
138600071023if  2c                   if             S1Copz  = 5
138700071023     c                             and  S01nrr  = 1
138800071023     c                   eval      $Fine    = *on
138900071023e   2c                   endif
139000071023     c                   eval      *in90    = *on
139100071023      * - F12=Ritorno
139200071024w   1c                   WHEN      O37ret   = '2'
139300071024      * - Inserito/Modificato/Eliminato giro ritiro
139400071024w   1c                   OTHER
139500071024     c     I37cro        chain     FNACR001
139600071024if  2c                   if        %found(FNACR01L)
139700071024     c                   exsr      CarS01
139800071024e   2c                   endif
139900071024e   1c                   ENDSL
140000071023      *
140100071023      * Ripristino area dati
140200071023     c                   movel     SavOR37ds     FIOR37ds
140300071023      *
140400071023     c                   ENDSR
140500000929
140600070730** - $MSG -------------------------------------------------------------------*
140700070801Elaborazione non possibile: manca la filiale gestione                           1
140800070801Filiale gestione errata                                                         2
140900070801Procedura ORM non attivata                                                      3
141000071024Filiale del codice cliente ritiro obbligatoria                                  4
141100070801Tipo anagrafica errato                                                          5
141200070801Frequenza ritiro errata                                                         6
141300070801Codice cliente piano dei conti errato                                           7
141400070801Filiale obbligatoria per codice giro diverso da '9999999999'                    8
141500071029Ambito instradamento errato                                                     9
141600070801Record annullato. Consentito solo il ripristino                                10
141700070801Record non annullato. Impossibile ripristinare                                 11
141800070801Opzione errata                                                                 12
141900080303Utente non autorizzato alla Gestione anagrafico clienti                        13
142000080303Cliente NON gestibile dall'utente                                              14
