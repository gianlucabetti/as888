000100070730      * FIOR37R *-----------------------------------------------------*
000200070730      *?GESTIONE ANAGRAFICA CLIENTI RITIRO (CON GIRO)                ?*
000300070730      *---------------------------------------------------------------*
000400070730
000500070731     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600070730
000700070730      *---------------------------------------------------------------*
000800070731     fAZORG01L  if   e           k disk
000900070801      *
001000070801     fFNACR01L  if   e           k disk    rename(FNACR000:FNACR001)
001100070730     fFNACR02L  if   e           k disk    rename(FNACR000:FNACR002)
001200071030     fFNACR13L  if   e           k disk    rename(FNACR100:FNACR103)
001300071024     fFNACR12L  if   e           k disk    rename(FNACR100:FNACR102)
001400170131     fFNACRE1L  if   e           k disk
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
003500170130      * - Per trasformare da maiuscolo a minuscolo
003600170130     d up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003700170130     d lo              c                   const('abcdefghijklmnopqrstuvwxyz')
003800070730      *
003900070730      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
004000070730      *
004100070730      * - Messaggi di errore
004200080303     d $Msg            s             78    dim(14) ctdata perrcd(1)             MSG VIDEO
004300080303      * - Filiali gestibili dall'utente
004400080303     d $Fgs            s              3    dim(250)       inz
004500070730      *
004600070730      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
004700070730      *
004800070730      * - Parametri
004900070730     d KPJBA         e ds
005000070730     d FIOR37ds      e ds                  inz
005100070730      *
005200070731      * - Reperimento dati utente
005300060203     d TIBS34ds      e ds
005400060203     d AZUTEds       e ds                  extname(AZUTE00F)
005500070731     d dDatiUte      e ds
005600080303     d dUte01        e ds                  inz
005700070730      *
005800080303      * - Caricamento schiera filiali gestibili
005900080303     d TRUL31ds      e ds                  inz
006000070731      *
006100070731      * - Lettura anagrafiche cliente
006200070731     d TIBS69ds      e ds                  inz
006300070731     d ds_CNACO      e ds                  inz extname(CNACO00F)
006400070731     d ds_CNIND      e ds                  inz extname(CNIND00F)
006500070731     d ds_CNCLP      e ds                  inz extname(CNCLP00F)
006600070731     d ds_FNCLS      e ds                  inz extname(FNCLS00F)
006700060203      *
006800070730      * - Gestione anagrafica giri
006900070730     d FIDG09ds      e ds                  inz
007000070730      *
007100070731     d Og148         e ds                  inz
007200070920     d Og147         e ds                  inz
007300070731     d Og143         e ds                  inz
007400080303      *
007500080303      * - Lettura tabelle
007600080303     d TIBS02ds      e ds
007700080303     d  T02mod       e                     inz('C')
007800080303     d  T02cod       e                     inz('LAT')
007900080303      *
008000080303      * - Tab. "LAT" = Livello autorità in funzioni aziendali
008100080303     d dLAT          e ds                  inz
008200070730      *
008300070730     d Status         sds           333
008400070730     d   SDSpgm          *proc
008500070731      *
008600070731     d DSFMT           ds           512
008700070731     d  £Tasto               369    369
008800070731     d  £SFLnrr              378    379b 0
008900070731      *
009000070731      * - Codice cliente ritiro
009100070731     d ds_CRO          ds            10    inz
009200070731     d  dsCRO1                        3  0 inz
009300070731     d  dsCRO2                        4  0 inz
009400070731     d  dsCRO3                        3  0 inz
009500070730      *
009600070730      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
009700070730      *
009800080303      * - Flags booleani
009900080303     d $InzD01         s               n   inz(*on)
010000080303     d $InzS01         s               n   inz(*on)
010100080303     d $Fine           s               n   inz(*off)
010200080303     d $EoF            s               n   inz(*off)
010300080303     d $OK             s               n   inz(*off)
010400080303     d $Giro           s               n   inz(*off)
010500080303     d $UteNoAbil      s               n   inz(*off)
010600080303     d SavIn18         s               n   inz(*off)
010700080303     d SavIn40         s               n   inz(*off)
010800170131     d SqlMail         s               n   inz(*off)
010900170131     d SqlSms          s               n   inz(*off)
011000070730      * - Indici di schiera
011100070730     d xx              s              3  0 inz
011200070731      * - Gestione video
011300070801     d S01nrr          s                   like(C01rcd)   inz
011400070731     d $Video          s              2    inz('D1')
011500070731     d wPag            s              4  0 inz
011600070731     d wRig            s              3  0 inz
011700070731     d W_SflPag        s              3  0 inz
011800070801      * - Comodo
011900080303     d wAbi            s                   like(UTEaut)   inz
012000070801     d W7_ACRcro       s                   like(ACRksc)   inz
012100070801     d W7_I37cro       s                   like(ACRksc)   inz
012200151013     d SavACRcro       s                   like(ACRcro)   inz
012300070801     d SavOR37ds       s                   like(FIOR37ds) inz
012400071024     d SavFGS          s                   like(V1Cfgs)   inz
012500170130     d SavV1mail       s                   like(V1mail)   inz
012600080303     d w003a           s              3    inz
012700070730      *
012800070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
012900070801      *
013000071024      * - FNACR02L
013100071024     c     k02acr02      klist
013200071024     c                   kfld                    ACRrsr
013300071024     c                   kfld                    ACRcro
013400071030      * - FNACR13L
013500071024     c     k02acr11      klist
013600071024     c                   kfld                    ACRcro
013700071025     c                   kfld                    ACR1pocgi
013800071024      * - FNACR12L
013900071024     c     k02acr12      klist
014000071024     c                   kfld                    V1Cpocgi
014100071024     c                   kfld                    V1Ccgi
014200000927
014300070730      *---------------------------------------------------------------*
014400070730      *?RIEPILOGO INDICATORI                                         ?*
014500070730      *---------------------------------------------------------------*
014600070801      *  10    - Abilitazione tasto funzionale F10=Inserimento  - D01 *
014700070801      *  18    - Abilitazione tasto funzionale F18=Cambio FGS   - D01 *
014800070801      *  25    - Comodo                                               *
014900070801      *  28    - Emissione del messaggio di errore a video            *
015000070801      *  30    - Pulizia del subfile                            - C01 *
015100070801      *  31    - NO emissione del subfile                       - C01 *
015200070801      *  33    - Fine dati nel subfile         (sflend)         - C01 *
015300070801      *  41    - Pgm richiamato con opz. "1"                    - C01 *
015400070801      *  45    - Pgm richiamato con opz. "5" o utente di sede   - C01 *
015500070801      *  50    - Filiale in gestione errata                     - D01 *
015600070801      *  51    - Cliente ritiro      errato                     - D01 *
015700070801      *  52    - Ragione sociale     errata                     - D01 *
015800070801      *  53    - Tipo anagrafica     errato                     - D01 *
015900070801      *  54    - Frequenza ritiro    errata                     - D01 *
016000070801      *  55    - Codice PdC          errato                     - D01 *
016100070801      *  56    - Giro                errato                     - D01 *
016200070801      *  90    - Generico di errore                                   *
016300000927      *---------------------------------------------------------------*
016400070730
016500070730     c     *Entry        plist
016600070730     c                   parm                    KPJBA
016700070730      *
016800070730      * Operazioni iniziali
016900070731     c                   exsr      RoutInz
017000070731      *
017100070731      * Gestione video
017200070731do  1c                   dow       $Fine   = *off
017300070731     c     $Video        caseq     'D1'          GesD01
017400070731     c     $Video        caseq     'S1'          GesS01
017500070731     c                   endcs
017600070731e   1c                   enddo
017700070731      *
017800070731      * Operazioni finali
017900070731     c                   clear                   TIBS69ds
018000070731     c                   eval      I69tla   = 'C'
018100070731     c                   call      'TIBS69R'
018200070731     c                   parm                    TIBS69ds
018300070731     c                   movel(p)  FIOR37ds      KPJBU
018400070731     c                   eval      *inLR   = *on
018500070731
018600070731      *---------------------------------------------------------------*
018700070731      *?Operazioni Iniziali                                          ?*
018800070731      *---------------------------------------------------------------*
018900070731     c     RoutInz       BEGSR
019000070802      *
019100070802     c                   if        KPJBU   <> *blanks
019200070802     c                   movel     KPJBU         FIOR37ds
019300070802     c                   else
019400070802     c                   clear                   FIOR37ds
019500070802     c                   endif
019600070731      *
019700070731      * Pulizia campi di output
019800070731     c                   clear                   O37cro
019900070731     c                   clear                   O37err
020000070731     c                   clear                   O37msg
020100070731      *
020200070731      * Controllo parametri ricevuti
020300070731sel 1c                   SELECT
020400070731      * - Controllo inserimento filiale gestione per Immissione/Variaz.
020500070802w   1c                   WHEN          (I37opz   = '2'
020600070803     c                             or   I37opz   = '3'
020700070803     c                             or   I37opz   = '7')
020800070731     c                             and  I37fgs   = *zeros
020900070731     c                   eval      O37err   = 'E'
021000070731     c                   eval      O37msg   = $Msg(01)
021100070731     c                   eval      $Fine    = *on
021200070731     c                   leavesr
021300070731e   1c                   ENDSL
021400071031      *
021500071105      * - Forzatura filiale cliente ritiro (SE richiesta)
021600071031if  1c                   if             I37opz  <> *blanks
021700071031     c                             and  I37cro   = *zeros
021800071105     c                             and  I37rag  <> *blanks
021900071105     c                   eval      I37cro   = I37fgs * 10000000
022000071031e   1c                   endif
022100070731      *
022200070731      * Reperimento dati job
022300070731     c                   exsr      DatiJob
022400080303     c                   eval      $UteNoAbil   =  (*in50 = *on)
022500080303      * -> se rilevato errore e pgm richiamato x mod.: passa il msg.
022600080303if  1c                   if        $UteNoAbil   =  *on
022700080303     c                             and  I37opz  <> *blanks
022800080303     c                   eval      O37err   = 'E'
022900080303     c                   eval      O37msg   = V1Dmsg
023000080303     c                   eval      $Fine    = *on
023100080303     c                   leavesr
023200080303e   1c                   endif
023300070731      *
023400070731      * Impostazione nome programma a video
023500070731     c                   movel     SDSpgm        V1Tpgm
023600070801      *
023700070801      * Impostazione attributi di visualizzazione in testata
023800070801sel 1c                   select
023900070801w   1c                   when      I37opz   = *blanks
024000070801     c                   eval      DA01     = C_ND
024100070801w   1c                   when      I37opz   = '1'
024200070801     c                   eval      V1Topz   = '  Selezione   '
024300070801     c                   eval      DA01     = C_HI_RI
024400080303w   1c                   when      I37opz   = '5'  or  DUTlpo = 'S'
024500070801     c                   eval      V1Topz   = 'Interrogazione'
024600070801     c                   eval      DA01     = C_HI_RI
024700070801x   1c                   other
024800070801     c                   eval      V1Topz   = '   Gestione   '
024900070801     c                   eval      DA01     = C_HI_RI
025000070801e   1c                   endsl
025100070808      *
025200070809      * RICEVUTI PARAMETRI DI SELEZIONE (codice o rag.soc.):
025300070808if  1c                   if             I37opz  <> *blanks
025400070808     c                             and (I37cro  <> *zeros
025500070808     c                              or  I37rag  <> *blanks)
025600070809      * - inizializzazione e controllo della 1ª videasta
025700070808     c                   exsr      InzD01
025800070808     c                   eval      $InzD01  = *off
025900070808     c                   exsr      CtrD01
026000070809sel 2c                   select
026100080303w   2c                   when      $UteNoAbil  = *on
026200080303     c                   seton                                        285090
026300080303     c                   eval      V1Dmsg  = $Msg(13)
026400070809      * - > visualizzazione 1ª videata se rilevati errori
026500070809w   2c                   when      *in90
026600070810      * - > richiamo pgm. FIOR37R1 se richiesta immissione/copia da
026700070810      *     TNTA60R
026800070809w   2c                   when      I37opz   = '3'
026900070809     c                   exsr      Call_FIOR37R1
027000070810     c                   eval      $Video   = 'S1'
027100070809      * - > visualizzazione subfile altrimenti
027200070809x   2c                   other
027300070808     c                   eval      $Video   = 'S1'
027400070809e   2c                   endsl
027500070808e   2c                   endif
027600070731      *
027700070731     c                   ENDSR
027800070731
027900070731      *---------------------------------------------------------------*
028000070731      *?Reperimento Dati del job (Utente/Operativi)                  ?*
028100070731      *---------------------------------------------------------------*
028200070731     c     DatiJob       BEGSR
028300070731      *
028400070731     c     *dtaara       define    §azute        azuteds
028500070731     c     *dtaara       define    §datiute      ddatiute
028600070731      *
028700070731     c                   in(E)     *dtaara
028800070731     c                   IF        %ERROR or RSUT = *blanks
028900070731     c                   clear                   Tibs34Ds
029000070731     c                   call      'TIBS34R'
029100070731     c                   parm                    Tibs34Ds
029200070731     c                   in        *dtaara
029300070731     c                   ENDIF
029400080303      *
029500080303     c                   clear                   wAbi
029600080303     c                   clear                   dLAT
029700080303      *
029800080303      * Verifica errori e autorità profilo
029900080303s   1c                   SELECT
030000080303      * - se ho errori nei dati utente esco dal pgm
030100080303w   1c                   WHEN      DUTerr  = 'E'
030200080303     c                   seton                                        285090
030300080303     c                   eval      V1Dmsg  = $Msg(13)
030400080303     c                   leavesr
030500080303      * - se non c'è l'abilitazione
030600080303      *   -> se 1° livello, abilitazioni al terminal
030700080303      *      se 2° livello, abilitazioni al punto operativo
030800080303      *      se sede è impossibile ma se capita mando a fine il pgm
030900080303w   1c                   WHEN      UTEaut  = *blanks
031000080303if  2c                   if        DUTlpo  = '1'
031100080303     c                   eval      wAbi    = 'TP'
031200080303e   2c                   endif
031300080303if  2c                   if        DUTlpo  = '2'
031400080303     c                   eval      wAbi    = 'PO'
031500080303e   2c                   endif
031600080303if  2c                   if        DUTlpo  = 'S'
031700080303     c                   seton                                        285090
031800080303     c                   eval      V1Dmsg  = $Msg(13)
031900080303     c                   leavesr
032000080303e   2c                   endif
032100080303      * - caricamento abilitazioni del profilo
032200080303x   1c                   OTHER
032300080303     c                   movel     UTEfaf        dUTE01
032400080303if  2c                   if        §UTEcli = *blanks
032500080303     c                   eval      wAbi    = UTEaut
032600080303x   2c                   else
032700080303     c                   eval      wAbi    = §UTEcli
032800080303e   2c                   endif
032900080303e   1c                   ENDSL
033000080303      *
033100080303      * Controllo se ok l'abilitazione dell'utente
033200080303     c                   reset                   TIBS02ds
033300080303     c                   eval      T02sif  = KNSIF
033400080303     c                   eval      T02ke1  = wAbi
033500080303     c                   call      'TIBS02R'
033600080303     c                   parm                    KPJBA
033700080303     c                   parm                    TIBS02ds
033800080303if  1c                   if        T02err  = *blanks
033900080303     c                   eval      dLat    = T02uni
034000080303e   1c                   endif
034100080303      *
034200080303      * Errore o non abilitato
034300080303if  1c                   if             T02err <> *blanks
034400080303     c                             or   §LATabi = 'S'
034500080303     c                   seton                                        285090
034600080303     c                   eval      V1Dmsg  = $Msg(13)
034700080303     c                   leavesr
034800080303e   1c                   endif
034900080303      *
035000080303      * Reperimento delle fil. gestibili dall'utente per i cod.clienti
035100080303     c                   clear                   TRUL31ds
035200080303     c                   eval      I31abi  = wAbi
035300080303     c                   eval      I31cdi  = DUTdis
035400080303     c                   eval      I31car  = DUTare
035500080303     c                   eval      I31cpo  = DUTpou
035600080303     c                   call      'TRUL31R'
035700080303     c                   parm                    KPJBA
035800080303     c                   parm                    TRUL31ds
035900080303if  1c                   IF        O31pog  > *zeros
036000080303     c                   movea     O31pog        $Fgs
036100080303x   1c                   ELSE
036200080303     c                   seton                                        285090
036300080303     c                   eval      V1Dmsg  = $Msg(13)
036400080303     c                   leavesr
036500080303e   1c                   ENDIF
036600070731      *
036700070731     c                   ENDSR
036800070731
036900070731      *---------------------------------------------------------------*
037000070731      *?Gestione videata D01                                         ?*
037100070731      *---------------------------------------------------------------*
037200070731     c     GesD01        BEGSR
037300070731      *
037400070731      * Inizializzazione videata
037500070731if  1c                   if        $InzD01  = *on
037600070731     c                   exsr      InzD01
037700070731     c                   eval      $InzD01  = *off
037800070731e   1c                   endif
037900070731      *
038000070731      * Emissione testata e riga tasti funzionali abilitati
038100070731     c                   write     OR37T01
038200070731     c                   write     OR37Z01
038300070731      *
038400070731      * Emissione videata
038500070731     c                   exfmt     OR37D01
038600070731     c                   setoff                                       28  90
038700070731     c                   clear                   V1Dmsg
038800070731      *
038900070731sel 1c                   SELECT
039000070731      * F3=Fine
039100070731w   1c                   WHEN      *inKC
039200070731     c                   exsr      F03D01
039300070731      * F10=Immissione
039400070731w   1c                   WHEN      *inKJ
039500070807     c                   exsr      CtrFGS
039600070807if  2c                   if        NOT  *in90
039700070801     c                   exsr      Call_FIOR37R1
039800070807e   2c                   endif
039900070731      * F18=Cambio filiale gestione
040000070731w   1c                   WHEN      *inKS
040100070731     c                   eval      *in18    = *off
040200070810     c                   eval      *in50    = *on
040300070731      * Enter
040400070731x   1c                   OTHER
040500070731     c                   exsr      CtrD01
040600070731if  2c                   if        *in90
040700070731     c                   leavesr
040800070731e   2c                   endif
040900071025     c                   eval      *in18    = (SavIn18 = *on)
041000070731     c                   eval      $Video   = 'S1'
041100070801     c                   eval      $InzS01  = *on
041200070801      *
041300070801e   1c                   ENDSL
041400070731      *
041500070731     c                   ENDSR
041600070731
041700070731      *---------------------------------------------------------------*
041800070731      *?Inizializzazione videata D01                                 ?*
041900070731      *---------------------------------------------------------------*
042000070731     c     InzD01        BEGSR
042100070731      *
042200070731     c                   clear                   OR37D01
042300070731      *
042400070731      * Impostazione automatica della filiale di gestione
042500070731sel 1c                   select
042600070731      * - forzatura filiale gestione SE richiamato
042700070801w   1c                   when           I37opz  <> *blanks
042800070731     c                             and  I37fgs  <> *zeros
042900070731     c                   eval      V1Cfgs   = I37fgs
043000070731      * - impostazione di default per filiali di 2° livello
043100070905w   1c                   when           I37opz   = *blanks
043200070731     c                             and (DUTlpo   = '2'
043300070731     c                              or  DUTlpo   = *blanks)
043400070731     c                   eval      V1Cfgs   = DUTpou
043500070731      * - impostazione di default per filiali di 1° livello
043600070801x   1c                   other
043700070801     c                   eval      V1Cfgs   = DUTtfp
043800070731e   1c                   endsl
043900070731      *
044000070731      * Decodifica filiale di gestione
044100070731     c                   exsr      CtrFGS
044200070731      *
044300070731      * Impostazione campi restanti
044400070801if  1c                   if        I37opz   = *blanks                           NON Richiamato
044500071023     c                   eval      V1Ccd1   = V1Cfgs
044600070731     c                   eval      V1Ccgi   = *all'9'
044700070731x   1c                   else
044800070731     c                   movel     I37cro        ds_CRO
044900070731     c                   eval      V1Ccd1   = dsCRO1
045000070731     c                   eval      V1Ccd2   = dsCRO2
045100070731     c                   eval      V1Ccd3   = dsCRO3
045200070731     c                   eval      V1Crag   = I37rag
045300070731     c                   eval      V1Ctcr   = I37tcr
045400070731     c                   eval      V1Cksc   = I37ksc
045500070731     c                   eval      V1Cpocgi = I37pocgi
045600070731     c                   eval      V1Ccgi   = I37cgi
045700070731e   1c                   endif
045800070809      *
045900070809      * Posizionamento cursore sulla ragione sociale
046000070809      *   SE NON ricevuto il codice
046100070809if  1c                   if             V1Ccd1  = *zeros
046200070809     c                             and  V1Ccd2  = *zeros
046300070809     c                             and  V1Ccd3  = *zeros
046400070809     c                   eval      *in52    = *on
046500070809e   1c                   endif
046600071025      *
046700071025      * Impostazione attributi di visualizzazione
046800071025      * - Consentire modifica del campo "filiale gestione"
046900071025     c                   eval      *in40    = (DUTlpo  = '1')
047000071025     c                   eval      SavIn40  = *in40
047100070731      *
047200070731      * Abilitazione tasti funzionali:
047300070731      * - F10=Inserimento
047400070918     c                   eval      *in10    = (I37opz <> '1'      and
047500070918     c                                         I37opz <> '3'      and
047600070810     c                                         I37opz <> '5'      and
047700070809     c                                         DUTlpo <> 'S')
047800070731      * - F18=Cambio filiale gestione
047900071025     c                   eval      *in18    = (I37opz  = *blanks  or
048000071025     c                                         I37opz  = '1'      or
048100071025     c                                         DUTlpo  = 'S')
048200070803     c                   eval      SavIn18  = *in18
048300070731      *
048400070731     c                   ENDSR
048500070731
048600070731      *---------------------------------------------------------------*
048700070731      *?Controllo filiale gestione (V1CFGS)                          ?*
048800070731      *---------------------------------------------------------------*
048900070731     c     CtrFGS        BEGSR
049000070731      *
049100070731      * Controllo/Decodifica filiale di gestione
049200070731      * - 0=Tutte
049300070807if  1c                   IF        V1Cfgs   = *zeros
049400070731      *
049500070803if  2c                   if        DUTlpo   = 'S'
049600070731     c                   movel(p)  '0=Tutte'     V1Dfgs
049700070803x   2c                   else
049800070803     c                   eval      V1Dfgs   = *all'? '
049900070803     c                   seton                                        285090
050000070803     c                   eval      V1Dmsg   = $Msg(01)
050100070803     c                   leavesr
050200070803e   2c                   endif
050300070731      *
050400070803x   1c                   ELSE
050500070731      *
050600070731      * - Controllo singola filiale
050700071024if  2c                   IF        V1Cfgs  <> SAVfgs
050800080526     c                   clear                   og143
050900071024     c                   clear                   V1Dfgs
051000070731     c     V1Cfgs        chain     AZORG
051100071024if  3c                   if        NOT  %found(AZORG01L)
051200070731     c                   eval      V1Dfgs   = *all'? '
051300070731     c                   seton                                        285090
051400070731     c                   eval      V1Dmsg   = $Msg(02)
051500070803     c                   leavesr
051600071024e   3c                   endif
051700070731     c                   movel     ORGdes        V1Dfgs
051800080526     c                   movel     ORGde3        Og143
051900070920     c                   movel     ORGde7        Og147
052000070731     c                   movel     ORGde8        Og148
052100071024     c                   eval      SAVfgs   = V1Cfgs
052200071024e   2c                   ENDIF
052300071024      *
052400070803if  2c                   IF        §OGorm   = *blanks
052500070731     c                   seton                                        285090
052600070731     c                   eval      V1Dmsg   = $Msg(03)
052700070803     c                   leavesr
052800070803e   2c                   ENDIF
052900080303      *
053000080303if  2c                   IF             DUTlpo  <> 'S'
053100080303     c                             and  I37opz  <> '1'
053200080303     c                             and  I37opz  <> 'A'
053300100914      *?Aggiungo anche opzione <> '3' tanto l'unico programma che richiama il 37R
053400100914      *?con opzione '3' è il TNTA60R in immissione.
053500100914     c                             and  I37opz  <> '3'
053600110516      *?Aggiungo anche opzione <> '5' è interrogazione, da ta60 si può interrogare
053700110516      *?qualsiasi cliente, da or39 viene fatto il controllo già da lui su cosa si può
053800110516      *?interrogare
053900110516     c                             and  I37opz  <> '5'
054000080303     c                   movel     V1Cfgs        w003a
054100080303     c     w003a         lookup    $Fgs                                   25
054200080303if  3c                   if        NOT  *in25
054300080303     c                   seton                                        285090
054400080303     c                   eval      V1Dmsg   = $Msg(02)
054500080303     c                   leavesr
054600080303e   3c                   endif
054700080303e   2c                   ENDIF
054800070731      *
054900070803e   1c                   ENDIF
055000070731      *
055100070731     c                   ENDSR
055200070731
055300070731      *---------------------------------------------------------------*
055400070731      *?Controllo videata D01                                        ?*
055500070731      *---------------------------------------------------------------*
055600070731     c     CtrD01        BEGSR
055700070801      *
055800070801     c                   movea     *zeros        *in(50)
055900070731      *
056000070731      * Controllo filiale di gestione
056100070731     c                   exsr      CtrFGS
056200070731if  1c                   if        *in90
056300070731     c                   leavesr
056400070731e   1c                   endif
056500071024      *
056600071024sel 1c                   SELECT
056700071024      * Filiale del codice cliente ritiro obbligatoria
056800071024      * (se non selezionato giro)
056900071024w   1c                   WHEN           V1Ccd1   = *zeros
057000071105     c                             and  V1Cpocgi < *all'9'
057100071024     c                             and (V1Ccgi   = *blanks
057200071024     c                              or  V1Ccgi   = *all'9')
057300071024     c                   seton                                        285190
057400071024     c                   eval      V1Dmsg    = $Msg(04)
057500071024     c                   leavesr
057600070731      * Controllo frequenza ritiro
057700071024w   1c                   WHEN           V1Ctcr <> *blanks
057800070731     c                             and  V1Ctcr <> 'F'
057900070731     c                             and  V1Ctcr <> 'M'
058000070731     c                             and  V1Ctcr <> 'O'
058100070731     c                             and  V1Ctcr <> 'R'
058200070731     c                   seton                                        285490
058300070801     c                   eval      V1Dmsg    = $Msg(06)
058400070731     c                   leavesr
058500071024e   1c                   ENDSL
058600070731      *
058700070731      * Controllo codice PdC
058800070801if  1c                   IF        V1Cksc   <> *zeros
058900070731     c                   clear                   TIBS69ds
059000070801     c                   eval      I69kac    =  V1Cksc
059100070731     c                   exsr      Call_TIBS69
059200070731if  2c                   if        *in28
059300070731     c                   leavesr
059400070731e   2c                   endif
059500070731     c                   clear                   Og143
059600070731     c                   movel     V1Cksc        ORGfil
059700070731     c     ORGfil        chain     AZORG
059800070731if  2c                   if        %found(AZORG01L)
059900070801     c                   eval      Og143     = ORGde3
060000070731e   2c                   endif
060100080303if  2c                   if             §OGntw   = 'LOG'
060200070731     c                             or   §OGntw   = 'XXX'
060300070731     c                   seton                                        285590
060400070801     c                   eval      V1Dmsg    = $Msg(07)
060500070731     c                   leavesr
060600070731e   2c                   endif
060700100914      *?Controllo se filiale cliente KSC gestita da filiale gestione utente solo se
060800100914      *?I37opz <> '3'.con opzione '3' è il TNTA60R in immissione.
060900100914      *?L'unico programma che richiama il FIOR37R con opz '3' è il TNTA60R in immissione
061000100914      *?ed ora con il nuovo CRM possiamo aprire clienti anche di altre filiali
061100100914     c                   IF        I37opz  <> '3'
061200080303     c                   eval      *in25     = *off
061300080303     c                   movel     V1Cksc        w003a
061400080303     c     w003a         lookup    $Fgs                                   25
061500080303if  2c                   if        NOT  *in25
061600080303     c                   seton                                        285590
061700080303     c                   eval      V1Dmsg    = $Msg(14)
061800080303     c                   leavesr
061900080303e   2c                   endif
062000100914     c                   ENDIF
062100070731e   1c                   ENDIF
062200070731      *
062300070731      * Controllo codice giro di ritiro
062400070829if  1c                   IF             V1Ccgi <> *blanks
062500070829     c                             and  V1Ccgi <> *all'9'
062600070731      * - Ricerca codice giro (nessun controllo)
062700070731     c                   eval      xx        = %scan('?' : V1Ccgi)
062800070731if  2c                   IF        xx        > *zeros
062900070731     c                   clear                   V1Ccgi
063000070731     c                   clear                   FIDG09ds
063100070731     c                   eval      D09iop0   = 'P01'
063200070731if  3c                   if        V1Cpocgi <> *zeros
063300070731     c                   eval      D09ifgs   = V1Cpocgi
063400070731x   3c                   else
063500071025     c                   eval      D09ifgs   = V1Ccd1
063600070731e   3c                   endif
063700070731     c                   eval      D09idat   = *date
063800070810     c                   eval      D09itug   = 'R'
063900070731     c                   movel(p)  FIDG09ds      KPJBU
064000070731     c                   call      'FIDG09R'
064100070731     c                   parm                    KPJBA
064200070731     c                   movel     KPJBU         FIDG09ds
064300070731if  3c                   if             D09Of03 <> *on
064400070731     c                             and  D09Of12 <> *on
064500070731     c                             and  D09Oerr <> *on
064600070801     c                   eval      V1Cpocgi  = D09ofgs
064700070801     c                   eval      V1Ccgi    = D09ocgi
064800070731e   3c                   endif
064900070801     c                   eval      *in90     = *on
065000070731     c                   leavesr
065100070731e   2c                   ENDIF
065200070829      * - Filiale obbligatoria
065300070829if  2c                   if        V1Cpocgi  = *zeros
065400070803     c                   seton                                        285690
065500070803     c                   eval      V1Dmsg    = $Msg(08)
065600070803     c                   leavesr
065700070828e   2c                   endif
065800070731e   1c                   ENDIF
065900071029      *
066000071029      * - Controllo ambito instradamento
066100071029if  1c                   if             V1Cain <> *blanks
066200071029     c                             and  V1Cain <> '='
066300071029     c                             and  V1Cain <> '>'
066400071029     c                             and  V1Cain <> '<'
066500071029     c                   seton                                        285790
066600071029     c                   eval      V1Dmsg    = $Msg(09)
066700071029     c                   leavesr
066800071029e   1c                   endif
066900071024      *
067000071024      * Se richiesti clienti ritiro SENZA giro ritiro
067100071024      *  e NON inserita alcuna filiale di ritiro
067200071024      * => viene forzata la filiale gestione
067300070731      *
067400070731      * Se NON vi sono errori, riposiziona normalmente il cursore sul
067500070731      *   campo "Ragione Sociale"
067600070801     c                   eval      *in52    = *on
067700070731      *
067800070731     c                   ENDSR
067900070731
068000070731      *---------------------------------------------------------------*
068100070731      *?Reperimento dati anagrafici                                  ?*
068200070731      *---------------------------------------------------------------*
068300070731     c     Call_TIBS69   BEGSR
068400070731      *
068500070731     c                   clear                   ds_CNACO
068600070731     c                   clear                   ds_CNIND
068700070731     c                   clear                   ds_CNCLP
068800070731     c                   clear                   ds_FNCLS
068900070731      *
069000070731     c                   call      'TIBS69R'
069100070731     c                   parm                    TIBS69ds
069200070731     c                   parm                    ds_CNACO
069300070731     c                   parm                    ds_CNIND
069400070731     c                   parm                    ds_CNCLP
069500070731     c                   parm                    ds_FNCLS
069600070731      *
069700070731     c                   eval      *in28    = (O69err = *on)
069800070731if  1c                   if        *in28
069900070731     c                   eval      *in90    = *on
070000070731     c                   eval      V1Dmsg   = O69msg
070100070731e   1c                   endif
070200070731      *
070300070731     c                   ENDSR
070400070731
070500070731      *---------------------------------------------------------------*
070600070731      *?Gestione tasto funzionale F3 da videata D01                  ?*
070700070731      *?F3=Fine                                                      ?*
070800070731      *---------------------------------------------------------------*
070900070731     c     F03D01        BEGSR
071000070731      *
071100070809     c                   eval      O37ret   = '1'
071200070809      *
071300070731      * Chiusura del programma
071400070801     c                   eval      $Fine    = *on
071500070731      *
071600070731     c                   ENDSR
071700070731
071800070731      *---------------------------------------------------------------*
071900070731      *?Gestione videata S01                                         ?*
072000070731      *---------------------------------------------------------------*
072100070731     c     GesS01        BEGSR
072200070731      *
072300070731      * Inizializzazione videata
072400070731if  1c                   if        $InzS01  = *on
072500070731     c                   exsr      InzS01
072600070731     c                   eval      $InzS01  = *off
072700070731e   1c                   endif
072800070731      *
072900070731      * Emissione testata e riga tasti funzionali abilitati
073000081211if  1c                   if        NOT *in28
073100070731     c                   write     OR37T01
073200070731     c                   write     OR37Z01
073300081211e   1c                   endif
073400070731      *
073500070731      * Posizionamento del cursore
073600070731sel 1c                   select
073700070731w   1c                   when      S01nrr  <= *zeros
073800070731     c                   write     OR37D02
073900070731w   1c                   when      C01csr   > *zeros
074000070731     c                   z-add     C01csr        C01rcd
074100070731e   1c                   endsl
074200070731      *
074300070731      * Emissione videata
074400070731     c                   exfmt     OR37C01
074500070731     c                   z-add     £SFLnrr       C01rcd
074600070731     c                   setoff                                       28  90
074700070731     c                   clear                   V1Dmsg
074800070731      *
074900070731sel 1c                   SELECT
075000070731      * F3=Fine
075100070731w   1c                   WHEN      *inKC
075200070731     c                   exsr      F03D01
075300070731      *
075400070731      * F5=Rivisualizzazione
075500070731w   2c                   WHEN      *inKE
075600070731     c                   eval      $InzS01  = *on
075700070731      *
075800070801      * F10=Inserimento
075900070801w   2c                   WHEN      *inKJ
076000070801     c                   exsr      Call_FIOR37R1
076100070731      *
076200070731      * F12=Ritorno
076300070731w   2c                   WHEN      *inKL
076400070801     c                   exsr      F12S01
076500070731      *
076600070731      * Roll-UP
076700070731w   1c                   WHEN      £Tasto   = C_RollUp
076800070731     c                   exsr      RollUpS01
076900080409      *
077000080409      * Nessun record nel SubFile (-> nessuna opzione)
077100080409w   1c                   when      S01nrr  <= *zeros
077200070731      *
077300070731      * Controllo opzioni
077400070731x   1c                   OTHER
077500070731     c                   exsr      OpzS01
077600070731      *
077700070731e   1c                   ENDSL
077800070731      *
077900070731     c                   ENDSR
078000070731
078100070731      *---------------------------------------------------------------*
078200070731      *?Inizializzazione videata S01                                 ?*
078300070731      *---------------------------------------------------------------*
078400070731     c     InzS01        BEGSR
078500070731      *
078600070731      * Pulizia subfile
078700070731     c                   seton                                        3031
078800070731     c                   write     OR37C01
078900070731     c                   setoff                                         3133
079000070731      *
079100070731     c                   reset                   $EoF
079200070731     c                   clear                   W_SflPag
079300070731     c                   clear                   C01rcd
079400070731     c                   clear                   C01csr
079500070731     c                   clear                   S01nrr
079600070731     c                   clear                   V1Dmsg
079700070731     c                   setoff                                       28  90
079800070801     c                   movea     *zeros        *in(40)
079900170131     c                   eval      SqlMail = *off
080000170131     c                   eval      SqlSms  = *off
080100070802      *
080200070802      * Abilitazione opzioni
080300070802      * - Opz. 1 = Selezione
080400070802     c                   eval      *in41    = (I37opz  = '1')
080500070802      * - Opz. 5 = Visualizzazione
080600070802     c                   eval      *in45    = (I37opz  = '5'   or
080700070802     c                                         DUTlpo  = 'S')
080800070731      *
080900070731      * Caricamento dei dati da presentare nel sfl
081000071024sel 1c                   SELECT
081100071024      * - per codice giro
081200071024w   1c                   WHEN           V1Ccgi <> *blanks
081300071024     c                             and  V1Ccgi <> *all'9'
081400071024     c     k02acr12      setll     FNACR102
081500071024     c     k02acr12      reade     FNACR102
081600071024     c                   eval      $EoF     = (%eof(FNACR12L))
081700071024      * - per codice cliente ritiro
081800170131w   1c                   WHEN      V1Ccd2 > 0 or
081900170131w   1c                             (V1Crag   = *blanks and
082000170131     c                              V1mail = *blanks and
082100170131     c                              V1sms = *blanks)
082200070731     c                   eval      dsCRO1   = V1Ccd1
082300070731     c                   eval      dsCRO2   = V1Ccd2
082400070731     c                   eval      dsCRO3   = V1Ccd3
082500070731     c                   move      ds_CRO        ACRcro
082600070801     c     ACRcro        setll     FNACR001
082700070731     c                   read      FNACR001
082800070731     c                   eval      $EoF     = (%eof(FNACR01L))
082900071024      * - per ragione sociale cliente ritiro
083000170131x   1c                   WHEN      V1Crag   <> *blanks
083100070731     c                   movel(p)  V1Crag        ACRrsr
083200071024     c                   eval      dsCRO1   = V1Ccd1
083300071024     c                   eval      dsCRO2   = V1Ccd2
083400071024     c                   eval      dsCRO3   = V1Ccd3
083500071024     c                   move      ds_CRO        ACRcro
083600071024     c     k02acr02      setll     FNACR002
083700070731     c                   read      FNACR002
083800070731     c                   eval      $EoF     = (%eof(FNACR02L))
083900170131      /free
084000170131       //?se ricerca per mail SQL su FNACRE x record 'MC'
084100170131         WHEN  V1mail <> *blanks;
084200170131           SqlMail = *on;
084300170131           exec sql
084400170131           declare FACRmail cursor for select ACREcro, ACREdati from FNACRE0F
084500170131           where ACREtrc = 'MC' and ACREatb = ' '
084600170131           order by ACREcro;
084700170131           exec sql OPEN FACRmail;
084800170131           IF  sqlcode < 0;
084900170131             $EoF = *on;
085000170131           ENDIF;
085100170131           exec sql
085200170131           FETCH NEXT from FACRmail into :ACREcro, :ACREdati;
085300170131           IF  sqlcod = 100 or sqlcod < 0;
085400170131             $EoF = *on;
085500170131           ENDIF;
085600170131       //?se ricerca per sms SQL su FNACRE x record 'SC'
085700170131         WHEN  V1sms <> *blanks;
085800170131           SqlSms = *on;
085900170131           exec sql
086000170131           declare FACRsms cursor for select ACREcro, ACREdati from FNACRE0F
086100170131           where ACREtrc = 'SC' and ACREatb = ' '
086200170131           order by ACREcro;
086300170131           exec sql OPEN FACRsms;
086400170131           IF  sqlcode < 0;
086500170131             $EoF = *on;
086600170131           ENDIF;
086700170131           exec sql
086800170131           FETCH NEXT from FACRsms into :ACREcro, :ACREdati;
086900170131           IF  sqlcod = 100 or sqlcod < 0;
087000170131             $EoF = *on;
087100170131           ENDIF;
087200170131      /end-free
087300071024e   1c                   ENDSL
087400170130
087500071024do  1c                   DOW            $EoF   = *off
087600071024     c                             and  S01nrr < C_SflPag
087700070731     c                   exsr      RollUpS01
087800070731e   1c                   ENDDO
087900170130      /free
088000170130       //?chiudo i cursori
088100170130         SELECT;
088200170131         WHEN  Sqlmail;
088300170131           exec sql close FACRmail;
088400170131         WHEN  Sqlsms;
088500170131           exec sql close FACRsms;
088600170130         ENDSL;
088700070731      *
088800070731      * Impaginazione subfile
088900070731      * -> forza l'impaginazione sul 1° rec. del subfile
089000070731if  1c                   if        S01nrr   > *zeros
089100070731     c                   eval      C01rcd   = 1
089200070731     c                   eval      C01csr   = 1
089300070731x   1c                   else
089400070731     c                   clear                   C01rcd
089500070731e   1c                   endif
089600070731      *
089700070731     c                   ENDSR
089800070731
089900070731      *---------------------------------------------------------------*
090000070731      *?Inizializzazione videata S01                                 ?*
090100070731      *---------------------------------------------------------------*
090200070731     c     RollUpS01     BEGSR
090300070731      *
090400070731     c                   eval      S01nrr   = (W_SflPag * C_SflPag)
090500070731     c                   add       1             W_SflPag
090600070731     c                   eval      *in32    = *off
090700151013      *
090800151013     c                   clear                   SavACRcro
090900070731      *
091000070731      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
091100070731do  1c                   DOW            $EoF   = *off
091200070731     c                             and  S01nrr < (W_SflPag * C_SflPag)
091300070731      *
091400070731      * - Selezione del record dell'archivio
091500070731     c                   exsr      sr_SelRec
091600070731      *
091700070731      *   e Caricamento dati nel record del subfile
091800070731if  2c                   if        $OK      = *on
091900070731     c                   exsr      CarS01
092000070802     c                   add       1             S01nrr
092100070802     c                   write     OR37S01
092200070731e   2c                   endif
092300070731      *
092400070731      * - Lettura record successivo nell'archivio
092500070809      *   (se non rilevato stop record elaborabili in selezione)
092600070809sel 2c                   select
092700070809w   2c                   when      $EoF     = *on
092800071024w   1c                   when           V1Ccgi <> *blanks
092900071024     c                             and  V1Ccgi <> *all'9'
093000071024     c     k02acr12      reade     FNACR102
093100071024     c                   eval      $EoF     = (%eof(FNACR12L))
093200170131w   2c                   when      V1Ccd2  > *zeros or
093300170131w   2c                             (V1Crag = *blanks and
093400170131     c                              V1mail = *blanks and
093500170131     c                              V1sms  = *blanks)
093600070731     c                   read      FNACR001
093700071024     c                   eval      $EoF     = (%eof(FNACR01L))
093800170131x   2c                   when      V1Crag <> *blanks
093900070731     c                   read      FNACR002
094000070731     c                   eval      $EoF     = (%eof(FNACR02L))
094100170131      /free
094200170131       //?se ricerca per mail SQL su FNACRE x record 'MC'
094300170131         WHEN  V1mail <> *blanks;
094400170131           exec sql
094500170131           FETCH NEXT from FACRmail into :ACREcro, :ACREdati;
094600170131           IF  sqlcod = 100 or sqlcod < 0;
094700170131             $EoF = *on;
094800170131           ENDIF;
094900170131       //?se ricerca per sms SQL su FNACRE x record 'SC'
095000170131         WHEN  V1sms <> *blanks;
095100170131           exec sql
095200170131           FETCH NEXT from FACRsms into :ACREcro, :ACREdati;
095300170131           IF  sqlcod = 100 or sqlcod < 0;
095400170131             $EoF = *on;
095500170131           ENDIF;
095600170131      /end-free
095700070809e   2c                   endsl
095800070731      *
095900070731e   1c                   ENDDO
096000070731      *
096100070731      * Visualizzazione del SFL (se ci sono dati)
096200070731     c                   eval      *in30    = (S01nrr <= *zeros)
096300070731      *
096400070731      * Attivazione (eventuale) del SFLEND
096500070731     c                   eval      *in33    = ($EoF = *on)
096600070731      *
096700070731      * Posizionamento cursore al 1° rcd della pagina
096800070731if  1c                   if        S01nrr   > *zeros
096900070731     c     S01nrr        div       C_SflPag      wPag
097000070731     c                   mvr                     wRig
097100070731     c                   eval      C01rcd   = wPag * C_SflPag
097200070731     c                   if        wRig     > *zeros
097300070731     c                   eval      C01rcd   = C01rcd + 1
097400070731     c                   else
097500070731     c                   eval      C01rcd   = C01rcd - C_SflPag + 1
097600070731     c                   endif
097700070731x   1c                   else
097800070731     c                   clear                   C01rcd
097900070731e   1c                   endif
098000070731     c                   eval      C01csr   = C01rcd
098100070731      *
098200070731     c                   ENDSR
098300070731
098400070731      *---------------------------------------------------------------*
098500070731      *?Selezione record in base alle selezioni                      ?*
098600070731      *---------------------------------------------------------------*
098700070731     c     sr_SelRec     BEGSR
098800070731      *
098900070731     c                   eval      $OK      = *off
099000170131     c                   clear                   SavV1mail
099100071024      *
099200071024      * Reperimento dati da FNACR01L se lettura di FNACR12L
099300071024if  1c                   if             V1Ccgi <> *blanks
099400071024     c                             and  V1Ccgi <> *all'9'
099500170131     c                   IF        ACRcro <> ACR1cro
099600071024     c     ACR1cro       chain     FNACR001
099700170131e   1c                   ELSE
099800170131      * Per lettura di FNACR12L posso leggere 2 volte lo stesso cliente
099900170131      * in questo caso non carico il subfile
100000170131     c                   leavesr
100100170131e   1c                   ENDIF
100200170131e   1c                   ENDIF
100300170131      /free
100400170131       //?Se non ho fatto SQL per mail o sms
100500170131       //?e ho mail e/o sms impostati nelle ricerche
100600170131       //?devo agganciare il file
100700170131       //?se ricerca per mail
100800170131         IF  V1Mail <> *blanks;
100900170131           IF  not SqlMail;
101000170131             chain (ACRcro:'MC') FNACRE1L;
101100170131             IF  ACREatb <> *blanks or not %found(FNACRE1L);
101200170131               clear ACREdati;
101300170131             ENDIF;
101400170131           ENDIF;
101500170131           SavV1mail = V1mail;
101600170131           IF  %scan(%trim(SavV1mail):ACREdati) = 0;
101700170131             SavV1mail = %xlate(up:lo:V1mail);
101800170131             IF  %scan(%trim(SavV1mail):ACREdati) = 0;
101900170131               leavesr;
102000170131             ENDIF;
102100170131           ENDIF;
102200170131         ENDIF;
102300170131       //?se ricerca per sms
102400170131         IF  V1sms <> *blanks;
102500170131           IF  not SqlSms;
102600170131             IF  not SqlMail;
102700170131               chain (ACRcro:'SC') FNACRE1L;
102800170131             ELSE;
102900170131               chain (ACREcro:'SC') FNACRE1L;
103000170131             ENDIF;
103100170131             IF  ACREatb <> *blanks or not %found(FNACRE1L);
103200170131               clear ACREdati;
103300170131             ENDIF;
103400170131           ENDIF;
103500170131           IF  %scan(%trim(V1sms):ACREdati) = 0;
103600170131             leavesr;
103700170131           ENDIF;
103800170131         ENDIF;
103900170131       //?se arrivo qua vuol dire che ho trovato mail o sms validi
104000170131       //?quindi aggancio FNACR00F se sto facendo la ricerca tramite SQL
104100170131         IF  SqlMail or SqlSms;
104200170131           chain (ACREcro) FNACR01L;
104300170131         ENDIF;
104400170131      /end-free
104500070801      *
104600070801      * - Se interrogazione ("5") o manutenzione del PdC ("A")
104700130417      *   o immissione da convalida trattative
104800070801      *   si devono caricare solo i codici uguali ai primi sette byte
104900070802if  1c                   if            (I37opz  = '5'
105000130417     c                             or   I37opz  = '3'
105100070802     c                             or   I37opz  = 'A')
105200070802     c                             and  I37cro <> *zeros
105300070801     c                   movel     ACRcro        W7_ACRcro
105400070801     c                   movel     I37cro        W7_I37cro
105500070801sel 2c                   select
105600070801      * - - se minore si deve tornare a leggere
105700070801w   2c                   when      W7_ACRcro  < W7_I37cro
105800070801     c                   leavesr
105900070801      * - - se maggiore si deve uscire
106000070801w   2c                   when      W7_ACRcro  > W7_I37cro
106100070809     c                   eval      $EoF       = *on
106200070801     c                   leavesr
106300070801e   2c                   endsl
106400070801e   1c                   endif
106500070801      *
106600071031sel 1c                   select
106700071024      * Filiale codice cliente ritiro
106800150909w   1c                   when      V1Ccd1 <> *zeros and
106900150909     c                             V1Ccd1 <>
107000150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
107100071024     c                   leavesr
107200070801      * Frequenza ritiro
107300071031w   1c                   when           V1Ctcr <> *blanks
107400070801     c                             and  ACRtcr <> V1Ctcr
107500070801     c                   leavesr
107600070801      * Codice Piano dei Conti
107700070801w   1c                   when           V1Cksc <> *zeros
107800070801     c                             and  ACRksc <> V1Cksc
107900070801     c                   leavesr
108000071031      * Tipo anagrafica (NON richiesto a video)
108100071031     c                   when           I37opz <> *blanks
108200071031     c                             and  I37tac <> *blanks
108300071031     c                             and  ACRtac <> I37tac
108400071031     c                   leavesr
108500071031e   1c                   endsl
108600070731      *
108700071024      * Codice giro di ritiro
108800071024      * - se richiesto con/senza giro ("99999"): nessun controllo
108900071024      * - se richiesto    con    giro: è già stato letto FNACR12L
109000071024      * - se richiesto   senza   giri: da scartare se rilevato un giro
109100071030sel 1c                   SELECT
109200071030w   1c                   WHEN      V1Ccgi   = *all'9'
109300071030w   1c                   WHEN      V1Ccgi   = *blanks
109400071029     c                   exsr      sr_ChkCGI
109500071030if  2c                   if        $Giro    = *on
109600071029     c                   leavesr
109700071029e   2c                   endif
109800071030x   1c                   OTHER
109900071030      * Ambito instradamento giro SE lettura di FNACR12L
110000071030if  2c                   if             V1Cain <> *blanks
110100071030     c                             and  V1Cain <> ACR1ain
110200071030     c                   leavesr
110300071030e   2c                   endif
110400071030e   1c                   ENDSL
110500071029      *
110600071029      * Ambito instradamento giro SE lettura di FNACR12L
110700071029if  1c                   if            (V1Ccgi <> *blanks
110800071029     c                             and  V1Ccgi <> *all'9')
110900071029     c                             and  V1Cain <> *blanks
111000071029     c                             and  V1Cain <> ACR1ain
111100071029     c                   leavesr
111200071029e   1c                   endif
111300150909
111400150909      * Filiale ritiro diversa da cod.Cliente
111500150909     c                   IF        V1fdiff = 'S' and
111600150909     c                             ACRpoa = V1Ccd1
111700150909     c                   leavesr
111800150909     c                   ENDIF
111900070731      *
112000071030     c                   eval      $OK      = *on
112100070731      *
112200070731     c                   ENDSR
112300071029
112400071029      *---------------------------------------------------------------*
112500071029      *?Verifica/Selezione giro ritiro per cliente ritiro            ?*
112600071029      *---------------------------------------------------------------*
112700071029     c     sr_ChkCGI     BEGSR
112800071029      *
112900071030     c                   eval      $Giro    = *on
113000071029      *
113100071029      * Posizionamento iniziale per verifica esistenza giri
113200071029sel 1c                   select
113300071029w   1c                   when      V1Cpocgi  <> *zeros
113400071029     c                   eval      ACR1pocgi  = V1Cpocgi
113500071030     c     k02acr11      setll     FNACR103
113600071029w   1c                   when      DUTlpo     = '2'
113700071029     c                   eval      ACR1pocgi  = DUTpou
113800071030     c     k02acr11      setll     FNACR103
113900071029x   1c                   other
114000071030     c     ACRcro        setll     FNACR103
114100071029e   1c                   endsl
114200071029      *
114300071030if  1c                   if        NOT  %equal(FNACR13L)
114400071030     c                   eval      $Giro    = *off
114500071029     c                   leavesr
114600071029e   1c                   endif
114700071029      *
114800071029      * TROVATO ALMENO UN RECORD:
114900071029      * - Se richiesto con/senza giro ("99999"): nessun controllo
115000071029      * - Se richiesto    con    giro: è già stato letto FNACR12L
115100071029      *                                (non viene eseguita questa subr.)
115200071029      * -?Se richiesto   senza   giri: da scartare se rilevato un giro?
115300071029      *                               ?dell'ambito inserito in D01?
115400071029      *
115500071029      * Richiesto senza giri di qualsiasi ambito e trovato giro
115600071030if  1c                   IF        V1Cain   =  *blanks
115700071029     c                   leavesr
115800071029      *
115900071029      * Richiesto senza giri di un ambito specifico: ricerca ambito
116000071029x   1c                   ELSE
116100071029      *
116200071030do  2c                   DOU       %eof(FNACR13L)
116300071030sel 3c                   select
116400071030w   3c                   when      V1Cpocgi  <> *zeros
116500071030     c     k02acr11      reade     FNACR103
116600071030w   3c                   when      DUTlpo     = '2'
116700071030     c     k02acr11      reade     FNACR103
116800071030x   3c                   other
116900071030     c     ACRcro        reade     FNACR103
117000071030e   3c                   endsl
117100071030sel 3c                   select
117200071030w   3c                   when      %eof(FNACR13L)
117300071030     c                   eval      $Giro      = *off
117400071030w   3c                   when      ACR1ain    = V1Cain
117500071029     c                   leavesr
117600071030e   3c                   endsl
117700071029e   2c                   ENDDO
117800071029      *
117900071029e   1c                   ENDIF
118000071029      *
118100071029     c                   ENDSR
118200070731
118300070731      *---------------------------------------------------------------*
118400070731      *?Caricamento singolo record nel SubFile S01                   ?*
118500070731      *---------------------------------------------------------------*
118600070731     c     CarS01        BEGSR
118700070731      *
118800071024      * Ricerca eventuali giri per cliente ritiro
118900071030     c     ACRcro        setll     FNACR103
119000071024      *
119100070731     c                   clear                   OR37S01
119200070731      * Campi hidden
119300070731      * Campi di solo output
119400070801     c                   eval      S1Catb   = ACRatb
119500070801     c                   move      ACRcro        ds_CRO
119600070801     c                   eval      S1Ccd1   = dsCRO1
119700070801     c                   eval      S1Ccd2   = dsCRO2
119800070801     c                   eval      S1Ccd3   = dsCRO3
119900070801     c                   movel     ACRrsr        S1Crag
120000071030if  1c                   if        %equal(FNACR13L)
120100071030     c                   eval      S1Csng   = 'S'
120200070903e   1c                   endif
120300070801     c                   if        ACRksc  <> *zeros
120400070801     c                   move      ACRksc        S1Cksc
120500070801     c                   endif
120600070801     c                   if        ACRccc  <> 999
120700070801     c                   move      ACRccc        S1Cctr
120800070801     c                   endif
120900070801     c                   eval      S1Ctcr   = ACRtcr
121000070801      *
121100070801     c                   movel     ACRinr        S1Cind
121200070801     c                   movel     ACRlor        S1Cloc
121300070801     c                   eval      S1Cprv   = ACRprr
121400070801     c                   eval      S1Cnaz   = ACRnar
121500150909
121600150909      * Filiale ritiro solo se diversa dal codice cliente
121700150909     c                   IF        %dec(%subst(%editc(ACRcro:'X'):1:3):3:0) <>
121800150909     c                             ACRpoa
121900150909     c                   eval      S1cpoa = %editc(ACRpoa:'X')
122000150909     c                   ELSE
122100150909     c                   clear                   S1cpoa
122200150909     c                   ENDIF
122300070731      *
122400070731     c                   ENDSR
122500070801
122600070801      *---------------------------------------------------------------*
122700070801      *?Gestione tasto funzionale F12 da videata S01                 ?*
122800070801      *?F12=Ritorno                                                  ?*
122900070801      *---------------------------------------------------------------*
123000070801     c     F12S01        BEGSR
123100070801      *
123200070801      * Chiusura del programma  SE  richiamato da altro programma
123300070801     c                   if             I37opz  <> *blanks
123400070801     c                             and (I37cro  <> *zeros
123500070801     c                              or  I37rag  <> *blanks)
123600070809     c                   eval      O37ret   = '2'
123700070801     c                   eval      $Fine    = *on
123800070801      * Ritorno alla videata D01  SE  richiamato da menù
123900070801     c                   else
124000070801     c                   eval      $Video   = 'D1'
124100071025     c                   eval      *in40    = (SavIn40 = *on)
124200071025      *
124300070801     c                   endif
124400070801      *
124500070801     c                   ENDSR
124600070731
124700070731      *---------------------------------------------------------------*
124800070731      *?Controllo opzioni subfile                                    ?*
124900070731      *---------------------------------------------------------------*
125000070731     c     OpzS01        BEGSR
125100070731      *
125200070731     c                   readc     OR37S01
125300070731      *
125400070731do  1c                   DOW       NOT %eof(FIOR37D)
125500070731      *
125600070801     c                   eval      *in32    = *off
125700070801     c                   movea     *zeros        *in(50)
125800070731     c                   z-add     S01nrr        C01rcd
125900070731      *
126000070731sel 2c                   SELECT
126100070731      * - Nessuna opzione
126200070801w   2c                   WHEN      S1Copz   = *zeros
126300070801      * - 1 = Selezione
126400070801w   2c                   WHEN           S1Copz  = 1
126500070801     c                             and  I37opz  = '1'
126600070808     c                   eval      dsCRO1   = S1Ccd1
126700070808     c                   eval      dsCRO2   = S1Ccd2
126800070808     c                   eval      dsCRO3   = S1Ccd3
126900070808     c                   move      ds_CRO        O37cro
127000070808     c                   eval      $Fine    = *on
127100070808     c                   eval      *in90    = *on
127200070731      * - 2 = Modifica
127300070801w   2c                   WHEN           S1Copz  = 2
127400070801     c                             and (I37opz <> '1'
127500070801     c                             and  I37opz <> '5')
127600070802     c                             and  DUTlpo <> 'S'
127700071026if  3c                   if        S1Ccd1   = V1Cfgs
127800070801     c                   exsr      Call_FIOR37R1
127900071026x   3c                   else
128000071024     c                   exsr      Call_FIOR37R2
128100071026e   3c                   endif
128200070807      * - 3 = Copia
128300070801w   2c                   WHEN           S1Copz  = 3
128400070801     c                             and (I37opz <> '1'
128500070801     c                             and  I37opz <> '5')
128600070801     c                             and  DUTlpo <> 'S'
128700071026if  3c                   if        S1Ccd1   = V1Cfgs
128800070801     c                   exsr      Call_FIOR37R1
128900071026x   3c                   else
129000071026     c                   seton                                        285090
129100071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
129200071026     c                                      + ' per un cliente ritiro +
129300071026     c                                        di altra filiale ('+
129400071026     c                                        %trim( %editw(+
129500071026     c                                              S1Ccd1:'0   '))
129600071026     c                                      + ')'
129700071026e   3c                   endif
129800070801      * - 4 = Annullamento
129900070801w   2c                   WHEN           S1Copz  = 4
130000070801     c                             and (I37opz <> '1'
130100070801     c                             and  I37opz <> '5')
130200070802     c                             and  DUTlpo <> 'S'
130300071026sel 3c                   select
130400071026w   3c                   when      S1Catb  <> *blanks
130500070801     c                   seton                                        285090
130600070801     c                   eval      V1Dmsg   = $Msg(10)
130700071026w   3c                   when      S1Ccd1   = V1Cfgs
130800140723w   3c                   IF        S1Copz  = 4 and
130900140807w   3c                             S1Ccd3  = 0 and
131000140807     c                             S1Ccd2  <> 9999 and
131100140807     c                             S1Ccd2  <> 8888
131200140723     c                   seton                                        285090
131300140723     c                   eval      V1Dmsg   = %trimr($Msg(12))
131400140723     c                                      + ' per luogo 000'
131500140723     c                   ELSE
131600070801     c                   exsr      Call_FIOR37R1
131700140723     c                   ENDIF
131800071026x   4c                   other
131900071026     c                   seton                                        285090
132000071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
132100071026     c                                      + ' per un cliente ritiro +
132200071026     c                                        di altra filiale ('+
132300071026     c                                        %trim( %editw(+
132400071026     c                                              S1Ccd1:'0   '))
132500071026     c                                      + ')'
132600071026e   3c                   endsl
132700070731      * - 5 = Visualizzazione
132800070801w   2c                   WHEN           S1Copz  = 5
132900070801     c                   exsr      Call_FIOR37R1
133000070801      * - 7 = Ripristino
133100070801w   2c                   WHEN           S1Copz  = 7
133200070801     c                             and (I37opz <> '1'
133300070801     c                             and  I37opz <> '5')
133400070801     c                             and  DUTlpo <> 'S'
133500071026sel 3c                   select
133600071026w   3c                   when      S1Catb   = *blanks
133700070801     c                   seton                                        285090
133800070801     c                   eval      V1Dmsg   = $Msg(11)
133900071026w   3c                   when      S1Ccd1   = V1Cfgs
134000070801     c                   exsr      Call_FIOR37R1
134100071026x   3c                   other
134200071026     c                   seton                                        285090
134300071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
134400071026     c                                      + ' per un cliente ritiro +
134500071026     c                                        di altra filiale ('+
134600071026     c                                        %trim( %editw(+
134700071026     c                                              S1Ccd1:'0   '))
134800071026     c                                      + ')'
134900071026e   3c                   endsl
135000070731      * - ? = Opzione NON valida
135100070731x   2c                   OTHER
135200070731     c                   seton                                        285090
135300070801     c                   eval      V1Dmsg   = $Msg(12)
135400070731e   2c                   ENDSL
135500070731      *
135600070731      * Aggiornamento sfl
135700070731sel 2c                   select
135800070731w   2c                   when      *in28
135900070807     c                   eval      *in32    = *on
136000070731     c                   z-add     C01rcd        C01csr
136100070807w   2c                   when      *in90    = *on
136200070731     c                   z-add     C01rcd        C01csr
136300070731     c                   clear                   S1Copz
136400070731x   2c                   other
136500070926     c                   z-add     C01rcd        C01csr
136600070731     c                   clear                   S1Copz
136700070731e   2c                   endsl
136800070801     c                   UPDATE    OR37S01
136900070731if  2c                   if        *in28  OR  *in90
137000070731     c                   leave
137100070731e   2c                   endif
137200070731      *
137300070731     c                   readc     OR37S01
137400070731      *
137500070731e   1c                   ENDDO
137600151013      *
137700151013      * -?SE si sta leggendo il file FNACR01L &?
137800151013      *  ?   è stato modificato/annullato/ripristinato un cliente?
137900151013      *  ?occorre riposizionarsi per l'eventuale pagina successiva?
138000151013if  1c                   if        V1Crag    = *blanks  and
138100151013     c                             SavACRcro > *zeros
138200151013     c     SavACRcro     chain     FNACR001
138300151013e   1c                   endif
138400070731      *
138500070731     c                   ENDSR
138600070801
138700070801      *---------------------------------------------------------------*
138800070801      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
138900070801      *---------------------------------------------------------------*
139000070801     c     Call_FIOR37R1 BEGSR
139100070801      *
139200070801      * Salvataggio area dati
139300070801     c                   movel     FIOR37ds      SavOR37ds
139400070801      *
139500070801      * Impostazione parametri
139600070801     c                   clear                   FIOR37ds
139700070801     c                   eval      I37fgs   = V1Cfgs
139800070801sel 1c                   SELECT
139900070809      * - F10 = Immissione
140000070801w   1c                   WHEN           *inKJ   = *on
140100070801     c                   eval      I37opz   = '1'
140200070809      * - I37opz = '3' (Immissione/Copia - obbligatorio!)
140300070809w   1c                   WHEN           $Video  = 'D1'
140400070809     c                             and  %subst(SavOR37ds : 1 : 1) = '3'
140500070809     c                   eval      I37opz   = 'A'
140600070809     c                   eval      dsCRO1   = V1Ccd1
140700070809     c                   eval      dsCRO2   = V1Ccd2
140800070809     c                   eval      dsCRO3   = V1Ccd3
140900070809     c                   move      ds_CRO        I37cro
141000070809     c                   eval      I37ksc   = V1Cksc
141100070801      * - 2 = Modifica
141200070801w   1c                   WHEN           $Video  = 'S1'
141300070801     c                             and  S1Copz  = 2
141400070801     c                   eval      I37opz   = '2'
141500070801     c                   eval      dsCRO1   = S1Ccd1
141600070801     c                   eval      dsCRO2   = S1Ccd2
141700070801     c                   eval      dsCRO3   = S1Ccd3
141800070801     c                   move      ds_CRO        I37cro
141900070801      * - 3 = Copia (non in sede)
142000070801w   1c                   WHEN           $Video  = 'S1'
142100070801     c                             and  S1Copz  = 3
142200070801     c                   eval      I37opz   = '3'
142300070801     c                   eval      dsCRO1   = S1Ccd1
142400070801     c                   eval      dsCRO2   = S1Ccd2
142500070801     c                   eval      dsCRO3   = S1Ccd3
142600070801     c                   move      ds_CRO        I37cro
142700070801      * - 4 = Annullamento
142800070801w   1c                   WHEN           $Video  = 'S1'
142900070801     c                             and  S1Copz  = 4
143000070801     c                   eval      I37opz   = '4'
143100070801     c                   eval      dsCRO1   = S1Ccd1
143200070801     c                   eval      dsCRO2   = S1Ccd2
143300070801     c                   eval      dsCRO3   = S1Ccd3
143400070801     c                   move      ds_CRO        I37cro
143500070801      * - 5 = Visualizzazione
143600070801w   1c                   WHEN           $Video  = 'S1'
143700070801     c                             and  S1Copz  = 5
143800070801     c                   eval      I37opz   = '5'
143900070801     c                   eval      dsCRO1   = S1Ccd1
144000070801     c                   eval      dsCRO2   = S1Ccd2
144100070801     c                   eval      dsCRO3   = S1Ccd3
144200070801     c                   move      ds_CRO        I37cro
144300070801      * - 7 = Ripristino
144400070801w   1c                   WHEN           $Video  = 'S1'
144500070801     c                             and  S1Copz  = 7
144600070801     c                   eval      I37opz   = '7'
144700070801     c                   eval      dsCRO1   = S1Ccd1
144800070801     c                   eval      dsCRO2   = S1Ccd2
144900070801     c                   eval      dsCRO3   = S1Ccd3
145000070801     c                   move      ds_CRO        I37cro
145100070801e   1c                   ENDSL
145200070801      *
145300070801     c                   movel(p)  FIOR37ds      KPJBU
145400070801     c                   call      'FIOR37R1'
145500070801     c                   parm                    KPJBA
145600070801     c                   movel     KPJBU         FIOR37ds
145700070801      *
145800070801sel 1c                   SELECT
145900070801      * - Errore
146000070801w   1c                   WHEN      O37err   = 'E'
146100070801     c                   eval      V1Dmsg   = O37msg
146200070801     c                   seton                                        28  90
146300070802      * - F3=Fine
146400070803w   1c                   WHEN      O37ret   = '1'
146500070809if  2c                   if             S1Copz  = 5
146600070809     c                             and  S01nrr  = 1
146700070809     c                   eval      $Fine    = *on
146800070809e   2c                   endif
146900070806     c                   eval      *in90    = *on
147000070803      * - F12=Ritorno
147100070803w   1c                   WHEN      O37ret   = '2'
147200070801      * - Inserito record
147300070803w   1c                   WHEN           O37ret  = *off
147400070803     c                             and  O37cro  > *zeros
147500070801     c                   eval      $InzS01  = *on
147600070802      * - Modificato record
147700070802w   1c                   OTHER
147800151013      * - -?SE si sta leggendo il file FNACR01L:?
147900151013      *    ?occorre salvare il cliente su cui si è posizionati per il?
148000151013      *    ?caricamento dell'eventuale pagina successiva del subfile?
148100151013if  2c                   if        V1Crag    = *blanks  and
148200151013     c                             SavACRcro = *zeros
148300151013     c                   eval      SavACRcro = ACRcro
148400151013e   2c                   endif
148500071024     c     I37cro        chain     FNACR001
148600070802if  2c                   if        %found(FNACR01L)
148700070802     c                   exsr      CarS01
148800070802e   2c                   endif
148900070801e   1c                   ENDSL
149000070801      *
149100070801      * Ripristino area dati
149200070801     c                   movel     SavOR37ds     FIOR37ds
149300070801      *
149400070801     c                   ENDSR
149500071023
149600071023      *---------------------------------------------------------------*
149700071023      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
149800071023      *---------------------------------------------------------------*
149900071024     c     Call_FIOR37R2 BEGSR
150000071023      *
150100071023      * Salvataggio area dati
150200071023     c                   movel     FIOR37ds      SavOR37ds
150300071023      *
150400071023      * Impostazione parametri
150500071024     c                   clear                   FIOR37ds
150600071024     c                   eval      I37fgs   = V1Cfgs
150700071023     c                   eval      dsCRO1   = S1Ccd1
150800071023     c                   eval      dsCRO2   = S1Ccd2
150900071023     c                   eval      dsCRO3   = S1Ccd3
151000071024     c                   move      ds_CRO        I37cro
151100071024     c                   eval      I37pocgi = V1Cfgs
151200071023      *
151300071024     c                   movel(p)  FIOR37ds      KPJBU
151400071024     c                   call      'FIOR37R2'
151500071023     c                   parm                    KPJBA
151600071024     c                   movel     KPJBU         FIOR37ds
151700071023      *
151800071023sel 1c                   SELECT
151900071023      * - Errore
152000071024w   1c                   WHEN      O37err   = 'E'
152100071023     c                   eval      V1Dmsg   = O37msg
152200071023     c                   seton                                        28  90
152300071023      * - F3=Fine
152400071024w   1c                   WHEN      O37ret   = '1'
152500071023if  2c                   if             S1Copz  = 5
152600071023     c                             and  S01nrr  = 1
152700071023     c                   eval      $Fine    = *on
152800071023e   2c                   endif
152900071023     c                   eval      *in90    = *on
153000071023      * - F12=Ritorno
153100071024w   1c                   WHEN      O37ret   = '2'
153200071024      * - Inserito/Modificato/Eliminato giro ritiro
153300071024w   1c                   OTHER
153400151013      * - -?SE si sta leggendo il file FNACR01L:?
153500151013      *    ?occorre salvare il cliente su cui si è posizionati per il?
153600151013      *    ?caricamento dell'eventuale pagina successiva del subfile?
153700151013if  2c                   if        V1Crag    = *blanks  and
153800151013     c                             SavACRcro = *zeros
153900151013     c                   eval      SavACRcro = ACRcro
154000151013e   2c                   endif
154100071024     c     I37cro        chain     FNACR001
154200071024if  2c                   if        %found(FNACR01L)
154300071024     c                   exsr      CarS01
154400071024e   2c                   endif
154500071024e   1c                   ENDSL
154600071023      *
154700071023      * Ripristino area dati
154800071023     c                   movel     SavOR37ds     FIOR37ds
154900071023      *
155000071023     c                   ENDSR
155100000929
155200070730** - $MSG -------------------------------------------------------------------*
155300070801Elaborazione non possibile: manca la filiale gestione                           1
155400070801Filiale gestione errata                                                         2
155500070801Procedura ORM non attivata                                                      3
155600071024Filiale del codice cliente ritiro obbligatoria                                  4
155700070801Tipo anagrafica errato                                                          5
155800070801Frequenza ritiro errata                                                         6
155900070801Codice cliente piano dei conti errato                                           7
156000070801Filiale obbligatoria per codice giro diverso da '9999999999'                    8
156100071029Ambito instradamento errato                                                     9
156200070801Record annullato. Consentito solo il ripristino                                10
156300070801Record non annullato. Impossibile ripristinare                                 11
156400070801Opzione errata                                                                 12
156500080303Utente non autorizzato alla Gestione anagrafico clienti                        13
156600080303Cliente NON gestibile dall'utente                                              14
