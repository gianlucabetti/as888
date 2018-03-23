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
004200170221     d $Msg            s             78    dim(16) ctdata perrcd(1)             MSG VIDEO
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
010500170214     d $AIN            s               n   inz(*off)
010600080303     d $UteNoAbil      s               n   inz(*off)
010700080303     d SavIn18         s               n   inz(*off)
010800080303     d SavIn40         s               n   inz(*off)
010900170131     d SqlMail         s               n   inz(*off)
011000170131     d SqlSms          s               n   inz(*off)
011100070730      * - Indici di schiera
011200070730     d xx              s              3  0 inz
011300070731      * - Gestione video
011400070801     d S01nrr          s                   like(C01rcd)   inz
011500070731     d $Video          s              2    inz('D1')
011600070731     d wPag            s              4  0 inz
011700070731     d wRig            s              3  0 inz
011800070731     d W_SflPag        s              3  0 inz
011900070801      * - Comodo
012000080303     d wAbi            s                   like(UTEaut)   inz
012100070801     d W7_ACRcro       s                   like(ACRksc)   inz
012200070801     d W7_I37cro       s                   like(ACRksc)   inz
012300170222     d*// SavACRcro       s                   like(ACRcro)   inz
012400070801     d SavOR37ds       s                   like(FIOR37ds) inz
012500071024     d SavFGS          s                   like(V1Cfgs)   inz
012600170130     d SavV1mail       s                   like(V1mail)   inz
012700080303     d w003a           s              3    inz
012800070730      *
012900070730      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
013000070801      *
013100071024      * - FNACR02L
013200071024     c     k02acr02      klist
013300071024     c                   kfld                    ACRrsr
013400071024     c                   kfld                    ACRcro
013500071030      * - FNACR13L
013600071024     c     k02acr11      klist
013700071024     c                   kfld                    ACRcro
013800071025     c                   kfld                    ACR1pocgi
013900071024      * - FNACR12L
014000071024     c     k02acr12      klist
014100071024     c                   kfld                    V1Cpocgi
014200071024     c                   kfld                    V1Ccgi
014300000927
014400070730      *---------------------------------------------------------------*
014500070730      *?RIEPILOGO INDICATORI                                         ?*
014600070730      *---------------------------------------------------------------*
014700070801      *  10    - Abilitazione tasto funzionale F10=Inserimento  - D01 *
014800070801      *  18    - Abilitazione tasto funzionale F18=Cambio FGS   - D01 *
014900070801      *  25    - Comodo                                               *
015000070801      *  28    - Emissione del messaggio di errore a video            *
015100070801      *  30    - Pulizia del subfile                            - C01 *
015200070801      *  31    - NO emissione del subfile                       - C01 *
015300070801      *  33    - Fine dati nel subfile         (sflend)         - C01 *
015400070801      *  41    - Pgm richiamato con opz. "1"                    - C01 *
015500070801      *  45    - Pgm richiamato con opz. "5" o utente di sede   - C01 *
015600070801      *  50    - Filiale in gestione errata                     - D01 *
015700070801      *  51    - Cliente ritiro      errato                     - D01 *
015800070801      *  52    - Ragione sociale     errata                     - D01 *
015900070801      *  53    - Tipo anagrafica     errato                     - D01 *
016000070801      *  54    - Frequenza ritiro    errata                     - D01 *
016100070801      *  55    - Codice PdC          errato                     - D01 *
016200070801      *  56    - Giro                errato                     - D01 *
016300070801      *  90    - Generico di errore                                   *
016400000927      *---------------------------------------------------------------*
016500070730
016600070730     c     *Entry        plist
016700070730     c                   parm                    KPJBA
016800070730      *
016900070730      * Operazioni iniziali
017000070731     c                   exsr      RoutInz
017100070731      *
017200070731      * Gestione video
017300070731do  1c                   dow       $Fine   = *off
017400070731     c     $Video        caseq     'D1'          GesD01
017500070731     c     $Video        caseq     'S1'          GesS01
017600070731     c                   endcs
017700070731e   1c                   enddo
017800070731      *
017900070731      * Operazioni finali
018000070731     c                   clear                   TIBS69ds
018100070731     c                   eval      I69tla   = 'C'
018200070731     c                   call      'TIBS69R'
018300070731     c                   parm                    TIBS69ds
018400070731     c                   movel(p)  FIOR37ds      KPJBU
018500070731     c                   eval      *inLR   = *on
018600070731
018700070731      *---------------------------------------------------------------*
018800070731      *?Operazioni Iniziali                                          ?*
018900070731      *---------------------------------------------------------------*
019000070731     c     RoutInz       BEGSR
019100070802      *
019200070802     c                   if        KPJBU   <> *blanks
019300070802     c                   movel     KPJBU         FIOR37ds
019400070802     c                   else
019500070802     c                   clear                   FIOR37ds
019600070802     c                   endif
019700070731      *
019800070731      * Pulizia campi di output
019900070731     c                   clear                   O37cro
020000070731     c                   clear                   O37err
020100070731     c                   clear                   O37msg
020200070731      *
020300070731      * Controllo parametri ricevuti
020400070731sel 1c                   SELECT
020500070731      * - Controllo inserimento filiale gestione per Immissione/Variaz.
020600070802w   1c                   WHEN          (I37opz   = '2'
020700070803     c                             or   I37opz   = '3'
020800070803     c                             or   I37opz   = '7')
020900070731     c                             and  I37fgs   = *zeros
021000070731     c                   eval      O37err   = 'E'
021100070731     c                   eval      O37msg   = $Msg(01)
021200070731     c                   eval      $Fine    = *on
021300070731     c                   leavesr
021400070731e   1c                   ENDSL
021500071031      *
021600071105      * - Forzatura filiale cliente ritiro (SE richiesta)
021700071031if  1c                   if             I37opz  <> *blanks
021800071031     c                             and  I37cro   = *zeros
021900071105     c                             and  I37rag  <> *blanks
022000071105     c                   eval      I37cro   = I37fgs * 10000000
022100071031e   1c                   endif
022200070731      *
022300070731      * Reperimento dati job
022400070731     c                   exsr      DatiJob
022500080303     c                   eval      $UteNoAbil   =  (*in50 = *on)
022600080303      * -> se rilevato errore e pgm richiamato x mod.: passa il msg.
022700080303if  1c                   if        $UteNoAbil   =  *on
022800080303     c                             and  I37opz  <> *blanks
022900080303     c                   eval      O37err   = 'E'
023000080303     c                   eval      O37msg   = V1Dmsg
023100080303     c                   eval      $Fine    = *on
023200080303     c                   leavesr
023300080303e   1c                   endif
023400070731      *
023500070731      * Impostazione nome programma a video
023600070731     c                   movel     SDSpgm        V1Tpgm
023700070801      *
023800070801      * Impostazione attributi di visualizzazione in testata
023900070801sel 1c                   select
024000070801w   1c                   when      I37opz   = *blanks
024100070801     c                   eval      DA01     = C_ND
024200070801w   1c                   when      I37opz   = '1'
024300070801     c                   eval      V1Topz   = '  Selezione   '
024400070801     c                   eval      DA01     = C_HI_RI
024500080303w   1c                   when      I37opz   = '5'  or  DUTlpo = 'S'
024600070801     c                   eval      V1Topz   = 'Interrogazione'
024700070801     c                   eval      DA01     = C_HI_RI
024800070801x   1c                   other
024900070801     c                   eval      V1Topz   = '   Gestione   '
025000070801     c                   eval      DA01     = C_HI_RI
025100070801e   1c                   endsl
025200070808      *
025300070809      * RICEVUTI PARAMETRI DI SELEZIONE (codice o rag.soc.):
025400070808if  1c                   if             I37opz  <> *blanks
025500070808     c                             and (I37cro  <> *zeros
025600070808     c                              or  I37rag  <> *blanks)
025700070809      * - inizializzazione e controllo della 1ª videasta
025800070808     c                   exsr      InzD01
025900070808     c                   eval      $InzD01  = *off
026000070808     c                   exsr      CtrD01
026100070809sel 2c                   select
026200080303w   2c                   when      $UteNoAbil  = *on
026300080303     c                   seton                                        285090
026400080303     c                   eval      V1Dmsg  = $Msg(13)
026500070809      * - > visualizzazione 1ª videata se rilevati errori
026600070809w   2c                   when      *in90
026700070810      * - > richiamo pgm. FIOR37R1 se richiesta immissione/copia da
026800070810      *     TNTA60R
026900070809w   2c                   when      I37opz   = '3'
027000070809     c                   exsr      Call_FIOR37R1
027100070810     c                   eval      $Video   = 'S1'
027200070809      * - > visualizzazione subfile altrimenti
027300070809x   2c                   other
027400070808     c                   eval      $Video   = 'S1'
027500070809e   2c                   endsl
027600070808e   2c                   endif
027700070731      *
027800070731     c                   ENDSR
027900070731
028000070731      *---------------------------------------------------------------*
028100070731      *?Reperimento Dati del job (Utente/Operativi)                  ?*
028200070731      *---------------------------------------------------------------*
028300070731     c     DatiJob       BEGSR
028400070731      *
028500070731     c     *dtaara       define    §azute        azuteds
028600070731     c     *dtaara       define    §datiute      ddatiute
028700070731      *
028800070731     c                   in(E)     *dtaara
028900070731     c                   IF        %ERROR or RSUT = *blanks
029000070731     c                   clear                   Tibs34Ds
029100070731     c                   call      'TIBS34R'
029200070731     c                   parm                    Tibs34Ds
029300070731     c                   in        *dtaara
029400070731     c                   ENDIF
029500080303      *
029600080303     c                   clear                   wAbi
029700080303     c                   clear                   dLAT
029800080303      *
029900080303      * Verifica errori e autorità profilo
030000080303s   1c                   SELECT
030100080303      * - se ho errori nei dati utente esco dal pgm
030200080303w   1c                   WHEN      DUTerr  = 'E'
030300080303     c                   seton                                        285090
030400080303     c                   eval      V1Dmsg  = $Msg(13)
030500080303     c                   leavesr
030600080303      * - se non c'è l'abilitazione
030700080303      *   -> se 1° livello, abilitazioni al terminal
030800080303      *      se 2° livello, abilitazioni al punto operativo
030900080303      *      se sede è impossibile ma se capita mando a fine il pgm
031000080303w   1c                   WHEN      UTEaut  = *blanks
031100080303if  2c                   if        DUTlpo  = '1'
031200080303     c                   eval      wAbi    = 'TP'
031300080303e   2c                   endif
031400080303if  2c                   if        DUTlpo  = '2'
031500080303     c                   eval      wAbi    = 'PO'
031600080303e   2c                   endif
031700080303if  2c                   if        DUTlpo  = 'S'
031800080303     c                   seton                                        285090
031900080303     c                   eval      V1Dmsg  = $Msg(13)
032000080303     c                   leavesr
032100080303e   2c                   endif
032200080303      * - caricamento abilitazioni del profilo
032300080303x   1c                   OTHER
032400080303     c                   movel     UTEfaf        dUTE01
032500080303if  2c                   if        §UTEcli = *blanks
032600080303     c                   eval      wAbi    = UTEaut
032700080303x   2c                   else
032800080303     c                   eval      wAbi    = §UTEcli
032900080303e   2c                   endif
033000080303e   1c                   ENDSL
033100080303      *
033200080303      * Controllo se ok l'abilitazione dell'utente
033300080303     c                   reset                   TIBS02ds
033400080303     c                   eval      T02sif  = KNSIF
033500080303     c                   eval      T02ke1  = wAbi
033600080303     c                   call      'TIBS02R'
033700080303     c                   parm                    KPJBA
033800080303     c                   parm                    TIBS02ds
033900080303if  1c                   if        T02err  = *blanks
034000080303     c                   eval      dLat    = T02uni
034100080303e   1c                   endif
034200080303      *
034300080303      * Errore o non abilitato
034400080303if  1c                   if             T02err <> *blanks
034500080303     c                             or   §LATabi = 'S'
034600080303     c                   seton                                        285090
034700080303     c                   eval      V1Dmsg  = $Msg(13)
034800080303     c                   leavesr
034900080303e   1c                   endif
035000080303      *
035100080303      * Reperimento delle fil. gestibili dall'utente per i cod.clienti
035200080303     c                   clear                   TRUL31ds
035300080303     c                   eval      I31abi  = wAbi
035400080303     c                   eval      I31cdi  = DUTdis
035500080303     c                   eval      I31car  = DUTare
035600080303     c                   eval      I31cpo  = DUTpou
035700080303     c                   call      'TRUL31R'
035800080303     c                   parm                    KPJBA
035900080303     c                   parm                    TRUL31ds
036000080303if  1c                   IF        O31pog  > *zeros
036100080303     c                   movea     O31pog        $Fgs
036200080303x   1c                   ELSE
036300080303     c                   seton                                        285090
036400080303     c                   eval      V1Dmsg  = $Msg(13)
036500080303     c                   leavesr
036600080303e   1c                   ENDIF
036700070731      *
036800070731     c                   ENDSR
036900070731
037000070731      *---------------------------------------------------------------*
037100070731      *?Gestione videata D01                                         ?*
037200070731      *---------------------------------------------------------------*
037300070731     c     GesD01        BEGSR
037400070731      *
037500070731      * Inizializzazione videata
037600070731if  1c                   if        $InzD01  = *on
037700070731     c                   exsr      InzD01
037800070731     c                   eval      $InzD01  = *off
037900070731e   1c                   endif
038000070731      *
038100070731      * Emissione testata e riga tasti funzionali abilitati
038200070731     c                   write     OR37T01
038300070731     c                   write     OR37Z01
038400070731      *
038500070731      * Emissione videata
038600070731     c                   exfmt     OR37D01
038700070731     c                   setoff                                       28  90
038800070731     c                   clear                   V1Dmsg
038900070731      *
039000070731sel 1c                   SELECT
039100070731      * F3=Fine
039200070731w   1c                   WHEN      *inKC
039300070731     c                   exsr      F03D01
039400070731      * F10=Immissione
039500070731w   1c                   WHEN      *inKJ
039600070807     c                   exsr      CtrFGS
039700070807if  2c                   if        NOT  *in90
039800070801     c                   exsr      Call_FIOR37R1
039900070807e   2c                   endif
040000070731      * F18=Cambio filiale gestione
040100070731w   1c                   WHEN      *inKS
040200070731     c                   eval      *in18    = *off
040300070810     c                   eval      *in50    = *on
040400070731      * Enter
040500070731x   1c                   OTHER
040600070731     c                   exsr      CtrD01
040700070731if  2c                   if        *in90
040800070731     c                   leavesr
040900070731e   2c                   endif
041000071025     c                   eval      *in18    = (SavIn18 = *on)
041100070731     c                   eval      $Video   = 'S1'
041200070801     c                   eval      $InzS01  = *on
041300070801      *
041400070801e   1c                   ENDSL
041500070731      *
041600070731     c                   ENDSR
041700070731
041800070731      *---------------------------------------------------------------*
041900070731      *?Inizializzazione videata D01                                 ?*
042000070731      *---------------------------------------------------------------*
042100070731     c     InzD01        BEGSR
042200070731      *
042300070731     c                   clear                   OR37D01
042400070731      *
042500070731      * Impostazione automatica della filiale di gestione
042600070731sel 1c                   select
042700070731      * - forzatura filiale gestione SE richiamato
042800070801w   1c                   when           I37opz  <> *blanks
042900070731     c                             and  I37fgs  <> *zeros
043000070731     c                   eval      V1Cfgs   = I37fgs
043100070731      * - impostazione di default per filiali di 2° livello
043200070905w   1c                   when           I37opz   = *blanks
043300070731     c                             and (DUTlpo   = '2'
043400070731     c                              or  DUTlpo   = *blanks)
043500070731     c                   eval      V1Cfgs   = DUTpou
043600070731      * - impostazione di default per filiali di 1° livello
043700070801x   1c                   other
043800070801     c                   eval      V1Cfgs   = DUTtfp
043900070731e   1c                   endsl
044000070731      *
044100070731      * Decodifica filiale di gestione
044200070731     c                   exsr      CtrFGS
044300070731      *
044400070731      * Impostazione campi restanti
044500070801if  1c                   if        I37opz   = *blanks                           NON Richiamato
044600071023     c                   eval      V1Ccd1   = V1Cfgs
044700070731     c                   eval      V1Ccgi   = *all'9'
044800070731x   1c                   else
044900070731     c                   movel     I37cro        ds_CRO
045000070731     c                   eval      V1Ccd1   = dsCRO1
045100070731     c                   eval      V1Ccd2   = dsCRO2
045200070731     c                   eval      V1Ccd3   = dsCRO3
045300070731     c                   eval      V1Crag   = I37rag
045400070731     c                   eval      V1Ctcr   = I37tcr
045500070731     c                   eval      V1Cksc   = I37ksc
045600070731     c                   eval      V1Cpocgi = I37pocgi
045700070731     c                   eval      V1Ccgi   = I37cgi
045800070731e   1c                   endif
045900070809      *
046000070809      * Posizionamento cursore sulla ragione sociale
046100070809      *   SE NON ricevuto il codice
046200070809if  1c                   if             V1Ccd1  = *zeros
046300070809     c                             and  V1Ccd2  = *zeros
046400070809     c                             and  V1Ccd3  = *zeros
046500070809     c                   eval      *in52    = *on
046600070809e   1c                   endif
046700071025      *
046800071025      * Impostazione attributi di visualizzazione
046900071025      * - Consentire modifica del campo "filiale gestione"
047000071025     c                   eval      *in40    = (DUTlpo  = '1')
047100071025     c                   eval      SavIn40  = *in40
047200070731      *
047300070731      * Abilitazione tasti funzionali:
047400070731      * - F10=Inserimento
047500070918     c                   eval      *in10    = (I37opz <> '1'      and
047600070918     c                                         I37opz <> '3'      and
047700070810     c                                         I37opz <> '5'      and
047800070809     c                                         DUTlpo <> 'S')
047900070731      * - F18=Cambio filiale gestione
048000071025     c                   eval      *in18    = (I37opz  = *blanks  or
048100071025     c                                         I37opz  = '1'      or
048200071025     c                                         DUTlpo  = 'S')
048300070803     c                   eval      SavIn18  = *in18
048400070731      *
048500070731     c                   ENDSR
048600070731
048700070731      *---------------------------------------------------------------*
048800070731      *?Controllo filiale gestione (V1CFGS)                          ?*
048900070731      *---------------------------------------------------------------*
049000070731     c     CtrFGS        BEGSR
049100070731      *
049200070731      * Controllo/Decodifica filiale di gestione
049300070731      * - 0=Tutte
049400070807if  1c                   IF        V1Cfgs   = *zeros
049500070731      *
049600070803if  2c                   if        DUTlpo   = 'S'
049700070731     c                   movel(p)  '0=Tutte'     V1Dfgs
049800070803x   2c                   else
049900070803     c                   eval      V1Dfgs   = *all'? '
050000070803     c                   seton                                        285090
050100070803     c                   eval      V1Dmsg   = $Msg(01)
050200070803     c                   leavesr
050300070803e   2c                   endif
050400070731      *
050500070803x   1c                   ELSE
050600070731      *
050700070731      * - Controllo singola filiale
050800071024if  2c                   IF        V1Cfgs  <> SAVfgs
050900080526     c                   clear                   og143
051000071024     c                   clear                   V1Dfgs
051100070731     c     V1Cfgs        chain     AZORG
051200071024if  3c                   if        NOT  %found(AZORG01L)
051300070731     c                   eval      V1Dfgs   = *all'? '
051400070731     c                   seton                                        285090
051500070731     c                   eval      V1Dmsg   = $Msg(02)
051600070803     c                   leavesr
051700071024e   3c                   endif
051800070731     c                   movel     ORGdes        V1Dfgs
051900080526     c                   movel     ORGde3        Og143
052000070920     c                   movel     ORGde7        Og147
052100070731     c                   movel     ORGde8        Og148
052200071024     c                   eval      SAVfgs   = V1Cfgs
052300071024e   2c                   ENDIF
052400071024      *
052500070803if  2c                   IF        §OGorm   = *blanks
052600070731     c                   seton                                        285090
052700070731     c                   eval      V1Dmsg   = $Msg(03)
052800070803     c                   leavesr
052900070803e   2c                   ENDIF
053000080303      *
053100080303if  2c                   IF             DUTlpo  <> 'S'
053200080303     c                             and  I37opz  <> '1'
053300080303     c                             and  I37opz  <> 'A'
053400100914      *?Aggiungo anche opzione <> '3' tanto l'unico programma che richiama il 37R
053500100914      *?con opzione '3' è il TNTA60R in immissione.
053600100914     c                             and  I37opz  <> '3'
053700110516      *?Aggiungo anche opzione <> '5' è interrogazione, da ta60 si può interrogare
053800110516      *?qualsiasi cliente, da or39 viene fatto il controllo già da lui su cosa si può
053900110516      *?interrogare
054000110516     c                             and  I37opz  <> '5'
054100080303     c                   movel     V1Cfgs        w003a
054200080303     c     w003a         lookup    $Fgs                                   25
054300080303if  3c                   if        NOT  *in25
054400080303     c                   seton                                        285090
054500080303     c                   eval      V1Dmsg   = $Msg(02)
054600080303     c                   leavesr
054700080303e   3c                   endif
054800080303e   2c                   ENDIF
054900070731      *
055000070803e   1c                   ENDIF
055100070731      *
055200070731     c                   ENDSR
055300070731
055400070731      *---------------------------------------------------------------*
055500070731      *?Controllo videata D01                                        ?*
055600070731      *---------------------------------------------------------------*
055700070731     c     CtrD01        BEGSR
055800070801      *
055900070801     c                   movea     *zeros        *in(50)
056000070731      *
056100070731      * Controllo filiale di gestione
056200070731     c                   exsr      CtrFGS
056300070731if  1c                   if        *in90
056400070731     c                   leavesr
056500070731e   1c                   endif
056600071024      *
056700071024sel 1c                   SELECT
056800071024      * Filiale del codice cliente ritiro obbligatoria
056900071024      * (se non selezionato giro)
057000071024w   1c                   WHEN           V1Ccd1   = *zeros
057100071105     c                             and  V1Cpocgi < *all'9'
057200170203     c*//*                         and (V1Ccgi   = *blanks
057300170203     c*//*                          or  V1Ccgi   = *all'9')
057400170203     c                             and  V1Ccgi   = *blanks
057500071024     c                   seton                                        285190
057600170214     c                   eval      V1Dmsg    = %trimR( $Msg(04) ) +
057700170215     c                             ' per clienti "SENZA Giro di Ritiro"'
057800071024     c                   leavesr
057900170214      * -?o SE selezionato "Filiale ritiro diversa" = "S"?
058000170214w   1c                   WHEN           V1Ccd1   = *zeros
058100170214     c                             and  V1Fdiff  = 'S'
058200170214     c                   seton                                        285190
058300170214     c                   eval      V1Dmsg    = %trimR ( $Msg(04) ) +
058400170215     c                             ' se richiesta "Fil. ritiro diversa"'
058500170214     c                   leavesr
058600070731      * Controllo frequenza ritiro
058700071024w   1c                   WHEN           V1Ctcr <> *blanks
058800070731     c                             and  V1Ctcr <> 'F'
058900070731     c                             and  V1Ctcr <> 'M'
059000070731     c                             and  V1Ctcr <> 'O'
059100070731     c                             and  V1Ctcr <> 'R'
059200070731     c                   seton                                        285490
059300070801     c                   eval      V1Dmsg    = $Msg(06)
059400070731     c                   leavesr
059500071024e   1c                   ENDSL
059600070731      *
059700070731      * Controllo codice PdC
059800070801if  1c                   IF        V1Cksc   <> *zeros
059900070731     c                   clear                   TIBS69ds
060000070801     c                   eval      I69kac    =  V1Cksc
060100070731     c                   exsr      Call_TIBS69
060200070731if  2c                   if        *in28
060300070731     c                   leavesr
060400070731e   2c                   endif
060500070731     c                   clear                   Og143
060600070731     c                   movel     V1Cksc        ORGfil
060700070731     c     ORGfil        chain     AZORG
060800070731if  2c                   if        %found(AZORG01L)
060900070801     c                   eval      Og143     = ORGde3
061000070731e   2c                   endif
061100080303if  2c                   if             §OGntw   = 'LOG'
061200070731     c                             or   §OGntw   = 'XXX'
061300070731     c                   seton                                        285590
061400070801     c                   eval      V1Dmsg    = $Msg(07)
061500070731     c                   leavesr
061600070731e   2c                   endif
061700100914      *?Controllo se filiale cliente KSC gestita da filiale gestione utente solo se
061800100914      *?I37opz <> '3'.con opzione '3' è il TNTA60R in immissione.
061900100914      *?L'unico programma che richiama il FIOR37R con opz '3' è il TNTA60R in immissione
062000100914      *?ed ora con il nuovo CRM possiamo aprire clienti anche di altre filiali
062100170213      *?==> NON PIÙ <==?
062200170213     c*//                IF        I37opz  <> '3'
062300170213     c*//                eval      *in25     = *off
062400170213     c*//                movel     V1Cksc        w003a
062500170213     c*//  w003a         lookup    $Fgs                                   25
062600170213if  2c*//                if        NOT  *in25
062700170213     c*//                seton                                        285590
062800170213     c*//                eval      V1Dmsg    = $Msg(14)
062900170213     c*//                leavesr
063000170213e   2c*//                endif
063100170213     c*//                ENDIF
063200070731e   1c                   ENDIF
063300070731      *
063400070731      * Controllo codice giro di ritiro
063500070829if  1c                   IF             V1Ccgi <> *blanks
063600070829     c                             and  V1Ccgi <> *all'9'
063700070731      * - Ricerca codice giro (nessun controllo)
063800070731     c                   eval      xx        = %scan('?' : V1Ccgi)
063900070731if  2c                   IF        xx        > *zeros
064000070731     c                   clear                   V1Ccgi
064100070731     c                   clear                   FIDG09ds
064200070731     c                   eval      D09iop0   = 'P01'
064300070731if  3c                   if        V1Cpocgi <> *zeros
064400070731     c                   eval      D09ifgs   = V1Cpocgi
064500070731x   3c                   else
064600170220     c*//*               eval      D09ifgs   = V1Ccd1
064700170220     c                   eval      D09ifgs   = V1Cfgs
064800070731e   3c                   endif
064900070731     c                   eval      D09idat   = *date
065000070810     c                   eval      D09itug   = 'R'
065100070731     c                   movel(p)  FIDG09ds      KPJBU
065200070731     c                   call      'FIDG09R'
065300070731     c                   parm                    KPJBA
065400070731     c                   movel     KPJBU         FIDG09ds
065500070731if  3c                   if             D09Of03 <> *on
065600070731     c                             and  D09Of12 <> *on
065700070731     c                             and  D09Oerr <> *on
065800070801     c                   eval      V1Cpocgi  = D09ofgs
065900070801     c                   eval      V1Ccgi    = D09ocgi
066000070731e   3c                   endif
066100070801     c                   eval      *in90     = *on
066200070731     c                   leavesr
066300070731e   2c                   ENDIF
066400070829      * - Filiale obbligatoria
066500070829if  2c                   if        V1Cpocgi  = *zeros
066600070803     c                   seton                                        285690
066700070803     c                   eval      V1Dmsg    = $Msg(08)
066800070803     c                   leavesr
066900070828e   2c                   endif
067000070731e   1c                   ENDIF
067100170220      * -?Filiale Giro (dev'essere uguale a Filiale Gestione)?
067200170220if  1c                   if             V1CpocGi <> *zeros
067300170220     c                             and  V1CpocGi <> V1Cfgs
067400170220     c                   seton                                        285690
067500170220     c                   eval      V1Dmsg    = $Msg(15)
067600170220     c                   leavesr
067700170220e   1c                   endif
067800071029      *
067900071029      * - Controllo ambito instradamento
068000071029if  1c                   if             V1Cain <> *blanks
068100071029     c                             and  V1Cain <> '='
068200071029     c                             and  V1Cain <> '>'
068300071029     c                             and  V1Cain <> '<'
068400071029     c                   seton                                        285790
068500071029     c                   eval      V1Dmsg    = $Msg(09)
068600071029     c                   leavesr
068700071029e   1c                   endif
068800170221      *
068900170221      * -?Almeno una parzializzazione/selezione obbligatoria?
069000170221if  1c                   if             V1Ccd1   = *zeros
069100170221     c                             and  V1Ccd2   = *zeros
069200170221     c                             and  V1Ccd3   = *zeros
069300170221     c                             and  V1Crag   = *blanks
069400170221     c                             and  V1mail   = *blanks
069500170221     c                             and  V1sms    = *blanks
069600170221     c                             and  V1Ctcr   = *blanks
069700170221     c                             and  V1Cksc   = *zeros
069800170221     c                             and  V1CpocGi = *zeros
069900170221     c                             and  V1CcGi   = *all'9'
070000170221     c                             and  V1Cain   = *blanks
070100170221     c                             and  V1fDiff  = *blanks
070200170221     c                   seton                                        285190
070300170221     c                   eval      V1Dmsg    = $Msg(16)
070400170221     c                   leavesr
070500170221e   1c                   endif
070600071024      *
070700071024      * Se richiesti clienti ritiro SENZA giro ritiro
070800071024      *  e NON inserita alcuna filiale di ritiro
070900071024      * => viene forzata la filiale gestione
071000070731      *
071100070731      * Se NON vi sono errori, riposiziona normalmente il cursore sul
071200070731      *   campo "Ragione Sociale"
071300070801     c                   eval      *in52    = *on
071400070731      *
071500070731     c                   ENDSR
071600070731
071700070731      *---------------------------------------------------------------*
071800070731      *?Reperimento dati anagrafici                                  ?*
071900070731      *---------------------------------------------------------------*
072000070731     c     Call_TIBS69   BEGSR
072100070731      *
072200070731     c                   clear                   ds_CNACO
072300070731     c                   clear                   ds_CNIND
072400070731     c                   clear                   ds_CNCLP
072500070731     c                   clear                   ds_FNCLS
072600070731      *
072700070731     c                   call      'TIBS69R'
072800070731     c                   parm                    TIBS69ds
072900070731     c                   parm                    ds_CNACO
073000070731     c                   parm                    ds_CNIND
073100070731     c                   parm                    ds_CNCLP
073200070731     c                   parm                    ds_FNCLS
073300070731      *
073400070731     c                   eval      *in28    = (O69err = *on)
073500070731if  1c                   if        *in28
073600070731     c                   eval      *in90    = *on
073700070731     c                   eval      V1Dmsg   = O69msg
073800070731e   1c                   endif
073900070731      *
074000070731     c                   ENDSR
074100070731
074200070731      *---------------------------------------------------------------*
074300070731      *?Gestione tasto funzionale F3 da videata D01                  ?*
074400070731      *?F3=Fine                                                      ?*
074500070731      *---------------------------------------------------------------*
074600070731     c     F03D01        BEGSR
074700070731      *
074800070809     c                   eval      O37ret   = '1'
074900070809      *
075000070731      * Chiusura del programma
075100070801     c                   eval      $Fine    = *on
075200070731      *
075300070731     c                   ENDSR
075400070731
075500070731      *---------------------------------------------------------------*
075600070731      *?Gestione videata S01                                         ?*
075700070731      *---------------------------------------------------------------*
075800070731     c     GesS01        BEGSR
075900070731      *
076000070731      * Inizializzazione videata
076100070731if  1c                   if        $InzS01  = *on
076200070731     c                   exsr      InzS01
076300070731     c                   eval      $InzS01  = *off
076400070731e   1c                   endif
076500070731      *
076600070731      * Emissione testata e riga tasti funzionali abilitati
076700081211if  1c                   if        NOT *in28
076800070731     c                   write     OR37T01
076900070731     c                   write     OR37Z01
077000081211e   1c                   endif
077100070731      *
077200070731      * Posizionamento del cursore
077300070731sel 1c                   select
077400070731w   1c                   when      S01nrr  <= *zeros
077500070731     c                   write     OR37D02
077600070731w   1c                   when      C01csr   > *zeros
077700070731     c                   z-add     C01csr        C01rcd
077800070731e   1c                   endsl
077900070731      *
078000070731      * Emissione videata
078100070731     c                   exfmt     OR37C01
078200070731     c                   z-add     £SFLnrr       C01rcd
078300070731     c                   setoff                                       28  90
078400070731     c                   clear                   V1Dmsg
078500070731      *
078600070731sel 1c                   SELECT
078700070731      * F3=Fine
078800070731w   1c                   WHEN      *inKC
078900070731     c                   exsr      F03D01
079000070731      *
079100070731      * F5=Rivisualizzazione
079200070731w   2c                   WHEN      *inKE
079300070731     c                   eval      $InzS01  = *on
079400070731      *
079500070801      * F10=Inserimento
079600070801w   2c                   WHEN      *inKJ
079700070801     c                   exsr      Call_FIOR37R1
079800070731      *
079900070731      * F12=Ritorno
080000070731w   2c                   WHEN      *inKL
080100070801     c                   exsr      F12S01
080200070731      *
080300070731      * Roll-UP
080400070731w   1c                   WHEN      £Tasto   = C_RollUp
080500070731     c                   exsr      RollUpS01
080600080409      *
080700080409      * Nessun record nel SubFile (-> nessuna opzione)
080800080409w   1c                   when      S01nrr  <= *zeros
080900070731      *
081000070731      * Controllo opzioni
081100070731x   1c                   OTHER
081200070731     c                   exsr      OpzS01
081300070731      *
081400070731e   1c                   ENDSL
081500070731      *
081600070731     c                   ENDSR
081700070731
081800070731      *---------------------------------------------------------------*
081900070731      *?Inizializzazione videata S01                                 ?*
082000070731      *---------------------------------------------------------------*
082100070731     c     InzS01        BEGSR
082200070731      *
082300070731      * Pulizia subfile
082400070731     c                   seton                                        3031
082500070731     c                   write     OR37C01
082600070731     c                   setoff                                         3133
082700070731      *
082800070731     c                   reset                   $EoF
082900070731     c                   clear                   W_SflPag
083000070731     c                   clear                   C01rcd
083100070731     c                   clear                   C01csr
083200070731     c                   clear                   S01nrr
083300070731     c                   clear                   V1Dmsg
083400070731     c                   setoff                                       28  90
083500070801     c                   movea     *zeros        *in(40)
083600170131     c                   eval      SqlMail = *off
083700170131     c                   eval      SqlSms  = *off
083800070802      *
083900070802      * Abilitazione opzioni
084000070802      * - Opz. 1 = Selezione
084100070802     c                   eval      *in41    = (I37opz  = '1')
084200070802      * - Opz. 5 = Visualizzazione
084300070802     c                   eval      *in45    = (I37opz  = '5'   or
084400070802     c                                         DUTlpo  = 'S')
084500070731      *
084600070731      * Caricamento dei dati da presentare nel sfl
084700071024sel 1c                   SELECT
084800071024      * - per codice giro
084900071024w   1c                   WHEN           V1Ccgi <> *blanks
085000071024     c                             and  V1Ccgi <> *all'9'
085100170203     c                             or   V1CpocGi <> *zero
085200170203if  2c                   if             V1Ccgi <> *blanks
085300170203     c                             and  V1Ccgi <> *all'9'
085400071024     c     k02acr12      setll     FNACR102
085500071024     c     k02acr12      reade     FNACR102
085600170203x   2c                   else
085700170203     c     V1CpocGi      setll     FNACR102
085800170203     c     V1CpocGi      reade     FNACR102
085900170203e   2c                   endif
086000071024     c                   eval      $EoF     = (%eof(FNACR12L))
086100071024      * - per codice cliente ritiro
086200170131w   1c                   WHEN      V1Ccd2 > 0 or
086300170203     c                             V1Ccd1 > *zeros  or
086400170213w   1c                             (V1Crag = *blanks and
086500170131     c                              V1mail = *blanks and
086600170213     c                              V1sms  = *blanks)
086700170215if  2c                   if             V1Ccd1   = *zeros
086800170215     c                             and  V1CpocGi = *zeros
086900170215     c                             and  V1Cain  <> *blanks
087000170215     c                   eval      dsCRO1   = V1Cfgs
087100170215x   2c                   else
087200070731     c                   eval      dsCRO1   = V1Ccd1
087300170215e   2c                   endif
087400070731     c                   eval      dsCRO2   = V1Ccd2
087500070731     c                   eval      dsCRO3   = V1Ccd3
087600070731     c                   move      ds_CRO        ACRcro
087700070801     c     ACRcro        setll     FNACR001
087800070731     c                   read      FNACR001
087900070731     c                   eval      $EoF     = (%eof(FNACR01L))
088000071024      * - per ragione sociale cliente ritiro
088100170131x   1c                   WHEN      V1Crag   <> *blanks
088200070731     c                   movel(p)  V1Crag        ACRrsr
088300071024     c                   eval      dsCRO1   = V1Ccd1
088400071024     c                   eval      dsCRO2   = V1Ccd2
088500071024     c                   eval      dsCRO3   = V1Ccd3
088600071024     c                   move      ds_CRO        ACRcro
088700071024     c     k02acr02      setll     FNACR002
088800070731     c                   read      FNACR002
088900070731     c                   eval      $EoF     = (%eof(FNACR02L))
089000170131      /free
089100170131       //?se ricerca per mail SQL su FNACRE x record 'MC'
089200170131         WHEN  V1mail <> *blanks;
089300170131           SqlMail = *on;
089400170131           exec sql
089500170131           declare FACRmail cursor for select ACREcro, ACREdati from FNACRE0F
089600170131           where ACREtrc = 'MC' and ACREatb = ' '
089700170131           order by ACREcro;
089800170131           exec sql OPEN FACRmail;
089900170131           IF  sqlcode < 0;
090000170131             $EoF = *on;
090100170131           ENDIF;
090200170131           exec sql
090300170131           FETCH NEXT from FACRmail into :ACREcro, :ACREdati;
090400170131           IF  sqlcod = 100 or sqlcod < 0;
090500170131             $EoF = *on;
090600170131           ENDIF;
090700170131       //?se ricerca per sms SQL su FNACRE x record 'SC'
090800170131         WHEN  V1sms <> *blanks;
090900170131           SqlSms = *on;
091000170131           exec sql
091100170131           declare FACRsms cursor for select ACREcro, ACREdati from FNACRE0F
091200170131           where ACREtrc = 'SC' and ACREatb = ' '
091300170131           order by ACREcro;
091400170131           exec sql OPEN FACRsms;
091500170131           IF  sqlcode < 0;
091600170131             $EoF = *on;
091700170131           ENDIF;
091800170131           exec sql
091900170131           FETCH NEXT from FACRsms into :ACREcro, :ACREdati;
092000170131           IF  sqlcod = 100 or sqlcod < 0;
092100170131             $EoF = *on;
092200170131           ENDIF;
092300170131      /end-free
092400071024e   1c                   ENDSL
092500170130
092600071024do  1c                   DOW            $EoF   = *off
092700071024     c                             and  S01nrr < C_SflPag
092800070731     c                   exsr      RollUpS01
092900070731e   1c                   ENDDO
093000170130      /free
093100170130       //?chiudo i cursori
093200170130         SELECT;
093300170131         WHEN  Sqlmail;
093400170131           exec sql close FACRmail;
093500170131         WHEN  Sqlsms;
093600170131           exec sql close FACRsms;
093700170130         ENDSL;
093800070731      *
093900070731      * Impaginazione subfile
094000070731      * -> forza l'impaginazione sul 1° rec. del subfile
094100070731if  1c                   if        S01nrr   > *zeros
094200070731     c                   eval      C01rcd   = 1
094300070731     c                   eval      C01csr   = 1
094400070731x   1c                   else
094500070731     c                   clear                   C01rcd
094600070731e   1c                   endif
094700070731      *
094800070731     c                   ENDSR
094900070731
095000070731      *---------------------------------------------------------------*
095100070731      *?Inizializzazione videata S01                                 ?*
095200070731      *---------------------------------------------------------------*
095300070731     c     RollUpS01     BEGSR
095400070731      *
095500070731     c                   eval      S01nrr   = (W_SflPag * C_SflPag)
095600070731     c                   add       1             W_SflPag
095700070731     c                   eval      *in32    = *off
095800151013      *
095900170222     c*//                clear                   SavACRcro
096000070731      *
096100070731      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
096200070731do  1c                   DOW            $EoF   = *off
096300070731     c                             and  S01nrr < (W_SflPag * C_SflPag)
096400070731      *
096500070731      * - Selezione del record dell'archivio
096600070731     c                   exsr      sr_SelRec
096700070731      *
096800070731      *   e Caricamento dati nel record del subfile
096900070731if  2c                   if        $OK      = *on
097000070731     c                   exsr      CarS01
097100070802     c                   add       1             S01nrr
097200070802     c                   write     OR37S01
097300070731e   2c                   endif
097400070731      *
097500070731      * - Lettura record successivo nell'archivio
097600070809      *   (se non rilevato stop record elaborabili in selezione)
097700070809sel 2c                   select
097800070809w   2c                   when      $EoF     = *on
097900071024w   1c                   when           V1Ccgi <> *blanks
098000071024     c                             and  V1Ccgi <> *all'9'
098100170203     c                             or   V1CpocGi <> *zero
098200170203if  2c                   if             V1Ccgi <> *blanks
098300170203     c                             and  V1Ccgi <> *all'9'
098400071024     c     k02acr12      reade     FNACR102
098500170203x   2c                   else
098600170203     c     V1CpocGi      reade     FNACR102
098700170203e   2c                   endif
098800071024     c                   eval      $EoF     = (%eof(FNACR12L))
098900170131w   2c                   when      V1Ccd2  > *zeros or
099000170203     c                             V1Ccd1  > *zeros or
099100170131w   2c                             (V1Crag = *blanks and
099200170131     c                              V1mail = *blanks and
099300170131     c                              V1sms  = *blanks)
099400070731     c                   read      FNACR001
099500071024     c                   eval      $EoF     = (%eof(FNACR01L))
099600170131x   2c                   when      V1Crag <> *blanks
099700070731     c                   read      FNACR002
099800070731     c                   eval      $EoF     = (%eof(FNACR02L))
099900170131      /free
100000170131       //?se ricerca per mail SQL su FNACRE x record 'MC'
100100170131         WHEN  V1mail <> *blanks;
100200170131           exec sql
100300170131           FETCH NEXT from FACRmail into :ACREcro, :ACREdati;
100400170131           IF  sqlcod = 100 or sqlcod < 0;
100500170131             $EoF = *on;
100600170131           ENDIF;
100700170131       //?se ricerca per sms SQL su FNACRE x record 'SC'
100800170131         WHEN  V1sms <> *blanks;
100900170131           exec sql
101000170131           FETCH NEXT from FACRsms into :ACREcro, :ACREdati;
101100170131           IF  sqlcod = 100 or sqlcod < 0;
101200170131             $EoF = *on;
101300170131           ENDIF;
101400170131      /end-free
101500070809e   2c                   endsl
101600070731      *
101700070731e   1c                   ENDDO
101800070731      *
101900070731      * Visualizzazione del SFL (se ci sono dati)
102000070731     c                   eval      *in30    = (S01nrr <= *zeros)
102100070731      *
102200070731      * Attivazione (eventuale) del SFLEND
102300070731     c                   eval      *in33    = ($EoF = *on)
102400070731      *
102500070731      * Posizionamento cursore al 1° rcd della pagina
102600070731if  1c                   if        S01nrr   > *zeros
102700070731     c     S01nrr        div       C_SflPag      wPag
102800070731     c                   mvr                     wRig
102900070731     c                   eval      C01rcd   = wPag * C_SflPag
103000070731     c                   if        wRig     > *zeros
103100070731     c                   eval      C01rcd   = C01rcd + 1
103200070731     c                   else
103300070731     c                   eval      C01rcd   = C01rcd - C_SflPag + 1
103400070731     c                   endif
103500070731x   1c                   else
103600070731     c                   clear                   C01rcd
103700070731e   1c                   endif
103800070731     c                   eval      C01csr   = C01rcd
103900070731      *
104000070731     c                   ENDSR
104100070731
104200070731      *---------------------------------------------------------------*
104300070731      *?Selezione record in base alle selezioni                      ?*
104400070731      *---------------------------------------------------------------*
104500070731     c     sr_SelRec     BEGSR
104600070731      *
104700070731     c                   eval      $OK      = *off
104800170131     c                   clear                   SavV1mail
104900071024      *
105000071024      * Reperimento dati da FNACR01L se lettura di FNACR12L
105100071024if  1c                   if             V1Ccgi <> *blanks
105200071024     c                             and  V1Ccgi <> *all'9'
105300170203     c                             or   V1CpocGi <> *zero
105400170203if  2c                   IF        ACRcro <> ACR1cro
105500071024     c     ACR1cro       chain     FNACR001
105600170203x   2c                   ELSE
105700170131      * Per lettura di FNACR12L posso leggere 2 volte lo stesso cliente
105800170131      * in questo caso non carico il subfile
105900170209      *?-SE- NON parzializzato Ambito instradamento:?
106000170209      *?Se inserita una parzializzazione per ambito "="   e?
106100170209      *?   scartato il primo record con ambito "<"?
106200170209      *?Il 2° rec che viene elaborato potrebbe essere quello giusto!?
106300170209     c                   if        V1Cain = *blank
106400170131     c                   leavesr
106500170209     c                   endif
106600170203e   2c                   ENDIF
106700170131e   1c                   ENDIF
106800170131      /free
106900170215         // -?Se NON inserita alcuna parzializzazione?
107000170215         //  ?       per filiale o per codice cliente?
107100170215         //  ?=> controllo che il cliente sia della filiale in gestione.?
107200170215         //  ?N.B. - Avendo già fatto il posizionamento (setll) pr FGS,?
107300170215         //  ?       al 1° cliente di filiale diversa si può associare?
107400170215         //  ?       l' %eof.?
107500170215         if       V1Ccd1   = *zeros
107600170215             and  V1CpocGi = *zeros
107700170215             and  V1Cain  <> *blanks
107800170215             and  %subst( %editc( ACRcro : 'X' ) : 1 : 3 )
107900170215                          <> %editc( V1Cfgs : 'X' );
108000170215           $EoF = *on;
108100170215           leavesr;
108200170215         endif;
108300170215
108400170131       //?Se non ho fatto SQL per mail o sms
108500170131       //?e ho mail e/o sms impostati nelle ricerche
108600170131       //?devo agganciare il file
108700170131       //?se ricerca per mail
108800170131         IF  V1Mail <> *blanks;
108900170131           IF  not SqlMail;
109000170131             chain (ACRcro:'MC') FNACRE1L;
109100170131             IF  ACREatb <> *blanks or not %found(FNACRE1L);
109200170131               clear ACREdati;
109300170131             ENDIF;
109400170131           ENDIF;
109500170131           SavV1mail = V1mail;
109600170131           IF  %scan(%trim(SavV1mail):ACREdati) = 0;
109700170131             SavV1mail = %xlate(up:lo:V1mail);
109800170131             IF  %scan(%trim(SavV1mail):ACREdati) = 0;
109900170131               leavesr;
110000170131             ENDIF;
110100170131           ENDIF;
110200170131         ENDIF;
110300170131       //?se ricerca per sms
110400170131         IF  V1sms <> *blanks;
110500170131           IF  not SqlSms;
110600170131             IF  not SqlMail;
110700170131               chain (ACRcro:'SC') FNACRE1L;
110800170131             ELSE;
110900170131               chain (ACREcro:'SC') FNACRE1L;
111000170131             ENDIF;
111100170131             IF  ACREatb <> *blanks or not %found(FNACRE1L);
111200170131               clear ACREdati;
111300170131             ENDIF;
111400170131           ENDIF;
111500170131           IF  %scan(%trim(V1sms):ACREdati) = 0;
111600170131             leavesr;
111700170131           ENDIF;
111800170131         ENDIF;
111900170131       //?se arrivo qua vuol dire che ho trovato mail o sms validi
112000170131       //?quindi aggancio FNACR00F se sto facendo la ricerca tramite SQL
112100170131         IF  SqlMail or SqlSms;
112200170131           chain (ACREcro) FNACR01L;
112300170131         ENDIF;
112400170131      /end-free
112500070801      *
112600070801      * - Se interrogazione ("5") o manutenzione del PdC ("A")
112700130417      *   o immissione da convalida trattative
112800070801      *   si devono caricare solo i codici uguali ai primi sette byte
112900070802if  1c                   if            (I37opz  = '5'
113000130417     c                             or   I37opz  = '3'
113100070802     c                             or   I37opz  = 'A')
113200070802     c                             and  I37cro <> *zeros
113300070801     c                   movel     ACRcro        W7_ACRcro
113400070801     c                   movel     I37cro        W7_I37cro
113500070801sel 2c                   select
113600070801      * - - se minore si deve tornare a leggere
113700070801w   2c                   when      W7_ACRcro  < W7_I37cro
113800070801     c                   leavesr
113900070801      * - - se maggiore si deve uscire
114000070801w   2c                   when      W7_ACRcro  > W7_I37cro
114100070809     c                   eval      $EoF       = *on
114200070801     c                   leavesr
114300070801e   2c                   endsl
114400070801e   1c                   endif
114500070801      *
114600071031sel 1c                   select
114700071024      * Filiale codice cliente ritiro
114800150909w   1c                   when      V1Ccd1 <> *zeros and
114900150909     c                             V1Ccd1 <>
115000150909     c                             %dec(%subst(%editc(ACRcro:'X'):1:3):3:0)
115100071024     c                   leavesr
115200170213      * Codice cliente ritiro
115300170213w   1c                   when      V1Ccd2 <> *zeros and
115400170213     c                             V1Ccd2 <>
115500170213     c                             %dec(%subst(%editc(ACRcro:'X'):4:4):4:0)
115600170213     c                   leavesr
115700170213w   1c                   when      V1Ccd3 <> *zeros and
115800170213     c                             V1Ccd3 <>
115900170213     c                             %dec(%subst(%editc(ACRcro:'X'):8:3):3:0)
116000170213     c                   leavesr
116100170213      * Ragione sociale
116200170213     c                   when      V1Crag <> *blanks  and
116300170213     c                             %trim(V1Crag) <> %subst( %trimL(ACRrsr)
116400170213     c                                       : 1 : %len( %trim(V1Crag) ) )
116500170213     c                   leavesr
116600070801      * Frequenza ritiro
116700071031w   1c                   when           V1Ctcr <> *blanks
116800070801     c                             and  ACRtcr <> V1Ctcr
116900070801     c                   leavesr
117000070801      * Codice Piano dei Conti
117100070801w   1c                   when           V1Cksc <> *zeros
117200070801     c                             and  ACRksc <> V1Cksc
117300070801     c                   leavesr
117400071031      * Tipo anagrafica (NON richiesto a video)
117500071031     c                   when           I37opz <> *blanks
117600071031     c                             and  I37tac <> *blanks
117700071031     c                             and  ACRtac <> I37tac
117800071031     c                   leavesr
117900170213      * Filiale codice giro di ritiro
118000170213     c                   when      V1CpocGi <> *zero    and
118100170213     c                             V1CpocGi <> *all'9'  and
118200170213     c                             V1CpocGi <> ACR1pocGi
118300170213     c                   leavesr
118400071031e   1c                   endsl
118500070731      *
118600071024      * Codice giro di ritiro
118700071024      * - se richiesto con/senza giro ("99999"): nessun controllo
118800071024      * - se richiesto    con    giro: è già stato letto FNACR12L
118900071024      * - se richiesto   senza   giri: da scartare se rilevato un giro
119000071030sel 1c                   SELECT
119100071030w   1c                   WHEN      V1Ccgi   = *all'9'
119200071030w   1c                   WHEN      V1Ccgi   = *blanks
119300071029     c                   exsr      sr_ChkCGI
119400071030if  2c                   if        $Giro    = *on
119500071029     c                   leavesr
119600071029e   2c                   endif
119700170214e   1c                   ENDSL
119800170214      *//*
119900170214      *//* Ambito instradamento giro SE lettura di FNACR12L
120000170214if  1c*//                   if            (V1Ccgi <> *blanks
120100170214     c*//                             and  V1Ccgi <> *all'9'
120200170214     c*//                             or   V1CpocGi <> *zero)
120300170214     c*//                             and  V1Cain <> *blanks
120400170214     c*//                             and  V1Cain <> ACR1ain
120500170214     c*//                   leavesr
120600170214e   1c*//                   endif
120700170214      *
120800170214      * -?Ambito instradamento giro?
120900170214if  1c                   IF        V1Cain <> *blanks
121000170214      *
121100170220sel 2c                   Select
121200170220      *
121300170214      * - -?Già in input FNACR12L -SE- parzializzato per?
121400170214      *    ?codice giro ritiro?
121500170220w   2c                   When          (V1Ccgi   <> *blanks
121600170214     c                             and  V1Ccgi   <> *all'9'
121700170214     c                              or  V1CpocGi <> *zero)
121800170214      *
121900170214if  3c                   if        V1Cain <> ACR1ain
122000170214     c                   leavesr
122100170214e   3c                   endif
122200170220      *
122300170220      * - -?Chiesti clienti SENZA giro... (???)?
122400170220w   2c                   When      V1Ccgi =  *blanks
122500170220      *
122600170220     c                   leavesr
122700170214      *
122800170214      * - -?Da cercare in FNACR13L altrimenti?
122900170220x   2c                   Other
123000170214      *
123100170214     c                   exsr      sr_ChkAIN
123200170214if  3c                   if        Not  $AIN
123300170214     c                   leavesr
123400170214e   3c                   endif
123500170214      *
123600170220e   2c                   EndSl
123700170214      *
123800170214e   1c                   ENDIF
123900170214      *
124000150909      * Filiale ritiro diversa da cod.Cliente
124100150909     c                   IF        V1fdiff = 'S' and
124200150909     c                             ACRpoa = V1Ccd1
124300150909     c                   leavesr
124400150909     c                   ENDIF
124500070731      *
124600071030     c                   eval      $OK      = *on
124700070731      *
124800070731     c                   ENDSR
124900071029
125000071029      *---------------------------------------------------------------*
125100071029      *?Verifica/Selezione giro ritiro per cliente ritiro            ?*
125200071029      *---------------------------------------------------------------*
125300071029     c     sr_ChkCGI     BEGSR
125400071029      *
125500071030     c                   eval      $Giro    = *on
125600071029      *
125700071029      * Posizionamento iniziale per verifica esistenza giri
125800071029sel 1c                   select
125900071029w   1c                   when      V1Cpocgi  <> *zeros
126000071029     c                   eval      ACR1pocgi  = V1Cpocgi
126100071030     c     k02acr11      setll     FNACR103
126200071029w   1c                   when      DUTlpo     = '2'
126300071029     c                   eval      ACR1pocgi  = DUTpou
126400071030     c     k02acr11      setll     FNACR103
126500071029x   1c                   other
126600071030     c     ACRcro        setll     FNACR103
126700071029e   1c                   endsl
126800071029      *
126900071030if  1c                   if        NOT  %equal(FNACR13L)
127000071030     c                   eval      $Giro    = *off
127100071029     c                   leavesr
127200071029e   1c                   endif
127300071029      *
127400071029      * TROVATO ALMENO UN RECORD:
127500071029      * - Se richiesto con/senza giro ("99999"): nessun controllo
127600071029      * - Se richiesto    con    giro: è già stato letto FNACR12L
127700071029      *                                (non viene eseguita questa subr.)
127800071029      * -?Se richiesto   senza   giri: da scartare se rilevato un giro?
127900071029      *                               ?dell'ambito inserito in D01?
128000071029      *
128100071029      * Richiesto senza giri di qualsiasi ambito e trovato giro
128200071030if  1c                   IF        V1Cain   =  *blanks
128300071029     c                   leavesr
128400071029      *
128500071029      * Richiesto senza giri di un ambito specifico: ricerca ambito
128600071029x   1c                   ELSE
128700071029      *
128800071030do  2c                   DOU       %eof(FNACR13L)
128900071030sel 3c                   select
129000071030w   3c                   when      V1Cpocgi  <> *zeros
129100071030     c     k02acr11      reade     FNACR103
129200071030w   3c                   when      DUTlpo     = '2'
129300071030     c     k02acr11      reade     FNACR103
129400071030x   3c                   other
129500071030     c     ACRcro        reade     FNACR103
129600071030e   3c                   endsl
129700071030sel 3c                   select
129800071030w   3c                   when      %eof(FNACR13L)
129900071030     c                   eval      $Giro      = *off
130000071030w   3c                   when      ACR1ain    = V1Cain
130100071029     c                   leavesr
130200071030e   3c                   endsl
130300071029e   2c                   ENDDO
130400071029      *
130500071029e   1c                   ENDIF
130600071029      *
130700071029     c                   ENDSR
130800170214
130900170214      *---------------------------------------------------------------*
131000170214      *?Verifica/Selezione Ambito Instradamento per Giro Ritiro      ?*
131100170214      *---------------------------------------------------------------*
131200170214     c     sr_ChkAIN     BEGSR
131300170214      *
131400170214     c                   eval      $AIN     = *off
131500170214      *
131600170220      * -?Posizionamento iniziale per verifica esistenza giri?
131700170214     c     ACRcro        setll     FNACR103
131800170214      *
131900170214if  1c                   if        NOT  %equal(FNACR13L)
132000170214     c                   leavesr
132100170214e   1c                   endif
132200170214      *
132300170220      * -?TROVATO ALMENO UN RECORD:?
132400170214      *
132500170220      * -?Ciclo di lettura Giri?
132600170214do  1c                   DoU       %eof(FNACR13L)
132700170214     c     ACRcro        reade     FNACR103
132800170214sel 2c                   select
132900170220      * - -?Fine giri?
133000170214w   2c                   when      %eof(FNACR13L)
133100170214     c                   leavesr
133200170220      * - -?Filiale giro diversa da quella inserita?
133300170220w   2c                   when           V1CpocGi  <> *zeros
133400170220     c                             and  ACR1pocGi <> V1CpocGi
133500170220     c                   leavesr
133600170220      * - -?Filiale giro diversa da quella di gestione?
133700170220w   2c                   when           V1CpocGi   = *zeros
133800170220     c                             and  ACR1pocGi <> V1Cfgs
133900170220     c                   leavesr
134000170220      * - -?Trovato giro per l'ambito di instradamento specificato?
134100170214w   2c                   when      ACR1ain = V1Cain
134200170214     c                   eval      $AIN    = *on
134300170214     c                   leavesr
134400170214e   2c                   endsl
134500170214e   1c                   EndDo
134600170214      *
134700170214     c                   ENDSR
134800070731
134900070731      *---------------------------------------------------------------*
135000070731      *?Caricamento singolo record nel SubFile S01                   ?*
135100070731      *---------------------------------------------------------------*
135200070731     c     CarS01        BEGSR
135300070731      *
135400071024      * Ricerca eventuali giri per cliente ritiro
135500071030     c     ACRcro        setll     FNACR103
135600071024      *
135700070731     c                   clear                   OR37S01
135800070731      * Campi hidden
135900070731      * Campi di solo output
136000070801     c                   eval      S1Catb   = ACRatb
136100070801     c                   move      ACRcro        ds_CRO
136200070801     c                   eval      S1Ccd1   = dsCRO1
136300070801     c                   eval      S1Ccd2   = dsCRO2
136400070801     c                   eval      S1Ccd3   = dsCRO3
136500070801     c                   movel     ACRrsr        S1Crag
136600071030if  1c                   if        %equal(FNACR13L)
136700071030     c                   eval      S1Csng   = 'S'
136800070903e   1c                   endif
136900070801     c                   if        ACRksc  <> *zeros
137000070801     c                   move      ACRksc        S1Cksc
137100070801     c                   endif
137200070801     c                   if        ACRccc  <> 999
137300070801     c                   move      ACRccc        S1Cctr
137400070801     c                   endif
137500070801     c                   eval      S1Ctcr   = ACRtcr
137600070801      *
137700070801     c                   movel     ACRinr        S1Cind
137800070801     c                   movel     ACRlor        S1Cloc
137900070801     c                   eval      S1Cprv   = ACRprr
138000070801     c                   eval      S1Cnaz   = ACRnar
138100150909
138200150909      * Filiale ritiro solo se diversa dal codice cliente
138300150909     c                   IF        %dec(%subst(%editc(ACRcro:'X'):1:3):3:0) <>
138400150909     c                             ACRpoa
138500150909     c                   eval      S1cpoa = %editc(ACRpoa:'X')
138600150909     c                   ELSE
138700150909     c                   clear                   S1cpoa
138800150909     c                   ENDIF
138900070731      *
139000070731     c                   ENDSR
139100070801
139200070801      *---------------------------------------------------------------*
139300070801      *?Gestione tasto funzionale F12 da videata S01                 ?*
139400070801      *?F12=Ritorno                                                  ?*
139500070801      *---------------------------------------------------------------*
139600070801     c     F12S01        BEGSR
139700070801      *
139800070801      * Chiusura del programma  SE  richiamato da altro programma
139900070801     c                   if             I37opz  <> *blanks
140000070801     c                             and (I37cro  <> *zeros
140100070801     c                              or  I37rag  <> *blanks)
140200070809     c                   eval      O37ret   = '2'
140300070801     c                   eval      $Fine    = *on
140400070801      * Ritorno alla videata D01  SE  richiamato da menù
140500070801     c                   else
140600070801     c                   eval      $Video   = 'D1'
140700071025     c                   eval      *in40    = (SavIn40 = *on)
140800071025      *
140900070801     c                   endif
141000070801      *
141100070801     c                   ENDSR
141200070731
141300070731      *---------------------------------------------------------------*
141400070731      *?Controllo opzioni subfile                                    ?*
141500070731      *---------------------------------------------------------------*
141600070731     c     OpzS01        BEGSR
141700070731      *
141800070731     c                   readc     OR37S01
141900070731      *
142000070731do  1c                   DOW       NOT %eof(FIOR37D)
142100070731      *
142200070801     c                   eval      *in32    = *off
142300070801     c                   movea     *zeros        *in(50)
142400070731     c                   z-add     S01nrr        C01rcd
142500070731      *
142600070731sel 2c                   SELECT
142700070731      * - Nessuna opzione
142800070801w   2c                   WHEN      S1Copz   = *zeros
142900070801      * - 1 = Selezione
143000070801w   2c                   WHEN           S1Copz  = 1
143100070801     c                             and  I37opz  = '1'
143200070808     c                   eval      dsCRO1   = S1Ccd1
143300070808     c                   eval      dsCRO2   = S1Ccd2
143400070808     c                   eval      dsCRO3   = S1Ccd3
143500070808     c                   move      ds_CRO        O37cro
143600070808     c                   eval      $Fine    = *on
143700070808     c                   eval      *in90    = *on
143800070731      * - 2 = Modifica
143900070801w   2c                   WHEN           S1Copz  = 2
144000070801     c                             and (I37opz <> '1'
144100070801     c                             and  I37opz <> '5')
144200070802     c                             and  DUTlpo <> 'S'
144300071026if  3c                   if        S1Ccd1   = V1Cfgs
144400070801     c                   exsr      Call_FIOR37R1
144500071026x   3c                   else
144600071024     c                   exsr      Call_FIOR37R2
144700071026e   3c                   endif
144800070807      * - 3 = Copia
144900070801w   2c                   WHEN           S1Copz  = 3
145000070801     c                             and (I37opz <> '1'
145100070801     c                             and  I37opz <> '5')
145200070801     c                             and  DUTlpo <> 'S'
145300071026if  3c                   if        S1Ccd1   = V1Cfgs
145400070801     c                   exsr      Call_FIOR37R1
145500071026x   3c                   else
145600071026     c                   seton                                        285090
145700071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
145800071026     c                                      + ' per un cliente ritiro +
145900071026     c                                        di altra filiale ('+
146000071026     c                                        %trim( %editw(+
146100071026     c                                              S1Ccd1:'0   '))
146200071026     c                                      + ')'
146300071026e   3c                   endif
146400070801      * - 4 = Annullamento
146500070801w   2c                   WHEN           S1Copz  = 4
146600070801     c                             and (I37opz <> '1'
146700070801     c                             and  I37opz <> '5')
146800070802     c                             and  DUTlpo <> 'S'
146900071026sel 3c                   select
147000071026w   3c                   when      S1Catb  <> *blanks
147100070801     c                   seton                                        285090
147200070801     c                   eval      V1Dmsg   = $Msg(10)
147300071026w   3c                   when      S1Ccd1   = V1Cfgs
147400140723w   3c                   IF        S1Copz  = 4 and
147500140807w   3c                             S1Ccd3  = 0 and
147600140807     c                             S1Ccd2  <> 9999 and
147700140807     c                             S1Ccd2  <> 8888
147800140723     c                   seton                                        285090
147900140723     c                   eval      V1Dmsg   = %trimr($Msg(12))
148000140723     c                                      + ' per luogo 000'
148100140723     c                   ELSE
148200070801     c                   exsr      Call_FIOR37R1
148300140723     c                   ENDIF
148400071026x   4c                   other
148500071026     c                   seton                                        285090
148600071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
148700071026     c                                      + ' per un cliente ritiro +
148800071026     c                                        di altra filiale ('+
148900071026     c                                        %trim( %editw(+
149000071026     c                                              S1Ccd1:'0   '))
149100071026     c                                      + ')'
149200071026e   3c                   endsl
149300070731      * - 5 = Visualizzazione
149400070801w   2c                   WHEN           S1Copz  = 5
149500070801     c                   exsr      Call_FIOR37R1
149600070801      * - 7 = Ripristino
149700070801w   2c                   WHEN           S1Copz  = 7
149800070801     c                             and (I37opz <> '1'
149900070801     c                             and  I37opz <> '5')
150000070801     c                             and  DUTlpo <> 'S'
150100071026sel 3c                   select
150200071026w   3c                   when      S1Catb   = *blanks
150300070801     c                   seton                                        285090
150400070801     c                   eval      V1Dmsg   = $Msg(11)
150500071026w   3c                   when      S1Ccd1   = V1Cfgs
150600070801     c                   exsr      Call_FIOR37R1
150700071026x   3c                   other
150800071026     c                   seton                                        285090
150900071026     c                   eval      V1Dmsg   = %trimr($Msg(12))
151000071026     c                                      + ' per un cliente ritiro +
151100071026     c                                        di altra filiale ('+
151200071026     c                                        %trim( %editw(+
151300071026     c                                              S1Ccd1:'0   '))
151400071026     c                                      + ')'
151500071026e   3c                   endsl
151600070731      * - ? = Opzione NON valida
151700070731x   2c                   OTHER
151800070731     c                   seton                                        285090
151900070801     c                   eval      V1Dmsg   = $Msg(12)
152000070731e   2c                   ENDSL
152100070731      *
152200070731      * Aggiornamento sfl
152300070731sel 2c                   select
152400070731w   2c                   when      *in28
152500070807     c                   eval      *in32    = *on
152600070731     c                   z-add     C01rcd        C01csr
152700070807w   2c                   when      *in90    = *on
152800070731     c                   z-add     C01rcd        C01csr
152900070731     c                   clear                   S1Copz
153000070731x   2c                   other
153100070926     c                   z-add     C01rcd        C01csr
153200070731     c                   clear                   S1Copz
153300070731e   2c                   endsl
153400070801     c                   UPDATE    OR37S01
153500070731if  2c                   if        *in28  OR  *in90
153600070731     c                   leave
153700070731e   2c                   endif
153800070731      *
153900070731     c                   readc     OR37S01
154000070731      *
154100070731e   1c                   ENDDO
154200170222      *//*
154300170222      *//* -?SE si sta leggendo il file FNACR01L &?
154400170222      *//*  ?   è stato modificato/annullato/ripristinato un cliente?
154500170222      *//*  ?occorre riposizionarsi per l'eventuale pagina successiva?
154600170222if  1c*//                   if        V1Crag    = *blanks  and
154700170222     c*//                             SavACRcro > *zeros
154800170222     c*//     SavACRcro     chain     FNACR001
154900170222e   1c*//                   endif
155000170222      * -?SE è stato modificato/annullato/ripristinato un cliente?
155100170222      *  ?sarà richiesta l'inizializzazione del subfile?
155200070731      *
155300070731     c                   ENDSR
155400070801
155500070801      *---------------------------------------------------------------*
155600070801      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
155700070801      *---------------------------------------------------------------*
155800070801     c     Call_FIOR37R1 BEGSR
155900070801      *
156000070801      * Salvataggio area dati
156100070801     c                   movel     FIOR37ds      SavOR37ds
156200070801      *
156300070801      * Impostazione parametri
156400070801     c                   clear                   FIOR37ds
156500070801     c                   eval      I37fgs   = V1Cfgs
156600070801sel 1c                   SELECT
156700070809      * - F10 = Immissione
156800070801w   1c                   WHEN           *inKJ   = *on
156900070801     c                   eval      I37opz   = '1'
157000070809      * - I37opz = '3' (Immissione/Copia - obbligatorio!)
157100070809w   1c                   WHEN           $Video  = 'D1'
157200070809     c                             and  %subst(SavOR37ds : 1 : 1) = '3'
157300070809     c                   eval      I37opz   = 'A'
157400070809     c                   eval      dsCRO1   = V1Ccd1
157500070809     c                   eval      dsCRO2   = V1Ccd2
157600070809     c                   eval      dsCRO3   = V1Ccd3
157700070809     c                   move      ds_CRO        I37cro
157800070809     c                   eval      I37ksc   = V1Cksc
157900070801      * - 2 = Modifica
158000070801w   1c                   WHEN           $Video  = 'S1'
158100070801     c                             and  S1Copz  = 2
158200070801     c                   eval      I37opz   = '2'
158300070801     c                   eval      dsCRO1   = S1Ccd1
158400070801     c                   eval      dsCRO2   = S1Ccd2
158500070801     c                   eval      dsCRO3   = S1Ccd3
158600070801     c                   move      ds_CRO        I37cro
158700070801      * - 3 = Copia (non in sede)
158800070801w   1c                   WHEN           $Video  = 'S1'
158900070801     c                             and  S1Copz  = 3
159000070801     c                   eval      I37opz   = '3'
159100070801     c                   eval      dsCRO1   = S1Ccd1
159200070801     c                   eval      dsCRO2   = S1Ccd2
159300070801     c                   eval      dsCRO3   = S1Ccd3
159400070801     c                   move      ds_CRO        I37cro
159500070801      * - 4 = Annullamento
159600070801w   1c                   WHEN           $Video  = 'S1'
159700070801     c                             and  S1Copz  = 4
159800070801     c                   eval      I37opz   = '4'
159900070801     c                   eval      dsCRO1   = S1Ccd1
160000070801     c                   eval      dsCRO2   = S1Ccd2
160100070801     c                   eval      dsCRO3   = S1Ccd3
160200070801     c                   move      ds_CRO        I37cro
160300070801      * - 5 = Visualizzazione
160400070801w   1c                   WHEN           $Video  = 'S1'
160500070801     c                             and  S1Copz  = 5
160600070801     c                   eval      I37opz   = '5'
160700070801     c                   eval      dsCRO1   = S1Ccd1
160800070801     c                   eval      dsCRO2   = S1Ccd2
160900070801     c                   eval      dsCRO3   = S1Ccd3
161000070801     c                   move      ds_CRO        I37cro
161100070801      * - 7 = Ripristino
161200070801w   1c                   WHEN           $Video  = 'S1'
161300070801     c                             and  S1Copz  = 7
161400070801     c                   eval      I37opz   = '7'
161500070801     c                   eval      dsCRO1   = S1Ccd1
161600070801     c                   eval      dsCRO2   = S1Ccd2
161700070801     c                   eval      dsCRO3   = S1Ccd3
161800070801     c                   move      ds_CRO        I37cro
161900070801e   1c                   ENDSL
162000070801      *
162100070801     c                   movel(p)  FIOR37ds      KPJBU
162200070801     c                   call      'FIOR37R1'
162300070801     c                   parm                    KPJBA
162400070801     c                   movel     KPJBU         FIOR37ds
162500070801      *
162600070801sel 1c                   SELECT
162700070801      * - Errore
162800070801w   1c                   WHEN      O37err   = 'E'
162900070801     c                   eval      V1Dmsg   = O37msg
163000070801     c                   seton                                        28  90
163100070802      * - F3=Fine
163200070803w   1c                   WHEN      O37ret   = '1'
163300070809if  2c                   if             S1Copz  = 5
163400070809     c                             and  S01nrr  = 1
163500070809     c                   eval      $Fine    = *on
163600070809e   2c                   endif
163700070806     c                   eval      *in90    = *on
163800070803      * - F12=Ritorno
163900070803w   1c                   WHEN      O37ret   = '2'
164000070801      * - Inserito record
164100070803w   1c                   WHEN           O37ret  = *off
164200070803     c                             and  O37cro  > *zeros
164300070801     c                   eval      $InzS01  = *on
164400070802      * - Modificato record
164500070802w   1c                   OTHER
164600170222      *//* - -?SE si sta leggendo il file FNACR01L:?
164700170222      *//*    ?occorre salvare il cliente su cui si è posizionati per il?
164800170222      *//*    ?caricamento dell'eventuale pagina successiva del subfile?
164900170222if  2c*//                   if        V1Crag    = *blanks  and
165000170222     c*//                             SavACRcro = *zeros
165100170222     c*//                   eval      SavACRcro = ACRcro
165200170222e   2c*//                   endif
165300170222     c*//     I37cro        chain     FNACR001
165400170222if  2c*//                   if        %found(FNACR01L)
165500170222     c*//                   exsr      CarS01
165600170222e   2c*//                   endif
165700170222      * - -?Potrebbe essere stato modificato qualcosa che renderebbe il?
165800170222      *    ?cliente NON più selezionabile, in base alle parzializzazioni?
165900170222      *    ?inserite nella videata D1?
166000170222     c                   eval      $InzS01  = *on
166100070801e   1c                   ENDSL
166200070801      *
166300070801      * Ripristino area dati
166400070801     c                   movel     SavOR37ds     FIOR37ds
166500070801      *
166600070801     c                   ENDSR
166700071023
166800071023      *---------------------------------------------------------------*
166900071023      *?Richiamo programma di Manutenzione Anagrafica Clienti Ritiro ?*
167000071023      *---------------------------------------------------------------*
167100071024     c     Call_FIOR37R2 BEGSR
167200071023      *
167300071023      * Salvataggio area dati
167400071023     c                   movel     FIOR37ds      SavOR37ds
167500071023      *
167600071023      * Impostazione parametri
167700071024     c                   clear                   FIOR37ds
167800071024     c                   eval      I37fgs   = V1Cfgs
167900071023     c                   eval      dsCRO1   = S1Ccd1
168000071023     c                   eval      dsCRO2   = S1Ccd2
168100071023     c                   eval      dsCRO3   = S1Ccd3
168200071024     c                   move      ds_CRO        I37cro
168300071024     c                   eval      I37pocgi = V1Cfgs
168400071023      *
168500071024     c                   movel(p)  FIOR37ds      KPJBU
168600071024     c                   call      'FIOR37R2'
168700071023     c                   parm                    KPJBA
168800071024     c                   movel     KPJBU         FIOR37ds
168900071023      *
169000071023sel 1c                   SELECT
169100071023      * - Errore
169200071024w   1c                   WHEN      O37err   = 'E'
169300071023     c                   eval      V1Dmsg   = O37msg
169400071023     c                   seton                                        28  90
169500071023      * - F3=Fine
169600071024w   1c                   WHEN      O37ret   = '1'
169700071023if  2c                   if             S1Copz  = 5
169800071023     c                             and  S01nrr  = 1
169900071023     c                   eval      $Fine    = *on
170000071023e   2c                   endif
170100071023     c                   eval      *in90    = *on
170200071023      * - F12=Ritorno
170300071024w   1c                   WHEN      O37ret   = '2'
170400071024      * - Inserito/Modificato/Eliminato giro ritiro
170500071024w   1c                   OTHER
170600170222      *//* - -?SE si sta leggendo il file FNACR01L:?
170700170222      *//*    ?occorre salvare il cliente su cui si è posizionati per il?
170800170222      *//*    ?caricamento dell'eventuale pagina successiva del subfile?
170900170222if  2c*//                   if        V1Crag    = *blanks  and
171000170222     c*//                             SavACRcro = *zeros
171100170222     c*//                   eval      SavACRcro = ACRcro
171200170222e   2c*//                   endif
171300170222     c*//     I37cro        chain     FNACR001
171400170222if  2c*//                   if        %found(FNACR01L)
171500170222     c*//                   exsr      CarS01
171600170222e   2c*//                   endif
171700170222      * - -?Potrebbe essere stato modificato qualcosa che renderebbe il?
171800170222      *    ?cliente NON più selezionabile, in base alle parzializzazioni?
171900170222      *    ?inserite nella videata D1?
172000170222     c                   eval      $InzS01  = *on
172100071024e   1c                   ENDSL
172200071023      *
172300071023      * Ripristino area dati
172400071023     c                   movel     SavOR37ds     FIOR37ds
172500071023      *
172600071023     c                   ENDSR
172700000929
172800070730** - $MSG -------------------------------------------------------------------*
172900070801Elaborazione non possibile: manca la filiale gestione                           1
173000070801Filiale gestione errata                                                         2
173100070801Procedura ORM non attivata                                                      3
173200170215Fil. del Codice cliente ritiro obbligatoria                                     4
173300070801Tipo anagrafica errato                                                          5
173400070801Frequenza ritiro errata                                                         6
173500070801Codice cliente piano dei conti errato                                           7
173600070801Filiale obbligatoria per codice giro diverso da '9999999999'                    8
173700071029Ambito instradamento errato                                                     9
173800070801Record annullato. Consentito solo il ripristino                                10
173900070801Record non annullato. Impossibile ripristinare                                 11
174000070801Opzione errata                                                                 12
174100080303Utente non autorizzato alla Gestione anagrafico clienti                        13
174200080303Cliente NON gestibile dall'utente                                              14
174300170220Filiale giro errata: deve coincidere con la filiale gestione                   15
174400170221Inserire ALMENO una selezione                                                  16
